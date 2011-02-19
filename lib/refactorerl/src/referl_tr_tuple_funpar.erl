%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc
%%% In this transformation, consecutive arguments of a function are
%%% contracted into a tuple. This transformation addresses the formal parameter
%%% list of all the clauses in the function definition as well as the
%%% actual parameter list in each perceptible (viz. by static analysis)
%%% call of the function. The transformation affects more than one module
%%% if the function is exported.
%%%
%%% == Conditions of applicability ==
%%%
%%% <ul>
%%%   <li>The function must be declared at the top level of a module, not a
%%%     function expression.</li>
%%%   <li>If the number of parameters that should be contracted into tuple is
%%%     greater than one the arity of function will be changed.
%%%     In this case the function with new arity should not conflict with
%%%     other functions.
%%%     <ul>
%%%       <li>If the function is not exported, it should not conflict with
%%%         other functions defined in the same module or imported from
%%%         other modules.</li>
%%%       <li>If the function is exported, then besides the requirement above,
%%%         for all modules where it is imported, it should not conflict with
%%%         functions defined in those modules or imported by those
%%%         modules.</li>
%%%     </ul></li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%%
%%% <ol>
%%%   <li>Change the formal parameter list in every clause of the function:
%%%     contract the formal arguments into a tuple pattern from the first
%%%     to the last argument that should be contracted.</li>
%%%   <li>If the function is exported from the module, then the export list
%%%     has to be modified: the arity of the function is updated with new
%%%     arity.</li>
%%%   <li>If the function is exported and another module imports it, then the
%%%     arity must be adjusted in the corresponding import list of that
%%%     module.</li>
%%%   <li>Implicit function references are turned into fun expressions that
%%%     call the function, and the result is handled as any other function
%%%     call.</li>
%%%   <li>For every application of the function, modify the actual parameter
%%%     list by contracting the actual arguments into a tuple from the first
%%%     to the last argument that should be contracted.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%%
%%% Dynamic function calls currently are not recognized therefore not handled.
%%% When the tool is able to detect these dynamic calls and determine the the
%%% parameters, then this transformation will able to handle these calls
%%% correctly.
%%%
%%% Currently, if the selection does not unambiguously determine the arguments
%%% of function that should be contracted into a tuple, the transformation is
%%% refused. When the tool is able to communicate with the user, this scenario
%%% can be resolved. (If the selection is an arity qualifier or an implicit
%%% function, the arguments aren't selected. So, currently these kinds of
%%% input are not permitted.)
%%%
%%% @author Kornél Horváth <kornel@inf.elte.hu>

-module(referl_tr_tuple_funpar).
-vsn("$Rev: 3185 $").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1, error_text/2]).

%%% ============================================================================
%%% Callbacks

%% @private
%% @spec prepare(Args::proplist()) -> TransformFunction::(() -> ok)
%% @throws {ModuleName::atom, ErrorCode::atom(), ErrorDetails::[term()]}
%% @doc  Prepare the transformation and get back a function that can make the
%%       transformation.
prepare(Args) ->
    % Checks and information collections
    {ModName, FunNode,FunName,FunArity,NewArity, Idx1,Idx2,
     CollideAutoImp, Exported} = find_nodes(Args),
    {FunDefs, FunApps, FunImpExps, FunImpls} =
        find_references(FunNode, FunArity, NewArity),
    % Transformation
    fun() ->
        % Transform function definitions
        Touchs1 = lists:foldl(
            fun(D, Acc) -> [transform_def(Idx1, Idx2, D)|Acc] end,
            [], FunDefs),
        % Transform implicit function expressions
        Touchs2 = lists:foldl(fun(I, Acc) ->
                [transform_impl(ModName,FunName,FunArity,Idx1,Idx2,Exported,I)|
                 Acc]
            end,
            Touchs1, FunImpls),
        % If function arity is change: transform import/expot lists
        Touchs3 = lists:foldl(
            fun(IE, Acc) -> [transform_impexp(FunName,NewArity, IE) | Acc] end,
            Touchs2, FunImpExps),
        % Transform function applications
        Touchs4 = lists:foldl(fun(A, Acc) ->
                [transform_app(ModName,FunName,Idx1,Idx2,CollideAutoImp,
                               Exported,A) | Acc]
            end,
            Touchs3, FunApps),
        % Touch nodes
        lists:foreach(fun(FileNode) -> ?Transform:touch(FileNode) end, Touchs4)
    end.


%% @private
%% @spec error_text(Code::atom(), Args::[term()]) -> string()
%% @doc
%% Give back the error message text for the transformation specific errors.
%% The error is specified by the `Code' and the `ArgsList'.
error_text(fun_expr, []) ->
    ?MISC:format("Function expressions are not supported!", []);
error_text(no_fundef, [ModName,FunName,FunArity]) ->
    ?MISC:format("The definition of the ~p:~p/~b function is not present in "
        "the tool!", [ModName,FunName,FunArity]);
error_text(zero_arity, [ModName,FunName]) ->
    ?MISC:format("The ~p:~p funtion with zero arity is not suitable!",
        [ModName,FunName]);
error_text(conflict, [ModName,FunName,NewArity]) ->
    ?MISC:format("The ~p:~p/~b funtion will be conflict with an existing "
        "function!", [ModName,FunName,NewArity]);
error_text(conflictAuto, [ModName,FunName,NewArity]) ->
    ?MISC:format("The ~p:~p/~b funtion will be conflict with an auto-imported "
        "function!", [ModName,FunName,NewArity]).




%%% ============================================================================
%%% Collect informations and checks


% Determine the semantical function node and the indices of the first nad last
% parameter that should contract into tuple
find_nodes(Args) ->
    case ?Args:expr_range(Args) of
        [EN] ->
            case ?Expr:kind(EN) of
                application -> find_nodes_(?Query:exec(EN,?Expr:children()));
                _           -> find_nodes_([EN])
            end;
        Exprs ->
            find_nodes_(Exprs)
    end.

% Determine the semantical function node and the indices of the first nad last
% parameter that should contract into tuple
find_nodes_(Exprs) ->
    [{LinkTag,ParentNode}] = ?Syn:parent(hd(Exprs)),
    ParentData             = ?Graph:data(ParentNode),
    % Check the parent and the link to the childs and get function node
    [FunNode] = case ParentData of
        #clause{kind=fundef} ->
            ?Check(pattern==LinkTag, ?RefErr0r(bad_range)),
            ?Query:exec(ParentNode, ?Query:seq([?Clause:form(),?Form:func()]));
        #expr{kind=application} ->
            ?Check(sub==LinkTag, ?RefErr0r(bad_range)),
            Funs = ?Query:exec(ParentNode, ?Expr:function()),
            ?Check(length(Funs)==1, ?LocalErr0r(fun_expr)),
            Funs;
        #clause{kind=funexpr} -> throw(?LocalErr0r(fun_expr));
        _                     -> throw(?RefErr0r(bad_range))
    end,
    [ModNode] = ?Query:exec(FunNode, ?Fun:module()),
    ModName   = ?Mod:name(ModNode),
    FunName   = ?Fun:name(FunNode),
    FunArity  = ?Fun:arity(FunNode),
    ?Check([]/=?Query:exec(FunNode, ?Fun:definition()),
        ?LocalError(no_fundef, [ModName,FunName,FunArity])),
    ?Check(0<FunArity ,?LocalError(zero_arity, [ModName,FunName])),
    % Calc argument indices
    Idx1_ = ?Graph:index(ParentNode, LinkTag, hd(Exprs)),
    Idx2_ = ?Graph:index(ParentNode, LinkTag, lists:last(Exprs)),
    {Idx1, Idx2} = case ParentData of
        #clause{kind=fundef}    -> {Idx1_,                  Idx2_  };
        #expr{kind=application} -> {lists:max([1,Idx1_-1]), Idx2_-1}
    end,
    % Get the module and check the collusion
    NewArity = FunArity-(Idx2-Idx1),
    Exported = ([]=/=?Query:exec(FunNode, ?Fun:exports())),
    CollideAutoImp = ?Fun:autoimported(FunName, NewArity),
    if
        FunArity/=NewArity ->
            check_collision(ModNode,ModName, FunNode,FunName,NewArity,
                            CollideAutoImp,Exported);
        true -> ok
    end,
    % Return with collected informations
    {ModName, FunNode,FunName,FunArity,NewArity, Idx1,Idx2,
     CollideAutoImp,Exported}.


% Check the collusion with other functions and auto imported functions
check_collision(ModNode,ModName, FunNode,FunName,NewArity, CollideAutoImp,
        Exported) ->
    ImpModNodes = ?Query:exec(FunNode, ?Fun:imported()),
    FunNodes = lists:usort(lists:flatten(
        [?Query:exec(MN, ?Mod:visible(FunName, NewArity)) ||
         MN<-[ModNode|ImpModNodes]])) -- [FunNode],
    ?Check([]==FunNodes,
           ?LocalError(conflict, [ModName,FunName,NewArity])),
    ?Check(not (CollideAutoImp andalso
                ([]=/=ImpModNodes orelse (not Exported))),
           ?LocalError(conflictAuto, [ModName,FunName,NewArity])).


% Collect definitions, applications, imports, exports of the function
find_references(FunNode, FunArity, NewArity) ->
    % Collect function definition clauses and patterns
    FunDefs = lists:map(
        fun(DN) -> {DN} end,
        ?Query:exec(FunNode, ?Query:seq([?Fun:definition(),?Form:clauses()]))),
    % Collect function application expressions and check application name is a
    % module qualifier
    FunApps = lists:map(
        fun(AN) -> {AN, []/=?Query:exec(AN, ?Expr:modq())} end,
        ?Query:exec(FunNode, ?Fun:applications())),
    % Collect arity qualifiers in import/export lists
    FunImpExps = if
            FunArity==NewArity -> [];
            true ->
                lists:map(
                    fun(IEN) -> {IEN, hd(?Graph:path(IEN, [{sub,2}])),
                        hd(?Graph:path(IEN, [{sub,2},{elex,1}])),
                        hd(?Syn:parent(IEN))}
                    end,
                    ?Query:exec(FunNode, ?Fun:impexps()))
        end,
    % Collect implicit function expressons and parent
    FunImpls = lists:map(
        fun(IN) -> {IN, hd(?Syn:parent(IN))} end,
        ?Query:exec(FunNode, ?Fun:implicits())),
    % Return collected informations
    {FunDefs, FunApps, FunImpExps, FunImpls}.




%%% ============================================================================
%%% Transformation


% Make transformation on function definition clauses
transform_def(Idx1, Idx2, {FunDefNode}) ->
    % Replace patterns with tuple
    % Lst contains the all child of FunDefNode function clause node beetwen
    % pattern/Idx1 and pattern/Idx2 childs:
    % [pattern/Idx1, clex/n, pattern/Idx1+1, clex/n+1, ... parretn/Idx2]
    ?Syn:replace(FunDefNode, {pattern, Idx1, Idx2-Idx1+1},
        fun(Lst) ->
            [?Syn:create(#expr{type=pattern,kind=tuple}, [{sub,N} || N<-Lst])]
        end),
    FunDefNode.


% Make transformation on function application expressions
transform_app(ModName, FunName, Idx1, Idx2, CollideAutoImp, Exported,
        {FunAppNode, ModuleQualifier}) ->
    % If the new function is collide with an auto imported function and the
    % first sub node of apllication (name node, sub/1) is not an module
    % qualifier than replace that with an module qualifier
    if
        CollideAutoImp andalso (not ModuleQualifier) andalso Exported ->
            MqExNode = create_module_qualifier(ModName, FunName),
            ?Syn:replace(FunAppNode, {sub,1,1}, [MqExNode]);
        true -> ok
    end,
    % Replace patterns with tuple
    % Lst contains the all child of FunAppNode function application node beetwen
    % sub/Idx1 and sub/Idx2 childs:
    % [sub/Idx1, elex/n, sub/Idx1+1, elex/n+1, ... sub/Idx2]
    ?Syn:replace(FunAppNode, {sub, 1+Idx1, Idx2-Idx1+1},
        fun(Lst) ->
            [?Syn:create(#expr{type=expr, kind=tuple}, [{sub,N} || N<-Lst])]
        end),
    FunAppNode.


% Make transformation on arity qualifiers in import/export lists
transform_impexp(_FunName, NewArity,
        {IENode,ArityNode,_ArityToken,{_ParentLink,_ParentNode}}) ->
    % Update arity node
    NewArityNode = ?Syn:create(#expr{type=expr, kind=integer, value=NewArity},
                               [?MISC:to_list(NewArity)]),
    ?Syn:replace(IENode, {node, ArityNode}, [NewArityNode]),
    IENode.


% Make transformation on implicit function expressions
transform_impl(ModName, FunName, FunArity, Idx1, Idx2, Exported,
        {FunImplNode,{_ParentLink,ParentNode}}) ->
    % Create function expression which contains application expression
    FunExprNode = create_fun_expr(ModName,FunName,FunArity,Idx1,Idx2,Exported),
    % Replace implicit function expression with function expression
    ?Syn:replace(ParentNode, {node,FunImplNode}, [FunExprNode]),
    ParentNode.



%%% ----------------------------------------------------------------------------
%%% Subtree generation functions

% Create module qualifier
create_module_qualifier(ModName, FunName) ->
    MExNode = ?Syn:create(#expr{type=expr, kind=atom, value=ModName},
                          [?MISC:to_list(ModName)]),
    FExNode = ?Syn:create(#expr{type=expr, kind=atom, value=FunName},
                          [?MISC:to_list(FunName)]),
    ?Syn:create(#expr{type=expr, kind=infix_expr, value=':'},
                [{sub, MExNode}, {sub, FExNode}] ).


% Create function expression
create_fun_expr(ModName, FunName, FunArity, Idx1, Idx2, Exported) ->
    % --- Application ---
    MqNode = if
        Exported -> create_module_qualifier(ModName, FunName);
        true     -> ?Syn:create(#expr{type=expr, kind=atom, value=FunName},
                                [?MISC:to_list(FunName)])
    end,
    % Create parameters and variables
    ParNameStrs = [ lists:concat(["P",I]) || I<-lists:seq(1,FunArity) ],
    VarNodes = [?Syn:create(#expr{type=expr, kind=variable, value=P}, [P]) ||
                P<-ParNameStrs ],
    % Split variable list
    {BeforeVarNodes, TAVarNodes   } = lists:split(Idx1-1, VarNodes),
    {TupleVarNodes,  AfterVarNodes} = lists:split(Idx2-Idx1+1, TAVarNodes),
    % Create variable tuple // {expr,pattern,tuple,undefined}
    TupleNode = ?Syn:create(#expr{type=pattern, kind=tuple},
                            [{sub, TupleVarNodes}] ),
    % Create application
    AppNode = ?Syn:create(#expr{type=expr, kind=application},
                [{sub, [MqNode|BeforeVarNodes] ++ [TupleNode|AfterVarNodes]}] ),
    % --- Function expression ---
    % Create pattern nodes
    PatternNodes = [
        ?Syn:create(#expr{type=pattern,kind=variable,value=P}, [P]) ||
        P<-ParNameStrs ],
    % Create function expression clause
    ClauseNode = ?Syn:create(#clause{type=scope, kind=funexpr},
                             [{pattern,PatternNodes},{body,AppNode}]),
    % Create function expression
    ?Syn:create(#expr{type=expr, kind=fun_expr}, [{exprcl,ClauseNode}]).


