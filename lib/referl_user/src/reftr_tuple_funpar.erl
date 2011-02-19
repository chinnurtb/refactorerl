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
%%%     greater than one, the arity of function will be changed.
%%%     In this case, the function with new arity should not conflict with
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

-module(reftr_tuple_funpar).
-vsn("$Rev: 4763 $"). % for emacs"

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks


%% @private
%% @spec error_text(Code::atom(), Args::[term()]) -> string()
%% @doc
%% Give back the error message text for the transformation specific errors.
%% The error is specified by the `Code' and the `ArgsList'.
error_text(fun_not_found, []) ->
    ?MISC:format("Internal error: function representation not found", []);
error_text(fun_expr, []) ->
    ?MISC:format("Fun expressions are not supported", []);
error_text(no_fundef, {ModName,FunName,OrigArity}) ->
    ?MISC:format("The definition of ~p:~p/~b is not available",
                 [ModName,FunName,OrigArity]);
error_text(zero_arity, [ModName,FunName]) ->
    ?MISC:format("Function ~p:~p should not have zero arity",
                 [ModName,FunName]);
error_text(conflict, [ModName,FunName,NewArity]) ->
    ?MISC:format("Function ~p:~p/~b is already defined",
                 [ModName,FunName,NewArity]);
error_text(conflictAuto, [ModName,FunName,NewArity]) ->
    ?MISC:format("Function ~p:~p/~b conflicts with an auto-imported function",
                 [ModName,FunName,NewArity]).

%% @private
%% @spec prepare(Args::proplist()) -> TransformFunction::(() -> ok)
%% @throws {ModuleName::atom, ErrorCode::atom(), ErrorDetails::[term()]}
%% @doc  Prepares the transformation and returns the transformation function.
prepare(Args) ->
    Exprs = get_selected_expr_range_tuple(Args),
    ?Check([] =/= Exprs, ?RefErr0r(bad_range)),
    [Expr1|_] = Exprs,

    [{_, Parent}] = ?Syn:parent(hd(Exprs)),
    ParentType    = get_node_type(Parent),
    ?Check(ParentType =/= funexpr, ?LocalErr0r(fun_expr)),
    ?Check(lists:member(ParentType, [fundef, arglist]), ?RefErr0r(bad_range)),

    Fun = get_fun_node(ParentType, Parent),
    {Mod, ModFunArity} = ?Fun:mod_fun_arity(Fun),
    {ModName, FunName, OrigArity} = ModFunArity,
    ?Check([] =/= ?Query:exec(Fun, ?Fun:definition()),
           ?LocalError(no_fundef, ModFunArity)),
    ?Check(0 < OrigArity, ?LocalError(zero_arity, [ModName,FunName])),

    IdxLen     = length(Exprs),
    Idx1       = first_expr_idx(ParentType, Parent, Expr1),
    NewArity   = OrigArity - IdxLen + 1,

    ExpMods    = ?Query:exec(Fun, ?Fun:exports()),
    ImpMods    = ?Query:exec(Fun, ?Fun:imported()),
    IsImported = ImpMods =/= [],
    IsExported = ExpMods =/= [],
    IsAutoImp  = ?Fun:is_autoimported(FunName, NewArity),
    case OrigArity =:= NewArity of
        true  -> ok;
        false ->
            ModNewFunArity = [ModName,FunName,NewArity],
            VisibleFuns    = visible_funs([Mod|ImpMods], FunName, NewArity),
            ?Check([] =:= VisibleFuns, ?LocalError(conflict, ModNewFunArity)),
            ?Check(not IsAutoImp orelse (not IsImported andalso IsExported),
                   ?LocalError(conflictAuto, ModNewFunArity))
    end,

    FunDefs = ?Query:exec(Fun, ?Query:seq([?Fun:definition(),?Form:clauses()])),
    Impls   = ?Query:exec(Fun, ?Fun:implicits()),
    {_Links, ImplParents} = lists:unzip(lists:flatten([?Syn:parent(Impl)
                                                            || Impl <- Impls])),
    FunImpExpsWArityNode  = fun_impexps(OrigArity, NewArity, Fun),
    {FunImpExps, _}       = lists:unzip(FunImpExpsWArityNode),

    [fun() ->
        ?Expr:expand_funexpr(Impls)
     end,
     fun(_) ->
        Apps = ?Query:exec(Fun, ?Fun:applications()),
        case IsAutoImp andalso IsExported of
            false -> ok;
            true  ->
                [?Expr:add_modq(App, ModName) || App <- Apps,
                                                 not ?Expr:app_has_modq(App)]
        end,
        Apps
     end,
     % todo Use ?Syn:get_comment and put_comment to preserve comments
     fun(Apps) ->
        [make_tuple_in_fundef(FunDef, Idx1, IdxLen) || FunDef <- FunDefs],
        [make_tuple_in_application(App, Idx1, IdxLen) || App <- Apps],
        [change_impexp_arity(ImpExp, OldArityNode, NewArity)
            || {ImpExp, OldArityNode} <- FunImpExpsWArityNode],
        [?Transform:touch(Node) || Node <- FunDefs ++ ImplParents ++ Apps ++ FunImpExps]
    end].

%%% ============================================================================
%%% Implementation


make_tuple_in_fundef(FunDef, Idx1, IdxLen) ->
    ?Syn:replace(FunDef, {pattern, Idx1, IdxLen}, tuple_replacer()).

change_impexp_arity(ImpExp, OldArityNode, NewArity) ->
    ?Syn:replace(ImpExp, {node, OldArityNode}, [new_arity_node(NewArity)]).

make_tuple_in_application(App, Idx1, IdxLen) ->
    [ArgList] = ?Query:exec(App, ?Expr:child(2)),
    ?Syn:replace(ArgList, {esub, Idx1, IdxLen}, tuple_replacer()).

tuple_replacer() ->
    fun(Lst) ->
            CLst =
                [begin
                     CNodes = ?Syn:copy(Node),
                     {Node, CNode} = lists:keyfind(Node, 1, CNodes),
                     CNode
                 end|| Node <- Lst],
            [?Syn:create(#expr{type=tuple}, [{esub, CLst}])]
    end.


get_fun_node(fundef, Parent) ->
    ?Query:exec1(Parent, ?Query:seq(?Clause:form(), ?Form:func()),
                 ?LocalErr0r(fun_not_found));
get_fun_node(arglist, Parent) ->
    ?Query:exec1(Parent, ?Query:seq(?Expr:parent(), ?Expr:function()),
                 ?LocalErr0r(fun_expr)).


fun_impexps(OrigArity, NewArity, _) when OrigArity =:= NewArity ->
    [];
fun_impexps(_, _, Fun) ->
    ImpExps  = ?Query:exec(Fun, ?Fun:impexps()),

    ArityIdx = 2,
    [{ImpExp, ?Query:exec1(ImpExp, ?Expr:child(ArityIdx), ?RefErr0r(internal))}
        || ImpExp <- ImpExps].


%% Returns the type of the node.
%% todo Eliminate ?ESG call.
get_node_type(Parent) ->
    case ?ESG:data(Parent) of
        #clause{type = Type} -> Type;
        #expr{type = Type}   -> Type
    end.


visible_funs(Mods, FunName, NewArity) ->
    lists:usort(?Query:exec(Mods, ?Mod:visible(FunName, NewArity))).


new_arity_node(Arity) ->
    ?Syn:construct({integer, Arity}).

%% Returns the range of expressions that describes what is to be tupled.
%% If a function application is selected, its argument nodes are returned,
%% since the number of these nodes gives the arity of the new tuple.
get_selected_expr_range_tuple(Args) ->
    case ?Args:expr_range(Args) of
        [Expr] ->
            case ?Expr:type(Expr) of
                application -> ?Query:exec(Expr, ?Expr:children());
                _           -> [Expr]
            end;
        ExprRange ->
            ExprRange
    end.


%% @doc Returns the index of the first expression and the length of
%% the selection.
first_expr_idx(Type, Parent, Expr1) ->
    All =
        case Type of
            fundef      -> ?Query:exec(Parent, ?Clause:patterns());
            arglist ->
                ?Query:exec(Parent, ?Expr:children())
        end,
    Before1  = lists:takewhile(fun(Node) -> Node =/= Expr1 end, All),
    length(Before1) + 1.
