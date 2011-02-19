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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc Turn lists:map and lists:filter calls into list comprehension syntax,
%%% or do it backwards.
%%%
%%% The two main cases of the transformation:
%%% <ul>
%%%  <li>lists:map/2 or lists:filter/2 or lists:foreach/2
%%%      to list comprehension</li>
%%%  <li>list comprehension to lists:map/2 and/or lists:filter/2</li>
%%% </ul>
%%% <h2>lists:map/2 or lists:filter/2 or lists:foreach/2
%%%      to list comprehension</h2>
%%% <h4>Informations about the transformation:</h4>
%%% <ul>
%%%  <li>The transformation is applied only if
%%%      lists:map/2 or
%%%      lists:filter/2 or
%%%      lists:foreach/2 is selected</li>
%%%  <li>If the first parameter is an explicit or an implicit fun expression
%%%      the arity of the function must be equal to 2</li>
%%%  <li>If the first parameter is not a fun
%%%      it is not checked what it really is</li>
%%%  <li>It is not checked wheter the second parameter really a list is</li>
%%% </ul>
%%% <h2>List comprehension to
%%%     lists:map/2 and/or lists:filter/2</h2>
%%% <h4>Informations about the transformation:</h4>
%%% <ul>
%%%  <li>The transformation does not supports list comprehensions
%%%      that contain more than one list generator</li>
%%%  <li>The result is a lists:filter/2 or a lists:map/2 or a composition of a
%%%      lists:filter/2 and a
%%%      lists:map/2</li>
%%%  <li>The transformation does not optimise according to
%%%      unused variables</li>
%%%  <li>It is not checked wheter the generator really a list is</li>
%%% </ul>
%%% @todo Should handle multi-level applications of lists:map and lists:filter.
%%%
%%%
%%% @author Csaba Imre Zempleni <zecoaat@inf.elte.hu>

-module(reftr_list_comp).
-vsn("$Rev: 5278 $ ").

%%% Exports
-export([prepare/1, error_text/2]).

%%% Includes
-include("user.hrl").

%%% ============================================================================
%%% Errors

%% @private
error_text(should_be_fun, _) ->
    ["The selection should contain one of these functions: ",
     ?MISC:funlist_text([{lists, map, 2}, {lists, filter, 2}, {lists, foreach, 2}])];
error_text(bad_arity_funexpr, [_Arity]) ->
    ["The arity of the function has to be one"];
error_text(too_complex, _) ->
    ["The structure of the list comp. is too complex ",
     "(The transformation is not recommened.)"].

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    case ?Args:expr_range(Args) of
        [Expr] -> Expr;
        _      -> Expr = throw(?RefErr0r(bad_kind))
    end,
    case ?Expr:type(Expr) of
        application ->
            [Fun|_]             = ?Query:exec(Expr, ?Expr:functions()),
            {_Mod, ModFunArity} = ?Fun:mod_fun_arity(Fun),
            ?Check(ModFunArity =:= {lists, map, 2} orelse
                   ModFunArity =:= {lists, foreach, 2} orelse
                   ModFunArity =:= {lists, filter, 2},
                   ?LocalError(should_be_fun, [])),

            case ModFunArity of
                {lists, filter, 2} ->
                    [_, ArgList] =
                        ?Query:exec(Expr, ?Expr:children()),
                    [ExprFun, Generator1] = 
                        ?Query:exec(ArgList, ?Expr:children()),
                    Generator = copy(Generator1), 
                    %[ExprFun, Generator] =
                    %    copy(?Query:exec(ArgList, ?Expr:children())),
                    Type = ?Expr:type(ExprFun),
                    Info = from_app_filter_info(Type, ExprFun),
                    [{_, Parent}] = ?Syn:parent(Expr),
                    UsedVarNames  = 
                        [ ?Var:name(V) || 
                            V <- ?Query:exec(Expr, ?Expr:variables())],
                    VarName       = 
                        ?Var:new_varname(Expr, "ListVar", UsedVarNames),
                    ?Transform:touch(Expr),
                    fun() ->
                            ListComp = from_filter_create_listcomp(
                                         Generator, VarName, Type, Info),
                            ?Syn:replace(Parent, {node, Expr}, [ListComp])
                    end;
                _MapOrForeach ->
                    [_, ArgList] =
                        ?Query:exec(Expr, ?Expr:children()),
                    [ExprFun, Generator1] = 
                        ?Query:exec(ArgList, ?Expr:children()),
                    Generator = copy(Generator1), 
                    %[ExprFun, Generator] =
                    %    copy(?Query:exec(ArgList, ?Expr:children())),
                    Type          = ?Expr:type(ExprFun),
                    Info          = from_app_info_by_kind(Type, ExprFun),
                    [{_, Parent}] = ?Syn:parent(Expr),
                    UsedVarNames  = 
                        [ ?Var:name(V) || 
                            V <- ?Query:exec(Expr, ?Expr:variables())],
                    VarName       = 
                        ?Var:new_varname(Expr, "ListVar", UsedVarNames),
                    ?Transform:touch(Expr),
                    fun() ->
                            ListComp = from_app_create_listcomp(
                                         Generator, VarName, Type, Info),
                            ?Syn:replace(Parent, {node, Expr}, [ListComp])
                    end
            end;
        list_comp ->
            Bodies = ?Query:exec(Expr, ?Query:seq(?Expr:clause(2), 
					          ?Clause:body())),
            {ListGens, CFilters} =
                lists:partition(fun(X) ->
                                        ?Expr:type(X) =:= list_gen
                                end, Bodies),
            ?Check(length(ListGens) =:= 1, ?LocalError(too_complex, [])),
            [ListGen] = ListGens,
            Filters = [?Query:exec(X, ?Expr:clause(1)) || X <- CFilters ],
            [PatternFilterNode] =
                copy(?Query:exec(ListGen,
				 ?Query:seq(?Expr:clause(1), 
					    ?Clause:pattern(1)))),
            PatternFilter = case ?Expr:type(PatternFilterNode) of
                                variable -> {var, PatternFilterNode};
                                _ -> {pattern, PatternFilterNode}
                            end,
            [Gen] = copy(?Query:exec(
			    ListGen,
			    ?Query:seq(?Expr:clause(2), ?Clause:body(1)))),
            [HExpr] = ?Query:exec(Expr, ?Query:seq(?Expr:clause(1),
					           ?Clause:body(1))),
            AppFunNode = from_lc_make_appfunnode(HExpr, PatternFilterNode),
            [{_, Parent}] = ?Syn:parent(Expr),

	    Same = same_expr(AppFunNode, PatternFilterNode),
	    
	    Andalsos = make_andalsos(Filters),
	    FunBody = make_funbody(Andalsos, PatternFilterNode),

            ?Transform:touch(Expr),
            fun() ->
		    Application = from_lc_create_app(
				    AppFunNode, PatternFilter,
				    Gen, Same, FunBody),
                    ?Syn:replace(Parent, {node, Expr}, [Application])
            end;
        _ ->
            throw(?RefErr0r(bad_kind))
    end.

%%% ============================================================================
%%% Implementation: application to list comp.


%% The lists:map/2 function can be applied either to
%% an implicit function with arity 1 or a fun expression.
%% The function returns the detected role with additional information,
%% and the pattern of the created list expression or
%% `new' if a pattern with a new name is to be created.
from_app_info_by_kind(implicit_fun, ExprFun) ->
    [FunNode, ArityNode|_] = ?Query:exec(ExprFun, ?Expr:children()),
    Arity = ?Expr:value(ArityNode),
    ?Check(Arity =:= 1, ?LocalError(bad_arity_funexpr, [ArityNode])),
    copy(FunNode);
from_app_info_by_kind(fun_expr, ExprFun) ->
    Clauses     = ?Query:exec(ExprFun, ?Expr:clauses()),
    PrePatterns =
	[ copy(?Query:exec(Cl, ?Clause:patterns())) || Cl <- Clauses ],
    ?Check(length(hd(PrePatterns)) =:= 1,
           ?LocalError(bad_arity_funexpr, [ExprFun])),
    Patterns    = [ {?Expr:type(hd(P)), P} || P <- PrePatterns ],
    Guards      = [ copy(?Query:exec(Cl, ?Clause:guard())) || Cl <- Clauses ],
    Bodies      = [ copy(?Query:exec(Cl, ?Clause:body())) || Cl <- Clauses ],
    lists:zip3(Patterns, Guards, Bodies);
from_app_info_by_kind(_, ExprFun) ->
    copy(ExprFun).

%% Gathers information about filter application
from_app_filter_info(implicit_fun, ExprFun) ->
    [FunNode, ArityNode|_] = ?Query:exec(ExprFun, ?Expr:children()),
    Arity = ?Expr:value(ArityNode),
    ?Check(Arity =:= 1, ?LocalError(bad_arity_funexpr, [ArityNode])),
    copy(FunNode);
from_app_filter_info(fun_expr, ExprFun) ->
    Clauses = ?Query:exec(ExprFun, ?Expr:clauses()),
    ?Check(length(?Query:exec(hd(Clauses), ?Clause:patterns())) =:= 1,
           ?LocalError(bad_arity_funexpr, [ExprFun])),
    Patterns = [ begin
                     [P] = ?Query:exec(C, ?Clause:patterns()),
                     {copy(P), ?Expr:type(P)}
                 end || C <- Clauses ],
    Guards = [ transform_guards(copy(?Query:exec(C, ?Clause:guard()))) || 
		      C <- Clauses ],
    Bodies = [ begin
		   B = ?Query:exec(C, ?Clause:body()),
		   LB = lists:last(B),
		   {copy(B), is_boolean_expr(LB)}
	       end || C <- Clauses ],
    lists:zip3(Patterns, Guards, Bodies);
from_app_filter_info(_Else, ExprFun) ->
    copy(ExprFun).

transform_guards([]) -> [];
transform_guards([Node]) -> 
    case ?Expr:value(Node) of
	NotSemicolon when NotSemicolon =/= ';' -> [Node];
	';' -> [transform_guards(Node)]
    end;
transform_guards(Node) ->
    case ?Expr:value(Node) of
	Ch when Ch =:= ';'; Ch =:= ',' -> 
	    Atom = case Ch of 
		       ';' -> 'orelse';
		       ',' -> 'andalso'
		   end,
	    case ?Query:exec(Node, ?Expr:children()) of
		[] -> Node;
		[A, B] -> {{infix_expr, Atom}, 
			   {paren, transform_guards(copy(A))},
			   {paren, transform_guards(copy(B))}}
	    end;
	_Else -> Node
    end.	    

%% Creates a list comprehension from map or foreach and returns its node.
from_app_create_listcomp(Generator, VarName, Type, Info) ->
    Pattern =
        case {Type, Info} of
            {fun_expr, [{{_, SinglePattern}, [], _}]}  ->
                SinglePattern;
            _ ->
                {var_pattern, VarName}
        end,
    CompHead =
        case {Type, Info} of
            {implicit_fun, Fun} ->
                {app, Fun, [{var, VarName}]};
            {fun_expr, [{{variable, _}, [], [SingleBody]}]} ->
                SingleBody;
            {fun_expr, [{{variable, _}, [], Bodies}]} ->
                {block_expr, Bodies};
            {fun_expr, Cls} ->
                Branches = [{pattern, Patterns, Guards, Bodies}
                            || {{_, Patterns}, Guards, Bodies} <- Cls],
                {'case', {var, VarName}, Branches};
            {_, Node} ->
                {app, {paren, Node}, [{var, VarName}]}
        end,

    ?Syn:construct({list_comp, CompHead, [{list_gen, Pattern, Generator}]}).

-define(IsVar(IV), (IV =:= variable orelse IV =:= joker)).
-define(IsTrue(IT), (IT =:= true orelse IT =:= always)).

%% Creates a list comprehension from filter and returns its node.
from_filter_create_listcomp(Generator, VarName, Type, Info) ->
    {Head, Pattern} =
        case {Type, Info} of
            {fun_expr, [{{Var, variable}, [], _}]} ->
		{{var, ?Expr:value(Var)}, Var};
            {fun_expr, [{{Var, variable}, _, _},
                        {{_, variable}, [], {_, endpoint}}]} ->
		{{var, ?Expr:value(Var)}, Var};
            {fun_expr, [{{Something, _}, _, {_, TA1}},
                        {{_, VJ1}, [], {_, endpoint}}]} when
		  ?IsVar(VJ1) andalso ?IsTrue(TA1) ->
		{copy(Something), Something};
            _Else ->
		{{var, VarName}, {var_pattern, VarName}}
        end,
    PreFilters =
        case {Type, Info} of
            {implicit_fun, Fun} ->
                [{app, Fun, [{var, VarName}]}];
            {fun_expr, [{{_, variable}, [], {[Body], TA}}]} when ?IsTrue(TA) ->
		make_bodies([Body], TA);
            {fun_expr, [{{_, variable}, [], {Bodies, TA}}]} when ?IsTrue(TA) ->
                [{block_expr, Bodies}];
            {fun_expr, [{_Var, Guards, {[Body], TA}},
                        {{_, VJ}, [], {_, endpoint}}]} when
		  ?IsVar(VJ) andalso ?IsTrue(TA) ->
                Guards ++ make_bodies([Body], TA);
            {fun_expr, [{_Var, Guards, {Bodies, TA}},
                        {{_, VJ}, [], {_, endpoint}}]} when
		  ?IsVar(VJ) andalso ?IsTrue(TA) ->
                [{block_expr, Guards ++ Bodies}];
            {fun_expr, Clauses} ->
                Branches = [{pattern, [Patterns], Guards, Bodies}
                            || {{Patterns, _}, Guards, {Bodies, _}} <- Clauses],
                [{'case', {var, VarName}, Branches}];
            {_Else2, Node} ->
                [{app, {paren, Node}, [{var, VarName}]}]
        end,

    Filters = [ {filter, Filter} || Filter <- PreFilters ],

    ?Syn:construct({list_comp, Head,
                    [{list_gen, Pattern, Generator},
                      Filters]}).

make_bodies(_Bodies, always) -> [];
make_bodies(Bodies, true) -> Bodies.

%%% ============================================================================
%%% Implementation: list comp to application.
from_lc_make_appfunnode(HExpr, PatternFilterNode) ->
    case ?Expr:type(HExpr) of
        block_expr ->
	    copy(?Query:exec(
		    HExpr, ?Query:seq(?Expr:clause(1), ?Clause:body())));
	case_expr -> case_to_fun(HExpr, PatternFilterNode);
        _ -> [copy(HExpr)]
    end.

from_lc_create_app(AppFunNode, PatternFilter, Gen, Same, Funbody) ->
    FilterNode =
        case {PatternFilter, Funbody} of
            {{var, _}, {atom, true}} -> Gen;
	    {_Else, FilterFun = {'fun', _}} ->
		{app, {{atom, lists}, ':', {atom, filter}}, [FilterFun, Gen]};
            {_Else, Funbody} -> 
		FilterFun = make_lists_filter_fun(PatternFilter, Funbody),
		{app, {{atom, lists}, ':', {atom, filter}}, [FilterFun, Gen]}
        end,
    {_, AppFunPattern} = PatternFilter,
    Structure = 
	case Same of
	    true -> FilterNode;
	    false ->
		Fun = case AppFunNode of
			  {'fun', _} -> AppFunNode;
			  _ -> 
			      AppFunPatternCopy = copy(AppFunPattern),
			      {'fun', [{fun_scope, 
					[AppFunPatternCopy], [], [AppFunNode]}]}
		      end,
		{app, {{atom, lists}, ':', {atom, map}}, [Fun, FilterNode]}
	end,
    ?Syn:construct(Structure).

make_lists_filter_fun({var, Var}, Funbody) ->
    Clause = {fun_scope, [Var], [], [Funbody]},
    {'fun', [Clause]};
make_lists_filter_fun({pattern, Pattern}, Funbody) ->
    True = {fun_scope, [Pattern], [], [Funbody]},
    False = {fun_scope, [{var_pattern, "_"}], [], [{atom, false}]},
    {'fun', [True, False]}.

-define(body(PBody), copy(hd(?Query:exec(PBody, ?Clause:body(1))))).

make_andalsos([]) -> {atom, true};
make_andalsos([A]) -> hd(?Query:exec(A, ?Clause:body(1)));%%?body(A);
make_andalsos([A, B]) -> {{infix_expr, 'andalso'}, ?body(A), ?body(B)};
make_andalsos([A | Tail]) ->
    {{infix_expr, 'andalso'}, ?body(A), make_andalsos(Tail)}.

make_funbody(True = {atom, true}, _) -> True;
make_funbody(Andalsos = {{infix_expr, 'andalso'}, _, _}, _) -> Andalsos;
make_funbody(Single, Pattern) ->
    case ?Expr:type(Single) of
	block_expr ->
	    copy(?Query:exec(Single, 
			     ?Query:seq(?Expr:clause(1), ?Clause:body())));
	case_expr -> case_to_fun(Single, Pattern);
	_ -> copy(Single)
    end.

case_to_fun(Case, Pattern) ->
    CasePattern = ?Query:exec(
		     Case, ?Query:seq(?Expr:clause(1), ?Clause:body())),
    case same_expr(CasePattern, Pattern) of
	false -> copy(Case);
	true ->
	    [_ | Clauses] = ?Query:exec(Case, ?Expr:clauses()),
	    Scopes = [ begin
			   P = copy(?Query:exec(X, ?Clause:patterns())),
			   G = copy(?Query:exec(X, ?Clause:guard())),
			   B = copy(?Query:exec(X, ?Clause:body())),
			   {fun_scope, P, G, B}
		       end || X <- Clauses ],
	    {'fun', Scopes}
    end.

is_boolean_expr(Expr) ->
    Type = ?Expr:type(Expr),
    Value = ?Expr:value(Expr),
    if Type =:= infix_expr andalso
       (Value =:= '==' orelse Value =:= '=:=' orelse
        Value =:= '/=' orelse Value =:= '=/=' orelse
        Value =:= '<' orelse Value =:= '=<' orelse
        Value =:= '>' orelse Value =:= '>=' orelse
        Value =:= 'andalso' orelse Value =:= 'and' orelse
        Value =:= 'orelse' orelse Value =:= 'or') -> true;
       Type =:= atom andalso Value =:= true -> always;
       Type =:= atom andalso Value =:= false -> endpoint;
       Type =:= application -> true; % maybe data flow?
       true -> false
    end.

same_expr([Expr1], Expr2) ->
    Tree1 = build_expr_tree(Expr1),
    Tree2 = build_expr_tree(Expr2),
    Tree1 =:= Tree2;
same_expr(_, _) ->
    false.

build_expr_tree(Expr) ->
    Data = expr_data(Expr),
    Children = ?Query:exec(Expr, ?Expr:children()),
    {Data, [ build_expr_tree(X) || X <- Children ]}.

expr_data(Expr) ->
    {?Expr:type(Expr), ?Expr:value(Expr)}.

copy([]) -> [];
copy([This | Tail]) -> [copy(This) | copy(Tail)];
copy(This) -> proplists:get_value(This, ?Syn:copy(This)).
