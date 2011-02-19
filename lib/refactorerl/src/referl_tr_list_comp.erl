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

%%% @doc Turn lists:map and lists:filter calls into list comprehension syntax,
%%%  or do it backwards. (Only lists:map to list comprehension implemented yet.)
%%%
%%%
%%% @author Csaba Imre Zempleni <zecoaat@inf.elte.hu>

-module(referl_tr_list_comp).
-vsn("$Rev$").

%%% Exports
-export([prepare/1, error_text/2]).

%%% Includes
-include("refactorerl.hrl").

%%% ============================================================================
%%% Errors

%% @private
error_text(not_map_filter, []) ->
    ["The selected section is not lists:map"];
error_text(bad_arity_funexpr, []) ->
    ["The map function's arity must be 1"].

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    ListExpr = ?Args:expr_range(Args),
    ?Check(length(ListExpr) =:= 1, ?RefError(bad_kind, [])),
    Expr = hd(ListExpr),
    case ?Expr:kind(Expr) of
	application ->
	    prepare_app(Expr);
	list_comp ->
	    prepare_list(Expr);
	_ ->
	    ?Check(false, ?RefError(bad_kind, []))
    end.

%%% ============================================================================
%%% lists:map to list comp section
    
prepare_app(Expr) ->
    ExprFun = hd(?Query:exec(Expr, ?Expr:functions())),
    ?Check(element(1, map_or_filter(ExprFun)), 
	   ?LocalError(not_map_filter, [])),
    [DoFun] = ?Query:exec(Expr, ?Expr:child(2)),
    {FunType, Desc, VarType} = check_app_dofun(DoFun),
    [DoList] = ?Query:exec(Expr, ?Expr:child(3)),
    [{_, Parent}] = ?Syn:parent(Expr),
    ?Transform:touch(Expr),
    VarName = get_allowed_varname(Expr),
    fun() ->
	    ListGenExpr = ?Syn:create(#clause{kind=expr}, [{body, DoList}]),
	    {Var, Pattern} = 
		case VarType of
		    new ->
			{?Syn:create(#expr{kind=variable}, [VarName]),
			 ?Syn:create(#expr{type=pattern, 
					   kind=variable},
				     [VarName])};
		    Original -> {[], Original}
		end,
	    ListGenPExpr = ?Syn:create(#clause{kind=pexpr}, 
				       [{pattern, Pattern}]),
	    ListGen = ?Syn:create(#expr{kind=list_gen},
				  [{exprcl, ListGenPExpr}, 
				   {exprcl, ListGenExpr}]),
	    Compr = ?Syn:create(#clause{kind=compr}, [{body, ListGen}]),
	    HExprBody = 
		case {FunType, Desc} of
		    {implicit_fun, FunNode} ->
			?Syn:create(#expr{kind=application},
				    [{sub, FunNode}, {sub, Var}]);
		    {fun_expr, {simple, Body}} -> Body;
		    {fun_expr, {begin_end, Bodys}} ->
			Clause = ?Syn:create(#clause{kind=block},
					     [{body, Bodys}]),
			?Syn:create(#expr{kind=block_expr},
				    [{exprcl, Clause}]);
		    {fun_expr, {case_expr, ClauseData}} ->
			Branches = 
			    [ case Guards of
				  [] ->
				      ?Syn:create(#clause{kind=pattern},
						  [{pattern, Patterns},
						   {body, Bodys}]);
				  _ ->
				      ?Syn:create(#clause{kind=pattern},
						  [{pattern, Patterns},
						   {guard, Guards},
						   {body, Bodys}])
			      end || {Patterns, Guards, Bodys} <- ClauseData ],
			HeadCl = ?Syn:create(#clause{kind=expr},
					     [{body, Var}]),
			?Syn:create(#expr{kind=case_expr},
				    [{headcl, HeadCl}, {exprcl, Branches}]);
		    {something_else, Node} ->
			Paren = ?Syn:create(#expr{kind=parenthesis}, 
					    [{sub, Node}]),
			?Syn:create(#expr{kind=application},
				    [{sub, Paren}, {sub, Var}])
		end,
	    HExpr = ?Syn:create(#clause{kind=hexpr}, [{body, HExprBody}]),
	    ListComp = ?Syn:create(#expr{kind=list_comp}, 
				   [{exprcl, HExpr}, {exprcl, Compr}]),
	    ?Syn:replace(Parent, {node, Expr}, [ListComp])
    end.

map_or_filter(Fun) ->
    FunName = ?Fun:name(Fun),
    ModName = ?Mod:name(hd(?Query:exec(Fun, ?Fun:module()))),
    Arity = ?Fun:arity(Fun),
    {ModName =:= lists andalso 
     (FunName =:= map
%      orelse FunName =:= filter 
%      orelse FunName =:= foreach
     ) andalso 
     Arity =:= 2, FunName}.

check_app_dofun(ExprFun) ->
    Kind = ?Expr:kind(ExprFun),
    case Kind of
	fun_expr ->
	    Clauses = ?Query:exec(ExprFun, ?Expr:clauses()),
	    First = hd(Clauses),
	    Patterns = ?Query:exec(First, ?Clause:patterns()),
	    ?Check(length(Patterns) =:= 1, ?LocalError(bad_arity_funexpr, [])),
	    HasGuard = (length(?Query:exec(First, ?Clause:guard())) > 0),
	    FirstBodys = ?Query:exec(First, ?Clause:body()),
	    Type = if(HasGuard orelse length(Clauses) > 1) ->
			   ClauseData =
			       [ begin 
				     CPatterns = 
					 ?Query:exec(Clause,
						     ?Clause:patterns()),
				     Guards =
					 ?Query:exec(Clause,
						     ?Clause:guard()),
				     Bodys =
					 ?Query:exec(Clause,
						     ?Clause:body()),
				     {CPatterns, Guards, Bodys}
				 end || Clause <-  Clauses ],
			   {case_expr, ClauseData};
		     (length(FirstBodys) > 1) -> {begin_end, FirstBodys};
		     (true) -> {simple, hd(FirstBodys)}
		   end,
	    VarType = if(element(1, Type) == case_expr) -> new;
			(true) -> hd(Patterns)
		      end,
	    {fun_expr, Type, VarType};
	 implicit_fun ->
	    [ArityNode] = ?Query:exec(ExprFun, ?Expr:child(2)),
	    ?Check(?Expr:value(ArityNode) =:= 1, 
		   ?LocalError(bad_arity_funexpr, [])),
	    [FunNode] = ?Query:exec(ExprFun, ?Expr:child(1)),
	    {implicit_fun, FunNode, new};
	 _ ->
	    {something_else, ExprFun, new}
    end.

%%% ============================================================================
%%% List comp to lists:map or lists:filter section

prepare_list(_Expr) ->
    fun() ->
	    ok
    end.

%%% ============================================================================
%%% Generate a valid variable name section

-define(Variables, ["X", "Y", "Z", "I", "J", "K", "L", "M", "N",
		    "A", "B", "C", "D", "E", "F", "G", "H", "O", 
		    "P", "Q", "R", "S", "T", "U", "V", "W"]).
-define(VarPrefix, "ListVar").

get_allowed_varname(Expr) ->
    VisVars = ?Query:exec(Expr, ?Expr:visible_vars()),
    Vars = ?Query:exec(Expr, ?Expr:variables()),
    VarNames = [ ?Var:name(X) || X <- Vars ++ VisVars ],
    Allowed = ?Variables -- VarNames,
    case Allowed of
	[First | _] -> First;
	[] -> get_next_listvar(VarNames)
    end.

get_next_listvar(List) ->
    {ok, RE} = re:compile("^" ++ ?VarPrefix),
    Found = lists:sort(search(List, RE, [])),
    Index = search_max_index(Found, RE),
    ?VarPrefix ++ integer_to_list(Index).

search([], _, Result) -> Result;
search([First | Tail], RE, Result) ->
    case re:run(First, RE) of
	nomatch ->
	    search(Tail, RE, Result);
	{match, _} ->
	    search(Tail, RE, [First | Result])
    end.

search_max_index(List, RE) ->
    Last = lists:last(List),
    {match, [{0, N}]} = re:run(Last, RE),
    {Int, _} = string:to_integer(lists:nthtail(N, Last)),
    Int + 1.
