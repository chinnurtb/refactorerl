%%% -*- coding: latin-1 -*-

%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://plc.inf.elte.hu/erlang/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%% License for the specific language governing rights and limitations under
%%% the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the Original Code is Eötvös Loránd University.
%%% Portions created by Eötvös Loránd University are Copyright 2008, Eötvös
%%% Loránd University. All Rights Reserved.

%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(refac_extract_funexpr).
-vsn("$Rev: 1335 $").

-export([extract_funexpr/1,
	 extract_funexpr/2]).

-include("refactorerl.hrl").

-define(VARPREFIX, "V").


%% ----------------------------------------------------------------------------
%% @spec extract_funexpr(node(), integer()) -> ok
%% @doc Extracts the implicit fun expression in the specified file on the
%%      specified position, when it exists.
%% @end
%% ----------------------------------------------------------------------------
extract_funexpr(FileNode, Pos) ->
    {Token,_,_,_} = ?QUERY:token_by_pos(FileNode, Pos),
    io:format("~p~n", [Token]),
    case search_fun_expr(Token) of
	not_found ->
	    refac_ui:message(status, "No fun expression found.");
	FunExpr ->
	    try
		extract_funexpr(FunExpr),
		refac_fileman:save_file(FileNode),
		refac_ui:message(reload, "~s", [(?GRAPH:data(FileNode))#file.path]),
		refac_ui:message(status, "Fun expression is now in explicit form.")
	    catch
		throw:Error ->
		    refac_ui:message(status, "~s", [Error])
	    end
    end.

%% @private
search_fun_expr(Entity) ->
    Path = lists:reverse(?QUERY:path_to_file(Entity)),
    NotAFunExpr = 
	fun(E) ->
		case ?GRAPH:data(E) of
		    #expr{kind = fun_expr} ->
			false;
		    _ ->
			true
		end
	end,
    case lists:dropwhile(NotAFunExpr, Path) of
	[FunExpr|_] ->
	    FunExpr;
	_ ->
	    not_found
    end.
    

%% ----------------------------------------------------------------------------
%% @spec extract_funexpr(node()) -> ok
%% @doc Extracts the implicit fun expression.
%% ```
%% Example:
%% fun abc/3 -==> fun(V1, V2, V3) -> abc(V1, V2, V3) end
%% '''
%% @end
%% ----------------------------------------------------------------------------
extract_funexpr(FunExpr) ->
    IsImplicit = is_implicit_fun_expr(FunExpr),
    case IsImplicit of
	false ->
	    throw("Expression must be an implicit fun expression!");
	true ->
	    continue
    end,
    First = ?QUERY:first_token(FunExpr, efirst),
    Last = ?QUERY:first_token(FunExpr, elast),
    [Next] = ?GRAPH:path(Last, [next]),

    {Name, Arity} = remove_short_funexp(FunExpr),

    ClPatternNodes = get_var_nodes(?VARPREFIX, Arity, []),
    AppParameters = create_parameters(get_var_names(?VARPREFIX, Arity, [])),
    AppNameNode = create_name(Name),

    AppExpr = create_application(AppParameters, AppNameNode),
    FunClause = 
	create_funclause(FunExpr, ClPatternNodes, [AppExpr]),
    [CF, CL] = 
	refac_token:token_update_funexpr(FunClause, 
					 ClPatternNodes, 
					 FunExpr, 
					 First, 
					 Next),
    refac_token:token_update_application(AppNameNode, 
					 AppParameters, 
					 AppExpr, 
					 CF, 
					 CL),
    ?ESG:close(), %% refac_movefun:insert_module_qualifier needs this!
    ok.


%% @private
is_implicit_fun_expr(Expr) ->
    ExprKindOk = 
	case ?GRAPH:data(Expr) of
	    #expr{kind=fun_expr} ->
		true;
	    _ ->
		false
	end,
    SubExprKindOk =
	case ?GRAPH:path(Expr, [sub]) of
	    [] ->
		false;
	    [SubExpr|_] ->
		case ?GRAPH:data(SubExpr) of
		    #expr{kind=arity_qualifier} ->
			true;
		    _ ->
			false
		end
	end,
    ExprKindOk andalso SubExprKindOk.


%% ----------------------------------------------------------------------------
%% @private
%% @doc Example: The implicit fun expresson is 'fun abc/3'.
%%      The subgraph of the fun expression will be deleted
%%      and the return value is {abc, 3}.
%% @end
%% ----------------------------------------------------------------------------
remove_short_funexp(FunExpr) ->
    [Expr] = ?GRAPH:path(FunExpr, [sub]),
    [NameExpr, ArityExpr] = ?GRAPH:path(Expr, [sub]),
    [Referred] = ?GRAPH:path(FunExpr, [funref]),
    #expr{value=Name} = ?GRAPH:data(NameExpr),
    #expr{value=Arity} = ?GRAPH:data(ArityExpr),
    Lex = ?GRAPH:path(NameExpr, [efirst]) 
	++ ?GRAPH:path(ArityExpr, [efirst])
	++ ?GRAPH:path(Expr, [elex]),
    [?MANIP:kill_token(T) || T <- Lex],
    ?ESG:remove(Expr, sub, NameExpr),
    ?ESG:remove(Expr, sub, ArityExpr),
    ?ESG:remove(FunExpr, sub, Expr),
    ?ESG:remove(FunExpr, funref, Referred),
    {Name, Arity}.


%% ----------------------------------------------------------------------------
%% @private
%% @doc Returns pattern variable graph nodes.
%%      Example:
%%      get_var_nodes("Var", 3, []) returns a list of 3 expression nodes with
%%      values `Var1', `Var2', `Var3'.
%% @end
%% ----------------------------------------------------------------------------
get_var_nodes(Prefix, Count, List) ->
    [?ESG:create(#expr{type=pattern,kind=variable,value=Name}) || 
	Name <- get_var_names(Prefix, Count, List)].


%% ----------------------------------------------------------------------------
%% @private
%% @doc Returns list of strings.
%%      Example:
%%      get_var_names("Var, 3, []) returns ["Var1", "Var2", "Var3"].
%% @end
%% ----------------------------------------------------------------------------
get_var_names(_, 0, List) ->
    List;
get_var_names(Prefix, Count, List) ->
    get_var_names(
      Prefix,
      Count-1,
      [Prefix++integer_to_list(Count) | List]).


%%%%% Clause and application creation


%% @private
create_funclause(Parent, FunPatternNodes, ExprList) ->
    FunClause = ?ESG:create(#clause{type = funcl}),
    ?ESG:insert(Parent,exprcl,FunClause),
    lists:foreach(fun(Pattern)->
			  ?ESG:insert(FunClause, pattern, Pattern)
		  end, FunPatternNodes),
    lists:foreach(fun(Expr)->
			  ?ESG:insert(FunClause, body, Expr)
		  end,ExprList),
    FunClause.


%% @private
create_application(PatternNodes, NameNode) ->
    AppNode = ?ESG:create(#expr{kind=application}),
    ?ESG:insert(AppNode, sub, NameNode),
    lists:foreach(fun(Pattern) ->
			  ?ESG:insert(AppNode, sub, Pattern)
                  end, PatternNodes),
    AppNode.


%% @private
create_name(Name) ->
    ?ESG:create(#expr{kind = atom, value = Name}).


%% @private
create_parameters(Names) ->
    [?ESG:create(#expr{kind = variable, value = Name}) || Name <- Names].
