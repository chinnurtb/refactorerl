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

%%% @doc Semantical analysis of node types.
%%%
%%% Set the `type' field of every expression to the correct value: `pattern'
%%% in patterns and `guard' in guard conditions (`expr' is the default
%%% value). There is no need to clear this information on removal.
%%%
%%% The `type' field of clauses will also be set here.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_anal_nodetype).
-vsn("$Rev: 2170 $").
-behaviour(referl_esg).

-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @private
init() ->
    [].

%% @private
insert(_, #form{}, _, Clause, C=#clause{}) ->
    ?Graph:update(Clause, C#clause{type=scope});

insert(_, #clause{}, pattern, Expr, E=#expr{}) ->
    ?Graph:update(Expr, E#expr{type=pattern});

insert(_, #clause{}, guard, Expr, E=#expr{}) ->
    ?Graph:update(Expr, E#expr{type=guard});

insert(M, #expr{kind=match_expr}, sub, Expr, E=#expr{}) ->
    case ?Graph:index(M, sub, Expr) of
        1 -> ?Graph:update(Expr, E#expr{type=pattern});
        _ -> ok
    end;

insert(_, #expr{type=T}, sub, Expr, E=#expr{type=T2}) when T =/= T2 ->
    ?Graph:update(Expr, E#expr{type=T});

insert(Parent, #expr{kind=Exp}, Tag, Clause, C=#clause{}) ->
    Type = clause_type(Exp, Tag, ?Graph:index(Parent, Tag, Clause)),
    ?Graph:update(Clause, C#clause{type=Type});

insert(_,_,_,_,_) ->
    ok.


clause_type(bin_comp,       _, _) -> hide;
clause_type(binary_gen,     _, 1) -> scope;
clause_type(binary_gen,     _, 2) -> hide;
clause_type(block_expr,     _, _) -> simple;
clause_type(case_expr, headcl, _) -> simple;
clause_type(case_expr, exprcl, _) -> branch;
clause_type(catch_expr,     _, _) -> hide;
clause_type(filter,         _, _) -> hide;
clause_type(fun_expr,       _, _) -> scope;
clause_type(if_expr,        _, _) -> branch;
clause_type(infix_expr,     _, 1) -> simple; % andalso, orelse
clause_type(infix_expr,     _, 2) -> hide;
clause_type(list_comp,      _, _) -> hide;
clause_type(list_gen,       _, 1) -> scope;
clause_type(list_gen,       _, 2) -> hide;
clause_type(receive_expr,   _, _) -> branch;
clause_type(try_expr,       _, _) -> hide.

%% @private
remove(_,_,_,_,_) ->
    ok.
