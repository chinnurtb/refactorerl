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

%%% @doc Analyse expression contexts. Clauses that define scopes get their
%%% `scope' attributes set to `true'. Furthermore, the following context
%%% structure is generated:
%%%
%%% <ul>
%%%
%%% <li>expressions are linked to their outermost direct superexpression
%%% (`sup')</li>
%%%
%%% <li>expressions are linked to their contained clauses (`clause'), link
%%% order reflects clause order</li>
%%%
%%% <li>outermost expressions are linked from their containing clause
%%% (`visib'), link order reflects visibility order</li>
%%%
%%% <li>clauses are linked to every containing scope clause (`scope'), link
%%% order goes from inner scopes to outer scopes</li>
%%%
%%% <li>scope clauses are linked to the containing module (`modctx')</li>
%%%
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(anal_context).
-vsn("$Rev: 1247 $").
-behaviour(refac_anal).

%% Interface exports
-export([expr_module/0, expr_visib/0, expr_super/0,
         clause_visib/0, clause_scopes/0]).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").


%% @spec expr_module() -> path()
%% @doc Path from an #expr{} to the containing #module{}.
expr_module() ->
    expr_visib() ++ [{scope,1}, modctx].

%% @spec expr_visib() -> path()
%% @doc Path from an #expr{} to the innermost visibility #clause{}.
expr_visib() ->
    expr_super() ++ [{visib, back}].

%% @spec expr_super() -> path()
%% @doc Path from an #expr{} to its outermost direct superexpression.
expr_super() ->
    [sup].

%% @spec clause_visib() -> path()
%% @doc Path from a #clause{} to the top #expr{}'s of its visibility area
clause_visib() ->
    [visib].

%% @spec clause_scopes() -> path()
%% @doc Path from a #clause{} to its every containing scope #clause{}
clause_scopes() ->
    [scope].

%% @private
init() ->
    [{clause, [{modctx, module}, {scope, clause}, {visib, expr}]},
     {expr,   [{sup, expr}, {clause, clause}]}
    ].

%% @private
insert(FunForm, #form{type=func}, funcl, Clause, ClData = #clause{}) ->
    [Mod] = ?GRAPH:path(FunForm,
                        [{form, back}] ++ anal_module:file_module()),
    ?GRAPH:update(Clause, ClData#clause{scope=true}),
    ?GRAPH:mklink(Clause, modctx, Mod),
    ?GRAPH:mklink(Clause, scope, Clause);

insert(_, #form{type=attrib}, attr, Expr, #expr{}) ->
    ?GRAPH:mklink(Expr, sup, Expr);

insert(Clause, #clause{}, Tag, Expr, #expr{}) ->
    ?GRAPH:mklink(Expr, sup, Expr),
    if
        Tag =:= body; Tag =:= guard; Tag =:= pattern; Tag =:= tmout ->
            add_visib(Clause, Expr);
        true -> ok
    end;

insert(Expr, #expr{}, _Tag, Clause, ClData=#clause{type=funcl}) ->
    [Mod] = ?GRAPH:path(Expr, expr_module()),
    Scopes = ?GRAPH:path(Expr, expr_visib() ++ [scope]),
    ?GRAPH:update(Clause, ClData#clause{scope=true}),
    ?GRAPH:mklink(Clause, modctx, Mod),
    ?GRAPH:mklink(Clause, scope, Clause),
    [ ?GRAPH:mklink(Clause, scope, Scope) || Scope <- Scopes],
    add_clause(Expr, Clause);

insert(Expr, #expr{}, _Tag, Clause, #clause{type=ClType})
  when ClType =:= branch; ClType =:= block ->
    [ ?GRAPH:mklink(Clause, scope, Scope) ||
        Scope <- ?GRAPH:path(Expr, expr_visib() ++ clause_scopes())],
    add_clause(Expr, Clause);

insert(Expr, #expr{}, _, Sub, #expr{}) ->
    [Sup] = ?GRAPH:path(Expr, [sup]),
    ?GRAPH:mklink(Sub, sup, Sup);

insert(_,_,_,_,_) ->
    ok.

%% @private
remove(_, #form{type=attrib}, attr, Expr, #expr{}) ->
    ?GRAPH:rmlink(Expr, sup, Expr);

remove(_, #expr{}, _, Expr, #expr{}) ->
    [Sup] = ?GRAPH:path(Expr, [sup]),
    ?GRAPH:rmlink(Expr, sup, Sup);

remove(Clause, #clause{}, Tag, Expr, #expr{})
  when Tag =:= body; Tag =:= guard; Tag =:= pattern; Tag =:= tmout ->
    ?GRAPH:rmlink(Expr, sup, Expr),
    ?GRAPH:rmlink(Clause, visib, Expr);

remove(Expr, #expr{}, _, Clause, #clause{}) ->
    ?GRAPH:rmlink(Expr, clause, Clause),
    [ ?GRAPH:rmlink(Clause, scope, Scope) ||
        Scope <- ?GRAPH:path(Clause, clause_scopes()) ],
    case ?GRAPH:path(Clause, [modctx]) of
        [Mod] -> ?GRAPH:rmlink(Clause, modctx, Mod);
        [] -> ok
    end;

remove(_, _, _, Scope, #clause{scope=true}) ->
    [ ?GRAPH:rmlink(Scope, scope, InScope) ||
        InScope <- ?GRAPH:path(Scope, [scope]) ],
    [ ?GRAPH:rmlink(Scope, modctx, Mod) ||
        Mod <- ?GRAPH:path(Scope, [modctx]) ];

remove(_,_,_,_,_) ->
    ok.


add_visib(Clause, Expr) ->
    Visib = ?GRAPH:path(Clause, [visib]),
    Content = ?GRAPH:path(Clause, [pattern]) ++
        ?GRAPH:path(Clause, [tmout]) ++
        ?GRAPH:path(Clause, [guard]) ++
        ?GRAPH:path(Clause, [body]),
    add_link(Clause, Expr, Content, Visib, visib).

add_clause(Expr, Clause) ->
    Clauses = ?GRAPH:path(Expr, [clause]),
    Content = ?GRAPH:path(Expr, [exprcl]) ++
        ?GRAPH:path(Expr, [handlercl]) ++
        ?GRAPH:path(Expr, [aftercl]),
    add_link(Expr, Clause, Content, Clauses, clause).

add_link(Parent, Child, Order, Children, Tag) ->
    add_link(Parent, Child, Order, Children, Tag, 1).

add_link(Parent, Child, [OH | _OT], _Children, Tag, Ind) when OH == Child ->
    ?GRAPH:mklink(Parent, {Tag, Ind}, Child);
add_link(Parent, Child, [OH | OT],  [CH | CT], Tag, Ind) when OH == CH ->
    add_link(Parent, Child, OT, CT, Tag, Ind+1);
add_link(Parent, Child, [_OH | OT], Children,  Tag, Ind) ->
    add_link(Parent, Child, OT, Children, Tag, Ind).
