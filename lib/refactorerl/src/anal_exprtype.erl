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

%%% @doc Analyse expression types. Set the `type' field of every expression to
%%% the correct value: `pattern' in patterns and `guard' in guard conditions
%%% (`expr' is the default value). There is no need to clear this information
%%% on removal.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(anal_exprtype).
-vsn("$Rev: 1247 $").
-behaviour(refac_anal).

-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @private
init() ->
    [].

%% @private
insert(_, #clause{}, pattern, Expr, E=#expr{}) ->
    ?GRAPH:update(Expr, E#expr{type=pattern});

insert(_, #clause{}, guard, Expr, E=#expr{}) ->
    ?GRAPH:update(Expr, E#expr{type=guard});

insert(M, #expr{kind=match_expr}, sub, Expr, E=#expr{}) ->
    case ?GRAPH:index(M, sub, Expr) of
        1 -> ?GRAPH:update(Expr, E#expr{type=pattern});
        _ -> ok
    end;

insert(_, #expr{type=T}, sub, Expr, E=#expr{type=T2}) when T =/= T2 ->
    ?GRAPH:update(Expr, E#expr{type=T});

insert(_,_,_,_,_) ->
    ok.

%% @private
remove(_,_,_,_,_) ->
    ok.
