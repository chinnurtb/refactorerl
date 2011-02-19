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

%%% @doc Analyse expression contexts. The following context structure is
%%% generated:
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
%%% <li>clauses are linked to their innermost containing scope clause
%%% (`scope')</li>
%%%
%%% <li>scope clauses are linked to the containing function clause
%%% (`functx')</li>
%%%
%%% <li>function clauses are linked to the containing module (`modctx')</li>
%%%
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_anal_context).
-vsn("$Rev: 1979 $").
-behaviour(referl_esg).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @private
init() ->
    [{clause, [{modctx, module}, {functx, clause}, {scope, clause},
               {visib, expr}]},
     {expr,   [{sup, expr}, {clause, clause}]}
    ].

%% @private
insert(FunForm, #form{type=func}, funcl, Clause, #clause{}) ->
    [Mod] = ?GRAPH:path(FunForm, [{form, back}, moddef]),
    ?GRAPH:mklink(Clause, modctx, Mod),
    ?GRAPH:mklink(Clause, functx, Clause),
    ?GRAPH:mklink(Clause, scope, Clause);

insert(_, #form{type=attrib}, attr, Expr, #expr{}) ->
    ?GRAPH:mklink(Expr, sup, Expr);

insert(Clause, #clause{kind=hexpr}, _, Expr, #expr{}) ->
    ?GRAPH:mklink(Expr, sup, Expr),
    [Compr] = ?GRAPH:path(Clause, [{exprcl, back}, {exprcl, last}]),
    add_cmp_head(Compr, Expr);

insert(Clause, #clause{kind=Kind}, Tag, Expr, #expr{}) ->
    ?GRAPH:mklink(Expr, sup, Expr),
    if
        Kind == compr ->
            add_cmp_clause(Clause, Expr);
        Tag /= name ->
            add_visib(Clause, Expr);
        true -> ok
    end;

insert(Expr, #expr{kind=Kind}, _Tag, Clause, #clause{type=scope}) ->
    [Fun] = ?GRAPH:path(Expr, [sup, {visib, back}, scope, functx]),
    ?GRAPH:mklink(Clause, functx, Fun),
    ?GRAPH:mklink(Clause, scope, Clause),
    add_clause(Expr, Kind, Clause);

insert(Expr, #expr{kind=Kind}, _Tag, Clause, #clause{}) ->
    [Scope] = ?GRAPH:path(Expr, [sup, {visib,back}, scope]),
    ?GRAPH:mklink(Clause, scope, Scope),
    add_clause(Expr, Kind, Clause);

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

remove(Clause, #clause{kind=Kind}, Tag, Expr, #expr{}) ->
    ?GRAPH:rmlink(Expr, sup, Expr),
    if
        Kind == hexpr ->
            case ?GRAPH:path(Expr, [{visib, back}]) of
                [C] -> ?GRAPH:rmlink(C, visib, Expr);
                [] -> ok
            end;
        Kind /= compr, Tag /= name ->
            ?GRAPH:rmlink(Clause, visib, Expr);
        true ->
            ok
    end;

remove(Expr, #expr{kind=ExKind}, _, Clause, #clause{kind=ClKind}) ->
    ?GRAPH:rmlink(Expr, clause, Clause),
    [Scope] = ?GRAPH:path(Clause, [scope]),
    ?GRAPH:rmlink(Clause, scope, Scope),
    case ?GRAPH:path(Clause, [functx]) of
        [Fun] -> ?GRAPH:rmlink(Clause, functx, Fun);
        [] -> ok
    end,
    if
        ClKind == compr ->
            case ?GRAPH:path(Clause, [visib]) of
                [] -> ok;
                [Head] -> ?GRAPH:rmlink(Clause, visib, Head)
            end;
        ExKind == list_gen; ExKind == binary_gen; ExKind == filter ->
            [Compr] = ?GRAPH:path(Expr, [{body, back}]),
            rem_cmp_clause(Compr, Expr, Clause);
        true ->
            ok
    end;

remove(_, #form{}, _, Clause, #clause{}) ->
    ?GRAPH:rmlink(Clause, scope, Clause),
    ?GRAPH:rmlink(Clause, functx, Clause),
    [Mod] = ?GRAPH:path(Clause, [modctx]),
    ?GRAPH:rmlink(Clause, modctx, Mod);

remove(_,_,_,_,_) ->
    ok.


%%% ----------------------------------------------------------------------------
%%% Helper functions

add_visib(Clause, Expr) ->
    Visib = ?GRAPH:path(Clause, [visib]),
    Content = ?GRAPH:path(Clause, [pattern]) ++
        ?GRAPH:path(Clause, [tmout]) ++
        ?GRAPH:path(Clause, [guard]) ++
        ?GRAPH:path(Clause, [body]),
    add_link(Clause, Expr, Content, Visib, visib).

add_clause(Expr, Kind, Clause) ->
    Clauses = ?GRAPH:path(Expr, [clause]),
    Content = ?GRAPH:path(Expr, [headcl]) ++
        ?GRAPH:path(Expr, [exprcl]) ++
        ?GRAPH:path(Expr, [catchcl]) ++
        ?GRAPH:path(Expr, [aftercl]),
    add_link(Expr, Clause, Content, Clauses, clause),
    Ind = ?GRAPH:index(Expr, exprcl, Clause),
    if
        (Kind == list_gen) or (Kind == binary_gen) or (Kind == filter),
        Ind == 1 ->
            [Compr] = ?GRAPH:path(Expr, [{body, back}]),
            add_cmp_clause(Compr, Expr, Clause);
        (Kind == list_comp) or (Kind == binary_comp),
        Ind == 2 ->
            [Head] = ?GRAPH:path(Expr, [{exprcl, 1}, body]),
            add_cmp_head(Clause, Head);
        true ->
            ok
    end.

add_link(Parent, Child, Order, Children, Tag) ->
    add_link(Parent, Child, Order, Children, Tag, 1).

add_link(Parent, Child, [OH | _OT], _Children, Tag, Ind) when OH == Child ->
    ?GRAPH:mklink(Parent, {Tag, Ind}, Child);
add_link(Parent, Child, [OH | OT],  [CH | CT], Tag, Ind) when OH == CH ->
    add_link(Parent, Child, OT, CT, Tag, Ind+1);
add_link(Parent, Child, [_OH | OT], Children,  Tag, Ind) ->
    add_link(Parent, Child, OT, Children, Tag, Ind).


fold_compr(Compr, Fun, Acc) ->
    [Head] = ?GRAPH:path(Compr, [{exprcl, back}, {exprcl,1}, body]),
    fold_compr(Compr, Head, Compr, Fun, Acc).

fold_compr(Compr, Head, Clause, Fun, Acc) ->
    {Next, Ind} =
        case ?GRAPH:path(Clause, [{visib, last}]) of
            [] -> {none, last}; 
            [Head] -> {Head, last};
            [Expr] ->
                case ?GRAPH:index(Compr, body, Expr) of
                    none -> {none, last}; 
                    I -> {Expr, I}
                end
        end,
    case Fun(Clause, Next, Ind, Acc) of
        {stop, Acc1} -> Acc1; 
        {next, Acc1} when Ind == last -> Acc1;
        {next, Acc1} ->
            [NCl] = ?GRAPH:path(Next, [{exprcl, 1}]),
            fold_compr(Compr, Head, NCl, Fun, Acc1);
        _ -> ok
    end.

add_cmp_head(Compr, Head) ->
    fold_compr(Compr, fun add_cmp_head/4, Head).

add_cmp_head(Clause, none, last, Head) ->
    ?GRAPH:mklink(Clause, visib, Head),
    stop;
add_cmp_head(_Clause, Head, last, Head) ->
    stop;
add_cmp_head(Clause, Expr, last, Head) ->
    error_logger:warning_msg("Overriding comprehension head: ~p->~p~n",
                             [Expr, Head]),
    ?GRAPH:rmlink(Clause, visib, Expr),
    ?GRAPH:mklink(Clause, visib, Head),
    stop;
add_cmp_head(_, _, _, Head) ->
    {next, Head}.


add_cmp_clause(Compr, Expr) ->
    [Clause] = ?GRAPH:path(Expr, [{exprcl,1}]),
    add_cmp_clause(Compr, Expr, Clause).

add_cmp_clause(Compr, Expr, Clause) ->
    fold_compr(Compr, fun add_cmp_clause/4,
               {Expr, ?GRAPH:index(Compr, body, Expr), Clause}).

add_cmp_clause(PrevCl, NextEx, NextInd, {Expr, Ind, Cl}=Acc) ->
    if
        NextEx == Expr ->
            stop;
        NextInd /= last, NextInd < Ind ->
            {next, Acc};
        NextEx == none ->
            ?GRAPH:mklink(PrevCl, visib, Expr),
            stop;
        true ->
            ?GRAPH:rmlink(PrevCl, visib, NextEx),
            ?GRAPH:mklink(PrevCl, visib, Expr),
            ?GRAPH:mklink(Cl, visib, NextEx),
            stop
    end.

rem_cmp_clause(Compr, Expr, Clause) ->
    fold_compr(Compr, fun rem_cmp_clause/4, {Expr, Clause}).

rem_cmp_clause(PrevCl, NextEx, _, {NextEx, Clause}) ->
    ?GRAPH:rmlink(PrevCl, visib, NextEx),
    {next, {PrevCl, Clause}};
rem_cmp_clause(_, none, _, _) ->
    stop;
rem_cmp_clause(Clause, NextEx, _, {PrevCl, Clause}) ->
    ?GRAPH:rmlink(Clause, visib, NextEx),
    ?GRAPH:mklink(PrevCl, visib, NextEx),
    stop;
rem_cmp_clause(_, _, _, Acc) ->
    {next, Acc}.
