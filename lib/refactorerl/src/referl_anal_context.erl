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
-vsn("$Rev: 3185 $").
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
    [Mod] = ?Graph:path(FunForm, [{form, back}, moddef]),
    ?Graph:mklink(Clause, modctx, Mod),
    ?Graph:mklink(Clause, functx, Clause),
    ?Graph:mklink(Clause, scope, Clause);

insert(_, #form{type=attrib}, attr, Expr, #expr{}) ->
    ?Graph:mklink(Expr, sup, Expr);

insert(Clause, #clause{kind=hexpr}, _, Expr, #expr{}) ->
    ?Graph:mklink(Expr, sup, Expr),
    [Compr] = ?Graph:path(Clause, [{exprcl, back}, {exprcl, last}]),
    add_cmp_head(Compr, Expr);

insert(Clause, #clause{kind=Kind}, Tag, Expr, #expr{}) ->
    ?Graph:mklink(Expr, sup, Expr),
    if
        Kind == compr ->
            ok;
        Tag /= name ->
            add_visib(Clause, Expr);
        true -> ok
    end;

insert(Expr, #expr{kind=Kind}, _Tag, Clause, #clause{type=scope}) ->
    ?Graph:mklink(Clause, scope, Clause),
    add_clause(Expr, Kind, Clause),
    case ?Graph:path(Expr, [sup, {visib, back}, scope, functx]) of
        [Fun] -> ?Graph:mklink(Clause, functx, Fun);
        []    -> ok % in attributes, no function context
    end;

insert(Expr, #expr{kind=Kind}, _Tag, Clause, #clause{}) ->
    add_clause(Expr, Kind, Clause),
    case ?Graph:path(Expr, [sup, {visib,back}, scope]) of
        [Scope] ->
            ?Graph:mklink(Clause, scope, Scope);
        []      -> ok % in attributes, no active scope
    end;

insert(Expr, #expr{}, _, Sub, #expr{}) ->
    [Sup] = ?Graph:path(Expr, [sup]),
    ?Graph:mklink(Sub, sup, Sup);

insert(_,_,_,_,_) ->
    ok.

%% @private
remove(_, #form{type=attrib}, attr, Expr, #expr{}) ->
    ?Graph:rmlink(Expr, sup, Expr);

remove(_, #expr{}, _, Expr, #expr{}) ->
    [Sup] = ?Graph:path(Expr, [sup]),
    ?Graph:rmlink(Expr, sup, Sup);

remove(Clause, #clause{kind=Kind}, Tag, Expr, #expr{}) ->
    ?Graph:rmlink(Expr, sup, Expr),
    if
        Kind == hexpr ->
            case ?Graph:path(Expr, [{visib, back}]) of
                [C] -> ?Graph:rmlink(C, visib, Expr);
                [] -> ok
            end;
        Kind /= compr, Tag /= name ->
            ?Graph:rmlink(Clause, visib, Expr);
        true ->
            ok
    end;

remove(Expr, #expr{kind=ExKind}, _, Clause, #clause{kind=ClKind}) ->
    case ?Graph:path(Clause, [functx]) of
        [Fun] -> ?Graph:rmlink(Clause, functx, Fun);
        [] -> ok
    end,
    if
        ClKind == compr ->
            case ?Graph:path(Clause, [visib]) of
                [] -> ok;
                [Head] -> ?Graph:rmlink(Clause, visib, Head)
            end;
        ExKind == list_gen; ExKind == binary_gen; ExKind == filter ->
            [Compr] = ?Graph:path(Expr, [{body, back}]),
            rem_cmp_clause(Compr, Expr, Clause);
        true ->
            ok
    end,
    ?Graph:rmlink(Expr, clause, Clause),
    case ?Graph:path(Clause, [scope]) of
        [Scope] -> ?Graph:rmlink(Clause, scope, Scope);
        []      -> ok % no scope in attributes
    end;

remove(_, #form{}, _, Clause, #clause{}) ->
    ?Graph:rmlink(Clause, scope, Clause),
    ?Graph:rmlink(Clause, functx, Clause),
    [Mod] = ?Graph:path(Clause, [modctx]),
    ?Graph:rmlink(Clause, modctx, Mod);

remove(_,_,_,_,_) ->
    ok.


%%% ----------------------------------------------------------------------------
%%% Helper functions

add_visib(Clause, Expr) ->
    Visib = ?Graph:path(Clause, [visib]),
    Content = ?Graph:path(Clause, [pattern]) ++
        ?Graph:path(Clause, [tmout]) ++
        ?Graph:path(Clause, [guard]) ++
        ?Graph:path(Clause, [body]),
    add_link(Clause, Expr, Content, Visib, visib).

add_clause(Expr, Kind, Clause) ->
    Clauses = ?Graph:path(Expr, [clause]),
    Content = ?Graph:path(Expr, [headcl]) ++
        ?Graph:path(Expr, [exprcl]) ++
        ?Graph:path(Expr, [catchcl]) ++
        ?Graph:path(Expr, [aftercl]),
    add_link(Expr, Clause, Content, Clauses, clause),
    Ind = ?Graph:index(Expr, exprcl, Clause),
    if
        (Kind == list_gen) or (Kind == binary_gen) or (Kind == filter),
        Ind == 1 ->
            [Compr] = ?Graph:path(Expr, [{body, back}]),
            add_cmp_clause(Compr, Expr, Clause);
        (Kind == list_comp) or (Kind == binary_comp),
        Ind == 2 ->
            [Head] = ?Graph:path(Expr, [{exprcl, 1}, body]),
            add_cmp_head(Clause, Head);
        true ->
            ok
    end.

add_link(Parent, Child, Order, Children, Tag) ->
    add_link(Parent, Child, Order, Children, Tag, 1).

add_link(Parent, Child, [OH | _OT], _Children, Tag, Ind) when OH == Child ->
    ?Graph:mklink(Parent, {Tag, Ind}, Child);
add_link(Parent, Child, [OH | OT],  [CH | CT], Tag, Ind) when OH == CH ->
    add_link(Parent, Child, OT, CT, Tag, Ind+1);
add_link(Parent, Child, [_OH | OT], Children,  Tag, Ind) ->
    add_link(Parent, Child, OT, Children, Tag, Ind).


%% Walk through a comprehension structure
%% `Fun' is called once for every nesting level of the structure
fold_compr(Compr, Fun, Acc) ->
    [Head] = ?Graph:path(Compr, [{exprcl, back}, {exprcl,1}, body]),
    fold_compr(Compr, Head, Compr, Fun, Acc).

fold_compr(Compr, Head, Clause, Fun, Acc) ->
    {Next, Ind} =
        case ?Graph:path(Clause, [{visib, last}]) of
            [] -> {none, last}; 
            [Head] -> {Head, last};
            [Expr] ->
                case ?Graph:index(Compr, body, Expr) of
                    none -> {none, last}; 
                    I -> {Expr, I}
                end
        end,
    case Fun(Clause, Next, Ind, Acc) of
        {stop, Acc1} -> Acc1; 
        {next, Acc1} when Ind == last -> Acc1;
        {next, Acc1} ->
            [NCl] = ?Graph:path(Next, [{exprcl, 1}]),
            fold_compr(Compr, Head, NCl, Fun, Acc1);
        _ -> ok
    end.


%% Insert the head of a comprehension into the comprehension structure
add_cmp_head(Compr, Head) ->
    fold_compr(Compr, fun add_cmp_head/4, Head).

add_cmp_head(Clause, none, last, Head) ->
    ?Graph:mklink(Clause, visib, Head),
    stop;
add_cmp_head(_Clause, Head, last, Head) ->
    stop;
add_cmp_head(Clause, Expr, last, Head) ->
    error_logger:warning_msg("Overriding comprehension head: ~p->~p~n",
                             [Expr, Head]),
    ?Graph:rmlink(Clause, visib, Expr),
    ?Graph:mklink(Clause, visib, Head),
    stop;
add_cmp_head(_, _, _, Head) ->
    {next, Head}.


%% Insert a comprehension element into a comprehension structure
add_cmp_clause(Compr, Expr, Clause) ->
    fold_compr(Compr, fun add_cmp_clause/4,
               {Expr, ?Graph:index(Compr, body, Expr), Clause}).

add_cmp_clause(PrevCl, NextEx, NextInd, {Expr, Ind, Cl}=Acc) ->
    if
        NextEx == Expr ->
            stop;
        NextInd /= last, NextInd < Ind ->
            {next, Acc};
        NextEx == none ->
            ?Graph:mklink(PrevCl, visib, Expr),
            stop;
        true ->
            ?Graph:rmlink(PrevCl, visib, NextEx),
            case ?Graph:path(PrevCl, [scope]) of
                [PrevScope] ->
                    case ?Graph:path(Cl, [scope]) of
                        []   -> ?Graph:mklink(Cl, scope, PrevScope);
                        [Cl] -> ok
                    end;
                [] ->
                    PrevScope = none
            end,
            ?Graph:mklink(PrevCl, visib, Expr),
            ?Graph:mklink(Cl, visib, NextEx),
            adjust_scope(NextEx, PrevScope),
            stop
    end.

%% Remove a comprehension element from a comprehension structure
rem_cmp_clause(Compr, Expr, Clause) ->
    fold_compr(Compr, fun rem_cmp_clause/4, {Expr, Clause}).

rem_cmp_clause(PrevCl, NextEx, _, {NextEx, Clause}) ->
    ?Graph:rmlink(PrevCl, visib, NextEx),
    {next, {PrevCl, Clause}};
rem_cmp_clause(_, none, _, _) ->
    stop;
rem_cmp_clause(Clause, NextEx, _, {PrevCl, Clause}) ->
    ?Graph:rmlink(Clause, visib, NextEx),
    [OldScope] = ?Graph:path(Clause, [scope]),
    ?Graph:mklink(PrevCl, visib, NextEx),
    adjust_scope(NextEx, OldScope),
    stop;
rem_cmp_clause(_, _, _, Acc) ->
    {next, Acc}.

%% Update clauses below `Expr' to belong to their proper scope
adjust_scope(Expr, Old) ->
    case ?Graph:path(Expr, [{visib, back}, scope]) of
        []      -> ok;
        [Old]   -> ok;
        [Scope] -> adjust_scope(?Graph:path(Expr, [clause]), Old, Scope)
    end.

adjust_scope(Clauses, Old, New) ->
    lists:foreach(
      fun(Cl) ->
              Cont =
                  case ?Graph:path(Cl, [scope]) of
                      [Old] ->
                          ?Graph:rmlink(Cl, scope, Old),
                          ?Graph:mklink(Cl, scope, New),
                          true;
                      [] ->
                          ?Graph:mklink(Cl, scope, New),
                          true;
                      _ ->
                          false
                  end,
              if
                  Cont ->
                      [adjust_scope(?Graph:path(E, [clause]), Old, New) ||
                          E <- ?Graph:path(Cl, [visib])];
                  not Cont -> ok
              end
      end,
      Clauses).
