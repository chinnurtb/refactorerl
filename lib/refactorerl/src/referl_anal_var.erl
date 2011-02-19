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

%%% @doc Analyse variable binding struture. The following semantical structure
%%% is built:
%%%
%%% <ul>
%%%
%%% <li>A semantical variable object (`#variable{}') is created for every
%%%   separate variable that appears in the code.</li>
%%%
%%% <li>Every clause of type `scope' has a `vardef' link to every variable
%%%   that is directly defined by the scope.</li>
%%%
%%% <li>Every variable expression that binds a variable has a `varbind' link
%%%   to the variable object.</li>
%%%
%%% <li>Every variable expression that reads a variable has a `varref' link
%%%   to the variable object.</li>
%%%
%%% <li>Every clause where a variable is visible has a `varvis' link to the
%%%   variable object.</li>
%%%
%%% <li>Every expression that introduces a variable into its visibility
%%%   context has a `varintro' linked from the variable object.</li>
%%%
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_anal_var).
-vsn("$Rev: 2170 $").
-behaviour(referl_esg).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%%-define(DO_TRACE, yes).
-ifdef(DO_TRACE).
  -define(TRACE(Str), io:format("~3b|~s~n", [?LINE, Str])).
-else.
  -define(TRACE(Str), ok).
-endif.

-define(VAR(V), [" [", ??V, "=", io_lib:format("~p",[V]), "]"]).

%% @private
init() ->
    [{variable, record_info(fields, variable), [{varintro, expr}]},
     {clause, [{vardef, variable}, {varvis, variable}]},
     {expr,   [{varref, variable}, {varbind, variable}]}
    ].


%% @private
insert(_,_,_,_, #expr{kind=variable, value="_"}) ->
    ok;

insert(_,_,_,Expr, #expr{kind=variable, type=pattern, value=Name}) ->
    ?TRACE("----------------------ins------------------------"),
    [Scope] = ?Graph:path(Expr, [sup, {visib, back}, scope]),
    scope(Scope, Name, visible_var(Scope, Name), add);

insert(_,_,_,Expr, #expr{kind=variable, value=Name}) ->
    ?TRACE("----------------------ins------------------------"),
    case ?Graph:path(Expr, [varref]) of
        [_Var] -> ok;
        [] ->
            case ?Graph:path(Expr, [sup,
                                    {visib, back},
                                    {varvis, {name,'==',Name}}]) of
                [] ->
                    ?TRACE(["not present",?VAR(Expr),?VAR(Name)]),
                    ok;
                [Var] ->
                    case visible(Expr, Var) of
                        true  ->
                            ?TRACE(["visible",?VAR(Expr),?VAR(Name)]),
                            ?Graph:mklink(Expr, varref, Var);
                        false ->
                            ?TRACE(["not visible",?VAR(Expr),?VAR(Name)]),
                            ok
                    end
            end
    end;

insert(Expr, #expr{}, _, Clause, #clause{}) ->
    %% TODO: branch insertion may hide variables
    Vars = ?Graph:path(Expr, [sup, {visib, back}, varvis]),
    [?Graph:mklink(Clause, varvis, Var) ||
        Var <- Vars,
        [] == ?Graph:path(
                 Clause,
                 [{varvis, {name,'==', (?Graph:data(Var))#variable.name}}]),
        visible(Expr, Var)];

insert(_,_,_,_,_) ->
    ok.

%% @private
remove(_,_,_,Expr, #expr{kind=variable, type=Type, value=Name}) ->
    case {?Graph:path(Expr, [varbind]), ?Graph:path(Expr, [varref])} of
        {[B],[]} -> unlink_var(Expr, varbind, B);
        {[],[R]} -> unlink_var(Expr, varref, R);
        {[],[]} -> ok
    end,
    if
        Type == pattern ->
            ?TRACE("-----------------------rem-----------------------"),
            [Scope] = ?Graph:path(Expr, [sup, {visib, back}, scope]),
            scope(Scope, Name, visible_var(Scope, Name), del);
        true ->
            ok
    end;

remove(_, _, _, Clause, #clause{}) ->
    %% TODO: branch removal may bind variables
    [?Graph:rmlink(Clause, varvis, Var) || Var <- ?Graph:path(Clause,[varvis])],
    [?Graph:rmlink(Var, varintro, Top) ||
        Top <- ?Graph:path(Clause, [visib]),
        Var <- ?Graph:path(Top,[{varintro, back}])];

remove(_,_,_,_,_) ->
    ok.

%%% ----------------------------------------------------------------------------
%%% Variable object handling

variable(Expr, Name) ->
    [Scope] = ?Graph:path(Expr, [sup, {visib, back}, scope]),
    case ?Graph:path(Scope, [{vardef, {name,'==',Name}}]) of
        [Var] -> Var; 
        [] ->
            Var = ?Graph:create(#variable{name=Name}),
            ?Graph:mklink(Scope, vardef, Var),
            Var
    end.


unlink_var(Expr, Link, Var) ->
    ?Graph:rmlink(Expr, Link, Var),
    unuse(Var).

unuse(Var) ->
    case ?Graph:path(Var,[{varref,back}])++?Graph:path(Var,[{varbind,back}]) of
        [] -> ?Graph:delete(Var);
        _  -> ok
    end.


%%% ----------------------------------------------------------------------------
%%% Visibility-related queries

visible_var(Clause, Name) ->
    case ?Graph:path(Clause, [{clause, back}, sup]) of
        [] -> undef; % top-level function clause
        [Top] ->
            case ?Graph:path(Top, [{visib, back}, {varvis,{name,'==',Name}}]) of
                [] -> undef; % variable not present at all
                [Var] ->
                    case visible(Top, Var) of
                        true  -> Var;
                        false -> undef
                    end
            end
    end.

visible(Expr, Var) ->
    [Top] = ?Graph:path(Expr, [sup]),
    [Clause] = ?Graph:path(Top, [{visib, back}]),
    case ?Graph:path(Clause, [visib, {intersect, Var, varintro}]) of
        [Intro] ->
            ?Graph:index(Clause, visib, Intro) <
                ?Graph:index(Clause, visib, Top);
        [] -> true
    end.


%%% ----------------------------------------------------------------------------
%%% Binding structure handling


%% Hide variable when it is bound by a pattern of the scope clause
scope(Scope, Name, Var, Mode) ->
    Shd = ?Graph:path(Scope, shadow_path(Mode, Name)),
    ?TRACE(["scope", ?VAR(Scope), ?VAR(Name), ?VAR(Mode), ?VAR(Shd)]),
    case Shd of
        [] -> clause(Scope, Name, Var, Mode);
        _  -> clause(Scope, Name, undef, Mode)
    end,
    Var.

shadow_path(add, Name) ->
    [pattern,
     {{sup, back}, {{kind, '==', variable}, 'and', {value, '==', Name}}}];
shadow_path(del, Name) ->
    [pattern,
     {sup, back},
     {varbind, {name, '==', Name}}].

%% Add varvis link for inherited variable, remove varvis link for
%% non-inherited variable.
clause(Clause, Name, Var, Mode) ->
    ?TRACE(["clause", ?VAR(Clause), ?VAR(Var)]),
    case ?Graph:path(Clause, [{varvis, {name, '==', Name}}]) of
        [] when is_tuple(Var), Mode /= del ->
            %% `Mode == del' is not possible in theory (no new inheritance is
            %% possible when a variable binding is removed), the condition is
            %% just for safety.
            ?Graph:mklink(Clause, varvis, Var);
        []    -> ok;
        [Var] -> ok;
        [OV]  ->
            ?TRACE(["  clear", ?VAR(OV)]),
            ?Graph:rmlink(Clause, varvis, OV),
            if
                is_tuple(Var) ->
                    ?Graph:mklink(Clause, varvis, Var);
                true -> ok
            end
    end,
    lists:foldl(
      fun (Expr, V) -> topexpr(Clause, Expr, Name, V, Mode) end,
      Var,
      ?Graph:path(Clause, [visib])).

%% Process subexpressions; add varintro+varvis for variable introduced here
topexpr(Clause, Top, Name, Var, Mode) ->
    ?TRACE(["topexpr", ?VAR(Top), ?VAR(Var)]),
    Vars = [expr(Expr, Name, Var, Mode) ||
               Expr <- ?Graph:path(Top, [{sup, back}])],
    [NewVar | _] = lists:sort(fun (A,B) -> ord(A) < ord(B) end, Vars),
    ?TRACE(["after top", ?VAR(NewVar)]),
    if
        Var == undef, is_tuple(NewVar) -> intro(Clause, Top, NewVar, Mode);
        true -> ok
    end,
    NewVar.

ord(T) when is_tuple(T) -> 1;
ord(hidden)             -> 2;
ord(undef)              -> 3; 
ord(_)                  -> 4.

%% Create `varintro' and `varvis' link. In `del' mode, removed subtrees cannot
%% introduce new variables, the only possible scenario when this function is
%% called when turning a reference into a binding.
intro(Clause, Top, Var, _Mode) ->
    ?TRACE(["intro", ?VAR(Clause), ?VAR(Top)]),
    ?Graph:mklink(Clause, varvis, Var),
    case ?Graph:path(Clause, [visib, {intersect, Var, varintro}]) of
        [Top] -> ok;
        [Intro] ->
            ?Graph:rmlink(Var, varintro, Intro),
            ?Graph:mklink(Var, varintro, Top);
        [] ->
            ?Graph:mklink(Var, varintro, Top)
    end.

%% Delegate based on expression structure
expr(Expr, Name, Var, Mode) ->
    case ?Graph:path(Expr, [clause]) of
        [] -> simple(Expr, Name, Var, Mode);
        Cls -> compound(Cls, Name, Var, Mode)
    end.

%% Handle variable expressions
simple(Expr, Name, Var, Mode) ->
    case ?Graph:data(Expr) of
        #expr{kind=variable, type=Type, value=Name} ->
            ?TRACE(["varexpr", ?VAR(Expr), ?VAR(Type), ?VAR(Var)]),
            varexpr(Expr, Name, Mode, Type, Var);
        _ ->
            Var
    end.

%% Determine the correct reference kind for a variable occurrence.
varexpr(Expr, Name, Mode, pattern, undef) ->
    Var = variable(Expr, Name),
    update(Expr, varbind, Var, Mode);

varexpr(Expr, Name, Mode, pattern, hidden) ->
    Var = variable(Expr, Name),
    update(Expr, varbind, Var, Mode),
    hidden;    

varexpr(Expr, _Name, Mode, pattern, Var) ->
    update(Expr, varref, Var, Mode);

varexpr(Expr, _Name, Mode, _Type, Var) when is_tuple(Var) ->
    update(Expr, varref, Var, Mode);

varexpr(Expr, _Name, _Mode, _Type, Var) when Var == undef; Var == hidden ->
    case {?Graph:path(Expr, [varbind]), ?Graph:path(Expr, [varref])} of
        {[B], []} -> unlink_var(Expr, varbind, B);
        {[], [R]} -> unlink_var(Expr, varref, R);
        {[], []} -> ok
    end,
    Var.


%% Modify expression links to have a link with tag `Link' to `Var'.
update(Expr, Link, Var, Mode) ->
    ?TRACE(["update", ?VAR(Expr), ?VAR(Link), ?VAR(Var)]),
    case {?Graph:path(Expr, [varbind]), ?Graph:path(Expr, [varref])} of
        %% In `del' mode, no new links are created -- expressions without
        %% links are probably not in the syntax tree
        {[], []} when Mode == add -> varlink(Expr, Link, Var);
        {[], []} when Mode == del ->
            if
                is_tuple(Var), Link == varbind -> unuse(Var), undef;
                true -> Var
            end;

        {[Var], []} when Link == varbind -> Var;
        {[Old], []} ->
            unlink_var(Expr, varbind, Old),
            varlink(Expr, Link, Var);

        {[], [Var]} when Link == varref -> Var;
        {[], [Old]} ->
            unlink_var(Expr, varref, Old),
            varlink(Expr, Link, Var)
    end.

varlink(Expr, Link, Var) when is_tuple(Var) ->
    ?Graph:mklink(Expr, Link, Var),
    Var;
varlink(_Expr, _Link, Var) when Var == undef; Var == hidden ->
    Var.

%% Implement hiding rules for clauses
compound(Clauses, Name, Var, Mode) ->
    %% Simple clauses may introduce variable for other clauses
    {Results, NewVar} = 
        lists:mapfoldl(
          fun
              ({scope, Cl}, V) -> {{hide, scope(Cl, Name, V, Mode)}, V};
              ({simple, Cl}, V) ->
                  V1 = clause(Cl, Name, V, Mode),
                  {{simple, V1}, V1};
              ({Type, Cl}, V) -> {{Type, clause(Cl, Name, V, Mode)}, V}
          end,
          Var,
          [{(?Graph:data(Cl))#clause.type, Cl} || Cl <- Clauses]),
    ?TRACE(["compound", ?VAR(Results), ?VAR(NewVar)]),
    %% Branches introduce variable when all branches introduce it
    if
        NewVar == undef ->
            case lists:partition(fun erlang:is_tuple/1,
                                 [V || {branch, V} <- Results]) of
                {[AllVar|_], []} -> ?TRACE(?VAR(AllVar)), AllVar; 
                _ -> NewVar
            end;
        true -> NewVar
    end.

