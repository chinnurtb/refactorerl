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
%%% <li>A semantical variable object is created for every separate variable
%%%   that appears in the code.</li>
%%%
%%% <li>The variable object is linked from the clause that defines the scope of
%%%   the variable. (`vardef')</li>
%%%
%%% <li>Every occurrence of the variable is linked to the variable.
%%%   (`varref')</li>
%%%
%%% <li>Every clause where the variable is visible is linked to the variable
%%%   object (`varvis').</li>
%%%
%%% <li>Every expression that introduces the variable into its visibility
%%%   context is linked from the variable object (`varintro').</li>
%%%
%%% <li>Every occurrence that binds the variable is linked to the variable
%%%   object (`varbind').</li>
%%%
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(anal_variable).
-vsn("$Rev: 1247 $").
-behaviour(refac_anal).

%% Interface exports
-export([variable/1]).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").


%% @spec variable(string()) -> path()
%% @doc Path from a #clause{} to the visible variable `Name'
variable(Name) ->
    [{varvis, {name, '==', Name}}].

%% @private
init() ->
    [{variable, record_info(fields, variable), [{varintro, expr},
                                                {varbind, expr}]},
     {clause, [{vardef, variable}, {varvis, variable}]},
     {expr,   [{varref, variable}]}
    ].

%% @private
insert(_, _, _, VarExp, Expr = #expr{kind=variable, value=Name}) ->
    Var = variable_object(VarExp, Name),
    if
        Expr#expr.type =:= pattern -> update_binding(VarExp, Var);
        true -> ok
    end;

insert(_,_,_,_,_) ->
    ok.


%% Find or create a variable object by name
variable_object(Expr, Name) ->
    case ?GRAPH:path(Expr, anal_context:expr_visib() ++ variable(Name)) of
        [Var|_] -> ok;
        [] ->
            Scopes = ?GRAPH:path(Expr,
                                 anal_context:expr_visib() ++
                                 anal_context:clause_scopes()),
            %% TODO check each scope
            case ?GRAPH:path(hd(Scopes), [{vardef, {name, '==', Name}}]) of
                [Var] -> ok;
                [] ->
                    Var = ?GRAPH:create(#variable{name=Name}),
                    ?GRAPH:mklink(hd(Scopes), vardef, Var)
            end
    end,
    %%?GRAPH:mklink(Expr, varref, Var),
    Var.

%% Update variable binding structure when a new variable pattern match is
%% found
update_binding(VarExp, Var) ->
    case update_binding(expr_parent(VarExp),
                        VarExp, ?GRAPH:data(VarExp), Var) of
        true ->
            ?GRAPH:mklink(Var, varbind, VarExp),
            ?GRAPH:mklink(VarExp, varref, Var);
        _    -> ok
    end.


%% Update introduction links wrt `Var', return true if `Sub' can contain
%% binding occurrences. Update visibility area as well. Walk the syntax tree
%% upwards: `Expr' is the parent of `Sub', `SubData == data(Sub)'. Pass the
%% data tag of `Expr' for pattern matching purposes.
update_binding(Expr, Sub, SubData, Var) ->
    update_binding(Expr, ?GRAPH:data(Expr), Sub, SubData, Var).

update_binding(E, D=#expr{}, _, #expr{}, Var) ->
    update_binding(expr_parent(E), E, D, Var);

update_binding(Clause, D=#clause{}, Sub, #expr{}, Var) ->
    %% Is the variable already visible?
    Vis = lists:member(Var, ?GRAPH:path(Clause, [varvis])),
    if
        not Vis -> insert_intro(Clause, D, Sub, Var);
        Vis ->
            %% Is the variable introduced in this clause?
            case ?GRAPH:path(Clause, anal_context:clause_visib() ++
                             [{intersect, Var, varintro}]) of
                [Intro] -> update_intro(Clause, D, Intro, Sub, Var);
                []      -> false
            end
    end;

update_binding(_Expr, #expr{kind=catch_expr}, _Sub, #clause{}, _Var) ->
    true;

update_binding(Expr, D=#expr{}, _Sub, #clause{}, Var) ->
    Cls = ?GRAPH:path(Expr, [clause]),
    Vis = ?GRAPH:path(Expr, [clause, {intersect, Var, {varvis, back}}]),
    if
        %% Is the variable visible in every clause?
        length(Cls) =:= length(Vis) ->
            update_binding(expr_parent(Expr), Expr, D, Var);
        true ->
            true
    end.

%% Make `Expr' introduce `Var' into `Clause' when `Var' is not visible yet.
%% Should always return `true'.
insert_intro(Clause, D, Expr, Var) ->
    Content = ?GRAPH:path(Clause, anal_context:clause_visib()),
    Visib = lists:dropwhile(fun(E) -> E =/= Expr end, Content),
    ?GRAPH:mklink(Clause, varvis, Var),
    ?GRAPH:mklink(Var, varintro, Expr),
    update_visib(tl(Visib), Var),
    case clause_parent(Clause) of
        none   -> true;
        Parent -> update_binding(Parent, Clause, D, Var)
    end.

%% Make `Expr' introduce `Var' into `Clause' when `Var' is already visible,
%% and `Intro' is the current introducing expression. Return `true' if `Expr'
%% is really an introducing expression.
update_intro(_, _, Intro, Expr, _) when Intro == Expr ->
    true;
update_intro(Clause, D, Intro, Expr, Var) ->
    Content = ?GRAPH:path(Clause, anal_context:clause_visib()),
    Visib = lists:dropwhile(fun(E) -> E /= Expr end, Content),
    Extend = lists:member(Intro, Visib),
    if
        not Extend -> false;
        Extend ->
            remove_intro(Intro, Var),
            ?GRAPH:mklink(Var, varintro, Expr),
            Prefix = lists:takewhile(fun(E) -> E /= Intro end, Visib),
            update_visib(tl(Prefix) ++ [Intro], Var),

            case clause_parent(Clause) of
                none   -> true;
                Parent -> update_binding(Parent, Clause, D, Var)
            end
    end.


%% Propagate variable visibility in a list of expressions.
update_visib(List, Var) ->
    update_visib(List, Var, (?GRAPH:data(Var))#variable.name).

update_visib([Node | Rest], Var, Name) ->
    case ?GRAPH:data(Node) of
        #clause{} ->
            ?GRAPH:mklink(Node, varvis, Var),
            update_visib(?GRAPH:path(Node, anal_context:clause_visib()),
                         Var, Name);
        
        #expr{kind=variable, value=Name} ->
            ?GRAPH:mklink(Node, varref, Var);

        #expr{} ->
            update_visib(?GRAPH:path(Node, [sub]) ++
                         ?GRAPH:path(Node, [clause]) ++
                         ?GRAPH:path(Node, [handlercl]) ++
                         ?GRAPH:path(Node, [aftercl]), Var, Name)
    end,
    update_visib(Rest, Var, Name);
update_visib([], _, _) ->
    ok.

%% Remove variable binding and introduction links from a subtree.
remove_intro(Node, Var) ->
    remove_intro(Node, ?GRAPH:data(Node), Var).

remove_intro(Expr, #expr{}, Var) ->
    ?GRAPH:rmlink(Expr, varintro, Var),
    
    Bindings = ?GRAPH:path(Expr, [{sup, back}, {intersect, Var, varbind}]),
    lists:foreach(fun(B) -> ?GRAPH:rmlink(Var, varbind, B) end, Bindings),
    
    Clauses = ?GRAPH:path(Expr, [{sup, back}, clause]) ++
        ?GRAPH:path(Expr, [{sup, back}, aftercl]) ++
        ?GRAPH:path(Expr, [{sup, back}, handlercl]),
    lists:foreach(fun(C) -> remove_intro(C, Var) end, Clauses);

remove_intro(Clause, #clause{}, Var) ->
    [Intro] = ?GRAPH:path(Clause, anal_context:clause_visib() ++
                          [{intersect, Var, varintro}]),
    remove_intro(Intro, Var).


%% Return the parent node of `Expr'.
expr_parent(Expr) ->
    parent(Expr, [{sub, back}, {body, back}, {pattern, back}, {guard, back}]).

%% Return the parent node of `Clause'.
clause_parent(Clause) ->
    parent(Clause, [{clause, back}, {aftercl, back}, {handlercl, back}]).

parent(N, [Tag | Rest]) ->
    case ?GRAPH:path(N, [Tag]) of
        [Parent] -> Parent;
        []       -> parent(N, Rest)
    end;
parent(_, []) ->
    none.

%% @private
remove(_,_,_,Scope, #clause{scope=true}) ->
    %% TODO: This is just a stupid garbage collection that works well at file
    %% removals
    [?GRAPH:delete(Var) || Var <- ?GRAPH:path(Scope, [vardef]) ];

remove(_,_,_,_,_) ->
    ok.
