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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc This module draws the automaton of a gen_fsm module.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(reflib_draw_gen_fsm).

-export([draw/2]).

-include("lib.hrl").

%% Opens `ModName' and draws its automaton into `ToFile' in dot and svg formats.
draw(ModName, ToFile) ->
    [Mod] = ?Query:exec(?Mod:find(ModName)),
    Init  = ?Query:exec1(Mod, ?Fun:find(init, 1), ?RefErr0r(bad_fun)),
        % todo should be as simple as this:
%    Exporteds = get_exporteds(Mod),
    Exporteds = [Exp || Exp = {Name, _} <- get_exporteds(Mod), Name =/= start_link],

    {ok, Dev} = file:open(ToFile, [write]),
    io:format(Dev, "digraph erl {~n", []),

    Inits = [{FunName, Fun, true} || {FunName, Fun} <- [{init, Init}|Exporteds]],
    draw_graph(Inits, [], Dev, Mod),

    io:format(Dev, "}~n", []),
    file:close(Dev),
    ToRoot = filename:rootname(ToFile),
    os:cmd("dot -Tsvg " ++ ToRoot ++ ".dot -o" ++ ToRoot ++ ".svg").

%% Walks the graph and draws one node at a time.
draw_graph([], _Reached, _Dev, _Mod) ->
    done;
draw_graph([{FunName, Node, IsInit}|Nodes], Done, Dev, Mod) ->
    case lists:member(Node, Done) of
        false ->
            Rets      = fun_return_points({function, Node}),
            NextSts   = lists:usort(lists:flatten(
                            [next_state(Mod, Ret) || Ret <- Rets])),
            IsStop    = lists:member(stop_state, NextSts),
            NextFuns  = [{Next, find_fun(Mod, Next), false} || {next, Next} <- NextSts],
            UNextFuns = lists:usort(lists:flatten(NextFuns)),
            draw_node(Dev, FunName, IsStop, IsInit),
            [draw_edge(Dev, FunName, Next) || {Next, _, _} <- UNextFuns],
            Remaining = lists:usort(NextFuns) ++ Nodes,
            draw_graph(Remaining, [Node|Done], Dev, Mod);
        true ->
            draw_graph(Nodes, Done, Dev, Mod)
    end.

%% Returns the function semantic node for `Mod':`FunName'.
find_fun(Mod, FunName) ->
    case ?Query:exec(Mod, ?Fun:find(FunName, 2)) of
        [Fun] -> Fun;
        []    -> ?Query:exec1(Mod, ?Fun:find(FunName, 3),
                              ?RefError(bad_fun, FunName))
    end.

%% Draws a node in style.
draw_node(Dev, Name, IsStop, IsInit) ->
    Shape =
        case IsStop of
            true  -> "doublecircle";
            false -> "circle"
        end,
    Color =
        case IsInit of
            true  -> "lightgreen";
            false -> "white"
        end,
    io:format(Dev, "~p [shape=\"" ++ Shape ++ "\", "
                       "fillcolor=\"" ++ Color ++ "\","
                       " style=\"filled\"]~n", [Name]).

%% Draws the edge link.
draw_edge(Dev, Name1, Name2) ->
    io:format(Dev, "~p -> ~p~n", [Name1, Name2]).

%% Returns the names of the next state(s) of the automaton.
%% The argument `Expr' is a return point of the function.
%% The possible return values are {next, Atom} | stop_state | ignore
%% or a list of these.
next_state(Mod, Expr) ->
    next_state(?Expr:type(Expr), Mod, Expr).

next_state(tuple, _Mod, Expr) ->
    [Node1] = ?Query:exec(Expr, ?Expr:child(1)),
    case {?Expr:type(Node1), ?Expr:value(Node1)} of
        {atom, Next} when Next =:= ok; Next =:= next_state ->
            [Node2] = ?Query:exec(Expr, ?Expr:child(2)),
            retval_next_state(Node2);
        {atom, reply} ->
            [Node3] = ?Query:exec(Expr, ?Expr:child(3)),
            retval_next_state(Node3);
        {atom, stop} ->
            stop_state;
        _ ->
            ignore
    end;
next_state(application, Mod, Expr) ->
    [Node1] = ?Query:exec(Expr, ?Expr:child(1)),
    [Node2] = ?Query:exec(Expr, ?Expr:child(2)),
    Args    = ?Query:exec(Node2, ?Expr:children()),
    Arity   = length(Args),
    {Mod2, Fun} =
        case ?Expr:type(Node1) of
            infix_expr ->
                [Node11]   = ?Query:exec(Node1, ?Expr:child(1)),
                [Node12]   = ?Query:exec(Node1, ?Expr:child(2)),
                NewModName = ?Expr:value(Node11),
                [NewMod]   = ?Query:exec(?Mod:find(NewModName)),
                {NewMod, Node12};
            atom ->
                {Mod, Node1}
        end,
    FunFound =
        case ?Expr:type(Fun) of
            atom ->
                {found, ?Expr:value(Fun)};
            variable ->
                % todo use data flow here as well
                not_found;
            _ ->
                not_found
        end,
    case {?Mod:name(Mod2), FunFound} of
        {gen_fsm, _} ->
            % ignoring internal gen_fsm functionality
            ignore;
        {_, {found, FunName}} ->
            Fun2 = ?Query:exec1(Mod2, ?Fun:find(FunName, Arity), ?RefErr0r(bad_fun)),
            % todo Multi-step loops should be avoided as well.
            Reached = fun_return_points({function, Fun2}) -- [Expr],
            [next_state(Mod2, Node) || Node <- Reached];
        {_, not_found} ->
            []
    end;
next_state(_, _Mod, _Expr) ->
    ignore.

%% Having found the component of the tuple that determines the next state,
%% this function returns the next states.
retval_next_state(Expr) ->
    case ?Expr:type(Expr) of
        atom ->
            [{next, ?Expr:value(Expr)}];
        variable ->
            Reached = ?Dataflow:reach([Expr], [back]) -- [Expr],
            [{next, ?Expr:value(Expr2)} || Expr2 <- Reached,
                                           ?Expr:type(Expr2) =:= atom];
        tuple ->
            [Node11] = ?Query:exec(Expr, ?Expr:child(1)),
            case ?Expr:type(Node11) of
                error ->
                    todo_make_error_node,
                    ignore;
                _ ->
                    ignore
            end;
        _ ->
            % todo could use data flow for an application
            ignore
    end.

%% todo this would be the right way to get the exported funs:
%% ?d(?Query:exec(Mod, ?Mod:exports())),
get_exporteds(Mod) ->
    AllForms = ?Query:exec(Mod, ?Query:seq(?Mod:file(), ?File:forms())),
    ExportForms = [Form || Form <- AllForms, ?Form:type(Form) == export],
    AllFuns = ?Query:exec(ExportForms, ?Query:seq(?Form:exprs(), ?Expr:children())),
    lists:map(
        fun(Fun) ->
            [NameN, ArityN] = ?Query:exec(Fun, ?Expr:children()),
            FunName  = ?Expr:value(NameN),
            FunArity = ?Expr:value(ArityN),
            {FunName, ?Query:exec1(Mod, ?Fun:find(FunName, FunArity), ?RefErr0r(bad_export))}
        end, AllFuns).

% todo Move the function from refusr_metrics to a general module.
fun_return_points(F) ->
    refusr_metrics:fun_return_points(F).
