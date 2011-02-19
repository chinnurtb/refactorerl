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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2010,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc This module contains facilities for storing
%%% the semantic part of the graph into a file, and for restoring it.
%%% This is used to store only the necessary information about
%%% library modules, or indeed any set of modules.

-module(refcore_store_graph).

-export([save/1, load/1]).

-include("core.hrl").

%% @spec save(string()) -> ok
%% @doc Saves the semantic information stored in the graph in a group of files
%% with the given base filename and several different extensions.
save(FileName) ->
    try
        {Nodes, Links} = ?Syn:walk_graph(?Graph:root(), semdf, fun act/4, {[], []},
                                         fun refanal_dataflow:back_nodes/1),
        {Nodes2, Links2} = simplify_df(Nodes, Links),
        Mods = [(?Graph:data(Mod))#module.name
                    || Mod <- ?Graph:path(?Graph:root(), [module])],
        {ok, Dev} = file:open(FileName, [write]),
        io:format(Dev, "~p.~n~p.~n~p.~n", [Mods, Nodes2, Links2])
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason}
    end.

%% This function simplifies the structure of the saved data flow subgraph,
%% e.g. eliminates superfluous `flow' edges.
simplify_df(Nodes, Links) ->
    remove_unreachable_dataflow(Nodes, Links).
%    {Nodes2, Links}.

%    {Nodes3, Links3} = simplify_df(Nodes, Nodes2, Links),
%    simplify_df(Nodes, Nodes, Links).


%% Removes those expressions from the graph that do not contribute
%% to the data flow of the functions: those that do not lead to a `fret' node.
%% During analysis, turns the intermediate nodes into `dflow' ones.
remove_unreachable_dataflow(Nodes, Links) ->
    Frets = [Node || {Node, #expr{type=fret}} <- Nodes],
    Reacheds = reachables(Frets, Nodes, Links, []),
    remove_loop_flows(simplify_df(leave_only_reached({Nodes, Links}, Reacheds))).

%% Removes those #expr{} nodes and related links that are bag ends for
%% the data flow.
leave_only_reached({Nodes, Links}, Reacheds) ->
    Nodes2 = lists:filter(fun({Node, #expr{}}) -> lists:member(Node, Reacheds); (_) -> true end, Nodes),
    Nodes3 = [expr_role_to_dflow(ND) || ND <- Nodes2],
    Links3 = [FTT || FTT <- Links, only_reached_exprs(FTT, Reacheds, Nodes2)],
    {Nodes3, Links3}.

%% Removes the looping flow edges.
remove_loop_flows({Nodes, Links}) ->
    Links2 = [FTT || FTT = {From, {Tag, To}} <- Links, not (From == To andalso Tag == flow)],
    {Nodes, Links2}.

only_reached_exprs({From, {_Tag, To}}, Reacheds, Nodes3) ->
    test_reached(From, Nodes3, Reacheds) andalso test_reached(To, Nodes3, Reacheds).

test_reached(Node, Nodes, Reacheds) ->
    case proplists:get_value(Node, Nodes) of
        #expr{} ->
            lists:member(Node, Reacheds);
        undefined ->
            false;
        _ ->
            true
    end.

expr_role_to_dflow({Node, Data = #expr{}}) ->
    {Node, Data#expr{role=dflow}};
expr_role_to_dflow(ND) ->
    ND.

reachables([], _Nodes, _Links, Reacheds) ->
    Reacheds;
reachables([Node|Rest], Nodes, Links, Reacheds) ->
    case lists:member(Node, Reacheds) of
        true ->
            reachables(Rest, Nodes, Links, Reacheds);
        false ->
            BackNodes = refanal_dataflow:back_nodes(Node),
            reachables(BackNodes ++ Rest, Nodes, Links, [Node|Reacheds])
    end.


simplify_df({Nodes, Links}) ->
    simplify_df(Nodes, Nodes, Links).

simplify_df([], Nodes, Links) ->
    {Nodes, Links};
simplify_df([{_, #expr{type=Type}}|Rest], Nodes, Links) when Type == fret; Type == fpar ->
    simplify_df(Rest, Nodes, Links);
simplify_df([{Node, _Data}|Rest], Nodes, Links) ->
    case to_links(Node, Links) of
        [{flow, From}] ->
            Links2 = Links -- [{From, {flow, Node}}],
            Links3 = [mv_link_node(Node, From, L) || L <- Links2],
            Nodes3 = rm_node(Node, Nodes),
            simplify_df({Nodes3, Links3});
        _ ->
            case from_links(Node, Links) of
                [{flow, To}] ->
                    Links2 = Links -- [{Node, {flow, To}}],
                    Links3 = [mv_link_node(Node, To, L) || L <- Links2],
                    Nodes3 = rm_node(Node, Nodes),
                    simplify_df({Nodes3, Links3});
                _ ->
                    simplify_df(Rest, Nodes, Links)
            end
    end.

mv_link_node(Old, New, {Old,  {Link, To}})  -> {New,  {Link, To}};
mv_link_node(Old, New, {From, {Link, Old}}) -> {From, {Link, New}};
mv_link_node(_, _, Link)                    -> Link.


rm_node(RmNode, Nodes) ->
    [ND || ND = {Node, _} <- Nodes, Node /= RmNode].

to_links(Node, Links) ->
    [{Link, From} || {From, {Link, To}} <- Links, To == Node, Link =/= cons_back].

from_links(Node, Links) ->
    [{Link, To} || {From, {Link, To}} <- Links, From == Node].



%% @spec load(string()) -> ok
%% @doc Loads the semantic module information saved in a group of files,
%% provided that none of the modules to be added are already present.
load(FileName) ->
    try
        {ok, [NewMods, Nodes, Links]} = file:consult(FileName),

        check_no_mod_clash(NewMods),

%        todo annotate that these are new nodes ("protect them")
%        Node2New = [{?Graph:root(), ?Graph:root()} |
%                    [{Node, ?Graph:create_prot(Data)}
%                        || {Node, Data} <- Nodes,
%                                           Node /= ?Graph:root()]],
        Node2New = [{?Graph:root(), ?Graph:root()} |
                    [{Node, ?Graph:create(Data)}
                        || {Node, Data} <- Nodes,
                           Node /= ?Graph:root()]],
        ToNew = fun proplists:get_value/2,
%        todo annotate that these are new edges ("protect them")
%        [?Graph:mklink_prot(ToNew(From, Node2New), Tag, ToNew(To, Node2New))
%            || {From, {Tag, To}} <- Links],

        [?Graph:mklink(ToNew(From, Node2New), Tag, ToNew(To, Node2New))
            || {From, {Tag, To}} <- Links],
        ok
    catch
        throw:{clash_mods, ErrMods} ->
            ModTxt = ?MISC:plural("module", ErrMods),
            EMTxts = ?MISC:join([atom_to_list(Err) || Err <- ErrMods], ", "),
            {error, ?MISC:format("~s already present: ~s", [ModTxt, EMTxts])};
        error:{badmatch, {error, Reason}} ->
            {error, Reason}
    end.

%% Throws an exception if at least one module whose semantic informations
%% are to be added is already present in the graph.
check_no_mod_clash(NewMods) ->
    Mods = [(?Graph:data(Mod))#module.name
                || Mod <- ?Graph:path(?Graph:root(), [module])],
    case NewMods -- Mods of
        NewMods -> ok;
        OKMods  -> throw({clash_mods, NewMods -- OKMods})
    end.

%% Adds the current node and its links to the collected state.
act(Node, _State = {Nodes, AllLinks}, Links, WalkNext) ->
    NewLinks = [{Node, Link} || Link <- Links],
    NewSt = {[{Node, ?Graph:data(Node)}|Nodes], NewLinks ++ AllLinks},
    WalkNext(NewSt, fun node_is_done/2).


%% A callback function to use with `?Syn:walk_graph'.
%% Returns whether `Node' has already been reached during the traversal.
node_is_done(Node, {Nodes, _Links}) ->
    lists:any(fun({Node2, _Data}) -> Node2 =:= Node end, Nodes).
