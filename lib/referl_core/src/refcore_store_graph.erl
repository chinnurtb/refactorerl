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
        {Nodes, Links} = ?Syn:walk_graph(?Graph:root(), sem, fun act/4, {[], []}),
        Mods = [(?Graph:data(Mod))#module.name
                    || Mod <- ?Graph:path(?Graph:root(), [module])],
        {ok, Dev} = file:open(FileName, [write]),
        io:format(Dev, "~p.~n~p.~n~p.~n", [Mods, Nodes, Links])
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason}
    end.

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
