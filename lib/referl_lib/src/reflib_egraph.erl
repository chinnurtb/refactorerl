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

%%% @doc This module can convert the graph (stored in the Mnesia database) to
%%% graph that is stored it ets (this "ets graph" is called "egraph").
%%%
%%% @todo Write the precise type of filter_fun()
%%% @todo The filter function part of the refac_draw_graph module should be put
%%% into a new module, so it could be used with this module, as well.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(reflib_egraph).
-vsn("$Rev: 3816 $").

-export([create_egraph/0, create_egraph/1]).

-include("lib.hrl").

%%% @type egraph() = ets({node(), {data(), ListOfLinks::[{tag(),node()}]}}).
%%%
%%% A graph that represents the graph that is in the Mnesia database of the
%%% tool.
%%% The types `node()', `data()' and `tag()' are described in the documentation
%%% of the `refac_graph' module.

%% @spec create_egraph() -> egraph()
%%
%% @doc Converts the graph in the database into egraph with the default options.
%% Same as `create_egraph([])'.
create_egraph() ->
    create_egraph([]).

%% @spec create_egraph(atom()) -> egraph()
%%
%% @doc Converts the graph in the database into egraph with edge filtering.
%% To not use filtering, invoke this function with `all' as `Filter'.
%%
%% Options:
%% <ul>
%%      <li>`ets_name': the name of the ets table that will be created.
%%          Default value: `egraph'.</li>
%%      <li>`filter': the filtering that will be used.
%%          Default value: `all'.</li>
%% </ul>
%%
%% @todo Maybe also the back edges should be included in the graph?
create_egraph(Options) ->
    Opts = cl_utils:proplist_update(create_egraph_default(),Options),
    E = ets:new(proplists:get_value(ets_name,Opts),[]),
    create_egraph(E, ?Graph:root(),
                  filter(proplists:get_value(filter,Opts))),
    E.

create_egraph_default() ->
    [{ets_name,egraph},
     {filter,all}].

%% @spec create_egraph(egraph(), node(), filter_fun()) -> ok
%%
%% @doc Adds `Node' and all the connected nodes recursively to `Ets' 
%% (it does not add a node if it is already in `Ets').
create_egraph(Ets, Node, Filter) ->
    case ets:lookup(Ets, Node) of
        [] ->
            Data = ?Graph:data(Node),
            Links = [Link || Link = {Tag, _To} <- ?Graph:links(Node),
                             Filter(element(1, Data), Tag)],
            ets:insert(Ets, {Node, {Data, Links}}),
            lists:foldl(fun ({_Tag, To}, _) ->
                                create_egraph(Ets, To, Filter)
                        end, 
                        ok,
                        Links),
            ok;
        _ -> 
            ok
    end.

filter(all) ->
    fun all_links/2.

all_links(_, _) -> 
    true.
