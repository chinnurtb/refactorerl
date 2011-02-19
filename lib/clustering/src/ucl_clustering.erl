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

%%% ============================================================================
%%% Module Informations

%%% @doc Library to manage the clustering of entities.
%%% A {@type clClustering} record contains the clusters, clusterings and the 
%%% connections between the clusters. This module contain usefull
%%% function which make easy to create or modify the clustering or query
%%% informatins about it.
%%%
%%% == TODO ==
%%% Manage cluster connection matrix.

%%% @todo Manage cluster connection matrix

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_clustering).
-vsn("$Rev: $").

-include("ucluster.hrl").



%%% ============================================================================
%%% Imports/exports

% Clustering
-export([new/0, delete/1, clone/1, pack/1, unpack/1]).
% Manage clusters
-export([clusterIds/1, get_clusters/2, set_clusters/2,
         get_cluster_ents/2, make_cluster/3, 
         move_ents/4, join_clusters/3]).



%%% ============================================================================
%%% Clustering

%% @spec new() -> Clustering::clClustering()
%% @doc  Create a new empty clustering.
new() ->
    #clClustering{
        clusters   = ets:new(cl_clusters, [{keypos,#clCluster.id}]),
        cl_members = ets:new(cl_members,  [bag])}.

%% @spec delete(Clustering::clClustering()) -> ok
%% @doc  Delete the `Clustering' clustering.
delete(Cl=#clClustering{}) ->
    ets:delete(Cl#clClustering.clusters),
    ets:delete(Cl#clClustering.cl_members),
    ?ClCommon:matrix_delete(Cl#clClustering.cl_conn).

%% @spec clone(Clustering::clClustering()) -> Clone::clClustering()
%% @doc Create a copy of the `Clustering'. The original and the copy are fully
%%      separated. There are no common dependencies like ETS tables.
clone(Cl=#clClustering{}) ->
    Cl#clClustering{
        clusters   = ?MISC:ets_clone(Cl#clClustering.clusters),
        cl_members = ?MISC:ets_clone(Cl#clClustering.cl_members),
        cl_conn    = ?ClCommon:matrix_clone(Cl#clClustering.cl_conn)}.


%% @spec pack(Clustering::clClustering()) -> PackedClustering::clClustering()
%% @doc Create a new `clClustering' object which contains all information from 
%%      the original `Clustering' as values.
%%      Don't use the returned `PackedClustering' object as the normal
%%      `clClustering' objects! In this case a run time error may occur.
%%      If you have a packed `clClustering' object you need to unpack with the
%%      {@link unpack/1} function before use it.
%% @see unpack/1
pack(Cl=#clClustering{}) ->
    Cl#clClustering{
        clusters    = ets:tab2list(Cl#clClustering.clusters),
        cl_members  = ets:tab2list(Cl#clClustering.cl_members),
        cl_conn     = ?ClCommon:matrix_pack(Cl#clClustering.cl_conn)}.

%% @spec unpack(PackedClustering::clClustering()) -> Clustering::clClustering()
%% @doc Create a normal `clClustering' object from the given packed
%%      `PackedClustering' objecjt.
%% @see pack/1
unpack(Cl=#clClustering{}) ->
    Cl1 = new(),
    ?MISC:ets_list2tab(Cl#clClustering.clusters,   Cl1#clClustering.clusters),
    ?MISC:ets_list2tab(Cl#clClustering.cl_members, Cl1#clClustering.cl_members),
    Cl1#clClustering{
        cl_conn = ?ClCommon:matrix_unpack(Cl#clClustering.cl_conn)}.



%%% ============================================================================
%%% Manage clusters

%% @spec clusterIds(Clustering::clClustering()) -> Ids::[term()]
%% @doc  Return the identifiers of all clusters in the clustering.
clusterIds(#clClustering{clusters=ClustersETS}) ->
    ?MISC:ets_keys(ClustersETS).


%% @spec get_clusters(Ids::[term()], Clustering::clClustering()) -> 
%%           Clusters::[clCluster()]
%% @doc  Convert the `Ids' cluster identifier list into cluster list.
get_clusters(Ids, #clClustering{clusters=ClustersETS}) when is_list(Ids) ->
    lists:foldl(
        fun(Id, Acc) -> 
            case ets:lookup(ClustersETS, Id) of
                [Cluster=#clCluster{}] -> [Cluster|Acc];
                _ -> Acc
            end
        end,
        [],
        lists:reverse(Ids)).


%% @spec set_clusters(Clusters::[clCluster()], Clustering::clClustering()) -> 
%%           NewClustering::clClustering()
%% @doc  Add or replace clusters.
set_clusters(Clusters, Cling=#clClustering{clusters=ClustersETS}) when 
        is_list(Clusters) ->
    lists:foreach(
        fun(Cl=#clCluster{}) -> ets:insert(ClustersETS, Cl) end,
        Clusters),
    Cling.


%% @spec get_cluster_ents(ClusterId::term(), Clustering::clClustering()) ->
%%           EntIds::[term()]
%% @doc  Return the identifiers of entities in the cluster given by `ClusterId'.
get_cluster_ents(ClId, #clClustering{cl_members=ClMemETS}) ->
    ets:select(ClMemETS, [{ {'$1','$2'}, [{'==','$1',ClId}], ['$2'] }]).


%% @spec make_cluster(EntIds::[term()], Cluster::clCluster(), 
%%               Clustering::clClustering()) -> NewClustering::clClustering()
%% @doc Add the new `Cluster' into the `clusters' set and all entities from
%%      `EntIds' member of `Cluster'.
make_cluster(EntIds, Cluster=#clCluster{id=ClId}, 
        Cling=#clClustering{cl_members=ClMemETS}) when is_list(EntIds) ->
    set_clusters(Cluster, Cling),
    lists:foreach(
        fun(EntId) ->
            % Delete entity from the current cluster
            case ets:select(ClMemETS, [{ {'_','$2'}, [{'==','$2',EntId}], 
                            ['$_'] }]) of
                [MemberObj] -> ets:delete(ClMemETS, MemberObj);
                []          -> ok
            end,
            % Add entity to the new cluster
            ets:insert(ClMemETS,{ClId,EntId})
        end,
        EntIds),
    Cling.


%% @spec move_ents(EntIds::[term()], ClFromId::term(), ClToId::term(),
%%               Clustering::clClustering()) -> NewClustering::clClustering()
%% @doc Move entities from `ClFormId' cluster to `ClToId' cluster. If the source
%%      cluster became emty that will be deleted.
move_ents(EntIds, ClFromId, ClToId, Cling=#clClustering{clusters=ClETS, 
        cl_members=ClMemETS}) when is_list(EntIds) ->
    % Move entities between clusters
    lists:foreach(
        fun(EntId) ->
            ets:delete_object(ClMemETS, {ClFromId, EntId}),
            ets:insert(ClMemETS, {ClToId, EntId})
        end,
        EntIds),
    % If source cluster became emty, delete that
    case ets:lookup(ClMemETS, ClFromId) of
        [] -> ets:delete(ClETS, ClFromId);
        _  -> ok
    end,
    Cling.


%% @spec join_clusters(ClBaseId::term(), ClIds::[term()],
%%               Clustering::clClustering()) -> NewClustering::clClustering()
%% @doc Merge clusters. `ClBaseId' the identifier of the cluster where move all
%%      entities from other clusters. The other `ClIds' clusters will be
%%      deleted.
join_clusters(ClBaseId, ClIds, Cling=#clClustering{}) ->
    lists:foreach(
        fun(ClId) ->
            move_ents(get_cluster_ents(ClId,Cling), ClId, ClBaseId, Cling)
        end,
        ClIds),
    Cling.



%%% ============================================================================
%%% Partition entities

%% @ spec create_open_clusters(OpenModNames::[atom()], State::clState()) ->
%%           NewState::clState()
%% @ doc Create open clusters from all single items that `modname' property of
%%      the item is in the `OpenModNames' list. All other item will be groped
%%      into common clusters by the `modename' value.
% create_clusters(Mode, ModNames, St=#clState{etc=EtcETS}) when 
        % is_atom(Mode), is_list(ModNames) ->
    % % Collect entities by module name
    % {CloseDict, OpenLst} = lists:foldl(
        % fun(#clItem{id=Id, props=Props}, {AccDict, AccLst}) ->
            % ModNames1 = case proplists:is_defined(modname, Props) of
                % true -> [get_value(modname, Props)];
                % _ -> lists:usort(lists:map(fun ?ClAttr:get_node_modname/1, 
                        % ?Query:exec(get_value(file,Props), ?ClAttr:fileMods()))
            % end,
            % case (Mode==close andalso []==ModNames1--ModNames) orelse
                % (Mode==open andalso []/=?MISC:intersect(ModNames1, ModNames)) of
                % true -> {AccDict, [Id|AccLst]};
                % _    -> {dict:append(hd(ModNames1), Id, AccDict), AccLst}
            % end
        % end,
        % {dict:new(), []},
        % ?ClState:entItems(St)),
    % % Get identifier for the next new cluster
    % NextClId0 = case ets:lookup(EtcETS, next_cl_id) of
        % [N] -> N;
        % []  -> 1
    % end,
    % % Create clusters from closed modules
    % {St1, NextClId1} = dict:fold(
        % fun(ModName, EntIds, {AccSt, AccClId}) ->
            % Cl = #clCluster{id=AccClId, mode=close, name=ModName},
            % {?ClState:make_cluster(EntIds, Cl, AccSt), AccClId+1}
        % end,
        % {St, NextClId0},
        % CloseDict),
    % % Create clusters from entities in open modules
    % {St2, NextClId2} = lists:foldl(
        % fun(EntId, {AccSt, AccClId}) ->
            % Cl = #clCluster{id=AccClId, mode=open, 
                % name=?MISC:format("cl~p", [AccClId])},
            % {?ClState:make_cluster([EntId], Cl, AccSt), AccClId+1}
        % end,
        % {St1, NextClId1},
        % OpenLst),
    % % Update state
    % ets:insert(EtcETS, {next_cl_id, NextClId2}),
    % St2.


%%
%%
% create_clusters(Mode, FilePaths, St=#clState{etc=EtcETS}) when 
        % is_atom(Mode), is_list(FilePaths) ->
    % % Collect entities by module name
    % {CloseDict, OpenLst} = lists:foldl(
        % fun(#clItem{id=Id, props=Props}, {AccDict, AccLst}) ->
            % FilePath = get_value(filepath, Props),
            % InSet    = lists:member(FilePath, FilePaths),
            % case (Mode==open  andalso InSet) orelse 
                 % (Mode==close andalso (not InSet)) of
                % true -> {AccDict, [Id|AccLst]};
                % _    -> {dict:append(FilePath, Id, AccDict), AccLst}
            % end
        % end,
        % {dict:new(), []},
        % ?ClState:entItems(St)),
    % % Get identifier for the next new cluster
    % NextClId0 = case ets:lookup(EtcETS, next_cl_id) of
        % [N] -> N;
        % []  -> 1
    % end,
    % % Create clusters from closed modules
    % {St1, NextClId1} = dict:fold(
        % fun(FilePath, EntIds, {AccSt, AccClId}) ->
            % Cl = #clCluster{id=AccClId, mode=close, name=FilePath},
            % {?ClState:make_cluster(EntIds, Cl, AccSt), AccClId+1}
        % end,
        % {St, NextClId0},
        % CloseDict),
    % % Create clusters from entities in open modules
    % {St2, NextClId2} = lists:foldl(
        % fun(EntId, {AccSt, AccClId}) ->
            % Cl = #clCluster{id=AccClId, mode=open, 
                % name=?MISC:format("cl~p", [AccClId])},
            % {?ClState:make_cluster([EntId], Cl, AccSt), AccClId+1}
        % end,
        % {St1, NextClId1},
        % OpenLst),
    % % Update state
    % ets:insert(EtcETS, {next_cl_id, NextClId2}),
    % St2.



