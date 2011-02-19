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

%%% @doc Manage the state of clustering
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(kcl_state).
-vsn("$Rev:  $").

-include("kcl.hrl").


%%% ============================================================================
%%% Imports/exports

%Imports
-import(proplists, [get_value/2, get_value/3]).


% Clustering state
-export([new/0, delete/1, to_record/1, from_record/1,
         save/2, load/1]).
% Building attribute matrix
-export([load_attr_matrix/3, build_attr_matrix/5]).
% Manage items
-export([get_itemIds/1, get_items/2, set_items/2, del_item/2, 
         entIds/1, attrIds/1, entItems/1, attrItems/1,
         add_ent/3, add_attr/3, del_ent/2, del_attr/2]).
% Connections between entities
-export([calc_conn_matrix/4, get_conn/3]).
% Manage clusters
-export([get_clusterIds/1, get_clusters/2, set_clusters/2,
         get_cluster_ents/2, make_cluster/3, 
         move_ents/4, join_clusters/3]).



%%% ============================================================================
%%% Clustering state

%% @spec new() -> State::clState()
%% @doc  Create a new clustering state.
new() ->
    #clState{
        items      = ets:new(cl_items,    []),
        clusters   = ets:new(cl_clusters, []),
        cl_members = ets:new(cl_members,  [bag]),
        etc        = ets:new(cl_etc,      [])}.


%% @spec delete(State::clState()) -> ok
%% @doc  Delete the `State' clustering state.
delete(State) ->
    ets:delete(State#clState.items),
    delete_matrix(State#clState.attr_matrix),
    delete_matrix(State#clState.conn_matrix),
    ets:delete(State#clState.clusters),
    ets:delete(State#clState.cl_members),
    delete_matrix(State#clState.cl_deps),
    delete_matrix(State#clState.cl_dist),
    ets:delete(State#clState.etc).
    
% Delete a matrix.
delete_matrix(undefined) -> ok;
delete_matrix(M)         -> ?Matrix:delete(M).


%%
%%
save(FileName, St=#clState{}) ->
    case file:open(FileName, [write]) of
        {ok, Dev} ->
            io:format(Dev, "~p.\n", [to_record(St)]),
            file:close(Dev);
        {error, Reason} ->
            throw(?LocalError(file_error, file:format_error(Reason)))
    end.    

%%
%%
load(FileName) ->
    case file:open(FileName, [read]) of
        {ok, Dev} ->
            case io:read(Dev, "") of
                {ok, St} -> 
                    St2 = from_record(St),
                    file:close(Dev),
                    St2;
                eof ->
                    throw(?LocalError(file_error, "End of file"));
                {error, Reason} ->
                    throw(?LocalError(file_error, file:format_error(Reason)))
            end;
        {error, Reason} ->
            throw(?LocalError(file_error, file:format_error(Reason)))
    end.    


%%
%%
to_record(St=#clState{}) ->
    St#clState{
        items       = ets:tab2list(St#clState.items),
        attr_matrix = matrix2record(St#clState.attr_matrix),
        conn_matrix = matrix2record(St#clState.conn_matrix),
        clusters    = ets:tab2list(St#clState.clusters),
        cl_members  = ets:tab2list(St#clState.cl_members),
        cl_deps     = matrix2record(St#clState.cl_deps),
        cl_dist     = matrix2record(St#clState.cl_dist),
        etc         = ets:tab2list(St#clState.etc)}.

%%
%%
from_record(St=#clState{}) ->
    S1 = new(),
    ets_list2tab(St#clState.items, S1#clState.items),
    ets_list2tab(St#clState.clusters, S1#clState.clusters),
    ets_list2tab(St#clState.cl_members,S1#clState.cl_members),
    ets_list2tab(St#clState.etc, S1#clState.etc),
    S1#clState{
        attr_matrix = record2matrix(St#clState.attr_matrix),
        conn_matrix = record2matrix(St#clState.conn_matrix),
        cl_deps     = record2matrix(St#clState.cl_deps),
        cl_dist     = record2matrix(St#clState.cl_dist)}.

% Convert matrix to record
matrix2record(undefined) -> undefined;
matrix2record(M)         -> ?Matrix:to_record(M).

% Convert record to matrix
record2matrix(undefined) -> undefined;
record2matrix(M)         -> ?Matrix:from_record(M).




%% @spec ets_list2tab(Objects::[Object], TableETS::tid()) -> ok
%%       Object = tuple()
%% @doc  Load the object from `Objects' into the `TableETS' table.
ets_list2tab(Objects, TableETS) ->
    lists:foreach(fun(Object) -> ets:insert(TableETS, Object) end, Objects).



%%% ============================================================================
%%% Building attribute matrix

%% @spec load_attr_matrix(AttrMatrix::matrix(), BoxFun, State::clState()) 
%%           -> NewSate::clState()
%%       BoxFun = ((ItemId) -> clItem())
%%       ItemId = term()
%% @doc Load the attribute matrix from the given `AttrMatrix' into the `State'
%%      clustering state. Actual attribute matrix and the all items will be 
%%      erased from `State'. The new items will be generated from row and
%%      column headers of `AttrMatrix' using the `BoxFun'.
load_attr_matrix(AttrMatrix, BoxFun, St=#clState{items=ItemsETS}) when 
        is_function(BoxFun) ->
    delete_matrix(St#clState.attr_matrix),
    ets:delete_all_objects(ItemsETS),
    EntIds  = ?Matrix:rows(AttrMatrix),
    AttrIds = ?Matrix:cols(AttrMatrix),
    set_items((AttrIds--EntIds)++EntIds, ItemsETS),
    St#clState{attr_matrix=AttrMatrix}.


%% @spec build_attr_matrix(Items::[clItem()], {DepFun, DefAttrValue::term()}, 
%%               DepFilterFun, OptimazeIds, State::clState()) 
%%                                                     -> NewState::clState()
%%       DepFun = ((Item::clItem()) -> [Dependency])
%%       Dependency = {Item::clItem(), DepItem::clItem(), Weight::term()}
%%       DepFilterFun = ((Dependency) -> bool())
%% @doc Build an attribute matrix. `DepFun' is executed on every item in 
%%      `Items'. The result list is filtred by `DepFilterFun'. The items table 
%%      and the attribute matrix are created from the remain dependencies.
build_attr_matrix(Items, {DepFun,DefAttrValue}, DepFilterFun, OptimazeIds,
        St=#clState{}) when is_list(Items), is_function(DepFun), 
        is_function(DepFilterFun) ->
    % Calculate dependencies
    Deps0 = lists:flatmap(DepFun, Items),
    % Filter dependencies
    Deps1 = lists:filter(DepFilterFun, Deps0),
    % Replace indentifiers of clItems with integers
    Deps = if
        OptimazeIds -> element(2,replace_dep_ids(Deps1, 1));
        true        -> Deps1
    end,
    % Build items table and prepare the attribute matrix building
    DepMatrixList = lists:map(
        fun({Item=#clItem{id=Id1}, DepItem=#clItem{id=Id2}, DepWeight}) ->
            set_items([Item, DepItem], St),
            {{Id1, Id2}, DepWeight}
        end,
        Deps),
    % Built attribute matrix
    AttrMatrix = ?Matrix:from_list(DepMatrixList, DefAttrValue),
    % Update clustering state
    St#clState{attr_matrix=AttrMatrix}.


% Replace indentifiers of clItems with integers
replace_dep_ids(Deps, StartId) when is_list(Deps), is_integer(StartId) ->
    ItemIds = lists:usort(lists:foldl(
        fun({#clItem{id=Id1}, #clItem{id=Id2}, _W}, Acc) -> [Id1,Id2|Acc] end,
        [],
        Deps)),
    {NextId1, ItemIdDict} = lists:foldl(
        fun(Id, {NextId, D}) -> {NextId+1, dict:store(Id, NextId, D)} end,
        {StartId, dict:new()},
        ItemIds),
    {NextId1, lists:map(
        fun({Item=#clItem{id=Id1}, DepItem=#clItem{id=Id2}, DepWeight}) ->
            {ok, Id1B} = dict:find(Id1, ItemIdDict),
            {ok, Id2B} = dict:find(Id2, ItemIdDict),
            {Item#clItem{id=Id1B}, DepItem#clItem{id=Id2B}, DepWeight}
        end,
        Deps)}.



%%% ============================================================================
%%% Manage items

%% @spec get_itemIds(State::clState()) -> Ids::[term()]
%% @doc  Return the identifiers of the all items in the clustering.
get_itemIds(#clState{items=ItemsETS}) ->
    ?MISC:ets_keys(ItemsETS).


%% @spec get_items(Ids::[term()], State::clState()) -> Items::[clItem()]
%% @doc  Convert the `Ids' item identifier list into item list.
get_items(Ids, #clState{items=ItemsETS}) when is_list(Ids) ->
    lists:foldl(
        fun(Id, Acc) -> 
            case ets:lookup(ItemsETS, Id) of
                [{Id,Item}] -> [Item|Acc];
                _           -> Acc
            end
        end,
        [],
        lists:reverse(Ids)).


%% @spec set_items(Items::[clItem()], State::clState()) -> NewState::clState()
%% @doc  Add or replace items.
set_items(Items, St=#clState{items=ItemsETS}) when is_list(Items) ->
    lists:foreach(
        fun(Item=#clItem{id=Id}) -> ets:insert(ItemsETS, {Id,Item}) end,
        Items),
    St.


%% @spec entIds(State::clState()) -> EntIds::[term()]
%% @doc  Return the entity identifiers.
entIds(#clState{attr_matrix=undefined}) -> [];
entIds(#clState{attr_matrix=M}) -> ?Matrix:rows(M).

%% @spec attrIds(State::clState()) -> AttrIds::[term()]
%% @doc  Return the attribute identifiers.
attrIds(#clState{attr_matrix=undefined}) -> [];
attrIds(#clState{attr_matrix=M}) -> ?Matrix:cols(M).

%% @spec entItems(State::clState()) -> EntItems::[clItem()]
%% @doc  Return the entity items.
entItems(St=#clState{items=ItemsETS}) ->
    lists:map(fun(Id) -> [{Id, Item}] = ets:lookup(ItemsETS, Id), Item end,
              entIds(St)).

%% @spec attrItems(State::clState()) -> AttrItems::[clItem()]
%% @doc  Return the attribute items.
attrItems(St=#clState{items=ItemsETS}) ->
    lists:map(fun(Id) -> [{Id, Item}] = ets:lookup(ItemsETS, Id), Item end,
              attrIds(St)).


%% @spec add_ent(EntItem::clItem(), 
%%               AttrValues::[{AttdId::term(), Value::term()}], 
%%               State::clState()) -> NewState::clState()
%% @doc Add a new entity to the clustering and the attribute matrix.
%%      `AttrValues' contains the attribute values of the entity. 
%%      The attributes must be specified by item identifiers.
add_ent(EntItem=#clItem{id=EntId}, AttrValues, 
        St=#clState{items=ItemsETS, attr_matrix=AM}) when is_list(AttrValues) ->
    AM2 = ?Matrix:add_row(EntId, AttrValues, AM),
    set_items([EntItem], ItemsETS),
    St#clState{attr_matrix=AM2}.


%% @spec add_attr(AtrItem::clItem(), 
%%               EntValues::[{EntId::term(), Value::term()}], 
%%               State::clState()) -> NewState::clState()
%% @doc Add a new attribute to the clustering and the attribute matrix.
%%      `EntValues' contains the attribute values for the entities.
%%      The entities must be specified by item identifiers.
add_attr(AttrItem=#clItem{id=AttrId}, EntValues, 
        St=#clState{items=ItemsETS, attr_matrix=AM}) when is_list(EntValues) ->
    AM2 = ?Matrix:add_col(AttrId, EntValues, AM),
    set_items([AttrItem], ItemsETS),
    St#clState{attr_matrix=AM2}.


%% @spec del_ent(EntId::term(), State::clState()) -> NewState::clState()
%% @doc Delete the entity given by `EntId' identifier from the clustering and 
%%      the attribute matrix.
del_ent(EntId, St=#clState{items=ItemsETS, attr_matrix=AM}) ->
    AM2 = ?Matrix:del_row(EntId, AM),
    St2 = St#clState{attr_matrix=AM2},
    case lists:member(EntId, attrIds(St2)) of
        true -> ok;
        _    -> ets:delete(ItemsETS, EntId)
    end,
    St2.


%% @spec del_attr(AttrId::term(), State::clState()) -> NewState::clState()
%% @doc Delete the attribute given by `AttrId' identifier from the clustering 
%%      and the attribute matrix.
del_attr(AttrId, St=#clState{items=ItemsETS, attr_matrix=AM}) ->
    AM2 = ?Matrix:del_col(AttrId, AM),
    St2 = St#clState{attr_matrix=AM2},
    case lists:member(AttrId, entIds(St2)) of
        true -> ok;
        _    -> ets:delete(ItemsETS, AttrId)
    end,
    St2.


%% @spec del_item(Id::term(), State::clState()) -> NewState::clState()
%% @doc  Delete the item specified by `Id' item identifier from the clustering
%%       and the attribute matrix.
del_item(Id, St=#clState{items=ItemsETS, attr_matrix=undefined}) ->
    ets:delete(ItemsETS, Id),
    St;
del_item(Id, St=#clState{}) -> 
    del_attr(Id, del_ent(Id, St)).



%%% ============================================================================
%%% Connection matrix

%%
%%
calc_conn_matrix(ConnFun, DefConn, Symmetric, St=#clState{}) ->
    EntIds = lists:usort(entIds(St)),
    ConnMatrix0 = ?Matrix:new(EntIds, EntIds, DefConn),
    ConnMatrix1 = calc_conn_matrix_(EntIds, ConnFun,Symmetric, St, ConnMatrix0),
    St#clState{conn_matrix=ConnMatrix1, conn_sym=Symmetric}.

% 
calc_conn_matrix_([],_ConnFun,_Symmetric,#clState{},ConnMatrix) -> ConnMatrix;
calc_conn_matrix_([EntId1|EntIds], ConnFun,Symmetric, 
        St=#clState{attr_matrix=AttrMatrix}, ConnMatrix) ->
    [Item1] = get_items([EntId1], St),
    ConnMatrix1 = case ConnFun(Item1, Item1, AttrMatrix) of
        {def,   _ } -> ConnMatrix;
        {value, V1} -> ?Matrix:set(EntId1,EntId1,V1,ConnMatrix)
    end,
    ConnMatrix2 = lists:foldl(
        fun(EntId2, AccMatrix) ->
            [Item2] = get_items([EntId2], St),
            AccMatrix1 = case ConnFun(Item1, Item2, AttrMatrix) of
                {def,   _ } -> AccMatrix;
                {value, V2} -> ?Matrix:set(EntId1,EntId2,V2,AccMatrix)
            end,
            if
                Symmetric -> AccMatrix1;
                true -> 
                    case ConnFun(Item2, Item1, AttrMatrix) of
                        {def,   _ } -> AccMatrix1;
                        {value, V3} -> ?Matrix:set(EntId2,EntId1,V3,AccMatrix1)
                    end
            end
        end,
        ConnMatrix1,
        EntIds),
    calc_conn_matrix_(EntIds, ConnFun,Symmetric, St, ConnMatrix2).


%%
%%
get_conn(EntId1, EntId2, #clState{conn_matrix=ConnMatrix, conn_sym=Sym}) ->
    [Id1,Id2] = if
        Sym  -> lists:usort([EntId1, EntId2]);
        true -> [EntId1, EntId2]
    end,
    ?Matrix:get(Id1,Id2,ConnMatrix).



%%% ============================================================================
%%% Manage clusters

%% @spec get_clusterIds(State::clState()) -> Ids::[term()]
%% @doc  Return the identifiers of the all clusters in the clustering.
get_clusterIds(#clState{clusters=ClustersETS}) ->
    ?MISC:ets_keys(ClustersETS).


%% @spec get_clusters(Ids::[term()], State::clState()) -> 
%%           Clusters::[clCluster()]
%% @doc  Convert the `Ids' cluster identifier list into cluster list.
get_clusters(Ids, #clState{items=ClustersETS}) when is_list(Ids) ->
    lists:foldl(
        fun(Id, Acc) -> 
            case ets:lookup(ClustersETS, Id) of
                [{Id,Cluster}] -> [Cluster|Acc];
                _           -> Acc
            end
        end,
        [],
        lists:reverse(Ids)).


%% @spec set_clusters(Clusters::[clCluster()], State::clState()) -> 
%%           NewState::clState()
%% @doc  Add or replace clusters.
set_clusters(Clusters, St=#clState{clusters=ClETS}) when is_list(Clusters) ->
    lists:foreach(
        fun(Cl=#clCluster{id=Id}) -> ets:insert(ClETS, {Id,Cl}) end,
        Clusters),
    St.


%% @spec get_cluster_ents(ClusterId::term(), State::clState()) ->
%%           EntIds::[term()]
%% @doc  Return the identifiers of entities in the cluster given by `ClusterId'.
get_cluster_ents(ClId, #clState{cl_members=ClMemETS}) ->
    ets:select(ClMemETS, [{ {'$1','$2'}, [{'==','$1',ClId}], ['$2'] }]).


%%
%%
make_cluster(EntIds, Cluster=#clCluster{id=ClId}, 
        St=#clState{clusters=ClETS,cl_members=ClMemETS}) when is_list(EntIds) ->
    ets:insert(ClETS, {ClId, Cluster}),
    lists:foreach(fun(EntId) -> ets:insert(ClMemETS,{ClId,EntId}) end, EntIds),
    St.


%%
%%
move_ents(EntIds, ClFromId, ClToId, St=#clState{clusters=ClETS, 
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
    St.


%%
%%
join_clusters(ClBaseId, ClIds, St=#clState{}) ->
    lists:foreach(
        fun(ClId) -> move_ents(get_cluster_ents(ClId,St),ClId,ClBaseId,St) end,
        ClIds),
    St.


