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

%%% @doc Library to manage the clustering state.
%%% A {@type clState} record contains the entities, attributes, clusters, 
%%% clusterings and the connections of all of these. This module contain usefull
%%% function which make easy to create or modify the state or query informatins
%%% about it.

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_state).
-vsn("$Rev: $").

-include("ucluster.hrl").



%%% ============================================================================
%%% Imports/exports

% Clustering state
-export([new/0, delete/1, pack/1, unpack/1, save/2, load/1]).
% Attribute matrix
-export([load_attr_matrix/3, build_attr_matrix/4]).
% Manage items
-export([itemIds/1, entIds/1, attrIds/1, entItems/1, attrItems/1,
         get_items/2, set_items/2, del_item/2, 
         add_ent/3, add_attr/3, del_ent/2, del_attr/2]).
% Connection matrix
-export([build_conn_matrix/5, create_conn_matrix/5, get_conn/4, set_conn/5]).
% Manage clusters
-export([get_clustering/1, set_clustering/2, 
         backup_clustering/1, restore_clustering/2,
         get_history_size/1, get_history/1, clear_history/1]).



%%% ============================================================================
%%% Clustering state

%% @spec new() -> State::clState()
%% @doc  Create a new empty clustering state.
new() ->
    #clState{
        items         = ets:new(cl_items,    [{keypos,#clItem.id}]),
        ent_conn_sym  = false,
        attr_conn_sym = false,
        cl_hist       = ets:new(cl_hist,     []),
        etc           = ets:new(cl_etc,      [])}.


%% @spec delete(State::clState()) -> ok
%% @doc  Delete the `State' clustering state.
delete(State) ->
    ets:delete(State#clState.items),
    ?ClCommon:matrix_delete(State#clState.attr_matrix),
    ?ClCommon:matrix_delete(State#clState.ent_conn_matrix),
    ?ClCommon:matrix_delete(State#clState.attr_conn_matrix),
    clustering_delete(State#clState.clustering),
    ets:delete(State#clState.cl_hist),
    ets:delete(State#clState.etc).


%% @spec pack(State::clState()) -> PackedState::clState()
%% @doc Create a new `clState' object which contains all information from 
%%      the original `State' as values.
%%      Don't use the returned `PackedState' object as the normal
%%      `clState' objects! In this case a run time error may occur.
%%      If you have a packed `clState' object you need to unpack with the
%%      {@link unpack/1} function before use it.
%% @see unpack/1
pack(St=#clState{}) ->
    St#clState{
        items            = ets:tab2list(St#clState.items),
        attr_matrix      = ?ClCommon:matrix_pack(St#clState.attr_matrix),
        ent_conn_matrix  = ?ClCommon:matrix_pack(St#clState.ent_conn_matrix),
        attr_conn_matrix = ?ClCommon:matrix_pack(St#clState.attr_conn_matrix),
        clustering       = clustering_pack(St#clState.clustering),
        cl_hist          = ets:tab2list(St#clState.cl_hist),
        etc              = ets:tab2list(St#clState.etc)}.

%% @spec unpack(PackedState::clState()) -> State::clState()
%% @doc Create a normal `clState' object from the given packed
%%      `PackedState' objecjt.
%% @see pack/1
unpack(St=#clState{}) ->
    St1 = new(),
    ?MISC:ets_list2tab(St#clState.items,      St1#clState.items),
    ?MISC:ets_list2tab(St#clState.cl_hist,    St1#clState.cl_hist),
    ?MISC:ets_list2tab(St#clState.etc,        St1#clState.etc),
    St1#clState{
        attr_matrix      = ?ClCommon:matrix_unpack(St#clState.attr_matrix),
        ent_conn_matrix  = ?ClCommon:matrix_unpack(St#clState.ent_conn_matrix),
        attr_conn_matrix = ?ClCommon:matrix_unpack(St#clState.attr_conn_matrix),
        clustering       = clustering_unpack(St#clState.clustering)}.


%% @spec save(State::clState(), FilePath::list()) -> ok
%% @doc Pack the `State' object and save that into the `FilePath' file.
save(St=#clState{}, FilePath) ->
    ?ClCommon:save("~p.\n", [pack(St)], FilePath).

%% @spec load(FilePath::list()) -> State::clState()
%% @doc Load a `clState' object from the `FilePath' file and unpack that.
load(FilePath) ->
    unpack(?ClCommon:load_test_rec(FilePath, clState)).


% Delete a clustering
clustering_delete(Cl=#clClustering{}) -> ?ClClustering:delete(Cl);
clustering_delete(?UNDEF)             -> ok.

% Pack a clustering
clustering_pack(Cl=#clClustering{}) -> ?ClClustering:pack(Cl);
clustering_pack(?UNDEF)             -> ?UNDEF.

% Unpack a clustering
clustering_unpack(Cl=#clClustering{}) -> ?ClClustering:unpack(Cl);
clustering_unpack(?UNDEF)             -> ?UNDEF.



%%% ============================================================================
%%% Attribute matrix

%% @spec load_attr_matrix(AttrMatrix::matrix(), BoxFun::BoxFun, 
%%           State::clState()) -> NewSate::clState()
%%       BoxFun = ((ItemId::term()) -> clItem())
%% @doc Load the attribute matrix from the given `AttrMatrix' into the `State'
%%      clustering state. Actual attribute matrix and all items will be 
%%      erased from `State'. The new items will be generated from row and
%%      column headers of `AttrMatrix' using the `BoxFun'.
load_attr_matrix(AttrMatrix, BoxFun, St=#clState{items=ItemsETS}) 
        when is_function(BoxFun,1) ->
    ?ClCommon:delete_matrix(St#clState.attr_matrix),
    ets:delete_all_objects(ItemsETS),
    EntIds  = ?Matrix:rows(AttrMatrix),
    AttrIds = ?Matrix:cols(AttrMatrix),
    Items = lists:foreach(BoxFun, (AttrIds--EntIds)++EntIds),
    set_items(Items, ItemsETS),
    St#clState{attr_matrix=AttrMatrix}.


%% @spec build_attr_matrix(EntFun::EntFun, {DepFun::DepFun, 
%%               DefAttrValue::term()}, DepFilterFun, State::clState()) ->
%%           NewState::clState()
%%       EntFun = (() -> [clItem()])
%%       DepFun = ((Item::clItem()) -> [Dependency])
%%       Dependency = {Item::clItem(), DepItem::clItem(), Weight::term()}
%%       DepFilterFun = ((Dependency) -> bool())
%% @doc Build an attribute matrix. First, the `EntFun' generate the entities and
%%      the `DepFun' is executed on every generated entity. Next, the generated
%%      dependencies are filtered by the `DepFilter' function. It it return
%%      `true' the dependency is kept otherwise dropped. Finally build the 
%%      attribute matrix from the remained dependecies. The rows and the colums
%%      of the attribute matrix will be labeled with the item identifiers.
%%      All items will be stored in the `items' field of the `NewState'.
build_attr_matrix(EntFun, {DepFun,DefAttrValue}, DepFilterFun, St=#clState{}) 
        when is_function(EntFun,0), is_function(DepFun,1), 
        is_function(DepFilterFun,1) ->
    % Generate entities, calculate and filter dependencies
    Deps = lists:filter(DepFilterFun, lists:flatmap(DepFun, EntFun())),
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



%%% ============================================================================
%%% Manage items

%% @spec itemIds(State::clState()) -> Ids::[term()]
%% @doc  Return the identifiers of all items in the clustering.
itemIds(#clState{items=ItemsETS}) -> ?MISC:ets_keys(ItemsETS).

%% @spec entIds(State::clState()) -> EntIds::[term()]
%% @doc  Return the entity identifiers.
entIds(#clState{attr_matrix=?UNDEF}) -> [];
entIds(#clState{attr_matrix=M})      -> ?Matrix:rows(M).

%% @spec attrIds(State::clState()) -> AttrIds::[term()]
%% @doc  Return the attribute identifiers.
attrIds(#clState{attr_matrix=?UNDEF}) -> [];
attrIds(#clState{attr_matrix=M})      -> ?Matrix:cols(M).

%% @spec entItems(State::clState()) -> EntItems::[clItem()]
%% @doc  Return the entity items.
entItems(St=#clState{}) ->
    get_items(entIds(St), St).

%% @spec attrItems(State::clState()) -> AttrItems::[clItem()]
%% @doc  Return the attribute items.
attrItems(St=#clState{}) ->
    get_items(attrIds(St), St).


%% @spec get_items(Ids::[term()], State::clState()) -> Items::[clItem()]
%% @doc  Convert the `Ids' item identifier list into item list.
get_items(Ids, #clState{items=ItemsETS}) when is_list(Ids) ->
    lists:foldl(
        fun(Id, Acc) -> 
            case ets:lookup(ItemsETS, Id) of
                [Item=#clItem{}] -> [Item|Acc];
                _                -> Acc
            end
        end,
        [],
        lists:reverse(Ids)).

%% @spec set_items(Items::[clItem()], State::clState()) -> NewState::clState()
%% @doc  Add or replace items.
set_items(Items, St=#clState{items=ItemsETS}) when is_list(Items) ->
    lists:foreach(
        fun(Item=#clItem{}) -> ets:insert(ItemsETS, Item) end,
        Items),
    St.


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
%% @doc Delete the entity given by `EntId' identifier from the attribute matrix.
%%      If the `EntId' is not an attribute too it will be eraset from the
%%      `items' set too.
del_ent(EntId, St=#clState{items=ItemsETS, attr_matrix=AM}) ->
    AM2 = ?Matrix:del_row(EntId, AM),
    St2 = St#clState{attr_matrix=AM2},
    case lists:member(EntId, attrIds(St2)) of
        true -> ok;
        _    -> ets:delete(ItemsETS, EntId)
    end,
    St2.


%% @spec del_attr(AttrId::term(), State::clState()) -> NewState::clState()
%% @doc Delete the attribute given by `AttrId' identifier from the attribute
%%      matrix. If the `AttrId' is not an entity too it will be eraset from the
%%      `items' set too.
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
del_item(Id, St=#clState{items=ItemsETS, attr_matrix=?UNDEF}) ->
    ets:delete(ItemsETS, Id),
    St;
del_item(Id, St=#clState{}) -> 
    del_attr(Id, del_ent(Id, St)).



%%% ============================================================================
%%% Connection matrix

%% @spec build_conn_matrix(Matrix::MatrixSelector, ConnFun::ConnFun,
%%               DefConn::term(), Symmetric::boolean(), State::clState()) ->
%%           NewState::clState()
%%       MatrixSelector = entity | attribute
%%       ConnFun = ((Item1::clItem(), Item2::clItem(), AttrMatrix::matrix()) ->
%%                      ConnResult)
%%       ConnResult = {def, any()} | {val, ConnVal::term()}
%% @doc Create the entity/attribute connection matrix. `Matrix' determine which
%%      matrix must be calculated. If `Symmetric' is true the connection of
%%      an item with an another item is same as the connection in reverse way.
%%      For every pair of entities/attributes the connection is calculated by
%%      the `ConnFun'. If the result of this function is a `{def,_}' tuple
%%      the connection value will be the `DefConn' value. Othervise the
%%      `ConnValue' will be used.
build_conn_matrix(Matrix, ConnFun, DefConn, Symmetric, St=#clState{}) when 
        (entity==Matrix orelse attribute==Matrix), is_function(ConnFun,3), 
        is_boolean(Symmetric) ->
    case Matrix of
        entity ->
            ConnMatrix = 
                create_conn_matrix(entIds(St), ConnFun,DefConn, Symmetric, St),
            St#clState{ent_conn_matrix=ConnMatrix, ent_conn_sym=Symmetric};
        attribute ->
            ConnMatrix = 
                create_conn_matrix(attrIds(St), ConnFun,DefConn, Symmetric, St),
            St#clState{attr_conn_matrix=ConnMatrix, attr_conn_sym=Symmetric}
    end.


%% @spec create_conn_matrix(ItemIds::[term()], ConnFun::ConnFun,
%%               DefConn::term(), Symmetric::boolean(), State::clState()) ->
%%           ConnMatrix::matrix()
%%       ConnFun = ((Item1::clItem(), Item2::clItem(), AttrMatrix::matrix()) ->
%%                      ConnResult)
%%       ConnResult = {def, any()} | {val, ConnVal::term()}
%% @doc Create the connection matrix of items given in `ItemIds'. All elem
%%      in `ItemIds' list must identify an exist item in the `State'.
%%      If `Symmetric' is true the connection of an item with an another item 
%%      is same as the connection in reverse way. In this case the returned
%%      matrix will be an upper triangle matrix.
%%      For every pair of items the connection is calculated by the `ConnFun'.
%%      If the result of this function is a `{def,_}' tuple the connection value
%%      will be the `DefConn' value. Othervise the `ConnValue' will be used.
create_conn_matrix(EntIds, ConnFun, DefConn, Symmetric, St=#clState{}) when 
        is_function(ConnFun,3), is_boolean(Symmetric) ->
    EntIds1 = lists:sort(EntIds),
    create_conn_matrix_(EntIds1, ConnFun,Symmetric, St, 
                        ?Matrix:new(EntIds1, EntIds1, DefConn)).

% Implementation of create_conn_matrix/5
create_conn_matrix_([],_ConnFun,_Symmetric,#clState{},ConnMatrix) -> ConnMatrix;
create_conn_matrix_([EntId1|EntIds], ConnFun,Symmetric, 
        St=#clState{attr_matrix=AttrMatrix}, ConnMatrix) ->
    [Item1] = get_items([EntId1], St),
    % Calc self connection
    ConnMatrix1 = case ConnFun(Item1, Item1, AttrMatrix) of
        {def,   _ } -> ConnMatrix;
        {value, V1} -> ?Matrix:set(EntId1,EntId1,V1,ConnMatrix)
    end,
    % Calc connections with other items
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
    create_conn_matrix_(EntIds, ConnFun,Symmetric, St, ConnMatrix2).


%% @spec get_conn(Matrix, ItemId1::term(), ItemId2::term(),
%%               State::clState()) -> NewState::clState()
%%       MatrixSelector = entity | attribute
%% @doc Return the connection value between `ItemId1' and `ItemId2'
%%      entities/attributes. `Matrix' determine which matrix must be used
%%      to read the connection value.
get_conn(Matrix, EntId1, EntId2, St=#clState{}) ->
    [Id1,Id2] = if
        (entity==Matrix andalso St#clState.ent_conn_sym) orelse 
        (attribute==Matrix andalso St#clState.attr_conn_sym) ->
            lists:sort([EntId1, EntId2]);
        true -> [EntId1, EntId2]
    end,
    case Matrix of
        entity    -> ?Matrix:get(Id1,Id2,St#clState.ent_conn_matrix);
        attribute -> ?Matrix:get(Id1,Id2,St#clState.attr_conn_matrix)
    end.

%% @spec set_conn(Matrix, ItemId1::term(), ItemId2::term(), ConnValue::term(),
%%               State::clState()) -> NewState::clState()
%%       MatrixSelector = entity | attribute
%% @doc Set the connection value between `ItemId1' and `ItemId2'
%%      entities/attributes. `Matrix' determine which matrix must be used
%%      to write the connection value.
set_conn(Matrix, EntId1, EntId2, Value, St=#clState{}) ->
    [Id1,Id2] = if
        (entity==Matrix andalso St#clState.ent_conn_sym) orelse 
        (attribute==Matrix andalso St#clState.attr_conn_sym) ->
            lists:sort([EntId1, EntId2]);
        true -> [EntId1, EntId2]
    end,
    case Matrix of
        entity -> 
            ECM = ?Matrix:set(Id1,Id2,Value,St#clState.ent_conn_matrix),
            St#clState{ent_conn_matrix=ECM};
        attribute -> 
            ACM = ?Matrix:set(Id1,Id2,Value,St#clState.attr_conn_matrix),
            St#clState{attr_conn_matrix=ACM}
    end.

    


%%% ============================================================================
%%% Manage clusterings

%% @spec get_clustering(State::clState()) ->
%%           Clustering::clClustering() | undefined
%% @doc Return the actual {@type clClustering} record.
get_clustering(#clState{clustering=Clustering}) ->
    Clustering.

%% @spec set_clustering(Clustering::Clustering, State::clState()) ->
%%           NewState::clState()
%%       Clustering = clClustering() | undefined
%% @doc Set or erase the actual clustering.
set_clustering(Cling, St=#clState{}) when ?UNDEF==Cling; 
        is_record(Cling,clClustering) ->
    St#clState{clustering=Cling}.


%% @spec backup_clustering(State::clState()) ->
%%           {NewState::clState(), BackupId::natural()}
%% @doc Backup the actual clustering into the `cl_hist' history (stack).
%%      The backup is identified by a natural number start with zero.
%%      This number can be used to restore the clustering.
%% @see restore_clustering/2
backup_clustering(St=#clState{clustering=Cling, cl_hist=ClHistETS}) ->
    BackupId = ets:info(ClHistETS, size),
    ets:insert(ClHistETS, {BackupId,?ClClustering:pack(Cling)}),
    {St, BackupId}.

%% @spec restore_clustering(BackupId::natural(), State::clState()) ->
%%           NewState::clState()
%% @doc Restore the backup identified by `BackupId' and erase this and all
%%      backup after this.
restore_clustering(BackupId, St=#clState{cl_hist=ClHistETS}) when 
        is_integer(BackupId), 0=<BackupId ->
    case ets:lookup(ClHistETS, BackupId) of
        [{BackupId,Cling}] ->
            % Delete this backup and all others after this
            lists:forach(
                fun(HistId) -> ets:delete(ClHistETS, HistId) end,
                ets:select(ClHistETS,
                           [{ {'$1','_'}, [{'=<','$1',BackupId}], ['$1'] }])),
            % Restore backup
            Cling1 = ?ClClustering:unpack(Cling),
            St#clState{clustering=Cling1};
        [] -> St
    end.


%% @spec get_history_size(State::clState()) -> HistorySize::natural()
%% @doc Return the history size. The backups are identified a natural number
%%      start with zero. It means if the history size is 10 than there are 10
%%      backup in the history and those are identified with 0,1,2,..,9.
%% @see backup_clustering/1
get_history_size(#clState{cl_hist=ClHistETS}) ->
    ets:info(ClHistETS, size).

%% get_history(State::clState()) -> PackedClusterings::[clClustering()]
%% @doc Return the history in packed format. The first clustering was backuped
%%      first.
%% @see backup_clustering/1
%% @see ucl_clustering:pack/1
get_history(#clState{cl_hist=ClHistETS}) ->
    % Get packed clusterings (Id, Cling), sort by HistId and drop the id
    element(2,lists:unzip(lists:sort(ets:tab2list(ClHistETS)))).

%% @spec clear_history(State::clState()) -> NewState::clState()
%% @doc Clear the content of the history.
clear_history(St=#clState{cl_hist=ClHistETS}) ->
    ets:delete_all_objects(ClHistETS),
    St.


