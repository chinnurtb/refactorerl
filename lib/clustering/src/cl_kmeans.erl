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

%%% @doc Implements k-means algorithm. 
%%%
%%% @author Petra Krizsai <krizsai@inf.elte.hu>

-module(cl_kmeans).

-vsn("$Rev: 2814 $").

-include("cluster.hrl").

-import(proplists, [get_value/2]).

%%% Interface function
-export([run_cluster/2]).

%%% Functions that are used by other modules, but are not interface functions
-export([random_elements/2, run_cluster_default/0, transform_ets/1, 
         compare_clusters/3]).

%%% @type clusters() = ets({entity(), centroid()}).
%%%
%%% Represents a clustering. Each cluster is represented by a centroid, which is
%%% assigned to the entities in that cluster.

%%% @type stop_rec() = #stop_rec{clusterlist=[clusters()],
%%%                              entities=[entity()]}.
%%%
%%% Represents parameters for stopping functions.
%%%
%%% Fields:
%%% <ul>
%%%     <li>`clusterlist': List of clusterings.</li>
%%%     <li>`entities': Entities of the clusterings.</li>
%%% </ul>
-record(stop_rec, {clusterlist, entities}).

%% @spec run_cluster(proplist(), matrix()) ->
%%                  {[dict(centroid(), [entity()])], matrix()} | [[[entity()]]]
%%
%% @doc Executes the K-means algorithm on the modules to be clustered. Returns
%% the result according to the format. If the selected format is `dict', the
%% attribs matrix is returned as well.
%%
%% Options:
%% <ul>
%%     <li>`k::positive()': The number of the clusters.</li>
%%     <li>`distfun::dist_fun()': The distance function.</li>
%%     <li>`mergefun::merge_fun()': The merge function.</li>
%%     <li>`entitylist::[entity()]': Entities to be clustered.</li>
%%     <li>`initcentroids::([centroid()] | undefined)': Initial centroids.</li>
%%     <li>`stoppingcriteria::({unchanged, MaxIteration::integer()} |
%%                             {iterations, MaxIteration::integer()})':
%%         It defines when to stop the algorithm. `unchanged' property means
%%         the algorithm runs until the clusterings are unchanged.
%%         `MaxIteration' specifies the maximum number of iterations.
%%         If it is `-1', there will be no limit for number of iterations.
%%         `MaxIteration' in `iterations' defines how many iterations to be
%%         performed.</li>
%%     <li>`format::(list | dict)': Defines the format of the result.</li>
%% </ul>
run_cluster(Options, Attribs) ->

    %% Reading the options
    Opts = cl_utils:proplist_update(run_cluster_default(), Options),
    InitCentroids = get_value(initcentroids, Opts),
    MergeFun = get_value(mergefun, Opts),
    DistFun = get_value(distfun, Opts),
    Entities = get_value(entitylist, Opts),
    StoppingCriteria = get_value(stoppingcriteria, Opts),
    Format = get_value(format, Opts),

    %% Calculating the initial centroids
    Centroids =
        case InitCentroids of
            undefined ->
                random_elements(Entities, get_value(k, Opts));
            _ ->
                InitCentroids
        end,

    %% Running the clustering algorithm
    {ClusterList, _, NewAttribs} =
        case StoppingCriteria of
             {iterations, Number} ->
                 do(Centroids, Attribs, DistFun, MergeFun,
                    fun stopping_fun_iterations/2, Number, Entities, []);
             {unchanged, MaxIterations} ->
                 do(Centroids, Attribs, DistFun, MergeFun,
                    fun stopping_fun_unchanged/2, MaxIterations, Entities, [])
        end,

    %% Destroying and converting Clusterings
    Clusterings =
      lists:foldl(
        fun(Ets, Clusterings) ->
           Dict = transform_ets(Ets),
           Clusters =
               case Format of
                   dict ->
                       Dict;
                   list ->
                       [Cluster || {_, Cluster} <- dict:to_list(Dict)]
               end,
           ets:delete(Ets),
           [Clusters | Clusterings]
        end, [], ClusterList),

    %% Returning the result and destroying NewAttribs if not returned
    case Format of
        dict ->
            {Clusterings, NewAttribs};
        list ->
            Clusterings
    end.

%% @spec run_cluster_default() -> proplist()
%%
%% @doc Returns the default options of {@link run_cluster/2}.
run_cluster_default() ->
    [{initcentroids, undefined},
     {stoppingcriteria, {unchanged, 10}},
     {format, list},
     {k, 3}].

stopping_fun_iterations(Number, _StopRec) ->
    {Number == 0, Number-1}.

%% @spec stopping_fun_unchanged(integer(), stop_rec()) -> {bool(), integer()}
%%
%% @doc Represents the unchanged stopping criteria, which means that the
%% algorithm runs until the criteria is true.
stopping_fun_unchanged(0, _StopRec) ->
    {true, 0};
stopping_fun_unchanged(MaxNumber,
                       #stop_rec{clusterlist=[NewClusters, PrevClusters | _],
                                 entities=Entities}) ->
    {compare_clusters(PrevClusters, NewClusters, Entities), MaxNumber-1};
stopping_fun_unchanged(MaxNumber, _StopRec)->
    {false, MaxNumber}.

%% @spec do([centroid()], matrix(), dist_fun(), merge_fun(), stopping_fun(),
%%          term(), [entity()], [clusters()]) -> {[clusters()], term(), matrix()}
%%
%% @doc Executes the core of the K-means algorithm according to the
%% `StoppingFun'.
do(Centroids, Attribs, DistFun, MergeFun, StoppingFun, Acc, Entities,
   ClusterList) ->

    Clusters = calc_clusters(Centroids, Attribs, DistFun, Entities),
    ClusterList2 = [Clusters | ClusterList],
    StopRec = #stop_rec{clusterlist=ClusterList2, entities=Entities},
    {Stop, Acc2} = StoppingFun(Acc, StopRec),
    case Stop of
        true ->
            {ClusterList2, Acc2, Attribs};
        false ->
            {NewCentroids, NewAttribs} =
                calc_centroids(Attribs, transform_ets(Clusters), MergeFun),
            do(NewCentroids, NewAttribs, DistFun, MergeFun, StoppingFun, Acc2,
               Entities, ClusterList2)
    end.

%% @spec compare_clusters(clusters(), clusters(), [entity()]) -> bool()
%%
%% @doc Returns whether two clusterings are equal.
compare_clusters(PrevClusters, NewClusters, Entities) ->
    Assignments = ets:new(assignments, []),
    Result =
        lists:foldl(
          fun (Entity, true) ->
                  %% The accumulator is true when the centroids match so far.
                  [{_, Centroid1}] = ets:lookup(PrevClusters, Entity),
                  [{_, Centroid2}] = ets:lookup(NewClusters, Entity),
                  case ets:lookup(Assignments, Centroid1) of
                      [] ->
                          ets:insert(Assignments, {Centroid1, Centroid2});
                      [{_, Something}] ->
                          Something == Centroid2
                  end;
              (_Entity, false) ->
                  %% If we found a bad match, the ets tables shouldn't be
                  %% examined any longer.
                  false
          end, true, Entities),
    ets:delete(Assignments),
    Result.

%% @spec random_elements([element()], integer()) -> [element()]
%%
%% @doc Returns `K' different elements of an `L' list. `K' is less or equal than
%% the length of `L'.
random_elements(L, K) ->
    {LastIndex, Table} = index_table(L),
    Elements = random_elements_core(LastIndex, K, Table),
    ets:delete(Table),
    Elements.

%% @spec index_table([element()]) -> {integer(), ets({integer(), element()})}
%%
%% @doc Creates an index table of the given list. If `E' is the `N'th element of
%% `L', then `{N, E}' will be an element of the index table. The function
%% returns the length of the list and the index table.
index_table(L) ->
    Table = ets:new(indextable, []),
    LastIndex = index_table(1, Table, L),
    {LastIndex, Table}.

index_table(I, _Table, []) ->
    I-1;
index_table(I, Table, [H | T]) ->
    ets:insert(Table, {I, H}),
    index_table(I+1, Table, T).

%% @spec random_elements_core(integer(), integer(), ets({integer(), element()}))
%%                           -> [element()]
%%
%% @doc It is the core of the {@link random_elements/2} function. It selects `K'
%% random elements using the given index table. The table should be indexed with
%% integers ranging from `1' to `LastIndex'.
random_elements_core(LastIndex, K, Table) ->
    random_elements_core(LastIndex, K, [], Table).

%% @spec random_elements_core(integer(), integer(), [element()], ets({integer(),
%%                            element()})) -> [element()]
%%
%% @doc It selects `K' random elements from the table, all of which are indexed
%% with integers between `1' and `LastIndex'. The random elements will be
%% appended to the front of `Acc' and returned.
random_elements_core(_LastIndex, 0, Acc, _Table) ->
    Acc;
random_elements_core(LastIndex, K, Acc, Table) ->
    RandomKey = random:uniform(LastIndex),
    [{RandomKey, RandomElement}] = ets:lookup(Table, RandomKey),
    [{LastIndex, LastElement}] = ets:lookup(Table, LastIndex),

    %% It assigns the last element to the randomly selected key, so that the key
    %% will not be selected later.
    ets:insert(Table, {RandomKey, LastElement}),

    Acc2 = [RandomElement | Acc],
    random_elements_core(LastIndex-1, K-1, Acc2, Table).

%% @spec calc_min_dist(entity(), [centroid()], matrix(), dist_fun()) ->
%%                     {centroid(), number()}
%%
%% @doc Calculates the closest centroid to the given entity.
calc_min_dist(Entity, Centroids, Attribs, DistFun) ->
    [HC | TC] = Centroids,
    lists:foldl(fun (Centroid, {Min, MinDist}) ->
                Dist = DistFun(Entity,
                               cl_matrix:get_row(Entity, Attribs),
                               Centroid,
                               cl_matrix:get_row(Centroid, Attribs)),
                    case Dist < MinDist of
                        true -> {Centroid, Dist};
                        false -> {Min, MinDist}
                    end
                end,
                {HC, DistFun(Entity, cl_matrix:get_row(Entity, Attribs),
                             HC, cl_matrix:get_row(HC, Attribs))},
                TC).


%% @spec calc_clusters([centroid()], matrix(), dist_fun(), [entity()]) ->
%%                     clusters()
%%
%% @doc Assigns the closest centroid to each entity, and returns the ets table
%% containing the assignments.
calc_clusters(Centroids, Attribs, DistFun, Entities) ->
    Clusters = ets:new(clusters, []),
    lists:foreach(fun (Entity) ->
                    {MinCentr, _} =
                        calc_min_dist(Entity, Centroids, Attribs, DistFun),
                    ets:insert(Clusters, {Entity, MinCentr})
                  end,
                  Entities),
    Clusters.

%% @spec transform_ets(clusters()) -> dict(centroid(), [entity()])
%%
%% @doc Transforms the ets table of the given clusters to a dict, where
%% lists of entities are assinged to the centroids.
transform_ets(Clusters) ->
    ets:foldl(fun ({E, C}, Dict) ->
                case dict:find(C, Dict) of
                    error ->
                        dict:store(C, [E], Dict);
                    {ok, Entities} ->
                        dict:store(C, [E|Entities], Dict)
                end
              end, dict:new(), Clusters).

%% @spec calc_new_centroid([entity()], matrix(), merge_fun()) ->
%%                         [{attr(), value()}]
%%
%% @doc A new centroid is calculated from the given entities.
calc_new_centroid(Entities, Attribs, MergeFun) ->
    MergeFun(Entities,
           lists:foldl(
            fun (Entity, Rows) ->
                [cl_matrix:get_row(Entity, Attribs) | Rows]
            end, [], Entities),
           []).

%% @spec calc_centroids(matrix(), dict(centroid(), [entity()]),
%%                      merge_fun()) -> {[centroid()], matrix()}
%%
%% @doc Returns a list with the new centroids and the updated attribute matrix.
calc_centroids(Attribs, Clusters, MergeFun) ->
    dict:fold(fun (_OldCentroid, Entities, {Centroids, Attribs2}) ->
                  Row = calc_new_centroid(Entities, Attribs2, MergeFun),
                  %%TODO do it more efficiently
                  Label = length(cl_matrix:rows(Attribs2)) + 1,
                  {[Label|Centroids],
                   cl_matrix:insert_new_row(Label, Row, Attribs2)}
              end, {[], Attribs}, Clusters).
