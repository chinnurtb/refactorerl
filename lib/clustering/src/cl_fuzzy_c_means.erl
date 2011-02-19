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

%%% @doc Implements fuzzy c-means algorithm.
%%%
%%% @author Petra Krizsai <krizsai@inf.elte.hu>

-module(cl_fuzzy_c_means).

-vsn("$Rev: 3185 $").

-include("cluster.hrl").

-import(proplists, [get_value/2]).
-export([run_cluster/2, run_cluster_default/0]).

%%% @type clusters() = ets({entity(), centroid()}).
%%%
%%% Represents a non-fuzzy clustering. Each cluster is represented by a
%%% centroid, which is assigned to the entities in that cluster.

%%% @type coeffmatrix() = matrix().
%%%
%%% Represents a coefficient matrix. Each row represents an entity, and each
%%% column represents a centroid. The values of cells are the degrees of
%%% belonging between the corresponding entity and centroid.

%%% @type stop_rec() = #stop_rec{clusterlist=[clusters()],
%%%                              coeffmatrixlist=[coeffmatrix()],
%%%                              entities=[entity()]}.
%%%
%%% Represents parameters for stopping functions.
%%%
%%% Fields:
%%% <ul>
%%%     <li>`clusterlist': List of non-fuzzy clusterings.</li>
%%%     <li>`coeffmatrixlist': List of coefficient matrices.</li>
%%%     <li>`entities': Entities of the clusterings.</li>
%%% </ul>
-record(stop_rec, {clusterlist, coeffmatrixlist, entities}).

%% @spec run_cluster(proplist(), matrix()) ->
%%                  [[[entity()]]] |
%%                  {[dict(centroid(), [entity()])], matrix()} |
%%                  {[coeffmatrix()], matrix()}
%%
%%
%% @doc Executes the fuzzy c-means algorithm on the modules to be clustered.
%% Returns the result according to the format. If the selected format is
%% `dict', the attribs matrix is returned as well. It it is `matrix', the
%% coefficient matrix is also returned.
%%
%% Options:
%% <ul>
%%     <li>`k::positive()': The number of the clusters.</li>
%%     <li>`distfun::dist_fun()': The distance function.</li>
%%     <li>`m::float()': The `M' parameter of the fuzzy c-means algorithm.
%%         Should be larger than 1 and smaller than or equal to 2. The
%%         default value is 2.</li>
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
%%     <li>`format::(list | dict | matrix)': Defines the format of the
%%         result.</li>
%% </ul>
run_cluster(Options, Attribs) ->

    %% Reading the options
    Opts = cl_utils:proplist_update(run_cluster_default(), Options),
    InitCentroids = get_value(initcentroids, Opts),
    DistFun = get_value(distfun, Opts),
    Entities = get_value(entitylist, Opts),
    StoppingCriteria = get_value(stoppingcriteria, Opts),
    M = get_value(m, Opts),
    Format = get_value(format, Opts),

    %% Calculating the initial centroids
    Centroids =
        case InitCentroids of
            undefined ->
                cl_kmeans:random_elements(Entities, get_value(k, Opts));
            _ ->
                InitCentroids
        end,

    %% Running the clustering algorithm
    {CoeffMatrixList, ClusterList, _, NewAttribs} =
        case StoppingCriteria of
             {iterations, Number} ->
                 do(Centroids, Attribs, DistFun, M,
                    fun stopping_fun_iterations/2, Number, Entities, [], []);
             {unchanged, MaxIterations} ->
                 do(Centroids, Attribs, DistFun, M,
                    fun stopping_fun_unchanged/2, MaxIterations, Entities,[],[])
        end,

    %% Destroying and converting Clusterings
    Clusterings =
      lists:foldl(
        fun(Ets, Clusterings) ->
           Dict = cl_kmeans:transform_ets(Ets),
           Clusters =
               case Format of
                   dict ->
                       Dict;
                   list ->
                       [Cluster || {_, Cluster} <- dict:to_list(Dict)];
                   matrix ->
                       ok % We won't return the resulted list anyway
               end,
           ets:delete(Ets),
           [Clusters | Clusterings]
        end, [], ClusterList),

    %% Destroying CoeffMatrix if it is not returned
    case lists:member(Format, [dict, list]) of
        true ->
            lists:foreach(
              fun(CoeffMatrix) ->
                      cl_matrix:delete(CoeffMatrix)
              end,
              CoeffMatrixList);
        false ->
            ok
    end,

    %% Returning the result and destroying NewAttribs if not returned
    case Format of
        dict ->
            {Clusterings, NewAttribs};
        list ->
            Clusterings;
        matrix ->
            {CoeffMatrixList, NewAttribs}
    end.

%% @spec run_cluster_default() -> proplist()
%%
%% @doc Returns the default options of {@link run_cluster/2}.
run_cluster_default() ->
    [{initcentroids, undefined},
     {stoppingcriteria, {unchanged, 10}},
     {format, list},
     {k, 3},
     {m, 2}].

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
    io:format("maxnumber: ~p~n", [MaxNumber]),
    {cl_kmeans:compare_clusters(PrevClusters, NewClusters, Entities),
     MaxNumber-1};
stopping_fun_unchanged(MaxNumber, _StopRec)->
    {false, MaxNumber}.

%% @spec do([centroid()], matrix(), dist_fun(), float(), stopping_fun(),
%%          term(), [entity()], [coeffmatrix()], [clusters()]) ->
%%         {[clusters()], [coeffmatrix()], term(), matrix()}
%%
%% @doc Executes the core of the fuzzy c-means algorithm according to the
%% `StoppingFun'.
do(Centroids, Attribs, DistFun, M, StoppingFun, Acc, Entities,
   CoeffMatrixList, ClusterList) ->

    CoeffMatrix = calc_coeff_matrix(Centroids, Entities, DistFun, Attribs, M),
    Clusters = convert_to_nonfuzzy(Centroids, CoeffMatrix),
    CoeffMatrixList2 = [CoeffMatrix | CoeffMatrixList],
    ClusterList2 = [Clusters | ClusterList],
    StopRec = #stop_rec{clusterlist=ClusterList2,
                        entities=Entities,
                        coeffmatrixlist=CoeffMatrixList2},
    {Stop, Acc2} = StoppingFun(Acc, StopRec),
    case Stop of
        true ->
            {CoeffMatrixList2, ClusterList2, Acc2, Attribs};
        false ->
            {NewCentroids, NewAttribs} =
                calc_centroids(Attribs, CoeffMatrix, Entities, Centroids, M),
            do(NewCentroids, NewAttribs, DistFun, M, StoppingFun, Acc2,
               Entities, CoeffMatrixList2, ClusterList2)
    end.

%% @spec convert_to_nonfuzzy([centroid()], coeffmatrix()) -> clusters()
%%
%% @doc Converts a fuzzy clustering to non-fuzzy clustering. The former is
%% represented by `CoeffMatrix'.
convert_to_nonfuzzy(Centroids, CoeffMatrix) ->
    Entities = cl_matrix:rows(CoeffMatrix),
    Clustering = ets:new(clustering, []),
    lists:foreach(
      fun(Entity) ->
              {_, MaxCentr} =
                  lists:foldl(
                    fun(Centroid, {MaxVal, MaxCentroid}) ->
                        Val = cl_matrix:get(Entity, Centroid, CoeffMatrix),
                        lists:max([{Val, Centroid}, {MaxVal, MaxCentroid}])
                    end,
                    {0, hd(Centroids)},
                    Centroids),
              ets:insert(Clustering, {Entity, MaxCentr})
      end,
      Entities),
    Clustering.

%% @spec calc_dist_ets(matrix(), dist_fun(), [centroid()], [entity()]) ->
%%                     ets({{centroid(), entity()}, number()})
%%
%% @doc Calculates a distance table.
calc_dist_ets(Attribs, DistFun, Centroids, Entities) ->
    Dist = ets:new(dist, []),
    lists:foreach(
      fun(Entity) ->
              lists:foreach(
                fun(Centroid) ->
                        Distance =
                            DistFun(Entity,
                                    cl_matrix:get_row(Entity, Attribs),
                                    Centroid,
                                    cl_matrix:get_row(Centroid, Attribs)),
                        ets:insert(Dist, {{Centroid, Entity}, Distance})
                end,
                Centroids)
      end,
      Entities),
    Dist.

%% @spec calc_coeff_matrix([cetroid()],[entity()],dist_fun,matrix(),matrix()) ->
%%                        matrix()
%%
%% @doc Returns a coefficient matrix that describes that relation between the
%% given centroids and entities.
%%
%% The following value will be the coefficient between `Entity' and `Centroid':
%%
%% ```
%%  (sum_j ( d(Centroid, Entity) / d(Centroid_j, Entity)) ^ (2/(M-1))) ^ (-1)
%% '''
calc_coeff_matrix(Centroids, Entities, DistFun, Attribs, M) ->
    CoeffMatrix1 = cl_matrix:new(Entities, Centroids, 0),
    DistanceTable = calc_dist_ets(Attribs, DistFun, Centroids, Entities),
    CoeffMatrix4 =
        lists:foldl(
          fun(Entity, CoeffMatrix2) ->
                  lists:foldl(
                    fun(Centroid, CoeffMatrix3) ->
                            insert_coeff(Centroid,
                                         Centroids,
                                         Entity,
                                         CoeffMatrix3,
                                         DistanceTable,
                                         M)
                      end,
                      CoeffMatrix2,
                      Centroids)
          end,
          CoeffMatrix1,
          Entities),
    ets:delete(DistanceTable),
    CoeffMatrix4.

%% @spec insert_coeff(centroid(), [centroid()], entity(), coeffmatrix(),
%%                    ets({centroid(), entity()}), float()) -> coeffmatrix()
%%
%% @doc Sets the coefficient between `Centroid' and `Entity' in `CoeffMatrix'.
insert_coeff(Centroid, Centroids, Entity, CoeffMatrix, DistanceTable, M) ->
    [{_, Distance}] = ets:lookup(DistanceTable, {Centroid, Entity}),
    Exponent = 2 / (M-1),
    CoefficientInverse =
        case Distance == 0 of
            true ->
                1;
            false ->
                lists:foldl(
                  fun(_Centroid2, null) ->
                          null;
                     (Centroid2, Acc) ->
                          [{_, Distance2}] =
                            ets:lookup(DistanceTable, {Centroid2, Entity}),
                          case Distance2 == 0 of
                              true ->
                                  null;
                              _ ->
                                  Acc + math:pow(Distance / Distance2, Exponent)
                          end
                  end,
                  0,
                  Centroids)
        end,
    Coefficient =
        case CoefficientInverse == null of
            true ->
                0;
            _ ->
                1 / CoefficientInverse
        end,
    cl_matrix:set(Entity, Centroid, Coefficient, CoeffMatrix).

%% @spec calc_centroids(matrix(), coeffmatrix(), [entity()], [centroid()],
%% float()) -> {[centroid()], matrix()}
%%
%% @doc Calculates the new centroids for each cluster.
calc_centroids(Attribs, Matrix, Entities, Centroids, M) ->
    lists:foldl(
      fun(Centroid, {NewCentroids, Attribs2})->
          NewAttrs = calc_new_centroid(Matrix, Entities, Centroid, Attribs2, M),
          NewLabel = length(cl_matrix:rows(Attribs2)) + 1,
          {[NewLabel|NewCentroids],
           cl_matrix:insert_new_row(NewLabel, NewAttrs, Attribs2)}
      end,
      {[], Attribs},
      Centroids).

%% @spec calc_new_centroid(matrix(), [entity()], [centroid()], matrix(),
%%                         integer()) -> [{attr(), value()}]
%%
%% @doc Calculates a new centroid.
calc_new_centroid(Matrix, Entities, Centroid, Attribs, M) ->
    Denominator =
        lists:sum(
            [ math:pow(cl_matrix:get(Entity, Centroid, Matrix), M) ||
              Entity <- Entities ]),
    AttribsCols = cl_matrix:cols(Attribs),
    SumWeightedEntities = ets:new(sumweightedentities, []),
    lists:foreach(
      fun(AttribsCol) ->
              ets:insert(SumWeightedEntities, {AttribsCol, 0})
      end, AttribsCols),
    lists:foreach(
      fun(Entity) ->
              Row = cl_matrix:get_row(Entity, Attribs),
              UKXM = math:pow(cl_matrix:get(Entity, Centroid, Matrix), M),
              lists:foreach(
                fun({Col, Val}) ->
                    [{_, Old}] = ets:lookup(SumWeightedEntities, Col),
                    ets:insert(SumWeightedEntities, {Col, Old + UKXM * Val})
                end, Row)
      end, Entities),
    NewAttrs =
        [ {Col, Val / Denominator} ||
          {Col, Val} <- ets:tab2list(SumWeightedEntities) ],
    ets:delete(SumWeightedEntities),
    NewAttrs.
