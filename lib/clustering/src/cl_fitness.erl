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

%%% @doc Defines measures for determining the fitness of a clustering result
%%% The clustering result is passed as a parameter, and it is suppesed to
%%% refer to the acual content of the database.
%%%
%%% @todo Correct the documentation: ets -> mnesia
%%%
%%% @author Hanna Kollo <khi@inf.elte.hu>

% call: cl_fitness:fitness(Clustering,FitnessOptions)

-module(cl_fitness).
-vsn("$Rev: 1335 $").

-export([fitness/1, fitness/2]).

-include("cluster.hrl").

%% @spec fitness(Clusters) -> float()
%%       Clusters = [Cluster]
%%       Cluster = [Module]
%%       Module = atom()
%%
%% @doc Calculates the fitness of the clustering by analysing 
%% cluster-inner and inter-cluster connections
%% It assumes that the `deps' table (the `function_calls' and `record_refs'
%% Mnesia tables in the current implementation) is present in the database.
fitness(Clusters) ->
    fitness(Clusters, [{mq, first_version}, {entities, #which_entities{}}]).

%% @doc Fitness function with an options list
fitness(Clusters, Options) ->
    case proplists:get_value(mq, Options, first_version) of
        first_version ->
            core(Clusters, {function_calls, record_refs}, 
                 proplists:get_value(entities, Options, #which_entities{}));
        second_version ->
            core2(Clusters, {function_calls, record_refs}, 
                  proplists:get_value(entities, Options, #which_entities{}))
    end.

%% internal
%% first version of MQ
core(Clusters, Tables, Params) ->
    lists:foldl(
           fun(Cluster, Acc) ->
                   cluster_factor(Clusters, 
                                  Cluster, 
                                  Tables, 
                                  Params)
                       + Acc
           end, 0, Clusters).
    
%% internal
%% second version of MQ
core2(Clusters, Tables, Params) ->
    K = length(Clusters),
    if K == 1 ->
            intra_conn(lists:nth(1, Clusters), Tables, Params);
       true ->
            Intra = 
                lists:foldl(fun(Cluster, Acc) ->
                                    Acc + intra_conn(Cluster, Tables, Params)
                            end, 0, Clusters),
            Inter = 
                lists:foldl(
                  fun(Cluster1, Acc1) -> 
                          Acc1 +
                              lists:foldl(
                                fun(Cluster2, Acc2) ->
                                        Acc2 + inter_conn(Cluster1, 
                                                          Cluster2, 
                                                          Tables,
                                                          Params)
                                end, 0, Clusters)
                  end, 0, Clusters),
            MQ = 1/K * Intra - 2/(K*(K-1)) * Inter,
            MQ
    end.
    
inter_conn(Cluster1, Cluster2, {FunctionCalls, _}, 
          #which_entities{funs=true, recs=false, macros=false}) ->
    inter_conn_fun(Cluster1, Cluster2, FunctionCalls).
    
    
intra_conn(Cluster, {FunctionCalls, _}, 
          #which_entities{funs=true, recs=false, macros=false}) ->
    intra_conn_fun(Cluster, FunctionCalls).

intra_conn_fun(Cluster, Table) ->
    N = length(Cluster),
    mu(Cluster, Table)/(N*N) .
    
inter_conn_fun(Cluster1, Cluster2, Table) ->    
    N1 = length(Cluster1),
    N2 = length(Cluster2),
    if Cluster1 == Cluster2 ->
            0;
       true ->
            epsilon(Cluster1, Cluster2, Table)/(2*N1*N2)
    end.
    
%% @doc Calculates the cluster factor of a clustrer by analysing
%% internal cohesion and inter-cluster connection strength.
cluster_factor(
  Clusters, Cluster, 
  {FunctionTable, RecordTable}, 
  #which_entities{funs=true, recs=true, macros=false}) ->    
    cluster_factor_all(Clusters, Cluster, FunctionTable, RecordTable);
cluster_factor(
  Clusters, Cluster, 
  {FunctionTable, _},
  #which_entities{funs=true, recs=false, macros=false}) ->    
    cluster_factor_fun(Clusters, Cluster, FunctionTable);
cluster_factor(
  Clusters, Cluster, 
  {_, RecordTable}, 
  #which_entities{funs=false, recs=true, macros=false}) ->    
    cluster_factor_rec(Clusters, Cluster, RecordTable).

cluster_factor_all(Clusters, Cluster, ModuleTable, RecordTable) ->
    Mu = mu(Cluster, ModuleTable) + mu2(Cluster, RecordTable),
    SumEpsilon = 
    lists:foldl(fun(Cl, Acc) ->
                        if Cl /= Cluster ->
                                Acc +
                                    epsilon(Cluster, Cl, ModuleTable) +
                                    epsilon(Cl, Cluster, ModuleTable) +
                                    epsilon2(Cluster, Cl, RecordTable);
                           true ->
                                Acc
                        end
                end, 0, Clusters),
    CF = if 2*Mu - SumEpsilon =< 0 ->
            0;
       true ->
            2*Mu/(2*Mu - SumEpsilon)
    end,
    CF.
    
cluster_factor_fun(Clusters, Cluster, Table) ->    
    Mu = mu(Cluster, Table),
    SumEpsilon = lists:foldl(fun (Cl, Acc) ->
                        if Cl /= Cluster ->
                                Acc + 
                                    epsilon(Cluster, Cl, Table) +
                                    epsilon(Cl, Cluster, Table);
                           true ->
                                Acc
                        end
                end, 0, Clusters),
    CF = if 2*Mu - SumEpsilon =< 0 ->
            0;
       true ->
            2*Mu/(2*Mu - SumEpsilon)
    end,
    CF.
    
cluster_factor_rec(Clusters, Cluster, RecordTable) ->    
    Mu2 = mu2(Cluster, RecordTable),
    SumEpsilon2 = lists:foldl(fun (Cl, Acc) ->
                        if Cl /= Cluster ->
                                Acc + 
                                    epsilon2(Cluster, Cl, RecordTable);
                           true ->
                                Acc
                        end
                end, 0, Clusters),
    CF = if 2*Mu2 - SumEpsilon2 =< 0 ->
            0;
       true ->
            2*Mu2/(2*Mu2 - SumEpsilon2)
    end,
    CF.

%% Cluster : [Module]
%% Table: ets({CallerMod, CalledMod, Fun}, Count)
mu(Cluster, Table) ->
    epsilon(Cluster, Cluster, Table).

%% Cluster1 : [Module]
%% Cluster2 : [Module]
%% Table: ets({CallerMod, CalledMod, Fun}, Count)
epsilon(Cluster1, Cluster2, Table) ->
    lists:foldl(
      fun(Module1, Acc1) ->
              Acc1 + lists:foldl(
                fun(Module2, Acc2) ->
                        if Module1 /= Module2 ->                                
                                alpha(Module1, Module2, Table) + Acc2;
                           true -> Acc2
                        end
                end, 0, Cluster2)
      end, 0, Cluster1).

%% internal
alpha(Module1, Module2, Table) ->
    case mnesia:dirty_match_object(Table,{'_',{Module1, Module2},'_'}) of
        [] -> 0;
        _ -> 1
    end.

%% Cluster : [Module]
%% Table: ets({Module, Record}, Count)
mu2(Cluster, Table) ->
    epsilon2(Cluster, Cluster, Table)/2. %% division due to commutatvity

%% Cluster1 : [Module]
%% Cluster2 : [Module]
%% Table: ets({Module, Record}, Count)
epsilon2(Cluster1, Cluster2, Table) ->
    lists:foldl(
      fun(Module1, Acc1) ->
              Acc1 + lists:foldl(
                fun(Module2, Acc2) ->
                        if Module1 /= Module2 ->                                
                                alpha2(Module1, Module2, Table) + Acc2;
                           true -> Acc2
                        end
                end, 0, Cluster2)
      end, 0, Cluster1).

 
%internal
alpha2(Module1, Module2, Table) ->
    L1 = mnesia:dirty_select(Table,
                             [{{'_',{Module1, '$1'}, '$2'}, [], ['$1']}]),
    L2 = mnesia:dirty_select(Table,
                             [{{'_',{Module2, '$1'}, '$2'}, [], ['$1']}]),
    %% count common elements
    R = lists:foldl(fun(Elem, Acc) ->
                        R = lists:member(Elem, L2),
                        if R ->
                                Acc + 1;
                           true -> Acc
                        end
                end, 0, L1),
    if R > 0 ->
            1;
       true ->
            0
    end.

