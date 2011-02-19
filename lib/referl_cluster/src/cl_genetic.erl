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

%%% @doc Implements a genetic algorithm that determines a promising module
%%% clustering, form the modules which are in the database.
%%% The value of the clustering is calculated by the
%%% {@link cl_fitness:fitness/1} function.
%%%
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_genetic).
-vsn("$Rev: 5049 $").

-export([ga/0, 
         ga/2,
         ga_default/0]).

-include("cluster.hrl").

-record(global, {data, output, options}).

%% @doc Runs genetic algoritm with default parameters.
ga() ->
    ga(stdout, [{population_size, 12},
                {iterations, 10},
                {mutation_rate, 0.9},
                {crossover_rate, 0.7},
                {elite_count, 2},
                {max_cluster_size, 5},
                {max_start_cluster_size, 2}]).

%% @doc Runs genetic algoritm with given parameters.
ga(Output, Options) ->
    cl_db:update(deps), 
    {W, C} = cl_out:open(Output), 
    FilesNum = length(?Query:exec([file])),
   	Options2 = cl_utils:proplist_update(ga_default(), Options),
%todo: remove Data
    Data = null, 
    Result = if FilesNum >= 2 ->
	                core(#global{data=Data, output=W, options=Options2});
	            true -> []
	         end,
    cl_out:close(C),
    Result.
%% @doc Returns back the default parameters of genetic algorithm.
ga_default() ->
    [{population_size, 12},
     {iterations, 10},
     {mutation_rate, 0.9},
     {crossover_rate, 0.7},
     {elite_count, 2},
     {max_cluster_size, 5},
     {max_start_cluster_size, 2}].

    
core(Global) ->
    PopulationSize = proplists:get_value(population_size,Global#global.options),
    Iterations = proplists:get_value(iterations,Global#global.options),
    cl_out:fwrite(Global#global.output, "Starting genetic algorithm...~n"),
    cl_out:fwrite(Global#global.output, 
                  "---------------------------------------------------~n"),
    L = chromosome_length(),
    Population = random_population(Global, PopulationSize, L),
    ga(Global, Population, L, 1,Iterations).    

%% ga(Global, Population, Length, N, N) ->
%%     cl_out:fwrite(Global#global.output, "Iteration no. ~p~n", [N]),
%%     PopFit = population_fitness(Global, Population, Length),
%%     {TmpList, _} = lists:unzip(PopFit),
%%     cl_out:fwrite(Global#global.output, "Fitnesses = ~p~n", [TmpList]),
%%     {_MostFitF, MostFitC} = most_fit(Global, Population, Length),
%%     Clusters = phenotype(Global, MostFitC, Length),
%%     Clusters;
ga(Global, Population, Length, N, N) ->
    cl_out:fwrite(Global#global.output, "Iteration no. ~p~n", [N]),
    PopFit = population_fitness(Global, Population, Length),
    {TmpList, _} = lists:unzip(PopFit),
    cl_out:fwrite(Global#global.output, "Fitnesses = ~p~n", [TmpList]),
%    {_MostFitF, MostFitC} = most_fit(Global, Population, Length),
%    Clusters = phenotype(Global, MostFitC, Length),
%    Clusters;
    lists:map(fun (Elem) -> phenotype(Global, Elem, Length) end, Population);
ga(Global, Population, Length, K, N) ->
    cl_out:fwrite(Global#global.output, "Iteration no. ~p~n", [K]),
    NewPopulation = create_new_population(Global, Population, Length),
    ga(Global, NewPopulation, Length, K+1, N).

create_new_population(Global, OldPopulation, Length) ->
    EliteCount = proplists:get_value(elite_count,Global#global.options),
    PopulationSize = proplists:get_value(population_size,Global#global.options),
    PopFit = population_fitness(Global, OldPopulation, Length),
    {TmpList, _} = lists:unzip(PopFit),
    cl_out:fwrite(Global#global.output, "Fitnesses = ~p~n", [TmpList]),
    PopLevel = population_leveled(Global, PopFit, Length),
    Elites = elites(Global, PopFit, EliteCount),
    NewPopulation =
    select_new_population(Global, Elites, PopFit, PopLevel, Length, 0, 
                          PopulationSize-EliteCount),
    NewPopulation.

elites(Global, PopFit, EliteCount) ->
    elites(Global, [], PopFit, EliteCount).

elites(_Global, Elites, _, 0) ->
    Elites;
elites(Global, Elites, [{_F, Chromosome} | RestPop], K) ->
    elites(Global, [Chromosome | Elites], RestPop, K-1).

population_fitness(Global, Population, Length) ->
    L = lists:map(
      fun(Chromosome) ->
              Phenotype = phenotype(Global, Chromosome, Length), 
              Fitness = 
                  cl_fitness:fitness(
                    Phenotype, 
                    [{mq, first_version}, 
                     {entity_type, module},
                     {entities, 
                      #which_entities{funs=true, recs=false, macros=false}}
                    ]),
              {Fitness, Chromosome}
      end, Population),
    lists:reverse(lists:sort(L)).

population_leveled(_Global, PopFit, Length) ->
    SumFitness = sum_fitness(PopFit),    
    if SumFitness == 0 ->
            {PopLevel, _} = 
                lists:mapfoldl(
                  fun({_Fitness, Chromosome}, Acc) ->
                          {{1/Length + Acc, Chromosome}, 1/Length + Acc}
                  end, 0, PopFit);
       true ->
            SortPop = lists:sort(PopFit),
            {PopLevel, _} = 
                lists:mapfoldl(
                  fun({Fitness, Chromosome}, Acc) ->
                          X = (Fitness + Acc)/SumFitness,
                          {{X, Chromosome}, Fitness + Acc}
                  end, 0, SortPop)
    end,
    PopLevel.
    

sum_fitness(PopFit) ->
    lists:foldl(fun({F, _C}, Acc) -> F + Acc end, 0, PopFit).

choose_chromosome(_Global, PopLevel, roulettewheel) ->
    Level = random:uniform(),
    [{_F, FirstCh} | _] = PopLevel,
    lists:foldl(fun({Value, Chromosome}, Acc) ->
                        if Value < Level ->
                                Chromosome;
                           true ->
                                Acc
                        end
                end, FirstCh, PopLevel);

choose_chromosome(_Global, PopFit, upperquarter) ->
    N = round(length(PopFit)/4),
    R = random:uniform(N),
    {_, Chromosome} = lists:nth(R, PopFit),
    Chromosome.

select_new_population(Global, PartPopulation, PopFit, PopLevel, Length, K, N) ->
    C1 = choose_chromosome(Global, PopFit, upperquarter),
    C2 = choose_chromosome(Global, PopFit, upperquarter),
    {C3, C4} = perform_crossover(Global, C1, C2, Length),
    {C5, C6} = perform_mutation(Global, C3, C4, Length),
    if K + 2 < N ->
            NewPopulation = [C5, C6 |PartPopulation],
            select_new_population(Global,
                                  NewPopulation, 
                                  PopFit, 
                                  PopLevel, 
                                  Length, 
                                  K+2, 
                                  N);
       K + 2 == N ->
            NewPopulation = [C5, C6 | PartPopulation],
            NewPopulation;
       K + 1 == N ->
            NewPopulation = [C5 | PartPopulation],
            NewPopulation
    end.

perform_crossover(Global, C1, C2, Length) ->
    CrossoverRate = proplists:get_value(crossover_rate,Global#global.options),
    CrossoverP = random:uniform(),
    if CrossoverP < CrossoverRate ->
            C3 = crossover(Global, C1, C2, Length),
            C4 = crossover(Global, C1, C2, Length);
       true ->
            C3 = C1,
            C4 = C2
    end,
    {C3, C4}.

perform_mutation(Global, C3, C4, Length) ->
    MutationRate = proplists:get_value(mutation_rate,Global#global.options),
    MutationP = random:uniform(),
    if MutationP < MutationRate ->
            C5 = mutation(Global, C3, Length),
            C6 = mutation(Global, C4, Length);
       true ->
            C5 = C3,
            C6 = C4
    end,
    {C5, C6}.


%% most_fit(Global, Population, Length) ->
%%     PopFit = population_fitness(Global, Population, Length),
%%     [First| _] = PopFit,
%%     {MostFitF, MostFitC} = lists:foldl(
%%                              fun({F, C}, {AccF, AccC}) ->
%%                                      if F > AccF ->
%%                                              {F, C};
%%                                         true ->
%%                                              {AccF, AccC}
%%                                      end
%%                              end, First, PopFit),
%%     {MostFitF, MostFitC}.

empty_clusters(Length) ->
    L = empty_clusters([], Length),
    dict:from_list(L).

empty_clusters(List, 0) ->
    List;
empty_clusters(List, K) ->
    empty_clusters([{K, []} | List], K-1).

phenotype(_Global, Chromosome, Length) ->
    Modules = modules(),
    L = dict:to_list(Chromosome),
    {_ ,List} = lists:unzip(L),
    Clusters = empty_clusters(Length),
    {C3, _} =
    lists:foldl(
      fun(Elem, {C, Index}) ->
              Module = lists:nth(Index, Modules),
              C2 = dict:update(
                     Elem, fun(Data) ->
                                   [?Mod:name(Module) | Data]
                           end, C),
              {C2, Index + 1}
      end, {Clusters, 1}, List),
    {_ , ClusterList} = lists:unzip(dict:to_list(C3)),
    Phenotype = lists:filter(fun([]) -> false; (_) -> true end, ClusterList),
    Phenotype.
                      
chromosome_length() ->
    length(modules()).

random_population(Global, PopulationSize, ChromosomeLength) ->
    random_population(Global, [], PopulationSize, PopulationSize, 
                      ChromosomeLength).

random_population(_Global, List, 0, _, _) ->
    List;
random_population(Global, List, K, PopulationSize, ChromosomeLength) ->
    MaxClusterSize = proplists:get_value(max_cluster_size,
                                         Global#global.options),
    MaxStartClusterSize = proplists:get_value(max_start_cluster_size,
                                              Global#global.options),
    R = random_chromosome(Global, ChromosomeLength, 
                          min(MaxClusterSize, MaxStartClusterSize)),
    random_population(Global, [R | List], K-1, PopulationSize, 
                      ChromosomeLength).

random_chromosome(Global, ChromosomeLength, Range) ->
    L = random_chromosome(Global, [], ChromosomeLength, ChromosomeLength, 
                          Range),
    dict:from_list(L).

random_chromosome(_Global, List, 0, _, _) ->
    List;
random_chromosome(Global, List, K, N, Range) ->
    R = random:uniform(Range),
    random_chromosome(Global, [{K, R} | List], K-1, N, Range).

mutation(Global, Chromosome, Length) ->
    MaxClusterSize = proplists:get_value(max_cluster_size,Global#global.options),
    Point = random:uniform(Length),
    R = random:uniform(min(Length, MaxClusterSize)),
    dict:update(Point, fun(_Value) -> R end, Chromosome).

crossover(_Global, Chromosome1, Chromosome2, Length) ->
    Point = random:uniform(Length),
    dict:merge(fun(Key, Value1, Value2) ->
                       if Key < Point ->
                               Value1;
                          true ->
                               Value2
                       end
               end, Chromosome1, Chromosome2).

modules() ->
    ?Query:exec(?Query:seq([file], ?File:module())).

min(X, Y) ->
    if X < Y ->
            X;
       true ->
            Y
    end.

