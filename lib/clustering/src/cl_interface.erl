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

%%% @doc Interface module of the clustering and related modules.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_interface).
-vsn("$Rev: 1335 $").

-export([run_cluster/0, run_cluster/1, run_cluster_default/0,
         run_cluster_default/1, run_cluster_labels/0, run_cluster_labels/1,
         get_libs/0, get_libs/1, get_libs_default/0, get_libs_labels/0,
         cut_libs/1, cut_libs_default/0, cut_libs_labels/0,
         fitness/1, fitness_default/0, fitness_labels/0]).

-import(proplists, [get_value/2]).

%% @spec run_cluster() -> ok
%%
%% @doc Runs the clustering with the default options.
%% Same as `run_cluster([])'.
run_cluster() ->
    run_cluster([]).

%% @spec run_cluster(proplist()) -> [[[mod_name()]]]
%%
%% @doc Runs the clustering with the given options.
%%
%% Options (every option has a default value):
%% <ul>
%%     <li>`alg::(agglom_attr | genetic)': the algorithm to be used.
%%         The default is `agglom_attr'.</li>
%%     <li>`modules': the list of modules that should be clustered.
%%         The default value is `undefined'.</li>
%%     <li>`skip_modules': the list of modules that should not be clustered.
%%         The default value is `undefined'.
%%         Either modules or skip_modules or both should be undefined.
%%         If both are undefined, all modules will be clustered.</li>
%%     <li>`log_output::output_ref()': it specifies where the cl_out:fwrite messages about
%%         the execution should be printed.
%%         The default is `stdout'.</li>
%%     <li>`print_options::proplist()':
%%         it specifies how the clustering should be printed.
%%         The `PrintOptions' is forwarded to the `print:print_clusters'
%%         function.
%%         The default is `[{output,null}]'.
%%         A sensible value is
%% ```
%% [{output,
%%   cl_out:start_section_process(cl_out:file_name_gen("output", ".txt"))}]
%% '''
%%         which will write the output into files `"output1.txt"', `output2.txt'
%%         etc. </li>
%% </ul>
%%
%% Options specific to the `agglom_attr' algorithm:
%% <ul>
%%     <li>`transformfun': a function that transforms the attribute matrix
%%         before running the clustering.
%%         It can be `zero_one' or `undefined'. 
%%         `zero_one' means that the weights that are positive will be
%%         transformed to 1.
%%         `undefined' means that no transformation will be performed.
%%         The default value is `undefined'.</li>
%%     <li>`distfun': a distance function.
%%         It can be `call_sum' and `weight'.
%%         The default value is `weight'.</li>
%%     <li>`anti_gravity': if the distance function works with anti_gravity
%%         (like the `weight' distance function), then the weight of that can be
%%         given here.
%%         It is a float (or integer) that can vary between 0 and 1.
%%         (If 0, there is no anti_gravity, if 1, the anti_gravity is very
%%         strong.)
%%         The default value is 0.5.</li>
%%     <li>`mergefun': a merge function.
%%         It can be `smart'.
%%         The default value is `smart'.</li>
%% </ul>
%%
%% Options specific to the `genetic' algorithm:
%% <ul>
%%     <li>`population_size': The number of chromosomes in an iteration.
%%         Default: 12.</li>
%%     <li>`iterations': The number of iteration in the algorithm.
%%         Default: 10.</li>
%%     <li>`mutation_rate': The probability of mutation.
%%         Default: 0.9.</li>
%%     <li>`crossover_rate': The probability of crossover.
%%         Default: 0.7.</li>
%%     <li>`elite_count': The number of chromosomes that survive their
%%         generation because of being the fittest.
%%         Default: 2.</li>
%%     <li>`max_cluster_size': Maximum number of clusters allowed.</li>
%%     <li>`max_start_cluster_size': Maximum number of clusters allowed at
%%         startup.</li>
%% </ul>
%%
%% Example for `Options':
%% ```
%% [{alg, agglom_attr},
%%  {skip_modules, [lib]},
%%  {distfun, weight},
%%  {mergefun, smart},
%%  {anti_gravity, 0.3}]
%% '''
run_cluster(Options) ->
    Opts = cl_utils:proplist_update(run_cluster_default(), Options),
    {W, C} = cl_out:open(get_value(log_output, Opts)),
    Clusterings =
        case get_value(alg, Opts) of
            agglom_attr ->
                run_agglom_attr(W, Opts);
            genetic ->
                cl_genetic:ga(W, Opts)
        end,

    case get_value(print_options,Opts) of
        [{output,null}] ->
            ok;
        PrintOptions ->
            cl_out:fwrite(W, "Printing the clusterings...~n"),
            cl_print:print_clusterings(Clusterings, PrintOptions)
    end,

    cl_out:fwrite(W, "Clustering finished.~n"),
    cl_out:close(C),
    Clusterings.

%% @spec run_cluster_default() -> proplist()
%%
%% @doc Returns the default options of {@link run_cluster/1}.
run_cluster_default() ->
    run_cluster_default(alg) ++
    run_cluster_default(all) ++
    run_cluster_default(agglom_attr) ++
    run_cluster_default(genetic).

%% @spec run_cluster_default(Part::(agglom_attr | genetic | all | alg)) ->
%%           proplist()
%%
%% @doc Returns the parts of the default options of {@link run_cluster/1}.
%%
%% The returned options:
%% <ul>
%%     <li>If `Part' is the name of an algorithm (`agglom_attr' or `genetic'),
%%         the returned options are the options specific to that algorithm.</li>
%%     <li>If `Part' is `all', the returned options are the options that are
%%         common for all algorithm (except for the `alg' options, which is not
%%         returned if `Part' is `all')</li>
%%     <li>If `Part' is `alg', the returned option is `alg'.</li>
%% </ul>
run_cluster_default(alg) ->
    [{alg, agglom_attr}];
run_cluster_default(all) ->
    [{modules, undefined},
     {log_output, stdout},
     {print_options, [{output,null}]}];
run_cluster_default(agglom_attr) ->
     [{skip_modules, undefined},
     {transformfun, undefined},
     {distfun, weight},
     {anti_gravity, 0.5},
     {mergefun, smart}];
run_cluster_default(genetic) ->
    [{population_size, 12},
     {iterations, 10},
     {mutation_rate, 0.9},
     {crossover_rate, 0.7},
     {elite_count, 2},
     {max_cluster_size, 5},
     {max_start_cluster_size, 2}].

%% @spec run_cluster_labels() -> [{atom(),string()}]
%%
%% @doc Returns the labels for the {@link run_cluster/1} function.
run_cluster_labels() ->
    run_cluster_labels(alg) ++
    run_cluster_labels(all) ++
    run_cluster_labels(agglom_attr) ++
    run_cluster_labels(genetic).

%% @spec run_cluster_labels(Part::(agglom_attr | genetic | all | alg)) ->
%%           [{atom(),string()}]
%%
%% @doc Returns the labels for the {@link run_cluster/1} function.
%% For the description of the `Part' argument, see {@link
%% run_cluster_default/1}.
run_cluster_labels(alg) ->
    [{alg, "Algorithm"}];
run_cluster_labels(all) ->
    [{modules, "Modules to cluster"},
     {log_output, "Log output"},
     {print_options, "Print options"}];
run_cluster_labels(agglom_attr) ->
     [{skip_modules, "Modules to skip"},
     {transformfun, "Transform function"},
     {distfun, "Distance function"},
     {anti_gravity, "Antigravity"},
     {mergefun, "Merge Function"}];
run_cluster_labels(genetic) ->
    [{population_size, "Population size"},
     {iterations, "Iterations"},
     {mutation_rate, "Mutation rate"},
     {crossover_rate, "Crossover rate"},
     {elite_count, "Elite count"},
     {max_cluster_size, "Maximum cluster size"},
     {max_start_cluster_size, "Maximum start-cluster-size"}].

%% @spec run_agglom_attr(output_ref(), proplist()) -> [[[mod_name()]]]
%%
%% @doc Runs the agglomerative clustering algorithm.
run_agglom_attr(Output, Options) ->
    
    {W, C} = cl_out:open(Output),
    cl_out:fwrite(W, "Updating and loading the attribute matrix...~n"),
    cl_db:update(mod_attr),
    Attribs = cl_db:load_matrix(mod_attr, mod_attr),

    cl_out:fwrite(W, "Filtering the attribute matrix...~n"),
    EntFilter =
        case {get_value(modules, Options),
              get_value(skip_modules, Options)} of
            {undefined, undefined} ->
                [];
            {Modules, undefined} ->
                [cl_utils:leave(Modules)];
            {undefined, SkipModules} ->
                [cl_utils:ignore(SkipModules)];
            {_, _} ->
                throw("One of 'modules' and 'skip_modules' must be" ++
                      "undefined, but neither is.")
        end,
    FilteredAttribs = cl_core:filter(Attribs, EntFilter, []),

    TransformedAttribs =
        case get_value(transformfun, Options) of
            undefined ->
                FilteredAttribs;
            zero_one -> 
                cl_out:fwrite(W, "Transforming the attribute matrix...~n"),
                cl_core:transform2(FilteredAttribs,
                                   fun cl_utils:transform_to_01/3)
        end,

    cl_out:fwrite(W, "Obtaining the distance function and merge function...~n"),
    DistFun = 
        case get_value(distfun, Options) of
            weight -> 
                AntiGravity = get_value(anti_gravity, Options),
                cl_distfun:weight_gen(cl_distfun:pow_size_fun_gen(AntiGravity));
            call_sum ->
                fun cl_distfun:call_sum/4
        end,

    MergeFun = 
        case get_value(mergefun, Options) of
            smart ->
                fun cl_mergefun:smart/3
        end,

    cl_out:fwrite(W, "Calculating the clusters...~n"),
    cl_out:close(C),
    Result = cl_core:agglom_attr(TransformedAttribs, DistFun, MergeFun),
    cl_matrix:delete(TransformedAttribs),
    Result.

%% @spec get_libs() -> ok
%%
%% @doc Returns modules that are thought to be library modules with the default
%% options.
%% Same as `get_libs([])'.
get_libs() ->
    get_libs([]).

%% @spec get_libs(proplist()) -> [mod_name()]
%%
%% @doc Returns modules that are thought to be library modules.
%%
%% Options:
%% <ul>
%%     <li>`limit': an integer. Modules which are used by at least `limit' other
%%         modules, will be returned as library modules.
%%         The default value is 5.</li>
%% </ul>
get_libs(Options) ->
    Opts = cl_utils:proplist_update(get_libs_default(), Options),
    cl_db:update(mod_attr),
    Attribs = cl_db:load_matrix(mod_attr, mod_attr),
    Libs = cl_attr:get_library_modules(
             Attribs,
             get_value(limit, Opts)),
    cl_matrix:delete(Attribs),
    Libs.

%% @spec get_libs_default() -> proplist()
%%
%% @doc Returns the default options for {@link get_libs/1}.
get_libs_default() ->
    [{limit, 5}].

%% @spec get_libs_labels() -> [{atom(),string()}]
%%
%% @doc Returns the label for {@link get_libs/1}.
get_libs_labels() ->
    [{limit, "Limit"}].

%% @spec cut_libs(proplist()) -> cuts_result_nice()
%%
%%           cuts_result_nice() = 
%%               [{FileName::string(),
%%                {ObjectsNotMoved::[graph_object()],
%%                 ObjectsMoved::[{ClusterId::number(),
%%                                 Cluster::[mod_name()],
%%                                 Objects::[graph_object()]}]}}]
%%           graph_object() = fun_attr() | rec_attr() | macro_attr()
%%
%% @doc Decomposes all the given modules and hrl files.
%%
%% Options:
%% <ul>
%%     <li>`clustering': the clustering, according to which the modules and
%%         headers should be decomposed.
%%         There is no default value, it must be given.</li>
%%     <li>`modules': modules that should be decomposed.
%%         The default is `[]'.</li>
%%     <li>`headers': postfix of headers that should be decomposed.
%%         The default is `[]'.
%%         E.g. if it is `["h.hrl"]', then `"h.hrl"' and "`oh.hrl'" will be, but
%%         `"ho.hrl"' will not be decomposed.</li>
%%     <li>`log_output::output_ref()': it specifies where the cl_out:fwrite messages about
%%         the execution should be printed.
%%         The default is `stdout'.</li>
%%     <li>`print_options::proplist()':
%%         it specifies how the decomposition should be printed.
%%         The `PrintOptions' is forwarded to the `print:print_cuts' function.
%%         The default is `[{output,null}]'.</li>
%% </ul>
cut_libs(Options) ->

    Opts = cl_utils:proplist_update(cut_libs_default(), Options),
    Clustering = cl_utils:get_defined_value(clustering, Opts),
    {W, C} = cl_out:open(get_value(log_output, Opts)),

    cl_out:fwrite(W, "Updating and loading the attribute matricies...~n"),
    cl_db:update(mod_attr),
    cl_db:update(fun_attr),

    cl_out:fwrite(W, "Calculating the decomposition...~n"),
    Cuts = cl_cutlib:cut_libs_all(Clustering,
                                  get_value(modules, Opts),
                                  get_value(headers, Opts)),

    case get_value(print_options,Opts) of
        [{output,null}] ->
            ok;
        PrintOptions ->
            cl_out:fwrite(W, "Printing the decomposition...~n"),
            cl_print:print_cuts(Cuts, PrintOptions)
    end,

    cl_out:fwrite(W, "Decomposition finished.~n"),
    cl_out:close(C),
    Cuts.

%% @spec cut_libs_default() -> proplist()
%%
%% @doc Returns the default options for {@link cut_libs/1}.
cut_libs_default() ->
    [{clustering, no_default},
     {modules, []},
     {headers, []},
     {log_output, stdout},
     {print_options, [{output, null}]}].

%% @spec cut_libs_labels() -> [{atom(),string()}]
%%
%% @doc Returns the label for {@link cut_libs/1}.
cut_libs_labels() ->
    [{clustering, "Clustering"}, 
     {modules, "Modules"},
     {headers, "Headers"},
     {log_output, "Log output"},
     {print_options, "Print options"}].

%% @spec fitness(proplist()) -> [number()]
%%
%% @doc Calculates the fitness value of the given clusterings.
%%
%% Options:
%% <ul>
%%     <li>`clusterings::[[mod_name()]]': the list of clusterings, whose fitness
%%         value should be calculated.
%%         This option is necessary, there is no default value.</li>
%%     <li>`fitness_options::proplist()': this option speficies how to calculate
%%         the fitness value. The value of this option will be passed to the
%%         `cl_fitness:fitness/2' function.
%%         The default is `[]'.</li>
%% </ul>
fitness(Options) ->
    Opts = cl_utils:proplist_update(fitness_default(), Options),
    Clusterings = get_value(clusterings, Opts),
    cl_db:update(deps),
    FitnessOptions = get_value(fitness_options, Opts),
    [ cl_fitness:fitness(Clustering, FitnessOptions) || 
        Clustering <- Clusterings ].

%% @spec fitness_default() -> proplist()
%%
%% @doc Returns the default options for {@link fitness/1}.
fitness_default() ->
    [{clusterings, no_default},
     {fitness_options, []}].

%% @spec fitness_labels() -> [{atom(),string()}]
%%
%% @doc Returns the label for {@link fitness/1}.
fitness_labels() ->
    [{clusterings, "Clusterings"},
     {fitness_options, "Fitness options"}].
