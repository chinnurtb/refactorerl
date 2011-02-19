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

%%% @doc Run module clustering using different algorithms and parameters.
%%%
%%% @todo This module should be cleaned up.
%%% @todo print_clusters_deprecated and print_clusters does not work.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_examples).
-vsn("$Rev: 1247 $").

-export([run_cg/1, run_cgc/1, run_cgca/1, run_cluster/1, run_fun_cluster/1,
         run_size_cluster_simple/0, run_size_cluster/1, run_size_cluster/3,
         cutlib/0,cutlib2/0]).
-export([run_save_size/3]).

-include("cluster.hrl").

%%% @type name() = string() | atom()

-define(MLGLIBS, [mlgAdmMgr,mlgCapabilities,mlgGcpI,mlgDcI,mlgCliLib,
                  mlgTrhLib,mlgTrafLib,mlgLib,mlgTrace,mlgBrmMsg,mlgHcMsg]).

%% @spec run_cg(name()) -> ok
%%
%% @doc Run module clustering based on the `call_graph' analyser, using
%% Jaccard and Ward distance functions.
run_cg(Output) ->
    run_cg_d([cl_utils:ignore(?MLGLIBS), fun drop_cli_snmp/2],
             [fun cl_call_graph:internal_fun/2],
             fun cl_call_graph:simplenum/1,
             fun cl_distfun:jaccard/4,
             fun cl_groupfun:ward/6,
             Output).

%% @spec run_cgc(name()) -> ok
%%
%% @doc Run module clustering based on the `call_graph' analyser, using call
%% count and Ward distance.
run_cgc(Output) ->
    run_cg_d([cl_utils:ignore(?MLGLIBS), fun drop_cli_snmp/2],
             [fun cl_call_graph:internal_fun/2],
             fun cl_call_graph:simpleoutnum/1,
             fun cl_distfun:call_cnt/4,
             fun cl_groupfun:ward/6,
             Output).

%% @spec run_cgca(name()) -> ok
%%
%% @doc Run module clustering based on the `call_graph' analyser, using call
%% count distance and sum of attributes.
run_cgca(Output) ->
    run_cg_a([cl_utils:ignore(?MLGLIBS), fun drop_cli_snmp/2],
             [fun 'cl_call_graph':internal_fun/2],
             fun 'cl_call_graph':simpleoutnum/1,
             fun 'cluster.distfun':call_cnt/4,
             fun 'cluster.mergefun':sum/3,
             Output).

run_cg_d(EntFilt, AttrFilt, Trf, Dist, Group, Out) ->
    io:put_chars("Calculating attributes...\n"),
    Attribs = core:attribs(cl_call_graph:attrib_data(),
                           cl_call_graph:empty_attrib()),
    Filtered = core:filter(Attribs, EntFilt, AttrFilt),
    Weights = core:transform(Filtered, Trf),
    io:put_chars("Calculating distances...\n"),
    Dists = core:distances(Weights, Dist),
    io:put_chars("Calculating clusters...\n"),
    Result = core:agglom_dist(Dists, Group),
    cl_print:print_clusters(Result, [modules,{output_dir,Out}]),
    mod_fitness:fitness_hierarchical(Result).

run_cg_a(EntFilt, AttrFilt, Trf, Dist, Merge, Out) ->
    io:put_chars("Calculating attributes...\n"),
    Attribs = core:attribs(cl_call_graph:attrib_data(), cl_call_graph:empty_attrib()),
    Filtered = core:filter(Attribs, EntFilt, AttrFilt),
    Weights = core:transform(Filtered, Trf),
    io:put_chars("Calculating clusters...\n"),
    Result = core:agglom_attr(Weights, Dist, Merge),
    cl_print:print_clusters(Result, [modules,{output_dir,Out}]),
    mod_fitness:fitness_hierarchical(Result).

%% @spec run_cluster(name()) -> ok
%%
%% @doc Runs clustering using the attribute matrix of mod_attr and clustering
%% algorithm of agglom_attr.
run_cluster(Output) ->
    run_cluster([fun 'cluster.mod_attr':library_mod/2],
                [fun 'cluster.mod_attr':internal_fun/2],
                fun 'cluster.distfun':call_rec_cnt/4,
                fun 'cluster.mergefun':avg/3,
                Output).    

run_cluster(EntFilt, AttrFilt, Dist, Merge, Out) ->
    io:put_chars("Calculating attributes...\n"),
    Attribs = core:attribs(mod_attr:attrib_data(), mod_attr:empty_attrib()),
    Filtered = core:filter(Attribs, EntFilt, AttrFilt),
    io:put_chars("Calculating clusters...\n"),
    Result = core:agglom_attr(Filtered, Dist, Merge),
    cl_print:print_clusters_deprecated(Result, Out).

%% @spec run_fun_cluster(name()) -> ok
%%
%% @doc Runs clustering on functions, using the attribute matrix of fun_attr
%% and clustering algorithm of agglom_attr.
run_fun_cluster(Output) ->
    run_fun_cluster([],
                    [],
                    fun 'cluster.distfun':call_fun_cnt/4,
                    fun 'cluster.mergefun':avg/3,
                    Output).   

run_fun_cluster(EntFilt, AttrFilt, Dist, Merge, Out) ->
    Attribs = core:attribs(fun_attr:fun_attrib_data(),
                           fun_attr:fun_empty_attrib()),
    Filtered = core:filter(Attribs, EntFilt, AttrFilt),
    Result = core:agglom_attr(Filtered, Dist, Merge),
    cl_print:print_clusters_deprecated(Result, Out).

%% @spec run_save_size(name(),number(),integer()) -> ok
%%
%% @doc Calculates and saves the fitness of the clusterings made by the
%% function run_size_cluster_simple.
run_save_size(Out, Pow, LibK) ->
    io:format("Calculating the attribute matrix...~n",[]),
    Attribs = core:attribs(
                fun_attr:mod_attrib_data(), 
                fun_attr:mod_empty_attrib()),
    Result=run_size_cluster_simple(Attribs,Pow, LibK),
    cl_print:print_clusters(Result, [modules,{output_dir,Out}]),
    mod_fitness:fitness_hierarchical(Result).

%% @doc Runs clustering with using size as an antigravity factor.
%% It uses the attribute matrix of fun_attr.
run_size_cluster_simple() ->
    io:format("Calculating the attribute matrix...~n",[]),
    Attribs = core:attribs(
                fun_attr:mod_attrib_data(), 
                fun_attr:mod_empty_attrib()),
    run_size_cluster_simple(Attribs).

run_size_cluster_simple(Attribs) ->
    run_size_cluster_simple(Attribs,1.2,13).

run_size_cluster_simple(Attribs,PowWeight,LibK) ->
    EntFilt = [cl_utils:ignore(fun_attr:get_library_modules(Attribs, LibK)),
               fun drop_cli_snmp/2],
    AttrFilt = [fun 'cluster.mod_attr':internal_fun/2],
    TrfFun = fun transform_to_01/3,
    SizeFun = fun(S1,S2) -> math:pow(S1+S2,PowWeight) end,
    DistFun = .cluster.distfun:weight_gen(SizeFun),
    MergeFun = fun 'cluster.mergefun':smart/3,

    Filtered = core:filter(Attribs, EntFilt, AttrFilt),
    Weights = core:transform2(Filtered, TrfFun),
    io:format("Calculating the clusters...~n",[]),
    core:agglom_attr(Weights, DistFun, MergeFun).

%% @spec run_size_cluster(name()) -> ok
%%
%% @doc Runs clustering with using size as an antigravity factor, and puts the
%% resulted files into the given directory.
%% It uses the attribute matrix of fun_attr.
run_size_cluster(OutputDir) ->
    run_size_cluster([create_dir],
                     filename:join(OutputDir,"now1"), 
                     fun(S1,S2) -> math:pow(S1+S2,0.3) end),
    run_size_cluster([create_dir],
                     filename:join(OutputDir,"now2"), 
                     fun(S1,S2) -> math:pow((S1+S2)/19,0.3) end).

transform_to_01(_,size,N) -> N;
transform_to_01(_,entities,L) -> L;
transform_to_01(default,default,_N) -> 1;
transform_to_01(_,_,0) -> 0;
transform_to_01(_,_,_N) -> 1.

%% @spec run_size_cluster([atom()],string(),(number(),number())->number()) ->
%%           ok
%%
%% @doc Runs clustering with using size as an antigravity factor, and puts the
%% resulted files into the given directory.
%% It uses the attribute matrix of fun_attr.
%% If `Options' contains the atom 'create_dir', the `OutputDir' will be created.
%% `SizeFun' specifies how to calculate the antigravity between two clusters
%% considering their size.
run_size_cluster(Options,OutputDir,SizeFun) ->
    run_size_cluster(Options,OutputDir,
                     [fun 'cluster.mod_attr':library_mod/2],
                     [fun 'cluster.mod_attr':internal_fun/2],
                     fun transform_to_01/3,
                     %%.cluster.distfun:call_rec_size_cnt_gen(SizeFun),
                     cluster.distfun:weight_gen(SizeFun),
                     fun 'cluster.distfun':weight_distvec/4,
                     fun 'cluster.mergefun':smart/3).

%% @doc Runs clustering. 
%% The clusters will be put into `OutputDir'/clusterNNN.txt.
%% Information about the merged clusters will be placed in 
%% `OutputDir'/merge.txt.
%% If `Options' contains create_dir, then the `OutputDir' will be created.
%% `InfoFun' is the function that returns the information that will be placed in
%% merge.txt.
run_size_cluster(Options,OutputDir,EntFilt,AttrFilt,TrfFun,DistFun,InfoFun,
                 MergeFun) ->
    case lists:member(create_dir,Options) of
        true -> file:make_dir(OutputDir);
        false -> ok
    end,
    MergeFileName = filename:join(OutputDir,"merge.txt"),
    io:format("Calculating the attribute matrix: ",[]),
    Sec1 = now_sec(),
    Attribs = core:attribs(
                fun_attr:mod_attrib_data(), 
                fun_attr:mod_empty_attrib()),
    Filtered = core:filter(Attribs, EntFilt, AttrFilt),
    Weights = case TrfFun of
                  nothing -> Attribs;
                  _ -> core:transform2(Filtered, TrfFun)
              end,
    cl_print:draw_attribs(Weights,filename:join(OutputDir,"attr.dot")),
    Sec2 = now_sec(),
    io:format("~p seconds.~n",[Sec2-Sec1]),
    io:format("Calculating the clusters: ",[]),
    Result = core:agglom_attr(Weights,
                              DistFun,
                              mergefun:print_merged_clusters(
                                InfoFun,
                                MergeFun,
                                MergeFileName)),
    Sec3 = now_sec(),
    io:format("~p seconds.~n",[Sec3-Sec2]),
    io:format("Printing the clusters: ",[]),
    cl_print:print_clusters(Result,[modules,{output_dir,OutputDir}]),
    Sec4 = now_sec(),
    io:format("~p seconds.~n",[Sec4-Sec3]).

now_sec() ->
    element(1,now())*1000000 + element(2,now()).

%% @doc Uses the cutlib module if the Refactorerl tool is in the database.
cutlib() ->
    io:format("Calculating the attribute matrix: ",[]),
    ModAttribs = core:attribs(
                   fun_attr:mod_attrib_data(), 
                   fun_attr:mod_empty_attrib()),
    FunAttribs = core:attribs(
                   fun_attr:fun_attrib_data(), 
                   fun_attr:fun_empty_attrib()),

    Clusters = 
        [[refac_superv, refac_event_printer, refac_emacs],
         [refac_anal, refac_synlex, refac_token, refac_query],
         [refac_ui, refac_movefun, refac_manip, refac_draw_graph,
          refac_preproc, refac_fileman, anal_record, anal_attrib],
         [anal_context, anal_module, anal_function, anal_exprtype,
          refac_extract_fun, anal_variable]],

    io:format("Library cutting: ~n",[]),
    Funs = cutlib:contents_of_modules([refac_graph],FunAttribs),
    cutlib:cut_lib(ModAttribs,FunAttribs,Clusters,Funs).

%% @doc This function calls the library splitting algorithm.
%% The 15 functions with the most incoming calls will be considered as library
%% functions. The antigravity will be a factor of 0.6.
cutlib2() ->
    LibK=15,
    cutlib3(
      fun(Attribs) ->
              Clusterings = run_size_cluster_simple(Attribs,0.6,LibK),
              lists:nth(3,Clusterings)
      end,
      LibK) %% LibK
        .

%% `ClusteringFun': function that returns the clustering based on the attribute
%% matrix.
%% `LibK': the number of user modules after a module is considered to be a
%% library module.
cutlib3(_ClusteringFun,LibK) ->
    io:format("Calculating the attribute matrix...",[]),
    MA = fun_attr:mod_attrib_data(), 
    ME = fun_attr:mod_empty_attrib(),
    FA = fun_attr:fun_attrib_data(), 
    FE = fun_attr:fun_empty_attrib(),

    %%io:format("MA:~n~p~n",[MA]),
    %%io:format("ME:~n~p~n",[ME]),
    %%io:format("FA:~n~p~n",[FA]),
    %%io:format("FE:~n~p~n",[FE]),

    ModAttribs = core:attribs(MA,ME),
    FunAttribs = core:attribs(FA,FE),

    %%io:format("ModAttribs:~n",[]),
    %%matrix2:dump(ModAttribs),
    %%io:format("FunAttribs:~n",[]),
    %%matrix2:dump(FunAttribs),

    Libs = fun_attr:get_library_modules(ModAttribs,LibK),
    io:format("Libs:~n~p~n",[Libs]),

    ModAttribs2 = matrix2:clone(ModAttribs),
    ModAttribs3 = core:filter(ModAttribs2,[cl_utils:ignore(Libs)],[]),

    Clusters = [[a1,a2],[a3]],
    %%Clusters = ClusteringFun(ModAttribs3),
    io:format("Clusters:~n~p~n",[Clusters]),

    io:format("Library cutting...~n",[]),
    LibFiles = 
        cutlib:collect_files_by_modnames(Libs)++
        cutlib:collect_files_by_filenames(["/a/x/ref/trunk/test/h.hrl"]),
    Cuts = cutlib:cut_libs(ModAttribs,FunAttribs,Clusters,LibFiles),
    lists:foldl(
        fun({File,Cut},_) ->
                FileName = (?ESG:data(File))#file.path,
            io:format("~nFile: ~p~n~n",[FileName]),
            cl_print:print_cut(Cut,[])
        end,
        undefined,
        Cuts
    ),

    %% deleting tables
    core:delete(ModAttribs),
    core:delete(ModAttribs3),
    core:delete(FunAttribs),
    ok. 

%% TODO: this function throws exception when `Mod' contains only one or two
%% characters
drop_cli_snmp(Mod, _) ->
    MN = atom_to_list(Mod),
    case .string:substr(MN, length(MN)-2) of
        "Cli" -> true;
        _ ->
            case .string:substr(MN, length(MN)-3) of
                "Snmp" -> true;
                _ -> false
            end
    end.
            
