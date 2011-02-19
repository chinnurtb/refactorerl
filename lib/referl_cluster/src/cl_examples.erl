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

%%% @doc Run module clustering using different algorithms and parameters.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(cl_examples).
-vsn("$Rev: 3185 $").

-export([run_cg/1, run_cgc/1, run_cgca/1]).

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
    cl_print:print_clusterings(Result, [modules,{output_dir,Out}]),
%    cl_print:print_clusters(Result, [modules,{output_dir,Out}]),
    mod_fitness:fitness_hierarchical(Result).

run_cg_a(EntFilt, AttrFilt, Trf, Dist, Merge, Out) ->
    io:put_chars("Calculating attributes...\n"),
    Attribs = core:attribs(cl_call_graph:attrib_data(), cl_call_graph:empty_attrib()),
    Filtered = core:filter(Attribs, EntFilt, AttrFilt),
    Weights = core:transform(Filtered, Trf),
    io:put_chars("Calculating clusters...\n"),
    Result = core:agglom_attr(Weights, Dist, Merge),
    cl_print:print_clusterings(Result, [modules,{output_dir,Out}]),
%    cl_print:print_clusters(Result, [modules,{output_dir,Out}]),
    mod_fitness:fitness_hierarchical(Result).

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

