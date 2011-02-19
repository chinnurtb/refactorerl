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

%%% @doc Test algorithm, similar to examples.erl, but with several different
%%% parameters.
%%%
%%% @todo This module should be cleaned up.
%%% @todo print_clusters_deprecated/2 does not exist any more, so it should be
%%% replaced by print_clusters.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_examples_2).
-vsn("$Rev: 1247 $").

-export([runall_cg/1]).

%% @doc Run module clustering with all possible parameters (12*6*7 times).
%% Output: directory, cl_print creates files within this directory
runall_cg(Output) ->
    CallMethod = [
                  {simplenum, fun cl_call_graph:simplenum/1},
                  {simpleinnum, fun cl_call_graph:simpleinnum/1},
                  {simpleoutnum, fun cl_call_graph:simpleoutnum/1},
                  {allwin, fun cl_call_graph:allwin/1},
                  {allwout, fun cl_call_graph:allwout/1},
                  {allw, fun cl_call_graph:allw/1},
                  {nodynin, fun cl_call_graph:nodynin/1},
                  {nodynout, fun cl_call_graph:nodynout/1},
                  {nodyn, fun cl_call_graph:nodyn/1},
                  {funbin, fun cl_call_graph:funbin/1},
                  {funbout, fun cl_call_graph:funbout/1},
                  {funb, fun cl_call_graph:funb/1}
                 ],
    DistMethod = [
                  {euclidian, fun cl_distfun:euclidian/4},
                  {jaccard, fun cl_distfun:jaccard/4},
                  {sorensen_dice, fun cl_distfun:sorensen_dice/4},
                  {sorensen_dice2, fun cl_distfun:sorensen_dice2/4},
                  {occhai, fun cl_distfun:ochiai/4},
                  {correlation, fun cl_distfun:correlation/4}
                 ],
    GroupMethod = [
                   {single, fun cl_groupfun:single/6},
                   {complete, fun cl_groupfun:complete/6},
                   {uwaverage, fun cl_groupfun:uwaverage/6},
                   {waverage, fun cl_groupfun:waverage/6},
                   {centroid, fun cl_groupfun:centroid/6},
                   {median, fun cl_groupfun:median/6},
                   {ward, fun cl_groupfun:ward/6}
                  ],
%    run_cg(...).
    lists:foreach(
      fun(CM) -> 
              lists:foreach(
                fun(DM) ->
                        lists:foreach(
                          fun(GM) ->
                                  {CM_name, CM_fun} = CM,
                                  {DM_name, DM_fun} = DM,
                                  {GM_name, GM_fun} = GM,

                                  Fulloutput = 
                                      Output ++ "_" ++
                                      [CM_name] ++ "_" ++ [DM_name] ++ 
                                      "_" ++ [GM_name],
                                  run_cg(Fulloutput, CM_fun, DM_fun, GM_fun)
                          end, 
                          GroupMethod)
                end,
                DistMethod)
      end,
      CallMethod).
    
run_cg(Output, CallMethod, DistMethod, GroupMethod) ->
    Attribs = core:attribs(cl_call_graph:attrib_data(),
                           cl_call_graph:empty_attrib()),
    Filtered = core:filter(Attribs,
                           [fun cl_call_graph:library_mod/2],
                           [fun cl_call_graph:internal_fun/2]),
    Weights = core:transform(Filtered, CallMethod),
    Dists = core:distances(Weights, DistMethod),
    Result = core:agglom(Dists, GroupMethod),
    cl_print:print_clusters_deprecated(Result, Output).
