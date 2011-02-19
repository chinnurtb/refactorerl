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

%%% @doc This module contains functions that calculate the virtual attribute
%%% vector of an entity group from the attribute vectors of the groups'
%%% contents.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_mergefun).
-vsn("$Rev: 3185 $").

-export([sum/3, avg/3, smart/3, print_merged_clusters/3]).

-include("cluster.hrl").

%% @type merge_fun() = (NewLst,[{attr(), value()}],
%%                      {entity(), [{attr(), value()}],
%%                       entity(), [{attr(), value()}]}) -> [{attr(), value()}]

%% @doc Calculate virtual attributes by summing the original attribute values.
sum(_, AttrLst,_) ->
    mapn(
      fun(Attrs) ->
              {Attr, _} = hd(Attrs),
              {Attr, lists:sum([V || {A,V} <- Attrs, A == Attr])}
      end, AttrLst).

%% @doc Calculate virtual attributes by taking the average of the original
%% attribute values.
avg(_, AttrLst,_) ->
    mapn(
      fun(Attrs) ->
              {Attr, _} = hd(Attrs),
              Vals = [V || {A,V} <- Attrs, A == Attr],
              {Attr, lists:sum(Vals) / length(Vals)}
      end, AttrLst).

mapn(_, []) -> [];
mapn(Fun, AttrLst) ->
    [ Fun([Head || [Head|_] <- AttrLst]) |
      mapn(Fun, [Tail || [_|Tail] <- AttrLst, Tail /= []])].

%% @spec print_merged_clusters(info_fun(),merge_fun(),writable_device()) ->
%%           merge_fun()
%%
%%       info_fun() = (entity(), [{attr(), value()}],
%%                     entity(), [{attr(), value()}]) -> term()
%%
%% @doc Generates a merge function that wraps another merge function
%% (`MergeFun').
%% It works as if `MergeFun' was the merge function, but in addition it prints
%% information about the merged clusters. 
%% `Infofun' is the function that converts the attributes of the clusters to
%% information that will be printed. It can be, for example, a distance
%% function.
%%
%% Example for usage:
%% ```
%% DistFun = cl_distfun:weight_gen(cl_distfun:pow_size_fun_gen(0.5)),
%% MergeFun = cl_mergefun:print_merged_clusters(DistFun,
%%                                              fun cl_mergefun:smart/3,
%%                                              {stdout,[]}),
%% cl_interface:run_cluster([{distfun, DistFun}, {mergefun, MergeFun}])
%% '''
%% A possible output:
%% ```
%% Updating and loading the attribute matrix...
%% Filtering the attribute matrix...
%% Obtaining the distance function and merge function...
%% Calculating the clusters...
%% 0.873485, [lib], [a1]
%% 0.706144, [a2], [a1,lib]
%% 1.40541, [a3], [a1,a2,lib]
%% Clustering finished.
%% [[[a3,a2,lib,a1]],[[a2,lib,a1],[a3]],[[lib,a1],[a3],[a2]]]
%% '''
print_merged_clusters(Infofun,MergeFun,W) ->
    fun(NewLst, AttrLst,{Gr1,A1,Gr2,A2}) ->
            D = Infofun(Gr1,A1,Gr2,A2),
            cl_out:fwrite(W,"~p, ",[D]),
            cl_out:fwrite(W,"~p, ",[proplists:get_value(entities,A1,[])]),
            cl_out:fwrite(W,"~p~n",[proplists:get_value(entities,A2,[])]),
            MergeFun(NewLst, AttrLst,{Gr1,A1,Gr2,A2})
    end.

%% @doc A merge function that receives an attribute matrix that contains the
%% following kind of columns: "size", "entities", function, record and marco.
smart(_, AttrLst,_) ->
    mapn(
      fun(Attrs) ->
              {Attr, _} = hd(Attrs),
              Vals = [V || {A,V} <- Attrs, A == Attr],
              case Attr of
                  size -> {size, lists:sum(Vals)};
                  entities -> {entities, lists:merge(Vals)};
                  #fun_attr{} -> {Attr,lists:sum(Vals)/length(Vals)};
                  #rec_attr{} -> {Attr,lists:sum(Vals)/length(Vals)};
                  #macro_attr{} -> {Attr,lists:sum(Vals)/length(Vals)};
                  _ -> undefined
              end
      end, AttrLst).

