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

%%% @doc Print the results of module clustering into dot files.
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(cl_misc).
-vsn("$Rev$").

-include("cluster.hrl").

%% =============================================================================
%% Exports

%% Tuples
-export([swap/1]).
%% List
-export([list_cnt/1]).



%% =============================================================================
%% Tuples

%% @spec swap(tuple() | [tuple()]) -> tuple() | [tuple()]
%% @doc  Swap the elements of 2-tuples.
swap({A,B}) -> {B,A};
swap(TupleList) when is_list(TupleList) -> [swap(T) || T<-TupleList].



%% =============================================================================
%% List

%% @spec list_cnt(List::[term()]) -> [{term(), Count::integer()}]
%% @doc  Return a sorted list of elments of `List' where same elements are 
%%       present only once. The elements are zipped into a tuple with their
%%       frequency.
list_cnt(List) when is_list(List) ->
    dict:to_list(lists:foldl(fun(Elem,D) -> dict:update_counter(Elem,1,D) end, 
                             dict:new(), List)).


