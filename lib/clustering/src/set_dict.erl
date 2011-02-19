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

%% @doc Library for dictionaries, where the value is a set.
%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(set_dict).
-vsn("$Rev: 3185 $").
-export([add/3, remove/3, to_list/1, to_sorted_list/1]).

%% @spec add(key(),value(),dict(key(),set(value))) ->
%%           dict(key(),set(value))
%% @doc Adds `Value' to the set that is assigned to `Key' in the dictionary
%% `Dict'.
add(Key,Value,Dict) ->
    case dict:find(Key,Dict) of
        {ok,Set} ->
            dict:store(Key, sets:add_element(Value,Set), Dict);
        error ->
            dict:store(Key, sets:from_list([Value]), Dict)
    end.

%% @spec remove(key(),value(),dict(key(),set(value))) ->
%%           dict(key(),set(value))
%% @doc Removes `Value' from the set that is assigned to `Key' in the
%% dictionary `Dict'.
%% If `Key' is not a key of the dictionary, nothing will happen.
remove(Key,Value,Dict) ->
    case dict:find(Key,Dict) of
        {ok,Set} ->
            NewSet = sets:del_element(Value,Set),
            dict:store(Key, NewSet, Dict);
        error ->
            Dict
    end.

%% @spec to_list(dict(key(),set(value))) -> [{key(),[value()]}]
%% @doc Converts the `Dict' dictionary into list of tuples, where the first
%% element of the tuple will be the key, and the second element of the tuple
%% will be a list of elements that are assigned to this key in `Dict'.
to_list(D) ->
    [{K,sets:to_list(V)} || {K,V} <- dict:to_list(D)].

%% @spec to_sorted_list(dict(key(),set(value))) -> [{key(),[value()]}]
%% @doc Converts the `Dict' dictionary into sorted list of tuples, where the
%% first element of the tuple will be the key, and the second element of the
%% tuple will be a sorted list of elements that are assigned to this key in
%% `Dict'.
to_sorted_list(D) ->
    lists:sort([{K,lists:sort(sets:to_list(V))} || {K,V} <- dict:to_list(D)]).
