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

%%% @doc Contains utilities for the clustering modules.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_utils).
-vsn("$Rev: 5049 $").

-export([ignore/1, leave/1, transform_to_01/3, 
         get_defined_value/2, proplist_update/2,
         concat_atoms/2]).

%% @spec ignore(term()) -> (term(),[{Attr,Out::integer()}]) -> bool()
%%
%% @doc Entity filter that drops the modules given as arguments.
ignore(Modules) ->
    fun (Module, _Calls) ->
            lists:member(Module, Modules)
    end.

%% @spec leave(term()) -> (term(),[{Attr,Out::integer()}]) -> bool()
%%
%% @doc Entity filter that keeps the modules given as arguments.
leave(Modules) ->
    fun (Module, _Calls) ->
            not lists:member(Module, Modules)
    end.

%% @private
transform_to_01(_,size,N) -> N;
transform_to_01(_,entities,L) -> L;
transform_to_01(default,default,_N) -> 1;
transform_to_01(_,_,0) -> 0;
transform_to_01(_,_,_N) -> 1.

%%%%% proplists

%% @spec get_defined_value(atom(),proplist()) -> term()
%%
%% @doc Gets the value belonging to `Item' in the `Options' property list.
%% If `Item' is not defined in `Options', an exception will be thrown.
get_defined_value(Item,Options) ->
    case proplists:get_value(Item,Options) of
        undefined ->
            throw(
              "The following option is undefined, which should be defined: '"++
              atom_to_list(Item)++"'");
        Value ->
            Value
    end.

%% @spec proplist_update(proplist(),proplist()) -> proplist()
%%
%% @doc Updates `List1' by `List2'.
%% The new properties will be the properties of `List2', except for the
%% properties that are only present in `List1'.
proplist_update(List1,List2) ->
    proplist_update_sorted(
      lists:usort(proplists:unfold(List1)),
      lists:usort(proplists:unfold(List2))).

%% @spec proplist_update_sorted(proplist(),proplist()) -> proplist()
%%
%% @doc Updates `List1' by `List2'.
%% The new properties will be the properties of `List2', except for the
%% properties that are only present in `List1'.
%% `List1' and `List2' must be unfolded and sorted.
proplist_update_sorted([],[]) ->
    [];
proplist_update_sorted([],L2) ->
    L2;
proplist_update_sorted(L1,[]) ->
    L1;
proplist_update_sorted([{Key,_Value1}|T1],[{Key,Value2}|T2]) ->
    [{Key,Value2}|proplist_update_sorted(T1,T2)];
proplist_update_sorted([{Key1,_}=H1|T1],[{Key2,_}=H2|T2]) ->
    case Key1 < Key2 of
        true ->
            [H1|proplist_update_sorted(T1,[H2|T2])];
        false ->
            [H2|proplist_update_sorted([H1|T1],T2)]
    end.

%%%%% etc

%% @spec concat_atoms(atom(),atom()) -> atom()
%%
%% @doc Concatenates two atoms as strings.
concat_atoms(Atom1,Atom2) ->
    list_to_atom(atom_to_list(Atom1) ++ atom_to_list(Atom2)).

