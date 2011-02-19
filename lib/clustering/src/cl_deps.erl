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

%%% @doc Calcultes the dependecy-tables.
%%% The `insert_function_calls' calculates information about which module calls
%%% which module.
%%% The `insert_record_refs' calculates information about which two modules use
%%% common records. 
%%%
%%% @todo modify record_refs.
%%%
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_deps).
-vsn("$Rev: 1146 $").

-export([insert_function_calls/1, insert_record_refs/1]).

-include("cluster.hrl").

%% Table: ets({CallerMod, CalledMod}, ok)
insert_function_calls(Table) ->
    L = lists:flatten([[{Module, Call} || Call <- function_calls(Module)] 
                       || Module <- modules()]),
    lists:foreach(
      fun({Module, Call}) ->
              {module, CallerMod} = ?GRAPH:data(Module),
              {module, CalledMod} = 
                  ?GRAPH:data(lists:nth(1,?GRAPH:path(Call, [{func, back}]))),
              ets:insert(Table, {{CallerMod, CalledMod}, ok})
      end,
      L).

%% needed: Table: ets({Module1, Module2}, ok)
%% currently: Table: ets({ModuleName, RecordName}, Count)
insert_record_refs(Table) ->
    L = lists:flatten([[{Module, Call} || Call <- record_refs(Module)] 
                       || Module <- modules()]),
    lists:foreach(
      fun({Module, Record}) ->
              {module, ModName} = ?GRAPH:data(Module),
              {record, RecName} = ?GRAPH:data(Record),
              R = ets:lookup(Table, {ModName, RecName}),
              if R == [] ->
                      ets:insert(Table, {{ModName, RecName}, 1});
                 true ->
                      [{_, Count}] = R,
                      ets:insert(Table, {{ModName, RecName}, Count+1})
              end
      end,
      L).

modules() ->
    ?GRAPH:path(?GRAPH:root(), [file, moddef]).

function_calls(Module) ->
    ?GRAPH:path(Module, [func, {fundef,back}, funcl, {scope, back}, visib,
                         {sup, back}, funref]).  

record_refs(Module) ->
    ?GRAPH:path(Module, [func, {fundef,back}, funcl, {scope, back}, visib,
                         {sup, back}, recref]).  

