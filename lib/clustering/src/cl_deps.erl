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
-vsn("$Rev: 2170 $").

-include("cluster.hrl").

%% =============================================================================
%% Exports

-export([insert_function_calls/1, insert_record_refs/1, 
         create_ffdg/1, create_frdg/1]).

-export([create_ffdg_matrix/0]).



%% =============================================================================

%% -------------------------------------------------------------------
%% Module-module and module-record dependencies

%% Table: ets({CallerMod, CalledMod}, ok)

%% @spec insert_function_calls(Table::EtsTableID) -> ok
%% @doc  Collect the module-module dependences by function calls into the 
%%       `Table' ETS table.
%%       The dependencies will placed into the table in the 
%%       `{{Entity1, Entity2}, ok}' format.
insert_function_calls(Table) ->
    L = lists:flatten([[{Module, Call} || Call <- function_calls(Module)] 
                       || Module <- modules()]),
    lists:foreach(
      fun({Module, Call}) ->
              %{module, CallerMod} = ?ESG:data(Module),
              %{module, CalledMod} = 
              #module{name=CallerMod} = ?ESG:data(Module),
              #module{name=CalledMod} =
                  ?ESG:data(lists:nth(1,?ESG:path(Call, [{func, back}]))),
              ets:insert(Table, {{CallerMod, CalledMod}, ok})
      end,
      L).


%% needed: Table: ets({Module1, Module2}, ok)
%% currently: Table: ets({ModuleName, RecordName}, Count)

%5 @spec insert_record_refs(Table::EtsTableID) -> ok
%% @doc  Collect the module-record dependences by record references into the 
%%       `Table' ETS table.
%%       The dependencies will placed into the table in the 
%%       `{{Entity1, Entity2}, Count}' format.
insert_record_refs(Table) ->
    L = lists:flatten([[{Module, Call} || Call <- record_refs(Module)] 
                       || Module <- modules()]),
    lists:foreach(
      fun({Module, Record}) ->
              %{module, ModName} = ?ESG:data(Module),
              %{record, RecName} = ?ESG:data(Record),
              #module{name=ModName} = ?ESG:data(Module),
              #record{name=RecName} = ?ESG:data(Record),
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
    ?ESG:path(?ESG:root(), [file, moddef]).

function_calls(Module) ->
    ?ESG:path(Module, [func, {fundef,back}, funcl, {scope, back}, visib,
                         {sup, back}, funref]).  

record_refs(Module) ->
    ?ESG:path(Module, [func, {fundef,back}, funcl, {scope, back}, visib,
                         {sup, back}, recref]).  



%% -------------------------------------------------------------------
%% Function-function and function-record dependencies

%% @spec create_ffdg(TableID::EtsTableID) -> ok
%% @doc  Collect the function-function dependences into the `TableID' ETS table.
%%       The dependencies will placed into the table in the 
%%       `{{Entity1, Entity2}, Count}' format.
create_ffdg(TableID) ->
    deps_to_ets(TableID,lists:flatmap(fun fun_fun_deps/1,functions()),true).

create_ffdg_matrix() ->
    deps_to_matrix(lists:flatmap(fun fun_fun_deps/1,functions()),true).

    
%% @spec create_frdg(TableID::EtsTableID) -> ok
%% @doc  Collect the function-record dependences into the `TableID' ETS table.
%%       The dependencies will placed into the table in the 
%%       `{{Entity1, Entity2}, Count}' format.
create_frdg(TableID) ->
    deps_to_ets(TableID,lists:flatmap(fun fun_rec_deps/1,functions()),true).


%% @spec deps_to_ets(TableID::EtsTableID, 
%%           DepPairs::[{Entity1::ClusteringEntity, Enrity::ClusteringEntity}], 
%%           Countable::bool()) -> ok
%% @doc Store a dependency lists into the `TableID' ets table. 
%%      The dependencies will placed into the table in the 
%%      `{{Entity1, Entity2}, Count}' format. If `Countable' is `true' the 
%%      `Count' contain the number of same dependency else simly `ok'.
%deps_to_ets(TableID, DepPairs, Countable) -> 
%    lists:foreach(
%        fun({Entity, RefObject}) ->
%            Count = case ets:lookup(TableID, {Entity,RefObject}) of
%                [{_,C}] -> C;
%                _       -> 0
%            end,
%            if
%                Countable -> ets:insert(TableID, {{Entity,RefObject}, Count+1});
%                true      -> ets:insert(TableID, {{Entity,RefObject}, ok})
%            end
%        end,
%        DepPairs).

deps_to_ets(TableID, DepPairs, Countable) ->
    lists:foreach(
        fun({{Entity,RefObject}, Weight}) ->
            if
                Countable -> ets:insert(TableID, {{Entity,RefObject}, Weight});
                true      -> ets:insert(TableID, {{Entity,RefObject}, ok})
            end
        end,
        ?CMISC:list_cnt(DepPairs)).


deps_to_matrix(DepPairs, Countable) ->
    {L1, L2} = lists:unzip(DepPairs),
    Entities = lists:usort(L1++L2),
    lists:foldl(
        fun({{Entity, RefObject}, Weight}, DepMtx) ->
            if
                Countable -> cl_matrix:set(Entity, RefObject, Weight, DepMtx);
                true      -> cl_matrix:set(Entity, RefObject, ok,     DepMtx)
            end
        end,
        cl_matrix:new(Entities, Entities, no),
        ?CMISC:list_cnt(DepPairs)).


%% @spec functions() -> [{ModuleName::atom(), FunctionNode::node()}]
%% @doc  Collect all function from the graph.
functions() ->
    lists:flatmap(
        fun(ModNode) ->
            #module{name=ModName} = ?Graph:data(ModNode),
            [{ModName,FuncNode} || FuncNode <- ?Graph:path(ModNode,[func])]
        end,
        modules()).


%% @spec fun_fun_deps({ModName::atom(), FunNode::node()}) -> 
%%           [{Function1::fun_attr(), Function2::fun_attr()}]
%% @doc  Calculate the function-function dependencies.
fun_fun_deps({ModName,FunNode}) -> 
    #func{name=FunName, arity=FunArity} = ?Graph:data(FunNode),
    lists:map(
        fun(FunRefNode) ->
            [ModRefNode] = ?Graph:path(FunRefNode, [{func,back}]),
            #module{name=ModRefName} = ?Graph:data(ModRefNode),
            #func{name=FunRefName, arity=FunRefArity} = ?Graph:data(FunRefNode),
            {#fun_attr{mod=ModName, name=FunName, arity=FunArity}, 
             #fun_attr{mod=ModRefName, name=FunRefName, arity=FunRefArity}}
        end,
        ?ESG:path(FunNode, [{fundef,back}, funcl, {scope,back}, visib, 
                             {sup,back}, funref])
    ).


%% @spec fun_rec_deps({ModName::atom(), FunNode::node()}) -> 
%%           [{Function::fun_attr(), {RefModName::atom(),RefRecordName::atom()}}]
%% @doc  Calculate the function-record dependencies.
fun_rec_deps({ModName,FunNode}) -> 
    #func{name=FunName, arity=FunArity} = ?Graph:data(FunNode),
    lists:map(
        fun(RecRefNode) ->
            ModRefNode = get_rec_module(RecRefNode),
            #module{name=ModRefName} = ?Graph:data(ModRefNode),
            #record{name=RecRefName} = ?Graph:data(RecRefNode),
            {#fun_attr{mod=ModName, name=FunName, arity=FunArity}, 
             {ModRefName, RecRefName}}
        end,
        lists:usort(?ESG:path(FunNode, [{fundef,back}, funcl, {scope,back}, 
                                        visib, {sup,back}, recref]))
    ).


%% @spec get_rec_module(RecNode::node()) -> ModuleNode::node()
%% @doc  Give back the module node that the `RecNode' is belong to. The 
%%       `RecNode' is also may be a semantical record node or a semantical 
%%       record field node.
get_rec_module(RecNode) ->
    case ?GR_UTILS:get_node_type(RecNode) of
        record -> hd( ?Graph:path(RecNode, [{record,back},{incl,back},moddef]));
        field  -> hd( ?Graph:path(RecNode, [{field,back},{record,back},
                                            {incl,back},moddef]))
    end.

