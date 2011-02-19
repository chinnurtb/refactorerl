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
-vsn("$Rev: 5296 $").

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
              CallerMod = ?Mod:name(Module),
              CalledMod =
                  ?Mod:name(lists:nth(1,?Query:exec(Call, [{func, back}]))),
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
              ModName = ?Mod:name(Module),
              RecName = ?Rec:name(Record),
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
    ?Query:exec([file, moddef]).

function_calls(Module) ->
    ?Query:exec(Module, 
    	%?Query:seq([func, {fundef,back}, funcl, {scope, back}, visib, {top, back}], ?Query:all([funeref, funlref])))
    	?Query:seq([?Mod:locals(), ?Fun:definition(), ?Form:clauses(), ?Clause:subscopes(), ?Clause:exprs(), 
    				?Expr:top_sub(), ?Expr:function()])).  

record_refs(Module) ->
    %?Query:exec(Module, [func, {fundef,back}, funcl, {scope, back}, visib, {top, back}, recref]).  
    ?Query:exec(Module, ?Query:seq([?Mod:locals(), ?Fun:definition(), ?Form:clauses(), ?Clause:subscopes(), 
    								?Clause:exprs(), ?Expr:top_sub(), ?Expr:record()])).



%% -------------------------------------------------------------------
%% Function-function and function-record dependencies

%% @spec create_ffdg(TableID::EtsTableID) -> ok
%% @doc  Collect the function-function dependences into the `TableID' ETS table.
%%       The dependencies will placed into the table in the 
%%       `{{Entity1, Entity2}, Count}' format.
create_ffdg(TableID) ->
    deps_to_ets(TableID,lists:flatmap(fun fun_fun_deps/1,functions()),true).

%% @spec create_ffdg_matrix() -> ok
%% @doc  Collect the function-function dependences into a matrix.
%%       The dependencies will placed into the matrix in the 
%%       `{{Entity1, Entity2}, Count}' format.
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
            ModName = ?Mod:name(ModNode),
            [{ModName,FuncNode} || FuncNode <- ?Query:exec(ModNode, ?Mod:locals())]
        end,
        modules()).


%% @spec fun_fun_deps({ModName::atom(), FunNode::node()}) -> 
%%           [{Function1::fun_attr(), Function2::fun_attr()}]
%% @doc  Calculate the function-function dependencies.
fun_fun_deps({ModName,FunNode}) -> 
    FunName = ?Fun:name(FunNode),
    FunArity = ?Fun:arity(FunNode),
    lists:map(
        fun(FunRefNode) ->
            [ModRefNode] = ?Query:exec(FunRefNode, ?Fun:module()),
            ModRefName = ?Mod:name(ModRefNode),
            FunRefName = ?Fun:name(FunRefNode), 
            FunRefArity = ?Fun:arity(FunRefNode),
            {#fun_attr{mod=ModName, name=FunName, arity=FunArity}, 
             #fun_attr{mod=ModRefName, name=FunRefName, arity=FunRefArity}}
        end,
        ?Query:exec(FunNode, ?Query:seq([?Fun:definition(), ?Form:clauses(), ?Clause:subscopes(), ?Clause:exprs(), 
    				?Expr:top_sub(), ?Expr:function()]))
    ).


%% @spec fun_rec_deps({ModName::atom(), FunNode::node()}) -> 
%%           [{Function::fun_attr(), {RefModName::atom(),RefRecordName::atom()}}]
%% @doc  Calculate the function-record dependencies.
fun_rec_deps({ModName,FunNode}) -> 
    FunName = ?Fun:name(FunNode), 
    FunArity = ?Fun:arity(FunNode),
    lists:map(
        fun(RecRefNode) ->
            ModRefNode = get_rec_module(RecRefNode),
            ModRefName = ?Mod:name(ModRefNode),
            RecRefName = ?Rec:name(RecRefNode),
            {#fun_attr{mod=ModName, name=FunName, arity=FunArity}, 
             {ModRefName, RecRefName}}
        end,
        lists:usort(?Query:exec(FunNode, ?Query:seq([?Fun:definition(), ?Form:clauses(), ?Clause:subscopes(), 
        											 ?Clause:exprs(), ?Expr:top_sub(), ?Expr:record()])))
    ).


%% @spec get_rec_module(RecNode::node()) -> ModuleNode::node()
%% @doc  Give back the module node that the `RecNode' is belong to. The 
%%       `RecNode' is also may be a semantical record node or a semantical 
%%       record field node.
get_rec_module(RecNode) ->
    case element(2,RecNode) of
        record -> hd( ?Query:exec(RecNode, ?Query:seq([?Rec:file(), ?File:included(), ?File:module()])));
        field  -> hd( ?Query:exec(RecNode, ?Query:seq([?RecField:recorddef(), ?Rec:file(), ?File:included(), ?File:module()])))
    end.

