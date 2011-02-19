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

%%% @doc Building attribute matrix
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(kcl_attr).
-vsn("$Rev:  $").

-include("kcl.hrl").


%%% ============================================================================
%%% Imports/exports

%Imports
-import(proplists, [get_value/2, get_value/3]).


% Boxing functions
-export([node2item/1, id2shortname_gen/1, get_node_modname/1]).
% Collect inital items
-export([ent_node_queries/0, ent_node_query_gen/1, ent_node_query_gen/2,
         query2items/2, query2items/3]).
% Dependencies
-export([node_node_depQueries/0, node_node_dep_gen/2, node_node_dep_gen/4,
         filter_propvalue/3, dep_filter_modname/2]).
% Building connection matrix
-export([common_attr/3]).



%%% ============================================================================
%%% Boxing functions

%%
%%
node2item(Node) when is_tuple(Node) ->
    node2item_(?Graph:data(Node), Node, get_node_file(Node)).

%
node2item_(#module{name=ModName}, Node, {FileNode, FilePath}) ->
    #clItem{id=Node, type=module,
        name=?MISC:format("~p", [ModName]),
        props=[{modname,get_node_modname(Node)}, 
               {node,Node}, {file,FileNode}, {filepath,FilePath}]};
node2item_(#func{name=FunName, arity=FunArity}, Node, {FileNode, FilePath}) ->
    ModName = get_node_modname(Node),
    #clItem{id=Node, type=function,
        name=?MISC:format("~p:~p/~p",[ModName,FunName,FunArity]),
        props=[{modname,ModName}, {funname,FunName}, {funarity,FunArity},
               {node,Node}, {file,FileNode}, {filepath,FilePath}]};
node2item_(#record{name=RecName}, Node, {FileNode, FilePath}) ->
    #clItem{id=Node, type=record,
        name=?MISC:format("~s:~p", [FilePath, RecName]),
        props=[{recname,RecName}, 
               {node, Node}, {file,FileNode}, {filepath,FilePath}]};
node2item_(#form{type='macro', tag=MacroName}, Node, {FileNode, FilePath}) ->
    #clItem{id=Node, type=macro,
        name=?MISC:format("?~s:~p", [FilePath, MacroName]),
        props=[{macroname,MacroName}, 
               {node, Node}, {file,FileNode}, {filepath,FilePath}]}.


%%
%%
id2shortname_gen(St=#clState{}) ->
    % Separate names, which are started with filepath, from the others
    {MFPairs, RMPairs} = lists:foldl(
        fun(I, {AccMF, AccRM}) -> 
            case lists:member(I#clItem.type, [record, macro]) of
                true -> {AccMF, [{I#clItem.id, I#clItem.name}|AccRM]};
                _    -> {[{I#clItem.id, I#clItem.name}|AccMF], AccRM}
            end
        end,
        {[], []},
        ?ClState:get_items(?ClState:get_itemIds(St), St)),
    % Find common path
    {RMIds, RMNames} = lists:unzip(RMPairs),
    RMNameParts = lists:map(
        fun(Name) -> 
            element(1,?MISC:string_split(Name, ["\\","/"], 0, false, false))
        end,
        RMNames),
    {_RMNamePartCommon, RMNamePartTails} = ?MISC:list_compareL(RMNameParts),
    % RMNameCommon = ?MISC:flatjoin(RMNamePartCommon, "/")++"/",
    RMNameTails = lists:map(fun(PartTail) -> ?MISC:flatjoin(PartTail, "/") end,
                            RMNamePartTails),
    % Remove common path from names
    Pairs = MFPairs ++ lists:zip(RMIds, RMNameTails),
    % Generate id2name function
    fun(Id) -> get_value(Id, Pairs) end.


%%
%%
get_node_modname(Node) when is_tuple(Node) ->
    ModQuery = case ?Graph:class(Node) of
        module -> [];
        func   -> ?Fun:module();
        record -> ?Query:seq(?Rec:file(),   ?File:module());
        macro  -> ?Query:seq(?Macro:file(), ?File:module());
        Class  -> throw(?LocalError(todo, ?MISC:format("module name detection "
                        "for ~p class nodes", [Class])))
    end,
    ModNode = case ?Query:exec(Node, ModQuery) of
        [MN1] -> MN1;
        MN2   -> MN2
    end,
    (?Graph:data(ModNode))#module.name.


%%
%%
get_node_file(Node) when is_tuple(Node) ->
    FileQuery = case ?Graph:class(Node) of
        module -> ?Mod:file();
        func   -> ?Query:seq(?Fun:module(), ?Mod:file());
        record -> ?Rec:file();
        macro  -> ?Macro:file();
        Class  -> throw(?LocalError(todo, ?MISC:format("file detection "
                        "for ~p class nodes", [Class])))
    end,
    case ?Query:exec(Node, FileQuery) of
        [FileNode] -> {FileNode, (?Graph:data(FileNode))#file.path};
        []         -> {no, ""}
    end.




%%% ============================================================================
%%% Building attribute matrix

% Query all of specified nodes
files()     -> [file].
modules()   -> [module].
% functions() -> ?Query:seq(modules(), ?Mod:locals()).
% records()   -> ?Query:seq(files(), ?File:records()).
% macros()    -> ?Query:seq(files(), ?File:macros()).

%
fileMods()  -> ?Query:seq(?File:included(), ?File:module()).


%%
%%
ent_node_queries() ->
    [{file,     files()},
     {module,   modules()},
     {function, ?Query:seq(modules(), ?Mod:locals())},
     {record,   ?Query:seq(files(), ?File:records())},
     {macro,    ?Query:seq(files(), ?File:macros())}].

%%
%%
ent_node_query_gen(EntClasses) ->
    ent_node_query_gen(ent_node_queries(), EntClasses).

%%
%%
ent_node_query_gen(EntNodeQueries, EntClasses) when
        is_list(EntNodeQueries), is_list(EntClasses) ->
    % Select only wanted dependency types
    {_EntClasses, EntQueries1} = lists:unzip(lists:filter(
        fun({EntClass, _Query}) -> lists:member(EntClass, EntClasses) end,
        EntNodeQueries)),
    % Dependency function: compute just the allowed dependencies
    ?Query:all(EntQueries1).


%%
%%
query2items(Node, Query) ->
    query2items(Node, Query, fun node2item/1).

%%
%%
query2items(Node, Query, BoxFun) when is_function(BoxFun) ->
    lists:map(BoxFun, ?Query:exec(Node, Query)).


%%
%%
node_node_depQueries() ->
    [% Module -> X dependencies
     % TODO: {module, module,   ???},
     % TODO: {module, function, ???},
     % TODO: {module, record,   ???},
     % TODO: {module, macro,    ???},
     % Function -> X dependencies
     % TODO: {function, module, ???},
     {function, function,
        [{fundef,back}, funcl, {scope,back}, visib, {sup,back}, funref]},
     {function, record,
        ?Query:seq([{fundef,back}, funcl, {scope,back}, visib, {sup,back}],
                   ?Query:any([recref], [fieldref, {field,back}]))},
     {function, macro, % Are there a simplier way ???
        ?Query:seq([
            [{fundef,back}],
            ?Query:all([
                [flex],
                ?Query:seq([
                    [funcl,{scope,back}],
                    ?Query:all([[visib,{sup,back},elex], [clex]]) ]) ]),
            [llex,mref] ]) }
     % Record -> X dependencies
     % TODO: {record, module, ???},
     % {record, function,
     %   [{recdef,back}, attr, {sup,back}, funref]},
     % {record, record,
     %   ?Query:seq([{fundef,back}, funcl, {scope,back}, visib, {sup,back}],
     %              ?Query:any([recref], [fieldref, {field,back}]))},
     % {record, macro,
     %   [{recdef,back}, attr, {sup,back}, elex, llex, mref]},
     % Macro -> X dependencies
     % TODO: {macro, module,   ???},
     % TODO: {macro, function, ???},
     %{macro, record,   ?Macro:records()},
     %{macro, macro,    ?Macro:macros()}
    ].

    
%%
%%
node_node_dep_gen(EntClasses, AttrClasses) ->
    node_node_dep_gen(node_node_depQueries(), EntClasses, AttrClasses, 
                      fun node2item/1).

%%
%%
node_node_dep_gen(NodeNodeDepQueries, EntClasses, AttrClasses, BoxFun) when
        is_list(NodeNodeDepQueries), is_list(EntClasses), is_list(AttrClasses),
        is_function(BoxFun) ->
    % Select only wanted dependency types
    NodeNodeDepQueries1 = lists:filter(
        fun({EntClass, AttrClass, _DepQuery}) ->
            lists:member(EntClass, EntClasses) andalso
            lists:member(AttrClass, AttrClasses)
        end,
        NodeNodeDepQueries),
    % Create dictionary for dependency queries ({node class, query})
    QueryDict0 = lists:foldl(
        fun({Class1,_C2,Query}, Dict) -> dict:append(Class1, Query, Dict) end,
        dict:new(),
        NodeNodeDepQueries1),
    QueryDict = dict:map(fun(_Class, Queries) -> ?Query:all(Queries) end,
        QueryDict0),
    % Dependency function: compute just the allowed dependencies
    fun(Item=#clItem{type=Type, props=Props}) ->
        Node = get_value(node, Props),
        DepNodeCnts = case dict:find(Type, QueryDict) of
            {ok, Query} -> ?MISC:list_cnt(?Query:exec(Node, Query));
            _           -> []
        end,
        lists:map(fun({DepNode,DepCnt}) -> {Item, BoxFun(DepNode), DepCnt} end,
                  DepNodeCnts)
    end.


%% @spec filter_propvalue(Item::clItem(), PropKey::atom(), 
%%               PropValues::[term()]) -> bool()
%% @doc  Return `true' if the `PropKey' property value of `Item' is in the
%%       `PropValues' list.
filter_propvalue(#clItem{props=Props}, PropKey, PropValues) when 
        is_atom(PropKey), is_list(PropValues) ->
    lists:member(get_value(PropKey, Props), PropValues).


%% @spec dep_filter_modname(Dependency, ModuleNames::[atom()]) -> bool()
%%       Dependency = {Item::clItem(), DepItem::clItem(), Weight::term()}
%% @doc  Return `true' if possible to include the file of the `Item1' or
%%       `Item2' into any module from the `ModuleNames' list. `ModuleNames'
%%       contains atoms. The file of an item is determined by the file node 
%%       that is stored in the `file' property.
dep_filter_modname({#clItem{props=Props1}, #clItem{props=Props2}, _W},
        ModuleNames) when is_list(ModuleNames) ->
    ModNodes1 = case get_value(file, Props1) of
        no -> [];
        FileNode1 -> ?Query:exec(FileNode1, fileMods())
    end,
    ModNodes2 = case get_value(file, Props2) of
        no -> [];
        FileNode2 -> ?Query:exec(FileNode2, fileMods())
    end,
    ModNames1 = lists:usort(lists:map(fun get_node_modname/1, ModNodes1)),
    ModNames2 = lists:usort(lists:map(fun get_node_modname/1, ModNodes2)),
    [] /= ?MISC:intersect(ModNames1, ModuleNames) orelse
     []/= ?MISC:intersect(ModNames2, ModuleNames).



%%% ============================================================================
%%% Building connection matrix

%%
%%
common_attr(#clItem{id=Id1}, #clItem{id=Id2}, AttrMatrix) ->
    case length(?MISC:intersect(?Matrix:get_row_non_def_ids(Id1, AttrMatrix),
            ?Matrix:get_row_non_def_ids(Id2, AttrMatrix))) of
        0 -> {def,   0};
        N -> {value, N}
    end.


