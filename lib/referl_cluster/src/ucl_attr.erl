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

%%% ============================================================================
%%% Module Informations

%%% @doc Library to manage attribute matrix of the clustering state.
%%% The attribute matrix store the connection between the entities and the
%%% attributes of the clustering process. 
%%% This matrix can be understanded as a dependency matrix too.
%%% The row labels are the entity identifiers and the column labels are the
%%% attribute identifiers. A cell value describe the connection between the 
%%% corresponfding entity and attribute items. The connection value can be any
%%% term which describe the relation between entities and attributes. You can
%%% fill the attribute matrix with that terms whiches are good for your specific
%%% clustering algorithm.
%%%
%%% This library contains functions for determine the entities, calculate the
%%% dependent attributes and the dependency value (same as the connection
%%% value). Once, the dependencies are generated the attribute matrix can be
%%% built. But before the building it should filter the dependencies and only
%%% the remain connectios build the matrix.

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_attr).
-vsn("$Rev:  $").

-include("ucluster.hrl").


%%% ============================================================================
%%% Imports/exports

% Boxing functions
-export([node2item/1, id2shortname_gen/1, get_node_modname/1, get_node_file/1]).
% Collect inital entities
-export([ent_node_queries/0,
         ent_node_gen/1, ent_node_gen/2, ent_node_gen/3]).
% Dependencies
-export([node_node_dep_queries/0,
         node_node_dep_gen/2, node_node_dep_gen/3, node_node_dep_gen/4]).
% Dependency filters
-export([filter_propvalue/3, dep_filter_modname_gen/1]).



%%% ============================================================================
%%% Boxing functions

%% @spec node2item(Node::node()) -> clItem()
%% @doc Box a ESG node into a {@type clItem} record.
%% The `id' field is the `Node' itself. 
%% The `{node,Node}, {file,FileNode}, {filepath,FilePath}' properties are
%% are stored in the `props' field.
%% The `type' field is the type of the `Node' which isn't equal with the class
%% of the `Node'. Currently the following type of nodes are supported:
%% <ul>
%%   <li>`module': a `module' node.
%%     The `name' field is the module name.
%%     Additional property is the `{modname,ModuleName}'.</li>
%%   <li>`function': a `fun' node.
%%     The `name' field is a string like 
%%     "ModoleName:FunctionName/FunctionArity".
%%     Additional properties are the `{modname,ModuleName},
%%     {funname,FunctionName}, {funarity,FunctionArity}'.</li>
%%   <li>`record': a `record' node.
%%     The `name' field is a string like "FilePath:RecordName".
%%     Additional properties are the `{recname,RecordName}'.</li>
%%   <li>`macro': a `form' node with `type=macro' tag.
%%     The `name' field is a string like "FilePath:MacroName".
%%     Additional properties are the `{macroname,MacroName}'.</li>
%% </ul>
node2item(Node) when is_tuple(Node) ->
    node2item_(?Graph:data(Node), Node, get_node_file(Node)).

% Implementation of node2item/1
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
node2item_(#form{type='record', tag=RecName}, Node, {FileNode, FilePath}) ->
    #clItem{id=Node, type=record,
        name=?MISC:format("~s:~p", [FilePath, RecName]),
        props=[{recname,RecName}, 
               {node, Node}, {file,FileNode}, {filepath,FilePath}]};
node2item_(#form{type='macro', tag=MacroName}, Node, {FileNode, FilePath}) ->
    #clItem{id=Node, type=macro,
        name=?MISC:format("?~s:~p", [FilePath, MacroName]),
        props=[{macroname,MacroName}, 
               {node, Node}, {file,FileNode}, {filepath,FilePath}]}.


%% @spec id2shortname_gen(State::clState()) -> Id2ShortNameFun
%%       Id2ShortNameFun = ((ItemId::term()) -> string())
%% @doc Return a function expression that map the item identifiers to minimal
%%      length and uniqe names. The common prefix of the full name of items
%%      are removed. Practically the cut is happening on directory separator
%%      character like `\' and `/'.
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
        ?ClState:get_items(?ClState:itemIds(St), St)),
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


%% @spec get_node_modname(Node::node()) -> ModuleName::atom()
%% @doc Get the module name for `Node'. Currentlly the module, function,
%%      record and macro nodes are supported.
get_node_modname(Node) when is_tuple(Node) ->
    ModQuery = 	case ?Syn:class(Node) of
    				module 	-> 	[];
    				func	-> 	?Fun:module();
    				form	-> 	case ?Form:type(Node) of
    								record	->	?Query:seq(?Rec:file(),   ?File:module());
    								macro	->	?Query:seq(?Macro:file(), ?File:module())
    							end;
    				Class	->	throw(?LocalError(todo, ?MISC:format("module name detection "
                        "for ~p class nodes", [Class])))
    			end,
    
    ModNode = case ?Query:exec(Node, ModQuery) of
        [MN1] -> MN1;
        MN2   -> MN2
    end,
    ?Mod:name(ModNode).


%% @spec get_node_file(Node::node()) -> {FileNode_or_undef, FilePath}
%%       FileNode_or_undef = FileNode::node() | undefined
%% @doc Get the file node and the file path for `Node'. Currentlly the module,
%%      function, record and macro nodes are supported.
get_node_file(Node) when is_tuple(Node) ->
    FileQuery =	case ?Syn:class(Node) of
    				module	->	?Mod:file();
    				func	->	?Query:seq(?Fun:module(), ?Mod:file());
    				form	->	case ?Form:type(Node) of
    								record	->	?Rec:file();
    								macro	->	?Macro:file()
    							end;
    				Class  -> throw(?LocalError(todo, ?MISC:format("file detection "
                        "for ~p class nodes", [Class])))
    			end,
    
    case ?Query:exec(Node, FileQuery) of
        [FileNode] -> {FileNode, ?File:path(FileNode)};
        []         -> {?UNDEF,   ""}
    end.



%%% ============================================================================
%%% Collect initial entities

%% @spec ent_node_queries() -> [{EntType::atom(), Query::query()}]
%% @doc Return a `query()' for every supported entity type which collect all
%% corresponded type of entities when execute it from the root node.
%% Currentlly the file, module, function, record and macro nodes are supported.
ent_node_queries() ->
    [{file,     files()},
     {module,   modules()},
     {function, ?Query:seq(modules(), ?Mod:locals())  },
     {record,   ?Query:seq(files(),   ?File:records())},
     {macro,    ?Query:seq(files(),   ?File:macros()) }].

% Query all of specified nodes
files()     -> [file].
modules()   -> ?Mod:all().

%
fileMods()  -> ?Query:seq(?File:included(), ?File:module()).


%% @spec ent_node_gen(EntTypes::[atom()]) -> EntFun
%%       EntFun = (() -> [clItem()])
%% @doc Create a function expression which generate the `EntTypes' type
%%      entities.
%%      The {@link nide2item/1} function will be used to box the nodes.
%%      For the avaible type of entities see {@link ent_node_queries/0}.
ent_node_gen(EntClasses) ->
    ent_node_gen(ent_node_queries(), EntClasses, fun node2item/1).

%% @spec ent_node_gen(EntTypes::[atom()], BoxFun::BoxFun) -> EntFun
%%       EntFun = (() -> [clItem()])
%%       BoxFun = ((Node::node()) -> clItem())
%% @doc Create a function expression which generate the `EntTypes' type
%%      entities. 
%%      The `BoxFun' function will be used to box the nodes.
%%      For the avaible type of entities see {@link ent_node_queries/0}.
ent_node_gen(EntClasses, BoxFun) ->
    ent_node_gen(ent_node_queries(), EntClasses, BoxFun).

%% @spec ent_node_gen(EntNodeQueries::[EntQuery], EntTypes::[atom()],
%%               BoxFun::BoxFun) -> EntFun
%%       EntQuery = {EntType::atom(), Query::query()}
%%       EntFun = (() -> [clItem()])
%%       BoxFun = ((Node::node()) -> clItem())
%% @doc Create a function expression which generate the `EntTypes' type
%%      entities.
%%      `EntNodeQueries' proplist contains the root queries for different
%%      entity types.
%%      The `BoxFun' function will be used to box the nodes.
ent_node_gen(EntNodeQueries, EntClasses, BoxFun) when is_function(BoxFun,1) ->
    % Select only wanted node types
    {_EntClasses, EntQueries1} = 
        lists:unzip(lists:filter(
            fun({EntClass, _Query}) -> lists:member(EntClass, EntClasses) end,
            EntNodeQueries)),
    % Entity node generation function: query just the asked nodes
    fun() ->
        lists:map(BoxFun, ?Query:exec(?Query:all(EntQueries1)))
    end.



%%% ============================================================================
%%% Dependencies

%% @spec node_node_dep_queries() ->
%%           [{EntType::atom(), AttrType::atom(), Query::query()}]
%% @doc Return a `query()' for every supported type of dependencytype between
%%      entities and attributes. These queries collect all dependent attribute
%%      for an entity executed from the entity node.
%%      Currently the function-function, function-record, function-macro
%%      dependencies are supported.
node_node_dep_queries() ->
    [% Module -> X dependencies
     % TODO: {module, module,   ???},
     % TODO: {module, function, ???},
     % TODO: {module, record,   ???},
     % TODO: {module, macro,    ???},
     % Function -> X dependencies
     % TODO: {function, module, ???},
     {function, function,
        ?Query:seq([?Fun:definition(), ?Form:clauses(), ?Clause:subscopes(), ?Clause:exprs(), 
    				?Expr:top_sub(), ?Expr:function()])},
     {function, record,
        ?Query:seq([?Fun:definition(), ?Form:clauses(), ?Clause:subscopes(), ?Clause:exprs(), 
    				?Expr:top_sub(), ?Query:any(?Expr:record(), ?Query:seq(?Expr:field(),?RecField:recorddef()))])},
     {function, macro, % Is there a simplier way ???
        ?Query:seq([
            ?Fun:definition(),
            ?Query:all([
                [flex],
                ?Query:seq([
                	?Form:clauses(), ?Clause:subscopes(),
                    ?Query:all(?Query:seq(?Clause:exprs(),?Expr:top_sub(),elex), [clex]) ]) ]),
            [llex,mref] ]) }
     % Record -> X dependencies
     % TODO: {record, module, ???},
     % {record, function,
     %   [{recdef,back}, attr, {top,back}, funeref, funlref]},
     % {record, record,
     %   ?Query:seq([{fundef,back}, funcl, {scope,back}, visib, {top,back}],
     %              ?Query:any([recref], [fieldref, {field,back}]))},
     % {record, macro,
     %   [{recdef,back}, attr, {top,back}, elex, llex, mref]},
     % Macro -> X dependencies
     % TODO: {macro, module,   ???},
     % TODO: {macro, function, ???},
     %{macro, record,   ?Macro:records()},
     %{macro, macro,    ?Macro:macros()}
    ].

    
%% @spec node_node_dep_gen(EntTypes::[atom()], AttrTypes::[atom()]) -> DepFun
%%       DepFun = ((Item::clItem()) -> [Dependency])
%%       Dependency = {Item::clItem(), DepItem::clItem(), Weight::term()}
%% @doc Create a function expression which generate the `AttrTypes' type
%%      dependent attributes for `EntTypes' type entities.
%%      The {@link nide2item/1} function will be used to box the nodes.
%%      For the avaible type of entities see {@link node_node_dep_queries/0}.
node_node_dep_gen(EntClasses, AttrClasses) ->
    node_node_dep_gen(node_node_dep_queries(), EntClasses, AttrClasses, 
                      fun node2item/1).

%% @spec node_node_dep_gen(EntTypes::[atom()], AttrTypes::[atom()],
%%               BoxFun::BoxFun) -> DepFun
%%       BoxFun = ((Node::node()) -> clItem())
%%       DepFun = ((Item::clItem()) -> [Dependency])
%%       Dependency = {Item::clItem(), DepItem::clItem(), Weight::term()}
%% @doc Create a function expression which generate the `AttrTypes' type
%%      dependent attributes for `EntTypes' type entities.
%%      The `BoxFun' function will be used to box the nodes.
%%      For the avaible type of entities see {@link node_node_dep_queries/0}.
node_node_dep_gen(EntClasses, AttrClasses, BoxFun) ->
    node_node_dep_gen(node_node_dep_queries(), EntClasses, AttrClasses, 
                      BoxFun).

%% @spec node_node_dep_gen(NodeNodeDepQueries::[DepQuery], EntTypes::[atom()],
%%               AttrTypes::[atom()], BoxFun::BoxFun) -> DepFun
%%       DepQuery = {EntType::atom(), AttrType::atom(), Query::query()}
%%       BoxFun = ((Node::node()) -> clItem())
%%       DepFun = ((Item::clItem()) -> [Dependency])
%%       Dependency = {Item::clItem(), DepItem::clItem(), Weight::term()}
%% @doc Create a function expression which generate the `AttrTypes' type
%%      dependent attributes for `EntTypes' type entities.
%%      `NodeNodeDepQueries' proplist contains the queries for different
%%      entity and attribute type pairs.
%%      The `BoxFun' function will be used to box the nodes.
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



%%% ============================================================================
%%% Dependency filters

%% @spec filter_propvalue(Item::clItem(), PropKey::atom(), 
%%               PropValues::[term()]) -> bool()
%% @doc  Return `true' if the `PropKey' property value of `Item' is in the
%%       `PropValues' list.
filter_propvalue(#clItem{props=Props}, PropKey, PropValues) when 
        is_atom(PropKey), is_list(PropValues) ->
    lists:member(get_value(PropKey, Props), PropValues).


%% @spec dep_filter_modname_gen(ModuleNames::[atom()]) -> 
%%           DepFilterFun
%%       DepFilterFun = ((Dep::Dependency) -> bool())
%%       Dependency = {Item::clItem(), DepItem::clItem(), Weight::term()}
%% @doc  Generate a function which filter the dependencies by the module name
%%       of the nodes. 
%%       The dependency filter function return `true' if possible to include 
%%       the file of the `Item1' or `Item2' into any module from the 
%%       `ModuleNames' list. `ModuleNames' contains atoms as module names.
%%       The file of an item is determined by the file node that is stored 
%%       in the `file' property.
dep_filter_modname_gen(ModuleNames) when is_list(ModuleNames) ->
    fun({#clItem{props=Props1}, #clItem{props=Props2}, _W}) ->
        ModNodes1 = case get_value(file, Props1, ?UNDEF) of
            ?UNDEF    -> [];
            FileNode1 -> ?Query:exec(FileNode1, fileMods())
        end,
        ModNodes2 = case get_value(file, Props2, ?UNDEF) of
            ?UNDEF    -> [];
            FileNode2 -> ?Query:exec(FileNode2, fileMods())
        end,
        ModNames1 = lists:usort(lists:map(fun get_node_modname/1, ModNodes1)),
        ModNames2 = lists:usort(lists:map(fun get_node_modname/1, ModNodes2)),
        % If any node in a specified module the dependency is kept
        [] /= ?MISC:intersect(ModNames1, ModuleNames) orelse
         []/= ?MISC:intersect(ModNames2, ModuleNames)
    end.


