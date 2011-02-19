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
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_draw_graph).
-vsn("$Rev: 3185 $").

-include("cluster.hrl").

%% =============================================================================
%% Exports

-export([draw_clusters/2, 
         mod_to_str/1, fun_attr_to_str/1, fun_attr_to_localstr/1, 
         draw_clusters/4, draw_hac_clusters/4]).



%% =============================================================================

%% @spec draw_clusters(Iterations::[[[ClusteringEntity]]], Dir::string()) -> 
%%           NumFiles::integer()
%% @doc  Draw the result of the clusterings into `dot' graph files. Every 
%%       clusterings goes into seperate files.
draw_clusters(Iterations, Dir) ->
    file:make_dir(Dir),
    lists:foldl(
      fun(Iteration, Acc) ->
              Filename = filename:join(
                           Dir,
                           lists:flatten(io_lib:format("~3..0b.dot", [Acc]))),
              %% ...
              case file:open(Filename, [write]) of
                  {ok, Dev} ->
                      io:format(Dev, "digraph calls { overlap=scale;~n", []),
                      [module_calls(Dev, M) ||
                          M <- ?ESG:path(?ESG:root(), [file, moddef])],
                      %% TODO: lists subgraphs
                      subgraphs(Dev, Iteration),
                      io:format(Dev, "}~n", []),
                      file:close(Dev);
                  {error, Reason} ->
                      io:format("Error: ~s~n", file:format_error(Reason))
              end,
              Acc + 1
      end,
      1,
      Iterations).

module_calls(Dev, Mod) ->
    #module{name=Name} = ?ESG:data(Mod),
    io:format(Dev, "~s [label=\"~s\"];~n", [Name, Name]),
    Calls = ?ESG:path(Mod, [func, {fundef,back}, funcl, {scope, back}, visib,
                              {sup, back}, funref, {func, back}]),
    %io:format("~s: ~b~n", [Name, length(Calls)]),
    print_calls(lists:sort(Calls), Name, Dev).

print_calls([{n, Node, N}, Node | Tail], From, Dev) ->
    print_calls([{n, Node, N+1} | Tail], From, Dev);
print_calls([{n, Node, N} | Tail], From, Dev) ->
    case ?ESG:path(Node, [{moddef,back}]) of
        [] -> ok;
        _ ->
            #module{name=To} = ?ESG:data(Node),
            if
                To /= From ->
                    io:format(Dev, "~s -> ~s [label=\"~b\"];~n", [From, To, N]);
                true ->
                    ok
            end
    end,
    print_calls(Tail, From, Dev);
print_calls([Node | Tail], From, Dev) ->
    print_calls([{n, Node, 1} | Tail], From, Dev);
print_calls([], _, _) -> ok.

subgraphs(Dev, Iteration) ->
    lists:foldl(fun(Cluster, Acc) -> 
                         io:format(Dev, "subgraph cluster_~w{~n", [Acc]),
                         print_list(Dev, Cluster),
                         io:format(Dev, "}~n", []),
                         Acc + 1
                 end, 1, Iteration).

print_list(Io, [Head | Tail]) ->
    io:format(Io, "~p;~n", [Head]),
    print_list(Io, Tail);
print_list(_, []) ->
    ok.



%% -------------------------------------------------------------------
%% Generalized depending graph drawer

%% @spec mod_to_str(ModName::atom()) -> string()
%% @doc  Give back the name of module as string.
mod_to_str(ModName) ->
    ?MISC:to_list(ModName).
    
%% @spec fun_attr_to_str(FunAttr::fun_attr()) -> string()
%% @doc  Give back the FunAttr as string.
fun_attr_to_str(#fun_attr{mod=Mod, name=Name, arity=Arity}) ->
    ?MISC:format("~p:~p/~b", [Mod,Name,Arity]).

%% @spec fun_attr_to_localstr(FunAttr::fun_attr()) -> string()
%% @doc  Give back the FunAttr as string without module qualifier.
fun_attr_to_localstr(#fun_attr{name=Name, arity=Arity}) ->
    ?MISC:format("~p/~b", [Name,Arity]).


%% @spec draw_clusters(Clusterings::[[[ClusteringEntity]]], Connections, 
%%                     EntLabelFun, Dir::string()) -> NumFiles::integer()
%%       Connections = {mnesia, mnesiaTableID } | 
%%                     {matrix, matrix()} | 
%%                     {ets,    etsTableID} |
%%                     {list,   DepList}
%%       EntLabelFun = ((ClusteringEntity) -> string())
%% @doc  Draw the result of the clusterings into `dot' graph files. Every 
%%       clusterings goes into seperate files.
%%
%%       The `Connections' contains the connections between the clustering
%%       entities.
%%       The connections in the mnesia table must be described in the 
%%       `{_, {Entity, RefObject}, Weight}' format. In the `ets' and `list' 
%%       case the format is `{{Entity, RefObject}, Weight}'. The row
%%       identifications of the matrix are the entities and the column 
%%       identifications are the referenced objects.
%%
%%       `EntLabelFun' is a function that create the string representation from
%%       the original clustering enity. You can use {@link mod_to_str} for 
%%       modules and {@link fun_attr_to_str} for fun_attr.
%%
%%       `Dir' is the diractory name where the `dot' files will be generated.
%%
%%       Return with the number of created files.
%%
%% Technically note:
%%
%%       In the actually implementation the mnesia table of `Connection' is 
%%       `function_calls' for module clustering and `ffdg' for function 
%%       clustering.
%% @see  draw_hac_clusters/4
draw_clusters([], _Connections, _EntLabelFun, _Dir) -> 
    0;
draw_clusters(Clusterings, Connections, EntLabelFun, Dir) when 
        is_list(Clusterings), is_tuple(Connections), is_function(EntLabelFun), 
        is_list(Dir) ->
    % Create entity dictionary (id and label)
    EntDict = create_ent_dict(lists:flatten(hd(Clusterings)), EntLabelFun),
    ConnLst = create_conn_list(Connections),
    % Create directory
    file:make_dir(Dir),
    % Create files
    lists:foldl(
        fun(Clusters, Acc) -> 
            Filename = filename:join(Dir, ?MISC:format("~3..0b.dot", [Acc])),
            case file:open(Filename, [write]) of
                {ok, Dev} ->
                    io:format(Dev, "digraph calls { overlap=scale;\n", []),
                    io:format(Dev, " //Vertices: entities\n", []),
                    write_entities(EntDict, Dev),
                    io:format(Dev, "\n //Edges: calls\n", []),
                    write_conns(ConnLst, EntDict, Dev),
                    io:format(Dev, "\n //Subgraphs: clusters\n", []),
                    write_clusters(Clusters, EntDict, Dev, 1),
                    io:format(Dev, "}\n", []),
                    file:close(Dev);
                {error, Reason} ->
                    io:format("Error: ~s\n", file:format_error(Reason))
            end,
            Acc+1
        end,
        1,
        Clusterings
    )-1. % Number of created files


%% @spec draw_hac_clusters(Clusterings::[[[ClusteringEntity]]], 
%%                         Connections, EntLabelFun, FileName::string()) -> ok
%%       Connections = {mnesia, mnesiaTableID } | 
%%                     {matrix, matrix()} | 
%%                     {ets,    etsTableID} |
%%                     {list,   DepList}
%%       DepList = [{{Entity::term(), RefObject::term()}, Value::term()}]
%%       EntLabelFun = ((ClusteringEntity) -> string())
%% @doc  Draw the result of the clusterings into a `dot' graph file. Every 
%%       clusterings goes into a single file. 
%%
%%       The `Clusterings' must be the results of a hierarchical agglomerative 
%%       clustering (HAC) or a same herarchical divisive clustering (HDC). 
%%       If the n-th level of the clusterings contains a cluster than 
%%       the (n+1)-th level must contians the same claster or a totally
%        dijunctive division of that.
%%
%%       The `Connections' contains the connections between the clustering
%%       entities.
%%       The connections in the mnesia table must be described in the 
%%       `{_, {Entity, RefObject}, Weight}' format. In the `ets' and `list' 
%%       case the format is `{{Entity, RefObject}, Weight}'. The row
%%       identifications of the matrix are the entities and the column 
%%       identifications are the referenced objects.
%%
%%       `EntLabelFun' is a function that create the string representation from
%%       the original clustering enity. You can use {@link mod_to_str} for 
%%       modules and {@link fun_attr_to_str} for fun_attr.
%%
%%       `FileName' is the file name of the generated `dot' graph.
%%
%% Technically note:
%%
%%       In the actually implementation the mnesia table of `Connection' is 
%%       `function_calls' for module clustering and `ffdg' for function 
%%       clustering.
%% @see  draw_clusters/4
draw_hac_clusters([], _Connections, _EntLabelFun, _FileName) -> 
    ok;
draw_hac_clusters(Clusterings, Connections, EntLabelFun, FileName) when 
        is_list(Clusterings), is_tuple(Connections), is_function(EntLabelFun), 
        is_list(FileName) ->
    % Create entity dictionary (id and label)
    EntDict  = create_ent_dict(lists:flatten(hd(Clusterings)), EntLabelFun),
    ConnLst = create_conn_list(Connections),
    % Create file
    case file:open(FileName, [write]) of
        {ok, Dev} ->
            io:format(Dev, "digraph calls { overlap=scale;\n", []),
            io:format(Dev, " //Vertices: entities\n", []),
            write_entities(EntDict, Dev),
            io:format(Dev, "\n //Edges: calls\n", []),
            write_conns(ConnLst, EntDict, Dev),
            io:format(Dev, "\n //Subgraphs: clusters\n", []),
            lists:foldl(
                fun(Cluster, Idx) ->
                    write_hac_dendogramm(EntDict, Dev, 1, Idx, Cluster, 
                                         tl(Clusterings))
                end,
                1,
                hd(Clusterings)),
            io:format(Dev, "}\n", []),
            file:close(Dev);
        {error, Reason} ->
            io:format("Error: ~s\n", file:format_error(Reason))
    end.


%% @spec create_ent_dict(Entities::[ClusteringEntity], EntLabelFun) -> 
%%           EntDict::dict()
%%       EntLabelFun = ((ClusteringEntity) -> string())
%% @doc  Create a new dictionary that contain an identification string for the 
%%       entity and the string representation of it. It mean the value type of
%%       the `EntDict' is a 2-tuple. (`{EntId::string(), EntLabel()::string()}')
create_ent_dict(Entities, EntLabelFun) ->
    element(1, lists:foldl(
        fun(Entity, {Dict, EntNum}) ->
            EntId    = ?MISC:format("e~b", [EntNum]),
            EntLabel = EntLabelFun(Entity),
            {dict:store(Entity, {EntId, EntLabel}, Dict), EntNum+1}
        end,
        {dict:new(), 1},
        Entities)).


%% @spec create_conn_list(Connections) -> ConnList
%%       Connections = {mnesia, mnesiaTableID } | 
%%                     {matrix, matrix()} | 
%%                     {ets,    etsTableID} |
%%                     {list,   ConnList}
%%       ConnList = [{{Entity1::term(), Entity2::term()}, Value::term()}]
%% @doc  Create connection graph from a mnesia table or a 
%%       {@link cl_matrix:matrix()} or an ets table.
%%       The connections in the mnesia table must be described in the 
%%       `{_, {Entity, RefObject}, Weight}' format. In the `ets' and `list' 
%%       case the format is `{{Entity, RefObject}, Weight}'. The row
%%       identifications of the matrix are the entities and the column 
%%       identifications are the referenced objects.
create_conn_list({mnesia, ConnTableID}) ->
    lists:map(
        fun({_,{Entity1,Entity2},Weight}) -> {{Entity1,Entity2},Weight} end,
        mnesia:dirty_match_object(ConnTableID, {'_', {'$1','$2'}, '$3'}));
create_conn_list({matrix, Matrix}) ->
    cl_matrix:to_list(Matrix);
create_conn_list({ets, ConnTableID}) ->
    ets:tab2list(ConnTableID);
create_conn_list({list, ConnList}) ->
    ConnList.


%% @spec write_entities(EntDict::dict(), Dev) -> ok
%% @doc  Write clustering entities into the `Dev' device
%%       `EntDict' contains the id and the label of the clustering entities.
%% @see  create_ent_dict/2
%% @see  draw_clusters/4
%% @see  draw_hac_clusters/4
write_entities(EntDict, Dev) ->
    dict:fold(
        fun(Entity, {EntId, EntLabel}, _) ->
            {ok, {EntId,EntLabel}} = dict:find(Entity, EntDict),
            io:format(Dev, " ~s [label=\"~s\"];\n", [EntId, EntLabel])
        end,
        ok,
        EntDict).


%% @spec write_conns(ConnLst, EntDict::dict(), Dev) -> ok
%%       ConnList = [{{Entity1::term(), Entity2::term()}, Value::term()}]
%% @doc  Write the connections between clustering entities into the `Dev' 
%%       device. `ConnLst' is list that contain the connections.
%%       `EntDict' contains the id and the label of the clustering entities.
%% @see  create_ent_dict/2
%% @see  create_conn_list/2
%% @see  draw_clusters/4
%% @see  draw_hac_clusters/4
write_conns(ConnLst, EntDict, Dev) ->
    lists:foreach(
        fun({{Entity1, Entity2}, Weight}) ->
            CountStr = if
                ok==Weight -> "";
                true -> ?MISC:format(" [label=\"~p\"]", [Weight]) %"
            end,
            case {dict:find(Entity1,EntDict), dict:find(Entity2,EntDict)} of
                {{ok,{Id1,_}},{ok,{Id2,_}}} -> 
                    io:format(Dev, " ~s -> ~s~s;\n", [Id1, Id2, CountStr]);
                _ -> ok
            end
        end,
        ConnLst).


%% @spec write_clusters(Clusters::[[ClusteringEntity]], EntDict::dict(), Dev, 
%%                      ClusterIdx::integer()) -> NexClusterIdx::integer()
%% @doc  Write the clusters as subgrpah into the `Dev' device. `ClusterIdx' is
%%       the index of the first cluster. Return the index of the next cluster.
%%       `EntDict' contains the id and the label of the clustering entities.
%% @see  create_ent_dict/2
%% @see  draw_clusters/4
%% @see  draw_hac_clusters/4
write_clusters(Clusters, EntDict, Dev, ClusterIdx) -> 
    lists:foldl(
        fun(Cluster, Idx) ->
            io:format(Dev, " subgraph cluster_~b {\n", [Idx]),
            write_cluster_contain(Cluster, EntDict, Dev, "  ", ";\n"),
            io:format(Dev, " }\n", []),
            Idx+1
        end,
        ClusterIdx,
        Clusters).


%% @spec write_cluster_contain(Cluster::[ClusteringEntity], EntDict::dict(), 
%%               Dev, PreStr::string(), PostStr::string()) -> ok
%% @doc  Write the entities of `Cluster' int the `Dev' device.
%%       `EntDict' contains the id and the label of the clustering entities.
%% @see  create_ent_dict/2
write_cluster_contain(Cluster, EntDict, Dev, PreStr, PostStr) ->
    lists:foreach(
        fun(Entity) ->
            EntId = element(1,element(2,dict:find(Entity,EntDict))),
            io:format(Dev, "~s~s~s", [PreStr,EntId,PostStr])
        end,
        Cluster).


%% @spec write_hac_dendogramm(EntDict::dict(), Dev, Level::integer(), 
%%               ClusterIdx::integer(), Cluster::[ClusteringEntity], 
%%               LowerClusterings::[[[ClusteringEntity]]]) -> ok
%% @doc  Write the `Cluster' as subgrpah into the `Dev' device. `ClusterIdx' is
%%       the index of the cluster. `Level' is the one based depth in the 
%%       hierachic. `LowerClusterings' are some clusterings in the hierarhic 
%%       bottom of `Cluster'. Subclusters are write as subgraph too.
%%       `EntDict' contains the id and the label of the clustering entities.
%% @see  create_ent_dict/2
%% @see  draw_clusters/4
%% @see  draw_hac_clusters/4
write_hac_dendogramm(EntDict, Dev, Level, ClusterIdx, 
                     Cluster, LowerClusterings) ->
    % Some statistics
    NumLowLevel = length(LowerClusterings),
    ClSize = length(Cluster),
    IndentStr = lists:flatten(lists:duplicate(Level," ")),
    % Write clusters
    if
        % Cluster with one element
        1==ClSize ->
            if
                % Part of a subgraph
                1<Level ->
                    write_cluster_contain(Cluster,EntDict,Dev,IndentStr,";\n");
                true -> ok % At the top level
            end,
            ClusterIdx;
        % Cluster with more elements in the lowest level
        0==NumLowLevel ->
            io:format(Dev, "~ssubgraph cluster_~b {\n", [IndentStr,ClusterIdx]),
            write_cluster_contain(Cluster, EntDict, Dev, " "++IndentStr, ";\n"),
            io:format(Dev, "~s}\n", [IndentStr]),
            ClusterIdx+1;
        % Cluster with more elements in a higher level
        true -> 
            % Find sub clusters
            ChildCls = ?MISC:list_contains(Cluster, hd(LowerClusterings)), 
            SubGraph = (1<length(ChildCls)),
            TailClusterings = tl(LowerClusterings),
            % If it has more than one sub cluster draw a new subgraph
            ClIdx1 = if
                SubGraph -> 
                    io:format(Dev, "~ssubgraph cluster_~b {\n", 
                              [IndentStr,ClusterIdx]),
                    ClusterIdx+1;
                true -> 
                    ClusterIdx
            end,
            % Draw sub clusters
            ClIdx2 = lists:foldl(
                fun(Cl, Idx) ->
                    write_hac_dendogramm(EntDict, Dev, Level+1, Idx,
                                         Cl, TailClusterings)
                end,
                ClIdx1,
                ChildCls),
            if
                SubGraph -> io:format(Dev, "~s}\n", [IndentStr]);
                true -> ok
            end,
            ClIdx2
    end.

