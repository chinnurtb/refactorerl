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
%%% Module information

%%% @doc Utilities for Erlang Semantical Graph.
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(reflib_graph_utils).
-vsn("$Rev: 4969 $").
-include("lib.hrl").
-include_lib("referl_core/src/refcore_schema.hrl").


%%% ============================================================================
%%% Exports

%% Nodes
% -export([is_virtual_node/1, get_orig_node/1, get_node_datas/1]).
-export([check_node/3, check_node/2, check_node_token/3, check_node_token/2]).
%% Links
-export([edges_idx/3]).
% Shemas, node types, link types
% -export([schema_has/4]).
%% Nodes and paths
-export([find_node/5, find_upnode/4, top_node/2]).
% Graph algorithms
% -export([dfs/3, dfs_id/5]).
% Syntax tree utilities
% -export([syn_children/1]).



%%% ----------------------------------------------------------------------------
%%% Nodes

%% @ spec get_node_type(Node::node()) -> atom()
%% @ doc  Return the type of the given node.
% get_node_type(Node) -> element(2,Node).


%% @ spec is_virtual_node(Node::node()) -> bool()
%% @ doc  Determine is the `Node' virtual?
% is_virtual_node(Node) ->
    % Virtuals = [{lex, [{#lex.type,'==',token},{#lex.data,'==',virtual}]}],
    % []/=lists:dropwhile(fun({T,Fs}) -> not check_node(Node,T,Fs) end,
                          % Virtuals).


%% @ spec get_orig_node(Node::node()) -> node()
%% @ doc  Give back the original node. If `Node' is virtual follow `orig' links
%%       else return itself.
% get_orig_node(Node) ->
    % case is_virtual_node(Node) of
        % true ->
            % [OrigNode] = ?Graph:path(Node, [{orig,1}]),
            % get_orig_node(OrigNode);
        % _ -> Node
    % end.


%% @ spec get_node_datas(Node::node()) -> [NodeData::tuple()]
%% @ doc  Give back `Node' data and the original node data. If `Node' is not
%%       virtual tha return only ona data.
% get_node_datas(Node) ->
    % case is_virtual_node(Node) of
        % true ->    [?Graph:data(Node), ?Graph:data(get_orig_node(Node))];
        % _    -> [?Graph:data(Node)]
    % end.


%% @spec check_node(Node::node(), NodeType::atom(),
%%           NoteDataFilters::[recordFilter()]) -> bool()
%% @throws badarg | {badmatch, Op}
%% @doc  Check `Node' type and data. If `NodeDataFilters' is empty `Node' will
%%       be accepted.
%%
%% Example:
%% ```
%% check_node(#expr{kind=tuple}, expr, [{#expr.kind,'==',tuple}]).
%% '''
%% @see  referl_misc:check_record/2
%% @see  referl_misc:get_rec_value/2
check_node(Node, token, Filters) when is_list(Filters) ->
    Data = ?Graph:data(Node),
    ?Graph:class(Node)==lex andalso Data#lex.type==token andalso 
    is_tuple(Data#lex.data) andalso ?MISC:check_record(Data#lex.data, Filters);
check_node(Node, Type, Filters) when is_atom(Type), is_list(Filters) ->
    ?Graph:class(Node)==Type andalso
    ?MISC:check_record(?Graph:data(Node), Filters).


%% @spec check_node(Node::node, Checks::[NodeFilterX]) ->
%%           {Index::integer(), NodeFilter}
%%       NodeFilterX = NodeFilter2 | NodeFilter3
%%       NodeFilter2 = {NodeType::atom(), NoteDataFilters::[recordFilter()]}
%%       NodeFilter3 = {Id::term(), NodeType::atom(),
%%           NoteDataFilters::[recordFilter()]}
%% @doc  Check `Node' with node filters from `Checks'. Node is accepted if
%%       any filter is accept that. The returned pair contain the
%%       index of first filter which accept the node and the filter. If any
%%       filter accept it the index is 0. 
%%
%% Example:
%% ```
%% check_node(#expr{kind=tuple}, [{case1, form, [{#form.type,'==',func}]},
%%                                {case2, expr, [{#expr.kind,'==',tuple}]}]).
%% '''
%% @see  check_node/3
check_node(Node, Checks) when is_list(Checks) ->
    ?MISC:list_find(fun({_Id,Type,Filters}) ->
                        check_node(Node, Type, Filters);
                    ({Type,Filters}) ->
                        check_node(Node, Type, Filters)
                    end,
                    Checks).


%% @spec check_node_token(Node::node(), NodeType::atom(),
%%           NoteDataFilters::[recordFilter()]) -> bool()
%% @throws badarg | {badmatch, Op}
%% @doc  Check `Node' type and data. If `NodeDataFilters' is empty `Node' will
%%       be accepted. 
%%       If `Type' is `lex' the `#lex.data' field will be checked by 
%%       `NoteDataFilters' instead `#lex{}'.
%%
%% Example:
%% ```
%% check_node_token(#lex{type=token, data=#token{type=arrow, value='->', 
%%                                               text="->"}}, lex, 
%%                  [{#token.type,'==',arrow}]).
%% '''
%% @see  referl_misc:check_node/3
check_node_token(Node, lex, Filters) ->
	Data = ?Graph:data(Node),
    case lex==?Graph:class(Node) andalso token==Data#lex.type of
        true -> is_tuple(Data#lex.data) andalso 
                ?MISC:check_record(Data#lex.data, Filters);
        _    -> check_node(Node, lex, Filters)
    end;
check_node_token(Node, Type, Filters) ->
    check_node(Node, Type, Filters).


%% @spec check_node_token(Node::node, Checks::[NodeFilter]) ->
%%           {Index::integer(), NodeFilter}
%%       NodeFilter = {Id::term(), NodeType::atom(),
%%           NoteDataFilters::[recordFilter()]}
%% @doc  Check `Node' with node filters from `Checks'. Node is accepted if
%%       any filter is accept that. The returned pair contain the
%%       index of first filter which accept the node and the filter. If any
%%       filter accept it the index is 0.
%%       If `NodeType' is `lex' the `#lex.data' field will be checked by 
%%       `NoteDataFilters' instead `#lex{}'.
%%
%% Example:
%% ```
%% check_node_token(#lex{data=#token{type=arrow, value='->', text="->"}}, 
%%                  [{case1, lex, 
%%                  [{#token.type,'==',arrow}]).
%% check_node_token(#lex{data=#token{type=arrow, value='->', text="->"}},
%%                  [{case1, form, [{#form.type,'==',func}]},
%%                   {case2, expr, [{#expr.kind,'==',tuple}]},
%%                   {case3, lex,  [{#token.type,'==',arrow}]}).
%% '''
%% @see  check_node/2
check_node_token(Node, Checks) when is_list(Checks) ->
    ?MISC:list_find(fun({_Id,Type,Filters}) ->
                        check_node_token(Node, Type, Filters);
                    ({Type,Filters}) ->
                        check_node_token(Node, Type, Filters)
                    end,
                    Checks).



%%% ----------------------------------------------------------------------------
%%% Links

%% @spec edges_idx(FromNode::node(), Direction, 
%%               ToTagNodes::[{LinkTag::atom(), ToNode::node()}]) ->
%%           [{LinkTag::atom(), ToNode::node(), LinkIndex::integer()}]
%%       Direction = forward | back
%% @doc  Expand links with indexes. Get indices of links from `FromNode' to
%%       `ToTagNodes' and and give back the extended path element.
edges_idx(FromNode, Dir, ToTagNodes) ->
    edges_idx_(FromNode, Dir, lists:reverse(ToTagNodes), []).

edges_idx_(_,_, [], Edges) -> Edges;
edges_idx_(FromNode, Dir, [{ToLink,ToNode}|ToTagNodes], Edges) ->
    Edge = case Dir of
        forward -> {ToLink,ToNode,?Graph:index(FromNode,ToLink,ToNode)};
        _       -> {ToLink,ToNode,?Graph:index(ToNode,ToLink,FromNode)}
    end,
    edges_idx_(FromNode, Dir, ToTagNodes, [Edge|Edges]).



%%% ----------------------------------------------------------------------------
%%% Shemas, node types, link types

%% @ spec schema_has(Schema::[SchElem], FromType::atom(), LinkTag::atom(), ToType::atom()) ->
%%           {LinkTag::atom(), ToType::atom()} | no
%%       SchElem = SchElem2 | SchElem3
%%       SchElem2 = {FromType::atom(), [{LinkTag::atom(), ToType::atom()}]}
%%       SchElem3 = {FromType::atom(), [RecordField::atom()],
%%                  [{LinkTag::atom(), ToType::atom()}]}
%% @ doc  Give back the matched {link, destination type} pairs from `FromType'
%%       in the `Schema'. If `LinkTag' or `ToType' is `undefined' every
%%       appropriate type is accepted.
%%
%% Example:
%% ```
%% case schema_has(?SYNTAX_SCHEMA,  FromNode, Tag, ToNode) of
%%     [{LinkTag, NodeType}|_] -> ok;
%%     _ -> no
%% end.
%% '''
% schema_has(Schema, FromType, LinkTag, ToType) when is_list(Schema),
        % is_atom(FromType), is_atom(LinkTag), is_atom(ToType) ->
    % case lists:dropwhile(fun(T) -> FromType/=element(1, T) end, Schema) of
        % [SchElem|_] ->
            % CheckFun = fun({LT,TT}) ->
                    % (LinkTag==undefined orelse LT==LinkTag) andalso
                    % (ToType ==undefined  orelse TT==ToType)
                % end,
            % %case lists:dropwhile(CheckFun, element(tuple_size(SchElem), SchElem)) of
            % %    [To|_] -> To;
            % %    _ -> no
            % %end;
            % lists:filter(CheckFun, element(tuple_size(SchElem), SchElem));
        % %_ -> no
        % _ -> []
    % end.



%%% ----------------------------------------------------------------------------
%%% Nodes and paths

%% @spec find_node(Node::node(),
%%                {NextFun::((Node::node(), Acc::term()) -> {NextNode::node()}),
%%                    NextAcc::term()},
%%                Nodes::[node()],
%%                Filters::[NodeFilter],
%%                Functions::[NodeFunction]) ->
%%            {FoundNode::node(), MatchType, MatchObject, PathToFoundNode,
%%                PathFromFoundNode} | no
%%       NextFun = ((Node::node(), Acc::term()) ->
%%           {NextNode::node(), PathTo, PathBack, NewAcc::term()} | no)
%%       NodeFilter = {Id::term(), NodeType::atom(),
%%           NodeDataFilters::[recordFilter()]}
%%       Id = atom()
%%       NodeFunction = {Id, Pred::((Node::node()) -> bool()) }
%%       MatchType   = node | filter | function
%%       MatchObject = node() | Id
%%       PathToFoundNode = path()
%%       PathFromFoundNode = path()
%% @throws {badmatch, RecordField} | {badmatch, Op} | term()
%% @doc  Start walk from `Node' by `NextFun' and find first node which is
%%       correspond to conditions.
%%       A node is accepted if same as one from Nodes. Or {@link check_node/2}
%%       accept that with the `Filters'. Or a `Pred' function  from
%%       Functions accept that (`Pred(CurrentNode) == true'). The checks are
%%       executed in the previous order. If all three lists are empty `Node' will
%%       be accepted.
%%       If a node is not accepted in any way step to the next node by `NextFun'.
%%
%%       Matching type (node, filter, function) with the matced object
%%       (node() or id()) determine the matching case.
%%       Pathes are concatenated path sequences given back by `NextFun'.
%%       Can use with {@link refcore_graph:path/2} function.
%%
%% Example:
%% ```
%%% ParentFun = fun(N, Acc) ->
%%%     case ?ESG:parent(N) of
%%%         [{Tag,Parent}] ->
%%%             Idx = ?Graph:index(Parent,Tag,N),
%%%             {Parent, [{Tag,back}], [{Tag,Idx}], Acc};
%%%         _ -> no
%%%     end
%%% end,
%%% find_node(Node, {ParentFun, ok}, NodeList, FilterList, FunList).
%% '''
%% @see  check_node/2
%% @see  check_record/2
%% @see  get_rec_value/2
find_node(Node, {_NextFun, _Acc}, [], [], []) -> Node;
find_node(Node, {NextFun, Acc}, NodeList, FilterList, FunList)
        when is_function(NextFun), is_list(NodeList), is_list(FilterList),
             is_list(FunList) ->
    find_node_(Node, {NextFun, Acc}, NodeList, FilterList, FunList, [],[]).

find_node_(Node, {NextFun, Acc}, NodeList, FilterList, FunList,
        PathTo, PathBack) ->
    % Compare with Nodes
    case lists:dropwhile(fun(N)->(N=/=Node)end, NodeList) of
        [] ->
            % Check node with filters
            case check_node(Node, FilterList) of
                {0,_}  ->
                    % Check node with functions
                    case lists:dropwhile(
                            fun({_Id,Fun}) -> not Fun(Node) end, FunList) of
                        [] ->
                            % Go up to next node
                            case NextFun(Node, Acc) of
                                {NextNode, PathTo1, PathBack1, Acc2} ->
                                    find_node_(NextNode, {NextFun, Acc2},
                                        NodeList, FilterList, FunList,
                                        PathTo++PathTo1, PathBack1++PathBack);
                                _ -> no
                            end;
                        [{MatchId,_}|_] ->
                            {Node, function, MatchId, PathTo,PathBack}
                    end;
                {_,{MatchId,_,_}} ->
                    {Node, filter, MatchId, PathTo, PathBack}
            end;
        [MatchNode|_] -> {Node, node, MatchNode, PathTo, PathBack}
    end.


%% @spec find_upnode(Node::node(), Nodes::[node()], Filters::[NodeFilter],
%%                Functions::[NodeFunction]) ->
%%            {FoundNode::node(), MatchType, MatchObject, PathUpToFoundNode,
%%                PathDownFromFoundNode} | no
%%       NodeFilter = {Id::term(), NodeType::atom(),
%%           NodeDataFilters::[recordFilter()]}
%%       Id = atom()
%%       NodeFunction = {Id, Pred::((Node::node()) -> bool()) }
%%       MatchType   = node | filter | function
%%       MatchObject = node() | Id
%%       PathToFoundNode = path()
%%       PathFromFoundNode = path()
%% @throws {badmatch, RecordField} | {badmatch, Op} | term()
%% @doc  Find first syntactical parent node of `Node' which is
%%       correspond to conditions.
%%       Wrapper function for {@link find_node/5}
%% @see  find_node/5
find_upnode(Node, NodeList, FilterList, FunList) ->
    ParentFun = fun(N, Acc) ->
        case ?Syn:parent(N) of
            [{Tag,Parent}] ->
                Idx = ?Graph:index(Parent,Tag,N),
                {Parent, [{Tag,back}], [{Tag,Idx}], Acc};
            _ -> no
        end
    end,
    find_node(Node, {ParentFun, ok}, NodeList, FilterList, FunList).


%% @spec top_node(Node1::node(), Node2::node()) ->
%%           {node(), PathFromTopNodeToNode1::path(), 
%%            PathFromTopNodeToNode2::path()} | no
%% @throws term()
%% @doc  Find lowest common sintactical or lexical parent node of given
%%       sintactical or lexical nodes. Both node must be in the syntactical
%%       part of the ESG.
%%       Paths from top node to nodes are also returned.
top_node(Node1, Node2) ->
    Root = ?Graph:root(),
    {_,node,_,_,Path1} = find_upnode(Node1, [Root], [], []),
    {_,node,_,_,Path2} = find_upnode(Node2, [Root], [], []),
    {PathCommon, Tail1, Tail2} = ?MISC:list_compare(Path1, Path2),
    case PathCommon of
        [] -> {Root, Tail1, Tail2};
        _  -> {hd(?Graph:path(Root, PathCommon)), Tail1, Tail2}
    end.



%%% ----------------------------------------------------------------------------
%%% Graph algorithms

%% @ spec dfs(Node::node(), Cfg::DepthConfig,
%%           Funs::{FunPre, FunPost, FunNexts, State}) -> State
%%       DepthConfig = {MaxDepth::integer(), NodeVisitMax::integer(),
%%                      NodeChildsVisitMax::integer()}
%%       FunPre  = FunNodeProc
%%       FunPost = FunNodeProc
%%       FunNodeProc = ((Node::node(), LinkTag, PrevNode, Count::natural(),
%%                       State) -> {SearchAction, State})
%%       SearchAction = ok | {back,natural()} | stop
%%       LinkTag  = atom() | no
%%       PrevNode = node() | no
%%       FunNexts = ((Node::node, State) ->
%%                       {NextNodes::[{LinkTag::atom(), Node::node()}], State})
%%       State = term()
%% @ doc  Depth-first search (DFS) in the graph starts from `Node'.
%%
%%       `FunNexts' function determine the next arcs and nodes from `Node' with
%%       the order of their. You can customize the walking direction with an
%%       own function or use the {@link dfs_all_nexts/2}, {@link dfs_syn_nexts},
%%       {@link dfs_syn_childs} functions. If you don't want to step forward to
%%       neighbours just give back an empty list as `NextNodes' and the
%%       algorithm will be step back.
%%
%%       Before and after the recursion on next nodes the `FunPre' and `FunPost'
%%       functions are executed to process `Node'. You can use `State' to
%%       preserve informations between runs. The `State' first pass to `FunPre'.
%%       After the new state pass to `FunNexts' and to the recursions on
%%       neighbour nodes of `Node'. Last the returned state from the recursions
%%       is pass to `FunPost'. The finally state is the result of the DFS.
%%
%%       The return value of node processing functions (`FunPre', `FunPost') is
%%       determine the behavior of the search at that node. These options are:
%%
%%       <ul>
%%         <li>`ok': The search is continued normally.</li>
%%         <li>`{back,N::natural()}': Instead continue the search at neighbours
%%            the algorithm step back `N' steps on the searching path and
%%            continue there.</li>
%%         <li>`stop': Same as `{back, Depth}' where `Depth' is the current
%%            length of searchin path.</li>
%%       </ul>
%%
%%       Some simply other configuration are required to guarantee the
%%       terminating and make the search more confortable. These options are
%%       sets by `DepthConfig' argument:
%%
%%       <ul>
%%         <li>`MaxDepth': Maximal length of paths from `Node'. It ensure the
%%             terminating of the searching.</li>
%%         <li>`NodeVisitMax': The searching visits a node maximun this
%%             times. After that the visits will be omitted. If the value is
%%             less than one the algorithm visits the node all times.</li>
%%         <li>`NodeChildsVisitMax': The searching step forward to the next
%%             nodes of a node maximun this times. After that the steps will be
%%             omitted. If the value is less than one the algorithm step
%%             forward to the next nodes of a node all times.</li>
%%       </ul>
%%
%% Example:
%% ```
%% FunWrite = fun(Node, _LinkTag, _Parentnode, _Count, NodeCount) ->
%%     io:format("~b: ~p\n", [NodeCount, Node]),
%%     {ok, NodeCount+1}
%% end,
%% FunNexts = fun(Node, State) ->
%%     {?Graph:links(Node), State}
%% end,
%% dfs(Root, {10,1,1}, {FunWrite, fun dfs_id/5, FunNexts, 0}).
%% '''
% dfs(Node, {MaxDepth, NodeVisitMax, NodeChildsVisitMax},
        % {FunPre,FunPost,FunNexts, State}) when is_integer(MaxDepth),
            % is_integer(NodeVisitMax), is_integer(NodeChildsVisitMax),
            % is_function(FunPre), is_function(FunPost), is_function(FunNexts) ->
    % VisNodeTabID = ets:new(dw_visnodes, []),
    % {_, State2} = dfs_(Node, no,no, {MaxDepth,NodeVisitMax,NodeChildsVisitMax},
        % 0, VisNodeTabID, {FunPre,FunPost,FunNexts, State}),
    % ets:delete(VisNodeTabID),
    % State2.

% Implementation function of dfs/3.
% Extra parameters:
%   Parent  = no | node()             % Parent node of Node
%   LinkTag = no | atom()             % Tag of link from Parent
%   Depth   = natural()               % Length of search path from starting node
%                                     % to current node
%   VisitNodeTabID = ets_table_id()   % How many times visit a node before?
% Return value: {Back, NewSate}
%   Back = natural()                  % Required backward steps from Node on the
%                                     % search path
%   NewState = term()                 % The calculated new state by Funs
% dfs_(Node, LinkTag, Parent, {MaxDepth,NodeVisitMax,NodeChildsVisitMax},
        % Depth, VisNodeTabID, {FunPre,FunPost,FunNexts, State}) ->
    % % Check node is visited
    % VisitCount0 = case ets:lookup(VisNodeTabID, Node) of
        % [{Node,C}] -> C;
        % _ -> 0
    % end,
    % % Visit node
    % if
        % 0<NodeVisitMax andalso NodeVisitMax=<VisitCount0 -> {0, State};
        % true ->
            % VisitCount = VisitCount0+1,
            % ets:insert(VisNodeTabID, {Node,VisitCount}),
            % % Run FunPre for Node
            % {Act1, State1} = FunPre(Node, LinkTag, Parent, VisitCount, State),
            % Back1 = case Act1 of
                % stop -> Depth+1;
                % {back,N11} when is_integer(N11), 0<N11 -> N11;
                % {back,N12} when is_integer(N12)        -> 0;
                % ok   -> 0
            % end,
            % % Visit childs
            % {Back2, State2} = if
                % 0<Back1 orelse MaxDepth=<Depth orelse
                        % (0<NodeChildsVisitMax andalso
                         % NodeChildsVisitMax<VisitCount) ->
                    % {Back1, State1};
                % true ->
                    % {Nexts, StateN} = FunNexts(Node, State1),
                    % ?MISC:partfold(
                        % fun({Tag,Child}, {_,St}) ->
                            % {B,S} = dfs_(Child, Tag, Node,
                                % {MaxDepth,NodeVisitMax,NodeChildsVisitMax},
                                % Depth+1, VisNodeTabID,
                                % {FunPre,FunPost,FunNexts, St}),
                            % if
                                % 0==B -> {next, {B,S}};
                                % true -> {stop, {B,S}}
                            % end
                        % end,
                        % {0, StateN}, Nexts) % -> {Back2, State2}
            % end,
            % % Run FunPost for Node
            % {Act3, State3} = FunPost(Node, LinkTag,Parent, VisitCount, State2),
            % Back3 = case Act3 of
                % stop -> Depth+1;
                % {back,N31} when is_integer(N31), 0<N31 -> N31;
                % {back,N32} when is_integer(N32)        -> 0;
                % ok   -> 0
            % end,
            % % Return with the required backward steps and the new state
            % case lists:max([Back2,Back3]) of
                % N41 when  1<N41 -> {N41-1, State3};
                % _               -> {0,     State3}
            % end
    % end.


%% @ spec dfs_id(_Node::node(), _LinkTag::atom(), _PrevNode::node(),
%%           _Count::natural(), State) -> {ok, State}
%% @ doc  Identically node processing function for Depth-first Search.
%%       Simply return {ok, `State'} which mean continue the searchig normally.
%% @ see  dfs/3
% dfs_id(_Node,_LinkTag,_Parent,_Count, State) -> {ok, State}.



%%% ----------------------------------------------------------------------------
%%% Syntax tree utilities

%% @ spec syn_children(Node::node) -> SynChilds::[node()]
%% @ doc  Return the syntactical childrens of the `Node' in the right syntactical
%%       order. Root and file nodes are also allowed.
% syn_children(Node) ->
    % lists:filter(
        % fun({Tag,Node2}) ->
            % (Tag/=llex) andalso
            % (([]/=schema_has(?SYNTAX_SCHEMA,  ?Graph:class(Node),
                % Tag, ?Graph:class(Node2))) orelse
             % ([]/=schema_has(?LEXICAL_SCHEMA, ?Graph:class(Node),
                % Tag, ?Graph:class(Node2))))
        % end,
        % ?SYNTAX:children(Node)).
        % %?Graph:links(Node)).



