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

%%% @doc Multiple hashing level tree
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(multi_hashtree).
-vsn("$Rev:  $").

-include("kcl.hrl").


%% =============================================================================
%% Exports

% Construction/destruction
-export([new/3, hash_gen/2, auto_hash_gen/3, delete/1]).
% General queries
-export([is_hashtree/1,
         get_levels/1, get_leaf_size/1, get_hashtable_size/1, get_hash_fun/1]).
% Operations
-export([insert/3, insertL/3, lookup/2, lookupL/2, subset/2, subsetL/2,
         simple_erase/2]).
         


%% =============================================================================
%% Types

%%
%%
-record(multi_hashTree, {
    levels,     % int(): number of hashing levels
    hash_size,  % int(): size of hash tables    
    hash_fun,   % ((term()) -> [1..hash_size]): the hash function
    leaf_size,  % int(): maximal number of elements in a leaf
    nodes,      % ETS_tid(): nodes in the hash tree
    hashes,     % ETS_tid(): partial hash values of elems in leaf nodes
    rootId}).   % int(): the ID of the root element in the nodesETS



%% =============================================================================
%% Hash generations

%% @spec hash_gen(HashTableSize::integer(), Numbers::boolean()) ->
%%           {HashTableSize::integer(), HashFun::((term()) -> HashRange)}
%% @doc      HashRange = 1..HashTableSize
%%        Generate a hash function which hash any term into the 
%%      `1..HashTablesSize' range.
%%      If `Numbers' is `true' than it will use `erlang:phash2/2' function 
%%      otherwise `erlang:phash/2' function.
hash_gen(HashTableSize,false) when is_integer(HashTableSize), 1<HashTableSize ->
    {HashTableSize, fun(X) -> erlang:phash(X,HashTableSize)    end};
hash_gen(HashTableSize,true) when is_integer(HashTableSize), 1<HashTableSize ->
    {HashTableSize, fun(X) -> erlang:phash2(X,HashTableSize)+1 end}.


%% @spec auto_hash_gen(Levels::integer(), MaxElems::integer(), 
%%               Numbers::boolean()) -> 
%%           {{HashTableSize::integer(), HashFun::((term()) -> HashRange)},
%%            MaxLeafSize::integer()}
%% @doc Generate the parameters of an `Levels'-level hash tree. It try to 
%%      generate hash table size and leaf size that build an optimal hash tree
%%      for about `MaxElems' elements. `Numbers' is used to generate the hash 
%%      function with {@link hash_gen/2} function.
%% @see hash_gen/2
auto_hash_gen(Levels, MaxElems, Numbers) when is_integer(Levels), 0<Levels, 
        is_integer(MaxElems), 0<MaxElems, is_boolean(Numbers) ->
    % Calculate: ceil(log2(MaxElem)/Levels)
    I = math_ceil((math:log(MaxElems)/math:log(2))/Levels),
    LS = trunc(math:pow(2,I)),
    HS = LS-1,
    {hash_gen(HS, Numbers), LS}.



%% =============================================================================
%% Construction/destruction

%% @spec new(LevelN::integer(), LeafListsMax::integer(), 
%%               HashTableSize::integer()) -> multi_hashTree()
%% @doc  Create a new empty n-level hash tree.
new(LevelN, {HashTableSize, HashFun}, MaxLeafSize) when is_integer(LevelN), 
        0<LevelN, is_integer(HashTableSize), 1<HashTableSize,
        is_function(HashFun), is_integer(MaxLeafSize), 0<MaxLeafSize ->
    NodesETS  = ets:new(muli_hashTree_nodes,  []),
    HashesETS = ets:new(muli_hashTree_hashes, []),
    RootId    = 0,
    ets:insert(NodesETS, {RootId, 0, []}), % Insert an empty leaf
    #multi_hashTree{levels=LevelN, hash_size=HashTableSize, hash_fun=HashFun, 
        leaf_size=MaxLeafSize, nodes=NodesETS, hashes=HashesETS, rootId=RootId}.


%% @spec delete(NHashTree::multi_hashTree()) -> ok
%% @doc  Delete the `NHashTree' n-level hash tree.
delete(#multi_hashTree{nodes=NodesETS, hashes=HashesETS}) ->
    ets:delete(NodesETS),
    ets:delete(HashesETS),
    ok.



%% =============================================================================
%% General queries

%%
%%
is_hashtree(#multi_hashTree{}) -> true;
is_hashtree(_) -> false.

%%
%%
get_levels(#multi_hashTree{levels=N}) -> N.

%%
%%
get_leaf_size(#multi_hashTree{leaf_size=LS}) -> LS.

%%
%%
get_hashtable_size(#multi_hashTree{hash_size=HS}) -> HS.

%%
%%
get_hash_fun(#multi_hashTree{hash_fun=HF}) -> HF.



%% =============================================================================
%% Operations


%%
%%
insertL(Elem, NList, HT=#multi_hashTree{hash_fun=HashFun}) ->
    insert(Elem, lists:map(HashFun, NList), HT).


%% @spec insert(Elem::term(), NHashList::[HashRange::integer()],
%%           NHashTree::multi_hashTree()) -> ok
%% @doc  Insert `Elem' element into the `NHashTree' n-level hash tree. The
%%       length of `NHashList' must be equal the `levels' value given in the
%%       `NHashTree' n-level hash tree. Every value in the list must be 
%%       generatedin by the `HashFun' function given in the `NHashTree'.
insert(Elem, NHashList, HT=#multi_hashTree{nodes=NodesETS, rootId=RootId}) when
        is_list(NHashList) ->
    [{RootId, RootType, RootContent}] = ets:lookup(NodesETS, RootId),
    insert_(RootType,0, RootId,RootContent, Elem,NHashList, HT),
    ok.

% Insert into an interior node
insert_(innode,K, _NodeId,ChildIds, Elem,[Hash|Hashes],
        HT=#multi_hashTree{nodes=NodesETS}) ->
    ChildId = lists:nth(Hash, ChildIds),
    [{ChildId, ChildType, ChildContent}] = ets:lookup(NodesETS, ChildId),
    insert_(ChildType,K+1, ChildId,ChildContent, Elem,Hashes, HT);
% Insert into a non full leaf
insert_(LeafSize,K, NodeId,LeafElems, Elem,Hashes, #multi_hashTree{levels=N,
        leaf_size=LS, nodes=NodesETS, hashes=HashesETS}) when LeafSize<LS ->
    ets:insert(NodesETS, {NodeId, LeafSize+1, [Elem|LeafElems]}),
    if % If there is usefull tail of Hashes than save it
        K<N -> ets:insert(HashesETS, {Elem, Hashes});
        true -> ok
    end;
% Insert into a full leaf at level N 
insert_(LeafSize,N, NodeId,LeafElems, Elem,_Hashes,
        #multi_hashTree{levels=N, nodes=NodesETS}) ->
    ets:insert(NodesETS, {NodeId, LeafSize+1, [Elem|LeafElems]}); % Reverse
    % ets:delete(HashesETS, Elem);
% Insert into a full leaf
insert_(_LeafSize,K, NodeId,LeafElems, Elem,[Hash|Hashes],
        HT=#multi_hashTree{levels=N, hash_size=HS, nodes=NodesETS, 
        hashes=HashesETS}) ->
    % Partition the elements in the leaf by the hash value
    LEDict0 = lists:foldl(fun(H, D) -> dict:store(H, [], D) end,
        dict:new(),
        lists:seq(1,HS)),
    NotLastLevel = (K+1<N), % Are the elems replaced into childs at last level?
    LEDict  = lists:foldl(
        fun(LE, AccDict) ->
            [{LE, [LEHash|LEHashes]}] = ets:lookup(HashesETS, LE),
            % Update partial hashes
            if
                NotLastLevel -> ets:insert(HashesETS, {LE, LEHashes});
                true         -> ets:delete(HashesETS, LE)
            end,
            dict:append(LEHash, LE, AccDict) % Reverse
        end,
        LEDict0,
        LeafElems),
    % Create child leaves
    ChildIds  = lists:map(
        fun({_H, Es}) ->
            NextNodeId = ets:info(NodesETS, size),
            ets:insert(NodesETS, {NextNodeId, length(Es), Es}),
            NextNodeId
        end,
        lists:usort(dict:to_list(LEDict))),
    % Replace leaf with a new interior node
    ets:insert(NodesETS, {NodeId, innode, ChildIds}),
    % Insert the element into the new interior node
    insert_(innode,K, NodeId,ChildIds, Elem,[Hash|Hashes], HT).


%%
%%    
lookupL(NList, HT=#multi_hashTree{hash_fun=HashFun}) ->
    lookup(lists:map(HashFun, NList), HT).


%% @spec lookup(NHashList::[HashRange::integer()], 
%%               NHashTree::multi_hashTree()) -> Elems::[term()]
%% @doc  Find the elements in the `NHashTree' n-level hash tree which have same
%%       `n' hash value as these are given in `NHashList'. 
%%       The `NHashList' length must be equal the `levels' value given in the
%%       `NHashTree' n-level hash tree. Every value in the list must be in the
%%       `[1, hash_size]' interval where `hash_size' is given in `NHashTree'.
%%       The returned list contains the matched elements in reverse order as 
%%       thats were inserted into the hash tree.
lookup(NHashList, HT=#multi_hashTree{nodes=NodesETS, rootId=RootId}) when
        is_list(NHashList) ->
    [{RootId, RootType, RootContent}] = ets:lookup(NodesETS, RootId),
    lookup_(RootType,0,RootContent, NHashList, HT).
    
% Lookup in an interior node
lookup_(innode,K,ChildIds, [Hash|Hashes], HT=#multi_hashTree{nodes=NodesETS}) ->
    ChildId = lists:nth(Hash, ChildIds),
    [{ChildId, ChildType, ChildContent}] = ets:lookup(NodesETS, ChildId),
    lookup_(ChildType,K+1,ChildContent, Hashes, HT);
% Lookup in a leaf node at level N
lookup_(_LeafSize,N,LeafElems, _Hashes, #multi_hashTree{levels=N}) -> LeafElems;
% Lookup in a leaf node
lookup_(_LeafSize,_K,LeafElems, Hashes, #multi_hashTree{hashes=HashesETS}) ->
    lists:filter(fun(LE) -> [{LE, Hashes}] == ets:lookup(HashesETS, LE) end,
                 LeafElems).


%%
%%    
subsetL(NplusList, HT=#multi_hashTree{hash_fun=HashFun}) ->
    subset(lists:map(HashFun, NplusList), HT).


%% @spec subset(NplusHashList::[HashRange::integer()], 
%%           NHashTree::multi_hashTree()) -> Elems::[term()]
%% @doc  Return all elements in the `NHashTree' n-level hash tree which have 
%%       n hash value that are containted by `NplusHashList'.
subset(NplusHashes, HT=#multi_hashTree{levels=N, nodes=NodesETS, rootId=RootId})
        when is_list(NplusHashes) ->
    [{RootId, RootType, RootContent}] = ets:lookup(NodesETS, RootId),
    subset_(RootType,0,RootContent, {length(NplusHashes)-N+1,NplusHashes}, 
            HT, []).
    
% Subset in an interior node
subset_(innode,K,ChildIds, {PrefixLength,Hashes}, 
        HT=#multi_hashTree{nodes=NodesETS}, AccElems) ->
    element(4, lists:foldl(
        fun(_PfLen, {SkipCnt, [H|Hs], D, AccElems1}) ->
            case dict:is_key(H,D) of
                false ->
                    [{_ChildId,ChildType,ChildContent}] = 
                        ets:lookup(NodesETS, lists:nth(H,ChildIds)),
                    {SkipCnt+1, Hs, dict:store(H,ok,D), 
                        subset_(ChildType,K+1,ChildContent,
                                {PrefixLength-SkipCnt,Hs},HT, AccElems1)};
                _ -> {SkipCnt+1, Hs, D, AccElems1}
            end
        end,
        {0, Hashes, dict:new(), AccElems},
        lists:seq(1,PrefixLength)));
% Subset in a leaf node at level N
subset_(_LeafSize,N,LeafElems, {_PL,_Hashes}, #multi_hashTree{levels=N}, 
        AccElems) -> 
    LeafElems++AccElems;
% Subset in a leaf node
subset_(_LeafSize,_K,LeafElems, {_PL,Hashes}, #multi_hashTree{hashes=HashesETS}, 
        AccElems) -> 
    lists:filter(
        fun(LE) -> 
            [{_LE, LEHashes}] = ets:lookup(HashesETS, LE),
            list_ord_contain(Hashes,LEHashes)
        end,
        LeafElems) ++ AccElems.


%%
%%
simple_erase(Elems, #multi_hashTree{nodes=NodesETS}) when is_list(Elems) ->
    ReplaceNodes = ets:foldl(
        fun({Id, LeafSize, LeafElems}, Acc) when is_integer(LeafSize) ->
            RestLEs = LeafElems--Elems,
            case length(RestLEs) of
                LeafSize     -> Acc;
                RestLeafSize -> [{Id, RestLeafSize, RestLEs}|Acc]
            end;
        (_, Acc) ->
            Acc
        end,
        [],
        NodesETS),
    lists:foreach(fun(Node) -> ets:insert(NodesETS, Node) end, ReplaceNodes).



%%% ============================================================================
%%% List operations

%% @spec list_ord_contain(List1::[term()], List2::[term()]) -> boolean()
%% @doc Check if `List1' contains `List2' in ordered way.
%%
%%      The complexity of `list_ord_contain(A, B)' is proportional to
%%      `length(A)+length(B)' instead of the complexity of `B--A' which 
%%      is proportional to `length(A)*length(B)'.
list_ord_contain(List1, List2) when is_list(List1), is_list(List1) ->
    list_ord_contain_(List2, List1).

list_ord_contain_([],    _Ys    ) -> true;
list_ord_contain_(_Xs,    []    ) -> false;
list_ord_contain_([X|Xs], [X|Ys]) -> list_ord_contain_(Xs, Ys);
list_ord_contain_(Xs,    [_Y|Ys]) -> list_ord_contain_(Xs, Ys).



%%% ============================================================================
%%% Math functions

%% @ spec math_floor(Number::number()) -> Floor::integer()
%% @ doc  Return the biggest integer which is less or equal than `Number'.
% math_floor(Number) when is_number(Number) -> 
    % Trunc = trunc(Number),
    % if
        % Trunc=<Number -> Trunc;
        % true          -> Trunc-1
    % end.


%% @spec math_ceil(Number::number()) -> Ceil::integer()
%% @doc  Return the smallest integer which is greater or equal than `Number'.
math_ceil(Number) when is_number(Number) -> 
    Trunc = trunc(Number),
    if
        Number=<Trunc -> Trunc;
        true          -> Trunc+1
    end.


