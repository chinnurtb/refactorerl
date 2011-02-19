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

%%% @doc Primary syntax tree interface. This module is intended to contain
%%% every operation that primarily works with the syntax tree representation
%%% of Erlang source code. This includes some token-related functionality,
%%% as they are stored as part of the syntax tree.
%%%
%%% @see referl_esg
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_syntax).
-vsn("$Rev: 2756 $").

%%% ============================================================================
%%% Exports

-export([node_type/1]).
-export([children/1, parent/1, leaves/1, tree_text/1,
         root_path/1, root_path/2, get_file/1]).
-export([create/2, replace/3, replace/2, copy/1]).
-export([build/2]).
-export([index/3, class/1]).

-export([first_leaf/0, last_leaf/0]).
-export([get_comments/1, put_comments/2]).

-include("refactorerl.hrl").
%% =============================================================================
%% Node properties

%% @spec node_type(Node::node()) -> atom()
%% @doc  The type of the given syntax node.
node_type({'$gn', Type, _}) ->
    Type.

%% =============================================================================
%% Queries

%% @spec first_leaf() -> (Node::node()) -> node()
%% @doc  First leaf of syntactic subtree specified by `Node' as root.
first_leaf() ->
    fun(Node) ->
        lists:flatten(leaf_down_(Node, fun erlang:hd/1))
    end.

%% @spec last_leaf() -> (Node::node()) -> node()
%% @doc  Last leaf of syntactic subtree specified by `Node' as root.
last_leaf() ->
    fun(Node) ->
        lists:flatten(leaf_down_(Node, fun lists:last/1))
    end.

%% Implementation function for first_leaf/1 and last_leaf/1 functions.
leaf_down_(Node, DownFun) ->
    case children(Node) of
        [] -> [Node];
        Childs -> leaf_down_(element(2,DownFun(Childs)), DownFun)
    end.


%% =============================================================================
%% Comment handling

%% @spec get_comments(node() | [node()]) -> comments()
%% @doc Strips comments from the parameter nodes and stroes them in a
%%      special container.
get_comments(Nodes) when is_list(Nodes) ->
    [{_Tag, Parent}] = parent(hd(Nodes)),
    Children = [N || {_T, N} <- children(Parent)],
    List     = cut(hd(Nodes), lists:last(Nodes), Children),
    lists:map(fun get_comments/1, List);
get_comments(Node) ->
    First = ?Query:exec1(Node, first_leaf(), error),
    Last  = ?Query:exec1(Node, last_leaf(),  error),

    case ?ESG:data(First) of
        FLex = #lex{data = FData = #token{prews = Pre}} ->
            ?ESG:update(First, FLex#lex{data = FData#token{prews = filter(Pre)}})
    end,

    case ?ESG:data(Last) of
        LLex = #lex{data = LData = #token{postws = Post}} ->
            ?ESG:update(Last, LLex#lex{data = LData#token{postws = filter(Post)}})
    end,

    {Pre, Post}.

%% @spec put_comments(node() | [node()], comments()) -> ok
%% @doc The given nodes are decorated with the comments in the second
%%      parameter. The comments should come from the same number of
%%      nodes and made by {@link get_comments/1}.
put_comments(Nodes, WS) when is_list(Nodes) ->
    [{_Tag, Parent}] = parent(hd(Nodes)),
    Children = [N || {_T, N} <- children(Parent)],
    List     = cut(hd(Nodes), lists:last(Nodes), Children),
    lists:zipwith(fun put_comments/2, List, WS);
put_comments(Node, {Pre, Post}) ->
    First = ?Query:exec1(Node, first_leaf(), error),
    Last  = ?Query:exec1(Node, last_leaf(),  error),

    case ?ESG:data(First) of
        FLex = #lex{data = FData} ->
            ?ESG:update(First, FLex#lex{data = FData#token{prews = Pre}})
    end,

    case ?ESG:data(Last) of
        LLex = #lex{data = LData} ->
            ?ESG:update(Last, LLex#lex{data = LData#token{postws = Post}})
    end.

cut(From, To, List) ->
    %% cut garbage from the front
    Temp = lists:reverse(lists:dropwhile(fun(E) -> E =/= From end, List)),
    %% cut garbage from the end
    lists:reverse(lists:dropwhile(fun(E) -> E =/= To end, Temp)).

filter(S) ->
    lists:filter(fun(C) -> lists:member(C, " \t\n") end, S).


%%% ============================================================================
%%% Syntax tree queries

%% @spec index(node(), atom(), node()) -> integer()
%% @doc Returns the index of a link.
%% @see referl_esg:index/3
index(Parent, Link, Child) ->
    ?Graph:index(Parent, Link, Child).

%% @spec class(node()) -> atom()
%% @doc Returns the node class of the node.
%% @see referl_graph:class/1.
class(Node) ->
    ?Graph:class(Node).


%% @spec children(node()) -> [{atom(), node()}]
%% @doc Returns the children of the node in the order as they appear in the
%% real syntax tree.
children(Node) ->
    Data = ?Graph:data(Node),
    case Data of
        {root}            -> children(Node, file);
        #file{}           -> children(Node, form);
        #form{type=error} -> children(Node, flex); %throw(node_error);
        #form{type=lex}   -> children(Node, flex);
        #form{type=macro} -> children(Node, flex);
        #lex{}            -> children(Node, llex);
        _                 -> children(Node, Data, fun linkdata/1)
    end.

linkdata({Tag, Sym})    -> {Tag, Sym};
linkdata({Tag, _, Lex}) -> {Tag, Lex}.


%% Return children with only one link tag
children(Node, Link) ->
    [{Link, C} || C <- ?Graph:path(Node, [Link])].

%% Return children based on node structure
children(Node, Data, Fun) ->
    children(Node, ?Nodes:structure(Data),?Nodes:lexlink(element(1,Data)), Fun).
children(Node, Struct, Lex, Fun) ->
    ord_children(Struct, Lex, child_set(unord_children(Node,Struct,Lex)), Fun).

unord_children(Node, Struct, Lex) ->
    [{Tag, ?Graph:path(Node, [Tag])} || Tag <- child_links(Struct, Lex)].


%% Match node structure with actual node links
ord_children([], _, _, _) ->
    [];

ord_children([{token, Type} | Tail], Lex, CST, Fun) ->
    {C, CST1} = next_child(Lex, CST),
    [Fun({Lex, Type, C}) | ord_children(Tail, Lex, CST1, Fun)];

ord_children([{symbol, Link} | Tail], Lex, CST, Fun) ->
    {C, CST1} = next_child(Link, CST),
    [Fun({Link, C}) | ord_children(Tail, Lex, CST1, Fun)];

ord_children([{repeat, Sep, Link}=R | Tail], Lex, CST, Fun) ->
    ord_children([{symbol, Link}, {optional,[{token, Sep}, R]} | Tail],
                 Lex, CST, Fun);

ord_children([{optional, Opt} | Tail], Lex, CST, Fun) ->
    case has_child(opt_link(Opt), CST) of
        true  -> ord_children(Opt ++ Tail, Lex, CST, Fun);
        false -> ord_children(Tail, Lex, CST, Fun)
    end.

%% Return the link tag which identifies the presence of an optional part
opt_link([{token, _}        | Tail])  -> opt_link(Tail);
opt_link([{symbol, Link}    | _])     -> Link;
opt_link([{repeat, _, Link} | _])     -> Link.


%% Child storage implementation
child_set(Children) ->
    orddict:from_list(Children).

get_child(Link, CST) ->
    case orddict:find(Link, CST) of
        {ok, L} -> L;
        error   -> []
    end.

next_child(Link, CST) ->
    case get_child(Link, CST) of
        [] -> missing;
        [N | Rest] ->
            {N, orddict:store(Link, Rest, CST)}
    end.

has_child(Link, CST) ->
    get_child(Link, CST) =/= [].


%% Return possible children link names from node structure
child_links(Struct, Lex) ->
    lists:usort(lists:flatten(child_links1(Struct, Lex))).

child_links1(Struct, Lex) ->
    lists:map(
      fun
          ({token,    _})    -> Lex;
          ({symbol,   Tag})  -> Tag;
          ({optional, Opt})  -> child_links1(Opt, Lex);
          ({repeat, _, Tag}) -> [Lex,Tag]
      end,
     Struct).


%% @spec parent(node()) -> [{atom(), node()}]
%% @doc Returns the syntactic parents of a node. Multiple parents are only
%% possible for preprocessed lexical nodes.
parent(Node) ->
    case ?Graph:data(Node) of
        {root}    -> [];
        #file{}   -> parent(Node, file);
        #form{}   -> parent(Node, form);
        #lex{}    -> parent(Node, [elex, llex, flex, clex]);
        Data      -> parent(Node, ?Nodes:parentlink(element(1, Data)))
    end.

parent(Node, [H|T]) ->
    case parent(Node, H) of
        []  -> parent(Node, T);
        Par -> Par
    end;
parent(_Node, []) ->
    [];
parent(Node, Link) when is_atom(Link) ->
    [{Link, Par} || Par <- ?Graph:path(Node, [{Link, back}])].

%% @spec leaves(node()) -> [node()]
%% @doc Returns the leaves of the syntactical subtree that starts at `Top',
%% in the correct syntactical order. This yields the original token nodes of
%% the subtree.
leaves(Top) ->
    Nodes = ets:new(visited_nodes, [set]),
    try
        lists:flatten(leaves(Top, Nodes))
    after
        %% TODO: investigate other representations' efficiency
        %% io:format("Records: ~b, memory: ~b~n",
        %%          [ets:info(Nodes, size), ets:info(Nodes, memory)]),
        ets:delete(Nodes)
    end.

leaves(Top, Nodes) ->
    case ets:lookup(Nodes, Top) of
        [] ->
            ets:insert(Nodes, {Top}),
            case children(Top) of
                []       -> [Top];
                Children -> [leaves(C, Nodes) || {_,C} <- Children]
            end;
        _  ->
            []
    end.


%% @spec tree_text(node()) -> Chars
%%       Chars = [char() | Chars]
%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top'.
tree_text(Top) ->
    [case ?ESG:data(Token) of
         #lex{data=#token{prews=Pre, text=Text, postws=Post}} ->
             [Pre, Text, Post];
         _ -> ""
     end || Token <- leaves(Top)].


%% @spec root_path(node()) -> [{atom(), node()}]
%% @doc Calls `root_path(Node, left)'.
%% @see root_path/2
root_path(Node) ->
    root_path(Node, left).

%% @spec root_path(node(), left|right) -> [{atom(), node()}]
%% @doc Returns the path from the root node to `Node'. Every path element
%% contains the link tag from the previous node and the next node. When the
%% path is not unique, `Choose' decides which path is taken: `left' chooses
%% the leftmost path in the syntax tree, `right' chooses the rightmost path.
root_path(Node, Choose) ->
    root_path(Node, Choose, []).

root_path(Node, Ch, Path) ->
    case parent(Node) of
        [] ->
            Path;
        [{Tag, Prev}] ->
            root_path(Prev, Ch, [{Tag, Node} | Path]);
        Parents ->
            select_path(Ch, ?Graph:root(),
                        [root_path(P, Ch, [{Tag, Node} | Path]) ||
                            {Tag, P} <- Parents])
    end.

select_path(_, _, [Path]) -> Path;
select_path(Ch, Prev, Paths) ->
    {Heads, Tails} = lists:unzip([{H,T} || [H|T] <- Paths]),
    case lists:usort(Heads) of
        [Next = {_Tag, Node}] ->
            [Next | select_path(Ch, Node, Tails)];
        Children ->
            Order = children(Prev),
            Selection = Order -- (Order -- Children),
            Next = {_Tag, Node} =
                if
                    Ch == left -> hd(Selection);
                    Ch == right  -> lists:last(Selection)
                end,
            [Next | select_path(Ch, Node, [T || [H|T] <- Paths, H =:= Next])]
    end.

%% @spec get_file(list() | node()) -> [node()]
%% @doc Returns the file that defines the node.
get_file(Lst) when is_list(Lst) ->
    lists:flatmap(fun get_file/1, Lst);
get_file(Node) ->
    Parents = ?Syn:parent(Node),
    Root = ?Graph:root(),
    if
        Parents =:= [{file, Root}] -> [Node];
        true -> get_file([P || {_,P} <- Parents])
    end.


%%% ============================================================================
%%% Syntax tree manipulation


%% @spec build(tuple(), [{atom(), node()}]) -> node()
%% @doc Build a new syntactic node from already existing children. This
%% function is used by the parser to construct the syntax tree.
build(Data, Links) ->
    Node = ?ESG:create(Data),
    lists:foreach(
        fun
            ({Tag, To}) -> ?ESG:insert(Node, Tag, To)
        end, lists:flatten(Links)),
    Node.


%% @spec create(data(), [ChildSpec]) -> node()
%%         ChildSpec = {Tag, node()} | string() |
%%                     {Tag, [node()]} | [ChildSpec]
%%         Tag = atom()
%%
%% @doc Creates a syntactic node. Children must be supplied in proper order,
%% except that lexical nodes may be left out -- keyword tokens needed by the
%% syntactical rules of the node are autogenerated.
%%
%% Returns the created node. Note that this node is not inserted into the
%% syntax tree, and will be deleted at the end of the current ESG batch if
%% you do not insert it explicitly.
create(Data, ChildSpec) ->
    Struct = ?Nodes:structure(Data),
    Lex = ?Nodes:lexlink(element(1,Data)),
    Children = create(Struct, Lex, unfold_childspec(ChildSpec, Lex)),
    Node = build(pp_node(Data), Children),
    update_attribs(Node, Data, Children, true),
    Node.

pp_node(F=#form{})   -> F#form{pp=node};
pp_node(C=#clause{}) -> C#clause{pp=node};
pp_node(E=#expr{})   -> E#expr{pp=node};
pp_node(D)           -> D.

unfold_childspec(CS, Lex) ->
    lists:flatmap(
      fun
          (Text=[Char|_]) when is_integer(Char) -> [{Lex, Text}];
          (Lst)           when is_list(Lst)     -> unfold_childspec(Lst, Lex);
          ({Tag, Lst})    when is_list(Lst)     -> [{Tag, N} || N <- Lst];
          (S)                                   -> [S]
      end,
      CS).


create([{token, Type} | ST], Lex, [{Lex, Spec} | CT]) ->
    [{Lex, generate(Type, Spec)} | create(ST, Lex, CT)];

create([{token, Type} | ST], Lex, CT) ->
    [{Lex, generate(Type)} | create(ST, Lex, CT)];

create([{symbol, Link} | ST], Lex, [{Link, Sym} | CT]) ->
    [{Link, Sym} | create(ST, Lex, CT)];

create([{repeat, Sep, Link}=R | ST], Lex, CT) ->
    create([{symbol, Link}, {optional, [{token, Sep}, R]} | ST], Lex, CT);

create([{optional, Opt} | ST], Lex, CT) ->
    case next_sym(CT, Lex) =:= opt_link(Opt) of
        true  -> create(Opt ++ ST, Lex, CT);
        false -> create(ST, Lex, CT)
    end;

create([], _Lex, []) ->
    [].

next_sym([{Lex, _} | Tail], Lex) -> next_sym(Tail, Lex);
next_sym([{Tag, _} | _],    _)   -> Tag;
next_sym([],                _)   -> [].

%% @spec copy(node()) -> [{node(),node()}]
%% @doc Makes a copy of a subtree. `Node' is the root of the subtree. The
%% return value is a mapping from the original nodes to the copied nodes.
%% @see referl_esg:copy/1.
copy(Node) ->
    ?ESG:copy(Node).


%% @spec replace(node(), ChildSel, Repl) -> [{Tag, node()}]
%%        ChildSel  = Tag | {Tag, Index} | {Tag, Start, Length} |
%%                    {node, node()} | {range, node(), node()}
%%        Repl      = [node()] | (([node()]) -> [node()])
%%        Tag       = atom()
%%        Index     = integer()
%%        Start     = integer()
%%        Length    = integer()
%%
%% @doc Replaces some children of a syntactical node. The children to be
%% replaced must form a continuous range; either this range or the
%% replacement may be an empty list (inserting children or removing
%% children). The replacement may be given either as a constant list, or may
%% be computed by a function from the replaced nodes.
%%
%% The return value is always the list of replaced children. These nodes
%% will be deleted at the end of the current ESG batch if they are not
%% inserted back into the syntax tree. Note that they can be returned from
%% the replacement function, in which case they will be inserted properly.
%%
%% Note that calling `replace/3' involves querying children, which means this
%% function cannot be called twice on the same node in the same ESG batch. If
%% more replacements should be done on a node, use `replace/2'.
replace(Node, ChildSel, Replacement) ->
    replace(Node, [{ChildSel, Replacement}]).

%% @spec replace(node(), [{ChildSel, Repl}]) -> [{Tag, node()}]
%% @doc Equivalent of calling {@link replace/3} with each of the child and
%% replacement specifications in the list, except that this works in a single
%% ESG batch.
replace(Node, RepList) ->
    Data = ?ESG:data(Node),
    Struct = ?Nodes:structure(Data),
    Lex = ?Nodes:lexlink(element(1,Data)),
    Children = children(Node, Struct, Lex, fun alldata/1),
    replace(Node, Data, Struct, Lex, Children, RepList).

replace(_, _, _, _, _, []) -> [];
replace(Node, Data, Struct, Lex, Children, [{ChildSel, Repl} | Tail]) ->
    {Pre, Link, Syms, Lexes, Post} = select(ChildSel, Children, Lex),
    {Result, NewData, NewChildren} =
        if
            Link =/= Lex ->
                replace_sym(Node, Data, Struct, Lex, Link,
                            Pre, Syms, Lexes, Post, Repl);
            Link =:= Lex ->
                replace_lex(Node, Data, Lex, Pre, Syms, Post, Repl)
        end,
    Result ++ replace(Node, NewData, Struct, Lex, NewChildren, Tail).

replace_sym(Node, Data, Struct, Lex, Link, Pre, Syms, Lexes, Post, Repl) ->
    NewSyms = replacement(Repl, Syms),
    [?ESG:remove(Node, Link, Sym) || Sym <- Syms],
    pp_children(NewSyms),
    CST = new_children(Pre, Link, NewSyms, Lex, Lexes, Post),
    NewChildren = rebuild(Struct, Node, Link, Lex, CST),
    {[{Link, Sym} || Sym <- Syms], Data, NewChildren}.

replace_lex(Node, Data, Lex, Pre, Syms, Post, Repl) ->
    {Types, LexNd} = lists:unzip(Syms),
    NewLex = replacement(Repl, LexNd),
    if
        length(LexNd) =:= length(NewLex) ->
            NewChildren =
                replace_lexes(Node, Lex, Pre, LexNd,
                              lists:zip(Types, NewLex), Post),
            NewData = update_attribs(Node, Data, NewChildren,
                                     fun pp_node/1, false),
            {[{Lex, Nd} || Nd <- LexNd], NewData, NewChildren};
        true ->
            erlang:error(bad_replacement)
    end.

replacement(Repl, Nodes) ->
    if
        is_function(Repl) -> Repl(Nodes);
        is_list(Repl)     -> Repl
    end.

pp_children(Children) ->
    lists:foreach(fun pp_child/1, Children).

pp_child(Node) ->
    Data = ?ESG:data(Node),
    NewData =
        case Data of
            #form{}=F   -> F#form{pp=child};
            #clause{}=C -> C#clause{pp=child};
            #expr{}=E   -> E#expr{pp=child};
            D           -> D
        end,
    if
        Data =/= NewData -> ?ESG:update(Node, NewData);
        true             -> ok
    end.

alldata({Tag, Type, Lex}) -> {Tag, {Type, Lex}};
alldata(Data) -> Data.

not_node(Node) ->
    fun
        ({_, {_, N}}) -> N /= Node;
        ({_, N})      -> N /= Node
    end.

%% Select children as specified to `replace/3'
select({node, Node}, Children, _Lex) ->
    {Pre, [{Link, Node} | Post]} =
        lists:splitwith(not_node(Node), Children),
    {Pre, Link, [Node], [], Post};

select({range, First, Last}, Children, Lex) ->
    {Pre, Tail} =
        lists:splitwith(not_node(First), Children),
    {Mid, [{Link, Last} | Post]} =
        lists:splitwith(not_node(Last), Tail),
    Repl = Mid ++ [{Link, Last}],
    case [C || C={T, _} <- Repl, T /= Link, T /= Lex] of
        [] ->
            {Pre, Link,
             [R || {T, R} <- Repl, T == Link],
             [L || {T, L} <- Repl, T == Lex],
             Post};
        _ ->
            erlang:error(bad_range)
    end;

select(Link, Children, Lex) when is_atom(Link) ->
    select({Link, 1, length([C || {L, C} <- Children, L =:= Link])},
           Children, Lex);

select({Link, Ind}, Children, Lex) ->
    select({Link, Ind, 1}, Children, Lex);

select({Link, Start, Len}, Children, Lex) ->
    {Pre, Syms, Lexes, Post} = select(Children, Link, Start, Len, Lex, []),
    {lists:reverse(Pre),
     Link,
     lists:reverse(Syms),
     lists:reverse(Lexes),
     Post}.

%% Find the starting point of the selection
select([{Link, _}=H | Tail]=Chld, Link, Start, Len, Lex, Pre) ->
    if
        Start > 1 ->
            select(Tail, Link, Start-1, Len, Lex, [H|Pre]);
        true ->
            select(Chld, Link, Len, Lex, Pre, [], [])
    end;
select([H | Tail], Link, Start, Len, Lex, Pre) ->
    select(Tail, Link, Start, Len, Lex, [H|Pre]);
select([], _, 0, _, _, Pre) ->
    {[], [], [], lists:reverse(Pre)};
select([], _, _, _, _, Pre) ->
    {Pre, [], [], []}.

%% Find the endpoint of the selection
select(Tail, _, 0, _, Pre, Syms, Lexes) ->
    {Pre, Syms, Lexes, Tail};
select([{Link, Sym} | Tail], Link, Len, Lex, Pre, Syms, Lexes) ->
    select(Tail, Link, Len-1, Lex, Pre, [Sym|Syms], Lexes);
select([{Lex, L} | Tail], Link, Len, Lex, Pre, Syms, Lexes) ->
    select(Tail, Link, Len, Lex, Pre, Syms, [L|Lexes]);
select([_|_Tail], _, _, _, _, _, _) ->
    erlang:error(bad_range);
select([], _, _, _, Pre, Syms, Lexes) ->
    {Pre, Syms, Lexes, []}.

%% Put old and new children in the right order
new_children(Pre, Link, Syms, Lex, Lexes, Post) ->
    child_set(
      [{Link,
        [N || {L,N} <- Pre, L =:= Link] ++
        [{new, S} || S <- Syms] ++
        [N || {L,N} <- Post, L =:= Link]},
       {Lex,
        [T || {L, T} <- Pre, L =:= Lex] ++
        Lexes ++
        [T || {L, T} <- Post, L =:= Lex]} |
       [{Ln,
         [N || {L, N} <- Pre, Ln =:= L] ++
         [N || {L, N} <- Post, Ln =:= L]} ||
           Ln <- lists:usort([L || {L, _} <- Pre ++ Post]),
           Ln =/= Link,
           Ln =/= Lex]]).

%% Generate missing tokens and drop unneeded tokens
rebuild(Struct, Node, Link, Lex, CST) ->
    rebuild(Struct, Node, Link, 1, Lex, 1, CST).

rebuild([{symbol, Link} | Tail], Node, Link, SI, Lex, LI, CST) ->
    case next_child(Link, CST) of
        {{new, Sym}, CST1} ->
            ?ESG:insert(Node, {Link, SI}, Sym);
        {Sym, CST1} ->
            ok
    end,
    [{Link, Sym} | rebuild(Tail, Node, Link, SI+1, Lex, LI, CST1)];

rebuild([{symbol, L} | Tail], Node, Link, SI, Lex, LI, CST) ->
    {Sym, CST1} = next_child(L, CST),
    [{L, Sym} | rebuild(Tail, Node, Link, SI, Lex, LI, CST1)];

rebuild([{token, Type} | Tail], Node, Link, SI, Lex, LI, CST) ->
    case next_child(Lex, CST) of
        {{Type, L}, CST1} ->
            ok;
        _ ->
            L = generate(Type),
            ?ESG:insert(Node, {Lex, LI}, L),
            CST1 = CST
    end,
    [{Lex, L} | rebuild(Tail, Node, Link, SI, Lex, LI+1, CST1)];

rebuild([{repeat, Sep, Ln}=R | Tail], Node, Link, SI, Lex, LI, CST) ->
    rebuild([{symbol, Ln}, {optional, [{token, Sep}, R]} | Tail],
            Node, Link, SI, Lex, LI, CST);

rebuild([{optional, Opt} | Tail], Node, Link, SI, Lex, LI, CST) ->
    case has_child(opt_link(Opt), CST) of
        true ->
            rebuild(Opt++Tail, Node, Link, SI, Lex, LI, CST);
        false ->
            CST1 = clear_tokens(Opt, Node, Lex, CST),
            rebuild(Tail, Node, Link, SI, Lex, LI, CST1)
    end;

rebuild([], _, _, _, _, _, _) -> [].

%% Remove the remaining tokens from a dropped `optional' part
clear_tokens([{token, Type} | Tail], Node, Lex, CST) ->
    case next_child(Lex, CST) of
        {{Type, L}, CST1} ->
            ?ESG:remove(Node, Lex, L),
            clear_tokens(Tail, Node, Lex, CST1);
        _ ->
            CST
    end;
clear_tokens([{repeat, Sep, _} | Tail]=Str, Node, Lex, CST) ->
    case next_child(Lex, CST) of
        {{Sep, L}, CST1} ->
            ?ESG:remove(Node, Lex, L),
            clear_tokens(Str, Node, Lex, CST1);
        _ ->
            clear_tokens(Tail, Node, Lex, CST)
    end;
clear_tokens([{optional, Opt} | Tail], Node, Lex, CST) ->
    CST1 = clear_tokens(Opt, Node, Lex, CST),
    clear_tokens(Tail, Node, Lex, CST1);
clear_tokens([{symbol, _} | Tail], Node, Lex, CST) ->
    clear_tokens(Tail, Node, Lex, CST);
clear_tokens([], _, _, CST) ->
    CST.

%% Replace lexical children
replace_lexes(Node, Lex, Pre, OldLex, NewLex, Post) ->
    LexWS = [{PreWS, PostWS} ||
                C <- OldLex,
                #lex{data=#token{prews=PreWS,
                                 postws=PostWS}} <- [?ESG:data(C)]],
    [?ESG:remove(Node, Lex, C) || C <- OldLex],
    Start = length([C || {L, C} <- Pre, L =:= Lex])+1,
    {Lexes, _} =
        lists:mapfoldl(
          fun ({WS, {Type, LexData}}, Ind) ->
                  Chld = generate(Type, LexData, WS),
                  ?ESG:insert(Node, {Lex, Ind}, Chld),
                  {{Lex, Chld}, Ind+1}
          end,
          Start,
          lists:zip(LexWS, NewLex)),
    Pre ++ Lexes ++ Post.

%% Generate a token node
generate(Type) ->
    generate(Type, atom_to_list(?Token:keyword_value(Type))).

generate(Type, Text) when is_list(Text) ->
    generate(Type, Text, {"", ""});
generate(_, Node) ->
    Node.

generate(Type, Text, {PreWS, PostWS}) ->
    ?ESG:create(#lex{type=token,
                     data=(?Token:build(Type, Text))#token{prews=PreWS,
                                                           postws=PostWS}}).


%% Update node attributes based on token values
update_attribs(Node, Data, Children, IsNew) ->
    update_attribs(Node, Data, Children, fun (D) -> D end, IsNew).

update_attribs(Node, Data, Children, Trf, IsNew) ->
    NewData =
        lists:foldl(
          fun ({{Link, Ind}, Field}, D) ->
                  Token = lists:nth(Ind, [T || {L, T} <- Children, L =:= Link]),
                  #token{value=Value} = ?Token:data(Token),
                  setelement(Field, D, Value)
          end,
          Trf(Data),
          ?Nodes:attribs(Data)),
    if
        NewData =/= Data ->
            case IsNew of
                true ->
                    ?ESG:update(Node, NewData);
                false ->
                    [{Link, Parent}] = parent(Node),
                    Index = index(Parent, Link, Node),
                    ?ESG:remove(Parent, Link, Node),
                    ?ESG:update(Node, NewData),
                    ?ESG:insert(Parent, {Link, Index}, Node)
            end;
        true ->
            ok
    end,
    NewData.

