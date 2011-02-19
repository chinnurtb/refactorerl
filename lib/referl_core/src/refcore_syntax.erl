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

%%% @doc Primary syntax tree interface. This module is intended to contain
%%% every operation that primarily works with the syntax tree representation
%%% of Erlang source code. This includes some token-related functionality,
%%% as they are stored as part of the syntax tree.
%%%
%%% @see refcore_esg
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refcore_syntax).
-vsn("$Rev: 5455 $ ").

%%% ============================================================================
%%% Exports

%% Node properties
-export([node_type/1]).
%% Queries
-export([first_leaf/0, last_leaf/0]).
%% Comment handling
-export([get_comments/1, put_comments/2]).
%% Syntax tree queries
-export([index/3, class/1]).
-export([children/1, children_idx/1, parent/1, leaves/1,
         tree_text/1, flat_text/1,
         root_path/1, root_path/2, get_file/1]).
-export([construct/1, create/2, replace/3, replace/2, copy/1]).
-export([is_virtual/1]).
%% Syntax tree manipulation
-export([build/2]).
%% Graph walk
-export([walk_graph/4, walk_graph/5]).
%% Link filtering
-export([filter_fun/1, reindex_links/1]).
%% Environment nodes
-export([create_env/2, create_lex/2]).
-export([add_env/2, set_env/2, get_envs/0, get_env/1, get_env/2,
         del_envs/0, del_env/1, del_env_sub/2, del_env_val/2, env_type/1]).



-include("core.hrl").
-include("refcore_schema.hrl").

%% TODO: do not use this header here
-include_lib("referl_lib/include/lib_export.hrl").

%%% ============================================================================
%%% Node properties

%% @spec node_type(Node::node()) -> atom()
%% @doc  The type of the given syntax node.
%% @deprecated Use {@link refcore_graph:class/1} instead.
node_type(Node) ->
    ?Graph:class(Node).

%%% ============================================================================
%%% Queries

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


%%% ============================================================================
%%% Comment handling

%% @spec get_comments(node() | [node()]) -> comments()
%% @doc Strips comments from the parameter nodes and stores them in a special
%% container. The stripped comment part is replaced with a single newline.
get_comments(Nodes) when is_list(Nodes) ->
    [{_Tag, Parent}] = parent(hd(Nodes)),
    Children = [N || {_T, N} <- children(Parent)],
    List     = cut(hd(Nodes), lists:last(Nodes), Children),
    lists:map(fun get_comments/1, List);
get_comments(Node) ->
    First = ?Query:exec1(Node, first_leaf(), error),
    Last  = ?Query:exec1(Node, last_leaf(),  error),

    {update_ws(First, #token.prews,  fun remcomment/1),
     update_ws(Last,  #token.postws, fun remcomment/1)}.


%% @spec put_comments(node() | [node()], comments()) -> ok
%% @doc The given nodes are decorated with the comments in the second
%% parameter. The comments should come from the same number of nodes and made
%% by {@link get_comments/1}. Formatting of nodes is preserved, comments are
%% inserted in place of newline characters if possible.
put_comments(Nodes, WS) when is_list(Nodes) ->
    [{_Tag, Parent}] = parent(hd(Nodes)),
    Children = [N || {_T, N} <- children(Parent)],
    List     = cut(hd(Nodes), lists:last(Nodes), Children),
    lists:zipwith(fun put_comments/2, List, WS);
put_comments(Node, {Pre, Post}) ->
    First = ?Query:exec1(Node, first_leaf(), error),
    Last  = ?Query:exec1(Node, last_leaf(),  error),

    update_ws(First, #token.prews, fun (WS) -> addcomment(WS, Pre, pre) end),
    update_ws(Last, #token.postws, fun (WS) -> addcomment(WS, Post, post) end),
    ok.

cut(From, To, List) ->
    %% cut garbage from the front
    Temp = lists:reverse(lists:dropwhile(fun(E) -> E =/= From end, List)),
    %% cut garbage from the end
    lists:reverse(lists:dropwhile(fun(E) -> E =/= To end, Temp)).

update_ws(Node, WSI, Fun) ->
    Data = #lex{data = Token} = ?ESG:data(Node),
    {WS, Rem} = Fun(element(WSI, Token)),
    ?ESG:update(Node, Data#lex{data = setelement(WSI, Token, WS)}),
    Rem.

remcomment(WS) ->
    {W,R} = rcpre(WS, ""),
    {lists:flatten(W), lists:flatten(R)}.

%% Find first comment sign
rcpre([],            Pre) -> {Pre, ""};
rcpre([$\% | _]=Cmt, Pre) -> rcmid(Cmt, "", [Pre, $\n]);
rcpre([C   | Tail],  Pre) -> rcpre(Tail, [Pre, C]).

%% Find end of comment
rcmid([],           Cmt, Pre) -> {Pre, Cmt};
rcmid([$\n | Tail], Cmt, Pre) -> rcpost(Tail, "", [Cmt, $\n], Pre);
rcmid([C   | Tail], Cmt, Pre) -> rcmid(Tail, [Cmt, C], Pre).

%% Find next comment or end of string
rcpost([],             Post, Cmt, Pre) -> {[Pre, Post], Cmt};
rcpost([$\% | _]=Next, Post, Cmt, Pre) -> rcmid(Next, [Cmt, Post], Pre);
rcpost([C   | Tail],   Post, Cmt, Pre) -> rcpost(Tail, [Post, C], Cmt, Pre).

addcomment(WS, Cmt, Place) ->
    {Post, Pref} = lists:splitwith(fun (C) -> C =/= $\n end,
                                   lists:reverse(WS)),
    {case Pref of
         "" when Place =:= pre ->
             Cmt ++ lists:reverse(Post);
         "" when Place =:= post ->
             lists:reverse(Post) ++ Cmt;
         [$\n|Tail] ->
             "\n" ++ lists:reverse(Tail) ++ Cmt ++ lists:reverse(Post)
     end,
     ""}.


%%% ============================================================================
%%% Syntax tree queries

%% @spec index(node(), atom(), node()) -> integer()
%% @doc Returns the index of a link.
%% @see refcore_esg:index/3
index(Parent, Link, Child) ->
    ?Graph:index(Parent, Link, Child).

%% @spec class(node()) -> atom()
%% @doc Returns the node class of the node.
%% @see refcore_graph:class/1.
class(Node) ->
    ?Graph:class(Node).


%% @spec children_idx(Node::node()) ->
%%           [{LikTag::atom(), Children::node(), LinkIndex::natural()}]
%% @doc Returns the childrens of the node in the order as they appear in the
%%      real syntax tree.
children_idx(Node) ->
    element(1, lists:mapfoldl(
        fun({T,N}, Dict) ->
            I = case dict:find(T, Dict) of
                {ok, LastIdx} -> LastIdx+1;
                _ -> 1
            end,
            {{T,N,I}, dict:update_counter(T,1,Dict)}
        end,
        dict:new(),
        children(Node))).


%% @spec children(Node::node()) -> [{LinkTag::atom(), Children::node()}]
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
    children(Node, ?ErlNodes:structure(Data),
             ?ErlNodes:lexlink(element(1,Data)), Fun).
children(Node, Struct, Lex, Fun) ->
    ChildSet = child_set(unord_children(Node,Struct,Lex)),
    try
        ord_children(Struct, Lex, ChildSet, Fun)
    catch
        error:{badmatch, missing} ->
            throw({invalid_children, Node, Struct, ChildSet})
    end.

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
    % If the optional contains only tokens and optionals,
    % the presence of the first token is checked
    % (which is presumed to be the only token of its type in the construct).
    % Otherwise, the presence of the first symbol is checked.
    Condition =
        case {only_opts_and_tokens(Opt), Opt} of
            { true, [{token, T}|_]} ->
                %% TODO: use get_child here
                case orddict:find(Lex, CST) of
                    {ok, [Token1|_]} ->
                        Token = find_orig_token(Token1),
                        Token#token.type == T;
                    _ ->
                        false
                end;
            {false, _} ->
                has_child(opt_sym_link(Opt), CST)
        end,

    case Condition of
        true  -> ord_children(Opt ++ Tail, Lex, CST, Fun);
        false -> ord_children(Tail, Lex, CST, Fun)
    end.

opt_sym_link([{token, _}        | Tail])  -> opt_sym_link(Tail);
opt_sym_link([{symbol, Link}    | _])   -> Link;
opt_sym_link([{repeat, _, Link} | _])   -> Link.


find_orig_token(LexNode) ->
    case ?Graph:data(LexNode) of
        #lex{data=virtual} ->
            [NextLex] = ?Graph:path(LexNode, [orig]),
            find_orig_token(NextLex);
        #lex{data=Token}   -> Token
    end.

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
        #lex{}    -> parent(Node, [elex, llex, flex, clex, tlex]);
        Data      -> parent(Node, ?ErlNodes:parentlink(element(1, Data)))
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
%% starts at `Top' as a deep list.
tree_text(Top) ->
    [[T#token.prews, T#token.text, T#token.postws]
        || Token <- leaves(Top),
           #lex{data=T=#token{}} <- [?ESG:data(Token)]].


%% @spec flat_text(node()) -> [char()]
%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top'.
flat_text(Top) ->
    lists:flatten(tree_text(Top)).


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

%%% ----------------------------------------------------------------------------
%%% Terse syntactic node creation

%% @doc Using a description consisting of nested tuples, constructs a syntax subtree.
%% Example: `{app, Fun, {var, VarName}}'.
%% In contrast to `create', which has to be called for all new nodes,
%% the preferred usage of `construct' is to collect the node creation information
%% and then call the function only once.
%% @todo This is still a work in progress, not all constructs are supported.
%% @todo Make constructs that can be inferred omissible.
%%       Example: {list_gen, Pattern, Generator} instead of
%%                {list_gen, {pexpr, Pattern}, {clause_expr, Generator}}
construct(Node = {'$gn', _, _}) -> Node;
construct({Node1, InfixAtom, Node2}) when (InfixAtom == 'orelse') or
					  (InfixAtom == 'andalso') ->
    L = create(#clause{type=expr}, [{body, construct(Node1)}]),
    R = create(#clause{type=expr}, [{body, construct(Node2)}]),
    create(#expr{type=infix_expr, value=InfixAtom}, [{exprcl, L}, {exprcl, R}]);
construct({Node1, InfixAtom, Node2}) when is_atom(InfixAtom) ->
    GNode1 = case Node1 of %% needed by upgrade_iface:simple_infix_expr/1
                 {'$gn', _, _} -> Node1; %% TODO: a more general solution
                 _ -> construct(Node1)
             end,
    GNode2 = construct(Node2),
    create(#expr{role=expr, type=infix_expr, value=InfixAtom},
            [{esub, GNode1}, {esub, GNode2}] );
construct(Tuple) when is_tuple(Tuple) ->
    [Type|OrigArgs] = tuple_to_list(Tuple),
    Args = [transform_arg(Arg) || Arg <- OrigArgs],
    construct(Type, Args).


transform_arg(Args) when is_list(Args) ->
    [transform_arg(Arg) || Arg <- Args];
transform_arg(Arg) ->
    case {is_tuple(Arg), ?Graph:is_gnode(Arg)} of
        {true, false} -> construct(Arg);
        _             -> Arg
    end.


%% @doc Creates a lexical element from the given token type and data.
create_lex(Type, Data) ->
    ?ESG:create(#lex{type=token, data=?Token:build(Type, Data)}).

%% @doc This function translates the arguments of `construct' to calls to `create'.
%% @todo This function should be automatically generated.
%%       from the syntax description.
construct({file, Path}, [Forms]) ->
    File = ?ESG:create(#file{type=module, path=Path, eol={lf,eol}, lastmod=now()}),
    ?ESG:insert(?ESG:root(), file, File),
    ?Graph:mklink(File, incl, File),
    [?ESG:insert(File, form, F) || F <- Forms],
    ?ESG:finalize(),
    File;

construct({form, Type, Name}, [Attr]) ->
    create(#form{type=Type},
           [{flex, create_lex(minus, "-")},
            {flex, create_lex(atom, io_lib:write(Type))},
            {flex, create_lex('(', atom_to_list('('))},
            {flex, create_lex(atom, io_lib:write(Name))},
            {tattr, Attr}]);
construct({form, Type}, [Attr]) ->
    create(#form{type=Type}, [{eattr, Attr}]);
construct({form, file}, [FileName, Line]) ->
    create(#form{type=file}, [{eattr, FileName}, {eattr, Line}]);
construct({attrib, Tag}, [Attr]) ->
    create(#form{type=attrib, tag=Tag}, [{eattr, Attr}]);

construct(funlist, [Elem]) ->
    create(#expr{role=attr, type=funlist}, [{esub, Elem}]);
construct(funref, [Name, Arity]) ->
    create(#expr{role=attr, type=funref}, [{esub, Name}, {esub, Arity}]);

construct(func, [Def]) ->
    create(#form{type=func}, [{funcl, D} || D <- Def]);
construct(fun_clause, [Name, Pattern, Guard, Body]) ->
    P = [{pattern, Node} || Node <- Pattern],
    B = [{body,    Node} || Node <- Body],
    create(#clause{var=scope, type=fundef}, [{name, Name} | P] ++ [{guard, Guard} | B]);

construct({spec_field, Name}, [DefaultVal]) ->
    create(#typexp{type=spec_field},
           [{tlex, create_lex(atom, io_lib:write(Name))},
            {texpr, DefaultVal}]);
construct({record_field, Field}, [Value]) ->
    create(#expr{type=record_field},
           [{elex, create_lex(atom, io_lib:write(Field))},
            {esub, Value}]);
construct({record_access, Name}, [Access, Value]) ->
    create(#expr{type=record_access},
           [{esub, Access},
            {elex, create_lex('#', atom_to_list('#'))},
            {elex, create_lex(atom, io_lib:write(Name))},
            {esub, Value}]);
construct({record_expr, Name}, [Fields]) ->
    FieldList = create(#expr{type=field_list}, [{esub, F} || F <- Fields]),
    create(#expr{type=record_expr},
           [{elex, create_lex('#', atom_to_list('#'))},
            {elex, create_lex(atom, io_lib:write(Name))},
            {esub, FieldList}]);
construct({record_index, Name}, [Field]) ->
    create(#expr{type=record_index, value=Name},
           [{elex, create_lex('#', atom_to_list('#'))},
            {elex, create_lex(atom, io_lib:write(Name))},
            {esub, Field}]);
construct({record_update, Name}, [Var, Fields]) ->
    F = create(#expr{type=field_list}, [{esub, F} || F <- Fields]),
    create(#expr{type=record_update},
           [{esub, Var},
            {elex, create_lex('#', atom_to_list('#'))},
            {elex, create_lex(atom, io_lib:write(Name))},
            {esub, F}]);

construct(tuple, [Elements]) ->
    create(#expr{type=tuple}, [{esub, E} || E <- Elements]);

construct(app, [Fun, Args]) ->
    ArgList = create(#expr{type=arglist}, [{esub, A} || A <- Args]),
    create(#expr{type=application}, [{esub, Fun}, {esub, ArgList}]);

construct(fun_scope, [Patterns, Guard, Bodys]) ->
    P = [{pattern, Node} || Node <- Patterns],
    B = [{body,    Node} || Node <- Bodys],
    create(#clause{var=scope, type=funexpr}, P ++ [{guard, Guard} | B]);
construct('fun', [Clauses]) ->
    create(#expr{role=expr, type=fun_expr}, [{exprcl, C} || C <- Clauses]);
construct(implicit_fun, [Name, Arity]) ->
    create(#expr{role=expr, type=implicit_fun}, [{esub, Name}, {esub, Arity}]);

construct(pattern, [Pattern, Guard, Body]) ->
    P = [{pattern, Node} || Node <- Pattern],
    B = [{body,    Node} || Node <- Body],
    create(#clause{type=pattern}, P ++ [{guard, Guard} | B]);
construct(guard, [Guard, Body]) ->
    B = [{body, Node} || Node <- Body],
    create(#clause{type=guard}, [{guard, Guard} | B]);
construct(var_pattern, [Name]) ->
    create(#expr{role=pattern, type=variable}, [Name]);

construct(paren, [Expr]) ->
    create(#expr{type=parenthesis}, [{esub, Expr}]);


construct(block_expr, [Bodys]) ->
    B = create(#clause{type=block}, [{body, B} || B <- Bodys]),
    create(#expr{type=block_expr}, [{exprcl, B}]);
construct('case', [HeadCl, Branches]) ->
    H = create(#clause{type=expr}, [{body, HeadCl}]),
    create(#expr{type=case_expr}, [{headcl, H} | [{exprcl, B} || B <- Branches]]);
construct('catch', [Expr]) ->
    E = create(#clause{type=expr}, [{body, Expr}]),
    create(#expr{type=catch_expr}, [{exprcl, E}]);
construct('if', [Clauses]) ->
    create(#expr{type=if_expr}, [{exprcl, C} || C <- Clauses]);
construct('receive', [Clauses, TimeOut]) ->
    T = case TimeOut of
            [] ->
                [];
            [Time, TBody] ->
                [{aftercl, create(#clause{type=timeout}, [{tmout, Time} | [{body, B} || B <- TBody]])}]
        end,
    create(#expr{type=receive_expr}, [{exprcl, C} || C <- Clauses] ++ T);
construct('try', [Head, Expr, Catch, After]) ->
    H = create(#clause{type=block}, [{body, H} || H <- Head]),
    C = case Catch of
            [] ->
                [];
            Catch ->
                [{catchcl, Catch}]
        end,
    A = case After of
            [] ->
                [];
            After ->
                [{aftercl, create(#clause{type=block}, [{body, A} || A <- After])}]
        end,
    create(#expr{type=try_expr}, [{headcl, H}, {exprcl, Expr}] ++ C ++ A);

construct(atom, [Name]) ->
    create(#expr{type=atom, value=Name}, [io_lib:write(Name)]);
construct(integer, [Integer]) ->
    create(#expr{type=integer}, [?MISC:to_list(Integer)]);
construct(float, [Float]) ->
    create(#expr{type=float}, [?MISC:to_list(Float)]);
construct(string, [Value]) ->
    create(#expr{type=string}, ["\"" ++ Value ++ "\""]);
construct(var, [Name]) ->
    create(#expr{type=variable}, [?MISC:to_list(Name)]);

construct(joker, [[]]) ->
    create(#expr{type=joker}, []);

construct(bin, [Fields]) ->
    create(#expr{type=binary}, [{esub, F} || F <- Fields]);
construct(bin_comp, [BitString, Qualifiers]) ->
    B = create(#clause{type=hexpr}, [{body, BitString}]),
    Q = create(#clause{type=compr}, [{body, Q} || Q <- Qualifiers]),
    create(#expr{role=expr, type=bin_comp}, [{exprcl, B}, {exprcl, Q}]);
construct(binary_field, [Fields]) ->
    create(#expr{type=binary_field}, [{esub, F} || F <- Fields]);
construct({bit_size_expr, Size}, [Type]) ->
    create(#expr{role=expr, type=bit_size_expr, value=Size},
           [{esub, Type},
            {elex, create_lex(colon, ":")},
            {elex, create_lex(integer, ?MISC:to_list(Size))}]);
construct(bin_gen, [Pattern, Generator]) ->
    P = create(#clause{type=pexpr}, [{pattern, Pattern}]),
    G = create(#clause{type=expr}, [{body, Generator}]),
    create(#expr{role=expr, type=binary_gen}, [{exprcl, P}, {exprcl, G}]);
construct(size_qualifier, [Field, Size]) ->
    create(#expr{type=size_qualifier}, [{esub, Field}, {esub, Size}]);

construct(cons, Elem) ->
    create(#expr{type=cons}, [{esub, E} || E <- Elem]);
construct(list, [Elem]) ->
    case Elem of
        [] ->
            [];
        Elem ->
            create(#expr{type=list}, [{esub, E} || E <- Elem])
    end;
construct(list_gen, [Pattern, Generator]) ->
    P = create(#clause{type=pexpr}, [{pattern, Pattern}]),
    G = create(#clause{type=expr}, [{body, Generator}]),
    create(#expr{role=expr, type=list_gen}, [{exprcl, P}, {exprcl, G}]);
construct(filter, [Filter]) ->
    C = create(#clause{type=compr}, [{body, Filter}]),
    create(#expr{role=expr, type=filter}, [{exprcl, C}]);
construct(list_comp, [Expr, Qualifier]) ->
    E = create(#clause{type=hexpr}, [{body, Expr}]),
    Q = create(#clause{type=compr}, [{body, Q} || Q <- Qualifier]),
    create(#expr{role=expr, type=list_comp}, [{exprcl, E}, {exprcl, Q}]);

construct({prefix_expr, PrefixAtom}, [Operand]) ->
    create(#expr{type=prefix_expr, value=PrefixAtom}, [{esub, Operand}]);
construct({infix_expr, InfixAtom}, [LeftOp, RightOp]) when (InfixAtom == 'orelse') or (InfixAtom == 'andalso') ->
    L = create(#clause{type=expr}, [{body, LeftOp}]),
    R = create(#clause{type=expr}, [{body, RightOp}]),
    create(#expr{type=infix_expr, value=InfixAtom}, [{exprcl, L}, {exprcl, R}]);
construct({infix_expr, InfixAtom}, [LeftOp, RightOp]) ->
    create(#expr{type=infix_expr, value=InfixAtom}, [{esub, LeftOp}, {esub, RightOp}]);
construct(send_expr, [LeftOp, RightOp]) ->
    create(#expr{type=send_expr}, [{esub, LeftOp}, {esub, RightOp}]);
construct(match_expr, [LeftOp, RightOp]) ->
    create(#expr{type=match_expr}, [{esub, LeftOp}, {esub, RightOp}]).


%%% ----------------------------------------------------------------------------
%%% Verbose syntactic node creation

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
    Struct = ?ErlNodes:structure(Data),
    Lex = ?ErlNodes:lexlink(element(1, Data)),
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
    case only_opts_and_tokens(Opt) of
        true ->
            %% Note that this means optional tokens are always generated.
            create(Opt ++ ST, Lex, CT);
        false ->
            case next_sym(CT, Lex) =:= opt_sym_link(Opt) of
                true  -> create(Opt ++ ST, Lex, CT);
                false -> create(ST, Lex, CT)
            end
    end;

create([], _Lex, []) ->
    [].

%% If the node structure description contains only tokens and optionals
%% within an optional construct, we will want to autocreate it.
only_opts_and_tokens([])                    -> true;
only_opts_and_tokens([{token,_}    | Tail]) -> only_opts_and_tokens(Tail);
only_opts_and_tokens([{optional,_} | Tail]) -> only_opts_and_tokens(Tail);
only_opts_and_tokens(_)                     -> false.

next_sym([{Lex, _} | Tail], Lex) -> next_sym(Tail, Lex);
next_sym([{Tag, _} | _],    _)   -> Tag;
next_sym([],                _)   -> [].

%% @spec copy(node()) -> [{node(),node()}]
%% @doc Makes a copy of a subtree. `Root' is the root node of the subtree. The
%% return value is a mapping from the original nodes to the copied nodes.
%% @todo support macros inside the subtree
copy(Root) ->
    Tree = ets:new(copytree, []),
    try
        collect_tree(Tree, Root),
        Mapping =
            ets:foldl(
              fun ({Node, Data, Children}, M) ->
                      NewNode = ?ESG:create(Data),
                      ets:insert(Tree, {Node, NewNode, Children}),
                      [{Node, NewNode} | M]
              end,
              [], Tree),
        ets:foldl(
          fun({_, Node, Children}, A) ->
                  [begin
                       [{_, New, _}] = ets:lookup(Tree, Child),
                       ?ESG:insert(Node, Tag, New)
                   end || {Tag, CL} <- Children,
                          Child <- CL],
                  A
          end, [], Tree),
        Mapping

    after
        ets:delete(Tree)
    end.

collect_tree(Tree, Root) ->
    case ets:member(Tree, Root) of
        true -> ok;
        false -> collect_children(Tree, Root)
    end.

collect_children(Tree, Root) ->
    Data = ?ESG:data(Root),
    Children = [get_children(Root, Tag) || Tag <- children_tags(Data)],
    ets:insert(Tree, {Root, Data, Children}),
    [collect_tree(Tree, Node) || {_, Nodes} <- Children, Node <- Nodes],
    ok.

get_children(Root, Tag) ->
    case Tag == elex andalso is_module_mac(Root) of
        true ->
            Mod = (?ESG:data(Root))#expr.value,
            Children = [?ESG:create(#lex{type=token,
                               data=(?Token:build(atom, atom_to_list(Mod)))})];
        _    ->
            Nodes = ?ESG:path(Root, [Tag]),
            Children = [case is_virtual(Node) of
                true ->
                    [Token] = ?ESG:path(Node, [orig]),
                    Token;
                _   ->
                    Node
            end || Node <- Nodes]
    end,
    {Tag, Children}.

%% Returns whether the node is a virtual token.
is_virtual(Node) ->
    ?Graph:class(Node) == lex andalso
    (?ESG:data(Node))#lex.data == virtual andalso
    ?ESG:path(Node, [orig]) =/= [].

is_module_mac(Node) ->
    Lexes = ?ESG:path(Node, [elex, llex, llex]),
    if
        length(Lexes) == 2 ->
	    [Q, M] = Lexes,
            ((?ESG:data(Q))#lex.data)#token.type == '?' andalso
                ((?ESG:data(M))#lex.data)#token.value == "MODULE";
        true -> false
    end.

children_tags(#lex{}) -> [llex];
children_tags(Data) ->
    [?ErlNodes:lexlink(element(1, Data)) |
     lists:usort(structure_tags(?ErlNodes:structure(Data)))].

structure_tags(Struct) ->
    lists:flatmap(
      fun
          ({symbol, Tag}) -> [Tag];
          ({token, _}) -> [];
          ({repeat, _, Tag}) -> [Tag];
          ({optional, Opt}) -> structure_tags(Opt)
      end,
      Struct).


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
    Struct = ?ErlNodes:structure(Data),
    Lex = ?ErlNodes:lexlink(element(1,Data)),
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
    [?ESG:remove(Node, Link, Sym) || Sym <- Syms],
    NewSyms = replacement(Repl, Syms),
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
            #form{}=F   -> F#form{pp=node};
            #clause{}=C -> C#clause{pp=node};
            #expr{}=E   -> E#expr{pp=node};
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

rebuild([{optional, [{token, T}]} | Tail], Node, Link, SI, Lex, LI, CST) ->
    rebuild([{token, T}|Tail], Node, Link, SI, Lex, LI, CST);

rebuild([{optional, Opt} | Tail], Node, Link, SI, Lex, LI, CST) ->
    case has_child(opt_sym_link(Opt), CST) of
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
    NewPostWS =
        case Type of
            stop -> PostWS ++ "\n";
            _    -> PostWS
        end,
    ?ESG:create(#lex{type=token,
             data=(?Token:build(Type, Text))#token{prews=PreWS,
                                                   postws=NewPostWS}}).

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
          ?ErlNodes:attribs(Data)),
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


%%% ============================================================================
%%% Graph walk

%% @type callback(Args, Result). A callback function receiving
%% the arguments listed in `Args' and returning `Result'.

%% @spec walk_graph(gnode(), atom(), Callback, state()) -> state()
%% where
%%        Callback     = callback(state(), StepCallback, state())
%%        StepCallback = callback(gnode(), state(), boolean())
%% @doc Walks the graph starting from `node', traversing syntax links first.
%% For each node, `ActOnNode' is called, which in turn calls `WalkNext'.
%% This way, `ActOnNode' can be written with more flexibility,
%% e.g. preorder and postorder walks can be made.
walk_graph(Node, Filter, ActOnNode, State) ->
    NoAdds = fun(_) -> [] end,
    walk_graph(Node, Filter, ActOnNode, State, NoAdds).

walk_graph(Node = {'$gn', FromType, _}, Filter, ActOnNode, State, AddNodes) ->
    Links = node_links(Node, FromType, Filter),
    {_, Nodes} = lists:unzip(Links),
    Adds       = AddNodes(Node),
    WalkNext   =
        fun (NewSt, NodeIsDone) ->
            lists:foldl(
                fun(To, St) ->
                    case NodeIsDone(To, St) of
                        true  -> St;
                        false -> walk_graph(To, Filter, ActOnNode, St, AddNodes)
                    end
                end, NewSt, Nodes ++ Adds)
        end,
    ActOnNode(Node, State, Links, WalkNext).

%% Returns the outgoing links of the graph node, syntax links first and in order.
node_links(Node, FromType, Filter) ->
    FilterFun = filter_fun(Filter),
    NodeClass = element(1, ?Graph:data(Node)),
    Links = [Link || Link = {Tag, _To} <- ?Graph:links(Node),
                     FilterFun(NodeClass, Tag)],
    IsSynNode =
        case ?Graph:data(Node) of
            #expr{type=fpar} -> false;
            #expr{type=fret} -> false;
            #expr{role=dflow} -> false;
            _ ->
                schema_has(?SYNTAX_SCHEMA, FromType) orelse
                schema_has(?LEXICAL_SCHEMA, FromType)
        end,
    case IsSynNode of
        false ->
            Links;
        true ->
            Children = children(Node),
            {ChildTags, _} = lists:unzip(Children),
            {_SynLinks, OtherLinks} =
                lists:partition(
                    fun({Tag, _}) ->
                        lists:member(Tag, ChildTags)
                        % ~ is_syn_link(NodeClass, Tag) orelse is_lex_link(NodeClass, Tag)
                    end, Links),
            FilteredOrdChildren =
                [Link || Link = {Tag, _To} <- children(Node),
                         FilterFun(NodeClass, Tag)],
            FilteredOrdChildren ++ OtherLinks
    end.


%% @doc Adds indices increasing by tag, as they appear in the graph.
reindex_links(L) -> reindex_links(L, []).

reindex_links([], _) -> [];
reindex_links([{Tag, To} | Tail], Idxs) ->
    Idx = proplists:get_value(Tag, Idxs, 1),
    [{Tag, Idx, To} | reindex_links(Tail, orddict:store(Tag, Idx+1, Idxs))].

%%% ============================================================================
%%% Link filtering

%% @doc Returns the edge filter function for filter type FType.
filter_fun(FType) ->
    fun(From, Tag) ->
        [] /= [ ok || Filter <- filters(FType), Filter(From, Tag) ]
    end.


%% @doc Returns the functions whose disjunction comprises the
%%      edge filter function. If an unknown filter is encountered, `all' is used.
filters(Filters) when is_list(Filters) ->
    lists:usort(lists:concat([F || F <- Filters]));
filters(synsem)  -> filters([syn, sem]);
filters(semsyn)  -> filters([syn, sem]);
filters(synlex)  -> filters([syn, lex]);
filters(all_env) -> [fun all_links_with_env/2];
filters(sem)     -> [fun is_sem_link/2, fun is_ctx_link/2];
filters(ctx)     -> [fun is_ctx_link/2, fun is_syn_link/2];
filters(syn)     -> [fun is_syn_link/2];
filters(lex)     -> [fun is_form/2, fun is_lex_link/2];
filters(not_lex) -> [fun not_lex/2];
filters(dataflow)-> [fun is_syn_link/2, fun is_data_flow_link/2];
filters(semdf)   -> [fun is_semdf_link/2];
filters(_)       -> [fun all_links/2].

%%% ----------------------------------------------------------------------------
%%% Link types

all_links_with_env(_, _) -> true.

all_links(_, To) -> To =/= env.

is_form(_, To) -> To =:= form.

%is_lex_link(_, To) -> lists:member(To, [elex, flex, clex]).
is_lex_link(FromType, ToType) -> schema_has(?LEXICAL_SCHEMA, FromType, ToType).

is_syn_link(FromType, ToType) -> schema_has(?SYNTAX_SCHEMA, FromType, ToType).


%% todo Would be more elegant if the analyser modules supplied this information.
is_ctx_link(clause, scope) -> true;
is_ctx_link(clause, visib) -> true;
is_ctx_link(expr, clause)  -> true;
is_ctx_link(expr, top)     -> true;
is_ctx_link(_,_)           -> false.

%% todo Would be more elegant if the analyser modules supplied this information.
is_sem_link(root, Tag)   -> Tag == module;
is_sem_link(file, Tag)   -> Tag == moddef;
is_sem_link(form, Tag)   -> Tag == fundef;
is_sem_link(clause, Tag) -> lists:member(Tag, [modctx, vardef, varvis]);
is_sem_link(expr, Tag)   -> lists:member(Tag, [modref, varref, varbind, funlref, funeref,
                                               flow, sel_e, cons_e, sel, cons]);
%is_sem_link(expr, Tag)   -> lists:member(Tag, [modref, varref, varbind, funlref, funeref,
%                                               flow, sel_e, cons_e, sel, cons_back]);
is_sem_link(Class, _)    -> lists:member(Class, [module, variable, func]).

is_semdf_link(root, Tag)   -> Tag == module;
is_semdf_link(file, Tag)   -> Tag == moddef;
is_semdf_link(form, Tag)   -> Tag == fundef;
is_semdf_link(expr, Tag)   -> lists:member(Tag, [modref, funeref,
                                               flow, sel_e, cons_e, sel, cons, dep]);
%is_semdf_link(expr, Tag)   -> lists:member(Tag, [modref, funeref,
%                                               flow, sel_e, cons_e, sel, cons_back, dep]);
is_semdf_link(Class, _)    -> lists:member(Class, [module, func]).

is_data_flow_link(expr, Tag) ->
    lists:member(Tag, [flow, sel, sel_e, cons_e, cons, dep]);
%is_data_flow_link(expr, Tag) ->
%    lists:member(Tag, [flow, sel, sel_e, cons_e, cons_back, dep]);
is_data_flow_link(_,_) -> false.

not_lex(_,    file) -> true;
not_lex(Node, Tag)  -> not is_lex_link(Node, Tag).


%% @doc  Returns if FromType is connected to ToType in the given schema.
schema_has(Schema, FromType, ToType) ->
    [] /= [ found || {From2, _, Tos} <- Schema
                   , {To2, _} <- Tos
                   , From2 == FromType
                   , To2   == ToType ].

%% @doc Returns whether the node type is present in the schema.
schema_has(Schema, Type) ->
    [] /= [ found || {From2, _, Tos} <- Schema
                   , {To2, _} <- Tos
                   , From2 == Type orelse To2 == Type ].


%%% ============================================================================
%%% Environment nodes
%%% @todo introduce typedness

%% @doc  Creates an environment node with the given data.
%% @todo Make an inverse operation.
%% @todo unexport
create_env(Name, Value) ->
    Data = #env{name = Name, value = Value},
    Node = ?Graph:create(Data),
    ?Graph:mklink(?Graph:root(), env, Node).

%% @doc  Adds a new environment node if it does not already exists
add_env(Name,Value)->
    case lists:member(Value,get_env(Name)) of
        true ->
            ok;
        false ->
            create_env(Name,Value)
    end.

%% @doc  Sets a new value for an environment node
set_env(Name,Value)->
    Old = p_named_env(Name),
    case create_env(Name,Value) of
        ok ->
            Del = lists:map(fun ?Graph:delete/1, Old),
            case lists:all(fun(ok)->true; (_)->false end, Del) of
                true  -> ok;
                false -> Del
            end;
        Err ->
            Err
    end.

%% @doc Returns the type of an environment node
env_type(env_var) ->
    proplist;
env_type(_)->
    atomic.

%% @doc  Returns all environment nodes.
get_envs() ->
    L = [{Name,Value} ||
            Env <- p_all_env(),
            #env{name=Name,value=Value} <- [?Graph:data(Env)]],
    [{K,proplists:get_all_values(K,L)} || K <- proplists:get_keys(L)].

%% @doc  Returns the values of environment nodes with the given name.
get_env(Name) ->
    [(?Graph:data(Env))#env.value || Env <- p_named_env(Name)].

%% @doc  Returns the looked-up subvalues from the environment variable.
get_env(Name, EnvName) ->
    proplists:get_all_values(EnvName, get_env(Name)).

%% @doc  Deletes all environment nodes.
del_envs() ->
    [?Graph:delete(Env) || Env <- p_all_env()].

%% @doc  Deletes environment nodes that have the name `Name'.
del_env(Name) ->
    [?Graph:delete(Env) || Env <- p_named_env(Name)].

%% @doc  Deletes environment node subentries with key `EnvName' from the
%% environment named `Name'.
del_env_sub(Name, EnvName) ->
    [?Graph:delete(Env) ||
        Env <- p_named_env(Name),
        {EN,_} <- [(?Graph:data(Env))#env.value],
        EN==EnvName].

%% @doc  Deletes environment node entries of name `Name' which are associated
%% with the value `Value'.
del_env_val(Name, Value) ->
    Envs = ?Graph:path(?Graph:root(), [{env, {{name, '==', Name}, 'and',
                                             {value, '==', Value}}}]),
    [?Graph:delete(Env) || Env <- Envs].

p_all_env()->
    ?Graph:path(?Graph:root(), [env]).

p_named_env(Name)->
    ?Graph:path(?Graph:root(), [{env, {name, '==', Name}}]).
