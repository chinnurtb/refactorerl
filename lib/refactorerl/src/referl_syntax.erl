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
%%% @see referl_lex
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(referl_syntax).
-vsn("$Rev: 1983 $").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

-export([file/1, get_file/1, top_node/2, form_type/1,
         file_by_modulename/1, function_forms/2, module_qualifier/1,
         form_def_path/1, form_inc_path/1]).
-export([child_node_order/1, structure_with_nodes/1]).
-export([children/1, leaves/1, tree_text/1]).
-export([create/2, replace/3]).
-export([create_by_struct/2]).

-export([single_parent/1, single_parent_link/1]).

-export([all_syntax_nodes/1]).
-export([path_from_root/1]).
-export([type/1]).
-export([ancestor_with_type/2]).

% TODO is this export needed?
%-export([split_at_link/3]).

%%% ============================================================================

%% @spec file(string()) -> not_found | {file, node()}
%%
%% @doc Returns the graph node of the file `FileName' if it is stored in the
%% graph.
file(FilePath) ->
    case ?GRAPH:path(?GRAPH:root(), [{file, {path, '==', FilePath}}]) of
        [File] -> {file, File};
        _      -> not_found
    end.


%% @spec file_by_modulename(atom() | string()) -> {node(), node()} | error
%% @doc Returns the specified pair of a module and a file node.
file_by_modulename(ModuleName) ->
    ModPath = [{module, {name, '==', ?MISC:to_atom(ModuleName)}}],
    try ?GRAPH:path(?GRAPH:root(), ModPath) of
        [Mod] ->
            [File] = ?GRAPH:path(Mod, [{moddef, back}]),
            {Mod, File};
        _ ->
            error
    catch error:badarg ->
                  error
          end.


%% @spec get_file(atom() | string()) -> node()
%% @throws term()
%%
%% @doc Returns the node of the speficied file.
get_file(FileName) ->
    case file(FileName) of
        {file, File} ->
            File;
        not_found ->
            throw("The file doesn't exist in the database: '"
                  ++ ?MISC:to_list(FileName) ++ "'")
    end.


%% @spec top_node(node(), node()) -> {atom(), node(), [node()], [node()]}
%% @doc Returns the top node with its type and the two remaining paths.
top_node(Expr1, Expr2) ->
    Path1                    = path_from_root(Expr1),
    Path2                    = path_from_root(Expr2),
    {TopPath, Rest1, Rest2}  = separate_prefix(Path1, Path2),
    [TopNode]                = ?ESG:path(?ESG:root(), TopPath),
    NodeType                 = type(TopNode),
    {NodeType, TopNode, Rest1, Rest2}.

%% @spec all_syntax_nodes(node()) -> [node()]
%% @doc Returns all syntax nodes in the subtree, inclusive of the top node.
all_syntax_nodes(Node) ->
    {_, Children} = lists:unzip(children(Node)),
    lists:flatten([Node | lists:map(fun all_syntax_nodes/1, Children)]).

%% Separates the same prefix of two lists.
%% Returns {Prefix, Rest1, Rest2}.
separate_prefix(Path1, Path2) ->
    separate_prefix(Path1, Path2, []).

separate_prefix([P1|Ps1], [P2|Ps2], Prefix) when P1 == P2 ->
    separate_prefix(Ps1, Ps2, [P1 | Prefix]);
separate_prefix(Rest1, Rest2, Prefix) ->
    {lists:reverse(Prefix), Rest1, Rest2}.



%% Returns the path that leads from the root to the given node.
path_from_root(Node) ->
    path_from_root(Node, []).

path_from_root(Node, Path) ->
    case ?ESG:root() of
        Node ->
            Path;
        _ ->
            {Link, Parent} = single_parent_link(Node),
            Index = ?ESG:index(Parent, Link, Node),
            path_from_root(Parent, [{Link, Index} | Path])
    end.



%% @spec single_parent_link(node()) -> {atom(), node()}
%% @throws string()
%% @doc Returns the parent and link of the node if there is exactly one parent,
%%      otherwise throws an error.
single_parent_link(Node) ->
    case ?ESG:parent(Node) of
        [{Link, Parent}] -> {Link, Parent};
        []               -> throw("No parent");
        _                -> throw("Too many parents")
    end.

%% @spec single_parent(node()) -> node()
%% @throws string()
%% @doc Returns the parent of the node if there is exactly one parent,
%%      otherwise throws an error.
single_parent(Node) ->
    {_, Parent} = single_parent_link(Node),
    Parent.



%% @spec form_type(node()) -> atom()
%% @doc Returns the type of the form as an atom.
%% The returned atom is the tag (e.g. export, import) by attribs
%% and `macro' by macro definition forms.
form_type(Form) ->
    #form{type=Type, tag=Tag} = ?GRAPH:data(Form),
    case Type of
        define -> macro;
        attrib -> Tag;
        _      -> Type
    end.

%% @doc Returns the type of the node.
%%      Either the node itself or its data can be given as input.
type(Node = {'$gn', _, _}) ->
    element(1, ?ESG:data(Node));
type(Data) ->
    element(1, Data).


%% @doc Returns the closest ancestor of the node that has the given type.
ancestor_with_type(Type, Node) ->
    case {?ESG:root(), type(Node)} of
        {Node, _} -> throw(?MISC:format("No ancestor with type ~p found.", Type));
        {_, Type} -> Node;
        {_, _}    -> ancestor_with_type(Type, ?SYNTAX:single_parent(Node))
    end.

%% @spec function_forms(node(), [{atom(), integer()}]) -> [node()]
%% @doc Returns function form nodes by the given (name, arity) list.
%% The first parameter is module node, the second is list of {name, arity} pairs.
%% Returns list of function nodes, which are in the specified module
%% and have the specified names and arities.
function_forms(Module, FnList) ->
    Nodes = [ ?GRAPH:path(Module, path_fun(Fn)) || Fn <- FnList],
    case [ error || Node <- Nodes, Node == [] ] of
        [] -> lists:flatten(Nodes);
        _  -> {error, "Fun not found"}
    end.

path_fun({Name, Arity}) ->
    referl_anal_fun:function(Name, Arity) ++ [{fundef, back}].


%%% ============================================================================
%%% Module qualifier


%% @spec module_qualifier(node()) -> no_module_qualifier | node() | false
%% @doc Returns the module node if the `Expr' is a module-qualified function
%% application, otherwise false or no_module_qualifier.
module_qualifier(Expr) ->
    case ?GRAPH:data(Expr) of
        #expr{kind=fun_expr}     -> false;
        #expr{kind=implicit_fun} ->
            case length(?GRAPH:path(Expr, [sub])) of
                3 ->
                    hd(?GRAPH:path(Expr, [sub, modref]));
                _ ->
                    no_module_qualifier
            end;
        #expr{kind=application}  ->
            case [?GRAPH:data(A) || A <- ?GRAPH:path(Expr, [sub])] of
                [#expr{kind=infix_expr, value=':'}|_] ->
                    hd(?GRAPH:path(Expr, [sub, sub, modref]));
                _ ->
                    no_module_qualifier
            end
    end.


%%% ============================================================================
%%% Common path sequences

%% @spec form_def_path(atom()) -> list()
%% @doc Returns path expression: from file to contained macro definition form.
form_def_path(Name) ->
    [incl, {form,  {{type, '==', define}, 'and', {tag, '==', Name}}}].

%% @spec form_inc_path(string()) -> list()
%% @doc Returns path expression: from file to contained include form.
%% The include form describes the inclusion of the given file path.
form_inc_path(Path) ->
    [incl, {form, {{type, '==', include}, 'and', {tag, '==', Path}}}].


%%% ============================================================================
%%% Token order restoration

% %% @doc Returns the text of the file.
% %% @deprecated Use {@link tree_text/1} instead.
% file_text(File) ->
%     Forms     = ?GRAPH:path(File, [form]),
%     Originals = original_tokens(Forms),
%     Data = [?LEX:token_data(N) || {N,_} <- Originals],
%     lists:flatmap(fun ?LEX:lex_text/1, Data).
%
% %% @doc Returns the list of the tokens below the nodes in their original order.
% %%      Macro substitutions are tracked and expanded only once.
% original_tokens(Nodes) ->
%     {Originals, _} = original_tokens(Nodes, []),
%     Originals.
%
% original_tokens(Nodes, Substs) ->
%     {Originals, Substs2} = lists:mapfoldl(fun original_token_nodes/2, Substs, Nodes),
%     {lists:flatten(Originals), Substs2}.
%
% %% @doc Returns the list of the tokens below the node in their original order.
% original_token_nodes(Node, Substs) ->
%     Data = ?GRAPH:data(Node),
%     case Data of
%         #lex{type = subst}    ->
%             case lists:member(Node, Substs) of
%                 true  -> {"", Substs};
%                 false -> lex_node_order(Node, [llex], Substs ++ [Node])
%             end;
%         #lex{}                -> {[{Node, Data}], Substs};
%         #form{type = include} -> lex_node_order(Node, [flex], Substs);
%         #form{type = define}  -> lex_node_order(Node, [flex], Substs);
%         #form{type = record}  -> lex_node_order(Node, [flex], Substs);
%         _ ->
%             ChildNodeOrder = child_node_order(Node),
%             original_tokens(ChildNodeOrder, Substs)
%     end.
%
% %% @doc Returns the list of the tokens below the node in their original order.
% %%      The tokens are connected by Link.
% lex_node_order(Node, Path, Substs) ->
%     Nodes = ?GRAPH:path(Node, Path),
%     original_tokens(Nodes, Substs).
%

%% @doc Returns the order of the children of a node.
child_node_order(Node) ->
    lists:flatmap(fun nodes_from_structure/1, structure_with_nodes(Node)).


%% @spec structure_with_nodes(node()) -> [StrWithNodes]
%%
%%      StrWithNodes    = {token, TokenType, node()}
%%                      | {symbol, SymbolLink, node()}
%%                      | {repeat, TokenType, SymbolLink, [StrWithNodes]}
%%                      | {optional, [StrWithNodes]}
%%      TokenType       = atom()
%%      SymbolLink      = atom()
%% @doc Returns the actual node structure enriched with the nodes in order.
%%      Compared to the structure description,
%%      tokens, symbols and repeats receive an additional tuple element.
structure_with_nodes(Node) ->
    Data    = ?GRAPH:data(Node),
    NodeStr = ?NODE_STRUCTURE:node_structure(Data),
    structure_with_nodes(Node, NodeStr).


%% @doc Determines the order of child nodes using structural information.
structure_with_nodes(Node, ActNodeStr) ->
    Syms     = node_types(ActNodeStr),
    GrpSyms  = [ {Sym, ?GRAPH:path(Node, [Sym])} || Sym <- Syms ],
    Tokens   = ?GRAPH:path(Node, [?LEX:lex_link(Node)]),
    FGrpSyms = lists:filter(fun({_, L}) -> L /= [] end, GrpSyms),
    case structure_with_nodes(ActNodeStr, FGrpSyms, Tokens, Node) of
        {NodesInStructure, [], []} -> NodesInStructure;
        {_, XSyms, XTokens}        -> error_unexpected_nodes(XSyms, XTokens)
    end.


error_unexpected_nodes([], XTokens) ->
    throw(?MISC:format("Unexpected tokens: ~p", [XTokens]));
error_unexpected_nodes(XSyms, []) ->
    throw(?MISC:format("Unexpected symbols: ~p", [XSyms]));
error_unexpected_nodes(XSyms, XTokens) ->
    throw(?MISC:format("Unexpected tokens: ~p, symbols: ~p", [XTokens, XSyms])).


%% @doc All node types in a structure description.
node_types(ActNodeStr) ->
    {_, Symbols} = node_types(ActNodeStr, {[], []}),
    lists:usort(Symbols).

%% @doc All token and symbol types in a structure description.
node_types(                  [], {Ts, Ss}) -> {Ts, Ss};
node_types([    {symbol, S}|Xs], {Ts, Ss}) -> node_types(Xs, {Ts, [S|Ss]});
node_types([     {token, T}|Xs], {Ts, Ss}) -> node_types(Xs, {[T|Ts], Ss});
node_types([ {repeat, T, S}|Xs], {Ts, Ss}) -> node_types(Xs, {[T|Ts], [S|Ss]});
node_types([{optional, Opt}|Xs], {Ts, Ss}) ->
    {OTs, OSs} = node_types(Opt, {[], []}),
    node_types(Xs, {Ts ++ OTs, Ss ++ OSs}).


%% Extracts the nodes from the enriched structure description.
nodes_from_structure({symbol, _, Node})     -> [Node];
nodes_from_structure({token, _, Node})      -> [Node];
nodes_from_structure({repeat, _, _, Nodes}) -> Nodes;
nodes_from_structure({optional, Opts})  ->
    lists:flatmap(fun nodes_from_structure/1, Opts).


%% Enriches the node structure with the nodes in order.
structure_with_nodes([], GrpSyms, Tokens, _) ->
    {[], GrpSyms, Tokens};

structure_with_nodes([{symbol, S}|Ss], GrpSyms, Tokens, Parent) ->
    {[SNode], GrpSyms2}       = ?MISC:lookup_symbols(S, 1, GrpSyms),
    {Rest, GrpSyms3, Tokens3} = structure_with_nodes(Ss, GrpSyms2, Tokens, Parent),
    {[{symbol, S, SNode}] ++ Rest, GrpSyms3, Tokens3};

structure_with_nodes([ {token, TType}|Ss], GrpSyms, [T|Tokens], Parent) ->
    {Rest, GrpSyms2, Tokens2} = structure_with_nodes(Ss, GrpSyms, Tokens, Parent),
    {[{token, TType, T}] ++ Rest, GrpSyms2, Tokens2};

structure_with_nodes([{repeat, T, S}|Ss], GrpSyms, Tokens, Parent) ->
    {value, {S, SNodes}}      = lists:keysearch(S, 1, GrpSyms),
    [FirstSNode|SNodeRest]    = SNodes,
    GrpSyms2                  = lists:keydelete(S, 1, GrpSyms),
    {SameTs, Ts}              = lists:split(length(SNodeRest), Tokens),
    {Rest, GrpSyms3, Tokens3} = structure_with_nodes(Ss, GrpSyms2, Ts, Parent),
    RepeatNodes               = [FirstSNode] ++ ?MISC:merge(SameTs, SNodeRest),
    {[{repeat, T, S, RepeatNodes}] ++ Rest, GrpSyms3, Tokens3};

structure_with_nodes([{optional, _Opts = [{token, _}|_]}|Ss], GrpSyms, [], Parent) ->
    {Rest, GrpSyms2, Tokens2} = structure_with_nodes(Ss, GrpSyms, [], Parent),
    {[{optional, []}] ++ Rest, GrpSyms2, Tokens2};

structure_with_nodes([{optional, Opts = [{token, T}|_]}|Ss],
                     GrpSyms, Tokens = [Token|_], Parent) ->
    Missing = is_token_missing(Token, T),
    structure_with_nodes_optional(Missing, Ss, Opts, GrpSyms, Tokens, Parent);

structure_with_nodes([{optional, Opts = [{repeat, _, S}|_]}|Ss],
                     GrpSyms, Tokens, Parent) ->
    Missing = lists:keysearch(S, 1, GrpSyms) == false,
    structure_with_nodes_optional(Missing, Ss, Opts, GrpSyms, Tokens, Parent);

structure_with_nodes(_, _, _, Parent) ->
    ErrMsg = "Bad node format: " ++ ?MISC:format("~p", [Parent]),
    throw(ErrMsg).


%% Returns whether the first token of an optional is the one we are searching.
is_token_missing(Token, OptionalTokenText) ->
    TAtom = OptionalTokenText,
    case ?GRAPH:data(Token) of
        #lex{data = #token{type = TAtom}} -> false;
        _                                 -> true
    end.


%% @doc Code for both cases (beginning with token or repeat) of the optionals.
structure_with_nodes_optional(false, Ss, Opts, GrpSyms, Tokens, Parent) ->
    {Opts2, GrpSyms2, Tokens2} = structure_with_nodes(Opts, GrpSyms, Tokens, Parent),
    {Rest, GrpSyms3, Tokens3}  = structure_with_nodes(Ss, GrpSyms2, Tokens2, Parent),
    {[{optional, Opts2}] ++ Rest, GrpSyms3, Tokens3};

structure_with_nodes_optional(true, Ss, _Opts, GrpSyms, Tokens, Parent) ->
    {Rest, GrpSyms2, Tokens2} = structure_with_nodes(Ss, GrpSyms, Tokens, Parent),
    {[{optional, []}] ++ Rest, GrpSyms2, Tokens2}.


%%% ============================================================================
%%% Syntax tree traversal using ?ESG and node structure


%% @spec children(node()) -> [{atom(), node()}]
%% @doc Returns the children of the node in the order as they appear in the
%% real syntax tree.
children(Node) ->
    Data = ?GRAPH:data(Node),
    case Data of
        {root}             -> ?GRAPH:links(Node);
        #file{}            -> [{form, F} || F <- ?GRAPH:path(Node, [form])];
        #form{type=error}  -> throw(node_error);
        #form{type=lex}    -> [{flex, F} || F <- ?GRAPH:path(Node, [flex])];
        #form{type=macro}  -> [{flex, F} || F <- ?GRAPH:path(Node, [flex])];
        #lex{}             -> [{llex, F} || F <- ?GRAPH:path(Node, [llex])];
        #form{}            -> children(Node, Data, flex);
        #clause{}          -> children(Node, Data, clex);
        #expr{}            -> children(Node, Data, elex)
    end.

children(Node, Data, LexTag) ->
    Struct = ?NODE_STRUCTURE:node_structure(Data),
    Children = unordered_children(Node, Struct, LexTag),
    order_children(Node, LexTag, Children, Struct).

unordered_children(Node, Struct, LexTag) ->
    [{Tag, Child} || Tag <- [LexTag | children_tags(Struct)],
                     Child <- ?GRAPH:path(Node, [Tag])].

children_tags(Struct) ->
    lists:usort(children_tags(Struct, [])).
children_tags([{symbol, Tag} | Tail], Tags) ->
    children_tags(Tail, [Tag | Tags]);
children_tags([{repeat, _, Tag} | Tail], Tags) ->
    children_tags(Tail, [Tag | Tags]);
children_tags([{optional, Opt}| Tail], Tags) ->
    children_tags(Tail, children_tags(Opt, Tags));
children_tags([{token, _}|Tail], Tags) ->
    children_tags(Tail, Tags);
children_tags([], Tags) ->
    Tags.

order_children(Parent, LexTag, Children, [{optional, Opt} | Rest]) ->
    Exists =
        case hd(Opt) of
            {token, Type} ->
                case lists:keysearch(LexTag, 1, Children) of
                    {value, {_, First}} ->
                        (?LEX:token_data(First))#token.type =:= Type;
                    false ->
                        false
                end;
            {repeat, _, Link} ->
                lists:keymember(Link, 1, Children)
        end,
    if
        Exists     -> order_children(Parent, LexTag, Children, Opt ++ Rest);
        not Exists -> order_children(Parent, LexTag, Children, Rest)
    end;

order_children(Parent, LexTag, Children, [{repeat, _Sep, Link} | Rest]) ->
    {Repeats, NonReps} = lists:partition(
                           fun ({Tag, _}) -> Tag =:= Link end,
                           Children),
    {SepTokens, Others} = take_separators(NonReps, LexTag, length(Repeats)-1),
    build_repeat(Repeats, SepTokens,
                 order_children(Parent, LexTag, Others, Rest));

order_children(Parent, LexTag, Children, [{symbol, Link} | Rest]) ->
    {value, Sym, Others} = lists:keytake(Link, 1, Children),
    [Sym | order_children(Parent, LexTag, Others, Rest)];

order_children(Parent, LexTag, Children, [{token, _Type} | Rest]) ->
    {value, Token, Others} = lists:keytake(LexTag, 1, Children),
    [Token | order_children(Parent, LexTag, Others, Rest)];

order_children(_, _, _, []) -> [].


take_separators(Lst, Tag, Num) ->
    {Seps,_} = lists:split(Num, [S || S={T, _} <- Lst, T =:= Tag]),
    {Seps, Lst -- Seps}.


build_repeat([Sym | SymTail], [Sep | SepTail], Tail) ->
    [Sym, Sep | build_repeat(SymTail, SepTail, Tail)];
build_repeat([Sym], [], Tail) ->
    [Sym | Tail].


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
create(Data, Children) ->
    Struct = ?NODE_STRUCTURE:node_structure(Data),
    Upd = match_children(Struct, unfold_children(Children), lex_tag(Data)),
    Node = ?ESG:create(Data),
    NewChildren = update_children(Node, [], Upd),
    update_attribs(Node, Data, NewChildren),
    Node.

%% @spec replace(node(), ChildSel, Repl) -> [{Tag, node()}]
%%        ChildSel = Tag | {Tag, Start, End} | {First, Last} |
%%                   {node, node()} | {before, node()} | {next_to, node()}
%%        Repl = (([{atom(), node()}]) -> [ChildSpec]) | [ChildSpec]
%%        Tag = atom()
%%        Start = integer()
%%        End = integer()
%%        First = node()
%%        Last = node()
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
replace(Node, ChildSel, Replacement) ->
    Data = ?GRAPH:data(Node),
    Struct = ?NODE_STRUCTURE:node_structure(Data),
    Children = children(Node),
    {Pre, Rep, Post} = select_children(ChildSel, Children, Struct),
                                                %io:format("Select: ~p~n ---~n   ~p~n ---~n   ~p~n", [Pre, Rep, Post]),
    New =
        unfold_children(
          if
              is_function(Replacement) -> Replacement(Rep);
              is_list(Replacement)     -> Replacement
          end),
                                                %io:format("New list: ~p~n", [Pre++New++Post]),
    Upd = match_children(Struct, Pre ++ New ++ Post, lex_tag(Data)),
                                                %io:format("Update: ~p~n", [Upd]),
    [?ESG:remove(Node, Tag, Child) || {Tag, Child} <- Rep],
    NewChildren = update_children(Node, Pre, (Upd -- Pre) -- Post),
    update_attribs(Node, Data, Pre ++ NewChildren ++ Post),
    Rep.

%% REM DIAL #lex{} clause currently never called
lex_tag(#form{})   -> flex;
lex_tag(#clause{}) -> clex;
lex_tag(#expr{})   -> elex;
lex_tag(#lex{})    -> llex.

unfold_children(Lst) ->
    lists:flatmap(fun unfold_childspec/1, Lst).

unfold_childspec(Text=[Char|_]) when is_integer(Char) ->
    [Text];
unfold_childspec(Lst) when is_list(Lst) ->
    unfold_children(Lst);
unfold_childspec({Tag, Lst}) when is_list(Lst) ->
    [{Tag, N} || N <- Lst];
unfold_childspec(Spec) -> [Spec].


select_children({Tag, Start, End}, Children, Struct) ->
    TagNodes = [N || {T,N} <- Children, T =:= Tag],
    if
        TagNodes == [] ->
            {Pre, Post} = split_at_link(Tag, Children, Struct),
            {Pre, [], Post};
        Start > length(TagNodes) ->
            select_children({next_to, lists:last(TagNodes)}, Children, Struct);
        Start >= End ->
            select_children({before, lists:nth(Start, TagNodes)},
                            Children, Struct);
        Start < End ->
            select_children({lists:nth(Start, TagNodes),
                             lists:nth(End-1, TagNodes)}, Children, Struct)
    end;
select_children(Tag, Children, Struct) when is_atom(Tag) ->
    case [N || {T, N} <- Children, T =:= Tag] of
        [] ->
            {Pre, Post} = split_at_link(Tag, Children, Struct),
            {Pre, [], Post};
        Lst ->
            select_children({hd(Lst), lists:last(Lst)}, Children, Struct)
    end;
select_children({before, Node}, Children, _) ->
    {Pre, Post} = split_at_node(Node, Children),
    {Pre, [], Post};
select_children({next_to, Node}, Children, _) ->
    {Pre, [Mid | Post]} = split_at_node(Node, Children),
    {Pre ++ [Mid], [], Post};
select_children({node, Node}, Children, S) ->
    select_children({Node, Node}, Children, S);
select_children({First, Last}, Children, _) ->
    {Pre, Tail} = split_at_node(First, Children),
    {Rep, [Mid | Post]} = split_at_node(Last, Tail),
    {Pre, Rep ++ [Mid], Post}.

split_at_node(Node, List) ->
    lists:splitwith(fun({_,N}) -> Node =/= N end, List).


split_at_link(Link, Children, Struct) ->
    split_at_link(Link, Children, [], Struct).

split_at_link(Link, Post, Pre, [{symbol, Link} |_]) ->
    {lists:reverse(Pre), Post};
split_at_link(Link, Post, Pre, [{repeat, _, Link}|_]) ->
    {lists:reverse(Pre), Post};
split_at_link(Link, Post, Pre, [{optional, [{repeat, _, Link}|_]}|_])->
    {lists:reverse(Pre), Post};
split_at_link(Link, [{Tag, Node} | CT], Pre, [{symbol, Tag} | ST]) ->
    split_at_link(Link, CT, [{Tag, Node} | Pre], ST);
split_at_link(Link, [{Tag,Node} | CT], Pre, [{token, _} | ST]) ->
    split_at_link(Link, CT, [{Tag,Node} | Pre], ST);
split_at_link(Link, [{Tag, Node} | CT], Pre, [{repeat, Sep, Tag}|ST]) ->
    try
        N={_, Next} = hd(CT),
        #token{type=Sep} = ?LEX:token_data(Next),
        split_at_link(Link, tl(CT), [N,{Tag,Node}|Pre], [{repeat,Sep,Tag}|ST])
    catch
        error:_ ->
            split_at_link(Link, CT, [{Tag,Node}|Pre], ST)
    end;
split_at_link(Link, [{_,Node}|_]=Chld, Pre,
              [{optional, Opt=[{token,Type}|_]} | ST]) ->
    try
        #token{type=Type} = ?LEX:token_data(Node),
        split_at_link(Link, Chld, Pre, Opt ++ ST)
    catch
        error:_ ->
            case has_link(Opt, Link) of
                true -> {lists:reverse(Pre), Chld};
                _    -> split_at_link(Link, Chld, Pre, ST)
            end
    end;
split_at_link(Link, [{Tag,_}|_]=Chld, Pre,
              [{optional, Opt=[{repeat,_,Tag}|_]} | ST]) ->
    split_at_link(Link, Chld, Pre, Opt ++ ST);
split_at_link(Link, Chld, Pre, [{optional, Opt}|ST]) ->
    case has_link(Opt, Link) of
        true -> {lists:reverse(Pre), Chld};
        _    -> split_at_link(Link, Chld, Pre, ST)
    end.


has_link([], _) ->
    false;
has_link([{symbol, Link}|_], Link) -> true;
has_link([{optional, Opt}|Tail], Link) ->
    has_link(Opt, Link) orelse has_link(Tail, Link);
has_link([{repeat, _, Link}|_], Link) -> true;
has_link([_|Tail], Link) ->
    has_link(Tail, Link).


%% @spec match_children(structure(), [ChildSpec], Tag) -> [ChildGenSpec]
%%         ChildSpec = {Tag, node()} | {Tag, text, string()}
%%         ChildGenSpec = {Tag, node()} |
%%                        {Tag, gen, Type} |
%%                        {Tag, scan, Type, string()}
%%         Tag = atom()
%%         Type = atom()
match_children(Struct, Children, LexTag) ->
    Children2 =
        lists:map(
          fun
          ({Tag, Node}) when Tag =:= LexTag ->
                         {?LEX:token_data(Node), Node};
          (Spec) -> Spec
                 end,
          Children),
    lists:map(
      fun
      (Spec) when element(1, Spec) =:= '$lex' ->
                     setelement(1, Spec, LexTag);
      (Spec) -> Spec
             end,
      match_children(Struct, Children2)).

%% Lexical tags are replaced with '$lex' because the lexical tag name is not
%% available for generation (for the {Tag, gen, Type} instruction).
match_children([{symbol, Link} | ST],
               [{Link,   Node} | CT]) ->
    [{Link, Node} |
     match_children(ST, CT)];

match_children([{token, Type} | ST],
               [{#token{type=Type}, Node} | CT]) ->
    [{'$lex', Node} |
     match_children(ST, CT)];

match_children([{token, Type}   | ST],
               [Text            | CT]) when is_list(Text) ->
    [{'$lex', scan, Type, Text} |
     match_children(ST, CT)];

match_children([{token, Type} | ST], Chld)   ->
    [{'$lex', gen, Type} |
     match_children(ST, Chld)];

match_children([{repeat, Sep, _Link}     | _] = Str,
               [{#token{type=Sep}, Node} | CT]) ->
    [{'$lex', del, Node} |
     match_children(Str, CT)];

match_children([{repeat, Sep, Link}=Rep | ST],
               [{Link, Node}            | CT]) ->
    [{Link, Node} |
     match_children([{optional, [{token, Sep}, Rep]} | ST], CT)];

match_children([{repeat, _, _} | ST], Chld)  ->
    match_children(ST, Chld);

match_children([{optional, [{token, Type} | OT]} | ST],
               [{#token{type=Type}, Node}        | CT]) ->
    try
        [{'$lex', del, Node} |
         match_children([{remove, OT} | ST], CT)]
    catch
        no_match ->
            [{'$lex', Node} | match_children(OT ++ ST, CT)]
    end;

match_children([{optional, [{token, Type} | _] = Opt} | ST],
               [{#token{type=TokType}, _}             | _] = Chld)
  when Type =/= TokType ->
    try
        match_children(ST, Chld)
    catch
        no_match ->
            match_children([{remove, Opt} | ST], Chld)
    end;

match_children([{optional, Opt} | ST], Chld) ->
    try
        match_children(ST, Chld)
    catch
        no_match ->
            match_children(Opt ++ ST, Chld)
    end;

match_children([{remove, [{token, Type} | OT]} | ST],
               [{#token{type=Type}, Node}      | CT]) ->
    [{'$lex', del, Node} |
     match_children([{remove, OT} | ST], CT)];

match_children([{remove, [{symbol, Link} | _]} | _],
               [{Link, _}                      | _]) ->
    throw(no_match);

match_children([{remove, _} | ST], Chld) ->
    match_children(ST, Chld);

match_children([], []) -> [];
match_children(_, _) -> throw(no_match).


%% @spec update_children(node(),[{Tag,node()}],ChildGenSpec) -> [{Tag,node()}]
%%       Tag = atom()
update_children(Parent, Prefix, ChildGenSpec) ->
    LinkCount =
        lists:foldl(
          fun ({Tag, _}, Cnt) -> dict:update_counter(Tag, 1, Cnt) end,
          dict:new(),
          Prefix),
    {Result, _} =
        lists:foldl(
          fun (ChildGen, {Result, Cnt}) ->
                  case generate_child(ChildGen) of
                      {add, Tag, Child} ->
                          {[{Tag, Child} | Result],
                           case dict:find(Tag, Cnt) of
                               {ok, N} ->
                                   ?ESG:insert(Parent, {Tag, N+1}, Child),
                                   dict:store(Tag, N+1, Cnt);
                               error ->
                                   ?ESG:insert(Parent, {Tag, 1}, Child),
                                   dict:store(Tag, 1, Cnt)
                           end};
                      {del, Tag, Child} ->
                          ?ESG:remove(Parent, Tag, Child),
                          {Result, Cnt}
                  end
          end,
          {[], LinkCount},
          ChildGenSpec),
    lists:reverse(Result).

generate_child({Tag, Node}) -> {add, Tag, Node};
generate_child({Tag, del, Node}) -> {del, Tag, Node};
generate_child({Tag, scan, Type, Text}) ->
    Node = ?ESG:create(
              #lex{type=token,
                   data=(?SYNLEX:lex_elem(Type, Text))#token{prews=new}}),
    {add, Tag, Node};
generate_child({Tag, gen, Type}) ->
    Text = atom_to_list(?SYNLEX:keyword_value(Type)),
    Node = ?ESG:create(
              #lex{type=token,
                   data=(?SYNLEX:lex_elem(Type, Text))#token{prews=new}}),
    {add, Tag, Node}.

update_attribs(Node, Data, Children) ->
    NewData =
        lists:foldl(
          fun ({{Link, Ind}, Field}, D) ->
                  Token = lists:nth(Ind, [T || {L, T} <- Children, L =:= Link]),
                  #token{value=Value} = ?LEX:token_data(Token),
                  setelement(Field, D, Value)
          end,
          Data,
          ?NODE_STRUCTURE:token_attribs(Data)),
    if
        NewData =/= Data -> ?ESG:update(Node, NewData);
        true             -> ok
    end.

%%% ============================================================================
%%% Another way to create

%% @spec create_by_struct(data(), [ChildSpec]) -> node()
%%         ChildSpec = node() | atom() |
%%                     Opt | Repeat
%%         Opt = {opt, [ChildSpec]} | no_opt
%%         Repeat = [ChildSpec]
%%
%% @doc Creates a syntactic node. Children must be supplied in the proper order,
%% except that lexical nodes may be left out -- keyword tokens needed by the
%% syntactical rules of the node are autogenerated.
%%
%% Returns the created node. Note that this node is not inserted into the
%% syntax tree, and will be deleted at the end of the current ESG batch if
%% you do not insert it explicitly.
create_by_struct(Data, Nodes) ->
    Structure = ?NODE_STRUCTURE:node_structure(Data),
    Desc = match_children_by_struct(Structure, Nodes),
    create(Data, Desc).

match_children_by_struct([], _) ->
    [];

match_children_by_struct([{symbol, Link}|Structure], [Node|Rest]) ->
    [{Link, Node}] ++ match_children_by_struct(Structure, Rest);

match_children_by_struct([{token, _}|Structure], [{force, Text}|Rest]) ->
    [Text] ++ match_children_by_struct(Structure, Rest);

match_children_by_struct([{token, TokenType}|Structure], Params) ->
    case autocreatable_token(TokenType) of
        false ->
            [Text|Rest] = Params,
            [Text] ++ match_children_by_struct(Structure, Rest);
        true ->
            match_children_by_struct(Structure, Params)
    end;

match_children_by_struct([{repeat, _, Link}|Structure], [NodeList|Rest]) ->
    [{Link, NodeList}] ++ match_children_by_struct(Structure, Rest);

match_children_by_struct([{optional, _Opt}|Structure], [no_opt|Rest]) ->
    match_children_by_struct(Structure, Rest);

match_children_by_struct([{optional, Opt}|Structure], [{opt, Nodes}|Rest]) ->
    match_children_by_struct(Opt, Nodes) ++ match_children_by_struct(Structure, Rest).

autocreatable_token(variable) -> false;
autocreatable_token(atom)     -> false;
autocreatable_token(string)   -> false;
autocreatable_token(integer)  -> false;
autocreatable_token(float)    -> false;
autocreatable_token(char)     -> false;
autocreatable_token(_)        -> true.
