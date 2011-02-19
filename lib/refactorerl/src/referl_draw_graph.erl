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

%%% @doc This module prints the syntax graph to a file.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>


-module(referl_draw_graph).
-vsn("$Rev: 2914 $").

-include("refactorerl.hrl").
-include("referl_schema.hrl").

%%% ============================================================================
%%% Exports

-export([draw_graph/1, draw_graph/2, draw_graph_tooltip/2]).



%%% ============================================================================
%%% Draw graph

%% @spec draw_graph(File :: string()) -> ok | string()
%% @doc  Draws the whole graph to the file.
draw_graph(File) -> draw_graph(File, all).

%% @spec draw_graph(File :: string(), Filter :: atom()) -> ok | string()
%% @doc  Draws the whole graph to File with edge filtering.
draw_graph(File, Filter) ->
    draw_graph(File, Filter, false).

%% @spec draw_graph_tooltip(File :: string(), Filter :: atom()) -> ok | string()
%% @doc  Draws the whole graph to File with edge filtering.
%%       Fill `tooltip' attributes with the data of the nodes. You can use
%%       this cool feature if you convert the dot file into svg.
draw_graph_tooltip(File, Filter) ->
    draw_graph(File, Filter, true).

%% @spec draw_graph(File :: string(), Filter :: atom(), ToolTip::boolean())
%%           -> ok | string()
%% @doc  Draws the whole graph to File with edge filtering.
%%       If `ToolTip' is `true' the `tooltip' attribute will contain the data of
%%       the node.
draw_graph(File, Filter, ToolTip) ->
    Root = ?Graph:root(),
    ets:new(nodes, [named_table]),
    ets:insert(nodes, {max, 0}),
    case file:open(File, [write]) of
        {ok, Dev} ->
            io:format(Dev, "digraph erl {~n", []),
%            io:format(Dev, "graph [ ordering=\"out\" ];~n", []),
            CollectedTokens = draw_graph(Dev, Root, Filter, ToolTip, []),
            io:format(Dev, "{rank=same;~s}~n", [CollectedTokens]),
            io:format(Dev, "}~n", []),
            Ret = file:close(Dev);
        {error, Reason} ->
            Ret = "Dump failed: " ++ file:format_error(Reason)
    end,
    ets:delete(nodes),
    Ret.


%% @doc Prints the graphviz representation of the graph into Dev,
%%      and returns the tokens of the file.
draw_graph(Dev, Node, Filter, ToolTip, Tokens) ->
    case ets:lookup(nodes, Node) of
        [] -> draw_new_node(Dev, Node, Filter, filter(Filter), ToolTip, Tokens);
        _  -> Tokens
    end.


draw_new_node(Dev, Node, Filter, FilterFun, ToolTip, Tokens) ->
    Ind    = ets:update_counter(nodes, max, 1),
    IntInd = integer_to_list(Ind),
    ets:insert(nodes, {Node, Ind}),

    io:format(Dev, "N~b [~s]~n", [Ind, nodelabel(Node, ToolTip)]),

    NodeClass = element(1, ?Graph:data(Node)),
    AllTokens =
        case NodeClass of
            token -> Tokens ++ ["N" ++ IntInd ++ ";"];
            _     -> Tokens
        end,
    Links = [Link || Link = {Tag, _To} <- ?Graph:links(Node),
                     FilterFun(NodeClass, Tag),
                     Tag /= env],

    {_SynLinks, OtherLinks} =
        lists:partition(fun({Tag, _}) ->
                            is_syn_link(NodeClass, Tag)
                             orelse is_lex_link(NodeClass, Tag)
                        end, Links),
    if
        Filter == lex orelse Filter == synlex ->
            % For Root and file nodes it cause run-time error: unknown structure
            %Children = ?SYNTAX:child_node_order(Node),
            %NewLinks = children_order_links(Children, SynLinks),
            % It give same result but it works for Root and file nodes
            NewLinks = ?Syn:children(Node),
            IndexLinks = index(NewLinks ++ OtherLinks);
        true ->
            IndexLinks = index(Links)
    end,

    CollectedTokens =
        lists:foldl(fun ({_Tag, _I, To}, Tokens2) ->
                        draw_graph(Dev, To, Filter, ToolTip, Tokens2)
                    end, AllTokens, IndexLinks),

    [ io:format(Dev, "N~b -> N~b [~s]~n",
                 [Ind,
                 element(2, hd(ets:lookup(nodes, To))),
                 linklabel(Tag, I)])
        ||
        {Tag, I, To} <- IndexLinks],

    CollectedTokens.


%children_order_links([], OtherLinks)          -> OtherLinks;
%children_order_links([Child|Children], Links) ->
%    {ToChild, NotToChild} =
%        lists:partition(fun({_, To}) -> To == Child end, Links),
%    ToChild ++ children_order_links(Children, NotToChild).


%% @doc Adds the appropriate link indexes to the parent-child lists.
index(L) -> index(L, []).


index([], _) -> [];
index([{Tag, To} | Tail], Inds) ->
    I = case proplists:get_value(Tag, Inds) of
            undefined -> 1;
            Ind       -> Ind
        end,
    [{Tag, I, To} | index(Tail, orddict:store(Tag, I+1, Inds))].


%%% ============================================================================
%%% Link filtering

%% @doc Returns the edge filter function for filter type FType.
filter(FType) ->
    fun(From, Tag) ->
        [] /= [ ok || Filter <- filters(FType), Filter(From, Tag) ]
    end.


%% @doc Returns the functions whose disjunction comprises the
%%      edge filter function.
filters(all)     -> [fun all_links/2];
filters(sem)     -> [fun is_sem_link/2, fun is_ctx_link/2, fun is_syn_link/2];
filters(ctx)     -> [fun is_ctx_link/2, fun is_syn_link/2];
filters(syn)     -> [fun is_syn_link/2];
filters(synlex)  -> [fun is_syn_link/2, fun is_lex_link/2];
filters(lex)     -> [fun is_form/2, fun is_lex_link/2];
filters(not_lex) -> [fun not_lex/2].

%%% ----------------------------------------------------------------------------
%%% Link types

all_links(_, _)       -> true.
is_form(_, To)        -> To == form.

is_lex_link(_, To) -> lists:member(To, [elex, flex, clex]).

is_syn_link(From, To) -> schema_has(?SYNTAX_SCHEMA, From, To).


%% todo Would be more elegant if the analyser modules supplied this information.
is_ctx_link(clause, scope) -> true;
is_ctx_link(clause, visib) -> true;
is_ctx_link(expr, clause)  -> true;
is_ctx_link(expr, sup)     -> true;
is_ctx_link(_,_)           -> false.

%% todo Would be more elegant if the analyser modules supplied this information.
is_sem_link(file, Tag)   -> Tag == moddef;
is_sem_link(form, Tag)   -> Tag == fundef;
is_sem_link(clause, Tag) -> lists:member(Tag, [modctx, vardef, varvis]);
is_sem_link(expr, Tag)   -> lists:member(Tag, [modref, varref, varbind, funref]);
is_sem_link(Class, _)    -> lists:member(Class, [module, variable, func]).

not_lex(Node, Tag) ->
    Tag == file orelse not(is_lex_link(Node, Tag)).


%% @doc  Returns if From->To is possible in the given schema.
schema_has(Schema, From, To) ->
    [] /= [ found || {From2, _, Tos} <- Schema
                   , {To2, _} <- Tos
                   , From2 == From
                   , To2   == To ].


%%% ============================================================================
%%% Implementation of the labels


%% @doc Returns the Graphviz label of the node.
nodelabel(Node = {_,_, Index}, ToolTip) ->
    Data          = ?Graph:data(Node),
    Shape         = shape(Data),
    Content       = label(Index, Data),
    IsRecordShape = Shape == "Mrecord" orelse Shape == "record",
    Label         = label_shape(IsRecordShape, Content, Index),
    TooltipStr = if
        ToolTip -> tooltipStr(Node,Data);
        true    -> ""
    end,
    % Write attributes
    io_lib:format("~s shape=\"~s\", label=\"~s\", fontsize=\"~p\"~s",
                  [nodestyle(Data), Shape, Label, labelsize(Data), TooltipStr]).

label_shape(false, Content, _)              -> escape_text(Content);
label_shape(true, {Top, Bottom}, Index)     -> boxed_shape(Top, Index, Bottom);
label_shape(true, {Type, Index, Bottom}, _) -> boxed_shape(Type, Index, Bottom).

boxed_shape(Top, Index, Bottom) ->
    record_component(
        [   record_component(
                [   escape_text(Top)
                ,   escape_text(Index)
                ] )
        ,   bottom_shape(Bottom)
        ] ).


bottom_shape({Bottom, undefined}) ->
    escape_text(Bottom);
bottom_shape({Bottom, Bottom}) ->
    escape_text(Bottom);
bottom_shape({Part1, Part2}) ->
    record_component(
        [   escape_text(Part1),
            escape_text(Part2) ] );
bottom_shape(Bottom) ->
    escape_text(Bottom).


record_component(Texts) ->
    "{" ++ ?MISC:join(Texts, "|") ++ "}".


nodestyle(#lex{}) ->  "color=lightsteelblue1, style=filled, ";
nodestyle(_)      ->  "".


labelsize(#lex{}) -> 12;
labelsize(_)      -> 18.


%% @doc Lexical tokens print their indexes in the graph, too.
label(Index, #lex{data=#token{type=Type, text=Text}}) ->
    { Type, Index, Text };
label(_, Data) -> label(Data).


%% @doc The label associated to the node data.
%%      Data with Mrecord or record shapes have two components.
label({root})                         -> "ROOT";
label(#lex{type=T})                   -> atom_to_list(T);
%label(#macro{name=N})                 -> {"macro", N};
label(#expr{type=T, kind=K, value=V}) -> explab(T,K,V);
label(#clause{type=_T, kind=K})       -> {"clause", atom_to_list(K)};
label(#form{type=include, tag=F})     -> {"include", F};
label(#form{type=attrib, tag=T})      -> atom_to_list(T);
label(#form{type=func})               -> "function";
label(#form{type=define})             -> "define";
label(#form{type=ppcond, tag=T})      -> atom_to_list(T);
label(#file{path=N})                  -> filename:basename(N);
label(#variable{name=N})              -> N;
label(#module{name=N})                -> N;
label(#func{name=N, arity=A})         -> atom_to_list(N) ++ "/" ++ integer_to_list(A);
label(#record{name=N})                -> atom_to_list(N);
label(#field{name=N})                 -> atom_to_list(N);
label(T) when is_tuple(T)             -> atom_to_list(element(1, T)).


shape({root})              -> "triangle";
shape(#lex{data=#token{}}) -> "Mrecord";
shape(#lex{})              -> "diamond";
%shape(#macro{})            -> "Mrecord";
shape(#expr{})             -> "Mrecord";
shape(#clause{})           -> "record";
shape(#form{type=include}) -> "record";
shape(#form{})             -> "box";
shape(#file{})             -> "box";
shape(#variable{})         -> "hexagon";
shape(#module{})           -> "hexagon";
shape(#func{})             -> "hexagon";
shape(#record{})           -> "hexagon";
shape(#field{})            -> "hexagon";
shape(T) when is_tuple(T)  -> "octagon".


%% @doc Escapes a text for Graphviz.
%% todo merge with ?MISC:escape/1, if possible
escape_text(Text) -> lists:flatmap(fun escape_char/1, ?MISC:to_list(Text)).


escape_char($\n)  -> "\\l";
escape_char($\{)  -> "\\{";
escape_char($\})  -> "\\}";
escape_char($<)   -> "\\<";
escape_char($>)   -> "\\>";
escape_char($@)   -> "\\@";
escape_char($")   -> [ $\\, $" ];
escape_char(Char) -> [Char].


linklabel(Tag, Ind) ->
    C=color(Tag),
    io_lib:format("label=\"~s/~b\", color=\"~s\", fontcolor=\"~s\"",
                  [Tag, Ind, C, C]).

explab(Type, Kind, Val) ->
    V = value_text(Val, Kind),
    if
        V =:= unknown     -> {Type, Kind};
        true              -> {Type, {Kind, V}}
    end.


value_text(      Val, char)        -> [Val];
value_text(      Val, variable)    -> Val;
value_text(      Val, string)      -> ?MISC:escape(Val);
value_text(      Val, integer)     -> Val;
value_text(      Val, float)       -> Val;
value_text(      Val, atom)        -> Val;
value_text(      Val, infix_expr)  -> Val;
value_text(      Val, prefix_expr) -> Val;
value_text(        _, underscore)  -> "_";
value_text(        _, nil)         -> "";
value_text(undefined, Kind)        -> Kind;
value_text(        _, _)           -> unknown.



%% todo It would be nice if the colours were returned by the analyser modules.
color(moddef) -> red;
color(modref) -> red;
color(module) -> red;

color(scope)  -> seagreen;
color(visib)  -> seagreen;
color(clause) -> seagreen;
color(modctx) -> seagreen;

color(func)   -> magenta;
color(fundef) -> magenta;
color(funexp) -> magenta;
color(funimp) -> magenta;
color(funref) -> magenta;

color(vardef) -> brown;
color(varvis) -> brown;
color(varref) -> brown;
color(varbind) -> brown;
color(varintro) -> brown;

color(sup) -> gray50;

color(incl)  -> blue;
color(mref)  -> blue;
color(mbody) -> blue;
color(marg)  -> blue;
color(macro) -> blue;
color(record) -> blue;
color(recdef) -> blue;
color(recref) -> blue;
color(field)    -> blue;
color(fieldref) -> blue;
color(fielddef) -> blue;

color(flex)   -> steelblue;
color(clex)   -> steelblue;
color(elex)   -> steelblue;
color(llex)   -> steelblue;

color(_) -> black.



%% -----------------------------------------------------------------------------
%% ToolTip texts

%% @spec tooltipStr(Node::node(), Data::tuple()) -> string()
%% @doc  Write the properties of the `Node' into a string. This text used as
%%  graphviz dot attribute for alt text in SVG format.
%%  String format:
%%  `, URL="1", tooltip="propKey1=propValue1&#13;&#10;...propKeyN=propValueN"'
tooltipStr(Node, Data) ->
    RevProps = lists:reverse([{node,Node}|
                    ?MISC:record_to_proplist(Data, nodedata_fields(Data))]),
    {LastKey,LastValue} = hd(RevProps),
    LastStr0 = ?MISC:format("~p=~p", [LastKey,LastValue]),
    LastStr  = ?MISC:string_replace(LastStr0, ["\""], "&quot;", 0),
    PropStr = lists:foldl(
        fun({K,V},StrEndAcc) ->
            KV0 = ?MISC:format("~p=~p&#13;&#10;", [K,V]),
            KV  = ?MISC:string_replace(KV0, ["\""], "&quot;", 0),
            KV++StrEndAcc
        end,
        LastStr,
        tl(RevProps)),
    ?MISC:format(", URL=\"1\", tooltip=\"~s\"", [PropStr]).


%% @spec nodedata_fields(NodeDataType::atom()) -> [RecordField::atom()]
%% @doc  Give back the record fields of the node data.
nodedata_fields({root})      -> [];
% Syntactical nodes
nodedata_fields(#file{})     -> record_info(fields, file);
nodedata_fields(#form{})     -> record_info(fields, form);
nodedata_fields(#clause{})   -> record_info(fields, clause);
nodedata_fields(#expr{})     -> record_info(fields, expr);
nodedata_fields(#lex{})      -> record_info(fields, lex);
nodedata_fields(#token{})    -> record_info(fields, token);
% Semantical nodes
nodedata_fields(#module{})   -> record_info(fields, module);
%nodedata_fields(#macro{})    -> record_info(fields, macro);
nodedata_fields(#record{})   -> record_info(fields, record);
nodedata_fields(#field{})    -> record_info(fields, field);
nodedata_fields(#func{})     -> record_info(fields, func);
nodedata_fields(#variable{}) -> record_info(fields, variable);
nodedata_fields(#env{})      -> record_info(fields, env).
