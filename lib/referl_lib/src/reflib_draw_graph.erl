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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc This module prints the syntax graph to a file.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Máté Tejfel <matej@inf.elte.hu>
-module(reflib_draw_graph).
-vsn("$Rev: 5654 $ ").

-include("lib.hrl").
-include_lib("referl_core/src/refcore_schema.hrl").

%%% ============================================================================
%%% Exports

-export([draw_graph/2, draw_graph/3, draw/5]).


%%% ============================================================================
%%% Graph drawing

%% @type filter() = 'all' | 'syn' | 'synlex' | 'sem' | 'dataflow' |
%%                  'synsem' | 'lex' | 'not_lex' | 'ctx' | 'all_env'.
%% Filters that can be applied when drawing the graph.
%% The filter `all' is the default; `syn' and `synlex' are of common use;
%% `sem' is used for storing semantic information about modules.

%% @spec draw_graph(ToFile :: string(), Filter :: atom())
%%                      -> ok | string()
%% @doc Draws the whole graph with the given filtering in verbose mode.
draw_graph(ToFile, Filter) ->
    draw(node, ?Graph:root(), Filter, ToFile, false).

%% @spec draw_graph(ToFile :: string(), Filter :: atom(), ToolTip::boolean())
%%                      -> ok | string()
%% @doc Draws the whole graph with the given filtering.
draw_graph(ToFile, Filter, ToolTip) ->
    draw(node, ?Graph:root(), Filter, ToFile, ToolTip).

%% @spec draw(Type :: func | argfuncs | node,
%%            Args :: arglist()|[{atom(),atom(),integer()}],
%%            Filter :: atom(), ToFile :: string(), ToolTip::boolean())
%%                      -> ok | string()
%% @doc  Draws the reachable nodes to the file `ToFile',
%%       the originating node(s) are described by `Type' and `Args'.
%%       Only the links indicated by `Filter' are traversed.
%%       Node text verbosity can be controlled by `ToolTip'.
draw(Type, Args, Filter, ToFile, ToolTip) ->
    ets:new(nodes, [named_table]),
    ets:insert(nodes, {max, 0}),
    try
        {ok, Dev} = file:open(ToFile, [write]),
        io:format(Dev, "digraph erl {~n", []),
        {ErrTxt, Nodes} = collect_nodes(Type, Args),
        %% todo Make an option out of the additional function.
        case Filter of
            semdf ->
                [?Syn:walk_graph(Node, Filter,
                                 fun store_and_draw_node/4,
                                 {Dev, ToolTip}, fun refanal_dataflow:back_nodes/1)
                    || Node <- Nodes];
            _ ->
                [?Syn:walk_graph(Node, Filter,
                                 fun store_and_draw_node/4,
                                 {Dev, ToolTip})
                    || Node <- Nodes]
        end,
        io:format(Dev, "}~n", []),
        file:close(Dev),
        case ErrTxt of
            "" -> ok;
            _  -> ErrTxt
        end
    catch
        error:{badmatch, {error, Reason}} ->
            "Dump failed: " ++ file:format_error(Reason)
    after
        ets:delete(nodes)
    end.


%% Collects the nodes that serve as the initial nodes of the graph walk.
collect_nodes(func, {Mod, Name, Arity}) ->
    collect_nodes(funcs, [{Mod, Name, Arity}]);
collect_nodes(argfuncs, Args) ->
    FunDefs = [ hd(?Query:exec(Fun, ?Fun:definition())) ||
                    Fun <- ?Args:functions(Args)],
    {"", FunDefs};
collect_nodes(funcs, Args) ->
    lists:foldl(
        fun ({Mod,Name,Arity}, {Param, DefNodes}) ->
            case ?Query:exec(?Mod:find_fundef(Mod, Name, Arity)) of
                []  ->
                    ErrMsg = ?MISC:format("The function ~p:~p/~b is unavailable",
                                           [Mod, Name, Arity]),
                    {Param ++ ErrMsg, DefNodes};
                [Def|_] ->
                    {Param, DefNodes ++ [Def]}
            end
        end, {"", []}, Args);
collect_nodes(node, Node) ->
    {"", [Node]}.


%%% ----------------------------------------------------------------------------
%%% ETS graph walk callbacks

%% A callback function to use with `?Syn:walk_graph'.
%% Returns whether `Node' has already been reached during the traversal.
node_is_done(Node, _Dev) ->
    [] =/= ets:lookup(nodes, Node).

%% A callback function to use with `?Syn:walk_graph'.
%% It enters the node in the `nodes' ETS table
%% that keeps track of the nodes reached in the traversal,
%% then prints the node and its links into the file for Graphviz.
store_and_draw_node(Node = {'$gn', From, FromIdx}, St = {Dev, ToolTip}, Links, WalkNext) ->
    Ind = ets:update_counter(nodes, max, 1),
    ets:insert(nodes, {Node, Ind}),

    Fmt = io_lib:format("\"N~p~b\" [~s]", [From, FromIdx, nodelabel(Node, ToolTip)]),
    NoEndl = re:replace(Fmt, "\n", "\\\\n", [{return, list}, global]),
    io:put_chars(Dev, NoEndl ++ "\n"),

    WalkNext(St, fun node_is_done/2),

    [ io:format(Dev, "\"N~p~b\" -> \"N~p~b\" [~s]~n",
                [From, FromIdx, To, ToIdx, linklabel(Tag, TagIdx, ToolTip)])
        || {Tag, TagIdx, {'$gn', To, ToIdx}} <- ?Syn:reindex_links(Links)],
    St.


%%% ============================================================================
%%% Implementation of the labels


%% @doc Returns the Graphviz label of the node.
nodelabel(Node = {_,_, Index}, HasToolTip) ->
    Data = ?Graph:data(Node),
    ToolTipText = tooltipStr(Node, Data),
    LabelSimplicity =
        case HasToolTip of
            true  -> only_text;
            false -> all_info
        end,
    nodelabel(Node, Index, ToolTipText, LabelSimplicity).

nodelabel(Node, Index, ToolTipText, LabelSimplicity) ->
    Data          = ?Graph:data(Node),
    Shape         = shape(Data),
    Content       = label(Index, Data),
    IsRecordShape = Shape == "Mrecord" orelse Shape == "record",
    Label         = label_shape(IsRecordShape, Content, Index),
    LabelText     = simplify_label(LabelSimplicity, Label),
    io_lib:format("~s shape=\"~s\", label=\"~s\", fontsize=\"~p\" ~s",
                  [nodestyle(Data), Shape, LabelText,
                   labelsize(Data), ToolTipText]).

%% Controls how much information is displayed on the label.
%% Expected values are `only_text', `text_index' and `all_info'.
simplify_label(only_text, [${,_,"|",[${,_,"|",Text,$}],$}]) ->
    Text;
simplify_label(only_text, [${,_,"|",Text,$}]) ->
    Text;
% simplify_label(text_index, [${,[${,_,"|",Index,$}],"|",[${,_,"|",Text,$}],$}]) ->
%     ["{{", Text, "|", Index, "}}"];
% simplify_label(text_index, [${,[${,_,"|",Index,$}],"|",Text,$}]) ->
%     ["{{", Text, "|", Index, "}}"];
simplify_label(_, Text) ->
    Text.

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
label(#expr{role=R, type=T, value=V}) -> explab(R,T,V);
label(#clause{var=_V, type=T})       -> {"clause", atom_to_list(T)};
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


linklabel(Tag, Ind, ToolTip) ->
    C=color(Tag),
    case ToolTip of
        false ->
            io_lib:format("label=\"~s/~b\", color=\"~s\", fontcolor=\"~s\"",
                            [Tag, Ind, C, C]);
        true  ->
            io_lib:format("color=\"~s\", tooltip=\"~s/~b\"",
                            [C, Tag, Ind])
    end.

explab(Role, Type, Val) ->
    V = value_text(Val, Type),
    if
        V =:= unknown     -> {Role, Type};
        true              -> {Role, {Type, V}}
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
value_text(undefined, Type)        -> Type;
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
color(funlref) -> magenta;
color(funeref) -> magenta;
color(dynfunlref) -> magenta;
color(dynfuneref) -> magenta;
color(ambfunlref) -> magenta;
color(ambfuneref) -> magenta;
color(funcall) -> magenta;

color(vardef) -> brown;
color(varvis) -> brown;
color(varref) -> brown;
color(varbind) -> brown;
color(varintro) -> brown;

color(top) -> gray50;

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

color(flow) -> indigo;
color(sel_e) -> orange;
color(sel) -> orange;
color(cons_e) -> orchid;
color(cons_back) -> orchid;
color(dep) -> orangered4;

color(_) -> black.



%% -----------------------------------------------------------------------------
%% ToolTip texts

%% @spec tooltipStr(Node::node(), Data::tuple()) -> string()
%% @doc  Write the properties of the `Node' into a string. This text used as
%%  graphviz dot attribute for alt text in SVG format.
%%  String format:
%%  `, URL="#ok", tooltip="propKey1=propValue1&#13;&#10;...propKeyN=propValueN"'
%% The `URL' tag is required by some SVG viewer to show the tooltip on hoover.
tooltipStr(Node, Data) ->
    LineSeparator = "&#13;&#10;",
    DataRec = element(1, Data),
    Props = [{node,Node}, {record, DataRec}
                | ?MISC:record_to_proplist(Data,nodedata_fields(Data))],
    Txt1 = [?MISC:format("~p=~p", [Key, Val]) || {Key, Val} <- Props],
    Txt2 = [?MISC:string_replace(Txt, ["\""], "&quot;", 0) || Txt <- Txt1],
    PropStr = ?MISC:join(Txt2, LineSeparator),
    ?MISC:format(", URL=\"#ok\", tooltip=\"~s\"", [PropStr]).


%% @spec nodedata_fields(NodeDataType::atom()) -> [RecordField::atom()]
%% @doc  Give back the record fields of the node data.
nodedata_fields({root})      -> [];
% Syntactical nodes
nodedata_fields(#file{})     -> record_info(fields, file);
nodedata_fields(#form{})     -> record_info(fields, form);
nodedata_fields(#clause{})   -> record_info(fields, clause);
nodedata_fields(#expr{})     -> record_info(fields, expr);
nodedata_fields(#typexp{})   -> record_info(fields, typexp);
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
