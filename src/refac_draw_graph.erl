%%% The contents of this file are subject to the Mozilla Public License
%%% Version 1.1 (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.mozilla.org/MPL/
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
%%%
%%% Contributor(s): ______________________________________.
%%%
%%% @doc This module prints the syntax graph to a file.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(refac_draw_graph).
-vsn("$Rev: 1206 $").

-export([draw_graph/2]).

-include("refactorerl.hrl").


-define(MAX_TOKEN_COMMENT_LENGTH, 0).
% unlimited | 0 | 1 | ...


%% =====================================================================
%% @spec draw_graph(String, Filter) -> ok | string()
%%
%% @doc
%% Draws the whole graph to the file.
%%
%% Parameter description:<pre>
%% <b>File</b> : The name of the destination file.
%% </pre>
%% @end
%% =====================================================================
draw_graph(File, Filter) ->
    Root = ?GRAPH:root(),
    ets:new(nodes, [named_table]),
    ets:insert(nodes, {max, 0}),
    case file:open(File, [write]) of
        {ok, Dev} ->
            io:format(Dev, "digraph erl {~n", []),
%            io:format(Dev, "graph [ ordering=\"out\" ];~n", []),
            CollectedTokens = draw_graph(Dev, Root, filter(Filter), []),
            io:format(Dev, "{rank=same;~s}~n", [CollectedTokens]),
            io:format(Dev, "}~n", []),
            Ret = file:close(Dev);
        {error, Reason} ->
            Ret = "Dump failed: " ++ file:format_error(Reason)
    end,
    ets:delete(nodes),
    Ret.

%% @private
draw_graph(Dev, Node, Filter, Tokens) ->
    case ets:lookup(nodes, Node) of
        [] ->
            Ind = ets:update_counter(nodes, max, 1),
            IntInd = integer_to_list(Ind),
            ets:insert(nodes, {Node, Ind}),
            io:format(Dev, "N~b [~s]~n", [Ind, nodelabel(Node)]),

            NodeClass = element(1, ?GRAPH:data(Node)),
            AllTokens = case NodeClass of
                            token -> Tokens ++ ["N" ++ IntInd ++ ";"];
                            _     -> Tokens
                        end,
            Links = index([Link || Link = {Tag, _To} <- ?GRAPH:links(Node),
                                   Filter(NodeClass, Tag)]),
            CollectedTokens =
              lists:foldl(fun ({_Tag, _I, To}, Tokens2) ->
                            draw_graph(Dev, To, Filter, Tokens2)
                          end, AllTokens, Links),
            [ io:format(Dev, "N~b -> N~b [~s]~n",
                         [Ind,
                         element(2, hd(ets:lookup(nodes, To))),
                         linklabel(Tag, I)])
                ||
                {Tag, I, To} <- Links],
            CollectedTokens;
        _ -> Tokens
    end.

filter(all) ->
    fun all_links/2;
filter(sem) ->
    fun (From, Tag) ->
            semantic_link(From,Tag) orelse
                context_link(From,Tag) orelse
                syntax_link(From,Tag)
    end;
filter(ctx) ->
    fun (From,Tag) ->
            context_link(From,Tag) orelse syntax_link(From,Tag)
    end;
filter(syn) ->
    fun syntax_link/2;
filter(synlex) ->
    fun (From,Tag) ->
            syntax_link(From,Tag) orelse lexical_link(From,Tag)
    end;
filter(lex) ->
    fun lexical_link/2;
filter(inp) ->
    fun(_,Tag) ->
            Tag == file orelse
                Tag == form_first_token orelse
                Tag == next
    end;
filter(not_lex) ->
    fun not_lex/2.

not_lex(Node, Tag) ->
    Tag == file orelse not(lexical_link(Node, Tag)).

all_links(_, _) -> true.

lexical_link(root, file) -> true;
lexical_link(_, Tag) ->
    lists:member(Tag, [macro, token_file, included, macro_app,
                       form_first_token, form_token, form_stop_token,
                       clause_token, clause_first_token, clause_last_token,
                       expr_token, expr_first_token, expr_last_token, next,
                       application, macro_token, macro_definition,
                       macro_parameter, macro_par_instance, referred_macro,
                       m_appar, app_token, app_replacement_token,
                       app_par_token]).

syntax_link(clause, Tag) ->
    lists:member(Tag, [body, guard, name, pattern, tmout]);
syntax_link(expr, Tag) ->
    lists:member(Tag, [aftercl,exprcl,handlercl,sub]);
syntax_link(file, Tag) ->
    Tag == form;
syntax_link(form, Tag) ->
    Tag == attr orelse Tag == funcl;
syntax_link(root, file) ->
    true;
syntax_link(_, _) ->
    false.

context_link(clause, Tag) ->
    Tag == scope orelse Tag == visib;
context_link(expr, Tag) ->
    Tag == clause orelse Tag == sup;
context_link(_,_) ->
    false.

semantic_link(clause, Tag) ->
    lists:member(Tag, [modctx, vardef, varvis]);
semantic_link(expr, Tag) ->
    lists:member(Tag, [modref, varref, varbind, funref]);
semantic_link(file, Tag) ->
    Tag == moddef;
semantic_link(form, Tag) ->
    Tag == fundef;
semantic_link(Class, _) ->
    Class == module orelse Class == variable orelse Class == func.

%% @private
index(L) ->
    index(L, []).

%% @private
index([], _) -> [];
index([{Tag, To} | Tail], Inds) ->
    I = case proplists:get_value(Tag, Inds) of
            undefined -> 1;
            Ind -> Ind
        end,
    [{Tag, I, To} | index(Tail, orddict:store(Tag, I+1, Inds))].


%% @private
label(#lex{data=#token{type=Type
            ,text=Text
            ,ws=#ws{bef=BT ,aft=AT  }}})  ->
    case ?MAX_TOKEN_COMMENT_LENGTH of
        0 ->
            ["Mrecord", "{<here> " ++ atom_to_list(Type) ++ "|" ++
                       graph_print_token(Text) ++ "}"];
        _ ->
            ["Mrecord", "{<here> " ++ atom_to_list(Type) ++ "|{" ++
                           print_comment(BT) ++ "|" ++
                       graph_print_token(Text) ++ "|" ++
                       print_comment(AT) ++ "}}"]
    end;
label(#lex{type=T})                   -> ["diamond", atom_to_list(T)];
label(#file{path=N})                  -> ["box", filename:basename(N)];
label(#variable{name=N})              -> ["hexagon", N];
label(#module{name=N})                -> ["hexagon", N];
label(#func{name=N, arity=A})         -> ["hexagon", [atom_to_list(N),
                                                      "/",integer_to_list(A)]];
label(#form{type=attrib, tag=T})      -> ["box", atom_to_list(T)];
label(#form{type=func})               -> ["box", "function"];
label(#form{type=include, tag=F})     -> ["record", "{include|"++F++"}"];
label(#form{type=define})             -> ["box", "define"];
label(#form{type=ppcond, tag=T})      -> ["box", atom_to_list(T)];
label(#macro{name=N})                 -> ["Mrecord", "{macro|" ++ N ++ "}"];
label(#clause{type=T})                -> ["record",
                                          "{clause|" ++ atom_to_list(T) ++"}"];
label(#expr{type=T, kind=K, value=V}) -> explab(T,K,V);
label({root})                         -> ["triangle", "ROOT"];
label(T) when is_tuple(T)             -> ["octagon",
                                          atom_to_list(element(1, T))].


%% @private
graph_print_token("->") -> "-\\>";
graph_print_token("{") -> "\\{";
graph_print_token("}") -> "\\}";
graph_print_token("@") -> "\\@";
graph_print_token(Text) -> quo(Text).


%% @private
print_comment(List) ->
  Comment =
    lists:map(fun ($\n)  -> "\\l";
                   ($\{)  -> "\\{";
                  ($\})  -> "\\}";
                  ($<)   -> "\\<";
                  ($>)   -> "\\>";
                  (Char) -> Char
              end, print_comment_flatten(List)),
  case ?MAX_TOKEN_COMMENT_LENGTH of
    unlimited -> Comment;
    0 -> "";
    N ->
      Length = length(Comment),
      if Length > N ->
        element(1, lists:split(N, Comment)) ++ "...";
      true ->
        Comment
      end
  end.

print_comment_flatten(List) ->
  lists:flatten(
  lists:map(fun(X) -> case X of {comment, Comment} -> Comment;
                                Text               -> Text
                      end
            end, List)).


%% @private
explab(Type, Kind, Val) ->
    V = case Kind of
            integer -> integer_to_list(Val);
            float -> float_to_list(Val);
            atom -> atom_to_list(Val);
            variable -> Val;
            char -> [Val];
            string -> quo(Val);
            infix_expr -> atom_to_list(Val);
            prefix_expr -> atom_to_list(Val);
            underscore -> "_";
            nil -> "[]";
            _ when Val =:= undefined -> atom_to_list(Kind);
            _ -> unknown
        end,
    T = case Type of
            expr -> "expr|";
            pattern -> "pattern|";
            guard -> "guard|"
        end,
    if
        V =:= unknown -> ["triangle", T++atom_to_list(Kind)];
        Val =:= undefined -> ["Mrecord", "{" ++ T ++ V ++ "}"];
        true -> ["Mrecord", "{" ++ T ++ V ++ "}"]
    end.


%% @private
nodelabel(Node) ->
    Data = ?GRAPH:data(Node),
    io_lib:format("shape=~s, label=\"~s\", fontsize=\"~p\"",
                  label(Data) ++ [labelsize(Data)]).


%% @private
labelsize(#token{}) -> 12;
labelsize(_) -> 18.


%% @private
linklabel(Tag, Ind) ->
    C=color(Tag),
    io_lib:format("label=\"~s/~b\", color=\"~s\", fontcolor=\"~s\"",
                  [Tag, Ind, C, C]).


%% @private
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

color(next)   -> steelblue;
color(flex)   -> steelblue;
color(ffirst) -> steelblue;
color(flast)  -> steelblue;
color(clex)   -> steelblue;
color(cfirst) -> steelblue;
color(clast)  -> steelblue;
color(elex)   -> steelblue;
color(efirst) -> steelblue;
color(elast)  -> steelblue;
color(llex)   -> steelblue;
color(lfirst) -> steelblue;
color(llast)  -> steelblue;

color(incl)  -> blue;
color(mref)  -> blue;
color(mbody) -> blue;
color(marg)  -> blue;
color(macro) -> blue;

color(_) -> black.


%% @private
quo(Str) ->
    lists:map(fun ($\") -> "\\\"";
                  ($\n) -> "\\\\n";
                  ($\\) -> "\\\\";
                  (C)   -> C
              end,
              Str).


%%% %% @private
%%% get_permitted_edges(Schema, schema) ->
%%%     {From, _, Edge} = lists:unzip3(Schema),
%%%     get_permitted_edges(lists:zip(From, Edge));
%%% get_permitted_edges(InitSchema, init) ->
%%%     get_permitted_edges(InitSchema).


%%% %% @private
%%% get_permitted_edges(Schema) ->
%%%     lists:foldl(
%%%         fun({From, EdgeList}, Edges) ->
%%%             Edges ++
%%%             lists:foldl(
%%%                 fun({Edge, To}, Edges2) ->
%%%                     Edges2 ++ [{From, To, Edge}]
%%%                 end, [], EdgeList)
%%%         end, [], Schema).
