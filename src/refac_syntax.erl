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
%%% @doc Common syntax utilities module.
%%%
%%% @author Robert Kitlei

%%% TODO:
%%%    * separate_whitespace gives bad results for embedded whitespace in
%%%      strings

-module(refac_syntax).
-vsn("$Rev: 1206 $").

-export([lex_elem/2, syn_elem/2]).

-export([ add_tokens/1, add_tokens/2, filter_for/2, get_filenode/1 ,
          get_token_by_position/2, get_path_to_root/1, get_top_expression/2,
          to_list/1, all_children/2 ]).

-export([file/1]).

-export([contents/0, contents/1]).

-include("refactorerl.hrl").



%% @spec lex_elem(atom(), string()) -> #token{}
%% @doc Creates lexical element data. The text of the element is given in
%% `Text', and it is turned into a token of type `Type'.
lex_elem(Type, Text) ->
    {Val, Pre, Post} = lex_value(Type, Text),
    #token{type= Type, text=Text, value=Val,
           ws = #ws{bef=Pre, aft=Post}}.

%% @spec syn_elem(refac_graph:data(), [Link]) -> node()
%%       Link = {tag(), node()} | [Link]
%%
%% @doc Creates a node in the syntax tree. The node will have `Data' as
%% its data tag and `Links' will be used as its children.
syn_elem(Data, Links) ->
    Node = ?ESG:create(Data),
    ParentType = element(1, Data),
    lists:foreach(
        fun
            ({'$token', {_, _, {_, TokenNode}}}) ->
                LinkTag = token_link_tag(ParentType),
                ?GRAPH:mklink(Node, LinkTag, TokenNode);
            ({Tag, {To, _, _}}) ->
                ?ESG:insert(Node, Tag, To)
        end, lists:flatten(Links)),
    FirstTokenNode = get_first_token_node(lists:flatten(Links)),
    LastTokenNode  = get_last_token_node(lists:flatten(lists:reverse(Links))),
    insert_first_token(element(1, Data), Node, FirstTokenNode),
    insert_last_token(element(1, Data), Node, LastTokenNode),
    {Node, FirstTokenNode, LastTokenNode}.

%% Returns the appropriate tag to link a token below the parent.
token_link_tag(form)   -> flex;
token_link_tag(clause) -> clex;
token_link_tag(expr)   -> elex.

%% Returns the first token node of the right hand side.
get_first_token_node([[]|Xs]) ->
    get_first_token_node(Xs);
get_first_token_node([{'$token', {_, _, {_,TokenNode}}}|_]) ->
    TokenNode;
get_first_token_node([{_, {_, FirstTokenNode, _}}|_]) ->
    FirstTokenNode.

%% Returns the last token node of the right hand side.
get_last_token_node([[]|Xs]) ->
    get_last_token_node(Xs);
get_last_token_node([{'$token', {_, _, {_,TokenNode}}}|_]) ->
    TokenNode;
get_last_token_node([{_, {_, _, LastToken}}|_]) ->
    LastToken.

%% Inserts the first token node below the most recently built node.
insert_first_token(expr, Node, TokenNode) ->
    ?GRAPH:mklink(Node, efirst, TokenNode);
insert_first_token(clause, Node, TokenNode) ->
    ?GRAPH:mklink(Node, cfirst, TokenNode);
insert_first_token(form, _, _) ->
    ok.

%% Inserts the last token node below the most recently built node.
insert_last_token(expr, Node, TokenNode) ->
    ?GRAPH:mklink(Node, elast, TokenNode);
insert_last_token(clause, Node, TokenNode) ->
    ?GRAPH:mklink(Node, clast, TokenNode);
insert_last_token(form, _, _) ->
    ok.

%% @spec file(string()) -> not_found | {file, node()}
%%
%% @doc Returns the graph node of the file `Filename' if it is stored in the
%% graph.
file(Filename) ->
    Path = [{file, {path, '==', Filename}}],
    case ?GRAPH:path(?GRAPH:root(), Path) of
        [File] ->
            {file, File};
        [] ->
            not_found;
        _ ->
            erlang:error({multiple_file, Filename})
    end.
                         


%% =====================================================================
%% @spec get_token_by_position(
%%           Filename :: string(), Position :: integer())
%%            ->   {TokenNode :: node(), Location :: atom(),
%%                  PosInText :: integer(), Char :: char()}
%%               | error_position_beyond_eof
%%
%% @doc
%% Returns information about the token that is
%% at the specified location in the file.
%%
%% Parameter description:<pre>
%% <b>Filename</b> : The file that contains the token.
%% <b>Position</b> : The position in the file.
%% </pre>
%% Return values:<pre>
%% <b>TokenNode</b> : The token node.
%% <b>Location</b> : Specifies whether the character occurs
%%                   in the token text itself or
%%                   in the whitespace before or after it.
%%                   Possible values are
%%                   beforeToken, in_token and afterToken.
%% <b>PosInText</b> : The position of the character in the text,
%%                    be it token text or whitespace.
%% <b>Char</b> : The pointed character itself.
%% </pre>
%% @end
%% =====================================================================
get_token_by_position(Filename, Position) ->
    FileNode = file(Filename),
    TokenNodes = ?GRAPH:path(element(2,FileNode), [form_first_token]),
    get_token(TokenNodes, Position).


%% =====================================================================
%% @spec get_token(TokenNodes :: [node()], Position :: integer())
%%         ->  {TokenNode :: node(), Location :: atom(),
%%              PosInText :: integer(), Char :: char()}
%%           | error_position_beyond_eof
%%
%% @doc
%% Returns information about the token that is
%% at the specified location in the file.
%%
%% Parameter description:<pre>
%% <b>TokenNodes</b> : The list of token nodes to be inspected.
%% <b>Position</b> : The position in the file.
%% </pre>
%% Return values:<pre>
%% <b>TokenNode</b> : The token node.
%% <b>Location</b> : Specifies whether the character occurs
%%                   in the token text itself or
%%                   in the whitespace before or after it.
%%                   Possible values are
%%                   beforeToken, in_token and afterToken.
%% <b>PosInText</b> : The position of the character in the text,
%%                    be it token text or whitespace.
%% <b>Char</b> : The pointed character itself.
%% </pre>
%% @end
%% =====================================================================
get_token([], _) -> error_position_beyond_eof;
get_token([TokenNode|TokenNodes], Position) ->
    #token{value=Value, ws=WS} = ?GRAPH:data(TokenNode),
    #ws{bef=BT, aft=AT} = WS,
    V = to_list(Value),
    LBT = length(BT),
    LV = length(V),
    LAT = length(AT),
    if
        Position =< LBT andalso LBT /= 0 ->
            {TokenNode, beforeToken, Position, lists:nth(Position, BT)};
        Position =< LBT + LV andalso LV /= 0 ->
            Pos = Position - LBT,
            {TokenNode, in_token, Pos, lists:nth(Pos, V)};
        Position =< LBT + LV + LAT andalso LAT /= 0 ->
            Pos = Position - LBT - LV,
            {TokenNode, in_token, Pos, lists:nth(Pos, AT)};
        true ->
            NextTokenNode = ?GRAPH:path(TokenNode, [next]),
            get_token(NextTokenNode ++ TokenNodes, Position - LBT - LV - LAT)
    end.



%% =====================================================================
%% @spec get_path_to_root(Node :: [node()])
%%         -> [node()] | no_path_found
%%
%% @doc
%% Returns the list of nodes from the root node
%% leading to the specified node.
%%
%% Parameter description:<pre>
%% <b>Node</b> : The goal node.
%% </pre>
%% @end
%% =====================================================================
get_path_to_root(Node) ->
    get_path_to_root(Node, []).
get_path_to_root(Node, Path) ->
    case parent(Node) of
        none ->
            [Node | Path];
        Parent ->
            get_path_to_root(Parent, [Node | Path])
    end.

parent(Node) ->
    PN = [?GRAPH:path(Node, [{Tag,back}]) ||
             Tag <- parent_tags(element(1, ?GRAPH:data(Node)))],
    case lists:append(PN) of
        [] -> none;
        [P|_] -> P
    end.

parent_tags(root)   -> [];
parent_tags(file)   -> [file, included];
parent_tags(form)   -> [form];
parent_tags(clause) -> [aftercl, exprcl, handlercl, funcl];
parent_tags(expr)   -> [body, guard, name, pattern, tmout, sub, attr];
parent_tags(token)  -> [token_file, clause_token, expr_token, form_token,
                        form_stop_token, macro_token];
parent_tags(macro)  -> [macro].
    


%% =====================================================================
%% @spec get_top_expression(Ts1 :: [node()], Ts2 :: [node()])
%%         -> {one_token, [node()]}
%%          | { TopExpression :: node(), PathToTop :: [node()],
%%              Rest1 :: [node()], Rest2 :: [node()]}
%%
%% @doc
%% Returns the list of nodes from the root node
%% leading to the specified node.
%%
%% Parameter description:<pre>
%% <b>Ts1, Ts2</b> : The paths from the root node to the nodes.
%% </pre>
%% Return values:<pre>
%% <b>TopExpression</b> : The node of the top expression.
%% <b>PathToTop</b> : The path from the root node to the top expression node
%%                    (not including it).
%% <b>Rest1, Rest2</b> : The rest of the paths (after the top expression node).
%% </pre>
%% @end
%% =====================================================================
get_top_expression(Ts, Ts) -> {one_token, Ts};
get_top_expression(Ts1, Ts2) -> get_top_expression(Ts1, Ts2, [], []).

get_top_expression([T1|Ts],[T1|Ts2],Path,Prev) ->
        get_top_expression(Ts,Ts2, Path ++ [Prev], T1);
get_top_expression(Ts1,Ts2,Path,Prev) -> {Prev, tl(Path), Ts1, Ts2}.



%% =====================================================================
%% @spec add_tokens(TokenNodes :: [node()]) -> node()
%%
%% @doc
%% Adds edge connections (next) between a list of tokens.
%% Returns the last token node.
%%
%% Parameter description:<pre>
%% <b>TokenNodes</b> : The token nodes to be connected.
%% </pre>
%% @end
%% =====================================================================
add_tokens(TokenNodes) -> add_tokens(no_prev, TokenNodes).

add_tokens(Prev, TokenNodes) ->
    lists:foldl(
        fun
            (Node, no_prev) -> Node;
            (TN2, TN1) -> ?GRAPH:mklink(TN1, next, TN2), TN2
        end, Prev, TokenNodes).


%% =====================================================================
%% @spec get_filenode(Filename :: string) -> [node()]
%%
%% @doc
%% Returns the token node of a file.
%%
%% Parameter description:<pre>
%% <b>Filename</b> : The name of the file.
%% </pre>
%% @end
%% =====================================================================
get_filenode(Filename) ->
    FilenameAtom = list_to_atom(Filename),
    filter_for( [ file ]
              , [ {form, { {type, '==', attrib}, 'and'
                         , {kind, '==', module}}}
                , {attr, {{ {type, '==', expr}, 'and'
                          , {kind, '==', atom}}, 'and'
                          , {value, '==', FilenameAtom}}}
                ]).


%% =====================================================================
%% @spec filter_for(Path, FilterPath) -> [node()]
%%
%% @doc
%% Filters a path expression by a sub-path.
%% Can be imagined as a compound path expression (the two concatenated),
%% where a level different from the last is returned.
%%
%% Parameter description:<pre>
%% <b>Path</b> : The first part of the compound expression, which leads
%%               up to the nodes that are returned.
%% <b>FilterPath</b> : The second part of the compound expression,
%%                     that starts directly below the target nodes
%%                     and filters them by placing constraints
%%                     in the path expression.
%% </pre>
%% @end
%% =====================================================================
filter_for(Path, FilterPath) ->
    Goal = ?GRAPH:path(?GRAPH:root(), Path),
    lists:filter(
        fun(Node) ->
            ?GRAPH:path(Node, FilterPath) /= []
        end, Goal).



%% =====================================================================
%% @spec to_list(V) -> string()
%%
%% @doc
%% Converts an atom, integer or string to string.
%%
%% Parameter description:<pre>
%% <b>V</b> : The value to be converted.
%% </pre>
%% @end
%% =====================================================================
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_integer(V) ->
    integer_to_list(V);
to_list(V) ->
    V.



%% =====================================================================
%% @spec all_children(Node :: node(), Tag :: atom()) -> [node()]
%%
%% @doc
%% Returns all nodes below a node that are connected by edges
%% that are labelled Tag.
%%
%% Parameter description:<pre>
%% <b>Node</b> : The start node.
%% <b>Tag</b> : The tag of the connection.
%% </pre>
%% @end
%% =====================================================================
all_children(Node, Tag) ->
    Children = ?GRAPH:path(Node, [Tag]),
    Children ++ lists:flatmap(fun(Child) ->
                                  all_children(Child, Tag)
                              end, Children).



%% Is this character a whitespace?
ws($\ ) -> true;
ws($\t) -> true;
ws($\n) -> true;
ws($\r) -> true;
ws(_)   -> false.

nws(C) -> not ws(C). 

%% @spec lex_value(atom(), string()) -> {term(), string(), string()}
%%
%% @doc Computes the real value of a token of type `Type' while stripping
%% off whitespace from it. Returns a tuple of the value, the preceding
%% whitespace and the succeeding whitespace.

lex_value(Type, Text) ->
    {Ws, Body} = lists:splitwith(fun ws/1, Text),
    lex_value(Type, Body, Ws).

lex_value(Type, "%" ++ Text, Pre) ->
    {Cm, Next} = lists:splitwith(fun(C) -> C /= $\n end, Text),
    {Ws, Body} = lists:splitwith(fun ws/1, Next),
    lex_value(Type, Body, Pre ++ Cm ++ Ws);

lex_value(variable, Text, Pre) ->
    {Name, Post} = lists:splitwith(fun nws/1, Text),
    {Name, Pre, Post};

lex_value(string, Text, Pre) ->
    {Val, Post} = quoted_value(Text, $\"),
    {Val, Pre, Post};

lex_value(atom, Text, Pre) when hd(Text) == $\' ->
    {Name, Post} = quoted_value(Text, $\'),
    {list_to_atom(Name), Pre, Post};
lex_value(atom, Text, Pre) ->
    {Name, Post} = lists:splitwith(fun nws/1, Text),
    {list_to_atom(Name), Pre, Post};

lex_value(char, [$$, $\\ | Text], Pre) ->
    {Val, Post} = escape_value(Text),
    {Val, Pre, Post};
lex_value(char, [$$, C | Post], Pre) ->
    {C, Pre, Post};

lex_value(integer, Text, Pre) ->
    {Val, Post} = lists:splitwith(fun nws/1, Text),
    case lists:splitwith(fun (C) -> C /= $# end, Val) of
        {Base, [$# | Num]} ->
            {erlang:list_to_integer(Num, list_to_integer(Base)), Pre, Post};
        {Num, []} ->
            {list_to_integer(Num), Pre, Post}
    end;

lex_value(float, Text, Pre) ->
    {Val, Post} = lists:splitwith(fun nws/1, Text),
    {list_to_float(Val), Pre, Post};

lex_value(Type, Text, Pre) ->
    Kw = keyword_value(Type),
    case {atom_to_list(Kw), Text} of
        {KwT, [KwT|Post]} -> {Kw, Pre, Post};
        _ ->                 {Type, Pre, Text}
    end.

keyword_value('stop')         -> '.';
keyword_value('dot')          -> '.';
keyword_value('op_paren')     -> '(';
keyword_value('cl_paren')     -> ')';
keyword_value('op_brace')     -> '(';
keyword_value('cl_brace')     -> '}';
keyword_value('op_bracket')   -> '[';
keyword_value('cl_bracket')   -> ']';
keyword_value('dbl_less')     -> '<<';
keyword_value('dbl_greater')  -> '>>';
keyword_value('comma')        -> ',';
keyword_value('semicol')      -> ';';
keyword_value('colon')        -> ':';
keyword_value('arrow')        -> '->';
keyword_value('vline')        -> '|';
keyword_value('dbl_vline')    -> '||';
keyword_value('hash')         -> '#';
keyword_value('equal')        -> '=';
keyword_value('plus')         -> '+';
keyword_value('minus')        -> '-';
keyword_value('star')         -> '*';
keyword_value('slash')        -> '/';
keyword_value('exclam')       -> '!';
keyword_value('dbl_plus')     -> '++';
keyword_value('dbl_minus')    -> '--';
keyword_value('dbl_eq')       -> '==';
keyword_value('slasheq')      -> '/=';
keyword_value('eqcoleq')      -> '=:=';
keyword_value('eqslasheq')    -> '=/=';
keyword_value('less')         -> '<';
keyword_value('greater')      -> '>';
keyword_value('eqless')       -> '=<';
keyword_value('greatereq')    -> '>=';
keyword_value('backarrow')    -> '<-';
keyword_value('backbinarrow') -> '<=';
keyword_value('questionm')    -> '?';
keyword_value(Atom)           -> Atom.

quoted_value([Q | Tail], Q) -> quoted_tail_value(Tail, Q, "");
quoted_value(Str, _)        -> {"", Str}.

quoted_tail_value([Q | Tail], Q, Val) -> {lists:reverse(Val), Tail};
quoted_tail_value("", Q, Val) -> quoted_tail_value([Q], Q, Val);
quoted_tail_value([$\\ | Text], Q, Val) ->
    {Esc, Tail} = escape_value(Text),
    quoted_tail_value(Tail, Q, [Esc | Val]);
quoted_tail_value([Char | Tail], Q, Val) ->
    quoted_tail_value(Tail, Q, [Char|Val]).

escape_value([$b|T]) -> {$\b,T};
escape_value([$d|T]) -> {$\d,T};
escape_value([$e|T]) -> {$\e,T};
escape_value([$f|T]) -> {$\f,T};
escape_value([$n|T]) -> {$\n,T};
escape_value([$r|T]) -> {$\r,T};
escape_value([$s|T]) -> {$\s,T};
escape_value([$t|T]) -> {$\t,T};
escape_value([$v|T]) -> {$\v,T};
escape_value([D|T]) when D >= $0, D =< $7 ->
    escape_value([D], T);
escape_value([C|T])  -> {C,T}.

escape_value(Num, [D|T]) when D >= $0, D =< $7, length(Num) < 3 ->
    escape_value(Num ++ [D], T);
escape_value(Num, Tail) ->
    {erlang:list_to_integer(Num, 8), Tail}.

%% @doc Prints the outline of the graph to the standard output.
contents() ->
    [contents(file, File) || File <- ?GRAPH:path(?GRAPH:root(), [file])],
    ok.

contents(File) ->
    case file(File) of
        {file, F} ->
            contents(file, F),
            contents(forms, F);
        _ ->
            io:format("Not found.~n", [])
    end,
    ok.

contents(file, File) ->
    #file{path=Path} = ?GRAPH:data(File),
    io:format("~s~n", [Path]),
    [io:format("  inc ~s~n", [(?GRAPH:data(Inc))#file.path]) ||
        Inc <- ?GRAPH:path(File, [incl]) -- [File]],
    io:format("  ~b forms~n", [length(?GRAPH:path(File, [form]))]),
    case length(?GRAPH:path(File, [{form, {type, '==', error}}])) of
        0 -> ok;
        N -> io:format("  ~b errors~n", [N])
    end,
    io:format("  ~b macros~n", [length(?GRAPH:path(File, [macro]))]);

contents(forms, File) ->
    [contents(form, Form) || Form <- ?GRAPH:path(File, [form])];

contents(form, Form) ->
    case ?GRAPH:data(Form) of
        #form{type=func} ->
            case ?GRAPH:path(Form, [fundef]) of
                [F] ->
                    #func{name=N, arity=A} = ?GRAPH:data(F),
                    io:format("  func ~s/~b~n", [N,A]);
                [] ->
                    io:format("  func (??)~n", [])
            end;
        #form{tag=define} ->
            case ?GRAPH:path(Form, [ffirst,next,next,next]) of
                [N] ->
                    #lex{data=#token{value=Name}} = ?GRAPH:data(N),
                    io:format("  def ~s~n", [Name]);
                [] ->
                    io:format("  def (??)~n", [])
            end;
        #form{type=store} ->
            [T1] = ?GRAPH:path(Form, [ffirst]),
            [T2] = ?GRAPH:path(T1, [next]),
            io:format("  store: ~s~s~n",
                      [ T || #lex{data=#token{text=T}} <-
                                 lists:map(fun refac_graph:data/1, [T1,T2])]);
        #form{type=error, tag=Error} ->
            io:format("  Error: ~p~n", [Error]);
        #form{type=Type, tag=Tag} ->
            io:format("  ~s: ~s~n", [Type, Tag])
    end.
