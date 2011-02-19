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

%%% @doc Syntax utilities for the parser and lexer.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_synlex).
-vsn("$Rev: 1740 $").

-export([lex_elem/2, syn_elem/2, keyword_value/1]).


-include("refactorerl.hrl").


%% @spec lex_elem(atom(), string()) -> #token{}
%% @doc Creates lexical element data. The text of the element is given in
%% `Text', and it is turned into a token of type `Type'.
lex_elem(Type, Text) ->
    #token{type= Type, text=Text, value=lex_value(Type, ?MISC:strip(Text))}.


%% @spec syn_elem(referl_graph:data(), [Link]) -> node()
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
                LinkTag = ?LEX:token_link_tag(ParentType),
                ?ESG:insert(Node, LinkTag, TokenNode);
            ({Tag, To}) ->
                ?ESG:insert(Node, Tag, To)
        end, lists:flatten(Links)),
    Node.




%% @spec lex_value(atom(), string()) -> {term(), string(), string()}
%%
%% @doc Computes the real value of a token of type `Type' while stripping
%% off whitespace from it. Returns a tuple of the value, the preceding
%% whitespace and the succeeding whitespace.

lex_value(variable, Text) ->
    Text;

lex_value(string, Text) ->
    quoted_value(Text, $\");

lex_value(atom, Text) when hd(Text) == $\' ->
    list_to_atom(quoted_value(Text, $\'));
lex_value(atom, Text) ->
    list_to_atom(Text);

lex_value(char, [$$, $\\ | Text]) ->
    {Val, _} = escape_value(Text),
    Val;
lex_value(char, [$$, C | _]) ->
    C;

lex_value(integer, Text) ->
    case lists:splitwith(fun (C) -> C /= $# end, Text) of
        {Base, [$# | Num]} ->
            erlang:list_to_integer(Num, list_to_integer(Base));
        {Num, []} ->
            list_to_integer(Num)
    end;

lex_value(float, Text) ->
    list_to_float(Text);

lex_value(Type, _Text) ->
    keyword_value(Type).


%% @spec keyword_value(atom()) -> atom()
%% @doc  Returns the keyword value.
%% @TODO Should be auto-generated in the future.
keyword_value(stop)         -> '.';
keyword_value(dot)          -> '.';
keyword_value(op_paren)     -> '(';
keyword_value(cl_paren)     -> ')';
keyword_value(op_brace)     -> '{';
keyword_value(cl_brace)     -> '}';
keyword_value(op_bracket)   -> '[';
keyword_value(cl_bracket)   -> ']';
keyword_value(dbl_less)     -> '<<';
keyword_value(dbl_greater)  -> '>>';
keyword_value(comma)        -> ',';
keyword_value(semicol)      -> ';';
keyword_value(colon)        -> ':';
keyword_value(arrow)        -> '->';
keyword_value(vline)        -> '|';
keyword_value(dbl_vline)    -> '||';
keyword_value(hash)         -> '#';
keyword_value(equal)        -> '=';
keyword_value(plus)         -> '+';
keyword_value(minus)        -> '-';
keyword_value(star)         -> '*';
keyword_value(slash)        -> '/';
keyword_value(exclam)       -> '!';
keyword_value(dbl_plus)     -> '++';
keyword_value(dbl_minus)    -> '--';
keyword_value(dbl_eq)       -> '==';
keyword_value(slasheq)      -> '/=';
keyword_value(eqcoleq)      -> '=:=';
keyword_value(eqslasheq)    -> '=/=';
keyword_value(less)         -> '<';
keyword_value(greater)      -> '>';
keyword_value(eqless)       -> '=<';
keyword_value(greatereq)    -> '>=';
keyword_value(backarrow)    -> '<-';
keyword_value(backbinarrow) -> '<=';
keyword_value(questionm)    -> '?';
keyword_value(Atom)         -> Atom.

quoted_value([Q | Tail], Q) -> quoted_tail_value(Tail, Q, "");
quoted_value(Str, _)        -> {"", Str}.

quoted_tail_value([Q | _], Q, Val) -> lists:reverse(Val);
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
