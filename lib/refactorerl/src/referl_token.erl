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

%%% @doc Token related queries and manipulations. Tokens are represented with
%%% nodes of class `lex' with the `type' attribute set to `token'. There are
%%% two kinds of tokens. Real tokens come directly from source code, and their
%%% `data' attribute contains the token data (in a `#token{}' record). Virtual
%%% tokens are created by preprocessor substitutions, and they are usually
%%% originated from a real token. Virtual tokens do not contain token data
%%% directly, because it is the same as the original.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_token).
-vsn("$Rev: 2497 $").

%% Properties
-export([pos/1, pos/2, text/1, data/1]).

%% Queries
-export([file/0, form/0, expr/0, clause/0, original/0, virtuals/0]).

%% Manipulations
-export([build/2, keyword_value/1]).
-export([foldpos/3, foldpos/4]).
-export([len/1, len/2]).


-include("refactorerl.hrl").


%% ============================================================================
%% Token properties

%% @spec data(node()) -> #token{}
%% @doc Returns the original token data of the node. This data is not
%% available directly in the node in case of preprocessor substitutions.
data(Lex) ->
    try ?Graph:data(?Query:exec1(Lex, original(), original)) of
        #lex{type=token, data=D} -> D;
        _ -> erlang:error({bad_token, Lex})
    catch
        throw:original ->
            erlang:error({bad_token, Lex})
    end.



%% @spec text(node()) -> string()
%% @doc Returns the textual form of the token (together with surrounding
%% whitespace). The testual representation is exactly the same as the
%% appearance of the token in the source file.
text(LexNode) ->
    [OrigNode] = ?Query:exec([LexNode], original()),
    #lex{data=#token{text=T,prews=P,postws=S}} = ?Graph:data(OrigNode),
    P++T++S.

%% spec linecol(node()) -> {integer(), integer()}
%% doc Should return the line and column number of the token.
%% linecol(_) ->
%%    todo.

%% @spec pos(node()) -> {integer(), integer()}
%% @doc Returns the character position of `Token' in the source file that
%% contains the token. The returned tuple contains the indices of the first
%% and last characters of the token.
pos(Token) ->
    File = ?Query:exec1(Token, file(), token_file),
    pos(File, Token).

%% @spec pos(node(), node()) -> {integer(), integer()}
%% @doc Returns the position of `Token' assuming that it is located in `File'.
%% @see pos/1
pos(File, Token) ->
    foldpos(
      fun (Node, _, Start, End, _) when Node =:= Token -> {stop, {Start, End}};
          (_,_,_,_,Acc) -> {next,Acc}
      end, not_found, File).


%% ============================================================================
%% Queries starting from tokens

%% @spec file() -> query(#lex{}, #file{})
%% @doc The result query returns the file that contains the token.
file() ->
    fun (Lex) -> first(file, ?Syn:root_path(Lex)) end.

%% @spec form() -> query(#lex{}, #form{})
%% @doc The result query returns the form that contains the token.
form() ->
    fun (Lex) -> first(form, ?Syn:root_path(Lex)) end.

first(Tag, [{Tag, N}|_])    -> [N];
first(Tag, [_       |Tl]) -> first(Tag, Tl);
first(_,   [])              -> [].

%% @spec expr() -> query(#lex{}, #expr{})
%% @doc The result query returns the direct containing expression of the
%% token, or an empty list if the direct parent of the token is not an
%% expression.
expr() ->
    ?Query:seq(original(), [{elex,back}]).

%% @spec clause() -> query(#lex{}, #clause{})
%% @doc The result query returns the direct containing clause of the token, or
%% an empty list if the direct parent of the token is not a clause.
clause() ->
    ?Query:seq(original(), [{clex,back}]).

%% @spec original() -> query(#lex{}, #lex{})
%% @doc The result query returns the original of the token.
original() ->
    fun original/1.

original(LexNode) ->
    case ?Graph:path(LexNode, [orig]) of
        [] -> [LexNode];
        [OrigNode|_] -> original(OrigNode)
    end.


%% @spec virtuals() -> query(#lex{}, #lex{})
%% @doc The result query returns all virtual tokens originating from the token.
virtuals() ->
    fun (Lex) -> virtuals([Lex], []) end.

virtuals([Head | Tail], Virt) ->
    case ?Graph:path(Head, [{orig, back}]) of
        [] -> virtuals(Tail, Virt);
        New -> virtuals(New ++ Tail, New ++ Virt)
    end;
virtuals([], Virt) -> Virt.

%%% ============================================================================
%%% Token manipulations

%%% ----------------------------------------------------------------------------
%%% Generic file token handler

%% @spec foldpos(Fun, Acc0, File::node()) -> AccLast
%%
%%       Fun = (Token::node(), Data::#token{}, Start, End, Acc) ->
%%          {stop, AccLast} | {next, AccNext}
%%
%% @doc Similar to `lists:foldl/3' on the tokens (and their positions) of the
%% file, except that this can stop processing. The token node, token data, and
%% the indices of the first and last character of the token are passed to the
%% function.
foldpos(Fun, Acc0, File) ->
    foldpos(Fun, Acc0, File, both).

foldpos(Fun, Acc0, File, Ws) ->
    Tokens = ?Syn:leaves(File),
    foldpos(Fun, Ws, Acc0, Tokens, 1).

foldpos(_, _, Acc, [], _) ->
    Acc;
foldpos(Fun, Ws, Acc, [Head|Tail], Pos) ->
    #lex{data=Data} = ?ESG:data(Head),
    End = Pos + len(Data, Ws) - 1,
    case Fun(Head, Data, Pos, End, Acc) of
        {stop, Result} -> Result;
        {next, Acc1}   -> foldpos(Fun, Ws, Acc1, Tail, End+1)
    end.

len(Data) ->
    len(Data, both).
len(#token{text=Text, prews=Pre, postws=Post}, Tag) ->
    case Tag of
        none -> length(Text);
        pre  -> length(Pre)  + length(Text);
        post -> length(Post) + length(Text);
        both -> length(Pre)  + length(Text) + length(Post)
    end.

%%% ----------------------------------------------------------------------------
%%% Token data generation

%% @spec build(atom(), string()) -> #token{}
%% @doc Creates lexical element data. The text of the element is given in
%% `Text', and it is turned into a token of type `Type'.
build(Type, Text) ->
    #token{type=Type, text=Text, value=value(Type, ?MISC:strip(Text))}.


value(variable, Text) ->
    Text;

value(string, Text) ->
    quoted_value(Text, $\");

value(atom, Text) when hd(Text) == $\' ->
    list_to_atom(quoted_value(Text, $\'));
value(atom, Text) ->
    list_to_atom(Text);

value(char, [$$, $\\ | Text]) ->
    {Val, _} = escape_value(Text),
    Val;
value(char, [$$, C | _]) ->
    C;

value(integer, Text) ->
    case lists:splitwith(fun (C) -> C /= $# end, Text) of
        {Base, [$# | Num]} ->
            erlang:list_to_integer(Num, list_to_integer(Base));
        {Num, []} ->
            list_to_integer(Num)
    end;

value(float, Text) ->
    list_to_float(Text);

value(Type, _Text) ->
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
