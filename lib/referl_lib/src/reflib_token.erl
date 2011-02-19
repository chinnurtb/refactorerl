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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Token related queries and manipulations. Tokens are represented with
%%% nodes of class `lex' with the `type' attribute set to `token'. There are
%%% two kinds of tokens. Real tokens come directly from source code, and their
%%% `data' attribute contains the token data (in a `#token{}' record). Virtual
%%% tokens are created by preprocessor substitutions, and they are usually
%%% originated from a real token. Virtual tokens do not contain token data
%%% directly, because it is the same as the original.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(reflib_token).
-vsn("$Rev: 5640 $ ").

-export([get_value/1]).

%% Properties
-export([pos/1, pos/2, text/1, data/1, type/1, is_virtual/1]).
-export([map_pos/2, map_pos/3]).

%% Queries
-export([file/0, form/0, expr/0, typexp/0, clause/0, original/0, virtuals/0]).
-export([linecol/1]).

%% Manipulations
-export([build/2, keyword_value/1]).
-export([foldpos/3, foldpos/4]).
-export([len/1, len/2]).


-include("lib.hrl").


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

%% @spec type(node()) -> atom
%% @doc Returns the type of the token contained by the lexical node.
type(Lex) ->
    (?Token:data(Lex))#token.type.

%% @type line_col() = {natural(), natural()}

%% @spec linecol(node()) -> {line_col(),line_col()}
%% @doc Returns the line and column number of the first and last character
%%      of the token.
linecol(Token) ->
    File = ?Query:exec1(Token, file(), token_file),
    linecol(File, Token).

%% @doc Returns the line and column number of the first and last character
%%      of `Token', assuming it is located in `File'.
linecol(File,Token) ->
    pos(File,Token,linecol).

%% @spec pos(node()) -> {integer(), integer()}
%% @doc Returns the character position of `Token' in the source file that
%% contains the token. The returned tuple contains the indices of the first
%% and last characters of the token without whitespace.
pos(Token) ->
    File = ?Query:exec1(Token, file(), token_file),
    pos(File, Token).

%% @doc Returns the position of `Token' assuming that it is located in `File'.
%% @see pos/1
%% @spec (file_node(),token_node()) -> {integer(),integer()}
pos(File,Token) ->
    pos(File,Token,scalar).

pos(File,Token,PosType)->
    foldpos(
      fun (Node, _, Start, End, _) when Node =:= Token ->
              {stop,{Start,End}};
          (_,_,_,_,Acc) ->
              {next,Acc}
      end, not_found, File, none, PosType).

%% @doc Returns the scalar position of all listed tokens from `File' in a
%% single run.
%% Note that it assumes that no duplicate token is given.
%% Use lists:usort/1 if in doubt.
%% @spec (#file{},[#token{}]) -> [{#token{},{integer(),integer()}}]
map_pos(File,Tokens)->
    map_pos(File,Tokens,scalar).

%% @doc Returns the position of all listed tokens from `File' in a single run.
%% Note that it assumes that no duplicate token is given.
%% Use lists:usort/1 if in doubt.
%% @spec (#file{},[#token{}],scalar|linecol) ->
%%  [{#token{},{integer(),integer()}}]
map_pos(File,Tokens,linecol) ->
    map_pos_file_or_form(File, Tokens, linecol);

map_pos(File,Tokens,PosType) ->
    Form2Tokens = ets:new(form2tokens, [bag]),
    Form2Pos    = ets:new(form2pos, [bag]),
    put_form_positions(File, Form2Pos),
    try
        [ets:insert(Form2Tokens, {f2t, token2form(Token), Token}) || Token <- Tokens],
        map_pos_form(ets:tab2list(Form2Tokens), Form2Pos, PosType)
    after
        ets:delete(Form2Tokens),
        ets:delete(Form2Pos)
    end.

put_form_positions(File, Form2Pos) ->
    Forms = ?Query:exec(File, ?File:real_forms()),
    lists:foldl(
        fun(Form, {Pos, LineCol}) ->
            ets:insert(Form2Pos, {f2p, Form, Pos}),
            #form{length=FLen, linecol=FLineCol} = ?ESG:data(Form),
            {adjust(Pos, FLen, scalar), adjust(LineCol, FLineCol, linecol)}
        end, {0, {0, 0}}, Forms).

map_pos_form([], _Form2Pos, _PosType) ->
    [];
map_pos_form([{f2t, Form, Token}|Rest], Form2Pos, PosType) ->
    {F2Tokens, Rest2} = lists:splitwith(fun({f2t, F, _}) -> F == Form end, Rest),
    Tokens = [Token | [T || {f2t, _, T} <- F2Tokens]],
    [{f2p, Form, FPos}] = ets:match_object(Form2Pos, {f2p, Form, '_'}),

    MappedPos = map_pos_file_or_form(Form, Tokens, PosType),
    Adjusted = [{T, {adjust(Pos1, FPos, PosType), adjust(Pos2, FPos, PosType)}} || {T, {Pos1, Pos2}} <- MappedPos],
    Adjusted ++ map_pos_form(Rest2, Form2Pos, PosType).

adjust(Pos, FPos, scalar) ->
    Pos + FPos;
adjust({0, Col}, {FLine, FCol}, linecol) ->
    {FLine, FCol + Col};
adjust({Line, Col}, {FLine, _FCol}, linecol) ->
    {Line + FLine, Col}.

map_pos_file_or_form(Form, Tokens, PosType) ->
    TokenSet = sets:from_list(Tokens),
    InitAcc = {length(Tokens),[]},
    {_,Result} =
        foldpos(
           fun (Node, _, Start, End, Acc) ->
                   case sets:is_element(Node,TokenSet) of
                       false ->
                           {next,Acc};
                       true ->
                           {N,Res} = Acc,
                           Res2 = [{Node,{Start,End}}|Res],
                           Acc2 = {N-1,Res2},
                           case N of
                               1 ->
                                   {stop,Acc2};
                               _ ->
                                   {next,Acc2}
                           end
                   end
           end, InitAcc, Form, none, PosType),
    Result.

token2form(Token) ->
    [{file, _}, {form, Form}|_] = ?Syn:root_path(Token),
    Form.


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

first(Tag, [{Tag, N}|_])  -> [N];
first(Tag, [_       |Tl]) -> first(Tag, Tl);
first(_,   [])            -> [].

%% @spec expr() -> query(#lex{}, #expr{})
%% @doc The result query returns the direct containing expression of the
%% token, or an empty list if the direct parent of the token is not an
%% expression.
expr() ->
    ?Query:seq(original(), [{elex,back}]).

%% @spec typexp() -> query(#lex{}, #typexp{})

%% @doc The result query returns the direct containing type expression
%% of the token, or an empty list if the direct parent of the token is
%% not a type expression.
typexp() ->
    ?Query:seq(original(), [{tlex,back}]).

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
        []           -> [LexNode];
        [OrigNode|_] -> original(OrigNode)
    end.


%% @spec virtuals() -> query(#lex{}, #lex{})
%% @doc The result query returns all virtual tokens originating from the token.
virtuals() ->
    fun (Lex) -> virtuals([Lex], []) end.

virtuals([Head | Tail], Virt) ->
    case ?Graph:path(Head, [{orig, back}]) of
        []  -> virtuals(Tail, Virt);
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

%% @type wsmask() = 'none' | 'pre' | 'post' | 'both'

%% @spec (Fun,Acc,node(),wsmask())->Acc
%%       Fun = (node(), #token{}, integer(), integer(), Acc) ->
%%          {stop, Acc} | {next, Acc}
foldpos(Fun, Acc0, File, Ws)->
    foldpos(Fun, Acc0, File, Ws, scalar).

foldpos(Fun, Acc0, File, Ws, PosType) ->
    Tokens = ?Syn:leaves(File),
    Start  = case PosType of
                 scalar -> 1;
                 linecol -> {1,1}
             end,
    foldpos0(Fun, Acc0, Tokens, Ws, Start, PosType).

foldpos0(Fun, Acc0, Tokens, none, Start, PosType)->
    foldpos1_none(Fun, Acc0, Tokens, Start, PosType);

foldpos0(Fun, Acc0, Tokens, Ws, Start, PosType) ->
    WsMask = ws2mask(Ws),
    StMask = lists:takewhile(fun id/1, ?MISC:map_not(WsMask)),
    foldpos1(Fun, {WsMask,StMask}, Acc0, Tokens, Start, PosType).

%% @private
foldpos1(_, _, Acc, [], _, _) ->
    Acc;
%@todo perhaps also support linecol
foldpos1(Fun, WsM={WsMask,StMask}, Acc, [Head|Tail], Pos, PosType=scalar) ->
    #lex{data=Data} = ?ESG:data(Head),
    Lens  = toklens(Data),
    Start = Pos   + sum_mask(StMask,Lens),
    End   = Start + sum_mask(WsMask,Lens) - 1,
    case Fun(Head, Data, Start, End, Acc) of
        {stop, Result} ->
            Result;
        {next, Acc1}   ->
            Next = Pos + lists:sum(Lens),
            foldpos1(Fun, WsM, Acc1, Tail, Next, PosType)
    end.

%% "Premature optimization is the root of all evil."
foldpos1_none(_, Acc, [], _, _) ->
    Acc;
foldpos1_none(Fun, Acc, [Head|Tail], Pos, PosType=scalar) ->
    #lex{data=Data} = ?ESG:data(Head),
    #token{prews=Pre, text=Text} = Data,
    Start = Pos   + length(Pre),
    End   = Start + length(Text) - 1,
    case Fun(Head, Data, Start, End, Acc) of
        {stop, Result} ->
            Result;
        {next, Acc1}   ->
            Next = End + 1 + length(Data#token.postws),
            foldpos1_none(Fun, Acc1, Tail, Next, PosType)
    end;

foldpos1_none(Fun, Acc, [Head|Tail], Pos, PosType=linecol) ->
    #lex{data=Data} = ?ESG:data(Head),
    #token{prews=Pre, text=Text} = Data,
    Start = lc_aggr(Pos,Pre), %@todo hm, could take a pretty PosType argument
    {InitT,LastT} = lists:split(length(Text)-1,Text),
    End   = lc_aggr(Start,InitT),
    case Fun(Head, Data, Start, End, Acc) of
        {stop, Result} ->
            Result;
        {next, Acc1}   ->
            Next = lc_aggr(End,[LastT|Data#token.postws]),
            foldpos1_none(Fun, Acc1, Tail, Next, PosType)
    end.

lc_aggr({L,C},Txt) ->
    ?MISC:string_linecol(Txt,inf,{1,L,C}).

id(X) ->
    X.

%% @spec (#token{}) -> natural()
len(Data) ->
    len(Data, both).
%% @spec (#token{},wsmask()) -> natural()
len(Data, Tag) ->
    sum_mask(ws2mask(Tag),toklens(Data)).

sum_mask(M,L) ->
    lists:sum(?MISC:mask(M,L)).

toklens(#token{prews=Pre, text=Text, postws=Post}) ->
    lists:map(fun length/1, [Pre,Text,Post]).

%% @spec (wsmask()) -> [bool()]
ws2mask(none) -> [false,true,false];
ws2mask(pre)  -> [true, true,false];
ws2mask(post) -> [false,true,true];
ws2mask(both) -> [true, true,true].


%%% ----------------------------------------------------------------------------
%%% Token data generation

%% todo Move this to the appropriate place, maybe even another module.
get_value(#token{type=Type, text=Text}) ->
    value(Type, ?MISC:strip(Text));
get_value(#lex{data=Token}) ->
    get_value(Token);
get_value(TokenNode) ->
    get_value(?Graph:data(TokenNode)).

%% @spec build(atom(), string()) -> #token{}
%% @doc Creates lexical element data. The text of the element is given in
%% `Text', and it is turned into a token of type `Type'.
build(Type, Text) ->
    #token{type=Type, text=Text}.


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
keyword_value(stop)	        -> '.';
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
escape_value([C|T])  -> {C,T};
escape_value("") -> {0, []}.

escape_value(Num, [D|T]) when D >= $0, D =< $7, length(Num) < 3 ->
    escape_value(Num ++ [D], T);
escape_value(Num, Tail) ->
    {erlang:list_to_integer(Num, 8), Tail}.

%% @spec is_virtual(Token::node()) -> true|false
%% @doc Returns true if the given token is a virtual token
is_virtual(Node) ->
    case ?Graph:class(Node) of
        lex ->
            case (?ESG:data(Node))#lex.data of
                virtual -> true;
                _ ->       false
            end;
        _   -> false
    end.
