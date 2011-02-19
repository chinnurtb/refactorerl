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

%%% @doc Indentation-based whitespace handler (a.k.a. pretty printer).
%%%
%%% Line endings inside expressions are not handled at all.
%%%
%%% After a separaqtor token, non-indented tokens do not need leading
%%% whitespace; this situation is not detected. This can even lead to a bug
%%% in case of record expressions (field name after the dot gets a leading
%%% space).
%%%
%%% One-liner clauses could be placed on one line, especially short fun
%%% expressions.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_anal_indent).
-vsn("$Rev: 1995 $").
-behaviour(referl_esg).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @private
init() -> [].

%% @private
insert(_, _, _, Clause, CD=#clause{indent=undefined}) ->
    Base = base_indent_cl(Clause),
    Ind = get_indent(Clause, CD, Base),
    ?GRAPH:update(Clause, CD#clause{indent=Ind});

insert(_, _, _, Expr, ED=#expr{indent=undefined}) ->
    Base = base_indent_ex(Expr),
    Ind = get_indent(Expr, ED, Base),
    ?GRAPH:update(Expr, ED#expr{indent=Ind});

insert(_, #lex{}, _, _, #lex{}) ->
    ok;

insert(Par, PD, _, Lex, #lex{data=#token{}}=L) ->
    Base = base_indent(Par, PD),
    indent(Par, PD, Lex, L, Base);

insert(_,_,_,_,_) ->
    ok.

%% @private
remove(_,_,_,_,_) ->
    ok.


%%% ----------------------------------------------------------------------------
%%% Indenting functions

get_indent(_Clause, #clause{kind=Kind}, _)
  when Kind == expr; Kind == pexpr; Kind == hexpr; Kind == compr ->
    "";
get_indent(Clause, #clause{}, Base) ->
    get_indent(Base, ?GRAPH:path(Clause, [body]));
get_indent(Expr, #expr{}, Base) ->
    get_indent(Base, ?GRAPH:path(Expr, [clause])).

get_indent(Base, Content) ->
    First = [?GRAPH:data(first_token(N)) || N <- Content],
    Ind = [if Pre == new; I /= undefined -> "    ";
              true -> lists:nthtail(length(Base), Pre)
           end ||
              #lex{data=#token{indent=I, prews=Pre}} <- First,
              Pre == new orelse (I==undefined andalso
                                 lists:prefix(Base, Pre))],
    if
        Ind == [] -> "    ";
        true -> hd(lists:usort(Ind))
    end.

 
indent(Par,PD,Token, #lex{data=#token{indent=Ind, prews=PreWS}=T}=L, Indent) ->
    if
        %% Generated token (implies Ind == undefined)
        PreWS == new ->
            {NewInd, Pre, Post} = whitespace(Par, PD, Token, T, Indent),
            ?GRAPH:update(Token,
                          L#lex{data=T#token{indent=NewInd,
                                             prews=Pre,
                                             postws=Post}});

        %% Source file token, first insertion: no WS changes
        Ind == undefined ->
            NewInd =
                case lists:prefix(Indent, PreWS) of
                    true -> length(Indent);
                    false ->0
                end,
            ?GRAPH:update(Token, L#lex{data=T#token{indent=NewInd}});

        %% Non-first insertion: re-indent
        is_integer(Ind) ->
            Rel = lists:nthtail(Ind, PreWS),
            {NewInd, Pre, Post} =
                whitespace(Par, PD, Token, T#token{prews=Rel}, Indent),
            ?GRAPH:update(Token, L#lex{data=T#token{indent=NewInd,
                                                    prews=Pre,
                                                    postws=Post}})
    end.


%%% ----------------------------------------------------------------------------
%%% Whitespace generation

whitespace(Par, #clause{}, Tok, #token{prews=Pre,type=comma}, _Ind) ->
    [_, {Next, _} | _] =
        lists:dropwhile(
          fun({_, N}) -> N =/= Tok end,
          ?SYNTAX:children(Par)),
    PW = if Pre == new -> ""; true -> Pre end,
    if
        Next == body -> {0, PW, "\n"};
        Next /= body -> {0, PW, ""}
    end;

whitespace(Par, PD, Tok, #token{type=Type, prews=Pre, postws=Post}, Ind) ->
    {I,Pre1} =
        case token_indent(Par, PD, Tok, Type) of
            newline ->
                {1, "\n"};
            indent when Pre == new ->
                {length(Ind), Ind};
            indent ->
                {length(Ind), Ind++Pre};
            none when Pre == new ->
                {0, ""};
            none ->
                {0, Pre};
            space when Pre == new; Pre == ""  ->
                {1, " "};
            space ->
                {0, Pre}
        end,
    Comment = lists:member($%, Post),
    {I, Pre1,
     case token_eol(Par, PD, Tok, Type) of
         eol when not Comment -> "\n";
         noeol when not Comment -> "";
         _ -> Post
     end}.


token_indent(Par, PD, Tok, Type) ->
    lists:foldl(
      fun
          ({T, Fun}, space) ->
              case Fun() of
                  true -> T;
                  false -> space
              end;
          (_, T) -> T
      end,
      space,
      [{newline,
        fun() ->  is_record(PD, expr) andalso
                      ?GRAPH:path(Par, [{name,back}]) /= [] end},
       {indent,
        fun() -> (is_record(PD, expr) andalso
                  PD#expr.kind == try_expr andalso Type == 'catch')
                     orelse Type == 'end'
                     orelse Type == 'after'
                     orelse indented_token(Par, PD, Tok) end},
       {none,
        fun() -> lists:member(Type,
                              [dot, op_paren, cl_paren, op_brace, cl_brace,
                               op_bracket, cl_bracket, comma, dbl_less,
                               dbl_greater, colon, hash, semicol, stop])
        end}]).

indented_token(Clause, D=#clause{}, Tok) ->
    is_first(Clause, D, Tok);
indented_token(Expr, D=#expr{}, Tok) ->
    is_first(Expr, D, Tok) andalso indented_parent(Expr);
indented_token(_, _, _) ->
    false.

indented_parent(Expr) ->
    case anypath(Expr, [{body, back}, {pattern, back}, {guard, back},
                        {tmout, back}, {sub, back}]) of
        [] -> false;
        [P|_] -> indented_node(P, ?GRAPH:data(P), Expr)
    end.

anypath(Node, Paths) ->
    lists:foldl(
      fun
          (P,[]) -> ?GRAPH:path(Node, [P]);
          (_,L)  -> L
      end,
      [],
      Paths).


indented_node(Cl, #clause{kind=Kind}, _)
  when Kind == expr; Kind == pexpr; Kind == hexpr; Kind == compr ->
    [P] = ?GRAPH:path(Cl, [{clause, back}]),
    indented_node(P, ?GRAPH:data(P), Cl);
indented_node(Clause, CD=#clause{}, Child) ->
    ?GRAPH:index(Clause, body, Child) /= none
        orelse is_first(Clause, CD, Child);
indented_node(Expr, D=#expr{}, Child) ->
    Tag =
        case hd(?NODE_STRUCTURE:node_structure(D)) of
            {token, _} -> [];
            {symbol, T} -> T;
            {repeat, _, T} -> T
        end,
    is_atom(Tag) andalso
        [Child] == ?GRAPH:path(Expr, [{Tag, 1}]) andalso
        indented_parent(Expr).



token_eol(_, _, _, stop) ->
    eol;
token_eol(Par, PD, Tok, Type) ->
    case lists:member(Type, ['of', 'begin', 'if', 'fun', 'receive',
                             'try', 'after', arrow, semicol]) of
        true -> eol;
        false ->
            case is_last(Par, PD, Tok) of
                true -> token_eol(Par, PD);
                false -> leave
            end
    end.

token_eol(Expr, #expr{}) ->
    case ?GRAPH:path(Expr, [{body, back}]) ++
        ?GRAPH:path(Expr, [{sub, back}]) of
        [] -> leave;
        [Parent|_] ->
            PD = ?GRAPH:data(Parent),
            case is_last(Parent, PD, Expr) of
                true -> token_eol(Parent, PD); 
                false -> leave
            end
    end;
token_eol(_Clause, #clause{kind=Kind})
  when Kind == expr; Kind == pexpr; Kind == hexpr; Kind == compr ->
    leave;
token_eol(Clause, #clause{}) ->
    case ?GRAPH:path(Clause, [{clause, back}]) of
        [] -> noeol;
        [Parent] ->
            case ?GRAPH:path(Parent, [{clause, last}]) of
                [Clause] -> eol;
                _        -> noeol
            end
    end.


                                     
%%% ----------------------------------------------------------------------------
%%% Indentation-related queries

base_indent(Node, #clause{}) -> base_indent_cl(Node); 
base_indent(Node, #expr{}) -> base_indent_ex(Node);
base_indent(_, _) -> "".

base_indent_cl(Cl) ->
    case ?GRAPH:path(Cl, [{clause, back}]) of
        [Expr] ->
            #expr{indent=Ind} = ?GRAPH:data(Expr),
            base_indent_ex(Expr) ++ Ind;
        [] -> ""
    end.

base_indent_ex(Expr) ->
    [Top] = ?GRAPH:path(Expr, [sup]),
    case ?GRAPH:path(Top, [{body, back}]) of
        [Clause] ->
            #clause{indent=Ind} = ?GRAPH:data(Clause),
            base_indent_cl(Clause) ++ Ind;
        [] ->
            case anypath(hd(?GRAPH:path(Expr,[sup])),
                         [{pattern, back}, {guard, back}, {tmout, back}]) of
                [Clause] ->
                    base_indent_cl(Clause);
                [] ->
                    ""
            end
    end.

first_token(Node) ->
    case hd(?NODE_STRUCTURE:node_structure(?GRAPH:data(Node))) of
        {token, _} ->
            hd(?GRAPH:path(Node, [{?LEX:token_link(Node), 1}]));
        {symbol, Link} ->
            first_token(hd(?GRAPH:path(Node, [{Link, 1}])));
        {repeat, _, Link} ->
            first_token(hd(?GRAPH:path(Node, [{Link, 1}])))
    end.


is_last(Par, PD, Child) ->
    Link =
        case lists:last(?NODE_STRUCTURE:node_structure(PD)) of
            {token, _} -> ?LEX:token_link(Par);
            {symbol, L} -> L;
            {repeat, _, L} -> L
        end,
    [Child] == ?GRAPH:path(Par, [{Link, last}]).
            
is_first(Par, PD, Child) ->
    Link =
        case hd(?NODE_STRUCTURE:node_structure(PD)) of
            {token, _} ->  ?LEX:token_link(Par);
            {symbol, L} -> L;
            {repeat, _, L} -> L
        end,
    [Child] == ?GRAPH:path(Par, [{Link, 1}]).

