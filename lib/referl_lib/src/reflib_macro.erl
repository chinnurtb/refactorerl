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

%%% @doc This module implements functions that are related to
%%% macros. Macros are represented with the form that defines them.

-module(reflib_macro).
-vsn("$Rev: 5698 $ ").

%% =============================================================================
%% Exports

%% Properties
-export([name/1]).
-export([refs/1, has_single_role/1]).

%% Queries
-export([file/0, find/1, macros/0, records/0, references/0, virtual_macro/0]).
-include("lib.hrl").

-export([check_single_usage/1, check_single_usage/2, usages/1]).
-export([update_macro/3, update_virtual_token/2]).
-export([is_virtual/2]).
-export([macro_contains_funapp/1]).
-export([inline_single_virtuals/2]).


%% =============================================================================
%% Properties

%% @spec name(node(#form{})) -> string()
%% @doc Returns the name of the macro.
name(Macro) ->
    (?Graph:data(Macro))#form.tag.


%% =============================================================================
%% Queries

%% @spec file() -> query(#form{}, #file{})
%% @doc The result query returns the file that defines the macro.
file() ->
    ?Form:file().

%% @spec find(string()) -> query(#file{}, #form{})
%% @doc The result query returns the macro with name `Name'
find(Name) ->
    [{form,{{type, '==', macro}, 'and',
            {tag,  '==', Name}}}].

%% @spec macros() -> query(#form{}, #form{})
%% @doc The result query returns all macros used in the body of the macro.
macros() ->
    fun(Macro) ->
            referred_macros(Macro, up)
    end.

%% @spec records() -> query(#form{}, #record{})
%% @doc The result query returns all records used in the macro.
records() ->
    fun(Macro) ->
            AllMacros = [Macro | ?Query:exec(Macro, macros())],
            Substs = lists:flatten([?Query:exec(M, [{mref, back}])
                                    || M <- AllMacros]),
            ?MISC:flatsort([?Query:exec(Subst, [{llex, back},
                                                {elex, back}, recref])
                            || Subst <- Substs])
    end.

%% @spec virtual_macro() -> query(#token{}, #form{})
%% @doc The result query returns the macro form, the parameter virtual token's
%% origin belongs to.
virtual_macro() ->
    [{llex}, {mref}].

%% @spec references() -> query(#form{}, #form{})
%% @doc The result query returns all macros that are using the argument macro.
references() ->
    fun(Macro) ->
            referred_macros(Macro, down)
    end.

referred_macros(Macro, Direction) ->
    case ?Query:exec(Macro, [{mref, back}]) of
        Substs when is_list(Substs) andalso Substs =/= [] ->
            SubstsWithRecur = ?MISC:flatsort([substs_recur(Subst, Direction)
                                              || Subst <- Substs]),
            lists:usort([?Query:exec1(Subst, [mref], subst_without_mref)
                         || Subst <- SubstsWithRecur]);
        _ ->
            []
    end.

%% @private
%% @spec substs_recur(node(), up | down) -> [node()]
%% @doc Returns all macro substitutions from a specified
%% substitution, up or down in the syntax tree.
substs_recur(Subst, Direction) ->
    Query = case Direction of
                up -> [{llex, back}, {llex, back}];
                down -> [llex, llex]
            end,
    News = [New || New <- ?Query:exec(Subst, Query),
                   #lex{type=subst} <- [?Graph:data(New)]],
    case News of
        [] -> [];
        _ ->  lists:flatten(News ++ [substs_recur(M, Direction) || M <- News])
    end.

%% @spec has_single_role([node()]) -> bool()
%% @doc Checks a list of virtual tokens if the macro they are originated
%% from is used in the same role in every substitution
has_single_role(Virtuals) when is_list(Virtuals) ->
    lists:all(fun(V) -> has_single_role(V) end, Virtuals);
has_single_role(Virtual) ->
    [Token] = ?Query:exec(Virtual, [orig]),
    Usages = (lists:usort(usages({?Token:type(Token), Token}))),
    length(Usages) == 1.

%% @spec usages({atom, node()}) -> [node()]
%% @doc Returns the list of semantic nodes `Token' is associated with.
%%      If it is unidentifiable, `Token' itself is returned.
%% @TODO the rest of the possible token types, better name
usages({TokenType, Token}) ->
    if
        (TokenType == '(' orelse TokenType == ')') -> Type = parenthesis;
        true                                       -> Type = TokenType
    end,
    Virtuals = ?Query:exec(Token, [{orig, back}]),
    GetNode = fun(Virtual) ->
                    [Node] = ?Query:exec(Virtual, ?Query:any([
                                      [{elex, back}], [{flex, back}],
                                      [{clex, back}], [{tlex, back}]
                             ])),
                    Tag = case ?ESG:data(Node) of
                               #form{}   -> flex;
                               #clause{} -> clex;
                               #expr{}   -> elex;
                               #typexp{} -> tlex
                          end,
                    usages(Type, Node, {Tag, ?ESG:index(Node, Tag, Virtual)})
               end,
    Res = [GetNode(V) || V <- Virtuals],

    lists:map(fun([])-> [Token]; (X) -> X end, Res);
usages(X) -> [X].

usages(atom, Expr, {Link, _}) ->
    case Link of
        elex ->
            ?Query:exec(Expr, ?Query:any([
                ?Expr:module(),
                [{name, back}, {funcl, back}, fundef], %% function def   -- elex
                [{esub, back}, funlref], %% function application or impl fun expr  -- elex
                [{esub, back}, {esub, back}, funeref], %% function application with module qualifier -- elex
                [{esub, back}, {esub, back}, funlref], %% function application with module qualifier -- elex
                ?Expr:record(),
                ?Expr:field()
            ]));
        tlex ->
              ?Query:exec(Expr, [fielddef]);
        flex ->
              ?Query:exec(Expr, ?Query:any([
                    [{form, back}, moddef],  %% module def    -- flex
                    [recdef] %% record def -- flex
              ]))
    end;
usages(variable, Expr, {elex, _}) ->
          ?Query:exec(Expr, ?Query:any([
                    [varbind],
                    [varref]
                  ])
          );
usages(integer, Expr, {elex, _}) ->
          ?Query:exec(Expr, ?Query:any([
                    [{esub, back}, funlref],
                    [{esub, back}, funeref]
                  ])
          );
usages(parenthesis, Expr, {Link, _}) ->
    case Link of
        elex ->
              ?Query:exec(Expr, ?Query:any([
                    [{esub, back}, funlref],
                    [{esub, back}, funeref],
                    []
              ]));
        clex ->
              ?Query:exec(Expr, ?Query:seq(?Clause:form(), ?Form:func()));
        flex ->
              case ?Form:type(Expr) of
                  module ->
                         ?Query:exec(Expr, ?Form:module());
                  record ->
                         ?Query:exec(Expr, ?Form:record());
                  _     -> Expr
              end
    end;
usages(',', Expr, {LinkTag, Index}) ->
    Res = case LinkTag of
                elex ->
                      ?Query:exec(Expr, ?Query:any([
                            [{esub, back}, funlref],
                            [{esub, back}, funeref],
                            [{esub, back}, recref],
                            []
                      ]));
                clex ->
                      ?Query:exec(Expr, ?Query:any([
                            ?Query:seq(?Clause:form(), ?Form:func()),
                            []
                      ]));
                flex ->
                      ?Query:exec(Expr, ?Query:any([
                            [recdef],
                            []
                      ]))
            end,
    case Res of
        [] ->    [{Expr, Index}];
        [Node] -> [{Node, Index}]
    end;
%usages(_, Expr, _) -> [Expr].
usages(_, _, _) -> [].

%% @spec refs(node()) -> [[node()]]
%% @doc Return every expression came from expanding the macro
refs(Macro) ->
    Substs = ?Query:exec(Macro, [{mref, back}]),
    F = fun(Subst) -> ?Query:exec(Subst, ?Query:seq([{llex, back}],
                                ?Query:any([[{elex, back}], [{clex, back}],
                                            [{flex, back}], [{tlex, back}]])))
        end,
    [F(X) || X <- Substs].


check_single_usage(Updates, Path) ->
    check_single_usage([{Updates, Path}]).

%% Throws `mac_error' if one of the updates (expression list-path pairs)
%% contain macros whose virtual tokens have different roles.
check_single_usage(Updates) ->
    Virtuals = [Token   ||  {ExpList, Path} <- Updates,
                            Token <- ?Query:exec(ExpList, Path),
                            (?Graph:data(Token))#lex.data == virtual],
    ?Check(has_single_role(Virtuals), ?RefError(mac_error, Virtuals)).

is_virtual(Node, Path) ->
    [Token] = ?Query:exec(Node, [Path]),
    case ?Graph:data(Token) of
        #lex{data=virtual} -> true;
        _                  -> false
    end.


%% `NewName' :: string(), `Path' :: {atom(), integer()} | atom()
%% If the token on the specified path is non-preproc, simply replaces it.
%% If the token comes from a macro substitution, see `update_virtual_token'.
update_macro(Node, Path, NewName) ->
    case ?Query:exec(Node, [Path]) of
        [Token] ->
            case ?Graph:data(Token) of
                #lex{data=virtual} ->
                    case Path of
                        {Lex, _Idx} -> ok;
                        Lex when is_atom(Lex) -> ok
                    end,
                    update_virtual_token(Token, NewName),
                    update_expr(Token, NewName, Lex),
                    ?Transform:touch(Token);
                _ ->
                    ?Syn:replace(Node, Path, [NewName]),
                    ?Transform:touch(Node)
            end;
        _ ->
            ok
    end.

%% Updates the expression that is built upon the virtual token.
%% todo There may be other types of ESG nodes that require a textual
%%      value instead of an atomic one.
update_expr(Token, NewNameStr, Lex) ->
    [Expr]                  = ?Query:exec(Token, [{Lex, back}]),
    Data = ?ESG:data(Expr),
    case Data of
        #expr{type=Type} ->
            Value =
                case Type of
                    variable -> NewNameStr;
                    _        -> list_to_atom(NewNameStr)
                end,
            ?ESG:update(Expr, Data#expr{value=Value});
        #typexp{} ->
            ?ESG:update(Expr, Data#typexp{tag=list_to_atom(NewNameStr)})
    end.


%% Updates the token that originates the virtual one.
update_virtual_token(Virtual, NewValue) ->
    [N]               = ?Query:exec(Virtual, [orig]),
    New               = [NewValue],
    ND=#lex{data=NND} = ?ESG:data(N),
    NewNode           = ND#lex{data=NND#token{text=New}},
    ?ESG:update(N, NewNode),
    [F] = ?Query:exec(N, [{llex, back}, {flex, back}]),
    ?Transform:touch(F).

%% @spec macro_contains_funapp(node()) -> [token()]
%% @doc Retuns the list of function application name virtual tokens
%% that come from applications of the function `FunNode'.
macro_contains_funapp(FunNode) ->
    FunApps = ?Query:exec(FunNode, ?Fun:applications()),
    [Token ||
       [Token] <- [?Query:exec(AppNode, [{esub, 1}, {elex, 1}]) ||
                     AppNode <- FunApps],
       (?Graph:data(Token))#lex.data == virtual].



%% Inlines those expressions that come from a macro substitution
%% and comprise exactly one token.
inline_single_virtuals(Exprs, Link) ->
    [inline_single_virtual(Expr, Link)
            ||  Expr <- Exprs,
                expr_should_be_inlined(Expr, Link)].

expr_should_be_inlined(Expr, Link) ->
    Virtuals = [Token   ||  Token <- ?Query:exec(Expr, [Link]),
                            (?Graph:data(Token))#lex.data == virtual],
    not has_single_role(Virtuals).


%% Inlines a substitution that produces exactly one virtual token.
inline_single_virtual(Parent, Lex) ->
    case [TIdx || TIdx = {Token, _} <- ?MISC:index_list(?Graph:path(Parent, [Lex])),
                  #lex{data=virtual} <- [?Graph:data(Token)]] of
        [{Token, Idx}] ->
            inline_single_virtual(Parent, Token, Idx, Lex);
        _ ->
            ok
    end.

inline_single_virtual(Parent, Token, Idx, Lex) ->
    [Orig]  = get_path_and_rmlink(Token, orig),
    [Subst] = get_path_and_rmlink(Token, llex),
    _       = get_path_and_rmlink(Subst, mref),
    SubstLs = get_path_and_rmlink(Subst, llex),
    Data    = ?Graph:data(Orig),
    NewT    = ?Graph:create(Data),
    [?Graph:delete(SubstL) || SubstL <- SubstLs],
    ?Graph:delete(Subst),
    ?Graph:rmlink(Parent, Lex, Token),
    ?Graph:mklink(Parent, {Lex, Idx}, NewT).

get_path_and_rmlink(Node, Link) ->
    Nodes = ?Graph:path(Node, [Link]),
    [?Graph:rmlink(Node, Link, ToNode) || ToNode <- Nodes],
    Nodes.
