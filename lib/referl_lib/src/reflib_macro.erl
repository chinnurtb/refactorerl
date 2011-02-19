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
-vsn("$Rev: 4957 $ ").

%% =============================================================================
%% Exports

%% Properties
-export([name/1]).
-export([refs/1, check/1]).

%% Queries
-export([file/0, find/1, macros/0, records/0, references/0]).
-include("lib.hrl").

-export([check_macros/1, check_macros/2, update_macro/3, usages/1]).

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

%% @spec check([node()]) -> bool()
%% @doc checks a list of virtual tokens if the macro they are originated
%% from is used in the same role in every substitution
%% @TODO better name
check([Virtual | Virtuals]) ->
    [Token] = ?Query:exec(Virtual, [orig]),
    Usages = (lists:usort(usages({?Token:type(Token), Token}))),
    case length(Usages) of
        1 -> check(Virtuals);
        _ -> false
    end;
check([]) -> true.

%% @spec usages({atom, node()}) -> [node()]
%% @doc returns the list of semantic nodes the given Token associated with
%% @TODO the rest of the possible token types, better name
usages({TokenType, Token}) ->
    if 
        (TokenType == '(' orelse TokenType == ')') ->
                                                   Type = parenthesis;
        true                                       -> 
                                                   Type = TokenType
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

   % Exprs = ?Query:exec(Virtuals, [{elex, back}]),
   % Erefs = [usages(Type, Expr, elex) || Expr <- Exprs],

   % Texprs = ?Query:exec(Virtuals, [{tlex, back}]),
   % Trefs = [usages(Type, Texpr, tlex) || Texpr <- Texprs],

   % Forms = ?Query:exec(Virtuals, [{flex, back}]),
   % Frefs = [usages(Type, Fexpr, flex) || Fexpr <- Forms],
   % Res = Erefs ++ Trefs ++ Frefs,
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
                      _      -> Expr
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
          [] ->     [{Expr, Index}];
          [Node] -> [{Node, Index}]
     end;
usages(_, Expr, _) -> [Expr].

%% @spec refs(node()) -> [[node()]]
%% @doc Return every expression came from expanding the macro
refs(Macro) ->
    Substs = ?Query:exec(Macro, [{mref, back}]),
    F = fun(Subst) -> ?Query:exec(Subst, ?Query:seq([{llex, back}], 
                                ?Query:any([[{elex, back}], [{clex, back}],
                                            [{flex, back}], [{tlex, bacl}]])))
        end,
    [F(X) || X <- Substs].

check_macros(Updates, Path) ->
    Virtuals = [Token || 
           Token <- lists:concat([?Query:exec(Exp, [Path]) || Exp <- Updates]),
           (?Graph:data(Token))#lex.data == virtual],
    ?Check(?Macro:check(Virtuals), ?RefError(mac_error, [])).

check_macros(Updates) ->

    Virts = fun(ExpList, Path) -> 
            [Token || Token <- lists:concat([?Query:exec(Exp, [Path]) || Exp <- ExpList]),
                                                (?Graph:data(Token))#lex.data == virtual]
        end,
    Virtuals = lists:concat([Virts(ExpList, Path) || {ExpList, Path} <- Updates]),

%    Virtuals = [Token || 
%           Token <- lists:concat([?Query:exec(Exp, [Path]) || {Exp, Path} <- Updates]),
%           (?Graph:data(Token))#lex.data == virtual],
    ?Check(?Macro:check(Virtuals), ?RefError(mac_error, [])).

update_macro(Node, Path, NewName) ->
    [Token] = ?Query:exec(Node, [Path]),
    case (?Graph:data(Token))#lex.data of
          virtual ->
                N = hd(?Query:exec(Node, [Path, orig])),
                New = [NewName],
                ND=#lex{data=NND} = ?ESG:data(N),
                NewNode = ND#lex{data=NND#token{value=New,text=New}},
                ?ESG:update(N, NewNode),
                [F] = ?Query:exec(N, [{llex, back}, {flex, back}]),
                ?Transform:touch(F);
          _       ->
                ?Syn:replace(Node, Path, [NewName]),
                ?Transform:touch(Node)
    end.
