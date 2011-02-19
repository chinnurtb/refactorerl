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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc This module contains functions that return a query that expects a 
%%% clause semantical node as starting point.
%%%
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(referl_clause).
-vsn("$Rev: 3185 $").

-include("refactorerl.hrl").

%% =============================================================================
%% Exports

%% Queries
-export([type/1, kind/1]).
-export([name/0, patterns/0, pattern/1, guard/0, funcl/0,
         body/0, body/1, exprs/0, expr/1, scope/0, form/0,
         variables/0, variable/1]).
-export([is_same_clause/1]).

%% =============================================================================
%% Clause properties

%% @spec type(node())-> atom()
%% @doc Returns the type of a clause.
type(Clause) -> (?ESG:data(Clause))#clause.type.

%% @spec kind(node())-> atom()
%% @doc Returns the kind of a clause.
kind(Clause) -> (?ESG:data(Clause))#clause.kind.

%% =============================================================================
%% Queries starting from clause nodes

%% @spec name() -> query(#clause{}, #expr{})
%% @doc The result query returns the name expressions of the clause.
name() ->
    [name].

%% @spec patterns() -> query(#clause{}, #expr{})
%% @doc The result query returns the patterns of the clause.
patterns() -> [pattern].

%% @spec pattern(integer()) -> query(#clause{}, #expr{})
%% @doc The result query returns the `I'th pattern of the clause.
pattern(I) ->
    [{pattern,I}].

%% @spec guard() -> query(#clause{}, #expr{})
%% @doc The result query returns the guard expressions of the clause.
guard() -> [guard].

%% @spec body() -> query(#clause{}, #expr{})
%% @doc The result query returns the top-level expressions from the body of the
%% clause.
body() -> [body].

%% @spec body(integer()) -> query(#clause{}, #expr{})
%% @doc The result query returns the `I'th top-level expression from the body 
%% of the clause.
body(I) -> [{body, I}].

%% @spec exprs() -> query(#clause{}, #expr{})
%% @doc The result query returns the top-level expressions of the clause.
exprs() ->
    [visib].

%% @spec expr(integer()) -> query(#clause{}, #expr{})
%% @doc The result query returns the `I'th top level expression of the clause.
expr(I) ->
    [{visib, I}].

%% @spec scope() -> query(#clause{}, #clause{})
%% @doc The result query returns the scope of the clause.
scope() ->
    [scope].

%% @spec funcl() -> query(#clause{}, #clause{})
%% @doc The result query returns the function clause which contains the clause.
funcl()->
    ?Query:seq(scope(), [functx]).

%% @spec form() -> query(#clause{}, #form{})
%% @doc The result query returns the form which contains the clause.
form() ->
    ?Query:seq(funcl(), [{funcl,back}]).

%% @spec variables() -> query(#clause{}, #variable{})
%% @doc The result query returns the variables that are defined in the scope
%% that contains the clause.
variables() ->
    [scope, vardef].

%% @spec variable(string()) -> query(#clause{}, #variable{})
%% @doc The result query returns the variable `VarName' that is defined in the
%% scope of the clause.
variable(VarName) ->
    [scope, {vardef, {name, '==', VarName}}].
%% =============================================================================
%% Clause related functions

%% @spec is_same_clause({#clause{}, #clause{}}) -> bool()
%% @doc  Returns whether the two clauses are the same, disregarding whitespace.
is_same_clause({C1, C2}) ->
    [Type1|Infos1] = clause_data(C1),
    [Type2|Infos2] = clause_data(C2),
    Diffs = [ d ||  {X1, X2} <- lists:zip(Infos1, Infos2),
                    length(X1) /= length(X2) orelse
                    not lists:all(fun ?Expr:is_same_expr/1, lists:zip(X1, X2))],
    Type1 == Type2 andalso Diffs == [].


%% Collects data about the clause.
clause_data(Clause) ->
    Type       = type(Clause),
    Patterns   = ?Query:exec([Clause], patterns()),
    Guards     = ?Query:exec([Clause], guard()),
    Body       = ?Query:exec([Clause], body()),
    [Type, Patterns, Guards, Body].
