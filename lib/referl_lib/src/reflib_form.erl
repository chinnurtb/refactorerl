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

%%% @doc This module contains functions that return a query that expects a form
%%% semantical node as starting point.
%%%
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(reflib_form).
-vsn("$Rev: 4524 $ ").

%% =============================================================================
%% Exports

-export([type/1]).

%% Queries
-export([clauses/0, clause/1, file/0]).
-export([module/0, func/0, record/0, exprs/0, expr/1, deep_exprs/0, macros/0,
         records/0]).

-include("lib.hrl").

%% @spec type(node()) -> atom()
%% @doc Returns the type of a form as an atom. The returned atom is
%% the type (e.g. export, import) by module attributes and `macro' by
%% macro definition forms.
type(Form) ->
    #form{type=Type, tag=Tag} = ?Graph:data(Form),
    case Type of
        define -> macro;
        attrib -> Tag;
        _      -> Type
    end.

%%% ============================================================================
%%% Form related queries

%% @spec module() -> query(#form{}, #module{})
%% @doc The result query returns the module node which contains the form.
module() ->
    [{form, back}, moddef].

%% @spec func() -> query(#form{}, #func{})
%% @doc The result query returns the function defined by the form.
func() ->
    [fundef].

%% @spec record() -> query(#form{}, #record{})
%% @doc The result query returns the record defined by the form.
record() ->
    [recdef].

%% @spec clauses() -> query(#form{}, #clause{})
%% @doc The result query returns the clauses of the form.
clauses() ->
    [funcl].

%% @spec clause(integer()) -> query(#form{}, #clause{})
%% @doc The result query returns the clause of the form with the given `I'
%% index.
clause(I) ->
    [{funcl, I}].

%% @spec file() -> query(#form{}, #file{})
%% @doc The result query returns the file node which contains the form.
file() -> [{form, back}].

%% @spec macros() -> query(#form{}, #form{})
%% @doc The result query returns the macro definitions of the macros used in
%% the form. In the result list any macro definition may occure in multiple
%% number. It is applicable for any form, except the macro definition forms
%% (in this case the result always will be an empty list).
macros() ->
    ?Query:any(
       ?Query:seq([
                   ?Form:clauses(),
                   ?Query:all(?Clause:exprs(), ?Clause:name()),
                   ?Expr:deep_sub(),
                   [{elex,1},{llex,1},{mref,1}]]),
       ?Query:seq([
                   exprs(),
                   ?Expr:deep_sub(),
                   [{elex,1},{llex,1},{mref,1}]])).

%% @spec records() -> query(#form{}, #record{})
%% @doc The result query returns the record definitions of the records used in
%% the form. In the result list any record definition may occure in multiple
%% number.
records() ->
    ?Query:any(
       ?Query:seq([
                   ?Form:clauses(),
                   ?Clause:exprs(),
                   ?Expr:records()]),
       ?Query:seq([
                   exprs(),
                   ?Expr:deep_sub(),
                   ?Expr:records()])).


%% @spec exprs() -> query(#form{}, #expr{})
%% @doc The result query returns subexpressions directly under the form
%% (mainly useful in attribute forms)
exprs() -> [eattr].

%% @spec expr(integer()) -> query(#form{}, #expr{})
%% @doc The result query returns the `I'th subexpression directly under the form
%% (mainly useful in attribute forms)
expr(I) -> [{eattr, I}].

%% @spec deep_exprs() -> query(#form{}, #expr{})
%% @doc The result query returns subexpressions under the form
%%      (either directly or by subexpressions).
deep_exprs() ->
    ?Query:all( exprs(),
                ?Query:seq([clauses(), ?Clause:exprs(), ?Expr:deep_sub()])).
