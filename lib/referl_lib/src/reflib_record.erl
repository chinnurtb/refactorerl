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

%%% @doc Record properties and record based queries

-module(reflib_record).
-vsn("$Rev: 5455 $ ").

%% =============================================================================
%% Exports

-export([find/1, fields/0, field/1, name/1, file/0, form/0, references/0]).

-include("lib.hrl").

%% =============================================================================


%% @spec find(atom()) -> query(#file{}, #record{})
%% @doc The result query returns the record with name `Name'
find(Name) ->
    [{record, {name, '==', Name}}].


%% @spec fields() -> query(#record{}, #field{})
%% @doc The result query returns the fields of the record
fields() ->
    [field].


%% @spec field(atom()) -> query(#record{}, #field{})
%% @doc The result query returns the record's field `Field'
field(Field) ->
    [{field, {name, '==', Field}}].


%% @spec name(node(#record{})) -> atom()
%% @doc The name of the record object
name(Record) ->
    (?Graph:data(Record))#record.name.

%% @spec file() -> query(#record{}, #file{})
%% @doc The result query returns the file that defines the record
file() ->
    [{record, back}].

%% @spec form() -> query(#record{}, #form{})
%% @doc The result query returns the form that defines the record
form() ->
    [{recdef, back}].

%% @spec references() -> query(#record{}, #expression{})
%% @doc The result query returns every reference to the record
references() ->
    [{recref, back}].
