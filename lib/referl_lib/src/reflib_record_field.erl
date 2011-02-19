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

%%% @doc Record field properties and record field based queries

-module(reflib_record_field).
-vsn("$Rev: 4100 $"). % for emacs"

%% =============================================================================
%% Exports

-export([file/0, recorddef/0, name/1, form/0, references/0]).

-include("lib.hrl").

%% =============================================================================


%% @spec name(node(#field{})) -> atom()
%% @doc The name of the field object
name(Field) ->
    (?Graph:data(Field))#field.name.

%% @spec file() -> query(#field{}, #file{})
%% @doc The result query returns the file that defines the record
file() ->
    [{field, back}, {record, back}].

%% @spec form() -> query(#field{}, #form{})
%% @doc The result query returns the form that defines the record
form() ->
    [{field, back}, {recdef, back}].

%% @spec references() -> query(#field{}, #expr{})
%% @doc The result query returns the references to the record field
references() ->
    [{fieldref, back}].

%% @spec recorddef() -> query(#field{}, #form{})
%% @doc The result query returns the defining record of the record field
recorddef() ->
    [{field, back}].
