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

%%% ============================================================================
%%% Module information

%%% @doc This refactoring renames record fields. After the
%%% transformation, the old name will be replaced by the new name in
%%% the record definition and in every reference to the given record
%%% field (e.g.\ record field access or field update expressions). The
%%% condition of the renaming is that there is no name conflict with
%%% another field in the record.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li>The record field to be renamed
%%%       (see {@link referl_args:record_field/1}).</li>
%%%   <li>The new name of the field
%%%       (see {@link referl_args:name/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>There must be no field with the new name in the record</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>The field name is changed to the new name in the definition
%%%   of the record and in every record expression that refers the
%%%   record field.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(referl_tr_rename_recfield).
-vsn("$Rev: 3185 $").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1, error_text/2]).


%%% ============================================================================
%%% Errors

%% @private
error_text(name_collision, []) ->
    "Name collision with an existing field name".


%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->

    NewName = ?Args:name(Args),
    Field   = ?Args:record_field(Args),
    Record  = ?Query:exec(Field, [{field, back}]),

    %% Checking name collisions

    Names = [Name || F <- ?Query:exec(Record, [field]) -- [Field],
                     #field{name=Name} <- [?ESG:data(F)]],
    ?Check(not lists:member(NewName, Names), ?LocalErr0r(name_collision)),

    %% Collecting references

    Refs    =
        ?Query:exec(Field, [{fieldref, back}]) ++
        ?Query:exec(Field, [{fielddef, back}]),

    fun() ->
        [?Syn:replace(Expr, {elex,1}, [io_lib:write_atom(NewName)]) ||
                Expr <- Refs],
        [?Transform:touch(Node) || Node <- Refs]
    end.
