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

%%% ============================================================================
%%% Module information

%%% @doc Rename record field
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(referl_tr_rename_recfield).
-vsn("$Rev: 1973 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/4, do/5]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

%% State record
-record(state, {filepath, file, recordname,
                pos = undefined, oldname = undefined,
                newname, record, field, refs}).

%%% ============================================================================
%%% Errors

throw_not_found() ->
    throw("record field not found").

msg_name_collision() ->
    "name collision with an existing field name".

%%% ============================================================================
%%% Interface

%% @spec do(pos, string(), integer(), atom()) -> ok
%% @doc Renames the specified record field to a new name.
%% The field is determined by a file path and a position in the file.
do(pos, FilePath, Pos, NewName) ->
    ?TRANSFORM:do(?MODULE, {pos, FilePath, Pos, NewName}).

%% @spec do(name, string(), atom(), atom(), atom()) -> ok
%% @doc Renames the specified record field to a new name.
%% The field is determined by a file path, a record name
%% and the old name of the field.
do(name, FilePath, RecordName, OldName, NewName) ->
    ?TRANSFORM:do(?MODULE, {name, FilePath, RecordName, OldName, NewName}).

%%% ============================================================================
%%% Callbacks

%% @private
init({pos, FilePath, Pos, NewName}) ->
    #state{filepath = FilePath, pos = Pos, newname = NewName};

init({name, FilePath, RecordName, OldName, NewName}) ->
    #state{filepath = FilePath, recordname = RecordName,
           oldname = OldName, newname = NewName}.

%% @private
steps() ->
    [fun new_name_to_atom/1,
     fun check_file_field/1,
     fun check_name_conflicts/1,
     fun query_field_refs/1].

%% @private
transform(#state{file=_File, refs=Refs, newname=NewName, field=Field}) ->
    Update =
        fun (D=#expr{kind=atom}) ->
                D#expr{value=NewName};
            (D=#lex{data=Data}) ->
                D#lex{data=Data#token{value=NewName, text=?MISC:to_list(NewName)}};
            (_) ->
                throw("Unknown reference")
        end,
    [?ESG:update(Node, Update(?ESG:data(Node))) || Node <- Refs],
    ?ESG:update(Field, (?ESG:data(Field))#field{name=NewName}),
    {changed_files(Refs), ok}.

changed_files(Refs) ->
    lists:usort([?SEMINF:parent_file(N) || N <- Refs]).

%%% ============================================================================
%%% Implementation

new_name_to_atom(St = #state{newname=NN}) ->
    St#state{newname=?MISC:to_atom(NN)}.

check_file_field(St = #state{oldname = undefined, filepath = FilePath, pos = Pos}) ->
    File = ?SYNTAX:get_file(FilePath),
    Token = ?LEX:get_token(File, Pos),
    [{_, Node}] = ?ESG:parent(Token),
    {Record, Field} = 
        case ?ESG:path(Node, [fieldref]) ++ ?ESG:path(Node, [fielddef]) of
            [Fld] ->
                [Rec] = ?ESG:path(Fld, [{field, back}]),
                {Rec, Fld};
            [] ->
                throw_not_found()
        end,    
    St#state{file = File, record = Record, field = Field};

check_file_field(St = #state{pos = undefined, filepath = FilePath,
                              oldname = OldName, recordname = RecordName}) ->
    File = ?SYNTAX:get_file(FilePath),
    RecNode = 
        case ?ESG:path(File, referl_anal_rec:record(RecordName)) of
            [R] ->
                R;
            [] ->
                throw_not_found()
        end,
    Field = 
        case ?ESG:path(RecNode, referl_anal_rec:field(OldName)) of
            [F] ->
                F;
            [] ->
                throw_not_found()
        end,
    St#state{file = File, record = RecNode, field = Field}.

check_name_conflicts(#state{record = Record, field = Field, newname = NewName}) ->
    Names = [Name || F <- ?ESG:path(Record, [field]) -- [Field],
                     #field{name=Name} <- [?ESG:data(F)]],
    ?MISC:error_on_difference(lists:member(NewName, Names), false, msg_name_collision()).

query_field_refs(St = #state{field=Field}) ->
    Refs = ?ESG:path(Field, [{fieldref, back}]) ++ ?ESG:path(Field, [{fielddef, back}]),
    Tokens =
        lists:flatmap(
          fun(Expr) -> ?ESG:path(Expr, [elex]) end,
          Refs),
    St#state{refs = Refs ++ Tokens}.
