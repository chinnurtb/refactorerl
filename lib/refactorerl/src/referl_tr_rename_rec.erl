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

%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%%
%%% @doc Rename record

-module(referl_tr_rename_rec).
-vsn("$Rev: 1973 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/4]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

%% State record
-record(state, {filepath, file, record,
                pos = undefined, oldname = undefined,
                newname, refs}).

%%% ============================================================================
%%% Errors

throw_record_not_found() ->
    throw("record not found").

msg_name_collision() ->
    "name collision with an existing record".

%%% ============================================================================
%%% Interface

%% @spec do(pos | name, string(), integer() | atom(), atom()) -> ok
%% @doc Renames the specified record to a new name.
%% The record can be specified by a position in the file or by its old name.
do(pos, FilePath, Pos, NewName) ->
    ?TRANSFORM:do(?MODULE, {pos, FilePath, Pos, NewName});
do(name, FilePath, OldName, NewName) ->
    ?TRANSFORM:do(?MODULE, {name, FilePath, OldName, NewName}).

%%% ============================================================================
%%% Callbacks

%% @private
init({pos, FilePath, Pos, NewName}) ->
    #state{filepath = FilePath, pos = Pos, newname = NewName};

init({name, FilePath, OldName, NewName}) ->
    #state{filepath = FilePath, oldname = OldName, newname = NewName}.

%% @private
steps() ->
    [
     fun new_name_to_atom/1,
     fun check_file_record/1,
     fun check_name_conflicts/1,
     fun query_rec_refs/1
    ].

%% @private
transform(#state{file = _File, refs = Refs, newname = NewName, record=Record}) ->
    Update =
        fun (D=#expr{kind=atom}) ->
                D#expr{value=NewName};
            (D=#lex{data=Data}) ->
                D#lex{data=Data#token{value=NewName, text=?MISC:to_list(NewName)}};
            (_) ->
                throw("Unknown reference")
        end,
    [?ESG:update(Node, Update(?ESG:data(Node))) || Node <- Refs],
    ?ESG:update(Record, (?ESG:data(Record))#record{name=NewName}),
    {changed_files(Refs), ok}.

changed_files(Refs) ->
    lists:usort([?SEMINF:parent_file(N) || N <- Refs]).

    

%%% ============================================================================
%%% Implementation

new_name_to_atom(St = #state{newname=NN}) ->
    St#state{newname=?MISC:to_atom(NN)}.

check_file_record(St = #state{oldname = undefined,
                              filepath = FilePath,
                              pos = Pos}) ->
    File = ?SYNTAX:get_file(FilePath),
    Token = ?LEX:get_token(File, Pos),
    [{_, Node}] = ?ESG:parent(Token),
    [RecNode] =
        case ?ESG:data(Node) of
            #expr{} ->
                referred_record(Node);
            #form{type=attrib, tag=record} ->
                ?ESG:path(Node, [recdef]);
            _ ->
                throw_record_not_found()
        end,
    St#state{file = File, record = RecNode};

check_file_record(St = #state{pos = undefined,
                              filepath = FilePath,
                              oldname = OldName}) ->
    File = ?SYNTAX:get_file(FilePath),
    [RecNode] = ?ESG:path(File, referl_anal_rec:record(OldName)),
    St#state{file = File, record = RecNode}.

referred_record(Node) ->
    case ?ESG:path(Node, [sup, {attr, back}, recdef]) ++
        ?ESG:path(Node, [{sub, back}, recref]) of
        [] -> throw_record_not_found();
        Rec -> Rec
    end.

check_name_conflicts(#state{file = File, newname = NewName}) ->
    Names = busy_recordnames(File),
    ?MISC:error_on_difference(lists:member(NewName, Names), false, msg_name_collision()).

busy_recordnames(File) ->
    Files = lists:usort(?ESG:path(File, [incl]) ++ ?ESG:path(File, [{incl, back}])),
    Names =
        lists:foldl(
          fun(F, Acc) ->
                  {Ns, _} =
                      lists:unzip(?LEX:existing_records_with_source(F)),
                  Acc ++ Ns
          end,
          [],
          Files),
    lists:usort(Names).

query_rec_refs(St = #state{record = Record}) ->
    Refs = [{Node, Data} || Node <- ?ESG:path(Record, [{recref, back}]),
                            Data <- [?ESG:data(Node)]],
    Exprs =
        lists:flatmap(
          fun({Node, #expr{kind=Kind}})
             when Kind == record_expr orelse Kind == record_index ->
                  ?ESG:path(Node, [{sub, 1}]);
             ({Node, #expr{kind=Kind}})
             when Kind == record_update orelse Kind == record_access ->
                  ?ESG:path(Node, [{sub, 2}])
          end,
          Refs) ++ ?ESG:path(Record, [{recdef, back}, {attr, 1}]),
    Tokens =
        lists:flatmap(
          fun(Expr) -> ?ESG:path(Expr, [elex]) end,
          Exprs),
    St#state{refs = Exprs ++ Tokens}.
