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

%%% @doc
%%% This refactoring renames records in modules or header files. After the
%%% transformation, the old name will be replaced by the
%%% new name in the record definition and in every reference to the given
%%% record (e.g.\ record field access or field update expressions). The
%%% condition of the renaming is that there is no name conflict with another
%%% record in the file (which contains the given record), in its
%%% includes, or where it is included (the latter is only possible when we
%%% are renaming in a header file).
%%%
%%% Parameters
%%% <ul>
%%%   <li>The record to be renamed can be specified by:
%%%     <ul>
%%%       <li>The containing file and a position that points to the record
%%%       definition or to a usage of the record (used in Emacs).</li>
%%%       <li>The containing file and the old record name (used in command
%%%       line).</li>
%%%     </ul>
%%%   </li>
%%%   <li>The new name of the record as a string or as an atom.</li>
%%% </ul>
%%%
%%% Side conditions
%%% <ul>
%%%   <li>There must be no record with the new name
%%%     <ul>
%%%       <li>in the file that contains the record,</li>
%%%       <li>in files, which are included by this file,</li>
%%%       <li>in files, which include this file.</li>
%%%     </ul>
%%%   </li>
%%% </ul>
%%%
%%% Transformation steps and compensations
%%% <ol>
%%%   <li>The record name is changed to the new name in the definition of
%%%   the record and in every record expression that refers the record.</li>
%%% </ol>
%%%
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Lilla Hajos <lya@elte.hu>


-module(referl_tr_rename_rec).
-vsn("$Rev: 2599 $").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1]).

%%% ============================================================================
%%% Callbacks

prepare(Args) ->
    NewName = ?Args:name(Args),
    Record  = ?Args:record(Args),
    check_name_conflicts(Record, NewName),
    Refs    = query_rec_refs(Record),
    fun() ->
        [ begin
            ?Syn:replace(Expr, {elex,1}, [io_lib:write_atom(NewName)]),
            ?Transform:touch(Expr)
          end || Expr <- Refs ]
    end.

%%% ============================================================================
%%% Implementation

check_name_conflicts(Record, NewName) ->
    File  = ?Query:exec(Record,?Rec:file()),
    Files = lists:usort(?Query:exec(File, {all, [incl], [{incl, back}]})),
    Names = [?Rec:name(Rec) || Rec <- ?Query:exec(Files,?File:records())],
    ?Check( not lists:member(NewName, Names),
            ?RefError(rec_exists,io_lib:write_atom(NewName))).

query_rec_refs(Record) ->
    ?Query:exec(Record, [{recdef, back}, {attr, 1}]) ++
    lists:flatmap(
      fun(Node) ->
          Kind = ?Expr:kind(Node),
          if
            Kind == record_expr orelse Kind == record_index ->
              ?Query:exec(Node, [{sub, 1}]);
            Kind == record_update orelse Kind == record_access ->
              ?Query:exec(Node, [{sub, 2}])
          end
      end,
      ?Query:exec(Record,[{recref, back}])).
