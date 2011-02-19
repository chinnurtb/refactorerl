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

%%% @doc This refactoring renames records in modules or header
%%% files. After the transformation, the old name will be replaced by
%%% the new name in the record definition and in every reference to
%%% the given record (e.g.\ record field access or field update
%%% expressions). The condition of the renaming is that there is no
%%% name conflict with another record in the file (which contains the
%%% given record), in its includes, or where it is included (the
%%% latter is only possible when we are renaming in a header file).
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li>The record to be renamed
%%%       (see {@link reflib_args:record/1}).</li>
%%%   <li>The new name of the record
%%%       (see {@link reflib_args:name/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
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
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>The record name is changed to the new name in the definition of
%%%   the record and in every record expression that refers the record.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Lilla Hajos <lya@elte.hu>


-module(reftr_rename_rec).
-vsn("$Rev: 5653 $"). % for emacs"

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks
error_text(rec_exists, NewName) ->
   ["Record ", io_lib:write_atom(NewName), " already exists"].

%% @private
prepare(Args) ->
    Record  = ?Args:record(Args),
    [File] = ?Query:exec(Record, ?Rec:file()),
    FilePath = ?File:path(File),

    Files = lists:usort(?Query:exec(Record,
                ?Query:seq(?Rec:file(),
                           ?Query:all(?File:includes(), ?File:included())))),
    Names = [?Rec:name(Rec) || Rec <- ?Query:exec(Files,?File:records())],

    ArgsInfo    = ask_transformation_info(Args, Record),
    NewName  = ?Args:ask(ArgsInfo, name, fun cc_newname/2, fun cc_error/3, Names),

    NameStr = io_lib:write_atom(NewName),
    {Def, Refs} = query_rec_refs(Record),

    [ ?Transform:touch(Expr) || Expr <- [Def | Refs]],
    [fun() ->
        ?Macro:inline_single_virtuals(Refs, elex),
        [?Macro:update_macro(Expr, {elex, 2}, NameStr) || Expr <- Refs],
        ?ESG:update(Def, (?ESG:data(Def))#form{tag=NewName})
    end,
    fun(_)->
            ?Query:exec(?Query:seq([?File:find(FilePath),
                                    ?Rec:find(NewName)]))
    end].

ask_transformation_info(Args, Record) ->
    RecName = ?Rec:name(Record),
    Info    = ?MISC:format("Renaming record: ~p", [RecName]),
    [{transformation_text, Info} | Args].

%%% ============================================================================
%%% Implementation

cc_newname(NewName, Names) ->
    ?Check( not lists:member(NewName, Names),
            ?LocalError(rec_exists, NewName)),
    NewName.

cc_error(?LocalError(rec_exists, NewName), NewName, _Names) ->
    ?MISC:format("The given record name (~p) is already used!",
                 [NewName]).

query_rec_refs(Record) ->
    [InDefs] = ?Query:exec(Record, ?Rec:form()),
    InRefs   = ?Query:exec(Record, ?Rec:references()),
    {InDefs, InRefs}.

