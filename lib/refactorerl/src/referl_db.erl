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

%%% @doc Low level database functionality.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(referl_db).
-vsn("$Rev: 3185 $").

-include("refactorerl.hrl").
-include("referl_db.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(Backup_name, backup).

-export([init/0]).

-export([set_schema/1, check_schema/1, reset_schema/0]).

-export([checkpoint/0, restore_checkpoint/1, undo/0, redo/0,
         delete_all_backups/0, delete_end_backups/1]).

%%% ----------------------------------------------------------------------------
%%% Init

%% @doc Initializes the mnesia-based database, checks for database
%% schema. When the essential tables are not present, creates them.
init() ->
    %% When mnesia is started the first time, a ram only empty schema is
    %% created. This is the clue that we should start with schema
    %% initialisation.
    case mnesia:table_info(schema, storage_type) of
        ram_copies ->
            create_essential_tables(),
            init_backup_counters(),
            %% no_schema means we should create more tables to store the graph
            %% itself
            no_schema;
        disc_copies ->
            %% has_schema means that the existing schema should be checked
            %% against the needed schema
            mnesia:wait_for_tables([counter],infinity),
            delete_end_backups({1,up}),
            init_backup_counters(),
            has_schema
    end.

create_essential_tables() ->
    %% The schema should be stored on disc
    mnesia:change_table_copy_type(schema, node(), disc_copies),

    %% In a new database, some tables are always created:
    %% node class table

    mnesia:create_table(
      class,
      [{attributes, record_info(fields, class)},
       {disc_copies, [node()]}]),

    %% node id table
    mnesia:create_table(
      nextid,
      [{attributes, record_info(fields, nextid)},
       {disc_copies, [node()]}]),

    %% link target class table
    mnesia:create_table(
      target,
      [{attributes, record_info(fields, target)},
       {disc_copies, [node()]}]),

    %% checkpoint counter table (needs a better name)
    mnesia:create_table(counter, [{ram_copies,[node()]}]).

%%% ----------------------------------------------------------------------------
%%% Schema functions

%% @doc Sets the database schema, creates the tables for node storing.
set_schema(Schema) ->
    init_schema(Schema),
    mnesia:dirty_write(#class{name='$hash', attribs=erlang:phash2(Schema)}).

init_schema([]) ->
    ok;
init_schema([{Class, Attribs, Links} | Tail]) ->
    if
        Class =/= root ->
            mnesia:create_table(Class,
                                [{attributes, [id, attribs]},
                                 {disc_copies, [node()]}]);
        true -> ok
    end,

    mnesia:create_table(
      linktab(Class),
      [{attributes, [id, ind, to]},
       {disc_copies, [node()]},
       {index, [to]},
       {type,bag}]),

    mnesia:dirty_write(#class{name=Class, attribs=Attribs}),
    [ mnesia:dirty_write(#target{start={Class, Tag}, next=To}) ||
        {Tag, To} <- Links],

    init_schema(Tail).

%% @doc Checks weather the stored old schema and the new one are
%% equivalent or different.
check_schema(Schema) ->
    mnesia:wait_for_tables([class, target, counter], infinity),
    [#class{attribs=OldHash}] = mnesia:dirty_read(class, '$hash'),
    NewHash = erlang:phash2(Schema),
    OldHash =:= NewHash.

%% @doc Resets the schema, deletes the schema tables.
reset_schema() ->
    error_logger:info_msg("Resetting database schema, restart follows\n"),
    [mnesia:delete_table(Tab) || Tab <- mnesia:system_info(tables),
                                 Tab =/= schema],
    mnesia:change_table_copy_type(schema, node(), ram_copies).

linktab(Class) ->
    list_to_atom(atom_to_list(Class)++"$lnk").

%%% ----------------------------------------------------------------------------
%%% Checkpoints

init_backup_counters() ->
    mnesia:dirty_write({counter,0,0}),
    mnesia:dirty_write({counter,1,0}).

undo() ->
    IdAkt = mnesia_idakt(),
    Id    = mnesia_id(),
    if IdAkt > 0 ->
            restore_checkpoint(Id),
            mnesia:dirty_write({counter, 1, IdAkt - 1}),
            ok;
        true ->
            throw(invalid_checkpoint_number)
    end.

redo() ->
    IdAkt = mnesia_idakt(),
    Id    = mnesia_id(),
    if IdAkt < Id ->
            restore_checkpoint(IdAkt + 1),
            mnesia:dirty_write({counter, 1, IdAkt + 1}),
            ok;
        true ->
            throw(invalid_checkpoint_number)
    end.

checkpoint()->
    delete_all_backups(),
    Name  = name(?Backup_name, 1),
    %% checkpoint_count()),
    %% checkpoint_number/0 is need to create more backup files
    Tab   = mnesia:system_info(tables),
    Args  = [{name, Name}, {max, Tab},
             {allow_remote, true}, {ram_overrides_dump, true}],
    catch mnesia:activate_checkpoint(Args),
    Dir   = mnesia:system_info(directory),
    File  = filename:absname_join(Dir, Name),
    catch mnesia:backup_checkpoint(Name, File),
    mnesia:deactivate_checkpoint(Name),
    mnesia:dirty_write({counter, 0, checkpoint_count()}),
    IdAkt = mnesia_idakt(),
    mnesia:dirty_write({counter, 1, IdAkt + 1}),
    {ok, Name}.

delete_backup(Id)->
    catch file:delete(
            filename:absname_join(
              mnesia:system_info(directory),
              name(?Backup_name, Id))).

delete_all_backups() ->
    [delete_backup(Id) || Id <- ?MISC:seq2(1, mnesia_id())],
    mnesia:dirty_write({counter, 0, 0}),
    mnesia:dirty_write({counter, 1, 0}),
    ok.

delete_end_backups({Id, up})->
    Name      = name(?Backup_name, Id),
    Directory = mnesia:system_info(directory),
    File      = filename:absname_join(Directory,Name),
    case filelib:is_file(File) of
        true ->
            file:delete(File),
            Dir = up;
        _ ->
            Dir = down
    end,
    delete_end_backups({Id + 1, Dir});
delete_end_backups({Id, down})->
    {Id - 1, deleted}.


restore_checkpoint(Count)->
    Name = name(?Backup_name, Count),
    Dir  = mnesia:system_info(directory),
    File = filename:absname_join(Dir,Name),
    mnesia:dirty_write({counter, 1, Count + 1}),
    catch mnesia:restore(File,[{recreate_tables, []},
                               {skip_tables, [counter]}]).

checkpoint_count() ->
    mnesia_id() + 1.

name(Name, Number) ->
    atom_to_list(Name) ++ "." ++ integer_to_list(Number).

mnesia_idakt() ->
    [{_, _, IdAkt}] = mnesia:dirty_read({counter,1}),
    IdAkt.

mnesia_id() ->
    [{_, _, Id}] = mnesia:dirty_read({counter,0}),
    Id.
