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

%%% @doc Direct database operations. The purpose of this module is to hide
%%% storage and transaction details, and to incorporate database level
%%% undo/redo. A long term goal is to enable switching off disk storage and
%%% run analysis in raw context to speed up initial loading.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refcore_db).
-vsn("$Rev: 5134 $").

-export([init/0, delete_all/0, exists/1, create/3, exec/1, exec/2]).
-export([backup/0, restore/1, undo/0, redo/0, clean/0]).

%% @spec init() -> empty | exists
%% @doc Initializes the database connection, creates a new database if
%% necessary.
init() ->
    %% When mnesia is started the first time, a ram only empty schema is
    %% created. This is the clue that we should start with schema
    %% initialization.
    case mnesia:table_info(schema, storage_type) of
        ram_copies ->
            %% The schema should be stored on disc
            mnesia:change_table_copy_type(schema, node(), disc_copies),

            %% checkpoint counter table (needs a better name)
            mnesia:create_table(counter, [{ram_copies,[node()]}]),
            init_counter(),

            empty;

        disc_copies ->
            mnesia:wait_for_tables([counter],infinity),
            delete_end_backups({1,up}),
            init_counter(),
            exists
    end.

%% @spec delete_all() -> ok
%% @doc Deletes all database tables.
delete_all() ->
    [ mnesia:delete_table(Tab) || Tab <- mnesia:system_info(tables),
                                  Tab =/= schema],
    mnesia:change_table_copy_type(schema, node(), ram_copies).

%% @spec exists(atom()) -> bool()
%% @doc Checks if `Table' is available.
exists(Table) ->
    try mnesia:table_info(Table, storage_type) of
        disc_copies ->
            mnesia:wait_for_tables([Table], infinity),
            true;
        _ ->
            true
    catch
        exit:{aborted, {no_exists, _, _}} ->
            false
    end.

%% @spec create(atom(), [atom()], [Opt]) -> ok
%% @doc Create a new table. `Opt' may be (almost) any mnesia table option.
create(Name, Attribs, Opts) ->
    {atomic, ok} = mnesia:create_table(Name,
                                       [{attributes, Attribs},
                                        {disc_copies, [node()]} | Opts]).

%% @spec exec(() -> term()) -> term()
%% @doc Executes `Fun' in a mnesia activity context.
exec(Fun) -> exec(Fun, wr).

%% @spec exec(() -> term(), rd | wr) -> term()
%% @doc Executes `Fun' in the given type of mnesia activity context. `wr'
%% means the transaction may read and write (or should be protected by
%% simultaneous writes), `rd' means the transaction is read only and there may
%% be no simultaneous writes.
exec(Fun, wr) ->
    mnesia:async_dirty(Fun);
exec(Fun, rd) ->
    mnesia:ets(Fun).

backup() ->
    checkpoint().

restore(Count) ->
    restore_checkpoint(Count),
    restore.

undo() ->
    IdAkt = mnesia_idakt(),
    Id    = mnesia_id(),
    if
        IdAkt > 0 ->
            restore_checkpoint(Id),
            mnesia:dirty_write({counter, 1, IdAkt - 1}),
            ok;
        true ->
            invalid_checkpoint_number
    end.

redo() ->
    IdAkt = mnesia_idakt(),
    Id    = mnesia_id(),
    if
        IdAkt < Id ->
            restore_checkpoint(IdAkt + 1),
            mnesia:dirty_write({counter, 1, IdAkt + 1}),
            ok;
        true ->
            invalid_checkpoint_number
    end.

clean() ->
    Id = mnesia_id(),
    delete_all_backups(Id).




%%% ---------------------------------------------------------------------------
%%% Checkpoints

-define(NAME, backup).


init_counter() ->
    mnesia:dirty_write({counter,0,0}),
    mnesia:dirty_write({counter,1,0}).

checkpoint()->
    %%temporary function call
    delete_all_backups(1),
    Name  = name(backup, 1),%checkpoint_count()),
    %checkpoint_number/0 is need to create more backup files
    Tab   = mnesia:system_info(tables),
    Args  = [{name,Name},{max, Tab},
             {allow_remote,true},{ram_overrides_dump,true}],
    catch mnesia:activate_checkpoint(Args),
    Dir   = mnesia:system_info(directory),
    File  = filename:absname_join(Dir,Name),
    catch mnesia:backup_checkpoint(Name,File),
    mnesia:deactivate_checkpoint(Name),
    mnesia:dirty_write({counter, 0, checkpoint_count()}),
    IdAkt = mnesia_idakt(),
    mnesia:dirty_write({counter, 1, IdAkt + 1}),
    {ok, Name}.

delete_all_backups(0)->
   mnesia:dirty_write({counter, 0, 0}),
   mnesia:dirty_write({counter, 1, 0}),
   {ok, backups_deleted};
delete_all_backups(Id)->
   Name = name(backup, Id),
   file:delete(filename:absname_join(mnesia:system_info(directory),Name)),
   delete_all_backups(Id - 1).


delete_end_backups({Id, up})->
    Name      = name(backup, Id),
    Directory = mnesia:system_info(directory),
    File      = filename:absname_join(Directory,Name),
    case filelib:is_file(File) of
        true ->
            file:delete(
              filename:absname_join(mnesia:system_info(directory),Name)),
            Dir = up;
        _ ->
            Dir = down
    end,
    delete_end_backups({Id + 1, Dir});
delete_end_backups({Id, down})->
    {Id - 1, deleted}.


restore_checkpoint(Count)->
    Name = name(?NAME, Count),
    Dir  = mnesia:system_info(directory),
    File = filename:absname_join(Dir,Name),
    mnesia:dirty_write({counter, 1, Count + 1}),
    catch mnesia:restore(File,[{recreate_tables, []},
                               {skip_tables, [counter]}]).


%% Returns the number of checkpoints in the system.
checkpoint_count() ->
    mnesia_id() + 1.

%%
name(Name, Number) ->
    atom_to_list(Name) ++ "." ++ integer_to_list(Number).


mnesia_idakt() ->
    [{_, _, IdAkt}] = mnesia:dirty_read({counter,1}),
    IdAkt.

mnesia_id() ->
    [{_, _, Id}] = mnesia:dirty_read({counter,0}),
    Id.
