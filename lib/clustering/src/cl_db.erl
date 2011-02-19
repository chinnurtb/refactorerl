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

%%% @doc This module offers a way to store a few data structures of the program
%%% persistently.
%%%
%%% The module has two levels.
%%%
%%% The low level can be used to save and load data structures to and from the
%%% Mnesia database. The name of the tables can be specified.
%%% Currently the matricies and ets tables are supported in the low level.
%%%
%%% The high level allows the user to invoke only the {@link update/1} and
%%% {@link invalidate/1} functions, that will take care of the recalculation of
%%% the tables. Only the tables necessary will be recalculated.
%%% Currently the attribute matricies and the dependencies are supported in the
%%% high level.
%%%
%%% Terminology: the word "table" will be used in an abstract way, it does not
%%% necessary mean one Mnesia table (e.g. a matrix is stored in a table, which
%%% is actually two Mnesia tables).
%%% The expression "Mnesia table" will be used when not the abstract tables are
%%% meant.
%%% 
%%% An example:
%%% If one needs the function attribute matrix, first he has to make sure
%%% that the function attribute matrix is up-to-date in the database with the
%%% following function call:
%%% ```
%%% cl_db:update(fun_attr)
%%% '''
%%%
%%% Then the matrix (which contains an ets table) can be obtained:
%%% ```
%%% FunAttr = cl_db:load_matrix(fun_attr, fun_attr)
%%% '''
%%%
%%% @todo Write the `load_ets' function.
%%% @todo `save_ets' works only if the table is a set, so maybe it should be
%%% renamed.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_db).
-vsn("$Rev: 3185 $").

-export([recalculate/1, update/1, is_valid/1, invalidate/0,
         save_matrix/2, load_matrix/2, delete_matrix/1, matrix_exists/1,
         save_deps/0, delete_deps/0, mnesia_table_exists/1]).

-include_lib("stdlib/include/qlc.hrl").

%%% @type table_name() = atom().
%%% The name of an (abstract) table.
%%% It is NOT necessarily the name of a Mnesia table.
%%%
%%% The supported table names currently are:
%%%
%%% <ul>
%%%     <li>`mod_attr': the module attribute matrix.</li>
%%%     <li>`fun_attr': the function attribute matrix.</li>
%%%     <li>`deps': the dependencies.</li>
%%% </ul>
%%%
%%% @type mnesia_result() = {atomic, ok} | {aborted, reason()}.

%% @type matrix() = #matrix{rows = [term()],
%%                          cols = [term()],
%%                          default = term(),
%%                          table = ets()}.
%% It represents a matrix.
-record(matrix, {rows, cols, default, table}).

%% @type matrix_label() = #matrix_label{index = {row, Row::integer()} |
%%                                              {col, Column::integer()},
%%                                      value = term()}.
%% It represents a label of a row or a column of a matrix.
%% If it is the label of the `N'th row, `index' will be `{row, N}'.
%% If it is the label of the `N'th column, `index' will be `{col, N}'.
%% `value' is the "name" or "label" of that row or column.
-record(matrix_label, {index, value=undefined}).

%% @type matrix_cell() = #matrix_cell{index = {Row::integer(),
%%                                             Column::integer()},
%%                                    value = term()}.
%% It represents a cell of a matrix.
%% If the row of the cell is `Row', the column of the cell is `Column' and the
%% value of the cell is `Value', the matrix_cell's `index' will be
%% `{Row, Column}' and its `value' will be `Value'.
-record(matrix_cell, {index, value}).

%%% ======================================================================
%%% High level functions

%% @spec recalculate(table_name()) -> ok
%%
%% @doc Recalculates the given table.
%% If `all' is given, all tables will be recalculated.
recalculate(mod_attr) ->
    MA = cl_attr:mod_attrib_data(), 
    ME = cl_attr:mod_empty_attrib(), 
    M = cl_core:attribs(MA, ME), 
    cl_db:save_matrix(M, mod_attr),
    ok;
recalculate(fun_attr) ->
    FA = cl_attr:fun_attrib_data(), 
    FE = cl_attr:fun_empty_attrib(), 
    F = cl_core:attribs(FA, FE), 
    cl_db:save_matrix(F, fun_attr),
    ok;
recalculate(deps) ->
    cl_db:save_deps();
recalculate(fdg) ->
    save_fdg();
recalculate(all) ->
    recalculate(mod_attr),
    recalculate(fun_attr),
    recalculate(deps),
    recalculate(fdg).

%% @spec update(table_name() | all) -> ok
%%
%% @doc Updates the given table.
%% If the given table is valid, nothing will happen.
%% If it is invalid or it does not exist, it will be calculated.
%% If `all' is given, all tables will be updated.
update(TableName) when TableName == mod_attr;
                       TableName == fun_attr ->
    case cl_db:matrix_exists(TableName) of
        false -> recalculate(TableName);
        true -> ok
    end;
update(deps) ->
    %case cl_db:mnesia_table_exists(deps) of
    case is_valid(deps) of
        false -> recalculate(deps);
        true -> ok
    end;
update(fdg) -> 
    case is_valid(fdg) of
        false -> recalculate(fdg);
        true  -> ok
    end;
update(all) ->
    update(mod_attr),
    update(fun_attr),
    update(deps),
    update(fdg).

%% @spec is_valid(table_name() | all) -> bool()
%%
%% @doc Returns whether the given table is valid.
%% If `all' is given, it will return whether all the tables are valid.
is_valid(TableName) when TableName == mod_attr;
                         TableName == fun_attr ->
    cl_db:matrix_exists(TableName);
is_valid(deps) ->
    %cl_db:mnesia_table_exists(deps);
    cl_db:mnesia_table_exists(function_calls) andalso 
    cl_db:mnesia_table_exists(record_calls);
is_valid(fdg) ->
    cl_db:mnesia_table_exists(ffdg) andalso 
    cl_db:mnesia_table_exists(frdg);
is_valid(all) ->
    is_valid(mod_attr) andalso
    is_valid(fun_attr) andalso
    is_valid(deps),
    is_valid(fdg).

%% @spec invalidate() -> ok
%%
%% @doc Invalidates all the tables in the database.
invalidate() ->
    cl_db:delete_matrix(mod_attr),
    cl_db:delete_matrix(fun_attr),
    cl_db:delete_deps(),
    delete_fdg().

%% @spec save_deps() -> ok
%%
%% @doc Saves the dependencies of the functions and records into the database.
save_deps() ->
    FunctionCalls = ets:new(function_calls, []),
    cl_deps:insert_function_calls(FunctionCalls),
    RecordRefs = ets:new(record_refs, []),
    cl_deps:insert_record_refs(RecordRefs),
    save_ets(FunctionCalls, function_calls, function_call,
             [relation, ok]),
    save_ets(RecordRefs, record_refs, record_ref,
             [relation, ok]),
    ets:delete(FunctionCalls),
    ets:delete(RecordRefs),
    ok.

%% @spec delete_deps() ->
%%           ok | {mnesia_result() | ok,
%%                 mnesia_result() | ok}
%%
%% @doc Deletes the dependencies of the functions and records from the database.
delete_deps() ->
    case {delete_mnesia_table(function_calls), 
          delete_mnesia_table(record_refs)} of
        {ok, ok} -> ok;
        Error -> Error
    end.


%% @spec save_fdg() -> ok
%% @doc Saves the function depending graph into the database. It mean create a 
%%      mnesia table (named `ffdg') that contains the function-function 
%%      dependences and an another table (named `frdg') that contains the 
%%      function-record dependences.
%% @see cl_db:create_ffdg/1
%% @see cl_db:create_frdg/1
save_fdg() -> 
    FunFunDeps = ets:new(ffdg, []),
    FunRecDeps = ets:new(frdg, []),
    cl_deps:create_ffdg(FunFunDeps),
    cl_deps:create_frdg(FunRecDeps),
    save_ets(FunFunDeps, ffdg, fun_fun_dep, [relation, count]),
    save_ets(FunRecDeps, frdg, fun_rec_dep, [relation, count]),
    ets:delete(FunFunDeps),
    ets:delete(FunRecDeps),
    ok.


%% @spec delete_fdg() -> ok
%% @doc Delete the function depending graph from the database.
%% @see save_fdg/0
delete_fdg() ->
    case {delete_mnesia_table(ffdg), 
          delete_mnesia_table(frdg)} of
        {ok, ok} -> ok;
        Error -> Error
    end.



%%% ======================================================================
%%% Low level functions

%%% ----------------------------------------------------------------------
%%% Saving matricies

%% @spec save_matrix(matrix(), table_name()) -> ok
%%
%% @doc Saves the given matrix to a database table.
%% The matrix will be saved into two Mnesia tables, whose names are
%% <code>TableName+'_labels'</code> and <code>TableName+'_cells'</code>.
%% If the Mnesia tables do not exist, new tables will be created. Otherwise the
%% tables will be overwritten.
save_matrix(#matrix{rows=Rows, cols=Cols, default=Default, table=EtsTable},
            TableName) ->

    LabelTable = cl_utils:concat_atoms(TableName, '_labels'),
    CellTable  = cl_utils:concat_atoms(TableName, '_cells'),

    LabelCreated = create_table(LabelTable, matrix_label,
                                record_info(fields, matrix_label)),
    CellCreated = create_table(CellTable, matrix_cell,
                               record_info(fields, matrix_cell)),

    mnesia:transaction(
      fun() ->
              case {LabelCreated, CellCreated} of
                  {{atomic, ok}, {atomic, ok}} ->
                      ok;
                  {{aborted, _}, {aborted, _}} ->
                      mnesia:clear_table(LabelTable),
                      mnesia:clear_table(CellTable)
              end,
              write_matrix(LabelTable, CellTable, Rows, Cols, Default, EtsTable)
      end),
    ok.

%% @spec create_table(atom(), atom(), [atom()]) -> mnesia_result()
%%
%% @doc Creates an empty table with the name `MnesiaTable', and with the given
%% record name and attributes.
create_table(MnesiaTable, RecordName, Attribs) ->
    mnesia:create_table(MnesiaTable,
                        [{attributes, Attribs},
                         {record_name, RecordName},
                         {disc_copies, [node()]}]).

%% @spec write_matrix(atom(), atom(), [term()], [term()], term(), ets()) -> ok
%%
%% @doc Writes the given matrix (`Rows', `Cols', `Default', `EtsTable')
%% into the given tables (`LabelTable', `CellTable').
write_matrix(LabelTable, CellTable, Rows, Cols, Default, EtsTable) ->
    lists:foldl(write_label_fun(LabelTable, row), ok, Rows),
    lists:foldl(write_label_fun(LabelTable, col), ok, Cols),
    mnesia:write(CellTable, #matrix_cell{index=default, value=Default}, write),

    ets:foldl(
      fun({{Row, Col}, Value}, _) ->
              Cell = #matrix_cell{index={Row, Col}, value=Value},
              mnesia:write(CellTable, Cell, write)
      end,
      ok,
      EtsTable),
    ok.

%% @spec write_label_fun(atom(), term()) -> ((Item::term(), term()) -> term())
%%
%% @doc Returns a function that writes `#matrix_label{index={Type, Item}}' into
%% the `LabelTable' Mnesia table.
write_label_fun(LabelTable, Type) ->
    fun(Item, _) ->
            Label = #matrix_label{index={Type, Item}},
            mnesia:write(LabelTable, Label, write)
    end.

%%%%% Other operations on matricies

%% @spec load_matrix(table_name(), atom()) -> matrix()
%%
%% @doc Loads the matrix which has the name `Table' from the database.
%% The result is a matrix that contains an ets table.
%% The name of the ets table will be `EtsName'.
load_matrix(Table, EtsName) ->
    LabelTable = cl_utils:concat_atoms(Table, '_labels'),
    CellTable  = cl_utils:concat_atoms(Table, '_cells'),

    {Rows, Cols} =
        lists:foldl(
          fun (#matrix_label{index=Index}, {Rows, Cols}) ->
                  case Index of 
                      {row, Row} -> {[Row|Rows], Cols};
                      {col, Col} -> {Rows, [Col|Cols]}
                  end
          end,
          {[], []},
          table_list(LabelTable)),

    EtsTable = ets:new(EtsName, []),
    Default =
        lists:foldl(
          fun (#matrix_cell{index=default, value=Value}, _) ->
                  Value;
              (#matrix_cell{index=RowCol, value=Value}, Default) ->
                  ets:insert(EtsTable, {RowCol, Value}),
                  Default
          end,
          undefined,
          table_list(CellTable)),

    #matrix{rows=Rows, cols=Cols, default=Default, table=EtsTable}.

%% @spec delete_matrix(table_name()) -> 
%%           ok | {mnesia_result() | ok,
%%                 mnesia_result() | ok}
%%
%% @doc Deletes the given matrix from the database.
delete_matrix(Name) ->
    case {delete_mnesia_table(cl_utils:concat_atoms(Name, '_labels')),
          delete_mnesia_table(cl_utils:concat_atoms(Name, '_cells'))} of
        {ok, ok} -> ok;
        Error -> Error
    end.

%% @spec matrix_exists(table_name()) -> bool()
%%
%% @doc Returns whether the given matrix exists in the database.
matrix_exists(Name) ->
    case {mnesia_table_exists(cl_utils:concat_atoms(Name, '_labels')),
          mnesia_table_exists(cl_utils:concat_atoms(Name, '_cells'))} of
        {true, true} -> true;
        {false, false} -> false
    end.

%%% ----------------------------------------------------------------------
%%% Operations on ets tables

%% @spec save_ets(ets(), atom(), atom(), [atom()]) -> ok
%%
%% @doc Saves the given ets table to the database.
%% It will be saved into the table `MnesiaTable', which will be an actual
%% Mnesia table.
%% If the table do not exist, a new table will be created. Otherwise the table
%% will be overwritten.
save_ets(EtsTable, MnesiaTable, RecordName, RecordFields) ->

    TableCreated = create_table(MnesiaTable, RecordName, RecordFields),

    mnesia:transaction(
      fun() ->
              case TableCreated of
                  {atomic, ok} ->
                      ok;
                  {aborted, _} ->
                      mnesia:clear_table(MnesiaTable)
              end,
              ets:foldl(
                fun(Item, _) ->
                        mnesia:write(
                          MnesiaTable,
                          list_to_tuple([RecordName|tuple_to_list(Item)]),
                          write)
                end,
                ok,
                EtsTable)
      end),
    ok.

%%% ======================================================================
%%% Utilities

%% @spec table_list(atom()) -> [term()]
%%
%% @doc Collects the content of the given Mnesia table into a list.
table_list(Table) ->
    Q = qlc:q([X || X <- mnesia:table(Table)]),
    {atomic, L} = mnesia:transaction(fun() -> qlc:e(Q) end),
    L.

%% @spec delete_mnesia_table(atom()) -> ok | {aborted, reason()}
%%
%% @doc Deletes the given Mnesia table.
%% If the tables exists, it does not do anything.
delete_mnesia_table(Table) ->
    case mnesia_table_exists(Table) of
        false -> 
            ok;
        true -> 
            case mnesia:delete_table(Table) of
                {atomic, ok} -> ok;
                Error -> Error
            end
    end.

%% @spec mnesia_table_exists(atom()) -> bool()
%%
%% @doc Returns whether the `Table' is an existing Mnesia table or not.
%%
%% @todo Is there a better solution?
mnesia_table_exists(Table) ->
    try
        mnesia:table_info(Table, all),
        true
    catch
        _:_ -> false
    end.

