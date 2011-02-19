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
%%% Module Informations

%%% @doc Efficient matrix implementation to be used for clustering.
%%% Attribute matrices are usually quite sparse, entity distance matrices
%%% are not; this representation should work reasonably well with both.

%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(cl_matrix).
-vsn("$Rev: 3263 $").

-include("cluster.hrl").



%%% ============================================================================
%%% Imports/Exports

% Proplists
-import(proplists, [get_value/2, get_value/3]).

% Matrix
-export([new/3, delete/1, clone/1, pack/1, unpack/1, save/3, load/1, 
         is_matrix/1]).
% Manage rows and columns
-export([add_col/2, add_col/3, add_row/2, add_row/3, 
         insert_new_row/3, % depricated
         del_col/2, del_row/2,
         swap_row/3, swap_col/3, move_row/4, move_col/4, 
         reorder_rows/2, reorder_cols/2]).
% Get and set values
-export([cols/1, rows/1,
         get/3, get2/3, get_row/2, get_col/2, 
         get_row_non_def/2, get_col_non_def/2,
         get_row_non_def_ids/2, get_col_non_def_ids/2, 
         set/4]).
% Transform matrix
-export([transform/2, transform2/2, 
         fold_col/4, fold_row/4, fold_col2/5, fold_row2/5]).
% Conversions
-export([to_list/1, from_list/2, to_record/1, from_record/1]).
% Dump
-export([dump/1, dump/2, dump/4]).



%%% ============================================================================
%%% Types

-record(matrix, {rows, cols, default, table}).

%%% @type row() = term(). Row labels.

%%% @type col() = term(). Column labels.

%%% @type val() = term(). Matrix element values.

%%% @type matrix(). The representation of a matrix is not defined.



%%% ============================================================================
%%% Matrix

%% @spec new([row()], [col()], val()) -> matrix()
%%
%% @doc Creates a new matrix with `Rows' as row labels, `Cols' as column
%% labels and `Default' as the default value of uninitialised elements.
new(Rows, Cols, Default) ->
    #matrix{rows=Rows, cols=Cols, default=Default, table=ets:new(matrix, [])}.


%% @spec delete(matrix()) -> ok
%%
%% @doc Deletes the matrix.
delete(#matrix{table=Tab}) ->
    ets:delete(Tab).


%% @spec clone(matrix()) -> matrix()
%%
%% @doc Creates a copy of a matrix.
%% If modification is made in the original or in the clone, it will not affect
%% the other one.
clone(#matrix{table=Table} = Matrix) ->
    Matrix#matrix{table=?MISC:ets_clone(Table)}.


%% @spec pack(Matrix::matrix()) -> PackedMatrix::matrix()
%% @doc Create a new `matrix' object which contains all information from 
%%      the original `Matrix' as values.
%%      Don't use the returned `PackedMatrix' object as the normal
%%      `matrix' objects! In this case a run time error may occur.
%%      If you have a packed `matrix' object you need to unpack with the
%%      {@link unpack/1} function before use it.
%% @see unpack/1
pack(Matrix=#matrix{}) ->
    to_record(Matrix).

%% @spec unpack(PackedMatrix::matrix()) -> Matrix::matrix()
%% @doc Create a normal `matrix' object from the given packed
%%      `Packedmatrix' object.
%% @see pack/1
unpack(Matrix=#matrix{}) ->
    from_record(Matrix).


%% @spec save(M::matrix(), FileName, Options::OptList) -> ok
%%       OptList = [Opt]
%%       Opt = {format,Format} | {id2NameFun, Id2NameFun}
%%       Format = pack | DumpFormat
%%       DumpFormat = normal | list | csv
%%       Id2NameFun = ((term()) -> term())
%% @doc Save the matrix into a file in the specified file format. For more 
%%      information about `DumpFormat' and `Id2NameFun' see {@link dump/4}.
%%      All row and column label will be replaced by the `Id2NameFun' function.
%%      The default format is `pack' which mean the matrix saved as packed
%%      matrix (see {@link pack/1}). The default row/column identifier 
%%      conversion function is the identity function.
%% @see dump/4
save(Matrix=#matrix{}, FileName, Options) ->
    case file:open(FileName, [write]) of
        {ok, Dev} ->
            case get_value(format,Options,pack) of
                pack   -> io:format(Dev, "~p.\n", [pack(Matrix)]);
                Format -> 
                    dump(Format, Dev, Matrix, 
                         get_value(id2NameFun,Options,fun(Id) -> Id end))
            end,
            file:close(Dev);
        {error, Reason} ->
            throw(?LocalError(file_error, file:format_error(Reason)))
    end.    


%% @spec load(FilePath::list()) -> Matrix::matrix()
%% @doc Load a packed `matrix' object from the `FilePath' file and unpack that.
load(FileName) ->
    case file:open(FileName, [read]) of
        {ok, Dev} ->
            case io:read(Dev, "") of
                {ok, Matrix} ->
                    file:close(Dev),
                    unpack(Matrix);
                eof ->
                    throw(?LocalError(file_error, "End of file"));
                {error, Reason} ->
                    throw(?LocalError(file_error, file:format_error(Reason)))
            end;
        {error, Reason} ->
            throw(?LocalError(file_error, file:format_error(Reason)))
    end.

%% @spec is_matrix(Term::term()) -> boolean()
%% @doc  Return `true' if `Term' is a `matrix()' otherwise `false'.
is_matrix(#matrix{}) -> true;
is_matrix(_)         -> false.



%%% ============================================================================
%%% Manage rows and columns

%% @spec add_col(col(), matrix()) -> matrix()
%%
%% @doc Adds a new column label `Col' to the matrix. Returns the updated
%% matrix.
add_col(Col, Matrix=#matrix{cols=Cols}) ->
    case lists:member(Col, Cols) of
        true ->
            erlang:error({existing_column, Col});
        false ->
            Matrix#matrix{cols=Cols++[Col]}
    end.

%% @spec add_row(row(), matrix()) -> matrix()
%%
%% @doc Adds a new row label `Row' to the matrix. Returns the updated
%% matrix.
add_row(Row, Matrix=#matrix{rows=Rows}) ->
    case lists:member(Row, Rows) of
        true ->
            erlang:error({existing_row, Row});
        false ->
            Matrix#matrix{rows=Rows++[Row]}
    end.


%% @spec add_col(ColLabel::row(), 
%%               ColValues::[ {RowLabel::col(), CellValue::val()} ], 
%%               Matrix::matrix()) -> NewMatrix::matrix()
%% @doc Add a new column labeled with `ColLabel' to the `Matrix' matrix. The 
%%      pairs in the `ColValues' specify the initial values in the column.
%%      Return the updated matrix.
add_col(ColLabel, ColValues, Matrix=#matrix{}) ->
    M2 = add_col(ColLabel, Matrix),
    #matrix{table=Tab} = M2,
    lists:foreach(
        fun ({Row, Value}) -> ets:insert(Tab, {{Row, ColLabel}, Value}) end, 
        ColValues),
    M2.

%% @spec add_row(RowLabel::row(), 
%%               RowValues::[{ColLabel::col(), CellValue::val()}], 
%%               Matrix::matrix()) -> NewMatrix::matrix()
%% @doc Add a new row labeled with `RowLabel' to the `Matrix' matrix. The 
%%      pairs in the `RowValues' specify the initial values in the row.
%%      Return the updated matrix.
add_row(RowLabel, RowValues, Matrix=#matrix{}) ->
    M2 = add_row(RowLabel, Matrix),
    #matrix{table=Tab} = M2,
    lists:foreach(
        fun ({Col, Value}) -> ets:insert(Tab, {{RowLabel, Col}, Value}) end, 
        RowValues),
    M2.

%% @spec insert_new_row(row(), [{col(), val()}], matrix()) -> matrix()
%%
%% @doc Inserts a new row with its elements into the matrix, and returns the
%% updated matrix.
%% @deprecated Please use the function {@link add_row/3} instead.
insert_new_row(RowLabel, RowValues, Matrix) ->
    % M2 = add_row(RowLabel, Matrix),
    % #matrix{table=Tab} = M2,
    % lists:foreach(fun ({Col, Value}) ->
                    % ets:insert(Tab, {{RowLabel, Col}, Value})
                  % end, RowValues),
    % M2.
    add_row(RowLabel, RowValues, Matrix).


%% @spec del_col(col(), matrix()) -> matrix()
%%
%% @doc Deletes column `Col' from the matrix, returns the updated matrix.
del_col(Col, Matrix=#matrix{cols=Cols, rows=Rows, table=Tab}) ->
    lists:foreach(
      fun(Row) -> ets:delete(Tab, {Row, Col}) end,
      Rows),
    Matrix#matrix{cols=Cols -- [Col]}.

%% @spec del_row(row(), matrix()) -> matrix()
%%
%% @doc Deletes row `Row' from the matrix, returns the updated matrix.
del_row(Row, Matrix=#matrix{cols=Cols, rows=Rows, table=Tab}) ->
    lists:foreach(
      fun(Col) -> ets:delete(Tab, {Row, Col}) end,
      Cols),
    Matrix#matrix{rows=Rows -- [Row]}.


%% @spec swap_row(Row1::row(), Row2::row(), Matrix::matrix()) -> 
%%           NewMatrix::matrix()
%% @doc Swap `Row1' and `Row2' rows in the matrix.
swap_row(Row1, Row2, M=#matrix{rows=Rows}) ->
    M#matrix{rows=?MISC:list_swap(Row1, Row2, Rows)}.

%% @spec swap_col(Col1::col(), Col2::col(), Matrix::matrix()) -> 
%%           NewMatrix::matrix()
%% @doc Swap `Col1' and `Col2' columns in the matrix.
swap_col(Col1, Col2, M=#matrix{cols=Cols}) ->
    M#matrix{cols=?MISC:list_swap(Col1, Col2, Cols)}.


%% @spec move_row(SrcRow::row(), DstRow::row(), Mode, Matrix::matrix()) -> 
%%           NewMatrix::matrix()
%%      Mode = before | after
%% @doc Move the `SrcRow' before/after the `DstRow' in the matrix.
move_row(SrcRow, DstRow, Mode, M=#matrix{rows=Rows}) ->
    M#matrix{rows=?MISC:list_move(SrcRow, DstRow, Mode, Rows)}.

%% @spec move_col(SrcCol::row(), DstCol::row(), Mode, Matrix::matrix()) -> 
%%           NewMatrix::matrix()
%%      Mode = before | after
%% @doc Move the `SrcCol' before/after the `DstCol' in the matrix.
move_col(SrcCol, DstCol, Mode, M=#matrix{cols=Cols}) ->
    M#matrix{cols=?MISC:list_move(SrcCol, DstCol, Mode, Cols)}.


%% @spec reorder_rows(Rows::[row()], Matrix::matrix()) -> 
%%           NewMatrix::matrix()
%% @doc Reorder the rows of the `Matrix' by the permutation of rows 
%%      given in `Rows'.
reorder_rows(Rows, M=#matrix{rows=Rows0}) when is_list(Rows) ->
    case []==(Rows0--Rows) andalso []==(Rows--Rows0) of 
        true -> ok;
        _    -> throw(?LocalError(invalid_permutation, [Rows0, Rows]))
    end,
    M#matrix{rows=Rows}.

%% @spec reorder_cols(Cols::[col()], Matrix::matrix()) -> 
%%           NewMatrix::matrix()
%% @doc Reorder the columns of the `Matrix' by the permutation of columns 
%%      given in `Cols'.
reorder_cols(Cols, M=#matrix{cols=Cols0}) when is_list(Cols) ->
    case []==(Cols0--Cols) andalso []==(Cols--Cols0) of
        true -> ok;
        _    -> throw(?LocalError(invalid_permutation, [Cols0, Cols]))
    end,
    M#matrix{cols=Cols}.



%%% ============================================================================
%%% Get and set values

%% @spec cols(Matrix::matrix()) -> [col()]
%%
%% @doc Returns the column labels of the matrix.
cols(#matrix{cols=Cols}) ->
    Cols.

%% @spec rows(Matrix::matrix()) -> [row()]
%%
%% @doc Returns the row labels of the matrix.
rows(#matrix{rows=Rows}) ->
    Rows.

%% @spec get(row(), col(), Matrix::matrix()) -> val()
%%
%% @doc Returns the element at position (`Row', `Col').
get(Row, Col, #matrix{table=Tab, default=Def}) ->
    case ets:lookup(Tab, {Row, Col}) of
        []         -> Def;
        [{_, Val}] -> Val
    end.

%% @spec get2(RowLabel::row(), ColumnLabel::col(), Matrix::matrix()) -> 
%%           {default, DefaultValue::val()} | {value, Value::val()}
%% @doc Returns the element at position (`Row', `Col').
get2(Row, Col, #matrix{table=Tab, default=Def}) ->
    case ets:lookup(Tab, {Row, Col}) of
        []         -> {default, Def};
        [{_, Val}] -> {value,   Val}
    end.

%% @spec get_row(row(), matrix()) -> [{col(), val()}]
%%
%% @doc Returns the specified row of the matrix.
get_row(Row, Mtr) ->
    fold_row(fun (C, V, L) -> [{C, V} | L] end, [], Row, Mtr).

%% @spec get_col(col(), matrix()) -> [{row(), val()}]
%%
%% @doc Returns the specified col of the matrix.
get_col(Col, Mtr) ->
    fold_col(fun (R, V, L) -> [{R, V} | L] end, [], Col, Mtr).


%% @spec get_row_non_def(RowId::row(), Matrix::matrix()) -> 
%%           [{ColId::col(), CellValue::val()}]
%% @doc Returns the specified column of the matrix without the default values.
get_row_non_def(Row, #matrix{table=TableID}) ->
    Row1 = if
        is_tuple(Row) -> {Row};
        true -> Row
    end,
    ets:select(TableID, [{ {{'$1','$2'},'$3'}, [{'==',Row1,'$1'}], 
                           [{{ '$2','$3' }}] }]).

%% @spec get_col_non_def(ColId::col(), Matrix::matrix()) -> 
%%           [{RowId::row(), CellValue::val()}]
%% @doc Returns the specified row of the matrix without the default values.
get_col_non_def(Col, #matrix{table=TableID}) ->
    Col1 = if
        is_tuple(Col) -> {Col};
        true -> Col
    end,
    ets:select(TableID, [{ {{'$1','$2'},'$3'}, [{'==',Col1,'$2'}], 
                           [{{ '$1','$3' }}] }]).


%% @spec get_row_non_def_ids(RowId::row(), Matrix::matrix()) -> 
%%           [{ColId::col(), CellValue::val()}]
%% @doc Returns the specified column of the matrix without the default values.
get_row_non_def_ids(Row, #matrix{table=TableID}) ->
    Row1 = if
        is_tuple(Row) -> {Row};
        true -> Row
    end,
    ets:select(TableID, [{ {{'$1','$2'},'_'}, [{'==',Row1,'$1'}], ['$2'] }]).

%% @spec get_col_non_def_ids(ColId::col(), Matrix::matrix()) -> 
%%           [{RowId::row(), CellValue::val()}]
%% @doc Returns the specified row of the matrix without the default values.
get_col_non_def_ids(Col, #matrix{table=TableID}) ->
    Col1 = if
        is_tuple(Col) -> {Col};
        true -> Col
    end,
    ets:select(TableID, [{ {{'$1','$2'},'_'}, [{'==',Col1,'$2'}], ['$1'] }]).


%% @spec set(row(), col(), val(), Matrix::matrix()) -> matrix()
%%
%% @doc Updates the element at position (`Row', `Col'), returns the updated
%% matrix.
set(Row, Col, Value, Matrix=#matrix{table=Tab}) ->
    ets:insert(Tab, {{Row,Col}, Value}),
    Matrix.



%%% ============================================================================
%%% Transform matrix

%% @spec transform((val()) -> val(), matrix()) -> matrix()
%%
%% @doc Applies `Fun' to every element of the matrix and stores the result
%% in its place.
transform(Fun, Matrix=#matrix{default=Def, table=Tab}) ->
    transform(Fun, ets:first(Tab), Tab),
    Matrix#matrix{default=Fun(Def)}.

transform(_Fun, '$end_of_table', _Tab) ->
    ok;
transform(Fun, Key, Tab) ->
    [{_, Val}] = ets:lookup(Tab, Key),
    ets:insert(Tab, {Key, Fun(Val)}),
    transform(Fun, ets:next(Tab, Key), Tab).


%% @spec transform2((col_key(), row_key(),val()) -> val(), matrix()) -> matrix()
%%
%% @doc Applies `Fun' to every element of the matrix and stores the result
%% in its place.
transform2(Fun, Matrix=#matrix{default=Def, table=Tab}) ->
    transform2(Fun, ets:first(Tab), Tab),
    Matrix#matrix{default=Fun(default,default,Def)}.

transform2(_Fun, '$end_of_table', _Tab) ->
    ok;
transform2(Fun, Key, Tab) ->
    [{_, Val}] = ets:lookup(Tab, Key),
    {RowKey, ColKey} = Key,
    ets:insert(Tab, {Key, Fun(RowKey,ColKey,Val)}),
    transform2(Fun, ets:next(Tab, Key), Tab).


%% @spec fold_col(FunType, term(), col(), matrix()) -> term()
%%       FunType = (row(), val(), Acc::term()) -> term()
%%
%% @doc Usual fold operation on a given column. No ordering guarantees.
fold_col(Fun, Acc0, Col, M=#matrix{rows=Rows}) ->
    lists:foldl(
      fun(Row, Acc) -> Fun(Row, get(Row, Col, M), Acc) end,
      Acc0, Rows).

%% @spec fold_row(FunType, term(), row(), matrix()) -> term()
%%       FunType = (col(), val(), Acc::term()) -> term()
%%
%% @doc Usual fold operation on a given row. No ordering guarantees.
fold_row(Fun, Acc0, Row, M=#matrix{cols=Cols}) ->
    lists:foldl(
      fun(Col, Acc) -> Fun(Col, get(Row, Col, M), Acc) end,
      Acc0, Cols).


%% @spec fold_col2(FunType, term(), col(), col(), matrix()) -> term()
%%       FunType = (row(), Val1::val(), Val2::val(), Acc::term()) -> term()
%%
%% @doc Usual fold operation on two columns. No ordering guarantees.
fold_col2(Fun, Acc0, Col1, Col2, M=#matrix{rows=Rows}) ->
    lists:foldl(
      fun(Row, Acc) ->
              Fun(Row, get(Row, Col1, M), get(Row, Col2, M), Acc)
      end, Acc0, Rows).

%% @spec fold_row2(FunType, term(), row(), row(), matrix()) -> term()
%%       FunType = (col(), Val1::val(), Val2::val(), Acc::term()) -> term()
%%
%% @doc Usual fold operation on two rows. No ordering guarantees.
fold_row2(Fun, Acc0, Row1, Row2, M=#matrix{cols=Cols}) ->
    lists:foldl(
      fun(Col, Acc) ->
              Fun(Col, get(M, Row1, Col), get(M, Row2, Col), Acc)
      end, Acc0, Cols).



%%% ============================================================================
%%% Conversion

%% @spec to_list(Matrix::matrix()) ->
%%           [{{Row::row(), Col::col()}, Value::term()}]
%% @doc  Fetch `Matrix' contain into a list.
to_list(#matrix{table=TabID}) ->
    ets:tab2list(TabID).


%% @spec from_list(List::[{{Row::row(), Col::col()}, Value::term()}], 
%%               Default::term()) -> matrix()
%% @doc  Create a matrix from `List'. Every element in the `List' contain the
%%       description of a cell of the matrix. This is a row-column index pair 
%%       and the value of current cell.
from_list(List, Default) ->
    % Calculate row and column identifications lists
    {Rows1, Cols1} = lists:foldl(
        fun({{Row,Col}, _Value}, {Rs,Cs}) -> {[Row|Rs],[Col|Cs]} end,
        {[],[]},
        List),
    % Create matrix
    lists:foldl(
        fun({{Row,Col}, Value}, Matrix=#matrix{}) ->
            set(Row, Col, Value, Matrix)
        end,
        new(lists:usort(Rows1), lists:usort(Cols1), Default),
        List).


%% @spec to_record(Matrix::matrix()) -> RecordMatrix::matrix()
%% @doc  Create a record from `Matrix'. This record contains all information 
%%       about `Matrix' and all the values as well. There are no other required 
%%       resources like ETS tables, etc. It contains everything as values.
%%
%%       `RecordMatrix' is not a usable matrix with matrix operations. It 
%%       can be used to store the whole matrix in a single term for example 
%%       when you want to save the matrix into a file. Before the use you need 
%%       to convert it back with the {@link from_record/1} function.
to_record(M=#matrix{}) ->
    M#matrix{table=to_list(M)}.

%% @spec from_record(RecordMatrix::matrix()) -> Matrix::matrix()
%% @doc  Create a matrix from the `RecordMatrix' that was created previously 
%%       from a matrix by the {@link to_record/1} function.
%% @see to_record/1
from_record(M=#matrix{rows=Rows, cols=Cols, default=Default, table=TabList}) ->
    #matrix{rows=Rows1,cols=Cols1,table=TabID1} = from_list(TabList,Default),
    case []==(Rows1--Rows) andalso []==(Cols1--Cols) of
        true -> M#matrix{table=TabID1};
        _    -> throw(?LocalError(bad_rowcols, [Rows1--Rows, Cols1--Cols]))
    end.



%%% ============================================================================
%%% Dump

%% @spec dump(matrix()) -> ok
%% @doc Prints the contents of a matrix to the standard output.
%% @see dump/2
%% @see dump/4
dump(M=#matrix{}) -> 
    dump(standard_io, M).

%% @spec dump(matrix(), io_device()) -> ok
%% @doc Prints the contents of a matrix to a given device.
%% @see dump/4
dump(Dev, M=#matrix{}) -> 
    dump(normal, Dev, M, fun(Id) -> Id end).

%% @spec dump(FileFormat, matrix(), io_device(), Id2NameFun) -> ok
%%       FileFormat = normal | csv
%%       Id2NameFun = ((term()) -> term())
%% @doc Prints the contents of a matrix to a given device in a specified file
%%      format.
%%
%% Allowed file formats:
%% <ul>
%%   <li>`normal': write the column headers in a list in the first line.
%%     The second line contain a list with 2-tuples. The first element of the 
%%     tuples is the row header and the second element is a list with the 
%%     row values in same order as the cols in the first line.</li>
%%   <li>`list': write a list with 2-tuple elements. The first element of every
%%     tuple is a 2-tuple which contain the row and column label. The second
%%     element is the value of the corresponding cell.</li>
%%   <li>`csv': (Comma Separated Values) Wtite a table. The rows are in 
%%     separated lines. The values in a row are separated with a semicolon(;).
%%     The first line contains the column headers (started with an empty cell).
%%     The following lines contain the rows started with the row header.</li>
%% </ul>
%%
%% When the column and row headers need to write the `Id2NameFun(Header)' will
%% be written instead the original headers. In default the `Id2NameFun' is the 
%% `fun(Id) -> Id end' identical function.
dump(normal, Dev, M=#matrix{}, Id2NameFun) when is_function(Id2NameFun) ->
    io:format(Dev, "~p~n~p~n",
               [lists:map(Id2NameFun,cols(M)),
                [{Id2NameFun(R),lists:reverse(
                      fold_row(fun(_,V,L)-> [V|L] end, [], R, M))} ||
                    R <- rows(M)]]);
% List: simple dump the to_list/1 result
dump(list, Dev, M=#matrix{}, Id2NameFun) when is_function(Id2NameFun) ->
    io:format(Dev, "~p\n",
        [lists:map(fun({{R,C},V}) -> {{Id2NameFun(R),Id2NameFun(C)},V} end,
                  to_list(M))]);
% CSV (Comma Separated Values)
dump(csv, Dev, M=#matrix{}, Id2NameFun) when is_function(Id2NameFun) ->
    % Get row and column identifiers
    Rows=rows(M),
    Cols=cols(M),
    % Write column headers
    lists:foreach(fun(C) -> io:format(Dev, ";~p", [Id2NameFun(C)]) end, Cols),
    io:format(Dev, "\n", []),
    % Write rows
    lists:foreach(
        fun(R) ->
            io:format(Dev, "~p", [Id2NameFun(R)]),
            lists:foreach(
                fun(C) -> 
                    case get2(R,C,M) of
                        {default, _} -> io:format(Dev, ";",   []);
                        {value,   V} -> io:format(Dev, ";~p", [V])
                    end
                end,
                Cols),
            io:format(Dev, "\n", [])
        end,
        Rows).


