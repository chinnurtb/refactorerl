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

%%% @doc Efficient matrix implementation to be used for clustering.
%%% Attribute matrices are usually quite sparse, entity distance matrices
%%% are not; this representation should work reasonably well with both.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(cl_matrix).
-vsn("$Rev: 1246 $").

-export([new/3, delete/1, add_col/2, add_row/2, del_col/2, del_row/2,
         cols/1, rows/1, get/3, set/4, transform/2, transform2/2,
         fold_col/4, fold_row/4, fold_col2/5, fold_row2/5, clone/1]).

-export([dump/1, dump/2]).

-record(matrix, {rows, cols, default, table}).

%%% @type row() = term(). Row labels.

%%% @type col() = term(). Column labels.

%%% @type val() = term(). Matrix element values.

%%% @type matrix(). The representation of a matrix is not defined.

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

%% @spec add_col(col(), matrix()) -> matrix()
%%
%% @doc Adds a new column label `Col' to the matrix. Returns the updated
%% matrix.
add_col(Col, Matrix=#matrix{cols=Cols}) ->
    case lists:member(Col, Cols) of
        true ->
            erlang:error({existing_column, Col});
        false ->
            Matrix#matrix{cols=[Col|Cols]}
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
            Matrix#matrix{rows=[Row|Rows]}
    end.

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

%% @spec set(row(), col(), val(), Matrix::matrix()) -> matrix()
%%
%% @doc Updates the element at position (`Row', `Col'), returns the updated
%% matrix.
set(Row, Col, Value, Matrix=#matrix{table=Tab}) ->
    ets:insert(Tab, {{Row,Col}, Value}),
    Matrix.

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

%% @spec clone(matrix()) -> matrix()
%%
%% @doc Creates a copy of a matrix.
%% If modification is made in the original or in the clone, it will not affect
%% the other one.
clone(#matrix{table=Table} = Matrix) ->
    Table2 = ets:new(matrix,[]),
    ets:foldl(
      fun(Row,_) ->
              ets:insert(Table2,Row)
      end,
      undefined,
      Table),
    Matrix#matrix{table=Table2}.


%% @spec dump(matrix()) -> ok
%%
%% @doc Prints the contents of a matrix to the standard output.
dump(M) ->
    dump(standard_io, M).

%% @spec dump(matrix(),io_device()) -> ok
%%
%% @doc Prints the contents of a matrix to a given device.
dump(D,M) ->
    io:format(D,"~p~n~p~n",
               [cols(M),
                [{R,lists:reverse(
                      fold_row(fun(_,V,L)-> [V|L] end, [], R, M))} ||
                    R <- rows(M)]]).

