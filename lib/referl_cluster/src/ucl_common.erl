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

%%% @doc Common functions.

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_common).
-vsn("$Rev: $").

-include("ucluster.hrl").



%%% ============================================================================
%%% Imports/exports

% Matrix
-export([matrix_delete/1, matrix_clone/1, matrix_pack/1, matrix_unpack/1]).
% IO
-export([save/3, load/1, load_test/3, load_test_rec/2]).
% ETS
-export([ets_pack/1, ets_unpack/2]).



%%% ============================================================================
%%% Matrix

%% @spec matrix_delete(Matrix::undefined | matrix()) -> ok
%% @doc  Delete a matrix.
%% @see  cl_matrix:delete/1
matrix_delete(?UNDEF) -> ?UNDEF;
matrix_delete(M)      -> ?Matrix:delete(M).

%% @spec matrix_clone(Matrix::undefined | matrix()) -> undefined | matrix()
%% @doc  Clone a matrix.
%% @see  cl_matrix:clone/1
matrix_clone(?UNDEF) -> ?UNDEF;
matrix_clone(M)      -> ?Matrix:clone(M).

%% @spec matrix_pack(Matrix::undefined | matrix()) -> undefined | matrix()
%% @doc  Pack a matrix
%% @see  cl_matrix:pack/1
matrix_pack(?UNDEF) -> ?UNDEF;
matrix_pack(M)      -> ?Matrix:pack(M).

%% @spec matrix_unpack(Matrix::undefined | matrix()) -> undefined | matrix()
%% @doc  Unpack a matrix
%% @see  cl_matrix:unpack/1
matrix_unpack(?UNDEF) -> ?UNDEF;
matrix_unpack(M)      -> ?Matrix:unpack(M).



%%% ============================================================================
%%% IO

%% @spec save(FormatStr::string(), FormatArgs::[term()], FilePath::string()) ->
%%           ok
%% @doc Save elems in `FormatArgs' into the `FilePath' file in the `FormatStr'
%%      format.
%%
%% Example:
%% ```
%% > save("~p.", [{1,2,[3,4,and_so_on]}], "file.txt").
%% '''
save(FormatStr, FormatArgs, FilePath) when is_list(FormatStr), 
        is_list(FormatArgs), is_list(FilePath) ->
    case file:open(FilePath, [write]) of
        {ok, Dev} ->
            io:format(Dev, FormatStr, FormatArgs),
            file:close(Dev);
        {error, Reason} ->
            throw(?RefError(file_open, [FilePath, file:format_error(Reason)]))
    end.    


%% @spec load(FilePath::string()) -> FileContent::term()
%% @throws refError(file_eof) | refError(file_error) | refError(file_open)
%% @doc Load the file content from the `FilePath' file.
load(FilePath) when is_list(FilePath) ->
    case file:open(FilePath, [read]) of
        {ok, Dev} ->
            case io:read(Dev, "") of
                {ok, FileContent} -> 
                    file:close(Dev),
                    FileContent;
                eof ->
                    throw(?RefError(file_eof, [FilePath, "End of file"]));
                {error, Reason} ->
                    throw(?RefError(file_error, 
                                    [FilePath, file:format_error(Reason)]))
            end;
        {error, Reason} ->
            throw(?RefError(file_open, [FilePath, file:format_error(Reason)]))
    end.

%% @spec load_test(FilePath::string(), TestFun::TestFun, Type::atom()) ->
%%           FileContent::term()
%%       TestFun = ((term()) -> boolean())
%% @throws refError(file_load_wrong_datatype)
%% @doc Load the file content from the `FilePath' file and check if the content
%%      is accepted by `TestFun'. The `Type' is the name of the datastructure
%%      needed to load from the file. If the `TestFun' don't accept the file
%%      content (return with `false') a `refError(file_load_wrong_datatype)'
%%      exception will be thrown with the `[FilePath,Type]' parameters.
load_test(FilePath,TestFun,Type) when is_list(FilePath), is_function(TestFun,1),
        is_atom(Type) ->
    FileContent = load(FilePath),
    case TestFun(FileContent) of
        true  -> FileContent;
        false -> throw(?RefError(file_load_wrong_datatype, [FilePath,Type]))
    end.

%% @spec load_test_rec(FilePath::string(), RecordType::atom()) ->
%%           FileContent::tuple()
%% @throws refError(file_load_wrong_datatype)
%% @doc Load the file content from the `FilePath' file and check if the content
%%      is a `RecordType' record.
load_test_rec(FilePath, RecType) when is_list(FilePath), is_atom(RecType) ->
    TestFun = fun(FileContent) ->
        is_tuple(FileContent) andalso RecType==element(1,FileContent)
    end,
    load_test(FilePath, TestFun, RecType).



%%% ============================================================================
%%% ETS

%% @spec ets_pack(EtsTableID::tid()) ->
%%           {TableName::atom(), Options::list(), Content::list()}
%% @doc Save the `EtsTableID' ETS table into a 3-tuple. The `Options' list can
%%      be used to create a new ETS table (see {@link ets:new/2}). The `Content'
%%      is a copy of the table content (see {@link ets:tab2list/1}).
ets_pack(EtsTableID) ->
    % Get informations about original table
    Name        = ets:info(EtsTableID, name),
    Type        = ets:info(EtsTableID, type),
    Named_table = case ets:info(EtsTableID, named_table) of
        true  -> [named_table];
        false -> []
    end,
    Protection  = ets:info(EtsTableID, protection),
    Keypos      = ets:info(EtsTableID, keypos),
    Content     = ets:tab2list(EtsTableID),
    % Build options list for ets:new/2
    Options     = [Type, Protection, {keypos,Keypos}|Named_table],
    % Return name, options and table content
    {Name, Options, Content}.

%% @spec ets_unpack({Name::atom(), Options::list(), Content::list()},
%%               NamedTable::boolean()) -> EtsTableID::tid()
%% @doc Create a new ETS table from a previously packed table
%%      (see {@link et_pack/1}). `Name' is the name of the table. The `Content'
%%      is the table content (see {@link ets:tab2list/1}). The `Options' list
%%      will be passed to the {@link ets:new/2} function.
%%      If `NamedTable' is `true' than it will create a named table.
%%      If `NamedTable' is `false' than it will create an unnamed table.
%%      If `NamedTable' is `undefined' than `Options' will determine whether
%%      it create a named or an unnamed table.
ets_unpack({Name, Options, Content}, NamedTable) when is_atom(Name),
        is_list(Options), is_list(Content),
        (?UNDEF==NamedTable orelse is_boolean(NamedTable)) ->
    % Modify Options if necesarry
    Options1 = case NamedTable of
        ?UNDEF -> Options;                              % not change
        false  -> Options--[named_table];               % remove named_table
        true   -> Options--[named_table]++[named_table] % add named_table
    end,
    % Create and fill the table
    EtsTableID = ets:new(Name, Options1),
    ?MISC:ets_list2tab(EtsTableID, Content),
    % Return the tabel identifier
    EtsTableID.



