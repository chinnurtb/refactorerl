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

%%% @doc Base clustering algorithm.
%%% This module implement of a partial clustering algorithm. This algorith is
%%% the base algorithm of every specified clustering algorithm. It contains
%%% general steps to prepare the environment for execute a specified algorithm.
%%% Functions to clear actual content from the database of the tool, load the
%%% files that contain the source to cluster, create attribute matrix,
%%% create connection matrix or save out the clustering state or a part of it
%%% into files.
%%%
%%% This module doesn't implement a real clustering algorithm. It just
%%% contains some usefull environment preparer step.

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_alg).
-vsn("$Rev: $").

-include("ucluster.hrl").
-include("ucl_options.hrl").



%%% ============================================================================
%%% Import/Export

% Error messages
-export([error_text/2]).

% Callback fuctions
-export([empty_default/0, empty_validator/0, identity/2,
         get_state_default/0, get_state_validator/0, get_state/2,
         save_validator/0, save/2,
         prepare_db_validator/0, prepare_db/2,
         load_files_validator/0, load_files/2,
         load_attr_matrix_default/0, load_attr_matrix_validator/0,
         load_attr_matrix/2,
         build_attr_matrix_default/0, build_attr_matrix_validator/0,
         build_attr_matrix/2,
         build_conn_matrix_default/0, build_conn_matrix_validator/0,
         build_conn_matrix/2]).



%%% ============================================================================
%%% Error messages

%% @private
%% @spec error_text(Type::atom(), Details::[term()]) -> string()
%% @doc Give back the error message text for the transformation specific errors.
%%      The error is specified by the `Type' and the `Details'.
error_text(load_error, {Path,Reason}) ->
    ?MISC:format("An error occured while load the ~s file (~p).",
                 [Path,Reason]);
error_text(missing_matrix, _) ->
    ?MISC:format("Matrix source is not specified. The \'filepath\' or "
                 "\'matrix\' option is required.", []);
error_text(invalid_ent_attr_options, _) ->
    ?MISC:format("Both of \'ent_types\', \'attr_types\' or \'ent_fun\', "
                 "\'dep_fun\' options are required.", []);
error_text(no_def_dependency_value, _) ->
    ?MISC:format("Default dependency value (\'dep_def_value\' option) is not "
                 "defied.", []);
error_text(no_def_connection_value, _) ->
    ?MISC:format("Default connection value (\'conn_def_value\' option) is not "
                 "defied.", []).



%%% ============================================================================
%%% Callback functions

%% @spec empty_default() -> []
%% @doc  Return an empty property list for the algorithm step functions which
%%       have no default functions defined.
empty_default() -> [].

%% @spec empty_validator() -> []
%% @doc  Return an empty validtaor list for the algorithm step functions which
%%       have no validator functions defined.
empty_validator() -> [].

%% @spec identity(State::clState(), Options::proplist()) ->
%%           {State::clState(), []}
%% @doc Empty algorithm step. It doesn't do anything just return the given
%%      `State'.
identity(St, _Options) ->
    {St, []}.


%% @spec get_state_default() -> DefaultOptions::proplist()
%% @doc Return the default options of the {@link get_state/2} function.
%%      For the avaible options see the {@link get_state_validator/0} function.
%% @see get_state_validator/0
get_state_default() ->
    [{from, new}].

%% @spec get_state_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the
%%       {@link get_state/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`from' = new | string(): The source of the clustering state.
%%     If it is `new' generate a new state. If it is a file path load the state
%%     from the file.</li>
%% </ul>
get_state_validator() ->
    [{from, fun(V) -> new==V orelse is_list(V) end}].

%% @spec get_state(State::clState(), Options::proplist()) ->
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Return a clustering state. Depending on the `from' option it generate
%%      a new state or load one from a file.
%%
%%      For the avaible options see {@link get_state_default/0} and
%%      {@link get_state_validator/0} functions.
%% @see get_state_default/0
%% @see get_state_validator/0
get_state(_St, Options) ->
    St =
        case get_value(from, Options) of
            new      -> ?ClState:new();
            FilePath -> ?ClState:load(FilePath)
        end,
    % Return state and steps
    {St, []}.


%% @spec save_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the
%%       {@link save/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`state' = string(): A save file path of the clustering state.</li>
%%   <li>`attr_matrix' = string(): A save file path of the attribute matrix.
%%      The matrix will be saved out in CSV format.</li>
%%   <li>`ent_conn_matrix' = string(): A save file path of the entity
%%      connection matrix. The matrix will be saved out in CSV format.</li>
%%   <li>`attr_conn_matrix' = string(): A save file path of the attribute
%%      connection matrix. The matrix will be saved out in CSV format.</li>
%% </ul>
save_validator() ->
    [{state,            fun(V) -> is_list(V) end},
     {attr_matrix,      fun(V) -> is_list(V) end},
     {ent_conn_matrix,  fun(V) -> is_list(V) end},
     {attr_conn_matrix, fun(V) -> is_list(V) end}].

%% @spec save(State::clState(), Options::proplist()) ->
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Save out into a file the clustering state or a part of that.
%%
%%      For the avaible options see {@link save_default/0} and
%%      {@link save_validator/0} functions.
%% @see save_default/0
%% @see save_validator/0
save(St=#clState{}, Options) ->
    MatrixSaveOpts = [{format,csv},{id2NameFun,?ClAttr:id2shortname_gen(St)}],
    lists:foreach(
        fun({Key, SaveFun}) ->
            case get_value(Key, Options, ?UNDEF) of
                ?UNDEF    -> ok;
                FilePath  -> SaveFun(St, FilePath)
            end
        end,
        [{state,
            fun(St1=#clState{}, FilePath) ->
                ?ClState:save(St1, FilePath)
            end},
         {attr_matrix,
            fun(#clState{attr_matrix=AM}, FilePath) ->
                ?Matrix:save(AM, FilePath, MatrixSaveOpts)
            end},
         {ent_conn_matrix,
            fun(#clState{ent_conn_matrix=CM}, FilePath) ->
                ?Matrix:save(CM, FilePath, MatrixSaveOpts)
            end},
         {attr_conn_matrix,
            fun(#clState{attr_conn_matrix=CM}, FilePath) ->
                ?Matrix:save(CM, FilePath, MatrixSaveOpts)
            end}]),
    % Return state and steps
    {St, []}.


%% @spec prepare_db_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the
%%       {@link prepare_db/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`reset' = any(): The value is not used. Reset the database.
%%     Actually it is not allowed because this case is not finished!</li>
%%   <li>`restore' = positive(): A positive integer. The identifier of a
%%     backup of the ESG. Restore the graph to this backup.</li>
%%   <li>`clear_files' = all | [FilePath::string()]: Clear the files from
%%     the tool.</li>
%% </ul>
prepare_db_validator() ->
    [%{reset,        fun(_) -> true end},
     {restore,      fun(V) -> is_integer(V) andalso 0<V end},
     {clear_files,  fun(V) -> all==V orelse ?ClOpts:is_listOfLists(V) end}].

%% @spec prepare_db(State::clState(), Options::proplist()) ->
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Prepare the database of tool to the clustering process. First restore
%%      a backup of the ESG. Next clear files from the graph.
%%
%%      For the avaible options see {@link prepare_db_default/0} and
%%      {@link prepare_db_validator/0} functions.
%% @see prepare_db_default/0
%% @see prepare_db_validator/0
prepare_db(St, Options) ->
    case get_value(reset, Options, ?UNDEF) of
        ?UNDEF ->
            case get_value(restore, Options, ?UNDEF) of
                ?UNDEF    -> ok;
                BackupId  -> ?Graph:restore(BackupId)
            end,
            case get_value(clear_files, Options, ?UNDEF) of
                ?UNDEF    -> ok;
                all       -> clear_files();
                FilePaths -> clear_files(FilePaths)
            end;
        _ -> ?Graph:reset_schema()
    end,
    % Return state and steps
    {St, []}.

% Removes all files from the database
clear_files() ->
    case ?Query:exec([file]) of
        [] ->
            ok;
        [File|_] ->
            ?FileMan:drop_file(File),
            clear_files()
    end.

% Remove selected files
clear_files([FilePath|FilePaths]) ->
    case ?Query:exec(?File:find(FilePath)) of
        [] ->
            ok;
        [File|_] ->
            ?FileMan:drop_file(File),
            clear_files(FilePaths)
    end.


%% @spec load_files_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the
%%       {@link load_files/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`files' = [string()]: Load source files specified in this list.</li>
%% </ul>
load_files_validator() ->
    [{files, fun(V) -> ?ClOpts:is_listOfLists(V) end}].

%% @spec load_files(State::clState(), Options::proplist()) ->
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Load source files into the tool.
%%
%%      For the avaible options see {@link load_files_default/0} and
%%      {@link load_files_validator/0} functions.
%% @see load_files_default/0
%% @see load_files_validator/0
load_files(St, Options) ->
    lists:foreach(
        fun(Path) ->
            case ?FileMan:add_file(Path) of
                {error, Reason} -> throw(?LocalError(load_error,{Path,Reason}));
                _               -> ok
            end
        end,
        get_value(files, Options, [])),
    {St, []}.


%% @spec load_attr_matrix_default() -> DefaultOptions::proplist()
%% @doc Return the default options of the {@link load_attr_matrix/2} function.
%%      For the avaible options see the {@link load_attr_matrix_validator/0}
%%      function.
%% @see load_attr_matrix_validator/0
load_attr_matrix_default() ->
    [{box_fun, {?ClAttr,node2item}}].

%% @spec load_attr_matrix_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the
%%       {@link load_attr_matrix/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`filepath' = string(): The path of the file which contain a saved
%%     attribute matrix in packed format. The items of the clustering
%%     will be generated from the row and column labels by the boxing function
%%     given in the `box_fun' option.</li>
%%   <li>`matrix' = matrix(): An attribute matrix. The items of the clustering
%%     will be generated from the row and column labels by the boxing function
%%     given in the `box_fun' option.</li>
%%   <li>`box_fun' = ((term()) -> clItem()): A boxing function. The items of the
%%     clustering will be generated from the row and column labels of the
%%     attribute matrix by this function.</li>
%% </ul>
%% @see ucl_state:load_attr_matrix/3
load_attr_matrix_validator() ->
    [{filepath, fun(V) -> is_list(V) end},
     {matrix,   fun(V) -> ?Matrix:is_matrix(V) end},
     {box_fun,  fun(V) -> ?ClOpts:is_fun_result(V,1) end}].

%% @spec load_attr_matrix(State::clState(), Options::proplist()) ->
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Load the attribute matrix from the specified soure into the `State'
%%      clustering state. Actual attribute matrix and all items will be
%%      erased from `State'. The new items will be generated from row and
%%      column headers of the attribute matrix using the boxing function.
%%      For the details see the {@link ucl_state:load_attr_matrix/3} function.
%%
%%      For the avaible options see {@link load_attr_matrix_default/0} and
%%      {@link load_attr_matrix_validator/0} functions.
%% @see load_attr_matrix_default/0
%% @see load_attr_matrix_validator/0
load_attr_matrix(St=#clState{}, Options) ->
    % Get the matrix
    Matrix0 = case get_value(filepath, Options, ?UNDEF) of
        ?UNDEF   -> get_value(matrix, Options, ?UNDEF);
        FilePath -> ?Matrix:load(FilePath)
    end,
    Matrix = if
        ?UNDEF==Matrix0 -> throw(?LocalError(missing_matrix, []));
        true            -> Matrix0
    end,
    % Load the matrix
    BoxFun = ?ClOpts:get_fun(get_value(box_fun, Options)),
    St2 = ?ClState:load_attr_matrix(Matrix, BoxFun, St),
    % Return new state
    {St2, []}.


%% @spec build_attr_matrix_default() -> DefaultOptions::proplist()
%% @doc Return the default options of the {@link build_attr_matrix/2} function.
%%      For the avaible options see the {@link build_attr_matrix_validator/0}
%%      function.
%% @see build_attr_matrix_validator/0
build_attr_matrix_default() ->
    [{box_fun,        {?ClAttr,node2item}},
     {dep_filter_fun, {?ClOpts,true}}].

%% @spec build_attr_matrix_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the
%%       {@link build_attr_matrix/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`ent_types' = [atom()]: The type of entities.
%%     The `attr_type' and `box_fun' options are also required.</li>
%%   <li>`attr_types' = [atom()]: The type of attributes.
%%     The `ent_type' and `box_fun' options are also required.</li>
%%   <li>`box_fun' = fun_result(1) with ((term()) -> clItem()) result: A boxing
%%     function with the. The items of the clustering will be generated from the
%%     entities and attributes by this function.</li>
%%   <li>`ent_fun' = fun_result(0) with (() -> clItem()): A function which
%%     generates the entity items of the clustering process.</li>
%%     The `attr_fun' options is also required.
%%   <li>`dep_fun' = fun_result(1) with ((EntItem::clItem()) ->
%%     [{EntItem::clItem(), DepItem::clItem(), DepWeight::term()}]) result:
%%     A function which generates the dependencies of the entity items. The
%%     give an entity item and return the dependency list of the entity.
%%     A dependency is a 3-tuple which contain the entity item, the dependend
%%     item (attribute) and the weight of the dependency.
%%     The `ent_fun' options is also required.</li>
%%   <li>`dep_def_value' = term(): The default dependency weight. It used
%%     between entities and attributes which are not in connection.</li>
%%   <li>`dep_filter_fun' = fun_result(1) with
%%     (({EntItem::clItem(), DepItem::clItem(), DepWeight::term()}) ->
%%     boolean()) result: A dependency filter function. The generated
%%     dependencies will be filtered by this function before the attribute marix
%%     will be built.</li>
%% </ul>
build_attr_matrix_validator() ->
    [{ent_types,      fun(V) -> ?ClOpts:is_listOfAtoms(V) end},
     {attr_types,     fun(V) -> ?ClOpts:is_listOfAtoms(V) end},
     {box_fun,        fun(V) -> ?ClOpts:is_fun_result(V,1) end},
     {ent_fun,        fun(V) -> ?ClOpts:is_fun_result(V,0) end},
     {dep_fun,        fun(V) -> ?ClOpts:is_fun_result(V,1) end},
     {dep_def_value,  fun(_) -> true end},
     {dep_filter_fun, fun(V) -> ?ClOpts:is_fun_result(V,1) end}].

%% @spec build_attr_matrix(State::clState(), Options::proplist()) ->
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Build the attribute matrix.
%%      For the details see the {@link ucl_state:bult_attr_matrix/4} function.
%%
%%      For the avaible options see {@link build_attr_matrix_default/0} and
%%      {@link build_attr_matrix_validator/0} functions.
%% @see build_attr_matrix_default/0
%% @see build_attr_matrix_validator/0
build_attr_matrix(St=#clState{}, Options) ->
    EntTypes  = get_value(ent_types,  Options, ?UNDEF),
    AttrTypes = get_value(attr_types, Options, ?UNDEF),
    EntFun0   = get_value(ent_fun, Options, ?UNDEF),
    DepFun0   = get_value(dep_fun, Options, ?UNDEF),
    Types = (?UNDEF/=EntTypes andalso ?UNDEF/=AttrTypes),
    Funs  = (?UNDEF/=EntFun0  andalso ?UNDEF/=DepFun0),
    % Create entity and dependency functions
    {EntFun, DepFun} = if
        Types ->
            BoxFun = ?ClOpts:get_fun(get_value(box_fun, Options), 1),
            EF = ?ClAttr:ent_node_gen(EntTypes,BoxFun),
            DF = ?ClAttr:node_node_dep_gen(EntTypes,AttrTypes,BoxFun),
            {EF, DF};
        Funs ->
            EF = ?ClOpts:get_fun(EntFun0, 0),
            DF = ?ClOpts:get_fun(DepFun0, 1),
            {EF, DF};
        true ->
            throw(?LocalError(invalid_ent_attr_options, []))
    end,
    % Determine default dependency value
    DefValue = case get_value(dep_def_value, Options, ?UNDEF) of
        ?UNDEF -> throw(?LocalError(no_def_dependency_value, []));
        DefVal -> DefVal
    end,
    % Build the attribute matrix
    DepFilterFun = ?ClOpts:get_fun(get_value(dep_filter_fun,Options), 1),
    St2 = ?ClState:build_attr_matrix(EntFun,{DepFun,DefValue},DepFilterFun,St),
    % Return new state
    {St2, []}.


%% @spec build_conn_matrix_default() -> DefaultOptions::proplist()
%% @doc Return the default options of the {@link build_conn_matrix/2} function.
%%      For the avaible options see the {@link build_conn_matrix_validator/0}
%%      function.
%% @see build_conn_matrix_validator/0
build_conn_matrix_default() ->
    [{matrix,    entity},
     {conn_fun,  {?ClConn,common_attr_cnt}},
     {symmetric, false}].

%% @spec build_conn_matrix_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the
%%       {@link build_conn_matrix/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`matrix' = entity | attribute: Select the connection matrix. There are
%%     two matrix the entity connection matrix and the attribute connection
%%     matrix.</li>
%%   <li>`conn_fun' = fun_result(3) with
%%     ((Item1::clItem(), Item2::clItem, AttrMatrix::matrix()) ->
%%     {def, any()} | {val, ConnValue::term()}) result:
%%     The connection function. It calculate the connection value between two
%%     item. If the connection is the default return a `{def,_}' tuple and the
%%     default connection value will be used. If the connection is not the
%%     default return a `{val, ConnValue}' tuple and the `ConnValue' will be
%%     used as the value of the connection.</li>
%%   <li>`conn_def_value' = term(): The default connection value.</li>
%%   <li>`symmetric' = boolean(): The connection matrix is symmetric or isn't.
%%     </li>
%% </ul>
build_conn_matrix_validator() ->
    [{matrix,          fun(V) -> lists:member(V, [entity,attribute]) end},
     {conn_fun,        fun(V) -> ?ClOpts:is_fun_result(V,3) end},
     {conn_def_value,  fun(_) -> true end},
     {symmetric,       fun(V) -> is_boolean(V) end}].

%% @spec build_conn_matrix(State::clState(), Options::proplist()) ->
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Create the entity/attribute connection matrix.
%%      For the details see the {@link ucl_state:bult_conn_matrix/5} function.
%%      For the avaible options see {@link build_conn_matrix_default/0} and
%%      {@link build_conn_matrix_validator/0} functions.
%% @see build_conn_matrix_default/0
%% @see build_conn_matrix_validator/0
build_conn_matrix(St=#clState{}, Options) ->
    % Get connection fun with, default connection value, is the matrix symmetric
    Matrix  = get_value(matrix,Options),
    ConnFun = ?ClOpts:get_fun(get_value(conn_fun,Options), 3),
    DefValue = case get_value(conn_def_value, Options, ?UNDEF) of
        ?UNDEF -> throw(?LocalError(no_def_connection_value, []));
        DF     -> DF
    end,
    Sym = get_value(symmetric,Options),
    % Calculate connection matrix
    St2 = ?ClState:build_conn_matrix(Matrix, ConnFun,DefValue, Sym, St),
    % Return new state
    {St2, []}.



