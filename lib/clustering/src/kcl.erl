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

%%% @doc Handle the whole clutering process.
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(kcl).
-vsn("$Rev:  $").

-include("kcl.hrl").


%%% ============================================================================
%%% Imports/exports

%Imports
-import(proplists, [get_value/2, get_value/3]).

% 
-export([master/1, master_file/2, master_default/0, master_validator/0]).



%%% ============================================================================
%%% Handle whole clustering process

%%
%%
master_default() ->
    [{ent_types,    [function]},
     {attr_types,   [function, record, macro]},
     {optimaze_ids, false},
     {alg_opts,     []}].


%%
%%
master_validator() ->
    [{load_state,                   fun is_list/1},
     {load_files, 
        fun(L) -> is_list(L) andalso lists:all(fun is_list/1, L) end},
     {ent_types, 
        fun(L) -> is_list(L) andalso lists:all(fun is_atom/1, L) end},
     {attr_types, 
        fun(L) -> is_list(L) andalso lists:all(fun is_atom/1, L) end},
     {dep_filer_modules, 
        fun(L) -> is_list(L) andalso lists:all(fun is_atom/1, L) end},
     {optimaze_ids,                 fun is_boolean/1},
     {init_group_openclose, 
        fun({M,Fs}) when is_atom(M), is_list(Fs) -> 
            lists:member(M, [open,close]) andalso
            lists:all(fun is_list/1,Fs);
        (_) -> false
        end},
     {init_save_state,              fun is_list/1},
     {init_save_attr_matrix,        fun is_list/1},
     {run_alg,                      fun is_atom/1},
     {alg_opts,
        fun(L) -> is_list(L) andalso lists:all(fun is_tuple/1, L) end},
     {afteralg_save_state,          fun is_list/1},
     {afteralg_save_attr_matrix,    fun is_list/1}].


%%
%%
master_file(Options, MasterFilePath) when is_list(Options) ->
    master(?MISC:proplist_merge_def(Options, load_masterfile(MasterFilePath))).


%%
%%
master(Options) when is_list(Options) ->
    Options1  = ?MISC:proplist_merge_def(Options, master_default()),
    case ?MISC:proplist_validate(Options1, master_validator()) of
        [] -> ok;
        Wrongs -> throw(?LocalError(invalid_options, Wrongs))
    end,
    ClSt1 = case get_value(load_state, Options1, []) of
        [] ->
            % Clear current content of database and load source files
            io:format("Clear database ...\n", []),
            clear_files(),
            load_files(get_value(load_files, Options1)),
            io:format("\n", []),
            % Create new state, build attribute matrix, create initial clusters
            ClSt0 = build_state(Options1),
            % Save clustering state and attribute matrix
            save_state(get_value(init_save_state, Options1, []), ClSt0),
            save_attr_matrix(get_value(init_save_attr_matrix, Options1, []),
                             ClSt0),
            ClSt0;
        SavedStatePath -> % Load saved state
            io:format("Load saved state: ~p ...\n", [SavedStatePath]),
            ?ClState:load(SavedStatePath)
    end,
    % Run selected algorithm
    ClSt3 = case get_value(run_alg, Options1, []) of
        []        -> ClSt1;
        AlgModule -> 
            ClSt2 = run_algorithm(AlgModule, Options1, ClSt1),
            % Save clustering state and attribute matrix
            save_state(get_value(afteralg_save_state, Options1, []), ClSt2),
            save_attr_matrix(get_value(afteralg_save_attr_matrix, Options1, []),
                             ClSt2),
            ClSt2
    end,
    % Return the builded state
    io:format("End of clustering operations.\n", []),
    ClSt3.


% Load master file
load_masterfile(MasterFilePath) ->
    case file:open(MasterFilePath, [read]) of
        {ok, Dev} ->
            case io:read(Dev, "") of
                {ok, FileContent} ->
                    file:close(Dev),
                    FileContent;
                eof ->
                    throw(?LocalError(file_error, "End of file"));
                {error, Reason} ->
                    throw(?LocalError(file_error, file:format_error(Reason)))
            end;
        {error, Reason} ->
            throw(?LocalError(file_error, file:format_error(Reason)))
    end.
    
    
% Removes all files from the tool
clear_files() ->
    case ?ESG:path(?ESG:root(), [file]) of
        [] -> 
            ok;
        [File|_] ->
            referl_fileman:drop_file(File),
            clear_files()
    end.


% Load source files into the tool
load_files(Files) ->
    io:format("Load source files:\n", []),
    lists:foldl(
        fun(Path, AccNow) -> 
            io:format("  ~s ... ", [Path]),
            case ?FileMan:add_file(Path) of
                {error, Reason} -> 
                    io:format("Error: ~p\n", [Reason]),
                    throw(?LocalError(load_error, Reason));
                _ -> ok
            end,
            AccNow2 = erlang:now(),
            TDiff = timer:now_diff(AccNow2, AccNow),
            io:format("~p sec\n", [TDiff/1.0e6]),
            AccNow2
        end, 
        erlang:now(),
        Files),
    ok.


% Create new clustering state and build attribute matrix
build_state(Options) ->
    EntTypes         = get_value(ent_types, Options),
    AttrTypes        = get_value(attr_types, Options),
    DepFilterModules = get_value(dep_filer_modules, Options),
    OptimazeIds      = get_value(optimaze_ids, Options),
    % Create new state
    io:format("Create clustering state ...\n", []),
    ClSt0 = ?ClState:new(),
    % Create dependencies: EntTypes -> AttrTypes
    io:format("Define dependencies: ~p -> ~p\n", [EntTypes,AttrTypes]),
    Items = ?ClAttr:query2items(?Graph:root(), 
                                ?ClAttr:ent_node_query_gen(EntTypes)),
    DepFun = ?ClAttr:node_node_dep_gen(EntTypes, AttrTypes),
    % Keep dependencies which are in connection with modules DepFilterModules
    io:format("Define dependency filter by module names: ~p\n", [DepFilterModules]),
    DepFilterFun = fun(Dep) -> 
            ?ClAttr:dep_filter_modname(Dep, DepFilterModules) 
        end,
    % Build attribute matrix, add items
    io:format("Build attribute matrix ... ", []),
    Now1 = erlang:now(),
    ClSt1 = ?ClState:build_attr_matrix(Items, {DepFun,0}, DepFilterFun, 
                                       OptimazeIds, ClSt0),
    RunTDiff1 = timer:now_diff(erlang:now(), Now1),
    io:format("~p sec\n", [RunTDiff1/1.0e6]),
    % Create open/close clusters from modules OCFilePaths, close/open the others
    case get_value(init_group_openclose, Options, []) of
        [] -> ClSt1;
        {OCMode, OCFilePaths} ->
            io:format("Create ~p clusters by files: ~p\n", 
                      [OCMode, OCFilePaths]),
            ?ClGroup:create_clusters(OCMode, OCFilePaths, ClSt1)
    end.


% Save clustering state
save_state([],  _ClSt) -> ok;
save_state(Path, ClSt) ->
    io:format("Save clustering state: ~s ...\n", [Path]),
    ?ClState:save(Path, ClSt).
    
% Save attribute matrix with short names
save_attr_matrix([],  _ClSt) -> ok;
save_attr_matrix(Path, ClSt) ->
    io:format("Save attribute matrix: ~s ...\n", [Path]),
    ?Matrix:save(csv, ClSt#clState.attr_matrix, Path, 
                 ?ClAttr:id2shortname_gen(ClSt)).


% Run clustering algorithm
run_algorithm(AlgModule, Options, ClSt) ->
    AlgOpts = get_value(alg_opts, Options, []),
    io:format("Cteate ~p algorithm with ~p options ...\n", 
              [AlgModule, AlgOpts]),
    AlgSt0 = AlgModule:new(AlgOpts),
    io:format("Prepate to run ...\n", []),
    {ClSt1, AlgSt1} = AlgModule:prepare(ClSt, AlgSt0),
    io:format("Run ...\n\n", []),
    io:format("---------------------------------------\n\n", []),
    RunNow1 = erlang:now(),
    {ClSt2, AlgSt2} = AlgModule:run(ClSt1, AlgSt1),
    RunTDiff = timer:now_diff(erlang:now(), RunNow1),
    io:format("\n---------------------------------------\n\n", []),
    io:format("Runnig time of the ~p algorithm: ~p sec\n", 
              [AlgModule, RunTDiff/1.0e6]),
    io:format("Store results ...\n", []),
    ClSt3 = AlgModule:store(ClSt2, AlgSt2),
    AlgModule:delete(AlgSt2),
    ClSt3.


