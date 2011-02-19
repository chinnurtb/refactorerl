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

%%% @doc This module contains the functionality which should be available to
%%% a user interface. Operations are asynchronous, results are returned by a
%%% gen_event manager.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_ui).
-vsn("$Rev: 1479$ ").
-behaviour(gen_server).

%%% Client exports
-export([stop/0, status/1, add/1, drop/1, draw/2,
         showconfig/0, saveconfig/3, filelist/0, reset/0]).

%%% Transformations
-export([extract/4, inline/2]).

-export([merge/4]).

-export([funexpr/2, reorder/3, generalize/4, elimvar/2, tuplefunpar/3]).

-export([funlist/1, movefun/3]).

-export([recordlist/1, moverec/3]).

%% Renamings
-export([renamemod/3,renamefun/3, renamevar/3, renamerec/3, renamefield/3]).

%%% Undo/redo
-export([backup/0, undo/1, clean/0]).

%%% Clustering
-export([cl_options/1, run_cl/3, cl_refresh/0]).

%% UI message handlers
-export([message/2, message/3, add_msg_handler/2, del_msg_handler/2]).

%%% Server exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("refactorerl.hrl").


%% @spec stop() -> ok
%% @doc Stops the RefactorErl server.
stop() -> cast({stop}).

%% @spec status(string()) -> ok
%%
%% @doc Sends a message about the status of the file.
%% The type of the message contains the status: it can be `add', `invalid' or
%% `drop'. The text of the message is the name of the file.
status(File) -> cast({status, File}).

%% @spec add(string()) -> ok
%%
%% @doc Adds the given file to the database.
add(File) -> cast({add, File}).

%% @spec drop(string()) -> ok
%%
%% @doc Drops the given file from the database.
drop(File) -> cast({drop, File}).

%% @spec draw(string(), integer()) -> ok
%%
%% @doc Draws the graph of the given file.
%% The type of the graph is specified in the `Type' argument.
draw(File, Type) -> cast({draw, File, Type}).

%% @spec showconfig() -> ok
%%
%% @doc Sends a message that contains the configuration of the server.
showconfig() -> cast({showconfig}).

%% @spec saveconfig([string()], [string()], string() | original) -> ok
%%
%% @doc Sets the server's saving configuration.
saveconfig(App, Inc, Out) -> cast({saveconfig, App, Inc, Out}).

%% @spec filelist() -> ok
%%
%% @doc Sends a message about each file that is stored in the database.
%% The type of the messages will be `filelist'.
filelist() -> cast({filelist}).

%% @spec reset() -> ok
%%
%% @doc Resets the database.
reset() -> cast({reset}).

%% @spec extract(File::string(), Begin::integer(), End::integer(),
%%               Name::string()) -> ok
%%
%% @doc Calls the extract function refactoring.
extract(File, Begin, End, Name) -> cast({extract, File, Begin, End, Name}).

%% @spec merge(File::string(), Begin::integer(), End::integer(),
%%             Name::string()) -> ok
%%
%% @doc Calls the merge expression refactoring.
merge(File, Begin, End, Name) -> cast({merge, File, Begin, End, Name}).

%% @spec inline(File::string(), Pos::integer()) -> ok
%%
%% @doc Calls the inline function refactoring.
inline(File, Pos) -> cast({inline, File, Pos}).

%% @spec funexpr(File::string(), Pos::integer()) -> ok
%%
%% @doc Calls the expand fun expression refactoring.
funexpr(File, Pos ) -> cast({funexpr, File, Pos}).

%% @spec generalize(File::string(), Begin::integer(), End::integer(),
%%                  Name::string()) -> ok
%%
%% @doc Calls the generalize function refactoring.
generalize(File, Begin, End, Name) ->
    cast({generalize, File, Begin, End, Name}).

%% @spec elimvar(File::string(), Pos::integer()) -> ok
%%
%% @doc Calls the eliminate variable refactoring.
elimvar(File, Pos) -> cast({elimvar, File, Pos}).

%% @spec tuplefunpar(File::string(), Begin::integer(), End::integer()) -> ok
%%
%% @doc Calls the tuple function arguments refactoring.
tuplefunpar(File, Begin, End) -> cast({tuplefunpar, File, Begin, End}).

%% @spec reorder(File::string(), Pos::integer(), Order::[integer()]) -> ok
%%
%% @doc Calls the reorder function arguments refactoring.
reorder(File, Pos, Order) -> cast({reorder, File, Pos, Order}).

%% @spec renamefun(File::string(), Name::string(), Pos::integer()) -> ok
%%
%% @doc Calls the rename function refactoring.
renamefun(File, Name, Pos) -> cast({renamefun, File, Name, Pos}).

%% @spec renamevar(File::string(), Name::string(), Pos::integer()) -> ok
%%
%% @doc Calls the rename variable refactoring.
renamevar(File, Name, Pos) -> cast({renamevar, File, Name, Pos}).

%% @spec renamemod(File::string(), Name::string(), Pos::integer()) -> ok
%%
%% @doc Calls the rename module refactoring.
renamemod(File, Name, Pos) -> cast({renamemod, File, Name, Pos}).

%% @spec renamerec(File::string(), Name::string(), Pos::integer()) -> ok
%%
%% @doc Calls the rename record refactoring.
renamerec(File, Name, Pos) -> cast({renamerec, File, Name, Pos}).

%% @spec renamefield(File::string(), Name::string(), Pos::integer()) -> ok
%%
%% @doc Calls the rename record field refactoring.
renamefield(File, Name, Pos) -> cast({renamefield, File, Name, Pos}).

%% @private
funlist(File) -> cast({funlist, File}).

%% @spec movefun(Source::string(), Target::string(), Funs::[string()]) -> ok
%%
%% @doc Calls the move function refactoring.
movefun(Source, Target, Funs)     -> cast({movefun, Source, Target, Funs}).

%% @private
recordlist(File)             -> cast({recordlist, File}).

%% @spec moverec(Source::string(), Target::string(), Funs::[string()]) -> ok
%%
%% @doc Calls the move record refactoring.
moverec(Source, Target, Recs)     -> cast({moverec, Source, Target, Recs}).

%% @spec run_cl(proplist(), atom(), atom()) -> ok
%%
%% @doc Invokes {@link cl_ui:run/1}. See the options there.
run_cl(Opt, Alg, Create)     -> cast({run_cl, Opt, Alg, Create}).

%% @spec cl_options(atom()) -> ok
%%
%% @doc Sends a message with the options of the given clustering algorithm.
cl_options(Alg)              -> cast({cl_options, Alg}).

%% @spec cl_refresh() -> ok
%%
%% @doc Refreshes the Emacs clustering interface.
cl_refresh()                 -> cast({cl_refresh}).

%% @spec backup() -> ok
%%
%% @doc Backups tables of Refactorerl database.
backup()                  -> cast({backup}).

%% @spec undo(string()) -> ok
%%
%% @doc Steps backward on the Refactorerl database.
undo(RFile)               -> cast({undo, RFile}).

%% @spec clean() -> ok
%%
%% @doc Clean backup files from the Refactorerl database.
clean()                   -> cast({clean}).
%redo(RFile)               -> cast({redo, RFile}).

cast(Data) -> gen_server:cast({?MODULE, ?REFERL_NODE}, Data).


%% @spec message(atom(), [term()]) -> ok
%% @doc Sends a message of `Type' which contains an arbitrary list of terms
%% to the registered user interface message handlers.
message(Type, Data) ->
    gen_event:notify({?UI_MSG_SERVER, ?REFERL_NODE}, {Type, Data}).

%% @spec message(atom(), string(), [term()]) -> ok
%% @doc Sends a message of `Type' with arguments formatted by `io_lib:format'
%% to the registered user interface message handlers.
message(Type, Format, Args) ->
    gen_event:notify({?UI_MSG_SERVER, ?REFERL_NODE},
                     {Type, ?MISC:format(Format, Args)}).

%% @spec add_msg_handler(atom(), term()) -> ok | term()
%% @doc Registers a new user interface message handler.
add_msg_handler(Handler, Args) ->
    gen_event:add_sup_handler({?UI_MSG_SERVER, ?REFERL_NODE}, Handler, Args).

%% @spec del_msg_handler(atom(), term()) -> ok | term()
%% @doc Deletes a registered user interface message handler.
del_msg_handler(Handler, Args) ->
    gen_event:delete_handler({?UI_MSG_SERVER, ?REFERL_NODE}, Handler, Args).


%% @doc Starts the server and links it with the calling process.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init(_) ->
    {ok, ok}.

%% @private
handle_cast(Cmd, S) when is_tuple(Cmd) ->
    [CmdName | Args] = tuple_to_list(Cmd),
    Handler = case CmdName of
                  stop        -> fun handle_stop/0;
                  status      -> fun handle_status/1;
                  add         -> fun handle_add/1;
                  drop        -> fun handle_drop/1;
                  draw        -> fun handle_draw/2;
                  showconfig  -> fun handle_showconfig/0;
                  saveconfig  -> fun handle_saveconfig/3;
                  reset       -> fun handle_reset/0;
                  echo        -> fun handle_echo/2;

                  extract     -> fun handle_extract/4;
                  merge       -> fun handle_merge/4;
                  inline      -> fun handle_inline/2;
                  funexpr     -> fun handle_funexpr/2;
                  generalize  -> fun handle_generalize/4;
                  elimvar     -> fun handle_elimvar/2;
                  tuplefunpar -> fun handle_tuplefunpar/3;
                  reorder     -> fun handle_reorder/3;

                  renamefun   -> fun handle_renamefun/3;
                  renamevar   -> fun handle_renamevar/3;
                  renamemod   -> fun handle_renamemod/3;
                  renamerec   -> fun handle_renamerec/3;
                  renamefield -> fun handle_renamefield/3;

                  filelist    -> fun handle_filelist/0;
                  funlist     -> fun handle_funlist/1;
                  movefun     -> fun handle_movefun/3;
                  recordlist  -> fun handle_recordlist/1;
                  moverec     -> fun handle_moverec/3;

                  backup      -> fun handle_backup/0;
                  undo        -> fun handle_undo/1;
                  clean       -> fun handle_clean/0;

                  run_cl      -> fun handle_run_cl/3;
                  cl_options  -> fun handle_cl_options/1;
                  cl_refresh  -> fun handle_cl_refresh/0;
                  _           -> error
              end,
    try
        apply(Handler, Args)
    catch
        error:Err ->
            error_logger:error_msg("Bad request ~p~n"
                                   "  Error: ~p~n"
                                   "  Stack: ~p~n",
                                   [Cmd, Err, erlang:get_stacktrace()]),
            message(error, "error handling request ~512P", [Cmd,4])
    end,
    {noreply, S}.

%% @private
handle_call(_,_,S) ->
    {stop, bad_call, S}.

%% @private
handle_info(_, S) ->
    {noreply, S}.

%% @private
terminate(_,_) ->
    ok.

%% @private
code_change(_,S,_) ->
    {ok, S}.

%%% ============================================================================
%%% Standard UI

handle_stop() ->
    message(status, "RefactorErl server is shutting down...",[]),
    init:stop().

handle_status(File) when is_list(File) ->
    case ?SYNTAX:file(File) of
        {file, F} ->
            case ?GRAPH:path(F, [{form, {type,'==',error}}]) of
                [] -> message(add, "~s", [File]);
                _  -> message(invalid, "~s", [File])
            end;
        not_found  -> message(drop, "~s", [File])
    end.

handle_add(Name) when is_list(Name) ->
    case ?SYNTAX:file(Name) of
        {file, F} ->
            Drop = [(?GRAPH:data(FN))#file.path ||
                       FN <- ?GRAPH:path(F, [{incl, back}])],
            ?FILEMAN:drop_file(F);
        _ ->
            Drop = []
    end,
    case ?FILEMAN:add_file(Name) of
        {error, Reason} ->
            [message(drop, "~s", [DF]) || DF <- Drop],
            message(invalid, "~s", [Name]),
            message(status, "<~s> ~s", [Name, error_message(Reason)]);
        {file, File} ->
            Add = [(?GRAPH:data(FN))#file.path ||
                      FN <- ?GRAPH:path(File, [incl])],
            [message(drop, "~s", [DF]) || DF <- Drop],
            [message(add, "~s", [AF]) || AF <- Add]
    end.

handle_drop(File) when is_list(File) ->
    case ?SYNTAX:file(File) of
        {file, F} ->
            Drop = [(?GRAPH:data(FN))#file.path ||
                       FN <- ?GRAPH:path(F, [{incl, back}])],
            ?FILEMAN:drop_file(F),
            [message(drop, "~s", [DF]) || DF <- Drop];
        not_found ->
            message(status, "Already dropped", [])
    end.

handle_draw(File, Type) ->
    Filt = case Type of
               2 -> sem;
               3 -> ctx;
               4 -> syn;
               5 -> synlex;
               6 -> lex;
               7 -> inp;
               8 -> not_lex;
               _ -> all
           end,
    ?DRAW_GRAPH:draw_graph(File, Filt),
    message(status, "Ok", []).

handle_showconfig() ->
    message(showconfig,
            [{Name, Value} ||
                Env <- ?GRAPH:path(?GRAPH:root(), [env]),
                #env{name=Name, value=Value} <- [?GRAPH:data(Env)]]).

handle_saveconfig(AppDirs, IncDirs, OutDir) ->
    [?GRAPH:delete(Env) || Env <- ?GRAPH:path(?GRAPH:root(), [env])],
    [?GRAPH:mklink(?GRAPH:root(), env,
                   ?GRAPH:create(#env{name=appbase, value=Dir})) ||
                      Dir <- AppDirs],
    [?GRAPH:mklink(?GRAPH:root(), env,
                   ?GRAPH:create(#env{name=include, value=Dir})) ||
                      Dir <- IncDirs],
    ?GRAPH:mklink(?GRAPH:root(), env,
                  ?GRAPH:create(#env{name=output, value=OutDir})),
    message(status, "Configuration saved.", []).

handle_filelist() ->
    Files = [ {(?GRAPH:data(F))#file.path, F} ||
                F <- ?GRAPH:path(?GRAPH:root(), [file])],
    St = fun(N) -> case ?GRAPH:path(N, [{form, {type,'==',error}}]) of
                       [] -> ""; _ -> "!" end
         end,
    [message(filelist, "~s~s",[St(N),F]) || {F,N}<-lists:sort(Files)].

handle_reset() ->
    ?GRAPH:reset_schema(),
    message(status, "Database clear started.", []).

handle_echo(Type, String) ->
    message(Type, "~s~n", [String]).

%% @private
error_message({no_include_file, File}) ->
    ?MISC:format("Include file \"~s\" not found", [File]);
error_message(Error) ->
    ?MISC:format("Error: ~p", [Error]).

%%% ============================================================================
%%% Transformation interface

transform_with_backup(TrModName, Args) ->
    try ?GRAPH:backup(),
        ?TRANSFORM:do(TrModName, Args)
    catch
        throw:Msg ->
            message(error, "Error: ~p", [Msg])
    end.

handle_extract(File, Beg, End, NewName) ->
    transform_with_backup(referl_tr_extract_fun, {File, NewName, Beg, End-1}).

handle_merge(File, Beg, End, NewName) ->
    transform_with_backup(referl_tr_merge, {File, Beg, End-1, NewName}).

handle_tuplefunpar(File, Beg, End) ->
    transform_with_backup(referl_tr_tuple_funpar,{file_pos,File, Beg, End-1}).

handle_reorder(ModName, Pos, Order)->
    transform_with_backup(referl_tr_reorder_funpar, {ModName, Pos, Order}).

handle_inline(File, Pos) ->
    transform_with_backup(referl_tr_inline_fun, {File, Pos}).

handle_funexpr(File, Pos) ->
    transform_with_backup(referl_tr_expand_funexpr, {File, Pos}).

handle_generalize(File, Beg, End, NewName) ->
    transform_with_backup(referl_tr_gen, {File, NewName, Beg, End-1}).

handle_elimvar(File, Pos) ->
    transform_with_backup(referl_tr_elim_var, {File, Pos}).

%%% ----------------------------------------------------------------------------
%%% Renamings

handle_renamemod(File, Name, Pos) ->
    try ?TRANSFORM:do(referl_tr_rename_mod,{File, Name, Pos}) of
        {ok, FileNodes} ->
            FilePath = (?GRAPH:data(hd(FileNodes)))#file.path,
            message(setbuffer, "~s", [FilePath]),
            message(reload, "~s", [FilePath]);
        _ ->
            ok %%message(status, "Unsuccessful", [])
    catch
        throw:Msg ->
            message(error, "Error: ~p", [Msg])
    end.

handle_renamefun(File, NewName, Pos) ->
    transform_with_backup(referl_tr_rename_fun, {pos, {File, Pos, NewName}}).

handle_renamevar(File, NewName, Pos) ->
    transform_with_backup(referl_tr_rename_var, {File, NewName, Pos}).

handle_renamerec(File, NewName, Pos) ->
    transform_with_backup(referl_tr_rename_rec, {pos, File, Pos, NewName}).

handle_renamefield(File, NewName, Pos) ->
    transform_with_backup(referl_tr_rename_recfield, {pos, File, Pos, NewName}).

%%% ----------------------------------------------------------------------------
%%% Movings

handle_funlist(File) ->
    case ?SYNTAX:file(File) of
        not_found ->
            message(funlist, []),
            message(status, "File not found (~s)", [File]);
        {file, F} ->
            message(funlist,
                    [{Name, Arity} ||
                        Fun <- ?GRAPH:path(F, [{form, {type, '==', func}},
                                               fundef]),
                        #func{name=Name, arity=Arity} <- [?GRAPH:data(Fun)]])
    end.

handle_movefun(Source, Target, Funs) ->
    case ?SYNTAX:file(Source) of
        not_found ->
            message(status, "File not found (~s)", [Source]);
        {file, File} ->
            case ?GRAPH:path(File, [moddef]) of
                [] ->
                    message(status, "The source file is not a module", []);
                [Mod] ->
                    transform_with_backup(
                        referl_tr_move_fun,
                        {(?GRAPH:data(Mod))#module.name, Funs, Target})
            end
    end.

handle_recordlist(File) ->
    case ?SYNTAX:file(File) of
        not_found ->
            message(recordlist, []),
            message(status, "File not found (~s)", [File]);
        {file, F} ->
            message(recordlist,
                    [Name ||
                        Record <- ?GRAPH:path(F, [record]),
                        #record{name=Name} <- [?GRAPH:data(Record)]])
    end.

handle_moverec(Source, Target, Records) ->
    TargetFile = 
        case filename:pathtype(Target) of
            absolute ->
                Target;
            relative ->
                SourceDir = filename:dirname(filename:absname(Source)),
                %%filename:absname_join(SourceDir, Target);
                filename:absname(Target, SourceDir);
            volumerelative ->
                todo
        end,
    transform_with_backup(referl_tr_move_rec, {Source, Records, TargetFile}).

%%% ============================================================================
%%% Clustering

%% Sending options for the emacs interface
handle_cl_options(Alg)->
    OptList = cl_ui:cl_options(Alg),
    Ls = [[A,B] || {A,B} <- OptList],
    message(options, [Alg]++Ls).

handle_cl_refresh()->
    case cl_ui:refresh() of
      {cl_ui, _} -> message(status, "cl_ui refresh: cleaned",[]);
      _ -> message(error, "cl_ui refresh: something wrong",[])
    end.

handle_run_cl(Opt, Alg, Create)->
    {Clustering, FittNum} = cl_ui:run({Opt, Alg, Create}),
    message(status,"Clustering algorithm has been finished."),
    message(result,Clustering),
    message(flist,"~w",[FittNum]).

%%% ============================================================================
%%% Backup

handle_backup() ->
   try ?GRAPH:backup() of
      {ok, Name} ->
          message(status, "Done ~s", [Name]);
      _ ->
          message(status, "Unsuccessful", [])
   catch
       throw:Msg ->
          message(error, "Error: ~p", [Msg])
   end.

handle_clean() ->
   try ?GRAPH:clean() of
      {ok, backups_deleted} ->
          message(status, "Backup storage is cleaned", []);
      _ ->
          message(status, "No backup files", [])
   catch
       throw:Msg ->
          message(error, "Error: ~p", [Msg])
   end.

handle_undo(RFile) ->
    try ?GRAPH:undo() of
       undo_is_ok  ->
           {file, SFile} = ?SYNTAX:file(RFile),
           ?FILEMAN:save_file(SFile),
           ?GRAPH:clean(),
           message(reload, "~s", [RFile]);
       _ ->
           message(status, "Need a backup to do it", [])
    catch
      throw:Msg ->
           message(error, "Error: ~p", [Msg])
    end.
