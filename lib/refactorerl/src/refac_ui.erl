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

-module(refac_ui).
-vsn("$Rev: 1335 $").
-behaviour(gen_server).

%%% Client exports
-export([stop/0, status/1, add/1, drop/1, draw/2, extract/4, showconfig/0,
         saveconfig/3, filelist/0, reset/0, funlist/1, movefun/3, backup/0,
         undo/1, clean/0, testing/2, cl_options/1, run_cl/3, cl_refresh/0,funexpr/2]).

%% UI message handlers
-export([message/2, message/3, add_msg_handler/2, del_msg_handler/2]).

%%% Server exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


-include("refactorerl.hrl").

stop()                    -> cast({stop}).
status(File)              -> cast({status, File}).
add(File)                 -> cast({add, File}).
drop(File)                -> cast({drop, File}).
draw(File, Type)          -> cast({draw, File, Type}).
extract(File, B, E, Name) -> cast({extract, File, B, E, Name}).
funexpr(File, Pos )       -> cast({funexpr, File, Pos}).
showconfig()              -> cast({showconfig}).
saveconfig(App, Inc, Out) -> cast({saveconfig, App, Inc, Out}).
filelist()                -> cast({filelist}).
reset()                   -> cast({reset}).
funlist(File)             -> cast({funlist, File}).
movefun(Src, Targ, Funs)  -> cast({movefun, Src, Targ, Funs}).
testing(Modules, Params)  -> cast({testing, Modules, Params}).
run_cl(Opt, Alg, Create)  -> cast({run_cl, Opt, Alg, Create}).
cl_options(Alg)           -> cast({cl_options, Alg}).
cl_refresh()              -> cast({cl_refresh}).
    

%% backups tables of Refactorerl database
backup()                  -> cast({backup}).
%% steps backward on the Refactorerl database
undo(RFile)               -> cast({undo, RFile}).
%% clean backup files from the Refactorerl database
clean()                   -> cast({clean}).
%redo(RFile)               -> cast({redo, RFile}).

cast(Data) -> gen_server:cast({?MODULE, ?REFAC_NODE}, Data).


%% @spec message(atom(), [term()]) -> ok
%% @doc Sends a message of `Type' wich contains an arbitrary list of terms
%% to the registered user interface message handlers.
message(Type, Data) ->
    gen_event:notify({?UI_MSG_SERVER, ?REFAC_NODE}, {Type, Data}).

%% @spec message(atom(), string(), [term()]) -> ok
%% @doc Sends a message of `Type' with arguments formatted by `io_lib:format'
%% to the registered user interface message handlers.
message(Type, Format, Args) ->
    gen_event:notify({?UI_MSG_SERVER, ?REFAC_NODE},
                     {Type, lists:flatten(io_lib:format(Format, Args))}).

%% @spec add_msg_handler(atom(), term()) -> ok | term()
%% @doc Registers a new user interface message handler.
add_msg_handler(Handler, Args) ->
    gen_event:add_sup_handler({?UI_MSG_SERVER, ?REFAC_NODE}, Handler, Args).

%% @spec del_msg_handler(atom(), term()) -> ok | term()
%% @doc Deletes a registered user interface message handler.
del_msg_handler(Handler, Args) ->
    gen_event:delete_handler({?UI_MSG_SERVER, ?REFAC_NODE}, Handler, Args).


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
                  extract     -> fun handle_extract/4;
		  funexpr     -> fun handle_funexpr/2;
                  showconfig  -> fun handle_showconfig/0;
                  saveconfig  -> fun handle_saveconfig/3;
                  filelist    -> fun handle_filelist/0;
                  funlist     -> fun handle_funlist/1;
                  movefun     -> fun handle_movefun/3;
                  reset       -> fun handle_reset/0;
                  echo        -> fun handle_echo/2;
                  testing     -> fun handle_testing/2;
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

%%Sending options for the emacs interface
handle_cl_options(Alg)->
    OptList = cl_ui:cl_options(Alg),
    Ls = [[A,B] ||{A,B} <- OptList],
    message(options,[Alg]++Ls).

handle_cl_refresh()->
    case cl_ui:refresh() of
      {cl_ui, _} -> message(status,"cl_ui refresh: cleaned",[]);
      _ -> message(error,"cl_ui refresh: something wrong",[])
    end.

handle_run_cl(Opt, Alg, Create)->
    {Clustering, FittNum} = cl_ui:run({Opt, Alg, Create}),
    message(status,"Clustering algorithm has been finished."),
    message(result,Clustering),
    message(flist,"~w",[FittNum])
    .

handle_stop() ->
    message(status, "RefactorErl server is shutting down...",[]),
    init:stop().

handle_status(File) when is_list(File) ->
    case refac_query:file(File) of
        {file, F} ->
            case ?GRAPH:path(F, [{form, {type,'==',error}}]) of
                [] -> message(add, "~s", [File]);
                _  -> message(invalid, "~s", [File])
            end;
        not_found  -> message(drop, "~s", [File])
    end.

handle_add(Name) when is_list(Name) ->
    case refac_query:file(Name) of
        {file, F} ->
            Drop = [(?GRAPH:data(FN))#file.path ||
                       FN <- ?GRAPH:path(F, [{incl, back}])],
            refac_fileman:drop_file(F);
        _ ->
            Drop = []
    end,
    case refac_fileman:add_file(Name) of
        {error, Reason} ->
            [message(drop, "~s", [DF]) || DF <- Drop],
            message(invalid, "~s", [Name]),
            message(status, "~s", [error_message(Reason)]);
        {file, File} ->
            Add = [(?GRAPH:data(FN))#file.path ||
                      FN <- ?GRAPH:path(File, [incl])],
            [message(drop, "~s", [DF]) || DF <- Drop],
            [message(add, "~s", [AF]) || AF <- Add]
    end.

handle_drop(File) when is_list(File) ->
    case refac_query:file(File) of
        {file, F} ->
            Drop = [(?GRAPH:data(FN))#file.path ||
                       FN <- ?GRAPH:path(F, [{incl, back}])],
            refac_fileman:drop_file(F),
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
    refac_draw_graph:draw_graph(File, Filt),
    message(status, "Ok", []).

handle_extract(File, Beg, End, NewName) ->
    try refac_extract_fun:extract_function(File, NewName, Beg, End-1) of
        {ok, ModFile} ->
            refac_fileman:save_file(ModFile),
            %refac_prettyprinter:prettyprint([File, File]),
            message(reload, "~s", [File]);
        _ ->
            message(status, "Unsuccessful", [])
    catch
        throw:Msg ->
            message(error, "Error: ~p", [Msg])
    end.

handle_funexpr(Filename, Pos) ->
    case refac_query:file(Filename) of
        not_found ->
            message(status, "File not found (~s)", [Filename]);
        {file, File} ->
	    refac_extract_funexpr:extract_funexpr(File, Pos)
    end.

handle_testing(Modules, Params)->
    try refac_test:main(Modules, Params) of
       {ok, done}->
          message(status, "Test - done", []);
	_ -> message(status, "undefined ",[])
    catch
        throw:Msg ->
	  message(error, "Error: ~p", [Msg])
    end.

handle_backup() ->
   try ?GRAPH:backup() of
      {ok, Name} ->
          message(backup, "Backup file : ~s", [Name]);
      _ ->
          message(status, "No backup", [])
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
       {reply, undo_is_ok, _} ->
           refac_fileman:save_file(RFile),
           message(undo, "Undo on ~s is ok", [RFile]);
       _ ->
           message(undo, "Undo on ~s is not ok", [RFile])
    catch 
      throw:Msg ->
           message(error, "Error: ~p", [Msg])
    end.

%TODO: Testing this function
%handle_redo(RFile) ->
%    try ?GRAPH:redo() of
%       {reply, redo_is_ok, _} ->
%           refac_fileman:save_file(RFile),
%           message(undo, "Redo on ~s is ok", [RFile]);
%       _ ->
%           message(undo, "Redo on ~s is not ok", [RFile])
%    catch 
%      throw:Msg ->
%           message(error, "Error: ~p", [Msg])
%    end.


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

handle_funlist(File) ->
    case refac_query:file(File) of
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
    case refac_query:file(Source) of
        not_found ->
            message(status, "File not found (~s)", [Source]);
        {file, File} ->
            case ?GRAPH:path(File, [moddef]) of
                [] ->
                    message(status, "The source file is not a module", []);
                [Mod] ->
                    refac_movefun:do((?GRAPH:data(Mod))#module.name,
                                     Funs, Target)
            end
    end.

handle_reset() ->
    refac_graph:reset_schema(),
    message(status, "Database clear started.", []).

handle_echo(Type, String) ->
    message(Type, "~s~n", [String]).


error_message({no_include_file, File}) ->
    io_lib:format("Include file \"~s\" not found", [File]);
error_message(Error) ->
    io_lib:format("Error: ~p", [Error]).


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

