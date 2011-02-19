%%% The contents of this file are subject to the Mozilla Public License
%%% Version 1.1 (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.mozilla.org/MPL/
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
%%%
%%% Contributor(s): ______________________________________.
%%%
%%% @doc This module contains the functionality which should be available to
%%% a user interface. Operations are asynchronous, results are returned by a
%%% gen_event manager.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refac_ui).
-vsn("$Rev: 1206 $").
-behaviour(gen_server).

%%% Client exports
-export([stop/0, status/1, add/1, drop/1, draw/2, extract/4, showconfig/0,
         saveconfig/3, filelist/0, reset/0]).

-export([message/3, add_msg_handler/2, del_msg_handler/2]).

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
showconfig()              -> cast({showconfig}).
saveconfig(App, Inc, Out) -> cast({saveconfig, App, Inc, Out}).
filelist()                -> cast({filelist}).
reset()                   -> cast({reset}).

cast(Data) -> gen_server:cast({?MODULE, ?REFAC_NODE}, Data).


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

%% @spec add_msg_handler(atom(), term()) -> ok | term()
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
handle_cast({stop}, S) ->
    message(status, "RefactorErl server is shutting down...",[]),
    init:stop(),
    {noreply, S};

handle_cast({status, File}, S) when is_list(File) ->
    case refac_syntax:file(File) of
        {file, F} ->
            case ?GRAPH:path(F, [{form, {type,'==',error}}]) of
                [] -> message(add, "~s", [File]);
                _  -> message(invalid, "~s", [File])
            end;
        not_found  -> message(drop, "~s", [File])
    end,
    {noreply, S};

handle_cast({add, Name}, S) when is_list(Name) ->
    case refac_syntax:file(Name) of
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
    end,
    {noreply, S};

handle_cast({drop, File}, S) when is_list(File) ->
    case refac_syntax:file(File) of
        {file, F} ->
            Drop = [(?GRAPH:data(FN))#file.path ||
                       FN <- ?GRAPH:path(F, [{incl, back}])],
            refac_fileman:drop_file(F),
            [message(drop, "~s", [DF]) || DF <- Drop];
        not_found ->
            message(status, "Already dropped", [])
    end,
    {noreply, S};

handle_cast({draw, File, Type}, S) ->
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
    message(status, "Ok", []),
    {noreply, S};

handle_cast({extract, File, Beg, End, NewName}, S) ->
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
    end,
    {noreply, S};

handle_cast({showconfig}, S) ->
    showconfig([?GRAPH:data(Env) ||
                   Env <- ?GRAPH:path(?GRAPH:root(), [env])]),
    {noreply, S};

handle_cast({saveconfig, AppDirs, IncDirs, OutDir}, S) ->
    [?GRAPH:delete(Env) || Env <- ?GRAPH:path(?GRAPH:root(), [env])],
    [?GRAPH:mklink(?GRAPH:root(), env,
                   ?GRAPH:create(#env{name=appbase, value=Dir})) ||
                      Dir <- AppDirs],
    [?GRAPH:mklink(?GRAPH:root(), env,
                   ?GRAPH:create(#env{name=include, value=Dir})) ||
                      Dir <- IncDirs],
    ?GRAPH:mklink(?GRAPH:root(), env,
                  ?GRAPH:create(#env{name=output, value=OutDir})),
    message(status, "Configuration saved.", []),
    {noreply, S};

handle_cast({filelist}, S) ->
    Files = [ {(?GRAPH:data(F))#file.path, F} ||
                F <- ?GRAPH:path(?GRAPH:root(), [file])],
    St = fun(N) -> case ?GRAPH:path(N, [{form, {type,'==',error}}]) of
                       [] -> ""; _ -> "!" end
         end,
    [message(filelist, "~s~s",[St(N),F]) || {F,N}<-lists:sort(Files)],
    {noreply, S};

handle_cast({reset}, S) ->
    refac_graph:reset_schema(),
    message(status, "Database clear started.", []),
    {noreply, S};

handle_cast({echo, Type, String}, S) ->
    message(Type, "~s~n", [String]),
    {noreply, S};

handle_cast(T, S) ->
    message(error, "Bad request ~512P", [T,4]),
    {noreply, S}.

showconfig([]) ->
    message(config_end, "", []);
showconfig([#env{name=include, value=Dir} | Tail]) ->
    message(include_dir, "~s", [Dir]),
    showconfig(Tail);
showconfig([#env{name=appbase, value=Dir} | Tail]) ->
    message(app_dir, "~s", [Dir]),
    showconfig(Tail);
showconfig([#env{name=output, value=Dir} | Tail]) ->
    message(out_dir, "~s", [Dir]),
    showconfig(Tail);
showconfig([_Head|Tail]) ->
    showconfig(Tail).


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

