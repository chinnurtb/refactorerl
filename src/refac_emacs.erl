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
%%% @doc This module provides an interface for Emacs. It is intended to be
%%% `start'ed from the command line using `-noshell'. It reads Erlang terms
%%% from the standard input, and passes them to an interpreter function
%%% (implemented by a gen_server). Responses are written to the standard
%%% output in the form required by the Emacs RefactorErl minor mode.
%%%
%%% It is possible to run this interface on the same node as the main server,
%%% but distributed operation is also supported, when the input loop has its
%%% own dedicated Erlang node. This enables to have an Erlang shell running on
%%% the server node for development.
%%%
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(refac_emacs).
-vsn("$Rev: 1206 $").
-behaviour(gen_event).

-export([start/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-export([loop/1]).

-include("refactorerl.hrl").

%% @doc Main loop that reads Erlang terms from standard input and passes them
%% to the interpreter server.
start() ->
    case net_adm:ping(?REFAC_NODE) of
        pong ->
            process_flag(trap_exit, true),
            run_server();
        _ ->
            timer:sleep(100),
            start()
    end.

run_server() ->
    Srv = spawn_link(fun emacs_server/0),
    receive
        {'EXIT', Srv, Reason} when Reason /= normal ->
            io:format("Server Error: ~p~n", [Reason]),
            run_server()
    end.

emacs_server() ->
    register(refac_input, self()),
    monitor_node(?REFAC_NODE, true),
    connect_msg_server(),
    Self = self(),
    loop(spawn_link(fun() -> read_loop(Self) end)).

connect_msg_server() ->
    try refac_ui:add_msg_handler(?MODULE, self()) of
        ok -> ok
    catch
        exit:noproc ->
            timer:sleep(100),
            connect_msg_server()
    end.

loop(Inp) ->
    receive
        {Inp, Term} ->
            try
                ui_call(Term)
            catch
                _:Err ->
                    io:format("Server Error: ~p~n", [Err])
            end,
            loop(Inp);
        {'EXIT', Inp, Reason} ->
            io:format("Read loop error: ~p~n", [Reason]),
            Self = self(),
            Inp1 = spawn_link(fun() -> read_loop(Self) end),
            loop(Inp1);
        {msg, Msg}    ->
            show_msg(Msg),
            loop(Inp);
        code_change ->
            ?MODULE:loop(Inp);
        {nodedown, ?REFAC_NODE} ->
            init:stop()
    end.

%% @private
read_loop(Pid) ->
    case io:read('') of
        {ok, Term} -> Pid ! {self(), Term};
        eof        -> Pid ! {self(), {quit}};
        {error, {_, Mod, Error}} ->
            Pid ! {msg, {error,
                         lists:flatten(
                           io_lib:format("~s", [Mod:format_error(Error)]))}};
        _ -> ok
    end,
    read_loop(Pid).



%% @private
ui_call({quit})          -> refac_ui:stop();
ui_call({add, File})     -> refac_ui:add(File);
ui_call({drop, File})    -> refac_ui:drop(File);
ui_call({draw, File, T}) -> refac_ui:draw(File, T);
ui_call({filelist})      -> refac_ui:filelist();
ui_call({status, File})  -> refac_ui:status(File);
ui_call({reset})         -> refac_ui:reset();
ui_call({showconfig})    -> refac_ui:showconfig();
ui_call({saveconfig, App, Inc, Out}) ->
    refac_ui:saveconfig(App, Inc, Out);
ui_call({extract, File, Beg, End, Name}) ->
    refac_ui:extract(File, Beg, End, Name);
ui_call(Term)                            ->
    io:format("Unrecognised command ~512P~n", [Term,4]).


show_msg({Type, Text}) ->
    io:format("##~s~s~n", [type_text(Type), Text]);
show_msg(Msg) ->
    io:format("##ERRUnknown message: ~p~n", [Msg]).

type_text(status)      -> "MSG";
type_text(error)       -> "ERR";
type_text(add)         -> "ADD";
type_text(drop)        -> "DRP";
type_text(reload)      -> "RLD";
type_text(invalid)     -> "INV";
type_text(filelist)    -> "LFN";
type_text(include_dir) -> "INC";
type_text(app_dir)     -> "APP";
type_text(out_dir)     -> "OUT";
type_text(config_end)  -> "CFG";
type_text(_)           -> "MSG".


%% Event handler callbacks
init(Pid) when is_pid(Pid) ->
    {ok, Pid}.

handle_event(Event, Pid) ->
    Pid ! {msg, Event},
    {ok, Pid}.

handle_call(_Req, State) ->
    {ok, undefined, State}.
handle_info(_Info, State) ->
    {ok, State}.
terminate(_Arg, _State) ->
    ok.
code_change(_, S, _) ->
    {ok, S}.
