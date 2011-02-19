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

%%% @doc A trivial event handler that send every UI event to a specified
%%% process. A new dispatcher process is started; this is neccessary to
%%% support initialization though rpc (see below). When the receiver process
%%% exits, the message handler is deleted.
%%%
%%% === Initialization using RPC ===
%%%
%%% When an RPC call is used to invoke {@link start/1}, which is the only
%%% possible way in case of using JInterface, the actual function call is made
%%% by the RPC server process. This would cause the RPC process to be linked
%%% with the message server instead of the process that actually receives the
%%% messages.
%%%
%%% This can only be solved by introducing a dispatcher process, which
%%% registers itself with the message server and gets linked with it, and
%%% links itself with the receiver process. This ensures the correct behaviour
%%% in case of the receiver process exits.
%%%
%%% === Possible enhancements ===
%%% <ol>
%%%  <li>Messages could be tagged (currently the bare events are sent)</li>
%%%  <li>Receiver process replacement could be supported</li>
%%% </ol>
%%%
%%% Currently there seems to be no need for these, although the first one is a
%%% step towards a much better architecture.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_ui_evsend).
-vsn("$Rev: 5134 $ ").
-behaviour(gen_event).

-export([start/1, start/2]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ui.hrl").

%% @spec start(pid()) -> pid()
%% @doc Registers a new process that should receive UI events. Returns the
%% dispatcher process ID.
start(Pid) ->
    start(Pid, notag).

%% @spec start(pid(),any()) -> pid()
%% @doc Registers a new process that should receive UI events. Returns the
%% dispatcher process ID.
%% The given tag will accompany each piped message in the form of {Tag,Msg}.
start(Pid, Tag) ->
    spawn(fun() -> sender(Pid, Tag) end).

sender(Pid, Tag) ->
    link(Pid),
    add_handler(),
    case Tag of
	notag -> sender_loop(Pid);
	Tag   -> sender_loop(Pid, Tag)
    end.

add_handler() ->
    try ?UI:add_msg_handler(self()) of
        ok -> ok
    catch
        exit:noproc -> timer:sleep(100), add_handler()
    end.

sender_loop(Pid, Tag) ->
    receive
        Event -> Pid ! {Tag, Event}, sender_loop(Pid, Tag)
    end.

sender_loop(Pid) ->
    receive
        Event -> Pid ! Event, sender_loop(Pid)
    end.


%% @private
%% Event handler callbacks
init(Sender) ->
    Sender ! installed,
    {ok, Sender}.

%% @private
handle_event(Event, Sender) ->
    Sender ! Event,
    {ok, Sender}.

%% @private
handle_call(_Req, Sender) ->
    {ok, undefined, Sender}.

%% @private
handle_info(_Info, Sender) ->
    {ok, Sender}.

%% @private
terminate(_Arg, Sender) ->
    Sender ! terminated,
    ok.

%% @private
code_change(_, S, _) ->
    {ok, S}.
