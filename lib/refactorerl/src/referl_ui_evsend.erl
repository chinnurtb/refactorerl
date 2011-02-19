%%% -*- coding: latin-1 -*-

%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://plc.inf.elte.hu/erlang/

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

%%% @doc A trivial event handler that send every UI event to a specified
%%% process.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_ui_evsend).
-vsn("$Rev: 2943 $").
-behaviour(gen_event).

-include("refactorerl.hrl").

-export([start/1]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

%% @spec start(pid()) -> ok
%% @doc Registers a new process that should receive UI events.
start(Pid) ->
    spawn(fun() -> sender(Pid) end).

sender(Pid) ->
    ?UI:add_msg_handler(?MODULE, self()),
    process_flag(trap_exit, true),
    link(Pid),
    sender_loop(Pid).

sender_loop(Pid) ->
    receive
        {'EXIT',_FromPID,_Reason} ->
%            io:format("evsend: exit~n");
%            ?UI:del_msg_handler(?MODULE, self())
             ok;
        Event ->
%            io:format("evsend: ~p~n",[Event]),
            Pid ! Event,
            sender_loop(Pid)
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
%    io:format("evsend: terminate~n"),
    Sender ! terminated,
    ok.

%% @private
code_change(_, S, _) ->
    {ok, S}.
