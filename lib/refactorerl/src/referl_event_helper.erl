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

%%% @doc This module provides helper functions to the event handling of the
%%% tool. Currently there is one helper function, {@link wait/1} that can be
%%% used to pause the current process until a given event occurs.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(referl_event_helper).
-vsn("$Rev: 1990 $").
-behaviour(gen_event).

-export([wait/1]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-include("refactorerl.hrl").

%% @spec wait(term()) -> ok
%%
%% @doc Waits until the given event occurs.
%%
%% For example, after calling {@link referl_ui:saveconfig/3}, we may want to
%% wait until the configuration saving operation is finished:
%% ```
%% ?UI:saveconfig([],[],original),
%% referl_event_helper:wait({status,"Configuration saved."})
%% '''
wait(Event) ->
    Message = make_ref(),
    start(self(), Event, Message),
    receive
        Message -> ok
    end,
    stop().

%% @doc Adds the event handler to the event manager of ?UI.
start(Pid, Event, MessageToSend) ->
    ?UI:add_msg_handler(?MODULE, {Pid, Event, MessageToSend}).

%% @doc Removes the event handler.
stop() ->
    ?UI:del_msg_handler(?MODULE, null).

%% Event handler callbacks
%% @private
init(State) ->
    {ok, State}.

%% @private
handle_event(Event, State) ->
    {Pid, EventToMonitor, MessageToSend} = State,
    case Event of
        EventToMonitor ->
            Pid ! MessageToSend;
        _ ->
            ok
    end,
    {ok, State}.

%% @private
handle_call(_Req, State) ->
    {ok, undefined, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Arg, _State) ->
    ok.

%% @private
code_change(_, S, _) ->
    {ok, S}.
