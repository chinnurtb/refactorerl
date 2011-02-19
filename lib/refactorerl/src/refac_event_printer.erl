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

%%% @doc This module provides an event listener, which listens the events raised
%%% by refac_ui and prints them to the standard output.
%%% This module helps using the tool without emacs.
%%%
%%% Example:
%%% ```
%%% % After starting the Erlang interpreter and the refactoring system:
%%% (refactorerl@localhost)1> refac_event_printer:start().
%%% ok
%%% (refactorerl@localhost)2> refac_ui:filelist().
%%% ok
%%% Event: {filelist,"/b1.erl"}
%%% Event: {filelist,"/b2.erl"}
%%% (refactorerl@localhost)3>
%%% '''
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(refac_event_printer).
-vsn("$Rev: 1247 $").
-behaviour(gen_event).

-export([start/0,stop/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

%% @doc Adds the event handler to the event manager of refac_ui.
start() ->
    refac_ui:add_msg_handler(?MODULE, null).

%% @doc Removes the event handler.
stop() ->
    refac_ui:del_msg_handler(?MODULE, null).
    
%% Event handler callbacks
init(State) ->
    {ok, State}.

handle_event(Event, State) ->
    io:format("Event: ~p~n",[Event]),
    {ok, State}.

handle_call(_Req, State) ->
    {ok, undefined, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.
