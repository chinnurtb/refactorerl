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

%%% @doc This module provides an event listener, which listens the events raised
%%% by `?UI' and prints them to the standard output.
%%% This module helps using the tool without Emacs.
%%%
%%% Example:
%%% ```
%%% % After starting the Erlang interpreter and the refactoring system:
%%% > referl_event_printer:start().
%%% ok
%%% > ?UI:filelist().
%%% ok
%%% Event: {filelist,"/b1.erl"}
%%% Event: {filelist,"/b2.erl"}
%%% >
%%% '''
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(referl_event_printer).
-vsn("$Rev: 3816 $").
-behaviour(gen_event).

-export([start/0,stop/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ui.hrl").

%% @doc Adds the event handler to the event manager of `?UI'.
start() ->
    ?UI:add_msg_handler(?MODULE, null, nosup).

%% @doc Removes the event handler.
stop() ->
    ?UI:del_msg_handler(?MODULE, null).

%% @private
%% Event handler callbacks
init(State) ->
    {ok, State}.

%% @private
handle_event(Event, State) ->
    io:format("Event: ~p~n",[Event]),
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
