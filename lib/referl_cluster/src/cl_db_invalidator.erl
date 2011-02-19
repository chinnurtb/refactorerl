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

%%% @doc This module implements an event listener that invalidates the database
%%% when a file was added, dropped or reloaded.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_db_invalidator).
-vsn("$Rev: 5049 $").
-behaviour(gen_event).

-export([start/0, stop/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

%% @doc Adds the event handler to the event manager of reflib_ui.
start() ->
    reflib_ui:add_msg_handler(?MODULE, null).

%% @doc Removes the event handler from the event manager of reflib_ui.
stop() ->
    reflib_ui:del_msg_handler(?MODULE, null).

%% ==================================================================
%% Event handler callbacks

%% @private
init(State) ->
    {ok, State}.

%% @private
handle_event(Event, State) ->
    case Event of
        {E, _} when E == drop; E == add; E == reload ->
            cl_db:invalidate();
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
