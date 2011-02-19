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

%%% @doc The callback module of the refactorer application and the callback
%%% module for the supervisors of the refactorer application.
%%%
%%% @author Laszlo Lovei <lovei@elte.hu>

-module(referl_superv).
-vsn("$Rev: 2170 $").
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).
-export([start_sup/1]).

-include("refactorerl.hrl").

%%% ============================================================================
%%% Application functions

%% @private
start(normal, _StartArgs) ->
    start_sup(top).

%% @private
start_sup(Node) ->
    supervisor:start_link(?MODULE, Node).

%% @private
stop(_State) ->
    ok.

%% =============================================================================
%% Supervisor functions

%% @private
init(top) ->
    {ok,
     {{one_for_one, 3, 1},
      [%% Graph storage subtree
       {graphsup,
        {?MODULE, start_sup, [graph]},
        permanent,
        infinity,
        supervisor,
        [?MODULE]},
       %% Transformation server
       {transform,
        {?Transform, start_link, []},
        permanent,
        3000,
        worker,
        [?Transform]},
       %% User interface bridge
       {ui,
        {?UI, start_link, []},
        permanent,
        1000,
        worker,
        [referl_ui]},
       %% User interface event handler
       {ui_msg,
        {gen_event, start_link, [{local, ?UI_MSG_SERVER}]},
        permanent,
        1000,
        worker,
        []}
      ]}};

init(graph) ->
    {ok,
     {{one_for_all,   % restart everyone in case of runtime error
       %3, 3600},     % 3 restarts in 1 hour at most
       1, 3},         % 1 restart in 3 seconds - development setting
      [%% Low level graph storage
       {graph,        % ID
        {?Graph, start_link, []},   % start function
        permanent,    % always restart
        3000,         % shutdown timeout: 3 second
        worker,       % worker child (not a supervisor)
        [?Graph]      % module list
       },
       %% Syntax tree storage and analyser
       {anal,
        {?ESG, start_link, []},
        permanent,
        3000,
        worker,
        [?ESG]
       }
      ]}}.
