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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc The callback module of the refactorer application and the callback
%%% module for the supervisors of the refactorer application.
%%%
%%% @author Laszlo Lovei <lovei@elte.hu>

-module(refanal_sup).
-vsn("$Rev: 4519 $").
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callback
-export([init/1]).

%% Supervisor tree building
-export([start_sup/1, start_sup/2]).

-include("core.hrl").

%%% ============================================================================
%%% Application functions

%% @private
start(normal, _StartArgs) ->
    start_sup(top).

%% @private
start_sup(Node) ->
    supervisor:start_link(?MODULE, Node).

%% @private
start_sup(Name, Node) ->
    supervisor:start_link({local, Name}, ?MODULE, Node).

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
       %% File manager server
       {fileman,
        {?FileMan, start_link, []},
        permanent,
        3000,
        worker,
        [?FileMan]},
       %% Semantic node server
       {semnode,
        {?NodeSync, start_link, []},
        permanent,
        3000,
        worker,
        [?NodeSync]}
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
       {esg,
        {?ESG, start_link, []},
        permanent,
        3000,
        worker,
        [?ESG]
       },
        {anal,
        {?MODULE, start_sup, [?ANAL_SERVER, anal]},
        permanent,
        3000,
        supervisor,
        [?MODULE]},
      %% Function analyser server
       {funprop,
        {?FunProp, start_link, []},
        permanent,
        3000,
        worker,
        [?FunProp]}
      ]}};

init(anal) ->
    {ok,
     {{simple_one_for_one, 1, 3},
      [{anal, {?Anal, start_link, []}, transient, brutal_kill, worker, [?Anal]}
      ]}}.

