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
%%% @doc The callback module of the refactorer application and the callback
%%% module for the supervisors of the refactorer application.
%%% @author Laszlo Lovei <lovei@elte.hu>

-module(refac_superv).
-vsn("$Rev: 1206 $").
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).
-export([start_sup/1]).

-include("refactorerl.hrl").

%% Application functions
%% =====================================================================
%% @spec start(StarType::StartType, StartArgs::term()) -> 
%%          {ok,Pid} | {ok, Pid, State} | {error,Error}
%%       StartType = normal
%%                                 
%% @doc
%%    Starts the refactorer application.
%% Parameter description:<pre>
%% <b>StartType</b>: must be `normal'
%% <b>StartArgs</b>: unused
%%</pre>
%% 
%% @end
%% =====================================================================
start(normal, _StartArgs) ->
    start_sup(top).

start_sup(Node) ->
    supervisor:start_link(?MODULE, Node).

%% =====================================================================
%% @spec stop(State::term()) -> none()
%%                                 
%% @doc
%%    Stops the refactorer application.
%% Parameter description:<pre>
%% <b>State</b>: unused
%%</pre>
%% 
%% @end
%% =====================================================================
stop(_State) ->
    ok.


%% Supervisor functions
%% @type child_spec() = {Id,StartFunc,Restart,Shutdown,Type,Modules}
%%  Id = term()
%%  StartFunc = {M,F,A}
%%  M = atom()
%%  F = atom()
%%  A = [term()].

%% =====================================================================
%% @spec init(Args::term()) -> Result
%%   Result = {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%%   RestartStrategy = one_for_all  | one_for_one |
%%                     rest_for_one | simple_one_for_one
%%   MaxR = integer()
%%   MaxT = integer()
%%   ChildSpec = child_spec()
%%                                 
%% @doc
%% Initalizes the supervisor which will manage the 
%% refactor_db gen_server  
%%
%% Parameter description:<pre>
%% <b>Args</b>: unused
%%</pre>
%% 
%% @end
%% =====================================================================
init(top) ->
    {ok,
     {{one_for_one, 3, 1},
      [{graphsup,
        {?MODULE, start_sup, [graph]},
        permanent,
        infinity,
        supervisor,
        [?MODULE]},
       {ui,
        {refac_ui, start_link, []},
        permanent,
        1000,
        worker,
        [refac_ui]},
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
       %3, 3600},      % 3 restarts in 1 hour at most
       1, 3},         % 1 restart in 3 seconds - development setting
      [{graph,     % ID
        {?GRAPH, start_link, []},   % start function
        permanent,    % always restart
        3000,         % shutdown timeout: 3 second
        worker,       % worker child (not a supervisor)
        [?GRAPH]      % module list
       },
       {anal,
        {?ESG, start_link, []},
        permanent,
        3000,
        worker,
        [?ESG]
       }
      ]}}.
