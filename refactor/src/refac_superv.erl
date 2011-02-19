%%% Copyright Notice © 2007 Eötvös Loránd University and Ericsson Hungary
%%% Software development supported by Ericsson Hungary and
%%% GVOP-3.2.2-2004-07-0005/3.0 ELTE IKKK.

%%% Materials  were  created, authored,  and/or  prepared  by  the Authors  at
%%% Department   of  Programming  Languages   and  Compilers,   Eötvös  Loránd
%%% University,  Budapest,  Hungary  (ELTE)  for Ericsson  Hungary  under  the
%%% agreement  between  Ericsson  Hungary  and  ELTE  IKKK.  Unless  otherwise
%%% specifically stated, no claim to copyright is being asserted and it may be
%%% freely  used as  in the  public domain  in accordance  with  Erlang Public
%%% License.  All rights,  including copyright,  are owned  or  controlled for
%%% these purposes by  the Ericsson Hungary and ELTE.  Copyright exists in all
%%% other original  material published on the  internet and may  belong to the
%%% authors depending on the circumstances of publication.

%%% --------------------------------------------------------------------------
%%% ``The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1,  (the  "License"); you  may  not  use  this file  except  in
%%% compliance with the License. You should have received a copy of the Erlang
%%% Public License along  with this software. If not, it  can be retrieved via
%%% the world wide web at http://www.erlang.org/.

%%% Software distributed under the License is distributed on an "AS IS" basis,
%%% WITHOUT WARRANTY OF  ANY KIND, either express or  implied. See the License
%%% for  the specific  language  governing rights  and  limitations under  the
%%% License.

%%% The Initial  Developer of  the Original Code  is Ericsson  Utvecklings AB.
%%% Portions created by Ericsson  are Copyright 1999, Ericsson Utvecklings AB.
%%% All Rights Reserved.''
%%% --------------------------------------------------------------------------

%%% The Contributors are the Authors listed below. All Rights Reserved.

%%% You may not alter or remove any trademark, copyright or other notice from
%%% copies of the content.

%%% Authors: Zoltán Csörnyei
%%%          Zoltán Horváth
%%%          Roland Király
%%%          Róbert Kitlei
%%%          Tamás Kozsik
%%%          László Lövei
%%%          Tamás Nagy
%%%          Melinda Tóth
%%%          Anikó Víg

%%% Author contact: erlang@plc.inf.elte.hu
%%% --------------------------------------------------------------------------

%% @copyright 2007 Eötvös Loránd University and Ericsson Hungary
%% @author Laszlo Lovei <lovei@elte.hu>

%% @doc  
%% An application and supervisor module to start a confifure the refactorer
%% Maybe it's a bit unusual for an application and a supervisor to share the
%% same module, but never mind now.
%% @end

-module(refac_superv).

-vsn('0.1').

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).


%% Application functions
%% =====================================================================
%% @spec start(StarType::StartType, StartArgs::term()) -> 
%%          {ok,Pid} | {ok, Pid, State} | {error,Error}
%%       StartType = normal
%%                                 
%% @doc
%%    Starts the refactorer application.
%% Parameter description:<pre>
%% <b>StartType</b> : Right now we only support normal start mode.
%% <b>StartArgs</b> : Right now we do not use the starting args.
%%</pre>
%% 
%% @end
%% =====================================================================
start(normal, _StartArgs) ->
    supervisor:start_link(?MODULE, []).

%% =====================================================================
%% @spec stop(State::term()) -> none()
%%                                 
%% @doc
%%    Stops the refactorer application.
%% Parameter description:<pre>
%% <b>State</b> : Right now we do not use the state arg.
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
%%   RestartStrategy = one_for_all | one_for_one | rest_for_one | simple_one_for_one
%%   MaxR = integer()
%%   MaxT = integer()
%%   ChildSpec = child_spec()
%%                                 
%% @doc
%% Initalizes the supervisor which will manage the 
%% refactor_db gen_server  
%%
%% Parameter description:<pre>
%% <b>Args</b> : Right now we do not use the init args.
%%</pre>
%% 
%% @end
%% =====================================================================
init(_Args) ->
    {ok, DbType} = application:get_env(db_backend),
    {ok,
     {{one_for_one,   % restart only the terminated children
       3, 3600},      % 3 restarts in 1 hour at most
      [{database,     % ID
        {refactor_db, start_link, [DbType]},   % start function
        permanent,    % restart when terminates abnormally
        3000,         % shutdown timeout: 3 second
        worker,       % worker child (not a supervisor)
        [refactor_db] % module list - used by the release handler for code
                      % replacement which we do not support
                      % someday maybe we will; added module, which implements
                      % gen_server behaviour
       }
      ]
     }}.
