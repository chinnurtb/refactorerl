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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("referl_core/include/core_global.hrl").
-include_lib("referl_core/include/core_export.hrl").

%%% ===========================================================================
%%% Modules

-define(ErlScanner, refcore_erl_scanner).
-define(ErlParser,  refcore_erl_parser).
-define(ErlNodes,   refcore_erl_nodes).
-define(Anal,       refcore_anal).
-define(NodeSync,   refcore_nodesync).
-define(FunProp,    refcore_funprop).
-define(Db,         refcore_db).

%%% ===========================================================================
%%% Processes

%% Graph storage server process name
-define(GRAPH_SERVER, graph_server).

%% Semantical analyser server process name
-define(ESG_SERVER, esg_server).

%% File manager server process name
-define(FILEMAN_SERVER, fileman_server).

%% Analyser process manager
-define(ANAL_SERVER, anal_sup_srv).

%% Analyser related processes
-define(NODESYNC_SERVER, nodesync_server).
-define(FUNPROP_SERVER, funprop_server).
