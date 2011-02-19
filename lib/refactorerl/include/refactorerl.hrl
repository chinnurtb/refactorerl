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

%% Name of the server node
-define(REFAC_NODE, refactorerl@localhost).

%% Query and manipulation modules
-define(QUERY, refac_query).
-define(MANIP, refac_manip).

%% Graph storage interface module
-define(GRAPH, refac_graph).

%% Graph storage server process name
-define(GRAPH_SERVER, graph_server).

%% Semantical analyser interface module
-define(ESG, refac_anal).

%% Semantical analyser server process name
-define(ESG_SERVER, esg_server).

%% Event manager name for user interface messages
-define(UI_MSG_SERVER, ui_message_server).

-record(file,     {path}).
-record(form,     {type, tag}).
-record(clause,   {type, scope=false}).
-record(expr,     {type=expr, kind, value}).
-record(lex,      {type, data}).
-record(token,    {type, value, text, prews="", postws=""}).

-record(module,   {name}).
-record(macro,    {name}).
-record(record,   {name}).
-record(func,     {type, name, arity}).
-record(variable, {name}).
-record(env,      {name, value}).
