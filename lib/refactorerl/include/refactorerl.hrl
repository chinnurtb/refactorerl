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

%%% ===========================================================================
%%% Processes

%% Graph storage server process name
-define(GRAPH_SERVER,   graph_server).

%% Semantical analyser server process name
-define(ESG_SERVER,     esg_server).

%% Event manager name for user interface messages
-define(UI_MSG_SERVER,  ui_message_server).

%% Transformation server
-define(TRANSFORM_SERVER,   transform_server).

%%% ===========================================================================
%%% Modules

%% Common utilities
-define(MISC,           referl_misc).


%% External application interfaces
-define(DRAW_GRAPH,     referl_draw_graph).
-define(FILEMAN,        referl_fileman).
-define(UI,             referl_ui).
-define(TRANSFORM,      referl_transform).


%% Middle level graph manipulation
-define(GRAPH,          referl_graph).
-define(ESG,            referl_esg).


%% High level graph manipulation interface
-define(MANIP,          referl_manip).


%% Query modules
-define(SEMINF,         referl_seminf).
-define(SYNTAX,         referl_syntax).
-define(LEX,            referl_lex).


%% Syntax tree construction and information
-define(PREPROC,        referl_preproc).
-define(SCANNER,        referl_syntax_scanner).
-define(PARSER,         referl_syntax_parser).
-define(SYNLEX,         referl_synlex).
-define(NODE_STRUCTURE, referl_syntax_nodes).


%%% ===========================================================================
%%% Server node

%% Name of the server node
-define(REFERL_NODE, refactorerl@localhost).


%%% ===========================================================================
%%% Node data records

-record(file,     {type, path, eol}).
-record(form,     {type, tag}).
-record(clause,   {type, kind, indent}).
-record(expr,     {type=expr, kind, value, indent}).
-record(lex,      {type, data}).
-record(token,    {type, value, text, indent, prews="", postws=""}).

-record(module,   {name}).
-record(macro,    {name}).
-record(record,   {name}).
-record(field,    {name}).
-record(func,     {type, name, arity, dirty}).
-record(variable, {name}).
-record(env,      {name, value}).
