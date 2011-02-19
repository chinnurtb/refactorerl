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

%% Name of the server node
-define(REFAC_NODE, refactorerl@localhost).

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
-record(form,     {type, tag, ws}).
-record(clause,   {type, scope=false, ws}).
-record(expr,     {type=expr, kind, value, ws}).
-record(lex,      {type, data}).
-record(token,    {type, text, value, ws}).
-record(ws,       {bef, aft}).

-record(module,   {name}).
-record(macro,    {name}).
-record(func,     {type, name, arity}).
-record(variable, {name}).
-record(env,      {name, value}).

-include("refac_syntax.hrl").

-define(LEXICAL_SCHEMA,
        [{lex,    record_info(fields,lex),    [{next, lex}, {llex, lex},
                                               {lfirst, lex}, {llast, lex},
                                               {mref, macro}, {iref, form}]},
         {macro,  record_info(fields, macro), [{marg, lex}, {mbody, lex}]},
         {file,   [{incl, file}, {macro, macro}]},
         {form,   [{flex, lex}, {ffirst, lex}, {flast, lex}]},
         {clause, [{clex, lex}, {cfirst, lex}, {clast, lex}]},
         {expr,   [{elex, lex}, {efirst, lex}, {elast, lex}]}
        ]).
