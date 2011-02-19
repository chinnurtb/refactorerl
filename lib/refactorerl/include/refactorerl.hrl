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
-define(GRAPH_SERVER, graph_server).

%% Semantical analyser server process name
-define(ESG_SERVER, esg_server).

%% Event manager name for user interface messages
-define(UI_MSG_SERVER, ui_message_server).

%% Transformation server
-define(TRANSFORM_SERVER, transform_server).

%% Lexical scanner server
-define(SCANNER_SERVER, scanner_server).

%%% ===========================================================================
%%% Modules

%% Common utilities
-define(MISC,           referl_misc).
-define(Error,          referl_error).
-define(GR_UTILS,       referl_graph_utils).

%% Storage layer
-define(Graph,          referl_graph).
-define(DRAW_GRAPH,     referl_draw_graph).

%% Semantical layer
-define(ESG,            referl_esg).
-define(Syn,            referl_syntax).
-define(Token,          referl_token).
-define(FileMan,        referl_fileman).
-define(PreProc,        referl_preproc).

%% Refactoring library
-define(Query,          referl_query).
-define(Args,           referl_args).
-define(File,           referl_file).
-define(Form,           referl_form).
-define(Clause,         referl_clause).
-define(Expr,           referl_expression).
-define(Mod,            referl_module).
-define(Fun,            referl_function).
-define(Var,            referl_variable).
-define(Rec,            referl_record).
-define(Macro,          referl_macro).
-define(Transform,      referl_transform).

%% External application interfaces
-define(UI,             referl_ui).


%% Syntax tree construction and information
-define(Scanner,        referl_scanner).
-define(Parser,         referl_syntax_parser).
-define(Nodes,          referl_syntax_nodes).


%%% ===========================================================================

-define(LocalError(Type, Detail), {?MODULE, Type, Detail}).
-define(RefError(Type, Detail),   {?Error, Type, Detail}).

-define(Check(Expr, Msg), case Expr of true -> ok; _ -> throw(Msg) end).

-define(LocalErr0r(Type),         ?LocalError(Type, [])).
-define(RefErr0r(Type),           ?RefError(Type, [])).

%%% ===========================================================================
%%% Server node

%% Name of the server node
-define(REFERL_NODE, refactorerl@localhost).


%%% ===========================================================================
%%% Node data records

-record(file,     {type, path, eol, lastmod}).
-record(form,     {type, tag, pp=none}).
-record(clause,   {type, kind, pp=none}).
-record(expr,     {type=expr, kind, value, pp=none}).
-record(lex,      {type, data}).
-record(token,    {type, value, text, prews="", postws=""}).

-record(module,   {name}).
-record(record,   {name}).
-record(field,    {name}).
-record(func,     {type, name, arity, dirty}).
-record(variable, {name}).
-record(env,      {name, value}).
