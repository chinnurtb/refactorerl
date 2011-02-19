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


%%% ===========================================================================
%%% Name of the server node
-define(REFERL_NODE, refactorerl@localhost).


%%% ===========================================================================
%%% Modules

-define(Transform,      reflib_transform).
-define(Error,          reflib_error).
-define(Query,          reflib_query).
-define(Args,           reflib_args).
-define(File,           reflib_file).
-define(Form,           reflib_form).
-define(Clause,         reflib_clause).
-define(Expr,           reflib_expression).
-define(Mod,            reflib_module).
-define(Fun,            reflib_function).
-define(Dynfun,         reflib_dynfun).
-define(Var,            reflib_variable).
-define(Rec,            reflib_record).
-define(RecField,       reflib_record_field).
-define(Macro,          reflib_macro).
-define(Token,          reflib_token).
-define(UI,             reflib_ui_router).

-define(UIB,            reflib_ui).
-define(DRAW_GRAPH,     reflib_draw_graph).
-define(GR_UTILS,       reflib_graph_utils).

%%% ===========================================================================
%%% Module error reporting

-define(LocalError(Type, Detail), {?MODULE, Type, Detail}).
-define(RefError(Type, Detail),   {?Error, Type, Detail}).

-define(Check(Expr, Msg), case Expr of true -> ok; _ -> throw(Msg) end).

-define(LocalErr0r(Type),         ?LocalError(Type, [])).
-define(RefErr0r(Type),           ?RefError(Type, [])).
