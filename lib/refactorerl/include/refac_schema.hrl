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
