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
%%% --------------------------------------------------------------------------

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

-define(APPLICATION, 1).
-define(ARITY_QUALIFIER, 2).
-define(ATOM, 3).
-define(ATTRIBUTE, 4).
-define(BINARY, 5).
-define(BINARY_FIELD, 6).
-define(BLOCK_EXPR, 7).
-define(CASE_EXPR, 8).
-define(CATCH_EXPR, 9).
-define(CHAR, 10).
-define(CLASS_QUALIFIER, 11).
-define(CLAUSE, 12).
-define(COMMENT, 13).
-define(COND_EXPR, 14).
-define(CONJUNCTION, 15).
-define(DISJUNCTION, 16).
-define(EOF_MARKER, 17).
-define(FLOAT, 18).
-define(FORM_LIST, 19).
-define(FUN_EXPR, 20).
-define(FUNCTION, 21).
-define(GENERATOR, 22).
-define(IF_EXPR, 23).
-define(IMPLICIT_FUN, 24).
-define(INFIX_EXPR, 25).
-define(INTEGER, 26).
-define(LIST, 27).
-define(LIST_COMP, 28).
-define(MACRO, 29).
-define(MATCH_EXPR, 30).
-define(MODULE_QUALIFIER, 31).
-define(NIL, 32).
-define(OPERATOR, 33).
-define(PARENTHESES, 34).
-define(PREFIX_EXPR, 35).
-define(QUALIFIED_NAME, 36).
-define(QUERY_EXPR, 37).
-define(RECEIVE_EXPR, 38).
-define(RECORD_ACCESS, 39).
-define(RECORD_EXPR, 40).
-define(RECORD_FIELD, 41).
-define(RECORD_INDEX_EXPR, 42).
-define(RULE, 43).
-define(SIZE_QUALIFIER, 44).
-define(STRING, 45).
-define(TEXT, 46).
-define(TRY_EXPR, 47).
-define(TUPLE, 48).
-define(UNDERSCORE, 49).
-define(VARIABLE, 50).
