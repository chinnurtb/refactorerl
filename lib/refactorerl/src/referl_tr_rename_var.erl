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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the rename variable refactoring.  It
%%% renames the selected variable when there are no name clashes with
%%% the new name.
%%%

%%% Conditions of applicability
%%% <ul>
%%% <li>The new variable name does not exist in the scope of the variable,
%%%   either as a defined variable or as a visible
%%%   variable.</li>
%%% </ul>

%%% Rules of the transformation
%%% <ol>
%%% <li>Replace every occurrence of the variable with the new name.  In
%%%   case of variable shadowing, other variables with the same name are not
%%%   modified.</li>
%%% </ol>

%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>

-module(referl_tr_rename_var).
-vsn("$Rev: 2613 $").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1]).

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Var    = ?Args:variable(Args),
    Name   = ?Args:varname(Args),
    Scopes = ?Query:exec([Var], ?Var:scopes()),
    Occs   = ?Query:exec([Var], ?Var:occurrences()),
    lists:foreach(fun (O) -> check_name_clash(Name, O) end, Scopes),
    fun () ->
            [?Syn:replace(VarExpr, {elex,1}, [Name]) || VarExpr <- Occs],
            ?Transform:touch(hd(Occs))
    end.

check_name_clash(Name, Ref) ->
    Names = [?Var:name(V) ||
                V <- ?Query:exec([Ref], ?Clause:variables())],
    ?Check(not lists:member(Name, Names), ?RefError(var_exists, Name)).
