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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc Rename variable implementation module. This refactoring
%%% renames a selected variable if the new name does not clash with
%%% any existing ones.

%%% == Parameters ==
%%% <ul>
%%% <li>A variable (see {@link reflib_args:variable/1}).</li>
%%% <li>A new variable name (see {@link reflib_args:varname/1}).</li>
%%% </ul>

%%% == Conditions of applicability ==
%%% <ul>
%%% <li>The new variable name does not exist in the scope of the
%%%   variable, either as a defined variable or as a visible
%%%   variable.</li>
%%% </ul>

%%% == Transformation steps and compensations ==
%%% <ol>
%%% <li>Replace every occurrence of the variable with the new name.  In
%%%   case of variable shadowing, other variables with the same name are not
%%%   modified.</li>
%%% </ol>

%%% == Implementation status ==
%%% This refactoring is fully implemented.

%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>

-module(reftr_rename_var).
-vsn("$Rev: 5496 $ ").

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Var        = ?Args:variable(Args, rename),

    Occs       = ?Query:exec(Var,  ?Var:occurrences()),
    Visibles   = ?Query:exec(Occs, ?Expr:visible_vars()),
    Useds      = ?Query:exec(Var,  ?Query:seq(?Var:scopes(), ?Clause:variables())),
    ClashNames = [?Var:name(V) || V <- Visibles ++ Useds],

    ArgsInfo   = add_transformation_info(Args, Var, Occs),


    NewName    = ?Args:ask(ArgsInfo, varname, fun cc_newname/2, fun cc_error/3,
                           ClashNames),

%    ?Macro:check_single_usage(Occs, [{elex, 1}]),
    [fun () ->
        ?Macro:inline_single_virtuals(Occs, elex),
        [?Macro:update_macro(VarOcc, {elex, 1}, NewName) || VarOcc <- Occs]
     end,
     fun(_)->
        [Var]
     end].

add_transformation_info(Args, Var, Occs) ->
    VarName = ?Var:name(Var),
    OccLen  = length(Occs),
    Info    = ?MISC:format("Renaming variable ~p (~p occurrences)",
                           [VarName, OccLen]),
    [{transformation_text, Info} | Args].

%%% ============================================================================
%%% Checks

cc_newname(NewVarName, ClashNames) ->
    ?Check(not lists:member(NewVarName, ClashNames),
           ?RefError(var_exists, NewVarName)),
    NewVarName.

cc_error(?RefError(var_exists, NewVarName), NewVarName, _ClashNames) ->
   ?MISC:format("The variable ~p is already used.", [NewVarName]).
