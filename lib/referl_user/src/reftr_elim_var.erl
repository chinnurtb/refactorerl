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

%%% @doc In this refactoring all instances of a variable are replaced
%%% with its bound value in that region where the variable is
%%% visible. The variable can be left out where its value is not used.

%%% == Parameters ==
%%% <ul>
%%% <li>A variable (see {@link reflib_args:variable/1}).</li>
%%% </ul>

%%% == Conditions of applicability ==
%%% <ul>
%%% <li>The variable has exactly one binding occurrence on the left hand side of
%%%   a pattern matching expression, and not a part of a compound pattern.</li>
%%% <li>The expression bound to the variable has no side effects.</li>
%%% <li>Every variable of the expression is visible (that is, not shadowed) at
%%%   every occurrence of the variable to be eliminated.</li>
%%% </li>

%%% == Transformation steps and compensations ==
%%% <ol>
%%% <li>Substitute every occurrence of the variable with the expression
%%%   bound to it at its binding occurrence, with parentheses around the
%%%   expression.</li>
%%% <li>If the result of the match expression that binds the variable is
%%%   discarded, remove the whole match expression. Otherwise, replace the match
%%%   expression with its right hand side.</li>
%%% </ol>

%%% == Implementation status ==
%%% This refactoring is fully implemented.

%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>

-module(reftr_elim_var).
-vsn("$Rev: 4705 $"). % for emacs"

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%% @private
error_text(bindings, [VarName]) ->
    ["Variable ", VarName, " has multiple bindings"];
error_text(function, [VarName]) ->
    ["Variable ", VarName, " is a function parameter"];
error_text(bind_not_in_match_expr, [_VarName]) ->
    ["Only variables bound in match expressions can be eliminated"];
error_text(side_effect, [VarName]) ->
    ["The definition of ", VarName, " has side effects"];
error_text(macro, [VarName]) ->
    ["Variable ", VarName, " is used as a macro argument"];
error_text(shadowed_vars, [ShadowedVars]) ->
    ["Free variables of the definition would become shadowed: ", ShadowedVars].

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Var     = ?Args:variable(Args),
    VarName = ?Var:name(Var),

    Pattern = ?Query:exec1([Var], ?Var:bindings(),
                           ?LocalError(bindings, [VarName])),
%    Match = ?Query:exec1([Pattern], [{esub, back}],
    Match = ?Query:exec1([Pattern], ?Expr:parent(),
                         ?LocalError(function, [VarName])),
    ?Check(?Expr:type(Match) =:= match_expr, 
           ?LocalError(bind_not_in_match_expr, [VarName])),
    [Pattern, Def] = ?Query:exec([Match], ?Expr:children()),
    ?Check(not lists:any(fun ?Fun:is_dirty/1,
                         ?Query:exec([Def], ?Expr:functions())),
           ?LocalError(side_effect, [VarName])),
    FVars = free_vars(Def),
    Refs = ?Query:exec([Var], ?Var:references()),
    ?Check(not lists:any(fun is_from_macro/1, Refs),
           ?LocalError(macro, [VarName])),
    ShadowedVars = lists:concat([FVars -- visible_vars(Ref) || Ref <- Refs]),
    ShadowedVarNames = lists:map(fun ?Var:name/1, ShadowedVars),
    ?Check(ShadowedVars == [],
           ?LocalError(shadowed_vars, [ShadowedVarNames])),
    DefUsed = def_is_used(Match),

    [fun () ->
             lists:foreach(fun (R) -> replace(R, enclose(copy(Def))) end, Refs)
     end,
     fun (_) ->
             ?Transform:touch(Match),
             if not DefUsed -> delete(Match);
                true        -> replace(Match, enclose(copy(Def)))
             end
     end].

%%% ----------------------------------------------------------------------------
%%% prepare helpers

free_vars(E) ->
    F = ?Query:exec([E], ?Expr:varrefs()),
    B = ?Query:exec([E], ?Expr:varbinds()),
    lists:usort(F) -- B.

visible_vars(E) ->
    ?Query:exec([E], ?Query:seq(?Expr:clause(), ?Clause:variables())).

is_from_macro(E) ->
    [] /= ?Query:exec([E], [{elex, {data, '==', virtual}}]).

def_is_used(Match) ->
    Body = ?Query:exec([Match], ?Query:seq(?Expr:clause(), ?Clause:exprs())),
    case lists:member(Match, Body) of
        false ->
             true;
        true ->
            Match == lists:last(Body)
    end.

%%% ----------------------------------------------------------------------------
%%% transform helpers

copy(Node) ->
    proplists:get_value(Node, ?Syn:copy(Node)).

enclose(Node) ->
    ?Syn:construct({paren, Node}).

replace(From, To) ->
    [{_, Parent}] = ?Syn:parent(From),
    ?Syn:replace(Parent, {node, From}, [To]).

delete(Node) ->
    [{_, Parent}] = ?Syn:parent(Node),
    ?Syn:replace(Parent, {node, Node}, []).
