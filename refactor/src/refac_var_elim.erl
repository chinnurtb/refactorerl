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
%%% ``The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1,  (the  "License"); you  may  not  use  this file  except  in
%%% compliance with the License. You should have received a copy of the Erlang
%%% Public License along  with this software. If not, it  can be retrieved via
%%% the world wide web at http://www.erlang.org/.

%%% Software distributed under the License is distributed on an "AS IS" basis,
%%% WITHOUT WARRANTY OF  ANY KIND, either express or  implied. See the License
%%% for  the specific  language  governing rights  and  limitations under  the
%%% License.

%%% The Initial  Developer of  the Original Code  is Ericsson  Utvecklings AB.
%%% Portions created by Ericsson  are Copyright 1999, Ericsson Utvecklings AB.
%%% All Rights Reserved.''
%%% --------------------------------------------------------------------------

%%% The Contributors are the Authors listed below. All Rights Reserved.

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

%% @copyright 2007 Eötvös Loránd University and Ericsson Hungary
%% @author Tamas Nagy <lestat@elte.hu>
%% @author Aniko Vig <viganiko@inf.elte.hu>


%% @doc 
%% This module implements the eliminate variable refactoring.
%% 
%% @end
-module(refac_var_elim).

-export([eliminate_variable/3]).

%% =====================================================================
%% @spec eliminate_variable(File::string(), Line::integer(), 
%%                       Col::integer()) -> none()
%%
%% @doc
%% Performs the precondition checks and the and the collects all the 
%% necessary information for the refactoring.
%% It throws various terms when there is an error, with this syntax:
%% {atom(), term()}. If the atom is ok, the refactoring has been 
%% preformed without a problem.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The pointed line number in the editor.
%% <b>Col</b> : The pointed column number in the editor. </pre>
%% @end
%% =====================================================================
eliminate_variable(File, Line, Col) ->
    MId = refac_common:get_module_id(File),
    Id = refac_common:get_id_from_pos(MId, Line, Col, variable),
    {BId,ScopeId} = refactor:get_var_bind_occ_and_scope_from_id(MId, Id),
    Occurrences = 
	refactor:get_every_occurrence_of_a_var_from_id(MId, BId),
    BodyId = refac_common:get_patternIdBody(MId, BId),
    VarIdBIdNames = refac_common:local_preorder_var(MId, BodyId),
    Scopes = refac_common:get_inner_scope(MId, ScopeId),
    VarName = refactor:get_name_from_name_id(MId, BId),

    refac_checks:check_non_binding_in_match_pattern(MId, ScopeId, VarName),
    refac_checks:check_if_binding_is_unambiguous(MId, VarName, ScopeId),
    refac_checks:check_if_body_doesnt_have_sideffects(MId, BodyId),
    refac_checks:check_are_match_body_variables_shadowed(
      MId, BId, Scopes, VarIdBIdNames),

    perform_refactoring(MId, BId, ScopeId, BodyId, Occurrences),
    
    {ok, VarName}.


%% =====================================================================
%% @spec perform_refactoring(MId::integer(), BId::integer(),
%%                       ScopeId::integer(), BodyId::integer(),
%%                       VarIds::[{integer()}]) -> ok
%%
%% @doc
%% Performs the refactoring, and commits the changes into the 
%% refactoring system.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : The variable's binding occurrence's id. 
%% <b>ScopeId</b> : The scope of the match expression.
%% <b>BodyId</b> : The id of the body of the match expression.
%% <b>VarIds</b> : The occurrences of the eliminated variable.
%% <b>Scopes</b> : The inner scopes in <b>ScopeId</b>. </pre>
%% @end
%% =====================================================================
perform_refactoring(MId, BId, _ScopeId, BodyId, []) ->
    case refac_checks:check_if_binding_occurrence_needed(MId, BId, BodyId) of
	false ->
	    refactor:remove_binding_occurrence(MId, BId, BodyId);
	true ->
	    remove_binding_occurrence_and_body(MId, BId, BodyId)
    end,
    refactor:commit();
perform_refactoring(MId, BId, ScopeId, BodyId, [{VarId} | Xs]) ->
    case refac_checks:check_if_occurrence_needed(MId, VarId) of
	false ->
	    replace_occurrence_with_body(MId, VarId, BodyId);
	true ->
	    refactor:remove_occurrence_from_tree(MId, VarId)
    end,
    perform_refactoring(MId, BId, ScopeId, BodyId, Xs).

%% =====================================================================
%% @spec remove_binding_occurrence_and_body(
%%                           MId::integer(), BId::integer(),
%%                        BodyId::integer()) -> ok
%%
%% @doc
%% Removes the binding occurrence of the eliminated variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : The variable's binding occurrence's id. 
%% <b>ScopeId</b> : The scope of the match expression.
%% <b>BodyId</b> : The id of the body of the match expression.
%% <b>VarIds</b> : The occurrences of the eliminated variable.
%% <b>Scopes</b> : The inner scopes in <b>ScopeId</b>. </pre>
%% @end
%% =====================================================================
remove_binding_occurrence_and_body(MId, BId, BodyId) ->
    MatchId = 
	refactor:get_match_expr_id_from_pattern_and_body_id(MId, BId, BodyId),
    refactor:remove_occurrence_from_tree(MId, MatchId).

%% =====================================================================
%% @spec replace_occurrence_with_body(
%%                           MId::integer(), VarId::integer(),
%%                        BodyId::integer()) -> ok
%%
%% @doc
%% Replaces the eliminated variables occurrence with its value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarIds</b> : An occurrence of the eliminated variable.
%% <b>BodyId</b> : The id of the body of the match expression. </pre>
%% @end
%% =====================================================================
replace_occurrence_with_body(MId, VarId, BodyId) ->
    ScopeId = refactor:get_scope_from_id(MId, VarId),
    delete_nodes:delete_node(MId, VarId),
    refactor:replicate_subtree(MId, BodyId, ScopeId, VarId).
    
