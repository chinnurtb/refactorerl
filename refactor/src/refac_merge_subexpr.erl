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
%% @author Robert Kitlei <kitlei@elte.hu>

%% @doc 
%% This module implements the refactoring merge subexpression duplicates.
%% 
%% @end

%% Changelog
%%  Version 0.1.2: Anikó Nagyné Víg added a new sideeffect check, which checks 
%%                 that the selected expression has sideeffect inside the 
%%                 expression. At the moment it checks if the expression 
%%                 contains any application or message passing. The change 
%%                 is in the merge_subexpression/6 function, the added function
%%                 call is refac_checks:check_if_body_doesnt_have_sideffects/2.
%% End Changelog
-module(refac_merge_subexpr).

-export([merge_subexpression/6]).

-include("node_type.hrl").

-vsn('0.2').

-define(NO_PREVIOUS_NODE, 0).

%% =====================================================================
%% @spec merge_subexpression(File::string(),
%%                           Line1::integer(), Col1::integer(), 
%%                           Line2::integer(), Col2::integer(),
%%                           NewName::string()) -> none()
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
%% <b>Line1</b> : The beginning line number pointed in the editor.
%% <b>Col1</b> : The beginning column number pointed in the editor.
%% <b>Line2</b> : The ending line number pointed in the editor.
%% <b>Col2</b> : The ending column number pointed in the editor.
%% <b>NewName</b> : The name to bind the expression to. </pre>
%% @end
%% =====================================================================
merge_subexpression(File, Line1, Col1, Line2, Col2, NewName) ->
  refac_checks:check_isVariableName(NewName),

  MId = refac_common:get_module_id(File),
  {Found, PathFromRootClause, ExprId} =
    get_expression_id(MId, Line1, Col1, Line2, Col2),
  refac_checks:check_found_expression(Found),

  RootClause = hd(PathFromRootClause),
  ExprParent = hd(tl(lists:reverse(PathFromRootClause))),
  ExprSideeffects = refac_common:get_sideeffects_by_parent(
  			MId, [ExprId], ExprParent),

  refac_checks:check_if_name_exists(MId, NewName, RootClause),
  refac_checks:check_sideeffects(ExprId, ExprSideeffects),
  refac_checks:check_if_body_doesnt_have_sideffects(MId, ExprId),
  refac_checks:check_send(MId, ExprId),

  ExprVars = refac_common:get_variables(MId, ExprId, with_root),
  BoundIds = get_id_bindings(MId, ExprVars, RootClause),

  refac_checks:check_if_bindings_are_unambiguous(MId, BoundIds, RootClause),

  AllExprInstances = get_expr_instances(MId, RootClause, ExprId, BoundIds),
  Sideeffects = refac_common:get_sideeffects(MId, RootClause, AllExprInstances),
  {_, SideeffectIds, _} = lists:unzip3(Sideeffects),
  ExprInstances = AllExprInstances -- SideeffectIds,
  {InsertionRoot, InsertionPoint} =
    find_insertion_point(MId, BoundIds, RootClause, ExprInstances),

  perform_refactoring(MId, InsertionRoot, ExprId, ExprInstances,
                      NewName, InsertionPoint, BoundIds),
  {ok, NewName}.


%% =====================================================================
%% @spec perform_refactoring(MId::integer(), RootClause::integer(), 
%%                           ExprId::integer(),
%%                           ExprInstances::[integer()],
%%                           NewName::string(),
%%                           InsertionPoint::integer(),
%%                           BoundIds::[integer()]) -> ok
%%
%% @doc
%% Performs the refactoring, and commits the changes into the 
%% refactoring system.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% <b>ExprId</b> : Id of the expression.
%% <b>ExprInstances</b> : List of all the expression instances.
%% <b>NewName</b> : The new name given for the variable.
%% <b>InsertionPoint</b> : Id after which the new binding is to be.
%% <b>BoundIds</b> : Ids bound in the expression.
%% </pre>
%% @end
%% =====================================================================
perform_refactoring(MId, RootClause, ExprId, ExprInstances,
                    NewName, InsertionPoint, BoundIds) ->
  NewVar = create_nodes:create_variable(MId, NewName),
  create_nodes:init_scope(MId, RootClause, NewVar),
  create_nodes:connect_variables(MId, [NewVar], [NewVar]),

  NewExprId = refactor:replicate_subtree(MId, ExprId, RootClause),
  NewMatchExpr = create_nodes:create_match_expr(MId, NewVar, NewExprId),

  insert_new_expression(MId, NewMatchExpr, RootClause, InsertionPoint),

  RemainingInstances = delete_instance_if_insertion_acquires_it(
                           MId, RootClause, ExprInstances, InsertionPoint,
			   BoundIds),

  replace_instances(MId, NewVar, NewName, RootClause, RemainingInstances),

  refactor:commit().  


%% =====================================================================
%% @spec delete_instance_if_insertion_acquires_it(
%%                           MId::integer(), RootClause::integer(), 
%%                           ExprInstances::[integer()],
%%                           InsertionPoint::integer(),
%%                           BoundIds::[integer()]) -> [integer()]
%%
%% @doc
%% Deletes the instance if it was replaced.
%% Returns the remaining instances.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% <b>ExprInstances</b> : List of all the expression instances.
%% <b>InsertionPoint</b> : Id after which the new binding is to be.
%% <b>BoundIds</b> : Ids bound in the expression.
%% </pre>
%% @end
%% =====================================================================
delete_instance_if_insertion_acquires_it(MId, RootClause, ExprInstances,
                                         InsertionPoint, BoundIds) ->
  MaybeToBeRemoved =
  if
    InsertionPoint == ?NO_PREVIOUS_NODE ->
      lists:nth(2, erl_syntax_db:clause_body(MId, RootClause));
    true ->
      InsertionPoint
  end,
  Remove = lists:member(MaybeToBeRemoved, ExprInstances) orelse
           lists:member(MaybeToBeRemoved, BoundIds),
  Deleted =
  if
    Remove ->
      delete_nodes:detach_node(MId, MaybeToBeRemoved, RootClause),
      delete_nodes:delete_node(MId, MaybeToBeRemoved),
      [MaybeToBeRemoved];
    true ->
      []
  end,
  ExprInstances -- Deleted.

%% =====================================================================
%% @spec get_expression_id(File::string(),
%%                         Line1::integer(), Col1::integer(), 
%%                         Line2::integer(), Col2::integer())
%%                            -> {Found::atom(),
%%                                PathFromRootClause::[integer()],
%%                                ExprId::integer()}
%% @doc
%% Returns the id of the expression.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line1</b> : The beginning line number pointed in the editor.
%% <b>Col1</b> : The beginning column number pointed in the editor.
%% <b>Line2</b> : The ending line number pointed in the editor.
%% <b>Col2</b> : The ending column number pointed in the editor.
%% </pre>
%% @end
%% =====================================================================
get_expression_id(MId, Line1, Col1, Line2, Col2) ->
  {CLine1, CCol1} = refactor:get_true_pos_from_pointed_pos(MId, Line1, Col1),
  {CLine2, CCol2} = refactor:get_true_pos_from_pointed_pos(MId, Line2, Col2),
  FromId = refac_common:get_lowest_id(
             MId, refactor:get_id_from_pos(MId, CLine1, CCol1)),
  ToId   = refac_common:get_lowest_id(
             MId, refactor:get_id_from_pos(MId, CLine2, CCol2)),
  RootClause = get_root_clause(MId, refactor:get_scope_from_id(MId, FromId)),
  {Found, PathFromRootClause, ExprId} =
    refac_common:find_expression_root_id(MId, RootClause, FromId, ToId),
  if
    Found andalso ExprId == RootClause ->
      {true, [RootClause, FromId], FromId};
    true ->
      {Found, PathFromRootClause, ExprId}
  end.


%% =====================================================================
%% @spec get_root_clause(MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns the root clause of an id.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id in the syntax tree.
%% </pre>
%% @end
%% =====================================================================
get_root_clause(MId, Id) ->
  Type = erl_syntax_db:type(MId, Id),
  case Type of
    ?CLAUSE ->
      Id;
    _Other ->
      OuterScope = refactor:get_containing_scope_id(MId, Id), 
      get_root_clause(MId, OuterScope)
  end.


%% =====================================================================
%% @spec get_id_bindings(MId::integer(), Vars::integer(),
%%                       RootClause::integer()) -> ok
%%
%% @doc
%% Returns the binding occurrence candidates of variables.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Vars</b> : Ids of the variables in the expression.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% </pre>
%% @end
%% =====================================================================
get_id_bindings(MId, Vars, RootClause) ->
  lists:flatten(
    lists:map(
      fun(VarId) ->
        VarName = refactor:get_name_from_name_id(MId, VarId),
        {_, Bind} = binding:get_binding_occurrence_candidates(MId, VarName, RootClause),
        Bind
      end, Vars)).

%% =====================================================================
%% @spec find_insertion_point(MId::integer(), BoundIds::[integer()],
%%                            RootClause::integer(), ExprIds::[integer()]) 
%%                                          -> integer()
%%
%% @doc
%% Returns the insertion point of the new binding.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BoundIds</b> : Binding occurrence candidates of all variables
%%                   of the expression.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% <b>ExprIds</b> : Ids of the expression.
%% </pre>
%% @end
%% =====================================================================
find_insertion_point(MId, BoundIds, RootClause, ExprIds) ->
  RootType = erl_syntax_db:type(MId, RootClause),
  ReverseBody = case RootType of
                  ?CLAUSE ->
                    lists:flatten(lists:reverse(
		        erl_syntax_db:clause_body(MId, RootClause)));
	          _ ->
		    refac_common:get_subtrees(MId, RootClause, without_root)
		end,
  LastSubtreeWithPossibleBinding =
    lists:dropwhile(
         fun(Subtree) ->
           [] == lists:filter(
                   fun(Var) ->
                     lists:member(Var, BoundIds)
                   end, refac_common:get_variables(MId, Subtree, with_root))
         end, ReverseBody),
  if
    LastSubtreeWithPossibleBinding == [] ->
      {RootClause, ?NO_PREVIOUS_NODE};
    true ->
      PossibleLocation = hd(LastSubtreeWithPossibleBinding),
      Subtrees = refac_common:get_subtrees(MId, PossibleLocation, with_root),
      HasBinding = Subtrees -- BoundIds,

      if
        HasBinding /= Subtrees -> % andalso HasExpr /= Subtrees ->
          {InsertionRoot, PreviousNode} =
            find_insertion_point(MId, BoundIds, PossibleLocation, ExprIds),
	  InsertionRootType = erl_syntax_db:type(MId, InsertionRoot),
	  case InsertionRootType of
	    ?CLAUSE ->
	      {InsertionRoot, PreviousNode};
	    _ ->
	      {RootClause, PossibleLocation}
          end;
        true ->
	  {RootClause, PossibleLocation}
      end
  end.

%% =====================================================================
%% @spec get_expr_instances(MId::integer(), RootClause::integer(),
%%                            ExprId::integer(), BoundIds::[integer()])
%%                               -> ok
%%
%% @doc
%% Returns the binding occurrence candidates of variables.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% <b>BoundIds</b> : Ids of binding occurrence candidates
%%                   of variables in the expression.
%% <b>ExprId</b> : Id of the expression.
%% </pre>
%% @end
%% =====================================================================
get_expr_instances(MId, RootClause, ExprId, BoundIds) ->
  ExprRootType = erl_syntax_db:type(MId, ExprId),
  ExprRootData = expr_instance_data(MId, ExprId, RootClause),
  Subtrees = refac_common:get_subtrees(MId, RootClause, without_root),
  FilteredPossibleExpressionNodes =
    lists:flatten(
      lists:map(
        fun(Id) ->
          IdType = erl_syntax_db:type(MId, Id),
          case IdType of
            ExprRootType ->
              ExprInstanceData = expr_instance_data(MId, Id, RootClause),
              if
                ExprRootData == ExprInstanceData ->
                  Id;
                true ->
                  []
              end;
            _Other ->
              []
          end
        end, Subtrees)),
  FilteredPossibleExpressionNodes -- BoundIds.

%% =====================================================================
%% @spec expr_instance_data(MId::integer(), Id::integer(),
%%                          RootClause::integer()) -> ok
%%
%% @doc
%% Returns the structure of the subtree essential for occurrence checking.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of an occurrence candidate.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% </pre>
%% @end
%% =====================================================================
expr_instance_data(MId, Id, RootClause) ->
  Subtrees = refac_common:get_subtrees(MId, Id, with_root),
  Types =
    lists:map(
      fun(Id2) ->
        Type = erl_syntax_db:type(MId, Id2),
        Additional =
          case Type of
            ?ATOM ->
              erl_syntax_db:atom_name(MId, Id2);
            ?ATTRIBUTE ->
              erl_syntax_db:attribute_name(MId, Id2);
            ?CHAR ->
              erl_syntax_db:char_value(MId, Id2);
            ?FLOAT ->
              erl_syntax_db:float_value(MId, Id2);
            ?FUNCTION ->
              erl_syntax_db:function_name(MId, Id2);
            ?INFIX_EXPR ->
              refactor:get_name_from_name_id(
                 MId, erl_syntax_db:infix_expr_operator(MId, Id2));
            ?INTEGER ->
              erl_syntax_db:integer_value(MId, Id2);
            ?MACRO ->
              erl_syntax_db:macro_name(MId, Id2);
            ?RECORD_FIELD ->
              {erl_syntax_db:record_field_name(MId, Id2),
               erl_syntax_db:record_field_value(MId, Id2)};
            ?RULE ->
              erl_syntax_db:rule_name(MId, Id2);
            ?STRING ->
              erl_syntax_db:string_value(MId, Id2);
            ?OPERATOR ->
              erl_syntax_db:operator_literal(MId, Id2);
            ?VARIABLE ->
              Name = erl_syntax_db:variable_literal(MId, Id2),
              Scope = refactor:get_scope_from_id(MId, Id2),
              {Name, binding:get_binding_occurrence_candidates(MId, Name, Scope)
              };
            _Other ->
              []
        end,
        {Type, Additional}
      end, Subtrees),
  BoundIds = lists:flatten(
               lists:map(
                 fun(Id2) ->
                   Type = erl_syntax_db:type(MId, Id2),
                   case Type of
                     ?VARIABLE ->
                       VarName = erl_syntax_db:variable_literal(MId, Id2),
                       binding:get_binding_occurrence_candidates(MId, VarName, RootClause);
                     _Other ->
                       []
                   end
                 end, refac_common:get_variables(MId, Id, with_root))),
  {Types, BoundIds}.


%% =====================================================================
%% @spec insert_new_expression(MId::integer(), NewMatchExpr::integer(),
%%                             RootClause::integer(), 
%%                             InsertionPoint::[integer()]) -> ok
%%
%% @doc
%% Inserts the new binding to the clause.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NewMatchExpr</b> : Id of the new match expression.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% <b>InsertionPoint</b> : Id after which the new binding is to be.
%% </pre>
%% @end
%% =====================================================================
insert_new_expression(MId, NewMatchExpr, RootClause, InsertionPoint) ->
  StructureHead = erl_syntax_db:clause_patterns(MId, RootClause),
  LastBoundIdInHead = lists:member(InsertionPoint, StructureHead),
  if
    LastBoundIdInHead ->
      After = ?NO_PREVIOUS_NODE;
    true ->
      After = InsertionPoint
  end,
  create_nodes:attach_subtree_to_node(MId, NewMatchExpr, RootClause, After).


%% =====================================================================
%% @spec replace_instances(MId::integer(), NewVar::integer(),
%%                           NewName::string(), RootClause::integer(), 
%%                           ExprInstances::[integer()]) -> ok
%%
%% @doc
%% Replaces the instances of the expression with the new variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NewVar</b> : Id of the new variable.
%% <b>NewName</b> : The new name given for the variable.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% <b>ExprInstances</b> : List of all the expression instances.
%% </pre>
%% @end
%% =====================================================================
replace_instances(MId, NewVar, NewName, RootClause, ExprInstances) ->
  lists:map(
    fun(Occ) ->
        delete_nodes:delete_node(MId, Occ),
        create_nodes:create_variable(MId, NewName, Occ),
        create_nodes:connect_variables(MId, [NewVar], [Occ]),	
        create_nodes:init_scope(MId, RootClause, Occ)
    end, ExprInstances).
