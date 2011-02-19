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
%% @author Melinda Toth <toth_m@inf.elte.hu>

%% @doc 
%% This module implements the extract function refactoring.
%% 
%% @end
-module(refac_extract_fun).
-export([extract_function/6]).

-include("node_type.hrl").

%% =====================================================================
%% @spec extract_function(File::string(), FromLine::integer(), 
%%                      FromCol::integer(), ToLine::integer(),
%%                         ToCol::integer(), NewName::string()) -> none()
%%
%% @doc
%% Performs the precondition checks and collects all the 
%% necessary information for the refactoring.
%% It throws various terms when there is an error, with this structure:
%% {atom(), term()}. If the atom is ok, the refactoring has been 
%% preformed without a problem.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>FromLine</b> : The pointed first line number in the editor.
%% <b>FromCol</b> : The pointed first column number in the editor.
%% <b>ToLine</b> : The pointed last line number in the editor.
%% <b>ToCol</b> : The pointed last column number in the editor.
%% <b>NewName</b> : The name of the extract function.
%% </pre>
%% @end
%% =====================================================================

extract_function(File, FromLine, FromCol, ToLine, ToCol, NewName)->	
  MId = refac_common:get_module_id(File),
  {FromCLine, FromCCol} = 
	   refactor:get_true_pos_from_pointed_pos( MId, FromLine, FromCol),
  {ToCLine, ToCCol} = 
	   refactor:get_true_pos_from_pointed_pos( MId, ToLine, ToCol),
  FromId = refac_common:
     get_lowest_id(MId, refactor:get_id_from_pos( MId, FromCLine,FromCCol)),
  ToId = refac_common:
     get_lowest_id(MId, refactor:get_id_from_pos( MId, ToCLine,ToCCol)),
 % [{FromId}] = refactor:get_id_from_pos( MId, FromCLine, FromCCol),
 % [{ToId}] = refactor:get_id_from_pos( MId, ToCLine, ToCCol),
  FromIdScope = refactor:get_scope_from_id(MId, FromId),
  {Found, Path, Ids} = 
        refac_common:find_expression_root_id(MId, FromIdScope, FromId, ToId),

  check_is_legal_body(Found,FromLine, FromCol, ToLine, ToCol, MId, Path, Ids), 

  Root = hd(Path),
  IdList=get_idlist(MId, Root, Ids),

  check_not_in_head_pattern_guard_macro( MId, Ids, Path),

  VarList = get_used_variable( MId, IdList, Ids),
  BoundVarList = get_bound_varlist(MId, VarList, Root),
  VarNameList =lists:usort(lists:map(fun(Elem)->
                              refactor:get_name_from_name_id(MId, Elem)
		        end, VarList)),
  BoundVarNameList = lists:map(fun(Elem)->
                                  refactor:get_name_from_name_id(MId, Elem)
			       end, BoundVarList),
  NotBoundVarNameList = VarNameList -- BoundVarNameList,

  check_all_var_bound_ok(MId, Root, VarList, BoundVarNameList),
  NotBoundVarList= 
   lists:usort(
     lists:flatten
	(lists:map(fun(VarName)->
          {_Found, BId} = 
	     binding:get_binding_occurrence_candidates(MId, VarName, Root),
             BId
                   end,NotBoundVarNameList))),
   Arity = length(NotBoundVarList),
  {_ExportListIds, FunctionData, ImportFunctions, _FunIsImportedMIds} = 
		                    refac_common:get_data(MId,NewName,Arity),
  check_is_legal_function_name(NewName, Arity, FunctionData, ImportFunctions),
  RootIn = get_root_id(Path, Ids),
  perform_refactoring(MId, NewName, IdList, NotBoundVarList, RootIn),
  {ok, done}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec get_root_id(Path::[integer()],
%%                  Ids:: {integer(), integer()} | integer()) -> [integer()]
%%
%% @doc
%% Gets the root of the selected sequence of expression.
%%
%% Parameter description:<pre>
%% <b>Path</b> : The path to the root of the selected sequence of expression.
%% <b>Ids</b> : Ids of the first and the last expression or 
%%              id of the expression.
%% </pre>
%% @end
%% =====================================================================
get_root_id(Path, Ids)->
    case Ids of
	{_Id1, _Id2}->
	    lists:last(Path);
	_ExprId ->
	    hd(tl(lists:reverse(Path)))
    end.


%% =====================================================================
%% @spec get_bound_varlist(MId::integer(),VarList::[integer()],
%%                          Root:: integer()) -> [integer()]
%%
%% @doc
%% Gets the variables which bounded in the selected sequence of expression.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarList</b> : The ids of the used variables.
%% <b>Root</b> : Id of the root of the subbtree which contain the
%%               selected sequence of expression.
%% </pre>
%% @end
%% =====================================================================

get_bound_varlist(MId, VarList, Root)->
  VarNameList = lists:map(fun(VarId)->
                                 refactor:get_name_from_name_id(MId, VarId) 
                           end, VarList),
  AllBoundList= 
   lists:usort(
     lists:flatten
	(lists:map(fun(VarName)->
          {_Found, BId} = 
	     binding:get_binding_occurrence_candidates(MId, VarName, Root),
             BId
                   end,VarNameList))),
  BoundList = lists:filter(fun(Elem)->
                              lists:member(Elem, AllBoundList)
                           end, VarList),
  BoundList.

%% =====================================================================
%% @spec check_is_legal_body(Found::atom(),FromLine::integer(), 
%%             FromCol::integer(), ToLine::integer(), ToCol::integer(),
%%             MId::integer(), Path::[integer()], 
%%             Ids:: {integer(), integer()} | integer()) -> ok
%%
%% @doc
%% Checks the starting and ending positions delimit a sequence of expression.
%%
%% Parameter description:<pre>
%% <b>FromLine</b> : The pointed first line number in the editor.
%% <b>FromCol</b> : The pointed first column number in the editor.
%% <b>ToLine</b> : The pointed last line number in the editor.
%% <b>ToCol</b> : The pointed last column number in the editor.
%% <b>MId</b> : Id of the module.
%% <b>Path</b> : The path to the root of the selected sequence of expression.
%% <b>Ids</b> : Ids of the first and the last expression or 
%%              id of the expression.
%% </pre>
%% @end
%% =====================================================================

check_is_legal_body(Found, FromLine, FromCol, ToLine, ToCol, MId, Path, Ids)->
  case Found of
    found_body->
               ok;
    found_expr->
	       ok;
    _ ->
      refac_checks:
	  error_handler({invalid_body, {{FromLine, FromCol},{ToLine,ToCol}}})
  end,
  Root = get_root_id(Path, Ids),
  case erl_syntax_db:type(MId, Root) of
    ?CASE_EXPR ->
       refac_checks:
	  error_handler({invalid_body, {{FromLine, FromCol},{ToLine,ToCol}}});
    ?IF_EXPR ->
       refac_checks:
          error_handler({invalid_body, {{FromLine, FromCol},{ToLine,ToCol}}});	  
    _  ->
	  ok
  end.
    

%% =====================================================================
%% @spec get_idlist(MId::integer(),Root::integer(),
%%               Ids::{integer(), integer()} | integer()) -> [integer()]
%%
%% @doc
%% Gets ids of the selected expressions.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Root</b> :Id of the root of the subbtree which contain the
%%               selected sequence of expression.
%% <b>Ids</b> : Ids of the first and the last expression or 
%%              id of the expression.
%% </pre>
%% @end
%% =====================================================================

get_idlist(MId, Root, Ids)->
  case Ids of
      {Id1, Id2}->
	  ClauseBody = erl_syntax_db:clause_body(MId, Root),
	  ReverseList = lists:reverse(lists:dropwhile(fun(Elem1)->
						         not (Elem1 == Id1)	     
						      end, ClauseBody)),
          lists:reverse(lists:dropwhile(fun(Elem2)->
					   not (Elem2 == Id2)	     
					end, ReverseList));
       ExprId ->
	  [ExprId]
  end.

%% =====================================================================
%% @spec get_used_variable(MId::integer(),IdList::[integer()],
%%                          Ids::{integer(), integer()}) -> [integer()]
%%
%% @doc
%% Gets used variable from the selected sequence of expression.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>IdList</b> : The ids of the selected expressions.
%% <b>Ids</b> : Ids of the first and the last expression.
%% </pre>
%% @end
%% =====================================================================

get_used_variable( MId, IdList, Ids)->
  case Ids of
      {_Id1, _Id2} ->
          lists:flatten(lists:map( fun(Id) ->
                           refac_common:get_variables(MId,Id,with_root)
                         end, IdList ));
      ExprId ->
	  lists:flatten(refac_common:get_variables(MId,ExprId,with_root))
  end.


%% =====================================================================
%% @spec check_all_var_bound_ok(MId::integer(), Root::integer(),
%%             VarList::[integer()], BoundVarNameList::[integer()]) -> ok
%%
%% @doc
%% Checks that all variables in the selected sequnce of expression not
%% appear outside of this seqence.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Root</b> : Id of the root of the subtree which contain the
%%               selected sequence of expression.
%% <b>VarList</b> : The variables of the expressions.
%% <b>BoundVarNamelist</b> : The names of the bounded variables 
%%                        in the selected expressions.
%% </pre>
%% @end
%% =====================================================================

check_all_var_bound_ok( MId, Root, VarList, BoundVarNameList)->
  All = lists:flatten(erl_syntax_db:subtrees(MId, Root)),
  AllVarList = 
     lists:flatten(lists:map(fun(Elem)->
			  refac_common:get_variables(MId, Elem,with_root)
		       end, All)),
  OutVarList = AllVarList -- VarList,
  OutVarNameList = lists:map(fun(VarId)->
			       refactor:get_name_from_name_id(MId, VarId)				      
			     end, OutVarList),
  OutsideUsedName = OutVarNameList -- (OutVarNameList -- BoundVarNameList),
  case OutsideUsedName of
     [] ->
	 ok;
     _ ->
	 refac_checks:error_handler({not_all_var_bound_ok, [OutsideUsedName]})
  end.

%% =====================================================================
%% @spec check_not_in_head_pattern_guard_macro( MId::integer(), 
%%                     Ids:: {integer(), integer()} | integer(),
%%                     Path::[integer()])-> ok
%%
%% @doc
%% Checks the expression id are not a part of  a guard sequence 
%% or a pattern or a macro or a list_comprehension node.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Path</b> : The path to the root of the selected sequence of expression.
%% <b>Ids</b> : Ids of the first and the last expression or 
%%              id of the expression.
%% </pre>
%% @end
%% =====================================================================

check_not_in_head_pattern_guard_macro( MId, Ids, Path)->
  case Ids of
     {_Id1, _Id2}->
	          ok;
     ExprId ->
        ExprParent = hd(tl(lists:reverse(Path))),
        ExprSideeffects = 
	  refac_common:get_sideeffects_by_parent(MId, [ExprId], ExprParent),
          refac_checks:check_sideeffects(ExprId, ExprSideeffects)
  end.

%% =====================================================================
%% @spec check_is_legal_function_name (NewName::string(), 
%%              Arity::integer(), FunctionData:: [{string(), integer()}], 
%%              ImportFunction::[{string(), string(), integer()}]  ) -> ok
%%
%% @doc
%% Checks that the function name is a legal function name, 
%% is not autoimported, is not exists and is not imported.
%%
%% Parameter description:<pre>
%% <b>NewName</b> : The exported function name.
%% <b>Arity</b> : The exported function arity.
%% <b>FunctionData</b> : The used functions name and arity.
%% <b>ImportFunction</b> : The imported functcion datas.
%% </pre>
%% @end
%% ===================================================================== 

check_is_legal_function_name( NewName, Arity, FunctionData, ImportFunctions)->	
  refac_checks:check_isFunctionName( NewName),
  refac_checks:check_is_autoimported( NewName),
  refac_checks:check_the_name_already_exists( NewName, Arity, FunctionData),
  refac_checks:check_the_name_is_imported( NewName, Arity, ImportFunctions).


%% =====================================================================
%% @spec perform_refactoring( MId::integer(), NewName::string(),
%%             IdList::[integer()], NotBoundVarList::[integer()],
%%             Root::integer()) -> ok
%%
%% @doc
%% Performs the refactoring, and commits the changes into the 
%% refactoring system.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NewName</b> : The name of the extracted function.
%% <b>IdList</b> : Ids of the selected expressions.
%% <b>NotBoundVarList</b> : Ids of the not bounded variables.
%% <b>Root</b> : Id of the root of the subbtree which contain the
%%                selected sequence of expression.
%% </pre>
%% @end
%% =====================================================================

perform_refactoring( MId, NewName, IdList, NotBoundVarList, Root)->
  NameId = create_nodes:create_atom(MId, NewName),
  ApplicationNameId = create_nodes:create_atom(MId, NewName),
  Id=hd(IdList),
  case length(IdList) of
    1 ->
      ScopeId = refactor:get_scope_from_id(MId, Id),
      BodyId = refactor:replicate_subtree(MId, Id, ScopeId),
      Clauses = create_nodes:create_clause(MId,NotBoundVarList,none,[BodyId]),
      FunId= create_nodes:create_function(MId, NameId, [Clauses]),
      delete_nodes:delete_node(MId, Id),
      create_nodes:
             create_application(MId, ApplicationNameId, NotBoundVarList, Id);
    _ ->
      Clauses = create_nodes:create_clause(MId,NotBoundVarList,none,IdList),
      FunId= create_nodes:create_function(MId, NameId, [Clauses]),
      ApplicationId = 
	create_nodes:create_application(MId,ApplicationNameId,NotBoundVarList),
      create_nodes:attach_subtree_to_node(MId, ApplicationId, Root, Id),
      lists:foreach(fun(Elem)->
	              delete_nodes:detach_node(MId, Elem, Root)
	             end,IdList)
  end,
  To = refactor:get_form_list_id_from_mid(MId),
  After = lists:last(erl_syntax_db:form_list_elements(MId, To)),
  create_nodes:attach_subtree_to_node(MId, FunId, To, After),
  refactor:commit().



