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
%%%          István Bozó

%%% Author contact: erlang@plc.inf.elte.hu
%%% --------------------------------------------------------------------------

%% @copyright 2007 Eötvös Loránd University and Ericsson Hungary

%% @author István Bozó <bozo_i@inf.elte.hu>

%% @doc
%% This module implements the inline function refactoring.
%%


-module(refac_inline_fun).

-export([inline_function/3]).

-include("node_type.hrl").

%% =====================================================================
%% @spec inline_function(File::string(), Line::integer(),
%%                      Col::integer() ) -> none()
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
%% <b>Line</b> : The pointed line number of the selected application
%%               in the editor.
%% <b>Col</b> : The pointed column number of the selected application
%%              in the editor.
%% </pre>
%% @end
%% =====================================================================

inline_function(File, Line, Col) ->
  MId = refac_common:get_module_id(File),
  {AppId, Path, AppScope} =
    get_data_about_application(MId, Line, Col),
  {MId2, FunClauses} = get_data_about_function(MId, AppId),
  {Parent, Previous} = parent_id_and_previous_node(MId, AppId, Path),
  {LocalApp, Exported, Imported} = get_applications(MId2, FunClauses),
  check_functions_suitability(MId, MId2, FunClauses, AppScope, 
                              AppId, LocalApp, Path),
  perform_refactoring(MId, MId2, Path, AppId, AppScope,Parent, Previous, 
                      FunClauses, Exported, Imported),
  refactor:commit(),
  {ok,done}.

%% =====================================================================
%% @spec get_data_about_application(MId::integer(),Line::integer(),
%%                                  Col::integer())
%%                                  -> {AppId::integer(),
%%                                      Path::[integer()],
%%                                      ScopeId::integer()}
%% @doc
%% Returns the tuple of data about the application. The Id of the
%% selected application, the path from the outhermost scope and the
%% id of the applications scope. If the selected object is not an
%% application then throws an exception.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : Id of the expression.
%% </pre>
%% @end
%% =====================================================================
get_data_about_application(MId, Line, Col) ->
  {Id,Type} = get_id_and_type_from_position(MId, Line, Col),
  AppCand = 
    case Type of
      ?APPLICATION -> [{Id}];
      ?MODULE_QUALIFIER ->
        get_application_id_from_argument(MId, Id);
      ?ATOM ->
        ModQu = get_module_qualifier(MId, Id),
	if ModQu == [] ->
	  TempAppId = get_application_id_from_argument(MId, Id),
	  if TempAppId == [] ->
	      refac_checks:error_handler({pos_error, {Line, Col}});
	    true -> TempAppId
	  end;
	  true ->
	    [{ModQualifier}] = ModQu,
	    get_application_id_from_argument(MId, ModQualifier)
	end;
      _Other ->
        refac_checks:error_handler({pos_error, {Line, Col}})
    end,
    {AppId} = 
      if AppCand == [] -> 
          refac_checks:error_handler({pos_error, {Line, Col}});
        true -> hd(AppCand)
      end,
    {Path, ScopeId} = get_path_to_app(MId, AppId),
    {AppId, Path, ScopeId}.

%% =====================================================================
%% @spec replication(MId2::integer(),FunClause::integer(),
%%                                   MId::integer(),
%%                                   AppScope::integer())
%%                                  -> [integer()]
%% @doc
%% Replicates the subtree with the new module id and with the
%% applications scope. It is used to replicate the functions body.
%%
%% Parameter description:<pre>
%% <b>MId2</b> : The id of the functions module.
%% <b>FunClause</b> : Id of the functions clause.
%% <b>MId</b> : The id of the applications module.
%% <b>AppScope</b> : Id of the applications scope.
%% </pre>
%% @end
%% =====================================================================
replication(MId2, FunClause, MId, AppScope) ->
    Subtrees = erl_syntax_db:subtrees(MId2, FunClause),
    Subtree = lists:flatten(hd(lists:reverse(Subtrees))),
    lists:map(fun (Id) ->
	        refactor:replicate_subtree_with_new_mid(MId2, Id, 
                                                        AppScope, MId)
	      end, Subtree).

%% =====================================================================
%% @spec clauses_from_list(MId::integer(),List::[integer()])
%%                                  -> [integer()]
%% @doc
%% Returns the clause ids from the list of ids in a given module. It is
%% used for setting the variable bindings after the inline.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>List</b> : List of ids.
%% </pre>
%% @end

%% =====================================================================
clauses_from_list(MId, List) ->
  lists:filter(fun (Id) ->
    case erl_syntax_db:type(MId, Id) of
      ?CLAUSE -> true;
      _Other -> false
    end
  end, List).

%% =====================================================================
%% @spec set_variable_binding(MId::integer(),VariableList::[integer()],
%%                            ClauseList::[integer()])
%%                                  -> ok
%% @doc
%% Sets the variables binding after the inline.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>VariableList</b> : List of the variable names.
%% <b>ClauseList</b> : List of the possible clauses of the binding.
%% </pre>
%% @end
%% =====================================================================
set_variable_binding(MId, VariableList, ClauseList) ->
  lists:foreach(fun (Id) ->
    Name = refactor:get_name_from_name_id(MId, Id),
    Binding = find_binding(MId, Id, Name, ClauseList),
    case length(Binding) of
      1 -> update_binding(MId, Id, hd(Binding));
      _Other ->
        refac_checks:error_handler({binding_ambiguous,{Name}})
    end
  end, VariableList).


%% =====================================================================
%% @spec find_binding(MId::integer(),Id::integer(),
%%                            Name::string(), ClauseList::[integer()])
%%                                  -> [integer()]
%% @doc
%% Finds the suitable binding for the variable.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>Id</b> : The id of the variable.
%% <b>Name</b> : The name of the variable.
%% <b>ClauseList</b> : List of clauses.
%% </pre>
%% @end
%% =====================================================================
find_binding(MId, Id, Name, [Cl | Tail]) ->
  {_, Binding} =
    binding:get_binding_occurrence_candidates(MId, Name, Cl),
    case Binding of
      [] -> find_binding(MId, Id, Name, Tail);
      _Other ->
        BindCandidate = suitable_binding(MId, Binding),
	if BindCandidate /= not_found -> [BindCandidate];
	  true -> find_binding(MId, Id, Name, Tail)
	end
    end;
find_binding(_MId, Id, _Name, []) ->
    %%refac_checks:error_handler({binding_not_found,{MId, Name}}).
  [Id].

%% =====================================================================
%% @spec suitable_binding(MId::integer(),Candidates::[integer()])
%%                                  -> [integer()]
%% @doc
%% Finds the suitable binding candidate.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>Candidates</b> : List of candidate ids.
%% </pre>
%% @end
%% =====================================================================
suitable_binding(_MId, []) -> not_found;
suitable_binding(MId, [Bind | Tail]) ->
  BindCand = refactor:get_var_bind_occ_from_id(MId, Bind),
  if Bind == BindCand -> Bind;
    true -> suitable_binding(MId, Tail)
  end.

%% =====================================================================
%% @spec update_binding(MId::integer(),Id::integer(), BindId::[integer()])
%%                                  -> ok
%% @doc
%% Updates the binding of the variable in the database.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>List</b> : List of ids.
%% <b>BindId</b> : Id of the binding.
%% </pre>
%% @end
%% =====================================================================
update_binding(MId, Id, BindId) ->
    refactor_db:update("update var_visib set target= " ++
			 (integer_to_list(BindId) ++
			    (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ ";")))))).

%% =====================================================================
%% @spec get_unbound_variables(MId::integer())
%%                                  -> ok
%% @doc
%% Returns variable ids where target is 0 in the variable table
%% of the database. These are the replicated variables, which was
%% mounded in the parameter list.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% </pre>
%% @end
%% =====================================================================
get_unbound_variables(MId) ->
  Variables =
    refactor_db:select("select id from var_visib where mid = "
		        ++ (integer_to_list(MId) ++ " and target=0;")),
  erl_syntax_db:untuple(Variables).

%% =====================================================================
%% @spec perform_refactoring(MId::integer(), MId2::integer(),
%%                           Path::[integer()], AppId::integer(), 
%%                           AppScope::integer(), Parent::integer(), 
%%                           PreviousNode::integer(), 
%%                           FunClauses::[integer()],
%%                           Exported::[{string(),integer()}],
%%                           Imported::[{string(),string(),integer()}])
%%                                  -> [integer()]
%% @doc
%% Performs the refactoring from the previously gathered information.
%% Perform the replication and the campulsory settings. Maintains the
%% import list and qualify the applications if it is neccessary.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>Path</b> : Path to the application from the root scope.
%% <b>AppId</b> : The applications id.
%% <b>AppScope</b> : Id of the applications scope.
%% <b>Parent</b> : The id of the applications parent node.
%% <b>PreviousNode</b> : The previous node under the parent.
%% <b>FunClauses</b> : The list of the functions clause ids.
%% <b>Exported</b> : Exported functions in the functions clause.
%% <b>Imported</b> : Imported functions in the functions clause.
%% </pre>
%% @end
%% =====================================================================
perform_refactoring(MId, MId2, Path, AppId, AppScope, Parent, 
                    PreviousNode, FunClauses, Exported, Imported) ->
  Path_Clauses_reverse = lists:reverse(clauses_from_list(MId,Path)),
  After = 
    if PreviousNode == first -> 0;
      true -> PreviousNode
    end,
  AppArgIds = get_app_argument_ids_from_appid(MId, AppId),
  MainNodes =
    case length(FunClauses) of
      1 ->
        replication_with_one_function_clause(MId,MId2,AppId,AppScope,
                                             AppArgIds, FunClauses,
                                             Parent,After,
                                             Path_Clauses_reverse);
      _ ->
        replication_with_more_function_clauses(MId, MId2, AppId, AppScope, 
                                               AppArgIds,  FunClauses, 
                                               Parent, After, 
                                               Path_Clauses_reverse)
    end,
  VariableList = get_unbound_variables(MId),
  set_scope(MId, MainNodes, AppScope),
  set_variable_binding(MId, VariableList, Path_Clauses_reverse),
  application_maintenance(MId, MId2, MainNodes, Exported, Imported),
  delete_nodes:delete_node(MId, AppId),
  delete_app_from_fun_cache(MId, AppId).

%% =====================================================================
%% @spec delete_app_from_fun_cache(MId::integer(), AppId::integer())
%%                                  -> ok
%% @doc
%% Delete the given element from the fun_cache table.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>AppId</b> : The applications id.
%% </pre>
%% @end
%% =====================================================================
delete_app_from_fun_cache(MId,AppId)->
  refactor_db:delete("delete from fun_cache where "
    		     " mid = " ++ integer_to_list(MId) ++ " and id=" ++
                     integer_to_list(AppId) ++ ";").

%% =====================================================================
%% @spec attach_nodes(MId::integer(),AppId::integer(), Parent::integer(),
%%                    Previous::integer(),Elements::[integer()],
%%                    WithBody::atom())
%%                                  -> [integer()]
%% @doc
%% Attaches the nodes to the syntaxtree. The way of attaching depends on 
%% the application's parent node.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>AppId</b> : The applications id.
%% <b>Parent</b> : The id of the applications parent node.
%% <b>Previous</b> : The previous node under the parent.
%% <b>Elements</b> : The list of the nodes to be attached.
%% <b>WithBody</b> : Atom : with_body, without_body
%% </pre>
%% @end
%% =====================================================================
attach_nodes(MId, AppId, Parent, Previous, Elements, WithBody)->
  Body =
    if WithBody == with_body ->
        create_nodes:create_block_expr(MId, Elements);
      true ->
        hd(Elements)
    end,
  case erl_syntax_db:type(MId, Parent) of
    ?CLAUSE ->
      lists:foreach(fun(Id)->
        create_nodes:attach_subtree_to_node(MId, Id, Parent, Previous)
      end,Elements),
      delete_nodes:detach_node(MId, AppId, Parent),
      Elements;
    ?CASE_EXPR ->
      update_case_argument(MId,Parent,0,Body),
      [Body];
    ?MATCH_EXPR ->
      delete_nodes:detach_node(MId, AppId, Parent),
      create_nodes:attach_subtree_to_node(MId, Body, Parent, Previous),
      [Body];
    ?GENERATOR ->
      update_generator_body(MId,Parent,Body),
      [Body];
    ?TUPLE ->
      update_tuple_element(MId,Parent,AppId,Body),
      [Body];
    ?APPLICATION ->
      update_application_argument(MId,Parent,AppId,Body),
      [Body];
    ?LIST ->
      update_list_element(MId,Parent,AppId,Body),
      [Body];
    ?INFIX_EXPR ->
      update_infix_expr(MId,Parent,AppId,Body),
      [Body];
    ?BLOCK_EXPR ->
      update_block_expr(MId,Parent,AppId,Body),
      [Body];
    _Other ->
      refac_checks:error_handler({bad_parent, {Parent}})
  end.

%% =====================================================================
%% @spec update_case_argument(MId::integer(), CaseId::integer(),
%%                            Pos::integer(), Arg::integer())
%%                                  -> ok
%% @doc
%% Updates the case argument on the given position with the new 
%% argument id.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the case expression.
%% <b>Pos</b> : Which position to update.
%% <b>Arg</b> : The id of the new argument.
%% </pre>
%% @end
%% =====================================================================
update_case_argument(MId,CaseId,Pos,Arg)->
  refactor_db:update("update case_expr set argument= " ++
		       (integer_to_list(Arg) ++
			  (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(CaseId) ++ 
                                      " and pos=" ++ integer_to_list(Pos) ++ ";")))))).

%% =====================================================================
%% @spec update_generator_body(MId::integer(), Id::integer(),
%%                             Body::integer())
%%                                  -> ok
%% @doc
%% Updates the generators body.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the generator.
%% <b>Body</b> : The id of the new body.
%% </pre>
%% @end
%% =====================================================================
update_generator_body(MId,Id,Body)->
  refactor_db:update("update generator set body= " ++
		       (integer_to_list(Body) ++
			  (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ 
                                      ";")))))).

%% =====================================================================
%% @spec update_tuple_element(MId::integer(), Id::integer(),
%%                            SourceId::integer(), Element::integer())
%%                                  -> ok
%% @doc
%% Updates the tuple element on the given source id with the new 
%% element.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the tuple.
%% <b>SourceId</b> : Which element to update.
%% <b>Element</b> : The id of the new element.
%% </pre>
%% @end
%% =====================================================================
update_tuple_element(MId,Id,SourceId,Element)->
  refactor_db:update("update tuple set element= " ++
		       (integer_to_list(Element) ++
			  (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ 
                                      " and element = " ++ integer_to_list(SourceId) ++ ";")))))).

%% =====================================================================
%% @spec update_application_argument(MId::integer(), Id::integer(),
%%                            SourceId::integer(), Arg::integer())
%%                                  -> ok
%% @doc
%% Updates the element of the applications argument with the new value.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the application.
%% <b>SourceId</b> : The id to be changed
%% <b>Arg</b> : The id of the new argument.
%% </pre>
%% @end
%% =====================================================================
update_application_argument(MId,Id,SourceId,Arg)->
  refactor_db:update("update application set argument= " ++
		       (integer_to_list(Arg) ++
			  (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ 
                                      " and argument = " ++ integer_to_list(SourceId) ++ ";")))))).

%% =====================================================================
%% @spec update_list_element(MId::integer(), Id::integer(),
%%                            SourceId::integer(), Element::integer())
%%                                  -> ok
%% @doc
%% Updates the list element on the given source id with the new 
%% element.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the list.
%% <b>SourceId</b> : Which element to update.
%% <b>Element</b> : The id of the new element.
%% </pre>
%% @end
%% =====================================================================
update_list_element(MId,Id,SourceId,Element)->
  refactor_db:update("update list set element= " ++
		       (integer_to_list(Element) ++
			  (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ 
                                      " and element = " ++ integer_to_list(SourceId) ++ ";")))))).

%% =====================================================================
%% @spec update_infix_expr(MId::integer(), Id::integer(),
%%                            SourceId::integer(), Element::integer())
%%                                  -> ok
%% @doc
%% Updates the infix expressions left or right side.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the infix expression.
%% <b>SourceId</b> : Which element to update.
%% <b>Element</b> : The id of the new element.
%% </pre>
%% @end
%% =====================================================================
update_infix_expr(MId,Id,SourceId,Element)->
  refactor_db:update("update infix_expr set lft= " ++
		       (integer_to_list(Element) ++
			  (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ 
                                      " and lft = " ++ integer_to_list(SourceId) ++ ";")))))),
  refactor_db:update("update infix_expr set rght= " ++
		       (integer_to_list(Element) ++
			  (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ 
                                      " and rght = " ++ integer_to_list(SourceId) ++ ";")))))).

%% =====================================================================
%% @spec update_block_expr(MId::integer(), Id::integer(),
%%                            SourceId::integer(), Body::integer())
%%                                  -> ok
%% @doc
%% Updates the block expressions selected body with the new body.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the block expression.
%% <b>SourceId</b> : Which element to update.
%% <b>Body</b> : The id of the new element.
%% </pre>
%% @end
%% =====================================================================
update_block_expr(MId,Id,SourceId,Body)->
  refactor_db:update("update block_expr set body= " ++
		       (integer_to_list(Body) ++
			  (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ 
                                      " and body = " ++ integer_to_list(SourceId) ++ ";")))))).
%% =====================================================================
%% @spec replication_with_more_function_clauses(MId::integer(), 
%%                            MId2::integer(), AppId::integer(),
%%                            AppScope::integer(), AppArgIds::[integer()],
%%                            FunClauses::[integer()],Parent::integer(), 
%%                            After::integer(),Path_Clause_revresed::[integer()])
%%                                  -> [integer()]
%% @doc
%% Do the replication, with more function clauses and attach it to the
%% tree.
%% 
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>AppId</b> : The applications id.
%% <b>AppScope</b> : Id of the applications scope.
%% <b>AppArgIds</b> : The ids of the application arguments.
%% <b>FunClauses</b> : The list of the functions clause ids.
%% <b>Parent</b> : The applications parent node.
%% <b>After</b> : The previous node of the application.
%% <b>Path_Clause_reversed</b> : The clause ids from the root, to the 
%%                              application reversed.
%% </pre>
%% @end
%% =====================================================================
replication_with_more_function_clauses(MId, MId2, AppId, AppScope, 
                                       AppArgIds, FunClauses, Parent, 
                                       After,Path_Clauses_reversed)->
  RepAppArgs = 
    lists:map(fun (Id) ->
      refactor:replicate_subtree_with_new_mid(MId, Id, AppScope, MId)
    end, AppArgIds),
  Clauses = 
    create_case_clauses(MId,MId2,AppScope,AppArgIds,FunClauses,
                        Path_Clauses_reversed),
  AppTuple = create_nodes:create_tuple(MId, RepAppArgs),
  CaseExpr = create_nodes:create_case_expr(MId, AppTuple, Clauses),
  attach_nodes(MId, AppId, Parent, After, [CaseExpr], without_body).

%% =====================================================================
%% @spec create_case_clauses(MId::integer(), MId2::integer(),
%%                            AppScope::integer(), AppArgIds::[integer()],
%%                            FunClauses::[integer()],
%%                            Path_Clauses_reversed::[integer()])
%%                                  -> [integer()]
%% @doc
%% Replicate the function clauses for the case expression with the
%% guards if there exists.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>AppScope</b> : Id of the applications scope.
%% <b>AppArgIds</b> : The ids of the application arguments.
%% <b>FunClauses</b> : The list of the functions clause ids.
%% <b>Path_Clause_reversed</b> : The clause ids from the root, to the 
%%                              application reversed.
%% </pre>
%% @end
%% =====================================================================
create_case_clauses(MId,MId2,AppScope,AppArgIds,FunClauses,
                    Path_Clauses_reversed)->
  lists:map(fun (Id) ->
    Guard = erl_syntax_db:clause_guard(MId2, Id),
    GuardRep = 
      if Guard == none -> none;
        true ->
	  refactor:replicate_subtree_with_new_mid(MId2, Guard, AppScope, MId)
      end,
      RepNodes = replication(MId2, Id, MId, AppScope),
      FunArgIds =  get_fun_argument_ids_from_scope_id(MId2, Id),
      RepFunArgs = 
	lists:map(fun (SubId) ->
	  refactor:replicate_subtree_with_new_mid(MId2, SubId, AppScope, MId)
	end, FunArgIds),
      %%tuple creation for patterns and case creation
      FunTuple = create_nodes:create_tuple(MId, RepFunArgs),
      CaseClause = 
        create_nodes:create_clause(MId, [FunTuple], GuardRep, RepNodes),
      %%renaming the variables
      VarPairs = 
        lists:usort(
          lists:flatten(get_var_pairs_for_rename(MId,MId2,FunArgIds,AppArgIds))),
      Variables = 
        lists:flatten(refac_common:get_variables(MId, CaseClause, with_root)),
      rename_variables(MId, Variables, VarPairs),
      ArgVariables =
        lists:flatten(
          lists:map(fun(Elem)->
	    refac_common:get_variables(MId, Elem, with_root)
	  end,RepFunArgs)
        ),
      set_variable_binding(MId, ArgVariables, Path_Clauses_reversed),
      CaseClause
  end, FunClauses).

%% =====================================================================
%% @spec replication_with_one_function_clause(MId::integer(), 
%%                            MId2::integer(), AppId::integer(),
%%                            AppScope::integer(), AppArgIds::[integer()],
%%                            FunClauses::[integer()],Parent::integer(), 
%%                            After::integer(),
%%                            Path_Clauses_reversed::[integer()])
%%                                  -> [integer()]
%% @doc
%% Replicate the function clause, it is used for replicating function
%% with one clause. Attaches nodes to the syntax tree.
%% 
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>AppId</b> : The applications id.
%% <b>AppScope</b> : Id of the applications scope.
%% <b>AppArgIds</b> : The ids of the application arguments.
%% <b>FunClauses</b> : The list of the functions clause ids.
%% <b>Parent</b> : The applications parent node.
%% <b>After</b> : The previous node of the application.
%% <b>Path_Clause_reversed</b> : The clause ids from the root, to the 
%%                              application reversed.
%% </pre>
%% @end
%% =====================================================================
replication_with_one_function_clause(MId,MId2,AppId,AppScope,AppArgIds,
                            FunClauses,Parent,After,Path_Clauses_reversed)->
  FunArgIds = 
    get_fun_argument_ids_from_scope_id(MId2, hd(FunClauses)),
  AttrPairs = 
    do_attribute_pairs(MId, MId2, AppArgIds, FunArgIds, []),
  RepNodes = replication(MId2, hd(FunClauses), MId, AppScope),
  ElementalType =
    is_case_expr_necessary(MId2,FunArgIds),
  WithCase = 
    if ElementalType == true ->
        true;
      true ->
        erl_syntax_db:clause_guard(MId2, hd(FunClauses))
    end,
  Nodes =
    case ((WithCase == none) and (not equable_names(MId2,FunArgIds))) of
      true ->
        replication_without_guard_or_elemental(MId,MId2,AppId, AppScope, 
                                  AppArgIds, FunClauses, RepNodes, 
                                  FunArgIds, AttrPairs, Parent ,After);
      _Other ->
        replication_with_guard_or_elemental(MId, MId2, AppId, AppScope, 
                               AppArgIds, FunClauses, FunArgIds,  
                               RepNodes, Parent, After, 
			       Path_Clauses_reversed)
    end,
  Nodes.
%% =====================================================================
%% @spec equable_names( MId2::integer(), FunArgs::[integer()])
%%                                  -> true | false
%% @doc
%% If the given list of arguments or in the subtrees are duplicates of 
%% variable names it returns true value, else false value.
%%
%% Parameter description:<pre>
%% <b>MId2</b> : The id of the functions module.
%% <b>FunArgs</b> : The list of the functions arguent ids.
%% </pre>
%% @end
%% =====================================================================
equable_names(MId2,FunArgs)->
  Variables = 
    lists:flatten(lists:map(fun(Id)->
      refac_common:get_variables(MId2, Id, with_root)
    end,FunArgs)),
  VarNames = lists:usort(get_var_names_from_list(MId2, Variables)),
  length(Variables) /= length(VarNames).

%% =====================================================================
%% @spec replication_without_guard_or_elemental(MId::integer(), 
%%                            MId2::integer(),
%%                            AppId::integer(), AppScope::integer(),
%%                            AppArgIds::[integer()],
%%                            FunClauses::[integer()],RepNodes::[integer()],
%%                            FunArgIds::[integer()],
%%                            AttrPairs::[{integer(),integer()}],
%%                            Parent::integer(),After::integer())
%%                                  -> [integer()]
%% @doc
%% Attaches the nodes from the functions clause and add matches uses
%% the arguments of the application and function if it is neccessary.
%% 
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>AppId</b> : The applications id.
%% <b>AppScope</b> : Id of the applications scope.
%% <b>AppArgIds</b> : Ids of the applications arguments.
%% <b>FunClauses</b> : The list of the functions clause ids.
%% <b>RepNodes</b> : Replicated nodes from the function clause.
%% <b>FunArgIds</b> : Function argument ids.
%% <b>AttrPairs</b> : Pairs of the function and application arguments.
%% <b>Parent</b> : The applications parent node.
%% <b>After</b> : The previous node of the application.
%% </pre>
%% @end
%% =====================================================================
replication_without_guard_or_elemental(MId,MId2,AppId,AppScope,AppArgIds,
                          FunClauses, RepNodes,FunArgIds,AttrPairs,
                          Parent,After)->
  RepAttrPairs = replicate_attr_pairs(MId,MId2,AppScope,AttrPairs),
  Nodes = 
    lists:map(fun(Elem)->
      element(1,Elem)
    end,RepAttrPairs),
  VarPairs = 
    lists:usort(
      lists:flatten(get_var_pairs_for_rename(MId,MId2,FunArgIds,AppArgIds))),
  Matches = 
    create_matches(MId, MId2, RepAttrPairs, hd(FunClauses), 
                   FunArgIds),
  Variables = 
    lists:flatten(
      lists:map(fun(Id)->
        refac_common:get_variables(MId, Id, with_root)
      end, Nodes ++ RepNodes)),
  rename_variables(MId, Variables, VarPairs),
  Elements = lists:reverse(Matches ++ RepNodes),
  attach_nodes(MId, AppId, Parent, After, Elements, with_body).


%% =====================================================================
%% @spec replicate_attr_pairs(MId::integer(),MId2::integer(), 
%%                            AppScope::integer(),
%%                            AttrPairs::[{integer(),integer()}])
%%                                  -> [{integer(),integer()}]
%% @doc
%% Replicates the given attribute pairs.
%% 
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>AppScope</b> : Id of the applications scope.
%% <b>AttrPairs</b> : Pairs of the function and application arguments.
%% </pre>
%% @end
%% =====================================================================

replicate_attr_pairs(MId,MId2,AppScope,AttrPairs)->
  lists:map(fun({Fun,App})->
    Fst = refactor:replicate_subtree_with_new_mid(MId2, Fun, AppScope, MId),
    Variables = 
      lists:flatten(refac_common:get_variables(MId, Fst, with_root)),
    lists:foreach(fun(Id)->
      update_binding(MId, Id, Id)
    end, Variables),
    Snd = refactor:replicate_subtree_with_new_mid(MId, App, AppScope, MId),
    {Fst,Snd}
  end,AttrPairs).

%% =====================================================================
%% @spec replication_with_guard_or_elemental(MId::integer(),
%%                            MId2::integer(),
%%                            AppId::integer(), AppScope::integer(),
%%                            AppArgIds::[integer()],
%%                            FunArgIds::[integer()],
%%                            RepNodes::[integer()],Guard::[integer()],
%%                            Parent::integer(),After::integer(),
%%                            Path_Clauses_reversed::[integer()])
%%                                  -> [integer()]
%% @doc
%% Replicate the nodes from the functions clause.
%% 
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>AppId</b> : The applications id.
%% <b>AppScope</b> : The applications scope id.
%% <b>AppArgIds</b> : Application argument Ids.
%% <b>FunArgIds</b> : Function argument ids.
%% <b>RepNodes</b> : Replicated nodes from the function clause.
%% <b>Guard</b> : The guard id of the functions clause.
%% <b>Parent</b> : The applications parent node.
%% <b>After</b> : The previous node of the application.
%% <b>Path_Clauses_reversed</b> : Reversed clauses from the path.
%% </pre>
%% @end
%% =====================================================================
replication_with_guard_or_elemental(MId, MId2, AppId, AppScope, AppArgIds, 
                       FunClauses, FunArgIds,  RepNodes, Parent, After,
		       Path_Clauses_reversed)->
  Guard = erl_syntax_db:clause_guard(MId2, hd(FunClauses)),
  %%replication of app and fun arguments
  GuardRep =
    if Guard /= none ->
        refactor:replicate_subtree_with_new_mid(MId2, Guard, AppScope, MId);
      true -> none
    end,
  RepAppArgs = 
  lists:map(fun (Id) ->
    refactor:replicate_subtree_with_new_mid(MId, Id, AppScope, MId)
  end, AppArgIds),
  RepFunArgs = 
  lists:map(fun (Id) ->
    refactor:replicate_subtree_with_new_mid(MId2, Id, AppScope, MId)
  end, FunArgIds),
  %%tuple creation for patterns and case creation
  AppTuple = create_nodes:create_tuple(MId, RepAppArgs),
  FunTuple = create_nodes:create_tuple(MId, RepFunArgs),
  CaseClause = 
    create_nodes:create_clause(MId, [FunTuple], GuardRep, RepNodes),
  VarPairs =
    lists:usort(
      lists:flatten(get_var_pairs_for_rename(MId,MId2,FunArgIds,AppArgIds))),
  Variables =
    lists:flatten(
      refac_common:get_variables(MId, CaseClause, with_root)),
  rename_variables(MId, Variables, VarPairs),
  CaseExpr = 
    create_nodes:create_case_expr(MId, AppTuple, [CaseClause]),
  ArgVariables =
    lists:flatten(
      lists:map(fun(Id)->
        refac_common:get_variables(MId, Id, with_root)
      end,RepFunArgs)
    ),
  set_variable_binding(MId, ArgVariables, Path_Clauses_reversed),
  attach_nodes(MId, AppId, Parent, After, [CaseExpr],without_body).

%% =====================================================================
%% @spec is_case_expr_necessary(MId2::integer(), FunArgIds::[integer()])
%%                                        -> true | false
%% @doc
%% Checks for the elemental types in the given list and in the subtree
%% of the given nodes.
%%
%%
%% Parameter description:<pre>
%% <b>MId2</b> : The id of the module.
%% <b>Ids</b> : The list of nodes.
%% </pre>
%% @end
%% =====================================================================
is_case_expr_necessary(MId2,Ids)->
  SubIds = 
    lists:flatten(lists:map(fun(Id)->
      refac_common:get_subtrees(MId2, Id, with_root)
    end,Ids)),
  is_case_necessary(MId2,SubIds).

%% =====================================================================
%% @spec is_case_necessary(MId2::integer(), Nodes::[integer()])
%%                                                             -> ok
%% @doc
%% Returns true or false atoms depending on the list contains an 
%% elemntal type or not. Elemental types :atom, char, float, integer,
%% string.
%%
%%
%% Parameter description:<pre>
%% <b>MId2</b> : The id of the module.
%% <b>Nodes</b> : The list of the node ids.
%% </pre>
%% @end
%% =====================================================================
is_case_necessary(MId2,[Hd|Tail])->
  case erl_syntax_db:type(MId2,Hd) of
    ?ATOM -> true;
    ?CHAR-> true;
    ?FLOAT-> true;
    ?INTEGER-> true;
    ?STRING-> true;
    _Other->
      is_case_necessary(MId2,Tail)
  end;
is_case_necessary(_MId2,[]) ->
  false.
    
%% =====================================================================
%% @spec application_maintenance(MId::integer(), MId2::integer(),
%%                            Nodes::[integer()],
%%                            Exported::[{string(),integer()}],
%%                            Imported::[{string(),string(),integer()}])
%%                                                             -> ok
%% @doc
%% This function do the maintenance of the replicated applications in the
%% replicated function clauses. Adds module qualifiers if it is neccessary.
%% And maintain the import list
%%
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Nodes</b> : The list of the node ids.
%% <b>Exported</b> : The list of exported functions, which are used in
%%                   the function clause/clauses.
%% <b>Imported</b> : The list of imported functions, which are used in
%%                   the function clause/clauses.
%% </pre>
%% @end
%% =====================================================================
application_maintenance(MId, MId2, Nodes, Exported, Imported) ->
  SubNodes = 
    lists:flatten(lists:map(fun (NodeId) ->
      refac_common:get_subtrees(MId, NodeId, with_root)
    end, Nodes)),
  ApplicationIds = get_application_ids_from_list(MId, SubNodes),
  if MId == MId2 ->
      ok;
    true ->
      NotQualified = lists:filter(fun (Id) ->
		       not is_app_qualified(MId, Id)
		     end, ApplicationIds),
      Bifs = get_bifs_from_appid_list(MId, NotQualified),
      AppIds = NotQualified -- Bifs,
      lists:foreach(fun (Id) ->
        NameAndArity = get_applications_name_and_arity(MId, Id),
	case lists:member(NameAndArity, Exported) of
	  true -> qualify_app(MId, MId2, Id);
	  false -> ok
	end
      end, AppIds),
      lists:foreach(fun(Id)->
        maintain_fun_cache(MId,Id,Imported)
      end,AppIds),
      import_list_maintenance(MId, Imported)
  end.

%% =====================================================================
%% @spec maintain_fun_cache(MId::integer(), AppId::integer(),
%%                            Imported::[{string(),string(),integer()}])
%%                                                             -> ok
%% @doc
%% Add new elements to fun_cache table if it is neccesary.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>AppId</b> : The id of the application.
%% <b>Imported</b> : The list of imported functions, which are used in
%%                   the function clause/clauses.
%% </pre>
%% @end
%% =====================================================================
maintain_fun_cache(MId,AppId,Imported)->
  Arg = get_app_posnull_arg(MId, AppId),
  Arity = refactor:get_application_arity_from_application_id(MId, AppId),
  case erl_syntax_db:type(MId,Arg) of
    ?MODULE_QUALIFIER ->
      {ModNameId,NameId} =
        get_module_and_body_from_module_qualifier(MId,Arg),
      ModName = refactor:get_name_from_name_id(MId, ModNameId),
      Name = refactor:get_name_from_name_id(MId, NameId),
      refactor:put_into_dbase_fun_cache([{MId,AppId,ModName,Name,Arity}], []);
    _Other ->
      Name = get_name_to_simple_app(MId, AppId),
      List = 
        lists:filter(fun({_Mid,Nm,Ar})->
          {Nm,Ar} == {Name,Arity}
        end,Imported),
      ModName = 
        if List == [] ->
            refactor:get_module_name(MId);
	  true ->
            element(1,hd(List))
        end,
      refactor:put_into_dbase_fun_cache([{MId,AppId,ModName,Name,Arity}], [])
  end.
      
	  
%% =====================================================================
%% @spec get_module_and_body_from_module_qualifier(MId::integer(), 
%%                                                 Id::integer())
%%                                                           -> ok
%% @doc
%% Returns a tuple of module and body identifier from the 
%% module_qualifier table.
%%
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the module qualifier.
%% </pre>
%% @end
%% =====================================================================
get_module_and_body_from_module_qualifier(MId,Id)->
  [Temp] =
    refactor_db:select("select module,body from module_qualifier "
			 "where mid = "
			   ++
			     (integer_to_list(MId) ++
				(" and id=" ++
				   (integer_to_list(Id) ++
				      ";")))),
  Temp.

%% =====================================================================
%% @spec get_app_posnull_arg(MId::integer(), AppId::integer())
%%                                                          -> ok
%% @doc
%% Returns the applications argument at position 0 from the given module
%% and identifier.
%%
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>AppId</b> : The applications id.
%% </pre>
%% @end
%% =====================================================================
get_app_posnull_arg(MId, AppId) ->
  [{Arg}] =
    refactor_db:select("select argument from application "
			 "where mid = "
			   ++
			     (integer_to_list(MId) ++
				(" and id=" ++
				   (integer_to_list(AppId) ++
				      " and pos=0;")))),
  Arg.

%% =====================================================================
%% @spec qualify_app(MId::integer(), MId2::integer(), Id::integer())
%%                                              -> {string(),integer()}
%% @doc
%% Modifies the application. It works only for the simple application
%% without the module qualifier.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>MId2</b> : The module of the function.
%% <b>Id</b> : The Id of the application.
%% </pre>
%% @end
%% =====================================================================
qualify_app(MId, MId2, Id) ->
  Body = get_app_posnull_arg(MId,Id),
  Mod2Name = refactor:get_module_name(MId2),
  Module = create_nodes:create_atom(MId, Mod2Name),
  ModuleQualifier =
    create_nodes:create_module_qualifier(MId, Module, Body),
  refactor:update_application_argument(MId, Id, ModuleQualifier, 0).

%% =====================================================================
%% @spec get_applications_name_and_arity(MId::integer(), Id::integer())
%%                                              -> {string(),integer()}
%% @doc
%% This function returns the applications name and arity. Important:
%% it works only for a simple applications, which are not qualified
%% with a module qualifier.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The applications id.
%% </pre>
%% @end
%% =====================================================================
get_applications_name_and_arity(MId, Id) ->
  Name = get_name_to_simple_app(MId, Id),
  Arity =
    refactor:get_application_arity_from_application_id(MId, Id),
  {Name, Arity}.

%% =====================================================================
%% @spec import_list_maintenance(MId::integer(),
%%                               Imported::[{string(),string(),integer()}])
%%                                                             -> ok
%% @doc
%% Do the import list maintenance after the inline if it is neccessary.
%% Inserts only the neccessary imports.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Imported</b> : The import list.
%% </pre>
%% @end
%% =====================================================================
import_list_maintenance(MId, Imported) ->
  ImportIds = refactor:get_import_list_ids(MId),
  ImportData = refactor:get_imported_functions(MId, ImportIds),
  Difference = Imported -- ImportData,
  ModName = refactor:get_module_name(MId),
  if Difference == [] -> ok;
    true ->
      ImportSorted = lists:sort(Imported),
      Blocks = create_import_blocks(ImportSorted, []),
      FormId = refactor:get_form_list_id_from_mid(MId),
      FormElem = erl_syntax_db:form_list_elements(MId, FormId),
      lists:foreach(fun (Elem) ->
        ActualName = element(1,hd(Elem)),
        if ModName == ActualName ->
            ok;
          true ->
            AttrId = create_import_attr(MId, Elem),
	    create_nodes:attach_subtree_to_node(MId, AttrId, FormId, hd(FormElem))
        end
      end, Blocks)
  end.

%% =====================================================================
%% @spec is_app_qualified(MId::integer(),Id::integer())
%%                                                 -> ok
%% @doc
%% Returns true or false value.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : Applications Id.
%% </pre>
%% @end
%% =====================================================================
is_app_qualified(MId, Id) ->
  case get_applications_arg_type(MId, Id) of
    ?MODULE_QUALIFIER -> true;
    _Other -> false
  end.

%% =====================================================================
%% @spec create_import_attr(MId::integer(),
%%                          ImpElem::[{string(),string(),integer()}])
%%                                                 -> integer()
%% @doc
%% Creates an import attribute from the given list.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>ImpElem</b> : The list for import.
%% </pre>
%% @end
%% =====================================================================
create_import_attr(MId, ImpElem) ->
  Elements = 
    lists:map(fun ({_ModName, Name, Arity}) ->
      ArityNode = create_nodes:create_integer(MId, Arity),
      AtomNode = create_nodes:create_atom(MId, Name),
      create_nodes:create_arity_qualifier(MId, AtomNode, ArityNode)
    end, ImpElem),
  ListId = create_nodes:create_list(MId, Elements, none),
  NameId = create_nodes:create_atom(MId, "import"),
  ModuleName = element(1, hd(ImpElem)),
  ModuleNameId = create_nodes:create_atom(MId, ModuleName),
  create_nodes:create_attribute(MId, NameId, [ModuleNameId, ListId]).

%% =====================================================================
%% @spec create_import_blocks(Imported::[{string(),string(),integer()}],
%%                            List::[[{string(),string(),integer()}]])
%%                                 -> [[{string(),string(),integer()}]]
%% @doc
%% Split the Import functions. Create groups by the module name.( The
%% first element in the tuple.)
%%
%% Parameter description:<pre>
%% <b>Imported</b> : The list of functions to be imported.
%% <b>List</b> : The already proccessed list.
%% </pre>
%% @end
%% =====================================================================
create_import_blocks([], List) -> List;
create_import_blocks(Imported, List) ->
  {RestList, Block} = 
    create_import_block(Imported, element(1, hd(Imported)), []),
  create_import_blocks(RestList, List ++ [Block]).

%% =====================================================================
%% @spec create_import_block(Imported::[{string(),string(),integer()}],
%%                          ModName::integer(),
%%                          List::[{string(),string(),integer()}])
%%                         -> {[{string(),string(),integer()}],
%%                             [{string(),string(),integer()}]}
%% @doc
%% Processes the element from the given list with the given module name.
%%
%% Parameter description:<pre>
%% <b>Imported</b> : The list of functions to be imported.
%% <b>ModuleName</b> : The name of the actual module to be proccessed.
%% <b>List</b> : The already processed eleemnts.
%% </pre>
%% @end
%% =====================================================================
create_import_block([], _, List) -> {[], List};
create_import_block([Hd | Tail], ModName, List) ->
  case element(1, Hd) of
    ModName ->
      create_import_block(Tail, ModName, List ++ [Hd]);
    _Other -> {[Hd | Tail], List}
  end.

%% =====================================================================
%% @spec set_scope(MId::integer(), List::[integer()],
%%                            ScopeId::integer())
%%                                  -> ok
%% @doc
%% Set the ids scope to the given scope id. If the node is a list
%% comprehension or a function expression, then create the a new scope
%% and the nodes scope under this node for a new scope id.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>List</b> : The list of the node ids.
%% <b>ScopeId</b> : The id of the scope.
%% </pre>
%% @end
%% =====================================================================
set_scope(MId, [Id | Tail], ScopeId) ->
  Type = erl_syntax_db:type(MId, Id),
  case Type of
    ?LIST_COMP ->
      create_nodes:create_scope(MId, Id, ScopeId),
      update_scope(MId, ScopeId, Id),
      set_scope(MId, lists:flatten(erl_syntax_db:subtrees(MId, Id)), Id);
    ?FUN_EXPR ->
      create_nodes:create_scope(MId, Id, ScopeId),
      update_scope(MId, ScopeId, Id),
      set_scope(MId, lists:flatten(erl_syntax_db:subtrees(MId, Id)), Id);
      _Other ->
        update_scope(MId, ScopeId, Id),
	set_scope(MId, lists:flatten(erl_syntax_db:subtrees(MId, Id)), ScopeId)
  end,
  set_scope(MId, Tail, ScopeId);
set_scope(_, [], _) -> ok.

%% =====================================================================
%% @spec update_scope(MId::integer(), ScopeId::integer(),
%%                            Id::integer())
%%                                  -> ok
%% @doc
%% Update the scope of the given id in the database.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>ScopeId</b> : The new scope id of the given id.
%% <b>Id</b> : The id of the node.
%% </pre>
%% @end
%% =====================================================================
update_scope(MId, ScopeId, Id) ->
    refactor_db:update("update scope set scope= " ++
			 (integer_to_list(ScopeId) ++
			    (" where mid= " ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ ";")))))).

%% =====================================================================
%% @spec return_pair(Name::string(), List::[{string(),string()}])
%%                                  -> string() | none
%% @doc
%% Returns the pair of the given string in the list.Or if there is no
%% pair for the given element it returns the atom none.
%%
%% Parameter description:<pre>
%% <b>Name</b> : The name of the examined string.
%% <b>List</b> :The list of the tuples.
%% </pre>
%% @end
%% =====================================================================	
return_pair(Name, [{Name, Second} | _Tail]) -> Second;
return_pair(Name, [{_First, _Second} | Tail]) ->
  return_pair(Name, Tail);
return_pair(_Name, []) -> none.

%% =====================================================================
%% @spec rename_variables(MId::integer(), Variables::[integer()],
%%                            Pairs::[{string(),string()}])
%%                                  -> ok
%% @doc
%% Renames varibales given in the list by using the given pairs.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Variables</b> : List of the variables in the functions clause.
%% <b>Pairs</b> : The pairs of the variable names int the arguments.
%% </pre>
%% @end
%% =====================================================================
rename_variables(MId, Variables, Pairs) ->
  lists:foreach(fun (VarId) ->
    VarName = refactor:get_name_from_name_id(MId, VarId),
    Pair = return_pair(VarName, Pairs),
    if (Pair /= VarName) and (Pair /= none) ->
        update_variable_name(MId, VarId, Pair);
      true -> ok
    end
  end, Variables).

%% =====================================================================
%% @spec get_var_pairs_for_rename(MId::integer(),MId2::integer(),
%%                                FunArgs::[integer()],
%%                                AppArgs::[integer()])
%%                                  -> ok
%% @doc
%% Returns the variable name pairs from the application arguments and 
%% from the function argumentsfor checking name collisions and
%% for renaming.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>FunArgs</b> : Ids from the function argument list.
%% <b>AppArgs</b> : Ids from the application argument list.
%% </pre>
%% @end
%% =====================================================================
get_var_pairs_for_rename(_MId,_MId2,[],[])-> [];
get_var_pairs_for_rename(MId,MId2,FunArgs,AppArgs)->
  Zipped = lists:zip(FunArgs,AppArgs),
  lists:map(fun(Elem)->
    FunArg = element(1,Elem),
    AppArg = element(2,Elem),
    FunArgType = erl_syntax_db:type(MId2,FunArg),
    AppArgType = erl_syntax_db:type(MId,AppArg),
    if FunArgType == AppArgType ->
        case FunArgType of
	  ?VARIABLE ->
            FunArgName = refactor:get_name_from_name_id(MId2, FunArg),
            AppArgName = refactor:get_name_from_name_id(MId, AppArg),
            [{FunArgName,AppArgName}];
          _Other ->
            Sub1 = erl_syntax_db:subtrees(MId2,FunArg),
            FunSubtree = 
              if Sub1 == [] -> [];
                true -> 
                  lists:flatten(hd(lists:reverse(Sub1)))
              end,
            Sub2 = erl_syntax_db:subtrees(MId,AppArg),
            AppSubtree =
              if Sub2 == [] -> [];
                true -> 
                  lists:flatten(
                    hd(lists:reverse(Sub2)))
              end,
            get_var_pairs_for_rename(MId,MId2,FunSubtree,AppSubtree)
        end;
      true->
        []
    end
  end,Zipped).

%% =====================================================================
%% @spec update_variable_name(MId::integer(), Id::integer(),
%%                            NewName::string())
%%                                  -> ok
%% @doc
%% Updates a variable name in the database for a new name.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : Id of the variable.
%% <b>NewName</b> : The new name of the variable.
%% </pre>
%% @end
%% =====================================================================
update_variable_name(MId, Id, NewName) ->
    refactor_db:update("update name set name=" ++
			 (io_lib:write_string(NewName) ++
			    (" where mid=" ++
			       (integer_to_list(MId) ++
				  (" and id = " ++
				     (integer_to_list(Id) ++ ";")))))).

%% =====================================================================
%% @spec create_matches(MId::integer(), MId2::integer(),
%%                             Pairs::[{integer(),integer()}],
%%                             FunScope::integer(),FunArgs::[integer()])
%%                                  -> [integer()]
%% @doc
%% Creates match expressions from the pairs created form the functions
%%  and applications arguments.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The Id of the applications module.
%% <b>MId2</b> : The Id of the functions module.
%% <b>Pairs</b> : The source pairs for the generating. The left and the
%%                right side of the match expression.
%% <b>FunScope</b> : The scope of the function.
%% <b>FunArgs</b> : The function's arguments.
%% </pre>
%% @end
%% =====================================================================

create_matches(MId, MId2, Pairs, FunScope, FunArgs) ->
  UnusedVar = 
    get_unused_variables_from_fun_arg(MId2, FunScope, FunArgs),
  FilteredPairs = 
    lists:filter(fun ({First, _Second}) ->
      Name = refactor:get_name_from_name_id(MId2, First),
      not(lists:member(Name, UnusedVar))
    end, Pairs),
  lists:map(fun ({Fun, App}) ->
    create_nodes:create_match_expr(MId, Fun, App)
  end, FilteredPairs).

%% =====================================================================
%% @spec do_attribute_pairs(MId::integer(),MId2::integer(),AppArgIds::[integer()],
%%             FunArgIds::[integer()],AttrPairs::[{integer(),integer()}])
%%                    -> {[{integer(),integer()}]}
%% @doc
%% Creates and returns the pairs of attributes except the variable pairs.
%% The attribute pairs are used to create match expressions.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The Id of the applications module.
%% <b>MId2</b> : The Id of the functions modul.
%% <b>AppArgIds</b> : The Ids of the applications parameters.
%% <b>FunArgIds</b> : The Ids of the functions parameters.
%% <b>AttrPairs</b> : List of integer tuples.
%% </pre>
%% @end
%% =====================================================================

do_attribute_pairs(MId, MId2, [App | AppArgIds], 
	 [Fun | FunArgIds], AttrPairs) ->
  AppType = erl_syntax_db:type(MId, App),
  FunType = erl_syntax_db:type(MId2, Fun),
  case {AppType, FunType} of
    {?VARIABLE, ?VARIABLE} ->
      do_attribute_pairs(MId, MId2, AppArgIds, FunArgIds, AttrPairs);
    _ ->
      do_attribute_pairs(MId, MId2, AppArgIds, FunArgIds, AttrPairs ++ [{Fun, App}])
  end;
do_attribute_pairs(_MId, _MId2, [], [], AttrPairs) ->
  AttrPairs.

%% =====================================================================
%% @spec check_functions_suitability(MId::integer(),MId2::integer(),
%%                                   FunClauses::[integer()],
%%                                   AppScope::integer(),
%%                                   AppId::integer(),LocalApp::[integer()],
%%                                   Path::[integer()])
%%                                  -> ok | error
%% @doc
%% Checks the functions suitability, checks the preconditions.
%% Various properties of the functions.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications modul.
%% <b>MId2</b> : The id of the functions modul.
%% <b>FunClauses</b> : The list of the function clause ids.
%% <b>AppScope</b> : The applications scope id.
%% <b>AppId</b> : The applications id.
%% <b>LocalApp</b> : The list of local applications.
%% <b>Path</b> : The path to the application.
%% </pre>
%% @end
%% =====================================================================
check_functions_suitability(MId, MId2, FunClauses,
			    AppScope, AppId, LocalApp, Path) ->
  if MId == MId2 ->
      ok;
    true ->
      if LocalApp == [] -> ok;
        true ->
          Local = lists:map(fun({Name,Arity})->
            Name ++ "/" ++ integer_to_list(Arity)
          end,LocalApp),
          refac_checks:error_handler({local_app, {Local}})
      end,
      lists:foreach(fun(FunClause)->
        Subtrees = lists:flatten(erl_syntax_db:subtrees(MId2, FunClause)),
        check_macros_and_records(MId2,Subtrees)
      end,FunClauses)
  end,
  AppArgIds = get_app_argument_ids_from_appid(MId, AppId),
  lists:foreach(fun (ClauseId) ->
    FunArgIds = 
      get_fun_argument_ids_from_scope_id(MId2, ClauseId),
    CollVar = 
      get_variable_collisions(MId, MId2, AppScope, AppArgIds, ClauseId,
							    FunArgIds, Path),
    if CollVar == [] -> ok;
      true ->
        refac_checks:error_handler({var_coll, {CollVar}})
    end
  end, FunClauses).

%% =====================================================================
%% @spec get_var_names_from_list(MId::integer(), List::[integer()])
%%                                  -> [string()]
%% @doc
%% Returns the list of the variable names from the given arglist.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>ArgList</b> : List of the argumentum ids.
%% </pre>
%% @end
%% =====================================================================
get_var_names_from_list(MId, List) ->
  lists:map(fun (Id) ->
    case erl_syntax_db:type(MId, Id) of
      ?VARIABLE ->
	refactor:get_name_from_name_id(MId, Id);
      _Other -> ""
    end
  end, List).

%% =====================================================================
%% @spec get_applications(MId2::integer(),ScopeId::integer())
%%                              -> {[integer()],[integer()],[integer()]}
%% @doc
%% Returns the local , exported and imported applications from the 
%% given function clauses.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>ScopeId</b> : Id of the expression.
%% </pre>
%% @end
%% =====================================================================
get_applications(MId2, FunClauses) ->
  ExportList = refactor:get_export_list(MId2),
  ImportIds = refactor:get_import_list_ids(MId2),
  ImportData = refactor:get_imported_functions(MId2, ImportIds),
  FunctList = 
    lists:flatten(lists:map(fun (FunClause) ->
      Ids = get_ids_from_scope(MId2, FunClause),
      ApplicationIds = get_application_ids_from_list(MId2, Ids),
      AppWithModQual = 
        lists:filter(fun (Id) ->
          case get_applications_arg_type(MId2, Id) of
            ?MODULE_QUALIFIER -> true;
	    _Other -> false
          end
        end, ApplicationIds),
      AppWithoutMod = ApplicationIds -- AppWithModQual,
      BifIds = get_bifs_from_appid_list(MId2, AppWithoutMod),
      NotQualifiedIds = AppWithoutMod -- BifIds,
      NamesAndArity = 
        lists:usort(lists:map(fun (Id) ->
          Arity =
            refactor:get_application_arity_from_application_id(MId2, Id),
 	  Name = get_name_to_simple_app(MId2, Id),
          {Name, Arity}
        end, NotQualifiedIds)),
      Exported = 
        lists:filter(fun (Elem) ->
	       lists:member(Elem, ExportList)
	end, NamesAndArity),
      NotClassified = NamesAndArity -- Exported,
      %%ExportedInClause = Exported -- NotClassified,
      Imported = 
        lists:filter(fun ({_MId, Name, Arity}) ->
	  lists:member({Name, Arity}, NotClassified)
	end, ImportData),
      LocalFunct = 
        NotClassified --
		   lists:map(fun ({_MId, Name, Arity}) -> {Name, Arity}
		   end, Imported),
      {LocalFunct, Exported, Imported}
    end, FunClauses)),
    summarize(FunctList,[],[],[]).
%% =====================================================================
%% @spec summarize(List::[{[{string(),integer()}],[{string(),integer()}],
%%                        [{string(),string(),integer()}]}],
%%                        Local::[{string(),integer()}],
%%                        Exported::[{string(),integer()}],
%%                        Imported::[{string(),string(),integer()}])
%%                    -> {[{string(),integer()}],[{string(),integer()}],
%%                        [{string(),string(),integer()}]}
%% @doc
%% Summarizes the given element is the list and return the usorted list 
%% in a tuple.
%%
%% Parameter description:<pre>
%% <b>List</b> : The list of tuples of local, exported and imported 
%%               functions.
%% <b>Local</b> : List of already processed local functions.
%% <b>Exported</b> : List of already processed exported functions.
%% <b>Imported</b> : List of already processed imported functions.
%% </pre>
%% @end
%% =====================================================================

summarize([],Local,Exported,Imported)->
  Loc = lists:usort(Local),
  Exp = lists:usort(Exported),
  Imp = lists:usort(Imported),
  {Loc,Exp,Imp};
summarize([Head|Tail],Local,Exported,Imported) ->
  summarize(Tail,Local ++ element(1,Head),Exported ++ element(2,Head),Imported ++ element(3,Head)).


%% =====================================================================
%% @spec check_macros_and_records(MId::integer(),Ids::[integer()])
%%                                  -> [string()]
%% @doc
%% Checks for macros and records in the given list. If there is a
%% record or a macro it throws an exception.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Ids</b> : List of nodes.
%% </pre>
%% @end
%% =====================================================================
check_macros_and_records(MId,Ids)->
  lists:foreach(fun(Id)->
    case erl_syntax_db:type(MId,Id) of
      ?MACRO ->
        refac_checks:error_handler({macro_in_funclause, {Id}});
      ?RECORD_ACCESS ->
        refac_checks:error_handler({record_in_funclause, {Id}});
      ?RECORD_EXPR ->
        refac_checks:error_handler({record_in_funclause, {Id}});
      _Other ->
        ok
    end
  end,Ids).
%% =====================================================================
%% @spec get_unused_variables_from_fun_arg(MId2::integer(),
%%                                         FunScope::integer(),
%%                                         FunArgs::[integer()])
%%                                  -> [string()]
%% @doc
%% Returns the unused variables which are in the argument list, but
%% not used int the functions body.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>FunScopeId</b> : Functions scope, clause id.
%% <b>FunArgs</b> : List of the function arguments.
%% </pre>
%% @end
%% =====================================================================
get_unused_variables_from_fun_arg(MId2, FunScope, FunArgs) ->
  Ids = 
  lists:flatten(lists:map(fun(Id)->
                  refac_common:get_subtrees(MId2, Id ,with_root)
                end,FunArgs)),
  FunArgNames = get_var_names_from_list(MId2, Ids),
  FunArgDupl = FunArgNames ++ FunArgNames,
  FunVarIds = lists:flatten(refac_common:get_variables(MId2, FunScope, with_root)),
  FunVariables = get_var_names_from_list(MId2, FunVarIds),
  FunArgDupl -- FunVariables.

%% =====================================================================
%% @spec get_variable_collisions(MId::integer(),MId2::integer(),
%%                                   AppScope::integer(),
%%                                   AppArgs::[integer()],
%%                                   FunScope::integer(),
%%                                   FunArgs::[integer()],
%%                                   Path::[integer()])
%%                                  -> [string()]
%% @doc
%% Returns the clashing variable names. If there is no collision, 
%% then returns empty list.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the applications module.
%% <b>MId2</b> : The id of the functions module.
%% <b>AppScope</b> : Aplications scope id.
%% <b>AppArgs</b> : List of the application arguments.
%% <b>FunScope</b> : Functions scope, clause id.
%% <b>FunArgs</b> : List of the function arguments.
%% <b>Path</b> : The path to the application.
%% </pre>
%% @end
%% =====================================================================
get_variable_collisions(MId, MId2, AppScope, AppArgs, FunScope, FunArgs, Path) ->
  AppVariables = refac_common:get_variables(MId, AppScope, with_root),
  Filtered =
    lists:filter(fun(Id)->
      Binding = refactor:get_var_bind_occ_from_id(MId, Id),
      Scope = refactor:get_scope_from_id(MId, Binding),
      lists:member(Scope,Path)
    end, AppVariables),
  FunVariables = refac_common:get_variables(MId2, FunScope, with_root),
  AppVarNames = 
    lists:usort(lists:map(fun (Id) ->
      refactor:get_name_from_name_id(MId, Id)
    end, Filtered)),
  FunVarNames = 
    lists:usort(lists:map(fun (Id) ->
      refactor:get_name_from_name_id(MId2, Id)
    end, FunVariables)),
  Pairs = 
    lists:flatten(get_var_pairs_for_rename(MId,MId2,FunArgs,AppArgs)),
  VariablesFromPairs =
    lists:map(fun(Elem)->
      element(1,Elem)
    end,Pairs),
  AppVarNames -- (AppVarNames -- (FunVarNames -- VariablesFromPairs)).



%% =====================================================================
%% @spec get_bifs_from_appid_list(MId::integer(),List::[integer()])
%%                                               -> [Id]
%% @doc
%% Filters the built in functions from the list of ids.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>List</b> : List of the ids.
%% </pre>
%% @end
%% =====================================================================
get_bifs_from_appid_list(MId, List) ->
  _Bifs = 
    lists:filter(fun (Id) ->
      Name = get_name_to_simple_app(MId, Id),
      Type = refactor:get_forbidden_name_type_from_the_name(Name),
      if Type == [{1}] -> true;
        true -> false
      end
    end, List).

%% =====================================================================
%% @spec get_name_to_simple_app(MId::integer(),AppId::integer())
%%                                  -> string()
%% @doc
%% Returns the data of the selected expression.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>AppId</b> : Application's Id.
%% </pre>
%% @end
%% =====================================================================
get_name_to_simple_app(MId, AppId) ->
  [{Name}] =
    refactor_db:select("select name from application a,name "
			 "n where a.mid = "
			   ++
			     (integer_to_list(MId) ++
				(" and a.mid=n.mid and a.id=" ++
				   (integer_to_list(AppId) ++
				      " and a.argument=n.id and a.pos=0;")))),
  Name.


%% =====================================================================
%% @spec get_applications_arg_type(MId::integer(),AppId::integer())
%%                                  -> integer()
%% @doc
%% Returns the applications argument type at the 0 position.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : Id of the expression.
%% </pre>
%% @end
%% =====================================================================
get_applications_arg_type(MId, AppId) ->
  [{Type}] =
    refactor_db:select("select type from application a,node_type "
			 "n where a.mid = "
			   ++
			     (integer_to_list(MId) ++
				(" and a.mid=n.mid and a.id=" ++
				   (integer_to_list(AppId) ++
				      " and a.argument=n.id and a.pos=0;")))),
    Type.

%% =====================================================================
%% @spec get_data_about_function(MId::integer(),AppId::integer())
%%                                  -> {MId2::integer(),
%%                                      FunId::integer(),
%%                                      FunClauses::[integer()]}
%% @doc
%% Returns the data about the funtion. The modul id, the function id
%% and the id of the clauses of the function.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : Id of the expression.
%% </pre>
%% @end
%% =====================================================================
get_data_about_function(MId, AppId) ->
  {MId2, FunId} = find_the_functions_module_clause_id(MId, AppId),
  FunClauseIds = refactor:get_clause_ids_from_fun_id(MId2, FunId),
  UntupledFunClauses =
    erl_syntax_db:untuple(FunClauseIds),
  {MId2, UntupledFunClauses}.

%% =====================================================================
%% @spec get_path_to_app(MId::integer(),Id::integer())
%%                                  -> {[integer()],integer()}
%% @doc
%% Returns the path and the scope of the given id.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : Id of the expression.
%% </pre>
%% @end
%% =====================================================================

get_path_to_app(MId, Id) ->
  ScopeId = refactor:get_scope_from_id(MId, Id),
  OutermostScope = get_outermost_scope_id(MId, ScopeId),
  {_Found, Path, _Ids} =
    refac_common:find_expression_root_id(MId, OutermostScope, Id, Id),
  {Path, ScopeId}.

%% =====================================================================
%% @spec get_outermost_scope_id(MId::integer(),Id::integer())
%%                                  -> integer()
%% @doc
%% Returns the otermost scope id.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : Id of the expression.
%% </pre>
%% @end
%% =====================================================================

get_outermost_scope_id(MId, Id) ->
  ActualType = erl_syntax_db:type(MId, Id),
  case ActualType of
    ?CLAUSE -> Id;
    _ ->
      OuterScope = refactor:get_containing_scope_id(MId, Id),
      get_outermost_scope_id(MId, OuterScope)
  end.

%% =====================================================================
%% @spec get_id_and_type_from_position(MId::integer(),
%%                Line::integer(), Col::integer()) -> {integer(),integer()}
%%
%% @doc
%% Returns the id and type of the pointed location.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Line</b> : The pointed line in the code.
%% <b>Col</b> : The pointed column in the code.
%% </pre>
%% @end
%% =====================================================================

get_id_and_type_from_position(MId, Line, Col) ->
  CLineCol = refactor:get_true_pos_from_pointed_pos(MId, Line, Col),
  {CLine, CCol} = 
    if CLineCol == [] -> 
        refac_checks:error_handler({pos_error, {Line, Col}});
      true -> CLineCol
    end,
  Id = 
    if CLineCol == [] ->
        refac_checks:error_handler({pos_error, {Line, Col}});
      true ->
        if CLine /= Line ->
	    refac_checks:error_handler({pos_error, {Line, Col}});
	  true ->
	    refac_common:get_lowest_id(MId, refactor:get_id_from_pos(MId,
								     CLine,
								     CCol))
	end
    end,
  if Id == [] ->
      refac_checks:error_handler({pos_error, {CLine, CCol}});
    true -> ok
  end,
  Type = erl_syntax_db:type(MId, Id),
  {Id, Type}.

%% =====================================================================
%% @spec get_application_id_from_argument(MId::integer(),
%%                Argument::integer()) -> [{integer()}]
%%
%% @doc
%% Returns the application id from the argument id.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Argument</b> : The id of the argument.
%% </pre>
%% @end
%% =====================================================================

get_application_id_from_argument(MId, Argument) ->
  refactor_db:select("select id from application where mid = "
		       ++
			 (integer_to_list(MId) ++
			    (" and pos= 0 and argument=" ++
			       (integer_to_list(Argument) ++ ";")))).

%% =====================================================================
%% @spec get_module_qualifier(MId::integer(),Id::integer())
%%                                                     -> [{integer()}]
%%
%% @doc
%% Returns the module qualifiers id from argument.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Argument</b> : The id of the argument.
%% </pre>
%% @end
%% =====================================================================

get_module_qualifier(MId, Id) ->
  refactor_db:select("select id from module_qualifier where "
		       "mid = "
			 ++
			 (integer_to_list(MId) ++
			    (" and module=" ++
			       (integer_to_list(Id) ++
				  (" or body=" ++
				     (integer_to_list(Id) ++ ";")))))).

%% =====================================================================
%% @spec find_the_functions_module_clause_id(MId::integer(),
%%                AppId::integer()) -> {MId2::integer(),FunId::integer()}
%%
%% @doc
%% Returns the functions source moduls id and the functions clause id
%% from the applications id. throws an exception if the functions module
%% is not loaded.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>AppId</b> : The id of the application.
%% </pre>
%% @end
%% =====================================================================

find_the_functions_module_clause_id(MId, AppId) ->
  FunModandId =
    refactor:get_fun_id_from_implicit_fun_id_or_application(MId, AppId),
  if FunModandId == [] ->
      [{ModuleName}] = get_module_name_from_app_id(MId, AppId),
      refac_checks:error_handler({not_loaded, {ModuleName}});
    true -> [{MId2, FunId}] = FunModandId, {MId2, FunId}
  end.

%% =====================================================================
%% @spec get_module_name_from_app_id(MId::integer(),
%%                AppId::integer()) -> [{MId2::integer()}]
%%
%% @doc
%% Returns the applications source modul name.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>AppId</b> : The id of the application.
%% </pre>
%% @end
%% =====================================================================
get_module_name_from_app_id(MId, AppId) ->
  refactor_db:select("select module from fun_cache where mid = "
		       ++
			 (integer_to_list(MId) ++
			    (" and id = " ++ (integer_to_list(AppId) ++ ";")))).

%% =====================================================================
%% @spec get_ids_from_scope(MId::integer(),ScopeId::integer()) ->
%%                                                Ids::[integer()]
%%
%% @doc
%% Returns the ids from the selected scope .
%%
%% Parameter description:<pre>
%% <b>MId</b> : The module id.
%% <b>ScopeId</b> : The id of the scope.
%% </pre>
%% @end
%% =====================================================================

get_ids_from_scope(MId, ScopeId) ->
  List =
    refactor_db:select("select id from scope where mid = "
			  ++
			     (integer_to_list(MId) ++
				(" and scope = " ++
				   (integer_to_list(ScopeId) ++ ";")))),
  erl_syntax_db:untuple(List).

%% =====================================================================
%% @spec get_application_ids_from_list(MId::integer(),List::[integer()]) ->
%%                                                 AppIds::[integer()]
%%
%% @doc
%% Returns the filtered list of application ids. It is used to filter
%% check if the function body is suitable to inline.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The module id.
%% <b>List</b> : The list of ids.
%% </pre>
%% @end
%% =====================================================================
get_application_ids_from_list(MId, List) ->
  lists:filter(fun (Elem) ->
    Type = erl_syntax_db:type(MId, Elem),
    case Type of
      ?APPLICATION -> true;
      _Other -> false
    end
  end, List).

%% =====================================================================
%% @spec get_app_argument_ids_from_appid(MId::integer(),
%%                                                 AppId::integer()) ->
%%                                               [integer()]
%%
%% @doc
%% Returns the application argument ids.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The module id of the selected application.
%% <b>AppId</b> : The id of the selected application.
%% </pre>
%% @end
%% =====================================================================

get_app_argument_ids_from_appid(MId, AppId) ->
  List =
    refactor_db:select("select argument from application where "
			 "mid = "
			   ++
			     (integer_to_list(MId) ++
				(" and id = " ++
				   (integer_to_list(AppId) ++
				      " and pos != 0;")))),
  erl_syntax_db:untuple(List).

%% =====================================================================
%% @spec get_fun_argument_ids_from_scope_id(MId2::integer(),
%%                             FunScopeId::integer())->Names
%%
%% @doc
%% Returns the functions argument ids from the mid and the functions
%% scope id.
%%
%% Parameter description:<pre>
%% <b>MId2</b> : The module id.
%% <b>FunscopeId</b> : The id of the functions body.
%% </pre>
%% @end
%% =====================================================================
get_fun_argument_ids_from_scope_id(MId2, FunScopeId) ->
  List =
    refactor_db:select("select argument  from clause where mid = "
			 ++
			     (integer_to_list(MId2) ++
				(" and id = " ++
				   (integer_to_list(FunScopeId) ++
				      " and qualifier = 0;")))),
  erl_syntax_db:untuple(List).

%% =====================================================================
%% @spec parent_id_and_previous_node(MId::integer(),Id::integer(),
%%                        Path::[integer()]) ->
%%                                      {integer(),integer() | first}
%%
%% @doc
%% Returs the parent id of the given id and the previous node in the
%% syntax tree. This is necessary information where to insert the body
%% of the function.
%%
%% Parameter description:<pre>
%% <b>MId</b> : The module id.
%% <b>Id</b> : The id of the selected application.
%% <b>Path</b> : The path to the selected application from the root.
%% </pre>
%% @end
%% =====================================================================

parent_id_and_previous_node(MId, Id, Path) ->
  Reverse = lists:reverse(Path),
  Parent = hd(tl(Reverse)),
  Subtrees = erl_syntax_db:subtrees(MId, Parent),
  Children = lists:flatten(hd(lists:reverse(Subtrees))),
  RevChildren = lists:reverse(Children),
  Following = get_following_id_in_list(Id, RevChildren),
  {Parent, Following}.

%% =====================================================================
%% @spec get_following_id_in_list(Id::integer(),List::[integer()]) ->
%%                                                first | integer()
%%
%% @doc
%% Return the id which follows the given id. Returns constant first if
%% the given id is the first in the given list.
%%
%% Parameter description:<pre>
%% <b>Id</b> : The given id.
%% <b>List</b> : The list of ids.
%% </pre>
%% @end
%% =====================================================================

get_following_id_in_list(Id, [Head | Tail]) ->
  if Head == Id ->
      if Tail == [] -> first;
        true -> hd(Tail)
      end;
    true -> get_following_id_in_list(Id, Tail)
  end;
get_following_id_in_list(_Id, []) -> first.

