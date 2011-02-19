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


%% @doc This is an interface package to the database. It contains various
%%      usefull library functions. It is a bit of a mess right now. 
%%      It has to be further divided into modules.
%% @end

-module(refactor).

-export([get_functions/1,get_module_names/0,
	 get_module_name_if_exists_in_module/3,get_imported_functions/2,
	 get_imported_functions_and_ids/2, get_export_list/1,
	 get_export_list_id/1, remove_binding_occurrence/3,
	 replicate_subtree/4, replicate_subtree/3, 
         connect_variable_to_first_occurrence/3,
	 matcher/2,stop/1,get_last_match/1,get_match_of/2,
	 %%nem biztos hogy kell
	 get_matches_of/2, add_match/3, is_compile_export_all/1,
	 not_end_of_block_expr/2, not_end_of_clause_body/2,
	 not_end_of_receive_expr_action/2, not_end_of_try_expr_body/2,
	 not_end_of_try_expr_after/2, not_end_of_try_expr_handlers/2,
	 remove_occurrence_from_tree/2, get_if_not_end_of_block_expr/2,
	 get_if_not_end_of_clause_body/2, 
	 get_if_not_end_of_receive_expr_action/2, 
	 get_if_not_end_of_try_expr_body/2, get_if_not_end_of_try_expr_after/2,
	 get_if_not_end_of_try_expr_handlers/2, delete_row/5, 
	 get_module_id_from_path_if_exists/1, get_module_id_from_path/1,
	 get_module_name_from_attribute_id/2, get_module_name/1,
	 get_module_ids_where_used_from_fun_id/2, 
	 get_module_qualifier_body_form_module_qualifier_id/2,
	 get_clause_argument_ids_from_scope/2,
	 get_forbidden_name_type_from_the_name/1,
	 get_arity_qualifier_ids_from_attribute_id/2,
	 get_arity_qualifier_data/2,
	 get_arity_qualifier_data_and_id/2,
	 get_arity_qualifier_body_form_arity_qualifier_id/2,
	 get_fun_id_from_clause_id/2,
	 get_fun_id_from_clause_id_function_table/2,
	 get_id_and_type_list_of_functions_from_pos/3,
	 get_id_and_type_list_of_functions_from_pos_no_implicit_fun/3,
	 get_fun_id_from_implicit_fun_id_or_application/2,
	 get_clause_mids_and_ids_from_fun_id/2,
	 get_clause_ids_from_fun_id/2,
	 get_argument_position_from_clause_id_and_pos/4,
	 get_argument_position_from_application_id_and_pos/4,
	 get_argument_nearest_position_and_type_from_clause_id_and_pos/4,
	 get_argument_nearest_position_and_type_from_application_id_and_pos/4,
	 get_max_argument_position_from_clause_id/2,
	 get_max_argument_position_from_application_id/2,
	 get_arguments_from_clause_id_and_from_to_pos/4,
	 get_arguments_from_application_id_and_from_to_pos/4,
	 get_arity_from_arity_id/2,
	 get_arity_from_fun_id/2,
	 get_application_arity_from_application_id/2,
	 get_fun_name_id_from_fun_id/2,
	 get_fun_name/2,
	 get_application_type_mid_id_from_fun_call_id/2,
	 get_implicit_fun_call_type_from_implicit_fun_id/2,
	 get_implicit_fun_call_type_mid_id_from_fun_call_id/2,
	 get_id_from_pos/3,
	 get_import_list_ids/1,
	 get_infix_expr_oper_from_tree/2,
	 get_match_expr_id_from_pattern_and_body_id/3,
	 get_match_expr_id_and_body_from_pattern/2,
	 get_match_expr_patterns_from_scopes/2,
	 get_name_from_name_id/2,
	 get_name_and_pos_from_name_id/2,
	 get_true_pos_from_pointed_pos/3,
	 get_pos_from_id/2,
	 get_scope_from_id/2,
	 get_inner_scope_ids_from_scope_id/2,
	 get_inner_scope_ids_from_scope_and_var_id/3,
	 get_outer_scope_ids_from_scope_and_var_id/3,
	 get_tuple_element_ids_from_tuple_id/2,
	 get_var_id_from_pos/3,
	 get_var_bind_occ_from_id/2,
	 get_var_bind_occ_and_scope_from_id/2,
	 get_clashed_var_id_from_scope_and_name/3,
	 get_pattern_var_id_from_var_bid_and_scope/3,
	 get_shadows_scope_and_bid_from_var_id/3,
	 get_every_fun_call_and_type_from_fun_id/2,
	 get_every_type_and_fun_call_from_fun_id/2,
	 get_every_mid/0,
	 get_all_function_id_from_the_module/1,
	 get_all_arity_from_the_module/1,
	 get_every_occurrence_of_a_var_from_id/2,
	 get_all_dynamic_fun_call_type/0,
         check_if_apply_exists_in_system/0,
         check_if_spawn_exists_in_system/0,
         check_if_hibernate_exists_in_system/0,
	 get_all_fun_name_and_fun_id/1,
	 get_all_simple_application_mid_id_and_name/1,
	 get_all_module_application_mid_id_module_name_and_fun_name/1,
	 get_all_deprecated_application_mid_id_and_tuple_id/1,
	 get_deprecated_application_name_and_module_name_from_tuple_id/2,
	 get_all_implicit_fun_module_id_id_name_id_and_type/1,
	 get_simple_implicit_fun_name_and_arity/2,
	 get_module_implicit_fun_module_name_name_and_arity/2,
	 delete_application_argument_between_pos/4,
	 delete_clause_argument_between_pos/4,
	 delete_match_expr/2,
	 delete_element_from_pos/2,
	 delete_element_from_node_type/2,
	 delete_element_from_scope/2,
	 drop_from_dbase/1,
	 insert_precomment_padding/4,
	 insert_precomment_text/4,
	 insert_postcomment_padding/4,
	 insert_postcomment_text/4,
	 insert_fun_visib_data/4,
	 insert_scope/3,
	 update_application/3,
	 update_clause_argument/4,
	 update_application_argument/4,
	 update_application_pos_between_given_positions/4,
	 update_clause_pos_between_given_positions/4,
	 update_clause_pos/3,
	 update_fun_visib/3,
	 update_function_name/3,
	 update_integer/3,
	 update_variable_name/3,
	 commit/0,
	 exists_in_dbase/1,
	 set_position/3,
	 put_into_dbase_fun_cache/2,
	 put_fun_calls_into_dbase1/2,
	 put_fun_calls_into_dbase2/2,
	 get_calls_for_this_module/1,
	 get_needed_funs_from_dbase_cache/1,
	 put_visib_in_database/2,
	 put_scope_visib_in_database/2,
	 get_form_list_id_from_mid/1,
	 simple_member_b/3,
	 get_containing_scope_id/2,
	 insert_fun_cache/5, insert_fun_call/3,
	 insert_var_visib/3, update_scope_visibility/3, 
	 update_var_visib/3, update_scope/4, 
	 get_fun_expr_clause_from_fun_id/2,
	 get_scope_visib/2]).

-export([create_condition_list/1, create_condition_list/2, 
	 create_condition_list3/2]).

%%%Funcionalities

%% =====================================================================
%% @spec get_functions(MId::integer()) -> [{string(),integer()}]
%%
%% @doc
%% Returns the functions' data (name, arity) which are in the module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.</pre>
%% @end
%% Origin: refac_var_elim
%% =====================================================================
get_functions(MId) ->
    FunctionIdsNames = 
	refactor_db:select( 
	  "select f.id,name from name as n,function as f"
	  " where n.mid=f.mid and n.mid=" 
	  ++ integer_to_list(MId) ++ " and f.pos=0"
	  " and n.id=f.clause;"),
    lists:map(fun ({FunId,Name}) -> 
		      [{Arity}] = 
			  refactor_db:select( 
			    "select argument from fun_visib"
			    " where mid=" ++ integer_to_list(MId) 
			    ++ " and id=" ++ integer_to_list(FunId) 
			    ++ " and pos=0;"), 
		      {Name, Arity} end,FunctionIdsNames).

%% =====================================================================
%% @spec get_module_names() -> [string()]
%%
%% @doc
%% Returns the modules' name which are in the refactoring system.
%% 
%% @end
%% =====================================================================
get_module_names() ->
    MIds = refactor_db:select("select mid from module;"),
    ModuleNames = lists:map(fun({MId}) -> 
				    Module = get_module_name(MId), 
				    Module end,MIds),
    ModuleNames.

%% =====================================================================
%% @spec get_module_name_if_exists_in_module(MId::integer(),
%%          Name::string(), Arity::integer()) -> Result
%%              Result = bool() | string()
%%
%% @doc
%% Returns the module name, if the function is in the module, 
%% otherwise it returns false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : Name of the function.
%% <b>Arity</b> : Arity of the function.</pre>
%% @end
%% =====================================================================
get_module_name_if_exists_in_module(MId, Name, Arity) ->
    Exists = 
	refactor_db:select(
	  "select n.mid from function as f, fun_visib fv ,name as n where"
	  " f.mid=fv.mid and fv.mid=n.mid and n.mid=" ++ integer_to_list(MId) 
	  ++ " and fv.pos=0 and fv.argument=" ++ integer_to_list(Arity) 
	  ++ " and fv.id=f.id and f.pos=0 and f.clause=n.id and n.name=" 
	  ++ io_lib:write_string(Name) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    get_module_name(MId)
    end.

%% =====================================================================
%% @spec get_imported_functions(MId::integer(),
%%          ImportListIds::[{integer()}]) -> Result
%%              Result = [{string(),string(),integer()}]
%%
%% @doc
%% Collects the imported functions (module name, name, arity) 
%% from a module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ImportListIds</b> : The module's importlists ids.</pre>
%% @end
%% origin: refac_ren_fun:get_datas_from_imported_too/5, 
%%         refac_ren_fun:get_datas/3
%% =====================================================================
get_imported_functions(MId, ImportListIds) ->
    ImportDataList = 
	lists:map(fun({Id}) -> 
			  [{ModuleName}] =
			      refactor_db:select(
				"select name from name,attribute_ where"
				" name.mid=attribute_.mid and"
				" name.id=attribute_.argument"
				" and pos=1 and attribute_.id="
				++ integer_to_list(Id) ++ " and name.mid="
				++ integer_to_list(MId) ++ " ;"),
			  ArityQualifierIds =
			      refactor_db:select(
				"select body,arity from"
				" arity_qualifier ar,attribute_ a,"
				" list l where ar.mid=a.mid and a.mid=l.mid"
				" and a.argument=l.id and l.element=ar.id"
				" and a.mid=" ++ integer_to_list(MId) 
				++ " and a.id=" ++ integer_to_list(Id)++ ";"),
			  ArityQualifierDatas = 
			      get_arity_qualifier_data(
				MId, ArityQualifierIds),
			  ImportDatas = 
			      add_element_to_tuples(
				ModuleName, ArityQualifierDatas),
			  ImportDatas end,
		  ImportListIds),
    lists:flatten(ImportDataList).

%% =====================================================================
%% @spec get_imported_functions_and_ids(MId::integer(),
%%          ImportListIds::[{integer()}]) -> Result
%%              Result = [{string(),string(),integer(),integer(),
%%                        integer(),integer()}]
%%
%% @doc
%% Collects the imported functions (module name, name, arity, 
%%  body id, arity id) from a module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ImportListIds</b> : The module's importlists ids.</pre>
%% @end
%% =====================================================================
get_imported_functions_and_ids(MId, ImportListIds) ->
    ImportDataList = 
	lists:map(fun({Id}) -> 
			  [{ModuleName}] =
			      refactor_db:select(
				"select name from name, attribute_ where"
				" name.mid=attribute_.mid"
				" and name.id=attribute_.argument"
				" and pos=1 and attribute_.id="
				++ integer_to_list(Id) ++ " and name.mid="
				++ integer_to_list(MId) ++ " ;"),
			  ArityQualifierIds =
			      refactor_db:select(
				"select body,arity from arity_qualifier ar,"
				" attribute_ a,list l where ar.mid=a.mid"
				" and a.mid=l.mid and a.argument=l.id and"
				" l.element=ar.id and a.mid=" 
				++ integer_to_list(MId) ++ " and a.id=" 
				++ integer_to_list(Id)++ ";"),
			  ArityQualifierDatas = 
			      get_arity_qualifier_data(
				MId, ArityQualifierIds),
			  ImportDatas = 
			      add_element_to_tuples(
				ModuleName, ArityQualifierDatas),
			  ImportDatasIds= 
			      lists:zipwith(
				fun({BodyId,ArityId},
				    {ModuleName2,FunName,Arity}) -> 
					{ModuleName2,FunName,Arity,
					 MId,BodyId,ArityId} end,
				ArityQualifierIds,ImportDatas),
			  ImportDatasIds end,ImportListIds),
    lists:flatten(ImportDataList).

%% =====================================================================
%% @spec get_export_list(MId::integer()) -> 
%%             [{string(), integer()}]
%%
%% @doc
%% Collects the exported functions (name, arity) from the module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.</pre>
%% @end
%% =====================================================================
get_export_list(MId) ->
    ExportListElements = get_export_list_id(MId),
    get_arity_qualifier_data(MId, ExportListElements).

%% =====================================================================
%% @spec get_export_list_id(MId::integer()) -> 
%%             [{string(), integer()}]
%%
%% @doc
%% Collects the exported functions (body id, arity id) from the module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.</pre>
%% @end
%% Origin:refac_ren_fun:get_datas/4, 
%%        refac_tuple_funpar:refresh_export_list/5
%% =====================================================================
get_export_list_id(MId)->
    ExportListElementIds = 
	refactor_db:select(
	  "select list.element from form_list, node_type,attribute_,list where"
	  " node_type.mid=form_list.mid and attribute_.mid=list.mid and"
	  " list.mid=form_list.mid and list.mid=" ++ integer_to_list(MId) 
	  ++ " and node_type.id=form_list.form and node_type.type=4 and"
	  " attribute_.id=form and list.id=argument and attribute_.pos=1 and"
	  " attribute_.id in (select attribute_.id from attribute_,name where"
	  " attribute_.mid=name.mid and name.mid=" ++ integer_to_list(MId) 
	  ++ " and attribute_.argument=name.id" ++ " and name=\"export\");" ),
    lists:map( fun({ElementIds}) -> 
		       [{BodyId, ArityId}] = 
			   refactor_db:select(
			     "select body,arity from arity_qualifier where"
			     " mid=" ++ integer_to_list(MId) ++ " and id=" 
			     ++ integer_to_list(ElementIds) ++ ";"),
		       {BodyId, ArityId} end, ExportListElementIds ).

%% =====================================================================
%% @spec add_element_to_tuples(Element::term(), List::[tuple()]) -> 
%%             [tuple()]
%%
%% @doc
%% Extends the tuples in a list with a new element.
%% 
%% Parameter description:<pre>
%% <b>Element</b> :The term with we want to extend the tuples.
%% <b>List</b> : List of tuples.</pre>
%% @end
%% origin: refactor:get_imported_functions_and_ids/3
%% =====================================================================
add_element_to_tuples(_Element, []) ->[];
add_element_to_tuples(Element, [X|Xs]) -> 
    [(list_to_tuple([Element] ++ tuple_to_list(X)))
     |add_element_to_tuples(Element,Xs)].

%% =====================================================================
%% @spec create_condition_list(List::[{integer(), integer()}]) -> string()
%%
%% @doc
%% Creates a condition list from the data in the list.
%% 
%% Parameter description:<pre>
%% <b>List</b> : Contains nodes (module and node id).</pre>
%% @end
%% origin: refac_reorder_funpar
%% =====================================================================
create_condition_list([]) ->
    [];
create_condition_list([{MId, Id}| Xs]) ->
    create_condition_list(Xs," (mid=" ++ integer_to_list(MId) 
			  ++ " and id=" ++ integer_to_list(Id) ++ ")");
create_condition_list([{_X, MId, Id}| Xs]) ->
    create_condition_list(Xs," (mid=" ++ integer_to_list(MId) 
			  ++ " and id=" ++ integer_to_list(Id) ++ ")").

%% =====================================================================
%% @spec create_condition_list(List::[{integer(), integer()}], 
%%             ConditionList::string()) -> string()
%%
%% @doc
%% Creates a condition list from the data in the list.
%% 
%% Parameter description:<pre>
%% <b>List</b> : Contains nodes (module and node id).
%% <b>ConditionList</b> : Part of the result condition list.</pre>
%% @end
%% origin: refac_reorder_funpar
%% =====================================================================
create_condition_list([], ConditionList) ->
    ConditionList;
create_condition_list([{MId, Id}| Xs], ConditionList) ->
    create_condition_list( Xs, ConditionList ++ " or (mid=" 
			   ++ integer_to_list(MId) 
			   ++ " and id=" ++ integer_to_list(Id) ++ ")");
create_condition_list([{_X, MId, Id}| Xs], ConditionList) ->
    create_condition_list( Xs, ConditionList ++ " or (mid=" 
			   ++ integer_to_list(MId) 
			   ++ " and id=" ++ integer_to_list(Id) ++ ")").

%% =====================================================================
%% @spec create_condition_list2(Value1::string(), Value2::string(), 
%%        List::[{integer(), integer(), string()}] ) -> string()
%%
%% @doc
%% Creates a condition list from the parameters.
%% 
%% Parameter description:<pre>
%% <b>Value1</b> : Contains a string used in the condition list.
%% <b>Value2</b> : Contains a string used in the condition list.
%% <b>List</b> : Contains variables (node id, first occurence id, name).
%% </pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
create_condition_list2(_Value1, _Value2, []) ->
    " ( true ) ";
create_condition_list2(Value1, Value2, [{_Id, BId, Name} | Xs]) ->
    create_condition_list2(
      "((" ++ Value1 ++ io_lib:write_string(Name) ++ " and "
      ++ Value2 ++ integer_to_list(BId) ++ ")", Value1, Value2, Xs).

%% =====================================================================
%% @spec create_condition_list2(ConditionList::string(), 
%%                   Value1::string(), Value2::string(), 
%%        List::[{integer(), integer(), string()}] ) -> string()
%%
%% @doc
%% Creates a condition list from the parameters.
%% 
%% Parameter description:<pre>
%% <b>ConditionList</b> : Part of the result condition list.
%% <b>Value1</b> : Contains a string used in the condition list.
%% <b>Value2</b> : Contains a string used in the condition list.
%% <b>List</b> : Contains variables (node id, first occurence id, name).
%% </pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
create_condition_list2(List, _Value1, _Value2,[]) ->
    List++")";
create_condition_list2(List, Value1, Value2, [{_Id, BId, Name} | Xs]) ->
    create_condition_list2(
      List ++ " or (" ++ Value1 ++ io_lib:write_string(Name) ++ " and "
      ++ Value2 ++ integer_to_list(BId) ++ ")", Value1, Value2, Xs).

%% =====================================================================
%% @spec create_condition_list3(Value::string(), 
%%        List::[integer()]) -> string()
%%
%% @doc
%% Creates a condition list from the parameters.
%% 
%% Parameter description:<pre>
%% <b>Value</b> : Contains a string used in the condition list.
%% <b>List</b> : Contains scope ids. </pre>
%% @end
%% origin: get_clause_argument_ids_from_scope/3
%% =====================================================================
create_condition_list3(_Value, []) ->
    "( true )";
create_condition_list3(Value, [Id |Scopes]) ->
    create_condition_list3( "( " ++ Value ++ integer_to_list(Id), 
			    Value, Scopes).

%% =====================================================================
%% @spec create_condition_list3(ConditionList::string(), Value::string(), 
%%        List::[integer()]) -> string()
%%
%% @doc
%% Creates a condition list from the parameters.
%% 
%% Parameter description:<pre>
%% <b>ConditionList</b> : Part of the result condition list.
%% <b>Value</b> : Contains a string used in the condition list.
%% <b>List</b> : Contains scope ids. </pre>
%% @end
%% origin: get_clause_argument_ids_from_scope/3
%% =====================================================================
create_condition_list3(List, _Value, []) ->
    List ++ " )";
create_condition_list3(List, Value, [ Id | Xs]) ->
    create_condition_list3(List ++ " or " ++ Value ++ integer_to_list(Id),
			   Value, Xs).

%% =====================================================================
%% @spec create_condition_list4(Value::string(), 
%%        List::[integer()]) -> string()
%%
%% @doc
%% Creates a condition list from the parameters.
%% 
%% Parameter description:<pre>
%% <b>Value</b> : Contains a string used in the condition list.
%% <b>List</b> : Contains node ids. </pre>
%% @end
%% origin: into_db:get_needed_funs_from_dbase_cache/2
%% =====================================================================
create_condition_list4(_String, []) ->
    ";";
create_condition_list4(String, [X | Xs]) ->
    create_condition_list4(String, Xs, " and (" ++ String 
			   ++ io_lib:write_string(X)).

%% =====================================================================
%% @spec create_condition_list4(Value::string(), 
%%        List::[integer()], ConditionList::string()) -> string()
%%
%% @doc
%% Creates a condition list from the parameters.
%% 
%% Parameter description:<pre>
%% <b>Value</b> : Contains a string used in the condition list.
%% <b>List</b> : Contains node ids.
%% <b>ConditionList</b> : Part of the result condition list. </pre>
%% @end
%% origin: into_db:get_needed_funs_from_dbase_cache/2
%% =====================================================================
create_condition_list4(_String, [], ConditionList) ->
    ConditionList ++ ");";
create_condition_list4(String, [X | Xs], ConditionList) ->
    create_condition_list4(String, Xs, ConditionList ++ "or " 
			   ++ String ++ io_lib:write_string(X)).

%% =====================================================================
%% @spec simple_member_b(FunName::string(), Arity::integer(), 
%%        List::[{string(), integer()}]) -> bool()
%%
%% @doc
%% Checks if {first param, second param} is a member of the list.
%% 
%% Parameter description:<pre>
%% <b>FunName</b> : function name.
%% <b>Arity</b> : function arity.
%% <b>List</b> : {function name, arity} list. </pre>
%% @end
%% origin: refac_ren_fun:get_datas/4
%% =====================================================================
simple_member_b(_FunName, _Arity, []) ->
    false;
simple_member_b(FunName, Arity, [{FunName, Arity} | _Xs]) ->
    true;
simple_member_b(FunName, Arity, [_X | Xs]) ->
    simple_member_b(FunName, Arity, Xs).

%% =====================================================================
%% @spec remove_binding_occurrence(MId::integer(), BId::integer(),
%%                 BodyId::integer()) -> ok
%%
%% @doc
%% Removes binding occurrence from clause body.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : Binding occurrence of a variable.
%% <b>BodyId</b> : Body of the variable. </pre>
%% @end
%% =====================================================================
remove_binding_occurrence(MId, BId, BodyId) ->
    MatchId = 
	get_match_expr_id_from_pattern_and_body_id(MId,BId,BodyId),
    ScopeId = get_scope_from_id(MId,MatchId),
    delete_match_expr(MId, MatchId),
    delete_element_from_pos(MId, MatchId),
    delete_element_from_node_type(MId, MatchId),
    delete_element_from_scope(MId, MatchId),
    Pid = spawn(fun () -> matcher([],0) end),
    pre_postorder(fun replicator/5, fun replicator/4, 
		  Pid, MId, BodyId, ScopeId, MatchId),
    delete_nodes:delete_node(MId, BId),
    stop(Pid).

%% =====================================================================
%% @spec replicate_subtree(MId::integer(), BodyId::integer(), 
%%        ScopeId::integer()) -> integer()
%%
%% @doc
%% Replicates a subtree with root BodyId, and scope ScopeId. 
%% The return value is the replicated subtrees root id.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyId</b> : Root of the subtree.
%% <b>ScopeId</b> : Scope of the subtree root. </pre>
%% @end
%% =====================================================================
replicate_subtree(MId, BodyId, ScopeId) ->
    Pid = spawn(fun () -> matcher([],0) end),
    NewId = postorder(fun replicator/4, Pid, 
		  MId, BodyId, ScopeId),
    stop(Pid), NewId.

%% =====================================================================
%% @spec replicate_subtree(MId::integer(), BodyId::integer(), 
%%        ScopeId::integer(), OldId::integer()) -> integer()
%%
%% @doc
%% Replicates a subtree with root BodyId, and scope ScopeId, 
%% to the root OldId.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyId</b> : Root of the subtree.
%% <b>ScopeId</b> : Scope of the subtree root.
%% <b>OldId</b> : Root of the replicated subtree. </pre>
%% @end
%% =====================================================================
replicate_subtree(MId, BodyId, ScopeId, OldId) ->
    Pid = spawn(fun () -> matcher([],0) end),
    OldId = pre_postorder(fun replicator/5, fun replicator/4, Pid, 
		  MId, BodyId, ScopeId, OldId),
    stop(Pid), OldId.

%% =====================================================================
%% @spec pre_postorder(Fpre::function(), F::function(), 
%%           Pid::pid(), MId::integer(), Tree::integer(), 
%%           ScopeId::integer(), OldId::integer()) -> integer()
%%
%% @doc
%% Traverses a subtree with root Tree, scope ScopeId, and uses
%% a seperate function(Fpre) for the root node.
%% 
%% Parameter description:<pre>
%% <b>Fpre</b> : Function used for the root node.
%% <b>F</b> : Function used for the other nodes.
%% <b>Pid</b> : The matcher factility's process id.
%% <b>MId</b> : Id of the module.
%% <b>Tree</b> : Root of the subtree.
%% <b>ScopeId</b> : Scope of the subtree root.
%% <b>OldId</b> : Root of the replicated subtree. </pre>
%% @end
%% @see matcher/2. Matcher facility
%% @end
%% =====================================================================
pre_postorder(Fpre, F, Pid, MId, Tree, ScopeId, OldId) ->
    Fpre(Pid, MId, ScopeId, OldId, 
	 case erl_syntax_db:subtrees(MId,Tree) of
	     [] -> Tree;
	     List -> [[postorder(F, Pid, MId, Subtree, ScopeId)
		       || Subtree <- Group]
		      || Group <- List],Tree
	 end).

%% =====================================================================
%% @spec postorder(F::function(), Pid::pid(), MId::integer(), 
%%        Tree::integer(), ScopeId::integer()) -> integer()
%%
%% @doc
%% Traverses a subtree with root Tree, scope ScopeId, 
%% and applies function F to every node.
%% 
%% Parameter description:<pre>
%% <b>F</b> : Function used for the other nodes.
%% <b>Pid</b> : The matcher factility's process id. 
%% <b>MId</b> : Id of the module.
%% <b>Tree</b> : Root of the subtree.
%% <b>ScopeId</b> : Scope of the subtree root.</pre>
%% @end
%% @see matcher/2. Matcher facility
%% @end
%% =====================================================================
postorder(F, Pid, MId, Tree, ScopeId) ->
    F(Pid, MId, ScopeId, case erl_syntax_db:subtrees(MId, Tree) of
			     [] -> Tree;
			     List -> [[postorder(F, Pid, MId, Subtree, 
						 ScopeId)
				       || Subtree <- Group]
				      || Group <- List], Tree
			      end).

%% =====================================================================
%% @spec replicator(Pid::pid(), MId::integer(), 
%%       ScopeId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Replicates the root node. It uses the existing subtree and 
%% the matcher facility to stick together the new tree. 
%% The result is the newly created node's id.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The matcher factility's process id. 
%% <b>MId</b> : Id of the module.
%% <b>Tree</b> : Root of the subtree.
%% <b>ScopeId</b> : Scope of the subtree root.
%% <b>Id</b> : Id of the replicated node.</pre>
%% @end
%% @see matcher/2. Matcher facility
%% @end
%% =====================================================================
replicator(Pid, MId, ScopeId, Id) ->
    case erl_syntax_db:type(MId, Id) of
	1 ->
	    OperId = erl_syntax_db:application_operator(MId, Id),
	    ArgumentsId = erl_syntax_db:application_arguments(MId, Id),
	    NewId = 
		create_nodes:create_application(
		  MId, get_match_of(Pid, OperId), 
		  get_matches_of(Pid, ArgumentsId));
	
	2 ->
	    BodyId = erl_syntax_db:arity_qualifier_body(MId, Id),
	    ArgumentId = 
		erl_syntax_db:arity_qualifier_argument(MId, Id),
	    NewId = 
		create_nodes:create_arity_qualifier(
		  MId, get_match_of(Pid, BodyId), 
		  get_match_of(Pid, ArgumentId));
	
	3 -> Name = erl_syntax_db:atom_name(MId, Id),
	     NewId = create_nodes:create_atom(MId, Name);
	
	4 ->
	    case erl_syntax_db:attribute_arguments(MId, Id) of
		none ->
		    NameId = erl_syntax_db:attribute_name(MId, Id),
		    NewId = 
			create_nodes:create_attribute(
			  MId, get_match_of(Pid, NameId), none);
		ArgumentsId ->
		    NameId = erl_syntax_db:attribute_name(MId,Id),
		    NewId = 
			erl_syntax_db:create_attribute(
			  MId, get_match_of(Pid, NameId), 
			  get_matches_of(Pid, ArgumentsId))
	    end;
	
	5 ->
	    FieldsId = erl_syntax_db:binary_fields(MId, Id),
	    NewId = create_nodes:create_binary(
		      MId, get_matches_of(Pid,FieldsId));
	
	6 ->
	    TypesId = erl_syntax_db:binary_field_types(MId, Id),
	    BodyId = erl_syntax_db:binary_field_body(MId, Id),
	    NewId = create_nodes:create_binary_field(
		      MId, get_match_of(Pid, BodyId), 
		      get_matches_of(Pid, TypesId));

	7 ->
	    BodyIds = erl_syntax_db:block_expr_body(MId, Id),
	    NewId = create_nodes:create_block_expr(
		      MId, get_matches_of(Pid, BodyIds));
	    
	8 ->
	    ArgumentId = erl_syntax_db:case_expr_argument(MId, Id),
	    ClausesId = erl_syntax_db:case_expr_clauses(MId, Id),
	    NewId = create_nodes:create_case_expr(
		      MId, get_match_of(Pid, ArgumentId), 
		      get_matches_of(Pid, ClausesId));

	9 ->
	    BodyId = erl_syntax_db:catch_expr_body(MId, Id),
	    NewId = create_nodes:create_catch_expr(
		      MId, get_match_of(Pid, BodyId));
	
	10 ->
	    Value = erl_syntax_db:char_value(MId, Id),
	    NewId = create_nodes:create_char(MId, Value);

	11 ->
	    ArgumentId = erl_syntax_db:class_qualifier_argument(MId, Id),
	    BodyId = erl_syntax_db:class_qualifier_body(MId, Id),
	    NewId = create_nodes:create_class_qualifier(
		      MId, get_match_of(Pid, ArgumentId), 
		      get_match_of(Pid, BodyId));

	12 ->
	    case erl_syntax_db:clause_guard(MId, Id) of
		none ->
		    PatternsId = erl_syntax_db:clause_patterns(MId, Id), 
		    BodyIds = erl_syntax_db:clause_body(MId, Id),
		    NewId = create_nodes:create_clause(
			      MId, get_matches_of(Pid, PatternsId), 
			      none, get_matches_of(Pid, BodyIds));

		GuardId ->
		    PatternsId = erl_syntax_db:clause_patterns(MId, Id), 
		    BodyIds = erl_syntax_db:clause_body(MId, Id),
		    NewId = create_nodes:create_clause(
			      MId, get_matches_of(Pid, PatternsId), 
			      get_match_of(Pid, GuardId), 
			      get_matches_of(Pid, BodyIds))
	    end;
	
	13 ->
	    Padding = erl_syntax_db:comment_padding(MId, Id),
	    Strings = erl_syntax_db:comment_text(MId, Id),
	    NewId = create_nodes:create_comment(
		      MId, Padding, Strings);

	14 ->
	    ClausesId = erl_syntax_db:cond_expr_clauses(MId, Id),
	    NewId = create_nodes:create_cond_expr(
		      MId, get_matches_of(Pid, ClausesId));

	15 ->
	    BodyIds = erl_syntax_db:conjunction_body(MId, Id),
	    NewId = create_nodes:create_conjunction(
		      MId, get_matches_of(Pid, BodyIds));

	16 ->
	    BodyIds = erl_syntax_db:disjunction_body(MId, Id),
	    NewId = create_nodes:create_disjunction(
		      MId, get_matches_of(Pid, BodyIds));
	
	17 ->
	    NewId = create_nodes:create_eof_marker(MId);
	
	18 ->
	    Value = erl_syntax_db:float_value(MId, Id),
	    NewId = create_nodes:create_float(MId, Value);
	    
	19 ->
	    ElementsId = erl_syntax_db:form_list_elements(MId, Id),
	    NewId = create_nodes:create_form_list(
		      MId, get_matches_of(Pid, ElementsId));
	
	20 ->
	    ClausesId = erl_syntax_db:fun_expr_clauses(MId, Id),
	    NewId = create_nodes:create_fun_expr(
		      MId, get_matches_of(Pid, ClausesId));
	
	21 ->
	    NameId = erl_syntax_db:function_name(MId, Id), 
	    ClausesId = erl_syntax_db:function_clauses(MId, Id),
	    NewId = create_nodes:create_function(
		      MId, get_match_of(Pid, NameId), 
		      get_matches_of(Pid, ClausesId));

	22 ->
	    PatternId = erl_syntax_db:generator_pattern(MId, Id), 
	    BodyId = erl_syntax_db:generator_body(MId, Id),
	    NewId = create_nodes:create_generator(
		      MId, get_match_of(Pid, PatternId), 
		      get_match_of(Pid, BodyId));
	
	23 ->
	    ClausesId = erl_syntax_db:if_expr_clauses(MId, Id),
	    NewId = create_nodes:create_if_expr(
		      MId, get_matches_of(Pid, ClausesId));

	24 ->
	    NameId = erl_syntax_db:implicit_fun_name(MId, Id),
	    NewId = create_nodes:create_implicit_fun(
		      MId, get_match_of(Pid, NameId));

	25 ->
	    LeftId = erl_syntax_db:infix_expr_left(MId, Id),
	    OperId = erl_syntax_db:infix_expr_operator(MId, Id),
	    RightId = erl_syntax_db:infix_expr_right(MId, Id),
	    NewId = create_nodes:create_infix_expr(
		      MId, get_match_of(Pid, LeftId), 
		      get_match_of(Pid, OperId), 
		      get_match_of(Pid, RightId));
	26 ->
	    Value = erl_syntax_db:integer_value(MId, Id),
	    NewId = create_nodes:create_integer(MId, Value);

	27 ->
	    case erl_syntax_db:list_suffix(MId, Id) of
		none ->
		    PrefixIds = erl_syntax_db:list_prefix(MId, Id),
		    NewId = create_nodes:create_list(
			      MId, get_matches_of(Pid, PrefixIds), none);
		SuffixId ->
		    PrefixIds = erl_syntax_db:list_prefix(MId, Id),
		    NewId = create_nodes:create_list(
			      MId, get_matches_of(Pid, PrefixIds), 
			      get_match_of(Pid, SuffixId))
	    end;
	
	28 ->
	    TemplateId = erl_syntax_db:list_comp_template(MId, Id),
	    BodyIds = erl_syntax_db:list_comp_body(MId, Id),
	    NewId = create_nodes:create_list_comp(
		      MId, get_match_of(Pid, TemplateId), 
		      get_matches_of(Pid, BodyIds));

	29 ->
	    case erl_syntax_db:macro_arguments(MId, Id) of
		none ->
		    NameId = erl_syntax_db:macro_name(MId, Id),
		    NewId = create_nodes:create_macro(
			      MId, get_match_of(Pid, NameId), none);
		ArgumentsId ->
		    NameId = erl_syntax_db:macro_name(MId, Id),
		    NewId = create_nodes:create_macro(
			      MId, get_match_of(Pid, NameId), 
			      get_matches_of(Pid, ArgumentsId))
	    end;

	30 ->
	    PatternId = erl_syntax_db:match_expr_pattern(MId, Id),
	    BodyId = erl_syntax_db:match_expr_body(MId, Id),
	    NewId = create_nodes:create_match_expr(
		      MId, get_match_of(Pid, PatternId), 
		      get_match_of(Pid, BodyId));

	31 ->
	    ArgumentId = erl_syntax_db:module_qualifier_argument(MId, Id),
	    BodyId = erl_syntax_db:module_qualifier_body(MId, Id),
	    NewId = create_nodes:create_module_qualifier(
		      MId, get_match_of(Pid, ArgumentId), 
		      get_match_of(Pid, BodyId));
	32 ->
	    NewId = create_nodes:create_nil(MId);
	
	33 ->
	    Name = erl_syntax_db:operator_literal(MId, Id),
	    NewId = create_nodes:create_operator(MId, Name);
	    
	34 ->
	    BodyId = erl_syntax_db:parentheses_body(MId, Id),
	    NewId = create_nodes:create_parentheses(
		      MId, get_match_of(Pid, BodyId));

	35 ->
	    OperatorId = erl_syntax_db:prefix_expr_operator(MId, Id),
	    ArgumentId = erl_syntax_db:prefix_expr_argument(MId, Id),
	    NewId = create_nodes:create_prefix_expr(
		      MId, get_match_of(Pid, OperatorId), 
		      get_match_of(Pid, ArgumentId));

	36 ->
	    SegmentsId = erl_syntax_db:qualified_name_segments(MId, Id),
	    NewId = create_nodes:create_qualified_name(
		      MId, get_matches_of(Pid, SegmentsId));
	
	37 ->
	    BodyId = erl_syntax_db:query_expr_body(MId, Id),
	    NewId = create_nodes:create_query_expr(
		      MId, get_match_of(Pid, BodyId));
	
	38 ->
	    case erl_syntax_db:receive_expr_timeout(MId, Id) of
		none ->
		    ClausesId = 
			erl_syntax_db:receive_expr_clauses(MId, Id),
		    NewId = 
			create_nodes:create_receive_expr(
			  MId, get_matches_of(Pid, ClausesId), none, []);
		TimeoutId ->
		    ClausesId = 
			erl_syntax_db:receive_expr_clauses(MId, Id),
		    ActionIds = 
			erl_syntax_db:receive_expr_action(MId, Id),
		    NewId = 
			create_nodes:create_receive_expr(
			  MId, get_matches_of(Pid, ClausesId), 
			  get_match_of(Pid, TimeoutId), 
			  get_matches_of(Pid, ActionIds))
	    end;
	
	39 ->
	    case erl_syntax_db:record_access_type(MId, Id) of
		none ->
		    ArgumentId = 
			erl_syntax_db:record_access_argument(MId, Id),
		    FieldId = 
			erl_syntax_db:record_access_field(MId, Id),
		    NewId = 
			create_nodes:create_record_access(
			  MId, get_match_of(Pid, ArgumentId), 
			  none, get_match_of(Pid, FieldId));
		TypeId ->
		    ArgumentId = 
			erl_syntax_db:record_access_argument(MId, Id),
		    FieldId = 
			erl_syntax_db:record_access_field(MId, Id),
		    NewId = 
			create_nodes:create_record_access(
			  MId, get_match_of(Pid, ArgumentId), 
			  get_match_of(Pid, TypeId), 
			  get_match_of(Pid, FieldId))
	    end;

	40 ->
	    case erl_syntax_db:record_expr_argument(MId, Id) of
		none ->
		    TypeId = erl_syntax_db:record_expr_type(MId, Id),
		    FieldsId = 
			erl_syntax_db:record_expr_fields(MId, Id),
		    NewId = 
			create_nodes:create_record_expr(
			  MId, none, get_match_of(Pid, TypeId), 
			  get_matches_of(Pid, FieldsId));
		ArgumentId ->
		    TypeId = erl_syntax_db:record_expr_type(MId, Id),
		    FieldsId = 
			erl_syntax_db:record_expr_fields(MId, Id),
		    NewId = 
			create_nodes:create_record_expr(
			  MId, get_match_of(Pid, ArgumentId), 
			  get_match_of(Pid, TypeId), 
			  get_matches_of(Pid, FieldsId))
	    end;
	
	41 ->
	    case erl_syntax_db:record_field_value(MId, Id) of
		none ->
		    NameId = erl_syntax_db:record_field_name(MId, Id),
		    NewId = 
			create_nodes:create_record_field(
			  MId, get_match_of(Pid, NameId), none);
		ValueId ->
		    NameId = erl_syntax_db:record_field_name(MId, Id),
		    NewId = 
			create_nodes:create_record_field(
			  MId, get_match_of(Pid, NameId), 
			  get_match_of(Pid, ValueId))
	    end;
	
	42 ->
	    TypeId = erl_syntax_db:record_index_expr_type(MId, Id),
	    FieldId = erl_syntax_db:record_index_expr_field(MId, Id),
	    NewId = 
		create_nodes:create_record_index_expr(
		  MId, get_match_of(Pid, TypeId), 
		  get_match_of(Pid, FieldId));

	43 ->
	    NameId =  erl_syntax_db:rule_name(MId, Id),
	    ClausesId = erl_syntax_db:rule_clauses(MId, Id),
	    NewId = 
		create_nodes:create_rule(
		  MId, get_match_of(Pid, NameId), 
		  get_matches_of(Pid, ClausesId));

	44 ->
	    BodyId = erl_syntax_db:size_qualifier_body(MId, Id),
	    ArgumentId = erl_syntax_db:size_qualifier_argument(MId, Id),
	    NewId = 
		create_nodes:create_size_qualifier(
		  MId, get_match_of(Pid, BodyId), 
		  get_match_of(Pid, ArgumentId));

	45 ->
	    String = erl_syntax_db:string_value(MId, Id),
	    NewId = create_nodes:create_string(MId, io_lib:write_string(String));

	46 ->
	    String = erl_syntax_db:text_string(MId, Id),
	    NewId = create_nodes:create_text(MId, io_lib:write_string(String));

	47 ->
	    BodyIds = erl_syntax_db:try_expr_body(MId, Id),
	    ClausesId = erl_syntax_db:try_expr_clauses(MId, Id),
	    HandlersId = erl_syntax_db:try_expr_handlers(MId, Id),
	    AfterIds = erl_syntax_db:try_expr_after(MId, Id),
	    NewId = 
		create_nodes:create_try_expr(
		  MId, get_matches_of(Pid, BodyIds), 
		  get_matches_of(Pid, ClausesId), 
		  get_matches_of(Pid, HandlersId), 
		  get_matches_of(Pid, AfterIds));

	48 ->
	    ElementsId = erl_syntax_db:tuple_elements(MId, Id),
	    NewId = create_nodes:create_tuple(
                      MId, get_matches_of(Pid, ElementsId));

	49 ->
	    NewId = create_nodes:create_underscore(MId);
	
        50 ->
	    Name = erl_syntax_db:variable_literal(MId, Id),
	    NewId = create_nodes:create_variable(MId, Name),
	    connect_variable_to_first_occurrence(MId, Id, NewId)	
    end,
    create_nodes:init_scope(MId, ScopeId, NewId),
    add_match(Pid, Id, NewId),
    NewId.

%% =====================================================================
%% @spec replicator(Pid::pid(), MId::integer(), 
%%       ScopeId::integer(), OldId::integer(), 
%%       Id::integer()) -> integer()
%%
%% @doc
%% Replicates the root node. It uses the existing subtree and 
%% the matcher facility to stick together the new tree. 
%% The result is the newly created node's id, 
%% which is the last parameter of the function.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The matcher factility's process id. 
%% <b>MId</b> : Id of the module.
%% <b>ScopeId</b> : Scope of the subtree root.
%% <b>OldId</b> : The new node's Id.
%% <b>Id</b> : Id of the replicated node.</pre>
%% @end
%% @see matcher/2. Matcher facility
%% @end
%% =====================================================================
replicator(Pid, MId, ScopeId, OldId, Id) ->
    case erl_syntax_db:type(MId, Id) of
	1 ->
	    OperId = erl_syntax_db:application_operator(MId, Id),
	    ArgumentsId = erl_syntax_db:application_arguments(MId, Id),
	    create_nodes:create_application(
	      MId, get_match_of(Pid, OperId), 
	      get_matches_of(Pid, ArgumentsId), OldId);
	
	2 ->
	    BodyId = erl_syntax_db:arity_qualifier_body(MId, Id),
	    ArgumentId = erl_syntax_db:arity_qualifier_argument(MId, Id),
	    create_nodes:create_arity_qualifier(
	      MId, get_match_of(Pid, BodyId), 
	      get_match_of(Pid, ArgumentId), OldId);
	
	3 -> Name = erl_syntax_db:atom_name(MId, Id),
	     create_nodes:create_atom(MId, Name, OldId);
	
	4 ->
	    case erl_syntax_db:attribute_arguments(MId, Id) of
		none ->
		    NameId = erl_syntax_db:attribute_name(MId, Id),
		    create_nodes:create_attribute(
		      MId, get_match_of(Pid, NameId), none, OldId);
		ArgumentsId ->
		    NameId = erl_syntax_db:attribute_name(MId, Id),
		    erl_syntax_db:create_attribute(
		      MId, get_match_of(Pid, NameId), 
		      get_matches_of(Pid, ArgumentsId), OldId)
	    end;
	
	5 ->
	    FieldsId = erl_syntax_db:binary_fields(MId, Id),
	    create_nodes:create_binary(
	      MId, get_matches_of(Pid,FieldsId), OldId);
	
	6 ->
	    TypesId = erl_syntax_db:binary_field_types(MId, Id),
	    BodyId = erl_syntax_db:binary_field_body(MId, Id),
	    create_nodes:create_binary_field(
	      MId, get_match_of(Pid, BodyId), 
	      get_matches_of(Pid, TypesId), OldId);

	7 ->
	    BodyIds = erl_syntax_db:block_expr_body(MId, Id),
	    create_nodes:create_block_expr(
	      MId, get_matches_of(Pid, BodyIds), OldId);
	    
	8 ->
	    ArgumentId = erl_syntax_db:case_expr_argument(MId, Id),
	    ClausesId = erl_syntax_db:case_expr_clauses(MId, Id),
	    create_nodes:create_case_expr(
	      MId, get_match_of(Pid, ArgumentId), 
	      get_matches_of(Pid, ClausesId), OldId);

	9 ->
	    BodyId = erl_syntax_db:catch_expr_body(MId, Id),
	    create_nodes:create_catch_expr(
	      MId, get_match_of(Pid, BodyId), OldId);
	
	10 ->
	    Value = erl_syntax_db:char_value(MId, Id),
	    create_nodes:create_char(MId, Value, OldId);

	11 ->
	    ArgumentId = erl_syntax_db:class_qualifier_argument(MId, Id),
	    BodyId = erl_syntax_db:class_qualifier_body(MId, Id),
	    create_nodes:class_qualifier(
	      MId, get_match_of(Pid, ArgumentId), 
	      get_match_of(Pid, BodyId), OldId);

	12 ->
	    case erl_syntax_db:clause_guard(MId, Id) of
		none ->
		    PatternsId = erl_syntax_db:clause_patterns(MId, Id), 
		    BodyIds = erl_syntax_db:clause_body(MId, Id),
		    create_nodes:create_clause(
		      MId, get_matches_of(Pid, PatternsId), 
		      none, get_matches_of(Pid, BodyIds), OldId);

		GuardId ->
		    PatternsId = erl_syntax_db:clause_patterns(MId, Id), 
		    BodyIds = erl_syntax_db:clause_body(MId, Id),
		    create_nodes:create_clause(
		      MId, get_matches_of(Pid, PatternsId), 
		      get_match_of(Pid, GuardId), 
		      get_matches_of(Pid, BodyIds), OldId)
	    end;
	
	13 ->
	    Padding = erl_syntax_db:comment_padding(MId, Id),
	    Strings = erl_syntax_db:comment_text(MId, Id),
	    create_nodes:create_comment(
	      MId, Padding, Strings, OldId);

	14 ->
	    ClausesId = erl_syntax_db:cond_expr_clauses(MId, Id),
	    create_nodes:create_cond_expr(
	      MId, get_matches_of(Pid, ClausesId), OldId);

	15 ->
	    BodyIds = erl_syntax_db:conjunction_body(MId, Id),
	    create_nodes:create_conjunction(
	      MId, get_matches_of(Pid, BodyIds), OldId);

	16 ->
	    BodyIds = erl_syntax_db:disjunction_body(MId, Id),
	    create_nodes:create_disjunction(
	      MId, get_matches_of(Pid, BodyIds), OldId);
	
	17 ->
	    create_nodes:create_eof_marker(MId, OldId);
	
	18 ->
	    Value = erl_syntax_db:float_value(MId, Id),
	    create_nodes:create_float(MId, Value, OldId);
	    
	19 ->
	    ElementsId = erl_syntax_db:form_list_elements(MId, Id),
	    create_nodes:create_form_list(
	      MId, get_matches_of(Pid, ElementsId), OldId);
	
	20 ->
	    ClausesId = erl_syntax_db:fun_expr_clauses(MId, Id),
	    create_nodes:create_fun_expr(
	      MId, get_matches_of(Pid, ClausesId), OldId);
	
	21 ->
	    NameId = erl_syntax_db:function_name(MId, Id), 
	    ClausesId = erl_syntax_db:function_clauses(MId, Id),
	    create_nodes:create_function(
	      MId, get_match_of(Pid, NameId), 
	      get_matches_of(Pid, ClausesId), OldId);

	22 ->
	    PatternId = erl_syntax_db:generator_pattern(MId, Id), 
	    BodyId = erl_syntax_db:generator_body(MId, Id),
	    create_nodes:create_generator(
	      MId, get_match_of(Pid, PatternId), 
	      get_match_of(Pid, BodyId), OldId);
	
	23 ->
	    ClausesId = erl_syntax_db:if_expr_clauses(MId, Id),
	    create_nodes:create_if_expr(
	      MId, get_matches_of(Pid, ClausesId), OldId);

	24 ->
	    NameId = erl_syntax_db:implicit_fun_name(MId, Id),
	    create_nodes:create_implicit_fun(
	      MId, get_match_of(Pid, NameId), OldId);

	25 ->
	    LeftId = erl_syntax_db:infix_expr_left(MId, Id),
	    OperId = erl_syntax_db:infix_expr_operator(MId, Id),
	    RightId = erl_syntax_db:infix_expr_right(MId, Id),
	    create_nodes:create_infix_expr(
	      MId, get_match_of(Pid, LeftId), 
	      get_match_of(Pid, OperId), 
	      get_match_of(Pid, RightId), OldId);

	26 ->
	    Value = erl_syntax_db:integer_value(MId, Id),
	    create_nodes:create_integer(MId, Value, OldId);

	27 ->
	    case erl_syntax_db:list_suffix(MId, Id) of
		none ->
		    PrefixIds = erl_syntax_db:list_prefix(MId, Id),
		    create_nodes:create_list(
		      MId, get_matches_of(Pid, PrefixIds), none, OldId);
		SuffixId ->
		    PrefixIds = erl_syntax_db:list_prefix(MId, Id),
		    create_nodes:create_list(
		      MId, get_matches_of(Pid, PrefixIds), 
		      get_match_of(Pid, SuffixId), OldId)
	    end;
	
	28 ->
	    TemplateId = erl_syntax_db:list_comp_template(MId, Id),
	    BodyIds = erl_syntax_db:list_comp_body(MId, Id),
	    create_nodes:create_list_comp(
	      MId, get_match_of(Pid, TemplateId), 
	      get_matches_of(Pid, BodyIds), OldId);

	29 ->
	    case erl_syntax_db:macro_arguments(MId, Id) of
		none ->
		    NameId = erl_syntax_db:macro_name(MId, Id),
		    create_nodes:create_macro(
		      MId, get_match_of(Pid, NameId), none, OldId);
		ArgumentsId ->
		    NameId = erl_syntax_db:macro_name(MId, Id),
		    create_nodes:create_macro(
		      MId, get_match_of(Pid, NameId), 
		      get_matches_of(Pid, ArgumentsId), OldId)
	    end;

	30 ->
	    PatternId = erl_syntax_db:match_expr_pattern(MId, Id),
	    BodyId = erl_syntax_db:match_expr_body(MId, Id),
	    create_nodes:create_match_expr(
	      MId, get_match_of(Pid, PatternId), 
	      get_match_of(Pid, BodyId), OldId);

	31 ->
	    ArgumentId = erl_syntax_db:module_qualifier_argument(MId, Id),
	    BodyId = erl_syntax_db:module_qualifier_body(MId, Id),
	    create_nodes:create_module_qualifier(
	      MId, get_match_of(Pid, ArgumentId), 
	      get_match_of(Pid, BodyId), OldId);

	32 ->
	    create_nodes:create_nil(MId, OldId);
	
	33 ->
	    Name = erl_syntax_db:operator_literal(MId, Id),
	    create_nodes:create_operator(MId, Name, OldId);
	    
	34 ->
	    BodyId = erl_syntax_db:parentheses_body(MId, Id),
	    create_nodes:create_parentheses(
	      MId, get_match_of(Pid, BodyId), OldId);

	35 ->
	    OperatorId = erl_syntax_db:prefix_expr_operator(MId, Id),
	    ArgumentId = erl_syntax_db:prefix_expr_argument(MId, Id),
	    create_nodes:create_prefix_expr(
	      MId, get_match_of(Pid, OperatorId), 
	      get_match_of(Pid, ArgumentId), OldId);

	36 ->
	    SegmentsId = erl_syntax_db:qualified_name_segments(MId, Id),
	    create_nodes:create_qualified_name(
	      MId, get_matches_of(Pid, SegmentsId), OldId);
	
	37 ->
	    BodyId = erl_syntax_db:query_expr_body(MId, Id),
	    create_nodes:create_query_expr(
	      MId, get_match_of(Pid, BodyId), OldId);
	
	38 ->
	    case erl_syntax_db:receive_expr_timeout(MId, Id) of
		none ->
		    ClausesId = 
			erl_syntax_db:receive_expr_clauses(MId, Id),
		    create_nodes:create_receive_expr(
		      MId, get_matches_of(Pid, ClausesId), 
		      none, [], OldId);
		TimeoutId ->
		    ClausesId = 
			erl_syntax_db:receive_expr_clauses(MId, Id),
		    ActionIds = 
			erl_syntax_db:receive_expr_action(MId, Id),
		    create_nodes:create_receive_expr(
		      MId, get_matches_of(Pid, ClausesId), 
		      get_match_of(Pid, TimeoutId), 
		      get_matches_of(Pid, ActionIds), OldId)
	    end;
	
	39 ->
	    case erl_syntax_db:record_access_type(MId, Id) of
		none ->
		    ArgumentId = 
			erl_syntax_db:record_access_argument(MId, Id),
		    FieldId = 
			erl_syntax_db:record_access_field(MId, Id),
		    create_nodes:create_record_access(
		      MId, get_match_of(Pid, ArgumentId), 
		      none, get_match_of(Pid, FieldId), OldId);
		TypeId ->
		    ArgumentId = 
			erl_syntax_db:record_access_argument(MId, Id),
		    FieldId = 
			erl_syntax_db:record_access_field(MId, Id),
		    create_nodes:create_record_access(
		      MId, get_match_of(Pid, ArgumentId), 
		      get_match_of(Pid, TypeId), 
		      get_match_of(Pid, FieldId), OldId)
	    end;

	40 ->
	    case erl_syntax_db:record_expr_argument(MId, Id) of
		none ->
		    TypeId = erl_syntax_db:record_expr_type(MId, Id),
		    FieldsId = 
			erl_syntax_db:record_expr_fields(MId, Id),
		    create_nodes:create_record_expr(
		      MId, none, get_match_of(Pid, TypeId), 
		      get_matches_of(Pid, FieldsId), OldId);
		ArgumentId ->
		    TypeId = erl_syntax_db:record_expr_type(MId, Id),
		    FieldsId = 
			erl_syntax_db:record_expr_fields(MId, Id),
		    create_nodes:create_record_expr(
		      MId, get_match_of(Pid, ArgumentId), 
		      get_match_of(Pid, TypeId), 
		      get_matches_of(Pid, FieldsId), OldId)
	    end;
	
	41 ->
	    case erl_syntax_db:record_field_value(MId, Id) of
		none ->
		    NameId = erl_syntax_db:record_field_name(MId, Id),
		    create_nodes:create_record_field(
		      MId, get_match_of(Pid, NameId), none, OldId);
		ValueId ->
		    NameId = erl_syntax_db:record_field_name(MId, Id),
		    create_nodes:create_record_field(
		      MId, get_match_of(Pid, NameId), 
		      get_match_of(Pid, ValueId), OldId)
	    end;
	
	42 ->
	    TypeId = erl_syntax_db:record_index_expr_type(MId, Id),
	    FieldId = erl_syntax_db:record_index_expr_field(MId, Id),
	    create_nodes:create_record_index_expr(
	      MId, get_match_of(Pid, TypeId), 
	      get_match_of(Pid, FieldId), OldId);

	43 ->
	    NameId =  erl_syntax_db:rule_name(MId, Id),
	    ClausesId = erl_syntax_db:rule_clauses(MId, Id),
	    create_nodes:create_rule(
	      MId, get_match_of(Pid, NameId), 
	      get_matches_of(Pid, ClausesId), OldId);

	44 ->
	    BodyId = erl_syntax_db:size_qualifier_body(MId, Id),
	    ArgumentId = erl_syntax_db:size_qualifier_argument(MId, Id),
	    create_nodes:create_size_qualifier(
	      MId, get_match_of(Pid, BodyId), 
	      get_match_of(Pid, ArgumentId), OldId);

	45 ->
	    String = erl_syntax_db:string_value(MId, Id),
	    create_nodes:create_string(MId, io_lib:write_string(String), OldId);

	46 ->
	    String = erl_syntax_db:text_string(MId, Id),
	    create_nodes:create_text(MId, io_lib:write_string(String), OldId);

	47 ->
	    BodyIds = erl_syntax_db:try_expr_body(MId, Id),
	    ClausesId = erl_syntax_db:try_expr_clauses(MId, Id),
	    HandlersId = erl_syntax_db:try_expr_handlers(MId, Id),
	    AfterIds = erl_syntax_db:try_expr_after(MId, Id),
	    create_nodes:create_try_expr(
	      MId, get_matches_of(Pid, BodyIds), 
	      get_matches_of(Pid, ClausesId), 
	      get_matches_of(Pid, HandlersId), 
	      get_matches_of(Pid, AfterIds), OldId);

	48 ->
	    ElementsId = erl_syntax_db:tuple_elements(MId, Id),
	    create_nodes:create_tuple(
              MId, get_matches_of(Pid, ElementsId), OldId);

	49 ->
	    create_nodes:create_underscore(MId, OldId);
	
        50 ->
	    Name = erl_syntax_db:variable_literal(MId, Id),
	    create_nodes:create_variable(MId, Name, OldId),
	    connect_variable_to_first_occurrence(MId, Id, OldId)	
    end,
    create_nodes:init_scope(MId, ScopeId, OldId),
    add_match(Pid, Id, OldId),
    OldId.

%% =====================================================================
%% @spec connect_variable_to_first_occurrence(MId::integer(), 
%         Id::integer(), NewId::integer()) -> integer()
%%
%% @doc
%% Connects the NewId variable node to the Id's first occurrence.
%% They become the same variables different occurrence.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : A variable's Id.
%% <b>NewId</b> : The same variable's newly created occurrence.</pre>
%% @end
%% =====================================================================
connect_variable_to_first_occurrence(MId, Id, NewId) ->
    [{FirstOccurrenceId}] = 
	refactor_db:select(
	  "select target from var_visib where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " ;"),
    create_nodes:connect_variables(MId, [FirstOccurrenceId], [NewId]).

%%% Matching facility BEGIN
%% =====================================================================
%% @spec matcher(List::[{integer(), integer()}], Last::integer()) -> 
%%                    none()
%%
%% @doc
%% Associates two value together. It is used in the replicator to keep 
%% track of the newly created nodes' ids. It is designed to work as a 
%% separate process.
%% 
%% Parameter description:<pre>
%% <b>List</b> : The list of the associated values.
%% <b>Last</b> : The last given pairs second element.</pre>
%% @end
%% =====================================================================
matcher(List,Last) ->
    receive
	{stop, Pid} ->
	    Pid ! {ok, self()};
	{last, Pid} ->
	    Pid ! {value, Last, self()},
	    matcher(List,Last);
	{match, Id, Pid} ->
	    case lists:keysearch(Id,1,List) of
		{value, {Id, MatchId}} ->
		    Pid ! {value, MatchId, self()};
		_ ->
		    Pid ! {error, no_such_id, self()}
	    end,
	    matcher(List,Last);
	{add_match, Pair, Pid} ->
	    Pid ! {got_value , self()},
	    matcher([Pair]++List, element(2,Pair));
	_ ->
	    matcher(List, Last)
    end.
	    
%% =====================================================================
%% @spec stop(Pid::pid()) -> ok
%%
%% @doc
%% Stops the matcher/2 function. This is on of the utility functions 
%% to hide the message sending, receiveing to the process.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : Process id of a matcher/2 function.</pre>
%% @end
%% =====================================================================
stop(Pid) ->
    Pid ! {stop, self()},
    receive
	{ok , Pid} ->
	    ok;
	_  ->
	    ok
    end.

%% =====================================================================
%% @spec get_last_match(Pid::pid()) -> integer()
%%
%% @doc
%% Gets the last sent pairs second parameter. 
%% This is on of the utility functions 
%% to hide the message sending, receiveing to the process.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : Process id of a matcher/2 function.</pre>
%% @end
%% =====================================================================
get_last_match(Pid) ->
    Pid ! {last, self()},
    receive
	{value, Id , Pid} ->
	    Id;
	_ ->
	    ok 
    end.

%% =====================================================================
%% @spec get_match_of(Pid::pid(), Id::integer()) -> integer()
%%
%% @doc
%% Gets the match of the second parameter. 
%% This is on of the utility functions 
%% to hide the message sending, receiveing to the process.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : Process id of a matcher/2 function.
%% <b>Id</b> : Id of a node.</pre>
%% @end
%% =====================================================================
get_match_of(Pid, Id) ->
    Pid ! {match, Id, self()},
    receive
	{value, NewId, Pid} ->
	    NewId;
	_  ->
	    ok 
    end.

%% =====================================================================
%% @spec get_matches_of(Pid::pid(), Ids::[integer()]) -> [integer()]
%%
%% @doc
%% Gets the match of the second parameter.
%% It works like the get_match_of/2 function, just it works on lists. 
%% This is on of the utility functions 
%% to hide the message sending, receiveing to the process.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : Process id of a matcher/2 function.
%% <b>Id</b> : List of node ids.</pre>
%% @end
%% =====================================================================
get_matches_of(_Pid, []) ->
    [];
get_matches_of(Pid, [Id | Xs]) ->
    [get_match_of(Pid, Id) | get_matches_of(Pid,Xs)].

%% =====================================================================
%% @spec add_match(Pid::pid(), Id::integer(), MatchId::integer()) 
%%                    -> ok
%%
%% @doc
%% Creates a match from the second and third parameter.
%% The second parameters match will be the third parameter. 
%% This is on of the utility functions 
%% to hide the message sending, receiveing to the process.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : Process id of a matcher/2 function.
%% <b>Id</b> :  Id of a node.
%% <b>MatchId</b> :  Id of a node.</pre>
%% @end
%% =====================================================================
add_match(Pid, Id, MatchId) ->
    Pid ! {add_match, {Id, MatchId}, self()},
    receive
	{got_value, Pid} ->
	    ok;
	_ ->
	    ok 
    end.	
%%% Matching facility END

%% =====================================================================
%% @spec is_compile_export_all(MId::integer()) -> bool()
%%
%% @doc
%% Checks if the module contain a <code>-compile(export_all).</code>
%% attribute.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.</pre>
%% @end
%% origin: refac_ren_fun:get_datas/4
%% =====================================================================
is_compile_export_all(MId) ->
    Exists = 
	refactor_db:select(
	  "select a.id from attribute_ as a, name as n"
	  " where a.mid=n.mid and n.mid=" ++ integer_to_list(MId) 
	  ++ " and a.argument=n.id and a.pos=1 and n.name=\"export_all\""
	  " and a.id in (select a.id from attribute_ as a,"
	  " name as n where a.mid=n.mid and n.mid=" 
	  ++ integer_to_list(MId) ++ " and a.argument=n.id and a.pos=0 and"
	  " n.name=\"compile\");"),
    Exists /= [].

%% =====================================================================
%% @spec not_end_of_block_expr(MId::integer(), 
%%       VarId::integer()) -> bool()
%%
%% @doc
%% Returns true or false if the variable was at the end of a 
%% block expression.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
not_end_of_block_expr(MId, VarId) ->    
    Exists = 
	refactor_db:select(
	  "select id,pos from block_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and body=" 
	  ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}]=Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from block_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " ;"),
            MaxPos /= Pos
    end.

%% =====================================================================
%% @spec not_end_of_clause_body(MId::integer(), 
%%       VarId::integer()) -> bool()
%%
%% @doc
%% Returns true or false if the variable was at the end of a 
%% clause body.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
not_end_of_clause_body(MId, VarId) ->    
    Exists = 
	refactor_db:select(
	  "select id,pos from clause where mid=" 
	  ++ integer_to_list(MId) ++ " and qualifier=2"
	  " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}]=Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from clause where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=2 ;"),
            MaxPos /= Pos
    end.

%% =====================================================================
%% @spec not_end_of_receive_expr_action(MId::integer(), 
%%       VarId::integer()) -> bool()
%%
%% @doc
%% Returns true or false if the variable was at the end of a receive
%% expression action.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
not_end_of_receive_expr_action(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from receive_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=2"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}]=Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from receive_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=2 ;"),
            MaxPos/=Pos
    end.

%% =====================================================================
%% @spec not_end_of_try_expr_body(MId::integer(), 
%%       VarId::integer()) -> bool()
%%
%% @doc
%% Returns true or false if the variable was at the end of a 
%% try expression body.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
not_end_of_try_expr_body(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from try_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=0"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}]=Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from try_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=0 ;"),
            MaxPos /= Pos
    end.

%% =====================================================================
%% @spec not_end_of_try_expr_after(MId::integer(), 
%%       VarId::integer()) -> bool()
%%
%% @doc
%% Returns true or false if the variable was at the end of a 
%% try expression after.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
not_end_of_try_expr_after(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from try_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=3"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}]=Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from try_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=3 ;"),
            MaxPos /= Pos
    end.

%% =====================================================================
%% @spec not_end_of_try_expr_handlers(MId::integer(), 
%%       VarId::integer()) -> bool()
%%
%% @doc
%% Returns true or false if the variable was at the end of a 
%% try expression handlers.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
not_end_of_try_expr_handlers(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from try_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=2"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}]=Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from try_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=2 ;"),
            MaxPos /= Pos
    end.
    
%% =====================================================================
%% @spec remove_occurrence_from_tree(MId::integer(), 
%%       VarId::integer()) -> ok
%%
%% @doc
%% Removes the variable occurrance if it is not needed in the code, 
%% and removes the variable node too.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================    
remove_occurrence_from_tree(MId, VarId) ->
    case get_if_not_end_of_block_expr(MId, VarId) of
	false ->
	    ok;
	{Id, Pos} ->
	    delete_row(MId, Id, Pos, "block_expr", -1)
    end,
    case get_if_not_end_of_clause_body(MId, VarId) of
	false ->
	    ok;
	{Id2, Pos2} ->
	    delete_row(MId, Id2, Pos2, "clause", 2)
    end,
    case get_if_not_end_of_receive_expr_action(MId, VarId) of
	false ->
	    ok;
	{Id3, Pos3} ->
	    delete_row(MId, Id3, Pos3, "receive_expr", 2)
    end,
    case get_if_not_end_of_try_expr_body(MId, VarId) of
	false ->
	    ok;
	{Id4, Pos4} ->
	    delete_row(MId, Id4, Pos4, "try_expr", 0)
    end,
    case get_if_not_end_of_try_expr_after(MId, VarId) of
	false ->
	    ok;
	{Id5, Pos5} ->
	    delete_row(MId, Id5, Pos5, "try_expr", 3)
    end,
    case get_if_not_end_of_try_expr_handlers(MId, VarId) of
	false ->
	    ok;
	{Id6, Pos6} ->
	    delete_row(MId, Id6, Pos6, "try_expr", 2)
    end,
    delete_nodes:delete_node(MId, VarId).    

%% =====================================================================
%% @spec get_if_not_end_of_block_expr(MId::integer(), 
%%       VarId::integer()) -> bool() | {integer(),integer()}
%%
%% @doc
%% Returns the position and the id of the block expression where the 
%% variable is the last element. 
%% If such block expression does not exists it returns false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
get_if_not_end_of_block_expr(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from block_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and body=" 
	       ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}]=Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from block_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " ;"),
	    case MaxPos==Pos of
		true ->
		    false;
		false ->
		    {Id, Pos}
	    end
    end.

%% =====================================================================
%% @spec get_if_not_end_of_clause_body(MId::integer(), 
%%       VarId::integer()) -> bool() | {integer(),integer()}
%%
%% @doc
%% Returns the position and the id of the clause body where the 
%% variable is the last element. 
%% If such clause body does not exists it returns false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================
get_if_not_end_of_clause_body(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from clause where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=2"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}]=Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from clause where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=2 ;"),
	    case MaxPos==Pos of
		true ->
		    false;
		false ->
		    {Id, Pos}
	    end
    end.

%% =====================================================================
%% @spec get_if_not_end_of_receive_expr_action(MId::integer(), 
%%       VarId::integer()) -> bool() | {integer(),integer()}
%%
%% @doc
%% Returns the position and the id of the receive expression action 
%% where the variable is the last element. 
%% If such receive expression action does not exists it returns false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================    
get_if_not_end_of_receive_expr_action(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from receive_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=2"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}] = Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from receive_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=2 ;"),
	    case MaxPos==Pos of
		true ->
		    false;
		false ->
		    {Id, Pos}
	    end
    end. 

%% =====================================================================
%% @spec get_if_not_end_of_try_expr_body(MId::integer(), 
%%       VarId::integer()) -> bool() | {integer(),integer()}
%%
%% @doc
%% Returns the position and the id of the try expression body 
%% where the variable is the last element. 
%% If such try expression body does not exists it returns false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% ===================================================================== 
get_if_not_end_of_try_expr_body(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from try_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=0"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}] = Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from try_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=0 ;"),
	    case MaxPos == Pos of
		true ->
		    false;
		false ->
		    {Id, Pos}
	    end
    end. 
 
%% =====================================================================
%% @spec get_if_not_end_of_try_expr_after(MId::integer(), 
%%       VarId::integer()) -> bool() | {integer(),integer()}
%%
%% @doc
%% Returns the position and the id of the try expression after 
%% where the variable is the last element. 
%% If such try expression after does not exists it returns false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================   
get_if_not_end_of_try_expr_after(MId, VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from try_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=3"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}] = Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from try_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=3 ;"),
	    case MaxPos == Pos of
		true ->
		    false;
		false ->
		    {Id, Pos}
	    end
    end.

%% =====================================================================
%% @spec get_if_not_end_of_try_expr_handlers(MId::integer(), 
%%       VarId::integer()) -> bool() | {integer(),integer()}
%%
%% @doc
%% Returns the position and the id of the try expression handler 
%% where the variable is the last element. 
%% If such try expression handler does not exists it returns false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of the variable.</pre>
%% @end
%% origin: refac_var_elim
%% ===================================================================== 
get_if_not_end_of_try_expr_handlers(MId,VarId) ->    
    Exists = refactor_db:select(
	       "select id,pos from try_expr where mid=" 
	       ++ integer_to_list(MId) ++ " and qualifier=2"
	       " and argument=" ++ integer_to_list(VarId) ++ " ;"),
    case Exists == [] of
	true ->
	    false;
	false ->
	    [{Id, Pos}] = Exists,
	    [{MaxPos}] = 
		refactor_db:select(
		  "select max(pos) from try_expr where mid=" 
		  ++ integer_to_list(MId) ++ " and id="
		  ++ integer_to_list(Id) ++ " and qualifier=2 ;"),
	    case MaxPos == Pos of
		true ->
		    false;
		false ->
		    {Id, Pos}
	    end
    end.  

%% =====================================================================
%% @spec delete_row(MId::integer(), 
%%       Id::integer(), Pos::integer(), Table:: string(), 
%%       Num::integer()) -> ok
%%
%% @doc
%% Deletes one child node. The MId and the Id identifies the parent node.
%% The Table identifies the type of the node. The other elements identify
%% the child node.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%% <b>Pos</b> : Position of the element.
%% <b>Table</b> : Name of the table.  
%% <b>Num</b> : Extra position information.</pre>
%% @end
%% origin: refac_var_elim
%% =====================================================================    
delete_row(MId, Id, Pos, Value, -1) ->
    refactor_db:delete(
      "delete from " ++ Value ++ " where mid=" ++ integer_to_list(MId) 
      ++ " and id=" ++ integer_to_list(Id) ++ " and pos=" 
      ++ integer_to_list(Pos) ++ " ;"), 
    refactor_db:update(
      "update " ++ Value ++ " set pos=pos-1 where mid=" 
      ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
      ++ " and pos>" ++ integer_to_list(Pos) ++ " ;");
delete_row(MId, Id, Pos, Value, Num) ->
    refactor_db:delete(
      "delete from " ++ Value ++ " where mid=" ++ integer_to_list(MId) 
      ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=" 
      ++ integer_to_list(Num) ++ " and pos=" ++ integer_to_list(Pos) ++ " ;"), 
    refactor_db:update(
      "update " ++ Value ++ " set pos=pos-1 where mid=" 
      ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
      ++ " and qualifier=" ++ integer_to_list(Num) 
      ++ " and pos>" ++ integer_to_list(Pos) ++ " ;").

%%Moduleid

%% =====================================================================
%% @spec get_module_id_from_path_if_exists(File::string()) -> integer()
%%
%% @doc
%% Returns the module's id, if exists. Otherwise empty list.
%% 
%% Parameter description:<pre>
%% <b>File</b> : Path of the module.</pre>
%% @end
%% origin: refac_ren_fun:rename_function/5,
%%         refac_ren_var:rename_variable/5,
%%         refac_reorder_funpar:reorder_funpar/5,
%%         refac_tuple_funpar:untuple_or_tuple_funpar/5
%%         refac_var_elim:eliminate_variable/4,
%%         into_db:simultaneous_visiting/3
%% ===================================================================== 
get_module_id_from_path_if_exists(File) ->
    MId = refactor_db:select(
	    "select mid from module where path=" 
	    ++ io_lib:write_string(File) ++";"),
    MId.

%% =====================================================================
%% @spec get_module_id_from_path(File::string()) -> integer()
%%
%% @doc
%% Returns the module's id.
%% 
%% Parameter description:<pre>
%% <b>File</b> : Path of the module.</pre>
%% @end
%% origin: into_db:simultaneous_visiting/3
%% ===================================================================== 
get_module_id_from_path(File) ->
    [{MId}] = refactor_db:select(
		"select mid from module where path=" 
		++ io_lib:write_string(File) ++";"),
    MId.

%%Module name

%% =====================================================================
%% @spec get_module_name_from_attribute_id(MId::integer(), 
%%           Id::integer()) -> integer()
%%
%% @doc
%% Returns the module's name.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Identifier of the attribute. (importlist) </pre>
%% @end
%% origin: refac_ren_fun:get_imported_functions_and_ids/3
%% ===================================================================== 
get_module_name_from_attribute_id(MId, Id) ->
    [{ModuleName}] = 
	refactor_db:select(
	  "select name from name,attribute_ where name.mid=attribute_.mid"
	  ++ " and name.id=attribute_.argument and pos=1 and attribute_.id="
	  ++ integer_to_list(Id) ++ " and name.mid="
	  ++ integer_to_list(MId) ++ " ;"),
    ModuleName.					     

%% =====================================================================
%% @spec get_module_name(MId::integer()) -> integer()
%%
%% @doc
%% Returns the module's name.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.</pre>
%% @end
%% origin: refac_ren_fun:get_datas_from_imported_too/5,
%%         refac_ren_fun:rename_in_separate_modules_import_list/7,
%%         refactor:get_module_name_if_exists_in_module/4
%% ===================================================================== 
get_module_name(MId) ->
    [{ModuleName}] = 
	refactor_db:select(
	  "select name from form_list,node_type,attribute_,name where " 
	  "node_type.mid=form_list.mid and attribute_.mid=name.mid"
	  " and name.mid=form_list.mid and name.mid=" ++ integer_to_list(MId) 
	  ++ " and node_type.id=form_list.form and node_type.type=4"
	  " and attribute_.id=form and name.id=argument and attribute_.pos=1"
	  " and attribute_.id in (select attribute_.id from attribute_,name"
	  " where attribute_.mid=name.mid and name.mid=" 
	  ++ integer_to_list(MId) ++ " and attribute_.argument=name.id"
	  " and name=\"module\");"),
    ModuleName.

%% =====================================================================
%% @spec get_module_ids_where_used_from_fun_id(MId::integer(), 
%%           Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the module ids in a list where the function is used.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Identifier of the function. </pre>
%% @end
%% origin: refac_tuple_funpar:check_name_clash/7,
%%         refac_tuple_funpar:refresh_import_list/5
%% ===================================================================== 
get_module_ids_where_used_from_fun_id(MId, Id) ->
    UsedLocations = 
	refactor_db:select(
	  "select distinct mid from fun_call where tmid =" 
	  ++ integer_to_list(MId) ++ " and target = " ++ integer_to_list(Id) 
	  ++ " ;"),
    UsedLocations.					    

%%Module qualifier body    

%% =====================================================================
%% @spec get_module_qualifier_body_form_module_qualifier_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns the body id (function name id).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Identifier of the module qualifier. </pre>
%% @end
%% origin: refac_ren_fun:rename_function_calls/5
%% ===================================================================== 
get_module_qualifier_body_form_module_qualifier_id(MId, Id) ->
    [{NameId}] = refactor_db:select(
		   "select body from module_qualifier where mid=" 
		   ++ integer_to_list(MId) ++ " and id= "
		   ++ integer_to_list(Id) ++ ";"),
    NameId.							    

%% Clause patterns

%% =====================================================================
%% @spec get_clause_argument_ids_from_scope(
%%           MId::integer(), Scopes::[integer()]) -> [{integer()}]
%%
%% @doc
%% Returns patterns of the clauses inside the scopes.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Scopes</b> : List of scope ids. </pre>
%% @end
%% origin: refac_var_elim:check_if_binding_is_unambiguous/6
%% ===================================================================== 
get_clause_argument_ids_from_scope(MId, Scopes) ->
    Patterns = refactor_db:select(
		 "select c.argument from clause as c, scope as s"
		 " where c.mid=s.mid and c.mid=" 
		 ++ integer_to_list(MId) ++ " and c.qualifier=0"
		 " and c.argument=s.id and " 
		 ++ create_condition_list3("s.scope=", Scopes) ++ " ;"),
    Patterns.	    

%%Forbidden name type

%% =====================================================================
%% @spec get_forbidden_name_type_from_the_name(
%%           Name::string()) -> [{integer()}]
%%
%% @doc
%% Returns the types of the forbidden functions in a list. 
%% (The list is not empty, when the new name causes name problems with 
%% autoimported functions, reserved words, user denied names)
%% 
%% Parameter description:<pre>
%% <b>Name</b> : Name of the function. </pre>
%% @end
%% origin: refac_ren_fun:is_autoimported/3
%% ===================================================================== 
get_forbidden_name_type_from_the_name(Name) ->
    Exists = refactor_db:select(
	       "select type from forbidden_names where forbidden_name=" 
	       ++ io_lib:write_string(Name) ++ ";"),
    Exists.				 

%%Arity qualifier id

%% =====================================================================
%% @spec get_arity_qualifier_ids_from_attribute_id(
%%           MId::integer(), Id::integer()) -> [{integer(),integer()}]
%%
%% @doc
%% Returns the arity qualifiers in a list of an attribute.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the attribute (importlist). </pre>
%% @end
%% origin: refac_ren_fun:get_imported_functions_and_ids/3
%% ===================================================================== 
get_arity_qualifier_ids_from_attribute_id(MId, Id) ->
    ArityQualifierIds =
	refactor_db:select(
	  "select body,arity from arity_qualifier ar,attribute_ a,list l"
	  " where ar.mid=a.mid and a.mid=l.mid and a.argument=l.id"
	  " and l.element=ar.id and a.mid=" ++ integer_to_list(MId) 
	  ++ " and a.id=" ++ integer_to_list(Id)++ ";"),
    ArityQualifierIds.					     

%% Arity qualifier datas (name,arity pairs)

%% =====================================================================
%% @spec get_arity_qualifier_data(
%%           MId::integer(), ArityQualifierIds::[{integer(),integer()}]) 
%%                                   -> [{string(),integer()}]
%%
%% @doc
%% Returns the name and arity pairs in a list from the id pairs.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ArityQualifierIds</b> : List of name and body id pairs. </pre>
%% @end
%% origin: refac_ren_fun:get_datas/4
%%         refac_tuple_funpar:refresh_export_list/5,
%%         refactor:get_imported_functions/3
%%         refactor:get_export_list/2
%% ===================================================================== 
get_arity_qualifier_data(MId,ArityQualifierIds) ->
    lists:map( fun({BodyId,ArityId}) ->
		       [{Body}] = 
			   refactor_db:select(
			     "select name from name where mid ="
			     ++ integer_to_list(MId) ++ " and id="
			     ++ integer_to_list(BodyId) ++";" ),
		       [{Arity}] = 
			   refactor_db:select(
			     "select value from integer_ where mid ="
			     ++ integer_to_list(MId) ++ " and id="
			     ++ integer_to_list(ArityId) ++ ";"),
		       {Body, list_to_integer(Arity)} end, ArityQualifierIds).

%% =====================================================================
%% @spec get_arity_qualifier_data_and_id(
%%           MId::integer(), ArityQualifierIds::[{integer(),integer()}]) 
%%                                   -> [{string(),integer()}]
%%
%% @doc
%% Returns the name, name id and arity pairs in a list from 
%% the id pairs.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ArityQualifierIds</b> : List of name and body id pairs. </pre>
%% @end
%% origin: refac_ren_fun:get_imported_functions_and_ids/3
%% ===================================================================== 
get_arity_qualifier_data_and_id(MId, ArityQualifierIds) ->
    lists:map( fun({BodyId,ArityId}) ->
		       [{Body}] = refactor_db:select(
				    "select name from name where mid ="
				    ++ integer_to_list(MId) ++ " and id="
				    ++ integer_to_list(BodyId) ++";" ),
		       [{Arity}] = refactor_db:select(
				     "select value from integer_ where mid ="
				     ++ integer_to_list(MId) ++ " and id="
				     ++ integer_to_list(ArityId) ++ ";"),
		       {Body, BodyId, list_to_integer(Arity)} end, 
	       ArityQualifierIds).		       

%% Arity_qualifier body

%% =====================================================================
%% @spec get_arity_qualifier_body_form_arity_qualifier_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns the body id (function name id or arity qualifier id).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Identifier of the arity qualifier. </pre>
%% @end
%% origin: refac_ren_fun:rename_function_calls/5
%% ===================================================================== 
get_arity_qualifier_body_form_arity_qualifier_id(MId, Id) ->
    [{NameId}] = refactor_db:select(
		   "select body from arity_qualifier where mid=" 
		   ++ integer_to_list(MId) ++ " and id= "
		   ++ integer_to_list(Id) ++ ";"),
    NameId.

%%Funid

%% =====================================================================
%% @spec get_fun_id_from_clause_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns function id of the selected clause.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause. </pre>
%% @end
%% origin: refac_ren_fun:find_the_function/4,
%%         refac_reorder_funpar:find_the_function/4
%% ===================================================================== 
get_fun_id_from_clause_id(MId, Id) ->
    FunId = 
	refactor_db:select(
	  "select id from fun_visib where mid =" ++ integer_to_list(MId) 
	  ++ " and argument = " ++ integer_to_list(Id) ++ " and pos !=0;"),
    FunId.

%% =====================================================================
%% @spec get_fun_id_from_clause_id_function_table(
%%           MId::integer(), Id::integer()) -> [{integer()}]
%%
%% @doc
%% Returns function id of the selected clause (scope).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause. </pre>
%% @end
%% origin: refac_ren_var:get_scope_type/3
%%         refac_tuple_funpar:find_the_function/7
%% ===================================================================== 
get_fun_id_from_clause_id_function_table(MId, Id) ->
    FunctionId = 
	refactor_db:select(
	  "select id from function where mid=" ++ integer_to_list(MId) 
	  ++ " and pos!=0 and clause=" ++ integer_to_list(Id) ++ " ;"),
    FunctionId.

%%Funid and type

%% =====================================================================
%% @spec get_id_and_type_list_of_functions_from_pos(
%%           MId::integer(), Line::integer(), 
%%           Col::integer()) -> [{integer(), integer()}]
%%
%% @doc
%% Returns the identifier, type pairs in a list, where the element is 
%% function type (a clause, or an application or an implicit function).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Line</b> : The pointed line number in the module.
%% <b>Col</b> : The pointed column number in the module. </pre>
%% @end
%% origin: refac_ren_fun:rename_function/5
%%         refac_reorder_funpar:reorder_funpar/5
%% ===================================================================== 
get_id_and_type_list_of_functions_from_pos(MId, Line, Col) ->
    Ids = refactor_db:select(
	    "select pos.id,type from node_type,pos where"
	    " pos.mid=node_type.mid and node_type.id=pos.id and pos.mid="
	    ++ integer_to_list(MId) ++" and line=" 
	    ++ integer_to_list(Line) ++ " and col=" ++ integer_to_list(Col) 
	    ++" and ( type=12 or type= 1 or type= 24);" ),
    Ids.	

%% =====================================================================
%% @spec get_id_and_type_list_of_functions_from_pos_no_implicit_fun(
%%           MId::integer(), Line::integer(), 
%%           Col::integer()) -> [{integer(), integer()}]
%%
%% @doc
%% Returns the identifier, type pairs in a list, where the element is 
%% function type (a clause, or an application).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Line</b> : The pointed line number in the module.
%% <b>Col</b> : The pointed column number in the module. </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_funpar/5
%% ===================================================================== 
get_id_and_type_list_of_functions_from_pos_no_implicit_fun(MId, Line, Col) ->
    Ids = refactor_db:select(
	    "select pos.id,type from node_type,pos where"
	    " pos.mid=node_type.mid and pos.mid=" ++ integer_to_list(MId) 
	    ++ " and node_type.id=pos.id and line=" ++ integer_to_list(Line) 
	    ++ " and (type=12 or type=1) and col = (select max(col) from"
	    " node_type,pos where pos.mid=node_type.mid and"
	    " pos.mid=" ++ integer_to_list(MId) ++ " and node_type.id=pos.id"
	    " and line=" ++ integer_to_list(Line) ++ " and col<=" 
	    ++ integer_to_list(Col) ++ " and ( type=12 or type=1 ));" ),
    Ids.	

%% =====================================================================
%% @spec get_fun_id_from_implicit_fun_id_or_application(
%%           MId::integer(), Id::integer()) -> [{integer(), integer()}]
%%
%% @doc
%% Returns module and function id of the function definition.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the implicit fun id or application. </pre>
%% @end
%% origin: refac_ren_fun:find_the_function/4,
%%         refac_reorder_funpar:find_the_function/4,
%%         refac_tuple_funpar:find_the_function/4
%% ===================================================================== 
get_fun_id_from_implicit_fun_id_or_application(MId, Id) ->
    refactor_db:select(
      "select tmid,target from fun_call where mid=" 
      ++ integer_to_list(MId) ++ " and id = " 
      ++ integer_to_list(Id) ++ ";").	

%% Fun clause ids

%% =====================================================================
%% @spec get_clause_mids_and_ids_from_fun_id(
%%           MId::integer(), Id::integer()) -> [{integer(), integer()}]
%%
%% @doc
%% Returns the clause ids of the function in a list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function. </pre>
%% @end
%% origin: refac_reorder_funpar:change_clauses_parameter_order/5
%% ===================================================================== 
get_clause_mids_and_ids_from_fun_id(MId, Id) ->
    ClauseIds = refactor_db:select(
		  "select mid,clause from function where mid=" 
		  ++ integer_to_list(MId) ++ " and id=" 
		  ++ integer_to_list(Id) ++ " and pos!=0;"),
    ClauseIds.

%% =====================================================================
%% @spec get_clause_ids_from_fun_id(
%%           MId::integer(), Id::integer()) -> [{integer()}]
%%
%% @doc
%% Returns the clause ids of the function in a list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function. </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_clause_parameters/6
%% ===================================================================== 
get_clause_ids_from_fun_id(MId, Id) ->
    ClauseIds = refactor_db:select(
		  "select clause from function where mid=" 
		  ++ integer_to_list(MId) ++ " and id=" 
		  ++ integer_to_list(Id) ++ " and pos!=0;"),
    ClauseIds.

%% Argument position

%% =====================================================================
%% @spec get_argument_position_from_clause_id_and_pos(
%%           MId::integer(), Id::integer(), 
%%           Line::integer(), Col::integer()) -> [{integer()}]
%%
%% @doc
%% Returns the sequential number of the pointed argument.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function. 
%% <b>Line</b> : Line number of the argument. 
%% <b>Col</b> : Column number of the argument. </pre>
%% @end
%% origin: refac_tuple_funpar:decide_pos_is_good/6
%% ===================================================================== 
get_argument_position_from_clause_id_and_pos(MId, Id, Line, Col)->
    Pos = refactor_db:select(
	    "select c.pos from clause as c,pos as p where c.mid=p.mid"
	    " and p.mid=" ++ integer_to_list(MId) ++ " and c.id=" 
	    ++ integer_to_list(Id) ++ " and c.qualifier=0"
	    " and c.argument=p.id and p.line=" ++ integer_to_list(Line) 
	    ++ " and p.col=" ++ integer_to_list(Col) ++ " ;"),
    Pos.  

%% =====================================================================
%% @spec get_argument_position_from_application_id_and_pos(
%%           MId::integer(), Id::integer(), 
%%           Line::integer(), Col::integer()) -> [{integer()}]
%%
%% @doc
%% Returns the sequential number of the pointed argument.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function. 
%% <b>Line</b> : Line number of the argument. 
%% <b>Col</b> : Column number of the argument. </pre>
%% @end
%% origin: refac_tuple_funpar:decide_pos_is_good/6
%% ===================================================================== 
get_argument_position_from_application_id_and_pos(MId, Id, Line, Col)->
    Pos = refactor_db:select(
	    "select a.pos from application as a,pos as p where a.mid=p.mid"
	    " and p.mid=" ++ integer_to_list(MId) ++ " and a.id=" 
	    ++ integer_to_list(Id) ++ " and a.pos!=0"
	    " and a.argument=p.id and p.line=" ++ integer_to_list(Line) 
	    ++ " and p.col=" ++ integer_to_list(Col) ++ " ;"), 
    Pos.  

%% =====================================================================
%% @spec get_argument_nearest_position_and_type_from_clause_id_and_pos(
%%           MId::integer(), Id::integer(), 
%%           Line::integer(), Col::integer()) -> [{integer(), integer()}]
%%
%% @doc
%% Returns the sequential number and its type of the nearest argument
%% to the pointed position.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause. 
%% <b>Line</b> : Line number of the argument. 
%% <b>Col</b> : Column number of the argument. </pre>
%% @end
%% origin: refac_tuple_funpar:decide_pos_is_good/6
%% ===================================================================== 
get_argument_nearest_position_and_type_from_clause_id_and_pos(
  MId, Id, Line, Col)->
    NearPosType = 
	refactor_db:select(
	  "select c.pos,c.qualifier from clause as c,pos as p where"
	  " c.mid=p.mid and p.mid=" ++ integer_to_list(MId) ++ " and c.id=" 
	  ++ integer_to_list(Id) ++ " and c.argument=p.id and p.line=" 
	  ++ integer_to_list(Line) ++ " and p.col=(select max(c.pos) from"
	  " clause as c, pos as p where c.mid=p.mid and p.mid=" 
	  ++ integer_to_list(MId) ++ " and c.id=" ++ integer_to_list(Id) 
	  ++ " and c.argument=p.id and p.line=" ++ integer_to_list(Line) 
	  ++ " and col<=" ++ integer_to_list(Col) ++ ");"),
    NearPosType.			       

%% =====================================================================
%% @spec 
%% get_argument_nearest_position_and_type_from_application_id_and_pos(
%%         MId::integer(), Id::integer(), 
%%         Line::integer(), Col::integer()) -> [{integer(), integer()}]
%%
%% @doc
%% Returns the sequential number and its id of the nearest argument
%% to the pointed position.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function. 
%% <b>Line</b> : Line number of the argument position. 
%% <b>Col</b> : Column number of the argument position. </pre>
%% @end
%% origin: refac_tuple_funpar:decide_pos_is_good/6
%% ===================================================================== 
get_argument_nearest_position_and_type_from_application_id_and_pos(
  MId, Id, Line, Col)->
    NearPosNearPosId = 
		refactor_db:select(
		  "select a.pos,a.argument from application as a,pos as p"
		  " where a.mid=p.mid and p.mid=" ++ integer_to_list(MId) 
		  ++ " and a.id=" ++ integer_to_list(Id) 
		  ++ " and a.argument=p.id and a.pos!=0 and p.line=" 
		  ++ integer_to_list(Line) ++ " and p.col=(select max(a.pos)"
		  " from application as a, pos as p where a.mid=p.mid and"
		  " p.mid=" ++ integer_to_list(MId) ++ " and a.id=" 
		  ++ integer_to_list(Id) ++ " and a.pos!=0 and"
		  " a.argument=p.id and p.line=" ++ integer_to_list(Line) 
		  ++ " and p.col<=" ++ integer_to_list(Col) ++ ");"),
    NearPosNearPosId.			       

%% =====================================================================
%% @spec get_max_argument_position_from_clause_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns the last sequential number of the clause argument (arity).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause. </pre>
%% @end
%% origin: refac_tuple_funpar:decide_pos_is_good/6
%% ===================================================================== 
get_max_argument_position_from_clause_id(MId, Id)->
    [{MaxPos}] = 
	refactor_db:select(
	  "select max(pos) from clause where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=0;"),
    MaxPos.		      

%% =====================================================================
%% @spec get_max_argument_position_from_application_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns the last sequential number of the application argument (arity).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the application. </pre>
%% @end
%% origin: refac_tuple_funpar:decide_pos_is_good/6
%% ===================================================================== 
get_max_argument_position_from_application_id(MId, Id)->
    [{MaxPos}] = 
	refactor_db:select(
	  "select max(pos) from application where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" 
	  ++ integer_to_list(Id) ++ " ;"),
    MaxPos.	

%% =====================================================================
%% @spec get_arguments_from_clause_id_and_from_to_pos(
%%           MId::integer(), Id::integer(), 
%%           From::integer(), To::integer()) -> [{integer()}]
%%
%% @doc
%% Returns the argument ids between the given positions.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause.
%% <b>From</b> : Starting argument pos.
%% <b>To</b> : Ending argument pos. </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_clause_parameters_/6
%% ===================================================================== 
get_arguments_from_clause_id_and_from_to_pos(MId, Id, From, To)->
    Elements = 
	refactor_db:select(
	  "select argument from clause where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=0 and pos>=" 
	  ++ integer_to_list(From) ++ " and pos <=" ++ integer_to_list(To) 
	  ++ " ;"),
    Elements.			    

%% =====================================================================
%% @spec get_arguments_from_application_id_and_from_to_pos(
%%           MId::integer(), Id::integer(), 
%%           From::integer(), To::integer()) -> [{integer()}]
%%
%% @doc
%% Returns the argument ids between the given positions.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the application.
%% <b>From</b> : Starting argument pos.
%% <b>To</b> : Ending argument pos. </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_applications_parameters_/6
%% ===================================================================== 
get_arguments_from_application_id_and_from_to_pos(MId, Id, From, To) ->
    Elements = 
	refactor_db:select(
	  "select argument from application where mid=" 
	  ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos>=" 
	  ++ integer_to_list(From) ++ " and pos <=" 
	  ++ integer_to_list(To) ++ " ;"),
    Elements.			    

%% =====================================================================
%% @spec get_arity_from_fun_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns arity id of the function.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause. </pre>
%% @end
%% origin: refac_ren_fun:find_the_function/4,
%%         refac_reorder_funpar:find_the_function/4
%% ===================================================================== 
get_arity_from_fun_id(MId, Id) ->
    [{Arity}] = 
	refactor_db:select(
	  "select argument from fun_visib where mid=" ++ integer_to_list(MId) 
	  ++ " and id = " ++ integer_to_list(Id) ++ " and pos =0;"),
    Arity.



%%Arity

%% =====================================================================
%% @spec get_arity_from_arity_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns value of the arity.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the arity. </pre>
%% @end
%% origin: refac_ren_fun:is_exported/5
%% ===================================================================== 
get_arity_from_arity_id(MId, Id) ->
    [{Arity}] = 
	refactor_db:select(
	  "select value from integer_ where mid=" ++ integer_to_list(MId) 
	  ++ " and id= " ++ integer_to_list(Id)++";" ),
    Arity.

%% =====================================================================
%% @spec get_application_arity_from_application_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns the application's arity.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the application. </pre>
%% @end
%% origin: into_db:get_applications_data/2
%% ===================================================================== 
get_application_arity_from_application_id(MId, Id) ->
    [{Arity}] = 
	refactor_db:select(
	  "select count(*) from application where"
	  ++ " mid=" ++ integer_to_list(MId) ++ " and"
	  ++ " id=" ++ integer_to_list(Id) ++ " and"
	  ++ " pos!=0;"),
    list_to_integer(Arity).

%%Funnameid

%% =====================================================================
%% @spec get_fun_name_id_from_fun_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns the id of the function name.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function. </pre>
%% @end
%% origin: refac_ren_fun:find_the_function/4, 
%%         refac_ren_fun:get_datas/3
%%         refac_ren_fun:rename_clauses/4
%% ===================================================================== 
get_fun_name_id_from_fun_id(MId, Id) ->
    [{FunnameId}] = 
	refactor_db:select(
	  "select clause from function where mid=" ++ integer_to_list(MId) 
	  ++ " and pos =0 and id = " ++ integer_to_list(Id) ++ ";"),
    FunnameId.				 

%%Funname

%% =====================================================================
%% @spec get_fun_name(
%%           MId::integer(), Id::integer()) -> string()
%%
%% @doc
%% Returns the name of the function.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function name. </pre>
%% @end
%% origin: refac_tuple_funpar:find_the_function/4,
%%         refac_tuple_funpar:find_the_function/7
%%         refac_tuple_funpar:refresh_export_list/5
%% ===================================================================== 
get_fun_name(MId, Id) ->
    [{FunName}] = 
	refactor_db:select(
	  "select name from function,name where name.mid=function.mid"
	  " and pos=0 and clause=name.id and name.mid=" 
	  ++ integer_to_list(MId) ++ " and function.id=" 
	  ++ integer_to_list(Id) ++ ";"),
    FunName.

%%Application type and name id

%% =====================================================================
%% @spec get_application_type_mid_id_from_fun_call_id(
%%           MId::integer(), Id::integer()) -> 
%%                                 [{integer(), integer(), integer()}]
%%
%% @doc
%% Returns the type, module id, id of the application. 
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function call. </pre>
%% @end
%% origin: refac_ren_fun:get_fun_calls/3
%% ===================================================================== 
get_application_type_mid_id_from_fun_call_id(MId, Id) ->
    [TypeandNameIds] =
	refactor_db:select(
	  "select type,application.mid,argument from application,node_type"
	  " where application.id= " ++ integer_to_list(Id)
	  ++ " and application.argument=node_type.id"
	  " and application.pos=0 and application.mid=" 
	  ++ integer_to_list(MId) ++ " and application.mid=node_type.mid ;"),
    TypeandNameIds.

%%Implicit fun call type 

%% =====================================================================
%% @spec get_implicit_fun_call_type_from_implicit_fun_id(
%%           MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Returns the type of the implicit fun call.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function call. </pre>
%% @end
%% origin: refac_reorder_funpar:get_fun_calls/5,
%%         refac_tuple_funpar:do_the_tupling/6
%% ===================================================================== 
get_implicit_fun_call_type_from_implicit_fun_id(MId, Id) ->
    [{Type}] = 
	refactor_db:select(
	  "select type from node_type as n,implicit_fun as i"
	  " where n.mid=i.mid and i.mid=" ++ integer_to_list(MId) 
	  ++ " and i.id=" ++ integer_to_list(Id) ++ " and i.name_id=n.id;"),
    Type.					     

%%Implicit fun call type and name id

%% =====================================================================
%% @spec get_implicit_fun_call_type_mid_id_from_fun_call_id(
%%           MId::integer(), Id::integer()) -> 
%%                                 {integer(), integer(), integer()}
%%
%% @doc
%% Returns the type, module id, id of the implicit fun call.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function call. </pre>
%% @end
%% origin: refac_ren_fun:get_fun_calls/3
%% ===================================================================== 
get_implicit_fun_call_type_mid_id_from_fun_call_id(MId, Id) ->
    [TypeandNameIds] =
	refactor_db:select(
	  "select type,implicit_fun.mid,name_id from implicit_fun,node_type"
	  " where name_id=node_type.id and implicit_fun.id="
	  ++ integer_to_list(Id) ++ " and implicit_fun.mid=node_type.mid"
	  ++ " and implicit_fun.mid=" ++ integer_to_list(MId) ++ " ;"),
    TypeandNameIds.

%% Id

%% =====================================================================
%% @spec get_id_from_pos(
%%          MId::integer(), Line::integer(), Col::integer()) -> 
%%                                 [{integer()}]
%%
%% @doc
%% Returns the id of the element in the given position.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Line</b> : The pointed line number in the module.
%% <b>Col</b> : The pointed column number in the module. </pre>
%% @end
%% origin: refac_tuple_funpar:local_preorder/5
%% =====================================================================     
get_id_from_pos(MId, Line, Col) ->
    ElementsId = 
	refactor_db:select(
	  "select id from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and line=" ++ integer_to_list(Line) ++ " and col=" 
	  ++ integer_to_list(Col) ++ " ;"),
    ElementsId.				   

%%Imporlist id

%% =====================================================================
%% @spec get_import_list_ids(MId::integer()) -> [{integer()}]
%%
%% @doc
%% Returns the import list ids of the module in a list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: refac_ren_fun:get_datas_from_imported_too/5,
%%         refac_ren_fun:get_datas/3,
%%         refac_ren_fun:rename_in_separate_modules_import_list/7
%% =====================================================================     
get_import_list_ids(MId) ->
    ImportListIds = 
	refactor_db:select(
	  "select distinct attribute_.id from form_list,node_type,attribute_"
	  " where node_type.mid=form_list.mid and "
	  "attribute_.mid=form_list.mid and attribute_.mid=" 
	  ++ integer_to_list(MId) ++ " and node_type.id=form_list.form"
	  " and node_type.type=4 and attribute_.id=form and attribute_.id in" 
	  " (select attribute_.id from attribute_,name where"
	  " attribute_.mid=name.mid and name.mid=" ++ integer_to_list(MId) 
	  ++ " and attribute_.argument=name.id and name=\"import\");"),
    ImportListIds.

%% Infix expression

%% =====================================================================
%% @spec get_infix_expr_oper_from_tree(
%%           MId::integer(), Id::integer()) ->  integer()
%%
%% @doc
%% Returns the oper of the infix expr.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the root of the infix expr subtree. </pre>
%% @end
%% origin: refac_var_elim:check_sideeffect/3
%% ===================================================================== 
get_infix_expr_oper_from_tree(MId, Id) ->
    [{Oper}] = 
	refactor_db:select(
	  "select oper from infix_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " ;"),
    Oper.

%% Match expression

%% =====================================================================
%% @spec get_match_expr_id_from_pattern_and_body_id(
%%           MId::integer(), Id::integer(), 
%%                           BodyId::integer()) ->  integer()
%%
%% @doc
%% Returns the id of the match expression.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the pattern. 
%% <b>BodyId</b> : Id of the body. </pre>
%% @end
%% origin: refac_var_elim:check_if_binding_occurence_needed/4,
%%         refac_var_elim:remove_binding_occurrence_and_body/4
%%         refac_var_elim:remove_binding_occurrence/4
%% ===================================================================== 
get_match_expr_id_from_pattern_and_body_id(MId, Id, BodyId) ->
    [{MatchId}] = 
	refactor_db:select(
	  "select id from match_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and pattern=" ++ integer_to_list(Id) ++ " and body=" 
	  ++ integer_to_list(BodyId) ++ " ;"),
    MatchId.	   

%% =====================================================================
%% @spec get_match_expr_id_and_body_from_pattern(
%%           MId::integer(), Id::integer()) ->  [{integer(), integer}]
%%
%% @doc
%% Returns the id and body of the match expression.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the pattern. </pre>
%% @end
%% origin: refac_var_elim:check_if_binding_occurrence_is_a_match_expr/4
%% ===================================================================== 
get_match_expr_id_and_body_from_pattern(MId, Id) ->
    PatternIdBody = 
	refactor_db:select(
	  "select id,body from match_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and pattern=" ++ integer_to_list(Id) ++ " ;"),
    PatternIdBody.	

%% =====================================================================
%% @spec get_match_expr_patterns_from_scopes(
%%           MId::integer(), Scopes::[integer()]) ->  [{integer()}]
%%
%% @doc
%% Returns the pattern ids of the match expression in the scopes.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Scopes</b> : Scope id list. </pre>
%% @end
%% origin: refac_var_elim:check_if_binding_is_unambiguous/6
%% ===================================================================== 
get_match_expr_patterns_from_scopes(MId, Scopes) ->
    MatchExprs = 
	refactor_db:select(
	  "select m.pattern from match_expr as m, scope as s where"
	  " m.mid=s.mid and m.mid=" ++ integer_to_list(MId) 
	  ++ " and m.pattern=s.id and " 
	  ++ create_condition_list3("s.scope=", Scopes) ++ " ;"),
    MatchExprs.	

%% Name

%% =====================================================================
%% @spec get_name_from_name_id(
%%           MId::integer(), Id::integer()) ->  string()
%%
%% @doc
%% Returns name of the element.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the name. </pre>
%% @end
%% origin: refac_ren_fun:find_the_function/4, refac_ren_fun:get_datas/3,
%%         refac_ren_fun:is_exported/5,
%%         refac_var_elim:check_var/3
%% ===================================================================== 
get_name_from_name_id(MId, Id) ->
    [{Name}] = 
	refactor_db:select(
	  "select name from name where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++ ";"),
    Name.

%% Name and Pos

%% =====================================================================
%% @spec get_name_and_pos_from_name_id(
%%           MId::integer(), Id::integer()) ->  
%%                   {string(), integer(), integer()}
%%
%% @doc
%% Returns name and pos (line and column) of the element.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the name. </pre>
%% @end
%% origin: refac_var_elim:is_there_a_pattern_variable_there/4
%% original name: get_error_datas/3
%% ===================================================================== 
get_name_and_pos_from_name_id(MId, Id) ->
    [Datas] = 
	refactor_db:select(
	  "select n.name,p.line,p.col from name as n,pos as p where"
	  " p.mid=n.mid and p.id=n.id and n.mid=" ++ integer_to_list(MId) 
	  ++ " and n.id=" ++ integer_to_list(Id) ++ " ;"),
    Datas.

%%Pos

%% =====================================================================
%% @spec get_true_pos_from_pointed_pos(
%%           MId::integer(), Line::integer(), Col::integer()) ->  
%%                   {integer(), integer()}
%%
%% @doc
%% Returns the position of the element which is stored in the database.
%% (the pointed position can be differrent).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Line</b> : The pointed line number in the module.
%% <b>Col</b> : The pointed column number in the module. </pre>
%% @end
%% origin: refac_ren_fun:rename_function/5,
%%         refac_ren_var:rename_variable/5,
%%         refac_reorder_funpar:reorder_funpar/5,
%%         refac_tuple_funpar:untuple_or_tuple_funpar/5
%%         refac_var_elim:eliminate_variable/4
%% ===================================================================== 
get_true_pos_from_pointed_pos(MId, Line, Col) ->
    CColList =
	refactor_db:select(
	  "select max(col) from pos where mid="
	  ++ integer_to_list(MId) 
	  ++ " and line=" ++ integer_to_list(Line) ++ " and col<=" 
	  ++ integer_to_list(Col+1) ++";" ), 
    if 
        CColList == [{null}] ->
            case  refactor_db:select(
                    "select max(col) from pos where mid="
                    ++ integer_to_list(MId) 
                    ++ " and line=" ++ integer_to_list(Line-1) ++ " ;" ) of
                [{null}] -> [];
                [{CCol}] -> {Line-1, CCol}
            end;
        true  ->
            {Line, element(1,hd(CColList))}
    end.

%% =====================================================================
%% @spec get_pos_from_id(MId::integer(), Id::integer()) ->  
%%                   {integer(), integer()}
%%
%% @doc
%% Returns the position (line, column) of the element.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the element. </pre>
%% @end
%% origin: refac_ren_var:check_list_comp/5,
%%         refac_ren_var:check_fun_expr/5,
%%         refac_ren_var:check_function/5,
%%         refac_var_elim:check_if_binding_occurrence_is_a_match_expr/4
%% ===================================================================== 
get_pos_from_id(MId, Id) ->
    [{Line,Col}] = 
	refactor_db:select(
	  "select line,col from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " ;"),
    {Line,Col}.

%% =====================================================================
%% @spec get_scope_from_id(MId::integer(), Id::integer()) ->  
%%                   integer()
%%
%% @doc
%% Returns the scope of the element.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the element. </pre>
%% @end
%% origin: refac_reorder_funpar:change_implicit_funcall_order/5,
%%         refac_tuple_funpar:tuple_applications_parameter/6,
%%         refac_tuple_funpar:tuple_implicit_functions/5,
%%         refac_var_elim:replace_occurrence_with_body/4,
%%         refac_var_elim:remove_binding_occurrence/4
%% ===================================================================== 
get_scope_from_id(MId, Id) ->
    [{ScopeId}] = 
	refactor_db:select(
	  "select scope from scope where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " ;"),
    ScopeId.

%% Inner scopes

%% =====================================================================
%% @spec get_inner_scope_ids_from_scope_id(
%%         MId::integer(), Id::integer()) ->  
%%                   [{integer()}]
%%
%% @doc
%% Returns ids of the inner scopes.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the scope. </pre>
%% @end
%% origin: refac_var_elim:produce_inner_scopes/3
%% ===================================================================== 
get_inner_scope_ids_from_scope_id(MId, Id) ->
    Scopes  = 
	refactor_db:select(
	  "select id from scope_visib where mid=" ++ integer_to_list(MId) 
	  ++ " and target=" ++ integer_to_list(Id) ++ " ;"), 
    Scopes.		    

%% Inner scopes, where name clash can be caused by the variable

%% =====================================================================
%% @spec get_inner_scope_ids_from_scope_and_var_id(
%%         MId::integer(), ScopeId::integer(), BId::integer()) ->  
%%                   [{integer()}]
%%
%% @doc
%% Returns ids of the inner scopes, where name clash can be caused by
%% the variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ScopeId</b> : Id of the scope. 
%% <b>BId</b> : Id of the variable's binding occurrence. </pre>
%% @end
%% origin: refac_ren_var:name_capture_innner_lower/5
%% ===================================================================== 
get_inner_scope_ids_from_scope_and_var_id(MId, ScopeId, BId) ->
    InnerScopes = 
	refactor_db:select(
	  "select s.id from scope_visib as s, pos as p1, pos as p2 where"
	  " s.mid=p1.mid and p1.mid=p2.mid and s.mid=" ++ integer_to_list(MId) 
	  ++ " and s.target=" ++ integer_to_list(ScopeId) ++ " and s.id!=" 
	  ++ integer_to_list(ScopeId) ++ " and p1.id=s.id and p2.id=" 
	  ++ integer_to_list(BId) ++ " and p1.line>=p2.line and"
	  " p1.col>=p2.col;"),
    InnerScopes.				       

%% Outer scopes, where name clash can be caused by the variable

%% =====================================================================
%% @spec get_outer_scope_ids_from_scope_and_var_id(
%%         MId::integer(), ScopeId::integer(), BId::integer()) ->  
%%                   [{integer()}]
%%
%% @doc
%% Returns ids of the outer scopes, where name clash can be caused 
%% by the variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ScopeId</b> : Id of the scope. 
%% <b>BId</b> : Id of the variable's binding occurrence. </pre>
%% @end
%% origin: refac_ren_var:name_capture_outer_upper/5
%% ===================================================================== 
get_outer_scope_ids_from_scope_and_var_id(MId, ScopeId, BId) ->
    OuterScopes = 
	refactor_db:select(
	  "select s.target from scope_visib as s, pos as p1, pos as p2 where"
	  " s.mid=p1.mid and p1.mid=p2.mid and s.mid=" ++ integer_to_list(MId) 
	  ++ " and s.id=" ++ integer_to_list(ScopeId) ++ " and s.target!=" 
	  ++ integer_to_list(ScopeId) ++ " and p1.id=s.target and p2.id=" 
	  ++ integer_to_list(BId) ++ " and p1.line<=p2.line and"
	  " p1.col<=p2.col;"),
    OuterScopes.				       

%% Tuple elements

%% =====================================================================
%% @spec get_tuple_element_ids_from_tuple_id(
%%         MId::integer(), Id::integer()) ->  [{integer()}]
%%
%% @doc
%% Returns ids of the tuple elements in a list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the tuple. </pre>
%% @end
%% origin: refac_ren_fun:rename_function_calls/5
%% ===================================================================== 
get_tuple_element_ids_from_tuple_id(MId, Id) ->
    Elements = 
	refactor_db:select(
	  "select element from tuple where mid= " 
	  ++ integer_to_list(MId) ++ " and id= "
	  ++ integer_to_list(Id) ++ ";"),
    Elements.				
							  
%%Variable id

%% =====================================================================
%% @spec get_var_id_from_pos(
%%           MId::integer(), Line::integer(), Col::integer()) ->  
%%                   [{integer()}]
%%
%% @doc
%% Returns id of the variable in the position.
%% Check if the one from the nearest nodes (in the syntax tree) 
%% is a variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Line</b> : Line number of the position.
%% <b>Col</b> : Column number of the position. </pre>
%% @end
%% origin: refac_ren_var:rename_variable/5
%%         refac_var_elim:eliminate_variable/4
%% ===================================================================== 
get_var_id_from_pos(MId, Line, Col) ->
    Id = refactor_db:select(
	   "select pos.id from node_type,pos where pos.mid=node_type.mid and"
	   " node_type.id=pos.id and pos.mid="
	   ++ integer_to_list(MId) ++" and line=" 
	   ++ integer_to_list(Line) ++ " and col=" ++ integer_to_list(Col) 
	   ++" and type=50;" ), 
    Id.		       

%% Binding occurence of the variable

%% =====================================================================
%% @spec get_var_bind_occ_from_id(
%%         MId::integer(), Id::integer()) ->  integer()
%%
%% @doc
%% Returns the binding occurence id of the variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the variable. </pre>
%% @end
%% origin: refac_var_elim:check_var/3
%% ===================================================================== 
get_var_bind_occ_from_id(MId, Id) ->
    [{BId}] = 
	refactor_db:select(
	  "select target from var_visib where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " ;"),
    BId.			   

%% Binding occurence and scope of the variable

%% =====================================================================
%% @spec get_var_bind_occ_and_scope_from_id(
%%         MId::integer(), Id::integer()) ->  {integer(), integer()}
%%
%% @doc
%% Returns the binding occurence id and the scope id of the variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the variable. </pre>
%% @end
%% origin: refac_ren_var:get_binding_occurrence_and_scope/4,
%%         refac_var_elim:get_binding_occurrence_and_scope/3
%% ===================================================================== 
get_var_bind_occ_and_scope_from_id(MId, Id) ->
    [{BId,ScopeId}] = 
	refactor_db:select(
	  "select v.target,s.scope from var_visib as v,scope as s where"
	  " s.mid=v.mid and v.target=s.id and v.mid=" 
	  ++ integer_to_list(MId) ++ " and v.id= " 
	  ++ integer_to_list(Id) ++ " ;"),
    {BId, ScopeId}.    

%%Variable name clash inside the scope

%% =====================================================================
%% @spec get_clashed_var_id_from_scope_and_name(
%%         MId::integer(), ScopeId::integer(), 
%%            NewName::string()) ->  [{integer()}]
%%
%% @doc
%% Returns ids of the clashed variables inside the scope.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ScopeId</b> : Id of the scope.
%% <b>NewName</b> : New name of the variable, which may cause name clash.</pre>
%% @end
%% origin: refac_ren_var:name_clash_inside/4
%% ===================================================================== 
get_clashed_var_id_from_scope_and_name(MId, ScopeId, NewName) ->
    Id = refactor_db:select(
	   "select n.id from var_visib as v,name as n,scope as s where"
	   " s.mid=v.mid and v.mid=n.mid and n.mid=" 
	   ++ integer_to_list(MId) ++ " and n.id=v.target and"
	   " s.id=v.target and v.target=v.id and s.scope=" 
	   ++ integer_to_list(ScopeId) ++ " and n.name=" 
	   ++ io_lib:write_string(NewName) ++ " ;"),
    Id.

%%Pattern variable

%% =====================================================================
%% @spec get_pattern_var_id_from_var_bid_and_scope(
%%         MId::integer(), BId::integer(), 
%%            Scopes::[integer()]) ->  [{integer()}]
%%
%% @doc
%% Returns the id of the pattern variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : Binding occurrence id of the variable.
%% <b>Scopes</b> : Ids of the scopes. </pre>
%% @end
%% origin: refac_var_elim:check_inner_scopes_too/4
%% ===================================================================== 
get_pattern_var_id_from_var_bid_and_scope(MId, BId, Scopes) ->
    PatternVariable = 
	refactor_db:select(
	  "select v.id from var_visib as v, scope as s where s.mid=v.mid"
	  " and v.id=s.id and v.mid=" ++ integer_to_list(MId) 
	  ++ " and v.target=" ++ integer_to_list(BId) ++ " and " 
	  ++ create_condition_list3("s.scope=", Scopes) ++ ";"),
    PatternVariable.					   

%%Variable shadows

%% =====================================================================
%% @spec get_shadows_scope_and_bid_from_var_id(
%%         MId::integer(), Scopes::[integer()],
%%         VarIdBIdNames::[{integer(), integer(), string()}]) ->  
%%                               [{integer(), integer()}]
%%
%% @doc
%% Returns scope and binding occurrrence ids of the shadows.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Scopes</b> : Ids of the scopes.
%% <b>VarIdBIdNames</b> : 
%%     The names, binding occurrence ids, and ids of the variables. </pre>
%% @end
%% origin: refac_var_elim:are_match_body_variables_shadowed/5
%% ===================================================================== 
get_shadows_scope_and_bid_from_var_id(MId, Scopes, VarIdBIdNames) ->
    Shadows = 
	refactor_db:select(
	  "select s.scope,v.target from scope as s ,var_visib as v,"
	  " name as n,node_type as n2 where v.mid=s.mid"
	  " and n.mid=v.mid and n2.mid=v.mid and v.mid=" 
	  ++ integer_to_list(MId) ++ " and " 
	  ++ create_condition_list3("s.scope=", Scopes)  
	  ++ " and v.id=s.id and n.id=v.id and n2.id=n.id and n2.type=50 and" 
	  ++ create_condition_list2("n.name=", "v.target!=",VarIdBIdNames) 
	  ++ " ;"),
    Shadows.

%%Collectors

%% =====================================================================
%% @spec get_every_fun_call_and_type_from_fun_id(
%%         MId::integer(), Id::integer()) ->  
%%                           [{integer(), integer(), integer()}]
%%
%% @doc
%% Returns the functions calling datas in a list.
%% (module id, call id, type) 
%% Collects every function calls of the function.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function. </pre>
%% @end
%% origin: refac_ren_fun:get_fun_calls/3,
%%         refac_reorder_funpar:get_fun_calls/5
%% ===================================================================== 
get_every_fun_call_and_type_from_fun_id(MId, Id) ->
    CallIds = 
	refactor_db:select(
	  "select fun_call.mid,fun_call.id,type from fun_call,node_type"
	  " where target= " ++integer_to_list(Id) 
	  ++ " and node_type.id=fun_call.id and fun_call.mid=node_type.mid"
	  " and fun_call.tmid=" ++ integer_to_list(MId)++ " ;"),
    CallIds.

%% =====================================================================
%% @spec get_every_type_and_fun_call_from_fun_id(
%%         MId::integer(), Id::integer()) ->  
%%                           [{integer(), integer(), integer()}]
%%
%% @doc
%% Returns the type, mid, id of the call in a list. 
%% Collects every function calls of the function.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function. </pre>
%% @end
%% origin: refac_tuple_funpar:do_the_tupling/6
%% ===================================================================== 
get_every_type_and_fun_call_from_fun_id(MId, Id) ->
    CallIds = 
	refactor_db:select(
	  "select type,fun_call.mid,fun_call.id from fun_call,node_type"
	  " where target= " ++integer_to_list(Id) 
	  ++ " and node_type.id=fun_call.id and fun_call.mid=node_type.mid"
	  " and fun_call.tmid=" ++ integer_to_list(MId)++ " ;"),
    CallIds.

%% =====================================================================
%% @spec get_every_mid() ->  [{integer()}]
%%
%% @doc
%% Returns all module identifier from the databse in a list.
%% 
%% @end
%% origin: refac_ren_fun:get_datas_from_imported_too/5
%% ===================================================================== 
get_every_mid() ->
    MIds = refactor_db:select("select mid from module;"),
    MIds.

%% =====================================================================
%% @spec get_all_function_id_from_the_module(
%%         MId::integer()) -> [{integer()}]
%%
%% @doc
%% Returns all function id from the module in a list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: refac_ren_fun:get_datas/3
%% ===================================================================== 
get_all_function_id_from_the_module(MId) ->
    FunIds = refactor_db:select(
	       "select distinct id from fun_visib where mid=" 
	       ++ integer_to_list(MId) ++ " ;"),
    FunIds.

%% =====================================================================
%% @spec get_all_arity_from_the_module(
%%         MId::integer()) -> [{integer()}]
%%
%% @doc
%% Returns all arities from a module in a list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: refac_ren_fun:get_datas/3
%% ===================================================================== 
get_all_arity_from_the_module(MId) ->
    Arities = 
	refactor_db:select(
	  "select argument from fun_visib where mid=" 
	  ++ integer_to_list(MId) ++ " and pos=0 ;"),
    Arities.

%% =====================================================================
%% @spec get_every_occurrence_of_a_var_from_id(
%%         MId::integer(), Id::integer()) ->  
%%                           [{integer()}]
%%
%% @doc
%% Returns all occurrences' id of the binding occurrence in a list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Binding occurence id of the variable. </pre>
%% @end
%% origin: refac_var_elim:get_all_occurrences/3
%% ===================================================================== 
get_every_occurrence_of_a_var_from_id(MId, Id) ->
    Occurrences = 
	refactor_db:select(
	  "select id from var_visib where mid=" ++ integer_to_list(MId) 
	  ++ " and target=" ++ integer_to_list(Id) ++ " ;"),
    Occurrences.				       

%% =====================================================================
%% @spec get_all_dynamic_fun_call_type() ->  [{integer()}]
%%
%% @doc
%% Returns the type of all the dynamic calls.
%% 
%% @end
%% origin: refac_ren_fun:finish_rename_function_check_for_possible_problems/1,
%%         refac_reorder_funpar:get_fun_calls/5,
%%         refac_tuple_funpar:do_the_tupling/6
%% ===================================================================== 
get_all_dynamic_fun_call_type() ->
    DynamicFunCall = 
	refactor_db:select(
	  "select type from application,node_type where"
	  " application.mid=node_type.mid and"
	  " node_type.id=application.argument and pos=0 and type=50;"),
    DynamicFunCall.	


%% =====================================================================
%% @spec check_if_apply_exists_in_system() ->  [{integer()}]
%%
%% @doc
%% Checks if apply call exists in the refactoring system (database).
%% 
%% @end
%% ===================================================================== 
check_if_apply_exists_in_system() ->
    refactor_db:select(
      "select a.id from application as a, name as n where"
      " a.mid=n.mid and a.argument=n.id and n.name=\"apply\";") ++
        refactor_db:select(
          "select m.id from module_qualifier as m, name as n,"
          " name as n2 where m.mid=n.mid and n.mid=n2.mid and"
          " m.body=n.id and m.module=n2.id and n.name=\"apply\""
          " and n2.name=\"erlang\";") ++
        refactor_db:select(
          "select m.id from arity_qualifier as m, name as n"
          " where m.mid=n.mid and m.body=n.id and n.name=\"apply\";") ++
        refactor_db:select(
          "select m.id from module_qualifier as m, name as n, name as n2,"
          " arity_qualifier as ar where m.mid=n.mid and n.mid=n2.mid and"
          " n2.mid=ar.mid and m.body=ar.id and m.module=n2.id and ar.body=n.id"
          " and n.name=\"apply\" and n2.name=\"erlang\";").

%% =====================================================================
%% @spec check_if_spawn_exists_in_system() ->  [{integer()}]
%%
%% @doc
%% Checks if spawn call exists in the refactoring system (database).
%% 
%% @end
%% ===================================================================== 
check_if_spawn_exists_in_system() ->
    refactor_db:select(
      "select a.id from application as a, name as n where"
      " a.mid=n.mid and a.argument=n.id and n.name like 'spawn%';") ++
        refactor_db:select(
          "select m.id from module_qualifier as m, name as n,"
          " name as n2 where m.mid=n.mid and n.mid=n2.mid and"
          " m.body=n.id and m.module=n2.id and n.name like 'spawn%'"
          " and n2.name=\"erlang\";") ++
        refactor_db:select(
          "select m.id from arity_qualifier as m, name as n"
          " where m.mid=n.mid and m.body=n.id and n.name like 'spawn%';") ++
        refactor_db:select(
          "select m.id from module_qualifier as m, name as n, name as n2,"
          " arity_qualifier as ar where m.mid=n.mid and n.mid=n2.mid and"
          " n2.mid=ar.mid and m.body=ar.id and m.module=n2.id and ar.body=n.id"
          " and n.name like 'spawn%' and n2.name=\"erlang\";").

%% =====================================================================
%% @spec check_if_hibernate_exists_in_system() ->  [{integer()}]
%%
%% @doc
%% Checks if hibernate call exists in the refactoring system (database).
%% 
%% @end
%% ===================================================================== 
check_if_hibernate_exists_in_system() ->
    refactor_db:select(
      "select m.id from module_qualifier as m, name as n,"
      " name as n2 where m.mid=n.mid and n.mid=n2.mid and"
      " m.body=n.id and m.module=n2.id and n.name=\"hibernate\""
      " and n2.name=\"erlang\";") ++
        refactor_db:select(
          "select m.id from module_qualifier as m, name as n, name as n2,"
          " arity_qualifier as ar where m.mid=n.mid and n.mid=n2.mid and"
          " n2.mid=ar.mid and m.body=ar.id and m.module=n2.id and ar.body=n.id"
          " and n.name=\"hibernate\" and n2.name=\"erlang\";").



%% =====================================================================
%% @spec get_all_fun_name_and_fun_id(
%%         MId::integer()) ->  [{string(), integer(), integer()}]
%%
%% @doc
%% Returns all {Name,Id,MId} tuple's in module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: into_db:get_fun_datas/2
%% ===================================================================== 
get_all_fun_name_and_fun_id(MId) ->
    FunNIM = 
	refactor_db:select(
	  "select name,function.id,name.mid from function,name where"
	  " name.mid=function.mid and pos=0 and clause=name.id and name.mid=" 
	  ++ integer_to_list(MId) ++ ";"),%Funname, Funid, and ModuleId
    FunNIM.

%% =====================================================================
%% @spec get_all_simple_application_mid_id_and_name(
%%         MId::integer()) ->  [{integer(), string(), integer()}]
%%
%% @doc
%% Returns all simple applications' datas in module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: into_db:get_applications_datas/2
%% ===================================================================== 
get_all_simple_application_mid_id_and_name(MId) ->
    SApplicationMIdIdName = 
	refactor_db:select(
	  "select application.mid,name,application.id from"
	  " application,node_type,name where node_type.mid=application.mid"
	  " and node_type.mid=name.mid and name.mid=" ++ integer_to_list(MId) 
	  ++ " and application.argument=node_type.id and pos=0 and"
	  " type=3 and name.id=node_type.id;"),
    SApplicationMIdIdName.

%% =====================================================================
%% @spec get_all_module_application_mid_id_module_name_and_fun_name(
%%         MId::integer()) ->  
%%                     [{integer(), integer(), string(), string()}]
%%
%% @doc
%% Returns all module applications' datas in module
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: into_db:get_applications_datas/2
%% ===================================================================== 
get_all_module_application_mid_id_module_name_and_fun_name(MId) ->
    MApplicationMIdIdModuleFunNames = 
	refactor_db:select(
	  "select a.mid,a.id,n1.name,n2.name from application as a,"
	  " node_type as n,module_qualifier as m,name as n1,name as n2"
	  " where n.mid=a.mid and n.mid=m.mid and n1.mid=n2.mid and"
	  " n1.mid=a.mid and a.argument=n.id and a.pos=0 and n.type=31 and"
	  " m.id=a.argument and n1.id=m.module and n2.id=m.body and n.mid=" 
	  ++ integer_to_list(MId) ++ ";"),
    MApplicationMIdIdModuleFunNames.

%% =====================================================================
%% @spec get_all_deprecated_application_mid_id_and_tuple_id(
%%         MId::integer()) ->  
%%                     [{integer(), integer(), integer()}]
%%
%% @doc
%% Returns all deprecated applications' datas in module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: into_db:get_applications_datas/2
%% ===================================================================== 
get_all_deprecated_application_mid_id_and_tuple_id(MId) ->    
    DApplicationMIdIdTupleId = 
	refactor_db:select(
	  "select application.mid,application.id,application.argument from"
	  " application,node_type,tuple,node_type as n where"
	  " node_type.mid=application.mid and tuple.mid=node_type.mid"
	  " and application.argument=node_type.id and application.pos=0"
	  " and node_type.type=48 and n.mid=tuple.mid and"
	  " application.argument=tuple.id and tuple.element=n.id and"
	  " n.type=3 and application.mid=" ++ integer_to_list(MId) ++ ";"),
    DApplicationMIdIdTupleId.

%% =====================================================================
%% @spec get_deprecated_application_name_and_module_name_from_tuple_id(
%%         MId::integer(), TupleId::integer()) ->  
%%                     [{string()}]
%%
%% @doc
%% Returns all deprecated applications' module name and name.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>TupleId</b> : Id of the tuple. </pre>
%% @end
%% origin: into_db:get_applications_datas/2
%% ===================================================================== 
get_deprecated_application_name_and_module_name_from_tuple_id(MId, TupleId) ->
    ModulenameName = 
	refactor_db:select(
	  "select name from name as n,tuple as t where "
	  " n.mid=t.mid and t.mid=" ++ integer_to_list(MId) 
	  ++ " and t.id=" ++ integer_to_list(TupleId) 
	  ++ " and t.element=n.id;"),
    ModulenameName.

%% =====================================================================
%% @spec get_all_implicit_fun_module_id_id_name_id_and_type(
%%         MId::integer()) ->  
%%                     [{integer(), integer(), integer(), integer()}]
%%
%% @doc
%% Returns all implicit fun datas in module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: into_db:get_implicit_fun_datas/2
%% ===================================================================== 
get_all_implicit_fun_module_id_id_name_id_and_type(MId) ->
    ImplFunMIdIdNameIdType = 
	refactor_db:select( 
	  "select i.mid,i.id,i.name_id,type from implicit_fun as i,"
	  " node_type as n where n.mid=i.mid and i.name_id=n.id and i.mid=" 
	  ++ integer_to_list(MId) ++ ";"),
    ImplFunMIdIdNameIdType.   

%% =====================================================================
%% @spec get_simple_implicit_fun_name_and_arity(
%%         MId::integer(), NameId::integer()) ->  
%%                     [{string(), integer()}]
%%
%% @doc
%% Returns the simple implicit fun's data.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NameId</b> : Id of the name node. </pre>
%% @end
%% origin: into_db:get_implicit_fun_datas/2
%% ===================================================================== 
get_simple_implicit_fun_name_and_arity(MId, NameId) ->
    [{Name,Arity}] = 
	refactor_db:select(
	  "select n.name,i.value from arity_qualifier as a, name as n,"
	  " integer_ as i where a.mid=n.mid and n.mid=i.mid and a.body=n.id"
	  " and a.arity=i.id and a.mid=" ++ integer_to_list(MId) 
	  ++ " and a.id=" ++ integer_to_list(NameId)++ " ;"),
    {Name,list_to_integer(Arity)}.

%% =====================================================================
%% @spec get_module_implicit_fun_module_name_name_and_arity(
%%         MId::integer(), NameId::integer()) ->  
%%                     [{string(), string(), integer()}]
%%
%% @doc
%% Returns the implicit fun's data module name, fun name and arity.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NameId</b> : Id of the name node. </pre>
%% @end
%% origin: into_db:get_implicit_fun_datas/2
%% ===================================================================== 
get_module_implicit_fun_module_name_name_and_arity(MId,NameId) ->
    [{Module,Name,Arity}] =
	refactor_db:select(
	  "select n1.name,n2.name,i.value from module_qualifier as m,"
	  " name as n1, name as n2, integer_ as i, arity_qualifier as a"
	  " where m.mid=i.mid and i.mid=n1.mid and n1.mid=n2.mid"
	  " and n2.mid=a.mid and m.module=n1.id and m.body=a.id and"
	  " a.body=n2.id and a.arity=i.id and m.mid="++integer_to_list(MId)
	  ++ " and m.id=" ++ integer_to_list(NameId) ++ " ;"),
    {Module,Name,list_to_integer(Arity)}.

%%Delete

%% =====================================================================
%% @spec delete_application_argument_between_pos(
%%         MId::integer(), Id::integer(),
%%         From::integer(), To::integer()) -> ok
%%
%% @doc
%% Deletes the arguments from application between the given positions.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the element.
%% <b>From</b> : The starting position (the next will be deleted first).
%% <b>To</b> : The ending position (this will be deleted too). </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_applications_parameter/6
%% ===================================================================== 
delete_application_argument_between_pos(MId, Id, From, To) ->
    refactor_db:delete(
      "delete from application where mid=" ++ integer_to_list(MId) 
      ++ " and id=" ++ integer_to_list(Id) ++ " and pos>" 
      ++ integer_to_list(From) ++ " and pos<=" 
      ++ integer_to_list(To) ++ " ;").

%% =====================================================================
%% @spec delete_clause_argument_between_pos(
%%         MId::integer(), Id::integer(),
%%         From::integer(), To::integer()) -> ok
%%
%% @doc
%% Deletes the arguments from clause between the given positions.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the element.
%% <b>From</b> : The starting position (the next will be deleted first).
%% <b>To</b> : The ending position (this will be deleted too). </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_applications_parameter/6
%% ===================================================================== 
delete_clause_argument_between_pos(MId, Id, From, To) ->
    refactor_db:delete(
      "delete from clause where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(Id) ++ " and qualifier=0 and pos>" 
      ++ integer_to_list(From) ++ " and pos<=" ++ integer_to_list(To) ++ " ;").

%% =====================================================================
%% @spec delete_match_expr(
%%         MId::integer(), Id::integer()) -> ok
%%
%% @doc
%% Deletes the match expression.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the element. </pre>
%% @end
%% origin: refac_var_elim:remove_binding_occurrence/4
%% ===================================================================== 
delete_match_expr(MId, Id) ->
    refactor_db:delete(
      "delete from match_expr where mid=" ++ integer_to_list(MId) 
      ++ " and id=" ++ integer_to_list(Id) ++ " ;").

%% =====================================================================
%% @spec delete_element_from_pos(
%%         MId::integer(), Id::integer()) -> ok
%%
%% @doc
%% Deletes the pos of the element.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the element. </pre>
%% @end
%% origin: refac_var_elim:remove_binding_occurrence/4
%% ===================================================================== 
delete_element_from_pos(MId, Id) ->
    refactor_db:delete(
      "delete from pos where mid=" ++ integer_to_list(MId) 
      ++ " and id=" ++ integer_to_list(Id) ++ " ;").

%% =====================================================================
%% @spec delete_element_from_node_type(
%%         MId::integer(), Id::integer()) -> ok
%%
%% @doc
%% Deletes the element from node_type.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the element. </pre>
%% @end
%% origin: refac_var_elim:remove_binding_occurrence/4
%% ===================================================================== 
delete_element_from_node_type(MId, Id) ->
    refactor_db:delete(
      "delete from node_type where mid=" ++ integer_to_list(MId) 
      ++ " and id=" ++ integer_to_list(Id) ++ " ;").

%% =====================================================================
%% @spec delete_element_from_scope(
%%         MId::integer(), Id::integer()) -> ok
%%
%% @doc
%% Deletes the element from node_type.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the element. </pre>
%% @end
%% origin: refac_var_elim:remove_binding_occurrence/4
%% ===================================================================== 
delete_element_from_scope(MId, Id) ->
    refactor_db:delete(
      "delete from scope where mid=" ++ integer_to_list(MId) 
      ++ " and id=" ++ integer_to_list(Id) ++ " ;").

%% delete whole module

%% =====================================================================
%% @spec drop_from_dbase(MId::integer()) -> ok
%%
%% @doc
%% Delete one module from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: into_db:parse_database/3
%% ===================================================================== 
drop_from_dbase(MId) ->
    refactor_db:delete(
      "delete from name where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from pos where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from node_type where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from application where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from arity_qualifier where mid= " ++ integer_to_list(MId) 
      ++ ";"),
    refactor_db:delete(
      "delete from attribute_ where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from binary_ where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from binary_field where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from block_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from case_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from catch_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from char_ where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from class_qualifier where mid= " ++ integer_to_list(MId) 
      ++ ";"),
    refactor_db:delete(
      "delete from clause where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from comment where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from cond_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from conjunction where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from disjunction where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from float_ where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from form_list where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from fun_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from function where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from generator where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from if_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from implicit_fun where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from infix_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from integer_ where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from list where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from list_comp where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from macro where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from match_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from module_qualifier where mid= " ++ integer_to_list(MId) 
      ++ ";"),
    refactor_db:delete(
      "delete from parentheses where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from prefix_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from qualified_name where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from query_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from receive_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from record_access where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from record_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from record_field where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from record_index_expr where mid= " ++ integer_to_list(MId) 
      ++ ";"),
    refactor_db:delete(
      "delete from rule where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from size_qualifier where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from string where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from text where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from try_expr where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from tuple where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from precomment where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from postcomment where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from id_count where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from var_visib where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from fun_visib where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from fun_call where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from fun_call where tmid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from fun_cache where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from scope where mid= " ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete(
      "delete from scope_visib where mid= " ++ integer_to_list(MId) ++ ";").

%%Insert

%%Comment

%% =====================================================================
%% @spec insert_precomment_padding(
%%         MId::integer(), Id::integer(),
%%         Pos::integer(), Padding::integer) -> ok
%%
%% @doc
%% Inserts the padding of the precomment node.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the commented node. 
%% <b>Pos</b> : The position of the comment.
%% <b>Padding</b> : The padding of the comment text. </pre>
%% @end
%% origin: into_db:add_id/4
%% ===================================================================== 
insert_precomment_padding(MId, Id, Pos, Padding) ->
    refactor_db:insert(
      "insert into precomment (mid,id,pos,qualifier,argument) values (" 
      ++ integer_to_list(MId) ++ "," ++ integer_to_list(Id) ++"," 
      ++ integer_to_list(Pos) ++" ,0 ," ++ integer_to_list(Padding) ++ ");").

%% =====================================================================
%% @spec insert_precomment_text(
%%         MId::integer(), Id::integer(),
%%         Pos::integer(), Text::[string()]) -> ok
%%
%% @doc
%% Inserts the text of the precomment node.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the commented node. 
%% <b>Pos</b> : The position of the comment.
%% <b>Padding</b> : The comment text. </pre>
%% @end
%% origin: into_db:add_id/4
%% ===================================================================== 
insert_precomment_text(MId, Id, Pos, Text) ->
    lists:foldl(
      fun(Element, Pos2)->
	      refactor_db:insert(
		"insert into precomment (mid,id,pos,qualifier,argument) values"
		" (" ++ integer_to_list(MId) ++ "," ++ integer_to_list(Id) 
		++ "," ++ integer_to_list(Pos) ++ "," ++ integer_to_list(Pos2) 
		++ "," ++ io_lib:write_string(Element) ++ ");") 
		  ,Pos2+1 end ,1,Text). 

%% =====================================================================
%% @spec insert_postcomment_padding(
%%         MId::integer(), Id::integer(),
%%         Pos::integer(), Padding::integer) -> ok
%%
%% @doc
%% Inserts the padding of the postcomment node.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the commented node. 
%% <b>Pos</b> : The position of the comment.
%% <b>Padding</b> : The padding of the comment text. </pre>
%% @end
%% origin: into_db:add_id/4
%% ===================================================================== 
insert_postcomment_padding(MId, Id, Pos, Padding) ->
    refactor_db:insert(
      "insert into postcomment (mid,id,pos,qualifier,argument) values (" 
      ++ integer_to_list(MId) ++ "," ++ integer_to_list(Id) ++"," 
      ++ integer_to_list(Pos) ++" ,0 ," ++ integer_to_list(Padding) ++ ");").

%% =====================================================================
%% @spec insert_postcomment_text(
%%         MId::integer(), Id::integer(),
%%         Pos::integer(), Text::[string()]) -> ok
%%
%% @doc
%% Inserts the text of the postcomment node.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the commented node. 
%% <b>Pos</b> : The position of the comment.
%% <b>Padding</b> : The comment text. </pre>
%% @end
%% origin: into_db:add_id/4
%% ===================================================================== 
insert_postcomment_text(MId, Id, Pos, Text) ->
    lists:foldl(
      fun(Element, Pos2)->
	      refactor_db:insert(
		"insert into postcomment (mid,id,pos,qualifier,argument)"
		" values (" ++ integer_to_list(MId) ++ "," 
		++ integer_to_list(Id) ++ "," ++ integer_to_list(Pos) ++ "," 
		++ integer_to_list(Pos2) ++ "," ++ io_lib:write_string(Element)
		++ ");") 
		  ,Pos2+1 end ,1,Text). 

%% =====================================================================
%% @spec insert_fun_visib_data(
%%         MId::integer(), FunId::integer(),
%%         Arity::integer(), ClausesId::[integer()]) -> ok
%%
%% @doc
%% Inserts the function datas into the fun_visib table.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of the function. 
%% <b>Arity</b> : Arity of the function.
%% <b>ClausesId</b> : List of the function's clauses. </pre>
%% @end
%% origin: into_db:visib/3
%% ===================================================================== 
insert_fun_visib_data(MId, FunId, Arity, ClausesId) ->
    refactor_db:insert(
      "insert into fun_visib (mid,id,pos,argument) values (" 
      ++ integer_to_list(MId) ++ "," 
      ++ integer_to_list(FunId) ++ ",0," ++ integer_to_list(Arity) ++ ");"), 
    lists:foldl(
      fun(Element, Pos)->
	      refactor_db:insert(
		"insert into fun_visib (mid,id,pos,argument) values (" 
		++ integer_to_list(MId) ++ "," ++ integer_to_list(FunId) ++"," 
		++ integer_to_list(Pos) ++"," ++ integer_to_list(Element) 
		++ ");") ,
	      Pos+1 end ,1,ClausesId).

%% =====================================================================
%% @spec insert_scope(
%%         MId::integer(), Node::integer(),
%%         ScopeId::integer()) -> ok
%%
%% @doc
%% Inserts the scope into the scope table.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Node</b> : Id of the node. 
%% <b>ScopeId</b> : Id of the target scope. </pre>
%% @end
%% origin: into_db:put_into_scope/3
%% ===================================================================== 
insert_scope(MId, Node, ScopeId) ->
    refactor_db:insert(
      "insert into scope (mid,id,scope) values (" ++ integer_to_list(MId) 
      ++ "," ++ integer_to_list(Node) ++ "," ++ integer_to_list(ScopeId) 
      ++  " );").

%%Update

%% =====================================================================
%% @spec update_application(
%%         Pos::integer(), NewPos::integer(),
%%         ApplicationConditionList::string()) -> ok
%%
%% @doc
%% Updates the position of the application parameters.
%% 
%% Parameter description:<pre>
%% <b>Pos</b> : The original position of the parameter.
%% <b>NewPos</b> : The new position of the parameter. 
%% <b>ApplicationConditionList</b> : Condition list " (mid=" ++ 
%%        integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) ++ ")" 
%% </pre>
%% @end
%% origin: refac_reorder_funpar:change_appl_par_order/4
%% ===================================================================== 
update_application(Pos, NewPos, ApplicationConditionList) ->
    refactor_db:update(
      "update application set pos=" ++ integer_to_list(NewPos) 
      ++ " where (" ++ ApplicationConditionList ++ " ) and pos=" 
      ++ integer_to_list(Pos) ++ " ;").

%% =====================================================================
%% @spec update_clause_argument(
%%         MId::integer(), Id::integer(),
%%         NewId::integer(), From::integer()) -> ok
%%
%% @doc
%% Updates the argument of the clause.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : The id of the clause.
%% <b>NewId</b> : The new id of the argument. 
%% <b>From</b> :  The position of the changing argument. </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_clause_parameters_/6
%% ===================================================================== 
update_clause_argument(MId, Id, NewId, From) ->
    refactor_db:update(
      "update clause set argument=" ++ integer_to_list(NewId) ++ " where mid=" 
      ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
      ++ " and qualifier=0 and pos=" 
      ++ integer_to_list(From) ++ " ;").

%% =====================================================================
%% @spec update_application_argument(
%%         MId::integer(), Id::integer(),
%%         NewId::integer(), From::integer()) -> ok
%%
%% @doc
%% Updates the argument of the application.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : The id of the application.
%% <b>NewId</b> : The new id of the argument. 
%% <b>From</b> :  The position of the changing argument. </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_applications_parameter/6
%% ===================================================================== 
update_application_argument(MId, Id, NewId, From) ->
    refactor_db:update(
      "update application set argument=" ++ integer_to_list(NewId) 
      ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(Id) ++ " and pos=" 
      ++ integer_to_list(From) ++ " ;").		   

%% =====================================================================
%% @spec update_application_pos_between_given_positions(
%%         MId::integer(), Id::integer(),
%%         From::integer(), To::integer()) -> ok
%%
%% @doc
%% Updates the positions of the application between given positions.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : The id of the application.
%% <b>From</b> : The starting position of the changing arguments. 
%% <b>To</b> :  The ending position of the changing arguments. </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_applications_parameter/6
%% ===================================================================== 
update_application_pos_between_given_positions(MId, Id, From, To) ->
    refactor_db:update(
      "update application set pos=pos-" ++ integer_to_list(To-From) 
      ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(Id) ++ " and pos>" 
      ++ integer_to_list(To) ++ " ;").

%% =====================================================================
%% @spec update_clause_pos_between_given_positions(
%%         MId::integer(), Id::integer(),
%%         From::integer(), To::integer()) -> ok
%%
%% @doc
%% Updates the positions of the clause between given positions.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : The id of the clause.
%% <b>From</b> : The starting position of the changing arguments. 
%% <b>To</b> :  The ending position of the changing arguments. </pre>
%% @end
%% origin: refac_tuple_funpar:tuple_clause_parameters_/6
%% ===================================================================== 
update_clause_pos_between_given_positions(MId, Id, From, To) ->
    refactor_db:update(
      "update clause set pos=pos-" ++ integer_to_list(To-From) 
      ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(Id) ++ " and qualifier=0 and pos>" 
      ++ integer_to_list(To) ++ " ;").		   

%% =====================================================================
%% @spec update_clause_pos(
%%         Pos::integer(), NewPos::integer(),
%%         ClauseConditionList::string()) -> ok
%%
%% @doc
%% Updates the position of the clause.
%% 
%% Parameter description:<pre>
%% <b>Pos</b> : The original position of the parameter.
%% <b>NewPos</b> : The new position of the parameter. 
%% <b>ClauseConditionList</b> : Condition list " (mid=" ++ 
%%        integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) ++ ")"
%% </pre>
%% @end
%% origin: refac_reorder_funpar:change_cl_par_order/4
%% ===================================================================== 
update_clause_pos(Pos, NewPos, ClauseConditionList) ->
    refactor_db:update(
      "update clause set pos=" ++ integer_to_list(NewPos) ++ " where (" 
      ++ ClauseConditionList ++ " ) and qualifier=0 and pos=" 
      ++ integer_to_list(Pos) ++ " ;").

%% =====================================================================
%% @spec update_fun_visib(
%%         MId::integer(), Id::integer(),
%%         NewArity::integer()) -> ok
%%
%% @doc
%% Updates the functions argument in the fun_visib table.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : The id of the funtion.
%% <b>NewArity</b> :  The new arity of the function. </pre>
%% @end
%% origin: refac_tuple_funpar:refresh_export_list/5
%% ===================================================================== 
update_fun_visib(MId, Id, NewArity) ->
    refactor_db:update(
      "update fun_visib set argument=" ++ integer_to_list(NewArity) 
      ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(Id) ++ " and pos=0;").

%% =====================================================================
%% @spec update_function_name(
%%         MId::integer(), Id::integer(),
%%         Newname::string()) -> ok
%%
%% @doc
%% Updates the function name to the new name.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : The id of the name.
%% <b>Newname</b> : The new function name. </pre>
%% @end
%% origin: refac_ren_fun:rename_in_export_list/8, 
%%         refac_ren_fun:module_member_import_rename/7
%%         refac_ren_fun:rename_function_calls/5, 
%%         refac_ren_fun:rename_clauses/4
%% ===================================================================== 
update_function_name(MId, Id, Newname) ->
    refactor_db:update(
      "update name set name = " ++ io_lib:write_string(Newname) 
      ++ "where mid=" ++ integer_to_list(MId) ++ " and id = " 
      ++ integer_to_list(Id) ++ ";").

%% =====================================================================
%% @spec update_integer(
%%         MId::integer(), Id::integer(),
%%         NewValue::integer()) -> ok
%%
%% @doc
%% Updates the integer value to the new value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : The id of the element.
%% <b>NewValue</b> : The new value. </pre>
%% @end
%% origin: refac_tuple_funpar:refresh_export_list/5,
%%         refac_tuple_funpar:refresh_import_list/6
%% ===================================================================== 
update_integer(MId, Id, NewValue) ->
    refactor_db:update(
      "update integer_ set value=" ++ integer_to_list(NewValue) 
      ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(Id) ++ " ;").

%% =====================================================================
%% @spec update_variable_name(
%%         MId::integer(), Id::integer(),
%%         Newname::string()) -> ok
%%
%% @doc
%% Updates the name to the new name in every occurence of the variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the binding occurrence.
%% <b>Newname</b> : The new variable name. </pre>
%% @end
%% origin: refac_ren_var:do_the_rename/4
%% ===================================================================== 
update_variable_name(MId, Id, NewName) ->
    refactor_db:update(
      "update name set name=" ++ io_lib:write_string(NewName)++ " where mid=" 
      ++ integer_to_list(MId) ++ " and id in (select id from var_visib"
      " where mid=" ++ integer_to_list(MId) ++ " and target=" 
      ++ integer_to_list(Id)++ "); ").

%% Commit

%% =====================================================================
%% @spec commit() -> ok
%%
%% @doc
%% Execute a commit.
%% 
%% @end
%% origin: refac_ren_fun:finish_rename_function_check_for_possible_problems/1,
%%         refac_reorder_funpar:get_fun_calls/5,
%%         refac_tuple_funpar:do_the_tupling/6,
%%         into_db:parse_database/3,
%%         refac_var_elim:do_the_elimination/7,
%%         into_db:simultaneous_visiting/3
%% ===================================================================== 
commit() ->
    refactor_db:commit().


%% =====================================================================
%% @spec exists_in_dbase(
%%         File::string()) -> {bool(), integer()}
%%
%% @doc
%% Check if File is already in the database.
%% 
%% Parameter description:<pre>
%% <b>File</b> : Path of the module. </pre>
%% @end
%% origin: into_db:parse_database/3
%% ===================================================================== 
exists_in_dbase(File) ->
    MId = refactor_db:select(
	    "select mid from module where path=" ++ io_lib:write_string(File) 
	    ++  ";"),
    case (MId == []) of
	true ->
	    [{MId2}] = refactor_db:select("select max(mid) from module;"),
	    case (MId2 == null ) of
		true ->
		    refactor_db:insert(
		      "insert into module (mid, path) values (" ++ "1" 
		      ++ "," ++ io_lib:write_string(File) ++ ") "),
		    {false,1};
		false ->
		    refactor_db:insert(
		      "insert into module (mid, path) values ("
		      ++ integer_to_list(MId2+1) 
		      ++ "," ++ io_lib:write_string(File) ++ ") "),
		    {false,MId2+1}
	    end;
	false ->
	    {true,element(1,hd(MId))}
    end.

%% =====================================================================
%% @spec set_position(
%%         MId::integer(), Id::integer(),
%%         Node::integer()) -> ok
%%
%% @doc
%% Updates the position of the node.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : The id of the node.
%% <b>NewValue</b> : The AST eqvivalent of the updated node. </pre>
%% @end
%% origin: into_db:simultaneous_visiting/3
%%         into_db:add_id/4
%% ===================================================================== 
set_position(MId, Id, Node) ->
    MaybePos = erl_syntax:get_pos(Node),
    case is_tuple(MaybePos) of  
	true ->
	    {Line, Col} = MaybePos,
	    refactor_db:update(
	      "update pos set line=" ++ integer_to_list(Line) ++ ",col=" 
	      ++ integer_to_list(Col) ++ " where mid=" ++ integer_to_list(MId) 
	      ++ " and id=" ++ integer_to_list(Id) ++ ";");
	false ->
	    ok    
    end.


%% origin: into_db, used there in: into_db:put_function_calls_in_db/2

%% =====================================================================
%% @spec put_into_dbase_fun_cache(
%%    StaticFunCalls::[{integer(), integer(), string(), string(), integer()}], 
%%    FunDatas::[{integer(), integer(), string(), string(), integer()}]
%%              ) -> ok
%%
%% @doc
%% Insert fun cache data to the database.
%% 
%% Parameter description:<pre>
%% <b>StaticFunCalls</b> : Static fun calls from the examined modules.
%% <b>FunDatas</b> : Function data from the examined modules. </pre>
%% @end
%% origin: into_db:put_function_calls_in_db/2
%% ===================================================================== 
put_into_dbase_fun_cache([],[]) ->
    ok;
put_into_dbase_fun_cache([],[{MId,Id,ModuleName,FunName,Arity} | Xs]) ->
    refactor_db:insert(
      "insert into fun_cache (mid,id,module,fun,arity,type) value ("
      ++ integer_to_list(MId) ++ "," ++ integer_to_list(Id) ++ "," 
      ++ io_lib:write_string(ModuleName) 
      ++ "," ++ io_lib:write_string(FunName) ++ "," ++ integer_to_list(Arity) 
      ++ ", 0);"),
    put_into_dbase_fun_cache([], Xs);
put_into_dbase_fun_cache([{MId,Id,ModuleName,FunName,Arity} | Xs], FunDatas) ->
    refactor_db:insert(
      "insert into fun_cache (mid,id,module,fun,arity,type) value ("
      ++ integer_to_list(MId) ++ "," ++ integer_to_list(Id) ++ "," 
      ++ io_lib:write_string(ModuleName) 
      ++ "," ++ io_lib:write_string(FunName) ++ "," ++ integer_to_list(Arity) 
      ++ ", 1);"),
    put_into_dbase_fun_cache(Xs,FunDatas).


%% =====================================================================
%% @spec put_fun_calls_into_dbase1(
%%      FunctionData::{integer(), integer(), string(), string(), integer()},
%%      FunCallsData::[{integer(), integer(), string(), string(), integer()}]
%%              ) -> ok
%%
%% @doc
%% Insert fun call data to the database.
%% 
%% Parameter description:<pre>
%% <b>FunctionData</b> : The functions data which we are mathcing the calls to.
%% <b>FunCallsDatas</b> : Function calls data. </pre>
%% @end
%% origin: into_db:put_function_calls_in_db/2
%% ===================================================================== 
put_fun_calls_into_dbase1(_Data,[]) ->
    ok;
put_fun_calls_into_dbase1({MId,FunId,ModuleName,FunName,FunArity},
			  [{MId2,FunId2,ModuleName,FunName,FunArity}|Xs]) ->
    refactor_db:insert(
      "insert into fun_call (mid,id,tmid,target) value ("
      ++ integer_to_list(MId2) ++ "," ++ integer_to_list(FunId2) ++ "," 
      ++ integer_to_list(MId) ++ "," ++ integer_to_list(FunId) ++ ");" ),
    put_fun_calls_into_dbase1({MId,FunId,ModuleName,FunName,FunArity},Xs);
put_fun_calls_into_dbase1({MId,FunId,ModuleName,FunName,FunArity},[_X|Xs]) ->
    put_fun_calls_into_dbase1({MId,FunId,ModuleName,FunName,FunArity},Xs).

%% =====================================================================
%% @spec put_fun_calls_into_dbase2(
%%      FunCallData::{integer(), integer(), string(), string(), integer()},
%%      FunctionsData::[{integer(), integer(), string(), string(), integer()}]
%%              ) -> ok
%%
%% @doc
%% Insert fun call data to the database.
%% 
%% Parameter description:<pre>
%% <b>FunCallData</b> : The fun call data which we are mathcing the calls to.
%% <b>FunCallsDatas</b> : Functions  data. </pre>
%% @end
%% origin: into_db:put_function_calls_in_db/2
%% ===================================================================== 
put_fun_calls_into_dbase2(_Data,[]) ->
    ok;
put_fun_calls_into_dbase2({MId,FunId,ModuleName,FunName,FunArity},
			  [{MId2,FunId2,ModuleName,FunName,FunArity}|Xs]) ->
    refactor_db:insert(
      "insert into fun_call (mid,id,tmid,target) value ("
      ++ integer_to_list(MId) ++ "," ++ integer_to_list(FunId) ++ "," 
      ++ integer_to_list(MId2) ++ "," ++ integer_to_list(FunId2) ++ ");" ),
    put_fun_calls_into_dbase2({MId,FunId,ModuleName,FunName,FunArity},Xs);
put_fun_calls_into_dbase2({MId,FunId,ModuleName,FunName,FunArity},[_X|Xs]) ->
    put_fun_calls_into_dbase2({MId,FunId,ModuleName,FunName,FunArity},Xs).

%% =====================================================================
%% @spec get_calls_for_this_module(
%%      ModuleName::string()) -> ok
%%
%% @doc
%% Get calls to the module from the system.
%% 
%% Parameter description:<pre>
%% <b>ModuleName</b> : The module's name. </pre>
%% @end
%% origin: into_db:put_function_calls_in_db/2
%% ===================================================================== 
get_calls_for_this_module(ModuleName) ->
    AllCalls = 
	refactor_db:select(
	  "select mid,id,module,fun,arity from fun_cache where module= " 
	  ++ io_lib:write_string(ModuleName) ++ " and type=1;"),
    AllCalls.

%% =====================================================================
%% @spec get_needed_funs_from_dbase_cache(
%%      UsedModules::[integer()]
%%          ) -> [{integer(), integer(), string(), string(), integer()}]
%%
%% @doc
%% Collect the possibly needed funs for fun call matching. 
%% It is based on the used modules.
%% 
%% Parameter description:<pre>
%% <b>UsedModules</b> : Module id list. </pre>
%% @end
%% origin: into_db:put_function_calls_in_db/2
%% ===================================================================== 
get_needed_funs_from_dbase_cache(UsedModules) ->
    NeededFuns = 
	refactor_db:select(
	  "select mid,id,module,fun,arity from fun_cache where type=0"
	  ++ create_condition_list4("module=", UsedModules)),
    NeededFuns.

%% =====================================================================
%% @spec put_visib_in_database(
%%      MId::integer(),
%%      Visiblist::[{string(), integer(),[integer()]}]
%%              ) -> ok
%%
%% @doc
%% Insert variable visibity data to the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Visiblist</b> : Variable visibility data from the examined clause. </pre>
%% @end
%% origin: into_db:visib_clause/3, 
%%         into_db:visib_fun_expr_clause/5, 
%%         into_db:visib_list_comp/5
%% ===================================================================== 
put_visib_in_database(MId, Visiblist) ->
    lists:map(
      fun (Variable) -> 
	      lists:map(
		fun(Element) ->
			refactor_db:insert(
			  "insert into var_visib (mid,id,target) values (" 
			  ++ integer_to_list(MId) ++ "," 
			  ++ integer_to_list(Element) ++","
			  ++ integer_to_list(element(2,Variable)) ++ ");") end,
		element(3,Variable))end,
      Visiblist).

%% =====================================================================
%% @spec put_scope_visib_in_database(
%%      MId::integer(),
%%      Scope::[{integer(), integer()}]
%%              ) -> ok
%%
%% @doc
%% Insert scope visibility data to the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Scope</b> : Scope visibility data from the examined clause. </pre>
%% @end
%% origin: into_db:visib_clause/3
%% ===================================================================== 
put_scope_visib_in_database(MId, Scope) ->
    lists:map(
      fun ({Inner,Outer}) ->
	      refactor_db:insert(
		"insert into scope_visib (mid, id, target) values ("
		++ integer_to_list(MId) ++ "," ++ integer_to_list(Inner) 
		++ "," ++ integer_to_list(Outer) ++ ");") end, Scope).

%% =====================================================================
%% @spec get_form_list_id_from_mid(
%%      MId::integer()) -> ok
%%
%% @doc
%% Get the root of the module from the module id.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% origin: into_db:simultaneous_visiting/3
%% ===================================================================== 
get_form_list_id_from_mid(MId) ->
    [{FormListId}] = 
	refactor_db:select(
	  "select formlid from id_count where mid=" 
	  ++ integer_to_list(MId) ++ ";"),
    FormListId.



%% =====================================================================
%% @spec get_containing_scope_id(MId::integer(),
%%                               ScopeId::integer()) -> ok
%%
%% @doc
%% Get the containing scope id of a scope id.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ScopeId</b> : Id of the scope. </pre>
%% @end
%% ===================================================================== 
get_containing_scope_id(MId, ScopeId) ->
    [{OuterScope}] = 
        refactor_db:select(
	    "select target from scope_visib where" ++
	    " mid=" ++ integer_to_list(MId) ++ " and "
	    " id=" ++ integer_to_list(ScopeId) ++ " ;"),
    OuterScope.

%% =====================================================================
%% @spec get_fun_expr_clause_from_fun_id(
%%         MId::integer(), Id::integer()) -> ok
%%
%% @doc
%% Gets fun_expression clauses id from fun_expression id.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the fun_expression.
%% </pre>
%% @end
%% origin: refac_extract_fun:get_fun_expr_clauses_from_fun_ids/2
%% ===================================================================== 

get_fun_expr_clause_from_fun_id(MId, Id)->
    [{Clause }] = refactor_db:select(
			"select clause from fun_expr where mid=" 
			++ integer_to_list(MId) ++ " and id=" 
			++ integer_to_list(Id) ++ " and pos!=0;"),
    Clause.


%% =====================================================================
%% @spec update_scope(MId::integer(),Id::integer(),
%%                   OldScope::integer(), NewScope::integer()) -> ok
%% @doc
%% Update the element's scope .
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id/b> : Id.
%% <b>OldScope</b> : The old scope id.
%% <b>NewScope</b> : The new scope id.
%% </pre>
%% @end
%% =====================================================================

update_scope(MId, Id, OldScopeId, NewScopeId)->
    refactor_db:update(
      "update scope set scope=" ++ integer_to_list(NewScopeId) 
      ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(Id) ++  " and scope=" 
      ++ integer_to_list(OldScopeId) ++ ";").


%% =====================================================================
%% @spec update_var_visib(MId::integer(),VarId::integer(),
%%                   NewBindingId::integer()) -> ok
%% @doc
%% Update the variable's visibility .
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>VarId</b> : The id of the variable.
%% <b>NewBindingId</b> : The new visibility.
%% </pre>
%% @end
%% =====================================================================

update_var_visib(MId, VarId, NewBindingId)->
    refactor_db:update(
      "update var_visib set target=" ++ integer_to_list(NewBindingId) 
      ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(VarId) ++ ";").


%% =====================================================================
%% @spec update_scope_visib(MId::integer(),Id::integer(),
%%                   NewVisibId::integer()) -> ok
%% @doc
%% Update the scope's visibility .
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The scopeid.
%% <b>NewVisibId</b> : The new visibility.
%% </pre>
%% @end
%% =====================================================================

update_scope_visibility(MId, Id, NewVisibId)->
    refactor_db:update(
      "update scope_visib set target=" ++ integer_to_list(NewVisibId) 
      ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
      ++ integer_to_list(Id) ++ ";").


%% =====================================================================
%% @spec insert_var_visib(MId::integer(),Id::integer(),
%%                   Target::integer()) -> ok
%% @doc
%% Insert the variable's visibility .
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Id</b> : The id of the variable.
%% <b>Target</b> : The visibility.
%% </pre>
%% @end
%% =====================================================================

insert_var_visib(MId, Id, Target)->
    refactor_db:insert(
      "insert into var_visib (mid,id,target) values (" 
      ++ integer_to_list(MId) ++ "," 
      ++ integer_to_list(Id) ++ "," ++ integer_to_list(Target) ++ ");").


%% =====================================================================
%% @spec insert_fun_call(MId::integer(),CallId::integer(),
%%                   FunId::integer()) -> ok
%% @doc
%% Insert function call.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>CallId</b> : The id of the application.
%% <b>FunId</b> : The function.
%% </pre>
%% @end
%% =====================================================================

insert_fun_call(MId, CallId, FunId)->
    refactor_db:insert(
      "insert into fun_call (mid,id,tmid, target) values (" 
      ++ integer_to_list(MId) ++ "," 
      ++ integer_to_list(CallId) ++ "," ++ integer_to_list(MId)
      ++ "," ++ integer_to_list(FunId) ++ ");").


%% =====================================================================
%% @spec insert_fun_cache(MId::integer(),Arity::integer(), FunName::atom(),
%%                   ApplicationId::integer(), Clause::integer()) -> ok
%% @doc
%% Insert function call.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Arity</b> : The arity of the function.
%% <b>FunName</b> : The name of the function.
%% <b>ApplicationId</b> : The id of the application.
%% <b>Clause</b> : The id of the function clause.
%% </pre>
%% @end
%% =====================================================================

insert_fun_cache(MId, Arity, FunName, ApplicationId,  Clause)->
    Module = refactor:get_module_name(MId),
    refactor_db:insert(
      "insert into fun_cache(mid,id,module,fun,arity,type) values (" 
      ++ integer_to_list(MId) ++ "," 
      ++ integer_to_list(ApplicationId) ++ "," ++ io_lib:write_string(Module)
      ++ "," ++ io_lib:write_string(FunName) ++ "," 
      ++ integer_to_list(Arity) ++ ",1);"),
    refactor_db:insert(
      "insert into fun_cache(mid,id,module,fun,arity,type) values (" 
      ++ integer_to_list(MId) ++ "," 
      ++ integer_to_list(Clause) ++ "," ++ io_lib:write_string(Module)
      ++ "," ++ io_lib:write_string(FunName) ++ "," 
      ++ integer_to_list(Arity) ++ ",0);").

%% =====================================================================
%% @spec get_scope_visib(
%%         MId::integer(), Scope::integer()) -> ok
%%
%% @doc
%% Gets the scope visibility.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Scope</b> : Id of the scope.
%% </pre>
%% @end
%% origin: refac_extract_fun:get_outer_scope/2
%% ===================================================================== 

get_scope_visib(MId, Scope)->
  [{Visib}] = refactor_db:select(
	"select target from scope_visib where mid=" 
	 ++ integer_to_list(MId) ++ " and id=" 
	 ++ integer_to_list(Scope) ++ ";"),
  Visib.
