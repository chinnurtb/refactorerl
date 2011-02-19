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
%% @author Tamas Nagy <lestat@elte.hu>
%% @author Aniko Vig <viganiko@inf.elte.hu>
%% @author Melinda Toth <toth_m@inf.elte.hu>

%% @doc This is a package with various useful library functions for the
%%      refactorings. It stores all the precondition checks which are common
%%      between the refactorings.
%% @end



-module(refac_checks).


-export([error_handler/1,contains_match_expr/2,
	 check_if_binding_is_unambiguous/3, 
	 check_if_body_doesnt_have_sideffects/2, check_sideeffect/2, 
	 check_if_occurrence_needed/2, check_if_binding_occurrence_needed/3,
	 check_are_match_body_variables_shadowed/4,
         check_inner_scopes_too/3,
	 check_orderList/2, 
	 check_the_name_already_exists/3, 
         check_the_name_is_imported/3, check_isFunctionName/1,
	 check_is_autoimported/1, 
	 check_isVariableName/1, check_expression/5,
         check_true_pos/3,
	 check_pos_error/2,check_is_function_clause/1,
	 check_is_element/2, check_parameter_type/2, check_is_parameter/2,
	 check_number_error/1, check_for_length_overrun/3, check_name_clash/5,
	 check_outer_name_clash/6, check_inner_name_clash/3,
	 check_if_name_exists/3,
         check_sideeffects/2,
	 check_found_expression/1,
	 check_if_bindings_are_unambiguous/3,
	 check_send/2,
	 check_non_binding_in_match_pattern/3,
	 check_all_var_bound_ok/1,
         check_is_legal_body/8,
         check_not_in_head_pattern_guard_macro/3,
         check_is_legal_function_name/4
         ]).

-include("node_type.hrl").

%%%%%%%%%%%%%%%%%%%%% error handler

%% =====================================================================
%% @spec error_handler(Error::{term(), term()}) -> none()
%%
%% @doc
%% Handles error in refactorings. Currently it throws an exception, with
%% the given parameters. 
%% 
%% Parameter description:<pre>
%% <b>Error</b> : A two element tuple containing the type of the error,
%%                and a message to be able to make a better error message. 
%% </pre>
%% @end
%% ===================================================================== 
error_handler({ErrorType, Message}) ->
  throw( {ErrorType, Message} ).

%%%%%%%%%%%%%%%%%%%%% from var_elim




%% =====================================================================
%% @spec check_if_binding_is_unambiguous(
%%          MId::integer(), VarName::string(), Scope::integer)
%%                -> ok
%%
%% @doc
%% Checks if a variable name is bound unambiguously. If it is not, it 
%% causes an ambiguous_defining_error error.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarName</b> : Variable name to be checked.
%% <b>Scope</b> : The scope to do the search in.
%% </pre>
%% @end
%% ===================================================================== 
check_if_binding_is_unambiguous(MId, VarName, Scope) ->
  BindingOccurrenceCandidates =
    binding:get_binding_occurrence_candidates(MId, VarName, Scope),
  case BindingOccurrenceCandidates of
    {true, [_BOcc]} ->
      ok;
    _Other ->
      error_handler({ambiguous_defining_error, 0})
  end.




%% =====================================================================
%% @spec check_if_body_doesnt_have_sideffects(MId::integer(), 
%%             BodyId::integer()) -> ok
%%
%% @doc
%% Checks if a body subtree does not have sideeffects. If it has it 
%% causes a sideeffect_error error.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyId</b> : Id of a body subtree's root.
%% </pre>
%% @end
%% ===================================================================== 
check_if_body_doesnt_have_sideffects(MId,BodyId) ->
    Result=refac_common:preorder(MId,BodyId, fun check_sideeffect/2),
    SideEffects=lists:member(true, lists:flatten([Result])),
    if
	SideEffects -> error_handler( {sideffect_error,0} );
	true -> ok
    end.

%% =====================================================================
%% @spec check_sideeffect(MId::integer(), 
%%             Tree::integer()) -> bool()
%%
%% @doc
%% Checks if a node has sideeffects. If it has it returns true, otherwise 
%% false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Tree</b> : Id of a node.
%% </pre>
%% @end
%% ===================================================================== 
check_sideeffect(MId, Tree) ->
    case erl_syntax_db:type(MId, Tree) of
	?APPLICATION ->
	    true;
	?INFIX_EXPR ->
            Oper = refactor:get_infix_expr_oper_from_tree( MId, Tree),
	    case Oper == "!" of
		true ->
		    true;
		false ->
		    false
	    end;
	_ ->
	    false
    end.



%% =====================================================================
%% @spec check_if_occurrence_needed(MId::integer(), 
%%             VarId::integer()) -> bool()
%%
%% @doc
%% Checks if a varible occurrence is needed.
%% (It is not needed if by removing the occurrence the result, 
%% and the behaviour of the code does not change.) If it is it returns true, 
%% otherwise false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarId</b> : Id of a variable node.
%% </pre>
%% @end
%% ===================================================================== 
check_if_occurrence_needed(MId, VarId) ->
    refactor:not_end_of_block_expr(MId, VarId) orelse
	refactor:not_end_of_clause_body(MId, VarId) orelse
	refactor:not_end_of_receive_expr_action(MId, VarId) orelse
	refactor:not_end_of_try_expr_body(MId, VarId) orelse
	refactor:not_end_of_try_expr_after(MId, VarId) orelse
	refactor:not_end_of_try_expr_handlers(MId, VarId).



%% =====================================================================
%% @spec check_if_binding_occurrence_needed(MId::integer(), 
%%             BId::integer(), BodyId::integer()) -> bool()
%%
%% @doc
%% Checks if a the varible's binding occurrence is needed.
%% (It is not needed if by removing the occurrence the result, 
%% and the behaviour of the code does not change.) If it is it returns true, 
%% otherwise false.
%% The observed occurrence looks like this <code>BId = BodyId</code>.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : Id of a variable node's binding occurrence.
%% <b>BodyId</b> : Id of an expression's root id.
%% </pre>
%% @end
%% =====================================================================
check_if_binding_occurrence_needed(MId, BId, BodyId) ->
    MatchId = 
	refactor:get_match_expr_id_from_pattern_and_body_id(MId, BId, BodyId),
    ContBinding = contains_match_expr(MId, BodyId),	
    ContBinding and check_if_occurrence_needed(MId, MatchId).
    
%% =====================================================================
%% @spec contains_match_expr(MId::integer(), 
%%             		     BId::integer()) -> bool()
%%
%% @doc
%% Checks if the expression of the node contains match_expression.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : Id of an expression's root id.
%% </pre>
%% @end
%% =====================================================================
contains_match_expr(MId, BId) ->
    case erl_syntax_db:type(MId, BId) == ?MATCH_EXPR of
     true ->
        false;
     false ->   
	Result = refac_common:preorder(
 	       MId, BId, fun(MId2,Tree) -> 
 				    case erl_syntax_db:type(MId2, Tree) == ?MATCH_EXPR of
 					true ->
					    false;
					false ->
					    true
 				    end 
 			    end),
        lists:member(true,lists:flatten(Result))
    end.    


%%%NOTE: added check_ before name
%% =====================================================================
%% @spec check_are_match_body_variables_shadowed(MId::integer(), 
%%             BId::integer(), Scopes::[integer()], 
%%             VarIdBIdNames::[{integer(), integer(), string()}]
%%                            ) -> false | ok
%%
%% @doc
%% Checks if a variable can be substitued at every occurrence with its 
%% body (the expression which is matched to the variable).
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : Id of a variable node's binding occurrence.
%% <b>Scopes</b> : Scope ids where we search for the variable occurences.
%% <b>VarIdBIdNames</b> : 
%%            The ids, binding occurrence ids, and names of the variables.
%% </pre>
%% @end
%% =====================================================================
check_are_match_body_variables_shadowed(_MId, _BId, _Scopes, []) ->
    false;
check_are_match_body_variables_shadowed(_MId, _BId, [], _VarIdBIdNames) ->
    false;
check_are_match_body_variables_shadowed(MId, BId, Scopes, VarIdBIdNames) ->
    Shadows = 
	refactor:get_shadows_scope_and_bid_from_var_id(
	  MId, Scopes, VarIdBIdNames),
    if
	Shadows /= [] ->
	    case is_there_a_pattern_variable_there(MId, BId, Shadows) of
		{Name,Line,Col} ->
		    error_handler( 
		      {body_variable_shadowed_error, {Name,Line,Col}} );
		_ -> ok
	    end;
	true -> ok
    end.

%% =====================================================================
%% @spec is_there_a_pattern_variable_there(MId::integer(), 
%%             BId::integer(), Shadows::[{integer(), integer()}]
%%                  ) -> false | {string(), integer(), integer()}
%%
%% @doc
%% Checks if a variable is shadowed in an inner scope.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : Id of a variable node's binding occurrence.
%% <b>Shadows</b> : Scope ids and binding occurrence ids of the possible
%%                  shadowing variables.
%% </pre>
%% @end
%% =====================================================================
is_there_a_pattern_variable_there(_MId, _BId, []) ->
    false;
is_there_a_pattern_variable_there(MId, BId, [{Scope,TargetId} | Xs]) ->
    case check_inner_scopes_too(
	   MId, BId, refac_common:get_inner_scope(MId, Scope)) of
	true ->
	    refactor:get_name_and_pos_from_name_id(MId, TargetId);
	false ->
	    is_there_a_pattern_variable_there(MId, BId, Xs)
    end.

%% =====================================================================
%% @spec check_inner_scopes_too(MId::integer(), 
%%             BId::integer(), Scopes::[integer()]
%%                  ) -> bool()
%%
%% @doc
%% Checks if a variable is shadowed in an inner scope.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : Id of a variable node's binding occurrence.
%% <b>Scopes</b> : Inner scopes of the scope where the variable is. 
%% </pre>
%% @end
%% =====================================================================
check_inner_scopes_too(MId, BId, Scopes) ->
    PatternVariable = 
	refactor:get_pattern_var_id_from_var_bid_and_scope(MId, BId, Scopes),
    case PatternVariable /= [] of
	false ->
	    false;
	true ->
	    true
    end.


%%%%%%%%%%%%%%%%%%%%% from reorder_funpar

%% =====================================================================
%% @spec check_produced_list(OrderList, 
%%                  Arity::integer()) -> ok
%%           OrderList = [integer()] | bad_list
%%
%% @doc
%% Checks if the reorder list conversion was successful. 
%% It it was not it raises an error.
%%  
%% Parameter description:<pre>
%% <b>OrderList</b> : The.reorder list.
%% <b>Arity</b> : The arity of the function which we want to modify.
%% </pre>
%% @end
%% =====================================================================
check_produced_list(OrderList, Arity) ->
    case OrderList of
	bad_list ->
	    error_handler( {bad_list, {Arity}} );
	_ ->
	    ok
    end.

%% =====================================================================
%% @spec check_arity(Arity::integer()) -> ok
%%
%% @doc
%% Checks if the the reordering has sense. 
%% The function we want to modify has any parameters, 
%% which can be reordered.
%%  
%% Parameter description:<pre>
%% <b>Arity</b> : The arity of the function which we want to modify.
%% </pre>
%% @end
%% =====================================================================
check_arity(0) ->
    error_handler({no_parameters, {0}} );
check_arity(_Arity) ->
    ok.

%% =====================================================================
%% @spec check_retains_all_elements(OrderList::[integer()], 
%%                  Arity::integer()) -> ok
%%
%% @doc
%% Checks if the reorder list retains all the elements. 
%% 
%% Parameter description:<pre>
%% <b>OrderList</b> : The.reorder list.
%% </pre>
%% @end
%% =====================================================================
check_retains_all_elements(Arity, OrderList) ->
    case lists:seq(1, Arity) == lists:sort(OrderList) of
	true ->
	    ok;
	false ->
	    Length = length(OrderList),
	    case Length == Arity of
		true ->
		    error_handler( {element_error, {Length, Arity}} );
		false ->
		    error_handler( {length_error, {Length, Arity}} )
	    end
    end.

%% =====================================================================
%% @spec check_no_change(Arity::integer(), OrderList::[integer()]) -> ok
%%
%% @doc
%% Checks if the reorder list changes anything. 
%%  
%% Parameter description:<pre>
%% <b>OrderList</b> : The.reorder list.
%% </pre>
%% @end
%% =====================================================================
check_no_change(Arity, OrderList) ->
    case lists:seq(1, Arity) == OrderList of
	true ->
	    error_handler( {no_change, {0}} );
	false ->
	    ok
    end.

%% =====================================================================
%% @spec check_orderList(Order::string(), Arity::integer()) -> ok
%%
%% @doc
%% It checks if the produced list is a valid permutation of the original order.
%%
%% Parameter description:<pre>
%% <b>OrderList</b> : The new order of the function parameters.
%% <b>Arity</b> : The arity of the reordered function. </pre>
%% @end
%% =====================================================================
check_orderList(OrderList, Arity) ->
    check_produced_list(OrderList, Arity),
    check_arity(Arity),
    check_retains_all_elements(Arity, OrderList),
    check_no_change(Arity, OrderList).


%%%%%%%%%%%%%%%%%%%%% from ren_fun

%% =====================================================================
%% @spec check_the_name_already_exists(
%%        NewName::string(), Arity::integer(), 
%%        FunctionData::[{string(), integer()}]) -> ok
%%
%% @doc
%% Checks if introducing a new function would clash with the 
%% existing ones. 
%%  
%% Parameter description:<pre>
%% <b>NewName</b> : The.name of the new function.
%% <b>Arity</b> : The.arity of the new function.
%% <b>FunctionData</b> : The.existing function's data in a module. 
%%                       (name, arity)
%% </pre>
%% @end
%% =====================================================================
check_the_name_already_exists(Newname, Arity, FunctionData) ->
    NoNameClash = no_name_clash(Newname, Arity, FunctionData),
    case NoNameClash of
	false ->
	    error_handler( {exists_error,{Newname,Arity}} );
	true ->
	    ok
    end.

%% =====================================================================
%% @spec no_name_clash(
%%        NewName::string(), Arity::integer(), 
%%        FunctionData::[{string(), integer()}]) -> bool()
%%
%% @doc
%% Checks if introducing a new function would clash with the 
%% existing ones. 
%%  
%% Parameter description:<pre>
%% <b>NewName</b> : The.name of the new function.
%% <b>Arity</b> : The.arity of the new function.
%% <b>FunctionData</b> : The.existing function's data in a module. 
%%                       (name, arity)
%% </pre>
%% @end
%% =====================================================================
no_name_clash(_Name, _Arity, [])->
    true;
no_name_clash(Name, Arity, [{Name,Arity}|_Xs])->
    false;
no_name_clash(Name, Arity, [{_Module,Name,Arity}|_Xs])->
    false;
no_name_clash(Name, Arity, [_X|Xs]) ->
    no_name_clash(Name, Arity, Xs).

%% =====================================================================
%% @spec check_the_name_is_imported(
%%        NewName::string(), Arity::integer(), 
%%        Importfunctions::[{string(), integer()}]) -> ok
%%
%% @doc
%% Checks if introducing a new function would clash with the 
%% imported ones. 
%%  
%% Parameter description:<pre>
%% <b>NewName</b> : The.name of the new function.
%% <b>Arity</b> : The.arity of the new function.
%% <b>ImportFunctions</b> : The.imported function's data in a module. 
%%                       (name, arity)
%% </pre>
%% @end
%% =====================================================================
check_the_name_is_imported(Newname, Arity, ImportFunctions) ->
    NoNameClash = no_name_clash(Newname, Arity, ImportFunctions),
    if
	NoNameClash ->
	    ok;
	true ->
	    error_handler({import_error,{Newname,Arity}} )
    end.

%% =====================================================================
%% @spec check_isFunctionName(
%%        NewName::string()) -> ok
%%
%% @doc
%% Checks if the new function name is acceptable as a function name. 
%%  
%% Parameter description:<pre>
%% <b>NewName</b> : The.name of the new function.
%% </pre>
%% @end
%% =====================================================================
check_isFunctionName(Newname) ->
    if
	hd(Newname) < 97; hd(Newname) > 122 ->
	    error_handler({not_function_name_error, Newname});
	true ->
	    ok
    end.

%% =====================================================================
%% @spec check_is_autoimported(
%%        NewName::string()) -> ok
%%
%% @doc
%% Checks if the new function name is acceptable as a function name.
%% Checks if it is not a reserved word, or an autoimported function's name,
%% or a user forbidden name.
%%  
%% Parameter description:<pre>
%% <b>NewName</b> : The.name of the new function.
%% </pre>
%% @end
%% =====================================================================
check_is_autoimported(Newname) ->
    Exists = refactor:get_forbidden_name_type_from_the_name(Newname),
    if
	Exists /= [] ->
	    error_handler({name_problem_type(element(1,hd(Exists))), Newname});
	true ->
	    ok
    end.

%% =====================================================================
%% @spec name_problem_type(
%%        ProblemCode::integer()) -> atom()
%%
%% @doc
%% Resolves the rejection code to a more comprehensive form.
%%  
%% Parameter description:<pre>
%% <b>ProblemCode</b> : The three possible function name rejections code.
%% </pre>
%% @end
%% =====================================================================
name_problem_type(1) ->
    autoimported_error;
name_problem_type(2) ->
    reserved_error;
name_problem_type(3) ->
    user_denied_error.


%%%%%%%%%%%%%%%%%%%%% from ren_var

%% =====================================================================
%% @spec check_isVariableName(
%%        NewName::string()) -> ok
%%
%% @doc
%% Checks if the new Variable name is acceptable as a function name. 
%%  
%% Parameter description:<pre>
%% <b>NewName</b> : The.name of the new variable.
%% </pre>
%% @end
%% =====================================================================
check_isVariableName(Newname) ->
    if
	hd(Newname) >= 97, hd(Newname) =< 122 ->
	    error_handler( {not_variable_name_error, Newname} );
	true ->
	    if
		Newname == "" ->
		    error_handler( {not_variable_name_error, Newname} );
		true ->
		    ok
	    end
    end.

%% =====================================================================
%% @spec check_expression(MId::integer(), NewName::string(),
%%           BId::integer(), ScopeId::integer(), 
%%           Type::integer()) -> ok
%%
%% @doc
%% Checks if the new variable name is legal in the expression.
%% It does not cause name clash or name capture.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of a module.
%% <b>NewName</b> : The.name of the new variable.
%% <b>BId</b> : The.binding occurrence of the modified variable.
%% <b>ScopeId</b> : The scope of the expression.
%% <b>Type</b> : The.type of the expression.
%% </pre>
%% @end
%% =====================================================================
check_expression(MId, NewName, BId, ScopeId, Type) ->
    case name_clash_inside(MId, NewName, ScopeId) of
	false -> ok;
	Id -> 
	    error_handler(
	      {name_clash, {NewName, refactor:get_pos_from_id(MId, Id)}})
    end,
    case name_capture_inner_lower(MId, NewName, BId, ScopeId) of
	false -> ok;
	Id2 -> 
	    error_handler(
	      {name_capture, {NewName, refactor:get_pos_from_id(MId,Id2)}})
    end,
    case Type of
	?FUNCTION ->
	    case name_capture_outer_upper(MId,NewName,BId,ScopeId) of
		false -> ok;
		Id3 -> 
		    error_handler(
		      {name_capture, 
		       {NewName, refactor:get_pos_from_id(MId,Id3)}})
	    end;
	_ -> ok
    end.

%% =====================================================================
%% @spec name_clash_inside(MId::integer(), NewName::string(),
%%          ScopeId::integer()) -> integer() | false
%%
%% @doc
%% Checks if the new variable name is legal in the expression.
%% It does not cause name clash or name capture in this scope.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of a module.
%% <b>NewName</b> : The.name of the new variable.
%% <b>ScopeId</b> : The scope of the expression.
%% </pre>
%% @end
%% =====================================================================
name_clash_inside(MId, NewName, ScopeId) ->
    Id = 
	refactor:get_clashed_var_id_from_scope_and_name(MId, ScopeId, NewName),
    case Id == [] of
	true ->
	    false;
	false ->
	    element(1,hd(Id))
    end.

%% =====================================================================
%% @spec name_capture_inner_lower(MId::integer(), NewName::string(),
%%          BId::integer(), ScopeId::integer()) -> integer() | false
%%
%% @doc
%% Checks if the new variable name is legal in the expression.
%% It does not cause name clash or name capture in inner scopes.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of a module.
%% <b>NewName</b> : The.name of the new variable.
%% <b>BId</b> : The.binding occurrence of the modified variable.
%% <b>ScopeId</b> : The scope of the expression.
%% </pre>
%% @end
%% =====================================================================
name_capture_inner_lower(MId, NewName, BId, ScopeId) ->
    InnerScopes = 
	refactor:get_inner_scope_ids_from_scope_and_var_id(MId, ScopeId, BId),
    InnerCaptureList = 
	lists:map(
	  fun ({ScpId}) ->
		  case name_clash_inside(MId,NewName,ScpId) of
		      false ->
			  name_capture_inner_lower(MId,NewName,BId,ScpId);
		      Id ->
			  Id
		  end
	  end, InnerScopes),
    FilteredInnerCaptureList =
	lists:filter(fun(false) -> false; (_) -> true end, InnerCaptureList),
    case FilteredInnerCaptureList == [] of
	true ->
	    false;
	false ->
	    hd(FilteredInnerCaptureList)
    end.

%% =====================================================================
%% @spec name_capture_outer_upper(MId::integer(), NewName::string(),
%%          BId::integer(), ScopeId::integer()) -> integer() | false
%%
%% @doc
%% Checks if the new variable name is legal in the expression.
%% It does not cause name clash or name capture in outer scopes.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of a module.
%% <b>NewName</b> : The.name of the new variable.
%% <b>BId</b> : The.binding occurrence of the modified variable.
%% <b>ScopeId</b> : The scope of the expression.
%% </pre>
%% @end
%% =====================================================================
name_capture_outer_upper(MId, NewName, BId, ScopeId)->
    OuterScopes = refactor:get_outer_scope_ids_from_scope_and_var_id(
		    MId, ScopeId, BId),
    OuterCaptureList = 
	lists:map(
	  fun ({ScpId}) ->
		  case name_clash_inside(MId, NewName, ScpId) of
		      false ->
			  name_capture_outer_upper(MId, NewName, BId, ScpId);
		      Id ->
			  Id
		  end
	  end, OuterScopes),
    FilteredOuterCaptureList =
	lists:filter(fun(false) -> false; (_) -> true end, OuterCaptureList),
    case FilteredOuterCaptureList == [] of
	true ->
	    false;
	false ->
	    hd(FilteredOuterCaptureList)
    end.

%%%%%%%%%%%%%%%%%%%%% from tuple_funpar

%%added because of the change in get_true_pos_from_pointed_pos
%% =====================================================================
%% @spec check_true_pos(CLineCol::{integer(), integer()}, Line::integer(),
%%          Col::integer()) -> ok
%%
%% @doc
%% Checks if corrected position is legal. It is in the same line where 
%% the pointed is.
%%  
%% Parameter description:<pre>
%% <b>CLineCol</b> : The corrected postition.
%% <b>Line</b> : The pointed line in the code.
%% <b>Col</b> : The pointed column in the code.
%% </pre>
%% @end
%% =====================================================================
check_true_pos([], Line, Col) ->
    error_handler( {pos_error, {Line, Col}} );    
check_true_pos({CLine, _CCol}, Line, Col) ->
    if CLine /= Line ->
            error_handler({pos_error, {Line, Col}});
       true ->
            ok
    end.

%% =====================================================================
%% @spec check_pos_error(ErrorPos::{integer(), integer()}, 
%%          IdandType::[{integer(), integer()}]) -> ok
%%
%% @doc
%% Checks if on the corrected position there is a legal node. 
%% For differerent refactorings different nodes are acceptable
%%  
%% Parameter description:<pre>
%% <b>ErrorPos</b> : The corrected postition.
%% <b>IdandType</b> : The acceptable nodes on the pointed position.
%% </pre>
%% @end
%% =====================================================================
check_pos_error(IdandType, ErrorPos) ->
    case IdandType == [] of
	true ->
	    error_handler({pos_error, ErrorPos});
	false ->
	    ok
    end.

%% =====================================================================
%% @spec check_is_function_clause(IsFunctionClause::[{integer()}]) -> ok
%%
%% @doc
%% Checks if the clause on the corrected position is a function's clause. 
%%  
%% Parameter description:<pre>
%% <b>IsFunctionClause</b> : The function's id in a list.
%% </pre>
%% @end
%% =====================================================================
check_is_function_clause(IsFunctionClause) ->
    case IsFunctionClause == [] of
	true ->
	    error_handler({not_function_clause_error,0});
	false ->
	    ok
    end.

%% =====================================================================
%% @spec check_is_element(IsElement::bool(), 
%%                          ErrorPos::{integer(), integer()}) -> ok
%%
%% @doc
%% Checks if the selected element on the corrected position is part of a 
%% parameter. 
%%  
%% Parameter description:<pre>
%% <b>IsElement</b> : A boolean value containing the result of the check.
%% <b>ErrorPos</b> : The corrected postition.
%% </pre>
%% @end
%% =====================================================================
check_is_element(IsElement, ErrorPos) ->
    case IsElement == true of
	false ->
	    error_handler({not_parameter_error, ErrorPos});
	true ->
	    ok
    end.

%% =====================================================================
%% @spec check_parameter_type(Type::integer(), 
%%                          ErrorPos::{integer(), integer()}) -> ok
%%
%% @doc
%% Checks if the selected element on the corrected position is a member 
%% of a parameters. 
%%  
%% Parameter description:<pre>
%% <b>Type</b> : The type of the selected element.
%% <b>ErrorPos</b> : The corrected postition.
%% </pre>
%% @end
%% =====================================================================
check_parameter_type(Type, ErrorPos) ->
    case Type == 0 of
	true ->
	    ok;
	false ->
	    error_handler({not_parameter_error, ErrorPos})
    end.

%% =====================================================================
%% @spec check_is_parameter(PosType::[{integer(), integer()}], 
%%                          ErrorPos::{integer(), integer()}) -> ok
%%
%% @doc
%% Checks if the selected element on the corrected position is a member 
%% of the function. 
%%  
%% Parameter description:<pre>
%% <b>PosType</b> : The position and type of the selected element.
%% <b>ErrorPos</b> : The corrected postition.
%% </pre>
%% @end
%% =====================================================================
check_is_parameter(PosType, ErrorPos) ->
    case PosType == [] of
	true ->
	    error_handler({not_parameter_error, ErrorPos});
	false ->
	    ok
    end.

%% =====================================================================
%% @spec check_number_error(Text::string()) -> ok
%%
%% @doc
%% Checks if the given number is bigger then 0.
%% 
%% Parameter description:<pre>
%% <b>Text</b> : The number given as a parameter of the refactoring.
%% </pre>
%% @end
%% =====================================================================
check_number_error(Text) ->
    case list_to_integer(Text) < 1 of
	true ->
	    error_handler({number_error, Text});
	false ->
	    ok
    end.
%% =====================================================================
%% @spec check_for_length_overrun(EndPos::integer(), MaxPos::integer(), 
%%                    Length::integer()) -> ok
%%
%% @doc
%% Checks if the given number indicates tupleing non existing parameters.
%%  
%% Parameter description:<pre>
%% <b>EndPos</b> : The last position which should be tupled.
%% <b>MaxPos</b> : The existing last position.
%% <b>Length</b> : The number given as a parameter of the refactoring.
%% </pre>
%% @end
%% =====================================================================
check_for_length_overrun(EndPos, MaxPos, Length) ->
    case (EndPos =< MaxPos) of
	true ->
	    ok;
	false ->
	    error_handler({too_big_number_error,Length})
    end.

%% =====================================================================
%% @spec check_name_clash(MId::integer(), FunId::integer(),
%%             FunName::string(), OldArity::integer(), 
%%            NewArity::integer()) -> ok
%%
%% @doc
%% Checks if the change in the arity of the function would cause 
%% name clash with an existing function.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of the function.
%% <b>FunName</b> : The name of the function.
%% <b>OldArity</b> : The old arity of the function.
%% <b>NewArity</b> : The new arity of the function.
%% </pre>
%% @end
%% =====================================================================
check_name_clash(MId, FunId, FunName, OldArity, NewArity) ->
    case OldArity == NewArity of
	true ->
	    ok;
	false ->
	    UsedLocations = 
		refactor:get_module_ids_where_used_from_fun_id(MId, FunId),
	    UsedOuterLocations=lists:delete({MId}, UsedLocations),	    
	    case UsedOuterLocations == [] of
		true ->
		    check_inner_name_clash(MId,FunName,NewArity);
		false ->
		    check_outer_name_clash(
		      MId, UsedOuterLocations, FunId, FunName,
		      OldArity ,NewArity)
	    end
    end.

%% =====================================================================
%% @spec check_outer_name_clash(MId::integer(), MIds::integer(),
%%             FunId::integer(),
%%             FunName::string(), OldArity::integer(), 
%%            NewArity::integer()) -> ok
%%
%% @doc
%% Checks if the change in the arity of the function would cause 
%% name clash with an existing function in modules where the function 
%% is used.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>MIds</b> : Id of modules where the function is used.
%% <b>FunId</b> : Id of the function.
%% <b>FunName</b> : The name of the function.
%% <b>OldArity</b> : The old arity of the function.
%% <b>NewArity</b> : The new arity of the function.
%% </pre>
%% @end
%% =====================================================================
check_outer_name_clash(MId, [], _FunId, FunName, _OldArity, NewArity) ->
    check_inner_name_clash(MId, FunName, NewArity);
check_outer_name_clash(
  MId, [{MId2}|Xs], FunId, FunName, OldArity, NewArity) ->
    ImportedFunctions = 
	refactor:get_imported_functions(
	  MId2, refactor:get_import_list_ids(MId2)),
    case lists:member(
	   {refactor:get_module_name(MId), FunName, OldArity}, 
	   ImportedFunctions) of
	true ->
	    case lists:any(
		   fun({_,FN,A}) -> 
			   (FN==FunName) and (A==NewArity) 
		   end,ImportedFunctions) of
		true ->
		    error_handler(
		      {clash_imported_function_error,
		       get_error_data(
			 MId2, ImportedFunctions, FunName, NewArity)});
		false ->
		    case lists:member({FunName,NewArity}, 
				      refactor:get_functions(MId2)) of
			true ->
			    error_handler(
			      {clash_existing_function_error,
			       get_error_data(MId2,FunName,NewArity)});
			false ->
			    check_outer_name_clash(
			      MId, Xs, FunId, FunName, OldArity, NewArity)
		    end
	    end;
	false ->
	    check_outer_name_clash(
	      MId, Xs, FunId, FunName, OldArity, NewArity)
    end.

%% =====================================================================
%% @spec check_inner_name_clash(MId::integer(), FunName::string(),
%%            NewArity::integer()) -> ok
%%
%% @doc
%% Checks if the change in the arity of the function would cause 
%% name clash with an existing function in the module where the function 
%% is defined.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunName</b> : The name of the function.
%% <b>NewArity</b> : The new arity of the function.
%% </pre>
%% @end
%% =====================================================================
check_inner_name_clash(MId, FunName, NewArity) ->
    case lists:member({FunName, NewArity}, 
		      refactor:get_functions(MId)) orelse
	local_member_b({FunName,NewArity},
		       refactor:get_imported_functions(
			 MId,refactor:get_import_list_ids(MId))) of
	true ->
	    error_handler({inner_name_clash,{FunName,NewArity}});
	false ->
	    ok
    end.

%% =====================================================================
%% @spec get_error_data(MId::integer(), 
%%        FunData::[{string(), string(), integer()}],
%%        FunName::string(), NewArity::integer()) -> 
%%       {string(), {string(), string(), integer()}}
%%
%% @doc
%% Creates sufficent information for an error report on a name clash.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunData</b> : Data about functions. One of them clashes with the
%%                  checked function. 
%% <b>FunName</b> : The name of the function.
%% <b>NewArity</b> : The new arity of the function.
%% </pre>
%% @end
%% =====================================================================
get_error_data(MId, [{Module,FunName,NewArity}|_Xs], FunName, NewArity)->
    {refactor:get_module_name(MId), {Module,FunName,NewArity}};
get_error_data(MId, [_X|Xs], FunName, NewArity) ->
    get_error_data(MId, Xs, FunName, NewArity).

%% =====================================================================
%% @spec get_error_data(MId::integer(), 
%%        FunName::string(), NewArity::integer()) -> 
%%       {string(), string(), integer()}
%%
%% @doc
%% Creates sufficent information for an error report on a name clash.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunName</b> : The name of the function.
%% <b>NewArity</b> : The new arity of the function.
%% </pre>
%% @end
%% =====================================================================
get_error_data(MId, FunName, NewArity) ->
    {refactor:get_module_name(MId), FunName, NewArity}.

%% =====================================================================
%% @spec local_member_b(FunData::{string(), integer()}, 
%%        FunsData::[{string(), string(), integer()}]) -> bool()
%%       
%%
%% @doc
%% Check if the function is a member of the function data list.
%%  
%% Parameter description:<pre>
%% <b>FunData</b> : Data of the function. (name, arity)
%% <b>FunsData</b> : Data of the functions. (module name, name, arity)
%% </pre>
%% @end
%% =====================================================================
local_member_b(_FunData, []) ->
    false;
local_member_b({FunName, Arity},[{_ModuleName, FunName, Arity} | _Xs]) ->
    true;
local_member_b({FunName, Arity},[_X | Xs]) ->
    local_member_b({FunName, Arity}, Xs).


%% =====================================================================
%% @spec check_if_name_exists(MId::integer(), 
%%        Name::string(), RootClause::integer()) -> ok
%%       
%%
%% @doc
%% Check if the name is already bound in the clause.
%%  
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : Name to be checked for.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% </pre>
%% @end
%% =====================================================================
check_if_name_exists(MId, Name, RootClause) ->
    {Found, _BindingOccurrences} = binding:get_binding_occurrence_candidates(
                                     MId, Name, RootClause),
    if
        Found ->
            error_handler({ambiguous_binding, Name});
        true ->
            ok
    end.


%% =====================================================================
%% @spec check_sideeffects(ExprId::integer(), 
%%        Sideeffects::[{integer(),integer(),integer()}]) -> ok
%%       
%%
%% @doc
%% Check if the expression has sideeffects.
%%  
%% Parameter description:<pre>
%% <b>ExprId</b> : Id of the expression.
%% <b>Sideeffects</b> : Parent, id and error description of sideeffects.
%% </pre>
%% @end
%% =====================================================================
check_sideeffects(ExprId, Sideeffects) ->
  ExprSideeffects = lists:filter(fun({_, Id, _}) -> Id == ExprId end, Sideeffects),
  case ExprSideeffects of
    [] -> ok;
    [{Parent, _, Sideeffect}] ->
        error_handler({Sideeffect, Parent})
  end.



%% =====================================================================
%% @spec check_found_expression(Found::atom()) -> ok
%%       
%%
%% @doc
%% Check if an expression was found.
%%  
%% Parameter description:<pre>
%% <b>Found</b> : Description of what was found (not_found etc.).
%% </pre>
%% @end
%% =====================================================================
check_found_expression(Found) ->
  case Found of
    found_expr -> ok;
    Other ->
      error_handler({Other, []})
  end.


%% =====================================================================
%% @spec check_if_bindings_are_unambiguous(
%%          MId::integer(), BoundIds::[integer()], Scope::integer)
%%                -> ok
%%
%% @doc
%% Checks if variables are bound unambiguously. If they are not, it 
%% causes an ambiguous_defining_error error.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BoundIds</b> : Variables to be checked.
%% <b>Scope</b> : The scope to do the search in.
%% </pre>
%% @end
%% ===================================================================== 
check_if_bindings_are_unambiguous(MId, BoundIds, Scope) ->
  lists:map(fun(Id) ->
              Name = refactor:get_name_from_name_id(MId, Id),
              check_if_binding_is_unambiguous(MId, Name, Scope)
            end, BoundIds).



%% =====================================================================
%% @spec check_send(
%%          MId::integer(), ExprId::integer()
%%                -> ok
%%
%% @doc
%% Checks if the expression contains a !. If it does, it 
%% causes an in_send error.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ExprId</b> : Id of the expression.
%% </pre>
%% @end
%% ===================================================================== 
check_send(MId, ExprId) ->
  Children = refac_common:get_subtrees(MId, ExprId, without_root),
  Ops = lists:filter(fun(Id) ->
                       Type = erl_syntax_db:type(MId, Id),
                       Type == ?OPERATOR
                     end, Children),
  OpLiterals = lists:map(
                 fun(Id) -> erl_syntax_db:operator_literal(MId, Id) end, Ops),
  HasSendLiteral = lists:member("!", OpLiterals),
  if
    HasSendLiteral ->
      refac_checks:error_handler({in_send, []});
    true ->
      ok
  end.




%% =====================================================================
%% @spec check_non_binding_in_match_pattern(
%%          MId::integer(), Root::integer(), VarName::integer()
%%                -> ok
%%
%% @doc
%% Checks if the variable name occurs in a match pattern
%% as a non-binding occurrence. If it does, it 
%% causes a non_binding_pattern_occurrence error.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ExprId</b> : Id of the expression.
%% </pre>
%% @end
%% ===================================================================== 
check_non_binding_in_match_pattern(MId, Root, VarName) ->
  Children = refac_common:get_subtrees(MId, Root, without_root),
  {_,Bind} = binding:get_binding_occurrence_candidates(MId, VarName, Root),
  Ops = lists:filter(
          fun(Id) ->
            Type = erl_syntax_db:type(MId, Id),
            if
	      Type == ?MATCH_EXPR ->
	        PatternId = erl_syntax_db:match_expr_pattern(MId, Id),
	        PatternType = erl_syntax_db:type(MId, PatternId),
                if
                  PatternType == ?VARIABLE ->
                    PatternName = refactor:get_name_from_name_id(MId, PatternId),
                    Bound = lists:member(PatternId, Bind),
                    PatternName == VarName andalso not Bound;
  	          true ->
		    false
	        end;
	      true ->
	        false
	    end
          end, Children),
  case Ops of
    [] -> ok;
    _ ->
      refac_checks:error_handler({non_binding_pattern_occurrence, []})
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

check_is_legal_function_name(NewName,Arity,FunctionData,ImportFunctions) ->
    check_isFunctionName(NewName),
    check_is_autoimported(NewName),
    check_the_name_already_exists(NewName,Arity, FunctionData),
    check_the_name_is_imported(NewName, Arity, ImportFunctions).

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
%% <b>Ids</b> : Ids of the first and the last expression or NotBounVarLis
%%              id of the expression.
%% </pre>
%% @end
%% =====================================================================

check_not_in_head_pattern_guard_macro(MId, Ids, Path) ->
    case Ids of
      {_Id1, _Id2} -> ok;
      ExprId ->
	  ExprParent = hd(tl(lists:reverse(Path))),
	  ExprSideeffects =
	      refac_common:get_sideeffects_by_parent(MId, [ExprId],
						     ExprParent),
	  check_sideeffects(ExprId, ExprSideeffects)
    end.

%% =====================================================================
%% @spec check_is_legal_body(Found::atom(),FromLine::integer(),
%%             FromCol::integer(), ToLine::integer(), ToCol::integer(), 
%%             MId::integer(), Ids:: {integer(), integer()} | integer(),
%%             Path::[integer()]) -> ok
%%
%% @doc
%% Checks the starting and ending positions delimit a sequence of expression.
%%
%% Parameter description:<pre>
%% <b>Found</b> : Description of what was found (not_found etc.).
%% <b>FromLine</b> : The pointed first line number in the editor.
%% <b>FromCol</b> : The pointed first column number in the editor.
%% <b>ToLine</b> : The pointed last line number in the editor.
%% <b>ToCol</b> : The pointed last column number in the editor.
%% <b>MId</b> : The id of the module.
%% <b>Ids</b> : Ids of the first and the last expressions or 
%%              id of the expression.
%% <b>Path</b> : The path to the root of the selected sequence of expression.
%% </pre>
%% @end
%% =====================================================================

check_is_legal_body(Found,FromLine,FromCol,ToLine,ToCol,MId,Ids,Path)->
    case Found of
      found_body -> ok;
      found_expr -> ok;
      _ ->
	  error_handler({invalid_body,
			{{FromLine, FromCol}, {ToLine, ToCol}}})
    end,
    Root = case Ids of
                {_Id1, _Id2} -> lists:last(Path);
                _ExprId -> hd(tl(lists:reverse(Path)))
           end,
    case erl_syntax_db:type(MId, Root) of
      ?CASE_EXPR ->
	  error_handler({invalid_body,
		       {{FromLine, FromCol}, {ToLine, ToCol}}});
      ?IF_EXPR ->
	  error_handler({invalid_body,
		       {{FromLine, FromCol}, {ToLine, ToCol}}});
      _ -> ok
    end.

 %% =====================================================================
%% @spec check_all_var_bound_ok(List::[integer()]) -> ok
%%
%% @doc
%% Checks that all variables with binding occurence in the selected sequence 
%% of expression not appear outside of this seqence.
%%
%% Parameter description:<pre>
%% <b>List</b> : The list of the outsideused insidebounded variables.
%% </pre>
%% @end
%% =====================================================================

 check_all_var_bound_ok(List) ->
    case List of
      [] -> ok;
      _ ->
	  refac_checks:error_handler({not_all_var_bound_ok,
				      [List]})
    end.
