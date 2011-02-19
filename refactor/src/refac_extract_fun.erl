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
-module(refac_extract_fun).

-vsn('0.1').

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

extract_function(File, FromLine, FromCol, ToLine, ToCol, NewName) ->

    MId = refac_common:get_module_id(File),

    {Found, Path, Ids} = 
                 get_selected_data(MId, FromLine, FromCol, ToLine, ToCol),

    refac_checks:check_is_legal_body(Found,FromLine,FromCol,ToLine,ToCol,
                                     MId,Ids,Path),

    Root = hd(Path),
    IdList = get_idlist(MId, Root, Ids),

    refac_checks:check_not_in_head_pattern_guard_macro(MId, Ids, Path),

    VarList = get_used_variable(MId, IdList, Ids),

    VarNameList = get_namelist_from_nameid(MId, VarList),
    BoundVarList = get_bound_varlist(MId, VarList),
    BoundVarNameList = get_namelist_from_nameid(MId, BoundVarList),


    OutList = 
        get_outsideused_var_binding_data(MId, Root, VarList, BoundVarList),
    refac_checks:check_all_var_bound_ok(OutList),

    NotBoundVarListName = VarNameList -- BoundVarNameList,
    NotBoundVarListId = get_not_bound_varlist_id_from_name(MId, 
                                                         NotBoundVarListName,
                                                         Root),

    Arity = length(NotBoundVarListName),
    {_ExportListIds, FunctionData, ImportFunctions,_FunIsImportedMIds} =
	                    refac_common:get_data(MId,NewName,Arity),

    refac_checks:check_is_legal_function_name(NewName,Arity,FunctionData,ImportFunctions),
 
    perform_refactoring(MId, NewName, IdList, BoundVarList,
			NotBoundVarListName, Root , VarList, 
			NotBoundVarListId),
    {ok, done}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec get_namelist_from_nameid(MId::integer(),IdList::[integer()])
%%                                                              -> [atom()]
%% @doc
%% Returns the name from the nameid.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>IdList</b> : List of the nameids.
%% </pre>
%% @end
%% =====================================================================

get_namelist_from_nameid(MId, IdList)->
    lists:usort(lists:map(fun (Id) ->
			      refactor:get_name_from_name_id(MId, Id)
			  end,
			  IdList)).

%% =====================================================================
%% @spec get_not_bound_varlist_id_from_name(MId::integer(),
%%                                          NotBoundVarListName::[atom()],
%%                                          Root::integer())   -> [integer()]
%% @doc
%% Returns the name from the nameid.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>NotBoundVarListName</b> : List of the names.
%% <b>Root </b> : The root of the expression.
%% </pre>
%% @end
%% =====================================================================

get_not_bound_varlist_id_from_name(MId, NotBoundVarListName, Root)->
    lists:usort( lists:flatten(
      lists:map(fun (VarName) ->
		 {_Found, BId} =
		    binding:get_binding_occurrence_candidates(MId, 
							      VarName, Root),
		 hd(BId)
		end,
		NotBoundVarListName))).

%% =====================================================================
%% @spec get_selected_data(MId::integer(),
%%                         FromLine::integer(), FromCol::integer(), 
%%                         ToLine::integer(), ToCol::integer())
%%                            -> {Found::atom(),
%%                                PathFromRootClause::[integer()],
%%                                ExprId::integer()|{integer(),integer()}}
%% @doc
%% Returns the important datas of the selected sequence of expression.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>FromLine</b> : The beginning line number pointed in the editor.
%% <b>FromCol</b> : The beginning column number pointed in the editor.
%% <b>ToLine</b> : The ending line number pointed in the editor.
%% <b>ToCol</b> : The ending column number pointed in the editor.
%% </pre>
%% @end
%% =====================================================================

get_selected_data(MId, FromLine, FromCol, ToLine, ToCol )->
    {FromCLine, FromCCol} =
	refactor:get_true_pos_from_pointed_pos(MId, FromLine, FromCol),
    {ToCLine, ToCCol} =
	refactor:get_true_pos_from_pointed_pos(MId, ToLine, ToCol),
    FromId = 
      refac_common:get_lowest_id(MId,
		refactor:get_id_from_pos(MId, FromCLine,FromCCol)),
    ToId = 
      refac_common:get_lowest_id(MId,
		refactor:get_id_from_pos(MId, ToCLine, ToCCol)),
    ScopeId = refactor:get_scope_from_id(MId, FromId),

    RootClause = get_root_clause(MId, ScopeId),

    refac_common:find_expression_root_id(MId, RootClause, FromId, ToId).

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
%% @spec get_bound_varlist(MId::integer(),VarList::[integer()]) 
%%                                  -> [integer()]
%%
%% @doc
%% Gets the variables which bounded in the selected sequence of expression.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarList</b> : The ids of the used variables.
%% </pre>
%% @end
%% =====================================================================

get_bound_varlist(MId, VarList) ->
    AllBoundList = get_unique_binding(MId, VarList),
    BoundList = lists:filter(fun (Elem) ->
				     lists:member(Elem, AllBoundList)
			     end,
			     VarList),
    BoundList.


%% =====================================================================
%% @spec get_idlist(MId::integer(), Root::integer(),
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

get_idlist(MId, Root, Ids) ->
    case Ids of
      {Id1, Id2} ->
	  ClauseBody = erl_syntax_db:clause_body(MId, Root),
	  ReverseList = lists:reverse(lists:dropwhile(fun(Elem1) ->
							 not (Elem1 == Id1)
						      end,
						      ClauseBody)),
	  lists:reverse(lists:dropwhile(fun (Elem2) ->
					   not (Elem2 == Id2)
					end,
					ReverseList));
      ExprId -> [ExprId]
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

get_used_variable(MId, IdList, Ids) ->
    case Ids of
      {_Id1, _Id2} ->
	  lists:flatten(
            lists:map(fun (Id) ->
			 refac_common:get_variables(MId, Id, with_root)
		      end,
		      IdList));
      ExprId ->
	  lists:flatten(refac_common:get_variables(MId, ExprId, with_root))
    end.

%% =====================================================================
%% @spec get_outsideused_var_binding_data(MId::integer(), Root::integer(),
%%             VarList::[integer()], BoundVarList::[integer()]) -> [integer()]
%%
%% @doc
%% Get outsideused variables binding data.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Root</b> : Id of the root of the subtree which contain the
%%               selected sequence of expression.
%% <b>VarList</b> : The variables of the expressions.
%% <b>BoundVarList</b> : The ids of the bounded variables
%%                        in the selected expressions.
%% </pre>
%% @end
%% =====================================================================

 get_outsideused_var_binding_data(MId, Root, VarList, BoundVarList) ->
    All = lists:flatten(erl_syntax_db:subtrees(MId, Root)),
    AllVarList = 
        lists:flatten(
          lists:map(fun (Elem) ->
		       refac_common:get_variables(MId, Elem, with_root)
		    end,
		    All)),
    OutVarList = AllVarList -- VarList,
    OutBinding = get_unique_binding(MId,OutVarList),
    InBinding = BoundVarList,
    OutBinding -- (OutBinding -- InBinding).

%% =====================================================================
%% @spec get_unique_binding(MId::integer(),
%%                          VarList::[integer()])  -> [integer()]
%% @doc
%% Returns the name from the nameid.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>VarList</b> : List of the variable id.
%% </pre>
%% @end
%% =====================================================================
get_unique_binding(MId, VarList)->
    lists:usort( lists:flatten(
      lists:map(fun (Var) ->
                 VarName=refactor:get_name_from_name_id(MId, Var),
                 Scope = refactor:get_scope_from_id(MId, Var),
		 {_Found, BId} =
		    binding:get_binding_occurrence_candidates(MId, 
							      VarName, Scope),
		 BId
		end,
		VarList))).

%% =====================================================================
%% @spec perform_refactoring( MId::integer(), NewName::string(),
%%             IdList::[integer()], BoundVarList::[integer()],
%%             NotBoundVarListName::[atom()],Root::integer(),
%%             VarList::[integer()], NotBoundVarList::[integer()]) -> ok
%%
%% @doc
%% Performs the refactoring, and commits the changes into the
%% refactoring system.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NewName</b> : The name of the extracted function.
%% <b>IdList</b> : Ids of the selected expressions.
%% <b>BoundVarList</b> : Ids of the bounded variables.
%% <b>NotBoundVarListName</b> : Names of the not bounded variables.
%% <b>Root</b> : Id of the root of the subbtree which contain the
%%                selected sequence of expression.
%% <b>VarList</b> : Ids of the used variables.
%% <b>NotBoundVarList</b> : Ids of the not bounded variables.
%% </pre>
%% @end
%% =====================================================================

perform_refactoring(MId, NewName, IdList, BoundVarList,
		    NotBoundVarListName, Root, VarList, NotBoundVarList) ->
    UseVarList = VarList -- BoundVarList,
    NameId = create_nodes:create_atom(MId, NewName),
    create_nodes:init_scope(MId, Root, NameId),
    ApplicationNameId = create_nodes:create_atom(MId, NewName),
    create_nodes:init_scope(MId, Root, ApplicationNameId),
    NewVarList = var_id_from_name( MId, NotBoundVarListName),
    insert_new_variables_visib(MId, NewVarList),
    Id = hd(IdList),
    case length(IdList) of
      1 ->
	FunId = change_expr(MId, NewName,NameId, IdList, Id, NewVarList, 
	                    UseVarList,NotBoundVarListName, Root, 
			    ApplicationNameId,VarList, NotBoundVarList);
      _ ->
	FunId = change_seq_expr(MId,NewName,NameId,IdList,Id,NewVarList,
				UseVarList,NotBoundVarListName,Root,
				ApplicationNameId,VarList, NotBoundVarList)
    end,
    To = refactor:get_form_list_id_from_mid(MId),
    OuterScope=get_outer_scope(MId, Root),
    [{After}] = refactor:get_fun_id_from_clause_id(MId, OuterScope),
    create_nodes:attach_subtree_to_node(MId, FunId, To, After),
    refactor:commit().


%% =====================================================================
%% @spec change_expr(MId::integer(),Name::atom(),NameId::integer(),
%%         IdList::[integer()], Id::integer(), NewVarList::[integer()],
%%         UseVarList::[integer()],NotBoundVarListName::[atom()],
%%         Root::integer(),ApplicationNameId::integer(),VarList::[integer()], 
%%         NotBoundVarList::[integer()]) -> integer()
%%
%% @doc
%% Performs the changes in datebase.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : The new function's name.
%% <b>NameId</b> : The new name's id.
%% <b>IdList</b> : Ids of the selected expressions.
%% <b>Id</b> : The id of the subtree's root.
%% <b>NewVarList</b> : The ids of the new functions parameters.
%% <b>UseVarList</b> : The ids of the used variables.
%% <b>NotBoundVarListName</b> : Names of the not bounded variables.
%% <b>Root</b> : Id of the root of the subbtree which contain the
%%                selected sequence of expression.
%% <b>ApplicationNameId</b> : The new application name's id.
%% <b>VarList</b> : Ids of the used variables.
%% <b>NotBoundVarList</b> : Ids of the not bounded variables.
%% </pre>
%% @end
%% =====================================================================

change_expr(MId, Name, NameId, _IdList, Id, NewVarList, _UseVarList,
            NotBoundVarListName, Root, ApplicationNameId, 
            _VarList, NotBoundVarList) ->

    ScopeId = refactor:get_scope_from_id(MId, Id), 
    BodyId = refactor:replicate_subtree(MId, Id, ScopeId),
    Clauses = create_nodes:create_clause(MId, NewVarList, none, [BodyId]),
    insert_new_scope_and_scope_visib(MId, Clauses, Clauses),

    ReplicVarList= 
       lists:flatten(refac_common:get_variables(MId, BodyId, with_root)),
    FunId = create_nodes:create_function(MId, NameId, [Clauses]),
    Arity = length(NewVarList),

    refactor:insert_fun_visib_data(MId, FunId, Arity, [Clauses]),
    create_nodes:init_scope(MId, Clauses, NewVarList),

    update_varlist_scope(MId, ReplicVarList, Root, Clauses),
    connect_body_to_parameters(MId, ReplicVarList, NewVarList),

    delete_nodes:delete_node(MId, Id),

    Parameters = var_id_from_name(MId, NotBoundVarListName),
    connect_parameters(MId, Parameters, Root, NotBoundVarList),
    create_nodes:create_application(MId, ApplicationNameId, Parameters, Id),
    refactor:insert_scope(MId, Id, Root),
    refactor:insert_fun_call(MId, Id, FunId),
    update_expr_scope(MId, [BodyId], Root, Clauses),
    refactor:insert_fun_cache(MId, Arity, Name, Id,  Clauses),
    FunId.

%% =====================================================================
%% @spec change_seq_expr(MId::integer(),Name::atom(),NameId::integer(),
%%         IdList::[integer()], Id::integer(), NewVarList::[integer()],
%%         UseVarList::[integer()],NotBoundVarListName::[atom()],
%%         Root::integer(),ApplicationNameId::integer(),VarList::[integer()], 
%%         NotBoundVarList::[integer()]) -> integer()
%%
%% @doc
%% Performs the changes in datebase.
%%
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : The new function's name.
%% <b>NameId</b> : The new name's id.
%% <b>IdList</b> : Ids of the selected expressions.
%% <b>Id</b> : The id of the subtree's root.
%% <b>NewVarList</b> : The ids of the new functions parameters.
%% <b>UseVarList</b> : The ids of the used variables.
%% <b>NotBoundVarListName</b> : Names of the not bounded variables.
%% <b>Root</b> : Id of the root of the subbtree which contain the
%%                selected sequence of expression.
%% <b>ApplicationNameId</b> : The new application name's id.
%% <b>VarList</b> : Ids of the used variables.
%% <b>NotBoundVarList</b> : Ids of the not bounded variables.
%% </pre>
%% @end
%% =====================================================================

change_seq_expr(MId, Name,NameId, IdList, Id, NewVarList, UseVarList,
		NotBoundVarListName, Root, ApplicationNameId, 
                VarList, NotBoundVarList) ->

    Clauses = create_nodes:create_clause(MId, NewVarList, none, IdList),
    insert_new_scope_and_scope_visib(MId, Clauses, Clauses),
    FunId = create_nodes:create_function(MId, NameId,[Clauses]),
    Arity = length(NewVarList),

    refactor:insert_fun_visib_data(MId, FunId, Arity, [Clauses]),
    create_nodes:init_scope(MId, Clauses, NewVarList),
    update_varlist_scope(MId, VarList, Root, Clauses),
    connect_body_to_parameters(MId, UseVarList, NewVarList),

    Parameters = var_id_from_name(MId, NotBoundVarListName),
    connect_parameters(MId, Parameters, Root, NotBoundVarList ),
    ApplicationId = create_nodes:create_application(MId,
						    ApplicationNameId,
						    Parameters),
    refactor:insert_scope(MId, ApplicationId, Root),
    create_nodes:attach_subtree_to_node(MId, ApplicationId, Root, Id),
    lists:foreach(fun (Elem) ->
			  delete_nodes:detach_node(MId, Elem, Root)
		  end,
		  IdList),
    refactor:insert_fun_call(MId, ApplicationId, FunId),
    update_expr_scope(MId, IdList, Root, Clauses),
    refactor:insert_fun_cache(MId, Arity,Name, ApplicationId,  Clauses),
    FunId.

%% =====================================================================
%% @spec var_id_from_name(MId::integer(),
%%                        VarListName::[integer()]) -> [integer()]
%% @doc
%% Create new variables with the given names.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>VarListName</b> : List of the variable's name.
%% </pre>
%% @end
%% =====================================================================

var_id_from_name( MId, VarListName)->
    List = lists:map(fun (VarName) ->
                        create_nodes:create_variable(MId, VarName)
	             end,
	             VarListName),
    List.

%% =====================================================================
%% @spec connect_parameters(MId::integer(),Parameters::[integer()],
%%             Root::integer(), VarListId::[integer()]) -> ok
%% @doc
%% Connects the new parameters  to the binding occurence.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Parameters</b> : The id of the new function parameters.
%% <b>Root</b> : The root of the selected part.
%% <b>VarListId</b> : List of the variable's id.
%% </pre>
%% @end
%% =====================================================================

connect_parameters(MId, Parameters, Root, VarListId)->
   create_nodes:connect_variables(MId, VarListId, Parameters),
   create_nodes:init_scope(MId, Root, Parameters).

%% =====================================================================
%% @spec connect_body_to_parameters(MId::integer(),VarList::[integer()],
%%                            NewVarList::[integer()]) -> ok
%% @doc
%% Connects the selected body's variables to the new function's parameters.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>VarList</b> : Ids of the body's variables.
%% <b>NewVarList</b> : List of the new variables.
%% </pre>
%% @end
%% =====================================================================

connect_body_to_parameters( MId, VarList, NewVarList)->
     lists:foreach(fun(Id)->
		   connect_variables_to_parameter( MId, VarList, Id)
		end, 
		NewVarList).

%% =====================================================================
%% @spec connect_variables_to_parameter(MId::integer(),VarList::[integer()],
%%                            NewBindingId::integer()) -> ok
%% @doc
%% Connects the variables to the new binding.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>VarList</b> : Ids of the variables.
%% <b>NewBindindId</b> : The new binding's id.
%% </pre>
%% @end
%% =====================================================================

connect_variables_to_parameter( MId, VarList, NewBindingId)->
   BindingName = refactor:get_name_from_name_id(MId, NewBindingId),
   ConnectList = 
    lists:filter(fun(Elem)->
                    Name = refactor:get_name_from_name_id(MId, Elem),
		    if 
		      Name == BindingName 
		                    -> true;
		      true 
				    -> false
		    end
		 end,
                 VarList),

   lists:foreach(fun(VarId)->
		  refactor:update_var_visib(MId, VarId, NewBindingId)
		end, 
                ConnectList).

%% =====================================================================
%% @spec insert_new_scope_and_scope_visib(MId::integer(),Node::integer(),
%%                            ScopeId::integer()) -> ok
%% @doc
%% Insert scope and scope visibility.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Node</b> : Id.
%% <b>ScopeId</b> : The new scope id.
%% </pre>
%% @end
%% =====================================================================

insert_new_scope_and_scope_visib(MId, Node, ScopeId)->
    refactor:insert_scope(MId, Node, ScopeId),
    refactor:put_scope_visib_in_database(MId, [{Node, ScopeId}]).

%% =====================================================================
%% @spec insert_new_variables_visib(MId::integer(),
%%                            VarList::[integer()]) -> ok
%% @doc
%% Insert variables visibility.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>VarList</b> : The variables id.
%% </pre>
%% @end
%% =====================================================================

insert_new_variables_visib(MId, VarList)->
    lists:foreach(fun(VarId)->
			refactor:insert_var_visib(MId, VarId, VarId) 
		  end,
		  VarList).

%% =====================================================================
%% @spec update_varlist_scope(MId::integer(),VarList::[integer()],
%%                   Old::integer(), Clauses::integer()) -> ok
%% @doc
%% Update the variables scope.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>VarlList</b> : Id of the variables.
%% <b>Old</b> : The old scope id.
%% <b>Clauses</b> : The new scope id.
%% </pre>
%% @end
%% =====================================================================

update_varlist_scope(MId, VarList, Old, Clauses)->
    lists:foreach(fun(VarId)->
			refactor:update_scope(MId, VarId, Old, Clauses) 
		  end,
		  VarList).


%% =====================================================================
%% @spec update_expr_scope(MId::integer(),List::[integer()],
%%                   OldScope::integer(), Scope::integer()) -> ok
%% @doc
%% Update the subtree element's scope .
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>List</b> : Ids.
%% <b>OldScope</b> : The old scope id.
%% <b>Scope</b> : The new scope id.
%% </pre>
%% @end
%% =====================================================================

update_expr_scope(MId, List, OldScope, Scope)->
   IdList = 
      lists:flatten(
          lists:map(fun(Id)->
		       refac_common:get_subtrees(MId, Id, with_root)
		    end,
		    List)),
   update_idlists_scope(MId, IdList,OldScope, Scope),


   FunExprList = select_id(MId, IdList, ?FUN_EXPR),
   ScopeList = get_fun_expr_clauses_from_fun_ids(MId, FunExprList),
   update_idlists_scope_visib(MId, ScopeList, Scope),

   ListCompList = select_id(MId, IdList, ?LIST_COMP),
   update_idlists_scope_visib(MId, ListCompList, Scope).


%% =====================================================================
%% @spec select_id(MId::integer(),IdList::[integer()],
%%                   Type::atom()) -> ok
%% @doc
%% Selects the given type ids from the list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>IdList</b> : Ids.
%% <b>Type</b> : The type.
%% </pre>
%% @end
%% =====================================================================

select_id(MId, IdList, Type)->
   lists:filter(fun(Id)->
                     erl_syntax_db:type(MId, Id) == Type
		end,
                IdList).

%% =====================================================================
%% @spec update_idlists_scope(MId::integer(),IdList::[integer()],
%%                   OldScope::integer(), Scope::integer()) -> ok
%% @doc
%% Update the subtree element's scope .
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>IdList</b> : Ids.
%% <b>OldScope</b> : The old scope id.
%% <b>Scope</b> : The new scope id.
%% </pre>
%% @end
%% =====================================================================

update_idlists_scope(MId, IdList, OldScope, Scope)->
    lists:foreach(fun(Id)->
			refactor:update_scope(MId, Id, OldScope, Scope) 
		  end,
		  IdList).

%% =====================================================================
%% @spec update_idlists_scope_visib(MId::integer(),IdList::[integer()],
%%                   Scope::integer()) -> ok
%% @doc
%% Update the element's scope visibility .
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>IdList</b> : Ids.
%% <b>Scope</b> : The new scope id.
%% </pre>
%% @end
%% =====================================================================

update_idlists_scope_visib(MId, IdList, Scope)->
    lists:foreach(fun(Id)->
			refactor:update_scope_visibility(MId, Id, Scope) 
		  end,
		  IdList).  

%% =====================================================================
%% @spec get_fun_expr_clauses_from_fun_ids(MId::integer(),
%%                   List::[integer()]) -> ok
%% @doc
%% Get fun expressions clauses ids. .
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>List</b> : Fun expressions ids.
%% </pre>
%% @end
%% =====================================================================

get_fun_expr_clauses_from_fun_ids(MId, List)->  
    lists:flatten(lists:map(fun(Id)->
			        refactor:get_fun_expr_clause_from_fun_id(MId, Id) 
		            end,
		            List)).    

%% =====================================================================
%% @spec get_outer_scope(MId::integer(),
%%                       Scope::integer()) -> integer()
%% @doc
%% Gets the outer scope of the scope.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The id of the module.
%% <b>Scope</b> : Id of the scope.
%% </pre>
%% @end
%% =====================================================================

get_outer_scope(MId, Scope)->
    Id = refactor:get_scope_visib(MId, Scope),
    if
      Id == Scope ->
		     Id;
      true ->
	       get_outer_scope(MId, Id)
    end.
