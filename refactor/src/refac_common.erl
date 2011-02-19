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

%% @doc This is a package with various useful library functions for the
%%      refactorings.
%% @end



-module(refac_common).

-vsn('0.1').

-export([get_module_id/1, find_the_function/2, get_id_from_pos/4,
	 get_function_definition/2, warnings/0,
	 get_inner_scope/2, produce_inner_scopes/2,
	 get_patternIdBody/2, local_preorder_var/2, check_var/2, preorder/3,
	 get_scope_type/2,
         get_fun_calls/2, find_expression_root_id/4, get_variables/3,
         get_data/3, get_subtrees/3,
         get_clause_body_nodes/2, get_lowest_id/2,
         get_sideeffects/3, get_sideeffects_by_parent/3]).

-include("node_type.hrl").

%% =====================================================================
%% @spec get_module_id(File::string()) -> integer()
%%
%% @doc
%% Returns the module's id, if exists. Otherwise throws exception.
%% 
%% Parameter description:<pre>
%% <b>File</b> : Path of the module.</pre>
%% @end
%% ===================================================================== 
get_module_id(File) ->
    MIdWrapper = refactor:get_module_id_from_path_if_exists(File),
    if
	MIdWrapper == [] ->
	    refac_checks:error_handler( {not_exists,File} );
	true ->
	    element(1,hd(MIdWrapper))
    end.


%% =====================================================================
%% @spec find_the_function(MId::integer(), 
%%                NodeandType::[{integer(), integer()}]) -> integer()
%%
%% @doc
%% Returns the function's module id and id from the pointed usage.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NodeandType</b> : The pointed location's id and type.
%% </pre>
%% @end
%% ===================================================================== 

find_the_function(MId, [{Id, Number}]) ->
    case Number of
	?CLAUSE ->
	    [{FunId}] = refactor:get_fun_id_from_clause_id(MId, Id),
	    {MId, FunId};
	?APPLICATION ->
	    case refactor:get_fun_id_from_implicit_fun_id_or_application(MId, Id) of
                [] -> refac_checks:error_handler({out_of_scope, 0});
                [Fun] -> Fun
            end;
	?IMPLICIT_FUN ->
	    case refactor:get_fun_id_from_implicit_fun_id_or_application(MId, Id) of
                [] -> refac_checks:error_handler({out_of_scope, 0});
                [Fun] -> Fun
            end
    end.

%% =====================================================================
%% @spec get_id_from_pos(MId::integer(), 
%%                Line::integer(), Col::integer(),
%%                Type::atom()) -> integer()
%%
%% @doc
%% Returns the id of the pointed location.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Line</b> : The pointed line in the code.
%% <b>Col</b> : The pointed column in the code.
%% <b>Type</b> : The pointed element's expected type.
%% </pre>
%% @end
%% ===================================================================== 
get_id_from_pos(MId, Line, Col, function) ->
    CLineCol = refactor:get_true_pos_from_pointed_pos(MId, Line, Col),
    if 
        CLineCol == [] ->
            refac_checks:error_handler({pos_error,{Line,Col}});
        true ->
            {CLine, CCol} = CLineCol,
            if 
                CLine /= Line -> 
                    refac_checks:error_handler({pos_error,{Line,Col}});
                true ->
                    IdWrapper = 
                        refactor:get_id_and_type_list_of_functions_from_pos(
                          MId, Line, CCol),
                    if
                        IdWrapper == [] ->
                            refac_checks:error_handler({pos_error,{Line,CCol}});
                        true ->
                            IdWrapper
                    end
            end
    end;
get_id_from_pos(MId, Line, Col, variable) ->
    CLineCol = refactor:get_true_pos_from_pointed_pos(MId, Line, Col),
    if 
        CLineCol == [] ->
            refac_checks:error_handler({pos_error,{Line,Col}});
        true ->
            {CLine, CCol} = CLineCol,
            if 
                CLine /= Line -> 
                    refac_checks:error_handler({pos_error,{Line,Col}});
                true ->
                    IdWrapper = 
                        refactor:get_var_id_from_pos(
                          MId, Line, CCol),
                    if
                        IdWrapper == [] ->
                            refac_checks:error_handler({pos_error,{Line,CCol}});
                        true ->
                            element(1, hd(IdWrapper))
                    end
            end
    end.
   

%% =====================================================================
%% @spec get_function_definition(MId::integer(), 
%%                NodeandType::[{integer(), integer()}]) -> integer()
%%
%% @doc
%% Returns the function's module id and id from the pointed usage.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NodeandType</b> : The pointed location's id and type.
%% </pre>
%% @end
%% ===================================================================== 
get_function_definition(MId, [{Id,IdType}]) ->
    case IdType of
	?CLAUSE ->
	    [{FunId}] = refactor:get_fun_id_from_clause_id(MId, Id),
	    {MId, FunId};
	?APPLICATION ->
	    refactor:get_fun_id_from_implicit_fun_id_or_application(MId, Id);
	?IMPLICIT_FUN ->
	    refactor:get_fun_id_from_implicit_fun_id_or_application(MId, Id)
    end.

%% =====================================================================
%% @spec warnings() -> ok
%%
%% @doc
%% Checks for unsupported function calls, which can cause problems after 
%% a refactoring.
%% 
%% @end
%% ===================================================================== 
warnings() ->
  warn_for_dynamic(),
  warn_for_spawn(),
  warn_for_apply(),
  warn_for_hibernate().

%% =====================================================================
%% @spec warn_for_apply() -> ok
%%
%% @doc
%% Checks for unsupported apply function call, 
%% which can cause problems after a refactoring.
%% 
%% @end
%% ===================================================================== 
warn_for_apply() ->
    case refactor:check_if_apply_exists_in_system() of
        [] -> ok;
        _ -> refac_checks:error_handler({warning, apply})
    end.

%% =====================================================================
%% @spec warn_for_spawn() -> ok
%%
%% @doc
%% Checks for unsupported spawn function call, 
%% which can cause problems after a refactoring.
%% 
%% @end
%% ===================================================================== 
warn_for_spawn() ->
    case refactor:check_if_spawn_exists_in_system() of
        [] -> ok;
        _ -> refac_checks:error_handler({warning, spawn})
    end.

%% =====================================================================
%% @spec warn_for_hibernate() -> ok
%%
%% @doc
%% Checks for unsupported hibernate function call, 
%% which can cause problems after a refactoring.
%% 
%% @end
%% ===================================================================== 
warn_for_hibernate() ->
    case refactor:check_if_hibernate_exists_in_system() of
        [] -> ok;
        _ -> refac_checks:error_handler({warning, hibernate})
    end.

%% =====================================================================
%% @spec warn_for_dynamic() -> ok
%%
%% @doc
%% Checks for unsupported dynamic function call, 
%% which can cause problems after a refactoring.
%% 
%% @end
%% ===================================================================== 
warn_for_dynamic() ->
    DynamicFunCall = refactor:get_all_dynamic_fun_call_type(),
    if
	DynamicFunCall /= [] ->
	    refac_checks:error_handler({warning, dynamic});
	true -> ok
    end.

%% =====================================================================
%% @spec get_inner_scope(MId::integer(), 
%%                ScopeId::integer()) -> [integer()]
%%
%% @doc
%% Returns the ScopeId scope's inner scopes.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ScopeId</b> : A scope in a funtion.
%% </pre>
%% @end
%% ===================================================================== 
get_inner_scope(MId, ScopeId) ->
    produce_inner_scopes(MId,[{ScopeId}]).

%% =====================================================================
%% @spec produce_inner_scopes(MId::integer(), 
%%                ScopeIds::[{integer()}]) -> [integer()]
%%
%% @doc
%% Returns the ScopeIds scopes' inner scopes. Populates the scope list 
%% until it does not change.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ScopeIds</b> : Scopes in a funtion.
%% </pre>
%% @end
%% ===================================================================== 
produce_inner_scopes(_MId, []) ->
    [];
produce_inner_scopes(MId, [ {ScopeId}| Xs]) ->
    Scopes = refactor:get_inner_scope_ids_from_scope_id(MId, ScopeId),
    case Scopes == [{ScopeId}] of
	true ->
	    [ScopeId | produce_inner_scopes(MId, Xs)];
	false->
	    [ScopeId | produce_inner_scopes(
			 MId, lists:delete({ScopeId},Scopes) ++ Xs)]
    end.

%% =====================================================================
%% @spec get_patternIdBody(MId::integer(), 
%%                BId::integer()) -> integer()
%%
%% @doc
%% Returns the expression's root which result will be the variables value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BId</b> : Binding occurrance of a variable.
%% </pre>
%% @end
%% ===================================================================== 
get_patternIdBody(MId, BId) ->
    PatternIdBody = refactor:get_match_expr_id_and_body_from_pattern(MId, BId),
    if
	PatternIdBody == [] ->
	    refac_checks:error_handler(
	      {bad_binding, refactor:get_pos_from_id(MId,BId)});
	true ->
	    element(2,hd(PatternIdBody))
    end.

%% =====================================================================
%% @spec local_preorder_var(MId::integer(), 
%%                BodyId::integer()) -> integer()
%%
%% @doc
%% Returns the given subtree's variables.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyId</b> : The root of a subtree.
%% </pre>
%% @end
%% ===================================================================== 
local_preorder_var(MId, BodyId) ->
    Result = preorder(MId,BodyId,fun check_var/2),
    lists:filter(fun (ok) -> false;
		     (_) -> true end,lists:flatten([Result])).

%% =====================================================================
%% @spec check_var(MId::integer(), 
%%                Tree::integer()) -> 
%%                       {integer(), integer(), string()} | ok
%%
%% @doc
%% Returns the node's data if it's a variable. Otherwise ok.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Tree</b> : Id of a node.
%% </pre>
%% @end
%% ===================================================================== 
check_var(MId, Tree) ->
    case erl_syntax_db:type(MId,Tree) == ?VARIABLE of
	true ->
        Name = refactor:get_name_from_name_id(MId, Tree),
        BId = refactor:get_var_bind_occ_from_id(MId, Tree),
        {Tree, BId, Name};
	false ->
        ok
    end.



%% =====================================================================
%% @spec preorder(MId::integer(), 
%%                Tree::integer(), F::function()) -> 
%%                       [term()]
%%
%% @doc
%% Traverses the Tree in preorder mode and 
%% calls F function for all elements in the tree.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Tree</b> : Id of a node.
%% <b>F</b> : A function which will be called for every node in the tree.
%% </pre>
%% @end
%% ===================================================================== 
preorder(MId, Tree, F)->
    case erl_syntax_db:subtrees(MId, Tree) of
	[] ->
	    [F(MId,Tree)];
	List ->
	    lists:map(
	      fun(Elements)->
		      lists:map(
			fun(Element)->
				[F(MId, Element) | preorder(MId,Element,F)]
			end, Elements)
	      end, List)
    end.

%% =====================================================================
%% @spec get_scope_type(MId::integer(), 
%%                ScopeId::integer()) -> 
%%                       integer()
%%
%% @doc
%% Returns the scopes type.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ScopeId</b> : A scope in a function.
%% </pre>
%% @end
%% ===================================================================== 
get_scope_type(MId, ScopeId) ->
    Type = erl_syntax_db:type(MId, ScopeId),
    case Type of
	?LIST_COMP ->
	    ?LIST_COMP;
	?CLAUSE ->
            FunctionId = refactor:get_fun_id_from_clause_id(MId, ScopeId),
	    case FunctionId == [] of
		true ->
		    ?FUN_EXPR;
		false ->
                    ?FUNCTION
	    end
    end.



%%%%%%%%%%%%%% from ren_fun, also used in reorder_funpar

%% =====================================================================
%% @spec get_fun_calls(MId::integer(), 
%%                FunId::integer()) -> 
%%              {CallData, CallData, CallData, CallData, CallData}
%%          CallData = [{integer(), integer(), integer()}]
%%
%% @doc
%% Returns the calls of a function seperated into list by type.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of a function.
%% </pre>
%% @end
%% ===================================================================== 
get_fun_calls(MId, FunId) ->
    CallIds = refactor:get_every_fun_call_and_type_from_fun_id(MId, FunId),
    ApplicationIds = filter_by_type( ?APPLICATION, CallIds ),
    ImplicitFunCallIds = filter_by_type( ?IMPLICIT_FUN, CallIds ),


    ApplicationTypeandNameIds =
	lists:map(
	  fun ({MId2, Id,_Type}) ->
		  refactor:get_application_type_mid_id_from_fun_call_id(
		    MId2, Id)
	  end, ApplicationIds ),
    ImplicitFunCallTypeandNameIds =
	lists:map(
	  fun ({MId2, Id,_Type}) ->
		  refactor:get_implicit_fun_call_type_mid_id_from_fun_call_id(
		    MId2, Id)
	  end, ImplicitFunCallIds ),
    ImplicitFunCallandTypeIds =
	lists:map(
	  fun ({MId3,Id,_Type}) ->
		  Type =
		      refactor:get_implicit_fun_call_type_from_implicit_fun_id(
			MId3, Id),
		  {Type,MId3,Id}
	  end, ImplicitFunCallIds),

    {ApplicationIds, ApplicationTypeandNameIds,
     ImplicitFunCallIds, ImplicitFunCallandTypeIds,
     ImplicitFunCallTypeandNameIds}.

%% =====================================================================
%% @spec filter_by_type(Type::integer(), 
%%          CallIdandTypeList::[{integer(), integer(), integer()}]) -> 
%%                       integer()
%%
%% @doc
%% Filter the elements out from the list which type is not Type.
%% 
%% Parameter description:<pre>
%% <b>Type</b> : A node type.
%% <b>CallIdandTypeList</b> : A list of nodes. (module id, id, type)
%% </pre>
%% @end
%% ===================================================================== 
filter_by_type( Type, CallIdandTypeList ) ->
    lists:filter( fun ( {_MId,_Id, Type2} ) ->
                          Type == Type2
                  end, CallIdandTypeList).


%% =====================================================================
%% @spec find_expression_root_id(MId::integer(), NodeId::integer(),
%%                               FromId::integer(), ToId::integer()) ->
%%                                   {Found, PathFromRootClause, ExprId} 
%%                            Found = atom()
%%               PathFromRootClause = [integer()]
%%                           ExprId = integer()|{integer(),integer()}
%%
%% @doc
%% Returns the root of the expression and the path to it from the root.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NodeId</b> : Id of the actual node.
%% <b>FromId</b> : Id to search from.
%% <b>ToId</b> : Id to search to.</pre>
%% Result description:<pre>
%% <b>Found</b> : Result of the search.
%%                Possible values: not_found, found_expr, found_body,
%%                                 found_first, found_last.
%% <b>PathFromRootClause</b> : Ids from the root to the expression.
%% <b>ExprId</b> : Id of the expression, 
%%                 or the first and last id of the expressions. </pre>
%% @end
%% ===================================================================== 
find_expression_root_id(MId, NodeId, FromId, ToId) ->
  Children = erl_syntax_db:subtrees(MId,NodeId),
  if
    FromId == ToId andalso FromId == NodeId ->
      {found_expr, [NodeId], NodeId};
    NodeId == FromId ->
      {Found, _, _} = find_expression_root_id(MId, NodeId, 0, ToId),
      if Found == found_last ->
        {found_expr, [NodeId], NodeId};
      true ->
        {found_first, not_used, not_used}
      end;
    NodeId == ToId ->
      {Found, _, _} = find_expression_root_id(MId, NodeId, FromId, 0),
      if Found == found_first ->
        {found_expr, [NodeId], NodeId};
      true ->
        {found_last, not_used, not_used}
      end;
    Children == [] ->
      if
        NodeId == FromId ->
          {found_first, not_used, not_used};
        NodeId == ToId ->
          {found_last, not_used, not_used};
	true ->
          {not_found, not_used, not_used}
      end;
    true ->
      ChildrenList = lists:flatten(Children),

      FirstChild = hd(ChildrenList),
      LastChild = lists:last(ChildrenList),
      {FirstState, _FirstPath, _FirstId} = find_expression_root_id(
                                MId, FirstChild, FromId, ToId),
      {LastState, _LastPath, _LastId} = find_expression_root_id(
                                MId, LastChild, FromId, ToId),
      NodeType = erl_syntax_db:type(MId, NodeId),

      if
        FirstChild == FromId andalso
        LastChild == ToId andalso
        FromId /= ToId ->
          {found_expr, [NodeId], NodeId};
        NodeId == FromId andalso LastState == found_last ->
          {found_expr, [NodeId], NodeId};
        FirstState == FromId andalso NodeType /= ?CLAUSE ->
          if
            LastState == found_last ->
              {found_expr, [NodeId], NodeId};
            FromId == ToId ->
              {found_expr, [NodeId,FirstChild], FirstChild};
            true ->
              {found_first, not_used, not_used}
          end;
        LastState == ToId andalso NodeType /= ?CLAUSE ->
          if
            FirstState == found_first ->
              {found_expr, [NodeId], NodeId};
            FromId == ToId ->
              {found_expr, [NodeId,LastChild], LastChild};
            true ->
              {found_last, not_used, not_used}
          end;
        true ->
          SearchInChildren = lists:map( fun(ChildNodeId) ->
                                          {ChildNodeId,
                                           find_expression_root_id(
                                            MId, ChildNodeId, FromId, ToId)}
                                        end, ChildrenList ),
          Found = lists:filter( fun ({_CId, {not_found, _Path, _Id}}) -> false;
                                    ( _Other ) -> true
                                end, SearchInChildren ),
          if
            NodeType == ?CLAUSE ->
              case Found of
                [{Id1, {found_first, _, _}}, {Id2, {found_last, _, _}}] ->
                  {found_body, [NodeId], {Id1, Id2}};
                [{_ActualId, {found_expr, Path, ExprId}}] ->
                  {found_expr, [NodeId] ++ Path, ExprId};
                [{Id, Info={Pos,_,_}}] ->
		  find_helper(Pos, Id, Info, FirstChild, LastChild);
                _Other ->
                  {not_found, not_used, not_used}
              end;
            true ->
              case Found of
                [] ->
                  {not_found, not_used, not_used};
                [{_Id1, {found_first, _, _}}, {_Id2, {found_last, _, _}}] ->
                  {found_expr, [NodeId], NodeId};
                [{_ActualId, {found_expr, Path, ExprId}}] ->
                  {found_expr, [NodeId] ++ Path, ExprId};
                [{_ActualId, {found_body, Path, ExprId}}] ->
                  {found_body, [NodeId] ++ Path, ExprId};
                [{Id, Info={Pos,_,_}}] ->
		  find_helper(Pos, Id, Info, FirstChild, LastChild);
                _Other ->
                  {not_found, not_used, not_used}
              end
          end
      end
  end.


%% =====================================================================
%% @spec find_helper(Pos::atom(), Id::integer(),
%%                   Info::{atom(),atom(),atom()},
%%                   FirstChild::integer(), LastChild::integer())
%%                      -> {atom(),atom(),atom()}
%%
%% @doc
%% A common case in the search for the expression root.
%% 
%% Parameter description:<pre>
%% <b>Pos</b> : The position found below the node (found_expr etc.).
%% <b>Id</b> : Id of the actual node.
%% <b>Info</b> : Result information when appropriate.
%% <b>FirstChild</b> : Id of the first child node.
%% <b>LastChild</b> : Id of the last child node.</pre>
%% @end
%% ===================================================================== 
find_helper(found_first, Id, Info, Id, _) -> Info;
find_helper(found_last, Id, Info, _, Id) -> Info;
find_helper(found_expr, _, Info, _, _) -> Info;
find_helper(found_body, _, Info, _, _) -> Info;
find_helper(_, _, _, _, _) -> {not_found, not_used, not_used}.


%% =====================================================================
%% @spec get_variables(MId::integer(), NodeId::integer(),
%%                     WithRoot::atom()) -> [integer()]
%%
%% @doc
%% Returns the variables below the node and the node itself.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NodeId</b> : Id of the actual node.
%% <b>WithRoot</b> : Include the root node (with_root) or not (without_root).
%% </pre>
%% @end
%% ===================================================================== 
get_variables(MId, NodeId, WithRoot) ->
  Subtrees = get_subtrees(MId, NodeId, WithRoot),
  lists:filter(fun(SubId) ->
                 case erl_syntax_db:type(MId, SubId) of
                   ?VARIABLE -> true;
                   _Other    -> false
                 end
               end, Subtrees).

%% =====================================================================
%% @spec get_subtrees(MId::integer(), NodeId::integer(),
%%                    WithRoot::atom()) -> [integer()]
%%  
%% @doc
%% Returns all the nodes below the node and the node itself.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NodeId</b> : Id of the actual node.
%% <b>WithRoot</b> : Include the root node (with_root) or not (without_root).
%% </pre>
%% @end
%% ===================================================================== 
get_subtrees(MId, NodeId, WithRoot) ->
  AllChildren = lists:flatten(erl_syntax_db:subtrees(MId, NodeId)),
  AdditionalNode = case WithRoot of
                     with_root ->
                       [NodeId];
                     without_root ->
                       []
                   end,
  lists:flatten(AdditionalNode ++ AllChildren ++
                lists:map( fun(ChildId) ->
                              get_subtrees(MId, ChildId, without_root)
                            end, AllChildren)).






%% =====================================================================
%% @spec get_data(
%%            MId::integer(), FunName::string(),
%%          Arity::integer()) -> 
%%          {[integer()], [{string(), integer()}], 
%%                [{string(), string(), integer()}], [integer()]}
%%
%% @doc
%% Returns data about the function, and about the surrounding functions.   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunName</b> : The name of the function.
%% <b>Arity</b> : The arity of the function.
%% </pre>
%% @end
%% ===================================================================== 

get_data(MId, FunName, Arity) ->
    ExportListIds = refactor:get_export_list_id(MId),
    ExportList = refactor:get_arity_qualifier_data(MId, ExportListIds),
    case refactor:simple_member_b(FunName,Arity,ExportList) or 
	refactor:is_compile_export_all(MId) of
	true ->
	    get_data_from_imported_too(MId, FunName, Arity, ExportListIds);
	false ->
	    get_data(MId, ExportListIds)
    end.

%% =====================================================================
%% @spec get_data_from_imported_too(
%%            MId::integer(), FunName::string(),
%%          Arity::integer(), ExportListIds::[integer()]) -> 
%%          {[integer()], [{string(), integer()}], 
%%                [{string(), string(), integer()}], [integer()]}
%%
%% @doc
%% Returns data about the function, and about the surrounding functions.   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunName</b> : The name of the function.
%% <b>Arity</b> : The arity of the function.
%% <b>ExportListIds</b> : The export list ids of the module.
%% </pre>
%% @end
%% ===================================================================== 
    get_data_from_imported_too(MId, FunName, Arity, ExportListIds) ->
    {ExportListIds, FunctionData, ImportFunctions,[]} = 
	get_data(MId, ExportListIds),
    MIds = refactor:get_every_mid(),
    FunIsImportedMIds = 
	lists:filter(
	  fun({MId2}) ->
		  ImportListIds = refactor:get_import_list_ids(MId2),
		  ImportFunctions2 = 
		      refactor:get_imported_functions(MId2, ImportListIds),
		  module_member_b(
		    refactor:get_module_name(MId), FunName,
		    Arity, ImportFunctions2)
	  end,
	  MIds),
    ImportFunModuleDatas = 
	lists:map(
	  fun({MId2}) -> get_data(MId2, ExportListIds) end, FunIsImportedMIds),
    add_to_module_data({ExportListIds, FunctionData, ImportFunctions}, 
		       ImportFunModuleDatas, FunIsImportedMIds).

%% =====================================================================
%% @spec add_to_module_data(
%%            ModuleData::{ExportListIds, FunctionData, ImportFunctions},
%%            ModuleDatas::[{ExportListIds, FunctionData, ImportFunctions}],
%%            FunIsImportedMIds::[integer()]) ->
%%          {ExportListIds, FunctionData, ImportFunctions, [integer()]}
%%
%%          ExportListIds = [integer()]
%%          FunctionData = [{string(), integer()}]
%%          ImportFunctions = [{string(), string(), integer()}]
%%
%% @doc
%% Merge together different modules' data.   
%% 
%% Parameter description:<pre>
%% <b>ModuleData</b> : Data about the module concerning the renamed function..
%% <b>ModuleDatas</b> : Data about the modules concerning the renamed function..
%% <b>FunIsImportedMIds</b> : 
%%               MId of modules where the renamed function. is imported.
%% </pre>
%% @end
%% =====================================================================  
add_to_module_data({ExportListIds, FunctionData, ImportFunctions}, [], 
		   FunIsImportedMIds) ->
    {ExportListIds, FunctionData, ImportFunctions, FunIsImportedMIds};
add_to_module_data({ExportListIds, FunctionData, ImportFunctions}, 
		   [{ExportListIds2, FunctionData2, ImportFunctions2,[]}|Xs], 
		   FunIsImportedMIds) ->
    add_to_module_data(
      {ExportListIds ++ ExportListIds2, FunctionData ++ FunctionData2, 
       ImportFunctions ++ ImportFunctions2}, 
      Xs, FunIsImportedMIds).

%% =====================================================================
%% @spec get_data(
%%            MId::integer(), ExportListIds::[integer()]) -> 
%%          {[integer()], [{string(), integer()}], 
%%                [{string(), string(), integer()}], [integer()]}
%%
%% @doc
%% Returns data about the function, and about the surrounding functions.   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ExportListIds</b> : The export list ids of the module.
%% </pre>
%% @end
%% ===================================================================== 
get_data(MId, ExportListIds) -> 
    FunIds = refactor:get_all_function_id_from_the_module(MId),
    FunnameIds = 
	lists:map(
	  fun({Element})-> 
		  refactor:get_fun_name_id_from_fun_id(MId, Element) end,
	  FunIds),
    Funnames = 
	lists:map(
	  fun(Element)-> 
		  refactor:get_name_from_name_id(MId, Element) end,
	  FunnameIds),
    Arities = refactor:get_all_arity_from_the_module(MId),
    ImportListIds = refactor:get_import_list_ids(MId),
    ImportFunctions = refactor:get_imported_functions(MId, ImportListIds),
    {ExportListIds, lists:zipwith(fun(X, {Y}) ->{X, Y} end, Funnames, Arities),
     ImportFunctions, []}.


%% =====================================================================
%% @spec module_member_b(
%%         Module::string(), FunName::string(),
%%         Arity::integer(), List::[{string(), string(), integer()}]) -> 
%%                    bool()
%%
%% @doc
%% Check if the searched function is a member of the list.   
%% 
%% Parameter description:<pre>
%% <b>Module</b> : The name of the module.
%% <b>FunName</b> : The name of the function.
%% <b>Arity</b> : The arity of the function.
%% <b>ExportListIds</b> : The export list ids of the module.
%% </pre>
%% @end
%% ===================================================================== 
module_member_b(_Module, _FunName, _Arity, []) ->
    false;
module_member_b(Module, FunName, Arity, [{Module,FunName,Arity}|_Xs]) ->
    true;
module_member_b(Module, FunName, Arity, [_X|Xs]) ->
    module_member_b(Module, FunName, Arity, Xs).



%% =====================================================================
%% @spec get_clause_body_nodes(MId::integer(), RootClause::integer()) -> 
%%                    [integer()]
%%
%% @doc
%% Returns all of the nodes in the body of the clause.   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% </pre>
%% @end
%% ===================================================================== 
get_clause_body_nodes(MId, RootClause) ->
  RootClauseBody = erl_syntax_db:clause_body(MId, RootClause),
  lists:foldl(fun(BodyId, Subtr) ->
                Ids = refac_common:get_subtrees(MId, BodyId, with_root),
                Subtr ++ Ids
              end, [], RootClauseBody).



%% =====================================================================
%% @spec get_lowest_id(MId::integer(), [Ids::integer()]) -> 
%%                    integer()
%%
%% @doc
%% Returns the leaf node in the syntax tree from the listed ones.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Ids</b> : Ids that are present in the syntax tree.
%% </pre>
%% @end
%% ===================================================================== 
get_lowest_id(_MId, [{Id}]) -> Id;
get_lowest_id(MId, [{Id}|Ids]) ->
  Subtrees = get_subtrees(MId, Id, without_root),
  Contains = lists:filter(fun({Id2}) -> lists:member(Id2, Subtrees) end, Ids),
  case Contains of
    [] -> Id;
    ContainedIds -> get_lowest_id(MId, ContainedIds)
  end.



%% =====================================================================
%% @spec get_sideeffects(MId::integer(), RootClause::integer(), 
%%                       ExprOccurrences::[integer()])
%%                         -> [{Parent, Id, ErrorMessage}]
%%                   Parent = integer()
%%                       Id = integer()
%%             ErrorMessage = atom()
%%       
%%
%% @doc
%% Returns which occurrences have sideeffects, which are inside a 
%%  list head, list body, generator pattern, clause guard, clause
%%   pattern, macro.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>RootClause</b> : Id of the clause containing the expressions.
%% <b>ExprOccurrences</b> : List of all the expression occurrences.
%% </pre>
%% @end
%% =====================================================================
get_sideeffects(MId, RootClause, ExprOccurrences) ->
  AllNodes = refac_common:get_subtrees(MId, RootClause, with_root),
  lists:flatten(
    lists:map(
      fun(Parent) ->
        get_sideeffects_by_parent(MId, ExprOccurrences, Parent)
      end, AllNodes)).

get_sideeffects_by_parent(MId, ExprOccurrences, Parent) ->
  CheckThese =
    [{?LIST_COMP, fun erl_syntax_db:list_comp_template/2, in_list_head}
    ,{?LIST_COMP, fun erl_syntax_db:list_comp_body/2, in_list_body}
    ,{?GENERATOR, fun erl_syntax_db:generator_pattern/2, in_generator_pattern}
    ,{?CONJUNCTION, fun(MId2, Node) -> refac_common:get_subtrees(MId2, Node, with_root) end, in_clause_guard}
    ,{?DISJUNCTION, fun(MId2, Node) -> refac_common:get_subtrees(MId2, Node, with_root) end, in_clause_guard}
    ,{?CLAUSE, fun erl_syntax_db:clause_patterns/2, in_clause_pattern}
    ,{?MACRO, fun(MId2, Macro) -> refac_common:get_subtrees(MId2, Macro, without_root) end, in_macro}
    ],
  ParentType = erl_syntax_db:type(MId, Parent),
  CheckedType = lists:filter(
                  fun({Type,_,_}) -> Type == ParentType end, CheckThese),
  lists:flatten(
    lists:map(
      fun({_, AvoidThese, ErrorMessage}) ->
        ContainsOccurrence =
          lists:dropwhile(
            fun(Id) -> not lists:member(Id, ExprOccurrences) end,
            lists:flatten([AvoidThese(MId, Parent)])),
        case ContainsOccurrence of
          [] -> [];
          [Id|_Ids] ->
            {Parent, Id, ErrorMessage}
        end
      end, CheckedType)).

