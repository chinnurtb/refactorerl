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
%% This module implements the rename function refactoring.
%% 
%% @end
-module(refac_ren_fun).

-vsn('0.1').

-export([rename_function/4]).

-include("node_type.hrl").


%% =====================================================================
%% @spec rename_function(File::string(), Line::integer(), 
%%                       Col::integer(), Newname::string()) -> none()
%%
%% @doc
%% Performs the precondition checks and the and the collects all the 
%% necessary information for the refactoring.
%% It throws various terms when there is an error, with this structure:
%% {atom(), term()}. If the atom is ok, the refactoring has been 
%% preformed without a problem.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The pointed line number in the editor.
%% <b>Col</b> : The pointed column number in the editor.
%% <b>Newname</b> : The new name of the function. </pre>
%% @end
%% =====================================================================
rename_function(File, Line, Col, Newname) ->
    MId = refac_common:get_module_id(File),
    IdandType = refac_common:get_id_from_pos(MId, Line, Col, function),
    {MId2, FunId} = refac_common:find_the_function(MId, IdandType),
    Arity = refactor:get_arity_from_fun_id(MId2, FunId),
    FunnameId = refactor:get_fun_name_id_from_fun_id (MId2, FunId),
    FunName = refactor:get_name_from_name_id (MId2, FunnameId),
    {ExportListIds, FunctionData, ImportFunctions, FunIsImportedMIds} = 
	get_data(MId2, FunName, Arity),
    
    refac_checks:check_isFunctionName(Newname),
    refac_checks:check_is_autoimported(Newname),
    refac_checks:check_the_name_already_exists(Newname,Arity,FunctionData),
    refac_checks:check_the_name_is_imported(Newname,Arity,ImportFunctions),

    perform_refactoring(
      MId2, FunName, Arity, ExportListIds, Newname, FunIsImportedMIds, FunId),

    refac_common:warnings(),
    {ok, Newname}.

%% =====================================================================
%% @spec perform_refactoring(
%%            MId::integer(), FunName::string(),
%%          Arity::integer(), ExportListIds::[{integer(), integer()}],
%%        Newname::string(), FunIsImportedMIds::[{integer()}],
%%        FunId::integer()) -> ok
%%
%% @doc
%% Performs the refactoring, and commits the changes into the 
%% refactoring system.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunName</b> : The name of the renamed function.
%% <b>Arity</b> : The arity of the renamed function.
%% <b>ExportListIds</b> : The data of the exported functions.
%% <b>Newname</b> : The new name of the renamed function.
%% <b>FunIsImportedMIds</b> : The modules' id where the 
%%                            function is imported.
%% <b>FunId</b> : The id of the renamed function. </pre>
%% @end
%% =====================================================================
perform_refactoring(
  MId, FunName, Arity, ExportListIds, Newname, FunIsImportedMIds, FunId) ->
    case is_exported(MId, FunName, Arity, ExportListIds) of
	false ->
	    ok;
	ExportNameId ->
	    refactor:update_function_name(MId, ExportNameId, Newname)
    end,

  lists:map(
    fun({MId2}) ->
	    ImportListIds = refactor:get_import_list_ids(MId2),
	    ImportFunctions = get_imported_functions_and_ids(
				MId2, ImportListIds),
	    rename_in_import_list(
	      Newname, 
	      refactor:get_module_name(MId), 
	      FunName, Arity, MId2, ImportFunctions)
    end, FunIsImportedMIds),
    
    rename_clauses(MId, Newname, FunId),
    rename_function_calls(MId, Newname, FunId, FunIsImportedMIds),

    refactor:commit().


%% =====================================================================
%% @spec rename_function_calls(
%%            MId::integer(), NewName::string(),
%%          FunId::integer(), FunIsImportedMIds::[{integer()}]) -> ok
%%
%% @doc
%% Renames the function calls which call the renamed function.   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Newname</b> : The new name of the renamed function.
%% <b>FunId</b> : The id of the function.
%% <b>FunIsImportedMIds</b> : The modules' id where the 
%%                            function is imported.</pre>
%% @end
%% =====================================================================
rename_function_calls(MId, Newname, FunId, FunIsImportedMIds) ->
    {ApplicationInnerNameIds, ApplicationDeprecatedNameIds, 
     ApplicationModuleNameIds, ImplicitInnerFunCallNameIds, 
     ImplicitModuleFunCallNameIds} =
	get_filtered_fun_calls(MId, FunId),
    lists:foreach(
      fun ({_Type,MId2,Id}) ->
	      refactor:update_function_name(MId2, Id, Newname)
      end, ApplicationInnerNameIds),
    lists:foreach(
      fun ({_Type,MId2,Id}) ->
	      Elements = 
		  refactor:get_tuple_element_ids_from_tuple_id(MId2, Id),
	      NameId = element(1,hd(tl(Elements))),
	      refactor:update_function_name(MId2, NameId, Newname)
      end, ApplicationDeprecatedNameIds),
    lists:foreach(
      fun ({_Type,MId2,Id}) ->
	      NameId = 
		  refactor:get_module_qualifier_body_form_module_qualifier_id(
		    MId2, Id),
	      refactor:update_function_name(MId2, NameId, Newname)
      end, ApplicationModuleNameIds),
    lists:foreach(
      fun ({_Type,MId2,Id}) ->
	      NameId = 
		  refactor:get_arity_qualifier_body_form_arity_qualifier_id(
		    MId2, Id),
	      refactor:update_function_name(MId2, NameId, Newname)
      end, ImplicitInnerFunCallNameIds),
    lists:foreach(
      fun ({_Type,MId2,Id}) ->
	      ArityQualifierId = 
		  refactor:get_module_qualifier_body_form_module_qualifier_id(
		    MId2, Id),
	      NameId = 
		  refactor:get_arity_qualifier_body_form_arity_qualifier_id(
		    MId2, ArityQualifierId),
	      refactor:update_function_name(MId2, NameId, Newname)
      end, ImplicitModuleFunCallNameIds),
    lists:foreach(
      fun(MId2) -> out_from_db:create_code(MId2) end,
      sets:to_list(
	sets:del_element(
	  MId, get_modified_MIds(ApplicationInnerNameIds 
				 ++ ApplicationDeprecatedNameIds
				 ++ ApplicationModuleNameIds 
				 ++ ImplicitInnerFunCallNameIds
				 ++ ImplicitModuleFunCallNameIds 
				 ++ FunIsImportedMIds)))).

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
%% @spec get_data(
%%            MId::integer(),
%%              ExportListIds::[integer()]) -> 
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
%% @spec get_imported_functions_and_ids(
%%            MId::integer(), ImportListIds::[integer()]) -> 
%%                [{string(), string(), integer()}]
%%
%% @doc
%% Returns the imported functions data in a list. 
%%     {module name, function name, arity} .   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ExportListIds</b> : The import list ids of the module.
%% </pre>
%% @end
%% =====================================================================
get_imported_functions_and_ids(MId, ImportListIds) ->
    ImportDataList = 
	lists:map(
	  fun({Id}) -> 
		  ModuleName = 
		      refactor:get_module_name_from_attribute_id(MId, Id),
		  ArityQualifierIds = 
		      refactor:get_arity_qualifier_ids_from_attribute_id(
			MId, Id),
		  ArityQualifierDatas = 
		      refactor:get_arity_qualifier_data_and_id(
			MId, ArityQualifierIds),
		  ImportDatas = 
		      refactor:add_element_to_tuples(
			ModuleName, ArityQualifierDatas),
		  ImportDatas end,
	  ImportListIds),
    lists:flatten(ImportDataList).

%% =====================================================================
%% @spec rename_in_import_list(
%%        NewName::string(), ModuleName::string(),
%%        FunName::string(), Arity::integer(),
%%        MId::integer(), 
%%        ImportFunctionDatas::[{string(), string(), integer(), integer()}]
%%                            ) -> ok
%%
%% @doc
%% Renames the function in the import lists.   
%% 
%% Parameter description:<pre>
%% <b>NewName</b> : The new name of the function.
%% <b>ModuleName</b> : The name of the module.
%% <b>FunName</b> : The name of the function.
%% <b>Arity</b> : The arity of the function.
%% <b>MId</b> : Id of the module.
%% <b>ImportFunctionDatas</b> : The data of the imported functions.
%% </pre>
%% @end
%% =====================================================================
rename_in_import_list(_Newname, _ModuleName, _FunName, 
			    _Arity, _MId, [])->
    ok;
rename_in_import_list(
  Newname, ModuleName, FunName, Arity, MId, 
  [{ModuleName, FunName, FunId, Arity} | Xs]) ->
    refactor:update_function_name(MId, FunId, Newname),
    rename_in_import_list(Newname, ModuleName, FunName, Arity, MId, Xs);
rename_in_import_list(
  Newname, ModuleName, FunName, 
  Arity, MId, [_X|Xs]) ->
    rename_in_import_list(Newname, ModuleName, FunName, Arity, MId, Xs).

%% =====================================================================
%% @spec is_exported(
%%         MId::integer(), FunName::string(),
%%         FunArity::integer(), ExporListData::[{integer(), integer()}]) -> 
%%                    bool()
%%
%% @doc
%% Check if the searched function is a member of the list.   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunName</b> : The name of the function.
%% <b>FunArity</b> : The arity of the function.
%% <b>ExportListData</b> : The module's export list's members id.
%% </pre>
%% @end
%% =====================================================================
is_exported(_MId, _Funname, _Funarity, [])->
    false;
is_exported(MId, Funname, Funarity, [{NameId,ArityId}|Xs]) ->
    Name = refactor:get_name_from_name_id(MId, NameId),
    case (Funname == Name) of
	true ->
            Arity = refactor:get_arity_from_arity_id(MId, ArityId),
	    case (Funarity == list_to_integer(Arity)) of
		true ->
		    NameId;
		false ->
		    is_exported(MId, Funname, Funarity, Xs)
	    end;
	false ->
	    is_exported(MId, Funname, Funarity, Xs)
    end.

%% =====================================================================
%% @spec get_modified_MIds(
%%         MIds::[{integer()}]) -> 
%%                    [integer()]
%%
%% @doc
%% Returns a unique list containing the modified modules module id.   
%% 
%% Parameter description:<pre>
%% <b>MIds</b> : A list of module ids.
%% </pre>
%% @end
%% =====================================================================
get_modified_MIds(List) ->
    get_modified_MIds(List, sets:new()).

%% @type set(). A set with undefined representation.

%% =====================================================================
%% @spec get_modified_MIds(
%%         MIds::[{integer()}], Acc::set()) -> 
%%                    [integer()]
%%
%% @doc
%% Returns a unique list containing the modified modules module id.   
%% 
%% Parameter description:<pre>
%% <b>MIds</b> : A list of module ids.
%% <b>Acc</b> : The accumulated set of module ids.
%% </pre>
%% @end
%% =====================================================================
get_modified_MIds([], Set) ->
    Set;
get_modified_MIds([{MId} | Xs], Set) ->
    get_modified_MIds(Xs, sets:add_element(MId, Set));
get_modified_MIds([{_Type,MId,_Id} | Xs], Set) ->
    get_modified_MIds(Xs, sets:add_element(MId, Set)).

%% =====================================================================
%% @spec rename_clauses(
%%        MId::integer(), NewName::string(),   
%%        FunId::integer()) -> ok
%%
%% @doc
%% Renames the function in the import lists.   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NewName</b> : The new name of the function.
%% <b>FunId</b> : Id of the function.
%% </pre>
%% @end
%% =====================================================================
rename_clauses(MId, Newname, FunId) ->
    FunnameId = refactor:get_fun_name_id_from_fun_id(MId, FunId),
    refactor:update_function_name(MId, FunnameId, Newname).

%% =====================================================================
%% @spec get_filtered_fun_calls(
%%        MId::integer(),  FunId::integer()) -> 
%%                {[{integer(), integer(), integer()}], 
%%                 [{integer(), integer(), integer()}], 
%%                 [{integer(), integer(), integer()}], 
%%                 [{integer(), integer(), integer()}], 
%%                 [{integer(), integer(), integer()}]}
%%
%% @doc
%% Returns the calls of the function sorted by type.   
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of the function.
%% </pre>
%% @end
%% =====================================================================
get_filtered_fun_calls(MId, FunId) ->
    {_ApplicationIds, ApplicationTypeandNameIds,
     _ImplicitFunCallIds, _ImplicitFunCallandTypeIds,
     ImplicitFunCallTypeandNameIds} = refac_common:get_fun_calls(MId, FunId),
    ApplicationInnerNameIds =
      filter_for(?ATOM, ApplicationTypeandNameIds),
    ApplicationModuleNameIds =
      filter_for(?MODULE_QUALIFIER, ApplicationTypeandNameIds),
    ApplicationDeprecatedNameIds =
      filter_for(?TUPLE, ApplicationTypeandNameIds),
    ImplicitInnerFunCallNameIds = 
      filter_for(?ARITY_QUALIFIER, ImplicitFunCallTypeandNameIds),
    ImplicitModuleFunCallNameIds = 
      filter_for(?MODULE_QUALIFIER, ImplicitFunCallTypeandNameIds),
    {ApplicationInnerNameIds, ApplicationDeprecatedNameIds, 
     ApplicationModuleNameIds, ImplicitInnerFunCallNameIds, 
     ImplicitModuleFunCallNameIds}.

%% =====================================================================
%% @spec filter_for(
%%        ExpectedType::integer(), 
%%        CallDataList::[{integer(), integer(), integer()}]
%%                 ) ->
%%                 [{integer(), integer(), integer()}]
%%
%% @doc
%% Filters for one type of function call.
%% 
%% Parameter description:<pre>
%% <b>ExpectedType</b> : Type of a funtion call.
%% <b>CallDataList</b> : Function call data list. {type, module id, id}
%% </pre>
%% @end
%% =====================================================================
filter_for(ExpectedType, List) ->
    lists:filter( fun ({Type,_MId,_Id}) -> Type == ExpectedType end, List).
