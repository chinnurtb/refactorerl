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
%% This module implements the tuple funpar refactoring.
%% 
%% @end

-module(refac_tuple_funpar).

-export([untuple_or_tuple_funpar/4,preorder/3]).

-include("node_type.hrl").

%% =====================================================================
%% @spec untuple_or_tuple_funpar(File::string(), Line::integer(), 
%%                       Col::integer(), NumberText::string()) -> none()
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
%% <b>NumberText</b> : The number of how many function parameters 
%%                     should be tupled. 
%% </pre>
%% @end
%% =====================================================================
untuple_or_tuple_funpar(File, Line, Col, NumberText) ->
    if
        NumberText == "" ->
            untuple_funpar(File, Line, Col);
        true ->
            Length = list_to_integer(NumberText),
            refac_checks:check_number_error(NumberText),
            tuple_funpar(File, Line, Col, Length)
    end.


%% =====================================================================
%% @spec untuple_funpar(File::string(), Line::integer(), 
%%                       Col::integer()) -> none()
%%
%% @doc
%% NOT YET IMPLEMENTED.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The pointed line number in the editor.
%% <b>Col</b> : The pointed column number in the editor.
%% </pre>
%% @end
%% =====================================================================
untuple_funpar(_File, _Line, _CCol) ->
    refac_checks:error_handler( {error, not_implemented_yet} ).


%% =====================================================================
%% @spec tuple_funpar(File::string(), Line::integer(), 
%%                       Col::integer(), NumberText::string()) -> {ok, done}
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
%% <b>NumberText</b> : The number of how many function parameters 
%%                     should be tupled. 
%% </pre>
%% @end
%% =====================================================================
tuple_funpar(File, Line, Col, Length) ->
    MId = refac_common:get_module_id(File),
    CLineCol = refactor:get_true_pos_from_pointed_pos(MId, Line, Col),
    refac_checks:check_true_pos(CLineCol, Line, Col),
    {_CLine, CCol} = CLineCol,
    IdandType =
        refactor:get_id_and_type_list_of_functions_from_pos_no_implicit_fun(
          MId, Line, CCol),
    ErrorPos = {Line, CCol},
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    refac_checks:check_pos_error(IdandType, ErrorPos),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {Id, IdType, Pos, MaxPos} = get_data(MId, Line, CCol, IdandType),
    {StartPos, EndPos} = get_positions(
                           MId, ErrorPos, Length, Id, IdType, Pos, MaxPos),
    RemainingPos = MaxPos - ( EndPos - StartPos ),
    {MId2, FunId, FunName} = find_the_function(MId, Id, IdType),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    refac_checks:check_name_clash(MId2, FunId, FunName, MaxPos, RemainingPos),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    perform_refactoring(MId2, FunId, StartPos, EndPos, MaxPos, RemainingPos),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    refac_common:warnings(),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    {ok, done}.

%% =====================================================================
%% @spec perform_refactoring(MId::integer(), FunId::integer(),
%%                          From::integer(), To::integer(),
%%                         Arity::integer(), NewArity::integer()) -> ok
%%
%% @doc
%% Performs the refactoring, and commits the changes into the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of the function.
%% <b>From</b> : The position of the first element which will be tupled.
%% <b>To</b> : The position of the last element which will be tupled.
%% <b>Arity</b> : The arity of the function.
%% <b>NewArity</b> : The new arity of the function
%% </pre>
%% @end
%% =====================================================================
perform_refactoring(MId, FunId, From, To, Arity, NewArity) ->
    {ApplicationIds, _ApplicationTypeandNameIds,
     _ImplicitFunCallIds, ImplicitFunCallandTypeIds,
     _ImplicitFunCallTypeandNameIds} = refac_common:get_fun_calls(MId, FunId),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    refresh_export_list(MId, FunId, Arity, NewArity),
    refresh_import_list(MId, FunId, Arity, NewArity),
    tuple_clause_parameters(MId, FunId, From, To, Arity),
    tuple_applications_parameter(MId, From, To, Arity, ApplicationIds),
    NewApplicationIds = tuple_implicit_functions(
                          MId, FunId, Arity, ImplicitFunCallandTypeIds),
    tuple_applications_parameter(MId, From, To, Arity, NewApplicationIds),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    refactor:commit().

%% =====================================================================
%% @spec get_positions(MId::integer(), ErrorPos::{integer(), integer()},
%%                          Length::integer(), Id::integer(),
%%                         IdType::integer(), Pos::[{integer()}], 
%%                        MaxPos::integer()) -> {integer(), integer()}
%%
%% @doc
%% Computes the first and last member of the new tuple.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ErrorPos</b> : The pointed position in the code.
%% <b>Length</b> : The number of elements which should be tupled.
%% <b>Id</b> : The nearest function clause, or application to the 
%%             pointed position.
%% <b>IdType</b> : The type of the nearest function usage or definition.
%% <b>Pos</b> : The pointed parameter. (Could be empty list, 
%%              if pointed position is not precise)
%% <b>MaxPos</b> : The function's last parameter's position.
%% </pre>
%% @end
%% =====================================================================
get_positions(MId, ErrorPos, Length, Id, IdType, Pos, MaxPos) ->
    case Pos == [] of
        false ->
            StartPos = element(1, hd(Pos)),
            EndPos = StartPos + Length - 1,
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            refac_checks:check_for_length_overrun(EndPos, MaxPos, Length),
            {StartPos, EndPos};
        true ->
            get_nearest_positions(MId, ErrorPos, Length, Id, IdType, MaxPos)
    end.

%% =====================================================================
%% @spec get_nearest_positions(MId::integer(), ErrorPos::{integer(), integer()},
%%                          Length::integer(), Id::integer(),
%%                         IdType::integer(),
%%                        MaxPos::integer()) -> {integer(), integer()}
%%
%% @doc
%% Computes the first and last member of the new tuple.
%% It is needed when the pointed position is not precise.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ErrorPos</b> : The pointed position in the code.
%% <b>Length</b> : The number of elements which should be tupled.
%% <b>Id</b> : The nearest function clause, or application to the 
%%             pointed position.
%% <b>IdType</b> : The type of the nearest function usage or definition.
%% <b>MaxPos</b> : The function's last parameter's position.
%% </pre>
%% @end
%% =====================================================================
get_nearest_positions(MId, ErrorPos, Length, Id, ?CLAUSE, MaxPos) ->
    {Line, Col} = ErrorPos,
    StartPosType =
        refactor:get_argument_nearest_position_and_type_from_clause_id_and_pos(
          MId, Id, Line, Col),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    refac_checks:check_is_parameter(StartPosType, ErrorPos),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [{StartPos, Type}] = StartPosType,
    EndPos = StartPos + Length - 1,
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    refac_checks:check_parameter_type(Type, ErrorPos),
    refac_checks:check_for_length_overrun(EndPos, MaxPos, Length),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    {StartPos, EndPos};
get_nearest_positions(MId, ErrorPos, Length, Id, ?APPLICATION, MaxPos) ->
    {Line, Col} = ErrorPos,
    StartPosId =
        refactor:get_argument_nearest_position_and_type_from_application_id_and_pos(
          MId, Id, Line, Col),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    refac_checks:check_is_parameter(StartPosId, ErrorPos),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [{StartPos, StartPosId}] = StartPosId,
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    case StartPos == MaxPos of
        false ->
            EndPos = StartPos + Length - 1,
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            refac_checks:check_for_length_overrun(EndPos, MaxPos, Length),
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            {StartPos, EndPos};
        true ->
            IsElement = local_preorder(MId, StartPosId,ErrorPos),
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            refac_checks:check_is_element(IsElement, ErrorPos),
            refac_checks:check_for_length_overrun(MaxPos, MaxPos, Length),
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
            {MaxPos, MaxPos}
    end.

%% =====================================================================
%% @spec get_data(MId::integer(), Line::integer(),
%%               CCol::integer(), 
%%             NearUse::[{integer(), integer()}]) -> 
%%                   {integer(), integer(), integer(), integer()}
%%
%% @doc
%% Returns the data of the pointed position.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Line</b> : The pointed line in the code.
%% <b>CCol</b> : The pointed column in the code.
%% <b>NearUse</b> : The nearest function clause or applications id and type.
%% </pre>
%% @end
%% =====================================================================
get_data(MId, Line, CCol, [{Id,?CLAUSE}]) ->
    Pos = refactor:get_argument_position_from_clause_id_and_pos(
            MId, Id, Line, CCol),
    MaxPos = refactor:get_max_argument_position_from_clause_id(MId, Id),
    {Id, ?CLAUSE, Pos, MaxPos};
get_data(MId, Line, CCol, [{Id, ?APPLICATION}]) ->
    Pos = refactor:get_argument_position_from_application_id_and_pos(
            MId, Id, Line, CCol),
    MaxPos = refactor:get_max_argument_position_from_application_id(MId, Id),
    {Id, ?APPLICATION, Pos, MaxPos}.

%% =====================================================================
%% @spec local_preorder(MId::integer(), NearPosId::integer(),
%%               ErrorPos::{integer(), integer()}) -> 
%%                   bool()
%%
%% @doc
%% Checks if the pointed position is in the complex structure NeaPosId.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NearPosId</b> : The pointed line in the code.
%% <b>ErrorPos</b> : The pointed column in the code.
%% </pre>
%% @end
%% =====================================================================
local_preorder(MId, NearPosId, ErrorPos) ->
    ElementsId = refactor:get_id_from_pos(MId, ErrorPos), 
    Element = element(1, hd(ElementsId)),
    Result = preorder(MId, NearPosId, Element),
    lists:member(true, lists:flatten(Result)).

%% =====================================================================
%% @spec preorder(MId::integer(), Tree::integer(),
%%               Id::integer()) -> 
%%                    Result
%%              Result = [term()] | bool()
%%
%% @doc
%% Checks if Id is a member of the Tree syntax tree. The result is either
%% a boolean value or a deep list of boolean values.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Tree</b> : Root of a subtree.
%% <b>Id</b> : Id of a node.
%% </pre>
%% @end
%% =====================================================================
preorder(MId,Tree,Id)->
    case Tree == Id of
        true ->
            true;
        false ->
            case erl_syntax_db:subtrees(MId,Tree) of
                [] ->
                    false;
                List ->
                    lists:map( 
                      fun(Elements) ->
                              lists:map( 
                                fun(Element) ->
                                        preorder(MId, Element, Id)
                                end, Elements)
                      end, List)
            end
    end.

%% =====================================================================
%% @spec find_the_function(MId::integer(), Id::integer(),
%%               Type::integer()) -> 
%%                  {integer(), integer(), integer()}
%%
%% @doc
%% Finds the function which parameters are pointed to create a tuple from it.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of a node.
%% <b>Type</b> : Type of the node.
%% </pre>
%% @end
%% =====================================================================
find_the_function(MId, Id, ?APPLICATION) ->
    {MId2, FunId} = refactor:get_fun_id_from_implicit_fun_id_or_application(
                     MId, Id),
    FunName = refactor:get_fun_name(MId2, FunId),
    {MId2, FunId, FunName};
find_the_function(MId, Id, ?CLAUSE) ->
    IsFunctionClause = refactor:get_fun_id_from_clause_id_function_table(MId, Id),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    refac_checks:check_is_function_clause(IsFunctionClause),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    FunId = element(1, hd(IsFunctionClause)),
    FunName = refactor:get_fun_name(MId, FunId),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    {MId, FunId, FunName}.

%% =====================================================================
%% @spec refresh_export_list(MId::integer(), FunId::integer(),
%%               Arity::integer(), NewArity::integer()) -> [ok]
%%
%% @doc
%% Updates the modified functions arity in the export list.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of a function.
%% <b>Arity</b> : Arity of the function.
%% <b>NewArity</b> : The new arity of the function.
%% </pre>
%% @end
%% =====================================================================
refresh_export_list(MId, FunId, Arity, NewArity) ->
    refactor:update_fun_visib(MId, FunId, NewArity),
    FunName = refactor:get_fun_name(MId ,FunId),
    lists:map(
      fun({BodyId, ArityId}) ->
              case refactor:get_arity_qualifier_data(MId, [{BodyId, ArityId}]) ==
                  [{FunName, Arity}] of
                  true ->
                      refactor:update_integer(MId, ArityId, NewArity);
                  false ->
                      ok
              end
      end,
      refactor:get_export_list_id(MId)).

%%%TODO: ezt lehetne altalanositani (refactor modulba vele)
%% =====================================================================
%% @spec refresh_import_list(MId::integer(), FunId::integer(),
%%               Arity::integer(), NewArity::integer()) -> ok
%%
%% @doc
%% Updates the modified functions arity in the import lists.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of a function.
%% <b>Arity</b> : Arity of the function.
%% <b>NewArity</b> : The new arity of the function.
%% </pre>
%% @end
%% =====================================================================
refresh_import_list(MId, FunId, Arity, NewArity) ->
    FunName = refactor:get_fun_name(MId, FunId),
    ModuleName = refactor:get_module_name(MId),
    UsedLocations = refactor:get_module_ids_where_used_from_fun_id(MId, FunId),
    UsedOuterLocations=lists:delete({MId}, UsedLocations),
    ImportListIds =
        lists:map(fun({MId2}) ->
                          {MId2, refactor:get_import_list_ids(MId2)}
                  end,UsedOuterLocations),
    ImportedFuns = my_flatten(lists:map(
                                fun({MId2, MId2ImportListIds}) ->
                                        refactor:get_imported_functions_and_ids(
                                          MId2, MId2ImportListIds)
                                end,ImportListIds)),
    refresh_import_list(ModuleName,FunName,Arity,NewArity,ImportedFuns).

%% =====================================================================
%% @spec refresh_import_list(MId::integer(), FunId::integer(),
%%               Arity::integer(), NewArity::integer(), 
%%             ImportedFuns::[{string(), string(), integer(), 
%%                             integer(), integer(), integer()}] ) -> ok
%%
%% @doc
%% Updates the modified functions arity in the import lists.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of a function.
%% <b>Arity</b> : Arity of the function.
%% <b>NewArity</b> : The new arity of the function.
%% <b>ImportedFuns</b> : The data of the imported functions.
%% </pre>
%% @end
%% =====================================================================
refresh_import_list(_ModuleName, _FunName, _Arity, _NewArity, []) ->
    ok;
refresh_import_list(ModuleName, FunName, Arity, NewArity,
                    [{ModuleName, FunName, Arity, MId, _BodyId, ArityId} | _Xs]) ->
    refactor:update_integer(MId, ArityId, NewArity);		   
refresh_import_list(ModuleName, FunName, Arity, NewArity,[_X | Xs]) ->
    refresh_import_list(ModuleName, FunName, Arity, NewArity, Xs).

%%%TODO: lehetne helyette lists:append. + meg ez nem tul hatekony a ++ miatt    
%% =====================================================================
%% @spec my_flatten(List::[[term()]]) -> [term()]
%%
%% @doc
%% Flatten a list with just one deepness.
%% 
%% Parameter description:<pre>
%% <b>List</b> : At least to list deep of terms.
%% </pre>
%% @end
%% =====================================================================
my_flatten([]) ->
    [];
my_flatten([X | Xs]) ->
    my_flatten(X, Xs).

my_flatten(X , []) ->
    X;
my_flatten(X, [Y | Ys]) ->
    my_flatten(X ++ Y, Ys).


%% =====================================================================
%% @spec tuple_clause_parameters(MId::integer(), FunId::integer(),
%%              From::integer(), To::integer(), Arity::integer()) -> ok
%%
%% @doc
%% Tuples the function's clauses' parameters from From to To.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of a function.
%% <b>From</b> : The starting element of the new tuple.
%% <b>To</b> : The last element of the new tuple.
%% <b>Arity</b> : Arity of the function.
%% </pre>
%% @end
%% =====================================================================
tuple_clause_parameters(MId, FunId, From, To, Arity) ->
    ClauseIds = refactor:get_clause_ids_from_fun_id(MId, FunId),
    tuple_clause_parameters_(MId, From, To, Arity, ClauseIds).

%% =====================================================================
%% @spec tuple_clause_parameters_(MId::integer(), From::integer(),
%%              To::integer(), Arity::integer(), ClauseIds::[{integer()}]) -> ok
%%
%% @doc
%% Tuples the function's clauses' parameters from From to To.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>From</b> : The starting element of the new tuple.
%% <b>To</b> : The last element of the new tuple.
%% <b>Arity</b> : Arity of the function.
%% <b>ClauseIds</b> : Ids of a function's clauses.
%% </pre>
%% @end
%% =====================================================================
tuple_clause_parameters_(_MId, _From, _To, _Arity, [])->
    ok;
tuple_clause_parameters_(MId, From, To, Arity, [{ClauseId} | Xs]) ->
    Elements = refactor:get_arguments_from_clause_id_and_from_to_pos(
                 MId, ClauseId, From, To),
    TupleId = create_nodes:create_tuple(MId, erl_syntax_db:untuple(Elements)),
    refactor:update_clause_argument(MId, ClauseId, TupleId, From),
    refactor:delete_clause_argument_between_pos(MId, ClauseId, From, To),
    refactor:update_clause_pos_between_given_positions(MId, ClauseId, From, To),
    create_nodes:init_scope(MId, ClauseId, [TupleId]),
    tuple_clause_parameters_(MId, From, To, Arity, Xs).

%% same thing in erl_syntax_db removed from here
%% untuple(List) ->
%%     lists:map(fun({Element}) -> Element end, List).

%% =====================================================================
%% @spec tuple_applications_parameter(
%%       MId::integer(), From::integer(),
%%        To::integer(), Arity::integer(), 
%%         ApplicationIds::[{integer(), integer()}]) -> ok
%%
%% @doc
%% Tuples the function's applications parameters from From to To.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>From</b> : The starting element of the new tuple.
%% <b>To</b> : The last element of the new tuple.
%% <b>Arity</b> : Arity of the function.
%% <b>ApplicationIds</b> : Ids of a function's applications.
%% </pre>
%% @end
%% =====================================================================
tuple_applications_parameter(_MId,_From,_To,_Arity,[]) ->
    ok;
tuple_applications_parameter(MId, From, To, Arity, [{MId2, ApplicationId} | Xs]) ->
    perform_tuple_applications_parameter(
      MId, From, To, Arity, MId2, ApplicationId, Xs);
tuple_applications_parameter(
  MId, From, To, Arity, [{MId2, ApplicationId, ?APPLICATION} | Xs]) ->
    perform_tuple_applications_parameter(
      MId, From, To, Arity, MId2, ApplicationId, Xs).

%% =====================================================================
%% @spec perform_tuple_applications_parameter(
%%       MId::integer(), From::integer(),
%%        To::integer(), Arity::integer(), MId2::integer() ,
%%       ApplicationId::integer(), 
%%      Xs::[{integer(), integer(), integer()}]) -> ok
%%
%% @doc
%% Tuples the function's applications parameters from From to To.
%% It is an auxiliary function of the @see tuple_applications_parameter/5.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>From</b> : The starting element of the new tuple.
%% <b>To</b> : The last element of the new tuple.
%% <b>Arity</b> : Arity of the function.
%% <b>MId</b> : Id of the module where the function call is.
%% <b>ApplicationId</b> : Ids of a function's call. (application)
%% <b>Xs</b> : Data of a function's calls. (applications)
%% </pre>
%% @end
%% =====================================================================
perform_tuple_applications_parameter(
  MId, From, To, Arity, MId2, ApplicationId, Xs) ->
    ScopeId = refactor:get_scope_from_id(MId2, ApplicationId),
    Elements = refactor:get_arguments_from_application_id_and_from_to_pos(
                 MId2, ApplicationId, From, To),
    TupleId = create_nodes:create_tuple(MId2, erl_syntax_db:untuple(Elements)),
    refactor:update_application_argument(MId2, ApplicationId, TupleId, From),
    refactor:delete_application_argument_between_pos(
      MId2, ApplicationId, From, To),
    refactor:update_application_pos_between_given_positions(
      MId2, ApplicationId, From, To),
    create_nodes:init_scope(MId2, ScopeId, [TupleId]),
    tuple_applications_parameter(MId, From, To, Arity, Xs).

%% =====================================================================
%% @spec tuple_implicit_functions(
%%       MId::integer(), FunId::integer(),
%%        Arity::integer(), 
%%        ImplicitFunData::[{integer(), integer(), integer()}]) -> 
%%                           [{integer(), integer()}]
%%
%% @doc
%% Tuples the function's implicit_fun calls from From to To.
%% We need to transform these calls into applications to be able to do
%% the transformation
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of the function.
%% <b>Arity</b> : Arity of the function.
%% <b>ApplicationIds</b> : Data of the function's implicit_fun calls
%%                            (type, module id, id).
%% </pre>
%% @end
%% =====================================================================
tuple_implicit_functions(_MId,_FunId,_Arity,[]) ->
    [];
tuple_implicit_functions(MId, FunId, Arity, 
                         [{?ARITY_QUALIFIER, MId2, ImplicitId} | Xs]) ->
    perform_tuple_implicit_functions(
      MId, FunId, Arity, MId2, ImplicitId, Xs,
      fun(FunNameId, ApplicationParList) ->
              ApplicationId = create_nodes:create_application(
                                MId2, FunNameId, ApplicationParList),
              {none, none, ApplicationId}
      end,
      fun(ClauseId, FormParList, ApplicationParList, FunNameId,
          _ModuleNameId, _ModuleQualifierId, ApplicationId) ->
              create_nodes:init_scope(
                MId2, ClauseId, FormParList ++ ApplicationParList
                ++ [FunNameId, ApplicationId, ClauseId])
      end);
tuple_implicit_functions(MId, FunId, Arity,
                         [{?MODULE_QUALIFIER, MId2, ImplicitId} | Xs]) ->
    perform_tuple_implicit_functions(
      MId, FunId, Arity, MId2, ImplicitId, Xs,
      fun(FunNameId,  ApplicationParList) ->
              ModuleNameId =
                  create_nodes:create_atom(MId2,refactor:get_module_name(MId)),
              ModuleQualifierId =
                  create_nodes:create_module_qualifier(
                    MId2, ModuleNameId, FunNameId),
              ApplicationId =
                  create_nodes:create_application(
                    MId2, ModuleQualifierId, ApplicationParList),
              {ModuleNameId, ModuleQualifierId, ApplicationId}
      end,
      fun(ClauseId,FormParList,ApplicationParList,FunNameId,
          ModuleNameId,ModuleQualifierId,ApplicationId) ->
              create_nodes:init_scope(
                MId2, ClauseId, FormParList ++ ApplicationParList
                ++ [FunNameId, ModuleNameId, ModuleQualifierId, 
                    ApplicationId, ClauseId])
      end).

%% =====================================================================
%% @spec perform_tuple_implicit_functions(
%%       MId::integer(), FunId::integer(),
%%        Arity::integer(), MId2::integer(), ImplicitId::integer(),
%%        ImplicitFunData::[{integer(), integer(), integer()}], 
%%        GetIds::function(), InitScope::function()) -> 
%%                           [{integer(), integer()}]
%%
%% @doc
%% Tuples the function's implicit_fun calls from From to To.
%% We need to transform these calls into applications to be able to do
%% the transformation
%% It is an auxiliary function of the @see tuple_implicit_functions/4.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of the function.
%% <b>Arity</b> : Arity of the function.
%% <b>MId2</b> : Id of the module where the implicit_fun is.
%% <b>ImplicitId</b> : Id of the implicit_fun.
%% <b>ApplicationIds</b> : Data of the function's implicit_fun calls
%%                            (type, module id, id).
%% <b>GetIds</b> : A function to create the basic structure of the 
%%                 transformed implicit_fun.
%% <b>InitScope</b> : A function to initialize the newly created 
%%                    elements scope.
%% </pre>
%% @end
%% =====================================================================
perform_tuple_implicit_functions(MId, FunId, Arity,
                                 MId2, ImplicitId, Xs, GetIds, InitScope) ->
    ScopeId = refactor:get_scope_from_id(MId2, ImplicitId),
    delete_nodes:delete_node(MId2,ImplicitId),
    {Name, Num} = create_nodes:create_fresh_variable(MId2),
    FormParList = create_nodes:create_variables(MId2, Name, Num, Arity),
    ApplicationParList = create_nodes:create_variables(MId2, Name, Num, Arity),
    FunNameId = create_nodes:create_atom(MId2, refactor:get_fun_name(MId, FunId)),
    {ModuleNameId, ModuleQualifierId, ApplicationId} =
        GetIds(FunNameId, ApplicationParList),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    create_nodes:connect_fun_call(MId2, ApplicationId, MId, FunId),
    ClauseId = create_nodes:create_clause(MId2, FormParList, none, [ApplicationId]),
    create_nodes:create_fun_expr(MId2, [ClauseId], ImplicitId),
    InitScope(ClauseId, FormParList, ApplicationParList, FunNameId,
              ModuleNameId, ModuleQualifierId, ApplicationId),
    create_nodes:init_scope(MId2, ScopeId,[ ImplicitId]),
    create_nodes:create_scope(MId2, ClauseId, ScopeId),
    create_nodes:connect_variables(MId2, FormParList, FormParList),
    create_nodes:connect_variables(MId2, FormParList, ApplicationParList),
    [{MId2, ApplicationId} | tuple_implicit_functions(MId, FunId, Arity, Xs)].
