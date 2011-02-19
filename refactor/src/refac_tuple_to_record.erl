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

%%% Author contact: erlang@plc.inf.elte.hu
%%% --------------------------------------------------------------------------

%% @copyright 2007 Eötvös Loránd University and Ericsson Hungary
%% @author Tamas Nagy <lestat@elte.hu>

%% @doc 
%% This module implements the tuple to record refactoring.
%% This version contains just the really basic refactoring.
%% The refactoring works only in one module and in one step deep.
%% It is always create the compensation macros to the record.
%% 
%% @end


%% Changelog
%%  Version 0.2: New module 
%% End changelog

-module(refac_tuple_to_record).

-export([tuple_to_record/7]).

-include("node_type.hrl").

-record(surdata, 
        {macro=false, application=false, pattern=false, lbody=false,
         msend=false, mid, id}).

%% =====================================================================
%% @spec tuple_to_record(File::string(), FromLine::integer(), 
%%                       FromCol::integer(), ToLine::integer(),
%%                       ToCol::integer(), RecordName::string(),
%%                       RecordParamsString::string()) -> {ok, done}
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
%% <b>FromLine</b> : The begining of the selection(Line).
%% <b>FromCol</b> : The begining of the selection(Column).
%% <b>ToLine</b> : The end of the selection(Line).
%% <b>ToCol</b> : The end of the selection(Column).
%% <b>RecordName</b> : The name of the record.
%% <b>RecordParamsString</b> : The name of the record fields.
%% </pre>
%% @end
%% =====================================================================
tuple_to_record(File, FromLine, FromCol, ToLine, ToCol, RecordName, 
                RecordParamsString) ->
    MId = refac_common:get_module_id(File),
    {Found, PathFromRootClause, ExprId} =
        get_expression_id(MId, FromLine, FromCol, ToLine, ToCol),
    RecordParams = convert_record_names(RecordParamsString),
%%DEBUG    io:format("RecordName:~p~n", [RecordName]),
%%DEBUG    io:format("RecordParams:~p~n", [RecordParams]),
    refac_checks:check_if_tuple(
      MId, Found, ExprId, FromLine, FromCol, ToLine, ToCol),
    refac_checks:check_param_number_equal_to_tuple_length(
      RecordParams, MId, ExprId),
    RootClause = hd(PathFromRootClause),
    refac_checks:check_if_not_embedded_tuple(MId, PathFromRootClause),
    Used = check_record_name_not_used(MId, RecordName),
%%DEBUG    io:format("RecordName Used:~p~n", [Used]),
    MaybeVar = get_match_expr_pattern(MId, PathFromRootClause),
%%DEBUG    io:format("Is there a match:~p~n", [MaybeVar]),
    TupleIds = get_all_the_tuples(MId, RootClause),
%%DEBUG    io:format("All the tuples:~p~n", [TupleIds]),
    SameTupleIds = 
        get_the_same_ones_as_the_selected_tuple(MId, ExprId, TupleIds),
%%DEBUG    io:format("Same tuples:~p~n", [SameTupleIds]),
    TupleData =
        get_tuples_surrounding(MId, SameTupleIds, RecordName),
%%DEBUG    io:format("Same tuples with surround:~p ~n", [TupleData]),
    MaybeVars = get_variables_surrounding(MId, MaybeVar, RecordName),
%%DEBUG    io:format("MaybeVars:~p~n", [MaybeVars]),
    FunId = get_function_id(MId, RootClause),
    FunctionCalls = get_function_calls(
                      MId, FunId, element(2,TupleData), 
                      element(3,TupleData), MaybeVars, RecordName),
%%DEBUG    io:format("FunctionCalls:~p~n", [FunctionCalls]),
    perform_refactoring(MId, MaybeVars, TupleData, FunctionCalls,
                        RecordName, RecordParams, Used),
    {ok, done}.

%% =====================================================================
%% @spec get_expression_id(MId::integer(), FromLine::integer(), 
%%                         FromCol::integer(), ToLine::integer(),
%%                         ToCol::integer()) -> none()
%%
%% @doc
%% Get the expression id from the pointed position.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FromLine</b> : The begining of the selection(Line).
%% <b>FromCol</b> : The begining of the selection(Column).
%% <b>ToLine</b> : The end of the selection(Line).
%% <b>ToCol</b> : The end of the selection(Column).
%% </pre>
%% @end
%% =====================================================================
get_expression_id(MId, FromLine, FromCol, ToLine, ToCol) ->
    {CFromLine, CFromCol} = 
        refactor:get_true_pos_from_pointed_pos(MId, FromLine, FromCol),
    {CToLine, CToCol} = 
        refactor:get_true_pos_from_pointed_pos(MId, ToLine, ToCol),
    FromId = refac_common:get_lowest_id(
               MId, refactor:get_id_from_pos(MId, CFromLine, CFromCol)),
    ToId   = refac_common:get_lowest_id(
               MId, refactor:get_id_from_pos(MId, CToLine, CToCol)),
    RootClause = get_root_clause(MId, refactor:get_scope_from_id(MId, FromId)),
    refac_common:find_expression_root_id(MId, RootClause, FromId, ToId).
    
%% =====================================================================
%% @spec convert_record_names(RecordParamsString::string()) 
%%                         -> [string()]
%%
%% @doc
%% Get the record field names from the string. 
%% 
%% Parameter description:<pre>
%% <b>RecordParamsString</b> : The name of the record fields.
%% </pre>
%% @end
%% =====================================================================
convert_record_names(RecordParamsString) ->
    produce_record_names([],RecordParamsString).

%% =====================================================================
%% @spec produce_record_names(RecordParamNames::[string()], 
%%                      String::string()) 
%%                              -> [string()]
%%
%% @doc
%% Produce the record field names from the string. 
%% 
%% Parameter description:<pre>
%% <b>RecordParamNames</b> : The already produced record field names.
%% <b>String</b> : The name of the record fields.
%% </pre>
%% @end
%% =====================================================================
produce_record_names(RecordParamNames, String) ->
    case io_lib:fread("~s", String) of
        {ok, [[]], []} ->
            lists:reverse(RecordParamNames);
        {more, "~s", 0, []} ->
            lists:reverse(RecordParamNames);
        {ok, [ParamName], Rest} ->
            produce_record_names([ParamName | RecordParamNames], Rest);
        Error ->
            refac_checks:
                error_handler(
                  {invalid_paramname, Error})
    end.

%% =====================================================================
%% @spec get_root_clause(MId::integer(), 
%%                      Id::integer()) 
%%                              -> integer()
%%
%% @doc
%% Get the selected node's root clause. 
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
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
%% @spec get_function_id(MId::integer(), 
%%                      Id::integer()) 
%%                              -> integer()
%%
%% @doc
%% Get the selected clause's function id. 
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause.
%% </pre>
%% @end
%% =====================================================================
get_function_id(MId, Id) ->
    element(1,hd(refactor:get_fun_id_from_clause_id(MId, Id))).

%% =====================================================================
%% @spec get_match_expr_pattern(MId::integer(), 
%%                      PathFromRootClause::[integer()]) 
%%                              -> nothing | {just, integer()}
%%
%% @doc
%% Check if the selected tuple is the right-hand side of a match 
%% expression. Get the left-hand side if it is a single variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>PathFromRootClause</b> : The path between the Root clause and 
%%                             selected tuple.
%% </pre>
%% @end
%% =====================================================================
get_match_expr_pattern(MId, PathFromRootClause) ->
    ExprParent = hd(tl(lists:reverse(PathFromRootClause))),
    ParentType = erl_syntax_db:type(MId, ExprParent),
    if ParentType =:= ?MATCH_EXPR ->
            Pattern = erl_syntax_db:match_expr_pattern(MId, ExprParent),
            PatternType = erl_syntax_db:type(MId, Pattern),
            if PatternType =:= ?VARIABLE ->
                    {just, Pattern};
               true ->
                    nothing
            end;
       true ->
            nothing
    end.

%% =====================================================================
%% @spec get_all_the_tuples(MId::integer(), 
%%                      Clause::integer()) 
%%                              -> [integer()]
%%
%% @doc
%% Get all the tuples in the Clause.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Clause</b> : Id of the clause.
%% </pre>
%% @end
%% =====================================================================
get_all_the_tuples(MId, Clause) ->
    Scopes = refac_common:get_inner_scope(MId, Clause),
    refactor:get_nodes(MId, Scopes,?TUPLE).
 
%% =====================================================================
%% @spec get_the_same_ones_as_the_selected_tuple(MId::integer(), 
%%                      ExprId::integer(), TupleIds::[integer()]) 
%%                              -> [integer()]
%%
%% @doc
%% Filter out all the tuples which are not equal to the selected tuple.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ExprId</b> : Id of the selected tuple.
%% <b>TupleIds</b> : Tuple ids.
%% </pre>
%% @end
%% =====================================================================
get_the_same_ones_as_the_selected_tuple(MId, ExprId, TupleIds) ->
    TupleElements = refactor:get_tuple_element_ids_from_tuple_id(MId, ExprId),
    lists:filter(
      fun(TupleId) ->
              TupleIdsElements = 
                  refactor:get_tuple_element_ids_from_tuple_id(MId, TupleId),
              case length(TupleElements) =:= length(TupleIdsElements) of
                  true ->
                      all_equal(
                        MId, lists:zipwith(fun ({X}, {Y}) -> {X,Y} end, 
                                           TupleElements, TupleIdsElements));
                  false ->
                      false
              end
      end, TupleIds).

%% =====================================================================
%% @spec all_equal(MId::integer(), 
%%                      Elements::[{integer(), integer()}]) 
%%                              -> bool()
%%
%% @doc
%% Check if all the elements of the list are equal.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Elements</b> : List of node id pairs.
%% </pre>
%% @end
%% =====================================================================
all_equal(_MId, []) ->
    true;
all_equal(MId, [X | Xs]) ->
    case equal(MId, X) of
        true -> 
            all_equal(MId, Xs);
        false ->
            false
    end.

%% =====================================================================
%% @spec equal(MId::integer(), 
%%             Element::{integer(), integer()}) 
%%                              -> bool()
%%
%% @doc
%% Check if two nodes are equal. 
%% For example: two nodes are the same if they have the same id, 
%% and two variable nodes are the same if their first occurrence 
%% is the same.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Element</b> : Node id pairs.
%% </pre>
%% @end
%% =====================================================================
equal(_MId, {X,X}) ->
    true;
equal(MId, {X, Y}) ->
    XType = erl_syntax_db:type(MId, X),
    YType = erl_syntax_db:type(MId, Y),
    if XType =:= YType andalso XType =:= ?INTEGER ->
            erl_syntax_db:integer_value(MId, X) =:= 
                erl_syntax_db:integer_value(MId, Y);
       XType =:= YType andalso XType =:= ?ATOM ->
            erl_syntax_db:atom_name(MId, X) =:= 
                erl_syntax_db:atom_name(MId, Y);
       XType =:= YType andalso XType =:= ?CHAR ->
            erl_syntax_db:char_value(MId, X) =:= 
                erl_syntax_db:char_value(MId, Y);
       XType =:= YType andalso XType =:= ?FLOAT ->
            erl_syntax_db:float_value(MId, X) =:= 
                erl_syntax_db:float_value(MId, Y);            
       XType =:= YType andalso XType =:= ?VARIABLE ->
            refactor:get_var_bind_occ_from_id(MId, X) =:=
                refactor:get_var_bind_occ_from_id(MId, Y);
       true ->
            false
    end.

%% =====================================================================
%% @spec check_record_name_not_used(MId::integer(), 
%%             RecordName::string()) 
%%                              -> used | not_used
%%
%% @doc
%% Check if the given recordname is used in the module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
check_record_name_not_used(MId, RecordName) ->
    case refactor:get_record_definition_ids(MId) of
        [] ->
            not_used;
        RecordIds ->
            case lists:member(
                   RecordName, 
                   lists:map(fun (RecordId) -> 
                                    refactor:get_record_name(MId, RecordId) end,
                             RecordIds)) of
                true -> used;
                false -> not_used
            end
    end.
        
%% @type surdata() = {surdata, A}
%%           A = term().
%%   The surdata record.

%% @type set().
%%   as returned by sets:new/0.


%% =====================================================================
%% @spec get_tuples_surrounding(MId::integer(), 
%%             SameTupleIds::[integer()], RecordName::string()) 
%%                              -> {[surdata()], [integer()], bool()}
%%
%% @doc
%% Gets the tuples surrounding required for the refactoring.
%% The structure returned is the surdata record.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>SameTupleIds</b> : List of tuple ids.
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
get_tuples_surrounding(MId, SameTupleIds, RecordName) ->
    SameTupleIdsWithSurround = 
        lists:map(
          fun (TupleId) -> get_tuple_surrounding(MId, TupleId, RecordName) end,
          SameTupleIds),
    {SameTupleIdsWithSurround, is_in_pattern(SameTupleIdsWithSurround), 
     is_last_body_element(SameTupleIdsWithSurround)}.

%% =====================================================================
%% @spec get_tuple_surrounding(MId::integer(), 
%%             TupleId::integer(), RecordName::string()) 
%%                              -> surdata()
%%
%% @doc
%% Gets the tuple surrounding required for the refactoring.
%% The structure returned is the surdata record.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>SameTupleIds</b> : A tuple id.
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
get_tuple_surrounding(MId, TupleId, RecordName) ->
    Elements = erl_syntax_db:untuple(
                 refactor:get_tuple_element_ids_from_tuple_id(MId, TupleId)),
    RootClause = get_root_clause(MId, refactor:get_scope_from_id(MId, TupleId)),
    {found_expr, PathFromRootClause, TupleId} =
        refac_common:find_expression_root_id(
          MId, RootClause, TupleId, lists:last(Elements)),
    get_surround_data(MId, TupleId, RootClause, PathFromRootClause, RecordName).

%% =====================================================================
%% @spec in_pattern(Data::surdata(), 
%%             Id::integer(), PatternIds::[integer()]) 
%%                              -> surdata()
%%
%% @doc
%% Checks whether the given id is in the pattern of the clause.
%% 
%% Parameter description:<pre>
%% <b>Data</b> : Surround data of a node.
%% <b>Id</b> : A node id.
%% <b>PatternIds</b> : A list of the clause's pattern id.
%% </pre>
%% @end
%% =====================================================================
in_pattern(Data, Id, PatternIds) ->
    case my_member(Id, PatternIds, 1) of
        false ->
            Data;
        Pos ->
            Data#surdata{pattern=Pos}
    end.

%% =====================================================================
%% @spec my_member(Element::term(), 
%%             List::[term()], Start::integer()) 
%%                              -> false | integer()
%%
%% @doc
%% Checks if Element is a member of the List. If it is it returns the 
%% position of it starting the count from Start. It it is not a member 
%% of the list it returns false.
%% 
%% Parameter description:<pre>
%% <b>Element</b> : An erlang term.
%% <b>List</b> : List of erlang terms.
%% <b>Start</b> : The position of the first element in the list.
%% </pre>
%% @end
%% =====================================================================
my_member(_Id, [], _Num) ->
    false;
my_member(Id, [Id | _Xs], Num) ->
    Num;
my_member(Id, [_X | Xs], Num) ->
    my_member(Id, Xs, Num+1).
        
%% =====================================================================
%% @spec is_own_macro(MId::integer(), 
%%             Id::integer(), RecordName::string()) 
%%                              -> atom()
%%
%% @doc
%% Checks if the macro call is generated by the refactoring tool during 
%% a previous refactoring.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of a macro call.
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
is_own_macro(MId, Id, RecordName) ->
    MacroName = refactor:get_name_from_name_id(
                  MId, erl_syntax_db:macro_name(MId, Id)),
    FromMacroName = "from_" ++ RecordName,
    ToMacroName = "to_" ++ RecordName,
    if FromMacroName =:= MacroName ->
            from_record;
       ToMacroName =:= MacroName ->
            to_record;
       true ->
            false
    end.

%% =====================================================================
%% @spec is_in_pattern(ExprIdsWithSurround::[surdata()]) 
%%                              -> [integer()]
%%
%% @doc
%% Gets which positions of the pattern has to be modified in the 
%% definition and in the function calls.
%% 
%% Parameter description:<pre>
%% <b>ExprIdsWithSurround</b> : List of expressions with surround data.
%% </pre>
%% @end
%% =====================================================================
is_in_pattern(ExprIdsWithSurround) ->
    is_in_pattern(ExprIdsWithSurround, []).
    
%% =====================================================================
%% @spec is_in_pattern(ExprIdsWithSurround::[surdata()], 
%%                     PatternPos::[integer()]) 
%%                              -> [integer()]
%%
%% @doc
%% Gets which positions of the pattern has to be modified in the 
%% definition and in the function calls as well.
%% 
%% Parameter description:<pre>
%% <b>ExprIdsWithSurround</b> : List of expressions with surround data.
%% <b>PatternPos</b> : Prefix of the positions where the pattern 
%%                     has to be modified.
%% </pre>
%% @end
%% =====================================================================
is_in_pattern([], PatternPos) ->
    PatternPos;
is_in_pattern([#surdata{pattern=false} | Xs], PatternPos) ->
    is_in_pattern(Xs, PatternPos);
is_in_pattern([#surdata{pattern=Pos} | Xs], PatternPos) ->
    is_in_pattern(Xs, [Pos | PatternPos]).

%% =====================================================================
%% @spec is_last_body_element(ExprIdsWithSurround::[surdata()]) 
%%                              -> [integer()]
%%
%% @doc
%% Checks whether one of the expressions which has to be modified is the 
%% last expression in the transformed clause.
%% 
%% Parameter description:<pre>
%% <b>ExprIdsWithSurround</b> : List of expressions with surround data.
%% </pre>
%% @end
%% =====================================================================
is_last_body_element([]) ->
    false;
is_last_body_element([#surdata{lbody=true} | _Xs]) ->
    true;
is_last_body_element([_X | Xs]) ->
    is_last_body_element(Xs).

%% =====================================================================
%% @spec get_variables_surrounding(MId::integer(), 
%%             VariableId::Maybe, RecordName::string()) 
%%                       ->  nothing | {just, [surdata()], [integer()]}
%%            Maybe = nothing | {just, integer()}
%%
%% @doc
%% Gets the variables' surrounding required for the refactoring.
%% The structure returned is the surdata record.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VariableId</b> : Id of the transformed variable. If exists one 
%%                     which has to be modified.
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
get_variables_surrounding(_MId, nothing, _RecordName) ->
    nothing;
get_variables_surrounding(MId, {just, VarId}, RecordName) ->
    {BindId, _ScopeId} = 
        refactor:get_var_bind_occ_and_scope_from_id(MId, VarId),
    VarIds = refactor:get_every_occurrence_of_a_var_from_id(MId, BindId),
    MaybeVars = 
        lists:map(
          fun ({VariableId}) -> 
                  get_variable_surrounding(MId, VariableId, RecordName) end, 
          VarIds),
    {just, {MaybeVars, is_in_pattern(MaybeVars), 
            is_last_body_element(MaybeVars)}}.

%% =====================================================================
%% @spec get_variable_surrounding(MId::integer(), 
%%             VariableId::integer(), RecordName::string()) 
%%                       ->  [surdata()]
%%
%% @doc
%% Gets the variable's surrounding required for the refactoring.
%% The structure returned is the surdata record.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VariableId</b> : Id of the variable.
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
get_variable_surrounding(MId, VariableId, RecordName) ->
    RootClause = get_root_clause(
                   MId, refactor:get_scope_from_id(MId, VariableId)),
    {found_expr, PathFromRootClause, VariableId} =
        refac_common:find_expression_root_id(
          MId, RootClause, VariableId, VariableId),
    get_surround_data(
      MId, VariableId, RootClause, PathFromRootClause, RecordName).

%% =====================================================================
%% @spec get_surround_data(MId::integer(), 
%%             ExprId::integer(), RootClause::integer(),
%%             PathFromRootClaue::[integer()], RecordName::string()) 
%%                       ->  [surdata()]
%%
%% @doc
%% Gets the exrpession's surrounding required for the refactoring.
%% The structure returned is the surdata record.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ExprId</b> : Id of the expression.
%% <b>RootClause</b> : Id of the expression's root clause.
%% <b>PathFromRootClause</b> : The path between the Root clause and 
%%                             selected expression.
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
get_surround_data(MId, ExprId, RootClause, PathFromRootClause, RecordName) ->
    PatternIds = refactor:get_pattern_ids(MId, RootClause),
    BodyIds = erl_syntax_db:clause_body(MId, RootClause),
    LastBody = lists:last(BodyIds),
    bodys_last_element(              
      lists:foldl(
        fun(Id, Data) ->
                Data1 = in_pattern(Data, Id, PatternIds),
                case erl_syntax_db:type(MId, Id) of
                    ?MACRO -> case is_own_macro(MId, Id, RecordName) of
                                  from_record ->  
                                      Data1#surdata{macro={from_record, Id}};
                                  to_record ->  
                                      Data1#surdata{macro={to_record, Id}};
                                  false -> 
                                      Data1#surdata{macro=true}
                              end;
                    ?APPLICATION -> Data1#surdata{application={true, Id}};
                    ?INFIX_EXPR -> case is_in_message_send(MId, Id) of
                                       true ->
                                           Data1#surdata{msend=true};
                                       false ->
                                           Data1
                                   end;
                    _Other ->
                        Data1
                end
        end,
        #surdata{mid=MId, id=ExprId}, PathFromRootClause), ExprId, LastBody).

%% =====================================================================
%% @spec bodys_last_element(ExprIdWithSurround::surdata(), 
%%                ExprId::integer(), LastBody::integer()) 
%%                              -> surdata()
%%
%% @doc
%% Checks whether the expression which has to modified is the 
%% last expression in the transformed clause.
%% 
%% Parameter description:<pre>
%% <b>ExprIdWithSurround</b> : List of expressions with surround data.
%% <b>ExprId</b> : Id of the expression id.
%% <b>LastBodyId</b> : Id of the last body element.
%% </pre>
%% @end
%% =====================================================================
bodys_last_element(Data1, Id, Id) ->
    Data1#surdata{lbody=true};
bodys_last_element(Data1, _Id, _LastBody) ->
    Data1.

%% =====================================================================
%% @spec is_in_message_send(MId::integer(), Id::integer()) 
%%                              -> bool()
%%
%% @doc
%% Checks whether the expression which has to modified is  
%% in a message send.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of an infix expression.
%% </pre>
%% @end
%% =====================================================================
is_in_message_send(MId, Id) ->
    "!" =:= erl_syntax_db:infix_expr_operator(MId, Id).

%% =====================================================================
%% @spec get_function_calls(MId::integer(), FunId::integer(),
%%                          InPattern::[integer()], LastBodyElement::bool(),
%%                          MaybeVars::Maybe, RecordName::string()) 
%%          -> Result
%%         Maybe = nothing | {just, {[surdata()], [integer()], bool()}}
%%         Result = nothing | {[{integer(), integer(), integer()}],
%%                        [{integer(), integer(), integer()}],integer(),
%%                         [{integer(), Used}]}
%%         Used = not_used | used
%%
%% @doc
%% Get the function calls which call the function modifed by 
%% the refactoring. The result is nothing if the modifications do not
%% require to modify the function calls. 
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : Id of the function.
%% <b>InPattern</b> : Which position of the function's pattern has 
%%                    to be modified.
%% <b>LastBodyElement</b> : Is one of the refactored expression the 
%%                          modified clause's last element.
%% <b>MaybeVars</b> : The data of the variables which has to be modified. 
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
get_function_calls(_MId, _FunId, [], false, MaybeVars, _RecordName)  
  when is_atom(MaybeVars);
       is_tuple(MaybeVars), element(2, element(2, MaybeVars)) =:= [],
       element(2, element(3, MaybeVars)) =:= false ->
    nothing;
get_function_calls(MId, FunId, _InPattern, _LastBodyElement, _MaybeVars, 
                   RecordName) ->
%%ApplicationIds {MId, Id, Type}
%%ImplicitFunCallandTypeIds {Type, MId, Id}
    {ApplicationIds, _ApplicationTypeandNameIds,
     _ImplicitFunCallIds, ImplicitFunCallandTypeIds,
     _ImplicitFunCallTypeandNameIds} = refac_common:get_fun_calls(MId, FunId),
    MIds = get_affected_module_ids(ApplicationIds, ImplicitFunCallandTypeIds, 
                            sets:new()),
    {ApplicationIds, ImplicitFunCallandTypeIds, FunId, 
     lists:map(
       fun (MId2) -> 
               Used = check_record_name_not_used(MId2, RecordName), 
               {MId2, Used} end, 
       MIds -- [MId])}.

%% =====================================================================
%% @spec get_affected_module_ids(
%%      ApplicationIds::[{integer(), integer(), integer()}], 
%%      ImplicitFunCallandTypeIds::[{integer(), integer(), integer()}], 
%%      Set::set())-> [{integer(), Used}]
%%
%%         Used = not_used | used
%%
%% @doc
%% Get the modules where there are function calls which has to be modified. 
%% 
%% Parameter description:<pre>
%% <b>ApplicationIds</b> : Application functions calls.
%% <b>ImplicitFunCallandTypeIds</b> : Implicit function calls.
%% <b>Set</b> : A set. As it is described in stdlib sets module.
%% </pre>
%% @end
%% =====================================================================
get_affected_module_ids([], [], Set) ->
    sets:to_list(Set);
get_affected_module_ids([{MId, _Id, _Type} | Xs], 
                        ImplicitFunCallandTypeIds, Set) ->
    get_affected_module_ids(
      Xs, ImplicitFunCallandTypeIds, sets:add_element(MId, Set));
get_affected_module_ids([], [{_Type, MId, _Id} | Xs], Set) ->
    get_affected_module_ids([], Xs, sets:add_element(MId, Set)).
    
%% =====================================================================
%% @spec perform_refactoring(MId::integer(), MaybeVars::Maybe,
%%      TupleData::{SameTupleIdsWithSurround, InPattern, LastBodyElement},
%%      FunctionCalls::CallsData, RecordName::string(), 
%%      RecordParams::[string()], Used::Used) 
%%          -> ok
%%
%%       SameTupleIdsWithSurround = [surdata()]
%%       InPattern = [integer()]
%%       LastBodyElement = bool()
%%       Used = used | not_used
%%       Maybe = nothing | {just, {[surdata()], [integer()], bool()}}
%%       CallsData = nothing | {[{integer(), integer(), integer()}],
%%                       [{integer(), integer(), integer()}],integer(), 
%%                       [{integer(), Used}]}
%%
%% @doc
%% Perform the refactoring based on the previously collected data.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>MaybeVars</b> : The data of the variables which has to be modified. 
%% <b>TupleData</b> : The data of the tuples which has to be modified. 
%% <b>FunctionCalls</b> : The data of the function calls which has to 
%%                        be modified. 
%% <b>RecordName</b> : A record name.
%% <b>RecordParams</b> : The name of the record fields.
%% <b>Used</b> : Is the record definition in the module.
%% </pre>
%% @end
%% =====================================================================
perform_refactoring(MId, MaybeVars, 
                    {SameTupleIdsWithSurround, InPattern, LastBodyElement},
                    FunctionCalls, RecordName, RecordParams, Used) ->
    convert_variables(MaybeVars, RecordName),
    convert_tuples(SameTupleIdsWithSurround, RecordName, RecordParams),
    convert_function_calls(
      MId, FunctionCalls, RecordName, RecordParams, MaybeVars, 
      InPattern, LastBodyElement),
    if FunctionCalls =/= nothing ->
            lists:map(
              fun({MId2,Used2}) ->
                      create_record_definition(
                        MId2, RecordName, RecordParams, Used2) end,
              element(4,FunctionCalls));
       true ->
            ok
    end,
    create_record_definition(MId, RecordName, RecordParams, Used).

%% =====================================================================
%% @spec convert_variables(MaybeVars::Maybe, RecordName::string()) 
%%          -> ok
%%
%%       Maybe = nothing | {just, {[surdata()], [integer()], bool()}}
%%
%% @doc
%% Transform the variable occurrences.
%% 
%% Parameter description:<pre>
%% <b>MaybeVars</b> : The data of the variables which has to be modified. 
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
convert_variables(nothing, _RecordName) ->
    ok;
convert_variables(
  {just, {VarsWithSurround, _InPattern, _LastBodyElement}}, RecordName) ->
    lists:map(
      fun(VarWithSurround) -> 
              convert_variable(VarWithSurround, RecordName) end, 
      VarsWithSurround).

%% =====================================================================
%% @spec convert_variable(VarWithSurround::surdata(), RecordName::string()) 
%%          -> ok
%%
%% @doc
%% Transform the variable occurrence.
%% 
%% Parameter description:<pre>
%% <b>VarWithSurround</b> : The data of the variable which has to be modified. 
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
convert_variable(#surdata{macro=Macro, application=Application, msend=MSend,
                          mid=MId, id=Id}, RecordName) ->
    if is_tuple(Application) and is_tuple(Macro) ->
            remove_macro_if_possible(
              MId, (element(2, Application)), 
              (element(2, Macro)), Id, RecordName);
       is_tuple(Application) or (MSend =:= true) or (Macro =:= true) -> 
            insert_conversion_macro(MId, Id, "from_" ++ RecordName, variable);
       true ->
            ok
    end.

%% =====================================================================
%% @spec remove_macro_if_possible(MId::integer(), AppId::integer(),
%%                                MacroId::integer(), VarId::integer(),
%%                                RecordName::string())-> ok
%%
%% @doc
%% Remove the macro from the variable if it is possible to do it, and it 
%% is not needed anymore.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>AppId</b> : Id of an application.
%% <b>MacroId</b> : Id of a macro.
%% <b>VarId</b> : Id of a variable.
%% <b>RecordName</b> : A record name.
%% </pre>
%% @end
%% =====================================================================
remove_macro_if_possible(MId, AppId, MacroId, VarId, RecordName) ->
    AppArguments = erl_syntax_db:application_arguments(MId, AppId),
    MacroArguments = erl_syntax_db:macro_arguments(MId, MacroId),
    case lists:member(MacroId, AppArguments) and 
        lists:member(VarId, MacroArguments) of
        true ->
            case my_member(MacroId, AppArguments, 0) of
                0 ->
                    After = erl_syntax_db:application_operator(MId, AppId);
                Pos ->
                    After = lists:nth(Pos, AppArguments)
            end,
            delete_conversion_macro(MId, AppId, MacroId, After);
        false ->
            insert_conversion_macro(
              MId, VarId, "from_" ++ RecordName, variable)
    end.
        
%% =====================================================================
%% @spec insert_conversion_macro(MId::integer(), Id::integer(),
%%                                MacroName::string(), Type::atom())-> ok
%%
%% @doc
%% Insert converion macro on the selected node.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of a node.
%% <b>MacroName</b> : A macro name.
%% <b>Type</b> : Type of the selected node.
%% </pre>
%% @end
%% =====================================================================
insert_conversion_macro(MId, Id, MacroName, variable) ->
    NewId = create_nodes:move_node(MId, Id), 
    NameId = create_nodes:create_atom(MId, MacroName),
    create_nodes:create_macro(MId, NameId, [NewId], Id),
    create_nodes:init_scope(
      MId, refactor:get_scope_from_id(MId, NewId), [NameId, Id]).


%% =====================================================================
%% @spec convert_tuples(SameTupleIdsWithSurround::[surdata()], 
%%                      RecordName::string(), 
%%                      RecordParams::[string()]) 
%%          -> ok
%%
%% @doc
%% Transform the tuples.
%% 
%% Parameter description:<pre>
%% <b>SameTupleIdsWithSurround</b> : The data of the tuples which has 
%%                                   to be modified. 
%% <b>RecordName</b> : A record name.
%% <b>RecordParams</b> : The name of the record fields.
%% </pre>
%% @end
%% =====================================================================
convert_tuples(SameTupleIdsWithSurround, RecordName, RecordParams) ->
    lists:map(
      fun(TupleWithSurround) -> 
              convert_tuple(TupleWithSurround, RecordName, RecordParams) end,
     SameTupleIdsWithSurround).

%% =====================================================================
%% @spec convert_tuple(TupleWithSurround::surdata(), 
%%                      RecordName::string(), 
%%                      RecordParams::[string()]) 
%%          -> ok
%%
%% @doc
%% Transform the tuple.
%% 
%% Parameter description:<pre>
%% <b>TupleWithSurround</b> : The data of the tuple which has 
%%                            to be modified. 
%% <b>RecordName</b> : A record name.
%% <b>RecordParams</b> : The name of the record fields.
%% </pre>
%% @end
%% =====================================================================
convert_tuple(#surdata{mid=MId, id=Id, macro=Macro, 
                       application=Application, msend = MSend}, 
              RecordName, RecordParams) ->
    TupleElementId = 
        refactor:get_tuple_element_ids_from_tuple_id(MId, Id),
    lists:map(
      fun({TId}) -> delete_nodes:detach_node(MId, TId, Id) end, 
      TupleElementId),
    Scope = refactor:get_scope_from_id(MId, Id),
    delete_nodes:delete_node(MId, Id),
    FieldNames = lists:map(
                   fun(RecordParam) ->
                           create_nodes:create_atom(MId, RecordParam) end, 
                   RecordParams),
    RecordFieldElementIds = 
        lists:zipwith(fun({TElementId}, FieldName) ->
                              {FieldName, TElementId} end,
                      TupleElementId, FieldNames),
    RecordFields = 
        lists:map(
          fun({FieldName, TId}) ->
                  create_nodes:create_record_field(
                    MId, FieldName, TId) end, 
          RecordFieldElementIds),
    RecordType = create_nodes:create_atom(MId, RecordName),
    create_nodes:init_scope(
      MId, Scope, [RecordType] ++ FieldNames ++ RecordFields),
    if is_tuple(Application) or MSend or Macro ->
            RecordExpr = create_nodes:create_record_expr(
                           MId, none, RecordType, RecordFields),
            NameId = create_nodes:create_atom(MId, "from_" ++ RecordName),
            create_nodes:create_macro(MId, NameId, [RecordExpr], Id),
            create_nodes:init_scope(
              MId, Scope, [RecordExpr, NameId, Id]);
       true ->
            create_nodes:create_record_expr(
              MId, none, RecordType, RecordFields, Id),
            create_nodes:init_scope(
              MId, Scope, [Id])
    end.

%% =====================================================================
%% @spec convert_function_calls(MId::integer(), FunctionCalls::CallsData, 
%%                        RecordName::string(), RecordParams::[string()], 
%%                        MaybeVars::Maybe, InPattern::[integer()], 
%%                        LastBodyElementTuple::bool()) 
%%          -> ok
%%
%%       CallsData = nothing | {[{integer(), integer(), integer()}],
%%                       [{integer(), integer(), integer()}],integer(),
%%                       [{integer(), Used}]}
%%       Maybe = nothing | {just, {[surdata()], [integer()], bool()}}
%%       Used = not_used | used
%%
%% @doc
%% Transform the function calls.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunctionCalls</b> : The data of the function calls which has to 
%%                        be modified. 
%% <b>RecordName</b> : A record name.
%% <b>RecordParams</b> : The name of the record fields.
%% <b>MaybeVars</b> : The data of the variables which has to be modified.
%% <b>InPattern</b> : Which position of the function's pattern has 
%%                    to be modified.
%% <b>LastBodyElement</b> : Is one of the refactored expression the 
%%                          modified clause's last element.
%% </pre>
%% @end
%% =====================================================================
convert_function_calls(
  _MId, nothing, _RecordName, _RecordParams, _MaybeVars, _InPattern, 
  _LastBodyElement) ->
    ok;            
convert_function_calls(
  MId, {ApplicationIds, ImplicitFunCallandTypeIds, FunId, _MIds}, RecordName, 
  RecordParams, MaybeVars, InPattern, LastBodyElementTuple) ->
    case MaybeVars of
        nothing ->
            PatternPos = lists:usort(InPattern),
%%DEBUG            io:format("~p ~n", [LastBodyElementTuple]),
            LastBodyElement = LastBodyElementTuple;
        {just, {_VarsWithSurround, VarPattern, LastBodyElementVar}} ->
            PatternPos = lists:usort(InPattern ++ VarPattern),
%%DEBUG            io:format("~p ~p ~n", [LastBodyElementTuple, LastBodyElementVar]),
            LastBodyElement = LastBodyElementTuple or LastBodyElementVar
    end,
    if (PatternPos =:= []) and (LastBodyElement =:= false) ->
            ok;
       true ->
            NewApplicationsIds = 
                convert_implicit_funs(MId, FunId, ImplicitFunCallandTypeIds),
            lists:map(
              fun (Pos) -> 
                      lists:map(
                        fun ({MId2, AppId, _Type}) ->
                                convert_function_call(
                                  MId2, AppId, RecordName, RecordParams, Pos) 
                        end, ApplicationIds ++ NewApplicationsIds) 
              end, PatternPos),                
            lists:map(
              fun ({MId2, AppId, _Type}) ->
                      convert_function_call(
                        MId2, AppId, RecordName, LastBodyElement) 
              end, ApplicationIds ++ NewApplicationsIds)
    end.

%% =====================================================================
%% @spec convert_implicit_funs(MId::integer(), FunId::integer(), 
%%            ImplicitFunCallandTypeIds::[{integer(), integer(), integer()}]) 
%%          -> ok
%%
%% @doc
%% Transform the implicit function calls into applications.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FuncId</b> : Id of the function. 
%% <b>RecordName</b> : A record name.
%% <b>ImplicitFunCallandTypeIds</b> : The implicit function calls data.
%% </pre>
%% @end
%% =====================================================================
convert_implicit_funs(MId, FunId, ImplicitFunCallandTypeIds) ->
    Arity = refactor:get_arity_from_fun_id(MId, FunId),
    lists:map(
      fun({Type, MId2, ImplicitId}) ->
              ScopeId = refactor:get_scope_from_id(MId2, ImplicitId),
              delete_nodes:delete_node(MId2, ImplicitId),
              {Name,Num} = create_nodes:create_fresh_variable(MId2),
              FormParList = create_nodes:create_variables(
                              MId2, Name, Num, Arity),
              ApplicationParList = create_nodes:create_variables(
                                     MId2, Name, Num, Arity),
              FunNameId = 
                  create_nodes:create_atom(
                    MId2, refactor:get_fun_name(MId, FunId)), 
              {ApplicationId, Additional} =
                  case Type of
                      ?ARITY_QUALIFIER ->
                          {create_nodes:create_application(
                             MId2, FunNameId, ApplicationParList),
                           []};
                      ?MODULE_QUALIFIER ->
                          ModuleNameId = 
                              create_nodes:create_atom(
                                MId2, refactor:get_module_name(MId)),
                          ModuleQualifierId = 
                              create_nodes:create_module_qualifier(
                                MId2, ModuleNameId, FunNameId),
                          {create_nodes:create_application(
                             MId2, ModuleQualifierId, ApplicationParList),
                           [ModuleNameId, ModuleQualifierId]}
                  end,
              create_nodes:connect_fun_call(MId2, ApplicationId, MId, FunId),
              ClauseId = 
                  create_nodes:create_clause(
                    MId2, FormParList, none, [ApplicationId]),
              create_nodes:create_fun_expr(MId2, [ClauseId], ImplicitId),
              create_nodes:init_scope(
                MId2, ClauseId, FormParList ++ ApplicationParList ++
                [FunNameId] ++ Additional ++ [ApplicationId, ClauseId]),
              create_nodes:init_scope(MId2, ScopeId, [ImplicitId]),
              create_nodes:create_scope(MId2, ClauseId, ScopeId),
              create_nodes:connect_variables(MId2, FormParList, FormParList),
              create_nodes:connect_variables(
                MId2, FormParList, ApplicationParList),
              {MId2, ApplicationId, not_used}
      end, ImplicitFunCallandTypeIds).

%% =====================================================================
%% @spec convert_function_call(MId::integer(), AppId::integer(), 
%%                        RecordName::string(), RecordParams::[string()], 
%%                        Pos::integer()) 
%%          -> ok
%%
%% @doc
%% Transform the function call(application).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>AppId</b> : Id of the application. 
%% <b>RecordName</b> : A record name.
%% <b>RecordParams</b> : The name of the record fields.
%% <b>Pos</b> : The pattern position where the modification 
%%              will take place.
%% </pre>
%% @end
%% =====================================================================
convert_function_call(MId, AppId, RecordName, RecordParams, Pos) ->
    Parameters = erl_syntax_db:application_arguments(MId, AppId),
    Parameter = lists:nth(Pos, Parameters),
    if Pos =:= 1 ->
            After = erl_syntax_db:application_operator(MId, AppId);
       true ->
            After = lists:nth(Pos - 1, Parameters)
    end,
    FromMacro = "from_" ++ RecordName,
    ToMacro = "to_" ++ RecordName,
    case erl_syntax_db:type(MId, Parameter) of
        ?MACRO ->
            case refactor:get_name_from_name_id(
                   MId, erl_syntax_db:macro_name(MId, Parameter)) of
                FromMacro ->
                    delete_conversion_macro(MId, AppId, Parameter, After);
                ToMacro ->
                    ok;%%delete_conversion_macro(MId, AppId, Parameter, After);
                _ ->
                    insert_conversion_macro_between(
                      MId, AppId, ToMacro, Parameter, After)
            end;
        ?TUPLE ->
            convert_tuple(
              #surdata{mid=MId, id=Parameter}, RecordName, RecordParams);
        _ ->
            insert_conversion_macro_between(
              MId, AppId, ToMacro, Parameter, After)
    end.

%% =====================================================================
%% @spec convert_function_call(MId::integer(), AppId::integer(), 
%%                        RecordName::string(), LastBodyElement::bool()) 
%%          -> ok
%%
%% @doc
%% Transform the function call(application). It depends on whether the 
%% clauses last element has been modified.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>AppId</b> : Id of the application. 
%% <b>RecordName</b> : A record name.
%% <b>LastBodyElement</b> : Is the transformed clause last element 
%%                          refactored.
%% </pre>
%% @end
%% =====================================================================
convert_function_call(_MId, _AppId, _RecordName, false) ->
%%DEBUG    io:format("LastBody == false", []);    
    ok;
convert_function_call(MId, AppId, RecordName, true) ->
%%DEBUG    io:format("LastBody == true", []),
    Scope = refactor:get_scope_from_id(MId, AppId),
    NewId = create_nodes:move_node(MId, AppId),
    NameId = create_nodes:create_atom(MId, "from_" ++ RecordName),
    create_nodes:create_macro(MId, NameId, [NewId], AppId),
    create_nodes:init_scope(
      MId, Scope, [NameId]).

%% =====================================================================
%% @spec delete_conversion_macro(MId::integer(), Root::integer(), 
%%                        Child::integer(), After::integer()) 
%%          -> ok
%%
%% @doc
%% Delete the conversion macro from the argument. 
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Root</b> : The id of the node which is the parent of the 
%%               macro call. 
%% <b>Child</b> : The id of the node which is the argument of the 
%%               macro call. 
%% <b>After</b> : The id of the node which will be the left sibling 
%%               of the macro calls argument. 
%% </pre>
%% @end
%% =====================================================================
delete_conversion_macro(MId, Root, Child, After) ->
    delete_nodes:detach_node(MId, Child, Root),
    Argument = erl_syntax_db:macro_arguments(MId, Child),
    delete_nodes:detach_node(MId, hd(Argument), Child),
    delete_nodes:delete_node(MId, Child),
    create_nodes:attach_subtree_to_node(MId, hd(Argument), Root, After).

%% =====================================================================
%% @spec insert_conversion_macro_between(MId::integer(), Root::integer(), 
%%                        MacroName::string(),
%%                        Child::integer(), After::integer()) 
%%          -> ok
%%
%% @doc
%% Insert conversion macro on a node. 
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Root</b> : The id of the node which will be the parent of the 
%%               macro call. 
%% <b>MacroName</b> : A macro name.
%% <b>Child</b> : The id of the node which will be the argument of the 
%%               macro call. 
%% <b>After</b> : The id of the node which will be the left sibling 
%%               of the macro call. 
%% </pre>
%% @end
%% =====================================================================
insert_conversion_macro_between(
  MId, Root, MacroName, Child, After) ->
    delete_nodes:detach_node(MId, Child, Root),
    NameId = create_nodes:create_atom(MId, MacroName),
    MacroId = create_nodes:create_macro(MId, NameId, [Child]),
    create_nodes:attach_subtree_to_node(MId, MacroId, Root, After),
    create_nodes:init_scope(
      MId, refactor:get_scope_from_id(MId, Child), 
      [NameId, MacroId]).

%% =====================================================================
%% @spec create_record_definition(MId::integer(),
%%                        RecordName::string(), RecordParams::[string()],
%%                        Used::Used) 
%%          -> ok
%%                 Used = not_used | used
%%
%% @doc
%% Insert the record definition and conversion macros into the module. 
%% 
%% Parameter description:<pre>
%% <b>RecordName</b> : A record name.
%% <b>RecordParams</b> : The name of the record fields.
%% <b>Used</b> : Is the record definition in the module.
%% </pre>
%% @end
%% =====================================================================
create_record_definition(_MId, _RecordName, _RecordParams, used) ->    
    ok;
create_record_definition(MId, RecordName, RecordParams, not_used) ->
    FormListId = refactor:get_form_list_id_from_mid(MId),
    %%define record
    DefAttrNameId = create_nodes:create_atom(MId, "record"),
    DefRecordNameId = create_nodes:create_atom(MId, RecordName),
    DefRecordParamNameIds = 
        lists:map(
          fun(Param) -> 
                  create_nodes:create_atom(MId, Param) end, 
          RecordParams),
    DefRecordParamIds = 
        lists:map(
          fun(ParamNameId) -> 
                  create_nodes:create_record_field(MId, ParamNameId, none) end, 
          DefRecordParamNameIds),
    DefTupleId = create_nodes:create_tuple(MId, DefRecordParamIds),
    DefAttrId = create_nodes:create_attribute(
                  MId, DefAttrNameId, [DefRecordNameId, DefTupleId]),
    create_nodes:attach_subtree_to_node(
      MId, DefAttrId, FormListId, 
      hd(tl(hd(erl_syntax_db:subtrees(MId,FormListId))))),
    %%from_record
    FromAttrNameId = create_nodes:create_atom(MId, "define"),
    {Name,Num} = create_nodes:create_fresh_variable(MId),
    FromDefVarList = 
        create_nodes:create_variables(MId, Name, Num, length(RecordParams)+1),
    FromUseVarList = 
        create_nodes:create_variables(MId, Name, Num, length(RecordParams)+1),
    FromMacroNameId = create_nodes:create_atom(MId, "from_" ++ RecordName),
    FromMacroApplId = 
        create_nodes:create_application(
          MId, FromMacroNameId, [hd(FromDefVarList)]),
    FromTupleId = create_nodes:create_tuple(MId, tl(FromUseVarList)),
    FromDefVarListRParams = lists:zip(RecordParams, tl(FromDefVarList)),
    FromRecordFields = 
        lists:map(
          fun({FieldName, Id}) ->
                  create_nodes:create_record_field(
                    MId,create_nodes:create_atom(MId, FieldName), Id) end, 
          FromDefVarListRParams),
    FromRecordType = create_nodes:create_atom(MId, RecordName),
    FromRecord = create_nodes:create_record_expr(
                   MId, none, FromRecordType, FromRecordFields),
    FromClause = create_nodes:create_clause(
                   MId, [FromRecord], none, [FromTupleId]),
    FromFunExpr = create_nodes:create_fun_expr(MId, [FromClause]),
    FromApplication = 
        create_nodes:create_application(MId, FromFunExpr, [hd(FromUseVarList)]),
    FromMacro = 
        create_nodes:create_attribute(
          MId, FromAttrNameId, [FromMacroApplId, FromApplication]),
    create_nodes:attach_subtree_to_node(MId, FromMacro, FormListId, DefAttrId),
    %%to_record
    ToAttrNameId = create_nodes:create_atom(MId, "define"),
    ToDefVarList = 
        create_nodes:create_variables(
          MId, Name, Num + length(RecordParams) + 1, length(RecordParams) + 1),
    ToUseVarList = 
        create_nodes:create_variables(
          MId, Name, Num + length(RecordParams) + 1, length(RecordParams) + 1),
    ToMacroNameId = create_nodes:create_atom(MId, "to_" ++ RecordName),
    ToMacroApplId = 
        create_nodes:create_application(
          MId, ToMacroNameId, [hd(ToDefVarList)]),
    ToTupleId = create_nodes:create_tuple(MId, tl(ToDefVarList)),
    ToUseVarListRParams = lists:zip(RecordParams, tl(ToUseVarList)),
    ToRecordFields = 
        lists:map(
          fun({FieldName, Id}) ->
                  create_nodes:create_record_field(
                    MId,create_nodes:create_atom(MId, FieldName), Id) end, 
          ToUseVarListRParams),
    ToRecordType = create_nodes:create_atom(MId, RecordName),
    ToRecord = create_nodes:create_record_expr(
                 MId, none, ToRecordType, ToRecordFields),
    ToClause = create_nodes:create_clause(
                   MId, [ToTupleId], none, [ToRecord]),
    ToFunExpr = create_nodes:create_fun_expr(MId, [ToClause]),
    ToApplication = 
        create_nodes:create_application(MId, ToFunExpr, [hd(ToUseVarList)]),
    ToMacro = 
        create_nodes:create_attribute(
          MId, ToAttrNameId, [ToMacroApplId, ToApplication]),
    create_nodes:attach_subtree_to_node(MId, ToMacro, FormListId, FromMacro).

