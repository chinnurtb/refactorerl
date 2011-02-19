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
%% This module implements the eliminate variable refactoring.
%% 
%% @end

%% Changelog
%%  Version 0.1.2: Bugfixing, when the reordered function has only 
%%                 applications as its calls, not implicit function.
%%                 One case added into the change_application_parameter_order/3
%%                 function to avoid the needless call of the 
%%                 change_parameter_order/4 function.
%%                 Bugfixing in the reorder_funpar/4 function, because it 
%%                 called the refactor:get_fun_calls/2 with wrong module id.
%% End changelog

-module(refac_reorder_funpar).

-export([reorder_funpar/4]).

-vsn('0.2.').

-include("node_type.hrl").

%% =====================================================================
%% @spec reorder_funpar(File::string(), Line::integer(), 
%%                       Col::integer(), Order::string()) -> none()
%%
%% @doc
%% Performs the precondition checks and the and the collects all the 
%% necessary information for the refactoring.
%% It throws various terms when there is an error, with this syntax:
%% {atom(), term()}. If the atom is ok, the refactoring has been 
%% performed without a problem.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The pointed line number in the editor.
%% <b>Col</b> : The pointed column number in the editor.
%% <b>Order</b> : The new order of the function parameters. </pre>
%% @end
%% =====================================================================
reorder_funpar(File, Line, Col, Order) ->
    MId = refac_common:get_module_id(File),
    IdandType = refac_common:get_id_from_pos(MId, Line, Col, function),
    {MId2, FunId} = refac_common:find_the_function(MId, IdandType),
    Arity = refactor:get_arity_from_fun_id(MId2, FunId),
    OrderList = produce_list(Order),
    ChangeOrderList = get_changing_order(OrderList, Arity),

    {ApplicationIds, _ApplicationTypeandNameIds,
     _ImplicitFunCallIds, ImplicitFunCallandTypeIds,
     _ImplicitFunCallTypeandNameIds} = refac_common:get_fun_calls(MId2, FunId),

    refac_checks:check_orderList(OrderList, Arity),

    perform_refactoring(
      MId2, FunId, ChangeOrderList, Arity, ApplicationIds, 
      ImplicitFunCallandTypeIds),

    refac_common:warnings(),
    {ok, done}.

%% =====================================================================
%% @spec perform_refactoring(
%%               MId::integer(), FunId::integer(),
%%   ChangeOrderList::[{integer(), integer()}], Arity::integer(),
%%    ApplicationIds::[{integer(), integer(), integer()}], 
%%   ImplicitFunCallandTypeIds::[{integer(), integer(), integer()}]
%%                          ) -> ok
%%
%% @doc
%% Performs the refactoring, and commits the changes into the 
%% refactoring system.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : The id of the reordered function. 
%% <b>ChangeOrderList</b> : The changing order list.
%% <b>Arity</b> : The arity of the reordered function.
%% <b>ApplicationIds</b> : List of the application calls 
%%    to the reordered function.
%% <b>ImplicitFunCallandTypeIds</b> : List of the implicit fun calls 
%%    to the reordered function. </pre>
%% @end
%% =====================================================================
perform_refactoring(
  MId, FunId, ChangeOrderList, Arity, ApplicationIds, 
  ImplicitFunCallandTypeIds) ->
    change_clauses_parameter_order(MId, FunId, ChangeOrderList, Arity),
    change_application_parameter_order(ChangeOrderList, Arity, ApplicationIds),
    NewApplicationList = 
	change_implicit_funcall_order(
	  MId, FunId, Arity, ImplicitFunCallandTypeIds),
    change_application_parameter_order(
      ChangeOrderList, Arity, NewApplicationList),

    refactor:commit().



%% =====================================================================
%% @spec produce_list(Order::string()) -> [integer()]
%%
%% @doc
%% Converts the string order list into a proper list. 
%% 
%% Parameter description:<pre>
%% <b>Order</b> : The new order of the function parameters. </pre>
%% @end
%% =====================================================================
produce_list([]) ->
    bad_list;
produce_list(Order) when hd(Order)>=48, hd(Order)=<57 ->
    produce_list(tl(Order),[hd(Order)-48]);
produce_list(Order) ->
    produce_list(tl(Order)).

%% =====================================================================
%% @spec produce_list(Order::string(), OrderList::[integer()]) -> [integer()]
%%
%% @doc
%% Converts the string order list into a proper list. 
%% 
%% Parameter description:<pre>
%% <b>Order</b> : The new order of the function parameters. 
%% <b>OrderList</b> : The proper list's prefix. </pre>
%% @end
%% =====================================================================
produce_list([], OrderList) ->
    lists:reverse(
      lists:filter(fun(0) -> false;(_Number) -> true end ,OrderList));
produce_list(Order, [X|Xs]) when hd(Order)>=48, hd(Order)=<57 ->
    produce_list(tl(Order), [X * 10 + (hd(Order)-48) | Xs]);
produce_list(Order, OrderList) ->
    produce_list(tl(Order), [0] ++ OrderList).

%% =====================================================================
%% @spec get_changing_order(OrderList::[integer()], Arity::integer()) -> 
%%                      [{integer(), integer()}]
%%
%% @doc
%% Converts the new order list list into changing steps. 
%% 
%% Parameter description:<pre>
%% <b>OrderList</b> : The new order of the function parameters.
%% <b>Arity</b> : The arity of the reordered function. </pre>
%% @end
%% =====================================================================
get_changing_order(OrderList, Arity) ->
    refac_checks:check_orderList(OrderList, Arity),
    get_changing_order(OrderList,OrderList,[],1).

%% =====================================================================
%% @spec get_changing_order(
%%                OrderList::[integer()], OrderList::[integer()],
%%                ChangeOrderList::[{integer(), integer()}], 
%%                Step::integer()) -> 
%%                      [{integer(), integer()}]
%%
%% @doc
%% Converts the new order list list into changing steps. 
%% 
%% Parameter description:<pre>
%% <b>OrderList</b> : The new order of the function parameters.
%% <b>ChangeOrderList</b> : The new order's changing steps 
%%                          of the function parameters prefix.
%% <b>Step</b> : The position of the element in the list 
%%               we are working with. </pre>
%% @end
%% =====================================================================
get_changing_order([], _OrderList, ChangeOrderList, _N) ->
    ChangeOrderList;
get_changing_order([X | Xs], OrderList, ChangeOrderList, X) ->
    get_changing_order(Xs, OrderList, ChangeOrderList, X+1);
get_changing_order([X | _Xs], OrderList, ChangeOrderList, N) ->
    NewList = change(N, X, OrderList),
    get_changing_order(NewList, NewList, [{N,X}]++ChangeOrderList, 1).

%% =====================================================================
%% @spec change(Pos1::integer(), Pos2::integer(), List::list()) -> 
%%                     list()
%%
%% @doc
%% Changes two elements in a list. 
%% 
%% Parameter description:<pre>
%% <b>Pos1</b> : A postion in a list.
%% <b>Pos2</b> : A postion in a list.
%% <b>List</b> : List of elements. </pre>
%% @end
%% =====================================================================
change(Pos1, Pos2, List) when Pos1 =< Pos2 ->
    Element1 = lists:nth(Pos1, List),
    Element2 = lists:nth(Pos2, List),
    change(Pos1, Element1, Pos2, Element2, List, 1);
change(Pos1, Pos2, List) ->
    Element1 = lists:nth(Pos1, List),
    Element2 = lists:nth(Pos2, List),
    change(Pos2, Element2, Pos1, Element1, List, 1).

%% =====================================================================
%% @spec change(Pos1::integer(), Element1::term(), 
%%              Pos2::integer(), Element2::term(),
%%              List::list(), Step::integer()) -> list()
%%
%% @doc
%% Changes two elements in a list. 
%% 
%% Parameter description:<pre>
%% <b>Pos1</b> : A postion in a list.
%% <b>Element1</b> : Element on <b>Pos1</b>.
%% <b>Pos2</b> : A postion in a list.
%% <b>Element2</b> : Element on <b>Pos2</b>.
%% <b>List</b> : List of elements.
%% <b>Step</b> : The position of the element in the list 
%%               we are working with. </pre>
%% @end
%% =====================================================================
change(Pos1, Element1, Pos2, Element2, [_X|Xs], Pos1) ->
    [Element2 | change(Pos1, Element1, Pos2, Element2, Xs, Pos1+1)];
change(_Pos1,Element1,Pos2,_Element2,[_X|Xs],Pos2) ->
    [Element1 | Xs];
change(Pos1, Element1, Pos2, Element2, [X|Xs], N) ->
    [X | change(Pos1, Element1, Pos2, Element2, Xs, N+1)].

%% =====================================================================
%% @spec change_clauses_parameter_order(
%%               MId::integer(), FunId::integer(),
%%   ChangeOrderList::[{integer(), integer()}], Arity::integer()) -> ok
%%
%% @doc
%% Changes the parameter order in the clauses.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : The id of the reordered function. 
%% <b>ChangeOrderList</b> : The changing order list. </pre>
%% @end
%% =====================================================================    
change_clauses_parameter_order(MId, FunId, ChangeOrderList, Arity) ->
    ClauseIds = refactor:get_clause_mids_and_ids_from_fun_id(MId, FunId),
    change_parameter_order(
      ChangeOrderList, refactor:create_condition_list(ClauseIds), Arity,
      fun refactor:update_clause_pos/3).

%% =====================================================================
%% @spec change_application_parameter_order(
%%   ChangeOrderList::[{integer(), integer()}], Arity::integer(),
%%    ApplicationIds::[{integer(), integer(), integer()}]) -> ok
%%
%% @doc
%% Changes the parameter order in the applications.
%% 
%% Parameter description:<pre>
%% <b>ChangeOrderList</b> : The changing order list.
%% <b>Arity</b> : The arity of the reordered function.
%% <b>ApplicationIds</b> : List of the application calls 
%%    to the reordered function. </pre>
%% @end
%% =====================================================================
change_application_parameter_order(ChangeOrderList, Arity, ApplicationIds) ->
    case ApplicationIds of
        [{_, _, _}|_] -> {MIds, Ids, _} = lists:unzip3(ApplicationIds);
        [{_, _}|_] -> {MIds, Ids} = lists:unzip(ApplicationIds);
        [] -> {MIds, Ids} = {[],[]}
    end,
    ReorderedApplicationIds = lists:zip(MIds, Ids),
    case ReorderedApplicationIds of
        [] -> ok;
        _ ->
            change_parameter_order(
              ChangeOrderList, 
              refactor:create_condition_list(ReorderedApplicationIds), Arity,
              fun refactor:update_application/3)
    end.

%% =====================================================================
%% @spec change_parameter_order(
%%   ChangeOrderList::[{integer(), integer()}], 
%%   ConditionList::string(), Arity::integer(),
%%   Updater::function()) -> ok
%%
%% @doc
%% Changes the parameter order in the applications.
%% 
%% Parameter description:<pre>
%% <b>ChangeOrderList</b> : The changing order list.
%% <b>ConditionList</b> : The string to identify the applications 
%%                                   in the refactoring system.
%% <b>Arity</b> : The arity of the reordered function.
%% <b>Updater</b> : Function to do the updating. </pre>
%% @end
%% =====================================================================
change_parameter_order(ChangeOrderList, ConditionList, Arity, Updater) ->
    lists:map(
      fun({N,M}) ->
        Updater(N, Arity+1, ConditionList),
        Updater(M, N, ConditionList),
        Updater(Arity+1, M, ConditionList)
      end, ChangeOrderList).

%% =====================================================================
%% @spec change_implicit_funcall_order(
%%               MId::integer(), FunId::integer(),
%%             Arity::integer(),
%%   ImplicitFunCallandTypeIds::[{integer(), integer(), integer()}]
%%                          ) -> [{integer(), integer()}]
%%
%% @doc
%% Converts the implicit fun calls to applications.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunId</b> : The id of the reordered function. 
%% <b>Arity</b> : The arity of the reordered function.
%% <b>ImplicitFunCallandTypeIds</b> : List of the implicit fun calls 
%%    to the reordered function. </pre>
%% @end
%% =====================================================================
change_implicit_funcall_order(MId, FunId, Arity, ImplicitFunCallandTypeIds) ->
  lists:map(
    fun({Type, MId2, ImplicitId}) ->
      ScopeId = refactor:get_scope_from_id(MId2, ImplicitId),
      delete_nodes:delete_node(MId2, ImplicitId),
      {Name,Num} = create_nodes:create_fresh_variable(MId2),
      FormParList = create_nodes:create_variables(MId2, Name, Num, Arity),
      ApplicationParList = create_nodes:create_variables(MId2, Name, Num, Arity),
      FunNameId = 
        create_nodes:create_atom(MId2, refactor:get_fun_name(MId, FunId)), 

      {ApplicationId, Additional} =
        case Type of
          ?ARITY_QUALIFIER ->
            {create_nodes:create_application(MId2, FunNameId, ApplicationParList),
             []};
          ?MODULE_QUALIFIER ->
            ModuleNameId = 
              create_nodes:create_atom(MId2, refactor:get_module_name(MId)),
            ModuleQualifierId = 
              create_nodes:create_module_qualifier(MId2, ModuleNameId, FunNameId),
              {create_nodes:create_application(
          	  MId2, ModuleQualifierId, ApplicationParList),
               [ModuleNameId, ModuleQualifierId]}
        end,

      create_nodes:connect_fun_call(MId2, ApplicationId, MId, FunId),
      ClauseId = 
        create_nodes:create_clause(MId2, FormParList, none, [ApplicationId]),
      create_nodes:create_fun_expr(MId2, [ClauseId], ImplicitId),
      create_nodes:init_scope(
        MId2, ClauseId, FormParList ++ ApplicationParList ++
                        [FunNameId] ++ Additional ++ [ApplicationId, ClauseId]),

      create_nodes:init_scope(MId2, ScopeId, [ImplicitId]),
      create_nodes:create_scope(MId2, ClauseId, ScopeId),
      create_nodes:connect_variables(MId2, FormParList, FormParList),
      create_nodes:connect_variables(MId2, FormParList, ApplicationParList),
      {MId2, ApplicationId}
    end, ImplicitFunCallandTypeIds).
