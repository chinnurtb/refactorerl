%%% -*- coding: latin-1 -*-

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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the `Reorder Function Parameters' refactoring.
%%%
%%% This refactoring can be carried out in almost every case without any 
%%% problems, only dynamic function calls put limits to its applicability.
%%% Within the bounds of static analysis, every parameter reordering can 
%%% be compensated at the place of function calls.
%%%
%%% Parameters
%%%
%%% The function to be modified. Currently it can be specified with a
%%% position anywhere in the function definition.
%%%
%%% The new order of the function arguments, a list of integer numbers that
%%% specifies which parameter goes where. For example, the list `3,2,1' means
%%% that the third parameter will be the first, the first parameter will be the
%%% last and the second parameter remains in the same place. The list must have
%%% the same length as the arity of the function.
%%%
%%% Conditions of applicability
%%%
%%% When a function application has an argument with side effects,
%%% the transformation may only be carried out after a warning that 
%%% the order of side effects most probably will change, which may
%%% change the way the program works.
%%%
%%% Transformation steps and compensations
%%%
%%%<ul>
%%% <li>Change the order of patterns in every clause's parameter list in the
%%%  function according to the given new order.
%%% </li>
%%% <li>
%%%  For every static call of the function, change the order of the
%%%  expressions that provide the actual parameters to the call according to the
%%%  given order (obviously in all modules).
%%% </li>
%%% <li>
%%%  Every implicit function expression is expanded to the corresponding
%%%  explicit expression which contains a static call to the function; this
%%%  function call is then updated as described in the previous case.
%%% </li>
%%% <li>
%%%  For every call of the function that provides the arguments as a list,
%%%  insert a compensating function expression that changes the order of the
%%%  elements in the list according to the given new order.
%%% </li>
%%% </ul>
%%% Implementation status
%%%
%%% Currently, this transformation can handle implicit function calls.
%%% These implicit calls are expanded automatically before the arguments 
%%% are reordered, so these function calls cannot cause any problems.
%%% On the other hand, there is a problem with dynamic function calls
%%% The arguments in such and similar constructs (for example, `apply/3'
%%% or `apply/4') currently cannot be recognised by the tool.
%%% Because of this, the transformation cannot detect these elements as 
%%% a function call and its arguments. When the tool is able to detect 
%%% this dynamic calls, the transformation can take care of these problems 
%%% automatically.

%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(referl_tr_reorder_funpar).
-vsn("$Rev: 2607 $ ").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([transform1p/1, 
         transform2p/3,
         transform3p/3, 
         prepare/1]).

%%% ============================================================================
%%% Callbacks

prepare(Args) ->
    NewOrder       = ?Args:order(Args),
    FunNode     = ?Args:function(Args),
    Arity       = ?Fun:arity(FunNode),

    ?Check(check_arity_in_order(NewOrder, Arity) == true, 
                                 ?RefError(bad_order, NewOrder)),

    ?Check(check_side_effect(FunNode) == false, ?RefError(side_effect,[])),

    Clauses  = ?Query:exec(FunNode, 
                           ?Query:seq(?Fun:definition(), ?Form:clauses())),

    ImpCalls = ?Query:exec(FunNode, ?Fun:implicits()),

    [
     fun()-> transform1p(ImpCalls) end,
     fun(ok)-> transform2p(Clauses, NewOrder, pattern) end,
     fun(ok)-> transform3p(FunNode, NewOrder, sub) end
    ].


%%% @private
transform1p(ImpCalls)->
    ?Expr:expand_funexpr(ImpCalls),ok.
%%% @private
transform2p(Funcl, Order, pattern)->
    Clauses = [{C, ?Query:exec(C,?Clause:patterns())}|| C <- Funcl],
    make_new_order(Clauses, Order, pattern),
    [?Transform:touch(Node) || Node <- Funcl],
    ok.
%%% @private
transform3p(FunNode, Order, sub)->
    FunApps = ?Query:exec(FunNode, ?Fun:applications()),
    Appls =[{C, tl(?Query:exec(C,?Expr:children()))}
                                       || C <- FunApps],
    make_new_order(Appls, Order, sub),
    [?Transform:touch(Node) || Node <- FunApps],
    ok.   


%%% ============================================================================
%%% Implementation


%% @spec make_new_order(Clauses::list(),Order::list(),atom()) -> ok
%%
%% @doc This function is a top level transformation. Makes a new order in
%% the parameter list of the marked function and changes the order of the
%% parameter lists in the function usings.
make_new_order(FunArgs, Order, pattern)->
    [?Syn:replace(Node, pattern, change_order(Args, Order))
                                       || {Node, Args} <- FunArgs],
    ok;
make_new_order(FunArgs, Order, sub) ->
    [?Syn:replace(Node,{range, lists:nth(1,Args),lists:last(Args)}, 
               change_order(Args, Order))
                                       || {Node, Args} <- FunArgs],
    ok.

%% @private
change_order(Args,Order)->
      [lists:nth(N, Args) || N <- Order].


%% @spec check_arity_in_order(NewOrder::list(),Arity::integer()) -> refst()
%%
%% @doc Compares the arity of the marked function and the number of the
%% order list element.
check_arity_in_order(NewOrder, Arity)->
    ValidOrder = lists:usort(NewOrder),
    (lists:max(ValidOrder) =< length(ValidOrder))
    and (lists:min(ValidOrder)>= 1) 
    and (length(ValidOrder) == Arity).


%% @private
check_side_effect(FunNode)->
    ?Fun:dirty(FunNode).
