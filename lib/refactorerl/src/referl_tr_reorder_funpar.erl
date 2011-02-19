%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc The order of a function's arguments is a small, aesthetic aspect 
%%%      of a program. Swapping arguments can improve the readability of the
%%%      program, and it can be used as a preparation for another refactoring,
%%%      eg. to create a tuple from arguments that aren't next to each other.
%%%
%%% <ul>
%%%  <li>A module (see {@link referl_args:order/1}). The new order of the
%%%      function arguments, a list of integer numbers that specifies which
%%%      parameter goes where. For example, the list `3,2,1' means that the
%%%      third parameter will be the first, the first parameter will be the
%%%      last and the second parameter remains in the same place. The list
%%%      must have the same length as the arity of the function.</li>
%%%  <li>A module (see {@link referl_args:function/1}).The function to be
%%%      modified. Currently it can be specified with aposition anywhere in
%%%      the function definition.</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%% <li>When a function application has an argument with side effects,
%%% the transformation may only be carried out after a warning that 
%%% the order of side effects most probably will change, which may
%%% change the way the program works.</li>
%%% </ul>
%%% 
%%% == Transformation steps and compensations ==
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
%%% 
%%% == Implementation status ==
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
%%% 
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(referl_tr_reorder_funpar).
-vsn("$Rev: 3185 $ ").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([transform1p/1, 
         transform2p/3,
         transform3p/3, 
         prepare/1,
         error_text/2]).

%%% ============================================================================
%%% Callbacks

%% @private
%% @spec error_text(Code::atom(), Args::[term()]) -> string()
%% @doc
%% Give back the error message text for the transformation specific errors.
%% The error is specified by the `Code' and the `ArgsList'.
error_text(side_effect, []) ->
    ?MISC:format("The function may have side effects!", []).


prepare(Args) ->
    NewOrder    = ?Args:order(Args),
    FunNode     = ?Args:function(Args),
    Arity       = ?Fun:arity(FunNode),

    ?Check(check_arity_in_order(NewOrder, Arity), 
                                 ?RefError(bad_order, [Arity])),

    ?Check(not check_side_effect(FunNode), ?LocalErr0r(side_effect)),

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
    N = length(ValidOrder),
    (N == Arity) andalso
    ((N == 0) orelse
     (lists:max(ValidOrder) =< N) and (lists:min(ValidOrder) >= 1)).


%% @private
check_side_effect(FunNode)->
    ?Fun:dirty(FunNode).
