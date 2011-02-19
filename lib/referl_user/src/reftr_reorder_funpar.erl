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
%%%  <li>A module (see {@link reflib_args:order/1}). The new order of the
%%%      function arguments, a list of integer numbers that specifies which
%%%      parameter goes where. For example, the list `3,2,1' means that the
%%%      third parameter will be the first, the first parameter will be the
%%%      last and the second parameter remains in the same place. The list
%%%      must have the same length as the arity of the function.</li>
%%%  <li>A module (see {@link reflib_args:function/1}).The function to be
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
%%% This transformation can handle implicit function calls.
%%% These implicit calls are expanded automatically before the arguments
%%% are reordered, so these function calls cannot cause any problems.
%%% On the other hand, there is a problem with dynamic function calls.
%%% The arguments in such and similar constructs (for example, `apply/3'
%%% or `apply/4') currently cannot be recognised by the tool.
%%% Because of this, the transformation cannot detect these elements as
%%% a function call and its arguments. When the tool is able to detect
%%% this dynamic calls, the transformation can take care of these problems
%%% automatically.
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(reftr_reorder_funpar).
-vsn("$Rev: 4766 $ ").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    NewOrder = ?Args:order(Args),
    Fun      = ?Args:function(Args),
    Arity    = ?Fun:arity(Fun),
    ?Check(lists:sort(NewOrder) =:= lists:seq(1, Arity),
           ?RefError(bad_order, [Arity])),

    {_Mod, ModFunArity} = ?Fun:mod_fun_arity(Fun),
% todo Repair the side effect analyser and reinsert this check.
    ?Check(not ?Fun:is_dirty(Fun), ?RefError(side_effect, [ModFunArity])),

    ImpCalls = ?Query:exec(Fun, ?Fun:implicits()),

    Clauses1 = ?Query:exec(Fun, ?Query:seq(?Fun:definition(), ?Form:clauses())),
    Clauses  = [{Cl, ?Query:exec(Cl, ?Clause:patterns())} || Cl <- Clauses1],

    [fun() -> ?Expr:expand_funexpr(ImpCalls) end,
     fun(_) ->
        % note: function applications have to be determined here,
        % as expand_funexpr will contribute to FunApps
        FunApps  = ?Query:exec(Fun, ?Fun:applications()),
        ArgLists = [?Query:exec(App, ?Expr:child(2)) || App <- FunApps],
        AppArgs  = [{ArgList, ?Query:exec(ArgList, ?Expr:children())}
                    || [ArgList] <- ArgLists],

        [?Transform:touch(Node) || {Node, _} <- Clauses ++ AppArgs],
        [?Syn:replace(Node, {range, hd(NArgs), lists:last(NArgs)},
                      reorder(NArgs, NewOrder))
         ||  {Node, NArgs} <- Clauses ++ AppArgs ]
     end].


%% @private
reorder(Args, Order) ->
      [lists:nth(N, Args) || N <- Order].
