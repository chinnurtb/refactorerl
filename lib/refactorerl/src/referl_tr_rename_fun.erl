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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the rename function refactoring.
%%%
%%% == Parameters ==
%%% <ul>
%%% <li>The function to be renamed (see {@link referl_args:function/1})</li>
%%% <li>The new name of the function (see {@link referl_args:name/1})</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%% <li> There must be no function with the given name and the same arity as the
%%% function to be renamed among the functions in the module,
%%% the functions imported in the module, and the auto-imported BIFs.</li>
%%% <li>There must be no function with the given name and the same arity as the
%%% function to be renamed among the local and imported
%%% functions in the modules that import the function to be renamed.</li>
%%% <li>When there are multiple overloaded versions of the function, there must
%%% be no call to the function that creates the list of arguments dynamically.
%%% </li></ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%  <li>The name label of the function is changed at every branch of the
%%%  definition to the new one.</li>
%%%  <li>In every static call to the function, the old function name is changed
%%%  to the new one.</li>
%%%  <li>Every implicit function expression is modified to contain the new
%%%  function name instead of the old one.</li>
%%%  <li>If the function is exported from the module, the old name is removed
%%%  from the export list and the new name is put in it.</li>
%%%  <li>If the function is imported in an other module, the import list is
%%%  changed in that module to contain the new name instead of the old one.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% <ul>
%%%   <li>All the transformation steps are implemented.</li>
%%%   <li >The transformation does not takes into account
%%%   the dynamic function calls. </li>
%%% </ul>
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author Atilla Erdodi <erdodi@elte.hu>

-module(referl_tr_rename_fun).
-vsn("$Rev: 3185 $ ").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1]).

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    FunNode = ?Args:function(Args),
    NewFunNameAtom = ?Args:name(Args),

    check_fun(FunNode, NewFunNameAtom),

    %% calculating the updates
    Updates = calc_update(FunNode),

    fun() ->
	    lists:foreach(
	      fun(Node) ->
		      ?Syn:replace(Node, {elex, 1},
				   [io_lib:write_atom(NewFunNameAtom)]),
		      ?Transform:touch(Node)
	      end, Updates)
    end.

%% @spec calc_update(node()) -> [node()]
%%
%% @doc Returns a list of name nodes that should be updated
calc_update(FunNode) ->

    %function definition
    [FunDefNode] = ?Query:exec(FunNode, ?Fun:definition()),
    FunClauseNodes = ?Query:exec(FunDefNode, ?Form:clauses()),
    FunNameNodes = lists:flatten([?Query:exec(FunClauseNode, ?Clause:name())
                                  || FunClauseNode <- FunClauseNodes]),

    ImpExpNodes = ?Query:exec(FunNode, ?Fun:impexps()),
    AppNodes = ?Query:exec(FunNode, ?Fun:applications()),
    ImplicitNodes = ?Query:exec(FunNode, ?Fun:implicits()),

    %function imports and exports
    ImpExpNameNodes = [hd(?Query:exec(Node, ?Expr:child(1))) ||
                          Node <- ImpExpNodes],

    %function applications
    AppNameNodes =
        [case (?Query:exec(Node, ?Expr:modq()) == []) of
             true ->
                 hd(?Query:exec(Node, ?Expr:child(1)));
             false -> % module qualifier present
                 [DelimiterNode] = ?Query:exec(Node, ?Expr:child(1)),
                 hd(?Query:exec(DelimiterNode, ?Expr:child(2)))
         end || Node <- AppNodes],

    %implicit function expressions
    ImplicitNameNodes =
        [case (?Query:exec(Node, ?Expr:modq()) == []) of
             true ->
                 hd(?Query:exec(Node, ?Expr:child(1)));
             false ->
                 [DelimiterNode] = ?Query:exec(Node, ?Expr:child(1)),
                 hd(?Query:exec(DelimiterNode, ?Expr:child(2)))
         end || Node <- ImplicitNodes],

    FunNameNodes ++ ImpExpNameNodes ++ AppNameNodes ++ ImplicitNameNodes.


check_fun(FunNode, NewName) ->
    Arity = ?Fun:arity(FunNode),
    Modules =
        ?Query:exec(FunNode, ?Fun:module()) ++
        ?Query:exec(FunNode, ?Fun:imported()),

    lists:foreach(
      fun(Module) ->
              ModName = ?Mod:name(Module),
              ?Check(?Query:exec(Module, ?Mod:local(NewName, Arity)) =:= [],
                     ?RefError(fun_exists, [ModName, NewName, Arity])),
              ?Check(?Query:exec(Module, ?Mod:imported(NewName, Arity)) =:= [],
                     ?RefError(imported_fun_exists, [ModName, [NewName, Arity]]))
      end, Modules),

    ?Check(?Fun:autoimported(NewName, Arity) =:= false,
           ?RefError(autoimported_fun_exists, [NewName, Arity])).
