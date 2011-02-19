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
%%% <li>The function to be renamed (see {@link reflib_args:function/1})</li>
%%% <li>The new name of the function (see {@link reflib_args:name/1})</li>
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

-module(reftr_rename_fun).
-vsn("$Rev: 5496 $ ").

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    FunNode  = ?Args:function(Args),
    ArgsInfo = add_transformation_info(Args, FunNode),
    Name     = ?Args:ask(ArgsInfo, name, fun cc_funname/2, fun cc_error/3, FunNode),
    NameStr  = io_lib:write_atom(Name),

    FunNameNodes      = fun_name_nodes(FunNode),
    ImpExpNameNodes   = imp_exp_name_nodes(FunNode),
    AppNameNodes      = app_name_nodes(FunNode),
    ImplicitNameNodes = imp_name_nodes(FunNode),
    Updates = FunNameNodes ++ ImpExpNameNodes ++ AppNameNodes ++ ImplicitNameNodes,

%    ?Macro:check_single_usage(Updates, [{elex, 1}]),

    [FunDefNode] = ?Query:exec(FunNode, ?Query:seq(?Fun:definition(), ?Form:func())),
    DynUpdates   = reflib_dynfun:collect({rename, func}, FunDefNode, Name),

    [fun() ->
        ?Macro:inline_single_virtuals(Updates, elex),

        % updating virtuals and non-virtuals in arbitrary order caused problems
        % todo check whether such a condition is still necessary
        %        if yes, document it in test cases
        {Virts, NonVirts} = lists:partition(fun(Node) -> ?Macro:is_virtual(Node, {elex, 1}) end, Updates),
        [?Macro:update_macro(Node, {elex, 1}, NameStr) || Node <- Virts],
        [?Macro:update_macro(Node, {elex, 1}, NameStr) || Node <- NonVirts],

        reflib_dynfun:transform(DynUpdates)
    end,
    fun(_)->
        [FunNode] %@todo where is it updated...?
    end].

%% Adds a `transformation_info' field to `Args'
%% that describes the current transformation.
add_transformation_info(Args, Fun) ->
    [Mod]   = ?Query:exec(Fun, ?Fun:module()),
    ModName = ?Mod:name(Mod),
    FunName = ?Fun:name(Fun),
    FunAr   = ?Fun:arity(Fun),
    Info    = ?MISC:format("Renaming function ~p:~p/~p", [ModName, FunName, FunAr]),
    [{transformation_text, Info} | Args].

%%% ============================================================================
%%% Implementation

imp_name_nodes(FunNode) ->
    ImplicitNodes = ?Query:exec(FunNode, ?Fun:implicits()),
    ImplicitNameNodes =
        [case ( ?Query:exec(Node, ?Expr:modq())==[]) of
            true ->
                hd( ?Query:exec(Node, ?Expr:child(1)));
            false ->
                [DelimiterNode] = ?Query:exec(Node, ?Expr:child(1)),
                hd( ?Query:exec(DelimiterNode, ?Expr:child(2)))
        end || Node<-ImplicitNodes],
    ImplicitNameNodes.

app_name_nodes(FunNode) ->
    AppNodes     = ?Query:exec(FunNode, ?Fun:applications()),
    AppNameNodes =
        [case ?Query:exec(Node, ?Expr:modq()) of
            [] ->
                hd( ?Query:exec(Node, ?Expr:child(1)));
            _ -> % module qualifier present
                [DelimiterNode] = ?Query:exec(Node, ?Expr:child(1)),
                hd( ?Query:exec(DelimiterNode, ?Expr:child(2)))
        end || Node <- AppNodes],
    AppNameNodes.

imp_exp_name_nodes(FunNode) ->
    ImpExpNodes = ?Query:exec(FunNode, ?Fun:impexps()),
    [hd(?Query:exec(Node, ?Expr:child(1))) || Node<-ImpExpNodes].

fun_name_nodes(FunNode) ->
    [FunDefNode] = ?Query:exec(FunNode, ?Fun:definition()),
    FunClauseNodes = ?Query:exec(FunDefNode, ?Form:clauses()),
    FunNameNodes = lists:flatten([ ?Query:exec(FunClauseNode, ?Clause:name())
            || FunClauseNode<-FunClauseNodes]),
    FunNameNodes.


%%% ============================================================================
%%% Checks

cc_funname(NewName, FunNode) ->
    Arity = ?Fun:arity(FunNode),
    ?Check(?Query:exec(FunNode, ?Fun:definition()) =/= [],
           ?RefError(fun_def_not_found, [?Fun:name(FunNode), Arity])),
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

    ?Check(not ?Fun:is_autoimported(NewName, Arity),
           ?RefError(autoimported_fun_exists, [NewName, Arity])),
    NewName.

cc_error(?RefError(_, _), Name, _FunNode) ->
    ?MISC:format("The function name ~p is already used.", [Name]).
