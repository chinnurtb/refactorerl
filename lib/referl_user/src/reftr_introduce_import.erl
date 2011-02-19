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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc
%%% This refactoring imports the functions of the selected module that are
%%% used in the current file and removes the module qualifiers from the
%%% function calls of this module.
%%%
%%% == Parameters ==
%%% <ul>
%%%     <li>The file the import list will be added to
%%%     (see {@link reflib_args:file/1}).</li>
%%%     <li>The module whose functions will be imported
%%%     (see {@link reflib_args:module/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%     <li>No local funtion of the file has the same name and arity as the
%%%     functions of the module that are used or imported in the file.</li>
%%%     <li>No imported funtion in the file has the same name and arity as the
%%%     functions of the module that are used in the file.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%     <li>In case there is no import list of the module in the file
%%%     a new import list containing the functions of the module that are
%%%     used in the file is added to the file.</li>
%%%     <li>In case there is only one import list of the module in the file
%%%     the rest of the functions used in the file are added to this list.</li>
%%%     <li>In case there is more then one import list of the module, the
%%%     contents of this list will be merged in one, and the rest of the
%%%     funcitons used in the file are added to this list.</li>
%%%     <li>The module qualifiers of the module are removed from the
%%%     corresponding functions.</li>
%%%     <li>The functions used only as implicit funexpression are not
%%%     imported and the module qualifiers will be intact.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Lilla Hajos <lya@elte.hu>

-module(reftr_introduce_import).
-vsn("$Rev: 2590 $ ").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%%% @private
error_text(name_conflict, FunInfo)->
    [ ?MISC:funlist_text(FunInfo),
      " cannot be imported:"
      " a local fun with the same name and arity already exists."].

%%% ============================================================================
%%% Callbacks

%%% @private
prepare(Args) ->
    File        = ?Args:file(Args),
    % todo Add transformation info
    Module      = ?Args:ask(Args, module, fun cc_module/2, fun cc_error/3, File),

    ImportForms = import_forms(File, Module),
    FunRefs     = funrefs(File, Module),
    FunList     = funs_to_import(File, Module, FunRefs),
    check_name_conflicts(File, Module, FunList),
    fun() ->
            case ImportForms of
                [] ->
                    ?Mod:add_import(File, FunList);
                _ ->
                    ?Mod:add_import(hd(ImportForms), FunList),
                    [ ?File:del_form(File,Form) || Form <- tl(ImportForms) ]
            end,
            lists:foreach( fun ?Expr:del_modq/1, FunRefs),

            ?Transform:touch(File)
    end.

%%% ============================================================================
%%% Implementation


%%@doc Import forms in 'File' which source `Module'
import_forms(File, Module) ->
    [ Form || Form <- ?Query:exec(File, ?File:forms()),
              ?Form:type(Form) == import,
              ?Query:exec(Form, ?Query:seq(?Form:expr(1), ?Expr:module()))
                  == [Module] ]. %@todo

%%@doc Funs of `Mod' which are called from `File' with module qualifiers.
funrefs(File, Mod) ->
    lists:flatmap(
      fun(Fun) ->
              [ FunRef ||
                  FunRef <- ?Query:exec(Fun, ?Fun:applications()),
                  ?Query:exec(FunRef, ?Expr:modq()) =/= [],
                  ?Query:exec(FunRef, ?Query:seq([?Expr:clause(),
                                                  ?Clause:form(),
                                                  ?Form:file()])) =:= [File]
              ]
      end,
      ?Query:exec(Mod, ?Mod:locals())).

%%@doc Funs used or imported in `File' which belong to the right module.
funs_to_import(File, Module, FunRefs) ->
    ImportedFunsFromMod =
        [ Fun ||
            Fun <- ?Query:exec(File, ?Query:seq(?File:module(),?Mod:imports())),
            ?Query:exec(Fun, ?Fun:module()) =:= [Module] ],
    NewFuns = ?Query:exec(FunRefs, ?Expr:function()),
    lists:usort(NewFuns ++ ImportedFunsFromMod).

check_name_conflicts(File, Module, FunList) ->
    LocalFuns = name_arity(?Query:exec(File,?Query:seq(?File:module(),
                                                         ?Mod:locals()))),
    ImportedFunsOtherThanMod =
        [ Fun ||
            Fun <- ?Query:exec(File, ?Query:seq(?File:module(),?Mod:imports())),
            ?Query:exec(Fun, ?Fun:module()) =/= [Module] ],
    ImportedFuns = name_arity(ImportedFunsOtherThanMod),
    NewImportFuns = name_arity(FunList),
    Funs = ?MISC:intersect(LocalFuns++ImportedFuns, NewImportFuns),
    ?Check(
       Funs == [],
       ?LocalError(name_conflict, [tuple_to_list(Fun) || Fun <- Funs])).

name_arity(Funs) ->
    [ {?Fun:name(Fun),?Fun:arity(Fun)} || Fun <- Funs ].

%%% ===========================================================================
%%% Checks

cc_module(Module, File) ->
    [FromModule] = ?Query:exec(File, ?File:module()),
    FromName     = ?Mod:name(FromModule),
    ToName       = ?Mod:name(Module),
    ?Check(FromName =/= ToName, ?RefError(source_and_target_equals, [FromName])),
    Module.

cc_error(?RefError(source_and_target_equals, [FromName]), _Module, _File) ->
    ?MISC:format("Module ~p is the source itself.", [FromName]).
