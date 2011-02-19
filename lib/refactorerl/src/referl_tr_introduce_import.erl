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

%%% @doc
%%% This refactoring imports the functions of the selected module that are 
%%% used in the current file and removes the module qualifiers from the
%%% function calls of this module.
%%%
%%% == Parameters ==
%%% <ul>
%%%     <li>The file the import list will be added to
%%%     (see {@link referl_args:file/1}).</li>
%%%     <li>The module whose functions will be imported
%%%     (see {@link referl_args:module/1}).</li>
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
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Lilla Hajos <lya@elte.hu>

-module(referl_tr_introduce_import).
-vsn("$Rev: 2590 $").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1, error_text/2]).


%%% ============================================================================
%%% Errors

error_text(name_conflict, FunInfo)->
    [ ?MISC:fun_text(FunInfo),
      " can not be imported: a local fun with the same name aready exists."].

%%% ============================================================================
%%% Callbacks


prepare(Args) ->
    File    = ?Args:file(Args),
    Module  = ?Args:module(Args),

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
            lists:map( fun ?Expr:del_modq/1, FunRefs),
            ?Transform:touch(File)
    end.


%%% ============================================================================
%%% Implementation


%%Forms that belong to the `Module',
import_forms(File, Module) ->
    [ Form || Form <- ?Query:exec(File, ?File:forms()),
              ?Form:type(Form) == import,
              ?Query:exec(Form, [{attr,1},modref]) == [Module] ].

%%The funs of `Mod' that are called from the `File' with module qualifier.
funrefs(File, Mod) ->
    lists:flatten(
      lists:map(
        fun(Fun) ->
                [ FunRef || FunRef <- ?Query:exec(Fun, [{funref,back}]),
                            ?Query:exec(FunRef, ?Expr:modq()) =/= [],
                            ?Syn:get_file(FunRef) =:= [File] ]
        end,
        ?Query:exec(Mod, ?Mod:locals()))).  

%%The funs used or imported in file `File', that belong to the right module.
funs_to_import(File, Module, FunRefs) ->
    ImportedFuns =
        [ Fun ||
            Fun <- ?Query:exec(File, ?Query:seq(?File:module(), [funimp])),
            ?Query:exec(Fun, ?Fun:module()) =:= [Module] ],

    NewFuns = ?Query:exec(FunRefs, ?Expr:function()),

    lists:usort(NewFuns++ImportedFuns).

check_name_conflicts(File, Module, FunList) ->
    LocalFuns = [ {?Fun:name(Fun),?Fun:arity(Fun)} ||
                    Fun <- ?Query:exec( File, ?Query:seq(?File:module(),
                                                         ?Mod:locals())) ],

    ImportedFuns =
        [ {?Fun:name(Fun),?Fun:arity(Fun)} ||
            Fun <- ?Query:exec(File, ?Query:seq(?File:module(), [funimp])),
            ?Query:exec(Fun, ?Fun:module()) =/= [Module] ],

    NewImportFuns = [ {?Fun:name(Fun),?Fun:arity(Fun)} || Fun <- FunList ],

    Funs = ?MISC:intersect((LocalFuns++ImportedFuns), NewImportFuns),
    ?Check( 
       Funs == [],
       ?LocalError( name_conflict,
                    [ element(1,hd(Funs)), element(2,hd(Funs)) ])).
