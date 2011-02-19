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

    FunRefs = funrefs(File, Module),

    Forms   = import_forms(File, Module),

    FunList = funs_to_import(Forms, FunRefs),
    
    check_name_conflicts(File, FunList),
    
    ParamsForReplace = [ {hd(?Query:exec(Ref, ?Expr:parent())),
                          ?Query:exec(Ref, ?Expr:child(2))} || Ref <- FunRefs ],

    fun() ->
            case Forms of
                [] ->
                    ?Mod:add_import(File, FunList);   
                _ ->
                    ?Mod:add_import(hd(Forms), FunList),
                    [ ?File:del_form(File,Form) || Form <- tl(Forms) ]
            end,
       [ ?Syn:replace(Parent, {sub,1}, NewChild) ||
           {Parent, NewChild} <- ParamsForReplace ],
       ?Transform:touch(File)
    end.


%%% ============================================================================
%%% Implementation


%%Only those funs of `Mod' that are called with module qualifier.
funrefs(File, Mod) ->
    FunForms = [ Form || Form <- ?Query:exec(File, ?File:forms()),
                         ?Form:type(Form) == func ],
    [ Ref || Ref <- ?Query:exec(FunForms, ?Form:deep_exprs()),
             ?Query:exec(Ref, [{sub,1}, modref]) == [Mod] ].

%%Import lists of `Module'.
import_forms(File, Module) ->
    [ Form ||
        Form <- ?Query:exec(File, ?File:forms()),
        ?Form:type(Form) == import,
        ?Query:exec(Form, [{attr,1},modref]) == [Module] ].

%%All the funs being used in `File', including the already imported ones.
funs_to_import(Forms, FunRefs) ->
    FunsAlreadyImported =
        [ ?Query:exec(Import, ?Query:seq([ [{attr,2}],
                                           ?Expr:child(1),
                                           ?Expr:children(),
                                           ?Expr:function() ])) ||
            Import <- Forms ],

    NewFuns = 
        [ ?Query:exec(FunRef,?Query:seq([ ?Expr:parent(),
                                          ?Expr:function() ])) ||
            FunRef <- FunRefs ],

    lists:usort(lists:flatten(NewFuns++FunsAlreadyImported)).


check_name_conflicts(File, FunList) ->
    LocalFuns = 
        [ {?Fun:name(Fun),?Fun:arity(Fun)} ||
            Fun <- ?Query:exec( File,
			        ?Query:seq(?File:module(),?Mod:locals()) ) ],

    ImportFuns =
        [ {?Fun:name(Fun),?Fun:arity(Fun)} || Fun <- FunList ],
        
    Funs = ?MISC:intersect(LocalFuns, ImportFuns),
    ?Check( 
       Funs == [],
       ?LocalError( name_conflict,
                    [ element(1,hd(Funs)), element(2,hd(Funs)) ])).
