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

%%% @doc Analyse function definitions and references. The following semantical
%%% structure is created:
%%%
%%% <ul>
%%%
%%% <li>A semantical function object is created for every referenced and/or
%%%   defined function</li>
%%%
%%% <li>The containing module object is linked to the function object
%%% (`func')</li>
%%%
%%% <li>The semantical function is linked to the syntactical function
%%%   definition when that definition is loaded (`fundef')</li>
%%%
%%% <li>Expressions that refer to a function are linked to the function object
%%%   (`funref'). Such expressions can be function applications and implicit
%%%   fun expressions.</li>
%%%
%%% <li>Functions exported from a module are linked from the module
%%%   (`funexp')</li>
%%%
%%% <li>Functions that are imported into a module are linked from the module
%%%   (`funimp')</li>
%%%
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_anal_fun).
-vsn("$Rev: 2693 $").
-behaviour(referl_esg).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").


%% @private
init() ->
    [{func, record_info(fields, func), []},
     {form,   [{fundef, func}]},
     {module, [{func, func}, {funexp, func}, {funimp, func}]},
     {expr,   [{funref, func}]}
    ].


%%% ----------------------------------------------------------------------------

%% @private

%% Function definitions

insert(Clause, #clause{kind = fundef}, name, _Expr, #expr{value = Name}) ->
    [FunForm] = ?Graph:path(Clause, [{funcl, back}]),
    case ?Graph:path(FunForm, [fundef]) of
        [] ->
            [Module] = ?Graph:path(FunForm, [{form, back}, moddef]),
            Arity = length(?Graph:path(Clause, [pattern])),
            function_object(Module, Name, Arity, FunForm);
        [FunObj] ->
            case ?Graph:data(FunObj) of
                #func{name=Name} ->
                    ok;
                #func{name=_OldName, arity=Arity} ->
                    [Module] = ?Graph:path(FunObj, [{func, back}]),
                    remove_funlink(FunForm, fundef, FunObj),
                    function_object(Module, Name, Arity, FunForm)
            end
    end;

insert(Clause, #clause{kind = fundef}, pattern, _Pattern, #expr{}) ->
    [FunForm] = ?Graph:path(Clause, [{funcl, back}]),
    Arity = length(?Graph:path(Clause, [pattern])),
    case ?Graph:path(FunForm, [fundef]) of
        [] ->
            [Module] = ?Graph:path(FunForm, [{form, back}, moddef]),
            function_object(Module, undefined, Arity, FunForm);
        [FunObj] ->
            case ?Graph:data(FunObj) of
                #func{arity=Arity} ->
                    ok;
                #func{name=Name, arity=_OldArity} ->
                    [Module] = ?Graph:path(FunObj, [{func, back}]),
                    remove_funlink(FunForm, fundef, FunObj),
                    function_object(Module, Name, Arity, FunForm)
            end
    end;

%%% ----------------------------------------------------------------------------

%% Export/import lists

insert(_, _, _, Expr, #expr{kind=infix_expr, value='/'}) ->
    case ?Graph:path(Expr, [sup, {attr, back}]) of
        [Attrib] ->
            case ?Graph:data(Attrib) of
                #form{tag=export} -> export_function(Attrib, Expr);
                #form{tag=import} -> import_function(Attrib, Expr)
            end;
        _ -> ok
    end;

%% TODO: remove/resolve code duplicates

insert(ArityQual, #expr{kind=infix_expr, value='/'}, sub,
       _, #expr{kind=atom, value=Name}) ->

    MaybeAttrib = ?Graph:path(ArityQual, [sup, {attr, back}]),
    case remove_funlink(ArityQual, funref) of
        [] ->
            case impexp_list(MaybeAttrib) of
                {export, Export} -> export_function(Export, ArityQual);
                {import, Import} -> import_function(Import, ArityQual);
                false            -> ok
            end;
        FunObj ->
            case ?Graph:data(FunObj) of
                #func{name=Name} ->
                    ok;
                #func{name=_OldName, arity=Arity} ->
                    update_impexp(MaybeAttrib, ArityQual, FunObj, Name, Arity)
            end
    end;

insert(ArityQual, #expr{kind=infix_expr, value='/'}, sub,
       _, #expr{kind=integer, value=Arity}) ->
    MaybeAttrib = ?Graph:path(ArityQual, [sup, {attr, back}]),
    case remove_funlink(ArityQual, funref) of
        [] ->
            case impexp_list(MaybeAttrib) of
                {export, Export} -> export_function(Export, ArityQual);
                {import, Import} -> import_function(Import, ArityQual);
                false            -> ok
            end;
        FunObj ->
            case ?Graph:data(FunObj) of
                #func{arity=Arity} ->
                    ok;
                #func{name=Name, arity=_OldArity} ->
                    update_impexp(MaybeAttrib, ArityQual, FunObj, Name, Arity)
            end
    end;

%% Function applications

insert(App, #expr{kind = Kind}, sub, _, #expr{})
  when Kind =:= application orelse Kind =:= implicit_fun ->
    anal_app(App);

insert(FunRef, #expr{type=expr, kind=infix_expr, value=':'}, sub, _, #expr{kind=atom}) ->
    %% only atom?
    [App] = ?Graph:path(FunRef, [{sub, back}]),
    anal_app(App);

insert(_,_,_,_,_) ->
    ok.

update_impexp(MaybeAttrib, ArityQual, FunObj, Name, Arity) ->
    case impexp_list(MaybeAttrib) of
        {export, Export} ->
            [Module] =
                ?Graph:path(Export, [{form, back}, moddef]),
            remove_funlink(Module, funexp, FunObj),
            export_function(ArityQual, Module, Name, Arity);
        {import, Import} ->
            [Module] =
                ?Graph:path(Import, [{form, back}, moddef]),
            remove_funlink(Module, funimp, FunObj),
            import_function(ArityQual, Import, Module, Name, Arity);
        false ->
            ok
    end.


%%% ----------------------------------------------------------------------------

%% @private

remove(_, _, _, Expr, #expr{kind=Kind, value=Op})
  when Kind == implicit_fun;
       Kind == application;
       Kind == infix_expr andalso Op == '/' -> %% todo
    case ?Graph:path(Expr, [funref]) of
        [Fun] ->
            remove_funlink(Expr, funref, Fun),
            case ?Graph:path(Expr, [sup, {attr, back}]) of
                [Attrib] ->
                    case ?Graph:path(Attrib, [{form, back}, moddef]) of
                        [Mod] ->
                            case ?Graph:data(Attrib) of
                                #form{tag=export} ->
                                    remove_funlink(Mod, funexp, Fun);
                                #form{tag=import} ->
                                    remove_funlink(Mod, funimp, Fun);
                                _ ->
                                    %% parsed expressions inside attributes may
                                    %% contain function calls
                                    ok
                            end;
                        M ->
                            error_logger:warning_msg("Bad module for ~p: ~p~n",
                                                     [Attrib, M])
                    end;
                [] -> ok
            end;
        [] -> ok
    end;

remove(_, #file{}, _, Func, #form{type=func}) ->
    case ?Graph:path(Func, [fundef]) of
        [FunObj] ->
            remove_funlink(Func, fundef, FunObj);
        D ->
            error_logger:warning_msg("Bad fundef for ~p: ~p~n", [Func, D])
    end;

remove(Clause, #clause{kind = fundef}, pattern, _Pattern, #expr{}) ->
    [FunForm] = ?Graph:path(Clause, [{funcl, back}]),
    Arity = length(?Graph:path(Clause, [pattern])) - 1,
    case ?Graph:path(FunForm, [fundef]) of
        [] ->
            ok;
        [FunObj] ->
            case ?Graph:data(FunObj) of
                #func{arity=Arity} ->
                    ok;
                #func{name=Name} ->
                    [Module] = ?Graph:path(FunObj, [{func, back}]),
                    remove_funlink(FunForm, fundef, FunObj),
                    function_object(Module, Name, Arity, FunForm)
            end
    end;

remove(App, #expr{kind = application}, sub, Expr, #expr{}) ->
    Index = ?Graph:index(App, sub, Expr),
    if Index >= 2 ->
            [_FunRef | Args] = ?Graph:path(App, [sub]),
            Arity = length(Args) - 1,
            case ?Graph:path(App, [funref]) of
                [] ->
                    ok;
                [FunObj] ->
                    case ?Graph:data(FunObj) of
                        #func{arity=Arity} ->
                            ok;
                        #func{name=Name} ->
                            [Module] = ?Graph:path(FunObj, [{func, back}]),
                            remove_funlink(App, funref, FunObj),
                            Fun = function_object(Module, Name, Arity),
                            ?Graph:mklink(App, funref, Fun);
                        _ -> ok
                    end
            end;
       true ->
            case ?Graph:path(App, [funref]) of
                [] ->
                    ok;
                [FunObj] ->
                    remove_funlink(App, funref, FunObj)
            end
    end;

remove(App, #expr{kind = implicit_fun}, sub, _Expr, #expr{}) ->
    case ?Graph:path(App, [funref]) of
        [] ->
            ok;
        [FunObj] ->
            remove_funlink(App, funref, FunObj)
    end;

remove(_,_,_,_,_) ->
    ok.


%%% ----------------------------------------------------------------------------

impexp_list(MaybeAttrib) ->
    case MaybeAttrib of
        [Attrib] ->
            case ?Graph:data(Attrib) of
                #form{tag=export} -> {export, Attrib};
                #form{tag=import} -> {import, Attrib}
            end;
        _ ->
            false
    end.

anal_app(App) ->
    [FunRef | Args] = ?Graph:path(App, [sub]),
    Arity = case ?Graph:data(App) of
                #expr{kind=application} ->
                    length(Args);
                #expr{kind=implicit_fun} ->
                    case Args of
                        [ArityExpr|_] -> (?Graph:data(ArityExpr))#expr.value;
                        _             -> throw("Missing arity expression")
                    end;
                _ ->
                    throw(not_an_application)
            end,
    Type = (?Graph:data(App))#expr.type,
    case ?Graph:path(App, [funref]) of
        []       -> ok;
        [FunObj] -> remove_funlink(App, funref, FunObj)
    end,
    case ?Graph:data(FunRef) of
        #expr{kind=variable} ->
            todo;
        #expr{kind=atom, value=Name} ->
            case function_object_no_modq(Type, App, Name, Arity) of
                not_supported ->
                    ok;
                Fun ->
                    ?Graph:mklink(App, funref, Fun)
            end;
        #expr{kind=infix_expr, value=':'} ->
            [ModN, FunN] = ?Graph:path(FunRef, [sub]),
            case {?Graph:data(ModN), ?Graph:data(FunN)} of
                {#expr{kind=atom, value=ModName},
                 #expr{kind=atom, value=FunName}} ->
                    [Mod] = ?Graph:path(?Graph:root(),
                                        [{module, {name,'==',ModName}}]),
                    Fun = function_object(Mod, FunName, Arity),
                    ?Graph:mklink(App, funref, Fun);
                _ -> ok
            end;
        _ ->
            ok
    end.

function_object(Module, Name, Arity) ->
    function_object(Module, Name, Arity, undefined).

function_object(Module, Name, Arity, Def) ->
    case ?Graph:path(Module, ?Fun:find(Name, Arity)) of
        [Fun] ->
            ok;
        [] ->
            Fun = ?Graph:create(#func{type=global, name=Name, arity=Arity}),
            ?Graph:mklink(Module, func, Fun)
    end,
    if
        Def =:= undefined ->
            ok;
        true ->
            ?Graph:mklink(Def, fundef, Fun)
    end,
    update_fun_data(Fun),
    Fun.

%% @spec function_object_no_modq(atom(), node(), atom(), integer()) ->
%%           node() | not_supported
function_object_no_modq(Type, Expr, Name, Arity) ->
    MaybeMod = ?Graph:path(Expr, [sup, {visib,back}, scope, functx, modctx]),
    case erl_internal:bif(Name, Arity) orelse
        (Type =:= guard andalso erl_internal:type_test(Name, Arity)) of
        true ->
            bif(Name, Arity);
        false ->
            case MaybeMod of
                [Mod] ->
                    case ?Graph:path(Mod, [{funimp, {{name,'==',Name},'and',
                                                     {arity,'==',Arity}}}]) of
                        [] ->
                            function_object(Mod, Name, Arity);
                        [Fun] ->
                            Fun
                    end;
                [] ->
                    not_supported
            end
    end.

bif(Name, Arity) ->
    case ?Graph:path(?Graph:root(), [{module, {name,'==',erlang}}]) of
        [] ->
            Mod = ?Graph:create(#module{name=erlang}),
            ?Graph:mklink(?ESG:root(), module, Mod);
        [Mod] ->
            ok
    end,
    function_object(Mod, Name, Arity).

remove_funlink(From, Tag) ->
    case ?Graph:path(From, [Tag]) of
        []       -> [];
        [FunObj] -> remove_funlink(From, Tag, FunObj),
                    FunObj
    end.

remove_funlink(From, Tag, Fun) ->
    ?Graph:rmlink(From, Tag, Fun),
    Links =
        ?Graph:path(Fun, [{fundef, back}]) ++
        ?Graph:path(Fun, [{funref, back}]) ++
        ?Graph:path(Fun, [{funimp, back}]) ++
        ?Graph:path(Fun, [{funexp, back}]),
    case Links of
        [] ->
            case ?Graph:path(Fun, [{func, back}]) of
                [Mod] ->
                    ?Graph:rmlink(Mod, func, Fun),
                    ?Graph:delete(Fun);
                M ->
                    error_logger:warning_msg("Bad function link to ~p: ~p~n",
                                             [Fun, M])
            end;
        _ -> ok
    end.

export_function(Export, AryQ) ->
    [Mod] = ?Graph:path(Export, [{form, back},moddef]),
    [Name, Arity] = [ (?Graph:data(Expr))#expr.value ||
                        Expr <- ?Graph:path(AryQ, [sub])],
    export_function(AryQ, Mod, Name, Arity).

export_function(AryQ, Module, Name, Arity) ->
    Fun = function_object(Module, Name, Arity),
    ?Graph:mklink(AryQ, funref, Fun),
    ?Graph:mklink(Module, funexp, Fun).

import_function(Import, AryQ) ->
    [DefMod] = ?Graph:path(Import, [{form, back},moddef]),
    [Name, Arity] = [ (?Graph:data(Expr))#expr.value ||
                        Expr <- ?Graph:path(AryQ, [sub])],
    import_function(AryQ, Import, DefMod, Name, Arity).

import_function(AryQ, Import, DefMod, Name, Arity) ->
    [ModExpr|_] = ?Graph:path(Import, [attr]),
    #expr{value=ModName} = ?Graph:data(ModExpr),
    [Mod] = ?Graph:path(?Graph:root(), [{module, {name,'==',ModName}}]),
    Fun = function_object(Mod, Name, Arity),
    ?Graph:mklink(AryQ, funref, Fun),
    ?Graph:mklink(DefMod, funimp, Fun).


%%% ----------------------------------------------------------------------------
%%% Functions from the old referl_seminf module

-define(DIRTY_BIFS,
        [apply, cancel_timer, check_process_code, delete_module, demonitor,
         disconnect_node, erase, exit, group_leader, halt, link, load_module,
         monitor_node, open_port, port_close, port_command, port_control,
         process_flag, processes, purge_module, put, register, registered,
         resume_process, self, send, send_after, send_nosuspend, spawn,
         spawn_link, spawn_opt, suspend_process, system_flag, throw, trace,
         trace_info, trace_pattern, unlink, unregister, yield]).

update_fun_data(Func) ->
    FD = ?Graph:data(Func),
    ?Graph:update(Func, FD#func{dirty = is_true_dirty(Func)}).


%% @spec is_true_dirty(node()) -> bool()
%% @doc Return true, if the function is true dirty function, so:
%% <ul><li>is dirty BIF</li>
%%     <li>contains message sender or message receiver expression</li>
%%     <li>or uses dirty built-in function</li>
%% </ul>
is_true_dirty(SemFunNode) ->
    DirtyBifs = dirty_bifs(),
    case ?Graph:path(SemFunNode, [{{func, back}, {name, '==', erlang}}]) of
        [_Module] ->
            #func{name=Name, arity=Arity} = ?Graph:data(SemFunNode),
            case lists:member({Name, Arity}, DirtyBifs) of
                true ->
                    true;
                false ->
                    is_true_dirty(SemFunNode, DirtyBifs)
            end;
        _ -> %% []
            is_true_dirty(SemFunNode, DirtyBifs)
    end.


is_true_dirty(SemFunNode, DirtyBifs) ->
    SynFunNode = ?Graph:path(SemFunNode, [{fundef, back}]),
    case filter_exprs(SynFunNode,
                      [send_expr, receive_expr]) of
        [] ->
            Applications = filter_exprs(SynFunNode, [application, fun_expr]),
            ReferredFuns =
                [{ModData#module.name,
                  FunData#func.name,
                  FunData#func.arity}
                 || App <- Applications,
                    Fun <- ?Graph:path(App, [funref]),
                    Mod <- ?Graph:path(Fun, [{func, back}]),
                    FunData <- [?Graph:data(Fun)],
                    ModData <- [?Graph:data(Mod)]],
            case lists:filter(
                   fun
                   ({erlang, FN, FA}) ->
                                     lists:member({FN, FA}, DirtyBifs);
                   (_) ->
                                     false
                             end,
                   ReferredFuns) of
                [] -> false;
                _ ->  true
            end;
        _ ->
            true
    end.


filter_exprs(FnForms, Kinds) ->
    lists:filter(
      fun(E) ->
              Kind = (?Graph:data(E))#expr.kind,
              lists:member(Kind, Kinds)
      end,
      lists:flatten(
        [?Graph:path(F, [funcl, {scope,back}, visib, {sup, back}])
         || F <- FnForms])).


%% @doc Creates a list of name-arity pairs from the names of dirty bifs.
%%      Example: exit becomes to [{exit, 1}, {exit, 2}]
dirty_bifs() ->
    lists:flatten([find_bifs_by_name(BifName) || BifName <- ?DIRTY_BIFS]).


%% @private
find_bifs_by_name(SearchedName) ->
    [{Name, Arity} || {Name, Arity} <- erlang:get_module_info(erlang, exports),
                      Name == SearchedName].
