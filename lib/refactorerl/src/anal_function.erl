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

-module(anal_function).
-vsn("$Rev: 1247 $").
-behaviour(refac_anal).

%% Interface exports
-export([function/2]).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @spec function(atom(), integer()) -> path()
%% @doc Path from a #module{} to the #function{} defined in it with name `Name'
%% and arity `Arity'.
function(Name, Arity) ->
    [{func, {{name, '==', Name}, 'and', {arity, '==', Arity}}}].


%% @private
init() ->
    [{func, record_info(fields, func), []},
     {form, [{fundef, func}]},
     {module, [{func, func}, {funexp, func}, {funimp, func}]},
     {expr,   [{funref, func}]}
    ].

%% @private
insert(File, #file{}, _, Func, #form{type=func}) ->
    [N] = ?GRAPH:path(Func, [{funcl,1}, name]),
    #expr{value=Name} = ?GRAPH:data(N),
    Arity = length(?GRAPH:path(Func, [{funcl,1}, pattern])),
    [Mod] = ?GRAPH:path(File, anal_module:file_module()),
    function_object(Mod, Name, Arity, Func);

insert(_,_,_,App, #expr{kind=application}) ->
    [FunRef | Args] = ?GRAPH:path(App, [sub]),
    case ?GRAPH:data(FunRef) of
        #expr{kind=variable} ->
	    todo;
        #expr{kind=atom, value=Name} ->
            [Mod] = ?GRAPH:path(App, anal_context:expr_module()),
            case [ImpFun || ImpFun <- 
                                ?GRAPH:path(App, anal_context:expr_module() 
                                            ++ [funimp]),
                            (?GRAPH:data(ImpFun))#func.name == Name andalso 
                            (?GRAPH:data(ImpFun))#func.arity == length(Args)]
                of
                [] ->
                    Fun = function_object(Mod, Name, length(Args));
                [Fun] ->
                    ok
            end,
            ?GRAPH:mklink(App, funref, Fun);
                    
        #expr{kind=module_qualifier} ->
            [ModN, FunN] = ?GRAPH:path(FunRef, [sub]),
            case {?GRAPH:data(ModN), ?GRAPH:data(FunN)} of
                {#expr{kind=atom, value=ModName},
                 #expr{kind=atom, value=FunName}} ->
                    [Mod] = ?GRAPH:path(?GRAPH:root(),
                                        anal_module:module(ModName)),
                    Fun = function_object(Mod, FunName, length(Args)),
                    ?GRAPH:mklink(App, funref, Fun);
                _ -> ok
            end;
	_ ->
	    ok
    
    end;

insert(Fun, #expr{kind=fun_expr}, sub, Expr, #expr{kind=arity_qualifier}) ->
    LinkFun = fun (Mod, NameExp, ArityExp) -> 
                      #expr{value=Name} = ?GRAPH:data(NameExp),
                      #expr{value=Arity} = ?GRAPH:data(ArityExp),
                      FunObj = function_object(Mod, Name, Arity),
                      ?GRAPH:mklink(Fun, funref, FunObj)
              end,
    case ?GRAPH:path(Expr, [sub]) of
        [FunExpr, ArityExp] ->
            case ?GRAPH:data(FunExpr) of
                #expr{kind=atom} ->
                    [Mod] = ?GRAPH:path(Fun, anal_context:expr_module()),
                    LinkFun(Mod, FunExpr, ArityExp);
                #expr{kind=module_qualifier} ->
                    [Mod] = ?GRAPH:path(FunExpr, anal_module:referred_mod()),
                    [_ModExpr, NameExpr] = ?GRAPH:path(FunExpr, [sub]),
                    LinkFun(Mod, NameExpr, ArityExp);
                _ ->
                    ok
            end;
        _ ->
            ok
    end;

insert(_, _, _, Expr, #expr{kind=arity_qualifier}) ->
    case ?GRAPH:path(Expr, anal_context:expr_super() ++ [{attr, back}]) of
        [Attrib] ->
            case ?GRAPH:data(Attrib) of
                #form{tag=export} ->
                    export_function(Attrib, Expr);
                #form{tag=import} ->
                    import_function(Attrib, Expr)
            end;
        _ -> ok
    end;

insert(_,_,_,_,_) ->
    ok.

%% @private
remove(_, _, _, Expr, #expr{kind=Kind})
  when Kind == fun_expr; Kind == application; Kind == arity_qualifier ->
    case ?GRAPH:path(Expr, [funref]) of
        [Fun] ->
            remove_funlink(Expr, funref, Fun),
            case ?GRAPH:path(Expr,
                             anal_context:expr_super() ++
                             [{attr, back}]) of
                [Attrib] ->
                    case ?GRAPH:path(Attrib,
                                        [{form, back}] ++
                                        anal_module:file_module()) of
                        [Mod] ->
                            case ?GRAPH:data(Attrib) of
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
    case ?GRAPH:path(Func, [fundef]) of
        [FunObj] ->
            remove_funlink(Func, fundef, FunObj);
        D ->
            error_logger:warning_msg("Bad fundef for ~p: ~p~n", [Func, D])
    end;

remove(_,_,_,_,_) ->
    ok.

function_object(Module, Name, Arity) ->
    function_object(Module, Name, Arity, undefined).

function_object(Module, Name, Arity, Def) ->
    case ?GRAPH:path(Module, function(Name, Arity)) of
        [Fun] ->
             ok;
        [] ->
            Fun = ?GRAPH:create(#func{type=global, name=Name, arity=Arity}),
            ?GRAPH:mklink(Module, func, Fun)
    end,
    if
        Def =:= undefined ->
            ok;
        true ->
            ?GRAPH:mklink(Def, fundef, Fun)
    end,
    Fun.

remove_funlink(From, Tag, Fun) ->
    ?GRAPH:rmlink(From, Tag, Fun),
    Links =
        ?GRAPH:path(Fun, [{fundef, back}]) ++
        ?GRAPH:path(Fun, [{funref, back}]) ++
        ?GRAPH:path(Fun, [{funimp, back}]) ++
        ?GRAPH:path(Fun, [{funexp, back}]),
    case Links of
        [] ->
            case ?GRAPH:path(Fun, [{func, back}]) of
                [Mod] ->
                    ?GRAPH:rmlink(Mod, func, Fun),            
                    ?GRAPH:delete(Fun);
                M ->
                    error_logger:warning_msg("Bad function link to ~p: ~p~n",
                                             [Fun, M])
            end;
        _ -> ok
    end.

export_function(Export, AryQ) ->
    [Mod] = ?GRAPH:path(Export, [{form, back}] ++ anal_module:file_module()),
    [Name, Arity] = [ (?GRAPH:data(Expr))#expr.value ||
                        Expr <- ?GRAPH:path(AryQ, [sub])],
    Fun = function_object(Mod, Name, Arity),
    ?GRAPH:mklink(AryQ, funref, Fun),
    ?GRAPH:mklink(Mod, funexp, Fun).

import_function(Import, AryQ) ->
    [ModExpr |_] = ?GRAPH:path(Import, [attr]),
    #expr{value=ModName} = ?GRAPH:data(ModExpr),
    [DefMod] = ?GRAPH:path(Import, [{form, back}] ++ anal_module:file_module()),
    [Mod] = ?GRAPH:path(?GRAPH:root(), anal_module:module(ModName)),
    [Name, Arity] = [ (?GRAPH:data(Expr))#expr.value ||
                        Expr <- ?GRAPH:path(AryQ, [sub])],
    Fun = function_object(Mod, Name, Arity),
    ?GRAPH:mklink(AryQ, funref, Fun),
    ?GRAPH:mklink(DefMod, funimp, Fun).
