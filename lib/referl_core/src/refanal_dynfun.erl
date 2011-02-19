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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Dynamic function call analyser.
%%%
%%% @author Dániel Horpácsi <daniel_h@inf.elte.hu>

-module(refanal_dynfun).
-vsn("$Rev: 5413 $ ").
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").

%% @private
schema() ->
    [{func, [{may_be, func}]},
     {expr, [{dynfuneref, func}, {ambfuneref, func},
	     {dynfunlref, func}, {ambfunlref, func}]}
    ].

%% @private
externs(_) -> [].

%% =============================================================================

%% @private
insert(Parent, _, {Tag, Child}, _) ->
    case ?Anal:data(Parent) of
        #file{type=module} when Tag == form ->
            walk(fun add/3, [{Child, ?NodeSync:get_node(module, Parent)}]);
        #file{} ->
            ok;

        #form{type=export} ->
            ok;
        #form{type=import} ->
            ok;
        #form{type=func} when Tag == funcl ->
            File = ?Anal:parent(Parent),
            walk(fun add/3,[{Child,{?NodeSync:get_node(module,File),Child}}]);
        #form{type=record} ->
            File = ?Anal:parent(Parent),
            walk(fun add/3,[{Child,{File,none}}]);
        #form{} ->
            ok;

        #clause{type=fundef} when Tag == name ->
            ok;
        #clause{type=fundef} when Tag == pattern ->
            ok;
        #clause{type=fundef} ->
            File = ?Anal:parent(?Anal:parent(Parent)),
            walk(fun add/3,[{Child,{?NodeSync:get_node(module,File), Parent}}]);
        #clause{} ->
            [FunCl] = ?Graph:path(Parent, [functx]),
            File = ?Anal:parent(?Anal:parent(FunCl)),
            walk(fun add/3,[{Child,{?NodeSync:get_node(module, File), FunCl}}]);

        #expr{type=T} when T == funref; T == funlist ->
            ok;
        #expr{role=attr} ->
            ok;
        #expr{type=T} when T == application; T == implicit_fun ->
            Ctx = ctx_info(Parent),
            del_ref(Parent, Child),
            add(Parent, ?Anal:data(Parent), Ctx),
            walk(fun add/3, [{Child, Ctx}]);
        #expr{type=T, value=V} when (T == infix_expr andalso V == ':');
                                    T ==  arglist ->
            Ctx = ctx_info(Parent),
            App = ?Anal:parent(Parent),
            del_ref(App, Child),
            add(App, ?Anal:data(App), Ctx),
            walk(fun add/3, [{Child, Ctx}]);
        #expr{} ->
            Ctx = ctx_info(Parent),
            walk(fun add/3, [{Child, Ctx}]);

        #typexp{} ->
            ok
    end,
    refanal_fun:update_funprop(Child).

add(Form, #form{type=func}, Mod) ->
    Cls = [Cl || {funcl, Cl} <- ?Anal:children(Form)],
    [{Cl, {Mod, Cl}} || Cl <- Cls];
add(Form, #form{type=record}, Mod) ->
    [{T, Mod} || {_, T} <- ?Anal:children(Form)];
add(_, #form{}, _) ->
    [];

add(Clause, #clause{}, Ctx) ->
    [{Chld, Ctx} || {_, Chld} <- ?Anal:children(Clause)];

add(Expr, #expr{type=application}, {Mod, _} = Ctx) ->
    [{esub, FunName}, {esub, ArgLst}] = ?Anal:children(Expr),
    Args = [A || {esub, A} <- ?Anal:children(ArgLst)],
    case ?Graph:path(Expr, [funlref]) ++  ?Graph:path(Expr, [funeref]) of
        [] -> add_funref(Expr, FunName, length(Args), Mod);
        [Function] ->
            [Module] = ?Graph:path(Function, [{func, back}]),
            case {?Graph:data(Module), ?Graph:data(Function)} of
                {#module{name = erlang}, #func{name = apply, arity = 3}} ->
                    add_funref(Expr, FunName, length(Args), Mod);
                _ -> ok
            end;
        _ -> ok
    end,
    [{FunName, Ctx} | [{A, Ctx} || A <- Args]];
add(Expr, #expr{}, Ctx) ->
    [{C, Ctx} || {_, C} <- ?Anal:children(Expr)];

add(TExpr, #typexp{}, Mod) ->
    [{C, {Mod, none}} || {_, C} <- ?Anal:children(TExpr)].


%% -----------------------------------------------------------------------------


add_funref(App, FunId, Arity, LM) when is_integer(Arity) ->
    add_funref(App, Arity, LM, lookup_funID(FunId, ?Anal:data(FunId)));

add_funref(App, Arity, LM, FunInfo) when is_integer(Arity) ->
    case FunInfo of
        {}       -> ok;
        {{}, {}} -> ok;

        {{ModName}, {FunName}} -> % case of implicit fun expressions
            ?NodeSync:add_ref(func, {dyneref, App}, {ModName, {FunName, Arity}});
        {{ModRef, ModName}, {_FunRef, FunName}} ->
            not anal_applycall({ModName, FunName, Arity}, App, LM) andalso
                begin
                    ?NodeSync:add_ref(module, {ref, ModRef}, ModName),
                    ?NodeSync:add_ref(func, {dyneref, App}, {ModName, {FunName, Arity}})
                end;
        {_FunRef, FunName} when is_atom(FunName) ->
            not anal_applycall({erlang, FunName, Arity}, App, LM) andalso
                ?NodeSync:add_ref(func, {dynlref, App}, {LM, {FunName, Arity}});

        {{}, {_FunRef, FunName}} ->
            ?NodeSync:add_ref(func, {dyneref, App}, {-1, {FunName, Arity}});
        {{ModRef, ModName}, {}} ->
            ?NodeSync:add_ref(module, {ref, ModRef}, ModName),
            ?NodeSync:add_ref(func, {dyneref, App}, {ModName, {-1, Arity}});

        {L, L2} when is_list(L); is_list(L2) ->
            [add_funref(App, Arity, LM, {M, F}) ||
                M <- lists:flatten([L]), F <- lists:flatten([L2])];
        L when is_list(L) -> [add_funref(App, Arity, LM, E) || E <- L]
    end.

anal_applycall({erlang, apply, 3}, App, LM) ->
    [_, {esub, ApplyArgs}] = ?Anal:children(App),
    [{esub, ApplyModRef}, {esub, ApplyFunRef}, {esub, ListOfArgs}] = ?Anal:children(ApplyArgs),
    R =  lookup_atoms(ApplyModRef, ApplyFunRef),
    Arity = case listcons_length(ListOfArgs) of
                incalculable -> -1;
                {I} -> -1*I-1;
                I -> I
            end,
    add_funref(App, Arity, LM, R),
    true;
anal_applycall(_, _, _) ->
    false.

%% =============================================================================
%% Function recognition

is_cons_expr(Node) -> (?Graph:data(Node))#expr.type == cons.
is_atom_expr(Node) -> (?Graph:data(Node))#expr.type == atom.
is_fun_expr(Node) -> (?Graph:data(Node))#expr.type == implicit_fun.
atom_value(Node) ->  #expr{type = atom, value = Val} = ?Graph:data(Node), Val.

-define(MultRefWarning, ?d("multiple function names recognized")).

lookup_funID(N, #expr{type=atom, value=Name}) ->
    {N, Name};
lookup_funID(C, #expr{type=infix_expr, value=':'}) ->
    [{esub, Mod}, {esub, Fun}] = ?Anal:children(C),
    lookup_atoms(Mod, Fun);
lookup_funID(N, #expr{}) ->
    lookup_funexpr_via_dataflow(N);
lookup_funID(_, _) -> {}.

lookup_funexpr_via_dataflow(N) ->
    Ns = ?Dataflow:reach([N], [back]),

    L = [N2 || N2 <- Ns, N2 /= N, ?Graph:class(N2) == expr, is_fun_expr(N2)],
    LL = lists:usort(lists:flatten([?Graph:path(N2, [funlref]) || N2 <- L])),
    LE = lists:usort(lists:flatten([?Graph:path(N2, [funeref]) || N2 <- L])),

    case LE of
        [] -> [{}];
        [Node] ->
            [Mod] = ?Graph:path(Node, [{func, back}]),
            [{{reflib_module:name(Mod)}, {reflib_function:name(Node)}}];
        Nodes when is_list(Nodes) ->
            ?MultRefWarning,
            [begin
                 [Mod] = ?Graph:path(Node, [{func, back}]),
                 {{reflib_module:name(Mod)}, {reflib_function:name(Node)}}
             end || Node <- Nodes]
    end ++
        case LL of
            [] -> [{}];
            [Node] ->
                [{dummy, reflib_function:name(Node)}];
            Nodes when is_list(Nodes) ->
                ?MultRefWarning,
                [{dummy, reflib_function:name(Node)} || Node <- Nodes]
        end.


lookup_atoms(ModRef, FunRef) ->
    {lookup_atom(ModRef, ?Anal:data(ModRef)),
     lookup_atom(FunRef, ?Anal:data(FunRef))}.

lookup_atom(N, #expr{type=atom, value=Name}) ->
    {N, Name};
lookup_atom(N, #expr{type=T})
  when T == variable    ; T == block_expr  ;
       T == if_expr     ; T == match_expr  ;
       T == case_expr   ; T == try_expr    ;
       T == parenthesis ->
    lookup_atom_via_dataflow(N);
lookup_atom(_, _) -> {}.

lookup_atom_via_dataflow(N) ->
    Ns = ?Dataflow:reach([N], [back]),
    L = [{N2, atom_value(N2)} || N2 <- Ns, N2 /= N,
                                 ?Graph:class(N2) == expr, is_atom_expr(N2)],
    case lists:usort(lists:flatten(L)) of
        [] -> {};
        [{Node, Atom}] when is_atom(Atom) -> {Node, Atom};
        [{_Node, Atom} | _] = L when is_atom(Atom) -> ?MultRefWarning, L
    end.

%% =============================================================================
%% Calculating a list's length associated with a node

sum_lengths({LL}, incalculable) when is_integer(LL) -> {LL};
sum_lengths( LL , incalculable) when is_integer(LL) -> {LL};
sum_lengths({LL}, {CL}) when is_integer(LL), is_integer(CL) -> {LL + CL};
sum_lengths( LL,  {CL}) when is_integer(LL), is_integer(CL) -> {LL + CL};
sum_lengths({LL},  CL ) when is_integer(LL), is_integer(CL) -> {LL + CL};
sum_lengths( LL,   CL ) when is_integer(LL), is_integer(CL) ->  LL + CL.

listcons_length(ListExpr) -> listcons_length(ListExpr, ?Anal:data(ListExpr)).
listcons_length(ListExpr, #expr{type=cons}) ->
    case ?Anal:children(ListExpr) of
        [] ->
            0;
        [{esub, Head}] ->
            list_length(Head);
        [{esub, Head}, {esub, Tail}] ->
            case lists:member(Tail, ?Graph:path(ListExpr, [flow])) of 
            %% eliminating infinite loops -- ? should be refined ?
                true -> sum_lengths(list_length(Head), incalculable);
                false -> sum_lengths(list_length(Head), listcons_length(Tail))
            end
    end;
listcons_length(N, #expr{type=T})
  when T == variable    ; T == block_expr  ;
       T == if_expr     ; T == match_expr  ;
       T == case_expr   ; T == try_expr    ;
       T == parenthesis ->
    Ns = ?Dataflow:reach([N], [back]),
    L = [N2 || N2 <- Ns, N2 /= N,
               ?Graph:class(N2) == expr, is_cons_expr(N2)],
    case L of
        []  -> incalculable;
        [E] -> listcons_length(E);
        _   -> [E|_] = lists:reverse(L), % is it correct?
               listcons_length(E)
    end;
listcons_length(_ListExpr, _Data) -> incalculable.

list_length(ListExpr) -> list_length(ListExpr, ?Anal:data(ListExpr)).
list_length(ListExpr, #expr{type = list}) ->
    length(?Anal:children(ListExpr));
list_length(_ListExpr, _Data) ->
    {1}.

%% =============================================================================

%% @private
remove(Parent, _, {Tag, Child}, _) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            walk(fun del/3, [{Child, Parent}]);
        #form{type=func} when Tag == funcl ->
            walk(fun del/3, [{Child, Child}]);
        #form{type=T} when T == record ->
            walk(fun del/3, [{Child, ?Anal:parent(Parent)}]);
        #form{} -> ok;

        #clause{type=fundef} when Tag == pattern ->
            ok;
        #clause{type=fundef} ->
            walk(fun del/3, [{Child, Parent}]);
        #clause{} ->
            [FunCl] = ?Graph:path(Parent, [functx]),
            walk(fun del/3, [{Child, FunCl}]);

        #expr{type=attr} ->
            ok;
        #expr{type=funref} ->
            ok;
        #expr{type=funlist} ->
            ok;
        #expr{type=T} when T == application orelse T == implicit_fun ->
            FunCl = get_funcl(Parent),
            del_ref(Parent, Child),
            walk(fun del/3, [{Child, FunCl}]);
        #expr{type=infix_expr, value=':'} ->
            FunCl = get_funcl(Parent),
            App = ?Anal:parent(Parent),
            del_ref(App, Child),
            walk(fun del/3, [{Child, FunCl}]);
        #expr{type=arglist} ->
            FunCl = get_funcl(Parent),
            App = ?Anal:parent(Parent),
            del(App, ?Anal:data(App), FunCl),
            walk(fun del/3, [{Child, FunCl}]);
        #expr{} ->
            FunCl = get_funcl(Parent),
            walk(fun del/3, [{Child, FunCl}]);

        #typexp{} ->
            ok
    end.


del(Form, #form{type=record}, _Mod) ->
    [{T, none} || {_, T} <- ?Anal:children(Form)];
del(_Form, #form{}, _Mod) ->
    [];

del(Clause, #clause{}, T) ->
    [{Chld, T} || {_, Chld} <- ?Anal:children(Clause)];

del(Expr, #expr{type=Type}, Top) when Type == implicit_fun;
                                      Type == application ->
    case ?Graph:path(Expr, [dynfunlref]) of
        [Fun] -> ?NodeSync:del_ref(func, {dynlref, Expr}, Fun);
        [] ->
            case ?Graph:path(Expr, [dynfuneref]) of
                [Fun] ->
                    ?NodeSync:del_ref(func, {dyneref, Expr}, Fun),
                    [{esub, FunName}| _] = ?Anal:children(Expr),
                    [{esub, ModName}, _] = ?Anal:children(FunName),
                    [Mod] = ?Graph:path(ModName, [modref]),
                    ?NodeSync:del_ref(module, {ref, ModName}, Mod);
                [] ->
                    ok
            end
    end,
    [{Chld, Top} || {_, Chld} <- ?Anal:children(Expr)];
del(Expr, #expr{}, Top) ->
    [{Chld, Top} || {_, Chld} <- ?Anal:children(Expr)];
del(TExpr, #typexp{}, Ctx) ->
    [{C, Ctx} || {_, C} <- ?Anal:children(TExpr)].

%% @private
update(_,_) ->
    ok.


%% =============================================================================

walk(Fun, [{Node, Ctx} | Tail]) ->
    walk(Fun, Fun(Node, ?Anal:data(Node), Ctx) ++ Tail);
walk(_, []) -> ok.

ctx_info(Expr)->
    FunCl = get_funcl(Expr),
    File = ?Anal:parent(?Anal:parent(FunCl)),
    {?NodeSync:get_node(module, File), FunCl}.

get_funcl(Expr) ->
    [FunCl] = ?Graph:path(Expr, [top, {body, back}, functx]) ++
              ?Graph:path(Expr, [top, {guard, back}, functx]) ++
              ?Graph:path(Expr, [top, {pattern, back}, functx]), %% ++
%% %%    ?Graph:path(Expr, [top, {name, back}, functx]),
    FunCl.

del_ref(App, Child)->
    del_dynref(App, Child),
    del_ambdynref(App, Child).

del_dynref(App, Child) ->
    case ?Graph:path(App, [dynfunlref]) of
        [Fun] -> ?NodeSync:del_ref(func, {dynlref, App}, Fun);
        [] ->
            case {?Graph:path(App, [dynfuneref]),
                  ?Graph:path(Child, [modref])} of
                {[Fun], [Mod] } ->
                    ?NodeSync:del_ref(func, {dyneref, App}, Fun),
                    ?NodeSync:del_ref(module, {ref, Child}, Mod);
                {[Fun], []} ->
                    ?NodeSync:del_ref(func, {dyneref, App}, Fun);
                {[], _} ->
                    ok
            end
    end.

del_ambdynref(App, Child) ->
    case ?Graph:path(App, [ambfunlref]) of
        [Fun] -> ?NodeSync:del_ref(func, {amblref, App}, Fun);
        [] ->
            case {?Graph:path(App, [ambfuneref]),
                  ?Graph:path(Child, [modref])} of
                {[Fun], [Mod] } ->
                    ?NodeSync:del_ref(func, {amberef, App}, Fun),
                    ?NodeSync:del_ref(module, {ref, Child}, Mod);
                {[Fun], []} ->
                    ?NodeSync:del_ref(func, {amberef, App}, Fun);
                {[], _} ->
                    ok
            end
    end.
