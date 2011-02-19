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

%%% @doc Variable analyser.
%%%
%%% The representation of list (and binary) comprehensions is illustrated by
%%% this diagram:
%%% <img src="listcomp.png"/>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refanal_var).
-vsn("$Rev: 5575 $ ").
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @private
schema() ->
    [{variable, record_info(fields, variable), [{varintro, expr}]},
     {clause, [{scope, clause}, {visib, expr},
               {vardef, variable}, {varvis, variable}]},
     {expr,   [{varref, variable}, {varbind, variable}]}
    ].

%%% @private
externs(_) -> [].

%% TODO: list comprehensions
%%% @private
insert(Parent, _Pre, {Tag, Child}, _) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            [ins_funclause(Cl) || {funcl, Cl} <- ?Anal:children(Child)],
            ok;
        #form{} when Tag == funcl ->
            ins_funclause(Child);
        #form{} when Tag == eattr; Tag == tattr ->
            ok;
        _ -> %% TODO eliminate this branch
            FunCl = find_funcl(Parent, ?Anal:data(Parent)),
            case FunCl of
                none -> ok;
                _ -> 
                    del(FunCl),
                    ins_funclause(FunCl)
            end
        %% #clause{} ->
        %%     ok; %%throw('TODO');
        %% #typexp{} ->
        %%     ok;
        %% #expr{role=attr} ->
        %%     ok;
        %% #expr{} ->
        %%     ok %%throw('TODO')
    end.

find_funcl(Node, #clause{}) ->
    [FunCl] = ?Graph:path(Node, [functx]),
    FunCl;
find_funcl(_Node, #expr{role=attr}) -> none;
find_funcl(Node, #expr{}) ->
    [FunCl] = ?Graph:path(Node, [top, {body, back}, functx]) ++
              ?Graph:path(Node, [top, {guard, back}, functx]) ++  
              ?Graph:path(Node, [top, {pattern, back}, functx]) ++
              ?Graph:path(Node, [top, {name, back}, functx]),
    FunCl;
find_funcl(_, _) -> none.

del(Node) ->
    del_link(Node, ?Anal:data(Node)),
    [del(Child) || {_, Child} <- ?Anal:children(Node)].

del_link(Node, #clause{})->
    [?Graph:rmlink(Node, scope,  Sc)   || Sc    <- ?Graph:path(Node, [scope])],
    [?Graph:rmlink(Node, visib,  Expr) || Expr  <- ?Graph:path(Node, [visib])],
    [?Graph:rmlink(Node, vardef, Var)  || Var   <- ?Graph:path(Node, [vardef])],
    [?Graph:rmlink(Node, varvis, Var)  || Var   <- ?Graph:path(Node, [varvis])];
%% [{scope, clause}, {visib, expr}, {vardef, variable}, {varvis, variable}]
del_link(Node, #expr{})->
    Vars = ?Graph:path(Node, [varref]) ++ ?Graph:path(Node, [varbind]),
    [[?Graph:rmlink(Cl,  varvis,   Var) 
        || Cl  <- ?Graph:path(Var, [{varvis, back}])] || Var <- Vars],
    [[?Graph:rmlink(Cl,  vardef,   Var)
        || Cl  <- ?Graph:path(Var, [{vardef, back}])] || Var <- Vars],
    [?Graph:rmlink(Node, varref,   Var) || Var <- ?Graph:path(Node, [varref])],
    [?Graph:rmlink(Node, varbind,  Var) || Var <- ?Graph:path(Node, [varbind])],
    [?Graph:rmlink(Var,  varintro, Node)
        || Var <- ?Graph:path(Node, [{varintro, back}])];
%% [{varref, variable}, {varbind, variable},
%%  {{varintor,back}, variable}]; %%{variable, [{varintro, expr}]}
del_link(_,_) -> ok.

ins_funclause(Clause) ->
    add(Clause, ?Anal:data(Clause), {Clause, scope}),
    upd_cont(Clause, true, env_empty(), env_empty()).

%%% @private
remove(Parent, _, {Tag, Child}, _) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            [del(Cl, ?Graph:data(Cl), Cl) ||
                {funcl, Cl} <- ?Anal:children(Child)],
            ok;
        #form{} when Tag == funcl ->
            del(Child, ?Graph:data(Child), Child);
        #form{} when Tag == eattr; Tag == tattr ->
            ok;
        _ -> del(Child)
        %% #clause{} ->
        %%     ok; %%throw('TODO');
        %% #typexp{} ->
        %%     ok;
        %% #expr{role=attr} ->
        %%     ok;
        %% #expr{} ->
        %%     ok %%throw('TODO')
    end.

%%% @private
update(_,_) ->
    %% TODO. (probably only variable name updates are interesting)
    ok.




add(Clause, #clause{} = CD, {Sc, CV}) ->
    ?Anal:update(Clause, CD#clause{var=CV}),
    Scope =
        if
            CV == scope -> Clause;
            true        -> Sc
        end,
    ?Graph:mklink(Clause, scope, Scope),

    [begin
         ?Graph:mklink(Clause, visib, Child),
         add(Child, ?Anal:data(Child), Scope)
     end || Child <- visib(Clause, CD)],
    ok;


add(Expr, #expr{type=Type}, Scope) ->
    case ?Anal:children(Expr) of
        [] -> ok;
        [{esub, _} | _] = Children ->
            [add(E, ?Anal:data(E), Scope) || {_, E} <- Children],
            ok;
        Clauses ->
            lists:foldl(
              fun ({_Tag, Clause}, CVI) ->
                      case CVI of
                          {CV, CT} -> ok;
                          CV       -> CT = CV
                      end,
                      add(Clause, ?Anal:data(Clause), {Scope, CV}),
                      CT
              end,
              clausevar(Type),
              Clauses)
    end.

%% `pexpr' and `hexpr' are only used in comprehensions
visib(Clause, #clause{type = compr}) ->
    [hd(child_visib(Clause))];
visib(Clause, #clause{type = pexpr}) ->
    compr_visib(Clause);
visib(_Clause, #clause{type = hexpr}) ->
    [];
visib(Clause, #clause{type = expr}) ->
    case ?Anal:data(?Anal:parent(Clause)) of
        #expr{type = filter} -> compr_visib(Clause);
        _                    -> child_visib(Clause)
    end;
visib(Clause, #clause{}) ->
    child_visib(Clause).


child_visib(Clause) ->
    [Child || {T,Child} <- ?Anal:children(Clause), T =/= name].

compr_visib(Clause) ->
    [{_, First}] = ?Anal:children(Clause),
    Parent = ?Anal:parent(Clause),
    Compr  = ?Anal:parent(Parent),
    case lists:dropwhile(fun ({_,E}) -> E =/= Parent end,
                         ?Anal:children(Compr)) of
        [{body, Parent}, {body, Next} | _] ->
            [First, Next];
        [{body, Parent}] ->
            %% This starts to get messy here. Maybe `add/3' should have a
            %% clause that deals with the whole structure.
            Top = ?Anal:parent(Compr),
            [{exprcl, Head} | _] = ?Anal:children(Top),
            [{body, Next}]       = ?Anal:children(Head),
            [First, Next]
    end.


del(Clause, #clause{var=CV}, Sc) ->
    case CV of
        scope ->
            Scope = Clause,
            [?Graph:delete(V) || V <- ?Graph:path(Clause, [vardef])];
        _ ->
            Scope = Sc,
            [?Graph:rmlink(V, varintro, I) ||
                I <- ?Graph:path(Clause, [visib]),
                V <- ?Graph:path(I, [{varintro, back}])],
            [?Graph:rmlink(Clause, varvis, V) ||
                V <- ?Graph:path(Clause, [varvis])]
    end,
    ?Graph:rmlink(Clause, scope, Scope),
    [begin
         ?Graph:rmlink(Clause, visib, Expr),
         del(Expr, ?Anal:data(Expr), Scope)
     end || Expr <- ?Graph:path(Clause, [visib])],
    ok;

del(Expr, #expr{type=variable}, _Sc) ->
    case ?Graph:path(Expr, [varref]) of
        [Var] -> ?Graph:rmlink(Expr, varref, Var);
        [] ->
            case ?Graph:path(Expr, [varbind]) of
                [Var] -> ?Graph:rmlink(Expr, varbind, Var);
                [] -> ok
            end
    end;

del(Expr, #expr{}, Sc) ->
    [del(Child, ?Anal:data(Child), Sc) || {_, Child} <- ?Anal:children(Expr)],
    ok.



get_var(Scope, Name) ->
    case ?Graph:path(Scope, [{vardef, {name, '==', Name}}]) of
        [Var] -> Var;
        [] ->
            Var = ?Graph:create(#variable{name=Name}),
            ?Graph:mklink(Scope, vardef, Var),
            Var
    end.

% todo Dialyzer claims this function is unused.
%      Remove if true, refute otherwise.
% get_varenv(Clause, Expr) ->
%     All = ?Graph:path(Clause, [varvis]),
%     {true, Prev} =
%         lists:foldl(
%           fun
%               (E, {false, _} = Acc) when E =/= Expr -> Acc;
%               (E, {_, VL}) ->
%                   {true, VL -- ?Graph:path(E, [{varintro, back}])}
%           end,
%           {false, All},
%           ?Graph:path(Clause, [visib])),
%     lists:foldl(
%       fun(V, Env) -> env_insert((?Graph:data(V))#variable.name, V, Env) end,
%       env_empty(),
%       Prev).





walk_children(Clause, #clause{}) ->
    ?Graph:path(Clause, [visib]);
walk_children(Expr, #expr{} = Data) ->
    case exprclass(Data) of
        leaf     -> [];
        simple   -> [E || {esub, E} <- ?Anal:children(Expr)];
        compound -> ?Graph:path(Expr, [clause]);
        compr    -> ?Graph:path(Expr, [{clause, {type,'==',compr}}])
    end.

walk_parent(_Clause, #clause{type=fundef}) -> [];
walk_parent(Clause, #clause{}) ->
    ?Graph:path(Clause, [{clause, back}]);
walk_parent(Expr, #expr{}) ->
    case ?Graph:path(Expr, [{visib, back}]) of
        [] -> [?Anal:parent(Expr)];
        Cl -> Cl
    end.


%% Update state: context is generated by walking downwards the tree. Input env
%% contains active variable bindings, it is updated when the next node in the
%% tree should "see" variables from the previous node. Output env accumulates
%% variable bindings created in a subtree.
-record(upd_st, {ctx, inenv, outenv}).

%% upd_cont -- continue applying updates to variable links starting from
%% `Node'.
%% TODO: detect when there are no updates and stop
upd_cont(Node, Include, InEnv, OutEnv) ->
    {NodeCtx, Stack} = cont_stack(Node),
    NodeSt = #upd_st{ctx = NodeCtx, inenv = InEnv, outenv = OutEnv},
    InitSt =
        if
            Include -> upd_tree(Node, NodeSt);
            not Include -> NodeSt
        end,
    lists:foldl(
      fun ({Parent, ParCtx, Children}, St) ->
              %% FIXME: check states
              upd_nodes(Parent, ?Anal:data(Parent), Children,
                        St#upd_st{ctx = ParCtx},
                        St)
      end,
      InitSt,
      Stack).

%% cont_stack -- prepare the stack of update contexts for the parent chain of
%% `Node'. Returns the context for `Node' and the stack. Stack elements
%% contain a node, its context, and its remaining children.
cont_stack(Node) ->
    case walk_parent(Node, ?Anal:data(Node)) of
        [] ->
            Parent = ?Anal:parent(Node),
            {upd_ctx(Parent, ?Anal:data(Parent), init), []};
        [Parent] ->
            {Ctx, Stack} = cont_stack(Parent),
            {_, [Node | Tail]} =
                lists:splitwith(fun (N) -> N =/= Node end,
                                walk_children(Parent, ?Anal:data(Parent))),
            {upd_ctx(Node, ?Anal:data(Node), Ctx),
             [{Parent, Ctx, Tail} | Stack]}
    end.


upd_tree(Node, St = #upd_st{ctx = Ctx, inenv = InEnv}) ->
    Data = ?Anal:data(Node),
    upd_nodes(Node, Data, walk_children(Node, Data),
              St, #upd_st{ctx = upd_ctx(Node, Data, Ctx),
                          inenv = InEnv,
                          outenv = env_empty()}).

upd_nodes(Parent, Data, Children, ParSt, ChildSt) ->
    #upd_st{outenv = ResultEnv} =
        lists:foldl(
          fun (Child, St) -> upd_tree(Child, St) end,
          ChildSt,
          Children),
    upd(Parent, Data, ResultEnv, ParSt).


%% Update context information
-record(upd_ctx, {scope,  % active scope node
                  top,    % true in top expressions
                  shadow, % true when shadowing should be performed
                  branch  % true when there were branch clauses previously
                 }).

upd_ctx(_Form, #form{}, init) ->
    #upd_ctx{top = true, shadow = true, branch = false};
upd_ctx(Clause, #clause{var = scope}, _Ctx) ->
    #upd_ctx{scope = Clause, top = true, shadow = true};
upd_ctx(_Clause, #clause{}, #upd_ctx{scope = Scope}) ->
    #upd_ctx{scope = Scope, top = true, shadow = false};
upd_ctx(_Expr, #expr{role = Role}, #upd_ctx{scope = Scope, shadow = Shd}) ->
    #upd_ctx{scope = Scope, top = false, branch = false,
             shadow = Shd andalso Role =:= pattern}.

%% upd(Node, Data, SubEnv, State) -- updates variable links for `Node' after
%% its subtree is updated. `SubEnv' is the output environment of the last
%% child of `Node'. Returns the updated state.

upd(Clause, #clause{var=ClVar}, SubEnv,
    #upd_st{inenv = InEnv, outenv = OutEnv,
            ctx = #upd_ctx{branch = BranchMode} = Ctx} = St) ->
    Visib = [Var || {_Name, Var} <- env_to_list(env_union(InEnv, SubEnv))],
    Links = ?Graph:path(Clause, [varvis]),
    [?Graph:rmlink(Clause, varvis, Var) || Var <- Links -- Visib],
    [?Graph:mklink(Clause, varvis, Var) || Var <- Visib -- Links],

    case ClVar of
        simple ->
            St#upd_st{inenv  = env_union(InEnv, SubEnv),
                      outenv = env_union(OutEnv, SubEnv)};
        branch when not BranchMode ->
            St#upd_st{outenv = env_union(OutEnv, SubEnv),
                      ctx = Ctx#upd_ctx{branch = true}};
        branch when BranchMode ->
            St#upd_st{outenv = env_intersect(OutEnv, SubEnv)};
        scope ->
            [?Graph:delete(Var) ||
                Var <- ?Graph:path(Clause, [vardef]),
                [] == ?Graph:path(Var, [{varbind, back}])],
            St;
        _ ->
            St
    end;

upd(Expr, #expr{type = variable, role = pattern, value = Name}, _SubEnv,
    #upd_st{inenv = InEnv, outenv = OutEnv,
            ctx = #upd_ctx{scope = Scope, shadow = Shadow}} = St) ->
    if
        Shadow ->
            Var = get_var(Scope, Name),
            case env_lookup(Name, InEnv) of
                none         -> Type = bind;
                {value, Var} -> Type = ref;
                {value, _}   -> Type = bind
            end;
        not Shadow ->
            case env_lookup(Name, InEnv) of
                none ->
                    Var = get_var(Scope, Name),
                    Type = bind;
                {value, Var} ->
                    Type = ref
            end
    end,

    case Type of
        ref ->
            upd_ref(Expr, Var),
            upd_intro(Expr, St);
        bind ->
            upd_bind(Expr, Var),
            upd_intro(Expr, St#upd_st{outenv=env_insert(Name, Var, OutEnv)})
    end;

upd(Expr, #expr{type = variable, value = Name}, _SubEnv,
    #upd_st{inenv = Env} = St) ->
    case env_lookup(Name, Env) of
        none         -> upd_ref(Expr, none);
        {value, Var} -> upd_ref(Expr, Var)
    end,
    upd_intro(Expr, St);

upd(Expr, #expr{}, SubEnv, St = #upd_st{outenv = OutEnv}) ->
    upd_intro(Expr, St#upd_st{outenv = env_union(OutEnv, SubEnv)}).



%% upd_intro -- finish expression update by updating varintro links
upd_intro(_Expr, #upd_st{ctx = #upd_ctx{top = false}} = St) ->
    %% varintro links are created for top expressions only
    St;
upd_intro(Expr, #upd_st{ctx = #upd_ctx{top = true},
                        inenv = InEnv, outenv = OutEnv} = St) ->
    %% The -- operator is important here: substracting only by keys is wrong,
    %% because in case of shadowing only the value is different
    New = [Var || {_Name, Var} <- env_to_list(OutEnv) -- env_to_list(InEnv)],
    Links = ?Graph:path(Expr, [{varintro, back}]),
    [?Graph:rmlink(Var, varintro, Expr) || Var <- Links -- New],
    [?Graph:mklink(Var, varintro, Expr) || Var <- New -- Links],
    St#upd_st{inenv = env_union(InEnv, OutEnv)}.


%% update variable binding and reference links
upd_bind(Expr, Var) -> upd_links(Expr, Var, varbind, varref).
upd_ref(Expr, Var)  -> upd_links(Expr, Var, varref, varbind).

upd_links(From, To, Add, Del) ->
    case ?Graph:path(From, [Del]) of
        [] -> ok;
        [BadD] -> ?Graph:rmlink(From, Del, BadD)
    end,
    case ?Graph:path(From, [Add]) of
        [] when To == none -> ok;
        [To] -> ok;

        [] -> ?Graph:mklink(From, Add, To);
        [BadA] when To == none -> ?Graph:rmlink(From, Add, BadA);
        [BadA] ->
            ?Graph:rmlink(From, Add, BadA),
            ?Graph:mklink(From, Add, To)
    end.


clausevar(form)         -> scope;
clausevar(bin_comp)     -> hide;
clausevar(binary_gen)   -> {scope, hide};
clausevar(block_expr)   -> simple;
clausevar(case_expr)    -> {simple,branch};
clausevar(catch_expr)   -> hide;
clausevar(filter)       -> hide;
clausevar(fun_expr)     -> scope;
clausevar(if_expr)      -> branch;
clausevar(infix_expr)   -> {simple, hide}; % only for andalso, orelse
clausevar(list_comp)    -> hide;
clausevar(list_gen)     -> {scope, hide};
clausevar(receive_expr) -> branch;
clausevar(try_expr)     -> hide.

exprclass(#expr{type = Type}) when Type == list_comp; Type == binary_comp ->
    compr;
exprclass(#expr{} = Data) ->
    exprclass(?ErlNodes:structure(Data));
exprclass([]) -> leaf;
exprclass([{symbol, esub} | _]) -> simple;
exprclass([{symbol, _} | _]) -> compound;
exprclass([{repeat, _, esub} | _]) -> simple;
exprclass([{repeat, _, _} | _]) -> compound;
exprclass([{optional, Opt} | Tail]) -> exprclass(Opt ++ Tail);
exprclass([_ | Tail]) -> exprclass(Tail).


%% ---------------------------------------------------------------------------
%% Environment handling

env_empty() -> gb_trees:empty().
env_insert(K, V, D) -> gb_trees:enter(K, V, D).
env_lookup(K, D) -> gb_trees:lookup(K, D).
env_to_list(D) -> gb_trees:to_list(D).
env_union(D1, D2) ->
    gb_fold(fun(K,V,D) -> gb_trees:enter(K, V, D) end, D1, D2).
env_intersect(D1, D2) ->
    gb_fold(
      fun(K,_V,D) ->
              case gb_trees:lookup(K, D2) of
                  none -> gb_trees:delete_any(K, D);
                  _ -> D
              end
      end,
      D1, D1).

gb_fold(F, Acc, T) ->
    gb_iterate(F, Acc, gb_trees:iterator(T)).

gb_iterate(F, Acc, I) ->
    case gb_trees:next(I) of
        {K, V, Next} -> gb_iterate(F, F(K, V, Acc), Next);
        none -> Acc
    end.

union_test() ->
    D1 = env_insert(a,1,env_insert(b,2,env_empty())),
    D2 = env_insert(b,2,env_insert(c,3,env_empty())),
    U = env_union(D1, D2),
    ?assertMatch({value,1}, env_lookup(a, U)),
    ?assertMatch({value,2}, env_lookup(b, U)),
    ?assertMatch({value,3}, env_lookup(c, U)),
    ?assertMatch(none, env_lookup(d, U)).

intersect_test() ->
    D1 = env_insert(a,1,env_insert(b,2,env_empty())),
    D2 = env_insert(b,2,env_insert(c,3,env_empty())),
    I = env_intersect(D1, D2),
    ?assertMatch({value,2}, env_lookup(b, I)),
    ?assertMatch(none, env_lookup(a, I)),
    ?assertMatch(none, env_lookup(c, I)).

