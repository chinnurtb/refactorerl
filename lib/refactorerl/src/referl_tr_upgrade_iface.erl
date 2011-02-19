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

%%% @doc This module implements a generic interface upgrade. The
%%% interface function `do/1' can be called with a data structure that
%%% describes a whole interface change for a function.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

%%% ----------------------------------------------------------------------------
%%% TODO:
%%% - catch clause to case clause
%%% - try clause to try clause
%%% ----------------------------------------------------------------------------

-module(referl_tr_upgrade_iface).
-vsn("$Rev: 3247 $").
-include("refactorerl.hrl").

%%% ----------------------------------------------------------------------------
%%% Exports

-export([do/1, simple_infix_expr/1]).
-export([error_text/2]).

%%% ============================================================================
%%% Header

%%% ----------------------------------------------------------------------------
%%% Errors

%% @private
error_text(no_match, _) ->
    "node cannot be matched with any of the change descriptors".

warning(S, D) -> error_logger:warning_msg(S ++ "~n", D).

%%% ----------------------------------------------------------------------------
%%% Records and macros

%% Representation of the change descriptor structures
-record(elem, {type, id, items, value, f, node,
               exceptionclass, generic = false}).
-define(List(X), #elem{type = list, X}.
-define(Tuple(X), #elem{type = tuple, X}.
-define(Simple(X), #elem{type = none, X}.
-define(SimplE(X, Y), #elem{type = none, X, Y}.

%% Mapping a change descriptor to a list's elements
-record(map, {change_desc}).

%% Transformation on structure elements (e.g. x+1, x-1)
-record(tr, {f1, f2}).
%% Simple change descriptor
-record(cd, {from, to}).
%% Generalised change descriptor
-record(gencd, {cds}).

-define(DOT, $.).

-define(TableName, upgrademodifs).

%% Scheduling
-define(CatchP(C, P), ets:insert(?TableName, {catchp, C, P})).
-define(Tr(T), ets:insert(?TableName, {transform, T})).
-define(UpgradE(Expr, CDs),
        ?Upgrade(Expr, undefined, CDs)).
-define(PatternUpgrade(Pattern, CDs),
        ets:insert(?TableName, {upgradep, Pattern, CDs})).
-define(Upgrade(App, Fun, CDs),
        ets:insert(?TableName, {upgrade, App, Fun, CDs})).

-define(NewDFGraph, ets:insert(?TableName, {dfgraph, referl_dataflow:new()})).
-define(DFGraph, element(2, hd(ets:lookup(?TableName, dfgraph)))).

-define(Analyse_batch, ?ESG:close()).

%%% ============================================================================
%%% Interface

%% @private
do(CD) ->
    ets:new(?TableName, [named_table, bag]),
    ?NewDFGraph,
    [fun()  -> change_interface(CD)             end,
     fun(_) -> do_upgrades()                    end,
     fun(_) -> do_delayed_transforms()          end,
     fun(_) -> do_clause_movings()              end,
     fun(_) -> referl_dataflow:delete(?DFGraph),
               ets:delete(?TableName)           end].

%% @doc Doing the scheduled upgrades
do_upgrades() -> do_upgrades([]).
do_upgrades(L) ->
    [begin
         upgrade(Expr, Fun, CDs),
         ets:delete_object(?TableName, {upgrade, Expr, Fun, CDs})
     end ||
        {upgrade, Expr, Fun, CDs} <- L],

    ?Analyse_batch,

    [begin
         upgrade_pattern(Pattern, CDs),
         ets:delete_object(?TableName, {upgradep, Pattern, CDs})
     end ||
        {upgradep, Pattern, CDs} <- ets:lookup(?TableName, upgradep)],

    ?Analyse_batch,

    case ets:lookup(?TableName, upgrade) of
        [] -> ok;
        News -> do_upgrades(News)
    end.

%% @doc Doing the scheduled transformations
do_delayed_transforms() -> do_delayed_transforms([]).
do_delayed_transforms(L) ->
    [begin
         T(),
         ?Analyse_batch,
         ets:delete_object(?TableName, {transform, T})
     end ||
        {transform, T} <- L],

    case ets:lookup(?TableName, transform) of
        [] -> ok;
        News -> do_delayed_transforms(News)
    end.

%% @doc Doing the scheduled clause movings
do_clause_movings() ->
    [case_cl2catch_cl(C, P) ||
        {catchp, C, P} <- ets:lookup(?TableName, catchp)].

case_cl2catch_cl(Class, Pattern) ->
    case ?Query:exec(Pattern, [{pattern, back}, {exprcl, back}]) of
        [E] ->
            case ?ESG:data(E) of
                #expr{kind = K} when K == case_expr orelse K == try_expr ->
                    case_or_try2try(E, Class, Pattern);
                _ ->
                    ok
            end;
        _ -> ok
    end,
    ?Analyse_batch.

case_or_try2try(E, Class, Pattern) ->
    PatternCl  = ?Query:exec(Pattern, [{pattern, back}]),
    HeadCl     = ?Query:exec(E, [headcl]),
    PatternCls = ?Query:exec(E, [exprcl]),
    TryExpr    = ?Syn:create(#expr{kind=try_expr},
                             [{headcl, HeadCl},
                              {exprcl, lists:subtract(PatternCls, PatternCl)},
                              {catchcl, PatternCl}]),
    replace(E, TryExpr),
    ?Analyse_batch,
    ClassExpr = ?Syn:create(#expr{kind=atom}, [atom_to_list(Class)]),
    InfixExpr = ?Syn:create(#expr{kind=infix_expr, value=':'},
                            [{sub, [ClassExpr, Pattern]}]),
    replace(Pattern, InfixExpr).

%%% ============================================================================
%%% Scanning and parsing the strings

scan_and_parse(X) ->
    S = case lists:last(X) of
            ?DOT -> ok;
            _    -> X ++ "."
        end,
    E = fun erlang:element/2,
    E(2, erl_parse:parse_exprs(E(2, erl_scan:string(S)))).

%% Parses the strings and converts the complete data to my
%% representation. The helper change descriptors and the
%% transformation descriptors are generated into the element's `f' tags.
parse_cds(Data) ->
    {FunInfo, Patterns, Helpers, Transforms} = Data,

    %% Putting he transformation descriptors into records

    Ts = [{Id, #tr{f1 = F1, f2 = F2}} || {Id, F1, F2} <- Transforms],

    %% Scanning and parsing, then putting the helper
    %% change descriptors into records

    Hs = [{Id, [#cd{from = p2m(scan_and_parse(P1)),
                    to   = p2m(scan_and_parse(P2))} ||
                   {P1, P2} <- PPairs]} ||
             {Id, PPairs} <- Helpers],

    %% Annotating the helpers with the already parsed helpers and
    %% transformations. (Now helpers can use each other in 1-level
    %% only. Is recursion needed?)

    AnnotatedHs = [{Id, [#cd{from = annotate_cd(F, Hs, Ts),
                             to = annotate_cd(T, Hs, Ts)} ||
                            #cd{from = F, to = T} <- CDList]} ||
                      {Id, CDList} <- Hs],

    %% Scanning, parsing the main change descriptors and
    %% updating/injecting the references of helpers and
    %% transformations

    Ps = [create_cd(Elem, AnnotatedHs, Ts) || Elem <- Patterns],

    {FunInfo, Ps}.

create_cd(Elem, Hs, Ts) ->
    case Elem of
        {P1, P2} -> #cd{from = annotate_cd(p2m(scan_and_parse(P1)), Hs, Ts),
                        to   = annotate_cd(p2m(scan_and_parse(P2)), Hs, Ts)};
        GenCD when is_list(GenCD) ->
            #gencd{cds = [create_cd(CD, Hs, Ts) || CD <- GenCD]}
    end.


%%% ============================================================================
%%% Main functions

change_interface(D) ->
    {{FromFun, ToFun}, Ps} = parse_cds(D),
    [{module, F_Mod}, {name, F_Name}, {arity, F_Arity}, _, _] =
        erlang:fun_info(FromFun),

    %% Searching for potential applications and changing the call
    %% interfaces. For implicit funs there's no compensation (when the
    %% function is applied throught a variable, we do not detect the
    %% application)

    case ?Query:exec(?Query:seq(?Mod:find(F_Mod),?Fun:find(F_Name, F_Arity))) of
        [] ->
            %% io:format("No applications to upgrade: ~p:~p/~p~n",
            %%          [F_Mod, F_Name, F_Arity]);
            ok;
        [F_FunObj] ->
            Apps = find_applications(F_FunObj),
            %% TODO
            CompFun = false,
            case CompFun of
                true ->
                    change_with_compfun(FromFun, ToFun, Ps, Apps);
                false ->
                    change_without_compfun(ToFun, Ps, Apps)
            end
    end.

change_without_compfun(ToFun, Ps, Apps) ->
    [?Upgrade(App, ToFun, Ps) || App <- Apps].

change_with_compfun(FromFun, ToFun, Ps, Apps) ->
    if is_function(ToFun) ->
            {CName, CFun} = create_comp_fun(FromFun, ToFun, Ps),
            [?File:add_form(File, CFun)
             || File <- ?Query:exec([file])],
            ?Analyse_batch,
            [begin
                 ?Transform:touch(App),
                 replace_app(App, CName)
             end || App <- Apps];
       true -> ok
    end.

find_applications(FunObj) ->
    ?Query:exec(FunObj, ?Fun:applications()).

%%% ----------------------------------------------------------------------------
%%% Upgrade functionality

upgrade(Expr, ToFun, CDs) ->
    try

	%% Finding patterns to upgrade that belong to the expression
        Ps = lists:filter(fun is_pattern/1, patterns_with_df(Expr)),

        ?Transform:touch(Expr),

        PCDs = case ?ESG:data(Expr) of
		   #expr{kind = application} ->
		       X = upgrade_app(Expr, ToFun, CDs),
                       ?Analyse_batch,
                       X;
		   #expr{kind = _} ->
		       CDs;
		   _ ->
		       warning("Not supported upgrade on ~p (~p)",
			       [Expr, ToFun]),
                       CDs
	       end,

	%% Upgrading the patterns using the pattern change descriptors
        upgrade_patterns(Ps, PCDs)
    catch
        throw:cannot_match_args ->
            warning("Cannot match/upgrade application: ~p (~p)", [Expr, ToFun])
    end.

upgrade_patterns(Ps, CDs) ->
    [upgrade_pattern(P, CDs) || P <- Ps],
    ok.

upgrade_pattern(P, CDs) ->
    case match_any(P, CDs) of
        {FD, #cd{to=TD}} ->
            Merged = merge([FD], TD),
            New = m2n(Merged), %% does only create operations
            replace(P, New), %% starts new batch (insert, delete)
            ?Analyse_batch,
            TD;
        {gen, {FD, #cd{to=TD}}, #gencd{cds = GCDs}} ->
            Merged = merge([FD], TD),
            New = m2n(Merged),
            replace(P, New),
            ?Analyse_batch,

            MaybeCaseExpr = ?Query:exec(New, [{pattern, back}, {exprcl, back}]),
            try {MaybeCaseExpr, ?ESG:data(hd(MaybeCaseExpr))} of
                {[CaseExpr], #expr{kind = case_expr}} ->
                    Patterns = ?Query:exec(CaseExpr, [exprcl, pattern]),
                    Vars = cd_generic_vars(FD),
                    case generic_variables(FD, Vars) of
                        false -> ok;
                        true  -> upgrade_case_patterns(CaseExpr, Patterns,
                                                       New, FD, Vars, GCDs)
                    end;
                _ ->
                    ok
            catch
                error:badarg -> ok %% hd([])
            end,
            TD
    end.

generic_variables(FD, Vars) ->
    lists:all(
      fun(Var) ->
              case lookupNode(FD, Var) of
                  [{node, N}|_] ->
                      case ?ESG:data(N) of
                          #expr{kind = variable} -> true;
                          _                      -> false
                      end;
                  _ ->
                      false
              end
      end,
      Vars).

upgrade_case_patterns(CaseExpr, Patterns, P, FD, Vars, GCDs) ->
    [Clause] = ?Query:exec(P, [{pattern, back}]),
    Body = ?Query:exec(Clause, [body]),
    AlternativeCDs =
        lists:filter(
          fun(CD = #cd{from = [F]}) ->
                  lists:any(fun(Var) -> cd_var_present(F, Var) end, Vars)
                      andalso F#elem.type =/= none
                      andalso not cd_clause_present(CD, Patterns)
          end,
          GCDs),
    ACDs = [{CD, [Var ||
                     Var <- Vars,
                     cd_var_present(F, Var)]} ||
               CD <- AlternativeCDs,
               #cd{from = F} <- [CD]],

    PatternVariants = [create_pattern_variant(FD, Body, X) || X <- ACDs],
    {NewPatterns, ToUpdates} = lists:unzip(PatternVariants),
    ?Syn:replace(CaseExpr, {node, Clause}, [Clause|NewPatterns]),
    ?Analyse_batch,
    [replace(A, m2n(B)) || {A, B} <- lists:flatten(ToUpdates)],
    ?Analyse_batch.

cd_clause_present(#cd{to = [T]}, Patterns) ->
    lists:any(
      fun(Expr) ->
              match({Expr, ?ESG:data(Expr)}, T) =/= no_match
      end,
      Patterns).


lookupSubCD(L, Id) when is_list(L) ->
    lists:flatmap(fun(I) -> lookupSubCD(I, Id) end, L);
lookupSubCD(D = #elem{id = Id}, Id) -> [D];
lookupSubCD(#elem{items = Items}, Id) when items =/= undefined ->
    lookupSubCD(Items, Id);
lookupSubCD(_, _) -> [].

create_pattern_variant(FD, Body, {#cd{from = F, to = T}, PVars}) ->
    PVarNodes = [{Id, lookupNode(FD, Id)} || Id <- PVars],
    VarENodesToReplace = [{Id, ?Graph:path(Node, [varbind, {varref, back}])} ||
                             {Id,  [{node, Node}|_]} <- PVarNodes],

    Merged = merge([FD], T),
    NewPattern = m2n(Merged),

    Copy = [?ESG:copy(B) || B <- Body],
    NewBodies = [keysearch(C, B) || {B, C} <- lists:zip(Body, Copy)],

    {?Syn:create(#clause{kind = pattern},
                 [{pattern, NewPattern}, {body, NewBodies}]),
     %% These parts have to be updated in a new batch
     %% List of {E1, E2} tuples, where E1 have to be replaced by m2n(E2)
     lists:flatten([try keysearch(C, Node) of
                        CopiedNode -> {CopiedNode, lookupSubCD(F, Id)}
                    catch
                        throw:not_found -> []
                    end ||
                       C <- Copy, {Id, [Node]} <- VarENodesToReplace])}.

cd_generic_vars(#elem{id = Id, type = none, generic = true}) when Id =/= undefined -> [Id];
cd_generic_vars(#elem{items = Items}) when Items =/= undefined ->
    lists:flatten([cd_generic_vars(I) || I <- Items]);
cd_generic_vars(_) -> [].

cd_var_present(L, Var) when is_list(L) ->
    lists:any(fun(CD) -> cd_var_present(CD, Var) end, L);
cd_var_present(#elem{id = Var, generic = true}, Var) -> true;
cd_var_present(#elem{items = Items}, Var) when Items =/= undefined ->
    lists:any(fun(I) -> cd_var_present(I, Var) end, Items);
cd_var_present(_, _) -> false.

upgrade_app(X, ToFun, CDs) when is_function(ToFun) ->
    [{module, T_ModName},
     {name, T_FunName},_,_,_] = erlang:fun_info(ToFun),
    [#cd{from = ArgOldPattern, to = ArgNewPattern}|Ps] = CDs,
    Args = app_args(X),
    case match(Args, ArgOldPattern) of
        no_match ->
            throw(cannot_match_args);
        MatchedArgs ->
            NewArgs = [begin
                           Merged = merge(MatchedArgs, P),
                           m2n(Merged)
                       end || P <- ArgNewPattern],
            NewApp = create_application(T_ModName, T_FunName, NewArgs),
            replace(X, NewApp),
            Ps
    end;
upgrade_app(X, Str, CDs) ->
    Node = hd(m2n(p2m(scan_and_parse(Str)))),
    replace(X, Node),
    CDs.

replace_app(X, T_FunName) ->
    [_FunRef|Args] = ?Query:exec(X, ?Expr:children()),
    NewApp = create_application(T_FunName, Args),
    replace(X, NewApp).


%%% ============================================================================
%%% Simple way of pattern searching

app_args(App) ->
    [_FunRef|ActParams] = ?Query:exec(App, ?Expr:children()),
    [{P, ?ESG:data(P)} || P <- ActParams].
        %%P <- [hd(lists:reverse(df_reach(X, [back]))) || X <- ActParams]].

is_pattern(E) ->
    case ?ESG:data(E) of
        #expr{type = pattern, value = Val} when Val =/= "_" -> true;
        _ -> false
    end.

patterns(L) when is_list(L) ->
    lists:flatmap(fun patterns/1, L);

patterns(X) ->
    patterns(X, ?ESG:data(X)).

patterns_repeat(L) when is_list(L) andalso L =/= [] ->
    case patterns(L) of
        [] -> L;
        L1 -> L1
    end;
patterns_repeat(_) -> [].

patterns(X, #expr{type = expr}) ->
    patterns_repeat(ps_case(X) ++ ps_list_gen(X) ++ ps_match(X));

patterns(X, #expr{type = pattern, kind = cons}) ->
    patterns_repeat(?Query:exec(X, ?Query:seq(?Expr:child(1), ?Expr:children())));

patterns(_X, _D) ->
    [].

patterns_with_df(X) ->
    DF = lists:flatmap(fun df_reach/1, patterns(X)),
    case lists:flatmap(fun patterns/1, DF) of
        [] ->
            case lists:filter(fun is_pattern/1, DF) of
                [] ->
                    [X];
                L ->
                    L
            end;
        R ->
            lists:filter(fun is_pattern/1, R)
    end.

df_reach(X) ->
    df_reach(X, []).

df_reach(X, Opts) ->
    [Func] = ?Query:exec(X, ?Query:seq([?Expr:clause(),
                                        ?Clause:form(),
                                        ?Form:func()])),
    G = ?DFGraph, 
    referl_dataflow:add(G, [Func]),
    SR = lists:usort(referl_dataflow:reach(G, [X], Opts)),

    case SR of
        [X] ->
            SR;
        _ ->
            lists:delete(X, SR)
    end.

ps_case(X) ->
    case [{E, ?ESG:data(E)} ||
             E <- ?Query:exec(X, [{body, back}, {headcl, back}])] of
        [{CaseExpr, #expr{kind = case_expr}}] ->
            ?Query:exec(CaseExpr, [exprcl, pattern]);
        [{TryExpr, #expr{kind = try_expr}}] ->
            ?Query:exec(TryExpr, [exprcl, pattern]);
        _ ->
            []
    end.

ps_list_gen(X) ->
    case [{E, ?ESG:data(E)} ||
             E <- ?Query:exec(X, [{body, back}, {exprcl, back}])] of
        [{ListGenExpr, #expr{kind = list_gen}}] ->
            ?Query:exec(ListGenExpr, [{exprcl, 1}, pattern]);
        _ ->
            []
    end.

ps_match(X) ->
    case [{E, ?ESG:data(E)} || E <- ?Query:exec(X, [{sub, back}])] of
        [{MatchExpr, #expr{kind = match_expr}}] ->
            L = ?Query:exec(MatchExpr, [{sub, 1}]),
            case lists:member(X, L) of
                true -> [];
                _ -> L
            end;
        _ ->
            []
    end.

%%% ============================================================================
%%% Matching to the existing graph nodes

%% The returned abstract datas contain the old nodes from the graph.

match_any(Pattern, DescList) when is_list(DescList) ->
    M = [case Desc of
             #cd{from = FD} ->
                 {match({Pattern, ?ESG:data(Pattern)}, hd(FD)), Desc};
             #gencd{cds = CDs} ->
                 try match_any(Pattern, CDs) of
                     R ->
                         {gen, R, Desc}
                 catch
                     throw:?LocalErr0r(no_match) ->
                         no_match
                 end
         end ||
            Desc <- DescList],
    case lists:filter(
           fun({R, _}) -> R =/= no_match;
              (R)      -> R =/= no_match
           end, M) of
        [] ->
            warning("Cannot find any match: ~p, ~p~n", [Pattern, DescList]),
            throw(?LocalErr0r(no_match));
        [H|_] -> H
    end.

match(Ps, Ds) when is_list(Ps) andalso is_list(Ds) ->
    case length(Ps) == length(Ds) of
        true ->
            M = lists:zipwith(fun match/2, Ps, Ds),
            case lists:any(fun(R) -> R == no_match end, M) of
                true  -> no_match;
                false -> M
            end;
        false ->
            no_match
    end;

match({N, #expr{}}, D = ?Simple(id = ID)) when ID =/= undefined ->
    D#elem{node = N};

match(X = {N, #expr{}}, D = #elem{id = ID, generic = true})
  when ID =/= undefined ->
    case match(X, D#elem{id = undefined}) of
        no_match -> no_match;
        D2       -> D2#elem{id = ID, node = N}
    end;

match({_N, #expr{value=Val1}}, D = ?Simple(value = Val2)) ->
    case ?MISC:to_list(Val1) == ?MISC:to_list(Val2) of
        true  -> D;
        false -> no_match
    end;

match({N, #expr{kind=cons}}, D = ?List(id = ID)) when ID =/= undefined ->
    D#elem{node = N};
match({N, #expr{kind=cons}}, D = ?List(items = ItemDescs)) ->
    Items = [{I, ?ESG:data(I)} || I <- ?Query:exec(N, [sub, sub])],
    case match(Items, ItemDescs) of
        no_match -> no_match;
        M -> D#elem{items=M}
    end;

match({N, #expr{kind=tuple}}, D = ?Tuple(id = ID)) when ID =/= undefined ->
    D#elem{node = N};
match({N, #expr{kind=tuple}}, D = ?Tuple(items = ItemDescs)) ->
    Items = [{I, ?ESG:data(I)} || I <- ?Query:exec(N, [sub])],
    case match(Items, ItemDescs) of
        no_match -> no_match;
        M -> D#elem{items=M}
    end;

match(_, _) -> no_match.

%%% ============================================================================
%%% Merge the old nodes into the new descriptors

merge(OD, L) when is_list(L) -> [merge(OD, I) || I <- L];

merge(OD, D = #elem{id = Id, items = undefined})
  when is_list(OD), Id =/= undefined ->
    Node = case lists:flatten(lookupNode(OD, Id)) of
               [] -> warning("node not present: ~p", [Id]),
                     ?Syn:create(#expr{kind=variable}, [atom_to_list(Id)]);
               [{node, N}] -> N
           end,
    case Node of
        undefined ->
            warning("Node found with ID ~p, but it is undefined", [Id]);
        _ ->
            ok
    end,
    D#elem{node = Node};
merge(OD, D = ?List(items = Items))  -> D#elem{items = merge(OD, Items)};
merge(OD, D = ?Tuple(items = Items)) -> D#elem{items = merge(OD, Items)};
merge(_, D) -> D.

lookupNode(L, Id) when is_list(L) ->
    lists:flatmap(fun(I) -> lookupNode(I, Id) end, L);
lookupNode(#elem{id = Id, node = N}, Id) -> [{node, N}];
lookupNode(#elem{items = Items}, Id) when items =/= undefined ->
    lookupNode(Items, Id);
lookupNode(_, _) -> [].


%%% ============================================================================
%%% Conversions between representations

%%% ----------------------------------------------------------------------------
%%% Parsed to my abstract


p2m(L) when is_list(L) -> lists:map(fun p2m/1, L);
p2m({nil,_}) ->                        ?List(items = []);
p2m({cons,_,Elem,{nil,_}}) ->          ?List(items = [p2m(Elem)]);
p2m({cons,_,Elem,L = {cons,_,_,_}}) -> ?List(items =
                                             [p2m(Elem)|(p2m(L))#elem.items]);
p2m({tuple,   _, L  }) -> ?Tuple(items = p2m(L));
p2m({atom,    _, Val}) -> ?Simple(value = Val);
p2m({string,  _, Val}) -> ?Simple(value = Val);
p2m({integer, _, Val}) -> ?Simple(value = Val);
p2m({var,     _, V  }) -> ?Simple(id = V);
p2m({call, _, {atom, _, TransformName}, [{var, _, V}]}) ->
    ?SimplE(id = V, f = TransformName);
%% TODO: catch pattern without explicit exception class
p2m({'catch', _, {remote, _, {atom, _, ExceptionClass}, ErrorPattern}}) ->
    X = p2m(ErrorPattern),
    X#elem{exceptionclass = ExceptionClass};
p2m({call, _, {atom, _,  map}, [{atom, _, ChDescName}, {var, _, V}]}) ->
    ?SimplE(id=V, f = #map{change_desc = ChDescName});
p2m({match, _, {var, _, Var}, {var,_,'_'}}) ->
    ?SimplE(id = Var, generic = true);
p2m({match, _, {var, _, Var}, Val}) ->
    X = p2m(Val),
    X#elem{id = Var, generic = true}.


annotate_cd(L, Hs, Ts) when is_list(L) -> [annotate_cd(I, Hs, Ts) || I <- L];
annotate_cd(E = #elem{f = F}, Hs, Ts) when is_atom(F), F =/= undefined ->
    case {lists:keysearch(F, 1, Hs), lists:keysearch(F, 1, Ts)} of
        {{value, {F, H = [#cd{}|_]}}, false} -> E#elem{f = annotate_cd(H, Hs, Ts)};
        {false, {value, {F, T = #tr{}}}}     -> E#elem{f = T};
        {false, false} -> throw(["Undefined transformation: ", F])
    end;
annotate_cd(E = #elem{f = #map{change_desc = CDName}}, Hs, Ts) ->
    case lists:keysearch(CDName, 1, Hs) of
        {value, {CDName, #cd{from = From, to = To}}} ->
            E#elem{f = #map{change_desc =
                            #cd{from = annotate_cd(From, Hs, Ts),
                                to = annotate_cd(To, Hs, Ts)}}};
        %% TODO
        {value, {CDName, [#cd{from = From, to = To}]}} ->
            E#elem{f = #map{change_desc =
                            #cd{from = annotate_cd(From, Hs, Ts),
                                to = annotate_cd(To, Hs, Ts)}}};
        false ->
            throw(["Undefined CD: ", CDName])
    end;
annotate_cd(L = #elem{items = Items}, Hs, Ts) when items =/= undefined ->
    L#elem{items = annotate_cd(Items, Hs, Ts)};
annotate_cd(X, _, _) -> X.

%%% ----------------------------------------------------------------------------
%%% My abstract to nodes

m2n(L) when is_list(L) -> lists:map(fun m2n/1, L);

m2n(D = ?Simple(f = F)) when F =/= undefined ->
    Node = m2n(D#elem{f = undefined}),
    case F of
        #tr{f1 = F1, f2 = F2} ->
            transform(Node, F1, F2);
        #map{change_desc = CD} ->
            map(Node, CD);
        [#cd{}|_] ->
            ?PatternUpgrade(Node, F);
        _ ->
            ok
    end,
    Node;

m2n(#elem{node = Node}) when Node =/= undefined -> Node;

m2n(D = #elem{exceptionclass = C}) when C =/= undefined ->
    Node = m2n(D#elem{exceptionclass = undefined}),
    ?CatchP(C, Node),
    Node;
m2n(?SimplE(id = undefined, value=V)) when is_atom(V) ->
    ?Syn:create(#expr{kind=atom}, [io_lib:write_atom(V)]);
m2n(?SimplE(id = undefined, value=V)) when is_list(V) ->
    ?Syn:create(#expr{kind=string}, [io_lib:write_string(V)]);
m2n(?SimplE(id = undefined, value=V)) when is_integer(V) ->
    ?Syn:create(#expr{kind=integer}, [integer_to_list(V)]);
m2n(?Simple(id = Id)) ->
    ?Syn:create(#expr{kind=variable}, [atom_to_list(Id)]);

m2n(?List(items = []))     -> create_empty_list();
m2n(?List(items = Items))  -> create_list(m2n(Items));
m2n(?Tuple(items = Items)) -> create_tuple(m2n(Items));
m2n(Node = {'$gn', _, _})  -> Node.


transform(Node, F1, F2) ->
    ?Tr(fun() ->
                begin
                    case ?ESG:data(Node) of
                        #expr{type = pattern, kind = variable} ->
                            Refs = ?Query:exec(Node,
                                               ?Query:seq([varbind],
                                                          ?Var:references())),
                            [F2(Ref) || Ref <- Refs];
                        _ ->
                            F1(Node)
                    end
                end
        end).

map(Node, CD) ->
    case ?ESG:data(Node) of
        #expr{kind = cons} ->
            ?UpgradE(Node, [CD]);
        #expr{type = pattern} ->
            to_map(Node, CD);
        #expr{kind = variable} ->
            Refs = ?Query:exec(Node, ?Query:seq([varbind], ?Var:references())),
            [?UpgradE(Ref, [CD]) || Ref <- Refs]
    end.

create_empty_list() -> ?Syn:create(#expr{kind=cons}, []).
create_list(Items) ->
    ListExpr = ?Syn:create(#expr{kind=list}, [{sub, Items}]),
    ?Syn:create(#expr{kind=cons}, [{sub, [ListExpr]}]).
create_tuple(Items) -> ?Syn:create(#expr{kind=tuple}, [{sub, Items}]).

to_map(Node, CD) ->
    ?Tr(fun() ->
                begin
                    Refs = ?Query:exec(Node,
                                       ?Query:seq([varbind],
                                                  ?Var:references())),
                    [begin
                         Map = create_map(CD, Ref),
                         replace(Ref, Map)
                     end || Ref <- Refs]
                end
        end).

create_map(CD, Node) ->
    #cd{from = F, to = T} = CD,
    Clause = ?Syn:create(#clause{kind=funexpr},
                         [{pattern, m2n(T)}] ++ [{body, m2n(F)}]),
    FunExpr = ?Syn:create(#expr{kind=fun_expr}, [{exprcl, Clause}]),
    create_application(lists, map, [FunExpr, Node]).


free_vars(L) when is_list(L) -> lists:map(fun free_vars/1, L);
free_vars(#elem{id = Id, node = Node})
  when Id =/= undefined andalso Node == undefined -> Id;
free_vars(#elem{items = Items}) when Items =/= undefined ->
    [free_vars(I) || I <- Items];
free_vars(_) -> [].

no_free_vars(D1, D2) ->
    lists:all(fun(Id) -> lookupNode(D1, Id) =/= [] end,
              lists:flatten(free_vars(D2))).

%%% ============================================================================
%%% Compensation function

create_comp_fun(FromFun, ToFun, CDs) ->
    [{module, F_Mod}, {name, F_Name}, {arity, F_Arity}, _, _] =
        erlang:fun_info(FromFun),
    [{module, T_Mod}, {name, T_Name}, {arity, T_Arity}, _, _] =
        erlang:fun_info(ToFun),
    [#cd{from = ArgOldPattern, to = ArgNewPattern}|Ps] = CDs,
    App = create_application(T_Mod, T_Name, ArgNewPattern),
    Case = create_case_expr(App, Ps),

    NameFmt    = "~p:~p/~p -> ~p:~p/~p",
    NameParams = [F_Mod, F_Name, F_Arity, T_Mod, T_Name, T_Arity],
    C_Funname  = list_to_atom(lists:flatten(io_lib:format(NameFmt,NameParams))),

    {C_Funname, create_fun_form(C_Funname, m2n(ArgOldPattern), Case)}.

create_fun_form(FunName, Pattern, Body) ->
    FunNameE = ?Syn:create(#expr{kind = atom}, [io_lib:write(FunName)]),
    ClNode   = ?Syn:create(#clause{kind = fundef},
                           [{name, FunNameE},
                            {pattern, Pattern},{body, Body}]),
    ?Syn:create(#form{type = func}, [{funcl,ClNode}]).

create_application(FunName, Args) ->
    FunNameE  = ?Syn:create(#expr{kind = atom}, [io_lib:write(FunName)]),
    ?Syn:create(#expr{kind=application},
                [{sub, [FunNameE] ++ Args}]).

create_application(ModName, FunName, Args) ->
    ModNameE  = ?Syn:create(#expr{kind = atom}, [io_lib:write(ModName)]),
    FunNameE  = ?Syn:create(#expr{kind = atom}, [io_lib:write(FunName)]),
    InfixExpr = ?Syn:create(#expr{kind=infix_expr, value=':'},
                            [{sub, [ModNameE, FunNameE]}]),
    ?Syn:create(#expr{kind=application},
                [{sub, [InfixExpr] ++ m2n(Args)}]).

create_case_expr(App, Ps) ->
    HeadCl = ?Syn:create(#clause{kind=expr},[{body,App}]),
    BodyCls = [?Syn:create(#clause{kind=pattern},
                           [{pattern, m2n(Pattern)},
                            {body, m2n(Body)}])
               || #cd{from = Body, to = Pattern} <- Ps,
                  no_free_vars(Pattern, Body)],
    ?Syn:create(#expr{kind=case_expr},
                [{headcl, HeadCl},
                 {exprcl, BodyCls}]).

%%% ============================================================================
%%% Helpers

keysearch(List, Key) ->
    case lists:keysearch(Key, 1, List) of
        {value, {Key, Val}} -> Val;
        _ -> throw(not_found)
    end.

%% @private
simple_infix_expr(S) ->
    [{op,1,Op,{var,1,'P'},{integer,1,C}}] = scan_and_parse(S),
    fun(Node) ->
            New = ?Syn:create(#expr{kind = infix_expr, value = Op},
                              [{sub, [Node, ?Syn:create(#expr{kind = integer},
                                                        [io_lib:write(C)])]}]),
            replace(Node, New)
    end.


replace(Old, New) when not is_list(New) ->
    replace(Old, [New]);
replace(Old, New) ->
    [{_, Parent}|_] = ?Syn:parent(Old),
    ?Syn:replace(Parent, {node, Old}, New).
