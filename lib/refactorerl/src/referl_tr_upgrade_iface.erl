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

%%% @doc This module implements a generic interface upgrade. The
%%% interface function `do/1' can be called with a data structure that
%%% describes a whole interface change for a function.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

%%% ============================================================================
%%% TODO:
%%% - generalization and specialization in patterns/change descriptors
%%% (e.g. re:matches/3 needs a nomatch pattern when {match, []} is the
%%% result, so a special case of {match, Matches})
%%% - catch clause to case clause
%%% - try clause to try clause
%%% ============================================================================

-module(referl_tr_upgrade_iface).
-vsn("$Rev: 3013 $").
-include("refactorerl.hrl").

%% ----------------------------------------------------------------------------
%% Exports

-export([do/1, simple_infix_expr/1]).
-export([error_text/2]).

%% ----------------------------------------------------------------------------
%% Errors

%% @private
error_text(no_match, _) ->
    "node cannot be matched with any of the change descriptors";
error_text(node_not_present, Id) ->
    lists:flatten(io_lib:format("node not present with ID: ~p", [Id]));
error_text(not_supported_upgrade, _) ->
    "not supported upgrade call".

%% ----------------------------------------------------------------------------
%% Records and macros

%% Representation of the change descriptor structures
-record(elem, {type, id, items, value, f, node, exceptionclass}).
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

-define(DOT, $.).

-define(TableName, upgrademodifs).
-define(CatchP(C, P), ets:insert(?TableName, {catchp, C, P})).
-define(Tr(T), ets:insert(?TableName, {transform, T})).


%%% ----------------------------------------------------------------------------
%%% Interface

%% @private
do(CD) ->
    ets:new(?TableName, [named_table, bag]),
    change_interface(CD),
    ?ESG:close(),
    [T() || {transform, T} <- ets:lookup(?TableName, transform)],
    ?ESG:close(),
    [case_cl2catch_cl(C, P) || {catchp, C, P} <- ets:lookup(?TableName, catchp)],
    ets:delete(?TableName).

%% @private
simple_infix_expr(S) ->
    [{op,1,Op,{var,1,'P'},{integer,1,C}}] = scan_and_parse(S),
    fun(Node) ->
            New = ?Syn:create(#expr{kind = infix_expr, value = Op},
                              [{sub, [Node, ?Syn:create(#expr{kind = integer},
                                                        [io_lib:write(C)])]}]),
            replace(Node, New)
    end.

%%% ----------------------------------------------------------------------------

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
    ?ESG:close().

case_or_try2try(E, Class, Pattern) ->
    PatternCl  = ?Query:exec(Pattern, [{pattern, back}]),
    HeadCl     = ?Query:exec(E, [headcl]),
    PatternCls = ?Query:exec(E, [exprcl]),
    TryExpr    = ?Syn:create(#expr{kind=try_expr},
                             [{headcl, HeadCl},
                              {exprcl, lists:subtract(PatternCls, PatternCl)},
                              {catchcl, PatternCl}]),
    replace(E, TryExpr),
    ?ESG:close(),
    ClassExpr = ?Syn:create(#expr{kind=atom}, [atom_to_list(Class)]),
    InfixExpr = ?Syn:create(#expr{kind=infix_expr, value=':'},
                            [{sub, [ClassExpr, Pattern]}]),
    replace(Pattern, InfixExpr).

%%% ----------------------------------------------------------------------------

scan_and_parse(X) ->
    S = case lists:last(X) of
            ?DOT -> ok;
            _    -> X ++ "."
        end,
    E = fun erlang:element/2,
    E(2, erl_parse:parse_exprs(E(2, erl_scan:string(S)))).

%% Parses the strings and converts the complete data to my
%% representation.  The helper change descriptors and the
%% transformation descriptors are generated into the element's `f' tags.
parse_cds(Data) ->
    {FunInfo, Patterns, Helpers, Transforms} = Data,

    %% Scanning and parsing, then putting into records the helper
    %% change descriptors

    Hs = [{Id, #cd{from = p2m(scan_and_parse(P1)),
                   to   = p2m(scan_and_parse(P2))}}
          || {Id, P1, P2} <- Helpers],

    %% Putting into records the transformation descriptors

    Ts = [{Id, #tr{f1 = F1, f2 = F2}} || {Id, F1, F2} <- Transforms],

    %% Scanning, parsing the main change descriptors and
    %% updating/injecting the references of helpers and
    %% transformations

    Ps = [#cd{from = annotate_cd(p2m(scan_and_parse(P1)), Hs, Ts),
              to   = annotate_cd(p2m(scan_and_parse(P2)), Hs, Ts)}
          || {P1, P2} <- Patterns],

    {FunInfo, Ps}.

%%% ----------------------------------------------------------------------------
%%% Main functions

change_interface(D) ->
    {{FromFun, ToFun}, Ps} = parse_cds(D),
    [{module, F_Mod}, {name, F_Name}, {arity, F_Arity}, _, _] =
        erlang:fun_info(FromFun),

    %% Searching for potential applications and changing the call
    %% interfaces. For implicit funs there's no compensation (when the
    %% function is applied throught a variable, we do not detect the
    %% application)

    case ?Query:exec(?Query:seq(?Mod:find(F_Mod), ?Fun:find(F_Name, F_Arity))) of
        [] ->
            ok;
        %% throw(lists:flatten(io_lib:format("No applications to upgrade (~p:~p/~p)!", [F_Mod, F_Name, F_Arity])));
        [F_FunObj] ->
            case 1*2 of %% we do not use compensation function
                1 ->
                    if is_function(ToFun) ->
                            CF = create_comp_fun(FromFun, ToFun, Ps),
                            [?File:add_form(File, CF)
                             || File <- ?Graph:path(?Graph:root(), [file])],
                            ?ESG:close();
                       true -> ok
                    end;
                _ ->
                    Apps = find_applications(F_FunObj),
                    [upgrade(App, ToFun, Ps) || App <- Apps]
            end
    end.

find_applications(FunObj) ->
    ?Query:exec(FunObj, ?Fun:applications()).

upgrade(X, ToFun, CDs) ->
    PCDs = case ?ESG:data(X) of
               #expr{kind = application} -> upgrade_app(X, ToFun, CDs);
               #expr{kind = variable}    -> CDs;
               _                         -> ?LocalErr0r(not_supported_upgrade)
           end,
    ?Transform:touch(X),
    Ps = get_patterns(X),
    MatchedPatterns = [{P, match_any(P, PCDs)} || P <- Ps],
    [replace(P, m2n_merge([FD], TD)) || {P, {FD, TD}} <- MatchedPatterns],
    %% [FD] is needed because of lookup's interface
    ok.

upgrade_app(X, ToFun, CDs) when is_function(ToFun) ->
    [{module, T_ModName},
     {name, T_FunName},_,_,_] = erlang:fun_info(ToFun),
    [#cd{from = ArgOldPattern, to = ArgNewPattern}|Ps] = CDs,
    Args = app_args(X),
    MatchedArgs = match(Args, ArgOldPattern),
    NewArgs = [m2n_merge(MatchedArgs, P) || P <- ArgNewPattern],
    NewApp = create_application(T_ModName, T_FunName, NewArgs),
    replace(X, NewApp),
    Ps;
upgrade_app(X, Str, CDs) ->
    Node = hd(m2n(p2m(scan_and_parse(Str)))),
    replace(X, Node),
    CDs.

replace(Old, New) when not is_list(New) ->
    replace(Old, [New]);
replace(Old, New) ->
    [{_, Parent}|_] = ?Syn:parent(Old),
    ?Syn:replace(Parent, {node, Old}, New).

%%% ----------------------------------------------------------------------------
%%% Simple way of pattern searching

app_args(App) ->
    [_FunRef|ActParams] = ?Graph:path(App, ?Expr:children()),
    [{P, ?Graph:data(P)} || P <- ActParams].

get_patterns(X) ->
    ps_case(X) ++ ps_list(X) ++ ps_match(X).

ps_case(X) ->
    case [{E, ?ESG:data(E)}
          || E <- ?Graph:path(X, [{body, back}, {headcl, back}])] of
        [{CaseExpr, #expr{kind=case_expr}}] ->
            ?Graph:path(CaseExpr, [exprcl, pattern]);
        [{TryExpr, #expr{kind=try_expr}}] ->
            ?Graph:path(TryExpr, [exprcl, pattern]);
        _ ->
            []
    end.

ps_list(X) ->
    case [{E, ?ESG:data(E)}
          || E <- ?Graph:path(X, [{body, back}, {exprcl, back}])] of
        [{ListGenExpr, #expr{kind=list_gen}}] ->
            ?Graph:path(ListGenExpr, [{exprcl, 1}, pattern]);
        _ ->
            []
    end.

ps_match(X) ->
    case [{E, ?ESG:data(E)} || E <- ?Graph:path(X, [{sub, back}])] of
        [{MatchExpr, #expr{kind=match_expr}}] ->
            ?Graph:path(MatchExpr, [{sub, 1}]);
        _ ->
            []
    end.

%%% ----------------------------------------------------------------------------
%%% Matching to the existing graph nodes

%% The returned abstract datas contain the old nodes from the graph.

match_any(Pattern, DescList) when is_list(DescList) ->
    M = [{match({Pattern, ?Graph:data(Pattern)}, hd(FD)), hd(TD)}
         || #cd{from = FD, to = TD} <- DescList],
    case lists:filter(fun({R, _}) -> R =/= no_match end, M) of
        []    -> ?LocalErr0r(no_match);
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
match({_N, #expr{value=Val1}}, D = ?Simple(value = Val2)) ->
    case ?MISC:to_list(Val1) == ?MISC:to_list(Val2) of
        true  -> D;
        false -> no_match
    end;

match({N, #expr{kind=cons}}, D = ?List(id = ID)) when ID =/= undefined ->
    D#elem{node = N};
match({N, #expr{kind=cons}}, D = ?List(items = ItemDescs)) ->
    Items = [{I, ?ESG:data(I)} || I <- ?Graph:path(N, [sub, sub])],
    case match(Items, ItemDescs) of
        no_match -> no_match;
        M -> D#elem{items=M}
    end;

match({N, #expr{kind=tuple}}, D = ?Tuple(id = ID)) when ID =/= undefined ->
    D#elem{node = N};
match({N, #expr{kind=tuple}}, D = ?Tuple(items = ItemDescs)) ->
    Items = [{I, ?ESG:data(I)} || I <- ?Graph:path(N, [sub])],
    case match(Items, ItemDescs) of
        no_match -> no_match;
        M -> D#elem{items=M}
    end;

match(_, _) -> no_match.

%%% ----------------------------------------------------------------------------
%%% Creating nodes from the annotated descriptions (some nodes comes
%%% from old patterns, others are created now)

%% m2n and m2n_merge are redundant...

m2n_merge(OD, L) when is_list(L) -> [m2n_merge(OD, I) || I <- L];

m2n_merge(OD, D = #elem{exceptionclass = C}) when C =/= undefined ->
    Node = m2n_merge(OD, D#elem{exceptionclass = undefined}),
    ?CatchP(C, Node),
    Node;
m2n_merge(_D, ?List(items=[]))     -> create_empty_list();
m2n_merge(OD, ?List(items=Items))  -> create_list(m2n_merge(OD, Items));
m2n_merge(OD, ?Tuple(items=Items)) -> create_tuple(m2n_merge(OD, Items));
m2n_merge(_, D = #elem{id = undefined}) -> m2n(D);
m2n_merge(OD, #elem{id = Id, f = F, items = undefined}) when is_list(OD) ->
    Node = case lists:flatten(lookupNode(OD, Id)) of
               [] -> ?LocalError(node_not_present, Id);
               [{node, N}] -> N
           end,
    case F of
        undefined -> ok;
        #map{change_desc = CD} ->
            %% Cool recursive call, ToFun is undefined.
            Refs = ?Graph:path(Node, ?Query:seq([varbind], ?Var:references())),
            [upgrade(Ref, undefined, [CD]) || Ref <- Refs];
        #tr{f1 = F1, f2 = F2} ->
            transform(Node, F1, F2)
    end,
    case Node of
        undefined ->
            io:format("~n*** Warning: node found with ID ~p, but it is undefined!~n", [Id]);
        _ ->
            ok
    end,
    Node.


lookupNode([#elem{id = Id, node = N}|_], Id) -> [{node, N}];
lookupNode([#elem{items = Items}|T], Id)
  when items =/= undefined -> lookupNode(Items, Id) ++ lookupNode(T, Id);
lookupNode([_|T], Id) -> lookupNode(T, Id);
lookupNode(_, _) -> [].

transform(Node, F1, F2) ->
    case ?Graph:data(Node) of
        #expr{kind = variable} ->
            ?Tr(fun() ->
                        begin
                            Refs = ?Graph:path(Node,
                                               ?Query:seq([varbind],
                                                          ?Var:references())),
                            [F2(Ref) || Ref <- Refs]
                        end
                end);
        #expr{type = pattern} ->
            ?Tr(fun() -> F1(Node) end)
    end.

%%% ----------------------------------------------------------------------------
%%% Conversions between representations

%% Parsed to my abstract

p2m(L) when is_list(L) -> lists:map(fun p2m/1, L);
p2m({nil,_}) ->                        ?List(items = []);
p2m({cons,_,Elem,{nil,_}}) ->          ?List(items = [p2m(Elem)]);
p2m({cons,_,Elem,L = {cons,_,_,_}}) -> ?List(items =
                                             [p2m(Elem)|(p2m(L))#elem.items]);
p2m({tuple,   _, L  }) -> ?Tuple(items = p2m(L));
p2m({atom,    _, Val}) -> ?Simple(value=Val);
p2m({string,  _, Val}) -> ?Simple(value=Val);
p2m({integer, _, Val}) -> ?Simple(value=Val);
p2m({var,     _, V  }) -> ?Simple(id=V);
p2m({call, _, {atom, _, TransformName}, [{var, _, V}]}) ->
    ?SimplE(id = V, f = TransformName);
%% TODO: catch pattern without explicit exception class
p2m({'catch', _, {remote, _, {atom, _, ExceptionClass}, ErrorPattern}}) ->
    X = p2m(ErrorPattern),
    X#elem{exceptionclass = ExceptionClass};
p2m({call, _, {atom, _,  map}, [{atom, _, ChDescName}, {var, _, V}]}) ->
    ?SimplE(id=V, f = #map{change_desc = ChDescName}).

annotate_cd(L, Hs, Ts) when is_list(L) -> [annotate_cd(I, Hs, Ts) || I <- L];
annotate_cd(E = #elem{f = F}, _Hs, Ts) when is_atom(F), F =/= undefined ->
    {value, {F, T = #tr{}}} = lists:keysearch(F, 1, Ts),
    E#elem{f = T};
annotate_cd(E = #elem{f = #map{change_desc = CD}}, Hs, Ts) ->
    F = E#elem.f,
    {value, {CD, #cd{from = From, to = To}}} = lists:keysearch(CD, 1, Hs),
    UpdatedF = F#map{change_desc = #cd{from = annotate_cd(From, Hs, Ts),
                                       to = annotate_cd(To, Hs, Ts)}},
    E#elem{f = UpdatedF};
annotate_cd(L = #elem{items = Items}, Hs, Ts) when items =/= undefined ->
    L#elem{items = annotate_cd(Items, Hs, Ts)};
annotate_cd(X, _, _) -> X.


%% My abstract to nodes

m2n(L) when is_list(L) -> lists:map(fun m2n/1, L);

m2n(#elem{node=Node}) when Node =/= undefined -> Node;

m2n(D = #elem{exceptionclass = C}) when C =/= undefined ->
    Node = m2n(D#elem{exceptionclass = undefined}),
    ?CatchP(C, Node),
    Node;
m2n(D = ?Simple(f = F)) when F =/= undefined ->
    Node = m2n(D#elem{f = undefined}),
    case F of
        #tr{f1 = F1, f2 = F2} ->
            transform(Node, F1, F2);
        #map{change_desc = CD} ->
            to_map(Node, CD);
        _ ->
            ok
    end,
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
m2n(Node = {'$gn', _, _}) -> Node.

create_empty_list() -> ?Syn:create(#expr{kind=cons}, []).
create_list(Items) ->
    ListExpr = ?Syn:create(#expr{kind=list}, [{sub, Items}]),
    ?Syn:create(#expr{kind=cons}, [{sub, [ListExpr]}]).
create_tuple(Items) -> ?Syn:create(#expr{kind=tuple}, [{sub, Items}]).

to_map(Node, CD) ->
    ?Tr(fun() ->
                begin
                    Refs = ?Graph:path(Node,
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
free_vars(#elem{items = Items}) when Items =/= undefined ->  [free_vars(I) || I <- Items];
free_vars(_) -> [].

no_free_vars(D1, D2) ->
    lists:all(fun(Id) -> lookupNode(D1, Id) =/= [] end,
              lists:flatten(free_vars(D2))).

%%% ----------------------------------------------------------------------------
%%% Compensation function

create_comp_fun(FromFun, ToFun, CDs) ->
    [{module, F_Mod}, {name, F_Name}, {arity, F_Arity}, _, _] =
        erlang:fun_info(FromFun),
    [{module, T_Mod}, {name, T_Name}, {arity, T_Arity}, _, _] =
        erlang:fun_info(ToFun),
    [#cd{from = ArgOldPattern, to = ArgNewPattern}|Ps] = CDs,
    App = create_application(T_Mod, T_Name, ArgNewPattern),
    Case = create_case_expr(App, Ps),

    NameForm  = "~p:~p/~p -> ~p:~p/~p",
    NameParams = [F_Mod, F_Name, F_Arity, T_Mod, T_Name, T_Arity],
    C_Funname = list_to_atom(lists:flatten(io_lib:format(NameForm,NameParams))),

    create_fun_form(C_Funname, m2n(ArgOldPattern), Case).

create_fun_form(FunName, Pattern, Body) ->
    FunNameE = ?Syn:create(#expr{kind = atom}, [io_lib:write(FunName)]),
    ClNode  = ?Syn:create(#clause{kind = fundef},
                          [{name, FunNameE},
                           {pattern, Pattern},{body, Body}]),
    ?Syn:create(#form{type = func}, [{funcl,ClNode}]).

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
