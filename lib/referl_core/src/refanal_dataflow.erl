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

%%% @doc Data flow analyser.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>

-module(refanal_dataflow).
-vsn("$Rev $"). %% for emacs"
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-export([reach/2]).

-export([back_nodes/1]).

-include("core.hrl").

%%% @private
schema() ->
    [{expr,   [{flow, expr},  {dep, expr},
               {sel, expr},   {cons_back, expr},
               {sel_e, expr}, {cons_e, expr}]},
     {func,   [{fpar, expr}, {fret, expr}]}].
    %% Note: s_i, c_i <-> sel/i, cons_back/i

%%% @private
externs(_) -> [].


remove_app_deps(App, Name, ArgList) ->
    Pars = [Par || {_, Par} <- ?Anal:children(ArgList)],
    Dels =
         case ?Anal:children(Name) of
             []                   -> [Name];
             [{_, Mod},{_, Fun}]  -> [Mod, Fun]
         end,
    safe_link(rmlink, Dels ++ Pars, dep, App),
    {App, ?Anal:parent(App)}.


%%% @private
insert(Parent, Pre, {Tag, Child}, Post) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            Apps = get_app(Child),
            AppList = [remove_app_deps(App, Name, ArgList)
                            || App <- Apps,
                               [{_, Name}, {_, ArgList}] <- [?Anal:children(App)],
                               ?Graph:path(Name, [dep]) =/= []],
            walk(fun add_del/4, [{Child, Parent} | AppList], mklink);
        #form{type=func} when Tag == funcl ->
            Apps = get_app(Child),
            Children = ?Anal:children(Child),
            {_, Last} = lists:last(Children),
            Pats = [Pat || {pattern, Pat} <- Children],
            [begin
                [_, {_, ArgList}] = ?Anal:children(App),
                Pars = [Par || {_, Par} <- ?Anal:children(ArgList)],
                [link(mklink, Par, flow, Pat) ||
                    {Pat, Par} <- lists:zip(Pats, Pars)],
                link(mklink, Last, flow, App)
             end || App <- Apps],
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #form{} -> ok;
        #clause{type=fundef} ->
            case Post of
                [] ->
                    Form = ?Anal:parent(Parent),
                    Apps = get_app(Form),
                    link(mklink, Child, flow, Apps),
                    case Pre of
                        [] -> ok;
                        _  -> {_, Last} = lists:last(Pre),
                              safe_link(rmlink, Last, flow, Apps)
                    end;
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=Type} when Type == pattern orelse Type == guard orelse
                                Type == timeout orelse Type == block orelse
                                Type == funexpr ->
            case Post of
                [] ->
                    Expr = ?Anal:parent(Parent),
                    link(mklink, Child, flow, Expr),
                    case Pre of
                        [] -> ok;
                        _  -> {_, Last} = lists:last(Pre),
                              safe_link(rmlink, Last, flow, Expr) %%ins vs. repl
                    end;
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=expr} -> %% catch
            Expr = ?Anal:parent(Parent),
            case ?Anal:data(Expr) of
                #expr{type=catch_expr} -> link(mklink, Child, flow, Expr);
                #expr{type=case_expr} ->
                    [_| Clauses] = ?Anal:children(Expr),
                    [begin
                      [{_, PatI} | _] = ?Anal:children(ClI),
                      link(mklink, Child, flow, PatI)
                     end || {_, ClI} <- Clauses];
                %% todo andalso, orelse
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=hexpr} ->
            ListComp = ?Anal:parent(Parent),
            link(mklink, Child, cons_e, ListComp),
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=T} when T==compr orelse T==list_gen orelse T==filter->
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #expr{type=T, value=V} when (T == infix_expr andalso V == ':') orelse
                                     T ==  arglist ->
            App = ?Anal:parent(Parent),
            AppPar = ?Anal:parent(App),
            add_del(rmlink, ?Anal:data(App), App, AppPar),
            add_del(mklink, ?Anal:data(App), App, AppPar),
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #expr{} ->
            %% Child es reszkifejezeseinek az elemzese, uj el a
            %% szulohoz kifejezes tipus szerint majd, most csak torol
            %% es ujraepit
            Parent2 = ?Anal:parent(Parent),
            add_del(rmlink, ?Anal:data(Parent), Parent, Parent2),
            add_del(mklink, ?Anal:data(Parent), Parent, Parent2),
            walk(fun add_del/4, [{Child, Parent}], mklink)
    end.


add_del(Dir, #expr{type=variable}, Expr, _P) ->
    VarB = ?Graph:path(Expr, [varbind]),
    case VarB of
        [Var] ->
            VarRefs = ?Graph:path(Var, [{varref, back}]),
            safe_link(Dir, Expr, flow, VarRefs);
        [] ->
            case ?Graph:path(Expr, [varref]) of
                [Var] ->
                    VarBs = ?Graph:path(Var, [{varbind, back}]),
                    safe_link(Dir, VarBs, flow, Expr);
                [] -> ok %%io:format("~nError in variable analyzer!~n~n", [])
                %% Todo: Eliminate this branch...
            end
    end,
    [];
add_del(Dir, #expr{type=match_expr, role=pattern}, Expr, _P) ->
    [{_, P1}, {_, P2}] = ?Anal:children(Expr),
    safe_link(Dir, Expr, flow, [P1,P2]),
    [{P1, Expr}, {P2, Expr}];
add_del(Dir, #expr{type=match_expr}, Expr, _P) ->
    [{_, P}, {_, E}] = ?Anal:children(Expr),
    safe_link(Dir, E, flow, [P, Expr]),
    [{P, Expr}, {E, Expr}];
add_del(Dir, #expr{type=infix_expr, value = V}, Expr, _P) when (V == 'andalso')
                                                 orelse  (V == 'orelse') ->
    [{_, C1}, {_, C2}] = ?Anal:children(Expr),
    [{_, E1}] = ?Anal:children(C1),
    [{_, E2}] = ?Anal:children(C2),
    safe_link(Dir, [E1, E2], dep, Expr),
    [{E1, Expr}, {E2, Expr}];
add_del(Dir, #expr{type=infix_expr}, Expr, _P) ->
    [{_, E1}, {_, E2}] = ?Anal:children(Expr),
    safe_link(Dir, [E1, E2], dep, Expr),
    [{E1, Expr}, {E2, Expr}];
add_del(Dir, #expr{type=prefix_expr}, Expr, _P) ->
    [{_, E1}] = ?Anal:children(Expr),
    safe_link(Dir, E1, dep, Expr),
    [{E1, Expr}];
add_del(Dir, #expr{type=parenthesis, role=pattern}, Expr, _P) ->
    [{_, P}] = ?Anal:children(Expr),
    safe_link(Dir, Expr, flow, P),
    [{P, Expr}];
add_del(Dir, #expr{type=parenthesis}, Expr, _P) ->
    [{_, E}] = ?Anal:children(Expr),
    safe_link(Dir, E, flow, Expr),
    [{E, Expr}];
add_del(Dir, #expr{type=tuple, role=pattern}, Expr, _P) ->
    Children = ?Anal:children(Expr),
    [safe_link(Dir, Expr, sel, P) || {_, P} <- Children],
    [{P, Expr} || {_, P} <- Children];
add_del(Dir, #expr{type=tuple}, Expr, _P) ->
    Children = ?Anal:children(Expr),
    [safe_link(Dir, Expr, cons_back, E) || {_, E} <- Children],
    [{E, Expr} || {_, E} <- Children];
add_del(Dir, #expr{type=cons, role=pattern}, Expr, _P) ->
    Children = ?Anal:children(Expr),
    case Children of
        [] -> [];
        [{_, List} | Cons] ->
            Elements = ?Anal:children(List),
            [safe_link(Dir, Expr, sel_e, P) || {_, P} <- Elements],
            [safe_link(Dir, Expr, flow, P) || {_, P} <- Cons],
            [{P, Expr} || {_, P} <- (Elements ++ Cons)]
    end;
add_del(Dir, #expr{type=cons}, Expr, _P) ->
    Children = ?Anal:children(Expr),
    case Children of
        [] -> [];
        [{_, List} | Cons] ->
            Elements = ?Anal:children(List),
            [safe_link(Dir, E, cons_e, Expr) || {_, E} <- Elements],
            [safe_link(Dir, E, flow, Expr) || {_, E} <- Cons],
            [{E, Expr} || {_, E} <- (Elements ++ Cons)]
    end;
add_del(Dir, #expr{type=case_expr}, Expr, _P) ->
    [{_, HeadCl} | Clauses] = ?Anal:children(Expr),
    [{_, HeadE}] = ?Anal:children(HeadCl),
    List =
        [begin
            List = [{_, PatI} | Rest ] = ?Anal:children(ClI),
            safe_link(Dir, HeadE, flow, PatI),
            safe_link(Dir, element(2, lists:last(Rest)), flow, Expr),
            List
         end || {_, ClI} <- Clauses],
    [{HeadE, Expr} | [{E, Expr} || {_, E} <- lists:flatten(List)]];
add_del(Dir, #expr{type=application}, Expr, _P) ->
    [{_, Name}, {_, ArgList}] = ?Anal:children(Expr),
    Pars = [Par || {_, Par} <- ?Anal:children(ArgList)],
    case ?Graph:path(Expr, [funlref, {fundef, back}]) ++
         ?Graph:path(Expr, [funeref, {fundef, back}]) of
        [Fun] ->
             _List =
                 [begin
                    Children = ?Anal:children(Cl),

                    FPars = ?Graph:path(Fun, [fundef, fpar]),
                    FRet = ?Graph:path(Fun, [fundef, fret]),

                    [safe_link(Dir, From, flow, To) ||
                          {From, To} <- lists:zip(Pars, FPars)],

                     safe_link(Dir, FRet, flow, Expr),
                     Children
                  end || Cl <- ?Graph:path(Fun, [funcl])],
             [{E, Expr} || E <- Pars];
        [] ->
            case ?Anal:children(Name) of
                [{esub, Mod},{esub, Fun}]  ->
                    safe_link(Dir, [Mod, Fun | Pars], dep, Expr),
                    [{E, Expr} || E <- [Mod, Fun | Pars]];
                [] -> %% Atom
                    safe_link(Dir, [Name | Pars], dep, Expr),
                    [{E, Expr} || E <- [Name | Pars]];
                _List -> %% fun(...) -> ... end (Args)
                        %% (fun(...) -> ... end) (Args)
                    safe_link(Dir, [Name | Pars], dep, Expr),
                    [{E, Expr} || E <- [Name | Pars]]
            end
    end;

add_del(Dir, #expr{type=block_expr}, Expr, _P) ->
    [{_, BlockCl}] = ?Anal:children(Expr),
    Body = ?Anal:children(BlockCl),
    safe_link(Dir, element(2, lists:last(Body)), flow, Expr),
    [{E, Expr} || {_, E} <- Body];
add_del(Dir, #expr{type=catch_expr}, Expr, _P) ->
    [{_, CatchCl}] = ?Anal:children(Expr),
    [{_, Body}] = ?Anal:children(CatchCl),
    safe_link(Dir, Body, flow, Expr),
    [{Body, Expr}];
add_del(Dir, #expr{type=if_expr}, Expr, _P) ->
    Clauses = ?Anal:children(Expr),
    List =
        [begin
            Children = ?Anal:children(ClI),
            safe_link(Dir, element(2, lists:last(Children)), flow, Expr),
            Children
         end || {_, ClI} <- Clauses],
    [{E, Expr} || {_, E} <- lists:flatten(List)];
add_del(Dir, #expr{type=try_expr}, Expr, _P) ->
    [{_, HeadCl} | Clauses] = ?Anal:children(Expr),
    HeadChildren = [ E || {_, E} <- ?Anal:children(HeadCl)],
    Last = lists:last(HeadChildren),
    List1 =
        [begin
            List = [{_, PatI} | Rest ] = ?Anal:children(ClI),
            safe_link(Dir, Last, flow, PatI),
            safe_link(Dir, element(2, lists:last(Rest)), flow, Expr),
            List
         end || {exprcl, ClI} <- Clauses],
    List2 =
        [begin
            List = ?Anal:children(ClI),
            safe_link(Dir, element(2, lists:last(List)), flow, Expr),
            List
         end || {catchcl, ClI} <- Clauses],
     [{H, Expr} || H <- HeadChildren] ++
      [{E, Expr} || {_, E} <- lists:flatten(List1 ++ List2)];

add_del(Dir, #expr{type=receive_expr}, Expr, _P) ->
    %% TODO: honnan jon a patternbe ertek?
    Clauses = ?Anal:children(Expr),
    List =
        [begin
            List = ?Anal:children(ClI),
            safe_link(Dir, element(2, lists:last(List)), flow, Expr),
            List
         end || {_, ClI} <- Clauses],
    [{E, Expr} || {_, E} <- lists:flatten(List)];

add_del(Dir, #expr{type=send_expr}, Expr, _P) ->
    %% TODO: hova kuldjuk el az erteket?
    [{_, E1}, {_, E2}] = ?Anal:children(Expr),
    safe_link(Dir, E2, flow, Expr),
    [{E1, Expr}, {E2, Expr}];

add_del(Dir, #expr{type=list_comp}, Expr, _P) ->
    [{_, HeadCl}, {_, ComprCl}] = ?Anal:children(Expr),
    [{_, HeadE}] = ?Anal:children(HeadCl),
    safe_link(Dir, HeadE, cons_e, Expr),
    [{HeadE, Expr} | [ {E, Expr} || {_, E} <- ?Anal:children(ComprCl)]];
add_del(Dir, #expr{type=list_gen}, Expr, _P) ->
    [{_, PCl}, {_, ECl}] = ?Anal:children(Expr),
    [{_, P}] = ?Anal:children(PCl),
    [{_, E}] = ?Anal:children(ECl),
    safe_link(Dir, E, sel_e, P),
    [{P, Expr}, {E, Expr}];
add_del(_Dir, #expr{type=filter}, Expr, _P) ->
    [{_, ECl}] = ?Anal:children(Expr),
    [{_, E}] = ?Anal:children(ECl),
    [{E, Expr}];

add_del(_Dir, #expr{type=binary_gen},  _Expr, _P) -> [];
%% P <= E
add_del(_Dir, #expr{type=binary},  _Expr, _P) -> [];
%% << ... >>
add_del(_Dir, #expr{type=bin_comp},  _Expr, _P) -> [];
%% << E || ... >>

%% size_qualifier, prfix_bit_expr
%% bit_size_expr
%% field_list, arglist

add_del(Dir, #expr{type=record_update}, Expr, _P) ->
    [{_, RecVar}, {_, FList}] = ?Anal:children(Expr),
    Fields = ?Anal:children(FList),
    safe_link(Dir, RecVar, cons_e, Expr), %% todo: cons_e ?????
    [safe_link(Dir, F, cons_e, Expr) || {_, F} <- Fields],
    [{RecVar, Expr} | [{F, Expr} || {_, F} <- Fields]];

add_del(Dir, #expr{type=record_expr, role = pattern}, Expr, _P) ->
    [{_, FList}] = ?Anal:children(Expr),
    Fields = ?Anal:children(FList),
    [safe_link(Dir, Expr, sel_e, F) || {_, F} <- Fields],
    [{F, Expr} || {_, F} <- Fields];

add_del(Dir, #expr{type=record_expr}, Expr, _P) ->
    [{_, FList}] = ?Anal:children(Expr),
    Fields = ?Anal:children(FList),
    [safe_link(Dir, F, cons_e, Expr) || {_, F} <- Fields],
    [{F, Expr} || {_, F} <- Fields];

add_del(Dir, #expr{type=T, role = pattern}, Expr, _P) when (T == record_field)
                                           orelse (T == record_joker_field) ->
    [{_, Pat}] = ?Anal:children(Expr),
    safe_link(Dir, Expr, flow, Pat),
    [{Pat, Expr}];

add_del(Dir, #expr{type= T}, Expr, _P) when (T == record_field) orelse
                                   (T == record_joker_field) ->
    [{_, E}] = ?Anal:children(Expr),
    safe_link(Dir, E, flow, Expr),
    [{E, Expr}];

add_del(Dir, #expr{type=record_index, role = pattern}, Expr, _P) ->
    [{_, P}] = ?Anal:children(Expr),
    safe_link(Dir, P, dep, Expr),
    [];

add_del(Dir, #expr{type= T}, Expr, _P) when (T == record_access) orelse
                                   (T == record_index) ->
    Children = ?Anal:children(Expr),
    [safe_link(Dir, E, dep, Expr) || {_, E} <- Children],
    [{E, Expr} || {_, E} <- Children];

add_del(Dir, #expr{type=implicit_fun}, Expr, _P) ->
    [{_, Name}, {_, Arity}] = ?Anal:children(Expr),
    safe_link(Dir, [Name, Arity], dep, Expr),
    [{Name, Expr}];

add_del(_Dir, #expr{type=fun_expr}, Expr, _P) ->
    Clauses = ?Anal:children(Expr),
    Exprs = lists:flatmap(fun ({_, Cl}) ->
                              ?Anal:children(Cl)
                          end, Clauses),
    [{E, Expr} || {_, E} <- Exprs];
add_del(_Dir, #expr{}, _Expr, _P) -> [];

add_del(_Dir, #form{type=func}, Func, _File) ->
    {_, Clauses} = lists:unzip(?Anal:children(Func)),

    [FRet] = ?Graph:path(Func, [fundef, fret]),
    [?Graph:mklink(Ret, flow, FRet) || Ret <- return_points(Clauses)],

    Exprs = lists:flatmap(fun (Cl) ->
                              ?Anal:children(Cl)
                          end, Clauses),
    [{Expr, Func} || {_, Expr} <- Exprs];

add_del(_Dir, #clause{}, Clause, P) ->
    Exprs = ?Anal:children(Clause),
    [{Expr, P} || {_, Expr} <- Exprs];

add_del(_Dir, _Type, _Expr, _P) -> [].


%% Returns those nodes that deliver the return values of a function.
return_points(Nodes) when is_list(Nodes) ->
    lists:flatten([return_points(Node, ?Graph:data(Node)) || Node <- Nodes]);
return_points(Node) ->
    return_points([Node]).

return_points(Node, #clause{}) ->
    return_points(?Graph:path(Node, [{body, last}]));
return_points(Node, #expr{}) ->
    case ?Graph:path(Node, [exprcl]) of
        []      -> [Node];
        Clauses -> return_points(Clauses)
    end.



%%% @private
remove(Parent, Pre, {Tag, Child}, Post) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
             Apps = get_app(Child),
             [clear_clause(Cl, Apps) || {funcl, Cl} <- ?Anal:children(Child)],
             walk(fun add_del/4, [{Child, Parent}], rmlink);
        #form{type=func} when Tag == funcl ->
             Apps = get_app(Parent),
             clear_clause(Child, Apps),
             walk(fun add_del/4, [{Child, Parent}], rmlink);
        #form{} -> ok;
        #clause{type=fundef} ->
            case Post of
                [] ->
                    Form = ?Anal:parent(Parent),
                    Apps = get_app(Form),
                    safe_link(rmlink, Child, flow, Apps),
                    case lists:reverse(Pre) of
                        [{body, Last} | _] -> link(mklink, Last, flow, Apps);
                        _ -> ok
                    end;
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=Type} when Type == pattern orelse Type == guard orelse
                                Type == timeout orelse Type == block orelse
                                Type == funexpr ->
            case Post of
                [] ->
                    Expr = ?Anal:parent(Parent),
                    safe_link(rmlink, Child, flow, Expr),
                    case lists:reverse(Pre) of
                        [{body, Last} | _] -> link(mklink, Last, flow, Expr);
                        _ -> ok
                    end;
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=expr} ->
            Expr = ?Anal:parent(Parent),
            case ?Anal:data(Expr) of
                #expr{type=catch_expr} -> safe_link(rmlink, Child, flow, Expr);
                #expr{type=case_expr} ->
                    [_| Clauses] = ?Anal:children(Expr),
                    [begin
                      [{_, PatI} | _] = ?Anal:children(ClI),
                      safe_link(rmlink, Child, flow, PatI)
                     end || {_, ClI} <- Clauses];
                %% todo andalso, orelse
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=hexpr} ->
            ListComp = ?Anal:parent(Parent),
            safe_link(rmlink, Child, cons_e, ListComp),
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=T} when T==compr orelse T==list_gen orelse T==filter->
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #expr{type=T, value=V} when (T == infix_expr andalso V == ':') orelse
                                     T ==  arglist ->
            App = ?Anal:parent(Parent),
            AppPar = ?Anal:parent(App),
            add_del(rmlink, App, ?Anal:data(App), AppPar),
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #expr{} ->
             walk(fun add_del/4, [{Child, Parent}], rmlink),
             case ?Anal:data(Child) of
                 #expr{} ->
                     [link(rmlink, Child, Link, ?Graph:path(Child, [Link]))
                        || Link <- forw_tags() ++ back_tags()];
                 _ -> ok
             end
    end.

%%% @private
update(_,_) -> ok.

walk(Fun, [{Node, Parent} | Tail], Dir) ->
    walk(Fun, Fun(Dir, ?Anal:data(Node), Node, Parent) ++ Tail, Dir);
walk(_, [], _) ->
    ok.

%% -----------------------------------------------------------------------------
%%

clear_clause(Cl, Apps) ->
    Children = ?Anal:children(Cl),
    Pats = [Pat || {pattern, Pat} <- Children],
    Last = element(2, lists:last(Children)),
    [begin
        [{_, Name}, {_, ArgList}] = ?Anal:children(App),
        Pars = [Par || {_, Par} <- ?Anal:children(ArgList)],
        [safe_link(rmlink, Par, flow, Pat) ||
                {Pat, Par} <- lists:zip(Pats, Pars)],
        safe_link(rmlink, Last, flow, App),
        case ?Anal:children(Name) of
            [{_, Mod},{_, Fun}]  ->
                link(mklink, [Mod, Fun | Pars], dep, App);
            [] ->
                link(mklink, [Name | Pars], dep, App)
        end
      end || App <- Apps].

forw_tags() ->
    [flow, dep, sel, {cons_back, back}, sel_e, cons_e].

back_tags() ->
    [{flow, back}, {dep, back}, {sel, back}, cons_back,
     {sel_e, back}, {cons_e, back}].

back_nodes(Node) ->
    case ?Graph:data(Node) of
        #expr{} ->
            lists:flatten([?Graph:path(Node, [Tag]) || Tag <- back_tags()]);
        _ ->
            []
    end.

get_app(Node) ->
    ?Graph:path(Node,[fundef, {{funeref, back}, {type,'==', application }}]) ++
    ?Graph:path(Node,[fundef, {{funlref, back}, {type,'==', application }}]).

link(Dir, List1, {Link, back}, List2) ->
    link(Dir, List2, Link, List1);
link(Dir, List1, Link, Elem2) when is_list(List1) ->
    [?Graph:Dir(Elem1, Link, Elem2) || Elem1 <- List1];
link(Dir, Elem1, Link, List2) when is_list(List2) ->
    [?Graph:Dir(Elem1, Link, Elem2) || Elem2 <- List2];
link(Dir, Elem1, Link, Elem2) ->
    ?Graph:Dir(Elem1, Link, Elem2).

safe_link(Dir, List1, {Link, back}, List2) ->
    safe_link(Dir, List2, Link, List1);
safe_link(mklink, List1, Link, List2) ->
    link(mklink, List1, Link, List2);
safe_link(Dir, List1, Link, Elem2) when is_list(List1) ->
    [?Graph:Dir(Elem1, Link, Elem2)
      || Elem1 <- List1, lists:member(Elem2, ?Graph:path(Elem1, [Link]))];
safe_link(Dir, Elem1, Link, List2) when is_list(List2) ->
    [?Graph:Dir(Elem1, Link, Elem2)
      || Elem2 <- List2, lists:member(Elem2, ?Graph:path(Elem1, [Link]))];
safe_link(Dir, Elem1, Link, Elem2) ->
    case lists:member(Elem2, ?Graph:path(Elem1, [Link])) of
        false -> ok;
        true  -> ?Graph:Dir(Elem1, Link, Elem2)
    end.
%% -----------------------------------------------------------------------------
%% Reaching

%% @spec reach([node()], [Opt]) -> [node()]
%%       Opt = back | {back, bool()} |
%%             safe | {safe, bool()}
%%
%% @doc Given a set of source expression nodes, returns the set of reachable
%% expressions. Supported options are:
%% <dl>
%%  <dt>{@type {back, false@}}</dt>
%%  <dd>This is the default behaviour: the result set contains expressions
%%      that may return the result of one of the source expressions.</dd>
%%
%%  <dt>{@type back | {back, true@}}</dt>
%%  <dd>The result set contains expressions with results that may be returned
%%      by one of the source expressions.</dd>
%%
%%  <dt>{@type {safe, false@}}</dt>
%%  <dd>This is the default behaviour: expressions in the result set may
%%      return a value that is independent of the source set.</dd>
%%
%%  <dt>{@type safe | {safe, true@}}</dt>
%%  <dd>Expressions in the result set cannot return values independent of the
%%      source set.</dd>
%% </dl>

reach(From, Opts) ->
    Dir =
        case proplists:get_value(back, Opts, false) of
            true  -> back;
            false -> forw
        end,
    Init = set_addl(From, set_new()),
    Reach = walk1(fun(Node) -> flow_step(Node, Dir) end, Init, Init),
    Result = 
        case proplists:get_value(safe, Opts, false) of
            false ->
                set_lst(Reach);
            true ->
                Strict = set_filt(Reach,fun(N) -> is_strict(N, Reach, Dir) end),
                Safe = set_filt(Strict, fun(N) -> is_safe(N, Strict, Dir) end),
                Safe = walk1(fun(N) -> safe_step(N, Safe, Dir) end, Init, Init)
        end,
    Fun = fun(Node) -> 
              ?Syn:class(Node) == expr andalso 
              reflib_expression:role(Node) =/= undefined 
          end,
    lists:filter(Fun, Result).

is_strict(Node, Reach, Dir) ->
    lists:all(fun not_dep/1, edges(Node, rev(Dir)))
    andalso
    lists:all(fun(N) -> set_has(N, Reach) end, flow_step(Node, rev(Dir))).

rev(forw) -> back;
rev(back) -> forw.

not_dep({dep, _}) -> false;
not_dep(_)      -> true.

is_safe(Node, Strict, Dir) ->
    lists:all(fun not_depcomp/1, edges(Node, Dir))
    andalso
    lists:all(fun(N) -> set_has(N, Strict) end,
              [N || {flow, N} <- edges(Node, Dir)]).

not_depcomp({dep, _})      -> false;
not_depcomp({{cons, _}, _}) -> false;
not_depcomp(_)           -> true.

safe_step(Node, Safe, Dir) ->
    case set_has(Node, Safe) of
        true  -> [N || {flow, N} <- edges(Node, Dir)];
        false -> []
    end.

walk1(NextF, Work, Set) ->
    case set_select(Work) of
        empty ->
%io:format("empty: ~p~n", [Set]),
            Set;
        {Sel, Rest} ->
            New = [N || N <- NextF(Sel), not set_has(N, Set)],
%io:format("not empty, New: ~p~n", [New]),
            walk1(NextF, set_addl(New, Rest), set_addl(New, Set))
    end.

flow_step(Node, Dir) ->
    lists:flatmap(fun (N) -> flow_step1(N, Dir) end, edges(Node, Dir)).

flow_step1({flow, Next}, _) ->
%io:format("flow: ~p~n", [Next]),
    [Next];
flow_step1({{cons, Ind}, Comp}, Dir=forw) ->
%io:format("cons: ~p~n", [Comp]),
    [Next || Reach <- reach([Comp], [Dir]),
             {{sel, I}, Next} <- edges(Reach, Dir),
             I =:= Ind];
flow_step1({{sel, Ind}, Comp}, Dir=back) ->
%io:format("sel: ~p~n", [Comp]),
    [Next || Reach <- reach([Comp], [Dir]),
             {{cons, I}, Next} <- edges(Reach, Dir),
             I =:= Ind];
flow_step1(_A, _B) ->
%io:format("A: ~p, B: ~p~n", [A, B]),
    [].

edges(Node, forw) ->
    [{flow, To} || To <- ?Graph:path(Node, [flow])] ++
    [{dep, To} || To <- ?Graph:path(Node, [dep])] ++
    [{{sel, ?Graph:index(Node, sel, To)}, To}
        || To <- ?Graph:path(Node, [sel])] ++
    [{{cons, ?Graph:index(To, cons_back, Node)}, To}
        || To <- ?Graph:path(Node, [{cons_back, back}])] ++
    [{{sel, e}, To} || To <- ?Graph:path(Node, [sel_e])] ++
    [{{cons, e}, To} || To <- ?Graph:path(Node, [cons_e])];

edges(Node, back) ->
    [{flow, To} || To <- ?Graph:path(Node, [{flow, back}])] ++
    [{{sel, ?Graph:index(Node, sel, To)}, To}
        || To <- ?Graph:path(Node, [{sel, back}])] ++
    [{{cons, ?Graph:index(To, cons_back, Node)}, To}
        || To <- ?Graph:path(Node, [cons_back])] ++
    [{{sel, e}, To} || To <- ?Graph:path(Node, [{sel_e, back}])] ++
    [{{cons, e}, To} || To <- ?Graph:path(Node, [{cons_e, back}])] ++
    [{dep, To} || To <- ?Graph:path(Node, [{dep, back}])].


%% ---------------------------------------------------------------------------
%% Manupulating the set
set_new() -> [].
set_add(El, Set) -> ordsets:add_element(El, Set).
set_has(El, Set) -> ordsets:is_element(El, Set).
set_filt(Set, Pred) -> ordsets:filter(Pred, Set).
set_lst(Set) -> Set.

set_select([]) -> empty;
set_select([H|T]) -> {H,T}.


set_addl(Lst, Set) ->
    lists:foldl(
      fun(E, S) -> set_add(E, S) end,
      Set, Lst).
