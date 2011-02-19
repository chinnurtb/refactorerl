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

%%% @doc This module implements the mapping algorithm between the
%%% Refactorerl's and the Wrangler's representation for Erlang
%%% programs.
%%%
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(referl_mapping_server).

-export([start/0, stop/0]).

-include("refactorerl.hrl").

%% @spec start() -> ok
%% @doc Starts the mapping server.
start() ->
    ets:new(ids, [named_table, public]),
    register(mapping_server,spawn_link(fun() -> loop() end)).

loop() ->
    receive
         {ids, [], _, _, Pid} -> Pid ! {ok}, loop();
         {ids, Ch, ChIds, ParentId, Pid} ->
%io:format("~n  ChIds ~p ~n ParentId ~p ~n", [ChIds, ParentId]),
             C = lists:flatten(Ch),
             case {refac_syntax:type(hd(C)), category(hd(C))} of
                 {macro, _} -> 
                     io:format("Skipping macro application --------------~n");
                 {_, macro_name} -> 
                     io:format("Skipping macro application or macro def--~n");
                 _ ->
                     case ets:lookup(ids, ParentId) of
                         [{ParentId, GP}] ->
%io:format("GP : ~p ~n",[GP]),
                             GCh = [{Tag, Node} || 
                                    {Tag, Node} <- ?Syn:children(GP), 
                                     Tag =/= flex, Tag =/= clex, 
                                     Tag =/= elex, Tag =/= elex], 
%io:format("~nGch ~p ~nCh ~p ~n", [GCh, lists:flatten(Ch)]),
                             case ets:lookup(ids, hd(ChIds)) of
                                 [] -> insert(lists:flatten(Ch),ChIds,GCh,GP);
                                 _  -> ok
                             end;
                         _ -> ok %% This happens in case of the different list 
                                 %% representations
                     end
             end,
             Pid ! {ok},
             loop();
         {first, _Node, Id, FName, Pid} ->
             [File] = ?Query:exec(referl_file:find(FName)),
             ins(Id, File),
             Pid ! {ok},
             loop();
         stop ->
             ok;
         _  -> io:format("~n Bad message received from Wrangler------------~n")
    end.

%% @spec stop() -> ok
%% @doc Stops the mapping server.
stop() ->
    mapping_server ! stop,
    ets:delete(ids).

insert([_Ch | _Chs], [Id | Ids], [{exprcl,Cl1}, {exprcl, Cl2}], GP)->
    case ?Expr:kind(GP) of
        list_comp -> 
            ins(Id, Cl1),
            HeadExpr = ?Query:exec1(Cl1, ?Clause:body(), bad_body),
            ins(Id, HeadExpr),    
            Body = ?Query:exec(Cl2, ?Clause:body()),
            ins_list_comp(Ids, Body);           
        list_gen -> 
            ins(Id, Cl1),
            PExpr = ?Query:exec1(Cl1, ?Clause:patterns(), bad_body),
            ins(Id, PExpr),
            ins(hd(Ids), Cl2),
            GExpr = ?Query:exec1(Cl2, ?Clause:body(), bad_body),
            ins(hd(Ids), GExpr);
        infix_expr ->
            ins(Id, Cl1),
            ins(hd(Ids), Cl2),
            Body1 = ?Query:exec1(Cl1, ?Clause:body(1), bad_body),
            Body2 = ?Query:exec1(Cl2, ?Clause:body(1), bad_body),
            ins(Id, Body1),
            ins(hd(Ids), GP),
            ins(hd(tl(Ids)), Body2);
        _ -> 
            ins(Id, Cl1),
            ins(hd(Ids), Cl2) 
    end;
insert(Chs, Ids, [{name, _Node }| Nodes], GP) ->
    insert(Chs, Ids, Nodes, GP);
insert([Ch | Chs], [Id | Ids], [{funcl, Node }| Nodes], GP) ->
    case refac_syntax:type(Ch) of
        atom ->
            GName = ?Query:exec1(Node, ?Clause:name(), bad_node),
            ins(Id, GName),
            insert(Chs, Ids,[{funcl, Node }| Nodes], GP);
        _ ->
            ins(Id, Node),
            insert(Chs, Ids, Nodes, GP)
    end;
insert(Chs, [Id | Ids], [{headcl, Node }| Nodes], GP) ->
    ins(Id, Node),
    Body = ?Query:exec1(Node, ?Clause:body(1), bad_body),
    insert(Chs, [Id | Ids], [{body, Body} | Nodes], GP);
insert([Ch | Chs], [Id | Ids], [{attr, Node }| Nodes], GP) ->
    case refac_syntax:data(Ch) of
        module ->
            insert(Chs, Ids, [{attr, Node} |Nodes], GP);
        export ->
            insert(Chs, Ids, [{attr, Node} |Nodes], GP);
        import ->
            insert(Chs, Ids, [{attr, Node} |Nodes], GP);
        _ ->
            ins(Id, Node),
            insert(Chs, Ids, Nodes, GP)
    end;
insert(C = [Ch | Chs], I = [_Id | Ids], N = [_H|_T], GP) ->
    case {refac_syntax:type(Ch), category(Ch)} of
        {macro, _} -> %% maybe this branch is unnecessary
             io:format("Skipping macro application --------------~n");
        {comment, _} -> 
             io:format("Skipping comments -----------------------~n"),
             insert2(Chs, Ids, N, GP);
        {_, macro_name} -> %% maybe this branch is unnecessary
             io:format("Skipping macro application or macro def--~n");
        _ -> insert2(C, I, N, GP)
    end;
insert(_, [Id], [], GP) -> %% this occures in case of guards
    ins(Id, GP);
insert(_, [], _, _) ->
     ok;
insert([Ch | _Chs], _, _, _) ->
    case refac_syntax:data(Ch) of
        {_, _, define} ->
            io:format("Skipping macro definition ---------------~n");
        _ ->
            io:format("Mapping ended with errors ------------------------~n")
    end.

insert2(C = [_Ch | Chs], I = [Id | Ids], N = [{_Tag, Node }| Nodes], GP) ->
    case {?Syn:class(Node), ?Syn:class(GP)} of
        {expr, expr} ->    
            case ?Expr:type(Node) of
                guard ->
                    Y = length(C),
                    case length(N) of
                        X when X > Y -> ins(Id, GP);
                        X when X < Y -> find_jump(C, I, N, ?Expr:kind(GP), GP);
                        _ ->
                            ins(Id, Node),
                            insert(Chs, Ids, Nodes, GP)
                    end;
                _ -> find_jump(C, I, N, ?Expr:kind(GP), GP)
            end;
        {expr, _} -> find_jump(C, I, N, clause, GP);
        {_, expr} -> find_jump(C, I, N, ?Expr:kind(GP), GP);
        _ -> 
            ins(Id, Node),
            insert(Chs, Ids, Nodes, GP)
    end.

ins_list_comp([Id | Ids], [Node | Nodes])->
    case ?Expr:kind(Node) of 
        filter -> 
            ins(Id, Node),
            FCl = ?Query:exec1(Node, ?Expr:clause(1), bad_filter),
            ins(Id, FCl),
            ins(Id, ?Query:exec1(FCl, ?Clause:body(), bad_body));
        _ -> 
            ins(Id, Node)
    end,
    ins_list_comp(Ids, Nodes);
ins_list_comp([],[]) -> ok.

find_jump(_ ,[Id1, Id2, Id3],[{_, Node1},{_, Node3}], infix_expr, Node2)->
    ins_cond(Id1, Node1),
    ins(Id2, Node2),
    ins_cond(Id3, Node3);
find_jump([_ | Chs], [Id | _], _, cons, GP)-> 
    WElems = [Id | get_wid(Chs, [])],
    RElems = get_node(GP),
    Pairs = lists:zip(WElems, RElems),
    Fun = fun({A,B}) -> ins_cond(A,B) end,
    lists:foreach(Fun, Pairs);
find_jump(Chs , Ids, [{_, Clause}], block_expr, Block)->
    BlockBody = [{body, Expr} || Expr <- ?Query:exec(Clause, ?Clause:body())],
    [{Id, Block}] = ets:match_object(ids, {'_', Block}),
    ins(Id, Clause),
    insert(Chs, Ids, BlockBody, Clause);
find_jump([_Ch | Chs], [Id | Ids], [ {_, Node }| Nodes], _, GP) ->
    ins_cond(Id, Node),
    insert(Chs, Ids, Nodes, GP).

get_node(Cons)->
    List = ?Query:exec(Cons, ?Expr:child(1)),
    ?Query:exec(List, ?Expr:children()) ++ ?Query:exec(Cons, ?Expr:child(2)).

get_wid([], Ids)-> Ids;
get_wid([Hd | Tail], Ids) -> 
    R = case refac_syntax:type(Hd) of
            list ->
                [Ch1 | Ch2] = lists:flatten(refac_syntax:subtrees(Hd)),
                Id1 = refac_util:get_id(Ch1),
                get_wid(Ch2, Ids ++ [Id1]);
            _ -> Ids ++ [refac_util:get_id(Hd)]
        end,
    get_wid(Tail, R).        
    
ins(A, B) ->
%io:format("Insert: ~p ~n ", [{A, B}]),
    ets:insert(ids, {A, B}).%,
%io:format("Insert succeded ~n").

ins_cond(Id, Node)->
    case ?Syn:class(Node) of
        expr ->
            case ?Expr:kind(Node) of
                parenthesis ->
                    ins(Id, Node),
                    PExpr = ?Query:exec1(Node, ?Expr:child(1), bad_filter),
                    ins_cond(Id, PExpr);
                _  ->
                    ins(Id, Node)
            end;
        _ -> ins(Id, Node)
    end.

category(Tree) ->
    case lists:keysearch(category, 1, refac_syntax:get_ann(Tree)) of
        {value, {category, C}} -> C;
        _ -> error
    end.

%% test - complex binaries


%%% TODO - Does Wrangler use somewhere the AST with expanded macros?
%%% In duplicated code detection does not.
