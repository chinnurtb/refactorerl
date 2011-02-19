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

%%% @doc This module implements the inline macro refactoring. The inline
%%% macro refactoring step substitutes a selected macro reference
%%% with the corresponding macro body and takes care of necessary
%%% compensations.

%%% == New heading ==
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(referl_tr_inline_mac).
-vsn("$Rev: 0000 $").
-include("refactorerl.hrl").
-define(NODETAG, '$gn').
-define(IS_NODE(Node), element(1, Node) =:= ?NODETAG).

%% Callbacks
-export([prepare/1, error_text/2]).

%%% ============================================================================
%%% Errors
error_text(not_applicable,[]) ->
    ["Inline macro is not applicable in this case"];
error_text(noquestion,[]) ->
    ["Stringification and embedded macros are not supported"];
error_text(_,_) -> unknown.

%%% ============================================================================
%%% Callbacks

prepare(Args) ->
    Macro   = ?Args:macuse(Args),
    Results = gather(Macro),
%    io:format("~p~n",[Results]),
    fun() ->
        Funs = [
            fun({U,T,V}) -> ?ESG:remove(U,T,V) end,
            fun id/1,
            fun({U,T,V}) -> ?ESG:insert(U,T,V) end,
            fun({U,T,D})->
                V = ?ESG:create(D),
                ?ESG:insert(U,T,V) end,
            fun ?Transform:touch/1 ],
        lists:zipwith(fun lists:foreach/2, Funs, Results),
        ?ESG:close()
    end.
 
%%% ============================================================================
%%% Implementation

gather(Subst) ->
    try
        throw_gather(Subst)
    catch
        error:{badmatch,_} -> throw(?LocalErr0r(not_applicable))
    end.

%% In variable names, single letters stand for graph nodes,
%% while letter pairs stand for edges between two said
%% nodes. Both are sets.
throw_gather(Subst) when ?IS_NODE(Subst) ->
    S      = [Subst],
%
    F      = [_] = ?Query:exec(S,[mref]), %Form
    check_no_questionm(F),
    SF     = product(S,mref,F),
%
    T      = [_|_] = ?Query:exec(S,[{llex,back}]), %Token
    TS     = product(T,llex,S),
    {E,ET} = nods_prods(T,{elex,back},1), %Expr
    {O,TO} = nods_prods(T,orig,1), %Origs
%
    L      = [_|_] = ?Query:exec(S,[llex]), %subst's chiLd
    SL     = product(S,llex,L),
    {P,LP} = nods_prods(L,llex,0), %Param's child
%
    RmLink  = SF++TS++TO++SL++LP++ET,
    DelNode = (P--O)++L++S++T,
    Touch   = E,
%
    ToClone = O--P,
    ToKeep  = O--ToClone,
    EO      = composition(ET,TO),
    MkLink  = restrict_codomain(EO,ToKeep),
    CrLink  = restrict_codomain(EO,ToClone),
    CrMk    = [{U,Tag,?ESG:data(V)} || {U,Tag,V} <- CrLink],
%
    [RmLink,DelNode,MkLink,CrMk,Touch].

%%% ============================================================================
%%% Checks

%% embedded macros and stringification is not currently supported
check_no_questionm(MacroForm) ->
    FChild = ?Query:exec(MacroForm,[flex]),
    BV = [_|_] = lists:filter(
             fun(N)->
                 (referl_graph:data(N))#lex.type==body
             end,
             FChild),
    B = [_|_] = ?Query:exec(BV,[llex]),
    Q = lists:filter(
            fun(N) ->
                #lex{type=T,data=D} = referl_graph:data(N),
                T==token andalso D#token.type==questionm
            end,
            B),
    ?Check(Q == [], ?LocalErr0r(noquestion)).

%%% ============================================================================
%%% Relational graph helpers

%% expand a vertex and report both the edges and the vertices
nods_prods(Parent,Edge,Min) ->
    Edges = expand_vertex(Parent,Edge,Min),
    Nodes = universe(Edges)--Parent,
    {Nodes,Edges}.

%% expand vertices to the directions of Edge and require
%% a minimal result set size for each vertex
expand_vertex(Parent=[_|_],Edge,Min)->
    GenFun = case Edge of
                 {T,back} when is_atom(T) ->
                     fun(N,C)->product(C,T,N) end;
                 T when is_atom(T) ->
                     fun(N,C)->product(N,T,C) end
             end,
    lists:flatmap(
        fun(Node)->
            Children = ?Query:exec([Node],[Edge]),
            case length(Children)>=Min of
                true  -> GenFun([Node],Children);
                false -> erlang:error({badmatch,[]})
            end
        end,
        Parent).

domain(R) ->
    lists:usort([X || {X,_,_}<-R]).

codomain(R)->
    lists:usort([Y || {_,_,Y}<-R]).

universe(R) ->
    lists:usort(domain(R) ++ codomain(R)).

product(A,T,B)->
    [{X,T,Y} || X<-A, Y<-B].

composition(R1,R2) ->
    [{U,T,V} || {U,T,W1}<-R1, {W2,_,V}<-R2, W1==W2].

restrict_codomain(R, A) ->
    [E || E={_,_,V}<-R, lists:member(V,A)].

id(X) -> X.
