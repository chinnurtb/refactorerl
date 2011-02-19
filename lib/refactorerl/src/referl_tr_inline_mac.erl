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

%%% @doc This module implements the inline macro refactoring. The inline
%%% macro refactoring step substitutes a selected macro reference
%%% with the corresponding macro body and takes care of necessary
%%% compensations.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li>A macro usage to be inlined.
%%%       (see {@link referl_args:macuse/1}) </li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>The selected macro must not be contain stringification or another
%%%       macro in its definition.</li>
%%%   <li>The selection must not be inside a macro definition.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>It should be checked for if
%%%       conditions of applicability are met.</li>
%%%   <li>The following must be gathered that are necessary to fully
%%%       finish the transformation:
%%%     <ul>
%%%       <li>Edges to be deleted: those between `subst' and both its
%%%           children and the macro definition; ones connecting all
%%%           intermediate `token' nodes with `subst', the containing
%%%           expression and the original lexical tokens; and the one between
%%%           the `subst' node and its arguments.</li>
%%%       <li>Nodes to be deleted: those lexical children of the `subst'
%%%           nodes parameter child which do not participate in the final
%%%           solution (commas), children of the `subst' node, the `subst'
%%%           node itself and the intermediate `token' nodes.</li>
%%%       <li>Edges to be rewired: those edges that originally connected a
%%%           non-shared lexical node created specifically for the given
%%%           substitution</li>
%%%       <li>Nodes to be created: those nodes which must be cloned from the
%%%           macro definition because they are shared between all usages</li>
%%%       <li>Node to touch at the end: the containing expression</li>
%%%     </ul>
%%%   </li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is implemented for most real-world use cases. What's
%%% left to do for a future enhancement is handling of macros embedded into
%%% each other and stringification (which requires minor modifications to the
%%% analyzer).
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(referl_tr_inline_mac).
-vsn("$Rev: 3185 $").
-include("refactorerl.hrl").

-define(NODETAG, '$gn').
-define(IS_NODE(Node), element(1, Node) =:= ?NODETAG).

%% Callbacks
-export([prepare/1, error_text/2]).

%%% ============================================================================
%%% Callbacks

%% @private
error_text(not_applicable,[]) ->
    ["Inline macro is not applicable in this case"];
error_text(noquestion,[]) ->
    ["Stringification and embedded macros are not supported"];
error_text(_,_) -> unknown.

%% @private
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

%% @doc calls `throw_gather' and traps its exception into an error message
gather(Subst) ->
    try
        throw_gather(Subst)
    catch
        error:{badmatch,_} -> throw(?LocalErr0r(not_applicable))
    end.

%% @doc Checks applicability and gathers all edges and nodes to be deleted,
%% edges to be rewired, nodes to be created and nodes to touch at the end.
%%  In variable names, single letters stand for graph nodes,
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

%%% ----------------------------------------------------------------------------
%%% Checks

%% @doc embedded macros and stringification is not currently supported,
%% so these are signaled as errors this time
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

%%% ----------------------------------------------------------------------------
%%% Relational graph helpers

%% @doc expand a vertex and report both the edges and the vertices
nods_prods(Parent,Edge,Min) ->
    Edges = expand_vertex(Parent,Edge,Min),
    Nodes = universe(Edges)--Parent,
    {Nodes,Edges}.

%% @doc expand vertices to the directions of Edge and require
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

%% @private
domain(R) ->
    lists:usort([X || {X,_,_}<-R]).

%% @private
codomain(R)->
    lists:usort([Y || {_,_,Y}<-R]).

%% @private
universe(R) ->
    lists:usort(domain(R) ++ codomain(R)).

%% @private
product(A,T,B)->
    [{X,T,Y} || X<-A, Y<-B].

%% @private
composition(R1,R2) ->
    [{U,T,V} || {U,T,W1}<-R1, {W2,_,V}<-R2, W1==W2].

%% @private
restrict_codomain(R, A) ->
    [E || E={_,_,V}<-R, lists:member(V,A)].

%% @private
id(X) -> X.
