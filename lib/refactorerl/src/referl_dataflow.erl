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


%%% @doc Data flow graph builder module. A static data flow graph can be built
%%% using this module, and there are operations that calculate data flow
%%% analysis results from this graph.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_dataflow).
-vsn("$Rev: 3006 $").

%% Data flow graph building
-export([new/0, new/1, delete/1, add/2, draw/2]).

%% Data flow analysis
-export([reach/2, safe_reach/2]).


-include("refactorerl.hrl").

%% @type graph(). A data flow graph.

-record(graph, {funs, flow, trace}).
-define(Trace(G, Fmt, Args),
        if
            G#graph.trace -> io:format(Fmt "~n", Args);
            true -> ok
        end).

%% @spec new() -> graph()
%% @doc Same as `new([])'.
%% @see new/1
new() ->
    new([]).

%% @spec new(Options::[Option]) -> graph()
%% @doc Create a new, empty data flow graph. `Option' can be:
%% <dl>
%%  <dt>`trace'</dt><dd>Trace the operations on the graph.</dd>
%% </dl>
new(Opt) ->
    #graph{funs = ets:new(funs, []),
           flow  = digraph:new(),
           trace = proplists:get_value(trace, Opt, false)}.

%% @spec delete(graph()) -> ok
%% @doc Deletes a data flow graph.
delete(#graph{flow=Fl, funs=Fs}) ->
    ets:delete(Fs),
    digraph:delete(Fl).

%% @spec add(graph(), [node()]) -> ok
%%
%% @doc Add a set of functions to the data flow graph. Functions may be
%% specified by `#function{}' nodes, or `#module{}' nodes which mean every
%% function in the module.
add(G, Nodes) ->
    Funs = lists:flatmap(fun funlist/1, Nodes),
    Exprs = lists:flatmap(fun (F) -> add_fun(G, F) end, Funs),
    lists:foreach(fun (E) -> add_expr(G, E) end, Exprs),
    ok.

funlist(Node) ->
    case ?Graph:class(Node) of
        module -> ?Query:exec(Node, ?Mod:locals());
        function -> [Node]
    end.

add_fun(G=#graph{funs=Fs, flow=Fl}, Fun) ->
    ?Trace(G, "Adding fun ~p", [Fun]),
    Clauses = ?Query:exec(Fun, ?Query:seq(?Fun:definition(), ?Form:clauses())),
    ?Trace(G, "  Clauses: ~p", [Clauses]),
    ets:insert(Fs, {Fun, Clauses}),
    Exprs =
      lists:flatmap(
        fun (Cl) ->
                ?Query:exec(Cl,?Query:seq(?Clause:exprs(),?Expr:top_deep_sub()))
        end,
        Clauses),
    ?Trace(G, "  Expressions: ~p", [Exprs]),
    lists:foreach(fun (E) -> digraph:add_vertex(Fl, E) end, Exprs),
    Exprs.

add_expr(G, Expr) ->
    Data = ?Graph:data(Expr),
    ?Trace(G, "~p: ~p", [Expr, Data#expr.kind]),
    add_expr(G, Expr, Data).

    %% Label =
    %%     case lists:member(Data#expr.kind,
    %%                       [atom, char, float, integer, string]) of
    %%         true -> [Data#expr.value];
    %%         false -> []
    %%     end,


%% -----------------------------------------------------------------------------
%% Static data flow edge generation

add_expr(G, Expr, #expr{kind=variable}) ->
    case ?Query:exec(Expr, ?Expr:varrefs()) of
        [] -> ok;
        [Var] ->
            [add_edge(G, Bind, Expr, f) ||
                Bind <- ?Query:exec(Var, ?Var:bindings())]
    end;

add_expr(G=#graph{funs=Fs}, Expr, #expr{kind=application}) ->
    [_ | Pars] = ?Query:exec(Expr, ?Expr:children()),
    Clauses =
        case ?Query:exec(Expr, ?Expr:function()) of
            [Fun] ->
                case ets:lookup(Fs, Fun) of
                    [] -> [];
                    [{_, Cls}] -> Cls
                end;
            [] -> []
        end,
    if
        Clauses =:= [] ->
            [add_edge(G, P, Expr, d) || P <- Pars];
        true ->
            [begin
                 Args = ?Query:exec(Cl, ?Clause:patterns()),
                 [add_edge(G, P, A, f) ||
                     {P, A} <- lists:zip(Pars, Args)],
                 [Res] = ?Query:exec(Cl, ?Clause:body(last)),
                 add_edge(G, Res, Expr, f)
             end || Cl <- Clauses]
    end;

%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=fun_expr}) ->
%%  add clauses to `funs'

add_expr(G, Expr, #expr{kind=match_expr, type=pattern}) ->
    [P1, P2] = ?Query:exec(Expr, ?Expr:children()),
    add_edge(G, Expr, P1, f),
    add_edge(G, Expr, P2, f);

add_expr(G, Expr, #expr{kind=match_expr}) ->
    [Pat, Exp] = ?Query:exec(Expr, ?Expr:children()),
    add_edge(G, Exp, Expr, f),
    add_edge(G, Exp, Pat, f);

add_expr(G, Expr, #expr{kind=cons, type=pattern}) ->
    [add_edge(G, Expr, Hd, {s,e}) ||
        Hd <- ?Query:exec(Expr, ?Query:seq(?Expr:child(1), ?Expr:children()))],
    [add_edge(G, Expr, Tl, {s,l}) ||
        Tl <- ?Query:exec(Expr, ?Expr:child(2))];

add_expr(G, Expr, #expr{kind=cons}) ->
    [add_edge(G, Hd, Expr, {c,e}) ||
        Hd <- ?Query:exec(Expr, ?Query:seq(?Expr:child(1), ?Expr:children()))],
    [add_edge(G, Tl, Expr, {c,l}) ||
        Tl <- ?Query:exec(Expr, ?Expr:child(2))];

%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=list_comp}) ->
%%  [ E || ... ]
%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=list_gen}) ->
%%  P <- E
%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=binary_gen}) ->
%%  P <= E


%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=binary}) ->
%% << ... >>
%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=bin_comp}) ->
%%  << E || ... >>

add_expr(G, Expr, #expr{kind=tuple, type=T}) ->
    Children = ?Query:exec(Expr, ?Expr:children()),
    [case T of
         pattern ->
             add_edge(G, Expr, Sub, {s,I});
         _ ->
             add_edge(G, Sub, Expr, {c,I})
     end || {Sub, I} <- lists:zip(Children, lists:seq(1, length(Children)))];

%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=record_access}) ->
%%  E#rec.fld
%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=record_update}) ->
%%  E#rec{fld=V}
%% add_expr(G=#graph{flow=Fl}, Expr, #expr{kind=record_expr}) ->
%%  #rec{fld=V}

add_expr(G, Expr, #expr{kind=send_expr}) ->
    [_Target, Exp] = ?Query:exec(Expr, ?Expr:children()),
    add_edge(G, Exp, Expr, f);

add_expr(G, Expr, #expr{kind=parenthesis}) ->
    [Res] = ?Query:exec(Expr, ?Expr:children()),
    add_edge(G, Res, Expr, f);

add_expr(G, Expr, #expr{kind=infix_expr}) ->
    [add_edge(G, Sub, Expr, d) ||
        Sub <- ?Query:exec(Expr, ?Expr:children())];

add_expr(G, Expr, #expr{kind=block_expr}) ->
    [Res] = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:body(last))),
    add_edge(G, Res, Expr, f);

add_expr(G, Expr, #expr{kind=catch_expr}) ->
    [Res] = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:body(last))),
    add_edge(G, Res, Expr, f);

add_expr(G, Expr, #expr{kind=if_expr}) ->
    [add_edge(G, Res, Expr, f) ||
        Res <- ?Query:exec(Expr,
                           ?Query:seq(?Expr:clauses(), ?Clause:body(last)))];

add_expr(G, Expr, #expr{kind=case_expr}) ->
    [Head] = ?Query:exec(Expr, [headcl, body]),
    [begin
         [Pat] = ?Query:exec(Cl, ?Clause:patterns()),
         add_edge(G, Head, Pat, f),
         [Res] = ?Query:exec(Cl, ?Clause:body(last)),
         add_edge(G, Res, Expr, f)
     end || Cl <- ?Query:exec(Expr, [exprcl])];

add_expr(G, Expr, #expr{kind=receive_expr}) ->
    [Res] = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:body(last))),
    add_edge(G, Res, Expr, f);

add_expr(G, Expr, #expr{kind=try_expr}) ->
    [Head] = ?Query:exec(Expr, [headcl, body]),
    [begin
         [Pat] = ?Query:exec(Cl, ?Clause:patterns()),
         add_edge(G, Head, Pat, f),
         [Res] = ?Query:exec(Cl, ?Clause:body(last)),
         add_edge(G, Res, Expr, f)
     end || Cl <- ?Query:exec(Expr, [exprcl])],
    [begin
         [Res] = ?Query:exec(Cl, ?Clause:body(last)),
         add_edge(G, Res, Expr, f)
     end || Cl <- ?Query:exec(Expr, [catchcl])];

add_expr(_, _, _) ->
    ok.


add_edge(G=#graph{flow=Fl}, From, To, Label) ->
    ?Trace(G, "  ~p -~p-> ~p", [From, Label, To]),
    digraph:add_edge(Fl, From, To, Label).

%% -----------------------------------------------------------------------------
%% Reaching

%% @spec reach(graph(), [node()]) -> [node()]
%% @doc
%% Given a set of source expression nodes, returns the set of reachable
%% expressions, that is, every expression that may get its value from one of
%% the source nodes.
reach(G, From) ->
    Init = set_addl(From, set_new()),
    reach(G, Init, Init).

reach(_G, [], Set) -> set_lst(Set);
reach(G, [Next | Rest], Set) ->
    case next_reach(G, Next) of
        [] ->
            reach(G, Rest, Set);
        Reach ->
            New = [N || N <- Reach, not set_has(N, Set)],
            reach(G, Rest ++ New, set_addl(New, Set))
    end.

next_reach(G=#graph{flow=Fl}, Node) ->
    lists:flatmap(fun (Edge) -> next_reach1(G, Edge) end,
                  digraph:out_edges(Fl, Node)).

next_reach1(G=#graph{flow=Fl}, Edge) ->
    case digraph:edge(Fl, Edge) of
        {_, _, Next, f} -> [Next];
        {_, _, Comp, {c, Ind}} ->
            [Next || Reach <- reach(G, [Comp]),
                     Out <- digraph:out_edges(Fl, Reach),
                     {_, _, Next, {s, I}} <- [digraph:edge(Out)],
                     I =:= Ind];
        _ -> []
    end.

%% @spec safe_reach(graph(), [node()]) -> [node()]
%%
%% @doc Return the set of safely reachable nodes. The result may have to be
%% split into safe nodes and border nodes.
safe_reach(_G, _From) ->
    throw(unimplemented).

set_new() -> [].
set_add(El, Set) -> ordsets:add_element(El, Set).
set_has(El, Set) -> ordsets:is_element(El, Set).
set_lst(Set) -> Set.

set_addl(Lst, Set) ->
    lists:foldl(
      fun(E, S) -> set_add(E, S) end,
      Set, Lst).

%% -----------------------------------------------------------------------------


%% @spec draw(graph(), Base::string()) -> ok
%%
%% @doc Create a set of graph drawings from the data flow graph. Every
%% component of the graph is written into its own `dot' file, their names
%% begin with `File', and they are indexed with natural numbers.
draw(#graph{flow=G}, File) ->
    Comp = [ C || C <- digraph_utils:components(G),
                  length(C) > 2],
    io:format("~b components~n", [length(Comp)]),
    lists:foreach(
      fun ({K, Vert}) ->
              case file:open(io_lib:format("~s~b.dot", [File, K]), [write]) of
                  {ok, Dev} ->
                      io:put_chars(
                        Dev,
                        "digraph flow {\n"
                        "node [shape=box,fontname=\"DejaVu Sans Mono\"]\n"
                        "edge [fontname=\"DejaVu Sans\",fontsize=8]\n"),
                      [draw(G, V, Dev) || V <- Vert],
                      io:put_chars(Dev, "}\n"),
                      file:close(Dev);
                  {error, Reason} ->
                      io:format("~s~b.dot: ~s~n",
                                [File, K, file:format_error(Reason)])
              end
      end,
      lists:zip(lists:seq(1, length(Comp)), Comp)),
    G.

draw(G, V, Dev) ->
    {_, _, I} = V,
    io:format(Dev, "N~b [shape=~s,label=<~s>]\n", [I, shape(V), text(V)]),
    [io:format(Dev, "N~b -> N~b [label=\"~p\",color=\"~s\"]\n",
               [I, To, Lab, color(Lab)]) ||
        Edge <- digraph:out_edges(G, V),
        {_, _, {_, _, To}, Lab} <- [digraph:edge(G, Edge)]].

shape(V) ->
    case ?Graph:data(V) of
        #expr{type=pattern} -> "hexagon";
        #expr{} -> "box";
        _ -> "circle"
    end.

color(f) -> blue;
color({c,_}) -> yellow;
color({s,_}) -> green;
color(d) -> red;
color(_) -> black.

text(Node) ->
    Text = string:strip(lists:flatten(?Syn:tree_text(Node))),
    FunText =
        case ?Query:exec(Node, ?Query:seq([?Expr:sup(), ?Expr:clause(),
                                           ?Clause:form(), ?Form:func()])) of
            [Fun] ->
                atom_to_list(?Fun:name(Fun)) ++ "/" ++
                    integer_to_list(?Fun:arity(Fun));
            [] -> "?"
        end,
    ["<FONT POINT-SIZE=\"9\">",
     lists:flatmap(
       fun ($<) -> "&lt;";
           ($>) -> "&gt;";
           ($&) -> "*amp;";
           ($\n) -> "<BR ALIGN=\"left\"/>";
           (C) -> [C]
       end, Text),
     "</FONT>",
     "<BR ALIGN=\"left\"/>",
     "<FONT POINT-SIZE=\"7\">", FunText, "</FONT>",
     "<BR ALIGN=\"right\"/>"].

%% -----------------------------------------------------------------------------

%% name(G, Expr, #expr{kind=atom
%% name(G, Expr, #expr{kind=binary
%% name(G, Expr, #expr{kind=char
%% name(G, Expr, #expr{kind=float
%% name(G, Expr, #expr{kind=implicit_fun
%% name(G, Expr, #expr{kind=integer
%% name(G, Expr, #expr{kind=mstring
%% name(G, Expr, #expr{kind=string
%% name(G, Expr, #expr{kind=record_index
%% name(G, Expr, #expr{kind=record_expr

%% name(G, Expr, #expr{kind=variable
%% name(G, Expr, #expr{kind=match_expr
%% name(G, Expr, #expr{kind=application

%% name(G, Expr, #expr{kind=cons
%% name(G, Expr, #expr{kind=list
%% name(G, Expr, #expr{kind=tuple
%% name(G, Expr, #expr{kind=infix_expr
%% name(G, Expr, #expr{kind=send_expr
%% name(G, Expr, #expr{kind=record_access
%% name(G, Expr, #expr{kind=record_update
%% name(G, Expr, #expr{kind=parenthesis
%% name(G, Expr, #expr{kind=list_comp
%% name(G, Expr, #expr{kind=bin_comp
%% name(G, Expr, #expr{kind=list_gen
%% name(G, Expr, #expr{kind=binary_gen
%% name(G, Expr, #expr{kind=filter

%% name(G, Expr, #expr{kind=block_expr
%% name(G, Expr, #expr{kind=catch_expr
%% name(G, Expr, #expr{kind=if_expr
%% name(G, Expr, #expr{kind=case_expr
%% name(G, Expr, #expr{kind=try_expr
%% name(G, Expr, #expr{kind=fun_expr
%% name(G, Expr, #expr{kind=receive_expr

