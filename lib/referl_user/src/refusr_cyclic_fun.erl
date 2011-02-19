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

%%% @doc Cyclic depedency examination in function level in the graph
%%% storage server. Draws out the dependency graph. <br/> X function is
%%% dependent from Y function if there is a Y function call in the
%%% definition of X. (X -> Y)<br/> For example, X -> Y -> Z -> X is a
%%% cycle.
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_cyclic_fun).
-vsn("$Rev: 5614 $").
-export([print_cycle/0, check_cycle/0, check_function/1, draw/0, draw/1, draw_cycles/0]).

-include("user.hrl").

%%% ============================================================================
%%% Graph queries


get_all_functions()->
    ?Query:exec(?Query:seq(?Mod:all(), ?Mod:locals())).

get_funcall(Functions)->
    [{Fun, Callees} || Fun <- Functions ,
                       Callees <- [?Query:exec(Fun, ?Fun:funcalls())],
                       Callees /= []].

get_func_mod(Fun)->
    ?Query:exec1(Fun, ?Fun:module(), mod_not_found).

%%% ============================================================================
%%% Building a digraph


build_graph(Graph)-> build_vertex(Graph, get_funcall(get_all_functions())).

build_vertex(_, [])-> ok;
build_vertex(Graph, [{Fun, Callees} | Rest]) ->
    check_subgraph(Graph, Fun),
    [begin
         check_subgraph(Graph, Callee),
         check_edge(Graph, Fun, Callee, "called")
     end
     || Callee <- Callees],
    build_vertex(Graph, Rest).

check_subgraph(Graph, Fun) ->
    Mod = get_func_mod(Fun),
    check_vertex(Graph, Mod),
    check_vertex(Graph, Fun),
    check_edge(Graph, Mod, Fun, "").

check_vertex(Graph, Vertex)->
    case digraph:vertex(Graph, Vertex) of
        false -> digraph:add_vertex(Graph, Vertex);
        _ -> ok
    end.

check_edge(Graph, V1, V2, Label)->
    case edge(Graph, V1, V2) of
	[] -> digraph:add_edge(Graph, V1, V2, Label);
	_ -> ok
    end.

edge(G, V1, V2)->
    [digraph:edge(G, E)
     || E <- ?MISC:intersect(digraph:out_edges(G, V1),
                             digraph:in_edges(G, V2))].

cycles(_, [], Cycles)-> Cycles;
cycles(Graph, [Head|Tail], Cycles)->
    case digraph:get_cycle(Graph, Head) of
	false -> cycles(Graph, Tail, Cycles);
	Cycle ->
            NewTail = [V || V <- Tail, not lists:member(V, Cycle)],
            cycles(Graph, NewTail, [Cycle | Cycles])
    end.

check_cycle(Graph, Vertices)->
    Cycles = cycles(Graph, Vertices, []),
    case Cycles of
	[]-> {ok, no_cyclic_dependency};
	_ -> {integer_to_list(length(Cycles)) ++ " cycle(s)", Cycles}
    end.

%%------------------------------------------------------------------------------------------------------------------
%% @spec check_cycle() -> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Builds a directed graph, checks for cycles in function level. If
%% cycles are found then the following tuple is the result: {Number of
%% cycles, Cycle list}.
check_cycle()->
    Graph = digraph:new(),
    try
        build_graph(Graph),
        check_cycle(Graph, digraph:vertices(Graph))
    after
        digraph:delete(Graph)
    end.

%% @spec print_cycle() -> ok
%% @doc Prints the result of check_cycle/0 to the standard output.
print_cycle() ->
    case check_cycle() of
        {Int, Nodes} when is_list(Nodes)->
            ListText = [[list_to_atom(lists:flatten(?MISC:fun_text(
                             [?Mod:name(?Query:exec1(Fun, ?Fun:module(), no_mod)), 
                              ?Fun:name(Fun), 
                              ?Fun:arity(Fun)]))) || Fun <- Cycle] || Cycle <- Nodes],
            io:format("~p~n~p~n", [Int, ListText]);
        Other -> io:format("~p~n", [Other])
    end. 

parse_funId(Str) ->
    try
        [Mod, Fun, Arity] = string:tokens(Str, ": /"),
        {list_to_atom(Mod), list_to_atom(Fun), list_to_integer(Arity)}
    catch
        _:_ -> false
    end.

get_node(FunId)->
    case parse_funId(FunId) of
        {Mod, Fun, Arity} ->
            ?Query:exec(?Query:exec(?Mod:find(Mod)), ?Fun:find(Fun, Arity));
        false ->
            []
    end.

%%------------------------------------------------------------------------------------------------------------
%% @spec check_function(Function)-> true | false | {error, bad_function_name} | erlang:error(error, Args)
%% Function = node() | string()
%% @doc Creates a subgraph from the given function as a starting node and checks for cycles.
%% <br/>The function can be given as a graph node or  in the following
%% order and combination:<br/> "module:name/arity" (eg.: "erlang:hd/1").
check_function({_, func , _} = Node) ->
    ?Graph:data(Node),
    check_fun_graph([Node]);


check_function(Fun)->
    case get_node(Fun) of
	[]->
	    {error, bad_function};
	Node -> check_fun_graph(Node)
    end.


check_fun_graph(Node)->
    Graph = digraph:new(),
    try
	build_subgraph(Graph, Node),
	case check_cycle(Graph, digraph:vertices(Graph)) of
	    {ok, no_cyclic_dependency} -> false;
	    _ -> true
	end
    after
        digraph:delete(Graph)
    end.

build_subgraph(Graph, Node)->
    case get_funcall(Node) of
	[] -> ok;
	[{Fun, CalledList}]->  
	    case lists:subtract(CalledList, digraph:vertices(Graph)) of
		[] ->  build_vertex(Graph, [{Fun, CalledList}]);
		NewVertices -> build_vertex(Graph, [{Fun, CalledList}]),
		              [build_subgraph(Graph, [Called])|| Called<-NewVertices]
	    end
    end.
		



%%-----------------------------------------------------------------------------------------------------------

%% @spec draw()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Equivalent to draw(""), prints out the entire graph. Output file is dep_func.dot.
draw()-> draw("").

%% @spec draw(Fun)-> {ok, no_cyclic_dependency} | {integer(), list()} | {error, bad_function_name} | erlang:error(error, Args)
%% Fun = node() | string()
%% @doc Creates a subgraph drawing from the directed graph from a given function as a starting node.
%% The function can be given as a graph node or  in the following
%% order and combination: "module:name/arity" (eg.: "erlang:hd/1").<br/>
%% The file's name: dep_func_<i>module_function_arity</i>.dot.
%% <br/> The indicators are the following:<br/>
%% - Red edge: cycle edge<br/>
%% - Dashed edge: funcall edge, a function calls, depends on another<br/>
%% - Purple box: module<br/>
%% - Black hexagon: function
draw(Fun)->
    Graph = digraph:new(),
    try
	case Fun of
	    "" -> 
		build_graph(Graph), 
		check_graph(Graph, "dep_func.dot");
	    "cycle" -> 
		cycle_graph(Graph);
	    {_, func, _} = Node ->
		?Graph:data(Node),
		File = make_file_name(?Mod:name(get_func_mod(Node)),
				      ?Fun:name(Node),
				      ?Fun:arity(Node)),
		fun_graph(Graph, [Node], File);    
	    _ -> 
		case get_node(Fun) of
		     []->
			 {error, bad_function};
		     Node ->
			 {Mod, Func, Arity} = parse_funId(Fun),
			 fun_graph(Graph, Node, make_file_name(Mod, Func, Arity))  
		 end
	end
    after
        digraph:delete(Graph)
    end.

%% @spec draw_cycles()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Equivalent to draw("cycle"), prints out a subgraph which contains the cycle(s).
%% Unless cycles exist, calls {@link draw/0}.
%% Output file is dep_func_cycles.dot.
draw_cycles()->
    draw("cycle").

check_graph(Graph, File)->
    Result = check_cycle(Graph, digraph:vertices(Graph)),
    write_file(Graph, Result, File ),
    Result.

fun_graph(Graph, Node, File)->
    build_subgraph(Graph, Node),
    check_graph(Graph, File).


cycle_graph(Graph)->
    build_graph(Graph), 
    Cycles =check_cycle(Graph, digraph:vertices(Graph)),
    case Cycles of
	{_, no_cyclic_dependency}->
	     draw();
	{_, Vertices} ->  Subgraph_vertices = lists:usort(lists:flatten(Vertices) ++
							  [get_func_mod(Vertex) 
							   || VList<- Vertices,
							      Vertex<-VList]),
			  Subgraph = digraph_utils:subgraph(Graph, Subgraph_vertices),
			  try
			      write_file(Subgraph, Cycles, "dep_func_cycles.dot")
			  after
			      digraph:delete(Subgraph)
			  end
    end,
    Cycles.

make_file_name(Mod, Func, Arity)->
    "dep_func_" ++ atom_to_list(Mod) ++ "_" 
	++ atom_to_list(Func)
	++ "_" ++ integer_to_list(Arity) ++".dot".
    
  

-define(Dot_Header, "digraph dependency_graph_function_level {\n").
-define(Dot_Root, "Nroot0 [shape=\"triangle\", label=\"ROOT\", fontsize=\"18\","
        "URL=\"#ok\", tooltip=\"node={'$gn', root, 0}\"]\n").
-define(Dot_Footer, "}\n").
-define(Dot_Node, "N~s~b [shape=\"~s\", label=\"~s\", fontsize=\"~b\","
        "color=\"~s\"  URL=\"#ok\", tooltip=\"node={'$gn', ~s, ~b}\"]\n").
-define(Dot_Edge, "N~s~b -> N~s~b [color=\"~s\", style =\"~s\" ]\n").
-define(Dot_CEdge, "Nfunc~b -> Nfunc~b [color=\"red\", style=\"~s\" ]\n").

write_file(Graph, Result, File) ->
    case file:open(File, [write]) of
        {ok, Dev}->
            try
                io:put_chars(Dev, ?Dot_Header),
                io:put_chars(Dev, ?Dot_Root),

                [draw_vertex(Vs, Dev) 
                 || Component <- digraph_utils:components(Graph),
                    Vs <- Component],
                case Result of
                    {_, no_cyclic_dependency} -> ok;
                    {_, List} ->
                        [begin
				if length(Edge) == 1 -> draw_loop_edge(Graph, hd(Edge), Dev);
				   true -> draw_cyclic_edge(Graph, Edge, Dev)
				end
			end || Edge <- List]
                end,
                [draw_edge(Edge, Dev) || Edge <- list_edges(Graph)],

                io:put_chars(Dev, ?Dot_Footer)
            after
                file:close(Dev)
            end;
        {error, Reason}->
            io:format("dep_func.dot: ~s~n", file:format_error(Reason))
    end.

draw_vertex(V, Dev)->
    {_, T, M} = V,
    {Shape, Size, Color} =
        case T of
            module -> {box, 18, purple};
            func   -> {hexagon, 14, black}
        end,

    io:format(Dev, ?Dot_Node, [T, M, Shape, label(V), Size, Color, T, M]),
    case T of
	module -> io:format(Dev, "Nroot0->N~s~b [color= \"black\"]\n", [T, M]);
        _ -> ""
    end.

draw_edge(Edge, Dev)->
    {_, {_, T1, V1}, {_, T2, V2}, Label} = Edge,
    {Color, Style} =
        case Label of
            "called" -> {black, dashed};
            _        -> {black, solid}
        end,
     io:format(Dev, ?Dot_Edge, [T1, V1, T2, V2, Color, Style]).


draw_cyclic_edge(_, [_ | []], _) -> ok;
draw_cyclic_edge(Graph, Edges, Dev)->
  E1 = hd(Edges),
  E2 = hd(tl(Edges)),
  case edge(Graph, E1, E2) of
   	[] ->  draw_cyclic_edge(Graph, tl(Edges), Dev);
  	[H | _] ->
  	    Style = case element(4, H) of
			"called" ->  "dashed";
  			_  ->  "solid"
		    end,
	    io:format(Dev, ?Dot_CEdge, [element(3, E1), element(3, E2), Style]),
	    digraph:del_edge(Graph, element(1, hd(edge(Graph, E1, E2)))),
	    draw_cyclic_edge(Graph, tl(Edges), Dev)
    end.

draw_loop_edge(Graph, Node, Dev)->
	case edge(Graph, Node, Node) of
       		 	[] ->  ok;
        		[H | _] ->
        		    Style = case element(4, H) of
					"called" ->  "dashed";
					_  ->  "solid"
				    end,
			    io:format(Dev, ?Dot_CEdge, [element(3, Node), element(3, Node), Style]),
			    digraph:del_edge(Graph, element(1, hd(edge(Graph, Node, Node))))
	end.
		

label(Vertex)->
    case ?Graph:class(Vertex) of
	module -> ?Mod:name(Vertex);
	func   -> atom_to_list(?Fun:name(Vertex)) ++ "/" ++
                      integer_to_list(?Fun:arity(Vertex))
    end.

list_edges(Graph)->
    [digraph:edge(Graph, E) || E <- digraph:edges(Graph)].
