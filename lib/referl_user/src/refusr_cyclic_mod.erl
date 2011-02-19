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

%%% @doc Cyclic depedency examination in module level in the graph storage 
%%% server. Draws out the dependency graph. <br/>
%%% X module is dependent from Y module: in the definition of a function of X 
%%% there is a function call of a function of Y.
%% (X -> Y) <br/>For example, X -> Y -> Z -> X is a cycle. 
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_cyclic_mod).
-vsn("$Rev: 5614 $").
-export([print_cycle/0, check_cycle/0, check_module/1, draw/0, draw_cycles/0, draw/1]).

-include("user.hrl").


get_all_modules()->
   ?Query:exec(?Mod:all()).

get_modcalls(Mods)->
    [{Mod,  get_mod_modcall(Mod)}
     || Mod<-Mods,
	get_mod_modcall(Mod) /= []].

get_mod_modcall(Mod)->
    [get_func_mod(Called)
     ||Called <- get_mod_funcall(Mod),
       get_func_mod(Called) /= Mod ].

get_mod_funcall(Mod)->
    lists:flatten([Funcalls
			      || Fun<-[?Query:exec(Mod, ?Mod:locals())],
				 Funcalls <- [?Query:exec(Fun, ?Fun:funcalls())],
				 Funcalls /= []]).

get_func_mod(Fun)->
    ?Query:exec1(Fun, ?Fun:module(), mod_not_found).


build_graph(Graph)->
    build_vertex(Graph, get_modcalls(get_all_modules())).

build_vertex(_, [])-> ok;
build_vertex(Graph, [{Mod, ModCalls} | Tail])->
    check_vertex(Graph, Mod),
    [begin
	 check_vertex(Graph, Called),
	 check_edge(Graph, Mod, Called)
     end || Called <- ModCalls],
    build_vertex(Graph, Tail).

    

check_vertex(Graph, Vertex)->
     case  digraph:vertex(Graph, Vertex) of
			      false -> digraph:add_vertex(Graph, Vertex);
			      _ -> ok
     end.

check_edge(Graph, Vertex1, Vertex2)->
    case edge(Graph, Vertex1, Vertex2) of
	[] ->
	     digraph:add_edge(Graph, Vertex1, Vertex2);
	_ -> ok
    end.
    
edge(Graph, Vertex1, Vertex2)->
    [digraph:edge(Graph, Edge)
     || Edge <- ?MISC:intersect(digraph:out_edges(Graph, Vertex1),
				digraph:in_edges(Graph, Vertex2))].


%%---------------------------------------------------------------------------------------------------------------
%% @spec check_cycle() -> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Builds a directed graph, checks for cycles in module level. If cycles are found then the following tuple is the result: {Number of
%% cycles, Cycle list}.
check_cycle()->
    Graph = digraph:new(),
    try
	build_graph(Graph),
	check_cycle(Graph, digraph:vertices(Graph))
    after
	digraph:delete(Graph)
    end.

cycles(_, [], Cycles)-> Cycles;
cycles(Graph, [Head|Tail], Cycles)->
    case digraph:get_cycle(Graph, Head)  of
	false ->
	    cycles(Graph, Tail, Cycles);
	NewCycle -> NewTail = [Vertex || Vertex<- Tail, not lists:member(Vertex, NewCycle)],
	     cycles(Graph, NewTail, [NewCycle | Cycles])
    end.

check_cycle(Graph, Vertices)->
    Cycles = cycles(Graph,Vertices, []),
    case Cycles of 
	[]->
	     {ok, no_cyclic_dependency};
	_ -> {integer_to_list(length(Cycles)) ++ " cycle(s)", Cycles}
    end.


%% @spec print_cycle() -> ok
%% @doc Prints the result of check_cycle/0 to the standard output.
print_cycle() ->
    case check_cycle() of
        {Int, Nodes} when is_list(Nodes)->
            ListText = [[atom_to_list(?Mod:name(Mod)) || Mod <- Cycle]
			|| Cycle <- Nodes],
            io:format("~p~n~p~n", [Int, ListText]);
        Other -> io:format("~p~n", [Other])
    end. 

%%------------------------------------------------------------------------------------------------------------
%% @spec check_module(Module)-> true | false | {error, bad_module_name} | erlang:error(error, Args)
%% Module = node() | string()
%% @doc Creates a subgraph from the given module as a starting node and checks for cycles.
%% <br/>The module can be given as a graph node or as its name.
check_module({_, module, _} = Node)->
    ?Graph:data(Node),
    check_mod_graph([Node]);

check_module(Mod)->
    case get_node(Mod) of 
	[] ->
	    {error, bad_module_name2};
	Node-> 
	    check_mod_graph(Node)
    end.

check_mod_graph(Node)->
    Graph = digraph:new(),
    try
	build_subgraph(Graph, Node),
	case  check_cycle(Graph, digraph:vertices(Graph)) of
	    {ok, no_cyclic_dependency}-> false;
	    _-> true
	end
    after
	digraph:delete(Graph)
    end.


build_subgraph(Graph, Node)->
    case get_modcalls(Node) of
	[] -> ok;
	[{Mod, CalledList}]->  
	    case lists:subtract(CalledList, digraph:vertices(Graph)) of
		[] ->  build_vertex(Graph, [{Mod, CalledList}]);
		NewVertices -> build_vertex(Graph, [{Mod, CalledList}]),
			       [build_subgraph(Graph, [Called])|| Called<-NewVertices]
	    end
    end.




get_node(Name)->
    ?Query:exec(?Mod:find(Name)). 
	

%%-----------------------------------------------------------------------------------------------------------

%% @spec draw()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Equivalent to draw(""), prints out the entire graph. Output file is dep_mod.dot.
draw()-> draw("").

%% @spec draw(Module)-> {ok, no_cyclic_dependency} | {integer(), list()} |  {error, bad_function_name} | erlang:error(error, Args)
%% Module = node() | string()
%% @doc Creates a subgraph drawing from the directed graph from a given module as a starting node.
%% <br/>The module can be given as a graph node or as its name.
%% The file's name: dep_<i>module</i>.dot.
draw(Mod)-> 
    Graph = digraph:new(),
    try
	case Mod of
	    "" -> 
		build_graph(Graph), 
		check_graph(Graph, "dep_mod.dot");
	    "cycle" -> 
		cycle_graph(Graph);
	    {_, module, _} = Node ->
		?Graph:data(Node),
		File = make_file_name(?Mod:name(Mod)),
		fun_graph(Graph, [Node], File);    
	    _ -> 
		case get_node(Mod) of
		     []->
			 {error, bad_module_name};
		     Node ->
			 fun_graph(Graph, Node, make_file_name(Mod))  
		 end
	end
    after
        digraph:delete(Graph)
    end.

%% @spec draw_cycles()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Equivalent to draw("cycle"), prints out a subgraph which contains the cycle(s).
%% Unless cycles exist, calls {@link draw/0}.
%% Output file is dep_mod_cycles.dot.
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
    Cycles = check_cycle(Graph, digraph:vertices(Graph)),
    case Cycles of
	{_, no_cyclic_dependency}->
	     draw();
	{_, Vertices} ->  
	    Subgraph = digraph_utils:subgraph(Graph,lists:flatten(Vertices)),
	    try
		write_file(Subgraph, Cycles, "dep_mod_cycles.dot")
	    after
		digraph:delete(Subgraph)
	    end
    end,
    Cycles.

make_file_name(Name)->
    "dep_mod_" ++ atom_to_list(Name) ++ ".dot".


-define(Dot_Header, "digraph dependency_graph_module_level {\n" ).
-define(Dot_footer,"}\n").
-define(Dot_vertex, "Nmodule~b [shape=\"hexagon\", label=\"~s\", fontsize=\"18\",  URL=\"#ok\", tooltip=\"node={'$gn', module, ~b}\"]\n").
-define(Dot_edge, "Nmodule~b -> Nmodule~b [color=\"black\" ]\n").
-define(Dot_cyclic_edge, "Nmodule~b -> Nmodule~b [color=\"red\" ]\n").

write_file(Graph, Result, File)->
    case file:open(File, [write]) of
	{ok, Dev}->
	    try
	    	io:put_chars(Dev, ?Dot_Header),

	    	[draw_vertex(Vertex, Dev)
	    	 || VertexList <-digraph_utils:components(Graph),
	    	    Vertex<-VertexList], 

	    	case Result of
	    	    {_, no_cyclic_dependency} -> ok;
	    	    {_, List} -> 
			[begin
				if length(Edge) == 1 -> draw_loop_edge(Graph, hd(Edge), Dev);
				   true -> draw_cyclic_edge(Graph, Edge, Dev)
				end
			end || Edge <- List]
	    	end,

	    	[draw_edge(Edge, Dev) || Edge<-list_edges(Graph)],    
	    	io:put_chars(Dev,?Dot_footer)
	    after
	    	file:close(Dev)
	    end;
	    
        {error, Reason}->
			 io:format("dep_mod.dot: ~s~n", file:format_error(Reason))
    end.

draw_vertex(V, Dev)->
    {_,_, M} = V,

    io:format(Dev, ?Dot_vertex , [M, ?Mod:name(V), M]).

draw_edge(Edge, Dev)->
    {_, {_, _, V1}, {_, _, V2}, _} = Edge,
    
    io:format(Dev, ?Dot_edge , [V1, V2]).


draw_cyclic_edge(_, [_ | []], _) -> ok;
draw_cyclic_edge(Graph, Edges, Dev)->
    Edge1 = hd(Edges),
    Edge2 = hd(tl(Edges)),
    case edge(Graph, Edge1, Edge2) of
        [] ->  draw_cyclic_edge(Graph, tl(Edges), Dev);
	_->
	    io:format(Dev, ?Dot_cyclic_edge, [element(3, Edge1), element(3, Edge2)]),
	    digraph:del_edge(Graph, element(1,hd(edge(Graph, Edge1, Edge2)))),
	    draw_cyclic_edge(Graph, tl(Edges), Dev)
    end.

draw_loop_edge(Graph, Node, Dev)->
	case edge(Graph, Node, Node) of
       		 	[] ->  ok;
        		_->
			    io:format(Dev, ?Dot_cyclic_edge, [element(3, Node), element(3, Node)]),
			    digraph:del_edge(Graph, element(1, hd(edge(Graph, Node, Node))))
	end.


list_edges(Graph)->
    [digraph:edge(Graph, Edge) || Edge<- digraph:edges(Graph)].


    

    
