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

%%% @doc Functionblock relationship examiner.
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_fb_relations).
-vsn("$Rev: 5746 $ ").
-export([get_relations/0, get_relations/1, is_relation/1, check_cycle/0, check_cycle/1]).
-export([draw/0, draw_cycles/0, draw/1]).

-include("user.hrl").

%% =============================================================================
%% Making proper input list

sort([])->
    case refusr_dir_sort:sort() of
        {error, _} -> throw(no_mod);
        List -> List
    end;
sort(List) when is_list(List)->
    sort_dir(List);
sort(_)->
    throw(badarg).

sort_dir([{_, _} | _]  = RelList) ->
    sort_dir(make_dir(RelList));
sort_dir(List) when is_list(List) ->
    case refusr_dir_sort:sort() of
        {error, _} -> throw(no_mod);
        DirSort -> [{Path, ModList} || {Path, ModList} <- DirSort,
                                       lists:member(Path, List)]
    end;
sort_dir(_) ->
    throw(badarg).


make_dir([])->
    [];
make_dir([{Base, FbList} | Rest])->
    [filename:join(Base, dir_to_list(Dir)) || Dir <- FbList] ++ make_dir(Rest).

dir_to_list(Dir) when is_integer(Dir) -> integer_to_list(Dir);
dir_to_list(Dir) when is_atom(Dir)    -> atom_to_list(Dir);
dir_to_list(_)                        -> throw(badarg).

%% =============================================================================
%% Building graph

build_graph(Graph, FbList)->
    [build_vertex(Graph, Element) || Element <- FbList].


build_vertex(Graph, {Path, ModList})->
    check_vertex(Graph, Path),
    [begin
    	 check_vertex(Graph, CalledPath),
	 if
	     Path == CalledPath -> ok;
	     true -> check_edge(Graph, Path, CalledPath)
	 end
     end || Mod <- ModList,
	    CalledPath <- get_called_dir(get_node(Mod))].


check_vertex(Graph, Vertex)->
    case  digraph:vertex(Graph, Vertex) of
        false -> digraph:add_vertex(Graph, Vertex);
        _ -> ok
    end.

check_edge(Graph, Vertex1, Vertex2)->
    case edge(Graph, Vertex1, Vertex2) of
	[] -> digraph:add_edge(Graph, Vertex1, Vertex2);
	_ -> ok
    end.

edge(Graph, Vertex1, Vertex2)->
    [digraph:edge(Graph, Edge)
     || Edge <- ?MISC:intersect(digraph:out_edges(Graph, Vertex1),
				digraph:in_edges(Graph, Vertex2))].


get_node(Name)->
    ?Query:exec(?Mod:find(Name)).

get_called_dir(Mod)->
    [Path || Called <- get_mod_funcall(Mod),
             (CalledMod = get_func_mod(Called)) /= Mod,
             {Path, _} <- refusr_dir_sort:sort([CalledMod])].

get_mod_funcall(Mod)->
    lists:flatten([Funcalls
                   || Fun <-[?Query:exec(Mod, ?Mod:locals())],
                      Funcalls <- [?Query:exec(Fun, ?Fun:funcalls())],
                      Funcalls /= []]).

get_func_mod(Fun)->
    ?Query:exec1(Fun, ?Fun:module(), mod_not_found).

graph(Graph, Arg)->
    FbList = sort(Arg),
    build_graph(Graph, FbList).


%% -----------------------------------------------------------------------------
%% @spec get_relations() -> [Relation_Pairs] | {error, bad_argument}
%% @doc Prints out every connection among every functionblock list.
%% Equivalent to get_relations([]), see {@link get_relations/1}.
get_relations()->
    get_relations([]).

%% -----------------------------------------------------------------------------
%% @spec get_relations(ParList) -> [Relation_Pairs] | {error, bad_argument}
%%           ParList = [string()]
%%                   | [{Basename::string(), [FunctionBlock::atom()]}]
%%           Relation_Pairs = tuple(Path)
%%           Path = string()
%%
%% @doc Displays the relationship among elements of the given
%% functionblock list. There is a relationship between `fb1' block and
%% `fb2' block if a module of `fb1' is dependent of a module of
%% `fb2'. The result is a tuple-list whose elements represent the
%% relations.
get_relations(ParList)->
    Graph = digraph:new(),
    try
	graph(Graph, ParList),
	[list_to_tuple(Edge) || Edge <- digraph_utils:components(Graph),
                                length(Edge) > 1]
    catch
        no_mod -> {error, no_modules_in_database};
        badarg -> {error, bad_argument}
    after
	digraph:delete(Graph)
    end.

%% -----------------------------------------------------------------------------
%% @spec is_relation(ParList)-> true | false | Error
%%           ParList = [string()]
%%                   | [{Basename::string(), [FunctionBlock::atom()]}]
%%           Error = {error, no_list_given}
%%                 | {error, bad_argument_list_length}
%%                 | {error, bad_argument_form}
%%
%% @doc Decides whether there is a connection between the two given
%% functionblocks.

is_relation([])->
    {error, no_list_given};
is_relation({_,_} = Arg)->
    is_relation(tuple_to_list(Arg));
is_relation(Arg) when not is_list(Arg) and not is_tuple(Arg)->
    {error, bad_argument_form};
is_relation(Arg) when length(Arg) == 2 ->
	case get_relations(Arg) of
		{error, Msg} -> {error, Msg};
		GetRel ->
   			lists:member(list_to_tuple(Arg), GetRel) or
			lists:member(list_to_tuple(lists:reverse(Arg)), GetRel)
   	 end;
is_relation(_) -> {error, bad_argument_list_length}.


%% -----------------------------------------------------------------------------
%% @spec check_cycle()-> {ok, no_cyclic_dependency}
%%                     | {integer(), list()}
%%                     | {error, bad_argument}
%%
%% @doc Looks for cycles among every function block list. Equivalent
%% to check_cycle([]). See {@link check_cycle/1}.
check_cycle()->
    check_cycle([]).

%% -----------------------------------------------------------------------------
%% @spec check_cycle(ParList) -> {ok, no_cyclic_dependency}
%%                             | {integer(), list()}
%%                             | {error, bad_argument}
%%           ParList = [string()]
%%                   | [{Basename::string(), [FunctionBlock::atom()]}]
%%
%% @doc Checks for cycles in the dependencies between the given
%% functionblock list.
check_cycle(ParList)->
    Graph = digraph:new(),
    try
	graph(Graph, ParList),
	check_cycle(Graph, digraph:vertices(Graph))
    catch
        no_mod -> {error, no_modules_in_database};
        badarg -> {error, bad_argument}
    after
	digraph:delete(Graph)
    end.

cycles(_, [], Cycles)-> Cycles;
cycles(Graph, [Head|Tail], Cycles)->
    case digraph:get_cycle(Graph, Head)  of
	false ->
	    cycles(Graph, Tail, Cycles);
	NewCycle ->
            NewTail = [Vertex || Vertex<- Tail, not lists:member(Vertex, NewCycle)],
            cycles(Graph, NewTail, [NewCycle | Cycles])
    end.

check_cycle(Graph, Vertices)->
    Cycles = cycles(Graph,Vertices, []),
    case Cycles of
	[]->
            {ok, no_cyclic_dependency};
	_ ->
            {integer_to_list(length(Cycles)) ++ " cycle(s)", Cycles}
    end.

%% =============================================================================
%% Draw graph

%% -----------------------------------------------------------------------------
%% @spec draw()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Prints out the entire graph. Output file is fb_relations.dot.
draw()-> draw([]).

%% -----------------------------------------------------------------------------
%% @spec draw(ParList) -> {ok, no_cyclic_dependency}
%%                      | {integer(), list()}
%%                      | {error, bad_function_name}
%%           ParList = [string()]
%%                   | [{Basename::string(), [FunctionBlock::atom()]}]
%%
%% @doc Creates a subgraph drawing from the given functionblock list.
%% The file's name: fb_relations.dot.
draw(ParList)->
    Graph = digraph:new(),
    try
	case ParList of
	    cycle ->
		cycle_graph(Graph, []);
	    _ -> normal_graph(Graph, ParList)
	end
    catch
        no_mod -> {error, no_modules_in_database};
        badarg -> {error, bad_argument}
    after
        digraph:delete(Graph)
    end.

normal_graph(Graph, Options) ->
    graph(Graph, Options),
    write_file(Graph,
	       check_cycle(Graph, digraph:vertices(Graph)),
	       "fb_relations.dot").

%% -----------------------------------------------------------------------------
%% @spec draw_cycles() -> {ok, no_cyclic_dependency}
%%                      | {integer(), list()}
%%                      | {error, bad_function_name}
%%
%% @doc Prints out a subgraph which contains the cycle(s). Unless
%% cycles exist, calls {@link draw/0}. Output file is
%% fb_rel_cycles.dot.
draw_cycles()->
    draw(cycle).

cycle_graph(Graph, Options)->
    graph(Graph, Options),
    Cycles = check_cycle(Graph, digraph:vertices(Graph)),
    case Cycles of
	{_, no_cyclic_dependency}->
            draw(all);
	{_, Vertices} ->
	    Subgraph = digraph_utils:subgraph(Graph,lists:merge(Vertices)),
	    try
		write_file(Subgraph, Cycles, "fb_rel_cycles.dot")
	    after
		digraph:delete(Subgraph)
	    end
    end,
    Cycles.

%% =============================================================================
%% Writing to the dot file

-define(Dot_Header, "digraph functionblock_relationships {\n" ).
-define(Dot_footer,"}\n").
-define(Dot_vertex, "Nfb~b [shape=\"hexagon\", label=\"~s\", fontsize=\"18\",  URL=\"#ok\", tooltip=\"~s\"]\n").
-define(Dot_edge, "Nfb~b -> Nfb~b [color=\"black\" ]\n").
-define(Dot_emph_edge, "Nfb~b -> Nfb~b [color=\"magenta\" ]\n").
-define(Dot_cyclic_edge, "Nfb~b -> Nfb~b [color=\"red\" ]\n").

write_file(Graph, Result, File)->
    case file:open(File, [write]) of
	{ok, Dev}->
	    try
	    	io:put_chars(Dev, ?Dot_Header),

		Sequent = lists:zip(
			    lists:seq(1, length(digraph:vertices(Graph))),
			    digraph:vertices(Graph)),

	    	[draw_vertex(Vertex, Dev, Sequent)
	    	 || Vertex <-digraph:vertices(Graph) ],

	    	case Result of
	    	    {_, no_cyclic_dependency} -> ok;
	    	    {_, List} ->
			[begin
                             if length(Edge) == 1 -> draw_loop_edge(Graph, hd(Edge), Dev, Sequent);
                                true -> draw_cyclic_edge(Graph, Edge, Dev, Sequent)
                             end
                         end || Edge <- List]
	    	end,

	    	[draw_edge(Edge, Dev, Sequent) || Edge<-list_edges(Graph)],
	    	io:put_chars(Dev,?Dot_footer)
	    after
	    	file:close(Dev)
	    end;

        {error, Reason}->
            io:format("dep_mod.dot: ~s~n", file:format_error(Reason))
    end.

draw_vertex(V, Dev, Sequent)->
    io:format(Dev, ?Dot_vertex , [get_seq(V, Sequent), filename:basename(V), V]).

get_seq(V, Sequent)->
    hd([Seq||{Seq, Vertex} <- Sequent, Vertex == V ]).

draw_edge(Edge, Dev, Sequent)->
    {_, V1, V2, _} = Edge,
    EdgeFmt = if V1 == "Other"; V2 == "Other" -> ?Dot_edge;
                 true                         -> ?Dot_emph_edge
              end,
    io:format(Dev, EdgeFmt , [get_seq(V1, Sequent), get_seq(V2, Sequent)]).


draw_cyclic_edge(_, [_ | []], _, _) -> ok;
draw_cyclic_edge(Graph, Edges, Dev, Sequent)->
    Edge1 = hd(Edges),
    Edge2 = hd(tl(Edges)),
    case edge(Graph, Edge1, Edge2) of
        [] ->  draw_cyclic_edge(Graph, tl(Edges), Dev, Sequent);
	_->
	    io:format(Dev, ?Dot_cyclic_edge,
		      [get_seq(Edge1, Sequent), get_seq(Edge2, Sequent)]),
	    digraph:del_edge(Graph, element(1,hd(edge(Graph, Edge1, Edge2)))),
	    draw_cyclic_edge(Graph, tl(Edges), Dev, Sequent)
    end.

draw_loop_edge(Graph, Node, Dev, Sequent)->
    case edge(Graph, Node, Node) of
	[] ->  ok;
	_->
	    io:format(Dev, ?Dot_cyclic_edge,
		      [get_seq(Node, Sequent), get_seq(Node, Sequent)]),
	    digraph:del_edge(Graph, element(1, hd(edge(Graph, Node, Node))))
    end.


list_edges(Graph)->
    [digraph:edge(Graph, Edge) || Edge<- digraph:edges(Graph)].
