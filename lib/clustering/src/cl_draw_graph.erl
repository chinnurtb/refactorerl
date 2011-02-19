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

%%% @doc Print the results of module clustering into dot files.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_draw_graph).
-vsn("$Rev: 1247 $").

-export([draw_clusters/2]).

-include("cluster.hrl").

draw_clusters(Iterations, Dir) ->
    file:make_dir(Dir),
    lists:foldl(
      fun(Iteration, Acc) ->
              Filename = filename:join(
                           Dir,
                           lists:flatten(io_lib:format("~3..0b.dot", [Acc]))),
              %% ...
              case file:open(Filename, [write]) of
                  {ok, Dev} ->
                      io:format(Dev, "digraph calls { overlap=scale;~n", []),
                      [module_calls(Dev, M) ||
                          M <- ?GRAPH:path(?GRAPH:root(), [file, moddef])],
                      %% TODO: lists subgraphs
                      subgraphs(Dev, Iteration),
                      io:format(Dev, "}~n", []),
                      file:close(Dev);
                  {error, Reason} ->
                      io:format("Error: ~s~n", file:format_error(Reason))
              end,
              Acc + 1
      end,
      1,
      Iterations).

module_calls(Dev, Mod) ->
    #module{name=Name} = ?GRAPH:data(Mod),
    io:format(Dev, "~s [label=\"~s\"];~n", [Name, Name]),
    Calls = ?GRAPH:path(Mod, [func, {fundef,back}, funcl, {scope, back}, visib,
                              {sup, back}, funref, {func, back}]),
    %io:format("~s: ~b~n", [Name, length(Calls)]),
    print_calls(lists:sort(Calls), Name, Dev).

print_calls([{n, Node, N}, Node | Tail], From, Dev) ->
    print_calls([{n, Node, N+1} | Tail], From, Dev);
print_calls([{n, Node, N} | Tail], From, Dev) ->
    case ?GRAPH:path(Node, [{moddef,back}]) of
        [] -> ok;
        _ ->
            #module{name=To} = ?GRAPH:data(Node),
            if
                To /= From ->
                    io:format(Dev, "~s -> ~s [label=\"~b\"];~n", [From, To, N]);
                true ->
                    ok
            end
    end,
    print_calls(Tail, From, Dev);
print_calls([Node | Tail], From, Dev) ->
    print_calls([{n, Node, 1} | Tail], From, Dev);
print_calls([], _, _) -> ok.

subgraphs(Dev, Iteration) ->
    lists:foldl(fun(Cluster, Acc) -> 
                         io:format(Dev, "subgraph cluster_~w{~n", [Acc]),
                         print_list(Dev, Cluster),
                         io:format(Dev, "}~n", []),
                         Acc + 1
                 end, 1, Iteration).

print_list(Io, [Head | Tail]) ->
    io:format(Io, "~p;~n", [Head]),
    print_list(Io, Tail);
print_list(_, []) ->
    ok.
