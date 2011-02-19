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

%%% @doc Generic graph query interface. A graph query is the generalization of
%%% a path (see {@link referl_graph:path()} and {@link referl_graph:path/2}).
%%% Paths are special cases of queries, and they are executed with the same
%%% efficiency when used in queries.
%%%
%%% A query always starts from a graph node, and returns a list of nodes.
%%% Generic queries are implemented as functions that have this interface. As
%%% a conveniency, queries can be executed starting from a list of nodes, this
%%% is equivalent of executing it on every node and concatenating the results.
%%%
%%% Queries have an associated "starting" and "ending" node class. A query can
%%% only be executed starting from nodes of its starting lcass, and the result
%%% contains nodes of the ending class. This restriction is inherited from
%%% graph paths, and is retained to enable query combinations.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_query).
-vsn("$Rev: 3572 $").

-export([exec/1, exec/2, exec1/2, exec1/3]).
-export([seq/1, seq/2, all/1, all/2, any/1, any/2, unique/1]).

%% @type query(Start, End)

%% query(Start, End) = referl_graph:path() |
%%                     fun(node(Start)) -> [node(End)] |
%%                     tuple()

-include("refactorerl.hrl").

%% @spec exec(query(root(), End)) -> [node(End)]
%% @doc Equivalent to `exec(?Graph:root(), Query)'.
exec(Query) -> exec([?ESG:root()], Query).

%% @spec exec1(query(root(), End), Error) -> node(End)
%% @doc Equivalent to `exec1(?Graph:root(), Query, Error)'.
exec1(Query, Error) ->
    exec1([?ESG:root()], Query, Error).

%% @spec exec1(node()|[node(Start)], query(Start, End), Error) -> node(End)
%% @doc Calls `exec(Start, Query)', and returns the singleton result. If the
%% result has zero or more than one elements, throws the excepion `Error'.
exec1(Start, Query, Error) ->
    case exec(Start, Query) of
        [Result] -> Result;
        _ -> throw(Error)
    end.

%% @spec exec(node(start)|[node(Start)], Query::query(Start, End)) ->
%%                                                                [node(End)]
%% @doc Executes `Query' starting from the nodes in `Start', and returns the
%% result of the query.
exec(Start, []) -> Start;
exec(Start, Query) when is_list(Start) ->
    lists:flatmap(fun(N) -> exec(N, Query) end, Start);

exec(Start, {seq, Q1, Q2}) ->
    exec(exec(Start, Q1), Q2);

exec(Start, {all, Q1, Q2}) ->
    exec(Start, Q1) ++ exec(Start, Q2);

exec(Start, {any, Q1, Q2}) ->
    case exec(Start, Q1) of
        []     -> exec(Start, Q2);
        Result -> Result
    end;

exec(Start, {unique, Q}) ->
    lists:usort(exec(Start, Q));

exec(Start, Query) when is_function(Query) ->
    Query(Start);

exec(Start, Path) ->
    ?ESG:path(Start, Path).

%% @spec seq(F::query(Start, Mid), S::query(Mid, End)) -> query(Start, End)
%% @doc Equivalent to `seq([F, S])'.
%% @see seq/1
seq(Q, []) -> Q;
seq([], Q) -> Q;
seq(P1, P2) when is_list(P1), is_list(P2) ->
    P1 ++ P2;
seq(P1, {seq, P2, Q3}) when is_list(P1), is_list(P2) ->
    {seq, P1 ++ P2, Q3};
seq({seq, Q1, Q2}, Q3) ->
    {seq, Q1, seq(Q2, Q3)};
seq(Q1, Q2) -> {seq, Q1, Q2}.

%% @spec seq([query()]) -> query()
%% @doc The result query is the sequence of the parameter queries: the second
%% query is executed on the result of the first, the third query on the result
%% of the second, and so on.
seq(Lst) -> lists:foldr(fun seq/2, [], Lst).

%% @spec all(F::query(Start, End), S::query(Start, End)) -> query(Start, End)
%% @doc Equivalent to `all([F, S])'.
%% @see all/1
all(Q, []) -> Q;
all([], Q) -> Q;
all(Q1, Q2) when is_list(Q1) andalso is_list(Q2) -> [[Q1, Q2]];
all(Q1, Q2) -> {all, Q1, Q2}.

%% @spec all([query()]) -> query()
%% @doc The result query returns results of all parameter queries (in the same
%% order as the queries appear in the parameter list).
all(Lst) -> lists:foldr(fun all/2, [], Lst).

%% @spec any(F::query(Start, End), S::query(Start, End)) -> query(Start, End)
%% @doc Equivalent to `any([F, S])'.
%% @see any/1
any([], Q) -> Q;
any(Q, []) -> Q;
any(Q1, Q2) -> {any, Q1, Q2}.

%% @spec any([query()]) -> query()
%% @doc The result query returns the same result as the first parameter query
%% that has any results. Queries are executed lazily, i.e. the second query is
%% executed only when the first query has no results, and so on.
any(Lst) -> lists:foldr(fun any/2, [], Lst).

%% @spec unique(query()) -> query()
%% @doc The result query returns the parameter query with unique
%% filter.
unique(Q) when is_list(Q) -> [{Q, unique}];
unique(Q) -> {unique, Q}.
