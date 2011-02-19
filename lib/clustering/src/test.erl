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

%%% @doc Tester module, which helps other modules test themselves.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(test).
-vsn("$Rev: 1246 $").

-export([test_all/0,test/1,test_module/1,sort/2]).
-export([test/0]).

-include("cluster.hrl").

test_all() ->
    test([test, test_cl_attr, test_cl_cutlib, test_cl_distfun, test_cl_utils,
          test_cl_out]).

test(Modules) ->
    lists:foldl(
      fun(Module,ok) ->
              case test_module(Module) of
                  ok -> 
                      ok;
                  {test_failed,Reason} ->
                      {test_failed,Module,Reason}
              end;
         (_,Fail) ->
              Fail
      end,
      ok,
      Modules).

%% @spec test_module(mod_name()) -> test_ok | {test_failed,term()}
%%
%% @doc Tests the given module, which has to be given as a global name 
%% (e.g. not 'test' but 'cluster.test').
test_module(Module)->
    try apply(Module,test,[]) of
        ok -> ok
    catch
        _:Reason ->
            {test_failed,Reason}
    end.

%% @spec sort(Term::term(),Format::term()) -> term()
%%
%% @doc Sorts `Term' according to `Format'.
%% It is very useful when testing.
sort(X,leave) ->
    X;
sort(L,[leave,Todo]) when is_list(L) ->
    [sort(E,Todo) || E <- L];
sort(L,sort) when is_list(L) ->
    sort(L,[sort,leave]);
sort(L,[sort,Todo]) when is_list(L) ->
    lists:sort(sort(L,[leave,Todo]));
sort(T,Todo) when is_tuple(T), is_tuple(Todo) ->
    TList = tuple_to_list(T),
    TodoList = tuple_to_list(Todo),
    {[],SortedTListRev} =
        lists:foldl(
          fun(E,{[ETodo|TodoTail],Acc}) ->
                  {TodoTail,[sort(E,ETodo)|Acc]}
          end,
          {TodoList,[]},
          TList),
    list_to_tuple(lists:reverse(SortedTListRev)).


%% @spec test() -> ok
%%
%% @doc It tests the modul and returns ok if the test passed.
test() ->
    ok = test_sort().

test_sort() ->
    1 = sort(1,leave),

    %% lists
    [1,3,2] = sort([1,3,2],leave),
    [1,2,3] = sort([1,3,2],[sort,leave]),
    [1,2,3] = sort([1,3,2],sort),
    [[1,3,2],[5,4,6],[7,9,8]] =
        sort([[1,3,2],[7,9,8],[5,4,6]],[sort,leave]),
    [[1,3,2],[5,4,6],[7,9,8]] =
        sort([[1,3,2],[7,9,8],[5,4,6]],sort),
    [[1,2,3],[7,8,9],[4,5,6]] =
        sort([[1,3,2],[7,9,8],[5,4,6]],[leave,sort]),
    [[1,2,3],[4,5,6],[7,8,9]] =
        sort([[1,3,2],[7,9,8],[5,4,6]],[sort,sort]),

    %% tuples
    {1,2} = sort({1,2},{leave,leave}),
    {[3,4],[2,1]} = sort({[4,3],[2,1]},{sort,leave}),
    {[4,3],[1,2]} = sort({[4,3],[2,1]},{leave,sort}),
    {[3,4],[1,2]} = sort({[4,3],[2,1]},{sort,sort}),

    %% tuples and lists
    {{[3,4],[1,4,3],[1,3,4]},[1,2],[2,1]} =
        sort({{[4,3],[1,4,3],[1,4,3]},[2,1],[2,1]},
             {{sort,leave,sort},sort,leave}),

    {{[{[1,2],"aba"},{[1,2,4],"aba"},{[1,3],"aba"}]}} =
        sort({{[{[1,2],"aba"},{[3,1],"aba"},{[4,1,2],"aba"}]}},
             {{[sort,{sort,leave}]}}),
    {{[{[1,2],"aba"},{[1,3],"aba"},{[1,2,4],"aba"}]}} =
        sort({{[{[1,2],"aba"},{[3,1],"aba"},{[4,1,2],"aba"}]}},
             {{[leave,{sort,leave}]}}),
    {{[{[1,2],"aba"},{[3,1],"aba"},{[4,1,2],"aba"}]}} =
        sort({{[{[1,2],"aba"},{[4,1,2],"aba"},{[3,1],"aba"}]}},
             {{[sort,{leave,leave}]}}),
    {{[{[1,2],"aba"},{[3,1],"aba"},{[4,1,2],"aba"}]}} =
        sort({{[{[1,2],"aba"},{[3,1],"aba"},{[4,1,2],"aba"}]}},
             {{[leave,{leave,leave}]}}),

    ok.
