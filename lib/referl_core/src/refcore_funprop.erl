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
%%% The Initial Developer of the  Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s  Lor�nd University are  Copyright 2007-2009,
%%% E�tv�s Lor�nd University. All Rights Reserved.

%%% @doc Function side effect analyser. Caches function side effect
%%% information for more efficient recalculation in case of change.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refcore_funprop).
-vsn("$Rev: 4770 $ ").
-behaviour(gen_server).

-export([update/2, remove/1]).

%% gen_server exports
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("core.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @spec update(gnode(), undefined | {integer(), [gnode()]}) -> ok
%% @doc Updates side effect information. `Fun'  is a `#func{}' node, `Info'
%% may be `undefined', which means the body of the function is not known (e.g.
%% got removed from the graph), or a tuple that contains the number of
%% expressions with possible side effects in the body of the function, and the
%% list of functions (`#func{}' nodes) called in the body of the function.
update(Fun, Info) ->
    gen_server:call(?FUNPROP_SERVER, {update, Fun, Info}).

%% @spec remove(gnode()) -> ok
%% @doc Removes a function node from the cache (must be called when a function
%% node is deleted).
remove(Fun) ->
    gen_server:call(?FUNPROP_SERVER, {remove, Fun}).


%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server.
start_link() ->
    gen_server:start_link({local, ?FUNPROP_SERVER}, ?MODULE, [], []).

%% @private
init(_) ->
    {ok, init_state()}.

%% @private
handle_call({update, Fun, Info}, _, State) ->
    {reply, ok, handle_update(Fun, Info, State)};
handle_call({remove, Fun}, _, State) ->
    {reply, ok, handle_remove(Fun, State)}.

%% @private
handle_cast(_, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_Old, State, _Extra) ->
    {ok, State}.


%%% ============================================================================
%%% Server implementation

init_state() ->
    ets:new(call_graph, []).

handle_remove(Fun, CG) ->
    ets:delete(CG, Fun),
    CG.


handle_update(Fun, undefined, CG) ->
    {Dirty, Refs, _} = read_info(Fun, CG),
    store_info(Fun, int, [], CG),
    if
        Dirty == no -> turn_dirty(Refs, CG);
        true -> ok
    end,
    CG;

handle_update(Fun, {Pure, Calls}, CG) ->
    {Dirty, Refs, _} = read_info(Fun, CG),
%%     % Looking for `int' in the closure instead of `int' or `ext'
%%     % in direct calls ensures that mutually recursive functions
%%     % are handled properly
%%     DirtyNeighbours = [F ||  F <- closure(Calls, fwd, CG),
%%                              dirty(F, CG) == int, F =/= Fun],
    {NewDirty, Update} =
        if 
            Pure -> 
                %% Looking for `int' in the closure instead of `int' or `ext' 
                %% in direct calls ensures that mutually recursive functions 
                %% are handled properly 
                case lists:any(fun (F) -> dirty(F, CG) == int end, 
                     closure(Calls, fwd, CG) -- [Fun]) of 
                    true when Dirty == no  -> {ext, fun turn_dirty/2}; 
                    true                   -> {ext, no}; 
                    false when Dirty == no -> {no,  no}; 
                    false                  -> {no,  fun turn_pure/2} 
                end; 
            not Pure and Dirty == no       -> {int, fun turn_dirty/2}; 
            not Pure                       -> {int, no} 
%%         case {Pure, DirtyNeighbours, Dirty} of
%%             {true,  [], no}  -> {ext, fun turn_dirty/2};
%%             {true,  [], _}   -> {ext, no};
%%             {true,  _,  no}  -> {no,  no};
%%             {true,  _, _}    -> {no,  fun turn_pure/2};
%%             {false, _, no}   -> {int, fun turn_dirty/2};
%%             {false, _, _}    -> {no,  fun turn_pure/2}
        end,
%io:format("UPDATE: ~p/~p->~p (upd: ~p)~n  refs: ~p~n  deps: ~p~n",
%          [Fun, Dirty, NewDirty, Update, Refs, Calls]),
    store_info(Fun, NewDirty, Calls, CG),
    is_function(Update) andalso
        [Update(F, CG) || F <- closure(Refs, back, CG)],
    CG.


dirty(Fun, CG) ->
    [{Fun, Dirty, _, _}] = ets:lookup(CG, Fun),
    Dirty.

store_info(Fun, Dirty, Calls, CG) ->
    {_, Refs, Deps} = read_info(Fun, CG),
    D = ?Graph:data(Fun),
    ?Graph:update(Fun, D#func{dirty=Dirty}),
    [add_call(Fun, Dep, CG) || Dep <- Calls -- Deps],
    [del_call(Fun, Dep, CG) || Dep <- Deps -- Calls],
    ets:insert(CG, {Fun, Dirty, Refs, Calls}).

add_call(From, To, CG) ->
    ?Graph:mklink(From, funcall, To),
    case ets:lookup(CG, To) of
        [] -> ok;
        [{To, Dirty, Refs, Deps}] ->
            ets:insert(CG, {To, Dirty, [From | Refs], Deps})
    end.

del_call(From, To, CG) ->
    ?Graph:rmlink(From, funcall, To),
    case ets:lookup(CG, To) of
        [] -> ok;
        [{To, Dirty, Refs, Deps}] ->
            ets:insert(CG, {To, Dirty, Refs -- [From], Deps})
    end.

read_info(Fun, CG) ->
    case ets:lookup(CG, Fun) of
        [{Fun, Dirty, Refs, Deps}] -> {Dirty, Refs, Deps};
        [] ->
            #func{dirty=Dirty} = ?Graph:data(Fun),
            Refs = ?Graph:path(Fun, [{funcall, back}]),
            Deps = ?Graph:path(Fun, [funcall]),
            ets:insert(CG, {Fun, Dirty, Refs, Deps}),
            {Dirty, Refs, Deps}
    end.

closure(Funs, Dir, CG) ->
    Tab = ets:new(closure, []),
    ets:insert(Tab, [{F} || F <- Funs]),
    closure(Funs, Tab, Dir, CG).

closure([], Tab, _, _) ->
    Result = ets:select(Tab, [{{'$1'},[],['$1']}]),
    ets:delete(Tab),
    Result;

closure([Fun | Rest], Tab, Dir, CG) ->
    Next =
        case {Dir, read_info(Fun, CG)} of
            {back, {_, Refs, _}} -> Refs;
            {fwd,  {_, _, Deps}} -> Deps
        end,
    New = [F || F <- Next, ets:insert_new(Tab, {F})],
    closure(New ++ Rest, Tab, Dir, CG).

turn_dirty(Fun, CG) ->
    {Dirty, _, Deps} = read_info(Fun, CG),
    if
        Dirty == no -> store_info(Fun, ext, Deps, CG);
        true -> ok
    end.

turn_pure(Fun, CG) ->
    {Dirty, _, Deps} = read_info(Fun, CG),
    case Dirty == int orelse
        lists:any(fun (F) -> dirty(F, CG) == int end,
                  closure(Deps, fwd, CG)) of
        true  -> ok;
        false -> store_info(Fun, no, Deps, CG)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Funpop test

update_test() ->
    M  = ?Graph:create(#func{name=m, arity=0}),
    ?FunProp:update(M, {true, []}),
    ?assertMatch(#func{dirty=no}, ?Graph:data(M)),

    S1 = ?Graph:create(#func{name=s, arity=1}),
    ?FunProp:update(S1, {true, [M]}),
    ?assertMatch(#func{dirty=no}, ?Graph:data(S1)),

    S2 = ?Graph:create(#func{name=s, arity=2}),
    ?FunProp:update(S2, {false, [M]}),
    ?assertMatch(#func{dirty=int}, ?Graph:data(S2)),

    T1 = ?Graph:create(#func{name=t, arity=1}),
    ?FunProp:update(T1, undefined),
    ?assertMatch(#func{dirty=int}, ?Graph:data(T1)),

    ?FunProp:update(M, {true, [T1]}),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(M)),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(S1)),

    ?FunProp:update(T1, {true, []}),
    ?assertMatch(#func{dirty=no}, ?Graph:data(T1)),
    ?assertMatch(#func{dirty=no}, ?Graph:data(M)),
    ?assertMatch(#func{dirty=no}, ?Graph:data(S1)),

    T2 = ?Graph:create(#func{name=t, arity=2}),
    ?FunProp:update(T2, {true, [S2]}),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(T2)),

    ?FunProp:update(M, {true, [T1, T2]}),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(M)),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(S1)),
    ?assertMatch(#func{dirty=int}, ?Graph:data(S2)),
    ?assertMatch(#func{dirty=no}, ?Graph:data(T1)).

