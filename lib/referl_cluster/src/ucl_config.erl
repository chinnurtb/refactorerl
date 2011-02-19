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
%%% Module Informations

%%% @doc Library to manage the clustering configuration.
%%% A {@type clConfig} record contains the clustering algorithms, the state of 
%%% the clustering process and book the already executed parts of the algorithm
%%% and the future parts. This module contain usefull function which make easy
%%% to create or modify the configuration or query informatins about it.

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_config).
-vsn("$Rev: $").

-include("ucluster.hrl").



%%% ============================================================================
%%% Imports/exports

% Clustering configuration
-export([new/3, delete/1, pack/1, unpack/1, save/2, load/1]).
% Modify configuration
-export([set_algs/2, update_algs/1, append_algs/2, drop_algs/3,
         add_steps/4, del_steps/3]).



%%% ============================================================================
%%% Clustering configuration

%% @spec new(InheritAlgs::[AlgName], Steps::[StepDesc], State::State) ->
%%           clConfig()
%%       AlgName = atom()
%%       StepDesc = {FunctionName::atom(), 
%%                   Options::[{Key::atom(), Value::term()}]}
%%       State = clState() | undefined
%% @doc Create a new clustering configuration. `InheritAlgs' list contains
%%      algorithm inheritance chain started with the most specific. `Steps' 
%%      list contains the algothm steps of the clustering algorithm started
%%      by the first. `State' is the initial state of the clustering process.
new(InheritAlgs, Steps, State) when is_list(InheritAlgs), 
        is_list(Steps), ?UNDEF==State orelse is_record(State, clState) ->
    update_algs(#clConfig{algs=InheritAlgs, steps=Steps, donesteps=[],
                          state=State}).

%% @spec delete(Config::clConfig()) -> ok
%% @doc  Delete the `Config' clustering configuration.
delete(#clConfig{state=St}) ->
    state_delete(St).


%% @spec pack(Config::clConfig()) -> PackedConfig::clConfig()
%% @doc Create a new `clConfig' object which contains all information from 
%%      the original `Config' as values.
%%      Don't use the returned `PackedConfig' object as the normal
%%      `clConfig' objects! In this case a run time error may occur.
%%      If you have a packed `clConfig' object you need to unpack with the
%%      {@link unpack/1} function before use it.
%% @see unpack/1
pack(Cfg=#clConfig{state=St}) ->
    Cfg#clConfig{state=state_pack(St)}.

%% @spec unpack(PackedConfig::clConfig()) -> Config::clConfig()
%% @doc Create a normal `clConfig' object from the given packed
%%      `PackedConfig' objecjt.
%% @see pack/1
unpack(Cfg=#clConfig{state=St}) ->
    Cfg#clConfig{state=state_unpack(St)}.


%% @spec save(Config::clConfig(), FilePath::list()) -> ok
%% @doc Pack the `Config' object and save that into the `FilePath' file.
save(Cfg=#clConfig{}, FilePath) when is_list(FilePath) ->
    ?ClCommon:save("~p.\n", [pack(Cfg)], FilePath).

%% @spec load(FilePath::list()) -> Config::clConfig()
%% @doc Load a `clConfig' object from the `FilePath' file and unpack that.
load(FilePath) ->
    update_algs(unpack(?ClCommon:load_test_rec(FilePath,clConfig))).


% Delete clustering state
state_delete(St=#clState{}) -> ?ClState:delete(St);
state_delete(?UNDEF)        -> ok.

% Pack clustering state
state_pack(St=#clState{}) -> ?ClState:pack(St);
state_pack(?UNDEF)        -> ?UNDEF.

% Unpack clustering state
state_unpack(St=#clState{}) -> ?ClState:unpack(St);
state_unpack(?UNDEF)        -> ?UNDEF.



%%% ============================================================================
%%% Modify configuration

%% @spec set_algs(Algs::[AlgName], Cfg::clConfig()) -> clConfig()
%%       AlgName = atom()
%% @doc Set the algorith inheritance chain.
%%      The first in `Algs' is the most specific algorithm and the last one
%%      is the most general algorithm.
%%      If the last algorithm isn't the base {@link ucl_alg} algorithm it will
%%      be concated to the end.
set_algs(Algs, Cfg=#clConfig{}) when is_list(Algs) -> 
    update_algs(Cfg#clConfig{algs=Algs}).

%% @spec update_algs(Config::clConfig()) -> clConfig()
%% @doc Add base {@link ucl_alg} algorithm to the end of inheritance chain of
%%      algorithms.
update_algs(Cfg=#clConfig{algs=Algs}) ->
    if  
        []/=Algs ->
            case lists:last(Algs) of
                ?ClAlg -> Cfg;
                _      -> Cfg#clConfig{algs=Algs++[?ClAlg]}
            end;
        true -> Cfg#clConfig{algs=[?ClAlg]}
    end.


%% @spec append_algs(Algs::[AlgName], Cfg::clConfig()) -> clConfig()
%%       AlgName = atom()
%% @doc Append algorithms to the begin of algorithm inheritance chain.
%%      The first in `Algs' is the most specific algorithm and the last one
%%      is the most general algorithm.
%%      If the last algorithm isn't the base {@link ucl_alg} algorithm it will
%%      be concated to the end.
append_algs(NewAlgs, Cfg=#clConfig{algs=Algs}) when is_list(NewAlgs) ->
    update_algs(Cfg#clConfig{algs=NewAlgs++Algs}).
    

%% @spec drop_algs(Alg::AlgName, DropIt::bool(), Cfg::clConfig()) -> clConfig()
%%       AlgName = atom() | {last}
%% @throws missing_algorithm
%% @doc Drop all algorithm from the most specific to `Alg'. If `DropIt' is true
%%      `Alg' will be dropped too.
%%      If there is not `Alg' in the actual algorithm inheritance chain an
%%      `missing_algorithm' exception will be thrown.
drop_algs({last}, DropIt, Cfg=#clConfig{algs=Algs}) ->
    drop_algs(lists:last(Algs), DropIt, Cfg);
drop_algs(Alg, DropIt, Cfg=#clConfig{algs=Algs}) when is_boolean(DropIt) ->
    Algs1 = case lists:dropwhile(fun(A) -> A/=Alg end, Algs) of 
        [] -> throw(missing_algorithm);
        [Alg|TailAlgs] ->
            if
                DropIt -> TailAlgs;
                true   -> [Alg|TailAlgs]
            end
    end,
    update_algs(Cfg#clConfig{algs=Algs1}).
    

%% @spec add_steps(NewSteps::[StepDesc], DstStep::StepName, AddMode::Mode,
%%               Cfg::clConfig()) -> clConfig()
%%       StepDesc = {StepName, StepOptions::proplist()}
%%       StepName = atom() | {first} | {last}
%%       Mode = before | after
%% @throws missing_step
%% @doc Add `NewSteps' clustering steps before/after `DstStep' step.
%%      If the actual list of steps is empty `DstStep' and `Mode' is ignored.
%%      If there is not `DstStep' in the actual steps sequence an
%%      `missing_step' exception will be thrown.
add_steps(NewSteps, _DstStep, _Mode, Cfg=#clConfig{steps=[]}) ->
    Cfg#clConfig{steps=NewSteps};
add_steps(NewSteps, {first}, Mode, Cfg=#clConfig{steps=Steps}) ->
    add_steps(NewSteps, hd(Steps), Mode, Cfg);
add_steps(NewSteps, {last}, Mode, Cfg=#clConfig{steps=Steps}) ->
    add_steps(NewSteps, lists:last(Steps), Mode, Cfg);
add_steps(NewSteps, DstStep, Mode, Cfg=#clConfig{steps=Steps}) when 
        is_list(Steps), is_atom(DstStep), 
        ('before'==Mode orelse 'after'==Mode) ->
    % Find DstStep and insert NewSteps
    {RevBeginSteps,Cnt,Found} = ?MISC:partfold(
        fun(Step={SN,_SO}, {AccRevSteps,AccCnt,_AccFound}) when SN==DstStep ->
            case Mode of
                'before' ->
                    {stop, {[Step|NewSteps++AccRevSteps], AccCnt+1, true}};
                'after'  ->
                    {stop, {NewSteps++[Step|AccRevSteps], AccCnt+1, true}}
            end;
        (Step, {AccRevSteps,AccCnt,AccFound}) -> 
            {next, {[Step|AccRevSteps], AccCnt+1, AccFound}}
        end,
        {[], 0, false},
        Steps),
    % If there is not DstStep in the steps list
    if
        Found -> ok;
        true  -> throw(missing_step)
    end,
    % Update steps list
    Steps1 = lists:reverse(RevBeginSteps) ++ lists:nthtail(Cnt,Steps),
    Cfg#clConfig{steps=Steps1}.


%% @spec del_steps(FromStep::StepName, ToStep::StepName, Cfg::clConfig()) ->
%%               clConfig()
%%       StepName = atom() | {first} | {last}
%% @throws missing_step
%% @doc Delete all steps from `FromStep' to `ToStep' from the actual steps list.
%%      If there is not `FromStep' or `ToStep' in the actual steps sequence an
%%      `missing_step' exception will be thrown.
del_steps({first}, ToStep, Cfg=#clConfig{steps=Steps}) ->
    del_steps(hd(Steps), ToStep, Cfg);
del_steps(FromStep, {last}, Cfg=#clConfig{steps=Steps}) ->
    del_steps(FromStep, lists:last(Steps), Cfg);
del_steps(FromStep, ToStep, Cfg=#clConfig{steps=Steps}) when is_atom(FromStep),
        is_atom(ToStep) ->
    % Find FromStep and ToSteps and erase steps between theses
    {RevBeginSteps,Cnt,Found} = ?MISC:partfold(
        fun({SN,_SO}, {AccRevSteps,AccCnt,0}) when SN==FromStep ->
            {next, {AccRevSteps,AccCnt+1,1}};
        ({SN,_SO}, {AccRevSteps,AccCnt,1}) when SN==ToStep ->
            {stop, {AccRevSteps,AccCnt+1,2}};
        (Step, {AccRevSteps,AccCnt,0}) -> 
            {next, {[Step|AccRevSteps], AccCnt+1, 0}};
        (_Step, {AccRevSteps,AccCnt,1}) -> 
            {next, {AccRevSteps, AccCnt+1, 0}}
        end,
        {[], 0, 0},
        Steps),
    % If there is not FromStep or ToStep in the steps list
    if
        2==Found -> ok;
        true     -> throw(missing_step)
    end,
    % Update steps list
    Steps1 = lists:reverse(RevBeginSteps) ++ lists:nthtail(Cnt,Steps),
    Cfg#clConfig{steps=Steps1}.



