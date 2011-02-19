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

%%% ============================================================================
%%% Module information

%%% @doc Module for initiating and controlling transformations.
%%% Given the name of the refactoring module and the actual parameters,
%%% it starts the transformation and mediates with the interface
%%% in case of a question. When there are no more preparatory steps
%%% to be taken, it executes the transformation with the collected
%%% parameters, then saves the changed files.
%%%
%%% Transformation steps may return an error, which is transferred to
%%% the interface as well.
%%%
%%% Refactorer modules have to provide the following functions.
%%% <ul>
%%%     <li>init/1:
%%%             This function expects the parameters of the refactoring
%%%             (usually a tuple), and returns the initial inner state of the
%%%             refactoring.
%%%     </li>
%%%     <li>steps/1:
%%%             A list of functions with one argument.
%%%     </li>
%%%     <li>transform/1:
%%%             This function receives the final state
%%%             collected previously, and performs the transformation.
%%%             It returns `{ChangedFiles, Result}'.
%%%     </li>
%%%     <li>functions listed in steps/1:
%%%             These functions have one argument.
%%%             The functions return one of the following:
%%%             <ul>
%%%                 <li>the transformed inner state of the refactoring
%%%                 </li>
%%%                 <li>`{question, Question, NewSt}':
%%%                     the step is restarted after the user provides an answer
%%%                 </li>
%%%                 <li>`ok': no change in the inner state (used in checks)
%%%                 </li>
%%%             </ul>
%%%             The functions may also throw a `string()';
%%%             this halts the transformation.
%%%             The functions are called by the transformation engine
%%%             in the following way:
%%%             <ul>
%%%                 <li>`{answer, Answer, State}':
%%%                     if the step is restarted after a question.
%%%                 </li>
%%%                 <li>`State':
%%%                     in the normal case.
%%%                 </li>
%%%             </ul>
%%%     </li>
%%% </ul>
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(referl_transform).
-vsn("$Rev: 1958 $").
-behaviour(gen_fsm).

-include("refactorerl.hrl").

-define(TIMEOUT, infinity).

%%% ============================================================================
%%% Exports

%% Client exports
-export([do/2, answer/1, cancel/0]).


%% Enviromental exports
-export([start_link/0]).

%% Transformation states
-export([start/3, question/2]).

%% gen_fsm exports
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%%% ============================================================================
%%% State information

%% module   : the module of the refactoring
%% steps    : remaining steps to be taken before the transformation
%%            the refactoring module's prepare() function provides it initially
%% refstate : the refactoring's inner state
-record(state, {    refmod    = no_module,
                    steps     = no_steps,
                    refst     = no_state
                   }).

%%% ============================================================================
%%% Server callbacks

%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server.
start_link() ->
    gen_fsm:start_link({local, ?TRANSFORM_SERVER}, ?MODULE, [], []).

%% @private
handle_sync_event(state, _From, SN, St) ->
    {reply, {SN, St}, SN, St}.

%% @private
init(_)                       -> {ok, start, #state{}}.
%% @private
handle_event(_Event, _SN, SD) -> {stop, async_not_supported, SD}.
%% @private
handle_info(_, SN, SD)        -> {next_state, SN, SD}.
%% @private
terminate(_,_,_)              -> ok.
%% @private
code_change(_, SN, SD, _)     -> {ok, SN, SD}.


%%% ----------------------------------------------------------------------------
%%% User interface callbacks


%% @doc Interface for doing the transformation.
do(RefMod, RefParams) ->
    gen_fsm:sync_send_event(?TRANSFORM_SERVER, {do, RefMod, RefParams},
                            ?TIMEOUT).


%% @spec answer(term()) -> ok
%% @doc Interface for sending an answer to the transformation.
answer(Answer) ->
    gen_fsm:sync_send_event(?TRANSFORM_SERVER, {answer, Answer}, ?TIMEOUT).


%% @spec cancel() -> ok
%% @doc Interface to cancel the transaction.
cancel() ->
    gen_fsm:sync_send_event(?TRANSFORM_SERVER, cancel, ?TIMEOUT).


%%% ============================================================================
%%% Transaction states

%% @private
start({do, RefMod, RefParams}, _From, #state{}) ->
    try RefMod:init(RefParams) of
        RefSt ->
            InitSt = #state{    refmod    = RefMod,
                                steps     = RefMod:steps(),
                                refst     = RefSt
                               },
            process_step(normal, InitSt)
    catch
        error:_ ->
                                                % TODO signal to UI as well?
            {reply, bad_param, start, #state{}}
    end;

start(_, _From, #state{}) ->
                                                % TODO signal to UI as well?
    {reply, bad_param, start, #state{}}.


%% @private
%% @spec question({atom(), [term()]}, #state{}) -> ok
%% @doc In case the step asked a question, this function handles the answer
%%      from the user interface. The user can also cancel the whole refactoring.
question({answer, Answer}, St = #state{}) ->
    process_step({answer, Answer}, St);
question(cancel, #state{}) ->
    {next_state, start, #state{}}.


%%% ----------------------------------------------------------------------------
%%% Transaction mechanism

%% @private
%% Process steps listed in RefMod:steps/0 are
%% functions with one parameter, one of the following.
%%  - check
%%      - may only return `ok', no state change
%%      - is encouraged to use ?MISC:error_on_difference/3
%%        for throwing a check failure
%%  - collection step
%%      - may throw, which aborts the transformation
%%      - may return an updated state
%%      - may return {question, Question, RefSt}
%%          - when the user supplies an answer,
%%            the same step function is called with {Answer, RefSt}
process_step(normal, #state{steps = [], refst = RefSt, refmod = RefMod}) ->
    {ChangedFiles, Result} = RefMod:transform(RefSt),
    [referl_fileman:save_file(F) || F <- ChangedFiles],
    [referl_ui:message(reload, "~s", [(?GRAPH:data(F))#file.path]) ||
        F <- ChangedFiles],
    ?UI:message(status, "Transformation completed."),
    {reply, {Result, ChangedFiles}, start, #state{}};
process_step(StepSt, St = #state{steps = [Step|Steps], refst = RefSt}) ->
    case StepSt of
        normal           -> Par = RefSt;
        {answer, Answer} -> Par = {answer, Answer, RefSt}
    end,
    try Step(Par) of
        {question, Question, NewRefSt} ->
            ?UI:message(question, Question),
            {reply, {question, Question}, question, St#state{refst = NewRefSt}};
        ok ->
            process_step(normal, St#state{steps = Steps});
        NewRefSt ->
            process_step(normal, St#state{steps = Steps, refst = NewRefSt})
    catch
        throw:Msg ->
            ?UI:message(status, "Error: ~s", [Msg]),
            {reply, {error, Msg}, start, #state{}};
        error:Msg ->
            ?UI:message(error, "~p", [Msg]),
            error_logger:error_msg("Transformation error in module ~s:~n"
                                   "  reason: ~p~n"
                                   "  stack:  ~p~n",
                                   [St#state.refmod, Msg,
                                    erlang:get_stacktrace()]),
            {reply, {error, Msg}, start, #state{}}
    end.
