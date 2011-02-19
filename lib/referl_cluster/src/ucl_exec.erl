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

%%% @doc A generic server to execute the clustering algorihtms.
%%%
%%% The server can execute the complete clustering process from the loading of
%%% source files across the running of the clustering algorithm to the saving
%%% of the results. Every activities are implemented as callback functions
%%% which are placed into algorithm calback modules.
%%%
%%% == Callback modules ==
%%%
%%% An algorithm has some parts. All parts are implemented as functions. These
%%% functions are called algorithm steps. The callback modules contain
%%% algorithm step functions. Every step function has the following signature:
%%%
%%% ```
%%% Stepfunction = ((State::State_or_undef, Options::proplist()) ->
%%%                     {NewState::State_or_undef, AdditionalSteps::StepDesc})
%%% State        = undefined | clState()
%%% StepDesc     = {FunctionName::atom(), Options::proplist()}
%%% '''
%%%
%%% A step function give the actual state ({@type clState}) of the clustering
%%% process and an options list. The options are control the working of the
%%% step. After the working return the updateded state and the list of
%%% required postprocessor steps if necessary.
%%%
%%% Every step function must have a default and a validator function too. If 
%%% there are not default or validator function the
%%% {@link ucl_alg:empty_default/0} and {@link ucl_alg:empty_validator/0}
%%% functions will be used.
%%% The name of these functions are same as the step function expect the
%%% `_default' and `_validator' tails. For example: `do', `do_default',
%%% `do_validator'. The signatures of these functions are:
%%%
%%% ```
%%% DefaultFunction    = (() -> DefaultOptions::proplist())
%%% ValidatrotFunction = (() -> Validators::[{Key::atom(), ValFun::ValFun}])
%%% ValFun             = ((Object::term()) -> boolean())
%%% '''
%%%
%%% The default function return the default options of the step function. The
%%% validator function return a property list which contains validator
%%% functions for different option keys. Before the execution of a step the
%%% original option list is updated with the defaults and validated with the
%%% validator functions. If some options are not valids the step will not be
%%% executed and the server will stops.
%%%
%%% == Execution server ==
%%%
%%% The server execute a sequence of steps. An algorithm define steps which
%%% are implement the clustering algorithm activities. But a clustering
%%% process is more than just the clusering algorithm. Before the execution of
%%% the algorithm need to load the source files, retrieve the entities and
%%% attributes, build attribute matrix and distance/similirarity (connection)
%%% matrix. After all these can run an algorithm. It means an algorithm can't
%%% define the whole clustering process. So let introduce the inheritance of
%%% algorithms. The `algs' let be a list of algorithms. The last one is the
%%% most general and the first one is the most specialized algorithm. The
%%% inheritance of algorithms is same as the inheritance of classes. A derived
%%% algorithm don't need define all steps just redefine some step or defined
%%% some new step. The base algorithm is the {@link ucl_alg} callback module
%%% which contains some step to load source files, build matrices and save
%%% informaions out from the clustering process.
%%%
%%% The server has a `steps' list which define the steps needed to be executed.
%%% Every step must be implemented one of the algorithm modules in the
%%% inheritance chain. The search for a step function is started in the most
%%% specialized module. If there is no appropriate function the server will
%%% stop the execution and keep the last correct state.
%%%
%%% When an error occur under the execution the server keep the last correct
%%% state which was the actual state before the currently executed step. There
%%% are two options of the server for this case. If `OnErrorHalt' flag is
%%% `true' the server halt on error. Otherwise store the error, stop the
%%% execution and restore srever state to the before of the step which ocurred
%%% the error. The {@link get_error/0} function return the last occured error.
%%% If the `OnErrorPath' is not empty and the `OnErrorHalt' is `true' the server
%%% will save the last correct server state into a file specified in
%%% `OnErrorPath' before halt. The server state can be loaded again with the
%%% {@link load_state/1} function.
%%%
%%% The result of the clustering process is depending on the algorithm. An
%%% algorithm step must store the result. The server won't store anything at
%%% the end of the execution.
%%%
%%% == TODO ==
%%%
%%% <ul>
%%%   <li>Actually the execution of an algorithm is happened syncron way.
%%%     At same time only one clustering process can be runned on same server.
%%%     </li>
%%%   <li>The algorithms write out informations to the user to the standard
%%%     output. It need to improve to use {@link cl_out}.</li>
%%% </ul>

%%% @todo
%%% Actually the execution of an algorithm is happened syncron way.
%%%   At same time only one clustering process can be runned on same server.
%%% The algorithms write out informations to the user to the standard
%%%   output. It need to improve to use {@link cl_out}.

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_exec).
-vsn("$Rev: $").
-behaviour(gen_server).

-include("ucluster.hrl").



%%% ============================================================================
%%% Exports/imports

% Error messages
-export([error_text/2]).

% User interface functions
-export([save_state/1, load_state/1,
         get_error/0, get_onErrorHalt/0, set_onErrorHalt/1,
         get_onErrorSavePath/0, set_onErrorSavePath/1,
         get_config/0, set_config/1,
         exec_steps/1]).

% Server interface functions
-export([start_link/0, start/0, stop/0]).

% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Reserved callback functions
-export([reserved_steps/0, exec_save_validator/0, exec_save/2]).

% TEST ONLY
%-export([new/3, delete/1, pack/1, unpack/1, exec_steps/2]).



%%% ============================================================================
%%% Macros

% Create the name of the function which provide the default options for 
% `FunName' function
-define(FunOptsDefs(FunName), 
    list_to_atom(atom_to_list(FunName)++"_default")).

% Create the name of the function which provide the validator list for 
% `FunName' function
-define(FunOptsValid(FunName), 
    list_to_atom(atom_to_list(FunName)++"_validator")).


%%% ============================================================================
%%% Error messages

%% @private
%% @spec error_text(Type::atom(), Details::[term()]) -> string()
%% @doc Give back the error message text for the transformation specific errors.
%%      The error is specified by the `Type' and the `Details'.
error_text(missing_fun, {ModNames,FunName}) ->
    ?MISC:format("There is no ~p/2 function in the ~p modules.", 
                 [FunName,ModNames]);
error_text(missing_module, {ModName,Error}) ->
    ?MISC:format("Can't load the ~p module. Reason: ~p.", [ModName,Error]);
error_text(no_more_steps, []) ->
    ?MISC:format("There are no more steps to execute.", []);
error_text(invalid_options, {{ModName,FunName},Wrongs}) ->
    ?MISC:format("The ~p options of ~p:~p/2 function are invalid.",
                 [Wrongs,ModName,FunName]).



%%% ============================================================================
%%% Interface functions

%%% ----------------------------------------------------------------------------
%%% Server interface functions

%% @private
%% @spec start_link() -> Result
%%       Result = {ok,pid()} | {error,Error}
%%       Error  = {already_started,Pid} | term()
%% @doc Create a ucl_exec process as part of a supervision tree.
%% @see gen_server:start_link/4
start_link() ->
    gen_server:start_link({local, ?ClExec_server}, ?MODULE, [], []).


%% @private
%% @spec start() -> Result
%%       Result = {ok,pid()} | {error,Error}
%%       Error  = {already_started,Pid} | term()
%% @doc Create a standalone ucl_exec process.
%% @see gen_server:start/4
start() ->
    gen_server:start({local, ?ClExec_server}, ?MODULE, [], []).


%% @private
%% @spec stop() -> ok
%% @doc Stop the standalone ucl_exec process.
stop() ->
    gen_server:cast(?ClExec_server, stop).



%%% ----------------------------------------------------------------------------
%%% User interface functions

%% @spec save_state(FilePath::list()) -> ok
%% @doc  Save the actual state of the server into the `FilePath' file.
save_state(FilePath) when is_list(FilePath) ->
    gen_server:call(?ClExec_server, {save_state, FilePath}).

%% @spec load_state(FilePath::list()) -> ok | Error
%%       Error = term()
%% @doc  Load state of the server from the `FilePath' file.
load_state(FilePath) when is_list(FilePath) ->
    gen_server:call(?ClExec_server, {load_state, FilePath}).


%% @spec get_error() -> undefined | Error
%%       Error = term()
%% @doc Return the error that was occured when the steps had been processed at
%%      last time. If there was no error return the `undefined' atom.
get_error() ->
    gen_server:call(?ClExec_server, get_error).

%% @spec get_onErrorHalt() -> boolean()
%% @doc Get the `OnErrorHalt' flag value.
%%      If it is true and an error occur while execute steps the server will 
%%      terminate. Otherwise the server keep the state which was actual before
%%      execute the wrong step.
get_onErrorHalt() ->
    gen_server:call(?ClExec_server, get_onErrorHalt).

%% @spec set_onErrorHalt(OnErrorHalt::boolean()) -> ok
%% @doc Set the `OnErrorHalt' flag value.
%%      If it is true and an error occur while execute steps the server will 
%%      terminate. Otherwise the server keep the state which was actual before
%%      execute the wrong step.
set_onErrorHalt(OnErrorHalt) when is_boolean(OnErrorHalt) ->
    gen_server:call(?ClExec_server, {set_onErrorHalt, OnErrorHalt}).

%% @spec get_onErrorSavePath() -> FilePath::list()
%% @doc Get the `OnErrorSavePath' property value.
%%      If it isn't an empty string and an error occur while execute steps and 
%%      the `OnErrorHalt' flag is set the server will save the server state
%%      which was actual before execute the wrong step into a file.
get_onErrorSavePath() ->
    gen_server:call(?ClExec_server, get_onErrorPath).

%% @spec set_onErrorSavePath(OnErrorSavePath::string()) -> ok
%% @doc Set the `OnErrorSavePath' property value.
%%      If it isn't an empty string and an error occur while execute steps and 
%%      the `OnErrorHalt' flag is set the server will save the server state
%%      which was actual before execute the wrong step into a file.
set_onErrorSavePath(OnErrorSavePath) when is_list(OnErrorSavePath) ->
    gen_server:call(?ClExec_server, {set_onErrorPath, OnErrorSavePath}).


%% @spec get_config() -> clConfig()
%% @doc Return the actual clustering configuration.
%%      Be carefull the returned configuration is not packed. It's the real
%%      clustering configuration copied out from the server. It contains exist
%%      ETS table identifiers and others. If delete it demage the configuration
%%      inside the server. But if you use/modify this record carefully with the
%%      library functions ({@link ucl_config}, {@link ucl_state}) and load it
%%      back with {@link set_config/1} you can do tricks between algorithm
%%      steps.
get_config() ->
    gen_server:call(?ClExec_server, get_config).

%% @spec set_config(Cfg::clConfig()) -> ok
%% @doc Set the clustering configuration. `Cfg' must be a real `clConfig' record
%%      not a packed one.
set_config(Cfg=#clConfig{}) ->
    gen_server:call(?ClExec_server, {set_config, Cfg}).


%% @spec exec_steps(StepCnt::all | natural()) ->
%%           {ProcessedStepCnt::natural(), ok | Error}
%%       Error = term()
%% @doc Execute `StepCnt' step from the begin of the actual list of steps.
%%      Return the number of sucessfully executed steps and the error term. 
%%      If there was no error retun `ok'.
exec_steps(StepCnt) when 
        (all==StepCnt orelse (is_integer(StepCnt) andalso 0<StepCnt)) ->
    gen_server:call(?ClExec_server, {exec_steps, StepCnt}, infinity).



%%% ============================================================================
%%% Implementation of server

%%% ----------------------------------------------------------------------------
%%% Server state

%% @ type clExecState().
-record(clExecState, {
    error,          % undefined | term(): last error
    onerror_halt,   % boolean(): terminate on error or don't
    onerror_path,   % filepath(): save file path for case termination by error
    config          % clConfig(): Clustering configuration
    }).



%%% ----------------------------------------------------------------------------
%%% Generic server callback functions

%% @private
% Create a new clustering configuration and set OnErrorHalt flag to false.
init(_Args) ->
    process_flag(trap_exit, true),
    Cfg = ?ClConfig:new([], [], ?UNDEF),
    {ok, new(false,"",Cfg)}.


%% @private
% Stop the server
handle_cast(stop, ExSt=#clExecState{}) ->
    {stop, normal, ExSt}.


%% @private
% Save the server state into a file.
handle_call({save_state, FilePath}, _From, ExSt=#clExecState{}) ->
    save(ExSt, FilePath),
    {reply, ok, ExSt};
% Load the server state ftom a file.
handle_call({load_state, FilePath}, _From, ExSt=#clExecState{}) ->
    try
        {reply, ok, load(FilePath)}
    catch
        Error -> {reply, Error, ExSt}
    end;

% Get the last error occured while execute the spets at last time.
handle_call(get_error, _From, ExSt=#clExecState{error=Error}) ->
    {reply, Error, ExSt};
% Get `OnErrorHalt' flag.
handle_call(get_onErrorHalt, _From, ExSt=#clExecState{onerror_halt=OEH}) ->
    {reply, OEH, ExSt};
% Set `OnErrorHalt' flag.
handle_call({set_onErrorHalt, OnErrorHalt}, _From, ExSt=#clExecState{}) ->
    {reply, ok, ExSt#clExecState{onerror_halt=OnErrorHalt}};
% Get `OnErrorSavePath' property.
handle_call(get_onErrorPath, _From, ExSt=#clExecState{onerror_path=OEP}) ->
    {reply, OEP, ExSt};
% Set `OnErrorSavePath' property.
handle_call({set_onErrorPath, OnErrorPath}, _From, ExSt=#clExecState{}) ->
    {reply, ok, ExSt#clExecState{onerror_path=OnErrorPath}};

% Get actual clustering configuration.
handle_call(get_config, _From, ExSt=#clExecState{config=Cfg=#clConfig{}}) ->
    {reply, Cfg, ExSt};
% Set actual clustering configuration.
handle_call({set_config,Cfg=#clConfig{}}, _From, ExSt=#clExecState{}) ->
    {reply, ok, ExSt#clExecState{config=Cfg}};

% Execute `StepCnt' steps from the begin of actual list of steps.
handle_call({exec_steps, StepCnt}, _From, ExSt=#clExecState{}) ->
    exec_steps(StepCnt, ExSt).


%% @private
% 
handle_info(_Info, ExSt=#clExecState{}) ->
    {noreply, ExSt}.


%% @private
% Cleanup when the server is terminated.
terminate(_Reason, ExSt=#clExecState{}) ->
    delete(ExSt),
    ?UI:message(status, "The clustering processor server is terminated", []),
    ok.


%% @private
% Convert server state when the code is changed on-the-fly.
code_change(_Old, ExSt=#clExecState{}, _New) ->
    ExSt.



%%% ----------------------------------------------------------------------------
%%% Functions of server state

%% @private
%% @spec new(OnErrorHalt::boolean(), OnErrorSavePath::list(),
%%               Config::clConfig()) -> clExecState()
%% @doc Create a new `clExecState' server state record.
new(OnErrorHalt, OnErrorSavePath, Config=#clConfig{}) when 
        is_boolean(OnErrorHalt), is_list(OnErrorSavePath) ->
    #clExecState{onerror_halt=OnErrorHalt, onerror_path=OnErrorSavePath,
                 config=Config}.

%% @private
%% @spec delete(ExecState::clExecState()) -> ok
%% @doc Delete a `clExecState' server state record.
delete(#clExecState{config=Cfg}) ->
    ?ClConfig:delete(Cfg).


%% @private
%% @spec pack(ExecState::clExecState()) -> PackedExecState::clExecState()
%% @doc Create a new `clExecState' object which contains all information from 
%%      the original `ExecState' as values.
%%      Don't use the returned `PackedExecState' object as the normal
%%      `clExecState' objects! In this case a run time error may occur.
%%      If you have a packed `clExecState' object you need to unpack with the
%%      {@link unpack/1} function before use it.
%% @see unpack/1
pack(ExSt=#clExecState{config=Cfg}) ->
    ExSt#clExecState{config=?ClConfig:pack(Cfg)}.

%% @private
%% @spec unpack(PackedExecState::clExecState()) -> ExecState::clExecState()
%% @doc Create a normal `clExecState' object from the given packed
%%      `PackedExecState' objecjt.
%% @see pack/1
unpack(ExSt=#clExecState{config=Cfg}) ->
    ExSt#clExecState{config=?ClConfig:unpack(Cfg)}.

%% @private
%% @spec save(ExecState::clExecState(), FilePath::list()) -> ok
%% @doc Pack the `ExecState' object and save that into the `FilePath' file.
save(ExSt=#clExecState{}, FilePath) when is_list(FilePath) ->
    ?ClCommon:save("~p.\n", [pack(ExSt)], FilePath).

%% @private
%% @spec load(FilePath::list()) -> ExecState::clExecState()
%% @doc Load a `clExecState' object from the `FilePath' file and unpack that.
load(FilePath) ->
    unpack(?ClCommon:load_test_rec(FilePath,clExecState)).



%%% ----------------------------------------------------------------------------
%%% Implementation of server

%% @private
%% @spec exec_steps(StepCnt::StepCnt, ExSt::clExecState()) -> 
%%               {reply, {ProcessedStepCnt::natural(), Error}, clExecState()}
%%       StepCnt = all | natural()
%%       Error = ok | term()
%% @doc Process `StepCnt' step from the begin of the actual list of steps.
%%      Return the number of processed steps and the error term. 
%%      If there was no error retun `ok'.
exec_steps(StepCnt,
        ExSt=#clExecState{onerror_halt=OnErrorHalt, onerror_path=OnErrorPath})
        when (all==StepCnt orelse (is_integer(StepCnt) andalso 0<StepCnt)) ->
    % Execute StepCnt step
    {ProcStepCnt, ExSt1} = 
        exec_steps_(StepCnt, 0, ExSt#clExecState{error=?UNDEF}),
    % If an error occured and the halt flag is set stop the server
    case ExSt1#clExecState.error of
        ?UNDEF -> {reply, {ProcStepCnt,ok}, ExSt1};
        Error  ->
            if
                OnErrorHalt ->
                    if
                        ""/=OnErrorPath -> save(ExSt1, OnErrorPath);
                        true -> ok
                    end,
                    {stop, {error,Error}, {ProcStepCnt,Error}, ExSt1};
                true ->
                    {reply, {ProcStepCnt,Error}, ExSt1}
            end
    end.

% Process `StepCnt' step
exec_steps_(0, ProcStepCnt, ExSt=#clExecState{}) ->
    {ProcStepCnt, ExSt};
exec_steps_(all, ProcStepCnt, ExSt=#clExecState{config=#clConfig{steps=[]}}) ->
    {ProcStepCnt, ExSt};
exec_steps_(StepCnt, ProcStepCnt, ExSt=#clExecState{}) ->
    ExSt1 = exec_steps_do(ExSt),
    case ExSt1#clExecState.error of
        ?UNDEF -> 
            StepCnt1 = if
                all==StepCnt -> all;
                true         -> StepCnt-1
            end,
            exec_steps_(StepCnt1, ProcStepCnt+1, ExSt1);
        _ -> 
            {ProcStepCnt, ExSt1}
    end.

% Process one step and handle the errors
exec_steps_do(ExSt=#clExecState{}) ->
    try
        exec_step(ExSt)
    catch
        throw:Error when ?Is_RefError(Error) ->
            ErrorMsg = ?Error:error_text(Error),
            ?UI:message(status, "~s", [ErrorMsg]),
            io:format(
                "** An exception occurred while execute a step.\n"
                "** Reason: ~p\n"
                "** Message: ~s\n\n",
                [Error, ErrorMsg]),
            ExSt#clExecState{error=Error};
        throw:Error ->
            ErrorMsg = ?MISC:format("~p", [Error]),
            ?UI:message(status, "~s", [ErrorMsg]),
            io:format(
                "** An exception occurred while execute a step.\n"
                "** Reason: ~p\n\n",
                [Error]),
            ExSt#clExecState{error=Error};
        error:Error -> 
            Stack = erlang:get_stacktrace(),
            ErrorMsg = ?MISC:format("~p", [Error]),
            ?UI:message(error,  "~s", [ErrorMsg]),
            io:format(
                "** A runtime error occurred while execute a step.\n"
                "** Reason: ~p\n\n",
                [Error]),
            error_logger:error_msg(
                "** A runtime error occurred while execute a step.\n"
                "** Reason: ~p\n"
                "** Stack:\n     ~p\n",
                [Error, Stack]),
            ExSt#clExecState{error=Error}
    end.


%% @private
%% @spec exec_step(ProcState::clExecState()) -> clExecState()
%% @throws localError(no_more_steps)
%% @doc Execute the first step from the actual list of steps.
exec_step(#clExecState{config=#clConfig{steps=[]}}) ->
    throw(?LocalError(no_more_steps, []));
exec_step(ExSt=#clExecState{config=Cfg=#clConfig{algs=Algs, steps=[Step|Steps],
        donesteps=DoneSteps, state=State}}) ->
    {FunName,_Opts} = Step,
    io:format("========================================\n", []),
    io:format("Step: ~p\n", [FunName]),
    % Prepare current step to execute
    {{ModName,FunName},Options} = prepare_step(Step, Algs),
    % Execute the step
    io:format("Function: ~p:~p/2\n", [ModName,FunName]),
    io:format("Options: ~p\n",   [Options]),
    io:format("--------------------\n", []),
    {Time, {State1,PlusSteps1}} = case ModName of
        ?MODULE -> timer:tc(?MODULE,FunName,[ExSt, Options]);
        _       -> timer:tc(ModName,FunName,[State,Options])
    end,
    io:format("--------------------\n", []),
    io:format("Runnig time: ~p sec\n\n", [Time/1.0e6]),
    % Return the updated server state
    Cfg1 = Cfg#clConfig{steps=PlusSteps1++Steps, donesteps=[Step|DoneSteps], 
                        state=State1},
    ExSt#clExecState{config=Cfg1}.


%% @private
%% @spec prepare_step({FunName::atom(),Options::proplist()}, Algs::[AlgName]) ->
%%           {{ModName::atom(),FunName::atom()}, UpdatedOptions::proplist()}
%%       AlgName = atom()
%% @throws localError(invalid_options)
%% @doc Prepare a step to execution. Find the module of the function of step.
%%      Get the default and validator function of it. Update the options of 
%%      step with the defaults and validate the updated list.
prepare_step({FunName,Options}, Algs) ->
    % Correct the inheritance chain by this module if ecessary
    Algs1 = case lists:member(FunName, reserved_steps()) of
        true  -> [?MODULE|Algs];
        false -> Algs
    end,
    % Find the most specified algorithm that has the function of the step 
    code:ensure_loaded(?ClAlg),
    ModName = find_module(Algs1, FunName,2),
    % Find default and validator functions
    DefsFunName  = ?FunOptsDefs(FunName),
    ValidFunName = ?FunOptsValid(FunName),
    {DM,DF} = 
        case erlang:function_exported(ModName, DefsFunName,0) of
            true  -> {ModName, DefsFunName};
            false -> {?ClAlg,  empty_default} % always []
        end,
    {VM,VF} = 
        case erlang:function_exported(ModName, ValidFunName,0) of
            true  -> {ModName, ValidFunName};
            false -> {?ClAlg,  empty_validator} % always []
        end,
    % Update Options with defauls and validate the new list
    Options1 = ?MISC:proplist_merge_def(Options, apply(DM,DF,[])),
    case ?MISC:proplist_validate(Options1, apply(VM,VF,[])) of
        {_,[]}     -> {{ModName,FunName},Options1};
        {_,Wrongs} ->
            throw(?LocalError(invalid_options, {{ModName,FunName},Wrongs}))
    end.


%% @private
%% @spec find_module(ModNames::[atom], FunName::atom, Arity::natural()) ->
%%               ModName::atom() | undefined
%% @throws localError(missing_fun)
%% @doc Find the first module from the `ModNames' list which has a 
%%      `FunName'/`Arity' function and return the module name.
%%      If a module is not loaded load that with the
%%      {@link code:ensure_loaded/1} function.
find_module(ModNames, FunName, Arity) when is_list(ModNames),
        is_atom(FunName), is_integer(Arity) ->
    % Find the step function and ensure the module is loaded
    FindRet = ?MISC:list_find(
        fun(ModName) ->
            case code:ensure_loaded(ModName) of
                {'module',ModName} -> ok;
                {error,   Error  } ->
                    throw(?LocalError(missing_module, {ModName,Error}))
            end,
            erlang:function_exported(ModName,FunName,Arity)
        end,
        ModNames),
    % Return the module name
    case FindRet of
        {0,_ } -> throw(?LocalError(missing_fun, {ModNames,FunName}));
        {_,MN} -> MN
    end.



%%% ============================================================================
%%% Reserved callback functions

%% @spec reserved_steps() -> [FunName::atom()]
%% @doc Return the reserved step functions. You can not define function which
%%      hide any reserved function. The reserved functions always are executed
%%      from this module. If you define a function with same name in an
%%      algorithm module that never will be executed by the clustering processor
%%      server.
%%
%%      Actually the reserver step function names are:
%%      [exec_save].
reserved_steps() ->
    [exec_save].


%% @spec exec_save_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the 
%%       {@link exec_save/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`server_state::string()': The save file path of the server state.</li>
%%   <li>`config::string()': The save file path of the actual clustering
%%     configuration.</li>
%% </ul>
exec_save_validator() ->
    [{server_state, fun(V) -> is_list(V) end},
     {config,       fun(V) -> is_list(V) end}].

%% @spec exec_save(ServerState::clExecState(), Options::proplist()) -> 
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Save the `ServerState' server state and the clustering configuration 
%%      into the specified files given in `Options' list.
%%
%%      For the avaible options see {@link exec_save_validator/0} function.
%% @see exec_save_validator/0
exec_save(ExSt=#clExecState{config=Cfg=#clConfig{steps=Steps, state=St}}, 
        Options) ->
    ExSt0 = ExSt#clExecState{config=Cfg#clConfig{steps=tl(Steps)}},
    lists:foreach(
        fun({Key, SaveFun}) ->
            case get_value(Key, Options, ?UNDEF) of
                ?UNDEF    -> ok;
                FilePath  -> SaveFun(ExSt0, FilePath)
            end
        end,
        [{server_state,
            fun(ExSt1=#clExecState{}, FilePath) ->
                save(ExSt1,FilePath)
            end},
         {config, 
            fun(#clExecState{config=Cfg1=#clConfig{}}, FilePath) ->
                ?ClConfig:save(Cfg1,FilePath)
            end}]),
    % Return state and steps
    {St, []}.



