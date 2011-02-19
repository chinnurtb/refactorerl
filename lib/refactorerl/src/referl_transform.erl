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

%%% @doc Asynchron transformation framework. Transformations are implemented
%%% in callback modules, and they are executed in a separate process. They
%%% can communicate with the user through this module. Currently only one
%%% transformation can be executed at a time, but support for nested
%%% transformations may be added later.
%%%
%%% == Callback modules ==
%%%
%%% Currently the only callback function in a transformation module is
%%%
%%% `prepare(Args::arglist()) -> Transformation'
%%%
%%% where `Transformation = fun() | [Transformation]'.
%%%
%%% This function gets the parameters of the transformation in an {@link
%%% referl_args:arglist(). argument proplist}, and should return a fun (or a
%%% possibly deep list of funs) that makes the transformation. The `prepare'
%%% function must ensure that the transformation can be carried out, that is,
%%% it should check every side condition and collect every needed data. The
%%% returned fun(s) should not be aborted after making changes to the graph
%%% (which implies that it must not ask questions, as they may be cancelled).
%%%
%%% The first transformation function receives no parameters. The following
%%% functions in the list receive the result of the previous function. In case
%%% of embedded lists of functions, they get the same parameter, their return
%%% values are collected into a list, and that list is passed to the next
%%% function.
%%%
%%% Every function in the list must form a valid ESG batch (see {@link
%%% referl_esg}). The batch is implicitly closed by this module, the
%%% transformation code should not call the ?ESG module directly.
%%%
%%% Error conditions are signalled by throwing exceptions from the
%%% function. Exception data should be constructed using the macros
%%% `?RefError' and `?LocalError'. See {@link referl_error} for details.
%%%
%%% When the transformation modifies something, it should use the {@link
%%% touch/1} function to request the saving of the modified file after the
%%% transformation.
%%%
%%% The return value of the last transformation function can be queried using
%%% the {@link wait/0} function.
%%%
%%% == Interaction ==
%%%
%%% A transformation module can ask questions using the {@link ask/1}
%%% function. Questions are identified by an arbitrary term; clients can
%%% specify the ID and make use of a custom ID system, or may let the system
%%% choose a unique ID for a question. Asking a question is an asynchron
%%% operation, the answer is only waited for (and returned) by {@link
%%% answer/1}.
%%%
%%% === Question properties ===
%%%
%%% A question is specified by a list which may contain two-tuples and
%%% atoms. The following elements are supported:
%%%
%%% <dl>
%%%
%%% <dt>`{text, Text}'</dt>
%%%
%%%   <dd>The text of the question (a deep character list). Must be
%%%   specified.</dd>
%%%
%%% <dt>`{type, Type}'</dt>
%%%
%%%   <dd>The type of the question. `Type' may have the following values
%%%   (the default is `string'):
%%%
%%%   <ul>
%%%   <li>`string': the answer may be an arbitrary string.</li>
%%%   <li>`yesno': the answer may be `yes' or `no'.</li>
%%%   </ul>
%%%   </dd>
%%%
%%% <dt>`{id, Id}'</dt>
%%%
%%%   <dd>The identifier of the question. Default: a unique identifier is
%%%   generated.</dd>
%%%
%%% </dl>
%%%
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(referl_transform).
-vsn("$Rev: 2944 $").
-behaviour(gen_server).

%%% ============================================================================
%%% Exports

%% User interface exports
-export([do/2, reply/2, cancel/1, wait/0]).

%% Transformation interface exports
-export([ask/1, answer/1, touch/1, rename/2]).

%% Enviroment exports
-export([start_link/0]).

%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("refactorerl.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%% @type proplist() = [atom() | {atom(), term()}]. See the standard module
%%% `proplists'.

%%% @type node() = referl_graph:node()

%%% ============================================================================
%%% Interface functions

%% @private
start_link() ->
    gen_server:start_link({local, ?TRANSFORM_SERVER}, ?MODULE, [], []).

%% @spec do(atom(), referl_args:arglist()) -> ok
%%
%% @doc Start a transformation. The transformation is implemented by module
%% `Mod', and `Args' are passed as transformation parameters.
do(Mod, Args) when is_list(Args) ->
    case gen_server:call(?TRANSFORM_SERVER, {do, Mod, Args}) of
        ok -> ok;
        {error, Error} -> erlang:error(Error, [Mod, Args])
    end.

%% @spec wait() -> none | {Return, term()}
%%       Return = result | abort | error | exit
%%
%% @doc Returns the result of the last transformation. If the transformation
%% is still running, waits for it to terminate. Only the first call will
%% return the result, subsequent calls will return `none'.
wait() ->
    gen_server:call(?TRANSFORM_SERVER, wait, infinity).

%% @spec ask(proplist()) -> term()
%% @doc Ask a question through the user interface. The question ID is returned.
ask(Props) ->
    case gen_server:call(?TRANSFORM_SERVER, {ask, self(), Props}) of
        {ok, Reply} -> Reply;
        {error, Error} -> erlang:error(Error, [Props])
    end.

%% @spec answer(term()) -> term()
%%
%% @doc Wait for the answer to a question previously asked using `ask/1'.
%% The answer is returned.
answer(Id) ->
    case gen_server:call(?TRANSFORM_SERVER, {answer, self(), Id}, infinity) of
        cancel         -> throw(?RefErr0r(cancelled));
        {reply, Reply} -> Reply;
        {error, Error} -> erlang:error(Error, [Id])
    end.

%% @spec touch(node()) -> ok
%%
%% @doc Arranges for the file that contains `Node' to be saved after the
%% transformation.
touch(Node) ->
    gen_server:cast(?TRANSFORM_SERVER, {touch, Node}).

%% @spec rename(OldPath, NewPath) -> ok
%%
%% @doc Arranges for the file to be saved after the transformation.
rename(OldPath, NewPath) ->
    gen_server:cast(?TRANSFORM_SERVER, {rename, {OldPath, NewPath}}).


%% @spec reply(integer(), term()) -> ok
%%
%% @doc Supply the answer to question number `Num'. This function is called
%% by the user interface bridge module.
reply(Num, Reply) ->
    gen_server:cast(?TRANSFORM_SERVER, {reply, Num, Reply}).

%% @spec cancel(integer()) -> ok
%%
%% @doc Deny the answer to question number `Num', which results in the
%% abortion of the transformation. This function is called by the user
%% interface bridge module.
cancel(Num) ->
    gen_server:cast(?TRANSFORM_SERVER, {cancel, Num}).

%%% ============================================================================
%%% Server callback functions

-record(state, {question, % ETS table for question data
                answer,   % ETS table for answer data
                wait,     % ETS table for answer requests
                current,  % PID of currently running transformation
                next,     % Number of next question
                save,     % List of file nodes to be saved
                rename    % List of file path pairs {OldPath, NewPath} to be
                          % renamed
               }).

%% @private
init(_) ->
    process_flag(trap_exit, true),
    {ok, init_state()}.

%% @private
handle_call({do, Mod, Args}, _From, State) ->
    {Rep, St} = handle_do(Mod, Args, State),
    {reply, Rep, St};

handle_call(wait, From, State) ->
    handle_wait(From, State);

handle_call({ask, Pid, Props}, _From, #state{current=Pid} = State) ->
    {Rep, St} = handle_ask(Pid, Props, State),
    {reply, Rep, St};

handle_call({ask, _Pid, _}, _From, St) ->
    {reply, {error, bad_client}, St};

handle_call({answer, Pid, ID}, From, State) ->
    handle_answer(Pid, ID, From, State);

handle_call({pending, Pid}, _From, State) ->
    {reply, handle_pending(Pid, State), State}.

%% @private
handle_cast({reply, Id, Reply}, St) ->
    handle_reply(Id, Reply, St),
    {noreply, St};

handle_cast({cancel, Id}, St) ->
    handle_cancel(Id, St),
    {noreply, St};

handle_cast({touch, Node}, State) ->
    St = handle_touch(Node, State),
    {noreply, St};

handle_cast({rename, Data}, State) ->
    St = handle_rename(Data, State),
    {noreply, St};

handle_cast({finish, Pid, Result}, #state{current=Pid} = State) ->
    St = handle_exit(Result, Pid, State),
    {noreply, St}.

%% @private
handle_info({'EXIT', Pid, Reason}, #state{current=Pid} = State) ->
    St = handle_exit({exit, Reason}, Pid, State),
    {noreply, St};
handle_info(_Msg, St) ->
    {noreply, St}.

%% @private
terminate(_R, #state{current=Cur}) ->
    if
        is_pid(Cur) ->
            exit(Cur, kill);
        true ->
            ok
    end.

%% @private
code_change(_Old, St, _New) -> St.

%%% ============================================================================
%%% Implementation
%%%
%%% There are three ETS tables that store question-related data:
%%%  question :: {Num::integer(), ID::term(), Owner::pid()}
%%%  answer   :: {{Owner::pid(), ID::term()}, Answer::term()}
%%%  wait     :: {{Owner::pid(), ID::term()}, Client::pid()}
%%%
%%% Questions are numbered by consecutive integers to make identification in
%%% UIs easier. Internally, the owner process and the specified or generated
%%% ID identifies a question.
%%%
%%% When an unavailable answer is requested, the request is stored in table
%%% `wait'. When the answer is received, this table is used to notify
%%% waiting clients. There is an extra `result' key for the client that
%%% waits for the result of the current transformation.

init_state() ->
    #state{question = ets:new(question, []),
           answer   = ets:new(answer, []),
           wait     = ets:new(wait, [bag]),
           current  = [],
           next     = 1,
           save     = [],
           rename   = []}.

%% Start a new transformation
handle_do(_Mod, _Args, #state{current=P} = St) when is_pid(P) ->
    {{error, running}, St};
handle_do(Mod, Args, #state{} = St) ->
    Pid = spawn_link(fun () -> do_transform(Mod, Args) end),
    {ok, St#state{current = Pid}}.

%% Return the result of a transformation
handle_wait(_From, #state{current=[]} = St) ->
    {reply, none, St};
handle_wait(From, #state{current=Pid, wait=WT} = St) when is_pid(Pid) ->
    ets:insert(WT, {result, From}),
    {noreply, St};
handle_wait(_From, #state{current=R} = St) ->
    {reply, R, St#state{current=[]}}.

%% Ask a question
handle_ask(Pid, Props, #state{question=QT, next=Next} = St) ->
    ID = proplists:get_value(id, Props, make_ref()),
    ets:insert(QT, {Next, ID, Pid}),
    P1 = [{text, lists:flatten(proplists:get_value(text, Props, "?"))} |
          proplists:delete(text, Props)],
    ?UI:message(question, {Next, P1}),
    {{ok, ID}, St#state{next=Next+1}}.

%% Store a reply to a question
handle_reply(Num, Reply, #state{question=QT, answer=AT, wait=WT}) ->
    handle_reply(Num, {reply, Reply}, QT, AT, WT).

handle_cancel(Num, #state{question=QT, answer=AT, wait=WT}) ->
    handle_reply(Num, cancel, QT, AT, WT).

handle_reply(Num, Reply, QT, AT, WT) ->
    case ets:lookup(QT, Num) of
        [{_, ID, Owner}] ->
            ets:delete(QT, Num),
            ets:insert(AT, {{Owner, ID}, Reply}),
            [gen_server:reply(Client, Reply) ||
                {_, Client} <- ets:lookup(WT, {Owner, ID})],
            ets:delete(WT, ID);
        [] ->
            ok
    end.

%% Return the answer for a question
handle_answer(Pid, ID, Client, #state{answer=AT, wait=WT} = St) ->
    case ets:lookup(AT, {Pid, ID}) of
        [{_, Reply}] ->
            {reply, Reply, St};
        [] ->
            ets:insert(WT, {{Pid, ID}, Client}),
            {noreply, St}
    end.

%% Return the list of question IDs waiting for an answer
handle_pending(Pid, #state{question=QT}) ->
    ets:select(QT, ets:fun2ms(fun({_, Id, P}) when P =:= Pid -> Id end)).

%% Remembers the files that have been changed.
%% The list `save' is not unique until the point of saving.
handle_touch(Node, #state{save=Save} = St) ->
    St#state{save=?Syn:get_file(Node)++Save}.

%% Remembers the files that have to be renamed.
%% The list `rename' is not unique until the point of saving.
handle_rename(Data, #state{rename=Rename} = St) ->
    St#state{rename=[Data|Rename]}.

%% Close a transformation
handle_exit(Result, Pid, #state{question=QT, answer=AT, wait=WT,
                                save=Save, rename=Rename} = St) ->
    case Result of
        {result, _}    ->
            try
                rename_files(lists:usort(Rename)),
                save_files(lists:usort(Save)),
                ?UI:message(status, "Finished", [])
            catch
                {rename, File, Err} ->
                    ?UI:message(status, "Error renaming ~s: ~s", [File, Err]);
                {save, File} ->
                    ?UI:message(status, "File ~s could not be saved", [File]);
                save ->
                    ?UI:message(error, "error during saving results")
            end;
        {exit, Reason} -> ?UI:message(status, "Exited: ~p", [Reason]);
        {abort, Error} -> ?UI:message(status, "~s", [?Error:error_text(Error)]);
        {error, Error} -> ?UI:message(error, "~p", [Error])
    end,
    ?UI:message(trfinished,ok),
    StRes = notify_wait(WT, Result),
    ets:match_delete(QT, {'_', '_', Pid}),
    ets:match_delete(AT, {{Pid, '_'}, '_'}),
    ets:match_delete(WT, {{Pid, '_'}, '_'}),
    St#state{current = StRes, save=[], rename=[]}.

notify_wait(WT, Result) ->
    case ets:lookup(WT, result) of
        [] ->
            Result;
        Lst ->
            [gen_server:reply(Client, Result) || {_, Client} <- Lst],
            ets:delete(WT, result),
            []
    end.

rename_files(Rename) ->
    lists:foreach(fun rename_file/1, Rename).

rename_file({OldPath, NewPath}) ->
    case file:rename(OldPath, NewPath) of
        ok ->
            ?UI:message(rename, {OldPath, NewPath});
        {error, Reason} ->
            throw({rename, OldPath, file:format_error(Reason)})
    end.

save_files(Files) ->
    lists:foreach(fun save_file/1, Files).

save_file(File) ->
    try
        #file{path=Path} = ?Graph:data(File),
        try
            ?FileMan:save_file(File),
            ?UI:message(reload, "~s", [Path])
        catch
            error:Reason ->
                error_logger:error_msg(
                  "Runtime error while saving file ~p~n"
                  "  Reason: ~p~n"
                  "  Stack: ~p~n",
                  [File, Reason, erlang:get_stacktrace()]),
                throw({save, Path})
        end
    catch
        _:_ ->
            error_logger:error_msg("Saving invalid file node ~p~n", [File]),
            throw(save)
    end.

%%% ----------------------------------------------------------------------------
%%% Transformation process

do_transform(Mod, Args) ->
    try
        Transform = Mod:prepare(Args),
        [answer(Id) ||
            Id <- gen_server:call(?TRANSFORM_SERVER, {pending, self()})],
        Result = run(Transform),
        gen_server:cast(?TRANSFORM_SERVER, {finish, self(), {result, Result}})
    catch
        throw:Error ->
            gen_server:cast(?TRANSFORM_SERVER, {finish, self(), {abort,Error}});
        error:Error ->
            gen_server:cast(?TRANSFORM_SERVER, {finish, self(), {error,Error}}),
            error_logger:error_msg(
              "** A runtime error occurred in ~p~n"
              "** Arguments: ~p~n"
              "** Reason: ~p~n"
              "** Stack:~n     ~p~n",
              [Mod, Args, Error, erlang:get_stacktrace()])
    end.

run(Transform) ->
    runs(Transform, first).

runs([], first)          -> [];
runs([], {arg, Arg})     -> Arg;
runs([Head | Tail], Arg) -> runs(Tail, {arg, runp(Head, Arg)});
runs(Fun, _)             -> runt(Fun).

runp([], _)            -> [];
runp([Head|Tail], Arg) -> [runp(Head, Arg) | runp(Tail, Arg)];
runp(Fun, first)       -> runt(Fun);
runp(Fun, {arg, Arg})  -> runt(fun() -> Fun(Arg) end).

runt(Fun) ->
    R = try
            Fun()
        after
            ?ESG:close()
        end,
    case is_list(R) andalso lists:all(fun is_function/1, R) of
        true  -> runs(R, first);
        false -> R
    end.
