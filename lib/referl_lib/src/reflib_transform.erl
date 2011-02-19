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
%%% reflib_args:arglist(). argument proplist}, and should return a fun (or a
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
%%% refcore_esg}). The batch is implicitly closed by this module, the
%%% transformation code should not call the ?ESG module directly.
%%%
%%% Error conditions are signalled by throwing exceptions from the
%%% function. Exception data should be constructed using the macros
%%% `?RefError' and `?LocalError'. See {@link reflib_error} for details.
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
%%%   <li>`string': the answer is an arbitrary string.</li>
%%%   <li>`yesno': the answer is `yes' or `no'.</li>
%%%   <li>`{select, [Values]}': the answer is one of `Values'.</li>
%%%   </ul>
%%%   </dd>
%%%
%%% <dt>`sync'</dt>
%%%
%%%   <dd>This property means that this question should be answered
%%%   immediately. User interfaces may choose to display this question
%%%   modally, while other questins can be accumulated into a form.</dd>
%%%
%%% <dt>`{ref, Node}'</dt>
%%%
%%%   <dd>This property is a reference to a syntax tree part. The user
%%%   interface will provide a way to show the code that belongs to the
%%%   subtree starting at `Node'.</dd>
%%%
%%% <dt>`{id, Id}'</dt>
%%%
%%%   <dd>The identifier of the question. Default: a unique identifier is
%%%   generated.</dd>
%%%
%%% </dl>
%%%
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(reflib_transform).
-vsn("$Rev: 5455 $").
-behaviour(gen_server).

%%% ============================================================================
%%% Exports

%% User interface exports
-export([do/3, reply/2, cancel/1, wait/0]).

%% Transformation interface exports
-export([question/1, touch/1, rename/2]).

%-export([ask/1, answer/1]). % @depracated

%% Enviroment exports
-export([start_link/0]).

%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Callbacks
-export([error_text/2]).

% private
-export([test123/0]).


-include("lib.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


%%% ----------------------------------------------------------------------------
%% @todo extract

send_change(MCB,Change) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{change,Change}]).

-define(Modified(File),
        {File, [{content,true}]}).
-define(Renamed(OldPath,NewPath),
        {OldPath, [{rename,NewPath}]}).


%%% ============================================================================
%%% Error texts

error_text(rename, [File,Err]) ->
    ["Error renaming ", File, ": ", Err];
error_text(save, [File]) ->
    ["File ", File, " could not be saved"];
error_text(save, []) ->
    ["error during saving results"];
error_text(exit, [Reason]) ->
    ["Transform exited: ", io_lib:print(Reason)];
error_text(error, [Error]) ->
    ["Transform internal error: ", io_lib:print(Error)];
error_text(exception, [Error]) ->
    ["Transform internal error exception (", io_lib:print(Error), ")"];
error_text(running, []) ->
    ["A transform or query is already running.",
     " Close all interaction dialogs and try again later."];
error_text(bad_client, []) ->
    ["Internal error: bad client request"];
error_text(bad_q, [Q]) ->
    ["Malformed question: ", io_lib:print(Q)].


%%% @type proplist() = [atom() | {atom(), term()}]. See the standard module
%%% `proplists'.

%%% @type node() = refcore_graph:node()

%%% ============================================================================
%%% Interface functions

%% @private
start_link() ->
    gen_server:start_link({local, ?TRANSFORM_SERVER}, ?MODULE, [], []).

%% @spec do(#msg_cb{}, atom(), reflib_args:arglist()) -> ok
%%
%% @doc Start a transformation. The transformation is implemented by module
%% `Mod', and `Args' are passed as transformation parameters.
do(_MCB=#msg_cb{}, Mod=reftr_apply_funcluster, Args) when is_list(Args) ->
    Mod:do(Args); %@todo un-hack
do(MCB=#msg_cb{}, Mod, Args) when is_list(Args) ->
    case gen_server:call(?TRANSFORM_SERVER, {do, MCB, Mod, Args}) of
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

question(Props) ->
    answer(ask(Props)).

%% @spec ask(proplist()) -> term()
%% @doc Ask a question through the user interface. The question ID is returned.
ask(Props) ->
    error_handled(
      gen_server:call(?TRANSFORM_SERVER, {ask, self(), Props}),
      [Props]).

error_handled(Result,Args) ->
    case Result of
        {reply, Reply} ->
            Reply;
        {error, Error} ->
            erlang:error(Error, Args)
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
%% @doc Arranges for the file to be renamed after the transformation.
rename(OldPath, NewPath) ->
    gen_server:cast(?TRANSFORM_SERVER, {rename, {OldPath, NewPath}}).


%% @spec reply(integer(), term()) -> ok
%%
%% @doc Supply the answer to question number `Num'. This function is called
%% by the user interface bridge module.
reply(Num, Reply) ->
    error_handled(
      gen_server:call(?TRANSFORM_SERVER, {reply, Num, Reply}),
      [Num,Reply]).

%% @spec cancel(integer()) -> ok
%%
%% @doc Deny the answer to question number `Num', which results in the
%% abortion of the transformation. This function is called by the user
%% interface bridge module.
cancel(Num) ->
    error_handled(
      gen_server:call(?TRANSFORM_SERVER, {cancel, Num}),
      [Num]).

%%% ============================================================================
%%% Server callback functions

-record(state, {question, % ETS table for question data
                answer,   % ETS table for answer data
                wait,     % ETS table for answer requests
                current,  % PID of currently running transformation
                next,     % Number of next question
                save,     % List of file nodes to be saved
                rename,   % List of file path pairs {OldPath, NewPath} to be
                          % renamed
                mcb       % #msg_cb{} message callbacks
               }).

%% @private
init(_) ->
    process_flag(trap_exit, true),
    {ok, init_state()}.

%% @private
handle_call({do, MCB, Mod, Args}, _From, State) ->
    {Rep, St} = handle_do(MCB, Mod, Args, State),
    {reply, Rep, St};

handle_call(wait, From, State) ->
    handle_wait(From, State);

handle_call({ask, Pid, Props}, _From, #state{current=Pid} = State) ->
    {Rep, St} = handle_ask(Pid, Props, State),
    {reply, Rep, St};

handle_call({ask, _Pid, _}, _From, St) ->
    {reply, {error, ?LocalError(bad_client, [])}, St};

handle_call({answer, Pid, ID}, From, State) ->
    handle_answer(Pid, ID, From, State);

handle_call({pending, Pid}, _From, State) ->
    {reply, handle_pending(Pid, State), State};

handle_call({reply, Id, Reply}, _From, St) ->
    {reply, handle_reply(Id, Reply, St), St};

handle_call({cancel, Id}, _From, St) ->
    {reply, handle_cancel(Id, St), St}.

%% @private
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
           rename   = [],
           mcb      = undef}.

%% Start a new transformation
handle_do(_MCB, _Mod, _Args, #state{current=P} = St) when is_pid(P) ->
    {{error, ?LocalError(running,[])}, St}; %@todo
handle_do(MCB, Mod, Args, #state{} = St) ->
    Pid = spawn_link(fun () -> do_transform(MCB, Mod, Args) end),
    {ok, St#state{current = Pid, mcb=MCB}}.

%% Return the result of a transformation
handle_wait(_From, #state{current=[]} = St) ->
    {reply, none, St};
handle_wait(From, #state{current=Pid, wait=WT} = St) when is_pid(Pid) ->
    ets:insert(WT, {result, From}),
    {noreply, St};
handle_wait(_From, #state{current=R} = St) ->
    {reply, R, St#state{current=[]}}.

%% Ask a question
handle_ask(Pid, Props, #state{mcb=MCB, question=QT, next=Next} = St) ->
    P0 = [ begin
           [Format,TxtL] = ?MISC:pget([format,text], Prop),
           case {Format,TxtL} of
               {[F],[Txt=[_|_]]} when is_atom(F) ->
                   {true, [{text, lists:flatten(Txt)} |
                       proplists:delete(text, Prop)]};
               _ ->
                   {false, []}
           end
       end || Prop <- Props],
    OK = lists:all(fun({B,_})->B end, P0),
    case OK of
        true ->
            {_, P1} = lists:unzip(P0),
            ID = proplists:get_value(id, Props, make_ref()),
            ets:insert(QT, {Next, ID, Pid}),
            (MCB#msg_cb.unicast)(question, {Next, P1}),
            {{reply, ID}, St#state{next=Next+1}};
        false ->
            {{error, ?LocalError(bad_q, Props)}, St}
    end.

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
            _ = [gen_server:reply(Client, Reply) ||
                    {_, Client} <- ets:lookup(WT, {Owner, ID})],
            ets:delete(WT, ID),
            {reply, noreply};
        [] ->
            {error,?LocalError(invalid_qid,[Num,Reply])}
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
handle_exit(Res, Pid,
            #state{question=QT, answer=AT, wait=WT, mcb=MCB,
                   save=Save, rename=Rename} = St) ->
    Result0 = %@todo
        case Res of
            {result, R} ->
                try
                    rename_files(MCB, lists:usort(Rename)),
                    save_files(MCB, lists:usort(Save)),
                    SaveU = lists:usort(Save),
                    SavedNames = lists:map(fun ?File:path/1,SaveU),
                    {result,[{renamed,Rename},{saved,SavedNames},{result,R}]}
                catch
                    {rename, File, Err} ->
                        ?LocalError(rename, [File, Err]);
                    {save, File} ->
                        ?LocalError(save, [File]);
                    save ->
                        ?LocalError(save, [])
                end;
            {exit, Reason} ->
                ?LocalError(exit, [Reason]);
            {abort, RefError} ->
                {abort, RefError};
            {error, Error={_,_,_}} ->
                Error;
            {error, Error} ->
                ?LocalError(error, [Error])
        end,
    Result =
        case Result0 of
            {_,_,_} ->
                {error, Result0};
            _ ->
                Result0
        end,
    StRes = notify_wait(WT, Result),
    ets:match_delete(QT, {'_', '_', Pid}),
    ets:match_delete(AT, {{Pid, '_'}, '_'}),
    ets:match_delete(WT, {{Pid, '_'}, '_'}),
    St#state{current = StRes, save=[], rename=[], mcb = undef}.

notify_wait(WT, Result) ->
    case ets:lookup(WT, result) of
        [] ->
            Result;
        Lst ->
            _ = [gen_server:reply(Client, Result) || {_, Client} <- Lst],
            ets:delete(WT, result),
            []
    end.

rename_files(MCB=#msg_cb{}, Rename) ->
    _ = [rename_file(MCB, File) || File <- Rename],
    ok.

rename_file(MCB=#msg_cb{}, {OldPath, NewPath}) ->
    case file:rename(OldPath, NewPath) of
        ok ->
            send_change(MCB, [?Renamed(OldPath,NewPath)]);
        {error, Reason} ->
            throw({rename, OldPath, file:format_error(Reason)})
    end.

save_files(MCB=#msg_cb{}, Files) ->
    _ = [save_file(MCB, File) || File <- Files],
    ok.

save_file(MCB=#msg_cb{}, File) ->
    try
        #file{path=Path} = ?Graph:data(File),
        try
            ok = ?FileMan:save_file(File),
            send_change(MCB, [?Modified(Path)])
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

do_transform(_MCB, Mod, Args) ->
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
    try
       runs(Transform, first)
    after
      ?ESG:finalize()
    end.


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
            ?ESG:finalize()
        end,
    case is_list(R) andalso lists:all(fun is_function/1, R) of
        true  -> runs(R, first);
        false -> R
    end.

%%% ----------------------------------------------------------------------------
%%% Validation
%%% @todo

%% @type ui_question() = proplist()
%% @type ui_answer() = proplist()
%% @type ui_questions() = [ui_question()]
%% @type ui_answers() = [ui_answer()]

test123() ->
    check_result([],[]).

check_result(Questions,Answers) ->
   Valid = validate(Questions,Answers),
   case lists:all(fun(X)->X end,Valid) of
       true -> true;
       false ->
           NewQ = insert_defaults(Questions,Answers,Valid),
           resend_questions(NewQ)
   end.

resend_questions(_) -> throw(todo).

insert_defaults([Q|Qs],[A|As],[V|Vs]) ->
    NQ = case V of
      true  -> Q ++ [{value,A}];
      false -> Q
    end,
    [NQ | insert_defaults(Qs,As,Vs)];
insert_defaults([],[],[]) ->
    [].

%% @doc
%% Constraint: length(Questions)==length(Answers)
%% @spec (ui_questions(), ui_answers()) -> [boolean()]
validate(Questions, Answers) ->
   lists:zipwith(fun validate1_cancel/2, Questions, Answers).

validate1_cancel(Question, Answer) ->
   (Answer==cancel) orelse validate1(Question,Answer).

validate1(Question, Answer) ->
   [Type, Validator] = ?MISC:pgetu([type, {validator,none}], Question),
   Typed = case Type of
       yesno ->
           is_boolean(Answer);
       string ->
           is_list(Answer);
       _ ->
           throw(?RefError(valtype_unknown,[Type]))
   end,
   Typed andalso validator(Type,Validator,Answer).

validator(_,none,_) -> true;
validator(T,V,_) ->
           throw(?RefError(valsubtype_unknown,[T,V])).
