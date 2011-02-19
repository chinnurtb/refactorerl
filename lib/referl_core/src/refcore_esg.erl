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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Erlang semantic graph interface. This module provides a generic
%%% framework for consistent semantical analysis of Erlang syntax trees.
%%% Basically it is a bridge between the semantical analyser modules and other
%%% components that need the result of the analysis.
%%%
%%% This module's interface is similar to {@link refcore_graph}'s, in fact,
%%% most functions are wrappers above the corresponding graph operation. The
%%% purpose of this layer is to maintain consistency between syntactic and
%%% semantic structures.
%%%
%%% == Graph manipulation interface ==
%%%
%%% The ESG server has two states, <i>query</i> and <i>update</i>. In query
%%% state, semantic links are consistent with syntactic structures, so query
%%% operations ({@link path/2}, {@link links/1}, {@link index/3}) are
%%% permitted on semantic links.
%%%
%%% Modifying operations ({@link insert/3}, {@link remove/3}, {@link
%%% update/2}) work on the syntax tree, and they return immediately after
%%% modifying the graph. This invalidates semantic consistency, and the server
%%% gets into update state: analysers processes are working on restoring
%%% semantic consistency, and only syntactic queries are permitted.
%%%
%%% The transition from update to query state is performed by calling {@link
%%% finalize/0}, which is a blocking call: it waits for every analyser process
%%% to finish, and enables semantic queries again.
%%%
%%% Limitations of operations:
%%% <ul>
%%% <li> Only correct subtrees are allowed to be inserted and removed; the node
%%%   that is being modified may be left in an incorrect state temporarily,
%%%   but it must not be detached from root, and new subtrees should be
%%%   assembled completely before attaching them.</li>
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refcore_esg).
-vsn("$Rev: 5615 $").
-behaviour(gen_server).


%% Client exports
-export([create/1, insert/3, remove/3, update/2,
         root/0, data/1, links/1, path/2, index/3,
         finalize/0]).
-export([copy/1]).

%% Analyser exports
-export([anal_report/3, anal_reserve/2]).

%% gen_server exports
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("core.hrl").
-include("refcore_schema.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(ESG_TIMEOUT, infinity).

%% TODO: timeout
-define(Call(Req, Args),
        case gen_server:call(?ESG_SERVER, {Req, Args}, ?ESG_TIMEOUT) of
            ok                  -> ok;
            {ok, Reply}         -> Reply;
            {error, Error}      -> erlang:error(Error, Args);
            {apply, Fun, Final} -> try Fun() after Final() end
        end).

%%% ============================================================================
%%% Interface functions

%% @spec create(data()) -> node()
%% @doc Creates a new node. If the node is not inserted into the syntax tree,
%% it is deleted at the end of the next batch.
%% @see refcore_graph:create/1
create(Data) ->
    ?Call(create, [Data]).

%% @spec insert(node(), atom() | {atom(), integer()}, node()) -> ok
%% @doc Inserts `Child' into the syntax tree under `Parent'. The insertion may
%% be delayed if there are analysers running of the same syntax tree part.
%% When this function returns, the link is inserted into the graph, and no
%% semantic queries are possible until a call to {@link finalize/0}.
%% @see refcore_graph:mklink/3
insert(Parent, Tag, Child) ->
    ?Call(insert, [Parent, Tag, Child]).

%% @spec remove(node(), atom(), node()) -> ok
%% @doc Removes `Child', which is under `Parent', from the syntax tree. The
%% removal may be delayed if there are analysers running of the same syntax
%% tree part. When this function returns, the link is removed from the graph,
%% and no semantic queries are possible until a call to {@link finalize/0}.
%% @see refcore_graph:rmlink/3
remove(Parent, Tag, Child) ->
    ?Call(remove, [Parent, Tag, Child]).

%% @spec update(node(), data()) -> ok
%% @doc Updates the attributes of `Node'. The update may be delayed if there
%% are analysers running of the same syntax tree part. When this function
%% returns, the node is upadted, and no semantic queries are possible until a
%% call to {@link finalize/0}.
%% @see refcore_graph:update/2
update(Node, Data) ->
    ?Call(update, [Node, Data]).

%% @spec copy(node()) -> [{node(),node()}]
%% @doc Calls {@link refcore_syntax:copy/1}.
%% @deprecated Use {@link refcore_syntax:copy/1}.
copy(Node) ->
    ?Syn:copy(Node).

%% @spec root() -> node()
%% @doc Returns the root node.
%% @see refcore_graph:root/0
root() ->
    ?Graph:root().

%% @spec data(node()) -> tuple()
%% @doc Returns the attributes of a node.
%% @see refcore_graph:data/1
data(Node) ->
    %% Semantic node data queries could be blocked.
    ?Graph:data(Node).

%% @spec links(node()) -> [{atom(), node()}]
%% @doc Returns the links starting from a node. If semantic queries are
%% disabled, the result contains only syntactic links.
%% @see refcore_graph:links/1
links(From) ->
    ?Call(links, [From]).

%% @spec path(node(), path()) -> [node()]
%% @doc Evaluates a path expression. If semantic queries are disabled, the
%% path may only contain syntactic link names.
%% @see refcore_graph:path/2
path(From, Path) ->
    ?Call(path, [From, Path]).

%% @spec index(node(), atom(), node()) -> integer() | none
%% @doc Returns the index of a link. If semantic queries are disabled, `Tag'
%% must be a syntactic link tag.
%% @see refcore_graph:index/3
index(From, Tag, To) ->
    ?Call(index, [From, Tag, To]).

%% @spec finalize() -> ok
%% @doc Waits for semantic analyser processes to finish, and enables semantic
%% queries.
finalize() ->
    ?Call(finalize, []).

%% @private
anal_report(Report, Pid, Top) ->
    gen_server:cast(?ESG_SERVER, {anal_report, Report, Pid, Top}).

%% @private
anal_reserve(Top, Args) ->
    ?Call(reserve, [Top, Args]).

%%% ============================================================================
%%% Server callbacks

%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server.
start_link() ->
    gen_server:start_link({local, ?ESG_SERVER}, ?MODULE, [], []).%{debug, [trace]}]).

%% @private
init(_) ->
    {Schema, State} = init_state(),
    refcore_pp_rules:check_load(refcore_pp_rules:erlang()),
    case ?Graph:schema(Schema) of
        init ->
            {ok, State};
        match ->
            {ok, State};
        mismatch ->
            case ?autoreset_schema of
                true ->
                    %% not stopping is dangerous, but makes development easier
                    {ok, schema_mismatch};
                false ->
                    {stop, schema_mismatch}
            end
    end.

%% @private
handle_call(Req, From, State) ->
    {noreply, process_queue(queue_req(Req, From, State))}.

%% @private
handle_cast({anal_report, Report, Pid, Top}, State) ->
    {noreply, process_queue(handle_anal_report(Report, Pid, Top, State))};
handle_cast({clear_semqry, Ref}, State) ->
    {noreply, process_queue(clear_semqry(Ref, State))}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, Info}, State) ->
    {noreply, process_queue(handle_anal_down(Pid, Info, State))};
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

%%% Inserting without delegation:
%%%
%%%      ESG        TOP       OFF
%%% -ins->|          .         .
%%%       |RSV T     |         .
%%%       +--ins:t-->|         .
%%%       |          +--pull-->|
%%%       |          :<--tree--+
%%%       |          |INS      .
%%%       |<--clr:t--+         .
%%%
%%%
%%% Inserting into a subtree which should be delegated:
%%%
%%%      ESG        ROOT       TOP       OFF
%%% -ins->|          .          .         .
%%%       |RSV R     |          .         .
%%%       +--ins:r-->|          .         .
%%%       |          +----------.--pull-->|
%%%       |          :<--tree---.---------+
%%%       |<---dlg---+          .         .
%%%       |RSV T     :          |         .
%%%       +----t---->:          |         .
%%%       |          +--push--->|         .
%%%       |          |INS       |         .
%%%       |          +--ins:t-->|         .
%%%       |<--clr:r--+          |INS      .
%%%       |<--clr:t--.----------+         .
%%%
%%%
%%% Inserting subtrees to be delegated:
%%%
%%%      ESG         ROOT       TOP1       TOP2      OFF
%%% -ins->|           .          .          .         .
%%%       |RSV R      |          .          .         .
%%%       +--ins:r--->|          .          .         .
%%%       |           +----------.----------.--pull-->|
%%%       |           :<--tree---.----------.---------+
%%%       |<---dlg----+          .          .         .
%%%       |RSV T1     :          |          .         .
%%%       +----t1---->:          |          .         .
%%%       |           +--push--->|          .         .
%%%       |<---dlg----+          |          .         .
%%%       |RSV T2     :          |          |         .
%%%       +----t2---->:          |          |         .
%%%       |           +----------|--push--->|         .
%%%       |           |INS       |          |         .
%%%       |           +-ins:t1-->|          |         .
%%%       |           +----------|-ins:t2-->|         .
%%%       |<--clr:r---+          |INS       |INS      .
%%%       |<--clr:t1--.----------+          |         .
%%%       |<--clr:t2--.----------.----------+         .
%%%
%%%
%%% Removing without delegation:
%%%
%%%      ESG        TOP       OFF
%%% -rem->|          .         .
%%%       |RSV T     |         .
%%%       +--rem:t-->|         .
%%%       |          |REM      .
%%%       |          +--push-->|
%%%       |<--clr:t--+         .
%%%
%%%
%%% Removing from a subtree which should be delegated:
%%%
%%%      ESG        ROOT       TOP       OFF
%%% -rem->|          .          .         .
%%%       |RSV R     |          .         .
%%%       +--rem:r-->|          .         .
%%%       |<---dlg---+          .         .
%%%       |RSV T     :          |         .
%%%       +----t---->|          |         .
%%%       |          +--copy--->|         .
%%%       |          |REM       |         .
%%%       |          +--rem:t-->|         .
%%%       |          +----------|--push-->|
%%%       |<--clr:r--+          |REM      .
%%%       |<--clr:t--.----------+         .
%%%
%%%
%%% Removing subtrees which should be delegated:
%%%
%%%      ESG         ROOT      TOP1      TOP2      OFF
%%% -rem->|           .         .         .         .
%%%       |RSV R      |         .         .         .
%%%       +--rem:r--->|         .         .         .
%%%       |<---dlg----+         .         .         .
%%%       |RSV T1     :         |         .         .
%%%       +----t1---->:         |         .         .
%%%       |           +--copy-->|         .         .
%%%       |<---dlg----+         |         .         .
%%%       |RSV T2     :         |         |         .
%%%       +----t2---->:         |         |         .
%%%       |           +---------|--copy-->|         .
%%%       |           |REM      |         |         .
%%%       |           +-rem:t1->|         |         .
%%%       |           +---------|-rem:t2->|         .
%%%       |           +---------|---------|--push-->|
%%%       |<--clr:r---+         |REM      |REM      .
%%%       |<--clr:t1--.---------+         |         .
%%%       |<--clr:t2--.---------.---------+         .

%%% Synchronisation: when an analyser process is working, modifications must
%%% be disabled on its subtree, because that would break analysers. Every node
%%% has an associated "top node", which is the nearest node in its parent
%%% chain which has an associated analyser. When a node is updated or a link
%%% is inserted, the corresponding top node's analyser gets reserved, which
%%% disables further modifications.
%%%
%%% When a link is removed, however, the removed subtree is detached from its
%%% top node before the analysis starts. This is resolved by "remembering" the
%%% removed link for the purposes of finding the top node until the analysis
%%% is finished.
%%%
%%% Tree parts not connected to the root node are stored in a special analyser
%%% process for garbage collection. It is represented by the atom `off_tree'
%%% used as top node. This process has no analysers, so it needs no
%%% synchronisation, except for the purposes of cache consistency. However,
%%% cache updates are only permitted during analysis and during communication
%%% between processes, which is always synchronised.


-record(state, {mod    :: atom(),        % Root analyser module name
                semlnk :: [atom()],      % Semantic link names
                anals  :: ets:tab(),     % Analyser processes
                toptmp :: ets:tab(),     % Temporary top nodes
                semqry :: [reference()] | disabled,
                queue  :: queue()
               }).


init_state() ->
    {ok, Mod} = application:get_env(root_anal),
    SemSchema = Mod:schema(),
    EnvSchema = [   {root,  [{env,env}]},
                    {env,   record_info(fields, env),   []}
                ],
    Schema = lists:foldl(fun add_schema/2, ?SYNTAX_SCHEMA,
                         ?LEXICAL_SCHEMA ++ EnvSchema ++ SemSchema),

    {Schema, #state{mod    = Mod,
                    semlnk = link_names(SemSchema),
                    anals  = init_anal(),
                    toptmp = ets:new(toptmp, []),
                    semqry = [],
                    queue  = queue:new()}}.

add_schema(Part = {_Class, _Attribs, _Links}, Schema) ->
    [Part | Schema];
add_schema({Class, Links}, Schema) ->
    lists:map(
      fun({C, Att, Lnk}) when C == Class ->
              {C, Att, Links ++ Lnk};
         (Part) -> Part
      end, Schema).

link_names(Schema) ->
    [L || Links <- [Lnk || {_, _, Lnk} <- Schema] ++
                   [Lnk || {_,    Lnk} <- Schema],
          {L,_} <- Links].

%%% ----------------------------------------------------------------------------
%%% Queue handling

%% Add a request to the end of the queue, or return the permission to run it
%% if no queueing is needed
queue_req(Req, From, #state{queue=Que}=St) ->
    case req_type(Req, St) of
        {direct, Fun} ->
            gen_server:reply(From, {apply, Fun, fun() -> ok end}),
            St;
        queue ->
            St#state{queue = queue:in({Req, From}, Que)};
        front ->
            St#state{queue = queue:in_r({Req, From}, Que)}
    end.

req_type({index, [From, Tag, To]}, #state{semlnk=SL}) ->
    case has_semlink(Tag, SL) of
        true -> queue;
        false -> {direct, fun() -> ?Graph:index(From, Tag, To) end}
    end;
req_type({path, [From, Path]}, #state{semlnk=SL}) ->
    case has_semlink(Path, SL) of
        true -> queue;
        false -> {direct, fun() -> ?Graph:path(From, Path) end}
    end;
req_type({reserve, _}, _) ->
    front;
req_type(_, _) ->
    queue.


%% Execute the elements of the queue if possible
process_queue(#state{queue=Que}=St) ->
    case queue:out(Que) of
        {empty, _} ->
            St;
        {{value, {Req, From}}, Q1} ->
            case execute(Req, St) of
                {ok, Reply, St1} ->
                    gen_server:reply(From, Reply),
                    process_queue(St1#state{queue=Q1});
                wait ->
                    St
            end
    end.


add_semqry(#state{semqry=SemQ}=St) when is_list(SemQ) ->
    Id = make_ref(),
    Close = fun() -> gen_server:cast(?ESG_SERVER, {clear_semqry, Id}) end,
    {Close, St#state{semqry=[Id|SemQ]}}.

clear_semqry(Id, #state{semqry=SemQ}=St) when is_list(SemQ) ->
    St#state{semqry=SemQ -- [Id]}.


%%% ----------------------------------------------------------------------------
%%% Execution

%% Execute a request

-spec execute(any(), #state{}) -> {ok, _, #state{}} | wait.

execute({create, [_]}=Req, #state{}=St) ->
    case is_enabled(off_tree, St) of
        false -> wait;
        true -> exec_anal(off_tree, Req, St)
    end;

execute({Fun, [Node | _]}=Req, #state{semqry=SemQ}=St)
  when Fun == insert; Fun == remove; Fun == update ->
    Top = top_node(Node, St),
    case is_enabled(Top, off_tree, St) of
        true when Top == off_tree; SemQ == disabled; SemQ == [] ->
            exec_anal(Top, Req, St);
        true  -> wait;
        false -> wait
    end;

execute({links, [From]}, #state{semqry=disabled, semlnk=SL}=St) ->
    {ok, {apply, fun() -> synlinks(From, SL) end, fun() -> ok end}, St};
execute({links, [From]}, #state{}=St) ->
    {Close, St1} = add_semqry(St),
    {ok, {apply, fun() -> ?Graph:links(From) end, Close}, St1};

execute({Fun, Args}, #state{semqry=SemQ}=St) when Fun == index; Fun == path ->
    if
        SemQ == disabled ->
            {ok, {error, semantic_link}, St};
        true ->
            {Close, St1} = add_semqry(St),
            {ok, {apply, fun() -> apply(refcore_graph, Fun, Args) end, Close}, St1}
    end;

execute({finalize, []}, #state{semqry=Q}=St) ->
    case dispatch_final(St) of
        finish when Q == disabled -> {ok, ok, St#state{semqry=[]}};
        finish                    -> {ok, ok, St};
        wait                      -> wait
    end;

execute({reserve, [Top, Args]}, #state{}=St) ->
    start_anal(Top, Args, St),
    case is_enabled(Top, St) of
        true  -> {ok, {ok, reserve_anal(Top, St)}, St};
        false -> wait
    end.


exec_anal(Top, Req, St) ->
    {Prepare, Apply, Dispatch} = req_funs(Req),
    Prepare(Top, St),
    try Apply() of
        Result ->
            St1 = Dispatch(Top, Result, St),
            {ok,
             {ok, Result},
             if
                 Top == off_tree -> St1;
                 true            -> St1#state{semqry=disabled}
             end}
    catch
        error:Error -> {ok, {error, Error}, St}
    end.

req_funs({insert, [Parent, Tag, Child]}) ->
    {fun(Top, St)    -> cache_node(Top, Parent, St),
                        cache_node(Top, Child, St) end,
     fun()           -> ?Graph:mklink(Parent, Tag, Child) end,
     fun(Top, _, St) -> dispatch_insert(Top, Parent, Tag, Child, St) end};

req_funs({remove, [Parent, Tag, Child]}) ->
    {fun(Top, St)    -> cache_node(Top, Parent, St),
                        cache_node(Top, Child, St) end,
     fun()           -> ?Graph:rmlink(Parent, Tag, Child) end,
     fun(Top, _, St) -> dispatch_remove(Top, Parent, Tag, Child, St) end};

req_funs({update, [Node, Data]}) ->
    {fun(Top, St)    -> cache_node(Top, Node, St) end,
     fun()           -> ?Graph:update(Node, Data) end,
     fun(Top, _, St) -> dispatch_update(Top, Node, Data, St) end};

req_funs({create, [Data]}) ->
    {fun(_, _)          -> ok end,
     fun()              -> ?Graph:create(Data) end,
     fun(Top, Node, St) -> dispatch_create(Top, Node, Data, St) end}.


is_enabled(First, Second, St) ->
    is_enabled(First, St) andalso (First =:= Second orelse is_enabled(Second, St)).

%% Finalize all analysers
dispatch_final(#state{}=St) ->
    {Frees, Busys} = anal_states(St),
    case Busys of
        [] ->
            case Frees of
                [] ->
                    ?NodeSync:clean(),
                    finish;
                [off_tree] ->
                    dispatch_finalize(off_tree, St),
                    wait;
                Lst ->
                    [dispatch_finalize(Top, St) || Top <- Lst, Top =/= off_tree],
                    wait
            end;
        _ ->
            wait
    end.

synlinks(From, Filter) ->
    [Lnk || {Tag, _}=Lnk <- ?Graph:links(From),
            not lists:member(Tag, Filter)].


%%% ----------------------------------------------------------------------------
%%% Analysis dispatchers

-record(anal, {top           :: tuple() | off_tree,
               pid           :: pid(),
               free  = false :: boolean()}).

init_anal() ->
    ets:new(analysers, [{keypos, #anal.top}]).

dispatch_insert(Top, Parent, Tag, Child, St) ->
    TopA = reserve_anal(Top, St),
    SrcA = if Top =:= off_tree -> local;
              true             -> unrsvd_anal(off_tree, St)
           end,
    ?Anal:insert(TopA, SrcA, Parent, Tag, Child),
    St.

dispatch_remove(Top, Parent, Tag, Child, St) ->
    TopA = reserve_anal(Top, St),
    TrgA = if Top =:= off_tree -> local;
              true             -> save_toptmp(Child, Top, St),
                                  unrsvd_anal(off_tree, St)
           end,
    ?Anal:remove(TopA, TrgA, Parent, Tag, Child),
    St.

dispatch_update(Top, Node, Data, St) ->
    ?Anal:update(reserve_anal(Top, St), Node, Data),
    St.

dispatch_create(Top, Node, Data, St) ->
    ?Anal:create(unrsvd_anal(Top, St), Node, Data),
    St.

dispatch_finalize(Top, St) ->
    ?Anal:finalize(unrsvd_anal(Top, St)),
    St.

cache_node(Top, Node, St) ->
    ?Anal:cache(unrsvd_anal(Top, St), Node).

%%% ----------------------------------------------------------------------------
%%% Analyser control

reserve_anal(Top, #state{anals=Anals}=St) ->
    Anal = #anal{pid=Pid, free=true} = find_anal(Top, St),
    if
        Top == off_tree -> {Pid, none};
        true ->
            ets:insert(Anals, Anal#anal{free=false}),
            {Pid, Top}
    end.

unrsvd_anal(Top, St) ->
    #anal{pid=Pid, free=true} = find_anal(Top, St),
    {Pid, none}.

save_toptmp(Node, Top, #state{toptmp=Tops}) ->
    ets:insert(Tops, {Node, Top}).

start_anal(Top, Args, #state{anals=Anals}) ->
    case ets:lookup(Anals, Top) of
        [Anal] -> Anal;
        [] ->
            Pid = ?Anal:start([{top, Top} | Args]),
            erlang:monitor(process, Pid),
            Anal = #anal{top=Top, pid=Pid},
            ets:insert(Anals, Anal),
            Anal
    end.

new_anal(Anals, Where, Params) ->
    TP = ?Anal:start([{top, Where}, {mods, Params}]),
    erlang:monitor(process, TP),
    T = #anal{top=Where, pid=TP},
    ets:insert(Anals, [T]),
    T.

find_anal(Top, #state{mod=Mod, anals=Anals}) ->
    case ets:lookup(Anals, Top) of
        [Anal] -> Anal;
        [] ->
            Root = ?Graph:root(),

            case ets:lookup(Anals, off_tree) of
                [OT] -> OT;
                []   -> OT = new_anal(Anals, off_tree, [])
            end,
            RT = new_anal(Anals, Root,     [Mod]),

            case Top of
                off_tree -> OT;
                _        -> RT
            end
    end.

handle_anal_report(reg, Pid, Top, #state{anals=Anals}=St) ->
    free_top_anal(Anals, Pid, Top),
    St;
handle_anal_report(clear, Pid, Top, #state{anals=Anals, toptmp=Tops}=St) ->
    remove_top(Tops, Top),
    ?Anal:finalize({Pid, St}),
    remove_top_anal(Anals, Top),
    St.

handle_anal_down(Pid, Info, #state{anals=Anals, toptmp=Tops}=St) ->
    #anal{top=Top} = Anal = get_anal_top(Anals, Pid),
    case Info of
        normal -> ets:delete(Anals, Top);
        _      -> ets:insert(Anals, Anal#anal{pid=killed, free=false})
    end,
    remove_top(Tops, Top),
    St.

%% Returns the analyser.
get_anal_top(Anals, Pid) ->
    DPQ = ets:fun2ms(fun(#anal{pid=P} = A) when Pid =:= P -> A end),
    [Anal] = ets:select(Anals, DPQ),
    Anal.


%% Sets the `Top' analyser's pid to `Pid' and frees it.
free_top_anal(Anals, Pid, Top) ->
    [Anal] = ets:lookup(Anals, Top),
    ets:insert(Anals, Anal#anal{pid=Pid, free=true}).

remove_top_anal(Anals, Top) ->
    [Anal] = ets:lookup(Anals, Top),
    ets:delete(Anals, Anal).

%% Removes `Top' from the set of `Tops'.
remove_top(Tops, Top) ->
    DTQ = ets:fun2ms(fun({_,T}) when T == Top -> true end),
    ets:select_delete(Tops, DTQ).

is_enabled(Top, #state{}=St) ->
    #anal{free=Free} = find_anal(Top, St),
    Free.

anal_states(#state{anals=Anals}) ->
    ets:foldl(
      fun
          (#anal{top=Top, free=true},  {Free, Busy}) -> {[Top|Free], Busy};
          (#anal{top=Top, free=false}, {Free, Busy}) -> {Free, [Top|Busy]}
      end,
      {[], []},
      Anals).

%%% ----------------------------------------------------------------------------
%%% Utilities

-spec top_node(tuple(), #state{}) -> tuple() | off_tree.
top_node(Node, #state{anals=Anals, toptmp=Tops} = St) ->
    case ets:lookup(Anals, Node) of
        [] ->
            case ets:lookup(Tops, Node) of
                [{Node, Top}] -> Top;
                [] -> parent_top(Node, St)
            end;
        [_] -> Node
    end.

parent_top(Node, St) ->
    case ?Syn:parent(Node) of
        [] ->
            case Node == ?Graph:root() of
                true -> Node;
                false -> off_tree
            end;
        [{_, Parent} | _] -> top_node(Parent, St)
    end.

has_semlink({intersect, _, Link}, Lst) ->
    has_semlink(Link, Lst);
has_semlink([{seq, P1, P2}], Lst) ->
    has_semlink(P1, Lst) orelse has_semlink(P2, Lst);
has_semlink({seq, P1, P2}, Lst) ->
    has_semlink(P1, Lst) orelse has_semlink(P2, Lst);
has_semlink({seq, Path}, Lst) when is_list(Path) ->
    lists:any(fun (E) -> has_semlink(E, Lst) end, Path);
has_semlink({all, P1, P2}, Lst) ->
    has_semlink(P1, Lst) orelse has_semlink(P2, Lst);
has_semlink({any, P1, P2}, Lst) ->
    has_semlink(P1, Lst) orelse has_semlink(P2, Lst);

has_semlink(F, _Lst) when is_function(F) ->
    true;
has_semlink(Atom, Lst) when is_atom(Atom) ->
    lists:member(Atom, Lst);
has_semlink({Link, _}, Lst) ->
    has_semlink(Link, Lst);
has_semlink(Path, Lst) when is_list(Path) ->
    lists:any(fun (E) -> has_semlink(E, Lst) end, Path).
