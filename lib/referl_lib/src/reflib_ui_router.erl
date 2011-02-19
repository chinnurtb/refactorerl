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

%%% @doc UI message router
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reflib_ui_router).
-vsn("$Rev: $ ").
-behaviour(gen_server).

%-compile(export_all).

%% interface
-export([getid/0, request/2, request/3]).
-export([add_msg_handler/1, del_msg_handler/1]).

%% private
-export([broadcast/2]).

%%% Server exports
-export([start_link/0]).

%% gen_server callbacks
-export([handle_call/3, handle_cast/2]).
-export([init/1, handle_info/2, terminate/2, code_change/3]).

%% Callbacks
-export([error_text/2]).


-include("lib.hrl").

%% constants
-define(T_Broadcast, global).
-define(T_Reply, reply).

-define(NoReply,   noreply).
-define(ERR(R),    {error,{R, ?Error:error_text(R)}}).
-define(LErr(R),   ?LocalErr(R,[])).
-define(LocalErr(R,L), ?ERR(?LocalError(R,L))).
-define(RefErr(R,L),   ?ERR(?RefError(R,L))).

%%% ============================================================================
%%% Error texts

error_text(bad_request, [X]) ->
    ["Bad UI request: ", ?MISC:any_to_string(X)];
error_text(exception, [Cl,Error,Stack]) ->
    ["Unhandled exception ", atom_to_list(Cl), ":", io_lib:write(Error),
    io_lib:print(Stack)];
error_text(bad_response, [X]) ->
    ["Bad UI functional backend response: ", ?MISC:any_to_string(X)].

%%% ============================================================================
%%% Error texts

% Note: you MUST NOT rely on the internal structure of a request ID
getid() ->
    gen_server:call({?MODULE, ?REFERL_NODE}, {getid}).

% Note: you MUST NOT rely on the internal structure of a request ID
request(ReqId, Request) ->
    request(self(), ReqId, Request).

% Note: you MUST NOT rely on the internal structure of a request ID
request(Pid, ReqId, Request) when is_pid(Pid) ->
    RPid = {Pid, node()},
    gen_server:call({?MODULE, ?REFERL_NODE}, {request, RPid, ReqId, Request});

% Note: you MUST NOT rely on the internal structure of a request ID
request(RPid, ReqId, Request) ->
    gen_server:call({?MODULE, ?REFERL_NODE}, {request, RPid, ReqId, Request}).

add_msg_handler(Pid) ->
    gen_server:call({?MODULE, ?REFERL_NODE}, {add_msg_handler, Pid}).

del_msg_handler(Pid) ->
    gen_server:call({?MODULE, ?REFERL_NODE}, {del_msg_handler, Pid}).

broadcast(Type, Details) ->
    gen_server:cast({?MODULE, ?REFERL_NODE}, {broadcast, Type, Details}).

%%

%% @doc Starts the server and links it with the calling process.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, []}.

%% ================
%% Implementation

handle_call({getid}, _From, State) ->
    Ref = make_ref(),
    Reply = {reqid, base64:encode_to_string(term_to_binary(Ref))}, %@todo
    {reply, Reply, State};

handle_call({request, {Pid, Node}, ReqId={reqid,S}, Request}, _From, State)
  when is_pid(Pid), is_atom(Node), is_list(S) ->
    MCB = create_callbacks(Pid, ReqId),
    Reply = run(Request, MCB),
    {reply, Reply, State};

handle_call({request, _Pid, _ReqId, _Request}, _From, State) ->
    {reply, error, State};

handle_call({add_msg_handler, HandlerPid}, _From, Handlers) ->
    HandlerPid ! installed,
    {reply, ok, [HandlerPid | Handlers]};

handle_call({del_msg_handler, Handler, _Args}, _From, Handlers) ->
    NewHandlers = lists:delete(Handler, Handlers),
    {reply, ok, NewHandlers};

handle_call(_, _, Handlers) ->
    {noreply, Handlers}.

handle_cast({broadcast, Type, Details}, Handlers) ->
    lists:foreach(fun(Pid) ->
                          Pid ! {?T_Broadcast, Type, Details}
                  end, Handlers),
    {noreply, Handlers};
handle_cast(_, Handlers) ->
    {noreply, Handlers}.

handle_info({'EXIT', Pid, _Reason}, Handlers) ->
    NewHandlers = lists:delete(Pid, Handlers),
    {noreply, NewHandlers};

%% @private
handle_info(_, S) ->
    {noreply, S}.

%% @private
terminate(_,_) ->
    ok.

%% @private
code_change(_,S,_) ->
    {ok, S}.

%% ========
%% Private implementation

create_callbacks(Pid, ReqId) ->
    UnicastFun =
        fun(Type, Details) ->
            Pid ! {ReqId, Type, Details}
        end,
    BroadcastFun =
        fun(Type, Details) ->
            ?MODULE:broadcast(Type, Details)
        end,
    #msg_cb{unicast = UnicastFun, broadcast = BroadcastFun}.

run(Request, MCB=#msg_cb{})->
    spawn(fun() ->
                  R = exec_req(Request, MCB),
                  case R of
                    ?NoReply -> ?NoReply;
                    _ ->
                        Reply =
                            case R of
                                {ok,{abort,E}} ->
                                    {ok,{abort,{E,?Error:error_text(E)}}};
                                {ok,_}->
                                    R;
                                {error,E} ->
                                    ?ERR(E);
                                _ ->
                                    ?LocalErr(bad_response, [R])
                            end,
                        (MCB#msg_cb.unicast)(?T_Reply, Reply)
                end
          end),
    ok.

exec_req(Request, MCB=#msg_cb{}) ->
    M = ?UIB,
    UI_funs = proplists:get_value(exports, M:module_info()),
    case catch tuple_to_list(Request) of
        [F | Args0] ->
            Args = [MCB | Args0],
            FunArity = {F,length(Args)},
            case lists:member(FunArity, UI_funs) of
                true ->
                    try
                        apply(M, F, Args)
                    catch
                        error:Error={_,_,_}->
                            {error, Error};
                        Cl:Error ->
                            {error,?LocalError(exception, [Cl, Error,
                                                  erlang:get_stacktrace()])}
                    end;
                false ->
                    {error,?LocalError(bad_request,[Request])}
            end;
        _ ->
            {error,?LocalErr(bad_request,[Request])}
            %@todo: or perhaps throw an exception instead?!
    end.

