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

%%% @doc This module provides an interface for Emacs. It is intended to be
%%% `start'ed from the command line using `-noshell'. It reads Erlang terms
%%% from the standard input, and passes them to an interpreter function
%%% (implemented by a gen_server). Responses are written to the standard
%%% output in the form required by the Emacs RefactorErl minor mode.
%%%
%%% It is possible to run this interface on the same node as the main server,
%%% but distributed operation is also supported, when the input loop has its
%%% own dedicated Erlang node. This enables to have an Erlang shell running on
%%% the server node for development.
%%%
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(referl_emacs).
-vsn("$Rev: 5269 $").

-export([start/0]).

-export([run_server/0]).

-include("ui.hrl").
%-include_lib("referl_core/include/core_export.hrl").
-define(RefStop,1000).

%% @doc Main loop that reads Erlang terms from standard input and passes them
%% to the interpreter server.
start() ->
    case net_adm:ping(?REFERL_NODE) of
        pong ->
            process_flag(trap_exit, true),
            run_server();
        _ ->
            timer:sleep(100),
            start()
    end.

%% @private
run_server() ->
    Srv = spawn_link(fun emacs_server/0),
    receive
        {'EXIT', Srv, Reason} when Reason /= normal ->
            io:format("Server Error: ~p~n", [Reason]),
            ?MODULE:run_server()
    end.

emacs_server() ->
    register(referl_input, self()),
    monitor_node(?REFERL_NODE, true),
    Self = self(),
    referl_ui_evsend:start(Self, msg),
    receive
        {msg, installed} ->
            loop(spawn_link(fun() -> read_loop(Self) end))
    end.

loop(Inp) ->
    receive
        {msg, terminated} ->
            loop(Inp);
        {msg, {gen_event_EXIT,referl_ui_evsend,shutdown}} ->
            loop(Inp);
        {msg, Msg}    -> %global messages
%?d(Msg),
            show_msg(Msg),
            loop(Inp);
        {Inp, Term} ->
%?d(Term),
            ui_call(Term),
            loop(Inp);
        {'EXIT', Inp, Reason} ->
            io:format("Read loop error: ~p~n", [Reason]),
            Self = self(),
            Inp1 = spawn_link(fun() -> read_loop(Self) end),
            loop(Inp1);
        code_change ->
            exit(code_change);
        {nodedown, ?REFERL_NODE} ->
            init:stop();
        Msg -> %unicast messages
            show_msg(Msg),
            loop(Inp)
    end.

%% @private
read_loop(Pid) ->
    case io:read('') of
        {ok, Term} -> Pid ! {self(), Term};
        eof        ->
            Pid ! {self(), {stop}},
            timer:sleep(?RefStop);
        {error, {_, Mod, Error}} ->
            Pid ! {msg, {error,
                         lists:flatten(
                           io_lib:format("~s", [Mod:format_error(Error)]))}};
        _ -> skip
    end,
    read_loop(Pid).



%% @private
ui_call({getid}) ->
    ReqID = ?UI:getid(),
    show_msg({reqid,ReqID});

ui_call(RNA) when is_tuple(RNA) ->
    [ReqID | NameArgs] = tuple_to_list(RNA),
    case ?UI:request(ReqID,list_to_tuple (NameArgs)) of
        ok -> ok;
        error ->
            io:format("Bad server request ~512P~n", [RNA, 6])
    end.

show_msg(X) ->
%    ?d(X),
    io:format("\2~s~n",[print_elisp(X)]).

print_elisp(Tuple) when is_tuple(Tuple) ->
    ["[",
     [[" ", print_elisp(Elem)] || Elem <- tuple_to_list(Tuple)],
     "]"];
print_elisp(Float) when is_float(Float)->
    float_to_list(Float);
print_elisp(List) when is_list(List) ->
    print_string(List, []);
print_elisp(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
print_elisp(Int) when is_integer(Int) ->
    integer_to_list(Int).

print_string([], Chars) ->
    ["\"", escape(lists:reverse(Chars)), "\""];
print_string([Char | Tail], Chars) when 0 =< Char, Char =< 255 ->
    print_string(Tail, [Char | Chars]);
print_string(Rest, Chars) ->
    ["(",
     [[" ", integer_to_list(Ch)] || Ch <- lists:reverse(Chars)],
     [[" ", print_elisp(D)] || D <- Rest],
     ")"].

escape([]) ->
    [];
escape([$\" | Tail]) ->
    ["\\\"" | escape(Tail)];
escape([$\\ | Tail]) ->
    ["\\\\" | escape(Tail)];
escape([$\n | Tail]) ->
    ["\\n"  | escape(Tail)];
escape([Chr | Tail]) ->
    [Chr    | escape(Tail)].

