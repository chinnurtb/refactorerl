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

%%% @doc Generic lexical scanner module. Works with state transition tables
%%% that are generated from the syntax description. The module also provides
%%% an interface that is compatible with the previous leex-genarated scanner.
%%%
%%% == Callback function ==
%%%
%%% The scanner uses a callback mechanism to provide analysis results. A
%%% single callback function is called with each token found in the input, at
%%% the end of the input, and when an error is encountered. The callback
%%% function returns a state, which is passed on at the next invocation; this
%%% state can be used for example to accumulate the tokens, or any other
%%% purpose. The initial state must be provided at the invocation of the
%%% scanner, and the final state is returned from the scanner.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_scanner).
-vsn("$Rev: 3185 $").
-behaviour(gen_server).

%% Client exports
-export([string/3, file/3]).

%% Compatibility interface
-export([string/1, file/1, format_error/1]).


%% Environment exports
-export([start_link/1]).

%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("refactorerl.hrl").

%% =============================================================================
%% Client interface

%% @type token_fun() = (ScannerResult, State) -> State
%%       ScannerResult = {token, Type::atom(), Text::string()} |
%%                       eof |
%%                       {no_match, Text::string()} |
%%                       {error, Error}

%% @private
start_link(File) ->
    gen_server:start_link({local, ?SCANNER_SERVER}, ?MODULE, File, []).

%% @spec string(string(), token_fun(), State) -> State
%% @doc Scans `String', calls `Fun' with every token and the actual state
%% information, and returns the final state.
string(String, Fun, Start) when is_list(String)->
    call({string, String, Fun, Start}).

%% @spec file(string(), token_fun(), State) -> State
%% @doc Scans file `File', calls `Fun' with every token and the actual state
%% information, and returns the final state.
file(Name, Fun, Start) ->
    call({file, Name, Fun, Start}).

call(Request) ->
    case gen_server:call(?SCANNER_SERVER, Request) of
        {ok, Result} -> Result;
        {error, Reason} -> erlang:error(Reason)
    end.


%% =============================================================================
%% Compatibility interface

-record(scan, {tokens=[], line=1}).

%% @spec(string(string()) -> Result
%%      Result = {ok, [{Type, Line, #token{}}], Line} |
%%               {error, {Line, Mod, Error}, Line}
%%      Line = integer()
%%      Type = atom()
%%      Mod = atom()
%% @doc Scans `String', returns the tokens of the string.
string(String) ->
    referl_scanner:string(String, fun scan/2, #scan{}).

%% @spec(string(string()) -> Result
%%      Result = {ok, [{Type, Line, #token{}}], Line} |
%%               {error, {Line, Mod, Error}, Line}
%%      Line = integer()
%%      Type = atom()
%%      Mod = atom()
%% @doc Scans the contents of `File', returns the tokens of the file.
file(File) ->
    referl_scanner:file(File, fun scan/2, #scan{}).

scan({token, Type, Text}, #scan{tokens=Ts, line=Ln}) ->
    #scan{tokens = [{Type, Ln, ?Token:build(Type, Text)} | Ts],
          line   = Ln + lines(Text)};
scan(eof, #scan{tokens=Ts, line=Ln}) ->
    {ok, lists:reverse(Ts), Ln};
scan({no_match, Text}, #scan{line=Ln}) ->
    {error,
     {Ln, ?MODULE, io_lib:format("Illegal character ~c in line ~b~n",
                                 [hd(Text), Ln])},
     Ln};
scan({error, Reason}, #scan{line=Ln}) ->
    {error, {Ln, ?MODULE, Reason}, Ln}.

lines(Text) -> lines(Text, 0).

lines([$\n|Tail], Ln) -> lines(Tail, Ln+1);
lines([_|Tail], Ln)   -> lines(Tail, Ln);
lines([], Ln)         -> Ln.

%% @spec format_error(Error) -> string()
%% @doc Return the description of `Error'.
format_error(S) ->
    lists:flatten(S).


%% =============================================================================
%% Server callbacks

%% @private
init(File) ->
    case file:consult(filename:join(code:priv_dir(refactorerl), File)) of
        {ok, [DFA]}     -> {ok, convert_dfa(DFA)};
        {error, Reason} -> {stop, {File, file:format_error(Reason)}}
    end.

%% @private
handle_call({string, String, Fun, Start}, _From, DFA) ->
    try lex_string(String, DFA, Fun, Start) of
        Result -> {reply, {ok, Result}, DFA}
    catch
        error:Reason -> {reply, {error, Reason, erlang:get_stacktrace()}, DFA};
        Reason -> {reply, {error, Reason}, DFA}
    end;

handle_call({file, Name, Fun, Start}, _From, DFA) ->
    try lex_file(Name, DFA, Fun, Start) of
        Result -> {reply, {ok, Result}, DFA}
    catch
        error:Reason -> {reply, {error, Reason, erlang:get_stacktrace()}, DFA};
        Reason -> {reply, {error, Reason}, DFA}
    end;

handle_call(_, _From, S) ->
    {reply, ok, S}.


%% @private
handle_cast(_, S) ->
    {noreply, S}.

%% @private
handle_info(_, S) ->
    {noreply, S}.

%% @private
terminate(_, _) ->
    ok.

%% @private
code_change(_, S, _) ->
    {ok, S}.


convert_dfa(DFA) ->
    Acc = [{S,A} || {S, {_,A}} <- DFA],
    Trans = [list_to_tuple([action(Ch, Tr, Acc) || Ch <- lists:seq(0, 255)])
             || {_St, {Tr, _Acc}} <- lists:sort(DFA)],
    list_to_tuple(Trans).

action(Ch, Tr, Acc) ->
    case orddict:find(Ch, Tr) of
        {ok, To} ->
            {ok, A} = orddict:find(To, Acc),
            {To, A};
        error -> undefined
    end.



lex_file(Name, DFA, Fun, Start) ->
    case file:read_file(Name) of
        {ok, Data} ->
            lex_string(binary_to_list(Data), DFA, Fun, Start);
        {error, Reason} ->
            Fun({error, file:format_error(Reason)}, Start)
    end.

lex_string(String, DFA, Fun, State) ->
    case next_token(String, 0, "", none, DFA) of
        {Type, Token, Rest} ->
            lex_string(Rest, DFA, Fun, Fun({token, Type, Token}, State));
        none when String =:= "" ->
            Fun(eof, State);
        none ->
            Fun({no_match, String}, State)
    end.

next_token("", _, _, Accept, _) ->
    accept(Accept);
next_token([Head|Tail], St, Token, Accept, DFA) ->
    Trans = element(St+1, DFA),
    case element(Head+1, Trans) of
        {Next, Acc} ->
            NewToken = [Head | Token],
            NewAccept =
                if
                    Acc =:= [] -> Accept;
                    true -> {Acc, NewToken, Tail}
                end,
            next_token(Tail, Next, NewToken, NewAccept, DFA);
        undefined ->
            accept(Accept)
    end.

accept({A, Tk, Tl}) -> {A, lists:reverse(Tk), Tl};
accept(Acc) -> Acc.
