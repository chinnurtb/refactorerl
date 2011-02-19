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

%%% @doc This module provides functions to call functionalities from
%%% Wrangler.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(referl_wrangler).
-vsn("$Rev: 3185 $").

-export([run_dup/1, test/0]).
-export([rename_function/5]).

-include("refactorerl.hrl").

%%---------------------------------------------------------------------------
%% Duplicated Code Detection

-define(Get(T, A), proplists:get_value(T, A)).

%% @private
test()->
    try run_dup([{dirs,["/home/guestuser/test"]},{token,2},{clone,1}])
    catch 
        throw:Reason -> io:format(Reason)
    end.

%% @spec run_dup(Args::proplist()) -> [clone()]
%% @doc Args should contain: 
%%     {dirs, [string()], {token, integer()}, {clone, integer()}}
%% Where `token' is the minimum number of tokens in a code clone, and 
%% `clone' is the minimum number of appearance time in a clone in the file.
%%
%% `clone()::[{File::string(), Start::{integer(),integer()}, 
%% End::{integer(), integer()}}]'
run_dup(Args)->
    Dirs = ?Get(dirs, Args),
    MinTok = integer_to_list(?Get(token, Args)),
    MinCl = integer_to_list(?Get(clone, Args)),
    referl_mapping_server:start(),
    case Dirs of
        [] -> D = get_files();
        _ -> D = FileNames = refac_util:expand_files(Dirs, ".erl"),
             [referl_fileman:add_file(File) || File <- FileNames]
    end, 
    Result = 
        try refac_duplicated_code:duplicated_code(D,MinTok,MinCl,8,refactorerl)
        catch
            throw:Error -> Error;
            _E1:_E2 -> 
                case D of 
                    [] -> {error, "There are no files in the database"};
                    _ -> {error, "Wrangler failed to perform this refactoring"}
                end
        end,
    case Result of
       {error, Reason} -> 
           io:format("Error: ~p ~n", [Reason]), 
           Res = [];
       {ok, []} -> 
           io:format("Wrangler did not find duplicated code freagments ~n"), 
           Res = [];
       {ok, List} -> 
%%             [[io:format("Duplicated nodes: ~w ~n", 
%%                        [[ets:lookup(ids,Id)||Id <- Ids]]) || 
%%               {_, _, Ids} <- Clones ]|| {Clones, _, _} <- List],
%% io:format("~nResultList ~p ~n", [List]),
           Res = result(List)
    end,
    referl_mapping_server:stop(),
    Res.


result(L)->
    [[{File, {L1, C1}, {L2, C2}}|| 
      {{File, L1, C1}, {File, L2, C2}, _Ids} <- Clones ] || 
       {Clones, _, _} <- L].


get_files()->
    lists:usort([?File:path(File) || 
                 File <- ?Graph:path(?Graph:root(), [file])]).
  
%%---------------------------------------------------------------------------
%% Rename Function


leader(W) ->
    Self = self(),
    receive
        {stop, _Pid} ->
            ok;
        {io_request, Sender, Self, Request} ->
            case Request of
                {put_chars,io_lib,format, [Text, List]} ->
                    cl_out:fwrite(W, Text, List);
                _ ->
                    cl_out:fwrite(W, "The printer said: ~p~n", [Request])
            end,
            Sender ! {io_reply, Self, ok},
            leader(W)
    end.

wrapper(Leader, Main, Fun) ->
    group_leader(Leader, self()),
    Result = Fun(),
    Main ! {result, self(), Result},
    ok.

redirect_output(Fun, Output) ->
    {W, C} = cl_out:open(Output),
    Main = self(),
    Leader = spawn_link(fun() -> leader(W) end),
    Wrapper = spawn_link(fun() -> wrapper(Leader, Main, Fun) end),
    receive
        {result, Wrapper, Result} -> ok
    end,
    Leader ! {stop, Main},
    cl_out:close(C),
    Result.

rename_function(FileName, {Line, Col}, NewFunName, Dirs, Output) ->
    {W, C} = cl_out:open(Output),
    RenamerFun = 
        fun() -> 
                application:start(wrangler_app),
                refac_rename_fun:rename_fun(
                    FileName, Line, Col, atom_to_list(NewFunName), Dirs)
        end,
    Result = redirect_output(RenamerFun, W),
    cl_out:close(C),
    Result.

