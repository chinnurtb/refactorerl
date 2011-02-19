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

%%% @doc This module provides functions to call Wrangler easier.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(referl_wrangler).
-vsn("$Rev: 2146 $").

-export([rename_function/5]).

-include("refactorerl.hrl").

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
