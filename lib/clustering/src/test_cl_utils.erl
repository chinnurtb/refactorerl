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

%%% @doc This is a test module for module `cl_utils'.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(test_cl_utils).
-vsn("$Rev: 1458 $").

-export([test/0]).

%% @spec test() -> ok
%% @doc It tests the modul and returns ok if the test passed.
test() ->
    ok = test_proplist_update().

test_proplist_update() ->
    [] = cl_utils:proplist_update([],[]),
    [{a,true}] = cl_utils:proplist_update([a],[]),
    [{a,true}] = cl_utils:proplist_update([],[a]),
    [{a,true}] = cl_utils:proplist_update([a],[a]),
    [{a,b}] = cl_utils:proplist_update([{a,b}],[]),
    [{a,b}] = cl_utils:proplist_update([],[{a,b}]),
    [{a,b}] = cl_utils:proplist_update([{a,b}],[{a,b}]),
    [{a,c}] = cl_utils:proplist_update([{a,b}],[{a,c}]),
    [{a,b}] = cl_utils:proplist_update([{a,c}],[{a,b}]),
    [{a,1},{c,true}] = cl_utils:proplist_update([{a,0},c],[{c,true},{a,1}]),
    [{a,0},{b,1},{c,2}] = cl_utils:proplist_update([{b,0},{a,0}],[{b,1},{c,2}]),
    [{a,0},{b,1},{c,2}] = cl_utils:proplist_update([{b,0},{a,0}],[{c,2},{b,1}]),
    ok.
