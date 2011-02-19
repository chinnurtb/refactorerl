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

%%% @doc This is a test module for module `cl_attr'.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(test_cl_attr).
-vsn("$Rev: 1246 $").

-export([test/0]).

-include("cluster.hrl").

%% @spec test() -> ok
%%
%% @doc It tests the modul and returns ok if the test passed.
test()->
    ok = test_library_mod(),
    ok = test_internal_fun(),
    ok = test_attribs_to_deps(),
    ok = test_attribs_to_uses().

test_library_mod() ->
    true =
        %% the module has to be dropped, because it does not have outgoing call
        cl_attr:library_mod(
          mod_1,
          []),
    true =
        %% the module has to be dropped, because it does not have outgoing call
        cl_attr:library_mod(
          mod_1,
          [{#fun_attr{mod=mod_1},1},
           {#fun_attr{mod=mod_2},0}]),
    false =
        %% the module has to be kept, because it has an outgoing call
        cl_attr:library_mod(
          mod_1,
          [{#fun_attr{mod=mod_1},0},
           {#fun_attr{mod=mod_2},1}]),
    ok.

test_internal_fun() ->
    true =
        %% the function has to be dropped, because nobody calls it who is not
        %% its container module
        cl_attr:internal_fun(
          #fun_attr{mod=mod_1},
          []),
    true =
        %% the function has to be dropped, because nobody calls it who is not
        %% its container module
        cl_attr:internal_fun(
          #fun_attr{mod=mod_1},
          [{mod_1,2},{mod_2,0}]),
    false =
        %% the function does not have to be dropped, because mod_2 calls it
        cl_attr:internal_fun(
          #fun_attr{mod=mod_1},
          [{mod_1,2},{mod_2,1}]),
    ok.

test_attrib_1() ->
    MA =
        [{a1,[{{fun_attr,lib,f,0},1}]},
         {a2,[{{fun_attr,a1,f,0},1},
              {{fun_attr,a1,g,0},1},
              {{fun_attr,lib,f,0},1}]},
         {a3,[{{fun_attr,a1,f,0},1}]},
         {a4,[{{fun_attr,a2,f,0},1},
              {{fun_attr,a3,f,0},1},
              {{fun_attr,a1,f,0},0}]}],
    ME = 0,
    cl_core:attribs(MA,ME).

test_attribs_to_deps() ->
    Attribs = test_attrib_1(),
    UsesSorted = cl_attr:attribs_to_deps(Attribs),
    ExpectedUses = [{a2,a1},{a3,a1},{a4,a2},{a4,a3}],
    cl_matrix:delete(Attribs),
    UsesSorted = ExpectedUses,
    ok.

test_attribs_to_uses() ->
    Attribs = test_attrib_1(),
    {RE,ER} = cl_attr:attribs_to_uses(Attribs),
    RES = set_dict:to_sorted_list(RE),
    ERS = set_dict:to_sorted_list(ER),
    ExpectedRES = [{a2,[a1]},{a3,[a1]},{a4,[a2,a3]}],
    ExpectedERS = [{a1,[a2,a3]},{a2,[a4]},{a3,[a4]}],
    cl_matrix:delete(Attribs),
    RES = ExpectedRES,
    ERS = ExpectedERS,
    ok.
