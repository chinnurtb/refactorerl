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

%%% @doc This is a test module for module `cl_distfun'.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(test_cl_distfun).
-vsn("$Rev: 1904 $").

-export([test/0]).

-include("cluster.hrl").

%% @spec test() -> ok
%% @doc It tests the modul and returns ok if the test passed.
test() ->
    ok = test_modcall_cnt(),
    ok = test_used_records().

test_modcall_cnt() ->

% TODO modcall_cnt function cannot be found
%
%    Mod1_f = #fun_attr{mod=mod_1,name=f,arity=0},
%    Mod1_g = #fun_attr{mod=mod_1,name=g,arity=1},
%    Mod2_g = #fun_attr{mod=mod_2,name=g,arity=2},
%    Mod2_h = #fun_attr{mod=mod_2,name=h,arity=2},
%
%    [] =
%        cl_distfun:modcall_cnt([{Mod1_f,0},{Mod1_g,0},{Mod2_g,0},{Mod2_h,0}],1),
%    [{mod_1,2}] =
%        cl_distfun:modcall_cnt([{Mod1_f,1},{Mod1_g,1},{Mod2_g,0},{Mod2_h,0}],1),
%    [{mod_2,2}] =
%        cl_distfun:modcall_cnt([{Mod1_f,0},{Mod1_g,0},{Mod2_g,2},{Mod2_h,3}],1),
%    [{mod_1,2},{mod_2,1}] =
%        cl_distfun:modcall_cnt([{Mod1_f,1},{Mod1_g,2},{Mod2_g,3},{Mod2_h,0}],1),
    ok.

test_used_records() ->
    Rec1 = #rec_attr{file=file_1, name=rec_1},
    Rec2 = #rec_attr{file=file_1, name=rec_2},
    Rec3 = #rec_attr{file=file_3, name=rec_2},
    Rec4 = #rec_attr{file=file_2, name=rec_4},
    Fun  = #fun_attr{mod=mod_1,name=f,arity=0},

    [Rec1,Rec2,Rec3] =
        cl_distfun:used_records([{Rec2,1},{Fun,3},{Rec3,3},{Rec4,0},{Rec1,2}]),
    ok.

