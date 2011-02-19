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

%%% ============================================================================
%%% Module information

%%% @doc

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Zsofia Arvay <zsofia.arvay@gmail.com>

-module(refusr_layer).
-vsn("$Rev: 5749 $").

-export([check_layered_architecture/1]).

-include("user.hrl").

%% @doc Checks whether the given three lists of modul names, defining
%% architecture layers do not insult the hierarchy with function calls.
check_layered_architecture(List)->
    ModList = [[?Query:exec(?Mod:find(Mod)) || Mod<-List_E] || List_E <- List],
    [FunList1, FunList2, FunList3] = 
        [?Query:exec(Mod, ?Mod:locals()) || Mod<-ModList],
    FunList = 
        [FunList1, FunList2, FunList3],
    [FunCall1, FunCall2, FunCall3] = 
        [?Query:exec(Fun, ?Fun:funcalls()) || Fun<-FunList],
    
    [check_disabled_functions(FunCall1, lists:merge(FunList2, FunList3)), 
     check_disabled_functions(FunCall2, FunList3), 
     check_disabled_functions(FunCall3, FunList1)].

check_disabled_functions([], _)->ok;
check_disabled_functions(_, [])->ok;
check_disabled_functions([Fun|Tail], DisabledFunctions)->
    case lists:member(Fun, DisabledFunctions) of
        false->check_disabled_functions(Tail, DisabledFunctions);
        true->error
    end.
