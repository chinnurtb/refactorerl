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

%%% @doc This module calls the pretty printer for modified code parts.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_anal_pp).
-vsn("$Rev: 2546 $").
-behaviour(referl_esg).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-define(PP_OPTIONS, [{indent, check}, {space, reformat}]).

-include("refactorerl.hrl").

%% @private
init() -> [].

%% @private
insert(_, _, _, C, #form{pp=node}=D)   -> ppn(C, D);
insert(P, _, L, _, #form{pp=child})    -> ppc(P, L);
insert(N, #form{pp=node}=D, _, _, _)   -> ppn(N, D);

insert(_, _, _, C, #clause{pp=node}=D) -> ppn(C, D);
insert(P, _, L, _, #clause{pp=child})  -> ppc(P, L);
insert(N, #clause{pp=node}=D, _, _, _) -> ppn(N, D);

insert(_, _, _, C, #expr{pp=node}=D)   -> ppn(C, D);
insert(P, _, L, _, #expr{pp=child})    -> ppc(P, L);
insert(N, #expr{pp=node}=D, _, _, _)   -> ppn(N, D);

insert(_,_,_,_,_) ->
    ok.

ppn(Node, Data) ->
    referl_pp:format(Node, Node,
                     ?PP_OPTIONS, referl_pp_rules:erlang()),
    ?Graph:update(Node, clear(Data)).

ppc(Parent, Link) ->
    Children = ?Graph:path(Parent, [{Link, {pp,'==',child}}]),
    referl_pp:format(hd(Children), lists:last(Children),
                     ?PP_OPTIONS, referl_pp_rules:erlang()),
    [?Graph:update(Node, clear(?Graph:data(Node))) || Node <- Children].

clear(F=#form{})   -> F#form{pp=none};
clear(C=#clause{}) -> C#clause{pp=none};
clear(E=#expr{})   -> E#expr{pp=none};
clear(D)           -> D.

%% @private
remove(_,_,_,_,_) ->
    ok.
