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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Root analyser.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refanal_root).
-vsn("$Rev$"). % for emacs"
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").

%%% @private
schema() ->
    {ok, Mods} = application:get_env(form_anal),
    lists:flatmap(fun(M) -> M:schema() end, Mods).

%%% @private
externs(Node) ->
    {ok, Mods} = application:get_env(form_anal),
    case ?Graph:class(Node) of
        file -> [{Form, Mods} || {form, Form} <- ?Anal:children(Node)];
        form -> [{Node, Mods}];
        _    -> [{parent_form(Node), Mods}]
    end.


parent_form(Node) ->
    P = ?Anal:parent(Node),
    case ?Graph:class(P) of
        form -> P;
        _    -> parent_form(P)
    end.

%%% @private
insert(_Parent, _, {_Tag, _Child}, _) ->
    ok. %io:format("ROOT: ~p +++~s+++ ~p~n", [Parent, Tag, Child]).

%%% @private
remove(_Parent,_,{_Tag, _Child},_) ->
    ok. %io:format("ROOT: ~p ---~s--- ~p~n", [Parent, Tag, Child]).

%%% @private
update(_Node, _Data) ->
    ok. %io:format("ROOT: ~p = ~p~n", [Node, Data]).
