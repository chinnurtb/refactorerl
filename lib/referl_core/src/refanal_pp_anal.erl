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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2010,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Analyser for the pretty printer (source code formatting) functionality.
%%%
%%% @author Benjamin Somhegyi <soto20@inf.elte.hu>

-module(refanal_pp_anal).
-vsn("$Rev$"). % for emacs"
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").
-include("refcore_pp.hrl").

%%% @private
schema() -> [].

%%% @private
externs(_) -> [].

%%% @private
insert(_, _, {_, Child}, _) ->
    case ?Anal:data(Child) of
        #form{pp=node} ->
            format_node(Child);
        #form{} ->
            format_children(Child);
        #clause{pp=node} ->
            format_node(Child);
        #clause{} ->
            format_children(Child);
        #expr{pp=node} ->
            format_node(Child);
        #expr{} ->
            format_children(Child);
        _ -> ok
    end.


%TODO: Do we need reformatting when remove/4 is called??
%%% @private
remove(_, _, _, _) ->
    ok.

%%% @private
update(_,_) ->
    ok.

walk(Fun, [Node | Tail]) ->
    walk(Fun, Fun(Node, ?Anal:data(Node)) ++ Tail);
walk(_, []) ->
    ok.

clear(Expr, #expr{pp=PPAttr}=Data) ->
    case PPAttr of
        node  -> ?Anal:update(Expr, Data#expr{pp=none});
        _     -> ok
    end,
    [Child || {_, Child} <- ?Anal:children(Expr)];
clear(Form, #form{pp=PPAttr}=Data) ->
    case PPAttr of
        node  -> ?Anal:update(Form, Data#form{pp=none});
        _     -> ok
    end,
    [Child || {_, Child} <- ?Anal:children(Form)];
clear(Clause, #clause{pp=PPAttr}=Data) ->
    case PPAttr of
        node  -> ?Anal:update(Clause, Data#clause{pp=none});
        _     -> ok
    end,
    [Child || {_, Child} <- ?Anal:children(Clause)];
clear(_, _) -> [].

format_node(Node) ->
    ?PP:format(Node, Node, ?PP_OPTIONS, ?PPR:erlang()),
    walk(fun clear/2, [Node]).

format_children(Node) ->
    Children = lists:filter(fun has_pp_node_set/1, ?Anal:children(Node)),
    case Children /= [] of
        true -> {_, First} = hd(Children),
                {_, Last} = lists:last(Children),
                ?PP:format(First, Last,
                            ?PP_OPTIONS,?PPR:erlang()),
                 walk(fun clear/2, [Child || {_, Child} <- Children]);
        false -> ok
    end.

has_pp_node_set({_, Node}) ->
    case ?Anal:data(Node) of
        #expr{pp=node} -> true;
        #form{pp=node} -> true;
        #clause{pp=node} -> true;
        _ -> false
    end.
