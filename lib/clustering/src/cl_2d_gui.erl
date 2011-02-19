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
%%% LorÃ¡nd University. All Rights Reserved.

%%% @doc Defines measures for determining the fitness of a clustering result
%%% The clustering result is passed as a parameter, and it is suppesed to
%%% refer to the acual content of the database.
%%%
%%% @author Petra Krizsai <krizsai@inf.elte.hu>

-module(cl_2d_gui).

-vsn("$Rev: 2616 $").

-include("cluster.hrl").

-import(proplists, [get_value/2]).
-export([start/0]).

-define(CANVAS_W, 250).
-define(CANVAS_H, 250).
-define(BUTTON_W, 150).
-define(BUTTON_H, 30).

-record(mem,{points}).

%% @spec start() -> ok
%%
%% @doc Starts graphic interface for clustering algorithm.
start() ->
    spawn(fun init/0).

init() ->
    create_gui(),
    Mem = #mem{points=[]},
    loop(Mem).

create_gui() ->
    Gs = gs:start(),
    WH = [{width, ?CANVAS_W + ?BUTTON_W}, {height, ?CANVAS_H}],
    gs:create(window, window ,Gs,
             [{keypress,true}, {configure,true},
              {title,"Two dimensional clustering"},{map,true}]++WH),
    gs:create(frame, packer, window,
        [{packer_x,[{fixed,?BUTTON_W},{fixed,?CANVAS_W}]},
         {packer_y,[{fixed,?CANVAS_H}]}]),
    gs:create(frame, packer1, packer,
        [{packer_x, [{fixed, ?BUTTON_W}]},
         {packer_y, [{fixed, ?BUTTON_H}, {fixed, ?BUTTON_H},
                     {fixed, ?BUTTON_H}, {fixed, ?BUTTON_H}]},
         {pack_xy, {1,1}}]),
    gs:create(frame, packer2, packer,
        [{packer_x, [{stretch, 1}, {fixed, ?CANVAS_W}, {stretch, 1}]},
         {packer_y, [{stretch, 1}, {fixed, ?CANVAS_H}, {stretch, 1}]},
         {pack_xy, {2,1}}]),
    gs:create(canvas, canvas, packer2,
              [{buttonpress, true}, {bg, {255, 255, 255}}, {pack_xy, {2, 2}},
               {width, ?CANVAS_W}, {height, ?CANVAS_H}]),
    gs:create(button, button1, packer1,
              [{label, {text, "Do one step"}}, {pack_xy, {1, 1}}]),
    gs:create(button, button2, packer1,
              [{label, {text, "Do clustering"}}, {pack_xy, {1, 2}}]),
    gs:create(button, button3, packer1,
              [{label, {text, "Modify centroids"}}, {pack_xy, {1, 3}}]),
    gs:create(button, button4, packer1,
              [{label, {text, "New"}}, {pack_xy, {1, 4}}]),
    gs:config(packer,WH).

loop(Mem) ->
    receive
        {gs, _, keypress, [], [Key_symbol|_]}
            when (Key_symbol=='Escape') or (Key_symbol=='q') ->
            gs:destroy(window);
        {gs, window, destroy, [], []} ->
            gs:destroy(window);
        {gs, canvas, buttonpress, [], [_, X, Y | _]} ->
            Mem2 = Mem#mem{points = [{X, Y} | Mem#mem.points]},
            loop(Mem2);
        Any ->
            io:format("event = ~w~n", [Any]),
            loop(Mem)
    end.
