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

%%% @doc Defines measures for determining the fitness of a clustering result
%%% The clustering result is passed as a parameter, and it is suppesed to
%%% refer to the acual content of the database.
%%%
%%% A point can be placed on the canvas with a single click. Initial centroids
%%% can be set by clicking on the existing points. Right mouse-click removes the
%%% point from the canvas.
%%% The button `Do one step' executes one step of the algorithm, while `Jump to
%%% result' shows the result of the clustering. Clicking on `Stop clustering'
%%% stops displaying the clustering for modifying the points or the centroids.
%%% A text box allows to define the `k' number of the clusters. If neither the
%%% initial centroids, nor the number of the clusters are given, the algorithm
%%% is executed with the default `k'.
%%%
%%% @author Petra Krizsai <krizsai@inf.elte.hu>

-module(cl_2d_gui).

-vsn("$Rev: 2875 $").

-include("cluster.hrl").

-import(proplists, [get_value/2]).
-export([start/0]).

-define(CANVAS_W, 500).
-define(CANVAS_H, 500).
-define(BUTTON_W, 150).
-define(BUTTON_H, 30).
-define(RADIUS, 7).
-define(RADIUS2, 5).

%%% @type shape().
%%%
%%% A graphic entity created by the `gs' module.

%%% @type mem() = #mem{points = [point()],
%%%                    clusterings = [dict(centroid(), [entity()])] | undefined,
%%%                    attribs = matrix() | undefined,
%%%                    connections = [shape()],
%%%                    centroids = [point()]}.
%%%
%%% Represents the memory of the GUI process.
%%%
%%% Fields:
%%% <ul>
%%%     <li>`points': Points given by the user.</li>
%%%     <li>`clusterings': The list of clusterings returned by the clustering
%%%         algorithm.</li>
%%%     <li>`attribs': The attribute matrix used by the clustering
%%%         algorithm.</li>
%%%     <li>`connections': The connections between the centroids and
%%%         entities.</li>
%%%     <li>`centroids': List of centroids, each of which was calculated by one
%%%         step of the algorithm.</li>
%%%     <li>`is_fuzzy': If `is_fuzzy' is true, the selected algorithm will be
%%%         fuzzy c-means, otherwise k-means. The default algorithm is the
%%%         k-means.</li>
%%% </ul>
-record(mem, {points=[], clusterings=undefined, attribs=undefined,
              connections=[], centroids=[], is_fuzzy=false}).

%%% @type point() = #point{coord = [{positive(), positive()}],
%%%                        shapes = shape(),
%%%                        type = entity | init_centroid | centroid}.
%%%
%%% Represents a point put onto the canvas by the user.
%%%
%%% Fields:
%%% <ul>
%%%     <li>`coord': `X' and `Y' coordinates of the point.</li>
%%%     <li>`shapes': The graphical object that represents the point on the
%%%         canvas.</li>
%%%     <li>`type': The type of the point.</li>
%%% </ul>
-record(point, {coord, shapes, type}).

%% @spec start() -> ok
%%
%% @doc Starts graphic interface of clustering in a separate process.
start() ->
    spawn(fun init/0).

%% @spec init() -> ok
%%
%% @doc Starts the graphic interface of clustering.
init() ->
    create_gui(),
    loop(#mem{}).

%% @spec create_gui() -> shape()
%%
%% @doc Creates a new graphical user interface.
create_gui() ->
    Gs = gs:start(),
    WH = [{width, ?CANVAS_W + ?BUTTON_W}, {height, ?CANVAS_H}],
    gs:create(window, window ,Gs,
             [{keypress, true}, {configure, true},
              {title, "Two dimensional clustering"}, {map, true}] ++ WH),
    gs:create(frame, packer, window,
        [{packer_x, [{fixed, ?BUTTON_W}, {fixed, ?CANVAS_W}]},
         {packer_y, [{fixed, ?CANVAS_H}]}]),
    gs:create(frame, packer1, packer,
        [{packer_x, [{fixed, ?BUTTON_W}]},
         {packer_y, [{fixed, ?BUTTON_H}, {fixed, ?BUTTON_H},
                     {fixed, ?BUTTON_H}, {fixed, ?BUTTON_H},
                     {fixed, ?BUTTON_H}, {fixed, ?BUTTON_H},
                     {fixed, ?BUTTON_H}, {fixed, ?BUTTON_H},
                     {fixed, ?BUTTON_H}]},
         {pack_xy, {1, 1}}]),
    gs:create(frame, packer2, packer,
        [{packer_x, [{stretch, 1}, {fixed, ?CANVAS_W}, {stretch, 1}]},
         {packer_y, [{stretch, 1}, {fixed, ?CANVAS_H}, {stretch, 1}]},
         {pack_xy, {2, 1}}]),
    gs:create(canvas, canvas, packer2,
              [{buttonpress, true}, {bg, {255, 255, 255}}, {pack_xy, {2, 2}},
               {width, ?CANVAS_W}, {height, ?CANVAS_H}]),
    gs:create(button, button_step, packer1,
              [{label, {text, "Do one step"}}, {pack_xy, {1, 1}}]),
    gs:create(button, button_cluster, packer1,
              [{label, {text, "Jump to the result"}}, {pack_xy, {1, 2}}]),
    gs:create(button, button_stop, packer1,
              [{label, {text, "Stop clustering"}}, {pack_xy, {1, 3}}]),
    gs:create(button, button_new, packer1,
              [{label, {text, "New"}}, {pack_xy, {1, 4}}]),
    gs:create(label, label1, packer1, [{label, {text, "The value of K:"}},
              {pack_xy, {1, 5}}]),
    gs:create(entry, entry, packer1,
              [{width, ?BUTTON_W}, {keypress, true}, {pack_xy, {1, 6}}]),
    gs:create(label, label2, packer1, [{label, {text, "The value of M:"}},
              {pack_xy, {1, 7}}]),
    gs:create(entry, entry2, packer1,
              [{width, ?BUTTON_W}, {keypress, true}, {pack_xy, {1, 8}}]),
    gs:create(checkbutton, check_fuzzy, packer1,
              [{label, {text, "Fuzzy c-means"}}, {select, false},
               {pack_xy, {1, 9}}]),
    gs:config(packer,WH).

%% @spec loop(mem()) ->  ok
%%
%% @doc Handles the user actions. This function runs while the GUI is open.
loop(Mem) ->
    receive
        {gs, window, destroy, [], []} ->
            destroy_attribs(Mem);
        {gs, check_fuzzy, click, [], _} ->
            loop(Mem#mem{is_fuzzy = not Mem#mem.is_fuzzy});
        {gs, canvas, buttonpress, [], [1, X, Y | _]} ->
            PointList = Mem#mem.points,
            Point = findpoint(PointList, X, Y),
            Mem2 =
                case Point of
                    notfound ->
                        Shape = gs:create(oval, canvas, [{bw, 0}]),
                        Point2 = #point{coord={X,Y},
                                        shapes=[Shape],
                                        type=entity},
                        Mem#mem{points = [set_properties(Point2) | PointList]};
                    #point{type=entity} ->
                        Point2 = clear_properties(Point),
                        Point3 = Point2#point{type=init_centroid},
                        Mem#mem{points = [set_properties(Point3) |
                                          lists:delete(Point, PointList)]};
                    #point{type=init_centroid} ->
                        Point2 = clear_properties(Point),
                        Point3 = Point2#point{type=entity},
                        Mem#mem{points = [set_properties(Point3) |
                                          lists:delete(Point, PointList)]}
                end,
            loop(Mem2);
        {gs, canvas, buttonpress, [], [2, _, _ | _]} ->
            _PointList = Mem#mem.points,
            loop(Mem);
        {gs, canvas, buttonpress, [], [3, X, Y | _]} ->
            PointList = Mem#mem.points,
            Point = findpoint(PointList, X, Y),
            Mem2 =
                case Point of
                    notfound ->
                        Mem;
                    #point{shapes=Shapes} ->
                        [ gs:destroy(Shape) || Shape <- Shapes ],
                        Mem#mem{points = lists:delete(Point, PointList)}
                end,
            loop(Mem2);
        {gs, button_new, click, [], _} ->
            Mem2 = stop_displaying_clustering(Mem),
            Mem3 = clear_points(Mem2),
            loop(Mem3);
        {gs, button_cluster, click, [], _} ->
            case Mem#mem.points of
                [] ->
                    loop(Mem);
                _ ->
                    case call_clustering(stop_displaying_clustering(Mem)) of
                        error ->
                            loop(Mem);
                        Mem2 ->
                            [Result | _ ] = lists:reverse(Mem2#mem.clusterings),
                            Mem3 = Mem2#mem{clusterings = [Result]},
                            Mem4 = start_displaying_clustering(Mem3),
                            loop(draw_clustering(Mem4))
                    end
            end;
        {gs, button_step, click, [], _} ->
            case Mem#mem.points of
                [] ->
                    loop(Mem);
                _ ->
                    #mem{clusterings = Clusterings} = Mem,
                    Mem4 =
                        case Clusterings of
                            undefined ->
                                case call_clustering(Mem) of
                                    error ->
                                        Mem;
                                    Mem2 ->
                                        Mem3 = start_displaying_clustering(Mem2),
                                        draw_clustering(Mem3)
                                end;
                            [] ->
                                Mem;
                            [_|_] ->
                                Mem2 = clear_clustering(Mem),
                                draw_clustering(Mem2)
                        end,
                    loop(Mem4)
            end;
        {gs, button_stop, click, [], _} ->
            loop(stop_displaying_clustering(Mem));
        _Any ->
            %% io:format("event = ~w~n", [_Any]),
            loop(Mem)
    end.

%% @spec start_displaying_clustering(mem()) -> mem()
%%
%% @doc Takes the necessary measures when starting displaying clusterings.
start_displaying_clustering(Mem = #mem{clusterings=undefined}) ->
    Mem;
start_displaying_clustering(Mem) ->
    NewPoints = [ clear_properties(Point) || Point <- Mem#mem.points],
    Mem#mem{points = NewPoints}.

%% @spec stop_displaying_clustering(mem()) -> mem()
%%
%% @doc Takes the necessary measures when stopping displaying clusterings.
stop_displaying_clustering(Mem = #mem{clusterings=undefined}) ->
    Mem;
stop_displaying_clustering(Mem) ->
    Mem2 = destroy_attribs(Mem),
    NewPoints = [ set_properties(Point) || Point <- Mem#mem.points ],
    Mem3 = Mem2#mem{attribs = undefined,
                   clusterings = undefined,
                   points = NewPoints},
    clear_clustering(Mem3).

%% @spec destroy_attribs(mem()) -> mem()
%%
%% @doc Destroys the attribute matrix in the memory.
destroy_attribs(Mem = #mem{attribs = Attribs}) ->
    case Attribs of
        undefined ->
            ok;
        _ ->
            cl_matrix:delete(Attribs)
    end,
    Mem#mem{attribs = undefined}.

%% @spec clear_points(mem()) -> mem()
%%
%% @doc Clears the points from the canvas.
clear_points(Mem = #mem{points = PointList}) ->
    [ gs:destroy(Shape) || #point{shapes=Shapes} <- PointList,
                           Shape <- Shapes ],
    Mem#mem{points = []}.

%% @spec clear_clustering(mem()) -> mem()
%%
%% @doc Removes the connections and centroids from the canvas and erases the
%% `connections' and `centroids' fields of the memory.
clear_clustering(Mem) ->
    [ gs:destroy(Connection) || Connection <- Mem#mem.connections ],
    [ gs:destroy(Shape) || #point{shapes=Shapes} <- Mem#mem.centroids,
                           Shape <- Shapes ],
    Mem#mem{connections = [], centroids = []}.

%% @spec draw_clustering(mem()) -> mem()
%%
%% @doc Draws the first clustering in `mem' with its new centroids and
%% connections between centroids and entities. Every new necessary information
%% will be stored in the memory.
draw_clustering(Mem) ->
    [Head | Tail] = Mem#mem.clusterings,
    Attribs = Mem#mem.attribs,
    Mem5 =
      dict:fold(
        fun(Centroid, EntityList, Mem2) ->
            CentroidAttribs = cl_matrix:get_row(Centroid, Attribs),
            CentroidX = proplists:get_value(x, CentroidAttribs),
            CentroidY = proplists:get_value(y, CentroidAttribs),
            CentroidShape = gs:create(oval, canvas, [{bw, 0}]),
            CentroidPoint =
                #point{coord = {CentroidX, CentroidY},
                       shapes = [CentroidShape],
                       type = centroid},
            Mem3 = Mem2#mem{centroids = [set_properties(CentroidPoint) |
                                         Mem2#mem.centroids]},
            lists:foldl(
              fun({EntityX, EntityY}, Mem4) ->
                  Connection = connect_points({CentroidX, CentroidY},
                                              {EntityX, EntityY}),
                  Mem4#mem{connections = [Connection | Mem4#mem.connections]}
              end, Mem3, EntityList)
        end, Mem, Head),
   Mem5#mem{clusterings = Tail}.


%% @spec call_clustering(mem()) -> mem() | error
%%
%% @doc Calls the clustering algorithm with the properties depending on whether
%% there are centroids selected by the user.
call_clustering(Mem = #mem{points = PointList, is_fuzzy = IsFuzzy}) ->
    Attribs = create_attribs(Mem),
    Centroids = [{X,Y} || #point{type=init_centroid, coord={X,Y}} <- PointList],
    Entities  = [{X,Y} || #point{coord = {X,Y}} <- PointList],
    PropList =
        case Centroids of
            [] ->
                K = get_k(Mem),
                case length(PointList) >= K andalso K > 0 of
                    true ->
                        [{k, K}];
                    false ->
                        % We don't have enough points to select initial
                        % centroids
                        error
                end;
            [_|_] ->
                [{initcentroids, Centroids}]
        end,
    NewPropList =
        case IsFuzzy of
            true ->
                case get_m(Mem) > 1 of
                    true ->
                        PropList ++ [{m, get_m(Mem)}];
                    false ->
                        error
                end;
            false ->
                PropList
        end,
    case PropList of
        error ->
            error;
        _ ->
            {Clusterings, Attribs2} =
                call_alg(Mem, NewPropList, Attribs,Entities),
            Mem#mem{clusterings = Clusterings, attribs = Attribs2}
    end.

%% @spec get_k(mem()) -> positive()
%%
%% @doc Gets the `k' selected by the user. If it is not defined, the
%% default value will be returned.
get_k(#mem{is_fuzzy=IsFuzzy}) ->
    try
        list_to_integer(gs:read(entry, text))
    catch
        _:_->
            case IsFuzzy of
                true ->
                    proplists:get_value(k,
                                        cl_fuzzy_c_means:run_cluster_default());
                false ->
                    proplists:get_value(k, cl_kmeans:run_cluster_default())
            end
    end.

%% @spec get_m(mem()) -> float()
%%
%% @doc Gets the `m' selected by the user. If it is not defined, the default
%% value will be returned.
get_m(#mem{is_fuzzy=IsFuzzy}) ->
    try
        list_to_float(gs:read(entry2, text))
    catch
        _:_->
            case IsFuzzy of
                true ->
                    proplists:get_value(m,
                                        cl_fuzzy_c_means:run_cluster_default());
                false ->
                    ok
            end
    end.

%% @spec call_alg(mem(), proplist(), matrix(), [entity()]) ->
%%               {[clusters()], matrix()}
%%
%% @doc Calls the corresponding algorithm.
call_alg(#mem{is_fuzzy=IsFuzzy}, PropList, Attribs, Entities) ->
    case IsFuzzy of
        true ->
            cl_fuzzy_c_means:run_cluster(PropList ++
                [{distfun, fun cl_distfun:euclidian/4},
                    {stoppingcriteria, {iterations, 100}},
                    {entitylist, Entities},
                    {format, dict}], Attribs);
        false ->
            cl_kmeans:run_cluster(PropList ++
                [{distfun, fun cl_distfun:euclidian/4},
                    {mergefun, fun cl_mergefun:avg/3},
                    {stoppingcriteria, {unchanged, 100}},
                    {entitylist, Entities},
                    {format, dict}], Attribs)
    end.

%% @spec findpoint([point()], positive(), positive()) -> point() | notfound
%%
%% @doc Returns a point in the `?RADIUS' radius of the given `{X, Y}'
%% coordinates.
findpoint([], _X, _Y) ->
    notfound;
findpoint([Head | Tail], X, Y) ->
    #point{coord = {X2, Y2}} = Head,
    case abs(X2-X) =< ?RADIUS andalso abs(Y2-Y) =< ?RADIUS of
        true ->
            Head;
        false ->
            findpoint(Tail, X, Y)
    end.

%% @spec connect_points({positive(), positive()},{positive(), positive()}) ->
%%                      shape()
%%
%% @doc Connects two points with a line.
connect_points({X1, Y1}, {X2, Y2}) ->
    gs:create(line, canvas, [{coords, [{X1, Y1}, {X2, Y2}]}, {width, 1}]).

%% @spec set_properties(point()) -> shape()
%%
%% @doc Sets the properties of the point when starting "edit" mode.
set_properties(Point = #point{shapes=[Shape],
                              type=entity,
                              coord={X, Y}}) ->
    config_shape(Shape, {X, Y}, entity),
    Point;
set_properties(Point = #point{shapes=[Shape],
                              type=centroid,
                              coord={X, Y}}) ->
    config_shape(Shape, {X, Y}, centroid),
    Point;
set_properties(Point = #point{shapes=[Shape],
                              type=init_centroid,
                              coord={X, Y}}) ->
    config_shape(Shape, {X, Y}, entity),
    CentroidShape = gs:create(oval, canvas, [{bw, 0}]),
    config_shape(CentroidShape, {X, Y}, centroid),
    Point#point{shapes=[Shape, CentroidShape]}.

%% @spec clear_properties(point()) -> shape()
%%
%% @doc Sets the properties of the point when stopping "edit" mode.
clear_properties(Point = #point{type=entity}) ->
    Point;
clear_properties(Point = #point{shapes = [Shape, CentroidShape],
                                type=init_centroid}) ->
    gs:destroy(CentroidShape),
    Point#point{shapes=[Shape]}.

config_shape(Shape, {X, Y}, centroid) ->
    gs:config(Shape, {fill, blue}),
    gs:config(Shape, {coords, [{X - ?RADIUS2, Y - ?RADIUS2},
                               {X + ?RADIUS2, Y + ?RADIUS2}]});
config_shape(Shape, {X, Y}, entity) ->
    gs:config(Shape, {fill, red}),
    gs:config(Shape, {coords, [{X - ?RADIUS, Y - ?RADIUS},
                               {X + ?RADIUS, Y + ?RADIUS}]}).

%% @spec create_attribs(mem()) -> mem()
%%
%% @doc Creates an attribute matrix for the points stored in the `mem'.
create_attribs(#mem{points = Points}) ->
    M = cl_matrix:new([], [x, y], 0),
    lists:foldl(
      fun(#point{coord={X, Y}}, M2) ->
              cl_matrix:insert_new_row({X, Y}, [{x, X}, {y, Y}], M2)
      end, M, Points).
