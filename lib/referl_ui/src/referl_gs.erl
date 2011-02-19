
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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2010,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc todo
%%%
%%% @todo author

-module(referl_gs).
-svn("$Rev$").

-export([start/0, stop/0]).

-include("ui.hrl").
-include("gs.hrl").

-define(Options, gs_options).
-define(Server, referl_gs_server).
-define(Watch, gs_ui_watcher).

%% @doc Starts the Erlang GS graphical refactoring browser user interface
%% and opens its main window.
start() ->
    case whereis(?Server) of
	undefined ->
	    register(?Server, spawn(fun() -> start_spawn() end)),
	    ok;
	_ ->
	    already_started
    end.

%% @doc Shuts down the Erlang GS graphical interface and closes its windows.
stop() ->
    terminate().

start_spawn() ->
    gs:start(),
    ?Config:start_link(),
    referl_ui_evsend:start(watch_start()),
    gs:create(window, win, gs:start(),
	      [{x, 30}, {y, 30},
	       {width, 800}, {height, 600},
	       {map, true}, {configure, true}]),
    gs:create(frame, frame, win, []),
    gs:create(canvas, tree, win,
	      [{bg, ?Config:color(tree, background)},
	       {vscroll, right}, {hscroll, bottom}]),
    gs:config(tree, {buttonpress, true}),
    gs:create(label, info_label, win,
	      [{label, {text, "Information:"}},
	       {justify, left}]),
    gs:create(editor, info, win,
	      [{enable, false}, {wrap, none},
	       {vscroll, right}, {hscroll, bottom}]),
    gs:create(label, text_label, win,
	      [{label, {text, "Source code:"}},
	       {justify, left}]),
    gs:create(editor, text, win,
	      [{enable, false}, {wrap, none},
	       {vscroll, right}, {hscroll, bottom}]),
    set_sizes(),
    window(?Sup:root(), []).

window(Root, History) ->
    Children = ?Sup:children(Root),
    gs:config(win, {title, title_text(Root, History)}),
    gs:config(tree, [{data, Root}]),
    draw_buttons(Root),
    draw_tree({Root, Children}),
    write_info(Root),
    write_text(Root),
    loop(History).

-define(P(Number, Percent), trunc(Number * Percent)).

set_sizes() ->
    Width = gs:read(win, width),
    Height = gs:read(win, height),
    gs:config(frame, [{x, 0}, {y, 0}, {width, Width}, {height, 100}]),
    gs:config(tree, [{x, 10}, {y, 100},
		     {width, ?P(Width, 0.55)}, {height, Height - 100 - 10}]),
    gs:config(info_label, [{x, ?P(Width, 0.6)}, {y, 100}]),
    gs:config(info, [{x, ?P(Width, 0.6)}, {y, 130},
	     {width, Width - ?P(Width, 0.6) - 10},
		     {height, ?P(Height, 0.2)}]),
    gs:config(text_label, [{x, ?P(Width, 0.6)}, {y, 130 + ?P(Height, 0.2)}]),
    gs:config(text, [{x, ?P(Width, 0.6)}, {y, 160 + ?P(Height, 0.2)},
		     {width, Width - ?P(Width, 0.6) - 10},
		     {height, Height - 160 - ?P(Height, 0.2) - 10}]).

loop(History) ->
    receive
	{gs, win, destroy, _, _} ->
	    terminate(),
	    stopped;
 	{gs, tree, buttonpress, _, [1, X, Y | _]} ->
 	    Obj = gs:read(tree, {hit, {X, Y}}),
	    Datas = [ A || A <- [ gs:read(B, data) || B <- Obj ], A /= []],
	    case Datas of
		[] -> loop(History);
		[{back, _} | _] ->
		    case History of
			[] -> loop(History);
			[Parent | Tail] ->
			    destroy_children(),
			    window(Parent, Tail)
		    end;
		[{new, Tag} | _]->
		    Root = gs:read(tree, data),
		    destroy_children(),
		    window(Tag, [Root | History])
	    end;
	{gs, _, click, {button, Tr}, _} ->
	    tr_window(Tr),
	    case Tr#tr.map of
		false ->
		    {New, NewHistory} = reload(Tr#tr.back, History),
		    window(New, NewHistory);
		true ->
		    loop(History)
	    end;
	{gs, _, click, {tr_ok, Tr}, _} ->
	    tr_ok(Tr),
	    {New, NewHistory} = reload(Tr#tr.back, History),
	    window(New, NewHistory);
	{gs, _, click, tr_cancel, _} ->
	    gs:destroy(trwin),
	    loop(History);
	{gs, _, click, errorbutton, _} ->
	    gs:destroy(errorwin),
	    loop(History);
	{gs, win, configure, _, _} ->
	    set_sizes(),
	    loop(History);
	reload ->
	    {New, NewHistory} = reload(false, History),
	    window(New, NewHistory);
	Msg ->
	    ?DebugThis(Msg),
	    loop(History)
    end.

terminate() ->
    gs:destroy(win),
    watch_stop(),
    ?Config:stop(),
    unregister(?Server).

reload(undo, _) ->
    {?Sup:root(), []};
reload(Back, History) ->
    case Back of
	true -> Now = [];
	false -> Now = [gs:read(tree, data)]
    end,
    destroy_children(),
    ?Sup:find_node(Now ++ History).


destroy_children() ->
    gs:config(info, {enable, true}),
    gs:config(info, clear),
    gs:config(info, {enable, false}),
    gs:config(text, {enable, true}),
    gs:config(text, clear),
    gs:config(text, {enable, false}),
    ToDestroy = gs:read(frame, children) ++ gs:read(tree, children),
    lists:foreach(fun(X) -> gs:destroy(X) end, ToDestroy).

-define(u(UD),
       case UD of
	   undefined -> "";
	   UDElse -> ?MISC:to_list(UDElse)
       end).

title_text(Tag, History) ->
    lists:flatten([ ?MISC:to_list(X#tag.class) ++ "(" ++
		    ?u(X#tag.name) ++ ") / " ||
		      X <- lists:reverse(History) ]) ++
	?MISC:to_list(Tag#tag.class) ++ "(" ++
	?u(Tag#tag.name) ++ ")".

tr_window(Tr) ->
    case Tr#tr.map of
	true ->
	    Height = 40 + length(Tr#tr.prop) * 40 + 40,
	    gs:create(window, trwin, win,
		      [{title, Tr#tr.label}, {map, true},
		       {x, 300}, {y, 300},
		       {width, 300}, {height, Height}]),
	    gs:create(label, trlabel, trwin,
		      [{width, 300}, {height, 30},
		       {label, {text, Tr#tr.desc}},
		       {font, {times, [bold], 20}}]),
	    lists:foreach(
	      fun({{entry, Name, Desc}, X}) ->
		      gs:create(label, trwin,
				[{label, {text, Desc}},
				 {x,10}, {y, 40 + (X-1) * 40}]),
		      L = length(Desc) * 8 + 20,
		      gs:create(entry, Name, trwin,
				[{data, Name}, {x, L},
				 {y, 40 + (X-1) * 40}]);
		 (_) -> void
	      end,
	      lists:zip(Tr#tr.prop,
			?MISC:seq2(1, length(Tr#tr.prop)))),
	    Y = 40 + length(Tr#tr.prop) * 40,
	    gs:create(button, trwin, [{label, {text, "OK"}},
				      {data, {tr_ok, Tr}},
				      {x, 10}, {y, Y}]),
	    gs:create(button, trwin, [{label, {text, "Cancel"}},
				      {data, tr_cancel},
				      {x, 130}, {y, Y}]);
	false ->
	    do_transform(Tr#tr.kind, Tr#tr.func, [])
    end.

tr_ok(Tr) ->
    Args = lists:map(
	     fun({entry, Name, _}) ->
		     {Name, gs:read(Name, text)}
	     end, Tr#tr.prop),
    gs:destroy(trwin),
    do_transform(Tr#tr.kind, Tr#tr.func, Args).

do_transform(undo, Fun, Args) ->
    Fun(Args),
    receive
	{uifinished, undo} -> void
    end;
do_transform(transform, Fun, Args) ->
    Fun(Args),
    receive
	{status, Status} -> void
    end,
    receive
	trfinished ->
	    case Status of
		"Finished" -> ok;
		ERROR -> errorwindow(ERROR)
	    end
    end.

errorwindow(ERROR) ->
    gs:create(window, errorwin, win,
	      [{x, 300}, {y, 300}, {height, 150},
	       {map, true}, {title, "ERROR"}]),
    gs:create(label, errorwin,
	      [{x, 0}, {y, 0}, {width, 300}, {height, 100},
	       {align, center}, {justify, center},
	       {label, {text, ERROR}}]),
    gs:create(button, errorwin,
	      [{x, 100}, {y, 100},
	       {label, {text, "OK"}}, {data, errorbutton}]).

draw_tree(Tree) ->
    {Root, Children} = Tree,
    ChildNum = length(Children),
    Height = gs:read(tree, height),
    Width = gs:read(tree, width),
    N = trunc((Height - 20) / 64),
    if(ChildNum > N) ->
	    gs:config(tree, [{scrollregion,
			      {0, 0, Width, length(Children) * 70}}]);
      (true) ->
	    gs:config(tree, [{scrollregion,
			      {0, 0, Width - 30 , Height - 30}}])
    end,
    lists:foreach(
      fun(Y) ->
	      P = Y * 60,
	      gs:create(line, tree,
			[{coords,
			  [{30, -30 + P}, {30, 35 + P}, {80, 35 + P}]},
			 {width, 2}])
      end,
      ?MISC:seq2(1, length(Children))),
    draw_node(Root, 20, 20, back),
    lists:foreach(
      fun({Name, Y}) ->
	      draw_node(Name, 80, Y + 20, new)
      end,
      lists:zip(Children,
		[ Z * 60 ||
		    Z <- ?MISC:seq2(1, length(Children))])).

-define(text_length(String), length(String) * 8 + 14).

draw_node(Tag, X, Y, To) ->
    Order = ?Config:order(Tag#tag.class),
    TagList = ?MISC:record_to_proplist(Tag, record_info(fields, tag)),
    OList = [ proplists:lookup(T, TagList) || T <- Order ],
    draw_recs(X, Y, To, OList, Tag).

draw_recs(_, _, _, [], _) -> done;
draw_recs(X, Y, To, [{_, undefined} | OList], Tag) ->
    draw_recs(X, Y, To, OList, Tag);
draw_recs(X, Y, To, [{Attr, Text} | OList], Tag) ->
    {BG, FG} = ?Config:color(Attr, Text),
    RecWidth = case Attr of
		   name -> ?text_length(Text);
		   _ -> ?Config:size(Attr)
	       end,
    gs:create(rectangle, tree,
	      [{coords, [{X , Y},
			 {X + RecWidth, Y + 26}]},
	       {fill, BG}, {bw, 2},
	       {data, {To, Tag}}]),
    gs:create(text, tree,
	      [{coords, [{X + 5, Y + 5}]},
	       {fg, FG},
	       {text, Text}]),
    draw_recs(X + RecWidth, Y, To, OList, Tag).

write_info(Tag) ->
    Text = lists:flatten([ [info_type(X), ": ", info_text(Y), "\n"] ||
			     {X, Y} <- ?Sup:info(Tag) ]),
    LineNum = length([ 10 || 10 <- Text ]),
    case Text of
	[] -> MaxLineLength = 0;
	_ ->
	    MaxLineLength = lists:max([ length(X) ||
					  X <- string:tokens(Text, "\n")])
    end,
    makescroll(info, {LineNum, 7}, {MaxLineLength, 40}),
    gs:config(info, {enable, true}),
    gs:config(info, {insert, {insert, Text}}),
    gs:config(info, {enable, false}).

info_type(name) -> "Name";
info_type(arity) -> "Arity";
info_type(exported) -> "Exported";
info_type(clausenum) -> "Number of Clauses";
info_type(file) -> "Location";
info_type(functions) -> "Functions";
info_type(files) -> "Number of files";
info_type(macros) -> "Macros";
info_type(records) -> "Records";
info_type(fieldnum) -> "Number of fields";
info_type(fieldnames) -> "Fields";
info_type(hasguard) -> "Has guard";
info_type(linenum) -> "Number of lines";
info_type(varbindingnum) -> "Number of bindings";
info_type(varrefnum) -> "Number of references";
info_type(_) -> "Unknown".

info_text({string, String}) -> String;
info_text({list, List}) -> string:join(List, ", ").

write_text(Tag) ->
    Text = lists:flatten(?Sup:text(Tag)),
    LineNum = length([ 10 || 10 <- Text ]),
    case Text of
	[] -> MaxLineLength = 0;
	_ ->
	    MaxLineLength = lists:max([ length(X) ||
					  X <- string:tokens(Text, "\n")])
    end,
    makescroll(text, {LineNum, 20}, {MaxLineLength, 40}),
    gs:config(text, {enable, true}),
    gs:config(text, {insert, {insert, Text}}),
    gs:config(text, {enable, false}).

makescroll(Editor, {Lines, MaxLines}, {Length, MaxLength}) ->
    if(Lines >= MaxLines) -> gs:config(Editor, {vscroll, right});
      (true) -> void
    end,
    if(Length >= MaxLength) -> gs:config(Editor, {hscroll, bottom});
      (true) -> void
    end.

watch_start() ->
    case whereis(?Watch) of
	undefined ->
	    Pid = spawn(fun() -> watch_loop() end),
	    register(?Watch, Pid),
	    Pid;
	Pid ->
	    Pid
    end.

watch_stop() ->
    ?Watch ! stop,
    unregister(?Watch).

watch_loop() ->
    receive
	stop ->
	    stopped;
	{Type, _} when Type == add orelse Type == reload ->
	    ?Server ! reload,
	    watch_loop();
	{status, Status} ->
	    ?Server ! {status, Status},
	    watch_loop();
	{trfinished, _} ->
	    ?Server ! trfinished,
	    watch_loop();
	{uifinished, undo} ->
	    ?Server ! {uifinished, undo},
	    watch_loop();
	{uifinished, reset} ->
	    ?Server ! {uifinished, reset},
	    watch_loop();
	_ ->
	    watch_loop()
    end.

draw_buttons(Tag) ->
    TrButtons = ?Sup:tr_buttons(Tag),
    ToolButtons = ?Sup:tool_buttons(),
    draw_buttonrow(TrButtons, 15),
    draw_buttonrow(ToolButtons, 55).

draw_buttonrow(Buttons, Y) ->
    lists:foreach(
      fun({B, X}) ->
	      gs:create(button, frame,
			[{x, 20 + (X - 1) * 110}, {y, Y},
			 {label, {text, B#tr.label}},
			 {data, {button, B}}])
      end, lists:zip(Buttons, ?MISC:seq2(1, length(Buttons)))).
