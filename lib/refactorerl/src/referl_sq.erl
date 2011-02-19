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

%%% @author Lilla Hajós <lya@elte.hu>

-module(referl_sq).
-vsn("$Rev: 3681 $").

-export([run/2]).
-export([prepare/1, error_text/2]).

-include("refactorerl.hrl").


-record(state, {type=none, res=[], prev_type=none, prev_res=[], prop=none}).

%%% ============================================================================
%%% Errors

error_text(lexical_error, Error) ->
    referl_sq_lexer:format_error(Error);
error_text(syntax_error, Error) ->
    referl_sq_parser:format_error(Error);
error_text(type_mismatch, Params) -> 
    io_lib:format("the types ~p and ~p don't match", Params);
error_text(illegal_property, Params) ->
    io_lib:format("illegal ~p property: ~p", Params);
error_text(illegal_selector, Params) ->
    io_lib:format("illegal ~p selector: ~p  ", Params);
error_text(non_bool_property, Param) ->
    io_lib:format("the property ~p in not bool", Param).
 
%%% ============================================================================
%%% Callbacks

%% @spec run(Params::proplist(), Query::string()) -> string()
%% @doc Returns the result of `Query'.
%% The proplist needs to contain `file' and `position' keys. The values of this
%% keys determine the initial state.
run(Params, Query) when is_list(Query) ->
    case referl_sq_lexer:string(Query) of
        {ok, Tokens, _} ->
            case referl_sq_parser:parse(Tokens) of
                {ok, Result} -> proc(Params, Result);
                {error, {_, _, Err}} -> throw(?LocalError(syntax_error, Err))
            end;
        {error, {_, _, Error}, _} ->
            throw(?LocalError(lexical_error, Error))
    end.

%% @private
prepare(Args) ->
    run([{display, msg} | Args], proplists:get_value(querystr, Args)),
    fun () -> nomsg end.

%%% ============================================================================
%%% Implementation

proc(Params, [Query|Queries]) ->
    Display = proplists:get_value(display, Params, stdio),
    Res = lists:foldl(fun proc_query/2, initial_state(Params, Query), Queries),
    show(Res, Display).


flatsort(List) -> lists:usort(lists:flatten(List)).

%% TODO: Maybe this should work on a query sequence not just a single query.
%% Maybe RTC?
proc_query({Mult, Query},
           #state{type = Type, res = Res, prev_type = PrevType} = State) ->
    ?Check( Type /= PrevType, ?LocalError(type_mismatch, [Type, PrevType])),
    SortedRes = 
        if
            Res == [] -> [];
            is_tuple(hd(Res)) -> flatsort([List || {_, List} <- Res]);
            true -> flatsort(Res)
        end,
    closure(Mult, Query, State, [ [Node] || Node <- SortedRes ], []);

proc_query(ParseElem, State) ->
    Selector = proplists:get_value(selector, ParseElem),
    FilterSeq = proplists:get_value(filter, ParseElem, []),
    filter(select(State, Selector), FilterSeq).


closure(_Mult, _Query, State, [], Acc2) ->
    State#state{res = lists:keysort(2, Acc2), prev_res = []};

closure(0, _Query, #state{type = Type} = State, Acc1, Acc2) ->
    ResAcc1 = [ {incomplete, lists:reverse(List)} || List <- Acc1],
    Res = lists:keysort(2, ResAcc1 ++ Acc2),
    State#state{res = Res, prev_type = Type, prev_res = []};

closure(Mult, Query, State, Acc1, Acc2) ->
    {NewState, NewAcc1, NewAcc2} = cl_step(Query, State, Acc1, Acc2),
    if
        is_integer(Mult) -> closure(Mult-1, Query, NewState, NewAcc1, NewAcc2);
        true -> closure(Mult, Query, NewState, NewAcc1, NewAcc2)
    end.

cl_step(Query, State, Acc1, Acc2) ->
    NewState = proc_query(Query, State),
    ZippedRes = lists:zip(NewState#state.prev_res, NewState#state.res),
    Acc = lists:flatmap(
            fun(List) ->
                    case lists:keysearch(hd(List), 1, ZippedRes) of
                        {value, {_, []}} -> [{finished, lists:reverse(List)}];
                        {value, {_, Res}} -> acc(List, Res)
                    end
            end,
            Acc1),
    {ToAcc2, NewAcc1} = lists:partition(fun is_tuple/1, Acc),
    {NewState, NewAcc1, Acc2 ++ ToAcc2}.

acc(List, NewRes) ->
    lists:map(
      fun(Node) ->
              case lists:member(Node, List) of
                  true ->
                      {recursive, lists:reverse([Node| List])};
                  false ->
                      [Node| List]
              end
      end,
      NewRes).

%%% ============================================================================
%%% Filter

filter(State, []) ->
    State;

filter(#state{type=Type, res=Res}=St, Filter) ->
    FilteredRes = [ filter(Type, Nodes, Filter) || Nodes <- Res ],
    St#state{res=FilteredRes, prop=none}.

filter(Type, Nodes, Filter) when is_atom(Filter) ->
    ?Check(referl_sq_lib:prop_type(referl_sq_lib:entity(Type), Filter) == bool,
           ?LocalError(non_bool_property, [Filter])),
    lists:filter(prop(Type, Filter), Nodes);

filter(Type, Nodes, {'not', Filter}) ->
    (Nodes -- filter(Type, Nodes, Filter));

filter(Type, Nodes, {'or', Filter1, Filter2}) ->
    lists:usort(filter(Type, Nodes, Filter1) ++ filter(Type, Nodes, Filter2));

filter(Type, Nodes, {'and', Filter1, Filter2}) ->
    filter(Type, filter(Type, Nodes, Filter1), Filter2);

filter(Type, Nodes, {Comp, Prop, {re, _, Re}}) ->
    PropFun = prop(Type, Prop),
    lists:filter(
      fun(Node) ->
              not apply(erlang, Comp, [re:run((PropFun)(Node), Re), nomatch])
      end,
      Nodes );

filter(Type, Nodes, {Comp, Prop, Value}) ->
    PropFun = prop(Type, Prop),
    lists:filter(
      fun(Node) ->  apply(erlang, Comp, [(PropFun)(Node), Value]) end,
      Nodes ).


prop(Type, Prop) ->
    Entity = referl_sq_lib:entity(Type),
    case referl_sq_lib:prop_fun(Entity, Prop) of
        false -> throw(?LocalError(illegal_property, [Type, Prop]));
        Fun   -> Fun
    end.

%%% ============================================================================
%%% Select

select(#state{type=Type, res=Res}=St, Selector) ->
    SortedRes = if
                    Res == [] -> [];
                    is_tuple(hd(Res)) -> flatsort([List || {_, List} <- Res]);
                    true -> flatsort(Res)
                end,
    Entity = referl_sq_lib:entity(Type),
    case referl_sq_lib:sel_fun(Entity, Selector) of
        false ->
            case referl_sq_lib:prop_fun(Entity, Selector) of
                false ->throw(?LocalError(illegal_selector, [Type, Selector]));
                _Fun -> St#state{res=SortedRes, prop=Selector}
            end;
        Fun ->
            NewRes = [ lists:usort((Fun)(Node)) || Node <- SortedRes ],
            #state{type = referl_sq_lib:sel_type(Entity, Selector),
                   res = NewRes,
                   prev_type = Type,
                   prev_res = SortedRes}
    end.

initial_state(Params, Query) when is_list(Query) ->
    Selector = proplists:get_value(selector, Query),
    FilterSeq = proplists:get_value(filter, Query, []),
    {Type, Entities} = referl_sq_lib:init_sel(Params, Selector),
    filter(#state{type = Type, res=[Entities]}, FilterSeq).


%%% ============================================================================
%%% Show

%% initial selector
show(#state{type=Type, res=Nodes, prev_type=none, prop=none}, Disp) ->
    List = lists:flatten(Nodes),
    Dict = positions(Type, List),
    display(
      lists:append(
        [[{dict:fetch(Node, Dict), text(Type, Node)},
          {nopos, "\n"}] || Node <- List]),
      Disp);
%% result of closure
show(#state{type=Type, res=Res, prev_res=[], prop=none}, Disp) ->
    Dict = positions(Type, flatsort([List || {_, List} <- Res])),
    display(
      lists:flatten(
        [ begin 
              Ed = case ClSt of
                       finished -> {nopos, "\n"};
                       incomplete -> {nopos, "...\n"};
                       recursive ->  {nopos, "*\n"}
                   end,
              [[[{dict:fetch(Node, Dict), text(Type, Node)},
                {nopos, " "}] || Node <- List], Ed]
          end || {ClSt, List} <- Res ]),
      Disp);

show(#state{type=Type, prev_type=PrevType, prop=none} = St, Disp) ->
    Dict = positions(Type, lists:flatten(St#state.res)),
    List = lists:filter( fun({_, Ns}) -> Ns /= [] end,
                         lists:zip(St#state.prev_res, St#state.res) ),
    display(
      lists:flatten(
        [[{nopos, [text(PrevType, PrevNode), ":\n"]},
         [[{nopos, "    "},
           {dict:fetch(Node, Dict), text(Type, Node)},
           {nopos,"\n"}] ||
             Node <- Nodes ]] ||
            {PrevNode, Nodes} <- List]),
      Disp);
%% property value
show(#state{type=Type, res=Res, prop=Prop}, Disp) ->
    Dict = positions(Type, lists:flatten(Res)),
    display(
      lists:append([[{dict:fetch(Node, Dict), text(Type, Node)},
        {nopos, [":\n    ", prop_text(Type, Node, Prop), "\n"]}] ||
          Node <- flatsort(Res)]),
      Disp).


display(List, stdio) ->
    io:put_chars([Text || {_, Text} <- List]);
display(List, msg) ->
    io:put_chars([Text || {_, Text} <- List]),
    ?UI:message(queryres, [{Pos, lists:flatten(Text)} || {Pos, Text} <- List]).


file_and_tokens(function, Node) ->
   ?Query:exec(Node, ?Query:seq(?Fun:definition(),
                                ?Query:all([ ?Form:file(),
                                             ?Syn:first_leaf(),
                                             ?Syn:last_leaf() ])));

file_and_tokens(record, Node) ->
    ?Query:exec(Node, ?Query:seq(?Rec:form(), 
                                 ?Query:all([ ?Form:file(),
                                              ?Syn:first_leaf(),
                                              ?Syn:last_leaf() ])));

file_and_tokens(field, Node) ->
    Rec = ?Query:exec(Node, [{field, back}]),
    file_and_tokens(record, Rec);

file_and_tokens(macro, Node) ->
   ?Query:exec(Node, ?Query:all([ ?Form:file(),
                                  ?Syn:first_leaf(),
                                  ?Syn:last_leaf() ]));

file_and_tokens(expression, Node) ->
    [File] = ?Syn:get_file(Node),
    [First] = ?Query:exec(Node, ?Syn:first_leaf()),
    [Last] = ?Query:exec(Node, ?Syn:last_leaf()),
    [File, First, Last];

file_and_tokens(variable, Node) ->
    case ?Query:exec(Node, ?Var:bindings()) of
        [Bind | _] -> file_and_tokens(expression, Bind);
        [] -> []
    end.


positions(file, Nodes) ->
    dict:from_list(
      [ begin
            Pos = case ?Graph:class(File) of
                      file ->
                          {?File:path(File), 1, 1};
                      module ->
                          case ?Query:exec(File, ?Mod:file()) of
                              [] -> nopos;
                              [Node] -> {?File:path(Node), 1, 1}
                          end
                  end,
            {File, Pos}
        end || File <- Nodes ]);

positions(Type, Nodes) ->
    NodesWithTokens = [ {Node, file_and_tokens(Type, Node)} || Node <- Nodes ],
    CollTokens = 
        dict:to_list(
          lists:foldl(
            fun({_Node, []}, Dict) ->
                    Dict;
               ({_Node, [File, First, Last]}, Dict) ->
                    D1 = case dict:is_key(File, Dict) of
                             false -> dict:store(File, [First], Dict);
                             true -> dict:append(File, First, Dict)
                         end,
                    dict:append(File, Last, D1)
            end,
            dict:new(),
            lists:keysort(2, NodesWithTokens))),
    TokenPos = lists:flatten(
                 [?Token:map_pos(File, Tokens)|| {File, Tokens} <- CollTokens]),
    lists:foldl(
      fun({Node, []}, Dict) ->
              dict:store(Node, nopos, Dict);
         ({Node, [File, First, Last]}, Dict) ->
              {value, {First, {Pos1, _}}} = lists:keysearch(First, 1, TokenPos),
              {value, {Last, {_, Pos2}}} = lists:keysearch(Last, 1, TokenPos),
              dict:store(Node, {?File:path(File), Pos1, Pos2}, Dict)
      end,
      dict:new(),
      NodesWithTokens).


text(file, File) ->
    case ?Graph:class(File) of
        file ->
            Path = ?File:path(File),
            string:substr(Path, string:rstr(Path, "/")+1);
        module ->
            case ?Query:exec(File, ?Mod:file()) of
                [] -> io_lib:write_atom(?Mod:name(File));
                [Node] -> 
                    Path = ?File:path(Node),
                    string:substr(Path, string:rstr(Path, "/")+1)
            end
    end;
text(function, Fun) ->
    [Mod] = ?Query:exec(Fun, ?Fun:module()),
    ?MISC:fun_text([?Mod:name(Mod), ?Fun:name(Fun), ?Fun:arity(Fun)]);
text(record, Rec) ->
    io_lib:write_atom(?Rec:name(Rec));
text(macro, Mac) ->
    ?Macro:name(Mac);
text(variable, Var) ->
    ?Var:name(Var);
text(field, Field) ->
    io_lib:write_atom((?Graph:data(Field))#field.name);
text(expression, Expr) ->
    Text = string:strip(lists:flatten(?Syn:tree_text(Expr))),
    string:strip(Text, both, $\n).


prop_text(Type, Node, Prop) ->
    PropVal = (prop(Type, Prop))(Node),
    [io_lib:format("~p = ~p", [Prop, PropVal])].
