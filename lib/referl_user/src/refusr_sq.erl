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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @author Lilla Hajós <lya@elte.hu>

-module(refusr_sq).
-vsn("$Rev: 4855 $ ").

-export([run/2]).
-export([prepare/1, error_text/2]).

-include("user.hrl").

-define(Lib, refusr_sq_lib).

%% The record `state' stores the data of a single step in the semantic query.
%% `action' is the type of the current action. The action can be:
%%   initial_selection
%%   selection
%%   closure
%%   iteration
%%   property_query
%%   statistics
%% `type', `res' and `prev_res', `prev_res' contains the data of the current and
%% the previous result.
-record(state, {action=selection, type, res, prev_type, prev_res=[],
                show_pos=true}).

-record(chains, {max_length=0, complete=[], incomplete=[], recursive=[]}).

%%% ============================================================================
%%% Errors

error_text(lexical_error, Error) ->
    refusr_sq_lexer:format_error(Error);
error_text(syntax_error, Error) ->
    refusr_sq_parser:format_error(Error);
error_text(illegal_property, Params) ->
    io_lib:format("illegal ~p property: ~p", Params);
error_text(illegal_selector, Params) ->
    io_lib:format("illegal ~p selector: ~p  ", Params);
error_text(type_mismatch, Params) ->
    io_lib:format("the entity types ~p and ~p don't match", Params);
error_text(non_bool_property, Param) ->
    io_lib:format("the property ~p is not bool", Param);
error_text(non_property, Param) ->
    io_lib:format("~p is not a property", Param);
error_text(prop_type_mismatch, Params) ->
    io_lib:format("the types of ~p and ~p don't match", Params).

%%% ============================================================================
%%% Callbacks

%% @spec run(Params::proplist(), Query::string()) -> string()
%% @doc Returns the result of `Query'.
%% The proplist needs to contain `file' and `position' keys. The values of this
%% keys determine the initial state.
run(Params, Query) when is_list(Query) ->
    case refusr_sq_lexer:string(Query) of
        {ok, Tokens, _} ->
            case refusr_sq_parser:parse(Tokens) of
                {ok, Result} ->
                    Display = proplists:get_value(display, Params, stdio),
                    ShowPos = proplists:get_value(show_pos, Params, true),
                    QueryRes = process_semantic_query(Params, Result),
                    show(QueryRes, Display, ShowPos);
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

process_semantic_query(Params, SemanticQuery) ->
    lists:foldl(fun process/2, Params, SemanticQuery).

process_query_seq(State, QuerySeq) ->
    lists:foldl(fun process/2, State, QuerySeq).

%%% ============================================================================
%%% Statistics

%% {statistics, Statistics}
process({statistics, Statistics}, State) ->
%TODO: any
    PropType = ?Lib:prop_type(State#state.prev_type, State#state.type),
    ?Check(PropType==int,
           ?LocalError(prop_type_mismatch, [int, PropType])),

    {Type, Result} = collect_res(State),
    List = lists:sort(State#state.res),

    #state{action    = statistics,
           type      = Statistics,
           res       = stat(Statistics, List),
           prev_type = Type,
           prev_res  = Result};

%%% ============================================================================
%%% Initial selection

%% {initial_selection, [{initial_selector, Selector}, {filter, Filter}]} |
%% {initial_selection, [{initial_selector, Selector}]}
process({initial_selection, InitialSelection}, Params) ->
    lists:foldl(fun process/2, Params, InitialSelection);

process({initial_selector, InitialSelector}, Params) ->
    {EntityType, Entities} = ?Lib:init_sel(Params, InitialSelector),
    #state{type = EntityType, res=[Entities], action=initial_selection};

process({filter, Filter},
        #state{action=initial_selection, type=Type, res=Res}=State) ->
    FilteredRes = [ filter(Filter, Type, Entities) || Entities <- Res ],
    State#state{res=FilteredRes};

%%% ============================================================================
%%% Selection, property query

%% {selection, [{selector, Selector}, {filter, Filter}]} |
%% {selection, [{selector, Selector}]}
process({selection, Selection}, State) ->
    lists:foldl(fun process/2, State, Selection);

%% Properties can stand in place of the selectors. That case can not be handled
%% at the parser, because they are both atoms.
process({selector, Selector}, State) ->
    {PrevType, PrevRes} = collect_res(State),

    case ?Lib:sel_fun(PrevType, Selector) of
        [] ->
            case ?Lib:prop_fun(PrevType, Selector) of
                [] ->
                    throw(?LocalError(illegal_selector, [PrevType, Selector]));
                [Fun] ->
                    #state{action    = property_query,
                           type      = Selector,
                           res       = [Fun(Entity) || Entity <- PrevRes],
                           prev_type = PrevType,
                           prev_res  = PrevRes}
            end;
        [Fun] ->
            NewRes = [lists:usort(Fun(Entity)) || Entity <- PrevRes],
            #state{action    = selection,
                   type      = ?Lib:sel_type(PrevType, Selector),
                   res       = NewRes,
                   prev_type = PrevType,
                   prev_res  = PrevRes}
    end;

process({filter, Filter}, #state{action=selection, type=Type, res=Res}=State) ->
    FilteredRes = [ filter(Filter, Type, Entities) || Entities <- Res ],
    State#state{res=FilteredRes};

process({filter, Filter}, #state{action=property_query}=State) ->
    {Entities, Properties} =
        lists:unzip([ {Entity, Prop} ||
                        {Entity, Prop} <-
                            lists:zip(State#state.prev_res, State#state.res),
                        filter(Filter, State#state.prev_type, [Entity]) /= []]),

    State#state{res = Properties, prev_res = Entities};

%%% ============================================================================
%%% Iteration, Closure

%% {iteration, [[{query_seq, QuerySeq}, {mult, Int}]]}
%% {iteration, [[{query_seq, QuerySeq}, {mult, Int}], {filter, Filter}]}
%% {closure,   [[{query_seq, QuerySeq}, {mult, infinite}]]}
%% {closure,   [[{query_seq, QuerySeq}, {mult, infinite}], {filter, Filter}]}
%% {closure,   [[{query_seq, QuerySeq}, {mult, Int}]]}
%% {closure,   [[{query_seq, QuerySeq}, {mult, Int}], {filter, Filter}]}
process({Action, [[{query_seq, QuerySeq}, {mult, Mult}]]}, State) ->
    {PrevType, PrevRes} = collect_res(State),
    FstNewState = process_query_seq(State, QuerySeq),
    ?Check(PrevType == FstNewState#state.type,
           ?LocalError(type_mismatch, [PrevType, FstNewState#state.type])),

    InitialChains =
        case State#state.action of
            Action -> State#state.res;
            _      -> #chains{incomplete=[[Entity]|| Entity<-PrevRes]}
        end,

    InitialMult =
        if
            is_integer(Mult) -> InitialChains#chains.max_length + Mult;
            true             -> Mult
        end,

    #state{type = PrevType,
           res = chains(InitialChains, QuerySeq, InitialMult, State, Action),
           prev_type = PrevType,
           prev_res = PrevRes,
           action = Action};

process({iteration, [Params, {filter, Filter}]}, State) ->
    NewState = process({iteration, [Params]}, State),
    Chains = NewState#state.res,

    NewIncomplete =
        lists:filter(
          fun(List) ->
                  filter(Filter, NewState#state.type, [hd(List)]) /= []
          end,
          Chains#chains.incomplete),

    NewChainsRec = Chains#chains{incomplete = NewIncomplete},
    NewState#state{res = NewChainsRec};

process({closure, [Params, {filter, Filter}]}, State) ->
    NewState = process({closure, [Params]}, State),
    Chains = NewState#state.res,
    Type = NewState#state.type,

    NewComplete = filter_chain(Filter, Type, Chains#chains.complete),
    NewIncomplete = filter_chain(Filter, Type, Chains#chains.incomplete),
    NewRecursive = filter_chain(Filter, Type, Chains#chains.recursive),

    NewChains = Chains#chains{complete   = NewComplete,
                              incomplete = NewIncomplete,
                              recursive  = NewRecursive},

    NewState#state{res = NewChains}.

filter_chain(Filter, Type, Chain) ->
    NewChainWithEmpties = [ filter(Filter, Type, List) || List <- Chain ],
    lists:filter( fun(List) -> List /= [] end, NewChainWithEmpties).

%%% ============================================================================
%%% Filters

%% @spec filter(Filter::term(), EntityType::atom(), [entity()]) -> [entity()]
filter(_, _Type, []) -> [];

filter('true', _EntityType, Entities) -> Entities;

filter('false', _EntityType, _Entities) -> [];

filter({'not', Filter}, EntityType, Entities) ->
    Entities -- filter(Filter, EntityType, Entities);

filter({'or', Filter1, Filter2}, EntityType, Entities) ->
    lists:usort(
      filter(Filter1, EntityType, Entities) ++
      filter(Filter2, EntityType, Entities));

filter({'and', Filter1, Filter2}, EntityType, Entities) ->
    filter(Filter2,
           EntityType,
           filter(Filter1, EntityType, Entities));

%% todo: sq + list [a,s,d...] -> split
filter({'in', Property, {query_seq, QuerySeq}}, EntityType, Entities) ->
    FstInitialState = #state{res=[hd(Entities)], type=EntityType},
    FstResultingState = process_query_seq(FstInitialState, QuerySeq),
    NewProperty = FstResultingState#state.type,
    NewEntityType = FstResultingState#state.prev_type,

    ?Check( FstResultingState#state.action == property_query,
            ?LocalError(non_property, [NewProperty])),

    PropertyType = ?Lib:prop_type(EntityType, Property),
    NewPropertyType = ?Lib:prop_type(NewEntityType, NewProperty),
    ?Check( PropertyType == any orelse NewPropertyType == any orelse
            PropertyType == NewPropertyType,
            ?LocalError(prop_type_mismatch, [Property, NewProperty])),

    PropFun = prop_fun(EntityType, Property),

    lists:filter(
      fun(Entity) ->
              InitialState = #state{res=[Entity], type=EntityType},
              ResultingState = process_query_seq(InitialState, QuerySeq),
              lists:member(PropFun(Entity), ResultingState#state.res)
      end,
      Entities);

filter({query_seq, QuerySeq}, EntityType, Entities) ->
    lists:filter(
      fun(Entity) ->
              State = #state{type=EntityType, res=[Entity]},
              NewState = process_query_seq(State, QuerySeq),
              {_Type, Res} = collect_res(NewState),
              Res =/= []
      end,
      Entities);

%% Comparison works on atom, int and string.
%% todo: regexp match -> if a string doesn't match
filter({CompOp, Filt1, Filt2}, EntityType, Entities) ->
    Comp1 = if
                is_tuple(Filt1) -> filter(Filt1, EntityType, Entities);
                true ->            Filt1
            end,

    Comp2 = if
                is_tuple(Filt2) -> filter(Filt2, EntityType, Entities);
                true ->            Filt2
            end,

    PropFun1 = ?Lib:prop_fun(EntityType, Comp1),
    PropFun2 = ?Lib:prop_fun(EntityType, Comp2),

    lists:filter(
      fun(Entity) ->
              CompL = case PropFun1 of [] -> Comp1; [Fun1] -> Fun1(Entity) end,
              CompR = case PropFun2 of [] -> Comp2; [Fun2] -> Fun2(Entity) end,
              compare(CompOp, CompL, CompR)
      end,
      Entities);

filter(Filter, EntityType, Entities)->
    ?Check(?Lib:prop_type(EntityType, Filter) == bool,
           ?LocalError(non_bool_property, [Filter])),
    lists:filter(prop_fun(EntityType, Filter), Entities).


%%% ============================================================================
%%% Helper functions

compare(like, CompL, CompR) ->
    Distance =
        if
            is_list(CompL) andalso is_list(CompR) ->
                refusr_strm:getDistance(CompL, CompR);
            is_atom(CompL) andalso is_atom(CompR) ->
                refusr_strm:getDistance(atom_to_list(CompL),
                                        atom_to_list(CompR));
            true -> false
        end,

    case Distance of
         {lev, 0} -> true;
         {lev, 1} -> true;
         {lev, 2} ->
            Length = if is_atom(CompL) -> length(atom_to_list(CompL));
                        true           -> length(CompL)
                     end,
            case Length > 5 of true -> true; _ -> false end;
        _ -> false
    end;

compare(CompOp, CompL, CompR) ->
    apply(erlang, CompOp, [CompL, CompR]).


chains(#chains{incomplete=[]}=Chains, _QuerySeq, _Mult, _St, _Action)->
    Chains;
chains(#chains{max_length=Mult}=Chains, _QuerySeq, Mult, _State, _Action)->
    Chains;

chains(Chains, QuerySeq, Mult, State, Action)->
    {PrevType, _PrevRes} = collect_res(State),
    NewChains =
        lists:flatmap(
          fun(Chain) ->
                  InitialState = #state{res=[hd(Chain)], type=PrevType},
                  NewState = process_query_seq(InitialState, QuerySeq),
                  {_Type, Result} = collect_res(NewState),
                  next_chain(Action, Chain, Result)
          end,
          Chains#chains.incomplete),

    {CompletedAndRecursive, Incomplete} =
        lists:partition(fun is_tuple/1, NewChains),

    {CompletedTuple, RecursiveTuple} =
        lists:partition(
          fun({complete, _}) -> true; ({recursive, _}) -> false end,
          lists:flatten(CompletedAndRecursive)),

    Completed = [List || {_, List} <- CompletedTuple],
    Recursive = [List || {_, List} <- RecursiveTuple],

    NewChainsRec =
        #chains{max_length        = Chains#chains.max_length+1,
                complete          = Chains#chains.complete ++ Completed,
                incomplete        = Incomplete,
                recursive         = Chains#chains.recursive ++ Recursive},

    chains(NewChainsRec, QuerySeq, Mult, State, Action).

next_chain(iteration, _Chain, []) ->
    [];
next_chain(iteration, Chain, Result) ->
    [ [Entity| Chain] || Entity <- Result ];
next_chain(closure, Chain, []) ->
    [{complete, Chain}];
next_chain(closure, Chain, Result) ->
    [ case lists:member(Entity, Chain) of
          true -> {recursive, [Entity|Chain]};
          _    -> [Entity| Chain]
      end || Entity <- Result ].

stat(_Statistics, []) -> 0;
stat(min, List) -> hd(List);
stat(max, List) -> lists:last(List);
stat(sum, List) -> lists:sum(List);
stat(avg, List) -> lists:sum(List) / length(List).

prop_fun(EntityType, Filter) ->
    case ?Lib:prop_fun(EntityType, Filter) of
        [] -> throw(?LocalError(illegal_property, [EntityType, Filter]));
        [Fun]   -> Fun
    end.

flatsort(List) -> lists:usort(lists:flatten(List)).

collect_res(#state{res=Res} = State) ->
    PrevRes =
        case State#state.action of
            property_query ->
                State#state.prev_res;
            closure ->
                flatsort(Res#chains.complete ++
                         Res#chains.incomplete ++
                         Res#chains.recursive);
            iteration ->
                flatsort([hd(List) || List <- Res#chains.incomplete]);
            _ ->
                flatsort(Res)
        end,

    PrevType = case State#state.action of
                   property_query -> State#state.prev_type;
                   _              -> State#state.type
               end,
    {PrevType, PrevRes}.


%%% ============================================================================
%%% Show

%% Calls `positions/2' when position display is on.
positions(_Type, _List, false) ->
    not_needed;
positions(Type, List, true) ->
    positions(Type, List).

%% If the node position is not needed,
%% this function skips its calculation.
%% Otherwise, it returns the real position and text of the node.
fetch(Node, Type, not_needed, false) ->
    {1, text(Type, Node)};
fetch(Node, Type, Dict, true) ->
    {dict:fetch(Node, Dict), text(Type, Node)}.


show(#state{type=Type, res=Nodes, action=initial_selection}, Disp, ShowPos) ->
    List = lists:flatten(Nodes),
    Dict = positions(Type, List, ShowPos),
    display(
      lists:append(
        [[fetch(Node, Type, Dict, ShowPos),
          {nopos, "\n"}] || Node <- List]),
      Disp);

show(#state{type=Type, res=Res, action=iteration}, Disp, ShowPos) ->
    Nodes = flatsort( Res#chains.complete ++
                      Res#chains.incomplete ++
                      Res#chains.recursive),

    Dict = positions(Type, Nodes, ShowPos),
    display(
      lists:flatten(
        [ [[[fetch(Node, Type, Dict, ShowPos), {nopos, " "}] ||
              Node <- lists:reverse(List)],
          {nopos, "...\n"}] || List <- Res#chains.incomplete ] ++

        [ [[[fetch(Node, Type, Dict, ShowPos), {nopos, " "}] ||
              Node <- lists:reverse(List)],
          {nopos, "*\n"}] || List <- Res#chains.recursive ] ++

        [ [[[fetch(Node, Type, Dict, ShowPos), {nopos, " "}] ||
              Node <- lists:reverse(List)],
          {nopos, "\n"}] || List <- Res#chains.complete ]),
      Disp);

show(#state{action=closure}=State, Disp, ShowPos) ->
    show(State#state{action=iteration}, Disp, ShowPos);

show(#state{type=Type, prev_type=PrevType, action=selection} = St, Disp, ShowPos) ->
    Dict = positions(Type, lists:flatten(St#state.res), ShowPos),
    List = lists:filter( fun({_, Ns}) -> Ns /= [] end,
                         lists:zip(St#state.prev_res, St#state.res) ),
    display(
      lists:flatten(
        [[{nopos, [text(PrevType, PrevNode), ":\n"]},
         [[{nopos, "    "},
           fetch(Node, Type, Dict, ShowPos),
           {nopos,"\n"}] ||
             Node <- Nodes ]] ||
            {PrevNode, Nodes} <- List]),
      Disp);

show(#state{type=Type, res=Res, action=property_query,
            prev_type=PrevType, prev_res=PrevRes}, Disp, ShowPos) ->
    Dict = positions(PrevType, PrevRes, ShowPos),
    display(
      lists:append(
        [[fetch(Node, PrevType, Dict, ShowPos),
          {nopos, [io_lib:format(":\n    ~p = ~p\n", [Type, Prop])]}] ||
            {Node, Prop} <- lists:zip(PrevRes, Res)]),
      Disp);

show(#state{type=Type, res=Res, action=statistics}, Disp, _) ->
    display([{nopos, io_lib:format("~p = ~p", [Type, Res])}], Disp).

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
              {First, {Pos1, _}} = lists:keyfind(First, 1, TokenPos),
              {Last, {_, Pos2}} = lists:keyfind(Last, 1, TokenPos),
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
    io_lib:write_atom(?RecField:name(Field));
text(expression, Expr) ->
    Text = string:strip(lists:flatten(?Syn:tree_text(Expr))),
    string:strip(Text, both, $\n).
