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
-vsn("$Rev: 5115 $ ").

-export([run/3]).
-export([prepare/1, error_text/2]).

-include("user.hrl").

-define(Lib, refusr_sq_lib).

%% The record `state' stores the data of a single step in the semantic query.
%% `action' is the type of the current action. The action can be:
%%   selection
%%   closure
%%   iteration
%%   property_query
%%   statistics
%% `type', `res' and `prev_res', `prev_res' contains the data of the current and
%% the previous result.
-record(state, {action=selection, type, res, prev_type=[], prev_res=[]}).

-record(chains, {complete=[], incomplete=[], recursive=[]}).

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
%error_text(type_mismatch, Params) ->
%    io_lib:format("the entity types ~p and ~p don't match", Params);
%error_text(non_bool_property, Param) -> type mismatch eleg
%    io_lib:format("the property ~p is not bool", Param);
%error_text(non_property, Param) -> % illegal property?
%    io_lib:format("~p is not a property", Param);
error_text(type_mismatch, Params) ->
    io_lib:format("the types of ~p and ~p don't match", Params).

%%% ============================================================================
%%% Callbacks

%% @spec run(DisplayOpt::proplist(), Params::proplist(), Query::string()) ->
%%           QueryResult::term()
%% @doc Returns the result of `Query' starting from the position given by
%%      `Params'. The format of the result is determined by `DisplayOpt'.
%%
%%      `Params' optionally contains `file' and `position' keys.
%%
%%      `DisplayOpt' contains the keys `positions' and `output'.
%%      The possible values for
%%          - `positions': none|scalar|linecol
%%          - `output': stdio|{iodev,io_device()}|msg|other|nodes
%%
%%      The `QueryResult' depends on the `output' key in `DisplayOpt'.
%%          - stdio: a formatted text written to stdio
%%          - {iodev,Dev::io_device()}: a formatted text written to Dev
%%          - msg: a message containing a list with the following types of
%%                 elements: {eq, Name, Value} |
%%                           {list, [{Position, Text}]} |
%%                           {chain, [{Posititon, Text}], PostWS} |
%%                           {group_by, {Position, Text}}
%%          - other: the same list that is otherwise sent by a message
%%          - nodes: a simple list of nodes
%%      The format of positions depends on the `positions' key in `DisplayOpt'.
%%          - none: nopos
%%          - scalar: {File::string(), PosFrom::integer(), PosTo::integer()}
%%          - linecol: {File::string(), PosFrom::{integer(), integer()},
%%                                      PosTo::{integer(), integer()}}
run(DisplayOpt, Params, Query) when is_list(Query) ->
    case refusr_sq_lexer:string(Query) of
        {ok, Tokens, _} ->
            case refusr_sq_parser:parse(Tokens) of
                {ok, Result} ->
                    QueryRes = process_semantic_query(Params, Result),
                    show(QueryRes, DisplayOpt);
                {error, {_, _, Err}} -> throw(?LocalError(syntax_error, Err))
            end;
        {error, {_, _, Error}, _} ->
            throw(?LocalError(lexical_error, Error))
    end.

%% @private
%todo: alapertelmezett? meggondolni mi kellene + eclipsenek nem biztos h ez kell
prepare(Args) ->
    run([{positions, scalar},{output, msg}],
        Args,
        proplists:get_value(querystr, Args)),
    fun () -> nomsg end.

%%% ============================================================================
%%% Implementation

process_semantic_query(Params, SemanticQuery) ->
    {Entities, Type, QuerySeq, LastQuery} = preproc(Params, SemanticQuery),
    {Type1, Res1} = process_query_seq({Type, Entities}, QuerySeq),
%Res1 - deep list?
    State = #state{action = selection, type = Type1, res = flatsort(Res1)},
    process_query_seq(State, LastQuery).

process_query_seq(State, QuerySeq) ->
    lists:foldr(fun process/2, State, QuerySeq).


%%% ============================================================================
%%% Preprocessing of queries

%% @private
%% @spec preproc(Params::proplist(), SemanticQuery::list()) ->
%%         {InitialEntities::[entity()], InitialType::atom(),
%%          QuerySeq::list(), LastQuery::list()}
preproc(Params, SemanticQuery) ->
    {InitSel, QuerySeq, LastQuery} = split_semantic_query(SemanticQuery),
    {InitialType, InitialEntities} = ?Lib:init_sel(Params, InitSel),

    {Type1, QuerySeq1} = lists:foldl(fun check/2, {InitialType, []}, QuerySeq),
    {_Type, LastQuery0} = lists:foldl(fun check_last/2, {Type1, []}, LastQuery),

    LastQuery1 =
        case LastQuery0 of
            [Prop={property,_,_}, Filt={filer,_}| QS] -> [Filt, Prop| QS];
            LastQuery0 -> LastQuery0
        end,

    {InitialEntities, InitialType, lists:flatten(QuerySeq1), LastQuery1}.

split_semantic_query(SemanticQuery)->
    [{initial_selection, InitialSelection}| Rest] = SemanticQuery,
    RevRest = lists:reverse(Rest),
    {RevLastQuery, RevQuerySeq} = 
        case RevRest of
            [{closure, _}|_] ->
                lists:splitwith(fun({closure,_})-> true; (_)-> false end,
                                RevRest);
            [{iteration, _}|_] ->
                lists:splitwith(fun({iteration,_})-> true; (_)-> false end,
                                RevRest);
            [{statistics,_},{selection,_}|_] ->
                lists:split(2, RevRest);
            [] ->
                {[], []};
            _ ->
                lists:split(1, RevRest)
        end,

    LastQuery = lists:flatten([Lst|| {_, Lst} <- lists:reverse(RevLastQuery)]),
    QuerySeq = lists:flatten([Lst || {_, Lst} <- lists:reverse(RevQuerySeq)]),

    case InitialSelection of
        [{initial_selector, InitialSelector},Filter] ->
            {InitialSelector, [Filter|QuerySeq], LastQuery};
        [{initial_selector, InitialSelector}] ->
            {InitialSelector, QuerySeq, LastQuery}
    end.


%todo: sel/prop_fun error message
%% selection: properties are skiped here
check({selector, Selector}, {Type, Lst}) ->
    case ?Lib:sel_type(Type, Selector) of
        [SelType] -> {SelType, [{selector, Selector, SelType}| Lst]};
        [] -> case ?Lib:prop_type(Type, Selector) of
                  [_PropType] -> {Type, Lst};
                  [] -> throw(?LocalError(illegal_selector, [Type, Selector]))
              end
    end;

%% iteration/closure with the multiplicity of 1
check({_Action, {query_seq, QuerySeq}, {mult, 1}}, {Type, Lst}) ->
    QS = lists:flatten([List|| {_,List} <- QuerySeq]),
    {QSType, QSLst} = lists:foldl(fun check/2, {Type, []}, QS),
    ?Check(Type == QSType, ?LocalError(type_mismatch, [Type, QSType])),
    {Type, [QSLst| Lst]};

%% iteration/closure
check({Action, {query_seq, QuerySeq}, {mult, Mult}}, {Type, Lst}) ->
    QS = lists:flatten([List|| {_,List} <- QuerySeq]),
    {QSType, QSLst} = lists:foldl(fun check/2, {Type, []}, QS),
    ?Check(Type == QSType, ?LocalError(type_mismatch, [Type, QSType])),
    {Type, [{Action, QSLst, Mult}| Lst]};

%todo - skipped properties?
check({filter, Filter}, {Type, Lst}) ->
    {Type, [{filter, Filter}|Lst]};

%% statistics
check(Statistics, {_Type, Lst = [{property, _Prop, PropType}| _]}) ->
    case PropType of
        any -> {int, [{statistics, Statistics}| Lst]};
        int -> {int, [{statistics, Statistics}| Lst]};
        _ -> throw(?LocalError(prop_type_mismatch, [int, PropType]))
    end;

check(_Statistics, {_Type, [Elem| _]}) ->
    throw(?LocalError(non_property, [element(2, Elem)])).

%% the last selector: check whether it is a property or not
check_last({selector, Selector}, {Type, Lst}) ->
    case ?Lib:sel_type(Type, Selector) of
        [SelType] -> {SelType, [{selector, Selector, SelType}| Lst]};
        [] ->
            case ?Lib:prop_type(Type, Selector) of
                [PropType] -> {Type, [{property, Selector, PropType}| Lst]};
                [] -> throw(?LocalError(illegal_selector, [Type, Selector]))
            end
    end;
check_last({Action, {query_seq, QuerySeq}, {mult, Mult}}, {Type, Lst}) ->
    QS = lists:flatten([List|| {_,List} <- QuerySeq]),
    {QSType, QSLst} = lists:foldl(fun check/2, {Type, []}, QS),
    ?Check(Type == QSType, ?LocalError(type_mismatch, [Type, QSType])),
    {Type, [{Action, QSLst, Mult}| Lst]};
check_last(LstElem, Acc) -> check(LstElem, Acc).


%%% ============================================================================
%%% Processing of queries

process(_Query, {_Type, []}) ->
    {_Query, []};

process(_Query, #state{res=[]}=State) ->
    State;

%%% ----------------------------------------------------------------------------
%%% Selection

process({selector, Selector, SelType}, {Type, Entities}) ->
    [Fun] = ?Lib:sel_fun(Type, Selector),
    {SelType, [Fun(Entity) || Entity <- flatsort(Entities)]};

process({selector, Selector, SelType}, #state{type=Type, res=Entities}) ->
    [Fun] = ?Lib:sel_fun(Type, Selector),
    Result = [Fun(Entity) || Entity <- flatsort(Entities)],

    #state{action    = selection,
           type      = SelType, res      = Result,
           prev_type = Type,    prev_res = flatsort(Entities)};

%%% ----------------------------------------------------------------------------
%%% Property query

process({property, Property, _PropType}, #state{type=Type, res=Entities}) ->
    [Fun] = ?Lib:prop_fun(Type, Property),
    SortedEntities = flatsort(Entities),
    Result = [Fun(Entity) || Entity <- SortedEntities],

    #state{action    = property_query,
           type      = Property, res      = Result,
           prev_type = Type,     prev_res = SortedEntities};

%%% ----------------------------------------------------------------------------
%%% Iteration, closure

process({_Action, _QSLst, 0}, Result) ->
    Result;

process({iteration, QSLst, Mult}, {Type, Entities}) ->
    Result = process_query_seq({Type, flatsort(Entities)}, QSLst),
    process({iteration, QSLst, Mult-1}, Result);

process({closure, QSLst, Mult}, {Type, Entities}) ->
    SortedEntities = flatsort(Entities),
    {Type, NewEntities} = process_query_seq({Type, SortedEntities}, QSLst),
    SortedNewEntities = flatsort(NewEntities),
    NewMult = case Mult of infinite -> infinite; Mult -> Mult-1 end,
    case SortedNewEntities of
        SortedEntities -> {Type, SortedEntities};
        _ -> process({closure, QSLst, NewMult}, {Type, SortedNewEntities})
    end;

process({Action, QSLst, Mult}, #state{type=Type, res=Res}=State) ->
    InitialChains =
        case State#state.action of
            selection -> #chains{incomplete=[[Entity]|| Entity<-flatsort(Res)]};
            Action -> Res
        end,

    #state{action = Action,
           type = Type, res = chains(InitialChains, QSLst, Type, Mult, Action)};

%%% ----------------------------------------------------------------------------
%%% Statistics

% todo: error message
process({statistics, Statistics}, #state{res=PropValues}) ->
    ?Check(lists:all(fun(Val) -> is_integer(Val) orelse is_float(Val) end,
                     PropValues),
           ?LocalError(type_mismatch, [any, int])),

    Value =
        case Statistics of
            %sd -> ;
            %var -> ;
            %med -> ;
            min -> lists:min(PropValues);
            max -> lists:max(PropValues);
            sum -> lists:sum(PropValues);
            avg -> lists:sum(PropValues) / length(PropValues)
        end,

    #state{action = statistics, type = Statistics, res = Value};

%%% ----------------------------------------------------------------------------
%%% Filtering

%todo: why a deep list?
process({filter, Filter}, {Type, Entities}) ->
    {Type, filter(Filter, Type, flatsort(Entities))};

process({filter, Filter}, #state{action=selection}=State) ->
    FilteredRes = [ filter(Filter, State#state.type, Entities) ||
                      Entities <- State#state.res ],
    State#state{res=FilteredRes};

process({filter, Filter}, #state{action=iteration, type=Type, res=Res}=State) ->
    NewIncomplete = lists:filter(
                      fun(Chain) -> filter(Filter, Type, [hd(Chain)]) /= [] end,
                      Res#chains.incomplete),

    NewChainsRec = Res#chains{incomplete = NewIncomplete},
    State#state{res = NewChainsRec};

process({filter, Filter}, #state{action=closure}=State) ->
    Type = State#state.type,
    Chains = State#state.res,
    NewComplete = filter_chain(Filter, Type, Chains#chains.complete),
    NewIncomplete = filter_chain(Filter, Type, Chains#chains.incomplete),
    NewRecursive = filter_chain(Filter, Type, Chains#chains.recursive),

    NewChains = Chains#chains{complete   = NewComplete,
                              incomplete = NewIncomplete,
                              recursive  = NewRecursive},

    State#state{res = NewChains}.


%%% ============================================================================
%%% Helper functions

chains(#chains{incomplete=[]}=Chains, _QSLst, _Type, _Mult, _Action)->
    Chains;

chains(Chains, _QSLst, _Type, 0, _Action)->
    Chains;

chains(Chains, QSLst, Type, Mult, Action) ->
    NewChains =
        lists:flatmap(
          fun(Chain) ->
                  {Type, Res} = process_query_seq({Type, [hd(Chain)]}, QSLst),
                  next_chain(Action, Chain, flatsort(Res))
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

    NewChainsRec = #chains{complete   = Chains#chains.complete ++ Completed,
                           incomplete = Incomplete,
                           recursive  = Chains#chains.recursive ++ Recursive},

    NewMult = case Mult of infinite -> infinite; Mult -> Mult-1 end,
    chains(NewChainsRec, QSLst, Type, NewMult, Action).


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


filter_chain(Filter, Type, Chain) ->
    NewChainWithEmpties = [ filter(Filter, Type, List) || List <- Chain ],
    lists:filter( fun(List) -> List /= [] end, NewChainWithEmpties).

%%% ============================================================================
%%% Filters

%% @private
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
%% filter({'in', Property, {query_seq, QuerySeq}}, EntityType, Entities) ->
%%     FstInitialState = #state{res=[hd(Entities)], type=EntityType},
%%     FstResultingState = process_query_seq(FstInitialState, QuerySeq),
%%     NewProperty = FstResultingState#state.type,
%%     NewEntityType = FstResultingState#state.prev_type,

%%     ?Check( FstResultingState#state.action == property_query,
%%             ?LocalError(non_property, [NewProperty])),

%%     PropertyType = ?Lib:prop_type(EntityType, Property),
%%     NewPropertyType = ?Lib:prop_type(NewEntityType, NewProperty),
%%     ?Check( PropertyType == any orelse NewPropertyType == any orelse
%%             PropertyType == NewPropertyType,
%%             ?LocalError(prop_type_mismatch, [Property, NewProperty])),

%%     PropFun = prop_fun(EntityType, Property),

%%     lists:filter(
%%       fun(Entity) ->
%%               InitialState = #state{res=[Entity], type=EntityType},
%%               ResultingState = process_query_seq(InitialState, QuerySeq),
%%               lists:member(PropFun(Entity), ResultingState#state.res)
%%       end,
%%       Entities);

filter({query_seq, QuerySeq}, EntityType, Entities) ->
    QS = lists:flatten([List|| {_,List} <- QuerySeq]),
    {_QSType, QSLst} = lists:foldl(fun check/2, {EntityType, []}, QS),
    lists:filter(
      fun(Entity) ->
              {_QSType, Res} = process_query_seq({EntityType, [Entity]}, QSLst),
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


prop_fun(EntityType, Filter) ->
    case ?Lib:prop_fun(EntityType, Filter) of
        [] -> throw(?LocalError(illegal_property, [EntityType, Filter]));
        [Fun]   -> Fun
    end.

flatsort(List) -> lists:usort(lists:flatten(List)).


%%% ============================================================================
%%% Show

%todo: links to the prev results?
%% @private
%% @spec state_to_disp(#state{}) ->
%%          {EntityType::atom(), EntitiesToPosition::[entity()], Display}
%%       Display = {eq, PropName::atom(), PropValue} |
%%                 {list, EntityType::atom(), [entity()]} |
%%                 {chain, EntityType::atom(), [entity()], PostWS} |
%%                 {group_by, EntityType::atom(), entity()}
%% @doc This gives the list of the entities where a position is needed, and
%%      converts a `state' to an intermediate format used for displaying.
state_to_disp(#state{action=selection, prev_res=[], type=Type, res=Res}) ->
    {Type, flatsort(Res), [{list, Type, flatsort(Res)}]};

state_to_disp(#state{action=selection} = State) ->
    ZippedRes = lists:filter(fun({_, Ns}) -> Ns /= [] end,
                             lists:zip(State#state.prev_res, State#state.res)),
    Disp = lists:flatmap(
             fun({Entity, List}) ->
                     [{group_by, State#state.prev_type, Entity},
                      {list, State#state.type, List}]
             end,
             ZippedRes ),
    {State#state.type, flatsort(State#state.res), Disp};

state_to_disp(#state{action=property_query} = State) ->
    ZippedRes = lists:zip(State#state.prev_res, State#state.res),
    Disp = lists:flatmap(
             fun({Entity, PropVal}) ->
                     [{group_by, State#state.prev_type, Entity},
                      {eq, State#state.type, PropVal}]
             end,
             ZippedRes),
    {State#state.prev_type, flatsort(State#state.prev_res), Disp};

state_to_disp(#state{action=statistics} = State) ->
    {none, [], [{eq, State#state.type, State#state.res}]};

state_to_disp(#state{action=iteration, res = Chains} = State) ->
    Disp = lists:map(
             fun(Chain) ->
                     {chain, State#state.type, lists:reverse(Chain), "\n"}
             end,
          Chains#chains.incomplete),
    {State#state.type, flatsort(Chains#chains.incomplete), Disp};

state_to_disp(#state{action=closure, res = Chains} = State) ->
    NodesToPosition = flatsort( Chains#chains.complete ++
                                Chains#chains.incomplete ++
                                Chains#chains.recursive ),
    Disp = lists:append(
             [ [{chain, State#state.type, lists:reverse(Chain), "...\n"}||
                   Chain <- Chains#chains.incomplete],
               [{chain, State#state.type, lists:reverse(Chain), "\n"}||
                   Chain <- Chains#chains.complete],
               [{chain, State#state.type, lists:reverse(Chain), "*\n"}||
                   Chain <- Chains#chains.recursive] ]),
    {State#state.type, NodesToPosition, Disp}.


positions(_Type, _Nodes, none) ->
    dict:new();
% todo: linecol? 1,1 -> {1,1},{1,1}?
positions(file, Nodes, _PositionOpt) ->
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

positions(Type, Nodes, PositionOpt) ->
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
    TokenPos = lists:flatten([?Token:map_pos(File, Tokens, PositionOpt)||
                                 {File, Tokens} <- CollTokens]),
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


show(State, DisplayOpt) ->
    PositionsOpt = proplists:get_value(positions, DisplayOpt, none),
    OutputOpt = proplists:get_value(output, DisplayOpt, stdio),

    {NodeType, NodesToPosition, Display} = state_to_disp(State),
    Dict = positions(NodeType, NodesToPosition, PositionsOpt),

    %todo: message handling in emacs
    display(Dict, Display, OutputOpt, PositionsOpt).


display(Dict, Display, stdio, _) ->
    TextPos = lists:flatten([format(Dict, Disp, stdio) || Disp <- Display]),
    DispText = [[Text, pos_text(Pos)] || {Pos, Text} <- TextPos],
    io:put_chars(lists:flatten(DispText));

display(Dict, Display, {iodev, Dev}, _) ->
    TextPos = lists:flatten([format(Dict, Disp, stdio) || Disp <- Display]),
    DispText = [[Text, pos_text(Pos)] || {Pos, Text} <- TextPos],
    io:put_chars(Dev, lists:flatten(DispText));

display(Dict, Display, other, _) ->
    [format(Dict, Disp, msg) || Disp <- Display];

display(Dict, Display, msg, scalar) ->
    ?UI:message(queryres,
                lists:flatten([format(Dict, Disp, stdio) || Disp <- Display]));
display(Dict, Display, msg, _) ->
    ?UI:message(queryres, [format(Dict, Disp, msg) || Disp <- Display]).

fetch(Entity, Dict) ->
    case dict:find(Entity, Dict) of
        error -> nopos;
        {ok, Value} -> Value
    end.

%todo: links to prev results?
format(Dict, {group_by, Type, Entity}, stdio) ->
    [{fetch(Entity, Dict), text(Type, Entity)}, {nopos, "\n"}];
format(Dict, {list, Type, Entities}, stdio) ->
    [ [{nopos, "    "},
       {fetch(Entity, Dict), text(Type, Entity)},
       {nopos, "\n"}]|| Entity <- Entities ];
format(Dict, {chain, Type, Chain, PostWS}, stdio) ->
    [[[{fetch(Entity, Dict), text(Type, Entity)}, {nopos, " "}]||
         Entity <- Chain],
     {nopos, PostWS}];
format(_Dict, {eq, PropName, PropVal}, stdio) ->
    [{nopos,
      lists:flatten(io_lib:format("    ~p = ~p\n", [PropName, PropVal]))}];

format(Dict, {group_by, Type, Entity}, msg) ->
    {group_by, {fetch(Entity, Dict), text(Type, Entity)}};
format(Dict, {list, Type, Entities}, msg) ->
    {list, [{fetch(Entity, Dict), text(Type, Entity)}|| Entity <- Entities]};
format(Dict, {chain, Type, Chain, PostWS}, msg) ->
    {chain,
     [{fetch(Entity, Dict), text(Type, Entity)}|| Entity <- Chain], PostWS};
format(_Dict, {eq, PropName, PropValue}, msg) ->
    {eq, PropName, PropValue}.


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
   ?Query:exec(Node,
               ?Query:all([?Form:file(), ?Syn:first_leaf(), ?Syn:last_leaf()]));

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


text(Type, Entity) ->
    lists:flatten(text0(Type, Entity)).

text0(file, File) ->
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
text0(function, Fun) ->
    [Mod] = ?Query:exec(Fun, ?Fun:module()),
    ?MISC:fun_text([?Mod:name(Mod), ?Fun:name(Fun), ?Fun:arity(Fun)]);
text0(record, Rec) ->
    io_lib:write_atom(?Rec:name(Rec));
text0(macro, Mac) ->
    ?Macro:name(Mac);
text0(variable, Var) ->
    ?Var:name(Var);
text0(field, Field) ->
    io_lib:write_atom(?RecField:name(Field));
text0(expression, Expr) ->
    Text = string:strip(lists:flatten(?Syn:tree_text(Expr))),
    string:strip(Text, both, $\n).

pos_text(nopos) ->
    [];
pos_text({File, {Pos11, Pos12}, {Pos21, Pos22}}) ->
    ["   ", File, io_lib:format(": ~p,~p-~p,~p", [Pos11, Pos12, Pos21, Pos22])];
pos_text({File, Pos1, Pos2}) ->
    ["   ", File, io_lib:format(": ~p-~p", [Pos1, Pos2])].
