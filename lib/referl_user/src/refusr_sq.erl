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
-vsn("$Rev: 5016 $ ").

-export([run/2]).
-export([prepare/1, error_text/2]).

-include("user.hrl").

-define(Lib, refusr_sq_lib).
-define(SQUI, refusr_sq_ui).

%% The record `state' stores the data of a single step in the semantic query.
%% `action' is the type of the current action. The action can be:
%%   initial_selection
%%   selection
%%   closure
%%   iteration
%%   property_query
%%   statistics
%% `type', `res', `prev_type' and `prev_res' contain data for the current and
%% the previous result respectively.
-record(state, {action=selection, type, res, prev_type, prev_res=[]}).

-record(chains, {max_length=0, complete=[], incomplete=[], recursive=[]}).

%%% ============================================================================
%%% Errors
%%% @private
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
                    QueryRes = process_semantic_query(Params, Result),
                    ShowPos = proplists:get_value(show_pos, Params, scalar),
                    PosNodes = ?SQUI:poscalc(QueryRes, ShowPos),
                    LineNum = proplists:get_value(linenum, Params, false),
                    PosText = ?SQUI:show(PosNodes, LineNum),
                    Dev = proplists:get_value(display, Params, stdio),
                    Display = {Dev, LineNum},
                    ?SQUI:display(PosText, Display);
                {error, {_, _, Err}} ->
                    throw(?LocalError(syntax_error, Err))
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

%% @todo sq + list [a,s,d...] -> split
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

%% @doc Comparison works on atom, int and string.
%% @todo regexp match -> if a string doesn't match
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

collect_res(#state{res=Res} = State) ->
    PrevRes =
        case State#state.action of
            property_query ->
                State#state.prev_res;
            closure ->
                ?MISC:flatsort(Res#chains.complete ++
                               Res#chains.incomplete ++
                               Res#chains.recursive);
            iteration ->
                ?MISC:flatsort([hd(List) || List <- Res#chains.incomplete]);
            _ ->
                ?MISC:flatsort(Res)
        end,

    PrevType = case State#state.action of
                   property_query -> State#state.prev_type;
                   _              -> State#state.type
               end,
    {PrevType, PrevRes}.
