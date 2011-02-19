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

%%% @doc This module provides interface functions for clustering. The different
%%% user interfaces can use this transformation to calculate clustering. There
%%% are two algorithms implemented for clustering:
%%% <ul>
%%%     <li>Agglomerative algorithm: which can be used for module and function
%%%         clustering too.</li>
%%%     <li>Genetic algorithm: which can be used for module clustering </li>
%%% </ul>
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>
%%% @author Gabor Horvath <tyros3@gmail.com>

-module(refcl_main).
-vsn("$Rev: 5540 $").

-export([prepare/1, refresh/0]).

-include("cluster.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(TBNAME,cl_ui).
-record(?TBNAME,{id = 0, options, fittnum, result}).

%% Interface
%%----------------------------------------------------------------

% @todo document
prepare(Args) ->
    Algorithm = ?Args:algorithm(Args),
    Entity = ?Args:entity(Args),

    Defaults = cl_interface:run_cluster_default(Algorithm),
    DefValues = [ Val || {_, Val} <- Defaults],
    Labels = [ Lab || {_, Lab} <- cl_interface:run_cluster_labels(Algorithm)],

    AlgLabel =
        case Algorithm of
            agglom_attr -> "Agglomerative";
            genetic     -> "Genetic"
        end,

    EntLabel =
        case Entity of
            module   -> "Module";
            function -> "Function"
        end,

    Qu = [[{format,info},{text, EntLabel ++ " clustering with " ++ AlgLabel ++ " algorithm"}]],
    LabelsAndValues = lists:zip(Labels, DefValues),
    Question = Qu ++ add_to_question(LabelsAndValues) ++
               [[{format,checkbox},{text,"Save results to database:"},{default,false}]],
    Ans = lists:reverse(tl(?Transform:question(Question))),

    CreateDb = case hd(Ans) of
                   yes -> true;
                   _   -> false
               end,
    FinalOpt = [{alg,Algorithm},{entity_type,Entity}] ++
                get_options(lists:reverse(tl(Ans)), DefValues, Labels),

    fun() ->
        Clusterings = cl_interface:run_cluster([{log_output, null}]++FinalOpt),
        Fitt_Num    = cl_interface:fitness([{clusterings, Clusterings},
                                 {fitness_options, FinalOpt}]),

        case CreateDb of
            true  -> case table_handler() of
                      {table, _} ->
                          store_result(FinalOpt, Fitt_Num, Clusterings);
                      _          -> throw({error, cl_ui_crashed})
                  end;
            false -> ok
        end,

        % Create output from clustering and fitness numbers
        Res = [[{format,info},{text,"Clustering results:"}]],
        ClusterOutput = [add_to_result(Cluster) || Cluster <- Clusterings],
        FittnessOutput = [[{format,info},{text,"Fitness Numbers:"}],add_to_result(Fitt_Num)],
        Result = Res ++ ClusterOutput ++ FittnessOutput,

        ?Transform:question(Result),

        [Clusterings,Fitt_Num]
    end.

%% Private
%%----------------------------------------------------------------

add_to_question([]) -> [];

add_to_question([{L,V} | LabelsAndValues]) ->
    [] ++ add_to_question(L, V) ++ add_to_question(LabelsAndValues).

add_to_question(Label, DefValue) ->
    case Label of 
        "Transform function" -> add_to_question(select, Label, [none, zero_one]);
        "Distance function" -> add_to_question(select, Label, [call_sum, weight]);
        _                   -> 
            case DefValue of 
                undefined -> add_to_question(type, Label, none);
                _         -> add_to_question(type, Label, DefValue)
            end
    end.

add_to_question(type, Label, DefValue) ->
    DefValueStr = io_lib:fwrite("~p",[DefValue]),
    [[{format,textbox}, {text,Label ++ "(Type: " ++ DefValueStr ++ " for default)"},{validator,text},{default,-1}]];

add_to_question(select, Label, Values) ->
    DefValueStr = io_lib:fwrite("~p",[Values]),
    [[{format,textbox}, {text,Label ++ "(Select: " ++ DefValueStr ++ ")"},{validator,text},{default,-1}]].

add_to_result(List) ->
    Text = io_lib:fwrite("~p", [List]),
    [{format,info},{text,Text}].

get_options(Terms, Opt, OptionName) ->
    Opts    = correct_opt(Opt,Terms),
    Options = [get_option(Value,Type) ||  {Value, Type} <- lists:zip(Opts, Terms)],
    lists:zip(OptionName,Options).

get_option(Value, Type) ->
    case Value of
        "none" -> convert("undefined",Type);
        _      -> convert(Value, Type)
    end.

convert(Value,Term)->
   if
     Value == [] -> V = Term;
     true        -> V = Value
   end,
   try
      case w_type(Term) of
         float    -> list_to_float(V);
         int      -> list_to_integer(V);
         %% TODO:Need a branch for the [empty] lists
         %% for example this term {nil, []} will be [nil ] on
         %% the side of the emacs interface
         %% The empty string is equivalent with the empty list
         atom     -> list_to_atom(V);
         function -> not_implemented;
         _        -> V
      end
   catch
     _:_ -> reflib_ui:message(error,"Invalid type ~s",[V]),
        V
   end.

w_type(T)->
    if
      is_atom(T)    -> atom;
      is_integer(T) -> int;
      is_float(T)   -> float;
      is_function(T)-> function;
      true          -> string
    end.

correct_opt(Opt,Term)->
    [valid(O,T) || {O,T} <- lists:zip(Opt,Term)].

valid([], Term) -> Term;
valid(Op, _   ) -> Op.

store_result(Opt, Fitt, Cl_res)->
    Qdc = qlc:q([Id || {_, Id, _, _, _} <-mnesia:table(?TBNAME)]),
    Qds = mnesia:async_dirty(fun()-> qlc:e(Qdc)  end),
    case Qds of
        [] -> Id = 0;
         _ -> Id = lists:max(Qds)
    end,
    Record=#cl_ui{id = Id + 1,
                  options=Opt,
                  fittnum = Fitt,
                  result = Cl_res},
    mnesia:dirty_write(?TBNAME, Record).

exists_table(Tab)->
    try
        mnesia:table_info(Tab,arity),
        {Tab,exists}
    catch
        _:_ -> {Tab, noexists}
    end.

table_handler()->
    case exists_table(?TBNAME) of
        {?TBNAME, exists}   -> {table, exists};
        {?TBNAME, noexists} ->
            mnesia:create_table(?TBNAME,
                                [{attributes,record_info(fields,?TBNAME)},
                                 {disc_copies,[node()]},
                                 {type, bag}]),
            {table, created};
       _ -> throw({error, cl_ui_table})
       end.

%%% @doc Refreshing the mnesia tables. This means deleting the existing tables.
refresh()->
    case exists_table(?TBNAME) of
       {?TBNAME, exists}   ->
            mnesia:delete_table(?TBNAME),
            {cl_ui, recreated};
       {?TBNAME, noexists} ->
            {cl_ui, noexec}
    end.
