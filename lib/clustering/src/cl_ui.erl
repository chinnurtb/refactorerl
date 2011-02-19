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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created by E�tv�s Lor�nd University are Copyright 2008, E�tv�s
%%% Lor�nd University. All Rights Reserved.

%%% @doc This module contains interface functions for cluster ui interface.
%%% Two algorithm are available - agglomerative/genetic.
%%% This module is the engine of the Emacs Clustering interface (Cluster UI).
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(cl_ui).
-vsn("$Rev: 1335 $").
-include_lib("stdlib/include/qlc.hrl").
-export([run/1,cl_options/1]).
-export([prepare/1,recalc/0,refresh/0]).

-define(TBNAME,cl_ui).
-record(?TBNAME,{id = 0, options, fittnum, result}).

%% Interface
%%----------------------------------------------------------------

%%% @spec recalc() -> {atom(), atom()}
%%%
%%% @doc  This function recalculates the Attribute Matrix uses
%%% cl_interface:recalculate_matrix/0
recalc()->
   cl_interface:recalculate_matrix(),
   {ok, attr_matrix}.


%%% @spec run({Alg::atom(),Options::list(),CreateDb::atom}) ->
%%%                                            ClResultMain::list()
%%% @doc This function can execute the clustering with several options
%%% Alg: specifies the algorithm wich is used
%%% Options: contains the clustering options from the emacs interface
%%% CreateDb: ...save result into a storage or not
run({Opt, Alg, CreateDb})->
    Terms       = [Def || {_, Def} <- cl_options_in(Alg)],
    Opts = correct_opt(Opt,Terms),
    Options     = [convert(Value,Type) || {Value, Type} 
                                     <- lists:zip(Opts, Terms)],
    OptionName  = [Name || {Name, _} <- cl_options_in(Alg)],
    FOpt        = lists:zip(OptionName,Options),
    FinalOpt    = [{alg, Alg}]++FOpt,
    Clustering  = cl_interface:run_cluster([{log_output, null}]++FinalOpt),
     cl_db:update(deps),
    Fitt_Num    = [cl_fitness:fitness(ClusterNum,FinalOpt) || 
                                          ClusterNum <- Clustering],
    case CreateDb of t ->
      case table_handler() of
         {table, _} -> 
              store_result(FinalOpt, Fitt_Num, Clustering);
         _ -> throw({error, cl_ui_crashed})
      end;
      [] -> ok
    end,
    Algorithm = case Alg of
       agglom_attr -> "Agglomerative algorithm";
       genetic     -> "Genetic algorithm";
       _           -> undefined
    end, 
    VClustering = [[Lista,fn,Number] 
          || {Lista,Number}<-lists:zip(Clustering,Fitt_Num)],
    {[Algorithm]++VClustering, Fitt_Num}.

%%% @spec cl_options_in(Alg::atom()) -> OptionList::list()
%%% 
%%% @doc Options for the main function - this 
%%% is not interface function
%%% Alg: specifies the used algorithm.
cl_options_in(Alg)->
   cl_interface:run_cluster_default(Alg).

%%% @spec cl_options(Alg::atom()) -> OptionList::list()
%%%
%%% @doc To read default options of the clustering algorithm
%%% The result of the function is a list which contains the 
%%% default values.
%%% Alg: specifies the used algorithm. 
cl_options(Alg)->
      [{Lab, Def} || {{_Name, Def},{_Name, Lab}} 
           <- lists:zip(cl_interface:run_cluster_default(Alg),
                        cl_interface:run_cluster_labels(Alg))].

%%% Helpers -------------------------------------------------
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
     _:_ -> refac_ui:message(error,"Invalid type ~s",[V]),
	    V
   end.

w_type(T)->
    if 
      is_atom(T)    -> atom;
      is_integer(T) -> int;
      is_float(T)   -> float;
      is_function(T)-> function;
      true             -> string
    end.

correct_opt(Opt,Term)->
    [valid(O,T) || {O,T} <- lists:zip(Opt,Term)].

valid(Op,Term)->
    if 
	Op == [] -> 
             Term;
	true    -> 
             Op
    end.    

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

prepare(Modules)->
    if 
         Modules /= []->
           lists:map(fun refac_ui:add_file/2, Modules), 
           cl_interface:recalculate_attr([])
    end,
    table_handler(),
    {ok, modules_are_loaded}.

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

refresh()->
    case exists_table(?TBNAME) of
       {?TBNAME, exists}   -> 
            mnesia:delete_table(?TBNAME),
            {cl_ui, recreated};
       {?TBNAME, noexists} ->
            {cl_ui, noexec}
    end.
