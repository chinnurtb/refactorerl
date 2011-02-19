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

%%% @doc This module provides input for module clustering based on the
%%% output of the `call_graph' module. Filter and weighting functions that
%%% work with this function call weight representation are also provided here.
%%%
%%% @author Aniko Nagyne Vig <viganiko@inf.elte.hu>
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_call_graph).
-vsn("$Rev: 3185 $").

-export([empty_attrib/0, attrib_data/0]).
%% Filter functions
-export([library_mod/2, library_mod_rel/1, internal_fun/2]).
%% Weighting functions
-export([simplenum/1, simpleinnum/1, simpleoutnum/1, allwin/1, allwout/1,
         allw/1, nodynin/1, nodynout/1, nodyn/1, funbin/1, funbout/1, funb/1]).

-include("cluster.hrl").

-record(call_data, {exp,cfunc,func,weight}).
-record(call, {mod,func,arg}).
%-record(dyn, {data, mod, func, arg}).

%% @type wght() = #wght{funb = integer(),
%%                      br = integer(),
%%                      dyn = integer()}.
%%
%% It represents a function call's  weight. 
%%
%% Fields:
%% <ul>
%%     <li>funb: Number of the clauses in the caller function, which calls
%%         the callie function.</li>
%%     <li>br: Number of the branches (if, case, etc.) in the function
%%         clause.</li>
%%     <li>dyn: Number of the possible function calls. It may be different
%%         from one, when the function call is dynamic.</li>
%% </ul>
-record(wght, {funb, br, dyn}).

%% @type weight_record() = #dw{ins = integer(),
%%                             infb = number(),
%%                             inb = number(),
%%                             ind = number(),
%%                             outs = integer(),
%%                             outfb = number(),
%%                             outb = number(),
%%                             outd = number()}.
%%
%% Stores data about the relation between a module and a function.
%%
%% Fields:
%% <ul>
%%     <li>ins: The number of the calls from the function to the module.</li>
%%     <li>infb: The sum of the multiplicative inverse of the funb-s of the
%%         function calls from the function to the module.</li> 
%%     <li>inb: The sum of the multiplicative inverse of the br-s of the
%%         function calls from the function to the module.</li> 
%%     <li>ind: The sum of the multiplicative inverse of the dyn-s of the
%%         function calls from the function to the module.</li> 
%%     <li>outs: The number of the calls from the module to the function.</li>
%%     <li>outb: The sum of the multiplicative inverse of the funb-s of the
%%         function calls from the module to the function.</li> 
%%     <li>outb: The sum of the multiplicative inverse of the br-s of the
%%         function calls from the module to the function.</li> 
%%     <li>outd: The sum of the multiplicative inverse of the dyn-s of the
%%         function calls from the module to the function.</li> 
%% </ul>
-record(dw, {ins=0,infb=0, inb=0, ind=0, outs=0, outfb=0, outb=0, outd=0}).


%% @spec empty_attrib() -> weight_record()
%%
%% @doc Default value for attributes missing from the generated data.
empty_attrib() ->
    #dw{}.

%% @spec attrib_data() -> [{Mod, [{{M,F,Ary}, weight_record()}]}]
%%
%% @doc Generates attribute data.
attrib_data() ->
    Calls = ets:new(calls, []),
    lists:foreach(
      fun(Call) -> store_call(Call, Calls) end,
      cg:get_calls()),
    Data = [mod_data(Mod, Calls) || Mod <- cg:get_modules()],
    ets:delete(Calls),
    Data.

mod_data(Mod, Calls) ->
    {Mod,
     [{Attr, Val} ||
         {{_,Attr}, Val} <- ets:match_object(Calls, {{Mod, '_'}, '_'})]}.

%% @spec store_call(call_data(),tid()) -> true
%%
%% @doc Stores a function call in the `Calls' table.
store_call(#call_data{func=#call{mod=CallerMod,
                                 func=CallerFun,
                                 arg=CallerAry},
                      cfunc=#call{mod=CalleeMod,
                                  func=CalleeFun,
                                  arg=CalleeAry},
                      weight=Weight},
           Calls) ->
    store_call({CallerMod, #fun_attr{mod=CalleeMod,
                                      name=CalleeFun,
                                      arity=CalleeAry}},
               Weight, fun caller/2, Calls),
    store_call({CalleeMod, #fun_attr{mod=CallerMod,
                                      name=CallerFun,
                                      arity=CallerAry}},
               Weight, fun callee/2, Calls).

%% @doc Updates the row with `Key' key of the `Calls' table with `Weight'.
%%
%% `AddFun' is the function which is used to update the existing data.
store_call(Key, Weight, AddFun, Calls) ->
    Data = case ets:lookup(Calls, Key) of
               [] -> #dw{};
               [{_,D}] -> D
           end,
    ets:insert(Calls, {Key, AddFun(Data, Weight)}).

%% @doc Updates `CD' with the following information:
%% "The module belonging to CD called the function belonging to CD."
caller(#dw{outs=S, outfb=FB, outb=B, outd=D} = CD,
       #wght{funb=FunB, br=Br, dyn=Dyn}) ->
    CD#dw{outs  = S+1,
          outfb = FB + 1/FunB,
          outb  = B + 1/Br,
          outd  = D + 1/Dyn}.

%% @doc Updates `CD' with following the information:
%% "The module belonging to CD was called by the function belonging to CD."
callee(#dw{ins=S, infb=FB, inb=B, ind=D} = CD,
       #wght{funb=FunB, br=Br, dyn=Dyn}) ->
    CD#dw{ins  = S+1,
          infb = FB + 1/FunB,
          inb  = B + 1/Br,
          ind  = D + 1/Dyn}.

%%%%% Filter functions

%% @doc Entity filter that drops modules with only incoming calls.
library_mod(Module, Calls) ->
    not lists:any(
          fun ({#fun_attr{mod=Mod}, #dw{outs=Out}})
              when Mod /= Module, Out > 0 ->
                  true;
              (_) ->
                  false
          end, Calls).

%% @doc Entity filter that drops modules wich have much more incoming calls
%% than outgoing calls.
library_mod_rel(Ratio) ->
    fun (Module, Calls) ->
            {InsNum, OutsNum} =
                lists:foldl(
                  fun({#fun_attr{mod=Mod}, #dw{ins=Ins, outs=Outs}}, {IN, OUT})
                     when Mod /= Module -> 
                          {IN + Ins, OUT+Outs}
                  end,
                  {0, 0}, Calls),
            InsNum /= 0 andalso OutsNum / InsNum < Ratio
    end.


%% @doc Attribute filter that drops module internal functions.
internal_fun(#fun_attr{mod=Module}, Calls) ->
    not lists:any(
          fun({Mod, #dw{ins=In, outs=Out}})
              when Mod /= Module, In+Out > 0 ->
                  true;
             (_) ->
                  false
          end, Calls).

%%%%% Functions that compute call weight values from call weight records

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
simplenum(#dw{ins=Ins, outs=Outs}) -> %%ins+outs
    Ins + Outs.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
simpleinnum(Dw)-> %%ins
    Dw#dw.ins.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
simpleoutnum(Dw)-> %%outs
    Dw#dw.outs.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
allwin(Dw)-> %%infb*inb*ind
    Dw#dw.inb*Dw#dw.infb*Dw#dw.ind.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
allwout(Dw)-> %%outfb*outb*outd
    Dw#dw.outb*Dw#dw.outb*Dw#dw.outd.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
allw(Dw)-> %%outfb*outb*outd+infb*inb*ind
    Dw#dw.outb*Dw#dw.outb*Dw#dw.outd+Dw#dw.inb*Dw#dw.infb*Dw#dw.ind.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
nodynin(Dw)-> %%infb*inb
    Dw#dw.inb*Dw#dw.infb.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
nodynout(Dw)-> %%outfb*outb
    Dw#dw.outb*Dw#dw.outb.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
nodyn(Dw)-> %%outfb*outb+infb*inb
    Dw#dw.outb*Dw#dw.outb+Dw#dw.inb*Dw#dw.infb.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
funbin(Dw)-> %%infb
    Dw#dw.inb.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
funbout(Dw)-> %%outfb
    Dw#dw.outb.

%% @doc Weighting function that transforms a call weight record into a
%% numerical call weight.
funb(Dw)-> %%outfb+infb
    Dw#dw.outb+Dw#dw.inb.

