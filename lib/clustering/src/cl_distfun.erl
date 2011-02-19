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

%%% @doc This module contains functions to calculate the distance of two
%%% entities based on their attributes. The functions assume that attribute
%%% values are numbers.
%%%
%%% @author Aniko Nagyne Vig <viganiko@inf.elte.hu>
%%% @author Krisztian Toth <ccc@inf.elte.hu>
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_distfun).
-vsn("$Rev: 1247 $").

-export([euclidian/4, jaccard/4, sorensen_dice/4, sorensen_dice2/4,
         ochiai/4, correlation/4, call_sum/4, call_cnt/4,
         weight_gen/1,weight_distvec/4,
         call_fun_cnt/4,lexgr_dist/1]).
-export([pow_size_fun_gen/1]).

-include("cluster.hrl").

%%% @type dist_fun() = (entity(), [{attr(), value()}],
%%%                     entity(), [{attr(), value()}]) -> number()

fold2(_,   Acc, [],      _)       -> Acc;
fold2(_,   Acc, _,       [])      -> Acc;
fold2(Fun, Acc, [H1|T1], [H2|T2]) -> fold2(Fun, Fun(H1, H2, Acc), T1, T2).

%% @spec euclidian(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%       
%% @doc Euclidian distance function.
euclidian(_E1, Attr1, _E2, Attr2) ->
    math:sqrt(fold2(
                fun({_,V1}, {_,V2}, Sum) -> Sum + (V1-V2) * (V1-V2) end,
                0, Attr1, Attr2)).


-record(presence, {both=0, first=0, second=0, none=0}).

presence(Attr1, Attr2) ->
    fold2(
      fun
      ({_,0}, {_,0}, P=#presence{none=N})   -> P#presence{none=N+1};
      ({_,_}, {_,0}, P=#presence{first=N})  -> P#presence{first=N+1};
      ({_,0}, {_,_}, P=#presence{second=N}) -> P#presence{second=N+1};
      ({_,_}, {_,_}, P=#presence{both=N})   -> P#presence{both=N+1}
         end,
      #presence{},
      Attr1, Attr2).

%% @spec jaccard(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Jaccard distance function.
jaccard(_E1, Attr1, _E2, Attr2) ->
    #presence{both=A, first=B, second=C} = presence(Attr1, Attr2),
    if
        A+B+C == 0 -> 1;
        true       -> 1 - A/(A+B+C)  %% the paper contains an sqrt too
    end.

%% @spec sorensen_dice(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Sorensen-Dice distance function.
sorensen_dice(_E1, Attr1, _E2, Attr2) ->
    #presence{both=A, first=B, second=C} = presence(Attr1, Attr2),
    if
        A+B+C == 0 -> 1;
	true       -> 1 - 2*A / (2*A + B + C) 
    end.

%% @spec sorensen_dice2(Mod,[{Attr, Weight}],Mod,[{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Sorensen-Dice distance function variant.
sorensen_dice2(_E1, Attr1, _E2, Attr2) ->
    #presence{both=A, first=B, second=C, none=D} = presence(Attr1, Attr2),
    if
        A+B+C+D == 0 -> 1;
	true         -> 1 - 2*(A+D) / (2*A + B + C + D) 
                        %%orig nincs +D alul, de akkor negativ ertekek
    end.

%% @spec ochiai(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Ochiai distance function.
ochiai(_E1, Attr1, _E2, Attr2) ->
    #presence{both=A, first=B, second=C} = presence(Attr1, Attr2),
    if
        (A+B)*(A+C) == 0 -> 1;
        true             -> 1 - A/math:sqrt((A+B)*(A+C)) 
    end.

%% @spec correlation(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Distance function based on correlation of attributes.
correlation(_E1, Attr1, _E2, Attr2) ->
    {SumV1, SumV2, SumV1xV1, SumV2xV2, SumV1xV2} =
        fold2(
          fun({_,V1}, {_,V2},
              {SV1, SV2, SV11, SV22, SV12}) ->
                  {SV1+V1, SV2+V2, SV11 + V1*V1, SV22 + V2*V2, SV12 + V1*V2}
          end, {0, 0, 0, 0, 0}, Attr1, Attr2),
    Corr = (SumV1xV1 - SumV1*SumV1) * (SumV2xV2 - SumV2*SumV2),
    R = if
            Corr =< 0 -> 1;
            true      -> (SumV1xV2 - SumV1*SumV2/length(Attr1)) / Corr
        end,
    if
        1-R < 0 -> 1; 
        true    -> math:sqrt((1-R)/2)
    end.


%% @spec call_sum(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Distance function based on function call structure, sums call weights.
call_sum(Ent1, Attr1, Ent2, Attr2) ->
    calls(Ent1, modcall_sum(Attr1), Ent2, modcall_sum(Attr2)).

modcall_sum(Attr) ->
    Mods = lists:usort([M || {#fun_attr{mod=M},W} <- Attr, W>0]),
    [{Mod, lists:sum([W || {#fun_attr{mod=M},W} <- Attr, M == Mod])} ||
        Mod <- Mods]. 

%% @spec call_cnt(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Distance function based on function call structure, ignores call
%% weights, only the count of different functions is used.
call_cnt(Ent1, Attr1, Ent2, Attr2) ->
    calls(Ent1, modcall_cnt(Attr1,1), Ent2, modcall_cnt(Attr2,1)).

modcall_cnt(Attr,Add) ->
    D = lists:foldl(
          fun ({#fun_attr{mod=M},W},D2) when W>0 ->
                  case dict:is_key(M,D2) of
                      true ->  
                          Plus = case Add of w -> W; 1 -> 1 end,
                          dict:store(M,dict:fetch(M,D2)+Plus,D2);
                      false -> 
                          dict:store(M,1,D2)
                  end;
              (_,D2) ->
                  D2
          end,
          dict:new(),
          Attr),
    dict:to_list(D).

%% Generic distance function for function calls
calls(Ent1, ModCall1, Ent2, ModCall2) ->
    Call12 = proplists:get_value(Ent1, ModCall2, 0),
    Call21 = proplists:get_value(Ent2, ModCall1, 0),
    Similar = lists:sum([min(W, proplists:get_value(M, ModCall2, 0)) ||
                            {M, W} <- ModCall1]),
    lexgr_dist([min(Call12, Call21), Call12 + Call21, Similar]).

min(A, B) when A < B -> A;
min(_, B) -> B.
max(A, B) when A > B -> A;
max(_, B) -> B.

%% @private
lexgr_dist([]) -> 0;
lexgr_dist([Head | Tail]) ->
    1 / (2 + Head - lexgr_dist(Tail)).

pow_size_fun_gen(PowWeight) ->
    fun(S1,S2) ->
            math:pow(S1+S2,PowWeight) 
    end.

%% @spec weight_gen(SizeFun) -> dist_fun()
%%       SizeFun = (Size1::number(),Size2::number()) -> number()
%%
%% @doc Generates a distance function.
%% The distance function is based on function call structure and record usage,
%% and it is weighted by the number calculated by `SizeFun'.
%% It ignores call weights, only the count of different functions is used.
weight_gen(SizeFun) ->
    fun (Mod1, Attr1, Mod2, Attr2) ->
            DistVector = weight_distvec(Mod1, Attr1, Mod2, Attr2),
            Size1 = proplists:get_value(size, Attr1, 1),
            Size2 = proplists:get_value(size, Attr2, 1),
            lexgr_dist(DistVector) * SizeFun(Size1,Size2)
    end.

%% @private
weight_distvec(_Mod1, Attr1, _Mod2, Attr2) ->
    ModCall1 = modcall_cnt(Attr1,w),
    ModCall2 = modcall_cnt(Attr2,w),
    Call12 = modcall2_cnt(proplists:get_value(entities,Attr2,[]), ModCall1),
    Call21 = modcall2_cnt(proplists:get_value(entities,Attr1,[]), ModCall2),
    {_SameRecMax,SameRecMin} = same_rec(Attr1,Attr2),
    Similar = lists:sum([min(W, proplists:get_value(M, ModCall2, 0)) ||
                            {M, W} <- ModCall1]),
    [min(Call12, Call21), SameRecMin, Call12 + Call21, Similar].

%% @doc Returns how heavily are the modules in `Modules' called by the module
%% from which ModCall was created.
modcall2_cnt(Modules,ModCall) -> 
    ModuleSet = ordsets:from_list(Modules),
    lists:foldl(
      fun ({M,W},N) -> % my module calls module M
              case ordsets:is_element(M,ModuleSet) of
                  true -> N+W;
                  false -> N
              end;
          (_,N) ->
              N
      end,
      0,
      ModCall).

%% @spec call_fun_cnt(#fun_attr{}, [{Attr, Weight}], #fun_attr{},
%%           [{Attr, Weight}]) -> Distance
%%
%%           Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Distance function based on function call structure and record usage
%% to be used with function clustering.
call_fun_cnt(Fun1, Attr1, Fun2, Attr2) ->
    Call12 = proplists:get_value(Fun1, Attr2, 0),
    Call21 = proplists:get_value(Fun2, Attr1, 0),
    Rec1 = used_records(Attr1),
    Rec2 = used_records(Attr2),
    SameRec = length(ordsets:intersection(Rec1, Rec2)),
    Similar = lists:sum([min(W, proplists:get_value(F, Attr2, 0)) ||
                            {F, W} <- Attr1]),
    lexgr_dist([min(Call12, Call21), SameRec, Call12 + Call21, Similar]).

used_records(Attr) ->
    lists:usort([R || {R=#rec_attr{}, W} <- Attr, W>0]).

used_records2(Attr) ->
    lists:foldl(
      fun ({R=#rec_attr{}, W}, D) ->
              dict:store(R,W,D);
          (_,D) ->
              D
      end,
      dict:new(),
      Attr).

same_rec(Attr1, Attr2) ->
    Rec1 = used_records2(Attr1),
    Rec2 = used_records2(Attr2),
    dict:fold(
      fun(R,W1,{Max,Min}) ->
              case dict:is_key(R,Rec2) of
                  true -> 
                      W2 = dict:fetch(R,Rec2),
                      {Max+max(W1,W2),Min+min(W1,W2)};
                  false ->
                      {Max,Min}
              end
      end,
      {0,0},
      Rec1).

