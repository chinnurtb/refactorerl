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

%%% ============================================================================
%%% Module Informations

%%% @doc Library to handle variable options of clustering steps.

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_options).
-vsn("$Rev: $").

-include("ucluster.hrl").
-include("ucl_options.hrl").



%%% ============================================================================
%%% Exports/imports

% Error messages
-export([error_text/2]).

% Functions for defaults
-export([true/1]).
% Functions for validators
-export([is_listOfLists/1, is_listOfAtoms/1, is_generator/1, 
         is_applicable_fun/2, is_fun_result/2,
         generate/1, get_fun/2]).



%%% ============================================================================
%%% Error messages

%% @private
%% @spec error_text(Type::atom(), Details::[term()]) -> string()
%% @doc Give back the error message text for the transformation specific errors.
%%      The error is specified by the `Type' and the `Details'.
error_text(undefined_function, {ModName,FunName,Arity}) ->
    ?MISC:format("Undefined function ~p:~p/~p.", [ModName,FunName,Arity]);
error_text(Type, Detail) ->
    ?MISC:format("Missing error message for {~p, ~p, ~p}.",
        [?MODULE, Type, Detail]).



%%% ============================================================================
%%% Functions for defaults

%% @spec true(any()) -> true
%% @doc  Return always true. The argument is not used.
true(_) ->
    true.



%%% ============================================================================
%%% Functions for validators

%%% ----------------------------------------------------------------------------
%%% List property values

%% @spec is_listOfLists(Object) -> boolean()
%% @doc Return `true' if `Object' is a list of lists.
is_listOfLists(List) when is_list(List) ->
    0 == element(1,?MISC:list_find(fun(Elem) -> not is_list(Elem) end, List));
is_listOfLists(_) ->
    false.


%% @spec is_listOfAtoms(Object) -> boolean()
%% @doc Return `true' if `Object' is a list of atoms.
is_listOfAtoms(List) when is_list(List) ->
    0 == element(1,?MISC:list_find(fun(Elem) -> not is_atom(Elem) end, List));
is_listOfAtoms(_) ->
    false.



%%% ----------------------------------------------------------------------------
%%% Function property values

%% @spec is_generator(Object::term()) -> boolean()
%% @doc Return `true' if the `Object' is a `generator' record and describe
%%      a calling of an exported function.
is_generator(#generator{modname=ModName,funname=FunName,args=Args}) when
        is_atom(ModName), is_atom(FunName), is_list(Args) ->
    erlang:function_exported(ModName,FunName,length(Args));
is_generator(_) -> false.

%% @spec generate(Gen::generator()) -> Value::term()
%% @throws refError(undefined_function)
%% @doc Generate a value by executing the function call which is described in 
%%      the `Gen' generator record.
%% @see is_generator/1
generate(#generator{modname=ModName,funname=FunName,args=Args}) when
        is_atom(ModName), is_atom(FunName), is_list(Args) ->
    try
        apply(ModName, FunName, Args)
    catch
        error:'undef' -> throw(?LocalError(undefined_function, 
                                           {ModName,FunName,length(Args)}))
    end.


%% @spec is_applicable_fun(Fun::Function, Arity::natural()) -> boolean()
%%       Fun = function(Arity) | {ModName::atom(),FunName::atom()}
%% @doc Return `true' if the `Fun' is a `Arity'-arity function. If `Fun' is a
%%      tuple the `ModName:FunName/Arity' function must be exported.
is_applicable_fun({ModName,FunName}, Arity) when is_integer(Arity), 0=<Arity,
        is_atom(ModName), is_atom(FunName) ->
    erlang:function_exported(ModName,FunName,Arity);
is_applicable_fun(Fun, Arity) when is_integer(Arity), 0=<Arity,
        is_function(Fun,Arity) ->
    true;
is_applicable_fun(_,_) ->
    false.

%% @spec is_fun_result(Fun, ReqArity::natural) -> boolean()
%%       Fun = function(Arity) | {ModName::atom(),FunName::atom()} | generator()
%% @doc Return `true' if the `Fun' is a `Arity'-arity function or a generator
%%      record which generate a `ReqArity'-arity function. If `Fun'
%%      is a tuple the `ModName:FunName/Arity' function must be exported.
%% @see is_applicable_fun/2
%% @see generate/1
is_fun_result(Fun, ReqArity) when is_integer(ReqArity), 0=<ReqArity ->
    try
        is_applicable_fun(get_fun(Fun,ReqArity),ReqArity)
    catch
        % throw:?RefErrorPattern(_,can_not_retrieve_fun,_) -> false
        throw:?LocalError(can_not_retrieve_fun, _) -> false
    end.


%% @spec get_fun(FunDesc, ReqArity::natural()) -> Fun
%%       FunDesc = {ModuleName::atom(),FunctionName::atom()} |
%%                 fun() | generator()
%%       Fun = fun(ReqArity)
%% @doc Create a function expression from the `FunDesc' function descriptor.
%%      In general `FunDesc' came from a property list where it is a value and
%%      determine a function that shuold be used by the owner function of the
%%      property list.
%%
%% The different forms of function descriptor may be usefull in different
%% situation.
%% <ul>
%%   <li>`{ModuleName::atom(), FunctionName::atom()}': the
%%     `ModuleName:FunctionName/ReqArity' function must be exported.</li>
%%   <li>`fun()': the function expression arity must equal with `ReqArity'.</li>
%%   <li>`generator()': the generated value must be one of the previous cases.
%%     See {@link generate/1} for the generation.</li>
%% </ul>
%%
%% @todo Write general funtion expression generator. Currently the function
%%       expressions with higher arity than 26 are not supported.
get_fun(Gen=#generator{}, ReqArity) when is_integer(ReqArity), 0=<ReqArity ->
    get_fun_(generate(Gen), ReqArity);
get_fun(Fun, ReqArity) ->
    get_fun_(Fun, ReqArity).

get_fun_({ModName,FunName}, ReqArity)  when is_integer(ReqArity), 0=<ReqArity,
        is_atom(ModName), is_atom(FunName) ->
    gf_(ReqArity,ModName,FunName);
get_fun_(Fun, ReqArity) when is_integer(ReqArity), 0=<ReqArity,
        is_function(Fun,ReqArity) -> 
    Fun;
get_fun_(Fun, ReqArity) ->
    throw(?LocalError(can_not_retrieve_fun, {Fun,ReqArity})).

% UGLY, UGHLY function expression generation !!! Is there any better method?
% Exercise:
%   There are 3 variable, two atom and a natural.
% Goal:
%   Create the `fun modname:funname/arity' implicit function expression 
%   in run time.
gf_( 0,MN,FN) -> fun() -> apply(MN,FN,[]) end;
gf_( 1,MN,FN) -> fun(A) -> apply(MN,FN,[A]) end;
gf_( 2,MN,FN) -> fun(A,B) -> apply(MN,FN,[A,B]) end;
gf_( 3,MN,FN) -> fun(A,B,C) -> apply(MN,FN,[A,B,C]) end;
gf_( 4,MN,FN) -> fun(A,B,C,D) -> apply(MN,FN,[A,B,C,D]) end;
gf_( 5,MN,FN) -> fun(A,B,C,D,E) -> apply(MN,FN,[A,B,C,D,E]) end;
gf_( 6,MN,FN) -> fun(A,B,C,D,E,F) -> apply(MN,FN,[A,B,C,D,E,F]) end;
gf_( 7,MN,FN) -> fun(A,B,C,D,E,F,G) -> apply(MN,FN,[A,B,C,D,E,F,G]) end;
gf_( 8,MN,FN) -> fun(A,B,C,D,E,F,G,H) -> apply(MN,FN,[A,B,C,D,E,F,G,H]) end;
gf_( 9,MN,FN) -> fun(A,B,C,D,E,F,G,H,I) -> apply(MN,FN,[A,B,C,D,E,F,G,H,I]) end;
gf_(10,MN,FN)->fun(A,B,C,D,E,F,G,H,I,J)->apply(MN,FN,[A,B,C,D,E,F,G,H,I,J]) end;
gf_(11,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K]) end;
gf_(12,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L]) end;
gf_(13,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M]) end;
gf_(14,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) end;
gf_(15,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) end;
gf_(16,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) end;
gf_(17,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) end;
gf_(18,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) end;
gf_(19,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) end;
gf_(20,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) end;
gf_(21,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) end;
gf_(22,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V]) end;
gf_(23,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W]) end;
gf_(24,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X]) end;
gf_(25,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y]) end;
gf_(26,MN,FN) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) ->
        apply(MN,FN,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z]) end.



