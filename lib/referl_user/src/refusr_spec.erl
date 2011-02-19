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
%%% Module information

%%% @doc 

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>

-module(refusr_spec).
-vsn("$Rev: $"). %for emacs"



-include("user.hrl").

-export([initETS/0, tv/0]).
-export([analyzeSpecs/0, genspec/1 ]).
-export([whichElem/2]).
%-export([getFunByMFA/3]).

-export([t/3, zipN/1]).


-record(type, 
        {kind,
         value}).



%% @todo: move these functions to their proper place

%% to reflib_file:
specs() ->
    ?Query:seq([file],[{form, {type, '==', spec}}]).
%% to reflib_typexp:
typexp_type(Node) ->
    (?Graph:data(Node))#typexp.type.
typexp_tag(Node) ->
    (?Graph:data(Node))#typexp.tag.
%% which element is Elem in List
whichElem(Elem, List) ->
    whichElem_acc(Elem, List, 1).
whichElem_acc(_, [], N) ->
    N;
whichElem_acc(Elem, [Head|Tail], N) ->
    case Elem == Head of
        true ->
            N;
        false ->
            whichElem_acc(Elem, Tail, N+1)
    end.


%% =========================================================================
%% =========================================================================
%% storing and retrieving the existing -spec forms
initETS() ->
    ets:new(specs, [named_table]),
    ets:new(vars, [named_table]).
tv() ->
    tv:start().


analyzeSpecs() ->
    AllSpecForms = ?Query:exec(specs()), 
    Specs = lists:map(fun buildFromSpec/1, AllSpecForms),
    lists:map(fun(X) ->
                      ets:insert(specs, X)
              end, Specs).

buildFromSpec(Form) ->
    [Modpart, Funpart]  = ?Query:exec(Form, [tattr]),
    [Mod] = ?Query:exec(Form, ?Form:module()),
    ModName = ?Mod:name(Mod),
    {Mod_ret, Name} = analModpart(Modpart),
    case {Mod_ret, ModName} of
        {{ok, ModName}, ModName} ->
            Mod;
        {{ok, ModName}, _Name} ->
            io:format("Warning! Module name doesn't match in specification! (~p)~n",
                      [{ModName, Name}]);
        {{error, unknown}, _} ->
            Mod
    end,
    FunPart = analFunpart(Funpart),
    #type{kind = funsig, value = {Args, _RetV}} = FunPart,
    [Func] = ?Query:exec(Mod, ?Fun:find(Name, length(Args))),
    {Func, FunPart}.

analModpart(Node) ->
    case typexp_type(Node) of
        module_qualifier ->
            [Modq, Name] = ?Query:exec(Node, [tsub]),
            {{ok, typexp_tag(Modq)}, typexp_tag(Name)};
        atom ->
            Name = typexp_tag(Node),
            {{error, unknown}, Name}
    end.

analFunpart(Node) ->
    case typexp_type(Node) of
        fun_sig ->
            [Arglist, RetType] = ?Query:exec(Node, [tsub]),
            #type{kind = funsig, 
                  value = {analFunpart(Arglist), analFunpart(RetType)}};
        arglist ->
            Args = ?Query:exec(Node, [tsub]),
            lists:map(fun analFunpart/1, Args);
        call ->
            [Type, ArgList] = ?Query:exec(Node, [tsub]),
            R = case ?Query:exec(ArgList, [tsub]) of
                    [] -> 
                        any;
                    Args ->
                        lists:map(fun analFunpart/1, Args)
                end,
            #type{kind = typexp_tag(Type), 
                  value = R};
        atom ->
            #type{kind = atom, 
                  value = typexp_tag(Node)};
        tuple ->
            Fields = ?Query:exec(Node, [tsub]),
            #type{kind = tuple, 
                  value = lists:map(fun analFunpart/1, Fields)};
        list ->
            Elems = ?Query:exec(Node, [tsub]),
            #type{kind = list, 
                  value = lists:map(fun analFunpart/1, Elems)};
        vardef ->
            [_VarName, Type] = ?Query:exec(Node, [tsub]),
            analFunpart(Type);
        paren ->
            [Children] = ?Query:exec(Node, [tsub]),
            analFunpart(Children);
        joker ->
            #type{kind = any,
                  value = any};
        union ->
            Fields = ?Query:exec(Node, [tsub]),
            #type{kind = union, 
                  value = lists:map(fun analFunpart/1, Fields)};
        _ ->
            not_implemented_yet
    end.

%% getArity({funsig, {arglist, ArgList}, _RetValue}) ->
%%    length(ArgList).

getTypeOfFunc(Func) ->
    case ets:lookup(specs, Func) of
        [{_F, Result}] ->
            io:format("DEBUG getTypeOfFunc/return:fromtable ~p~n",[Result]),
            Result;
        [] ->
            Result = genspec(Func),
            io:format("DEBUG getTypeOfFunc/return:genspec ~p~n",[Result]),
            Result
    end.


%% =========================================================================
%% =========================================================================
%% =========================================================================


genspec([Module, Name, Arity]) ->
    io:format("DEBUG (genspec/MFA) ~p~n",[{Module,Name,Arity}]),
    [Func] = ?Query:exec(?Query:exec(?Mod:find(Module)), 
                         ?Mod:local(Name, Arity)),
    genspec(Func);
genspec(Func) ->
    io:format("DEBUG (genspec/Func) ~p~n",[Func]),
    [Mod] = ?Query:exec(Func, ?Fun:module()),
    _ModName = ?Mod:name(Mod),
    Clauses = ?Query:exec(Func, ?Query:seq([?Fun:definition(),
                                          ?Form:clauses()])),
    Arguments  = lists:map(fun(X) ->
                                   P = ?Query:exec(X, ?Clause:patterns()),
                                   lists:map(fun getType/1, P)
                           end, Clauses),
    _Guards = lists:map(fun(X) ->
                               ?Query:exec(X, ?Clause:guard())
                       end, Clauses),
    Bodies = lists:map(fun(X) ->
                               B = ?Query:exec(X, ?Clause:body()),
                               lists:map(fun getType/1, B)
                       end, Clauses),
    ArgTypeList = lists:map(fun(X) ->
                                    io:format("DEBUG (genspec/arguments) ~p~n",[Arguments]),
                                    lists:map(fun finalize/1, X)
                            end, Arguments),
    ArgType = case ArgTypeList of
                  [AX] ->
                      AX;
                  AL ->
                      lists:map(fun(X) ->
                                        #type{kind=union, value=X}
                                end, zipN(AL))
              end,
    io:format("DEBUG (genspec/argtype) ~p~n",[ArgType]),
    RetTypeList = lists:map(fun(X) ->
                                    lists:last(X)
                            end, Bodies),
    RetType = case RetTypeList of
                  [RX] ->
                      RX;
                  RL ->
                      #type{kind=union, value=RL}
              end,
    io:format("DEBUG (genspec/Rettype) ~p~n",[RetType]),
    ets:insert(specs, {Func,#type{kind=funsig, value={ArgType,RetType}}}),
    % {Func, Mod, ModName, Clauses, Arguments, Guards, Bodies, ArgType, RetType}.
    #type{kind=funsig, value={ArgType,RetType}}.

getType(E) ->
    T = ?Expr:type(E),
    case T of
        integer ->
            io:format("DEBUG (getType/int) ~p~n",[E]),
            #type{kind = int, value = ?Expr:value(E)};
        float ->
            io:format("DEBUG (getType/float) ~p~n",[E]),
            #type{kind = T, value = ?Expr:value(E)};
        atom ->
            io:format("DEBUG (getType/atom) ~p~n",[E]),
            #type{kind = T, value = ?Expr:value(E)};
        string ->
            io:format("DEBUG (getType/string) ~p~n",[E]),
            #type{kind = T, value = ?Expr:value(E)};
        tuple ->
            io:format("DEBUG (getType/tuple) ~p~n",[E]),
            Ch = ?Query:exec(E, ?Expr:children()),
            #type{kind = T, 
                  value = lists:map(fun getType/1, Ch)};
        cons ->
            io:format("DEBUG (getType/cons) ~p~n",[E]),
            Ch = ?Query:exec(E, ?Expr:children()),
            R = lists:map(fun(X) ->
                                  case ?Expr:type(X) of
                                      cons ->
                                          [L] = ?Query:exec(X, ?Expr:children()),
                                          Ch2 = ?Query:exec(L, ?Expr:children()),
                                          lists:map(fun getType/1, Ch2);
                                      list ->
                                          Ch2 = ?Query:exec(X, ?Expr:children()),
                                          lists:map(fun getType/1, Ch2);
                                      _ ->
                                          notimplemented
                                  end
                          end, Ch),
            CL = lists:flatten(R),
            #type{kind = list,
                  value = CL};
        variable ->
            io:format("DEBUG (getType/variable) ~p~n",[E]),
            case ?Query:exec(E, ?Query:any([varref],[varbind])) of
                [] ->
                    #type{kind = error, 
                          value = unbound_variable};
                [R] ->
                    addVariableToTable(R),
                    #type{kind = T,
                          value = R}
            end;
        match_expr ->
            io:format("DEBUG (getType/match_expr) ~p~n",[E]),
            [Lhs,Rhs] = ?Query:exec(E, ?Expr:children()),
            RhsT = getType(Rhs),
            LhsT = getType(Lhs),
            case match(LhsT, RhsT) of
                {success, _} ->
                    RhsT;
                {bad_match, _} ->
                    badmatch
            end;
        application ->
            io:format("DEBUG (getType/application) ~p~n",[E]),
            [Func] = ?Query:exec(E, ?Expr:function()),
            #type{kind = funsig, value = {ArgList, RetValue}} = getTypeOfFunc(Func),
            Args = ?Query:exec(?Query:exec(E, ?Expr:child(2)),?Expr:children()),
            io:format("DEBUG (getType/application/args) ~p~n",[{ArgList,Args}]),
            lists:map(fun({Lhs, Rhs}) ->
                              RhsT = getType(Rhs),
                              match(Lhs,RhsT)
                      end,
                      lists:zip(ArgList,Args)),
            RetValue;
        infix_expr ->
            io:format("DEBUG (getType/infix_expr) ~p~n",[E]),
            [Lhs,Rhs] = ?Query:exec(E, ?Expr:children()),
            RhsT = getType(Rhs),
            LhsT = getType(Lhs),
            io:format("DEBUG (getType/infix_expr/LhsT/RhsT) ~p~n",[{LhsT,RhsT}]),
            {LT, RT, RetType} = typeOfInfix(?Expr:value(E)),
            case {match(LhsT, LT), match(RhsT, RT)} of
                {{success, _},{success, _}} ->
                    RetType;
                _ ->
                    badmatch
            end;
        prefix_expr ->
            io:format("DEBUG (getType/prefix_expr) ~p~n",[E]),
            [Ch] = ?Query:exec(E, ?Expr:children()),
            ChT = getType(Ch),
            {OperandType, RetType} = typeOfPrefix(?Expr:value(E)),
            case match(ChT, OperandType) of
                {success, _} ->
                    RetType;
                _ ->
                    badmatch
            end;
        parenthesis ->
            [Ch] = ?Query:exec(E, ?Expr:children()),
            getType(Ch);
        _ ->
            io:format("DEBUG (getType/Unimplemented) ~p~n",[E]),
            unimplemented
    end.




addVariableToTable(V) ->
    case ets:lookup(vars,V) of
        [] ->
            ets:insert(vars, {V, #type{kind = any, value = any}}); 
        _ ->
            ok
    end.

match(#type{kind=any, value=any}, X) ->
    io:format("DEBUG (match/{any,any}/X) ~p~n",[X]),
    {success, X};
match(#type{kind=X, value=any}, #type{kind=X, value=Y}) ->
    io:format("DEBUG (match/{X,any}/{X,Y}) ~p~n",[{X,Y}]),
    {success, #type{kind=X, value=Y}};
match(#type{kind = tuple, value = V}, T) ->
    io:format("DEBUG (match/{tuple,V}/T) ~p~n",[{V,T}]),    
    if
        T#type.kind =:= tuple ->
            case length(V) =:= length(T#type.value) of
                true ->
                    L = lists:zip(V, T#type.value),
                    R = lists:all(fun({X,Y}) ->
                                          {B, _} = match(X,Y),
                                          B =:= success
                                  end, L),
                    case R of
                        true ->
                            {success, T};
                        false ->
                            {bad_match, T}
                    end;
                false ->
                    {bad_match, T}
            end;
        true ->
            {bad_match, T}
    end;
match(#type{kind=list, value=X}, #type{kind=list, value=[Y]}) when is_list(X) ->
    io:format("DEBUG (match/{list,X}/{list,[Y]}) ~p~n",[{X,Y}]),
    R = lists:all(fun(Z) ->
                          {B, _} = match(Z, Y),
                          B =:= success
                  end, X),
    case R of
        true ->
            {success, #type{kind=list, value=[Y]}};
        false ->
            {bad_match, #type{kind=list, value=[Y]}}
    end;
match(#type{kind=list, value=X}, #type{kind=list, value=Y}) when is_list(X) and is_list(Y) ->
    io:format("DEBUG (match/{list,[X]}/{list,[Y]}) ~p~n",[{X,Y}]),
    case length(X) =:= length(Y) of
        true ->
            L = lists:zip(X, Y),
            R = lists:all(fun({Z,W}) ->
                                  {B, _} = match(Z,W),
                                  B =:= success
                          end, L),
            case R of
                true ->
                    {success, #type{kind=list, value=Y}};
                false ->
                    {bad_match, #type{kind=list, value=Y}}
            end;
        false ->
            {bad_match, #type{kind=list, value=Y}}
    end;
match(#type{kind=variable, value=X}, Y) ->
    io:format("DEBUG (match/{var,X}/Y) ~p~n",[{X,Y}]),
    {matchVar(X, Y), Y};
match(Y, #type{kind=variable, value=X}) ->
    io:format("DEBUG (match/Y/{var,X}) ~p~n",[{Y,X}]),
    match(#type{kind=variable, value=X}, Y);
match(#type{kind=union, value=X}, #type{kind=union, value=Y}) ->
    io:format("DEBUG (match/{union,X}/{union,Y}) ~p~n",[{X,Y}]),
    I = lists:filter(fun(Z) ->
                             case match(#type{kind=union, value=X}, Z) of
                                 {success, _} ->
                                     true;
                                 _ ->
                                     false
                             end
                     end, Y),
    case I of 
        [] ->
            badmatch;
        [Q] ->
            {success, Q};
        W ->
            {success, #type{kind=union, value=W}}
    end;                   

match(#type{kind=union, value=X}, Y) ->
    io:format("DEBUG (match/{union,X}/Y) ~p~n",[{X,Y}]),
    Intersection = lists:filter(fun(Z) ->
                                        case match(Z,Y) of
                                            {success, _} ->
                                                true;
                                            _ ->
                                                false
                                        end
                                end, X),
    case Intersection of 
        [] ->
            badmatch;
        [Q] ->
            {success, Q};
        W ->
            {success, #type{kind=union, value=W}}
    end;
match(Y, #type{kind=union, value=X}) ->
    io:format("DEBUG (match/Y/{union,X}) ~p~n",[{Y,X}]),
    match(#type{kind=union, value=X}, Y);
match(#type{kind=X, value=XX}, #type{kind=X, value=_Y}) ->
    io:format("DEBUG (match/{X,XX}/{X,Y}) ~p~n",[{X,XX,_Y}]),
    {success, #type{kind=X, value=XX}};
match(_X,Y) ->
    io:format("DEBUG (match/badmatch) ~p~n",[{_X,Y}]),
    {bad_match, Y}.



matchVar(Var, Type) ->
    [{_,PastType}] = ets:lookup(vars, Var),
    io:format("DEBUG (matchVar/PastType,Type): ~p~n", [{PastType,Type}]),
    case match(PastType, Type) of
        {success, T} ->
            ets:insert(vars, {Var, T}),
            success;
        _ ->
            bad_match
    end.

%% =========================================================================
%% replaces every variable record with its actual type

finalize(#type{kind=variable, value=V}) ->
    [{_,VarType}] = ets:lookup(vars, V),
    io:format("DEBUG (finalze/variable): ~p~n", [{V,VarType}]),
    VarType;
finalize(Type) ->
    #type{kind=X, value=V} = Type,
    case lists:member(X, [int,float,atom,string]) of
        true ->
            Type;
        false ->
            io:format("DEBUG (finalize/case/false) ~p~n",[{X,V}]),
            #type{kind=X, value=lists:map(fun finalize/1, V)}
    end.

%% =========================================================================
%% like lists:zip/2, but it takes arbitrary number of list with the same 
%% length and returns ordered n-th.

zipN(List) ->
    F = fun(N) ->
                lists:map(takeNth(N), List)
        end,
    lists:map(F, lists:seq(1,length(hd(List)))).

takeNth(N) ->
 fun(X) when is_list(X) ->
                lists:nth(N, X)
 end.




%% Only for testing

t(M,F,A) ->
    initETS(),
    analyzeSpecs(),
    genspec([M,F,A]).


%% =========================================================================
%% =========================================================================
%% INFIX OPERATORS

typeOfInfix('+') ->
    {#type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]}};
typeOfInfix('-') ->
    {#type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]}};
typeOfInfix('*') ->
    {#type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]}};
typeOfInfix('/') ->
    {#type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=float,value=any}};
typeOfInfix('div') ->
    {#type{kind=int,value=any},
     #type{kind=int,value=any},
     #type{kind=int,value=any}};
typeOfInfix('rem') ->
    {#type{kind=int,value=any},
     #type{kind=int,value=any},
     #type{kind=int,value=any}};
typeOfInfix('band') ->
    {#type{kind=int,value=any},
     #type{kind=int,value=any},
     #type{kind=int,value=any}};
typeOfInfix('bor') ->
    {#type{kind=int,value=any},
     #type{kind=int,value=any},
     #type{kind=int,value=any}};
typeOfInfix('bxor') ->
    {#type{kind=int,value=any},
     #type{kind=int,value=any},
     #type{kind=int,value=any}};
typeOfInfix('bsl') ->
    {#type{kind=int,value=any},
     #type{kind=int,value=any},
     #type{kind=int,value=any}};
typeOfInfix('bsr') ->
    {#type{kind=int,value=any},
     #type{kind=int,value=any},
     #type{kind=int,value=any}};
typeOfInfix('and') ->
    {#type{kind=any,value=any},
     #type{kind=any,value=any},
     #type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]}};
typeOfInfix('or') ->
    {#type{kind=any,value=any},
     #type{kind=any,value=any},
     #type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]}};
typeOfInfix('xor') ->
    {#type{kind=any,value=any},
     #type{kind=any,value=any},
     #type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]}};
typeOfInfix('orelse') ->
    {#type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]},
     #type{kind=any,value=any},
     #type{kind=any,value=any}};
typeOfInfix('andalso') ->
    {#type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]},
     #type{kind=any,value=any},
     #type{kind=any,value=any}}.

%% =========================================================================
%% =========================================================================
%% INFIX OPERATORS

typeOfPrefix('+') ->
    {#type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]}};
typeOfPrefix('-') ->
    {#type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]},
     #type{kind=union, value=[#type{kind=int,value=any},#type{kind=float,value=any}]}};
typeOfPrefix('bnot') ->
    {#type{kind=int,value=any},
     #type{kind=int,value=any}};
typeOfPrefix('not') ->
    {#type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]},
     #type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]}}.
