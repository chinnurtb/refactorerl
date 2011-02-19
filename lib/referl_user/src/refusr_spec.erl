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
-vsn("$Rev: 5410 $"). %for emacs"

-include_lib("stdlib/include/ms_transform.hrl").

-include("user.hrl").

-export([run/1]).
-export([initETS/0, tv/0]).
-export([analyzeSpecs/0, genspec/1 ]).
% -export([whichElem/2]).

-export([t/3, td/3, zipN/1]).


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
% whichElem(Elem, List) ->
%     whichElem_acc(Elem, List, 1).
% whichElem_acc(_, [], N) ->
%     N;
% whichElem_acc(Elem, [Head|Tail], N) ->
%     case Elem == Head of
%         true ->
%             N;
%         false ->
%             whichElem_acc(Elem, Tail, N+1)
%     end.


%% =========================================================================
%% =========================================================================
%% interfaces

run(Func) ->
    initETS(),
    %analyzeSpecs(),
    Res = genspec(Func),
    ErrorList = ets:tab2list(errors),
    WarningList = ets:tab2list(warnings),
    % TODO these lines should be removed in the final version
    ets:delete(specs),
    ets:delete(errors),
    ets:delete(warnings),
    {refusr_spec_pp:print(Res), ErrorList, WarningList}.

%% =========================================================================
%% =========================================================================
%% storing and retrieving the existing -spec forms
initETS() ->
    ets:new(specs, [named_table, {keypos, 2}]),
    ets:new(errors, [bag, named_table]),
    ets:new(warnings, [bag, named_table]).

tv() ->
    tv:start().

getNewEnv() ->
    ets:new(vars, []).

getNewEnv(OldEnv) ->
    NewEnv = ets:new(vars, []),
    ets:insert(NewEnv, ets:tab2list(OldEnv)),
    NewEnv.

lookup_var(Env, Var) ->
    case ets:lookup(Env, Var) of
        [] ->
            {Var, #type{kind=any, value=any}};
        [R] ->
            R
    end.

getTypeOfFunc(Func) ->
    case ets:select(specs, sel(Func)) of
        [] ->
            genspec(Func);
        Result ->
            Result
    end.

sel(Func) -> ets:fun2ms(fun({F,C,I,FS}) when F == Func -> {F,C,I,FS} end).

updateFinal(Clause, IsFinal) ->
    case ets:lookup(specs, Clause) of
        [] ->
            error;
        [{Func, Cl, true, FunSig}] ->
            ets:insert(specs, {Func, Cl, true and IsFinal, FunSig});
        [{Func, Cl, false, FunSig}] ->
            ets:insert(specs, {Func, Cl, false, FunSig});
        [{Func, Cl, undefined, FunSig}] ->
            ets:insert(specs, {Func, Cl, IsFinal, FunSig})
    end.


%% =========================================================================
%% =========================================================================


analyzeSpecs() ->
    AllSpecForms = ?Query:exec(specs()),
    Specs = lists:map(fun buildFromSpec/1, AllSpecForms),
    lists:map(fun(X) ->
                      ets:insert(specs, X)
              end, Specs).

buildFromSpec(Form) ->
    [Modpart, Funpart]  = ?Query:exec(Form, [tattr]),
    [Mod] = ?Query:exec(Form, ?Form:module()),
    %% ?d([Mod]),
    ModName = ?Mod:name(Mod),
    {Mod_ret, Name} = analModpart(Modpart),
    %% ?d([{Mod_ret, Name}]),
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
    %% ?d([FunPart]),
    #type{kind = funsig, value = {Args, _RetV}} = FunPart,
    %% ?d([{Args,Name}]),
    [Func] = ?Query:exec(Mod, ?Fun:find(Name, length(Args))),
    % ?d(Func),
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


%% =========================================================================
%% =========================================================================
%% generates specs for a fun.


genspec(Module, Name, Arity) ->
    % ?d({Module,Name,Arity}),
    [Func] = ?Query:exec(?Query:exec(?Mod:find(Module)),
                         ?Mod:local(Name, Arity)),
    genspec(Func).
genspec(Func) ->
    [Mod] = ?Query:exec(Func, ?Fun:module()),
    _ModName = ?Mod:name(Mod),
    Clauses = ?Query:exec(Func, ?Query:seq([?Fun:definition(),
                                          ?Form:clauses()])),
    case Clauses of
        [] ->
            [{Func, Clauses, true,
              #type{kind=funsig,
                    value={[#type{kind='any', value='any'} || _X<-lists:seq(1, ?Fun:arity(Func))],
                           #type{kind='any', value='any'}}}}];
        _ -> Parts = [{X,
                       getNewEnv(),
                       ?Query:exec(X, ?Clause:patterns()),
                       ?Query:exec(X, ?Clause:guard()),
                       ?Query:exec(X, ?Clause:body())} || X <- Clauses],
             initDummyTypes(Func, Parts),
             _Types = [ typeForClause(Func, Cl, Env, Patterns, Guards, Body)
                        || {Cl, Env, Patterns, Guards, Body} <- Parts]
    end.


typeForClause(Func, Cl, Env, Patterns, Guards, Body) ->
    %?d({Cl}),
    {F,C,A,R} = {Func, Cl,
                   argType(Env, Cl, Patterns, Guards),
                   finalize(Env, lists:last(retType(Env, Cl, Body)))},
    FinalArguments = [finalize(Env, X) || X<-A],
    NewFunSig = #type{kind=funsig, value={FinalArguments, R}},
    %?d({NewFunSig}),
    ets:delete(Env),
    I = case ets:lookup(specs, C) of
            [] ->
                {error, 'something_possibly_wrong_w_ets_tables'};
            [{_F, _C, _IsFinal, NewFunSig}] ->
                ets:insert(specs, {F, C, true, NewFunSig}),
                true;
            [{_F, _C, IsFinal, _FunSig}] ->
                ets:insert(specs, {F, C, IsFinal, NewFunSig}),
                IsFinal
        end,
    {Func, Cl, I, NewFunSig}.

%% =========================================================================
%% GUARDS

processGuards(Env, Guard) ->
    guardType(Env, Guard, ?Expr:type(Guard)).

guardType(Env, Guard, infix_expr) ->
%     ?d(Guard),
    [Lhs, Rhs] = ?Query:exec(Guard, ?Expr:children()),
%     ?d({Lhs,Rhs}),
    case ?Expr:value(Guard) of
        ';' ->
            LhsT = guardType(getNewEnv(Env), Lhs, ?Expr:type(Lhs)),
            RhsT = guardType(getNewEnv(Env), Rhs, ?Expr:type(Rhs));
        'or' ->
            LhsT = guardType(getNewEnv(Env), Lhs, ?Expr:type(Lhs)),
            RhsT = guardType(getNewEnv(Env), Rhs, ?Expr:type(Rhs));
        _ ->
            LhsT = guardType(Env, Lhs, ?Expr:type(Lhs)),
            RhsT = guardType(Env, Rhs, ?Expr:type(Rhs))
    end,
    {LT, RT, RetType} = typeOfInfix(?Expr:value(Guard)),
    %?d({LhsT, RhsT, LT, RT}),
    case {match(Env, LhsT, LT), match(Env, RhsT, RT)} of
        {{success, _},{success, _}} ->
            RetType;
        _ ->
            #type{kind='none', value='none'}
    end;
guardType(Env, Guard, application) ->
%     ?d(Guard),
    [NameNode| ArgList] = ?Query:exec(Guard, ?Expr:children()),
    Name = ?Expr:value(NameNode),
    guardAppl(Env, Name, ?Query:exec(ArgList, ?Expr:children()));
guardType(Env, Guard, prefix_expr) ->
%     ?d(Guard),
    [Ch] = ?Query:exec(Guard, ?Expr:children()),
    ChT = guardType(Env, Ch, ?Expr:type(Ch)),
    {OperandType, RetType} = typeOfPrefix(?Expr:value(Guard)),
    case match(Env, ChT, OperandType) of
        {success, _} ->
            RetType;
        _ ->
            #type{kind='none', value='none'}
    end;
guardType(_Env, _Guard, T) ->
%     ?d(_Guard),
    #type{kind=T, value='any'}.

guardApplCase(Env, Node, Rhs) ->
%     ?d({Env, Node, Rhs}),
    case ?Expr:type(Node) of
        variable ->
            [V] = ?Query:exec(Node, ?Query:any([varref],[varbind])),
            match(Env, #type{kind=variable, value=V}, Rhs);
        atom ->
            true;
        _ ->
            false
    end.

guardAppl(Env, 'is_atom', [Node]) ->
    guardApplCase(Env, Node, #type{kind='atom', value='any'});
guardAppl(Env, 'is_binary', [Node]) ->
    guardApplCase(Env, Node, #type{kind='binary', value='any'});
guardAppl(Env, 'is_bitstring', [Node]) ->
    guardApplCase(Env, Node, #type{kind='bitstring', value='any'});
guardAppl(Env, 'is_boolean', [Node]) ->
    guardApplCase(Env, Node, #type{kind='union',
                                   value=[#type{kind='atom', value='true'},
                                          #type{kind='atom', value='false'}]});
guardAppl(Env, 'is_float', [Node]) ->
    guardApplCase(Env, Node, #type{kind='float', value='any'});
guardAppl(Env, 'is_function', [Node]) ->
    guardApplCase(Env, Node, #type{kind='funsig', value='any'});
guardAppl(Env, 'is_function', [Name, Arity]) ->
    guardApplCase(Env, Name, #type{kind='atom', value='any'}),
    guardApplCase(Env, Arity, #type{kind='integer', value='any'});
guardAppl(Env, 'is_integer', [Node]) ->
    guardApplCase(Env, Node, #type{kind='int', value='any'});
guardAppl(Env, 'is_list', [Node]) ->
    guardApplCase(Env, Node, #type{kind='list',
                                   value={#type{kind='any', value='any'},
                                          #type{kind='any', value='any'}}});
guardAppl(Env, 'is_number', [Node]) ->
    guardApplCase(Env, Node, #type{kind=union,
                                   value=[#type{kind=int,value=any},
                                          #type{kind=float,value=any}]});
guardAppl(Env, 'is_pid', [Node]) ->
    guardApplCase(Env, Node, #type{kind='pid', value='any'});
guardAppl(Env, 'is_port', [Node]) ->
    guardApplCase(Env, Node, #type{kind='port', value='any'});
guardAppl(Env, 'is_reference', [Node]) ->
    guardApplCase(Env, Node, #type{kind='refence', value='any'});
guardAppl(Env, 'is_tuple', [Node]) ->
    guardApplCase(Env, Node, #type{kind='tuple', value='any'});
guardAppl(_, _, _) ->
    true.



argType(Env, Clause, Patterns, Guards) ->
    Args = [getType(Env, Clause, P) || P <- Patterns],
    [processGuards(Env, G) || G <- Guards],
    Args.

retType(Env, Clause, Body) ->
    [getType(Env, Clause, B) || B <- Body].

initDummyTypes(Func, Parts) ->
    lists:map(fun({Cl, _, ArgList, _, _}) ->
                    ArgTypeList = lists:map(fun(_) ->
                                                #type{kind=any, value=any}
                                            end, ArgList),
                    % ?d(ArgTypeList),
                    ets:insert(specs,
                            {Func,
                             Cl,
                             'true',
                             #type{kind=funsig,
                                   value={ArgTypeList,#type{kind=any,
                                                            value=any}}}})
              end, Parts).


%% =========================================================================
%% =========================================================================
%% analyzes the type of Expr expression in Env envirionment

getType(Env, Clause, Expr) ->
    getType(Env, Clause, Expr, ?Expr:type(Expr)).

getType(_Env, _Clause, Expr, integer) ->
%      ?d(Expr),
    #type{kind = 'int', value = ?Expr:value(Expr)};
getType(_Env, _Clause, Expr, float) ->
%      ?d(Expr),
    #type{kind = 'float', value = ?Expr:value(Expr)};
getType(_Env, _Clause, Expr, atom) ->
%      ?d(Expr),
    #type{kind = 'atom', value = ?Expr:value(Expr)};
getType(_Env, _Clause, Expr, string) ->
%      ?d(Expr),
    #type{kind = 'string', value = ?Expr:value(Expr)};
getType(_Env, _Clause, _Expr, joker) ->
%      ?d(_Expr),
    #type{kind = 'any', value = 'any'};
getType(Env, Clause, Expr, tuple) ->
%      ?d(Expr),
    Ch = ?Query:exec(Expr, ?Expr:children()),
    #type{kind = 'tuple',
          value = [getType(Env, Clause, X) || X<-Ch]};
getType(Env, Clause, Expr, cons) ->
%      ?d(Expr),
    case ?Query:exec(Expr, ?Expr:children()) of
        [] ->
            #type{kind='list', value='nil'};
        [LP|CP] ->
            % ?d({LP, CP}),
            consToRec(Env, Clause, ?Query:exec(LP, ?Expr:children()), CP)
    end;
getType(Env, _Clause, Expr, variable) ->
%      ?d(Expr),
    case ?Query:exec(Expr, ?Query:any([varref],[varbind])) of
        [] ->
            #type{kind = error,
                  value = unbound_variable};
        [R] ->
            addVariableToTable(Env, R),
            #type{kind = 'variable',
                  value = R}
    end;
getType(Env, Clause, Expr, match_expr) ->
%      ?d(Expr),
    [Lhs,Rhs] = ?Query:exec(Expr, ?Expr:children()),
    RhsT = getType(Env, Clause, Rhs),
    LhsT = getType(Env, Clause, Lhs),
    case match(Env, LhsT, RhsT) of
        {success, _} ->
            finalize(Env, RhsT);
        {bad_match, _} ->
            #type{kind='none', value='none'}
    end;
getType(Env, Clause, Expr, application) ->
%      ?d(Expr),
    OrigFuns = ?Query:exec(Clause, ?Query:seq([?Clause:form(), ?Form:func()])),
    %?d(OrigFuns),
    Funcs = ?Query:exec(Expr, ?Expr:function()),
%     ?d({OrigFuns, Funcs}),
    case (OrigFuns =:= Funcs) or (Funcs == []) of
        true ->
%             ?d(igen),
            #type{kind=any, value=any};
        false ->
%             ?d(nem),
            [Func] = Funcs,
% ?d(Func),
            Args = ?Query:exec(?Query:exec(Expr, ?Expr:child(2)),?Expr:children()),
% ?d(Args),
            ArgsTypes = [ finalize(Env, getType(Env, Clause, Arg)) || Arg <- Args ],
% ?d(ArgsTypes),
            {IsFinal, FunSigList} = selectClauses(Env, Func, ArgsTypes),
% ?d(nem4),
            updateFinal(Clause, IsFinal),
% ?d(nem5),
            retTypeOfFunSigList(FunSigList)
    end;
getType(Env, Clause, Expr, infix_expr) ->
%      ?d(Expr),
    [Lhs,Rhs] = ?Query:exec(Expr, ?Expr:children()),
    LhsT = getType(Env, Clause, Lhs),
    RhsT = getType(Env, Clause, Rhs),
    {LT, RT, RetType} = typeOfInfix(?Expr:value(Expr)),
    % ?d({LhsT, RhsT, LT, RT}),
    case {match(Env, LhsT, LT), match(Env, RhsT, RT)} of
        {{success, _},{success, _}} ->
            RetType;
        _ ->
            #type{kind='none', value='none'}
    end;
getType(Env, Clause, Expr, prefix_expr) ->
%      ?d(Expr),
    [Ch] = ?Query:exec(Expr, ?Expr:children()),
    ChT = getType(Env, Clause, Ch),
    {OperandType, RetType} = typeOfPrefix(?Expr:value(Expr)),
    case match(Env, ChT, OperandType) of
        {success, _} ->
            RetType;
        _ ->
            #type{kind='none', value='none'}
    end;
getType(Env, Clause, Expr, parenthesis) ->
    [Ch] = ?Query:exec(Expr, ?Expr:children()),
    getType(Env, Clause, Ch);
getType(Env, Clause, Expr, case_expr) ->
%      ?d(Expr),
    [Head] = ?Query:exec(Expr, ?Query:seq([headcl],?Clause:body())),
    HeadType = getType(Env, Clause, Head),
    PatternClauses = ?Query:exec(Expr, [exprcl]),
    Parts = [{Cl, getNewEnv(Env),
              ?Query:exec(Cl, ?Clause:patterns()),
              ?Query:exec(Cl, ?Clause:guard()),
              ?Query:exec(Cl, ?Clause:body())}
                ||Cl <- PatternClauses],
    Types = [ {Cl, NewEnv,
               argType(NewEnv, Cl, Patterns, Guards),
               retType(NewEnv, Cl, Body)}
                || {Cl, NewEnv, Patterns, Guards, Body} <- Parts],
    _Res = [ {Cl, NewEnv,
             [finalize(NewEnv, X) || X <- ArgList],
             lists:last(RetTypeList)}
                || {Cl, NewEnv, ArgList, RetTypeList} <- Types],
    Ps = lists:flatten([ArgList || {_, _, ArgList, _} <- Types]),
    R = lists:flatten([RetList || {_, _, _, RetList} <- Types]),
    match(Env, HeadType, finalize(Env, #type{kind='union', value=Ps})),
    case R of
        [] ->
            error;
        [X] ->
            X;
        L when is_list(L) ->
            #type{kind='union', value=L}
    end;

getType(_Env, _Clause, _Expr, _) ->
%      ?d({unimplemented, _Expr}),
    unimplemented,
    #type{kind = 'any', value = 'any'}.

consToRec(_Env, _Clause, [], []) ->
    #type{kind = 'list', value = 'nil'};
consToRec(Env, Clause, [], [C]) ->
    getType(Env, Clause, C);
consToRec(Env, Clause, [H|T], C) ->
    #type{kind=list, value={getType(Env, Clause, H), consToRec(Env, Clause, T, C)}}.

addVariableToTable(Env, V) ->
    % ?d(addVariable),
    case ets:lookup(Env,V) of
        [] ->
            ets:insert(Env, {V, #type{kind = any, value = any}});
        _ ->
            ok
    end.

selectClauses(Env, Func, ArgsTypes) ->
    ListOfFunSigs = getTypeOfFunc(Func),
    %% ?d(ListOfFunSigs),
    R = lists:filter(fun({_F, _Cl, _IsFinal, FunSig}) ->
                            {AL, _RV} = FunSig#type.value,
                            R = [match(Env, X, Y) || {X, Y}<-lists:zip(AL, ArgsTypes)],
                            lists:all(fun({success, _}) -> true;
                                         (_)           -> false
                                      end, R)
                       end, ListOfFunSigs),
    R2 = lists:map(fun({_,_,IsFinal,FunSig}) ->
                          {IsFinal, FunSig}
                   end, R),
    {FinalList, FunSigList} = lists:unzip(R2),
    {lists:all(fun toBool/1,FinalList), FunSigList}.

retTypeOfFunSigList(FunSigList) ->
    R = lists:map(fun(#type{kind=funsig, value={_, RT}}) ->
                        RT
                  end, FunSigList),
    case R of
        []  ->
            #type{kind='none', value='none'};
        [X] ->
            X;
        L   ->
            #type{kind='union', value=L}
    end.


%% =========================================================================
%% =========================================================================
%% analyzes the type of Expr expression in Env envirionment


match(_Env, #type{kind=any, value=any}, X) ->
    % ?d(X),
    {success, X};
match(_Env, X, #type{kind=any, value=any}) ->
    % ?d(X),
    {success, X};
match(_Env, #type{kind=X, value=any}, #type{kind=X, value=Y}) ->
    % ?d({X,Y}),
    {success, #type{kind=X, value=Y}};
%% TODO újragondolni a tuple match-et
match(Env, #type{kind = tuple, value = V}, T) ->
    % ?d({V,T}),
    if
        T#type.kind =:= tuple ->
            case length(V) =:= length(T#type.value) of
                true ->
                    L = lists:zip(V, T#type.value),
                    R = lists:all(fun({X,Y}) ->
                                          {B, _} = match(Env, X, Y),
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
match(_Env, #type{kind=list, value=nil}, #type{kind=list, value=nil})->
    % ?d({nil, nil}),
    {success, #type{kind=list, value=nil}};
match(Env, #type{kind=list, value={LH,LT}}, #type{kind=list, value={LH,RT}}) ->
    % ?d({list, LT, RT}),
    match(Env, LT, RT);
match(Env, #type{kind=list, value={LH,LT}}, #type{kind=list, value={RH,RT}}) ->
    % ?d({list, LH, RH}),
    case match(Env, LH, RH) of
        {success, _} ->
            match(Env, LT, RT);
        {_, Ret} ->
            {bad_match, Ret}
    end;
match(Env, #type{kind=variable, value=X}, Y) ->
%     ?d({X,Y}),
    {matchVar(Env, X, Y), Y};
match(Env, Y, #type{kind=variable, value=X}) ->
%     ?d({Y,X}),
    {matchVar(Env, X, Y), Y};
match(Env, #type{kind=union, value=X}, #type{kind=union, value=Y}) ->
    % ?d({X,Y}),
    I = lists:filter(fun(Z) ->
                             case match(Env, #type{kind=union, value=X}, Z) of
                                 {success, _} ->
                                     true;
                                 _ ->
                                     false
                             end
                     end, Y),
    case I of
        [] ->
            {badmatch, #type{kind='none', value='none'}};
        [Q] ->
            {success, Q};
        W ->
            {success, #type{kind=union, value=W}}
    end;
match(Env, #type{kind=union, value=X}, Y) ->
%     ?d({X,Y}),
    Intersection = lists:filter(fun(Z) ->
%                                         ?d(Z),
                                        case match(Env, Z,Y) of
                                            {success, _} ->
%                                                 ?d({success,Z,Y}),
                                                true;
                                            _ ->
%                                                 ?d({fail,Z,Y}),
                                                false
                                        end
                                end, X),
%     ?d(Intersection),
    case Intersection of
        [] ->
            #type{kind='none', value='none'};
        [Q] ->
            {success, Q};
        W ->
            {success, #type{kind=union, value=W}}
    end;
match(Env, Y, #type{kind=union, value=X}) ->
    % ?d({Y,X}),
    match(Env, #type{kind=union, value=X}, Y);
match(_Env, #type{kind=X, value=XX}, #type{kind=X, value=_Y}) ->
    % ?d({X,XX,_Y}),
    {bad_match, #type{kind=X, value=XX}};
match(_Env, _X,Y) ->
%     ?d({_X,Y}),
    {bad_match, Y}.

matchVar(Env, Var, Type) ->
%     ?d({Var,Type}),
    {_,PastType} = lookup_var(Env, Var),
%     ?d(PastType),
    Res = match(Env, PastType, Type),
%     ?d(Res),
    case Res of
        {success, T} ->
%             ?d(T),
            ets:insert(Env, {Var, T}),
            success;
        _ ->
%             ?d({PastType, Type}),
            bad_match
    end.

%% =========================================================================
%% replaces every variable record with its actual type

finalize(Env, #type{kind=variable, value=V}) ->
    {_,VarType} = lookup_var(Env, V),
    VarType;
finalize(Env, #type{kind=K, value=any}) ->
    #type{kind=K, value=any};
finalize(_Env, #type{kind=list, value=nil}) ->
    #type{kind=list, value=nil};
finalize(Env, #type{kind=list, value={H,T}}) ->
    #type{kind=list, value={finalize(Env, H), finalize(Env, T)}};
finalize(Env, #type{kind=tuple, value=V}) ->
    #type{kind=tuple, value=[finalize(Env, X)||X<-V]};
finalize(Env, #type{kind=union, value=V}) ->
%    #type{kind=union, value=[finalize(Env, X)||X<-V]};
     simplify(#type{kind=union, value=uniq([finalize(Env, X)||X<-V])});
finalize(Env, Type) ->
    Type.

% finalize(Env, Type) ->
%     #type{kind=K, value=V} = Type,
%     case lists:member(K, [int,float,atom,string,any,none]) of
%         true ->
%             Type;
%         false ->
%             #type{kind=K, value=[finalize(Env, X)||X<-V]}
%     end.

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

simplify(#type{kind=union, value=[]}) ->
    #type{kind=none, value=none};
simplify(#type{kind=union, value=[X]}) ->
    X;
simplify(T) ->
    T.

uniq([]) ->
    [];
uniq([X]) ->
    [X];
uniq([Head|Tail]) ->
    uniqacc(Head,Tail).

uniqacc(Head,[Head|Tail]) ->
    uniqacc(Head,Tail);
uniqacc(Head,[]) ->
    [Head];
uniqacc(Head,[NotHead|Tail]) ->
    [Head|uniq([NotHead|Tail])].


toBool(true) ->
    true;
toBool(false) ->
    false;
toBool(undefined) ->
    true.

%% Only for testing

t(M,F,A) ->
    initETS(),
    analyzeSpecs(),
    Res = genspec(M,F,A),
    %ets:tab2list(specs),
    ets:delete(specs),
    ets:delete(errors),
    ets:delete(warnings),
    refusr_spec_pp:print(Res).
td(M,F,A) ->
    initETS(),
    analyzeSpecs(),
    Res = genspec(M,F,A),
    %ets:tab2list(specs),
    ets:delete(specs),
    ets:delete(errors),
    ets:delete(warnings),
    Res.


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
typeOfInfix(',') ->
    {#type{kind=any,value=any},
     #type{kind=any,value=any},
     #type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]}};
typeOfInfix(';') ->
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
     #type{kind=any,value=any}};
typeOfInfix('++') ->
    {#type{kind=list,value=any},
     #type{kind=list,value=any},
     #type{kind=list,value=any}};
typeOfInfix(_) ->
    {#type{kind=any,value=any},
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
     #type{kind=union, value=[#type{kind=atom,value='true'},#type{kind=atom,value='false'}]}};
typeOfPrefix(_) ->
    {#type{kind=any,value=any},
     #type{kind=any,value=any}}.

