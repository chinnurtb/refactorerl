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

%%% @author Márton Wolosz <marton.wolosz@gmail.com>

-module(refusr_spec_pp).

-include("user.hrl").

-export([print/1]).
%-export([arg_type/1, arg_list_type/1, type/1, separate/2]).
%-export([resolve_query/2, resolve_clause/1, add_commas/1, compile_args/3]).

% @todo Ask: what to export?
% @todo latin-1


print(List) ->
    lists:concat(separate(lists:map(fun(H) ->
                Func      = element(1, H),
                FuncName  = reflib_function:name(Func),
                FuncArity = reflib_function:arity(Func),
                _Clause    = element(2, H),
                FunSig    = element(3, element(4, H)),
%                ?d({FuncName,FuncArity,_Clause}),
                L         = separate([type(X) || X <- element(1, FunSig)], ", "),
                %?d(L),
                ArgTypes  = lists:append(L),
                %?d(ArgTypes),
                RetValue  = type(element(2,FunSig)),
                %?d(RetValue),
                [FuncName] ++ ["/"] ++ [FuncArity] ++ [" :: ("] ++
                            [ArgTypes] ++ [") -> "] ++ RetValue
              end, List), "; ")).

% arg_type({type, Kind, Value}) ->
%     case Kind of
%         'tuple' -> lists:append([arg_type(X) || X <- Value]);
%         'list'  -> arg_list_type(Value);
%         _       -> type({type, Kind, Value})
%     end.
%
% arg_list_type(Value) ->
%     if
%         Value == nil -> [];
%         Value /= nil -> lists:append([arg_type(X) || X <- tuple_to_list(Value)])
%     end.

type({'type', 'tuple', 'any'}) ->
    ["tuple()"];
type({'type', 'tuple', Elements}) ->
    ["{"] ++ separate([type(X) || X <- Elements], ", ") ++ ["}"];
type({'type', 'list', 'nil'}) ->
    ["nil()"];
type({'type', 'list', 'any'}) ->
    ["list()"];
type({'type', 'list', {H,T}}) ->
    ["["] ++ type(H) ++ [", "] ++ type(T)++ ["]"];
type({'type', 'union', Types}) ->
    ["("] ++ separate([type(X) || X <- Types], "|") ++ [")"];
type({'type', 'any', _}) ->
    ["any()"];
type({'type', 'int', 'any'}) ->
    ["integer()"];
type({'type', 'int', N}) ->
    [lists:concat(["integer(", N, ")"])];
type({'type', 'float', 'any'}) ->
    ["float()"];
type({'type', 'float', N}) ->
    [lists:concat(["float(", N, ")"])]; % @todo: nicer float to string
type({'type', 'atom', 'any'}) ->
    ["atom()"];
type({'type', 'atom', Value}) ->
    [lists:concat(["atom(", Value, ")"])];
type({'type', 'string', Value}) ->
    [lists:concat(["string(\"", Value, "\")"])];
type({'type', 'none', 'none'}) ->
    ["none()"];
type({'type', _X, _Y}) ->
    %?d({X,Y}),
    ["none()"];
type([T]) ->
    type(T).% @todo: Ask: list or nil?

separate([], _)            -> []; % @todo: Ask: is it important?
separate([H], _)           -> H;
separate([H|T], Separator) -> H ++ [Separator] ++ separate(T, Separator).

% resolve_query(Object, patterns) ->
%     [[resolve_clause(X)] || X <- ?Query:exec(Object, ?Clause:patterns())];
% resolve_query(Object, children) ->
%     [[resolve_clause(X)] || X <- ?Query:exec(Object, ?Expr:children())].
%
% resolve_clause(Clause) ->
%     case reflib_expression:type(Clause) of
%         'variable' -> [{reflib_expression:value(Clause) ++ " : "}];
%         'tuple'    -> [{"{"}] ++ resolve_query(Clause,children) ++ [{"}"}];
%         'cons'     -> [{"["}] ++ resolve_query(Clause,children) ++ [{"]"}];
%         'list'     -> resolve_query(Clause,children);
%         _          -> [{""}] ++ resolve_query(Clause,children)
%     end.

% add_commas([]) -> [];
% add_commas([H|T]) ->
%     if
%         is_list(H) and is_list(hd(T)) -> add_commas([H] ++ [{", "}] ++ T);
%         is_list(H)                    -> [add_commas(H)] ++ add_commas(T);
%         true                          -> [H] ++ add_commas(T)
%     end.

% compile_args(Buffer, [], _) -> Buffer;
% compile_args(Buffer, ArgNames, ArgTypes) ->
%     case hd(ArgNames) of % @todo: {", "}, {"{"}, {"}"}, {"["}, {"]"} in one line!
%         {", "} -> compile_args(Buffer ++ [element(1,hd(ArgNames))],
%                                tl(ArgNames), ArgTypes);
%         {"{"}  -> compile_args(Buffer ++ [element(1,hd(ArgNames))],
%                               tl(ArgNames), ArgTypes);
%         {"}"}  -> compile_args(Buffer ++ [element(1,hd(ArgNames))],
%                               tl(ArgNames), ArgTypes);
%         {"["}  -> compile_args(Buffer ++ [element(1,hd(ArgNames))],
%                               tl(ArgNames), ArgTypes);
%         {"]"}  -> compile_args(Buffer ++ [element(1,hd(ArgNames))],
%                               tl(ArgNames), ArgTypes);
%         _      -> compile_args(Buffer ++ [element(1,hd(ArgNames))] ++
%                                [hd(ArgTypes)], tl(ArgNames), tl(ArgTypes))
%     end.

