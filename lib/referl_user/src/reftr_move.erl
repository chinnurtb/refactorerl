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
%%%
%%% ============================================================================
%%% Module information

%%% @doc This module implements a common mover refactoring.
%%% Nothing else...
%%%
%%% @author Csaba Imre Zempleni <zecoaat@inf.elte.hu>

-module(reftr_move).
-vsn("$Rev$ ").

%% Callbacks
-include("user.hrl").

-export([prepare/1]).

-define(Move, reftr_move_).

-define(Return(ReturnThis), {return, ReturnThis}).

prepare(Args) ->
    try try_functions(Args)
    catch
        ?Return(Fun) -> Fun
    end.

try_functions(Args) ->
    try_type(function, Args),
    try_type(macro, Args),
    try_type(record, Args),

    throw(?RefErr0r(bad_kind)).

try_type(Type, Args) ->
    try ?Args:Type(Args) of
        Node ->
            NewArgs = transform_args(Type, Node, Args) ++
                      [{ask_missing , proplists:get_value(ask_missing, Args)}],
            Fun = call_type(Type, NewArgs),
            throw(?Return(Fun))
    catch
        ?RefError(_ThrowType, _ThrowArgs) ->
            skip
    end.

transform_args(function, Node, Args) ->
    {_, {_Mod, Fun, Arity}} = ?Fun:mod_fun_arity(Node),
    Str = ?Args:string(Args),
    File = file(Args),
    [{funlist, [{Fun, Arity}]}, {name, Str}, {file, File}];
transform_args(macro, Node, Args) ->
    MacName = ?Macro:name(Node),
    Str = ?Args:string(Args),
    File = file(Args),
    [{maclist, [MacName]}, {filename, Str}, {file, File} ];
transform_args(record, Node, Args) ->
    RecName = ?Rec:name(Node),
    Str = ?Args:string(Args),
    File = file(Args),
    [{reclist, [RecName]}, {filename, Str}, {file, File} ].

call_type(Type, Args) ->
    ModName = make_modname(Type),
    ModName:prepare(Args).

make_modname(Type) ->
    PrefixStr = ?MISC:to_list(?Move),
    TypeAtom = get_type_modatom(Type),
    TypeStr = ?MISC:to_list(TypeAtom),
    ?MISC:to_atom(PrefixStr ++ TypeStr).

file(Args) -> proplists:get_value(file, Args).

get_type_modatom(function) -> 'fun';
get_type_modatom(record) -> rec;
get_type_modatom(macro) -> mac.
