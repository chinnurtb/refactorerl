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

%%% @doc This module implements a common renamer refactoring.
%%%
%%% @author Csaba Imre Zempleni <zecoaat@inf.elte.hu>

-module(reftr_rename).
-vsn("$Rev$").

%% Callbacks
-export([prepare/1]).

%% Includes and definitions
-include("user.hrl").

-define(Rename, reftr_rename_).
-define(Return(RThis), {return, RThis}).

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    try try_functions(Args)
    catch
	?Return(Fun) -> Fun
    end.

try_functions(Args) ->
    try_type(function, Args),
    try_type(record_field, Args),
    try_type(record, Args),
    try_type(variable, Args),
    try_type(macro, Args),

    try_module(Args),
    try_include(Args),

    throw(?RefErr0r(bad_kind)).

try_type(Type, Args) ->
    try ?Args:Type(Args) of
	_Return -> 
	    Fun = call_type(Type, transform_args(Type, Args)),
	    throw(?Return(Fun))
    catch
	?RefError(ThrowType, ThrowArgs) -> 
	    check_throw_type(Type, ThrowType, ThrowArgs)
    end.

try_module(Args) ->
    Form = get_form_by_pos(Args),
    case ?Form:type(Form) of
	module -> Fun = call_type(module, transform_args(module, Args)),
		  throw(?Return(Fun));
	_Else -> void
    end.

try_include(Args) ->
    Form = get_form_by_pos(Args),
    FdepIref = ?Query:exec(Form, ?Query:seq([fdep], [iref])),
    Iref = ?Query:exec(Form, [iref]),
    case  FdepIref ++ Iref of
	[File] -> 
	    Path = ?File:path(File),
	    Fun = call_type(include, transform_args_include(Args, Path)),
	    throw(?Return(Fun));
	[] -> void
    end.

transform_args(variable, Args) ->
    Name = ?MISC:to_list(?Args:string(Args)),
    VarName = case ?Var:valid_name(Name) of
		  true  -> Name;
		  false -> throw(?RefErr0r(bad_var_name))
	      end,
    [{varname, VarName} | Args];
transform_args(macro, Args) ->
    Name = ?MISC:to_list(?Args:string(Args)),
    [{macname, Name} | Args];
transform_args(module, Args) ->
    String = ?Args:string(Args),
    NewArgs = proplists:delete(position, Args),
    [{name, String} | NewArgs];
transform_args(_Type, Args) -> 
    String = ?Args:string(Args),
    [{name, String} | Args].

transform_args_include(Args, Path) ->
    String = ?Args:string(Args),
    [{file, Path}, {filename, String}].

check_throw_type(function, Type, _Args) when 
  Type =:= fun_not_found; Type =:= pos_bad_type -> void;
check_throw_type(record, Type, _Args) when 
  Type =:= rec_not_found; Type =:= pos_bad_type -> void;
check_throw_type(record_field, Type, _Args) when 
  Type =:= rec_not_found; 
  Type =:= recfld_not_found; 
  Type =:= pos_bad_type -> void;
check_throw_type(macro, Type, _Args) when 
  Type =:= mac_not_found; Type =:= pos_bad_type -> void;
check_throw_type(variable, pos_bad_type, _Args) -> void;
check_throw_type(module, Type, _Args) when
  Type =:= mod_not_found;
  Type =:= pos_bad_type;
  Type =:= file_not_module -> void;
check_throw_type(_Else, Throw, Args) ->
    throw(?RefError(Throw, Args)).

call_type(Type, Args) ->
    ModName = make_modname(Type),
    ModName:prepare(Args).

make_modname(Type) ->
    PrefixStr = ?MISC:to_list(?Rename),
    TypeAtom = get_type_modatom(Type),
    TypeStr = ?MISC:to_list(TypeAtom),
    ?MISC:to_atom(PrefixStr ++ TypeStr).

get_type_modatom(function) -> 'fun';
get_type_modatom(record) -> rec;
get_type_modatom(record_field) -> recfield;
get_type_modatom(variable) -> var;
get_type_modatom(macro) -> mac;
get_type_modatom(module) -> mod;
get_type_modatom(include) -> header.

get_form_by_pos(Args) ->
    File = proplists:get_value(file, Args),
    Pos = proplists:get_value(position, Args),
    [Lex] = ?Query:exec(?Query:seq(?File:find(File), ?File:token(Pos))),
    [Form] = ?Query:exec(Lex, ?Token:form()),
    Form.
