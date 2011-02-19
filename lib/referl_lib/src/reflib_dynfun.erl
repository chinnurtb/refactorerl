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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc High level function-related operations. This module contains
%%% functions that expect a function semantical node as their parameter (or
%%% return a query that expects a function semantical node as starting point).
%%%
%%% @author Matyas Karacsonyi <k_matyas@inf.elte.hu>

%%% ============================================================================
%%% Module information

-module(reflib_dynfun).
-vsn("$Rev: $ ").

-include("lib.hrl").

%% =============================================================================
%% Exports

%% Queries
-export([ambdyn_call/0, ambdyn/0, dynfun_call/0, dynfun/0]).

%% Transformations
-export([collect/3, transform/1]).

%% =============================================================================
%% Queries starting from functions

ambdyn_call() ->
    ?Query:all([{may_be, back}, {ambfuneref, back}],
	       [{ambfuneref, back}]).

ambdyn() ->
    [ambfuneref, may_be].

dynfun_call() ->
    [{dynfuneref, back}].

dynfun() ->
    [dynfuneref].

%% =============================================================================
%% Dynamic function related transformations

collect(TransformType, Def, Parameter) ->
    case TransformType of
	move           -> {move,   collect_move(Def, Parameter)};
	{rename, mod}  -> {rename, collect_rename(Def, Parameter, 1)};
	{rename, func} -> {rename, collect_rename(Def, Parameter, 2)};
	Kind           -> {param,  collect_param(Def, Kind, Parameter)}
    end.

transform({Kind, Updates}) ->
    [begin
	 {Node, Graph} =
	     case {Kind, Update} of
		 {rename, {From, To}} ->
		     {From, [To]};

		 {rename, {Appl, OrigName, NewName, VarName}} ->
		     {Appl, 
		      [{paren,
			{'case', copy(Appl),
			 [{pattern, [{atom, OrigName}], [], [{atom, NewName}]},
			  {pattern, [{var, VarName}], [], [{var, VarName}]}]}}]};

		 {rename, {mfa, Appl, Mod, Fun, Param, ModName, OrigName,
			   NewName, VarFun}} ->
		     {Appl,
		      [{app,
			{{infix_expr, ':'},
			 copy(Mod),
			 {'case', {tuple, [copy(Mod), copy(Fun)]},
			  [{pattern,
			    [{tuple, [{atom, ModName}, {atom, OrigName}]}],
			    [],
			    [{atom, NewName}]},
			   {pattern, [{var, VarFun}], [], [{var, VarFun}]}]}},
			[copy(P) || P <- Param]}]};

		 {rename, {mfa, Appl, Mod, Fun, Param, ModName, OrigName,
			   NewName, VarMod, VarFun}} ->
		     {Appl,
		      [{match_expr, {var, VarMod}, copy(Mod)},
		       {app,
			{{var, VarMod}, ':',
			 {'case', {tuple, [{var, VarMod}, copy(Fun)]},
			  [{pattern,
			    [{tuple, [{atom, ModName}, {atom, OrigName}]}],
			    [],
			    [{atom, NewName}]},
			   {pattern, [{var, VarFun}], [], [{var, VarFun}]}]}},
			[copy(P) || P <- Param]}]};

		 {rename, {apply, Appl, Mod, Fun, Param, ModName, OrigName,
			   NewName, VarFun}} ->
		     {Appl,
		      [{app,
			{atom, apply},
			[copy(Mod),
			 {'case', {tuple, [copy(Mod), copy(Fun)]},
			  [{pattern,
			    [{tuple, [{atom, ModName}, {atom, OrigName}]}],
			    [],
			    [{atom, NewName}]},
			   {pattern,
			    [{tuple, [{joker, []}, {var, VarFun}]}],
			    [],
			    [{var, VarFun}]}]},
			 {list, [copy(P) || P <- Param]}]}]};
		 
		 {rename, {apply, Appl, Mod, Fun, Param, ModName, OrigName,
			   NewName, VarMod, VarFun}} ->
		     {Appl,
		      [{match_expr, {var, VarMod}, copy(Mod)},
		       {app,
			{atom, apply},
			[{var, VarMod},
			 {'case', {tuple, [{var, VarMod}, copy(Fun)]},
			  [{pattern,
			    [{tuple, [{atom, ModName}, {atom, OrigName}]}],
			    [],
			    [{atom, NewName}]},
			   {pattern,
			    [{tuple, [{joker, []}, {var, VarFun}]}],
			    [],
			    [{var, VarFun}]}]},
			 {list, [copy(P) || P <- Param]}]}]};
		  
		 {move, {mfa, Appl, Mod, OrigMod, NewMod, Fun,
			 TransFun, _, Param, VarMod, VarFun, _}} ->
		     {Appl,
		      [{match_expr, {var, VarMod}, copy(Mod)},
		       {match_expr, {var, VarFun}, copy(Fun)},
		       {app,
			{{paren,
			 {'if',
			  [{guard,
			    {{paren, {{var, VarMod}, '==', {atom, OrigMod}}},
			     'andalso',
			     {paren, {{var, VarFun}, '==', {atom, TransFun}}}},
			    [{atom, NewMod}]},
			   {guard, {atom, true}, [{var, VarMod}]}]}},
			':', {var, VarFun}}, [copy(P) || P <- Param]}]};
		 
		 {move, {apply, Appl, Mod, OrigMod, NewMod, Fun,
			 TransFun, Arity, Param, VarMod, VarFun, VarParam}} ->
		     {Appl,
		      [{match_expr, {var, VarMod}, copy(Mod)},
		       {match_expr, {var, VarFun}, copy(Fun)},
		       {match_expr, {var, VarParam}, copy(Param)},
		       {app,
			{atom, apply},
			[{'if',
			  [{guard,
			    {{paren, {{var, VarMod}, '==', {atom, OrigMod}}},
			     'andalso',
			     {{paren, {{var, VarFun}, '==', {atom, TransFun}}},
			     'andalso',
			     {paren,
			      {{app, {atom, length}, [{var, VarParam}]},
			       '==',
			       {integer, Arity}}}}},
			    [{atom, NewMod}]},
			   {guard, {atom, true}, [{var, VarMod}]}]},
			 {var, VarFun},
			 {var, VarParam}]}]};

		 {param, {apply, Appl, Param, Mod, Fun, MName, FName, FArity,
			  VarMod, VarFun, VarParam, Transf, VarTrans}} ->
		     NewParam =
			 case Transf of
			     {tuple,   TParam} -> comp_tuple_apply(TParam, VarParam, VarTrans);
			     {int_rec, TParam} -> comp_rec_apply(TParam, VarParam, VarTrans);
			     {reord,   TParam} -> comp_reord_apply(TParam, VarParam, VarTrans)
			 end,
		     {Appl,
		      [{match_expr, {var, VarMod}, copy(Mod)},
		       {match_expr, {var, VarFun}, copy(Fun)},
		       {match_expr, {var, VarParam}, copy(Param)},
		       {app, {atom, apply},
			[{var, VarMod},
			 {var, VarFun},
			 {'case', {tuple,
				   [{var, VarMod},
				    {var, VarFun},
				    {app, {atom, length}, [{var, VarParam}]}]},
			  [{pattern,
			    [{tuple, [{atom, MName}, {atom, FName}, {integer, FArity}]}],
			    [],
			    [NewParam]},
			   {pattern,
			    [{joker, []}], [], [{var, VarParam}]}]}]}]};
		 
		 {param, {mfa, Appl, Param, Mod, Fun, MName, FName, _,
			  VarMod, VarFun, _, Transf, _}} ->
		     NewParam = 
			 case Transf of
			     {tuple,   TParam} -> comp_tuple_mfa(TParam, Param);
			     {int_rec, TParam} -> comp_rec_mfa(TParam, Param);
			     {reord,   TParam} -> comp_reord_mfa(TParam, Param)
			 end,
		     {Appl,
		      [{match_expr, {var, VarMod}, copy(Mod)},
		       {match_expr, {var, VarFun}, copy(Fun)},
		       {'case', {tuple, [{var, VarMod}, {var, VarFun}]},
			[{pattern, 
			  [{tuple, [{atom, MName}, {atom, FName}]}],
			  [],
			  [{app, {{atom, MName}, ':', {atom, FName}}, NewParam}]},
			 {pattern,
			  [{joker, []}],
			  [],
			  [{app, {{var, VarMod}, ':', {var, VarFun}},
			    [copy(P) || P <- Param]}]}]}]}
	     end,
	 
	 replace(Node, Graph)
     end || Update <- Updates].

collect_move(Def, NewMod) ->
    OrigMod     = ?Mod:name(hd(?Query:exec(Def, ?Fun:module()))),
    TransFun    = ?Fun:name(Def),
    Arity       = ?Fun:arity(Def),
    DynFunCalls = ?Query:exec(Def, ?Query:all(dynfun_call(), ambdyn_call())),
    NewModName  = ?Mod:name(NewMod),

    [begin
	 [Appl]  = ?Query:exec(DCall, [{esub, 1}]),
	 CType =
	     case ?Expr:value(Appl) of
		 apply -> apply;
		 _     -> mfa
	     end,

	 Param =
	     case CType of
		 apply ->
		     hd(?Query:exec(DCall, ?Query:seq([{esub, 2}], [{esub, 3}])));
		 mfa ->
		     ?Query:exec(DCall, ?Query:seq([{esub, 2}], ?Expr:children()))
	     end,

	 [Mod] = funcall_parameter(DCall, 1),
	 [Fun] = funcall_parameter(DCall, 2),
	 
	 VarMod   = ?Var:new_varname(DCall, "Mod"),
	 VarFun   = ?Var:new_varname(DCall, "Fun", [VarMod]),
	 VarParam = ?Var:new_varname(DCall, "Param", [VarMod, VarFun]),
	 {CType, DCall, Mod, OrigMod, NewModName, Fun, TransFun, Arity, Param, VarMod, VarFun, VarParam}
     end || DCall <- DynFunCalls].

collect_param(Def, TKind, TParam) ->
    MName       = ?Mod:name(hd(?Query:exec(Def, ?Fun:module()))),
    FName       = ?Fun:name(Def),
    FArity      = ?Fun:arity(Def),
    DynFunCalls = ?Query:exec(Def, ?Query:all(dynfun_call(), ambdyn_call())),
    
    [begin
	 [Appl]  = ?Query:exec(DCall, [{esub, 1}]),
	 CType =
	     case ?Expr:value(Appl) of
		 apply -> apply;
		 _     -> mfa
	     end,
	 
	 Param =
	     case CType of
		 apply ->
		     hd(?Query:exec(DCall, ?Query:seq([{esub, 2}], [{esub, 3}])));
		 mfa ->
		     ?Query:exec(DCall, ?Query:seq([{esub, 2}], ?Expr:children()))
	     end,
	 
	 [Mod] = funcall_parameter(DCall, 1),
	 [Fun] = funcall_parameter(DCall, 2),
	 VarMod   = ?Var:new_varname(DCall, "Mod"),
	 VarFun   = ?Var:new_varname(DCall, "Fun", [VarMod]),
	 VarParam = ?Var:new_varname(DCall, "Fun", [VarMod, VarFun]),
	 VarTrans = gen_var(DCall, case TKind of
				       int_rec ->
					   {_, Fields} = TParam,
					   length(Fields);
				       _       -> FArity
				   end, [VarMod, VarFun, VarParam]),
	 
	 {CType, DCall, Param, Mod, Fun, MName, FName, FArity,
	  VarMod, VarFun, VarParam, {TKind, TParam}, VarTrans}
     end || DCall <- DynFunCalls].

gen_var(_, 0, _) ->
    [];
gen_var(Node, Count, Buffer) ->
    Var = ?Var:new_varname(Node, "Var", Buffer),
    [Var | gen_var(Node, Count - 1, [Var|Buffer])].

collect_rename(Def, NewName, Edge) ->
    OrigName =
	case Edge of
	    1 -> ?Mod:name(Def);
	    2 -> ?Fun:name(Def)
	end,
    Route =
	case Edge of
	    1 -> ?Query:seq(?Mod:locals(), ?Query:all(dynfun_call(), ambdyn_call()));
	    2 -> ?Query:all(dynfun_call(), ambdyn_call())
	end,

    DynFunCalls = lists:usort(?Query:exec(Def, Route)),
    ModName     =
	case Edge of
	    1 -> OrigName;
	    2 -> ?Mod:name(hd(?Query:exec(Def, ?Fun:module())))
	end,
    
    [begin
	 [TransParam] = funcall_parameter(DCall, Edge),
	 [Mod]        = funcall_parameter(DCall, 1),

	 Source = [Node || Node <- ?Dataflow:reach([TransParam], [back]),
			   ?Expr:type(Node) == atom
			       andalso ?Expr:value(Node) == OrigName],

	 VarName     = ?Var:new_varname(TransParam, "Var"),
	 FunCallType = get_funcall_type(DCall),
	 Param       =
	     case FunCallType of
		 apply -> ?Query:exec(DCall, ?Query:seq([{esub, 2}], [{esub, 3}]));
		 mfa   -> ?Query:exec(DCall, ?Query:seq([{esub, 2}], ?Expr:children()))
	     end,
	 
	 case {Source, Edge, ?Expr:type(Mod)} of
	     {[], 2, atom} ->
		 {TransParam, OrigName, NewName, VarName};
	     
	     {[], 2, variable} ->
		 {FunCallType, DCall, Mod, TransParam, Param, ModName, OrigName,
		  NewName, VarName};
	     
	     {[], 2, _} ->
		 VarMod = ?Var:new_varname(TransParam, "Var", [VarName]),
		 {FunCallType, DCall, Mod, TransParam, Param,  ModName, OrigName,
		  NewName, VarMod, VarName};
	     
	     {[],  _, _} ->
		 {TransParam, OrigName, NewName, VarName};
	     
	     {[Start], _, ModType} ->
		 Flow = [Node || Node <- ?Dataflow:reach([Start], [{back, false}]),
				 ?Expr:type(Node) == variable],
		 VarName = ?Var:new_varname(TransParam, "Var"),
		 
		 case {has_dity_usage(Flow), Edge, ModType} of
		     {false, 2, atom} ->
			 {Start, {atom, NewName}};
		     
		     {true, 2, atom} ->
			 {TransParam, OrigName, NewName, VarName};
		     
		     {_, 2, variable} ->
			 {FunCallType, DCall, Mod, TransParam, Param, ModName,
			  OrigName, NewName, VarName};
		     
		     {_, 2, _} ->
			 VarMod = ?Var:new_varname(TransParam, "Var", [VarName]),
			 {FunCallType, DCall, Mod, TransParam, Param,  ModName,
			  OrigName, NewName, VarMod, VarName};
		     
		     {false, _, _} ->
			 {Start, {atom, NewName}};
		     
		     {true, _, _} ->
			 VarName = ?Var:new_varname(TransParam, "Var"),
			 {TransParam, OrigName, NewName, VarName}
		 end
	 end
     end || DCall <- DynFunCalls].

funcall_parameter(DCall, Edge) ->   
    Route =
	case get_funcall_type(DCall) of
	    apply -> ?Query:seq([{esub, 2}], [{esub, Edge}]);
	    _     -> ?Query:seq([{esub, 1}], [{esub, Edge}])
	end,
    ?Query:exec(DCall, Route).

get_funcall_type(DCall) ->
    [FunCall] = ?Query:exec(DCall, [{esub, 1}]),
    case ?Expr:value(FunCall) of
	apply -> apply;
	_     -> mfa
    end.

has_dity_usage([]) ->
    false;
has_dity_usage([Node|Rest]) ->
    [{_, Parent}] = ?Syn:parent(Node),
    case ?Expr:type(Parent) of
	arglist ->
	    [Fun] = ?Query:exec(Parent,
				?Query:seq([{esub, back}],
					   ?Query:any([[funeref],
						       [ambfuneref],
						       [dynfuneref]]))),
	    ?Fun:is_dirty(Fun);
	_ ->
	    false
    end orelse has_dity_usage(Rest).

comp_tuple_apply({IdxFrom, IdxLen}, VarParam, VarTrans) ->
    NewParam = comp_tuple(VarTrans, 1, IdxFrom, IdxLen),
    [{match_expr,
      {cons, {list, [{var, Var} || Var <- VarTrans]}},
      {var, VarParam}},
    {cons, {list, NewParam}}].

comp_tuple([Var|RestVar], Cnt, IdxFrom, IdxLen) when Cnt == IdxFrom ->
    {Tuple, Rest} = comp_tuple(RestVar, IdxLen - 1, [{var, Var}]),
    [{tuple, Tuple} | [{var, R} || R <- Rest]];
comp_tuple([Var|RestVar], Cnt, IdxFrom, IdxLen) ->
    [{var, Var} | comp_tuple(RestVar, Cnt + 1, IdxFrom, IdxLen)].

comp_tuple(RestVar, 0, Buffer) ->
    {Buffer, RestVar};
comp_tuple([Var|RestVar], Cnt, Buffer) ->
    comp_tuple(RestVar, Cnt - 1, Buffer ++ [{var, Var}]).

comp_tuple_mfa({IdxFrom, IdxLen}, Param) ->
    comp_tuple_mfa(Param, 1, IdxFrom, IdxLen).

comp_tuple_mfa([Var|RestVar], Cnt, IdxFrom, IdxLen) when Cnt == IdxFrom ->
    {Tuple, Rest} = comp_tuple_mfa(RestVar, IdxLen - 1, [copy(Var)]),
    [{tuple, Tuple} | [copy(R) || R <- Rest]];
comp_tuple_mfa([Var|RestVar], Cnt, IdxFrom, IdxLen) ->
    [copy(Var) | comp_tuple_mfa(RestVar, Cnt + 1, IdxFrom, IdxLen)].

comp_tuple_mfa(RestVar, 0, Buffer) ->
    {Buffer, RestVar};
comp_tuple_mfa([Var|RestVar], Cnt, Buffer) ->
    comp_tuple_mfa(RestVar, Cnt - 1, Buffer ++ [copy(Var)]).

comp_rec_apply({RName, RFields}, VarParam, VarTrans) ->
    [{match_expr,
      {cons, {list, [{tuple, [{var, Var} || Var <- VarTrans]}]}},
      {var, VarParam}},
      {{record_expr, RName},
       lists:zipwith(
	 fun (RF, Var) ->
		 {{record_field, RF}, {var, Var}}
	 end, RFields, VarTrans)}].

comp_rec_mfa({RName, RFields}, Param) ->
    [{{record_expr, RName},
      lists:zipwith(
	fun (RF, P) ->
		{{record_field, RF}, copy(P)}
	end, RFields, ?Query:exec(Param, ?Expr:children()))}].

comp_reord_apply(TParam, VarParam, VarTrans) ->
    [{match_expr,
      {cons, {list, [{var, Var} || Var <- VarTrans]}},
      {var, VarParam}},
     {cons, {list, [{var, lists:nth(N, VarTrans)} || N <- TParam]}}].

comp_reord_mfa(TParam, Param) ->
    [copy(lists:nth(N, Param)) || N <- TParam].

copy(Node) ->
    proplists:get_value(Node, ?Syn:copy(Node)).

replace(From, Graph) ->
    case ?Syn:parent(From) of
        [{_, Parent}] ->
            ?Syn:replace(Parent, {node, From}, [?Syn:construct(G) || G <- Graph]),
	    ?Transform:touch(Parent);
        _ ->
            []
    end.
