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

%%% @author Matyas Karacsonyi <k_matyas@inf.elte.hu >

-module(refcore_loadbeam).
-vsn("$Rev: 4099 $ ").

-export([start/3]).

-include("core.hrl").

%% @spec start(FileName, string(), boolean()) -> Result
%%            FileName = atom() | list()
%%            Result   = {ok, node()} | {error, list()}
%% @doc Creates a syntactic tree from a BEAM file, which was compiled using
%% `debug_info' option.
start(Input, OutputDir, ToSave) ->
    case filelib:is_dir(OutputDir) of
        true ->
            Module = filename:basename(Input, ".beam"),
            NewName = filename:join(OutputDir, Module++".erl"), %@todo fixme
            case filelib:is_file(NewName) of
                false ->
                    case is_loaded(Module) of
                        false -> perform(Input, OutputDir, ToSave);
                        true  -> {error, "Module is already loaded"}
                    end;
                true -> {error, "Output file already exists"}
            end;
        false -> {error, "Output directory does not exist"}
    end.

perform(Input, OutputDir, ToSave) ->
    case beam_lib:chunks(Input, [abstract_code]) of
        {ok, {_, [{abstract_code, {_, AbstractCode}}]}} ->
            FileNode = ?Syn:construct(transform(AbstractCode, OutputDir)),
            ToSave andalso ?FileMan:save_file(FileNode),
            {ok, FileNode};
        {error, Reason} -> {error, Reason}
    end.

%% @spec is_loaded(string()) -> boolean()
%% @doc Checks whether the file is already loaded.
is_loaded(ModuleName) ->
    LoadedFiles = [filename:basename((?Graph:data(Node))#file.path, ".erl")
                   || Node <- ?Graph:path(?Graph:root(), [file])],
    [1 || FileName <- LoadedFiles, FileName == ModuleName] /= [].

%% @spec transform([AbstractCode], string()) -> AbstractCodeTree
%%   AbstractCodeTree = {atom(), Value} |
%%                      {{atom(), atom()}, Value} |
%%                      {{atom(), atom(), int()}, Value}
%%   Value = atom() | float() | int() | list()
%% @doc Creates a tree description from an Erlang abstract code, which
%% easily can be loaded into RefactorErl by using {@link
%% erl_syntax:construct/1}.

transform([{attribute, _, file, {FileName, 1}} | AbsCode], OutputDir) ->
    {{file, filename:join(OutputDir, FileName)},
     [Node || Node <- [transform(Node) || Node <- AbsCode], Node /= []]}.


transform({attribute, _, module, Name}) ->
    {{form, module, Name}, []};
transform({attribute, _, Value, List}) when (Value == export) or
                                            (Value == import) ->
    {{form, Value}, [{funlist, [{funref, get_type(Name), get_type(Arity)}
                                || {Name, Arity} <- List]}]};
transform({attribute, _, file, {Path, Line}}) ->
    {{form, file}, {string, Path}, {integer, Line}};
transform({attribute, _, record, {Name, Fields}}) ->
    {{form, record, Name},
     [case F of
	  {record_field, _, {atom, _, FieldName}} ->
	      {{spec_field, FieldName}, []};
	  {record_field, _, {atom, _, FieldName}, DefaultVal} ->
	      {{spec_field, FieldName}, transform(DefaultVal)}
      end || F <- Fields]};
transform({attribute, _, Value, {Name, Value}}) ->
    {{form, Value}, get_type(Name), get_type(Value)};
transform({attribute, _, Value, Attr}) ->
    {{attrib, Value}, get_type(Attr)};

transform({function, _, Name, _, Clauses}) ->
    {func, [transform({fun_clause, get_type(Name), C}) || C <- Clauses]};


transform({Kind, Header, {clause, _, Pattern, Guard, Body}}) ->
    {Kind, Header,
	   [transform(P) || P <- Pattern],
	   guard_disj(Guard),
	   [transform(B) || B <- Body]};
transform({'catch', {clause, _, Pattern, Guard, Body}}) ->
    {pattern, [tuple_to_infix(Pattern)],
              guard_disj(Guard),
              [transform(B) || B <- Body]};
transform({guard, {clause, _, [], Guard, Body}}) ->
    {guard, guard_disj(Guard), [transform(B) || B <- Body]};
transform({Kind, {clause, _, Pattern, Guard, Body}}) ->
    {Kind, [transform(P) || P <- Pattern],
	   guard_disj(Guard),
	   [transform(B) || B <- Body]};

transform({record, _, Name, Attr}) ->
    {{record_expr, Name}, [transform(A) || A <- Attr]};
transform({record, _, Var, Name, Fields}) ->
    {{record_update, Name}, transform(Var), [transform(F) || F <- Fields]};
transform({record_field, _, {atom, _, Field}, Value}) ->
    {{record_field, Field}, transform(Value)};
transform({record_field, _, Access, Field, Value}) ->
    {{record_access, Field}, transform(Access), transform(Value)};
transform({record_index, _, Name, Field}) ->
    {{record_index, Name}, transform(Field)};

transform({'fun', _, Attr}) ->
    transform(Attr);
transform({clauses, Clauses}) ->
    {'fun', [transform({fun_scope, C}) || C <- Clauses]};
transform({function, Module, Name, Arity}) ->
    {implicit_fun, {{infix_expr, ':'}, get_type(Module), get_type(Name)}, get_type(Arity)};
transform({function, Name, Arity}) ->    
    {implicit_fun, get_type(Name), get_type(Arity)};


transform({block, _, Clauses}) ->
    {block_expr, [transform(C) || C <- Clauses]};
transform({'case', _, HeadCl, Branches}) ->
    {'case', transform(HeadCl),
     [transform({pattern, B}) || B <- Branches]};
transform({'catch', _, Expr}) ->
    {'catch', transform(Expr)};
transform({'if', _, Clauses}) ->
    {'if', [transform({guard, C}) || C <- Clauses]};
transform({'receive', _, Clauses, TimeOut, TBody}) ->
    {'receive', [transform({pattern, C}) || C <- Clauses],
                [transform(TimeOut), [transform(B) || B <- TBody]]};
transform({'try', _, HeadCl, ExprCl, CatchCl, AfterCl}) ->
    {'try', [transform(H) || H <- HeadCl],
            [transform({pattern, E}) || E <- ExprCl],
            [transform({'catch', C}) || C <- CatchCl],
            [transform(A) || A <- AfterCl]};

transform(List) when (element(1, List) == nil) or (element(1, List) == cons) ->
    abslist_to_list(List);
transform({lc, _, HeadExpr, Generator}) ->
    {list_comp, transform(HeadExpr),
		[transform(G) || G <- Generator]};
transform({generate, _, Pattern, Clause}) ->
    {list_gen, transform(Pattern), transform(Clause)};

transform({tuple, _, Attr}) ->
    {tuple, [transform(A) || A <- Attr]};


transform({match, _, LeftOp, RightOp}) ->
    {match_expr, transform(LeftOp), transform(RightOp)};
transform({op, _, '!', LeftOp, RightOp}) ->
    {send_expr, transform(LeftOp), transform(RightOp)};
transform({op, _, Operator, LeftOp, RightOp}) when (Operator == 'and') or 
                                                   (Operator == 'or') or
                                                   (Operator == 'andalso') or 
                                                   (Operator == 'orelse') ->
    {{infix_expr, Operator}, {paren, transform(LeftOp)},
			     {paren, transform(RightOp)}};
transform({op, _, Operator, LeftOp, RightOp}) ->
    {{infix_expr, Operator}, transform(LeftOp),
			     transform(RightOp)};
transform({op, _, Operator, Operand}) ->
    {{prefix_expr, Operator}, transform(Operand)};


transform({bin, _, Parameters}) ->
    {bin, [transform(P) || P <- Parameters]};
transform({bc, _, HeadExpr, Generator}) ->
    {bin_comp, transform(HeadExpr),
	       [transform(G) || G <- Generator]};
transform({b_generate, _, Pattern, Clause}) ->
    {bin_gen, transform(Pattern), transform(Clause)};
transform({bin_element, _, Value, Size, TSL}) ->
    {binary_field,
     [case Size of
	  default -> transform(Value);
	  _       -> {size_qualifier, transform(Value), transform(Size)}
      end,
      get_tsl(TSL)]};

transform({call, _, Name, Parameters}) ->
    {app, transform(Name), [transform(P) || P <- Parameters]};
transform({remote, _, Module, Fun}) ->
    {{infix_expr, ':'}, transform(Module), transform(Fun)};


transform({var, _, '_'}) ->
    {joker, []};
transform({Kind, _, Value}) ->
    {Kind, Value};

transform([]) ->
    [];
transform({eof, _}) ->
    [];
transform(Error) ->
    io:format("Error: (not implemented) ~p~n", [Error]).


%% @spec abslist_to_list(tuple()) -> list()
%% @doc
abslist_to_list(List) ->
    abslist_to_list(List, []).

abslist_to_list({nil, _}, List) ->
    case List of
        [] ->
            {cons, []};
        List ->
            {cons, {list, List}}
    end;
abslist_to_list({cons, _, Value, Rest}, List) ->
    abslist_to_list(Rest, List ++ [transform(Value)]);
abslist_to_list(Value, List) ->
    {cons, {list, List}, transform(Value)}.

%% @spec get_tsl(Value) -> list()
%%       Value = atom() | list()
%% @doc
get_tsl(default) ->
    [];
get_tsl([]) ->
    [];
get_tsl([Type | Rest]) ->
    [case Type of
	{T, Size} ->
	    {{bit_size_expr, Size}, get_type(T)};
	_ ->
	    get_type(Type)
    end | get_tsl(Rest)];
get_tsl(Type) ->
    get_type(Type).

%% @spec get_type(Value) -> {Type, Value}
%%    Value = atom() | integer() | float() | list()
%%    Type = atom()
%% @doc
get_type(Value) when is_atom(Value) ->
    {atom, Value};
get_type(Value) when is_integer(Value) ->
    {integer, Value};
get_type(Value) when is_float(Value) ->
    {integer, Value};
get_type(Value) ->
    {string, Value}.

%% @spec guard_disj(list()) -> tuple()
%% @doc
guard_disj([]) ->
    [];
guard_disj([Head|Rest]) ->
    case Rest of
	[] ->
	    guard_conj(Head);
	_ ->
	    {{infix_expr, ';'}, guard_conj(Head), guard_disj(Rest)}
    end.

%% @spec guard_conj(list()) -> tuple()
%% @doc
guard_conj([Head|Rest]) ->
    case Rest of
	[] ->
	    transform(Head);
	_ ->
	    {{infix_expr, ','}, transform(Head), guard_conj(Rest)}
    end.

%% @spec tuple_to_infix(tuple()) -> tuple()
%% @doc
tuple_to_infix([{tuple, _, [Class, Pattern, _]}]) ->
    case Class of
        'throw' ->
            transform(Pattern);
        Class ->
            {{infix_expr, ':'}, transform(Class), transform(Pattern)}
    end.
