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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created by E�tv�s Lor�nd University are Copyright 2008, E�tv�s
%%% Lor�nd University. All Rights Reserved.

%%% @doc Generic refactoring argument handler. The goal of this module is to
%%% let refactoring code be independent of parameter representation. Parameter
%%% lists are represented by property lists, but refactorings need not know
%%% the property keys used for a certain parameter. This allows multiple ways
%%% of specifying a parameter (like a function can be specified by a file and
%%% a position, or a module and a function name).
%%%
%%% Parameters are classified as follows:
%%% <dl>
%%%  <dt>Source parameters</dt>
%%%  <dd>These are usually existing entities which are to be modified by the
%%%      refactoring. Interface functions are named after the kind of entity
%%%      that is required by the refactoring, and they return graph nodes.</dd>
%%%
%%%  <dt>Target parameters</dt>
%%%  <dd>These parameter values are usually either properties (like a new
%%%      name) or entities that need not exist (like a target file which can
%%%      be created). Target interface functions thus always return a property
%%%      value, which can be used to identify target entities (e.g. a module
%%%      by its name).</dd>
%%% </dl>
%%%
%%% @type arglist() = [{Key::atom(), Value}]. Refactoring argument list, which
%%% may contain the following key-value pairs:
%%% <table border="1">
%%%  <tr><th>key</th><th>value type</th><th>constraint</th></tr>
%%%  <tr><td>`arity'</td>  <td>{@type integer()}</td><td>Non-negative</td></tr>
%%%  <tr><td>`file'</td>     <td>{@type string()}</td>
%%%                                                <td>Valid filename</td></tr>
%%%  <tr><td>`filename'</td> <td>{@type string()}</td>
%%%                                                <td>Valid filename</td></tr>
%%%  <tr><td>`function'</td> <td>{@type atom()}</td></tr>
%%%  <tr><td>`funlist'</td>
%%%                    <td>{@type [{Fun::atom(), Arity::integer()@}]}</td></tr>
%%%  <tr><td>`module'</td>   <td>{@type atom()}</td></tr>
%%%  <tr><td>`name'</td>     <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`number'</td>   <td>{@type number()}</td></tr>
%%%  <tr><td>`order'</td>    <td>{@type [integer()]}</td>
%%%                                 <td>The numbers 1..n in any order</td></tr>
%%%  <tr><td>`position'</td> <td>{@type integer()}</td><td>Positive</td></tr>
%%%  <tr><td>`posrange'</td><td>{@type {Begin::integer(),End::integer()@}}</td>
%%%                                       <td>1 &lt;= Begin &lt;= End</td></tr>
%%%  <tr><td>`reclist'</td><td>{@type [atom()]}</td></tr>
%%%  <tr><td>`text'</td>     <td>{@type string()|atom()}</td></tr>
%%%  <tr><td>`varname'</td>  <td>{@type string()}</td>
%%%                                          <td>Valid variable name</td></tr>
%%% </table>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_args).
-vsn("$Rev: 2599 $").

-export([string/1, string/3, integer/1, integer/3, name/1, atom/3]).
-export([function/1, variable/1, module/1, file/1, expression/1,
         functions/1, record/1, record_field/1, records/1]).
-export([varname/1, filename/1]).
-export([expr_range/1, order/1]).
-export([error_text/2]).

-include("refactorerl.hrl").

%%% @type node() = referl_graph:node()

%% @spec string(arglist()) -> string()
%% @doc Returns a generic target string value. This value is specified with a
%% `text' key in the argument list.
string(Args) -> string(text, "A text", Args).

%% @spec integer(arglist()) -> integer()
%% @doc Returns a generic target integer value. This value is specified with a
%% `number' key in the argument list.
integer(Args) -> integer(number, "A number", Args).

%% @spec name(arglist()) -> atom()
%% @doc Returns a generic target name as an atom (which can be used as a
%% function or module name). This value is specified with a `name' key in the
%% argument list.
name(Args) -> atom(name, "Target name", Args).


%% @spec string(atom(), string(), arglist()) -> string()
%% @doc Returns an arbitrary string parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link string/1} is insufficient.
string(Key, Desc, Args) ->
    call_arg(Args, Desc, [{stringval(Key), [Key]}]).

stringval(Key) ->
    fun
        (Atom) when is_atom(Atom) -> atom_to_list(Atom);
        (Str) when is_list(Str) -> Str;
        (_) -> throw(?RefError(arg_type, [Key, "string"]))
    end.

%% @spec integer(atom(), string(), arglist()) -> integer()
%% @doc Returns an arbitrary integer parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link integer/1} is insufficient.
integer(Key, Desc, Args) ->
    call_arg(Args, Desc, [{intval(Key), [Key]}]).

intval(Key) ->
    fun
        (Num) when is_integer(Num) -> Num;
        (Num) when is_float(Num)   -> trunc(Num);
        (_) -> throw(?RefError(arg_type, [Key, "integer"]))
    end.

%% @spec atom(atom(), string(), arglist()) -> atom()
%% @doc Returns an arbitrary atom parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link name/1} is insufficient.
atom(Key, Desc, Args) ->
    call_arg(Args, Desc, [{atomval(Key), [Key]}]).

atomval(Key) ->
    fun
        (Atom) when is_atom(Atom) -> Atom;
        (Str) when is_list(Str) ->
            case erl_scan:string(Str) of
                {ok, [{atom, _, Atom}], _} -> Atom;
                _ -> throw(?LocalError(bad_atom, [Str]))
            end;
        (_) -> throw(?RefError(arg_type, [Key, "atom"]))
    end.

%% @spec function(arglist()) -> node()
%% @doc Returns the source function node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`module', `function', and `arity' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
function(Args) ->
    call_arg(Args, "Function",
             [{fun funbyname/3, [module, function, arity]},
              {fun funbypos/2, [file, position]}]).

funbyname(Mod, Fun, Arity) ->
    ?Query:exec1(
       ?Query:seq([?Mod:find(Mod),
                   ?Mod:local(Fun, Arity)]),
       ?RefError(fun_not_found, [Mod, Fun, Arity])).

%% @todo Accept the `fun' token of implicit functions
funbypos(File, Pos) ->
    Expr = ?Query:exec1(
              ?Query:seq([?File:find(File),
                          ?File:token(Pos),
                          ?Token:expr()]),
              ?RefError(pos_bad_type, ['fun', Pos])),
    case ?Query:exec(Expr, ?Query:seq([?Expr:nameof(),
                                       ?Clause:form(),
                                       ?Form:func()])) of
        [Fun] -> Fun;
        []    -> funbyref(Expr, Pos)
    end.

funbyref(Expr, Pos) ->
    case ?Query:exec(Expr, ?Expr:parent()) of
        [] -> throw(?RefError(pos_bad_type, ['fun', Pos]));
        [Par] ->
            case ?Expr:kind(Par) of
                T when T =:= application;
                       T =:= implicit_fun ->
                    ?Query:exec1(Par, ?Expr:function(),
                                 ?RefError(pos_bad_type, ['fun', Pos]));
                infix_expr ->
                    ?Query:exec1(Par,
                                 ?Query:seq(?Expr:parent(),
                                            ?Expr:function()),
                                 ?RefError(pos_bad_type, ['fun', Pos]));
                _ ->
                    throw(?RefError(pos_bad_type, ['fun', Pos]))
            end
    end.

%% @spec functions(arglist()) -> [node()]
%% @doc Returns the source function node list. This value is specified with
%% `file' and `funlist' keys in the argument list.
functions(Args) ->
    call_arg(Args, "Function list",
             [{fun funsbyname/2, [file, funlist]}]).

funsbyname(File, Funs) ->
    Mod = ?Query:exec1(
             ?Query:seq([?File:find(File),
                         ?File:module()]),
             ?RefError(file_not_module, [File])),
    [?Query:exec1(Mod, ?Mod:local(Fun, Arity),
                  ?RefError(fun_not_found, [Fun, Arity])) ||
        {Fun, Arity} <- Funs].

%% @spec record(arglist()) -> node()
%% @doc Returns the source record node. This value is specified with
%% `file' and `position' keys in the argument list.
record(Args) ->
    call_arg(Args, "Record",
             [{fun recbypos/2, [file, position]}]).

recbypos(File, Pos) ->
    Expr = ?Query:exec1(
              ?Query:seq([?File:find(File),
                          ?File:token(Pos),
                          ?Token:expr()]),
              ?RefError(pos_bad_type, [rec, Pos])),
    case ?Query:exec(Expr, ?Query:seq([[{attr,back}],
                       [recdef]])) of
        [Record] -> Record;
        []       -> ?Query:exec1(Expr,
                 ?Query:seq([?Expr:parent(),
                         [recref]]),
                 ?RefError(pos_bad_type, [rec, Pos]))
    end.

%% @spec record_field(arglist()) -> node()
%% @doc Returns the source record field node. This value is specified
%% with `file' and `position' keys in the argument list.
record_field(Args) ->
    call_arg(Args, "Record field",
             [{fun recfieldbypos/2, [file, position]}]).

recfieldbypos(File, Pos) ->
    Expr = ?Query:exec1(
              ?Query:seq([?File:find(File),
                          ?File:token(Pos),
                          ?Token:expr()]),
              ?RefError(pos_bad_type, [recfield, Pos])),
    case ?Query:exec(Expr, [fielddef]) of
        [Field] -> Field;
        []       -> ?Query:exec1(Expr, [fieldref],
                 ?RefError(pos_bad_type, [recfield, Pos]))
    end.

%% @spec records(arglist()) -> [node()]
%% @doc Returns the source record node list. This value is specified with
%% `file' and `reclist' keys in the argument list.
records(Args) ->
    call_arg(Args, "Record list",
             [{fun recsbyname/2, [file, reclist]}]).

recsbyname(File, Recs) ->
    F = ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])),
    [?Query:exec1(F, ?File:record(Rec),
                  ?RefError(rec_not_found, [Rec])) || Rec <- Recs].

%% @spec variable(arglist()) -> node()
%% @doc Returns the source variable node. This value is specified with
%% `file' and `position' keys in the argument list.
variable(Args) ->
    call_arg(Args, "Variable",
             [{fun varbypos/2, [file, position]}]).

varbypos(File, Pos) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?File:token(Pos),
                   ?Token:expr(),
                   ?Expr:variables()]),
       ?RefError(bad_var, [])).

%% @spec varname(arglist()) -> string()
%% @doc Returns the target variable name. This value is specified with
%% a `varname' key in the argument list.
varname(Args) ->
    call_arg(Args, "Target variable name",
             [{fun varstr/1, [varname]}]).

varstr(Var) ->
    case ?Var:valid_name(Var) of
        true  -> Var;
        false -> throw(?RefErr0r(bad_var_name))
    end.

%% @spec expression(arglist()) -> node()
%% @doc Returns the source expression. This value is specified with
%% `file' and `position' keys in the argument list.
expression(Args) ->
    call_arg(Args, "Source expression",
             [{fun exprbypos/2, [file, position]}]).

exprbypos(File, Pos) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?File:token(Pos),
                   ?Token:expr()]),
       ?RefError(token_parent, [expr])).

%% @spec file(arglist()) -> node()
%% @doc Returns the source file node. This value is specified with
%% a `file' key in the argument list.
file(Args) ->
    call_arg(Args, "Source file",
             [{fun filebyname/1, [file]}]).

filebyname(File) ->
    ?Query:exec1(
       ?File:find(File),
       ?RefError(file_not_present, [File])).

%% @spec filename(arglist()) -> string()
%% @doc Returns the target file name. This value is specified with
%% a `filename' key in the argument list.
filename(Args) ->
    call_arg(Args, "Target file name",
             [{fun filestr/1, [filename]}]).

filestr(Name) ->
    Name.

%% @spec module(arglist()) -> node()
%% @doc Returns the source module node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>a `module' key, or</li>
%%  <li>a `file' key.</li>
%% </ul>
module(Args) ->
    call_arg(Args, "Source module",
             [{fun modbyname/1, [module]},
              {fun modbypos/2,  [file, position]},
              {fun modbyfile/1, [file]}]).

modbyname(Mod) ->
    ?Query:exec1(
       ?Mod:find(Mod),
       ?RefError(mod_not_found, [Mod])).

modbypos(_File, _Pos) ->
    throw(not_implemented).

modbyfile(File) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?File:module()]),
       ?RefError(file_not_module, [File])).


%% @spec expr_range(arglist()) -> [node()]
%% @doc Returns the source expression range. A valid expression range is
%% either a non-empty continuous list of top level expressions linked with the
%% same tag to a clause, or a single expression. This value is specified with
%% `file' and `posrange' keys in the argument list.
expr_range(Args) ->
    call_arg(Args, "Expression range",
             [{fun expr_range/2, [file, posrange]}]).

expr_range(File, {Pos1, Pos2}) ->
    F     = ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])),
    Start = ?Query:exec1(F, ?File:token(Pos1), ?RefError(illegal_pos, [File, Pos1])),
    End   = ?Query:exec1(F, ?File:token(Pos2), ?RefError(illegal_pos, [File, Pos2])),
    Exprs = range(joint(?Syn:root_path(Start, left),
                        ?Syn:root_path(End, right))),
    [First] = ?Query:exec(hd(Exprs), ?Syn:first_leaf()),
    [Last]  = ?Query:exec(lists:last(Exprs), ?Syn:last_leaf()),
    ?Check(First =:= Start andalso Last =:= End,
           ?RefErr0r(bad_range)),
    Exprs.

%% Turns the result of `joint/2' into a list of expressions.
range({Expr, sub, F, L}) ->
    Chld = ?Syn:children(Expr),
    case {hd(Chld), lists:last(Chld)} of
        {{sub,F}, {sub,L}} -> [Expr];
        _ ->
            FI = ?Syn:index(Expr, sub, F),
            LI = ?Syn:index(Expr, sub, L),
            ?ESG:path(Expr, [{sub, {FI, LI+1}}])
    end;
range({Expr, elex, _, _}) ->
    [Expr];
range({Cls,  CT, F, L}) when CT == body; CT == pattern; CT == guard ->
    FI = ?Syn:index(Cls, CT, F),
    LI = ?Syn:index(Cls, CT, L),
    ?ESG:path(Cls, [{CT, {FI, LI+1}}]);
range({Lex, llex, _, _}) ->
    %% No multiple parents here
    [{Tag, Parent}] = ?Syn:parent(Lex),
    range({Parent, Tag, Lex, Lex});
range(_) ->
    throw(?RefErr0r(bad_range)).

%% @spec joint(P1::[{atom(), node()}], P2::[{atom(), node()}]) ->
%%         {node(), atom(), node(), node()}
%% @doc Returns the fork point of the argument paths. `P1' and `P2' must have
%% a common prefix which is continued with the same link tag (the prefix may
%% be empty). The returned tuple contains the last common node after the
%% longest such prefix, the common link tag, and the continuing nodes (they
%% may be the same when the two paths are continued with different link tags).
joint([{L,N}=H|T1], [H|T2])     -> joint(T1, T2, {?Graph:root(), L, N});
joint([{L,N1}|_],   [{L,N2}|_]) -> {?Graph:root(), L, N1, N2};
joint(_, _)                     -> throw(?RefErr0r(bad_range)).

%% Drop the common prefix. The third argument contains the last two common
%% nodes and the link tag between them.
joint([{L,N}=H|T1], [H|T2],      {_,_,P}) -> joint(T1, T2, {P,L,N});
joint([{L, N1}|_],  [{L, N2}|_], {_,_,P}) -> {P, L, N1, N2};
joint(_,           _,            {P,L,C}) -> {P, L, C, C}.


%% @spec order(arglist()) -> [integer()]
%% @doc Returns the target order (a list of consecutive integers starting from
%% 1, in an arbitrary order). This value is specified with an `order' key in
%% the argument list.
order(Args) ->
    call_arg(Args, "Order",
             [{fun orderbylst/1, [order]}]).

orderbylst(List) ->
    if
        is_list(List) ->
            ListLength = length(List),
            case lists:sort(List) =:= lists:seq(1, ListLength) of
                true ->
                    List;
                false ->
                    throw(?RefError(bad_order, [ListLength]))
            end;
        true ->
            throw(?RefError(order_not_list, []))
    end.

%%% ----------------------------------------------------------------------------
%%% Generic argument finder

%% @spec call_arg(arglist(), string(), ArgSpec) -> term()
%%       ArgSpec = [{fun(), [atom()]}]
%% @doc Applies the first argument specification that matches `Args'. Matching
%% means that every property key that is specified in the argument
%% specification list exists in `Args'; in this case, the function provided in
%% the specification is called with the property values passed as individual
%% arguments to the function. When no math is found, an exception is thrown
%% using the argument description in `Desc'.
call_arg(_Args, Desc, []) ->
    throw(?RefError(missing_arg, [Desc]));
call_arg(Args, Desc, [{Fun, Params} | Tail]) ->
    try
        apply(Fun, [arg(Args, Name) || Name <- Params])
    catch
        throw:?LocalError(not_found, _) ->
            call_arg(Args, Desc, Tail)
    end.

arg(Args, Name) ->
    case proplists:lookup(Name, Args) of
        {Name, Value} -> Value;
        none -> throw(?LocalError(not_found, [Name]))
    end.


error_text(bad_atom, [Text]) ->
    ["\"", Text, "\" is not a valid atom (use single quotes)"].