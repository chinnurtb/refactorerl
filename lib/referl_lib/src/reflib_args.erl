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
%%%  <tr><td>`create_new_file'</td> <td>{@type bool()}</td></tr>
%%%  <tr><td>`file'</td>     <td>{@type string()}</td>
%%%                                                <td>Valid filename</td></tr>
%%%  <tr><td>`filename'</td> <td>{@type string()}</td>
%%%                                                <td>Valid filename</td></tr>
%%%  <tr><td>`function'</td> <td>{@type atom()}</td></tr>
%%%  <tr><td>`funlist'</td>
%%%                    <td>{@type [{Fun::atom(), Arity::integer()@}]}</td></tr>
%%%  <tr><td>`funclusters'</td>
%%       <td>{@type [[{Mod::atom(), Fun::atom(), Arity::integer()@}]]}</td></tr>
%%%  <tr><td>`has_side_effect'</td> <td>{@type bool()}</td></tr>
%%%  <tr><td>`macname'</td>    <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`macro'</td>    <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`maclist'</td><td>{@type [atom()|string()]}</td></tr>
%%%  <tr><td>`module'</td>   <td>{@type atom()}</td></tr>
%%%  <tr><td>`name'</td>     <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`number'</td>   <td>{@type number()}</td></tr>
%%%  <tr><td>`order'</td>    <td>{@type [integer()]}</td>
%%%                                 <td>The numbers 1..n in any order</td></tr>
%%%  <tr><td>`position'</td> <td>{@type integer()}</td><td>Positive</td></tr>
%%%  <tr><td>`posrange'</td><td>{@type {Begin::integer(),End::integer()@}}</td>
%%%                                       <td>1 &lt;= Begin &lt;= End</td></tr>
%%%  <tr><td>`record'</td><td>{@type atom()}</td></tr>
%%%  <tr><td>`recfield'</td><td>{@type atom()}</td></tr>
%%%  <tr><td>`reclist'</td><td>{@type [atom()]}</td></tr>
%%%  <tr><td>`text'</td>     <td>{@type string()|atom()}</td></tr>
%%%  <tr><td>`varname'</td>  <td>{@type string()}</td>
%%%                                          <td>Valid variable name</td></tr>
%%% </table>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reflib_args).
-vsn("$Rev: 4838 $ ").

-export([string/1, string/3, integer/1, integer/3, name/1, atom/3, bool/3]).
-export([function/1, variable/1, module/1, file/1, expression/1,
         functions/1, record/1, record_field/1, records/1,
         macro/1, macros/1, funclusters/1]).
-export([varname/1, filename/1, macname/1, macuse/1]).
-export([expr_range/1, order/1]).
-export([error_text/2]).

-include("lib.hrl").
-define(CallArg(A,S,L),?MISC:call_arg(A,S,L)).

%%% @type node() = referl_graph:node()

%%% ============================================================================
%%% Error text

error_text(bad_atom, [Text]) ->
    ["\"", Text, "\" is not a valid atom (use single quotes)"];
error_text(bad_funclusters, [Clusters]) ->
    ["\"", io_lib:print(Clusters), "\" is not a valid function clustering"].


%%% ============================================================================
%%% Database independent functions

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
    ?CallArg(Args, Desc, [{stringval(Key), [Key]}]).

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
    ?CallArg(Args, Desc, [{intval(Key), [Key]}]).

intval(Key) ->
    fun
        (Num) when is_integer(Num) -> Num;
        (Num) when is_float(Num)   -> trunc(Num);
        (_) -> throw(?RefError(arg_type, [Key, "integer"]))
    end.

%% @spec bool(atom(), string(), arglist()) -> bool()
%% @doc Returns an arbitrary boolean parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message.
bool(Key, Desc, Args) ->
    ?CallArg(Args, Desc, [{boolval(Key), [Key]}]).

boolval(Key) ->
    fun
        (B) when is_boolean(B) -> B;
        (_) -> throw(?RefError(arg_type, [Key, "bool"]))
    end.

%% @spec atom(atom(), string(), arglist()) -> atom()
%% @doc Returns an arbitrary atom parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link name/1} is insufficient.
atom(Key, Desc, Args) ->
    ?CallArg(Args, Desc, [{atomval(Key), [Key]}]).

%% Note: since `erl_scan:string/1` identifies `spec` as a category in itself
%%       and not as an atom, it is dealt with as a unique case.
atomval(Key) ->
    fun
        (Atom) when is_atom(Atom) -> Atom;
        ("spec") -> spec;
        (Str) when is_list(Str) ->
            case erl_scan:string(Str) of
                {ok, [{atom, _, Atom}], _} -> Atom;
                _ -> throw(?LocalError(bad_atom, [Str]))
            end;
        (_) -> throw(?RefError(arg_type, [Key, "atom"]))
    end.

%% @spec filename(arglist()) -> string()
%% @doc Returns the target file name. This value is specified with
%% a `filename' key in the argument list.
filename(Args) ->
    string(filename, "Target file name", Args).

%% @spec order(arglist()) -> [integer()]
%% @doc Returns the target order (a list of consecutive integers starting from
%% 1, in an arbitrary order). This value is specified with an `order' key in
%% the argument list.
order(Args) ->
    ?CallArg(Args, "Order",
             [{fun orderbylst/1, [order]}]).

orderbylst(List) ->
    if
        is_list(List) ->
            ListLength = length(List),
            case lists:sort(List) =:= ?MISC:seq2(1, ListLength) of
                true ->
                    List;
                false ->
                    throw(?RefError(bad_order, [ListLength]))
            end;
        true ->
            throw(?RefError(order_not_list, []))
    end.


%%% ============================================================================
%%% Database dependent functions

funclusters(Args) ->
    ?CallArg(Args, "Function clusters",
        [{fun funclusters_/1, [funclusters]}]).

funclusters_(Clusters) ->
    try
        [[ begin
               {M,F,A}=Fun,
               true = is_atom(M) andalso is_atom(F) andalso is_integer(A)
           end
           || Fun <- Cluster ] || Cluster <- Clusters],
        Clusters
    catch
        _:_ -> throw(?RefError(bad_funclusters,[Clusters]))
        %@todo badmatch
    end.


%% @spec function(arglist()) -> node()
%% @doc Returns the source function node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`module', `function', and `arity' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
function(Args) ->
    ?CallArg(Args, "Function",
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
            case ?Expr:type(Par) of
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
    ?CallArg(Args, "Function list",
             [{fun funsbyname/2, [file, funlist]},
              {fun funbypos_/2, [file, position]}]).

funsbyname(File, Funs) ->
    Mod = ?Query:exec1(
             ?Query:seq([?File:find(File),
                         ?File:module()]),
             ?RefError(file_not_module, [File])),
    [?Query:exec1(Mod, ?Mod:local(Fun, Arity),
                  ?RefError(fun_not_found, [Fun, Arity])) ||
        {Fun, Arity} <- Funs].

funbypos_(File,Pos) ->
    [funbypos(File,Pos)].

%% @spec macro(arglist()) -> node()
%% @doc Returns the source macro node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`file' and `macro' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
macro(Args) ->
    ?CallArg(Args, "Macro",
             [{fun macbyname/2, [file, macro]},
              {fun macbypos/2, [file, position]}]).

macbyname(File, Macro) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?Macro:find(Macro)]),
       ?RefError(mac_not_found, [Macro])).

macbypos(File, Pos) ->
    T= ?Query:exec1(
         ?Query:seq([?File:find(File),
                     ?File:token(Pos)]),
         ?RefError(pos_bad_type, [mac, Pos])),
    case ?Query:exec([T], [{llex,back},mref]) of
        [Macro] -> Macro;
        []      -> ?Query:exec1([T], ?Query:seq([
                     [{{flex,back}, {type, '==', macro}}] ]),
                   ?RefError(pos_bad_type, [mac, Pos]))
    end.

%% @spec macuse(arglist()) -> node()
%% @doc Returns the source macro use node. This value is specified in the
%% argument list with `file' and `position' keys.
macuse(Args) ->
    ?CallArg(Args, "Macro",
             [{fun macusebypos/2, [file, position]}]).

macusebypos(File, Pos) ->
    P= ?Query:exec1(
         ?Query:seq([?File:find(File),
                     ?File:token(Pos),
                     [{llex,back}]]),
         ?RefError(pos_bad_type, [mac, Pos])),
    case ?Query:exec([P], [mref]) of
        [_] -> P;
        []  -> throw(?RefError(pos_bad_type, [mac, Pos]))
    end.

%% @spec macros(arglist()) -> [node()]
%% @doc Returns the source macro node list. This value is specified with
%% `file' and `maclist' keys in the argument list.
macros(Args) ->
    ?CallArg(Args, "Macro list",
             [{fun macsbyname/2, [file, maclist]}]).

macsbyname(File, Macs) ->
    F = ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])),
    [?Query:exec1(F, ?File:macro(Mac),
                  ?RefError(mac_not_found, [Mac])) || Mac <- Macs].

%% @spec record(arglist()) -> node()
%% @doc Returns the source record node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`file' and `record' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
record(Args) ->
    ?CallArg(Args, "Record",
             [{fun recbyname/2, [file, record]},
              {fun recbypos/2,  [file, position]}]).

recbyname(File, Record) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?Rec:find(Record)]),
       ?RefError(rec_not_found, [Record])).

recbypos(File, Pos) ->
    [Token] = ?Query:exec(
                  ?Query:seq(?File:find(File),
                             ?File:token(Pos))),
    Rec = [Form || Form <- ?Query:exec(Token, ?Token:form()),
                   ?Form:type(Form) == record], %rec-ben hasznalt rec?
    case ?Query:exec(Rec, ?Form:record()) of
        [Record] -> Record;
        []       -> ?Query:exec1(Token,
                        ?Query:seq([?Token:expr(),
                                    ?Expr:record()]),
                 ?RefError(pos_bad_type, [rec, Pos]))
    end.

%% @spec record_field(arglist()) -> node()
%% @doc Returns the source record field node. This value is
%% specified in the argument list with
%% <ul>
%%  <li>`file', `record' and `recfield' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>

record_field(Args) ->
    ?CallArg(Args, "Record field",
             [{fun recfieldbyname/3, [file, record, recfield]},
              {fun recfieldbypos/2,  [file, position]}]).

recfieldbyname(File, Record, Field) ->
    Rec = ?Query:exec1(
             ?Query:seq([?File:find(File),
                         ?Rec:find(Record)]),
             ?RefError(rec_not_found, [Record])),
    ?Query:exec1(Rec, ?Rec:field(Field),
             ?RefError(recfld_not_found, [Record,Field])).

recfieldbypos(File, Pos) ->
    [Token] = ?Query:exec(?Query:seq([?File:find(File), ?File:token(Pos)])),
    case ?Query:exec(Token, ?Query:seq([?Token:typexp(), [fielddef]])) of
        [Field]  -> Field;
        []       -> ?Query:exec1(Token,
                                 ?Query:seq([?Token:expr(), [fieldref]]),
                                 ?RefError(pos_bad_type, [recfield, Pos]))
    end.


%% @spec records(arglist()) -> [node()]
%% @doc Returns the source record node list. This value is specified with
%% `file' and `reclist' keys in the argument list.
records(Args) ->
    ?CallArg(Args, "Record list",
             [{fun recsbyname/2, [file, reclist]}]).

recsbyname(File, Recs) ->
    F = ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])),
    [?Query:exec1(F, ?File:record(Rec),
                  ?RefError(rec_not_found, [Rec])) || Rec <- Recs].

%% @spec variable(arglist()) -> node()
%% @doc Returns the source variable node. This value is specified with
%% `file' and `position' keys in the argument list.
variable(Args) ->
    ?CallArg(Args, "Variable",
             [{fun varbypos/2, [file, position]}]).

varbypos(File, Pos) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?File:token(Pos),
                   ?Token:expr(),
                   ?Expr:variables()]),
       ?RefError(pos_bad_type, ['variable',Pos])).

%% @spec varname(arglist()) -> string()
%% @doc Returns the target variable name. This value is specified with
%% a `varname' key in the argument list.
varname(Args) ->
    ?CallArg(Args, "Target variable name",
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
    ?CallArg(Args, "Source expression",
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
    ?CallArg(Args, "Source file",
             [{fun filebyname/1, [file]}]).

filebyname(File) ->
    ?Query:exec1(
       ?File:find(File),
       ?RefError(file_not_present, [File])).

%% @spec macname(arglist()) -> string()
%% @doc Returns the target macro name. This value is specified with
%% a `macname' key in the argument list.
macname(Args) ->
    ?CallArg(Args, "Target macro name",
             [{fun macrostr/1, [macname]}]).

macrostr(Name) when is_atom(Name) ->
    io_lib:write(Name);
macrostr(Name) when is_list(Name) ->
    case ?Var:valid_name(Name) of
        true  -> Name;
        false -> throw(?RefErr0r(bad_mac_name))
    end.

%% @spec module(arglist()) -> node()
%% @doc Returns the source module node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>a `module' key, or</li>
%%  <li>a `file' key.</li>
%% </ul>
module(Args) ->
    ?CallArg(Args, "Source module",
             [{fun modbyname/1, [module]},
              {fun modbypos/2,  [file, position]},
              {fun modbyfile/1, [file]}]).

modbyname(Mod) ->
    ?Query:exec1(
       ?Mod:find(Mod),
       ?RefError(mod_not_found, [Mod])).

modbypos(File, Pos) ->
    ?Query:exec1(
       ?Query:seq( [?File:find(File),
                    ?File:token(Pos),
                    ?Token:expr(),
                    [modref] ]),
       ?RefError(pos_bad_type, [module, Pos])).

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
    ?CallArg(Args, "Expression range",
             [{fun expr_range/2,   [file, posrange]},
              {fun expr_posnumr/3, [file, position, number]}]).

expr_posnumr(File, Pos1, Num) ->
    ?Check(Num>=1, ?RefErr0r(bad_range)),
    IllP  = fun(Loc,Path)-> ?Query:exec1(Loc,Path,
                            ?RefError(illegal_pos, [File, Pos1])) end,
    F      = get_filenode(File),
    Start  = IllP(F, ?File:token(Pos1)),
    StPat  = IllP(Start, [{elex,back}]),
    FunCl  = IllP(StPat, [{pattern,back}]),
    FunDef = IllP(FunCl, [{funcl,back}, fundef]),
    Arity  = ?Fun:arity(FunDef),
    ?Check(Num=<Arity, ?RefErr0r(bad_range)),
    IsIdx  = fun(I) -> StPat =:= IllP(FunCl, [{pattern,I}]) end,
    {Idx,_Pos} = ?MISC:list_find(IsIdx, lists:seq(1,Arity-Num+1)),
    EndIdx = Idx+Num-1,
    ?Check((Idx>0) andalso (EndIdx=<Arity), ?RefErr0r(bad_range)),
    EndE   = IllP(FunCl, [{pattern,EndIdx}]),
    End    = hd(lists:reverse(?Query:exec(EndE,[elex]))),
    expr_range_common(Start, End).

expr_range(File, {Pos1, Pos2}) ->
    F     = get_filenode(File),
    Start = ?Query:exec1(F, ?File:token(Pos1),
                         ?RefError(illegal_pos, [File, Pos1])),
    End   = ?Query:exec1(F, ?File:token(Pos2),
                         ?RefError(illegal_pos, [File, Pos2])),
    expr_range_common(Start, End).

get_filenode(File) ->
    ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])).

expr_range_common(Start, End) ->
    Exprs = range(joint(?Syn:root_path(Start, left),
                        ?Syn:root_path(End, right))),
    [First] = ?Query:exec(hd(Exprs), ?Syn:first_leaf()),
    [Last]  = ?Query:exec(lists:last(Exprs), ?Syn:last_leaf()),
    ?Check(First =:= Start andalso Last =:= End,
           ?RefErr0r(bad_range)),
    Exprs.

%% Turns the result of `joint/2' into a list of expressions.
range({Expr, esub, F, L}) ->
    Chld = ?Syn:children(Expr),
    case {hd(Chld), lists:last(Chld)} of
        {{esub,F}, {esub,L}} -> [Expr];
        _ ->
            FI = ?Syn:index(Expr, esub, F),
            LI = ?Syn:index(Expr, esub, L),
            ?ESG:path(Expr, [{esub, {FI, LI+1}}])
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
