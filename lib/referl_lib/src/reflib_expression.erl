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

%%% @doc This module implements queries about expressions.
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(reflib_expression).
-vsn("$Rev: 5103 $ ").

-export([role/1, type/1, value/1, is_expr/1]).
-export([clause/0, attrib_form/0, children/0, child/1, parent/0]).
-export([variables/0, varbinds/0, scope_varbinds/0, binding_vars/0, varrefs/0,
         visible_vars/0, record/0, records/0, field/0, fields/0,
         fielddef/0, module/0, macros/0, function/0, functions/0, funapps/0,
         clauses/0, clause/1, nameof/0, dynfunction/0]).
-export([modq/0, add_modq/2, del_modq/1, upd_modq/2, expand_funexpr/1]).

-export([top_sub/0, sub/0, top_deep_sub/0, deep_sub/0, top/0, is_top/1]).
-export([is_same_expr/1, is_leaf/1]).
-export([has_side_effect/1, has_message_passing/1, has_dirty_fun/1]).
-export([fun_app_args/1, app_has_modq/1]).
-export([virtuals/1]).


-include("lib.hrl").

%% =============================================================================
%% Expression properties
%% @spec role(node())-> atom()
%% @doc Returns the role of an expression.
role(Expr) -> (?ESG:data(Expr))#expr.role.

%% @spec type(node())-> atom()
%% @doc Returns the type of an expression.
type(Expr) -> (?ESG:data(Expr))#expr.type.

%% @spec value(node())-> term()
%% @doc Returns the value of an expression.
value(Expr) -> (?ESG:data(Expr))#expr.value.

%% @spec is_expr(node())-> bool()
%% @doc Returns whether the node is an expression.
is_expr(Node) -> ?Syn:class(Node) == expr.

%% =============================================================================
%% Expression related queries

%% @ spec all(node()) -> [node()]
%% @ doc Returns all syntactic node from a syntax tree
%% all([]) -> [];
%% all([Expr|Tail]) ->
%%     Subs = ?Query:exec([Expr], ?Query:any([[clause, visib, {top, back}],
%%                                            [{top, back}],
%%                                            [esub]])),
%%     [Expr] ++ all((Subs -- [Expr]) ++ Tail);
%% all(Expr) -> all([Expr]).

%% @spec deep_sub()-> query(#expr{}, #expr{})
%% @doc The result query returns every expression from the subtree in an
%% undetermined order.
deep_sub() ->
  ?Query:any(?Query:seq(fun top_deep_sub/1, top_sub()),
             fun deep_sub/1).

deep_sub(Sub) ->
    [Sub | ?Query:exec(Sub,
                       ?Query:any(
                                  ?Query:seq(children(), fun deep_sub/1),
                                  ?Query:seq([clauses(),
                                              ?Clause:exprs(),
                                              fun top_deep_sub/1,
                                              top_sub()])))].

%% @spec top_deep_sub()-> query(#expr{}, #expr{})
%% @doc The result query returns every expression from the subtree in an
%% undetermined order if the root of the subtree is a top-level expression.
top_deep_sub() ->
  ?Query:seq(
    fun top_deep_sub/1,
    top_sub()).

top_deep_sub(Top) ->
    [Top | ?Query:exec(Top,
                       ?Query:seq([top_sub(),
                                   clauses(),
                                   ?Clause:exprs(),
                                   fun top_deep_sub/1]))].


%% @spec clause() -> query(#expr{}, #clause{})
%% @doc The result query returns the clause that contains the expression.
clause() -> [top, {visib, back}].

%% @spec attrib_form() -> query(#expr{}, #form{})
%% @doc The result query returns the attribute form that contains the
%% expression.
attrib_form() -> [top, {eattr, back}].

%% @spec nameof() -> query(#expr{}, #clause{})
%% @doc The result query returns the clause that has the expression as its
%% name part.
nameof() -> [{name, back}].

%% @spec parent() -> query(#expr{}, #expr{})
%% @doc The result query returns the closest superexpression of the input
%% expression.
parent() ->
    ?Query:any([{esub, back}],
               [{visib, back}, {clause, back}]).

%% @spec variables() -> query(#expr{}, #variable{})
%% @doc The result query returns every variable inside the expression.
variables()->
    ?Query:seq(deep_sub(), ?Query:any([varbind], [varref])).

%% @spec varbinds() -> query(#expr{}, #variable{})
%% @doc The result query returns every variable that is bound in the expression
%% (either directly or by subexpressions).
varbinds()->
    ?Query:seq(deep_sub(), [varbind]).

%% @spec binding_vars() -> query(#variable{}, #expr{})
%% @doc The result query returns the binding expressions of a variable.
binding_vars()->
    [{varbind, back}].

%% @spec scope_varbinds() -> query(#expr{}, #variable{})
%% @doc The result query returns every variable
%%      that is bound in the scope of the expression's enclosing clause.
scope_varbinds() ->
    ?Query:seq([?Expr:clause(), ?Clause:scope(), ?Clause:variables()]).

%% @spec varrefs() -> query(#expr{}, #variable{})
%% @doc The result query returns every variable that is refferred in the
%% expression (either directly or by subexpressions).
varrefs()->
    ?Query:seq(deep_sub(), [varref]).

%% @spec record() -> query(#expr{}, #record{})
%% @doc The result query returns the record that is referred directly
%% by the expression.
record()->
    [recref].

%% @spec records() -> query(#expr{}, #record{})
%% @doc The result query returns every record that is referred in the
%% expression (either directly or by subexpressions).
records()->
    ?Query:seq(deep_sub(), [recref]).

%% @spec field() -> query(#expr{}, #node{})
%% @doc The result query returns the record field that is reffered directly
%% by the expression.
field() ->
    [fieldref].

%% @spec fields() -> query(#expr{}, #node{})
%% @doc The result query returns every record field that is reffered
%% by the expression (either directly or by subexpressions).
fields() ->
    ?Query:seq(deep_sub(), [fieldref]).

%% @spec fielddef() -> query(#expr{}, #node{})
%% @doc The result query returns the record field defined directly by the
%% expression.
fielddef() ->
    [fielddef].

%% @spec module() -> query(#expr{}, #module{})
%% @doc The result query returns the module node referred directly by the
%% expression.
module() ->
    [modref].

%% @spec macros() -> query(#expr{}, #form{})
%% @doc The result query returns the macro definitions of the macros used in
%% the expression. In the result list any macro definition may occure in
%%  multiple number.
macros() ->
    ?Query:seq(deep_sub(), [{elex,1},{llex,1},{mref,1}]).

%% @spec function() -> query(#expr{}, #func{})
%% @doc The result query returns the function which is referred directly by
%% the expression.
function() ->
    ?Query:all([funlref], [funeref]).

%% @spec dynfunction() -> query(#expr{}, #func{})
%% @doc The result query returns the function which is referred directly
%% or dynamically by the expression.
dynfunction() ->
    ?Query:all([[funlref], [funeref], [dynfunlref], [dynfuneref]]).

%% @spec functions() -> query(#expr{}, #func{})
%% @doc The result query returns every function that is referred in the
%% expression (either directly or by subexpressions).
functions()->
    ?Query:seq(deep_sub(), function()).

%% @spec funapps() -> query(#expr{}, #func{})
%% @doc The result query returns every function that is referred in the
%% expression with a funtion application (either directly or by subexpressions).
funapps()->
    fun(Expr) ->
        Apps = [App ||App <- ?Query:exec(Expr, deep_sub()),
                      type(App) == 'application'],
        ?Query:exec(Apps, function())
    end.

%% Maybe a query for the implicit fun expressions...

%% @spec visible_vars() -> query(#expr{}, #variable{})
%% @doc The result query returns every variable that is visible in the
%% expression.
visible_vars() ->
    fun (Expr) ->
            [Vis] = ?Query:exec(Expr, clause()),
            lists:filter(
              fun(Var) ->
                      case ?Graph:path(Var, [varintro,
                                             {intersect, Vis, visib}]) of
                          [] -> true;
                          [Intro] ->
                              EI = ?Graph:index(Vis, visib, Expr),
                              II = ?Graph:index(Vis, visib, Intro),
                              EI > II
                      end
              end,
              ?Query:exec(Vis, [varvis]))
    end.

%% @spec children() -> query(#expr{}, #expr{})
%% @doc The result query returns every child of an expression.
children() ->
    [esub].

%%@spec child(integer()) -> query(#expr{}, #expr{})
%% @doc The result query returns the I-th child of an expression.
child(I) ->
    [{esub, I}].

%% @spec top_sub() -> query(#expr{}, #expr{})
%% @doc The result query returns only the direct subexpressions of a top-level
%% expression.
top_sub()->
    [{top, back}].

%% @spec sub() -> query(#expr{}, #expr{})
%% @doc The result query returns only the direct subexpressions of an
%% expression.
sub() ->
  ?Query:any(top_sub(),
             fun sub_expr/1).

sub_expr(Sub) ->
    [Sub | ?Query:exec(Sub, ?Query:seq(children(), fun sub_expr/1))].
%% @spec top() -> query(#expr{}, #expr{})
%% @doc The result query returns the outermost direct superexpression.
top()->
    [top].

%% @spec is_top(node()) -> bool()
%% @doc Returns true if the expression is a top-level expression.
is_top(Node)->
    case ?Query:exec(Node, top()) of
        [Node] -> true;
        _      -> false
    end.

%% @spec clauses() -> query(#expr{}, #clause{})
%% @doc The result query returns every clause of an expression.
clauses() ->
    ?Query:all([headcl], ?Query:all([exprcl], ?Query:all([catchcl], [aftercl]))).
    % [clause].

%% @spec clause(integer()) -> query(#expr{}, #clause{})
%% @doc The result query returns the I-th clause of an expression.
clause(I) ->
    fun (Expr) ->
        AllClauses = ?Query:exec(Expr, clauses()),
        lists:nth(I, AllClauses)
    end.
    % ~ [{clause, I}].

%% @spec modq() -> query(#expr{}, #expr{})
%% @doc The result query returns the module qualifier part of
%% funexpr/fun application.
modq()->
    fun(Expr)->
      case ?Graph:data(hd(?Graph:path(Expr, child(1)))) of
          #expr{type = infix_expr, value = ':'} -> ?Graph:path(Expr, child(1));
          _ -> []
      end
    end.

%% =============================================================================
%% Expression related transformations

%% @spec add_modq(node(), atom()) -> node()
%% @doc Add module qualifier to fun expr/application.
add_modq(Expr, Name) ->
    ModName   = ?Syn:create(#expr{type = atom}, [atom_to_list(Name)]),
    [FunName] = ?Graph:path(Expr, child(1)),
    Copy     = ?Syn:copy(FunName),
    {FunName, CFunName} = lists:keyfind(FunName, 1, Copy),

    InfixExpr = ?Syn:create(#expr{type=infix_expr, value=':'},
                               [{esub, [ModName, CFunName]}]),
    ?Syn:replace(Expr, {node, FunName}, [InfixExpr]).

%% @spec del_modq(node()) -> node()
%% @doc Remove module qualifier from fun expr/application.
del_modq(Expr) ->
    [Modq, _ArgList] = ?Query:exec(Expr, children()),
    [_ModName, FunName] = ?Query:exec(Modq, children()),
    Copy     = ?Syn:copy(FunName),
    {FunName, CFunName} = lists:keyfind(FunName, 1, Copy),
    ?Syn:replace(Expr, {node, Modq}, [CFunName]).

%% @spec upd_modq(node(), atom()) -> node()
%% @doc Change module qualifier name.
upd_modq(Expr, NewName) ->
    NewModName   = ?Syn:create(#expr{type = atom}, [atom_to_list(NewName)]),
    [Modq]       = ?Graph:path(Expr, child(1)),
    [OldModName] = ?Graph:path(Modq, child(1)),
    ?Syn:replace(Modq, {node, OldModName}, [NewModName]).

%% @spec expand_funexpr(node() | [node()]) -> ok
%% @doc Makes explicit fun expression(s) in place of implicit one(s).
expand_funexpr(Implicits) when is_list(Implicits) ->
    [expand_funexpr(I) || I <- Implicits];

expand_funexpr(Implicit) ->
    [{_, Parent}]       = ?Syn:parent(Implicit),
    [FunRef, ArityExpr] = ?Query:exec(Implicit, children()),
    #expr{value=Arity}  = ?Graph:data(ArityExpr),

    VarNames = get_var_names("V", Arity, []),

    Patterns = [{var_pattern, Name} || Name <- VarNames],
    Vars     = [{var, Name}         || Name <- VarNames],

    Copy     = ?Syn:copy(FunRef),
    {FunRef, CFunRef} = lists:keyfind(FunRef, 1, Copy),

    Explicit = ?Syn:construct({'fun', [{fun_scope, [Patterns], [],
                                                   [{app, CFunRef, Vars}]}]}),
    ?Syn:replace(Parent, {node, Implicit}, [Explicit]).

get_var_names(_, 0, List) -> List;
get_var_names(Prefix, Count, List) ->
    get_var_names(Prefix, Count-1, [Prefix++integer_to_list(Count) | List]).

%% =============================================================================
%% Expression related functions

%% @spec is_same_expr({#expr{}, #expr{}}) -> bool()
%% @doc  Returns whether the two expressions are the same, disregarding whitespace.
%% @todo Support macros.
%% @todo Does not detect peculiar cases, e.g. two similar functions,
%%       because the function arguments have separate variable nodes.
is_same_expr({E, E}) ->
    true;
is_same_expr({E1, E2}) ->
    case {is_leaf(E1), is_leaf(E2), role(E1), role(E2)} of
        {    _,     _, R1, R2} when R1 =/= R2 -> false;
        { true, false,  _,  _} -> false;
        {false,  true,  _,  _} -> false;
        { true,  true,  _,  _} ->
            lex_content(E1) == lex_content(E2);
        {false, false,  _,  _} ->
            {Subs1, Cls1} = expr_data(E1),
            {Subs2, Cls2} = expr_data(E2),
            lex_content(E1) == lex_content(E2) andalso
                length(Subs1) == length(Subs2) andalso
                length(Cls1) == length(Cls2) andalso
                lists_all(fun is_same_expr/1, lists:zip(Subs1, Subs2)) andalso
                lists_all(fun ?Clause:is_same_clause/1, lists:zip(Cls1, Cls2))
    end.

lists_all(_Fun, []) -> true;
lists_all(Fun, [X|Xs]) ->
    case Fun(X) of
        true  -> lists_all(Fun, Xs);
        false -> false
    end.

%% Returns whether the expression has subexpressions.
is_leaf(Expr) ->
    [] == ?Query:exec([Expr], children()) andalso
    [] == ?Query:exec([Expr], clauses()).

%% Returns the lexical contents related to a node.
%% For variables, returns the bound variable node.
lex_content(Expr) ->
    case type(Expr) of
        variable -> ?Query:exec([Expr], varrefs());
        _ ->
            Lexes = ?Query:exec([Expr], [elex]),
            [(?Token:data(Lex))#token.value || Lex <- Lexes]
    end.

%% Collects data about the expression.
expr_data(Expr) ->
    Subs = ?Query:exec([Expr], sub()) -- [Expr],
    Cls  = ?Query:exec([Expr], clauses()),
    {Subs, Cls}.

%% @spec has_side_effect(#expr{}) -> bool()
%% @doc Returns true if one of the expressions has a side effect,
%% false if does not have side effect, and unknown otherwise.
has_side_effect(Expr)->
    has_message_passing(Expr) orelse has_dirty_fun(Expr).

%% @spec (#expr{}) -> bool()
has_message_passing(Expr) ->
    DeepExprs = ?Query:exec(Expr, ?Expr:deep_sub()),
    [] /= [message_passing || E <- DeepExprs,
                              Kind <- [?Expr:type(E)],
                              Kind == send_expr orelse Kind == receive_expr].

%% @spec (#expr{}) -> bool()
%% @doc Returns whether the given expression contains a side effect.
has_dirty_fun(Expr) ->
    Funs      = ?Query:exec(Expr, ?Expr:functions()),
    [] =/= [Fun || Fun <- Funs, ?Fun:is_dirty(Fun)].

%% @doc Returns the arguments of the given function application.
fun_app_args(App) ->
    [_Name|Params] = ?Query:exec(App, ?Expr:children()),
    Params.

%% @doc Returns whether the given application has a module qualifier.
app_has_modq(App) ->
    [] =/= ?Query:exec(App, modq()).

%% @spec (#expr{}) -> [#lex{}]
%% @doc Returns every virtual token under the expression.
virtuals(Exp) ->
    [Token || Token <- ?Query:exec(Exp, ?Query:seq(deep_sub(), [elex])), (?Graph:data(Token))#lex.data =:= virtual].
