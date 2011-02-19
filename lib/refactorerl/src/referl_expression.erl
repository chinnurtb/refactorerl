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
%%% The Initial Developer of the Original Code is Eötvös Loránd University.
%%% Portions created by Eötvös Loránd University are Copyright 2008, Eötvös
%%% Loránd University. All Rights Reserved.

%%% @doc This module implements queries about expressions.
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(referl_expression).
-vsn("$Rev: 2931 $").

-export([type/1, kind/1, value/1, is_expr/1]).
-export([clause/0, attrib_form/0, children/0, child/1, parent/0]).
-export([variables/0, varbinds/0, scope_varbinds/0, binding_vars/0, varrefs/0,
         visible_vars/0,
         record/0, records/0, macros/0, function/0, functions/0, funapps/0,
         clauses/0, clause/1, nameof/0]).
-export([modq/0, add_modq/2, del_modq/1, upd_modq/2, expand_funexpr/1]).

-export([top_sub/0, sub/0, top_deep_sub/0, deep_sub/0, sup/0, is_top/1]).
-export([is_same_expr/1]).
-export([side_effect/1]).


-include("refactorerl.hrl").

%% =============================================================================
%% Expression properties
%% @spec type(node())-> atom()
%% @doc Returns the type of an expression.
type(Expr) -> (?ESG:data(Expr))#expr.type.

%% @spec kind(node())-> atom()
%% @doc Returns the kind of an expression.
kind(Expr) -> (?ESG:data(Expr))#expr.kind.

%% @spec value(node())-> term()
%% @doc Returns the value of an expression.
value(Expr) -> (?ESG:data(Expr))#expr.value.

%% @spec is_expr(node())-> bool()
%% @doc Returns whether the node is an expression.
is_expr(Node) -> element(1, ?ESG:data(Node)) == expr.

%% =============================================================================
%% Expression related queries

%% @ spec all(node()) -> [node()]
%% @ doc Returns all syntactic node from a syntax tree
%% all([]) -> [];
%% all([Expr|Tail]) ->
%%     Subs = ?Query:exec([Expr], ?Query:any([[clause, visib, {sup, back}],
%%                                            [{sup, back}],
%%                                            [sub]])),
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
clause() -> [sup, {visib, back}].

%% @spec attrib_form() -> query(#expr{}, #form{})
%% @doc The result query returns the attribute form that contains the
%% expression.
attrib_form() -> [sup, {attr, back}].

%% @spec nameof() -> query(#expr{}, #clause{})
%% @doc The result query returns the clause that has the expression as its
%% name part.
nameof() -> [{name, back}].

%% @spec parent() -> query(#expr{}, #expr{})
%% @doc The result query returns the closest superexpression of the input
%% expression.
parent() ->
    ?Query:any([{sub, back}],
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
    [funref].

%% @spec functions() -> query(#expr{}, #func{})
%% @doc The result query returns every function that is referred in the
%% expression (either directly or by subexpressions).
functions()->
    ?Query:seq(deep_sub(), [funref]).

%% @spec funapps() -> query(#expr{}, #func{})
%% @doc The result query returns every function that is referred in the
%% expression with a funtion application (either directly or by subexpressions).
funapps()->
    fun(Expr) ->
        Apps = [App ||App <- ?Query:exec(Expr, deep_sub()),
                      kind(App) == 'application'],
        ?Query:exec(Apps, [funref])
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
    [sub].

%%@spec child(integer()) -> query(#expr{}, #expr{})
%% @doc The result query returns the I-th child of an expression.
child(I) ->
    [{sub, I}].

%% @spec top_sub() -> query(#expr{}, #expr{})
%% @doc The result query returns only the direct subexpressions of a top-level
%% expression.
top_sub()->
    [{sup, back}].

%% @spec sub() -> query(#expr{}, #expr{})
%% @doc The result query returns only the direct subexpressions of an
%% expression.
sub() ->
  ?Query:any(top_sub(),
             fun sub_expr/1).

sub_expr(Sub) ->
    [Sub | ?Query:exec(Sub, ?Query:seq(children(), fun sub_expr/1))].

%% @spec sup() -> query(#expr{}, #expr{})
%% @doc The result query returns the outermost direct superexpression.
sup()->
    [sup].

%% @spec is_top(node()) -> bool()
%% @doc Returns true if the expression is a top-level expression.
is_top(Node)->
    case ?Query:exec(Node, sup()) of
        [Node] -> true;
        _      -> false
    end.

%% @spec clauses() -> query(#expr{}, #clause{})
%% @doc The result query returns every clause of an expression.
clauses()->
    [clause].

%% @spec clause(integer()) -> query(#expr{}, #clause{})
%% @doc The result query returns the I-th clause of an expression.
clause(I) ->
    [{clause, I}].

%% @spec modq() -> query(#expr{}, #expr{})
%% @doc The result query returns the module qualifier part of
%% funexpr/fun application.
modq()->
    fun(Expr)->
      case ?Graph:data(hd(?Graph:path(Expr, [{sub,1}]))) of
          #expr{kind = infix_expr, value = ':'} -> ?Graph:path(Expr, [{sub,1}]);
          _ -> []
      end
    end.

%% =============================================================================
%% Expression related transformations

%% @spec add_modq(node(), atom()) -> node()
%% @doc Add module qualifier to fun expr/application.
add_modq(Expr, Name) ->
    ModName   = ?Syn:create(#expr{kind = atom}, [atom_to_list(Name)]),
    [FunName] = ?Graph:path(Expr, [{sub, 1}]),
    InfixExpr = ?Syn:create(#expr{kind=infix_expr, value=':'},
                               [{sub, [ModName, FunName]}]),
    ?Syn:replace(Expr, {node, FunName}, [InfixExpr]).

%% @spec del_modq(node()) -> node()
%% @doc Remove module qualifier from fun expr/application.
del_modq(Expr) ->
    [Modq]    = ?Graph:path(Expr, [{sub, 1}]),
    [FunName] = ?Graph:path(Modq, [{sub, 2}]),
    ?Syn:replace(Expr, {node, Modq}, [FunName]).

%% @spec upd_modq(node(), atom()) -> node()
%% @doc Change module qualifier name.
upd_modq(Expr, NewName) ->
    NewModName   = ?Syn:create(#expr{kind = atom}, [atom_to_list(NewName)]),
    [Modq] = ?Graph:path(Expr, [{sub, 1}]),
    [OldModName] = ?Graph:path(Modq, [{sub, 1}]),
    ?Syn:replace(Modq, {node, OldModName}, [NewModName]).

%% @spec expand_funexpr(node() | [node()]) -> ok
%% @doc Makes explicit fun expression(s) in place of implicit one(s).
expand_funexpr(Implicits) when is_list(Implicits) ->
    [expand_funexpr(I) || I <- Implicits];

expand_funexpr(Implicit) ->
    [{_, Parent}]       = ?Syn:parent(Implicit),
    [FunRef, ArityExpr] = ?Query:exec(Implicit, children()),
    ?ESG:remove(Implicit, sub, FunRef),
    #expr{value=Arity}  = ?Graph:data(ArityExpr),
    {Pattern, VarNodes} = pattern_and_args(Arity),
    App                 = ?Syn:create(#expr{kind=application},
                                      [{sub, [FunRef] ++ VarNodes}]),
    Clause              = ?Syn:create(#clause{kind=funexpr},
                                      [{pattern, Pattern}] ++ [{body, App}]),

    Explicit = ?Syn:create(#expr{kind=fun_expr}, [{exprcl, Clause}]),
    ?Syn:replace(Parent, {node, Implicit}, [Explicit]).

pattern_and_args(Arity) ->
    VarNames = get_var_names("V", Arity, []),
    Pattern  = [?Syn:create(#expr{type=pattern, kind=variable}, [Name])
                || Name <- VarNames],
    VarNodes = [?Syn:create(#expr{kind=variable}, [Name])
                || Name <- VarNames],
    {Pattern, VarNodes}.

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
is_same_expr({E1, E2}) ->
    case {is_leaf(E1), is_leaf(E2)} of
        { true, false} -> false;
        {false,  true} -> false;
        { true,  true} ->
            lex_content(E1) == lex_content(E2);
        {false, false} ->
            {Type1, Subs1, Cls1} = expr_data(E1),
            {Type2, Subs2, Cls2} = expr_data(E2),
            Type1 == Type2 andalso
                lex_content(E1) == lex_content(E2) andalso
                length(Subs1) == length(Subs2) andalso
                length(Cls1) == length(Cls2) andalso
                lists:all(fun is_same_expr/1, lists:zip(Subs1, Subs2)) andalso
                lists:all(fun ?Clause:is_same_clause/1, lists:zip(Cls1, Cls2))
    end.

%% Returns whether the expression has subexpressions.
is_leaf(Expr) ->
    [] == ?Query:exec([Expr], children()) andalso
    [] == ?Query:exec([Expr], clauses()).

%% Returns the lexical contents related to a node.
%% For variables, returns the bound variable node.
lex_content(Expr) ->
    case kind(Expr) of
        variable -> ?Query:exec([Expr], varrefs());
        _ ->
            Lexes = ?Query:exec([Expr], [elex]),
            [(?Token:data(Lex))#token.value || Lex <- Lexes]
    end.

%% Collects data about the expression.
expr_data(Expr) ->
    Type = type(Expr),
    Subs = ?Query:exec([Expr], sub()) -- [Expr],
    Cls  = ?Query:exec([Expr], clauses()),
    {Type, Subs, Cls}.

%% @spec side_effect(node()) -> bool()
%% @doc Returns true if one of the expressions has a side effect,
%% false if does not have side effect, and unknown otherwise.

side_effect(Expr)->
    Children  = ?Query:exec(Expr, ?Expr:deep_sub()),
    SideEffs  = [Node || Node <- Children,
                         (?Expr:kind(Node) == send_expr) orelse
                         (?Expr:kind(Node) == receive_expr)],
    Funs      = ?Query:exec(Expr, ?Expr:functions()),
    DirtyFunc = [Fun || Fun <- Funs, ?Fun:dirty(Fun)],
    UnKnown   = [Fun || Fun <- Funs, ?Fun:dirty(Fun) == unknown],
    case {SideEffs =/= [], UnKnown =/= [], DirtyFunc =/= []} of
        {true, _, _} -> true;
        {_, true, _} -> true;
    %% when we can not determine whether the expression has a side-effect,
    %% we handle this as it has
    %% TODO: ask the user wheather it has a side-effect
        {_, _, true} -> true;
        {_, _,    _} -> false
    end.
