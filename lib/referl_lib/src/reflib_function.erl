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
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(reflib_function).
-vsn("$Rev: 5013 $"). % for emacs"

%% =============================================================================
%% Exports

%% Properties
-export([name/1, arity/1,
         dirtiness/1, is_dirty/1,
         is_autoimported/2, is_typeguard/2, is_exported/1]).

%% Queries
-export([find/2, definition/0, module/0, applications/0, applications/1,
         implicits/0, implicits/1, return_points/1,
         exports/0, imports/0, imported/0, impexps/0]).

%% Transformations
-export([add_export/1, del_export/1]).

-export([called/0, funcalls/0]).

%% Composite
-export([mod_fun_arity/1]).

-include("lib.hrl").

%% =============================================================================
%% Function related properties

%% @spec name(node()) -> atom()
%% @doc Returns the name of the function.
name(Fun) -> (?Graph:data(Fun))#func.name.

%% @spec arity(node()) -> integer()
%% @doc Returns the arity of the function.
arity(Fun) -> (?Graph:data(Fun))#func.arity.

%% @spec dirtiness(node()) -> no | int | ext
%% @doc Returns the dirtiness of the function.
dirtiness(Fun) -> (?Graph:data(Fun))#func.dirty.

%% @spec is_dirty(node()) -> bool()
%% @doc Returns whether the function is dirty.
is_dirty(Fun) -> dirtiness(Fun) =/= no.

%% @spec is_exported(node(#func{})) -> bool()
%% @doc Returns `true' if the function is is_exported.
is_exported(Fun) ->
    case ?Query:exec(Fun, [{funexp, back}]) of
        [] -> false;
        _  -> true
    end.

%% @spec is_autoimported(atom(), integer()) -> bool()
%% @doc Returns `true' if the function `Name/Arity' is an is_autoimported BIF.
is_autoimported(Name, Arity) ->
    erl_internal:bif(Name, Arity).

%% @spec is_typeguard(atom(), integer()) -> bool()
%% @doc Returns `true' if the function `Name/Arity' is a type test (used
%% exclusively in guards).
is_typeguard(Name, Arity) ->
    erl_internal:type_test(Name, Arity).


%% =============================================================================
%% Queries starting from functions

%% @spec find(atom(), integer()) -> query(#module{}, #func{})
%% @doc The result query returns the function with name `Name' and
%% arity `Arity'.
find(Name, Arity) ->
    [{func, {{name, '==', Name}, 'and', {arity, '==', Arity}}}].

%% @spec module() -> query(#func{}, #module{})
%% @doc The result query returns the module which contains the function.
module() ->
    [{func, back}].

%% @spec definition() -> query(#func{}, #form{})
%% @doc The result query returns the definition of the function. There may be
%% only one definition, which may be missing (the source of the function may
%% not be loaded).
definition() ->
    [{fundef,back}].

%% @spec funcalls() -> query(#func{}, #func{})
%% @doc The result query returns every functions that calls the function.
funcalls() ->
    [funcall].

%% @spec called() -> query(#func{}, #func{})
%% @doc The result query returns every functions that called from the function.
called() ->
    [{funcall, back}].

%% @spec applications() -> query(#func{}, #expr{})
%% @doc The result query returns every application that calls the function.
applications() ->
    ?Query:all([[{{funlref,back}, {type, '==', application}}],
                [{{funeref,back}, {type, '==', application}}]]).

%% @spec applications(node()) -> query(#func{}, #expr{})
%% @doc The result query returns every application from the syntactic subtree
%% of the expression node `Expr' that calls the function.
applications(Expr) ->
    Subs = ?Query:exec(Expr, ?Expr:deep_sub()),
    Filter = fun(E) -> lists:member(E, Subs) end,
    filtered_query(applications(), Filter).

%% @spec implicits() -> query(#func{}, #expr{})
%% @doc The result query returns every implicit function expression that
%% refers the function.
implicits() ->
    ?Query:all([[{{funlref,back},{type,'==',implicit_fun}}],
                [{{funeref,back},{type,'==',implicit_fun}}]]).

%% @spec implicits(node()) -> query(#func{}, #expr{})
%% @doc The result query returns every implicit function expression from the
%% syntactic subtree of the expression node `Expr' that refers the function.
implicits(Expr) ->
    Subs = ?Query:exec(Expr, ?Expr:deep_sub()),
    Filter = fun(E) -> lists:member(E, Subs) end,
    filtered_query(implicits(), Filter).

%% @spec exports() -> query(#func{}, #expr{})
%% @doc The result query returns the export list element expression that
%% refers the function (there may be only one such expression).
exports() ->
    Path = [top, {{eattr,back}, {type,'==',export}}],
    Filter = fun(E) -> [] =/= ?Query:exec(E, Path) end,
    filtered_query(impexps(), Filter).

%% @spec imported() -> query(#func{}, #module{})
%% @doc The result query returns the modules that import the function.
imported() ->
    [{funimp,  back}].

%% @spec imports() -> query(#func{}, #expr{})
%% @doc The result query returns every import list element expression that
%% refers the function.
imports() ->
    Path = [top, {{eattr,back}, {type,'==',import}}],
    Filter = fun(E) -> [] =/= ?Query:exec(E, Path) end,
    filtered_query(impexps(), Filter).

%% @doc The result query returns the import and export list element expressions
%% that refer the function.
impexps() ->
    ?Query:all([[{{funlref,back},{type,'==',funref}}],
                [{{funeref,back},{type,'==',funref}}]]).

%% @spec filtered_query(query(), (node()) -> bool()) -> query(#func{}, #expr{})
%% @doc Returns a query that can filter the results of `Query' by a
%% `Filter' fun.
filtered_query(Query, Filter) ->
    fun(FunObj) ->
            [E || E <- ?Query:exec(FunObj, Query), Filter(E)]
    end.


%% @spec return_points(node()) -> query(#func{}, #expr{})
%% @doc The result query returns every return points of the function `Fun'.
return_points(Fun) ->
   fun(_)->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
				      ?Fun:definition(), ?Form:clauses())),
    LastTopExprs = [lists:last(
            ?Query:exec(Cl, ?Clause:exprs())) || Cl <- FunClauses],
    lists:flatten([rtn_points(LTP, ?Expr:type(LTP)) || LTP <- LastTopExprs])
   end.

%%% @private
rtn_points(Expr, Kind) when  Kind == case_expr;
                                Kind == try_expr;
                                Kind == if_expr->
    Clauses = ?Query:exec(Expr, ?Expr:clauses()),
    HeadClauses = ?Query:exec(Expr, [headcl]),
    Exprs = [lists:last(?Query:exec(Cl, ?Clause:exprs()))
                                            || Cl <- Clauses -- HeadClauses],
    [rtn_points(ExprL,?Expr:type(ExprL)) || ExprL <- Exprs];
rtn_points(Expr, _Other)->
    Expr.



%% =============================================================================
%% Function related transformations

%% @spec add_export(node()) -> ok
%% @doc Adds `Fun' to an arbitrary export list.
add_export(Fun) ->
    case is_exported(Fun) of
        true  -> ok;
        false ->
            [Mod] = ?Query:exec(Fun, module()),
            File = ?Query:exec1(Mod, ?Mod:file(),
                                ?RefError(no_file,[module,Mod])),
            NameExpr = ?Syn:create(#expr{type=atom},
                                   [?MISC:to_list(name(Fun))]),
            ArityExpr = ?Syn:create(#expr{type=integer},
                                    [?MISC:to_list(arity(Fun))]),
            ListItem = ?Syn:create(#expr{type=funref},
                                   [{esub, [NameExpr, ArityExpr]}]),
            List = ?Syn:create(#expr{type=funlist},
                               [{esub, ListItem}]),
            Form = ?Syn:create(#form{type=export},
                               ["-", "export", {eattr, List}]),
            ?File:add_form(File, Form),
            ok
    end.

%% @spec del_export(node()) -> ok
%% @doc Removes `Fun' from the export list which contains it.
del_export(Fun) ->
    %% more export items are accepted (`erlc' gives only warning)
    [begin
         Form = ?Query:exec1(Expr, ?Expr:attrib_form(),
                             form_not_found),
         List = ?Query:exec1(Expr, ?Expr:parent(),
                             list_not_found),
         case length(?Query:exec(List, ?Expr:children())) of
             1 -> ?File:del_form(Form);
             _ -> ?Syn:replace(List, {node, Expr}, [])
         end
     end || Expr <- ?Query:exec(Fun, exports())],
    ok.

%% =============================================================================
%% Composite

%% @doc Returns the module semantic nodes,
%% the module name, the function name and the arity of the function,
%% the latter three as a tuple.
mod_fun_arity(Fun) ->
    Mod = ?Query:exec1(Fun, module(), ?RefErr0r(ambiguous)),
    {Mod, {?Mod:name(Mod), name(Fun), arity(Fun)}}.
