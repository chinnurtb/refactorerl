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

%%% @doc High level function-related operations. This module contains
%%% functions that expect a function semantical node as their parameter (or
%%% return a query that expects a function semantical node as starting point).
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_function).
-vsn("$Rev: 3331 $").
-include("refactorerl.hrl").

%% =============================================================================
%% Exports

%% Properties
-export([name/1, arity/1, dirty/1, autoimported/2, typeguard/2, exported/1]).

%% Queries
-export([find/2, definition/0, module/0, applications/0, applications/1,
         implicits/0, implicits/1, 
         exports/0, imports/0, imported/0, impexps/0]).

%% Transformations
-export([add_export/1, del_export/1]).


%% =============================================================================
%% Function related properties

%% @spec name(node()) -> atom()
%% @doc Returns the name of the function.
name(Fun) -> (?Graph:data(Fun))#func.name.

%% @spec arity(node()) -> integer()
%% @doc Returns the arity of the function.
arity(Fun) -> (?Graph:data(Fun))#func.arity.

%% @spec dirty(node()) -> bool()
%% @doc Returns `true' if the function is not side effect free.
dirty(Fun) -> (?Graph:data(Fun))#func.dirty.

%% @spec exported(node(#func{})) -> bool()
%% @doc Returns `true' if the function is exported.
exported(Fun) ->
    case ?Query:exec(Fun, [{funexp, back}]) of
        [] -> false;
        _  -> true
    end.

%% @spec autoimported(atom(), integer()) -> bool()
%% @doc Returns `true' if the function `Name/Arity' is an autoimported BIF.
autoimported(Name, Arity) ->
    erl_internal:bif(Name, Arity).

%% @spec typeguard(atom(), integer()) -> bool()
%% @doc Returns `true' if the function `Name/Arity' is a type test (used
%% exclusively in guards).
typeguard(Name, Arity) ->
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


%% @spec applications() -> query(#func{}, #expr{})
%% @doc The result query returns every application that calls the function.
applications() ->
    [{{funref,back}, {kind, '==', application}}].

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
    [{{funref,back},{kind,'==',implicit_fun}}].

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
    Path = [sup, {{attr,back}, {{type,'==',attrib}, 'and', {tag,'==',export}}}],
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
    Path = [sup, {{attr,back}, {{type,'==',attrib}, 'and', {tag,'==',import}}}],
    Filter = fun(E) -> [] =/= ?Query:exec(E, Path) end,
    filtered_query(impexps(), Filter).

%% @doc The result query returns the import and export list element expressions
%% that refer the function.
impexps() ->
    [{{funref,back},{kind,'==',infix_expr}}].


%% @spec filtered_query(query(), (node()) -> bool()) -> query(#func{}, #expr{})
%% @doc Returns a query that can filter the results of `Query' by a
%% `Filter' fun.
filtered_query(Query, Filter) ->
    fun(FunObj) ->
            [E || E <- ?Query:exec(FunObj, Query), Filter(E)]
    end.


%% =============================================================================
%% Function related transformations

%% @spec add_export(node()) -> ok
%% @doc Adds `Fun' to an arbitrary export list.
add_export(Fun) ->
    case exported(Fun) of
        true  -> ok;
        false ->
            [Mod] = ?Query:exec(Fun, module()),
            File = ?Query:exec1(Mod, ?Mod:file(),
                                ?RefError(no_file,[module,Mod])),
            NameExpr = ?Syn:create(#expr{kind=atom}, 
                                   [?MISC:to_list(name(Fun))]),
            ArityExpr = ?Syn:create(#expr{kind=integer}, 
                                    [?MISC:to_list(arity(Fun))]),
            ListItem = ?Syn:create(#expr{kind=infix_expr, value='/'},
                                   [{sub, [NameExpr, ArityExpr]}]),
            List = ?Syn:create(#expr{kind=cons},
                               [{sub, [?Syn:create(#expr{kind=list},
                                                   [{sub, ListItem}])]}]),
            Form = ?Syn:create(#form{type=attrib, tag=export},
                               ["-", "export", {attr, List}]),
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
