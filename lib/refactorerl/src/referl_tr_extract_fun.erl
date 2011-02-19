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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the extract function refactoring.
%%% An alternative of a function definition might contain an expression
%%% (or a sequence of expressions) which can be considered as a logical unit,
%%% hence a function definition can be created from it. The extracted function
%%% is lifted to the module level, and it is parametrized with the `free'
%%% variables of the original expression(s): those variables which are bound
%%% outside of the expression(s), but the value of which is used by the
%%% expression(s). The extracted function will not be exported from the module.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li> A non-empty, continuous sequence of expressions
%%%        (see {@link referl_args:expr_range/1}). </li>
%%%   <li> The name of the function to introduce
%%%        (see {@link referl_args:name/1}). </li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li> The name of the function to be introduced should not conflict with
%%%   another function, either defined in the same module, imported from
%%%   another module, or being an autoimported built-in function (overloading).
%%%    Furthermore, the name should be a legal function name.</li>
%%%   <li> The starting and ending positions should delimit a sequence of
%%%   expressions.</li>
%%%   <li> Variables with possible binding occurrences in the selected
%%%   sequence of expressions should not appear outside of the
%%%   sequence of expressions.</li>
%%%   <li> The extracted sequence of expressions cannot be part of a guard
%%%   sequence.</li>
%%%   <li> The extracted sequence of expressions cannot be part of a
%%%   pattern.</li>
%%%   <li> The extracted sequence of expressions cannot be part of macro
%%%   definition, and are not part of macro application parameters.</li>
%%%   <li> If the selection is a part of a list comprehension, it must be
%%%   a single expression and must not be the generator of the comprehension.
%%%   </li>
%%% </ul>
%%%
%%% == Rules of the transformation ==
%%% <ol>
%%%   <li> Collect all variables that the selected sequence of expressions
%%%   depends on.</li>
%%%   <li> Collect variables from the selected variables in step 1, which has
%%%   binding occurrence out of the selected part of the module. </li>
%%%   <li> Add a new function definition to the current module with a single
%%%   alternative. The name of the function is an argument to the
%%%   refactoring. The formal parameter list consists of the variables
%%%   selected in step 2.</li>
%%%   <li> Replace the selected sequence of expressions with a function call
%%%   expression, where the name of the function is given as an argument
%%%   to the refactoring, and the actual parameter list consists of the
%%%   variables selected in step 2.</li>
%%%   <li> The order of the variables must be the same in steps 3 and 4.</li>
%%%   <li> If the selected expression is a block-expression, eliminate the
%%%   begin-end keywords from the expression in the body of the created new
%%%   function.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(referl_tr_extract_fun).
-vsn("$Rev: 2964 $").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1]).

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Exprs   = ?Args:expr_range(Args),
    {Link, Parent} = check_expr_link(Exprs),
    NewName = ?Args:name(Args),
    Module = ?Args:module(Args),
    lists:foreach(fun check_expr/1, Exprs),
    Form = ?Query:exec1(hd(Exprs), ?Query:seq(?Expr:clause(), ?Clause:form()),
                        ?RefErr0r(parent_not_form)),
    [File] = ?Query:exec(Form, ?Form:file()),
    {Bound, NotBound} = vars(Exprs),
    check_var(Bound,Exprs),
    DelExprs = Exprs,
    NewExprs = eliminate_begin_end(Exprs),
    Comments = ?Syn:get_comments(NewExprs),
    PatNames = lists:usort([?Var:name(Var) || Var <- NotBound]),
    Arity = length(PatNames),
    check_fun(Module, NewName, Arity),
    FormIndex  = ?Syn:index(File, form, Form),
    ?Transform:touch(File),
    [fun() ->
         replace_with_app(Parent, NewName, PatNames, DelExprs, Link),
         add_fun_form(File, NewName, PatNames, NewExprs, FormIndex)
    end,
    fun(_) ->
            ?Syn:put_comments(NewExprs, Comments)
    end].

vars(Exprs) ->
    VarBinds = lists:usort(?Query:exec(Exprs, ?Expr:varbinds())),
    VarRefs  = lists:usort(?Query:exec(Exprs, ?Expr:varrefs())),
    {VarBinds, VarRefs -- VarBinds}.


eliminate_begin_end(Exprs) ->
    case {?Expr:kind(hd(Exprs)), length(Exprs)} of
        {block_expr, 1} ->
            ?Query:exec(hd(Exprs), ?Query:seq(?Expr:clauses(), ?Clause:body()));
        _ -> Exprs
    end.

%%% ----------------------------------------------------------------------------
%%% Checks

%% Note: the expression list is never empty, guaranteed by `?Args:expr_range/1'.
check_expr_link(E = [Expr1|Rest])->
    case ?Syn:parent(Expr1) of
        [{body, Par}] ->
            ?Check(Rest =:= [] orelse ?Expr:kind(Expr1) =/= filter,
                   ?RefError(bad_kind, filter)),
            {body, Par};
        [{sub, Par}]  ->
            ParKind = ?Expr:kind(Par),
            ParVal = ?Expr:value(Par),
            Index = ?Syn:index(Par, sub, Expr1),
            ?Check(ParKind =/= application orelse
                   length(E) =/= 1 orelse Index =/= 1,
                   ?RefError(bad_kind, 'application name')),
            ?Check((ParKind =/= infix_expr orelse ParVal =/= ':') orelse
                   length(E) =/= 1,
                   ?RefError(bad_kind, 'module qualifier')),
            ?Check(Rest =:= [], ?RefErr0r(bad_range)),
            {sub, Par};
        [{pattern, _Par}] -> throw(?RefError(bad_kind, pattern));
        [{guard, _Par}] -> throw(?RefError(bad_kind, guard));
        _ ->  throw(?RefErr0r(bad_kind))
    end.

check_expr(Expr) ->
    Type = ?Expr:type(Expr),
    Kind = ?Expr:kind(Expr),
    ?Check(Type =:= expr,
           ?RefError(bad_kind, Type)),
    ?Check(Kind =/= compr andalso
           Kind =/= list_gen,
           ?RefError(bad_kind, list_comp)),
    ?Check(Kind =/= binary_gen andalso
           Kind =/= binary_field andalso
           Kind =/= prefix_bit_expr andalso
           Kind =/= bit_size_expr andalso
           Kind =/= size_qualifier,
           ?RefError(bad_kind, binary)),
    case ?Query:exec(Expr, ?Expr:parent()) of
        []        -> ok;
        [Parent]  ->
            ParKind = ?Expr:kind(Parent),
            ?Check(ParKind =/= binary_field andalso
                   ParKind =/= prefix_bit_expr andalso
                   ParKind =/= bit_size_expr andalso
                   ParKind =/= size_qualifier,
                   ?RefError(bad_kind, binary))
    end.


check_fun(Module, NewName, Arity) ->
    ModName = ?Mod:name(Module),
    ?Check(?Query:exec(Module, ?Mod:local(NewName, Arity)) =:= [],
           ?RefError(fun_exists, [ModName, NewName, Arity])),
    ?Check(?Query:exec(Module, ?Mod:imported(NewName, Arity)) =:= [],
           ?RefError(imported_fun_exists, [ModName, [NewName, Arity]])),
    ?Check(?Fun:autoimported(NewName, Arity) =:= false,
           ?RefError(autoimported_fun_exists, [NewName, Arity])).

check_var(BoundVars, Exprs)->
    Occurrences  = lists:flatten([?Query:exec(BoundVars,
                                              ?Var:occurrences(Expr)) ||
                                  Expr <- Exprs]),
    AllVarOccurs = ?Query:exec(BoundVars, ?Var:occurrences()),
    Clash = lists:usort([list_to_atom(?Expr:value(Var)) ||
                         Var <- (AllVarOccurs -- Occurrences)]),
    ?Check( Clash =:= [],
           ?RefError(outside_used_vars, Clash)).


%%% ===========================================================================
%%% Syntactic transformations.

replace_with_app(Parent, AppName, AppArgNames, DelExprs, Type)->
    Name = ?Syn:create(#expr{kind = atom}, [io_lib:write(AppName)]),
    Args = [?Syn:create(#expr{kind = variable}, [AName])
                                || AName <- AppArgNames],
    App  = ?Syn:create(#expr{kind = application}, [{sub, [Name]},
                               {sub, Args}]),
    case Type of
        sub  -> ?Syn:replace(Parent, {node, hd(DelExprs)}, [App]);
        body -> Dels = {range, hd(DelExprs), lists:last(DelExprs)},
                ?Syn:replace(Parent, Dels, [App])
    end.

add_fun_form(File, FunName, ClPatternNames, Body, FormIndex) ->
    ClPatternNodes = [?Syn:create(#expr{kind = variable},[Name])
                     || Name <- ClPatternNames ],
    FName   = ?Syn:create(#expr{kind = atom}, [io_lib:write(FunName)]),
    ClNode  = ?Syn:create(#clause{kind = fundef},[{name, FName},
                             {pattern, ClPatternNodes},{body, Body}]),
    FunForm = ?Syn:create(#form{type = func}, [{funcl,ClNode}]),
    ?File:add_form(File, FormIndex + 1, FunForm).
