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
%%% The Initial Developer of the  Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s  Lor�nd University are  Copyright 2008-2009,
%%% E�tv�s Lor�nd University. All Rights Reserved.

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
%%%        (see {@link reflib_args:expr_range/1}). </li>
%%%   <li> The name of the function to introduce
%%%        (see {@link reflib_args:name/1}). </li>
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
%%% @author Melinda T�th <toth_m@inf.elte.hu>

-module(reftr_extract_fun).
-vsn("$Rev: 4706 $"). % for emacs"

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Exprs   = ?Args:expr_range(Args),
    {Link, Parent} = check_expr_link(Exprs),
    NewName = ?Args:name(Args),
    Module = ?Args:module(Args),
    lists:foreach(fun check_expr/1, Exprs),
    Form = ?Query:exec1(hd(Exprs), ?Query:seq([?Expr:clause(),
                                               ?Clause:funcl(),
                                               ?Clause:form()]),
                        ?RefErr0r(parent_not_form)),
    [File] = ?Query:exec(Form, ?Form:file()),
    {Bound, NotBound} = vars(Exprs),
    check_var(Bound,Exprs),
    DelExprs = Exprs,
    PatNames = lists:usort([?Var:name(Var) || Var <- NotBound]),
    Arity = length(PatNames),
    check_fun(Module, NewName, Arity),
    FormIndex  = ?Syn:index(File, form, Form),
    ?Transform:touch(File),
    {NewExprs, CommentExprs} = eliminate_begin_end(Exprs),
    Comments = ?Syn:get_comments(CommentExprs),
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
    case {?Expr:type(hd(Exprs)), length(Exprs)} of
        {block_expr, 1} ->
            NewExprs = ?Query:exec(hd(Exprs), 
                                 ?Query:seq(?Expr:clauses(), ?Clause:body())),
            {[proplists:get_value(Node, ?Syn:copy(Node)) || Node <- NewExprs],
             NewExprs};
        _ -> {Exprs, Exprs}
    end.

%%% ----------------------------------------------------------------------------
%%% Checks

%% Note: the expression list is never empty, guaranteed by `?Args:expr_range/1'.
check_expr_link(E = [Expr1|Rest])->
    case ?Syn:parent(Expr1) of
        [{body, Par}] ->
            ?Check(Rest =:= [] orelse ?Expr:type(Expr1) =/= filter,
                   ?RefError(bad_kind, filter)),
            {body, Par};
        [{esub, Par}]  ->
            ParKind = ?Expr:type(Par),
            ParVal = ?Expr:value(Par),
            Index = ?Syn:index(Par, esub, Expr1),
            ?Check(ParKind =/= application orelse
                   length(E) =/= 1 orelse Index =/= 1,
                   ?RefError(bad_kind, 'application name')),
            ?Check((ParKind =/= infix_expr orelse ParVal =/= ':') orelse
                   length(E) =/= 1,
                   ?RefError(bad_kind, 'module qualifier')),
            ?Check(Rest =:= [], ?RefErr0r(bad_range)),
            {esub, Par};
        [{pattern, _Par}] -> throw(?RefError(bad_kind, pattern));
        [{guard, _Par}] -> throw(?RefError(bad_kind, guard));
        _ ->  throw(?RefErr0r(bad_kind))
    end.

check_expr(Expr) ->
    Type = ?Expr:role(Expr),
    Kind = ?Expr:type(Expr),
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
    ?Check(Kind =/= record_field,
           ?RefError(bad_kind, record_field)),
    ?Check(Kind =/= list orelse
           length(?Query:exec(Expr, ?Expr:children())) ==1,
           ?RefError(bad_kind, list_elements)),
    case ?Query:exec(Expr, ?Expr:parent()) of
        []        -> ok;
        [Parent]  ->
            ParKind = ?Expr:type(Parent),
            ?Check(ParKind =/= binary_field andalso
                   ParKind =/= prefix_bit_expr andalso
                   ParKind =/= bit_size_expr andalso
                   ParKind =/= size_qualifier,
                   ?RefError(bad_kind, binary)),
            ?Check(ParKind =/= implicit_fun,
                   ?RefError(bad_kind, implicit_fun_part)),
            ?Check(ParKind =/= record_access,
                   ?RefError(bad_kind, record_access_part)),
            ?Check(ParKind =/= record_update,
                   ?RefError(bad_kind, record_update_part)),
            ?Check(ParKind =/= record_field orelse
                   ?Syn:index(Parent, esub, Expr) =/= 1,
                   ?RefError(bad_kind, record_field_name)),
            ?Check(ParKind =/= record_expr orelse
                   ?Syn:index(Parent, esub, Expr) =/= 1,
                   ?RefError(bad_kind, record_name)),
            ?Check(ParKind =/= record_expr orelse
                   ?Syn:index(Parent, esub, Expr) =/= 2,
                   ?RefError(bad_kind, record_expr_part))
    end.


check_fun(Module, NewName, Arity) ->
    ModName = ?Mod:name(Module),
    ?Check(?Query:exec(Module, ?Mod:local(NewName, Arity)) =:= [],
           ?RefError(fun_exists, [ModName, NewName, Arity])),
    ?Check(?Query:exec(Module, ?Mod:imported(NewName, Arity)) =:= [],
           ?RefError(imported_fun_exists, [ModName, [NewName, Arity]])),
    ?Check(not ?Fun:is_autoimported(NewName, Arity),
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
    Name     = ?Syn:create(#expr{type = atom}, [io_lib:write(AppName)]),
    Args     = [?Syn:create(#expr{type = variable}, [AName])
                                || AName <- AppArgNames],
    ArgList  = ?Syn:create(#expr{type = arglist}, [{esub, Args}]),
    App      = ?Syn:create(#expr{type = application}, [{esub, [Name]},
                                                       {esub, ArgList}]),

    case Type of
        esub  -> ?Syn:replace(Parent, {node, hd(DelExprs)}, [App]);
        body -> Dels = {range, hd(DelExprs), lists:last(DelExprs)},
                ?Syn:replace(Parent, Dels, [App])
    end.

add_fun_form(File, FunName, ClPatternNames, Body, FormIndex) ->
    ClPatternNodes = [?Syn:create(#expr{type = variable},[Name])
                     || Name <- ClPatternNames ],
    FName   = ?Syn:create(#expr{type = atom}, [io_lib:write(FunName)]),
    ClNode  = ?Syn:create(#clause{type = fundef},[{name, FName},
                             {pattern, ClPatternNodes},{body, Body}]),
    FunForm = ?Syn:create(#form{type = func}, [{funcl,ClNode}]),
    ?File:add_form(File, FormIndex + 1, FunForm).