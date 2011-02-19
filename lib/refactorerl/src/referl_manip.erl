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

%%% @doc Common syntax manipulation module.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(referl_manip).
-vsn("$Rev: 1972 $").

-include("refactorerl.hrl").
-include("referl_schema.hrl").

%%% ============================================================================
%%% Exports

-export([move_form/3, remove_form/1, insert_form/2]).
-export([add_import/4, add_export/3, remove_expimp/2]).
-export([add_include/2]).
-export([rename_import/2]).

-export([copy_include/2, move_include/3]).
-export([expand_funexpr/1]).

-export([insert_module_qualifier/2,
         remove_module_qualifier/1,
         update_module_qualifier/2]).

-export([refresh_semantic_links/2]).

%%% ============================================================================
%%% Insert/move/remove form

%% @spec move_form(node(), node(), node()) -> node()
%% @doc Removes the form from the original parent
%%      and inserts it below the new parent.
move_form(Form, FromParent, ToParent) ->
    ?ESG:remove(FromParent, form, Form),
    insert_form(ToParent, Form).


%% @spec remove_form(node()) -> node()
%% @doc Removes a form from its parent file.
remove_form(Form) ->
    [File] = ?GRAPH:path(Form, [{form, back}]),
    ?ESG:remove(File, form, Form).


%% @spec insert_form(node(), node()) -> ok
%% @doc Inserts a form below a file, maintaining the order of form types:
%%      `module', `export', `import', `macro', `record', `include'.
insert_form(File, Form) ->
    ?ESG:insert(File,
                {form, form_insert_index(File, ?SYNTAX:form_type(Form))},
                Form),
    ?ESG:close().


%% @spec form_insert_index(node(), atom()) -> integer() | last
%% @doc Determines the index of the new form.
form_insert_index(_File, func) -> last;
form_insert_index(File, Type) ->
    Antecedents = antecedent_forms(Type),
    Forms = ?GRAPH:path(File, [form]),
    PrevForms = [Form || Form <- Forms,
                         lists:member(?SYNTAX:form_type(Form),
                                      Antecedents)],
    length(PrevForms) + 1.


%% @spec antecedent_forms(atom()) -> [node()]
%% @doc Returns the form types that may appear earlier in the file.
antecedent_forms(Type) ->
    List = [module, export, import, macro, record, include],
    lists:takewhile(fun(Elem) -> Elem /= Type end, List) ++ [Type].


%%% ============================================================================
%%% Add/remove import/export


%% @spec remove_expimp(node(), node()) -> ok
%% @doc Removes an export or import form.
%%      If the list becomes empty, the whole form is removed.
remove_expimp(ListCons, Expr) ->
    [List] = ?GRAPH:path(ListCons, [{sub, 1}]),
    Exprs  = ?GRAPH:path(List, [sub]),
    case Exprs -- [Expr] of
        [] ->
            [Form] = ?GRAPH:path(ListCons, [{attr, back}]),
            remove_form(Form);
        _ ->
            ?SYNTAX:replace(List, {node, Expr}, [])
    end.


%% @spec add_export(node(), atom(), integer()) -> ok
%% @doc Adds an export form to the file that contains the `Name/Arity' pair.
add_export(File, Name, Arity) ->
    case ?SEMINF:is_exported(File, Name, Arity) of
        true  -> ok;
        false ->
            Export = ?SYNTAX:create_by_struct(
                        #form{type=attrib, tag=export},
                        [{force, "-"}, "export",
                         [single_list(Name, Arity)]]),
            insert_form(File, Export)
    end.


%% @spec add_import(node(), node(), atom(), integer()) -> ok
%% @doc Adds an export form to the file that contains the `Name/Arity' pair.
add_import(File, Module, Name, Arity) ->
    ModuleName = (?GRAPH:data(Module))#module.name,
    case ?SEMINF:is_imported(File, ModuleName, Name, Arity) of
        true  -> ok;
        false ->
            ModuleExpr = ?SYNTAX:create_by_struct(
                            #expr{kind=atom, value=ModuleName}, [?MISC:to_list(ModuleName)]),
            Import = ?SYNTAX:create_by_struct(#form{type=attrib, tag=import},
                           [{force, "-"}, "import", [ModuleExpr, single_list(Name, Arity)]]),
            insert_form(File, Import)
    end.

single_list(Name, Arity) ->
    ?SYNTAX:create_by_struct(#expr{kind=cons},
                   [?SYNTAX:create_by_struct(
                       #expr{kind=list},
                       [[expimp_item(Name, Arity)]]), no_opt]).

expimp_item(Name, Arity) ->
    ?SYNTAX:create_by_struct(#expr{kind=infix_expr, value='/'},
                   [?SYNTAX:create_by_struct(#expr{kind=atom, value=Name}, [?MISC:to_list(Name)]),
                    ?SYNTAX:create_by_struct(#expr{kind=integer, value=Arity}, [?MISC:to_list(Arity)])]).


%% @spec add_include(node(), string()) -> node()
%% @doc Adds an include form to the file if the include path is not present.
add_include(File, InclPath) ->
    case ?LEX:is_included(File, InclPath) of
        true ->
            ok;
        false ->
            Tokens =
                [?ESG:create(#lex{type=token,
                                  data=?SYNLEX:lex_elem(Type, Text)}) ||
                    {Type, Text} <-
                        [{minus, "-"},
                         {atom, "include"},
                         {op_paren, "("},
                         {string, "\"" ++ InclPath ++ "\""},
                         {cl_paren, ")"},
                         {stop, ".\n"}]],
            ?FILEMAN:add_form(File, form_insert_index(File, include), Tokens)
    end.


%%% ============================================================================
%%% Rename import

%% @doc Renames the module of an import form.
rename_import(ImportForm, Mod) ->
    #module{name = Name} = ?GRAPH:data(Mod),
    [ModNameExpr|_] = ?GRAPH:path(ImportForm, [{attr, 1}]),
    update_atom_expr(ModNameExpr, Name),
    refresh_semantic_links(form, ImportForm).

%%% ============================================================================
%%% Move/copy include

%% @spec move_include(node(), node(), node()) -> ok
%% @doc Moves an import form to another module.
move_include(Form, FromMod, ToMod) ->
    ?ESG:remove(FromMod, form, Form),
    copy_include(Form, ToMod).

%% @spec copy_include(node(), node()) -> ok
%% @doc Copies an import form to a module.
copy_include(Form, ToMod) ->
    Path = (?GRAPH:data(Form))#form.tag,
    [File] = ?GRAPH:path(ToMod, [{moddef, back}]),
    AlreadyIncluded = ?LEX:is_included(ToMod, Path),
    if AlreadyIncluded ->
            ok;
       true ->
            add_include(File, Path)
    end,
    ok.


%%% ============================================================================
%%% Expand implicit fun expression


%%% ----------------------------------------------------------------------------
%%% Top interface

%% @spec expand_funexpr(node()) -> node()
%% @doc Makes an explicit fun expression in place of an implicit one.
expand_funexpr(Expr) ->
    [{Link,Parent}] = ?ESG:parent(Expr),
    Index = ?GRAPH:index(Parent, Link, Expr),
    {New, App} = case ?SYNTAX:structure_with_nodes(Expr) of
              [_,{symbol,_,ModuleName},{optional, [_,{symbol,_,FunName}]},
               _,{symbol,_,Arity}] ->
                  #expr{value=ArityInt} = ?GRAPH:data(Arity),
                  expand({Expr, Parent, Link, Index}, ModuleName, FunName, ArityInt);
              [_, {symbol,_,FunName},{optional, []},_, {symbol, _, Arity}] ->
                  #expr{value=ArityInt} = ?GRAPH:data(Arity),
                  expand({Expr, Parent, Link, Index}, FunName, ArityInt)
          end,
    {New, App}.

%%% ----------------------------------------------------------------------------
%%% 4 cases

expand(D, FunName, 0) ->
    FunId = FunName,
    Pattern = [no_opt],
    VarNodes = [no_opt],
    expand_final(D, FunId, Pattern, VarNodes);

expand(D, FunName, ArityInt) ->
    FunId = FunName,
    Vars = get_var_names("V", ArityInt, []),
    Pattern = optlist([?SYNTAX:create_by_struct(
                           #expr{type=pattern, kind=variable, value=V}, [V])
                        || V <- Vars]),
    VarNodes = optlist([?SYNTAX:create_by_struct(
                           #expr{kind=variable, value=V}, [V])
                        || V <- Vars]),
    expand_final(D, FunId, Pattern, VarNodes).

expand(D, ModuleName, FunName, 0) ->
    FunId = ?SYNTAX:create_by_struct(#expr{kind=infix_expr, value=':'},
                            [ModuleName, FunName]),
    Pattern = [no_opt],
    VarNodes = [no_opt],
    expand_final(D, FunId, Pattern, VarNodes);

expand(D, ModuleName, FunName, ArityInt) ->
    FunId = ?SYNTAX:create_by_struct(#expr{kind=infix_expr, value=':'},
                            [ModuleName, FunName]),
    Vars = get_var_names("V", ArityInt, []),
    Pattern = optlist([?SYNTAX:create_by_struct(
                           #expr{type=pattern, kind=variable, value=V}, [V])
                        || V <- Vars]),
    VarNodes = optlist([?SYNTAX:create_by_struct(
                           #expr{kind=variable, value=V}, [V])
                        || V <- Vars]),
    expand_final(D, FunId, Pattern, VarNodes).


%%% ----------------------------------------------------------------------------
%%% Helper funs

expand_final({Expr, Parent, Link, Index}, FunId, Pattern, VarNodes) ->
    {Clause, App} = new_funexpr_clause(Pattern, FunId, VarNodes),
    New = ?SYNTAX:create_by_struct(#expr{kind=fun_expr},
                                   [Clause]),
    ?ESG:remove(Parent, Link, Expr),
    ?ESG:insert(Parent, {Link, Index}, New),
    {New, App}.

new_funexpr_clause(Pattern, FunId, VarNodes) ->
    App = ?SYNTAX:create_by_struct(#expr{kind=application},
                                   [FunId] ++ VarNodes), 
    {[?SYNTAX:create_by_struct(#clause{kind=funexpr},
                               Pattern ++ [no_opt] ++ [[App]])], App}.

get_var_names(_, 0, List) ->
    List;
get_var_names(Prefix, Count, List) ->
    get_var_names(Prefix, Count-1, [Prefix++integer_to_list(Count) | List]).

optlist(List) ->
    [{opt, [List]}].

%%% ============================================================================
%%% Insert/remove/update module qualifier

%% @spec insert_module_qualifier(node(), node()) -> ok
%% @doc Inserts a module qualifier to a function application.
insert_module_qualifier(Expr, ModuleNode) ->
    #module{name = ModuleName} = ?GRAPH:data(ModuleNode),
    [FunNameNode|_] = ?GRAPH:path(Expr, [sub]),
    ModNameNode = 
        ?SYNTAX:create_by_struct(#expr{kind=atom, value=ModuleName},
                                 [?MISC:to_list(ModuleName)]),
    case ?GRAPH:data(Expr) of
        #expr{kind = application} ->
            InfixExpr = 
                ?SYNTAX:create_by_struct(#expr{kind=infix_expr, value=':'},
                                         [ModNameNode, FunNameNode]),
            ?SYNTAX:replace(Expr, {node, FunNameNode}, [{sub, InfixExpr}]);
        #expr{kind = implicit_fun} ->
            ?SYNTAX:replace(Expr, {before, FunNameNode}, [{sub, ModNameNode}])
    end.

%% @spec remove_module_qualifier(node()) -> ok
%% @doc Removes the module qualifier of a qualified function application.
remove_module_qualifier(Expr) ->
    case ?GRAPH:data(Expr) of
        #expr{kind = application} ->
            [InfixExpr|_] = ?GRAPH:path(Expr, [sub]),
            [_M, FunNameExpr] = ?GRAPH:path(InfixExpr, [sub]),
            ?SYNTAX:replace(Expr, {node, InfixExpr}, [{sub, FunNameExpr}]);
        #expr{kind = implicit_fun} ->
            [ModNameExpr, FunNameExpr | _] = ?GRAPH:path(Expr, [sub]),
            ?SYNTAX:replace(Expr, {ModNameExpr, FunNameExpr}, [{sub, FunNameExpr}])
    end.

%% @spec update_module_qualifier(node(), node()) -> ok
%% @doc Changes the module qualifier of a qualified function application
%%      to that of the given module.
update_module_qualifier(Expr, ModuleNode) ->
    #module{name = NewModuleName} = ?GRAPH:data(ModuleNode),
    case ?GRAPH:data(Expr) of
        #expr{kind = application} ->
            [InfixExpr|_] = ?GRAPH:path(Expr, [sub]),
            [ModNameExpr, _FunNameExpr] = ?GRAPH:path(InfixExpr, [sub]),
            update_atom_expr(ModNameExpr, NewModuleName);
        #expr{kind = implicit_fun} ->
            [ModNameExpr, _FunNameExpr | _] = ?GRAPH:path(Expr, [sub]),
            update_atom_expr(ModNameExpr, NewModuleName)
    end.

update_atom_expr(Expr, Atom) ->
    [Lex] = ?GRAPH:path(Expr, [elex]),
    EData = ?GRAPH:data(Expr),
    LData = ?GRAPH:data(Lex),
    TData = LData#lex.data,
    ?ESG:update(Expr, EData#expr{value=Atom}),
    ?ESG:update(Lex, LData#lex{data=TData#token{value=Atom, text=?MISC:to_list(Atom)}}).

%%% ============================================================================
%%% Reinsert funref/form

%% @doc Removes and reinserts a node in order to reestablish semantic links.
refresh_semantic_links(Type, Node) ->
    {Parent, Link, Index} = parent_link_index(Type, Node),
    ?ESG:remove(Parent, Link, Node),
    ?ESG:insert(Parent, {Link, Index}, Node).

%% @doc Returns the node's parent, the parent-child node link and the child index.
parent_link_index(form, Form) ->
    [Parent] = ?GRAPH:path(Form, [{form, back}]),
    Index = ?GRAPH:index(Parent, form, Form),
    {Parent, form, Index};
parent_link_index(funref, FunRef) ->
    {Parent, Link} = expr_parent_link(FunRef),
    Index = ?GRAPH:index(Parent, Link, FunRef),
    {Parent, Link, Index}.


expr_parent_link(Node) ->
    ParentLinks =
        [ {?GRAPH:path(Node, [{Link, back}]), Link}
          || {_From, _, Links} <- ?SYNTAX_SCHEMA, {Link, expr} <- Links],
    [{[Parent], Link}] =
        lists:filter(fun({Parent,_}) -> Parent /= [] end, ParentLinks),
    {Parent, Link}.
