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

%%% @author Jimmy <>
%%%
%%% @doc todo
%%%
%%% @todo make representation change
%%%       Missing functions:
%%%         reflib_clause:kind/1
%%%             &lt;- referl_gs_sup:children/2
%%%         reflib_expression:kind/1
%%%             &lt;- referl_gs_sup:children/2

-module(referl_gs_sup).

-svn("$Rev$ ").

%% Exports
-export([root/0, children/1, info/1, text/1, find_node/1,
         tr_buttons/1, tool_buttons/0]).

%% Includes
-include("ui.hrl").
-include("gs.hrl").
-include_lib("referl_core/include/core_export.hrl").

%% Node index
-define(Index(X), element(3, X)). % TODO: find a function!!!

%% Missing queries
-define(QFile, [file]).
-define(RecDef, [recdef]).
-define(RecDefBack, [{recdef, back}]).
-define(Field, [field]).
-define(FieldDef, [{fielddef, back}]).
-define(RecAttr, [attr]).
-define(VarBinds, [varbind]).
-define(VarRefs, [varref]).

%% Macros
-define(ia(IAA), list_to_atom(integer_to_list(IAA))).

%% The `undefined' problem
-ifndef(show_undefined).
-define(undefined(UD),
        case UD of
           undefined -> undefined;
           UDElse -> ?MISC:to_list(UDElse)
        end).
-else.
-define(undefined(UD), ?MIDC:to_list(UD)).
-endif.

%% Implementation

root() ->
    #tag{class=root, node=?Graph:root()}.

children(Tag) ->
    children(?Syn:class(Tag#tag.node), Tag#tag.node).

children(root, _) ->
    [ gather_file(X) || X <- ?Query:exec(?QFile)];
children(module, Module) ->
    Forms = ?Query:exec(Module, [?Mod:file(), ?File:forms()]),
    [ gather_form(X) || X <- Forms ,
                        lists:member(?Form:type(X), [macro, record, func]) ];
children(func, Node) ->
    case ?Syn:class(Node) of
        func ->
            Clauses = ?Query:exec(Node, [?Fun:definition(), ?Form:clauses()]),
            [ #tag{class=clause,
                   type=?Clause:var(Clause),
                   kind=?Clause:type(Clause),
                   name=integer_to_list(X),
                   id=integer_to_list(?Index(Clause)),
                   node=Clause} ||
                {Clause, X} <- lists:zip(Clauses,
                                         ?MISC:seq2(1, length(Clauses)))];
        _ -> []
    end;
children(clause, Clause) ->
    Patterns = ?Query:exec(Clause, ?Clause:patterns()),
    Guard = ?Query:exec(Clause, ?Clause:guard()),
    Bodys = ?Query:exec(Clause, ?Clause:body()),
    Nodes = lists:flatten([Patterns, Guard, Bodys]),
    [#tag{class=expr,
          type=?Expr:role(X),
          kind=?Expr:type(X),
          id=?Index(X),
          name=?undefined(?Expr:value(X)),
          node=X} || X <- Nodes ];
children(expr, Expr) ->
    VarBinds = ?Query:exec(Expr, ?VarBinds),
    VarRefs = ?Query:exec(Expr, ?VarRefs),
    Children = ?Query:exec(Expr, ?Expr:children()),
    Clauses = ?Query:exec(Expr, ?Expr:clauses()),
    VarTags = [ #tag{class=variable,
                     id=integer_to_list(?Index(X)),
                     name=?Var:name(X),
                     node=X} || X <- VarBinds ++ VarRefs ],
    ExprTags = [ #tag{class=expr,
                      type=?Expr:role(X),
                      kind=?Expr:type(X),
                      id=integer_to_list(?Index(X)),
                      name=?undefined(?Expr:value(X)),
                      node=X} || X <- Children ],
    ClauseTags = [ #tag{class=clause,
                        type=?Clause:var(Clause),
                        kind=?Clause:type(Clause),
                        id=integer_to_list(?Index(Clause)),
                        node=Clause} || Clause <- Clauses ],
    lists:flatten([VarTags, ExprTags, ClauseTags]);
children(record, Rec) ->
    Attributes = ?Query:exec(Rec, [?RecDefBack, ?RecAttr]),
    [ #tag{class=?Syn:class(Node), name=integer_to_list(X),
           id=integer_to_list(?Index(Node)), node=Node} ||
        {Node, X} <- lists:zip(Attributes,
                               ?MISC:seq2(1, length(Attributes))) ];
children(_, _) ->
    [].

gather_file(File) ->
    Type = ?File:type(File),
    case Type of
        module ->
            [Node] = ?Query:exec(File, ?File:module()),
            Name = ?MISC:to_list(?Mod:name(Node));
        header ->
            Node = File,
            Name = filename:basename(?File:path(File))
    end,
    #tag{class=Type, id=integer_to_list(?Index(Node)),
         name=Name, node=Node}.

gather_form(Form) ->
    Type = ?Form:type(Form),
    case Type of
        macro ->
            Node = Form,
            Name = ?Macro:name(Node);
        record ->
            [Node] = ?Query:exec(Form, ?RecDef),
            Name = ?MISC:to_list(?Rec:name(Node));
        func ->
            [Node] = ?Query:exec(Form, ?Form:func()),
            Name =
                ?MISC:to_list(?Fun:name(Node)) ++ "/" ++
                integer_to_list(?Fun:arity(Node))
    end,
    #tag{class=Type, id=integer_to_list(?Index(Node)),
         name=Name, node=Node}.

find_node(History) ->
    find_node(lists:reverse(History), []).

find_node([], [A | B]) -> {A, B};
find_node([Tag | Tail], NewHistory) ->
    try ?Graph:data(Tag#tag.node) of
        _ -> find_node(Tail, [Tag | NewHistory])
    catch
        error:bad_node ->
            [A | B] = NewHistory,
            {A , B}
    end.

info(Tag) ->
    info(Tag#tag.class, Tag#tag.node).

info(root, _) ->
    Files = ?Query:exec(?QFile),
    FileNum = integer_to_list(length(Files)),
    [{files, {string, FileNum}}];
info(module, Mod) ->
    ModName = atom_to_list(?Mod:name(Mod)),
    [File] = ?Query:exec(Mod, ?Mod:file()),
    FileName = ?File:path(File),
    Locals = ?Query:exec(Mod, ?Mod:locals()),
    LocalNames = [ atom_to_list(X) ++ "/" ++ integer_to_list(Y) ||
                      {X, Y} <- [ {?Fun:name(F), ?Fun:arity(F)} ||
                                    F <- Locals ] ],
    ExportedNames = [ atom_to_list(X) ++ "/" ++ integer_to_list(Y) ||
                        {X, Y} <- [ {?Fun:name(F), ?Fun:arity(F)} ||
                                      F <- Locals,
                                      ?Fun:is_exported(F) ] ],
    Macros = ?Query:exec(File, ?File:macros()),
    MacroNames = [ ?Macro:name(X) || X <- Macros],
    Records = ?Query:exec(File, ?File:records()),
    RecordNames = [ atom_to_list(?Rec:name(X)) || X <- Records],
    [{name, {string, ModName}},
     {file, {string, FileName}},
     {functions, {list, LocalNames}},
     {exported, {list, ExportedNames}},
     {macros, {list, MacroNames}},
     {records, {list, RecordNames}}];
info(macro, Macro) ->
    Name = ?Macro:name(Macro),
    [{name, {string, Name}}];
info(record, Rec) ->
    Name = atom_to_list(?Rec:name(Rec)),
    Fields = ?Query:exec(Rec, ?Field),
    FieldsNum = integer_to_list(length(Fields)),
    FieldNames =
        [atom_to_list(?Expr:value(hd(?Query:exec(Field, ?FieldDef)))) ||
           Field <- Fields ],
    [{name, {string, Name}},
     {fieldnum, {string, FieldsNum}},
     {fieldnames, {list, FieldNames}}];
info(func, Fun) ->
    Name = atom_to_list(?Fun:name(Fun)),
    Arity = integer_to_list(?Fun:arity(Fun)),
    Exported = atom_to_list(?Fun:is_exported(Fun)),
    FunDef = ?Query:exec(Fun, ?Fun:definition()),
    Clauses = ?Query:exec(FunDef, ?Form:clauses()),
    ClauseNum = integer_to_list(length(Clauses)),
    [{name, {string, Name}},
     {arity, {string, Arity}},
     {exported, {string, Exported}},
     {clausenum, {string, ClauseNum}}];
info(clause, Clause) ->
    HasGuard = atom_to_list([] /= ?Query:exec(Clause, ?Clause:guard())),
    LineNum = integer_to_list(length(?Query:exec(Clause, ?Clause:body()))),
    [{hasguard, {string, HasGuard}},
     {linenum, {string, LineNum}}];
info(variable, Var) ->
    VarName = ?Var:name(Var),
    Bindings = ?Query:exec(Var, ?Var:bindings()),
    BindingNum = integer_to_list(length(Bindings)),
    Refs = ?Query:exec(Var, ?Var:references()),
    RefNum = integer_to_list(length(Refs)),
    [{name, {string, VarName}},
     {varbindingnum, {string, BindingNum}},
     {varrefnum, {string, RefNum}}];
info(_, _) ->
    [].

text(Tag) ->
    text(Tag#tag.class, Tag#tag.node).

text(root, _) -> [];
text(variable, _) -> [];
text(module, Mod) ->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    ?Syn:tree_text(File);
text(func, Fun) ->
    [Form] = ?Query:exec(Fun, ?Fun:definition()),
    ?Syn:tree_text(Form);
text(record, Rec) ->
    [Form] = ?Query:exec(Rec, ?Rec:form()),
    ?Syn:tree_text(Form);
text(_, Node) ->
    ?Syn:tree_text(Node).

tool_buttons() ->
    Undo =
        fun(_) ->
                ?UI:undo([])
        end,
    [#tr{label="UNDO",
         back=undo,
         func=Undo,
         kind=undo}].

tr_buttons(#tag{class=variable, node=Var}) ->
    [Token] = ?Query:exec(Var, [?Var:bindings(), [elex]]),
    [File] = ?Query:exec(Token, ?Token:file()),
    FileName = ?File:path(File),
    {Pos, _} = ?Token:pos(Token),
    Rename =
        fun(Args) ->
                Name = proplists:get_value(varname, Args),
                 ?UI:transform(reftr_rename_var,
                               [{file, FileName}, {position, Pos},
                               {varname, Name}])
        end,
    Eliminate =
        fun(_) ->
                ?UI:transform(reftr_elim_var,
                              [{file, FileName}, {position, Pos}])
        end,
    [#tr{label="Rename",
         map=true,
         desc="Rename Variable",
         prop=[{entry, varname, "New name: "}],
         func=Rename},
     #tr{label="Eliminate",
         back=true,
         func=Eliminate}];
tr_buttons(#tag{class=func, node=Fun}) ->
    [Mod] = ?Query:exec(Fun, ?Fun:module()),
    ModName = ?Mod:name(Mod),
    FileName = ?File:path(hd(?Query:exec(Mod, ?Mod:file()))),
    FunName = ?Fun:name(Fun),
    Arity = ?Fun:arity(Fun),
    FirstClause = hd(?Query:exec(Fun, [?Fun:definition(), ?Form:clauses()])),
    Patterns = ?Query:exec(FirstClause, ?Clause:patterns()),
    Rename =
        fun(Args) ->
                Name = proplists:get_value(name, Args),
                ?UI:transform(reftr_rename_fun,
                              [{module, ModName}, {function, FunName},
                               {arity, Arity}, {name, Name}])
        end,
    Move =
        fun(Args) ->
                Name = proplists:get_value(name, Args),
                ?UI:transform(reftr_move_fun,
                              [{file, FileName},
                               {funlist, [{FunName, Arity}]},
                               {name, Name}])
        end,
    Reorder =
        fun(Args) ->
                Order = [ case proplists:get_value(?ia(X), Args) of
                              [] -> 0;
                              N -> list_to_integer(N)
                          end || X <- ?MISC:seq2(1, Arity) ],
                ?UI:transform(reftr_reorder_funpar,
                              [{module, ModName}, {function, FunName},
                               {arity, Arity}, {order, Order}])
        end,
    Tuple =
        fun(_) ->
                P1 = hd(Patterns),
                Pn = lists:last(Patterns),
                {First, _} = ?Token:pos(hd(?Query:exec(P1, [elex]))),
                {_, Last} = ?Token:pos(hd(?Query:exec(Pn, [elex]))),
                ?UI:transform(reftr_tuple_funpar,
                              [{file, FileName}, {posrange, {First, Last}}])
        end,
    [#tr{label="Rename",
         map=true,
         desc="Rename Function",
         prop=[{entry, name, "New name: "}],
         func=Rename},
     #tr{label="Move",
         map=true,
         desc="Move Function",
         prop=[{entry, name, "Module: "}],
         func=Move},
     #tr{label="Reorder",
         map=true,
         desc="Reorder Function Parameters",
         prop=[ {entry, ?ia(X),
                 "Argument " ++ integer_to_list(X) ++ "'s new pos.: "} ||
                  X <- ?MISC:seq2(1, Arity)],
         func=Reorder}] ++
        case length(Patterns) of
            0 -> [];
            _ ->
                [#tr{label="Tuple",
                     func=Tuple}]
        end;
tr_buttons(#tag{class=module, node=Mod}) ->
    FileName = ?File:path(hd(?Query:exec(Mod, ?Mod:file()))),
    Rename =
        fun(Args) ->
                Name = proplists:get_value(name, Args),
                ?UI:transform(reftr_rename_mod,
                              [{file, FileName}, {name, Name}])
        end,
    [#tr{label="Rename",
         map=true,
         desc="Rename Module",
         prop=[{entry, name, "New name: "}],
         back=true,
         func=Rename}];
tr_buttons(#tag{class=expr, kind=implicit_fun, node=Expr}) ->
    [File] = ?Query:exec(Expr,
                       ?Query:seq(
                          ?Query:seq(
                             ?Expr:clause(),
                             ?Clause:form()),
                          ?Form:file())),
    FileName = ?File:path(File),
    Token = hd(?Query:exec(Expr, [elex])),
    {Pos, _} = ?Token:pos(Token),
    Expand =
        fun(_) ->
                ?UI:transform(reftr_expand_funexpr,
                              [{file, FileName}, {position, Pos}])
        end,
    [#tr{label="Expand",
         func=Expand}];
tr_buttons(#tag{class=expr, kind=application, node=Expr}) ->
    [File] = ?Query:exec(Expr,
                       ?Query:seq(
                          ?Query:seq(
                             ?Expr:clause(),
                             ?Clause:form()),
                          ?Form:file())),
    FileName = ?File:path(File),
    Token = hd(?Query:exec(Expr, [?Expr:children(), elex])),
    {Pos, _} = ?Token:pos(Token),
    Inline =
        fun(_) ->
                ?UI:transform(reftr_inline_fun,
                              [{file, FileName}, {position, Pos}])
        end,
    [#tr{label="Inline",
         func=Inline}];
tr_buttons(_) ->
    [].
