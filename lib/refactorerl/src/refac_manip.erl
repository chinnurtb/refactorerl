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

-module(refac_manip).
-vsn("$Rev: 1335 $").

-include("refactorerl.hrl").
-include("refac_schema.hrl").

-export([create_include_form/2,
	 create_import_form/2,
	 add_import/3,
	 remove_import/2,
	 remove_import/1,
	 rename_import/2,
	 create_export_form/1,
	 add_export/3,
	 remove_export/3,
	 remove_export/2,
	 remove_export/1,
	 insert_module_qualifier/2,
	 remove_module_qualifier/1,
	 update_module_qualifier/2,
	 move_form/3,
	 move_include/3,
	 copy_include/2,
	 create_and_add_export/3,
	 create_and_add_import/4,
	 insert_form/2,
	 insert_application_again/1,
	 kill_token/1]).


%%% ===========================================================================
%%                            INCLUDE FORMS
%%% ===========================================================================


create_include_form(File, Path) ->
    AlreadyIncluded = ?QUERY:is_included(File, Path),
    if AlreadyIncluded ->
	    ok;
       true ->
	    Form = ?ESG:create(#form{type = include, tag = Path}),
	    insert_form(File, Form),
	    FileName = filename:basename(Path),
	    TokenInfo = [{minus,    "-",       ""},
			 {atom,     "include", ""},
			 {op_paren, "(",       ""},
			 {string,   FileName,  ""},
			 {cl_paren, ")",       ""},
			 {stop,     ".",       "\n"}],
	    TokenNodes = [create_lexical_token(Type, Text, "", Postws) 
			  || {Type, Text, Postws} <- TokenInfo],
	    create_next_links(TokenNodes),
	    ?GRAPH:mklink(Form, ffirst, lists:nth(1, TokenNodes)),
	    ?GRAPH:mklink(Form, flast, lists:nth(6, TokenNodes)),
	    ok
    end.


%%% ===========================================================================
%%                       EXPORT AND IMPORT LISTS
%%% ===========================================================================


%% ----------------------------------------------------------------------------
%% @spec create_import_form(node(), node()) -> node()
%% @doc Creates a function-import list in 'File', returns the empty list.
%% <pre>
%% The imported module name is the name of 'Mod' and the list will be empty.
%% Example:
%%   -import(modname, []).
%% </pre>
%% @end
%% ----------------------------------------------------------------------------
create_import_form(File, Mod) ->
    #module{name = MName} = ?GRAPH:data(Mod),
    Form = ?ESG:create(#form{type = attrib, tag = import}),
    {MNameExpr, MNameToken} = create_expr_and_token(atom, MName),
    ListExpr = ?ESG:create(#expr{kind = list}),
    ?ESG:insert(Form, {attr, 1}, MNameExpr),
    ?ESG:insert(Form, {attr, 2}, ListExpr),
    insert_form(File, Form),
    
    TokenInfo = [{form, minus,      "-",      ""},
 		 {form, atom,       "import", ""},
 		 {form, op_paren,   "(",      ""},
 		 {form, comma,      ",",      ""},
 		 {list, op_bracket, "[",      ""},
 		 {list, cl_bracket, "]",      ""},
 		 {form, cl_paren,   ")",      ""},
 		 {form, stop,       ".",      "\n"}],
    Tokens = [{ParentType, create_lexical_token(Type, Text, "", Postws)}
	      || {ParentType, Type, Text, Postws} <- TokenInfo],
    lists:foreach(fun({form, TN}) -> ?GRAPH:mklink(Form, flex, TN);
 		     ({list, TN}) -> ?GRAPH:mklink(ListExpr, elex, TN)
 		  end, Tokens),
    
    {_, TokenNodes} = lists:unzip(Tokens),
    [T1, T2, T3 | Tail] = TokenNodes,
    create_next_links([T1, T2, T3] ++ [MNameToken] ++ Tail),
    ?GRAPH:mklink(Form,     ffirst, lists:nth(1, TokenNodes)),
    ?GRAPH:mklink(Form,      flast, lists:nth(8, TokenNodes)),
    ?GRAPH:mklink(ListExpr, efirst, lists:nth(5, TokenNodes)),
    ?GRAPH:mklink(ListExpr,  elast, lists:nth(6, TokenNodes)),
    ListExpr.


%% ----------------------------------------------------------------------------
%% @spec add_import(node(), string(), integer()) -> ok
%% @doc Adds the name and arity as arity-qualifier to the given import list.
%% @end
%% ----------------------------------------------------------------------------
add_import(Import, Name, Arity) ->
    add_expimp(Import, Name, Arity).


%% ----------------------------------------------------------------------------
%% @spec remove_import(node(), node()) -> ok
%% @doc Removes the arity-qualifier expression (an item in function import list)
%% from the import list.
%% @end
%% ----------------------------------------------------------------------------
remove_import(ImportList, ImportExpr) ->
    remove_expimp(ImportList, ImportExpr).


%% ----------------------------------------------------------------------------
%% @spec remove_import(node()) -> ok
%% @doc Removes the whole import list.
%% @end
%% ----------------------------------------------------------------------------
remove_import(ImportForm) ->
    [File] = ?GRAPH:path(ImportForm, [{form, back}]),
    ?ESG:remove(File, form, ImportForm),
    ok.


%% ----------------------------------------------------------------------------
%% @spec rename_import(node(), node()) -> ok
%% @doc Renames the module name in function-import list.
%% <pre>
%% (The new name is the name of 'Mod'.)
%% Example:
%% -import(mod_a). --> -import(mod_b).
%% -import(mod_a, [...]). --> -import(mod_b, [...]).
%% </pre>
%% @end
%% ----------------------------------------------------------------------------
rename_import(ImportForm, Mod) ->
    #module{name = NewModuleName} = ?GRAPH:data(Mod),
    [ModuleNameExpr] = ?GRAPH:path(ImportForm, [{attr, 1}]),
    [ModuleNameToken] = ?GRAPH:path(ModuleNameExpr, [efirst]),

    NewModuleNameExprData = (?GRAPH:data(ModuleNameExpr))#expr{value=NewModuleName},
    ModuleNameTokenData = ?GRAPH:data(ModuleNameToken),
    ModuleNameTokenDataD = (?GRAPH:data(ModuleNameToken))#lex.data,
    NewModuleNameTokenDataD = ModuleNameTokenDataD#token{
				value = NewModuleName,
				text  = atom_to_list(NewModuleName)},
    NewModuleNameTokenData = ModuleNameTokenData#lex{data=NewModuleNameTokenDataD},
    ?GRAPH:update(ModuleNameExpr, NewModuleNameExprData),
    ?GRAPH:update(ModuleNameToken, NewModuleNameTokenData),
    insert_form_again(ImportForm),
    ok.


%% ----------------------------------------------------------------------------
%% @spec create_export_form(node()) -> ok
%% @doc Creates an empty export list in 'File'.
%% @end
%% ----------------------------------------------------------------------------
create_export_form(File) ->
    Form = ?ESG:create(#form{type=attrib, tag=export}),
    ListExpr = ?ESG:create(#expr{kind=list}),
    insert_form(File, Form),
    ?ESG:insert(Form, attr, ListExpr),

    TokenInfo = [{form, minus,      "-",      ""},
		 {form, atom,       "export", ""},
		 {form, op_paren,   "(",      ""},
		 {list, op_bracket, "[",      ""},
		 {list, cl_bracket, "]",      ""},
		 {form, cl_paren,   ")",      ""},
		 {form, stop,       ".",      "\n"}],
    Tokens = [{ParentType, create_lexical_token(Type, Text, "", Postws)}
	      || {ParentType, Type, Text, Postws} <- TokenInfo],
    {_, TokenNodes} = lists:unzip(Tokens),
    
    lists:foreach(fun({form, TN}) -> ?GRAPH:mklink(Form, flex, TN);
		     ({list, TN}) -> ?GRAPH:mklink(ListExpr, elex, TN)
		  end, Tokens),
    create_next_links(TokenNodes),

    ?GRAPH:mklink(Form,     ffirst, lists:nth(1, TokenNodes)),
    ?GRAPH:mklink(Form,      flast, lists:nth(7, TokenNodes)),
    ?GRAPH:mklink(ListExpr, efirst, lists:nth(4, TokenNodes)),
    ?GRAPH:mklink(ListExpr,  elast, lists:nth(5, TokenNodes)),
    ListExpr.


%% ----------------------------------------------------------------------------
%% @spec add_export(node(), string(), integer()) -> ok
%% @doc Adds the name and arity as arity-qualifier to the given export list.
%% @end
%% ----------------------------------------------------------------------------
add_export(Export, Name, Arity) ->
    add_expimp(Export, Name, Arity).


%% ----------------------------------------------------------------------------
%% @spec remove_export(node(), node()) -> ok
%% @doc Removes the arity-qualifier expression (an item of the export list)
%% from 'ExportList'.
%% @end
%% ----------------------------------------------------------------------------
remove_export(ExportList, ExportExpr) ->
    remove_expimp(ExportList, ExportExpr).


%% ----------------------------------------------------------------------------
%% @spec remove_export(node()) -> ok
%% @doc Removes the arity-qualifier expression (an item of the export list)
%% from the containing export list.
%% @end
%% ----------------------------------------------------------------------------
remove_export(ExportExpr) ->
    remove_export(hd(?GRAPH:path(ExportExpr, [sup])), ExportExpr).


%% ----------------------------------------------------------------------------
%% @spec remove_export(node(), string(), integer()) -> ok
%% @doc Removes the name and arity as arity-qualifier from 'ExportList'.
%% @end
%% ----------------------------------------------------------------------------
remove_export(ExportList, Name, Arity) ->
    remove_export(
      ExportList, 
      hd([Sub || Sub <- ?GRAPH:path(ExportList, [sub]),
		 [] /= ([First] = ?GRAPH:path(Sub, [efirst])),
		 [] /= ([Last]  = ?GRAPH:path(Sub, [elast] )),
		 Name  == ((?GRAPH:data(First))#lex.data)#token.value,
		 Arity == ((?GRAPH:data(Last))#lex.data)#token.value])).


%% ----------------------------------------------------------------------------
%% @spec add_expimp(node(), string(), integer()) -> ok
%% @doc Adds the name and arity as arity-qualifier to 'List'.
%% (The list node can be import and export list.)
%% @end
%% ----------------------------------------------------------------------------
add_expimp(List, Name, Arity) ->
    Subs        = ?GRAPH:path(List, [sub]),
    [OpBracket] = ?GRAPH:path(List, [efirst]),
    [ClBracket] = ?GRAPH:path(List, [elast]),
    case length(Subs) of
	0 ->
	    ?GRAPH:rmlink(OpBracket, next, ClBracket),
	    add_expimp_to_list(List, Name, Arity, OpBracket, ClBracket);
	_ ->
	    [Prev] = ?GRAPH:path(List, [elast, {next, back}]),
	    Commas = ?GRAPH:path(List, [elex]),
	    Comma = create_lexical_token(comma, ",", "", " "),
	    ?GRAPH:mklink(Prev, next, Comma),
	    ?GRAPH:rmlink(Prev, next, ClBracket),
	    ?GRAPH:mklink(List, {elex, length(Commas) - 1}, Comma),
	    add_expimp_to_list(List, Name, Arity, Comma, ClBracket)
    end.


%% ----------------------------------------------------------------------------
%% @spec add_expimp_to_list(node(), string(), integer(), node(), node()) -> ok
%% @doc Adds the name and arity as arity-qualifier to the given list.
%% @end
%% @todo When Expr type is arity_qualifier, graph drawing isn't working.
%%       (anal_function:import_function makes some illegal?)
%% ----------------------------------------------------------------------------
add_expimp_to_list(List, Name, Arity, Prev, Next) ->
    %%ArityAsAtom = list_to_atom(integer_to_list(Arity)),
    {NameExpr, NameAtom}   = create_expr_and_token(atom, Name),
    {ArityExpr, ArityAtom} = create_expr_and_token(integer, Arity),
    Expr = ?ESG:create(#expr{kind = arity_qualifier}),
    SAtom = create_lexical_token(slash, "/", "", ""),
    ?ESG:insert(Expr, sub, NameExpr),
    ?ESG:insert(Expr, sub, ArityExpr),
    ?ESG:insert(List, sub, Expr),
    ?GRAPH:mklink(Expr, efirst, NameAtom),
    ?GRAPH:mklink(Expr, elex, SAtom),
    ?GRAPH:mklink(Expr, elast, ArityAtom),
    ?GRAPH:mklink(Prev, next, NameAtom),
    ?GRAPH:mklink(NameAtom, next, SAtom),
    ?GRAPH:mklink(SAtom, next, ArityAtom),
    ?GRAPH:mklink(ArityAtom, next, Next),
    ok.


%% ----------------------------------------------------------------------------
%% @spec remove_expimp(node(), node()) -> ok
%% @doc Removes the name and arity as arity-qualifier from 'List'.
%% (The list node can be import and export list.)
%% @end
%% ----------------------------------------------------------------------------
remove_expimp(List, Expr) ->
    [First] = ?GRAPH:path(Expr, [efirst]),
    [Last]  = ?GRAPH:path(Expr, [elast]),
    [Prev]  = ?GRAPH:path(First, [{next, back}]),
    [PPrev] = ?GRAPH:path(Prev,  [{next, back}]),
    [Next]  = ?GRAPH:path(Last,  [next]),
    [NNext] = ?GRAPH:path(Next,  [next]),
    {RealPrev, RealNext} =
	case {?GRAPH:data(Prev), ?GRAPH:data(Next)} of
	    { #lex{data=#token{type=op_bracket}},
	      #lex{data=#token{type=cl_bracket}}} ->
		{Prev, Next};
	    {_, #lex{data=#token{type=cl_bracket}}} ->
		?GRAPH:rmlink(PPrev, next, Prev),
		?GRAPH:rmlink(List, elex, Prev),
		{PPrev, Next};
	    _ ->
		?GRAPH:rmlink(Next, next, NNext),
		?GRAPH:rmlink(List, elex, Next),
		{Prev, NNext}
	end,
    ?ESG:remove(List, sub, Expr),
    ?GRAPH:rmlink(Prev, next, First),
    ?GRAPH:rmlink(Last, next, Next),
    ?GRAPH:mklink(RealPrev, next, RealNext),
    ok.


%% ----------------------------------------------------------------------------
%% @spec create_and_add_export(node(), atom(), integer()) -> ok
%% @doc When the name-arity pair isn't exported in 'File' yet, than creates an
%% empty export list in 'File' and adds the name-arity pair to this new list.
%% @end
%% ----------------------------------------------------------------------------
create_and_add_export(File, Name, Arity) ->
    AlreadyExported = ?QUERY:is_exported(File, Name, Arity),
    if AlreadyExported ->
	    ok;
       true ->
	    NewExportList = create_export_form(File),
	    add_export(NewExportList, Name, Arity)
    end.


%% ----------------------------------------------------------------------------
%% @spec create_and_add_import(node(), node(), atom(), integer()) -> ok
%% @doc When the name-arity pair isn't imported from 'Module' in 'File' yet,
%% then creates an empty import list in 'File' to importing functions
%% from 'Module', and than adds the name-arity pair to this new list.
%% @end
%% ----------------------------------------------------------------------------
create_and_add_import(File, Module, Name, Arity) ->
    ModuleName = (?GRAPH:data(Module))#module.name,
    AlreadyImported = ?QUERY:is_imported(File, ModuleName, Name, Arity),
    if AlreadyImported ->
	    ok;
       true ->
	    NewImportList = create_import_form(File, Module),
	    add_import(NewImportList, Name, Arity)
    end.


%%% ===========================================================================
%%                           MODULE QUALIFIERS
%%% ===========================================================================


%% ----------------------------------------------------------------------------
%% @spec insert_module_qualifier(node(), node()) -> ok
%% @doc Inserts the name of 'ModuleNode' as module-qualifier before 'FunRef'.
%% @end
%% ----------------------------------------------------------------------------
insert_module_qualifier(ModuleNode, FunRef) ->
    try
	[NameNode] = ?GRAPH:path(FunRef, [{sub, 1}]),
	NameToken = ?QUERY:first_token(NameNode),
	?ESG:remove(FunRef,    sub, NameNode), 
	
	#module{name = MName} = ?GRAPH:data(ModuleNode),
	[FunRefFirstLex] = ?GRAPH:path(FunRef, [efirst]),
	FunRefFirstToken = ?QUERY:first_token(FunRef),
	[PrevToken] = ?GRAPH:path(FunRefFirstToken, [{next, back}]),
	
	MQualNode = ?ESG:create(#expr{kind = module_qualifier}),
	MQualColon = create_lexical_token(colon, ":", "", ""),
	{MNameNode, MNameToken} = create_expr_and_token(atom, MName),
	move_prews(NameToken, MNameToken),
	
	?ESG:insert(FunRef,    {sub, 1}, MQualNode),
	?ESG:insert(MQualNode, {sub, 1}, MNameNode), 
	?ESG:insert(MQualNode, {sub, 2}, NameNode),
	
	?GRAPH:rmlink(FunRef, efirst, FunRefFirstLex),
	?GRAPH:mklink(FunRef, efirst, MNameToken),
	
	?GRAPH:mklink(MQualNode, efirst, MNameToken),
	?GRAPH:mklink(MQualNode, elex,   MQualColon),
	?GRAPH:mklink(MQualNode, elast,  FunRefFirstToken),
	
	?GRAPH:rmlink(PrevToken,  next, FunRefFirstToken),
	?GRAPH:mklink(PrevToken,  next, MNameToken),
	?GRAPH:mklink(MNameToken, next, MQualColon),
	?GRAPH:mklink(MQualColon, next, FunRefFirstToken),
	ok
    catch throw:not_supported ->
	    io:format("insert_module_qualifier: " ++ 
		      "can't get the name node's first token. (Maybe it is from include.)")
    end.


%% ----------------------------------------------------------------------------
%% @spec remove_module_qualifier(node()) -> ok
%% @doc Removes the module-qualifier from before 'Funref'.
%% @end
%% ----------------------------------------------------------------------------
remove_module_qualifier(Funref) ->
    try
	[ModuleQualNode] = ?GRAPH:path(Funref, [{sub, {kind, '==', module_qualifier}}]),
	[ModuleNameExpr, FunNameExpr] = ?GRAPH:path(ModuleQualNode, [sub]),
	
	FirstToken = ?QUERY:first_token(ModuleQualNode),
	[EFirst] = ?GRAPH:path(ModuleQualNode, [efirst]),
	[Prev] = ?GRAPH:path(FirstToken, [{next, back}]),
	FunNameToken = ?QUERY:first_token(FunNameExpr),

	[Colon] = ?GRAPH:path(FunNameToken, [{next, back}]),
	
	copy_prews(FirstToken, FunNameToken),
	
	?ESG:remove(ModuleQualNode, sub, ModuleNameExpr),
	?ESG:remove(ModuleQualNode, sub, FunNameExpr),
	?ESG:remove(Funref,         sub, ModuleQualNode),
	
	kill_token(FirstToken),
	kill_token(EFirst),
	kill_token(Colon),
	
	?ESG:insert(Funref, {sub, 1}, FunNameExpr),
	?GRAPH:mklink(Prev, next, FunNameToken),
	?GRAPH:mklink(Funref, efirst, FunNameToken),
	?ESG:close(), %% needed, because insert uses this modifications
	ok
    catch throw:not_supported ->
	    io:format("remove_module_qualifier: " ++ 
		      "can't get first token. (Maybe it is from include.)")
    end.


%% ----------------------------------------------------------------------------
%% @spec update_module_qualifier(node(), node()) -> ok
%% @doc Updates the module-qualifier of 'Funref'.
%% The new qualifier will be the name of 'ModuleNode'.
%% @end
%% ----------------------------------------------------------------------------
update_module_qualifier(Funref, ModuleNode) ->
    remove_module_qualifier(Funref),
    insert_module_qualifier(ModuleNode, Funref),
    ok.


%%% ===========================================================================
%%                             LEXICAL LAYER
%%% ===========================================================================

%% @private
create_lexical_token(Type, Text, Prews, Postws) ->
    case Type of
	string ->
	    ?GRAPH:create(#lex{type = token,
			       data = #token {
				 type   = Type,
				 text   = "\"" ++ Text ++ "\"",
				 value  = Text,
				 prews  = Prews,
				 postws = Postws}});
	_ ->
	    ?GRAPH:create(#lex{type = token,
			       data = #token {
				 type   = Type,
				 text   = Text,
				 value  = list_to_atom(Text),
				 prews  = Prews,
				 postws = Postws}})
    end.


%% @private
create_expr_and_token(Type, Name) when is_atom(Name) ->
    create_expr_and_token(Type, atom_to_list(Name), Name);

create_expr_and_token(Type, Name) when is_integer(Name) ->
    create_expr_and_token(Type, integer_to_list(Name), Name);

create_expr_and_token(Type, Name) ->
    create_expr_and_token(Type, Name, Name).


%% @private
create_expr_and_token(Type, Text, Value) ->
    ExprNode = 
	case Type of
	    atom ->
		?ESG:create(#expr{kind = atom, value = Value});
	    integer ->
		?ESG:create(#expr{kind = integer, value = Value})
	end,
    TokenNode = ?GRAPH:create(#lex{type = token,
				   data = #token{type  = Type,
						 text  = Text,
						 value = Value}}),
    ?ESG:insert(ExprNode, efirst, TokenNode),
    ?ESG:insert(ExprNode,  elast, TokenNode),
    ?ESG:insert(ExprNode,   elex, TokenNode),
    {ExprNode, TokenNode}.


%% @private
create_next_links(TokenList) ->
    [?GRAPH:mklink(N1, next, N2)
     || {N1, N2} <- lists:zip(TokenList, tl(TokenList) ++ [no_link]), 
	N2 /= no_link ],
    ok.

%% @private
copy_prews(From, To) ->
    FData = ?GRAPH:data(From),
    FDataD = FData#lex.data,
    Ws = FDataD#token.prews,
    TData = ?GRAPH:data(To),
    TDataD = TData#lex.data,
    NewData = TDataD#token{prews = Ws},
    ?GRAPH:update(To, TData#lex{data = NewData}).


%% @private
move_prews(From, To) ->
    FData = ?GRAPH:data(From),
    FDataD = FData#lex.data,
    Ws = FDataD#token.prews,
    TData = ?GRAPH:data(To),
    TDataD = TData#lex.data,
    NewFData = FDataD#token{prews = ""},
    NewTData = TDataD#token{prews = Ws},
    ?GRAPH:update(From, FData#lex{data=NewFData}),
    ?GRAPH:update(To, TData#lex{data = NewTData}).


kill_token(Token) ->
    case ?GRAPH:path(Token, [{next, back}]) of
	[Prev] ->
	    ?GRAPH:rmlink(Prev, next, Token);
	[] ->
	    none
    end,
    case ?GRAPH:path(Token, [next]) of
	[Next] ->
	    ?GRAPH:rmlink(Token, next, Next);
	[] ->
	    none
    end,

    Connections1 = [Connections || {_, _, Connections} <- ?LEXICAL_SCHEMA],
    Connections2 = [Connections || {_, Connections}    <- ?LEXICAL_SCHEMA],
    lists:foreach(
      fun(Link) ->
	      [?GRAPH:rmlink(Parent, Link, Token) 
	       || Parent <- ?GRAPH:path(Token, [{Link, back}])]
      end,
      [Link || {Link, lex} <- lists:append(Connections1 ++ Connections2), Link /= next]),
    ok.


%%% ===========================================================================
%%                                 MOVINGS
%%% ===========================================================================


%% ----------------------------------------------------------------------------
%% @spec move_form(node(), node(), node()) -> ok
%% @doc Moves the form between files.
%% @end
%% ----------------------------------------------------------------------------
move_form(Form, From, To) ->
    ?ESG:remove(From, form, Form),
    insert_form(To, Form),
    ok.

move_include(Form, From, To) ->
    ?ESG:remove(From, form, Form),
    Path = (?GRAPH:data(Form))#form.tag,
    AlreadyIncluded = ?QUERY:is_included(To, Path),
    if AlreadyIncluded ->
	    ok;
       true ->
	    insert_form(To, Form)
    end,
    ok.


copy_include(Form, To) ->
    Path = (?GRAPH:data(Form))#form.tag,
    AlreadyIncluded = ?QUERY:is_included(To, Path),
    if AlreadyIncluded ->
	    ok;
       true ->
	    insert_form(To, Form)
    end,
    ok.


%%% ===========================================================================
%%                            INSERT FUNCTIONS
%%% ===========================================================================


%% @private
insert_application_again(Funref) ->
    {Parent, Link, Index} = funref_parent_link(Funref),
    ?ESG:remove(Parent, Link, Funref),
    ?ESG:insert(Parent, {Link, Index}, Funref).


%% @private
insert_form_again(Form) ->
    [Parent] = ?GRAPH:path(Form, [{form, back}]),
    Index = ?QUERY:link_index(Parent, form, Form),
    ?ESG:remove(Parent, form, Form),
    ?ESG:insert(Parent, {form, Index}, Form).
    

%% @private
funref_parent_link(FunRef) ->
    ParentLinks =
	[ {?GRAPH:path(FunRef, [{Link, back}]), Link}
          || {_From, _, Links} <- ?SYNTAX_SCHEMA,
	     {Link, To} <- Links,
	     To == expr],
    [{[Parent], Link}] =
	lists:filter(fun({Parent,_}) -> Parent /= [] end, ParentLinks),
    Index = ?QUERY:link_index(Parent, Link, FunRef),
    {Parent, Link, Index}.


%% @private
antecedents(export) ->
    [export];
antecedents(import) ->
    [export, import];
antecedents(macro) ->
    [import, export, macro];
antecedents(record) ->
    antecedents(macro) ++ [record];
antecedents(include) ->
    antecedents(record) ++ [include].


%% @private
insert_pos(File, Antecedents) ->
    [_Module|Forms] = ?GRAPH:path(File, [form]),
    case Antecedents of
	[] ->
	    2;
	_ ->
	    pos(Forms, 2, Antecedents)
    end.


%% @private
pos([], Index, _) ->
    Index;
pos([H|T], Index, Antecedents) ->
    #form{type=Type, tag=Tag} = ?GRAPH:data(H),
    case Type of
	define ->
	    MyType = macro;
	attrib ->
	    MyType = Tag;
	_ ->
	    MyType = Type
    end,
    Continue = lists:member(MyType, Antecedents),
    if Continue ->
	    pos(T, Index+1, Antecedents);
       true ->
	    Index
    end.


insert_form(File, Form) ->
    #form{type=Type, tag=Tag} = ?GRAPH:data(Form),
    case Type of
	define ->
	    MyType = macro;
	attrib ->
	    MyType = Tag;
	_ ->
	    MyType = Type
    end,
    case MyType of
	func ->
	    ?ESG:insert(File, form, Form);
	_ ->
	    Pos = insert_pos(File, antecedents(MyType)),
	    ?ESG:insert(File, {form, Pos}, Form)
    end,
    ?ESG:close(). %% needed by form insert positions
