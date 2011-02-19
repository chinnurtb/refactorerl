%%% Copyright Notice © 2007 Eötvös Loránd University and Ericsson Hungary
%%% Software development supported by Ericsson Hungary and
%%% GVOP-3.2.2-2004-07-0005/3.0 ELTE IKKK.

%%% Materials  were  created, authored,  and/or  prepared  by  the Authors  at
%%% Department   of  Programming  Languages   and  Compilers,   Eötvös  Loránd
%%% University,  Budapest,  Hungary  (ELTE)  for Ericsson  Hungary  under  the
%%% agreement  between  Ericsson  Hungary  and  ELTE  IKKK.  Unless  otherwise
%%% specifically stated, no claim to copyright is being asserted and it may be
%%% freely  used as  in the  public domain  in accordance  with  Erlang Public
%%% License.  All rights,  including copyright,  are owned  or  controlled for
%%% these purposes by  the Ericsson Hungary and ELTE.  Copyright exists in all
%%% other original  material published on the  internet and may  belong to the
%%% authors depending on the circumstances of publication.

%%% --------------------------------------------------------------------------
%%% ``The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1,  (the  "License"); you  may  not  use  this file  except  in
%%% compliance with the License. You should have received a copy of the Erlang
%%% Public License along  with this software. If not, it  can be retrieved via
%%% the world wide web at http://www.erlang.org/.

%%% Software distributed under the License is distributed on an "AS IS" basis,
%%% WITHOUT WARRANTY OF  ANY KIND, either express or  implied. See the License
%%% for  the specific  language  governing rights  and  limitations under  the
%%% License.

%%% The Initial  Developer of  the Original Code  is Ericsson  Utvecklings AB.
%%% Portions created by Ericsson  are Copyright 1999, Ericsson Utvecklings AB.
%%% All Rights Reserved.''
%%% --------------------------------------------------------------------------

%%% The Contributors are the Authors listed below. All Rights Reserved.

%%% You may not alter or remove any trademark, copyright or other notice from
%%% copies of the content.

%%% Authors: Zoltán Csörnyei
%%%          Zoltán Horváth
%%%          Roland Király
%%%          Róbert Kitlei
%%%          Tamás Kozsik
%%%          László Lövei
%%%          Tamás Nagy
%%%          Melinda Tóth
%%%          Anikó Víg

%%% Author contact: erlang@plc.inf.elte.hu
%%% --------------------------------------------------------------------------

%% @copyright 2007 Eötvös Loránd University and Ericsson Hungary
%% @author Tamas Nagy <lestat@elte.hu>
%% @author Aniko Vig <viganiko@inf.elte.hu>

%% @doc 
%% This module creates the AST from the modules in the database.
%% There are to different methods to do this. 
%% For the first method you need the file path, and for the second 
%% the module id. The second one is normally used in refactorings, 
%% when the module ids are available.
%% @end

-module(out_from_db).

-vsn('0.1').

-export([create_code/2,create_code/1]).

%% =====================================================================
%% @spec create_code(File::string(), OutFile::string()) -> 
%%                         {ok,Location::string()}
%%
%% @doc
%% Creates the module from the data strored in the database.
%% The output location can be different from the original location.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The original path of the module.
%% <b>OutFile</b> : The output path. Where the module should be created 
%% </pre>
%% @end
%% =====================================================================
create_code(File, OutFile) ->
    MId = refactor_db:select(
	    "select mid from module where path=" 
	    ++ io_lib:write_string(File) ++";"),
    case (MId == []) of
	true ->
	    {not_exists,File};
	false ->
	    [{FormLId}]= 
		refactor_db:select(
		  "select formlid from id_count where mid=" 
		  ++ integer_to_list(element(1,hd(MId))) ++ ";" ),
	    create_file(build_tree(element(1,hd(MId)), FormLId), OutFile)
    end.

%% =====================================================================
%% @spec create_code(MId::integer()) -> {ok, Location::string()}
%%
%% @doc
%% Creates the module from the data strored in the database.
%% The output location can not be different from the original location.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. </pre>
%% @end
%% =====================================================================
create_code(MId) ->    
    [{FormLId}]= refactor_db:select(
		   "select formlid from id_count where mid=" 
		   ++ integer_to_list(MId) ++ ";" ),
    [{OutFile}] = refactor_db:select(
		    "select path from module where mid="
		    ++ integer_to_list(MId) ++ ";"), 
    create_file(build_tree(MId, FormLId),OutFile).

%% =====================================================================
%% @type syntaxTree(). See the syntax tools tool for further help.
%% @spec build_tree(MId::integer(), 
%%            FormLId::integer()) -> syntaxTree()
%%
%% @doc
%% Creates the module's AST from the data stored in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module. 
%% <b>FormLId</b> : The root node's id of the module's AST. </pre>
%% @end
%% =====================================================================
build_tree(MId, FormLId) ->
    Elements = refactor_db:select(
		 "select * from form_list where mid=" ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(FormLId) ++" order by pos;"),
    Trees = 
	lists:map(
	  fun(Element)->
		  build_element(MId, element(4,Element))end,Elements),
    erl_syntax:form_list(Trees).

%% =====================================================================
%% @spec create_file(Tree::syntaxTree(), 
%%            OutFile::string()) -> {ok, Location::string()}
%%
%% @doc
%% Creates the module from the AST.
%% 
%% Parameter description:<pre>
%% <b>Tree</b> : The module's AST.
%% <b>OutFile</b> : The destination file path. </pre>
%% @end
%% =====================================================================
create_file(Tree, OutFile) ->
    {_,Io}=file:open(OutFile,[write]),
    io:put_chars(Io,erl_prettypr:format(Tree)),
    file:close(Io),
    {ok,OutFile}.

%% =====================================================================
%% @spec build_element(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates syntax tree node from the data stored in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
build_element(MId, Id) ->
    case erl_syntax_db:type(MId,Id) of
	1 ->
	    give_pp_comments(MId, Id, make_application(MId, Id));
	2 ->
	    give_pp_comments(MId, Id, make_arity_qualifier(MId, Id));
	3 ->
	    give_pp_comments(MId, Id, make_atom(MId, Id));
	4 ->
	    give_pp_comments(MId, Id, make_attribute(MId, Id));
	5 ->
	    give_pp_comments(MId, Id, make_binary(MId, Id));
	6 ->
	    give_pp_comments(MId, Id, make_binary_field(MId, Id));
	7 ->
	    give_pp_comments(MId, Id, make_block_expr(MId, Id));
	8 ->
	    give_pp_comments(MId, Id, make_case_expr(MId, Id));
	9 ->
	    give_pp_comments(MId, Id, make_cath_expr(MId, Id));
	10 ->
	    give_pp_comments(MId, Id, make_char(MId, Id));
	11 ->
	    give_pp_comments(MId, Id, make_class_qualifier(MId, Id));
	12 ->
	    give_pp_comments(MId, Id, make_clause(MId, Id));
	13 ->
	    give_pp_comments(MId, Id, make_comment(MId, Id));
	14 ->
	    give_pp_comments(MId, Id, make_cond_expr(MId, Id));
	15 ->
	    give_pp_comments(MId, Id, make_conjunction(MId, Id));
	16 ->
	    give_pp_comments(MId, Id, make_disjunction(MId, Id));
	17 ->
	    give_pp_comments(MId, Id, make_eof_marker(MId, Id));
	18 ->
	    give_pp_comments(MId, Id, make_float(MId, Id));
	19 ->
	    give_pp_comments(MId, Id, make_form_list(MId, Id));
	20 ->
	    give_pp_comments(MId, Id, make_fun_expr(MId, Id));
	21 ->
	    give_pp_comments(MId, Id, make_function(MId, Id));
	22 ->
	    give_pp_comments(MId, Id, make_generator(MId, Id));
	23 ->
	    give_pp_comments(MId, Id, make_if_expr(MId, Id));
	24 ->
	    give_pp_comments(MId, Id, make_implicit_fun(MId, Id));
	25 ->
	    give_pp_comments(MId, Id, make_infix_expr(MId, Id));
	26 ->
	    give_pp_comments(MId, Id, make_integer(MId, Id));
	27 ->
	    give_pp_comments(MId, Id, make_list(MId, Id));
	28 ->
	    give_pp_comments(MId, Id, make_list_comp(MId, Id));
	29 ->
	    give_pp_comments(MId, Id, make_macro(MId, Id));
	30 ->
	    give_pp_comments(MId, Id, make_match_expr(MId, Id));
	31 ->
	    give_pp_comments(MId, Id, make_module_qualifier(MId, Id));
	32 ->
	    give_pp_comments(MId, Id, make_nil(MId, Id));
	33 ->
	    give_pp_comments(MId, Id, make_operator(MId, Id));
	34 ->
	    give_pp_comments(MId, Id, make_parentheses(MId, Id));
	35 ->
	    give_pp_comments(MId, Id, make_prefix_expr(MId, Id));
	36 ->
	    give_pp_comments(MId, Id, make_qualified_name(MId, Id));
	37 ->
	    give_pp_comments(MId, Id, make_query_expr(MId, Id));
	38 ->
	    give_pp_comments(MId, Id, make_receive_expr(MId, Id));
	39 ->
	    give_pp_comments(MId, Id, make_record_access(MId, Id));
	40 ->
	    give_pp_comments(MId, Id, make_record_expr(MId, Id));
	41 ->
	    give_pp_comments(MId, Id, make_record_field(MId, Id));
	42 ->
	    give_pp_comments(MId, Id, make_record_index_expr(MId, Id));
	43 ->
	    give_pp_comments(MId, Id, make_rule(MId, Id));
	44 ->
	    give_pp_comments(MId, Id, make_size_qualifier(MId, Id));
	45 ->
	    give_pp_comments(MId, Id, make_string(MId, Id));
	46 ->
	    give_pp_comments(MId, Id, make_text(MId, Id));
	47 ->
	    give_pp_comments(MId, Id, make_try_expr(MId, Id));
	48 ->
	    give_pp_comments(MId, Id, make_tuple(MId, Id));
	49 ->
	    give_pp_comments(MId, Id, make_underscore(MId, Id));
	50 ->
	    give_pp_comments(MId, Id, make_variable(MId, Id))
    end.

%% =====================================================================
%% @spec make_application(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an application syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the application node. </pre>
%% @end
%% =====================================================================
make_application(MId, Id) -> 
    [Operator] = 
	refactor_db:select(
	  "select * from application where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++ " and pos=0;"),
    Attributes =
	refactor_db:select(
	  "select * from application where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++ " and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:application(
	build_element(MId, element(4,Operator)),
	lists:map(
	  fun(Attr) -> build_element(MId, element(4,Attr)) end,Attributes))
      ,Pos). 

%% =====================================================================
%% @spec make_arity_qualifier(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an arity_qualifier syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the arity_qualifier node. </pre>
%% @end
%% =====================================================================
make_arity_qualifier(MId, Id) -> 
    [Qualifier] = 
	refactor_db:select(
	  "select * from arity_qualifier where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:arity_qualifier(
	build_element(MId, element(3,Qualifier)),
	build_element(MId, element(4,Qualifier)))
      ,Pos).

%% =====================================================================
%% @spec make_atom(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an atom syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the atom node. </pre>
%% @end
%% =====================================================================
make_atom(MId, Id) -> 
    [{Name}] = 
	refactor_db:select(
	  "select name from name where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = refactor_db:select(
		"select line from pos where mid=" ++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:atom(Name),Pos).

%% =====================================================================
%% @spec make_attribute(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an attribute syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the attribute node. </pre>
%% @end
%% =====================================================================    
make_attribute(MId, Id) -> 
    [{Name}] = 
	refactor_db:select(
	  "select argument from attribute_ where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;"),
    Arguments = 
	refactor_db:select(
	  "select argument from attribute_ where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;"),
    [{Pos}] = refactor_db:select(
		"select line from pos where mid=" ++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:attribute(
	build_element(MId, Name),
	lists:map(
	  fun({Argument})->
		  build_element(MId, Argument)end,Arguments))
      ,Pos).

%% =====================================================================
%% @spec make_binary(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a binary syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the binary node. </pre>
%% @end
%% =====================================================================
make_binary(MId, Id) -> 
    Fields = 
	refactor_db:select(
	  "select field from binary_ where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:binary(
	lists:map(fun({Field}) -> 
			  build_element(MId, Field) end, Fields))
      ,Pos).

%% =====================================================================
%% @spec make_binary_field(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a binary_field syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the binary_field node. </pre>
%% @end
%% =====================================================================
make_binary_field(MId, Id) -> 
    [{Body}] = 
	refactor_db:select(
	  "select argument from binary_field where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and pos=0;"),
    Types = 
	refactor_db:select(
	  "select argument from binary_field where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:binary_field(
	build_element(MId, Body),
	lists:map(fun({Type})->build_element(MId, Type)end,Types))
      ,Pos).

%% =====================================================================
%% @spec make_block_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a block_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the block_expr node. </pre>
%% @end
%% =====================================================================
make_block_expr(MId, Id) -> 
    Bodys = 
	refactor_db:select(
	  "select body from block_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:block_expr(
	lists:map(fun({Body}) -> 
			  build_element(MId, Body) end, Bodys))
      ,Pos).

%% =====================================================================
%% @spec make_case_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a case_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the case_expr node. </pre>
%% @end
%% =====================================================================
make_case_expr(MId, Id) -> 
    [{Argument}] = 
	refactor_db:select(
	  "select argument from case_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;"),
    Clauses = 
	refactor_db:select(
	  "select argument from case_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:case_expr(
	build_element(MId, Argument),
	lists:map(fun({Clause})->
			  build_element(MId, Clause)end,Clauses))
      , Pos).

%% =====================================================================
%% @spec make_cath_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a cath_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the cath_expr node. </pre>
%% @end
%% =====================================================================
make_cath_expr(MId, Id) -> 
    [{Expr}] = 
	refactor_db:select(
	  "select expression from catch_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) ++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:catch_expr(
	build_element(MId, Expr))
		       , Pos).

%% =====================================================================
%% @spec make_char(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a char syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the char node. </pre>
%% @end
%% =====================================================================
make_char(MId, Id) -> 
    [{Char}] = 
	refactor_db:select(
	  "select value from char_ where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:char(Char), Pos).

%% =====================================================================
%% @spec make_class_qualifier(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a class_qualifier syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the class_qualifier node. </pre>
%% @end
%% =====================================================================
make_class_qualifier(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select class,body from class_qualifier where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:class_qualifier(
	build_element(MId, element(1,Line)),
	build_element(MId, element(2,Line)))
      , Pos).

%% =====================================================================
%% @spec make_clause(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a clause syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause node. </pre>
%% @end
%% =====================================================================
make_clause(MId, Id) -> 
    Patterns = 
	refactor_db:select(
	  "select argument from clause where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++" and qualifier=0"++";"),
    Guard = 
	refactor_db:select(
	  "select argument from clause where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++" and qualifier=1"++";"),    
    Body = 
	refactor_db:select(
	  "select argument from clause where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++" and qualifier=2"++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    case Guard of
	[] ->
	    erl_syntax:set_pos(
	      erl_syntax:clause(
		lists:map(
		  fun(Pattern) -> 
			  build_element(MId, element(1,Pattern)) end,
		  Patterns),
		none,
		lists:map(
		  fun(BodyEl) -> 
			  build_element(MId, element(1,BodyEl)) end, 
		  Body))
	      , Pos);
	_ ->
	    erl_syntax:set_pos(
	      erl_syntax:clause(
		lists:map(
		  fun(Pattern) -> 
			  build_element(MId, element(1,Pattern)) end,
		  Patterns),
		build_element(MId, element(1,hd(Guard))),
		lists:map(
		  fun(BodyEl) -> 
			  build_element(MId, element(1,BodyEl)) end, 
		  Body))
	      , Pos)
    end.

%% =====================================================================
%% @spec make_comment(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a comment syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the comment node. </pre>
%% @end
%% =====================================================================
make_comment(MId, Id) -> 
    Padding = 
	refactor_db:select(
	  "select argument from comment where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;"),
    Comments =
	refactor_db:select(
	  "select argument from comment where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;"),
    [{Pos}] = refactor_db:select(
		"select line from pos where mid=" ++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++";"),
    case Padding of
	[] ->
	    erl_syntax:set_pos(
	      erl_syntax:comment(
		lists:map(fun({Comment})->Comment end,Comments)),Pos);
	_  ->
	    erl_syntax:set_pos(
	      erl_syntax:comment(
		list_to_integer(element(1,hd(Padding))),
		lists:map(fun({Comment})->Comment end,Comments)),Pos)
    end.

%% =====================================================================
%% @spec make_cond_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a cond_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the cond_expr node. </pre>
%% @end
%% =====================================================================
make_cond_expr(MId, Id) -> 
    Clauses = 
	refactor_db:select(
	  "select clause from cond_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:cond_expr(
	lists:map(
	  fun({Clause}) -> build_element(MId, Clause)end,Clauses))
      , Pos).

%% =====================================================================
%% @spec make_conjunction(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a conjunction syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the conjunction node. </pre>
%% @end
%% =====================================================================
make_conjunction(MId, Id) -> 
    Conjunctions = 
	refactor_db:select(
	  "select argument from conjunction where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] =
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:conjunction(
	lists:map(
	  fun({Conjunction}) ->
		  build_element(MId, Conjunction)end, Conjunctions))
      , Pos).

%% =====================================================================
%% @spec make_disjunction(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a disjunction syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the disjunction node. </pre>
%% @end
%% =====================================================================
make_disjunction(MId, Id) -> 
    Disjunctions = 
	refactor_db:select(
	  "select argument from disjunction where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:disjunction(
	lists:map(
	  fun({Disjunction})-> 
		  build_element(MId, Disjunction)end,Disjunctions))
      , Pos).

%% =====================================================================
%% @spec make_eof_marker(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an eof_marker syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the eof_marker node. </pre>
%% @end
%% =====================================================================
make_eof_marker(MId, Id) -> 
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:eof_marker(), Pos).    

%% =====================================================================
%% @spec make_float(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a float syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the float node. </pre>
%% @end
%% =====================================================================
make_float(MId, Id) -> 
    [{Value}] = 
	refactor_db:select(
	  "select value from float_ where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:float(Value), Pos).

%% =====================================================================
%% @spec make_form_list(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a form_list syntax tree node from the data stored 
%% in the database. (Impossible)
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the form_list node. </pre>
%% @end
%% =====================================================================
make_form_list(_MId,_Id) -> 
    io:format("Valami nagyon-nagy gaz van, meg egy form_list!!!",[]). 

%% =====================================================================
%% @spec make_fun_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a fun_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the fun_expr node. </pre>
%% @end
%% =====================================================================
make_fun_expr(MId, Id) -> 
    Clauses = 
	refactor_db:select(
	  "select clause from fun_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:fun_expr(
	lists:map(
	  fun({Clause})->
		  build_element(MId, Clause)end,Clauses))
      , Pos).

%% =====================================================================
%% @spec make_function(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a function syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function node. </pre>
%% @end
%% =====================================================================
make_function(MId, Id) -> 
    [{Name}] = 
	refactor_db:select(
	  "select clause from function where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;"),
    Clauses = 
	refactor_db:select(
	  "select clause from function where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:function(
	build_element(MId, Name),
	lists:map(
	  fun({Clause})->
		  build_element(MId, Clause)end,Clauses))
      , Pos).

%% =====================================================================
%% @spec make_generator(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a generator syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the generator node. </pre>
%% @end
%% =====================================================================
make_generator(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select pattern,body from generator where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:generator(
	build_element(MId, element(1,Line)),
	build_element(MId, element(2,Line)))
      , Pos).

%% =====================================================================
%% @spec make_if_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an if_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the if_expr node. </pre>
%% @end
%% =====================================================================
make_if_expr(MId, Id) -> 
    Clauses = 
	refactor_db:select(
	  "select clause from if_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:if_expr(
	lists:map(
	  fun({Clause})->
		  build_element(MId, Clause)end,Clauses))
      , Pos).

%% =====================================================================
%% @spec make_implicit_fun(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an implicit_fun syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the implicit_fun node. </pre>
%% @end
%% =====================================================================
make_implicit_fun(MId, Id) -> 
    [{Name}] = 
	refactor_db:select(
	  "select name_id from implicit_fun where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:implicit_fun(build_element(MId, Name)), Pos).

%% =====================================================================
%% @spec make_infix_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an infix_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the infix_expr node. </pre>
%% @end
%% =====================================================================
make_infix_expr(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select lft,oper,rght from infix_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:infix_expr(
	build_element(MId, element(1,Line)),
	build_element(MId, element(2,Line)),
	build_element(MId, element(3,Line)))
      , Pos).

%% =====================================================================
%% @spec make_integer(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an integer syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the integer node. </pre>
%% @end
%% =====================================================================
make_integer(MId, Id) -> 
    [{Value}] = 
	refactor_db:select(
	  "select value from integer_ where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:integer(list_to_integer(Value)), Pos).

%% =====================================================================
%% @spec make_list(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a list syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the list node. </pre>
%% @end
%% =====================================================================
make_list(MId, Id) -> 
    Tail = 
	refactor_db:select(
	  "select element from list where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;"),
    Elements = 
	refactor_db:select(
	  "select element from list where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    case Tail of
	[] ->
	    erl_syntax:set_pos(
	      erl_syntax:list(
		lists:map(
		  fun({Element})->
			  build_element(MId, Element)end,Elements))
	      , Pos);
	_  ->
	    erl_syntax:set_pos(
	      erl_syntax:list(
		lists:map(
		  fun({Element})->build_element(MId, Element)end,Elements),
		build_element(MId, element(1,hd(Tail))))
	      , Pos)
    end.

%% =====================================================================
%% @spec make_list_comp(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a list_comp syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the list_comp node. </pre>
%% @end
%% =====================================================================
make_list_comp(MId, Id) -> 
    [{Template}] = 
	refactor_db:select(
	  "select argument from list_comp where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;"),
    Body = 
	refactor_db:select(
	  "select argument from list_comp where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:list_comp(
	build_element(MId, Template),
	lists:map(
	  fun({BodyEl})-> 
		  build_element(MId, BodyEl)end,Body))
      , Pos).

%% =====================================================================
%% @spec make_macro(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a macro syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the macro node. </pre>
%% @end
%% =====================================================================
make_macro(MId, Id) -> 
    [{Name}] = 
	refactor_db:select(
	  "select argument from macro where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;"),
    Arguments = 
	refactor_db:select(
	  "select argument from macro where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    case Arguments of
	[] ->
	    erl_syntax:set_pos(
	      erl_syntax:macro(build_element(MId,Name)), Pos);
	_  ->
	    erl_syntax:set_pos(
	      erl_syntax:macro(
		build_element(MId, Name),
		lists:map(
		  fun({Argument})->
			  build_element(MId, Argument)end,Arguments))
	      , Pos)
    end.

%% =====================================================================
%% @spec make_match_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a match_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the match_expr node. </pre>
%% @end
%% =====================================================================
make_match_expr(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select pattern,body from match_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:match_expr(
	build_element(MId,element(1,Line)),
	build_element(MId,element(2,Line)))
      , Pos).

%% =====================================================================
%% @spec make_module_qualifier(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a module_qualifier syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the module_qualifier node. </pre>
%% @end
%% =====================================================================
make_module_qualifier(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select module,body from module_qualifier where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:module_qualifier(
	build_element(MId, element(1,Line)),
	build_element(MId, element(2,Line)))
      , Pos).

%% =====================================================================
%% @spec make_nil(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a nil syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the nil node. </pre>
%% @end
%% =====================================================================
make_nil(MId, Id) -> 
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:nil(), Pos).

%% =====================================================================
%% @spec make_operator(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates an operator syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the operator node. </pre>
%% @end
%% =====================================================================
make_operator(MId, Id) -> 
    [{Name}] = 
	refactor_db:select(
	  "select name from name where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:operator(Name), Pos).

%% =====================================================================
%% @spec make_parentheses(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a parentheses syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the parentheses node. </pre>
%% @end
%% =====================================================================
make_parentheses(MId, Id) -> 
    [{Body}] = 
	refactor_db:select(
	  "select body from parentheses where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:parentheses(Body), Pos).

%% =====================================================================
%% @spec make_prefix_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a prefix_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the prefix_expr node. </pre>
%% @end
%% =====================================================================
make_prefix_expr(MId, Id) -> 
    [Line] =
	refactor_db:select(
	  "select operator,argument from prefix_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:prefix_expr(
	build_element(MId,element(1,Line)),
	build_element(MId,element(2,Line)))
      , Pos).

%% =====================================================================
%% @spec make_qualified_name(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a qualified_name syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the qualified_name node. </pre>
%% @end
%% =====================================================================
make_qualified_name(MId, Id) -> 
    Segments = 
	refactor_db:select(
	  "select segment from qualified_name where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:qualified_name(
	lists:map(
	  fun({Segment})->
		  build_element(MId, Segment) end, Segments))
      , Pos).

%% =====================================================================
%% @spec make_query_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a query_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the query_expr node. </pre>
%% @end
%% =====================================================================
make_query_expr(MId, Id) -> 
    [{Body}] = 
	refactor_db:select(
	  "select body from query_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:query_expr(Body), Pos).

%% =====================================================================
%% @spec make_receive_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a receive_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the receive_expr node. </pre>
%% @end
%% =====================================================================
make_receive_expr(MId, Id) -> 
    Clauses = 
	refactor_db:select(
	  "select argument from receive_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)
	  ++ " and qualifier=0" ++ ";"),
    Timeout = 
	refactor_db:select(
	  "select argument from receive_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and qualifier=1" ++ ";"),    
    Actions = 
	refactor_db:select(
	  "select argument from receive_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and qualifier=2" ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    case Timeout of
	[] ->
	    erl_syntax:set_pos(
	      erl_syntax:receive_expr(
		lists:map(
		  fun({Clause}) -> 
			  build_element(MId, Clause) end,Clauses),
		none,
		lists:map(
		  fun({Action}) -> 
			  build_element(MId, Action) end, Actions)),
	      Pos);
	_ ->
	    erl_syntax:set_pos(
	      erl_syntax:receive_expr(
		lists:map(
		  fun({Clause}) -> 
			  build_element(MId, Clause) end, Clauses),
		build_element(MId, element(1,hd(Timeout))),
		lists:map(
		  fun({Action}) -> 
			  build_element(MId, Action) end, Actions)),
	      Pos)
    end.

%% =====================================================================
%% @spec make_record_access(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a record_access syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the record_access node. </pre>
%% @end
%% =====================================================================
make_record_access(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select argument,type,field from record_access where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    case element(2,Line) of 
	-1 ->
	    erl_syntax:set_pos(
	      erl_syntax:record_access(
		build_element(MId, element(1,Line)),
		none,
		build_element(MId, element(3,Line))),
	      Pos);
	_  ->
	    erl_syntax:set_pos(
	      erl_syntax:record_access(
		build_element(MId,element(1,Line)),
		build_element(MId,element(2,Line)),
		build_element(MId,element(3,Line))),
	      Pos)
    end.

%% =====================================================================
%% @spec make_record_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a record_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the record_expr node. </pre>
%% @end
%% =====================================================================
make_record_expr(MId, Id) -> 
    Argument = 
	refactor_db:select(
	  "select argument from record_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and pos=-1;"),
    [{Type}] = 
	refactor_db:select(
	  "select argument from record_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and pos=0;"),
    Fields = 
	refactor_db:select(
	  "select argument from record_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and pos!=-1 and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    case Argument of
	[] ->
	    erl_syntax:set_pos(
	      erl_syntax:record_expr(
		none,
		build_element(MId, Type),
		lists:map(
		  fun({Field})->
			  build_element(MId, Field)end,Fields)),
	      Pos);
	_  ->
	    erl_syntax:set_pos(
	      erl_syntax:record_expr(
		build_element(MId, element(1,hd(Argument))),
		build_element(MId, Type),
		lists:map(
		  fun({Field})->
			  build_element(MId, Field) end, Fields)),
	      Pos)
    end.

%% =====================================================================
%% @spec make_record_field(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a record_field syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the record_field node. </pre>
%% @end
%% =====================================================================
make_record_field(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select name_id,value from record_field where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = refactor_db:select(
		"select line from pos where mid=" ++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++";"),
    case element(2,Line) of
	-1 ->
	    erl_syntax:set_pos(
	      erl_syntax:record_field(
		build_element(MId, element(1,Line)),
		none),
	      Pos);
	_  ->
	    erl_syntax:set_pos(
	      erl_syntax:record_field(
		build_element(MId, element(1,Line)),
		build_element(MId, element(2,Line))),
	      Pos)
    end.

%% =====================================================================
%% @spec make_record_index_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a record_index_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the record_index_expr node. </pre>
%% @end
%% =====================================================================
make_record_index_expr(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select type,field from record_index_expr where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:record_index_expr(
	build_element(MId, element(1,Line)),
	build_element(MId, element(2,Line))),
      Pos).

%% =====================================================================
%% @spec make_rule(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a rule syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the rule node. </pre>
%% @end
%% =====================================================================
make_rule(MId, Id) -> 
    [{Name}] = 
	refactor_db:select(
	  "select argument from rule where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;"),
    Clauses = 
	refactor_db:select(
	  "select argument from rule where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:rule(build_element(MId, Name),
		      lists:map(
			fun({Clause})->
				build_element(MId, Clause) end,Clauses))
      , Pos).

%% =====================================================================
%% @spec make_size_qualifier(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a size_qualifier syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the size_qualifier node. </pre>
%% @end
%% =====================================================================
make_size_qualifier(MId, Id) -> 
    [Line] = 
	refactor_db:select(
	  "select body,size from size_qualifier where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:size_qualifier(
	build_element(MId, element(1,Line)),
	build_element(MId, element(2,Line)))
      , Pos).

%% =====================================================================
%% @spec make_string(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a string syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the string node. </pre>
%% @end
%% =====================================================================
make_string(MId, Id) ->
    [{String}] = 
	refactor_db:select(
	  "select value from string where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:string(String), Pos).

%% =====================================================================
%% @spec make_text(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a text syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the text node. </pre>
%% @end
%% =====================================================================
make_text(MId, Id) -> 
    [{Text}] = 
	refactor_db:select(
	  "select value from text where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:text(Text), Pos).

%% =====================================================================
%% @spec make_try_expr(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a try_expr syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the try_expr node. </pre>
%% @end
%% =====================================================================
make_try_expr(MId, Id) -> 
    Body = 
	refactor_db:select(
	  "select argument from try_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=0;"),
    Clauses = 
	refactor_db:select(
	  "select argument from try_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=1;"),
    Handlers = 
	refactor_db:select(
	  "select argument from try_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=2;"),
    After = 
	refactor_db:select(
	  "select argument from try_expr where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=3;"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:try_expr(
	lists:map(
	  fun({BodyEl})->
		  build_element(MId, BodyEl)end, Body),
	lists:map(
	  fun({Clause})->
		  build_element(MId, Clause)end, Clauses),
	lists:map(
	  fun({Handler})->
		  build_element(MId, Handler)end, Handlers),
	lists:map(
	  fun({AfterEl})->
		  build_element(MId, AfterEl)end, After)),
      Pos).

%% =====================================================================
%% @spec make_tuple(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a tuple syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the tuple node. </pre>
%% @end
%% =====================================================================
make_tuple(MId, Id) -> 
    Elements = 
	refactor_db:select(
	  "select element from tuple where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(
      erl_syntax:tuple(
	lists:map(
	  fun({Element})->
		  build_element(MId, Element)end, Elements))
      , Pos).

%% =====================================================================
%% @spec make_underscore(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a underscore syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the underscore node. </pre>
%% @end
%% =====================================================================
make_underscore(MId, Id) -> 
    [{Pos}] = 
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:underscore(), Pos).

%% =====================================================================
%% @spec make_variable(MId::integer(), 
%%            Id::integer()) -> syntaxTree()
%%
%% @doc
%% Creates a variable syntax tree node from the data stored 
%% in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the variable node. </pre>
%% @end
%% =====================================================================
make_variable(MId, Id) ->
    [{Name}] = 
	refactor_db:select(
	  "select name from name where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id)++";"),
    [{Pos}] =
	refactor_db:select(
	  "select line from pos where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++";"),
    erl_syntax:set_pos(erl_syntax:variable(Name), Pos).

%% =====================================================================
%% @spec give_pp_comments(MId::integer(), 
%%            Id::integer(), Node::syntaxTree()) -> syntaxTree()
%%
%% @doc
%% Adds pre- and postcomment to the  syntax tree node 
%% from the data stored in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%% <b>Node</b> : A syntax tree node. </pre>
%% @end
%% =====================================================================   
give_pp_comments(MId, Id,  Node) ->
    Precomm = 
	refactor_db:select(
	  "select distinct pos from precomment where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    Postcomm = 
	refactor_db:select(
	  "select distinct pos from postcomment where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) ++ ";"),
    Node2= add_precomment(MId, Id, Node, length(Precomm),[]),
    add_postcomment(MId, Id, Node2, length(Postcomm),[]).

%% =====================================================================
%% @spec add_precomment(MId::integer(), 
%%            Id::integer(), Node::syntaxTree(), 
%%           Num::integer(), Comments::[syntaxTree()]) -> syntaxTree()
%%
%% @doc
%% Adds precomment to the syntax tree node 
%% from the data stored in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. 
%% <b>Node</b> : A syntax tree node.
%% <b>Num</b> : Number of the comment nodes left to create.
%% <b>Comments</b> : The already created comment nodes. </pre>
%% @end
%% =====================================================================
add_precomment(_MId, _Id, Node, 0, Comments) ->
    erl_syntax:add_precomments(Comments,Node);
add_precomment(MId, Id, Node, Num, Comments) ->
    Padding = 
	refactor_db:select(
	  "select argument from precomment where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos=" 
	  ++ integer_to_list(Num) ++ " and qualifier= 0;"),
    Commentsl = 
	refactor_db:select(
	  "select argument from precomment where mid=" ++ integer_to_list(MId) 
	  ++ " and id=" ++ integer_to_list(Id) ++ " and pos="
	  ++ integer_to_list(Num) ++ " and qualifier!= 0;"),
    case Padding of
	[] ->
	    Comment = erl_syntax:comment(
			lists:map(
			  fun({Comment})->Comment end,Commentsl));
	_  ->
	    Comment= 
		erl_syntax:comment(
		  list_to_integer(element(1,hd(Padding))),
		  lists:map(
		    fun({Comment})->Comment end,Commentsl))
    end,
    add_precomment(MId, Id, Node, Num-1, [Comment] ++ Comments).

%% =====================================================================
%% @spec add_postcomment(MId::integer(), 
%%            Id::integer(), Node::syntaxTree(), 
%%           Num::integer(), Comments::[syntaxTree()]) -> syntaxTree()
%%
%% @doc
%% Adds postcomment to the syntax tree node 
%% from the data stored in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. 
%% <b>Node</b> : A syntax tree node. 
%% <b>Num</b> : Number of the comment nodes left to create.
%% <b>Comments</b> : The already created comment nodes. </pre>
%% @end
%% =====================================================================
add_postcomment(_MId, _Id, Node, 0, Comments) ->
    erl_syntax:add_postcomments(Comments, Node);
add_postcomment(MId, Id, Node, Num, Comments) ->
    Padding = 
	refactor_db:select(
	  "select argument from postcomment where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and pos=" ++ integer_to_list(Num) ++ " and qualifier= 0;"),
    Commentsl = 
	refactor_db:select(
	  "select argument from postcomment where mid=" 
	  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
	  ++ " and pos=" ++ integer_to_list(Num) ++ " and qualifier!= 0;"),
    case Padding of
	[] ->
	    Comment = 
		erl_syntax:comment(
		  lists:map(fun({Comment})->Comment end, Commentsl));
	_  ->
	    Comment = 
		erl_syntax:comment(
		  list_to_integer(element(1,hd(Padding))),
		  lists:map(fun({Comment})->Comment end, Commentsl))
    end,
    add_postcomment(MId, Id, Node, Num-1, [Comment]++Comments).
