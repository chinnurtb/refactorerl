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

%% @doc The module deletes nodes directly from the database.
%% @end


-module(delete_nodes).

-include("node_type.hrl").

-export([delete_node/2,delete_arity_qualifier/2,
	 delete_atom/2,delete_implicit_fun/2,
	 delete_integer/2,delete_module_qualifier/2, 
         detach_node/3]).


%% =====================================================================
%% @spec delete_node(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current element from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%% </pre>
%% @end
%% Used: 
%% =====================================================================
delete_node(MId,Id) ->
    Type = erl_syntax_db:type(MId, Id),
    delete_line("node_type", MId, Id),
    delete_line("scope", MId, Id),
    delete_line("pos", MId, Id),
    case Type of
	1 ->
	    delete_application(MId,Id);
	2 ->
	    delete_arity_qualifier(MId,Id);
	3 ->
	    delete_atom(MId,Id);
	4 ->
	    delete_attribute(MId,Id);
	5 ->
	    delete_binary(MId,Id);
	6 ->
	    delete_binary_field(MId,Id);
	7 ->
	    delete_block_expr(MId,Id);
	8 ->
	    delete_case_expr(MId,Id); 
	9 ->
	    delete_catch_expr(MId,Id);
	10 ->
	    delete_char(MId,Id);
	11 ->
	    delete_class_qualifier(MId,Id);
	12 ->
	    delete_clause(MId,Id);
	13 ->
	    delete_comment(MId,Id);
	14 ->
	    delete_cond_expr(MId,Id);
	15 ->
	    delete_conjunction(MId,Id);
	16 ->
	    delete_disjunction(MId,Id);
	17 ->
	    delete_eof_marker(MId,Id);
	18 ->
	    delete_float(MId,Id);
	19 ->
	    delete_form_list(MId,Id);
	20 ->
	    delete_fun_expr(MId,Id);
	21 ->
	    delete_function(MId,Id);
	22 ->
	    delete_generator(MId,Id);
	23 ->
	    delete_if_expr(MId,Id);
	24 ->
	    delete_implicit_fun(MId,Id);
	25 ->
	    delete_infix_expr(MId,Id);
	26 ->
	    delete_integer(MId,Id);
	27 ->
	    delete_list(MId,Id);
	28 ->
	    delete_list_comp(MId,Id);
	29 ->
	    delete_macro(MId,Id);
	30 ->
	    delete_match_expr(MId,Id);
	31 ->
	    delete_module_qualifier(MId,Id);
	32 ->
	    delete_nil(MId,Id);
	33 ->
	    delete_operator(MId,Id);
	34 ->
	    delete_parentheses(MId,Id);
	35 ->
	    delete_prefix_expr(MId,Id);
	36 ->
	    delete_qualified_name(MId,Id);
	37 ->
	    delete_query_expr(MId,Id);
	38 ->
	    delete_receive_expr(MId,Id);
	39 ->
	    delete_record_access(MId,Id);
	40 ->
	    delete_record_expr(MId,Id);
	41 ->
	    delete_record_field(MId,Id);
	42 ->
	    delete_record_index_expr(MId,Id);
	43 ->
	    delete_rule(MId,Id);
	44 ->
	    delete_size_qualifier(MId,Id);
	45 ->
	    delete_string(MId,Id);
	46 ->
	    delete_text(MId,Id);
	47 ->
	    delete_try_expr(MId,Id);
	48 ->
	    delete_tuple(MId,Id);
	49 ->
	    delete_underscore(MId,Id);
	50 ->
	    delete_variable(MId,Id);
	_ ->
	    ok
    end.


%% =====================================================================
%% @spec delete_nodes(MId::integer(),
%%          	      Ids::[{integer(),integer()}] | 
%%		           [{integer(),integer(),integer()}] | 
%%			   [integer()]) -> ok
%%
%% @doc
%% Deletes the current elements from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : List of node ids.
%% </pre>
%% @end
%% Used: 
%% =====================================================================

delete_nodes(MId, [{Id1, Id2}]) ->
    delete_node(MId,Id1),
    delete_node(MId,Id2);
delete_nodes(MId, [{Id1, Id2, Id3}]) ->
    delete_node(MId,Id1),
    delete_node(MId,Id2),
    delete_node(MId,Id3);
delete_nodes(MId,List) ->
    lists:map(fun({Id}) -> delete_node(MId,Id) end, List).

%% =====================================================================
%% @spec delete_application(MId::integer(),
%%          	     	    Id::integer()) -> ok
%%
%% @doc
%% Deletes the current application from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the application.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


delete_application(MId,Id) ->
    Elements = get_elements("application", "argument" , MId, Id),
    delete_line("fun_call", MId, Id),
    delete_line("application", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_arity_qualifier(MId::integer(),
%%          	     	    	Id::integer()) -> ok
%%
%% @doc
%% Deletes the current arity qualifier from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the arity qualifier.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_arity_qualifier(MId,Id) ->
    [{Body,Arity}] = refactor_db:select("select body,arity from "
    					"arity_qualifier where "
    					"mid=" ++ integer_to_list(MId) ++ 
    					" and id=" ++ integer_to_list(Id) ++ 
    					" ;"),
    delete_line("arity_qualifier", MId, Id),
    delete_atom(MId, Body),
    delete_integer(MId, Arity).

%% =====================================================================
%% @spec delete_atom(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current atom from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the atom.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_atom(MId,Id) ->
    delete_line("name", MId, Id).

%% =====================================================================
%% @spec delete_attribute(MId::integer(),
%%          	     	  Id::integer()) -> ok
%%
%% @doc
%% Deletes the current attribute from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the attribute.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_attribute(MId,Id) ->
    Elements = get_elements("attribute", "argument" , MId, Id),
    delete_line("attribute", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_binary(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current binary from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the binary.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_binary(MId,Id) ->
    Elements = get_elements("binary_", "field" , MId, Id),
    delete_line("binary_", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_binary_field(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current binary field from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the binary field.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_binary_field(MId,Id) ->
    Elements = get_elements("binary_field", "argument" , MId, Id),
    delete_line("binary_field", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_block_expr(MId::integer(),
%%          	     	   Id::integer()) -> ok
%%
%% @doc
%% Deletes the current block expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the block expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_block_expr(MId,Id) ->
    Elements = get_elements("block_expr", "body" , MId, Id),
    delete_line("block_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_case_expr(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current case expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the case expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_case_expr(MId,Id) ->
    Elements = get_elements("case_expr", "argument" , MId, Id),
    delete_line("case_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_catch_expr(MId::integer(),
%%          	     	   Id::integer()) -> ok
%%
%% @doc
%% Deletes the current catch expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the catch expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_catch_expr(MId,Id) ->
    [{Expression}] = refactor_db:select("select expression from catch_expr "
    					"where mid=" ++ integer_to_list(MId)
					++ " and id=" ++ integer_to_list(Id) 
					++ " ;"),
    delete_line("catch_expr", MId, Id),
    delete_node(MId, Expression).

%% =====================================================================
%% @spec delete_char(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current char from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the char.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_char(MId,Id) ->
    delete_line("char_", MId, Id).

%% =====================================================================
%% @spec delete_class_qualifier(MId::integer(),
%%          	     		Id::integer()) -> ok
%%
%% @doc
%% Deletes the current class qualifier from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the class qualifier.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_class_qualifier(MId,Id) ->
    [{Class,Body}] = refactor_db:select("select class,body from "
    					"class_qualifier where "
    					"mid=" ++ integer_to_list(MId) ++ 
    					" and id=" ++ integer_to_list(Id) 
    					++ " ;"),
    delete_line("class_qualifier", MId, Id),
    delete_node(MId, Class),
    delete_node(MId, Body).

%% =====================================================================
%% @spec delete_clause(MId::integer(),
%%          	       Id::integer()) -> ok
%%
%% @doc
%% Deletes the current clause from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the clause.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_clause(MId,Id) ->
    Elements = get_elements("clause", "argument" , MId, Id),
    delete_line("clause", MId, Id),
    delete_line("scope_visib", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_comment(MId::integer(),
%%          	        Id::integer()) -> ok
%%
%% @doc
%% Deletes the current comment from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the comment.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_comment(MId,Id) ->
    delete_line("comment", MId, Id).
    
%% =====================================================================
%% @spec delete_cond_expr(MId::integer(),
%%          	          Id::integer()) -> ok
%%
%% @doc
%% Deletes the current condition expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the condition expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_cond_expr(MId,Id) ->
    Elements = get_elements("cond_expr", "clause" , MId, Id),
    delete_line("cond_expr", MId, Id),
    delete_line("node_type", MId, Id),
    delete_line("scope", MId, Id),
    delete_line("pos", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_conjunction(MId::integer(),
%%          	     	    Id::integer()) -> ok
%%
%% @doc
%% Deletes the current conjunction from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the conjunction.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_conjunction(MId,Id) ->
    Elements = get_elements("conjunction", "argument" , MId, Id),
    delete_line("conjunction", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_disjunction(MId::integer(),
%%          	     	    Id::integer()) -> ok
%%
%% @doc
%% Deletes the current disjunction from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the disjunction.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_disjunction(MId,Id) ->
    Elements = get_elements("disjunction", "argument" , MId, Id),
    delete_line("disjunction", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_eof_marker(MId::integer(),
%%          	     	   Id::integer()) -> ok
%%
%% @doc
%% Deletes the current eof marker from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the eof marker.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_eof_marker(_MId,_Id) ->
    ok.

%% =====================================================================
%% @spec delete_float(MId::integer(),
%%          	      Id::integer()) -> ok
%%
%% @doc
%% Deletes the current float from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the float.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_float(MId,Id) ->
    delete_line("float_", MId, Id).

%% =====================================================================
%% @spec delete_form_list(MId::integer(),
%%          	     	  Id::integer()) -> ok
%%
%% @doc
%% Deletes the current form list from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the form list.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_form_list(MId,Id) ->
    Elements = get_elements("form_list", "form" , MId, Id),
    delete_line("form_list", MId, Id),
    refactor_db:delete("delete from id_count where "
    		       "mid=" ++ integer_to_list(MId) ++ ";"),
    refactor_db:delete("delete from module where "
    			"mid=" ++ integer_to_list(MId) ++ ";"),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_fun_expr(MId::integer(),
%%          	     	 Id::integer()) -> ok
%%
%% @doc
%% Deletes the current fun expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the fun expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_fun_expr(MId,Id) ->
    Elements = get_elements("fun_expr", "clause" , MId, Id),
    delete_line("fun_expr", MId, Id),
    delete_line("fun_visib", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_function(MId::integer(),
%%          	     	 Id::integer()) -> ok
%%
%% @doc
%% Deletes the current function from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the function.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_function(MId,Id) ->
    Elements = get_elements("function", "clause" , MId, Id),
    delete_line("function", MId, Id),
    delete_line("fun_visib", MId, Id),
    refactor_db:delete("delete from fun_call where "
    			"tmid=" ++ integer_to_list(MId) ++ 
    			" and target=" ++ integer_to_list(Id)++ ";"),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_generator(MId::integer(),
%%          	     	  Id::integer()) -> ok
%%
%% @doc
%% Deletes the current generator from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the generator.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_generator(MId,Id) ->
    [{Pattern,Body}] = refactor_db:select("select pattern,body from "
    					  "generator where mid=" ++ 
    					  integer_to_list(MId) ++ 
    					  " and id=" ++ integer_to_list(Id) 
    					  ++ " ;"),
    delete_line("generator", MId, Id),
    delete_node(MId,Pattern),
    delete_node(MId,Body).

%% =====================================================================
%% @spec delete_if_expr(MId::integer(),
%%          	        Id::integer()) -> ok
%%
%% @doc
%% Deletes the current if expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the if expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_if_expr(MId,Id) ->
    Elements = get_elements("if_expr", "clause" , MId, Id),
    delete_line("if_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_implicit_fun(MId::integer(),
%%          	     	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current implicit fun from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the implicit fun.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_implicit_fun(MId,Id) ->
    delete_line("fun_call",MId,Id),
    [{NameId}] = refactor_db:select("select name_id from implicit_fun "
    				    "where mid=" ++ integer_to_list(MId) ++ 
    				    " and id=" ++ integer_to_list(Id) ++ " ;"),
    delete_line("implicit_fun", MId, Id),
    delete_node(MId, NameId).

%% =====================================================================
%% @spec delete_infix_expr(MId::integer(),
%%          	     	   Id::integer()) -> ok
%%
%% @doc
%% Deletes the current infix expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the infix expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_infix_expr(MId,Id) ->
    Elements = refactor_db:select("select lft,oper,rght from infix_expr "
    				  "where mid=" ++ integer_to_list(MId) ++ 
    				  " and id=" ++ integer_to_list(Id) ++ " ;"),
    delete_line("infix_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_integer(MId::integer(),
%%          	        Id::integer()) -> ok
%%
%% @doc
%% Deletes the current integer from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the integer.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_integer(MId,Id) ->
    delete_line("integer_",MId,Id).

%% =====================================================================
%% @spec delete_list(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current list from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the list.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_list(MId,Id) ->
    Elements = get_elements("list", "element" , MId, Id),
    delete_line("list", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_list_comp(MId::integer(),
%%          	     	  Id::integer()) -> ok
%%
%% @doc
%% Deletes the current list comprehension from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the list comprehension.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_list_comp(MId,Id) ->
    Elements = get_elements("list_comp", "argument" , MId, Id),
    delete_line("list_comp", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_macro(MId::integer(),
%%          	      Id::integer()) -> ok
%%
%% @doc
%% Deletes the current macro from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the macro.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_macro(MId,Id) ->
    Elements = get_elements("macro", "argument" , MId, Id),
    delete_line("macro", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_match_expr(MId::integer(),
%%          	     	   Id::integer()) -> ok
%%
%% @doc
%% Deletes the current match expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the match expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_match_expr(MId,Id) ->
    Elements = refactor_db:select("select pattern,body from match_expr "
    				  "where mid=" ++ integer_to_list(MId) ++ 
    				  " and id=" ++ integer_to_list(Id) ++ " ;"),
    delete_line("match_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_module_qualifier(MId::integer(),
%%          	     		 Id::integer()) -> ok
%%
%% @doc
%% Deletes the current module qualifier from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the module qualifier.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_module_qualifier(MId,Id) ->
    [{Module,Body}] = refactor_db:select("select module,body from "
    					 "module_qualifier where mid=" 
					 ++ integer_to_list(MId) ++ 
					 " and id=" ++ integer_to_list(Id) 
					 ++ " ;"),
    delete_line("module_qualifier", MId,Id),
    delete_node(MId,Module),
    delete_node(MId,Body).

%% =====================================================================
%% @spec delete_nil(MId::integer(),
%%          	    Id::integer()) -> ok
%%
%% @doc
%% Deletes the current nil from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the nil.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_nil(_MId,_Id) ->
    ok.

%% =====================================================================
%% @spec delete_operator(MId::integer(),
%%          	         Id::integer()) -> ok
%%
%% @doc
%% Deletes the current operator from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the operator.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_operator(MId,Id) ->
    delete_line("name", MId,Id).

%% =====================================================================
%% @spec delete_parentheses(MId::integer(),
%%          	     	    Id::integer()) -> ok
%%
%% @doc
%% Deletes the current parantheses from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the parantheses.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_parentheses(MId,Id) ->
    [{Body}] = refactor_db:select("select body from parentheses where "
    				  "mid=" ++ integer_to_list(MId) ++ 
    				  " and id=" ++ integer_to_list(Id) ++ " ;"),
    delete_line("parentheses", MId, Id),
    delete_node(MId, Body).  

%% =====================================================================
%% @spec delete_prefix_expr(MId::integer(),
%%          	     	    Id::integer()) -> ok
%%
%% @doc
%% Deletes the current prefix expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the prefix expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_prefix_expr(MId,Id) ->
    Elements = refactor_db:select("select operator,argument from prefix_expr"
    				   " where mid=" ++ integer_to_list(MId) ++ 
    				   " and id=" ++ integer_to_list(Id) ++ " ;"),
    delete_line("prefix_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_qualified_name(MId::integer(),
%%          	     	       Id::integer()) -> ok
%%
%% @doc
%% Deletes the current qualified name from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the qualified name.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_qualified_name(MId,Id) ->
    Elements = get_elements("qualified_name", "segment" , MId, Id),
    delete_line("qualified_name", MId, Id),
    delete_nodes(MId, Elements).    

%% =====================================================================
%% @spec delete_query_expr(MId::integer(),
%%          	     	   Id::integer()) -> ok
%%
%% @doc
%% Deletes the current query expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the query expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_query_expr(MId,Id) ->
    [{Body}] = refactor_db:select("select body from query_expr where "
    				  "mid=" ++ integer_to_list(MId) ++ 
    				  " and id=" ++ integer_to_list(Id) ++ " ;"),
    delete_line("query_expr", MId, Id),
    delete_node(MId, Body).  

%% =====================================================================
%% @spec delete_receive_expr(MId::integer(),
%%          	     	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current receive expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the receive expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_receive_expr(MId,Id) ->
    Elements = get_elements("receive_expr", "argument" , MId, Id),
    delete_line("receive_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_record_access(MId::integer(),
%%          	     	      Id::integer()) -> ok
%%
%% @doc
%% Deletes the current record access from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the record access.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_record_access(MId,Id) ->
    Elements = get_elements("record_access", "argument,type,field", MId, Id),
    delete_line("record_access", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_record_expr(MId::integer(),
%%          	     	    Id::integer()) -> ok
%%
%% @doc
%% Deletes the current record expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the record expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_record_expr(MId,Id) ->
    Elements = get_elements("record_expr", "argument", MId, Id),
    delete_line("record_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_record_field(MId::integer(),
%%          	     	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current record field from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the record field.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_record_field(MId,Id) ->
    Elements = get_elements("record_field", "name_id,value", MId, Id),
    delete_line("record_field", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_record_index_expr(MId::integer(),
%%          	     		  Id::integer()) -> ok
%%
%% @doc
%% Deletes the current record index expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the record index expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_record_index_expr(MId,Id) ->
    Elements = get_elements("record_index_expr", "type,field", MId, Id),
    delete_line("record_index_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_rule(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current rule from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the rule.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_rule(MId,Id) ->
    Elements = get_elements("rule", "argument", MId, Id),
    delete_line("rule", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_size_qualifier(MId::integer(),
%%          	     	       Id::integer()) -> ok
%%
%% @doc
%% Deletes the current size qualifier from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the size qualifier.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_size_qualifier(MId,Id) ->
    Elements = get_elements("size_qualifier", "body,size", MId, Id),
    delete_line("size_qualifier", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_string(MId::integer(),
%%          	       Id::integer()) -> ok
%%
%% @doc
%% Deletes the current string from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the string.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_string(MId,Id) ->
    delete_line("string", MId, Id).

%% =====================================================================
%% @spec delete_text(MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current text from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the text.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_text(MId,Id) ->
    delete_line("text", MId, Id).

%% =====================================================================
%% @spec delete_try_expr(MId::integer(),
%%          	     	 Id::integer()) -> ok
%%
%% @doc
%% Deletes the current try expression from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the try expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_try_expr(MId,Id) ->
    Elements = get_elements("try_expr", "argument", MId, Id),
    delete_line("try_expr", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_tuple(MId::integer(),
%%          	      Id::integer()) -> ok
%%
%% @doc
%% Deletes the current tuple from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the tuple.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_tuple(MId,Id) ->
    Elements = get_elements("tuple", "element", MId, Id),
    delete_line("tuple", MId, Id),
    delete_nodes(MId, Elements).

%% =====================================================================
%% @spec delete_underscore(MId::integer(),
%%          	     	   Id::integer()) -> ok
%%
%% @doc
%% Deletes the current underscore from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the underscore.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_underscore(_MId,_Id) ->
    ok.

%% =====================================================================
%% @spec delete_variable(MId::integer(),
%%          	     	 Id::integer()) -> ok
%%
%% @doc
%% Deletes the current variable from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the variable.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

delete_variable(MId,Id) ->
    delete_line("name", MId, Id),
    delete_line("var_visib", MId, Id),
    case it_is_first_occurrence(MId,Id) of
	false ->
	    ok;
	true ->
	    introduce_new_first_occurrence(MId,Id)
    end.

%% =====================================================================
%% @spec delete_line(Table::string(),
%%		     MId::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Deletes the current line from the database.
%% 
%% Parameter description:<pre>
%% <b>Table</b> : The name of the table, which contains the line.
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%%</pre>
%% @end
%% Used: delete_*/2
%% =====================================================================

delete_line(Table,MId,Id) ->    
    refactor_db:delete("delete from " ++ Table ++ 
    			" where mid=" ++ integer_to_list(MId) ++ 
    			" and id=" ++ integer_to_list(Id)++ ";").

%% =====================================================================
%% @spec get_elements(Table::string(),
%%		      Attribute::string(),
%%		      MId::integer(),
%%          	      Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the attributes of the node from the table. 
%% 
%% Parameter description:<pre>
%% <b>Table</b> : The name of the table, which contains the line.
%% <b>Attribute</b> : The name of the column in the table, the attribute.
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%%</pre>
%% @end
%% Used: delete_*/2
%% =====================================================================

get_elements(Table, Attribute, MId, Id) ->
    Elements = refactor_db:select("select " ++ Attribute ++ " from " ++ Table 
    				  ++ " where mid=" ++ integer_to_list(MId) ++ 
    				  " and id=" ++ integer_to_list(Id) ++ " ;"),
    Elements.

%% =====================================================================
%% @spec it_is_first_occurrence(MId::integer(),
%%          	     		Id::integer()) -> bool()
%%
%% @doc
%% Dicedes if the current variable is a first occurrence, and 
%% returns with the boolean value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%%</pre>
%% @end
%% Used: delete_variable/2
%% =====================================================================

it_is_first_occurrence(MId,Id) ->
    VarIds = refactor_db:select("select id from var_visib where "
    				"mid=" ++ integer_to_list(MId) ++ 
    				" and target=" ++ integer_to_list(Id) ++ " ;"),
    VarIds /= [].

%% =====================================================================
%% @spec introduce_new_first_occurrence(MId::integer(),
%%          	     			Id::integer()) -> ok
%%
%% @doc
%% Deletes the current element from the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%%</pre>
%% @end
%% Used: delete_variable/2
%% =====================================================================

introduce_new_first_occurrence(MId,Id) ->
    VarIdsPos = refactor_db:select("select v.id,p.line,p.col from "
    				   "var_visib as v,pos as p"
				   " where v.mid=p.mid and "
				   " v.mid=" ++ integer_to_list(MId) ++ 
				   " and v.target=" ++ integer_to_list(Id) 
				   ++ " and v.id=p.id ;"),
    FirstId = get_first_occurrence(VarIdsPos),
    refactor_db:update("update var_visib set target= " 
		       ++ integer_to_list(FirstId) ++ 
    			" where mid=" ++ integer_to_list(MId) ++ 
    			" and target=" ++ integer_to_list(Id)++ ";").

%% =====================================================================
%% @spec get_first_occurrence(List::[{integer(),integer(), integer()}]) 
%%				-> integer()
%%
%% @doc
%% Returns the first occurence of the variables.
%% 
%% Parameter description:<pre>
%% <b>List</b> : List of the ids, line and column numbers.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
get_first_occurrence([{Id,Line,Col}|Xs]) ->
    get_first_occurrence(Xs, Id, Line, Col).

%% =====================================================================
%% @spec get_first_occurrence(List::[{integer(),integer(), integer()}],
%%			      Id::integer(),
%%			      Line::integer(),
%%			      Col::integer()) -> integer()
%%
%% @doc
%% Returns the first occurence of the variables.
%% 
%% Parameter description:<pre>
%% <b>List</b> : List of the ids, line and column numbers.
%% <b>Id</b> : Id of the variable.
%% <b>Line</b> : The line number of the position.
%% <b>Col</b> : The column number of the position.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
get_first_occurrence([], Id, _Line, _Col) ->
    Id;
get_first_occurrence([{Id2,Line2,Col2}|Xs], _Id, Line, Col) 
  when Line2<Line; Line==Line2, Col2<Col ->
    get_first_occurrence(Xs, Id2, Line2, Col2);
get_first_occurrence([_VarIdPos | Xs], Id, Line, Col) ->
    get_first_occurrence(Xs , Id, Line, Col).


%% =====================================================================
%% @spec detach_node(MId::integer(),
%%          	     Id::integer(), From::integer()) -> ok
%%
%% @doc
%% Detaches the element from its root.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%% <b>From</b> : Id of the root node.
%% </pre>
%% @end
%% =====================================================================    
detach_node(MId, Id, From) ->
    Type = erl_syntax_db:type(MId, From),
    detach_node(MId, Id, From, Type).

%% =====================================================================
%% @spec detach_node(MId::integer(), Id::integer(), 
%%                  From::integer(), Type::integer()) -> ok
%%
%% @doc
%% Detaches the element from its root.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%% <b>From</b> : Id of the root node.
%% <b>Type</b> : Type of the root node.
%% </pre>
%% @end
%% =====================================================================    
detach_node(MId, Id, From, ?CLAUSE) -> 
    [{Pos, Qualifier}] = 
        refactor_db:select(
          "select pos, qualifier from clause where mid=" ++ integer_to_list(MId) 
          ++ " and id=" ++ integer_to_list(From) ++ " and argument=" 
          ++ integer_to_list(Id) ++";"),
    [{MaxPos}] = 
        refactor_db:select(
          "select max(pos) from clause where mid=" ++ integer_to_list(MId) 
          ++ " and id=" ++ integer_to_list(From) ++ " and qualifier=" 
          ++ integer_to_list(Qualifier) ++" ;"),
    refactor_db:delete(
      "delete from clause where mid=" ++ integer_to_list(MId) 
      ++ " and id=" ++ integer_to_list(From) ++ " and pos=" ++ integer_to_list(Pos) 
      ++ " and qualifier=" ++ integer_to_list(Qualifier) ++ ";" ),
    if 
        MaxPos /= Pos ->
            lists:foreach(
              fun (Pos2) ->
                      refactor_db:update(
                        "update clause set pos=pos-1 where mid=" 
                        ++ integer_to_list(MId)
                        ++ " and id="++ integer_to_list(From) ++ " and qualifier=" 
                        ++ integer_to_list(Qualifier) 
                        ++ " and pos=" ++ integer_to_list(Pos2) ++ ";")
              end, lists:seq(Pos+1, MaxPos));
        true ->
            ok
    end;
detach_node(MId, Id, From, ?MATCH_EXPR) -> 
    [{Pattern, Body}] = 
        refactor_db:select(
          "select pattern,body from match_expr where mid=" ++ integer_to_list(MId) 
          ++ " and id=" ++ integer_to_list(From) ++ ";"),
    if 
        Pattern == Id ->
            refactor_db:update(
              "update match_expr set pattern=0 where mid=" 
              ++ integer_to_list(MId)
              ++ " and id="++ integer_to_list(From) 
              ++  ";");
        Body == Id ->
            refactor_db:update(
              "update match_expr set body=0 where mid=" 
              ++ integer_to_list(MId)
              ++ " and id="++ integer_to_list(From) 
              ++  ";");
        true ->
            ok
    end;
detach_node(_MId, _Id, _From, _Type) ->
    error_logger:info_msg("Detach not implemented for this node_type yet.").




