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

%% @doc The module creates nodes directly into the database.
%% @end


-module(create_nodes).

-vsn('0.1').

-include("node_type.hrl").

-export([create_application/4,create_application/3,
	 create_arity_qualifier/4,create_arity_qualifier/3,
	 create_atom/3,create_atom/2,
	 create_attribute/4,create_attribute/3,
	 create_binary/3,create_binary/2,
	 create_binary_field/4,create_binary_field/3,
	 create_block_expr/3,create_block_expr/2,
	 create_case_expr/4,create_case_expr/3,
	 create_catch_expr/3,create_catch_expr/2,
	 create_char/3,create_char/2,
	 create_class_qualifier/4,create_class_qualifier/3,
	 create_clause/5,create_clause/4,
	 create_comment/4,create_comment/3,
	 create_cond_expr/3,create_cond_expr/2,
	 create_conjunction/3,create_conjunction/2,
	 create_disjunction/3,create_disjunction/2,
	 create_eof_marker/2,create_eof_marker/1,
	 create_float/3,create_float/2,
	 create_form_list/3,create_form_list/2,
	 create_fun_expr/3,create_fun_expr/2,
	 create_function/4,create_function/3,
	 create_generator/4,create_generator/3,
	 create_if_expr/3,create_if_expr/2,
	 create_implicit_fun/3,create_implicit_fun/2,
	 create_infix_expr/5,create_infix_expr/4,
	 create_integer/3,create_integer/2,
	 create_list/4,create_list/3,
	 create_list_comp/4,create_list_comp/3,
	 create_macro/4,create_macro/3,
	 create_match_expr/4,create_match_expr/3,
	 create_module_qualifier/4,create_module_qualifier/3,
	 create_nil/2,create_nil/1,
	 create_operator/3,create_operator/2,
	 create_parentheses/3,create_parentheses/2,
	 create_prefix_expr/4,create_prefix_expr/3,
	 create_qualified_name/3,create_qualified_name/2,
	 create_query_expr/3,create_query_expr/2,
	 create_receive_expr/5,create_receive_expr/4,
	 create_record_access/5,create_record_access/4,
	 create_record_expr/5,create_record_expr/4,
	 create_record_field/4,create_record_field/3,
	 create_record_index_expr/4,create_record_index_expr/3,
	 create_rule/4,create_rule/3,
	 create_size_qualifier/4,create_size_qualifier/3,
	 create_string/3,create_string/2,
	 create_text/3,create_text/2,
	 create_try_expr/6,create_try_expr/5,
	 create_tuple/3,create_tuple/2,
	 create_underscore/2,create_underscore/1,
	 create_variable/3,create_variable/2,
	 create_variables/4,create_fresh_variable/1,
	 init_position/2,init_scope/3,create_scope/3,
	 connect_fun_call/4,connect_variables/3,
	 conv_from_ast_to_id/1, attach_subtree_to_node/4,
         move_node/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Node creating functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% application %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_application(MId::integer(),
%%          		    FunctionId::integer(),
%%          		    ArgumentsId::[integer()],
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new application into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunctionId</b> : Identifier of the function.
%% <b>ArgumentsId</b> : Identifiers of the arguments.
%% <b>Id</b> : Identifier of the new application node.
%%</pre>
%% @end
%% Used: create_application/3
%% =====================================================================
create_application(MId,FunctionId,ArgumentsId,Id) ->
    refactor_db:insert("insert into application (mid,id,pos, argument) 
    		       values (" ++ integer_to_list(MId) ++ "," 
		       ++ integer_to_list(Id) ++", 0," 
		       ++ integer_to_list(FunctionId) ++ ");"),
    lists:foldl(fun(Element, Pos)->
                refactor_db:insert("insert into application"
                                   " (mid,id,pos,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,ArgumentsId),
    refactor_db:insert("insert into node_type (mid,id,type)"
    		        " values (" ++ integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++",1" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_application(MId::integer(),
%%          		    FunctionId::integer(),
%%          		    ArgumentsId::[integer()]) -> integer()
%%
%% @doc
%% Creates a new application into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunctionId</b> : Identifier of the function.
%% <b>ArgumentsId</b> : Identifiers of the arguments.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_application(MId,FunctionId,ArgumentsId) ->
    NewId = get_next_id(MId),
    create_application(MId,FunctionId,ArgumentsId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% arity_qualifier %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_arity_qualifier(MId::integer(),
%%          		        BodyId::integer(),
%%          		    	ArityId::integer(),
%%          		    	Id::integer()) -> ok
%%
%% @doc
%% Creates a new arity qualifier into the database, if the new id is
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyId</b> : Identifier of the body.
%% <b>ArityId</b> : Identifier of the arity.
%% <b>Id</b> : Identifier of the new arity qualifier node.
%%</pre>
%% @end
%% Used: create_arity_qualifier/3
%% =====================================================================
create_arity_qualifier(MId,BodyId,ArityId,Id) ->
    refactor_db:insert("insert into arity_qualifier (mid,id,body,arity)
    			values (" ++ integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++ "," ++ 
			integer_to_list(BodyId) ++"," 
			++ integer_to_list(ArityId) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" 
    			++ integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++",2" ++ ");"),
    init_position(MId,Id).



%% =====================================================================
%% @spec create_arity_qualifier(MId::integer(),
%%          		        BodyId::integer(),
%%          		    	ArityId::integer()) -> integer()
%%
%% @doc
%% Creates a new arity qualifier into the database, if the new id is
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyId</b> : Identifier of the body.
%% <b>ArityId</b> : Identifier of the arity.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_arity_qualifier(MId,BodyId,ArityId) ->
    NewId = get_next_id(MId),
    create_arity_qualifier(MId,BodyId,ArityId,NewId),
    NewId.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% atom %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_atom(MId::integer(),
%%          	     Name::string(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Creates a new atom into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : Name of the atom.
%% <b>Id</b> : Identifier of the new atom node.
%%</pre>
%% @end
%% Used: create_atom/2
%% =====================================================================
create_atom(MId,Name,Id) ->
    refactor_db:insert("insert into name (mid,id,name) values (" ++ 
    			integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++"," ++ 
			io_lib:write_string(Name) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++",3" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_atom(MId::integer(),
%%          	     Name::string()) -> integer()
%%
%% @doc
%% Creates a new atom into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : Name of the atom.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_atom(MId,Name) ->
    NewId = get_next_id(MId),
    create_atom(MId,Name,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% attribute %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_attribute(MId::integer(),
%%          		  NameId::integer(),
%%          		  ArgumentsId::none() | [integer()],
%%          		  Id::integer()) -> ok
%%
%% @doc
%% Creates a new attribute into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunctionId</b> : Identifier of the function.
%% <b>ArgumentsId</b> : Identifiers of the arguments (possibly none).
%% <b>Id</b> : Identifier of the new attribute node.
%%</pre>
%% @end
%% Used: create_attribute/3
%% =====================================================================

create_attribute(MId,NameId,none,Id) ->
    refactor_db:insert("insert into attribute_ (mid,id,pos, argument) values ("
			++ integer_to_list(MId) ++ "," ++ integer_to_list(Id) 
			++", 0," ++ integer_to_list(NameId) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++",4" ++ ");"),
    init_position(MId,Id);
create_attribute(MId,NameId,ArgumentsId,Id) ->
    refactor_db:insert("insert into attribute_ (mid,id,pos, argument) values ("
			++ integer_to_list(MId) ++ "," ++ integer_to_list(Id) 
			++", 0," ++ integer_to_list(NameId) ++ ");"),
    lists:foldl(fun(Element, Pos)->
    	refactor_db:insert("insert into attribute_ (mid,id,pos,argument) "
    			   "values (" ++ integer_to_list(MId) ++ "," ++ 
    			   integer_to_list(Id) ++"," ++ 
    			   integer_to_list(Pos) ++"," ++ 
    			   integer_to_list(Element) ++ ");"),
    	Pos+1 end ,1,ArgumentsId),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",4" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_attribute(MId::integer(),
%%          		  NameId::integer(),
%%          		  ArgumentsId::none() | [integer()]) -> integer()
%%
%% @doc
%% Creates a new attribute into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunctionId</b> : Identifier of the function.
%% <b>ArgumentsId</b> : Identifiers of the arguments (possibly none).
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_attribute(MId,NameId,ArgumentsId) ->
    NewId = get_next_id(MId),
    create_attribute(MId,NameId,ArgumentsId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% binary %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_binary(MId::integer(),
%%          	       FieldsId::[integer()],
%%          	       Id::integer()) -> ok
%%
%% @doc
%% Creates a new binary into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FieldsId</b> : Identifiers of the fields.
%% <b>Id</b> : Identifier of the new binary node.
%%</pre>
%% @end
%% Used: create_binary/2
%% =====================================================================

create_binary(MId,FieldsId,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into binary_ (mid,id,pos,field) "
    				   "values (" ++ integer_to_list(MId) ++ "," 
    				   ++ integer_to_list(Id) ++"," 
				   ++ integer_to_list(Pos) ++"," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,FieldsId),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++",5" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_binary(MId::integer(),
%%          	       FieldsId::[integer()]) -> integer()
%%
%% @doc
%% Creates a new binary into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FieldsId</b> : Identifiers of the fields.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_binary(MId,FieldsId) ->
    NewId = get_next_id(MId),
    create_binary(MId,FieldsId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% binary_field %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_binary_field(MId::integer(),
%%          		     BodyId::integer(),
%%          		     TypesId::[integer()],
%%          		     Id::integer()) -> ok
%%
%% @doc
%% Creates a new binary field into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyId</b> : Identifier of the body.
%% <b>TypesId</b> : Identifiers of the types.
%% <b>Id</b> : Identifier of the new binary field node.
%%</pre>
%% @end
%% Used: create_binary_field/3
%% =====================================================================
create_binary_field(MId,BodyId,TypesId,Id) ->
    refactor_db:insert("insert into binary_field (mid,id,pos, argument) "
    			"values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++", 0," 
			++ integer_to_list(BodyId) ++ ");"),
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into binary_field "
    				   "(mid,id,pos,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,TypesId),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++",6" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_binary_field(MId::integer(),
%%          		     BodyId::integer(),
%%          		     TypesId::[integer()]) -> integer()
%%
%% @doc
%% Creates a new binary field into the database, if the new id is not 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyId</b> : Identifier of the body.
%% <b>TypesId</b> : Identifiers of the types.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_binary_field(MId,BodyId,TypesId) ->
    NewId = get_next_id(MId),
    create_binary_field(MId,BodyId,TypesId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% block_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_block_expr(MId::integer(),
%%          		   BodyIds::[integer()],
%%          		   Id::integer()) -> ok
%%
%% @doc
%% Creates a new block expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyIds</b> : List of the body identifiers.
%% <b>Id</b> : Identifier of the new block expression node.
%%</pre>
%% @end
%% Used: create_block_expr/2
%% =====================================================================
create_block_expr(MId,BodyIds,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into block_expr (mid,id,pos,body)"
    				   " values (" ++ integer_to_list(MId) ++ "," 
    				   ++ integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,BodyIds),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++",7" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_block_expr(MId::integer(),
%%          		   BodyIds::[integer()]) -> integer()
%%
%% @doc
%% Creates a new block expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>BodyIds</b> : List of the body identifiers.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_block_expr(MId,BodyIds) ->
    NewId = get_next_id(MId),
    create_block_expr(MId,BodyIds,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% case_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_case_expr(MId::integer(),
%%          		  ArgumentId::integer(),
%%          		  ClausesId::[integer()],
%%          		  Id::integer()) -> ok
%%
%% @doc
%% Creates a new case expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ArgumentId</b> : Identifier of the argument.
%% <b>ClausesId</b> : Identifiers of the clauses.
%% <b>Id</b> : Identifier of the new case expression node.
%%</pre>
%% @end
%% Used: create_case_expr/3
%% =====================================================================
create_case_expr(MId,ArgumentId,ClausesId,Id) ->
    refactor_db:insert("insert into case_expr (mid,id,pos, argument) values (" 
		       ++ integer_to_list(MId) ++ "," 
		       ++ integer_to_list(Id) ++", 0," 
		       ++ integer_to_list(ArgumentId) ++ ");"),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into case_expr "
    				   "(mid,id,pos,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++ 
				   integer_to_list(Element) ++ ");"),
		Pos+1 end ,1,ClausesId),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," 
			++ integer_to_list(Id) ++",8" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_case_expr(MId::integer(),
%%          		  ArgumentId::integer(),
%%          		  ClausesId::[integer()]) -> integer()
%%
%% @doc
%% Creates a new case expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ArgumentId</b> : Identifier of the argument.
%% <b>ClausesId</b> : Identifiers of the clauses.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_case_expr(MId,ArgumentId,ClausesId) ->
    NewId = get_next_id(MId),
    create_case_expr(MId,ArgumentId,ClausesId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% catch_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_catch_expr(MId::integer(),
%%          		   ExpressionId::integer(),
%%          		   Id::integer()) -> ok
%%
%% @doc
%% Creates a new catch expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ExpressionId</b> : Identifier of the expression.
%% <b>Id</b> : Identifier of the new catch expression node.
%%</pre>
%% @end
%% Used: create_catch_expr/2
%% =====================================================================
create_catch_expr(MId,ExpressionId,Id) ->
    refactor_db:insert("insert into catch_expr (mid,id,expression) values (" ++
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(ExpressionId) ++");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",9" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_catch_expr(MId::integer(),
%%          		   ExpressionId::integer()) -> integer()
%%
%% @doc
%% Creates a new catch expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ExpressionId</b> : Identifier of the expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_catch_expr(MId,ExpressionId) ->
    NewId = get_next_id(MId),
    create_catch_expr(MId,ExpressionId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% char %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_char(MId::integer(),
%%          	     Value::integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Creates a new char into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Value</b> : Value of the char.
%% <b>Id</b> : Identifier of the new char node.
%%</pre>
%% @end
%% Used: create_char/2
%% =====================================================================
create_char(MId,Value,Id) -> 
    refactor_db:insert("insert into char_ (mid,id,value) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Value) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",10" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_char(MId::integer(),
%%          	     Value::integer()) -> integer()
%%
%% @doc
%% Creates a new char into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Value</b> : Value of the char.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_char(MId,Value) ->
    NewId = get_next_id(MId),
    create_char(MId,Value,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% class_qualifier %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_class_qualifier(MId::integer(),
%%          		        ClassId::integer(),
%%          		        BodyId::integer(),
%%          		        Id::integer()) -> ok
%%
%% @doc
%% Creates a new class qualifier into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ClassId</b> : Identifier of the class.
%% <b>BodyId</b> : Identifier of the body.
%% <b>Id</b> : Identifier of the new class qualifier node.
%%</pre>
%% @end
%% Used: create_class_qualifier/3
%% =====================================================================
create_class_qualifier(MId,ClassId,BodyId,Id)  ->
    refactor_db:insert("insert into class_qualifier (mid,id,class,body) "
    			"values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(ClassId) ++","
			++ integer_to_list(BodyId) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",11" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_class_qualifier(MId::integer(),
%%          		        ClassId::integer(),
%%          		        BodyId::integer()) -> integer()
%%
%% @doc
%% Creates a new class qualifier into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ClassId</b> : Identifier of the class.
%% <b>BodyId</b> : Identifier of the body.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_class_qualifier(MId,ClassId,BodyId) ->
    NewId = get_next_id(MId),
    create_class_qualifier(MId,ClassId,BodyId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% clause %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_clause(MId::integer(),
%%          	       PatternsId::[integer()],
%%          	       GuardId::none() | integer(),
%%          	       BodyIds::[integer()],
%%          	       Id::integer()) -> ok
%%
%% @doc
%% Creates a new clause into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>PatternsId</b> : Identifier of the patterns.
%% <b>GuardId</b> : Identifier of the guard (possibly none).
%% <b>BodyIds</b> : Identifier of the bodys.
%% <b>Id</b> : Identifier of the new clause node.
%%</pre>
%% @end
%% Used: create_clause/4
%% =====================================================================

create_clause(MId,PatternsId,none,BodyIds,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into clause "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++"0," ++
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,PatternsId),
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into clause "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++",2," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,BodyIds),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",12" ++ ");"),
    init_position(MId,Id);
create_clause(MId,PatternsId,GuardId,BodyIds,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into clause "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++"0," ++
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,PatternsId),
    refactor_db:insert("insert into clause (mid,id,pos,qualifier,argument) "
    			"values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++"," ++ "1" ++"," ++"1," ++ 
    			integer_to_list(GuardId) ++ ");"),
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into clause "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++",2," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,BodyIds),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",12" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_clause(MId::integer(),
%%          	       PatternsId::[integer()],
%%          	       GuardId::none() | integer(),
%%          	       BodyIds::[integer()]) -> integer()
%%
%% @doc
%% Creates a new clause into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>PatternsId</b> : Identifier of the patterns.
%% <b>GuardId</b> : Identifier of the guard (possibly none).
%% <b>BodyIds</b> : Identifier of the bodys.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_clause(MId,PatternsId,GuardId,BodyIds) ->
    NewId = get_next_id(MId),
    create_clause(MId,PatternsId,GuardId,BodyIds,NewId),
    NewId.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_comment(MId::integer(),
%%          		Padding::none() | integer(),
%%          		Strings::[string()],
%%          		Id::integer()) -> ok
%%
%% @doc
%% Creates a new comment into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Padding</b> : Length of the padding (possibly none).
%% <b>Strings</b> : List of the text strings.
%% <b>Id</b> : Identifier of the new comment node.
%%</pre>
%% @end
%% Used: create_comment/3
%% =====================================================================
create_comment(MId,none,Strings,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into comment (mid,id,pos,argument)"
    				   " values (" ++ integer_to_list(MId) ++ "," 
    				   ++ integer_to_list(Id) ++","++ 
    				   integer_to_list(Pos) ++ "," ++ 
    				   io_lib:write_string(Element) ++ ");")
		,Pos+1 end ,1,Strings),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",13" ++ ");"),
    init_position(MId,Id);
create_comment(MId,Padding,Strings,Id) ->
    refactor_db:insert("insert into comment (mid,id, pos, argument) values (" 
		       ++ integer_to_list(MId) ++ "," ++ 
		       integer_to_list(Id) ++", 0," ++ 
		       integer_to_list(Padding) ++ ");"),
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into comment (mid,id,pos,argument)"
    				   " values (" ++ integer_to_list(MId) ++ "," 
    				   ++ integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++ "," ++ 
    				   io_lib:write_string(Element) ++ ");")
		,Pos+1 end ,1,Strings),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",13" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_comment(MId::integer(),
%%          		Padding::none() | integer(),
%%          		Strings::[string()]) -> integer()
%%
%% @doc
%% Creates a new comment into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Padding</b> : Length of the padding (possibly none).
%% <b>Strings</b> : List of the text strings.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_comment(MId,Padding,Strings) ->
    NewId = get_next_id(MId),
    create_comment(MId,Padding,Strings,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% cond_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_cond_expr(MId::integer(),
%%          		  ClausesId::[integer()],
%%       		  Id::integer()) -> ok
%%
%% @doc
%% Creates a new condition expression into the database, if the new id 
%% is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ClausesId</b> : Identifier of the clauses.
%% <b>Id</b> : Identifier of the new condition expression node.
%%</pre>
%% @end
%% Used: create_cond_expr/3
%% =====================================================================

create_cond_expr(MId,ClausesId,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into cond_expr (mid,id,pos,clause)"
    				   " values (" ++ integer_to_list(MId) ++ "," 
    				   ++ integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");"),
		Pos+1 end ,1,ClausesId),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",14" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_cond_expr(MId::integer(),
%%          		  ClausesId::[integer()]) -> integer
%%
%% @doc
%% Creates a new condition expression into the database, if the new id 
%% is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ClausesId</b> : Identifier of the clauses.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_cond_expr(MId,ClausesId) ->
    NewId = get_next_id(MId),
    create_cond_expr(MId,ClausesId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% conjunction %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_conjunction(MId::integer(),
%%          		    ListIds::[integer()],
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new conjunction into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ListIds</b> : Identifier of the lists.
%% <b>Id</b> : Identifier of the new conjunction node.
%%</pre>
%% @end
%% Used: create_conjunction/2
%% =====================================================================

create_conjunction(MId,ListIds,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into conjunction "
    				   "(mid,id,pos,argument) values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");"),
		Pos+1 end ,1,ListIds),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",15" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_conjunction(MId::integer(),
%%          		    ListIds::[integer()]) -> integer()
%%
%% @doc
%% Creates a new conjunction into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ListIds</b> : Identifier of the lists.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_conjunction(MId,ListIds) ->
    NewId = get_next_id(MId),
    create_conjunction(MId,ListIds,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% disjunction %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_disjunction(MId::integer(),
%%          		    ListIds::[integer()],
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new disjunction into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ListIds</b> : Identifier of the lists.
%% <b>Id</b> : Identifier of the new disjunction node.
%%</pre>
%% @end
%% Used: create_disjunction/2
%% =====================================================================
create_disjunction(MId,ListIds,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into disjunction "
    				   "(mid,id,pos,argument) values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");"),
				   Pos+1 end ,1,ListIds),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",16" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_disjunction(MId::integer(),
%%          		    ListIds::[integer()]) -> integer()
%%
%% @doc
%% Creates a new disjunction into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ListIds</b> : Identifier of the lists.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_disjunction(MId,ListIds) ->
    NewId = get_next_id(MId),
    create_disjunction(MId,ListIds,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% eof_marker %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_eof_marker(MId::integer(),
%%          		   Id::integer()) -> ok
%%
%% @doc
%% Creates a new eof marker into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Identifier of the new eof marker node.
%%</pre>
%% @end
%% Used: create_eof_marker/1
%% =====================================================================
create_eof_marker(MId,Id) ->
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",17" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_eof_marker(MId::integer()) -> integer()
%%
%% @doc
%% Creates a new eof marker into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_eof_marker(MId) ->
    NewId = get_next_id(MId),
    create_eof_marker(MId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% float %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_float(MId::integer(),
%%          	      Value::integer(),
%%          	      Id::integer()) -> ok
%%
%% @doc
%% Creates a new float into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Value</b> : Value of the float.
%% <b>Id</b> : Identifier of the new float node.
%%</pre>
%% @end
%% Used: create_float/2
%% =====================================================================
create_float(MId,Value,Id) ->
    refactor_db:insert("insert into float_ (mid,id,value) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			float_to_list(Value)++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",18" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_float(MId::integer(),
%%          	      Value::integer()) -> integer()
%%
%% @doc
%% Creates a new float into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Value</b> : Value of the float.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_float(MId,Value) ->
    NewId = get_next_id(MId),
    create_float(MId,Value,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% form_list %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_form_list(MId::integer(),
%%          		  FormsId::[integer()],
%%          		  Id::integer()) -> ok
%%
%% @doc
%% Creates a new form list into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FormsId</b> : Identifier of the function.
%% <b>Id</b> : Identifier of the new form list node.
%%</pre>
%% @end
%% Used: create_form_list/2
%% =====================================================================

create_form_list(MId,FormsId,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into form_list (mid,id,pos,form) "
    				   "values (" ++ integer_to_list(MId) ++ "," ++ 
    		      		   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");"),
		Pos+1 end ,1,FormsId),
    refactor_db:insert("insert into id_count (mid,formlid,num) values(" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Id) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",19" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_form_list(MId::integer(),
%%          		  FormsId::[integer()]) -> integer()
%%
%% @doc
%% Creates a new form list into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FormsId</b> : Identifier of the function.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_form_list(MId,FormsId) ->
    NewId = get_next_id(MId),
    create_form_list(MId,FormsId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% fun_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_fun_expr(MId::integer(),
%%          		    ClausesId::[integer()],
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new fun expression into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ClausesId</b> : Identifier of the clauses.
%% <b>Id</b> : Identifier of the new fun expression node.
%%</pre>
%% @end
%% Used: create_fun_expr/2
%% =====================================================================
create_fun_expr(MId,ClausesId,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into fun_expr (mid,id,pos,clause) "
    				   "values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");"),
		Pos+1 end ,1,ClausesId),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",20" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_fun_expr(MId::integer(),
%%          		 ClausesId::[integer()]) -> integer()
%%
%% @doc
%% Creates a new fun expression into the database, if the new id is not 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ClausesId</b> : Identifier of the clauses.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_fun_expr(MId,ClausesId) ->
    NewId = get_next_id(MId),
    create_fun_expr(MId,ClausesId,NewId),
    NewId.        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% function %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_function(MId::integer(),
%%          		 NameId::integer(),
%%          		 ClausesId::[integer()],
%%          		 Id::integer()) -> ok
%%
%% @doc
%% Creates a new function into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NameId</b> : Identifier of the name.
%% <b>ClausesId</b> : Identifiers of the clauses.
%% <b>Id</b> : Identifier of the new function node.
%%</pre>
%% @end
%% Used: create_function/3
%% =====================================================================
create_function(MId,NameId,ClausesId,Id) ->
    refactor_db:insert("insert into function (mid,id,pos,clause) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++"," ++ "0" ++"," ++ 
    			integer_to_list(NameId) ++ ");"),
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into function (mid,id,pos,clause) "
    				   "values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,ClausesId),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",21" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_function(MId::integer(),
%%          		 NameId::integer(),
%%          		 ClausesId::[integer()]) -> integer()
%%
%% @doc
%% Creates a new function into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NameId</b> : Identifier of the name.
%% <b>ClausesId</b> : Identifiers of the clauses.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_function(MId,NameId,ClausesId) ->
    NewId = get_next_id(MId),
    create_function(MId,NameId,ClausesId, NewId),
    NewId.   
						  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% generator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_generator(MId::integer(),
%%          		  PatternId::integer(),
%%          		  BodyId::integer(),
%%          		  Id::integer()) -> ok
%%
%% @doc
%% Creates a new generator into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>PatternId</b> : Identifier of the function.
%% <b>BodyId</b> : Identifier of the arguments.
%% <b>Id</b> : Identifier of the new generator node.
%%</pre>
%% @end
%% Used: create_generator/3
%% =====================================================================

create_generator(MId,PatternId,BodyId,Id) ->
    refactor_db:insert("insert into generator (mid,id,pattern,body) values (" 
    			++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(PatternId) ++"," ++ 
    			integer_to_list(BodyId) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",22" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_generator(MId::integer(),
%%          		  PatternId::integer(),
%%          		  BodyId::integer()) -> integer()
%%
%% @doc
%% Creates a new generator into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>PatternId</b> : Identifier of the function.
%% <b>BodyId</b> : Identifier of the arguments.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_generator(MId,PatternId,BodyId) ->
    NewId = get_next_id(MId),
    create_generator(MId,PatternId,BodyId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% if_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_if_expr(MId::integer(),
%%          		ClausesId::[integer()],
%%          		Id::integer()) -> ok
%%
%% @doc
%% Creates a new if expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ClausesId</b> : Identifiers of the clauses.
%% <b>Id</b> : Identifier of the new if expression node.
%%</pre>
%% @end
%% Used: create_if_expr/2
%% =====================================================================

create_if_expr(MId,ClausesId,Id) ->
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into if_expr (mid,id,pos,clause) "
    				   "values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");"),
		Pos+1 end ,1,ClausesId),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",23" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_if_expr(MId::integer(),
%%          		ClausesId::[integer()]) -> integer()
%%
%% @doc
%% Creates a new if expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>ClausesId</b> : Identifiers of the clauses.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_if_expr(MId,ClausesId) ->
    NewId = get_next_id(MId),
    create_if_expr(MId,ClausesId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% implicit_fun %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_implicit_fun(MId::integer(),
%%          		     NameId::integer(),
%%           		     Id::integer()) -> ok
%%
%% @doc
%% Creates a new implicit fun into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NameId</b> : Identifier of the name.
%% <b>Id</b> : Identifier of the new implicit fun node.
%%</pre>
%% @end
%% Used: create_implicit_fun/2
%% =====================================================================
create_implicit_fun(MId,NameId,Id) ->
    refactor_db:insert("insert into implicit_fun (mid,id,name_id) values (" ++
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(NameId) ++");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",24" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_implicit_fun(MId::integer(),
%%          		     NameId::integer()) -> integer()
%%
%% @doc
%% Creates a new implicit fun into the database, if the new id is not 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NameId</b> : Identifier of the name.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_implicit_fun(MId,NameId) ->
    NewId = get_next_id(MId),
    create_implicit_fun(MId,NameId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% infix_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_infix_expr(MId::integer(),
%%          		   LeftId::integer(),
%%          		   OperatorId::integer(),
%%          		   RightId::integer(),
%%          		   Id::integer()) -> ok
%%
%% @doc
%% Creates a new infix expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>LeftId</b> : Identifier of the left side expression.
%% <b>OperatorId</b> : Identifier of the operator.
%% <b>RightId</b> : Identifier of the right side expression.
%% <b>Id</b> : Identifier of the new infix expression node.
%%</pre>
%% @end
%% Used: create_infix_expr/4
%% =====================================================================

create_infix_expr(MId,LeftId,OperatorId,RightId,Id) ->
    refactor_db:insert("insert into infix_expr (mid,id,lft,oper,rght)"
    			" values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(LeftId) ++"," ++ 
    			integer_to_list(OperatorId) ++","++ 
    			integer_to_list(RightId) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",25" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_infix_expr(MId::integer(),
%%          		   LeftId::integer(),
%%          		   OperatorId::integer(),
%%          		   RightId::integer()) -> integer()
%%
%% @doc
%% Creates a new infix expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>LeftId</b> : Identifier of the left side expression.
%% <b>OperatorId</b> : Identifier of the operator.
%% <b>RightId</b> : Identifier of the right side expression.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_infix_expr(MId,LeftId,OperatorId,RightId) ->
    NewId = get_next_id(MId),
    create_infix_expr(MId,LeftId,OperatorId,RightId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% integer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_integer(MId::integer(),
%%          		Value::integer(),
%%          		Id::integer()) -> ok
%%
%% @doc
%% Creates a new integer into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Value</b> : Value of the integer.
%% <b>Id</b> : Identifier of the new integer node.
%%</pre>
%% @end
%% Used: create_integer/2
%% =====================================================================
create_integer(MId,Value,Id) ->
    refactor_db:insert("insert into integer_ (mid,id,value) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Value)++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",26" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_integer(MId::integer(),
%%          		Value::integer()) -> integer()
%%
%% @doc
%% Creates a new integer into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Value</b> : Value of the integer.
%%</pre>
%% @end
%% Used:
%% =====================================================================

create_integer(MId,Value) ->
    NewId = get_next_id(MId),
    create_integer(MId,Value,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% list %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_list(MId::integer(),
%%          	     Elements::[integer()],
%%          	     Tail::none() | integer(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Creates a new list into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Elements</b> : Identifiers of the elements.
%% <b>Tail</b> : Identifier of the tail.
%% <b>Id</b> : Identifier of the new list node.
%%</pre>
%% @end
%% Used: create_list/3
%% =====================================================================

create_list(MId,Elements,none,Id)  ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into list (mid,id,pos,element)"
    				   " values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Elements),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",27" ++ ");"),
    init_position(MId,Id);
create_list(MId,Elements,Tail,Id)  ->
    refactor_db:insert("insert into list (mid,id, pos, element) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++", 0," ++ 
    			integer_to_list(Tail) ++ ");"),
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into list (mid,id,pos,element) "
    				   "values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Elements),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",27" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_list(MId::integer(),
%%          	     Elements::[integer()],
%%          	     Tail::none() | integer()) -> integer()
%%
%% @doc
%% Creates a new list into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Elements</b> : Identifiers of the elements.
%% <b>Tail</b> : Identifier of the tail.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_list(MId,Elements,Tail) ->
    NewId = get_next_id(MId),
    create_list(MId,Elements,Tail,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% list_comp %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_list_comp(MId::integer(),
%%          		  Template::integer(),
%%          		  Body::integer(),
%%          		  Id::integer()) -> ok
%%
%% @doc
%% Creates a new list comprehension into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Template</b> : Identifier of the template.
%% <b>Body</b> : Identifier of the body.
%% <b>Id</b> : Identifier of the new list comprehension node.
%%</pre>
%% @end
%% Used: create_list_comp/3
%% =====================================================================


create_list_comp(MId,Template,Body,Id) ->
    refactor_db:insert("insert into list_comp (mid,id, pos, argument) "
    		       "values (" ++ 
    		       integer_to_list(MId) ++ "," ++ 
    		       integer_to_list(Id) ++", 0," ++ 
    		       integer_to_list(Template) ++ ");"),
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into list_comp "
    		      		   "(mid,id,pos,argument) values (" ++ 
    		      		   integer_to_list(MId) ++ "," ++ 
    		      		   integer_to_list(Id) ++"," ++ 
    		      		   integer_to_list(Pos) ++"," ++ 
    		      		   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Body),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",28" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_list_comp(MId::integer(),
%%          		  Template::integer(),
%%          		  Body::integer()) -> integer()
%%
%% @doc
%% Creates a new list comprehension into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Template</b> : Identifier of the template.
%% <b>Body</b> : Identifier of the body.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_list_comp(MId,Template,Body) ->
    NewId = get_next_id(MId),
    create_list_comp(MId,Template,Body,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% macro %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_macro(MId::integer(),
%%          	      Name::integer(),
%%          	      Arguments::none() | [integer()],
%%          	      Id::integer()) -> ok
%%
%% @doc
%% Creates a new macro into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunctionId</b> : Identifier of the name.
%% <b>ArgumentsId</b> : Identifiers of the arguments (possibly none).
%% <b>Id</b> : Identifier of the new macro node.
%%</pre>
%% @end
%% Used: create_macro/3
%% =====================================================================


create_macro(MId,Name,none,Id) ->
    refactor_db:insert("insert into macro (mid,id, pos, argument) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++", 0," ++ 
    			integer_to_list(Name) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",29" ++ ");"),
    init_position(MId,Id);
create_macro(MId,Name,Arguments,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into macro (mid,id,pos,argument) "
    				   "values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Arguments),
    refactor_db:insert("insert into macro (mid,id, pos, argument) values (" ++
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++", 0," ++ 
    			integer_to_list(Name) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",29" ++ ");"),
    init_position(MId,Id).


%% =====================================================================
%% @spec create_macro(MId::integer(),
%%          	      Name::integer(),
%%          	      Arguments::none() | [integer()]) -> integer()
%%
%% @doc
%% Creates a new macro into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>FunctionId</b> : Identifier of the name.
%% <b>ArgumentsId</b> : Identifiers of the arguments (possibly none).
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_macro(MId,Name,Arguments) ->
    NewId = get_next_id(MId),
    create_macro(MId,Name,Arguments,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% match_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_match_expr(MId::integer(),
%%          		   Pattern::integer(),
%%          		   Body::integer(),
%%          		   Id::integer()) -> ok
%%
%% @doc
%% Creates a new match expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Pattern</b> : Identifier of the pattern.
%% <b>Body</b> : Identifier of the body.
%% <b>Id</b> : Identifier of the new match expression node.
%%</pre>
%% @end
%% Used: create_match_expr/3
%% =====================================================================
create_match_expr(MId,Pattern,Body,Id) ->
    refactor_db:insert("insert into match_expr (mid,id,pattern,body) values ("
    			++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Pattern) ++"," ++ 
    			integer_to_list(Body) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",30" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_match_expr(MId::integer(),
%%          		   Pattern::integer(),
%%          		   Body::integer()) -> integer()
%%
%% @doc
%% Creates a new match expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Pattern</b> : Identifier of the pattern.
%% <b>Body</b> : Identifier of the body.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_match_expr(MId,Pattern,Body) ->
    NewId = get_next_id(MId),
    create_match_expr(MId,Pattern,Body,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% module_qualifier %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_module_qualifier(MId::integer(),
%%          		    Module::integer(),
%%          		    Body::integer(),
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new module qualifier into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the current module.
%% <b>Module</b> : Identifier of the import module.
%% <b>Body</b> : Identifier of the body.
%% <b>Id</b> : Identifier of the new module qualifier node.
%%</pre>
%% @end
%% Used: create_module_qualifier/3
%% =====================================================================

create_module_qualifier(MId,Module,Body,Id) ->
    refactor_db:insert("insert into module_qualifier (mid,id,module,body) "
    			"values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Module) ++","++ 
    			integer_to_list(Body) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",31" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_module_qualifier(MId::integer(),
%%          		    Module::integer(),
%%          		    Body::integer()) -> integer()
%%
%% @doc
%% Creates a new module qualifier into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the current module.
%% <b>Module</b> : Identifier of the import module.
%% <b>Body</b> : Identifier of the body.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_module_qualifier(MId,Module,Body) ->
    NewId = get_next_id(MId),
    create_module_qualifier(MId,Module,Body,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% nil %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_nil(MId::integer(),
%%          	    Id::integer()) -> ok
%%
%% @doc
%% Creates a new nil into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Identifier of the new nil node.
%%</pre>
%% @end
%% Used: create_nil/1
%% =====================================================================

create_nil(MId,Id) ->
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",32" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_nil(MId::integer()) -> ok
%%
%% @doc
%% Creates a new nil into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_nil(MId) ->
    NewId = get_next_id(MId),
    create_nil(MId,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% operator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_operator(MId::integer(),
%%          		 Name::string(),
%%          		 Id::integer()) -> ok
%%
%% @doc
%% Creates a new operator into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : String of the name.
%% <b>Id</b> : Identifier of the new operator node.
%%</pre>
%% @end
%% Used: create_operator/2
%% =====================================================================


create_operator(MId,Name,Id) ->
    refactor_db:insert("insert into name (mid,id,name) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++"," ++ 
    			io_lib:write_string(Name)++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",33" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_operator(MId::integer(),
%%          		 Name::string()) -> integer()
%%
%% @doc
%% Creates a new operator into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : String of the name.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_operator(MId,Name) ->
    NewId = get_next_id(MId),
    create_operator(MId,Name,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% parentheses %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_parentheses(MId::integer(),
%%          		    Body::integer(),
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new parentheses into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Body</b> : Identifier of the body.
%% <b>Id</b> : Identifier of the new parentheses node.
%%</pre>
%% @end
%% Used: create_parentheses/2
%% =====================================================================
create_parentheses(MId,Body,Id) ->
    refactor_db:insert("insert into parentheses (mid,id,body) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Body) ++");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",34" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_parentheses(MId::integer(),
%%          		    Body::integer()) -> integer()
%%
%% @doc
%% Creates a new parentheses into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Body</b> : Identifier of the body.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_parentheses(MId,Body) ->
    NewId = get_next_id(MId),
    create_parentheses(MId,Body,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% prefix_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_prefix_expr(MId::integer(),
%%          		    Operator::integer(),
%%          		    Argument::integer(),
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new prefix expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Operator</b> : Identifier of the function.
%% <b>Argument</b> : Identifier of the argument.
%% <b>Id</b> : Identifier of the new prefix expression node.
%%</pre>
%% @end
%% Used: create_prefix_expr/3
%% =====================================================================
create_prefix_expr(MId,Operator,Argument,Id) ->
    refactor_db:insert("insert into prefix_expr (mid,id,operator,argument) "
    			"values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Operator) ++","++ 
    			integer_to_list(Argument) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",35" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_prefix_expr(MId::integer(),
%%          		    Operator::integer(),
%%          		    Argument::integer()) -> integer()
%%
%% @doc
%% Creates a new prefix expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Operator</b> : Identifier of the function.
%% <b>Argument</b> : Identifier of the argument.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_prefix_expr(MId,Operator,Argument) ->
    NewId = get_next_id(MId),
    create_prefix_expr(MId,Operator,Argument,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% qualified_name %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_qualified_name(MId::integer(),
%%          		       Segments::[integer()],
%%          		       Id::integer()) -> ok
%%
%% @doc
%% Creates a new qualified name into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Segments</b> : Identifiers of the segments.
%% <b>Id</b> : Identifier of the new qualified name node.
%%</pre>
%% @end
%% Used: create_qualified_name/2
%% =====================================================================

create_qualified_name(MId,Segments,Id) ->
    lists:foldl(fun(Element, Pos)->
    		refactor_db:insert("insert into qualified_name "
    				   "(mid,id,pos,segment) values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Segments),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",36" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_qualified_name(MId::integer(),
%%          		       Segments::[integer()]) -> integer()
%%
%% @doc
%% Creates a new qualified name into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Segments</b> : Identifiers of the segments.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_qualified_name(MId,Segments) ->
    NewId = get_next_id(MId),
    create_qualified_name(MId,Segments,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% query_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_query_expr(MId::integer(),
%%          		   Body::integer(),
%%          		   Id::integer()) -> ok
%%
%% @doc
%% Creates a new query expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Body</b> : Identifier of the body.
%% <b>Id</b> : Identifier of the new query expression node.
%%</pre>
%% @end
%% Used: create_query_expr/2
%% =====================================================================
create_query_expr(MId,Body,Id) -> 
    refactor_db:insert("insert into query_expr (mid,id,body) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Body) ++");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",37" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_query_expr(MId::integer(),
%%          		   Body::integer()) -> integer()
%%
%% @doc
%% Creates a new query expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Body</b> : Identifier of the body.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_query_expr(MId,Body) ->
    NewId = get_next_id(MId),
    create_query_expr(MId,Body,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% receive_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_receive_expr(MId::integer(),
%%          		    Clauses::[integer()],
%%          		    Timeout::none() | integer(),
%%          		    Action::[integer()],
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new receive expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Clauses</b> : Identifiers of the clauses.
%% <b>Timeout</b> : Identifier of the timeout (possibly none).
%% <b>Action</b> : Identifiers of the actions.
%% <b>Id</b> : Identifier of the new receive expression node.
%%</pre>
%% @end
%% Used: create_receive_expr/4
%% =====================================================================

create_receive_expr(MId,Clauses,none,Action,Id) ->
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into receive_expr "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++"0," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Clauses),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into receive_expr "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++"2," ++ 
				   integer_to_list(Element) ++ ");"),
		Pos+1 end ,1,Action),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",38" ++ ");"),
    init_position(MId,Id);
create_receive_expr(MId,Clauses,Timeout,Action,Id) ->
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into receive_expr "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++"0," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Clauses),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into receive_expr "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++"2," ++ 
				   integer_to_list(Element) ++ ");"),
		Pos+1 end ,1,Action),
    refactor_db:insert("insert into receive_expr "
    		       "(mid,id,pos,qualifier,argument) values (" ++ 
    		       integer_to_list(MId) ++ "," ++ 
    		       integer_to_list(Id) ++", 1, 1," ++ 
    		       integer_to_list(Timeout) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",38" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_receive_expr(MId::integer(),
%%          		    Clauses::[integer()],
%%          		    Timeout::none() | integer(),
%%          		    Action::[integer()]) -> integer()
%%
%% @doc
%% Creates a new receive expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Clauses</b> : Identifiers of the clauses.
%% <b>Timeout</b> : Identifier of the timeout (possibly none).
%% <b>Action</b> : Identifiers of the actions.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_receive_expr(MId,Clauses,Timeout,Action) ->
    NewId = get_next_id(MId),
    create_receive_expr(MId,Clauses,Timeout,Action,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% record_access %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_record_access(MId::integer(),
%%          		      Argument::integer(),
%%          		      Type::none() | integer(),
%%          		      Field::integer(),
%%          		      Id::integer()) -> ok
%%
%% @doc
%% Creates a new record access into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Argument</b> : Identifier of the argument.
%% <b>Type</b> : Identifier of the type (possibly none).
%% <b>Field</b> : Identifier of the field.
%% <b>Id</b> : Identifier of the new record access node.
%%</pre>
%% @end
%% Used: create_record_access/4
%% =====================================================================

create_record_access(MId,Argument,none,Field,Id) ->
    refactor_db:insert("insert into record_access "
    		       "(mid,id,argument,type,field) values (" ++ 
    		       integer_to_list(Id) ++ "," ++ 
    		       integer_to_list(Argument) ++","++ "-1" ++"," ++ 
    		       integer_to_list(Field) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",39" ++ ");"),
    init_position(MId,Id);
create_record_access(MId,Argument,Type,Field,Id) ->
    refactor_db:insert("insert into record_access "
    			"(mid,id,argument,type,field) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Argument) ++ ","++ 
    			integer_to_list(Type) ++"," ++ 
    			integer_to_list(Field) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",39" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_record_access(MId::integer(),
%%          		      Argument::integer(),
%%          		      Type::none() | integer(),
%%          		      Field::integer()) -> integer()
%%
%% @doc
%% Creates a new record access into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Argument</b> : Identifier of the argument.
%% <b>Type</b> : Identifier of the type (possibly none).
%% <b>Field</b> : Identifier of the field.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_record_access(MId,Argument,Type,Field) ->
    NewId = get_next_id(MId),
    create_record_access(MId,Argument,Type,Field,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% record_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_record_expr(MId::integer(),
%%          		    Argument::none() | integer(),
%%          		    Type::integer(),
%%          		    Fields::[integer()],
%%          		    Id::integer()) -> ok
%%
%% @doc
%% Creates a new record expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Argument</b> : Identifier of the argument (possibly none).
%% <b>Type</b> : Identifier of the type.
%% <b>Fields</b> : Identifiers of the fields.
%% <b>Id</b> : Identifier of the new record expression node.
%%</pre>
%% @end
%% Used: create_record_expr/4
%% =====================================================================
create_record_expr(MId,none,Type,Fields,Id) ->
    refactor_db:insert("insert into record_expr (mid,id,pos,argument) "
    			"values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ "0" ++"," ++ 
    			integer_to_list(Type) ++");"),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into record_expr "
    				   "(mid,id,pos,argument) values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Fields),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",40" ++ ");"),
    init_position(MId,Id);
create_record_expr(MId,Argument,Type,Fields,Id) ->
    refactor_db:insert("insert into record_expr (mid,id,pos,argument) "
    			"values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ "-1" ++","++ 
    			integer_to_list(Argument) ++");"),
    refactor_db:insert("insert into record_expr (mid,id,pos,argument) "
    			"values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ "0" ++"," ++ 
    			integer_to_list(Type) ++");"),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into record_expr "
    				   "(mid,id,pos,argument) values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Fields),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",40" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_record_expr(MId::integer(),
%%          		    Argument::none() | integer(),
%%          		    Type::integer(),
%%          		    Fields::[integer()]) -> integer()
%%
%% @doc
%% Creates a new record expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Argument</b> : Identifier of the argument (possibly none).
%% <b>Type</b> : Identifier of the type.
%% <b>Fields</b> : Identifiers of the fields.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
create_record_expr(MId,Argument,Type,Fields) ->
    NewId = get_next_id(MId),
    create_record_expr(MId,Argument,Type,Fields,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% record_field %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_record_field(MId::integer(),
%%          		     Name::integer(),
%%          		     Value::none() | integer(),
%%          		     Id::integer()) -> ok
%%
%% @doc
%% Creates a new record field into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : Identifier of the function.
%% <b>Value</b> : Identifier of the arguments (possibly none).
%% <b>Id</b> : Identifier of the new record field node.
%%</pre>
%% @end
%% Used: create_record_field/3
%% =====================================================================

create_record_field(MId,Name,none,Id) ->
    refactor_db:insert("insert into record_field (mid,id,name_id,value)"
    			" values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Name) ++", -1" ++");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",41" ++ ");"),
    init_position(MId,Id);
create_record_field(MId,Name,Value,Id) ->
    refactor_db:insert("insert into record_field (mid,id,name_id,value) "
    			"values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Name) ++","++ 
    			integer_to_list(Value) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",41" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_record_field(MId::integer(),
%%          		     Name::integer(),
%%          		     Value::none() | integer()) -> integer()
%%
%% @doc
%% Creates a new record field into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : Identifier of the function.
%% <b>Value</b> : Identifier of the arguments (possibly none).
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_record_field(MId,Name,Value) ->
    NewId = get_next_id(MId),
    create_record_field(MId,Name,Value,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% record_index_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_record_index_expr(MId::integer(),
%%          		          Type::integer(),
%%          		          Field::integer(),
%%          		          Id::integer()) -> ok
%%
%% @doc
%% Creates a new record index expression into the database, if the new 
%% id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Type</b> : Identifier of the type.
%% <b>Field</b> : Identifier of the field.
%% <b>Id</b> : Identifier of the new record index expression node.
%%</pre>
%% @end
%% Used: create_record_index_expr/3
%% =====================================================================
create_record_index_expr(MId,Type,Field,Id) ->
    refactor_db:insert("insert into record_index_expr (mid,id,type,field)"
    			" values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Type) ++","++ 
    			integer_to_list(Field) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",42" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_record_index_expr(MId::integer(),
%%          		          Type::integer(),
%%          		          Field::integer()) -> integer()
%%
%% @doc
%% Creates a new record index expression into the database, if the new 
%% id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Type</b> : Identifier of the type.
%% <b>Field</b> : Identifier of the field.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_record_index_expr(MId,Type,Field) ->
    NewId = get_next_id(MId),
    create_record_index_expr(MId,Type,Field,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% rule %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_rule(MId::integer(),
%%          	     Name::integer(),
%%          	     Clauses::[integer()],
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Creates a new rule into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : Identifier of the name.
%% <b>Clauses</b> : Identifiers of the clauses.
%% <b>Id</b> : Identifier of the new rule node.
%%</pre>
%% @end
%% Used: create_rule/3
%% =====================================================================
create_rule(MId,Name,Clauses,Id) ->
    refactor_db:insert("insert into rule (mid,id, pos, argument) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++", 0," ++ 
    			integer_to_list(Name) ++ ");"),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into rule (mid,id,pos,argument) "
    		 		   "values (" ++ 
    		 		   integer_to_list(MId) ++ "," ++ 
    		 		   integer_to_list(Id) ++"," ++ 
    		 		   integer_to_list(Pos) ++"," ++ 
    		 		   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Clauses),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",43" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_rule(MId::integer(),
%%          	     Name::integer(),
%%          	     Clauses::[integer()]) -> integer()
%%
%% @doc
%% Creates a new rule into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : Identifier of the name.
%% <b>Clauses</b> : Identifiers of the clauses.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_rule(MId,Name,Clauses) ->
    NewId = get_next_id(MId),
    create_rule(MId,Name,Clauses,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% size_qualifier %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_size_qualifier(MId::integer(),
%%          		       Body::integer(),
%%          		       Size::integer(),
%%          		       Id::integer()) -> ok
%%
%% @doc
%% Creates a new size qualifier into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Body</b> : Identifier of the body.
%% <b>Size</b> : Identifier of the size.
%% <b>Id</b> : Identifier of the new size qualifier node.
%%</pre>
%% @end
%% Used: create_size_qualifier/3
%% =====================================================================

create_size_qualifier(MId,Body,Size,Id) ->
    refactor_db:insert("insert into size_qualifier (mid,id,body,size) "
    			"values (" ++ integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(Body) ++","++ 
    			integer_to_list(Size) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",44" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_size_qualifier(MId::integer(),
%%          		       Body::integer(),
%%          		       Size::integer()) -> integer()
%%
%% @doc
%% Creates a new size qualifier into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Body</b> : Identifier of the body.
%% <b>Size</b> : Identifier of the size.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_size_qualifier(MId,Body,Size) ->
    NewId = get_next_id(MId),
    create_size_qualifier(MId,Body,Size,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% string %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_string(MId::integer(),
%%          	       Value::string(),
%%          	       Id::integer()) -> ok
%%
%% @doc
%% Creates a new string into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Value</b> : The value (string).
%% <b>Id</b> : Identifier of the new string node.
%%</pre>
%% @end
%% Used: create_string/2
%% =====================================================================
create_string(MId,Value,Id) ->
    refactor_db:insert("insert into string (mid,id,value) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ Value ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",45" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_string(MId::integer(),
%%          	       Value::string()) -> integer()
%%
%% @doc
%% Creates a new string into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Value</b> : The value (string).
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_string(MId,Value) ->
    NewId = get_next_id(MId),
    create_string(MId,Value,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% text %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_text(MId::integer(),
%%          	     String::string(),
%%          	     Id::integer()) -> ok
%%
%% @doc
%% Creates a new text into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>String</b> : The value (string).
%% <b>Id</b> : Identifier of the new text node.
%%</pre>
%% @end
%% Used: create_text/2
%% =====================================================================
create_text(MId,String,Id) ->
    refactor_db:insert("insert into text (mid,id,value) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			io_lib:write_string(String) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",46" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_text(MId::integer(),
%%          	     String::string()) -> integer()
%%
%% @doc
%% Creates a new text into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>String</b> : The value (string).
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_text(MId,String) ->
    NewId = get_next_id(MId),
    create_text(MId,String,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% try_expr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_try_expr(MId::integer(),
%%          		 Body::[integer()],
%%          		 Clauses::[integer()],
%%          		 Handlers::[integer()],
%%          		 After::[integer()],
%%          		 Id::integer()) -> ok
%%
%% @doc
%% Creates a new try expression into the database, if the new id is 
%% known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Body</b> : Identifiers of the bodies.
%% <b>Clauses</b> : Identifier of the clauses.
%% <b>Handlers</b> : Identifier of the handlers.
%% <b>After</b> : Identifier of the afters.
%% <b>Id</b> : Identifier of the new try expression node.
%%</pre>
%% @end
%% Used: create_try_expr/5
%% =====================================================================
create_try_expr(MId,Body,Clauses,Handlers,After,Id) ->
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into try_expr "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++ "," ++ 
				   integer_to_list(Pos) ++"," ++"0," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1, Body),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into try_expr "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++"," ++ 
				   integer_to_list(Pos) ++"," ++"1," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Clauses),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into try_expr "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++ "," ++ 
				   integer_to_list(Pos) ++"," ++"2," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Handlers),
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into try_expr "
    				   "(mid,id,pos,qualifier,argument) values (" 
				   ++ integer_to_list(MId) ++ "," ++ 
				   integer_to_list(Id) ++ "," ++ 
				   integer_to_list(Pos) ++"," ++"3," ++ 
				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,After),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",47" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_try_expr(MId::integer(),
%%          		 Body::[integer()],
%%          		 Clauses::[integer()],
%%          		 Handlers::[integer()],
%%          		 After::[integer()]) -> integer()
%%
%% @doc
%% Creates a new try expression into the database, if the new id is 
%% not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Body</b> : Identifiers of the bodies.
%% <b>Clauses</b> : Identifier of the clauses.
%% <b>Handlers</b> : Identifier of the handlers.
%% <b>After</b> : Identifier of the afters.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_try_expr(MId,Body,Clauses,Handlers,After) ->
    NewId = get_next_id(MId),
    create_try_expr(MId,Body,Clauses,Handlers,After,NewId),
    NewId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% tuple %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_tuple(MId::integer(),
%%          	      Elements::[integer()],
%%          	      Id::integer()) -> ok
%%
%% @doc
%% Creates a new tuple into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Elements</b> : Identifiers of the elements.
%% <b>Id</b> : Identifier of the new tuple node.
%%</pre>
%% @end
%% Used: create_tuple/2
%% =====================================================================
create_tuple(MId,Elements,Id)  ->
    lists:foldl(fun(Element, Pos)-> 
    		refactor_db:insert("insert into tuple (mid,id,pos,element) "
    				   "values (" ++ 
    				   integer_to_list(MId) ++ "," ++ 
    				   integer_to_list(Id) ++"," ++ 
    				   integer_to_list(Pos) ++"," ++ 
    				   integer_to_list(Element) ++ ");") ,
		Pos+1 end ,1,Elements),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",48" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_tuple(MId::integer(),
%%          	      Elements::[integer()]) -> integer()
%%
%% @doc
%% Creates a new tuple into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Elements</b> : Identifiers of the elements.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

create_tuple(MId,Elements) ->
    NewId = get_next_id(MId),
    create_tuple(MId,Elements,NewId),
    NewId. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% underscore %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_underscore(MId::integer(),
%%          		   Id::integer()) -> ok
%%
%% @doc
%% Creates a new underscore into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Identifier of the new underscore node.
%%</pre>
%% @end
%% Used: create_underscore/1
%% =====================================================================

create_underscore(MId,Id) ->
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",49" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_underscore(MId::integer()) -> integer()
%%
%% @doc
%% Creates a new underscore into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%%</pre>
%% @end
%% Used: create_underscore/1
%% =====================================================================


create_underscore(MId) ->
    NewId = get_next_id(MId),
    create_underscore(MId,NewId),
    NewId.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% variable %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec create_variable(MId::integer(),
%%          		 Name::string(),
%%          		 Id::integer()) -> ok
%%
%% @doc
%% Creates a new variable into the database, if the new id is known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : String of the name.
%% <b>Id</b> : Identifier of the new variable node.
%%</pre>
%% @end
%% Used: create_variable/2
%% =====================================================================

create_variable(MId,Name,Id)  ->
    refactor_db:insert("insert into name (mid,id,name) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++"," ++ 
    			io_lib:write_string(Name) ++ ");"),
    refactor_db:insert("insert into node_type (mid,id,type) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++",50" ++ ");"),
    init_position(MId,Id).

%% =====================================================================
%% @spec create_variable(MId::integer(),
%%          		 Name::string()) -> integer()
%%
%% @doc
%% Creates a new variable into the database, if the new id is not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : String of the name.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_variable(MId,Name) ->
    NewId = get_next_id(MId),
    create_variable(MId,Name,NewId),
    NewId.
    
%% =====================================================================
%% @spec create_variables(MId::integer(),
%%          		  Name::string(),
%%          		  Num::integer(),
%%                        Arity::integer()) -> [integer()]
%%
%% @doc
%% Creates a new variables into the database, if the new ids are not known.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Name</b> : String of the name.
%% <b>Num</b> : Number (part of the variable name).
%% <b>Arity</b> : Number of the needed variables.
%%</pre>
%% @end
%% Used: 
%% =====================================================================
    

create_variables(_MId,_Name,_Num,0)->
    [];
create_variables(MId,Name,Num,Arity)->
   [create_variable(MId,Name++integer_to_list(Num)) | 
   create_variables(MId,Name,Num+1,Arity-1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Utility functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% =====================================================================
%% @spec get_next_id(MId::integer()) -> integer()
%%
%% @doc
%% Gets the next possible free identifier in the module and returns 
%% with it.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


get_next_id(MId) ->
    [{Num}] = refactor_db:select("select num from id_count where mid=" ++  
    			 	 integer_to_list(MId) ++ ";"),
    refactor_db:update("update id_count set num=num+1 where mid=" ++  
    			integer_to_list(MId) ++ ";"),
    Num+1.

%% =====================================================================
%% @spec create_fresh_variable(MId::integer()) -> {string(),integer()}
%%
%% @doc
%% Creates a new unique variable name with a string and a number, and 
%% returns with this two parts in a tuple.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_fresh_variable(MId) ->
    case get_max_variable_length(MId) of
        null -> Length = "0";
        Length -> ok
    end,
    case list_to_integer(Length) =< 9 of
	true ->
	    { "RefacVar_", 1 };
	false ->
	    { "RefacVar_" ,((list_to_integer(Length)-9)*10)}
    end.


%% =====================================================================
%% @spec get_max_variable_length(MId::integer()) -> integer()
%%
%% @doc
%% Gets the length of the longest variable in the module.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%%</pre>
%% @end
%% Used: create_fresh_variable/1
%% =====================================================================

get_max_variable_length(MId) ->
    [{Length}] = refactor_db:select( "select max(length(name)) from "
    				     "name as n, node_type as no"
				      " where n.mid=no.mid and n.mid=" ++  
				      integer_to_list(MId) ++ 
				      " and n.id=no.id and no.type=50;"),
    Length.

%% =====================================================================
%% @spec init_position(MId::integer(),
%%                     Ids::[integer()] | integer()) -> ok
%%
%% @doc
%% Inits the positions of the identifier(s in the list).
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Ids</b> : Id or list of ids of the element(s) which positions has
%% to be inited.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


init_position(_MId,[]) -> ok;
init_position(MId,[Id | Xs]) ->
    refactor_db:insert("insert into pos (mid,id,line,col) values (" 
		       ++  integer_to_list(MId) ++ "," ++ 
		       integer_to_list(Id) ++ ",0,0);"),
    init_position(MId,Xs);
init_position(MId,Id) ->
    refactor_db:insert("insert into pos (mid,id,line,col) values (" 
			++  integer_to_list(MId) ++ "," ++ 
			integer_to_list(Id) ++ ",0,0);").

%% =====================================================================
%% @spec init_scope(MId::integer(),
%%		    Scope::integer(),
%%                  Ids::[integer()] | integer()) -> ok
%%
%% @doc
%% Inits the scopes of the identifier(s in the list) within the given scope.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Scope</b> : Indentifier of the scope.
%% <b>Ids</b> : Id or list of ids of the element(s) which scopes has
%% to be inited.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


init_scope(_MId,_ScopeId,[]) ->
    ok;
init_scope(MId,ScopeId,[Id | Xs]) ->
    refactor_db:insert("insert into scope (mid,id,scope) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(ScopeId) ++ ");"),
    init_scope(MId,ScopeId,Xs);
init_scope(MId,ScopeId,Id) ->
    refactor_db:insert("insert into scope (mid,id,scope) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Id) ++ "," ++ 
    			integer_to_list(ScopeId) ++ ");").

%% =====================================================================
%% @spec create_scope(MId::integer(),
%%		      NewScope::integer(),
%%                    ScopeId::integer()) -> ok
%%
%% @doc
%% Creates a new scope within the given scope.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NewScope</b> : Indentifier of the new scope.
%% <b>ScopeId</b> : Indentifier of the existing scope.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


create_scope(MId,NewScope,ScopeId) ->
    refactor_db:insert( "insert into scope_visib (mid,id,target) values (" ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(NewScope) ++ "," ++ 
    			integer_to_list(ScopeId) ++ ");").

%% =====================================================================
%% @spec connect_fun_call(MId2::integer(),
%%		          Id::integer(),
%%		          MId::integer(),
%%                        FunId::integer()) -> ok
%%
%% @doc
%% Connects the function call (MId2, Id) to the target function.
%% 
%% Parameter description:<pre>
%% <b>MId2</b> : Id of the module.
%% <b>Id</b> : Indentifier of the function call.
%% <b>MId</b> : Indentifier of the target module.
%% <b>FunId</b> : Indentifier of the target function.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


connect_fun_call(MId2,Id,MId,FunId) ->
    refactor_db:insert("insert into fun_call (mid,id,tmid,target) values (" ++
    			integer_to_list(MId2) ++ "," ++ 
    			integer_to_list(Id) ++"," ++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(FunId) ++ ");").

%% =====================================================================
%% @spec connect_variables(MId::integer(),
%%		          Ids::[integer()],
%%                        TIds::[integer()]) -> ok
%%
%% @doc
%% Connects the function call (MId2, Id) to the target function.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Ids</b> : Indentifiers of the variables.
%% <b>TIds</b> : Indentifiers of the target variables.
%%</pre>
%% @end
%% Used: 
%% =====================================================================


connect_variables(_MId,[],[]) ->
    ok;
connect_variables(MId,[X | Xs],[Y | Ys]) ->
    refactor_db:insert("insert into var_visib (mid,id,target) values ("++ 
    			integer_to_list(MId) ++ "," ++ 
    			integer_to_list(Y) ++ "," ++ 
    			integer_to_list(X) ++ ");"),
    connect_variables(MId,Xs,Ys).

%% =====================================================================
%% @spec conv_from_ast_to_id(Nodes::[term()] | term() | none()) -> 
%%				[integer] | [] | integer() | none()  
%%
%% @doc
%% Converts the tree node to database id.
%% 
%% Parameter description:<pre>
%% <b>Nodes</b> : List of syntax tree nodes.
%%</pre>
%% @end
%% Used: 
%% =====================================================================

conv_from_ast_to_id([]) ->
    [];
conv_from_ast_to_id([X|Xs]) ->
    [hd(erl_syntax:get_ann(X))|conv_from_ast_to_id(Xs)];
conv_from_ast_to_id(none) ->
    none;
conv_from_ast_to_id(X) ->
    hd(erl_syntax:get_ann(X)).

%% =====================================================================
%% @spec attach_subtree_to_node(MId::integer(), Id::integer(), 
%%                     To::integer(), After::integer()) -> ok
%%
%% @doc
%% Attaches the element to a new root, after a specified child.
%% If After is 0 this node will be the leftmost child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%% <b>To</b> : Id of the root node.
%% <b>After</b> : Id of a child node.
%%</pre>
%% @end
%% =====================================================================    
attach_subtree_to_node(MId, Id, To, After) ->
    Type = erl_syntax_db:type(MId, To),
    attach_subtree_to_node(MId, Id, To, After, Type).

%% =====================================================================
%% @spec attach_subtree_to_node(MId::integer(), Id::integer(), 
%%            To::integer(), After::integer(), Type::integer()) -> ok
%%
%% @doc
%% Attaches the element to a new root, after a specified child. 
%% If After is 0 this node will be the leftmost child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%% <b>To</b> : Id of the root node.
%% <b>After</b> : Id of a child node.
%% <b>Type</b> : Type of the root node.
%%</pre>
%% @end
%% =====================================================================    
attach_subtree_to_node(MId, Id, To, After, ?FORM_LIST) ->
    if 
        After =:= 0 ->
            Pos = 0;
        true ->
            [{Pos}] = 
                refactor_db:select(
                  "select pos from form_list where mid=" ++ integer_to_list(MId) 
                  ++ " and id=" ++ integer_to_list(To) ++ " and form=" 
                  ++ integer_to_list(After) ++ " ;")
    end,
    [{MaxPos}] = 
        refactor_db:select(
          "select max(pos) from form_list where mid=" ++ integer_to_list(MId) 
          ++ " and id=" ++ integer_to_list(To) ++ " ;"),
    if 
        Pos /= MaxPos ->
            lists:foreach(
              fun (Pos2) ->
                      refactor_db:update(
                        "update form_list set pos=pos+1 where mid=" 
                        ++ integer_to_list(MId) 
                        ++ " and id= " ++ integer_to_list(To) ++ " and pos=" 
                        ++ integer_to_list(Pos2) ++ " ;")
              end, lists:seq(MaxPos, Pos+1, -1));
        true ->
            ok
    end,
    refactor_db:insert(
      "insert into form_list(mid, id, pos, form) values (" 
      ++ integer_to_list(MId) ++ ", " ++ integer_to_list(To)++ ", " 
      ++ integer_to_list(Pos+1) ++ ", " ++ integer_to_list(Id)++ ");");
attach_subtree_to_node(MId, Id, To, After, ?CLAUSE) ->
    if 
        After == 0 ->
            {Pos, Qualifier} = {0,2};
        true ->
            [{Pos, Qualifier}] = 
                refactor_db:select(
                  "select pos, qualifier from clause where mid=" 
                  ++ integer_to_list(MId) 
                  ++ " and id=" ++ integer_to_list(To) ++ " and argument=" 
                  ++ integer_to_list(After) ++";")
    end,
    [{MaxPos}] = 
        refactor_db:select(
          "select max(pos) from clause where mid=" 
          ++ integer_to_list(MId) 
          ++ " and id=" ++ integer_to_list(To) ++ " and qualifier=" 
          ++ integer_to_list(Qualifier) ++ " ;"),
    if 
        Pos /= MaxPos ->
            lists:foreach( 
              fun (Pos2) ->
                      refactor_db:update(
                        "update clause set pos=pos+1 where mid=" 
                        ++ integer_to_list(MId) 
                        ++ " and id= " ++ integer_to_list(To) ++ " and pos=" 
                        ++ integer_to_list(Pos2) ++ 
                        " and qualifier=" ++ integer_to_list(Qualifier) 
                        ++ " ;")
              end, lists:seq(MaxPos, Pos+1, -1));
        true ->
            ok
    end,
    refactor_db:insert(
      "insert into clause (mid, id, pos, qualifier, argument) values (" 
      ++ integer_to_list(MId) ++ ", " ++ integer_to_list(To)++ ", " 
      ++ integer_to_list(Pos+1) ++ ", " ++ integer_to_list(Qualifier) ++ ", " 
      ++ integer_to_list(Id)++ ");");
attach_subtree_to_node(MId, Id, To, _After, ?MATCH_EXPR) ->
    [{Pattern, Body}] = 
        refactor_db:select(
          "select pattern,body from match_expr where mid=" ++ integer_to_list(MId) 
          ++ " and id=" ++ integer_to_list(To) ++ ";"),
    if 
        Pattern == 0 ->
            refactor_db:update(
              "update match_expr set pattern=" ++ integer_to_list(Id) 
              ++ " where mid=" ++ integer_to_list(MId)
              ++ " and id=" ++ integer_to_list(To) ++  ";");
        Body == 0 ->
            refactor_db:update(
              "update match_expr set body=" ++ integer_to_list(Id) 
              ++ " where mid=" ++ integer_to_list(MId)
              ++ " and id="++ integer_to_list(To) ++  ";");
        true ->
            ok
    end;
attach_subtree_to_node(MId, Id, To, After, ?APPLICATION) ->
    if 
        After =:= 0 ->
            Pos = 0;
        true ->
            [{Pos}] = 
                refactor_db:select(
                  "select pos from application where mid=" 
                  ++ integer_to_list(MId) 
                  ++ " and id=" ++ integer_to_list(To) ++ " and argument=" 
                  ++ integer_to_list(After) ++ " ;")
    end,
    [{MaxPos}] = 
        refactor_db:select(
          "select max(pos) from application where mid=" ++ integer_to_list(MId) 
          ++ " and id=" ++ integer_to_list(To) ++ " ;"),
    if 
        Pos /= MaxPos ->
            lists:foreach(
              fun (Pos2) ->
                      refactor_db:update(
                        "update application set pos=pos+1 where mid=" 
                        ++ integer_to_list(MId) 
                        ++ " and id= " ++ integer_to_list(To) ++ " and pos=" 
                        ++ integer_to_list(Pos2) ++ " ;")
              end, lists:seq(MaxPos, Pos+1, -1));
        true ->
            ok
    end,
    refactor_db:insert(
      "insert into application(mid, id, pos, argument) values (" 
      ++ integer_to_list(MId) ++ ", " ++ integer_to_list(To)++ ", " 
      ++ integer_to_list(Pos+1) ++ ", " ++ integer_to_list(Id)++ ");");
attach_subtree_to_node(_MId, _Id, _To, _After, _Type) ->
    error_logger:info_msg("Attach not implemented for this node_type yet.").

%% =====================================================================
%% @spec move_node(MId::integer(), Id::integer()) -> integer()
%%
%% @doc
%% Changes a node id. Therefore making it possible to insert a new 
%% elements between it and it's parent without knowing the parent's id.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%%</pre>
%% @end
%% =====================================================================    
move_node(MId, Id) ->
    move_node(MId, Id, erl_syntax_db:type(MId, Id)).

%% =====================================================================
%% @spec move_node(MId::integer(), Id::integer(), Type::integer()
%%                    ) -> integer()
%%
%% @doc
%% Changes a node id. Therefore making it possible to insert a new 
%% elements between it and it's parent without knowing the parent's id.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node.
%% <b>Type</b> : Type of the node.
%%</pre>
%% @end
%% =====================================================================    
move_node(MId, Id, ?VARIABLE) ->
    Name = erl_syntax_db:variable_literal(MId, Id),
    NewId = create_nodes:create_variable(MId, Name),
    create_nodes:init_scope(
      MId, refactor:get_scope_from_id(MId, Id), [NewId]),
    refactor_db:update("update var_visib set id=" ++ integer_to_list(NewId) 
                       ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
                       ++ integer_to_list(Id) ++ ";"),
    refactor_db:update("update var_visib set target=" ++ integer_to_list(NewId) 
                       ++ " where mid=" ++ integer_to_list(MId) 
                       ++ " and target=" ++ integer_to_list(Id) ++ ";"),
    delete_nodes:delete_node(MId, Id),
    NewId;
move_node(MId, Id, ?APPLICATION) ->
    NameId = erl_syntax_db:application_operator(MId, Id),
    Arguments = erl_syntax_db:application_arguments(MId, Id),
    NewId = create_nodes:create_application(MId, NameId, Arguments),
    create_nodes:init_scope(
      MId, refactor:get_scope_from_id(MId, Id), [NewId]),
    lists:map(
      fun (Element) -> delete_nodes:detach_node(MId, Element, Id) end,
      [NameId] ++ Arguments),
    refactor_db:update("update fun_call set id=" ++ integer_to_list(NewId) 
                       ++ " where mid=" ++ integer_to_list(MId) ++ " and id=" 
                       ++ integer_to_list(Id) ++ ";"),
    delete_nodes:delete_node(MId, Id),
    NewId.
