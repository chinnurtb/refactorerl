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

%% @doc This module implements a great deal of the erl_syntax module's functions 
%% for the database approach. These functions directly use the database.
%% @end

-module(erl_syntax_db).

-export([application_operator/2, application_arguments/2, 
	 arity_qualifier_body/2, arity_qualifier_argument/2, 
	 atom_name/2, attribute_arguments/2, attribute_name/2, 
	 binary_fields/2, binary_field_types/2, binary_field_body/2,
	 block_expr_body/2, case_expr_argument/2, case_expr_clauses/2,
	 catch_expr_body/2, char_value/2, class_qualifier_argument/2,
	 class_qualifier_body/2, clause_guard/2, clause_patterns/2,
	 clause_body/2, comment_padding/2, comment_text/2, 
	 cond_expr_clauses/2, conjunction_body/2, disjunction_body/2,
	 float_value/2, form_list_elements/2, fun_expr_clauses/2,
	 function_name/2, function_clauses/2, generator_pattern/2,
	 generator_body/2, if_expr_clauses/2, implicit_fun_name/2, 
	 infix_expr_left/2, infix_expr_operator/2, infix_expr_right/2,
	 integer_value/2, list_suffix/2, list_prefix/2, list_comp_template/2,
	 list_comp_body/2, macro_arguments/2, macro_name/2, 
	 match_expr_pattern/2, match_expr_body/2, module_qualifier_argument/2,
	 module_qualifier_body/2, operator_literal/2, parentheses_body/2, 
	 prefix_expr_operator/2, prefix_expr_argument/2, 
	 qualified_name_segments/2, query_expr_body/2, receive_expr_timeout/2,
	 receive_expr_clauses/2, receive_expr_action/2, record_access_type/2,
	 record_access_argument/2, record_access_field/2, 
	 record_expr_argument/2, record_expr_type/2, record_expr_fields/2,
	 record_field_value/2, record_field_name/2, record_index_expr_type/2, 
	 record_index_expr_field/2, rule_name/2, rule_clauses/2, 
	 size_qualifier_body/2, size_qualifier_argument/2, string_value/2, 
	 text_string/2, try_expr_body/2, try_expr_clauses/2, 
	 try_expr_handlers/2, try_expr_after/2, tuple_elements/2,
	 variable_literal/2, subtrees/2, type/2]).

-export([untuple/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec application_operator(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the  application node's  operator child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
application_operator(MId, Id) ->
    [{Operator}] = refactor_db:select(
		     "select argument from application where mid=" 
		     ++ integer_to_list(MId) ++ " and id=" 
		     ++ integer_to_list(Id) ++ " and pos=0;" ),
    Operator.

%% =====================================================================
%% @spec application_arguments(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the application node's argument children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
application_arguments(MId, Id) ->
    Arguments = refactor_db:select(
		  "select argument from application where mid=" 
		  ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
		  ++ " and pos!=0;" ),
    untuple(Arguments).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec arity_qualifier_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the arity_qualifier node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
arity_qualifier_body(MId, Id) ->
    [{Body}] = refactor_db:select(
		 "select body from arity_qualifier where mid=" 
		 ++ integer_to_list(MId) ++ " and id=" ++ integer_to_list(Id) 
		 ++ ";" ),
    Body.

%% =====================================================================
%% @spec arity_qualifier_argument(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the arity_qualifier node's argument child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
arity_qualifier_argument(MId, Id) ->
    [{Arity}] = refactor_db:select(
		  "select arity from arity_qualifier where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ ";" ),
    Arity.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec atom_name(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the atom node's name child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
atom_name(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select name from name where mid=" ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ ";" ),
    Name.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec attribute_arguments(MId::integer(), 
%%            Id::integer()) -> [integer()] | none
%%
%% @doc
%% Returns the attribute node's argument children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
attribute_arguments(MId, Id) ->
    Arguments = refactor_db:select(
		  "select argument from attribute_ where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    case Arguments == [] of
	true ->
	    none;
	false ->
	    untuple(Arguments)
    end.

%% =====================================================================
%% @spec attribute_name(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the attribute node's name child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
attribute_name(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select argument from attribute_ where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    Name.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec binary_fields(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the binary node's field children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
binary_fields(MId, Id) ->
    Fields = refactor_db:select(
	       "select field from binary_ where mid=" ++ integer_to_list(MId) 
	       ++ " and id=" ++ integer_to_list(Id) ++ ";" ),
    untuple(Fields).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec binary_field_types(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the binary_field node's type children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
binary_field_types(MId, Id) ->
    Arguments = refactor_db:select(
		  "select argument from binary_field where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    untuple(Arguments).

%% =====================================================================
%% @spec binary_field_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the binary_field node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
binary_field_body(MId, Id) ->    
    [{Body}] = refactor_db:select(
		 "select argument from binary_field where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    Body.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec block_expr_body(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the block_expr node's body children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
block_expr_body(MId, Id) ->
    Body = refactor_db:select(
	     "select body from block_expr where mid=" ++ integer_to_list(MId) 
	     ++ " and id=" ++ integer_to_list(Id) ++ ";" ),
    untuple(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec (MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the  node's  child/children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
case_expr_argument(MId, Id) ->    
    [{Argument}] = refactor_db:select(
		     "select argument from case_expr where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    Argument.

%% =====================================================================
%% @spec case_expr_clauses(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the  case_expr node's clause children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
case_expr_clauses(MId, Id) ->
    Clauses = refactor_db:select(
		"select argument from case_expr where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    untuple(Clauses).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec catch_expr_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the catch_expr node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
catch_expr_body(MId, Id) ->
    [{Body}] = refactor_db:select(
		 "select expression from catch_expr where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ ";" ),
    Body.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec char_value(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the char node's value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
char_value(MId, Id) ->
    [{Value}] = refactor_db:select(
		  "select value from char_ where mid=" ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ ";" ),
    Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec class_qualifier_argument(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the class_qualifier node's argument child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
class_qualifier_argument(MId, Id) ->
    [{Argument}] = refactor_db:select(
		     "select class from class_qualifier where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ ";" ),
    Argument.    

%% =====================================================================
%% @spec class_qualifier_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the class_qualifier node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
class_qualifier_body(MId, Id) ->
    [{Body}] = refactor_db:select(
		 "select body from class_qualifier where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ ";" ),
    Body.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec clause_guard(MId::integer(), 
%%            Id::integer()) -> integer() | none
%%
%% @doc
%% Returns the clause node's guard guard child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
clause_guard(MId, Id) ->
    Guard = refactor_db:select(
	      "select argument from clause where mid=" ++ integer_to_list(MId) 
	      ++ " and id=" ++ integer_to_list(Id) 
	      ++ " and qualifier=1 and pos=1;" ),
    case Guard == [] of
	true ->
	    none;    
	false ->
	    element(1,hd(Guard))
    end.

%% =====================================================================
%% @spec clause_patterns(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the clause node's pattern children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
clause_patterns(MId, Id) ->
    Patterns = refactor_db:select(
		 "select argument from clause where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=0;" ),
    untuple(Patterns).

%% =====================================================================
%% @spec clause_body(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the clause node's body children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
clause_body(MId, Id) ->
    Body = refactor_db:select(
	     "select argument from clause where mid=" ++ integer_to_list(MId) 
	     ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=2;" ),
    untuple(Body).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec comment_padding(MId::integer(), 
%%            Id::integer()) -> integer() | none
%%
%% @doc
%% Returns the comment node's padding value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
comment_padding(MId, Id) ->
    Padding = refactor_db:select(
		"select argument from comment where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    case Padding == [] of
	true ->
	    none;    
	false ->
	    list_to_integer(element(1,hd(Padding)))
    end.    

%% =====================================================================
%% @spec comment_text(MId::integer(), 
%%            Id::integer()) -> [string()]
%%
%% @doc
%% Returns the comment node's text.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
comment_text(MId, Id) ->
    Text = refactor_db:select(
	     "select argument from comment where mid=" ++ integer_to_list(MId) 
	     ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    untuple(Text).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec (MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the  node's  child/children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
cond_expr_clauses(MId, Id) ->
    Clauses = refactor_db:select(
		"select clause from cond_expr where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    untuple(Clauses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec conjunction_body(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the conjunction node's body children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
conjunction_body(MId, Id) ->    
    Body = refactor_db:select(
	     "select argument from conjunction where mid=" 
	     ++ integer_to_list(MId) 
	     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    untuple(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec disjunction_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the disjunction node's body children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
disjunction_body(MId, Id) ->
    Body = refactor_db:select(
	     "select argument from disjunction where mid=" 
	     ++ integer_to_list(MId) 
	     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    untuple(Body).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec float_value(MId::integer(), 
%%            Id::integer()) -> float()
%%
%% @doc
%% Returns the float node's value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
float_value(MId, Id) ->
    [{Value}] = refactor_db:select(
		  "select value from float_ where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec form_list_elements(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the form_list node's children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
form_list_elements(MId, Id) ->
    Body = refactor_db:select(
	     "select form from form_list where mid=" ++ integer_to_list(MId) 
	     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    untuple(Body).     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec fun_expr_clauses(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the fun_expr node's clause children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
fun_expr_clauses(MId, Id) ->
    Clauses = refactor_db:select(
		"select clause from fun_expr where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    untuple(Clauses).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec function_name(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the function node's name child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
function_name(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select clause from function where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    Name.     

%% =====================================================================
%% @spec function_clauses(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the function node's clause children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
function_clauses(MId, Id) ->
    Clauses = refactor_db:select(
		"select clause from function where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    untuple(Clauses).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec generator_pattern(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the generator node's pattern child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
generator_pattern(MId, Id) -> 
    [{Pattern}] = refactor_db:select(
		    "select pattern from generator where mid=" 
		    ++ integer_to_list(MId) 
		    ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Pattern.
    
%% =====================================================================
%% @spec generator_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the generator node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
generator_body(MId, Id) ->
    [{Body}] = refactor_db:select(
		 "select body from generator where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Body.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec if_expr_clauses(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the if_expr node's clause children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
if_expr_clauses(MId, Id) ->
    Clauses = refactor_db:select(
		"select clause from if_expr where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    untuple(Clauses).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec implicit_fun_name(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the implicit_fun node's name child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
implicit_fun_name(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select name_id from implicit_fun where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Name. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec infix_expr_left(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the infix_expr node's left child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
infix_expr_left(MId, Id) ->
    [{Left}] = refactor_db:select(
		 "select lft from infix_expr where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Left. 

%% =====================================================================
%% @spec infix_expr_operator(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the infix_expr node's operator child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
infix_expr_operator(MId, Id) ->
    [{Operator}] = refactor_db:select(
		     "select oper from infix_expr where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Operator. 

%% =====================================================================
%% @spec infix_expr_right(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the infix_expr node's right child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
infix_expr_right(MId, Id) ->
    [{Right}] = refactor_db:select(
		  "select rght from infix_expr where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Right.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec integer_value(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the integer node's value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
integer_value(MId, Id) ->
    [{Value}] = refactor_db:select(
		  "select value from integer_ where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " ;"),
    list_to_integer(Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec list_suffix(MId::integer(), 
%%            Id::integer()) -> integer() | none
%%
%% @doc
%% Returns the list node's suffix child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
list_suffix(MId, Id) ->
    Suffix = refactor_db:select(
	       "select element from list where mid=" ++ integer_to_list(MId) 
	       ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    case Suffix == [] of
	true ->
	    none;
	false ->
	    element(1,hd(Suffix))
    end.

%% =====================================================================
%% @spec list_prefix(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the list node's prefix children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
list_prefix(MId, Id) ->
    Prefix = refactor_db:select(
	       "select element from list where mid=" ++ integer_to_list(MId) 
	       ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    untuple(Prefix).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec list_comp_template(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the list_comp node's template child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
list_comp_template(MId, Id) -> 
    [{Template}] = refactor_db:select(
		     "select argument from list_comp where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    Template.

%% =====================================================================
%% @spec list_comp_body(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the list_comp node's body children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
list_comp_body(MId, Id) ->
    Body = refactor_db:select(
	     "select argument from list_comp where mid=" 
	     ++ integer_to_list(MId) 
	     ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    untuple(Body).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec macro_arguments(MId::integer(), 
%%            Id::integer()) -> [integer()] | none
%%
%% @doc
%% Returns the macro node's argument children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
macro_arguments(MId, Id) ->
    Arguments = refactor_db:select(
		  "select argument from macro where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    case Arguments == [] of
	true ->
	    none;
	false ->
	    untuple(Arguments)
    end.

%% =====================================================================
%% @spec macro_name(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the macro node's name child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
macro_name(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select argument from macro where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),    
    Name.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec match_expr_pattern(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the match_expr node's pattern child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
match_expr_pattern(MId, Id) ->
    [{Pattern}] = refactor_db:select(
		    "select pattern from match_expr where mid=" 
		    ++ integer_to_list(MId) 
		    ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Pattern.

%% =====================================================================
%% @spec match_expr_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the match_expr node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
match_expr_body(MId, Id) ->
    [{Body}] = refactor_db:select(
		 "select body from match_expr where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Body.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec module_qualifier_argument(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the module_qualifier node's argument child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
module_qualifier_argument(MId, Id) ->
    [{Argument}] = refactor_db:select(
		     "select module from module_qualifier where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Argument.

%% =====================================================================
%% @spec module_qualifier_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the module_qualifier node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
module_qualifier_body(MId, Id) ->
    [{Body}] = refactor_db:select(
		 "select body from module_qualifier where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Body.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec operator_literal(MId::integer(), 
%%            Id::integer()) -> string()
%%
%% @doc
%% Returns the operator node's  value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
operator_literal(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select name from name where mid=" ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Name.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec parentheses_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the parentheses node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
parentheses_body(MId, Id) ->    
    [{Body}] = refactor_db:select(
		 "select body from parentheses where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Body.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec prefix_expr_operator(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the prefix_expr node's operator child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
prefix_expr_operator(MId, Id) ->
    [{Operator}] = refactor_db:select(
		     "select operator from prefix_expr where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Operator.

%% =====================================================================
%% @spec prefix_expr_argument(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the prefix_expr node's argument child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
prefix_expr_argument(MId, Id) ->
    [{Argument}] = refactor_db:select(
		     "select argument from prefix_expr where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Argument.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec qualified_name_segments(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the qualified_name node's segment children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
qualified_name_segments(MId, Id) ->
    Segments = refactor_db:select(
		 "select segment from qualified_name where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    untuple(Segments).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec query_expr_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the query_expr node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
query_expr_body(MId, Id) ->
    [{Body}] = refactor_db:select(
		 "select body from query_expr where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    Body.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec receive_expr_timeout(MId::integer(), 
%%            Id::integer()) -> integer() | none
%%
%% @doc
%% Returns the receive_expr node's timeout child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
receive_expr_timeout(MId, Id) ->
    TimeOut = refactor_db:select(
		"select argument from receive_expr where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=1;" ),
    case TimeOut == [] of
	true ->
	    none;
	false ->
	    element(1,hd(TimeOut))
    end.

%% =====================================================================
%% @spec receive_expr_clauses(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the receive_expr node's clause children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
receive_expr_clauses(MId, Id) ->
    Clauses = refactor_db:select(
		"select argument from receive_expr where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=0;" ),
    untuple(Clauses).

%% =====================================================================
%% @spec receive_expr_action(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the receive_expr node's action children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
receive_expr_action(MId, Id) ->
    Action = refactor_db:select(
	       "select argument from receive_expr where mid=" 
	       ++ integer_to_list(MId) 
	       ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=2;" ),
    untuple(Action).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec record_access_type(MId::integer(), 
%%            Id::integer()) -> integer() | none
%%
%% @doc
%% Returns the record_access node's type child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_access_type(MId, Id) ->
    [{Type}] = refactor_db:select(
		 "select type from record_access where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),    
    case Type == -1 of
	true ->
	    none;
	false ->
	    Type
    end.


%% =====================================================================
%% @spec record_access_argument(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the record_access node's argument child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_access_argument(MId, Id) ->
    [{Argument}] = refactor_db:select(
		     "select argument from record_access where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Argument.

%% =====================================================================
%% @spec record_access_field(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the record_access node's field child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_access_field(MId, Id) ->
    [{Field}] = refactor_db:select(
		  "select field from record_access where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Field.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec record_expr_argument(MId::integer(), 
%%            Id::integer()) -> integer() | none
%%
%% @doc
%% Returns the record_expr node's argument child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_expr_argument(MId, Id) ->
    Argument = refactor_db:select(
		 "select argument from record_expr where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and pos=-1;" ),
    case Argument == [] of
	true ->
	    none;
	false ->
	    element(1,hd(Argument))
    end. 


%% =====================================================================
%% @spec record_expr_type(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the record_expr node's type child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_expr_type(MId, Id) ->
    [{Type}] = refactor_db:select(
		 "select argument from record_expr where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    Type.


%% =====================================================================
%% @spec record_expr_fields(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the record_expr node's field children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_expr_fields(MId, Id) ->
    Fields = refactor_db:select(
	       "select argument from record_expr where mid=" 
	       ++ integer_to_list(MId) 
	       ++ " and id=" ++ integer_to_list(Id) 
	       ++ " and pos!=0 and pos!=-1;" ),
    untuple(Fields).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec record_field_value(MId::integer(), 
%%            Id::integer()) -> integer() | none
%%
%% @doc
%% Returns the record_field node's value child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_field_value(MId, Id) ->
    [{Value}] = refactor_db:select(
		  "select value from record_field where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    case Value == -1 of
	true ->
	    none;
	false ->
	    Value
    end.

%% =====================================================================
%% @spec record_field_name(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the record_field node's name child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_field_name(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select name_id from record_field where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Name.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec record_index_expr_type(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the record_index_expr node's type child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_index_expr_type(MId, Id) ->
    [{Type}] = refactor_db:select(
		 "select type from record_index_expr where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Type.


%% =====================================================================
%% @spec record_index_expr_field(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the record_index_expr node's field child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
record_index_expr_field(MId, Id) ->
    [{Field}] = refactor_db:select(
		  "select field from record_index_expr where mid=" 
		  ++ integer_to_list(MId) 
		  ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Field.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec rule_name(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the rule node's name child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
rule_name(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select argument from rule where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and pos=0;" ),
    Name.    


%% =====================================================================
%% @spec rule_clauses(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the rule node's clause children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
rule_clauses(MId, Id) ->
    Clauses = refactor_db:select(
		"select argument from rule where mid=" ++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " and pos!=0;" ),
    Clauses.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec size_qualifier_body(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the size_qualifier node's body child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
size_qualifier_body(MId, Id) ->
    [{Body}] = refactor_db:select(
		 "select body from size_qualifier where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Body.

%% =====================================================================
%% @spec size_qualifier_argument(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the size_qualifier node's argument child.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
size_qualifier_argument(MId, Id) ->
    [{Argument}] = refactor_db:select(
		     "select size from size_qualifier where mid=" 
		     ++ integer_to_list(MId) 
		     ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Argument.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec string_value(MId::integer(), 
%%            Id::integer()) -> string()
%%
%% @doc
%% Returns the string node's  value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
string_value(MId, Id) ->
    [{String}] = refactor_db:select(
		   "select value from string where mid=" 
		   ++ integer_to_list(MId) 
		   ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    String.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec text_string(MId::integer(), 
%%            Id::integer()) -> string()
%%
%% @doc
%% Returns the text node's  value,
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
text_string(MId, Id) ->
    [{String}] = refactor_db:select(
		   "select value from text where mid=" ++ integer_to_list(MId) 
		   ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    String.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec try_expr_body(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the try_expr node's body children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
try_expr_body(MId, Id) ->
    Body = refactor_db:select(
	     "select argument from try_expr where mid=" 
	     ++ integer_to_list(MId) 
	     ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=0;" ),
    untuple(Body).


%% =====================================================================
%% @spec try_expr_clauses(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the try_expr node's clauses children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
try_expr_clauses(MId, Id) ->
    Clauses = refactor_db:select(
		"select argument from try_expr where mid=" 
		++ integer_to_list(MId) 
		++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=1;" ),
    untuple(Clauses).


%% =====================================================================
%% @spec try_expr_handlers(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the try_expr node's handler children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
try_expr_handlers(MId, Id) ->
    Handlers = refactor_db:select(
		 "select argument from try_expr where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=2;" ),
    untuple(Handlers).


%% =====================================================================
%% @spec try_expr_after(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the try_expr node's after children
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
try_expr_after(MId, Id) ->
    After = refactor_db:select(
	      "select argument from try_expr where mid=" 
	      ++ integer_to_list(MId) 
	      ++ " and id=" ++ integer_to_list(Id) ++ " and qualifier=3;" ),
    untuple(After).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec tuple_elements(MId::integer(), 
%%            Id::integer()) -> [integer()]
%%
%% @doc
%% Returns the tuple node's element children.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
tuple_elements(MId, Id) ->    
    Elements = refactor_db:select(
		 "select element from tuple where mid=" 
		 ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    untuple(Elements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec variable_literal(MId::integer(), 
%%            Id::integer()) -> string()
%%
%% @doc
%% Returns the variable node's  value.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
variable_literal(MId, Id) ->
    [{Name}] = refactor_db:select(
		 "select name from name where mid=" ++ integer_to_list(MId) 
		 ++ " and id=" ++ integer_to_list(Id) ++ " ;" ),
    Name.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =====================================================================
%% @spec subtrees(MId::integer(), 
%%            Id::integer()) -> [[integer()]]
%%
%% @doc
%% Returns the node's children sorted by type.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
subtrees(MId, Id) ->    
    case type(MId, Id) of
	1 ->
	    [[application_operator(MId, Id)],
	     application_arguments(MId, Id)];
	2 ->
	    [[arity_qualifier_body(MId, Id)],
	     [arity_qualifier_argument(MId, Id)]];
	4 ->
	    case attribute_arguments(MId, Id) of
		none ->
		    [[attribute_name(MId, Id)]];
		As ->
		    [[attribute_name(MId, Id)], As]
	    end;
	5 ->
	    [binary_fields(MId, Id)];
	6 ->
	    case binary_field_types(MId, Id) of
		[] ->
		    [[binary_field_body(MId, Id)]];
		Ts ->
		    [[binary_field_body(MId, Id)],
		     Ts]
	    end;
	7 ->
	    [block_expr_body(MId, Id)];
	8 ->
	    [[case_expr_argument(MId, Id)],
	     case_expr_clauses(MId, Id)];
	9 ->
	    [[catch_expr_body(MId, Id)]];
	11 ->
	    [[class_qualifier_argument(MId, Id)],
	    [class_qualifier_body(MId, Id)]];
	12 ->
	    case clause_guard(MId, Id) of
		none ->
		    [clause_patterns(MId, Id), clause_body(MId, Id)];
		G ->
		    [clause_patterns(MId, Id), [G],
		     clause_body(MId, Id)]
	    end;
	14 ->
	    [cond_expr_clauses(MId, Id)];
	15 ->
	    [conjunction_body(MId, Id)];
	16 ->
	    [disjunction_body(MId, Id)];
	19 ->
	    [form_list_elements(MId, Id)];
	20 ->
	    [fun_expr_clauses(MId, Id)];
	21 ->
	    [[function_name(MId, Id)], function_clauses(MId, Id)];
	22 ->
	    [[generator_pattern(MId, Id)], [generator_body(MId, Id)]];
	23 ->
	    [if_expr_clauses(MId, Id)];
	24 ->
	    [[implicit_fun_name(MId, Id)]];
	25 ->
	    [[infix_expr_left(MId, Id)],
	     [infix_expr_operator(MId, Id)],
	     [infix_expr_right(MId, Id)]];
	27 ->
	    case list_suffix(MId, Id) of
		none ->
		    [list_prefix(MId, Id)];
		S ->
		    [list_prefix(MId, Id), [S]]
	    end;
	28 ->
	    [[list_comp_template(MId, Id)], list_comp_body(MId, Id)];
	29 ->
	    case macro_arguments(MId, Id) of
		none ->
		    [[macro_name(MId, Id)]];
		As ->
		    [[macro_name(MId, Id)], As]
	    end;
	30 ->
	    [[match_expr_pattern(MId, Id)],
	     [match_expr_body(MId, Id)]];
	31 ->
	    [[module_qualifier_argument(MId, Id)],
	     [module_qualifier_body(MId, Id)]];
	34 ->
	    [[parentheses_body(MId, Id)]];
	35 ->
	    [[prefix_expr_operator(MId, Id)],
	     [prefix_expr_argument(MId, Id)]];
	36 ->
	    [qualified_name_segments(MId, Id)];
	37 ->
	    [[query_expr_body(MId, Id)]];
	38 ->
	    case receive_expr_timeout(MId, Id) of
		none ->
		    [receive_expr_clauses(MId, Id)];
		E ->
		    [receive_expr_clauses(MId, Id),
		     [E],
		     receive_expr_action(MId,Id)]
	    end;
	39 ->
	    case record_access_type(MId,Id) of
		none ->
		    [[record_access_argument(MId, Id)],
		     [record_access_field(MId, Id)]];
		R ->
		    [[record_access_argument(MId, Id)],
		     [R],
		     [record_access_field(MId, Id)]]
	    end;
	40 ->
	    case record_expr_argument(MId, Id) of
		none ->
		    [[record_expr_type(MId, Id)],
		     record_expr_fields(MId, Id)];
		V ->
		    [[V],
		     [record_expr_type(MId, Id)],
		     record_expr_fields(MId, Id)]
	    end;
	41 ->
	    case record_field_value(MId, Id) of
		none ->
		    [[record_field_name(MId, Id)]];
		V ->
		    [[record_field_name(MId, Id)], [V]]
	    end;
	42 ->
	    [[record_index_expr_type(MId, Id)],
	     [record_index_expr_field(MId, Id)]];
	43 ->
	    [[rule_name(MId, Id)], rule_clauses(MId, Id)];
	44 ->
	    [[size_qualifier_body(MId, Id)],
	     [size_qualifier_argument(MId, Id)]];
	47 ->
	    [try_expr_body(MId, Id),
	     try_expr_clauses(MId, Id),
	     try_expr_handlers(MId, Id),
	     try_expr_after(MId, Id)];
	48 ->
	    [tuple_elements(MId, Id)];
	_ ->
	    []
    end.

%% =====================================================================
%% @spec type(MId::integer(), 
%%            Id::integer()) -> integer()
%%
%% @doc
%% Returns the node's type.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
type(MId, Id) ->
    [{Type}] = refactor_db:select(
		 "select type from node_type where mid=" 
		 ++ integer_to_list(MId) ++ " and id=" 
		 ++ integer_to_list(Id)++ ";"),
    Type.

%% =====================================================================
%% @spec untuple(List::[{term()}]) -> [term()]
%%
%% @doc
%% Untuple the list's elements. The tuples have to be one element tuples.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>Id</b> : Id of the node. </pre>
%% @end
%% =====================================================================
untuple(List) ->
    lists:map(fun({Element}) -> Element end, List).    

