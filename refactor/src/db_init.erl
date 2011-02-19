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

%% @doc The module constructs the database, it can build up the ODBC 
%%	connection, drop and create the tables, and stop the connection.
%% @end


-module(db_init).

-vsn('0.1').

-export([init/0
         ]).


%a teljes adatbazis ujraepitese
%% =====================================================================
%% @spec init() -> ok
%%                                    
%%
%% @doc
%% The function creates the database structure for the parsed code. 
%% Drops every previous table and creates the new empty ones.
%% 
%% Parameter description:<pre>
%% </pre>
%% @end
%% Used: d_client module
%% =====================================================================
init() ->
    refactor_db:drop_table("DROP TABLE IF EXISTS name,pos,var_visib,fun_visib,"
    			   "fun_call,fun_cache,scope_visib,scope,node_type,"
    			   "application,arity_qualifier,attribute_,binary_,"
    			   "binary_field,block_expr,case_expr,catch_expr,"
    			   "char_,class_qualifier,clause,comment,"
		           "cond_expr,conjunction,disjunction;"), 
		           %tablak torles
    refactor_db:drop_table("DROP TABLE IF EXISTS float_,form_list,fun_expr,"
    			   "function,generator,if_expr,implicit_fun,"
		   	   "infix_expr,integer_,list,list_comp,macro,"
		   	   "match_expr,module_qualifier,parentheses,"
		   	   "prefix_expr,qualified_name,query_expr,"
		   	   "receive_expr,record_access,record_expr,"
		   	   "record_field,record_index_expr,rule,"
		   	   "size_qualifier,string,text,try_expr,tuple,"
		   	   "precomment,postcomment,forbidden_names,"
		   	   "module,id_count;"),
    refactor_db:commit(),
    refactor_db:create_table("CREATE TABLE fun_cache (mid integer NOT NULL,"
    			     "id integer NOT NULL, "
    			     "module varchar(255) NOT NULL,"
    			     "fun varchar(255) NOT NULL, "
    			     "arity integer NOT NULL,"
    			     " type integer NOT NULL, "
    			     "PRIMARY KEY(mid, id));"),
    refactor_db:create_table("CREATE TABLE scope (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "scope integer NOT NULL, "
    			     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE scope_visib (mid integer NOT NULL,"
    			     " id integer NOT NULL,"
    			     " target integer NOT NULL,"
    			     " PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE id_count (mid integer PRIMARY KEY,"
    			     "formlid integer NOT NULL, "
    			     "num integer NOT NULL)"),
    refactor_db:create_table("CREATE TABLE module (mid integer PRIMARY KEY,"
    			     "path varchar(255) NOT NULL);"),
    refactor_db:create_table("CREATE TABLE name (mid integer NOT NULL, "
    			     "id integer NOT NULL,"
    			     " name varchar(255) NOT NULL, "
    			     "PRIMARY KEY(mid,id));"), 
						%nev<->id megfeleltetes
    refactor_db:create_table("CREATE TABLE pos  (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "line integer NOT NULL, "
    			     "col integer NOT NULL,"
		             " PRIMARY KEY(mid,id));"),
    % id=fgv node id-je, 0-as pos arity, 1..n clause-ok id-je
    refactor_db:create_table("CREATE TABLE fun_visib (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		             " argument integer NOT NULL, "
		             "PRIMARY KEY(mid,id,pos)) ;"),
    refactor_db:create_table("CREATE TABLE var_visib (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "target integer NOT NULL,"
		             " PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE fun_call (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "tmid integer NOT NULL, "
    			     "target integer NOT NULL,"
		             " PRIMARY KEY(mid,id));"), 
		             %application(fgv-hivas) melyik is tulajdonkeppen 
    refactor_db:create_table("CREATE TABLE precomment (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " qualifier integer NOT NULL, "
		   	     "argument varchar(10000) NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos,qualifier));"),
    refactor_db:create_table("CREATE TABLE postcomment (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		    	     " qualifier integer NOT NULL, "
		    	     "argument varchar(10000) NOT NULL, "
		    	     "PRIMARY KEY(mid,id,pos,qualifier));"),
    %pozicio eltarolasa minden egyes id-hez
    refactor_db:create_table("CREATE TABLE forbidden_names ("
    			     "type integer NOT NULL, "
    			     "forbidden_name varchar(255) PRIMARY KEY);"),
    refactor_db:create_table("CREATE TABLE node_type (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "type integer NOT NULL,"
		   	     " PRIMARY KEY(mid,id));"), 
		   	     %node<->tipus megfeleltetes
    refactor_db:create_table("CREATE TABLE application (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos) );"),
    refactor_db:create_table("CREATE TABLE arity_qualifier ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "body integer NOT NULL,"
		   	     " arity integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE attribute_ (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE binary_ (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " field integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE binary_field ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE block_expr ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " body integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE case_expr ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE catch_expr (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "expression integer NOT NULL,"
		   	     " PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE char_ (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "value integer NOT NULL,"
		   	     " PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE class_qualifier ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "class integer NOT NULL,"
		   	     " body integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE clause (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " qualifier integer NOT NULL, "
		   	     "argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos,qualifier) );"),
    refactor_db:create_table("CREATE TABLE comment (mid integer NOT NULL, "
    	 		     "id integer NOT NULL, "
    	 		     "pos integer NOT NULL,"
		   	     " argument varchar(1000) NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE cond_expr (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " clause integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE conjunction ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		             " argument integer NOT NULL, "
		             "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE disjunction (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE float_ (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "value float NOT NULL, "
    			     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE form_list (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " form integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE fun_expr (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " clause integer NOT NULL,"
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE function (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		    	     " clause integer NOT NULL, "
		    	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE generator (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pattern integer NOT NULL,"
		   	     " body integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE if_expr (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " clause integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE implicit_fun ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "name_id integer NOT NULL,"
		             " PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE infix_expr (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "lft integer NOT NULL,"
		    	     " oper integer NOT NULL, "
		    	     "rght integer NOT NULL, "
		    	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE integer_ (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "value varchar(255) NOT NULL,"
		   	     " PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE list (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " element integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE list_comp (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE macro (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE match_expr (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pattern integer NOT NULL,"
		   	     " body integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE module_qualifier ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "module integer NOT NULL,"
		   	     " body integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE parentheses (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "body integer NOT NULL, "
    			     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE prefix_expr ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "operator integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE qualified_name ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " segment integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE query_expr (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "body integer NOT NULL, "
    			     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE receive_expr ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " qualifier integer NOT NULL, "
		   	     "argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos,qualifier) );"),
    refactor_db:create_table("CREATE TABLE record_access ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "argument integer NOT NULL,"
		   	     " type integer NOT NULL, "
		   	     "field integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE record_expr (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE record_field ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "name_id integer NOT NULL,"
		   	     " value integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE record_index_expr ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "type integer NOT NULL,"
		   	     " field integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE rule (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL, "
    			     "argument integer NOT NULL,"
		   	     " PRIMARY KEY(mid,id,pos));"),
    refactor_db:create_table("CREATE TABLE size_qualifier ("
    			     "mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "body integer NOT NULL,"
		   	     " size integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE string (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "value varchar(50000) NOT NULL, "
    			     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE text (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "value varchar(50000) NOT NULL, "
    			     "PRIMARY KEY(mid,id));"),
    refactor_db:create_table("CREATE TABLE try_expr (mid integer NOT NULL,"
    			     " id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " qualifier integer NOT NULL, "
		   	     "argument integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos,qualifier));"),
    refactor_db:create_table("CREATE TABLE tuple (mid integer NOT NULL, "
    			     "id integer NOT NULL, "
    			     "pos integer NOT NULL,"
		   	     " element integer NOT NULL, "
		   	     "PRIMARY KEY(mid,id,pos));"),
    refactor_db:commit(),
    add_forbidden_names(),
    refactor_db:commit().

%% =====================================================================
%% @spec add_forbidden_names() -> ok
%%
%% @doc
%% Adds the forbidden names to the database with their types.
%% The type is 1 if the name is a BIF, and 2 if the name is a reserved
%% word.
%% 
%% Parameter description:<pre>
%%</pre>
%% @end
%% Used: 
%% =====================================================================    
add_forbidden_names() ->
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"abs\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"apply\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"atom_to_list\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"binary_to_list\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"binary_to_term\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"check_process_code\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"concat_binary\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"date\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"delete_module\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"disconnect_node\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"element\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"erase\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"exit\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"float\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"float_to_list\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"garbage_collect\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"get\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"get_keys\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"group_leader\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"halt\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"hd\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"integer_to_list\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"iolist_to_binary\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"iolist_size\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_alive\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_atom\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_binary\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_boolean\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_float\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_function\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_integer\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_list\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_number\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_pid\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_port\");"),    
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_process_alive\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_record\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_reference\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"is_tuple\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"length\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"link\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"list_to_atom\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"list_to_binary\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"list_to_existing_atom\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"list_to_float\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"list_to_integer\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"list_to_pid\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"list_to_tuple\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"load_module\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"make_ref\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"module_loaded\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"monitor_node\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"node\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"nodes\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"now\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"open_port\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"pid_to_list\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"port_close\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"port_command\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"port_connect\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"port_control\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"pre_loaded\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"process_flag\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"process_info\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"processes\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"purge_module\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"put\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"register\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"registered\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"round\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"self\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"setelement\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"size\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"spawn\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"spawn_link\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"spawn_opt\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"split_binary\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"statistics\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"term_to_binary\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"throw\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"time\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"tl\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"trunc\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"tuple_to_list\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"unlink\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"unregister\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (1, \"whereis\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"after\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"and\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"andalso\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"band\");"),
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"begin\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"bnot\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"bor\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"bsl\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"bsr\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"bxor\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"case\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"catch\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"cond\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"div\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"end\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"fun\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"if\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"let\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"not\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"of\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"or\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"orelse\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"query\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"receive\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"rem\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"try\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"when\");"), 
    refactor_db:insert("insert into forbidden_names (type, forbidden_name) "
                       "values (2, \"xor\");").
