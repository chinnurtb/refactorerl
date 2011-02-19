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
%%% --------------------------------------------------------------------------

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

%% @doc The module's main function is to put the abstract syntax tree 
%%      (and the original code) into the database.
%% @end


-module(into_db).

-export([parse/1, increment/1, get_id/1,set_positions/1]).
-import(erl_syntax,[subtrees/1,update_tree/2,set_ann/2,add_ann/2,get_ann/1]).  

%% =====================================================================
%% @spec parse(File::string()) -> 
%%                                    {atom(),term()}
%%
%% @doc
%% The function parses the source file, and if there was no error it 
%% recomments the file and uses some format tools and write out the result 
%% to the file, and after that it puts into the database.
%% 
%% Parameter description:<pre>
%% <b>File</b> : Path of the file.</pre>
%% @end
%% Used: d_client module
%% =====================================================================

parse(File)->
    case epp_dodger1:parse_file(File) of
        {ok, Forms} ->
	    check_error(Forms),
	    Comments = erl_comment_scan:file(File),
	    Forms1 = erl_recomment1:recomment_forms(Forms, Comments),
	    Forms2 = erl_syntax:flatten_form_list(Forms1),
	    {_,Io}=file:open(File,[write]),
	    io:put_chars(Io,erl_prettypr:format(Forms2)),
	    file:close(Io),
	    {ok,Forms3} = epp_dodger1:parse_file(File),
	    Comments2 = erl_comment_scan:file(File),
	    Forms4 = erl_recomment1:recomment_forms(Forms3, Comments2),
	    Forms5 = erl_syntax:flatten_form_list(Forms4),
	    parse_database(Forms5, File);            
        {error, R} ->
	    exit({error, R})
    end.

%% =====================================================================
%% @spec check_error(Fs::[tuple()]) -> 
%%                                    [atom()]
%%
%% @doc
%% The function checks the parsed file does not contain error_marker node.
%% 
%% Parameter description:<pre>
%% <b>Fs</b> : List of forms.</pre>
%% @end
%% Used: parse/1
%% =====================================================================


check_error(Fs) ->
    Fun = fun (F) ->
                  case erl_syntax:type(F) of
                      error_marker ->
                          S = case erl_syntax:error_marker_info(F) of
                                  {_, M, D} ->
                                      M:format_error(D);
                                  _ ->
                                      "unknown error"
                              end,
			  {Line, _Col} = erl_syntax:get_pos(F),
                          io:format("ERROR in abstract tree at line ~B:~n ~s ~n", [Line,S]),
                          exit(error);
                      _ ->
                          ok
                  end
          end,
    lists:foreach(Fun, Fs).

%% =====================================================================
%% @spec parse_database(Forms::[tuple()], File::string()) -> 
%%                                    {atom(), term()}
%%
%% @doc
%% This function stores the datas of the File (module) from the Forms into the 
%% database. At first the function checks if the current file already in the 
%% database or not. If it is in then drop the datas of the file. After it 
%% fills up the database with the information from the file (the syntax tree 
%% nodes and the visibility informations too).
%% 
%% Parameter description:<pre>
%% <b>Forms</b> : List of forms.
%% <b>File</b> : Path of the file.</pre>
%% @end
%% Used: parse/1
%% =====================================================================


parse_database(Forms, File) ->
    case refactor:exists_in_dbase(File) of
	{true,MId} -> 
	    refactor:drop_from_dbase(MId);
	{false,MId} -> 
	    ok
    end,
    Pid = spawn(fun() -> increment(0) end),
    Forms2 = postorder(fun add_id/3,Pid,MId, Forms),
    Pid ! stop,
    preorder(fun visib/2,MId,Forms2),
    put_function_calls_in_db(MId),
    refactor:commit().


%% =====================================================================
%% @spec postorder(F::function(), Pid::pid(), Mid::integer(), 
%%                 Tree::integer()) -> 
%%                                    term()
%%
%% @doc
%% The function walks the Tree by postorder ingress, and in every step
%%   uses a function to the subtree/node and returns with the first
%%   parameter's return value.
%% 
%% Parameter description:<pre>
%% <b>F</b> : The function which will execute on every node(the
%%    		  function has three parameters).
%% <b>Pid</b> : The pid of the spawn in parse_database.
%% <b>MId</b> : The identifier of the module.
%% <b>Tree</b> : The structure, which have to be walked and updated.</pre>
%% @end
%% Used: parse_database/2
%% =====================================================================



postorder(F, Pid, MId, Tree) ->
    F(Pid, MId, case subtrees(Tree) of
			 [] -> Tree;
			 List -> update_tree(Tree,
					     [[postorder(F, Pid, MId, Subtree)
					       || Subtree <- Group]
					      || Group <- List])
	   end).

%% =====================================================================
%% @spec add_id(Pid::pid(), Mid::integer(), Node::integer()) -> 
%%                                    term()
%%
%% @doc
%%     The function adds a unique identifier to the node, 
%%     and puts into the correct database table the datas of the node.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the spawn in parse_database.
%% <b>MId</b> : The identifier of the module.
%% <b>Node</b> : The actual node which will be put into the database.</pre> 
%% @end
%% Used: parse_database/2 as the first parameter of postorder/4 function.
%% =====================================================================


add_id(Pid,MId,Node)->
    Id = get_id(Pid),
    Node2=set_ann(Node, [Id]),
    lists:foldl(fun(Node_, Pos) -> 
			case (erl_syntax:comment_padding(Node_)) of
			    none ->
				ok;
			    _ ->
			        refactor:insert_precomment_padding(MId,
			          Id,Pos,erl_syntax:comment_padding(Node_))
			end,
			refactor:insert_precomment_text(MId,
			   Id,Pos,erl_syntax:comment_text(Node_)),
			Pos+1 end,1,erl_syntax:get_precomments(Node)), 
    lists:foldl(fun(Node_, Pos) -> 
			case (erl_syntax:comment_padding(Node_)) of
			    none ->
				ok;
			    _ ->
				refactor:insert_postcomment_padding(MId,
				   Id,Pos,erl_syntax:comment_padding(Node_))
			end,
			refactor:insert_postcomment_text(MId,
			   Id,Pos,erl_syntax:comment_text(Node_)),
			Pos+1 end,1,erl_syntax:get_postcomments(Node)), 
    case erl_syntax:type(Node) of
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	application ->
	    create_nodes:create_application(MId,
	       hd(erl_syntax:get_ann(erl_syntax:application_operator(Node))),
               create_nodes:conv_from_ast_to_id(
                 erl_syntax:application_arguments(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	arity_qualifier ->
	    create_nodes:create_arity_qualifier(MId,
	       hd(erl_syntax:get_ann(erl_syntax:arity_qualifier_body(Node))),
	       hd(erl_syntax:get_ann(erl_syntax:arity_qualifier_argument(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	atom ->
	    create_nodes:create_atom(MId,erl_syntax:atom_name(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	attribute ->
	    create_nodes:create_attribute(MId,
	       hd(erl_syntax:get_ann(erl_syntax:attribute_name(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:attribute_arguments(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	binary ->
	    create_nodes:create_binary(MId,create_nodes:conv_from_ast_to_id(
	         erl_syntax:binary_fields(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	binary_field ->
	    create_nodes:create_binary_field(MId,
	       hd(get_ann(erl_syntax:binary_field_body(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:binary_field_types(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	block_expr ->
	    create_nodes:create_block_expr(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:block_expr_body(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	case_expr ->
	    create_nodes:create_case_expr(MId,
	       hd(get_ann(erl_syntax:case_expr_argument(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:case_expr_clauses(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	catch_expr ->
	    create_nodes:create_catch_expr(MId,
	    hd(get_ann(erl_syntax:catch_expr_body(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	char ->
	    create_nodes:create_char(MId,erl_syntax:char_value(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	class_qualifier  ->
	    create_nodes:create_class_qualifier(MId,
	       hd(get_ann(erl_syntax:class_qualifier_argument(Node))),
	       hd(get_ann(erl_syntax:class_qualifier_body(Node))), Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	clause ->
	    create_nodes:create_clause(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:clause_patterns(Node)),
	       create_nodes:conv_from_ast_to_id(erl_syntax:clause_guard(Node)),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:clause_body(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 comment ->
	    create_nodes:create_comment(MId,erl_syntax:comment_padding(Node),
	       erl_syntax:comment_text(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	cond_expr ->
	    create_nodes:create_cond_expr(MId,
	       create_nodes:conv_from_ast_to_id(
		 erl_syntax:cond_expr_clauses(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	conjunction ->
	    create_nodes:create_conjunction(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:conjunction_body(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	disjunction ->
	    create_nodes:create_disjunction(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:disjunction_body(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	eof_marker ->
	    create_nodes:create_eof_marker(MId,Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	error_marker ->
	    ok ; %(Ilyen nincs, ha van az eleg erdekes mert mar egyszer lekezeltuk elmeletileg :)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	float ->
	    create_nodes:create_float(MId,erl_syntax:float_value(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	form_list ->
	    create_nodes:create_form_list(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:form_list_elements(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	fun_expr ->
            create_nodes:create_fun_expr(MId,create_nodes:conv_from_ast_to_id(
                 erl_syntax:fun_expr_clauses(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	function ->
	    create_nodes:create_function(MId,
	       hd(get_ann(erl_syntax:function_name(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:function_clauses(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	generator  ->
	    create_nodes:create_generator(MId,
	       hd(get_ann(erl_syntax:generator_pattern(Node))),
	       hd(get_ann(erl_syntax:generator_body(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	if_expr ->
	    create_nodes:create_if_expr(MId,create_nodes:conv_from_ast_to_id(
		 erl_syntax:if_expr_clauses(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	implicit_fun ->
	    create_nodes:create_implicit_fun(MId,
	       hd(get_ann(erl_syntax:implicit_fun_name(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	infix_expr ->
	    create_nodes:create_infix_expr(MId,
	       hd(get_ann(erl_syntax:infix_expr_left(Node))),
	       hd(get_ann(erl_syntax:infix_expr_operator(Node))),
	       hd(get_ann(erl_syntax:infix_expr_right(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	integer ->
	    create_nodes:create_integer(MId,erl_syntax:integer_value(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	list  ->
	    create_nodes:create_list(MId,
	       create_nodes:conv_from_ast_to_id(erl_syntax:list_prefix(Node)),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:list_suffix(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	list_comp ->
	    create_nodes:create_list_comp(MId,
	       hd(get_ann(erl_syntax:list_comp_template(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:list_comp_body(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	macro ->
	    create_nodes:create_macro(MId,
	       hd(get_ann(erl_syntax:macro_name(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:macro_arguments(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	match_expr ->
	    create_nodes:create_match_expr(MId,
	       hd(get_ann(erl_syntax:match_expr_pattern(Node))),
	       hd(get_ann(erl_syntax:match_expr_body(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	module_qualifier ->
	    create_nodes:create_module_qualifier(MId,
	       hd(get_ann(erl_syntax:module_qualifier_argument(Node))),
	       hd(get_ann(erl_syntax:module_qualifier_body(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	nil ->
            create_nodes:create_nil(MId,Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	operator ->
	    create_nodes:create_operator(MId,
	       erl_syntax:operator_literal(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	parentheses ->
	    create_nodes:create_parentheses(MId,
	       hd(get_ann(erl_syntax:parentheses_body(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	prefix_expr ->
	    create_nodes:create_prefix_expr(MId,
	       hd(get_ann(erl_syntax:prefix_expr_operator(Node))),
	       hd(get_ann(erl_syntax:prefix_expr_argument(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	qualified_name ->
	    create_nodes:create_qualified_name(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:qualified_name_segments(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	query_expr -> 
	    create_nodes:create_query_expr(MId,
	       hd(get_ann(erl_syntax:query_expr_body(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	receive_expr ->
	    create_nodes:create_receive_expr(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:receive_expr_clauses(Node)),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:receive_expr_timeout(Node)),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:receive_expr_action(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	record_access ->
	    create_nodes:create_record_access(MId,
	       hd(get_ann(erl_syntax:record_access_argument(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:record_access_type(Node)),
	       hd(get_ann(erl_syntax:record_access_field(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	record_expr ->
	    create_nodes:create_record_expr(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:record_expr_argument(Node)),
	       hd(get_ann(erl_syntax:record_expr_type(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:record_expr_fields(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	record_field ->
	    create_nodes:create_record_field(MId,
	       hd(get_ann(erl_syntax:record_field_name(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:record_field_value(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	record_index_expr ->
	    create_nodes:create_record_index_expr(MId,
	       hd(get_ann(erl_syntax:record_index_expr_type(Node))),
	       hd(get_ann(erl_syntax:record_index_expr_field(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	rule ->
            create_nodes:create_rule(MId,
               hd(get_ann(erl_syntax:rule_name(Node))),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:rule_clauses(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	size_qualifier ->
	    create_nodes:create_size_qualifier(MId,
	       hd(get_ann(erl_syntax:size_qualifier_body(Node))),
	       hd(get_ann(erl_syntax:size_qualifier_argument(Node))),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	string ->
	    create_nodes:create_string(MId,erl_syntax:string_literal(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	text ->
	    create_nodes:create_text(MId,erl_syntax:text_string(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	try_expr ->
	    create_nodes:create_try_expr(MId,
	       create_nodes:conv_from_ast_to_id(erl_syntax:try_expr_body(Node)),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:try_expr_clauses(Node)),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:try_expr_handlers(Node)),
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:try_expr_after(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	tuple  ->
	    create_nodes:create_tuple(MId,
	       create_nodes:conv_from_ast_to_id(
	         erl_syntax:tuple_elements(Node)),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	underscore ->
	    create_nodes:create_underscore(MId,Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	variable  ->
 	    create_nodes:create_variable(MId,
 	       erl_syntax:variable_literal(Node),Id);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	warning_marker ->
            io:format("Warning van a faban kulon kezelendo!",[]); 
            % na ilyet sem szeretnek latni.
	_ ->
	    io:format("Modositottak a syntax_tools??? Uj tipusu node!",[]) 
	    %Ha ilyen van syntax-tools valtozott
    end,
    refactor:set_position(MId,Id,Node2),
    Node2.

%% =====================================================================
%% @spec get_id(Pid::pid()) ->  integer()
%%
%% @doc
%%      The function returns with a unique identifier.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the spawn in parse_database.</pre>
%% @end
%% Used: add_id/3.
%% =====================================================================

get_id(Pid) ->
    Pid ! self(),
    receive 
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec increment(Pid::pid()) ->  integer()
%%
%% @doc
%%      The function produces the unique identifier. 
%%	It cooperates with function get_id/1.
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the spawn in parse_database.</pre>
%% @end
%% Used: increment/1 and parse_database/2.
%% =====================================================================
    

increment(N) ->
    receive
	stop ->
	    ok;
	Pid ->
	    Pid ! N+1,
	    increment(N+1)
    end.
    
%% =====================================================================
%% @spec put_function_calls_in_db(MId::integer()) ->  [ok]
%%
%% @doc
%%      The function search the module and get every possible
%%  	calling of the functions and stores into the fun\_call 
%%	table in the database.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The identifier of the module.</pre>
%% @end
%% Used: parse_database/2.
%% =====================================================================    

put_function_calls_in_db(MId) ->
    ModuleName = refactor:get_module_name(MId),
    FunDatas = get_fun_datas(MId), 
     %{MId,FunId,ModuleName,FunName,FunArity,IsExported}
    ApplicationDatas = get_applications_data(MId),
    ImplicitFunDatas = get_implicit_fun_datas(MId),
    StaticFunCalls = ApplicationDatas ++ ImplicitFunDatas,
    refactor:put_into_dbase_fun_cache(StaticFunCalls,FunDatas),
    FilteredStaticFunCalls = lists:filter( fun({_MId, _Id, Module,_Name,_Arity}) -> 
                                            Module/=ModuleName end, 
					   StaticFunCalls),
    AllFunCalls = refactor:get_calls_for_this_module(ModuleName),
    UsedModules = get_unique_module_names(FilteredStaticFunCalls),
    PossiblyNeededFuns = refactor:get_needed_funs_from_dbase_cache(UsedModules),

    lists:foreach(fun(Element) -> 
      refactor:put_fun_calls_into_dbase1(Element,AllFunCalls) end, FunDatas),
    lists:foreach(fun(Element) -> 
      refactor:put_fun_calls_into_dbase2(Element,PossiblyNeededFuns) end, 
      FilteredStaticFunCalls).
      
%% =====================================================================
%% @spec get_fun_datas(MId::integer()) ->  
%%		[{string(),integer(),integer(),string(),integer()}]
%%
%% @doc
%%      The function collects the informations of every functions 
%%	from the module. The informations are the followings:
%% 	
%%  	the name of the function
%% 	
%% 	the identifier of the function
%% 	
%% 	the identifier of the module
%% 	
%% 	the name of the module
%% 	
%% 	arity of the function
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The identifier of the module.</pre>
%% @end
%% Used: put_function_calls_in_db/1.
%% =====================================================================    
      


get_fun_datas(MId) ->
    %Funname, Funid, and ModuleId
    FunNIM = refactor:get_all_fun_name_and_fun_id(MId),
    FunNIMM = lists:map(fun({Name,FunId,MId2}) -> 
       	ModuleName = refactor:get_module_name(MId2),
       	{Name,FunId,MId2,ModuleName} end,FunNIM ),
    %FunName,FunId,ModuleId and ModuleName
    FunMIMNA = lists:map(fun({Name,FunId,MId2,ModuleName}) -> 
       	FunArity = refactor:get_arity_from_fun_id(MId2,FunId), 
       	{MId2,FunId,ModuleName,Name,FunArity} end, FunNIMM),
    FunMIMNA.
    %FunName,FunId,ModuleId,ModuleName and FunArity

%% =====================================================================
%% @spec get_applications_data(MId::integer()) ->  
%%		[{integer(),integer(),string(),string(),integer()}]
%%
%% @doc
%%      The function collects the static function calling expressions 
%%	and their datas from the module where the function is not in 
%%	an implicit function calling (the simple, the identified by a 
%%	module name and deprecated applications). 
%%	The informations are the followings:
%% 	
%%  	the module identifier of the application
%% 	
%% 	the identifier of the application
%% 	
%% 	the name of the module
%% 	
%% 	the name of the application
%% 	
%% 	arity of the application
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The identifier of the module.</pre>
%% @end
%% Used: put_function_calls_in_db/1.
%% =====================================================================     
    
get_applications_data(MId) ->    
    SApplicationMIdIdName = 
    	refactor:get_all_simple_application_mid_id_and_name(MId),
    SApplicationMIdIdNameArity = lists:map(fun({MId2,Name,Id}) -> 
	Arity = refactor:get_application_arity_from_application_id(MId2,Id),
	{MId2,Id,Name,Arity} end,SApplicationMIdIdName),
    SApplicationModulenameNameArityIdMId = 
    	lists:map(fun({MId2,Id,Name,Arity}) -> 
    			Module = get_module_name(MId2,Name,Arity),
			{MId2,Id,Module,Name,Arity} end, 
		  SApplicationMIdIdNameArity),
    MApplicationMIdIdModuleFunNames = 
    	refactor:get_all_module_application_mid_id_module_name_and_fun_name(MId),
    MApplicationModulenameNameArityIdMId = 
	lists:map(fun({MId2,Id,Module,Name}) -> 
  		  Arity = refactor:get_application_arity_from_application_id(
  		                   MId2,Id),
		  {MId2,Id,Module,Name,Arity} end,
	      MApplicationMIdIdModuleFunNames),
    DApplicationMIdIdTupleId = 
    	refactor:get_all_deprecated_application_mid_id_and_tuple_id(MId),
    DApplicationModulenameNameArityIdMId =
	lists:map(fun({MId2,Id,TupleId}) ->
	  Arity = refactor:get_application_arity_from_application_id(MId2,Id),   
	  ModulenameName = 
	    refactor:get_deprecated_application_name_and_module_name_from_tuple_id(
	    		MId2,TupleId),
	 {MId2,Id,element(1,hd(ModulenameName)),
	  element(1,hd(tl(ModulenameName))),Arity} end,
	DApplicationMIdIdTupleId),
    SApplicationModulenameNameArityIdMId ++ 
    MApplicationModulenameNameArityIdMId ++ 
    DApplicationModulenameNameArityIdMId.
    
%% =====================================================================
%% @spec get_implicit_fun_datas(MId::integer()) ->  
%%		[{integer(),integer(),string(),string(),integer()}]
%%
%% @doc
%%      The function collects every informations of implicit fun 
%%	call expressions which are stored in the module.
%%	The informations are the followings:
%% 	
%%  	the module identifier of the expression
%% 	
%% 	the identifier of the expression
%% 	
%% 	the name of the expression
%% 	
%% 	the name of the expression
%% 	
%% 	arity of the expression
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The identifier of the module.</pre>
%% @end
%% Used: put_function_calls_in_db/1.
%% =====================================================================    

get_implicit_fun_datas(MId) ->
    ImplFunMIdIdNameIdType = 
    	refactor:get_all_implicit_fun_module_id_id_name_id_and_type(MId),
    ImplFunModulenameNameArityIdMid = 
	lists:map(fun({MId2,Id,NameId,2}) -> 
	  		{Name,Arity} = 
	    		refactor:get_simple_implicit_fun_name_and_arity(
	    				MId2,NameId),
	  		Module = get_module_name(MId2,Name,Arity),
	  		{MId2,Id,Module,Name,Arity};
		     ({MId2,Id,NameId,31}) ->
			{Module,Name,Arity} = 
			refactor:get_module_implicit_fun_module_name_name_and_arity(
					MId2,NameId),
			{MId2,Id,Module,Name,Arity} end,
		  ImplFunMIdIdNameIdType),
    ImplFunModulenameNameArityIdMid.
    
%% =====================================================================
%% @spec get_module_name(MId::integer(), Name::string(), Arity::integer()) ->  
%%		string()
%%
%% @doc
%%      The function gets the original module name of the function which 
%%      are not qualified by module name, but imported or auto imported.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The identifier of the module.
%% <b>Name</b> : The name of the function.
%% <b>Arity</b> : The arity of the function.</pre>
%% @end
%% Used: get_fun_datas/1, get_applications_data/1, get_implicit_fun_datas/1.
%% =====================================================================      
    
get_module_name(MId,Name,Arity) ->  
    case is_imported(MId,Name,Arity) of
	false ->
	    case refactor:get_module_name_if_exists_in_module(MId,Name,Arity) of
		false ->
		    "erlang";
		Module2 ->
		    Module2
	    end;
	Module ->
	    Module
    end.
    
%% =====================================================================
%% @spec is_imported(MId::integer(), Name::string(), Arity::integer()) ->  
%%		bool() | string()
%%
%% @doc
%%      The function decides if the current function is imported in the 
%%	current module or not. If it is imported the function returns 
%%	with the name of the original module. Else it returns with false.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : The identifier of the module.
%% <b>Name</b> : The name of the function.
%% <b>Arity</b> : The arity of the function.</pre>
%% @end
%% Used: get_module_name/3
%% =====================================================================         

is_imported(MId,Name,Arity) ->
    ImportListIds = refactor:get_import_list_ids(MId),
    case (ImportListIds == []) of
	true ->
	    false;
	false ->
	   ImportedFunctions = refactor:get_imported_functions(
	   					MId,ImportListIds),
	    case module_member(Name,Arity,ImportedFunctions) of
		false ->
		    false;
		Module ->
		    Module
	    end
    end.
    
%% =====================================================================
%% @spec module_member(FunName::string(), Arity::integer(), 
%%        List::[{string(), string(), integer()}]) -> 
%%		bool() | string()
%%
%% @doc
%% 	The function checks if the current function is a member of which 
%%	module between the imported functions. The function returns with
%%	false value when the current function is not a member of the 
%%	imported functions, and returns with the original module name if
%%	it is a member.
%% 
%% Parameter description:<pre>
%% <b>FunName</b> : function name.
%% <b>Arity</b> : function arity.
%% <b>List</b> : {module name, function name, arity} list. </pre>
%% @end
%% Used: is_imported/3
%% =====================================================================    

module_member(_FunName,_Arity,[]) ->
    false;
module_member(FunName,Arity,[{ModuleName,FunName,Arity}|_Xs]) ->
    ModuleName;
module_member(FunName,Arity,[_X|Xs]) ->
    module_member(FunName,Arity,Xs).

%% =====================================================================
%% @spec get_unique_module_names(FilteredStaticFunCalls::
%%       [{integer(),integer(),string(),string(),integer()}]) -> 
%%		[string()]
%%
%% @doc
%% 	The function creates a unique list of the module names which 
%%	occur in the list using a set.
%%      
%% 
%% Parameter description:<pre>
%% <b>FilteredStaticFunCalls</b> : {module id, fun id, module name, 
%%				   function name, arity} list. </pre>
%% @end
%% Used: put_function_calls_in_db/1
%% =====================================================================    
get_unique_module_names(FilteredStaticFunCalls) -> %Set
    get_unique_module_name(FilteredStaticFunCalls, sets:new()).
    
%% =====================================================================
%% @type set(). A set, special list from the sets module.
%% @spec get_unique_module_name(FilteredStaticFunCalls::
%%       [{integer(),integer(),string(),string(),integer()}],
%%	  Set::set()) -> 
%%		list()
%%
%% @doc
%% 	The function creates a unique list of the module names which 
%%	occur in the list using a set. 
%%      
%% 
%% Parameter description:<pre>
%% <b>FilteredStaticFunCalls</b> : {module id, fun id, module name, 
%%				   function name, arity} list. 
%% <b>Set</b> : set of the module names.</pre>
%% @end
%% Used: put_function_calls_in_db/1
%% =====================================================================    
get_unique_module_name([], Set) ->
    sets:to_list(Set);
get_unique_module_name(
  [{_MId,_FunId,ModuleName,_FunName,_FunArity}| Xs], Set) ->
    get_unique_module_name(Xs, sets:add_element(ModuleName, Set)).

%% =====================================================================
%% @spec put_into_scope(MId::integer(), Pid::pid(), Node::integer()) -> 
%%		ok
%%
%% @doc
%% 	The function puts the scope information of the node to the 
%%	database.
%%      
%% 
%% Parameter description:<pre>
%% <b>MId</b> : identifier of the module. 
%% <b>Pid</b> : pid of the scope server. 
%% <b>Node</b> : identifier of the node.</pre>
%% @end
%% Used: preorder_2/5
%% =====================================================================

 put_into_scope(MId,Pid,Node) ->
     ScopeId = get_current_scope(Pid),
       refactor:insert_scope(MId, Node, ScopeId).
       
%% =====================================================================
%% @spec preorder(F::function(), MId::integer(), Tree::integer()) -> 
%%		ok
%%
%% @doc
%% 	The function walks the Tree by preorder ingress, and in every 
%%	step uses a function to the subtree/node and returns with the 
%%	first parameter's return value.
%%      
%% 
%% Parameter description:<pre>
%% <b>F</b> : the used function to every node. 
%% <b>MId</b> : identifier of the module. 
%% <b>Tree</b> : identifier of the root node of the tree.</pre>
%% @end
%% Used: parse_database/2
%% =====================================================================
       

preorder(F, MId, Tree) ->
    case erl_syntax:type(Tree) of
	function ->
	    F(MId,Tree);
	    _ ->
	    case subtrees(Tree) of
		[] -> 
		    ok;
		List -> 
		    lists:map(fun(Elements)-> 
		    	lists:map(fun(Element)-> 
		    		preorder(F,MId,Element)end
					,Elements) end
			      ,List)
	    end
    end.
    
%% =====================================================================
%% @spec visib(MId::integer(), Node::integer()) -> 
%%		[ok]
%%
%% @doc
%% 	The function stores the vibilitiy informations of the node to 
%%	the database.
%%      
%% 
%% Parameter description:<pre>
%% <b>MId</b> : identifier of the module. 
%% <b>Node</b> : identifier of the node (function).</pre>
%% @end
%% Used: parse_database/2
%% =====================================================================    

visib(MId,Node) ->
    FunId = hd(get_ann(Node)),
    Arity = erl_syntax:function_arity(Node),
    Clauses = erl_syntax:function_clauses(Node),
    Ids = lists:map(fun(Clause)->hd(get_ann(Clause)) end,Clauses),
    lists:map(fun(Clause)->visib_clause(MId,Clause)end,Clauses),
    refactor:insert_fun_visib_data(MId,FunId,Arity,Ids).
    
%% =====================================================================
%% @spec visib_clause(MId::integer(), Clause::integer()) -> 
%%		integer()
%%
%% @doc
%% 	The function stores the vibilitiy and scope informations 
%%	of the clause to the database.
%%      
%% 
%% Parameter description:<pre>
%% <b>MId</b> : identifier of the module. 
%% <b>Clause</b> : identifier of the clause.</pre>
%% @end
%% Used: visib/2
%% =====================================================================    
    
visib_clause(MId,Clause) ->
%%    Pattern = erl_syntax:clause_patterns(Clause),
    Pid = spawn(fun() -> visibility(1,[],normal) end),
    Pid2 = spawn(fun() -> 
    	scope([hd(get_ann(Clause))],
    	      [{hd(get_ann(Clause)),hd(get_ann(Clause))}]) end),
%%REMOVED: it isn't needed
%%    PatternVars = lists:filter(fun(Element)->
%%     		case erl_syntax:type(Element) of 
%%     			variable -> true; 
%%     			_ -> false end end,
%%                  Pattern),
%%    lists:map(fun(Element)->put_in(Pid, Element) end,PatternVars),
    preorder_2(fun visib_fun_expr/4,Pid,Pid2,MId,Clause),
    Visib = get_last_variables(Pid),
    Scope = get_scope_list(Pid2),
    refactor:put_visib_in_database(MId,Visib),
    refactor:put_scope_visib_in_database(MId,Scope),
    stop(Pid),
    stop(Pid2).

%% =====================================================================
%% @spec preorder_2(Pid::pid(), Pid2::pid(),
%%		    MId::integer(), Tree::integer()) -> 
%%				ok
%%
%% @doc
%% 	The function walks the Tree by preorder ingress, and in every 
%%	step uses a function to the subtree/node and returns with the 
%%	first parameter's return value.
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the visibility server. 
%% <b>Pid</b> : The pid of the scope server. 
%% <b>MId</b> : identifier of the module. 
%% <b>Tree</b> : identifier of the root node of the tree.</pre>
%% @end
%% =====================================================================

preorder_2(Pid, Pid2, MId, Tree) ->
    put_into_scope(MId,Pid2,hd(get_ann(Tree))),
    case erl_syntax:type(Tree) of
	variable ->
	    put_in_sh_check(Pid,Tree);
	_ ->	   
	    case subtrees(Tree) of
		[] -> 
		    ok;
		List -> 
		    lists:map(fun(Elements)->
		    	lists:map(fun(Element)->
		    		preorder_2(Pid,Pid2,MId,Element)end
						     ,Elements) end
			      ,List)
	    end
    end.

%% =====================================================================
%% @spec preorder_2(Newf::function(), Pid::pid(), Pid2::pid(),
%%		    MId::integer(), Tree::integer()) -> 
%%				ok
%%
%% @doc
%% 	The function walks the Tree by preorder ingress, and in every 
%%	step uses a function to the subtree/node and returns with the 
%%	first parameter's return value.
%%      
%% 
%% Parameter description:<pre>
%% <b>Newf</b> : the used function to every node. 
%% <b>Pid</b> : The pid of the visibility server. 
%% <b>Pid</b> : The pid of the scope server. 
%% <b>MId</b> : identifier of the module. 
%% <b>Tree</b> : identifier of the root node of the tree.</pre>
%% @end
%% Used: visib_clause/2, visib_fun_expr/4, visib_fun_expr_clause/4,
%%	   visib_list_comp/4, visib_generator/4, preorder_2/5
%% =====================================================================
preorder_2(Newf, Pid, Pid2, MId, Tree) ->
    put_into_scope(MId,Pid2,hd(get_ann(Tree))),
    case erl_syntax:type(Tree) of
	fun_expr ->
	    Newf(Pid, Pid2, MId, Tree);
	variable ->
	    put_in(Pid,Tree);
	list_comp ->
	    visib_list_comp(Pid,Pid2,MId,Tree);
	generator ->
	    visib_generator(Pid,Pid2,MId,Tree);
	_ ->	   
	    case subtrees(Tree) of
		[] -> 
		    ok;
		List -> 
		    lists:map(fun(Elements)->
		    	lists:map(fun(Element)->
		    		preorder_2(Newf,Pid,Pid2,MId,Element)end
						     ,Elements) end
			      ,List)
	    end
    end.
    
%% =====================================================================
%% @spec visib_fun_expr(Pid::pid(), Pid2::pid(),
%%		    MId::integer(), Node::integer()) -> 
%%				[ok]
%%
%% @doc
%% 	The function gets every clauses of the fun\_expr node 
%%	parameter and handles every clause to store the visibility 
%%	informations to the database.
%%      
%% 
%% Parameter description:<pre>
%% <b>Newf</b> : the used function to every node. 
%% <b>Pid</b> : The pid of the visibility server. 
%% <b>Pid</b> : The pid of the scope server. 
%% <b>MId</b> : identifier of the module. 
%% <b>Node</b> : identifier of the node.</pre>
%% @end
%% Used: visib_clause/2, visib_fun_expr_clause/4,
%%	   visib_list_comp/4, visib_generator/4, preorder_2/5
%% =====================================================================
    

visib_fun_expr(Pid,Pid2,MId,Node) ->
    Clauses = erl_syntax:fun_expr_clauses(Node),
    lists:map(fun(Clause)->
    	visib_fun_expr_clause(Pid,Pid2,MId,Clause)end,Clauses).
    	
%% =====================================================================
%% @spec visib_fun_expr_clause(Pid::pid(), Pid2::pid(),
%%		    MId::integer(), Clause::integer()) -> 
%%				[ok]
%%
%% @doc
%% 	This function searches the current fun expression clause 
%%	for the variable visibility informations and stores them
%%	to the database.
%%      
%% 
%% Parameter description:<pre>
%% <b>Newf</b> : the used function to every node. 
%% <b>Pid</b> : The pid of the visibility server. 
%% <b>Pid</b> : The pid of the scope server. 
%% <b>MId</b> : identifier of the module. 
%% <b>Clause</b> : identifier of the clause.</pre>
%% @end
%% Used: visib_fun_expr/4
%% =====================================================================    	
    
visib_fun_expr_clause(Pid, Pid2, MId, Clause) ->
    Pattern = erl_syntax:clause_patterns(Clause),
    split(Pid,normal),
    new_scope(Pid2,Clause),
    lists:map(fun (Element)-> preorder_2(Pid, Pid2, MId, Element) end,
              Pattern),
    lists:map(fun (Element)-> 
                      preorder_2(fun visib_fun_expr/4,Pid,Pid2,MId,Element)
              end,
              case erl_syntax:clause_guard(Clause) of 
                  none -> erl_syntax:clause_body(Clause);
                  G -> [G | erl_syntax:clause_body(Clause)]
              end),
    Visib = get_last_variables(Pid),
    end_of_scope(Pid2),
    refactor:put_visib_in_database(MId,Visib).
    
%% =====================================================================
%% @spec visib_list_comp(Pid::pid(), Pid2::pid(),
%%		    MId::integer(), Node::integer()) -> 
%%				[ok]
%%
%% @doc
%% 	This function searches the current list comp expressions 
%%	for the variable visibility informations and stores them
%%	to the database.
%%      
%% 
%% Parameter description:<pre>
%% <b>Newf</b> : the used function to every node. 
%% <b>Pid</b> : The pid of the visibility server. 
%% <b>Pid</b> : The pid of the scope server. 
%% <b>MId</b> : identifier of the module. 
%% <b>Node</b> : identifier of the node.</pre>
%% @end
%% Used: preorder_2/5
%% =====================================================================     

visib_list_comp(Pid,Pid2,MId,Node) ->
    BodyNodes = erl_syntax:list_comp_body(Node),
    Template = erl_syntax:list_comp_template(Node),
    split(Pid,listcomp),
    new_scope(Pid2,Node),
    lists:map(fun(Element)->
    	preorder_2(fun visib_fun_expr/4,Pid,Pid2,MId,Element) end,BodyNodes),
    change_state(Pid,template),
    preorder_2(fun visib_fun_expr/4,Pid,Pid2,MId,Template),
    change_state(Pid,reverse),
    Visib = get_last_variables(Pid),
    end_of_scope(Pid2),
    refactor:put_visib_in_database(MId,Visib).
    
%% =====================================================================
%% @spec visib_generator(Pid::pid(), Pid2::pid(),
%%		    MId::integer(), Node::integer()) -> 
%%				integer()
%%
%% @doc
%% 	This function searches the current generator expressions 
%%	for the variable visibility informations and stores them
%%	to the database.
%%      
%% 
%% Parameter description:<pre>
%% <b>Newf</b> : the used function to every node. 
%% <b>Pid</b> : The pid of the visibility server. 
%% <b>Pid</b> : The pid of the scope server. 
%% <b>MId</b> : identifier of the module. 
%% <b>Node</b> : identifier of the node.</pre>
%% @end
%% Used: preorder_2/5
%% =====================================================================    
       
visib_generator(Pid,Pid2,MId,Node) ->
    GenBody = erl_syntax:generator_body(Node),
    GenPattern = erl_syntax:generator_pattern(Node),
    preorder_2(fun visib_fun_expr/4,Pid,Pid2,MId,GenBody),    
    change_state(Pid,generator),
    preorder_2(fun visib_fun_expr/4,Pid,Pid2,MId,GenPattern),
    change_state(Pid,reverse).

%% =====================================================================
%% @spec visibility(N::integer(), List::[integer()],
%%		    Modes::atom() | [atom()]) -> 
%%				none()
%%
%% @doc
%% 	This function is a server function which collects the variables
%%	from separate functions to a list. The result can be get by
%%	get_last_variable function.
%%      
%% 
%% Parameter description:<pre>
%% <b>N</b> : An integer which handle the variable list of
%%	      separate clauses. 
%% <b>List</b> : A list of the current variables. 
%% <b>Pid</b> : The pid of the scope server. 
%% <b>Modes</b> : The functionality mode of the server (or in a list).</pre>
%% @end
%% Used: visib_clause/2, visibility/3
%% =====================================================================

visibility(1,List,normal) ->
    receive
	{stop,Pid} ->
	    Pid ! ok;
	{add,Name,Id,Pid} -> 
	    Pid ! ok,
	    visibility(1,add_simple(Name,Id,List),normal);
	{add_sh_check,Name,Id,Pid} ->
	    Pid ! ok,
	    visibility(1,add_simple(Name,Id,List),normal);
	{get_last,Pid} ->
	    Pid ! {ok,List},
	    visibility(1,[],normal);
	{split,Mode,Pid} ->
	    Pid ! ok,
	    visibility(2,[[],List],[Mode|normal])
    end;

visibility(N,List=[X|Xs],Modes=[normal|Ms]) ->
    receive
	{stop,Pid} ->
	    Pid ! ok;
	{add,Name,Id,Pid} -> 
	    Pid ! ok,
	    visibility(N,add(Name,Id,List,N),Modes);
	{add_sh_check,Name,Id,Pid} ->
	    Pid ! ok,
	    visibility(N,add_sh_check(Name,Id,List,N),Modes);
	{get_last,Pid} ->
	    Pid ! {ok,X},
	    case N == 2 of
		true ->
		    visibility(1,hd(Xs),Ms);
		false ->
		    visibility(N-1,Xs,Ms)
	    end;
	{split,Mode,Pid} ->
	    Pid ! ok,
	    visibility(N+1,[[]]++List,[Mode]++Modes);
	{change,State,Pid} -> %ilyen elmeletileg nincsen :)
	    Pid ! ok,
	    case State == reverse of
		true ->
		    visibility(N, List, Ms);
		false ->
		    visibility(N, List,[State]++Modes)
	    end
    end;

visibility(N, List=[X|Xs],Modes=[listcomp|Ms]) ->
    receive
	{stop,Pid} -> 
	    Pid ! ok;
	{add,Name,Id,Pid} -> %ez az erdekes
	    Pid ! ok,
	    visibility(N,add(Name,Id,List,N),Modes);
	{add_sh_check,Name,Id,Pid} -> %ilyen nincs elmeletileg :)
	    Pid ! ok,
	    visibility(N,add_sh_check(Name,Id,List,N),Modes);
	{get_last,Pid} ->
	    Pid ! {ok,X},
	    case N == 2 of
		true ->
		    visibility(1,hd(Xs),Ms);
		false ->
		    visibility(N-1,Xs,Ms)
	    end;
	{split,Mode,Pid} ->
	    Pid ! ok,
	    visibility(N+1,[[]]++List,[Mode]++Modes);
	{change,State,Pid} ->
	    Pid ! ok,
	    case State == reverse of
		true ->
		    visibility(N, List, Ms);
		false ->
		    visibility(N, List,[State]++Modes)
	    end    
    end;

visibility(N, List=[X|Xs],Modes=[generator|Ms]) ->
    receive
	{stop,Pid} ->
	    Pid ! ok;
	{add,Name,Id,Pid} -> %ez az erdekes
	    Pid ! ok,
	    visibility(N,add_generator(Name,Id,List,N),Modes);
	{add_sh_check,Name,Id,Pid} -> %ilyen nincs
	    Pid ! ok,
	    visibility(N,add_sh_check(Name,Id,List,N),Modes);
	{get_last,Pid} -> %ilyen nincs 
	    Pid ! {ok,X},
	    case N == 2 of
		true ->
		    visibility(1,hd(Xs),Ms);
		false ->
		    visibility(N-1,Xs,Ms)
	    end;
	{split,Mode,Pid} -> %ilyen nincs
	    Pid ! ok,
	    visibility(N+1,[[]]++List,[Mode]++Modes);
	{change,State,Pid} -> %ilyen nincs
	    Pid ! ok,
	    case State == reverse of
		true ->
		    visibility(N, List, Ms);
		false ->
		    visibility(N, List,[State]++Modes)
	    end    
    end;
visibility(N, List=[X|Xs],Modes=[template|Ms]) ->
    receive
	{stop,Pid} ->
	    Pid ! ok;
	{add,Name,Id,Pid} -> % ez az erdekes
	    Pid ! ok,
	    visibility(N,add_template(Name,Id,List,N),Modes);
	{add_sh_check,Name,Id,Pid} -> %ilyen nincs
	    Pid ! ok,
	    visibility(N,add_sh_check(Name,Id,List,N),Modes);
	{get_last,Pid} -> %ilyen nincs
	    Pid ! {ok,X},
	    case N == 2 of
		true ->
		    visibility(1,hd(Xs),Ms);
		false ->
		    visibility(N-1,Xs,Ms)
	    end;
	{split,Mode,Pid} -> %ilyen nincs
	    Pid ! ok,
	    visibility(N+1,[[]]++List,[Mode]++Modes);
	{change,State,Pid} -> %ilyen nincs
	    Pid ! ok,
	    case State == reverse of
		true ->
		    visibility(N, List, Ms);
		false ->
		    visibility(N, List,[State]++Modes)
	    end    
    end.

%% =====================================================================
%% @spec add_simple(Name::integer(), Id::integer(),
%%		    List::[{string(), integer(), [integer()]}]) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function adds the identifier of the variable to the 
%%	current list and returns the updated list.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).</pre>
%% @end
%% used: add/4, add_simple/3, add_to_nth_template/4 , add_to_nth/4 ,
%%	   add_sh_check/4, visibility/3
%% =====================================================================

add_simple(Name,Id,[]) ->
    [{Name,Id,[Id]}];
add_simple(Name,Id,[X|Xs]) ->
    case Name == element(1,X) of
	true ->
	   [{Name,element(2,X),element(3,X)++[Id]}|Xs];
	false ->
	   [X|add_simple(Name,Id,Xs)]
    end.

%% =====================================================================
%% @spec add(Name::integer(), Id::integer(),
%%	     List::[{string(), integer(), [integer()]}], N::integer) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function adds the identifier of the variable to the 
%%	current list and returns the updated list.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).
%% <b>N</b> : Number of list elements.</pre>
%% @end
%% Used: visibility/3
%% =====================================================================

add(Name,Id,Lists=[X|Xs],N)->
    Boollist=lists:map(fun (List) -> 
    		lists:any(fun(Element)->
    			element(1,Element) == Name end,
				      List) end,
	      Lists),
    Member = get_true_pos(Boollist),
    case (Member == 1) or (Member > N) of
	true ->
	    [add_simple(Name,Id,X)|Xs];
	false ->
	    add_to_nth(Name,Id,Lists,Member)
    end.
    
%% =====================================================================
%% @spec add_generator(Name::integer(), Id::integer(),
%%	     List::[{string(), integer(), [integer()]}], N::integer) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function adds the identifier of the variable to the 
%%	current list and returns the updated list.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).
%% <b>N</b> : Number of list elements.</pre>
%% @end
%% Used: visibility/3
%% =====================================================================
    
    
add_generator(Name,Id,[X|Xs],_N) ->
    [add_simple_generator(Name,Id,X)|Xs].

%% =====================================================================
%% @spec add_simple_generator(Name::integer(), Id::integer(),
%%	     List::[{string(), integer(), [integer()]}]) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function updates the list of the variables with the current 
%%	variable.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).</pre>
%% @end
%% Used: add_generator/4
%% =====================================================================

add_simple_generator(Name,Id,List) ->
    [{Name,Id,[Id]}] ++ List.
    
%% =====================================================================
%% @spec add_template(Name::integer(), Id::integer(),
%%	     List::[{string(), integer(), [integer()]}], N::integer) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function adds the identifier of the variable to the 
%%	current list and returns the updated list.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).
%% <b>N</b> : Number of list elements.</pre>
%% @end
%% Used: visibility/3
%% =====================================================================    

add_template(Name,Id,Lists=[X|Xs],N) ->
    Boollist=lists:map(fun (List) -> lists:any(fun(Element)->element(1,Element) == Name end,
				      List) end,
	      Lists),
    Member = get_true_pos(Boollist),
    case (Member == 1) or (Member > N) of
	true ->
	    [add_simple_template(Name,Id,X)|Xs];
	false ->
	    add_to_nth_template(Name,Id,Lists,Member)
    end. 
    
%% =====================================================================
%% @spec add_simple_template(Name::integer(), Id::integer(),
%%	     List::[{string(), integer(), [integer()]}]) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function adds the identifier of the variable to the 
%%	current list and returns the updated list.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).</pre>
%% @end
%% Used: add_template/4, add_simple_template/3
%% =====================================================================    
    

add_simple_template(Name,Id,[]) -> %ilyen nincs
    [{Name,Id,[Id]}];
add_simple_template(Name,Id,[X|Xs]) ->
    case Name == element(1,X) of
	true ->
	   [{Name,Id,element(3,X)++[Id]}|Xs];
	false ->
	   [X|add_simple_template(Name,Id,Xs)]
    end.
    
%% =====================================================================
%% @spec add_sh_check(Name::integer(), Id::integer(),
%%	     List::[{string(), integer(), [integer()]}], N::integer) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function adds the identifier of the variable to the 
%%	current list and returns the updated list.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).
%% <b>N</b> : Number of list elements.</pre>
%% @end
%% Used: visibility/3
%% =====================================================================     

add_sh_check(Name,Id,[X|Xs],_N)->
    [add_simple(Name,Id,X)|Xs].
    
%% =====================================================================
%% @spec put_in(Pid::pid(), Node::integer()) -> 
%%				integer()
%%
%% @doc
%% 	The function starts the putting into the database a 
%%	variable visibility informations.
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the visibility server.
%% <b>Node</b> : The node of the variable.</pre>
%% @end
%% Used: visib_clause/2, preorder_2/5
%% =====================================================================    

put_in(Pid, Node) ->
    Pid ! {add, erl_syntax:variable_name(Node),hd(get_ann(Node)),self()},
    receive
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec put_in_sh_check(Pid::pid(), Node::integer()) -> 
%%				integer()
%%
%% @doc
%% 	The function starts the putting into the database a 
%%	variable visibility informations.
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the visibility server.
%% <b>Node</b> : The node of the variable.</pre>
%% @end
%% Used: visib_fun_expr_clause/4
%% =====================================================================    

put_in_sh_check(Pid, Node) ->
    Pid ! {add_sh_check, erl_syntax:variable_name(Node),hd(get_ann(Node)),self()},
    receive
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec get_last_variables(Pid::pid()) -> 
%%				integer() | list()
%%
%% @doc
%% 	The function gives the collected variable informations to the caller.
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the visibility server.</pre>
%% @end
%% Used: visib_clause/2, visib_list_comp/4, visib_fun_expr_clause/4
%% =====================================================================    
    
    
get_last_variables(Pid) ->
    Pid ! {get_last, self()},
    receive
	{ok,N} ->
	    N;
	_ ->
	    io:format("Huston we have a problem, getting the last 
	               visibility list ~n",[]),
	    []
    end.
    
%% =====================================================================
%% @spec split(Pid::pid(), Mode::atom()) -> 
%%				integer() 
%%
%% @doc
%% 	The function starts to split the connection with the given mode.  
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the visibility server.
%% <b>Mode</b> : The mode of the server (normal, template, ...)</pre>
%% @end
%% Used: visibility/3, visib_list_comp/4, visib_fun_expr_clause/4
%% =====================================================================    
        

split(Pid,Mode) ->
    Pid ! {split,Mode,self()},
    receive
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec stop(Pid::pid()) -> 
%%				integer() 
%%
%% @doc
%% 	The function stops the connection.  
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the visibility server.</pre>
%% @end
%% Used: visib_clause/2, visibility/3
%% =====================================================================    

stop(Pid) ->
    Pid ! {stop,self()},
    receive
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec change_state(Pid::pid(), State::atom()) -> 
%%				integer() 
%%
%% @doc
%% 	The function changes the state of the connection.  
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the visibility server.
%% <b>Sate</b> : The mode of the server (normal, template, ...)</pre>
%% @end
%% Used: visib_generator/4, visib_list_comp/4
%% =====================================================================     

change_state(Pid,State) ->
    Pid ! {change,State,self()},
    receive
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec get_true_pos(List::[bool()]) -> 
%%				integer() 
%%
%% @doc
%% 	The function returns with the position number of the first 
%%	true value in the list.  
%%      
%% 
%% Parameter description:<pre>
%% <b>List</b> : List of booleans.</pre>
%% @end
%% Used: add/4, add_template/4
%% =====================================================================    

get_true_pos(List) ->
    get_true_pos(List,1).
    
%% =====================================================================
%% @spec get_true_pos(List::[bool()], N::integer()) -> 
%%				integer() 
%%
%% @doc
%% 	The function returns with the position number of the first 
%%	true value in the list.  
%%      
%% 
%% Parameter description:<pre>
%% <b>List</b> : List of booleans.
%% <b>N</b> : The position in the list.</pre>
%% @end
%% Used: get_true_pos/1
%% =====================================================================    

get_true_pos([],N)->
    N;
get_true_pos([true|_Xs],N) ->
    N;
get_true_pos([_X|Xs],N) ->
    get_true_pos(Xs,N+1).


%% =====================================================================
%% @spec add_to_nth(Name::integer(), Id::integer(),
%%	     List::[{string(), integer(), [integer()]}], N::integer) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function adds the identifier of the variable to the 
%%	current list and returns the updated list.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).
%% <b>N</b> : Number of list elements.</pre>
%% @end
%% Used: visibility/3, add_to_nth/4, add/4
%% ===================================================================== 

add_to_nth(Name,Id,[X|Xs],1)->
    [add_simple(Name,Id,X)|Xs];
add_to_nth(Name,Id,[X|Xs],N) ->
    [X|add_to_nth(Name,Id,Xs,N-1)].
    
%% =====================================================================
%% @spec add_to_nth_template(Name::integer(), Id::integer(),
%%	     List::[{string(), integer(), [integer()]}], N::integer) -> 
%%				[{string(), integer(), [integer()]}]
%%
%% @doc
%% 	The function adds the identifier of the variable to the 
%%	current list and returns the updated list.
%%      
%% 
%% Parameter description:<pre>
%% <b>Name</b> : The name of the variable.
%% <b>Id</b> : The identifier of the variable.
%% <b>List</b> : List of tuples (variable names, variable
%%     identifiers, list of identifiers of the variable).
%% <b>N</b> : Number of list elements.</pre>
%% @end
%% Used: visibility/3, add_template/4, add_to_nth_template/4
%% ===================================================================== 
    

add_to_nth_template(Name,Id,[X|Xs],1)->
    [add_simple(Name,Id,X)|Xs];
add_to_nth_template(Name,Id,[X|Xs],N) ->
    [X|add_to_nth_template(Name,Id,Xs,N-1)].

%% Scope management BEGIN


%% =====================================================================
%% @spec get_current_scope(Pid::pid()) -> 
%%				integer() 
%%
%% @doc
%% 	The function gets the current scope.  
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the scope server.</pre>
%% @end
%% Used: put_into_scope/3
%% ===================================================================== 

get_current_scope(Pid) ->
    Pid ! {current,self()},
    receive
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec get_scope_list(Pid::pid()) -> 
%%				integer() 
%%
%% @doc
%% 	The function gets the scope list.  
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the scope server.</pre>
%% @end
%% Used: visib_clause/2
%% ===================================================================== 
    

get_scope_list(Pid) ->
    Pid ! {list,self()},
    receive
	N ->
	    N
    end.    
    
%% =====================================================================
%% @spec new_scope(Pid::pid(), Node::integer()) -> 
%%				integer() 
%%
%% @doc
%% 	The function connects a new scope to the node.  
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the scope server.
%% <b>Node</b> : The identifier of the node.</pre>
%% @end
%% Used: visib_list_comp/4, visib_fun_expr_clause/4
%% ===================================================================== 
    

new_scope(Pid,Node) ->
    Pid ! {new_scope,hd(get_ann(Node)),self()},
    receive
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec end_of_scope(Pid::pid()) -> 
%%				integer() 
%%
%% @doc
%% 	The function closes the scope.  
%%      
%% 
%% Parameter description:<pre>
%% <b>Pid</b> : The pid of the scope server.</pre>
%% @end
%% Used: visib_list_comp/4, visib_fun_expr_clause/4
%% =====================================================================    

end_of_scope(Pid) ->
    Pid ! {end_scope,self()},
    receive
	N ->
	    N
    end.
    
%% =====================================================================
%% @spec scope(Scope::[integer()], List::[{integer(), integer()}]) -> 
%%				none() 
%%
%% @doc
%% 	The function is the server function to the process, which 
%%	stores the scope informations to the database.
%%      
%% 
%% Parameter description:<pre>
%% <b>Scope</b> : The list of identifiers. The head of the list id 
%%		  the current scope.
%% <b>List</b> : The list of the inner-outer scoping identifier pairs.</pre>
%% @end
%% Used: visib_clause/2, scope/2
%% =====================================================================    
    

scope(Scope = [X|Xs],List) ->
    receive 
	{stop,Pid} ->
	    Pid ! ok;
	{new_scope, Node, Pid} ->
	    Pid ! ok,
	    scope([Node] ++ Scope, [{Node,X}] ++ List);
	{end_scope,Pid} ->
	    Pid ! ok,
	    scope(Xs,List);
	{list,Pid} ->
	    Pid ! List,
	    scope(Scope,List);
	{current,Pid} ->
	    Pid ! X,
	    scope(Scope,List)
    end.

%% Scope management END

%% Set positions in module BEGIN

%% =====================================================================
%% @spec set_positions(File::string()) -> 
%%				ok 
%%
%% @doc
%% 	The function updates the positions in the database after the 
%%	refactor step using a simultaneous traverse on the database 
%%	and the syntax tree.
%%      
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the current file.</pre>
%% @end
%% Used: d_client
%% =====================================================================
		
set_positions(File) ->
    {ok,Forms} = epp_dodger1:parse_file(File),
    Comments = erl_comment_scan:file(File),
    Forms2 = erl_recomment1:recomment_forms(Forms, Comments),
    Forms3 = erl_syntax:flatten_form_list(Forms2),
    simultaneous_visiting(Forms3, File).

%% =====================================================================
%% @spec simultaneous_visiting(Forms::integer(), File::string()) -> 
%%				ok 
%%
%% @doc
%% 	The function starts the simultaneous preorder traverse to
%%  	synchronize the position informations of the nodes between
%%	the database and the source.
%%      
%% 
%% Parameter description:<pre>
%% <b>Forms</b> : The root of the parsered source code.
%% <b>File</b> : The path of the current file.</pre>
%% @end
%% Used: set_positions/1
%% =====================================================================

simultaneous_visiting(Forms, File) ->
    MId = refactor:get_module_id_from_path(File),
    FormListId = refactor:get_form_list_id_from_mid(MId),
    simultaneous_preorder(fun refactor:set_position/3, MId, FormListId, Forms),
    refactor:commit().
    
%% =====================================================================
%% @spec simultaneous_preorder(F::function(), MId::integer(),
%%			       Id::integer(), Tree::term()) -> 
%%				ok 
%%
%% @doc
%% 	The function executes the simultaneous preorder traverse to
%%  	synchronize the position informations of the nodes between 
%%	the database and the source.  
%%      
%% 
%% Parameter description:<pre>
%% <b>F</b> : The function which used to the remained parameters.
%% <b>MId</b> : The identifier of the module.
%% <b>Id</b> : The identifier of the current node.
%% <b>Tree</b> : The tree of the current node in the syntax tree.</pre>
%% @end
%% Used: set_positions/1
%% =====================================================================
    

simultaneous_preorder(F, MId, Id, Tree) ->
    F(MId,Id,Tree),
    case subtrees(Tree) of
	[] -> 
	    ok;
	List ->
	    JoinedList = lists:zip(lists:flatten(List),
	    		 lists:flatten(erl_syntax_db:subtrees(MId,Id))),
	    lists:map(fun({Element,NewId})-> 
	    		simultaneous_preorder(F,MId,NewId,Element) end
		      ,JoinedList)
    end.



%% Set positions in module END
