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

%%% @doc
%%% This module implements the extract function refactoring.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>

-module(refac_extract_fun).
-vsn("$Rev: 1247 $").

-include("refactorerl.hrl").

-export([extract_function/4]).

%% =====================================================================
%% @spec extract_function(File::string(), NewName::string(),
%%                    FromPos::integer(), ToPos::integer()) 
%%                                          ->atom() | {atom(), term()}
%%
%% @doc
%% Performs the precondition checks and collects all the
%% necessary information for the refactoring.
%% It throws various terms when there is an error, with this structure:
%% {atom(), term()}. If the atom is ok, the refactoring has been
%% preformed without a problem.
%%
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>NewName</b> : The name of the extract function.
%% <b>FromPos</b> : The pointed first position in the editor.
%% <b>ToPos</b> : The pointed last position in the editor.
%% </pre>
%% @end
%% =====================================================================
extract_function( File, NewName, FromPos, ToPos)->
    {ModuleNode, SourceNode} = get_module_and_source_node(File),
    
    {FromToken,PathFrom} = get_token_from_pos(SourceNode, FromPos),
    {ToToken,PathTo} = get_token_from_pos(SourceNode,ToPos),

    Result = refac_query:top_expr(PathFrom, PathTo),
    {FirstExpr, LastExpr, Found, Top} = 
             get_first_and_last_expr(FromToken, ToToken, Result, PathFrom),

    check_is_legal_body(ModuleNode, Found),
    check_not_in_pattern_guard_macro_listgen(FirstExpr,LastExpr,FromToken),  

    {AllSupExpr, ExprList} = get_expr_list(FirstExpr, LastExpr,Top),

    {UseVarNode, UseVarObject} = 
            get_used_variable_node_and_object(ExprList,Found),

    BoundVarNode = 
            get_inside_bound_variables(UseVarNode, unique(UseVarObject,[])),

    NotBoundVarNode = UseVarNode -- BoundVarNode,

    NotBoundVarName = get_var_name(NotBoundVarNode),

    Arity = length(NotBoundVarName),
    check_is_legal_name(ModuleNode, NewName, Arity),
    check_all_var_bound_ok(AllSupExpr, UseVarObject, BoundVarNode),

    perform_refactoring(Found,SourceNode,NewName, 
                        NotBoundVarName,ExprList,Top,
                        FromToken, ToToken), 
    {ok, SourceNode}.


%% =====================================================================
%% @spec get_module_and_source_node(File::string()) -> node()
%%
%% @doc
%% Gets the module node and the source node from the path of the module.
%%
%% Parameter description:<pre>
%% <b>File</b> : The name of the module.
%% </pre>
%% @end
%% ===================================================================== 
get_module_and_source_node(File)->
    RootNode = ?GRAPH:root(),
    Source = ?GRAPH:path(RootNode, [{file, {path, '==', File}}]),
    case Source of
          []->
	    error({not_exists, File});
	  _ -> ok
    end,
    [SourceNode] = Source,
    Module = ?GRAPH:path(SourceNode, [moddef]),
    case Module of
          []->
	    error({not_exists, File});
	  _ -> ok
    end,
    [ModuleNode] = Module,
    {ModuleNode, SourceNode }.


%% =====================================================================
%% @spec get_token_from_pos(FileName::string(),Pos::integer()) -> ok
%%
%% @doc
%% Gets token from the position.
%%
%% Parameter description:<pre>
%% <b>FileName</b> : The file name.
%% <b>Pos</b> : The position of the token.
%% </pre>
%% @end
%% ===================================================================== 
get_token_from_pos(FileName,P)->
    X = refac_query:token_by_pos(FileName, P),
    case X of
          {_,_,_,_}-> ok;
	  _ ->  error({bad_position, P})
    end,
    {Token,_,_,_} = X,
    Path = refac_query:path_to_file(Token),
    {Token, Path}.

%% =====================================================================
%% @spec get_first_and_last_expr(FromToken::token(), ToToken::token(),
%%                               ResultList::[node()], Path::[node()]) 
%%                                        ->{node(),node(),atom(),node()}
%%
%% @doc
%% Gets the first and the last expression node.
%%
%% Parameter description:<pre>
%% <b>FromToken</b> : The pointed first token.
%% <b>ToToken</b> : The pointed last token.
%% <b>ResultList</b> : The common part of the pathes from the root to
%%                     the pointed tokens.
%% <b>Path</b> : The path from the root to the pointed token.
%% </pre>
%% @end
%% =====================================================================
get_first_and_last_expr(FromToken, ToToken, ResultList, Path)->
    case element(1,ResultList) of
       one_token->
           {FirstExpr, LastExpr, Found, Top} = 
                             one_token(Path,FromToken,ToToken);
       _->
           {FirstExpr, LastExpr, Found, Top} = 
                             more_token(FromToken,ToToken,ResultList)
    end,
    {FirstExpr, LastExpr, Found, Top}.


%% =====================================================================
%% @spec one_token(Path::[node()],FromToken::token(), ToToken::token())
%%                                         ->{node(),node(),atom(),node()}
%% @doc
%% Gets the first and the last expression node.
%%
%% Parameter description:<pre>
%% <b>Path</b> : The path from the root to the pointed token.
%% <b>FromToken</b> : The pointed first token.
%% <b>ToToken</b> : The pointed last token.
%% </pre>
%% @end
%% =====================================================================
one_token(Path,FromToken,ToToken)->
    ExprNode = hd(tl(lists:reverse(Path))),
    First = ?GRAPH:path(ExprNode, [efirst]),
    Last = ?GRAPH:path(ExprNode, [elast]),
    if (First == [FromToken]) and (Last == [ToToken])->
                                      {ExprNode,ExprNode,found_expr,ExprNode};
        true ->  {ok,ok,not_found,[]}
    end.


%% =====================================================================
%% @spec more_token(FromToken::token(), ToToken::token(),
%%                  ResultList::[node()])->{node(),node(),atom(),node()}
%% @doc
%% Gets the first and the last expression node.
%%
%% Parameter description:<pre>
%% <b>FromToken</b> : The pointed first token.
%% <b>ToToken</b> : The pointed last token.
%% <b>ResultList</b> : The common part of the pathes from the root to
%%                     the pointed tokens.
%% </pre>
%% @end
%% =====================================================================

more_token(FromToken, ToToken, ResultList)->
   {Top,_,Path1,Path2} = ResultList,
   First = ?GRAPH:path(FromToken, [{efirst, back}]) ++
           ?GRAPH:path(FromToken, [{lfirst, back}]),
   Last = ?GRAPH:path(ToToken, [{elast, back}]) ++
          ?GRAPH:path(ToToken, [{llast, back}]),
   if 
     (First == []) or (Last == []) ->
                            {ok,ok,not_found,[]};
     true ->
         case Top of
              {_,clause,_} ->
                get_exprs(hd(Path1),hd(Path2),Top,FromToken,ToToken,found_body);
              {_,expr,_} ->
                get_exprs(Top,Top,Top,FromToken,ToToken,found_expr);
              _ -> 
               {ok,ok,not_found,[]}
         end
   end.

%% =====================================================================
%% @spec get_exprs(Top1::node(), Top2::node(), Top::node(),
%%                  FromToken::token(), ToToken::token(),
%%                  Status::atom())->{node(),node(),atom(),node()}
%% @doc
%% Gets the first and the last expression node.
%%
%% Parameter description:<pre>
%% <b>Top1</b> : The pointed first node.
%% <b>Top2</b> : The pointed last node.
%% <b>Top</b> : The comon expression node.
%% <b>FromToken</b> : The pointed first token.
%% <b>ToToken</b> : The pointed last token.
%% <b>Status</b> : An atom.
%% </pre>
%% @end
%% =====================================================================    
get_exprs(Top1,Top2,Top,FromToken,ToToken,Status)->
  TopFirst = ?GRAPH:path(Top1, [efirst]),
  TopLast = ?GRAPH:path(Top2, [elast]),
  case TopFirst of
    []-> {ok,ok,not_found,[]};
    [TopEFirst] ->
       case TopLast of 
         [] -> {ok,ok,not_found,[]};
         [TopELast]-> 
            case ?GRAPH:data(TopEFirst) of
              {_, subst, _}-> FirstT = ?GRAPH:path(TopEFirst, [lfirst]);
               _ -> FirstT = ?GRAPH:path(Top1, [efirst])
            end,   
            case ?GRAPH:data(TopELast) of
              {_, subst, _}-> LastT = ?GRAPH:path(TopELast, [llast]);
               _ -> LastT = ?GRAPH:path(Top2, [elast])
            end,  
            if (FirstT == [FromToken]) and (LastT == [ToToken])->
                                            {Top1,Top2,Status,Top};
               true ->  {ok,ok,not_found,[]}
            end
       end
  end.


%% =====================================================================
%% @spec get_expr_list(FirstExpr::node(),LastExpr::node(), 
%%                             ClauseNode::node())-> {[node()],[node()]}
%%
%% @doc
%% Gets the selected expressions nodes, 
%%            and every top level node from the selected clause.
%%
%% Parameter description:<pre>
%% <b>FirstExpr</b> : The pointed first expression's node.
%% <b>LastExpr</b> : The pointed last expression's node.
%% <b>ClauseNode</b> : The pointed part's clause.
%% </pre>
%% @end
%% =====================================================================
get_expr_list(FirstExpr, LastExpr,ClauseNode)->
    if 
       FirstExpr == LastExpr->
            ExprList = [FirstExpr],
            [SuperExpr] = ?GRAPH:path(FirstExpr, [sup]),
            [Clause] = ?GRAPH:path(SuperExpr, [{visib,back}]),
            AllExprList = ?GRAPH:path(Clause,[body]);
       true ->
	    AllExprList = ?GRAPH:path(ClauseNode,[body]),
	    ReverseList = lists:reverse
	                    (lists:dropwhile(fun(Elem1) ->
		                                not (Elem1 == FirstExpr) 
					     end,
					     AllExprList)),
	    ExprList = lists:reverse
	                    (lists:dropwhile(fun (Elem2) ->
					          not (Elem2 == LastExpr)
					     end,
					     ReverseList))
    end,
    {AllExprList, ExprList}.


%% =====================================================================
%% @spec get_used_variable_node_and_object(List::[node()],  
%%                                 Found::atom()) -> {[node()],[node()]}
%%
%% @doc
%% Gets the used variables and it's variable object from the expressions.
%%
%% Parameter description:<pre>
%% <b>List</b> : The expression's node list.
%% <b>Found</b> : An atom.
%% </pre>
%% @end
%% ===================================================================== 
get_used_variable_node_and_object(List,Found)->
    if
       Found == found_body ->
                get_used_variable_node_and_object_body(List);
       true ->
                get_used_variable_node_and_object_subexpr(List)
    end.


%% =====================================================================
%% @spec get_varnode(List::[node()])->[node()]
%% @doc
%% Gets variable nodes from the expression nodes.
%%
%% Parameter description:<pre>
%% <b>List</b> : The expression's node list.
%% </pre>
%% @end
%% =====================================================================
get_varnode(List)->
    case List of
        []->
	    [];
	[Head|Tail] ->
            ?GRAPH:path(Head,[{{sup,back},{kind,'==',variable}}])
                 ++ get_varnode(Tail)
    end.


%% =====================================================================
%% @spec get_used_variable_node_and_object_body(List::[node()])->
%%                                                   {[node()],[node()]}
%% @doc
%% Gets the used variables and it's variable object from the expressions.
%%
%% Parameter description:<pre>
%% <b>List</b> : The expression's node list.
%% </pre>
%% @end
%% =====================================================================
get_used_variable_node_and_object_body(List)->
    VarNode = get_varnode(List),  
    VarObject = get_varref(VarNode),
    {unique(VarNode,[]), VarObject}.


%% =====================================================================
%% @spec get_used_variable_node_and_object_subexpr(List::[node()])->
%%                                                   {[node()],[node()]}
%% @doc
%% Gets the used variables and it's variable object from the expressions.
%%
%% Parameter description:<pre>
%% <b>List</b> : The selected expression node list.
%% </pre>
%% @end
%% =====================================================================
get_used_variable_node_and_object_subexpr(List)->
  [Expr] = List,
  #expr{kind=Kind} = ?GRAPH:data(Expr),
  case Kind of
     variable ->
           VarObject = ?GRAPH:path(Expr,[varref]),
           {[Expr], VarObject};
     _ ->
           VarNodes = get_var_from_sub([Expr]),
	   VarObjects = get_varref(VarNodes),
           {unique(VarNodes,[]), VarObjects}
  end.

%% =====================================================================
%% @spec get_var_from_sub(List::[node()])-> [node()]
%% @doc
%% Gets the used variables from a (sub)expression.
%%
%% Parameter description:<pre>
%% <b>List</b> : An expression node list.
%% </pre>
%% @end
%% =====================================================================
get_var_from_sub(List)->
    case List of
        []->
	    [];
	[Head|Tail] ->
             VarList = ?GRAPH:path(Head,[{sub,{kind, '==', variable}}]),
             SubList = ?GRAPH:path(Head,[sub]),
             ClauseList = ?GRAPH:path(Head,[clause]),
             VarList ++ get_var_from_sub(SubList) 
                     ++ get_var_from_clause(ClauseList)
                     ++  get_var_from_sub(Tail)
    end.


%% =====================================================================
%% @spec get_var_from_clause(List::[node()])-> [node()]
%% @doc
%% Gets the used variables from a clause.
%%
%% Parameter description:<pre>
%% <b>List</b> : An expression node list.
%% </pre>
%% @end
%% =====================================================================
get_var_from_clause(List)->
    case List of
        []->
	    [];
	[Head|Tail] ->
             SubList = ?GRAPH:path(Head,[visib]),
              get_varnode(SubList) ++ get_var_from_clause(Tail)

    end.



%% =====================================================================
%% @spec get_inside_bound_variables(VarNode::[node()],
%%                                    VarObject::[node()]) -> [node()]
%%
%% @doc
%% Gets the bounded variables from the nodelist.
%%
%% Parameter description:<pre>
%% <b>VarNode</b> : The variable node list.
%% <b>VarObject</b> : The variable object node list.
%% </pre>
%% @end
%% ===================================================================== 
get_inside_bound_variables(VarNode, VarObject)->
    Bindings = get_bindings(VarObject),
    InsideBinding = VarNode -- (VarNode -- Bindings),
    InsideVarObjects = get_varref(InsideBinding),
    VariableRef = get_varref_back(InsideVarObjects),
    _NotBoundVarnode = VarNode -- (VarNode -- VariableRef).

%% =====================================================================
%% @spec get_varref(VarList::[node()])->[node()]
%% @doc
%% Gets the variable's object.
%%
%% Parameter description:<pre>
%% <b>VarObjectList</b> : The variable node list.
%% </pre>
%% @end
%% =====================================================================
get_varref(List)->
    case List of
        []->
	    [];
	[Head|Tail] ->
            ?GRAPH:path(Head,[varref]) ++ get_varref(Tail)
    end.


%% =====================================================================
%% @spec get_varref_back(VarObjectList::[node()])->[node()]
%% @doc
%% Gets the variable occurances.
%%
%% Parameter description:<pre>
%% <b>VarObjectList</b> : The variable object node list.
%% </pre>
%% @end
%% =====================================================================
get_varref_back(List)->
    case List of
        []->
	    [];
	[Head|Tail] ->
            ?GRAPH:path(Head,[{varref,back}]) ++ get_varref_back(Tail)
    end.

%% =====================================================================
%% @spec get_bindings(VarObjectList::[node()])->[node()]
%% @doc
%% Gets the variable binging.
%%
%% Parameter description:<pre>
%% <b>VarObjectList</b> : The variable object node list.
%% </pre>
%% @end
%% =====================================================================
get_bindings(VarObjectList)->
    case VarObjectList of
      []->
	    [];
      [Head|Tail] ->
            ?GRAPH:path(Head,[varbind]) ++ get_bindings(Tail)
    end.


%% =====================================================================
%% @spec get_var_name(List::[node()]) -> [node()]
%%
%% @doc
%% Gets the name of the variable from the variable object.
%%
%% Parameter description:<pre>
%% <b>List</b> : The expression's node list.
%% </pre>
%% @end
%% ===================================================================== 
get_var_name(List)->
    VarObject = get_varref(List),
    VarName = lists:map(fun(Var)->
                            {_,Name} = ?GRAPH:data(Var),
                            Name
                        end,
                        VarObject),
    unique(VarName,[]).

%% =====================================================================
%% @spec check_is_legal_body(ModuleNode::node(), Found::atom()) -> atom()
%%
%% @doc
%% Checks the starting and ending positions delimit a sequence of expression.
%%
%% Parameter description:<pre>
%% <b>ModuleNode</b> : The module node.
%% <b>Found</b> : An atom.
%% </pre>
%% @end
%% =====================================================================
check_is_legal_body(ModuleNode, Found)->
    case Found of
       not_found ->
                   error({not_legal_body, ModuleNode});
       _ ->
                   ok
    end.


%% =====================================================================
%% @spec check_is_legal_name(ModuleNode::node(),
%%        NewName::string(), Arity::integer()) -> atom()
%%
%% @doc
%% Checks that the function name is a legal function name, 
%% is not autoimported, is not exists and is not imported.
%%
%% Parameter description:<pre>
%% <b>ModuleNode</b> : The module node.
%% <b>NewName</b> : The name of the new function.
%% <b>Arity</b> : The arity of the new function.
%% </pre>
%% @end
%% ===================================================================== 
check_is_legal_name(ModuleNode, NewName, Arity)->
    check_is_function_name(NewName),
    check_is_autoimported(NewName),
    check_the_name_already_exists(ModuleNode, NewName, Arity),
    check_the_name_is_imported(ModuleNode, NewName, Arity).


%% =====================================================================
%% @spec check_is_function_name(NewName::string()) -> atom()
%%
%% @doc
%% Checks if the new function name is acceptable as a function name. 
%%  
%% Parameter description:<pre>
%% <b>NewName</b> : The name of the new function.
%% </pre>
%% @end
%% =====================================================================
check_is_function_name(NewName)->
    if
	hd(NewName) < 97; hd(NewName) > 122 ->
	    error({not_function_name_error, NewName});
	true ->
	    ok
    end.


%% =====================================================================
%% @spec check_is_autoimported(NewName::string()) -> atom()
%%
%% @doc
%% Checks if the new function name is acceptable as a function name.
%% Checks if it is not a reserved word, or an autoimported function's 
%% name, or a user forbidden name.
%%  
%% Parameter description:<pre>
%% <b>NewName</b> : The name of the new function.
%% </pre>
%% @end
%% =====================================================================
check_is_autoimported(_NewName)->  
                                  ok.


%% =====================================================================
%% @spec check_the_name_already_exists(ModuleNode::node(),
%%                         NewName::string(), Arity::integer()) -> atom()
%%
%% @doc
%% Checks if introducing a new function would clash with the 
%% existing ones. 
%%  
%% Parameter description:<pre>
%% <b>ModuleNode</b> : The module node.
%% <b>NewName</b> : The name of the new function.
%% <b>Arity</b> : The arity of the new function.
%% </pre>
%% @end
%% =====================================================================
check_the_name_already_exists(ModuleNode, NewName, Arity)->
    FunObjects = ?GRAPH:path(ModuleNode,[func]),
    ClashList = 
      lists:filter(fun(Fun)->
                       #func{name=Name,arity=ArityFun}=?GRAPH:data(Fun),
                      (Name == list_to_atom(NewName)) 
                                          and (Arity == ArityFun)
                   end,
                   FunObjects),
    case ClashList of
       []->
           ok;
       _ ->
            error({exists_error, {NewName, Arity}})
    end.


%% =====================================================================
%% @spec check_the_name_is_imported(ModuleNode::node(),
%%                         NewName::string(), Arity::integer()) -> atom()
%%
%% @doc
%% Checks if introducing a new function would clash with the 
%% imported ones. 
%%  
%% Parameter description:<pre>
%% <b>ModuleNode</b> : The module node.
%% <b>NewName</b> : The name of the new function.
%% <b>Arity</b> : The arity of the new function.
%% </pre>
%% @end
%% =====================================================================
check_the_name_is_imported(ModuleNode, NewName, Arity)->
    ImportedFunctions = ?GRAPH:path(ModuleNode,[funimp]),
    ClashList = lists:filter(fun(Fun)->
                               #func{name=Name, arity=ArityFun} = 
                                                        ?GRAPH:data(Fun),
                               (Name == list_to_atom(NewName)) 
                                                 and (Arity == ArityFun)
                             end,
                             ImportedFunctions),
    case ClashList of
       []->
           ok;
       _ ->
            error({import_error, {NewName, Arity}})
    end.


%% =====================================================================
%% @spec check_not_in_pattern_guard_macro_listgen( FromExpr::node(),
%%                     ToExpr::node(), FromToken::[token()])-> atom()
%%
%% @doc
%% Checks the expression id are not a part of  a guard sequence
%% or a pattern or a macro or a list generator node.
%%
%% Parameter description:<pre>
%% <b>FromExpr</b> : The pointed first expression node.
%% <b>ToExpr</b> : The pointed last expression node.
%% <b>FromToken</b> : The pointed first expression token.
%% </pre>
%% @end
%% =====================================================================
check_not_in_pattern_guard_macro_listgen(FromExpr, ToExpr, FromToken)->
    #expr{type=FromType, kind = FromKind} = ?GRAPH:data(FromExpr),
    #expr{type=ToType, kind = ToKind} = ?GRAPH:data(ToExpr),
    case FromType of
         pattern -> error({in_pattern, {}});
         guard ->  error({in_guard, {}});
         _  ->
               case ToType of 
                  pattern -> error({in_pattern, {}});
                  guard ->  error({in_guard, {}});
                  _  -> ok
               end
    end,
    case FromKind of
         list_gen -> error({in_listgen, {}});
         conjunction -> error({in_guard, {}});
         disjunction -> error({in_guard, {}});
         _  ->
               case ToKind of 
                  list_gen -> error({in_listgen, {}});
                  conjunction -> error({in_guard, {}});
                  disjunction -> error({in_guard, {}});
                  _  -> ok
               end
    end,
    if
       FromExpr /= ToExpr -> 
                           ok;
       true -> 
         List = ?GRAPH:path(FromToken,[{mbody,back}]),
         if 
             length(List) /= 0 -> 
                          error({in_macro, {}});
	     true ->
                     ok
         end
    end.


%% =====================================================================
%% @spec check_all_var_bound_ok(AllSupExpr::[node()],
%%             UseVarObject::[node()], BoundVarNode::[node()] ) -> atom()
%%
%% @doc
%% Checks that all variables with binding occurence in the selected  
%% sequence of expression not appear outside of this seqence.
%%
%% Parameter description:<pre>
%% <b>AllSupExpr</b> : The expression node list.
%% <b>UseVarObject</b> : The variable object list.
%% <b>BoundVarNode</b> : The bounded variable node list.
%% </pre>
%% @end
%% =====================================================================
check_all_var_bound_ok(AllSupExpr, UseVarObject, BoundVarNode)->
    {_AllVarNode, AllVarObject} = 
            get_used_variable_node_and_object_body(AllSupExpr),
    OutVarObject = AllVarObject -- UseVarObject,
    BoundVarObject = get_varref(BoundVarNode),
    BadList = OutVarObject -- (OutVarObject -- BoundVarObject),
    case BadList of
       []->
           ok;
       _ ->
            error({not_all_var_bound_ok, {BadList}})
    end.

%% =====================================================================
%% @spec perform_refactoring(Found::atom(),SourceNode::node(), 
%%                     NewName::string(), NotBoundVarName::[atom()],
%%                     ExprList::[node()],Top::node(),
%%                     FromToken::token(), ToToken::token())->atom()
%% @doc
%% Creates the new function and the application,
%% and inserts it to the graph. 
%%
%% Parameter description:<pre>
%% <b>Found</b> : The Found atom.
%% <b>SourceNode</b> : The node of the source.
%% <b>NewName</b> : The created function name.
%% <b>NotBoundVarName</b> : The not bounded variable names
%% <b>ExprList</b> : The selected expression's node list.
%% <b>Top</b> : The selected expression's clause.
%% <b>FromToken</b> : The pointed first token.
%% <b>ToToken</b> : The pointed last token.
%% </pre>
%% @end
%% =====================================================================
perform_refactoring(Found,SourceNode, NewName, NotBoundVarName,
                    ExprList, Top, FromToken, ToToken)->

    [SuperExpr] = ?ESG:path(hd(ExprList), [sup]),
    [Clause] = ?ESG:path(SuperExpr, [{visib,back}]),
    [Scope] = ?ESG:path(Clause, [scope]),
    [Func] = ?ESG:path(Scope, [{funcl,back}]),
    
    IndexForm = ?ESG:index(SourceNode, form,Func),

    {BeforeFromToken, AfterToToken,Index} = 
                     remove_nodes(Found,Top,ExprList,FromToken,ToToken),

    AppPatternNodes = create_parameters(NotBoundVarName),
    AppNameNode = create_name(NewName),
    AppNode = 
         create_application(Found,Top,AppPatternNodes,AppNameNode,Index),

    FunPatternNodes = create_parameters(NotBoundVarName),
    FunNameNode = create_name(NewName),
    {FunClause,Function} = 
         create_function(SourceNode,FunPatternNodes,FunNameNode,
                         ExprList,IndexForm),

    refac_token:token_update_application(AppNameNode, AppPatternNodes,
                                 AppNode,BeforeFromToken, AfterToToken),

    refac_token:token_update_function(FromToken,ToToken,FunNameNode,  
                                FunPatternNodes, FunClause,Function),
    ?ESG:close(),
    ok.

%% =====================================================================
%% @spec remove_nodes(Found::atom(), Top::node(), ExprList::[node()],
%%                    FromToken::token(),ToToken::token())
%%                                  ->{token(),token(),integer()}
%% @doc
%% Removes the selected part from the graph. 
%%
%% Parameter description:<pre>
%% <b>Found</b> : The Found atom.
%% <b>Top </b> : The selected expression's clause.
%% <b>ExprList </b> : The selected expression's node list.
%% <b>FromToken</b> : The pointed first token.
%% <b>ToToken</b> : The pointed last token.
%% </pre>
%% @end
%% =====================================================================
remove_nodes(Found,Top,ExprList,FromToken,ToToken)->
    [BeforeFromToken] = ?ESG:path(FromToken,[{next,back}]),
    [AfterToToken] = ?ESG:path(ToToken,[next]),
    case Found of
      found_body->
         Index = ?ESG:index(Top,body,hd(ExprList)),
         lists:map(fun(Node)->
                        ?ESG:remove(Top, body, Node)
                   end,ExprList);
      _->
         Before1 = ?ESG:path(Top,[{sub,back}]) ,
         case Before1 of
            [] -> 
                [Before]= ?ESG:path(Top,[{body,back}]),
                Index = ?ESG:index(Before,body,hd(ExprList)),
                lists:map(fun(Node)->
                            ?ESG:remove(Before, body, Node)
                          end,ExprList);
            _ ->
                [Before] = Before1,
                Index = ?ESG:index(Before,sub,hd(ExprList)),
                lists:map(fun(Node)->
                           ?ESG:remove(Before, sub, Node)
                          end,ExprList)
         end
    end,

    ?GRAPH:rmlink(BeforeFromToken,next,FromToken),
    ?GRAPH:rmlink(ToToken,next,AfterToToken),
   {BeforeFromToken, AfterToToken,Index}.


%% =====================================================================
%% @spec create_name(Name::string()) -> node()
%% @doc
%% Creates an atom with the new function name. 
%%
%% Parameter description:<pre>
%% <b>Name </b> : The new function name.
%% </pre>
%% @end
%% =====================================================================
create_name(Name)->
   ?ESG:create(#expr{kind = atom, value = list_to_atom(Name)}).



%% =====================================================================
%% @spec create_parameters(NameList::[string()])->{[node()]}
%% @doc
%% Creates variables. 
%%
%% Parameter description:<pre>
%% <b>NameList </b> : The not bounded variable's name 
%%                    from the selected part.
%% </pre>
%% @end
%% =====================================================================
create_parameters(NameList)->
   lists:map(fun(Name)->
                   ?ESG:create( #expr{kind = variable, value = Name})
             end, NameList).
    

%% =====================================================================
%% @spec create_application(Found::atom(),Top::node(),PatternNodes::[node()],
%%                          NameNode::node(),Index::integer())->node()
%% @doc
%% Creates the new application and inserts it to the graph.
%%
%% Parameter description:<pre>
%% <b>Found</b> : The Found atom.
%% <b>Top </b> : The selected part's clause node.
%% <b>PatternNodes </b> : The application parameter's nodes.
%% <b>NameNode </b> : The new application name's node.
%% <b>Index</b> : The number of the removed node in the graph.
%% </pre>
%% @end
%% =====================================================================
create_application(Found,Top,PatternNodes,NameNode,Index)->
    AppNode = ?ESG:create(#expr{kind = application}),
    case Found of
      found_body->
         ?ESG:insert(Top,{body,Index},AppNode);
      _->
         ?ESG:insert(Top,{sub,Index},AppNode)
    end,
    ?ESG:insert(AppNode, {sub,1}, NameNode),
    lists:foreach(fun(Pattern)->
                      ?ESG:insert(AppNode, sub, Pattern)
                  end, PatternNodes),
    AppNode.

%% =====================================================================
%% @spec create_function(SourceNode::node(),FunPatternNodes::[node()],
%%           FunNameNode::node(),ExprList::[node()],Index::integer())
%%                                                  ->{node(),node()}
%% @doc
%% Creates the new function and inserts it to the graph.
%%
%% Parameter description:<pre>
%% <b>SourceNode </b> : The source node.
%% <b>FunPatternNodes </b> : The function parameter's nodes.
%% <b>FunNameNode </b> : The new function name's node.
%% <b>ExprList</b> : The selected expression's node list.
%% <b>Index</b> : The number of the removed node in the graph.
%% </pre>
%% @end
%% =====================================================================
create_function(SourceNode,FunPatternNodes,FunNameNode,ExprList,IndexF)->
    Index = IndexF + 1,
    FunNode = ?ESG:create(#form{type = func}),
    ?ESG:insert(SourceNode, {form,Index}, FunNode),
    FunClause = ?ESG:create(#clause{type = funlc}),
    ?ESG:insert(FunNode,funcl,FunClause),
    ?ESG:insert(FunClause, name, FunNameNode),
    lists:foreach(fun(Pattern)->
                      ?ESG:insert(FunClause, pattern, Pattern)
                  end, FunPatternNodes),
    lists:foreach(fun(Expr)->
                      ?ESG:insert(FunClause, body, Expr)
                  end,ExprList),
    {FunClause,FunNode}.


%% =====================================================================
%% @spec error(Error::{term(), term()}) -> none()
%%
%% @doc
%% Handles error in refactorings. Currently it throws an exception, with
%% the given parameters. 
%% 
%% Parameter description:<pre>
%% <b>Error</b> : A two element tuple containing the type of the error,
%%                and a message to be able to make a better error message. 
%% </pre>
%% @end
%% ===================================================================== 
error({Error, Message})->
    throw({Error, Message}).


%% =====================================================================
%% @spec unique(List::[node()], Unique::[node()]) -> [node()]
%%
%% @doc
%% Create a unique list from List.
%% 
%% Parameter description:<pre>
%% <b>List</b>: A node list.
%% <b>Unique</b>: A node list with unique nodes.
%% </pre>
%% @end
%% ===================================================================== 
unique(List,Unique)->
    case List of
      [] ->
         Unique ;
      _ ->
         Element = member(hd(List),Unique),
         case Element of
            true  -> unique(tl(List), Unique );
	    false ->
                 unique(tl(List),Unique ++ [hd(List)])
         end
    end.


%% =====================================================================
%% @spec member(Elem::node(), List::[node()]) -> bool()
%%
%% @doc
%% Returns with true if the Elem is a member of the List.
%% 
%% Parameter description:<pre>
%% <b>Elem</b>: A node.
%% <b>List</b>: A node list.
%% </pre>
%% @end
%% ===================================================================== 
member(Elem, List)->
    case List of
      [] -> false;
      _  ->
          case hd(List) of
            Elem -> true;
	    _    -> member(Elem,tl(List))
          end
    end.
