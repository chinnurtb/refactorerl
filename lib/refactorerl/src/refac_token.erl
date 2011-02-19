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

-module(refac_token).
-vsn("$Rev: 1335 $").

-export([token_update_function/6,
	 token_update_application/5,
	 token_update_funexpr/5]).

-include("refactorerl.hrl").
%% =====================================================================
%% @spec token_update_function(FromToken::token(),ToToken::token(),
%%         FunNameNode::token(), FunPatternNodes::[token()],
%%         FunClause::node(),Function::node())->ok
%% @doc
%% Creates  the tokens for the new function and the application, 
%% and insert it to the graph.
%% Parameter description:<pre>
%% <b>FromToken</b>: The pointed first token.
%% <b>ToToken</b>: The pointed last token.
%% <b>FunNameNode</b>: The function name node.
%% <b>FunPatternNodes</b>: The function pattern nodes.
%% <b>FunClause</b>: The selected function clause.
%% <b>Function</b>: The created function node.
%% </pre>
%% @end
%% @TODO: #token  ->  #lex(#token)
%% =====================================================================
token_update_function(FromToken,ToToken,FunNameNode,FunPatternNodes,
                      FunClause,Function)->
    FunPatternTokens = create_var_token(FunPatternNodes),
    FunNameToken = create_name_token(FunNameNode,""),
    FunClosePar = 
        insert_parameter_with_parenthesis(FunNameToken,FunPatternTokens,
                                          clex,FunClause),
    ?GRAPH:mklink(Function,ffirst, FunNameToken) ,
    ?GRAPH:mklink(FunClause,cfirst, FunNameToken) ,
    Arrow = ?ESG:create(#lex{type = token, data = #token{type=arrow,
                        text = "->",value = '->', prews=" ", postws="\n"}}),
    ?ESG:insert(FunClause,clex,Arrow),    
    ?GRAPH:mklink(FunClosePar,{next,1},Arrow),
    ?ESG:insert(FunClause,clex,FunClosePar), 
    ?GRAPH:mklink(Arrow,{next,1},FromToken),
    Stop = ?ESG:create(#lex{type = token, data = #token{type=stop,
                       text =".",value = '.', postws="\n\n"}}),
    ?GRAPH:mklink(ToToken,{next,1},Stop),

    ?ESG:insert(Function,flex,Stop),
    ?ESG:insert(Function,flast,Stop).

%% =====================================================================
%% @spec token_update_application(AppNameNode::token(), 
%%                    AppPatternNodes::[token()],AppNode::node(), 
%%                    BeforeFromToken::token(),AfterToToken::token())->ok
%% @doc
%% Creates the tokens for the new application and inserts it to the graph.
%% Parameter description:<pre>
%% <b>AppNameNode</b>: The application name node.
%% <b>AppPatternNodes</b>: The application pattern nodes.
%% <b>AppNode</b>: The created application node.
%% <b>BeforeFromToken</b>: The token before the FromToken.
%% <b>AfterToToken</b>: The token after the ToToken.
%% </pre>
%% @end
%% =====================================================================
token_update_application(AppNameNode,AppPatternNodes,AppNode,
                         BeforeFromToken,AfterToToken)->
    AppPatternTokens = create_var_token(AppPatternNodes),
    AppNameToken = create_name_token(AppNameNode," "),
    ?GRAPH:mklink(BeforeFromToken,next,AppNameToken),
    ?ESG:insert(AppNode,efirst,AppNameToken),
    AppClosePar = 
        insert_parameter_with_parenthesis(AppNameToken,AppPatternTokens,
                                          elex,AppNode),
    ?GRAPH:mklink(AppClosePar,next,AfterToToken),
    ?ESG:insert(AppNode,elast,AppClosePar).


%% =====================================================================
%% @spec insert_parameter_with_parenthesis(NameToken::token(),
%%         PatternTokens::[token()],Text::atom(),
%%         From::token())-> token()
%% @doc
%% Insert parameters into the graph with parenthesis.
%%
%% Parameter description:<pre>
%% <b>NameToken</b>: The created name token.
%% <b>PatternTokens</b>: The application or function pattern tokens.
%% <b>Text</b>: The label of the inserted edge.
%% <b>From</b>: The application or the function node.
%% </pre>
%% @end
%% =====================================================================
insert_parameter_with_parenthesis(PrevToken, PatternTokens, Link, From) ->
    OpenPar = ?ESG:create(#lex{type = token, 
			       data = #token{
				 type  = op_paren,
				 text  = "(", 
				 value = '(',
				 prews = " "}}),
    ClosePar = ?ESG:create(#lex{type = token, 
				data = #token{
				  type  = op_paren,
				  text  = ")",
				  value = ')',
				  prews = " "}}),
    ?GRAPH:mklink(PrevToken, {next, 1}, OpenPar),
    ?ESG:insert(From, Link, OpenPar),
    case PatternTokens of
	[] -> 
	    After = OpenPar;
	_ -> 
	    ?GRAPH:mklink(OpenPar, {next, 1}, hd(PatternTokens)),
	    After = insert_parameters(hd(PatternTokens),
				      tl(PatternTokens),
				      Link,
				      From)
    end,
    ?GRAPH:mklink(After, {next,1}, ClosePar),
    ?ESG:insert(From, Link, ClosePar),
    ClosePar.


%% =====================================================================
%% @spec insert_parameters(After::token(),List::[token()],Text::atom(),
%%                                              From::token())-> token()
%% @doc
%% Insert the function or the application parameters.
%%
%% Parameter description:<pre>
%% <b>After</b>: The last token.
%% <b>List</b>: The application or function pattern tokens.
%% <b>Text</b>: The label of the inserted edge.
%% <b>From</b>: The application or the function node.
%% </pre>
%% @end
%% =====================================================================
insert_parameters(After,List,Text,From)->
    case List of
      []->
	 After;   
      [Head|Tail]->
         Comma = ?ESG:create(#lex{type = token,
				  data = #token{
				    type  =comma,
				    text  =",", 
				    value = ','}}),
         ?GRAPH:mklink(After,{next,1},Comma),
         ?ESG:insert(From,Text,Comma),
         ?GRAPH:mklink(Comma,{next,1},Head),
         insert_parameters(Head,Tail,Text,From)
    end.

%% =====================================================================
%% @spec create_var_token(VarNode::[node()])->{[token()]}
%% @doc
%% Creates variable node tokens. 
%%
%% Parameter description:<pre>
%% <b>VarNode </b> : Expression node list.
%% </pre>
%% @end
%% =====================================================================
create_var_token(VarNode)->
    lists:map(
      fun(Var)->
	      #expr{value=Name}=?GRAPH:data(Var),
	      Token = ?ESG:create(#lex{type = token, 
				       data = #token{
					 type  = variable,
					 text  = Name,
					 value = Name,
					 prews = " "}}),
	      ?ESG:insert(Var, efirst, Token),
	      ?ESG:insert(Var, elex, Token),
	      ?ESG:insert(Var, elast, Token),
	      Token
      end, VarNode).

%% =====================================================================
%% @spec create_name_token(VarNode::[node()],Prefix::string())->{[token()]}
%% @doc
%% Creates variable node tokens. 
%%
%% Parameter description:<pre>
%% <b>NameNode </b> : Name node.
%% <b>Prefix </b> : The token's prefix.
%% </pre>
%% @end
%% =====================================================================
create_name_token(NameNode,Prefix)->
    #expr{value=Name}=?GRAPH:data(NameNode),
    NameToken = ?ESG:create(#lex{type = token,
				 data = #token{
				   type  = atom, 
				   text  = atom_to_list(Name),
                                   value = Name, 
				   prews = Prefix}}),
    ?ESG:insert(NameNode, efirst, NameToken),
    ?ESG:insert(NameNode, elex, NameToken),
    ?ESG:insert(NameNode, elast, NameToken),
    NameToken.

%% =====================================================================
%% FOR FUNEXP EXTRACTING
%% =====================================================================

token_update_funexpr(FunClause, FunPatternNodes, FunExpr, First, Next)->
    FunPatternTokens = create_var_token(FunPatternNodes),
    FunClauseParameters = 
        insert_parameter_with_parenthesis(First, FunPatternTokens,
                                          clex, FunClause),
    Arrow = ?ESG:create(#lex{type = token, 
			     data = #token{
			       type = arrow,
			       text   = "->",
			       value  = '->',
                               prews  = " ", 
			       postws = " "}}),
    End = ?ESG:create(#lex{type = token,
			   data = #token{
			     type  = 'end',
			     text  = "end",
			     value = 'end',
			     prews = " "}}),
    ?ESG:insert(FunClause, clex, Arrow),
    ?ESG:insert(FunClauseParameters, next, Arrow),
    ?ESG:insert(FunExpr, elex, End),
    ?ESG:insert(FunExpr, elast, End),
    if
	Next /= [] -> ?ESG:insert(End, next, Next);
	true -> ok
    end,
    [Arrow, End].

