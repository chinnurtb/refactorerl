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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the inline function refactoring.
%%%
%%% @author István Bozó <bozo_i@inf.elte.hu>


-module(referl_tr_inline_fun).
-vsn("$Rev: 1991 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/2]).
%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

-record(refst, {filename, pos, file, module, token, form, index,
                expr, appnode, qualifier, parameters, funnode,
                funclauses, guards, funfile, funpatternlists,
                funpatternvarlists, appclause,
                appvariables, funbodynodelists, unusedvarlists, boundvarlists,
                varpairlists, matchpairlists, appnodelists, appparent, plink,
                imported, exported, localapps}).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface

%% @spec do(string(), integer()) -> ok
%%
%% @doc Inline the function application in the source file `FileName' on the
%% selected position `Pos'.
do(FileName, Pos) ->
    ?TRANSFORM:do(?MODULE,{FileName, Pos}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FileName, Pos})->
    #refst{ filename = FileName, pos  = Pos}.

%% @private
steps() ->
    [fun file_node/1,
     fun module_node/1,
     fun current_token/1,
     fun expr_type_kind/1,
     fun app_and_module/1,
     fun get_parameters/1,
     fun get_fun_data/1,
     fun get_function_bodynodes/1,
     fun get_fun_module/1,
     fun get_appclause_variables/1,
     fun get_attr_pairs/1,
     fun get_bound_and_unused_vars_from_pattern/1,
     fun check_var_collisions/1,
     fun get_app_nodes_from_clauses/1,
     fun get_applications/1,
     fun check_applications/1,
     fun get_parent_node/1,
     fun check_macros_records/1].

%% Removes the application and inserts a function clause(s) instead.
%% @private
transform(#refst{ file = File, funclauses = FunClauses, guards = Guards,
                  appparent = AppParent, appnode = AppNode, plink = PLink,
                  funbodynodelists = FunBodyNodeLists, funfile = FunFile,
                  funpatternlists = FunPatternLists, parameters = Parameters,
                  varpairlists = VarPairLists, matchpairlists = MatchPairLists,
                  unusedvarlists = _UnusedVarLists,
                  boundvarlists = BoundVarLists,
                  imported = Imported, exported = Exported}) ->
    ClauseNr = length(FunClauses),
    GuardedOrAmbiguous = 
        (length(lists:flatten(Guards)) /= 0) 
        or (is_var_rename_ambiguous(VarPairLists)),
    SubsNodeNr = length(hd(FunBodyNodeLists) ++
                        lists:flatten(hd(MatchPairLists))),
    ReplaceType = get_replace_type(AppParent),
    case {ClauseNr,GuardedOrAmbiguous,SubsNodeNr,ReplaceType} of
        {1, false, 1, simple} -> %% simple replace
            [CopiedNodes] =
                replicate_nodes_and_rename_vars(FunBodyNodeLists,
                                                BoundVarLists,
                                                VarPairLists),
            ?SYNTAX:replace(AppParent,{node, AppNode}, 
                            [{PLink, hd(CopiedNodes)}]);
        {1, false, 1, block} -> %% simple replace with parenthesis
            [CopiedNodes] =
                replicate_nodes_and_rename_vars(FunBodyNodeLists,
                                                BoundVarLists,
                                                VarPairLists),
            Paren =
                ?SYNTAX:create(#expr{kind='parenthesis'},
                               [{sub,hd(CopiedNodes)}]),
            ?SYNTAX:replace(AppParent,{node, AppNode}, [{PLink, Paren}]);
        {1, false, _, simple} -> %% simple replace
            Match =
                create_match_expr(hd(MatchPairLists),
                                  hd(BoundVarLists),
                                  hd(VarPairLists)),
            [CopiedNodes] =
                replicate_nodes_and_rename_vars(FunBodyNodeLists,
                                                BoundVarLists,
                                                VarPairLists),
            Body = [ {PLink, B} || B <- Match ++ CopiedNodes],
            ?SYNTAX:replace(AppParent,{node, AppNode}, Body);
        {1, false, _, _} -> %% block replace
            Match =
                create_match_expr(hd(MatchPairLists),
                                  hd(BoundVarLists),
                                  hd(VarPairLists)),
            [CopiedNodes] =
                replicate_nodes_and_rename_vars(FunBodyNodeLists,
                                                BoundVarLists,
                                                VarPairLists),
            Block =
                ?SYNTAX:create(#clause{kind=block},
                               [{body, Match ++ CopiedNodes}]),
            BlockExpr =
                ?SYNTAX:create(#expr{kind=block_expr},
                               [{exprcl,Block}]),
            ?SYNTAX:replace(AppParent,{node, AppNode},
                            [{PLink,BlockExpr}]);
        _ ->
            ClauseDatas =
                create_data_for_case_expr(FunPatternLists, Guards, 
                                          FunBodyNodeLists,
                                          BoundVarLists, VarPairLists),
            CopiedNodes = 
                lists:flatten([ [F] ++ [S] ++ [T] || {F, S, T} <- ClauseDatas]),
            replace_app_with_case_expr(AppParent, AppNode, PLink,
                                       Parameters, ClauseDatas)
    end,
    ?ESG:close(),
    case File of
        FunFile ->
            ok;
        _ ->
            maintain_applications(CopiedNodes, Imported, Exported)
    end,
    ?ESG:close(),
    {[File], ok}.

get_replace_type(AppParent)->
    case ?ESG:data(AppParent) of
        #clause{kind = expr} ->
            UpperNode =
                ?GRAPH:path(AppParent,
                            [{{exprcl,back},
                              {{kind, '==', list_comp},'or',
                               {{kind, '==', list_gen},'or',
                                {kind, '==', filter}}}}]),
            if UpperNode == [] -> simple;
               true -> block
            end;
        #clause{} ->
            simple;
        _ ->
            block
    end.

is_var_rename_ambiguous(VarPairLists)->
    Result =
        lists:flatten([ is_ambiguous(VarPairList) || VarPairList <- 
                                                         VarPairLists]),
    lists:member(true, Result).

is_ambiguous(VarPairList) ->
    FromNames =
        lists:usort([ From || {From,_} <- VarPairList]),
    length(VarPairList) /= length(FromNames).

create_data_for_case_expr(FunPatternLists, Guards, FunBodyNodeLists, 
                          BoundVarLists, VarPairLists)->
    CasePatterns =
        replicate_nodes_and_rename_vars(FunPatternLists, BoundVarLists, 
                                        VarPairLists),
    CaseClauseGuards =
        replicate_guards_and_rename_vars(Guards, BoundVarLists, 
                                         VarPairLists),
    CaseClauseBodies =
        replicate_nodes_and_rename_vars(FunBodyNodeLists, BoundVarLists, 
                                        VarPairLists),
    lists:zip3(CasePatterns, CaseClauseGuards,
               CaseClauseBodies).


get_pair2(Node, NodeList)->
    {value, {_, NodePair}} =
        lists:keysearch(Node, 1, NodeList),
    NodePair.
    

replicate_guards_and_rename_vars(Guards, BoundVarLists, VarPairLists) ->
    Zipped = lists:zip3(Guards, BoundVarLists, VarPairLists),
    lists:map(
      fun({Guard, BoundVars, VarPairList})->
              case Guard of
                  [] -> [];
                  _ ->
                      Nodes =
                          ?ESG:copy(Guard),
                      CopiedNode = get_pair2(Guard, Nodes),
                      OldVariables =
                          [ OldNode || {OldNode, _} <- Nodes,
                                       lists:member(OldNode, BoundVars)],
                      [ begin
                            NewVarNode = get_pair2(Variable, Nodes),
                            OldVarToken = 
                                ?GRAPH:path(Variable,[elex]),
                            NewVarToken =
                                get_pair2(OldVarToken, Nodes),
                            %%update name
                            #expr{value = OldName} = ?GRAPH:data(Variable),
                            NewName = get_pair(OldName, VarPairList),
                            update_var_name(NewVarNode, NewVarToken, 
                                            NewName)
                        end || Variable <- OldVariables],
                      CopiedNode
              end
      end,Zipped).


replicate_nodes_and_rename_vars(NodeLists, BoundVarLists, VarPairLists) ->
    Zipped =
        lists:zip3(NodeLists, BoundVarLists, VarPairLists),
    [ [ begin
            Nodes =
                ?ESG:copy(Node),
            CopiedNode = get_pair2(Node, Nodes),
            OldVariables =
                [ OldNode || {OldNode, _} <- Nodes,
                             lists:member(OldNode, BoundVars)],
            [ begin
                  NewVarNode = get_pair2(Variable, Nodes),
                  [OldVarToken] = 
                      ?GRAPH:path(Variable,[elex]),
                  NewVarToken =
                      get_pair2(OldVarToken, Nodes),
                  %%update name
                  #expr{value = OldName} = ?GRAPH:data(Variable),
                  NewName = get_pair(OldName, VarPairList),
                  update_var_name(NewVarNode, NewVarToken, 
                                  NewName)
              end || Variable <- OldVariables],
            CopiedNode
        end
        || Node <- BodyNodes]
      || {BodyNodes, BoundVars, VarPairList} <- Zipped].

create_match_expr(MatchPairList, BoundVars, VarPairList)->
    LeftSideNodes =
        [begin
            Nodes =
                ?ESG:copy(Left),
            LeftNode = get_pair2(Left, Nodes),
            OldVariables =
                [ OldNode || {OldNode, _} <- Nodes,
                             lists:member(OldNode, BoundVars)],
            [ begin
                  NewVarNode = get_pair2(Variable, Nodes),
                  [OldVarToken] = 
                      ?GRAPH:path(Variable,[elex]),
                  NewVarToken =
                      get_pair2(OldVarToken, Nodes),
                  %%update name
                  #expr{value = OldName} = ?GRAPH:data(Variable),
                  NewName = get_pair(OldName, VarPairList),
                  update_var_name(NewVarNode, NewVarToken, 
                                  NewName)
              end || Variable <- OldVariables],
             LeftNode
         end
         || {Left, _} <- MatchPairList],
    RightSideNodes =
        [ begin
              RightSide = ?ESG:copy(Right),
              get_pair2(Right,RightSide)
          end
          || {_, Right} <- MatchPairList ],

    case {LeftSideNodes, RightSideNodes} of
        {[],[]} ->
            [];
        {[LeftNode],[RightNode]} ->
            [?SYNTAX:create(#expr{kind=match_expr},
                            [{sub,LeftNode},{sub,RightNode}])];
        _ ->
            LeftSide =
                ?SYNTAX:create(#expr{kind=tuple},
                               [{sub , LeftSideNodes}]),
            RightSide =
                ?SYNTAX:create(#expr{kind=tuple},
                               [{sub , RightSideNodes}]),
            [?SYNTAX:create(#expr{kind=match_expr},
                            [{sub,LeftSide},{sub,RightSide}])]
    end.

replace_app_with_case_expr(AppParent, AppNode, PLink, Parameters, ClauseDatas)->
    ArgNode =
        case Parameters of
            [Par] ->
                Par;
            _ ->
                ?SYNTAX:create(#expr{kind=tuple},
                               [{sub , Parameters}])
        end,
    HeadClause =
        ?SYNTAX:create(#clause{kind=expr},[{body,ArgNode}]),
    Branches =
        lists:map(fun({FunPattern, Guard, FunBodyNodes})->
                          Pattern =
                              case FunPattern of
                                  [FP] ->
                                      FP;
                                  _ ->
                                      ?SYNTAX:create(#expr{kind=tuple},
                                                     [{sub,FunPattern}])
                              end,
                          case Guard of
                              [] ->
                                  ?SYNTAX:create(#clause{kind=pattern},
                                                 [{pattern, Pattern},
                                                  {body, FunBodyNodes}]);
                              _ ->
                                  ?SYNTAX:create(#clause{kind=pattern},
                                                 [{pattern, Pattern},
                                                  {guard, Guard},
                                                  {body, FunBodyNodes}])
                          end
                  end,ClauseDatas),
    CaseExpr =
        ?SYNTAX:create(#expr{kind=case_expr},
                       [{headcl, HeadClause},
                        {exprcl, Branches}]),
    ?SYNTAX:replace(AppParent,{node,AppNode},[{PLink, CaseExpr}]).

get_bound_and_unused_vars_from_pattern(St = #refst{funpatternlists =
                                                   FunPatternLists})->
    FunPatternVars =
        [ ?SEMINF:vars(sub,PatternNodes) || PatternNodes <- FunPatternLists],
    VarObjs =
        [ ?SEMINF:varbinds(PatternVars) || PatternVars <- FunPatternVars ],
    VarNodes =
        [?SEMINF:varbinds_back(VarObj) ++ ?SEMINF:varrefs_back(VarObj)
          || VarObj <- VarObjs],
    UnusedVarNodes =
        [[ ?SEMINF:varbinds_back([Obj]) ||
             Obj <- VarObj,
             length(?SEMINF:varrefs_back([Obj])) == 0]
         || VarObj <- VarObjs],
    St#refst{boundvarlists = VarNodes, unusedvarlists = UnusedVarNodes,
             funpatternvarlists = FunPatternVars}.

update_var_name(VarNode, VarToken, NewName)->
    case NewName of
        no_pair ->
            ok;
        _ ->
            Data = ?GRAPH:data(VarNode),
            Lex = ?GRAPH:data(VarToken),
            #lex{data = TokenData = #token{value = _OldName}} = Lex,
            NewToken =
                Lex#lex{data =
                        TokenData#token{value = NewName,
                                        text = NewName}},
            ?GRAPH:update(VarNode, Data#expr{value = NewName}),
            ?GRAPH:update(VarToken, NewToken)
    end.

maintain_applications(Nodes, Imported, Exported)->
    FunData = Imported ++ Exported,
    AppNodes = 
        ?SEMINF:apps(sub,Nodes),
    [begin
         [FunObj] = 
             ?GRAPH:path(App,[funref]),
         #func{name = Name, arity = Arity} =
             ?GRAPH:data(FunObj),
         case get_mod({Name,Arity}, FunData) of
             [] ->
                 ok;
             Mod ->
                 insert_module_qualifier(Mod, App)
         end
     end 
     || App <- AppNodes].

insert_module_qualifier(ModName, AppNode)->
    AppSubExpr = ?GRAPH:path(AppNode,[{sub,1}]),
    #expr{value = AppName} =
        ?GRAPH:data(hd(AppSubExpr)),
    NewAppSub =
        ?SYNTAX:create(#expr{kind='atom'},[atom_to_list(AppName)]),
    ModuleExpr =
        ?SYNTAX:create(#expr{kind='atom'},[atom_to_list(ModName)]),
    ModuleQualifier =
        ?SYNTAX:create(#expr{kind='infix_expr',value=':'},
                       [{sub,[ModuleExpr, NewAppSub]}]),
    ?SYNTAX:replace(AppNode,{node,hd(AppSubExpr)},[{sub, [ModuleQualifier]}]).

get_mod(_, []) ->
    [];
get_mod({Name,Arity},[{Mod, Name, Arity}|_Tail])->
    Mod;
get_mod({Name,Arity},[_| Tail]) ->
    get_mod({Name,Arity},Tail).

get_pair(Name,[{Name,NewName}|_Rest])->
    NewName;
get_pair(Name,[_|Rest]) ->
    get_pair(Name,Rest);
get_pair(_Name,[]) ->
    no_pair.

%%% ============================================================================
%%% Implementation

file_node(St = #refst{filename = FileName}) ->
    case ?SYNTAX:file(FileName) of
        {file, File} -> St#refst{file = File};
        _            -> throw("File is not present in the database!")
    end.

module_node(St = #refst{file = File}) ->
    case ?GRAPH:path(File, [moddef]) of
        [ModuleNode] -> St#refst{module = ModuleNode};
        _            -> throw("Module is not present!")
    end.

current_token(St = #refst{file = File, pos = Pos}) ->
    Token = ?LEX:token_by_pos(File, Pos),
    case Token of
        illegal -> 
            throw(?MISC:format("The selected position ~p does not indicate" ++
                               " a function application!",[Pos]));
        _       -> ok
    end,
    {_, TokenNode} = Token,
    St#refst{token = TokenNode}.

expr_type_kind(St = #refst{token = Token, pos = Pos}) ->
    [{_,Expr}] = ?ESG:parent(Token),
    Data = ?ESG:data(Expr),
    case Data of
        #expr{kind = Kind} ->
            case Kind of
                atom ->
                    St#refst{ expr = Expr};
                infix_expr ->
                    SubExpr =
                        hd(lists:reverse(?ESG:path(Expr,[sub]))),
                    St#refst{ expr = SubExpr};
                _ -> 
                    throw(?MISC:format("The selected position ~p does not" ++ 
                                       " indicate a function application!"
                                       ,[Pos]))
            end;
        _ -> 
            throw(?MISC:format("The selected position ~p does not indicate" ++
                               " a function application!",[Pos]))
    end.

app_and_module(St = #refst{expr = Expr, pos = Pos}) ->
    Node = ?ESG:path(Expr, [{sub,back}]),
    if Node == [] -> 
            throw(?MISC:format("The selected position ~p does not indicate" ++
                               " a function application!",[Pos]));
        true -> ok
    end,
    #expr{kind = Kind} = ?ESG:data(hd(Node)),
    CurrNode = hd(Node),
    {Qualifier, AppNode} =
        case Kind of
            infix_expr ->
                App = hd(?ESG:path(CurrNode,[{sub,back}])),
                {Node, App};
            application -> {undefined, CurrNode};
            _           -> 
                throw(?MISC:format("The selected position ~p does not indicate"
                                   ++ " a function application!",[Pos]))
        end,
    [{PLink,_}] = ?ESG:parent(AppNode),
    St#refst{ appnode = AppNode, qualifier = Qualifier, plink = PLink}.

get_parameters(St = #refst{appnode = AppNode}) ->
    Subs = ?ESG:path(AppNode, [sub]),
    St#refst{ parameters = tl(Subs)}.

get_fun_data(St = #refst{appnode = AppNode}) ->
    Node = ?ESG:path(AppNode, [funref, {fundef,back}]),
    case Node of
        [] ->
            throw("The definition module of the function is not present in the"
                  ++ " database!");
        _ ->
            [FunNode] = Node,
            FunClauses = ?ESG:path(FunNode,[funcl]),
            FunPatternLists =
                [?ESG:path(Clause,[pattern])|| Clause <- FunClauses],
            Guards =
                lists:map(fun(Clause)->
                    Guard = ?ESG:path(Clause,[guard]),
                    case Guard of
                        [] -> [];
                        _  -> hd(Guard)
                    end
                end,FunClauses),
            St#refst{funnode = FunNode, funclauses = FunClauses,
                     funpatternlists = FunPatternLists, guards = Guards}
    end.

get_appclause_variables(St = #refst{appnode = AppNode}) ->
    [ClauseNode] = ?ESG:path(AppNode, [sup,{visib, back}]),
    VarNode = lists:flatten(?SEMINF:vars(clause,[ClauseNode])),
    St#refst{appvariables = VarNode, appclause = ClauseNode}.


get_attr_pairs(St = #refst{parameters = Parameters, 
                           funpatternlists = FunPatternLists}) ->
     AttrPairLists =
        lists:map(fun(Elem)->
                          get_attr_pairs(Elem,Parameters)
                  end,FunPatternLists),
    VarPairs =
        [[ Pair || {Kind, Pair} <- AttrPairList, Kind == variable]
         || AttrPairList <- AttrPairLists],
    MatchPairs =
        [[ Pair || {Kind, Pair} <- AttrPairList, Kind == match]
         || AttrPairList <- AttrPairLists],

    St#refst{varpairlists = VarPairs, matchpairlists = MatchPairs}.


%%supporting function for previous

get_attr_pairs(Patterns, Parameters)->
    Zipped = lists:zip(Patterns, Parameters),
    lists:flatten(
      lists:map(
        fun({FunArg, AppArg})->
                #expr{kind = FunArgType,value = FunArgName} = ?ESG:data(FunArg),
                #expr{kind = AppArgType,value = AppArgName} = ?ESG:data(AppArg),
                case FunArgType of
                    AppArgType when AppArgType == variable ->
                        [{variable,{FunArgName,AppArgName}}];

                    AppArgType when (AppArgType == cons ) ->
                        FunArgSub = ?ESG:path(FunArg,[sub]),
                        AppArgSub = ?ESG:path(AppArg,[sub]),
                        case length(FunArgSub)==length(AppArgSub) of
                            true ->
                                SubNodes =
                                    get_attr_pairs(FunArgSub,AppArgSub),
                                case [{Kind, Rest} || {Kind, Rest} <- SubNodes, 
                                                      Kind == match] of
                                    [] ->
                                        [{Kind, Rest} || {Kind, Rest} 
                                                             <- SubNodes, 
                                                         Kind == variable];
                                    _ ->
                                        [{match,{FunArg, AppArg}}] ++
                                            [{Kind, Rest} || {Kind, Rest} 
                                                                 <- SubNodes, 
                                                             Kind == variable]
                                end;
                            _ ->
                                [{match,{FunArg,AppArg}}]
                        end;
                    AppArgType when (AppArgType == record_expr) ->
                        [{match,{FunArg,AppArg}}];
                    AppArgType when (AppArgType == match_expr) ->
                        FunArgSub = ?ESG:path(FunArg,[sub]),
                        AppArgSub = ?ESG:path(AppArg,[sub]),
                        SubNodes =
                            get_attr_pairs(FunArgSub,AppArgSub),
                        [{match,{FunArg,AppArg}}] ++
                            [ {Kind,Rest} || {Kind, Rest} <- SubNodes,
                                             Kind == variable];
                    AppArgType ->
                        FunArgSub = ?ESG:path(FunArg,[sub]),
                        AppArgSub = ?ESG:path(AppArg,[sub]),
                        case length(FunArgSub) == length(AppArgSub) of
                            true ->
                                get_attr_pairs(FunArgSub,AppArgSub);
                            _Other ->
                                [{match,{FunArg,AppArg}}]
                        end;
                    _Other ->
                        [{match,{FunArg,AppArg}}]
                end
        end,Zipped)).

get_parent_node(St = #refst{appnode = AppNode}) ->
    [{_,Parent}] = ?ESG:parent(AppNode),
    St#refst{appparent = Parent}.

get_app_nodes_from_clauses(St = #refst{funclauses = FunClauses}) ->
    AppNodes =
        lists:map(fun(Clause)->
            ?SEMINF:apps(clause,[Clause])
        end,FunClauses),
    St#refst{appnodelists = AppNodes}.

get_fun_module(St = #refst{funnode = FunNode}) ->
    FunFile = ?ESG:path(FunNode,[{form,back}]),
    case FunFile of
        [] -> throw("The definition module of the function is not loaded to the database!");
        _  -> St#refst{funfile = hd(FunFile)}
    end.

get_data_about_app(AppNode)->
    [PossibleQual] = ?ESG:path(AppNode,[{sub,1}]),
    case ?ESG:data(PossibleQual) of
        #expr{value = ':'} ->
            [];
        _ ->
            [FunObj] = ?ESG:path(AppNode,[funref]),
            ExpObj = ?ESG:path(FunObj,[{funexp, back}]),
            ImpObj = ?ESG:path(FunObj,[{funimp, back}]),
            #func{name = Name, arity = Arity} =
                ?ESG:data(FunObj),
            case {ExpObj,ImpObj} of
                {[],[]} ->
                    {undefined,{undefined, Name, Arity}};
                {[Mod],[]} ->
                    #module{name = ModName} = ?ESG:data(Mod),
                    {exported, {ModName, Name, Arity}};
                {[],[_Mod]} ->
                    [ModObj] = ?ESG:path(FunObj,[{func,back}]),
                    #module{name = ModName} = ?ESG:data(ModObj),
                    {imported, {ModName, Name, Arity}}
            end
    end.

get_applications(St = #refst{file = File, funfile = FunFile,
                                         appnodelists = AppNodeLists}) ->
    case File of
        FunFile ->
            St;
        _ ->
            AppLists =
                [lists:flatten([ get_data_about_app(AppNode) 
                                 || AppNode <- AppNodeList])
                 || AppNodeList <- AppNodeLists],
            Undefined =
                [ [ Fun || {Kind, Fun} <- AppList, Kind == undefined]
                  || AppList <- AppLists],
            Exported =
                lists:usort(
                  lists:flatten(
                    [ [ Fun || {Kind, Fun} <- AppList, Kind == exported]
                      || AppList <- AppLists])),
            Imported =
                lists:usort(
                  lists:flatten(
                    [ [ Fun || {Kind, Fun} <- AppList, Kind == imported]
                      || AppList <- AppLists])),
            St#refst{imported = Imported, exported = Exported,
                     localapps = Undefined}
    end.

get_function_bodynodes(St = #refst{funclauses = FunClauses})->
    BodyNodeLists =
        [?ESG:path(Clause,[body]) || Clause <- FunClauses],
    St#refst{funbodynodelists = BodyNodeLists}.

concat_names([])->
    "";
concat_names([Head|[]]) ->
    atom_to_list(Head);
concat_names([Head|Tail]) ->
    atom_to_list(Head) ++ ", " ++ concat_names(Tail).

%%% ============================================================================
%%% Checks

check_var_collisions(#refst{funclauses = FunClauses,
                            appvariables = AppVarNodes,
                            varpairlists = VarPairLists}) ->
    AppVarNames =
        ?SEMINF:var_names(AppVarNodes),
    Zipped = lists:zip(FunClauses, VarPairLists),%%BoundVarNames),
    Collisions =
        lists:flatten(
          lists:map(
            fun({Clause, VarPairList})->
                    VarNodes =
                        ?SEMINF:vars(clause,[Clause]),
                    VarNames =
                        ?SEMINF:var_names(VarNodes),
                    Inter = ?MISC:intersect(VarNames, AppVarNames) --
                        [First || {First,_} <- VarPairList],
                    [ list_to_atom(In) || In <- Inter]
            end, Zipped)),
    case Collisions of
        [] ->
            ok;
        _ -> 
            throw("There are variable name collisions: " ++ 
                  concat_names(Collisions))
    end,
    ok.

check_applications(#refst{file = File, funfile = FunFile,
                          localapps = LocalApps})->
    case File of
        FunFile ->
            ok;
        _ ->
            Apps =
                lists:flatten(LocalApps),
            BifList =
                [{Type, Name, Arity}
                 || {Type, Name, Arity} <- Apps, erl_internal:bif(Name, Arity)],
            case (Apps -- BifList) of
                [] ->
                    ok;
                _ -> throw("There are local function calls in the body of the"
                           ++ " function: " ++ concat_names(Apps -- BifList))
            end
    end.

check_macros_records(#refst{file = File, funfile = FunFile, 
                            funnode = FunNode})->
    case File of
        FunFile ->
            ok;
        _ ->
            {Macros, _} = ?LEX:used_macros([FunNode]),
            {Records, _} = ?LEX:used_records([FunNode]),
            case {Macros,Records} of
                {[],[]} ->
                    ok;
                {_,[]} ->
                    throw("There are macro applications in the body of the"
                           ++ " function. This case is not implemented yet.");
                {[], _} ->
                    throw("There are record expressions in the body of the"
                           ++ " function. This case is not implemented yet.");
                _ ->
                    throw("There are record expressionsand macro applications"
                          ++ " in the body of the"
                           ++ " function. This case is not implemented yet.")
            end
    end.
