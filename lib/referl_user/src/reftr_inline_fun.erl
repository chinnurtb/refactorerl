%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc This module implements the inline function refactoring. The inline
%%% function refactoring step substitutes the selected application with the
%%% corresponding function body and executes the required compensations.
%%%
%%%
%%% == Parameters ==
%%% <ul>
%%% <li> An expression (see {@link reflib_args:expression/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>Applying the inline function must not cause variable name
%%%   conflicts. A variable name conflict arises when the same variable name
%%%   is used in the body of the function clause and in the scope of the
%%%   selected function application, except the variables which are bound in
%%%   the formal parameters where the structure of the formal and
%%%   the actual parameters are equivalent</li>
%%%   <li>If the function is defined in other module:
%%%   <ul>
%%%     <li>the function do not contain local (not exported) function
%%%     applications in its body.</li>
%%%     <li>macro name conflicts must not occur in the current module, that is,
%%%     macro names used in the functions must refer to the same macro
%%%     definition in the current and in the definition module. This applies to
%%%     macros used in these macros too.</li>
%%%     <li>record name conflicts must not occur in the current module, that is,
%%%     record names used in the functions must refer to the same record
%%%     definition in the current and in the definition module.</li>
%%%   </ul></li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>In the refactoring step the functions application to be inlined have
%%%   to be marked.</li>
%%%   <li>The defining module of the corresponding function must be loaded.</li>
%%%   <li>Copy the function clause(s) and create (if it is needed) a
%%%   corresponding structure from the expressions of the body(ies), the guard
%%%   expressions (if there is any) and from the patterns of the function
%%%   clause(s).</li>
%%%   <li>Where the actual and formal parameters are structurally equivalent,
%%%   create variable name pairs and rename the corresponding variables in the
%%%   copied body.</li>
%%%   <li>For variable names which collide generate a new name which eliminates
%%%   the collision.</li>
%%%   <li>Where the formal and structural parameters are not equivalent, create
%%%   a match expression from these parameters. The left hand side is a tuple
%%%   from the formal parameters, and the right hand side is a tuple from the
%%%   actual parameters.</li>
%%%   <li>If the function consists of
%%%   <ul>
%%%     <li>one clause and does not have guard expression and the body of the
%%%     function contains only one expression and match expressions should not
%%%     be created from the parameters, replace the application with this single
%%%     expression.</li>
%%%     <li>one clause and does not have guard expression and the body of the
%%%     function contains more than one  expression and the parent expression of
%%%     the selected application is a clause, replace the application with the
%%%     sequence of the expressions from the body of the function clause
%%%     extended with the created match expression.</li>
%%%     <li>one clause and does not have guard expression and the body of the
%%%     function contains more than one expression and the selected application
%%%     is a subexpression:
%%%     <ol>
%%%       <li>create a begin-end block from the sequence of the expressions
%%%       from the body of the function clause extended with the created match
%%%       expression</li>
%%%       <li>replace the application with this begin-end block.</li>
%%%     </ol></li>
%%%     <li>more than one clause, or it has guard expressions (or both) or
%%%     variables appear multiple times with the same name in a pattern list:
%%%     <ol>
%%%       <li>create a case expression from the function clause body(ies)
%%%       expression(s), guard and pattern expressions</li>
%%%       <li>replace the application with this case expression.</li>
%%%     </ol></li>
%%%   </ul></li>
%%%   <li>If the definition of the function is in another module
%%%   <ul>
%%%     <li>qualify the applications in the copied body which call exported
%%%     functions from the defining module.</li>
%%%     <li>qualify the applications in the copied body which call imported
%%%     functions.</li>
%%%     <li>copy or import record and macro definitions to the current module
%%%     which are used in the copied body(ies).</li>
%%%   </ul></li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(reftr_inline_fun).
-vsn("$Rev: 5043 $ ").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%%% @private
error_text(local_apps, [FunModuleName, LocalApps]) ->
    LocalAppsText =
        [ ?MISC:fun_text([Name, Arity]) || {_, Name, Arity} <- LocalApps],
    ["Local functions of module ", atom_to_list(FunModuleName),
     " are called in the function body: ",
     ?MISC:join(LocalAppsText)].

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args)->
    File = ?Args:file(Args),
    [AppModule] = ?Query:exec(File, ?File:module()),
    %% The selected expression
    Expr = ?Args:expression(Args),
    %% Determines the application node, if the selected position is not an
    %% application then throws an error
    AppNode = get_app_node(Expr),
    AppArgs = ?Query:exec(AppNode,
                    ?Query:seq(?Expr:child(2), ?Expr:children())),
    FunObjNode = ?Args:function(Args),
    [FunForm] = ?Query:exec(FunObjNode, ?Fun:definition()),
    FunClauses = ?Query:exec(FunForm, ?Form:clauses()),
    ?Check(FunClauses /= [], ?RefError(fun_not_found,[?Fun:name(FunObjNode),
                                                      ?Fun:arity(FunObjNode)])),
    [FunModule] = ?Query:exec(FunObjNode, ?Fun:module()),
    [FunFile] = ?Query:exec(FunModule, ?Mod:file()),
    %% Get application nodes from the function and check if there is any local
    %% application used, which is needed to be included or qualified
    AppNodeLists =
        get_app_nodes_from_clauses(FunClauses),
    {Imported, Exported, LocalsAndBIFs} =
        get_application_data(AppNodeLists, AppModule, FunModule),
    LocalApps = check_applications(LocalsAndBIFs, AppModule, FunModule),
    FunModuleName = ?Mod:name(FunModule),
    ?Check( LocalApps == [],
           ?LocalError(local_apps, [FunModuleName, LocalApps])),
    %% Checks the record/macro information if the files are different
    RecMacInfo =
        if FunFile =:= File ->
                [];
           true ->
                reftr_move_fun:prepare_recmac([FunForm], FunFile, File)
        end,
    PatternLists =
        [?Query:exec(Clause, ?Clause:patterns()) || Clause <- FunClauses],
    GuardLists =
        [?Query:exec(Clause, ?Clause:guard()) || Clause <- FunClauses],
    ClauseBodyNodeLists =
        [?Query:exec(Clause, ?Clause:body()) || Clause <- FunClauses],
    ScopeVariables =
        ?Query:exec(Expr, ?Query:seq(?Expr:clause(), ?Clause:variables())),
    {VarPLists, MatchPairLists} = get_attr_pairs(AppArgs, PatternLists),
    UnusedVarLists =
        get_unused_vars_from_pattern(PatternLists),
    BoundVarLists =
        [begin
             AllClause = ?Query:exec(Clause, [{functx,back}]),
             ?Query:exec(AllClause, ?Query:seq(?Clause:variables(),
                                               ?Var:occurrences()))
         end
         || Clause <- FunClauses],
    VarPairLists =
        eliminate_var_collisions(FunClauses, ScopeVariables, VarPLists),
    [{_, AppParent}] = ?Syn:parent(AppNode),
    VarsAndNames = varnodes_and_names(FunClauses),
    AppComments = ?Syn:get_comments(AppNode),
    %% The transformation and compensation steps
    [fun()->
             transform_first_phase(FunClauses, GuardLists, AppParent, AppNode,
                                   ClauseBodyNodeLists, PatternLists, AppArgs,
                                   VarPairLists, MatchPairLists, BoundVarLists,
                                   VarsAndNames, UnusedVarLists)
     end,
     fun({CopiedNodes, VarLists, NodeForComment})->
             ?Syn:put_comments(NodeForComment, AppComments),
             {CopiedNodes, VarLists}
     end,
     fun({CopiedNodes, VarLists})->
             Nodes =  ?Query:exec(CopiedNodes, ?Expr:deep_sub()),
             AppNodes = [ Node || Node <- Nodes,
                                  ?Expr:type(Node) == application,
                                 ?Query:exec(Node, ?Expr:modq()) == []],
             PairLists = pairs_to_rename(VarLists, VarPairLists),
             rename_variables(PairLists),
             AppNodes
     end,
     fun(AppNodes) ->
             case AppModule of
                 FunModule -> ok;
                 _ ->
                     maintain_applications(AppNodes, Imported, Exported)
             end
     end
    ] ++ rec_mac_tr_steps(FunFile, File, RecMacInfo).


%%% ============================================================================
%%% Implementation

rec_mac_tr_steps(FunFile, File, RecMacInfo) ->
    Corr = fun reftr_move_fun:correct_recmac/4,
    [fun(_) -> Corr(record, {Loc_Incl, Node, Form, Removable}, FunFile, File)
     end || {rec_info, Loc_Incl, Node, Form, Removable} <- RecMacInfo] ++
        [fun(_) -> Corr(macro, {Loc_Incl, Form, Form, Removable}, FunFile, File)
         end || {mac_info, Loc_Incl,Form, Removable} <- RecMacInfo].

%% Removes the application and inserts a function clause(s) instead.
transform_first_phase(FunClauses, GuardLists, AppParent, AppNode,
                      ClauseBodyNodeLists, PatternLists, Parameters,
                      VarPairLists, MatchPairLists, BoundVarLists,
                      VarsAndNames, UnusedVarLists) ->
    ClauseNr = length(FunClauses),
    GuardedOrAmbiguous =
        (length(lists:flatten(GuardLists)) /= 0)
        or (is_var_rename_ambiguous(VarPairLists)),
    SubsNodeNr = length(hd(ClauseBodyNodeLists) ++
                        lists:flatten(hd(MatchPairLists))),
    ReplaceType = get_replace_type(AppParent),
    ?Transform:touch(AppParent),
    VarLists =
        case {ClauseNr,GuardedOrAmbiguous,SubsNodeNr,ReplaceType} of
            {1, false, 1, simple} -> %% simple replace
                {[CopiedNodes], VarLsts} =
                    replicate_nodes_and_return_vars(ClauseBodyNodeLists,
                                                    BoundVarLists, VarsAndNames,
                                                    UnusedVarLists),
                ?Syn:replace(AppParent,{node, AppNode},
                             [hd(CopiedNodes)]),
                NodeForComment = hd(CopiedNodes),
                VarLsts;
            {1, false, 1, block} -> %% simple replace with parenthesis
                {[CopiedNodes], VarLsts} =
                    replicate_nodes_and_return_vars(ClauseBodyNodeLists,
                                                    BoundVarLists, VarsAndNames,
                                                    UnusedVarLists),
                Paren =
                    ?Syn:create(#expr{type='parenthesis'},
                                [{esub,hd(CopiedNodes)}]),
                ?Syn:replace(AppParent,{node, AppNode}, [Paren]),
                NodeForComment = Paren,
                VarLsts;
            {1, false, _, simple} -> %% simple replace with match
                {Match, VarLst} =
                    create_match_expr(hd(MatchPairLists),
                                      hd(BoundVarLists), VarsAndNames,
                                      hd(UnusedVarLists)),
                {[CopiedNodes], VarLsts} =
                    replicate_nodes_and_return_vars(ClauseBodyNodeLists,
                                                    BoundVarLists, VarsAndNames,
                                                    UnusedVarLists),
                ?Syn:replace(AppParent,{node, AppNode}, Match ++ CopiedNodes),
                NodeForComment =
                    case Match of
                        [] -> hd(CopiedNodes);
                        _  -> hd(Match)
                    end,
                [lists:flatten(VarLsts ++ VarLst)];
            {1, false, _, _} -> %% block replace
                {Match, VarLst} =
                    create_match_expr(hd(MatchPairLists),
                                      hd(BoundVarLists), VarsAndNames,
                                      hd(UnusedVarLists)),
                {[CopiedNodes], VarLsts} =
                    replicate_nodes_and_return_vars(ClauseBodyNodeLists,
                                                    BoundVarLists, VarsAndNames,
                                                    UnusedVarLists),
                Block =
                    ?Syn:create(#clause{type=block},
                                [{body, Match ++ CopiedNodes}]),
                BlockExpr =
                    ?Syn:create(#expr{type=block_expr},
                                [{exprcl,Block}]),
                ?Syn:replace(AppParent,{node, AppNode},
                             [BlockExpr]),
                NodeForComment = BlockExpr,
                [lists:flatten(VarLsts ++ VarLst)];
            _ -> %% case replace
                {ClauseData_, VarLsts} =
                    create_data_for_case_expr(PatternLists, GuardLists,
                                              ClauseBodyNodeLists,BoundVarLists,
                                              VarsAndNames, UnusedVarLists),
                CopiedNodes =
                    lists:flatten(
                      [ [F] ++ [S] ++ [T] || {F, S, T} <- ClauseData_]),
                NodeForComment =
                    replace_app_with_case_expr(AppParent, AppNode,
                                               Parameters, ClauseData_),
                VarLsts
    end,
    {CopiedNodes, VarLists, NodeForComment}.

get_replace_type(AppParent)->
    PClass = ?Syn:class(AppParent),
    [{_, PP}] = ?Syn:parent(AppParent),
    PPClass = ?Syn:class(PP),
    case {PClass, PPClass} of
        {clause, expr} ->
            block;
        {clause, _} ->
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
                          BoundVarLists, VarsAndNames, UnusedVarLists)->
    {CasePatterns, VarLists1} =
        replicate_nodes_and_return_vars(FunPatternLists, BoundVarLists,
                                        VarsAndNames, UnusedVarLists),
    {CaseClauseGuards, VarLists2} =
        replicate_guards_and_return_vars(Guards, BoundVarLists, VarsAndNames),
    {CaseClauseBodies, VarLists3} =
        replicate_nodes_and_return_vars(FunBodyNodeLists, BoundVarLists,
                                        VarsAndNames, UnusedVarLists),
    VarZip = lists:zip3(VarLists1, VarLists2, VarLists3),
    VarLists =
        [ F ++ S ++ T || {F, S, T} <- VarZip],
    {lists:zip3(CasePatterns, CaseClauseGuards,CaseClauseBodies), VarLists}.

replicate_guards_and_return_vars(Guards, BoundVarLists, VarsAndNames) ->
    Zipped = lists:zip(Guards, BoundVarLists),
    GuardsAndVars =
        lists:map(
          fun({Guard, BoundVars})->
                  case Guard of
                      [] -> {[],[]};
                      _ ->
                          copy_node_and_return_variables(hd(Guard), BoundVars,
                                                         VarsAndNames, [])
                  end
          end,Zipped),
    lists:unzip(GuardsAndVars).

replicate_nodes_and_return_vars(NodeLists, BoundVarLists, VarsAndNames,
                                UnusedVarLists) ->
    Zipped =
        lists:zip3(NodeLists, BoundVarLists, UnusedVarLists),
    NodeAndVarPairLists =
        [[ copy_node_and_return_variables(Node, BoundVars, VarsAndNames,
                                          UnusedVars)
           || Node <- BodyNodes]
         || {BodyNodes, BoundVars, UnusedVars} <- Zipped],
    CopiedNodeLists =
        [[ Node || {Node, _} <- PairList]
         || PairList <- NodeAndVarPairLists],
    VarLists =
        [ lists:flatten([ Vars || {_, Vars} <- PairList])
          || PairList <- NodeAndVarPairLists],
    {CopiedNodeLists, VarLists}.

create_match_expr(MatchPairList, BoundVars, VarsAndNames, UnusedVars)->
    {LeftSideNodes, VarNodes} =
        lists:unzip(
          [ copy_node_and_return_variables(Left, BoundVars, VarsAndNames,
                                           UnusedVars)
            || {Left, _} <- MatchPairList]),
    RightSideNodes =
        [ begin
              RightSide = ?Syn:copy(Right),
              get_pair(Right,RightSide)
          end
          || {_, Right} <- MatchPairList],
    case {LeftSideNodes, RightSideNodes} of
        {[],[]} ->
            {[],[]};
        {[LeftNode],[RightNode]} ->
            {[?Syn:create(#expr{type=match_expr},
                         [{esub,LeftNode},{esub,RightNode}])],
             lists:flatten(VarNodes)};
        _ ->
            LeftSide =
                ?Syn:create(#expr{type=tuple},
                               [{esub , LeftSideNodes}]),
            RightSide =
                ?Syn:create(#expr{type=tuple},
                               [{esub , RightSideNodes}]),
            {[?Syn:create(#expr{type=match_expr},
                          [{esub,LeftSide},{esub,RightSide}])],
             lists:flatten(VarNodes)}
    end.

copy_node_and_return_variables(Node, BoundVars, VarsAndNames, UnusedVarList) ->
    Nodes =
        ?Syn:copy(Node),
    CopiedNode = get_pair(Node, Nodes),
    Variables =
        [ begin
              case lists:member(OldNode, UnusedVarList) of
                  true ->
                      {NewNode, "_"};
                  _ ->
                      Name = get_pair(OldNode, VarsAndNames),
                      {NewNode, Name}
              end
          end
          || {OldNode, NewNode} <- Nodes,
             lists:member(OldNode, BoundVars)],
    {CopiedNode, Variables}.


varnodes_and_names(Clauses) ->
    lists:flatten(
      [ begin
            AllClause = ?Query:exec(Clause, [{functx, back}]),
            VarObjs = ?Query:exec(AllClause, ?Clause:variables()),
            [ begin
                  Name = ?Var:name(VarObj),
                  Occ = ?Query:exec(VarObj, ?Var:occurrences()),
                  [ {O, Name} || O <- Occ]
              end
              || VarObj <- VarObjs]
        end
        || Clause <- Clauses]).

replace_app_with_case_expr(AppParent, AppNode, Parameters, ClauseData_)->
    ArgNode =
        begin
            RepParameters =
                [ begin
                      {Parameter, RepNode} =
                          lists:keyfind(Parameter, 1, ?Syn:copy(Parameter)),
                      RepNode
                  end || Parameter <- Parameters],
            case RepParameters of
                [P] -> P;
                _ ->
                    ?Syn:create(#expr{type=tuple},
                                [{esub , RepParameters}])
            end
        end,
    HeadClause =
        ?Syn:create(#clause{type=expr},[{body,ArgNode}]),
    Branches =
        lists:map(
          fun({FunPattern, Guard, FunBodyNodes})->
                  Pattern =
                      case FunPattern of
                          [FP] ->
                              FP;
                          _ ->
                              ?Syn:create(#expr{type=tuple},
                                          [{esub,FunPattern}])
                      end,
                  case Guard of
                      [] ->
                          ?Syn:create(#clause{type=pattern},
                                      [{pattern, Pattern},
                                       {body, FunBodyNodes}]);
                      _ ->
                          ?Syn:create(#clause{type=pattern},
                                      [{pattern, Pattern},
                                       {guard, Guard},
                                       {body, FunBodyNodes}])
                  end
          end,ClauseData_),
    CaseExpr =
        ?Syn:create(#expr{type=case_expr},
                       [{headcl, HeadClause},
                        {exprcl, Branches}]),
    ?Syn:replace(AppParent,{node,AppNode},[CaseExpr]),
    CaseExpr.


pairs_to_rename(VarLists, VarPairLists) ->
    Zipped = lists:zip(VarLists, VarPairLists),
    [ [ begin
            case OldName of
                "_" ->
                    {Var, "_"};
                _ ->
                    NewName = get_pair(OldName, VarPairList),
                    {Var, NewName}
            end
        end || {Var, OldName} <- VarList ]
      || {VarList, VarPairList} <- Zipped].

rename_variables(PairLists)->
    [ [ update_var_name(Pair) || Pair <- PairList]
      || PairList <- PairLists].

update_var_name({VarNode, NewName})->
    case NewName of
        no_pair ->
            ok;
        _ ->
            ?Syn:replace(VarNode, {elex, 1}, [NewName])
    end.

maintain_applications(AppNodes, Imported, Exported)->
    FunData = Imported ++ Exported,
    [begin
         [FunObj] = ?Query:exec(App, ?Expr:function()),
         Name = ?Fun:name(FunObj),
         Arity = ?Fun:arity(FunObj),
         case get_mod({Name,Arity}, FunData) of
             [] ->
                 ok;
             Mod ->
                 ?Expr:add_modq(App, Mod)
         end
     end
     || App <- AppNodes].

get_mod(_, []) ->
    [];
get_mod({Name,Arity},[{Mod, Name, Arity}|_Tail])->
    Mod;
get_mod({Name,Arity},[_| Tail]) ->
    get_mod({Name,Arity},Tail).

get_pair(N, NList)->
    case lists:keyfind(N, 1, NList) of
        {_, NPair} ->
            NPair;
        false ->
            no_pair
    end.

get_app_node(Expr)->
    Parent = ?Query:exec1(Expr, ?Expr:parent(),
                          ?RefError(bad_kind, application)),
    case ?Expr:type(Parent) of
        application ->
            Parent;
        infix_expr ->
            Parent2 =
                ?Query:exec1(Parent, ?Expr:parent(),
                             ?RefError(bad_kind, application)),
            ?Check(?Expr:type(Parent2) == application,
                   ?RefError(bad_kind, application)),
            Parent2
    end.

get_unused_vars_from_pattern(PatternLists) ->
    VarObjLists =
        [ ?Query:exec(PatternNodes, ?Expr:variables())
          || PatternNodes <- PatternLists],
    [lists:flatten([ ?Query:exec(Obj,?Var:bindings()) ||
                       Obj <- VarObjList,
                       length(?Query:exec(Obj, ?Var:references())) == 0])
     || VarObjList <- VarObjLists].

get_attr_pairs(Parameters, FunPatternLists) ->
    AttrPairLists =
        lists:map(fun(Elem)->
                          get_attr_pairs({Elem,Parameters})
                  end,FunPatternLists),
    VarPairs =
        [[ Pair || {Kind, Pair} <- AttrPairList, Kind == variable]
         || AttrPairList <- AttrPairLists],
    MatchPairs =
        [[ Pair || {Kind, Pair} <- AttrPairList, Kind == match]
         || AttrPairList <- AttrPairLists],
    {VarPairs, MatchPairs}.


%%supporting function for previous

get_attr_pairs({Patterns, Parameters})->
    Zipped = lists:zip(Patterns, Parameters),
    lists:flatten(
      lists:map(
        fun({FunArg, AppArg})->
                FunArgType = ?Expr:type(FunArg),
                AppArgType = ?Expr:type(AppArg),
                case FunArgType of
                    AppArgType when AppArgType == variable ->
                        ArgVariable = ?Query:exec(FunArg,
                                                  ?Expr:variables()),
                        case ArgVariable of
                            [] -> [];
                            _ ->
                                [{variable,
                                  {?Var:name(hd(ArgVariable)),
                                   ?Var:name(
                                      hd(?Query:exec(AppArg,
                                                     ?Expr:variables())))}}
                                ]
                        end;

                    AppArgType when (AppArgType == cons ) ->
                        FunArgSub = ?Query:exec(FunArg, ?Expr:children()),
                        AppArgSub = ?Query:exec(AppArg, ?Expr:children()),
                        case length(FunArgSub)==length(AppArgSub) of
                            true ->
                                SubNodes =
                                    get_attr_pairs({FunArgSub,AppArgSub}),
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
                        FunArgSub = ?Query:exec(FunArg, ?Expr:children()),
                        AppArgSub = ?Query:exec(AppArg, ?Expr:children()),
                        SubNodes =
                            get_attr_pairs({FunArgSub,AppArgSub}),
                        [{match,{FunArg,AppArg}}] ++
                            [ {Kind,Rest} || {Kind, Rest} <- SubNodes,
                                             Kind == variable];
                    AppArgType ->
                        FunArgSub = ?Query:exec(FunArg, ?Expr:children()),
                        AppArgSub = ?Query:exec(AppArg, ?Expr:children()),
                        case length(FunArgSub) == length(AppArgSub) of
                            true ->
                                get_attr_pairs({FunArgSub,AppArgSub});
                            _Other ->
                                [{match,{FunArg,AppArg}}]
                        end;
                    _Other ->
                        [{match,{FunArg,AppArg}}]
                end
        end,Zipped)).

get_app_nodes_from_clauses(FunClauses) ->
    ExprLists =
        [ ?Query:exec(Clause, ?Query:seq(?Clause:exprs(), ?Expr:deep_sub()))
          || Clause <- FunClauses],
    [[ Expr
       || Expr <- ExprList, ?Expr:type(Expr) == application ]
     || ExprList <- ExprLists].

get_data_about_app(Mod, Node)->
    PossibleQual = ?Query:exec(Node, ?Expr:modq()),
    case PossibleQual of
        [] ->
            [FunObj] = ?Query:exec(Node, ?Expr:function()),
            IsExported = ?Fun:is_exported(FunObj),
            Name = ?Fun:name(FunObj),
            Arity = ?Fun:arity(FunObj),
            IsImported = (?Query:exec(Mod, ?Mod:imported(Name, Arity)) /= []),
            FunObjMod = ?Query:exec1(FunObj, ?Fun:module(), ambiguous),
            case {IsExported, IsImported} of
                {false, false} ->
                    {undefined,{undefined, Name, Arity}};
                {true, false} ->
                    ModName = ?Mod:name(FunObjMod),
                    {exported, {ModName, Name, Arity}};
                {_, true} ->
                    ModName = ?Mod:name(FunObjMod),
                    {imported, {ModName, Name, Arity}}
            end;
        _ ->
            []
    end.

get_application_data(AppNodeLists, AppMod, FunMod) when AppMod /= FunMod ->
    AppLists =
        [lists:flatten([ get_data_about_app(FunMod, Node)
                         || Node <- AppNodeList])
         || AppNodeList <- AppNodeLists],
    LocalsAndBIF =
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
    {Imported, Exported, LocalsAndBIF};
get_application_data(_,_,_) ->
    {[], [], []}.

eliminate_var_collisions(FunClauses, ScopeVarObjs, VarPairLists) ->
    ScopeVarNames =
        [?Var:name(Obj) || Obj <- ScopeVarObjs],
    Zipped = lists:zip(FunClauses, VarPairLists),
    lists:map(
      fun({Clause, VarPairList})->
              AllClause = ?Query:exec(Clause, [{functx, back}]),
              VarObjNodes =
                    ?Query:exec(AllClause, ?Clause:variables()),
              VarNames =
                  [ ?Var:name(VarObj) || VarObj <- VarObjNodes],
              PatternNames = [First || {First,_} <- VarPairList],
              CollNames =
                  ?MISC:intersect(VarNames, ScopeVarNames) -- PatternNames,
              VarPairList ++
                  eliminate_vars(CollNames, VarNames ++ ScopeVarNames ++
                                 PatternNames, [])
      end, Zipped).

eliminate_vars([], _, AddPairs)->
    AddPairs;
eliminate_vars([Name|Tail], ExVarNames, AddPairs) ->
    NewName = gen_new_name(Name, ExVarNames, 0),
    eliminate_vars(Tail, ExVarNames ++ [NewName],
                   AddPairs ++ [{Name, NewName}]).

gen_new_name(Name, VarNames, N) ->
    NewName = Name ++ "_" ++ integer_to_list(N),
    case ?MISC:intersect([NewName], VarNames) of
        [] ->
            NewName;
        _ ->
            gen_new_name(Name, VarNames, N+1)
    end.

%%% ============================================================================
%%% Checks

check_applications(LocalApps, AppMod, FunMod) ->
    case AppMod of
        FunMod -> [];
        _ ->
            Apps =
                lists:flatten(LocalApps),
            BifList =
                [{Type, Name, Arity}
                 || {Type, Name, Arity} <- Apps, erl_internal:bif(Name, Arity)],
            Apps -- BifList
    end.
