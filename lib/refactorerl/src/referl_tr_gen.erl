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

%%% @doc
%%% This module implements the generalize function definition refactoring.
%%% The refactoring is generalize a function definition by selecting an
%%% expression or a continious sequence of expressions and making this
%%% the value of a new argument added to the definition of the function,
%%% and the actual parameter at all call site becomes the selected part
%%% with the corresponing compensation.
%%%
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(referl_tr_gen).
-vsn("$Rev: 1941 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/4]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

-record(refst, {filename, varname, frompos, topos,
                file, module, module_name, fromtoken, totoken,
                fromexpr, toexpr, topexpr, ref_case,
                expressions, fun_clause, function,
                function_obj, function_name, arity,
                used_vars, all_vars, patterns, ref_type,
                side_effect, guard, macro, record, fun_calls,
                rec_fun_calls, rec_fun_cl, exported, bound_vars,
                used_var_names}).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface

%% @spec do(string(), string(), integer(), integer()) -> ok
%%
%% @doc Generalize a function definition by selecting the expression(s) between
%% the positions `Begin' and `End' in the source `File'.
%% The new pattern name is `VarName'.
do(File, VarName, Begin, End) ->
    ?TRANSFORM:do(?MODULE, {File, VarName, Begin, End}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FileName, VarName, FromPos, ToPos}) ->
    #refst{ filename = FileName,
            varname  = VarName,
            frompos  = FromPos,
            topos    = ToPos }.

%% @private
steps() ->
    [
        fun file_node/1,
        fun module_node/1,
        fun from_token/1,
        fun to_token/1,
        fun from_expr/1,
        fun to_expr/1,
        fun top_expr/1,
        fun check_is_pattern/1,
        fun check_is_var_name/1,
        fun refac_case/1,
        fun find_funclause/1,
        fun find_function/1,
        fun selected_exprs/1,
        fun check_whole_expr_is_selected/1,
        fun check_listgen/1,
        fun function_data/1,
        fun check_recursive_calls/1,
        fun check_fun_name/1,
        fun get_variable_data/1,
        fun check_new_var_name/1,
        fun refac_type/1,
        fun check_local_vars/1,
        fun side_effect/1,
        fun guard_expression/1,
        fun get_fun_calls/1,
        fun check_bound_var_name/1,
        fun is_exported/1
    ].

%% @private
transform(#refst{file = File, ref_case = RCase, varname = VName,
                 expressions = Exprs, fun_calls = FunCalls, function = Fun,
                 ref_type = RType, side_effect = SideEffect,
                 used_var_names = UVars, exported = Export,
                 rec_fun_calls = RecCalls, arity = Arity,
                 function_obj = FunObj, fun_clause = FunCl,
                 rec_fun_cl = RecFunCl})->
    Data = ?ESG:data(FunObj),
    FunCallData  = [get_app_data(App, Exprs)|| App <- FunCalls],
    RecCallData  = [get_app_data(App, Exprs)|| App <- RecCalls],
    [{_,Parent}] = ?ESG:parent(hd(Exprs)),
%%TODO: Guard esetet kulon lekezelni
    Type =
        case {RType, SideEffect, length(Exprs)>1} of
            {_ , _, true}         -> funexpr;
            {_, true,_}           -> funexpr;
            {without_vars,_,_}    -> variable;
            {with_params, _,_}    -> funexpr;
            {with_local_vars,_,_} -> funexpr
        end,
    case {RCase, Type} of
        {subexpr, variable} ->
            replace_subexpression(Parent,VName,hd(Exprs), var, []),
            insert_app_arg(FunCallData, var, UVars),
            insert_rec_app_arg(RecCallData, VName),
            insert_old_fun(File, Fun, Exprs, var, UVars, Export);
        {subexpr, funexpr}  ->
            replace_subexpression(Parent,VName,hd(Exprs),funexp,UVars),
            insert_app_arg(FunCallData, funexp, UVars),
            insert_rec_app_arg(RecCallData, VName),
            insert_old_fun(File, Fun, Exprs, funexp, UVars, Export);
        {_     , variable}  ->
            replace_expressions(Parent, VName, Exprs, var, []),
            insert_app_arg(FunCallData, var, UVars),
            insert_rec_app_arg(RecCallData, VName),
            insert_old_fun(File, Fun, Exprs, var, UVars, Export);
        {_, _}              ->
            replace_expressions(Parent, VName, Exprs, funexp, UVars),
            insert_app_arg(FunCallData, funexp, UVars),
            insert_rec_app_arg(RecCallData, VName),
            insert_old_fun(File, Fun, Exprs, funexp, UVars, Export)
    end,
    insert_fun_pattern(Fun, VName, FunCl, RecFunCl),
    ?ESG:update(FunObj, Data#func{arity = Arity + 1}),
    ?ESG:close(),
    {[File], ok}.

get_app_data(AppNode, Exprs)->
    Args = tl(?ESG:path(AppNode, [sub])),
    {Args, Exprs, AppNode}.

%%% ============================================================================
%%% Implementation

file_node(St = #refst{filename = FileName}) ->
    case ?SYNTAX:file(FileName) of
        {file, File} -> 
            St#refst{file = File};
        not_found -> 
            throw({"The file does not exist in the database!", FileName})
    end.


module_node(St = #refst{file = File}) ->
    case ?ESG:path(File, [moddef]) of
        [ModuleNode] -> 
            St#refst{module = ModuleNode, 
                     module_name =(?ESG:data(ModuleNode))#module.name};
        _ -> 
            throw("The module node does not exist in the graph!")
    end.


from_token(St = #refst{file = File, frompos = FromPos}) ->
    case  ?LEX:token_by_pos(File, FromPos) of
        {ok, FromToken} -> 
            St#refst{fromtoken = FromToken};
        _ -> 
            throw("There is not any lexical element in the selected position!")
    end.


to_token(St = #refst{file = File, topos = ToPos}) ->
    case  ?LEX:token_by_pos(File, ToPos) of
        {ok, ToToken} -> 
            St#refst{totoken = ToToken};
        _ -> 
            throw("There is not any lexical element in the selected position!")
    end.


from_expr(St = #refst{fromtoken = FromToken}) ->
    [{_,FromExpr}] = ?ESG:parent(FromToken),
    case ?ESG:data(FromExpr) of
        #expr{} -> St#refst{fromexpr = FromExpr};
        _       ->  throw("The selected part is not a legal sequence!")
    end.


to_expr(St = #refst{totoken = ToToken}) ->
    [{_,ToExpr}] = ?ESG:parent(ToToken),
    case ?ESG:data(ToExpr) of
        #expr{} -> St#refst{toexpr = ToExpr};
        _ -> throw("The selected part is not a legal sequence!!")
    end.

top_expr(St = #refst{fromexpr = FromExpr, toexpr = ToExpr}) ->
    case ?SYNTAX:top_node(FromExpr, ToExpr) of
        {expr, TopExpr, _, _}   -> St#refst{topexpr = TopExpr};
        {clause, TopExpr, _, _} -> St#refst{topexpr = TopExpr};
        _  -> throw("The selected part is not a legal sequence!")
    end.


refac_case(St = #refst{topexpr = TopExpr}) ->
    case {?ESG:data(TopExpr), ?ESG:data(element(2,hd(?ESG:parent(TopExpr))))} of
        {#expr{}, #expr{}}   -> St#refst{ref_case = subexpr};
        {#expr{}, #clause{}} -> St#refst{ref_case = expr};
        {#clause{}, _}       -> St#refst{ref_case = body};
        {_, _} -> throw("The selected part is not a legal sequence!")
    end.


selected_exprs(St = #refst{ref_case = body, fromexpr = FromExpr,
                           toexpr = ToExpr, fun_clause = ClauseNode}) ->
    BodyExprs = ?ESG:path(ClauseNode,[body]),
    case BodyExprs of
        [] -> throw("The selected part is not a legal sequence!");
        _  -> ok
    end,
    [From]    = ?ESG:path(FromExpr, [sup]),
    [To]      = ?ESG:path(ToExpr, [sup]),
    Exprs     = ?MISC:separate_interval(BodyExprs, From, To),
    case Exprs of
        [] -> throw("The selected part is not a legal sequence!");
        _  -> St#refst{expressions = Exprs}
    end;

selected_exprs(St = #refst{topexpr = TopExpr}) ->
    St#refst{expressions = [TopExpr]}.


find_funclause(St = #refst{fromexpr = FromExpr})->
    [FunCl] = ?ESG:path(FromExpr, [sup, {visib, back}, scope, functx]),
    St#refst{fun_clause = FunCl}.


find_function(St = #refst{fun_clause = Clause})->
    [Function] = ?ESG:path(Clause, [{funcl, back}]),
    St#refst{function = Function}.


function_data(St = #refst{function = Function})->
    [FunObject] = ?ESG:path(Function, [fundef]),
    #func{name = Name, arity = Arity} = ?ESG:data(FunObject),
    St#refst{function_obj = FunObject, function_name = Name, arity = Arity}.


get_variable_data(St = #refst{expressions = Exprs, fun_clause = Clause,
                              function = Function})->
    AllClauses  = ?ESG:path(Function, [funcl]),
    UsedVars    = ?SEMINF:vars(sub, Exprs),
    AllVars     = ?SEMINF:vars(clause, AllClauses),
    Patterns    = ?ESG:path(Clause, [pattern]),
    PatternVars = ?SEMINF:vars(sub, Patterns),
    St#refst{used_vars = UsedVars, all_vars = AllVars, patterns = PatternVars}.


refac_type(St = #refst{used_vars = UsedVars, patterns = Patterns})->
    UsedVarObjs      = lists:usort(?SEMINF:varrefs(UsedVars) ++ 
                                   ?SEMINF:varbinds(UsedVars)),
    PatternObj       = lists:usort(?SEMINF:varbinds(Patterns)),
    UsedBindings     = ?SEMINF:varbinds_back(UsedVarObjs),
    InsideBoundVars  = ?MISC:intersect(UsedBindings, UsedVars),
    case {UsedVars, UsedVarObjs -- PatternObj, InsideBoundVars} of
        {[],  _,  _} -> St#refst{ref_type = without_vars};
        { _, [],  _} -> St#refst{ref_type = with_params};
        { _,  _, []} -> St#refst{ref_type = with_local_vars};
        { _,  _,  _} -> St#refst{ref_type = with_bound_vars} 
    end.


side_effect(St = #refst{expressions = Exprs})->
    Children  = get_all_children(Exprs),
    SideEffs  = [Node || Node <- Children,
                         ((?ESG:data(Node) == #expr{kind = send_expr}) or
                          (?ESG:data(Node) == #expr{kind = receive_expr}))],
    Apps      = [Node || Node <- Children,
                         ?ESG:data(Node) == #expr{kind = application}],
    DirtyFunc = lists:flatten([?ESG:path(App, [{funref, {dirty, '==', true}}])
                               || App <- Apps]),
    UnKnown   = lists:flatten([?ESG:path(App, [{funref,{dirty,'==',unknown}}])
                               || App <- Apps]),
    case {length(SideEffs) /= 0, length(UnKnown) /=0, length(DirtyFunc) /= 0} of
        {true, _, _} -> St#refst{side_effect = true};
        {_, true, _} -> St#refst{side_effect = true};
%% egyenlore ha nem tudjuk eldonteni, hogy van-e mellekhatasa, akkor ugy
%% tekintunk ra mintha lenne, kesobb esetleg meg lehet kerdezni a felhasznalaot
%% hogy szerinte van-e
        {_, _, true} -> St#refst{side_effect = true};
        {_, _,    _} -> St#refst{side_effect = false}
    end.

get_all_children([])->
    [];
get_all_children(Exprs)->
    Children = [Node || {_, Node} <-
               lists:flatten([?SYNTAX:children(Expr) || Expr <- Exprs])],
    Exprs ++ get_all_children(Children).



guard_expression( St = #refst{expressions = Exprs})->
    #expr{type = Type1} = ?ESG:data(hd(Exprs)),
    #expr{type = Type2} = ?ESG:data(lists:last(Exprs)),
    case {Type1, Type2} of
        {guard, guard} -> 
            %%St#refst{guard = true},
            throw("Sorry, it does not work yet!");
        {guard, _} -> throw("The selected part is not a legal sequence!");
        {_, guard} -> throw("The selected part is not a legal sequence!");
        _          -> St#refst{guard = false}
    end.


get_fun_calls(St = #refst{function_obj = FunctionObj, file = File,
                          function = Function})->
    FunCalls    = ?ESG:path(FunctionObj,[{{funref, back},
                                      {kind, '==', application}}]),
    RecCalls    = [FunCall ||FunCall <- FunCalls, [Function] == ?ESG:path(
                    FunCall, [sup,{visib,back},scope,functx,{funcl,back}])],
    ModFunCalls = [FunCall || FunCall <- (FunCalls -- RecCalls), 
                              File == get_file_from_expr(FunCall)],
    RecCls      = [hd(?ESG:path(Call, [sup,{visib,back},scope,functx])) || 
                   Call <- RecCalls],
    St#refst{fun_calls = ModFunCalls, rec_fun_calls = RecCalls, 
             rec_fun_cl = RecCls}.

get_file_from_expr(Expr)->
    [File] = ?ESG:path(Expr, [sup, {visib, back}, scope, functx,
                              {funcl, back}, {form, back}]),
    File.


is_exported(St = #refst{function_obj = FunObj})->
    Exported = ?ESG:path(FunObj, [{funexp, back}]),
    case Exported of
        [] -> St#refst{exported = false};
        _  -> St#refst{exported = true}
    end.

%%% ----------------------------------------------------------------------------
%%% Checks


check_bound_var_name(_St = #refst{bound_vars = Vars, fun_calls = FunCalls,
                                  ref_type = with_bound_vars})->
    Names  = [(?ESG:data(Var))#expr.value || Var <- Vars],
    lists:map(
        fun(App)->
%% TODO: Ezt meg finomitani...
            [FunCl] = ?ESG:path(App, [sup, {visib, back}, scope, functx]),
            VisibVars = ?ESG:path(FunCl, [varvis]),
            VisibNames = [(?ESG:data(Var))#variable.name || Var <- VisibVars],
            Clashes = ?MISC:intersect(Names, VisibNames),
            case length(Clashes)<2 of
                true ->
                    ErrMsg = "The new variable " ++  var_io(Clashes) 
                    ++ " is already exist in the scope of a function call!";
                false ->
                    ErrMsg = "The new variables " ++  var_io(Clashes) 
                    ++ " are already exist in the scope of a function call!"
            end,
            ?MISC:error_on_difference(Clashes, [], ErrMsg)
        end, FunCalls),
    ok;

check_bound_var_name(_) -> ok.

var_io([])->
    "";
var_io([Head])->
    Head;
var_io([Head|Tail]) ->
    Head ++ ", " ++ var_io(Tail).    


check_recursive_calls(#refst{expressions = Expressions, module_name = MName,
                             function_name = Name, arity = Arity})->
    Apps = ?SEMINF:apps(sub, Expressions),
    Clashes = [error || App <- Apps, (({local, Name, Arity} == app_data(App)) 
                        or ({MName, Name, Arity} == app_data(App)))],
    ?MISC:error_on_difference(Clashes, [], 
                              "The selection cointains a resursive call").

app_data(App)->
    [Name1] = ?ESG:path(App, [{sub, 1}]),
    case ?ESG:data(Name1) of
        #expr{value = ':'} -> 
            Name = (?ESG:data(hd(?ESG:path(Name1, [{sub, 2}]))))#expr.value,
            Mod  = (?ESG:data(hd(?ESG:path(Name1, [{sub, 1}]))))#expr.value;
        _ ->
            Name = (?ESG:data(Name1))#expr.value,
            Mod = local
    end,
    Arity = length(?ESG:path(App, [sub]))-1,
    {Mod, Name, Arity}.
            
    

check_is_var_name(_St = #refst{varname = Name})->
    case ?LEX:is_valid_name(variable, Name) of
        false -> 
            Message = ?MISC:format("The given name ~p is not a legal " ++ 
                      "variable name!", [Name]),
            throw(Message);
        true  -> ok
    end.


check_whole_expr_is_selected(#refst{fromtoken=FromToken, totoken=ToToken,
                                    expressions = Exprs}) ->
     First = ?LEX:first_token(hd(Exprs)),
     Last  = ?LEX:last_token(lists:last(Exprs)),
      if
         First/= FromToken -> 
             throw("The selected part is not a legal sequence!");
         Last /= ToToken -> 
             throw("The selected part is not a legal sequence!");
         true -> 
             ok
      end.


check_listgen(#refst{topexpr = TopExpr})->
    case ?ESG:data(TopExpr) of
        #expr{kind = list_gen} ->
            throw("The selected expression  is a list generator!");
        _                      -> ok
    end,
    [{_,Parent}] = ?ESG:parent(TopExpr),
    case ?ESG:data(Parent) of
        #expr{kind = list_gen} ->
            Sub1 = ?ESG:path(Parent, [{sub,1}]),
            case Sub1 == [TopExpr] of
                true ->throw("The selected expression is in a list generator!");
                _    ->ok
            end;
        _ -> ok
    end.


check_is_pattern(St = #refst{fromexpr = FromExpr, toexpr = ToExpr})->
    case {?ESG:data(FromExpr), ?ESG:data(ToExpr)} of
        {#expr{type = pattern}, #expr{type = pattern}} -> 
            throw("The selected expression is a pattern!");
        {#expr{}, #expr{}} -> 
            St;
        _ ->
           throw("The selected part is not a legal sequence!")
    end.


check_fun_name(St)->
    check_fun_autoimported(St),
    check_fun_used(St).
 

check_fun_used(#refst{module=Module, function_name=Name, arity=Arity,
                      function_obj = FunObject})->
    FuncObjects = (?ESG:path(Module, [func]) ++
                   ?ESG:path(Module, [funimp])) -- [FunObject],
    FunRec      = [?ESG:data(Obj)|| Obj <- FuncObjects],
    Clashes     = [clash || #func{name=FName,arity=FArity} <- FunRec,
                            Name  == FName, Arity+1 == FArity],
    ErrMsg = ?MISC:format("The given function ~p/~p is already in use.",
                            [Name, Arity+1]),
    ?MISC:error_on_difference(Clashes, [], ErrMsg).


check_fun_autoimported(#refst{function_name=Name, arity=Arity}) ->
    Clashes = [erl_internal:bif(Name, Arity+1)],
    ErrMsg = ?MISC:format(
                "The given function ~p/~p is autoimported", [Name, Arity+1]),
    ?MISC:error_on_difference(Clashes, [false], ErrMsg).


check_new_var_name(_St = #refst{all_vars = Variables, varname = NewName})->
    VarNames  = ?SEMINF:var_names(Variables),
    Clashes   = [clash || VarName <- VarNames, NewName  == VarName],
    ErrMsg = ?MISC:format("The given variable ~p already exists.", [NewName]),
    ?MISC:error_on_difference(Clashes, [], ErrMsg).


check_local_vars(St = #refst{ref_type = with_bound_vars, used_vars = VarNodes, 
                             fun_clause = FunCl})->
    VarObjs     = ?SEMINF:varrefs(VarNodes) ++ ?SEMINF:varbinds(VarNodes),
    BoundVars   = ?SEMINF:inside_bound_vars(VarNodes, VarObjs),
    AllVarNodes = ?SEMINF:vars(clause, [FunCl]),
    AllVarObjs  = ?SEMINF:varrefs(AllVarNodes)++ ?SEMINF:varbinds(AllVarNodes),
    OutVarObjs  = AllVarObjs -- VarObjs,
    OutVarNodes = ?SEMINF:varbinds_back(OutVarObjs),
    BadList     = ?MISC:intersect(OutVarNodes, BoundVars),
    ErrNames    = [(?ESG:data(Bad))#expr.value|| Bad <- BadList],
    ErrMsg      = 
        "The following variables are bound inside the selection, " ++
        "but are used outside: " ++ var_io(ErrNames),
    ?MISC:error_on_difference(BadList, [], ErrMsg),
    VarNames    = lists:usort([(?ESG:data(Var))#expr.value|| Var <- VarNodes]),
    BoundNames  = lists:usort([(?ESG:data(Var))#expr.value|| Var <-BoundVars]),
    Names       = lists:usort(VarNames -- BoundNames),
    St#refst{used_var_names = Names, bound_vars = BoundVars};

check_local_vars(St = #refst{used_vars = VarNodes}) ->
    VarNames    = [(?ESG:data(Var))#expr.value|| Var <- VarNodes],
    St#refst{used_var_names = lists:usort(VarNames)}.


%%% ===========================================================================
%%% Transformations from referl_create

insert_app_arg(FunCallData, Type, ParNames) ->
    lists:foreach(
        fun({AppArgs, Exprs, App})->
            CExprsList = [{?ESG:copy(Expr), Expr}||Expr<-Exprs],
            CExprs     = find_copy_expr(CExprsList),
            Pars = [?SYNTAX:create(#expr{kind = variable}, [Par]) 
                    || Par <- ParNames],
            case Type of 
                var    -> [NewArg] = CExprs;
                funexp -> 
                    Cl     = ?SYNTAX:create(#clause{kind=funexpr},
                                        [{pattern, Pars},{body,CExprs}]),
                    NewArg = ?SYNTAX:create(#expr{kind=fun_expr},[{exprcl,Cl}])
            end,     
            case length(AppArgs) of
                0 -> Children = ?SYNTAX:children(App),
                     Tag = {before, element(2,lists:last(Children))};
                _ -> Tag = {next_to,lists:last(AppArgs)}
            end,
            ?SYNTAX:replace(App, Tag, [{sub,NewArg}])
        end, FunCallData).

insert_rec_app_arg(RecCallData, VarName)->
    lists:foreach(
        fun({AppArgs, _Exprs, App})->
            VarNode = ?SYNTAX:create(#expr{kind = variable}, [VarName]),
            case length(AppArgs) of
                0 -> Children = ?SYNTAX:children(App),
                     Tag = {before, element(2,lists:last(Children))};
                _ -> Tag = {next_to,lists:last(AppArgs)}
            end,
            ?SYNTAX:replace(App, Tag, [{sub,VarNode}])
        end, RecCallData).

insert_fun_pattern(Function, VarName, OrCl, RecFunCl)->
    FunCl  = ?GRAPH:path(Function, [funcl]),
    ClData = [{?GRAPH:path(Cl, [pattern]), Cl} || Cl <- FunCl],
    lists:foreach(
        fun({Pats, Cl})->
            case lists:member(Cl, [OrCl] ++ RecFunCl) of
                true -> 
                    New = ?SYNTAX:create(#expr{kind=variable},[VarName]);
                false -> 
                    New = ?SYNTAX:create(#expr{kind=variable},["_" ++ VarName])
            end,
            case length(Pats) of
                0 -> Tag = {pattern, 1, 1};
                _ -> Tag = {next_to,lists:last(Pats)}
            end,
            ?SYNTAX:replace(Cl, Tag, [{pattern,New}])
        end, ClData). 


replace_expressions(Parent, VarName, DelExprs, Type, ArgNames)->
    VarNode = ?SYNTAX:create(#expr{kind = variable}, [VarName]),
    case Type of 
        var    -> Node = VarNode;
        funexp -> 
            Args = [?SYNTAX:create(#expr{kind = variable}, [Arg]) 
                    || Arg <- ArgNames],
            Node        = ?SYNTAX:create(#expr{kind=application},
                                              [{sub,VarNode}, {sub, Args}])
    end,
    Delete = {hd(DelExprs), lists:last(DelExprs)},
    ?SYNTAX:replace(Parent,Delete,[{body, Node}]).


replace_subexpression(Parent, VarName, DelExpr, Type,ArgNames)->
    NewVar  = ?SYNTAX:create(#expr{kind = variable}, [VarName]),
    case Type of 
          var    -> New = NewVar;
          funexp -> 
            Args = [?SYNTAX:create(#expr{kind = variable}, [Arg]) 
                    || Arg <- ArgNames],
            New         = ?SYNTAX:create(#expr{kind=application},
                                              [{sub,NewVar}, {sub, Args}])
    end,
    ?SYNTAX:replace(Parent,{node, DelExpr},[{sub, New}]).


insert_old_fun(_, _, _, _, _, false)->
    ok;
insert_old_fun(File, Fun, Exprs, Type, ArgNames, true)->
    FunCl  = ?GRAPH:path(Fun, [funcl]),
    ClData = [{?GRAPH:path(Cl, [name]), ?GRAPH:path(Cl, [pattern]),
               ?GRAPH:path(Cl, [guard])} || Cl <- FunCl],
    FunCls = 
        lists:map(
            fun({[Name], Pats, Guard})->
                CGuardList = [{?ESG:copy(Expr), Expr}||Expr<-Guard],
                CGuard     = find_copy_expr(CGuardList),
                CPatsList1 = [{?ESG:copy(Expr), Expr}||Expr<-Pats],
                CPats1     = find_copy_expr(CPatsList1),
                CNameList1 = ?ESG:copy(Name),
                CName1     = find_copy_expr([{CNameList1, Name}]),
                CPatsList2 = [{?ESG:copy(Expr), Expr}||Expr<-Pats],
                CPats2     = find_copy_expr(CPatsList2),
                CNameList2 = ?ESG:copy(Name),
                CName2     = find_copy_expr([{CNameList2, Name}]),
                CExprsList = [{?ESG:copy(Expr), Expr}||Expr<-Exprs],
                CExprs     = find_copy_expr(CExprsList),
                Args = [?SYNTAX:create(#expr{kind = variable}, [Arg]) 
                        || Arg <- ArgNames],
                case Type of 
                    var    -> NewArg = CExprs;
                    funexp -> 
                         Clause = ?SYNTAX:create(#clause{kind=funexpr},
                                        [{pattern, Args},{body,CExprs}]),
                         NewArg = [?SYNTAX:create(#expr{kind=fun_expr},
                                                       [{exprcl,Clause}])]
                end,
                NewApp = ?SYNTAX:create(#expr{kind = application}, 
                                       [{sub, CName2},{sub, CPats2++NewArg}]),
                ?SYNTAX:create(#clause{kind = fundef}, [{name, CName1},
                                                        {pattern, CPats1}, 
                                                        {guard, CGuard}, 
                                                        {body,  [NewApp]}])
            end, ClData),
    OldFun = ?SYNTAX:create(#form{type = func}, [{funcl, FunCls}]),
    Index  = ?GRAPH:index(File, form, Fun),
    ?ESG:insert(File, {form, Index}, OldFun).

find_copy_expr(CExprsList)->
    lists:map(
        fun({List, Expr}) -> 
            {value, {Expr, CExpr}} = lists:keysearch(Expr, 1, List),
            CExpr 
        end, CExprsList).  
