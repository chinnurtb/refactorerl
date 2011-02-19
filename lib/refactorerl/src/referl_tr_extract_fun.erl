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

%%% @doc This module implements the extract function refactoring.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(referl_tr_extract_fun).
-vsn("$Rev: 1939 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/4]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

-record(refst, {filename, newname, frompos, topos,
                file, module, fromtoken, totoken,
                form, index, notboundvarnames,
                fromexpr, fromtype, fromkind, toexpr,
                topexpr, topclause, refcase, exprs,
                varrefs, vars, boundvars, notboundvars,
                arity, parent, delexprs}).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface

%% @spec do(string(), string(), integer(), integer()) -> ok
%%
%% @doc Create a new function definition from the selected expression(s)
%% between 
%% the given positions `Begin' and `End' if the side-conditions are hold, and 
%% replace the selection with a call of this new function. The name of the  
%% function is the parameter `FunName'. The variables used inside but 
%% bound outside the selected part becomes the parameters of the new function.
do(File, FunName, Begin, End) ->
    ?TRANSFORM:do(?MODULE, {File, FunName, Begin, End}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FileName, NewName, FromPos, ToPos}) ->
    #refst{filename = FileName,
           newname  = list_to_atom(NewName),
           frompos  = FromPos,
           topos    = ToPos}.

%%% ----------------------------------------------------------------------------
%%% Transformation steps

%% @private
steps() ->
    [
        fun file_node/1,
        fun module_node/1,
        fun from_token/1,
        fun to_token/1,
        fun from_expr_type_kind/1,

        fun check_from/1,

%        fun check_macro/1,

        fun to_expr/1,
        fun top_expr/1,
        fun check_is_pattern/1,
        fun refac_case/1,
        fun top_clause/1,
        fun selected_exprs/1,

        fun check_top_expr/1,
        fun check_whole_expr_is_selected/1,
        fun check_is_binary_field/1,

        fun form_index/1,
        fun used_vars/1,
        fun bindings/1,

        fun check_is_function_name/1,
        fun check_is_autoimported/1,
        fun check_existing_funname/1,
        fun check_imported_funname/1,
        fun check_all_var_bound_ok/1,

        fun parent/1,
        fun delexprs/1,
        fun eliminate_begin_end/1
    ].


%% @private
%% Removes the selection and inserts a function application instead.
%% Adds the selection as a new function at the end of the file.
transform(#refst{topexpr          = TopExpr,
                 refcase          = RefacCase,
                 file             = File,
                 index            = FormIndex,
                 newname          = Name,
                 notboundvarnames = NotBoundVarNames,
                 exprs            = Exprs,
                 parent           = Parent,
                 delexprs         = DelExprs}) ->
    case RefacCase of
        expr ->
            Index  = ?ESG:index(Parent, sub, TopExpr),
            replace_with_app(Parent, Name, NotBoundVarNames,
                                     hd(DelExprs),Index, sub);
        _  ->
            replace_with_app(Parent, Name, NotBoundVarNames,
                                     DelExprs, index, body)
    end,
    add_fun_form(File, Name, NotBoundVarNames, Exprs, FormIndex),
    ?ESG:close(),

    {[File], ok}.


%%% ============================================================================
%%% Implementation

file_node(St = #refst{filename = FileName}) ->
    case ?SYNTAX:file(FileName) of
        {file, File} -> St#refst{file = File};
        not_found    -> throw({"The file does not exist in the database!",
                               FileName})
    end.


module_node(St = #refst{file = File}) ->
    case ?GRAPH:path(File, [moddef]) of
        [ModuleNode] -> St#refst{module = ModuleNode};
        _            -> throw("The module node does not exist in the graph!")
    end.


from_token(St = #refst{file = File, frompos = FromPos}) ->
    case  ?LEX:token_by_pos(File, FromPos) of
        {ok, FromToken} -> St#refst{fromtoken = FromToken};
        _         -> throw("There is not any lexical element " ++
                            "in the selected position")
    end.


to_token(St = #refst{file = File, topos = ToPos}) ->
    case  ?LEX:token_by_pos(File, ToPos) of
        {ok, ToToken} -> St#refst{totoken = ToToken};
        _         -> throw("There is not any lexical element " ++
                            "in the selected position")
    end.


from_expr_type_kind(St = #refst{fromtoken = FromToken}) ->
    [{_,FromExpr}] = ?ESG:parent(FromToken),
    case ?ESG:data(FromExpr) of
        #expr{type=FromType, kind = FromKind} ->
            St#refst{fromexpr = FromExpr,
                     fromtype = FromType,
                     fromkind = FromKind };
        _ ->
            throw("The selected part is not a legal sequence")
    end.


to_expr(St = #refst{totoken = ToToken}) ->
    [{_,ToExpr}] = ?ESG:parent(ToToken),
    case ?ESG:data(ToExpr) of
        #expr{} -> St#refst{toexpr = ToExpr};
        _ -> throw("The selected part is not a legal sequence")
    end.


top_expr(St = #refst{fromexpr = FromExpr, toexpr = ToExpr}) ->
    case ?SYNTAX:top_node(FromExpr, ToExpr) of
        {expr, TopExpr, _, _}   -> St#refst{topexpr = TopExpr};
        {clause, TopExpr, _, _} -> St#refst{topexpr = TopExpr};
        _  -> throw("The selected part is not a legal sequence!!!")
    end.


%% Three cases of the refactoring according to the selection:
%%      refcase_expr      : an expression that does not form a whole body
%%      refcase_expr_body : an expression that forms a whole body
%%      refcase_body      : a part of a body, contains multiple expressions
refac_case(St = #refst{topexpr = TopExpr}) ->
    case {?ESG:data(TopExpr), ?ESG:data(?SYNTAX:single_parent(TopExpr))} of
        {#expr{},   #expr{}}   -> St#refst{refcase = expr};
        {#expr{},   #clause{}} -> St#refst{refcase = expr_body};
        {#clause{}, _}         -> St#refst{refcase = body};
        {_, _} -> throw("The selected part is not a legal sequence!")
    end.


top_clause(St = #refst{fromexpr = Expr}) ->
    case ?ESG:path(Expr, [sup, {visib, back}, scope, functx]) of
        [] -> throw("The selected part is not a legal sequence!");
        [TopClause] ->
            St#refst{topclause = TopClause}
    end.


%% Returns the list of the selected expressions.
selected_exprs(St = #refst{refcase = body, fromexpr = FromExpr,
                           toexpr = ToExpr, topexpr = ClauseNode}) ->
    BodyExprs = ?ESG:path(ClauseNode,[body]),
    case BodyExprs of
        [] -> throw("The selected part is not a legal sequence!");
        _  -> ok
    end,
    [From]    = ?ESG:path(FromExpr, [sup]),
    [To]      = ?ESG:path(ToExpr, [sup]),
    Selected  = ?MISC:separate_interval(BodyExprs, From, To),
    case Selected of
        [] -> throw("The selected part is not a legal sequence!");
        _  -> ok
    end,
    St#refst{exprs = Selected};

selected_exprs(St = #refst{topexpr = TopExpr}) ->
    St#refst{exprs = [TopExpr]}.


form_index(St = #refst{exprs = [Expr1|_], file = File}) ->
    [Form] = ?ESG:path(Expr1, [sup, {visib,back}, scope, functx, {funcl,back}]),
    Index  = ?ESG:index(File, form, Form),
    St#refst{form = Form, index = Index}.


used_vars(St = #refst{exprs = Exprs}) ->
    Vars    = ?SEMINF:vars(sub, Exprs),
    VarRefs = ?SEMINF:varrefs(Vars) ++ ?SEMINF:varbinds(Vars),
    St#refst{vars = Vars, varrefs = VarRefs}.


bindings(St = #refst{vars = Vars, varrefs = VarRefs}) ->
    BoundVars        = ?SEMINF:inside_bound_vars(Vars, lists:usort(VarRefs)),
    NotBoundVars     = Vars -- BoundVars,
    NotBoundVarNames = lists:usort(
                       [(?ESG:data(Var))#expr.value|| Var <- NotBoundVars]),
    Arity            = length(NotBoundVarNames),
    St#refst{boundvars        = BoundVars,
             notboundvars     = NotBoundVars,
             notboundvarnames = NotBoundVarNames,
             arity            = Arity }.


parent(St = #refst{exprs = Exprs})->
    [{_,Parent}] = ?ESG:parent(hd(Exprs)),
    St#refst{parent = Parent}.

delexprs(St = #refst{exprs = Exprs})->
    St#refst{delexprs = Exprs}.


eliminate_begin_end(#refst{refcase = body})->
    ok;
eliminate_begin_end(St = #refst{exprs = Exprs} ) ->
    case ?ESG:data(hd(Exprs)) of
        #expr{kind = block_expr} ->
            NewExprs = ?ESG:path(hd(Exprs), [exprcl, body]),
            St#refst{exprs = NewExprs};
        _ -> St
    end.



%%% ----------------------------------------------------------------------------
%%% Checks

check_whole_expr_is_selected(#refst{fromtoken=FromToken, totoken=ToToken,
                                    exprs = Exprs}) ->
     First = ?LEX:first_token(hd(Exprs)),
     Last  = ?LEX:last_token(lists:last(Exprs)),
      if
         First/= FromToken -> 
             throw("The selected part is not a legal sequence");
         Last /= ToToken   -> 
             throw("The selected part is not a legal sequence");
         true -> 
             ok
      end.

check_is_function_name(#refst{newname = NewName}) ->
    case ?LEX:is_valid_name(function, atom_to_list(NewName)) of
        true -> ok;
        _    -> 
            Message = ?MISC:format("The given name ~p is not a legal " ++ 
                      "function name!", [NewName]),
            throw(Message)
    end.


check_is_autoimported(#refst{newname = NewName, arity = NewArity})->
    Clashes     = [erl_internal:bif(NewName, NewArity)],
    ErrFunArity = ?MISC:format("~p/~p", [NewName, NewArity]),
    ErrMsg      = "The given function " ++ ErrFunArity ++ " is autoimported",
    ?MISC:error_on_difference(Clashes, [false], ErrMsg).

check_existing_funname(#refst{module=Module, newname=NewName, arity=Arity})->
    check_funs(Module, NewName, Arity, func).

check_imported_funname(#refst{module=Module, newname=NewName, arity=Arity})->
    check_funs(Module, NewName, Arity, funimp).


check_funs(Module, NewName, NewArity, Link) ->
    Objects = ?GRAPH:path(Module, [Link]),
    Clashes = [clash || Obj <- Objects,
                        #func{name=Name,arity=Arity} <- [?GRAPH:data(Obj)],
                        Name  == NewName, Arity == NewArity],
    ErrFunArity = ?MISC:format("~p/~p", [NewName, NewArity]),
    ErrMsg      = "The given function " ++ErrFunArity++ " exists in the module",
    ?MISC:error_on_difference(Clashes, [], ErrMsg).


%% check_macro(#refst{fromexpr = Expr, toexpr = Expr}) -> ok;
%% check_macro(#refst{fromtoken = FromToken}) ->
%%     PossibleMacro = ?GRAPH:path(FromToken, [{mbody,back}]),
%%     ?MISC:error_on_difference(PossibleMacro, [], in_macro, {}).

check_from(#refst{fromtype = guard}) ->
    throw("The selected expression is a guard expression");
check_from(#refst{fromkind = conjunction}) ->
    throw("The selected expression is a guard expression");
check_from(#refst{fromkind = disjunction}) ->
    throw("The selected expression is a guard expression");
check_from(_) ->
    ok.


check_top_expr(#refst{topexpr = TopExpr}) ->
    case ?ESG:data(TopExpr) of
        #expr{kind = list_gen} ->
            throw("The selected expression  is a list generator");
        _                      -> ok
    end,
    [{_,Parent}] = ?ESG:parent(TopExpr),
    case ?ESG:data(Parent) of
        #expr{kind = list_gen} ->
            Sub1 = ?ESG:path(Parent, [{sub,1}]),
            case Sub1 == [TopExpr] of
                true -> throw("The selected expression is in a list generator");
                _    -> ok
            end;
        _ -> ok
    end.


check_is_pattern(St = #refst{fromexpr = FromExpr,  toexpr = ToExpr})->
    case {?ESG:data(FromExpr), ?ESG:data(ToExpr)} of
        {#expr{type = pattern}, #expr{type = pattern}} -> 
            throw("The selected expression is a pattern!");
        {#expr{}, #expr{}} -> 
            St;
        _ ->
           throw("The selected part is not a legal sequence!")
    end.


check_is_binary_field(#refst{topexpr = TopExpr})->
    case ?ESG:data(element(2,hd(?ESG:parent(TopExpr)))) of
        #expr{kind = binary_field} -> throw("The selected expression "
                                             ++ " is a binary_field");
        _                          -> ok
    end.


check_all_var_bound_ok(#refst{boundvars = BoundVars, varrefs = VarObjs,
                              topclause = TopClause})->
    AllVarNodes = ?SEMINF:vars(clause, [TopClause]),
    AllVarObjs  = ?SEMINF:varrefs(AllVarNodes) ++ ?SEMINF:varbinds(AllVarNodes),
    OutVarObjs  = AllVarObjs -- VarObjs,
    OutUsedVarBindings = ?SEMINF:varbinds_back(OutVarObjs),
    BadList     = ?MISC:intersect(OutUsedVarBindings, BoundVars),
    ErrNames    = [(?ESG:data(Bad))#expr.value|| Bad <- BadList],
    ErrMsg      = 
        "The following variables are bound inside the selection, " ++
        "but are used outside: " ++ var_io(ErrNames),
    ?MISC:error_on_difference(BadList, [], ErrMsg).

var_io([])->
    "";
var_io([Head])->
    Head;
var_io([Head|Tail]) ->
    Head ++ ", " ++ var_io(Tail).


%%% Transformations from referl_create

%% Extract function uses it
replace_with_app(Parent, AppName, AppArgNames, DelExprs, Index, Type)->
    Name = ?SYNTAX:create(#expr{kind = atom}, [atom_to_list(AppName)]),
    Args = [?SYNTAX:create(#expr{kind = variable}, [AName]) 
                                || AName <- AppArgNames],
    App  = ?SYNTAX:create(#expr{kind = application}, [{sub, [Name]}, 
                               {sub, Args}]),
    case Type of 
        sub  -> ?ESG:remove(Parent, sub, DelExprs),
                ?ESG:insert(Parent, {sub,Index}, App);
        body -> Dels = {hd(DelExprs), lists:last(DelExprs)},
                ?SYNTAX:replace(Parent, Dels, [{body, [App]}])
    end.


%% Extract function uses it
add_fun_form(File, FunName, ClPatternNames, Body, FormIndex) ->
    ClPatternNodes = [?SYNTAX:create(#expr{kind = variable},[Name])
                     || Name <- ClPatternNames ],
    FName   =   ?SYNTAX:create(#expr{kind = atom}, [atom_to_list(FunName)]),
    ClNode  = ?SYNTAX:create(#clause{kind = fundef},[{name, FName},
                             {pattern, ClPatternNodes},{body, Body}]),
    FunForm = ?SYNTAX:create(#form{type = func}, [{funcl,ClNode}]),
    ?ESG:insert(File, {form, FormIndex +1}, FunForm).


