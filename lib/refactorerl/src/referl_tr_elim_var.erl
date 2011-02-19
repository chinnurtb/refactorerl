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

%%% @doc This module implements the eliminate variable refactoring.
%%%
%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>

-module(referl_tr_elim_var).
-vsn("$Rev: 1967 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/2]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

-record(refst, {filename, file, pos, var,
                def, def_used, fvars, binding, refs}).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface

%% @spec do(string(), integer()) -> ok
%%
%% @doc Eliminate the variable in the pointed position `Pos'.
do(File, Pos) ->
     ?TRANSFORM:do(?MODULE, {File, Pos}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FileName, Pos}) ->
    #refst{ filename = FileName,
            pos = Pos }.

%% @private
steps() ->
    [?MISC:get_file_node(#refst.filename, #refst.file),
     fun var_node/1,
     fun get_var_def/1,
     fun def_is_used/1,
     fun check_sideeffects/1,
     fun get_free_vars/1,
     fun get_refs/1,
     fun check_macro/1,
     fun check_visibility/1
    ].

%% @private
transform(#refst{file = File,
                 def = Def, def_used = Used,
                 refs = Refs, binding = Binding}) ->
    lists:foreach(fun (R) -> replace(R, enclose(copy(Def))) end, Refs),
    ?ESG:close(),
    if not Used -> delete(Binding);
       true     -> replace(Binding, enclose(Def))
    end,
    ?ESG:close(),
    {[File],ok}.

copy(Node) ->
    proplists:get_value(Node, ?ESG:copy(Node)).

enclose(Node) ->
    ?SYNTAX:create(#expr{kind=parenthesis}, [{sub, Node}]).

replace(From, To) ->
    [{Tag, Parent}] = ?ESG:parent(From),
    ?SYNTAX:replace(Parent, {node, From}, [{Tag, To}]).

delete(Node) ->
    [{_, Parent}] = ?ESG:parent(Node),
    ?SYNTAX:replace(Parent, {node, Node}, []).

%%% ============================================================================
%%% Implementation

var_node(St = #refst{file = File, pos = Pos}) ->
    Token = ?LEX:get_token(File, Pos),
    [{_,Expr}] = ?ESG:parent(Token),
    case ?ESG:data(Expr) of
        #expr{kind = variable} ->
            [Var] = ?ESG:path(Expr, [varref]) ++ ?ESG:path(Expr, [varbind]),
            St#refst{var = Var};
        _ ->
            throw("that's not a variable")
    end.

get_var_def(St = #refst{var = Var}) ->
    case ?ESG:path(Var, [{varbind, back}]) of
        [Pattern] ->
            case ?ESG:path(Pattern, [{sub, back}]) of
                [Match] ->
                    case ?ESG:data(Match) of
                        #expr{kind = match_expr} ->
                            [Pattern, Def] = ?ESG:path(Match, [sub]),
                            St#refst{def = Def};
                        _ ->
                            throw("cannot eliminate that variable")
                    end;
                _ -> % var is a function parameter
                    throw("cannot eliminate a function parameter")
            end;
        _ -> % var has multiple bindings
            throw("cannot eliminate variables with multiple bindings")
    end.

def_is_used(St = #refst{var = Var}) ->
    [Pattern] = ?ESG:path(Var, [{varbind, back}]),
    [Intro] = ?ESG:path(Var, [varintro]),
    %% If the result of the binding expression is not used directly,
    %% then Pattern will be among Intros children
    case lists:member(Pattern, ?ESG:path(Intro, [sub])) of
        false -> St#refst{def_used = true};
        true  -> St#refst{def_used = last_expr(Intro)}
    end.

%% @spec last_expr(Expr::node()) -> bool()
%%
%% @doc `Expr' is the last expression in the clause. `Expr' must be a
%% direct child of its clause.
last_expr(E) ->
    E == ?ESG:path(E, [{visib, back}, {visib, last}]).

check_sideeffects(#refst{def = Def}) ->
    ?MISC:error_on_difference(any_tree(fun has_sideeffect/1, Def), false,
                             "the variable definition has sideeffects").

has_sideeffect(Node) ->
    case ?ESG:data(Node) of
        #expr{kind = application} ->
            case ?ESG:path(Node, [funref]) of
                [Fun] -> (?ESG:data(Fun))#func.dirty;
                [] -> false
            end;
        _ ->
             false
    end.

get_free_vars(St = #refst{def = Def}) ->
    FVars = free_vars(Def),
    St#refst{fvars = FVars}.

get_refs(St = #refst{var = VarNode}) ->
    Exprs = ?ESG:path(VarNode, [{varref, back}]),
    [Binding] = ?ESG:path(VarNode, [{varbind, back}, {sub, back}]),
    St#refst{refs = Exprs, binding = Binding}.

check_macro(#refst{refs = Refs}) ->
    Check = lists:all(fun is_from_macro/1, Refs),
    ?MISC:error_on_difference(Check, true, "variable is a macro argument"
                                           " (not supported yet)").

is_from_macro(Expr) ->
    [] == ?ESG:path(Expr, [{elex, {data, '==', virtual}}]).

check_visibility(#refst{refs = Refs, fvars = FVars}) ->
    Check = lists:all(fun (R) -> FVars -- visible_vars(R) == [] end, Refs),
    ?MISC:error_on_difference(Check, true, "the definition refers to a variable"
                                           " wich is shadowed at an occurence").

visible_vars(Expr) ->
    ?ESG:path(Expr, [sup, {visib, back}, varvis]).

%%% ============================================================================
%%% Misc. functions

%% why isn't this in lists?
any(Bs) -> lists:any(fun (X) -> X end, Bs).

any_tree(Pred, Node) ->
    fold_tree(fun(N, B) -> Pred(N) orelse B end, fun(Bs) -> any(Bs) end, Node).

free_vars(Node) ->
    FVars = fold_tree(collect_vars([varref]),  fun lists:flatten/1, Node),
    BVars = fold_tree(collect_vars([varbind]), fun lists:flatten/1, Node),
    lists:usort(FVars) -- BVars.

collect_vars(Path) ->
    fun (Node, Vars) ->
            case ?ESG:data(Node) of
                #expr{kind = variable} ->
                    ?ESG:path(Node, Path) ++ Vars;
                _ ->
                    Vars
            end
    end.

fold_tree(Reduce, Combine, Root) ->
    Reduce(Root, Combine([fold_tree(Reduce, Combine, N) ||
                             {_, N} <- ?ESG:children(Root)])).
