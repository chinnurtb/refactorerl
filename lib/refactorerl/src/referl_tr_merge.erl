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

%%% @doc This module implements the ``merge expression duplicates'' transformation.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(referl_tr_merge).
-vsn("$Rev: 1566 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/4]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

-record(refst,  {
                    filename,       % name of the file
                    fpos, tpos,     % the beginning and the end of the selection
                    newname,        % the name of the variable to be introduced

                    file,           % the file node
                    topexpr,        % the selected expression node
                    instparents,    % the instances of the expression
                                    % and their parents in the tree
                    inspoint,       % the insertion point of the new match expr
                    changeinsts,
                    matchexprparent,
                    insprev,
                    vars,
                    allvars
                }).

%%% ============================================================================
%%% Errors

%warn_only_instance() ->
%    "Warning: name is only instance.".

no_expr_found() ->
    "The selection is not a valid expression.".

too_many_overexprs() ->
    "Too many over-expressions".

invalid_var_name(Name) ->
    ?MISC:format("\"~p\" is not a variable name.", [Name]).

error_not_replaceable(TopExpr) ->
    IsListComp  = "Selection is in a list comprehension",
    IsListGen   = "Selection is in a list generator",
    IsGuard     = "Selection is in a guard expression",
    IsPattern   = "Selection is in a pattern",
    Errors =
        [   {IsListComp,    over_expr_has_type(TopExpr, list_comp)},
            {IsListGen,     over_expr_has_type(TopExpr, list_gen)},
            {IsGuard,       sup_expr_has_type(TopExpr, guard)},
            {IsPattern,     sup_expr_has_type(TopExpr, pattern)} ],

    case [ ErrMsg || {ErrMsg, true} <- Errors ] of
        [ErrMsg|_] -> throw(ErrMsg);
        _          -> throw("Internal error")
    end.

%%% ============================================================================
%%% Interface

%% @spec do(string(), integer(), integer(), string()) -> [node()]
%% @doc Merges all duplicates of the selection in the file to the new name.
%%      Returns the node of the file that has been changed.
do(FileName, FPos, TPos, NewName) ->
    ?TRANSFORM:do(?MODULE, {FileName, FPos, TPos, NewName}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FileName, FPos, TPos, NewName}) ->
    #refst{
        filename    = FileName,
        fpos        = FPos,
        tpos        = TPos,
        newname     = NewName
    }.

%% @private
steps() ->
    [
        fun check_var_name/1,
        fun check_var_exists/1,
        fun file/1,
        fun top_expr/1,
        fun selection_vars/1,
        fun instances/1,
        fun inst_parents/1,
        fun check_replaceable_selection/1,
        fun match_expr_parent/1,
        fun inspoint/1
    ].

%% @private
transform(St = #refst{file = File}) ->
    insert_new_match_expr(St),
    change_instances(St),
    {[File], ok}.


insert_new_match_expr(#refst{newname         = NewName,
                             topexpr         = TopExpr,
                             matchexprparent = MatchExprParent,
                             insprev         = InsPrev }) ->

    {value, {_, TopCopy}} = lists:keysearch(TopExpr, 1, ?ESG:copy(TopExpr)),
    NewVar       = ?SYNTAX:create(#expr{kind=variable}, [NewName]),
    NewMatchExpr = ?SYNTAX:create(#expr{kind=match_expr},
                                  [{sub, NewVar}, {sub, TopCopy}]),
    MatchExpr    = [ {body, NewMatchExpr} ],

    case InsPrev of
        []       -> InsTo = {body, 1, 1};
        [Prev|_] -> InsTo = {next_to, Prev}
    end,

    ?SYNTAX:replace(MatchExprParent, InsTo, MatchExpr),
    ?ESG:close().


% TODO if parenthesized expression is replaced, do not leave the parentheses
change_instances(#refst{newname     = NewName,
                        instparents = InstParents }) ->
    ReplInstParents = InstParents,
    lists:foreach(
        fun({RInst, {_Link, RParent}}) ->
            NewVar = ?SYNTAX:create(#expr{kind=variable}, [NewName]),
            ?SYNTAX:replace(RParent, {RInst, RInst}, [{body, [NewVar]}]),
            ?ESG:close()
        end, ReplInstParents).

%%% ============================================================================
%%% Preparation

file(St = #refst{filename = FileName}) ->
    {file, File} = ?SYNTAX:file(FileName),
    St#refst{file = File}.

top_expr(St = #refst{file = File,
                     fpos = FPos,
                     tpos = TPos}) ->
    {ok, FToken} = ?LEX:token_by_pos(File, FPos),
    {ok, TToken} = ?LEX:token_by_pos(File, TPos),
    case ?SYNTAX:top_node(FToken, TToken) of
        {expr, TopExpr, _, _} ->
            St#refst{topexpr = TopExpr};
        {lex, TopLex, _, _}   ->
            {_, TopExpr} = ?SYNTAX:single_parent_link(TopLex),
            St#refst{topexpr = TopExpr};
        _                     ->
            throw(no_expr_found())
    end.


selection_vars(St = #refst{topexpr = TopExpr}) ->
    Vars    = ?SEMINF:vars(sub, [TopExpr]),
    AllVars = [ ?ESG:path(Var, [varref, {varref, back}]) || Var <- Vars ],
    St#refst{vars = Vars, allvars = AllVars}.


instances(St = #refst{topexpr = TopExpr, file = File, vars = []}) ->
    [_ToFile, ToForm|_] = ?SYNTAX:path_from_root(TopExpr),
    [TopForm]           = ?ESG:path(File, [ToForm]),
    AllSynNodes         = ?SYNTAX:all_syntax_nodes(TopForm),
    AllSynDatas         = [{Node, ?ESG:data(Node)} || Node <- AllSynNodes],
    TopExprData         = ?ESG:data(TopExpr),
    Insts               = [Inst     ||  {Inst, InstData} <- AllSynDatas,
                                        InstData == TopExprData,
                                        is_same_subtree(Inst, TopExpr) ],
    St#refst{changeinsts = Insts};
instances(St = #refst{  topexpr = TopExpr,
                        vars    = [Var1|_],
                        allvars = [Var1s|_] }) ->
    {_, _, VarPathToTop, _} = ?SYNTAX:top_node(Var1, TopExpr),
    MaybeInsts  = [ var_top(Bind, VarPathToTop) || Bind <- Var1s ],
    Insts       = [ Inst || Inst <- lists:flatten(MaybeInsts),
                            is_same_subtree(Inst, TopExpr),
                            is_valid_inst(Inst) ],
    St#refst{changeinsts = Insts}.


inst_parents(St = #refst{changeinsts = Insts}) ->
    InstParents = [ {Inst, ?SYNTAX:single_parent_link(Inst)} || Inst <- Insts ],
    St#refst{instparents = InstParents}.


match_expr_parent(St = #refst{changeinsts = Insts}) ->
    case Insts of
        [OnlyInst] ->
            {_, ImmediateParent} = ?SYNTAX:single_parent_link(OnlyInst);
        [Inst1, Inst2|RestInsts] ->
            ImmediateParent = lists:foldl(fun top_node_fun/2,
                                          top_node_fun(Inst1, Inst2), RestInsts)
    end,
    MatchExprParent = ?SYNTAX:ancestor_with_type(clause, ImmediateParent),
    St#refst{matchexprparent = MatchExprParent}.




%% Returns the insertion parent and the previous child
%% (or no_prev, if there is no such child).
inspoint(St = #refst{matchexprparent = MatchExprParent, allvars = AllVars}) ->
    RevChildren = lists:reverse(?SYNTAX:children(MatchExprParent)),
    InsPrev = [ Child || {body, Child} <- RevChildren,
                         binds_var(Child, AllVars) orelse
                            not has_any_var(Child, AllVars) ],
    St#refst{insprev = InsPrev}.


%%% ============================================================================
%%% Implementation

%% Tries to follow a parent sequence from a node.
%% If it yields a node, the node is possibly an instance of the expression.
var_top(Node, Path) ->
    BackPath = [ {Link, back} || {Link, _Index} <- Path ],
    case ?ESG:path(Node, BackPath) of
        [PossibleInst] -> PossibleInst;
        _              -> []
    end.


is_same_subtree(BindNode, Node) ->
    BindChildren = ?ESG:children(BindNode),
    Children     = ?ESG:children(Node),
    if
        length(BindChildren) == length(Children) ->
            DiffChildren =
                [ {diff, BCh, Ch}
                    ||  {{_, BCh}, {_, Ch}} <- lists:zip(BindChildren, Children),
                        not is_same_subtree(BCh, Ch) ],
            is_same_node(BindNode, Node) andalso DiffChildren == [];
        true ->
            false
    end.

is_same_node(Node1, Node2) ->
    Data1 = ?ESG:data(Node1),
    Data2 = ?ESG:data(Node2),
    case {Data1, Data2} of
        {#lex{type = T1, data = #token{type = TT1, value = V1}},
         #lex{type = T2, data = #token{type = TT2, value = V2}}} ->
            T1 == T2 andalso TT1 == TT2 andalso V1 == V2;
        {D1, D2} ->
            D1 == D2
    end.


%% Returns whether the expression binds the variable.
%% TODO make it more accurate
binds_var(Node, Vars) ->
    VarNodes = lists:usort(lists:flatten(
                [ ?ESG:path(Var, [varref]) || Var <- lists:flatten(Vars) ])),
    case ?ESG:data(Node) of
        #expr{} ->
            [] /= ?MISC:intersect(?ESG:path(Node, [{varintro, back}]), VarNodes);
        _ ->
            false
    end.


%% Returns whether a node has any of the listed variables.
has_any_var(Node, Vars) ->
    ExprVars = ?SEMINF:vars(sub, [Node]),
    [] == [ has_var || Var <- Vars, lists:member(Var, ExprVars) ].


top_node_fun(Node1, Node2) ->
    {_, Parent, _, _} = ?SYNTAX:top_node(Node1, Node2),
    Parent.


%% Returns whether the node is allowed as an instance.
is_valid_inst(Node) ->
    InvalidInst =   over_expr_has_type(Node, list_comp) orelse
                    over_expr_has_type(Node, list_gen) orelse
                    sup_expr_has_type(Node, guard) orelse
                    sup_expr_has_type(Node, pattern),
    not InvalidInst.


%% Returns the expression that contains the clause above the expression.
%% Throws an error if there is more than one such node.
%% TODO better throw
over_expr(Expr) ->
    case ?ESG:path(Expr, [sup, {body, back}, {exprcl, back}]) of
        [_,_|_] -> throw(too_many_overexprs());
        Found   -> Found
    end.


%% Returns whether the expression has the expected type.
expr_has_type(Expr, ExpectedType) ->
    #expr{type = Type} = ?ESG:data(Expr),
    Type == ExpectedType.

%% Returns whether the over-expression has the expected type.
over_expr_has_type(Expr, ExpectedType) ->
    case over_expr(Expr) of
        []         -> false;
        [OverExpr] -> expr_has_type(OverExpr, ExpectedType)
    end.

%% Returns whether the super-expression has the expected type.
sup_expr_has_type(Expr, ExpectedType) ->
    [SupExpr] = ?ESG:path(Expr, [sup]),
    expr_has_type(SupExpr, ExpectedType).


%%% ============================================================================
%%% Checks

% TODO check also the following
% - macros
% - BIFs
% - send, receive

%% TODO move it to another module as a more general function
check_var_name(#refst{newname = Name = [FirstChar|_]}) ->
    if
        $A =< FirstChar, FirstChar =< $Z -> ok;
        true                             -> throw(invalid_var_name(Name))
    end.

check_replaceable_selection(#refst{topexpr = TopExpr, changeinsts = ChangeInsts}) ->
    case lists:member(TopExpr, ChangeInsts) of
        true  -> ok;
        false -> error_not_replaceable(TopExpr)
    end.

% TODO NewName is already bound
check_var_exists(#refst{newname = _NewName}) ->
    ok.
