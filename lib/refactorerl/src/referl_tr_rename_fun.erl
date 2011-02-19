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

%%% @doc This module implements the rename function refactoring.
%%% Currently it works only in special cases and it does not check any
%%% conditions.
%%%
%%% @todo Now every file is saved; only the modified ones should be.
%%% @todo Files should not be saved here.
%%% @todo The function could be selected by clicking on the export list's
%%% corresponding item.
%%% @todo If a function is exported and is imported to module `a', and the new
%%% form of the function would collide with a local or imported function in `a',
%%% then the refactoring should not be allowed.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(referl_tr_rename_fun).
-vsn("$Rev: 1952 $ ").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/3, do/4]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

%%% type refst() = #refst{spectype = (pos | mfa),
%%%                       filename = string(),
%%%                       pos = natural(),
%%%                       modname = atom(),
%%%                       oldfunname = atom(),
%%%                       arity = integer(),
%%%                       newfunname = atom() | string(),
%%%                       funnode = node()}.
%%%
%%% Describes a specific rename function refactoring.
%%%
%%% Fields:
%%% <ul>
%%%     <li>`spectype': If `pos', then the specification is given with the
%%%         `filename', `pos' and `newfunname' fields. If it is `mfa', the
%%%         specification is given with the `modname', `oldfunname', `arity' and
%%%         `newfunname' fields.</li>
%%%     <li>`filename', `pos': The name and the position where the function to
%%%         be renamed is.</li>
%%%     <li>`modname', `oldfunname', `arity': The module, the name and the arity
%%%         of the function to be renamed.</li>
%%%     <li>`newfunname': The new name of the function to be renamed.</li>
%%%     <li>`funnode': The semantic node of the function to be renamed.</li>
%%% </ul>
-record(refst, {spectype, filename, pos, modname, oldfunname, arity, newfunname,
                funnode}).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface

% do({...}) ->
%     todo.

%% @spec do(string(), integer(), atom() | string()) -> ok
%%
%% @doc Renames the function at `Pos' position in `FileName' file to
%% `NewFunName'.
%%
%% @todo Check that FileName is a list, Pos is an integer and NewFunName is an
%% atom.
do(FileName, Pos, NewFunName) ->
    ?TRANSFORM:do(?MODULE, {pos, {FileName, Pos, NewFunName}}).

%% @spec do(atom(), atom(), integer(), atom() | string()) -> ok
%%
%% @doc Renames the `OldFunName' function in the `ModName' module to
%% `NewFunName'.
%%
%% @todo Check the type of the arguments.
do(ModName, OldFunName, Arity, NewFunName) ->
    ?TRANSFORM:do(?MODULE, {mfa, {ModName, OldFunName, Arity, NewFunName}}).

%%% ============================================================================
%%% Callbacks

%% @private
init({pos, {FileName, Pos, NewFunName}}) ->
    #refst{spectype = pos,
           filename = FileName,
           pos = Pos,
           newfunname = NewFunName};
init({mfa, {ModName, OldFunName, Arity, NewFunName}}) ->
    #refst{spectype = mfa,
           modname = ModName,
           oldfunname = OldFunName,
           arity = Arity,
           newfunname = NewFunName}.

%% @private
steps() ->
    [fun preparation/1].


%% @private
transform(St = #refst{funnode = FunNode}) ->
    %% calculating the updates
    Updates = calc_update(FunNode, St),

    %% applying the updates
    lists:foreach(
      fun({Node, NewData}) ->
              ?ESG:update(Node, NewData)
      end, Updates),

    ?ESG:close(),

    %% TODO delete next line?
    [ referl_fileman:save_file(X) || X <- ?ESG:path(?ESG:root(), [file]) ],
    {?ESG:path(?ESG:root(), [file]), ok}.

%%% ============================================================================
%%% Implementation

%% @spec preparation(State::refst()) -> refst()
%%
%% @doc Calculates certain undefined fields of the `State' and check the
%% preconditions of the refactoring.
%% The following fields of the `State' are set:
%% `spectype', `oldfunname', `arity', `newfunname', `funnode'.
%% The following fields are not sure to be set:
%% `filename', `pos', `modname'.
preparation(St = #refst{spectype = SpecType, newfunname = NewFunNameOrig}) ->
    NewFunName =
        case is_atom(NewFunNameOrig) of
            true -> NewFunNameOrig;
            false -> list_to_atom(NewFunNameOrig)
        end,
    case SpecType of
        pos ->
            #refst{filename = FileName, pos = Pos} = St,
            FunNode = get_fun_by_token(
                        ?LEX:get_token(?SYNTAX:get_file(FileName), Pos)),
            #func{name = OldFunName, arity = Arity} = ?ESG:data(FunNode);
        mfa ->
            #refst{modname = ModName,
                   oldfunname = OldFunName,
                   arity = Arity} = St,
            FunNode = ?SEMINF:get_fun_by_mfa(ModName, OldFunName, Arity)
    end,
    [Mod] = ?ESG:path(FunNode, [{func, back}]),
    ?SEMINF:check_mod_does_not_have_fun(Mod, NewFunName, Arity),
    ?SEMINF:check_mod_does_not_import_fun(Mod, NewFunName, Arity),
    ?SEMINF:check_not_autoimported(NewFunName, Arity),
    St#refst{oldfunname = OldFunName,
             arity = Arity,
             funnode = FunNode,
             newfunname = NewFunName}.

%%% ============================================================================
%%% Transformation

%% @spec calc_update(node(), refst()) -> [{node(), data()}]
%%
%% @doc Calculates the updates that should be performed on the nodes, starting
%% from `Node'.
calc_update(Node, Args) ->
    calc_update(Node, ?ESG:data(Node), Args).

%% @spec calc_update(node(), Data::data(), refst()) -> [{node(), data()}]
%%
%% @doc Calculates the updates that should be performed on the nodes, starting
%% from `Node'.
%% This function is separated from {@link calc_update/2} only so that the code
%% will be nicer this way.
%% This function should not be called directly, only through
%% {@link calc_update/2}. `Data' is the data that belongs to `Node'.
%% `calc_update(Node, Args)' is the same as
%% `calc_update(Node, ?ESG:data(Node), Args)'.
calc_update(Node, Data=#func{type = global, name = OldFunName, arity = Arity},
            Args = #refst{oldfunname = OldFunName,
                          arity = Arity,
                          newfunname = NewFunName}) ->
    NodeUpdate = {Node, Data#func{name=NewFunName}},
    FunNameNodes = ?ESG:path(Node, [{fundef, back}, funcl, name]),
    UserFunNodes = ?ESG:path(Node, [{funref, back}]),
    OtherUpdates = [ Update ||
                       ExprNode <- FunNameNodes ++ UserFunNodes,
                       Update <- calc_update(ExprNode, Args) ],
    [NodeUpdate|OtherUpdates];
calc_update(Node, #expr{type = expr, kind = application}, Args) ->
    [FunNameNode|_] = ?ESG:path(Node, [sub]),
    calc_update(FunNameNode, Args);
calc_update(Node, #expr{type = expr, kind = infix_expr, value = '/'}, Args) ->
    [FunNameNode, _ArityNode] = ?ESG:path(Node, [sub]),
    calc_update(FunNameNode, Args);
calc_update(Node, #expr{type = expr, kind = infix_expr, value = ':'}, Args) ->
    [_ModNode, FunNameNode] = ?ESG:path(Node, [sub]),
    calc_update(FunNameNode, Args);
calc_update(Node, #expr{type = expr, kind = implicit_fun}, Args) ->
    case ?ESG:path(Node, [sub]) of
        [FunNameNode, _ArityNode] ->
            calc_update(FunNameNode, Args);
        [_ModNode, FunNameNode, _ArityNode] ->
            calc_update(FunNameNode, Args)
    end;
calc_update(Node,
            Data = #expr{type = expr, kind = atom, value = OldFunName},
            Args = #refst{oldfunname = OldFunName,
                          newfunname = NewFunName}) ->
    NodeNewData = Data#expr{value = NewFunName},
    [LexNode] = ?ESG:path(Node, [elex]),
    [{Node, NodeNewData} | calc_update(LexNode, Args)];
calc_update(Node,
            Lex = #lex{type = token,
                       data = Data = #token{type = atom, value = OldFunName}},
            _Args = #refst{oldfunname = OldFunName,
                           newfunname = NewFunName}) ->
    NewFunNameString = ?MISC:format("~p",[NewFunName]),
    LexNodeNewData =
        Lex#lex{data = Data#token{value = NewFunName,
                                  text = NewFunNameString}},
    [{Node, LexNodeNewData}].

%%% ----------------------------------------------------------------------------
%%% General functions
%%% TODO should be placed in a general module

%% @spec get_fun_by_token(node()) -> node()
%%
%% @doc Returns the semantic node of the function that is represented by the
%% token under the cursor. The token may be the function name in the function's
%% definition, but it may be a function call.
get_fun_by_token(Token) ->
    Path1 = [{elex, back}, {name, back}, {funcl, back}, fundef],
    Path2 = [{elex, back}, sup, funref],
    case try_paths([{Token, Path1}, {Token, Path2}]) of
        {_, [FunNode]} -> FunNode;
        _ -> throw("The selected position does not indicate a function!")
    end.

%% @spec try_path(node(), path()) -> [node()]
%%
%% @doc Calls the `path' function with the given arguments.
%% Returns an empty list in case of a bad path.
try_path(Node, Path) ->
    try
        ?ESG:path(Node, Path)
    catch
        error:{bad_path, _} -> []
    end.

%% @spec try_paths([{node(), path()}]) -> {{node(), path()}, [node()]}
%%
%% @doc Calls the `path' function with the `{Node, Path}' combinations.
%% Stops at the first pair, where the result is not empty.
%% That result and the matching pair will be the result of the function.
try_paths([]) ->
    [];
try_paths([{Node, Path}|Tail]) ->
    case try_path(Node, Path) of
        [] -> try_paths(Tail);
        Result -> {{Node, Path}, Result}
    end.
