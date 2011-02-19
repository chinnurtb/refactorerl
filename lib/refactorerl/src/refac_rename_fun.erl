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

%%% @doc This module implements the rename function refactoring.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(refac_rename_fun).
-vsn("$Rev: 1335 $").

-export([rename_function/4]).

-include("refactorerl.hrl").

%% @type rf() = #rf{mod_name = atom(),
%%                  old_fun_name = atom(),
%%                  arity = integer(),
%%                  new_fun_name = atom()}.
%% 
%% It represents the information about the renaming request.
%% This type is an argument for {@link calc_update/2} and {@link calc_update/3}.
-record(rf, {mod_name, old_fun_name, arity, new_fun_name}).

%% @spec rename_function(atom(), atom(), integer(), atom()) -> ok
%%
%% @doc Renames the `OldFunName' function in the `ModName' module to
%% `NewFunName'.
rename_function(ModName, OldFunName, Arity, NewFunName) ->
    Root = ?ESG:root(),

    %% our module
    [ ModNode ] = ?ESG:path(Root, [file, {moddef, {name, '==', ModName}}]),

    %% our function's semantic node
    [ FunNode ] = 
        ?ESG:path(ModNode, anal_function:function(OldFunName, Arity)),

    %% calculating the updates
    Updates = calc_update(FunNode, #rf{mod_name = ModName, 
                                       old_fun_name = OldFunName,
                                       arity = Arity,
                                       new_fun_name = NewFunName}),

    %% applying the updates
    lists:foreach(
      fun({Node, NewData}) ->
              ?ESG:update(Node, NewData)
      end,
      Updates),

    ?ESG:close(),
    [ refac_fileman:save_file(X) || X <- ?ESG:path(Root, [file]) ],
    ok.

%% @spec calc_update(node(), rf()) -> [{node(),data()}]
%%
%% @doc Calculates the updates that should be performed on the nodes, starting
%% from `Node'.
calc_update(Node, Args) ->
    calc_update(Node, ?ESG:data(Node), Args).

%% @spec calc_update(node(), Data::data(), rf()) -> [{node(),data()}]
%%
%% @doc Calculates the updates that should be performed on the nodes, starting
%% from `Node'.
%% This function is separated from {@link calc_update/2} only so that the code
%% will be nicer this way. 
%% This function should not be called directly, only through {@link calc_update/2}.
%% `Data' is the data that belongs to `Node'.
%% `calc_update(Node, Args)' is the same as
%% `calc_update(Node, ?ESG:data(Node), Args)'.
calc_update(Node, #func{type = global, name = OldFunName, arity = Arity},
            Args = #rf{old_fun_name = OldFunName, 
                       arity = Arity,
                       new_fun_name = NewFunName}) ->
    NodeUpdate = {Node, {func, global, NewFunName, Arity}},
    [ FunDefNode ] = ?ESG:path(Node, [{fundef, back}, funcl, name]),
    UserFunNodes = ?ESG:path(Node, [{funref, back}]),
    OtherUpdates = [ Update || 
                       ExprNode <- [FunDefNode|UserFunNodes],
                       Update <- calc_update(ExprNode, Args) ],
    [NodeUpdate|OtherUpdates];
calc_update(Node, #expr{type = expr, kind = application}, Args) ->
    [ ExprNode | _ ] = ?ESG:path(Node, [sub]),
    calc_update(ExprNode, Args);
calc_update(Node, #expr{type = expr, kind = arity_qualifier}, Args) ->
    [ ExprNode, _ArityNode ] = ?ESG:path(Node, [sub]),
    calc_update(ExprNode, Args);
calc_update(Node, #expr{type = expr, kind = module_qualifier}, Args) ->
    [ _ModNode, FunNode ] = ?ESG:path(Node, [sub]),
    calc_update(FunNode, Args);
calc_update(Node,
            Data = #expr{type = expr, kind = atom, value = OldFunName},
            Args = #rf{old_fun_name = OldFunName,
                       new_fun_name = NewFunName}) ->
    NodeNewData = Data#expr{value = NewFunName},
    [ LexNode ] = ?ESG:path(Node, [efirst]),
    [ {Node, NodeNewData} | calc_update(LexNode, Args)];
calc_update(Node,
            Lex = #lex{type = token, 
                       data = Data = #token{type = atom, value = OldFunName}},
            _Args = #rf{old_fun_name = OldFunName,
                        new_fun_name = NewFunName}) ->
    LexNodeNewData =
        Lex#lex{data = Data#token{value = NewFunName,
                                  text = atom_to_list(NewFunName)}},
    [ {Node, LexNodeNewData} ].
