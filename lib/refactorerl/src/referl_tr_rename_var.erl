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

%%% @doc This module implements the rename variable refactoring.  It
%%% renames the selected variable when there are no name clashes with
%%% the new name.
%%%
%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>

-module(referl_tr_rename_var).
-vsn("$Rev: 1965 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/3]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

-record(refst, {filename, newname, pos,
                file, var, refs}).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface

%% @spec do(string(), string(), integer()) -> ok
%%
%% @doc Rename the variable on the selected position `Pos' and its occurences.
do(FileName, NewName, Pos) ->
     ?TRANSFORM:do(?MODULE,{FileName, NewName, Pos}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FileName, NewName, Pos}) ->
    #refst{ filename = FileName,
            newname = NewName,
            pos = Pos }.

%% @private
steps() ->
    [?MISC:check_varname(#refst.newname),
     ?MISC:get_file_node(#refst.filename, #refst.file),
     fun var_node/1,
     fun check_name_clash/1,
     fun var_refs/1].

%% @private
transform(#refst{refs = Refs, newname = NewName, file = File}) ->
    Update = fun (N = #variable{}) ->
                     N#variable{name = NewName};
                 (N = #expr{}) ->
                     N#expr{value = NewName};
                 (N = #lex{data = Data}) ->
                     N#lex{data = Data#token{value = NewName, text = NewName}}
             end,
    [?ESG:update(Node, Update(?ESG:data(Node))) || Node <- Refs],
    {[File],ok}.

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

var_refs(St = #refst{var = Var}) ->
    Exprs = ?ESG:path(Var, [{varref,  back}]) ++
            ?ESG:path(Var, [{varbind, back}]),
    Tokens = [Token || Expr <- Exprs, Token <- ?ESG:path(Expr, [elex])],
    St#refst{refs = [Var] ++ Exprs ++ Tokens}.

%%% ----------------------------------------------------------------------------
%%% Checks

check_name_clash(#refst{var = Var, newname = NewName}) ->
    Refs = ?ESG:path(Var, [{varref, back}]) ++ 
           ?ESG:path(Var, [{varbind, back}]),
    lists:foreach(fun (R) -> check_name_clash(NewName, R) end, Refs).

check_name_clash(Name, Ref) ->
    VarNames = [(?ESG:data(Var))#variable.name || Var <- visible_vars(Ref)],
    ?MISC:error_on_difference(lists:member(Name, VarNames), false,
                              "there is already a variable "
                              "with the same name").

visible_vars(Expr) ->
    lists:usort(?ESG:path(Expr, [sup, {visib, back}, scope, varvis]) ++
                ?ESG:path(Expr, [sup, {visib, back}, scope, vardef])).
