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

%%% @doc Expand implicit fun expression
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(referl_tr_expand_funexpr).
-vsn("$Rev: 1971 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/2, is_implicit_fun_expr/1]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

%% State record
-record(state, {filepath, file, pos, node}).

%%% ============================================================================
%%% Errors

implicit_not_found() ->
    throw("No implicit fun expression found.").

%%% ============================================================================
%%% Interface

%% @spec do(node(), integer()) -> ok
%% @doc Expands the implicit fun expression in the specified file on the
%% specified position, when it exists.
do(FilePath, Pos) ->
    ?TRANSFORM:do(?MODULE, {FilePath, Pos}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FilePath, Pos}) ->
    #state{filepath = FilePath, pos=Pos}.

%% @private
steps() ->
    [fun prepare/1].

%% @private
transform(#state{file=File, node=Node}) ->
    ?MANIP:expand_funexpr(Node),
    ?ESG:close(),
    {[File], ok}.


%%% ============================================================================
%%% Implementation

prepare(St = #state{filepath=FilePath, pos=Pos}) ->
    File = ?SYNTAX:get_file(FilePath),
    Token = ?LEX:get_token(File, Pos),
    case get_fun_expr(Token) of 
        not_found ->
            implicit_not_found();
        Node ->
            St#state{file=File, node = Node}
    end.

%% @private
get_fun_expr(Entity) ->
    case ?ESG:data(Entity) of
        #expr{kind = implicit_fun} ->
            Entity;
        #file{} ->
            not_found;
        _ ->
            [{_, P}|_] = ?ESG:parent(Entity),
            get_fun_expr(P)
    end.

%% @private
is_implicit_fun_expr(Expr) ->
    ExprKindOk =
        case ?GRAPH:data(Expr) of
            #expr{kind=implicit_fun} ->
                true;
            _ ->
                false
        end,
    SubExprKindOk =
        case ?GRAPH:path(Expr, [sub]) of
            [] ->
                false;
            [SubExpr|_] ->
                case ?GRAPH:data(SubExpr) of
                    #expr{kind=infix_expr, value='/'} ->
                        true;
                    _ ->
                        false
                end
        end,
    ExprKindOk andalso SubExprKindOk.

