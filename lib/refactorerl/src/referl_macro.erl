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

%%% @doc This module implements functions that are related to
%%% macros. Macros are represented with the form that defines them.

-module(referl_macro).
-vsn("$Rev: 2390 $").
-include("refactorerl.hrl").

%% =============================================================================
%% Exports

%% Properties
-export([name/1]).

%% Queries
-export([file/0, macros/0, records/0, references/0]).


%% =============================================================================
%% Properties

%% @spec name(node(#form{})) -> string()
%% @doc Returns the name of the macro.
name(Macro) ->
    (?Graph:data(Macro))#form.tag.


%% =============================================================================
%% Queries

%% @spec file() -> query(#form{}, #file{})
%% @doc The result query returns the file that defines the macro.
file() ->
    ?Form:file().

%% @spec macros() -> query(#form{}, #form{})
%% @doc The result query returns all macros used in the body of the macro.
macros() ->
    fun(Macro) ->
            referred_macros(Macro, up)
    end.

%% @spec records() -> query(#form{}, #record{})
%% @doc The result query returns all records used in the macro.
records() ->
    fun(Macro) ->
            AllMacros = [Macro | ?Query:exec(Macro, macros())],
            Substs = lists:flatten([?Query:exec(M, [{mref, back}])
                                    || M <- AllMacros]),
            ?MISC:flatsort([?Query:exec(Subst, [{llex, back},
                                                {elex, back}, recref])
                            || Subst <- Substs])
    end.

%% @spec references() -> query(#form{}, #form{})
%% @doc The result query returns all macros that are using the argument macro.
references() ->
    fun(Macro) ->
            referred_macros(Macro, down)
    end.

referred_macros(Macro, Direction) ->
    case ?Query:exec(Macro, [{mref, back}]) of
        Substs when is_list(Substs) andalso Substs =/= [] ->
            SubstsWithRecur = ?MISC:flatsort([substs_recur(Subst, Direction)
                                              || Subst <- Substs]),
            lists:usort([?Query:exec1(Subst, [mref], subst_without_mref)
                         || Subst <- SubstsWithRecur]);
        _ ->
            []
    end.

%% @private
%% @spec substs_recur(node(), up | down) -> [node()]
%% @doc Returns all macro substitutions from a specified
%% substitution, up or down in the syntax tree.
substs_recur(Subst, Direction) ->
    Query = case Direction of
                up -> [{llex, back}, {llex, back}];
                down -> [llex, llex]
            end,
    News = [New || New <- ?Query:exec(Subst, Query),
                   #lex{type=subst} <- [?Graph:data(New)]],
    case News of
        [] -> [];
        _ ->  lists:flatten(News ++ [substs_recur(M, Direction) || M <- News])
    end.
