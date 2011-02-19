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

%%% @doc Create clusters
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(kcl_group).
-vsn("$Rev:  $").

-include("kcl.hrl").


%%% ============================================================================
%%% Imports/exports

%Imports
-import(proplists, [get_value/2, get_value/3]).


% Partition entities
-export([create_clusters/3]).



%%% ============================================================================
%%% Partition entities

%% @ spec create_open_clusters(OpenModNames::[atom()], State::clState()) ->
%%           NewState::clState()
%% @ doc Create open clusters from all single items that `modname' property of
%%      the item is in the `OpenModNames' list. All other item will be groped
%%      into common clusters by the `modename' value.
% create_clusters(Mode, ModNames, St=#clState{etc=EtcETS}) when 
        % is_atom(Mode), is_list(ModNames) ->
    % % Collect entities by module name
    % {CloseDict, OpenLst} = lists:foldl(
        % fun(#clItem{id=Id, props=Props}, {AccDict, AccLst}) ->
            % ModNames1 = case proplists:is_defined(modname, Props) of
                % true -> [get_value(modname, Props)];
                % _ -> lists:usort(lists:map(fun ?ClAttr:get_node_modname/1, 
                        % ?Query:exec(get_value(file,Props), ?ClAttr:fileMods()))
            % end,
            % case (Mode==close andalso []==ModNames1--ModNames) orelse
                % (Mode==open andalso []/=?MISC:intersect(ModNames1, ModNames)) of
                % true -> {AccDict, [Id|AccLst]};
                % _    -> {dict:append(hd(ModNames1), Id, AccDict), AccLst}
            % end
        % end,
        % {dict:new(), []},
        % ?ClState:entItems(St)),
    % % Get identifier for the next new cluster
    % NextClId0 = case ets:lookup(EtcETS, next_cl_id) of
        % [N] -> N;
        % []  -> 1
    % end,
    % % Create clusters from closed modules
    % {St1, NextClId1} = dict:fold(
        % fun(ModName, EntIds, {AccSt, AccClId}) ->
            % Cl = #clCluster{id=AccClId, mode=close, name=ModName},
            % {?ClState:make_cluster(EntIds, Cl, AccSt), AccClId+1}
        % end,
        % {St, NextClId0},
        % CloseDict),
    % % Create clusters from entities in open modules
    % {St2, NextClId2} = lists:foldl(
        % fun(EntId, {AccSt, AccClId}) ->
            % Cl = #clCluster{id=AccClId, mode=open, 
                % name=?MISC:format("cl~p", [AccClId])},
            % {?ClState:make_cluster([EntId], Cl, AccSt), AccClId+1}
        % end,
        % {St1, NextClId1},
        % OpenLst),
    % % Update state
    % ets:insert(EtcETS, {next_cl_id, NextClId2}),
    % St2.


%%
%%
create_clusters(Mode, FilePaths, St=#clState{etc=EtcETS}) when 
        is_atom(Mode), is_list(FilePaths) ->
    % Collect entities by module name
    {CloseDict, OpenLst} = lists:foldl(
        fun(#clItem{id=Id, props=Props}, {AccDict, AccLst}) ->
            FilePath = get_value(filepath, Props),
            InSet    = lists:member(FilePath, FilePaths),
            case (Mode==open  andalso InSet) orelse 
                 (Mode==close andalso (not InSet)) of
                true -> {AccDict, [Id|AccLst]};
                _    -> {dict:append(FilePath, Id, AccDict), AccLst}
            end
        end,
        {dict:new(), []},
        ?ClState:entItems(St)),
    % Get identifier for the next new cluster
    NextClId0 = case ets:lookup(EtcETS, next_cl_id) of
        [N] -> N;
        []  -> 1
    end,
    % Create clusters from closed modules
    {St1, NextClId1} = dict:fold(
        fun(FilePath, EntIds, {AccSt, AccClId}) ->
            Cl = #clCluster{id=AccClId, mode=close, name=FilePath},
            {?ClState:make_cluster(EntIds, Cl, AccSt), AccClId+1}
        end,
        {St, NextClId0},
        CloseDict),
    % Create clusters from entities in open modules
    {St2, NextClId2} = lists:foldl(
        fun(EntId, {AccSt, AccClId}) ->
            Cl = #clCluster{id=AccClId, mode=open, 
                name=?MISC:format("cl~p", [AccClId])},
            {?ClState:make_cluster([EntId], Cl, AccSt), AccClId+1}
        end,
        {St1, NextClId1},
        OpenLst),
    % Update state
    ets:insert(EtcETS, {next_cl_id, NextClId2}),
    St2.





