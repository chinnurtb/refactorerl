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

%%% @doc This module implements the rename module refactoring.
%%%
%%% @author István Bozó <bozo_i@inf.elte.hu>

-module(referl_tr_rename_mod).
-vsn("$Rev: 1933 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/3]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

-record(refst,{filename,
               newname,
               pos,
               filenode,
               modobj,
               modname,
               modexprnode,
               newfilepath,
               modrefnodes
              }).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface

%% @spec do(string(), string(), integer()) -> ok
%%
%% @doc Renames the file `FileName' and the module to `NewName' if on the 
%% selected position `Pos' is a module attribute.

do(FileName, NewName, Pos) ->
    ?TRANSFORM:do(?MODULE,{FileName, NewName, Pos}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FileName, NewName, Pos})->
    #refst{filename = FileName, newname = list_to_atom(NewName), pos = Pos}.

%% @private
steps() ->
    [
     fun check_new_name_is_legal/1,
     fun file_node/1,
     fun check_is_module_attr/1,
     fun check_module_exits_in_the_graph/1,
     fun module_data/1,
     fun new_file_path/1,
     fun check_does_file_exist/1,
     fun get_nodes_for_update/1
    ].

%% @private
transform(#refst{filenode = FileNode, modexprnode = ModExprNode,
                 modrefnodes = ModRefNodes, newname = NewName,
                 newfilepath = NewFilePath, filename = FileName,
                 modobj = ModObj}) ->
    Files =
        lists:usort(
          [ begin
                update_expr_node(Node, NewName),
                get_file_node(Node)
                end
            || Node <- (ModRefNodes ++ [ModExprNode])]),
    update_file_node(FileNode, NewFilePath),
    update_module_obj(ModObj, NewName),
    file:rename(FileName, NewFilePath),
    ?ESG:close(),
    {[FileNode] ++ (Files -- [FileNode]), ok}.

%%% ============================================================================
%%% Implementation

get_file_node(Node) ->
    [FileNode] =
        ?ESG:path(Node, [sup, {visib,back}, scope, functx,
                         {funcl, back}, {form, back}]) ++
        ?ESG:path(Node, [{attr, back}, {form, back}]),
    FileNode.


file_node(St = #refst{filename = FileName}) ->
    case ?SYNTAX:file(FileName) of
        {file, FileNode} ->
            St#refst{filenode = FileNode};
        _  ->
            throw("Module is not present in the database!")
    end.

module_data(St = #refst{filenode = FileNode}) ->
    [ModuleExpr] =
        ?ESG:path(FileNode,[{form,{tag, '==', module}}, attr]),
    #expr{value = Name} =
        ?ESG:data(ModuleExpr),
    [ModuleObj] =
        ?ESG:path(FileNode,[moddef]),
    St#refst{modname = Name, modexprnode = ModuleExpr, modobj = ModuleObj}.

new_file_path(St = #refst{newname = NewName, filename = FileName}) ->
    Dir =
        filename:dirname(FileName),
    NewFilePath =
        filename:join([Dir, atom_to_list(NewName) ++ ".erl"]),
    St#refst{newfilepath = NewFilePath}.

get_nodes_for_update(St = #refst{modobj = ModObj}) ->
    RefNodes =
        ?ESG:path(ModObj,[{modref, back}]),
    St#refst{modrefnodes = RefNodes}.

%%% ============================================================================
%%% Transformation

update_expr_node(Node, NewName)->
    Data = ?GRAPH:data(Node),
    [Token] = ?GRAPH:path(Node,[elex]),
    Lex = ?GRAPH:data(Token),
    #lex{data = TokenData = #token{value = _OldName}} = Lex,
    NewToken =
        Lex#lex{data =
                TokenData#token{value = NewName,
                                text = atom_to_list(NewName)}},
    ?ESG:update(Node, Data#expr{value = NewName}),
    ?ESG:update(Token, NewToken).

update_file_node(FileNode, NewFilePath) ->
    Data = ?GRAPH:data(FileNode),
    ?ESG:update(FileNode, Data#file{path = NewFilePath}).

update_module_obj(ModObj, NewName)->
    ?ESG:update(ModObj, #module{name = NewName}).


%%% ============================================================================
%%% Checks

check_new_name_is_legal(#refst{newname = NewName}) ->
    case ?LEX:is_valid_name(function, atom_to_list(NewName)) of
        true ->
            ok;
        _ ->
            throw(?MISC:format("The given name ~n is not a legal module name!",
                               [NewName]))
    end.

check_module_exits_in_the_graph(#refst{newname = NewName}) ->
    Module =
        ?ESG:path(?ESG:root(), [{module, {name, '==', NewName}}]),
    case Module of
        [] ->
            ok;
        _ -> throw("There is a module with the same name in the database!")
    end.

check_does_file_exist(#refst{newfilepath = NewFilePath}) ->
    case filelib:is_file(NewFilePath) of
        true ->
            throw("There is a file with the same name in the current" ++ 
                  " directory!");
        _ ->
            ok
    end.

check_is_module_attr(#refst{pos = Pos, filenode = FileNode})->
    Token = ?LEX:token_by_pos(FileNode, Pos),
    case Token of
        illegal -> 
            throw(?MISC:format("The selected position ~p does not indicate" ++ 
                               " a module attribute!",[Pos]));
        _       -> ok
    end,
    {_, TokenNode} = Token,
    Module =
        ?ESG:path(TokenNode,[{{flex, back},{tag,'==',module}}]) ++
        ?ESG:path(TokenNode,[{elex, back},{{attr, back},{tag,'==',module}}]),
    if Module == [] ->
            throw(?MISC:format("The selected position ~p does not indicate" ++
                               " a module attribute!",[Pos]));
       true ->
            ok
    end.
