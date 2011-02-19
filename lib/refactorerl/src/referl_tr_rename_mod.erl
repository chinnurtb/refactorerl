%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc This module implements the rename module refactoring. The rename module
%%% refactoring renames a module to the given new name. Renames the file and
%%% make changes in other file where this module is referenced.
%%%
%%% == Parameters ==
%%% <ul>
%%% <li>A module (see {@link referl_args:module/1}).</li>
%%% </ul> 
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>The given new name should be a legal file name.</li>
%%%   <li>There must not exist another module with the given new name in the
%%%   graph.</li>
%%%   <li>There must not exist another file with the same name as the given new
%%%    name in the directory of the module to be renamed.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>Rename the current module name to the new name.</li>
%%%   <li>Rename the related module qualifiers to the given new name.</li>
%%%   <li>Rename the references to the module in the import lists.</li>
%%%   <li>Rename the file to the new name.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%% 
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(referl_tr_rename_mod).
-vsn("$Rev: 3185 $").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1]).

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args)->
    NewName = ?Args:name(Args),
    ?Check(hd(io_lib:write(NewName)) /= $', %if quoted -> refuse
           ?RefErr0r(quoted_atom) ),
    FileNode = ?Args:file(Args),
    ModuleObj = ?Args:module(Args),
    ?Check(?Query:exec(?Mod:find(NewName)) == [],
           ?RefError(module_exists,[NewName])),
    [ModuleExpr] =
        ?Query:exec(FileNode, ?Query:seq(?File:form(1), ?Form:exprs())),
    OldPath = ?File:path(FileNode),
    NewPath = new_file_path(NewName, OldPath),
    ?Check(not filelib:is_file(NewPath), ?RefError(file_exists,[NewPath])),
    References = ?Query:exec(ModuleObj, ?Mod:references()),
    fun()->
            transform(FileNode, ModuleExpr, References, NewName, NewPath,
                      OldPath, ModuleObj)
    end.


%%% ============================================================================
%%% Implementation

transform(FileNode, ModExprNode, References, NewName, NewPath, OldPath,
          ModObj) ->
    ?File:upd_path(FileNode, NewPath),
    ?Transform:touch(FileNode),
    [ begin
          ?Syn:replace(Node, {elex, 1}, [atom_to_list(NewName)]),
          ?Transform:touch(Node)
      end
      || Node <- (References ++ [ModExprNode])],
    %% TODO: Remove it!!!
    ObjData = ?Graph:data(ModObj),
    ?Graph:update(ModObj, ObjData#module{name = NewName}),
    %%
    ?Transform:rename(OldPath, NewPath).

new_file_path(NewName, OldPath) ->
    Dir =
        filename:dirname(OldPath),
    filename:join([Dir, atom_to_list(NewName) ++ ".erl"]).
