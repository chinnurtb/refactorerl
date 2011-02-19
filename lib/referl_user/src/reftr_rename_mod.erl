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
%%% <li>A module (see {@link reflib_args:module/1}).</li>
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
%%%   <li>Rename the Refs to the module in the import lists.</li>
%%%   <li>Rename the file to the new name.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(reftr_rename_mod).
-vsn("$Rev: 5496 $ ").

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args)->
    ModuleObj = ?Args:module(Args),
    [FileNode] = ?Query:exec(ModuleObj, ?Mod:file()),

    OldPath = ?File:path(FileNode),

    ArgsInfo = add_transformation_info(Args, ModuleObj, OldPath),
    {NewName, NewPath} = ?Args:ask(ArgsInfo, name, fun cc_newname/2, fun cc_error/3, OldPath),

    [ModForm] = ?File:module_form(FileNode),

    Refs = [Node || Node <- ?Query:exec(ModuleObj, ?Mod:references()),
                    not is_module_mac(Node)],

%    ?Macro:check_single_usage(Refs, [{elex, 1}]),

    DynUpdates = reflib_dynfun:collect({rename, mod}, ModuleObj, NewName),

    ?Transform:touch(FileNode),
    [fun()->
        ?Macro:inline_single_virtuals(Refs, elex),

        ?File:upd_path(FileNode, NewPath),
        [?Macro:update_macro(Node, {elex, 1}, atom_to_list(NewName))
            || Node <- Refs],

        Data = ?ESG:data(ModForm),
        ?ESG:update(ModForm, Data#form{tag = NewName}),

        ?Transform:rename(OldPath, NewPath),
        reflib_dynfun:transform(DynUpdates)
    end,
    fun(_)->
        [ModuleObj] % @todo where is it updated...?
    end].

add_transformation_info(Args, Module, Path) ->
    ModName = ?Mod:name(Module),
    Info    = ?MISC:format("Renaming module ~p (Path: ~p)",
                           [ModName, Path]),
    [{transformation_text, Info} | Args].

%%% ============================================================================
%%% Implementation

new_file_path(NewName, OldPath) ->
    Dir = filename:dirname(OldPath),
    filename:join([Dir, atom_to_list(NewName) ++ ".erl"]).

is_module_mac(Node) ->
    case ?Query:exec(Node, [elex, llex]) of
        []    -> false;
        Subst -> ?Query:exec(Subst, [mref]) == []
    end.


%%% ============================================================================
%%% Checks

is_quoted(Name) ->
    hd(io_lib:write(Name)) == $'.

cc_newname(NewModName, OldPath) ->
    ?Check(?Query:exec(?Mod:find(NewModName)) == [],
           ?RefError(module_exists,[NewModName])),
    ?Check(not is_quoted(NewModName), ?RefErr0r(quoted_atom)),
    NewFilePath = new_file_path(NewModName, OldPath),
    ?Check(not filelib:is_file(NewFilePath),
           ?RefError(file_exists,[NewFilePath])),

    {NewModName, NewFilePath}.

cc_error(?RefError(module_exists,[NewModName]), NewModName, _OldPath) ->
   ?MISC:format("The module name ~p is already used.", [NewModName]);
cc_error(?RefError(file_exists,[NewFilePath]), _NewModName, _OldPath) ->
   ?MISC:format("The file ~p is already used.", [NewFilePath]).
