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

%%% @doc The rename header refactoring renames the header file to the given
%%%      new name, and makes changes in those files in which it is referred to.
%%%      If the new name of the header file contains a path and this path is not
%%%      equal to the original one, the transformation moves the header file to
%%%      its new place and renames it.
%%%
%%% == Parameters ==
%%% <ul>
%%%  <li> A module (see {@link reflib_args:filename/1}). The header file to
%%%       be modified. Currently it can be specified with a position anywhere
%%%       in the include file.</li>
%%%  <li> A module (see {@link reflib_args:file/1}). The new name of the
%%%       header file.</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>The type of the file has to be a header file. If the pointed file is
%%%    a module, the transformation will fail.</li>
%%%   <li>The directory must not contain a file having the same name as new
%%%        name given. </li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ul>
%%%   <li>Rename the header file name to the new name on the graph.</li>
%%%   <li>Rename the references to the header file in the include forms.
%%%       (Actually, the include form will be deleted and recreated with a new
%%%       path and filename).</li>
%%%   <li>Rename or move and rename the file to the new name.</li>
%%% </ul>
%%%
%%% == Implementation status ==
%%%
%%% This refactoring has been implemented.
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(reftr_rename_header).
-vsn(" $Rev: 5496 $ ").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks
%%% @private
prepare(Args)->
    FileNode = ?Args:file(Args),

    OldPath = ?File:path(FileNode),
    Path    = filename:dirname(OldPath),
    ?Check(?File:type(FileNode) == header, ?RefError(file_not_hrl,[OldPath])),

    ArgsInfo = add_transformation_info(Args, FileNode),
    NewPath  = ?Args:ask(ArgsInfo, filename, fun cc_newpath/2, fun cc_error/3, Path),

    InclFiles = ?Query:exec(FileNode, ?File:included())--[FileNode],

    [?Transform:touch(F) || F <- [FileNode | InclFiles]],

    [fun()->
       ?Transform:rename(OldPath, NewPath),
       ?File:upd_path(FileNode, NewPath),
       ?FileMan:save_file(FileNode)
     end]
    ++
    [fun(ok)-> ?File:del_include(WInclFile, FileNode) end
                                         || WInclFile <- InclFiles]
    ++
    [fun(ok)-> ?File:add_include(WInclFile, FileNode) end
                                         || WInclFile <- InclFiles]
    ++
    [fun(ok)-> [FileNode] end].

add_transformation_info(Args, File) ->
    Path = ?File:path(File),
    Info = ?MISC:format("Renaming header: ~p", [Path]),
    [{transformation_text, Info} | Args].

%%% ============================================================================
%%% Checks

cc_newpath(NewName, Path) ->
    ?Check(not lists:member("..", filename:split(NewName)),
           ?RefError(rel_path, [NewName])),
    NewHrl = filename:join(Path, NewName),
    ?Check(not filelib:is_file(NewHrl), ?RefError(used_header, NewHrl)),
    NewHrl.

cc_error(?RefError(used_header, NewHrl), _NewName, _Path) ->
   ?MISC:format("The header file ~p is already used.", [NewHrl]).
