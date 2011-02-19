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

%%% @doc The rename header refactoring renames the header file to the given
%%%      new name, and makes changes in those files in which it is referred to.
%%%      If the new name of the header file contains a path and this path is not
%%%      equal to the original one, the transformation moves the header file to
%%%      its new place and renames it.
%%%
%%% == Parameters ==
%%% <ul>
%%%  <li> A module (see {@link referl_args:filename/1}). The header file to 
%%%       be modified. Currently it can be specified with a position anywhere
%%%       in the include file.</li>
%%%  <li> A module (see {@link referl_args:file/1}). The new name of the 
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

-module(referl_tr_rename_header).
-vsn(" $Rev: 3027 $ ").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([transform/2,
         transform0/3,
         transform1/2,
         prepare/1]).

%%% ============================================================================
%%% Callbacks

prepare(Args)->
    NewName = ?Args:filename(Args),
    %%NewName1 = ?File:abs_path(NewName),
    FileNode = ?Args:file(Args),
    OldPath = ?File:path(FileNode),
    ?Check(?File:type(FileNode) == header,
                ?RefError(file_not_hrl,[OldPath])),

    
    Path = filename:dirname(OldPath),
    NewPath = filename:join([Path, NewName]),

    ?Check(lists:member("..",filename:split(NewName)) /= true, 
                                 ?RefError(rel_path, [NewName])),

    ?Check(not filelib:is_file(NewPath), 
                     ?RefError(file_exists,[NewPath])),

    InclFiles = ?Query:exec(FileNode, ?File:included())--[FileNode],

    [fun()-> transform0(FileNode, NewPath, OldPath) end]
    ++
    [fun(ok)-> transform(WInclFile, FileNode) end 
                                         || WInclFile <- InclFiles]
    ++
    [fun(ok)-> transform1(WInclFile, FileNode) end 
                                         || WInclFile <- InclFiles].

%%% ============================================================================
%%% Implementation

transform(ActInclFile, FileNode)->
   ?File:del_include(ActInclFile, FileNode),
   ?Transform:touch(ActInclFile),
   ok.
transform0(FileNode, NewPath, OldPath)->
   ?Transform:rename(OldPath, NewPath),
   ?File:upd_path(FileNode, NewPath),
   ?Transform:touch(FileNode),
   referl_fileman:save_file(FileNode),
   ok.

transform1(ActInclFile, FileNode)->
   ?File:add_include(ActInclFile, FileNode),
   ?Transform:touch(ActInclFile).
