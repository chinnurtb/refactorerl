%%% Copyright Notice © 2007 Eötvös Loránd University and Ericsson Hungary
%%% Software development supported by Ericsson Hungary and
%%% GVOP-3.2.2-2004-07-0005/3.0 ELTE IKKK.

%%% Materials  were  created, authored,  and/or  prepared  by  the Authors  at
%%% Department   of  Programming  Languages   and  Compilers,   Eötvös  Loránd
%%% University,  Budapest,  Hungary  (ELTE)  for Ericsson  Hungary  under  the
%%% agreement  between  Ericsson  Hungary  and  ELTE  IKKK.  Unless  otherwise
%%% specifically stated, no claim to copyright is being asserted and it may be
%%% freely  used as  in the  public domain  in accordance  with  Erlang Public
%%% License.  All rights,  including copyright,  are owned  or  controlled for
%%% these purposes by  the Ericsson Hungary and ELTE.  Copyright exists in all
%%% other original  material published on the  internet and may  belong to the
%%% authors depending on the circumstances of publication.

%%% --------------------------------------------------------------------------
%%% ``The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1,  (the  "License"); you  may  not  use  this file  except  in
%%% compliance with the License. You should have received a copy of the Erlang
%%% Public License along  with this software. If not, it  can be retrieved via
%%% the world wide web at http://www.erlang.org/.

%%% Software distributed under the License is distributed on an "AS IS" basis,
%%% WITHOUT WARRANTY OF  ANY KIND, either express or  implied. See the License
%%% for  the specific  language  governing rights  and  limitations under  the
%%% License.

%%% The Initial  Developer of  the Original Code  is Ericsson  Utvecklings AB.
%%% Portions created by Ericsson  are Copyright 1999, Ericsson Utvecklings AB.
%%% All Rights Reserved.''
%%% --------------------------------------------------------------------------

%%% The Contributors are the Authors listed below. All Rights Reserved.

%%% You may not alter or remove any trademark, copyright or other notice from
%%% copies of the content.

%%% Authors: Zoltán Csörnyei
%%%          Zoltán Horváth
%%%          Roland Király
%%%          Róbert Kitlei
%%%          Tamás Kozsik
%%%          László Lövei
%%%          Tamás Nagy
%%%          Melinda Tóth
%%%          Anikó Víg

%%% Author contact: erlang@plc.inf.elte.hu
%%% --------------------------------------------------------------------------

%% @copyright 2007 Eötvös Loránd University and Ericsson Hungary
%% @author Tamas Nagy <lestat@elte.hu>
%% @author Aniko Vig <viganiko@inf.elte.hu>

%% @doc 
%% This module implements the rename variable refactoring.
%% 
%% @end


-module(refac_ren_var).

-export([rename_variable/4]).

%% =====================================================================
%% @spec rename_variable(File::string(), Line::integer(), 
%%                       Col::integer(), NewName::integer()) -> none()
%%
%% @doc
%% Performs the precondition checks and the and the collects all the 
%% necessary information for the refactoring.
%% It throws various terms when there is an error, with this syntax:
%% {atom(), term()}. If the atom is ok, the refactoring has been 
%% preformed without a problem.
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>Line</b> : The pointed line number in the editor.
%% <b>Col</b> : The pointed column number in the editor.
%% <b>NewName</b> : The new name given for the variable. </pre>
%% @end
%% =====================================================================
rename_variable(File, Line, Col, NewName) ->    
    MId = refac_common:get_module_id(File),
    Id = refac_common:get_id_from_pos(MId, Line, Col, variable),
    {BId, ScopeId} = refactor:get_var_bind_occ_and_scope_from_id(MId, Id),
    Type = refac_common:get_scope_type(MId, ScopeId),
    refac_checks:check_isVariableName(NewName),
    refac_checks:check_expression(MId, NewName, BId, ScopeId, Type),

    perform_refactoring(MId, NewName, BId),
    {ok, NewName}.

%% =====================================================================
%% @spec perform_refactoring(MId::integer(), NewName::string(), 
%%                       BId::integer()) -> ok
%%
%% @doc
%% Performs the refactoring, and commits the changes into the 
%% refactoring system.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>NewName</b> : The new name given for the variable.
%% <b>BId</b> : The variable's binding occurrence's id. </pre>
%% @end
%% =====================================================================
perform_refactoring(MId, NewName, BId) ->
    refactor:update_variable_name(MId, BId, NewName),
    refactor:commit().
