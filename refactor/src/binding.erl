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
%% @author Robert Kitlei <kitlei@elte.hu>

%% @doc 
%% This module implements the recovering of binding occurrence candidates.
%% 
%% @end
-module(binding).

-export([get_binding_occurrence_candidates/3]).

-include("node_type.hrl").

-vsn('0.1').

-define(CONJUNCTIVE, true).
-define(DISJUNCTIVE, true).

%% =====================================================================
%% @spec get_binding_occurrence_candidates(MId::integer(),
%%                                         VarName::string(),
%%                                         NodeId::integer())
%%                                           -> [integer()]
%%
%% @doc
%% Returns the list of binding occurrence candidate ids for a variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarName</b> : The name of the variable.
%% <b>NodeId</b> : The root of the search; has to be
%%                       of type ?CLAUSE. Is usually the scope
%%                       of VarName.</pre>
%% @end
%% =====================================================================
get_binding_occurrence_candidates(MId, VarName, NodeId) ->
  NodeType = erl_syntax_db:type(MId, NodeId),
  IsConjunctiveNode = lists:member(NodeType, [?IF_EXPR, ?INFIX_EXPR]),

  if
    NodeType == ?VARIABLE ->
      NodeName = refactor:get_name_from_name_id(MId, NodeId),
      if
        VarName == NodeName ->
          {true, [NodeId]};
        true ->
          {false, []}
      end;
    NodeType == ?CASE_EXPR ->
      Argument = erl_syntax_db:case_expr_argument(MId, NodeId),
      Clauses = lists:flatten(erl_syntax_db:case_expr_clauses(MId, NodeId)),
      {FoundOpen1, _FoundClosed1, BoundIds1} =
        get_binding_occurrences_(MId, VarName, [Argument], ?CONJUNCTIVE),
      {_FoundOpen2, FoundClosed2, BoundIds2} =
        get_binding_occurrences_(MId, VarName, Clauses, ?DISJUNCTIVE),
      if
        not FoundOpen1 ->
          {true, BoundIds1};
        true ->
          {FoundClosed2, BoundIds1 ++ BoundIds2}
      end;
    true ->
      AllSubtrees = lists:flatten(erl_syntax_db:subtrees(MId, NodeId)),
      Subtrees =
      if
        NodeType == ?INFIX_EXPR ->
          [Op1,_,Op2] = AllSubtrees,
	  [Op1,Op2];
	true ->
          AllSubtrees
      end,
      {FoundOpen, FoundClosed, BoundIds} =
        get_binding_occurrences_(MId, VarName, Subtrees, IsConjunctiveNode),
      {(IsConjunctiveNode andalso not FoundOpen) or
       (not IsConjunctiveNode andalso FoundClosed), BoundIds}
  end.

%% =====================================================================
%% @spec get_binding_occurrences_(MId::integer(),
%%                                VarName::string(),
%%                                Nodes::[integer()],
%%                                IsConjunctiveNode::bool()) ->
%%                                           [integer()]
%%
%% @doc
%% Returns the list of binding occurrence candidate ids for a variable.
%% 
%% Parameter description:<pre>
%% <b>MId</b> : Id of the module.
%% <b>VarName</b> : The name of the variable.
%% <b>Nodes</b> : The nodes the check concerns.
%% <b>IsConjunctiveNode</b> : Is the node conjunctive?</pre>
%% @end
%% =====================================================================
get_binding_occurrences_(MId, VarName, Nodes, IsConjunctiveNode) ->
  lists:foldl(
    fun(Node, {FoundOpen, FoundClosed, BoundIds}) ->
      {SubIsClosed, SubBoundIds} = get_binding_occurrence_candidates(MId, VarName, Node),
      if
        not IsConjunctiveNode andalso FoundClosed ->
          NewBoundIds = BoundIds;
        true ->
          NewBoundIds = BoundIds ++ SubBoundIds
        end,
        {FoundOpen orelse not SubIsClosed, FoundClosed orelse SubIsClosed, NewBoundIds}
    end, {false, false, []}, Nodes).
