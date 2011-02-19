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

-module(graph_printer).

-export([print/4]).

print(FileName, MId, RootId, SpecialNodes) ->
  {ok, Fd} = file:open(FileName ++ ".dot", write),

  Text1 = print_(MId, RootId, SpecialNodes),
  Text2 = get_types(MId, RootId),

  file:write(Fd, io_lib:format("digraph refactor_graph {~n~s~n~s}~n", [Text1, Text2])),
  file:close(Fd),
  os:cmd("dot " ++ FileName  ++ ".dot -Tpng -o " ++ FileName  ++ ".png").

print_(MId, RootId, []) ->
  print_nodes(MId, RootId);
print_(MId, RootId, [{Nodes, Style}|MoreNodes]) ->
  print_(MId, RootId, MoreNodes) ++
  lists:foldl(fun(Node, Text) ->
                Text ++
                integer_to_list(Node) ++ " [" ++ Style ++ "];\n"
              end, "", Nodes).

print_nodes(MId, Node) ->
  Children = lists:flatten(erl_syntax_db:subtrees(MId, Node)),
  lists:foldl(fun(Child, Text) ->
                Text ++
                integer_to_list(Node) ++ " -> " ++ integer_to_list(Child) ++ ";\n" ++
                print_nodes(MId, Child)
              end, "", Children).

get_types(MId, RootId) ->
  Children = refac_common:get_subtrees(MId, RootId, with_root),
  Types = [application, arity_qualifier, atom, attribute, binary, binary_field, block_expr, case_expr, catch_expr, char, class_qualifier, clause, comment, cond_expr, conjunction, disjunction, eof_marker, float, form_list, fun_expr, function, generator, if_expr, implicit_fun, infix_expr, integer, list, list_comp, macro, match_expr, module_qualifier, nil, operator, parentheses, prefix_expr, qualified_name, query_expr, receive_expr, record_access, record_expr, record_field, record_index_expr, rule, size_qualifier, string, text, try_expr, tuple, underscore, variable],
  lists:foldl(fun(Child, Text) ->
                Type = erl_syntax_db:type(MId, Child),
		AtomicName = lists:nth(Type, Types),
                Text ++
                integer_to_list(Child) ++ " [shape=record," ++
		                            "label=\"{" ++ integer_to_list(Child) ++ "|" ++
                                                           atom_to_list(AtomicName) ++ "}\"]" ++
		                         ";\n"
              end, "", Children).
