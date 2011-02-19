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

%%% @doc Common syntax query module.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(refac_query).
-vsn("$Rev: 1335 $").

-include("refactorerl.hrl").
-include("refac_schema.hrl").


%%  Top-down queries.
-export([ file/1, module/2 ]).

%%  Bottom-up queries.
-export([ token_by_pos/2, path_to_file/1, top_expr/2, link_index/3 ]).


%%% ---------------------------------------------------------------------------
%%% Query functions for move function and move macro
%%% ---------------------------------------------------------------------------

-export([is_exported/3,
     is_imported/4,
     is_included/2,
     is_header_file/1]).
-export([function_forms_and_nodes/2,
     function_exists/3,
     applications/1,
     referer_importlist/2]).
-export([included_from_file/3,
     included_from_path/3]).
-export([has_module_qualifier/1]).
-export([existing_recordnames/1,
     existing_macronames/1,
     includable/2,
     used_records/1,
     used_macros/1]).
-export([macros_by_macro/1,
     records_by_macro/1,
     macro_users/1]).
-export([first_token/1,
     first_token/2]).


%% @spec file(string()) -> not_found | {file, node()}
%%
%% @doc Returns the graph node of the file `Filename' if it is stored in the
%% graph.
file(Filename) ->
    case ?GRAPH:path(?GRAPH:root(), [{file, {path, '==', Filename}}]) of
        [File]  ->   {file, File};
        []      ->   not_found;
        _       ->   erlang:error({multiple_file, Filename})
    end.


%% @spec module(node(), string()) -> {module, node()} | not_found
%% @doc Returns the graph node of the module of the file `Filename'
%%      if it is stored in the graph.
module(File, FileName)->
  case ?GRAPH:path(File, [moddef]) of
    []        ->  not_found;
    [Module]  ->  {module, Module};
    _         ->  erlang:error({multiple_module, FileName})
  end.



%% @spec token_by_pos(File :: node(), Pos :: integer())
%%            -> { TokenNode :: node(), Location :: atom(),
%%                 PosInText :: integer(), Char :: char()}
%%             | {remains, integer()}
%%             | error_position_beyond_eof
%%
%%       Location = beforeToken | in_token | afterToken
%%
%% @doc  Returns information about the token that is
%%       at the specified location in the file.
token_by_pos(File, Pos) ->
    FirstTNs = ?GRAPH:path(File, [form, ffirst]),
    lists:foldl(
        fun (FirstTN, {remains, RPos}) -> token(FirstTN, RPos);
            (_, Result)                -> Result
        end, {remains, Pos}, FirstTNs).



%% @spec token(TN :: node(), Pos :: integer())
%%         ->  {TN :: node(), Location :: atom(),
%%              PosInText :: integer(), PointedChar :: char()}
%%          |  {remains, integer()}
%%
%%       Location = beforeToken | in_token | afterToken
%%
%% @doc
%% Returns information about the token
%% at the specified location in the file.
token(TN, Pos) ->
    #lex{data=#token{text=Text, prews=Pre, postws=Post}} = ?GRAPH:data(TN),
    V     = Text,
    LPre  = spec_length(Pre),
    LV    = spec_length(V),
    LPost = spec_length(Post),
    if
        Pos =< LPre andalso LPre /= 0 ->
            {TN, before_token, Pos, lists:nth(Pos, Pre)};
        Pos =< LPre + LV andalso LV /= 0 ->
            Pos2 = Pos - LPre,
            {TN, in_token, Pos, lists:nth(Pos2, V)};
        Pos =< LPre + LV + LPost andalso LPost /= 0 ->
            Pos2 = Pos - LPre - LV,
            {TN, after_token, Pos, lists:nth(Pos2, Post)};
        Pos > LPre + LV + LPost ->
            Remains = Pos - LPre - LV - LPost,
            case ?GRAPH:path(TN, [next]) of
                [NextTN] -> token(NextTN, Remains);
                []       -> {remains, Remains}
            end
    end.


%% @doc Returns the length of the string.
%%      MSDOS line ends are counted as one character.
spec_length([])                -> 0;
spec_length([$\r, $\n | Rest]) -> 1 + spec_length(Rest);
spec_length([$\n, $\r | Rest]) -> 1 + spec_length(Rest);
spec_length([_ | Rest])        -> 1 + spec_length(Rest).


%% @spec path_to_file(Node :: node()) -> [node()] | no_path_found
%% @doc  Returns the list of nodes from the file node
%%       leading to the specified node.
path_to_file(Node) ->
    path_to_file(Node, []).
path_to_file(Node, Path) ->
    case parent(Node) of
        none ->
            [Node | Path];
        Parent ->
            path_to_file(Parent, [Node | Path])
    end.


% @spec parent(Node :: node()) -> node() | none
% @doc  Returns the parent of a node in the graph.
parent(Node) ->
    case element(1, ?GRAPH:data(Node)) of
      root -> [];
      NodeTag ->
        PN = [?GRAPH:path(Node, [{Tag, back}]) || Tag <- parent_tags(NodeTag)],
        case lists:append(PN) -- [Node] of
          []    -> none;
%          [P|_] -> P
          Parents ->
            [Parent] = syntax_nodes(Parents -- lists:map(fun parent/1, Parents)),
            Parent
        end
    end.


syntax_nodes(Nodes) ->
  Schema = ?SYNTAX_SCHEMA,
  Links1 = [ Links || {_, _, Links} <- Schema ],
  Links2 = [ Links || {_, Links}    <- Schema ],
  SynLinks = [To || {_, To} <- lists:append(Links1 ++ Links2)],
  GoodLinks = [{'$gn', Link, N}
                   || {'$gn', Link, N} <- Nodes, lists:member(Link, SynLinks)],
  lists:usort(GoodLinks).



%% @spec parent_tags(Tag :: tag()) -> [tag()]
%% @doc  Returns the parent tags possible for a given tag.
%%       We stop upon reaching a file node.
parent_tags(file) -> [];
parent_tags(Tag) ->
  Schema = ?LEXICAL_SCHEMA ++ ?SYNTAX_SCHEMA,
  Connections1 = [ Connections || {_, _, Connections} <- Schema ],
  Connections2 = [ Connections || {_, Connections}    <- Schema ],
  [From || {From, To} <- lists:append(Connections1 ++ Connections2)
         , To == Tag
         , From /= next].



%% @spec top_expr(Ts1 :: [node()], Ts2 :: [node()])
%%         -> {one_token, [node()]}
%%          | { TopExpr :: node(), PathToTopExpr :: [node()],
%%              Rest1 :: [node()], Rest2 :: [node()]}
%%
%% @doc
%% Returns the list of nodes from the file node
%% leading down to the specified node.
top_expr(Ts, Ts)   -> {one_token, Ts};
top_expr(Ts1, Ts2) -> top_expr(Ts1, Ts2, [], []).

top_expr([T1a|Ts],[T1b|Ts2],Path,Prev) when T1a == T1b ->
  top_expr(Ts,Ts2, Path ++ [Prev], T1a);
top_expr(Ts1,Ts2,Path,Prev) ->
  {Prev, tl(Path), Ts1, Ts2}.


%% @spec to_list(V) -> string()
%% @doc  Converts an atom, integer or string to string.
to_list(V) when is_atom(V)     ->  atom_to_list(V);
to_list(V) when is_integer(V)  ->  integer_to_list(V);
to_list(V)                     ->  V.


%% @spec link_index(node(), tag(), node()) -> integer()
%% @doc  Returns the index of the node among the links.
link_index(Parent, Link, Node) ->
  Links = ?GRAPH:path(Parent, [Link]),
  IndexedLinks = lists:zip(lists:seq(1, length(Links)), Links),
  [Index] = [ I || {I, ILink} <- IndexedLinks, ILink == Node],
  Index.


%%% ===========================================================================
%%% ===========================================================================


%% @private
get_mod(Entity) ->
    case ?GRAPH:data(Entity) of
    #file{} ->
        case ?GRAPH:path(Entity, anal_module:file_module()) of
        [Mod] -> Mod;
        _ -> not_found
        end;
    #module{} ->
        Entity;
    _ ->
        throw("Illegal entity type.")
    end.


%% ----------------------------------------------------------------------------
%% @doc Returns true, if the Name-Arity pair is exported
%%      in the file or module, otherwise false.
%% ----------------------------------------------------------------------------
is_exported(Entity, Name, Arity) ->
    case get_mod(Entity) of
    not_found -> false;
    Module ->
        case ?GRAPH:path(Module, anal_function:function(Name, Arity)) of
        [Fun] ->
            lists:member(Fun, ?GRAPH:path(Module, [funexp]));
        _ ->
            false
        end
    end.


%% ----------------------------------------------------------------------------
%% @doc Returns true, if the Name-Arity pair is imported
%%      from module named ModuleName in the file or module 'Entity',
%%      otherwise false.
%% ----------------------------------------------------------------------------
is_imported(Entity, ModuleName, Name, Arity) ->
    case get_mod(Entity) of
    not_found -> false;
    Module ->
        [SourceMod] = ?GRAPH:path(?GRAPH:root(), anal_module:module(ModuleName)),
        case ?GRAPH:path(SourceMod, anal_function:function(Name, Arity)) of
        [Fun] ->
            lists:member(Fun, ?GRAPH:path(Module, [funimp]));
        _ ->
            false
        end
    end.


%% ----------------------------------------------------------------------------
%% @doc Returns true, if a file with specified path, or node is included
%%      in File.
%% ----------------------------------------------------------------------------
is_included(File, Data) ->
    if is_list(Data) ->
        ([] =/= ?GRAPH:path(File, [{incl, {path, '==', Data}}]));
       true ->
        lists:member(Data, ?GRAPH:path(File, [incl]))
    end.


%% ----------------------------------------------------------------------------
%% @spec includable(node(), node()) -> bool()
%% @doc Returns true, if there's no conflict between the record
%%      and macro entities of the two file.
%% ----------------------------------------------------------------------------
includable(Target, Incl) ->
    M1 = existing_macros_with_source(Target),
    M2 = existing_macros_with_source(Incl),
    R1 = existing_records_with_source(Target),
    R2 = existing_records_with_source(Incl),
    {MN1, _} = lists:unzip(M1--M2),
    {MN2, _} = lists:unzip(M2--M1),
    {RN1, _} = lists:unzip(R1--R2),
    {RN2, _} = lists:unzip(R2--R1),
    intersect(MN1, MN2) == [] andalso intersect(RN1, RN2) == [].


%% ----------------------------------------------------------------------------
%% @spec function_forms_and_nodes(node(), [{atom(), integer()}]) ->
%%               {[node()], [node()]}
%% @doc - First parameter is module node
%%      - Second is list of {name, arity} pairs.
%%      Returns list of function forms and semancical functions,
%%      which are in the specified module and have the specified
%%      names and arities.
%% ----------------------------------------------------------------------------
function_forms_and_nodes(Module, FnList) ->
    FnNodes =
    lists:map(
      fun({Name, Arity}) ->
          case ?GRAPH:path(
              Module,
              anal_function:function(Name, Arity)) of
              [] -> throw("Function form not found");
              [Form] -> Form
          end
      end,
      FnList),
    FnForms =
    lists:map(
      fun(Form) ->
          case ?GRAPH:path(Form, [{fundef, back}]) of
              [] -> throw("Semantical function not found");
              [FunNode] -> FunNode
          end
      end,
      FnNodes),
    {FnForms, FnNodes}.


is_header_file(File) ->
    ?GRAPH:path(File, [moddef]) == [].


%% ----------------------------------------------------------------------------
%% @spec included_from_path(node(), string(), atom()) -> string()
%% @doc Returns the path of the include file, which is included in 'File'
%%      and contains the entity definition (macro or record).
%% ----------------------------------------------------------------------------
included_from_path(File, Name, Tag) ->
    (?GRAPH:data(included_from_file(File, Name, Tag)))#file.path.


%% ----------------------------------------------------------------------------
%% @spec included_from_file(node(), string(), atom()) -> node()
%% @doc Returns the node of the include file, which is included in 'File'
%%      and contains the entity definition (macro or record).
%% ----------------------------------------------------------------------------
included_from_file(File, Name, Tag) ->
    hd(?GRAPH:path(File, [incl, {Tag, {name, '==', Name}}, {Tag, back}])).


%% ----------------------------------------------------------------------------
%% @spec function_exists(node(), string(), integer()) -> bool()
%% @doc Returns true, if function with the specified name
%%      and arity in the module already exists.
%% ----------------------------------------------------------------------------
function_exists(ModuleNode, Name, Arity)->
    case ?GRAPH:path(ModuleNode, anal_function:function(Name, Arity)) of
        [] -> false;
        _  -> true
    end.


%% ----------------------------------------------------------------------------
%% @spec has_module_qualifier(node()) -> false | node()
%% @doc Returns the module node if the 'Expr' is a module-qualified function
%%      application, otherwise false.
%% ----------------------------------------------------------------------------
has_module_qualifier(Expr) ->
    case ?GRAPH:data(Expr) of
    #expr{kind=application} ->
        case [?GRAPH:data(A) || A <- ?GRAPH:path(Expr, [sub])] of
        [#expr{kind=module_qualifier}|_] ->
            hd(?GRAPH:path(hd(?GRAPH:path(Expr, [sub])), [modref]));
        _ ->
            false
        end;
    #expr{kind=fun_expr} ->
        false;
    _ ->
        false
    end.


applications(FnForms) ->
    lists:filter(
      fun(E) ->
          case ?GRAPH:data(E) of
          #expr{kind=application} -> true;
          #expr{kind=fun_expr} ->    true;
          _ ->                       false
          end
      end,
      lists:flatten(
    [?GRAPH:path(F, [funcl, {scope,back}, visib, {sup, back}])
     || F <- FnForms])).


referer_importlist(File, FunNode) ->
    FormRefs = ?GRAPH:path(FunNode, [{funref, back}, sup, {{attr, back}, {type, '==', attrib}}]),
    lists:flatten(
      lists:map(
    fun(Form) ->
        case ?GRAPH:data(Form) of
            #form{type = attrib, tag = import} ->
            case ?GRAPH:path(Form, [{form, back}]) of
                [File] ->
                case length(?GRAPH:path(Form, [attr])) of
                    1 ->
                    [Module] = ?GRAPH:path(Form, [{attr, 1}, modref]),
                    {1, Form, Module};
                    2 ->
                    [ImportList] = ?GRAPH:path(Form, [{attr, 2}]),
                    [ImpExpr] =
                        lists:filter(
                          fun(Expr) ->
                              ?GRAPH:path(Expr, [funref]) == [FunNode] end,
                          ?GRAPH:path(ImportList, [sub])),
                    [Module] = ?GRAPH:path(Form, [{attr, 1}, modref]),
                    {2, ImportList, ImpExpr, Module};
                    _ ->
                    []
                end;
                _ ->
                []
            end;
            _ ->
            []
        end
    end,
    FormRefs)).


%%% ===========================================================================
%%%                     RECORD AND MACRO INFORMATIONS
%%% ===========================================================================


%% ----------------------------------------------------------------------------
%% @spec existing_macronames(node()) -> {[string()], [string()], [string()]}
%% @doc Tuple of list of macro names, that are defined in the file
%% and its includes.
%% {Local, Incl, All}; All == Local ++ Incl
%% ----------------------------------------------------------------------------
existing_macronames(File) ->
    Fun = fun(Def) -> #macro{name=Name} = ?GRAPH:data(Def), Name end,
    {L, I, A} = existing_names(File, macro, Fun),
    {L -- ["MODULE"], I -- ["MODULE"], A -- ["MODULE"]}.


%% ----------------------------------------------------------------------------
%% @spec existing_recordnames(node()) -> {[string()], [string()], [string()]}
%% @doc Tuple of list of record names, that are defined in the file
%% and its includes.
%% {Local, Incl, All}; All == Local ++ Incl
%% ----------------------------------------------------------------------------
existing_recordnames(File) ->
    Fun = fun(Def) -> #record{name=Name} = ?GRAPH:data(Def), Name end,
    existing_names(File, record, Fun).


%% ----------------------------------------------------------------------------
%% @private
%% @spec existing_names(node(), atom(), function()) ->
%%                                {[string()], [string()], [string()]}
%% @doc Tuple of list of record/macro names, that are defined in the file
%% and its includes.
%% The 'Type' parameter determines that what kind of entity is searched.
%% ----------------------------------------------------------------------------
existing_names(File, Type, Fun) ->
    All = lists:map(Fun, ?GRAPH:path(File, [incl, Type])),
    Local = lists:map(Fun, ?GRAPH:path(File, [Type])),
    Incl = All -- Local,
    {Local, Incl, All}.


existing_macros_with_source(File) ->
    Fun = fun(Def) -> #macro{name=Name} = ?GRAPH:data(Def), Name end,
    existing_names_with_source(File, macro, Fun).

existing_records_with_source(File) ->
    Fun = fun(Def) -> #record{name=Name} = ?GRAPH:data(Def), Name end,
    existing_names_with_source(File, record, Fun).

existing_names_with_source(File, Type, Fun) ->
    lists:usort(lists:flatten([{E, File} || E <- lists:map(Fun, ?GRAPH:path(File, [Type]))] ++
    [existing_names_with_source(F, Type, Fun)
     || F <- (?GRAPH:path(File, [incl]) -- [File])])).


%%% ===========================================================================


%% ----------------------------------------------------------------------------
%% @spec used_records([node()]) -> {[node()], [string()]}
%% @doc Returns the list of record nodes and names,
%% that are used in the functions.
%% ----------------------------------------------------------------------------
used_records(Funs) ->
    Records =
    lists:usort(lists:umerge(
              [?GRAPH:path(Fun,
                   [funcl, {scope,back}, visib, {sup, back}, recref])
               || Fun <- Funs])),
    {Records, [Name || R <- Records, #record{name=Name} <- [?GRAPH:data(R)]]}.


%% ----------------------------------------------------------------------------
%% @spec used_macros([node()]) -> {[node()], [string()]}
%% @doc Returns the list of macro nodes and names,
%% that are used in the functions.
%% ----------------------------------------------------------------------------
used_macros(Funs) ->
    Substs =
    lists:filter(
      fun(Lex) ->
          [] =/= ?GRAPH:path(Lex, [mref])
      end,
      lists:usort(lists:umerge(
            [?GRAPH:path(Fn, [funcl, {scope,back}, clex])
             || Fn <- Funs]
            ++
            [?GRAPH:path(Fn, [funcl, {scope,back}, visib,
                      {sup, back}, elex])
             || Fn <- Funs]))),
    Used = lists:usort(lists:flatten([macros_by_macro_recur(Subst, {lfirst, back}) || Subst <- Substs])),
    Users = lists:usort(lists:flatten([macros_by_macro_recur(Subst, lfirst) || Subst <- Substs])),
    AllSubsts = lists:usort(Substs ++ Used ++ Users),
    Macros = [hd(?GRAPH:path(S, [mref])) || S <- AllSubsts],
    {Macros, [Name || M <- Macros, #macro{name=Name} <- [?GRAPH:data(M)]]}.


%%% ===========================================================================


%% ----------------------------------------------------------------------------
%% @spec macros_by_macro(node()) -> [node()]
%% @doc Returns all macro nodes, that are used by the specified 'Macro', recursive.
%% ----------------------------------------------------------------------------
macros_by_macro(Macro) ->
    case ?GRAPH:path(Macro, [{mref, back}]) of
    Substs when is_list(Substs) andalso Substs =/= [] ->
        All = lists:usort(lists:flatten([macros_by_macro_recur(Subst, {lfirst, back}) || Subst <- Substs])),
        lists:usort([hd(?GRAPH:path(S, [mref])) || S <- All]);
    _ ->
        []
    end.


%% @private
macros_by_macro_recur(Subst, Link) ->
    News = lists:filter(
         fun(E) -> case ?GRAPH:data(E) of
               #lex{type = subst} -> true;
               _                  -> false
               end end,
         ?GRAPH:path(Subst, [Link])),
    case News of
    [] -> [];
    _ ->  lists:flatten(News ++ [macros_by_macro_recur(M, Link) || M <- News])
    end.


%% ----------------------------------------------------------------------------
%% @spec records_by_macro(node()) -> [node()]
%% @doc Returns all record nodes, that are used by 'Macro', recursive.
%% ----------------------------------------------------------------------------
records_by_macro(Macro) ->
    Ms = [Macro | macros_by_macro(Macro)],
    Ss = lists:flatten([?GRAPH:path(M, [{mref, back}]) || M <- Ms]),
    lists:usort(lists:flatten([?GRAPH:path(S, [{elex, back}, recref]) || S <- Ss])).


%% ----------------------------------------------------------------------------
%% @spec macro_users(node()) -> [node()]
%% @doc Returns all macro nodes, that use the specified 'Macro', recursive.
%% ----------------------------------------------------------------------------
macro_users(Macro) ->
    case ?GRAPH:path(Macro, [{mref, back}]) of
    Substs when is_list(Substs) andalso Substs =/= [] ->
        All = lists:usort(lists:flatten([macros_by_macro_recur(Subst, lfirst) || Subst <- Substs])),
        lists:usort([hd(?GRAPH:path(S, [mref])) || S <- All]);
    _ ->
        []
    end.


first_token(Expr) ->
    first_token(Expr, efirst).
first_token(Entity, Link) ->
    [Lex] = ?GRAPH:path(Entity, [Link]),
    case ?GRAPH:data(Lex) of
    #lex{type = subst} -> first_token(Lex, lfirst);
    #lex{type = incl}  -> throw(not_supported);
    #lex{type = param} -> throw(not_supported);
    #lex{type = token} -> Lex;
    _ ->                  throw(not_supported)
    end.


intersect(L1, L2) ->
    L1 -- (L1 -- L2).
