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

%%% @doc Lexical information query module.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(referl_lex).
-vsn("$Rev: 1994 $").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

-export([token_by_pos/2, get_token/2, pos_to_linecol/2, file_length/1,
         pos_of_token/2, fold_tokens/3, token_length/1, lex_link/1, reflink/1,
         token_link/1, token_link_tag/1, token_parent/1,
         is_included/2, is_header_file/1,
         included_from_file/3, included_from_path/3]).
-export([existing_recordnames/1,existing_macronames/1,
         existing_records_with_source/1,
         macros_by_macro/1, records_by_macro/1, macro_users/1,
         used_records/1, used_macros/1,
         existing_names/2, used_nodes_names/2]).
-export([prews/1, postws/1, lex_info/1, lex_text/1,
         first_token/1, last_token/1, token_data/1]).
-export([exists/3, movable_to_header/4, containing_files/1,
         includable/2]).

%% String checks
-export([is_valid_name/2]).

%%% ============================================================================
%%% Contents

%% @spec token_by_pos(node(), integer()) -> {ok, node()} | illegal
%%
%% @doc Returns the token at the specified position in the file.
%% @see token_length/1
token_by_pos(_File, Pos) when Pos =< 0 -> illegal;
token_by_pos(File, Pos) ->
    fold_tokens(
      fun (Node, _D, Start, End, _) when Start =< Pos,
                                         Pos < End ->
              {stop, {ok, Node}};
          (_,_,_,_,Acc) ->
              {next, Acc}
      end, illegal, File).

%% @spec get_token(node(), natural()) -> node()
%%
%% @doc Returns the token in `File' at `Pos' position.
get_token(File, Pos) ->
    case  ?LEX:token_by_pos(File, Pos) of
        {ok, Token} -> Token;
        _ -> throw("There isn't any lexical element in the selected position")
    end.

%% @spec file_length(File::node()) -> integer()
%%
%% @doc Returns the length of the file, i.e. the number of valid indices.
file_length(File) ->
    fold_tokens(fun(_,_,_,End,_) -> {next, End} end, 1, File) - 1.

%% @spec pos_of_token(node(), node()) -> {natural(), natural()} | not_found
%%
%% @doc Returns the position of the first and the last character of the `Token'
%% token, which must be in the file represented by `File'.
%% The `Token' has to be in the given file, otherwise it will not be found.
pos_of_token(File, Token) ->
    fold_tokens(
      fun (Node, _, Start, End, _) when Node =:= Token -> {stop, {Start, End}};
          (_,_,_,_,Acc) -> {next,Acc}
      end, not_found, File).

%% @spec fold_tokens(Fun, Acc0, File::node()) -> AccLast
%%
%%       Fun = (Token::node(), Data::#token{}, Start, End, Acc) ->
%%          {stop, AccLast} | {next, AccNext}
%%
%% @doc Similar to `lists:foldl/3' on the tokens of the file, except that this
%% can stop processing. The token node, token data, and the starting and ending
%% positions of the token are passed to the function.
fold_tokens(Fun, Acc0, File) ->
    Tokens = ?SYNTAX:leaves(File),
    fold_tokens(Fun, Acc0, Tokens, 1).

fold_tokens(_, Acc, [], _) ->
    Acc;
fold_tokens(Fun, Acc, [Head|Tail], Pos) ->
    #lex{data=Data} = ?ESG:data(Head),
    End = Pos + token_length(Data),
    case Fun(Head, Data, Pos, End, Acc) of
        {stop, Result} -> Result;
        {next, Acc1}   -> fold_tokens(Fun, Acc1, Tail, End)
    end.


%% @spec token_length(#lex{}|#token{}) -> integer()
%% @doc Returns the string-length of the token. Used in token_by_pos/2.
token_length(#lex{data=#token{}=Data}) ->
    token_length(Data);
token_length(#token{text=Text, prews=Pre, postws=Post}) ->
    strlen(Pre) + strlen(Text) + strlen(Post).

strlen(Str) -> strlen(Str, 0).

strlen([],                Len) -> Len;
strlen([$\r, $\n | Tail], Len) -> strlen(Tail, Len+1);
strlen([_        | Tail], Len) -> strlen(Tail, Len+1).


%% @spec lex_link(node()) -> atom()
%% @doc The link between the node type and its direct tokens.
lex_link(Node)              -> lex_link_by_data(?GRAPH:data(Node)).

lex_link_by_data(#expr{})   -> elex;
lex_link_by_data(#clause{}) -> clex;
lex_link_by_data(#form{})   -> flex.


%% @spec reflink(macro | record) -> mref | recref
%% @doc The reference link label of the entity.
reflink(macro)  -> mref;
reflink(record) -> recref.


%% @spec token_link(node()) -> flex | clex | elex
%% @doc Returns the token link associated to the node.
token_link(Node) -> token_link_tag(element(1, ?GRAPH:data(Node))).

%% @spec token_link_tag(form | clause | expr) -> flex | clex | elex
%% @doc Returns the token link associated to the node type.
token_link_tag(form)   -> flex;
token_link_tag(clause) -> clex;
token_link_tag(expr)   -> elex.


%% @spec token_parent(node()) -> [node()]
%% @doc Returns the syntactical parents of a node.
token_parent(Token) ->
    [T || {_,T} <- case ?ESG:path(Token, [{orig, back}]) of
                       [] -> ?ESG:parent(Token);
                       Subst -> lists:flatmap(fun ?ESG:parent/1, Subst)
                   end].


%%% ============================================================================
%%% Converting position given by index to line+column

%% @spec pos_to_linecol(node(), natural()) -> {positive(), positive()}
%%
%% @doc Converts the `Pos' index position of `File' to a line-column position.
pos_to_linecol(File, Pos) ->
    fold_tokens(
      fun (_, Token, Start, End, LineCol) when Start =< Pos, Pos < End ->
              {stop, add_count(Pos - Start, LineCol, get_token_text(Token))};
          (_, Token, _, _, LineCol) ->
              {next, add_to_linecol(LineCol, get_token_text(Token))}
      end, {1, 1}, File).

%% @spec add_count(natural(), {positive(), positive()}, string()) ->
%%           {positive(), positive()}
%%
%% @doc Adds the `Count' first character of `Text' to `LineCol'.
%% `CR LF' is counted as one character.
add_count(0, LineCol, _) ->
    LineCol;
add_count(Count, {L, C}, [Head|Tail]) ->
    {NewLC, NewTail} =
        case Head of
            $\r ->
                {{L + 1, 1}, case Tail of
                                 [$\n|Tail2] -> Tail2;
                                 _ -> Tail
                             end};
            $\n ->
                {{L + 1, 1}, Tail};
            _ ->
                {{L, C + 1}, Tail}
        end,
    add_count(Count - 1, NewLC, NewTail).

%% @spec get_token_text(token()) -> string()
%%
%% @doc Returns the whole text of the token.
get_token_text(#token{text=Text, prews=PreWS, postws=PostWS}) ->
    PreWS ++ Text ++ PostWS.

%% @spec add_to_linecol({positive(), positive()}, string()) ->
%%           {positive(), positive()}
%%
%% @doc If `Text' starts at the `{L, C}' position, the function returns the
%% first position after `Text'.
add_to_linecol({L, C}, Text) ->
    case count_newline(Text) of
        0 -> {L, C + postchar(Text)};
        N -> {L + N, 1 + postchar(Text)}
    end.

%% @spec count_newline([term()]) -> natural()
%%
%% @doc Returns how many times is newline contained by `L'.
count_newline(List) ->
    {Count, _} =
        lists:foldl(
          fun ($\r, {Count, _}) -> {Count + 1, last_cr};
              ($\n, {Count, last_not_cr}) -> {Count + 1, last_not_cr};
              ($\n, {Count, last_cr}) -> {Count, last_not_cr};
              (_, {Count, _}) -> {Count, last_not_cr}
          end, {0, last_not_cr}, List),
    Count.

%% @spec postchar(string()) -> natural()
%%
%% @doc Returns the number of characters after the last newline character in
%% `Text'.
postchar(Text) ->
    ?MISC:partfold(
       fun ($\n, Count) -> {stop, Count};
           ($\r, Count) -> {stop, Count};
           (_, Count) -> {next, Count + 1}
       end, 0, lists:reverse(Text)).


%%% ============================================================================
%%% File inclusion information

%% @spec is_included(node(), Data) -> bool()
%%     Data = string() | node()
%% @doc Returns true, if a file with specified path, or node is included
%%      in File, otherwise false.
is_included(File, Data) when is_list(Data) ->
    ?GRAPH:path(File, [{incl, {path, '==', Data}}]) =/= [];
is_included(File, Data) ->
    lists:member(Data, ?GRAPH:path(File, [incl])).


%% @spec is_header_file(node()) -> bool()
%% @doc Return true if there's module associated to the file in the DB.
is_header_file(File) ->
    (?GRAPH:data(File))#file.type == header andalso
        ?GRAPH:path(File, [moddef]) == [].


%% @spec included_from_path(node(), string(), atom()) -> string()
%% @doc Returns the path of the include file, which is included in `File'
%% and contains the entity definition (macro or record).
included_from_path(File, Name, Tag) ->
    (?GRAPH:data(included_from_file(File, Name, Tag)))#file.path.


%% @spec included_from_file(node(), string(), atom()) -> node()
%% @doc Returns the node of the include file, which is included in `File'
%% and contains the entity definition (macro or record).
included_from_file(File, Name, Tag) ->
    hd(?GRAPH:path(File, [incl, {Tag, {name, '==', Name}}, {Tag, back}])).


%%% ============================================================================
%%% Token information

%% @doc Returns the pre-whitespace of a token node.
prews(Node)  -> ((?GRAPH:data(first_token(Node)))#lex.data)#token.prews.

%% @doc Returns the post-whitespace of a token node.
postws(Node) -> ((?GRAPH:data(first_token(Node)))#lex.data)#token.postws.

%% @doc Returns the lexical data, the token data and the pre-whitespace
%%      of the token node.
lex_info(Node) ->
    Lex                   = ?GRAPH:data(Node),
    #lex{data = Data}     = Lex,
    #token{prews = PreWs} = Data,
    {Lex, Data, PreWs}.

%% @doc Reproduces the text of a token.
lex_text(#token{text = Text, prews = PreWS, postws = PostWS}) ->
    PreWS ++ Text ++ PostWS.

%% @doc Returns the first (last) token below the node.
first_token(Node) -> roll_down(Node, fun hd/1).

%% @doc Returns the first (last) token below the node.
last_token(Node)  -> roll_down(Node, fun lists:last/1).

%% @doc Walks a path down the syntax tree. `fun PosFun/1' sets the direction.
roll_down(Node, PosFun) ->
    case ?GRAPH:data(Node) of
        #lex{} -> Node;
        _      ->
            NodeOrder = ?SYNTAX:child_node_order(Node),
            roll_down(PosFun(NodeOrder), PosFun)
    end.

%% @spec token_data(node()) -> #token{}
%% @doc Returns the original token data for a token node
token_data(Node) ->
    case ?GRAPH:data(Node) of
        #lex{type=token, data=virtual} ->
            [Orig] = ?GRAPH:path(Node, [orig]),
            token_data(Orig);
        #lex{type=token, data=#token{}=Data} ->
            Data
    end.

%%% ============================================================================
%%% Miscellaneous short functions

includers(File) -> ?GRAPH:path(File, [{incl, back}]) -- [File].

%% @spec exists(node(), atom(), Tag) -> bool()
%%         Tag = macro | record
%% @doc Returns true if already exists a macro/record in the file
%% (or in its includes) with the given name.
exists(File, Name, Tag) ->
    ?GRAPH:path(File, [incl, {Tag, {name, '==', Name}}]) /= [].

%% @spec movable_to_header(Tag, atom(), node(), node()) -> bool()
%%         Tag = record | macro
%% @doc Returns true, if the given name do not clash in files, which are
%% including  the target file (except the from file). This means the record or
%% macro is movable to the header without name conflicts.
movable_to_header(Tag, Name, FFile, TFile) ->
    TargetIncluders = includers(TFile) -- [FFile],
    not lists:any(fun(File) -> exists(File, Name, Tag) end, TargetIncluders).

%% @spec containing_files([node()]) -> [node()]
%% @doc Returns a list of files containing the given forms.
containing_files(Forms) ->
    lists:usort([hd(?GRAPH:path(F, [{form, back}])) || F <- Forms]).


%%% ============================================================================
%%% Record/macro informations


%% @spec includable(node(), node()) -> bool()
%% @doc Returns true, if there's no conflict between the record
%%      and macro entities of the two file.
includable(Target, Incl) ->
    M1 = existing_macros_with_source(Target),
    M2 = existing_macros_with_source(Incl),
    R1 = existing_records_with_source(Target),
    R2 = existing_records_with_source(Incl),
    {MN1, _} = lists:unzip(M1--M2),
    {MN2, _} = lists:unzip(M2--M1),
    {RN1, _} = lists:unzip(R1--R2),
    {RN2, _} = lists:unzip(R2--R1),
    ?MISC:intersect(MN1, MN2) == [] andalso ?MISC:intersect(RN1, RN2) == [].

recmac_name(record, Node) -> #record{name=Name} = ?GRAPH:data(Node), Name;
recmac_name( macro, Node) ->  #macro{name=Name} = ?GRAPH:data(Node), Name.


%%% ----------------------------------------------------------------------------
%%% Existing entities in a file


%% @spec existing_macronames(node()) -> {[string()], [string()], [string()]}
%% @doc Tuple of list of macro names, that are defined in the file
%% and its includes. So: {Local, Incl, All} where All == Local ++ Incl
existing_macronames(File) ->
    {L, I, A} = existing_names(macro, File),
    MM = ["MODULE"],
    {L -- MM, I -- MM, A -- MM}.

%% @spec existing_recordnames(node()) -> {[string()], [string()], [string()]}
%% @doc Tuple of list of record names, that are defined in the file
%% and its includes. So: {Local, Incl, All} where All == Local ++ Incl
existing_recordnames(File) ->
    existing_names(record, File).

%% @private
%% @spec existing_names(node(), atom()) ->
%%           {[string()], [string()], [string()]}
%% @doc Tuple of list of record/macro names, that are defined in the file
%% and its includes.
existing_names(Type, File) ->
    All   = [recmac_name(Type, Node) || Node <- ?GRAPH:path(File, [incl, Type])],
    Local = [recmac_name(Type, Node) || Node <- ?GRAPH:path(File, [Type])],
    Incl = All -- Local,
    {Local, Incl, All}.

existing_macros_with_source(File) ->
    existing_names_with_source(File, macro).

%% @spec existing_records_with_source(node()) -> [{atom(), node()}]
%% @doc Returns list of pairs: the defined record names with the
%% files those contain the definitons.
existing_records_with_source(File) ->
    existing_names_with_source(File, record).

existing_names_with_source(File, Type) ->
    ?MISC:flatsort(
        [{E, File}
         || E <- [recmac_name(Type, Node) || Node <- ?GRAPH:path(File, [Type])]]
        ++
        [existing_names_with_source(F, Type)
         || F <- (?GRAPH:path(File, [incl]) -- [File])]
       ).


%%% ----------------------------------------------------------------------------
%%% Recursive macro/record queries


%% @spec records_by_macro(node()) -> [node()]
%% @doc Returns all record nodes that are referenced by `Macro'.
records_by_macro(Macro) ->
    Ms = [Macro | macros_by_macro(Macro)],
    Ss = lists:flatten([?GRAPH:path(M, [{mref, back}]) || M <- Ms]),
    ?MISC:flatsort([?GRAPH:path(S, [{llex, back}, {elex, back}, recref]) || S <- Ss]).

%% @spec macros_by_macro(node()) -> [node()]
%% @doc Returns all macro nodes that are referenced by the given macro.
macros_by_macro(Macro) -> macro_query(Macro, [{llex, back}, {llex, back}]).

%% @spec macro_users(node()) -> [node()]
%% @doc Returns macro nodes that are referencing to the given macro.
macro_users(Macro)     -> macro_query(Macro, [llex, llex]).

macro_query(Macro, Link) ->
    case ?GRAPH:path(Macro, [{mref, back}]) of
        Substs when is_list(Substs) andalso Substs =/= [] ->
            MacrosRecur = [macros_recur(Subst, Link) || Subst <- Substs],
            All         = ?MISC:flatsort(MacrosRecur),
            lists:usort([hd(?GRAPH:path(S, [mref])) || S <- All]);
        _ ->
            []
    end.

macros_recur(Subst, Link) ->
    News = [ New || New <- ?GRAPH:path(Subst, Link),
                    {lex, subst, _} <- [?GRAPH:data(New)]],
    case News of
        [] -> [];
        _ ->  lists:flatten(News ++ [macros_recur(M, Link) || M <- News])
    end.

%%% ----------------------------------------------------------------------------
%%% Used entities in functions


%% @spec used_records([node()]) -> {[node()], [string()]}
%% @doc Returns the list of record nodes and names,
%% that are used in the functions.
used_records(Funs) ->
    Path = [funcl, {functx,back}, {scope,back}, visib, {sup, back}, recref],
    Records =
        lists:usort(lists:umerge([?GRAPH:path(Fun, Path) || Fun <- Funs])),
    {Records, [recmac_name(record, R) || R <- Records]}.

%% @spec used_macros([node()]) -> {[node()], [string()]}
%% @doc Returns the list of macro nodes and names,
%% that are used in the functions.
used_macros(Funs) ->
    Lexicals = lists:usort(
                 lists:umerge(
                   lexicals(Funs, [clex]) ++
                   lexicals(Funs, [visib, {sup, back}, elex]))),
    Substs = [S || Lex <- Lexicals, S <- ?GRAPH:path(Lex, [llex]), 
                   [] =/= ?GRAPH:path(S, [mref])],
    %Used   = [macros_recur(Subst, [{llex, back}, {llex, back}]) || Subst <- Substs],
    %Users  = [macros_recur(Subst, [llex, llex]) || Subst <- Substs],
    %AllSubsts = ?MISC:flatsort(Substs ++ Used ++ Users),
    AllSubsts = ?MISC:flatsort(Substs),
    Macros = [hd(?GRAPH:path(S, [mref])) || S <- AllSubsts],
    {Macros, [[recmac_name(macro, M) || M <- Macros]]}.

%% @spec used_nodes_names(Tag, [node()]) -> {[node()], [atom()]}
%%         Tag = record | macro
%% @doc Used records or macros in the given function's bodies.
used_nodes_names(record, FnForms) -> used_records(FnForms);
used_nodes_names(macro,  FnForms) -> used_macros(FnForms).


%% @private
lexicals(Funs, SubPath) ->
    [?GRAPH:path(Fn, [funcl, {functx,back}, {scope,back}] ++ SubPath) ||
        Fn <- Funs].

%%% ============================================================================
%%% String checks

%% @spec is_valid_name(atom(), string()) -> bool()
%% @doc Checks whether the string is a representative of the given type.
%% @TODO should use scanner to check validity
is_valid_name(_, "") ->
    false;
is_valid_name(function, [H | T]) ->
    is_lower(H) andalso correct_tail(T);
is_valid_name(variable, "_") ->
    false;
is_valid_name(variable, [H|T]) ->
    (is_upper(H) orelse H == 95) andalso correct_tail(T).

is_upper(L) -> $A =< L andalso L =< $Z.
is_lower(L) -> $a =< L andalso L =< $z.
is_digit(L) -> $0 =< L andalso L =< $9.


correct_tail([])->
    true;
correct_tail([H|T]) ->
    (is_upper(H) orelse is_lower(H) orelse is_digit(H) orelse (H==64) orelse (H==95))
     andalso correct_tail(T).
