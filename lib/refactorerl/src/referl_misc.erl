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

%%% @doc This module collects miscellaneous functions,
%%%      mostly with functionality missing from standard modules, e.g. lists.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(referl_misc).
-vsn("$Rev: 1947 $").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Conversions between data structures.
-export([to_atom/1, to_list/1]).

%% Operations on lists.
-export([intersect/2, merge/2, index_list/1, text_index_list/1,
         separate_interval/3,
         join/1, join/2, flatjoin/2, flatsort/1,
         group_list/1, partfold/3, partfold/4]).

%% Error signalling.
-export([format/2]).
-export([error_on_difference/3]).

%% Map handling.
-export([lookup_symbols/3]).

%% Text handling.
-export([strip/1, escape/1]).

%% Check makers
-export([check_varname/1, get_file_node/2]).

%%% ============================================================================
%%% List/atom/etc. conversions

%% @spec to_atom(atom() | integer()) -> atom()
%% @doc Converts a list or an atom to an atom.
to_atom(Data) when is_atom(Data) -> Data;
to_atom(Data) when is_list(Data) -> list_to_atom(Data).

%% @spec to_list(atom() | integer() | float() | list()) -> list()
%% @doc Converts an atom/integer/list to a list.
to_list(Text) when is_atom(Text)    -> atom_to_list(Text);
to_list(Text) when is_integer(Text) -> integer_to_list(Text);
to_list(Text) when is_float(Text)   -> float_to_list(Text);
to_list(Text) when is_list(Text)    -> Text.


%%% ============================================================================
%%% Additions to the lists: module

%% @spec index_list([Elem]) -> [{Elem, integer()}]
%% @doc Adds indices to the elements of the list.
index_list(List) -> lists:zip(List, lists:seq(1, length(List))).


%% @spec text_index_list([Elem]) -> [{Elem, integer()}]
%% @doc Adds textual indices to the elements of the list.
text_index_list(List) ->
    [ {Elem, integer_to_list(Index)} || {Elem, Index} <- index_list(List) ].


%% @spec intersect([Elem], [Elem]) -> [Elem]
%% @doc Returns the intersection of the lists.
intersect(L1, L2) -> L1 -- (L1 -- L2).


%% @spec merge([Elem], [Elem]) -> [Elem]
%% @doc Merges two lists (having the same length) by elements
merge([], [])         -> [];
merge([X|Xs], [Y|Ys]) -> [X,Y|merge(Xs,Ys)].


%% @spec separate_interval([Elem], integer(), integer()) -> [Elem]
%% @doc Returns the elements of the list between the two given elements.
separate_interval(List, First, Last) ->
    list_reverse_drop(list_reverse_drop(List, First), Last).

%% @spec list_reverse_drop([Elem], Elem) -> [Elem]
%% @doc Reverses the list and drops elements up to the one given.
%%      Use it twice to separate an interval.
list_reverse_drop(List, Elem) ->
    Drop = lists:dropwhile(fun(Elem1) -> Elem1 /= Elem end, List),
    lists:reverse(Drop).


%% @spec join([string()]) -> [string()]
%% @doc Separate a list with commas.
join(List) -> join(List, ", ").


%% @spec join([Elem], Elem) -> [Elem]
%% @doc Inserts a separator between elements of a list.
%%      Also called `intercalate' in other languages.
join([E1, E2| Es], S) -> [E1, S| join([E2| Es], S)];
join([E], _)          -> [E];
join([], _)           -> [].

%% @spec flatjoin([Elem], Elem) -> [Elem]
%% @doc Inserts a separator between elements of a list, then flattens the result.
%%      Useful when handling texts.
flatjoin(List, Separator) -> lists:flatten(join(List, Separator)).

%% @spec flatsort([Elem]) -> [Elem]
%% @doc Flattens a list, then sorts it.
flatsort(List) ->
    lists:usort(lists:flatten(List)).


%% @spec group_list([{Keyword, Elem}]) -> [{Keyword, [Elem]}]
%% @doc Groups a list by its first elements. The list has to be ordered.
group_list([])                      -> [];
group_list([{Keyword, Value} | Ms]) ->
    {Vs, Rest} = split_by_keyword(Keyword, Ms),
    [{Keyword, [Value] ++ Vs}] ++ group_list(Rest).


%% Splits the first elements of an [{A,B}] list
%% whose first component equals the given value.
split_by_keyword(Keyword, Pairs)     -> split_by_keyword(Keyword, Pairs, []).

split_by_keyword(K, [{K,X}|Ks], Acc) -> split_by_keyword(K, Ks, Acc ++ [X]);
split_by_keyword(_, Ks, Acc)         -> {Acc, Ks}.

%% @spec partfold(Fun, Acc::term(), [term()]) -> term()
%%   Fun = (Elem::term(), AccIn::term()) ->
%%             {stop, AccOut} | {next, AccOut}
%%
%% @doc Similar to `lists:foldl/3', except that this can stop processing.
partfold(_Fun, Acc, []) ->
    Acc;
partfold(Fun, Acc0, [Head|Tail]) ->
    case Fun(Head, Acc0) of
        {stop, Result} -> Result;
        {next, Acc1} -> partfold(Fun, Acc1, Tail)
    end.

%% @spec partfold(Cond, Fun, Acc::term(), [term()]) -> term()
%%   Cond = (Elem::term(), AccIn::term()) -> bool()
%%   Fun = (Elem::term(), AccIn::term()) -> AccOut
%%
%% @doc Similar to `lists:foldl/3', except that this can stop processing.
partfold(_Cond, _Fun, Acc, []) ->
    Acc;
partfold(Cond, Fun, Acc, [Head|Tail]) ->
    case Cond(Head, Acc) of
        false -> Acc;
        true -> partfold(Cond, Fun, Fun(Head, Acc), Tail)
    end.

%%% ============================================================================
%%% Error signalling

%% @spec format(string(), [Params]) -> string()
%% @doc Creates a formatted string similar to io_lib:format/2.
%%      This is an ordinary string, not a deep list.
%%      If the text is too long, it will be broken to lines.
%%      In order to avoid breaking into lines, use for only the smallest
%%      portion to be formatted, and concatenate the rest of the message.
format(Format, Params) ->
    lists:flatten(io_lib:format(Format, Params)).

%% @spec error_on_difference(Type, Type, string()) -> ok
%% @doc Signals an error if the first two parameters differ.
error_on_difference(Val, ExpectedVal, ErrorText) ->
    if
        Val == ExpectedVal -> ok;
        true               -> throw(ErrorText)
    end.

%%% ============================================================================
%%% Map handling

%% @doc N nodes of symbol type S are removed from GrpSyms.
lookup_symbols(S, N, GrpSyms) ->
    {value, {S, SNodes}} = lists:keysearch(S, 1, GrpSyms),
    {SNodes1, SNodes2} = lists:split(N, SNodes),
    GrpSyms2 =
      case SNodes2 of
        [] -> lists:keydelete(S, 1, GrpSyms);
        _  -> lists:keydelete(S, 1, GrpSyms) ++ [{S, SNodes2}]
      end,
    {SNodes1, GrpSyms2}.

%%% ============================================================================
%%% Text handling

%% @spec strip(string()) -> string()
%% @doc Removes whitespace from the beginning and end of text.
strip(Text) ->
    IsWS     = fun(C) -> lists:member(C, " \t\n") end,
    RevStrip = fun(X) -> lists:dropwhile(IsWS, lists:reverse(X)) end,
    NoCommentText = lists:takewhile(fun(Char) -> Char /= $% end, Text),
    RevStrip(RevStrip(NoCommentText)).


%% @spec escape(string()) -> string()
%% @doc Escapes characters with backspace in a string.
escape([Char | Tail])
  when Char =:= $*; Char =:= $+; Char =:= $\\;
       Char =:= $.; Char =:= $$; Char =:= $^;
       Char =:= $(; Char =:= $); Char =:= $?;
       Char =:= $[; Char =:= $]; Char =:= $|;
       Char =:= $"     -> [$\\, Char | escape(Tail)];
escape([Char | Tail])
  when Char =:= $\ ; Char =:= $\t; Char =:= $\n  -> escape(Tail);
escape([Char | Tail])                            -> [Char | escape(Tail)];
escape([])                                       -> [].

%%% ============================================================================
%%% Check makers

%% @spec get_file_node(NamePos::integer(), FilePos::integer()) -> function()
%%
%% @doc Returns a function which takes a record, extracts a filename
%% from it, then stores the file node in the record. If the file
%% cannot be found it thrwos an exception. `NamePos' and `FilePos' are
%% the indices of the filename and filenode fields of the record. The
%% returned function can be used as a step in a refactoring. For
%% example:
%%
%% ```
%% -record(state, {filename, file}).
%% steps() -> [...
%%             get_file_node(#state.filename, #state.file),
%%             ...].
%% '''
get_file_node(NamePos, FilePos) ->
    fun (S) ->
            FileName = element(NamePos, S),
            case ?SYNTAX:file(FileName) of
                {file, File} -> setelement(FilePos, S, File);
                not_found    -> throw({"The file doesn't exist in the database!",
                                       FileName})
            end
    end.

%% @spec check_varname(NamePos::integer()) -> function()
%%
%% @doc Returns a function which takes a record, extarcts a name from
%% it, then check that the name is a legal vaiable name, throwing an
%% exception otherwise. `NamePos' is the index of the name field in
%% the record. The returned function can be used as a step in a
%% refactoring. Example:
%%
%% ```
%% -record(state, {varname}).
%% steps() -> [...
%%             check_varname(#state.varname),
%%             ...].
%% '''
check_varname(NamePos) ->
    fun (S) ->
            Name = element(NamePos, S),
            case is_list(Name) andalso ?LEX:is_valid_name(variable, Name) of
                false -> throw({"The given name is not a legal variable name",
                                Name});
                true  -> S
            end
    end.
