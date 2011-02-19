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
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(referl_misc).
-vsn("$Rev: 2621 $").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Conversions between data structures.
-export([to_atom/1, to_list/1]).

%% Records
-export([get_rec_type/1, get_rec_value/2, set_rec_value/3, check_record/2,
         record_to_proplist/2, record_to_proplist/3]).

%% Operations on general lists
-export([intersect/2, merge/2,
         separated_text/1,
         index_list/1, text_index_list/1,
         separate_interval/3,
         join/1, join/2, flatjoin/2, flatsort/1,
         group_list/1, partfold/3, partfold/4,
         list_find/2, list_member/2, list_compare/2, list_compare/3,
         list_contains/2, list_substract/3,
         common_prefix/2]).

%% Operations on special lists
-export([funlist_text/1, fun_text/1, add_article/1]).

%% Error signalling.
-export([format/2]).
-export([error_on_difference/3]).

%% Map handling.
-export([lookup_symbols/3]).

%% Text handling.
-export([strip/1, escape/1]).
%% Strings
-export([string_char_type/1, string_is_letter/1,
         string_strs/2, string_length/1, string_length/2, string_EOLs/0,
         string_lines/1, string_lines/2, string_split/5, string_replace/4,
         string_trim/1, string_trim/2, string_trim/3]).


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


%% @spec separated_text([atom()]) -> [string()]
%% @doc Converts the list of atoms into a printable format, the elements are
%% separated by a comma.
separated_text(Atoms) ->
    Texts = lists:map(fun atom_to_list/1, Atoms),
    join(Texts).


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
%% @spec common_prefix(list(), list()) -> list()
%% @doc  Returns the longest prefix of two lists.
common_prefix([A|Xs], [A|Ys]) -> [A|common_prefix(Xs, Ys)];
common_prefix(_, _) -> [].


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


%% @spec funlist_text([{atom(), natural()}]) -> [string()]
%% @doc  A list that enumerates a nonempty list of functions when flattened.
funlist_text([{FirstName, FirstArity}|Rest]) ->
    [   fun_text({FirstName, FirstArity}),
        [", " ++ fun_text([Name, Arity]) || {Name, Arity} <- Rest] ].


%% @spec fun_text(funinfo2()|funinfo3()) -> [string()]
%% @doc Converts the function signature into a printable format.
fun_text([Name, Arity]) ->
    [atom_to_list(Name), "/", integer_to_list(Arity)];
fun_text([Mod, Name, Arity]) ->
    [atom_to_list(Mod), ":", fun_text([Name, Arity])].


%% Adds the appropriate article to a string.
%% Special cases (e.g. "a European") are not accounted for.
add_article(Text = [Ch1|_])
    when Ch1 =:= $a, Ch1 =:= $e, Ch1 =:= $i, Ch1 =:= $o, Ch1 =:= $u,
         Ch1 =:= $A, Ch1 =:= $E, Ch1 =:= $I, Ch1 =:= $O, Ch1 =:= $U ->
    ["an ", Text];
add_article(Text) ->
    ["a ", Text].

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

%%% ----------------------------------------------------------------------------
%%% Record functions

%% @type record() = tuple(). The Built-in erlang record type.


%% @spec get_rec_type(Record::record()) -> atom()
%% @doc  Give back the type of `Record'.
get_rec_type(Record) when is_tuple(Record) -> element(1,Record).


%% @spec get_rec_value(Record::record(), FieldIndex::natural()) -> term()
%% @throws badarg
%% @doc  Return the value of the `Field' field of the `Record'. The
%%       `FieldIndex' is the position of the specified field in the tuple
%%        representation of the record.
%%
%% See <A HREF="http://www.erlang.org/doc/reference_manual/records.html#8.3"
%%      TARGET="_blank"><TT>Erlang Reference Manual - 8.3 Acessing Record Fields
%%      </TT></A>
%%
%% Example:
%% ```
%% get_rec_value(#people{name="Brian"}, #people.name).
%% '''
get_rec_value(Record, Key) when is_tuple(Record), is_integer(Key), 1<Key ->
    element(Key, Record).


%% @spec set_rec_value(Record::record(), FieldIndex::recordField(),
%%           Value::term()) -> record()
%% @throws badarg
%% @doc  Set the `Field' field of the `Record' to `Value'. The
%%       `FieldIndex' is the position of the specified field in the tuple
%%        representation of the record.
%%
%% See <A HREF="http://www.erlang.org/doc/reference_manual/records.html#8.3"
%%      TARGET="_blank"><TT>Erlang Reference Manual - 8.3 Acessing Record Fields
%%      </TT></A>
%%
%% Example:
%% ```
%% set_rec_value(#people{name="Brian", age=32}, #people.age, 33).
%% '''
set_rec_value(Record, Key, Value) when is_tuple(Record), is_integer(Key), 1<Key ->
    setelement(Key, Record, Value).


%% @type recordFilter() = {FieldIndex::natural(), Op, Value::term()}
%%       Op = '==' | '/='.
%% Condition for a field of a record.

%% @spec check_record(Record::record(), Filters::[recordFilter()]) -> bool()
%% @throws badarg | {badmatch, Op}
%% @doc  Check fields of `Record' by the given filter list.
%%       Get the field of the `Record' which specified with `Field' and compare
%%       that with `Value' using the `Op' as operator. If `Filters' is empty
%%       `Record' will be accepted.
%%
%% Example:
%% ```
%% check_record(#people{name="Brian", age=33}, [{#people.age,'==',33}]).
%% '''
%% @see  get_rec_value/2
check_record(Record, RecordFilters) when is_tuple(Record),
                                         is_list(RecordFilters) ->
    check_record_(Record, RecordFilters).

check_record_(_Record, []) -> true;
check_record_(Record, [{Key, Op, Value}|Filters]) ->
    RecordValue = get_rec_value(Record, Key),
    Match = case Op of
        '==' -> RecordValue == Value;
        '/=' -> RecordValue /= Value;
        _    -> erlang:error({badmatch,Op})
    end,
    case Match of
        true -> check_record_(Record, Filters);
        _    -> false
    end.


%% @spec record_to_proplist(Record::record(), RecordFields::[recordField()]) ->
%%           [{recordField(), term()}]
%% @doc  Put the record field name and value pairs into a tuple list.
%%       `RecordField' must contain the all field names of the record type in
%%       rigth order.
record_to_proplist(Record, RecFields) when is_tuple(Record), is_list(RecFields) ->
    lists:zip(RecFields,tl(tuple_to_list(Record))).

%% @spec record_to_proplist(Record::record(), Fields::[recordField()],
%%               RecordFields::[recordField()]) ->
%%           [{recordField(), term()}]
%% @doc  Put the record field name and value pairs into a tuple list.
%%       `Fields' may be partial and permuted list.
%%       `RecordField' must contain the all field names of the record type in
%%       rigth order.
record_to_proplist(Record, Fields, RecFields) when is_tuple(Record),
        is_list(Fields), is_list(RecFields) ->
    lists:foldl(fun(F, List) ->
                    [{F, get_rec_value(Record, F)}|List] end,
                [], lists:reverse(Fields)).



%%% ----------------------------------------------------------------------------
%%% List functions

%% @spec list_find(Pred::Pred, List::[term()]) -> {Index::integer(), Value::term()}
%%       Pred    = Value | PredFun
%%       Value   = term()
%%       PredFun = ((term()) -> bool())
%% @doc  Find the first element int the `List' that is accepted by `Pred'.
%%       `Pred' also may be a value and a function. If `Pred' is a value the
%%       `PredFun' is generated automatically to "`fun(Value) ->
%%       Value==ListElement end'" function.
%%       Return the value and the one based index of finded element. If there
%%       is no element found the `Index' is 0.
%%
%% Example:
%% ```
%% > list_find(fun(Ch) -> $X==Ch end, "Hello Mr. X!").
%% {11, 88}
%% '''
list_find(PredFun, List) when is_function(PredFun), is_list(List) ->
    list_find_(List, PredFun, 1);
list_find(Elem, List) when (not is_function(Elem)), is_list(List) ->
    list_find(fun(E) -> E==Elem end, List).

list_find_([],     _,       _  ) -> {0,no};
list_find_([X|Xs], PredFun, Idx) ->
    case PredFun(X) of
        true -> {Idx, X};
        _    -> list_find_(Xs, PredFun, Idx+1)
    end.


%% @spec list_member(Pred::Pred, List::[term()]) -> boolean()
%%       Pred    = Value | PredFun
%%       Value   = term()
%%       PredFun = ((term()) -> bool())
%% @doc  Find the first element int the `List' that is accepted by `Pred'.
%%       `Pred' also may be a value and a function. If `Pred' is a value the
%%       `PredFun' is generated automatically to "`fun(Value) ->
%%       Value==ListElement end'" function.
%%       If there is an element in the list that is accepted it returns
%%       with true otherwise false.
%%       Wrapper function to {@link list_find/2}.
%%
%% Example:
%% ```
%% > list_member(fun(Ch) -> $X==Ch end, "Hello Mr. X!").
%% true
%% '''
%% @see list_find/2
list_member(Pred, List) ->
    0/=element(1,list_find(Pred,List)).


%% @spec list_compare(List1::[term()], List2::[trem()]) ->
%%            {CommonPrefix::[term()], Tail1::[term()], Tail2::[term()]}
%% @doc  Compare lists elements with `==' operator and return the same prefix
%%       and the different tails.
%% @see  list_compare/3
list_compare(List1, List2) when is_list(List1), is_list(List2) ->
    list_compare(fun(X,Y) -> X==Y end, List1, List2).

%% @spec list_compare(EquFun::((ListElem1::term(), ListElem2::term()) -> bool()),
%%                List1::[term()], List2::[trem()]) ->
%%            {CommonPrefix::[term()], Tail1::[term()], Tail2::[term()]}
%% @doc  Compare lists elements with `EquFun' function and return the same prefix
%%       and the different tails.
%%
%% Example:
%% ```
%% > list_compare(fun(Ch1, Ch2) -> Ch1==Ch2 end, "Hello World!", "Hello Mars!").
%% {"Hello ", "World!", "Mars!"}
%% '''
list_compare(EquFun, List1, List2) when is_function(EquFun),
                                        is_list(List1), is_list(List2) ->
    list_compare_(EquFun, List1, List2, []).

list_compare_(_EquFun, [], Tail2, Common) -> {lists:reverse(Common), [], Tail2};
list_compare_(_EquFun, Tail1, [], Common) -> {lists:reverse(Common), Tail1, []};
list_compare_(EquFun, Tail1=[X|Xs], Tail2=[Y|Ys], Common) ->
    case EquFun(X,Y) of
        true -> list_compare_(EquFun, Xs, Ys, [X|Common]);
        _    -> {lists:reverse(Common), Tail1, Tail2}
    end.


%% @spec list_contains(List::list(), PossibleSubLists::[list()]) ->
%%           RealSubLists::[list()]
%% @doc  Find the real sublist of `List' from `PossibleSubLists'.
list_contains(List, SubLists) when is_list(List), is_list(SubLists) ->
    lists:filter(fun(SubList) -> []==(SubList--List) end, SubLists).


%% @spec list_substract(List1::[term()], List2::[term()], EquFun) -> [term()]
%%       EquFun = ((Elem1::term(), Elem2::term()) -> boolean())
%% @doc  Generalization of `--' list operator. Value of `EquFun' true when
%%       two element from the two list is equivalent.
list_substract([],  _List2, _EquFun) -> [];
list_substract(List1, [],   _EquFun) when is_list(List1) -> List1;
list_substract(List1, List2, EquFun) when is_list(List1), is_list(List2),
                                           is_function(EquFun) ->
    list_substract_(lists:reverse(List1), List2, EquFun, []).

list_substract_([],    _Ys, _EquFun, Zs) -> Zs;
list_substract_([X|Xs], Ys,  EquFun, Zs) ->
    case ?MISC:list_find(fun(Y) -> EquFun(X,Y) end, Ys) of
        {0,_} -> list_substract_(Xs, Ys, EquFun, [X|Zs]);
        {N,_} ->
            NewYs = lists:sublist(Ys,N-1) ++ lists:nthtail(N,Ys),
            list_substract_(Xs, NewYs, EquFun, Zs)
    end.



%%% ----------------------------------------------------------------------------
%%% String functions

%% @spec string_char_type(CharCode::integer()) -> CharType
%%       CharType = control | punctuation | digit | uppercase | lowercase |
%%                  unknown
%% @doc  Give back the type of the charcter specified by `CharCode'. If
%%       the type is unknown the result is `unknown'. Based on the erlang
%%       syntax.
%%
%% See <A HREF="http://www.erlang.org/doc/reference_manual/introduction.html#1.6"
%%      TARGET="_blank"><TT>Erlang Reference Manual - 1.6 Character Set</TT></A>
string_char_type(CharCode) when is_integer(CharCode) ->
    ChTypes = [
        { 32,  47, punctuation},    % SP !"#$%&'()*+,-./
        { 48,  57, digit},          % 0..9
        { 58,  64, punctuation},    % :;>=<?@
        { 65,  90, uppercase},      % A..Z
        { 91,  96, punctuation},    % [\]^_`
        { 97, 122, lowercase},      % a..z
        {123, 126, punctuation},    % {|}~
        {128, 159, control},        %
        {160, 191, punctuation},    %
        {192, 214, uppercase},      %
        {215, 215, punctuation},    %
        {216, 222, uppercase},      %
        {223, 246, lowercase},      %
        {247, 247, punctuation},    %
        {248, 255, lowercase}],     %
    CheckFun = fun({B, E, _T}) -> (CharCode<B) orelse (E<CharCode) end,
    case lists:dropwhile(CheckFun, ChTypes) of
        [] -> unknown;
        [{_B, _E, Type}|_] -> Type
    end.


%% @spec string_is_letter(CharCode::integer()) -> bool()
%% @doc  Check the `CharCode' represent a letter.
%% @see string_char_type/1
string_is_letter(CharCode) when is_integer(CharCode) ->
    Type = string_char_type(CharCode),
    (Type==uppercase) orelse (Type==lowercase).


%% @spec string_strs(String::string(), SubStrings::[string()]) ->
%%           {FirstPosition::integer(), MatchSubString::string()}
%% @doc  Find first substring in the `String'. Same as `string:str/2' but it
%%       find more than one substring. The returned pair is contained the lowest
%%       matching position of substrings, and the first from macthed substings
%%       at that position. If theres is no matched substring the position is 0.
%%
%% Example:
%% ```
%% > string_strs("Hello World!", ["Hello","World"]).
%% {1, "Hello"}
%% '''
string_strs(String, SubStrs) when is_list(String), is_list(SubStrs)->
    % Find first occurence of delimiters
    FirstFun = fun(Sub, {MinPos, MinSubStr}) ->
        Pos = string:str(String, Sub),
        if
            0<Pos andalso Pos<MinPos -> {Pos, Sub};
            true -> {MinPos, MinSubStr}
        end
    end,
    {MinPos, MinSubStr} =
        lists:foldl(FirstFun, {string:len(String)+1,""}, SubStrs),
    % Return founded position
    if
        MinSubStr/="" -> {MinPos, MinSubStr};
        true  -> {0, MinSubStr}
    end.


%% @spec string_length(String::string()) -> Length::integer()
%% @doc  Calculate the length of `String'. Windows new line characters are
%%       counted as one character.
%% @see string_length/2
string_length(String) when is_list(String) ->
    string_length(String, ["\r\n"]).

%% @spec string_length(String::string(), MultiByteChars::[string()]) ->
%%           Length::integer()
%% @doc  Calculate the length of `String'. All multibyte character given in
%%       `MultiByteChars' are counted as one charcter.
string_length(String, MultiByteChars) when is_list(String),
                                           is_list(MultiByteChars) ->
    string_length_(String, MultiByteChars, 0).

string_length_(String, MultiByteChars, Length) ->
    case string_strs(String, MultiByteChars) of
        {0,_} -> Length + string:len(String);
        {Pos,ChrStr} ->
            string_length_(lists:nthtail(Pos+string:len(ChrStr)-1,String),
                MultiByteChars, Length+Pos)
    end.


%% @spec string_EOLs() -> [string()]
%% @doc  Supported "End of Line" charcter sequences. Currently support Windows
%%       (\r\n), Macintosh (\r) and Unix (\n) end of line character sequences.
string_EOLs() -> ["\r\n","\r","\n"].  % [Windows, Macintosh, Unix]


%% @spec string_lines(String::string()) ->
%%           {Lines::[string()], FoundedEOLs::integer()}
%% @doc  Broke `String' into lines. Lines are also conatin the "End of Line"
%%       character sequences.
%% @see string_lines/2
string_lines(String) when is_list(String) ->
    string_lines(String, string_EOLs()).

%% @spec string_lines(String::string(), EOLs::[string()]) ->
%%           {Lines::[string()], FoundedEOLs::integer()}
%% @doc  Broke `String' into lines. Lines are also conatin the End of Line
%%       character sequences.
%% @see string_split/5
string_lines(String, EOLs) when is_list(String), is_list(EOLs) ->
    string_split(String, EOLs, 0, true, true).


%% @spec string_split(String::string(), DelimStrs::[string()], Count::integer(),
%%               Empty::bool(), WithDelim::bool()) ->
%%           {Parts::[string()], FoundedDelims::integer()}
%% @doc  Split `String' into `Parts' by strings from `DelimStrs'. Find
%%       only first `Count' part and the all remain string will be the
%%       `Count'+1-th part. If `Count' is lower then 1 all part will be returned.
%%       If `Empty' is true the parts with zero length are also given back in
%%       `Parts'. If `WithDelim' is true the string parts and the delimiter
%%       strings will be concatented.
%%
%% Example:
%% ```
%% > string_split("123.::456.789::", [".","::"], 2, true, true).
%% {["123.","::","456.789::"], 2}
%% '''
%%
%% @see string_strs/2
string_split(String, DelimStrs, Count, Empty, WithDelim) when is_list(String),
        is_list(DelimStrs), is_integer(Count), is_boolean(Empty),
        is_boolean(WithDelim) ->
    String2 = lists:flatten(String),
    DelimStrs2 = lists:map(fun lists:flatten/1, DelimStrs),
    string_split_(String2, DelimStrs2, Count, Empty, WithDelim, {[],0}).

% Split string
string_split_(String, DelimStrs, Count, Empty, WithDelim, {Parts,Founds}) ->
    % Find first occurence of delimiters
    {MinPos0, MinDelim} = string_strs(String, DelimStrs),
    % If enough parts are founded
    MinPos = if
        0<Count andalso Count=<length(Parts) -> 0;
        true -> MinPos0
    end,
    % Set splitting parameters
    {Founds2, Pos, Len} = if
        0<MinPos -> {Founds+1, MinPos, length(MinDelim)};
        true     -> {Founds,   length(String)+1, 0}
    end,
    % Split with or without delimiter string
    {Part1, TailStr} = if
        WithDelim ->
            lists:split(Pos+Len-1, String);
        true ->
            {H,  T0} = lists:split(Pos-1, String),
            {_T1,T2} = lists:split(Len,   T0),
            {H, T2}
    end,
    % Add splitted part to parts if necessarry
    Parts2 = if
        0==length(Part1) andalso (not Empty) -> Parts;
        true -> [Part1|Parts]
    end,
    % Process remains string if necessary
    if
        0==MinPos ->
            {lists:reverse(Parts2), Founds2};
        true ->
            string_split_(TailStr, DelimStrs, Count, Empty, WithDelim,
                {Parts2, Founds2})
    end.


%% @spec string_replace(String::string(), SrcStrs::[string()],
%%               DstStr::string(), Count::integer()) -> string()
%% @doc  Find substrings from `SubStrs' in `String' and replace them to
%%       `DstStr'. If `Count' is positive than replace only the first `Count'
%%       occurence otherwise replace all.
string_replace(String, SrcStrs, DstStr, Count) when is_list(String),
        is_list(SrcStrs), is_list(DstStr), is_integer(Count) ->
    % Split string by SrcStrs
    {Parts, Found} = string_split(String, SrcStrs, Count, true, false),
    if
        0<Found -> % Rejoin parts with DstStr
            RevParts = lists:reverse(Parts),
            lists:foldl(
                fun(Str, AccStr) -> Str++DstStr++AccStr end,
                hd(RevParts),
                tl(RevParts));
        true -> % Not found subsrings
            String
    end.


%%
%%
string_trim(String) -> string_trim(String, both).

%%
%%
string_trim(String, Mode) -> string_trim(String, Mode, " \t").

%%
%%
string_trim(String, Mode, TrimChrs) ->
    String1 = case lists:member(Mode, [left, both]) of
        true -> lists:dropwhile(fun(Ch) -> lists:member(Ch, TrimChrs) end,
                                String);
        _ -> String
    end,
    case lists:member(Mode, [right, both]) of
        true -> lists:reverse(lists:dropwhile(
                                    fun(Ch) -> lists:member(Ch, TrimChrs) end,
                                    lists:reverse(String1)));
        _ -> String1
    end.


