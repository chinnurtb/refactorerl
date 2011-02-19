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

%%% @doc Lexical analyser generator (or scanner compiler). Actually, this
%%% module only generates the state transition table for the lexical analyser,
%%% and saves it in a file as a textually represented Erlang term. The type of
%%% the table is the following:
%%%
%%% {@type [{State::integer(), {[{Char::char(), NextState::integer()@}],
%%%        Accept::atom()@}@}]}.
%%%
%%% The elements of the list are ordered by `State'. These tables are used by
%%% scanners created with {@link refgen_scanner:new/1}.
%%%
%%% The input of the generator is a simple text file. Each line of the file
%%% should have the following format:
%%%
%%% `name = regexp'
%%%
%%% If `name' begins with a capital letter, it works like binding a regular
%%% expression to a variable, which can later be referred by `{name}'.
%%% Otherwise, `name' will be used as an atom, and occurrences of the given
%%% regular expression will be tagged with this atom.
%%%
%%% Regular expressions may contain the following special constructs:
%%% <ul>
%%% <li>`a*' means zero or more repetitions</li>
%%% <li>`a+' means one or more repetitions</li>
%%% <li>`a?' means zero or one occurrence</li>
%%% <li>`a|b' means either `a' or `b'</li>
%%% <li>`(abc)' means grouping</li>
%%% <li>`[abc]' means either `a', `b', or `c'</li>
%%% <li>`[a-z]' means any character between `a' and `z'</li>
%%% <li>`[^a-z]' means any character not between `a' and `z'</li>
%%% <li>`.' means any character</li>
%%% <li>`\n' means LF</li>
%%% <li>`\r' means CR</li>
%%% <li>`\t' means a tabulator character</li>
%%% <li>`\' and any other character means that character</li>
%%% <li>`{Name}' means the contents of variable `Name'</li>
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refgen_scanc).
-vsn("$Rev: 3874 $").

-export([file/1, file/2, table/1]).

%% @spec file(string()) -> ok | {error, Reason}
%% @doc Same as `file(File, [])'.
%% @see file/2.
file(File) ->
    file(File, []).

%% @spec file(string(), Options::[{atom(), term()}]) -> ok | {error, Reason}
%% @doc Produces a lexical analyser state transition table from `File'.
%% Supported options are:
%% <dl>
%%  <dt>{@type {output, OutFile::string()@}}</dt>
%%  <dd>Save the output in file `OutFile'. Default: `.tab' appended to the
%%    input file name.</dd>
%% </dl>
file(File, Opt) ->
    case file:open(File, [read]) of
        {ok, Dev} ->
            gen(Dev, proplists:get_value(output, Opt, File ++ ".tab"), Opt);
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

gen(Input, OutFile, Opt) ->
    try gen_table(parse_input(Input)) of
        Table ->
            case file:open(OutFile, [write]) of
                {ok, Dev} ->
                    case proplists:get_value(verbose, Opt, false) of
                        true ->
                            io:format("Saving lexical analyser table in ~s~n",
                                      [OutFile]);
                        false -> ok
                    end,
                    ok = io:format(Dev, "~p.~n", [Table]),
                    file:close(Dev);
                {error, Reason} ->
                    {error, Reason}
            end
    catch
        throw:Error ->
            {error, Error}
    end.

%% @spec table([{Name::string, Regexp::string()}]) -> term()
%% @doc Produces a lexical analyser state transition table and returns it. The
%% return value can be passed as a `table' option to {@link
%% refgen_scanner:new/1}.
table(Table) ->
    gen_table(parse_table(Table)).

gen_table(Input) ->
    %%io:format("~p~n", [Input]),
    NFA = build_nfa(Input),
    DFA = nfa_to_dfa(NFA),
    fa_delete(NFA),
    MDFA = min_dfa(DFA),
    fa_delete(DFA),
    F = flat_dfa(MDFA),
    fa_delete(MDFA),
    F.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Input file parser

parse_input(File) ->
    parse_input(File, 1, [], dict:new()).

parse_input(File, Ln, Rules, Defs) ->
    case io:get_line(File, '') of
        eof ->
            file:close(File),
            lists:reverse(Rules);
        Line ->
            try parse_line(Line, Defs) of
                {Name, Regexp} when hd(Name) >= $A, hd(Name) =< $Z ->
                    parse_input(File, Ln+1,
                                Rules, dict:store(Name, Regexp, Defs));
                {Name, Regexp} ->
                    parse_input(File, Ln+1,
                                [{Regexp, list_to_atom(Name)} | Rules], Defs)
            catch
                throw:Reason ->
                    io:format("Error in line ~b: ~s~n", [Ln, Reason]),
                    throw(bad_file)
            end
    end.

parse_line(Line, Defs) ->
    case re:run(Line, "([0-9A-Z_a-z]+)\\h*=\\h*([^\\n]*)",
                [{capture, all_but_first, list}]) of
        {match, [Name, Re]} ->
            {Name, parse_regexp(Re, Defs)};
        nomatch ->
            throw("Bad rule")
    end.

parse_table(Table) ->
    {Rules, _} =
        lists:foldl(
          fun
              ({Name, Regex}, {Rules, Defs}) when hd(Name) >= $A,
                                                  hd(Name) =< $Z ->
                  RE = parse_regexp(Regex, Defs),
                  {Rules, dict:store(Name, RE, Defs)};
              ({Name, Regex}, {Rules, Defs}) ->
                  RE = parse_regexp(Regex, Defs),
                  {[{RE, Name} | Rules], Defs}
          end,
          {[], dict:new()},
          Table),
    lists:reverse(Rules).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Regular expression parser

parse_regexp(Inp, Names) ->
    {RE, ""} = parse_regexp(Inp, [], Names),
    %%io:format("~w~n",[RE]),
    RE.
parse_regexp(Rest = "", RE, _N) ->
    {lists:reverse(RE), Rest};
parse_regexp(Rest = (")" ++ _), RE, _N) ->
    {lists:reverse(RE), Rest};
parse_regexp("(" ++ Rest, RE, N) ->
    {RE1, Rest1} = parse_regexp(Rest, [], N),
    ")" ++ Rest2 = Rest1,
    parse_regexp(Rest2, [RE1 | RE], N);
parse_regexp("*" ++ Rest, [Top | RE], N) ->
    parse_regexp(Rest, [{rep, Top} | RE], N);
parse_regexp("+" ++ Rest, [Top | RE], N) ->
    parse_regexp(Rest, [{rep, Top}, Top | RE], N);
parse_regexp("?" ++ Rest, [Top | RE], N) ->
    parse_regexp(Rest, [{opt, Top} | RE], N);
parse_regexp("|" ++ Rest, RE, N) ->
    {RE1, Rest1} = parse_regexp(Rest, [], N),
    parse_regexp(Rest1, [{br, lists:reverse(RE), RE1}], N);
parse_regexp("[" ++ Rest, RE, N) ->
    {Set, Rest1} = parse_set(Rest),
    parse_regexp(Rest1, [Set | RE], N);
parse_regexp("{" ++ Rest, RE, N) ->
    {RE1, Rest1} = parse_name(Rest, N),
    parse_regexp(Rest1, [RE1 | RE], N);
parse_regexp("." ++ Rest, RE, N) ->
    parse_regexp(Rest, [{set, [{0,255}]}|RE], N);
parse_regexp("\\" ++ Rest, RE, N) ->
    {Char, Rest1} = parse_escape(Rest),
    parse_regexp(Rest1, [{char, Char}|RE], N);
parse_regexp([C | Rest], RE, N) ->
    parse_regexp(Rest, [{char, C}|RE], N).

parse_set("^" ++ Chars) -> parse_set(Chars, negset, []);
parse_set(Chars) -> parse_set(Chars, set, []).

parse_set("]" ++ Rest, Tag, []) ->
    parse_set(Rest, Tag, [$]]);
parse_set("]" ++ Rest, Tag, [{C} | Set]) ->
    {{Tag, [C|Set]}, Rest};
parse_set("]" ++ Rest, Tag, Set) ->
    {{Tag, Set}, Rest};
parse_set([$- | Rest], Tag, [Last | Set]) when is_integer(Last) ->
    parse_set(Rest, Tag, [{Last} | Set]);
parse_set([$\\ | Rest], Tag, Set) ->
    {Char, Rest1} = parse_escape(Rest),
    parse_set(Rest1, Tag, add_char(Char,Set));
parse_set([C | Rest], Tag, Set) ->
    parse_set(Rest, Tag, add_char(C, Set)).

add_char(End, [{Begin} | Set]) -> [{Begin, End} | Set];
add_char(Char, Set)            -> [Char|Set].

parse_escape("n" ++ Rest) -> {$\n, Rest};
parse_escape("t" ++ Rest) -> {$\t, Rest};
parse_escape("r" ++ Rest) -> {$\r, Rest};
parse_escape([A,B,C|Rest])
  when A >= $0, A =< $9, B >= $0, B =< $9, C >= $0, C =< $9 ->
    {64*(A-$0) + 8*(B-$0) + (C-$0), Rest};
parse_escape([C|Rest]) -> {C, Rest}.

parse_name(Text, Names) ->
    parse_name(Text, "", Names).
parse_name("}" ++ Rest, Name, Names) ->
    {dict:fetch(lists:reverse(Name), Names), Rest};
parse_name([C | Rest], Name, Names) ->
    parse_name(Rest, [C|Name], Names).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Build NFA from regexp


build_nfa(Input) ->
    NFA = fa_new(bag),
    Start = fa_newstate(NFA),
    fa_index(NFA, Start, start),
    {NumInp, _} =
        lists:mapfoldl(
          fun({RE, Acc}, N) -> {{RE, {N, Acc}}, N+1} end,
          1, Input),
    lists:foreach(fun(Cat) -> build_nfa(NFA, Cat, Start) end, NumInp),
    NFA.

build_nfa(NFA, {RE, Acc}, Start) ->
    AccSt = fa_newstate(NFA, Acc),
    build_re(RE, Start, AccSt, NFA).

build_re([RE], In, Out, NFA) ->
    build_re(RE, In, Out, NFA);
build_re([RE | Rest], In, Out, NFA) ->
    St = fa_newstate(NFA),
    build_re(RE, In, St, NFA),
    build_re(Rest, St, Out, NFA);
build_re({opt, RE}, In, Out, NFA) ->
    build_re(RE, In, Out, NFA),
    fa_addtrans(NFA, In, e, Out);
build_re({rep, RE}, In, Out, NFA) ->
    St = fa_newstate(NFA),
    build_re(RE, St, St, NFA),
    fa_addtrans(NFA, In, e, St),
    fa_addtrans(NFA, St, e, Out);
build_re({br, RE1, RE2}, In, Out, NFA) ->
    build_re(RE1, In, Out, NFA),
    build_re(RE2, In, Out, NFA);
build_re({char, C}, In, Out, NFA) ->
    fa_addtrans(NFA, In, C, Out);
build_re({set, Set}, In, Out, NFA) ->
    lists:foreach(fun (C) -> fa_addtrans(NFA, In, C, Out) end,
                  set_chars(Set));
build_re({negset, Set}, In, Out, NFA) ->
    lists:foreach(fun (C) -> fa_addtrans(NFA, In, C, Out) end,
                  lists:seq(0, 255) -- set_chars(Set)).

set_chars(Set) ->
    lists:usort(
      lists:flatmap(fun ({B,E}) -> lists:seq(B,E);
                        (C) -> [C] end,
                    Set)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Build DFA from NFA

nfa_to_dfa(NFA) ->
    DFA = fa_new(set),
    [NSt] = fa_find(NFA, start),
    Cl = closure(NFA, [NSt]),
    DSt = fa_newstate(DFA),
    fa_index(DFA, DSt, start),
    fa_index(DFA, DSt, Cl),
    det_nfa_states(DFA, NFA, [{DSt, Cl}]),
    DFA.

det_nfa_states(_DFA, _NFA, []) -> ok;
det_nfa_states(DFA, NFA, [{DSt, NSt} | Rest]) ->
    Trs = lists:usort(
            [C || S <- NSt,
                  {C, _} <- fa_all_trans(NFA, S),
                  C =/= e]),
    New = lists:flatmap(
            fun(C) -> det_nfa_trans(NFA, NSt, DFA, DSt, C) end,
            Trs),
    det_nfa_states(DFA, NFA, Rest ++ New).

det_nfa_trans(NFA, NSt, DFA, DSt, Tr) ->
    Next = lists:flatmap(fun(S) -> fa_trans(NFA, S, Tr) end, NSt),
    %io:format("~w --~c--> ~w~n", [NSt, Tr, Next]),
    NTo = closure(NFA, lists:usort(Next)),
    New =
        case fa_find(DFA, NTo) of
            [] ->
                AccLst = lists:usort([fa_accept(NFA, S) || S <- NTo]),
                Acc = case AccLst -- [[]] of
                          [{_, First}|_] -> First;
                          []  -> [];
                          L   -> throw({conflict, L}) % obsolete clause
                      end,
                To = fa_newstate(DFA, Acc),
                fa_index(DFA, To, NTo),
                [{To, NTo}];
            [To] ->
                []
        end,
    fa_addtrans(DFA, DSt, Tr, To),
    New.

closure(NFA, Lst) ->
    C=closure(NFA, Lst, Lst),
    %io:format("CL(~w) = ~w~n", [Lst, C]),
    C.
closure(NFA, Cl, Prev) ->
    Next = lists:flatmap(fun (S) -> fa_trans(NFA, S, e) end, Prev),
    New = lists:usort(Next) -- Cl,
    if
        New =:= [] -> Cl;
        true -> closure(NFA, lists:umerge(Cl, New), New)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Minimise DFA

min_dfa(DFA) ->
    FDFA = lists:keysort(2, flat_dfa(DFA)),
    MFDFA = drop_dups(FDFA),
    [Start] = fa_find(DFA, start),
    unflat_dfa(MFDFA, Start).

flat_dfa(DFA) ->
    [{St, {lists:usort(fa_all_trans(DFA, St)),
           fa_accept(DFA, St)}} ||
        St <- fa_states(DFA)].

unflat_dfa(FDFA, Start) ->
    DFA = fa_new(set),
    StMap = dict:from_list([{St, fa_newstate(DFA, Acc)} ||
                               {St, {_, Acc}} <- shift_start(FDFA, Start)]),
    lists:foreach(
      fun ({From, Tr, To}) -> fa_addtrans(DFA, From, Tr, To) end,
      [{From, Tr, dict:fetch(To, StMap)} ||
          {St, {Trans, _}} <- FDFA,
          is_integer(From = dict:fetch(St, StMap)),
          {Tr, To} <- Trans]),
    DFA.

shift_start(FDFA, Start) ->
    {value, StData, FDFA1} = lists:keytake(Start, 1, FDFA),
    [StData | FDFA1].


drop_dups(FDFA) ->
    case find_dup(FDFA) of
        {F,S} ->
            drop_dups(drop_dup(FDFA, F, S));
        none ->
            FDFA
    end.

find_dup([{St1, Data}, {St2, Data} | _]) -> {St1, St2};
find_dup([_ | Tail])                     -> find_dup(Tail);
find_dup([])                             -> none.

drop_dup(FDFA, F, S) ->
    lists:flatmap(
      fun({St, _}) when St =:= S ->
              [];
         ({St, {Trans, Acc}}) ->
              [{St, {lists:map(subst_to(S, F), Trans), Acc}}]
                  end,
      FDFA).

subst_to(S, F) ->
    fun
        ({Tr, To}) when To =:= S -> {Tr, F};
        (Trans) -> Trans
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generic finite automaton operations


fa_new(Type) when Type =:= set; Type =:= bag ->
    FA=ets:new(aut, [Type]),
    ets:insert(FA, {next, 0}),
    FA.

fa_newstate(FA) ->
    fa_newstate(FA, []).

fa_newstate(FA, Accept) ->
    [{_,S}] = ets:lookup(FA, next),
    ets:delete(FA, next),
    ets:insert(FA, {next, S+1}),
    ets:insert(FA, {S, Accept}),
    S.

fa_states(FA) ->
    [{_, Next}] = ets:lookup(FA, next),
    lists:seq(0, Next-1).

fa_accept(FA, St) ->
    [{_, Acc}] = ets:lookup(FA, St),
    Acc.

fa_addtrans(FA, In, C, Out) ->
    ets:insert(FA, {{In, C}, Out}).

fa_trans(FA, In, C) ->
    [To || {_, To} <- ets:lookup(FA, {In, C})].

fa_all_trans(FA, In) ->
    ets:select(FA, [{{{In,'$1'},'$2'},[],[{{'$1','$2'}}]}]).

fa_index(FA, St, Ind) ->
    ets:insert(FA, {{ind, Ind}, St}).

fa_find(FA, Ind) ->
    [St || {_, St} <- ets:lookup(FA, {ind, Ind})].

fa_delete(FA) ->
    ets:delete(FA).
