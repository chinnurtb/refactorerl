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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc
%%% Pretty Printer to format the modified and generated Erlang source code.
%%% It can modify the spaces between tokens, break and indent lines for
%%% different syntactical structures, break long lines and multiple line terms.
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(refcore_pp).
-vsn("$Rev$").

%%% ============================================================================
%%% Exports/imports

% Error messages
-export([error_text/2]).
% Formatter functions
-export([format/4, format_default/0, format_validator/0]).


-include("core.hrl").
-include("refcore_pp.hrl").

-include_lib("referl_lib/include/lib_export.hrl").



%%% ============================================================================
%%% Types and type functions



%% @ type tokenInfo().
%% Informations about a token.
-record(tokenInfo, {
    indent,         % Indentation record (see below)
    charIdx,        % Pair: Index of first and last visible character
    lineCharCnt,    % Number of caharcters in the line to this token
    rules = [],     % Formating rules
    data            % The data of the lexical node in the graph (a #lex record)
    }).

%% @ type multiInfo().
%% Informations about a multiple line term token.
-record(multiInfo, {
    tokenFirst,     % First token of the multiple line term
    tokenLast,      % Last token of the multiple line term
    rules = []      % Extra formating rules
    }).

%% @ type lineInfo().
%% Informations about lines.
-record(lineInfo, {
    % If you modify it, check get_token_line/3 function also !!!
    tokenFirst,     % First token of the line
    tokenLast,      % Last token of the line
    charCnt,        %
    lengthVisible,  %
    lengthTotal     %
    }).


%% @type ppState().
%% Informatins about tokens colected by Pretty Printer.
-record(ppState, {
    tokenFrom,      % Index: first token of selected part
    tokenTo,        % Index: last token of selected part
    hasPrevLine,    % Bool: there is a line before the selection
    hasNextLine,    % Bool: there is a line after the selection
    etc,            % ETS: counters and other various things
    tokens,         % ETS: contains the real tokens
    longNl,         % ETS: Line breaker tokens in long lines
    multiNl,        % ETS: Line breaker tokens in multiple line terms
    subst,          % ETS: Macro subsituations
    lines}).        % ETS: Line first and last token index (pair)

%% @private
%% @spec create_ppState() -> ppState()
%% @doc  Create a new ppState record and create ETS tables.
create_ppState() ->
    #ppState{
        etc      = ets:new(pp_etc,     []),
        tokens   = ets:new(pp_tokens,  []),
        longNl   = ets:new(pp_longNl,  []),
        multiNl  = ets:new(pp_multiNl, [ordered_set]),
        subst    = ets:new(pp_subst,   []),
        lines    = ets:new(pp_lines,   [ordered_set])}.

%% @private
%% @spec delete_ppState(State::ppState()) -> ok
%% @doc  Delete all ETS tables from a ppState record.
delete_ppState(State=#ppState{}) ->
    ets:delete(State#ppState.etc),
    ets:delete(State#ppState.tokens),
    ets:delete(State#ppState.longNl),
    ets:delete(State#ppState.multiNl),
    ets:delete(State#ppState.subst),
    ets:delete(State#ppState.lines),
    ok.






%%% ============================================================================
%%% Error messages


%% @private
%% @spec error_text(Type::atom(), Args::[term()]) -> string()
%% @doc  Give back the error message text of the error specified by `Type' and
%%       `Args'.
error_text(not_syntree_nodes, [Node1,Node2]) ->
    ?MISC:format("The ~p and ~p are not syntax nodes from the same syntax "
                 "tree.", [Node1,Node2]);
error_text(not_token_leaves, [FirstToken,LastToken]) ->
    ?MISC:format("The ~p or ~p syntax leaf node is not token.",
                 [FirstToken,LastToken]);
error_text(part_macro, Wrongs) ->
    ?MISC:format("The following macro subsituations ({Node, macro name}) are "
        "partial in the selcted part: ~p.", Wrongs);
error_text(not_has_tokens, [Node]) ->
    ?MISC:format("There is no order for the tokens of ~p node or "
        "it doesn't have tokens.", [Node]);
error_text(invalid_options, Wrongs) ->
    ?MISC:format("The following format options are invalids ({Key, Value}): ~p",
        Wrongs).






%%% ============================================================================
%%% Source code formation



%% @spec format_default() -> proplist()
%% @doc  Return the default options of {@link format/4}.
%%       For the allowed options and values see {@link format_validator/0}.
%% @see format_validator/0
format_default() ->
    [{indent,check},{newline,check},{space,reformat},
     {longline,true},{multiline,true}].


%% @spec format_validator() -> proplist()
%% @doc  Return validator functions for the options of {@link format/4}.
%%
%% The allowed options and values are:
%% <ul>
%%   <li>indent: Indent the lines
%%     <ul>
%%       <li>check: If the indentation size is smaller than the calculated
%%         size then correct it.</li>
%%       <li>reset: Delete the actual indentation of the lines and set the
%%         calculated size.</li>
%%       <li>false, no: The indentations won't be modified.</li>
%%     </ul></li>
%%   <li>newline: Recommended line breaks for syntactical structures
%%     <ul>
%%       <li>check: If the number of line breakers between tokens is less then
%%         the recommended by the rules then correct it.</li>
%%       <li>false, no: The recommended line breakers won't be modified.</li>
%%     </ul></li>
%%   <li>space: Required spaces between tokens
%%     <ul>
%%       <li>check: If the number of spaces between tokens is less then
%%         the required by the rules then correct it.</li>
%%       <li>reformat: Keep all lines but eliminate spaces outside of
%%         comments and set the required spaces again.</li>
%%       <li>reset: Delete the spaces and set the required spaces.</li>
%%       <li>false, no: The spaces won't be modified.</li>
%%     </ul></li>
%%   <li>longline = boolean(): Break long lines.</li>
%%   <li>multiline = boolean(): Do extra line breaks for multiple line terms.
%%     </li>
%%   <li>graph = {FilePath_prefix::string(), Mode::atom()}: Draw graph in
%%     `Mode' mode. For more information about `Mode' see
%%     {@link reflib_draw_graph:draw_file/2} function.
%%     The actual time and the formatting phase name will be concated after the
%%     `FilePath_prefix' string.
%%     Example: graph_MegaSec-Sec-MicroSec_before.dot</li>
%% </ul>
%% In general the values `false' and `no' are same.
format_validator() ->
    [{indent,    fun(V) -> lists:member(V, [check,reset,false,no]) end},
     {newline,   fun(V) -> lists:member(V, [check,      false,no]) end},
     {space,     fun(V) -> lists:member(V, [check,reformat,reset,false,no])end},
     {longline,  fun is_boolean/1},
     {multiline, fun is_boolean/1},
     {debug,     fun(V) -> proplist_is_valid(V, format_validator(debug)) end}].

% Validator for debug options
format_validator(debug) ->
    [{graph,    fun({F,M}) when is_list(F), is_atom(M) -> true;
                (_) -> false end}].


% Validate a proplist
proplist_is_valid(Options, ValidatorList) when is_list(Options),
        is_list(ValidatorList) ->
    FindRet = ?MISC:list_find(
        fun(E) ->
            not is_tuple(E) orelse 2/=tuple_size(E) orelse
            not is_atom(element(1,E))
        end,
        Options),
    case FindRet of
        {0,_} ->
            {_Misses,Wrongs} = ?MISC:proplist_validate(Options, ValidatorList),
            [] == Wrongs;
        {_,_} -> false
    end;
proplist_is_valid(_Options, _ValidatorList) -> false.


%% @spec format(Node1::node(), Node2::node(), Options::proplist(),
%%              Config_Rules::{Config::ppConfig(), ConfigRulesFun::RulesFun}) ->
%%           ok
%%       RulesFun = (() -> {IndentRules, WhiteSpaceRules, LongLineRules,
%%                          MultipleLineTermRules})
%%       IndentRules = [indentRule()]
%%       WhiteSpaceRules = [tokenRule()]
%%       LongLineRules = [tokenRule()]
%%       MultipleLineTermRules = [tokenRule()]
%% @throws {ModuleName::atom, ErrorCode::atom(), ErrorDetails::[term()]}
%% @doc  Format a part of source Erlang code. The part that should be formatted
%%       is specified by `Node1' and `Node2' syntactical nodes as the first and
%%       last node. The line prefix before the first token of `Node1', the
%%       line postfix after the last token of the `Node2', the previous line
%%       last and the next line first tokens are also will be formatted. But
%%       the required spaces and recommended line breakers will be modified
%%       just on the `Node1'..`Node2' interval.
%%
%%       The `Options' control the formatting method. For the legal oprtions see
%%       {@link format_validator/0}. {@link format_default/0} returns the
%%       default options.
%%
%%       The `Config' contains the ETS table identifiers of formatiing rules
%%       and configurations. If the ETS tables specified in `Config' are
%%       invalids the rules returned by `ConfigRulesFun' are loaded into the
%%       ETS tables.
%%       For Erlang language the {@link referl_pp_rules:erlang/0} function
%%       supply the default configurations and the rule provider function.
%% @see format_default/0
%% @see format_validator/0
%% @see referl_pp_rules:erlang/0
format(Node1, Node2, Options, {Config0=#ppConfig{general=ConfigGen}, RulesFun})
        when is_list(Options) ->
    % Check the options
    Options1 = ?MISC:proplist_merge_def(Options, format_default()),
    case ?MISC:proplist_validate(Options1, format_validator()) of
        {[],[]} -> ok;
        {Misses,Wrongs} -> throw(?LocalError(invalid_options, Misses++Wrongs))
    end,
    % Get the options
    InMode      = proplists:get_value(indent,    Options1),
    NlMode      = proplists:get_value(newline,   Options1),
    SpMode      = proplists:get_value(space,     Options1),
    LongLnMode  = proplists:get_value(longline,  Options1),
    MultiLnMode = proplists:get_value(multiline, Options1),
    % Debug options
    DbgOpts   = proplists:get_value(debug, Options1, []),
    Dbg_Graph = draw_graph_init(proplists:get_value(graph, DbgOpts, undefined)),
    draw_graph(Dbg_Graph, start),
    % Check formatting rules
    Config = ?PPR:check_load({Config0, RulesFun}),
    % Downpropagate the indentation iformations and collect real tokens
    State = downProp(Node1,Node2, Config,create_ppState()),
    % Some simple variable to simplify code
    TokensETS = State#ppState.tokens,
    DummyTokInfo = #tokenInfo{rules=[],
                    data=#lex{data=#token{text="",prews="",postws=""}}},
    % Add spaces and new lines to the white spaces between tokens
    case lists:member(SpMode, [check,reformat,reset]) orelse
            lists:member(NlMode, [check]) of
        true ->
            % Format the begin of file
            DummyTokRec = {no,no, DummyTokInfo}, % Node=no : Idx=no isn't used
            if
                1==State#ppState.tokenFrom andalso
                (not State#ppState.hasPrevLine) ->
                    [{1,TokenF,InfoF}] = ets:lookup(TokensETS, 1),
                    format_space([DummyTokRec, {1,TokenF,InfoF}],
                        SpMode, NlMode, ConfigGen#ppConfig_general.nlStr,
                        ConfigGen#ppConfig_general.commentLines, TokensETS);
                true -> ok
            end,
            % Format the selected part
            Cnt = ets:info(TokensETS, size),
            SpaceIdx1 = lists:max([1,   State#ppState.tokenFrom-1]),
            SpaceIdx2 = lists:min([Cnt, State#ppState.tokenTo+1]),
            TokRecs = lists:usort(ets:select(TokensETS, [{{'$1','_','_'},
                    [{'=<',SpaceIdx1,'$1'},{'=<','$1',SpaceIdx2}], ['$_']}])),
            format_space(TokRecs, SpMode, NlMode,
                         ConfigGen#ppConfig_general.nlStr,
                         ConfigGen#ppConfig_general.commentLines, TokensETS),
            % Format the end of file
            if
                Cnt==State#ppState.tokenTo andalso
                (not State#ppState.hasNextLine) ->
                    [{Cnt,TokenL,InfoL}] = ets:lookup(TokensETS, Cnt),
                    format_space([{Cnt,TokenL,InfoL}, DummyTokRec],
                        SpMode, NlMode, ConfigGen#ppConfig_general.nlStr,
                        ConfigGen#ppConfig_general.commentLines, TokensETS);
                true -> ok
            end;
        _ -> ok
    end,
    % Get the line prefix of first line that contain tokens to format and
    % create dummy {idx, node, tokenInfo} tuples
    [{_,Token1,_}] = ets:lookup(TokensETS, 1),
    {_,_,LineBegin0} = reallineBegin(Token1, 2),
    LineBegin0TokRecs = lists:map(
        fun(TokNode) ->
            {no, TokNode, DummyTokInfo} % Idx=no won't be used
        end,
        LineBegin0),
    % Calculate initial indices
    % DummyList = lists:duplicate(length(LineBegin0),ok),
    % Indices0 = calc_indices_tokrec(lists:zip3(DummyList,LineBegin0,DummyList),
        % {0,0,0,""}, {0,0}, {false,false},
        % ConfigGen#ppConfig_general.tabSize, State),
    Indices0 = calc_indices_tokrec(LineBegin0TokRecs, {0,0,0,""}, {0,0},
                {false,false}, ConfigGen#ppConfig_general.tabSize, State),
    calc_indices_tokrec(lists:usort(ets:tab2list(TokensETS)), Indices0,
        {1,1},{true,true}, ConfigGen#ppConfig_general.tabSize, State),
    % Set initial indentations
    case lists:member(InMode, [check,reset]) of
        true ->
            LnIdxs    = ?MISC:ets_keys(State#ppState.lines),
            LnIdxsLen = length(LnIdxs),
            LnIdxs2 = case {State#ppState.hasPrevLine,
                            State#ppState.hasNextLine} of
                {true,true}->lists:sublist(LnIdxs,2,lists:max([LnIdxsLen-2,0]));
                {true, false} -> lists:sublist(LnIdxs,2,LnIdxsLen-1);
                {false,true } -> lists:sublist(LnIdxs,1,LnIdxsLen-1);
                {false,false} -> LnIdxs
            end,
            format_indent(LnIdxs2, InMode, ConfigGen#ppConfig_general.useTab,
                          ConfigGen#ppConfig_general.tabSize, State);
        _ -> ok
    end,
    % Multiple line terms and long lines
    format_ml_ll(InMode, MultiLnMode,LongLnMode, ConfigGen,State),
    % Store back the datas of tokens
    store_token_data(TokensETS),
    % Delete state
    delete_ppState(State),
    % Draw graph after the formation
    draw_graph(Dbg_Graph, stop),
    ok.


% Initialize graph drawing
draw_graph_init({FilePathPrefrix, Mode}) ->
    {MegaSec,Sec,MicroSec} = erlang:now(),
    {?MISC:format("~s_~b-~b-~b",[FilePathPrefrix,MegaSec,Sec,MicroSec]), Mode};
draw_graph_init(_) ->
    undefined.

% Draw a graph
draw_graph({FilePathPrefix, Mode}, StepName) ->
    FilePath = ?MISC:format("~s_~p.dot", [FilePathPrefix, StepName]),
    ?DRAW_GRAPH:draw_graph(FilePath, Mode, true);
draw_graph(_, _StepName) ->
    ok.


% Store back the datas of nodes into the ESG
store_token_data(TokensETS) ->
    ets:foldl(
        fun({_Idx,Token,#tokenInfo{data=Data}}, _Acc) ->
            ?Graph:update(Token, Data)
        end,
        ok, TokensETS).




%% -----------------------------------------------------------------------------
%% Multiple line terms and long lines


% Apply the multiple line term and the long line formattion
format_ml_ll(InMode, MultiLnMode, LongLnMode, ConfigGen=#ppConfig_general{},
        State=#ppState{}) ->
    % Do multiple line terms  or long lines formation
    MultiModCnt = if
        MultiLnMode -> format_ml_(InMode, ConfigGen, State, 0);
        true        -> 0
    end,
    LongModCnt = if
        LongLnMode -> format_ll_(InMode, ConfigGen, State, 0);
        true       -> 0
    end,
    if
        0==MultiModCnt andalso 0==LongModCnt -> ok;
        true ->
            format_ml_ll(InMode, MultiLnMode, LongLnMode, ConfigGen, State)
    end.

% Apply the multiple line term formattion as many times as it possible
format_ml_(InMode, ConfigGen=#ppConfig_general{}, State, ModCount) ->
    case format_multi(InMode, ConfigGen, State) of
        [] -> ModCount;
        _  -> format_ml_(InMode, ConfigGen, State, ModCount+1)
    end.

% Apply the long line formattion as many times as it possible
format_ll_(InMode, ConfigGen=#ppConfig_general{}, State, ModCount) ->
    case format_long(InMode, ConfigGen, State) of
        [] -> ModCount;
        _  -> format_ll_(InMode, ConfigGen, State, ModCount+1)
    end.


% Apply the multiple line term formattion
format_multi(InMode, ConfigGen=#ppConfig_general{},
        State=#ppState{tokens=TokensETS, etc=EtcETS, lines=LinesETS,
                       multiNl=MultiETS}) ->
    % Find the real multiple line terms
    [{minLineIdx, MinLineIdx}] = ets:lookup(EtcETS, minLineIdx),
    [{maxLineIdx, MaxLineIdx}] = ets:lookup(EtcETS, maxLineIdx),
    Breaks = ets:foldl(
        fun({Idx, #multiInfo{tokenFirst=Idx1,tokenLast=Idx2, rules=PlusRules}},
                AccBreaks) ->
            LineIdx1 = get_token_line(Idx1, MinLineIdx, LinesETS),
            LineIdx2 = get_token_line(Idx2, MaxLineIdx, LinesETS),
            if
                LineIdx1/=LineIdx2 ->
                    [{Idx,Token,Info}] = ets:lookup(TokensETS,Idx),
                    [{{Idx,Token,Info},PlusRules}|AccBreaks];
                true -> AccBreaks
            end
        end,
        [], MultiETS),
    % Apply extra rules and return the indices of the modified lines
    format_plusWsRules(Breaks, MultiETS, InMode, ConfigGen, State).


% Apply the long line formattion
format_long(InMode, ConfigGen=#ppConfig_general{optLineLength=OptLineLength,
        minTextChars=MinTextChars, maxTextChars=MaxTextChars},
        State=#ppState{tokens=TokensETS, lines=LinesETS, longNl=LongETS}) ->
    % Find the long line breaker tokens
    Breaks = ets:foldl(
        fun({_LnIdx, #lineInfo{tokenFirst=Idx1,tokenLast=Idx2,charCnt=CharCnt,
                lengthVisible=LengthV}}, AccBreaks) ->
            if
                (OptLineLength<LengthV andalso MinTextChars=<CharCnt) orelse
                 MaxTextChars<CharCnt ->
                    OrdBreakStats =
                        format_long_sort_breaks(Idx1, Idx2, LongETS, TokensETS),
                    format_long_find_break(OrdBreakStats, OptLineLength,
                        MinTextChars, MaxTextChars, AccBreaks);
                 true ->
                    AccBreaks
            end
        end,
        [], LinesETS),
    % Apply extra rules and return the indices of the modified lines
    format_plusWsRules(Breaks, LongETS, InMode, ConfigGen, State).


% Sort the line breaker tokens in the line
% Two level order:
%   - Level 1: token is better if it is in a higher position in the syntax tree
%   - Level 2: token is better if it closer to the line end
format_long_sort_breaks(Idx1, Idx2, LongETS, TokensETS) ->
    lists:sort(lists:map(
        fun({Idx,SynLevel,PlusRules}) ->
            [{Idx,Token,Info}] = ets:lookup(TokensETS,Idx),
            {FirstCharIdx,LastCharIdx} = Info#tokenInfo.charIdx,
            {{ % Priority
              0+SynLevel,    % Higher in syntax tree
              0-LastCharIdx  % Closer to line end
             },
             {Idx,Token,Info}, % TokRec
             PlusRules,        % PlusRules
             { % Textual indices
              {FirstCharIdx,LastCharIdx},   % first/last character index
              Info#tokenInfo.lineCharCnt}}  % character in the line
        end,
        ets:select(LongETS,
            [{ {'$1','_','_'}, % {ChildIdx,SynLevel,LongRule}
               [{'=<',Idx1,'$1'}, {'=<','$1',Idx2}], % in the line
               ['$_'] % {ChildIdx,SynLevel,LongRule}
             }]) )).


% Find the best long line breaker
format_long_find_break(OrdBreakStats, OptLineLength,MinTextChars,MaxTextChars,
        AccBreaks) ->
    case ?MISC:list_find(
            fun({_Pr,_TR,_R,{{_FCIdx,LastCharIdx},TokCharCnt}}) ->
                LastCharIdx=<OptLineLength andalso
                MinTextChars=<TokCharCnt andalso
                TokCharCnt=<MaxTextChars
            end,
            OrdBreakStats) of
        {0,_} ->
            case ?MISC:list_find(
                    fun({_Pr,_TR,_R,{{_FCIdx,LastCharIdx},_CC}}) ->
                        LastCharIdx=<OptLineLength
                    end,
                    OrdBreakStats) of
                {0,_} ->
                    case OrdBreakStats of
                        [] -> AccBreaks;
                        [{_Pr,TokRec,PlusRules,_TxtIdxs}|_]  ->
                            [{TokRec,PlusRules} | AccBreaks]
                    end;
                {_, {_Pr,TokRec,PlusRules,_TxtIdxs}} ->
                    [{TokRec,PlusRules} | AccBreaks]
            end;
        {_, {_Pr,TokRec,PlusRules,_TxtIdxs}} ->
            [{TokRec,PlusRules} | AccBreaks]
    end.


% Apply the extra white space & new line formattion rules for a token
format_plusWsRules(Breaks, EtsTableId, InMode, ConfigGen=#ppConfig_general{},
        State=#ppState{}) ->
    lists:foldl(fun({{Idx,Token,Info},PlusRules}, ModLineIdxs) ->
            ets:delete(EtsTableId, Idx),    % It is processed now
            {PlusDiff, ModLnIdxs} =
                format_plusWsRules_({Idx,Token,Info}, PlusRules, InMode,
                                    ConfigGen, State),
            ModLineIdxs2 = lists:map(fun(Id) -> Id+PlusDiff end, ModLineIdxs),
            ModLnIdxs++ModLineIdxs2
        end,
        [], Breaks).

% Apply the extra white space & new line formattion rules for a token
format_plusWsRules_({IdxB,TokenB,InfoB}, Rules, InMode,
        #ppConfig_general{useTab=UseTab, tabSize=TabSize, nlStr=NlStr,
                          commentLines=CmLns},
        State=#ppState{tokens=TokensETS, lines=LinesETS}) ->
    % Add extra rules to the originals
    InfoB2 = InfoB#tokenInfo{
                rules=?PPR:merge_token_rules(InfoB#tokenInfo.rules,Rules)},
    ets:insert(TokensETS, {IdxB,TokenB,InfoB2}),
    % Get previous/next tokens and information about lines
    TokensCnt = ets:info(State#ppState.tokens, size),
    IdxA = lists:max([1,        IdxB-1]),
    IdxC = lists:min([TokensCnt,IdxB+1]),
    LineIdxA1 = get_token_line(IdxA, -1, LinesETS), % all exist
    LineIdxB1 = get_token_line(IdxB, -1, LinesETS),
    LineIdxC1 = get_token_line(IdxC, -1, LinesETS),
    [{LineIdxA1, LineInfoA1}] = ets:lookup(LinesETS, LineIdxA1),
    [{LineIdxC1, LineInfoC1}] = ets:lookup(LinesETS, LineIdxC1),
    LineDiffAB1 = LineIdxB1 - LineIdxA1,
    LineDiffBC1 = LineIdxC1 - LineIdxB1,
    % Apply extra new line rules and get the new different between lines
    {LineDiffAB2, ModLineIdxsB} = if
        IdxA<IdxB ->
            [{IdxA,TokenA,InfoA}] = ets:lookup(TokensETS, IdxA),
            {LDB,_} = format_sp_nl({IdxA,TokenA,InfoA},{IdxB,TokenB,InfoB2},
                                   no,check, NlStr,CmLns, TokensETS),
            {LDB, [LineIdxA1+LDB]};
        true -> {0, []}
    end,
    {LineDiffBC2, ModLineIdxsC} = if
        IdxB<IdxC ->
            [{IdxC,TokenC,InfoC}] = ets:lookup(TokensETS, IdxC),
            {LDC,_} = format_sp_nl({IdxB,TokenB,InfoB2},{IdxC,TokenC,InfoC},
                                   no,check, NlStr,CmLns, TokensETS),
            {LDC, [LineIdxA1+LineDiffAB2+LDC|ModLineIdxsB]};
        true -> {0, ModLineIdxsB}
    end,
    % Modify lines and return the indices of modified lines
    PlusDiff = (LineDiffAB2+LineDiffBC2) - (LineDiffAB1+LineDiffBC1),
    if
        LineDiffAB1==LineDiffAB2 andalso LineDiffBC1==LineDiffBC2 ->
            {PlusDiff, []};
        true ->
            % Remove original lines
            lists:foreach(fun(LineIdx) -> ets:delete(LinesETS, LineIdx) end,
                lists:usort([LineIdxA1,LineIdxB1,LineIdxC1])),
            % Shift lines
            shift_lines(LineIdxA1, PlusDiff, LinesETS),
            % Recalculate indices
            recalc_indices_byfirst({IdxA,LineInfoC1#lineInfo.tokenLast},
                {LineIdxA1,LineInfoA1#lineInfo.tokenFirst}, TabSize, State),
            ModLineIdxs = lists:usort(ModLineIdxsC) -- [LineIdxA1],
            % Indent modified lines
            case lists:member(InMode, [check,reset]) of
                true -> format_indent(ModLineIdxs,check,UseTab,TabSize,State);
                _    -> ok
            end,
            {PlusDiff, ModLineIdxs}
    end.




%% -----------------------------------------------------------------------------
%% Indentation


% Apply the indentation formattion for lines
format_indent([], _InMode, _UseTab, _TabSize, _State) -> ok;
format_indent([LineIdx|LineIdxs], InMode, UseTab, TabSize,
        State=#ppState{tokens=TokensETS, lines=LinesETS}) ->
    % Get the First token of line
    [{LineIdx, #lineInfo{tokenFirst=LineFirstIdx, tokenLast=LineLastIdx}}] =
        ets:lookup(LinesETS,  LineIdx),
    [{LineFirstIdx,Token1,Info1}] = ets:lookup(TokensETS, LineFirstIdx),
    % Get current indentation
    LexData1 = Info1#tokenInfo.data,
    TokData1 = LexData1#lex.data,
    {Parts1,_} = ?MISC:string_lines(TokData1#token.prews),
    {PreWsFirstLines, [PreWsLastLine]} = split(length(Parts1)-1,Parts1),
    CurrInSize1 = get_last_charIdx(0, PreWsLastLine, TabSize),
    % Get target indentation
    InLevel1 = (Info1#tokenInfo.indent)#indent.level,
    InSize1  = InLevel1*TabSize,
    % Modify indentation
    if
        (check==InMode andalso CurrInSize1<InSize1) orelse
         reset==InMode ->
            % Indent prews
            IndentString = if
                UseTab -> lists:duplicate(InLevel1, 9);
                true   -> lists:duplicate(InSize1, 32)
            end,
            PreWsFirstLines2 = lists:map(
                fun(Ln) -> IndentString ++ ?MISC:string_trim(Ln,left) end,
                PreWsFirstLines),
            % Updates prews
            update_token_ws({LineFirstIdx,Token1,Info1},
                {#token.prews,[PreWsFirstLines2,IndentString]},TokensETS),
            % Info1B = update_token_info(Info1, #token.prews,
                                       % [PreWsFirstLines2,IndentString]),
            % Indent postws
            [{LineLastIdx,Token2,Info2}] = ets:lookup(TokensETS, LineLastIdx),
            LexData2 = Info2#tokenInfo.data,
            TokData2 = LexData2#lex.data,
            {Parts2,_} = ?MISC:string_lines(TokData2#token.postws),
            {[PostWsFirstLine], PostWsLastLines0} = lists:split(1,Parts2),
            {PostWsMiddleLines, [PostWsLastLine]} =
                split(length(PostWsLastLines0)-1,PostWsLastLines0),
            PostWsMiddleLines2 = lists:map(
                    fun(TrimLn) ->
                        case ?MISC:list_find(TrimLn, ?MISC:string_EOLs()) of
                            {0,_} -> IndentString ++ TrimLn;
                            {_,_} -> TrimLn
                        end
                    end,
                    trim_lines(PostWsMiddleLines, left)),
            % Update postws
            update_token_ws({LineLastIdx,Token2,Info2}, {#token.postws,
                [PostWsFirstLine,PostWsMiddleLines2,PostWsLastLine]},TokensETS),
            % Update token indices
            [{LineFirstIdx,Token1,Info1C}] = ets:lookup(TokensETS,LineFirstIdx),
            {_,{FirstCharIdx,LastCharIdx},CharCnt,_} =
                get_token_indices({0, 0, 0, ""}, Info1C, TabSize),
            Info1D = Info1C#tokenInfo{charIdx={FirstCharIdx,LastCharIdx},
                                      lineCharCnt=CharCnt},
            ets:insert(TokensETS, {LineFirstIdx, Token1, Info1D}),
            % Update the other token indices in the line
            recalc_indices_byfirst({LineFirstIdx,LineLastIdx},
                {LineIdx,LineFirstIdx}, TabSize, State);
        true -> ok
    end,
    % Recursion
    format_indent(LineIdxs, InMode, UseTab, TabSize, State).



%% -----------------------------------------------------------------------------
%% Spaces and new lines: set spaces and required line breakers between tokens


% Apply the white space & new line formattion
format_space([], _SpMode, _NlMode, _NlStr, _CommentLines,_TokensETS) -> 0;
format_space([TokRec1|TokRecs], SpMode,NlMode, NlStr,CommentLines, TokensETS) ->
    {LineNumDiff, _} = lists:foldl(
        fun({IdxB,TokenB,InfoB}, {AccLineNumDiff, TokRecA}) ->
            {PlusLineNumDiff, InfoB2} =
                format_sp_nl(TokRecA,{IdxB,TokenB,InfoB}, SpMode,NlMode,
                             NlStr,CommentLines, TokensETS),
            {AccLineNumDiff+PlusLineNumDiff, {IdxB,TokenB,InfoB2}}
        end,
        {0, TokRec1},
        TokRecs),
    LineNumDiff.


% Apply the white space & new line formattion for two following token
format_sp_nl({IdxA,TokenA,InfoA=#tokenInfo{rules=RulesA, data=DataA}},
        {IdxB,TokenB,InfoB=#tokenInfo{rules=RulesB, data=DataB}},
        SpMode, NlMode, NlStr, _CommentLines, TokensETS) ->
    % Get the newline rule value between tokens
    {WsMax,WsSum,WsA,WsB} = ?PPR:get_token_rule_between(ws, RulesA, RulesB),
    {NlMax,_,_,  _  } = ?PPR:get_token_rule_between(nl, RulesA, RulesB),
    WsBetween = (DataA#lex.data)#token.postws ++ (DataB#lex.data)#token.prews,
    % Get the white space between tokens
    {LinesA,LinesB, NlCount,WsLen} = case SpMode of
            reset    -> split_tokens_ws("", WsA, WsB, fun(Lns) -> Lns end);
            reformat -> split_tokens_ws(WsBetween, WsA, WsB,
                                        fun(Lns) -> trim_lines(Lns, both) end);
            _        -> split_tokens_ws(WsBetween, WsA, WsB,
                                        fun(Lns) -> Lns end)
        end,
    % Broke the current white spaces into lines
    {FirstLinesA, [LastLineA]} = split(length(LinesA)-1,LinesA),
    {[FirstLineB], LastLinesB} = lists:split(1,LinesB),
    % Correct the current white spaces
    {LinesA1, LinesB1, LineNumDiff} = if
        % New lines
        check==NlMode andalso 0<NlMax andalso NlCount<NlMax ->
            case ?MISC:string_strs(LastLineA, ?MISC:string_EOLs()) of
                {0, _} -> {FirstLinesA++[LastLineA++NlStr]++
                              lists:duplicate(NlMax-NlCount-1,NlStr),
                           [FirstLineB|LastLinesB],
                           NlMax};
                _      -> {FirstLinesA++[LastLineA]++
                               lists:duplicate(NlMax-NlCount,NlStr),
                           [FirstLineB|LastLinesB],
                           NlMax}
            end;
        % Only spaces
        (reset==SpMode orelse reformat==SpMode orelse check==SpMode) andalso
         0==NlCount andalso WsLen<WsMax ->
            WsLenA = round(WsA/WsSum*(WsMax-WsLen)),
            WsLenB = (WsMax-WsLen)-WsLenA,
            {FirstLinesA++[LastLineA++lists:duplicate(WsLenA,32)],
             [lists:duplicate(WsLenB,32)++FirstLineB|LastLinesB],
             NlCount};
        % Not change
        true ->
            {LinesA, LinesB, NlCount}
    end,
    % Updete tokens white spaces
    InfoB2 = update_tokens_ws({IdxA,TokenA,InfoA, remove_lines1_ends(LinesA1)},
                {IdxB,TokenB,InfoB, remove_lines_ends(LinesB1)}, TokensETS),
    % Return difference between the line numbers of tokens
    {LineNumDiff, InfoB2}.



%%% ----------------------------------------------------------------------------
%%% White space partitions

% Remove line end white spaces.
% The first line handled in different mode.
remove_lines1_ends([Line|Lines]) ->
    case string:strip(Line) of
        "" -> [Line|remove_lines_ends(Lines)];
        _  ->
            case hd(is_comment_lines([Line])) andalso 32/=hd(Line) of
                true ->
                    [ [32 | hd(remove_lines_ends([Line]))] |
                      remove_lines_ends(Lines)];
                _    -> remove_lines_ends([Line|Lines])
            end
    end.

% Remove line end white spaces
remove_lines_ends(Lines) -> trim_lines(Lines, right, true, false).


% Check the `Line' is a comment line
is_comment_lines(Lines) when is_list(Lines) ->
    lists:map(fun(TrimLn) -> ""=/=TrimLn end,
              trim_lines(Lines, both, false, true)).

% Trim lines
trim_lines(Lines, Mode) -> trim_lines(Lines, Mode, true, true).

% Trim lines
trim_lines(Lines, Mode, LineEnd, NotLine) when is_atom(Mode),
        is_boolean(LineEnd), is_boolean(NotLine) ->
    lists:map(fun(Ln) ->
            case ?MISC:string_strs(Ln, ?MISC:string_EOLs()) of
                {0,_    } ->
                    if
                        NotLine -> ?MISC:string_trim(Ln, Mode);
                        true    -> Ln
                    end;
                {N,NlStr} ->
                    Ln1 = ?MISC:string_trim(lists:sublist(Ln, 1, N-1), Mode),
                    if
                        LineEnd -> Ln1++NlStr;
                        true    -> Ln1
                    end
            end
        end,
        Lines).


% Split white space between two following token
split_tokens_ws(String, WsVal1, WsVal2, LinesFun) when is_list(String),
        is_integer(WsVal1), 0=<WsVal1, is_integer(WsVal2), 0=<WsVal2,
        is_function(LinesFun) ->
    {Lines0, NlCount} = ?MISC:string_lines(String),
    Lines = LinesFun(Lines0),
    WsLen = lists:foldl(fun(Ln, Acc) -> Acc+length(Ln) end, 0, Lines),
    {LinesA, LinesB} = case length(Lines) of
        1 -> % Only spaces (Token1 Token2)
            WsLen1 = case WsVal1+WsVal2 of
                0     -> 0;
                WsSum -> round((WsVal1/WsSum)*WsLen)
            end,
            {Str1, Str2} = lists:split(WsLen1, hd(Lines)),
            {[Str1], [Str2]};
        2 -> % One one line (Token1 <comment>\n Token2)
            {[hd(Lines)], tl(Lines)};
        _ -> % More lines (Token1 <comment>\n ... <comment>\n Token2)
            {L1,      LTail} = lists:split(1, Lines),
            {LMiddle, LLast} = split(length(LTail)-1, LTail),
            CmLines = is_comment_lines(LMiddle),
            % % Comments on middle are belong to the previous lines
            % case ?MISC:list_find(false, lists:reverse(CmLines)) of
                % {0,_} -> {L1, LMiddle++LLast};
                % {N,_} ->
                    % {LM1,LM2} = lists:split(length(LMiddle)-N+1,LMiddle),
                    % {L1++LM1, LM2++LLast}
            % end
            % Comments on middle are belong to the follow lines
            case ?MISC:list_find(false, CmLines) of
                {0,_} -> {L1, LTail};
                {N,_} ->
                    case ?MISC:list_find(true, lists:nthtail(N,CmLines)) of
                        {0,_} -> {L1++LMiddle, LLast};
                        {M,_} ->
                            {LM1,LM2} = lists:split(N+M-1,LMiddle),
                            {L1++LM1, LM2++LLast}
                    end
            end
    end,
    {LinesA,LinesB, NlCount, WsLen}.


% Store the formatted white space between two following token
update_tokens_ws({IdxA,TokenA,InfoA,LinesA}, {IdxB,TokenB,InfoB,LinesB},
        TokensETS) when is_integer(IdxA), 0<IdxA, is_integer(IdxB), 0<IdxB ->
    update_token_ws({IdxA,TokenA,InfoA}, {#token.postws,LinesA}, TokensETS),
    update_token_ws({IdxB,TokenB,InfoB}, {#token.prews, LinesB}, TokensETS);
update_tokens_ws({IdxA,TokenA,InfoA,LinesA}, {_IdxB,_TokenB,_InfoB,LinesB},
        TokensETS) when is_integer(IdxA), 0<IdxA ->
    update_token_ws({IdxA,TokenA,InfoA},
                    {#token.postws, [LinesA|LinesB]}, TokensETS);
update_tokens_ws({_IdxA,_TokenA,_InfoA,LinesA}, {IdxB,TokenB,InfoB,LinesB},
        TokensETS) when is_integer(IdxB), 0<IdxB ->
    update_token_ws({IdxB,TokenB,InfoB},
                    {#token.prews, [LinesA|LinesB]}, TokensETS).

% Store the formatted white space of a token before or after the token
update_token_ws({Idx,Token,TokInfo}, {TokenField,Lines}, TokensETS) ->
    TokInfo2 = update_token_info(TokInfo, TokenField, Lines),
    ets:insert(TokensETS, {Idx,Token,TokInfo2}),
    TokInfo2.

% Modify the TokenInfo of the token with the formatted white space of a token
% before or after the token
update_token_info(TokInfo, TokenField, Lines) ->
    LexData1 = TokInfo#tokenInfo.data,
    TokData1 = LexData1#lex.data,
    TokData2 = setelement(TokenField, TokData1, lists:flatten(Lines)),
    LexData2 = LexData1#lex{data=TokData2},
    TokInfo#tokenInfo{data=LexData2}.






%% -----------------------------------------------------------------------------
%% Line and charcter index operations


% Find the line that contain the token and give back the index of the line
get_token_line(Idx, DefaultLineIdx, LinesETS) ->
    Select = [{{'$1','$2'},
               [{'=<',{element,#lineInfo.tokenFirst,'$2'},Idx},
                {'=<',Idx, {element,#lineInfo.tokenLast,'$2'}}],
               ['$1']}],
    case ets:select(LinesETS, Select) of
        [LineIdx] -> LineIdx;
        []        -> DefaultLineIdx
    end.


% Find the index of a token
get_token_idx(Token, DefaultIdx, TokensETS) ->
    case ets:select(TokensETS, [{{'$1','$2','_'}, [{'==','$2',{const,Token}}],
            ['$1']}]) of
        [Idx] -> Idx;
        []    -> DefaultIdx;
        % todo What if several indices are found?
        _     -> DefaultIdx
    end.


% Set the indices of the first and last token of the multiple line terms
calc_multi_indices(#ppState{tokens=TokensETS, multiNl=MultiETS}) ->
    MultiRecs2 = ets:foldl(
        fun({Idx,MultiInfo=#multiInfo{tokenFirst=TokenA, tokenLast=TokenB}},
                Recs) ->
            IdxA = get_token_idx(TokenA, -1, TokensETS),
            IdxB = get_token_idx(TokenB, -1, TokensETS),
            [{Idx,MultiInfo#multiInfo{tokenFirst=IdxA, tokenLast=IdxB}}|Recs]
        end,
        [],
        MultiETS),
    lists:foreach(fun({Idx,MultiInfo}) ->
            ets:insert(MultiETS, {Idx,MultiInfo})
        end,
        MultiRecs2).


% Shift lines (Shift the line indices)
shift_lines(FromLineIdx, Diff, _LinesETS) when
        is_integer(FromLineIdx), 0=<FromLineIdx, is_integer(Diff), 0==Diff ->
    ok;
shift_lines(FromLineIdx, Diff, LinesETS) when
        is_integer(FromLineIdx), 0=<FromLineIdx, is_integer(Diff) ->
    % Get lines that we should shift
    LineRecs = ets:select(LinesETS, [{{'$1','_'}, [{'<',FromLineIdx,'$1'}],
                                      ['$_']}]),
    % Delete lines
    lists:foreach(fun({LineIdx,_}) -> ets:delete(LinesETS, LineIdx) end,
                  LineRecs),
    % Add lines back with shifted line indices
    lists:foreach(fun({LineIdx, Info}) ->
            ets:insert(LinesETS, {LineIdx+Diff, Info})
        end,
        LineRecs).


% Recalculate the indices of the tail of line. Use the indices of the
% first token to calculate the indices of the other tokens in the line
recalc_indices_byfirst({Idx1,Idx2}, {LineIdx,LineFirstIdx},
        TabSize, State=#ppState{tokens=TokensETS}) ->
    [{Idx1,_Token1,Info1}] = ets:lookup(TokensETS, Idx1),
    LastCharIdx1 = element(2,Info1#tokenInfo.charIdx),
    PostWs1      = ((Info1#tokenInfo.data)#lex.data)#token.postws,
    CharCnt1     = Info1#tokenInfo.lineCharCnt,
    TokRecs = lists:usort(ets:select(TokensETS,
        [{{'$1','_','_'}, [{'<',Idx1,'$1'},{'=<','$1',Idx2}], ['$_']}])),
    calc_indices_tokrec(TokRecs, {LineIdx,LastCharIdx1,CharCnt1,PostWs1},
        {LineFirstIdx,Idx1}, {true,true}, TabSize, State).


% Calculate textual indices of tokens.
% If `SetToken' is false the calculated indices won't be written back
% into the `State'. If `SetToken' is false `IdxB' and `InfoB' is not used.
calc_indices_tokrec([], {LineIdxA,LastCharIdxA,CharCntA,PostWsA},
        {LineAFirstIdx,LineALastIdx}, {_SetToken,SetLine}, TabSize,
        State=#ppState{}) ->
    % Set line indices
    if
        SetLine ->
            calc_indices_tokrec_line({LineIdxA,LineAFirstIdx,LineALastIdx},
                {CharCntA,LastCharIdxA,PostWsA}, TabSize, State#ppState.lines),
            % Update the minimal/maximal line index
            LineIdxs = ?MISC:ets_keys(State#ppState.lines),
            ets:insert(State#ppState.etc, {minLineIdx, hd(LineIdxs)}),
            ets:insert(State#ppState.etc, {maxLineIdx, lists:last(LineIdxs)});
        true -> ok
    end,
    % Return
    {LineIdxA,LastCharIdxA,CharCntA,PostWsA};
calc_indices_tokrec([{IdxB,TokenB,InfoB}|TokRecs],
        {LineIdxA,LastCharIdxA,CharCntA,PostWsA}, {LineAFirstIdx,LineALastIdx},
        {SetToken, SetLine}, TabSize, State=#ppState{})
        when is_integer(LineIdxA), is_integer(LastCharIdxA), is_list(PostWsA),
        is_boolean(SetToken), is_boolean(SetLine) ->
    % Get token indices
    {LineIdxB, {FirstCharIdxB,LastCharIdxB}, CharCntB, PostWsB} =
        get_token_indices({LineIdxA, LastCharIdxA, CharCntA, PostWsA},
                          InfoB, TabSize),
    % Calc line indices
    {LineBFirstIdx, LineBLastIdx} = if
        LineIdxA/=LineIdxB -> {IdxB,          IdxB};
        true ->               {LineAFirstIdx, IdxB}
    end,
    % Set token indices
    if
        SetToken ->
            InfoB2 = InfoB#tokenInfo{charIdx={FirstCharIdxB, LastCharIdxB},
                                     lineCharCnt=CharCntB},
            ets:insert(State#ppState.tokens, {IdxB, TokenB, InfoB2});
        true -> ok
    end,
    % Set line indices
    if
        SetLine andalso LineIdxA/=LineIdxB ->
            calc_indices_tokrec_line({LineIdxA,LineAFirstIdx,LineALastIdx},
                {CharCntA,LastCharIdxA,PostWsA}, TabSize, State#ppState.lines);
        true -> ok
    end,
    % Recursion
    calc_indices_tokrec(TokRecs, {LineIdxB,LastCharIdxB,CharCntB,PostWsB},
        {LineBFirstIdx, LineBLastIdx}, {SetToken,SetLine}, TabSize, State).


% Calculate textual indices of a line and store those
calc_indices_tokrec_line({LineIdx, LineFirstIdx, LineLastIdx},
        {CharCnt, LastCharIdx, LastPostWs}, TabSize, LinesETS) ->
    % Calc the line length
    {Parts,_} = ?MISC:string_split(LastPostWs,?MISC:string_EOLs(),1,true,false),
    LineEndCharIdx = get_last_charIdx(LastCharIdx, hd(Parts), TabSize),
    % Insert line record
    ets:insert(LinesETS, {LineIdx,
        #lineInfo{tokenFirst=LineFirstIdx, tokenLast=LineLastIdx,
            charCnt=CharCnt, lengthVisible=LastCharIdx,
            lengthTotal=LineEndCharIdx}}).


% Calculate textual indices of a token
get_token_indices({LineIdxA,LastCharIdxA,CharCntA,PostWsA}, TokenInfoB,
        TabSize) ->
    % Calculate the line index of TokenB
    DataBdata = (TokenInfoB#tokenInfo.data)#lex.data,
    PreWsB  = DataBdata#token.prews,
    TextB   = DataBdata#token.text,
    PostWsB = DataBdata#token.postws,
    {Parts,NlCount} = ?MISC:string_lines(PostWsA++PreWsB),
    TextLengthB = length(TextB),
    {LineIdxB,CharIdxB0,CharCntB} = if
            % continue the current line
            0==NlCount -> {LineIdxA, LastCharIdxA, CharCntA+TextLengthB};
            % start a new line
            true -> {LineIdxA+NlCount, 0, TextLengthB}
        end,
    % Calculate inedex of last visible character of TokenB
    FirstCharIdxB = get_last_charIdx(CharIdxB0, lists:last(Parts), TabSize) + 1,
    LastCharIdxB  = FirstCharIdxB + TextLengthB - 1,
    % Return the indices
    {LineIdxB, {FirstCharIdxB,LastCharIdxB}, CharCntB, PostWsB}.


% Get the textual index of the last character of a string
get_last_charIdx(PrevCharIdx, String, TabSize) ->
    lists:foldl(
        fun(9,ChIdx) -> ChIdx + (TabSize-(ChIdx rem TabSize));
        (_,ChIdx)    -> ChIdx+1
        end,
        PrevCharIdx,
        String).






%%% ============================================================================
%%% Indentation downpropagation and token collections

% Down propagate the indentation informations in the syntax tree and collect
% the token formatting rules
downProp(Node1, Node2, Config=#ppConfig{}, State=#ppState{}) ->
    % Check syntax subtree and get the first/last token
    {{Node1RealFirst,Node2RealLast}, {HasPrevLine, HasNextLine},
     {PathDown1,PathDown2}, FileNode} = check_subtree(Node1, Node2),
    % Set initial values for the Pretty Printer
    State1 = State#ppState{tokenFrom=Node1RealFirst, tokenTo=Node2RealLast,
                           hasPrevLine=HasPrevLine, hasNextLine=HasNextLine},
    % Down propagate the indentation informations
    downProp_({downProp_nodeInfo(FileNode), 0}, {PathDown1,PathDown2},
              {path,path}, 0, Config,State1),
    % Check macro substituations
    % case get_part_substs(State1#ppState.subst) of
        % [] -> ok;
        % Wrongs -> throw(?LocalError(part_macro, Wrongs))
    % end,
    % Get the indices of first and last token where need to modify spaces
    Idx1 = case ets:lookup(State1#ppState.etc, tokenFrom) of
        [{tokenFrom,I1}] -> I1;
        _                -> 1
    end,
    Idx2 = case ets:lookup(State1#ppState.etc, tokenTo) of
        [{tokenTo,I2}] -> I2;
        _              -> ets:info(State#ppState.tokens, size)
    end,
    % Calculate the indices of the firdt/last tokens of the multiple line terms
    calc_multi_indices(State1),
    % Return the state
    State1#ppState{tokenFrom=Idx1, tokenTo=Idx2}.


% Check nodes are syntax nodes and in the same syntax subtree and
% give back the first and last token of selected syntax subtree
check_subtree(Node1, Node2) ->
    case lists:member(?Graph:class(Node1), [clause,expr,file,form,lex]) andalso
         lists:member(?Graph:class(Node2), [clause,expr,file,form,lex]) andalso
         (?Graph:root() /= element(1,?GR_UTILS:top_node(Node1,Node2))) of
        false -> throw(?LocalError(not_syntree_nodes,[Node1,Node2]));
        _     -> ok
    end,
    % Find first and last token
    Node1RealFirst = realtoken_first(Node1),
    Node2RealLast  = realtoken_last(Node2),
    case Node1RealFirst==no orelse ?Graph:class(Node1RealFirst)/=lex orelse
         Node2RealLast==no  orelse ?Graph:class(Node2RealLast)/=lex  of
        true ->
            throw(?LocalError(not_token_leaves,[Node1RealFirst,Node2RealLast]));
        _    -> ok
    end,
    % Find the syntax tree part of the real tokens
    {HasPrevLine, RealFirstToken} = case reallineBegin(Node1RealFirst, 0) of
        {RealLnFirst,  no,             _} -> {false, RealLnFirst   };
        {_RealLnFirst, PrevRealLnLast, _} -> {true,  PrevRealLnLast}
    end,
    {HasNextLine, RealLastToken} = case reallineEnd(Node2RealLast, 0) of
        {RealLnLast,  no,              _} -> {false, RealLnLast     };
        {_RealLnLast, NextRealLnFirst, _} -> {true,  NextRealLnFirst}
    end,
    % Calc the boundary down pathes from the root
    PathDown1  = ?Syn:root_path(RealFirstToken, left),
    PathDown2  = ?Syn:root_path(RealLastToken, right),
    FileNode   = element(2,hd(PathDown1)),
    PathDown1B = path_down(tl(PathDown1), FileNode, []),
    PathDown2B = path_down(tl(PathDown2), FileNode, []),
    % Return
    {{Node1RealFirst, Node2RealLast}, {HasPrevLine, HasNextLine},
     {PathDown1B, PathDown2B}, FileNode}.


% Make down path expression from the path returned by referl_syntax:root_path/2
path_down([], _ParentNode, PathDown) -> lists:reverse(PathDown);
path_down([{Tag,ChildNode}|Path], ParentNode, PathDown) ->
    Idx = ?Graph:index(ParentNode,Tag,ChildNode),
    path_down(Path, ChildNode, [{Tag,Idx}|PathDown]).


% Find the partially subsituated macros
% get_part_substs(SubStTableID) ->
    % ets:foldl(fun({SubStNode, TokCnt, FoundTok}, Wrongs) ->
            % if
                % FoundTok/=TokCnt ->
                    % [{SubStNode,(?Graph:data(SubStNode))#lex.data,
                      % TokCnt,FoundTok} | Wrongs];
                % true -> Wrongs
            % end
        % end,
        % [], SubStTableID).


% Return informations about `Node' to the following investigations.
downProp_nodeInfo(Node) ->
    {Node, ?Graph:class(Node), ?Graph:data(Node)}.


% Down propagate the indentation informations in the syntax tree and collect
% the token formatting rules
downProp_({{Parent,ParentClass,ParentData},ParLevel},
        {PathDown1,PathDown2}, {Begin,End}, SynLevel,
        Config=#ppConfig{indent=InRules, ws_nl=WsNlRules, longNl=LongRules,
        multiNl=MultiRules}, State=#ppState{subst=SubStETS}) ->
    % Determine the childerns where  must downpropagate the indentation
    % informations
    ChildTNIs   = ?Syn:children_idx(Parent),
    ChildLength = length(ChildTNIs),
    Idx1 =
        ?PPR:find_child_path(Begin,PathDown1,Parent,{ChildTNIs,ChildLength},1),
    Idx2 =
        ?PPR:find_child_path(End,PathDown2,Parent,{ChildTNIs,ChildLength},Idx1),
    % Downpropagate the indentation informations
    ChildTNIIs = ?PPR:get_indent({{Parent,ParentClass,ParentData},ParLevel},
                                 {ChildTNIs,ChildLength}, InRules),
    % Cut required part
    DownChildTNIIs = lists:sublist(ChildTNIIs, Idx1, Idx2-Idx1+1),
    % Create parameters to recursive calls
    PathDown1tail = case PathDown1     of [] -> [];    _  -> tl(PathDown1) end,
    PathDown2tail = case PathDown2     of [] -> [];    _  -> tl(PathDown2) end,
    Begin_        = case PathDown1tail of [] -> first; _  -> Begin         end,
    End_          = case PathDown2tail of [] -> last;  _  -> End           end,
    DownModes = case length(DownChildTNIIs) of
        0 -> [];
        1 -> [{Begin_,End_}];
        2 -> [{Begin_,last},{first,End_}];
        N -> [{Begin_,last}|lists:duplicate(N-2,{first,last})]++[{first,End_}]
    end,
    % Propagate down the indentations recursivelly
    lists:foldl(
        fun({_Tag,Child,_,ChildInd}, [Mode|Modes]) ->
            ChildInfo = downProp_nodeInfo(Child),
            downProp_tok_rules({Parent,ParentClass,ParentData},
                {ChildInfo,ChildInd}, SynLevel,
                {WsNlRules, LongRules, MultiRules}, State),
            case downProp_down(ChildInfo, SubStETS) of
                true ->
                    downProp_({ChildInfo, ChildInd#indent.level},
                        {PathDown1tail,PathDown2tail}, Mode,
                        SynLevel+1, Config, State);
                _ -> ok
            end,
            Modes
        end,
        DownModes,
        DownChildTNIIs).


% If the token is a macro substituation and it is founded first go into
% recursion otherwise do not.
downProp_down({Child,lex,ChildData}, SubStETS) ->
    % If the token is a macro substituation and ...
    case lists:member(ChildData#lex.type, [subst, incl]) of
        true ->
            case ets:member(SubStETS, Child) of
                % ... it is already founded just increase the counter
                true ->
                    % ets:update_counter(SubStETS, Child, {3,1}),
                    false;
                % ... it is founded first go into recursion
                false ->
                    % TokCnt = length(?Graph:path(Child, [{llex,back}])),
                    % ets:insert(SubStETS, {Child,TokCnt,1}),
                    ets:insert(SubStETS, {Child,undefined,1}),
                    true
            end;
        false -> [] /= ?Graph:path(Child, [llex])
    end;
downProp_down({_Child,_ChildClass,_ChildData}, _SubStETS) ->
    true.


% Find the token formatting rules for the token and store the relevant
% informations about the syntactical envinronment of the token
downProp_tok_rules({Parent,ParentClass,ParentData},
        {{Child,ChildClass,ChildData},ChildInd}, SynLevel,
        {WsNlRules, LongNlRules, MultiNlRules},
        #ppState{tokens=TokensETS, tokenFrom=TokenFrom, tokenTo=TokenTo,
                 etc=EtcETS, longNl=LongETS, multiNl=MultiETS}) ->
    % If the node is a real token ...
    if
        lex==ChildClass andalso token==(ChildData#lex.type) andalso
        virtual/=(ChildData#lex.data) ->
            % get the diffrent rules of token
            WR = ?PPR:get_token_rules({Parent,ParentClass,ParentData},
                        {Child,ChildClass,ChildData}, WsNlRules),
            LR = ?PPR:get_token_rules({Parent,ParentClass,ParentData},
                        {Child,ChildClass,ChildData}, LongNlRules),
            MR = ?PPR:get_token_rules({Parent,ParentClass,ParentData},
                        {Child,ChildClass,ChildData}, MultiNlRules),
            % ... add real tokens to the list with indent and rules...
            ChildIdx = 1 + ets:info(TokensETS, size),
            ets:insert(TokensETS, {ChildIdx,Child,
                       #tokenInfo{indent=ChildInd, rules=WR, data=ChildData}}),
            % ... and find the index of the first and last real token ...
            if
                Child==TokenFrom -> ets:insert(EtcETS, {tokenFrom, ChildIdx});
                true -> ok
            end,
            if
                Child==TokenTo -> ets:insert(EtcETS, {tokenTo, ChildIdx});
                true -> ok
            end,
            % ... and add line breaker rules
            if
                []/=LR -> ets:insert(LongETS, {ChildIdx,SynLevel,LR});
                true   -> ok
            end,
            % ... and add line breaker rules for multiple line terms
            if
                []/=MR ->
                    RF = realtoken_first(Parent),
                    RL = realtoken_last(Parent),
                    ets:insert(MultiETS, {ChildIdx,
                            #multiInfo{rules=MR, tokenFirst=RF, tokenLast=RL}});
                true -> ok
            end;
        true -> ok
    end.






%%% ============================================================================
%%% Token operations



%% @spec realtoken_first(Node::node()) -> node() | no
%% @doc  First real token of `Node'.
realtoken_first(Node) ->
    realtoken_down_(Node, fun erlang:hd/1).

%% @spec realtoken_last(Node::node()) -> node() | no
%% @doc  Last real token of `Node'.
realtoken_last(Node) ->
    realtoken_down_(Node, fun lists:last/1).

%% @doc Implementation function for `realtoken_first/1' and `realtoken_last/1'.
%% @todo Dialyzer claims that this function will never return `no'.
%%       Is this an error in the algorithm?
realtoken_down_(Node, DownFun) ->
    Class = ?Graph:class(Node),
    case lists:member(Class, [file,form,clause,typexp,expr,lex]) of
        false -> throw(?LocalError(not_has_tokens, [Node]));
        true  ->
            case ?Syn:children(Node) of
                [] when Class/=lex ->
                    throw(?LocalError(not_has_tokens, [Node]));
                [] -> Node;
                Childs -> realtoken_down_(element(2,DownFun(Childs)), DownFun)
            end
    end.


%% @spec realtoken_next(Node::node()) -> node() | no
%% @doc  First real token after the tokens of `Node'.
realtoken_next(Node) ->
    realtoken_neighbour(Node, fun(L) when is_list(L) -> L end,
                         fun realtoken_first/1).

%% @spec realtoken_prev(Node::node()) -> node() | no
%% @doc  Last realtoken before the tokens of `Node'.
realtoken_prev(Node) ->
    realtoken_neighbour(Node, fun lists:reverse/1, fun realtoken_last/1).

% Implementation function for realtoken_next/1 and realtoken_prev/1 functions.
realtoken_neighbour(Node, DirFun, DownFun) ->
    case lists:member(?Graph:class(Node), [clause,expr,form,typexp,lex]) of
        false -> no;
        _ ->
            case ?Syn:parent(Node) of
                [] -> no;
                [{_,Parent}] ->
                    case lists:dropwhile(fun({_T,N}) -> N/=Node end,
                                         DirFun(?Syn:children(Parent))) of
                        [{_,Node},{_,NextNode}|_] -> DownFun(NextNode);
                        _ -> realtoken_neighbour(Parent, DirFun, DownFun)
                    end;
                Parents ->
                    realtoken_neighbour_(Parents,DownFun(Node),DirFun,DownFun)
            end
    end.

% Implementation helper function for realtoken_neighbour/3
realtoken_neighbour_([],                  _FirstLeaf,_DirFun,_DownFun) -> no;
realtoken_neighbour_([{_,Parent}|Parents], FirstLeaf, DirFun, DownFun) ->
    case realtoken_neighbour(Parent, DirFun, DownFun) of
    %case DownFun(Parent) of
        FirstLeaf -> realtoken_neighbour_(Parents, FirstLeaf, DirFun, DownFun);
        NextLeaf  -> NextLeaf
    end.




%% -----------------------------------------------------------------------------
%% Token lines


% Collect the real tokens in the real line before the `Token'.
reallineBegin(Token, CollectFrom) ->
    xlnxtok(Token, fun realtoken_prev/1, fun(L) when is_list(L) -> L end,
            CollectFrom, 1, []).

% Collect the real tokens in the real line after the `Token'.
reallineEnd(Token, CollectFrom) ->
    xlnxtok(Token, fun realtoken_next/1, fun lists:reverse/1,CollectFrom,1,[]).

% Implementing function for lineBegin/2 and lineEnd/2
xlnxtok(Token, NeighFun, DirFun, CollectFrom, Count, Line) ->
    NeighToken = NeighFun(Token),
    [Token1,Token2] = DirFun([NeighToken,Token]),
    {Idx,_} = ?MISC:string_strs(ws_between(Token1,Token2), ?MISC:string_EOLs()),
    % If we reach the first token, that we need to collect, collect it.
    NewLine = if
        0<CollectFrom andalso CollectFrom=<Count -> [Token|Line];
        true                                     -> Line
    end,
    % If we are still in same line check the next token else we are done.
    if
        no/=NeighToken andalso 0==Idx ->
            xlnxtok(NeighToken,NeighFun, DirFun, CollectFrom, Count+1, NewLine);
        true ->
            {Token, NeighToken, DirFun(NewLine)}
    end.


%% @spec ws_between(Token1::node(), Token2::node()) -> string()
%% @doc  White spaces between `Token1' and `Token2'. `Token2' must follow
%%       `Token1'.
ws_between(Token1, Token2) ->
    Ws1 = case Token1 of
        no -> "";
        _  -> ((?Graph:data(Token1))#lex.data)#token.postws
    end,
    Ws2 = case Token2 of
        no -> "";
        _  -> ((?Graph:data(Token2))#lex.data)#token.prews
    end,
    Ws1 ++ Ws2.


%% This function was introduced to change lists:split/2 at some place places
%% where an empty list argument could occur.
split(_, []) -> {[], [[]]};
split(Length, Parts) -> lists:split(Length, Parts).
