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

%%% ============================================================================
%%% Module information

%%% @doc
%%% Pretty Printer to format the modified and generated Erlang source code.
%%% It can modify the spaces between tokens, break and indent lines for
%%% different syntactical structures, break long lines and multiple line terms.
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(referl_pp).
-vsn("$Rev: $").
-include("refactorerl.hrl").
-include("referl_pp_types.hrl").

-define(PPR, referl_pp_rules).



%%% ============================================================================
%%% Exports/imports

% Error messages
-export([error_text/2]).
% Formatter functions
-export([format/4, format_default/0, format_validator/0]).


-export([trim_lines/2, trim_lines/4, is_comment_lines/1]).




%%% ============================================================================
%%% Types and type functions



%% @ type indent().
%% Indentation informations.
-record(indent, {
    level,          % Level in the syntax tree
    diff,           % Indentation different between the parent en this node
    str             % Indentation string on the line begin (optional)
    }).

%% @ type tokenInfo().
%% Informations about a token.
-record(tokenInfo, {
    indent,         % Indentation record (see below)
    charIdx,        % Pair: Index of first and last visible character
    lineCharCnt,    % Number of caharcters in the line to this token
    rules = []      % Formating rules
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


%% @ type ppState().
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
%% @doc  Create a new ppState record.
create_ppState() ->
    #ppState{
        etc      = ets:new(pp_etc,     []),
        tokens   = ets:new(pp_tokens,  [ordered_set]),
        longNl   = ets:new(pp_longNl,  []),
        multiNl  = ets:new(pp_multiNl, [ordered_set]),
        subst    = ets:new(pp_subst,   []),
        lines    = ets:new(pp_lines,   [ordered_set])}.

%% @private
%% @spec delete_ppState(State::ppState()) -> ok
%% @doc  Delete all ets tables from a ppState record.
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
    ?MISC:format("The following macro subsituations({Node, macro name}) are "
        "partial in the selcted part: ~p.", Wrongs);
error_text(not_has_tokens, [Node]) ->
    ?MISC:format("There is no order for the tokens of ~p node or "
        "it doesn't have tokens.", [Node]);
error_text(invalid_options, Wrongs) ->
    ?MISC:format("The following format options are invalid({Key, Value}): ~p",
        Wrongs).






%%% ============================================================================
%%% Miscellaneous functions



%% @spec proplist_merge_def(List::proplist(), DefList::proplist()) ->
%%           proplist()
%% @doc  Add missing key value pair from `DefList' to `List'.
proplist_merge_def(List, DefList) when is_list(List), is_list(DefList) ->
    List ++ ?MISC:list_substract(DefList, List,
        fun({Type,_},{Type,_}) -> true; ({_,_},{_,_}) -> false end).


%% @spec proplist_validate(List::proplist(), ValidatorList::ValidatorProplist) ->
%%           WrongPairs::proplist()
%%       ValidatorProplist = [{Key::atom(),
%%                             ValidatorFun::((Value::term()) -> boolean())}]
%% @doc  Validate the values of keys in the `List' by the validator functions
%%       given in `ValidatorList'.
proplist_validate(List,ValidatorList) when is_list(List),
        is_list(ValidatorList) ->
    lists:reverse(lists:foldl(
        fun({Key,Value}, Wrongs) ->
            case proplists:get_value(Key, ValidatorList) of
                Fun when is_function(Fun) ->
                    case Fun(Value) of
                        true -> Wrongs;
                        _    -> [{Key,Value}|Wrongs]
                    end;
                undefined -> [{Key,Value}|Wrongs]
            end
        end,
        [],
        List)).


%% @spec ets_keys(EtsTableID::integer()) -> [term()]
%% @doc  Returns the keys of the ETS table identified by `EtsTableID'.
%%       If the type ot the ETS table is `ordered_set' than list is also
%%       ordered. In other cases the order is undefined it depending on the
%%       storage order.
ets_keys(EtsTableID) ->
    ets:select(EtsTableID,
        [{'_', [], [{element,ets:info(EtsTableID,keypos),'$_'}]}]).






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
%% </ul>
%% In general the values `false' and `no' are same.
format_validator() ->
    [{indent,    fun(V) -> lists:member(V, [check,reset,false,no]) end},
     {newline,   fun(V) -> lists:member(V, [check,      false,no]) end},
     {space,     fun(V) -> lists:member(V, [check,reformat,reset,false,no])end},
     {longline,  fun is_boolean/1},
     {multiline, fun is_boolean/1}].


%% @spec format(Node1::node(), Node2::node(), Options::proplist(),
%%               Config::ppConfig()) -> ok
%% @throws {ModuleName::atom, ErrorCode::atom(), ErrorDetails::[term()]}
%% @doc  Format a part of source Erlang code. The part that should be formatted
%%       is specified by `Node1' and `Node2' syntactical nodes as the first and
%%       last node. The line prefix before the first token of `Node1', the
%%       line postfix after the last token of the `Node2', the previous line
%%       last and the next line first tokens are also will be formatted. But
%%       the required spaces and recommended line breakers will be bodified
%%       just on the `Node1'..`Node2' interval.
%%
%%       The `Options' direct the formatting method. For the legal oprtions see
%%       {@link format_validator/0}. {@link format_default/0} returns the
%%       default options.
%%
%%       The `Config' contains the formatiing rules and configurations.
%%       For Erlang language the {@link referl_pp_rules:erlang/0} function
%%       supply the default configurations.
%% @see format_default/0
%% @see format_validator/0
%% @see referl_pp_rules:erlang/0
format(Node1, Node2, Options, Config=#ppConfig{general=ConfigGen}) when
        is_list(Options) ->
    % Check the options
    Options1 = proplist_merge_def(Options, format_default()),
    case proplist_validate(Options1, format_validator()) of
        [] -> ok;
        Wrongs -> throw(?LocalError(invalid_options, Wrongs))
    end,
    % Get the options
    InMode      = proplists:get_value(indent,    Options1),
    NlMode      = proplists:get_value(newline,   Options1),
    SpMode      = proplists:get_value(space,     Options1),
    LongLnMode  = proplists:get_value(longline,  Options1),
    MultiLnMode = proplists:get_value(multiline, Options1),
    % Downpropagate the indentation iformations and collect real tokens
    State0 = create_ppState(),
    State = downProp(Node1, Node2, Config, State0),
    % Add spaces and new lines to the white spaces between tokens
    Idxs = ets_keys(State#ppState.tokens),
    case lists:member(SpMode, [check,reformat,reset]) orelse
            lists:member(NlMode, [check]) of
        true ->
            % Format the begin of file
            if
                1==State#ppState.tokenFrom andalso
                (not State#ppState.hasPrevLine) ->
                    [{_,TokenF,InfoF}] = ets:lookup(State#ppState.tokens, 1),
                    format_sp_nl({no,#tokenInfo{rules=[]}}, {TokenF,InfoF},
                        SpMode, NlMode, ConfigGen#ppConfig_general.nlStr,
                        ConfigGen#ppConfig_general.commentLines);
                true -> ok
            end,
            % Format the selected part
            [{tokensCount,Cnt}] = ets:lookup(State#ppState.etc, tokensCount),
            SpaceIdx1 = lists:max([1,   State#ppState.tokenFrom-1]),
            SpaceIdx2 = lists:min([Cnt, State#ppState.tokenTo+1]),
            SpaceIdxs = lists:sublist(Idxs, SpaceIdx1, SpaceIdx2-SpaceIdx1+1),
            format_space(SpaceIdxs, SpMode, NlMode,
                         ConfigGen#ppConfig_general.nlStr,
                         ConfigGen#ppConfig_general.commentLines, State),
            % Format the end of file
            if
                Cnt==State#ppState.tokenTo andalso
                (not State#ppState.hasNextLine) ->
                    [{_,TokenL,InfoL}] = ets:lookup(State#ppState.tokens, Cnt),
                    format_sp_nl({TokenL,InfoL}, {no,#tokenInfo{rules=[]}},
                        SpMode, NlMode, ConfigGen#ppConfig_general.nlStr,
                        ConfigGen#ppConfig_general.commentLines);
                true -> ok
            end;
        _ -> ok
    end,
    % Calculate initial indices
    [{_,Token1,_}] = ets:lookup(State#ppState.tokens, hd(Idxs)),
    {_,_,LineBegin0} = reallineBegin(Token1, 2),
    DummyList = lists:duplicate(length(LineBegin0),ok),
    Indices0 = calc_indices_tokrec(lists:zip3(DummyList,LineBegin0,DummyList),
        {0,0,0,""}, {0,0}, {false,false},
        ConfigGen#ppConfig_general.tabSize, State),
    calc_indices_tokrec(ets:tab2list(State#ppState.tokens), Indices0,
        {hd(Idxs),hd(Idxs)}, {true,true},
        ConfigGen#ppConfig_general.tabSize, State),
    % Set initial indentations
    case lists:member(InMode, [check,reset]) of
        true ->
            LnIdxs    = ets_keys(State#ppState.lines),
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
    % Delete state
    delete_ppState(State),
    ok.




%% -----------------------------------------------------------------------------
%% Multiple line terms and long lines


%%
%%
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

%
format_ml_(InMode, ConfigGen=#ppConfig_general{}, State, ModCount) ->
    case format_multi(InMode, ConfigGen, State) of
        [] -> ModCount;
        _  -> format_ml_(InMode, ConfigGen, State, ModCount+1)
    end.

%
format_ll_(InMode, ConfigGen=#ppConfig_general{}, State, ModCount) ->
    case format_long(InMode, ConfigGen, State) of
        [] -> ModCount;
        _  -> format_ll_(InMode, ConfigGen, State, ModCount+1)
    end.


%%
%%
format_multi(InMode, ConfigGen=#ppConfig_general{},
        State=#ppState{etc=EtcETS, lines=LinesETS, multiNl=MultiETS}) ->
    % Find the real multiple line terms
    [{minLineIdx, MinLineIdx}] = ets:lookup(EtcETS, minLineIdx),
    [{maxLineIdx, MaxLineIdx}] = ets:lookup(EtcETS, maxLineIdx),
    Breaks = ets:foldl(
        fun({Idx, #multiInfo{tokenFirst=Idx1,tokenLast=Idx2, rules=PlusRules}},
                AccBreaks) ->
            LineIdx1 = get_token_line(Idx1, MinLineIdx, LinesETS),
            LineIdx2 = get_token_line(Idx2, MaxLineIdx, LinesETS),
            if
                LineIdx1/=LineIdx2 -> [{Idx,PlusRules}|AccBreaks];
                true               -> AccBreaks
            end
        end,
        [], MultiETS),
    % Apply extra rules and return the indices of the modified lines
    lists:foldl(fun({Idx,PlusRules}, ModLineIdxs) ->
            ets:delete(MultiETS, Idx),    % It is processed now
            {PlusDiff, ModLnIdxs} = format_plusNl(Idx, PlusRules, InMode,
                ConfigGen, State),
            ModLineIdxs2 = lists:map(fun(Id) -> Id+PlusDiff end, ModLineIdxs),
            ModLnIdxs++ModLineIdxs2
        end,
        [], Breaks).


% Get the available line breakers in priority order
% Two level order:
%   - Level 1: token is better if it is in a higher position in the syntax tree
%   - Level 2: token is better if it closer to the line end
% PrioBreaks=[{Priority,TokIdx,Rules}], Priority={Syntax level, LineEnd-TokIdx}
format_long(InMode, ConfigGen=#ppConfig_general{optLineLength=OptLineLength,
        minTextChars=MinTextChars, maxTextChars=MaxTextChars},
        State=#ppState{tokens=TokensETS, lines=LinesETS, longNl=LongETS}) ->
    Breaks = ets:foldl(
        fun({_LnIdx, #lineInfo{tokenFirst=Idx1,tokenLast=Idx2,charCnt=CharCnt,
                lengthVisible=LengthV}}, AccBreaks) ->
            if
                (OptLineLength<LengthV andalso MinTextChars=<CharCnt) orelse
                 MaxTextChars<CharCnt ->
                    PriorBreaks = lists:sort(ets:select(LongETS,
                        [{{'$1','$2','$3'}, [{'=<',Idx1,'$1'},{'=<','$1',Idx2}],
                          [{{ {{'$2',{'-',0,'$1'}}}, '$1','$3' }}] }])),
                    OrdBreakStats = lists:map(fun({_Prior,Idx,PlusRules}) ->
                            [{Idx,_Token,Info}] = ets:lookup(TokensETS,Idx),
                            LastCharIdx = element(2,Info#tokenInfo.charIdx),
                            TokCharCnt  = Info#tokenInfo.lineCharCnt,
                            {Idx,PlusRules, {LastCharIdx,TokCharCnt}}
                        end,
                        PriorBreaks),
                    format_long_(OrdBreakStats, OptLineLength, MinTextChars,
                                 MaxTextChars, AccBreaks);
                 true ->
                    AccBreaks
            end
        end,
        [], LinesETS),
    % Apply extra rules and return the indices of the modified lines
    lists:foldl(fun({Idx,PlusRules}, ModLineIdxs) ->
            ets:delete(LongETS, Idx),    % It is processed now
            {PlusDiff, ModLnIdxs} = format_plusNl(Idx, PlusRules, InMode,
                ConfigGen, State),
            ModLineIdxs2 = lists:map(fun(Id) -> Id+PlusDiff end, ModLineIdxs),
            ModLnIdxs++ModLineIdxs2
        end,
        [], Breaks).

% Find the best long line breaker
format_long_(OrdBreakStats, OptLineLength,MinTextChars,MaxTextChars,
        AccBreaks) ->
    case ?MISC:list_find(fun({_,_, {LastCharIdx,TokCharCnt}}) ->
                LastCharIdx=<OptLineLength andalso
                MinTextChars=<TokCharCnt andalso
                TokCharCnt=<MaxTextChars
            end,
            OrdBreakStats) of
        {0,_} ->
            case ?MISC:list_find(fun({_,_, {LastCharIdx,_}}) ->
                    LastCharIdx=<OptLineLength end, OrdBreakStats) of
                {0,_} ->
                    case OrdBreakStats of
                        [] -> AccBreaks;
                        _  ->
                            {Idx,PlusRules,_} = lists:min(OrdBreakStats),
                            [{Idx,PlusRules} | AccBreaks]
                    end;
                {_, {Idx,PlusRules,_}} -> [{Idx,PlusRules} | AccBreaks]
            end;
        {_, {Idx,PlusRules,_}} -> [{Idx,PlusRules} | AccBreaks]
    end.



%%
%%
format_plusNl(IdxB, Rules, InMode, #ppConfig_general{useTab=UseTab,
        tabSize=TabSize, nlStr=NlStr, commentLines=CmLns},
        State=#ppState{tokens=TokensETS, lines=LinesETS}) ->
    % Add extra rules to the originals
    [{IdxB, TokenB, InfoB}] = ets:lookup(TokensETS, IdxB),
    InfoB2 = InfoB#tokenInfo{rules=?PPR:merge_token_rules(InfoB#tokenInfo.rules,
                                                          Rules)},
    ets:insert(TokensETS, {IdxB,TokenB,InfoB2}),
    % Get previous/next tokens and information about lines
    [{tokensCount, TokensCnt}] = ets:lookup(State#ppState.etc, tokensCount),
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
    [{_, TokenA, InfoA}] = ets:lookup(TokensETS, IdxA),
    LineDiffAB2 = if
        IdxA<IdxB ->
            format_sp_nl({TokenA,InfoA},{TokenB,InfoB2},no,check,NlStr,CmLns);
        true -> 0
    end,
    LineDiffBC2 = if
        IdxB<IdxC ->
            [{_, TokenC, InfoC}] = ets:lookup(TokensETS, IdxC),
            format_sp_nl({TokenB,InfoB2},{TokenC,InfoC},no,check,NlStr,CmLns);
        true -> 0
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
            ModLineIdxs = ets:select(LinesETS, [{{'$1','_'},
                [{'<',LineIdxA1,'$1'},
                 {'=<','$1',LineIdxA1+LineDiffAB2+LineDiffBC2}],
                ['$1']}]),
            % Indent modified lines
            case lists:member(InMode, [check,reset]) of
                true -> format_indent(ModLineIdxs,InMode,UseTab,TabSize,State);
                _    -> ok
            end,
            {PlusDiff, ModLineIdxs}
    end.




%% -----------------------------------------------------------------------------
%% Indentation


%%
%%
format_indent([], _InMode, _UseTab, _TabSize, _State) -> ok;
format_indent([LineIdx|LineIdxs], InMode, UseTab, TabSize,
        State=#ppState{tokens=TokensETS, lines=LinesETS}) ->
    % Get the First token of line
    [{LineIdx, #lineInfo{tokenFirst=LineFirstIdx, tokenLast=LineLastIdx}}] =
        ets:lookup(LinesETS,  LineIdx),
    [{LineFirstIdx,Token1,Info1}] = ets:lookup(TokensETS, LineFirstIdx),
    % Get current indentation
    LexData1 = ?Graph:data(Token1),
    TokData1 = LexData1#lex.data,
    {Parts1,_} = ?MISC:string_lines(TokData1#token.prews),
    {PreWsFirstLines, [PreWsLastLine]} = lists:split(length(Parts1)-1,Parts1),
    CurrInSize1 = get_last_charIdx(0, PreWsLastLine, TabSize),
    % Get target indentation
    InLevel1 = (Info1#tokenInfo.indent)#indent.level,
    InSize1  = InLevel1*TabSize,
    % Modify indentation
    if
        (check==InMode andalso CurrInSize1<InSize1) orelse
         reset==InMode ->
            % Indent prews
            PreWsLastLine2 = if
                UseTab -> lists:duplicate(InLevel1, 9);
                true   -> lists:duplicate(InSize1, 32)
            end,
            PreWsFirstLines2 = lists:map(
                fun(Ln) -> PreWsLastLine2 ++ ?MISC:string_trim(Ln,left) end,
                PreWsFirstLines),
            % Update prews
            TokData1B = TokData1#token{prews =
                lists:flatten(PreWsFirstLines2++[PreWsLastLine2])},
            LexData1B = LexData1#lex{data = TokData1B},
            ?Graph:update(Token1, LexData1B),
            % Indent postws
            [{LineLastIdx,Token2,_Info2}] = ets:lookup(TokensETS, LineLastIdx),
            LexData2 = ?Graph:data(Token2),
            TokData2 = LexData2#lex.data,
            {Parts2,_} = ?MISC:string_lines(TokData2#token.postws),
            {[PostWsFirstLine], PostWsLastLines} = lists:split(1,Parts2),
            PostWsLastLines2 = lists:map(fun(Ln) -> 
					case ?MISC:string_strs(Ln, ?MISC:string_EOLs()) of
						{0,_} -> Ln;
						{_,_} -> PreWsLastLine2 ++ ?MISC:string_trim(Ln,left)
					end
				end,
                PostWsLastLines),
            % Update postws
            TokData2B = TokData2#token{postws =
                lists:flatten([PostWsFirstLine|PostWsLastLines2])},
            LexData2B = LexData2#lex{data = TokData2B},
            ?Graph:update(Token2, LexData2B),
            % Update token indices
            {_,{FirstCharIdx,LastCharIdx},CharCnt,_} =
                get_token_indices({0, 0, 0, ""}, Token1, TabSize),
            Info1B = Info1#tokenInfo{charIdx={FirstCharIdx,LastCharIdx},
                                   lineCharCnt=CharCnt},
            ets:insert(TokensETS, {LineFirstIdx, Token1, Info1B}),
            % Update the other token indices in the line
            recalc_indices_byfirst({LineFirstIdx,LineLastIdx},
                {LineIdx,LineFirstIdx}, TabSize, State);
        true -> ok
    end,
    % Recursion
    format_indent(LineIdxs, InMode, UseTab, TabSize, State).




%% -----------------------------------------------------------------------------
%% Spaces and new lines: set spaces and required line breakers between tokens


%%
%%
format_space(Idxs, SpMode, NlMode, NlStr, CommentLines, State=#ppState{}) ->
    [{_,Token1,Info1}] = ets:lookup(State#ppState.tokens, hd(Idxs)),
    lists:foldl(
        fun(IdxB, {TokenA,InfoA}) ->
            [{IdxB,TokenB,InfoB}] = ets:lookup(State#ppState.tokens, IdxB),
            format_sp_nl({TokenA,InfoA}, {TokenB,InfoB}, SpMode, NlMode,
                         NlStr, CommentLines),
            {TokenB,InfoB}
        end,
        {Token1,Info1},
        tl(Idxs)).


%%
%%
format_sp_nl({TokenA,InfoA}, {TokenB,InfoB}, SpMode, NlMode,
        NlStr, _CommentLines) ->
    % Get the newline rule value between tokens
    RulesA = InfoA#tokenInfo.rules,
    RulesB = InfoB#tokenInfo.rules,
    {WsMax,_,WsA,WsB} = ?PPR:get_token_rule_between(ws, RulesA, RulesB),
    {NlMax,_,_,  _  } = ?PPR:get_token_rule_between(nl, RulesA, RulesB),
    % Get the white space between tokens
    {LinesA,LinesB, NlCount,WsLen} = case SpMode of
            reset    -> split_tokens_ws("", WsA, WsB, fun(Lns) -> Lns end);
            reformat -> split_tokens_ws(ws_between(TokenA, TokenB), WsA, WsB,
                                        fun(Lns) -> trim_lines(Lns, both) end);
            _        -> split_tokens_ws(ws_between(TokenA, TokenB), WsA, WsB,
                                        fun(Lns) -> Lns end)
        end,
    % Broke the current white spaces into lines
    {FirstLinesA, [LastLineA]} = lists:split(length(LinesA)-1,LinesA),
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
            WsLenA = round(WsA/WsMax*(WsMax-WsLen)),
            WsLenB = (WsMax-WsLen)-WsLenA,
            {FirstLinesA++[LastLineA++lists:duplicate(WsLenA,32)],
             [lists:duplicate(WsLenB,32)++FirstLineB|LastLinesB],
             NlCount};
        % Not change
        true ->
            {LinesA, LinesB, NlCount}
    end,
    % Updete tokens white spaces
    update_tokens_ws(TokenA, remove_lines1_ends(LinesA1),
                     TokenB, remove_lines_ends(LinesB1)),
    % Return difference between the line numbers of tokens
    LineNumDiff.




%%% ----------------------------------------------------------------------------
%%% White space partitions

%%
%%
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

%%
%%
remove_lines_ends(Lines) -> trim_lines(Lines, right, true, false).


%%
%%
is_comment_lines(Lines) when is_list(Lines) ->
    lists:map(fun(TrimLn) -> ""=/=TrimLn end,
              trim_lines(Lines, both, false, true)).

%%
%%
trim_lines(Lines, Mode) -> trim_lines(Lines, Mode, true, true).

%%
%%
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


%%
%%
split_tokens_ws(String, WsVal1, WsVal2, LinesFun) when is_list(String),
        is_integer(WsVal1), 0=<WsVal1, is_integer(WsVal2), 0=<WsVal2,
        is_function(LinesFun) ->
    {Lines0, NlCount} = ?MISC:string_lines(String),
    Lines = LinesFun(Lines0),
    WsLen = lists:foldl(fun(Ln, Acc) -> Acc+length(Ln) end, 0, Lines),
    {LinesA, LinesB} = case length(Lines) of
        1 -> % Only spaces (Token1   Token2)
            WsLen1 = case WsVal1+WsVal2 of
                0     -> 0;
                WsSum -> round((WsVal1/WsSum)*WsLen)
            end,
            {Str1, Str2} = lists:split(WsLen1, hd(Lines)),
            {[Str1], [Str2]};
        2 -> % One one line (Token1  % comment line\n   Token2)
            {[hd(Lines)], tl(Lines)};
        _ -> % More lines (Token1 %comment1\n %comment1\n\n %comment2\n Token2)
            {L1,      LTail} = lists:split(1, Lines),
            {LMiddle, LLast} = lists:split(length(LTail)-1, LTail),
            CmLines = is_comment_lines(LMiddle),
            case ?MISC:list_find(false, lists:reverse(CmLines)) of
                {0,_} -> {L1, LMiddle++LLast};
                {N,_} ->
                    {LM1,LM2} = lists:split(length(LMiddle)-N+1,LMiddle),
                    {L1++LM1, LM2++LLast}
            end
    end,
    {LinesA,LinesB, NlCount, WsLen}.



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


%%
%%
update_tokens_ws(no, LinesA, TokenB, LinesB) when is_list(LinesA),
        is_tuple(TokenB), is_list(LinesB) ->
    LexDataB  = ?Graph:data(TokenB),
    TokDataB  = LexDataB#lex.data,
    TokDataB2 = TokDataB#token{prews=lists:flatten(LinesA++LinesB)},
    LexDataB2 = LexDataB#lex{data=TokDataB2},
    ?Graph:update(TokenB, LexDataB2);
update_tokens_ws(TokenA, LinesA, no, LinesB) when is_tuple(TokenA),
        is_list(LinesA), is_list(LinesB) ->
    LexDataA  = ?Graph:data(TokenA),
    TokDataA  = LexDataA#lex.data,
    TokDataA2 = TokDataA#token{postws=lists:flatten(LinesA++LinesB)},
    LexDataA2 = LexDataA#lex{data=TokDataA2},
    ?Graph:update(TokenA, LexDataA2);
update_tokens_ws(TokenA, LinesA, TokenB, LinesB) when is_tuple(TokenA),
        is_tuple(TokenB), is_list(LinesA), is_list(LinesB) ->
    LexDataA  = ?Graph:data(TokenA),
    TokDataA  = LexDataA#lex.data,
    TokDataA2 = TokDataA#token{postws=lists:flatten(LinesA)},
    LexDataA2 = LexDataA#lex{data=TokDataA2},
    LexDataB  = ?Graph:data(TokenB),
    TokDataB  = LexDataB#lex.data,
    TokDataB2 = TokDataB#token{prews=lists:flatten(LinesB)},
    LexDataB2 = LexDataB#lex{data=TokDataB2},
    ?Graph:update(TokenA, LexDataA2),
    ?Graph:update(TokenB, LexDataB2).




%% -----------------------------------------------------------------------------
%% Line and charcter index operations


%%
%%
get_token_line(Idx, DefaultLineIdx, LinesETS) ->
    Select = [{{'$1','$2'},
               [{'=<',{element,#lineInfo.tokenFirst,'$2'},Idx},
                {'=<',Idx, {element,#lineInfo.tokenLast,'$2'}}],
               ['$1']}],
    case ets:select(LinesETS, Select) of
        [LineIdx] -> LineIdx;
        []        -> DefaultLineIdx
    end.


%%
%%
get_token_idx(Token, DefaultIdx, TokensETS) ->
    case ets:select(TokensETS, [{{'$1','$2','_'}, [{'==','$2',{const,Token}}],
            ['$1']}]) of
        [Idx] -> Idx;
        []    -> DefaultIdx
    end.


%%
%%
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


%%
%%
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


%%
%% @ doc Recalculate the indices of the tail of line. Use the indices of the
%%       first token to calculate the indices of the other tokens in the line
recalc_indices_byfirst({Idx1,Idx2}, {LineIdx,LineFirstIdx},
        TabSize, State=#ppState{}) ->
    [{Idx1,Token1,Info1}] = ets:lookup(State#ppState.tokens, Idx1),
    LastCharIdx1 = element(2,Info1#tokenInfo.charIdx),
    LastPostWs1  = ((?Graph:data(Token1))#lex.data)#token.postws,
    CharCnt1     = Info1#tokenInfo.lineCharCnt,
    TokRecs = ets:select(State#ppState.tokens,
        [{{'$1','_','_'}, [{'<',Idx1,'$1'},{'=<','$1',Idx2}], ['$_']}]),
    calc_indices_tokrec(TokRecs, {LineIdx,LastCharIdx1,CharCnt1,LastPostWs1},
        {LineFirstIdx,Idx1}, {true,true}, TabSize, State).


%%
%% @doc  If `SetToken' is false the calculated indices won't be written back
%%       into the `State'. If `SetToken' is false `IdxB' and `InfoB' is not
%%       used.
calc_indices_tokrec([], {LineIdxA,LastCharIdxA,CharCntA,PostWsA},
        {LineAFirstIdx,LineALastIdx}, {_SetToken,SetLine}, TabSize,
        State=#ppState{}) ->
    % Set line indices
    if
        SetLine ->
            calc_indices_tokrec_line({LineIdxA,LineAFirstIdx,LineALastIdx},
                {CharCntA,LastCharIdxA,PostWsA}, TabSize, State),
            % Update the minimal/maximal line index
            LineIdxs = ets_keys(State#ppState.lines),
            ets:insert(State#ppState.etc, {minLineIdx, hd(LineIdxs)}),
            ets:insert(State#ppState.etc, {maxLineIdx, lists:last(LineIdxs)});
        true -> ok
    end,
    % Return
    {LineIdxA,LastCharIdxA,CharCntA,PostWsA};
calc_indices_tokrec([{IdxB,TokenB,InfoB}|TokRecs],
        {LineIdxA,LastCharIdxA,CharCntA,PostWsA}, {LineAFirstIdx,LineALastIdx},
        {SetToken, SetLine}, TabSize, State=#ppState{tokens=TokensETS})
        when is_integer(LineIdxA), is_integer(LastCharIdxA), is_list(PostWsA),
        is_boolean(SetToken), is_boolean(SetLine) ->
    % Get token indices
    {LineIdxB, {FirstCharIdxB,LastCharIdxB}, CharCntB, PostWsB} =
        get_token_indices({LineIdxA, LastCharIdxA, CharCntA, PostWsA},
                          TokenB, TabSize),
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
            ets:insert(TokensETS, {IdxB, TokenB, InfoB2});
        true -> ok
    end,
    % Set line indices
    if
        SetLine andalso LineIdxA/=LineIdxB ->
            calc_indices_tokrec_line({LineIdxA,LineAFirstIdx,LineALastIdx},
                {CharCntA,LastCharIdxA,PostWsA}, TabSize, State);
        true -> ok
    end,
    % Recursion
    calc_indices_tokrec(TokRecs, {LineIdxB,LastCharIdxB,CharCntB,PostWsB},
        {LineBFirstIdx, LineBLastIdx}, {SetToken,SetLine}, TabSize, State).


%%
%%
calc_indices_tokrec_line({LineIdx, LineFirstIdx, LineLastIdx},
        {CharCnt, LastCharIdx, LastPostWs}, TabSize, State=#ppState{}) ->
    % Calc the line length
    {Parts,_} = ?MISC:string_split(LastPostWs,?MISC:string_EOLs(),1,true,false),
    LineEndCharIdx = get_last_charIdx(LastCharIdx, hd(Parts), TabSize),
    % Insert line record
    ets:insert(State#ppState.lines, {LineIdx,
        #lineInfo{tokenFirst=LineFirstIdx, tokenLast=LineLastIdx,
            charCnt=CharCnt, lengthVisible=LastCharIdx,
            lengthTotal=LineEndCharIdx}}).


%%
%%
get_token_indices({LineIdxA,LastCharIdxA,CharCntA,PostWsA}, TokenB, TabSize) ->
    % Calculate the line index of TokenB
    PreWsB  = ((?Graph:data(TokenB))#lex.data)#token.prews,
    TextB   = ((?Graph:data(TokenB))#lex.data)#token.text,
    PostWsB = ((?Graph:data(TokenB))#lex.data)#token.postws,
    {Parts,NlCount} = ?MISC:string_lines(PostWsA++PreWsB),
    TextLengthB = length(TextB),
    {LineIdxB,CharIdxB0,CharCntB} = case NlCount of
            % continue the current line
            0 -> {LineIdxA,         LastCharIdxA, CharCntA+TextLengthB};
            % start a new line
            _ -> {LineIdxA+NlCount, 0,            TextLengthB}
        end,
    % Calculate inedex of last visible character of TokenB
    FirstCharIdxB = get_last_charIdx(CharIdxB0, lists:last(Parts), TabSize) + 1,
    LastCharIdxB  = FirstCharIdxB + TextLengthB - 1,
    % Return the indices
    {LineIdxB, {FirstCharIdxB,LastCharIdxB}, CharCntB, PostWsB}.


%%
%%
get_last_charIdx(PrevCharIdx, String, TabSize) ->
    lists:foldl(
        fun(9,ChIdx) -> ChIdx + (TabSize-(ChIdx rem TabSize));
        (_,ChIdx)    -> ChIdx+1
        end,
        PrevCharIdx,
        String).






%%% ============================================================================
%%% Indentation downpropagation and token collections



%%
%%
downProp(Node1, Node2, Config=#ppConfig{}, State=#ppState{}) ->
    % Check syntax subtree and get the first/last token
    {{Node1RealFirst,Node2RealLast}, {HasPrevLine, HasNextLine},
     {PathDown1,PathDown2}, FileNode} = check_subtree(Node1, Node2),
    % Set initial values for the Pretty Printer
    State1 = State#ppState{tokenFrom=Node1RealFirst, tokenTo=Node2RealLast,
                           hasPrevLine=HasPrevLine, hasNextLine=HasNextLine},
    ets:insert(State1#ppState.etc, {tokensCount, 0}),
    % Down propagate the indentation informations
    downProp_(FileNode, #indent{level=0,diff=0}, PathDown1,PathDown2,
        {path,path}, 0, Config,State1),
    % Check macro substituations
    % case get_part_substs(State1#ppState.subst) of
        % [] -> ok;
        % Wrongs -> throw(?LocalError(part_macro, Wrongs))
    % end,
    % Get the indices of first and last token where need to modify spaces
    Idx1 = case ets:lookup(State1#ppState.etc, tokenFrom) of
        [{tokenFrom,I1}] -> I1;
        _ -> 1
    end,
    [{tokensCount,Cnt}] = ets:lookup(State1#ppState.etc, tokensCount),
    Idx2 = case ets:lookup(State1#ppState.etc, tokenTo) of
        [{tokenTo,I2}] -> I2;
        _ -> Cnt
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
    % referl_draw_graph:draw_graph("ef20_5e.dot", synlex),
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
    FileNode = element(2,hd(PathDown1)),
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
%% get_part_substs(SubStTableID) ->
%%     ets:foldl(fun({SubStNode, TokCnt, FoundTok}, Wrongs) ->
%%             if
%%                 FoundTok/=TokCnt ->
%%                     [{SubStNode,(?Graph:data(SubStNode))#lex.data,
%%                       TokCnt,FoundTok} | Wrongs];
%%                 true -> Wrongs
%%             end
%%         end,
%%         [], SubStTableID).


%%
%%
downProp_(Node,NodeInd, PathDown1,PathDown2, {Begin,End}, SynLevel,
        Config=#ppConfig{}, State=#ppState{}) ->
    % Determine the childerns where  must down propagate the indentation
    % informations
    ChildTNIs = ?GR_UTILS:edges_idx(Node, forward, ?Syn:children(Node)),
    Idx1 = ?PPR:get_child_idx({Begin,PathDown1}, from, ChildTNIs, 1),
    Idx2 = ?PPR:get_child_idx({End,  PathDown2}, to,   ChildTNIs, Idx1),
    DownChildTNIs = if
        Idx1=<Idx2 andalso 0<Idx1 andalso 0<Idx2 ->
            lists:sublist(ChildTNIs, Idx1, Idx2-Idx1+1);
        true -> []
    end,
    % Create parameters for recursive calls
    PathDown1tail = case PathDown1     of [] -> [];    _  -> tl(PathDown1) end,
    PathDown2tail = case PathDown2     of [] -> [];    _  -> tl(PathDown2) end,
    Begin_        = case PathDown1tail of [] -> first; _  -> Begin         end,
    End_          = case PathDown2tail of [] -> last;  _  -> End           end,
    DownModes = case length(DownChildTNIs) of
        0 -> [];
        1 -> [{Begin_,End_}];
        2 -> [{Begin_,last},{first,End_}];
        N -> [{Begin_,last}|lists:duplicate(N-2,{first,last})]++[{first,End_}]
    end,
    % Propagate down the indentations recursivelly
    lists:foldl(
        fun({Tag,Child,_}, [Mode|Modes]) ->
            ChildInd = downProp_getChildInd(Node,NodeInd, Tag,Child,
                                            Config#ppConfig.indent),
            case downProp_pair(Node, Child,ChildInd, SynLevel,
                {Config#ppConfig.ws_nl, Config#ppConfig.longNl,
                 Config#ppConfig.multiNl}, State) of
                true ->
                    downProp_(Child,ChildInd, PathDown1tail,PathDown2tail,Mode,
                        SynLevel+1, Config, State);
                _ -> ok
            end,
            Modes
        end,
        DownModes,
        DownChildTNIs).


%%
%%
downProp_getChildInd(Parent,ParentInd, LinkTag,Child, IndentRules) ->
    % Set indentations and return true if must go into recursion on child
    Diff = ?PPR:get_indent_diff(IndentRules, Parent,Child,LinkTag),
    #indent{level=ParentInd#indent.level+Diff, diff=Diff}.


%%
%%
downProp_pair(ParentNode, ChildNode,ChildInd, SynLevel,
        {WsNlRules, LongNlRules, MultiNlRules}, State=#ppState{}) ->
    % Get child class and data
    ChildClass = ?Graph:class(ChildNode),
    ChildData  = ?Graph:data(ChildNode),
    % If the node is a real token ...
    case lex==ChildClass andalso token==(ChildData#lex.type) andalso
            virtual/=(ChildData#lex.data) of
        true ->
            % ... add real tokens to the list with indent and rules...
            [{tokensCount,TokenCnt0}] =
                ets:lookup(State#ppState.etc, tokensCount),
            ChildIdx = TokenCnt0+1,
            ets:insert(State#ppState.etc, {tokensCount, ChildIdx}),
            ets:insert(State#ppState.tokens,
                {ChildIdx,ChildNode,
                 #tokenInfo{indent=ChildInd,
                            rules=?PPR:merge_token_defrules(
                                ?PPR:get_token_rules(WsNlRules,
                                                     ChildNode, ParentNode))
                            }
                }),
            % ... and find the index of the first and last real token ...
            case ChildNode==State#ppState.tokenFrom of
                true -> ets:insert(State#ppState.etc, {tokenFrom, ChildIdx});
                _    -> ok
            end,
            case ChildNode==State#ppState.tokenTo of
                true -> ets:insert(State#ppState.etc, {tokenTo, ChildIdx});
                _    -> ok
            end,
            % ... and collect line breakers with rules
            % long line breakers
            case ?PPR:merge_token_defrules(?PPR:get_token_rules(
                    LongNlRules, ChildNode, ParentNode)) of
                [] -> ok;
                LongRules -> ets:insert(State#ppState.longNl,
                                        {ChildIdx, SynLevel, LongRules})
            end,
            % line breakers for multiple line terms
            case ?PPR:merge_token_defrules(?PPR:get_token_rules(
                    MultiNlRules, ChildNode, ParentNode)) of
                [] -> ok;
                MultiRules ->
                    RealFirst = realtoken_first(ParentNode),
                    RealLast  = realtoken_last(ParentNode),
                    ets:insert(State#ppState.multiNl,
                        {ChildIdx, #multiInfo{rules=MultiRules,
                         tokenFirst=RealFirst, tokenLast=RealLast} })
            end;
        _ -> ok
    end,
    % If the token is a macro substituation and ...
    case ChildClass of
        lex ->
            case lists:member(ChildData#lex.type, [subst, incl]) of
                true ->
                    case ets:lookup(State#ppState.subst, ChildNode) of
                        % ... it is already founded just increase the counter
                        [{ChildNode,TokCnt,FoundTok}] ->
                            ets:insert(State#ppState.subst,
                                {ChildNode,TokCnt,FoundTok+1}),
                            false;
                        % ... it is founded first go into recursion on childs
                        [] ->
                            TokCnt = length(
                                ?Graph:path(ChildNode, [{llex,back}])),
                            ets:insert(State#ppState.subst,
                                {ChildNode,TokCnt,1}),
                            true
                    end;
                _ -> 0<length(?Graph:path(ChildNode, [llex]))
            end;
        _ -> true
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

% Implementation function for realtoken_first/1 and realtoken_last/1 functions.
realtoken_down_(Node, DownFun) ->
    Class = ?Graph:class(Node),
    case lists:member(Class, [file,form,clause,expr,lex]) of
        false -> throw(?LocalError(not_has_tokens, [Node]));
        _     ->
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
    case lists:member(?Graph:class(Node), [clause,expr,form,lex]) of
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


%%
%%
reallineBegin(Token, CollectFrom) ->
    xlnxtok(Token, fun realtoken_prev/1, fun(L) when is_list(L) -> L end,
            CollectFrom, 1, []).

%%
%%
reallineEnd(Token, CollectFrom) ->
    xlnxtok(Token, fun realtoken_next/1, fun lists:reverse/1,CollectFrom,1,[]).

% Implementing function for lineBegin/2 and lineEnd/2
xlnxtok(Token, NeighFun, DirFun, CollectFrom, Count, Line) ->
    NeighToken = NeighFun(Token),
    [Token1,Token2] = DirFun([NeighToken,Token]),
    {Idx,_} = ?MISC:string_strs(ws_between(Token1,Token2), ?MISC:string_EOLs()),
    if
        no/=NeighToken andalso 0==Idx ->
            if
                0<CollectFrom andalso CollectFrom=<Count ->
                    xlnxtok(NeighToken, NeighFun, DirFun, CollectFrom,
                             Count+1, [Token|Line]);
                true ->
                    xlnxtok(NeighToken, NeighFun, DirFun, CollectFrom,
                             Count+1, Line)
            end;
        true ->
            if
                0<CollectFrom andalso CollectFrom=<Count ->
                    {Token, NeighToken, DirFun([Token|Line])};
                true ->
                    {Token, NeighToken, DirFun(Line)}
            end
    end.


