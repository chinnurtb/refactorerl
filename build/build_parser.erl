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

%%% @doc Automatic builder module, whose source is the XML syntax description.
%%%      It creates inputs for the parser and scanner generators,
%%%      the syntax schema and the node structure description.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(build_parser).
-vsn("$Rev: 3043 $").

-export([build/5]).

-include_lib("xmerl/include/xmerl.hrl").

%%%%% =========================================================================
%%%%% Node structure

%% @doc Runs the build process. Builds the necessary files.
build(SyntaxFile, ScannerFile, ParserFile, NodeStructureFile, SchemaFile) ->
    {Syntax, _} = xmerl_scan:file(SyntaxFile, [{validation,dtd}]),
    FilesToBuild =
        [
            {fun node_structure/1, NodeStructureFile, "node structure"}
        ,   {fun scanner/1, ScannerFile, "scanner"}
        ,   {fun parser/1, ParserFile, "parser"}
        ,   {fun schema/1, SchemaFile, "schema"}
        ],

    [ build_file(Syntax, Fun, File, Description)
        || {Fun, File, Description} <- FilesToBuild ].


%% Build one file and write it to the disk.
build_file(Syntax, Fun, File, Description) ->
    io:put_chars("Generating " ++ Description ++ ": " ++ File ++ "\n"),
    case file:open(File, [write]) of
        {ok, Dev} ->
            io:put_chars(Dev, Fun(Syntax)),
            file:close(Dev);
        {error, Reason} ->
            throw({Description, file:format_error(Reason)})
    end.


%%%%% =========================================================================
%%%%% Node structure

%% The text of the node structure file.
node_structure(Root = #xmlElement{name='erlang-syntax'}) ->
    TokenClasses =
        [{Cl, Tok} || [[Cl], Tok] <-
                          collect_xml(Root,
                                      [{"token-class", ["@name",
                                                        "token/@type"]}])],
    %% lists:concat -- when the text contains things like `&lt;', xmerl
    %% returns it in pieces which must be concatenated
    Keywords =
        [{Kw,lists:concat(Val)} || [[Kw],Val] <-
                         collect_xml(Root, [{"keyword", ["@name","text()"]}])],

    Rules =
        lists:usort(
          rule_structure(
            [ {Class, Lex, lists:zip(ANs, AVs), lists:zip(TAs, TTs), Body} ||
                [[Class], [Lex], ANs, AVs, TAs, TTs, Body] <-
                    collect_xml(Root,
                                [{"rule-class", ["@class", "@lexlink"]},
                                 {"ruleset/rule",
                                  ["attrib/@name", "attrib/@value",
                                   "token/@attr", "token[@attr]/@type",
                                   "*"]}])],
            Keywords,
            TokenClasses)),

    ensure_no_duplicates(Rules),

    LexInf =
        [{Class, Lex} ||
            [[Class], [Lex]] <-
                collect_xml(Root, [{"rule-class", ["@class", "@lexlink"]}])],

    ["-module(referl_syntax_nodes).\n",
     "-export([structure/1, attribs/1, lexlink/1, parentlink/1]).\n",
     "-include(\"refactorerl.hrl\").\n\n",
     [structure_text(R) || R <- Rules],
     "structure(_) -> erlang:error(unknown_structure).\n\n\n",
     [attrib_text(R) || R <- Rules],
     "attribs(_) -> [].\n\n\n",
     [lexlink_text(Class, Lex) || {Class, Lex} <- LexInf],
     "lexlink(_) -> erlang:error(unknown_class).\n\n\n",
     [parent_text(Class, Links) || {Class, Links} <- parent_info(Root)],
     "parentlink(_) -> erlang:error(unknown_class).\n"
    ].

structure_text({Class, _Lex, Attribs, Struct, _TokenAttrib}) ->
    ["structure(", data_text(Class, Attribs), ") ->\n",
     io_lib:format("    ~p;", [Struct]),
     "\n\n"].

attrib_text({_C, _L, _A, _S, []}) -> "";
attrib_text({Class, Lex, Attribs, _Struct, TokenAttrib}) ->
    ["attribs(", data_text(Class, Attribs), ") ->\n",
     "    [",
     join([io_lib:format("{{~s, ~b}, #~s.~s}", [Lex, N, Class, Attr]) ||
              {Attr, N} <- TokenAttrib],
          ","), "];\n\n"].

data_text(Class, Attribs) ->
    ["#", Class, "{",
     join([[A,"='",V,"'"] || {A,V} <- Attribs], ","),
     "}"].

lexlink_text(Class, Lex) ->
    ["lexlink(", Class, ") -> ", Lex, ";\n"].

parent_text(Class, Links) ->
    ["parentlink(", Class, ") -> [", join(Links, ", "), "];\n"].

parent_info(Root) ->
    ParLink =
        lists:foldl(
          fun ({Name, Link}, D) -> dict:append(Name, Link, D) end,
          dict:new(),
          [{Name, Link} ||
              [[Name],[Link]] <- collect_xml(
                                   Root, [{"rule-class/ruleset/rule//symbol",
                                           ["@name", "@link"]}]) ++
                  collect_xml(
                    Root, [{"rule-class/ruleset/rule//repeat",
                            ["@symbol", "@link"]}])
                 ]),
    Classes =
        lists:foldl(
          fun({Class, Name}, D) -> dict:append(Class, Name, D) end,
          dict:new(),
          [{Class, Name} ||
              [[Class], [Name]] <- collect_xml(
                                     Root, [{"rule-class", "@class"},
                                            {"ruleset","@head"}])]),
    [{Class, lists:usort(lists:flatmap(
                           fun(N) -> case dict:find(N, ParLink) of
                                         {ok, L} -> L;
                                         error   -> [] end
                           end, dict:fetch(Class, Classes)))} ||
        Class <- dict:fetch_keys(Classes)].

ensure_no_duplicates([{C1, Att1, Tok1, _, _} |
                       Tail = [{C2, Att2, Tok2, _, _} | _]]) ->
    case C1 == C2 andalso
        lists:sort(Att1) == lists:sort(Att2) andalso
        lists:sort(Tok1) == lists:sort(Tok2) of
        true ->
            throw({ambiguous, C1, Att1, Tok1});
        false ->
            ensure_no_duplicates(Tail)
    end;
ensure_no_duplicates([{_C,_A,_T,_,_}]) ->
    ok.


rule_structure(Rules, Kw, TCl) ->
    lists:flatten(
      [rule_structure(Class, Lex, Attribs, [], [], Tokens, Kw, TCl, Rule) ||
          {Class, Lex, Attribs, Tokens, Rule} <- Rules]).

rule_structure(Class, Lex, Attr, TokenVal, TokenAttr, [], _Kw, _TCl, Rule) ->
    {Class, Lex,
     Attr ++ [{A,V} || {_,_,A,V} <- TokenVal],
     rule_structure(Rule, TokenVal),
     [attrib_token(T, Rule) || T <- TokenAttr]};

rule_structure(C, L, A, TokenVal, TokenAttr,
               [{Attr, Type} | Tail], Kw, TCl, Rule) ->
    RealTypes = 
        case lists:keysearch(Type, 1, TCl) of
            {value, {_, V}} -> V;
            false           -> [Type]
        end,
    {Values, NewTokenAttr} =
        lists:foldl(
          fun(RT, {TV, TA}) ->
                  case lists:keysearch(RT, 1, Kw) of
                      {value, {_, Val}} -> {[{Type, RT, Attr, Val} | TV], TA};
                      false             -> {TV, [Attr | TA]}
                  end
          end,
          {[], TokenAttr},
          RealTypes),
    if
        Values =:= [] ->
            rule_structure(C, L, A, TokenVal, NewTokenAttr,
                           Tail, Kw, TCl, Rule);
        true ->
            [rule_structure(C, L, A, [V|TokenVal], NewTokenAttr,
                            Tail, Kw, TCl, Rule) || V <- Values]
    end.

rule_structure(Rule, TokenVal) ->
     lists:flatmap(fun(E) -> elem_structure(E, TokenVal) end, Rule).

elem_structure(Elem = #xmlElement{name=symbol}, _) ->
    Link = xpath_attr("@link", Elem),
    [{symbol, list_to_atom(Link)}];
elem_structure(Elem = #xmlElement{name=token}, TokenVal) ->
    Type = xpath_attr("@type", Elem),
    case lists:keysearch(Type, 1, TokenVal) of
        {value,{_,RT,_,_}} -> [{token, list_to_atom(RT)}];
        false              -> [{token, list_to_atom(Type)}]
    end;
elem_structure(Elem = #xmlElement{name=optional}, TV) ->
    [[Content]] = collect_xml(Elem, [{".", "*"}]),
    [{optional, rule_structure(Content, TV)}];
elem_structure(Elem = #xmlElement{name=repeat}, _) ->
    [[[Sep],[Link]]] = collect_xml(Elem, [{".",["@separator","@link"]}]),
    [{repeat, list_to_atom(Sep), list_to_atom(Link)}];
elem_structure(_,_) -> [].


attrib_token(Attr, Rule) ->
    TokenAttrs =
        [A || #xmlElement{name=token} = Elem <- Rule, 
              [A] <- collect_xml(Elem,[{".",["@attr"]}])],
    N = length(lists:takewhile(
                 fun(AL) -> not lists:member(Attr, AL) end,
                 TokenAttrs)),
    {Attr, N+1}.




join([E1, E2| Es], S) -> [E1, S| join([E2| Es], S)];
join([E], _)          -> [E];
join([], _)           -> [].


%%%%% =========================================================================
%%%%% Parser

%% the text of the parser, to be handled by yecc
parser(Root) ->
    RootSymbol   = xpath_attr("@start", Root),
    {NTs, Rules} = parser_rules(Root),
    Tokens       = lists:usort(collect_xml(Root, [{"//token", "@type"}])),
    TokenClasses = lists:usort(collect_xml(Root, [{"//token-class", "@name"}])),
    Terminals    = Tokens -- TokenClasses,

    ["Nonterminals ", join(NTs, " "), ".\n",
     "Terminals ", join(Terminals, " "), ".\n",
     "Rootsymbol ", RootSymbol, ".\n",
     Rules,
     "Erlang code.\n",
     "-include(\"refactorerl.hrl\").\n",
     "-import(?Syn, [build/2]).\n",
     "tvalue({_Type,_Index,{#token{value=V},_Node}})->V.\n",
     "tnode({_Type,_Index,{_Token,Node}})->Node.\n"].


%% returns {NTs, Rules}
parser_rules(Root) ->
    {OneLineRulesText, OneLineNTs} = one_line_rules(Root),

    Rules = collect_xml(Root,
                        [ {"rule-class", ["@class", "@lexlink"]},
                          {"ruleset", "@head"},
                          {"rule", ["attrib/@name", "attrib/@value", "*"]}
                         ]),
    GeneratedRules   = lists:map(fun parser_generate_rule/1,
                                 text_index_list(Rules)),
    {RulesText, NTs} = lists:unzip(GeneratedRules),

    AllNTs           = lists:usort(lists:concat(NTs) ++
                                   lists:concat(OneLineNTs)),

    {AllNTs, [OneLineRulesText, RulesText]}.

index_list(List) -> lists:zip(List, lists:seq(1, length(List))).
text_index_list(List) ->
    [ {Elem, integer_to_list(Index)} || {Elem, Index} <- index_list(List) ].

%% Returns the texts and nonterminals of chain rules and token definition rules.
one_line_rules(Root) ->
    ChainRules       = collect_xml(Root, [ {"//ruleset", "@head"}
                                         , {"chain-rule", "@name"}
                                         ]),
    TokenDefRules    = collect_xml(Root, [ {"//token-class", "@name"}
                                         , {"token", "@type"}
                                         ]),
    OneLineRules     = ChainRules ++ TokenDefRules,
    OneLineRulesText = lists:flatmap(fun one_line_rule_text/1, OneLineRules),
    OneLineNTs       = lists:map(fun erlang:hd/1, OneLineRules),
    {OneLineRulesText, OneLineNTs}.


one_line_rule_text([Head, Name]) ->
    [Head, " -> ", Name, " : '$1'.\n"].



synelem(Class, Nr) -> ["{", Class, ", '$", Nr, "'}"].
lexelem(Lex, Nr) -> ["{", Lex, ", tnode('$", Nr, "')}"]. 

gen_rule(Head, Nr, Head, Class, _, _) ->
    {{Head, synelem(Class, Nr)}, "", [Head]};
gen_rule(Elem = #xmlElement{name=Name}, Nr, Head, Class, Lex, RuleFullHead) ->
    gen_rule2(Name, Elem, Nr, Head, Class, Lex, RuleFullHead).


gen_rule2(token, Elem, Nr, Head, _, Lex, _) ->
    TokenType = xpath_attr("@type", Elem),
    {{TokenType, lexelem(Lex, Nr)}, "", [Head]};
gen_rule2(symbol, Elem, Nr, Head, _, _, _) ->
    Name = xpath_attr("@name", Elem),
    Link = xpath_attr("@link", Elem),
    {{Name, synelem(Link, Nr)}, "", [Head]};
gen_rule2(repeat, Elem, Nr, _, _, Lex, RuleFullHead) ->
    NewHead                    = RuleFullHead ++ "@reps" ++ Nr,
    [TokenName]                = collect_xml(Elem, [{".", "@separator"}]),
    [[SymbolName, SymbolLink]] =
        collect_xml(Elem, [{".", ["@symbol", "@link"]}]),
    SimpleRule =
        NewHead ++ " -> " ++ SymbolName ++ " : {" ++ SymbolLink ++ ", '$1'}.\n",
    TokenRule =
        [NewHead, " -> ", NewHead, " ", TokenName, " ", SymbolName,
         " : ['$1', ",lexelem(Lex, "2"),", ",synelem(SymbolLink, "3"),"].\n"],
    { {NewHead, ["'$",Nr,"'"]}, SimpleRule ++ TokenRule, [NewHead]};
gen_rule2(optional, Elem, Nr, _, Class, Lex, RuleFullHead) ->
    NewHead   = RuleFullHead ++ "@opt" ++ Nr,
    EmptyRule = NewHead ++ " -> '$empty' : [].\n",
    Content   = xmerl_xpath:string("*", Elem),
    {InnerRule, InnerNTs} =
        parser_generate_rule({[Class, Lex, [NewHead], [], [], Content], ""}),
    { {NewHead, ["'$",Nr,"'"]}, EmptyRule ++ InnerRule, [NewHead] ++ InnerNTs}.


parser_generate_rule({[Class, Lex, [Head],
                       AttribName, AttribValue, RHS], RuleNr}) ->
    RuleFullHead = Head ++ RuleNr,
    FilteredRHS  = [E || E=#xmlElement{name=Name} <- RHS, Name =/= attrib],
    IndexedRHS   = text_index_list(FilteredRHS),
    Mixture      = [ gen_rule(RHSElem, Nr, Head, Class, Lex, RuleFullHead)
                        || {RHSElem, Nr} <- IndexedRHS ],

    {RuleComponents, GeneratedRules, NTs} = lists:unzip3(Mixture),

    InterspacedRuleComponents = join(RuleComponents, {" ", ", "}),
    {RHStext, SemanticText}   = lists:unzip(InterspacedRuleComponents),
    SynElem = parser_synelem(AttribName, AttribValue,
                             SemanticText, IndexedRHS, Class),
    { Head ++ " -> " ++ RHStext ++ " : " ++ SynElem ++ ".\n" ++ GeneratedRules
    , [Head] ++ lists:concat(NTs)}.


parser_synelem([], [], SemanticText, _, _) ->
    "[" ++ SemanticText ++ "]";
parser_synelem(AttribName, AttribValue, SemanticText, IndexedRHS, Class) ->
    Attrs = [ {xpath_attr("@attr", Elem), Nr}   || {Elem, Nr} <- IndexedRHS ],
    AdditionalAttrs =
        [ As ++ "=tvalue('$" ++ Nr ++ "')" || {As, Nr} <- Attrs, As /= none ],

    AllAdditionalAttrs =
        [[AN, "=", AV] || {AN, AV} <- lists:zip(AttribName, AttribValue)] ++
        AdditionalAttrs,

    SemText = lists:flatten(SemanticText),

    ["build(#", Class, "{", join(AllAdditionalAttrs, ","), "}, ",
     "[", SemText, "])"].

%%%%% =========================================================================
%%%%% Schema

%% the text of the syntax schema, to be put into a header file
schema(Root) ->
    Connections =
        collect_xml(Root,
                    [{"rule-class", "@class"},
                     {"//symbol", ["@name", "@link"]}]) ++
        collect_xml(Root,
                    [{"rule-class", "@class"},
                     {"//repeat", ["@symbol", "@link"]}]),

    Connections2 =
        lists:foldl(
            fun([[FromClass], [Via], [ToClass]], Acc) ->
                ViaClassInList =
                    collect_xml(Root, [ {"rule-class", "@class"}
                                      , {"ruleset[@head = '" ++ Via ++ "']", ""}
                                      ]),
                case ViaClassInList of
                    []           -> Acc;
                    [[ViaClass]] -> Acc ++ [[FromClass, ViaClass, ToClass]]
                end
            end, [], Connections),

    UniqueConnections = [[SchemaHead|_]|_] = lists:usort(Connections2),
    Connections3 =
       [{"root", [{"file", "file"}]}]
        ++ [{"file", [{"form", "form"}]}]
        ++ schema_group(SchemaHead, UniqueConnections),
                        schema_text(Connections3).


schema_text(Connections) ->
    "-define(SYNTAX_SCHEMA, [\n"
     ++ lists:flatten(join(lists:map(fun schema_conn/1, Connections), ",\n"))
     ++ "]).\n".


schema_conn({Head, Pairs}) ->
    RecordInfo =
        if
            Head =:= "root" -> "[]";
            true            -> "record_info(fields, " ++ Head ++ ")"
        end,
    Pairs2 = [ ["{",Elem1,",",Elem2,"}"] || {Elem1, Elem2} <- Pairs],
    Pairs3 = ["[",join([lists:flatten(join(Pairs2, ",\n     "))], ", "), "]"],
    ["  {", Head, ",", RecordInfo, ",", Pairs3, "}"].

schema_group(Head, Connections) -> schema_group(Connections, Head, [], []).

schema_group([], Head, RuleAcc, Acc) ->
    Acc ++ [{Head, RuleAcc}];
schema_group([[Head, Via, To]|Xs], Head, RuleAcc, Acc) ->
    schema_group(Xs, Head, RuleAcc ++ [{To, Via}], Acc);
schema_group([[From, Via, To]|Xs], Head, RuleAcc, Acc) ->
    schema_group(Xs, From, [{To, Via}], Acc ++ [{Head, RuleAcc}]).


%%%%% =========================================================================
%%%%% Scanner

%% the text of the scanner, to be handled by leex
scanner(Root) ->
    [scanner_definitions(Root),
     scanner_keywords(Root),
     scanner_lexicals(Root)].

scanner_definitions(Root) ->
    [[Name, "=", sc_texts(Content), "\n"] ||
        [Name, Content] <- collect_xml(Root, [{"pattern", ["@name", "*"]}])].

%%%     EndToken = xpath_attr("@end", Root),
%%%     "Definitions.\n" ++ scanner_generate_definitions(Root)
%%%         ++ "Rules.\n"
%%%         ++ scanner_generate_rules(Root, EndToken)
%%%         ++ scanner_generate_lexical_rules(Root, EndToken)
%%%         ++ "Erlang code.\n"
%%%         ++ "-include(\"refactorerl.hrl\").\n"
%%%         ++ "-import(?Token, [build/2]).\n".

%%% scanner_generate_definitions(Root) ->
%%%     Patterns = collect_xml( Root, [ {"pattern", ["@name", "*"]}]),
%%%     [ PatternName ++ " = " ++ sc_texts(PatternContents) ++ "\n"
%%%         || [PatternName, PatternContents] <- Patterns ].


sc_texts(#xmlText{value=E}) ->
    escape(E);
sc_texts(Patterns) when is_list(Patterns) ->
    lists:flatmap(fun sc_texts/1, Patterns);
sc_texts(Elem = #xmlElement{name=Name, content=Content}) ->
    sc_text(Name, Elem, Content).

escape([Char | Tail])
  when Char =:= $*; Char =:= $+; Char =:= $\\;
       Char =:= $.; Char =:= $$; Char =:= $^;
       Char =:= $(; Char =:= $); Char =:= $?;
       Char =:= $[; Char =:= $]; Char =:= $|;
       Char =:= ${; Char =:= $};
       Char =:= $\"     -> [$\\, Char | escape(Tail)];
escape([Char | Tail])
  when Char =:= $\ ; Char =:= $\t; Char =:= $\n  -> escape(Tail);
escape([Char | Tail])                            -> [Char | escape(Tail)];
escape([])                                       -> [].

sc_text(br, _, Content)          -> sc_texts(Content);
sc_text(text, _, Content)        -> sc_texts(Content);
sc_text(match, Elem, _)          -> "{"  ++ xpath_attr("@name", Elem) ++ "}";
sc_text(rep, _, Content)         -> "("  ++ sc_texts(Content) ++ ")*";
sc_text(opt, _, Content)         -> "("  ++ sc_texts(Content) ++ ")?";
sc_text('chars-of', _, Content)  -> "["  ++ sc_texts(Content) ++ "]";
sc_text('chars-but', _, Content) -> "[^" ++ sc_texts(Content) ++ "]";
sc_text(char, Elem, _) ->
    CharCode = xpath_attr("@code", Elem),
    octal(list_to_integer(CharCode));
sc_text(branches, Elem, _) ->
    Branches       = xmerl_xpath:string("br", Elem),
    BranchPatterns = lists:map(fun sc_texts/1, Branches),
    "(" ++ join(BranchPatterns, "|") ++ ")";
sc_text(range, Elem, _) ->
    [#xmlElement{content=FromContent}] = xmerl_xpath:string("from", Elem),
    [#xmlElement{content=ToContent}]   = xmerl_xpath:string("to", Elem),
    sc_texts(FromContent) ++ "-" ++ sc_texts(ToContent).

octal(N)    -> octal(N, 3).

octal(N, 1) -> "\\" ++ integer_to_list(N);
octal(N, K) -> octal(N div 8, K - 1) ++ integer_to_list(N rem 8).

%% scanner_generate_rules(Root, EndToken) ->
%%     KwNameText = collect_xml( Root, [ {"keyword", ["@name", "text()"]}]),
%%     lists:flatmap(
%%         fun ([Name, Text]) ->
%%                 escape(lists:flatten(Text))
%%                     ++ " : "
%%                     ++ scanner_RHS(Name, EndToken)
%%         end, KwNameText ).

scanner_keywords(Root) ->
    [[Name, "=", escape(lists:flatten(Text)), "\n"] ||
        [Name, Text] <- collect_xml(Root, [{"keyword", ["@name", "text()"]}])].

scanner_lexicals(Root) ->
    [[Name, "=", sc_texts(Content), "\n"] ||
        [Name, Content] <- collect_xml(Root, [{"lexical", ["@name", "*"]}])].

%% scanner_generate_lexical_rules(Root, EndToken) ->
%%     LexicalRules = collect_xml( Root, [{"lexical", ["@name", "*"]}]),
%%     lists:flatmap(
%%         fun ([Name, Contents]) ->
%%             sc_texts(Contents) ++ " : " ++ scanner_RHS(Name, EndToken)
%%         end, LexicalRules).

%% scanner_RHS([Name], EndToken) ->
%%     TokenType =
%%         if
%%             Name =:= EndToken -> "end_token";
%%             true              -> "token"
%%         end,
%%     ["{", TokenType, ",",
%%      "{'", Name, "', TokenLine, build('", Name, "', TokenChars)}}.\n"].


%%%%% =========================================================================
%%%%% XML/XPath utilities

xpath_attr(XPathString, Node) ->
    XPathExpr = xmerl_xpath:string(XPathString, Node),
    case XPathExpr of
      [#xmlAttribute{value=Value}] -> Value;
      _ -> none
    end.


%% collects nodes starting from a node
%% the list of constraints may have the following shapes:
%%    {Waypoint, PathToInterestingElement}
%%    {Waypoint, [PathToInterestingElement1, ...]}
%% the function finds the elements [{W1, ...}, {W2, ...}, ..., {Wn, E}]
%%    as the combined path Root/W1/W2/.../Wn/E in an efficient way
%% returned are the list of interesting elements by the waypoints
collect_xml(RootNode, Constraints) ->
    VerboseConstraints =
        lists:map(
            fun({Waypoint, Collect}) -> {Waypoint, Collect, no_filter};
               (X)                   -> X
            end
        , Constraints),
    collect_xml(RootNode, VerboseConstraints, []).


collect_xml(_, [], Acc) -> [Acc];
collect_xml(Node, [{Waypoint, Collect, Filter}|Cs], Acc) ->
    lists:flatmap(
        fun(NextNode) ->
            FilterAllows = Filter == no_filter orelse Filter(NextNode),
            if
                FilterAllows ->
                    case Collect of
                        [[_|_]|_] -> RealCollect = Collect;
                        []        -> RealCollect = Collect;
                        _         -> RealCollect = [Collect]
                    end,
                Addition =
                    [ convert_from_xml(xmerl_xpath:string(C, NextNode))
                        || C <- RealCollect ],
                collect_xml(NextNode, Cs, Acc ++ Addition);
            true ->
                []
        end
    end
  , xmerl_xpath:string(Waypoint, Node)).


convert_from_xml(L) when is_list(L) -> lists:map(fun convert_from_xml/1, L);
convert_from_xml([])                         -> [];
convert_from_xml(#xmlAttribute{value = V})   -> V;
convert_from_xml(#xmlText{value = V})        -> V;
convert_from_xml([#xmlAttribute{value = V}]) -> V;
convert_from_xml([#xmlText{value = V}])      -> V;
convert_from_xml(X)                          -> X.



