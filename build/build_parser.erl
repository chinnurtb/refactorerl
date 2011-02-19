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

%%% @doc Builds inputs for the parser and scanner generators from the XML
%%% description of the syntax, and creates the syntax schema
%%%
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(build_parser).
-vsn("$Rev: 1247 $").

-export([build/4]).

-include_lib("xmerl/include/xmerl.hrl").

%% @doc Runs the build process.
build(Input, Scanner, Parser, Schema) ->
    {Syntax, _} = xmerl_scan:file(Input),
    generate(Syntax, scanner, Scanner, fun scanner/2),
    generate(Syntax, parser, Parser, fun parser/2),
    generate(Syntax, schema, Schema, fun schema/2).

generate(Syntax, Desc, File, Fun) ->
    io:format("Generating ~s: ~s~n", [Desc, File]), 
    case file:open(File, [write]) of
        {ok, Dev} ->
            Fun(Syntax, Dev),
            file:close(Dev);
        {error, Reason} ->
            throw({Desc, file:format_error(Reason)})
    end.

%%% ========================= Parser ================================


%% Internal state for collecting parser information
-record(parser, {nonterm=[],     % Names of nonterminals
                 term=[],        % Names of terminals
                 rules=[]}).     % Rules: {Name, [Prod], [Action]}

%% Internal state for collection ruleset information
-record(ruleset, {name="",       % Ruleset head (name of rule)
                  index=1,       % Rule index
                  parser=#parser{}}).   % Partial parser information

%% Internal state for collection rule information
-record(rule, {prefix="",        % Prefix for new rule names
               index=1,          % Production index
               prods=[],         % Collected productions
               lnk=[],           % Collected link information
               attr=[],          % Collected attribute information
               parser=#parser{}}).      % Partial parser information

%% Generate the parser generator input
parser(Elem = #xmlElement{name='erlang-syntax'}, Dev) ->
    Parser = union(parser_rulesets(Elem), parser_tokens(Elem)),

    io:format(Dev, "Nonterminals", []),
    lists:foreach(
      fun(S)-> io:format(Dev, " ~s", [S]) end, Parser#parser.nonterm),
    io:format(Dev, ".~n", []),

    io:format(Dev, "Terminals", []),
    lists:foreach(
      fun (S) ->
              io:format(Dev, " ~s", [S]) end, Parser#parser.term),
    io:format(Dev, ".~n", []),
    
    io:format(Dev, "Rootsymbol ~s.~n", [attrib(Elem, start)]),

    lists:foreach(
      fun({Name, Prods, Act})->
              io:format(Dev, "~s -> ~s : ~s.~n",
                        [Name, insert_delim(Prods, " "), Act])
      end, Parser#parser.rules),

    io:format(Dev, "Erlang code.~n~s",
              ["-import(refac_synlex, [syn_elem/2]).\n"
               "-include(\"refactorerl.hrl\").\n"
               "tvalue({_,_,{#token{value=V},_}})->V.\n"]).


%% Collect terminal names
parser_tokens(Elem) ->
    lists:foldl(fun parser_token/2, #parser{}, Elem#xmlElement.content).

parser_token(Elem = #xmlElement{name=lexical}, Parser) ->
    union(Parser, #parser{term=[attrib(Elem, name)]});
parser_token(Elem = #xmlElement{name=keyword}, Parser) ->
    union(Parser, #parser{term=[keyword_name(Elem)]});
parser_token(_, Parser) ->    
    Parser.

%% Process rulesets: collect nonterminal names and rules
parser_rulesets(Elem) ->
    lists:foldl(fun parser_ruleset/2, #parser{}, Elem#xmlElement.content).

parser_ruleset(Elem = #xmlElement{name=ruleset}, Parser) ->
    Name = attrib(Elem, head),
    union([Parser,
           #parser{nonterm=[Name]},
           parser_rules(Name, Elem)]);

parser_ruleset(Elem = #xmlElement{name='token-class'}, Parser) ->
    Name = attrib(Elem, name),
    union([Parser,
           #parser{nonterm=[Name]},
           token_class(Name, Elem)]);

parser_ruleset(_, Parser) -> Parser.


%% Process rules in a ruleset
parser_rules(Name, Elem) ->
    (lists:foldl(fun parser_rule/2,
                 #ruleset{name=Name},
                 Elem#xmlElement.content))#ruleset.parser.

parser_rule(Elem = #xmlElement{name=rule}, Rules) ->
    Prefix = io_lib:format("~s~b", [Rules#ruleset.name, Rules#ruleset.index]),
    Rule = rule_contents(Elem, Prefix),
    Act = ["syn_elem(#", attrib(Elem, class),
           "{", insert_delim(Rule#rule.attr, ", "),
           "}, [", insert_delim(Rule#rule.lnk, ", "), "])"],
    next(Rules, [#parser{rules = [{Rules#ruleset.name,
                                   Rule#rule.prods,
                                   Act}] },
                 Rule#rule.parser]);

parser_rule(Elem = #xmlElement{name='copy-rule'}, Rules) ->
    next(Rules, [#parser{rules = [{Rules#ruleset.name,
                                   [attrib(Elem, name)],
                                   ["'$1'"]}]
                         }]);

parser_rule(_, Acc) ->
    Acc.

%% Generate productions for a rule
rule_contents(Elem, Prefix) ->
    rule_contents(Elem, Prefix, 1).
rule_contents(Elem, Prefix, StartInd) ->
    lists:foldl(fun rule_content/2,
                #rule{prefix=Prefix, index=StartInd},
                Elem#xmlElement.content).

rule_content(Elem = #xmlElement{name=attrib}, Rule) ->
    Rule#rule{attr=Rule#rule.attr ++ [[attrib(Elem, name),"=",text_of(Elem)]]};

rule_content(Elem = #xmlElement{name=token}, Rule) ->
    Prods = [attrib(Elem, type)],
    Tok = [io_lib:format("{'$token', '$~b'}", [Rule#rule.index])],
    case attrib(Elem, attr) of
        "" -> Attr = [];
        AN -> Attr = [[AN, io_lib:format("=tvalue('$~b')", [Rule#rule.index])]]
    end,
    next(Rule#rule{prods=Rule#rule.prods ++ Prods,
                   lnk=Rule#rule.lnk ++ Tok,
                   attr=Rule#rule.attr ++ Attr});

rule_content(Elem = #xmlElement{name=symbol}, Rule) ->
    Prods = [attrib(Elem, name)],
    Lnk = [io_lib:format("{~s, '$~b'}", [attrib(Elem, link), Rule#rule.index])],
    next(Rule#rule{prods=Rule#rule.prods ++ Prods,
                   lnk=Rule#rule.lnk ++ Lnk});

rule_content(Elem = #xmlElement{name=optional}, Rule) ->
    Name = io_lib:format("~s@opt~b", [Rule#rule.prefix, Rule#rule.index]),
    OptRule = rule_contents(Elem, Name),
    Prods = Rule#rule.prods ++ [Name],
    Lnk = Rule#rule.lnk ++ [io_lib:format("'$~b'", [Rule#rule.index])],
    next(Rule#rule{prods=Prods, lnk=Lnk},
         [#parser{nonterm=[Name],
                  rules=[{Name, ["'$empty'"], ["[]"]},
                         {Name, OptRule#rule.prods,
                          ["[", insert_delim(OptRule#rule.lnk, ", "), "]"]}
                         ]},
          OptRule#rule.parser]);

rule_content(Elem = #xmlElement{name=repeat}, Rule) ->
    Name = io_lib:format("~s@rep~b", [Rule#rule.prefix, Rule#rule.index]),
    RepRule = rule_contents(Elem, Name, 2),
    Prods = Rule#rule.prods ++ [Name],
    Lnk = Rule#rule.lnk ++ [io_lib:format("'$~b'", [Rule#rule.index])],
    next(Rule#rule{prods=Prods, lnk=Lnk},
         [#parser{nonterm=[Name],
                  rules=[{Name, ["'$empty'"], ["[]"]},
                         {Name, [Name | RepRule#rule.prods],
                          ["[",
                           insert_delim(["'$1'" | RepRule#rule.lnk], ", "),
                           "]"]}
                        ]},
         RepRule#rule.parser]);

rule_content(_, Rule) -> Rule.

%% Generate rules for a token class
token_class(Name, Elem) ->
    element(2, lists:foldl(fun token_rule/2,
                           {Name, #parser{}},
                           Elem#xmlElement.content)).

token_rule(Elem = #xmlElement{name=token}, {Class, Parser}) ->
    Name = attrib(Elem, type),
    {Class, union(Parser,
                  #parser{rules=[{Class, [Name], ["'$1'"]}]}
                 )};

token_rule(_, Parser) -> Parser.

%% Increase the index in a #rule or #ruleset
next(Rule=#rule{index=Index})     -> Rule#rule{index = Index+1};
next(Rules=#ruleset{index=Index}) -> Rules#ruleset{index = Index+1}.

%% Extend the parser in a #rule or a #ruleset
next(Rule=#rule{parser=Parser}, Parsers) ->
    (next(Rule))#rule{parser = union([Parser | Parsers])};
next(Rules=#ruleset{parser=Parser}, Parsers) ->
    (next(Rules))#ruleset{parser = union([Parser | Parsers])}.


%%% ========================= Scanner ================================

-record(scanner, {defs=[], rules=[], prekw=[], postkw=[]}).

scanner(Elem = #xmlElement{name='erlang-syntax'}, Dev) ->
    Scanner = scanner_tokens(Elem),
    io:format(Dev, "Definitions.~n", []),
    lists:foreach(
      fun({Name, Regexp}) -> io:format(Dev, "~s = ~s~n", [Name, Regexp]) end,
      Scanner#scanner.defs),
    io:format(Dev, "Rules.~n", []),
    End = attrib(Elem, 'end'),
    lists:foreach(
      fun ({Name, Regexp}) ->
              Type = if Name =:= End -> "end_token"; true -> "token" end,
              io:format(Dev, "~s : {~s, {'~s', TokenLine, "
                                        "lex_elem('~s', TokenChars)}}.~n",
                        [Regexp, Type, Name, Name]) end,
      Scanner#scanner.rules),
    io:format(Dev, "Erlang code.~n~s",
              ["-import(refac_synlex, [lex_elem/2]).\n"]).

scanner_tokens(Elem) ->
    S1=lists:foldl(fun scanner_kwinfo/2, #scanner{}, Elem#xmlElement.content),
    lists:foldr(fun scanner_token/2, S1, Elem#xmlElement.content).

scanner_kwinfo(Elem = #xmlElement{name='keyword-prefix'}, Scanner) ->
    Scanner#scanner{prekw=scanner_pattern(Elem)};
scanner_kwinfo(Elem = #xmlElement{name='keyword-postfix'}, Scanner) ->
    Scanner#scanner{postkw=scanner_pattern(Elem)};
scanner_kwinfo(_, Scanner) -> Scanner.


scanner_token(Elem = #xmlElement{name=keyword}, Scanner) ->
    Name = keyword_name(Elem),
    Regexp = [Scanner#scanner.prekw,
              pattern_chars(Elem),
              Scanner#scanner.postkw],
    Scanner#scanner{rules = [{Name, Regexp} | Scanner#scanner.rules]};

scanner_token(Elem = #xmlElement{name=lexical}, Scanner) ->
    Name = attrib(Elem, name),
    Regexp = scanner_pattern(Elem),
    Scanner#scanner{rules = [{Name, Regexp} | Scanner#scanner.rules]};

scanner_token(Elem = #xmlElement{name=pattern}, Scanner) ->
    Name = attrib(Elem, name),
    Regexp = scanner_pattern(Elem),
    Scanner#scanner{defs = [{Name, Regexp} | Scanner#scanner.defs]};

scanner_token(_, Scanner) -> Scanner.


scanner_pattern(Elem) ->
    lists:foldr(fun pattern_elem/2, [], Elem#xmlElement.content).

pattern_elem(Elem = #xmlElement{name=text}, Regexp) ->
    [pattern_chars(Elem) | Regexp];
pattern_elem(Elem = #xmlElement{name='chars-of'}, Regexp) ->
    [["[", pattern_chars(Elem), "]"] | Regexp];
pattern_elem(Elem = #xmlElement{name='chars-but'}, Regexp) ->
    [["[^", pattern_chars(Elem), "]"] | Regexp];
pattern_elem(Elem = #xmlElement{name=branches}, Regexp) ->
    [["(", pattern_branches(Elem), ")"] | Regexp];
pattern_elem(Elem = #xmlElement{name=rep}, Regexp) ->
    [["(", scanner_pattern(Elem), ")*"] | Regexp];
pattern_elem(Elem = #xmlElement{name=opt}, Regexp) ->
    [["(", scanner_pattern(Elem), ")?"] | Regexp];
pattern_elem(Elem = #xmlElement{name=match}, Regexp) ->
    [["{", attrib(Elem, name), "}"] | Regexp];
pattern_elem(_, Regexp) -> Regexp.

pattern_branches(Elem) ->
    insert_delim(
      lists:foldr(fun pattern_branch/2, [], Elem#xmlElement.content),
      "|").

pattern_branch(Elem = #xmlElement{name=br}, Regexp) ->
    [scanner_pattern(Elem) | Regexp];
pattern_branch(_, Regexp) -> Regexp.

pattern_chars(Elem) ->
    lists:foldr(fun pattern_char/2, [], Elem#xmlElement.content).

pattern_char(Elem = #xmlElement{name=range}, Regexp) ->
    [From] = xmerl_xpath:string("from", Elem),
    [To] = xmerl_xpath:string("to", Elem),
    [[pattern_chars(From), "-", pattern_chars(To)] | Regexp];
pattern_char(Elem = #xmlElement{name=char}, Regexp) ->
    [["\\", octal(attrib(Elem, code))] | Regexp];
pattern_char(#xmlText{value=Text}, Regexp) ->
    [escape(Text) | Regexp].

escape([Char | Tail])
  when Char =:= $*; Char =:= $+; Char =:= $\\;
       Char =:= $.; Char =:= $$; Char =:= $^;
       Char =:= $(; Char =:= $); Char =:= $?;
       Char =:= $[; Char =:= $]; Char =:= $|     -> [$\\, Char | escape(Tail)];
escape([Char | Tail])
  when Char =:= $\ ; Char =:= $\t; Char =:= $\n  -> escape(Tail);
escape([Char | Tail])                            -> [Char | escape(Tail)];
escape([])                                       -> [].

octal(Decimal) ->
    {ok, [Num], _} = io_lib:fread("~d", Decimal),
    io_lib:format("~3.8.0b", [Num]).

%%% ========================= Schema ================================

-record(schema, {class=[], copies=[], links=[]}).

schema(Elem = #xmlElement{name='erlang-syntax'}, Dev) ->
    Rulesets = schema_rulesets(Elem),
    Normal = schema_normal(Rulesets),
    Schema = [{"file", [{"form","form"}]}, {"env", []} | schema_links(Normal)],
    io:format(Dev, "-define(SYNTAX_SCHEMA, [~n", []),
    [ io:format(Dev, "\t{~s, record_info(fields, ~s),\n\t\t[~s]},~n",
                [Name, Name, linkstr(Links)]) || {Name, Links} <- Schema],
    io:format(Dev, "\t{root, [], [{file, file}, {env,env}]}~n]).~n", []).

linkstr([]) -> "";
linkstr([{FT, FC} | Rest]) ->
    ["{", FT, ",", FC, "}"] ++
        [ [",\n\t\t{", T, ",", C, "}"] || {T,C} <- Rest].


schema_rulesets(Elem) ->
    lists:foldl(fun schema_ruleset/2, [], Elem#xmlElement.content).

schema_ruleset(Elem = #xmlElement{name=ruleset}, Schema) ->
    Name = attrib(Elem, head),
    orddict:store(Name, schema_rules(Elem), Schema);
schema_ruleset(_, Schema) -> Schema.

schema_rules(Elem) ->   
    lists:foldl(fun schema_rule/2, #schema{}, Elem#xmlElement.content).

schema_rule(Elem = #xmlElement{name=rule}, Rules) ->
    Class = attrib(Elem, class),
    Classes = ordsets:add_element(Class, Rules#schema.class),
    schema_prods(Elem, Rules#schema{class=Classes});
schema_rule(Elem = #xmlElement{name='copy-rule'}, Rules) ->
    Name = attrib(Elem, name),
    Rules#schema{copies = ordsets:add_element(Name, Rules#schema.copies)};
schema_rule(_, Links) -> Links.


schema_prods(Elem, Rules) ->
    lists:foldl(fun schema_prod/2, Rules, Elem#xmlElement.content).

schema_prod(Elem = #xmlElement{name=symbol}, Rules) ->
    Rules#schema{links = [{attrib(Elem, link), attrib(Elem, name)} |
                          Rules#schema.links ]};
schema_prod(Elem = #xmlElement{name=repeat}, Rules) ->
    schema_prods(Elem, Rules);
schema_prod(Elem = #xmlElement{name=optional}, Rules) ->
    schema_prods(Elem, Rules);
schema_prod(_, Rules) -> Rules.


schema_normal(Rulesets) ->
    lists:foldl(fun(Key, Acc) -> schema_normal(Key, Acc, Rulesets) end,
                [], orddict:fetch_keys(Rulesets)).

schema_normal(Head, Schema, Rulesets) ->
    Orig = orddict:fetch(Head, Rulesets),
    Info = merge_copies(Orig, Rulesets),
    Class = case Info#schema.class of
                [One] -> One;
                [] -> throw({no_class, Head});
                _ -> throw({multiple_class, Head})
            end,
    orddict:store(Head, #schema{class=Class, links=Info#schema.links}, Schema).
    

merge_copies(S=#schema{copies = C}, Rulesets) when length(C) > 0 ->
    merge_copies(S, C, Rulesets);
merge_copies(S, _) -> S.

merge_copies(Schema, [Copy | Rest], Rulesets) ->
    Info = merge_copies(orddict:fetch(Copy, Rulesets), Rulesets),
    merge_copies(
      Schema#schema{class = ordsets:union(Schema#schema.class,
                                          Info#schema.class),
                    links = Schema#schema.links ++ Info#schema.links},
      Rest, Rulesets);
merge_copies(Schema, [], _) ->
    Schema.


schema_links(Rulesets) ->
    lists:foldl(fun(Key, Schema) -> schema_links(Key, Rulesets, Schema) end,
                [], orddict:fetch_keys(Rulesets)).

schema_links(Head, Rulesets, Schema) ->
    #schema{class=Class, links=Links} = orddict:fetch(Head, Rulesets),
    Targets = case orddict:is_key(Class, Schema) of
                  true -> orddict:fetch(Class, Schema);
                  false -> []
              end,
    orddict:store(Class, schema_linkclass(Links, Targets, Rulesets), Schema).

schema_linkclass([], Targets, _) ->
    Targets;
schema_linkclass([{Tag, Target} | Rest], Targets, Rulesets) ->
    #schema{class=Class} = orddict:fetch(Target, Rulesets),
    T1 = case orddict:is_key(Tag, Targets) of
             false ->
                 orddict:store(Tag, Class, Targets);
             true ->
                 case orddict:fetch(Tag, Targets) of
                     Class ->
                         Targets;
                     Other ->
                         throw({link_conflict, Class, Other})
                 end
         end,
    schema_linkclass(Rest, T1, Rulesets).

    

%%% ========================= Generic ================================

%% Return the name attribute of a keyword element (maybe implicit)
keyword_name(Elem)->
    case attrib(Elem, name) of
        "" -> text_of(Elem);
        Name -> Name
    end.

%% Query an attribute of an XML element
attrib(Elem, Attr) ->
    case lists:keysearch(Attr,#xmlAttribute.name,Elem#xmlElement.attributes) of
        {value, AttrVal} -> AttrVal#xmlAttribute.value;
        _                -> ""
    end.

%% Query the textual contents of an XML element
text_of(Elem) ->
    lists:foldr(fun text_of/2, "", Elem#xmlElement.content).

text_of(#xmlText{value=Value}, Str) ->
    Value++Str;
text_of(_, Str) ->
    Str.

%% Compute the union of two #parsers
union(Lst) ->
    lists:foldr(fun union/2, #parser{}, Lst).

union(Elem, Parser) ->
    #parser{nonterm = Elem#parser.nonterm ++ Parser#parser.nonterm,
            term    = Elem#parser.term ++ Parser#parser.term,
            rules   = Elem#parser.rules ++ Parser#parser.rules
           }.

%% Insert a delimiter between members of an IOList
insert_delim([], _) -> [];
insert_delim([Last], _) -> [Last];
insert_delim([Head | Tail], Delim) -> [Head, Delim | insert_delim(Tail, Delim)].

