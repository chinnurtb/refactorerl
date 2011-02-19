%%% -*- mode: Erlang; coding: latin-1 -*-

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

Nonterminals
  Graph Params Syntax Lexicals Keywords
  Param ParamVal ParamMap
  Ruleset Rules Rule Attrs0 Attrs Attr Elements Element Const
  Lexical Regex Branch Piece Atom Classes Class
  KwSet KwTrans Transitions.

Terminals var atom integer string char dot
   '{' '}' '[' ']' '(' ')' '<' '>' '<<' '>>'
   '->' '|' '#'  '<-' '=' ',' '!' '-' '*' '+' '?' '.' ':'.

Rootsymbol Graph.

Graph -> Params Syntax Lexicals Keywords KwTrans :
             {'$1', '$2', '$3', '$4', '$5'}.

Params -> '$empty' : [].
Params -> Param Params : ['$1' | '$2'].

Param -> '[' var '=' ParamVal ']' : {value('$2'), '$4'}.

ParamVal -> atom : value('$1').
ParamVal -> integer : value('$1').
ParamVal -> string : value('$1').
ParamVal -> '{' ParamMap '}' : '$2'.

ParamMap -> '$empty' : [].
ParamMap -> atom ':' ParamVal ParamMap : [{value('$1'), '$3'} | '$4'].

Syntax -> Ruleset : ['$1'].
Syntax -> Ruleset Syntax : ['$1' | '$2'].

Ruleset -> var '->' Rules : #symrules{lhs=value('$1'), rules='$3'}.

Rules -> Rule : ['$1'].
Rules -> Rule '|' Rules : ['$1' | '$3'].

Rule -> '#' atom '{' Attrs0 '}' '(' Elements ')' :
            #rule{class = value('$2'), attrs = '$4', rhs   = '$7'}.
Rule -> var : #chain{ref = value('$1')}.

Attrs0 -> '$empty' : [].
Attrs0 -> Attrs : '$1'.

Attrs -> Attr : ['$1'].
Attrs -> Attr ',' Attrs : ['$1' | '$3'].

Attr -> atom '=' Const : #cattr{name=value('$1'), value='$3'}.
Attr -> atom '<-' atom : #tattr{name=value('$1'), token=value('$3')}.
Attr -> atom '<-' atom integer :
            #tattr{name=value('$1'), token=value('$3'), index=value('$4')}.

Elements -> Element : ['$1'].
Elements -> Element Elements : ['$1' | '$2'].

Element -> atom : #term{token=value('$1')}.
Element -> atom '->' var : #nonterm{name=value('$3'), link=value('$1')}.
Element -> '{' Elements '}' : #repeat{rule='$2'}.
Element -> '[' Elements ']' : #optional{rule='$2'}.

Const -> atom : value('$1').
Const -> integer : value('$1').
Const -> string : value('$1').


Lexicals -> Lexical : ['$1'].
Lexicals -> Lexical Lexicals : ['$1' | '$2'].

Lexical -> atom '->' Regex : {value('$1'), '$3'}.

Regex -> Branch : ['$1'].
Regex -> Branch '|' Regex : ['$1' | '$3'].

Branch -> Piece : ['$1'].
Branch -> Piece Branch : ['$1' | '$2'].

Piece -> Atom : '$1'.
Piece -> Atom '*'           : {rep, '$1'}.
Piece -> Atom '+'           : {rep1, '$1'}.
Piece -> Atom '?'           : {opt, '$1'}.

Atom -> var                 : {var,  value('$1')}.
Atom -> string              : {text, value('$1')}.
Atom -> char                : {text, [value('$1')]}.
Atom -> '[' Classes ']'     : {any,  '$2'}.
Atom -> '[' '!' Classes ']' : {anybut, '$3'}.
Atom -> dot                 : {any, []}.
Atom -> '.'                 : {any, []}.
Atom -> '(' Regex ')'       : {paren, '$2'}.
Atom -> '(' ')'             : {empty, []}.

Classes -> Class : ['$1'].
Classes -> Class Classes : ['$1' | '$2'].

Class -> string : value('$1').
Class -> char : [value('$1')].
Class -> char '-' char : {value('$1'), value('$3')}.


Keywords -> '$empty' : [].
Keywords -> '<<' var KwSet '>>' Keywords :
                [#keywset{name=value('$2'), keywords='$3'} | '$5'].

KwSet -> '$empty' : [].
KwSet -> string KwSet : [{value('$1'), list_to_atom(value('$1'))} | '$2'].
KwSet -> string ':' atom KwSet : [{value('$1'), value('$3')} | '$4'].
KwSet -> var KwSet : [value('$1') | '$2'].

KwTrans -> '$empty' : [].
KwTrans -> '<' var ':' Transitions '>' KwTrans : [{value('$2'), '$4'} |'$6'].

Transitions -> '*' '->' var : [value('$3')].
Transitions -> atom '->' var Transitions : [{value('$1'), value('$3')} | '$4'].

%%% ============================================================================
Erlang code.

-export([file/2]).

-include_lib("referl_gen/include/gen_export.hrl").

-record(cattr, {name, value}).
-record(tattr, {name, token, index=1}).
-type attrib() :: #cattr{} | #tattr{}.

-record(term,     {token :: atom()}).
-record(nonterm,  {name :: atom(), link :: atom()}).
-record(repeat,   {rule :: [_]}).
-record(optional, {rule :: [_]}).
-type rule_elem() :: #term{} | #nonterm{} | #repeat{} | #optional{}.

-record(rule, {lhs   :: atom(),
               class :: atom(),
               attrs :: [attrib()],
               rhs   :: [rule_elem()]}).
-record(chain, {ref  :: atom()}).

-record(symrules, {lhs   :: atom(),
                   rules :: [rule_elem()]}).

-record(keywset, {name :: atom(),
                  keywords :: [{string(), atom()} | atom()]}).

value({Type,  _Line})        -> Type;
value({_Type, _Line, Value}) -> Value.

-record(vattr, {name, index, eindex}).
-record(struct, {data :: {atom(), [#cattr{}]},
                 head, structure, attribs}).

file(File, Prefix) ->
    {ok, Text} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Text)),
    {ok, {Params, Syntax, Lexicals, Keywords, KwTrans}} = parse(Tokens),

    ets:new(rules,     [bag, named_table]),
    ets:new(terminals, [set, named_table]),
    ets:new(structure, [set, named_table, {keypos, #struct.data}]),
    ets:new(synschema, [bag, named_table]),

    try
        syntax(Params, Syntax),
        normalize_schema(),

        append_file(
          Prefix ++ "scanner.erl",
          fun(Dev) ->
                  print_scanner(Dev, Params, Lexicals, Keywords, KwTrans)
          end),
        prepend_file(
          Prefix ++ "parser.yrl",
          fun (Dev) -> print_parser(Dev, Params) end),
        append_file(
          Prefix ++ "nodes.erl",
          fun (Dev) -> print_nodes(Dev, Params) end),
        save_file(
          Prefix ++ "schema.hrl",
          fun (Dev) -> print_schema(Dev) end)

    after
        ets:delete(rules),
        ets:delete(terminals),
        ets:delete(structure),
        ets:delete(synschema)
    end.

save_file(Name, Printer) ->
    write_file(Name, "", Printer, "").

append_file(Name, Printer) ->
    write_file(Name, src_text(Name), Printer, "").

prepend_file(Name, Printer) ->
    write_file(Name, "", Printer, src_text(Name)).

write_file(Name, Head, Printer, Tail) ->
    case file:open(Name, [write]) of
        {ok, Dev} ->
            ok = file:write(Dev, Head),
            Printer(Dev),
            ok = file:write(Dev, Tail),
            ok = file:close(Dev);
        {error, Reason} ->
            io:format("~s: ~s~n", [Name, file:format_error(Reason)]),
            throw(Reason)
    end.

src_text(Name) ->
    case file:read_file(Name ++ "Src") of
        {ok, Text} -> Text;
        {error, Reason} ->
            io:format("~s: ~s~n", [Name++"Src",
                                   file:format_error(Reason)]),
            throw(Reason)
    end.


%%% ============================================================================
%%% Syntax analysis (result: parser rules, node structure)

syntax(Params, Syntax) ->
    {'LexLinks', LexLinks} = proplists:lookup('LexLinks', Params),
    lists:foreach(fun (SR) -> sym_rules(SR, LexLinks) end, Syntax).

sym_rules(#symrules{lhs=LHS, rules=Rules}, LL) ->
    lists:foreach(fun (R) -> sym_rule(LHS, R, LL) end, Rules).


sym_rule(LHS, #chain{ref=RHS}, _) ->
    ets:insert(
      rules,
      {LHS, atom_to_list(RHS), {atom, field(1)}}),

    ets:insert(synschema, {LHS, {ref, RHS}});

sym_rule(LHS, #rule{class=Class, attrs=Attrs, rhs=RHS}, LexLinks) ->
    {Class, Lex} = proplists:lookup(Class, LexLinks),
    DN = atom_to_list(LHS) ++ attr_dn(Attrs),
    CA = [A || A=#cattr{} <- Attrs],
    VA = [A || A=#tattr{} <- Attrs],
    try
        {Struct, Rules, Aux, Values} = proc_rule(RHS, DN, VA, 1, 1),
        validate_struct(Struct),

        ets:insert(
          rules,
          [{LHS, rule_text(Rule), builder(Class, Lex, CA++Values, Rule)} ||
              Rule <- Rules]),

        ets:insert(
          rules,
          [{Sym, rule_text(Rule), children(Rule, Lex)} || {Sym, Rule} <- Aux]),

        ets:insert(terminals, terminals(Struct)),

        ets:insert(
          synschema,
          [{LHS, {class, Class}} |
           lists:flatten([schema(Class, Elem, Aux)
                          || Rule <- Rules, Elem <- Rule])]),

        Str = #struct{data      = {Class, CA},
                      head      = data(Class, CA),
                      structure = {abstract, Struct},
                      attribs   = attr_value(Class, Lex, Values)},

        ets:insert_new(structure, Str) orelse
           ets:lookup(structure, Str#struct.data) =:= [Str] orelse
              throw({ambiguous_struct, Class, CA})
    catch
        throw:Err ->
            throw({DN, Err})
    end.

data(Class, Attrs) ->
    {record_expr,
     {atom, Class},
     [{record_field,
       {atom, Name},
       {abstract, Value}} || #cattr{name=Name, value=Value} <- Attrs] ++
     [{record_field,
       {atom, Name},
       {application, {atom, tv}, [{atom, field(EInd)}]}} ||
         #vattr{name=Name, eindex=EInd} <- Attrs]
    }.

builder(Class, Lex, Attrs, Rule) ->
    {application,
     {atom, build},
     [data(Class, Attrs), children(Rule, Lex)]
    }.

children(Rule, Lex) ->
    {list, [childref(Child, Lex) || Child <- Rule]}.

childref({Ind, #term{}}, Lex) ->
    {tuple, [{atom, Lex}, {application, {atom, tn}, [{atom, field(Ind)}]}]};
childref({Ind, #nonterm{link=Link}}, _) ->
    {tuple, [{atom, Link}, {atom, field(Ind)}]};
childref({Ind, _}, _) ->
    {atom, field(Ind)}.

attr_value(Class, Lex, Values) ->
    {list,
     [{tuple, [{tuple, [{atom, Lex}, {integer, Index}]},
               {record_index_expr, {atom, Class}, {atom, Name}}]} ||
         #vattr{name=Name, index=Index} <- Values]}.

rule_text([]) ->
    "'$empty'";
rule_text(Rule) ->
    string:join([name(Elem) || Elem <- Rule], " ").

name({_, #term{token=Token}}) -> io_lib:write(Token);
name({_, #nonterm{name=Sym}}) -> atom_to_list(Sym);
name({_, Name}) when is_atom(Name) -> atom_to_list(Name).

field(Ind) ->
    "$"++integer_to_list(Ind).

attr_dn(Attrs) ->
    lists:flatten([["_", atom_to_list(Val)] || #cattr{value=Val} <- Attrs]).

terminals([{token, T} | Tail]) -> [{T} | terminals(Tail)];
terminals([{optional, Opt}|Tail]) -> terminals(Opt)++terminals(Tail);
terminals([{repeat, Sep, _}|Tail]) -> [{Sep}|terminals(Tail)];
terminals([_|Tail]) -> terminals(Tail);
terminals([]) -> [].

schema(_,   {_, #term{}}, _) -> [];
schema(Cls, {_, #nonterm{link=Lnk, name=Sym}}, _) -> {{Cls, Lnk}, Sym};
schema(Cls, {_, Ref}, Aux) ->
    {value, {Ref, Rule}, Aux1} = lists:keytake(Ref, 1, Aux),
    [schema(Cls, E, Aux1) || E <- Rule].


-type index() :: pos_integer().
-type flat_rule() :: [{index(), #term{} | #nonterm{} | atom()}].
-spec proc_rule([rule_elem()], string(), [#tattr{}], index(), index()) ->
    {[tuple()], [flat_rule()], [{atom(), flat_rule()}], [#vattr{}]}.
proc_rule([#optional{rule=Opt} | Tail],
          DN, VA, ElemInd, LexInd) ->
    {OptStruct, _, OptAux, OptValues} =
        proc_rule(Opt, DN, VA, ElemInd, LexInd),
    OptValues == [] orelse throw(opt_values),

    {TailStruct, TailRules, TailAux, TailValues} =
        proc_rule(Tail, DN, VA, ElemInd, LexInd),

    {_, OptRules, _, _} =
        proc_rule(Opt ++ Tail, DN, VA, ElemInd, LexInd),

    {[{optional, OptStruct} | TailStruct],
     OptRules ++ TailRules,
     OptAux ++ TailAux, TailValues};

proc_rule([#nonterm{name=Sym, link=Link},
           #repeat{rule=[#term{token=Sep},
                         #nonterm{name=Sym, link=Link}]} | Tail],
          DN, VA, ElemInd, LexInd) ->
    {Struct, Rules, Aux, Values} = proc_rule(Tail, DN, VA, ElemInd+1, LexInd),
    AuxName = list_to_atom(DN ++ "@rep_" ++ atom_to_list(Link)),
    {[{repeat, Sep, Link} | Struct],
     [[{ElemInd, AuxName} | Rule] || Rule <- Rules],
     [{AuxName, [{1, AuxName},
                 {2, #term{token=Sep}},
                 {3, #nonterm{name=Sym, link=Link}}]},
      {AuxName, [{1, #nonterm{name=Sym, link=Link}}]} | Aux],
     Values};

proc_rule([#nonterm{link=Link} = Elem | Tail],
          DN, VA, ElemInd, LexInd) ->
    {Struct, Rules, Aux, Values} = proc_rule(Tail, DN, VA, ElemInd+1, LexInd),
    {[{symbol, Link} | Struct],
     [[{ElemInd, Elem} | Rule] || Rule <- Rules],
     Aux, Values};

proc_rule([#term{token=Token} = Elem | Tail],
          DN, VA, ElemInd, LexInd) ->
    {NewVal, NextVA} =
        case lists:keyfind(Token, #tattr.token, VA) of
            #tattr{index=1, name=Name} = F ->
                {[#vattr{name=Name, index=LexInd, eindex=ElemInd}], VA -- [F]};
            #tattr{index=N} = F ->
                {[], [F#tattr{index=N-1} | VA -- [F]]};
            _ ->
                {[], VA}
        end,
    {Struct, Rules, Aux, Values} =
        proc_rule(Tail, DN, NextVA, ElemInd+1, LexInd+1),
    {[{token, Token} | Struct],
     [[{ElemInd, Elem} | Rule] || Rule <- Rules],
     Aux,
     NewVal ++ Values};

proc_rule([Other | _], _, _, _, _) ->
    throw({bad_struct, Other});

proc_rule([], _, _, _, _) ->
    {[], [[]], [], []}.


%% Substitutes chain rule structure, checks inconsistent node classes
normalize_schema() ->
    Missing = ets:match(synschema, {'$1', {ref, '$2'}}),
    Known = [{Sym, Ref, Class} ||
                [Sym, Ref] <- Missing,
                {_, {class, Class}} <- ets:lookup(synschema, Ref)],
    lists:foreach(
      fun({Sym, Ref, Class}) ->
              ets:delete_object(synschema, {Sym, {ref, Ref}}),
              ets:insert(synschema, {Sym, {class, Class}}),
              Check = ets:match(synschema, {Sym, {class, '$1'}}),
              Check =:= [[Class]] orelse
                  throw({ambiguous_class, Sym, Check})
      end,
      Known),
    if
        Missing =:= [] ->
            normalize_links();
        Known =:= [] ->
            throw({unknown_class, Missing});
        true ->
            normalize_schema()
    end.

%% Replaces link target symbol names with link target classes, checks missing
%% symbol definitions
normalize_links() ->
    lists:foreach(
      fun([Class, Link, Sym]) ->
              case ets:lookup(synschema, Sym) of
                  [{Sym, {class, Target}}] ->
                      ets:delete_object(synschema, {{Class, Link}, Sym}),
                      ets:insert(synschema, {{Class, Link}, Target});
                  [] ->
                      throw({unknown_class, Sym})
              end
      end,
      ets:match(synschema, {{'$1', '$2'}, '$3'})).

%% Checks optional tokens
%% Checks invalid link order
validate_struct(Struct) ->
    Links = ?MISC:uniq(struct_links(Struct)),
    case lists:sort(Links) == lists:usort(Links) of
        false -> throw({multiple_link_slice, Links});
        true  -> ok
    end.

struct_links(Struct) ->
    lists:flatmap(
        fun
            ({token, _}) -> [];
            ({symbol, Lnk}) -> [Lnk];
            ({repeat, _, Lnk}) -> [Lnk];
            ({optional, Opt}) ->
                case struct_links(Opt) of
                    [] -> throw(opt_no_link);
                    OptLst -> OptLst
                end
        end,
        Struct).

%%% ============================================================================
%%% Source code generation


%% Parser

print_parser(Dev, Params) ->
    {'RootSym', RootSym} = proplists:lookup('RootSym', Params),
    io:put_chars(
      Dev,
      ["Nonterminals\n",
       prettypr:format(
         prettypr:nest(
           2, prettypr:text_par(
                string:join(
                  [atom_to_list(NT) || NT <- ets_keys(rules)], " ")))),
       ".\n\n",
       "Terminals\n",
       prettypr:format(
         prettypr:nest(
           2, prettypr:text_par(
                string:join([io_lib:write(Term) ||
                                {Term} <- ets:tab2list(terminals)], " ")))),
       ".\n\n",
       "Rootsymbol ", atom_to_list(RootSym), ".\n\n"]),
    lists:foreach(
      fun ({LHS, Children, Expr}) ->
              io:format(Dev,
                        "~s ->~n    ~s :~n~s.~n",
                        [LHS, Children, code(4, Expr)])
      end,
      ets:tab2list(rules)),
    io:put_chars(Dev, ["Erlang code.\n\n"]).

%% Node structure info

print_nodes(Dev, Params) ->
    Struct =
        {function,
         {atom, structure},
         [{clause, [Head], none, [Struct]} ||
             #struct{head=Head, structure=Struct} <- ets:tab2list(structure)] ++
         [{clause, [{variable, 'D'}], none,
           [{application, {module_qualifier, {atom, erlang}, {atom, error}},
             [{tuple, [{atom, unknown_structure},{variable,'D'}]}]}]}]},
    Attrs =
        {function,
         {atom, attribs},
         [{clause, [Head], none, [Att]} ||
             #struct{head=Head, attribs=Att} <- ets:tab2list(structure),
             Att =/= {list, []}] ++
         [{clause, [{underscore}], none, [{list, []}]}]},

    Lex =
        {function,
         {atom, lexlink},
         [{clause, [{atom, Class}], none, [{atom, L}]} ||
             {Class, L} <- proplists:get_value('LexLinks', Params)] ++
         [{clause, [{variable, 'C'}], none,
           [{application, {module_qualifier, {atom, erlang}, {atom, error}},
             [{tuple,[{atom, unknown_class},{variable,'C'}]}]}]}]},

    Links =
        lists:foldl(
          fun ({Child, Link}, P) -> orddict:append(Child, Link, P) end,
          [],
          lists:usort([{Child, Link} || {_, Link, Child} <- schema_links()])),

    Parent =
        {function,
         {atom, parentlink},
         [{clause, [{atom, Class}], none,
           [{abstract, proplists:get_value(Class, Links, [])}]} ||
             {Class, _} <- proplists:get_value('LexLinks', Params)] ++
         [{clause, [{variable,'C'}], none,
           [{application, {module_qualifier, {atom, erlang}, {atom, error}},
             [{tuple, [{atom, unknown_class},{variable,'C'}]}]}]}]},

    io:put_chars(
      Dev,
      [code(Struct), "\n\n",
       code(Attrs), "\n\n",
       code(Lex), "\n\n",
       code(Parent), "\n"]).

%% Graph schema

print_schema(Dev) ->
    Links = schema_links(),
    Schema =
        {attribute, {atom, define},
         [{variable, 'SYNTAX_SCHEMA'},
          {list, [{tuple, [{atom, root}, {list, []},
                           {list, [{tuple, [{atom, file},{atom,file}]}]}]} |
                  schema_desc([{file, form, form} | lists:sort(Links)])]}]},
    io:put_chars(Dev, [code(Schema), "\n"]).

schema_desc([{FCl, _, _} | _] = Lst) ->
    {Desc, Tail} = schema_desc(FCl, Lst, []),
    [{tuple, [{atom, FCl},
              {application,
               {atom, record_info},
               [{atom, fields}, {atom, FCl}]},
              {abstract, Desc}]} | schema_desc(Tail)];
schema_desc([]) ->
    [].

schema_desc(FCl, [{FCl, Lnk, TCl} | Tail], Lst) ->
    schema_desc(FCl, Tail, [{Lnk, TCl} | Lst]);
schema_desc(_C, Tail, Lst) ->
    {lists:reverse(Lst), Tail}.


schema_links() ->
    [case ets:lookup(synschema, Key) of
         [{_, To}] -> {FC, Lnk, To};
         Clash -> throw({ambiguous_schema, FC, Lnk, [To || {_,To} <- Clash]})
     end ||
        {FC, Lnk}=Key <- ets_keys(synschema)].


%% Scanner

print_scanner(Dev, Params, Lexicals, Keywords, KwTrans) ->
    Table =
        refgen_scanc:table(
          [{lex_name(Name), lists:flatten(regex_text(Regex))} ||
              {Name, Regex} <- Lexicals]),

    {'KeywordStart', Start} = proplists:lookup('KeywordStart', Params),
    io:put_chars(
      Dev,
      [code(keyword_fun(Keywords)), "\n",
       code(next_fun(KwTrans)), "\n",
       code({function, {atom, start},
             [{clause, [], none, [{atom, Start}]}]}), "\n",
       code({function, {atom, table},
             [{clause, [], none, [{abstract, Table}]}]}), "\n"
      ]).



lex_name(Name) ->
    case atom_to_list(Name) of
        S=[C|_] when C >= $A, C =< $Z -> S;
        _ -> Name
    end.

regex_text(Branches) ->
    string:join([piece_text(Piece) || Piece <- Branches], "|").
piece_text(Seq) ->
    [re_elem_text(El) || El <- Seq].

re_elem_text({rep,    RE}) -> [re_elem_text(RE), "*"];
re_elem_text({rep1,   RE}) -> [re_elem_text(RE), "+"];
re_elem_text({opt,    RE}) -> [re_elem_text(RE), "?"];
re_elem_text({text,   T})  -> escape(T);
re_elem_text({any,    Cl}) -> ["[",  chrclass(Cl), "]"];
re_elem_text({anybut, Cl}) -> ["[^", chrclass(Cl), "]"];
re_elem_text({paren,  RE}) -> ["(",  regex_text(RE), ")"];
re_elem_text({var,    V})  -> ["{", atom_to_list(V), "}"];
re_elem_text({empty,  []}) -> "".

escape([Head | Tail]) ->
    [escape(Head) | escape(Tail)];
escape([]) -> [];
escape(Char) when Char < 32; Char > 127 ->
    io_lib:format("\\~3.8.0b", [Char]);
escape(Char)
  when Char =:= $*; Char =:= $+; Char =:= $\\;
       Char =:= $.; Char =:= $$; Char =:= $^;
       Char =:= $(; Char =:= $); Char =:= $?;
       Char =:= $[; Char =:= $]; Char =:= $|;
       Char =:= ${; Char =:= $};
       Char =:= $\" -> [$\\, Char];
escape(Char) -> Char.

chrclass(Lst) when is_list(Lst) ->
    [chrclass(C) || C <- Lst];
chrclass({From, To}) ->
    [chrclass(From), "-", chrclass(To)];
chrclass(Char)  when Char < 32; Char > 127 ->
    io_lib:format("\\~3.8.0b", [Char]);
chrclass(Char) when Char =:= $\\; Char =:= $-; Char =:= $^; Char =:= $] ->
    [$\\, Char];
chrclass(Char) -> Char.


keyword_fun(KeySets) ->
    {function,
     {atom, keyword},
     [{clause,
       [{atom, Set}, {underscore}, {string, Text}], none, [{atom, Name}]} ||
         #keywset{name=Set} <- KeySets,
         {Text, Name} <- lists:flatten(keyword_set(Set, KeySets))] ++
     [{clause,
       [{underscore}, {variable, 'T'}, {underscore}], none, [{variable, 'T'}]
      }]
    }.

keyword_set(Set, KeySets) ->
    #keywset{keywords=Keywords} = lists:keyfind(Set, #keywset.name, KeySets),
    [case Kw of
         {_, _} -> Kw;
         Ref -> keyword_set(Ref, KeySets)
     end || Kw <- Keywords].


next_fun(KwTrans) ->
    {function, {atom, next},
     [next_clause(State, Trans) ||
         {State, Transitions} <- KwTrans,
         Trans <- Transitions]}.

next_clause(State, {Token, Next}) ->
    {clause, [{atom, State}, {atom, Token}], none, [{atom, Next}]};
next_clause(State, Next) ->
    {clause, [{atom, State}, {underscore}], none, [{atom, Next}]}.


ets_keys(Tab) -> ets_keys(Tab, ets:first(Tab)).
ets_keys(_Tab, '$end_of_table') -> [];
ets_keys(Tab, Key) -> [Key | ets_keys(Tab, ets:next(Tab, Key))].


%%% ============================================================================
%%% Erlang source code generation

%% Code description format: {FunName, Arg1, ..., ArgN}
%%
%% `FunName' is an atom, `ArgI' is either a constant or a code description
%% (possibly nested in lists).
%%
%% `referl_syntax:FunName(Arg1, ..., ArgN)' is called recursively on thi
%% structure to obtain a syntax tree, which is then pretty printed.

-type code_desc() :: tuple().

-spec code(code_desc()) -> string().
code(Desc) -> code(0, Desc).

-spec code(non_neg_integer(), code_desc()) -> string().
code(Ind, Desc) ->
    prettypr:format(
      prettypr:nest(Ind,
                    erl_prettypr:best(construct_code(Desc)))).

construct_code({abstract, Data}) ->
    erl_syntax:abstract(Data);
construct_code(Call) when is_tuple(Call) ->
    [Type | Args] = tuple_to_list(Call),
    apply(erl_syntax, Type, construct_code(Args));
construct_code(Lst) when is_list(Lst) ->
    [construct_code(El) || El <- Lst];
construct_code(Data) ->
    Data.

