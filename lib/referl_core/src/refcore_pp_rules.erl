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
%%% This module is a library of the Pretty Printer to handle the different 
%%% formatting rule types. 
%%% Supply the default formatting rules and configurations of the Erlang 
%%% language. There are several function to get the indentation values, the 
%%% required space size between tokens, the recommended line breakers, the 
%%% long line breakers and the line bbreakers of the multiple line terms.
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(refcore_pp_rules).
-vsn("$Rev$").

%%% ============================================================================
%%% Exports

% Formatting rules for Erlang
-export([erlang/0, erlang_rules/0]).
% Load formatting rules into ETS
-export([check_load/1, reset/1, load/2, delete/1]).
% Key fields of nodes
-export([parentDataKeyFields/1, childDataKeyFields/1, dataKeyFields/1]).
% Find a specified children of a node
-export([find_child_path/5]).
% Indentations
-export([get_indent/3]).
% White spaces & line breakers
-export([get_token_rules/3,
         merge_token_rules/2, class_token_rulesR/1, class_token_rules/1,
         get_token_rule/2, get_token_rule_between/3]).


-include("core.hrl").
-include("refcore_pp.hrl").


%%% ============================================================================
%%% Defines

-define(LongNlRules, [{nl,0,1}]).



%%% ============================================================================
%%% Formatting rules for Erlang

%% @spec erlang() -> {Config::ppConfig(), ConfigRulesFun::RulesFun}
%%       RulesFun = (() -> {IndentRules, WhiteSpaceRules, LongLineRules,
%%                          MultipleLineTermRules})
%%       IndentRules = [indentRule()]
%%       WhiteSpaceRules = [tokenRule()]
%%       LongLineRules = [tokenRule()]
%%       MultipleLineTermRules = [tokenRule()]
%% @doc Give back the default formatting configuration for the Erlang language.
%% @see erlang_rules/0
erlang() ->
    {#ppConfig{
        % General
        general = #ppConfig_general{
            useTab        = false,          % bool()
            tabSize       = 4,              % natural()
            optLineLength = 80,             % natural()
            minTextChars  = 30,             % natural()
            maxTextChars  = 75,             % natural()
            nlStr         = "\n",           % string()
            commentLines  = ["%"]           % [string()]
        },
        indent  = {pp_in_cd, pp_in_c},
        ws_nl   = {pp_wsnl_cdt,  pp_wsnl_ct,  {pp_wsnl_cd, [bag]}},
        longNl  = {pp_long_cdt,  pp_long_ct,  {pp_long_cd, [bag]}},
        multiNl = {pp_multi_cdt, pp_multi_ct, {pp_multi_cd,[bag]}}
     },
     fun erlang_rules/0}.


%% @spec erlang_rules() -> {IndentRules, WhiteSpaceRules, LongLineRules,
%%                          MultipleLineTermRules}
%%       IndentRules = [indentRule()]
%%       WhiteSpaceRules = [tokenRule()]
%%       LongLineRules = [tokenRule()]
%%       MultipleLineTermRules = [tokenRule()]
%% @doc Give back the default formatting rules for the Erlang language.
erlang_rules() ->
    {
    % --- Indentations ------------------------------------------
    %#region % indentations
     [
        % Form
        #indentRule{
            parent = [{form,func}],
            indent = []},
        #indentRule{
            parent = [{form,lex}], % Empty form (eol)
            indent = []},
        #indentRule{
            parent = [form], % Attribute form
            indent = [{from, {flex,3}, % op_paren
                       to,   last,     % stop token
                       1}]},
        % Clause
        #indentRule{
            parent = [{clause,fundef},{clause,funexpr}],
            indent = [{'after', {clex,1}, % first parameter
                       before,  {body,1}, % arrow
                       2},
                      {from,    {body,1}, % first body
                       to,      last,     % last body
                       1}]},
        #indentRule{
            parent = [{clause,guard},{clause,timeout},{clause,pattern}],
            indent = [{from, {body,1}, % first body
                       to,   last,     % last body
                       1}]},
        % Expression
        #indentRule{
            parent = [{expr,match_expr},{clause,send_expr}],
            indent = [{from, {esub,2}, % right part
                       to,   last,    % right part
                       1}]},
        #indentRule{
            parent = [{expr,arglist}],
            indent = [{'after', {elex,1}, % first parameter
                       to,      last,     % cl_paren
                       1}]},
        #indentRule{
            parent = [{expr,field_list}], %under rec_expr
            indent = [{from, {elex,1   }, % op_brace
                       to,   {elex,last}, % cl_brace
                       1}]},
        #indentRule{
            parent = [{expr,block_expr}],
            indent = [{from, {exprcl,1},    % block content
                       to,   {exprcl,1},    % block content
                       1}]},
        #indentRule{
            parent = [{expr,if_expr}],
            indent = [{from, {exprcl,1},    % first expression clause
                       to,   {exprcl,last}, % last expression clause
                       1}]},
        #indentRule{
            parent = [{expr,case_expr}],
            indent = [{from, {headcl,1},    % head clause
                       to,   {headcl,1},    % head clause
                       1},
                      {from, {exprcl,1},    % first expression clause
                       to,   {exprcl,last}, % last expression clause
                       1}]},
        #indentRule{
            parent = [{expr,receive_expr}],
            indent = [{from, {exprcl,1},    % first expression clause (OPTIONAL)
                       to,   {exprcl,last}, % last expression clause (OPTIONAL)
                       1},
                      {from, {aftercl,1},   % after clause (OPTIONAL)
                       to,   {aftercl,1},   % after clause (OPTIONAL)
                       1}]},
        #indentRule{
            parent = [{expr,try_expr}],
            indent = [{from, {headcl,1},    % head clause
                       to,   {headcl,1},    % head clause
                       1},
                      {from, {exprcl,1},    % first expression clause (OPTIONAL)
                       to,   {exprcl,last}, % last expression clause (OPTIONAL)
                       1},
                      {from, {catchcl,1},    % first catch clause (OPTIONAL)
                       to,   {catchcl,last}, % last catch clause (OPTIONAL)
                       1},
                      {from, {aftercl,1},    % after clause (OPTIONAL)
                       to,   {aftercl,1},    % after clause (OPTIONAL)
                       1}]},
        % Lexical element
        #indentRule{
            parent = [{lex, arg}], % macro definition parameter
            indent = [{'after', first, % first parameter
                       to,      last,  % cl_paren
                       1}]},
        %#indentRule{
        %    parent = [{lex, subst}], % macro subsituation
        %    indent = [{'after', {llex,3}, % op_paren (OPTIONAL)
        %               to,      last,     % cl_paren
        %               1}]},
        #indentRule{
            parent = [{lex, incl}], % include attribute
            indent = [{'after', {llex,3}, % op_paren (OPTIONAL)
                       to,      last,     % cl_paren
                       1}]}
     ],
    %#endregion % indentations

    % --- White space and required line breakers ----------------
    %#region % white spaces and required line breakers
     [
        % Lexical element
        %#tokenRule{
        %    parents = [{lex, subst}], % Macro subsituation
        %    rules   = [{[','],              [{ws,0,1}]} ]},
        %#tokenRule{
        %    parents = [{lex, subst}], % Macro subsituation
        %    rules   = [{[',',{position,last}],   [{ws,0,1}]},
        %               {[{position,first}],      [{ws,1,0}]} ]},
        #tokenRule{
            parents = [{lex, arg}],   % macro definition parameter
            rules   = [{[','],                [{ws,0,1}]}]},
        #tokenRule{
            parents = [{lex, body}],  % macro definition body
            rules   = [{[{position,any}],     [{ws,1,1}]}]},
        #tokenRule{
            parents = [{lex,incl}],   % include attribute
            rules   = [{['-'],                  [{ws,1,0},{nl,1,0}]},
                       {[stop],               [{ws,0,1},{nl,0,1}]}]},
        #tokenRule{
            parents = [{lex,incl}],
            rules   = [
                {[{check_fun,diff_next_form_gen(-1,lex,include),'-'}],
                 {[{ws,1,0},{nl,2,0}],[{ws,1,0},{nl,1,0}]} },
                {[{check_fun,diff_next_form_gen(+1,lex,include),stop}],
                 {[{ws,0,1},{nl,0,2}],[{ws,0,1},{nl,0,1}]} } ]},
        % Unary operator
        #tokenRule{
            parents = [{expr, prefix_bit_expr}, {expr, prefix_expr}],
            rules   = [{['+','-'],                   [{ws,1,0}]},
                       {['not','bnot'],              [{ws,1,1}]} ]},
        #tokenRule{
            parents = [{expr, catch_expr}],
            rules   = [{['catch'],            [{ws,0,1}]} ]},
        % Binary operator
        #tokenRule{
            parents = [{expr, infix_expr}],
            rules = [
                {['+','-','*','/','++','--'], [{ws,0,0}]},
                {['and','andalso','or','orelse','xor'],      [{ws,1,1}]},
                {['band','bor','bsl','bsr','bxor'],          [{ws,1,1}]},
                {['div','rem'],                              [{ws,1,1}]},
                {[',',';'],                                  [{ws,0,1}]},
                {[':'],                                      [{ws,0,0}]},
                {['<','>','=<','>=','==',
                  '=:=','/=','=/='],                         [{ws,0,0}]} ]},
        % Expression
        #tokenRule{
            parents = [{expr, block_expr}],
            rules   = [{['begin'],    [{ws,0,1},{nl,0,1}]},
                       {['end'],      [{ws,1,0},{nl,1,0}]} ]},
        #tokenRule{
            parents = [{expr, if_expr}],
            rules   = [{['if'],       [{ws,0,1},{nl,0,1}]},
                       {[';'],        [{ws,0,1},{nl,0,1}]},
                       {['end'],      [{ws,1,0},{nl,1,0}]} ]},
        #tokenRule{
            parents = [{expr, case_expr}],
            rules   = [{['case'],            [{ws,0,1}]},
                       {['of'],              [{ws,1,1},{nl,0,1}]},
                       {[';'],               [{ws,0,1},{nl,0,1}]},
                       {['end'],             [{ws,1,0},{nl,1,0}]} ]},
        #tokenRule{
            parents = [{expr, receive_expr}],
            rules   = [{['receive'],    [{ws,0,1},{nl,0,1}]},
                       {[';'],          [{ws,0,1},{nl,0,1}]},
                       {['after'],      [{ws,1,1},{nl,1,1}]},
                       {['end'],        [{ws,1,0},{nl,1,0}]} ]},
        #tokenRule{
            parents = [{expr, try_expr}],
            rules   = [{['try'],                    [{ws,0,1},{nl,0,1}]},
                       {[';'],                      [{ws,0,1},{nl,0,1}]},
                       {['of','catch','after'],     [{ws,1,1},{nl,1,1}]},
                       {['end'],                    [{ws,1,0},{nl,1,0}]} ]},
        #tokenRule{
            parents = [{expr, fun_expr}],
            rules   = [{['fun'],             [{ws,0,0}]},
                       {['end'],             [{ws,1,0}]}]},
        #tokenRule{
            parents = [{expr, implicit_fun}],
            rules   = [{['fun'],             [{ws,0,1}]}]},
        #tokenRule{
            parents = [{expr, atom}], % function name
            rules   = [
                {[{check_fun,fun funcform_first_name/1,atom}],  [{nl,1,0}]}]},
        #tokenRule{
            parents = [{expr, mstring}],
            rules   = [{[{before_link,esub,string}],     [{ws,0,1}]} ]},
        #tokenRule{
            parents = [{expr, match_expr}],
            rules   = [{['='],                        [{ws,1,1}]}]},
        #tokenRule{
            parents = [expr],
            rules   = [{['cond','let','query'],         [{ws,0,1}] },
                       {[',',';'],                      [{ws,0,1}]},
                       {['(',')',
                         '{','}',
                         '[',']'],                      [{ws,0,0}]},
                       {['<<','>>'],                    [{ws,0,0}]},
                       {['.','<-','<='],                [{ws,0,0}]},
                       {['#'],                          [{ws,0,0}]},
                       {['|','||','!'],                 [{ws,1,1}]} ]},
        % Clause
        #tokenRule{
            parents = [{clause,fundef},{clause,guard},{clause,timeout},
                       {clause,pattern}],
            rules   = [{[{before_link,body,','}],     [{ws,0,1},{nl,0,1}]} ]},
        #tokenRule{
            parents = [{clause,funexpr}],
            rules   = [{[{before_link,body,','}],     [{ws,0,1}]} ]},
        #tokenRule{
            parents = [clause],
            rules   = [{['(',')'],                     [{ws,0,0}]},
                       {['when','->'],                 [{ws,1,1}]},
                       {[','],                         [{ws,0,1}]} ]},
        % Form
        #tokenRule{
            %parents = [{form, {attrib,module}}],
            parents = [{form, module}],
            rules   = [{['-'],         [{ws,0,0}]},
                       {[stop],        [{ws,0,1},{nl,0,2}]} ]},
        #tokenRule{
            parents = [{form,record}],
            rules   = [{['-'],         [{ws,1,0},{nl,2,0}]},
                       {[stop],        [{ws,0,1},{nl,0,2}]} ]},
        #tokenRule{
            parents = [{form,{attrib,vsn}},
                       {form,{attrib,behaviour}},{form,{attrib,file}}],
            rules   = [{['-'],         [{ws,1,0},{nl,2,0}]},
                       {[stop],        [{ws,0,1},{nl,0,2}]} ]},
        #tokenRule{
            parents = [{form,{attrib,ifdef}},{form,{attrib,ifndef}}],
            rules   = [{['-'],         [{ws,1,0},{nl,2,0}]},
                       {[stop],        [{ws,0,1},{nl,0,1}]} ]},
        #tokenRule{
            parents = [{form,{attrib,endif}}],
            rules   = [{['-'],         [{ws,1,0},{nl,1,0}]},
                       {[stop],        [{ws,0,1},{nl,0,2}]} ]},
        #tokenRule{
            parents = [{form, func}],
            rules   = [{[';'],         [{ws,0,1},{nl,0,1}]},
                       {[stop],        [{ws,0,1},{nl,0,1}]} ]},
        #tokenRule{
            parents = [{form, export}],
            rules   = [
                {[{check_fun,diff_next_form_gen(-1,export,undefined),'-'}],
                 {[{ws,1,0},{nl,2,0}],[{ws,1,0},{nl,1,0}]} },
                {[{check_fun,diff_next_form_gen(+1,export,undefined),stop}],
                 {[{ws,0,1},{nl,0,2}],[{ws,0,1},{nl,0,1}]} } ]},
        #tokenRule{
            parents = [{form, import}],
            rules   = [
                {[{check_fun,diff_next_form_gen(-1,import,undefined),'-'}],
                 {[{ws,1,0},{nl,2,0}],[{ws,1,0},{nl,1,0}]} },
                {[{check_fun,diff_next_form_gen(+1,import,undefined),stop}],
                 {[{ws,0,1},{nl,0,2}],[{ws,0,1},{nl,0,1}]} } ]},
        #tokenRule{
            parents = [form],
            rules   = [{[','],         [{ws,0,1}         ]},
                       {['-'],         [{ws,1,0},{nl,1,0}]},
                       {[stop],        [{ws,0,1},{nl,0,1}]} ]}
     ],
    %#endregion % white spaces and required line breakers

    % --- Line breakers for long lines --------------------------
    %#region % long line breakers
     [
        % Lexical element
        #tokenRule{
            parents = [{lex, subst}], % macro subsituation
            rules   = [{[','],              ?LongNlRules}]},
        #tokenRule{
            parents = [{lex, arg}],   % macro definition parameter
            rules   = [{[','],              ?LongNlRules}]},
        #tokenRule{
            parents = [{lex, body}],  % macro definition body
            rules   = [{['+','-','*','++','--',
                         'and','andalso','or','orelse','xor',
                         'band','bor','bsl','bsr','bxor',
                         'div','rem',
                         ',',';',
                         '<','>','=<','>=','==',
                         '=:=','/=','=/=',
                         '|','||','!','=',
                         '->'],
                        ?LongNlRules}]},
        % Binary operator
        #tokenRule{
            parents = [{expr, infix_expr}],
            rules   = [{['+','-','*','++','--',
                         {check_fun,fun no_impexp_slash/1,'/'},
                         'and','andalso','or','orelse','xor',
                         'band','bor','bsl','bsr','bxor',
                         'div','rem',
                         ',',';',
                         '<','>','=<','>=','==',
                         '=:=','/=','=/='],
                        ?LongNlRules}]},
        % Expression
        #tokenRule{
            parents = [{expr, arglist}],
            rules   = [{['(', ','],                      ?LongNlRules}]},
        #tokenRule{
            parents = [{expr, fun_expr}],
            rules   = [{[';'],                           ?LongNlRules}]},
        #tokenRule{
            parents = [{expr, mstring}],
            rules   = [{[{before_link,esub,string}],     ?LongNlRules}]},
        #tokenRule{
            parents = [{expr,match_expr}],
            rules   = [{['='],                           ?LongNlRules}]},
        #tokenRule{
            parents = [expr],
            rules   = [{[',','|','||','!'],              ?LongNlRules}]},
        % Clause
        #tokenRule{
            parents = [{clause,fundef},{clause, funexpr},{clause,guard},
                       {clause,timeout},{clause,pattern}],
            rules   = [{['->'],                          ?LongNlRules}]},
        #tokenRule{
            parents = [{clause, funexpr}],
            rules   = [{[{before_link,body,','}],        ?LongNlRules}]},
        #tokenRule{
            parents = [clause],
            rules   = [{[',',')','when'],                ?LongNlRules}]},
        % Form
        #tokenRule{
            parents = [form],
            rules   = [{[','],                         ?LongNlRules}]}
     ],
    %#endregion % long line breakers

    % --- Extra line breakers for long terms --------------------
    %#region % terms in multiple lines
     [
        % function expression
        #tokenRule{
            parents = [{expr, fun_expr}],
            rules   = [{[';'],                         [{nl,0,1}]},
                       {['end'],                       [{nl,1,0}]}]},
        % clause
        #tokenRule{
            parents = [{clause,fundef},{clause, funexpr},{clause,guard},
                       {clause,timeout},{clause,pattern}],
            rules   = [{['->'],        [{nl,0,1}]}]},
        #tokenRule{
            parents = [{clause, funexpr}],
            rules   = [{[{before_link,body,','}],      [{nl,0,1}]}]}
     ]
    %#endregion % terms in multiple lines
    }.


% Check `Token' is the name of the first clause of a function form
funcform_first_name(Token) ->
    [Token] == ?Graph:path(Token, [{elex,back},{name,back},{funcl,back},
                                   {funcl,1},  {name,1},   {elex,1}]).

% Check `Token' is not slash in an import or export list
no_impexp_slash(Token) ->
    [] == ?Graph:path(Token, [{elex,back},{esub,back},
            {{eattr,back}, {{type,'==',import},'or',{type,'==',export}
                           }}]).

% Check if the neighbour form is diffrent from this form
diff_next_form_gen(Diff, Type, Tag) ->
    fun(Token) ->
        [{file,FileNode},{form,FormNode1}|_] = ?Syn:root_path(Token),
        FormLinkIdx1 = ?Graph:index(FileNode,form,FormNode1),
        case ?Graph:path(FileNode, [{form,FormLinkIdx1+Diff}]) of
            [FormNode2] ->
                FormData2 = ?Graph:data(FormNode2),
                Type/=FormData2#form.type orelse 
                (undefined/=Tag andalso Tag/=FormData2#form.tag);
            [] -> false
        end
    end.



%%% ============================================================================
%%% Load formatting rules into ETS tables

%% @spec check_load(Config_Rules::{Config::ppConfig(), RulesFun}) -> 
%%           Config::ppConfig()
%%       RulesFun = (() -> {IndentRules, WhiteSpaceRules, LongLineRules,
%%                          MultipleLineTermRules})
%%       IndentRules = [indentRule()]
%%       WhiteSpaceRules = [tokenRule()]
%%       LongLineRules = [tokenRule()]
%%       MultipleLineTermRules = [tokenRule()]
%% @doc Check if the formatting rules are loaded into ETS tables, specified in
%%      `Config' and return the `Config' configuration. If tables are not exist
%%      load rules provided by `RulesFun' into ETS tables.
%% @see erlang/0
%% @see erlang_rules/0
check_load({Config=#ppConfig{}, RulesFun}) ->
    case ?MISC:ets_exist(element(1,Config#ppConfig.indent)) of
        false -> load(Config, RulesFun());
        _     -> clear_table_names(Config)
    end.

%% @spec reset(Config_Rules::{Config::ppConfig(), RulesFun}) -> 
%%           Config::ppConfig()
%%       RulesFun = (() -> {IndentRules, WhiteSpaceRules, LongLineRules,
%%                          MultipleLineTermRules})
%%       IndentRules = [indentRule()]
%%       WhiteSpaceRules = [tokenRule()]
%%       LongLineRules = [tokenRule()]
%%       MultipleLineTermRules = [tokenRule()]
%% @doc Check if the formatting rules are loaded into ETS tables, specified in
%%      `Config' and return the `Config' configuration.
%%      If tables are exist delete them. If tables are not exist load rules
%%      provided by `RulesFun' into ETS tables.
%% @see erlang/0
%% @see erlang_rules/0
reset({Config=#ppConfig{}, RulesFun}) ->
    case ?MISC:ets_exist(element(1,Config#ppConfig.indent)) of
        true -> delete(Config);
        _    -> ok
    end,
    load(Config, RulesFun()).


%% @spec load(Config::ppConfig(), Rules::{IndentRules, WhiteSpaceRules, 
%%               LongLineRules, MultipleLineTermRules}) -> ppConfig()
%%       IndentRules = [indentRule()]
%%       WhiteSpaceRules = [tokenRule()]
%%       LongLineRules = [tokenRule()]
%%       MultipleLineTermRules = [tokenRule()]
%% @doc Load indent, white space and new line, long line breaker and
%%      multi line term breaker rules into named ETS tables specified by
%%      `Config'.
%% @see check_load/1
load(Config=#ppConfig{indent=IndentETSs, ws_nl=WsNlETSs, longNl=LongETSs,
        multiNl=MultiETSs}, {IndentRules, WsNlRules, LongRules, MultiRules}) ->
    EtsOptions = [named_table],
    load_in(IndentRules, create_tables(IndentETSs, EtsOptions)),
    load_tok(WsNlRules,  create_tables(WsNlETSs,   EtsOptions)),
    load_tok(LongRules,  create_tables(LongETSs,   EtsOptions)),
    load_tok(MultiRules, create_tables(MultiETSs,  EtsOptions)),
    clear_table_names(Config).


%% @spec delete(Config::ppConfig()) -> ok
%% @doc Delete indent, white space and new line, long line breaker and
%%      multi line term breaker rules from ETS specified by `Config'.
delete(#ppConfig{indent=InTables, ws_nl=WsNlTables, longNl=LongTables,
        multiNl=MultiTables}) ->
    delete_tables(InTables),
    delete_tables(WsNlTables),
    delete_tables(LongTables),
    delete_tables(MultiTables),
    ok.


%% @spec clear_table_names(Config::ppConfig()) -> ok
%% @doc Remove the extra options from the table names.
clear_table_names(Config=#ppConfig{indent=IndentETSs, ws_nl=WsNlETSs,
        longNl=LongETSs, multiNl=MultiETSs}) ->
    Config#ppConfig{
        indent  = clear_table_names_(IndentETSs),
        ws_nl   = clear_table_names_(WsNlETSs),
        longNl  = clear_table_names_(LongETSs),
        multiNl = clear_table_names_(MultiETSs)}.

%
clear_table_names_(TableNamesT) ->
    TableNames = lists:map(
        fun({TableName, _ExtraOpts}) -> TableName;
        (TableName) -> TableName
        end,
        tuple_to_list(TableNamesT)),
    list_to_tuple(TableNames).
    
    
    
% Create ETS tables for formatting rules
create_tables(TableNamesT, Options) when is_tuple(TableNamesT) ->
    RevTableNames = lists:foldl(
        fun({TableName, ExtraOpts}, Acc) ->
            ets:new(TableName, ExtraOpts++Options),
            [TableName|Acc];
        (TableName, Acc) ->
            ets:new(TableName, Options),
            [TableName|Acc]
        end,
        [],
        tuple_to_list(TableNamesT)),
    list_to_tuple(lists:reverse(RevTableNames)).

% Delete ETS tables
delete_tables(TableNamesT) ->
    lists:foreach(
        fun({TableName, _ExtraOpts}) -> ets:delete(TableName);
        (TableName) -> ets:delete(TableName)
        end,
        tuple_to_list(TableNamesT)).


%% Load indent rules into named ETS tables
load_in([], _InETSs) -> ok;
load_in([#indentRule{parent=Parents, indent=Indent}|IndentRules],
         {InCD_ETS, InC_ETS}) ->
    lists:foreach(
        fun({ParentClass, ParentKeyValue}) ->
            ets:insert(InCD_ETS, {{ParentClass, ParentKeyValue}, Indent});
        (ParentClass) ->
            ets:insert(InC_ETS,  {ParentClass, Indent})
        end,
        Parents),
    load_in(IndentRules, {InCD_ETS, InC_ETS}).


%% Load white space & new line, long line breaker and multi line term breaker
%% rules into named ETS tables
load_tok([], _TokETSs) -> ok;
load_tok([#tokenRule{parents=Parents, rules=Rules}|TokenRules],
        {TokCDT_ETS, TokCT_ETS, TokCD_ETS}) ->
    lists:foreach(
        fun(Parent) ->
            load_tok_(Rules, Parent, {TokCDT_ETS, TokCT_ETS, TokCD_ETS})
        end,
        Parents),
    load_tok(TokenRules, {TokCDT_ETS, TokCT_ETS, TokCD_ETS}).

% Load token rules into named ETS tables
load_tok_([],_Parent, _TokETSs) -> ok;
load_tok_([{Childs, Rule}| Rules], Parent, {TokCDT_ETS,TokCT_ETS,TokCD_ETS}) ->
    lists:foreach(
        fun(Child) ->
            load_tok_add(Parent, Child, Rule, {TokCDT_ETS,TokCT_ETS,TokCD_ETS})
        end, Childs),
    load_tok_(Rules, Parent, {TokCDT_ETS,TokCT_ETS,TokCD_ETS}).

% Add a token rule into the right ETS table
load_tok_add(Parent, Child, Rule, {TokCDT_ETS, TokCT_ETS, TokCD_ETS}) ->
    HaveParD   = is_tuple(Parent),
    HaveChildT = (not is_tuple(Child)) orelse (3==tuple_size(Child)),
    {TokETS, Key} = case {HaveParD, HaveChildT} of
        {true,  true } -> {TokCDT_ETS,
            {element(1,Parent),element(2,Parent),load_tok_add_getTokT(Child)}};
        {true,  false} -> {TokCD_ETS, Parent};
        {false, true } -> {TokCT_ETS, {Parent,load_tok_add_getTokT(Child)}}
    end,
    Value = case Child of
        {CondType, CondParam, _TokType} -> {CondType, CondParam, Rule};
        {position, any      }           -> Rule;
        {CondType, CondParam}           -> {CondType, CondParam, Rule};
        T when (not is_tuple(T))        -> Rule
    end,
    ets:insert(TokETS, {Key, Value}).

% Get the token type
load_tok_add_getTokT({_CondType,_CondParam, TokType}) -> TokType;
load_tok_add_getTokT(TokType) when not is_tuple(TokType) -> TokType.



%%% ============================================================================
%%% Key fields of specialized data records of nodes in the syntax tree

%% @spec parentDataKeyFields(Data::record()) -> RecordKeyFields
%%       RecordKeyFields = atom | tuple(RecordKeyFields)
%% @doc Give back the key fields of the data record of a syntactical parent
%%      node from the ESG. Key fields are that fields whiches are identify the
%%      node for the Pretty Printer rule query functions when the node is a
%%      parent node.
%% @see dataKeyFields/1
parentDataKeyFields(Anything) -> dataKeyFields(Anything).

%% @spec childDataKeyFields(Data::record()) -> RecordKeyFields
%%       RecordKeyFields = atom | tuple(RecordKeyFields)
%% @doc Give back the key fields of the data record of a syntactical child
%%      node from the ESG. Key fields are that fields whiches are identify the
%%      node for the Pretty Printer rule query functions when the node is a
%%      child node.
%% @see dataKeyFields/1
childDataKeyFields(#lex{type=token, data=Token}) -> dataKeyFields(Token);
childDataKeyFields(Other)                        -> dataKeyFields(Other).

%% @spec dataKeyFields(Data::record()) -> RecordKeyFields
%%       RecordKeyFields = atom | tuple(RecordKeyFields)
%% @doc Give back the key fields of the data record of a syntactical
%%      node from the ESG. Key fields are that fields whiches are identify the
%%      node for the Pretty Printer rule query functions.
dataKeyFields(#file{type=T})             -> T;
dataKeyFields(#form{type=attrib, tag=A}) -> {attrib,A};
dataKeyFields(#form{type=T})             -> T;
dataKeyFields(#clause{type=T})           -> T;
dataKeyFields(#expr{type=T})             -> T;
dataKeyFields(#lex{type=T})              -> T;
dataKeyFields(#token{type=T})            -> T;
dataKeyFields(#typexp{type=T})           -> T.


%%% ============================================================================
%%% Find a corresponded children of a node by a special descriptions

%% @spec get_child_idx(Selector::LinkSelector, Parent::node(),
%%               Childs::{[ChildTNI], ChildLength::natural()}, 
%%               StartIdx::natural()) ->
%%           ChildIdx::natural()
%%       LinkSelector = first | last | {LinkTag, LinkIndex} | {LinkTag, last}
%%       LinkTag = atom()
%%       LinkIdx = natural()
%%       ChildTNI = {LinkTag, Child::node(), LinkIdx}
%% @doc Find the specified child of `Parent' node and return the index of that.
%%      The search starts from the `StartIdx'-th child. If `StartIdx' must be
%%      between one and `ChildLength'.
get_child_idx(first,     _Parent,{_ChildTNIs,_ChildLength}, StartIdx) ->
    StartIdx;
get_child_idx(last,      _Parent,{_ChildTNIs, ChildLength},_StartIdx) ->
    ChildLength;
get_child_idx({Tag,last}, Parent, {ChildTNIs, ChildLength}, StartIdx) ->
    TagCnt = length(?Graph:path(Parent, [Tag])),
    get_child_idx({Tag,TagCnt}, Parent, {ChildTNIs, ChildLength}, StartIdx);
get_child_idx({Tag,Idx}, _Parent, {ChildTNIs,_ChildLength}, StartIdx) ->
    {ChildIdx, _Child} =
        ?MISC:list_find(fun({T,_N,I}) -> T==Tag andalso I==Idx end,
                        lists:nthtail(StartIdx-1,ChildTNIs)),
    StartIdx-1+ChildIdx.


%% @spec find_child_path(Selector::LinkSelector, DownPath, Parent::node(),
%%               Childs::{ChildTNIs::[ChildTNI], ChildLength::natural()}, 
%%               StartIdx::natural()) ->
%%           ChildIdx::natural()
%%       LinkSelector = first | last | path
%%       DownPath = [{LinkTag, LinkIdx}]
%%       LinkTag = atom()
%%       LinkIdx = natural()
%%       ChildTNI = {LinkTag, Child::node(), LinkIdx}
%% @doc Find the specified child of `Parent' node and return the index of that.
%%      The search starts from the `StartIdx'-th child. If `StartIdx' must be
%%      between one and `ChildLength'. This function is wrapper function to
%%      the {@link get_child_idx/4} function. If the `LinkSelector' is `path'
%%      than the real selector is the head of the `DownPath' list.
%% @see get_child_idx/4
find_child_path(first,_DownPath,_Parent,{_ChildTNIs,_ChildLength}, StartIdx) ->
    StartIdx;
find_child_path(last, _DownPath,_Parent,{_ChildTNIs, ChildLength},_StartIdx) ->
    ChildLength;
find_child_path(path, [{T,I}|_DownPath], Parent, {ChildTNIs,ChildLength},
        StartIdx) ->
    get_child_idx({T,I}, Parent, {ChildTNIs,ChildLength}, StartIdx).


%% @spec mod_idx(Idx::natural(), Mode::IdxModifier, MaxIdx::natural()) -> 
%%           natural()
%%       IdxModifier = from | to | before | after
%% @doc Modify the index returned by {@link get_child_idx/4}. The interval of
%%      the usable indices is the [`1',`MaxIdx'].
mod_idx(0,  _Mode,   _MaxIdx) -> 0;
mod_idx(Idx, from,   _MaxIdx) -> Idx;
mod_idx(Idx, to,     _MaxIdx) -> Idx;
mod_idx(Idx, 'after', MaxIdx) -> lists:min([Idx+1,MaxIdx]);
mod_idx(Idx, before, _MaxIdx) -> lists:max([Idx-1,1]).


%% @spec get_child_idx_mod(Selector::LinkSelector, Mode::IdxModifier, 
%%               Parent::node(),
%%               Childs::{ChildTNIs::[ChildTNI],ChildLength::natural()}, 
%%               StartIdx::natural()) ->
%%           ChildIdx::natural()
%%       LinkSelector = first | last | {LinkTag, LinkIndex} | {LinkTag, last}
%%       LinkTag = atom()
%%       LinkIdx = natural()
%%       IdxModifier = from | to | before | after
%%       ChildTNI = {LinkTag, Child::node(), LinkIdx}
%% @doc Apply the {@link get_child_idx/4} and the {@link mod_idx/3}.
%% @see get_child_idx/4
%% @see mod_idx/3
get_child_idx_mod(ChildDesc, Mode, Parent, {ChildTNIs,ChildLength}, StartIdx) ->
    Idx0 = get_child_idx(ChildDesc, Parent, {ChildTNIs,ChildLength},StartIdx),
    mod_idx(Idx0, Mode, ChildLength).



%%% ============================================================================
%%% Get differences of indentations of a parent node and the children of that

%% @spec get_indent(Parent::{ParentInfo::NodeInfo,ParLevel::natural()},
%%               Childs::{ChildTNIs::[ChildTNI], ChildLength::natural()},
%%               Tables::{InCD_ETS::tid(),InC_ETS::tid()}) -> [ChildTNII]
%%       NodeInfo = {Node::node(), NodeClass::atom(), NodeData::record()}
%%       ChildTNI = {LinkTag::atom(), Child::node(), LinkIdx::natural()}
%%       ChildTNII = {LinkTag::atom(), Child::node(), LinkIdx::natural(),
%%                    ChildIndent::indent()}
%% @doc Find the indentations rule for the `Parent' and the childs from the ETS
%%      tables and update the list of childs with the indentation informations.
get_indent({{Parent,ParentClass,ParentData},ParLevel}, {ChildTNIs, ChildLength},
        {InCD_ETS,InC_ETS}) ->
    ParDataKeys = parentDataKeyFields(ParentData),
    IR = case ets:lookup(InCD_ETS, {ParentClass, ParDataKeys}) of
        []  -> ets:lookup(InC_ETS, ParentClass);
        IR1 -> IR1
    end,
    IndentRule = case IR of
        [{_Id, Rule}] -> Rule;
        _             -> [{from, first, to, last, 0}]
    end,
    set_indent(IndentRule, {Parent,ParLevel}, {ChildTNIs,ChildLength,0}, []).


% Set the indentations of the childs by the partition rules
set_indent([], {_Parent,ParLevel}, {ChildTNIs,ChildLen,DoneCnt}, RevTNIIs) ->
    {_, RevTNIIs2} = set_indent_copy(ChildLen-DoneCnt,
                        lists:nthtail(DoneCnt,ChildTNIs), ParLevel, RevTNIIs),
    lists:reverse(RevTNIIs2);
set_indent([{ModeA,ChildA,ModeB,ChildB,Diff}|Parts], {Parent,ParLevel},
        {ChildTNIs,ChildLen,DoneCnt}, RevTNIIs) ->
    case get_child_idx_mod(ChildA, ModeA, Parent, {ChildTNIs,ChildLen}, 1) of
        IdxA when 0<IdxA ->
            case get_child_idx_mod(ChildB,ModeB,Parent,{ChildTNIs,ChildLen},
                                   IdxA) of
                IdxB when DoneCnt<IdxB ->
                    {ChildTNIs2, RevTNIIs2} =
                        set_indent_copy(lists:max([0,IdxA-1-DoneCnt]),
                                        lists:nthtail(DoneCnt,ChildTNIs),
                                        ParLevel, RevTNIIs),
                    {ChildTNIs3, RevTNIIs3} =
                        set_indent_mod(IdxB-lists:max([DoneCnt+1,IdxA])+1,
                                       ChildTNIs2, {ParLevel,Diff}, RevTNIIs2),
                    set_indent(Parts, {Parent,ParLevel},
                               {ChildTNIs3,ChildLen-IdxB+1,1}, RevTNIIs3);
                _ ->
                    set_indent(Parts, {Parent,ParLevel},
                               {ChildTNIs,ChildLen,DoneCnt}, RevTNIIs)
            end;
        _ ->
            set_indent(Parts, {Parent,ParLevel}, {ChildTNIs,ChildLen,DoneCnt},
                       RevTNIIs)
    end.

% Copy childs without modify the indentations
set_indent_copy(0, ChildTNIs,_ParLevel, RevTNIIs) -> {ChildTNIs, RevTNIIs};
set_indent_copy(_CopyCnt, [],_ParLevel, RevTNIIs) -> {[],        RevTNIIs};
set_indent_copy(CopyCnt, [{T,N,I}|ChildTNIs], ParLevel, RevTNIIs) ->
    set_indent_copy(CopyCnt-1, ChildTNIs, ParLevel,
        [{T,N,I,#indent{level=ParLevel, diff=0}}|RevTNIIs]).

% Copy childs with modify the indentations
set_indent_mod(0, ChildTNIs,{_ParLevel,_Diff},RevTNIIs) -> {ChildTNIs,RevTNIIs};
set_indent_mod(1, [{T,N,I}|ChildTNIs], {ParLevel,Diff}, RevTNIIs) ->
    {[{T,N,I}|ChildTNIs],
     [{T,N,I,#indent{level=ParLevel+Diff, diff=Diff}}|RevTNIIs]};
set_indent_mod(ModCnt, [{T,N,I}|ChildTNIs], {ParLevel,Diff}, RevTNIIs) ->
    set_indent_mod(ModCnt-1, ChildTNIs, {ParLevel,Diff},
        [{T,N,I,#indent{level=ParLevel+Diff, diff=Diff}}|RevTNIIs]).



%%% ============================================================================
%%% Get the white space rules

%% @spec get_token_rules(ParentInfo::NodeInfo, TokenInfo::NodeInfo,
%%               Tables::{TokCDT_ETS::tid(), TokCT_ETS::tid(), 
%%                        TokCD_ETS::tid()}) ->
%%           [TokenFormattingRule]
%%       NodeInfo = {Node::node(), NodeClass::atom(), NodeData::record()}
%%       TokenFormattingRule = {TokenFormattingType::atom,
%%           BeforeTokenValue::term(), AfterTokenValue::term()}
%% @doc Find the token formatting rules for the parent and the child from the
%%      ETS tables.
get_token_rules({Parent,ParentClass,ParentData}, {Token,_,TokenData},
        {TokCDT_ETS, TokCT_ETS, TokCD_ETS}) ->
    ParDataKeys = parentDataKeyFields(ParentData),
    TokDataKeys = childDataKeyFields(TokenData),
    % Find rules
    FindOrder = [{TokCDT_ETS, {ParentClass, ParDataKeys, TokDataKeys}},
                 {TokCT_ETS,  {ParentClass, TokDataKeys}},
                 {TokCD_ETS,  {ParentClass, ParDataKeys}}],
    get_token_rules_(FindOrder, Parent, Token).


% Find the rules in ETS tables in priority order
get_token_rules_([], _Parent, _Token) -> [];
get_token_rules_([{ETS, Key}|FindOrder], Parent, Token) ->
    case ets:lookup(ETS, Key) of
        [] -> get_token_rules_(FindOrder, Parent, Token);
        Objects ->
            Ret = ?MISC:partfold(
                fun(_, false) -> true; (_, _) -> false end,
                fun({_Key,TokRule}, _Acc) ->
                    get_tok_rule(is_apllicable_tok_rule(TokRule,Parent,Token),
                                 TokRule)
                end,
                false,
                Objects),
            case Ret of
                false -> get_token_rules_(FindOrder, Parent, Token);
                Rule  -> Rule
            end
    end.


% Check if the founded rule is applicable in the concrete situation
is_apllicable_tok_rule(TokRule, _Parent, _Token) when is_list(TokRule)->
    true;
is_apllicable_tok_rule({before_link, NextLinkTag, _TokRule}, Parent, Token) ->
    case lists:dropwhile(fun({_,N}) -> N/=Token end, ?Syn:children(Parent)) of
        [{_,Token},{NextLinkTag,_}|_] -> true;
        _ -> false
    end;
is_apllicable_tok_rule({check_fun,  CheckFun,    _TokRule}, _Parent, Token) ->
    CheckFun(Token);
is_apllicable_tok_rule({position, first, _TokRule}, Parent, Token) ->
    Token =:= element(2,hd(?Syn:children(Parent)));
is_apllicable_tok_rule({position, last,  _TokRule}, Parent, Token) ->
    Token =:= element(2,lists:last(?Syn:children(Parent))).


% % Get the concrete token rules
% get_pure_tok_rule({_ConType, _ConParam, TokRule}) -> TokRule;
% get_pure_tok_rule(TokRule)                        -> TokRule.

% Get the concrete token rules
get_tok_rule(true,  {_ConType, _ConParam, {TRule,_FRule}}) -> TRule;
get_tok_rule(true,  {_ConType, _ConParam, Rule          }) -> Rule;
get_tok_rule(true,  Rule                                 ) -> Rule;
get_tok_rule(false, {_ConType, _ConParam, {_TRule,FRule}}) -> FRule;
get_tok_rule(false, {_ConType, _ConParam, _Rule         }) -> false;
get_tok_rule(false, _Rule                                ) -> false.



%%% ----------------------------------------------------------------------------
%%% Handler functions for rules of tokens


%% @spec merge_token_rules(Rules1::[tokenRule()], Rules2::[tokenRule()]) ->
%%           Rules::[tokenRule()]
%% @doc  Merge the rules. If there are same rules in both list the smaller rule
%%       in absolute value will be dropped.
merge_token_rules(Rules1, Rules2) when is_list(Rules1), is_list(Rules2) ->
    ClassDict = class_token_rulesR(lists:merge(lists:usort(Rules1),
                                               lists:usort(Rules2))),
    lists:reverse(dict:fold(
        fun(_Type, Rules, List) ->
            case Rules of
                [] -> List;
                _  -> [maxabs_token_rule(Rules)|List]
            end
        end,
        [],
        ClassDict)).


%% @spec class_token_rulesR(Rules::[tokenRule()]) ->
%%           ClassDict::dict(RuleType::atom(), Rules::[tokenRule()])
%% @doc  Split `Rules' into separated lists by the first element of the 3-tuple
%%       rules. The keys of `ClassDict' are the first elements and the values
%%       are the separated lists. The list are contain rules in reverse order
%%       as the `Rules' list.
class_token_rulesR(Rules) when is_list(Rules) ->
    lists:foldl(fun(Rule={Type,_,_}, Dict) ->
            case dict:find(Type,Dict) of
                error    -> dict:store(Type, [Rule], Dict);
                {ok, Rs} -> dict:store(Type, [Rule|Rs], Dict)
            end
        end,
        dict:new(),
        Rules).

%% @spec class_token_rules(Rules::[tokenRule()]) ->
%%           ClassDict::dict(RuleType::atom(), Rules::[tokenRule()])
%% @doc  Split `Rules' into separated lists by the first element of the 3-tuple
%%       rules. The keys of `ClassDict' are the first elements and the values
%%       are the separated lists. The list are contain rules in same order
%%       as the `Rules' list.
class_token_rules(Rules) ->
    dict:map(fun(_Type,Rs) -> lists:reverse(Rs) end,
        class_token_rulesR(Rules)).


% The list must contain same typed rules. Return that rule values that are
% greatest in absolute value.
maxabs_token_rule([]) -> no;
maxabs_token_rule([{Type,Val1,Val2}|Tail]) ->
    MaxAbsFun = fun({_T,V1,V2},{MA1,MV1,MA2,MV2}) ->
        A1 = abs(V1), A2 = abs(V2),
        case {MA1<A1, MA2<A2} of
            {true,  true } -> { A1,  V1,  A2,  V2};
            {true,  false} -> { A1,  V1, MA2, MV2};
            {false, true } -> {MA1, MV1,  A2,  V2};
            _              -> {MA1, MV1, MA2, MV2}
        end
    end,
    {_,MV1,_,MV2} = lists:foldl(MaxAbsFun,{abs(Val1),Val1,abs(Val2),Val2},Tail),
    {Type,MV1,MV2}.


%% @spec get_token_rule(Type::atom(), Rules::[tokenRule()]) -> tokenRule()
%% @doc  Return the `Type' typed rule from `Rules'.
get_token_rule(Type, Rules) when is_atom(Type), is_list(Rules) ->
    element(2,
        ?MISC:list_find(fun({T,_,_}) when T==Type ->
                    true;
            ({_,_,_}) ->
                false
            end,
            Rules)).


%% @spec get_token_rule_between(Type::atom(), Rules1::[tokenRule()],
%%               Rules2::[tokenRule()]) ->
%%           {Max::integer(), Sum::integer(), After1::integer(),
%%            Before2::integer()}
%% @doc  `Rules1' and `Rules2' are the rules of two following token and it
%%       return the maximum, summary and original values of `Type' typed rules
%%       for the formatting between tokens.
get_token_rule_between(Type, Rules1, Rules2) when is_atom(Type),
        is_list(Rules1), is_list(Rules2) ->
    case {get_token_rule(Type, Rules1), get_token_rule(Type, Rules2)} of
        {{_T,_B1,A1},{_T,B2,_A2}} -> {lists:max([A1,B2]),A1+B2,A1,B2};
        {{_T,_B1,A1},no         } -> {A1, A1, A1, 0 };
        {no,         {_T,B2,_A2}} -> {B2, B2, 0,  B2};
        {no,         no         } -> {0,  0,  0,  0 }
    end.


