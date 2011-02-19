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

%%% @doc Erlang preprocessor graph transformations. This module contains the
%%% functions that build the graph representation of the lexical layer of
%%% Erlang files, perform preprocessing of Erlang source code, and delete
%%% the representation from the graph.
%%%
%%% The lexical layer representation will be described here in the future.
%%%
%%% The following preprocessor transformations are performed by this module:
%%%
%%% <ul>
%%%
%%% <li>`ifdef', `ifndef', `else', and `endif' directives are handled
%%%   according to symbolic names stored in `#env{name=def}' graph
%%%   nodes. Parts excluded from compilation are stored in the graph, but
%%%   not processed any further (dropped by the preprocessor).</li>
%%%
%%% <li>`include' and `include_lib' directives are handled by reading and
%%%   storing the specified files (@see ?FILEMAN:add_file/1), and
%%%   substituting their contents during preprocessing. Note that macro and
%%%   record definitions are not substituted, only unknown attributes and
%%%   function definitions.</li>
%%%
%%% <li>`define' directives are parsed and macros are stored using
%%%   `#macro{}' graph nodes.</li>
%%%
%%% <li>`?MACRO' and `?MACRO(Par, ..., Par)' references are substituted when
%%%   the macro body is available, other macros trigger a "Non-local macro"
%%%   warning message.</li>
%%%
%%% </ul>
%%%
%%% @author Robert Kitlei
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

%%% Plans:
%%%   Remove `macro' nodes, use `#lex{type=macro}' with `#lex{type=marg}' and
%%%     `#lex{type=mbody}' -- this automates lexical cleaning
%%%   Include line number with tokens, possibly {file, line} -- better error
%%%      reporting

-module(referl_preproc).
-vsn("$Rev: 1903 $").

-export([preprocess/2, preprocess/3, clear/1]).

-include("refactorerl.hrl").

%%% @type formTokens() = [{#token{}, node()}]. A list that contains token
%%% data for a form.

%% @spec preprocess(formTokens(), node()) ->
%%                                   [{tokens, formTokens()} | {form, node()}]
%%
%% @doc Processes `Tokens' according to the Erlang preprocessor rules. The
%% input should be a list of tokens that make a single, complete form,
%% directly from the scanner; the output is a preprocessed list of forms.
%% Forms tagged `form' does not need any more processing, these are already
%% parsed by the preprocessor (e.g. macro definitions); other forms are
%% subject to further parsing.
preprocess(Tokens, File) ->
    {Result, _} = preprocess(Tokens, File, start),
    Result.

%% @spec preprocess(formTokens(), node(), state()) ->
%%                         {[{tokens, formTokens()} | {form, node()}], state()}
%%
%% @doc Re-entrant preprocessor that handles conditional compilation. The
%% same notes apply as for `preprocess/2'. First time this function must be
%% called with `start' as the state value, subsequent calls should pass the
%% next form token list and the state returned from the previous call. Note
%% that file inclusion will close the active ESG batch, so returned form
%% nodes must be inserted into the syntax tree by the caller immediately.
preprocess(Tokens, File, start) ->
    preprocess(Tokens, File, [copy]);

preprocess([{#token{type=minus},              T1},
            {#token{type=atom, value=If},     T2},
            {#token{type=op_paren},           T3},
            {#token{type=AtVar, value=Name},  T4},
            {#token{type=cl_paren},           T5},
            {#token{type=stop},               T6}], _File, St)
  when ((If==ifdef) or (If==ifndef)), ((AtVar==atom) or (AtVar==variable)) ->
    TNodes = [T1, T2, T3, T4, T5, T6],
    Def = ?GRAPH:path(?GRAPH:root(),
                      [{env, {{name, '==', def}, 'and',
                              {value, '==', macro_name(Name)}}}]),
    {{form, create_form(lex, If, TNodes)},
     if
         ((If == ifdef)  and (Def /= [])) or
         ((If == ifndef) and (Def == [])) ->
             [copy | St];

         true ->
             [skip | St]
     end};

preprocess([{#token{type=minus},              T1},
            {#token{type=atom, value=else},   T2},
            {#token{type=stop},               T3}], _File, [StTop|St]) ->
    {{form, create_form(lex, else, [T1, T2, T3])},
     case StTop of
         copy -> [skip|St];
         skip -> [copy|St]
     end};

preprocess([{#token{type=minus},              T1},
            {#token{type=atom, value=endif},  T2},
            {#token{type=stop},               T3}], _File, [_|St]) ->
    {{form, create_form(lex, endif, [T1, T2, T3])}, St};

preprocess(Form, File, St=[StTop|_]) ->
    case StTop of
        copy ->
            {form(Form, File), St};
        skip ->
            {{form, create_form(lex, skip, token_nodes(Form))}, St}
    end.

%% @spec form(formTokens(), node()) -> [{tokens, formTokens()} | {form, node()}]
%%
%% @doc Preprocesses a form: macro definitions are stored (without further
%% preprocessing), included files are linked to the top level file, other
%% forms are subject to macro substitution. The ?MODULE macro is created
%% here when encountering a `-module' attribute. Preprocessor directives are
%% turned into an analysed form, other forms are returned as token lists.
form([{#token{type=minus},                      T1},
      {#token{type=atom,      value=Include},   T2},
      {#token{type=op_paren},                   T3},
      {#token{type=string,    value=Filename},  T4},
      {#token{type=cl_paren},                   T5},
      {#token{type=stop},                       T6}], File)
  when Include =:= include; Include =:= include_lib ->
    TNodes = [T1, T2, T3, T4, T5, T6],
    try
        IncName = find_include(Include, Filename, File),
        include(File, IncName, TNodes)
    catch
        throw:Error ->
            [{form, create_form(error, Error, TNodes)}]
    end;

form([{#token{type=minus},                        T1},
      {#token{type=atom,           value=define}, T2},
      {#token{type=op_paren},                     T3} |
      Tail = [{#token{value=Name},                 _} | _]], File) ->
    TNodes = [T1, T2, T3],
    try define(Tail, File) of
        Rest ->
            [{form, create_form(macro, macro_name(Name), TNodes ++ Rest)}]
    catch
        throw:Error ->
            [{form, create_form(error, Error, TNodes ++ token_nodes(Tail))}]
    end;

form(Tokens, File) ->
    try macro_subst(Tokens, File) of
        TList=[{#token{type=minus},                   _},
               {#token{type=atom,      value=module}, _},
               {#token{type=op_paren},                _},
               {#token{type=atom},                 Name} | _] ->
            Mod = ?GRAPH:create(#macro{name="MODULE"}),
            ?GRAPH:mklink(File, macro, Mod),
            ?GRAPH:mklink(Mod, mbody, Name),
            [{tokens, TList}];
        TList ->
            [{tokens, TList}]
    catch
        throw:Error ->
            [{form, create_form(error, Error, token_nodes(Tokens))}]
    end.

%% @spec token_nodes(formTokens()) -> [node()]
%% @doc Returns the token nodes from a token data list.
token_nodes(Tokens) ->
    [Node || {_Data, Node} <- Tokens].

%% @spec create_form(atom(), atom(), node()) -> node()
%% @doc Creates a lexical-only form
create_form(Type, Tag, Tokens) ->
    Form = ?ESG:create(#form{type=Type, tag=Tag}),
    [?ESG:insert(Form, flex, Token) || Token <- Tokens ],
    Form.

%% @spec define(formTokens(), node()) -> [node()]
%% @doc Analyses macro definition, returns the lexical nodes
define([{#token{type=NameType, value=Name}, T} | Tokens], File)
  when NameType == atom; NameType == variable ->
    Def = ?GRAPH:create(#macro{name = macro_name(Name)}),
    ?GRAPH:mklink(File, macro, Def),
    [T | define(arglist, Tokens, Def)].

define(arglist, [{#token{type=op_paren}, T} | Tokens], Def) ->
    [T | define(nextarg, Tokens, Def)];
define(arglist, [{#token{type=comma},    T} | Tokens], Def) ->
    [T | define(body, Tokens, Def)];

define(nextarg, [{#token{type=variable}, T} | Tokens], Def) ->
    ?GRAPH:mklink(Def, marg, T),
    [T | define(argsep, Tokens, Def)];

define(argsep, [{#token{type=comma},     T} | Tokens], Def) ->
    [T | define(nextarg, Tokens, Def)];
define(argsep, [{#token{type=cl_paren},  T1},
                {#token{type=comma},     T2} | Tokens], Def) ->
    [T1, T2 | define(body, Tokens, Def)];

define(body, [{#token{type=cl_paren},    T},
              {#token{type=stop},        Stop} ], _Def) ->
    [T, Stop];
%% Quote from epp.erl: "Be nice, allow no right paren!"
define(body, [{#token{type=stop},        Stop} ], _Def) ->
    [Stop];
define(body, [{_, T} | Tokens], Def) ->
    ?GRAPH:mklink(Def, mbody, T),
    [T | define(body, Tokens, Def)];

define(Stat, [], _Def) ->
    throw({define, Stat}).


%% @spec macro_subst(formTokens(), node()) -> formTokens()
%% @doc Performs macro substitution in a token list
macro_subst([Head = {#token{type=questionm}, Q} | Tail], File) ->
    case macro(Tail, File, Q) of
        skip                -> [Head | macro_subst(Tail, File)];
        {done, Subst, Rest} -> Subst ++ macro_subst(Rest, File);
        {App, Params, Rest} ->
            macro_subst(substitute(App, Params) ++ Rest, File)
    end;
macro_subst([Head | Tail], File) ->
    [Head | macro_subst(Tail, File)];
macro_subst([], _File) ->
    [].


%% @spec macro(formTokens(), node(), node()) ->
%%       skip |
%%       {done, formTokens(), formTokens()} |
%%       {node(), [formTokens()], formTokens()}
%% @doc Analyses macro substitution. Returns `skip' for not existing macros,
%% `{done, Result, Rest}' for macros substituted here, and
%% `{App, Params, Rest}' for macros to be substituted later.
macro([{#token{type=NameType, value=Name}, N} | Tokens], File, First)
  when NameType == atom; NameType == variable ->
    MacroPath = [incl, {macro, {name, '==', macro_name(Name)}}],
    case lists:usort(?GRAPH:path(File, MacroPath)) of
        [Def|Others] ->
            if
                Others =/= [] ->
                    error_logger:warning_msg("Multiply defined macro ~s~n",
                                             [macro_name(Name)]);
                true -> ok
            end,
            App = ?ESG:create(#lex{type=subst, data=macro_name(Name)}),
            ?ESG:insert(App, llex, First),
            ?ESG:insert(App, llex, N),
            ?GRAPH:mklink(App, mref, Def),
            %% Look for macro parameters only when the definition needs them
            case ?GRAPH:path(Def, [marg]) of
                [] -> {App, [], Tokens};
                _  -> macro(paramlist, Tokens, [], App)
            end;
        [] when Name == "FILE" ->
            App = ?ESG:create(#lex{type=subst, data=Name}),
            #file{path=Path} = ?GRAPH:data(File),
            {done,
             [{#token{type=string, value=Path}, virtual(none, App)}],
             Tokens};
        [] when Name == "LINE" ->
            App = ?ESG:create(#lex{type=subst, data=Name}),
            {done,
             [{#token{type=integer, value=42}, virtual(none, App)}],
             Tokens};
        [] ->
            error_logger:warning_msg("Non-local macro ~s in ~s~n",
                                     [macro_name(Name),
                                      (?GRAPH:data(File))#file.path]),
            skip
    end.

macro(paramlist, [{#token{type=op_paren}, N} | Tokens], Params, App) ->
    ?ESG:insert(App, llex, N),
    macro(param, Tokens, Params, App);

macro(param, Tokens, Params, App) ->
    Par = ?ESG:create(#lex{type=param}),
    ?ESG:insert(App, llex, Par),
    macro_param(Tokens, Par, [], [], Params, App).

%% @spec macro_param(Tokens::formTokens(), node(), formTokens(), ParStack,
%%       [formTokens()], node()) -> {node(), [formTokens()], formTokens()}
%%   ParStack = [op_paren|op_brace|op_bracket]
%%
%% @doc Continues macro substitution analysis when parameters are present.
%% <ul>
%%  <li>`Tokens' is the input token list</li>
%%  <li>`ParOb' is the node of the last parameter (which is being analysed)</li>
%%  <li>`Ps' is the reversed list of tokens in the last parameter</li>
%%  <li>`Paren' is the stack of parentheses</li>
%%  <li>`Params' is the reversed parameter list (of token lists)</li>
%%  <li>`App' is the macro application node</li>
%% </ul>
macro_param([P={#token{type=OpParen}, N} | Tokens],
            ParOb, Ps, Paren, Params, App)
  when OpParen == op_paren; OpParen == op_bracket; OpParen == op_brace ->
    ?ESG:insert(ParOb, llex, N),
    macro_param(Tokens, ParOb, [P|Ps], [OpParen | Paren], Params, App);

macro_param([P={#token{type=ClParen}, N} | Tokens],
            ParOb, Ps, [OpParen|Paren], Params, App)
  when OpParen == op_paren,   ClParen == cl_paren;
       OpParen == op_brace,   ClParen == cl_brace;
       OpParen == op_bracket, ClParen == cl_bracket ->
    ?ESG:insert(ParOb, llex, N),
    macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App);

macro_param([{#token{type=cl_paren}, N} | Tokens],
            _ParOb, Ps, [], Params, App) ->
    ?ESG:insert(App, llex, N),
    {App, lists:reverse([lists:reverse(Ps)|Params]), Tokens};

macro_param([{#token{type=comma}, T} | Tokens],
            ParOb, Ps, [], Params, App) ->
    ?ESG:insert(ParOb, llex, T),
    macro(param, Tokens, [lists:reverse(Ps)|Params], App);

macro_param([P={#token{}, T} | Tokens],
            ParOb, Ps, Paren, Params, App) ->
    ?ESG:insert(ParOb, llex, T),
    macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App).


%% @spec substitute(node(), [formTokens()]) -> formTokens()
%% @doc Generates the result of a macro substitution
substitute(App, Params) ->
    [Def] = ?GRAPH:path(App, [mref]),
    Args = [((?GRAPH:data(Arg))#lex.data)#token.value ||
               Arg <- ?GRAPH:path(Def, [marg])],
    substitute(?GRAPH:path(Def, [mbody]), Args, Params, App).

%% @spec substitute([node()], [string()], [formTokens()], node()) ->
%%          formTokens()
substitute([First | Rest], Args, Params, App) ->
    case ?GRAPH:data(First) of
        #lex{type=token, data=Token} ->
            subs_token(First, Token, Args, Params, App) ++
                substitute(Rest, Args, Params, App);
        #lex{type=Type} ->
            exit({substitution_error, Type})
    end;
substitute([], _, _, _) ->
    [].

%% @spec subs_token(node(), #token{}, [string()],[formTokens()], node()) ->
%%         formTokens()
%% @doc Substitutes a macro argument with the corresponding parameter tokens
subs_token(_, #token{type=variable, value=Name}, [Name|_], [Par|_], App) ->
    [{Data, virtual(Node, App)} || {Data, Node} <- Par];
subs_token(TN, T=#token{type=variable}, [_ | Args], [_ | Pars], App) ->
    subs_token(TN, T, Args, Pars, App);
subs_token(TN, T, _, _, App) ->
    [{T, virtual(TN, App)}].


macro_name(Atom) when is_atom(Atom) -> atom_to_list(Atom);
macro_name(Str)  when is_list(Str)  -> Str.


%% Looks for the first existing include file in the list of include
%% directories.
find_include(include, Name, FileNode) ->
    #file{path=FilePath} = ?GRAPH:data(FileNode),
    Base = filename:dirname(FilePath),
    Path = [{env, {name, '==', include}}],
    Dirs = [(?GRAPH:data(Dir))#env.value ||
               Dir <- ?GRAPH:path(?GRAPH:root(), Path)],
    case [Filename ||   Dir      <- [Base | Dirs],
                        Filename <- [filename:join(Dir, Name)],
                        filelib:is_file(Filename)] of
        [Filename|_] -> Filename;
        []       -> throw({no_include_file, Name})
    end;

find_include(include_lib, Name, _) ->
    {App, [$/ | Dir]} = lists:splitwith(
                          fun (Ch) -> Ch /= $/ end,
                          Name),
    Path = [{env, {name, '==', appbase}}],
    case [filename:join(AD, Dir) ||
             #env{value=Base} <- [?GRAPH:data(E) ||
                                     E <- ?GRAPH:path(?GRAPH:root(), Path)],
             AD <- filelib:wildcard(filename:join(Base, App) ++ "*")] of
        [Filename|_] -> Filename;
        []           -> throw({no_include_file, Name})
    end.


%% @spec include(node(), string(), [node()]) ->
%%         [{tokens,formTokens()} | {form, node()}]
include(File, IncName, Tokens) ->
    FileType = (?GRAPH:data(File))#file.type,
    case ?FILEMAN:add_file(IncName) of
        {error, Reason} ->
            throw({include_error, IncName, Reason});
        {file, IncFile} ->
            [?GRAPH:mklink(File, incl, Inc) ||
                Inc <- ?GRAPH:path(IncFile, [incl])],
            if
                FileType =:= header ->
                    Form = create_form(lex, include, Tokens),
                    ?GRAPH:mklink(Form, iref, IncFile),
                    [{form, Form}];
                FileType =:= module ->
                    IncNode = ?ESG:create(#lex{type=incl}),
                    [?ESG:insert(IncNode, llex, T) || T <- Tokens],
                    IncForm = create_form(lex, include, [IncNode]),
                    ?GRAPH:mklink(IncForm, iref, IncFile),
                    [{form, IncForm} |
                     [{tokens, macro_subst(Form, File)} ||
                         Form <- include_forms(IncFile, IncNode)]]
            end
    end.

%% @spec include_forms(node(), node()) -> [formTokens()]
include_forms(IncFile, IncNode) ->
    lists:flatmap(
      fun
          (Form) ->
              include_form(?GRAPH:data(Form), Form, IncNode)
      end,
      ?GRAPH:path(IncFile, [form])).

%% @spec include_form(#form{}, node(), node()) -> [formTokens()]
include_form(#form{tag=store}, Form, IncNode) ->
    [[include_token(T, IncNode) || T <- ?GRAPH:path(Form, [flex])]];
include_form(#form{tag=include}, Form, IncNode) ->
    [File] = ?GRAPH:path(Form, [iref]),
    include_forms(File, IncNode);
include_form(#form{}, _Form, _IncNode) ->
    [].

include_token(Token, IncNode) ->
    #lex{type=token, data=TD} = ?GRAPH:data(Token),
    {TD, virtual(Token, IncNode)}.


%% Creates a virtual token
virtual(Token, Source) ->
    VT = ?ESG:create(#lex{type=token, data=virtual}),
    if
        Token =/= none ->
            ?GRAPH:mklink(VT, orig, Token)
    end,
    ?ESG:insert(VT, llex, Source),
    VT.


%% @spec clear(node()) -> ok
%%
%% @doc Removes all lexical information from `File'. This includes tokens,
%% macros and macro substitutions.
clear(File) ->
  [clear_macro(Macro) || Macro <- ?GRAPH:path(File, [macro])].


clear_macro(Macro) ->
    case ?GRAPH:path(Macro, [{mref, back}]) of
        [] -> ok;
        _ ->
            error_logger:warning_msg("Deleting used macro: ~s~n",
                                     [(?GRAPH:data(Macro))#macro.name])
    end,
    ?GRAPH:delete(Macro).
