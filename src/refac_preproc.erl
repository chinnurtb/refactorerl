%%% The contents of this file are subject to the Mozilla Public License
%%% Version 1.1 (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.mozilla.org/MPL/
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
%%%
%%% Contributor(s): ______________________________________.
%%%
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
%%%   storing the specified files (@see refac_fileman:add_file/1), and
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

-module(refac_preproc).
-vsn("$Rev: 1206 $").

-export([preprocess/2, clear/1]).

-include("refactorerl.hrl").


%% @spec preprocess([FormTokens], node()) ->
%%                                   [{tokens, FormTokens} | {form, node()}]
%%       FormTokens = [{#token{}, node()}]
%%
%% @doc Processes `Tokens' according to the Erlang preprocessor rules. The
%% input should be a list of tokens split into forms directly from the
%% scanner; the output is a preprocessed list of forms. Forms tagged `form'
%% does not need any more processing, these are already parsed by the
%% preprocessor (e.g. macro definitions); other forms are subject to further
%% parsing.
preprocess(Forms, File) ->
    ?GRAPH:mklink(File, incl, File),
    forms(Forms, File, [copy]).

forms([], _File, _) ->
    [];
forms([[{#token{type=minus},              F},
        {#token{type=atom, value=If},     _},
        {#token{type=op_paren},           _},
        {#token{type=AtVar, value=Name},  _},
        {#token{type=cl_paren},           _},
        {#token{type=stop},               L}] | Forms], File, St)
  when ((If==ifdef) or (If==ifndef)), ((AtVar==atom) or (AtVar==variable)) ->
    Def = ?GRAPH:path(?GRAPH:root(),
                      [{env, {{name, '==', def}, 'and',
                              {value, '==', macro_name(Name)}}}]),
    [{form, create_form(ppcond, ifdef, F, L)} |
      if
          ((If == ifdef)  and (Def /= [])) or
          ((If == ifndef) and (Def == [])) ->
              forms(Forms, File, [copy | St]);
          
          true ->
              forms(Forms, File, [skip | St])
      end];

forms([[{#token{type=minus},              F},
        {#token{type=atom, value=else},   _},
        {#token{type=stop},               L}] | Forms], File, [StTop|St]) ->
    [{form, create_form(ppcond, else, F, L)} |
     case StTop of
         copy -> forms(Forms, File, [skip|St]);
         skip -> forms(Forms, File, [copy|St])
     end];

forms([[{#token{type=minus},              F},
        {#token{type=atom, value=endif},  _},
        {#token{type=stop},               L}] | Forms], File, [_|St]) ->
    [{form, create_form(ppcond, endif, F, L)} | forms(Forms, File, St)];

forms([Form=[{_,F}|_] | Forms], File, St=[StTop|_]) ->
    case StTop of
        copy ->
            form(Form, File) ++ forms(Forms, File, St);
        skip ->
            [{form, create_form(ppcond, skip, F, last_node(Form))} |
             forms(Forms, File, St)]
    end.

%% @spec form(FormTokens, node()) -> {tokens, FormTokens} | {form, node()}
%%       FormTokens = [{#token{}, node()}]
%%
%% @doc Preprocesses a form: macro definitions are stored (without further
%% preprocessing), included files are linked to the top level file, other
%% forms are subject to macro substitution
form([{#token{type=minus},                      F},
      {#token{type=atom,      value=Include},   _},
      {#token{type=op_paren},                   _},
      {#token{type=string,    value=Filename},  _},
      {#token{type=cl_paren},                   _},
      {#token{type=stop},                       L}], File)
  when Include =:= include; Include =:= include_lib ->
    try
        IncFile = find_include(Include, Filename, File),
        include(File, IncFile, F, L)
    catch
        throw:Error ->
            [{form, create_form(error, Error, F, L)}]
    end;

form([{#token{type=minus},                      F},
      {#token{type=atom,      value=define},    _},
      {#token{type=op_paren},                   _} |
      Tail = [{#token{value=Name},              _} | _]], File) ->
    try define(Tail, File) of
        L ->
            [{form, create_form(define, macro_name(Name), F, L)}]
    catch
        throw:Error ->
            [{form, create_form(error, Error, F, last_node(Tail))}]
    end;

form([{#token{type=minus},                F},
      {#token{type=atom, value=record},   _},
      {#token{type=op_paren},             _},
      {#token{type=atom, value=Name},     _} | Tail], _File) ->
    [{form, create_form(record, Name, F, last_node(Tail))}];
            
form(Tokens = [{_,F}|_], File) ->
    try macro_subst(Tokens, File) of
        TL=[{#token{type=minus},                _},
            {#token{type=atom, value=module},   _},
            {#token{type=op_paren},             _},
            {#token{type=atom},                 Name} | _] ->
            Mod = ?GRAPH:create(#macro{name="MODULE"}),
            ?GRAPH:mklink(File, macro, Mod),
            ?GRAPH:mklink(Mod, mbody, Name),
            [{tokens, TL}];
        TL ->
            [{tokens, TL}]
    catch
        throw:Error ->
            [{form, create_form(error, Error, F, last_node(Tokens))}]
    end.

create_form(Type, Tag, First, Last) ->
    Form = ?GRAPH:create(#form{type=Type, tag=Tag}),
    ?GRAPH:mklink(Form, ffirst, First),
    ?GRAPH:mklink(Form, flast, Last),
    Form.

last_node(Lst) ->
    {_, N} = lists:last(Lst),
    N.

%% Performs macro substitution in a token list
macro_subst([Head = {#token{type=questionm}, Q} | Tail], File) ->
    case macro(Tail, File, Q) of
        skip         -> [Head | macro_subst(Tail, File)];
        {done, Subst, Rest} -> Subst ++ macro_subst(Rest, File);
        {App, Params, Rest}  ->
            macro_subst(substitute(App, Params) ++ Rest, File)
    end;
macro_subst([Head | Tail], File) ->
    [Head | macro_subst(Tail, File)];
macro_subst([], _File) ->
    [].


%% Analyses macro definition
define([{#token{type=NameType, value=Name}, _} | Tokens], File)
  when NameType == atom; NameType == variable ->
    Def = ?GRAPH:create(#macro{name = macro_name(Name)}),
    ?GRAPH:mklink(File, macro, Def),
    define(arglist, Tokens, Def).

define(arglist, [{#token{type=op_paren}, _} | Tokens], Def) ->
    define(nextarg, Tokens, Def);
define(arglist, [{#token{type=comma},    _} | Tokens], Def) ->
    define(body, Tokens, Def);

define(nextarg, [{#token{type=variable}, V} | Tokens], Def) ->
    ?GRAPH:mklink(Def, marg, V),
    define(argsep, Tokens, Def);

define(argsep, [{#token{type=comma},     _} | Tokens], Def) ->
    define(nextarg, Tokens, Def);
define(argsep, [{#token{type=cl_paren},  _},
                {#token{type=comma},     _} | Tokens], Def) ->
    define(body, Tokens, Def);

define(body, [{#token{type=cl_paren},    _},
              {#token{type=stop},        Stop} ], _Def) ->
    Stop;
%% Quote from epp.erl: "Be nice, allow no right paren!"
define(body, [{#token{type=stop},        Stop} ], _Def) ->
    Stop;
define(body, [{_, T} | Tokens], Def) ->
    ?GRAPH:mklink(Def, mbody, T),
    define(body, Tokens, Def);

define(Stat, [], _Def) ->
    throw({define, Stat}).

    
%% Analyses macro substitution                             
macro([{#token{type=NameType, value=Name}, N} | Tokens], File, First)
  when NameType == atom; NameType == variable ->
    case ?GRAPH:path(File, [incl, {macro, {name, '==', macro_name(Name)}}]) of
        [Def] ->
            App = ?GRAPH:create(#lex{type=subst, data=macro_name(Name)}),
            ?GRAPH:mklink(App, lfirst, First),
            ?GRAPH:mklink(App, mref, Def),
            case ?GRAPH:path(Def, [marg]) of
                [] ->
                    ?GRAPH:mklink(App, llast, N),
                    {App, [], Tokens};
                _ ->
                    macro(paramlist, Tokens, [], App)
            end;
        [] when Name == "FILE" ->
            App = ?GRAPH:create(#lex{type=subst, data=Name}),
            ?GRAPH:mklink(App, lfirst, First),
            ?GRAPH:mklink(App, llast, N),
            #file{path=Path} = ?GRAPH:data(File),
            {done, [{#token{type=string, value=Path}, App}], Tokens};
        [] when Name == "LINE" ->
            App = ?GRAPH:create(#lex{type=subst, data=Name}),
            ?GRAPH:mklink(App, lfirst, First),
            ?GRAPH:mklink(App, llast, N),
            {done, [{#token{type=integer, value=42}, App}], Tokens};
        [] ->
            error_logger:warning_msg("Non-local macro ~s in ~s~n",
                                     [macro_name(Name),
                                      (?GRAPH:data(File))#file.path]),
            skip;
        _ ->
            throw({multiple_macro, Name})
    end.

macro(paramlist, [{#token{type=op_paren}, _} | Tokens], Params, App) ->
    macro(param, Tokens, Params, App);

macro(param, Tokens, Params, App) ->
    Par = ?GRAPH:create(#lex{type=param}),
    ?GRAPH:mklink(App, llex, Par),
    macro_param(Tokens, Par, [], [], Params, App).

macro_param([P={#token{type=OpParen}, N} | Tokens],
            ParOb, Ps, Paren, Params, App)
  when OpParen == op_paren; OpParen == op_bracket; OpParen == op_brace ->
    ?GRAPH:mklink(ParOb, llex, N),
    macro_param(Tokens, ParOb, [P|Ps], [OpParen | Paren], Params, App);

macro_param([P={#token{type=ClParen}, N} | Tokens],
            ParOb, Ps, [OpParen|Paren], Params, App)
  when OpParen == op_paren,   ClParen == cl_paren;
       OpParen == op_brace,   ClParen == cl_brace;
       OpParen == op_bracket, ClParen == cl_bracket ->
    ?GRAPH:mklink(ParOb, llex, N),
    macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App);

macro_param([{#token{type=cl_paren}, N} | Tokens],
            _ParOb, Ps, [], Params, App) ->
    ?GRAPH:mklink(App, llast, N),
    {App, lists:reverse([lists:reverse(Ps)|Params]), Tokens};

macro_param([{#token{type=comma}, _} | Tokens],
            _ParOb, Ps, [], Params, App) ->
    macro(param, Tokens, [lists:reverse(Ps)|Params], App);

macro_param([P={#token{}, T} | Tokens],
            ParOb, Ps, Paren, Params, App) ->
    ?GRAPH:mklink(ParOb, llex, T),
    macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App).


%% Generates the result of a macro substitution
substitute(App, Params) ->
    [Def] = ?GRAPH:path(App, [mref]),
    Args = [((?GRAPH:data(Arg))#lex.data)#token.value ||
               Arg <- ?GRAPH:path(Def, [marg])],
    substitute(?GRAPH:path(Def, [mbody]), Args, Params, App).

substitute([First | Rest], Args, Params, App) ->
    case ?GRAPH:data(First) of
        #lex{type=token, data=Token} ->
            subs_token(Token, Args, Params, App) ++
                substitute(Rest, Args, Params, App);
        #lex{type=Type} ->
            exit({substitution_error, Type})
    end;
substitute([], _, _, _) ->
    [].

subs_token(#token{type=variable, value=Name}, [Name | _], [Par | _], _App) ->
    Par;
subs_token(T=#token{type=variable}, [_ | Args], [_ | Pars], App) ->
    subs_token(T, Args, Pars, App);
subs_token(T, _, _, App) ->
    [{T, App}].
            

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
    case [File || Dir <- [Base | Dirs],
                  File <- [filename:join(Dir, Name)],
                  filelib:is_file(File)] of
        [File|_] -> File;
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
        [File|_] -> File;
        []     -> throw({no_include_file, Name})
    end.


%% @spec include(node(), string(), node(), node()) -> ok
include(File, IncName, First, Last) ->    
    case refac_fileman:add_file(IncName) of
        {error, Reason} ->
            throw({include_error, IncName, Reason});
        {file, IncFile} ->
            Form = create_form(include, IncName, First, Last),
            [ ?GRAPH:mklink(File, incl, Inc) ||
                Inc <- ?GRAPH:path(IncFile, [incl]) ],
            [{form, Form} | include_forms(IncFile, File)]
    end.

include_forms(File, Parent) ->
    lists:map(
      fun (Form) ->
              Incl = ?GRAPH:create(#lex{type=incl}),
              ?GRAPH:mklink(Incl, iref, Form),
              [First] = ?GRAPH:path(Form, [ffirst]),
              {tokens, macro_subst(include_form(First, Incl), Parent)}
      end,
      ?GRAPH:path(File, [{form, {type,'==',store}}])).

include_form(Lex, Incl) ->
    case ?GRAPH:data(Lex) of
        #lex{type=token, data=Data} ->
            case ?GRAPH:path(Lex, [next]) of
                [] ->  [{Data, Incl}]; 
                [N] -> [{Data, Incl} | include_form(N, Incl)]
            end;
        #lex{type=incl} ->
            [First] = ?GRAPH:path(Lex, [iref, ffirst]),
            include_form(First, Incl);
        D ->
            exit({include_form, Lex, D})
    end.

%% @spec clear(node()) -> ok
%%
%% @doc Removes all lexical information from `File'. This includes tokens,
%% macros and macro substitutions.
clear(File) ->
    [clear_tokens(First) || First <- ?GRAPH:path(File, [form, ffirst])],
    [clear_macro(File, Macro) || Macro <- ?GRAPH:path(File, [macro])],
    ok.

clear_tokens(Token) ->
    [clear_subst(S) || S <- ?GRAPH:path(Token, [{lfirst, back}])],
    case ?GRAPH:path(Token, [next]) of
        [Next] ->
            ?GRAPH:delete(Token),
            clear_tokens(Next);
        [] ->
            ?GRAPH:delete(Token)
    end.

clear_macro(_File, Macro) ->
    case ?GRAPH:path(Macro, [{mref, back}]) of
        [] -> ok;
        Subst ->
            error_logger:warning_msg("Deleting used macro: ~s~n",
                                     [(?GRAPH:data(Macro))#macro.name]),
            [clear_subst(S) || S <- Subst]
    end,
    ?GRAPH:delete(Macro).

clear_subst(Subst) ->
    [?GRAPH:delete(Par) || Par <- ?GRAPH:path(Subst, [llex])],
    ?GRAPH:delete(Subst).
