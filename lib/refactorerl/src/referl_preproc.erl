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
%%%   storing the specified files (@see ?FileMan:add_file/1), and
%%%   substituting their contents during preprocessing. Note that macro and
%%%   record definitions are not substituted, only unknown attributes and
%%%   function definitions.</li>
%%%
%%% <li>`define' directives are parsed and macros are stored using
%%%   `#form{type=macro, tag=Name}' graph nodes.</li>
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
%%%   Include line number with tokens, possibly {file, line} -- better error
%%%      reporting

-module(referl_preproc).
-vsn("$Rev: 2559 $").

-export([preprocess/2, preprocess/3]).

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
    Def = ?Graph:path(?Graph:root(),
                      [{env, {{name, '==', def}, 'and',
                              {value, '==', macro_name(Name)}}}]),
    {[{form, create_form(lex, If, TNodes)}],
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
    {[{form, create_form(lex, else, [T1, T2, T3])}],
     case StTop of
         copy -> [skip|St];
         skip -> [copy|St]
     end};

preprocess([{#token{type=minus},              T1},
            {#token{type=atom, value=endif},  T2},
            {#token{type=stop},               T3}], _File, [_|St]) ->
    {[{form, create_form(lex, endif, [T1, T2, T3])}], St};

preprocess(Form, File, St=[StTop|_]) ->
    case StTop of
        copy ->
            {form(Form, File), St};
        skip ->
            {[{form, create_form(lex, skip, token_nodes(Form))}], St}
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
      {#token{type=op_paren},                     T3} | Tail], _File) ->
    TNodes = [T1, T2, T3],
    try define(Tail) of
        {Name, Body} ->
            [{form, create_form(macro, Name, TNodes ++ Body)}]
    catch
        throw:Error ->
            [{form, create_form(error, Error, TNodes ++ token_nodes(Tail))}]
    end;

form(Tokens, File) ->
    try
        [{tokens, macro_subst(Tokens, File)}]
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

%% @spec define(formTokens()) -> [node()]
%% @doc Analyses macro definition, returns the lexical nodes
define([{#token{type=NameType, value=Name}, T} | Tokens])
  when NameType == atom; NameType == variable ->
    {macro_name(Name), [T | define(arglist, Tokens, none)]}.

define(arglist, [{#token{type=op_paren}, T1},
                 {#token{type=cl_paren}, T2},
                 {#token{type=comma},    T3} | Tokens], none) ->
    Arg = ?ESG:create(#lex{type=arg}),
    ?ESG:insert(Arg, llex, T1),
    ?ESG:insert(Arg, llex, T2),
    [Arg, T3 | define(body, Tokens, none)];
define(arglist, [{#token{type=op_paren}, T} | Tokens], none) ->
    Arg = ?ESG:create(#lex{type=arg}),
    ?ESG:insert(Arg, llex, T),
    define(nextarg, Tokens, Arg);
define(arglist, [{#token{type=comma},    T} | Tokens], none) ->
    [T | define(body, Tokens, none)];

define(nextarg, [{#token{type=variable}, T} | Tokens], Arg) ->
    ?ESG:insert(Arg, llex, T),
    define(argsep, Tokens, Arg);

define(argsep, [{#token{type=comma},     T} | Tokens], Arg) ->
    ?ESG:insert(Arg, llex, T),
    define(nextarg, Tokens, Arg);
define(argsep, [{#token{type=cl_paren},  T1},
                {#token{type=comma},     T2} | Tokens], Arg) ->
    ?ESG:insert(Arg, llex, T1),
    [Arg, T2 | define(body, Tokens, none)];

define(body, Tokens, none) ->
    define(body, Tokens, ?ESG:create(#lex{type=body}));

define(body, [{#token{type=cl_paren},    T},
              {#token{type=stop},        Stop} ], Body) ->
    [Body, T, Stop];
%% Quote from epp.erl: "Be nice, allow no right paren!"
define(body, [{#token{type=stop},        Stop} ], Body) ->
    [Body, Stop];
define(body, [{_, T} | Tokens], Body) ->
    ?ESG:insert(Body, llex, T),
    define(body, Tokens, Body);

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
    MacroPath = [incl, {form, {{type, '==', macro}, 'and',
                               {tag, '==', macro_name(Name)}}}],
    case lists:usort(?Graph:path(File, MacroPath)) of
        [Def|Others] ->
            if
                Others =/= [] ->
                    error_logger:warning_msg("Multiply defined macro ~s~n",
                                             [macro_name(Name)]);
                true -> ok
            end,
            App = macro_app(Name, [First, N]),
            ?Graph:mklink(App, mref, Def),
            %% Look for macro parameters only when the definition needs them
            case ?Graph:path(Def, [{flex, {type, '==', arg}}]) of
                [] -> {App, [], Tokens};
                _  -> macro_params(Tokens, App)
            end;
        [] when Name == "MODULE";
                Name == "MODULE_STRING" ->
            case ?Graph:data(File) of
                #file{type=module, path=Path} ->
                    ModName = filename:basename(Path, ".erl"),
                    App = macro_app(Name, [First, N]),
                    {done,
                     [{if
                           Name == "MODULE" ->
                               #token{type=atom, value=list_to_atom(ModName)};
                           true ->
                               #token{type=string, value=ModName}
                       end,
                       virtual(none, App)}],
                     Tokens};
                _ -> skip
            end;
        [] when Name == "FILE" ->
            App = macro_app(Name, [First, N]),
            #file{path=Path} = ?Graph:data(File),
            {done,
             [{#token{type=string, value=Path}, virtual(none, App)}],
             Tokens};
        [] when Name == "LINE" ->
            App = macro_app(Name, [First, N]),
            {done,
             [{#token{type=integer, value=42}, virtual(none, App)}],
             Tokens};
        [] ->
            error_logger:warning_msg("Non-local macro ~s in ~s~n",
                                     [macro_name(Name),
                                      (?Graph:data(File))#file.path]),
            skip
    end.

macro_app(Name, Tokens) ->
    App = ?ESG:create(#lex{type=subst, data=macro_name(Name)}),
    lists:foreach(fun(T) -> ?ESG:insert(App, llex, T) end, Tokens),
    App.

macro_params([{#token{type=op_paren}, O},
              {#token{type=cl_paren}, C} | Tokens], App) ->
    ?ESG:insert(App, llex, O),
    ?ESG:insert(App, llex, C),
    {App, [], Tokens};

macro_params([{#token{type=op_paren}, N} | Tokens], App) ->
    ?ESG:insert(App, llex, N),
    macro_param(Tokens, [], App).

macro_param(Tokens, Params, App) ->
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
    macro_param(Tokens, [lists:reverse(Ps)|Params], App);

macro_param([P={#token{}, T} | Tokens],
            ParOb, Ps, Paren, Params, App) ->
    ?ESG:insert(ParOb, llex, T),
    macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App).


%% @spec substitute(node(), formTokens()) -> formTokens()
%% @doc Generates the result of a macro substitution
substitute(App, Params) ->
    [Def] = ?Graph:path(App, [mref]),
    ArgTokens = [?Graph:data(A) ||
                    A <- ?Graph:path(Def, [{flex, {type, '==', arg}}, llex])],
    Args = [Var || #lex{data=#token{type=variable, value=Var}} <- ArgTokens],
    Body = ?Graph:path(Def, [{flex, {type, '==', body}}, llex]),
    substitute([{(?Graph:data(N))#lex.data, N} || N <- Body],
               Args, Params, App).

%% @spec substitute(Body::formTokens(), [string()], [formTokens()], node()) ->
%%          formTokens()
substitute([{#token{type=variable, value=Arg}, N}|Rest], Args, Params, App) ->
    [{Data, virtual(Node, App)} ||
        {Data, Node} <- subs_arg(Arg, N, Args, Params)] ++
        substitute(Rest, Args, Params, App);
substitute([{#token{type=questionm}, _},
            {#token{type=questionm}, _},
            {#token{type=variable, value=Arg}, N}|Rest], Args, Params, App) ->
    %% Proper text substitution could be implemented here if needed
    [{#token{type=string, value=Arg}, virtual(N, App)} |
     substitute(Rest, Args, Params, App)];
substitute([{Data, Node} | Rest], Args, Params, App) ->
    [{Data, virtual(Node, App)} | substitute(Rest, Args, Params, App)];
substitute([], _, _, _) ->
    [].

subs_arg(Name, _, [Name|_], [Value|_]) ->
    Value; 
subs_arg(Name, Node, [_|Args], [_|Params]) ->
    subs_arg(Name, Node, Args, Params);
subs_arg(Name, Node, [], _) ->
    [{#token{type=variable, value=Name}, Node}].


macro_name(Atom) when is_atom(Atom) -> atom_to_list(Atom);
macro_name(Str)  when is_list(Str)  -> Str.


%% Looks for the first existing include file in the list of include
%% directories.
find_include(include, Name, FileNode) ->
    case ?Graph:path(?Graph:root(), ?File:find(Name)) of
        [_InclFile] -> Name;
        _ -> 
            #file{path=FilePath} = ?Graph:data(FileNode),
            Base = filename:dirname(FilePath),
            Path = [{env, {name, '==', include}}],
            Dirs = [(?Graph:data(Dir))#env.value ||
                       Dir <- ?Graph:path(?Graph:root(), Path)],
            case [Filename ||   Dir      <- [Base | Dirs],
                                Filename <- [filename:join(Dir, Name)],
                                filelib:is_file(Filename)] of
                [Filename|_] -> Filename;
                []       -> throw({no_include_file, Name})
            end
    end;

find_include(include_lib, Name, _) ->
    {App, [$/ | Dir]} = lists:splitwith(
                          fun (Ch) -> Ch /= $/ end,
                          Name),
    Path = [{env, {name, '==', appbase}}],
    case [filename:join(AD, Dir) ||
             #env{value=Base} <- [?Graph:data(E) ||
                                     E <- ?Graph:path(?Graph:root(), Path)],
             AD <- filelib:wildcard(filename:join(Base, App) ++ "*")] of
        [Filename|_] -> Filename;
        []           -> throw({no_include_file, Name})
    end.


%% @spec include(node(), string(), [node()]) ->
%%         [{tokens,formTokens()} | {form, node()}]
include(File, IncName, Tokens) ->
    FileType = (?Graph:data(File))#file.type,
    case ?FileMan:add_file(IncName) of
        {error, Reason} ->
            throw({include_error, IncName, Reason});
        {file, IncFile} ->
            [?Graph:mklink(File, incl, Inc) ||
                Inc <- ?Graph:path(IncFile, [incl])],
            if
                FileType =:= header ->
                    Form = create_form(lex, include, Tokens),
                    ?Graph:mklink(Form, iref, IncFile),
                    [{form, Form}];
                FileType =:= module ->
                    IncNode = ?ESG:create(#lex{type=incl}),
                    [?ESG:insert(IncNode, llex, T) || T <- Tokens],
                    IncForm = create_form(lex, include, [IncNode]),
                    ?Graph:mklink(IncForm, iref, IncFile),
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
              include_form(?Graph:data(Form), Form, IncNode)
      end,
      ?Graph:path(IncFile, [form])).

%% @spec include_form(#form{}, node(), node()) -> [formTokens()]
include_form(#form{tag=store}, Form, IncNode) ->
    [[include_token(T, IncNode) || T <- ?Graph:path(Form, [flex])]];
include_form(#form{tag=include}, Form, IncNode) ->
    [File] = ?Graph:path(Form, [iref]),
    include_forms(File, IncNode);
include_form(#form{}, _Form, _IncNode) ->
    [].

include_token(Token, IncNode) ->
    #lex{type=token, data=TD} = ?Graph:data(Token),
    {TD, virtual(Token, IncNode)}.


%% Creates a virtual token
virtual(Token, Source) ->
    VT = ?ESG:create(#lex{type=token, data=virtual}),
    if
        Token =/= none ->
            ?Graph:mklink(VT, orig, Token);
        true ->
            ok
    end,
    ?ESG:insert(VT, llex, Source),
    VT.
