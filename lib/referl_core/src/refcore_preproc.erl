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
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

%%% Plans:
%%%   Include line number with tokens, possibly {file, line} -- better error
%%%      reporting

-module(refcore_preproc).
-vsn("$Rev: 5262 $ ").

-export([preprocess/2, preprocess/3, detach/2]).

-include("core.hrl").

%%% @type formTokens() = [{#token{}, node()}]. A list that contains token
%%% data for a form.

%%% @type processedForm() = {token, formTokens()} |
%%%                         {vtokens, Dep::node(), Orig::node(), formTokens()} |
%%%                         {form, node()}.
%%% Represetation of a form after preprocessing. The result may be a comlpeted
%%% lexical form that needs no further processing, or a list of tokens.
%%% Virtual forms may appear after file inclusion; these are entirely consist
%%% of virtual tokens copied from form `Orig', with their origin in another
%%% file, which is referred by form `Dep'.

%% @spec preprocess(formTokens(), node()) -> [processedForm()]
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
%%                                           {[processedForm()], state()}
%%
%% @doc Re-entrant preprocessor that handles conditional compilation. The
%% same notes apply as for `preprocess/2'. First time this function must be
%% called with `start' as the state value, subsequent calls should pass the
%% next form token list and the state returned from the previous call. Note
%% that file inclusion will close the active ESG batch, so returned form
%% nodes must be inserted into the syntax tree by the caller immediately.
preprocess(Tokens, File, start) ->
    preprocess(Tokens, File, [copy]);

preprocess([{#token{type='-'},                T1},
            {#token{type=atom, value=If},     T2},
            {#token{type='('},                T3},
            {#token{type=AtVar, value=Name},  T4},
            {#token{type=')'},                T5},
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

preprocess([{#token{type='-'},                T1},
            {#token{type=atom, value=else},   T2},
            {#token{type=stop},               T3}], _File, [StTop|St]) ->
    {[{form, create_form(lex, else, [T1, T2, T3])}],
     case StTop of
         copy -> [skip|St];
         skip -> [copy|St]
     end};

preprocess([{#token{type='-'},                T1},
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

%% @spec form(formTokens(), node()) -> [processedForm()]
%%
%% @doc Preprocesses a form: macro definitions are stored (without further
%% preprocessing), included files are linked to the top level file, other
%% forms are subject to macro substitution. The ?MODULE macro is created
%% here when encountering a `-module' attribute. Preprocessor directives are
%% turned into an analysed form, other forms are returned as token lists.
form([{#token{type='-'},                        T1},
      {#token{type=atom,      value=Include},   T2},
      {#token{type='('},                        T3},
      {#token{type=string,    value=Filename},  T4},
      {#token{type=')'},                        T5},
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

form([{#token{type='-'},                          T1},
      {#token{type=atom,           value=define}, T2},
      {#token{type='('},                          T3} | Tail], _File) ->
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

define(arglist, [{#token{type='('}, T1},
                 {#token{type=')'}, T2},
                 {#token{type=','}, T3}     | Tokens], none) ->
    Arg = ?ESG:create(#lex{type=arg}),
    ?ESG:insert(Arg, llex, T1),
    ?ESG:insert(Arg, llex, T2),
    [Arg, T3 | define(body, Tokens, none)];
define(arglist, [{#token{type='('},      T} | Tokens], none) ->
    Arg = ?ESG:create(#lex{type=arg}),
    ?ESG:insert(Arg, llex, T),
    define(nextarg, Tokens, Arg);
define(arglist, [{#token{type=','},      T} | Tokens], none) ->
    [T | define(body, Tokens, none)];

define(nextarg, [{#token{type=variable}, T} | Tokens], Arg) ->
    ?ESG:insert(Arg, llex, T),
    define(argsep, Tokens, Arg);

define(argsep, [{#token{type=','},       T} | Tokens], Arg) ->
    ?ESG:insert(Arg, llex, T),
    define(nextarg, Tokens, Arg);
define(argsep, [{#token{type=')'},      T1},
                {#token{type=','},      T2} | Tokens], Arg) ->
    ?ESG:insert(Arg, llex, T1),
    [Arg, T2 | define(body, Tokens, none)];

define(body, Tokens, none) ->
    define(body, Tokens, ?ESG:create(#lex{type=body}));

define(body, [{#token{type=')'},         T},
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
macro_subst([Head = {#token{type='?'}, Q} | Tail], File) ->
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
%% `{done, Result, Rest}' for special macros substituted here, and
%% `{App, Params, Rest}' for normal macros to be substituted later.
macro([{#token{type=NameType, value=Name}, N} | Tokens], File, First)
  when NameType == atom; NameType == variable ->
    MacroPath = [{incl, back}, incl, {form, {{type, '==', macro}, 'and',
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

macro_params([{#token{type='('}, O},
              {#token{type=')'}, C} | Tokens], App) ->
    ?ESG:insert(App, llex, O),
    ?ESG:insert(App, llex, C),
    {App, [], Tokens};

macro_params([{#token{type='('}, N} | Tokens], App) ->
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
macro_param([P={#token{type=ClParen}, N} | Tokens],
            ParOb, Ps, [ClParen|Paren], Params, App) ->
    ?ESG:insert(ParOb, llex, N),
    macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App);

macro_param([{#token{type=')'}, N} | Tokens],
            _ParOb, Ps, [], Params, App) ->
    ?ESG:insert(App, llex, N),
    {App, lists:reverse([lists:reverse(Ps)|Params]), Tokens};

macro_param([{#token{type=','}, N} | Tokens],
            ParOb, Ps, [], Params, App) ->
    ?ESG:insert(ParOb, llex, N),
    macro_param(Tokens, [lists:reverse(Ps)|Params], App);

macro_param([P={#token{type=Type}, N} | Tokens],
            ParOb, Ps, Paren, Params, App) ->
    ?ESG:insert(ParOb, llex, N),
    case close_paren(Type) of
        {type, ClType} ->
            macro_param(Tokens, ParOb, [P|Ps], [ClType | Paren], Params, App);
        none ->
            macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App)
    end.

close_paren('(')       -> {type, ')'};
close_paren('{')       -> {type, '}'};
close_paren('[')       -> {type, ']'};
close_paren('<<')      -> {type, '>>'};
close_paren('begin')   -> {type, 'end'};
close_paren('if')      -> {type, 'end'};
close_paren('case')    -> {type, 'end'};
close_paren('receive') -> {type, 'end'};
close_paren('try')     -> {type, 'end'};
close_paren('cond')    -> {type, 'end'};
close_paren(_)         -> none.

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
substitute([{#token{type='?'}, _},
            {#token{type='?'}, _},
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

%% Returns the canonical name of the file, which does not contain ".."
%% in the path. Should work for both Unix and Windows (and mixed) style paths.
canonical_name(Filename) ->
    case elim_up_dir(elim_up_dir(Filename, "\\\\"), "/") of
        Filename -> Filename;
        Shorter  -> canonical_name(Shorter)
    end.

%% Eliminates a "/dir/../" section from the path.
%% `Sep' is the path separator, which has to expand to one regexp character.
elim_up_dir(Filename, Sep) ->
    re:replace(Filename,
               "[^" ++ Sep ++ "]+" ++ Sep ++ "[.][.]" ++ Sep ++ "?", "",
               [{return, list}]).

%% Looks for the first existing include file in the list of include
%% directories.
find_include(include, Name, FileNode) ->
    case ?Graph:path(?Graph:root(), [{file, {path, '==', Name}}]) of
        [_InclFile] -> Name;
        _ ->
            #file{path=FilePath} = ?Graph:data(FileNode),
            RealName = real_path(Name),
            Base = filename:dirname(FilePath),
            AppBases = dirs_by_env(appbase),
            Dirs = dirs_by_env(include) ++
                   component_dir(AppBases, "include") ++
                   component_dir(AppBases, "src"),
            case [Filename ||   Dir      <- [Base | Dirs],
                                Filename <- [filename:join(Dir, RealName)],
                                filelib:is_file(Filename)] of
                [Filename|_] -> canonical_name(Filename);
                []       ->
                    error_logger:warning_msg("Include file not found: ~s~n",
                                             [Name]),
                    throw({no_include_file, Name})
            end
    end;

find_include(include_lib, Name, _) ->
    AppBases = dirs_by_env(appbase),
    case lists:member($/, Name) of
        false ->
            throw({include_lib, application_not_specified, Name});
        true ->
            {BaseName, [$/ | Dir]} =
                lists:splitwith(fun (Ch) -> Ch /= $/ end, Name),
            case app_files(AppBases, BaseName, Dir) of
                [Filename|_] -> canonical_name(Filename);
                []           ->
                    error_logger:warning_msg("Include lib not found: ~s~n",
                                             [Name]),
                    throw({no_include_file, Name})
            end
    end.

%% @doc Returns the specified component subdirectory (e.g. 'src' or 'include')
%% from all applications.
component_dir(AppBases, SubDir) ->
    Dirs = lists:concat([filelib:wildcard(filename:join([AppBase, "*", SubDir]))
                || AppBase <- AppBases]),
    [Dir || Dir <- Dirs, filelib:is_dir(Dir)].

%% @doc Resolves the beginning environment variable of the path.
%% Primarily it searches for an #env{name=env_var, value={EnvName, Path}}
%% environment in the graph,
%% secondarily it checks the OS environment variables.
real_path([$$|Path]) ->
    ["", EnvVar|Rest] = re:split(Path, "([a-zA-Z_]*\/?)", [{return, list}]),
    BEnvVar = filename:basename(EnvVar),
    EnvPath =
        case proplists:get_value(BEnvVar, ?Syn:get_env(env_var)) of
            undefined ->
                case os:getenv(BEnvVar) of
                    false -> throw({unknown_env_in_include, [$$|BEnvVar]});
                    EnvPath2 -> EnvPath2
                end;
            EnvPath2 ->
                EnvPath2
        end,
    filename:join(EnvPath, lists:flatten(Rest));
real_path(Path) ->
    Path.

%% Returns the names of the files
%% that are described in the named environment.
dirs_by_env(Name) ->
    [(?Graph:data(Dir))#env.value ||
        Dir <- ?Graph:path(?Graph:root(), [{env, {name, '==', Name}}])].

%% Returns the files in the specified directory of the application.
%% The function tries to access all version numbers of `BaseName'.
app_files(AppBases, BaseName, Dir) ->
    [filename:join(AD, Dir) ||
        AppBase <- AppBases,
        AD <- filelib:wildcard(filename:join(AppBase, BaseName ++ "*"))].



%% @spec include(node(), string(), [node()]) -> [processedForm()]
%% @doc Adds an include file into the graph. Returns the form list that is the
%% result of including the file.
include(File, IncName, Tokens) ->
    FileType = (?Graph:data(File))#file.type,
    case ?Graph:path(?Graph:root(), [{file, {path, '==', IncName}}]) of
        [File2] -> Add = {none, File2};
        _       -> Add = {new, File2=?FileMan:create_file_node(IncName, eol)}
    end,
    ?Graph:mklink(File, incl, File2),
    case ?FileMan:add_file(Add) of
        {error, Reason} ->
            ?Graph:rmlink(File, incl, File2),
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
                     [{vtokens, IncForm, Orig, macro_subst(Form, File)} ||
                         {Orig, Form} <- include_forms(IncFile, IncNode)]]
            end
    end.

%% @spec include_forms(node(), node()) -> [{node(), formTokens()}]
include_forms(IncFile, IncNode) ->
    lists:flatmap(
      fun
          (Form) ->
              include_form(?Graph:data(Form), Form, IncNode)
      end,
      ?Graph:path(IncFile, [form])).

%% @spec include_form(#form{}, node(), node()) -> [formTokens()]
include_form(#form{tag=store}, Form, IncNode) ->
    [{Form, [include_token(T, IncNode) || T <- ?Graph:path(Form, [flex])]}];
%% TODO: what about include_lib?
include_form(#form{tag=include}, Form, IncNode) ->
    [File] = ?Graph:path(Form, [iref]),
    include_forms(File, IncNode);
include_form(#form{}, _Form, _IncNode) ->
    [].

include_token(Token, IncNode) ->
    {Orig, Data} = orig_token(Token),
    {Data, virtual(Orig, IncNode)}.

orig_token(Token) ->
    case ?Graph:data(Token) of
        #lex{type=token, data=virtual} ->
            [Orig] = ?Graph:path(Token, [orig]),
            orig_token(Orig);
        #lex{type=token, data=Data} ->
            {Token, Data}
    end.

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

%% @spec detach(node(), node()) -> ok
%% @doc Detaches preprocessor-generated form `Form' from its container file
%% `File' by deleting preprocessor-generated links. Currently this only means
%% cleaning up file inclusion information, other preprocessor-related stuff is
%% represented in the syntax tree.
detach(File, Form) ->
    case ?Graph:path(Form, [iref]) of
        [] -> ok;
        [Incl] -> remove_include(File, Incl)
    end.

remove_include(File, Incl) ->
    OtherIncl = ?Graph:path(File, [form, iref]) -- [Incl],
    Hold = lists:append([?Graph:path(I, [incl]) || I <- OtherIncl]),
    lists:foreach(
      fun (Drop) -> ?Graph:rmlink(File, incl, Drop) end,
      ?Graph:path(Incl, [incl]) -- Hold).
