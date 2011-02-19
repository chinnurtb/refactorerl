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

%%% @doc Builder implementation. The module supports recompilation of
%%% changed source files, dependency caching, multiple targets, and build
%%% rules extensible using Erlang code.
%%%
%%% == Build rules ==
%%%
%%% The builder is directory-oriented, one run should build the contents of
%%% one directory. A directory may provide multiple build targets. Rules on
%%% how to build targets are provided in a file called `build.rules'. This
%%% file contains a set of Erlang expressions (each of them terminated with
%%% a full stop). These expressions may use variables defined by rules, and
%%% there are some predefined variables as well: `Dir' contains the path of
%%% the directory to be built, and `Target' contains the name of the target
%%% to be built. The following expression types are supported:
%%%
%%% <ol>
%%%
%%%  <li>Record expressions are used to specify build rules. The list of
%%%    supported rules and their exact meaning is listed below.</li>
%%%
%%%  <li>Match expressions with only a variable on the left hand side are
%%%    treated as variable assignment. These variables can be accessed in
%%%    other expressions, and they can be redefined.</li>
%%%
%%%  <li>Other expressions are simply evaluated and their result is
%%%    discarded (as well as their variable bindings).</li>
%%%
%%% </ol>
%%%
%%% == Rule types ==
%%% <dl>
%%%
%%%  <dt>`#target{name, rules}'</dt> <dd>This rule specifies how
%%%    to build a target. The `name' of the target should be an atom, and
%%%    the `rules' are specified as a list of build rules (these are handled
%%%    in the same way as top level expressions).</dd>
%%%
%%%  <dt>`#invoke{dir, target}'</dt> <dd>This rule can be used to
%%%    recursively run the build process on another directory and/or another
%%%    target. The directory is specified by a string that contains a
%%%    relative path, the default is the directory being built. The target
%%%    name is an atom, the default is the target currently being
%%%    built.
%%%
%%%    This rule can be used to describe target dependencies, or to build
%%%    multiple directories. Other directories should have their own
%%%    `build.rules' file that inherits all variables and rules from the
%%%    upper level build file.</dd>
%%%
%%%  <dt>`#rule{name, action, output, deps}'</dt> <dd>This rule specifies
%%%    how to build (generate, compile, ...) new files from existing ones.
%%%    Every rule has a unique `name' (an atom) and an `action' (a single
%%%    expression). When a rule is invoked on a source file, `action' is
%%%    executed (the file name is accessible in the variable `File').
%%%
%%%    `output' is an expression that should return either one, or a list of
%%%    file names (with their path relative to the directory being built):
%%%    these are the outputs of the build step. The build step is executed
%%%    only when there is a dependency that is newer than one of the
%%%    outputs. Note that this implies that rules with no `output' are never
%%%    executed.
%%%
%%%    `deps' (optional) is an expression that may return either one or a
%%%    list of dependency specifications of the form `{file, File}' or
%%%    `{mod, Module}'. The rule is not executed when every `File' and every
%%%    `Module' is older than all output files. Rules that have no source
%%%    files should have `deps' in order to be run.</dd>
%%%
%%%  <dt>`#source{rule, files}'</dt> <dd>This rule specifies that a set of
%%%    source `files' should be built using the given `rule'. `files' is an
%%%    expression that returns either one or a list of file name patterns
%%%    (interpreted by filelib:wildcard/1), these patterns are expanded
%%%    relatively to the directory being built. The `action' of `rule' is
%%%    executed separately on each file when necessary (there is a
%%%    dependency which is newer than one of the outputs, the source file
%%%    being an implicit dependency).</dd>
%%%
%%%  <dt>`#apply{files, action}'</dt> <dd>This is a more lightweight version
%%%    of `#source': `action' is an expression that is executed on every
%%%    file from `files' without condition. `files' is the same as in
%%%    `#source', `#action' is the same as in `#rule'.</dd>
%%%
%%% </dl>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_gen_build).
-vsn("$Rev: 4797 $").

-export([start/1, run/2, run/3]).
-export([erl_includes/2]).


%%% ============================================================================
%%% Entry functions

%% @spec start([Arg::string()]) -> halt()
%% @doc Command line entry point. It is designed to be called as `erl -run
%% referl_gen_build start target dir'. The first argument is a target name,
%% the second, optional argument is the directory to be built (the default
%% is ".", the current directory). Basically runs `run/2' and calls `halt'
%% to stop the emulator when finished.
start([Target]) ->
    start([Target, "."]);
start([Target, Dir]) ->
    case run(Dir, list_to_atom(Target)) of
        ok ->
            halt();
        read_error ->
            io:put_chars("Error reading build rules.\n"),
            halt(1);
        {build_error, _, _} ->
            io:put_chars("Build error.\n"),
            halt(2)
    end.

%% @spec run(atom(), string()) -> ok | read_error |
%%                                {build_error, Reason::term(), Stack::term()}
%% @doc Calls `run(Dir, Target, [])'.
%% @see run/3
run(Dir, Target) ->
    run(Dir, Target, []).



-record(env, {dir    = ".",
              target = default,
              rules  = dict:new(),
              vars   = erl_eval:new_bindings(),
              deps,
              force}).

%% @spec run(atom(), string(), [Opt]) -> ok | read_error |
%%                                {build_error, Reason::term(), Stack::term()}
%% @doc Build process entry point. Builds `Target' in directory `Dir'. `Opt'
%% may be one of the following:
%% <dl>
%%  <dt>`force'</dt> <dd>Force rebuilding even when the source has not been
%%  changed</dd>
%% </dl>
run(Dir, Target, Opt) ->
    Force = proplists:get_value(force, Opt, false),
    Deps = ets:new(deps, []),
    DepFile = filename:join(Dir, "build.deps"),
    case file:consult(DepFile) of
        {ok, [DepList]} when not Force -> ets:insert(Deps, DepList);
        _                              -> ok
    end,
    try run(#env{dir=Dir, target=Target, deps=Deps, force=Force}) of
        _ -> ok
    catch
        throw:Error -> Error;
        exit:clear_deps -> ets:delete_all_objects(Deps), ok
    after
        case ets:tab2list(Deps) of
            [] -> ok;
            DepL ->
                case file:write_file(DepFile,
                                     [io_lib:print(DepL), ".\n"]) of
                    ok -> ok;
                    {error, Reason} ->
                        io:format("Warning: build.deps:~s~n",
                                  [file:format_error(Reason)])
                end
        end,
        ets:delete(Deps)
    end.

run(#env{dir=Dir, target=Target, vars=Vars}=Env) ->
    File = filename:join(Dir, "build.rules"),
    case read_rules(File) of
        {ok, Build} ->
            build(Build, Env#env{vars=bind([{'Dir', Dir},
                                            {'Target', Target}], Vars)});
        {error, Reason} ->
            io:format("~s:~s~n", [File, Reason]),
            throw(read_error)
    end.


%%% ============================================================================
%%% Interpret rule files

read_rules(File) ->
    case file:open(File, [read]) of
        {ok, Dev} ->
            read_rules(Dev, 1, []);
        {error, Reason} ->
            {error, [" ", file:format_error(Reason)]}
    end.

read_rules(Dev, Line, Rules) ->
    case io:parse_erl_exprs(Dev, '', Line) of
        {ok, Exprs, EndLine} ->
            read_rules(Dev, EndLine, add_rules(Exprs, Rules));
        {eof, _} ->
            {ok, lists:reverse(Rules)};
        {error, {Ln, Mod, Err}, _} ->
            {error, io_lib:format("~b:~s",
                                  [Ln, apply(Mod, format_error, [Err])])}
    end.

add_rules(Exprs, Rules) ->
    lists:foldl(fun add_rule/2, Rules, Exprs).

add_rule(Expr, Rules) ->
    case erl_syntax:type(Expr) of
        record_expr ->
            add_rec_rule(erl_syntax:record_expr_type(Expr),
                         erl_syntax:record_expr_fields(Expr),
                         Rules);
        match_expr ->
            add_var_rule(erl_syntax:match_expr_pattern(Expr),
                         erl_syntax:match_expr_body(Expr),
                         Rules);
        _ ->
            add_exec_rule(Expr, Rules)
    end.

add_rec_rule(RecType, RecFld, Rules) ->
    Name = erl_syntax:atom_value(RecType),
    Params = [{erl_syntax:atom_value(erl_syntax:record_field_name(Fld)),
               erl_syntax:record_field_value(Fld)} || Fld <- RecFld],
    [{Name, Params} | Rules].

add_var_rule(Var, Expr, Rules) ->
    case erl_syntax:type(Var) of
        variable ->
            [{set, [{var, erl_syntax:variable_name(Var)},
                    {value, Expr}]} | Rules];
        T ->
            io:format("Warning: unhandled pattern type ~s~n", [T]),
            [{exec, [{expr, Expr}]} | Rules]
    end.

add_exec_rule(Expr, Rules) ->
    [{exec, [{expr, Expr}]} | Rules].


%%% ============================================================================
%%% Evaluate build rules

build([], Env) -> Env;

build([{set, Props} | Tail], #env{vars=Vars}=Env) ->
    Var = proplists:get_value(var, Props),
    Value = eval(value, Props, Vars, undefined),
    build(Tail, Env#env{vars=bind([{Var, Value}], Vars)});

build([{rule, Props} | Tail], #env{rules=Rules, vars=Vars}=Env) ->
    Name = eval(name, Props, Vars, default),
    build(Tail, Env#env{rules=dict:store(Name, rule(Name, Props), Rules)});

build([{source, Props} | Tail], #env{dir=Dir, rules=Rules, vars=Vars}=Env) ->
    RuleName = eval(rule, Props, Vars, default),
    Rule = dict:fetch(RuleName, Rules),

    case get_files(Props, Vars, Dir) of
        []    -> apply_rule1(Rule, [], Env);
        Files -> apply_rule(Rule, Files, Env)
    end,
    build(Tail, Env);

build([{apply, Props} | Tail], #env{dir=Dir, vars=Vars}=Env) ->
    Action = get_fun(action, Props, []),
    lists:foreach(
      fun (File) ->
              io:put_chars([File, "\n"]),
              Action(bind([{'File',File}], Vars))
      end,
      get_files(Props, Vars, Dir)),
    build(Tail, Env);

build([{target, Props} | Tail], #env{target=Target, vars=Vars}=Env) ->
    case eval(name, Props, Vars, default) of
        Target ->
            Build = lists:reverse(add_rules(get_list(rules, Props), [])),
            build(Build ++ Tail, Env);
        _ ->
            build(Tail, Env)
    end;

build([{invoke, Props} | Tail], #env{target=Target, dir=Dir, vars=Vars}=Env) ->
    IncDir = eval(dir, Props, Vars, "."),
    IncTarget = eval(target, Props, Vars, Target),
    case eval(precond, Props, Vars, true) of
        true ->
            #env{rules=NewRules} =
                run(Env#env{dir=filename:join(Dir, IncDir), target=IncTarget}),
            build(Tail, Env#env{rules=NewRules});
        false ->
            build(Tail, Env)
    end;

build([{exec, Props} | Tail], #env{vars=Vars}=Env) ->
    eval(expr, Props, Vars, undefined),
    build(Tail, Env);

build([{Rec, _} | Tail], Env) ->
    io:format("Warning: unhandled rule ~s~n", [Rec]),
    build(Tail, Env).

get_files(Props, Vars, Dir) ->
    get_files(files, Props, Vars, Dir) -- get_files(skip_files, Props, Vars, Dir).

get_files(Tag, Props, Vars, Dir) ->
        lists:append(
          [filelib:wildcard(filename:join(Dir, F)) ||
              F <- mklist(eval(Tag, Props, Vars, []))]).


%%% ============================================================================
%%% Rule setup and execution

-record(rule, {name, output, action, deps}).
rule(Name, RuleProps) ->
    #rule{name   = Name,
          output = get_fun(output, RuleProps, []),
          action = get_fun(action, RuleProps),
          deps   = get_fun(deps, RuleProps, [])}.


apply_rule(Rule, Files, Env) ->
    lists:foreach(fun (F) -> apply_rule1(Rule, F, Env) end, Files).

apply_rule1(#rule{output=OutF, action=ActF, deps=DepF}=Rule, File,
            #env{dir=Dir, deps=Deps} = Env) ->
    Vars = bind([{'File', File}], Env#env.vars),

    {FileDeps, DepIsOld} =
        case ets:lookup(Deps, filename:absname(File)) of
            [{_, OldDeps}] -> {OldDeps, true};
            []                -> {file_deps(DepF, File, Vars), false}
        end,
    DepTimes = [modtime(Dep) || Dep <- FileDeps],

    Output   = [filename:join(Dir, F) || F <- mklist(OutF(Vars))],
    OutTimes = [modtime({file,Out}) || Out <- Output],

    Newer    = (Env#env.force orelse
                lists:max(DepTimes) > lists:min(OutTimes)),
    Missing  = lists:any(fun(0) -> true; (_) -> false end, DepTimes),
    if
        Newer, not Missing ->
            message(Rule, File, Output),
            try
                ActF(Vars)
            catch
                error:Error ->
                    io:format("Error building target `~s' in directory ~s:~n"
                              "    rule `~s' failed on source ~s~n",
                              [Env#env.target, Dir, Rule#rule.name, File]),
                    throw({build_error, Error, erlang:get_stacktrace()})
            end;
        Missing ->
            io:format(
              "~s: warning: missing dependency:~n~s",
              [string:join(Output, " "),
               [case D of
                    {file, F} -> ["    ", F, "\n"];
                    {mod, M}  -> ["    module ", atom_to_list(M), "\n"]
                end || D <- FileDeps, modtime(D) =:= 0]]);
        not Newer ->
            ok
    end,
    if
        Newer, DepIsOld ->
            ets:insert(Deps, {filename:absname(File),
                              file_deps(DepF, File, Vars)});
        not DepIsOld ->
            ets:insert(Deps, {filename:absname(File), FileDeps});
        true ->
            ok
    end.

file_deps(DepF, File, Vars) ->
    [case Dep of
         {file, F} -> {file, filename:absname(F)};
         _         -> Dep
     end || Dep <- file_deps(File, mklist(DepF(Vars)))].

file_deps("", Deps)   -> Deps;
file_deps(File, Deps) -> [{file, File} | Deps].


modtime({file, File}) ->
    case filelib:last_modified(File) of
        0 -> 0;
        T -> calendar:datetime_to_gregorian_seconds(
               hd(calendar:local_time_to_universal_time_dst(T)))
    end;
modtime({mod, Mod}) ->
    try
        {Y,M,D,H,N,S} = proplists:get_value(time, Mod:module_info(compile)),
        calendar:datetime_to_gregorian_seconds({{Y,M,D},{H,N,S}})
    catch
        error:{badmatch,undefined} -> 1; % unknown compile time, but exists
        error:undef                -> 0
    end.

mklist([])                           -> [];
mklist(Str) when is_integer(hd(Str)) -> [Str];
mklist(Lst) when is_list(Lst)        -> Lst;
mklist(Elem)                         -> [Elem].

message(#rule{name=Name}, Input, Output) ->
    File =
        if
            Input == "" -> string:join(Output, " ");
            true -> Input
        end,
    io:put_chars(["[", atom_to_list(Name), "] ", File, "\n"]).


%%% ============================================================================
%%% Expression evaluation

bind(Vars, V0) ->
    lists:foldl(fun({N,V}, B) -> erl_eval:add_binding(N,V,B) end, V0, Vars).


eval(Key, Props, Vars, Default) ->
    case proplists:lookup(Key, Props) of
        none        -> Default;
        {Key, Expr} -> real_eval(Expr, Vars)
    end.

get_list(Key, Props) ->
    case proplists:lookup(Key, Props) of
        none ->
            [];
        {Key, Expr} ->
            case erl_syntax:type(Expr) of
                list ->
                    erl_syntax:list_elements(Expr);
                _ ->
                    [Expr]
            end
    end.

get_fun(Key, Props) ->
    case proplists:lookup(Key, Props) of
        none ->
            throw({no_field, Key});
        {Key, Expr} ->
            fun(Vars) ->
                    real_eval(Expr, Vars)
            end
    end.

get_fun(Key, Props, Def) ->
    case proplists:lookup(Key, Props) of
        none ->
            fun(_) -> Def end;
        {Key, Expr} ->
            fun(Vars) ->
                    real_eval(Expr, Vars)
            end
    end.

real_eval(Expr, Vars) ->
    {value, Result, _} = erl_eval:expr(Expr, Vars,
                                       {value, fun local_handler/2}),
    Result.

local_handler(Fun, Args) ->
    apply(filename, Fun, Args).


%%% ============================================================================
%%% Utilities for rule files

%% @spec erl_includes(string(), [string()]) -> [string()]
%% @doc Returns files included by `File'. `IncPath' is the search path for
%% include files.

%% Based on make:check_includes
erl_includes(File, IncPath) ->
    Path = [filename:dirname(File) | IncPath],
    case epp:open(File, Path, []) of
	{ok, Epp} ->
	    get_includes(Epp, File, []);
	_Error ->
	    []
    end.

get_includes(Epp, File, Inc) ->
    case epp:parse_erl_form(Epp) of
	{ok, {attribute, 1, file, {File, 1}}} ->
	    get_includes(Epp, File, Inc);
	{ok, {attribute, 1, file, {IncFile, 1}}} ->
            get_includes(Epp, File, [IncFile|Inc]);
	{ok, _} ->
	    get_includes(Epp, File, Inc);
	{eof, _} ->
	    epp:close(Epp),
	    Inc;
	{error, _Error} ->
	    get_includes(Epp, File, Inc)
    end.
