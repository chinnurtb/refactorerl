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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created by E�tv�s Lor�nd University are Copyright 2008, E�tv�s
%%% Lor�nd University. All Rights Reserved.

%%% @doc RefactorErl build script: generates the scanner and the parser,
%%% compiles modules, creates release descriptors, startup scripts, release
%%% tarballs.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(build).
-vsn("$Rev: 1247 $").

-export([start/0, cmp/0, rel/0, doc/0, build/3]).

-define(SYNTAX_FILE, "refac_syntax").
-define(SYNTAX_SUFFIX, ".xml").
-define(PARSER_SUFFIX, "_parser.yrl").
-define(SCANNER_SUFFIX, "_scanner.xrl").
-define(PARSER_OUTPUT, "_parser.erl").
-define(SCANNER_OUTPUT, "_scanner.erl").

%% @doc Stand-alone builder function. It is to be used as `erl -noshell -run
%% build'.
start() ->
    case rel() of
        ok    -> halt();
        error -> halt(1);
        _     -> halt(2)
    end.

%% @doc Compiles all modules that are out of date. Note that this includes the
%% scanner and parser generation.
cmp() ->
    build(compile, [load]).

%% @doc Creates a complete release. This includes the compilation phase
%% besides the generation of the release descriptor and the startup script.
rel() ->
    build(release, []).

%% @doc Creates the edoc documentation of the system.
doc() ->
    build(docs, []).

build(Phase, Opts) ->
    {ok, Cwd} = file:get_cwd(),
    try
        BaseDir = get_base_dir(),
        refresh_builder(BaseDir),
        ?MODULE:build(Phase, BaseDir, Opts)
    catch
        E ->
            io:format("Error: ~p~n", [E]),
            error
    after
        file:set_cwd(Cwd)
    end.

%% @private
%% @spec build(Phase, BaseDir, Opts) -> ok
%%       Phase = compile | release | docs
%%       BaseDir = string()
%%       Opts = [atom()]
build(compile, BaseDir, Opts) ->
    file:set_cwd(filename:join(BaseDir, "lib")),
    build_parser(Opts),
    make(Opts);

build(release, BaseDir, Opts) ->
    build(compile, BaseDir, Opts),
    build_release(BaseDir);

build(docs, _, _) ->
    edoc:application(refactorerl),
    edoc:application(clustering).

get_base_dir() ->
    BaseDir = case code:lib_dir(refactorerl) of
                  {error, _} ->
                      {ok, Cwd} = file:get_cwd(),
                      find_base_dir(filename:split(Cwd));
                  Dir when is_list(Dir) ->
                      find_base_dir(filename:split(Dir))
              end,
    lists:map(fun(D) -> code:add_patha(filename:join(BaseDir, D)) end,
              filelib:wildcard("lib/*/ebin", BaseDir)),
    BaseDir.

find_base_dir([]) ->
    exit(builder_not_found);
find_base_dir(Path) ->
    Dir = filename:join(Path),
    case filelib:is_dir(filename:join(Dir, "build")) of
        true ->
            Dir;
        false ->
            [_ | Prefix] = lists:reverse(Path),
            find_base_dir(lists:reverse(Prefix))
    end.
            
make(Opts) ->
    case make:all(Opts) of
        error -> throw(make_error);
        _ -> ok
    end.
            

refresh_builder(BaseDir) ->
    BuildDir = filename:join(BaseDir, "build"),
    code:add_pathz(BuildDir),
    file:set_cwd(BuildDir),
    make([load]).

build_parser(_Opts) ->
    {ok, Cwd} = file:get_cwd(),
    try
        file:set_cwd(filename:join([Cwd, "refactorerl", "src"])),
        InTime = file_mtime(?SYNTAX_FILE ?SYNTAX_SUFFIX),
        ParserTime = file_mtime(?SYNTAX_FILE ?PARSER_SUFFIX),
        ScannerTime = file_mtime(?SYNTAX_FILE ?SCANNER_SUFFIX),
        BuilderTime = module_time(build_parser),
        if
            InTime =:= 0 ->
                Force = false,
                throw(no_syntax_file);
            InTime > ParserTime; InTime > ScannerTime;
            BuilderTime > ParserTime; BuilderTime > ScannerTime ->
                Force = true,
                build_parser:build(?SYNTAX_FILE ?SYNTAX_SUFFIX,
                                   ?SYNTAX_FILE ?SCANNER_SUFFIX,
                                   ?SYNTAX_FILE ?PARSER_SUFFIX,
                                   filename:join(["..",
                                                  "include",
                                                  ?SYNTAX_FILE ".hrl"]));
            true ->
                Force = false
        end,
        ScOutTime = file_mtime(?SYNTAX_FILE ?SCANNER_OUTPUT),
        PrOutTime = file_mtime(?SYNTAX_FILE ?PARSER_OUTPUT),
        if
            Force; ScannerTime > ScOutTime ->
                case leex:file(?SYNTAX_FILE ?SCANNER_SUFFIX) of
                    error -> throw(leex_error);
                    _ -> ok
                end;
            true ->
                ok
        end,
        if
            Force; ParserTime > PrOutTime ->
                case yecc:file(?SYNTAX_FILE ?PARSER_SUFFIX) of
                    error -> throw(yecc_error);
                    _ -> ok
                end;
            true ->
                ok
        end
    after
        file:set_cwd(Cwd)
    end.


build_release(BaseDir) ->
    ok = application:load(refactorerl),
    ok = application:load(clustering),
    {ok, RApps} = application:get_key(refactorerl, applications),
    {ok, CApps} = application:get_key(clustering, applications),
    {ok, Vsn} = application:get_key(refactorerl, vsn),
    Rel = {release,
           {"refactorerl", Vsn},
           {erts, erlang:system_info(version)},
           lists:map(
             fun(App) ->
                     application:load(App),
                     {ok, V} = application:get_key(App, vsn),
                     {App, V}
             end,
             ordsets:from_list(RApps ++ CApps ++ [refactorerl, clustering]))},
    ok = file:set_cwd(BaseDir),
    case file:open("refactorerl.rel", [write]) of
        {ok, Dev} ->
            io:format(Dev, "~p.~n", [Rel]),
            file:close(Dev);
        {error, Reason} ->
            throw({open_error, file:format_error(Reason)})
    end,
    systools:make_script("refactorerl").


file_mtime(File) ->
    case filelib:last_modified(File) of
        0 -> 0;
        T -> calendar:datetime_to_gregorian_seconds(
               hd(calendar:local_time_to_universal_time_dst(T)))
    end.

module_time(Mod) ->
    {Y,M,D,H,N,S} = proplists:get_value(time, Mod:module_info(compile)),
    calendar:datetime_to_gregorian_seconds({{Y,M,D},{H,N,S}}).
    
