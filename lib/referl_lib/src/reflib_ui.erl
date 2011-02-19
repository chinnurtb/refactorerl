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

%%% @doc This module provides a bridge between user interfaces and system
%%% functionality. A user interface requests operations and receives results
%%% through this module. Requests are asychronously handled, the results are
%%% returned through a `gen_event' event manager. Events can occur
%%% independently of operations, a user interface probably wants to handle
%%% these as well.
%%%
%%% Operations requests are sent using the exported functions of this module.
%%% See the description of the functions below.
%%%
%%% == Message types ==
%%%
%%% To receive event messages, a standard `gen_event' callback module must be
%%% registered by calling {@link add_msg_handler/2}. The following event
%%% messages may be sent to the callback modules:
%%%
%%% <dl>
%%%
%%% <dt>{@type {status, Message::string()@}}</dt> <dd>A textual message that
%%% informs the user of the tool's status.</dd>
%%%
%%% <dt>{@type {error, Message::string()@}}</dt> <dd>A textual error message
%%% that describes an internal error situation of the tool.</dd>
%%%
%%% <dt>{@type {add, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are added (or maybe re-added) to the database.</dd>
%%%
%%% <dt>{@type {drop, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are removed from the database.</dd>
%%%
%%% <dt>{@type {invalid, Path::path()@}}</dt> <dd>The file specified by `Path'
%%% is added to the database, but it is invalid (contains errors).</dd>
%%%
%%% <dt>{@type {reload, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are modified by the tool (and should be reloaded from disk
%%% by the UI).</dd>
%%%
%%% <dt>{@type {rename, {From::path(), To::path()@}@}}</dt> <dd>The file
%%% `From' is changed to have the name `To' (and this changes should be
%%% followed by the UI).</dd>
%%%
%%% <dt>{@type {showconfig, Config::[{Key::atom(), Value@}]@}}</dt> <dd>This
%%% message describes the currect runtime configuration of the tool. Currently
%%% used key values are `appbase', `include', and `output'. See also {@link
%%% saveconfig/3}.</dd>
%%%
%%% <dt>{@type {filelist, Files::[{Path::path(), Error::atom()@}]@}}</dt>
%%% <dd>This message contains the files that are currently stored in the
%%% database, and whether they haver errors or not.</dd>
%%%
%%% <dt>{@type {filepos,
%%% FilePos::[{Path::path(),SatartPos::{integer(), integer()@},
%%% EndPos::{integer(), integer()@}@}]@}}</dt> <dd>This message
%%% contains a list of starting and ending position and their containing
%%% file</dd>
%%%
%%% <dt>{@type {funlist, Functions::[{Name::atom(),
%%% Arity::integer()@}]@}}</dt> <dd>This message contains a list of functions.
%%% <small>TODO: This is rather specific to the move function refactoring, and
%%% should be improved.</small></dd>
%%%
%%% <dt>{@type {recordlist, Records::[Name::atom()]@}}</dt> <dd>This message
%%% contains a list of records. <small>TODO: this is rather specific to the
%%% move record refactoring, and should be improved.</small></dd>
%%%
%%% <dt>{@type {macrolist, Macros::[Name::atom()|string()]@}}</dt> <dd>This
%%% message contains a list of macros. <small>TODO: this is rather specific
%%% to the move macro refactoring, and should be improved.</small></dd>
%%%
%%% <dt>{@type {question, {Id::integer(), Details::proplist()@}@}}</dt>
%%% <dd>Contains a question that must be answered by {@link reply/2} or
%%% cancelled by {@link cancel/1}. <small>TODO: `Details' are to be
%%% specified.</small></dd>
%%%
%%% <dt>{@type {uifinished, Operation::atom()@}}</dt> <dd>The spawning of the
%%% specified operation is complete on the UI side.</dd>
%%%
%%% <dt>{@type {trfinished, ok@}}</dt> <dd>An operation is finished
%%% on the transform side.</dd>
%%%
%%% <dt>{@type {progress, {add|drop, Path::path(), Count::integer(),
%%% Max::integer()@}@}}</dt> <dd>Progress report of a file (re)loading or
%%% dropping operation. `Max' is the maximal number of steps, `Count' is the
%%% number of currently finished steps.</dd>
%%%
%%% </dl>
%%%
%%% ------------
%%% 2010.01.18 bkil
%%% status: I am constantly verifying if it compiles and passes
%%%  dialyzer (all via flymake), but never run it
%%% @todo actualize comments
%%% @todo finish filelist
%%% @todo transform
%%%
%%% A quick overview of the new message format.
%%% Unicast:
%%%  {ReqID, progress, todo()} |
%%%  {ReqID, reply, {error,Reason} | {ok,any()}}
%%%  {ReqID, question, {NewID,todo()}}
%%%  {ReqID, answer, todo()}
%%% Broadcast:
%%%  {B,statusinfo,StatusData}
%%%   where StatusData=
%%%    {shutdown,Reason} |
%%%    {reset,Reason} |
%%%    {change,FileChange}
%%%     where FileChange = [{Filename,[{rename,New}    |
%%%                                    {content,true}  |
%%%                                    {present,true|false}|
%%%                                    {error,Errors}  |
%%%                                    {lastmod,todo()}|
%%%                                    {type,todo()} ]}]
%%%      where Errors = [todo()]
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reflib_ui).
-vsn("$Rev: 1479$ ").

%%% ----------------------------------------------------------------------------
%%% Client exports

%%% Database control
-export([stop/1, reset/1, add/2, drop/2, undo/2, saveconfig/4]).
-export([load_beam/4, add_dir/2, drop_dir/2]).

%%% Status queries
-export([status/2, showconfig/1, filelist/1, status_info/2]).

%%% Semantic queries
-export([draw/3]).

%%% Internal queries
-export([funlist/2, recordlist/2, macrolist/2]).

%%% Refactoring
-export([transform/3, reply/3, cancel/2]).

%%% Clustering
-export([cl_options/2, run_cl/4, cl_refresh/1]).

%% Callbacks
-export([error_text/2]).

-include("lib.hrl").
-include_lib("kernel/include/file.hrl").


%%% ----------------------------------------------------------------------------
%%% UI protocol description

% each function invocation must return with exactly one of the following:
-define(NoReply,   noreply).
-define(Dat(X),    {ok,X}).
-define(OK,        ?Dat([])).
-define(ERR(R),    {error,R}).
-define(LErr(R),   ?LocalErr(R,[])).
-define(LocalErr(R,L), ?ERR(?LocalError(R,L))).
-define(RefErr(R,L),   ?ERR(?RefError(R,L))).

% the following can be sent anytime:
send_progress(MCB, Op, File, Count, Max) ->
    (MCB#msg_cb.unicast)(progress,{Op, File, Count, Max}).

send_change(MCB,Change) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{change,Change}]).

send_shutdown(MCB) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{shutdown,"manual shutdown initiated"}]).

send_reset(MCB) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{reset,"manual restart initiated"}]).

% possible formats of changes
-define(PresentCorrect(File),
        {File, [{present,true}]}).
-define(PresentCorrectTM(File,Type,LastMod),
        {File, [{present,true},
                {type, Type}, {lastmod, LastMod}]}).
-define(PresentError(File,Errors),
        {File, [{error,Errors}]}). %,{present,true}
-define(PresentErrorTM(File,Type,LastMod,Errors),
        {File, [{present,true}, {error,Errors},
                {type, Type}, {lastmod, LastMod}]}).
-define(NotPresent(File),
        {File, [{present,false}]}).
-define(Modified(File),
        {File, [{content,true}]}).
-define(Renamed(OldPath,NewPath),
        {OldPath, [{rename,NewPath}]}).
-define(AddedCorrect(File),     ?PresentCorrect(File)).
-define(AddedError(File,Error), ?PresentError(File,Error)).
-define(Dropped(File),          ?NotPresent(File)).


%%% ----------------------------------------------------------------------------
%%% types

%%% @type path() = string(). The absolute path of a file.


%%% ============================================================================
%%% Error texts

error_text(wrangler_dir, []) ->
    ["Please set Wrangler's ",
     "installation directory, and restart RefactorErl"];
error_text(cl_ui_refresh, []) ->
    ["cl_ui refresh: something wrong"];
error_text(no_backup, []) ->
    ["Need a backup to do it"];
error_text(undef, []) ->
    ["undefined function"];
error_text(noresult, []) ->
    ["no result available"];
error_text(trfail, []) ->
    ["initiation of the transformation failed"];
error_text(no_dups, []) ->
    ["Wrangler did not find any ",
     "duplicated code fragments"];
error_text(not_found, [Path])->
    ["No file could be handled from \"", Path, "\""];
error_text(somebad, [])->
    ["Processing failed for some files, check the errors"].

%@todo refactor to ?LocalError
%error_message({no_include_file, File}) ->
%    ?MISC:format("Include file \"~s\" not found", [File]);
%error_message({some_proc, []})->
%    ?MISC:format("Not all files were processed", []);
%error_message({none_proc, []})->
%    ?MISC:format("No files were processed", []);
%error_message(Error) ->
%    ?MISC:format("Error: ~p", [Error]).



%%% ============================================================================
%%% Standard UI exports


%% @spec stop(#msg_cb{}) -> ok
%% @doc Stops the RefactorErl server.
stop(MCB) ->
    send_shutdown(MCB),
    init:stop(),
    ?OK.
% "RefactorErl server is shutting down..."

%% @spec status(#msg_cb{}, path()) -> ok
%% @doc Requests information about the status of `File'. The result is a
%% message of type `add', `invalid', or `drop'.
status(_MCB, FileName) when is_list(FileName) ->
    case ?Query:exec(?File:find(FileName)) of
        [File] ->
            case ?Query:exec(File, ?File:error_forms()) of
                [] -> ?Dat([?PresentCorrect(FileName)]);
                E  -> ?Dat([?PresentError(FileName,
                                          decode_error_form(File,E))])
            end;
        []  -> ?Dat([?NotPresent(FileName)])
    end.


%% @spec add(#msg_cb{}, path()) -> ok
%% @doc Adds `File' to the database.
%% @todo maybe introduce a `reload' or `update' message
add(MCB, FileName) when is_list(FileName) ->
    do_add_file(MCB,FileName).

%% @spec drop(#msg_cb{}, path()) -> ok
%% @doc Drops `File' from the database.
drop(MCB, FileName) when is_list(FileName) ->
    do_drop_file(MCB,FileName).

%% @spec add_dir(#msg_cb{}, path()) -> ok
%% @doc Adds recursively to the database starting from `File'.
add_dir(MCB,FileName) when is_list(FileName) ->
    add_flat(recurse_erl(MCB,FileName, fun add_filedir/2)).

%% @spec drop_dir(#msg_cb{}, path()) -> ok
%% @doc Drops recursively to the database starting from `File'.
drop_dir(MCB,FileName) when is_list(FileName) ->
    add_flat(recurse_erl(MCB,FileName, fun drop_filedir/2)).

%% @spec load_beam(#msg_cb{}, path(), path(), boolean()) -> ok
%% @doc Loads a BEAM file compiled with `debug_info' and saves the result
load_beam(MCB,FileName,TargetDir,ToSave)
  when is_list(FileName), is_list(TargetDir), is_boolean(ToSave) ->
    case refcore_loadbeam:start(FileName,TargetDir,ToSave) of
        {error, Reason} ->
            send_change(MCB,[?AddedError(FileName,[])]), %@todo
            ?LErr(Reason);
        {ok, File} ->
            Add = [?File:path(FN) || FN <- ?Query:exec(File, ?File:includes())],
            send_change(MCB,[?AddedCorrect(F) || F <- Add]),
            ?OK
    end.

%% @spec draw(#msg_cb{}, path(), integer()) -> ok
%% @doc Creates a `.dot' drawing of the graph in the database, and saves it in
%% `File'. The contents of the graph are filtered according to `Filter'; the
%% value `1' means no filtering, then different numbers select different
%% filters.
draw(_MCB, File, Type) ->
    Filter = convert_filter(Type),
    ok = ?DRAW_GRAPH:draw_graph(File, Filter),
    ?OK.

%% @spec showconfig(#msg_cb{}) -> ok
%% @doc Requests configuration information. The result is sent in a message of
%% type `showconfig'.
showconfig(_MCB) ->
    ?Dat([{Name, Value} ||
              Env <- ?Query:exec([env]),
              #env{name=Name, value=Value} <- [?Graph:data(Env)]]).

%% @spec saveconfig(#msg_cb{}, [path()], [path()], path() | original) -> ok
%% @doc Modifies the runtime configuration of the tool.
%% <ul>
%% <li>`App' is a list of directories that are searched for applications for
%%   `include_lib' directives</li>
%% <li>`Inc' is a list of directories that are searched for `include'
%%   directives</li>
%% <li>`Out' is the output directory of the tool. Its value can be `original',
%%   which means files should be overwritten, or a name of a directory, which
%%   means modified files should be saved there.</li>
%% </ul>
%% @todo If this option is still necessary, it should use ?Graph:save_envs/0.
saveconfig(_MCB, AppDirs, IncDirs, OutDir) ->
    ?Syn:del_envs(),
    [?Syn:create_env(appbase, Dir) || Dir <- AppDirs],
    [?Syn:create_env(include, Dir) || Dir <- IncDirs],
    ?Syn:create_env(output, OutDir),
    ?OK.
% "Configuration saved."

%% @spec filelist(#msg_cb{}) -> ok
%% @doc Requests a list of the files in the database. The result is sent in a
%% message of type `filelist'.
filelist(_MCB) ->
    Files = ?Query:exec([file]), %@todo
    FileStats =
        [ case ?Query:exec(FileNode, ?File:error_forms()) of
              [] -> ?PresentCorrect(FilePath);
              E  -> ?PresentError(FilePath,decode_error_form(FileNode,E))
          end || FileNode <- Files, FilePath <- [?File:path(FileNode)]],
    ?Dat(FileStats).

%% @spec reset(#msg_cb{}) -> ok
%% @doc Resets the database.
reset(MCB) ->
    send_reset(MCB),
    ?Graph:reset_schema(),
    ?OK.
% "Database clear started."

%%% ============================================================================
%%% Transformation interface

-define(SQ,refusr_sq).
-define(MQ,refusr_metrics).
-define(CL,refcl_main).

transform(MCB, metric_query, Args) ->
    Run = fun()->?Transform:do(MCB, ?MQ, Args) end,
    transform_(MCB,Run);

transform(MCB, semantic_query, Args) ->
    Run = fun()->?Transform:do(MCB, ?SQ, Args) end,
    transform_(MCB,Run);

transform(MCB, clustering, Args) ->
    Run = fun()->?Transform:do(MCB, ?CL, Args) end,
    transform_(MCB,Run);

%% @spec transform(#msg_cb{}, atom(), proplist()) -> ok
%% @doc Initiates a transformation and waits for an initiated transform to end.
%% Returns the result of the said transformation.
%% `Refac' is a transformation module name without the common prefix,
%% `Args'  is a proplist that contains the arguments of the transformation.
%% @see reflib_transform
%% @see reflib_args
transform(MCB, Refac, Args) ->
    Mod = list_to_atom("reftr_"++atom_to_list(Refac)),
    ?Graph:backup(),
    Run = fun()->?Transform:do(MCB, Mod, Args) end,
    transform_(MCB,Run).

transform_(_MCB, Run) when is_function(Run,0) ->
    case Run() of
        ok ->
            Result = ?Transform:wait(),
            case Result of
                none ->
                    ?Dat(Result);
                {result,_} ->
                    ?Dat(Result);
                {abort,_}->
                    ?Dat(Result);
                {error,E} ->
                    {error,E}
            end;
        _ ->
            ?LErr(trfail)
    end.

%% @spec reply(#msg_cb{}, integer(), term()) -> ok
%% @doc Provides a reply to a previously asked question.
%% @see reflib_transform:reply/2
reply(_MCB, Id, Reply) ->
    noreply = ?Transform:reply(Id, Reply),
    ?NoReply.

%% @spec cancel(#msg_cb{}, integer()) -> ok
%% @doc Cancels a previously asked question.
%% @see reflib_transform:cancel/1
cancel(_MCB, Id) ->
    noreply = ?Transform:cancel(Id),
    ?NoReply.


%%% ----------------------------------------------------------------------------
%%% Movings

%% @spec funlist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of functions defined in `File'. The result is
%% returned in a message of type `funlist'.
%% @todo error handling?!
funlist(_MCB, File) ->
    ?Dat([{?Fun:name(F), ?Fun:arity(F)} ||
             F <- ?Query:exec(
                     ?Query:seq([?File:find(File),
                                 ?File:module(),
                                 ?Mod:locals()]))]).

%% @spec recordlist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of records defined in `File'. The result is
%% returned in a message of type `recordlist'.
recordlist(_MCB, File) ->
    ?Dat([?Rec:name(R) ||
             R <- ?Query:exec(
                     ?Query:seq(?File:find(File), ?File:records()))]).

%% @spec macrolist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of macros defined in `File'. The result is
%% returned in a message of type `macrolist'.
macrolist(_MCB, File) ->
    ?Dat([?Macro:name(M) ||
             M <- ?Query:exec(
                     ?Query:seq(?File:find(File), ?File:macros()))]).

%%% ----------------------------------------------------------------------------
%%% Clustering

%% @spec cl_options(#msg_cb{}, atom()) -> ok
%% @doc Requests the options of the given clustering algorithm.
cl_options(_MCB, Alg) ->
    OptList = cl_ui:cl_options(Alg),
    Ls = [[A,B] || {A,B} <- OptList],
    ?Dat([Alg]++Ls).

%% @spec cl_refresh(#msg_cb{}) -> ok
%% @doc Refreshes the Emacs clustering interface.
cl_refresh(_MCB) ->
    case cl_ui:refresh() of
        {cl_ui, recreated} ->
            ?OK;
        _ ->
            ?LErr(cl_ui_refresh) %@todo
    end.
% "cl_ui refresh: cleaned"

%% @spec run_cl(#msg_cb{}, proplist(), atom(), atom()) -> ok
%% @doc Invokes {@link cl_ui:run/1}. See the options there.
run_cl(_MCB, Opt, Alg, Create) ->
    {Clustering, FittNum} = cl_ui:run({Opt, Alg, Create}),
    ?Dat([{result,Clustering},
          {flist,?MISC:format("~w",[FittNum])}]).
% "Clustering algorithm finished."

%%% ============================================================================
%%% Backup

%% @spec undo(#msg_cb{}, string()) -> ok
%% @doc Steps backward on the Refactorerl database.
undo(MCB, _RFile) ->
    %% (_RFile) This parameter is need to handle multiple backup checkpoints.
    FilesBefore = files_with_lastmod(),
    try ?Graph:undo() of
        ok  ->
            FilesAfter = files_with_lastmod(),
            ModifiedFiles =
                [ begin
                      ok = ?FileMan:save_file(File),
                      ?File:path(File) end ||
                    {File, LastMod} <- FilesAfter,
                    lists:keysearch(File, 1, FilesBefore) =/= LastMod ],

            send_change(MCB,[?Modified(F) || F <- ModifiedFiles]),

            ?Graph:clean(),
            ?OK;
        _ ->
            ?LErr(no_backup)
    catch
        throw:Msg ->
            ?LErr(Msg)
    end.

%%% ----------------------------------------------------------------------------
%%% Attributes of the error form

%% @spec decode_error_form(#file{}, ErrList) -> ok
%% @doc This function can collect information about error forms.
decode_error_form(FileNode,ErrList) ->
      [begin
         Token = paarse((?Graph:data(Form))#form.tag),
         {Position, Text} =
             case Token of
                 [] ->
                     N = ?File:length(FileNode),
                     {{N,N}, "EOF"};
                 _ -> {?Token:pos(Token),
                       ?Token:text(Token)}
             end,
         [{nexttokentext, Text}, {position, Position}]
       end || Form <- ErrList].

%%% ----------------------------------------------------------------------------
%%% Handling file status information

%% @spec status_info(#msg_cb{}, [string()]) -> ok
%%
%% @doc This function collets information about files loaded in the
%%      RefactorErl database.
%%      Parameter of the function: `[]' means that the function find all
%%      files form the database.
%%      When the parameter is a list of the file paths or it is one file path
%%      in a list, the function is collecting information about these files
%%      only.
%%      The message of the function is a `FileStatusList' that is a
%%      [proplist()] contains information about the file(s).
%%
%%      Elements of the `FileStatusList':
%%
%%      {file,string()}          The path and the name of the file
%%      {error,Errors}           The file contains errors: list @todo
%%      {type,atom()}            The type of the file:
%%                               `none'|`module'|`include'
%%      {lastmod,int()|atom()}   The last modification of th file. The
%%                               default value is `undefined'
%%      {present,boolean()}      Status of the file:  @todo
status_info(_MCB, FileList) ->
    ?Dat(file_status_info(FileList)).

%%% ============================================================================
%%% Private implementation

%% @doc Parser for the error messages
paarse({_,Mesg})->
    case
        re:run(Mesg, "{{.+},{.+}}", [{capture, first}]) of
        {match, [{F, L}]} ->
            SToken = string:substr(Mesg, F+1, L),
            {ok, STerm, _}= erl_scan:string(SToken++"."),
            {ok, Tken} = erl_parse:parse_term(STerm),
            {_,Token} = Tken, Token;
        _ -> []
    end;
paarse(_) ->
    [].

%%% ----------------------------------------------------------------------------

add_filedir(MCB,File) ->
    case ?MISC:is_erl(File) of
        true  ->
            do_add_file(MCB,File);
        false ->
            true = ?MISC:is_beam(File),
            do_load_beam(MCB,File)
    end.

drop_filedir(MCB,File)->
    do_drop_file(MCB,File).

do_add_file(MCB,FileName)->
    case ?FileMan:add_file(FileName,
                           [update, {progress, progress(MCB,add)}]) of
        {error, Reason} ->
            send_change(MCB,[?AddedError(FileName,[])]), %@todo
            ?LErr(Reason);
        {file, File} ->
            Nodes = ?Query:exec(File, ?File:includes()),
            Paths = [?File:path(FN) || FN <- Nodes],
            send_change(MCB,[?AddedCorrect(F) || F <- Paths]),
            ?Dat(Nodes)
    end.

do_drop_file(MCB,FileName)->
    case ?Query:exec(?File:find(FileName)) of
        [File] ->
            Drop =
                [?File:path(FN) || FN <- ?Query:exec(File, ?File:included())],
            ?FileMan:drop_file(File, [{progress, progress(MCB,drop)}]),
            send_change(MCB,[?Dropped(F) || F <- Drop]),
            ?OK;
        [] ->
            ?RefErr(file_not_present,[FileName])
    end.

%% @doc Traverse a complete directory recursively while doing the action
%% specified on all "*.erl" and "*.beam" files each folder contains.
recurse_erl(MCB,Start=[_|_], Action) when is_function(Action,2) ->
    Files = erl_beam(MCB,Start),
    Result = [Action(MCB,F) || F <- Files].
% @todo
    %% case Result /= [] of
    %%     true ->
    %%         case lists:any(fun(X)->X end, Result) of
    %%             false ->
    %%                 message(status, "~s", [error_message({none_proc,[]})]);
    %%             true ->
    %%                 case lists:any(fun(X)->not X end, Result) of
    %%                     true ->
    %%                         message(status, "~s",
    %%                                 [error_message({some_proc,[]})]);
    %%                     false ->
    %%                         ok
    %%                 end
    %%         end;
    %%     false ->
    %%         ok
    %% end.

%% @doc Traverse a complete directory recursively to collect
%% all "*.erl" and "*.beam" files each folder contains.
erl_beam(_MCB,Start) ->
    Files = ?MISC:find_files(
           filename:absname(Start),
           ?MISC:'or'(fun ?MISC:is_erl/1, fun ?MISC:is_beam/1)),
    case Files of
    [] ->
        throw(?LocalError(not_found,[Start]));
    _ ->
        FileMod = [{filename:rootname(filename:basename(F)), F}
               || F <- Files],
        {Erl,Beam0} = lists:partition(
                fun({_M,F})-> ?MISC:is_erl(F) end, FileMod),
        {ErlMod,_ErlFile} = lists:unzip(Erl),
        Beam = ?MISC:pdel(ErlMod, Beam0),
        All = Erl ++ Beam,
        [F || {_,F} <- All]
    end.

do_load_beam(MCB,File) ->
    load_beam(MCB,File,filename:dirname(File),false).

add_flat(Results0)->
    Results = lists:flatten(Results0),
    case lists:all(fun({ok,_})->true; % ?OK, ?Dat
                      (_)->false end,
                   Results) of
        true->
            Unpack = [N || {ok,N} <- Results], % ?OK, ?Dat
            ?Dat(lists:flatten(Unpack));
        false->
            ?LErr(somebad)
    end.

%%% ----------------------------------------------------------------------------

progress(MCB,Op) ->
    fun
        (File, Count, Max) ->
            send_progress(MCB, Op, File, Count, Max)
    end.

%% @doc Filters can be given either by their atomic abbreviations
%%      or by numeric codes.
convert_filter(Type) when is_atom(Type) -> Type;
convert_filter(2) -> sem;
convert_filter(3) -> ctx;
convert_filter(4) -> syn;
convert_filter(5) -> synlex;
convert_filter(6) -> lex;
convert_filter(7) -> inp;
convert_filter(8) -> not_lex;
convert_filter(_) -> all.


files_with_lastmod() ->
    [{File, LastMod}
     || File <- ?Query:exec([file]),
        LastMod <- [(?ESG:data(File))#file.lastmod]].

file_status_info([])->
    Files = ?Query:exec([file]),
    [file_stat(FileName, FileNode) ||
        FileNode <- Files, FileName <- [?File:path(FileNode)]];

file_status_info(FileList)-> %@todo
    lists:map(
      fun(FileName) ->
              case ?Query:exec(?File:find(FileName)) of
                  [] ->
                      ?NotPresent(FileName);
                  [FileNode] ->
                      file_stat(FileName,FileNode)
              end
      end,
      FileList).

file_stat(Path,FileNode)->
    Type = ?File:type(FileNode),
    LastMod = (?Graph:data(FileNode))#file.lastmod,
    case ?Query:exec(FileNode, [{form, {type, '==', error}}]) of
        [] -> ?PresentCorrectTM(Path, Type, LastMod);
        E  -> ?PresentErrorTM(Path, Type, LastMod,
                              decode_error_form(FileNode,E))
    end.

