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
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reflib_ui).
-vsn("$Rev: 1479$ ").
-behaviour(gen_server).

%%% ----------------------------------------------------------------------------
%%% Client exports

%%% Database control
-export([stop/0, reset/0, add/1, drop/1, undo/1, saveconfig/3]).
-export([load_beam/3]).

%%% Status queries
-export([status/1, showconfig/0, filelist/0, status_info/1, error_attr/0]).

%%% Semantic queries
-export([draw/2]).

%%% Internal queries
-export([funlist/1, recordlist/1, macrolist/1]).

%%% Refactoring
-export([transform/2, reply/2, cancel/1]).

%%% Clustering
-export([cl_options/1, run_cl/3, cl_refresh/0]).

%%% UI message handlers
-export([message/2, message/3, add_msg_handler/2, add_msg_handler/3,
         del_msg_handler/2]).


%%% ----------------------------------------------------------------------------
%%% Server exports
-export([start_link/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


-include("lib.hrl").
-include_lib("kernel/include/file.hrl").

%%% @type path() = string(). The absolute path of a file.

%% @spec stop() -> ok
%% @doc Stops the RefactorErl server.
stop() -> cast({stop}).

%% @spec status(path()) -> ok
%% @doc Requests information about the status of `File'. The result is a
%% message of type `add', `invalid', or `drop'.
status(File) -> cast({status, File}).

%% @spec status_info([string()]) -> ok
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
%%      {errors,atom()}          The file contains errors: `yes'|`no'
%%      {type,atom()}            The type of the file:
%%                               `none'|`module'|`include'
%%      {lastmod,int()|atom()}   The last modification of th file. The
%%                               default value is `undefined'
%%      {status,atom()}          Status of the file: `loaded' or `missing'
status_info(Files) -> cast({status_info, Files}).

%% @spec add(path()) -> ok
%% @doc Adds `File' to the database.
add(File) -> cast({add, File}).

%% @spec load_beam(path(), path(), boolean()) -> ok
%% @doc Loads a BEAM file compiled with `debug_info' and saves the result
load_beam(File,TargetDir,ToSave) -> cast({load_beam, File, TargetDir, ToSave}).

%% @spec drop(path()) -> ok
%% @doc Drops `File' from the database.
drop(File) -> cast({drop, File}).

%% @spec draw(path(), integer()) -> ok
%% @doc Creates a `.dot' drawing of the graph in the database, and saves it in
%% `File'. The contents of the graph are filtered according to `Filter'; the
%% value `1' means no filtering, then different numbers select different
%% filters.
draw(File, Filter) -> cast({draw, File, Filter}).

%% @spec showconfig() -> ok
%% @doc Requests configuration information. The result is sent in a message of
%% type `showconfig'.
showconfig() -> cast({showconfig}).

%% @spec saveconfig([path()], [path()], path() | original) -> ok
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
saveconfig(App, Inc, Out) -> cast({saveconfig, App, Inc, Out}).

%% @spec filelist() -> ok
%% @doc Requests a list of the files in the database. The result is sent in a
%% message of type `filelist'.
filelist() -> cast({filelist}).

%% @spec reset() -> ok
%% @doc Resets the database.
reset() -> cast({reset}).

%% @spec transform(atom(), proplist()) -> ok
%% @doc Initiates a transformation. `Mod' is a transformation module name,
%% `Args' is a proplist that contains the arguments of the transformation.
%% @see reflib_transform
%% @see reflib_args
transform(Mod, Args) -> cast({transform, Mod, Args}).

%% @spec reply(integer(), term()) -> ok
%% @doc Provides a reply to a previously asked question.
%% @see reflib_transform:reply/2
reply(Id, Reply) -> cast({reply, Id, Reply}).

%% @spec cancel(integer()) -> ok
%% @doc Cancels a previously asked question.
%% @see reflib_transform:cancel/1
cancel(Id) -> cast({cancel, Id}).

%% @spec funlist(path()) -> ok
%% @doc Requests the list of functions defined in `File'. The result is
%% returned in a message of type `funlist'.
funlist(File) -> cast({funlist, File}).

%% @spec recordlist(path()) -> ok
%% @doc Requests the list of records defined in `File'. The result is
%% returned in a message of type `recordlist'.
recordlist(File) -> cast({recordlist, File}).

%% @spec macrolist(path()) -> ok
%% @doc Requests the list of records defined in `File'. The result is
%% returned in a message of type `recordlist'.
macrolist(File) -> cast({macrolist, File}).

%% @spec run_cl(proplist(), atom(), atom()) -> ok
%% @doc Invokes {@link cl_ui:run/1}. See the options there.
run_cl(Opt, Alg, Create) -> cast({run_cl, Opt, Alg, Create}).

%% @spec cl_options(atom()) -> ok
%% @doc Requests the options of the given clustering algorithm.
cl_options(Alg) -> cast({cl_options, Alg}).

%% @spec cl_refresh() -> ok
%% @doc Refreshes the Emacs clustering interface.
cl_refresh() -> cast({cl_refresh}).

%% @spec error_attr() -> ok
%% @doc This function can collect information about error forms.
error_attr() -> cast({error_attr}).

%% @spec undo(string()) -> ok
%% @doc Steps backward on the Refactorerl database.
undo(RFile) -> cast({undo, RFile}).


cast(Data) -> gen_server:cast({?MODULE, ?REFERL_NODE}, Data).

%% @spec message(atom(), [term()]) -> ok
%% @doc Sends a message of `Type' which contains an arbitrary list of terms
%% to the registered user interface message handlers.
message(Type, Data) ->
    gen_event:notify({?UI_MSG_SERVER, ?REFERL_NODE}, {Type, Data}).

%% @spec message(atom(), string(), [term()]) -> ok
%% @doc Sends a message of `Type' with arguments formatted by `io_lib:format'
%% to the registered user interface message handlers.
message(Type, Format, Args) ->
    gen_event:notify({?UI_MSG_SERVER, ?REFERL_NODE},
                     {Type, ?MISC:format(Format, Args)}).

%% @spec add_msg_handler(atom(), term()) -> ok | term()
%% @doc Registers a new user interface message handler (which must be a
%% `gen_event' callback module).
add_msg_handler(Handler, Args) ->
    gen_event:add_sup_handler({?UI_MSG_SERVER, ?REFERL_NODE}, Handler, Args).

%% @spec add_msg_handler(atom(), term(), atom()) -> ok | term()
%% @doc Registers a new user interface message handler (which must be a
%% `gen_event' callback module).
add_msg_handler(Handler, Args, nosup) ->
    gen_event:add_handler({?UI_MSG_SERVER, ?REFERL_NODE}, Handler, Args).

%% @spec del_msg_handler(atom(), term()) -> ok | term()
%% @doc Deletes a registered user interface message handler.
del_msg_handler(Handler, Args) ->
    gen_event:delete_handler({?UI_MSG_SERVER, ?REFERL_NODE}, Handler, Args).


%% @doc Starts the server and links it with the calling process.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init(_) ->
    {ok, ok}.

%% @private
handle_cast(Cmd, S) when is_tuple(Cmd)->
    try
        handle(Cmd)
    catch
        error:Err ->
            error_logger:error_msg("Bad request ~p~n"
                                   "  Error: ~p~n"
                                   "  Stack: ~p~n",
                                   [Cmd, Err, erlang:get_stacktrace()]),
            message(error, "error handling request ~512P", [Cmd,4])
    end,
    message(uifinished, element(1,Cmd)),
    {noreply, S}.

%% @private
handle_call(_,_,S) ->
    {stop, bad_call, S}.

%% @private
handle_info(_, S) ->
    {noreply, S}.

%% @private
terminate(_,_) ->
    ok.

%% @private
code_change(_,S,_) ->
    {ok, S}.

%%% ============================================================================
%%% Standard UI

handle({stop}) ->
    message(status, "RefactorErl server is shutting down...",[]),
    init:stop();

handle({status, FileName}) when is_list(FileName) ->
    case ?Query:exec(?File:find(FileName)) of
        [File] ->
            case ?Query:exec(File, [{form, {type,'==',error}}]) of
                [] -> message(add, [FileName]);
                _  -> message(invalid, "~s", [FileName])
            end;
        []  -> message(drop, [FileName])
    end;

%% todo: maybe introduce a `reload' or `update' message
handle({add, FileName}) when is_list(FileName) ->
    case ?FileMan:add_file(FileName, [update, {progress, progress(add)}]) of
        {error, Reason} ->
            message(invalid, "~s", [FileName]),
            message(status, "~s", [error_message(Reason)]);
        {file, File} ->
            Add = [?File:path(FN) || FN <- ?Query:exec(File, ?File:includes())],
            message(add, Add)
    end;

handle({load_beam, FileName, TargetDir, ToSave})
  when is_list(FileName), is_list(TargetDir), is_boolean(ToSave) ->
    case refcore_loadbeam:start(FileName,TargetDir,ToSave) of
        {error, Reason} ->
            message(invalid, "~s", [FileName]),
            message(status, "~s", [error_message(Reason)]);
        {ok, File} ->
            Add = [?File:path(FN) || FN <- ?Query:exec(File, ?File:includes())],
            message(add, Add)
    end;

handle({drop, FileName}) when is_list(FileName) ->
    case ?Query:exec(?File:find(FileName)) of
        [File] ->
            Drop =
                [?File:path(FN) || FN <- ?Query:exec(File, ?File:included())],
            ?FileMan:drop_file(File, [{progress, progress(drop)}]),
            message(drop, Drop);
        [] ->
            message(status, "Already dropped", [])
    end;

handle({draw, File, Type}) ->
    Filter = convert_filter(Type),
    ?DRAW_GRAPH:draw_graph(File, Filter),
    message(status, "Ok", []);

handle({showconfig}) ->
    message(showconfig,
            [{Name, Value} ||
                Env <- ?Query:exec([env]),
                #env{name=Name, value=Value} <- [?Graph:data(Env)]]);

handle({saveconfig, AppDirs, IncDirs, OutDir}) ->
    [?Graph:delete(Env) || Env <- ?Graph:path(?Graph:root(), [env])],
    [?Graph:mklink(?Graph:root(), env,
                   ?Graph:create(#env{name=appbase, value=Dir})) ||
                      Dir <- AppDirs],
    [?Graph:mklink(?Graph:root(), env,
                   ?Graph:create(#env{name=include, value=Dir})) ||
                      Dir <- IncDirs],
    ?Graph:mklink(?Graph:root(), env,
                  ?Graph:create(#env{name=output, value=OutDir})),
    message(status, "Configuration saved.", []);

handle({filelist}) ->
    Files = ?Query:exec([file]),

    Err = fun(File) ->
                  case ?Query:exec(File, [{form, {type,'==',error}}]) of
                      [] -> no_error;
                      _  -> error
                  end
          end,

    FilesWithErr = [ {?File:path(File), Err(File)} || File <- Files],

    message(filelist, FilesWithErr);

handle({reset}) ->
    ?Graph:reset_schema(),
    message(status, "Database clear started.", []);

%%% ============================================================================
%%% Transformation interface

handle({transform, Mod, Args}) ->
    ?Graph:backup(),
    ?Transform:do(Mod, Args);

handle({reply, Id, Reply}) ->
    ?Transform:reply(Id, Reply);

handle({cancel, Id}) ->
    ?Transform:cancel(Id);

%%% ----------------------------------------------------------------------------
%%% Movings

handle({funlist, File}) ->
    message(funlist,
            [{?Fun:name(F), ?Fun:arity(F)} ||
                F <- ?Query:exec(
                        ?Query:seq([?File:find(File),
                                    ?File:module(),
                                    ?Mod:locals()]))]);

handle({recordlist, File}) ->
    message(recordlist,
            [?Rec:name(R) ||
                R <- ?Query:exec(
                        ?Query:seq(?File:find(File), ?File:records()))]);

handle({macrolist, File}) ->
    message(macrolist,
            [?Macro:name(M) ||
                M <- ?Query:exec(
                        ?Query:seq(?File:find(File), ?File:macros()))]);

%%% ============================================================================
%%% Clustering

%% Sending options for the emacs interface
handle({cl_options, Alg}) ->
    OptList = cl_ui:cl_options(Alg),
    Ls = [[A,B] || {A,B} <- OptList],
    message(options, [Alg]++Ls);

handle({cl_refresh}) ->
    case cl_ui:refresh() of
      {cl_ui, _} -> message(status, "cl_ui refresh: cleaned",[]);
      _ -> message(error, "cl_ui refresh: something wrong",[])
    end;

handle({run_cl, Opt, Alg, Create}) ->
    {Clustering, FittNum} = cl_ui:run({Opt, Alg, Create}),
    message(status,"Clustering algorithm has been finished."),
    message(result,Clustering),
    message(flist,"~w",[FittNum]);

%%% ============================================================================
%%% Backup

handle({undo, _RFile}) ->
    %% (_RFile) This parameter is need to handling more backup checkpoints.
    FilesBefore = files_with_lastmod(),
    try ?Graph:undo() of
        ok  ->
            FilesAfter = files_with_lastmod(),
            ModifiedFiles =
                [ begin ?FileMan:save_file(File), ?File:path(File) end ||
                    {File, LastMod} <- FilesAfter,
                    lists:keysearch(File, 1, FilesBefore) =/= LastMod ],

            message(reload, ModifiedFiles),

            ?Graph:clean();
        _ ->
            message(status, "Need a backup to do it", [])
    catch
        throw:Msg ->
            message(error, "Error: ~p", [Msg])
    end;

%%% ============================================================================
%%% Attributes of the error form

%% @private
handle({error_attr}) ->
    case ?Query:exec([file, {form, {type, '==', error}}]) of
        [] ->  message(errorlist, []);
        ErrList when is_list(ErrList)->
            try
                AllError =
                [begin
                     [File] = ?Query:exec(Form, ?Form:file()),
                     Token = paarse((?Graph:data(Form))#form.tag),
                     {Position, Text} = case Token of
                                            [] -> {{?File:length(File)-1,
                                                    ?File:length(File)}, "EOF"};
                                            _ -> {?Token:pos(Token),
                                                  ?Token:text(Token)}
                                        end,
                     [{filepath, ?File:path(File)}, {nexttokentext, Text},
                      {position, Position}]

                 end || Form <- ErrList],
                message(errorlist, [AllError])
            catch
                throw:InternalErrorMsg ->
                    message(error, [InternalErrorMsg])
            end
    end;

%%% ============================================================================
%%% Handling file status information

handle({status_info, FileList}) ->
    message(filestatus, [file_status_info(FileList)]).

%%% ============================================================================
%%% Parser for the error messages
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

%%% ============================================================================

progress(Op) ->
    fun
        (File, Count, Max) ->
            ?UI:message(progress, {Op, File, Count, Max})
    end.


%% Filters can be given by their atomic abbreviations or by numeric codes.
convert_filter(Type) when is_atom(Type) -> Type;
convert_filter(2) -> sem;
convert_filter(3) -> ctx;
convert_filter(4) -> syn;
convert_filter(5) -> synlex;
convert_filter(6) -> lex;
convert_filter(7) -> inp;
convert_filter(8) -> not_lex;
convert_filter(_) -> all.


error_message({no_include_file, File}) ->
    ?MISC:format("Include file \"~s\" not found", [File]);
error_message(Error) ->
    ?MISC:format("Error: ~p", [Error]).


files_with_lastmod() ->
    [{File, LastMod}
     || File <- ?Query:exec([file]),
        LastMod <- [(?ESG:data(File))#file.lastmod]].

file_status_info([])->
    Files = ?Query:exec([file]),
    [file_stat(File) || File <- Files];

file_status_info(FileList)->
    lists:map(
      fun(FileName) ->
              case ?Query:exec(?File:find(FileName)) of
                  [File] ->
                      file_stat(File);
                  _ ->
                      [{file, FileName}, {errors, no}, {type, none},
                       {lastmod, undefined}, {status, missing}]
              end
      end,
      FileList).

file_stat(File)->
    Path = ?File:path(File),
    Type = ?File:type(File),
    LastMod = (?Graph:data(File))#file.lastmod,
    IsError = case ?Query:exec(File, [{form, {type, '==', error}}]) of
                  [] -> no;
                  _ -> yes
              end,

    [{file, Path}, {error,IsError}, {type, Type},
     {lastmod, LastMod}, {status, loaded}].
