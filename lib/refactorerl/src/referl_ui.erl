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
%%% <dt>{@type {add, Path::path()@}}</dt> <dd>The file specified by `Path' is
%%% added (or maybe re-added) to the database.</dd>
%%%
%%% <dt>{@type {drop, Path::path()@}}</dt> <dd>The file specified by `Path' is
%%% removed from the database.</dd>
%%%
%%% <dt>{@type {invalid, Path::path()@}}</dt> <dd>The file specified by `Path'
%%% is added to the database, but it is invalid (contains errors).</dd>
%%%
%%% <dt>{@type {reload, Path::path()@}}</dt> <dd>The file specified by `Path'
%%% is modified by the tool (and should be reloaded from disk by the UI).</dd>
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
%%% <dt>{@type {filelist, Files::[path()]@}}</dt> <dd>This message contains
%%% the files that are currently stored in the database. There is a special
%%% notation: when the first character of a filename is an exclamation mark
%%% (`!'), it is not part of the filename, it means that the file has some
%%% errors. <small>TODO: This representation should be improved.</small></dd>
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
%%% move record refactoring, ans should be improved.</small></dd>
%%%
%%% <dt>{@type {macrolist, Macros::[Name::atom()|string()]@}}</dt> <dd>This message
%%% contains a list of macros. <small>TODO: this is rather specific to the
%%% move macro refactoring, ans should be improved.</small></dd>
%%%
%%% <dt>{@type {question, {Id::integer(), Details::proplist()@}@}}</dt>
%%% <dd>Contains a question that must be answered by {@link reply/2} or
%%% cancelled by {@link cancel/1}. <small>TODO: `Details' are to be
%%% specified.</small></dd>
%%%
%%% <dt>{@type {uifinished, Operation::atom()@}}</dt> <dd>The spawning of the
%%  specified operation is complete on the UI side.</dd>
%%%
%%% <dt>{@type {trfinished, ok@}}</dt> <dd>An operation is finished
%%  on the transform side.</dd>
%%%
%%% </dl>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_ui).
-vsn("$Rev: 1479$ ").
-behaviour(gen_server).

%%% Client exports
-export([stop/0, status/1, add/1, drop/1, draw/2,
         showconfig/0, saveconfig/3, filelist/0, reset/0]).

-export([transform/2, reply/2, cancel/1, wr_transform/2, search_dup/1]).
-export([funlist/1, recordlist/1, macrolist/1]).

%%% Undo/redo
-export([backup/0, undo/1, clean/0]).

%%% Clustering
-export([cl_options/1, run_cl/3, cl_refresh/0]).

%% UI message handlers
-export([message/2, message/3, add_msg_handler/2, del_msg_handler/2,
         error_message/1]).

%% File status info
-export([status_info/1]).

%%% Error forms
-export([error_attr/0]).

%%% Server exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("refactorerl.hrl").
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
%%      The message of the function is a `FileStatusList' that is a [proplist()]
%%      containes information about the file(s).
%%
%%      Elements of the `FileStatusList':
%%
%%      {file,string()}          The path and the name of the file
%%      {errors,atom()}          The file contains errors: `yes'|`no'
%%      {type,atom()}            The type of the file: `none'|`module'|`include'
%%      {lastmod,int()|atom()}   The last modification of th file. The 
%%                               default value is `undefined' 
%%      {status,atom()}          Status of the file: `loaded' or `missing'
status_info(Prop) -> cast({status_info, Prop}).

%% @spec add(path()) -> ok
%% @doc Adds `File' to the database.
add(File) -> cast({add, File}).

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
%% @see referl_transform
%% @see referl_args
transform(Mod, Args) -> cast({transform, Mod, Args}).

%% @spec search_dup(proplist()) -> [clone()] 

%% @doc Calls Wrangler's duplicated code detection functionality and
%% returns the found `clones'. The proplist should contain the
%% directory(ies) or the file where the detection is performed, 
%% the minimum number of tokens in a code clone, and 
%% the minimum number of appearance time in a clone in the file.
%% 
%% `clone()::[{File::string(), Start::{integer(),integer()}, 
%% End::{integer(), integer()}}]'
%% @see duplicated_code_detection
search_dup(Args)-> cast({search_dup, Args}).

%% @spec wr_transform(atom(), proplist()) -> ok
%% @doc Initiates a transformation from Wrangler. `Mod' is a transformation 
%% module name, `Args' is a proplist that contains the arguments of the 
%% transformation.
%% @see referl_tr_wrangler
wr_transform(Mod, Args) -> cast({wr_transform, Mod, Args}).

%% @spec reply(integer(), term()) -> ok
%% @doc Provides a reply to a previously asked question.
%% @see referl_transform:reply/2
reply(Id, Reply) -> cast({reply, Id, Reply}).

%% @spec cancel(integer()) -> ok
%% @doc Cancels a previously asked question.
%% @see referl_transform:cancel/1
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

%% @spec backup() -> ok
%% @doc Backups tables of Refactorerl database.
backup() -> cast({backup}).

%% @spec error_attr() -> ok
%% @doc This function can collect information about error forms.
error_attr() -> cast({error_attr}).

%% @spec undo(string()) -> ok
%% @doc Steps backward on the Refactorerl database.
undo(RFile) -> cast({undo, RFile}).

%% @spec clean() -> ok
%% @doc Clean backup files from the Refactorerl database.
clean() -> cast({clean}).
%redo(RFile) -> cast({redo, RFile}).

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
handle_cast(Cmd, S) when is_tuple(Cmd) ->
    [CmdName | Args] = tuple_to_list(Cmd),
    Handler = case CmdName of
                  stop         -> fun handle_stop/0;
                  status       -> fun handle_status/1;
                  status_info  -> fun handle_status_info/1;
                  add          -> fun handle_add/1;
                  drop         -> fun handle_drop/1;
                  draw         -> fun handle_draw/2;
                  showconfig   -> fun handle_showconfig/0;
                  saveconfig   -> fun handle_saveconfig/3;
                  reset        -> fun handle_reset/0;
                  echo         -> fun handle_echo/2;

                  transform    -> fun handle_transform/2;
                  wr_transform -> fun handle_wr_transform/2;
                  search_dup   -> fun handle_search_dup/1;
                  reply        -> fun handle_reply/2;
                  cancel       -> fun handle_cancel/1;

                  filelist     -> fun handle_filelist/0;
                  funlist      -> fun handle_funlist/1;
                  recordlist   -> fun handle_recordlist/1;
                  macrolist    -> fun handle_macrolist/1;

                  backup       -> fun handle_backup/0;
                  undo         -> fun handle_undo/1;
                  clean        -> fun handle_clean/0;

                  run_cl       -> fun handle_run_cl/3;
                  cl_options   -> fun handle_cl_options/1;
                  cl_refresh   -> fun handle_cl_refresh/0;
                  error_attr  -> fun handle_error_attr/0;
                   _            -> error
              end,
    try
        apply(Handler, Args)
    catch
        error:Err ->
            error_logger:error_msg("Bad request ~p~n"
                                   "  Error: ~p~n"
                                   "  Stack: ~p~n",
                                   [Cmd, Err, erlang:get_stacktrace()]),
            message(error, "error handling request ~512P", [Cmd,4])
    end,
    message(uifinished, CmdName),
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

handle_stop() ->
    message(status, "RefactorErl server is shutting down...",[]),
    init:stop().

handle_status(File) when is_list(File) ->
    case ?Query:exec(?File:find(File)) of
        [F] ->
            case ?Graph:path(F, [{form, {type,'==',error}}]) of
                [] -> message(add, "~s", [File]);
                _  -> message(invalid, "~s", [File])
            end;
        []  -> message(drop, "~s", [File])
    end.

handle_add(Name) when is_list(Name) ->
    case ?Query:exec(?File:find(Name)) of
        [F] ->
            Drop = [(?Graph:data(FN))#file.path ||
                       FN <- ?Graph:path(F, [{incl, back}])],
            ?FileMan:drop_file(F);
        [] ->
            Drop = []
    end,
    case ?FileMan:add_file(Name) of
        {error, Reason} ->
            [message(drop, "~s", [DF]) || DF <- Drop],
            message(invalid, "~s", [Name]),
            message(status, "~s", [error_message(Reason)]);
        {file, File} ->
            Add = [(?Graph:data(FN))#file.path ||
                      FN <- ?Graph:path(File, [incl])],
            [message(drop, "~s", [DF]) || DF <- Drop],
            [message(add, "~s", [AF]) || AF <- Add]
    end.

handle_drop(File) when is_list(File) ->
    case ?Query:exec(?File:find(File)) of
        [F] ->
            Drop = [(?Graph:data(FN))#file.path ||
                       FN <- ?Graph:path(F, [{incl, back}])],
            ?FileMan:drop_file(F),
            [message(drop, "~s", [DF]) || DF <- Drop];
        [] ->
            message(status, "Already dropped", [])
    end.

handle_draw(File, Type) ->
    Filt = case Type of
               2 -> sem;
               3 -> ctx;
               4 -> syn;
               5 -> synlex;
               6 -> lex;
               7 -> inp;
               8 -> not_lex;
               _ -> all
           end,
    ?DRAW_GRAPH:draw_graph(File, Filt),
    message(status, "Ok", []).

handle_showconfig() ->
    message(showconfig,
            [{Name, Value} ||
                Env <- ?Graph:path(?Graph:root(), [env]),
                #env{name=Name, value=Value} <- [?Graph:data(Env)]]).

handle_saveconfig(AppDirs, IncDirs, OutDir) ->
    [?Graph:delete(Env) || Env <- ?Graph:path(?Graph:root(), [env])],
    [?Graph:mklink(?Graph:root(), env,
                   ?Graph:create(#env{name=appbase, value=Dir})) ||
                      Dir <- AppDirs],
    [?Graph:mklink(?Graph:root(), env,
                   ?Graph:create(#env{name=include, value=Dir})) ||
                      Dir <- IncDirs],
    ?Graph:mklink(?Graph:root(), env,
                  ?Graph:create(#env{name=output, value=OutDir})),
    message(status, "Configuration saved.", []).

handle_filelist() ->
    Files = [ {(?Graph:data(F))#file.path, F} ||
                F <- ?Graph:path(?Graph:root(), [file])],
    St = fun(N) -> case ?Graph:path(N, [{form, {type,'==',error}}]) of
                       [] -> ""; _ -> "!" end
         end,
    [message(filelist, "~s~s",[St(N),F]) || {F,N}<-lists:sort(Files)].

handle_reset() ->
    ?Graph:reset_schema(),
    message(status, "Database clear started.", []).

handle_echo(Type, String) ->
    message(Type, "~s~n", [String]).

%% @private
error_message({no_include_file, File}) ->
    ?MISC:format("Include file \"~s\" not found", [File]);
error_message(Error) ->
    ?MISC:format("Error: ~p", [Error]).


%%% ============================================================================
%%% Transformation interface

handle_transform(Mod, Args) ->
    ?Graph:backup(),
    ?Transform:do(Mod, Args).

handle_wr_transform(Mod, Args) ->
    case application:start(wrangler_app) of 
         {error, {already_started, wrangler_app}} -> wrtrans(Mod, Args);
         {error, _} -> message(error, "~p", "Please set Wrangler's " ++
                         "installation directory, and restart RefactorErl");
        _ -> wrtrans(Mod, Args)
    end.

wrtrans(Mod, Args)->
    application:start(wrangler_app),
    case apply(referl_tr_wrangler, Mod, [Args]) of
        {NewPath, {ok, Files}} ->
            message(rename, {hd(Files), NewPath}),
            [message(reload, "~s", [Path]) || Path <- tl(Files) ++ [NewPath]],
            message(status, "Finished", []),
            [wrdrop(File) || File <- [ NewPath | tl(Files)]];
        {_File, {error, Reason}} -> message(status, "~p", [Reason]);
        {error, Reason} -> message(status, "~p", [Reason]);
        {ok, Files} -> 
            [message(reload, "~s", [Path]) || Path <- Files],
            message(status, "Finished", []),
            [wrdrop(File) || File <- Files];
        Error -> message(error, "~p", [Error])
    end.
    %application:stop(wrangler).

wrdrop(File) when is_list(File) ->
    case ?Query:exec(?File:find(File)) of
        [F] ->
            Drop = [(?Graph:data(FN))#file.path ||
                       FN <- ?Graph:path(F, [{incl, back}])],
            ?FileMan:drop_file(F),
            [message(drop, "~s", [DF]) || DF <- Drop];
        [] -> ok
    end.

handle_reply(Id, Reply) ->
    ?Transform:reply(Id, Reply).

handle_cancel(Id) ->
    ?Transform:cancel(Id).

%%% ============================================================================
%%% Duplicated code detection

handle_search_dup(Args) ->
    case application:start(wrangler_app) of 
         {error, {already_started, wrangler_app}} -> dup(Args);
         {error, _} -> message(error, "~p", "Please set Wrangler's " ++
                         "installation directory, and restart RefactorErl");
         _ -> dup(Args)

    end.    

dup(Args) ->
    List = referl_wrangler:run_dup(Args),
    case List of
        [] -> message(error, "~p", "Wrangler did not find any "++
                      "duplicated code freagments");
        _ -> message(filepos, List)
    end.

%%% ----------------------------------------------------------------------------
%%% Movings

handle_funlist(File) ->
    message(funlist,
            [{?Fun:name(F), ?Fun:arity(F)} ||
                F <- ?Query:exec(
                        ?Query:seq([?File:find(File),
                                    ?File:module(),
                                    ?Mod:locals()]))]).

handle_recordlist(File) ->
    message(recordlist,
            [?Rec:name(R) ||
                R <- ?Query:exec(
                        ?Query:seq(?File:find(File),
                                    ?File:records()))]).

handle_macrolist(File) ->
    message(macrolist,
            [?Macro:name(M) ||
                M <- ?Query:exec(
                        ?Query:seq(?File:find(File),
                                    ?File:macros()))]).

%%% ============================================================================
%%% Clustering

%% Sending options for the emacs interface
handle_cl_options(Alg)->
    OptList = cl_ui:cl_options(Alg),
    Ls = [[A,B] || {A,B} <- OptList],
    message(options, [Alg]++Ls).

handle_cl_refresh()->
    case cl_ui:refresh() of
      {cl_ui, _} -> message(status, "cl_ui refresh: cleaned",[]);
      _ -> message(error, "cl_ui refresh: something wrong",[])
    end.

handle_run_cl(Opt, Alg, Create)->
    {Clustering, FittNum} = cl_ui:run({Opt, Alg, Create}),
    message(status,"Clustering algorithm has been finished."),
    message(result,Clustering),
    message(flist,"~w",[FittNum]).

%%% ============================================================================
%%% Backup

handle_backup() ->
   try ?Graph:backup() of
      {ok, Name} ->
          message(status, "Done ~s", [Name]);
      _ ->
          message(status, "Unsuccessful", [])
   catch
       throw:Msg ->
          message(error, "Error: ~p", [Msg])
   end.

handle_clean() ->
   try ?Graph:clean() of
      {ok, backups_deleted} ->
          message(status, "Backup storage is cleaned", []);
      _ ->
          message(status, "No backup files", [])
   catch
       throw:Msg ->
          message(error, "Error: ~p", [Msg])
   end.

handle_undo(_RFile) ->
    %% (_RFile) This parameter is need to handling more backup checkpoints.
    FilesBefore = files_with_lastmod(),
    try ?Graph:undo() of
        undo_is_ok  ->
            FilesAfter = files_with_lastmod(),
            [begin
                 case lists:keysearch(File, 1, FilesBefore) =:= LastMod of
                     true ->
                         ok;
                     false ->
                         ?FileMan:save_file(File),
                         message(reload, "~s", [?File:path(File)])
                 end
             end || {File, LastMod} <- FilesAfter],
            ?Graph:clean();
        _ ->
            message(status, "Need a backup to do it", [])
    catch
        throw:Msg ->
            message(error, "Error: ~p", [Msg])
    end.

files_with_lastmod() ->
    [{File, LastMod}
     || File <- ?Query:exec([file]),
        LastMod <- [(?ESG:data(File))#file.lastmod]].


%%% ============================================================================
%%% Attributes of the error form

%% @private
handle_error_attr()->
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
   end.

%%% ============================================================================
%%% Handling file status information

handle_status_info(Prop)->
    message(filestatus, [file_status_info(Prop)]).

file_status_info([])->
    Files = ?Graph:path(?Graph:root(),[file]),
    [file_stat(?File:path(FileNode)) || FileNode <- Files];

file_status_info(FileList)->
    [file_stat(File) || File <- FileList].

%% @private
file_stat(File)->
    NoFile = [{file, File}, {errors, no}, {type, none}, 
              {lastmod, undefined}, {status, missing}],

    Node = ?Graph:path(?Graph:root(),[{file,{path,'==',File}}]),
    case Node of
                 [] -> FileStat = NoFile;
           [InNode] -> case ?Graph:data(InNode) of
                      #file{path=Path, type=Type, lastmod=LastMod} ->
                           IsError = case ?Query:exec(InNode, 
                                             [{form, {type, '==', error}}]) of
                                           [] -> no;
                                            _ -> yes
                                     end,
                            FileStat = [{file, Path}, {error,IsError},
                                        {type, Type}, {lastmod, LastMod}, 
                                        {status, loaded}];    
                         _ ->  FileStat = NoFile
                   end;
	      _ -> FileStat = NoFile
    end,
    FileStat.


%%% ============================================================================
%%% Parser for the error messages
paarse({_,Mesg})->
    case regexp:match(Mesg,"{'\\$gn',[a-z]+,[0-9]+}") of
         {match, F, L}->
	    SToken = string:substr(Mesg, F, L),
           {ok, STerm, _}= erl_scan:string(SToken++"."),
           {ok, Token} = erl_parse:parse_term(STerm),
           Token;
	 _ -> []
    end;
paarse(_) ->
   [].
