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

%%% ============================================================================
%%% Module information

%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%%
%%% @doc Move record transformation module

-module(referl_tr_move_rec).
-vsn("$Rev: 1973 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/3]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

-record(state, {from, fromfile,
                target, targetfile,
                fromheader, toheader,
                infos, names, nodes, forms, files}).

-record(info, {name, form, node, files}).

%%% ============================================================================
%%% Errors

throw_name_collision(ErrArg) ->
    throw("record with specified name already exists in target " ++ ErrArg).

msg_not_only_in_target() ->
    "can't move record from header, it is used not only in target".

msg_unincludable() ->
    "can't include the target hrl in some files, " ++
        "which include the from-file header".

msg_not_movable() ->
    "can't move to header, because there's name " ++
        "conflict with a record in a file, " ++
        "which includes the target hrl".

msg_delete() ->
    "can't delete record from source file".

%%% ============================================================================
%%% Interface

%% @spec do(string(), [atom()], string()) -> ok
%% @doc Moves the definiton of the given records from one file to another one.
do(FromFile, Names, TargetFile) ->
    ?TRANSFORM:do(?MODULE, {FromFile, Names, TargetFile}).

%%% ============================================================================
%%% Callbacks

%% @private
init({FromFile, Names, TargetFile}) ->
    Infos = [#info{name = Name} || Name <- Names],
    #state{from = FromFile, target = TargetFile, infos = Infos}.

%% @private
steps() ->
    [fun check_source_target/1,
     fun query_filetypes/1,
     fun query_records/1,
     fun check_name_conflicts/1,
     fun check_unincludable/1,
     fun check_movable_to_header/1,
     fun check_used_only_in_target/1,
     fun check_no_using/1].

%% @private
transform(St = #state{fromfile=FFile, targetfile=TFile, nodes=Nodes,
                      forms=Forms, toheader=ToHeader, files=Files}) ->
    TPath = (?GRAPH:data(TFile))#file.path,

    %% Must be before the form movings!
    [?GRAPH:rmlink(FFile, record, Node) || Node <- Nodes],
    [?GRAPH:mklink(TFile, record, Node) || Node <- Nodes],

    [?MANIP:move_form(Form, FFile, TFile)    || Form <- Forms],
    [?MANIP:add_include(File, TPath) || File <- Files, ToHeader],
    ?ESG:close(),
    {changed_files(St), ok}.

changed_files(#state{toheader=false, fromfile=FFile, targetfile=TFile}) ->
    [FFile, TFile];
changed_files(#state{toheader=true, files=Files, fromfile=FFile, targetfile=TFile}) ->
    lists:usort([FFile, TFile] ++ Files).


%%% ============================================================================
%%% Implementation

query_filetypes(St = #state{fromfile=FFile, targetfile=TFile}) ->
    St#state{fromheader = ?LEX:is_header_file(FFile),
             toheader =   ?LEX:is_header_file(TFile)}.

query_records(St = #state{fromfile=FFile, infos=Infos}) ->
    NewInfos = [info(FFile, Name) || #info{name=Name} <- Infos],
    {Names, Nodes, Forms, Files} =
        lists:foldl(
          fun(#info{name=Name, node=Node, form=Form, files=Files}, {Na, No, Fo, Fi}) ->
                  {[Name|Na], [Node|No], [Form|Fo], lists:usort(lists:flatten([Files|Fi]))}
          end,
          {[], [], [], []},
          NewInfos),
    St#state{infos=NewInfos, names=Names, nodes=Nodes, files=Files, forms=Forms}.

info(FFile, N) ->
    Name = ?MISC:to_atom(N),
    [Node] = ?GRAPH:path(FFile, [{record, {name, '==', N}}]),
    [Form] = ?GRAPH:path(Node, [{recdef, back}]),
    Refs = ?GRAPH:path(Node, [{recref, back}, sup, {visib, back},
                              scope, {funcl, back}]),
    Files = ?LEX:containing_files(Refs),
    #info{name=Name, node=Node, form=Form, files=Files}.

%%% ----------------------------------------------------------------------------
%%% Checks

check_source_target(St = #state{from=From, target=Target}) ->
    FFile = ?SYNTAX:get_file(From),
    TFile = ?SYNTAX:get_file(Target),
    St#state{fromfile = FFile, targetfile=TFile}.

check_name_conflicts(#state{targetfile=TFile, names=Names}) ->
    Clash = [Name || Name <- Names, ?LEX:exists(TFile, Name, record)],
    ErrArg = ?MISC:format("(~p).", [Clash]),
    case Clash of
        [] ->
            ok;
        _ ->
            throw_name_collision(ErrArg)
    end.

%% From Header, To Source
%% Movable only, when names used only in target module
check_used_only_in_target(#state{fromheader=true, toheader=false,
                                 files=Files, targetfile=TFile}) ->
    if 
        Files == [] -> ok;
        true ->
            ?MISC:error_on_difference(Files, [TFile], msg_not_only_in_target()),
            ok
    end;
check_used_only_in_target(_) -> ok.

%% From Header, To Header
%% Error, when we can't include the target somewhere
check_unincludable(#state{fromheader=true, toheader=true,
                          targetfile=TFile, files=Files}) ->
    Ok = lists:all(fun(File) -> ?LEX:includable(File, TFile) end, Files),
    ?MISC:error_on_difference(Ok, true, msg_unincludable()),
    ok;
check_unincludable(_) -> ok.

%% To Header
%% Moving will make no name conflicts
check_movable_to_header(#state{toheader=true, names=Names,
                               fromfile=FFile, targetfile=TFile}) ->
    [?MISC:error_on_difference(
        ?LEX:movable_to_header(record, Name, FFile, TFile), true, msg_not_movable()) ||
        Name <- Names],
    ok;
check_movable_to_header(_) -> ok.

%% From Source, To Source
%% Movable only when the record is not used
check_no_using(#state{fromheader=false, toheader=false, files=Files}) ->
    ?MISC:error_on_difference(Files, [], msg_delete()),
    ok;
check_no_using(_) -> ok.
