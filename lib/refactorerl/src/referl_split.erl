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

%%% @doc This module can collect data from the output of the cutlib,
%%% it can execute splitting using the data from the cutlib.
%%% To do splitting this module uses the referl_fileman, referl_movefun
%%% and the referl_movemacro modules.
%%%
%%% TODO: need to use move_record and move_macro modules.
%%% TODO: using Emacs to run refac split
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

%%% @private

-module(referl_split).
-vsn("$Rev: 1895 $").
-include("refactorerl.hrl").

-export([create_modules/1,add_module/1,
         collect_attribs/1,start/1,functions/1,do_refac/3,do/1]).

-export([prepare/0]).
-define(CUTLIB,{[],[]}).
%-record(module,{name}).
%-record(file,{path}).

-record(rec_attr, {file, name}).
-record(macro_attr,{file,name}).
-record(fun_attr,{mod, name, arity}).

-define(MODNAME,"lib").
-define(PATH,"").

%%% REM DIAL CutResult is always empty
%%% @spec start({Modname::string(),OutPut::cutlib_output(),_SourceFiles}) ->
%%%                                {start, done}| {error, Reason}
%%%
%%% @doc This function (main function of the module) starts the
%%% clustering, creates files, creates new clusters and executes
%%% the collect_attribs/1

start({ModName, _Output, _SourceFiles})->
    ?GRAPH:backup(),
    %lists:map(fun ?FILEMAN:add_file/1,
    %                          SourceFiles),
    {CutResult,_CutData}= ?CUTLIB,
    %Files = [(?ESG:data(File))#file.path ||
    %          {File,{_NotMoved, _Moved}} <- CutResult],
    MovedList = [{File,Moved} || {File, {_NotMoved, Moved}} <- CutResult],
    %%%lists:map(fun ?FILEMAN:add_file/1, Files),
    collect_attribs({ets:new(clusters,[bag]), MovedList, ModName}),
    {refac_split, done}.

%%% @spec collect_attribs({EtsT::ets(),
%%%                       Movelist::list(),
%%%                       ModName::string()}) -> term()
%%%
%%% @doc This function collects the functions, macros and records to move
%%% into a new clusters, creates the clusters and executes the move
%%% function refactoring on the element of the cutlibs' output

collect_attribs({EtsT, MoveList, ModName})->
    Nls = [Moved || {_File, Moved} <- MoveList],
    NlsB = [[Num ||{Num,_} <-Lista] || Lista <- Nls],
    Numlist= lists:umerge(NlsB),
    [[functions({EtsT, [], Num, File, Attr}) || {Num, Attr} <- Lista]
               || {File,Lista} <- MoveList],
    %ClusterIds = [Cluster || {_File, Cluster} <- Nls],
    create_modules({ModName, Numlist}),
    do_refac(EtsT, Numlist,ModName).

do_refac(EtsT, [Akt| NumList], ModName)->
    RecS = [{Record, ModName} || Record <- ets:lookup(EtsT,Akt)],
    lists:map(fun do/1, RecS),
    do_refac(EtsT, NumList, ModName);
do_refac(_EtsT, [], _ModName)->
    {refac_split, ok}.

do({Rec, ModName})->
    case Rec of
       {Num,#fun_attr{mod=Mod,name=Name,arity=Arity}}=Rec ->
                  LMnt = {Num, ?PATH++atom_to_list(Mod)++".erl", Name, Arity},
                  movefun(LMnt,ModName);
       {Num,#rec_attr{file=ToFile, name=Name}}=Rec ->
                  _LMnt = {Num, ToFile, Name};
       {Num,#macro_attr{file=ToFile, name=Name}}=Rec ->
                  _LMnt = {Num, ToFile, Name};
      _ -> _LMnt = error
   end.

movefun(LMnt,Module)->
    {ClusterId, Source, FunName, Arity}=LMnt,
    Target = list_to_atom(Module++integer_to_list(ClusterId)),
    Funs = [{FunName, Arity}],
    io:format("~nTargetFile : ~w~n SourceFile : ~s~n Funs:~w~n",
              [Target, Source, Funs]),
% TODO should it not call referl_tr_move_fun:do?
    referl_ui:movefun(Source,Target,Funs),
    io:format("~s/~w  ~s.erl",[FunName,Arity,Target]).

%%% @spec functions(Arg) ->  {refac_split,function,done}
%%%                      |{error,Reason}
%%%
%%% @doc This function stores the records from the output of the cutlib
%%% into an ets table named cluster

functions({Ets, Lista, Num, FromFile, [Rec| AttrTail]})->
    %for testing io:format("~w~n~n",[Rec]),
    L = Lista ++ [{Num,Rec}],
    functions({Ets, L, Num, FromFile, AttrTail});
functions({Ets, Lista, _Num, _FromFile, []}) ->
       ets:insert(Ets,Lista),
       ok.

%%% get the actual directory

getdir()->
    %file:get_cwd()
    {ok, ?PATH}.


create_modules({ModName, [ModuleId| Modules]})->
    {ok, Dir}=getdir(),
    Name = ModName ++ integer_to_list(ModuleId),
    Fname = Name ++ ".erl",
    %%%For testingio:format("~s~n",[Fname]),
    Data = list_to_binary("-module("
                          ++Name
                          ++").\n"),
    case file:write_file(
                  filename:absname_join(Dir,Fname),Data) of
        ok -> ok;
        {error, Reason} -> throw({file_creation_error, Reason})
%        _ -> {unknown_error}
    end,
    Path = filename:absname_join(Dir,Fname),
    case filelib:is_file(Path) of
        true -> add_module(Fname);
        false -> throw({file_not_found,Fname})
    end,
    create_modules({ModName,Modules});
create_modules({_,[]})->
    {ok, created}.

%%% @spec add_module(Path::string()) ->
%%%                  {file, Node::node()} | {error, Reason}
%%% @doc This function calls the referl_fileman:add_file/1 to
%%% move the new modules into a refactorerl graph
%%% This is optional in the start/1
add_module(Path)->
    case ?FILEMAN:add_file(?PATH++Path) of
        {file, Node}-> ?FILEMAN:add_file(?PATH++Path),{ok, Node};
        {error, Reason}-> throw({has_no_effect,Reason})
    end.

prepare()->
   referl_ui:saveconfig([],[],?PATH),
   {ok, prepared}.
