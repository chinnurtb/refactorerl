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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the `Move macros between files' refactoring.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li>The file from where the macro definition is to be moved
%%%       (see {@link reflib_args:file/1}).</li>
%%%   <li>The destination file's name
%%%       (see {@link reflib_args:filename/1}).</li>
%%%   <li>The list of macros to be moved
%%%       (see {@link reflib_args:macros/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ol>
%%%   <li>The names of the macros to be moved must not clash with existing
%%%       macro names in none of:
%%%     <ul>
%%%       <li>the target file</li>
%%%       <li>the target's included files</li>
%%%       <li>files where the target is included</li>
%%%     </ul> </li>
%%%   <li>Moving macros from a header to a module is only allowed if there
%%%       exist no other module that both includes the header and uses
%%%       some of the macros to be moved.</li>
%%%   <li>An include form can only by introduced when it does not cause
%%%       inconsistency at the place of inclusion.</li>
%%% </ol>
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>
%%% @todo a clean rewrite of the code, perhaps accompanied by a fusion
%%%       with move_rec, because at the moment, this is a tweaked copy
%%%       of Daniel's move_rec.
%%% @todo Perhaps also consider fusion with move_fun?

-module(reftr_move_mac).
-vsn("$Rev: 5651 $").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([prepare/1]).
-export([error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Main types

-record(macinfo, {name, form, files}).
-record(info, {fromfile, tofile, fromheader, toheader,
               names, nodes, forms, files}).


%%% ============================================================================
%%% Errors

%% @private
error_text(name_collision, [Clash]) ->
    ["Macro with given name already exists in target ",
     ?MISC:format("~p", [Clash])];

error_text(not_only_in_target, [OtherUseNames]) ->
    ["Cannot remove macro from header, included in other files: ",
     ?MISC:join(OtherUseNames)];

error_text(unincludable, [FileName, ToFileName]) ->
    ["Cannot include ", FileName, " in ", ToFileName];
%    "Cannot include the target header in some files, " ++
%        "where the from-file header is included";

error_text(not_movable, [FileName, Name]) ->
    ["File ", FileName, " also includes the target file ",
     "and causes name collision with macro ", Name];
%    "Cannot move to header, because there's name " ++
%        "collision (mediately) with a macro in a file, " ++
%        "which includes the target header";

error_text(delete, []) ->
    "Cannot delete macro from the source file as it is being used".


%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Macros   = ?Args:macros(Args),
    [FromFile] = ?Query:exec(hd(Macros), ?Macro:file()),
    FromPath = ?File:path(FromFile),
    ToName   = ?Args:filename(Args),
    ToPath   = target_path(ToName, FromPath),
    ToFile   = ?Query:exec1(?File:find(ToPath), ?RefErr0r(target_not_found)),

    {Names, Forms, Files} =
        lists:foldl(
          fun(#macinfo{name=Name, form=Form, files=Files},
              {Na, Fo, Fi}) ->
                  {[Name|Na], [Form|Fo], lists:usort(lists:flatten([Files|Fi]))}
          end,
          {[], [], []},
          [info(Node) || Node <- Macros]),

    check_name_conflicts(FromFile, ToFile, Names),

    Info = #info{fromfile = FromFile, tofile = ToFile,
                 fromheader = ?File:type(FromFile) == header,
                 toheader   = ?File:type(ToFile)   == header,
                 names = Names, forms = Forms, files = Files},

    check_unincludable(Info),
    check_movable_to_header(Info),
    check_used_only_in_target(Info),
    check_no_using(Info),

    [fun() ->
        transform(Info)
     end,
     fun(_)->
        Macros
     end].


%%% ============================================================================
%%% Implementation

info(Form) ->
    Refs = ?Query:exec(Form,
                       ?Query:seq([[{mref,back}, {llex,back}, {elex,back}],
                                   ?Expr:clause(),
                                   ?Clause:form(), ?Form:file()])),
    #macinfo{name=?Macro:name(Form), form=Form, files=lists:usort(Refs)}.

transform(#info{fromfile=FFile, tofile=TFile, forms=Forms,
                toheader=ToHeader, files=Files}) ->
    %% Must be before the form movings! ?? Analysers??!!
    %[?Graph:rmlink(FFile, record, Node) || Node <- Nodes],
    %[?Graph:mklink(TFile, record, Node) || Node <- Nodes],
    [?Transform:touch(File) || File <- lists:usort([FFile, TFile] ++ Files)],
    [fun() -> init end]
        ++
        [fun(_) ->
                 ?File:add_include(File, TFile)
         end
         || File <- Files, ToHeader]
        ++
        [fun(_) ->
                 ?File:del_form(Form),
                 ?File:add_form(TFile, Form)
         end
         || Form <- Forms].



%%% ----------------------------------------------------------------------------
%%% Checks

check_name_conflicts(FromFile, ToFile, Names) ->
    Includes =
        fun(File) ->
                Incs = ?Query:exec(File, ?File:includes()),
                lists:delete(FromFile, lists:usort(Incs))
        end,
    Exists =
        fun(File, Name) ->
                ?Query:exec(File,
                            ?Query:seq([Includes, ?File:macro(Name)])) =/= []
        end,
%    io:format("..... includes ~p~n", [Includes(ToFile)]),
%    [begin
%         io:format(".... ~p~n", [])
%     end || Name <- Names],
    Clash = [Name || Name <- Names, Exists(ToFile, Name)],
    ?Check(Clash == [], ?LocalError(name_collision, [Clash])).

%% header -> module
%% (Are the macros used only in target module?)
check_used_only_in_target(#info{fromheader=true, toheader=false,
                                files=Files, tofile=TFile}) ->
    OtherUses = Files -- [TFile],
    OtherUseNames = lists:map(fun ?File:path/1, OtherUses),
    ?Check(OtherUses == [], ?LocalError(not_only_in_target, [OtherUseNames]));
check_used_only_in_target(_) -> ok.

%% module -> module
%% (Are the macros used?)
check_no_using(#info{fromheader=false, toheader=false, files=Files}) ->
    ?Check(Files == [], ?LocalErr0r(delete)); %% todo: correct?
check_no_using(_) -> ok.

%% header -> header
%% (Error, when we cannot include the target header somewhere.)
check_unincludable(#info{fromheader=true, toheader=true,
                         tofile=ToFile, files=Files}) ->
    ToFileName = ?File:path(ToFile),
    [?Check(?File:includable(File, ToFile),
            ?LocalError(unincludable, [FileName, ToFileName]))
     || File <- Files,
        FileName <- [?File:path(File)]];
check_unincludable(_) -> ok.


%% _ -> header
%% (Moving should not make name conflicts.)
check_movable_to_header(#info{toheader=true, names=Names,
                              fromfile=FFile, tofile=TFile}) ->
    TargetIncluders = ?Query:exec(TFile, ?File:included()) -- [FFile, TFile],
    [?Check(?Query:exec(File, ?File:macro(Name)) == [],
            ?LocalError(not_movable, [FileName, Name]))
     || Name <- Names,
        File <- TargetIncluders,
        FileName <- [?File:path(File)]];
check_movable_to_header(_) -> ok.


%%% ----------------------------------------------------------------------------

target_path(ToName, FromPath) ->
    case filename:pathtype(ToName) of
        absolute ->
            ToName;
        relative ->
%%            SourceDir = filename:dirname(filename:absname(FromPath)),
            SourceDir = filename:dirname(FromPath),
            filename:absname(ToName, SourceDir);
        volumerelative ->
            todo
    end.
