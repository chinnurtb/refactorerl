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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the rename macro refactoring. This
%%% refactoring can rename macros in either modules or header files.
%%% The transformation results in the macro name at both the definition
%%% and at references to the given macro to be changed. The condition
%%% of the renaming is that there is no name conflict with another record
%%% in the file containing the macro, in any of its includes or anywhere
%%% it has been included at.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li> The macro to be renamed
%%%        (see {@link referl_args:macro/1}).</li>
%%%   <li> The name which is to be given to the macro
%%%        (see {@link referl_args:name/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li> No macro already exist with the same new name in either
%%%     <ul>
%%%       <li>in a file hosting definition or usage of the macro,</li>
%%%       <li>in files included by the said,</li>
%%%       <li>in files that include the said</li>
%%%     </ul>
%%%   </li>
%%% </ul>
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>The macro name is replaced with the new name at both definition
%%%       and all usage sites</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(referl_tr_rename_mac).
-vsn("$Rev: 3185 $").
-include("refactorerl.hrl").

-define(NODECLASS(Node), element(2,Node)).

%% Callbacks
-export([prepare/1]).
-export([error_text/2]).

%%% ============================================================================
%%% Callbacks

%% @private
error_text(int_graph,[]) ->
    ["Internal error caused by graph structure anomaly"];
error_text(expected_node,[Atom,Node])->
    ["Expected node class "++io_lib:print(Atom)++
     " at node "++io_lib:print(Node)];
error_text(_,_) -> unknown.

%% @private
prepare(Args) ->
    NewName = ?Args:macname(Args),
    Macro   = ?Args:macro(Args),
    check_conflicts(Macro,NewName),
    Refs    = query_mac_replacers(Macro,NewName),

    MacD    = ?ESG:data(Macro),
    NewMacD = MacD#form{tag=NewName},

    fun() ->
%%        ?Syn:replace(Macro, {flex,4}, [NewName]),
        ?ESG:update(Macro, NewMacD),
        ?Transform:touch(Macro),
        lists:foreach(fun replace_macname/1, Refs),
%%        ?Syn:replace(Subst, {llex,2}, [NewName]),
        ?ESG:close()
    end.

%%% ============================================================================
%%% Implementation

%% @doc Checks for the existance possible naming conflicts after the rename.
check_conflicts(Macro,NewName) ->
    File   = ?Query:exec([Macro], ?Macro:file()),
    Files  = lists:usort(?Query:exec(
             File, {all, [incl], [{incl,back}]})),
    Macros = ?Query:exec(Files,?File:macros()),
    Names  = [?Macro:name(M) || M <- Macros],
    ?Check(not lists:member(NewName,Names),
           ?RefError(mac_exists,[NewName])).
    
%% @doc Gathers the lexical nodes corresponding to both the definition and
%% the usages of the macro.
query_mac_replacers(Macro,NewName) ->
    Refs = ?Query:exec([Macro],[{mref,back}]),
    [  replacer_of_macname(Macro, {flex,4}, NewName) |
     [ replacer_of_macname(Subst, {llex,2}, NewName) ||
       Subst <- Refs ] ].

%% @doc It dereferences the leaf to handle both macro definition and uses.
%% It also handles virtual nodes caused by included macros.
replacer_of_macname(Parent, Leaf, New) ->
    NameNode = ?Query:exec1([Parent], [Leaf],
                            ?LocalError(int_graph,[])),
    ?Check(?NODECLASS(NameNode)==lex,
           ?LocalError(expected_node,[lex,NameNode])),
    NameNodeD=#lex{data=NND} = ?ESG:data(NameNode),
    case NND of
        virtual ->
            replacer_of_macname(NameNode,{orig,1},New);
        _ ->
            NewNameNodeD = NameNodeD#lex{
                             data=NND#token{value=New,text=New}},
            case ?Query:exec([NameNode],[{flex,back}]) of
                 []           -> {Parent,    NameNode,NewNameNodeD};
                 [RealParent] -> {RealParent,NameNode,NewNameNodeD}
            end
    end.

%% @doc Commences the rename given in the parameter on a single node
%% of the appropriate type.
replace_macname({Parent,NameNode,NewNameNodeD}) ->
    ?ESG:update(NameNode, NewNameNodeD),
    ?Transform:touch(Parent).
