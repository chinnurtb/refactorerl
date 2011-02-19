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

%%% @doc Analyse record definitions and references. The following semantical
%%% structure is created:
%%%
%%% <ul>
%%%
%%% <li>A semantical record object is created for every referenced and/or
%%%   defined record</li>
%%%
%%% <li>The containing module object is linked to the record object
%%%   (`record')</li>
%%%
%%% <li>The semantical record is linked to the syntactical record
%%%   definition when that definition is loaded (`recdef')</li>
%%%
%%% <li>Expressions that refer to a record are linked to the record object
%%%   (`recref').</li>
%%%
%%% </ul>
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(anal_record).
-vsn("$Rev: 1247 $").
-behaviour(refac_anal).

%% Interface exports
-export([record/1]).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

record(Name) ->
    [{record, {name, '==', Name}}].

%% @private
init() ->
    [{record, record_info(fields, record), []}, 
     {form, [{recdef, record}]},
     {file, [{record, record}]},
     {expr,   [{recref, record}]}
    ].

%% @private
insert(File, #file{}, _, Record, #form{type=record, tag=Name}) ->
    record_object(File, Name, Record);

insert(_,_,_, E, #expr{kind=record_expr}) ->
    [N|_] = ?GRAPH:path(E, [sub]),
    #expr{kind=atom, value=Name} = ?GRAPH:data(N),
    [File] = ?GRAPH:path(E, anal_context:expr_module() ++ [{moddef, back}]),
    case ?GRAPH:path(File, [incl] ++ record(Name)) of
	[] ->
	    Record = record_object(File, Name);
	[Record] ->
	    ok
    end,
    ?GRAPH:mklink(E, recref, Record);	    

insert(_,_,_,_,_) ->
    ok.

%% @private
remove(_, #file{}, _, Record, #form{type=record}) ->
    case ?GRAPH:path(Record, [recdef]) of
        [RecObj] ->
            remove_reclink(Record, recdef, RecObj);
        D ->
            error_logger:warning_msg("Bad recdef for ~p: ~p~n", [Record, D])
    end;

remove(_, _, _, Expr, #expr{kind=record_expr}) ->
    case ?GRAPH:path(Expr, [recref]) of
        [Record] ->
            remove_reclink(Expr, recref, Record);
	[] ->
	    ok
    end;

remove(_,_,_,_,_) ->
    ok.

remove_reclink(From, Tag, Record) ->
    ?GRAPH:rmlink(From, Tag, Record),
    Links =
        ?GRAPH:path(Record, [{recdef, back}]) ++
        ?GRAPH:path(Record, [{recref, back}]),
    case Links of
        [] ->
            case ?GRAPH:path(Record, [{record, back}]) of
                [File] ->
                    ?GRAPH:rmlink(File, record, Record),            
                    ?GRAPH:delete(Record);
                M ->
                    error_logger:warning_msg("Bad record link to ~p: ~p~n",
                                             [Record, M])
            end;
        _ -> ok
    end.

record_object(File, Name) ->
    record_object(File, Name, undefined).

record_object(File, Name, Def) ->
    case ?GRAPH:path(File, [incl, {record, {name, '==', Name}}]) of
	[Record] ->
	    ok;
	[] ->
	    Record = ?GRAPH:create(#record{name=Name}),
	    ?GRAPH:mklink(File, record, Record)
    end,
    if
	Def =:= undefined ->
	    ok;
	true ->
	    ?GRAPH:mklink(Def, recdef, Record)
    end,
    Record.
