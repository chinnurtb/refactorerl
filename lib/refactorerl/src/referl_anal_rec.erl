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
%%% <li>A semantical record object is created for every referenced and/or
%%%   defined record field too</li>
%%%
%%% <li>The containing record object is linked to the field object
%%%   (`field')</li>
%%%
%%% <li>The semantical field is linked to the syntactical field
%%%   definition when that definition is loaded (`fielddef')</li>
%%%
%%% <li>Expressions that refer to a field are linked to the field object
%%%   (`fieldref').</li>
%%%
%%% </ul>
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_anal_rec).
-vsn("$Rev: 1950 $").
-behaviour(referl_esg).

%% Interface exports
-export([record/1, field/1]).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @private
record(Name) ->
    [{record, {name, '==', Name}}].

%% @private
field(Name) ->
    [{field, {name, '==', Name}}].

%% @private
init() ->
    [{record, record_info(fields, record), []},
     {form, [{recdef, record}]},
     {file, [{record, record}]},
     {expr,   [{recref, record}]}
    ] ++
        [{field,  record_info(fields, field), []},
         {expr, [{fielddef, field}]},
         {record, [{field, field}]},
         {expr, [{fieldref, field}]}
        ].


%%% ----------------------------------------------------------------------------
%%% Insert


%% @private
insert(File, #file{}, _, Record, #form{type=attrib, tag=record}) ->
    #expr{value=Name} = ?GRAPH:data(hd(?GRAPH:path(Record, [{attr, 1}]))),
    case ?GRAPH:path(File, record(Name)) of
        [] -> update_field_defs(Record, record_object(File, Name, Record));
        _  -> record_object(File, Name, Record)
    end;

insert(_,_,_, E, #expr{kind=Kind})
  when Kind == record_expr orelse Kind == record_index ->
    [N | T] = ?GRAPH:path(E, [sub]),
    #expr{kind=atom, value=Name} = ?GRAPH:data(N),
    [File]=?GRAPH:path(E,[sup,{visib,back},scope,functx,modctx,{moddef,back}]),
    case ?GRAPH:path(File, [incl] ++ record(Name)) of
        [] ->
            Record = record_object(File, Name);
        [Record] ->
            ok
    end,
    ?GRAPH:mklink(E, recref, Record),
    field_references(Kind, Record, T);

insert(_,_,_, E, #expr{kind=Kind})
  when Kind == record_update orelse Kind == record_access ->
    [_, N | T] = ?GRAPH:path(E, [sub]),
    #expr{kind=atom, value=Name} = ?GRAPH:data(N),
    [File]=?GRAPH:path(E,[sup,{visib,back},scope,functx,modctx,{moddef,back}]),
    case ?GRAPH:path(File, [incl] ++ record(Name)) of
        [] ->
            Record = record_object(File, Name);
        [Record] ->
            ok
    end,
    ?GRAPH:mklink(E, recref, Record),
    field_references(Kind, Record, T);

insert(_,_,_,_,_) ->
    ok.

update_field_defs(Form, Record) ->
    [update_field_def(Def, ?GRAPH:data(Def), Record) ||
        Def <- ?GRAPH:path(Form, [{attr, 2}, sub])].

update_field_def(Node, #expr{kind=match_expr}, Record) ->
    [Left, Right] = ?GRAPH:path(Node, [sub]),
    D = ?GRAPH:data(Left),
    #expr{value=Name} = D,
    ?GRAPH:update(Left, D#expr{type=expr}),
    ?GRAPH:update(Node, (?GRAPH:data(Right))#expr{kind=record_field}),
    field_object(Record, Name, Left);

update_field_def(Node, #expr{kind=atom, value=Name}, Record) ->
    field_object(Record, Name, Node).

field_references(Kind, Record, T)
  when Kind == record_update orelse Kind == record_expr ->
    FieldRefs = [hd(?GRAPH:path(RecField, [{sub, 1}])) || RecField <- T],
    lists:foreach(
      fun(FRef) ->
              #expr{kind=atom, value=FieldName} = ?GRAPH:data(FRef),
              Field = field_object(Record, FieldName),
              ?GRAPH:mklink(FRef, fieldref, Field)
      end,
      FieldRefs);

field_references(Kind, Record, T)
  when Kind == record_index orelse Kind == record_access ->
    [F] = T,
    #expr{kind=atom, value=FieldName} = ?GRAPH:data(F),
    Field = field_object(Record, FieldName),
    ?GRAPH:mklink(F, fieldref, Field).


%%% ----------------------------------------------------------------------------
%%% Remove


%% @private
remove(_, #file{}, _, Record, #form{type=attrib, tag=record}) ->
    case ?GRAPH:path(Record, [recdef]) of
        [RecObj] ->
            remove_reclink(Record, recdef, RecObj);
        D ->
            error_logger:warning_msg("Bad recdef for ~p: ~p~n", [Record, D])
    end;

remove(_, _, _, Expr, #expr{kind=Kind})
  when Kind == record_expr orelse Kind == record_index orelse
       Kind == record_update orelse Kind == record_access ->
    case ?GRAPH:path(Expr, [recref]) of
        [Record] ->
            remove_reclink(Expr, recref, Record);
        [] ->
            ok
    end;

remove(_,_,_,_,_) ->
    ok.

remove_fields(Record) ->
    Fields = ?GRAPH:path(Record, [field]),
    [[?GRAPH:rmlink(Ref, fieldref, Field) ||
         Ref <- ?GRAPH:path(Field, [{fieldref,back}])] ||
        Field <- Fields],
    [?GRAPH:rmlink(Record, field, Field) || Field <- Fields],
    [?GRAPH:delete(Field) || Field <- Fields].

remove_reclink(From, Tag, Record) ->
    ?GRAPH:rmlink(From, Tag, Record),
    Links =
        ?GRAPH:path(Record, [{recdef, back}]) ++
        ?GRAPH:path(Record, [{recref, back}]),
    case Links of
        [] ->
            case ?GRAPH:path(Record, [{record, back}]) of
                [File] ->
                    remove_fields(Record),
                    ?GRAPH:rmlink(File, record, Record),
                    ?GRAPH:delete(Record);
                M ->
                    error_logger:warning_msg("Bad record link to ~p: ~p~n",
                                             [Record, M])
            end;
        _ -> ok
    end.


%%% ----------------------------------------------------------------------------
%%% Semantical object creation


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

field_object(Record, Name) ->
    field_object(Record, Name, undefined).

field_object(Record, Name, Def) ->
    case ?GRAPH:path(Record, [{field, {name, '==', Name}}]) of
        [Field] ->
            ok;
        [] ->
            Field = ?GRAPH:create(#field{name=Name}),
            ?GRAPH:mklink(Record, field, Field)
    end,
    if
        Def =:= undefined ->
            ok;
        true ->
            ?GRAPH:mklink(Def, fielddef, Field)
    end,
    Field.
