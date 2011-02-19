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
-vsn("$Rev: 2693 $").
-behaviour(referl_esg).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @private
record(Name) -> [{record, {name, '==', Name}}].

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

%% Record definition

%% @private
insert(File, #file{}, _, Record, #form{type=attrib, tag=record}) ->
    #expr{value=Name} = ?Graph:data(hd(?Graph:path(Record, [{attr, 1}]))),
    record_def(File, Record, Name);

%% Record name in definition

insert(Form, #form{type=attrib, tag=record}, attr,
       _, #expr{kind=atom, value=Name}) ->

    [File] = ?Graph:path(Form, [{form, back}]),
    case ?Graph:path(Form, [recdef]) of
        [] ->
            record_def(File, Form, Name);
        [Record] ->
            case ?Rec:name(Record) of
                Name ->
                    ok;
                _OldName ->
                    Fields = ?Graph:path(Record, [field]),
                    [remove_fieldlink(Def, fielddef, Field)
                     || Field <- Fields,
                        Def <- ?Graph:path(Field, [{fielddef,back}])],
                    remove_reclink(Form, recdef, Record),
                    record_def(File, Form, Name)
            end
    end;

%% Complete record expression

insert(_,_,_, RecRef, #expr{kind=Kind})
  when Kind == record_expr orelse Kind == record_index orelse
       Kind == record_update orelse Kind == record_access ->
    {NameExpr, Rest} = record_ref_params(RecRef, Kind),
    Record = record_reference(RecRef, NameExpr),
    field_references(Kind, Record, Rest);

%% Record name in record expression
%% Record field index/access

insert(RecRef, #expr{kind=Kind}, sub, NameExpr, #expr{value=Name})
  when Kind == record_expr orelse Kind == record_index orelse
       Kind == record_update orelse Kind == record_access ->

    NameIndex = case Kind of
                    record_expr   -> 1; record_index  -> 1;
                    record_update -> 2; record_access -> 2
                end,
    FieldIndex = case Kind of
                     record_expr   -> 0; record_index  -> 2;
                     record_update -> 0; record_access -> 3
                end,
    case ?Syn:index(RecRef, sub, NameExpr) of
        NameIndex ->
            case ?Graph:path(RecRef, [recref]) of
                [] ->
                    {_, Rest} = record_ref_params(RecRef, Kind),
                    NewRecord = record_reference(RecRef, NameExpr),
                    field_references(Kind, NewRecord, Rest);
                [Record] ->
                    case ?Rec:name(Record) of
                        Name ->
                            ok;
                        _ ->
                            Subs = ?Graph:path(RecRef, [{sup, back}]) ++
                                ?Graph:path(RecRef, [sub]) ++
                                ?Graph:path(RecRef, [sub, sub]),
                            Fields = ?Graph:path(Record, [field]),
                            [remove_fieldlink(Ref, fieldref, Field)
                             || Field <- Fields,
                                Ref <- ?Graph:path(Field, [{fieldref,back}]),
                                lists:member(Ref, Subs)],
                            remove_reclink(RecRef, recref, Record),
                            {_, Rest} = record_ref_params(RecRef, Kind),
                            NewRecord = record_reference(RecRef, NameExpr),
                            field_references(Kind, NewRecord, Rest)
                    end
            end;
        FieldIndex ->
            [Record] = ?Graph:path(RecRef, [recref]),
            case ?Graph:path(NameExpr, [fieldref]) of
                [] ->
                    field_ref(NameExpr, Record, Name);
                [Field] ->
                    case (?Graph:data(Field))#field.name of
                            Name ->
                            ok;
                        _ ->
                            remove_fieldlink(NameExpr, fieldref, Field),
                            field_ref(NameExpr, Record, Name)
                    end
            end;
        _ ->
            ok
    end;

%% Name in record field expression
%% (record definition, record expression, record update)

insert(FieldExpr, #expr{kind=record_field}, sub, E, #expr{value=Name}) ->
    [Parent] = ?Graph:path(FieldExpr, [{sub, back}]),
    Index = ?Syn:index(FieldExpr, sub, E),
    case (?Graph:data(Parent))#expr.kind of
        tuple ->
            [Record] = ?Graph:path(FieldExpr, [sup, {attr, back}, recdef]),
            case Index of
                1 ->
                    case ?Graph:path(E, [fielddef]) of
                        [] ->
                            field_object(Record, Name, E);
                        [Field] ->
                            case (?Graph:data(Field))#field.name of
                                Name ->
                                    ok;
                                _ ->
                                    remove_fieldlink(E, fielddef, Field),
                                    field_object(Record, Name, E)
                            end
                    end;
                _ ->
                    ok
            end;
        _ ->
            [Record] = ?Graph:path(FieldExpr, [{sub, back}, recref]),
            case Index of
                1 ->
                    case ?Graph:path(E, [fieldref]) of
                        [] ->
                            field_ref(E, Record, Name);
                        [Field] ->
                            case (?Graph:data(Field))#field.name of
                                Name ->
                                    ok;
                                _ ->
                                    remove_fieldlink(E, fieldref, Field),
                                    field_ref(E, Record, Name)
                            end
                    end;
                _ ->
                    ok
            end
    end;

insert(Tuple, #expr{kind=tuple}, sub, NameExpr, #expr{kind=atom, value=Name}) ->
    case ?Graph:path(Tuple, [{attr, back}, recdef]) of
        [Record] ->
            case ?Graph:path(NameExpr, [fielddef]) of
                [] ->
                    field_object(Record, Name, NameExpr);
                [Field] ->
                    case (?Graph:data(Field))#field.name of
                        Name ->
                            ok;
                        _ ->
                            remove_fieldlink(NameExpr, fielddef, Field),
                            field_object(Record, Name, NameExpr)
                    end
            end;
        [] ->
            ok
    end;

insert(_,_,_,_,_) ->
    ok.

record_ref_params(RecRef, Kind) ->
    Subs = ?Graph:path(RecRef, [sub]),
    case Kind of
        record_expr   -> [   NameExpr | Rest] = Subs;
        record_index  -> [   NameExpr | Rest] = Subs;
        record_update -> [_, NameExpr | Rest] = Subs;
        record_access -> [_, NameExpr | Rest] = Subs
    end,
    {NameExpr, Rest}.

record_def(File, Form, Name) ->
    case ?Graph:path(File, record(Name)) of
        [] ->
            update_field_defs(Form, record_object(File, Name, Form));
        _  ->
            record_object(File, Name, Form)
    end.

record_reference(RecordExpr, NameExpr) ->
    #expr{kind=atom, value=Name} = ?Graph:data(NameExpr),
    [File] = ?Syn:get_file(RecordExpr),
    Record = record_object(File, Name),
    ?Graph:mklink(RecordExpr, recref, Record),
    Record.

update_field_defs(Form, Record) ->
    [update_field_def(Def, ?Graph:data(Def), Record) ||
        Def <- ?Graph:path(Form, [{attr, 2}, sub])].

update_field_def(Node, #expr{kind=record_field}, Record) ->
    [Left, _] = ?Graph:path(Node, [sub]),
    #expr{value=Name} = ?Graph:data(Left),
    field_object(Record, Name, Left);

update_field_def(Node, #expr{kind=match_expr}, Record) ->
    [Left, Right] = ?Graph:path(Node, [sub]),
    #expr{value=Name} = D = ?Graph:data(Left),
    ?Graph:update(Left, D#expr{type=expr}),
    ?Graph:update(Node, (?Graph:data(Right))#expr{kind=record_field}),
    field_object(Record, Name, Left);

update_field_def(Node, #expr{kind=atom, value=Name}, Record) ->
    field_object(Record, Name, Node).

field_references(Kind, Record, T)
  when Kind == record_update orelse Kind == record_expr ->
    FieldRefs = [FieldName || RecField <- T,
                              FieldName <- ?Graph:path(RecField, [{sub, 1}]),
                              #expr{kind=atom} <- [?Graph:data(FieldName)]],
    lists:foreach(
      fun(FRef) ->
              #expr{kind=atom, value=FieldName} = ?Graph:data(FRef),
              field_ref(FRef, Record, FieldName)
              %% TODO: links to wildcard field refs
      end,
      FieldRefs);

field_references(Kind, Record, T)
  when Kind == record_index orelse Kind == record_access ->
    [F] = T,
    #expr{kind=atom, value=FieldName} = ?Graph:data(F),
    field_ref(F, Record, FieldName).

field_ref(Expr, Record, FieldName) ->
    Field = field_object(Record, FieldName),
    ?Graph:mklink(Expr, fieldref, Field).

%%% ----------------------------------------------------------------------------
%%% Remove


%% @private
remove(_, #file{}, _, Form, #form{type=attrib, tag=record}) ->
    case ?Graph:path(Form, [recdef]) of
        [RecObj] ->
            Fields = ?Graph:path(RecObj, [field]),
            [remove_fieldlink(Def, fielddef, Field)
             || Field <- Fields,
                Def <- ?Graph:path(Field, [{fielddef,back}])],
            remove_reclink(Form, recdef, RecObj);
        D ->
            error_logger:warning_msg("Bad recdef for ~p: ~p~n", [Form, D])
    end;

remove(_, _, _, Expr, #expr{kind=Kind})
  when Kind == record_expr orelse Kind == record_index orelse
       Kind == record_update orelse Kind == record_access ->
    case ?Graph:path(Expr, [recref]) of
        [RecObj] ->
            Fields = ?Graph:path(RecObj, [field]),
            Subs = ?Graph:path(Expr, [sub]) ++ ?Graph:path(Expr, [sub, sub]),
            [remove_fieldlink(Ref, fieldref, Field)
             || Field <- Fields,
                Ref <- ?Graph:path(Field, [{fieldref,back}]),
                lists:member(Ref, Subs)],
            remove_reclink(Expr, recref, RecObj);
        [] ->
            ok
    end;

remove(_,_,_,_,_) ->
    ok.

remove_reclink(From, Tag, Record) ->
    ?Graph:rmlink(From, Tag, Record),
    Links =
        ?Graph:path(Record, [{recdef, back}]) ++
        ?Graph:path(Record, [{recref, back}]),
    case Links of
        [] ->
            case ?Graph:path(Record, [{record, back}]) of
                [File] ->
                    destroy_record(File, Record);
                M ->
                    error_logger:warning_msg("Bad record link to ~p: ~p~n",
                                             [Record, M])
            end;
        _ -> ok
    end.

destroy_record(File, Record) ->
    Fields = ?Graph:path(Record, [field]),
    [remove_fieldlink(Def, fielddef, Field)
     || Field <- Fields,
        Def <- ?Graph:path(Field, [{fielddef,back}])],
    [remove_fieldlink(Ref, fieldref, Field)
     || Field <- Fields,
        Ref <- ?Graph:path(Field, [{fieldref,back}])],
    ?Graph:rmlink(File, record, Record),
    ?Graph:delete(Record).

remove_fieldlink(From, Tag, Field) ->
    ?Graph:rmlink(From, Tag, Field),
    Links =
        ?Graph:path(Field, [{fielddef, back}]) ++
        ?Graph:path(Field, [{fieldref, back}]),
    case Links of
        [] ->
            case ?Graph:path(Field, [{field, back}]) of
                [Record] ->
                    ?Graph:rmlink(Record, field, Field),
                    ?Graph:delete(Field);
                M ->
                    error_logger:warning_msg("Bad field link to ~p: ~p~n",
                                             [Field, M])
            end;
        _ -> ok
    end.


%%% ----------------------------------------------------------------------------
%%% Semantical object creation


record_object(File, Name) ->
    record_object(File, Name, undefined).

record_object(File, Name, Def) ->
    case ?Graph:path(File, [incl, {record, {name, '==', Name}}]) of
        [Record] ->
            ok;
        [] ->
            Record =
                case ?Graph:path(File, [{incl, back}, {record, {name, '==', Name}}]) of
                    [R] ->
                        [FileOrigin] = ?Graph:path(R, [{record, back}]),
                        ?Graph:rmlink(FileOrigin, record, R),
                        R;
                    [] ->
                        ?Graph:create(#record{name=Name})
                end,
            ?Graph:mklink(File, record, Record)
    end,
    if
        Def =:= undefined ->
            ok;
        true ->
            ?Graph:mklink(Def, recdef, Record)
    end,
    Record.

field_object(Record, Name) ->
    field_object(Record, Name, undefined).

field_object(Record, Name, Def) ->
    case ?Graph:path(Record, [{field, {name, '==', Name}}]) of
        [Field] ->
            ok;
        [] ->
            Field = ?Graph:create(#field{name=Name}),
            ?Graph:mklink(Record, field, Field)
    end,
    if
        Def =:= undefined ->
            ok;
        true ->
            ?Graph:mklink(Def, fielddef, Field)
    end,
    Field.
