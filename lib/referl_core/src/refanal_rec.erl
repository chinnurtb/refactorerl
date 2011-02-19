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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Record analyser.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>

-module(refanal_rec).
-vsn("$Rev $"). %% for emacs"
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").

%% TODO: do not use this header here
-define(Token, reflib_token).

%%% @private
schema() ->
    [{field,  record_info(fields, field), []},
     {typexp, [{fielddef, field}]},
     {expr,   [{fieldref, field}]},
     {record, record_info(fields, record), [{field, field}]},
     {file,   [{record, record}]},
     {form,   [{recdef, record}]},
     {expr,   [{recref, record}]}
    ].

%%% @private
externs(_) -> [].

%%% @private
insert(Parent, _Pre, {Tag, Child}, _Post) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            walk(fun add_del/4, [{Child, Parent}], add_ref);
        #form{type=record, tag=Name} ->
            File = parent_file(Parent),
            walk(fun add_del/4, [{Child, {File, Name}}], add_ref);
        #form{type=func} when Tag == funcl ->
            File = parent_file(Parent),
            walk(fun add_del/4, [{Child, File}], add_ref);
        #form{} -> ok;
        #clause{} ->
            File = parent_file(Parent),
            walk(fun add_del/4, [{Child, File}], add_ref);
        #expr{type=T, value = RName} when T == record_update orelse
                                          T == record_expr ->
            File = file(Parent, ?Anal:data(Parent)),
            walk(fun add_del/4, [{Child, {File, RName}}], add_ref);
        #expr{type=record_index, value = RName} ->
            File = file(Parent, ?Anal:data(Parent)),
            #expr{value = FName} = ?Anal:data(Child),
            ?NodeSync:add_ref(field, {ref, Child}, {{File, RName}, FName});
        #expr{type=record_access, value = RName} ->
            File = file(Parent, ?Anal:data(Parent)),
            case ?Anal:data(Child) of
                #expr{type=atom, value=FName} ->
                    ?NodeSync:add_ref(field, {ref,Child}, {{File,RName},FName});
                _ ->
                    walk(fun add_del/4, [{Child, File}], add_ref)
            end;
        #expr{type=field_list} ->
            Rec = ?Anal:parent(Parent),
            %% TODO eliminate: #expr{value = Name} = ?Anal:data(Rec),
            [Lex] = ?Graph:path(Rec, [{elex,2}]),
            RName = ?Token:get_value(Lex),
            %% TODO
            File = file(Parent, ?Anal:data(Parent)),
            walk(fun add_del/4, [{Child, {File, RName}}], add_ref);
        #expr{} ->
            File = file(Parent, ?Anal:data(Parent)),
            walk(fun add_del/4, [{Child, File}], add_ref);
        #typexp{} ->
            RecForm = ?Anal:parent(Parent),
            #form{tag=RName} = ?Anal:data(RecForm),
            File = file(Parent, ?Anal:data(Parent)),
            walk(fun add_del/4, [{Child, {File, RName}}], add_ref)
    end.


add_del(Dir, #form{type=record, tag=Name}, Form, File)->
    ?NodeSync:Dir(rec, {def, Form}, {File, Name}),
    Children = ?Anal:children(Form),
    [{Field, {File, Name}} || {_, Field} <- Children];
add_del(_Dir, #form{type=func}, Form, File) ->
    [{Cl, File} || {funcl, Cl} <- ?Anal:children(Form)];
add_del(_Dir, #form{}, _Form, _File) ->
    [];

add_del(_Dir, #clause{}, Clause, File) ->
    [{Ch, File} || {_, Ch} <- ?Anal:children(Clause)];

add_del(Dir, #expr{type=record_expr, value = Name}, Expr, File) ->
    ?NodeSync:Dir(rec, {ref, Expr}, {File, Name}),
    [{Fld, {File, Name}} || {_, Fld} <- ?Anal:children(Expr)]; %% FldSpec
add_del(Dir, #expr{type=record_index, value = Name}, Expr, File) ->
    ?NodeSync:Dir(rec, {ref, Expr}, {File, Name}),
    [{esub, Fld}] = ?Anal:children(Expr), %% EAtom
    #expr{value = FldName} = ?Anal:data(Fld),
    ?NodeSync:Dir(field, {ref, Fld}, {{File, Name}, FldName}),
    [];
add_del(Dir, #expr{type=record_update, value = Name}, Expr, File) ->
    ?NodeSync:Dir(rec, {ref, Expr}, {File, Name}),
    [{esub, RecExpr}, {esub, Fld}] = ?Anal:children(Expr), %% ExpMax, FldSpec
    [{RecExpr, File}, {Fld, {File, Name}}];
add_del(Dir, #expr{type=record_access, value=Name}, Expr, File) ->
    ?NodeSync:Dir(rec, {ref, Expr}, {File, Name}),
    [{esub, RecExpr}, {esub, Fld}] = ?Anal:children(Expr), %% ExpMax, EAtom
    #expr{value = FldName} = ?Anal:data(Fld),
    ?NodeSync:Dir(field, {ref, Fld}, {{File, Name}, FldName}),
    [{RecExpr, File}];
add_del(_Dir, #expr{type=field_list}, Expr, {File, RecName}) ->
   [{Fld, {File, RecName}} || {esub, Fld} <- ?Anal:children(Expr)];
add_del(Dir, #expr{type=record_field, value = Name}, Expr, {File, RecName}) ->
   ?NodeSync:Dir(field, {ref, Expr}, {{File, RecName}, Name}),
   [{Ch, File} || {_, Ch} <- ?Anal:children(Expr)];
add_del(_Dir, #expr{type=record_joker_field}, Expr, {File, _RecName}) ->
   %% todo: be kell kotni valahova?
   [{Ch, File} || {_, Ch} <- ?Anal:children(Expr)];
add_del(_Dir, #expr{}, Expr, File)->
   [{Ch, File} || {_, Ch} <- ?Anal:children(Expr)];

add_del(Dir, #typexp{tag=Name}, TypExp, {File, RecName})->
    ?NodeSync:Dir(field, {def, TypExp}, {{File, RecName}, Name}),
    [{Ch, File} || {texpr, Ch} <- ?Anal:children(TypExp)]. %%;

%% add_del(_, _, _, _) ->
%%     [].

%%% @private
remove(Parent, _Pre, {Tag, Child}, _Post) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            walk(fun add_del/4, [{Child, Parent}], del_ref);
        #form{type=record, tag=Name} ->
            File = parent_file(Parent),
            walk(fun add_del/4, [{Child, {File, Name}}], del_ref);
        #form{type=func} when Tag == funcl ->
            File = parent_file(Parent),
            walk(fun add_del/4, [{Child, File}], del_ref);
        #form{} ->
             ok;

        #clause{} ->
            File = parent_file(Parent),
            walk(fun add_del/4, [{Child, File}], del_ref);

        #expr{type=T, value = RName} when T == record_update orelse
                                           T == record_expr ->
            File = file(Parent, ?Anal:data(Parent)),
            walk(fun add_del/4, [{Child, {File, RName}}], del_ref);
        #expr{type=record_index, value = RName} ->
            File = file(Parent, ?Anal:data(Parent)),
            #expr{value = FName} = ?Anal:data(Child),
            ?NodeSync:del_ref(field, {ref, Child}, {{File, RName}, FName});
        #expr{type=record_access, value = RName} ->
            File = file(Parent, ?Anal:data(Parent)),
            case ?Anal:data(Child) of
                #expr{type=atom, value=FName} ->
                    ?NodeSync:del_ref(field, {ref,Child}, {{File,RName},FName});
                _ ->
                    walk(fun add_del/4, [{Child, File}], del_ref)
            end;
        #expr{type=field_list} ->
            Rec = ?Anal:parent(Parent),
            %% TODO eliminate: #expr{value = Name} = ?Anal:data(Rec),
            [Lex] = ?Graph:path(Rec, [{elex,2}]),
            RName = ?Token:get_value(Lex),
            %% TODO
            File = file(Parent, ?Anal:data(Parent)),
            walk(fun add_del/4, [{Child, {File, RName}}], del_ref);
        #expr{} ->
            File = file(Parent, ?Anal:data(Parent)),
            walk(fun add_del/4, [{Child, File}], del_ref);

        #typexp{} ->
            RecForm = ?Anal:parent(Parent),
            #form{tag=RName} = ?Anal:data(RecForm),
            File = file(Parent, ?Anal:data(Parent)),
            walk(fun add_del/4, [{Child, {File, RName}}], del_ref)
    end.

%%% @private
update(Form, #form{type=record, tag=Name})->
    File = ?Anal:parent(Form),
    ?NodeSync:move_refs(rec, [def], Form, {File, Name}),
    [Lex] = ?Graph:path(Form, [{flex, 4}]),
    ?Syn:update_lex_with_text(Lex, Name),
    ok;
update(_,_) -> ok.


walk(Fun, [{Node, Parent} | Tail], Dir) ->
    walk(Fun, Fun(Dir, ?Anal:data(Node), Node, Parent) ++ Tail, Dir);
walk(_, [], _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parent_file(Node) ->
    P = ?Anal:parent(Node),
    case ?Graph:class(P) of
        file -> P;
        _    -> parent_file(P)
    end.

file(Expr, #expr{role=attr})->
    [File] = ?Graph:path(Expr, [top, {texpr, back}, {tattr, back},
                                  {form, back}, {incl, back}]) ++
             ?Graph:path(Expr, [top, {eattr, back}, {form, back}]),
    File;
file(Expr, #expr{}) ->
    parent_file(Expr);
%%     FunCl = get_funcl(Expr),
%%     [File] = ?Graph:path(FunCl, [{funcl, back}, {form, back}]),
%%     File;
file(TypE, #typexp{}) ->
    [File] = ?Graph:path(TypE, [{tattr, back}, {form, back}, {incl, back}]),
    File.

%% file(Cl, #clause{}) ->
%%     [File] = ?Graph:path(Cl, [functx, {funcl, back}, {form, back}]),
%%     File;
%% file(Form, #form{}) ->
%%     [File] = ?Graph:path(Form, [{form, back}]),
%%     File.

%% get_funcl(Expr) ->
%%     [FunCl] = ?Graph:path(Expr, [top, {body, back}, functx]) ++
%%               ?Graph:path(Expr, [top, {guard, back}, functx]) ++
%%               ?Graph:path(Expr, [top, {pattern, back}, functx]), %% ++
%% %%    ?Graph:path(Expr, [top, {name, back}, functx]),
%%     FunCl.

