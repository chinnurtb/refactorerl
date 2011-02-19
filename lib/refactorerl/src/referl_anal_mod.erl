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

%%% @doc Analyse module definitions and references. The following semantical
%%% structure is created:
%%%
%%% <ul>
%%%
%%% <li>A semantical module object is created for every referenced and/or
%%%   defined module (`#module{}').</li>
%%%
%%% <li>The module object is linked to the root object (`module').</li>
%%%
%%% <li>The source file of the module is linked to the module (`moddef').</li>
%%%
%%% <li>Every expression that refers to a module is linked to the module object
%%%   (`modref').</li>
%%%
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(referl_anal_mod).
-vsn("$Rev: 1972 $").
-behaviour(referl_esg).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @private
init() ->
    [{root,   [{module, module}]},
     {file,   [{moddef, module}]},
     {expr,   [{modref, module}]},
     {module, record_info(fields, module), []}
    ].

%% @private
insert(File, #file{}, _, Attr, #form{type=attrib, tag=module}) ->
    case ?GRAPH:path(Attr, [attr]) of
        [] -> ok;
        [ModName] ->
            #expr{value=Name} = ?GRAPH:data(ModName),
            create_module(File, Name)
    end;

insert(_, _, _, Attr, #form{type=attrib, tag=import}) ->
    case ?GRAPH:path(Attr, [attr]) of
        []            -> ok;
        [_]           -> ok;
        [ModExpr | _] -> mod_name_ref(ModExpr)
    end;

insert(_, _, _, App, #expr{kind=application}) ->
    case ?GRAPH:path(App, [{sub, {{kind, '==', infix_expr}, 'and',
                                  {value, '==', ':'}}}, {sub, 1}]) of
        [ModRef] -> mod_name_ref(ModRef);
        _        -> ok
    end;

insert(_, _, _, Fun, #expr{kind=implicit_fun}) ->
    case ?GRAPH:path(Fun, [sub]) of
        [ModRef, _FunRef, _Arity] -> mod_name_ref(ModRef);
        [_FunRef, _Arity]         -> ok
    end;

insert(_,_,_,_,_) ->
    ok.

mod_name_ref(ModNameNode) ->
    case ?GRAPH:data(ModNameNode) of
        #expr{kind=atom, value=Name} ->
            Mod = create_module(undefined, Name),
            ?GRAPH:mklink(ModNameNode, modref, Mod);
        _ -> ok
    end.

%% @private
remove(File, #file{}, _, _, #form{type=attrib, tag=module}) ->
    case ?GRAPH:path(File, [moddef]) of
        [] -> ok;
        [Mod] -> remove_module(File, Mod)
    end;

remove(_, _, _, Attr, #form{type=attrib, tag=import}) ->
    case ?GRAPH:path(Attr, [attr]) of
        [] -> ok;
        [_] -> ok;
        [ModExpr | _] -> del_modref(ModExpr)
    end;  

remove(_, _, _, App, #expr{kind=application}) ->
    case ?GRAPH:path(App, [{sub, {{kind, '==', infix_expr}, 'and',
                                  {value, '==', ':'}}}, {sub, 1}]) of
        [ModRef] -> del_modref(ModRef);
        _        -> ok
    end;

remove(_, _, _, Fun, #expr{kind=implicit_fun}) ->
    case ?GRAPH:path(Fun, [sub]) of
        [MR, _FR, _Ar] -> del_modref(MR);
        [_FR, _Ar] -> ok
    end;

remove(_,_,_,_,_) ->
    ok.

del_modref(ModRef) ->
    case ?GRAPH:path(ModRef, [modref]) of
        [Mod] ->
            ?GRAPH:rmlink(ModRef, modref, Mod),
            remove_module(undefined, Mod);
        _ -> ok
    end.

create_module(File, Name) ->
    case ?GRAPH:path(?GRAPH:root(), [{module, {name, '==', Name}}]) of
        [Mod] -> ok;
        [] ->
            Mod = ?GRAPH:create(#module{name=Name}),
            ?GRAPH:mklink(?GRAPH:root(), module, Mod)
    end,
    case File of
        undefined -> ok;
        _         -> ?GRAPH:mklink(File, moddef, Mod)
    end,
    Mod.

remove_module(File, Mod) ->
    case File of
        undefined -> ok;
        _         -> ?GRAPH:rmlink(File, moddef, Mod)
    end,
    case ?GRAPH:path(Mod, [{modref, back}]) ++ 
        ?GRAPH:path(Mod, [{moddef, back}]) of
        [] ->
            ?GRAPH:rmlink(?GRAPH:root(), module, Mod),
            ?GRAPH:delete(Mod);
        _ -> ok
    end.
