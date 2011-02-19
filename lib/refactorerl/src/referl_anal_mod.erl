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
-vsn("$Rev: 2324 $").
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


%%% ----------------------------------------------------------------------------

%% @private
insert(File, #file{}, _, Attr, #form{type=attrib, tag=module}) ->
    case ?Graph:path(Attr, [attr]) of
        [] -> ok;
        [ModName] ->
            #expr{value=Name} = ?Graph:data(ModName),
            create_module(File, Name)
    end;

insert(_, _, _, Attr, #form{type=attrib, tag=import}) ->
    case ?Graph:path(Attr, [attr]) of
        []            -> ok;
        [_]           -> ok;
        [ModExpr | _] -> mod_name_ref(ModExpr)
    end;

insert(I, #expr{kind=infix_expr,value=':'}, _, E, #expr{}) ->
    case ?Graph:index(I, sub, E) of
        1 -> mod_name_ref(E);
        _ -> ok
    end;

insert(_,_,_,_,_) ->
    ok.

mod_name_ref(ModNameNode) ->
    case ?Graph:data(ModNameNode) of
        #expr{kind=atom, value=Name} ->
            Mod = create_module(undefined, Name),
            ?Graph:mklink(ModNameNode, modref, Mod);
        _ -> ok
    end.


%%% ----------------------------------------------------------------------------

%% @private
remove(File, #file{}, _, _, #form{type=attrib, tag=module}) ->
    case ?Graph:path(File, [moddef]) of
        [] -> ok;
        [Mod] -> remove_module(File, Mod)
    end;

remove(_, _, _, Attr, #form{type=attrib, tag=import}) ->
    case ?Graph:path(Attr, [attr]) of
        [] -> ok;
        [_] -> ok;
        [ModExpr | _] -> del_modref(ModExpr)
    end;

remove(_, _, _, E, #expr{kind=infix_expr,value=':'}) ->
    case ?Graph:path(E, [{sub, 1}]) of
        [ModRef] -> del_modref(ModRef);
        _        -> ok
    end;

remove(_,_,_,_,_) ->
    ok.

del_modref(ModRef) ->
    case ?Graph:path(ModRef, [modref]) of
        [Mod] ->
            ?Graph:rmlink(ModRef, modref, Mod),
            remove_module(undefined, Mod);
        _ -> ok
    end.


%%% ----------------------------------------------------------------------------

create_module(File, Name) ->
    case ?Graph:path(?Graph:root(), [{module, {name, '==', Name}}]) of
        [Mod] -> ok;
        [] ->
            Mod = ?Graph:create(#module{name=Name}),
            ?Graph:mklink(?Graph:root(), module, Mod)
    end,
    case File of
        undefined -> ok;
        _         -> ?Graph:mklink(File, moddef, Mod)
    end,
    Mod.

remove_module(File, Mod) ->
    case File of
        undefined -> ok;
        _         -> ?Graph:rmlink(File, moddef, Mod)
    end,
    case ?Graph:path(Mod, [{modref, back}]) ++ 
        ?Graph:path(Mod, [{moddef, back}]) ++
        ?Graph:path(Mod, [func]) of
        [] ->
            ?Graph:rmlink(?Graph:root(), module, Mod),
            ?Graph:delete(Mod);
        _ -> ok
    end.
