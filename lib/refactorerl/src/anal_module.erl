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

-module(anal_module).
-vsn("$Rev: 1247 $").
-behaviour(refac_anal).

%% Interface exports
-export([file_module/0, module/1, referred_mod/0]).

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").

%% @spec file_module() -> path()
%% @doc Path from a #file{} to the #module{} defined by the file.
file_module() ->
    [moddef].

%% @spec module(atom()) -> path()
%% @doc Path from #root{} to the #module{} with name `Name'.
module(Name) ->
    [{module, {name, '==', Name}}].

%% @spec referred_mod() -> path()
%% @doc Path from an #expr{} to the #module{} it refers.
referred_mod() ->
    [modref].

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
        [] -> ok;
        [ModExpr | _] ->
            #expr{value=ModName} = ?GRAPH:data(ModExpr),
            Mod = create_module(undefined, ModName),
            ?GRAPH:mklink(ModExpr, modref, Mod)
    end;

insert(_, _, _, MQ, #expr{kind=module_qualifier}) ->
    [ModName] = ?GRAPH:path(MQ, [{sub, 1}]),
    case ?GRAPH:data(ModName) of
        #expr{kind=atom, value=Name} ->
            Mod = create_module(undefined, Name),
            ?GRAPH:mklink(MQ, modref, Mod);
        _ -> ok
    end;

insert(_,_,_,_,_) ->
    ok.


%% @private
remove(File, #file{}, _, _, #form{type=attrib, tag=module}) ->
    case ?GRAPH:path(File, [moddef]) of
        [] -> ok;
        [Mod] -> remove_module(File, Mod)
    end;

remove(_, _, _, Attr, #form{type=attrib, tag=import}) ->
    case ?GRAPH:path(Attr, [attr]) of
        [] -> ok;
        [ModExpr | _] ->
            case ?GRAPH:path(ModExpr, [modref]) of
                [Mod] ->
                    ?GRAPH:rmlink(ModExpr, modref, Mod),
                    remove_module(undefined, Mod);
                _ -> ok
            end
    end;  

remove(_, _, _, MQ, #expr{kind=module_qualifier}) ->
    case ?GRAPH:path(MQ, [modref]) of
        [Mod] ->
            ?GRAPH:rmlink(MQ, modref, Mod),
            remove_module(undefined, Mod);
        _ -> ok
    end;

remove(_,_,_,_,_) ->
    ok.


create_module(File, Name) ->
    case ?GRAPH:path(?GRAPH:root(), [{module, {name, '==', Name}}]) of
        [Mod] -> ok;
        [] ->
            Mod = ?GRAPH:create(#module{name=Name}),
            ?GRAPH:mklink(?GRAPH:root(), module, Mod)
    end,
    case File of
        undefined -> ok;
        _ -> ?GRAPH:mklink(File, moddef, Mod)
    end,
    Mod.

remove_module(File, Mod) ->
    case File of
        undefined -> ok;
        _ -> ?GRAPH:rmlink(File, moddef, Mod)
    end,
    case ?GRAPH:path(Mod, [{modref, back}]) of
        [] ->
            ?GRAPH:rmlink(?GRAPH:root(), module, Mod),
            ?GRAPH:delete(Mod);
        _ -> ok
    end.
