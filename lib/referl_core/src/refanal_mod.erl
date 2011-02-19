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

%%% @doc Module definition analyser.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refanal_mod).
-vsn("$Rev$"). % for emacs "
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @private
schema() ->
    [{module, record_info(fields, module), []},
     {root,   [{module, module}]},
     {file,   [{moddef, module}]},
     {clause, [{modctx, module}]}
    ].

%%% @private
externs(_) -> [].

%%% @private
insert(Parent, _, {_, Child}, _) ->
    case ?Anal:data(Child) of
        #form{type=module, tag=Name} ->
            ?NodeSync:move_refs(module, [exp, func, ref, def, ctx, imp],
                                Parent, Name);
        #form{type=func} ->
            [?NodeSync:add_ref(module, {ctx, Cl}, Parent) ||
                {funcl, Cl} <- ?Anal:children(Child)],
            ok;
        #clause{type=fundef} ->
            [File] = ?Graph:path(Parent, [{form, back}]),
            ?NodeSync:add_ref(module, {ctx, Child}, File);
        _ -> ok
    end.

%%% @private
remove(Parent, _, {_, Child}, _) ->
    case ?Anal:data(Child) of
        #form{type=module, tag=Name} ->
            ?NodeSync:move_refs(module, [exp, func, ref, def, ctx, imp],
                                Name, Parent),
            ok;
        #form{type=func} ->
            [?NodeSync:del_ref(module, {ctx, Cl}, Parent) ||
                {funcl, Cl} <- ?Anal:children(Child)],
            ok;
        #clause{type=fundef} ->
            [File] = ?Graph:path(Parent, [{form, back}]),
            ?NodeSync:del_ref(module, {ctx, Child}, File);
        _ -> ok
    end.

%%% @private
update(Form, #form{type=module, tag=Name}) ->
    File = ?Anal:parent(Form),
    ?NodeSync:move_refs(module, [exp, func, ref, ctx, imp, def], File, Name),
    [ModLex] = ?Graph:path(Form, [{flex, 4}]),
    ?Syn:update_lex_with_text(ModLex, Name),
    ok;
update(_, _) ->
    ok.


smoke_test() ->
    F = ?ESG:create(#file{type=module, path="alma.erl"}),
    M = ?Syn:create(#form{type=module}, ["-", "module", "(", "alma"]),
    ?ESG:insert(?ESG:root(), file, F),
    ?ESG:insert(F, form, M),
    ?ESG:finalize(),
    [Mod] = ?ESG:path(?ESG:root(), [{module, {name, '==', alma}}]),
    ?assertMatch([F], ?ESG:path(Mod, [{moddef, back}])),

    ?FileMan:add_text(F, last, "f()->ok.\n"),
    ?ESG:finalize(),
    [N] = ?ESG:path(Mod, [{modctx, back}, name]),
    ?assertMatch(#expr{value=f}, ?ESG:data(N)),

    ?ESG:remove(F, form, M),
    ?ESG:finalize(),
    ?assertMatch([], ?ESG:path(?ESG:root(), [{module, {name, '==', alma}},
                                             {moddef, back}])),

    ?ESG:remove(?ESG:root(), file, F),
    ?ESG:finalize().
