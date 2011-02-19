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

%%% @doc Sorts the modules according to their consisting directories, thus
%%% the modules which are in the same directory will be in the same group.
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_dir_sort).
-vsn("$Rev: 5676 $ ").
-export([sort/0, sort/1]).

-include("user.hrl").

%%% ============================================================================
%%% Sorting queries

%% @spec sort() -> Sorted | Error
%%           Sorted = [{Directory, Modules}]
%%           Directory = string()
%%           Modules = [atom()]
%%
%%           Error = {error, no_modules_in_database}
%%                 | {error, no_list_given}
%%                 | {error, bad_list}
%% @doc Equivalent to {@link sort/1}, where the parameter is each module
%% in the graph.
sort() ->
    case ?Query:exec(?Mod:all()) of
        []      -> {error, no_modules_in_database};
        ModList -> sort(ModList)
    end.

%% @spec sort(ParList)-> Sorted | Error
%%           Sorted = [{Directory, Modules}]
%%           Directory = string()
%%           Modules = [atom()]
%%
%%           ParList = [Module]
%%           Module = node() | atom()
%%
%%           Error = {error, no_list_given}
%%                 | {error, bad_list}
%% @doc Sorts the modules (which can be given either as nodes or with
%% their names as atoms) according to their directories. Those which do
%% not belong to any of the directories will be put in a group named
%% "Other".
%% The output looks like the following:
%% <br/> [{"DirName1", [Modules1]},
%% <br/>  {"DirName2", [Modules2]}
%% <br/>  ...
%% <br/>  {"Other", [ModulesN]}]
sort([])->
    {error, no_list_given};
sort(Mods) ->
    try
        dir_sort([module_node(M) || M <- Mods])
    catch
        throw: _ -> {error, bad_list}
    end.

module_node(Mod) when is_atom(Mod) ->
    ?Query:exec1(?Mod:find(Mod), mod_not_found);
module_node({_, _, _} = Mod) ->
    ?Graph:data(Mod),
    Mod;
module_node(_) ->
    throw(badarg).

%%% ============================================================================
%%% Building list

dir_sort(ModList)->merge(mod_group(ModList)).

mod_group(ModList)->
    [{Group, ?Mod:name(Mod)}
	  || Mod <- ModList, 
	     Group <- groupname(?Query:exec(Mod, ?Mod:file()))].

groupname([])     -> ["Other"];
groupname(Files) -> [filename:dirname(?File:path(File)) || File <- Files].



merge([]) -> [];
merge([{FilePath, _} | Rest] = List) ->
    SameDir = [{Path, Mod} || {Path, Mod} <- List, FilePath == Path],
    [{FilePath, [Mod || {_, Mod} <- SameDir]}
     | merge(Rest -- SameDir)].


