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

%%% @doc This module provides functions that help to calculate the attribute
%%% matrix and to filter this matrix.
%%% 
%%% Two kinds of attribute matrix can be created using the module: the module
%%% attribute matrix and the function attribute matrix. In the first, the
%%% entities are modules. In the second, the entities are functions.
%%% The attributes are functions, records and macros in both cases.
%%% The elements of the matrix are numbers. An element describes how many times
%%% was the attribute used by the entity.
%%%
%%% The module attribute matrix can be created by using the
%%% {@link mod_empty_attrib/0} and {@link mod_attrib_data/0} functions:
%%% ```
%%%  Attribs = cl_core:attribs(cl_attr:mod_attrib_data(),
%%%                            cl_attr:mod_empty_attrib()).
%%% '''
%%% The function attribute matrix can be created by using the
%%% {@link fun_empty_attrib/0} and {@link fun_attrib_data/0} functions:
%%% ```
%%%  Attribs = cl_core:attribs(cl_attr:fun_attrib_data(),
%%%                            cl_attr:fun_empty_attrib()).
%%% '''
%%% All these four functions work on the database which is stored by the
%%% Refactorerl application.
%%%
%%% The filter functions {@link library_mod/2} and {@link internal_fun/2} can be
%%% used with the `core:filter/3' function:
%%% ```
%%% Filtered = cl_core:filter(Attribs, 
%%%                           [fun cl_attr:library_mod/2],
%%%                           [fun cl_attr:internal_fun/2]).
%%% '''
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_attr).
-vsn("$Rev: 1489 $").

-export([mod_empty_attrib/0, mod_attrib_data/0, fun_empty_attrib/0,
         fun_attrib_data/0]).

%% Filter functions
-export([library_mod/2, internal_fun/2]).
-export([attribs_to_deps/1, attribs_to_uses/1, get_library_modules/2]).
-export([fun_to_fun_attr/1, rec_to_rec_attr/1, macro_to_macro_attr/1]).

-include("cluster.hrl").

%%% @type mod_name() = atom().
%%%
%%% The name of a module.

%%% @type ref_adder_fun() =
%%%           (Fun::fun_attr(), Object::term(), Count::integer(),
%%%            D::dictionary()) ->  dictionary().
%%%
%%% Updates the `D' dictionary with the information that
%%%          "`Fun' referred to `Object' `Count' times."

%% @spec mod_empty_attrib() -> integer()
%%
%% @doc Returns the default value of the module attribute matrix.
mod_empty_attrib() -> 0.

%% @spec fun_empty_attrib() -> integer()
%%
%% @doc Returns the default value of the function attribute matrix.
fun_empty_attrib() -> 0.

%% @spec mod_attrib_data() -> [{mod_name(), [{Attrib, integer()}]}]
%%
%% @doc Returns the module attribute matrix of the files which are in the
%% database.
mod_attrib_data() ->
    Empty = mod_empty_attrib(),
    attrib_data(
      [ (?ESG:data(Mod))#module.name ||
          Mod <- ?ESG:path(?ESG:root(), [file, moddef])],
      [{[funref],
        fun(#fun_attr{mod=Mod}, Fun2, Count, D) ->
                add_ref_to_dict(Mod, fun_to_fun_attr(Fun2), Count, D, Empty)
        end},
       {[recref],
        fun(#fun_attr{mod=Mod}, Record, Count, D) ->
                add_ref_to_dict(Mod, rec_to_rec_attr(Record), Count, D, Empty)
        end},
       {[elex, mref],
        fun(#fun_attr{mod=Mod}, Macro, Count, D) ->
                add_ref_to_dict(Mod, macro_to_macro_attr(Macro), Count, D,Empty)
        end}]).

%% @spec fun_attrib_data() -> [{fun_attr(), [{Attrib, integer()}]}]
%%
%% @doc Returns the function attribute matrix of the files which are
%% in the database.
fun_attrib_data()->
    Empty = fun_empty_attrib(),
    attrib_data(
      [ fun_to_fun_attr(Fun) ||
          Fun <- ?ESG:path(?ESG:root(), [file, moddef, func])],
      [{[funref],
        fun(Fun1, Fun2, Count, D) ->
                add_ref_to_dict(Fun1, fun_to_fun_attr(Fun2), Count, D, Empty)
        end},
       {[recref],
        fun(Fun1, Record, Count, D) ->
                add_ref_to_dict(Fun1, rec_to_rec_attr(Record), Count, D, Empty)
        end},
       {[elex, mref],
        fun(Fun1, Macro, Count, D) ->
                add_ref_to_dict(Fun1, macro_to_macro_attr(Macro), Count,D,Empty)
        end}]).

%% @spec attrib_data([entity()], [{[atom()], ref_adder_fun()}]) -> 
%%           [{entity(), [{Attrib, integer()}]}]
%%
%% @doc Returns an attribute matrix which is based on reference numbers
%% (how many times is an attribute referenced in an entity).
%% `Entities' will be the entities of the matrix.
%% Elements of `RefAdders' specify the references and the functions that will
%% add these references to the matrix.
%% These elements are `{PathEnd, RefAdderFun}' tuples.
%% The `attrib_data' function finds the objects specified by `PathEnd' and adds
%% their references to the matrix using the correspondent` RefAdderFun'.
attrib_data(Entities, RefAdders) ->
    Funs = ?ESG:path(?ESG:root(), [file, moddef, func]),

    %% initial dictionary (which represents the attribute matrix)
    D1 = dict:from_list([{Entity, dict:new()} || Entity <- Entities ]),

    %% add the references to the dictionary according to RefAdders
    D2 = lists:foldl(
           fun({PathEnd, RefAdderFun}, D) ->
                   add_refs(Funs, PathEnd, D, RefAdderFun)
           end,
           D1,
           RefAdders),

    [{Entity, [{entities, [Entity]}, {size, 1}]++dict:to_list(Attribs)} || 
        {Entity, Attribs} <- dict:to_list(D2)].

%% @spec add_refs([Fun], [term()], dictionary(), ref_adder_fun()) ->
%%           dictionary()
%%
%% @doc Counts the references in the functions to the given kind of objects, and
%% adds this information to the `Dict'.
%% `AdderFun' is used to add the new reference to the `Dict'.
add_refs(Funs, PathEnd, Dict, AdderFun) ->
    Path = [{fundef, back}, funcl, {scope, back}, visib, {sup, back}] 
        ++ PathEnd,
    References =
        [{fun_to_fun_attr(Fun), Object, Count} ||
            Fun <- Funs,
            {Object, Count} <- count(?ESG:path(Fun, Path))],

    lists:foldl(
      fun({Fun, Object, Count}, D) ->
              AdderFun(Fun, Object, Count, D)
      end, Dict, References).

%% @spec add_ref_to_dict(atom(), term(), integer(), dictionary(), term()) ->
%%           dictionary()
%%
%% @doc Adds the information to the `Dict' that
%% "`Entity' references to `Object' `Count' times".
add_ref_to_dict(Entity, Object, Count, Dict, EmptyAttrib) ->

    %% EntityAttributes: Attr->Value dictionary
    EntityAttributes = case dict:is_key(Entity, Dict) of
                           true -> dict:fetch(Entity, Dict);
                           false -> dict:new()
                       end,

    Out = case dict:is_key(Object, EntityAttributes) of
              true -> dict:fetch(Object, EntityAttributes);
              false -> EmptyAttrib
          end,

    EntityAttributesNew = dict:store(Object, Out+Count, EntityAttributes),
    dict:store(Entity, EntityAttributesNew, Dict).

%% @spec fun_to_fun_attr(node()) -> #fun_attr{}
%%
%% @doc Returns the fun_attr record of the `Fun' function node.
fun_to_fun_attr(Fun) ->
    #func{name=Name, arity=Arity} = ?ESG:data(Fun),
    [Mod] = ?ESG:path(Fun, [{func, back}]),
    #module{name=ModName} = ?ESG:data(Mod),
    #fun_attr{mod=ModName, name=Name, arity=Arity}.

%% @spec rec_to_rec_attr(node()) -> #rec_attr{}
%% 
%% @doc Returns the rec_attr record of the `Record' record node.
rec_to_rec_attr(Record) ->
    #record{name=Name} = ?ESG:data(Record),
    [File] = ?ESG:path(Record, [{record, back}]),
    #file{path=Path} = ?ESG:data(File),
    #rec_attr{file=Path, name=Name}.

%% @spec macro_to_macro_attr(node()) -> #macro_attr{}
%%
%% @doc Returns the macro_attr record of the `Macro' macro node.
macro_to_macro_attr(Macro) ->
    #macro{name=Name} = ?ESG:data(Macro),
    [File] = ?ESG:path(Macro, [{macro, back}]),
    #file{path=Path} = ?ESG:data(File),
    #macro_attr{file=Path, name=Name}.

%% @spec count([term()]) -> [{term(), integer()}]
%%
%% @doc A {T, N} tuple in the result means that the term T is contained by the
%% original list N times.
count(List) -> count(lists:sort(List), []).

count([], Res) -> Res;
count([Head | Tail], [{Head, N} | Res]) ->
    count(Tail, [{Head, N+1} | Res]);
count([Head | Tail], Res) ->
    count(Tail, [{Head, 1} | Res]).


%% @spec library_mod(term(), [{Attr, Out::integer()}]) -> bool()
%%
%% @doc Entity filter that drops modules with only incoming calls.
library_mod(Module, Calls) ->
    not lists:any(
          fun ({#fun_attr{mod=Mod}, Out})
              when Mod /= Module, Out > 0 ->
                  true;
              (_) ->
                  false
          end, Calls).

%% @spec internal_fun(Attr, [{Mod::term(), Out::integer()}]) -> bool()
%%
%% @doc Attribute filter that drops module internal functions.
internal_fun(#fun_attr{mod=FunMod}, Calls) ->
    not lists:any(
          fun({Mod, Out})
             when Mod /= FunMod, Out > 0 ->
                  true;
             (_) ->
                  false
          end, Calls);

internal_fun(_, _) ->
    false.

%% @spec attribs_to_deps(attribs()) -> [{mod_name(), mod_name()}]
%%
%% @doc The returning list contains `{Mod1, Mod2}' pairs that represent that
%% `Mod1' calls `Mod2'.
%% "dep" is short for "dependency".
attribs_to_deps(Attribs) ->
    Rows = cl_matrix:rows(Attribs),
    OrdRows = ordsets:from_list(Rows),
    Deps = lists:foldl(
             fun(Mod1, Deps1) ->
                     cl_matrix:fold_row(
                       fun (#fun_attr{mod=Mod2}, N, Uses2) 
                           when N/=0, Mod1/=Mod2 ->
                               case ordsets:is_element(Mod2, OrdRows) of
                                   true -> [{Mod1, Mod2}|Uses2];
                                   false -> Uses2
                               end;
                           (_, _, Uses2) ->
                               Uses2
                       end, Deps1, Mod1, Attribs)
             end, [], Rows),
    lists:usort(Deps).

%% @spec attribs_to_uses(attribs()) -> [{mod_name(), [mod_name()]}]
%%
%% @doc The returning list contains `{Mod, Modules}' pairs that represent that
%% `Mod' uses exactly the modules that are in `Modules'.
attribs_to_uses(Attribs) ->
    Rows = cl_matrix:rows(Attribs),
    OrdRows = ordsets:from_list(Rows),
    lists:foldl(
      fun(Mod1, Uses1) ->
              cl_matrix:fold_row(
                fun (#fun_attr{mod=Mod2}, N, Uses2) when N/=0, Mod1/=Mod2 ->
                        case ordsets:is_element(Mod2, OrdRows) of
                            true ->
                                {RE, ER} = Uses2,
                                {set_dict:add(Mod1, Mod2, RE),
                                 set_dict:add(Mod2, Mod1, ER)};
                            false ->
                                Uses2
                        end;
                    (_, _, Uses2) ->
                        Uses2
                end,
                Uses1,
                Mod1,
                Attribs)
      end,
      {dict:new(), dict:new()},
      Rows).

%% @spec get_library_modules(attribs(), number()) -> [mod_name()]
%%
%% @doc The returning list contains the modules that are used by at least `N'
%% other modules.
get_library_modules(Attribs, N) ->
    {_, ER} = attribs_to_uses(Attribs),
    dict:fold(
      fun(Callee, Callers, L) ->
              case sets:size(Callers) >= N of
                  true -> [Callee|L];
                  false -> L
              end
      end, [], ER).
