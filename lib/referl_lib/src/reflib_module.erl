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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc This module contains functions that return a queries stating
%%%      from the module and it contains two module related transformations
%%%      to delete function and add a function into an arbritary import list.

%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(reflib_module).
-vsn(" $Rev: 2224$ ").

%% =============================================================================
%% Exports

-export([name/1, find/1, all/0, file/0, locals/0, local/2,
         references/0,
         imported/2, exported/2, exports/0, imports/0, visible/2, is_imported/2,
         add_import/2,del_import/2]).
-export([find_fundef/3]).

-include("lib.hrl").


%% =============================================================================
%% Module related properties


%% @spec name(node(#module{})) -> atom()
%% @doc Returns the name of the module
name(Mod) ->
    (?Graph:data(Mod))#module.name.


%% @spec find(atom()) -> query(root(), #module{})
%% @doc This function can find a module  by `Name' (an atom).
find(Name) ->
    [{module, {name, '==', Name}}].

%% @spec all() -> query(root(), #module{})
%% @doc The query returns every module in the graph.
all() ->
    [{module, {name, '/=', []}}].

%% =============================================================================
%% Queries starting from modules


%% @spec file() -> query(#module{}, #file{})
%% @doc The result of this function is a query specifies the module
%%      definition file.
file() ->
    [{moddef, back}].

%% @spec references() -> query(#module{}, #expr{})
%% @doc The result of this function is a query specifies module
%%      references.
references()->
    [{modref, back}].

%% @spec locals() -> query(#module{}, #func{})
%% @doc The result query returns all functions defined in the module.
locals() ->
    [func].

%% @spec local(atom(), integer()) -> query(#module{}, #func{})
%% @doc The result query returns a function defined in this module.
local(Name, Ary)->
    [{func, {{name,'==', Name},'and',{arity,'==', Ary}}}].


%% @spec imported(atom(), integer()) -> query(#module{}, #func{})
%% @doc The result query returns a function that imported into this module.
imported(Name, Ary) ->
    [{funimp, {{name, '==', Name} ,'and' ,{arity,'==', Ary}}}].


%% @spec is_imported(node(#module{}), node(#func{})) -> bool()
is_imported(Mod, Fun) ->
    Name = ?Fun:name(Fun),
    Arity = ?Fun:arity(Fun),
    case ?Query:exec(Mod, imported(Name, Arity)) of
        [] -> false;
        _ ->  true
    end.


%% @spec exported(Name, Ary) -> query(#module{}, #func{})
%% @doc The result query returns a function exported from this module.
exported(Name, Ary)->
    [{funexp, {{name,'==', Name} ,'and' ,{arity,'==', Ary}}}].


%% @spec exports() -> query(#module{}, #func{})
%% @doc The result query returns the set of exported functions.
exports() ->
    [funexp].

%% @spec imports() -> query(#module{}, #func{})
%% @doc The result query returns the set of imported functions.
imports() ->
    [funimp].

%% @spec visible(Name, Ary) -> query(#module{}, #func{})
%% @doc The result query returns a function visible in this module.
visible(Name, Ary)->
    ?Query:all([{func,{{name, '==', Name}, 'and', {arity, '==', Ary}}}],
               [{funimp,{{name, '==', Name}, 'and', {arity, '==', Ary}}}]).


%% =============================================================================
%% Module related transformations

%% @spec add_import(To, What) -> ok
%%       To   = node(#file{}) | node(#module{}) | node(#form{})
%%       What = node(#fun{}) | [node(#fun{})]
%% @doc This function can add a new function or a list of functions to
%%      - an existing import list,
%%          in case all the functions belong to the same module as the import
%%              list,
%%          functions already present in the import list won't be added again;
%%      - a file or a module,
%%          in case all the functions belong to the same module,
%%          in this case a new import list will be created and added to the
%%              file or the module.
%% This function uses `ESG' queries!  `ESG:finalize/0' has to be used
%% after the transformation ends. (or use it in a new esg `batch').
%% The function does not create an import list if the given
%% list of functions is empty.

add_import(_To, []) ->
    ok;

add_import(To, Fun) when not is_list(Fun)->
    add_import(To, [Fun]);

add_import(To, FunList) ->
    case ?Syn:node_type(To) of
        file   ->
            add_import_form(To, FunList);
        module ->
            [File] = ?Query:exec(To, ?Mod:file()),
            add_import_form(File, FunList);
        form   ->
            add_import_items(To, FunList)
    end,
	ok.

add_import_form(File, FunList)->
    [Mod] = ?Query:exec(hd(FunList), ?Fun:module()),
    check_imports(Mod, FunList),
    NewFuns = sort_imports(File, FunList),

    ModName = ?Mod:name(Mod),
    ModExpr = ?Syn:create(#expr{type=atom}, [atom_to_list(ModName)]),

    NewImportItems = [ create_import_item(?Fun:name(Fun), ?Fun:arity(Fun)) ||
                         Fun <- NewFuns ],

    List =
        case NewImportItems of
            [] ->
                ?Syn:create(#expr{type=funlist}, []);
            _ ->
                ?Syn:create(#expr{type=funlist}, [{esub, NewImportItems}])
        end,

    ImportForm = ?Syn:create(#form{type=import},
                             ["-","import", {eattr, ModExpr}, {eattr, List}]),

    ?File:add_form(File, ImportForm).

add_import_items(Form, FunList)->
    [Mod] = ?Query:exec(Form, [{eattr,1},modref]),
    check_imports(Mod, FunList),

    IList = hd(?Query:exec(Form, [{eattr,2}])),

    OldFuns = ?Query:exec(IList, ?Query:seq([ ?Expr:children(),
                                                ?Expr:function() ])),
    NewFuns = (lists:usort(FunList) -- OldFuns),

    NewImportItems = [ create_import_item(?Fun:name(Fun), ?Fun:arity(Fun)) ||
                         Fun <- NewFuns ],

    ?Syn:replace(IList,{esub,length(OldFuns)+1,length(NewFuns)},NewImportItems).

create_import_item(Name, Arity) ->
    CName  = ?Syn:create(#expr{type=atom, value=Name}, [atom_to_list(Name)]),
    CArity = ?Syn:create(#expr{type=integer,value=Arity},
                         [integer_to_list(Arity)]),
    ?Syn:create(#expr{type=funref}, [{esub, [CName, CArity]}]).

%% All funs should come from module `Mod'.
check_imports(Mod, FunList) ->
    lists:map(
      fun(Fun) ->
              ?Check( [Mod] == ?Query:exec(Fun,?Fun:module()),
                      ?RefError(
                         fun_not_found,
                         [ ?Mod:name(Mod),?Fun:name(Fun),?Fun:arity(Fun) ]))
      end,
      FunList).

%% Removes the duplicates from the `FunList' and leaves out the already imported
%% ones.
sort_imports(File, FunList)->
    [Module] = ?Query:exec(File, ?File:module()),
    [ Fun || Fun <- lists:usort(FunList), not is_imported(Module, Fun) ].


%% @spec del_import(Mod, Fun) -> ok
%% @doc Removes `Fun' from the export list wich contains
%%      it in the module `Mod'.
%% This function uses esg queries!  The `ESG:close/0' has to be used
%% after the removing ends. (or use it in a new esg `batch')
del_import(Mod, Fun)->
    [Expr] = ?Query:exec(Fun, ?Fun:imports()),
    Form = ?Query:exec1(Expr, ?Expr:attrib_form(), form_not_found),
    ParList = ?Query:exec1(Expr,?Expr:parent(),parent_list_not_found),
    case ?Query:exec1(Form,?Form:module(),module_not_found) of
        Mod -> case length(?Query:exec(ParList,?Expr:children())) of
                   1 -> ?File:del_form(Form);
                   _ -> ?Syn:replace(ParList,{node, Expr},[])
               end,
               ok;
        _ -> ok

    end.


%% @spec find_fundef(atom(), atom(), integer()) -> node()
%% @doc Returns a query that finds the given function.
find_fundef(Mod, Name, Arity) ->
    ?Query:seq([find(Mod), ?Fun:find(Name, Arity), ?Fun:definition()]).
