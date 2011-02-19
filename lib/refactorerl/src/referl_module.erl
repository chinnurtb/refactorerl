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

%%% @doc This module contains functions that return a queries stating 
%%%      from the module and it contains two module related transformations
%%%      to delete function and add a function into an arbritary import list. 

%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(referl_module).
-vsn(" $Rev: 2224$ ").

%% =============================================================================
%% Exports

-export([name/1, find/1, file/0, locals/0, local/2,
         references/0,
         imported/2, exported/2, visible/2,
         add_import/2,del_import/2
        ]).

-include("refactorerl.hrl").


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


%% @spec imported(Name, Ary) -> query(#module{}, #func{})
%% @doc The result query returns a function that imported into this module.
imported(Name, Ary)->
    [{funimp, {{name, '==', Name} ,'and' ,{arity,'==', Ary}}}].


%% @spec exported(Name, Ary) -> query(#module{}, #func{})
%% @doc The result query returns a function exported from this module.
exported(Name, Ary)->
    [{funexp, {{name,'==', Name} ,'and' ,{arity,'==', Ary}}}].


%% @spec visible(Name, Ary) -> query(#module{}, #func{})
%% @doc The result query returns a function visible in this module.
visible(Name, Ary)-> 
    ?Query:all([{func,{{name, '==', Name}, 'and', {arity, '==', Ary}}}],
                [{funimp,{{name, '==', Name}, 'and', {arity, '==', Ary}}}]).


%% =============================================================================
%% Module related transformations


%% @spec add_import(Mod, Fun) -> ok
%% @doc This function can add a new function `Fun' into an arbitrary
%%      import list in the module `Mod'. (actually creates a new import
%%      list for the function `Fun').
%% This function uses `ESG' queries!  The `ESG:close/0' has to be used
%% after the transformation ends. (or use it in a new esg `batch')
%% If the function exists in the import list, the transformation will fail.
add_import(Mod, Fun)->
    FunMod = ?Query:exec1(Fun, ?Fun:module(), mod_not_found),

    ModName = name(FunMod),

    File = ?Query:exec1(Mod, file(), []),
    ModExpr = ?Syn:create(#expr{kind=atom, value=ModName},
                          [atom_to_list(ModName)]),
 

    %Minus   = create_lex(minus, "-"),
    %NewName = create_lex(atom, "import"),
    ImportForm = ?Syn:create(#form{type=attrib, tag=import},
                         ["-","import",
                          %{flex, Minus},
                          %{flex, NewName},
                          {attr, ModExpr},     
                          {attr, create_list(
                                   ?Fun:name(Fun),?Fun:arity(Fun))}]),
    ?File:add_form(File, ImportForm),
    ok.

%% @private
create_list(Name, Arity) ->
    ?Syn:create(#expr{kind=cons},
     [{sub,[ ?Syn:create(#expr{kind=list},
                            [{sub, [create_import_item(Name, Arity)]}])]}]).


%% @private
create_import_item(Name, Arity) ->
    CName  = ?Syn:create(#expr{kind=atom, value=Name},
                                [atom_to_list(Name)]),
    CArity = ?Syn:create(#expr{kind=integer,value=Arity},
                                [integer_to_list(Arity)]),
    ?Syn:create(#expr{kind=infix_expr, value='/'},
                                   [{sub, [CName, CArity]}]).


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
