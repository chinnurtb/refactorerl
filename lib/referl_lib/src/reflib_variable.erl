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

%%% @doc This modules implements queries about variables.
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(reflib_variable).
-vsn("$Rev: 4474 $").

-export([name/1]).
-export([valid_name/1]).
-export([bindings/0, bindings/1,
         references/0, references/1,
         occurrences/0, occurrences/1,
         scopes/0, clause/0]).

%% Variable name generation
-export([new_varname/2, new_varname_with_prefix/2]).

-include("lib.hrl").


%% @spec name(node()) -> string()
%% @doc Returns the name of a variable
name(Var) ->
    (?ESG:data(Var))#variable.name.

%% @spec valid_name(string()) -> bool() 
%% @doc  Check the `NameStr' string is represent a legal variable name.
%% @see  referl_misc:string_char_type/1
valid_name(NameStr)  when is_list(NameStr) ->
    valid_name_(lists:flatten(NameStr)).

valid_name_("")    -> false;
valid_name_("_")   -> false;
valid_name_([H|T]) ->
    (H == $_ orelse ?MISC:string_char_type(H) == uppercase) andalso
        lists:all(fun allow_in_name/1, T).

%% @spec allow_in_name(CharCode::integer()) -> bool()
%% @doc  Check the character represented by `CharCode' is allowed in atom or 
%%       variable name.
%% @see  referl_misc:string_char_type/1
allow_in_name(CharCode) when is_integer(CharCode) ->
    (?MISC:string_is_letter(CharCode)) orelse 
    (?MISC:string_char_type(CharCode) == digit) orelse 
    (CharCode == $@) orelse (CharCode == $_).

%% @spec bindings() -> query(#variable{}, #expr{})
%% @doc Returns every binding of a variable
bindings() ->
    [{varbind, back}].

%% @spec bindings(Expr::node()) -> query(#variable{}, #expr{})
%% @doc Returns every binding inside an expression
bindings(Expr) ->
    fun(Var) ->
        ?MISC:intersect(?Query:exec([Var], bindings()), 
                        ?Query:exec([Expr], ?Expr:deep_sub()))
    end.

%% @spec references() -> query(#variable{}, #expr{})
%% @doc Returns every reference of a variable
references() ->
    [{varref, back}].
    
%% @spec references(Expr::node()) -> query(#variable{}, #expr{})
%% @doc Returns every refernce of a variable inside an expression
references(Expr) ->
    fun(Var) ->
        ?MISC:intersect(?Query:exec([Var], references()), 
                        ?Query:exec([Expr], ?Expr:deep_sub()))
    end.

%% @spec occurrences() -> query(#variable{}, #expr{})
%% @doc Returns every occurrence of a variable
occurrences() ->
    ?Query:all(bindings(), references()).

%% @spec occurrences(Expr::node()) -> query(#variable{}, #expr{})
%% @doc Returns every occurrence of a variable inside an expression
occurrences(Expr) ->
    fun(Var) ->
        ?MISC:intersect(?Query:exec([Var], occurrences()), 
                        ?Query:exec([Expr], ?Expr:deep_sub()))
    end.


%% @spec scopes() -> query(#variable{}, #clause{})
%% @doc The result query returns every scope that contains the variable.
scopes() ->
    fun(Var) ->
            lists:usort(
              ?Query:exec(Var,
                          ?Query:all([{vardef, back}],
                                     [{{varvis, back}, {var,'==',scope}}])))
    end.

%% @spec clause() -> query(#variable{}, #clause{})
%% @doc The result query returns the function clause the variable is defined in.
clause() ->
    [{vardef, back}].


%%% ============================================================================
%%% Variable name generation

%% Names for new variables in order of preference.
-define(Variables, ["X", "Y", "Z", "I", "J", "K", "L", "M", "N",
                    "A", "B", "C", "D", "E", "F", "G", "H", "O",
                    "P", "Q", "R", "S", "T", "U", "V", "W"]).

%% @doc Returns a variable name that is not bound in the scope of the expression.
%% The preferred names are single character ones,
%% with "usual variable names" having extra preference.
%% If none of them are available, a fresh variable name will be returned,
%% composed of the given prefix and a yet unused index.
new_varname(Expr, Prefix) ->
    Vars      = ?Query:exec(Expr, ?Query:all(?Expr:visible_vars(),
                                             ?Expr:variables())),
    UsedNames = [?Var:name(X) || X <- Vars],
    Allowed   = ?Variables -- UsedNames,
    case Allowed of
        [First | _] -> First;
        []          -> varname_with_next_idx(Prefix, UsedNames, 1)
    end.

%% @doc Returns a variable name that is not bound in the scope of the expression.
%% The new name consists of the prefix and a yet unused index.
new_varname_with_prefix(Expr, Prefix) ->
    Vars      = ?Query:exec(Expr, ?Expr:visible_vars()),
    UsedNames = [?Var:name(X) || X <- Vars, lists:prefix(Prefix, ?Var:name(X))],
    varname_with_next_idx(Prefix, UsedNames, 1).

varname_with_next_idx(Prefix, UsedNames, Idx) ->
    VarName = Prefix ++ integer_to_list(Idx),
    case lists:member(VarName, UsedNames) of
        true  -> varname_with_next_idx(Prefix, UsedNames, Idx + 1);
        false -> VarName
    end.
