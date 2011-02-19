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

%%% @doc Standard refactoring errors. This module contains the textual
%%% description of standard refactoring errors that can be thrown using the
%%% macro `?RefError'.
%%%
%%% Error terms that are thrown by refactoring modules have the
%%% following format:
%%%
%%% ```{Module, Type, Details}'''
%%%
%%% where `Module' is the "owner" module of the error (it's like a
%%% namespace for errors), `Type' is an atom that identifies the error
%%% condition, and `Details' is a list, its contents depend on `Type'.
%%%
%%% There are two macros that should be used to throw errors:
%%%
%%% <ul>
%%%
%%%  <li>`?RefError(Type, Detail)' creates a standard error term that
%%%    belongs to this module.</li>
%%%
%%%  <li>`?LocalError(Type, Detail)' creates an error that belongs to
%%%    the module that uses the macro.</li>
%%%
%%% </ul>
%%%
%%% Both should be used like this: `throw(?RefError(missing_arg, [Key]))'.
%%%
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(referl_error).
-vsn("$Rev: 3026 $").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

-export([error_text/1]).


%%% ============================================================================
%%% Error messages

%% @spec error_text(error()) -> String
%%       String = [char() | String]
%% @doc Returns a textual description of the error term.
error_text({?MODULE, Type, Detail}) ->
    error_text(Type, Detail);
error_text({Mod, Type, Detail}) ->
    try
        case Mod:error_text(Type, Detail) of
            unknown -> unknown_error_text(Mod, Type, Detail);
            Text    -> Text
        end
    catch
        _:_ ->
            unknown_error_text(Mod, Type, Detail)
    end.

%% @doc  Error text in case of an unknown error.
unknown_error_text(Mod, Type, Detail) ->
    io_lib:format("Unknown error: ~p:~p~nError details: ~p",
                  [Mod, Type, Detail]).


% Removed
%error_text(ambiguous_token, _) ->
%    "The selection has multiple references";
%error_text(missing_opt, [Arg]) ->
%    ["Option ", atom_to_list(Arg), " is not specified"];
%error_text(lex_not_found, [_Path, _Pos]) ->
%    ["There is no lexical element in the given position"];

error_text(target_not_found, _) ->
    "Target module not found";
error_text(source_not_found, _) ->
    "Source module not found";
error_text(cancelled, _) ->
    "Transformation aborted by user";
error_text(illegal_pos, [File, Pos]) ->
    ["Position ", integer_to_list(Pos), " not found in file ", File];
error_text(token_parent, [Type]) ->
    ["The selection has to be inside ", ?MISC:add_article(node_kind_text(Type))];
error_text(file_not_present, [Name]) ->
    ["File ", Name, " cannot be found in the database"];
error_text(file_not_module, [File]) ->
    ["File ", File, " does not identify a module"];
error_text(file_not_hrl, [File]) ->
    ["File ", File, " does not identify a header file"];
error_text(rel_path, [_]) ->
    ["The path of the header file has to be an absolute path"];
error_text(mod_not_found, [Name]) ->
    ["Module ", atom_to_list(Name), " not found"];
error_text(fun_not_found, FunInfo) ->
    ["Function ", ?MISC:fun_text(FunInfo), " not found"];
error_text(rec_not_found, [Name]) ->
    ["Record ", atom_to_list(Name), " not found"];
error_text(recfld_not_found, RecFld) ->
    ["Record field ", ?MISC:recfld_text(RecFld), " not found"];
error_text(mac_not_found, [Name]) ->
    ["Macro ", Name, " not found"];
error_text(fun_exists, FunInfo) ->
    ["Function ", ?MISC:fun_text(FunInfo), " already exists"];
error_text(var_exists, VarName) ->
    ["Variable ", VarName, " already exists"];
error_text(rec_exists, RecName) ->
    ["Record ", RecName, " already exists"];
error_text(mac_exists, RecName) ->
    ["Macro ", RecName, " already exists"];
error_text(imported_fun_exists, [_Mod, FunInfo]) ->
    ["Function ", ?MISC:fun_text(FunInfo), " is already imported"];
error_text(autoimported_fun_exists, FunInfo) ->
    ["Function ", ?MISC:fun_text(FunInfo), " is autoimported"];
error_text(missing_arg, [Desc]) ->
    ["Missing argument: ", Desc];
error_text(arg_type, [Name, Type]) ->
    ["Argument \"", atom_to_list(Name), "\" has to be ", ?MISC:add_article(Type)];
error_text(pos_bad_type, [ExpectedType, Pos]) ->
    ["The given position (", integer_to_list(Pos), ") ",
     "has to indicate ", ?MISC:add_article(node_kind_text(ExpectedType))];
error_text(bad_order, [Length]) ->
    ["The given order should have all values from 1 to ", integer_to_list(Length)];
error_text(order_not_list, []) ->
    ["The new order has to be given as a list"];
error_text(order_arity_mismatch, [Arity]) ->
    ["The given order should have the same arity as the function: ",
     integer_to_list(Arity)];
error_text(bad_range, []) ->
    ["A valid expression range has to be selected"];
error_text(outside_used_vars, List) ->
    ["Variable used outside the selection: ",
     ?MISC:separated_text(List)];
error_text(var_exists_app, List)->
    ["No visible names may be used: ", ?MISC:separated_text(List)];
error_text(bad_kind, []) ->
    ["The selection is not acceptable for this transformation"];
error_text(bad_kind, BadKind) ->
    ["The selection is ", ?MISC:add_article(node_kind_text(BadKind)),
     ", which is not acceptable for this transformation"];
error_text(parent_not_form, []) ->
    ["The selection has to be a valid part of a function body"];
error_text(recursive_subexpr, []) ->
    ["The selection may not contain recursive calls"];
error_text(bad_var_name, []) ->
    ["Illegal variable name given"];
error_text(quoted_atom, []) ->
    ["Quoted atoms are not allowed"];
error_text(module_exists,[Name]) ->
    ["Collision with existing module \"", atom_to_list(Name), "\""];
error_text(file_exists,[Path]) ->
    ["Collision with existing file \"", Path, "\""];
error_text(_, _) ->
    unknown.




%% The textual representation of a node type or kind.
%% If several node types are given in a list, the first one is selected.
%% todo Should be included in the XML representation and generated.
node_kind_text([Type|_])    -> node_kind_text(Type);
node_kind_text(guard)       -> "guard";
node_kind_text(pattern)     -> "pattern";
node_kind_text(list_comp)   -> "list comprehension";
node_kind_text(list_gen)    -> "list generator";
node_kind_text(binary)      -> "binary";
node_kind_text(filter)      -> "filter";
node_kind_text('fun')       -> "fun expression";
node_kind_text(application) -> "function application";
node_kind_text(rec)         -> "record";
node_kind_text(recfield)    -> "record field";
node_kind_text(expr)        -> "expression";
node_kind_text(Type)        -> ["code of type \"", atom_to_list(Type), "\""].
