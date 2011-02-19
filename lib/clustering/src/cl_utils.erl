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

%%% @doc Contains utilities for the clustering modules.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_utils).
-vsn("$Rev: 1247 $").

-export([ignore/1, leave/1, transform_to_01/3, 
         open_output/1, open_output/2, open_append/1,
         close_output/1, close_output/2,
         get_defined_value/2, proplist_update/2,
         concat_atoms/2, integer_to_list/2]).

%% @spec ignore(term()) -> (term(),[{Attr,Out::integer()}]) -> bool()
%%
%% @doc Entity filter that drops the modules given as arguments.
ignore(Modules) ->
    fun (Module, _Calls) ->
            lists:member(Module, Modules)
    end.

leave(Modules) ->
    fun (Module, _Calls) ->
            not lists:member(Module, Modules)
    end.

transform_to_01(_,size,N) -> N;
transform_to_01(_,entities,L) -> L;
transform_to_01(default,default,_N) -> 1;
transform_to_01(_,_,0) -> 0;
transform_to_01(_,_,_N) -> 1.

%%%%% input, output

%%% @todo This section should be deleted.

%%% @type io_ref() = stdout | string().
%%% It is a reference to an IO device.
%%% If it is 'stdout', then it refers to the standard output.
%%% If it is a string, then it refers to the file with that name.

%%% @type io_device().
%%% See the documentation of the io module.

%% @spec open_output(io_ref()) -> io_device()
%%
%% @doc Opens a file for writing. If the file exists, it will be overwritten.
%%
%% @deprecated Use the `cl_out' module instead.
open_output(null) ->
    null;
open_output(stdout) ->
    standard_io;
open_output(Fn) when is_list(Fn) ->
    case file:open(Fn, [write]) of
        {ok, Dev} ->
            Dev;
        {error, Reason} ->
            {open, Fn, file:format_error(Reason)}
    end.

%% @spec open_output(io_ref() | io_device(),bool()) -> io_ref() | io_device()
%%
%% @doc If `HandleOutput' is true, opens `Output' with 'open_output'; otherwise
%% it does nothing.
%%
%% @deprecated Use the `cl_out' module instead.
open_output(Output,HandleOutput) ->
    case HandleOutput of
        false -> Output;
        true -> open_output(Output)
    end.

%% @spec open_append(io_ref()) -> io_device()
%%
%% @doc Opens a file for writing in 'append' mode.
%%
%% @deprecated Use the `cl_out' module instead.
open_append(null) ->
    null;
open_append(stdout) ->
    standard_io;
open_append(Fn) when is_list(Fn) ->
    case file:open(Fn, [append]) of
        {ok, Dev} ->
            Dev;
        {error, Reason} ->
            throw({open, Fn, file:format_error(Reason)})
    end.

%% @spec close_output(io_device()) -> ok
%%
%% @doc Closes the `Output' device. If the output is 'standard_io', it does
%% nothing.
%%
%% @deprecated Use the `cl_out' module instead.
close_output(null) ->
    ok;
close_output(standard_io) ->
    ok;
close_output(Output) ->
    file:close(Output).

%% @spec close_output(io_ref() | io_device(),bool()) -> ok
%%
%% @doc If `HandleOutput' is true, closes `Output' with 'close_output';
%% otherwise it does nothing.
%%
%% @deprecated Use the `cl_out' module instead.
close_output(Output,HandleOutput) ->
    case HandleOutput of
        false -> ok;
        true -> close_output(Output)
    end.

%%%%% proplists

%% @spec get_defined_value(atom(),proplist()) -> term()
%%
%% @doc Gets the value belonging to `Item' in the `Options' property list.
%% If `Item' is not defined in `Options', an exception will be thrown.
get_defined_value(Item,Options) ->
    case proplists:get_value(Item,Options) of
        undefined ->
            throw(
              "The following option is undefined, which should be defined: '"++
              atom_to_list(Item)++"'");
        Value ->
            Value
    end.

%% @spec proplist_update(proplist(),proplist()) -> proplist()
%%
%% @doc Updates `List1' by `List2'.
%% The new properties will be the properties of `List2', except for the
%% properties that are only present in `List1'.
proplist_update(List1,List2) ->
    proplist_update_sorted(
      lists:usort(proplists:unfold(List1)),
      lists:usort(proplists:unfold(List2))).

%% @spec proplist_update_sorted(proplist(),proplist()) -> proplist()
%%
%% @doc Updates `List1' by `List2'.
%% The new properties will be the properties of `List2', except for the
%% properties that are only present in `List1'.
%% `List1' and `List2' must be unfolded and sorted.
proplist_update_sorted([],[]) ->
    [];
proplist_update_sorted([],L2) ->
    L2;
proplist_update_sorted(L1,[]) ->
    L1;
proplist_update_sorted([{Key,_Value1}|T1],[{Key,Value2}|T2]) ->
    [{Key,Value2}|proplist_update_sorted(T1,T2)];
proplist_update_sorted([{Key1,_}=H1|T1],[{Key2,_}=H2|T2]) ->
    case Key1 < Key2 of
        true ->
            [H1|proplist_update_sorted(T1,[H2|T2])];
        false ->
            [H2|proplist_update_sorted([H1|T1],T2)]
    end.

%%%%% etc

%% @spec concat_atoms(atom(),atom()) -> atom()
%%
%% @doc Concatenates two atoms as strings.
concat_atoms(Atom1,Atom2) ->
    list_to_atom(atom_to_list(Atom1) ++ atom_to_list(Atom2)).

%% @spec integer_to_list(integer(),integer()) -> string()
%%
%% @doc Converts `N' to string with `P' leading zeros.
integer_to_list(N,P) ->
    case lists:flatten(io_lib:format("~"++integer_to_list(P)++"..0b", [N])) of
        %% if "***" was returned, it means that N does not fit in P characters.
        [$*|_] -> integer_to_list(N);
        Result -> Result
    end.
