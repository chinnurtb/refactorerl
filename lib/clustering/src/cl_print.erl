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

%%% @doc Print the results of module clustering and splitting files.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_print).
-vsn("$Rev: 1974 $").

-export([print_clusterings/1, print_clusterings/2,
         print_clustering/1, print_clustering/2,
         print_cluster/1, print_cluster/2,
         print_cuts/1, print_cuts/2,
         print_cut/1, print_cut/2]).
-export([interface_funs/1]).

-include("cluster.hrl").

-import(proplists, [get_value/2]).

%%% @type mod_name() = atom().
%%%
%%% It represents the name of a module.

%%% @type proplist() = [atom()|{atom(), term()}].
%%%
%%% See the documentation of the proplists module.

%%%%% Printing clusterings

%% @spec print_clusterings([[[mod_name()]]]) -> ok
%%
%% @doc Prints a list of clusterings with default options.
%% Same as `print_clusterings(Clusterings, [])'.
print_clusterings(Clusterings) ->
    print_clusterings(Clusterings, []).

%% @spec print_clusterings([[[mod_name()]]], proplist()) -> ok
%%
%% @doc Prints a list of clusterings.
%%
%% Options:
%% <ul>
%%     <li>`output::output_ref()': it specifies where to print.
%%         The default is `stdout'.</li>
%%     <li>`indent::string()':
%%         The value of this option will be placed in the output where
%%         indentation is needed.
%%         It is four spaces by default.</li>
%%     <li>`print_clusterings::[atom() | meta()]':
%%         the elements of this option specify what should be printed.
%%         It can contain the following items:
%%         <ul>
%%              <li>`clusterings': prints the clusterings.</li>
%%              <li>Meta items.</li>
%%         </ul>
%%         The default is:
%%         <code>[clusterings]</code></li>
%%     <li>`print_clusterings::[atom() | meta()]':
%%         the elements of this option specify what should be printed when
%%         printing a clustering.
%%         For the items that it can contain, see {@link print_clustering/2}.
%%         The default is:
%%         <code>[new_section, clusters]</code></li>
%%     <li>`print_cluster::[atom() | meta()]':
%%         the elements of this option specify what should be printed when
%%         printing a cluster.
%%         For the items that it can contain and for the default value, see
%%         {@link print_cluster/2}.</li>
%% </ul>
print_clusterings(Clusterings, Options) ->
    Opts = cl_utils:proplist_update(print_clusterings_default(), Options),
    {W, C} = cl_out:open(get_value(output, Opts)),

    lists:foreach(
      fun (clusterings) ->
              Opts2 = [{output, W} | proplists:delete(output, Opts)],
              lists:foreach(
                fun (Clustering) ->
                        print_clustering(Clustering, Opts2)
                end,
                Clusterings);
          (Meta) ->
              print_meta(W, Meta)
      end,
      get_value(print_clusterings, Opts)),

    cl_out:close(C).

print_clusterings_default() ->
    cl_utils:proplist_update(
      print_clustering_default(),
      [{print_clusterings, [clusterings]},
       {print_clustering, [new_section, clusters]}]).

%% @spec print_clustering([[mod_name()]]) -> ok
%%
%% @doc Prints a clustering with default options.
%% Same as `print_clustering(Clustering, [])'.
print_clustering(Clustering) ->
    print_clustering(Clustering, []).

%% @spec print_clustering([[mod_name()]], proplist()) -> ok
%%
%% @doc Prints a clustering.
%%
%% Options:
%% <ul>
%%     <li>`output::output_ref()': it specifies where to print.
%%         The default is `stdout'.</li>
%%     <li>`indent::string()':
%%         The value of this option will be placed in the output where
%%         indentation is needed.
%%         It is four spaCes by default.</li>
%%     <li>`print_clustering::[atom() | meta()]':
%%         the elements of this option specify what should be printed.
%%         It can contain the following items:
%%         <ul>
%%              <li>`clusters': prints the clusters.</li>
%%              <li>Meta items.</li>
%%         </ul>
%%         The default is:
%%         <code>[clusters]</code></li>
%%     <li>`print_cluster::[atom() | meta()]':
%%         the elements of this option specify what should be printed when
%%         printing a cluster.
%%         For the items that it can contain see {@link print_cluster/2}.
%%         The default value:
%% ```
%% ["Interface functions:", nl, interface_funs,
%%  "Modules:", nl, modules, nl]
%% '''</li>
%% </ul>
print_clustering(Clustering, Options) ->
    Opts = cl_utils:proplist_update(print_clustering_default(), Options),
    {W, C} = cl_out:open(get_value(output, Opts)),

    Indent = get_value(indent, Opts),
    lists:foreach(
      fun (clusters) ->
              Opts2 = [{output, W} | proplists:delete(output, Opts)],
              lists:foreach(
                fun (Cluster) ->
                        print_cluster(Cluster, Opts2)
                end,
                Clustering);
          (modules_compact) ->
              print_term(W, lists:sort(Clustering), Indent);
          (Meta) ->
              print_meta(W, Meta)
      end,
      get_value(print_clustering, Opts)),

    cl_out:close(C).

print_clustering_default() ->
    cl_utils:proplist_update(
      print_cluster_default(),
      [{print_clustering, [clusters]},
       {print_cluster,
        ["Interface functions:", nl, interface_funs,
         "Modules:", nl, modules, nl]}]).

%% @spec print_cluster([mod_name()]) -> ok
%%
%% @doc Prints a cluster with default options.
%% Same as `print_cluster(Cluster, [])'.
print_cluster(Cluster) ->
    print_cluster(Cluster, []).

%% @spec print_cluster([mod_name()], proplist()) -> ok
%%
%% @doc Prints a cluster.
%%
%% Options:
%% <ul>
%%     <li>`output::output_ref()': it specifies where to print.
%%         The default is `stdout'.</li>
%%     <li>`indent::string()':
%%         The value of this option will be placed in the output where
%%         indentation is needed.
%%         It is four spaces by default.</li>
%%     <li>`print_cluster::[atom() | meta()]':
%%         the elements of this option specify what should be printed.
%%         It can contain the following items:
%%         <ul>
%%              <li>`interface_funs': prints the interface functions of the
%%                  cluster.</li>
%%              <li>`modules': prints each module in a separate row.</li>
%%              <li>`modules_compact': prints all modules in a sorted list.</li>
%%              <li>Meta items.</li>
%%         </ul>
%%         The default is:
%% ```
%% ["Interface functions:", nl, interface_funs,
%% "Modules:", nl, modules]
%% '''</li>
%% </ul>
print_cluster(Cluster, Options) ->
    Opts = cl_utils:proplist_update(print_cluster_default(), Options),
    {W, C} = cl_out:open(get_value(output, Opts)),

    Indent = get_value(indent, Opts),
    lists:foreach(
      fun (interface_funs) ->
              print_string_list(
                W,
                lists:usort([fun_description(Fun) ||
                                Fun <- interface_funs(Cluster)]),
                Indent);
          (modules) ->
              print_string_list(W, Cluster, Indent);
          (modules_compact) ->
              print_term(W, lists:sort(Cluster), Indent);
          (Meta) ->
              print_meta(W, Meta)
      end,
      get_value(print_cluster, Opts)),

    cl_out:close(C).

print_cluster_default() ->
    [{output, stdout},
     {indent, "    "},
     {print_cluster,
      ["Interface functions:", nl, interface_funs,
       "Modules:", nl, modules]}].

%%%%% Printing cuts

%% @type meta() = new_section | nl | '-' | '=' | string().
%%
%% A meta item represents an item, which can be printed:
%% <ul>
%%     <li>`new_section': starts a new section (see the documentation of the
%%         `cl_out' module)</li>
%%     <li>`nl': a newline,</li>
%%     <li>`-': 70 hyphens,</li>
%%     <li>`=': 70 equal signs,</li>
%%     <li>`string()': the given string.</li>
%% </ul>

%% @spec print_cuts(cuts_result_nice()) -> ok
%%
%% @doc Prints the result of `cut_libs_all' with the default options.
%% Same as `print_cuts(CutResult, [])'.
print_cuts(Cuts) ->
    print_cuts(Cuts, []).

%% @spec print_cuts(cuts_result_nice(), proplist()) -> ok
%%
%% @doc Prints the result of `cut_libs_all'.
%%
%% Options:
%% <ul>
%%     <li>`output::output_ref()': specifies the output.
%%         The default is `stdout'.</li>
%%     <li>`indent::string()':
%%         The value of this option will be placed in the output where
%%         indentation is needed.
%%         It is four spaces by default.</li>
%%     <li>`print_cuts::[atom() | meta()]':
%%         the elements of this option specify what should be printed.
%%         It can contain the following items:
%%         <ul>
%%              <li>`cuts': prints the decomposition of the file.</li>
%%              <li>Meta items.</li>
%%         </ul>
%%         The default is:
%%         <code>[cuts]</code></li>
%%     <li>`print_cut::[moved | not_moved | meta()]':
%%         the elements of this option specify what should be printed when
%%         printing a cut.
%%         For the items that it can contain and for the default value, see
%%         {@link print_cut/2}.</li>
%%     <li>`print_moved::[atom() | meta() | tuple()]':
%%         the elements of this option specify what should be printed in a
%%         group of moved objects.
%%         For the items that it can contain and for the default value, see
%%         {@link print_cut/2}.</li>
%%     <li>`print_not_moved::[atom() | meta() | tuple()]':
%%         the elements of this option specify what should be printed in a group
%%         of not moved objects.
%%         For the items that it can contain and for the default value, see
%%         {@link print_cut/2}.</li>
%% </ul>
print_cuts(Cuts, Options) ->
    Opts = cl_utils:proplist_update(print_cuts_default(), Options),
    {W, C} = cl_out:open(get_value(output, Opts)),

    lists:foreach(
      fun (cuts) ->
              Opts2 = [{output, W} | proplists:delete(output, Opts)],
              lists:foreach(
                fun({FileName, Cut}) ->
                        print_cut(Cut, [{file_name, FileName}|Opts2])
                end,
                Cuts);
          (Meta) ->
              print_meta(W, Meta)
      end,
      get_value(print_cuts, Opts)),

    cl_out:close(C).

%% @spec print_cuts_default() -> proplist()
%%
%% @doc Returns the default options of `print_cuts'.
print_cuts_default() ->
    cl_utils:proplist_update(
      print_cut_default(),
      [{print_cuts, [cuts]}]).

%% @spec print_cut(cut_result_nice()) -> ok
%%
%% @doc Prints the result of {@link cl_cutlib:cut_lib/4} with the default
%% options.
%% Same as `print_cut(CutResult, [])'.
print_cut(CutResult) ->
    print_cut(CutResult, []).

%% @spec print_cut(cut_result_nice(), proplist()) -> ok
%%
%% @doc Prints the result of `cutlib'.
%%
%% Options:
%% <ul>
%%     <li>`output::output_ref()': specifies the output.
%%         The default is `stdout'.</li>
%%     <li>`indent::string()':
%%         The value of this option will be placed in the output where
%%         indentation is needed.
%%         It is four spaces by default.</li>
%%     <li>`print_cut::[moved | not_moved | meta()]':
%%         the elements of this option specify what should be printed in a cut.
%%         It can contain the following items:
%%         <ul>
%%              <li>`file_name': prints the name of the file.</li>
%%              <li>`file_basename': prints the base name of the file.</li>
%%              <li>`moved': prints the group of moved objects.</li>
%%              <li>`not_moved': prints the group of not moved objects.</li>
%%              <li>Meta items.</li>
%%         </ul>
%%         The default value is:
%% ```
%%  ['=',
%%   "Parts of file ", file_basename, ":", nl,
%%   moved,
%%   not_moved]
%% '''</li>
%%     <li>`print_moved::[atom() | meta() | tuple()]':
%%         the elements of this option specify what should be printed in a
%%         group of moved objects.
%%         It can contain the following items, all of which print some
%%         information about the moved objects:
%%         <ul>
%%              <li>`modules': prints each module in a separate row.</li>
%%              <li>`modules_compact': prints all modules in a sorted list.</li>
%%              <li>`cluster_id': prints the id of the cluster where the objects
%%                  are moved to.</li>
%%              <li>`exp_funs': prints the exported functions.</li>
%%              <li>`int_funs': prints the internal functions.</li>
%%              <li>`funs': prints all the functions. If the printing functions
%%                  do not have information about the file where the functions
%%                  are (e.g. because the database does not contain it any
%%                  more), then the functions will be printed only here, they
%%                  will not be printed either in the `exp_funs' or in the
%%                  `int_funs' section. </li>
%%              <li>`records': prints the records.</li>
%%              <li>`macros': prints the macros.</li>
%%              <li>`etc': prints the objects that do not fit into any
%%                  group.</li>
%%              <li>`count': prints the count of the objects printed in the
%%                  current tuple.
%%                  E.g. when the tuple
%% <code>{"Exported functions (", count, "):", nl, exp_funs, nl}</code>
%%                  is printed, the `count' will print the number of items
%%                  printed by `exp_funs', i.e. the number of exported
%%                  functions.</li>
%%              <li>Meta items.</li>
%%         </ul>
%%         Elements in a tuple are printed the same way as elements directly in
%%         the list (the only exception is the `count' item).
%%         Tuples can be used for printing the `count' item.
%%
%%         The default value of the option is:
%% ```
%% ['-',
%%  "File parts that are used by cluster ", cluster_id, nl,
%%  modules_compact, ":", nl, nl,
%%  {"Exported functions (", count, "):", nl, exp_funs, nl},
%%  {"Internal functions (", count, "):", nl, int_funs, nl},
%%  {"Macros (", count, "):", nl, macros, nl},
%%  {"Records (", count, "):", nl, records, nl}]
%% '''</li>
%%     <li>`print_not_moved::[atom() | meta() | tuple()]':
%%         the elements of this option specify what should be printed in a group
%%         of not moved objects.
%%         It can contain the same items as `print_moved', except for
%%         `cluster_id'.
%%         The default value is:
%% ```
%% ['-',
%%  "File parts that are used by more clusters:", nl, nl,
%%  {"Exported functions (", count, "):", nl, exp_funs, nl},
%%  {"Internal functions (", count, "):", nl, int_funs, nl},
%%  {"Macros (", count, "):", nl, macros, nl},
%%  {"Records (", count, "):", nl, records, nl}]
%% '''</li>
%%     <li>`file_name::(undefined | string())'.
%%         The name of the file that is decomposed.
%%         If the file is in the database, the function will try to perform
%%         queries about it.
%%         The default value is `undefined'.</li>
%% </ul>
print_cut({ObjectsNotMoved, ObjectsMoved}, Options) ->
    Opts = cl_utils:proplist_update(print_cut_default(), Options),
    {W, C} = cl_out:open(get_value(output, Opts)),

    FileName = get_value(file_name, Opts),
    File =
        case FileName of
            undefined ->
                undefined;
            _ ->
                {file, File2} = ?SYNTAX:file(FileName),
                File2
        end,

    lists:foreach(
      fun (file_name) ->
              cl_out:fwrite(W, "~s", [FileName]);
          (file_basename) ->
              cl_out:fwrite(W, "~s", [filename:basename(FileName)]);
          (not_moved) ->
              print_objects(W, ObjectsNotMoved,
                            get_value(print_not_moved, Opts),
                            undefined, undefined, File, Opts);
          (moved) ->
              lists:foreach(
                fun({ClusterId, Cluster, ObjectsInCluster}) ->
                        print_objects(W, ObjectsInCluster,
                                      get_value(print_moved, Opts),
                                      ClusterId, Cluster, File, Opts)
                end,
                ObjectsMoved);
          (Meta) ->
              print_meta(W, Meta)
      end,
      get_value(print_cut, Opts)),

    cl_out:close(C).

%% @spec print_cut_default() -> proplist()
%%
%% @doc Returns the default options of `print_cut'.
print_cut_default() ->
    [{output, stdout},
     {indent, "    "},
     {print_cut,
      ['=',
       "Parts of file ", file_basename, ":", nl,
       moved,
       not_moved]},
     {print_moved,
      ['-',
       "File parts that are used by cluster ", cluster_id, nl,
       modules_compact, ":", nl, nl,
       {"Exported functions (", count, "):", nl, exp_funs, nl},
       {"Internal functions (", count, "):", nl, int_funs, nl},
       {"Macros (", count, "):", nl, macros, nl},
       {"Records (", count, "):", nl, records, nl}]},
     {print_not_moved,
      ['-',
       "File parts that are used by more clusters:", nl, nl,
       {"Exported functions (", count, "):", nl, exp_funs, nl},
       {"Internal functions (", count, "):", nl, int_funs, nl},
       {"Macros (", count, "):", nl, macros, nl},
       {"Records (", count, "):", nl, records, nl}]}].

%% @spec print_objects(writable_device(), [object()], [term()], cluster_id(),
%%                     [mod_name()], (file() | undefined), cut_opt()) -> ok
%%
%% @doc Prints a list of objects.
print_objects(W, Objects, PrintItems, ClusterId, Cluster, File, Opts) ->

    %% a category->[object] dictionary
    D2 =
        lists:foldl(
          fun(Object, D1) ->
                  Category = object_category(Object, File),
                  dict:store(Category, [Object|dict:fetch(Category, D1)], D1)
          end,
          dict:from_list([{int_funs, []}, {exp_funs, []}, {funs, []},
                          {records, []}, {macros, []}, {etc, []}]),
          Objects),

    %% a category->[object] dictionary, where the [object] lists are sorted
    D4 =
        dict:fold(
          fun(Category, List, D3) ->
                  dict:store(Category, lists:sort(List), D3)
          end,
          dict:new(),
          D2),

    Indent = get_value(indent, Opts),
    lists:foreach(
      fun (Item) ->
              print_objects_item(W, Item, D4, Cluster, ClusterId, Indent)
      end,
      PrintItems),
    ok.

%% @spec object_category(object(), (file() | undefined)) -> atom()
%%
%% @doc Returns the category of the given object.
object_category(#fun_attr{}, undefined) ->
    funs;
object_category(#fun_attr{name=Name, arity=Arity}, File) ->
    case ?SEMINF:is_exported(File, Name, Arity) of
        true -> exp_funs;
        false -> int_funs
    end;
object_category(#rec_attr{}, _) ->
    records;
object_category(#macro_attr{}, _) ->
    macros;
object_category(_, _) ->
    etc.

%% @spec print_objects_item(writable_device(), term(),
%%                          dictionary(atom(), [object()]),
%%                          [mod_name()], cluster_id(), string()) -> ok
%%
%% @doc Prints an item in `print_objects'.
print_objects_item(W, Item, D, Cluster, ClusterId, Indent) ->
    ItemLst = if
                  is_tuple(Item) -> tuple_to_list(Item);
                  true -> [Item]
              end,
    PrintItems =
        lists:map(
          fun (modules) ->
                  {fun print_string_list/3,
                   Cluster};
              (modules_compact) ->
                  {fun print_term/3,
                   lists:sort(Cluster)};
              (cluster_id) ->
                  {fun print_term/3,
                   ClusterId};
              (exp_funs) ->
                  {fun print_object_pure_list/3,
                   dict:fetch(exp_funs, D)};
              (int_funs) ->
                  {fun print_object_pure_list/3,
                   dict:fetch(int_funs, D)};
              (funs) ->
                  {fun print_object_pure_list/3,
                   dict:fetch(exp_funs, D) ++
                   dict:fetch(int_funs, D) ++
                   dict:fetch(funs, D)};
              (records) ->
                  {fun print_object_pure_list/3,
                   dict:fetch(records, D)};
              (macros) ->
                  {fun print_object_pure_list/3,
                   dict:fetch(macros, D)};
              (etc) ->
                  {fun print_object_pure_list/3,
                   dict:fetch(etc, D)};
              (count) ->
                  {meta, count};
              (Meta) ->
                  {meta, Meta}
          end,
          ItemLst),
    DataItems =
        lists:flatmap(
          fun ({Fn, Lst}) when is_function(Fn), is_list(Lst) -> [Lst];
              (_) -> []
          end,
          PrintItems),
    AllData = lists:flatten(DataItems),
    if
        AllData =:= [], DataItems =/= [] ->
            ok;
        true ->
            lists:foreach(
              fun ({PrintFn, Arg}) when is_function(PrintFn) ->
                      PrintFn(W, Arg, Indent);
                  ({meta, count}) ->
                      cl_out:fwrite(W, "~b", [length(AllData)]);
                  ({meta, Meta}) ->
                      print_meta(W, Meta)
              end,
              PrintItems)
    end.

%% @spec print_term(writable_device(), term(), string()) -> ok
%%
%% @doc Prints an Erlang term after the given indent.
print_term(W, Term, Indent) ->
    cl_out:fwrite(W, "~s~p", [Indent, Term]).

%% @spec print_string_list(writable_device(), [string()], string()) -> ok
%%
%% @doc Prints a list of string after the given indent.
print_string_list(W, L, Indent) ->
    print_list_gen(W, L, Indent, fun(S) -> S end).

%% @spec print_object_pure_list(writable_device(), [object()], string()) -> ok
%%
%% @doc Prints a list of objects after the given indent.
print_object_pure_list(W, L, Indent) ->
    print_list_gen(W, L, Indent, fun object_pure_to_string/1).

%% @spec object_pure_to_string(object()) -> string()
%%
%% @doc Converts the given object to string.
object_pure_to_string(#fun_attr{name=Name, arity=Arity}) ->
    atom_to_list(Name)++"/"++integer_to_list(Arity);
object_pure_to_string(#rec_attr{name=Name}) ->
    atom_to_list(Name);
object_pure_to_string(#macro_attr{name=Name}) ->
    Name.

%%%%% Generic print functions

%% @spec print_meta(writable_device(), meta()) -> ok
%%
%% @doc Prints a meta item. See the documentation of the type `meta()' for the
%% details.
print_meta(W, Meta) ->
    case Meta of
        new_section -> cl_out:new_section(W);
        '-' -> cl_out:fwrite(W, "~70c~n", "-");
        '=' -> cl_out:fwrite(W, "~70c~n", "=");
        nl  -> cl_out:fwrite(W, "~n");
        Str when is_list(Str) ->
            cl_out:fwrite(W, "~s", [Str])
    end.

%% @spec print_list_gen(writable_device(), [a()], string(), (a()) ->
%%           string()) -> ok
%%
%%           a() = term()
%%
%% @doc Prints the elements of the given list.
%% More precisely, all the elements of the list are transformed with `Fun', and
%% the result of the transformation is printed.
%% Every element will go into a separate line after the given indent.
print_list_gen(_, [], _, _) ->
    ok;
print_list_gen(W, [Head | Tail], Indent, Fun) ->
    cl_out:fwrite(W, "~s~s~n", [Indent, Fun(Head)]),
    print_list_gen(W, Tail, Indent, Fun).

%%%%% Query functions

modules() ->
    ?ESG:path(?ESG:root(), [module]).

function_calls(Module) ->
    ?ESG:path(Module, [func, {fundef, back}, funcl, {scope, back}, visib,
                         {sup, back}, funref]).

module_module_calls() ->
    L = lists:flatten([[{Module, Call} || Call <- function_calls(Module)]
                       || Module <- modules()]),
    lists:map(
      fun({Module, Call}) ->
              {module, CallerMod} = ?ESG:data(Module),
              {module, CalledMod} =
                  ?ESG:data(hd(?ESG:path(Call, [{func, back}]))),
              {func, _, Fun, Arity} = ?ESG:data(Call),
              {CallerMod, CalledMod, Fun, Arity}
      end,
      L).

%% @doc Returns the interface functions of the given cluster.
interface_funs(Cluster) ->
    [{CallerMod, CalledMod, _Fun, _Arity} ||
        {CallerMod, CalledMod, _Fun, _Arity} <- module_module_calls(),
        not lists:member(CallerMod, Cluster),
        lists:member(CalledMod, Cluster)].

fun_description({_CallerMod, CalledMod, Fun, Arity}) ->
    atom_to_list(CalledMod) ++ ":" ++ atom_to_list(Fun) ++ "/"
        ++ integer_to_list(Arity).
