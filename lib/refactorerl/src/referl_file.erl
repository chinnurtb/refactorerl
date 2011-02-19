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

%%% @doc File properties and file based queries

-module(referl_file).
-vsn("$Rev: 2565 $").
-include("refactorerl.hrl").

-import(?MISC, [to_atom/1, to_list/1]).

%% =============================================================================
%% Exports

%% Properties
-export([path/1, length/1, type/1, includable/2]).

%% Queries
-export([token/1, token/2,
         find/1, module/0, forms/0, form/1,
         include_form/1, includes/0, included/0,
         records/0, record/1, macros/0, macro/1]).

%% Manips
-export([add_include/2, del_include/2, add_form/2, add_form/3,
         del_form/1, del_form/2, upd_path/2]).


%% =============================================================================
%% File related properties

%% @spec path(node()) -> string()
%% @doc Returns the stored path of the file.
path(FileNode) ->
    (?Graph:data(FileNode))#file.path.

%% @spec type(node()) -> atom()
%% @doc Returns the type of the file (module or header).
type(FileNode) ->
    (?Graph:data(FileNode))#file.type.

%% @spec length(node()) -> integer()
%% @doc Return the length of the file (number of characters).
length(FileNode) ->
    erlang:length(lists:flatten(?Syn:tree_text(FileNode))).

%% @spec includable(node(#file{}), node(#file{type=header})) -> bool()
%% @doc Returns true if record and macro names do not conflict after
%% the inclusion. The names are collected and checked recursively in
%% header files.
includable(Target, Incl) ->
    M1 = existing_ent_with_source(Target, macro),
    M2 = existing_ent_with_source(Incl,   macro),
    R1 = existing_ent_with_source(Target, record),
    R2 = existing_ent_with_source(Incl,   record),
    {MN1, _} = lists:unzip(M1--M2), %% Is in Target, but isn't in Incl
    {MN2, _} = lists:unzip(M2--M1), %% Is in Incl, but isn't in Target
    {RN1, _} = lists:unzip(R1--R2),
    {RN2, _} = lists:unzip(R2--R1),
    ?MISC:intersect(MN1, MN2) == [] andalso ?MISC:intersect(RN1, RN2) == [].

existing_ent_with_source(File, Tag) ->
    FileQuery = case Tag of
                    record -> records();
                    macro  -> macros()
                end,
    GetName = case Tag of
                  record -> fun ?Rec:name/1;
                  macro  -> fun ?Macro:name/1
              end,
    ?MISC:flatsort(
       [{GetName(Node), File} || Node <- ?Query:exec(File, FileQuery)]
       ++ [existing_ent_with_source(F, Tag) ||
              F <- (?Query:exec(File, includes()) -- [File])]).


%% =============================================================================
%% Queries starting from files

%% @spec token(Pos::integer()) -> query(#file{}, #lex{})
%% @doc Returns the token that is at the specified position  in the file text.
token(Pos) when Pos > 0 ->
    token(Pos, both).

%% @spec token(Pos::integer(), Ws::none|pre|post|both) -> query(#file{}, #lex{})

%% @doc Returns the token that is at the specified position in the
%% file text, but when `Ws' is given:
%% <ul>
%%   <li>`none' means whitespace is not accepted</li>
%%   <li>`pre' means whitespace is handled as prefix (the next token
%%   is the result)</li>
%%   <li>`post' means whitespace is postfix (the previous token is the
%%   result)</li>
%%   <li>`both' means both prefix and postfix are accepted</li>
%% </ul>
token(Pos, Ws) when Pos > 0 ->
    fun (File) ->
            ?Token:foldpos(
               fun (Node, _D, Start, End, _) when Start =< Pos, Pos =< End -> 
                       {stop, [Node]};
                   (_,_,_,_,Acc) -> 
                       {next, Acc}
               end, [], File, Ws)
    end.

%% @spec find(string()) -> query(root(), #file{})
%% @doc Finds file node by its path.
find(Path) -> [{file, {path, '==', Path}}].

%% @spec module() -> query(#file{}, #module{})
%% @doc The result query returns the described erlang module.
module() ->
    [moddef].

%% @spec forms() -> query(#file{}, #form{})
%% @doc The result query returns all the contained forms.
forms() ->
    [form].

%% @spec form(integer()) -> query(#file{}, #form{})
%% @doc The result query returns the nth contained form.
form(I) ->
    [{form, I}].

%% @spec includes() -> query(#file{}, #file{})
%% @doc The result query returns the included files.
includes() ->
    [incl].

%% @spec included() -> query(#file{}, #file{})
%% @doc The result query returns files where the file is included.
included() ->
    [{incl, back}].


%% @spec include_form(IncFile::node()) -> query(#file{}, #form{})
%% @doc The result query returns the form that includes `IncFile' in the file.
include_form(IncFile) ->
    [form, {intersect, IncFile, {iref, back}}].


%% @spec records() -> query(#file{}, #record{})
%% @doc The result query returns all the defined records.
records() ->
    [record].

%% @spec record(atom() | string()) -> query(#file{}, #record{})
%% @doc The result query returns the defined record by name .
record(Name) ->
    [{record, {name, '==', to_atom(Name)}}].

%% @spec macros() -> query(#file{}, #form{})
%% @doc The result query returns all the defined macros.
macros() ->
    [{form, {type, '==', macro}}].

%% @spec macro(atom() | string()) -> query(#file{}, #form{})
%% @doc The result query returns the defined macro by name.
macro(Name) ->
    [{form, {{type, '==', macro}, 'and',
             {tag, '==', to_list(Name)}}}].

%%% ============================================================================
%%% Manipulations

%% @spec add_include(node(#file{}), node(#file{}) | string()) -> ok
%% @doc Adds an include form to the file if the include path is not present.
add_include(File, InclPath) when is_list(InclPath) ->
    case lists:member(?Graph:path(?Graph:root(), find(InclPath)), 
                      ?Graph:path(File, includes())) of
        true ->
            ok;
        false ->
            PL1 = filename:split(InclPath),
            PL2 = filename:split(filename:dirname(?File:path(File))),
            Path = filename:join(relative_path(PL1, PL2)),
            Tokens =
                [?ESG:create(#lex{type=token,
                                  data=?Token:build(Type, Text)}) ||
                    {Type, Text} <-
                        [{minus, "-"},
                         {atom, "include"},
                         {op_paren, "("},
                         {string, "\"" ++ Path ++ "\""},
                         {cl_paren, ")"},
                         {stop, ".\n"}]],
            ?FileMan:add_form(File, form_index(File, include), Tokens)
    end;
add_include(File, InclFile) ->
    add_include(File, path(InclFile)).

relative_path([P|Path], [P|Dir]) -> relative_path(Path, Dir);
relative_path(Path, _) -> Path.
    

%% @spec del_include(node(#file{}), node(#file{})) -> ok
%% @doc Removes the including of a file.
%% @todo incl links are not removed, only the include form
del_include(File, InclFile) ->
    [begin ?Graph:rmlink(File, incl, I) end
     || I <- incl_files_recur([InclFile])],
    del_form(File, ?Query:exec1(File, include_form(InclFile),
                                include_not_present)).

incl_files_recur(Files) ->
    case lists:usort(?Query:exec(Files, includes()) -- Files) of
        [] -> Files;
        Is -> lists:usort(Files ++ incl_files_recur(Is))
    end.

%% @spec del_form(node(#form{})) -> ok
%% @doc Removes a form from a file
del_form(Form) ->
    File = ?Query:exec1(Form, ?Form:file(), file_not_found),
    ?ESG:remove(File, form, Form).

%% @spec del_form(node(#file{}), node(#form{})) -> ok
%% @doc Removes a form from a file
del_form(File, Form) ->
    ?ESG:remove(File, form, Form).

%% @spec add_form(node(#file{}), node(#form{})) -> ok
%% @doc Inserts a form below a file, maintaining the order of form types:
%% `func', `module', `export', `import', `include', `macro', `record'.
add_form(File, Form) ->
    ?ESG:insert(File,{form, form_index(File, ?Form:type(Form))}, Form).

%% @spec add_form(node(#file{}), integer(), node(#form{})) -> ok
%% @doc Inserts a form with the given index.
add_form(File, Index, Form) ->
    ?ESG:insert(File,{form, Index}, Form).

%% @spec form_index(node(), atom()) -> integer() | last
%% @doc Determines the index of a new form.
form_index(_File, func) -> last;
form_index(File, Type) ->
    PrevForms   = [Form || Form <- ?Graph:path(File, [form]),
                           lists:member(?Form:type(Form),
                                        antecedent_types(Type))],
    erlang:length(PrevForms) + 1.


%% @spec antecedent_types(atom()) -> [node()]
%% @doc Returns the form types that may appear earlier in the file.
antecedent_types(Type) ->
    List = [module, export, import, include, macro, record],
    lists:takewhile(fun(Elem) -> Elem /= Type end, List) ++ [Type].

%% @spec upd_path(node(), string()) -> node()
%% @doc Updates the path of the file. Note that if this function is called
%% during an active ESG batch, the whole file will be reanalysed.
upd_path(File, NewPath) ->
    Data = ?ESG:data(File),
    ?ESG:update(File, Data#file{path = NewPath}).
