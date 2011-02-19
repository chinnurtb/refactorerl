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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the move function refactoring.
%%% First of all, analyzes the macro and record usage, the
%%% moving function names and than confronts with the stored information of
%%% the target module. Checks the result for conflicts, and when the
%%% result meet all of the conditions, starts performing the function move.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>


-module(referl_tr_move_fun).
-vsn("$Rev: 1996 $").
-include("refactorerl.hrl").
-import(lists, [filter/2, usort/1, member/2, flatten/1]).

%%% ============================================================================
%%% Exports

%% Interface
-export([do/3]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%%% ============================================================================
%%% Refactoring state

%% State record
-record(state, {from, frommodule, fromfile,
                fnlist, fnforms, fnnodes,
                target, targetmodule, targetfile,

                f_existing_names = [],
                t_existing_names = [],
                used_nodes_names = [],
                used_included    = [],
                clash            = [],
                recmacinfo       = []}).

%% type: import | export
%% list: the containing list-expr node
%% item: the expr
-record(list_del, {type, list, item}).
-record(list_add, {type, file, module, name, arity}).

%%% ============================================================================
%%% Errors

throw_target_not_found() ->
    throw("target module not found").

throw_source_not_found() ->
    throw("source module not found").

throw_name_conflict(Args) ->
    throw("name conflict " ++ ?MISC:format("(~p)", [Args])).

throw_recmac_local_conflict() ->
    throw("local defined entities conflict in target").

throw_unincludables(Args) ->
    throw("can't include headers in target " ++ ?MISC:format("(~p)", [Args])).

throw_includes_not_match(Arg1, Arg2) ->
    throw("target includes do not match the requested includes.~n" ++ 
          "Needed: "   ++ ?MISC:format("~p", [Arg1]) ++ 
          "~nExists: " ++ ?MISC:format("~p", [Arg2])).

%%% ============================================================================
%%% Interface

%% @spec do(string(), [{atom(), integer()}], string()) -> ok
%% @doc Moves the specified function definitions from a module
%% to an another one. The moving is compensated.
do(From, FnList, Target) ->
    ?TRANSFORM:do(?MODULE, {From, FnList, Target}).

%%% ============================================================================
%%% Callbacks

%% @private
init({From, FnList, Target}) -> #state{from=From, fnlist=FnList, target=Target}.


%% @private
steps() ->
    [
     fun check_target_module/1,
     fun check_from_module/1,
     fun query_fnforms/1,
     fun query_fnnodes/1,
     fun check_funnames/1
    ]
        ++ steps(record)
        ++ steps(macro).

%% @private
steps(Tag) ->
    [fun(St) -> Fun(Tag, St) end || Fun <- recmac_steps()].

recmac_steps() ->
    [
     fun query_existing_names/2,
     fun query_used_nodes_names/2,
     fun calc_recmac_clash/2,
     fun calc_used_included/2,
     fun check_recmac/2,
     fun calc_recmac_info/2
    ].


%% @private
transform(St = #state{fromfile = FFile, frommodule = FModule,
                      targetfile = TFile, fnforms = FnForms, fnnodes = FnNodes,
                      recmacinfo = RecmacInfo}) ->

    ImportedFuns = ?GRAPH:path(FModule, [funimp]),

    RecordInfo = recmac_component(record, RecmacInfo),
    MacroInfo  = recmac_component(macro,  RecmacInfo),
    [correct_recmac(record, Info, St) || Info <- RecordInfo],
    [correct_recmac(macro,  Info, St) || Info <- MacroInfo],
    CompB = [correct_body(FunForm, ImportedFuns, St) || FunForm <- FnForms],
    CompR = [correct_refs(FunNode, Expr, St) || FunNode <- FnNodes,
                                                Expr <- funrefs(FunNode)],
    
    ?ESG:close(),

    {ChangedFiles, AppChanges, ListAdds, ListRemoves} =
        analyse_changes(flatten(CompB ++ CompR), FFile, TFile),

    [expimp_list_adds(I) || I <- ListAdds],
    [expimp_list_dels(List, Exprs, FModule, FnForms) ||
        {List, Exprs} <- format_list_removings(ListRemoves)],

    [?MANIP:move_form(F, FFile, TFile) || F <- FnForms],

    [?MANIP:refresh_semantic_links(funref, App) || {app_update, App} <- AppChanges],

    ?ESG:close(),

    {ChangedFiles, ok}.

analyse_changes(Changes, FFile, TFile) ->
    FileChanges = filterpairs(Changes, file),
    AppChanges  = filterpairs(Changes, app_update),

    ChangedFiles =
        usort([File || {file, File} <- FileChanges] ++ [FFile, TFile]),

    ListChanges = usort(Changes -- FileChanges -- AppChanges),
    {ListAdds, ListRemoves} =
        lists:foldl(
          fun(A = #list_add{}, {As, Rs}) -> {[A|As], Rs};
             (R = #list_del{}, {As, Rs}) -> {As, [R|Rs]};
             (_, {As, Rs})               -> {As, Rs}
          end,
          {[], []},
          ListChanges),
    {ChangedFiles, AppChanges, ListAdds, ListRemoves}.


%%% ============================================================================
%%% Implementation

recmac_component(Tag, InfoPair) ->
    {value, {Tag, Info}} = lists:keysearch(Tag, 1, InfoPair), Info.
%%    hd([I || {T, I} <- Info, T == Tag]).


query_fnforms(St=#state{frommodule = FromModule,
                        fnlist     = FnList}) ->
    case ?SYNTAX:function_forms(FromModule, FnList) of
        {error, Error} -> {error, Error};
        FnForms        -> St#state{fnforms = FnForms}
    end.

query_fnnodes(St = #state{fnforms=FnForms}) ->
    St#state{
      fnnodes = [Node || F <- FnForms, Node <- ?GRAPH:path(F, [fundef])]}.


query_existing_names(Tag, St = #state{fromfile = FromFile,
                                      targetfile = TargetFile,
                                      f_existing_names = FE,
                                      t_existing_names = TE}) ->
    NFE = FE ++ [{Tag, ?LEX:existing_names(Tag, FromFile)}],
    NTE = TE ++ [{Tag, ?LEX:existing_names(Tag, TargetFile)}],
    St#state{f_existing_names = NFE, t_existing_names = NTE}.

query_used_nodes_names(Tag, St = #state{fnforms = FnForms,
                                        used_nodes_names = UN}) ->
    NUN = UN ++ [{Tag, ?LEX:used_nodes_names(Tag, FnForms)}],
    St#state{used_nodes_names = NUN}.

calc_recmac_clash(Tag, St = #state{f_existing_names = FEN,
                                   t_existing_names = TEN,
                                   used_nodes_names = UNN,
                                   clash = StClash}) ->
    {LocalInFrom, _, _} = recmac_component(Tag, FEN),
    {_, _, AllInTarget} = recmac_component(Tag, TEN),
    {_, UsedNames}      = recmac_component(Tag, UNN),
    Clash = ?MISC:intersect(AllInTarget, UsedNames),
    ClashAndLocal = {Clash, ?MISC:intersect(Clash, LocalInFrom)},
    St#state{clash = StClash ++ [{Tag, ClashAndLocal}]}.

calc_used_included(Tag, St = #state{f_existing_names = FEN,
                                    used_nodes_names = UNN,
                                    used_included = StUI}) ->
    {_, Included, _} = recmac_component(Tag, FEN),
    {_, UsedNames}   = recmac_component(Tag, UNN),
    UsedIncluded = ?MISC:intersect(Included, UsedNames),
    St#state{used_included = StUI ++ [{Tag, UsedIncluded}]}.


calc_recmac_info(Tag, St = #state{fnforms = FnForms,
                                  used_nodes_names = StUN,
                                  used_included = StUI,
                                  recmacinfo=StRecmacInfo}) ->
    {UsedNodes, _} = recmac_component(Tag, StUN),
    UsedIncluded   = recmac_component(Tag, StUI),
    RecmacInfo =
        [recmac_info(Node, FnForms, UsedIncluded, ?LEX:reflink(Tag))
         || Node <- UsedNodes],
    St#state{recmacinfo = StRecmacInfo ++ [{Tag, RecmacInfo}]}.

recmac_info(Node, FnForms, UsedIncluded, RefLink) ->
    {_, Name} = ?GRAPH:data(Node),
    Removable = recmac_referer_funforms(RefLink, Node) -- FnForms == [],
    [Form]    = form_of_entity(RefLink, Node, Name),
    case lists:member(Name, UsedIncluded) of
        true  -> {    include, Node, Form, Removable};
        false -> {localdefine, Node, Form, Removable}
    end.

form_of_entity(recref, Node, _) ->
    ?GRAPH:path(Node, [{recdef, back}]);

form_of_entity(mref, Node, Name) ->
    ?GRAPH:path(Node, [{macro, back}] ++ ?SYNTAX:form_def_path(Name)).

recmac_referer_funforms(recref, Node) ->
    Path1 = [sup, {visib, back}],
    usort(recmac_referer_funforms(Node, recref, Path1));

recmac_referer_funforms(mref, Node) ->
    Path1 = [{clex, back}],
    Path2 = [{elex, back}, sup, {visib, back}],
    lists:umerge(
      recmac_referer_funforms(Node, mref, Path1),
      recmac_referer_funforms(Node, mref, Path2)).

recmac_referer_funforms(Node, Link, Path) ->
    ?GRAPH:path(Node, [{Link, back}] ++ Path ++ [scope, functx, {funcl, back}]).

%%% ----------------------------------------------------------------------------
%%% Checks

check_target_module(St=#state{target=Target}) ->
    case ?SYNTAX:file_by_modulename(Target) of
        {TMod, TFile} ->  St#state{targetmodule = TMod, targetfile = TFile};
        error         ->  throw_target_not_found()
    end.

check_from_module(St=#state{from=From}) ->
    case ?SYNTAX:file_by_modulename(From) of
        {FMod, FFile} ->  St#state{frommodule = FMod, fromfile = FFile};
        error         ->  throw_source_not_found()
    end.

check_funnames(#state{targetmodule=TargetModule, fnlist=FnList}) ->
    Clash = [{Name, Arity} || {Name, Arity} <- FnList,
                                   ?SEMINF:fun_exists(TargetModule, Name, Arity)],
    case Clash of
        [] -> ok;
        _  -> throw_name_conflict(Clash)
    end.

check_recmac(Tag, St = #state{clash = StClash}) ->
    {Clash, LocalClash} = recmac_component(Tag, StClash),
    case Clash of
        [] ->
            check_include_conflicts(Tag, St);
        _ ->
            case LocalClash of
                [] ->
                    check_same_includes(Tag, St);
                _  ->
                    throw_recmac_local_conflict()
            end
    end.

check_include_conflicts(Tag, #state{fromfile = FromFile,
                                    targetfile = TargetFile,
                                    used_included = StUI}) ->
    UsedIncluded  = recmac_component(Tag, StUI),
    WithInclFile = [{?LEX:included_from_file(FromFile, Entity, Tag), Entity}
                    || Entity <- UsedIncluded],
    Conflicts = [{(?GRAPH:data(InclFile))#file.path, Entity}
                 || {InclFile, Entity} <- WithInclFile,
                    not ?LEX:includable(TargetFile, InclFile)],
    case Conflicts of
        [] ->
            ok;
        _ ->
            throw_unincludables(Conflicts)
    end.

check_same_includes(Tag, #state{fromfile = FromFile,
                                targetfile = TargetFile,
                                clash = StClash}) ->
    {Clash, _} = recmac_component(Tag, StClash),
    InclClashPaths =
        usort([?LEX:included_from_path(FromFile, Name, Tag) || Name <- Clash]),
    T_Includes =
        usort([Path || I <- ?GRAPH:path(TargetFile, [incl]),
                       #file{path=Path} <- [?GRAPH:data(I)]]),
    case InclClashPaths -- T_Includes of
        [] ->
            ok;
        _ ->
            throw_includes_not_match(InclClashPaths, T_Includes)
    end.


%%% ============================================================================
%%% Compensations

%%% ----------------------------------------------------------------------------
%%% Macro/record

correct_recmac(_, {localdefine, _Node, Form, false}, #state{targetfile=TFile}) ->
    {value, {_, NewForm}} = lists:keysearch(Form, 1, ?ESG:copy(Form)),
    ?MANIP:insert_form(TFile, NewForm);

correct_recmac(_, {localdefine, _Node, Form, true},
               #state{fromfile = FFile, targetfile = TFile}) ->
    ?MANIP:move_form(Form, FFile, TFile);

correct_recmac(Tag, {include, Node, _Form, Removable},
               #state{fromfile=FFile, targetfile=TFile}) ->
    [IncludeFile]    = ?GRAPH:path(Node, [{Tag, back}]),
    #file{path=Path} = ?GRAPH:data(IncludeFile),
    IncludeForm      = ?GRAPH:path(FFile, ?SYNTAX:form_inc_path(Path)),
    case {IncludeForm, Removable} of
        {[F], true}  -> ?MANIP:move_include(F, FFile, TFile);
        {[F], false} -> ?MANIP:copy_include(F, TFile);
        {_, _}       -> ok
    end.

%%% ----------------------------------------------------------------------------
%%% Moved function references

correct_refs(Fun, Expr, St = #state{targetfile   = TFile,
                                    targetmodule = TargetMod}) ->
    case [?GRAPH:data(A) || A <- ?GRAPH:path(Expr, [sup, {attr, back}])] of
        [#form{type = attrib, tag = export}] ->
            correct_export_refs(Expr, Fun, TFile);
        [#form{type = attrib, tag = import}] ->
            correct_import_refs(Expr, Fun, TargetMod);
        [] ->
            correct_module_qualifier(Expr, St)
    end.

correct_export_refs(Expr, Fun, TFile) ->
    [ExportForm] = ?GRAPH:path(Expr, [sup, {attr, back}]),
    [ExportList] = ?GRAPH:path(ExportForm, [attr]),
    #func{name=Name, arity=Arity} = ?GRAPH:data(Fun),
    [#list_del{type=export, list=ExportList, item=Expr},
     #list_add{type=export, file=TFile, name=Name, arity=Arity}].

correct_import_refs(Expr, Fun, TargetMod) ->
    [ImportForm] = ?GRAPH:path(Expr, [sup, {attr, back}]),
    [Mod] = ?GRAPH:path(ImportForm, [{form, back}, moddef]),
    Attrs = ?GRAPH:path(ImportForm, [attr]),
    case {Attrs, Mod}  of
        {[_, ImportList], TargetMod} ->
            #list_del{type=import, list=ImportList, item=Expr};
        {[_, ImportList], _} ->
            [File] = ?GRAPH:path(Mod, [{moddef, back}]),
            #func{name=Name, arity=Arity} = ?GRAPH:data(Fun),
                                                %?MANIP:add_import(File, TargetMod, Name, Arity),
            [{file, File},
             #list_del{type=import, list=ImportList, item=Expr},
             #list_add{type=import, file=File, module=TargetMod,
                       name=Name, arity=Arity}];
        {_,TargetMod} ->
            ?MANIP:remove_form(ImportForm),
            [];
        {_, _} ->
            ?MANIP:rename_import(ImportForm, TargetMod),
            {file, ?GRAPH:path(Mod, [{moddef, back}])}
    end.


%%% ----------------------------------------------------------------------------
%%% Module qualifiers


correct_module_qualifier(Expr, #state{frommodule   = FromMod,
                                                %fromfile     = FFile,
                                      targetmodule = TargetMod,
                                      targetfile   = TFile,
                                      fnnodes      = FnNodes}) ->
    [File]           = ?GRAPH:path(Expr, [sup, {visib, back}, scope, functx,
                                          {funcl, back}, {form, back}]),
    [ModOrigin]      = ?GRAPH:path(File, [moddef]),
    [Referred]       = ?GRAPH:path(Expr, [funref]),
    [FunIsReferring] = ?GRAPH:path(Expr, [sup, {body, back}, scope,
                                          functx, {funcl, back}, fundef]),
    case {ModOrigin, ?SYNTAX:module_qualifier(Expr)} of
        {TargetMod, FromMod} ->
            ?MANIP:remove_module_qualifier(Expr),
            [{file, File}, {app_update, Expr}];
        {FromMod, MQ} when MQ == no_module_qualifier orelse MQ == false ->
            ReferredMoved = lists:member(Referred, FnNodes),
            RefererMoved  = lists:member(FunIsReferring, FnNodes),
            #func{name=Name, arity=Arity} = ?GRAPH:data(Referred),
            case {ReferredMoved, RefererMoved} of
                {true, true} ->
                    [#list_add{type=export, file=TFile, name=Name, arity=Arity}];
                {true, false} ->
                    ?MANIP:insert_module_qualifier(Expr, TargetMod),
                    [{file, File}, {app_update, Expr},
                     #list_add{type=export, file=TFile, name=Name, arity=Arity}];
                %%{false, true} ->
                %%    insert_module_qualifier(Expr, FromMod),
                %%    [{file, File}, {app_update, Expr},
                %%     #list_add{type=export, file=FFile, name=Name, arity=Arity}];
                _ ->
                    []
            end;
        {_, FromMod} ->
            ?MANIP:update_module_qualifier(Expr, TargetMod),
            [{file, File}, {app_update, Expr}];
        {_, _} ->
            []
    end.


%%% ----------------------------------------------------------------------------
%%% Applications in moved bodies


correct_body(FunForm, ImportedFuns, St) ->
    Expressions = ?GRAPH:path(FunForm, path_form_expressions()),
    flatten([correct_application(Expr, ImportedFuns, St) ||
                Expr <- Expressions]).


correct_application(Expr, ImportedFuns,
                    St=#state{fnnodes=FnNodes, targetmodule=TModule}) ->
    [ReferredFun] = ?GRAPH:path(Expr, [funref]),
    ReferredMoved = lists:member(ReferredFun, FnNodes),
    case {ReferredMoved, ?SYNTAX:module_qualifier(Expr)} of
        {false, TModule} ->
            ?MANIP:remove_module_qualifier(Expr),
            [{app_update, Expr}];
        {false, no_module_qualifier} ->
            correct_application_has_no_qualifier(Expr, ReferredFun,
                                                 ImportedFuns, St);
        {_, _} ->
            []
    end.

%% REM DIAL the tuple [{1, _ImportForm, Module}] never matches now
correct_application_has_no_qualifier(Application, ReferredFun, ImportedFuns,
                                     #state{fromfile=FFile, targetfile=TFile, frommodule=FModule}) ->
    Imported = lists:member(ReferredFun, ImportedFuns),
    #func{name=Name, arity=Arity} = ?GRAPH:data(ReferredFun),
    case Imported of
        true ->
            case ?SEMINF:import_list_refs(FFile, ReferredFun) of
                [{1, _ImportForm, Module}] ->
                                                %?MANIP:add_import(TFile, Module, Name, Arity),
                    [#list_add{type=import, file=TFile, module=Module,
                               name=Name, arity=Arity}];
                [{2, ImportList, ImpExpr, Module}] ->
                                                %?MANIP:add_import(TFile, Module, Name, Arity),
                    [#list_del{type=import, list=ImportList, item=ImpExpr},
                     #list_add{type=import, file=TFile, module=Module,
                               name=Name, arity=Arity}];
                [no_refs] -> []
            end;
        false ->
            case ?GRAPH:path(ReferredFun, [{fundef, back}]) of
                [] ->
                    %% BIF
                    [];
                _ ->
                    %% local function in from
                    ?MANIP:insert_module_qualifier(Application, FModule),
                    [#list_add{type=export, file=FFile, name=Name, arity=Arity},
                     {app_update, Application}]
            end
    end.

%%% ----------------------------------------------------------------------------
%%% Export/import lists


expimp_list_adds(#list_add{type = Type, file = File,module = Module,
                           name = Name, arity = Arity}) ->
    case Type of
        export -> ?MANIP:add_export(File,  Name, Arity);
        import -> ?MANIP:add_import(File, Module, Name, Arity)
    end.

expimp_list_dels(List, Exprs, _FModule, FnForms) ->
    AllApps = apps_funexprs(FnForms),
    File = ?SEMINF:parent_file(List),
    Removable = filter_removable_lists(Exprs, File, AllApps),
    RemExprs = [E || {_, E} <- Removable],
    case ?GRAPH:path(List, [sub]) -- RemExprs of
        [] ->
            [Form] = ?GRAPH:path(List, [{attr, back}]),
            ?MANIP:remove_form(Form);
        _ ->
            [?MANIP:remove_expimp(List, Expr) || {_, Expr} <- Removable]
    end.


%% @doc We remove that import/export lists, which are removable:
%%      have no other referers, just the moved applications.
filter_removable_lists(Exprs, File, AllApps) ->
    lists:filter(
      fun
          ({export, _}) ->
              true;
          ({import, Expr}) ->
              referers(Expr, File) -- AllApps == []
      end,
      Exprs).

%% @doc Which expressions are applying the imported function
%%      in the source module?
referers(ImportExpr, File) ->
    [FunRef ||
        FunRef <- ?GRAPH:path(ImportExpr, [funref, {funref, back}]),
        is_same_module_ref(FunRef, File)].

is_same_module_ref(FunRef, File) ->
    SameModule = fun() -> ?SEMINF:parent_file(FunRef) == File end,
    NoQualifier = fun() -> ?SYNTAX:module_qualifier(FunRef) == no_module_qualifier orelse
                               ?SYNTAX:module_qualifier(FunRef) == false end,
    case ?GRAPH:data(FunRef) of
        #expr{kind=application}  -> SameModule() andalso NoQualifier();
        #expr{kind=implicit_fun} -> SameModule();
        _                        -> false
    end.


%%% ----------------------------------------------------------------------------
%%% Short functions


filterpairs(List, Atom) ->
    lists:usort(lists:filter(
                  fun(Elem) ->
                          case Elem of
                              {Atom, _} -> true;
                              _ -> false
                          end
                  end,
                  List)).

path_form_expressions() ->
    [funcl, {functx,back}, 
     {scope,back}, visib, 
     {{sup, back}, 
      {{kind, '==', application}, 'or', 
       {kind, '==', implicit_fun}
      }
     }
    ].

apps_funexprs(FnForms) ->
    [App || Form <- FnForms,
            App <- ?GRAPH:path(Form, path_form_expressions()),
            #expr{type=Type} <- [?GRAPH:data(App)],
            Type == application orelse Type == fun_expr].

format_list_removings(ListRemoves) ->
    Lists = lists:usort([List || #list_del{list=List} <- ListRemoves]),
    [{List,
      [{Type, Expr} || #list_del{type=Type, item=Expr, list=L} <- ListRemoves,
                       L == List]}
     || List <- Lists].

funrefs(Fun) -> ?GRAPH:path(Fun, [{funref, back}]).
