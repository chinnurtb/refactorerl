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

%%% @doc This module implements the move function refactoring.
%%% First of all, analyses the macro and record usage, the
%%% moving function names and than confronts with the stored information of
%%% the target module. Checks the result for conflicts, and when the 
%%% result meet all of the conditions, starts performing the function move.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>


-module(refac_movefun).
-vsn("$Rev: 1335 $").
-behaviour(gen_fsm).

-include("refactorerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([test/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Enviromental exports
-export([start_link/0]).

%% gen_fsm exports
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-export([start/2, question/2]).

%% Client exports
-export([do/3, answer/1, cancel/0]).

-record(state, {questions = [],
                from, frommodule, fromfile, fnlist, fnforms, fnnodes, target,
                targetmodule, targetfile, macroinfo, recordinfo}).

-record(entity_info, {name, node, form, locality, removable}).


%% type: import | export
%% list: the containing list-expr node
%% item: the expr
-record(list_del, {type, list, item}).
-record(list_add, {type, file, module, name, arity}).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ----------------------------------------------------------------------------
%% @spec do(atom(), [{atom(), integer()}], string()) -> ok
%% @doc Interface for doing movefun.
%% The 'From' and 'Target' parameters are module names.
%% @end
%% ----------------------------------------------------------------------------
do(From, FnList, Target) ->
    send_event({do, From, FnList, Target}).


%% ----------------------------------------------------------------------------
%% @spec answer(term()) -> ok
%% @doc Interface for send answer
%% @end
%% ----------------------------------------------------------------------------
answer(Answers) ->
    send_event({answer, Answers}).


%% ----------------------------------------------------------------------------
%% @spec cancel() -> ok
%% @doc Interface for cancel the movefun
%% @end
%% ----------------------------------------------------------------------------
cancel() ->
    send_event(cancel).

%% @private
send_event(Event) ->
    gen_fsm:send_event(?MODULE, Event).
    

%%% ===========================================================================
%%% Server callbacks

init(_) ->
    {ok, start, #state{}}.

handle_event(_, S, D) ->
    {next_state, S, D}.

handle_sync_event(_, _, S, D) ->
    {reply, ok, S, D}.

handle_info(_, S, D) ->
    {next_state, S, D}.

terminate(_, _, _) ->
    ok.

code_change(_, S, D, _) ->
    {ok, S, D}.

start({do, From, FnList, Target}, #state{}) ->
    next_state(fun() -> prepare(From, FnList, Target) end).

%% ----------------------------------------------------------------------------
%% @spec question({atom(), [term()]}, #state{}) -> ok
%% @end
%% ----------------------------------------------------------------------------
question({answer, Answers}, S=#state{}) ->
    next_state(fun() -> answers(S, Answers) end);
question(cancel, #state{}) ->
    {next_state, start, #state{}}.

next_state(Func) ->
    try Func() of
        St = #state{questions = []} ->
            transform(St),
            {next_state, start, #state{}};
        St = #state{questions = Quest} ->
            lists:foreach(
              fun (Q) -> refac_ui:message(question, "~s", [Q]) end,
              Quest),
            {next_state, question, St}
    catch
	throw:{Message, Args} ->
	    refac_ui:message(status, Message, Args),
	    {next_state, start, #state{}};
	  throw:Error ->
	    refac_ui:message(status, "Move error: ~s", [Error]),
	    {next_state, start, #state{}}
    end.


%%% ===========================================================================


%% ----------------------------------------------------------------------------
%% @spec prepare(atom() | string(), 
%%               [{atom(), integer()}],
%%               atom() | string()) 
%%                                      -> #state{}
%% @doc Prepares for function moving, calls the condition checkers
%% @end
%% ----------------------------------------------------------------------------
prepare(From, FnList, Target) ->
    lists:foldl(
      fun (Fun, St) -> Fun(St) end,
      #state{from=From, fnlist=FnList, target=Target},
      [fun check_target_module/1,
       fun check_from_module/1,
       fun check_funnames/1,
       fun check_macros/1,
       fun check_records/1]).

%% @private
answers(St, _Answers) ->
    St.


%% ----------------------------------------------------------------------------
%% private
%% @spec transform(#state{}) -> #state{}
%% @doc Performs the transformation.
%% @end
%% ----------------------------------------------------------------------------
transform(St = #state{fromfile   = FFile,
		      frommodule = FModule,
		      targetfile = TFile, 
		      fnforms    = FnForms,
		      fnnodes    = FnNodes,
		      recordinfo = RecordInfo,
		      macroinfo  = MacroInfo}) ->

    ImportedFuns = ?GRAPH:path(FModule, [funimp]),
    ReferredBy =  [{FunNode, Expr} || 
		      FunNode <- FnNodes, 
		      Expr <- ?GRAPH:path(FunNode, [{funref, back}])],

    Changes = 
	lists:flatten(
	  [correct_references(F, E, St) || {F, E} <- ReferredBy] ++
	  [correct_body(FunForm, ImportedFuns, St) || FunForm <- FnForms] ++
	  [correct_rec_mac(M, St, macro) || M <- MacroInfo] ++
	  [correct_rec_mac(R, St, record) || R <- RecordInfo]),

    FileChanges = filterpairs(Changes, file),
    AppChanges = filterpairs(Changes, app_update),

    ChangedFiles = lists:usort(
		     [File || {file, File} <- FileChanges] ++ 
		     [FFile, TFile]),

    ListChanges = lists:usort(Changes -- FileChanges -- AppChanges),
    {ListAdds, ListRemoves} = 
	lists:foldl(
	  fun(A = #list_add{}, {As, Rs}) -> {[A|As], Rs};
	     (R = #list_del{}, {As, Rs}) -> {As, [R|Rs]};
	     (_, {As, Rs})               -> {As, Rs}
	  end,
	  {[], []},
	  ListChanges),

    lists:foreach(fun(F) -> ?MANIP:move_form(F, FFile, TFile) end, FnForms),

    [handle_list_addings(I) || I <- ListAdds],
    [handle_list_removings(List, Exprs, FModule, FnForms) || 
	{List, Exprs} <- format_list_removings(ListRemoves)],
    [?MANIP:insert_application_again(App) || {app_update, App} <- AppChanges],

    ?ESG:close(),
    [refac_fileman:save_file(F) || F <- ChangedFiles],
    [refac_ui:message(reload, "~s", [(?GRAPH:data(F))#file.path]) ||
                                                   F <- ChangedFiles],
    refac_ui:message(status, "Functions moved, ~p files saved.", 
		     [length(ChangedFiles)]), 
    St.


%% ----------------------------------------------------------------------------
%% @private
%% @spec check_target_module(#state{}) -> #state{}
%% @end
%% ----------------------------------------------------------------------------
check_target_module(St=#state{target=Target}) ->
    try
	if is_atom(Target) -> TargetAsAtom = Target;
	   true ->            TargetAsAtom = list_to_existing_atom(Target)
	end,
	case ?GRAPH:path(?GRAPH:root(),
			 [{module, {name, '==', TargetAsAtom}}]) of
	    [Mod] ->
		[File] = ?GRAPH:path(Mod, [{moddef, back}]),
		St#state{targetmodule = Mod, targetfile = File};
	    [] ->
		throw("Target module not found")
	end
    catch error:badarg ->
	    throw("Target module not found")
    end.


%% ----------------------------------------------------------------------------
%% @private
%% @spec check_from_module(#state{}) -> #state{}
%% @end
%% ----------------------------------------------------------------------------
check_from_module(St=#state{from=From, fnlist=FnList}) ->
    try
	if is_atom(From) -> FromAsAtom = From;
	   true ->          FromAsAtom = list_to_existing_atom(From)
	end,
	case ?GRAPH:path(?GRAPH:root(), 
			 [{module, {name, '==', FromAsAtom}}]) of
	    [Mod] ->
		[File] = ?GRAPH:path(Mod, [{moddef, back}]),
		{FnForms, FnNodes} = ?QUERY:function_forms_and_nodes(Mod, FnList),
		St#state{frommodule = Mod, fromfile = File, 
			 fnforms=FnForms, fnnodes=FnNodes};
	    [] ->
		throw("Source module not found")
	end
    catch error:badarg ->
	    throw("Target module not found")
    end.


%% ----------------------------------------------------------------------------
%% @private
%% @spec check_funnames(#state{}) -> #state{}
%% @doc Looks for function name conflicts.
%% @end
%% ----------------------------------------------------------------------------
check_funnames(St=#state{targetmodule = TargetModule, 
			 fnlist       = FnList}) ->
    Clash = lists:filter(fun({Name, Arity}) ->
                                 ?QUERY:function_exists(TargetModule, Name, Arity)
                         end, FnList),
    case Clash of
        [] -> St;
        _  -> throw({"Name conflict: ~p", [Clash]})
    end.


%% ----------------------------------------------------------------------------
%% @private
%% @spec check_macros(#state{}) -> #state{}
%% @doc Checks the macro using and its conditions.
%% @end
%% ----------------------------------------------------------------------------
check_macros(State=#state{}) -> 
    check_entities(State, 
		   fun ?QUERY:existing_macronames/1, 
		   fun ?QUERY:used_macros/1, 
		   macro).


%% ----------------------------------------------------------------------------
%% @private
%% @spec check_records(#state{}) -> #state{}
%% @doc Checks the record using and its conditions.
%% @end
%% ----------------------------------------------------------------------------
check_records(State=#state{}) -> 
    check_entities(State, 
		   fun ?QUERY:existing_recordnames/1, 
		   fun ?QUERY:used_records/1, 
		   record).


%%% ===========================================================================
%%%                    FUNCTION REFERENCE CORRECTION
%%% ===========================================================================
%%%
%%% The corrector functions return information about
%%% - which files were modified
%%% - which export/import lists/items need to add (#list_add)
%%% - which export/import lists/items need to remove (#list_del)
%%% - which function applications need readding to the graph
%%%   (this is for semantical link correction)


%% ----------------------------------------------------------------------------
%% @doc Corrects the reference of Fun.
%%      Expr has a funref link to Fun and we need to analyse the dependences.
%%      If it is needed, we compense. Used compensations:
%%      - export, import list adding, deleting
%%      - module qualifier inserting, removing
%%
%%      References can be:
%%      - export list items
%%      - import list items
%%      - function applications
%%      - implicit fun expressions
%% @end
%% ----------------------------------------------------------------------------
correct_references(Fun, Expr, St = #state{targetfile   = TFile, 
					  targetmodule = TargetMod}) ->

    case [?GRAPH:data(A) || A <- ?GRAPH:path(Expr, [sup, {attr, back}])] of

	[#form{type = attrib, tag = export}] ->
	    [ExportForm] = ?GRAPH:path(Expr, [sup, {attr, back}]),
	    [ExportList] = ?GRAPH:path(ExportForm, [attr]),
	    #func{name=Name, arity=Arity} = ?GRAPH:data(Fun),
	    %% have to do:
	    [#list_del{type=export, list=ExportList, item=Expr},
	     #list_add{type=export, file=TFile, name=Name, arity=Arity}];

	[#form{type = attrib, tag = import}] ->
	    [ImportForm] = ?GRAPH:path(Expr, [sup, {attr, back}]),
	    [Mod] = ?GRAPH:path(ImportForm, [{form, back}, moddef]),
	    case length(?GRAPH:path(ImportForm, [attr])) of
		1 ->
		    if Mod == TargetMod ->
			    ?MANIP:remove_import(ImportForm),
			    %% nothing to do
			    [];
		       true ->
			    ?MANIP:rename_import(ImportForm, TargetMod),
			    %% file is modified
			    {file, ?GRAPH:path(Mod, [{moddef, back}])}
		    end;
		2 ->
		    [ImportList] = ?GRAPH:path(ImportForm, [{attr, 2}]),
		    if Mod == TargetMod ->
			    #list_del{type=import, list=ImportList, item=Expr};
		       true ->
			    [File] = ?GRAPH:path(Mod, [{moddef, back}]),
			    #func{name=Name, arity=Arity} = ?GRAPH:data(Fun),
			    ?MANIP:create_and_add_import(File, TargetMod, Name, Arity),
			    [{file, File},
			     #list_del{type=import, list=ImportList, item=Expr},
			     #list_add{type=import, file=File, module=TargetMod, 
				       name=Name, arity=Arity}]
		    end
	    end;
	[] ->
	    correct_module_qualifier(Expr, St)
    end.


%% ----------------------------------------------------------------------------
%% @doc Corrects the body of the specified function.
%%      In the function's body there's references to another
%%      functions, which references/applications need to analyse and
%%      compense. We search the function applications in the body
%%      and call analyse_application for each application.
%% @end
%% ----------------------------------------------------------------------------
correct_body(FunForm, ImportedFuns, St) ->
    lists:flatten(
      [analyse_application(App, ImportedFuns, St) 
       || App <- ?QUERY:applications([FunForm])]).


analyse_application(Application, ImportedFuns, 
		    St = #state{fnnodes = FnNodes}) ->
    
    case ?GRAPH:path(Application, [funref]) of
	[ReferredFun] ->
	    AlreadyMoved = lists:member(ReferredFun, FnNodes),
	    if AlreadyMoved ->
		    [];
	       true ->
		    correct_application(Application, ReferredFun, ImportedFuns, St)
	    end;
	[] ->
	    []
    end.


correct_application(Application, ReferredFun, ImportedFuns, 
		    #state{targetfile   = TFile, 
			   targetmodule = TModule, 
			   fromfile     = FFile, 
			   frommodule   = FModule}) ->

    case ?QUERY:has_module_qualifier(Application) of
	TModule ->
	    ?MANIP:remove_module_qualifier(Application),
	    [{app_update, Application}];
	false ->
	    Imported = lists:member(ReferredFun, ImportedFuns),
	    #func{name=Name, arity=Arity} = ?GRAPH:data(ReferredFun),
	    if Imported ->
		    case ?QUERY:referer_importlist(FFile, ReferredFun) of
			[{1, _ImportForm, Module}] ->
			    %% TODO: can we delete the file import?
			    %%    ?MANIP:remove_import(ImportForm),
			    ?MANIP:create_and_add_import(TFile, Module, Name, Arity),
			    [#list_add{type=import, file=TFile, module=Module,
				       name=Name, arity=Arity}];
			[{2, ImportList, ImpExpr, Module}] ->
			    ?MANIP:create_and_add_import(TFile, Module, Name, Arity),
			    [#list_del{type=import, list=ImportList, item=ImpExpr},
			     #list_add{type=import, file=TFile, module=Module,
				       name=Name, arity=Arity}];
			[] -> []
		    end;
	       true ->
		    %% BIFs (function objects without definition)
		    %% do not need module qualifiers
		    case ?GRAPH:path(Application, [funref, {fundef, back}]) of
			[] -> [];
			_ ->
			    insert_module_qualifier(Application, FModule),
			    [#list_add{type=export, file=FFile, name=Name, arity=Arity},
			     {app_update, Application}]
		    end
	    end;
	_ ->
	    []
    end.


correct_module_qualifier(Expr, #state{frommodule   = FromMod, 
				      targetmodule = TargetMod, 
				      targetfile   = TFile,
				      fnnodes      = FnNodes}) ->
    Mod = expr_module(Expr),
    [File] = ?GRAPH:path(Mod, [{moddef, back}]),
    [Referred] = ?GRAPH:path(Expr, [funref]),
    [FunIsReferring] = ?GRAPH:path(Expr, [sup, {body, back}, 
					  scope, {funcl, back}, fundef]),
    case Mod of
	TargetMod ->
	    case ?QUERY:has_module_qualifier(Expr) of
		FromMod ->
		    ?MANIP:remove_module_qualifier(Expr),
		    [{file, File}, {app_update, Expr}];
		_ ->
		    []
	    end;
	FromMod ->
	    ReferredMoved = lists:member(Referred, FnNodes),
	    case ReferredMoved of
		true ->
		    #func{name=Name, arity=Arity} = ?GRAPH:data(Referred),
		    BothMoved = lists:member(FunIsReferring, FnNodes),
		    case BothMoved of
			true ->
			    [#list_add{type=export, file=TFile, name=Name, arity=Arity}];
			false ->
			    insert_module_qualifier(Expr, TargetMod),
			    [{file, File}, {app_update, Expr},
			     #list_add{type=export, file=TFile, name=Name, arity=Arity}]
		    end;
		false ->
		    []
	    end;
	_ ->
	    case ?QUERY:has_module_qualifier(Expr) of
		FromMod ->
		    ?MANIP:update_module_qualifier(Expr, TargetMod),
		    [{file, File}, {app_update, Expr}];
		_ ->
		    []
	    end
    end.


insert_module_qualifier(Expr, Mod) ->
    case ?GRAPH:data(Expr) of

	#expr{kind=application} ->
	    ?MANIP:insert_module_qualifier(Mod, Expr);

	#expr{kind=fun_expr} ->
	    refac_extract_funexpr:extract_funexpr(Expr),
	    [App] = ?GRAPH:path(Expr, [exprcl, body]),
	    ?MANIP:insert_module_qualifier(Mod, App)
    end.


%%% ===========================================================================
%%%                       LIST ADDINGS AND REMOVINGS
%%% ===========================================================================


handle_list_addings(#list_add{type   = Type, 
			      file   = File, 
			      module = Module, 
			      name   = Name, 
			      arity  = Arity}) ->
    case Type of
	export ->
	    ?MANIP:create_and_add_export(File,  Name, Arity);
	import ->
	    ?MANIP:create_and_add_import(File, Module, Name, Arity)
    end.


format_list_removings(ListRemoves) ->
    Lists = lists:usort([List || #list_del{list=List} <- ListRemoves]),
    [{List, 
      [{Type, Expr} || #list_del{type=Type, item=Expr, list=L} <- ListRemoves, 
		       L == List]}
     || List <- Lists].


%% @doc Which expressions are applying the imported function
%%      in the source module?
referers(ImportExpr, FModule) ->
    lists:filter(
      fun(E) ->
	      case ?GRAPH:data(E) of
		  #expr{kind=application} ->
		      (expr_module(E) == FModule)
			  and
			    (?QUERY:has_module_qualifier(E) == false);
		  #expr{kind=fun_expr} ->
		      expr_module(E) == FModule;
		  _ ->
		      false
	      end
      end,
      ?GRAPH:path(ImportExpr, [funref, {funref, back}])).


%% @doc We remove that import/export lists, which are removable:
%%      have no other referers, just the moved applications.
filter_removable_lists(Exprs, FModule, AllApps) ->
    lists:filter(
      fun
	  ({export, _}) ->
	      true;
	  ({import, Expr}) ->
	      ImportReferers = referers(Expr, FModule),
	      case ImportReferers -- AllApps of
		  [] ->
		      true;
		  _ ->
		      false
	      end
      end,
      Exprs).


handle_list_removings(List, Exprs, FModule, FnForms) ->
    AllApps = ?QUERY:applications(FnForms),
    Removable = filter_removable_lists(Exprs, FModule, AllApps),
    Es = [E || {_, E} <- Removable],
    case ?GRAPH:path(List, [sub]) -- Es of
	[] ->
	    [Form] = ?GRAPH:path(List, [{attr, back}]),
	    ?MANIP:remove_import(Form);
	_ ->
	    lists:foreach(
	      fun
		  ({export, Expr}) ->
		      ?MANIP:remove_export(List, Expr);
		  ({import, Expr}) ->
		      ?MANIP:remove_import(List, Expr)
	      end,
	      Removable)
    end.


%%% ===========================================================================
%%%                      RECORD AND MACRO CORRECTIONS
%%% ===========================================================================


correct_rec_mac(#entity_info{form      = Form,
			     node      = Node,
			     removable = Removable,
			     locality  = InclOrLocal},
		#state{fromfile   = FFile, 
		       targetfile = TFile},
		Tag) ->
    case InclOrLocal of
	include ->
	    [IncludeFile] = ?GRAPH:path(Node, [{Tag, back}]),
	    IncludeForm = 
		?GRAPH:path(FFile, 
			    [{form,
			      {{type, '==', include}, 
			       'and', 
			       {tag, '==', (?GRAPH:data(IncludeFile))#file.path}}}]),
	    case IncludeForm of
		[F] ->
		    case Removable of
			true ->
			    ?MANIP:move_include(F, FFile, TFile),
			    [];
			false ->
			    ?MANIP:copy_include(F, TFile),
			    []
		    end;
		_ ->
		    []
	    end;
	localdefine ->
	    case Removable of
		true ->
		    ?MANIP:move_form(Form, FFile, TFile),
		    [];
		false ->
		    ?MANIP:insert_form(TFile, Form),
		    []
	    end 
    end.


%%% ===========================================================================
%%%                       RECORD AND MACRO CHECKINGS
%%% ===========================================================================


%% ----------------------------------------------------------------------------
%% @private
%% @spec check_entities(#state{}, function(), function(), atom()) -> #state{}
%% @doc This is a generic function for check the conditions of entity usage.
%% @end
%% ----------------------------------------------------------------------------
check_entities(St=#state{fromfile   = FromFile,
			 targetfile = TargetFile,
			 fnforms    = FnForms}, 
	       Existing, Used, Tag) ->

    {F_Local,F_Incl,_}     = Existing(FromFile),
    {UsedNodes, UsedNames} = Used(FnForms),
    {_,_,AllInTarget}      = Existing(TargetFile),
    Clash                  = intersect(UsedNames, AllInTarget),
    case Clash of
	[] ->
	    UsedIncluded = intersect(UsedNames, F_Incl),
	    case include_conflicts(FromFile, UsedIncluded, TargetFile, Tag) of
		[] ->
		    update_state_info(St, Tag, UsedNodes, FnForms, UsedIncluded);
		CantIncl ->
		    throw({"Can't include in target: ~p~n", [CantIncl]})
		    %% TODO: cut from hrl?
	    end;
	_ ->
	    case intersect(Clash, F_Local) of
		[] ->
		    InclClashPaths = 
			lists:usort(
			  [?QUERY:included_from_path(FromFile, C, Tag) 
			   || C <- Clash]), 
		    T_Includes = 
			lists:usort(
			  [Path 
			   || I <- ?GRAPH:path(TargetFile, [incl]), 
			      #file{path=Path} <- [?GRAPH:data(I)]]),

		    case InclClashPaths -- T_Includes of
			[] ->
			    update_state_info(St, Tag, UsedNodes, FnForms, UsedNames);
			_ ->
			    throw({"Target includes do not match" ++ 
				   "the requested includes.~nNeeded: ~p~n" ++
				   "Exists: ~p~n.", [InclClashPaths, T_Includes]})
		    end;
		_  ->
		    throw("Local defined entities conflict in target.")
	    end
    end.


%% ----------------------------------------------------------------------------
%% @private
%% @spec update_state_info(#state{}, atom(), [string()], [node()], [string()]) -> 
%%                                 #state{}
%% @doc Returns new state, that contains the calculated macro/record informations
%% (tuple of: name, removable or not, included or local defined).
%% @end
%% ----------------------------------------------------------------------------
update_state_info(St=#state{}, macro, UsedNodes, FnForms, UsedNames) ->
    St#state{macroinfo=entity_info(UsedNodes, FnForms, UsedNames, mref)};
update_state_info(St=#state{}, record, UsedNodes, FnForms, UsedNames) ->
    St#state{recordinfo=entity_info(UsedNodes, FnForms, UsedNames, recref)}.


%% ----------------------------------------------------------------------------
%% @private
%% @spec include_conflicts(node(), [string()], node(), atom()) -> 
%%               [{string(), string()}]
%% @doc Creates a list with pairs
%%      (include filenode with the included entity name).
%%      Then filters the list for not includable occurrences
%%      and modifies the include nodes to include paths.
%% @end
%% ----------------------------------------------------------------------------
include_conflicts(FromFile, UsedIncluded, TargetFile, Tag) ->
    UsedInclFiles = 
	[{?QUERY:included_from_file(FromFile, M, Tag), M} || M <- UsedIncluded],
    lists:map(
      fun({I, M}) -> {(?GRAPH:data(I))#file.path, M} end,
      lists:filter(
	fun({I, _}) -> not ?QUERY:includable(TargetFile, I) end, 
	UsedInclFiles)).


%% ----------------------------------------------------------------------------
%% @private
%% @spec entity_info([node()], [node()], [string()], atom()) -> 
%%               [#entity_info{}]
%% @doc Returns list with info about the entities.
%% @end
%% ----------------------------------------------------------------------------
entity_info(Entities, FnForms, UsedIncluded, RefLink) ->
    lists:map(
      fun(Info = #entity_info{name=Name, node=Node}) ->
	      Included = lists:member(Name, UsedIncluded),
	      if RefLink == recref ->
		      [Form] = ?GRAPH:path(Node, [{recdef, back}]),
		      if Included ->
			      Info#entity_info{form=Form, locality=include};
			 true ->
			      Info#entity_info{form=Form, locality=localdefine}
		      end;
		 true ->
		      [File] = ?GRAPH:path(Node, [{macro, back}]),
		      [Form] = ?GRAPH:path(File, [incl, {form, 
						    {{type, '==', define}, 
						     'and', 
						     {tag, '==', Name}}}]),
		      if Included ->
			      Info#entity_info{form=Form, locality=include};
			 true ->
			      Info#entity_info{form=Form, locality=localdefine}
		      end
	      end
      end,
      lists:map(
	fun(Node) ->
		{_, Name} = ?GRAPH:data(Node),
		if 
		    RefLink == recref ->
			Removable = 
			    (lists:usort(?GRAPH:path(Node, 
						     [{RefLink, back}, 
						      sup, {visib, back}, 
						      scope, {funcl, back}])) 
			     -- FnForms) == [],
			#entity_info{node=Node, name=Name, removable=Removable};
		    true ->
			Removable = 
			    (lists:usort(
			       lists:umerge(
				 ?GRAPH:path(Node, 
					     [{RefLink, back}, 
					      {clex, back}, scope, 
					      {funcl, back}]),
				 ?GRAPH:path(Node, 
					     [{RefLink, back}, 
					      {elex, back}, sup, 
					      {visib, back}, scope, 
					      {funcl, back}]))) 
			     -- FnForms) == [],
			#entity_info{node=Node, name=Name, removable=Removable}
		end
	end, Entities)).


%% ----------------------------------------------------------------------------
%% @private
%% @spec intersect(list(), list()) -> list()
%% @doc Returns the intersect of the lists
%% @end
%% ----------------------------------------------------------------------------
intersect(L1, L2) ->
    L1 -- (L1 -- L2).


expr_module(E) ->
    hd(?GRAPH:path(E, anal_context:expr_module())).


filterpairs(List, Atom) ->
    lists:usort(lists:filter(
      fun(Elem) -> 
	      case Elem of
		  {Atom, _} -> true;
		  _ -> false
	      end 
      end, 
      List)).

funs(File) ->
    Fs = ?GRAPH:path(File, [{form, {type, '==', func}}]),
    N = length(Fs),
    Forms =  [lists:nth(random:uniform(N), Fs),
	      lists:nth(random:uniform(N), Fs),
	      lists:nth(random:uniform(N), Fs),
	      lists:nth(random:uniform(N), Fs)],
    Funs = [hd(?GRAPH:path(Form, [fundef])) || Form <- Forms],
    lists:usort([{Name, Arity} || F <- Funs, #func{name=Name, arity=Arity} <- [?GRAPH:data(F)]]).


test() ->
    Files = ?GRAPH:path(?GRAPH:root(), [file]),
    Sources = lists:filter(
		fun(F0) ->
			length(?GRAPH:path(F0, [{form, {type, '==', func}}])) > 3
		end,
		lists:filter(
		  fun(F1) ->
			  string:right((?GRAPH:data(F1))#file.path, 3) == "erl"
		  end,
		  Files)),
    NumberOfFiles = length(Sources),
    io:format("~p~n", [NumberOfFiles]),
    FromFile = lists:nth(random:uniform(NumberOfFiles), Sources),
    ToFile = lists:nth(random:uniform(NumberOfFiles), Sources),
    case FromFile of
	ToFile ->
	    throw("ugyanazok");
	_ ->
	    FromName = filename:basename((?GRAPH:data(FromFile))#file.path, ".erl"),
	    ToName = filename:basename((?GRAPH:data(ToFile))#file.path, ".erl"),
	    Functions = funs(FromFile),
	    io:format("Moving ~p~nFrom: ~p~nTo:~p~n", [Functions, FromName, ToName]),
	    do(FromName, Functions, ToName)
    end,
    ok.
