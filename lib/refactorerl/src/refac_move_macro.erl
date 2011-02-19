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

%%% @doc Move macro transformation
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(refac_move_macro).
-vsn("$Rev: 1014 $").

-include("refactorerl.hrl").

-export([move_macro/3]).

-record(info, {node, name, form, files}).


%% @spec move_macro(node(), [string()], node()) -> ok
move_macro(FFile, Names, TFile) ->
    FromHeader = ?QUERY:is_header_file(FFile),
    ToHeader   = ?QUERY:is_header_file(TFile),
    BasicInfos = [scan(FFile, Name, TFile) || Name <- Names],
    Infos = [usings(I) || I <- BasicInfos],
    {_, Nodes, _, _} = all_info(Infos),

    case lists:usort(
	   lists:flatten(
	     [?QUERY:macros_by_macro(M) || M <- Nodes])) -- Nodes of
	[] ->
	    continue;
	_ ->
	    throw("Can't move macro(s), because the moved macro(s) " ++
		  "refer(s) to another macro(s). Try to move those too.")
    end,
    case lists:usort(
	   lists:flatten(
	     [?QUERY:macro_users(M) || M <- Nodes])) -- Nodes of
	[] ->
	    continue;
	_ ->
	    throw("Can't move macro(s), because the moved macro(s) " ++
		  "are used by another macro(s). Try to move those too.")
    end,
    case lists:flatten([?QUERY:records_by_macro(M) || M <- Nodes]) of
	[] ->
	    continue;
	_ ->
	    throw("Cant't move macro(s), because the moved macro(s) " ++
		  " refer(s) to record(s)")
    end,
    Do = 
	fun(I) -> 
		case transform(FFile, TFile, FromHeader, ToHeader, I) of
		    error1 -> 
			throw("Can't include the target in some files, " ++ 
			      "which included the from-file header.");
		    error2 ->
			throw("Can't move to header, because there's name " ++
			      "conflict with a macro in a file, " ++
			      "which includes the target hrl.");
		    error3 ->
			throw("Can't move macro from header, it is used not only in target.");
		    error4 ->
			throw("Can't delete macro from source file.");
		    Modified ->
			Modified
		end
	end,
    ChangedFiles = lists:flatten([Do(I) || I <- Infos]),
    ?ESG:close(),
    All = lists:usort([FFile, TFile] ++ ChangedFiles),
    [refac_fileman:save_file(F) || F <- All],
    ok.


%% @private
scan(FFile, N, TFile) ->
    if is_atom(N) ->
	    Name = atom_to_list(N);
       true ->
	    Name = N
    end,
    NameConflict = macro_exists(TFile, Name),
    if NameConflict ->
	    throw("Macro with specified name already exists in target.");
       true -> continue
    end,
    [Node] = ?GRAPH:path(FFile, [{macro, {name, '==', Name}}]),
    [Form] = ?GRAPH:path(FFile, [{form,  {{type, '==', define}, 
					 'and', 
					 {tag, '==', Name}}}]),
    #info{node = Node,
	  name = Name,
	  form = Form}.


%% @private
usings(Info = #info{node=Node}) ->
    Usings = lists:umerge(?GRAPH:path(Node, [{mref, back}, 
					     {clex, back}, scope, 
					     {funcl, back}]),
			  ?GRAPH:path(Node, [{mref, back}, 
					     {elex, back}, sup, 
					     {visib, back}, scope, 
					     {funcl, back}])),
    Files = containing_files(Usings),
    Info#info{files=Files}.


%% @private
transform(FFile, TFile, FromHeader, ToHeader, 
   #info{name=Name, form=Form, files=Files}) ->
    TargetPath = (?GRAPH:data(TFile))#file.path,
    case FromHeader of
	true ->
	    case ToHeader of
		true ->
		    Movable = movable_to_header(Name, FFile, TFile),
		    case Movable of
			true ->
			    Clash = filter_unincludable(Files, TFile),
			    case Clash of
				[] ->
				    ?MANIP:move_form(Form, FFile, TFile),
				    [?MANIP:create_include_form(F, TargetPath) 
				     || F <- Files],
				    Files;
				_ ->
				    error1
			    end;
			false ->
			    error2
		    end;
		false ->
		    Clash = Files -- [TFile],
		    if Clash == [] ->
			    ?MANIP:move_form(Form, FFile, TFile),
			    [];
		       true ->
			    error3
		    end
	    end;
	false ->
	    case ToHeader of
		true ->
		    Movable = movable_to_header(Name, FFile, TFile),
		    case Movable of
			true ->
			    ?MANIP:move_form(Form, FFile, TFile),
			    [?MANIP:create_include_form(F, TargetPath) 
			     || F <- Files],
			    Files;
			false ->
			    error2
		    end;
		false ->
		    if Files == [] ->
			    ?MANIP:move_form(Form, FFile, TFile),
			    [];
		       true ->
			    error4
		    end
	    end
    end.


%%% ===========================================================================


%% @private
all_info(Infos) ->
    lists:foldl(
      fun(#info{name=Name, node=Node, form=Form, files=Files}, {Na, No, Fo, Fi}) -> 
	      {[Name|Na], [Node|No], [Form|Fo], [Files|Fi]}
      end,
      {[], [], [], []}, 
      Infos).


%% @private
filter_unincludable(Files, TFile) ->
    lists:filter(
      fun(F) ->
	      Included = ?QUERY:is_included(F, TFile),
	      if Included ->
		      false;
		 true ->
		      not ?QUERY:includable(F, TFile)
	      end
      end,
      Files).


%% @private
containing_files(Forms) ->
    lists:usort([hd(?GRAPH:path(F, [{form, back}])) || F <- Forms]).


%% @private
macro_exists(File, Name) ->
    case ?GRAPH:path(File, [{macro, {name, '==', Name}}]) of
	[] -> false;
	_ ->  true
    end.


%% @private
includers(File) ->
    ?GRAPH:path(File, [{incl, back}]) -- [File].


%% @private
%% @doc Returns true, if there's no name conflict with macro
%% in any file, which includes TFile.
movable_to_header(Name, FFile, TFile) ->
    TargetIncluders = includers(TFile) -- [FFile],
    not lists:any(fun(F) -> macro_exists(F, Name) end,
		  TargetIncluders).
