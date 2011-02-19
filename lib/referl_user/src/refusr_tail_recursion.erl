-module(refusr_tail_recursion).
-export([is_tail_recursive/3, is_tail_recursive/1, functions_in_module_tail_recursive/1, all_function_tail_recursive/0]).

% @spec	is_tail_recursive(atom(), atom(), atom()) -> boolean
% @doc 	The function determines, wether the given function is tail-recursive.
%	The first parameter is the module atom, the second is the function atom,
%	and the third one is the arity.
%	If the function does not exist, then throws an '{error, fun_not_found, FunFeatures}' exception,
%	where 'FunFeatures' contains information about the given function.
is_tail_recursive(Module, Function, Arity) ->
  FunNode = reflib_args:function([{module, Module},{function, Function},{arity, Arity}, {ask_missing, false}]),
  is_tail_recursive(FunNode).

% @spec	is_tail_recursive(node()) -> boolean
% @doc 	The function determines, wether the given function is tail-recursive.
%	The only parameter is the function node.
%	If the function does not exist, then throws an '{error, fun_not_found, FunFeatures}' exception,
%	where 'FunFeatures' contains information about the given function.
is_tail_recursive(FunNode) ->
  FunDef = reflib_query:exec(FunNode, reflib_function:definition()),
  FunFeatures = reflib_function:mod_fun_arity(FunNode),

  AnalysedLastCommands = is_last_commands_are_tail_recursive(FunDef, reflib_form:clauses(), [], FunFeatures),

   if
     (AnalysedLastCommands == []) ->
       throw({error, fun_not_found, FunFeatures});
     true ->
       list_and(AnalysedLastCommands)
   end.

list_and(Lst) ->
  lists:foldl(fun(X, Acc) -> (X and Acc) end, true, Lst).

list_or(Lst) ->
  lists:foldl(fun(X, Acc) -> (X or Acc) end, false, Lst).

is_last_commands_are_tail_recursive(Node, Direction, VisitedApps, FunFeatures) ->
  Bodies = get_bodies(Node, Direction),

  LastCommands = lists:map(fun(X) -> lists:last(X) end, 
			      Bodies),

  lists:flatten(lists:map(fun(X) -> check_command(X, VisitedApps, FunFeatures) end, 
			    LastCommands)).

get_bodies(Node, Direction) ->
  Cls = reflib_query:exec(Node, Direction),
  lists:map(fun(X) -> reflib_query:exec(X, [body]) end, Cls).

check_command(Command, VisitedApps, FunFeatures) ->
  CommandType = reflib_expression:type(Command),
  
  case CommandType of
    application ->
      {_ModuleNode, {Module, Name, Arity}} = FunFeatures,

      case (reflib_query:exec(Command, [funeref])) of
	[] ->
	  [AppNode] = reflib_query:exec(Command, [funlref]),
	  {_AppModuleNode, {AppModule, AppName, AppArity}} = reflib_function:mod_fun_arity(AppNode),
	  if
	    ((Module == AppModule) and (Name == AppName) and (Arity == AppArity)) ->
	      true;
	    true ->
	      AppArgs = [{module, AppModule}, {function, AppName}, {arity, AppArity}],
	      AppHasBeenVisited = lists:member(AppArgs, VisitedApps),
	      if
		AppHasBeenVisited ->
		  false;
		true ->
		  AppDef = reflib_query:exec(AppNode, reflib_function:definition()),
		  is_last_commands_are_tail_recursive(AppDef, reflib_form:clauses(), lists:append([AppArgs], VisitedApps), FunFeatures)
	      end
	  end;
	_Else ->
	  false
      end;

    if_expr ->
	AnalysedLastCommands = is_last_commands_are_tail_recursive(Command, [exprcl], VisitedApps, FunFeatures),
	list_and(AnalysedLastCommands);

    case_expr ->
	AnalysedLastCommands = is_last_commands_are_tail_recursive(Command, [exprcl], VisitedApps, FunFeatures),
	list_and(AnalysedLastCommands);

    try_expr ->
      CommandChildNodes = reflib_query:exec(Command, [exprcl]),
      if
	(CommandChildNodes /= []) ->
	  TryBranch = is_last_commands_are_tail_recursive(Command, [exprcl], VisitedApps, FunFeatures);
	true ->
	  TryBranch = is_last_commands_are_tail_recursive(Command, [headcl], VisitedApps, FunFeatures)
      end,
      CatchBranch = is_last_commands_are_tail_recursive(Command, [catchcl], VisitedApps, FunFeatures),
      list_and(TryBranch ++ CatchBranch);

    receive_expr ->
      AnalysedLastCommands = is_last_commands_are_tail_recursive(Command, [exprcl], VisitedApps, FunFeatures),
      AllTrue = list_and(AnalysedLastCommands),
      if
	AllTrue ->
	  true;
	true ->
	  AnyFalse = list_or(AnalysedLastCommands),
	  if 
	    (AnyFalse == false) ->
	      false;
	    true ->
	      Bodies = get_bodies(Command, [exprcl]),
	      AnalysedBodies = lists:map(
				  fun(I) ->
				    Actual = lists:nth(I,AnalysedLastCommands),
				    if
				      Actual ->
					true;
				      true ->
					ActualBranch = lists:nth(I,Bodies),
					AnalysedBranch = lists:flatten(lists:map(fun(X) -> check_command(X, VisitedApps, FunFeatures) end, 
					  ActualBranch)),
					AllBranchCommandFalse = list_or(AnalysedBranch),
					if
					  (AllBranchCommandFalse == false) ->
					    true;
					  true ->
					    false
					end
				    end
				  end,
				  lists:seq(1,length(AnalysedLastCommands))),
	     list_and(AnalysedBodies)
	  end
      end;

    (_) ->
      false
  end.
  


% @doc 	The function gets all function in a module, and determines their tail-recursion.
%	The only parameter is the module node.
%	If there are no functions in the module, then throws an '{error, no_functions_were_found, ModNode}' exception,
%	where 'ModNode' contains information about the given module.
functions_in_module_tail_recursive(ModNode) when is_tuple(ModNode) ->
  FunNodes = reflib_query:exec(ModNode, [func]),

  if
    (FunNodes == []) ->
      throw({error, no_functions_were_found, ModNode});
    true ->
      {reflib_module:name(ModNode),
      lists:map(fun(FunNode) ->
		    {_ModuleNode, {_Module, Name, Arity}} = reflib_function:mod_fun_arity(FunNode),
		    {atom_to_list(Name) ++ "/" ++ number_to_string(Arity), is_tail_recursive(FunNode)}
		end,
		FunNodes)}
  end;

% @doc 	The function gets all function in a module, and determines their tail-recursion.
%	The only parameter is the module atom.
%	If there are no functions in the module, then throws an '{error, no_functions_were_found, ModNode}' exception,
%	where 'ModNode' contains information about the given module.
functions_in_module_tail_recursive(Module) ->
  ModNode = reflib_args:module([{module, Module}]),
  functions_in_module_tail_recursive(ModNode).

number_to_string(N) ->
  lists:flatten(io_lib:format("~p", [N])).



% @doc 	Gets all function from all module, and determines their tail-recursion.
%	If there are no modules in the database, then throws an {error, no_modules_were_found} exception.
all_function_tail_recursive() ->
  Files = reflib_query:exec([file]),
  ModNodes = reflib_query:exec(Files, [moddef]),

  if
    (ModNodes == []) ->
      throw({error, no_modules_were_found});
    true ->
      lists:map(fun(ModNode) ->
		  functions_in_module_tail_recursive(ModNode)
		end,
		ModNodes)
  end.
