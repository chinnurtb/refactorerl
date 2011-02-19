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

%%% @doc Semantical information query module.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(referl_seminf).
-vsn("$Rev: 1976 $").

-include("refactorerl.hrl").

-export([module/1, get_mod/1, get_fun_by_mfa/3, import_list_refs/2,
         is_exported/3, is_imported/4]).
-export([inside_bound_vars/2, var_names/1, vars/2,
         varnodes/1, varrefs/1, varbinds/1, varbinds_back/1]).
-export([fun_exists/3,fun_imported/3, varrefs_back/1, check_mod_does_not_have_fun/3,
         check_mod_does_not_import_fun/3, check_not_autoimported/2,
         expr_module/1, export_with_name_arity/3, parent_file/1, apps/2]).
-export([update_fun_data/1]).

%% @spec module(node()) -> {module, node()} | not_found
%% @throws string()
%%
%% @doc Returns the module node corresponding to a module node or a file node.
module(Node) ->
    case ?GRAPH:data(Node) of
        #file{} ->
            case ?GRAPH:path(Node, [moddef]) of
                [Mod] -> {module, Mod};
                _     -> not_found
            end;
        #module{} -> {module, Node};
        _         -> throw("Illegal starting node.")
    end.

%% @spec get_mod(atom()) -> node()
%% @throws term()
%%
%% @doc Returns the semantic node of the speficied module.
get_mod(ModName) ->
    case ?ESG:path(?ESG:root(), [file, {moddef, {name, '==', ModName}}]) of
        [ModNode] -> ModNode;
        _ -> throw("There isn't a module " ++ ?MISC:to_list(ModName))
    end.

%% @spec get_fun_by_mfa(atom() | node(), atom() | string(), natural()) -> node()
%% @throws term()
%%
%% @doc Returns the semantic node of the speficied function.
%% The module can be specified by its name or node.
get_fun_by_mfa(Mod, FunName, Arity) ->
    ModNode =
        case is_atom(Mod) of
            true -> get_mod(Mod);
            false -> Mod
        end,
    case ?ESG:path(ModNode, referl_anal_fun:function(FunName, Arity)) of
        [FunNode] ->
            FunNode;
        _ ->
            throw("The module does not contain function '"
                  ++ ?MISC:to_list(FunName) ++ "'")
    end.

%%% ============================================================================
%%% Import list references

%% @spec import_list_refs(node(), node()) -> RetVal
%%         RetVal = {1, node(), node()} | {2, node(), node(), node()} | no_refs
%%
%% @doc Function `FunNode' has references. Possible reference is the function
%%      or module import. We want to get this import references from a
%%      specified file.
%%      Example: ex.erl contains line `-import(a, [b/1])'. When we search
%%      the import references of `b/1' in `ex.erl', we get the details of this
%%      import form.
import_list_refs(File, FunNode) ->
    FormRefs = ?GRAPH:path(FunNode, [{funref, back}, sup, {{attr, back}, {type, '==', attrib}}]),
    lists:flatten(
      [import_list_ref(?GRAPH:path(Form, [attr]), FunNode)
       || Form <- FormRefs,
          #form{tag = import} <- [?GRAPH:data(Form)],
          ?GRAPH:path(Form, [{form, back}]) == [File]]).

%% @private
%% @see import_list_refs/2.
%import_list_ref(ImportForm = [_], _FunNode) ->
import_list_ref([ImportForm], _FunNode) ->
    [Module]  = ?GRAPH:path(ImportForm, [modref]),
    {1, ImportForm, Module};
import_list_ref([ModNameExpr,ImportList], FunNode) ->
    [ImpExpr] = [ Expr
                  || Expr <- ?GRAPH:path(ImportList, [sub, sub]),
                     ?GRAPH:path(Expr, [funref]) == [FunNode]],
    [Module]  = ?GRAPH:path(ModNameExpr, [modref]),
    {2, ImportList, ImpExpr, Module};
import_list_ref(_, _) ->
    no_refs.



%%% ============================================================================
%%% Export/import queries


%% @spec is_exported(node(), atom(), integer()) -> bool()
%%
%% @doc Returns true, if the Name-Arity pair is exported
%%      in the file or module, otherwise false.
is_exported(Node, Name, Arity) ->
    case module(Node) of
        not_found -> false;
        {module, Module} -> find_impexp(Module, Name, Arity, funexp)
    end.

%% @spec is_imported(node(), atom(), atom(), integer()) -> bool()
%%
%% @doc Returns true, if the Name-Arity pair is imported
%%      from module named ModuleName in the file or module 'Node',
%%      otherwise false.
is_imported(Node, ModuleName, Name, Arity) ->
    case module(Node) of
        not_found -> false;
        {module, _Module} ->
            [SourceMod] = ?GRAPH:path(?GRAPH:root(),
                                      [{module, {name, '==', ModuleName}}]),
            find_impexp(SourceMod, Name, Arity, funimp)
    end.

%% @private
find_impexp(Module, Name, Arity, ImpExpLink) ->
    case ?GRAPH:path(Module, referl_anal_fun:function(Name, Arity)) of
        [Fun] -> lists:member(Fun, ?GRAPH:path(Module, [ImpExpLink]));
        _     -> false
    end.


%% @spec export_with_name_arity(node(), atom(), integer()) -> [node()]
%%
%% @doc Returns the Name/Arity export node in the export list.
export_with_name_arity(ExportList, Name, Arity) ->
    Subs       = ?ESG:path(ExportList, [sub]),
    NameAritys =
        lists:map(
            fun(Sub) ->
                [NameNode, ArityNode] = ?GRAPH:path(Sub, [sub, elex]),
                Name2  = ((?GRAPH:data(NameNode))#lex.data)#token.value,
                Arity2 = ((?GRAPH:data(ArityNode))#lex.data)#token.value,
                {Sub, Name2, Arity2}
            end, Subs),
    [Sub || {Sub, Name2, Arity2} <- NameAritys
          , Name == Name2
          , Arity == Arity2 ].


%%% ============================================================================


%% @spec fun_exists(node(), atom() | string(), integer()) -> bool()
%%
%% @doc Returns whether there is a `Name/Arity' function in module `ModuleNode'.
fun_exists(ModuleNode, Name, Arity)->
    ?GRAPH:path(ModuleNode, referl_anal_fun:function(Name, Arity)) /= [].

%% @spec fun_imported(node(), atom() | string(), integer()) -> bool()
%%
%% @doc Returns whether there is a `Name/Arity' function imported in module
%% `ModuleNode'.
fun_imported(ModuleNode, Name, Arity)->
    Path = [{funimp, {{name, '==', Name}, 'and', {arity, '==', Arity}}}],
    ?ESG:path(ModuleNode, Path) /= [].

%% @spec check_mod_does_not_have_fun(node(), atom(), natural()) -> ok
%% @throws term()
%%
%% @doc Checks that the given module does not contain a function with the given
%% name and arity.
check_mod_does_not_have_fun(ModNode, FunName, Arity) ->
    case fun_exists(ModNode, FunName, Arity) of
        false -> ok;
        true -> throw("There already is a " ++ ?MISC:to_list(FunName) ++ "/" 
                      ++ ?MISC:to_list(Arity) ++" function in the module")
    end.

%% @spec check_mod_does_not_import_fun(node(), atom(), natural()) -> ok
%% @throws term()
%%
%% @doc Checks that the given module does not import a function with the given
%% name and arity.
check_mod_does_not_import_fun(ModNode, FunName, Arity) ->
    case fun_imported(ModNode, FunName, Arity) of
        false -> ok;
        true -> throw("There already is a " ++ ?MISC:to_list(FunName) ++ "/"
                      ++ ?MISC:to_list(Arity) ++ " in the module.")
    end.

%% @spec check_not_autoimported(atom(), natural()) -> ok
%% @throws term()
%%
%% @doc Checks that the given function is not an autoimported function.
check_not_autoimported(FunName, Arity) ->
    case erl_internal:bif(FunName, Arity) of
        false ->
            ok;
        true ->
            throw("There is an autoimported function "
                  ++ ?MISC:to_list(FunName) ++ "/" ++ ?MISC:to_list(Arity))
    end.

%% @spec expr_module(node()) -> node()
%%
%% @doc Returns the module node which contains the expression.
expr_module(E) ->
    case ?GRAPH:path(E, expr_module()) of
        [Module] -> Module;
        []       -> not_found
    end.

expr_module() ->
    expr_visib() ++ [scope, functx, modctx].
expr_visib() ->
    expr_super() ++ [{visib, back}].
expr_super() ->
    [sup].


%% @spec parent_file(node()) -> node()
%%
%% @doc Returns the file that contains the node.
parent_file(Node) ->
    case ?ESG:data(Node) of
        #file{} ->
            Node;
        _ ->
            hd(lists:flatten([parent_file(Parent) ||
                              {_, Parent} <-  ?ESG:parent(Node)]))
    end.


%%% ============================================================================
%%% Variable queries

%% @spec varrefs([node()]) -> [node()]
%%
%% @doc Returns all variables that are referenced by the nodes.
varrefs(Nodes) -> map_path(Nodes, [varref]).

%% @spec varrefs_back([node()]) -> [node()]
%%
%% @doc Returns all references to the variables.
varrefs_back(Nodes) ->
    map_path(Nodes, [{varref, back}]).

%% @spec varbinds([node()]) -> [node()]
%%
%% @doc Returns the variable object from a binding occurrance.
varbinds(Nodes) ->
    map_path(Nodes, [varbind]).

%% @spec varbinds_back([node()]) -> [node()]
%%
%% @doc Returns all binding occurrance of the variables.
varbinds_back(Nodes) ->
    map_path(Nodes, [{varbind, back}]).

%% @spec varnodes([node()]) -> [node()]
%%
%% @doc Returns all variable nodes below the expression.
varnodes(Nodes) ->
    map_path(Nodes, [{{sup,back}, {kind,'==',variable}}]).

%% @spec map_path([node()], path()) -> [node()]
%%
%% @doc Returns the nodes that are found from the nodes following the path.
map_path(Nodes, Path) ->
    lists:flatten([?ESG:path(Node,Path)||Node<-Nodes]).


%% @spec vars(sub | clause, node()) -> [node()]
%%
%% @doc Returns all variables that are below the node.
vars(Origin, Nodes) ->
    lists:flatten([var(Origin, Node) || Node <- Nodes]).

var(sub, Node) ->
    case ?ESG:data(Node) of
        #expr{kind = variable} -> Node;
        #lex{}                 -> [];
        _ ->
            Subs        = ?ESG:path(Node,[sub]),
            SubVars     = ?ESG:path(Node,[{sub, {kind,'==',variable}}]),
            SubNodes    = Subs -- SubVars,
            ClauseNodes = ?ESG:path(Node, [headcl ]) ++
                          ?ESG:path(Node, [catchcl]) ++
                          ?ESG:path(Node, [exprcl ]) ++
                          ?ESG:path(Node, [aftercl]),
            SubVars ++ vars(sub, SubNodes) ++ vars(clause, ClauseNodes)
    end;

var(clause, Node) ->
    VarVisObj = ?ESG:path(Node, [varvis]),
    varrefs_back(VarVisObj) ++ varbinds_back(VarVisObj).


%% @spec var_names([node()]) -> [string()]
%%
%% @doc Returns the names of the variables.
var_names(List) ->
    VarObjects = varrefs(List) ++ varbinds(List),
    VarData    = lists:map(fun ?ESG:data/1, VarObjects),
    VarNames   = [ Name || {_, Name} <- VarData ],
    lists:usort(VarNames).

%% @spec inside_bound_vars([node()], node()) -> [node()]
%%
%% @doc Select the variable binding occurences from a node list.
inside_bound_vars(VarNodes, VarObject) ->
    Bindings = varbinds_back(VarObject),
    ?MISC:intersect(VarNodes, Bindings).

%% @spec apps(sub | clause | function, node()) -> [node()]
%%
%% @doc Returns all application that are below the nodes.
apps(Origin, Nodes) ->
    lists:flatten([app(Origin, Node) || Node <- Nodes]).

app(sub, Node) ->
    case ?ESG:data(Node) of
        #expr{kind = application} -> List = [Node];
        _ -> List = []
    end,
    case ?ESG:data(Node) of
        #expr{kind = variable} -> [];
        #lex{}                 -> [];
        _ ->
            Subs        = ?ESG:path(Node,[sub]),
            ClauseNodes = ?ESG:path(Node, [headcl ]) ++
                          ?ESG:path(Node, [catchcl]) ++
                          ?ESG:path(Node, [exprcl ]) ++
                          ?ESG:path(Node, [aftercl]),
            apps(sub, Subs) ++ apps(clause, ClauseNodes) ++ List
    end;

app(clause, Node) ->
    SupExprs = ?ESG:path(Node, [visib]),
    apps(sub, SupExprs);

app(function, Node) ->
    Clauses = ?ESG:path(Node, [funcl]),
    apps(clause, Clauses).


%%% ============================================================================
%%% Side-effect analyzing


-define(DIRTY_BIFS,
        [apply, cancel_timer, check_process_code, delete_module, demonitor,
         disconnect_node, erase, exit, group_leader, halt, link, load_module,
         monitor_node, open_port, port_close, port_command, port_control,
         process_flag, processes, purge_module, put, register, registered,
         resume_process, self, send, send_after, send_nosuspend, spawn,
         spawn_link, spawn_opt, suspend_process, system_flag, throw, trace,
         trace_info, trace_pattern, unlink, unregister, yield]).


%% @spec update_fun_data(node()) -> ok
%% @doc Updates the semantical function object's dirty field. It will be true,
%% when in the body of the function exists message passing or it calls dirty BIF.
update_fun_data(Func) ->
    FD = ?GRAPH:data(Func),
    ?GRAPH:update(Func, FD#func{dirty = is_true_dirty(Func)}).


%% @spec is_true_dirty(node()) -> bool()
%% @doc Return true, if the function is true dirty function, so:
%% <ul><li>is dirty BIF</li>
%%     <li>contains message sender or message receiver expression</li>
%%     <li>or uses dirty built-in function</li>
%% </ul>
is_true_dirty(SemFunNode) ->
    DirtyBifs = dirty_bifs(),
    case ?GRAPH:path(SemFunNode, [{{func, back}, {name, '==', erlang}}]) of
        [_Module] ->
            #func{name=Name, arity=Arity} = ?GRAPH:data(SemFunNode),
            case lists:member({Name, Arity}, DirtyBifs) of
                true ->
                    true;
                false ->
                    is_true_dirty(SemFunNode, DirtyBifs)
            end;
        _ -> %% []
            is_true_dirty(SemFunNode, DirtyBifs)
    end.


is_true_dirty(SemFunNode, DirtyBifs) ->
    SynFunNode = ?GRAPH:path(SemFunNode, [{fundef, back}]),
    case filter_exprs(SynFunNode,
                      [send_expr, receive_expr]) of
        [] ->
            Applications = filter_exprs(SynFunNode, [application, fun_expr]),
            ReferredFuns =
                [{ModData#module.name,
                  FunData#func.name,
                  FunData#func.arity}
                 || App <- Applications,
                    Fun <- ?GRAPH:path(App, [funref]),
                    Mod <- ?GRAPH:path(Fun, [{func, back}]),
                    FunData <- [?GRAPH:data(Fun)],
                    ModData <- [?GRAPH:data(Mod)]],
            case lists:filter(
                   fun
                   ({erlang, FN, FA}) ->
                                     lists:member({FN, FA}, DirtyBifs);
                   (_) ->
                                     false
                             end,
                   ReferredFuns) of
                [] -> false;
                _ ->  true
            end;
        _ ->
            true
    end.


filter_exprs(FnForms, Kinds) ->
    lists:filter(
      fun(E) ->
              Kind = (?GRAPH:data(E))#expr.kind,
              lists:member(Kind, Kinds)
      end,
      lists:flatten(
        [?GRAPH:path(F, [funcl, {scope,back}, visib, {sup, back}])
         || F <- FnForms])).


%% @doc Creates a list of name-arity pairs from the names of dirty bifs.
%%      Example: exit becomes to [{exit, 1}, {exit, 2}]
dirty_bifs() ->
    lists:flatten([find_bifs_by_name(BifName) || BifName <- ?DIRTY_BIFS]).


%% @private
find_bifs_by_name(SearchedName) ->
    [{Name, Arity} || {Name, Arity} <- erlang:get_module_info(erlang, exports),
                      Name == SearchedName].
