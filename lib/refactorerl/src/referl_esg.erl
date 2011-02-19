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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc This module provides a generic framework for consistent semantical
%%% analysis of Erlang syntax trees. Basically it is a bridge between the
%%% semantical analyser modules and other components that need the result of
%%% the analysis.
%%%
%%% The public interface provides operations for syntax tree manipulation. The
%%% tree is stored in a {@link referl_graph. semantical graph}, and similar
%%% operations are provided here. The difference is that a set of semantical
%%% analyser modules are systematically called on the modified parts of the
%%% syntax tree, which ensures that the semantical information stored in the
%%% graph is always consistent with the syntax tree. Furthermore, garbage
%%% collection for the nodes of the tree is provided as well.
%%%
%%% == Operation batches ==
%%%
%%% Consistency is ensured by means of <em>operation batches</em>. A batch
%%% is basically a series of modifications to the syntax tree, which are
%%% executed asynchronously together with semantical analysis. A batch
%%% begins with a {@link remove/3} or {@link insert/3} call, and it must be
%%% explicitly closed with a {@link close/0} call. When a batch is active,
%%% no read operations are permitted, because graph consistency is only
%%% guaranteed after closing the batch. An exception from this is {@link
%%% data/1}, which can be called during a batch.
%%%
%%% Nodes can be created at any time regardless of the batch state. The
%%% module provides garbage collection for nodes: every node that is created
%%% during a batch but not inserted into the tree will be deleted at the
%%% end of the batch. This applies to the removed parts of the tree as
%%% well.
%%%
%%% Attribute updates are possible in two phases. When no batch is active,
%%% the semantical consistency of the graph must be maintained by the
%%% client. During an active batch, only nodes that are removed from the
%%% syntax tree can be updated; when these node are re-inserted into the
%%% tree, attached semantical links are updated.
%%%
%%% == Analyser modules ==
%%% The list of modules called for semantical analysis is taken from the
%%% application parameter `anal_mods'. This must be a list of module names,
%%% each of them is implemented using the `referl_esg' behaviour. These
%%% modules will be called in the order they are given in this list.
%%%
%%% There are two types of operations that involve semantical analysis:
%%% `insert', when a node is inserted into the syntax tree, and `remove', when
%%% a node is removed from the tree.
%%%
%%% == Callback functions ==
%%%
%%% === init/0 ===
%%%
%%% Returns a list of schema information that describes the node classes used
%%% by the module. This schema information will be added to the syntactical
%%% schema. New links can be added to existing classes as well. The type of
%%% the return value is similar to {@link referl_graph:schema()}, except that
%%% in case of existing node classes, the list of attributes must be left out
%%% entirely.
%%%
%%% === insert/5 ===
%%%
%%% `insert(Parent::node(), ParentData::data(), Tag::atom(), Child::node(),
%%% ChildData::data())' is called on nodes newly inserted into the tree in a
%%% preorder fashion. The newly inserted node is `Child', and it is inserted
%%% under `Parent' with a link tag `Tag'. It can be assumed that
%%%
%%% <ul>
%%%
%%%  <li>The syntax tree has its final form (no further insertions will be
%%%    made),</li>
%%%
%%%  <li>`insert' has already been called on `Parent', and</li>
%%%
%%%  <li>`insert' from every other module before this one has already been
%%%    called on the whole syntax tree.</li>
%%%
%%% </ul>
%%%
%%% === remove/5 ===
%%%
%%% `remove(Parent::node(), ParentData::data(), Tag::atom(), Child::node(),
%%% ChildData::data())' is called on nodes before their removal from the tree
%%% in a postorder fashion. The node to be removed is `Child', and it is
%%% inserted under `Parent' with a link tag `Tag'. It can be assumed that
%%%
%%% <ul>
%%%
%%%  <li>The syntax tree has its original form (no removals has been made
%%%    yet),</li>
%%%
%%%  <li>`remove' has already been called on every child of `Child',
%%%    and</li>
%%%
%%%  <li>`remove' from every other module after this one has already been
%%%    called on the whole syntax tree.</li>
%%%
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>


-module(referl_esg).
-vsn("$Rev: 3571 $").
-behaviour(gen_fsm).

%% Client exports
-export([create/1, insert/3, remove/3, update/2, close/0,
         root/0, data/1, links/1, path/2, index/3]).
-export([copy/1]).

%% Enviromental exports
-export([start_link/0, behaviour_info/1]).

%% gen_fsm exports
-export([init/1, no_batch/3, batch/3,
         handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).


-include("refactorerl.hrl").
-include("referl_schema.hrl").
-define(TIMEOUT, infinity).


%% @private
behaviour_info(callbacks) ->
    [{init,0},    % {update,3},
     {insert,5},  % {post_insert,5},
     {remove,5}]. %, {post_remove,5}].

%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server.
start_link() ->
    gen_fsm:start_link({local, ?ESG_SERVER}, ?MODULE, [], []).

%% @spec create(data()) -> node()
%% @doc Creates a new node. If the node is not inserted into the syntax tree,
%% it is deleted at the end of the next batch.
%% @see referl_graph:create/1
create(Data) ->
    gen_fsm:sync_send_all_state_event(?ESG_SERVER, {create, Data}, ?TIMEOUT).

%% @spec insert(node(), atom() | {atom(), integer()}, node()) -> ok
%% @doc Inserts `Child' into the syntax tree under `Parent'. If there is no
%% active operation batch, begins a new one. The actual insertion is delayed
%% until the end of the batch.
%% @see referl_graph:mklink/3
insert(Parent, Tag, Child) ->
    modif_event({insert, {Parent, Tag, Child}}).

%% @spec remove(node(), atom(), node()) -> ok
%% @doc Removes `Child', which is under `Parent', from the syntax tree.If
%% there is no active operation batch, begins a new one. The actual removal
%% is delayed until the end of the batch.
%% @see referl_graph:rmlink/3
remove(Parent, Tag, Child) ->
    modif_event({remove, {Parent, Tag, Child}}).

%% @spec update(node(), data()) -> ok
%% @doc Updates the attributes of `Node'. If there is no active operation
%% batch, semantical consistency is not guaranteed. If a batch is active,
%% the actual update is delayed until the end of the batch.
%% @see referl_graph:update/2
update(Node, Data) ->
    modif_event({update, {Node, Data}}).

%% @spec copy(node()) -> [{node(),node()}]
%% @doc Makes a copy of a subtree. `Node' is the root of the subtree. The
%% return value is a mapping from the original nodes to the copied nodes.
copy(Node) ->
    modif_event({copy, Node}).

%% @spec close() -> ok
%% @doc Closes the active operation batch. Finalizes every insertion,
%% removal and update, and recalculates the semantical part of the
%% graph. Deletes every node that is created or removed during the batch and
%% not inserted back into the tree (unreachable from the root node in the
%% syntax tree).
close() ->
    modif_event(close).

%% @doc Returns the root node.
%% @see referl_graph:root/0
root() ->
    ?Graph:root().

%% @doc Returns the attributes of a node.
%% @see referl_graph:data/1
data(Node) ->
    gen_fsm:sync_send_all_state_event(?ESG_SERVER, {data, Node}, ?TIMEOUT).

%% @doc Returns the links starting from a node.
%% @see referl_graph:links/1
links(From) ->
    query_op(fun() -> ?Graph:links(From) end).

%% @doc Evaluates a path expression.
%% @see referl_graph:path/2
path(From, Path) ->
    query_op(fun() -> ?Graph:path(From, Path) end).

%% @doc Returns the index of a link.
%% @see referl_graph:index/3
index(From, Tag, To) ->
    query_op(fun() -> ?Graph:index(From, Tag, To) end).

%% spec parent(node()) -> [{atom(), node()}]
%% doc Returns the parents of a node in the syntax tree. Note that there may
%% be more than one parents for a node.
%%parent(Node) ->
%%    gen_fsm:sync_send_all_state_event(?ESG_SERVER, {parent, Node}, ?TIMEOUT).

%% spec children(node()) -> [{atom(),node()}]
%% doc Returns the children of a node in the syntax tree. The returned list
%% contains link tags grouped together, and nodes in the same order as they
%% appear in the graph.
%%children(Node) ->
%%    gen_fsm:sync_send_all_state_event(?ESG_SERVER, {children, Node}, ?TIMEOUT).


modif_event(Event) ->
    case gen_fsm:sync_send_event(?ESG_SERVER, Event, ?TIMEOUT) of
        {error, Error} -> erlang:error(Error);
        {return, Value} -> Value;
        ok -> ok
    end.

query_op(Fun) ->
    case gen_fsm:sync_send_all_state_event(?ESG_SERVER, batch, ?TIMEOUT) of
        true -> erlang:error(batch_logic);
        _    -> Fun()
    end.

%%% ============================================================================
%%% Server callbacks

-record(state, {mods,     % List of analyser module names
                ins = [], % Links inserted during the active batch
                upd = [], % Nodes updated during the active batch
                rm  = [], % Links removed during the active batch
                cre = []  % Nodes created since the last batch close
               }).

%% Table `tree_link': stored on disc, contains the links that are part of the
%% syntax tree and are reachable from the root node.
-record(tree_link, {parent, tag, child}).

%% Table `tree_temp': stored only in ram, contains the links which can form
%% some part of the syntax tree, but are not reachable from the root node
%% (subtrees which are not yet inserted or removed, and may be attached to
%% the tree later). The contents of this table are used for garbage
%% collection, and are removed at the end of the batch.
-record(tree_temp, {parent, tag, child}).


%% @private
init([]) ->
    {ok, Mods} = application:get_env(anal_mods),
    SchemaParts = lists:flatmap(fun (Mod) -> Mod:init() end, Mods),
    EnvSchema = [   {root,  [{env,env}]},
                    {env,   record_info(fields, env),   []}
                ],
    Schema = lists:foldl(fun add_schema/2, ?SYNTAX_SCHEMA,
                         ?LEXICAL_SCHEMA ++ SchemaParts ++ EnvSchema),
    ?Graph:schema(Schema),
    case mnesia:create_table(tree_link,
                             [{attributes, record_info(fields, tree_link)},
                              {disc_copies, [node()]},
                              {index, [#tree_link.child]},
                              {type, bag}]) of
        {atomic, ok} ->
            {atomic, ok} =
                mnesia:create_table(
                  tree_temp,
                  [{attributes, record_info(fields, tree_temp)},
                   {type, bag}]),
            ok =
                mnesia:dirty_write(#tree_link{parent=root,
                                              tag=root,
                                              child=?Graph:root()}),
            ?Graph:mklink(?Graph:root(), env,
                          ?Graph:create(#env{name=appbase,
                                             value=code:lib_dir()})),
            ?Graph:mklink(?Graph:root(), env,
                          ?Graph:create(#env{name=output,
                                             value=original}));
        {aborted, {already_exists, _}} ->
            ok = mnesia:wait_for_tables([tree_link], infinity)
    end,
    {ok, no_batch, #state{mods=Mods}}.

add_schema(Part = {_Class, _Attribs, _Links}, Schema) ->
    [Part | Schema];
add_schema({Class, Links}, Schema) ->
    lists:map(
      fun({C, Att, Lnk}) when C == Class ->
              {C, Att, Links ++ Lnk};
         (Part) -> Part
      end, Schema).

%% @private
no_batch(close, _From, State) ->
    {reply, ok, no_batch, State};
no_batch({update, {N, D}}, _From, State) ->
    ?Graph:update(N, D),
    {reply, ok, no_batch, State};
no_batch({copy, Node}, _From, State) ->
    {Reply, S1} = handle_copy(Node, State),
    {reply, Reply, batch, S1};
no_batch({Command, Args}, _From, State) ->
    S1 = handle_command(Command, Args, State),
    {reply, ok, batch, S1}.

%% @private
batch(close, _From, State) ->
    {Reply, S1} = handle_close(State),
    {reply, Reply, no_batch, S1};
batch({copy, Node}, _From, State) ->
    {Reply, S1} = handle_copy(Node, State),
    {reply, Reply, batch, S1};
batch({Command, Args}, _From, State) ->
    S1 = handle_command(Command, Args, State),
    {reply, ok, batch, S1}.

handle_command(update, Args, #state{upd=U} = St) -> St#state{upd=[Args | U]};
handle_command(remove, Args, #state{rm=R}  = St) -> St#state{rm =[Args | R]};
handle_command(insert, Args = {_,_,Child}, #state{ins=I, cre=C} = St) ->
    St#state{ins = [Args | I], cre = C--[Child]}.

handle_close(#state{mods=Mods, ins=Ins, upd=Upd, rm=Rm, cre=Cre}) ->
    %% Remove
    Removed = lists:flatmap(fun do_remove/1, Rm),
    run_callbacks(lists:reverse(Mods), remove, lists:reverse(Removed)),
    lists:foreach(fun({P, T, C}) -> ?Graph:rmlink(P, T, C) end, Rm),

    %% Update
    lists:foreach(fun do_update/1, lists:reverse(Upd)),

    %% Insert
    RevIns = lists:reverse(Ins),
    lists:foreach(fun({P, T, C}) -> ?Graph:mklink(P, T, C) end, RevIns),
    Inserted = lists:flatmap(fun do_insert/1, RevIns),

    %% Collect garbage
    remove_garbage(Cre),

    %% Run insertion analysers
    run_callbacks(Mods, insert, Inserted),

    {ok, #state{mods=Mods}}.

do_remove({Parent, Tag, Child}) ->
    case mnesia:dirty_match_object(
           #tree_temp{parent=Parent, tag=Tag, child=Child}) of
        []    -> ok;
        [Tmp] ->
            ok = mnesia:dirty_delete_object(Tmp),
            ok = mnesia:dirty_write(
                   #tree_temp{parent=del, tag=del, child=Child})
    end,
    case mnesia:dirty_match_object(
           #tree_link{parent=Parent, tag=Tag, child=Child}) of
        []    -> [];
        [Lnk] ->
            ok = mnesia:dirty_delete_object(Lnk),
            case referred(Child) of
                false ->
                    ok = mnesia:dirty_write(
                           #tree_temp{parent=del, tag=del, child=Child});
                true  -> ok
            end,
            [{Parent, Tag, Child} |
             move_tree(tree_link, Child, tree_temp,
                       fun(N) -> not referred(N) end)]
    end.

do_update({Node, Data}) ->
    ?Graph:update(Node, Data).

do_insert({Parent, TagSpec, Child}) ->
    Tag = bare_tag(TagSpec),
    case mnesia:dirty_match_object(
           #tree_temp{parent=del, tag=del, child=Child}) of
        []    -> ok;
        [Del] -> ok = mnesia:dirty_delete_object(Del)
    end,
    case referred(Parent) of
        false ->
            ok = mnesia:dirty_write(
                   #tree_temp{parent=Parent, tag=Tag, child=Child}),
            [];
        true  ->
            ok = mnesia:dirty_write(
                   #tree_link{parent=Parent, tag=Tag, child=Child}),
            [{Parent, Tag, Child} |
             move_tree(tree_temp, Child, tree_link)]
    end.

bare_tag({Tag, _})              -> Tag;
bare_tag(Tag) when is_atom(Tag) -> Tag.


move_tree(From, Parent, To) ->
    move_tree(From, Parent, To, all).

move_tree(From, Parent, To, Filter) ->
    Cond =
        if
            is_function(Filter) -> Filter(Parent);
            true                -> true
        end,
    lists:flatmap(
      fun({_From, _Parent, Tag, Child}) ->
              ok = mnesia:dirty_delete_object({From, Parent, Tag, Child}),
              ok = mnesia:dirty_write({To, Parent, Tag, Child}),
              [{Parent, Tag, Child} |
               move_tree(From, Child, To)]
      end,
      if
          Cond -> mnesia:dirty_read(From, Parent);
          true -> []
      end).

referred(Node) ->
    mnesia:dirty_index_read(tree_link, Node, #tree_link.child) =/= [].


remove_garbage(Created) ->
    lists:foreach(fun ?Graph:delete/1, Created),
    remove_garbage().

remove_garbage() ->
    case mnesia:dirty_first(tree_temp) of
        '$end_of_table' ->
            ok;
        Key ->
            lists:foreach(
              fun
                  (#tree_temp{child=C}) ->
                      case referred(C) of
                          false -> ?Graph:delete(C);
                          true  -> ok
                      end
              end,
              mnesia:dirty_read(tree_temp, Key)),
            ok = mnesia:dirty_delete(tree_temp, Key),
            remove_garbage()
    end.



run_callbacks(Mods, Fun, Nodes) ->
    lists:foreach(fun(Mod) -> run_callback(Mod, Fun, Nodes) end, Mods).

run_callback(Mod, Fun, Nodes) ->
    lists:foreach(fun(Node) -> run_one_callback(Mod, Fun, Node) end, Nodes).

run_one_callback(Mod, Fun, {Parent, Tag, Child}) ->
    try
        Mod:Fun(Parent, ?Graph:data(Parent), Tag, Child, ?Graph:data(Child))
    catch
        error:Reason ->
            error_logger:error_msg("Analyser error in module ~s:~n"
                                   "  call:   ~s(~p, ~p, ~p)~n"
                                   "  reason: ~p~n"
                                   "  stack:  ~p~n",
                                   [Mod, Fun, Parent, Tag, Child, Reason,
                                    erlang:get_stacktrace()]);
        throw:Term ->
            error_logger:warning_msg("Semantic error: ~p~n", [Term])
    end.



handle_copy(Root, #state{rm=Rm, ins=Ins} = St) ->
    Tree = ets:new(copytree, []),
    try
        collect_tree(Tree, Root, Ins, Rm),
        ets:foldl(fun check_parents/2, {Tree, Root}, Tree),
        Mapping = ets:foldl(
                    fun({Node, Chld}, Lst) ->
                            N = ?Graph:create(?Graph:data(Node)),
                            ets:insert(Tree, {Node, Chld, N}),
                            [{Node, N} | Lst]
                    end,
                    [], Tree),
        NewIns = ets:foldl(
                   fun({_, Children, NewNode}, Lst) ->
                           lists:reverse(
                             [{NewNode, Tag, NewChild} ||
                                 {Tag, Child} <- Children,
                                 {_, _, NewChild} <- ets:lookup(Tree, Child)],
                             Lst)
                   end,
                   Ins, Tree),
        {{return, Mapping}, St#state{ins=NewIns}}
    catch
        Error ->
            {{error, Error}, St}
    after
        ets:delete(Tree)
    end.

collect_tree(Tree, Root, Ins, Rm) ->
    case ets:member(Tree, Root) of
        true -> ok;
        false -> collect_children(Tree, Root, Ins, Rm)
    end.

collect_children(Tree, Root, Ins, Rm) ->
    OrigChildren = [{Tag, Child} || #tree_link{tag=Tag, child=Child} <-
                                        mnesia:dirty_read(tree_link, Root)],
    Children =
        (lists:sort(link_order(Root), OrigChildren) --
         [{Tag, Child} || {Par, Tag, Child} <- Rm, Par =:= Root]) ++
        lists:reverse([{Tag, Child} || {Par, Tag,Child} <- Ins, Par =:= Root]),
    ets:insert(Tree, {Root, Children}),
    lists:foreach(
      fun ({_, Node}) -> collect_tree(Tree, Node, Ins, Rm) end,
      Children).

check_parents({Root, _}, Arg={_,Root}) ->
    Arg;
check_parents({Node, _}, Arg={Tree,_}) ->
    lists:foreach(
      fun(#tree_link{parent=P}) ->
              case ets:member(Tree, P) of
                  true -> ok;
                  false -> throw({missing_parent, Node, P})
              end
      end,
      mnesia:dirty_index_read(tree_link, Node, #tree_link.child)),
    Arg.


%% @private
handle_sync_event(batch, _From, SN, State) ->
    {reply, SN =:= batch, SN, State};
handle_sync_event({create, Data}, _From, SN, #state{cre=Cre} = St) ->
    Node = ?Graph:create(Data),
    {reply, Node, SN, St#state{cre = [Node | Cre]}};
handle_sync_event({data, Node}, _From, SN, State) ->
    {reply, handle_data(Node, State), SN, State};
handle_sync_event({parent, Node}, _From, SN, State) ->
    {reply, handle_parent(Node), SN, State};
handle_sync_event({children, Node}, _From, SN, State) ->
    {reply, handle_children(Node), SN, State}.

handle_data(Node, #state{upd=Upd}) ->
    case lists:keysearch(Node, 1, Upd) of
        {value, {_, Data}} -> Data;
        false -> ?Graph:data(Node)
    end.

handle_parent(Node) ->
    Links = mnesia:dirty_index_read(tree_link, Node, #tree_link.child),
    [{T,P} || #tree_link{tag=T,parent=P} <- Links, P =/= root, P =/= del].


handle_children(Parent) ->
    Links = mnesia:dirty_read(tree_link, Parent),
    Data = [{Tag, Chld} || #tree_link{tag=Tag, child=Chld} <- Links],
    lists:sort(link_order(Parent), Data).

link_order(Parent) ->
    fun
        ({Tag, Node1}, {Tag, Node2}) ->
            ?Graph:index(Parent, Tag, Node1) <
                ?Graph:index(Parent, Tag, Node2);
        ({Tag1, _}, {Tag2, _}) ->
            Tag1 < Tag2
    end.


%% @private
handle_event(_Event, _SN, SD) ->
    {stop, bad_request, SD}.

%% @private
handle_info(_, SN, SD) ->
    {next_state, SN, SD}.

%% @private
terminate(_, _, _) ->
    ok.

%% @private
code_change(_, SN, SD, _) ->
    {ok, SN, SD}.

