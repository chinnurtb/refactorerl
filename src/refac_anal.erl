%%% The contents of this file are subject to the Mozilla Public License
%%% Version 1.1 (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.mozilla.org/MPL/
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
%%%
%%% Contributor(s): ______________________________________.
%%%
%%% @doc This module provides a generic framework for consistent semantical
%%% analysis of Erlang syntax trees. Basically it is a bridge between the
%%% semantical analyser modules and other components that need the result of
%%% the analysis.
%%%
%%% The public interface provides operations for syntax tree manipulation. The
%%% tree is stored in a {@link refac_graph. semantical graph}, and similar
%%% operations are provided here. The difference is that a set of semantical
%%% analyser modules are systematically called on the modified parts of the
%%% syntax tree, which ensures that the semantical information stored in the
%%% graph is always consistent with the syntax tree. Furthermore, garbage
%%% collection for the nodes of the tree is provided as well.
%%%
%%% == Transactions ==
%%%
%%% Consistency is ensured by means of <em>transactions</em>. A transaction
%%% consists of four parts:
%%%
%%% <ul>
%%%
%%%  <li>First, queries can be made against the graph. Semantical links and
%%%   nodes can appear in the queries, and the result will be consistent with
%%%   the syntax tree. The only modification that is permitted at this time is
%%%   a semantics-preserving update (see below). The first non-update
%%%   modification ends this part.</li>
%%%
%%%  <li>Modifications may begin with link removals. After that, no queries
%%%   are possible, and after the first update or insert, no more remove
%%%   operations are permitted.</li>
%%%
%%%  <li>Attribute updates that modify the semantics come next, still no
%%%  queries, and the first insertion makes further updates illegal.</li>
%%%
%%%  <li>The last part consists of link insertions. After the first insertion,
%%%   no other operation is permitted.</li>
%%%
%%% </ul>
%%%
%%% When the transaction is closed, the semantical analysis of the
%%% modifications happens, and the next queries get the new syntax tree with
%%% consistent semantical information.
%%%
%%% Nodes can be created at any time regardless of the transaction state. The
%%% module provides garbage collection for nodes: every node that is created
%%% during a transaction but not inserted into the tree will be deleted at the
%%% end of the transaction. This applies to the removed parts of the tree as
%%% well.
%%%
%%% Attribute updates are possible in two phases. During queries, the
%%% semantical consistency of the graph must be maintained by the client.
%%% Between the remove and insert phase, only nodes that are removed from the
%%% syntax tree can be updated; when these node are re-inserted into the tree,
%%% attached semantical links are updated.
%%%
%%% == Analyser modules ==
%%% The list of modules called for semantical analysis is taken from the
%%% application parameter `anal_mods'. This must be a list of module names,
%%% each of them is implemented using the `refac_anal' behaviour. These
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
%%% the return value is similar to {@link refac_graph:schema()}, except that
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

-module(refac_anal).
-vsn("$Rev: 1206 $").
-behaviour(gen_fsm).

%% Client exports
-export([create/1, insert/3, remove/3, update/2, close/0,
         root/0, data/1, links/1, path/2, index/3]).

%% Enviromental exports
-export([start_link/0, behaviour_info/1]).

%% gen_fsm exports
-export([init/1, s_query/3, s_remove/3, s_update/3, s_insert/3,
         handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).


-include("refactorerl.hrl").


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
%% it is deleted at the end of the active transaction. Can be used in every
%% transaction phase.
%% @see refac_graph:create/1
create(Data) ->
    gen_fsm:sync_send_all_state_event(?ESG_SERVER, {create, Data}).

%% @spec insert(node(), atom() | {atom(), integer()}, node()) -> ok
%% @doc Inserts `Child' into the syntax tree under `Parent'. Begins the insert
%% phase of the transaction. The actual insertion may be delayed until the end
%% of the transaction.
%% @see refac_graph:mklink/3
insert(Parent, Tag, Child) ->
    gen_fsm:sync_send_event(?ESG_SERVER, {insert, Parent, Tag, Child}).

%% @spec remove(node(), atom(), node()) -> ok
%% @doc Removes `Child', which is under `Parent', from the syntax tree. Begins
%% the remove phase of the transaction (invalid after the update and insert
%% phases). The actual removal may be delayed until the end of the transaction.
%% @see refac_graph:rmlink/3
remove(Parent, Tag, Child) ->
    gen_fsm:sync_send_event(?ESG_SERVER, {remove, Parent, Tag, Child}).

%% @spec update(node(), data()) -> ok
%% @doc Updates the attributes of `Node'. After the remove phase this begins
%% the update phase of the transaction (invalid after the insert phase), in
%% this case the actual update may be delayed until the end of the
%% transaction.
%% @see refac_graph:update/2
update(Node, Data) ->
    case gen_fsm:sync_send_event(?ESG_SERVER, trans_running) of
        true -> gen_fsm:sync_send_event(?ESG_SERVER, {update, Node, Data});
        _    -> ?GRAPH:update(Node, Data)
    end.


%% @spec close() -> ok
%% @doc Closes the active transaction. Finalizes every insertion, removal and
%% update, and recalculates the semantical part of the graph. Deletes every
%% node that is created or removed during the transaction and not inserted
%% back into the tree (unreachable from the root node in the syntax tree).
close() ->
    gen_fsm:sync_send_event(?ESG_SERVER, close, infinity).

%% @doc Returns the root node. Can be used in every transaction phase.
%% @see refac_graph:root/0
root() ->
    ?GRAPH:root().

%% @doc Returns the attributes of a node.
%% @see refac_graph:data/1
data(Node) ->
    query_op(fun() -> ?GRAPH:data(Node) end).

%% @doc Returns the links starting from a node.
%% @see refac_graph:links/1
links(From) ->
    query_op(fun() -> ?GRAPH:links(From) end).

%% @doc Evaluates a path expression.
%% @see refac_graph:path/2
path(From, Path) ->
    query_op(fun() -> ?GRAPH:path(From, Path) end).

%% @doc Returns the index of a link.
%% @see refac_graph:index/3
index(From, Tag, To) ->
    query_op(fun() -> ?GRAPH:index(From, Tag, To) end).

query_op(Fun) ->
    case gen_fsm:sync_send_event(?ESG_SERVER, trans_running) of
        true -> {error, transaction_logic};
        _    -> Fun()
    end.

%%% ==================================================================
%%% Server callbacks

-record(state, {mods, anal = [], remove = []}).

-record(tree_node, {node, state=unused}).
-record(tree_link, {parent, tag, child}).
-record(removed, {parent, tag, child}).
-record(inserted, {parent, tag, child}).

%% Tables:
%%  tree_node: the nodes of the syntax tree, `state' can be `used' or `unused'
%%    depending on wether the node is accessible from the root. `unused' nodes
%%    are deleted at the end of the transaction.
%%  tree_link: links of the syntax tree
%%  inserted: subtrees that are `insert'-ed but unreachable from the syntax
%%    tree - used to ensure the proper traversal order regardless of the
%%    insertion order
%%  removed: the subtrees that are removed from the syntax tree, used to
%%    traverse removed and re-inserted subtrees
%% State:
%%  mods: list of analyser callback modules
%%  anal: modified links that are to be analysed at the end of the transaction
%%  remove: links to be removed after semantical information removal

%% @private
init([]) ->
    {ok, Mods} = application:get_env(anal_mods),
    SchemaParts = lists:flatmap(fun (Mod) -> Mod:init() end, Mods),
    Schema = lists:foldl(fun add_schema/2, ?SYNTAX_SCHEMA,
                         ?LEXICAL_SCHEMA ++ SchemaParts),
    ?GRAPH:schema(Schema),
    case mnesia:create_table(tree_node,
                             [{attributes, record_info(fields, tree_node)},
                              {disc_copies, [node()]}]) of
        {atomic, ok} ->
            mnesia:create_table(
              tree_link,
              [{attributes, record_info(fields, tree_link)},
               {disc_copies, [node()]},
               {type, bag}]),
            mnesia:create_table(
              removed,
              [{attributes, record_info(fields, removed)},
               {type, bag}]),
            mnesia:create_table(
              inserted,
              [{attributes, record_info(fields, inserted)},
               {type, bag}]),
            mnesia:dirty_write(#tree_node{node=?GRAPH:root(), state=used});
        {aborted, {already_exists, _}} ->
            mnesia:wait_for_tables([tree_node, tree_link], infinity)
    end,
    {ok, s_query, #state{mods=Mods}}.

add_schema(Part = {_Class, _Attribs, _Links}, Schema) ->
    [Part | Schema];
add_schema({Class, Links}, Schema) ->
    lists:map(
      fun({C, Att, Lnk}) when C == Class ->
              {C, Att, Links ++ Lnk};
         (Part) -> Part
      end, Schema).

%% @private
s_query(trans_running, _From, State) ->
    {reply, false, s_query, State};
s_query({insert, P, T, C}, _From, State) ->
    handle_insert(P, T, C, State);
s_query({update, N, D}, _From, State) ->
    handle_update(N, D, State);
s_query({remove, P, T, C}, _From, State) ->
    handle_remove(P, T, C, State);
s_query(close, _From, State) ->
    {reply, {error, no_transaction}, s_query, State}.

%% @private
s_remove(trans_running, _From, State) ->
    {reply, true, s_remove, State};
s_remove({remove, P, T, C}, _From, State) ->
    handle_remove(P, T, C, State);
s_remove({update, N, D}, _From, State) ->
    handle_update(N, D, close_remove(State));
s_remove({insert, P, T, C}, _From, State) ->
    handle_insert(P, T, C, close_remove(State));
s_remove(close, _From, State) ->
    handle_close(close_remove(State)).

%% @private
s_update(trans_running, _From, State) ->
    {reply, true, s_update, State};
s_update({update, N, D}, _From, State) ->
    handle_update(N, D, State);
s_update({insert, P, T, C}, _From, State) ->
    handle_insert(P, T, C, State);
s_update(close, _From, State) ->
    handle_close(State);
s_update(_, _From, State) ->
    {reply, {error, transaction_logic}, s_update, State}.

%% @private
s_insert(trans_running, _From, State) ->
    {reply, true, s_insert, State};
s_insert({insert, P, T, C}, _From, State) ->
    handle_insert(P, T, C, State);
s_insert(close, _From, State) ->
    handle_close(State);
s_insert(_, _From, State) ->
    {reply, {error, transaction_logic}, s_insert, State}.

%% @private
handle_sync_event({create, Data}, _From, SN, State) ->
    Node = ?GRAPH:create(Data),
    mnesia:dirty_write(#tree_node{node=Node, state=unused}),
    {reply, Node, SN, State};
handle_sync_event(state, _From, SN, State) ->
    {reply, {SN, State}, SN, State}.

%% "remove link" operation
handle_remove(Parent, Tag, Child, State) ->
    #state{anal=Anal, remove=Rm} = State,
    case mnesia:dirty_read(tree_node, Child) of
        [#tree_node{state=used}] ->
            mnesia:dirty_delete_object(#tree_link{parent=Parent,
                                                  tag=Tag,
                                                  child=Child}),
            A1 = [{Parent, Tag, Child} | remove_subtree(Child, [])] ++ Anal;
        _ ->
            A1 = Anal
    end,
    R1 = [ {Parent, Tag, Child} | Rm ],
    {reply, ok, s_remove, State#state{anal=A1, remove=R1}}.

%% move a subtree from table `tree' to table `removed'
remove_subtree(Parent, Trn) ->
    mnesia:dirty_write(#tree_node{node=Parent, state=unused}),
    T1 = remove_links(mnesia:dirty_read(tree_link, Parent), Trn),
    mnesia:dirty_delete(tree_link, Parent),
    T1.

remove_links([], Trn) ->
    Trn;
remove_links([#tree_link{parent=Parent, tag=Tag, child=Child} | Rest], Trn) ->
    mnesia:dirty_write(#removed{parent=Parent, tag=Tag, child=Child}),
    T1 = remove_subtree(Child, Trn),
    remove_links(Rest, [{Parent, Tag, Child} | T1]).

%% end of remove transaction phase: run remove callbacks and remove links
close_remove(State=#state{mods=Mods, anal=Trn, remove=Remove}) ->
    RevTrn = lists:reverse(Trn),
    RevMods = lists:reverse(Mods),
    run_callbacks(RevMods, remove, RevTrn),
    [?GRAPH:rmlink(P, T ,C) || {P, T, C} <- Remove],
    State#state{anal=[], remove=[]}.


%% "update" operation: performed immediately
handle_update(Node, Data, State=#state{}) ->
    ?GRAPH:update(Node, Data),
    {reply, ok, s_update, State}.


%% "insert" action: performed only for subtrees that are attached to the
%% syntax tree, unreachable subtrees are stored in the `insert' table
handle_insert(Parent, Tag, Child, State=#state{anal=Anal}) ->
    case mnesia:dirty_read(tree_node, Parent) of
        [#tree_node{state=used}] ->
            ?GRAPH:mklink(Parent, Tag, Child),
            RealTag = case Tag of {T,_} -> T; T->T end,
            mnesia:dirty_write(#tree_link{parent=Parent,
                                          tag=RealTag,
                                          child=Child}),
            A1 = insert_subtree(Child, [{Parent, RealTag, Child}]) ++ Anal;
        _ ->
            A1 = Anal,
            mnesia:dirty_write(#inserted{parent=Parent, tag=Tag, child=Child})
    end,
    {reply, ok, s_insert, State#state{anal=A1}}.

%% move a subtree into table `tree', either from table `insert' (newly
%% inserted links) or table `remove' (re-attached links)
insert_subtree(Parent, Trn) ->
    mnesia:dirty_write(#tree_node{node=Parent, state=used}),
    T1 = insert_removed(mnesia:dirty_read(removed, Parent), Trn),
    mnesia:dirty_delete(removed, Parent),
    T2 = insert_inserted(mnesia:dirty_read(inserted, Parent), T1),
    mnesia:dirty_delete(inserted, Parent),
    T2.

insert_removed([], Trn) ->
    Trn;
insert_removed([#removed{parent=Parent, tag=Tag, child=Child} | Rest], Trn) ->
    mnesia:dirty_write(#tree_link{parent=Parent, tag=Tag, child=Child}),
    T1 = insert_subtree(Child, [{Parent, Tag, Child} | Trn]),
    insert_removed(Rest, T1).

insert_inserted([], Trn) ->
    Trn;
insert_inserted([#inserted{parent=Parent, tag=Tag, child=Child} | Rest], Trn) ->
    RealTag = case Tag of {T,_} -> T; T->T end,
    mnesia:dirty_write(#tree_link{parent=Parent, tag=RealTag, child=Child}),
    ?GRAPH:mklink(Parent, Tag, Child),
    T1 = insert_subtree(Child, [{Parent, RealTag, Child} | Trn]),
    insert_inserted(Rest, T1).


%% "close transaction" operation: run insert callbacks, delete garbage nodes
handle_close(State=#state{mods=Mods, anal=RevTrn}) ->
    Trn = lists:reverse(RevTrn),
    run_callbacks(Mods, insert, Trn),
    Garb = [Node ||
               #tree_node{node=Node} <- mnesia:dirty_match_object(
                                          #tree_node{state=unused, _='_'})],
    [ ?GRAPH:delete(N) || N <- Garb],
    [ mnesia:dirty_delete(tree_node, N) || N <- Garb],
    [ mnesia:dirty_delete(removed, N) || N <- Garb],
    {reply, ok, s_query, State#state{anal=[]}}.



run_callbacks([Mod | Rest], Fun, Nodes) ->
    [ Mod:Fun(Parent, ?GRAPH:data(Parent), Tag, Child, ?GRAPH:data(Child)) ||
        {Parent, Tag, Child} <- Nodes ],
    run_callbacks(Rest, Fun, Nodes);
run_callbacks([],_,_) ->
    ok.

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
