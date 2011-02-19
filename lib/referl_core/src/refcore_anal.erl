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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Semantic analyser behaviour. Semantic analysers build the semantic
%%% part of the Erlang semantic graph. They are executed when a part of the
%%% syntax tree is changed, and they should modify the semantic links to
%%% reflect the change.
%%%
%%% Analysers are executed in dedicated processes. More than one process may
%%% analyse unrelated parts of the syntax tree simultaneously. One task of
%%% this module is to synchronise these concurrent analyser activities.
%%%
%%% The other task of the module is to provide a caching mechanism for syntax
%%% tree parts to speed up syntax tree traversals needed during analysis.
%%% There is a set of functions ({@link data/1}, {@link update/2}, {@link
%%% parent/1}, {@link children/1}) that can be used to access this cache,
%%% these may only be called in analyser processes.
%%%
%%% == Callback functions ==
%%%
%%% Analyser functionality must be implemented as a callback module that uses
%%% this behaviour. Such a callback module must implement the following
%%% functions:
%%%
%%% <dl>
%%%  <dt><b>schema() -> [Schema]</b></dt>
%%%  <dd>This function should return a partial graph schema. The final graph
%%%   schema is assembled from the generated syntactic schema plus the
%%%   semantic schema parts returned by analyser modules.</dd>
%%%
%%%  <dt><b>externs(Node) -> [Extern]</b></dt>
%%%  <dd>This function is called before analysis of a new subtree is started
%%%   in an analyser process. The function should return a (possibly empty)
%%%   list of graph nodes. These nodes are root points of subtrees that will
%%%   be analysed by a new process.</dd>
%%%
%%%  <dt><b>insert(Parent, Pre, Child, Post)</b></dt>
%%%  <dd>This function should analyse a newly inserted subtree. Argument
%%%   types:
%%%   <ul>
%%%    <li>Parent: {@type node()}</li>
%%%    <li>Pre: {@type [{atom(), node()@}]}</li>
%%%    <li>Child: {@type {atom(), node()@}}</li>
%%%    <li>Post: {@type [{atom(), node()@}]}</li>
%%%   </ul>
%%%   The root point of the inserted subtree is `Child' (both the link tag and
%%%   the node is passed as parameter). This node is inserted under `Parent'
%%%   which is already analysed. `Pre' and `Post' are the siblings of `Child'.
%%%  </dd>
%%%
%%%  <dt><b>remove(Parent, Pre, Child, Post)</b></dt>
%%%  <dd>This function should analyse a subtree that is going to be removed.
%%%   Argument types are the same as in case of `insert', but here the link
%%%   from `Parent' to `Child' will be deleted.</dd>
%%%
%%%  <dt><b>update(Node, Data)</b></dt>
%%%  <dd>This function should analyse a node when its attributes are changed.
%%%   `Node' is the node that is updated, `Data' is the new data for the node.
%%%  </dd>
%%% </dl>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refcore_anal).
-vsn("$Rev: 5277 $").                           % this makes emacs happy: "
-behaviour(gen_server).

-export([start/1, insert/5, remove/5, update/3, create/3,
         pull/2, push/3, cache/2, finalize/1]).
-export([data/1, parent/1, children/1, update/2]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([behaviour_info/1]).

-include("core.hrl").

-opaque gnode() :: tuple().

-define(CALL_TIMEOUT, infinity).

%%% ============================================================================
%%% Server interface functions

%% @private
start(Arg) ->
    {ok, Pid} = supervisor:start_child(?ANAL_SERVER, [Arg]),
    Pid.

%% @doc Stores a new link in the cache, pulls the subtree of `Child' from
%% `Src', and runs insertion analysers.
insert(Anal, Src, Parent, Tag, Child) ->
    cast(Anal, {insert, Src, Parent, Tag, Child}).

%% @doc Removes a link from the cache, runs removal analysers, and pushes the
%% subtree of `Child' to `Trg'.
remove(Anal, Trg, Parent, Tag, Child) ->
    cast(Anal, {remove, Trg, Parent, Tag, Child}).

%% @doc Updates node data in the cache, and runs update analysers.
update(Anal, Node, Data) ->
    cast(Anal, {update, Node, Data}).

%% @doc Updates node data in the cache for new nodes with no links.
create(Anal, Node, Data) ->
    cast(Anal, {create, Node, Data}).

%% @doc Caches node data and links (needed before update and remove).
cache(Anal, Node) ->
    call(Anal, {cache, Node}).

%% @doc Removes and returns a subtree from the cache, runs removal analysers.
pull(Anal, Top) ->
    call(Anal, {pull, Top}).

%% @doc Stores a subtree in the cache, runs insertion analysers.
push(Anal, Top, Tree) ->
    %% TODO: top node is unused
    cast(Anal, {push, Top, Tree}).

%% @doc Deletes unlinked subtrees, runs final analysers.
finalize(Anal) ->
    cast(Anal, finalize).

cast({Pid, Clear}, Req) -> gen_server:cast(Pid, {Req, Clear}).
call({Pid, Clear}, Req) -> gen_server:call(Pid, {Req, Clear}, ?CALL_TIMEOUT).


%% @private
behaviour_info(callbacks) ->
    [{schema,0}, {externs,1}, {insert,4}, {remove,4}, {update,2}].

%%% ============================================================================
%%% Callback functions

%% @private
start_link(Arg) ->
    %% TODO: naming is useful only for debugging, later it should be removed
    %% to avoid creating unnecessary atoms
    Name = case proplists:get_value(top,Arg) of
               {_,C,I} -> list_to_atom(lists:flatten(io_lib:write({C,I})));
               off_tree -> off_tree
           end,
    gen_server:start_link({local, Name},
                          ?MODULE, Arg, []).%{debug, [trace]}]).

-record(state, {top  :: gnode(),
                mods :: [atom()],
                tree :: ets:tab()}).

%% @private
init(Args) ->
    {top, Top} = proplists:lookup(top, Args),
    ?ESG:anal_report(reg, self(), Top),
    {ok, #state{top=Top,
                mods = proplists:get_value(mods, Args, []),
                tree = init_tree()}}.

%% @private
handle_cast({Req, Clear}, St) ->
    case handle_req(Req, St) of
        ok -> send_clear(Clear), {noreply, St};
        stop -> {stop, normal, St}
    end.

%% @private
handle_call({Req, Clear}, _From, St) ->
    case handle_req(Req, St) of
        {ok, Reply} -> send_clear(Clear), {reply, Reply, St};
        {stop, Reply} -> {stop, normal, Reply, St}
    end.

%% @private
handle_info(_, St) ->
    {noreply, St}.

%% @private
terminate(_, _) ->
    ok.

%% @private
code_change(_, S, _) ->
    {ok, S}.


send_clear(none) -> ok;
send_clear(Clear) -> ?ESG:anal_report(clear, self(), Clear).

handle_req({insert, Src, P, T, C}, St) -> handle_insert(Src, P, T, C, St);
handle_req({remove, Trg, P, T, C}, St) -> handle_remove(Trg, P, T, C, St);
handle_req({update, Node, Data},   St) -> handle_update(Node, Data, St);
handle_req({create, Node, Data},   St) -> handle_create(Node, Data, St);
handle_req({push, Top, Data},      St) -> handle_push(Top, Data, St);
handle_req({pull, Top},            St) -> handle_pull(Top, St);
handle_req({cache, Node},          St) -> read_entry(Node, St), {ok, ok};
handle_req(finalize,               St) -> handle_finalize(St).


%%% ============================================================================
%%% Server implementation

%%% See also the message sequence charts in refcore_esg.erl.

handle_insert(Src, Parent, Tag, Child, #state{}=St) ->
    Src == local orelse Src == deleg orelse
        pull_tree(Src, Child, St),
    %% In case of delegation, the first analyser has already inserted the link
    Src == deleg orelse
        insert_link(Parent, Tag, Child, St),
    case delegate(Child, extract, St) of
        {children, Deleg} ->
            anal_insert(Parent, children_ctx(Parent, Child, St), St),
            [?Anal:insert(Anal, deleg, P, T, C) || {Anal, {P, T, C}} <- Deleg],
            ok;
        {parent, Anal} ->
            ?Anal:insert(Anal, deleg, Parent, Tag, Child),
            ok
    end.

pull_tree(Src, Node, St) ->
    Subtree = ?Anal:pull(Src, Node),
    store_tree(Subtree, St).

%% TODO: when TOP is removed, stop
handle_remove(Trg, Parent, Tag, Child, #state{}=St) ->
    case delegate(Child, read, St) of
        {children, Deleg} ->
            Ctx = children_ctx(Parent, Child, St),
            remove_link(Parent, Tag, Child, St),
            anal_remove(Parent, Ctx, St),
            [?Anal:remove(Anal, deleg, P, T, C) ||
                {Anal, {P, T, C}} <- Deleg],
            if
                Trg == deleg -> subtree(Child, remove, St);
                Trg == local -> ok;
                true -> push_tree(Trg, Child, extract, St)
            end,
            ok;
        {parent, Anal} ->
            ?Anal:remove(Anal, deleg, Parent, Tag, Child),
            remove_link(Parent, Tag, Child, St),
            Trg == deleg orelse Trg == local orelse
                push_tree(Trg, Child, extract, St),
            ok
    end.

push_tree(Trg, Node, Op, St) ->
    %% Sending cached parent info is needed when removing is delegated, as the
    %% local cache contains the original links which are needed in the remote
    %% cache too
    Parent =
        try parent(Node, St) of
            P -> [read_entry(P, St)]
        catch
            no_parent -> []
        end,
    Subtree = subtree(Node, Op, St),
    ?Anal:push(Trg, Node, Parent ++ Subtree). %% TODO: top node is unused

handle_update(Node, Data, #state{}=St) ->
    case delegate(Node, no_children, extract, St) of
        {parent, Anal} ->
            ?Anal:update(Anal, Node, Data),
            ok;
        local ->
            update_entry(Node, Data, St),
            anal_update(Node, Data, St),
            ok
    end.

handle_create(Node, Data, #state{}=St) ->
    write_entry(make_entry(Node, Data), St),
    ok.

%% TODO: top node is unused
handle_push(_Node, Data, #state{}=St) ->
    store_tree(Data, St),
    ok.

handle_pull(Node, #state{}=St) ->
    {ok, subtree(Node, extract, St)}.

handle_finalize(St) ->
    delete_garbage(St),
    stop.

%% Delegates subtrees: starts analyser processes, removes subtrees from the
%% local cache and send them to the remote analysers. Return values:
%% * `{parent, Anal}' is returned when `Node' should not be handled at all by
%%   this analyser (this includes the case when `Node' is the top of `Anal')
%% * `{children, [Anal]}' is returned when `Node' should be handled locally,
%%   but it may have delegated children (empty list means no delegation)
%% * `local' is returned only when `Mode' is `no_children', and it has the
%%   same meaning as `{children, []}' (in this case, no children analysers
%%   will ever be started).
delegate(Node, Op, #state{}=St) ->
    delegate(Node, children, Op, St).
delegate(Node, Mode, Op, #state{top=Top}=St) ->
    Ext = case ?Graph:class(Node) of
              lex -> [];
              _   -> anal_externs(Node, St)
          end,
    case deleg_tops(Ext, [Node, Top], St) of
        [Top] when length(Ext) =:= 1 ->
            {parent, start_deleg(hd(Ext), Op, St)};
        _ when Mode == no_children ->
            local;
        [] ->
            {children, []};
        [Node] ->
            {children, [{start_deleg(E, Op, St), lnkinf(E, St)} || E <- Ext]}
    end.

start_deleg({Top, Mods}, Op, St) ->
    Rsv = {Pid, _} = ?ESG:anal_reserve(Top, [{mods, Mods}]),
    push_tree({Pid, none}, Top, Op, St),
    Rsv.

lnkinf({Node, _}, St) ->
    P = parent(Node, St),
    {Tag, _} = lists:keyfind(Node, 2, children(P, St)),
    {P, Tag, Node}.

deleg_tops(Deleg, Tops, St) ->
    lists:usort([deleg_top(D, Tops, St) || {D, _} <- Deleg]).

deleg_top(D, Tops, St) ->
    P = parent(D, St),
    case lists:member(P, Tops) of
        true -> P;
        false -> deleg_top(P, Tops, St)
    end.

%%% ============================================================================
%%% Tree handling

-record(entry, {node     :: gnode(),
                data     :: tuple(),
                parent   :: gnode() | none | undefined,
                children :: [{atom(), [gnode()]}],
                lexlink  :: atom(),
                lexes    :: [gnode()]}).

%% Create an empty syntax tree
init_tree() ->
    ets:new(tree, [{keypos, #entry.node}]).

%% Return the syntactic parent of a (syntactic) node, or throw an exception if
%% the node is disconnected.
-spec parent(gnode(), #state{}) -> gnode().
parent(Node, St) ->
    case read_entry(Node, St) of
        #entry{parent=none}      -> throw(no_parent);
        #entry{parent=undefined} -> throw(no_parent);
        #entry{parent=Parent}    -> Parent
    end.

%% Return the syntactic children of the node in proper order.
-spec children(gnode(), #state{}) -> [{atom(), gnode()}].
children(Node, St) ->
    #entry{children=Children} = read_entry(Node, St),
    [{T, C} || {T, CL} <- Children, C <- CL].

%% Return the data of a node.
-spec data(gnode(), #state{}) -> tuple().
data(Node, St) ->
    #entry{data=Data} = read_entry(Node, St),
    Data.

%% Add a syntactic or lexical link to the syntax tree.
-spec insert_link(gnode(), atom() | {atom(), integer()|last},
                  gnode(), #state{}) -> ok.
insert_link(Parent, TagSpec, Child, St) ->
    #entry{lexlink=Lex} = PE = read_entry(Parent, St),
    case TagSpec of
        {Lex, I} -> insert_lex(PE, I,    Child, St);
        Lex      -> insert_lex(PE, last, Child, St);
        {Tag, I} -> insert_child(PE, Tag, I,    Child, St);
        Tag      -> insert_child(PE, Tag, last, Child, St)
    end.

%% Add a lexical link.
insert_lex(PE=#entry{lexes=Lexes}, Ind, Child, St) ->
    write_entry(PE#entry{lexes=insert_at(Lexes, Ind, Child)}, St).

%% Add a syntactic link.
insert_child(PE=#entry{children=Children}, Link, Ind, Child, St) ->
    {Link, Lst} = lists:keyfind(Link, 1, Children),
    write_entry(
      PE#entry{children=lists:keyreplace(Link, 1, Children,
                                         {Link, insert_at(Lst, Ind, Child)})},
      St),
    CE = read_entry(Child, St),
    write_entry(CE#entry{parent=PE#entry.node}, St).

-spec insert_at([A], integer()|last, A) -> [A].
insert_at(Lst, last, El) ->
    Lst ++ [El];
insert_at(Lst, I, El) ->
    {Pre, Post} = lists:split(I-1, Lst),
    Pre ++ [El] ++ Post.

%% Delete a syntactic or lexical link from the syntax tree.
-spec remove_link(gnode(), atom(), gnode(), #state{}) -> ok.
remove_link(Parent, Tag, Child, St) ->
    #entry{lexlink=Lex} = PE = read_entry(Parent, St),
    if
        Tag =:= Lex ->
            #entry{lexes=Lexes} = PE,
            write_entry(PE#entry{lexes=Lexes -- [Child]}, St);
        true ->
            #entry{children=Children} = PE,
            {Tag, Lst} = lists:keyfind(Tag, 1, Children),
            write_entry(PE#entry{children=lists:keyreplace(
                                            Tag, 1, Children,
                                            {Tag, Lst -- [Child]})}, St),
            CE = read_entry(Child, St),
            write_entry(CE#entry{parent=none}, St)
    end.

%% Update node data.
-spec update_entry(gnode(), tuple(), #state{}) -> ok.
update_entry(Node, Data, St) ->
    E = read_entry(Node, St),
    write_entry(E#entry{data=Data}, St).

%% Remove and return a whole subtree from the syntax tree.
-spec subtree(gnode(), read|extract|remove, #state{}) -> [#entry{}] | ok.
subtree(Top, Op, #state{tree=Tree}=St) ->
    #entry{children=Children, lexes=Lexes} = Entry = read_entry(Top, St),
    Op == read orelse
        ets:delete(Tree, Top),
    Cont = [C || {_, CL} <- Children, C <- CL] ++ Lexes,
    if
        Op == remove ->
            [subtree(Node, Op, St) || Node <- Cont],
            ok;
        true ->
            [Entry |
             lists:flatmap(fun(Node) -> subtree(Node, Op, St) end, Cont)]
    end.

%% Add a whole subtree to the syntax tree.
-spec store_tree([#entry{}], #state{}) -> ok.
store_tree(Data, #state{tree=Tree}) ->
    ets:insert(Tree, Data).

%% Delete disconnected parts of the syntax tree.
-spec delete_garbage(#state{}) -> ok.
delete_garbage(#state{tree=Tree}=St) ->
    [garbage_tree(Top, St) ||
        [Top] <- ets:match(Tree, #entry{parent=none, node='$1', _='_'})],
    ok.

garbage_tree(Top, St) ->
    #entry{children=Cs, lexlink=LT, lexes=Ls} = read_entry(Top, St),
    Children =
        [{T, C} || {T, Cl} <- Cs, C <- Cl] ++
        [{LT, L} || L <- Ls],
    [case read_entry(Child, St) of
        #entry{data=#lex{type=ST}} when ST == subst; ST == incl ->
            ?NodeSync:del_ref(subst, {Tag, Top}, Child);
         _ ->
             garbage_tree(Child, St)
     end || {Tag, Child} <- Children],
    ?Graph:delete(Top).


%%% ----------------------------------------------------------------------------
%%% Low level entry handling

-spec read_entry(gnode(), #state{}) -> #entry{}.
read_entry(Node, #state{tree=Tree}) ->
    case ets:lookup(Tree, Node) of
        [Entry] -> Entry;
        [] ->
            Entry = make_entry(Node),
            ets:insert(Tree, Entry),
            Entry
    end.

-spec write_entry(#entry{}, #state{}) -> ok.
write_entry(Entry, #state{tree=Tree}) ->
    true = ets:insert(Tree, Entry),
    ok.

-spec make_entry(gnode()) -> #entry{}.
make_entry(Node) ->
    make_entry(Node, ?Graph:data(Node)).
make_entry(Node, Data) ->
    Parent =
        case Data of
            #lex{} -> undefined;
            {root} -> undefined;
            _      -> case ?Syn:parent(Node) of
                          [{_,P}] -> P;
                          _   -> none
                      end
        end,
    Children = get_children(Node, Data),
    {LexLink, Lexes} = get_lexes(Node, Data),
    #entry{node     = Node,
           data     = Data,
           parent   = Parent,
           children = Children,
           lexlink  = LexLink,
           lexes    = Lexes}.

%% `get_children' and `get_lexes' are specialised versions of ?Syn:children.
%% They both work with syntactic structures with missing elements, which is
%% essential during transformation.

%% `get_children' returns syntactic children, grouped by their link name.
%% Missing links are also present with an empty child list, which makes
%% inserting those links easier.

-spec get_children(gnode(), tuple()) -> [{atom(), [gnode()]}].
get_children(Node, {root})  -> [{file, ?Graph:path(Node, [file])}];
get_children(Node, #file{}) -> [{form, ?Graph:path(Node, [form])}];
get_children(_,    #lex{})  -> [];
get_children(_,    #form{type=T})
  when T == lex; T == macro; T == error -> [];
get_children(Node, Data) ->
    [{Link, ?Graph:path(Node, [Link])} ||
        Link <- link_slices(?ErlNodes:structure(Data))].

link_slices(Struct) ->
    ?MISC:uniq(lists:flatten([link_names(E) || E <- Struct])).

link_names({token, _})        -> [];
link_names({symbol, Link})    -> [Link];
link_names({repeat, _, Link}) -> [Link];
link_names({optional, Opt})   -> [link_names(El) || El<-Opt].


-spec get_lexes(gnode(), tuple()) -> {atom(), [gnode()]}.
get_lexes(_,    {root})  -> {'$none', []};
get_lexes(_,    #file{}) -> {'$none', []};
get_lexes(Node, #lex{})  -> {llex, ?Graph:path(Node, [llex])};
%get_lexes(Node, #form{type=T}) when T == lex; T == macro; T == error ->
%    {flex, ?Graph:path(Node, [flex])};
get_lexes(Node, Data) ->
    LexLink = ?ErlNodes:lexlink(element(1, Data)),
    {LexLink, ?Graph:path(Node, [LexLink])}.

%%% ============================================================================
%%% Analyser functions

-define(AnalState, refcore_anal_state).

with_report(ActionFun, ErrorMsg, St, Mods) ->
    try
        put(?AnalState, St),
        lists:map(ActionFun, Mods)
    catch
        Class:Reason ->
            error_logger:error_report(
              [{text, ErrorMsg},
               {class, Class},
               {reason, Reason},
               {stack, erlang:get_stacktrace()}]),
            []
    after
        erase(?AnalState)
    end.


anal_externs(Node, #state{mods=Mods}=St) ->
    lists:flatten(
        with_report(fun(Mod) -> Mod:externs(Node) end,
                    "Delegation analyser error", St, Mods)).

anal_insert(_, no_ctx, _) -> ok;
anal_insert(Parent, {Pre, This, Post}, #state{mods=Mods}=St) ->
    with_report(fun(Mod) -> Mod:insert(Parent, Pre, This, Post) end,
                "Insert analyser error", St, Mods).


anal_remove(_, no_ctx, _) -> ok;
anal_remove(Parent, {Pre, This, Post}, #state{mods=Mods}=St) ->
    with_report(fun(Mod) -> Mod:remove(Parent, Pre, This, Post) end,
                "Remove analyser error", St, lists:reverse(Mods)).


anal_update(_, #lex{}, _) -> ok;
anal_update(Node, Data, #state{mods=Mods}=St) ->
    with_report(fun(Mod) -> Mod:update(Node, Data) end,
                "Update analyser error", St, Mods).


children_ctx(Parent, Child, St) ->
    Children = children(Parent, St),
    case lists:splitwith(fun({_,C}) -> C =/= Child end, Children) of
        {Pre, [This | Post]} -> {Pre, This, Post};
        {_, []} -> no_ctx
    end.


%%% ============================================================================
%%% Analyser interface functions

%% @spec data(node()) -> tuple()
%% @doc A cached variant of {@link refcore_graph:data/1}.
data(Node) -> data(Node, get(?AnalState)).

%% @spec parent(node()) -> node()
%% @doc A cached variant of {@link refcore_syntax:parent/1}. Returns only
%% syntactic parents which are always unique, throws an exception when no
%% parent is found.
parent(Node) -> parent(Node, get(?AnalState)).

%% @spec children(node()) -> [{atom(), node()}]
%% @doc A cached variant of {@link refcore_syntax:children/1}. Returns only
%% syntactic children (i.e. lexical children are missing from the result).
children(Node) -> children(Node, get(?AnalState)).

%% @spec update(node(), tuple()) -> ok
%% @doc A cached variant of {@link refcore_graph:update/2}.
update(Node, Data) ->
    ?Graph:update(Node, Data),
    update_entry(Node, Data, get(?AnalState)).
