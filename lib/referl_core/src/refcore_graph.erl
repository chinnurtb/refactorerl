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

%%% @doc Graph storage server. This is a mnesia based implementation of
%%% the semantical graph server interface.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refcore_graph).
-vsn("$Rev: 4979 $").
-behaviour(gen_server).

%%% ============================================================================
%%% Exports

%% Client exports
-export([start_link/0, schema/1, reset_schema/0, erase_nodes/0,
         root/0, create/1, update/2, delete/1, data/1, class/1,
         mklink/3, rmlink/3, links/1, path/2, index/3,
         set_prop/3, get_prop/2, get_props/1, del_prop/2]).

%% Compatibility interface to refcore_db
-export([backup/0, restore/1, undo/0, redo/0, clean/0]).
-export([is_gnode/1]).

%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% ============================================================================
%%% Client functions

-include("core.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(NODETAG, '$gn').
-define(TIMEOUT, 60000).

-define(IS_NODE(Node), element(1, Node) =:= ?NODETAG).

-define(Call(Req, Args),
        begin
            Request = list_to_tuple([Req | Args]),
            case gen_server:call(?GRAPH_SERVER, Request, ?TIMEOUT) of
                ok             -> ok;
                {ok, Reply}    -> Reply;
                {error, Error} -> erlang:error(Error, Args)
            end
        end).


%% @type node().
%%  Represents a node in the graph.

%% @type data() = tuple().
%%  Represents the class and attributes of a node. This is essentially a
%%  record, the name of the record (or the first element of the tuple) is
%%  the class name, and the fields are the attributes.

%% @type path() = [PathElem]
%%       PathElem = Tag | {Tag, Index} | {Tag, Filter} | {intersect, Node, Tag}
%%       Tag = atom() | {atom(), back}
%%       Index = integer() | last | {integer(), last} | {integer(), integer()}
%%       Filter = {Filter, and, Filter} | {Filter, or, Filter}
%%              | {not, Filter} | {Attrib, Op, term()}
%%       Attrib = atom()
%%       Op = '==' | '/=' | '=<' | '>=' | '<' | '>'.
%% Indexes start at 1. `{Start, End}' interval means indexes `Ind' that satisfy
%% `Start =< Ind < End'.

%% @type schema() = [ClassDef]
%%       ClassDef = {ClassName::atom(), [Attrib::atom()], [Link]}
%%       Link = {Tag::atom(), ClassName::atom()}.
%%  Describes the schema of the graph.

%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server. There will be no initial schema information,
%% {@link schema/1} must be called before the server can actually be used.
start_link() ->
    gen_server:start_link({local, ?GRAPH_SERVER}, ?MODULE, [], []).

%% @spec schema(schema()) -> init | match | mismatch
%% @doc Initialises the server with a given schema. Checks whether the
%% persistent storage has the same schema as given here, and initialises it
%% if neccessary.
schema(Schema) ->
    gen_server:call(?GRAPH_SERVER, {schema, Schema}).

%% @spec reset_schema() -> ok
%% @doc This function can be used to erase the current graph schema when a new
%% schema has to be used. Erases all data from the database.
%% @see erase_graph/0
reset_schema() ->
    gen_server:cast(?GRAPH_SERVER, {reset_schema}).

%% @spec erase_nodes() -> ok
%% @doc This function erases the contents of the database while retaining the
%% schema. This is a synchronous operation (opposed to {@link reset_schema/0}).
erase_nodes() ->
    ?Call(erase_nodes, []).

%% @spec root() -> node()
%% @doc Returns the root node.
root() ->
    ?Call(root, []).

%% @spec create(data()) -> node()
%% @doc Creates a new node. The class and attributes of the node are given by
%% Data.
create(Data) ->
    ?Call(create, [Data]).

%% @spec update(node(), data()) -> ok
%% @doc Updates the attributes of a node. The new attributes are given by
%% Data, which must have the same class as Node.
update(Node, Data) when ?IS_NODE(Node) ->
    ?Call(update, [Node, Data]).

%% @spec delete(node()) -> ok
%% @doc Deletes the given node.
delete(Node) when ?IS_NODE(Node) ->
    ?Call(delete, [Node]).

%% @spec data(node()) -> data()
%% @doc Returns the data associated with a node.
data(Node) when ?IS_NODE(Node) ->
    ?Call(data, [Node]).

%% @spec class(node()) -> atom()
%% @doc Returns the node class of the node. This is equivalent to
%% `element(1, data(Node))' (but may be faster).
class({?NODETAG, Class, _Id}) ->
    Class.

%% @spec mklink(node(), atom() | {atom(), integer()}, node()) -> ok
%% @doc Creates a link between two nodes.
mklink(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To),
                           is_atom(Tag); is_tuple(Tag) ->
    ?Call(mklink, [From, Tag, To]).


%% @spec rmlink(node(), atom(), node()) -> ok
%% @doc Removes a link between two nodes.
rmlink(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To), is_atom(Tag) ->
    ?Call(rmlink, [From, Tag, To]).

%% @spec links(node()) -> [{atom(), node()}]
%% @doc Returns the links starting from a node.
links(Node) when ?IS_NODE(Node) ->
    ?Call(links, [Node]).

%% @spec path(node(), path()) -> [node()]
%% @doc Evaluates a path expression starting from Node, and returns the
%% resulting nodes.
path(Node, []) -> [Node];
path(Node, Path) when ?IS_NODE(Node), is_list(Path) ->
    ?Call(path, [Node, Path]).

%% @spec index(node(), atom(), node()) -> integer() | none
%% @doc Returns the index of a link. If there is a link between `From' and `To'
%% with a tag `Tag', then the result is the index of this link among the links
%% with tag `Tag'. Otherwise, the result is `none'.
index(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To), is_atom(Tag) ->
    ?Call(index, [From, Tag, To]).

%% @spec set_prop(node(), atom(), term()) -> ok
%% @doc Set a node property.
set_prop(Node, Key, Value) when ?IS_NODE(Node), is_atom(Key) ->
    ?Call(setp, [Node, Key, Value]).

%% @spec get_prop(node(), atom()) -> {ok, term()} | undefined
%% @doc Get the value of a node property.
get_prop(Node, Key) when ?IS_NODE(Node), is_atom(Key) ->
    ?Call(getp, [Node, Key]).

%% @spec get_props(node()) -> [{atom(), term()}]
%% @doc Get all properties of a node.
get_props(Node) when ?IS_NODE(Node) ->
    ?Call(getp, [Node]).

%% @spec del_prop(node(), atom()) -> ok
%% @doc Remove a property of a node.
del_prop(Node, Key) when ?IS_NODE(Node), is_atom(Key) ->
    ?Call(delp, [Node, Key]).


%% @spec backup() -> {ok,backupfile} | error
%% @doc Creates a new checkpoint from mnesia tables and returns the
%% name of the checkpoint.
backup() ->
    ?Db:backup().

%% @spec restore(integer()) -> restore | error
%% @doc Restores the previous state of the database from the checkpoit
%% file created by backup/0
restore(Count) ->
    ?Db:restore(Count).

%% @spec undo() -> undo_is_ok | invalid_checkpoint_number
%% @doc Restores the previous state of the database from the last
%% checkpoint. The actual checkpoint numbers are
%% stored in the database.
undo() ->
    ?Db:undo().

%% @spec redo() -> restored | invalid_checkpoint_number
%% @doc Restores the next state of the database from the last
%% checkpoint. The actual checkpoint numbers are
%% stored in the database.
redo() ->
    ?Db:redo().

%% @spec clean() -> cleaned_all_backups
%% @doc Cleans all backup files from the mnesia storage
clean()->
    ?Db:clean().

%% @spec is_gnode(term()) -> boolean()
%% @doc  Returns whether the argument is the representation of a graph node.
is_gnode({'$gn', _, _}) -> true;
is_gnode(_)             -> false.

%%% ============================================================================
%%% Implementation
%%%
%%% For each node class, there are two mnesia tables:
%%%  - one with the name of the class that stores the attributes
%%%  - one with '$lnk' appended to the class name that stores link information
%%%
%%% Every node has an integer ID, which is unique in its class. A node
%%% is represented by a {?NODETAG, Class, Id} tuple.
%%%
%%% The attribute table contains tuples of the form {Id, Data}, the key
%%% is the first element.
%%%
%%% The link table contains tuples of the form
%%%   {Class, {From, Tag}, Index, To},
%%% where Class is the name of the table, From and To are ID-s, Tag is the
%%% link tag and Index is the index of the link among the links from the
%%% same node and with the same tag. The (non-unique) key is the {From, Tag}
%%% element, and there is a mnesia index on the To element for backward
%%% links.

%%% ----------------------------------------------------------------------------
%%% Callback functions

-define(Exec(Body), ?Db:exec(fun() -> Body end)).

%% @private
init(_) ->
    process_flag(trap_exit, true),
    case ?Db:init() of
        empty ->
            init_graph(),
            %% no_schema means we should create more tables to store the graph
            %% itself
            {ok, no_schema};
        exists ->
            case ?Db:exists(class) and ?Db:exists(target) and
                 ?Db:exists(nextid) of
                true ->
                    %% has_schema means that the existing schema should be
                    %% checked against the needed schema
                    {ok, has_schema};
                false ->
                    init_graph(),
                    {ok, no_schema}
            end
    end.

%% @private
handle_cast({reset_schema}, S)   -> handle_reset_schema(S).


%% @private
handle_call({schema, Schema}, _F, S) ->
    handle_schema(Schema, S);
handle_call(_Req, _F, no_schema) ->
    {stop, no_schema, no_schema, no_schema};

handle_call(_Req, _F, schema_error) ->
    {stop, schema_error, schema_error, schema_error};

handle_call({root},                  _F, S) -> handle_root(S);
handle_call({create, Data},          _F, S) -> handle_create(Data, S);
handle_call({update, Node, Data},    _F, S) -> handle_update(Data, Node, S);
handle_call({delete, Node},          _F, S) -> handle_delete(Node, S);
handle_call({data,   Node},          _F, S) -> handle_data(Node, S);
handle_call({mklink, From, Tag, To}, _F, S) -> handle_mklink(From, Tag, To, S);
handle_call({rmlink, From, Tag, To}, _F, S) -> handle_rmlink(From, Tag, To, S);
handle_call({links,  Node},          _F, S) -> handle_links(Node, S);
handle_call({path,   Node, Path},    _F, S) -> handle_path(Node, Path, S);
handle_call({index,  From, Tag, To}, _F, S) -> handle_index(From, Tag, To, S);
handle_call({setp,   Node,Key,Val},  _F, S) -> handle_setp(Node, Key, Val, S);
handle_call({getp,   Node, Key},     _F, S) -> handle_getp(Node, Key, S);
handle_call({getp,   Node},          _F, S) -> handle_getp(Node, S);
handle_call({delp,   Node, Key},     _F, S) -> handle_delp(Node, Key, S);
handle_call({erase_nodes},           _F, S) -> handle_erase(S).

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    %%delete_end_backups({1,up}),
    ok.

%% @private
code_change(_Old, State, _Extra) ->
    {ok, State}.


%%% ----------------------------------------------------------------------------
%%% Data types

%% Class table: stores the attribute names for every node class
-record(class, {name, attribs}).
%% Target table: stores the link target class name for every starting class,
%% link tag pair
-record(target, {start, next}).
%% Nextid table: stores the next available ID for every node class
-record(nextid, {class, id}).

%%% ----------------------------------------------------------------------------
%%% Schema handling

init_graph() ->
    %% In a new database, some tables are always created: node class table
    ?Db:create(class, record_info(fields, class), []),
    %% node id table
    ?Db:create(nextid, record_info(fields, nextid), []),
    %% link target class table
    ?Db:create(target, record_info(fields, target), []).

handle_reset_schema(_) ->
    error_logger:info_msg("Resetting database schema, restart follows\n"),
    save_envs(),
    ?Db:delete_all(),
    {stop, normal, no_schema}.

%% Saves the environment configuration to `EnvConfFile'.
save_envs() ->
    EnvConfFile = "refactorerl.emacs.configuration",
    {reply, {ok, Root}, off} = handle_root(off),
    {reply, {ok, Envs}, off} = handle_path(Root, [env], off),
    EnvDatas = [Data || Env <- Envs,
                        {reply, {ok, Data}, off} <- [handle_data(Env, off)]],
    {ok, Dev} = file:open(EnvConfFile, [write]),
    io:format(Dev, "~p.~n", [EnvDatas]),
    file:close(Dev).

handle_schema(Schema, no_schema) ->
    init_schema(Schema),
    ?Exec(mnesia:write(#class{name='$hash', attribs=erlang:phash2(Schema)})),
    {reply, init, has_schema};

handle_schema(Schema, has_schema) ->
    case ?Exec(mnesia:read(class, '$hash', read)) of
        [#class{attribs=OldHash}] -> ok;
        _                         -> OldHash = unknown
    end,
    NewHash = erlang:phash2(Schema),
    if
        OldHash =:= NewHash ->
            {reply, match, has_schema};
        true ->
            error_logger:error_report(
              [{module, ?MODULE},
               {message,
                "Required and stored graph schemas are different."}]),
            {reply, mismatch, schema_error}
    end.

init_schema([]) ->
    ok;
init_schema([{Class, Attribs, Links} | Tail]) ->
    if
        Class =/= root ->
            ?Db:create(Class, [id, attribs, props], []);
        true -> ok
    end,

    ?Db:create(linktab(Class), [id, ind, to], [{index, [to]}, {type,bag}]),

    ?Exec(
       begin
           mnesia:write(#class{name=Class, attribs=Attribs}),
           [ mnesia:write(#target{start={Class, Tag}, next=To}) ||
               {Tag, To} <- Links]
       end),

    init_schema(Tail).

handle_erase(S) ->
    Q = qlc:q([{Name, linktab(Name)} ||
                  #class{name=Name} <- mnesia:table(class),
                  Name =/= root, Name =/= '$hash']),
    ?Exec(lists:foreach(fun erase_class/1, qlc:e(Q))),
    {reply, ok, S}.

erase_class({Class, Lnk}) ->
    erase_tab(Class),
    erase_tab(Lnk),
    mnesia:delete({nextid, Class}).

erase_tab(Tab) ->
    lists:foreach(fun mnesia:delete/1,
                  [{Tab, Key} || Key <- mnesia:all_keys(Tab)]).

%%% ----------------------------------------------------------------------------
%%% Node operations

handle_root(S) ->
    {reply, {ok, {?NODETAG, root, 0}}, S}.

handle_data({?NODETAG, root, _}, S) ->
    {reply, {ok, {root}}, S};
handle_data({?NODETAG, Class, Id}, S) ->
    case ?Exec(mnesia:read(Class, Id, read)) of
        [{_,_,Data,_}] -> {reply, {ok, Data}, S};
        []             -> {reply, {error, bad_node}, S}
    end.

handle_create(Data, S) ->
    Class = element(1, Data),
    Id = ?Exec(create_node(Class, Data)),
    {reply, {ok, {?NODETAG, Class, Id}}, S}.

create_node(Class, Data) ->
    Id = mnesia:dirty_update_counter(nextid, Class, 1),
    mnesia:write({Class, Id, Data, []}),
    Id.

handle_update(Data, {?NODETAG, Class, Id}, S) ->
    if
        element(1, Data)=:=Class ->
            ?Exec(
               begin
                   [{Class, Id, _, Props}] = mnesia:read({Class, Id}),
                   mnesia:write({Class, Id, Data, Props})
               end),
            {reply, ok, S};
        true ->
            {reply, {error, bad_class}, S}
    end.

handle_delete({?NODETAG, Class, Id}, S) ->
    ?Exec(
       begin
           mnesia:delete({Class, Id}),
           delete_forward_links(Class, Id),
           delete_backward_links(Class, Id)
       end),
    {reply, ok, S}.

delete_forward_links(Class, Id) ->
    Tags = [Tag || {Tag, _} <- link_targets(Class, fwd)],
    [ok = mnesia:delete({linktab(Class), {Id, Tag}}) ||
        Tag <- Tags].

delete_backward_links(Class, Id) ->
    Links =
        lists:foldl(
          fun
              ({FTag, FCl}, Lnk) ->
                  orddict:append(FCl, FTag, Lnk)
          end,
          [],
          link_targets(Class, back)),

    Reorders =
        [begin
            mnesia:delete_object(Rec),
            {linktab(FCl), From}
         end ||
            {FCl, Tags}             <- Links,
            Rec = {_,From = {_,FTag}, _,_} <-
                mnesia:index_match_object({linktab(FCl), '_', '_', Id}, 4),
            lists:member(FTag, Tags)],
    [begin
        Nodes = mnesia:match_object({CLnk, From, '_', '_'}),
        [ begin
            mnesia:delete_object(Obj),
            mnesia:write({CLnk, {FId, Tag}, Idx2, To})
          end
            ||  {Obj = {_CLnk, {FId, Tag}, Idx, To}, Idx2}  <- ?MISC:index_list(Nodes),
                Idx =/= Idx2]
     end ||
        {CLnk, From} <- lists:usort(Reorders)].


%%% ----------------------------------------------------------------------------
%%% Link queries

%% Note: the links need reordering, as Mnesia does not keep the order.
%% Since the links are unique, lists:usort is applicable.
handle_links({?NODETAG, Class, Id}, S) ->
    T = [{{Id, Tag}, TCl} || {Tag, TCl} <- link_targets(Class, fwd)],
    Q = qlc:q([ {Tag, {?NODETAG, TCl, To}} ||
                  {ST={_,Tag}, TCl} <- T,
                  {_, FL, _Ind, To} <- mnesia:table(linktab(Class)),
                  ST =:= FL]),
    Run = ?Exec(qlc:e(Q)),
    Reordered = lists:usort(Run),
    {reply, {ok, Reordered}, S}.

handle_index({?NODETAG, FCl, FId}, Tag, {?NODETAG, TCl, TId}, S) ->
    case link_target(FCl, Tag, fwd) of
        {class, TCl} ->
            case link_index(FCl, FId, Tag, TId) of
                []    -> {reply, {ok, none}, S};
                [Ind] -> {reply, {ok, Ind}, S};
                Multi when is_list(Multi) -> {reply, {ok, Multi}, S}
            end;
        _ ->
            {reply, {error, bad_link}, S}
    end.

handle_path({?NODETAG, Class, Id}, Path, S) ->
    try compile_path(Path, Class, [Id]) of
        Query ->
            {reply, {ok, ?Exec(qlc:e(Query))}, S}
    catch
        throw:Msg ->
            {reply, {error, Msg}, S}
    end.

%% Returns existing link indexes (normally only zero or one)
link_index(FCl, FId, Tag, TId) ->
    Q = qlc:q([Ind || {_, Key, Ind, To} <- mnesia:table(linktab(FCl)),
                      Key =:= {FId, Tag},
                      To =:= TId]),
    ?Exec(qlc:e(Q)).


%%% ----------------------------------------------------------------------------
%%% Link modifications

handle_mklink({?NODETAG, FCl, FId}, TagInfo, {?NODETAG, TCl, TId}, S) ->
    {Tag, Ind} =
        if
            is_atom(TagInfo) -> {TagInfo, last};
            true             -> TagInfo
        end,
    case link_target(FCl, Tag, fwd) of
        {class, TCl} ->
            ?Exec(
               case node_exists(FCl, FId) andalso
                   node_exists(TCl, TId) of
                   true ->
                       do_mklink(FCl, FId, Tag, Ind, TId),
                       {reply, ok, S};
                   false ->
                       {reply, {error, bad_node}, S}
               end);
        _ ->
            {reply, {error, bad_link}, S}
       end.

do_mklink(FCl, FId, Tag, Ind, TId) ->
    Absent = link_index(FCl, FId, Tag, TId) =:= [],
    if
        not Absent -> ok;
        Ind =:= last ->
            append_link(linktab(FCl), FId, TId, Tag);
        true ->
            insert_link(linktab(FCl), FId, TId, Tag, Ind)
    end.

node_exists(root, 0)   -> true;
node_exists(Class, Id) -> mnesia:read(Class, Id) =/= [].

%% Only creates the new link
append_link(Links, From, To, Tag) ->
    Ind = length(mnesia:read(Links, {From, Tag}, read)) + 1,
    mnesia:write({Links, {From, Tag}, Ind, To}).

%% Replaces the set of links completely with the new set
insert_link(Links, From, To, Tag, Ind) ->
    LinkList = mnesia:read(Links, {From,Tag}, read),
    NewLinks =
        [ Lnk || Lnk = {_, _, I, _} <- LinkList, I < Ind ]++
        [ {Links, {From, Tag}, Ind, To} ] ++
        [ {L, F, I+1, T} || {L, F, I, T} <- LinkList, I >= Ind ],
    mnesia_replace_links(From, Tag, Links, NewLinks).


handle_rmlink({?NODETAG, FCl, FId}, Tag, {?NODETAG, TCl, TId}, S) ->
    case link_target(FCl, Tag, fwd) of
        {class, TCl} ->
            {reply, ?Exec(remove_link(linktab(FCl), FId, TId, Tag)), S};
        _ ->
            {reply, {error, bad_link}, S}
    end.

%% Replaces the set of links completely
remove_link(OldLinks, From, To, Tag) ->
    LinkList = mnesia:read(OldLinks, {From, Tag}),
    case  [I || {_, _, I, T} <- LinkList, T == To] of
        [Ind] ->
            NewLinks =
                [ Lnk || Lnk = {_, _, I, _} <- LinkList, I < Ind ] ++
                [ {L, F, I-1, T} ||
                    {L, F, I, T} <- LinkList, I > Ind ],
            mnesia_replace_links(From, Tag, OldLinks, NewLinks),
            ok;
        [] ->
            {error, not_exists};
        _ ->
            throw({multiple_links, Tag})
    end.

%% Removes the old links from Mnesia and inserts the new ones instead.
mnesia_replace_links(From, Tag, OldLinks, NewLinks) ->
    mnesia:delete({OldLinks, {From,Tag}}),
    [mnesia:write(Lnk) || Lnk <- NewLinks].



%%% ----------------------------------------------------------------------------
%%% Property functions

handle_setp({?NODETAG, Class, Id}, Key, Value, S) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, Data, Props}] = mnesia:read({Class, Id}),
               NewProps = [{Key, Value} | proplists:delete(Key, Props)],
               mnesia:write({Class, Id, Data, NewProps}),
               {reply, ok, S};
           false ->
               {reply, {error, bad_node}, S}
       end).

handle_getp({?NODETAG, Class, Id}, Key, S) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, _Data, Props}] = mnesia:read({Class, Id}),
               Value =
                   case proplists:lookup(Key, Props) of
                       none -> undefined;
                       {Key, Val} -> {ok, Val}
                   end,
               {reply, {ok, Value}, S};
           false ->
               {reply, {error, bad_node}, S}
       end).

handle_getp({?NODETAG, Class, Id}, S) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, _Data, Props}] = mnesia:read({Class, Id}),
               {reply, {ok, lists:usort(Props)}, S};
           false ->
               {reply, {error, bad_node}, S}
       end).

handle_delp({?NODETAG, Class, Id}, Key, S) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, Data, Props}] = mnesia:read({Class, Id}),
               NewProps = proplists:delete(Key, Props),
               mnesia:write({Class, Id, Data, NewProps}),
               {reply, ok, S};
           false ->
               {reply, {error, bad_node}, S}
       end).

%%% ----------------------------------------------------------------------------
%%% Path compiler

compile_path([], Class, Query) ->
    qlc:q([{?NODETAG, Class, Id} || Id <- Query]);

compile_path([S={intersect, {?NODETAG, SCl, SId}, Step} | Rest],
             Class, Query) ->
    {Tag, Dir} =
        if
            is_atom(Step) -> {Step, fwd};
            true          -> Step
        end,

    case link_target(SCl, Tag, Dir) of
        {class, Class} -> ok;
        {class, _}     -> throw({bad_class, S, Class});
        error          -> throw({bad_link, S, Class})
    end,

    Result =
        case Dir of
            fwd ->
                R =
                    qlc:q([{Ind, Id}
                            || {_, From, Ind, To} <- mnesia:table(linktab(SCl)),
                               From =:= {SId, Tag},
                               Id <- Query,
                               Id =:= To]),
                run_and_reindex(R);
            back ->
                qlc:q(
                  [Id || {_, From, _,To} <- mnesia:table(linktab(Class)),
                         SId =:= To,
                         Id <- Query,
                         From =:= {Id, Tag}])
        end,
    compile_path(Rest, Class, Result);

compile_path([Elem | Rest], Class, Query) ->
    {Dir, Filter} =
        case Elem of
            {{Tag, back}, Filt} when is_atom(Tag) -> {back, Filt};
            {Tag,  back}        when is_atom(Tag) -> {back, {}};
            {Tag,         Filt} when is_atom(Tag) -> {fwd,  Filt};
            Tag                 when is_atom(Tag) -> {fwd,  {}};
            Tag                                   -> throw({bad_path, Elem})
        end,

    NextClass =
        case link_target(Class, Tag, Dir) of
            {class, C} -> C;
            error      -> throw({bad_path, Class, Elem})
        end,

    Cond =
        case Filter of
            {} ->
                fun(_Fr, _Ind, _Id) -> true end;
            Index when is_integer(Index),
                       Dir =:= fwd ->
                fun(_Fr, Ind, _Id) -> Ind =:= Index end;
            last when Dir =:= fwd ->
                fun
                    (Fr, Ind, _Id) ->
                        L = length(mnesia:read({linktab(Class), {Fr, Tag}})),
                        Ind =:= L
                end;
            {Ind1, last} when is_integer(Ind1),
                              Dir =:= fwd ->
                fun(_Fr, Ind, _Id) -> Ind >= Ind1 end;
            {Ind1, Ind2} when is_integer(Ind1),
                              is_integer(Ind2),
                              Dir =:= fwd ->
                fun(_Fr, Ind, _Id) -> Ind >= Ind1 andalso Ind < Ind2 end;
            _ ->
                compile_filter(attribs(NextClass), NextClass, Filter)
        end,

    %% This query generates the result set. It depends on that objects for the
    %% same key preserve their insertion order, which gives the correct order
    %% in the result in case of forward links -- this needs to be checked in
    %% case of Mnesia. Backward links do not maintain order.
    Result =
        case Dir of
            fwd ->
                %% This query gives a result that enables QLC to recognise the
                %% lookup join in the next query
                Keys = qlc:q([{{Id, Tag}} || Id <- Query]),
                R =
                    qlc:q([{Ind, To} || {K1} <- Keys,
                                 {_, K2, Ind, To} <- mnesia:table(linktab(Class)),
                                 K1 =:= K2, Cond(element(1,K1), Ind, To)]),
                run_and_reindex(R);
            back ->
                Keys = qlc:q([{Id} || Id <- Query]),
                qlc:q([From || {_, {From, T}, _, To}
                                   <- mnesia:table(linktab(NextClass)),
                               {Id} <- Keys, To =:= Id, T =:= Tag,
                               Cond(Id, -1, From)])
        end,
    compile_path(Rest, NextClass, Result).

%% Runs the query and reorders the acquired table part by the indices.
%% This is needed because Mnesia does not maintain the table as sorted.
run_and_reindex(Query) ->
    Result = ?Exec(qlc:e(Query)),
    {_Idxs, Query2} = lists:unzip(lists:usort(Result)),
%    case length(_Idxs) /= length(Result) of
%        true -> throw(index_error);
%        false -> ok
%    end,
    Query2.

compile_filter(AttrInfo, Attrs, {'not', Filter}) ->
    F = compile_filter(AttrInfo, Attrs, Filter),
    fun(Fr, Ind, Id) -> not F(Fr, Ind, Id) end;
compile_filter(AttrInfo, Attrs, {Filt1, Op, Filt2})
  when Op =:= 'and' orelse Op =:= 'or' ->
    F1 = compile_filter(AttrInfo, Attrs, Filt1),
    F2 = compile_filter(AttrInfo, Attrs, Filt2),
    case Op of
        'and' -> fun(Fr, Ind, Id) -> F1(Fr,Ind,Id) andalso F2(Fr,Ind,Id) end;
        'or' ->  fun(Fr, Ind, Id) -> F1(Fr,Ind,Id) orelse  F2(Fr,Ind,Id) end
    end;
compile_filter(AttrInfo, Attrs, {Attr, Op, Value})
  when is_atom(Attr) ->
    case Op of
        '==' -> OpF = fun(A,B) -> A =:= B end;
        '/=' -> OpF = fun(A,B) -> A =/= B end;
        '<'  -> OpF = fun(A,B) -> A <   B end;
        '=<' -> OpF = fun(A,B) -> A =<  B end;
        '>'  -> OpF = fun(A,B) -> A >   B end;
        '>=' -> OpF = fun(A,B) -> A >=  B end
    end,
    Ind = indexof(Attr, AttrInfo) + 1,
    if
        Ind =:= 0 -> throw({bad_attribute, Attr});
        true ->
            fun (_Fr, _Ind, Id) ->
                    [{_,_,Data,_}] = mnesia:read(Attrs, Id, read),
                    OpF(element(Ind, Data), Value)
            end
    end;
compile_filter(_, _, Cond) ->
    throw({bad_condition, Cond}).

indexof(El, Lst) -> indexof(El, Lst, 1).

indexof(_,  [],        _)   -> -1;
indexof(El, [El | _],  Ind) -> Ind;
indexof(El, [_  | Tl], Ind) -> indexof(El, Tl, Ind+1).


%% TODO: this would probably be more efficient with a table stored in the
%% process state
linktab(Class) ->
    list_to_atom(atom_to_list(Class)++"$lnk").


%%% ----------------------------------------------------------------------------
%%% Schema queries

%% Maybe these would also be quicker with a table in the process state, it
%% should be measured

link_target(Class, Tag, fwd) ->
    case ?Db:exec(fun() -> mnesia:read(target, {Class, Tag}) end, rd) of
        []                  -> error;
        [#target{next=TCl}] -> {class, TCl}
    end;

link_target(Class, Tag, back) ->
    Q = qlc:q([FCl || #target{start={FCl, T}, next=TCl} <- mnesia:table(target),
                      TCl =:= Class, T =:= Tag]),
    case ?Db:exec(fun() -> qlc:e(Q) end, rd) of
        []    -> error;
        [TCl] -> {class, TCl}
    end.

link_targets(Class, fwd) ->
    Q = qlc:q([{Tag, TCl} ||
                  #target{start={FCl, Tag}, next=TCl} <- mnesia:table(target),
                  FCl =:= Class]),
    ?Db:exec(fun() -> qlc:e(Q) end, rd);

link_targets(Class, back) ->
    Q = qlc:q([{Tag, FCl} ||
                  #target{start={FCl, Tag}, next=TCl} <- mnesia:table(target),
                  TCl =:= Class]),
    ?Db:exec(fun() -> qlc:e(Q) end, rd).

attribs(Class) ->
    ?Db:exec(fun() ->
                     [#class{attribs=Attribs}] =
                         mnesia:read(class, Class),
                     Attribs
             end, rd).
