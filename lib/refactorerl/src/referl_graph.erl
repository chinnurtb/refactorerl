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

%%% @doc Graph storage server. This is a mnesia based implementation of
%%% the semantical graph server interface.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(referl_graph).
-vsn("$Rev: 1920 $").
-behaviour(gen_server).
-include("refactorerl.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(NODETAG, '$gn').
-define(TIMEOUT, 60000).

-define(IS_NODE(Node), element(1, Node) =:= ?NODETAG).

%%% ============================================================================
%%% Exports

%% Client exports
-export([start_link/0, schema/1, reset_schema/0, root/0,
         create/1, update/2, delete/1, data/1,
         mklink/3, rmlink/3, links/1, path/2, index/3, info/0]).

-export([backup/0, restore/1, undo/0, redo/0, clean/0]).

%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% ============================================================================
%%% Structures

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

%% Client functions

%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server. There will be no initial schema information,
%% {@link schema/1} must be called before the server can actually be used.
start_link() ->
    gen_server:start_link({local, ?GRAPH_SERVER}, ?MODULE, [], []).

%% @spec schema(schema()) -> ok | mismatch | already_given
%% @doc Initialises the server with a given schema. Checks wether the
%% persistent storage has the same schema as given here, and initialises it
%% if neccessary.
schema(Schema) ->
    gen_server:cast(?GRAPH_SERVER, {schema, Schema}).

%% @spec reset_schema() -> ok
%% @doc This function can be used to erase the current graph schema when a new
%% schema has to be used. Erases all data from the database.
reset_schema() ->
    gen_server:cast(?GRAPH_SERVER, {reset_schema}).

%% @spec root() -> node()
%% @doc Returns the root node.
root() ->
    gen_server:call(?GRAPH_SERVER, {root}, ?TIMEOUT).

%% @spec create(data()) -> node()
%% @doc Creates a new node. The class and attributes of the node are given by
%% Data.
create(Data) ->
    gen_server:call(?GRAPH_SERVER, {create, Data}, ?TIMEOUT).

%% @spec update(node(), data()) -> ok
%% @doc Updates the attributes of a node. The new attributes are given by
%% Data, which must have the same class as Node.
update(Node, Data) when ?IS_NODE(Node) ->
    case gen_server:call(?GRAPH_SERVER, {update, Node, Data}, ?TIMEOUT) of
        ok    -> ok;
        Error -> erlang:error(Error, [Node, Data])
    end.

%% @spec delete(node()) -> ok
%% @doc Deletes the given node.
delete(Node) when ?IS_NODE(Node) ->
    gen_server:call(?GRAPH_SERVER, {delete, Node}, ?TIMEOUT).

%% @spec data(node()) -> data()
%% @doc Returns the data associated with a node.
data(Node) when ?IS_NODE(Node) ->
    case gen_server:call(?GRAPH_SERVER, {data, Node}, ?TIMEOUT) of
        {data, Data}   -> Data;
        {error, Error} -> erlang:error(Error, [Node])
    end.

%% @spec mklink(node(), atom() | {atom(), integer()}, node()) -> ok
%% @doc Creates a link between two nodes.
mklink(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To),
                           is_atom(Tag); is_tuple(Tag) ->
    case gen_server:call(?GRAPH_SERVER, {mklink, From, Tag, To}, ?TIMEOUT) of
        bad_link -> erlang:error(bad_link, [From,Tag,To]);
        Reply    -> Reply
    end.


%% @spec rmlink(node(), atom(), node()) -> ok
%% @doc Removes a link between two nodes.
rmlink(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To), is_atom(Tag) ->
    case gen_server:call(?GRAPH_SERVER, {rmlink, From, Tag, To}, ?TIMEOUT) of
        ok    -> ok;
        Error -> erlang:error(Error, [From, Tag, To])
    end.

%% @spec links(node()) -> [{atom(), node()}]
%% @doc Returns the links starting from a node.
links(Node) when ?IS_NODE(Node) ->
    gen_server:call(?GRAPH_SERVER, {links, Node}, ?TIMEOUT).

%% @spec path(node(), path()) -> [node()]
%% @doc Evaluates a path expression starting from Node, and returns the
%% resulting nodes.
path(Node, Path) when ?IS_NODE(Node), length(Path) > 0 ->
    case gen_server:call(?GRAPH_SERVER, {path, Node, Path}, ?TIMEOUT) of
        {error, Error} -> erlang:error({bad_path, Error}, [Node, Path]);
        Reply          -> Reply
    end.

%% @spec index(node(), atom(), node()) -> integer() | none
%% @doc Returns the index of a link. If there is a link between `From' and `To'
%% with a tag `Tag', then the result is the index of this link among the links
%% with tag `Tag'. Otherwise, the result is `none'.
index(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To), is_atom(Tag) ->
    case gen_server:call(?GRAPH_SERVER, {index, From, Tag, To}, ?TIMEOUT) of
        bad_link -> erlang:error(bad_link, [From, Tag, To]);
        Reply    -> Reply
    end.


%% @spec info() -> [{atom(), integer()}]
%% @doc Returns statistics about the graph.
info() ->
    gen_server:call(?GRAPH_SERVER, {info}, ?TIMEOUT).

%% @spec backup() -> {ok,backupfile} | error
%% @doc Creates a new checkpoint from mnesia tables and returns the
%% name of the checkpoint.
backup() ->
    gen_server:call(?GRAPH_SERVER, {backup}, ?TIMEOUT).

%% @spec restore(integer()) -> restore | error
%% @doc Restores the previous state of the database from the checkpoit
%% file created by backup/0
restore(Count) ->
    gen_server:call(?GRAPH_SERVER, {restore, Count}, ?TIMEOUT).

%% @spec undo() -> undo_is_ok | invalid_checkpoint_number
%% @doc Restores the previous state of the database from the last
%% checkpoint. The actual checkpoint numbers are
%% stored in the database.
undo() ->
    gen_server:call(?GRAPH_SERVER, {undo}, ?TIMEOUT).

%% @spec redo() -> restored | invalid_checkpoint_number
%% @doc Restores the next state of the database from the last
%% checkpoint. The actual checkpoint numbers are
%% stored in the database.
redo() ->
    gen_server:call(?GRAPH_SERVER, {redo}, ?TIMEOUT).

%% @spec clean() -> cleaned_all_backups
%% @doc Cleans all backup files from the mnesia storage
clean()->
    gen_server:call(?GRAPH_SERVER, {clean}, ?TIMEOUT).

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

%% Callback functions
-define(NAME, backup).

%% Class table: stores the attribute names for every node class
-record(class, {name, attribs}).
%% Target table: stores the link target class name for every starting class,
%% link tag pair
-record(target, {start, next}).
%% Nextid table: stores the next available ID for every node class
-record(nextid, {class, id}).

%% @private
init(_) ->
    process_flag(trap_exit, true),
    %% When mnesia is started the first time, a ram only empty schema is
    %% created. This is the clue that we should start with schema
    %% initialisation.
    case mnesia:table_info(schema, storage_type) of
        ram_copies ->
            %% The schema should be stored on disc
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            %% In a new database, some tables are always created:
            %% node class table
            mnesia:create_table(
              class,
              [{attributes, record_info(fields, class)},
               {disc_copies, [node()]}]),
            %% node id table
            mnesia:create_table(
              nextid,
              [{attributes, record_info(fields, nextid)},
               {disc_copies, [node()]}]),
            %% link target class table
            mnesia:create_table(
              target,
              [{attributes, record_info(fields, target)},
               {disc_copies, [node()]}]),
            %% checkpoint counter table (needs a better name)
            mnesia:create_table(counter, [{ram_copies,[node()]}]),
            init_counter(),
            %% no_schema means we should create more tables to store the graph
            %% itself
            {ok, no_schema};
        disc_copies ->
            %% has_schema means that the existing schema should be checked
            %% against the needed schema
            mnesia:wait_for_tables([counter],infinity),
            delete_end_backups({1,up}),
            init_counter(),
            {ok, has_schema}
    end.

%% @private
handle_cast({schema, Schema}, no_schema) ->
    init_schema(Schema),
    mnesia:dirty_write(#class{name='$hash', attribs=erlang:phash2(Schema)}),
    {noreply, has_schema};

handle_cast({schema, Schema}, has_schema) ->
    mnesia:wait_for_tables([class, target, counter], infinity),
    [#class{attribs=OldHash}] = mnesia:dirty_read(class, '$hash'),
    NewHash = erlang:phash2(Schema),
    if
        OldHash =:= NewHash ->
            {noreply, has_schema};
        true ->
            error_logger:error_report(
              [{module, ?MODULE},
               {message,
                "Required and stored graph schemas are different."}]),
            {noreply, schema_error}
    end;

handle_cast({reset_schema}, _) ->
    error_logger:info_msg("Resetting database schema, restart follows\n"),
    [ mnesia:delete_table(Tab) || Tab <- mnesia:system_info(tables),
                                  Tab =/= schema],
    mnesia:change_table_copy_type(schema, node(), ram_copies),
    {stop, normal, no_schema}.

%% @private
handle_call(_Req, _F, no_schema) ->
    {stop, no_schema, no_schema, no_schema};

handle_call(_Req, _F, schema_error) ->
    {stop, schema_error, schema_error, schema_error};

handle_call({root}, _F, S) ->
    {reply, {?NODETAG, root, 0}, S};

handle_call({create, Data}, _F, S) ->
    Class = element(1, Data),
    Id = mnesia:dirty_update_counter(nextid, Class, 1),
    mnesia:dirty_write({Class, Id ,Data}),
    {reply, {?NODETAG, Class, Id}, S};

handle_call({update, {?NODETAG, Class, Id}, Data}, _F, S) ->
    if
        element(1, Data) =:= Class ->
            mnesia:dirty_write({Class, Id, Data}),
            {reply, ok, S};
        true ->
            {reply, bad_class, S}
    end;

handle_call({delete, {?NODETAG, Class, Id}}, _F, S) ->
    mnesia:dirty_delete(Class, Id),
    delete_forward_links(Class, Id),
    delete_backward_links(Class, Id),
    {reply, ok, S};

handle_call({data, {?NODETAG, root, _}}, _F, S) ->
    {reply, {data, {root}}, S};

handle_call({data, {?NODETAG, Class, Id}}, _F, S) ->
    case mnesia:dirty_read(Class, Id) of
        [{_, _,Data}] -> {reply, {data, Data}, S};
        []            -> {reply, {error, bad_node}, S}
    end;

handle_call({mklink, {?NODETAG, FCl, FId},
             TagInfo, {?NODETAG, TCl, TId}}, _F, S) ->
    case TagInfo of
        {Tag, Ind}            -> ok;
        Tag when is_atom(Tag) -> Ind = last
    end,
    case mnesia:dirty_read(target, {FCl, Tag}) of
        [#target{next=TCl}] ->
            if
                Ind =:= last -> append_link(linktab(FCl), FId, TId, Tag);
                true         -> insert_link(linktab(FCl), FId, TId, Tag, Ind)
            end,
            {reply, ok, S};
        _ ->
            {reply, bad_link, S}
    end;

handle_call({rmlink, {?NODETAG, FCl, FId},
             Tag, {?NODETAG, TCl, TId}}, _F, S) ->
    case mnesia:dirty_read(target, {FCl, Tag}) of
        [#target{next=TCl}] ->
            {reply, remove_link(linktab(FCl), FId, TId, Tag), S};
        _ ->
            {reply, bad_link, S}
    end;

handle_call({links, {?NODETAG, Class, Id}}, _F, S) ->
    T = qlc:q([{{Id, STag}, TCl} ||
                  #target{start={SCl, STag}, next=TCl} <- mnesia:table(target),
                  SCl =:= Class]),
    Q = qlc:q([ {Tag, {?NODETAG, TCl, To}} ||
                  {ST={_,Tag}, TCl} <- T,
                  {_, FL, _Ind, To} <- mnesia:table(linktab(Class)),
                  ST =:= FL]),
    {reply, mnesia:async_dirty(fun () -> qlc:e(Q) end), S};

handle_call({path, {?NODETAG, Class, Id}, Path}, _F, S) ->
    try compile_path(Path, Class, [Id]) of
        Query ->
            Reply = mnesia:async_dirty(fun()-> qlc:e(Query) end),
            {reply, Reply, S}
    catch
        throw:Msg ->
            {reply, {error, Msg}, S}
    end;

handle_call({index, {?NODETAG, FCl, FId},
             Tag, {?NODETAG, TCl, TId}}, _F, S) ->
    case mnesia:dirty_read(target, {FCl, Tag}) of
        [#target{next=TCl}] ->
            Q = ets:fun2ms(fun ({_, Key, Ind, To})
                               when Key =:= {FId, Tag},
                                    To =:= TId -> Ind end),
            case mnesia:dirty_select(linktab(FCl), Q) of
                []                        -> {reply, none, S};
                [Ind]                     -> {reply, Ind, S};
                Multi when is_list(Multi) -> {reply, Multi, S}
            end;
        _ ->
            {reply, bad_link, S}
    end;

handle_call({info}, _F, S) ->
    WS = erlang:system_info(wordsize),
    Classes = [N || #class{name=N} <-
                        mnesia:dirty_match_object(#class{_='_'})],
    {reply,
     [ tabinfo(Info, WS) ||
         Info <- [{nodes, Classes},
                  {links, [linktab(C) || C <- Classes]},
                  {tree, [tree_node, tree_link, removed, inserted]}]],
     S};

handle_call({backup}, _F, S) ->
    {reply, checkpoint(), S};

handle_call({restore, Count}, _F, S) ->
    restore_checkpoint(Count),
    {reply, restore, S};

handle_call({undo}, _F, S) ->
    IdAkt = mnesia_idakt(),
    Id    = mnesia_id(),
    if
        IdAkt > 0 ->
            restore_checkpoint(Id),
            mnesia:dirty_write({counter, 1, IdAkt - 1}),
            Reply = undo_is_ok;
        true ->
            Reply = invalid_checkpoint_number
    end,
    {reply, Reply, S};

handle_call({redo}, _F, S) ->
    IdAkt = mnesia_idakt(),
    Id    = mnesia_id(),
    if
        IdAkt < Id ->
            restore_checkpoint(IdAkt + 1),
            mnesia:dirty_write({counter, 1, IdAkt + 1}),
            Reply = redo_is_ok;
        true ->
            Reply = invalid_checkpoint_number
    end,
    {reply, Reply, S};

handle_call({clean}, _F, S) ->
    Id    = mnesia_id(),
    {reply, delete_all_backups(Id), S}.

%%% ----------------------------------------------------------------------------
%%% Implementation details


delete_forward_links(Class, Id) ->
    Tags = mnesia:dirty_select(
             target,
             ets:fun2ms(fun (#target{start={Cls, Tag}})
                            when Cls =:= Class -> Tag
                        end)),
    [ ok = mnesia:dirty_delete(linktab(Class), {Id, Tag}) ||
        Tag <- Tags].

delete_backward_links(Class, Id) ->
    Links =
        lists:foldl(
          fun
              (#target{start={FCl, FTag}}, Lnk) ->
                  orddict:append(FCl, FTag, Lnk)
          end,
          [],
          mnesia:dirty_match_object(#target{next=Class, _='_'})),

    [ mnesia:dirty_delete_object(Rec) ||
        {FCl, Tags}             <- Links,
        Rec = {_,{_,FTag}, _,_} <-
            mnesia:dirty_index_match_object({linktab(FCl), '_', '_', Id}, 4),
        lists:member(FTag, Tags) ].

tabinfo({Tag, Tables}, WS) ->
    TableSizes = [mnesia:table_info(T, size)   || T <- Tables],
    TableMems  = [mnesia:table_info(T, memory) || T <- Tables],
    {Tag, lists:sum(TableSizes), WS * lists:sum(TableMems)}.


%% Replaces the set of links completely with the new set
insert_link(Links, From, To, Tag, Ind) ->
    LinkList = mnesia:dirty_read(Links, {From,Tag}),
    NewLinks =
        [ Lnk || Lnk = {_, _, I, _} <- LinkList, I < Ind ]++
        [ {Links, {From, Tag}, Ind, To} ] ++
        [ {L, F, I+1, T} || {L, F, I, T} <- LinkList, I >= Ind ],

    mnesia_replace_links(From, Tag, Links, NewLinks).


%% Only creates the new link
append_link(Links, From, To, Tag) ->
    Ind = length(mnesia:dirty_read(Links, {From, Tag})) + 1,
    mnesia:dirty_write({Links, {From, Tag}, Ind, To}).

%%% ----------------------------------------------------------------------------
%%% Utility functions

%% Removes the old links from Mnesia and inserts the new ones instead.
mnesia_replace_links(From, Tag, OldLinks, NewLinks) ->
    mnesia:dirty_delete(OldLinks, {From,Tag}),
    [mnesia:dirty_write(Lnk) || Lnk <- NewLinks].

%% Replaces the set of links completely
remove_link(OldLinks, From, To, Tag) ->
    LinkList = mnesia:dirty_read(OldLinks, {From, Tag}),
    case  [I || {_, _, I, T} <- LinkList, T == To] of
        [Ind] ->
            NewLinks =
                [ Lnk || Lnk = {_, _, I, _} <- LinkList, I < Ind ] ++
                [ {L, F, I-1, T} || {L, F, I, T} <- LinkList, I > Ind ],
            mnesia_replace_links(From, Tag, OldLinks, NewLinks),
            ok;
        [] ->
            not_exists;
        _ ->
            exit({multiple_links, Tag})
    end.



%% Returns the number of checkpoints in the system.
checkpoint_count() ->
    mnesia_id() + 1.

%%
name(Name, Number) ->
    atom_to_list(Name) ++ "." ++ integer_to_list(Number).


mnesia_idakt() ->
    [{_, _, IdAkt}] = mnesia:dirty_read({counter,1}),
    IdAkt.

mnesia_id() ->
    [{_, _, Id}] = mnesia:dirty_read({counter,0}),
    Id.

%%% ----------------------------------------------------------------------------
%%% Checkpoints

init_counter() ->
    mnesia:dirty_write({counter,0,0}),
    mnesia:dirty_write({counter,1,0}).

checkpoint()->
    %%temporary function call
    delete_all_backups(1),
    Name  = name(backup, 1),%checkpoint_count()),
    %checkpoint_number/0 is need to create more backup files
    Tab   = mnesia:system_info(tables),
    Args  = [{name,Name},{max, Tab},
             {allow_remote,true},{ram_overrides_dump,true}],
    catch mnesia:activate_checkpoint(Args),
    Dir   = mnesia:system_info(directory),
    File  = filename:absname_join(Dir,Name),
    catch mnesia:backup_checkpoint(Name,File),
    mnesia:deactivate_checkpoint(Name),
    mnesia:dirty_write({counter, 0, checkpoint_count()}),
    IdAkt = mnesia_idakt(),
    mnesia:dirty_write({counter, 1, IdAkt + 1}),
    {ok, Name}.

delete_all_backups(0)->
   mnesia:dirty_write({counter, 0, 0}),
   mnesia:dirty_write({counter, 1, 0}),
   {ok, backups_deleted};
delete_all_backups(Id)->
   Name = name(backup, Id),
   file:delete(filename:absname_join(mnesia:system_info(directory),Name)),
   delete_all_backups(Id - 1).


delete_end_backups({Id, up})->
    Name      = name(backup, Id),
    Directory = mnesia:system_info(directory),
    File      = filename:absname_join(Directory,Name),
    case filelib:is_file(File) of
        true ->
            file:delete(
              filename:absname_join(mnesia:system_info(directory),Name)),
            Dir = up;
        _ ->
            Dir = down
    end,
    delete_end_backups({Id + 1, Dir});
delete_end_backups({Id, down})->
    {Id - 1, deleted}.


restore_checkpoint(Count)->
    Name = name(?NAME, Count),
    Dir  = mnesia:system_info(directory),
    File = filename:absname_join(Dir,Name),
    mnesia:dirty_write({counter, 1, Count + 1}),
    catch mnesia:restore(File,[{recreate_tables, []},
                               {skip_tables, [counter]}]).


init_schema([]) ->
    ok;
init_schema([{Class, Attribs, Links} | Tail]) ->
    if
        Class =/= root ->
            mnesia:create_table(
              Class,
              [{attributes, [id, attribs]},
               {disc_copies, [node()]}]);
        true -> ok
    end,

    mnesia:create_table(
        linktab(Class),
        [{attributes, [id, ind, to]},
         {disc_copies, [node()]},
         {index, [to]},
         {type,bag}]),

    mnesia:dirty_write(#class{name=Class, attribs=Attribs}),
    [ mnesia:dirty_write(#target{start={Class, Tag}, next=To}) ||
        {Tag, To} <- Links],

    init_schema(Tail).

compile_path([], Class, Query) ->
    %%error_logger:info_msg("~s~n", [qlc:info(Query)]),
    qlc:q([ {?NODETAG, Class, Id} || Id <- Query]);

compile_path([S={intersect, {?NODETAG, SCl, SId}, Step} | Rest],
             Class, Query) ->
    case Step of
        {Tag, back} ->
            case mnesia:dirty_read(target, {Class, Tag}) of
                [#target{next = SCl}] -> ok;
                _                     -> throw({bad_class, S, Class})
            end,
            Dir = back;

        Tag when is_atom(Tag) ->
            case mnesia:dirty_read(target, {SCl, Tag}) of
                [#target{next = Class}] -> ok;
                _                       -> throw({bad_class, S, Class})
            end,
            Dir = fwd
    end,

    Result =
        case Dir of
            fwd ->
                qlc:q([Id || {_, From, _, To} <- mnesia:table(linktab(SCl)),
                             From =:= {SId, Tag},
                             Id <- Query, Id =:= To]);
            back ->
                qlc:q(
                  [Id || {_, From, _,To} <- mnesia:table(linktab(Class)),
                         SId =:= To,
                         Id <- Query, From =:= {Id, Tag}])
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
        case Dir of
            fwd ->
                case mnesia:dirty_read(target, {Class, Tag}) of
                    [#target{next=NC}] -> NC;
                    _                  -> throw({bad_path, Class, Elem})
                end;
            back ->
                case mnesia:dirty_match_object(
                       #target{start={'_', Tag}, next=Class, _='_'}) of
                    [#target{start={NC, _}}] -> NC;
                    _                        -> throw({bad_path, Class, Elem})
                end
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
                        L = length(mnesia:dirty_read(linktab(Class),
                                                     {Fr, Tag})),
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
                [#class{attribs=Attribs}] =
                    mnesia:dirty_read(class, NextClass),
                compile_filter(Attribs, NextClass, Filter)
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
                qlc:q([To || {K1} <- Keys,
                             {_, K2, Ind, To} <- mnesia:table(linktab(Class)),
                             K1 =:= K2, Cond(element(1,K1), Ind, To)]);
            back ->
                Keys = qlc:q([{Id} || Id <- Query]),
                qlc:q([From || {_, {From, T}, _, To}
                                   <- mnesia:table(linktab(NextClass)),
                               {Id} <- Keys, To =:= Id, T =:= Tag,
                               Cond(Id, -1, From)])
        end,
    compile_path(Rest, NextClass, Result).

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
                    [{_,_, Data}] = mnesia:dirty_read(Attrs, Id),
                    OpF(element(Ind, Data), Value)
            end
    end;
compile_filter(_, _, Cond) ->
    throw({bad_condition, Cond}).

indexof(El, Lst) -> indexof(El, Lst, 1).

indexof(_,  [],        _)   -> -1;
indexof(El, [El | _],  Ind) -> Ind;
indexof(El, [_  | Tl], Ind) -> indexof(El, Tl, Ind+1).


%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    delete_end_backups({1,up}),
    ok.

%% @private
code_change(_Old, State, _Extra) ->
    {ok, State}.


%% @private
linktab(Class) ->
    list_to_atom(atom_to_list(Class)++"$lnk").
