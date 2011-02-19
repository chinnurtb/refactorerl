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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information
%%%
%%% @doc @todo

-module(ris).
-vsn("$Rev: 9876 $ ").

-export([q/1,q1/1,qstr/1, unpack/1]).
-export([show/1,show/2, print/1, print/2]).

-export([add/1, add_byname/1, drop/1]).
-export([envs/0, env/1, env/2, addenv/3, setenv/2,
         delenv/1, delenv/2, delenv/3]).

-export([rename/2, move/2, extract/2, inline/1, eliminate/1, reorder/2,
     intrec/2, intmac/2, merge/2, genfun/2, expfun/1, tupfun/2,
     upregex/0]).
-export([error_text/2]).
% build, test, errors, cat_errors,

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

-define(SQ,refusr_sq).
-define(TRANSFORM_SERVER,transform_server).
-record(statistic,{v}).
-record(entity,{e}).
-record(property,{v}).
-record(rich, {text, nodes}).

% @todo
error_text(ErrType, ErrParams) ->
    ["ris internal error - unknown error message: {",
     io_lib:print(ErrType), ", ", io_lib:print(ErrParams), "}"].

% ris:q("mods").
% ris:q("mods.name").
% ris:q("mods.fun").
% ris:q("mods.fun.arity:sum").
% ris:q("mods.fun.(refs.fundef)+").
% ris:q("mods.fun.{refs.fundef}2").
% ris:q([mods,".fun",".name"]).
% ris:add({"../a.erl"}).
% ris:drop({"/root/erl/tool/../a.erl"}).
% ris:add_byname("../a.erl").
% ris:add(mods).
% ris:drop('mods[name=="a"]').


%%% ============================================================================

qd(Sel)->
    desugar(q(Sel)).

qd(Sel,S) ->
    desugar(q(Sel, S)).

desugar(#rich{nodes=N})->
    N;
desugar(X)->
    X.

q(X)->
    q(X,none).

q({Q1,union,Q2},S)->
    lists:usort(qd(Q1,S)++qd(Q2,S));
q({Q1,intersect,Q2},S)->
    ?MISC:intersect(qd(Q1,S), qd(Q2,S));
q({Q1,minus,Q2},S) ->
    qd(Q1,S)--qd(Q2,S);
q({Q1,range,Q2},S) ->
    todo;

q(Sel,S)when is_atom(Sel)->
    q(atom_to_list(Sel),S);

q({Sel},S)when is_list(Sel)->
    q(["files[path==\"" ++ Sel ++ "\"]"],S);

q(Sel=#rich{},S)->
    q(desugar(Sel),S);

q(Sel,S=#rich{})->
    q(Sel,desugar(S));

q(Sel,S)when is_list(Sel)->
    case lists:all(fun is_entity/1,Sel) of
        true->
            Sel;
        false->
            case io_lib:char_list(Sel) of %@todo
                true->
                    q_start(Sel,S);
                false ->
                    lists:foldl(fun(Sel1,Start)->
                                    q(Sel1,Start)
                                end, S, Sel)
            end
    end.

q_start(Sel,none)->
    run_q([{output,nodes}],[],Sel);
q_start(Sel,[])->
    [];
q_start(Sel,Start)->
    Nodes = [N || #entity{e=N} <- Start],
    case Nodes of
        [] -> [];
        _ ->
        run_q([{output,nodes}],[{node_list,Nodes}],Sel)
    end.

is_entity(#entity{})->
    true;
is_entity(_)->
    false.
    %@todo ?SQ:is_entity/1

q1(Sel)->
    hd(unpack(q(Sel))).

unpack(Sel)->
    [case X of
         #entity{e=E}->
             E;
         #property{v=V}->
             V;
         #statistic{v=V}->
             V
     end || X <- desugar(Sel)].

qstr(Sel)->
    R = q1(Sel),
    case erlang_type(R) of
        atom ->
            atom_to_list(R);
        integer->
            integer_to_list(R);
        list ->
            case io_lib:char_list(R) of %@todo
                true -> R
            end
    end.

erlang_type(X) when is_atom(X)->
    atom;
erlang_type(X) when is_integer(X)->
    integer;
erlang_type(X) when is_list(X)->
    list;
erlang_type(_) ->
    todo.

print(R)->
    io:format("~s~n",[show(R)]).

print(R,Options)->
    io:format("~s~n",[show(R,Options)]).

show(#rich{text=T})->
    T;

show(Result)->
    show(Result,[]).

show(Result0,Options)->
    Result = unpack(Result0),
    case ?MISC:pget([linenum,linecol],Options) of
        [[],[]]->
            ?SQ:format_nodes(Result, none);
        [_,_] ->
            ?SQ:format_nodes(Result, linecol)
    end.

%%% ============================================================================

query_nodes(Source,NodeTypes)->
    guard_nodetype(qd(Source),NodeTypes).

guard_nodetype(Nodes,NodeTypes)->
    Accepted = ?MISC:flatsort([NodeTypes]),
    case is_subset(entity_types(Nodes),Accepted) of
        true  -> Nodes;
        false -> throw(todo)
    end.

dispatch_nodetype([],_Dispatchers)->
    throw(?LocalError(empty_selection,[]));
dispatch_nodetype(Nodes,Dispatchers)->
    Got = entity_types(Nodes),
    R =
        [Value ||
            {NodeTypes,Value} <- Dispatchers,
            Accepted <- [lists:flatten([NodeTypes])],
        true==is_subset(Got,Accepted)],
    case R of
        [] ->
            throw(?LocalError(no_match,[Got]));
        [_] ->
            R;
        [_|_] ->
            throw(?LocalError(multi_match,[Got,R]))
    end.

entity_types(#statistic{})->
    statistic;
entity_types(Nodes)->
    lists:usort(lists:map(fun entity_type/1,Nodes)).

entity_type(#statistic{})->
    statistic;
entity_type(#property{})->
    property;
entity_type(#entity{e=Entity})->
    node_type(Entity).
%    ?SQ:entity_type(Entity).

node_type(Node)->
    case ?Syn:node_type(Node) of
        form     -> macro;
        variable -> variable;
        field    -> recfield;
        record   -> record;
        module   -> mod;
        file     -> file;
        func     -> function;
        expr     -> expression
    end.


is_subset(Sub,Sup)->
    []==Sub--Sup.

refac(Name, Args)->
    Res = transform(Name, Args),
    to_entities(Res).

transform(Name, Args)->
    Res = ui({transform, Name, [{ask_missing,false} | Args]}),
    case Res of
        {ok, ResultU} ->
            case ResultU of
                {result,R} ->
                    [Result] = ?MISC:pgetu([result],R),
                    Result;
                {abort, {_,_}} -> throw(ResultU)
            end;
        {error, {_,_}} -> throw(Res)
    end.

ui(NameArgs)->
    pong = net_adm:ping(?REFERL_NODE),
%    monitor_node(?REFERL_NODE, true),
%    Ref = {?TRANSFORM_SERVER,?REFERL_NODE},
    ReqID = ?UI:getid(),
    ok = ?UI:request(ReqID,NameArgs),
    ui_loop(ReqID).

ui_loop(ReqID)->
    ui_loop(ReqID,[]).

ui_loop(ReqID,Errors)->
    receive
        {ReqID, reply, R} ->
            case Errors of
                []     -> R;
                [Error]-> {ok,{error,{Error,?Error:error_text(Error)}}}
            end;
        {ReqID, progress, _D} ->
            ui_loop(ReqID,Errors);
        {ReqID, question, Q={QID,_Questions}} ->
            ?UI:request(ReqID, {cancel, QID}),
            ui_loop(ReqID,[?LocalError(no_interaction,[Q])])
    end.

add({Filename=[C|_]}) when is_integer(C)->
    add_byname(Filename);

add(Source)->
    add_drop(add_dir,Source).

drop(Source)->
    add_drop(drop_dir,Source).

add_byname(Source)->
    {ok, N} = ui({add_dir,[Source]}),
    to_entities(N).

add_drop(Which,Source)->
    ModFile = query_nodes(Source,[mod, file]),
    each_ui(Which, unpack(q([ModFile,".path"]))).

each_ui(Func,Files) ->
    Success =
        [begin
            Res = ui({Func,File}),
            case {Func,Res} of
                {drop_dir, {ok,_}} -> [File];
                {add_dir, {ok,N}} -> to_entities(N);
                {drop_dir,_} -> []; %@todo only accept if missing
                {add_dir,_} -> [] %@todo exception
            end
         end || File <- Files],
    lists:append(Success).


%% -----------------------------------------------------------------------------

envs()->
    lists:map(fun show_env/1, ?Syn:get_envs()).

%% @doc Lists a specific environment node
env(Name)->
    show_env({Name,?Syn:get_env(Name)}).

%#env{name=env_var, value=[{EnvName, EnvVal}]}

%% @doc Adds a new subkey to a proplist environment node
addenv(Name, EnvName, EnvVal) when is_atom(EnvName)->
    addenv(Name, atom_to_list(EnvName), EnvVal);
addenv(Name, EnvName, EnvVal) when is_atom(EnvVal)->
    addenv(Name, EnvName, atom_to_list(EnvVal));
addenv(Name, EnvName, EnvVal)->
    ?Syn:add_env(Name, {EnvName, EnvVal}).

%% @doc Deletes a subkey from a proplist environment node
delenv(Name, EnvName) when is_atom(EnvName)->
    delenv(Name,atom_to_list(EnvName));
delenv(Name, EnvName)->
    ?Syn:del_env_sub(Name,EnvName).

%% @doc Deletes a specific key-value pair from a proplist environmnent node
delenv(Name, EnvName, EnvVal) when is_atom(EnvName)->
    delenv(Name, atom_to_list(EnvName), EnvVal);
delenv(Name, EnvName, EnvVal) when is_atom(EnvVal)->
    delenv(Name, EnvName, atom_to_list(EnvVal));
delenv(Name, EnvName, EnvVal)->
    ?Syn:del_env_val(Name,{EnvName,EnvVal}).

%% @doc Lists the value for a subkey of a proplist environment node
env(Name, EnvName) when is_atom(EnvName)->
    env(Name, atom_to_list(EnvName));
env(Name, EnvName)->
    case ?Syn:get_env(Name,EnvName) of
        [] ->
            throw(todo);
        L=[_|_] ->
            show_env({Name,[{EnvName,EV} || EV <- L]})
    end.

%#env{name=output}
%#env{name=backup}

%% @doc Sets the value of an environment node
%% @todo setenv/3
setenv(Name, Value)->
    case ?Syn:env_type(Name) of
        proplist ->
            throw(todo);
        atomic ->
            ?Syn:set_env(Name,Value)
    end.

%% @doc  Deletes an environment node
delenv(Name)->
    ?Syn:del_env(Name).

show_env({_Name,[]})->
    throw(todo);
show_env({Name,Values})->
    case ?Syn:env_type(Name) of
        proplist ->
            {Name, Values};
        atomic ->
            [Value] = Values,
            {Name,Value}
    end.

%% -----------------------------------------------------------------------------

%%% ============================================================================

any_to_atom(X) when is_atom(X)->
    X;
any_to_atom(L=[C|_]) when is_integer(C)->
    list_to_atom(L).

any_to_list(X) when is_atom(X)->
    atom_to_list(X);
any_to_list(L=[C|_]) when is_integer(C)->
    L.

rename(Old,New)->
    Atom = fun()-> {name,any_to_atom(New)} end,
    List = fun(Key) -> fun()-> {Key, any_to_list(New)} end end,
    L=[{function,{rename_fun,Atom}},
       {variable,{rename_var,List(varname)}},
       {file,{rename_header,List(filename)}},
       {mod,{rename_mod,Atom}},
       {record,{rename_rec,Atom}},
       {recfield,{rename_recfield,Atom}},
       {macro,{rename_mac,List(macname)}}],
    Nodes = qd(Old),
    [{Fun,Key}] = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},Key()]).

% ris:rename('mods[name=="a"].fun[name==f]', h).
% ris:rename('mods[name=="a"].fun[name==f].var[name=="X"]', "Y").
% ris:rename('files[name=="e.hrl"]', "y.hrl").
% ris:rename('mods[name=="a"]', "b").
% ris:rename('files.macros[name=="M"]', "Mac2").
% ris:rename('files.records[name==rec1]', rec2).
% ris:rename('files.records[name==rec1].fields[name==r1]', fld1).

move(Source,Dest)->
    Atom = fun()-> {name,any_to_atom(Dest)} end,
    File =
        fun() ->
            Path =
                case Dest of
                    {Path_} -> Path_;
                    _ -> qstr("files[name==\"" ++ any_to_list(Dest) ++ "\"].path")
                end,
            {filename, Path} end,
    L=[{function,{move_fun,Atom}},
       {record,{move_rec, File}},
       {macro,{move_mac,File}}],
    Nodes = qd(Source),
    [{Fun,Key}] = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},Key()]).

% ris:move('mods[name=="a"].fun[name==f]', b).
% ris:move('mods[name=="c"].record[name==rec]', x).
% ris:move('mods[name=="c"].macro[name=="M"]', x).

extract(Source,New)->
    L=[{function,extract_fun}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},{name,any_to_atom(New)}]).

inline(Source)->
    L=[{function,inline_fun},
       {macro,inline_mac}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)}]).

eliminate(Source)->
    L=[{variable,elim_var}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)}]).

%% -----------------------------------------------------------------------------

intrec(Source,{Name,Fields}) when is_atom(Name), is_list(Fields)->
    %@todo convert
    L=[{expression,introduce_rec}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},{name,Name},{fields,Fields}]).

intmac(Source,NewName)->
    %@todo convert
    L=[{expression,introduce_macro}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},{name,any_to_list(NewName)}]).

merge(Source,New)->
    L=[{expression,merge}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},{varname,New}]).

genfun(Source,NewVar)->
    L=[{function,gen}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},{name,NewVar}]).

expfun(Source)->
    L=[{function,expand_funexpr}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)}]).

tupfun(Source, Range)->
    %@todo alternate calling
    L=[{variable,tuple_funpar}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)}]). %@todo

reorder(Source, Order=[_|_])->
    %@todo validate
    L=[{function,reorder_funpar}],
    Nodes = qd(Source),
    Fun = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},{order,Order}]).

upregex()->
    refac(upgrade_regexp,[]).


%% -----------------------------------------------------------------------------


% mockups

% @todo
run_q(DispOpt,Start,Query)->
    Args = [{display_opt,DispOpt},
            {start_opt,Start},
            {querystr,Query}],
%    ?d(Args),
    Result = transform(semantic_query, Args),
    [Text,Nodes] = ?MISC:pgetu([text,nodes], Result),
    V = case is_list(Nodes) of
            false->
                [#statistic{v=Nodes}];
            true->
                to_entities(Nodes)
        end,
%    ?d(V),
    #rich{text=Text, nodes=V}.

to_entities(Nodes)->
    [case is_entity_(E) of
        true->
            #entity{e=E};
        false->
            #property{v=E}
     end || E <- Nodes].

is_entity_({'$gn',_,_})->
    true;
is_entity_(_)->
    false.
