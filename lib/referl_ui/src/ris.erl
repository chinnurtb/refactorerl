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

-export([q/1,q1/1,qstr/1]).
-export([show/1,show/2]).

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
-define(SQM,?MODULE).
-define(TRANSFORM_SERVER,transform_server).
-record(statistic,{v}).
-record(entity,{e}).
-record(property,{v}).

% @todo
error_text(ErrType, ErrParams) ->
    ["ris internal error - unknown error message: {",
     io_lib:print(ErrType), ", ", io_lib:print(ErrParams), "}"].

%%% ============================================================================

q(X)->
    q(X,none).

q({Q1,union,Q2},S)->
    lists:usort(q(Q1,S)++q(Q2,S));
q({Q1,intersect,Q2},S)->
    ?MISC:intersect(q(Q1,S),q(Q2,S));
q({Q1,minus,Q2},S) ->
    q(Q1,S)--q(Q2,S);
q({Q1,range,Q2},S) ->
    todo;

q(Sel,S)when is_atom(Sel)->
    q(atom_to_list(Sel),S);

q(Sel,S)when is_list(Sel)->
    case lists:all(fun is_entity/1,Sel) of
        true->
            Sel;
        false->
            case io_lib:char_list(Sel) of %@todo
                true->
                    q_start(Sel,none);
                false ->
                    lists:foldl(fun(Sel1,Start)->
                                    q(Sel1,Start)
                                end, S, Sel)
            end
    end.

q_start(Sel,none)->
    ?SQM:run([{output,nodes}],[],Sel);
q_start(Sel,S)->
    ?SQM:run([{output,nodes}],[{start,S}],Sel).

is_entity(#entity{})->
    true;
is_entity(_)->
    false.
    %@todo ?SQ:is_entity/1

q1(Sel)->
    case hd(q(Sel)) of
        #entity{e=E}->
            E;
        #property{v=V}->
            V;
        #statistic{v=V}->
            V
    end.

qstr(Sel)->
    R = q1(Sel),
    case erlang_type(R) of
        atom ->
            atom_to_list(R);
        integer->
            integer_to_list(R);
        list ->
            case io_lib:char_list(Sel) of %@todo
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

show(Result)->
    show(Result,[]).

show(Result,Options0)->
    Options =
      [case E of
         {out,FileName} ->
            {ok,IODev} = file:open(FileName, [write]),
                 % the file will be closed on process termination
            {output,{iodev,IODev}};
         {linenum,B}when is_boolean(B)->
            {positions,linecol}
       end || E <- proplists:unfold(Options0)],
    ?SQM:show_(Result,Options).

%%% ============================================================================

query_nodes(Source,NodeTypes)->
    guard_nodetype(q(Source),NodeTypes).

guard_nodetype(Nodes,NodeTypes)->
    Accepted = ?MISC:flatsort([NodeTypes]),
    case is_subset(entity_types(Nodes),Accepted) of
        true  -> Nodes;
        false -> throw(todo)
    end.

dispatch_nodetype(Nodes,Dispatchers)->
    Got = entity_types(Nodes),
    [Value ||
    {NodeTypes,Value} <- Dispatchers,
    Accepted <- ?MISC:flatsort([NodeTypes]),
    true==is_subset(Got,Accepted)].

entity_types(#statistic{})->
    statistic;
entity_types(Nodes)->
    lists:usort(lists:map(fun entity_type/1),Nodes).

entity_type(#statistic{})->
    statistic;
entity_type(#property{})->
    property;
entity_type(#entity{e=Entity})->
    ?SQ:entity_type(Entity).

is_subset(Sub,Sup)->
    []==Sub--Sup.

ui({Name,Args})->
    pong = net_adm:ping(?REFERL_NODE),
%    monitor_node(?REFERL_NODE, true),
    Ref = {?TRANSFORM_SERVER,?REFERL_NODE},
    link(Ref),
    erlang:apply(?UI,Name,Args),
    Result = get_all_msg(),
    unlink(Ref),
    Result.

add(Source)->
    add_drop(add_dir,Source).

drop(Source)->
    add_drop(drop_dir,Source).

add_byname(Source)->
    each_ui(add_dir,[Source]).

add_drop(Which,Source)->
    each_ui(Which,query_nodes([Source,"@file.name"],file)).

each_ui(Func,Files) ->
    [ui({Func,File}) || File <- Files].


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

rename(Old,New)->
    L=[{function,{rename_fun,name}},
       {variable,{rename_var,varname}},
       {file,{rename_header,name}},
       {mod,{rename_mod,name}},
       {record,{rename_rec,name}},
       {record_field,{rename_recfield,name}},
       {macro,{rename_macro,name}}],
    Nodes = q(Old),
    {Fun,Key} = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes},{Key,New}]}).

move(Source,Dest)->
    L=[{function,{move_fun,name}},
    %@todo convert
       {record,{move_rec,filename}},
       {macro,{move_macro,filename}}],
    Nodes = q(Source),
    {Fun,Key} = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes},{Key,Dest}]}).

extract(Source,New)->
    L=[{function,extract_fun}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes},{name,New}]}).

inline(Source)->
    L=[{function,inline_fun},
       {macro,inline_mac}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes}]}).

eliminate(Source)->
    L=[{variable,elim_var}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes}]}).

%% -----------------------------------------------------------------------------

intrec(Source,{Name,Fields})->
    %@todo convert
    L=[{expression,introduce_rec}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes},{name,Name},{fields,Fields}]}).

intmac(Source,New)->
    %@todo convert
    L=[{expression,introduce_macro}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes},{name,New}]}).

merge(Source,New)->
    L=[{expression,merge}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes},{varname,New}]}).

genfun(Source,New)->
    L=[{function,gen}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes},{name,New}]}).

expfun(Source)->
    L=[{function,expand_funexpr}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes}]}).

tupfun(Source, _Range)->
    %@todo alternate calling
    L=[{variable,tuple_funpar}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes}]}).

reorder(Source, Order)->
    %@todo validate
    L=[{function,reorder_funpar}],
    Nodes = q(Source),
    Fun = dispatch_nodetype(Nodes,L),
    ui({transform,Fun,[{nodes,Nodes},{order,Order}]}).

upregex()->
    ui({transform,upgrade_regexp,[]}).

%% -----------------------------------------------------------------------------

get_all_msg()->
    get_all_msg(uifinished).
    %trfinished

get_all_msg(F) ->
    get_all_msg_loop(F,nostatus).

%% @private
%% @doc print out all messages of results
get_all_msg_loop(F,S) ->
    receive
        {F,[]}         -> finished;
        {F,_}          -> S;
        {uifinished,_} -> get_all_msg_loop(F,S);
        {status,S2}    -> get_all_msg_loop(F,S2);
        {invalid,_}    -> {error,get_all_msg_loop(F,S)};
        {progress, _}  -> get_all_msg_loop(F,S)
    end.

%% -----------------------------------------------------------------------------


% mockups

% @todo
run(Disp,Start,Query)->
    Result = ?SQ:run(Disp,Start,Query),
    case is_list(Result) of
        false->
            [#statistic{v=Result}];
        true->
            [case ?SQM:is_entity_(E) of
                 true->
                     #entity{e=E};
                 false->
                     #property{v=E}
             end || E <- Result]
    end.

show_(Result)->
    show_(Result,[]).
show_(Result,Options)->
    %@todo
    [case T of #entity{e=N}->?Graph:data(N);_->T end ||T<-Result].

is_entity_(N)->
    true.
