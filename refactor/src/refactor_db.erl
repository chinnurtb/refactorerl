%%% Copyright Notice © 2007 Eötvös Loránd University and Ericsson Hungary
%%% Software development supported by Ericsson Hungary and
%%% GVOP-3.2.2-2004-07-0005/3.0 ELTE IKKK.

%%% Materials  were  created, authored,  and/or  prepared  by  the Authors  at
%%% Department   of  Programming  Languages   and  Compilers,   Eötvös  Loránd
%%% University,  Budapest,  Hungary  (ELTE)  for Ericsson  Hungary  under  the
%%% agreement  between  Ericsson  Hungary  and  ELTE  IKKK.  Unless  otherwise
%%% specifically stated, no claim to copyright is being asserted and it may be
%%% freely  used as  in the  public domain  in accordance  with  Erlang Public
%%% License.  All rights,  including copyright,  are owned  or  controlled for
%%% these purposes by  the Ericsson Hungary and ELTE.  Copyright exists in all
%%% other original  material published on the  internet and may  belong to the
%%% authors depending on the circumstances of publication.

%%% --------------------------------------------------------------------------
%%% ``The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1,  (the  "License"); you  may  not  use  this file  except  in
%%% compliance with the License. You should have received a copy of the Erlang
%%% Public License along  with this software. If not, it  can be retrieved via
%%% the world wide web at http://www.erlang.org/.

%%% Software distributed under the License is distributed on an "AS IS" basis,
%%% WITHOUT WARRANTY OF  ANY KIND, either express or  implied. See the License
%%% for  the specific  language  governing rights  and  limitations under  the
%%% License.

%%% The Initial  Developer of  the Original Code  is Ericsson  Utvecklings AB.
%%% Portions created by Ericsson  are Copyright 1999, Ericsson Utvecklings AB.
%%% All Rights Reserved.''
%%% --------------------------------------------------------------------------

%%% The Contributors are the Authors listed below. All Rights Reserved.

%%% You may not alter or remove any trademark, copyright or other notice from
%%% copies of the content.

%%% Authors: Zoltán Csörnyei
%%%          Zoltán Horváth
%%%          Roland Király
%%%          Róbert Kitlei
%%%          Tamás Kozsik
%%%          László Lövei
%%%          Tamás Nagy
%%%          Melinda Tóth
%%%          Anikó Víg

%%% Author contact: erlang@plc.inf.elte.hu
%%% --------------------------------------------------------------------------

%% @copyright 2007 Eötvös Loránd University and Ericsson Hungary
%% @author Tamas Nagy <lestat@elte.hu>
%% @author Aniko Vig <viganiko@inf.elte.hu>

%% @doc Gen_server module to handle database connection, queries.
%% @end

    
-module(refactor_db).

-behaviour(gen_server).

%% API
-export([start_link/1,stop/0,insert/1,delete/1,select/1,update/1,
         create_table/1, drop_table/1,commit/0, set_autocommit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

	 

%% @type state(Connection, Type)
%%          Connection = pid() | integer()
%%          Type = atom().
%% A record which stores the inner state of the gen_server
-record(state, {connection, type}).

%%====================================================================
%% API
%%====================================================================

%% =====================================================================
%% @spec start_link(Type::atom()) -> {ok,Pid} | ignore | {error,Error}
%%                                 
%% @doc
%%    Starts the server.
%% Parameter description:<pre>
%% <b>Type</b> : Define connection type. (odbc, mysql) </pre>
%% 
%% @end
%% =====================================================================
start_link(Type) ->
    gen_server:start_link({local, refactor_db}, ?MODULE, [Type], []).


%% =====================================================================
%% @spec stop() -> ok
%%                                 
%% @doc
%%    Stops the server.
%% 
%% @end
%% =====================================================================
stop() ->
    gen_server:cast(refactor_db, stop).

%% =====================================================================
%% @spec commit() -> ok
%%                                 
%% @doc
%%    Commits the changes to the database.
%% 
%% @end
%% =====================================================================
commit() ->
    gen_server:call(refactor_db, {commit, 0}).

%% =====================================================================
%% @spec create_table(Query::string()) -> ok
%%                                 
%% @doc
%%    Sends the create table query to the database.
%% 
%% Parameter description:<pre>
%% <b>Query</b> : Update query string.</pre>
%% @end
%% =====================================================================
create_table(Query) ->
    gen_server:call(refactor_db, {create, Query}).

%% =====================================================================
%% @spec drop_table(Query::string()) -> ok
%%                                 
%% @doc
%%    Sends the drop table query to the database.
%% 
%% Parameter description:<pre>
%% <b>Query</b> : Update query string.</pre>
%% @end
%% =====================================================================
drop_table(Query) ->
    gen_server:call(refactor_db, {drop, Query}).

%% =====================================================================
%% @spec update(Query::string()) -> ok
%%                                 
%% @doc
%%    Sends the update query to the database.
%% 
%% Parameter description:<pre>
%% <b>Query</b> : Update query string.</pre>
%% @end
%% =====================================================================
update(Query) ->
    gen_server:call(refactor_db, {update, Query}).

%% =====================================================================
%% @spec insert(Query::string()) -> ok
%%                                 
%% @doc
%%    Sends the insert query to the database.
%% 
%% Parameter description:<pre>
%% <b>Query</b> : Insert query string.</pre>
%% @end
%% =====================================================================
insert(Query) ->
    gen_server:call(refactor_db, {insert, Query}).

%% =====================================================================
%% @spec delete(Query::string()) -> ok
%%                                 
%% @doc
%%    Sends the delete query to the database.
%% 
%% Parameter description:<pre>
%% <b>Query</b> : Delete query string.</pre>
%% @end
%% =====================================================================
delete(Query) ->
    gen_server:call(refactor_db, {delete, Query}).

%% =====================================================================
%% @spec select(Query::string()) -> ok
%%                                 
%% @doc
%%    Sends the select query to the database.
%% 
%% Parameter description:<pre>
%% <b>Query</b> : Select query string.</pre>
%% @end
%% =====================================================================
select(Query) ->
    gen_server:call(refactor_db, {select, Query}).

%% =====================================================================
%% @spec set_autocommit(Option) -> ok
%%         Option = on | off
%%                                 
%% @doc
%%    Sets the autocommit on or off for the actual session.
%% 
%% Parameter description:<pre>
%% <b>Option</b> : The option atom on or off.</pre>
%% @end
%% =====================================================================
set_autocommit(Option) ->
    gen_server:call(refactor_db, {autocommit, Option}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% =====================================================================
%% @private
%% @spec init(Args::list()) -> {ok, State::state()}
%%                                                         
%% @doc
%% Initiates the server.
%% If the parameter is odbc it starts the odbc application 
%% and creates the connection.
%% If the parameter is mysql it starts the native mysql connection.
%% 
%% Parameter description:<pre>
%% <b>Args</b> : Arguments of the initalization.
%%                (It has one atom element: odbc, or mysql)</pre>
%% @end
%% =====================================================================
init([odbc]) ->
    process_flag(trap_exit, true), % trap signals from supervisor
    case application:start(odbc, permanent) of
        ok -> ok;
        {error, {already_started, odbc}} -> ok;
        {error, Reason} -> throw(Reason)
    end,
    {ok, DBQ} = application:get_env(db_name),
    {ok, UID} = application:get_env(db_user),
    {ok, PWD} = application:get_env(db_password),
    {ok , Con} = 
	odbc:connect("DSN=Erlang;DBQ=" ++ DBQ ++
                     ";UID=" ++ UID ++
		     ";PWD=" ++ PWD, 
		     [{auto_commit,off}]),
    {ok, #state{connection = Con, type = odbc}};
init([mysql]) ->
    process_flag(trap_exit, true), % trap signals from supervisor
    {ok, ID} = application:get_env(db_id),
    {ok, HOST} = application:get_env(db_host),
    {ok, USER} = application:get_env(db_user),
    {ok, PASSWORD} = application:get_env(db_password),
    {ok, DATABASE} = application:get_env(db_name),
    mysql:start_link(ID, HOST, USER, PASSWORD, DATABASE,
                     fun(_A,_B,_C) -> ok end),
    mysql:fetch(ID, "SET AUTOCOMMIT=0;"),
    {ok, #state{connection = ID, type=mysql}}.

%% =====================================================================
%% @private
%% @spec handle_call(Request::{atom(),string()}, 
%%          From::pid(), State::state()) -> 
%%                         {reply, Reply, State::state()}
%%          Reply = ok | [tuple()] 
%%                                 
%% @doc
%% Handles call messages. There are six type of them: 
%% commit, create, drop, update, insert, delete, select.
%% 
%% Parameter description:<pre>
%% <b>Request</b> : 
%%     A tuple which contains the query and type of the query.
%% <b>From</b>:Pid of the process where the call have come from.  
%% <b>State</b>:Current inner state of the gen_server. </pre>
%%
%% @end
%% =====================================================================
handle_call(Message, From, State = #state{type=odbc}) ->
    handle_call_with_odbc_connection(Message, From, State);
handle_call(Message, From, State = #state{type=mysql}) ->
    handle_call_with_mysql_connection(Message, From, State).

%% =====================================================================
%% @private
%% @spec handle_cast(Msg::atom(), State::state()) ->
%%                  {noreply, State::state()} |
%%                 {stop, normal, State::state()}
%%                                 
%% @doc
%%    Handling cast messages. There is currently one valid, 
%% which stops the gen_server.
%% 
%% Parameter description:<pre>
%% <b>Msg</b> : The message identified by an atom.
%% <b>State</b> : Current inner state of the gen_server.</pre>
%%
%% @end
%% =====================================================================
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    error_logger:error_msg("Error occured in refactor_db~n"
			   "Bad cast sent to server.~n"
			   "Message:~p~n", [Msg]),
    {noreply, State}.

%% =====================================================================
%% @private
%% @spec handle_info(Info::term(), State::state()) -> 
%%           {noreply, State::state()}
%%                                 
%% @doc
%%    Handling all non call/cast messages. Currently just a noreply. 
%% 
%% Parameter description:<pre>
%% <b>Info</b> : The message identified by anything.
%% <b>State</b> : Current inner state of the gen_server.</pre>
%%
%% @end
%% =====================================================================
handle_info(_Info, State) ->
    {noreply, State}.

%% =====================================================================
%% @private
%% @spec terminate(Reason::term(), State::state()) -> none()
%%                                 
%% @doc
%%    This function is called by a gen_server when it is about to
%% terminate. It is the opposite of Module:init/1. 
%% The return value is ignored.
%% 
%% Parameter description:<pre>
%% <b>Reason</b> : The reason of the termination. Currently ignored.
%% <b>State</b> : Current inner state of the gen_server.</pre>
%%
%% @end
%% =====================================================================
terminate(_Reason, State= #state{type=odbc}) ->
    odbc:disconnect(State#state.connection),
    case lists:keymember(odbc,1, application:which_applications()) of
	true ->
	    application:stop(odbc);
	false ->
	    ok
    end;
terminate(_Reason, _State = #state{type=mysql}) ->
    ok.


%% =====================================================================
%% @private
%% @spec code_change(OldVsn, State::state(), Extra) -> {ok, NewState::state()}
%%          OldVsn = Vsn | {down, Vsn}
%%             Vsn = term()
%%           Extra = term()
%%                       
%% @doc
%%    Convert process state when code is changed. 
%% Currently doesn't change anything.
%% 
%% Parameter description:<pre>
%% <b>OldVsn</b> : The old version of the code.
%% <b>State</b> : Current inner state of the gen_server.
%% <b>Extra</b> : Extra information required for the state change.</pre>
%%
%% @end
%% =====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% =====================================================================
%% @private
%% @spec handle_call_with_odbc_connection(Request::{atom(),string()}, 
%%                          From::pid(), State::state()) -> 
%%                                  {reply, Reply, State::state()}
%%                          Reply = ok | [tuple()] 
%%                                 
%% @doc
%% Handles call messages, when the current connection is through odbc. 
%% There are seven type of them: 
%% commit, create, drop, update, insert, delete, select, autocommit.
%% 
%% Parameter description:<pre>
%% <b>Request</b> : 
%%     A tuple which contains the query and type of the query.
%% <b>From</b>:Pid of the process where the call have come from.  
%% <b>State</b>:Current inner state of the gen_server. </pre>
%%
%% @end
%% =====================================================================
handle_call_with_odbc_connection({commit, Request}, _From, State) ->
    case odbc:sql_query(State#state.connection, "Commit;") of
	{updated, _} ->
	    {reply, ok, State};
	{_, _, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad commit sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Reply]),
	    {reply, ok, State};
	{error,Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad commit sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Error]),
	    {reply, ok, State}
    end;
handle_call_with_odbc_connection({create, Request}, _From, State) ->
    case odbc:sql_query(State#state.connection, Request) of
	{updated, _} ->
	    {reply, ok, State};
	{_, _, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad create table query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Reply]),
	    {reply, ok, State};
	{error,Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad create table query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Error]),
	    {reply, ok, State}
    end;
handle_call_with_odbc_connection({drop, Request}, _From, State) ->
    case odbc:sql_query(State#state.connection, Request) of
	{updated, _} ->
	    {reply, ok, State};
	{_, _, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad drop table query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Reply]),
	    {reply, ok, State};
	{error,Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad drop table query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Error]),
	    {reply, ok, State}
    end;
handle_call_with_odbc_connection({update, Request}, _From, State) ->
    case odbc:sql_query(State#state.connection, Request) of
	{updated, _} ->
	    {reply, ok, State};
	{_, _, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad update query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Reply]),
	    {reply, ok, State};
	{error,Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad update query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Error]),
	    {reply, ok, State}
    end;
handle_call_with_odbc_connection({insert, Request}, _From, State) ->
    case odbc:sql_query(State#state.connection, Request) of
	{updated, _} ->
	    {reply,ok,  State};
	{_, _, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad insert query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Reply]),
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad insert query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Error]),
	    {reply, ok, State}
    end;
handle_call_with_odbc_connection({delete, Request}, _From, State) ->
    case odbc:sql_query(State#state.connection, Request) of
	{updated, _} ->
	    {reply, ok, State};
	{_, _, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad delete query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Reply]),
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad delete query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Error]),
	    {reply, ok, State}
    end;
handle_call_with_odbc_connection({select, Request}, _From, State) ->
    case odbc:sql_query(State#state.connection, Request) of
	{updated, _} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad select query sent.~n"
				   "Looks more like an update or insert~n"
				   "Query:~p~n", [Request]),
	    {reply, ok, State};
	{_, _, Reply} ->
	    {reply, Reply, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad select query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, Error]),
	    {reply, ok, State}
	    
    end;
handle_call_with_odbc_connection({autocommit, Option}, _From, State) ->
    case odbc:sql_query(State#state.connection, 
                        "SET AUTOCOMMIT=" ++  case Option of
                                                  on -> "1";
                                                  off -> "0"
                                              end  ++ " ;") of
	{updated, _} ->
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad autocommit setting sent.~n"
				   "Result:~p~n", [Error]),
	    {reply, ok, State}
	    
    end.

%% =====================================================================
%% @private
%% @spec handle_call_with_mysql_connection(Request::{atom(),string()}, 
%%                          From::pid(), State::state()) -> 
%%                                  {reply, Reply, State::state()}
%%                          Reply = ok | [tuple()] 
%%                                 
%% @doc
%% Handles call messages, when the current connection is native 
%% mysql connection. 
%% There are seven type of them: 
%% commit, create, drop, update, insert, delete, select, set_autocommit.
%% 
%% Parameter description:<pre>
%% <b>Request</b> : 
%%     A tuple which contains the query and type of the query.
%% <b>From</b>:Pid of the process where the call have come from.  
%% <b>State</b>:Current inner state of the gen_server. </pre>
%%
%% @end
%% =====================================================================
handle_call_with_mysql_connection({commit, Request}, _From, State) ->
    case mysql:fetch(State#state.connection, "Commit;") of
	{updated, _} ->
	    {reply, ok, State};
	{data, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad commit sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, 
						   get_mysql_result(Reply)]),
	    {reply, ok, State};
	{error,Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad commit sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, 
						   get_mysql_error(Error)]),
	    {reply, ok, State}
    end;
handle_call_with_mysql_connection({create, Request}, _From, State) ->
    case mysql:fetch(State#state.connection, Request) of
	{updated, _} ->
	    {reply, ok, State};
	{data, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad create table query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request, 
						   get_mysql_result(Reply)]),
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad create table query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
 						   get_mysql_error(Error)]),
	    {reply, ok, State}
    end;
handle_call_with_mysql_connection({drop, Request}, _From, State) ->
    case mysql:fetch(State#state.connection, Request) of
	{updated, _} ->
	    {reply, ok, State};
	{data, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad drop table query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
						   get_mysql_result(Reply)]),
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad drop table query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
 						   get_mysql_error(Error)]),
	    {reply, ok, State}
    end;
handle_call_with_mysql_connection({update, Request}, _From, State) ->
    case mysql:fetch(State#state.connection, Request) of
	{updated, _} ->
	    {reply, ok, State};
	{data, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad update query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
						   get_mysql_result(Reply)]),
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad update query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
 						   get_mysql_error(Error)]),
	    {reply, ok, State}
    end;
handle_call_with_mysql_connection({insert, Request}, _From, State) ->
    case mysql:fetch(State#state.connection, Request) of
	{updated, _} ->
	    {reply,ok,  State};
	{data, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad insert query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
						   get_mysql_result(Reply)]),
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad insert query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
						   get_mysql_error(Error)]),
	    {reply, ok, State}
    end;
handle_call_with_mysql_connection({delete, Request}, _From, State) ->
    case mysql:fetch(State#state.connection, Request) of
	{updated, _} ->
	    {reply, ok, State};
	{data, Reply} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad delete query sent.~n"
				   "Looks more like a select~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
						   get_mysql_result(Reply)]),
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad delete query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
 						   get_mysql_error(Error)]),
	    {reply, ok, State}
    end;
handle_call_with_mysql_connection({select, Request}, _From, State) ->
    case mysql:fetch(State#state.connection, Request) of
	{updated, _} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad select query sent.~n"
				   "Looks more like an update or insert~n"
				   "Query:~p~n", [Request]),
	    {reply, ok, State};
	{data, Reply} ->
	    {reply, get_mysql_result(Reply), State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad select query sent.~n"
				   "Query:~p~n"
				   "Result:~p~n", [Request,
 						   get_mysql_error(Error)]),
	    {reply, ok, State}
	    
    end;
handle_call_with_mysql_connection({autocommit, Option}, _From, State) ->
    case mysql:fetch(State#state.connection, "SET AUTOCOMMIT=" ++  case Option of
                                                                   on -> "1";
                                                                   off -> "0"
                                                               end  ++ " ;") of
	{updated, _} ->
	    {reply, ok, State};
	{error, Error} ->
	    error_logger:error_msg("Error occured in refactor_db~n"
				   "Bad autocommit setting sent.~n"
				   "Result:~p~n", [get_mysql_error(Error)]),
	    {reply, ok, State}
	    
    end.



%% =====================================================================
%% @spec get_mysql_result(Data::term()) -> 
%%                                [tuple()] 
%%                                 
%% @doc
%% Extracts the select's result from the returned data and converts 
%% into the same as the odbc connection's return structure.
%% 
%% Parameter description:<pre>
%% <b>Data</b> : The native Mysql conncetions result structure. </pre>
%%
%% @end
%% =====================================================================
get_mysql_result(Data) ->
    ConvList = get_conversion_list(mysql:get_result_field_info(Data)),
    lists:map(
      fun(Row) -> 
              list_to_tuple(
                convert_from_string(Row,ConvList)) end, 
      mysql:get_result_rows(Data)).

%% =====================================================================
%% @spec get_mysql_error(Data::term()) -> 
%%                                string()
%%                                 
%% @doc
%% Extracts the error's reason from the returned data.
%% 
%% Parameter description:<pre>
%% <b>Data</b> : The native Mysql conncetions result structure. </pre>
%%
%% @end
%% =====================================================================
get_mysql_error(Error) ->
    mysql:get_result_reason(Error).

%% =====================================================================
%% @spec get_conversion_list(FieldInfo::[{tuple()}]) -> 
%%                                [{integer(), function()}]
%%                                 
%% @doc
%% Gets the elements which has to be converted from string 
%% to another type.
%% 
%% Parameter description:<pre>
%% <b>FieldInfo</b> : The Field info of the result query. </pre>
%%
%% @end
%% =====================================================================
get_conversion_list(FieldInfo) ->
    get_conversion_list(FieldInfo, [], 1).

%% =====================================================================
%% @spec get_conversion_list(FieldInfo::[tuple()], 
%%                    ConvList::[integer()], Pos::integer()) -> 
%%                                [{integer(),function()}]
%%                                 
%% @doc
%% Gets the elements which has to be converted from string 
%% to another type.
%% 
%% Parameter description:<pre>
%% <b>FieldInfo</b> : The field info of the result query.
%% <b>ConvList</b> : Prefix of the result list.
%% <b>Pos</b> : The position of the processed field info. </pre>
%%
%% @end
%% =====================================================================
get_conversion_list([],ConvList, _Pos) ->
    lists:reverse(ConvList);
get_conversion_list([{_Table,_Field,_Length,'LONG'} | Xs], ConvList, Pos) ->
    get_conversion_list(Xs, [{Pos, fun list_to_integer/1} | ConvList], Pos+1);
get_conversion_list([{_Table,_Field,_Length,'FLOAT'} | Xs], ConvList, Pos) ->
    get_conversion_list(Xs, [{Pos, fun (Value) ->
                                           case lists:member($., Value) of
                                              true -> list_to_float(Value);
                                              false -> 
                                                case lists:member($e, Value) of
						   true -> list_to_float(add_dotzero(Value));
						   false -> list_to_float(Value++".0")
                                                end
                                           end
                                   end} | ConvList], Pos+1);    
get_conversion_list([{_Table,_Field,_Length,_Name} | Xs], ConvList, Pos) ->
    get_conversion_list(Xs, ConvList, Pos+1).
    
%% =====================================================================
%% @spec add_dotzero(Value::string()) -> string() 
%%                                
%%                                 
%% @doc
%% Adds the needed .0 in the following string 1e+6 to convert it a real
%%    float format: 1.0e+6.
%% 
%% Parameter description:<pre>
%% <b>Value</b> : The wrong float format.
%%
%% @end
%% Used in: get_conversion_list/3
%% =====================================================================    
add_dotzero(Value) ->
   lists:flatten(lists:map(fun (Element) -> case Element of
                                 $e -> ".0e";
                                 _ -> Element
                               end end,Value)).
    
%% =====================================================================
%% @spec convert_from_string(Row::[string()], 
%%                    ConvList::[{integer(), function()}]) -> 
%%                                [Result]
%%           Result = integer() | string()
%%                                 
%% @doc
%% Converts the requested elements of the Row from string 
%% to another type. The postion of these elements are in the ConvList.
%% 
%% Parameter description:<pre>
%% <b>Row</b> : One Row of the result.
%% <b>ConvList</b> : Position, and converter function list. </pre>
%%
%% @end
%% =====================================================================
convert_from_string(Row, ConvList) ->
    convert_from_string(Row, 1, ConvList).

%% =====================================================================
%% @spec convert_from_string(Row::[string()], Pos::integer(), 
%%                    ConvList::[{integer(), function()}]) -> 
%%                                [Result]
%%           Result = integer() | string()
%%                                 
%% @doc
%% Converts the requested elements of the Row from string 
%% to another type. The postion of these elements are in the ConvList.
%% 
%% Parameter description:<pre>
%% <b>Row</b> : One Row of the result.
%% <b>Pos</b> : The position of the processed row element.
%% <b>ConvList</b> : Position, and converter function list. </pre>
%%
%% @end
%% =====================================================================
convert_from_string([null], 1, _ConvList) ->
    [null];
convert_from_string(Row, _Pos, []) ->
    Row;
convert_from_string([Field | Xs], Pos, [{Pos,Converter} | Ys]) ->
    [Converter(Field) | convert_from_string(Xs, Pos+1, Ys)];
convert_from_string([Field | Xs], Pos, ConvList) ->
    [Field | convert_from_string(Xs, Pos+1, ConvList)].
