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

%%% @author Judit Koszegi <kojqaai@inf.elte.hu>

-module(web_helper).
-vsn("$Rev: 5507 $ ").

-export([start_yaws/0, start_yaws/1, stop_yaws/0]).
-export([result_to_ehtml/1, get_files/0, query_list_from_tab/1, get_value/2]).
-export([init_tab/0, close_tab/0, delete_from_tab/2]).
-export([decode_quotes/1]).

-include("ui.hrl").

-define(TAB, query_table).

%%% ============================================================================
%%% Start, configure and stop yaws web server

%% @doc Start yaws web server with default configuration.
start_yaws() ->
    start_yaws([]).

%% @doc Configure and start yaws web server.
start_yaws(["from_script", YPath, YName, YPort, YListen]) ->
    YawsPathProp = {yaws_path,YPath},
    YawsNameProp = {yaws_name,YName},
    YawsPortProp = {yaws_port, convert_list_to_integer(YPort)},
    YawsListenProp = {yaws_listen, string_to_ip(YListen)},
    AllProp = [YawsNameProp,YawsPortProp,YawsListenProp], 
    case YPath of
        "no_path" -> start_yaws(AllProp);
        _ -> start_yaws([YawsPathProp|AllProp])
    end;

start_yaws(Proplist) ->
    case (proplists:get_value(yaws_path,Proplist)) of
        undefined -> ok;
        YawsDir ->
            case code:add_path(YawsDir) of
                true -> ok; 
                {error,_} ->
                    throw(?RefError(file_notdir,[YawsDir]))
            end
    end,
    Port = proplists:get_value(yaws_port,Proplist,8001),
    Listen = proplists:get_value(yaws_listen,Proplist,{0,0,0,0}),  
    Name = proplists:get_value(yaws_name,Proplist,"refactorErl"),
    yaws:start_embedded(
      get_index_yaws_folder(),
      [{port,Port},{servername,Name},{listen, Listen}]).

%% @doc Stop yaws web server.
stop_yaws() ->
    application:stop(yaws).

get_index_yaws_folder() ->
    Path0 = filename:split(filename:dirname(code:which(web_helper))),
    Path =
        filename:join([
                       filename:join(
                         lists:sublist(Path0,length(Path0)-1)),
                      "web"]),
    Path.

convert_list_to_integer(String) ->
    try
        list_to_integer(String)
    catch _A:_B ->
            throw(?RefError(list_to_integer_error,[String]))
    end.

take_and_drop(L) -> 
    {convert_list_to_integer(lists:takewhile(fun(X) -> not(X == hd(".")) end,L)),
     (lists:dropwhile(fun(X) -> not(X == hd(".")) end,L))}.

string_to_ip(YPort) -> 
    L1 = YPort,
    {Number1,L2} = take_and_drop(L1),
    case L2 of
        [] ->  throw(?RefError(ip_format_error,[YPort]));
        _ ->
            {Number2,L3} = take_and_drop(tl(L2)),
            case L3 of
                [] -> throw(?RefError(ip_format_error,[YPort]));
                _ ->  {Number3,L4} = take_and_drop(tl(L3)),
                      case L4 of 
                          [] -> throw(?RefError(ip_format_error,[YPort]));
                          _ ->  {Number4,_L5} = take_and_drop(tl(L4)),
                                {Number1,Number2,Number3,Number4}
                      end
            end
    end.

%%% ============================================================================
%%% Calculate the result of a query and transform it to yaws' html format

calculate_result(Q) ->
    ReqID = ?UI:getid(),
    Req = {transform,semantic_query, 
           [{ask_missing,false},
            {querystr,Q},
            {display_opt,[{positions,scalar}, {output,other}]},
            {start_opt,[]}]},
    ok = ?UI:request(ReqID,Req),
    {ok, Result} = 
        receive
            {ReqID, reply, R} ->
                R;
            M ->
                {ok, {error, M}}
        end,
    Result.

%% @doc Returns with the result of the given query in ehtm format
result_to_ehtml({_,[],_}) ->
    result_to_ehtml(noquery);
result_to_ehtml({'query',Q,U}) ->
    case calculate_result(Q) of
        {result, Result} ->
            [R0] = ?MISC:pgetu([result],Result),
            R = io_lib:format("~p",[R0]),
            [_Space|User] = U,
            Hash = get_database_hash(),
            Users = case find_in_tab(Q) of
                        [] -> [User];
                        [{_Q,_R,UL,_H}] -> 
                            case lists:member(User,UL) of
                                true -> UL;
                                false -> [User|UL]
                            end
                    end,
            insert_to_tab(Q,R,Users,Hash),
            add_possible_warning(ehtml_from_query_posres(R));
        E ->
            error_handler(E)
    end;
result_to_ehtml({prev_query,Q,U}) ->
    init_tab(),
    [{_Q,_R,_U, H}] = find_in_tab(Q),
    case is_database_changed(H) of
        true ->
            result_to_ehtml({'query',Q,U});
        false ->
             [{_Q,R,_U, _H}] = find_in_tab(Q),
            add_possible_warning(ehtml_from_query_posres(R))
    end;
result_to_ehtml(noquery) ->
    {table, [],
     [{tr, [],
       [
        {td, [], "Please type a query"}
       ]
      }]};
result_to_ehtml(_) ->
    result_to_ehtml(noquery).

add_possible_warning(Result) ->
    case is_error_from_in_database() of
        true -> [{'div',[],error_handler(warning)},
                 {'div',[{style,"float:left"}],Result}];
        false -> {'div',[{style,"float:left"}],Result}
    end.

error_handler(warning) ->
    Warning = "Warning: the database contains file(s) with error(s).",
    {p,[{id,"errorW"}],{font,[{color,"red"},{size,"3"}], Warning}};
error_handler(E) ->
    Message = case E of
                  {abort, M} -> reflib_error:error_text(M);
                  {error, M} -> "fatal: " ++ io_lib:format("~p",[M])
              end,
    {p,[{id,"errorP"}],{font,[{color,"red"},{size,"4"}], Message}}.

%%% ----------------------------------------------------------------------------
%%% Helper functions (transform query result to ehtml format)

ehtml_from_query_posres(Table) ->
    TableElements = to_table(list_to_term(lists:flatten(Table))),
    case TableElements of
        [] -> 
            {table, [],
             [{tr, [],
               [
                {td, [], "No result"}
               ]
              }]};
        _ ->
            {table, [], TableElements}
    end.

to_table([]) -> [];
to_table([{list,L}]) ->
    case L of 
        [] ->  [{tr, [], [{td, [], "No result"}]}];
        _ -> [{tr, [], [{td, [], to_html({list,L})}]}]
    end;
to_table([{group_by,Row1},Row2|Tail]) ->
    [{tr, [], [{td, [], to_html(Row1)},
               {td, [], to_html(Row2)}]}
     |to_table(Tail)];
to_table([{chain,L,End}|Tail]) ->
    [{tr, [], [{td, [], [to_html({chainList,L}),to_html({chain_end,End})]}]}
     | to_table(Tail)];
to_table(R) ->
    [{tr, [], [{td, [], R}]}].

to_html({list,[]}) -> [];
to_html({list,[H]}) -> to_html({H,{height,"0.7"}});
to_html({list,[H|T]}) -> [to_html({H,{height,"0.7"}})|to_html({list,T})];

to_html({chainList,[]}) -> [];
to_html({chainList,[H]}) -> to_html({H,{comma,no}});
to_html({chainList,[H|T]}) -> [to_html({H,{comma,yes}})|to_html({chainList,T})];

to_html({chain_end,"\n"}) -> [];
to_html({chain_end,"*\n"}) -> "*";

to_html({X,{comma,IsComma}}) ->
    Separator = case IsComma of
                    yes -> ", ";
                    no -> ""
                end,
    case get_pos(X) of
        nopos ->  
            {span,[],get_text(X) ++ Separator};
        {File,StartPos,EndPos} -> 
            [{a,[{href,"javascript:"},{onclick,"showSelection('"  ++ 
                    File ++ "'," ++ 
                    io_lib:format("~p,",[StartPos]) ++ 
                    io_lib:format("~p)",[EndPos])}],
             get_text(X)},{span,[],Separator}]
    end;
to_html({X,{height,Line_h}}) ->
    case get_pos(X) of
        nopos ->
            {p,[{style,"line-height:"++ Line_h}],{span,[],get_text(X)}};
        {File,StartPos,EndPos} ->
            {p,[{style,"line-height:" ++ Line_h}],
             {a,[{href,"javascript:"},{onclick,"showSelection('"  ++
                           File ++ "'," ++
                           io_lib:format("~p,",[StartPos]) ++
                           io_lib:format("~p)",[EndPos])}],
                    get_text(X)}}
    end;
to_html(X) -> to_html({X,{height,"1.2"}}).

get_text({group_by,{_Pos,Text}}) -> Text;
get_text({_Pos,Text}) -> Text;
get_text({eq,Text1,Text2}) -> io_lib:format("~p",[Text1]) ++ " = "
                                  ++ io_lib:format("~p",[Text2]);
get_text(_) -> "notext".

get_pos({group_by,{{File,StartPos,EndPos},_Text}}) ->
    get_pos({{File,StartPos,EndPos},_Text});
get_pos({{File,StartPos,EndPos},_Text}) ->
    {File, StartPos, EndPos};
get_pos(_) -> nopos.

list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case erl_parse:parse_term(T) of
        {ok, Term} ->
            Term;
        {error, Error} ->
            Error
    end.

%%% ============================================================================
%%% Operations on 'query_table' dets table

%% @doc Initialize/open query_table
init_tab() ->
    dets:open_file(?TAB,[]).

%% @doc Close query_table
close_tab() ->
    dets:close(?TAB).

insert_to_tab(Query, Result, User, Hash) ->
    dets:insert(?TAB,{Query,Result,User,Hash}).

find_in_tab(Query) ->
    dets:lookup(?TAB,Query).

delete_from_tab(Query) ->
    dets:delete(?TAB,Query).

%% @doc Delete the given query from the given user in the query_table
delete_from_tab(Query,Usr) ->
    [{Query,Result,UserList,Hash}] = dets:lookup(?TAB,Query),
    UserList2 = lists:delete(Usr,UserList),
    case UserList2 of
        [] -> 
            delete_from_tab(Query);
        _ ->
            insert_to_tab(Query,Result,UserList2,Hash)
    end.

%%% ============================================================================
%%% Make html list from the query table

%% @doc Returns with a html list generated from the query table
query_list_from_tab(Usr) ->
    L = dets:match_object(?TAB,{'_','_','_','_'}),
    L2 = case Usr of
             "all" ->
                 L;
             _ ->
                 lists:filter(fun({_Q,_R,Users,_H}) -> 
                                      lists:member(Usr,Users)
                              end,
                              L)    
         end,
    Lis = 
        lists:map(fun ({Q,_R,_U,_H})->
                          CodedQuotedQ = code_quotes(Q),
                          QueryA = 
                              {a,
                               [{onclick,"selectQuery('" ++  
                                 CodedQuotedQ ++ "')"},
                                {href,"javascript:"},
                                {class,"queryA"},
                                {title,"show result"}],
                               {span,[],Q}},
                          case Usr of
                              "all" ->
                                  {li,[{id, CodedQuotedQ}],QueryA};
                              _ ->
                                  {li,[{id, CodedQuotedQ}],
                                   [{a,[{href,"javascript:"},
                                        {class,"delete"},
                                        {title,"delete"},
                                        {onclick,"deleteQuery('" ++  
                                         CodedQuotedQ ++ "')"}],
                                     {span,[],"X"}},
                                    QueryA]}
                          end   
                  end,
                  L2),
    {ehtml,{ul,[{id,"queryList"},{class,"queryList"}],Lis}}.

code_quotes(Str) ->
    Str2 = re:replace(Str,"\"","``",[{return,list},global]),
    re:replace(Str2,"'","`",[{return,list},global]).
  
%% @private
decode_quotes(Str) ->
    Str2 = re:replace(Str,"``","\"",[{return,list},global]),
    re:replace(Str2,"`","'",[{return,list},global]).

%%% ============================================================================
%%% Helper functions

is_database_changed(Hash1) ->
    Hash1 /= get_database_hash().

get_database_hash() ->
    HashList = lists:filter(fun(X) -> X /= virtual end,
                            [(refcore_graph:data(Node))#form.hash
                             || Node <- refcore_graph:path(
                                          refcore_graph:root(), [file, form])]),
    erlang:phash2(HashList).

%%% ----------------------------------------------------------------------------

is_error_from_in_database() ->
    case ?Query:exec(?Query:seq([file],?File:error_forms())) of
        [] -> false;
        _ -> true
    end.
    
%%% ----------------------------------------------------------------------------

%% @private
get_value(Key,L) ->
    case Key of
        "u" -> proplists:get_value(Key,L,no_user);
        "user" -> proplists:get_value(Key,L,no_user);
        _ ->  proplists:get_value(Key,L,noquery)
    end.

%%% ----------------------------------------------------------------------------

%% @doc Returns with a html option list generated from the files that
%% have been loaded into the database
get_files() ->
    Files = lists:map(fun reflib_file:path/1, reflib_query:exec([file])),
    ShortFiles = lists:map(
                   fun(X) ->
                           case length(X)>30 of
                               false ->
                                   {X,X};
                               true ->
                                   {X,("..." ++ lists:nthtail((length(X)-33),X))}
                           end
                   end, Files),
    lists:map(fun({X,Y}) -> {option,[{value,X}],Y} end, ShortFiles).
