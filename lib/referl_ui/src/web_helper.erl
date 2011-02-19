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
-vsn("$Rev: 5220 $ ").

-export([result_to_ehtml/1, get_files/0, tab_to_js_create_list/1, get_value/2]).
-export([init_tab/0, close_tab/0, delete_from_tab/1]).

-define(TAB, query_table).
-record(form, {type, tag, paren=default, pp=none, hash}).

get_value(Key,L) ->
    case (lists:keysearch(Key,1,L)) of
        {value, {_Key, Value}} ->
            Value;
        false ->
            case Key of
                "u" -> no_user;
                "user" -> no_user;
                _ -> noquery
            end
    end.

calculate_result(Q) ->
    try
        io_lib:format("~p",[refusr_sq:run([{positions,scalar},{output,other}],[],Q)])
    catch
        E={M,_,_} when is_atom(M) ->
            {error,reflib_error:error_text(E)}
    end.

result_to_ehtml({_,[],_}) ->
    result_to_ehtml(noquery);
result_to_ehtml({'query',Q,U}) ->
    case calculate_result(Q) of
        {error,Err} -> error_handle(Err);
        R ->
            [_Space|User] = U,
            Hash = get_database_hash(),
            insert_to_tab(Q,R,User,Hash),
            [ehtml_from_query_posres(R)]
    end;
result_to_ehtml({prev_query,Q,U}) ->
    case is_database_changed(hash_for_query(Q)) of
        true ->
            case calculate_result(Q) of
                {error,Err} -> error_handle(Err);
                R -> [_Space|User] = U,
                     Hash = get_database_hash(),
                     insert_to_tab(Q,R,User,Hash),
                     ehtml_from_query_posres(R)
            end;
        false ->
            R = find_in_tab(Q),
            ehtml_from_query_posres(R)
    end;
result_to_ehtml(noquery) ->
    {table, [],
     [{tr, [],
       [
        {td, [], "Please type a query"}
       ]
      }]}.

error_handle(Err) ->
    {script,[],"alert(\"" ++ Err ++ "\");"}.

%% -----------------------------------------------------------------------------

ehtml_from_query_posres(Table) ->
    {table, [], to_table(list_to_term(lists:flatten(Table)))}.

to_table([]) -> [];
to_table([{list,L}]) ->
    [{tr, [], [{td, [], to_html({list,L})}]}];
to_table([{group_by,Row1},Row2|Tail]) ->
    [{tr, [], [{td, [], to_html(Row1)},
               {td, [], to_html(Row2)}]}
     |to_table(Tail)];
to_table([{chain,L,End}|Tail]) ->
    [{tr, [], [{td, [], [to_html({list,L}),to_html({chain_end,End})]}]}
     | to_table(Tail)];
to_table(R) ->
    [{tr, [], [{td, [], R}]}].


to_html({list,[]}) -> [];
to_html({list,[H]}) -> to_html({no_comma,H});
to_html({list,[H|T]}) -> [to_html({with_comma,H})|to_html({list,T})];

to_html({chain_end,"\n"}) -> [];
to_html({chain_end,"*\n"}) -> "*";

to_html({no_comma,X}) ->
    case get_pos(X) of
        nopos ->
            {span,[],get_text(X)};
        {File,StartPos,EndPos} ->
            {u,[],{span,[{onclick,  "showSelection('"  ++
                          File ++ "'," ++
                          io_lib:format("~p,",[StartPos]) ++
                          io_lib:format("~p)",[EndPos])}],
                   get_text(X)}}
    end;
to_html({with_comma,X}) ->
    case get_pos(X) of
        nopos ->
            {span,[],get_text(X) ++ ", "};
        {File,StartPos,EndPos} ->
            [{u,[],{span,[{onclick,  "showSelection('"  ++
                           File ++ "'," ++
                           io_lib:format("~p,",[StartPos]) ++
                           io_lib:format("~p)",[EndPos])}],
                    get_text(X)}},{span,[],", "}]

    end;
to_html(X) -> to_html({no_comma,X}).

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

%% -----------------------------------------------------------------------------

init_tab() ->
    dets:open_file(?TAB,[]).

close_tab() ->
    dets:close(?TAB).

insert_to_tab(Query, Result, User, Hash) ->
    dets:insert(?TAB,{Query,Result,User,Hash}).

find_in_tab(Query) ->
    [{_Q,R,_U, _H}] = dets:lookup(?TAB,Query),
    R.

hash_for_query(Query) ->
    [{_Q,_R,_U, H}] = dets:lookup(?TAB,Query),
    H.

delete_from_tab(Query) ->
    dets:delete(?TAB,Query).

%% -----------------------------------------------------------------------------

tab_to_js_create_list(Usr) ->
    L =
        case Usr of
	    all ->
                dets:match_object(?TAB,{'_','_','_','_'});
	    _ ->
                dets:match_object(?TAB,{'_','_',Usr,'_'})
	end,
    Html_text = lists:concat(lists:map(fun to_li_tag/1,L)),
    {html, io_lib:format("~s",[Html_text])}.

standard_text() ->
    "li = document.createElement('li'); " ++
        "nobr = document.createElement('nobr'); " ++
        "li.appendChild(nobr); ".

to_li_tag({Q,_Res,_Usr,_Hash}) ->
    standard_text() ++
        "nobr.innerHTML = \"" ++ Q ++ "\"; " ++
        "li.id = \"" ++ Q ++ "\"; " ++
        "container.appendChild(li); ".

%% -----------------------------------------------------------------------------

is_database_changed(Hash1) ->
    Hash1 /= get_database_hash().

get_database_hash() ->
    HashList = lists:filter(fun(X) -> X /= virtual end,
                            [(refcore_graph:data(Node))#form.hash
                             || Node <- refcore_graph:path(
                                          refcore_graph:root(), [file, form])]),
    erlang:phash2(HashList).

%% -----------------------------------------------------------------------------

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
