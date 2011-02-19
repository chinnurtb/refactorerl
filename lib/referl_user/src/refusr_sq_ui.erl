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

%%% @author Lilla Hajós <lya@elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(refusr_sq_ui).
-vsn("$Rev: 5016 $ ").

-export([poscalc/2, show/2, display/2]).

-include("user.hrl").

-define(Lib, refusr_sq_lib).

%% The record `state' stores the data of a single step in the semantic query.
%% `action' is the type of the current action. The action can be:
%%   initial_selection
%%   selection
%%   closure
%%   iteration
%%   property_query
%%   statistics
%% `type', `res', `prev_type' and `prev_res' contain data for the current and
%% the previous result respectively.
-record(state, {action=selection, type, res, prev_type, prev_res=[]}).

-record(chains, {max_length=0, complete=[], incomplete=[], recursive=[]}).

-record(init,{type,nodes}). % nodes::[{Pos,Node}]
-record(iter,{type,part,rec,comp}). % part::[[{Pos,Node}]]
-record(sel, {prevtype,type,branches}). % branches::[{PrevNode,[{Pos,Node}]}]
-record(prop,{prevtype,property,values}). % values::[{{Pos,PrevNode},Value}]
-record(stat,{statistic,value}).


%%% ============================================================================
%%% Show

%% @type linecol() = {natural(), natural()}

%% @type fpos() = nopos | {filename(), Pos1, Pos1}
%%        Pos1 = 1 | natural() | linecol()

%% @type postype() = none|scalar|linecol

%% @type posres() = #init{} | #iter{} | #sel{} | #prop{} | #stat{}

%% @spec (#state{},postype()) -> posres()
%% @doc Calculates positions for a semantic query result.
poscalc(#state{action=initial_selection, type=Type, res=Res}, ShowPos) ->
    Nodes = lists:flatten(Res),
    Fetch = positions(Type, Nodes, ShowPos),
    PosNodes = Fetch(Nodes),
    #init{type=Type, nodes=PosNodes};

poscalc(#state{action=iteration, type=Type,
               res=#chains{incomplete=Part0, recursive=Rec0, complete=Comp0}},
        ShowPos) ->
    Chains = [Part0,Rec0,Comp0],
    Nodes = ?MISC:flatsort(Chains),
    Fetch = positions(Type, Nodes, ShowPos),
    [Part,Rec,Comp] = [[Fetch(lists:reverse(NodesRev)) ||
                           NodesRev <- Chain ] ||
                          Chain <- Chains],
    #iter{type=Type, part=Part, rec=Rec, comp=Comp};

poscalc(#state{action=closure}=State, ShowPos) ->
    poscalc(State#state{action=iteration}, ShowPos);

poscalc(#state{action=selection, type=Type, res=Res,
               prev_type=PrevType, prev_res=PrevRes}, ShowPos) ->
    Fetch = positions(Type, lists:flatten(Res), ShowPos),
    List = lists:filter( fun({_, Ns}) -> Ns /= [] end,
                         lists:zip(PrevRes, Res) ),
    Branches = [{PrevNode, Fetch(Nodes)} || {PrevNode, Nodes} <- List],
    #sel{prevtype=PrevType, type=Type, branches=Branches};

poscalc(#state{action=property_query, type=Type, res=Values,
               prev_type=PrevType, prev_res=PrevRes}, ShowPos) ->
    Fetch = positions(PrevType, PrevRes, ShowPos),
    Keys = Fetch(PrevRes),
    KV = lists:zip(Keys, Values),
    #prop{prevtype=PrevType, property=Type, values=KV};

poscalc(#state{action=statistics, type=Type, res=Res}, _) ->
    #stat{statistic=Type, value=Res}.

%% -----------------------------------------------------------------------------

%% @spec (node_type(),[#node{}],postype()) ->
%%        fun(([#node{}]) -> [{fpos(), #node{}}])
%% @doc Returns a node position calculator mapping fun.
positions(_Type, _List, none) ->
    fun(Nodes) ->
            [{0,Node} || Node <- Nodes] % @todo why 1?
    end;
positions(Type, List, ShowPos) when ShowPos==scalar orelse ShowPos==linecol ->
    Dict = positions0(Type, List, ShowPos),
    fun(Nodes) ->
            [{dict:fetch(Node, Dict), Node} || Node <- Nodes]
    end.

positions0(file, Nodes, _ShowPos) ->
    dict:from_list(
      [ begin
            Pos = case ?Graph:class(File) of
                      file ->
                          {?File:path(File), 1, 1};
                      module ->
                          case ?Query:exec(File, ?Mod:file()) of
                              [] -> nopos;
                              [Node] -> {?File:path(Node), 1, 1}
                          end
                  end,
            {File, Pos}
        end || File <- Nodes ]);

positions0(Type, Nodes, ShowPos) ->
    NodesWithTokens = [ {Node, file_and_tokens(Type, Node)} || Node <- Nodes ],
    CollTokens =
        dict:to_list(
          lists:foldl(
            fun({_Node, []}, Dict) ->
                    Dict;
               ({_Node, [File, First, Last]}, Dict) ->
                    D1 = case dict:is_key(File, Dict) of
                             false -> dict:store(File, [First], Dict);
                             true -> dict:append(File, First, Dict)
                         end,
                    dict:append(File, Last, D1)
            end,
            dict:new(),
            lists:keysort(2, NodesWithTokens))),
    TokenPos0 = [?Token:map_pos(File, Tokens, ShowPos) ||
                    {File, Tokens} <- CollTokens],
    TokenPos = dict:from_list(lists:flatten(TokenPos0)),
    lists:foldl(
      fun({Node, []}, Dict) ->
              dict:store(Node, nopos, Dict);
         ({Node, [File, First, Last]}, Dict) ->
              {Pos1, _} = dict:fetch(First, TokenPos),
              {_, Pos2} = dict:fetch(Last, TokenPos),
              dict:store(Node, {?File:path(File), Pos1, Pos2}, Dict)
      end,
      dict:new(),
      NodesWithTokens).


file_and_tokens(function, Node) ->
   ?Query:exec(Node, ?Query:seq(?Fun:definition(),
                                ?Query:all([ ?Form:file(),
                                             ?Syn:first_leaf(),
                                             ?Syn:last_leaf() ])));

file_and_tokens(record, Node) ->
    ?Query:exec(Node, ?Query:seq(?Rec:form(),
                                 ?Query:all([ ?Form:file(),
                                              ?Syn:first_leaf(),
                                              ?Syn:last_leaf() ])));

file_and_tokens(field, Node) ->
    Rec = ?Query:exec(Node, ?RecField:recorddef()),
    file_and_tokens(record, Rec);

file_and_tokens(macro, Node) ->
   ?Query:exec(Node, ?Query:all([ ?Form:file(),
                                  ?Syn:first_leaf(),
                                  ?Syn:last_leaf() ]));

file_and_tokens(expression, Node) ->
    [File] = ?Syn:get_file(Node),
    [First] = ?Query:exec(Node, ?Syn:first_leaf()),
    [Last] = ?Query:exec(Node, ?Syn:last_leaf()),
    [File, First, Last];

file_and_tokens(variable, Node) ->
    case ?Query:exec(Node, ?Var:bindings()) of
        [Bind | _] -> file_and_tokens(expression, Bind);
        [] -> []
    end.

%% -----------------------------------------------------------------------------

%% @doc Shows a positionally annotated query result in a more textually explicit
%%      form.
%% @spec (posres(),LineNum::boolean()) -> shownres()
show(PosRes, LineNum)->
    lists:flatten(show_(PosRes, LineNum)).

show_(#stat{statistic=S, value=V}, _) ->
    [{nopos, io_lib:format("~p = ~p~n", [S, V])}];

show_(#init{type=T, nodes=N}, _) ->
    capstr(texts(T,N),"\n");

show_(#prop{prevtype=PrevType, property=Property, values=PosNodes}, _) ->
    [[{Pos,text(PrevType, PrevNode)},
      {nopos,io_lib:format(":~n    ~p = ~p~n", [Property, Value])}] ||
        {{Pos,PrevNode},Value} <- PosNodes];

show_(#sel{prevtype=PrevType, type=Type, branches=B}, true) ->
    [[{nopos, [" ", text(PrevType,PrevNode), ":\n"]},
      capstr(texts(Type,Nodes),"\n")] ||
        {PrevNode,Nodes} <- B];

show_(#sel{prevtype=PrevType, type=Type, branches=B}, false)->
    [[{nopos, [text(PrevType,PrevNode), ":\n"]},
      [[{nopos, "    "}, {Pos,text(Type, Node)}, {nopos, "\n"}] ||
          {Pos,Node} <- PosNodes]] ||
        {PrevNode,PosNodes} <- B];

show_(#iter{type=Type, part=Part, rec=Rec, comp=Comp}, true) ->
    Do = fun ([],_) ->
                 [];
             (Chain,Str)->
                 ChainT = [capstr(texts(Type,Nodes),"\n") || Nodes <- Chain],
                 PosText = ?MISC:join(ChainT,{nopos,"\n"}),
                 [{nopos,[" ",Str,":\n"]},PosText]
         end,
    Do(Part,"incomplete") ++ Do(Rec,"recursive") ++ Do(Comp,"complete");

show_(#iter{type=Type, part=Part, rec=Rec, comp=Comp}, false)->
    Do = fun(Chain,Str)->
                 [[?MISC:join(texts(Type,Nodes), {nopos, " "}),
                             {nopos, [Str,"\n"]}] || Nodes <- Chain ]
         end,
    Do(Part," ...") ++ Do(Rec," *") ++ Do(Comp,"").

capstr(List,Str)->
    cap(List,{nopos, Str}).

cap(List,Lid)->
    lists:append([[Jar,Lid] || Jar <- List]).

%% @doc Outputs a `show'n query result via standard output, a file or
%% a UI message.
%% @spec (shownres(), Display::{stdio | {iodev,Dev} | msg, postype()}) -> any()
display(List, {stdio, LineNum}) ->
    io:put_chars(lined(List,LineNum));
display(List, {{iodev,Dev}, LineNum}) ->
    io:put_chars(Dev, lined(List,LineNum));
display(List, {msg, LineNum}) ->
    io:put_chars(lined(List,LineNum)), % debug
    ?UI:message(queryres, [{Pos, lists:flatten(Text)} || {Pos, Text} <- List]).

lined(List,true)->
    [case Pos of
         X when X==nopos orelse X==1 ->
             Text;
         {_File,1,1} ->
             Text;
         {File,{Line1,_Col1},{_Line2,_Col2}} ->
             File ++ ":" ++ io_lib:print(Line1) ++ ": " ++ Text
     end || {Pos, Text} <- List];
lined(List,false) ->
    [Text || {_, Text} <- List].

texts(Type, PosNodes)->
    [{Pos,text(Type, Node)} || {Pos,Node} <- PosNodes].

text(file, File) ->
    case ?Graph:class(File) of
        file ->
            filename:basename(?File:path(File));
        module ->
            case ?Query:exec(File, ?Mod:file()) of
                [] ->
                    io_lib:write_atom(?Mod:name(File));
                [Node] ->
                    filename:basename(?File:path(Node))
            end
    end;
text(function, Fun) ->
    [Mod] = ?Query:exec(Fun, ?Fun:module()),
    ?MISC:fun_text([?Mod:name(Mod), ?Fun:name(Fun), ?Fun:arity(Fun)]);
text(record, Rec) ->
    io_lib:write_atom(?Rec:name(Rec));
text(macro, Mac) ->
    ?Macro:name(Mac);
text(variable, Var) ->
    ?Var:name(Var);
text(field, Field) ->
    io_lib:write_atom(?RecField:name(Field));
text(expression, Expr) -> %@todo strip comments
    Text = string:strip(lists:flatten(?Syn:tree_text(Expr))),
    string:strip(Text, both, $\n).
