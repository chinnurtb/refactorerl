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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% == Parameters ==
%%% <ul>
%%% <li>A tuple (see {@link reflib_args:expr_range/1}).</li>
%%% <li>A record name (see {@link reflib_args:name/1}).</li>
%%% <li>Record filed names separated by whitespace
%%%   (see {@link reflib_args:string/1}).</li>
%%% </ul>

%%% == Conditions of applicability ==
%%% <ul>
%%% <li> The name of the record we introduce should not conflict with
%%%   another record. Furthermore,
%%%   the name and field names should be a legal record name.</li>
%%% <li> The starting and ending positions should delimit a tuple
%%%   skeleton.</li>
%%% <li> The transformed tuple cannot be embedded in a list
%%%   comprehension, list or another tuple.</li>
%%% <li> The given field names number should match the number of the
%%%   tuple's elements.</li>
%%% <li> The selected tuple cannot be a parameter of a function
%%%   expression.</li>
%%% <li> If the selected tuple is a function parameter, there must not be
%%%   an implicit reference to the function.</li>
%%% </ul>

%%% == Transformation steps and compensations ==
%%% <ol>
%%% <li> If the record didn't exist before, insert its definition.</li>
%%% <li> Change the selected tuple to a record expression.</li>
%%% <li> If the tuple was a function parameter, find all corresponding
%%%   function calls and insert a function expression around the
%%%   corresponding argument, wich transforms the matching tuple to the
%%%   record expression.</li>
%%% <li> Find all function calls in the function clause, which
%%%   parameters contains a same typed record. Using the dataflow
%%%   graph every possible called function are collected, where
%%%   the transformation is executed again (these tuple parameters
%%%   are replaced to record update).</li>
%%% <li> Finally, the transformation finds every directly affected
%%%    parts the graph - which call the transformed function - and
%%%    replace their parameters to a record expression.</li>
%%% </ol>

%%% == Implementation status ==
%%% Although the refactoring is implemented, the testing is in progress.
%%% There are some known bugs, which elimination will be done in the near
%%% future.

%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>
%%% @author Matyas Karacsonyi <k_matyas@inf.elte.hu>

-module(reftr_introduce_rec).
-vsn("$Rev: 4783 $ ").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

-import(lists, [zipwith/3, flatten/1, any/2]).
-define(deb(Arg), io:format("Debug: ~p~n", [Arg])).
%%% ============================================================================
%%% Errors

%% @private
error_text(not_tuple, []) ->
    ["The selection is not a tuple skeleton."];
error_text(wrong_arity, [])->
    ["The number of element doesn't match the number of fields."];
error_text(record_exists, [Name]) ->
    ["A record named ", io_lib:write(Name), " already exists."];
error_text(implicit, [])->
    ["There is an implicit reference to the function."];
error_text(illegal_selection, []) ->
    ["The selection cannot be refactored."].

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Expr      = get_skeleton(?Args:expr_range(Args)),
    Recname   = ?Args:name(Args),
    Recfields = split_fields(?Args:string(Args)),

    Exprs = ?Query:exec([Expr], ?Expr:children()),
    ?Check(length(Recfields) == length(Exprs), ?LocalError(wrong_arity, [])),

    File = ?Args:file(Args),
    Recs = ?Query:exec(File, ?File:records()),
    ?Check(not lists:member(Recname, [?Rec:name(R) || R <- Recs]),
           ?LocalError(record_exists, [Recname])),

    [{_, Parent}] = ?Syn:parent(Expr),
    [fun () ->
             Form = define_record(Recname, Recfields),
             ?File:add_form(File, Form),
             ?Transform:touch(File)
     end,
     case ?ESG:data(Parent) of
         #clause{type=fundef} ->
             ?Check(?Query:exec(Parent, ?Query:seq([?Clause:form(), ?Form:func(),
                                                    ?Fun:implicits()])) == [],
                    ?LocalError(implicit, [])),
             Schema = ?Query:exec(Expr, ?Expr:children()),

             transform(Recname, Recfields, Schema, Expr);

         _ ->
             throw(?LocalError(illegal_selection, []))
     end].

transform(RName, RFields, Schema, Start) ->
    fun(_) ->
            List = collect(Schema, Start),
            Called = 
                [begin
                     PC = ?Query:exec(Pattern, ?Expr:children()),
                     PR = case FunCalls of
                              [] ->
                                  make_record(RName, RFields, PFields);
                              _ ->
                                  make_record_pattern(VarName, RName, RFields, PFields)
                          end,
                     replace(Pattern, PR),

                     lists:foreach(
                       fun (Node) ->
                               NC = ?Query:exec(Node, ?Expr:children()),
                               RU = make_record_update(VarName, RName, RFields, PC, NC),
                               replace(Node, RU)
                       end, FunCalls),
                     Back
                 end || {Pattern, PFields, VarName, FunCalls, Back} <- List],
            
            lists:foreach(
              fun (X) ->
                      Expr = ?Query:exec(X, ?Expr:children()),
                      Record = make_record(RName, RFields, Expr),
                      replace(X, Record)
              end, lists:umerge(Called))
    end.

collect(Schema, Pattern) ->
    Visited = ets:new(visited_nodes, [set]),
    
    try
        PC = ?Query:exec(Pattern, ?Expr:children()),
        [{_, Parent}] = ?Syn:parent(Pattern),
        X = ?Query:exec(Parent, [pattern]),
        lists:flatten(
          [begin
               C  = ?Query:exec(P, ?Expr:children()),
               
               case check_type(PC, C) of
                   true ->
                       collect(Schema, P, Visited);
                   false ->
                       []
               end
           end || P <- X])
    after
        ets:delete(Visited)
    end.

collect(Schema, Pattern, Visited) ->
    case ets:lookup(Visited, Pattern) of
        [] ->
            ets:insert(Visited, {Pattern}),
            Children = ?Query:exec(Pattern, ?Expr:children()),
            CD = [?ESG:data(C) || C <- Children],
            
            PFields = [begin
                           Data = ?ESG:data(C),
                           case Data of
                               #expr{type=joker} ->
                                   no;
                               #expr{type=variable} ->
                                   var_occ(Data, CD,
                                           lists:delete(C,
                                                        ?Query:exec(C, ?Query:seq(?Expr:variables(),
                                                                                  ?Var:occurrences()))));
                               #expr{type=match_expr} ->
                                   [Left, Right] = ?Query:exec(C ,?Expr:children()),
                                   Occ = lists:delete(Left, ?Query:exec(Left,
                                                                        ?Query:seq(?Expr:variables(),
                                                                                   ?Var:occurrences()))),
                                   case var_occ(Data, CD, Occ) of
                                       no ->
                                           Right;
                                       _ ->
                                           C
                                   end;
                               _ ->
                                   C
                           end
                       end || C <- Children],
            
            Sites = get_occurrences(Schema, Children),
            
            FunCalls = lists:umerge(
                         [begin
                              [{_, Parent}] = ?Syn:parent(S),
                              
                              case ?ESG:data(Parent) of
                                  #expr{type=arglist} ->
                                      [S];
                                  _ ->
                                      []
                              end
                          end || S <- Sites]),
            
            Back = lists:delete(Pattern, ?Dataflow:reach([Pattern], [back])),
            Flow = lists:flatten([lists:delete(FC, ?Dataflow:reach([FC], [{back, false}])) ||
                                     FC <- FunCalls]),
            
            FlC = lists:umerge([collect(Schema, F, Visited) || F <- Flow]),
            [{Pattern, PFields, ?Var:new_varname(Pattern, "Rec"), FunCalls, Back} | FlC];
        
        _ ->
            []
    end.


var_occ(_, _, []) ->
    no;
var_occ(C, Pattern, [Node|Rest]) ->
    [{_, Parent}] = ?Syn:parent(Node),
    
    case ?ESG:data(Parent) of
        #expr{type=tuple} ->
            [{_, P2}] = ?Syn:parent(Parent),
            
            case ?ESG:data(P2) of
                #expr{type=arglist} ->
                    Eq = lists:zipwith(
                           fun (P, N) ->
                                   ND = ?ESG:data(N),
                                   if
                                       (C =:= P) and (P#expr.value /= ND#expr.value) ->
                                           true;
                                       true ->
                                           false
                                   end
                           end, Pattern, ?Query:exec(Parent, ?Expr:children())),
                    case lists:member(true, Eq) of
                        true ->
                            Node;
                        _ ->
                            var_occ(C, Pattern, Rest)
                    end;
                _ ->
                    Node
            end;
        _ ->
            Node
    end.


get_skeleton(Exprs) ->
    ?Check(length(Exprs) == 1, ?LocalError(not_tuple, [])),
    [Expr] = Exprs,
    ?Check(?Expr:type(Expr) == tuple, ?LocalError(not_tuple, [])),
    Expr.


split_fields(S) ->
    [list_to_atom(A) || A <- string:tokens(S, " ")].


%% @spec make_record(atom(), [atom()], [node()]) -> node()
%% @doc Creates a record expression. Copies the expressions.
make_record(RName, RFields, Exprs) ->
    RFNodes = zipwith(fun(N, E) ->
                              {{record_field, N}, copy(E)}
                      end, RFields, Exprs),
    ?Syn:construct({{record_expr, RName}, RFNodes}).


make_record_pattern(VarName, RName, Fields, Pattern) ->
    RFields = make_record_pattern(Fields, Pattern),
    ?Syn:construct({match_expr, {var, VarName}, {{record_expr, RName}, RFields}}).

make_record_pattern([], []) ->
    [];
make_record_pattern([FName|Fields], [Node|NP]) ->
    case Node of
        no ->
            [];
        _ ->
            [{{record_field, FName}, copy(Node)}]
    end ++ make_record_pattern(Fields, NP).


make_record_update(VarName, RName, RFields, PC, NC) ->
    RUFields = make_record_update(RFields, PC, NC),
    
    ?Syn:construct(
       case RUFields of
           [] ->
               {var, VarName};
           _ ->
               {{record_update, RName}, {var, VarName}, RUFields}
       end).

make_record_update([], [], []) ->
    [];
make_record_update([FName|RFields], [Pattern|PC], [Node|NC]) ->
    P = ?ESG:data(Pattern),
    N = ?ESG:data(Node),

    if
        P#expr.value /= N#expr.value ->
            [{{record_field, FName}, copy(Node)}];
        true ->
            []
    end ++ make_record_update(RFields, PC, NC).

%% @spec define_record(atom(), [atom()]) -> form()
%% @doc Creates a record definition.
define_record(Name, Fields) ->
    ?Syn:construct({{form, record, Name}, [{{spec_field, F}, []} || F <- Fields]}).

copy(Node) ->
    proplists:get_value(Node, ?Syn:copy(Node)).

replace(From, To) ->
    ?Transform:touch(From),
    case ?Syn:parent(From) of
        [{_, Parent}] ->
            ?Syn:replace(Parent, {node, From}, [To]);
        _ ->
            []
    end.

get_occurrences(Schema, Children) ->
    VarOcc = ?Query:exec(Children, ?Query:seq(?Expr:variables(), ?Var:occurrences())),

    lists:umerge(
      [begin
           [{_, Parent}] = ?Syn:parent(Node),
           
           case ?ESG:data(Parent) of
               #expr{type=tuple} ->
                   C  = ?Query:exec(Parent, ?Expr:children()),
                   
                   case check_type(Schema, C) of
                       true ->
                           [Parent];
                       
                       false ->
                           []
                   end;
               
               _ ->
                   []
           end
       end || Node <- VarOcc]).

check_type([], []) ->
    true;
check_type(_, []) ->
    false;
check_type([], _) ->
    false;
check_type([Pattern|RestPatt], [Schema|RestSch]) ->
    P = ?ESG:data(Pattern),
    S = ?ESG:data(Schema),
    if
        (P#expr.type == S#expr.type) or
        (P#expr.type == variable) or (S#expr.type ==joker) ->
            check_type(RestPatt, RestSch);

        true ->
            false
    end.
