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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2010,
%%% Eötvös Loránd University. All Rights Reserved.


%%% ============================================================================
%%% Module information

%%% @doc This module is implemetns the introduce record refactoring. Given a
%%% tuple skeleton, this transformation converts it to a record expression and
%%% inserts compensating record expressions or record update expression  at the
%%% needed places.
%%%
%%% == Parameters ==
%%% <ul>
%%% <li>A tuple (see {@link reflib_args:expr_range/1}).</li>
%%% <li>A record name (see {@link reflib_args:name/1}).</li>
%%% <li>Record filed names separated by whitespace
%%%   (see {@link reflib_args:string/1}).</li>
%%% </ul>
%%%
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
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%% <li> The refactoring finds every tuple in the function pattern, which has
%%%   the same type. </li>
%%% <li> The transformation checks every function clause to find those,
%%%   which parameter contains at least one same typed tuple. </li>
%%% <li> The refactoring collects every function calls, which calls the
%%%   collected function clause. </li>
%%% <li> The refactoring finds all function calls in the collected
%%%   function clauses, where the parameter contains at least one same
%%%   typed tuple. </li>
%%% <li> The transformation collects the return parameter, if it is a same
%%%   typed tuple </li>
%%% <li> The refactoring finds every function calls in the collected function
%%%   clauses, which parameter contains at least one same typed tuple. The
%%%   transformation finds that function and the collection starts again
%%%   from the first step. </li>
%%% <li> If the record didn't exist before, its definition is constructed. </li>
%%% <li> The collected tuples in the function patterns are replaced to record
%%%   expressions. If a function clause contains function calls, the affected
%%%   record gets bound with a variable name. </li>
%%% <li> The return value is transformed to a record expression, if it was
%%%   collected. </li>
%%% <li> The unused variabled (in the record expression) are elliminated. </li>
%%% <li> Those function calls parameters, which calls a collected function,
%%%   are transformed to record expression. </li>
%%% <li> If a collected function's return parameter is a same typed tuple,
%%%   and the calling place is match expression, the left side is transformed
%%%   to a record expression too. </li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% Although the refactoring is implemented, the testing is in progress.
%%%
%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>
%%% @author Matyas Karacsonyi <k_matyas@inf.elte.hu>

-module(reftr_introduce_rec).
-vsn("$Rev: 4988 $ ").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").


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

             [{_, Fun}] = ?Syn:parent(Parent),

             fun(_) ->
                     transform(Recname, Recfields, Schema, Fun)
             end;

         _ ->
             throw(?LocalError(illegal_selection, []))
     end].

transform(RName, RFields, Schema, Fun) ->
    List = lists:flatten(collect(Schema, Fun)),

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

	     [begin
		  Fields = ?Query:exec(R, ?Expr:children()),
		  Ret = make_record(RName, RFields, Fields),
		  replace(R, Ret)
	      end || R <- Return, R /= []],

             lists:foreach(
               fun (Node) ->
                       NC = ?Query:exec(Node, ?Expr:children()),
                       RU = make_record_update(VarName, RName, RFields, PC, NC),
                       replace(Node, RU)
               end, FunCalls),
             Back
         end || {Pattern, PFields, VarName, FunCalls, Back, Return} <- List],

    lists:foreach(
      fun (X) ->
              Expr = ?Query:exec(X, ?Expr:children()),
              Record = make_record(RName, RFields, Expr),
              replace(X, Record)
      end, lists:umerge(Called)).

collect(Schema, Fun) ->
    Visited = ets:new(visited_nodes, [set]),

    try
        collect(Schema, Fun, Visited)
    after
        ets:delete(Visited)
    end.

collect(Schema, Fun, Visited) ->
    Clauses = ?Query:exec(Fun, [funcl]),

    TC = lists:flatten(
           [begin
                CP = collect_clauses(Schema, Clause, Visited),

                case CP of
                    [] ->
                        [];
                    _ ->
                        [P || P <- CP, P /= []]
                end
            end || Clause <- Clauses]),
    
    FunCalls = lists:flatten([FC || {_, _, _, FC, _, _} <- TC]),
    Flow = lists:flatten([lists:delete(FC, ?Dataflow:reach([FC], [{back, false}])) ||
                             FC <- FunCalls]),
    Funs = 
	[X || {_, X} <- 
		  lists:umerge([lists:filter(fun (X) ->
						     case X of 
							 {form, _} -> true;
							 _ -> false
						     end
					     end, ?Syn:root_path(Call)) || Call <- Flow])],

    TC ++ [collect(Schema, F, Visited) || F <- Funs].


collect_clauses(Schema, Clause, Visited) ->
    Patterns = ?Query:exec(Clause, [pattern]),
    UsedNamesBuffer = ets:new(used_var_names, [set]),

    try
        [begin
             case ets:lookup(Visited, Pattern) of
                 [] ->
                     ets:insert(Visited, {Pattern}),
                     Fields = ?Query:exec(Pattern, ?Expr:children()),
		     collect_clauses(Schema, Clause, Pattern, Fields, Visited, UsedNamesBuffer);

                 _ ->
                     []
             end
         end || Pattern <- Patterns]
    after
        ets:delete(UsedNamesBuffer)
    end.

collect_clauses(Schema, Clause, Pattern, Fields, Visited, UsedNamesBuffer) ->
    case check_type(Schema, Fields) of
	true ->
	    PFields = pattern_fields([?ESG:data(C) || C <- Fields], Fields),
	    FunCalls = get_funcalls(Schema, Fields, Visited),
	    Back = lists:delete(Pattern, ?Dataflow:reach([Pattern], [back])),
	    
	    UsedNames = [Elem || {Elem} <- ets:tab2list(UsedNamesBuffer)],
	    VarName = ?Var:new_varname(Pattern, "Rec", UsedNames),
	    ets:insert(UsedNamesBuffer, {VarName}),

	    Last = lists:last(?Query:exec(Clause, [body])),
	    Return =
		case {?Expr:type(Last),
		      check_type(Schema, ?Query:exec(Last, ?Expr:children()))} of
		    {tuple, true} ->
			[Last |
			 [begin
			      RootPath = lists:reverse(?Syn:root_path(B)),
			      ME = lists:filter(
				     fun ({_, X}) ->
					     case ?ESG:data(X) of
						 #expr{type=match_expr} ->
						     true;
						 _ ->
						     false
					     end
				     end, RootPath),

			      case ME of
				  [] ->
				      [];

				  [{_, MatchExpr}|_] ->
				      [Left|_] = ?Query:exec(MatchExpr, ?Expr:children()),
				      Left
			      end
			  end || B <- Back]];
		    
		    _ ->
			[]
		end,
	    
	    {Pattern, PFields, VarName, FunCalls, Back, Return};
	
	_ ->
	    []
    end.


%% @doc Collects which record elements should be bound in the pattern of a function.
pattern_fields(_, []) ->
    [];
pattern_fields(FData, [Field|Rest]) ->
    Data = ?ESG:data(Field),

    Ret =
        case Data of
            #expr{type=joker} ->
                no;
            #expr{type=variable} ->
                var_occ(Data, FData,
                        lists:delete(Field,
                                     ?Query:exec(Field, ?Query:seq(?Expr:variables(),
                                                                   ?Var:occurrences()))));
            #expr{type=match_expr} ->
                [Left, Right] = ?Query:exec(Field ,?Expr:children()),
                Occ = lists:delete(Left, ?Query:exec(Left,
                                                     ?Query:seq(?Expr:variables(),
                                                                ?Var:occurrences()))),
                case var_occ(Data, FData, Occ) of
                    no ->
                        Right;
                    _ ->
                        Field
                end;
            _ ->
                Field
        end,
    [Ret | pattern_fields(FData, Rest)].

%% @doc Collects every same typed tuple in a function clause
var_occ(_, _, []) ->
    no;
var_occ(C, Pattern, [Node|Rest]) ->
    [{_, Parent}] = ?Syn:parent(Node),
    [{_, P2}] = ?Syn:parent(Parent),

    case {?ESG:data(Parent), ?ESG:data(P2)} of
        {#expr{type=tuple}, #expr{type=arglist}} ->
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
    end.

%% @spec get_funcalls(node(), node(), node()) -> node()
%% @doc Collects every function call from a function clause, where its parameter
%%   contains at least one same typed tuple.
get_funcalls(Schema, Fields, Visited) ->
    VarOcc = ?Query:exec(Fields, ?Query:seq(?Expr:variables(), ?Var:occurrences())),

    FunCalls =
        [begin
             [{_, Parent}] = ?Syn:parent(Node),
             [{_, P2}] = ?Syn:parent(Parent),

             case {?ESG:data(Parent), ?ESG:data(P2), ets:lookup(Visited, Parent)} of
                 {#expr{type=tuple}, #expr{type=arglist}, []} ->
                     ets:insert(Visited, {Parent}),
                     Children  = ?Query:exec(Parent, ?Expr:children()),

                     case check_type(Schema, Children) of
                         true ->
                             Parent;

                         false ->
                             []
                     end;

                 {#expr{type=infix_expr}, #expr{type=tuple}, []} ->
                     ets:insert(Visited, {Parent}),
                     Children  = ?Query:exec(P2, ?Expr:children()),
                     [{_, P}] = ?Syn:parent(P2),

                     case {check_type(Schema, Children), ?ESG:data(P)} of
                         {true, #expr{type=arglist}} ->
                             P2;

                         {false, _} ->
                             []
                     end;

                 _ ->
                     []
             end
         end || Node <- VarOcc],
   [Ret || Ret <- FunCalls, Ret /= []].

%% @spec check_type([node()], [node()]) -> true | false
check_type([], []) ->
    true;
check_type(_, []) ->
    false;
check_type([], _) ->
    false;
check_type([Pattern|RestPatt], [Schema|RestSch]) ->
    P = ?Expr:type(Pattern),
    S = ?Expr:type(Schema),
    if
        (P == S) or
        (P == variable) or (S == joker) or
        (S == variable) or (P == infix_expr)->
            check_type(RestPatt, RestSch);

        true ->
            false
    end.


%% @spec define_record(atom(), [atom()]) -> node()
%% @doc Creates a record definition.
define_record(Name, Fields) ->
    ?Syn:construct({{form, record, Name}, [{{spec_field, F}, []} || F <- Fields]}).

%% @spec make_record(atom(), [atom()], [node()]) -> node()
%% @doc Creates a record expression.
make_record(RName, RFields, Pattern) ->
    RFNodes = make_record(RFields, Pattern),
    ?Syn:construct({{record_expr, RName}, RFNodes}).

make_record([], []) ->
    [];
make_record([FName|Fields], [Node|NP]) ->
    case Node of
        no ->
            [];
        _ ->
            [{{record_field, FName}, copy(Node)}]
    end ++ make_record(Fields, NP).


%% @spec make_record_pattern(atom(), atom(), [atom()], [node()]) -> node()
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


%% @spec make_record_update(atom(), atom(), [atom()], [node()], [node()]) -> node()
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
    P = ?Expr:value(Pattern),
    N = ?Expr:value(Node),

    if
        P /= N ->
            [{{record_field, FName}, copy(Node)}];
        true ->
            []
    end ++ make_record_update(RFields, PC, NC).


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


get_skeleton(Exprs) ->
    ?Check(length(Exprs) == 1, ?LocalError(not_tuple, [])),
    [Expr] = Exprs,
    ?Check(?Expr:type(Expr) == tuple, ?LocalError(not_tuple, [])),
    Expr.

split_fields(S) ->
    [list_to_atom(A) || A <- string:tokens(S, " ")].
