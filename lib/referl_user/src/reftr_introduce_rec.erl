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
-vsn("$Rev: 5661 $ ").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").


%%% ============================================================================
%%% Errors

%% @private
error_text(not_tuple, []) ->
    ["The selection is not a tuple skeleton."];
error_text(wrong_arity, [])->
    ["The number of elements doesn't match the number of fields."];
error_text(record_exists, [Name]) ->
    ["A record named ", io_lib:write(Name), " already exists."];
error_text(implicit, [])->
    ["There is an implicit reference to the function that contains the selection."];
error_text(illegal_selection, []) ->
    ["The selected tuple is not a parameter of a function definition."].

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Expr      = get_tuple(?Args:expr_range(Args)),

    [{_, Parent}] = ?Syn:parent(Expr),
    [{_, Fun}]    = ?Syn:parent(Parent),
    ?Check(is_fundef_param(Expr, Parent), ?LocalErr0r(illegal_selection)),
    ?Check([] =:= implicit_refs(Parent), ?LocalErr0r(implicit)),

    File     = ?Args:file(Args), % @todo expr - form - file
    FilePath = ?File:path(File),
    Recnames = [?Rec:name(R) || R <- ?Query:exec(File, ?File:records())],
    Args2    = info1(Args, Expr, Fun),
    RName    = ?Args:ask(Args2, name, fun cc_recname/2, fun cc_error/3, Recnames),

    Schema   = ?Query:exec(Expr, ?Expr:children()),
    RecArity = length(Schema),
    Args3    = info2(Args2, RName, RecArity),
    RFields  = ?Args:ask(Args3, recfield_names, fun cc_recfield/2, fun cc_error/3,
                         RecArity, fun split_fields/1),

    Collected = lists:flatten(collect(Schema, Fun)),
    UseInfos = lists:flatten([Clause || {_, Clause} <- Collected]),
    FunDefs = lists:umerge([?Query:exec([FD], [fundef]) || {FD, _} <- Collected]),
    DynFunCalls =
        [?Dynfun:collect(int_rec, FD, {RName, RFields}) || FD <- FunDefs],

    Calleds = [Called || {_, _, _, _, Called, _} <- UseInfos],
    UCalledWChs = [{Node, Children}
                        ||  Node <- lists:umerge(Calleds),
                            Children <- [?Query:exec(Node, ?Expr:children())],
                            Children =/= []],

    ?Transform:touch(File),
    [fun () ->
        Form = define_record(RName, RFields),
        ?File:add_form(File, Form)
     end,
     fun(_) ->
        [replace_record_use(UseInfo, RFields, RName) || UseInfo <- UseInfos],
        [replace(Node, make_record(RName, RFields, Children))
            || {Node, Children} <- UCalledWChs],
        [?Dynfun:transform(DCall) || DCall <- DynFunCalls]
     end,
     fun(_)->
            ?Query:exec(?Query:seq([?File:find(FilePath),
                                    ?Rec:find(RName)]))
     end].

%%% ============================================================================
%%% Implementation

info1(Args, Expr, Fun) ->
    [FunDef] = ?Query:exec(Fun, ?Form:func()),
    Info =
        ?MISC:format(
            "Introducing a record from ~p, a parameter in function ~p.",
                [?Syn:flat_text(Expr), get_mfa_text(FunDef)]),
    [{transformation_text, Info} | Args].

info2(Args, RName, RecArity) ->
    Info =
        ?MISC:format("The new record #~p will have ~p fields.",
                     [RName, RecArity]),
    [{transformation_text, Info} | Args].


%% Returns the Mod:Fun/Arity text, formatted.
%% todo Move to an appropriate module. Possibly it already exists...
get_mfa_text(Fun) ->
    [Mod] = ?Query:exec(Fun, ?Fun:module()),
    ?MISC:format("~p:~p/~p", [?Mod:name(Mod), ?Fun:name(Fun), ?Fun:arity(Fun)]).


%% Returns whether the node is a parameter in a function definition.
is_fundef_param(Node, Parent) ->
    case ?ESG:data(Parent) of
        #clause{type=fundef} ->
            Patterns = ?Query:exec(Parent, ?Clause:patterns()),
            lists:member(Node, Patterns);
        _ -> false
    end.

implicit_refs(Node) ->
    ?Query:exec(Node, ?Query:seq([?Clause:form(), ?Form:func(), ?Fun:implicits()])).


replace_record_use({Pattern, PFields, VarName, FunCalls, _, Returns}, RFields, RName) ->
    replace_pattern(FunCalls, PFields, Pattern, RFields, RName, VarName),
    [replace_record(RFields, RName, Return) || Return <- Returns],

    PC = ?Query:exec(Pattern, ?Expr:children()),
    [replace_record_update(FunCall, PC, RFields, RName, VarName)
        || FunCall <- FunCalls].


replace_record_update(Funcall, PC, RFields, RName, VarName) ->
    NC = ?Query:exec(Funcall, ?Expr:children()),
    RU = make_record_update(VarName, RName, RFields, PC, NC),
    replace(Funcall, RU).

replace_record(RFields, RName, Return) ->
    Fields = ?Query:exec(Return, ?Expr:children()),
    Ret = make_record(RName, RFields, Fields),
    replace(Return, Ret).

replace_pattern(FunCalls, PFields, Pattern, RFields, RName, VarName) ->
    PR =
        case FunCalls of
            [] -> make_record(RName, RFields, PFields);
            _  -> make_record_pattern(VarName, RName, RFields, PFields)
        end,
    replace(Pattern, PR).

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
                case collect_clauses(Schema, Clause, Visited) of
                    [] ->
                        [];
                    CP ->
                        [P || P <- CP, P /= []]
                end
            end || Clause <- Clauses]),
    FunCalls = lists:flatten([FC || {_, _, _, FC, _, _} <- TC]),
    Flow = lists:flatten([lists:delete(FC, ?Dataflow:reach([FC], [{back, false}])) ||
                             FC <- FunCalls]),
    Funs =
        [X || {_, X} <-
                  lists:umerge([lists:filter(
                                  fun (X) ->
                                          case X of
                                              {form, _} -> true;
                                              _ -> false
                                          end
                                  end, ?Syn:root_path(Call)) || Call <- Flow])],

    [{Fun, TC} | [collect(Schema, F, Visited) || F <- Funs]].


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

        false ->
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
                           C =:= P andalso P#expr.value =/= ND#expr.value
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


get_tuple(Exprs) ->
    ?Check(length(Exprs) == 1, ?LocalErr0r(not_tuple)),
    [Expr] = Exprs,
    ?Check(?Expr:type(Expr) == tuple, ?LocalErr0r(not_tuple)),
    Expr.

split_fields(S) ->
    [list_to_atom(A) || A <- string:tokens(S, " ")].


%%% ============================================================================
%%% Checks

cc_recname(Recname, Recnames) ->
    ?Check(not lists:member(Recname, Recnames), ?LocalError(record_exists, [Recname])),
    Recname.

cc_recfield(Recfields, RecArity) ->
    ?Check(length(Recfields) == RecArity, ?LocalErr0r(wrong_arity)),
    ?Check(length(lists:usort(Recfields)) == length(Recfields),
           ?LocalErr0r(multiply_given_fields)),
    Recfields.

cc_error(?LocalError(record_exists, [Recname]), Recname, _Recnames) ->
    ?MISC:format("Record ~p already exists.", [Recname]);
cc_error(?LocalErr0r(multiply_given_fields), Recfields, _RecArity) ->
    ?MISC:format("Multiply given record field names: ~p.",
                 [Recfields -- lists:usort(Recfields)]);
cc_error(?LocalErr0r(wrong_arity), Recfields, RecArity) ->
    ?MISC:format("The record should have ~p fields. Instead, ~p field names were given.",
                 [RecArity, length(Recfields)]).
