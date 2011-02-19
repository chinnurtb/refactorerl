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

%%% @doc The order of a function's arguments is a small, aesthetic aspect
%%%      of a program. Swapping arguments can improve the readability of the
%%%      program, and it can be used as a preparation for another refactoring,
%%%      eg. to create a tuple from arguments that aren't next to each other.
%%%
%%% <ul>
%%%  <li>A module (see {@link reflib_args:order/1}). The new order of the
%%%      function arguments, a list of integer numbers that specifies which
%%%      parameter goes where. For example, the list `3,2,1' means that the
%%%      third parameter will be the first, the first parameter will be the
%%%      last and the second parameter remains in the same place. The list
%%%      must have the same length as the arity of the function.</li>
%%%  <li>A module (see {@link reflib_args:function/1}).The function to be
%%%      modified. Currently it can be specified with aposition anywhere in
%%%      the function definition.</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%% <li>When a function application has an argument with side effects,
%%% the transformation may only be carried out after a warning that
%%% the order of side effects most probably will change, which may
%%% change the way the program works.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%%
%%%<ul>
%%% <li>Change the order of patterns in every clause's parameter list in the
%%%  function according to the given new order.
%%% </li>
%%% <li>
%%%  For every static call of the function, change the order of the
%%%  expressions that provide the actual parameters to the call according to the
%%%  given order (obviously in all modules).
%%% </li>
%%% <li>
%%%  Every implicit function expression is expanded to the corresponding
%%%  explicit expression which contains a static call to the function; this
%%%  function call is then updated as described in the previous case.
%%% </li>
%%% <li>
%%%  For every call of the function that provides the arguments as a list,
%%%  insert a compensating function expression that changes the order of the
%%%  elements in the list according to the given new order.
%%% </li>
%%% </ul>
%%%
%%% == Implementation status ==
%%%
%%% This transformation can handle implicit function calls.
%%% These implicit calls are expanded automatically before the arguments
%%% are reordered, so these function calls cannot cause any problems.
%%% On the other hand, there is a problem with dynamic function calls.
%%% The arguments in such and similar constructs (for example, `apply/3'
%%% or `apply/4') currently cannot be recognised by the tool.
%%% Because of this, the transformation cannot detect these elements as
%%% a function call and its arguments. When the tool is able to detect
%%% this dynamic calls, the transformation can take care of these problems
%%% automatically.
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(reftr_reorder_funpar).
-vsn("$Rev: 5508 $ ").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
error_text(dirty_arg, MFA) ->
    FunInfo = ?MISC:fun_text(MFA),
    ["The function has a dirty argument in ", FunInfo].

%% Note: depends on the representation of applications
%% TODO: move to a query library
path_application_args() -> ?Expr:child(2).

path_expr_fun() -> ?Query:seq([?Expr:clause(), ?Clause:form(), ?Form:func()]).

legal_order(Order, Arity) -> lists:sort(Order) =:= lists:seq(1, Arity).

%% @private
prepare(Args) ->
    Fun      = ?Args:function(Args),
    Arity    = ?Fun:arity(Fun),

    ArgsInfo = add_transformation_info(Args, Fun, Arity),
    NewOrder = ?Args:ask(ArgsInfo, order, fun cc_neworder/2, fun cc_error/3, Arity),

    ImpCalls = ?Query:exec(Fun, ?Fun:implicits()),
    DynFunCalls = ?Dynfun:collect(reord, Fun, NewOrder),
    [fun()  -> ?Expr:expand_funexpr(ImpCalls) end,
     fun(_) -> perform(Fun, NewOrder)  end,
     fun(_) -> ?Dynfun:transform(DynFunCalls) end].

%% @private
add_transformation_info(Args, Fun, Arity) ->
    FunName = ?Fun:name(Fun),
    Info    = ?MISC:format("Reordering function parameters: ~p/~p",
                           [FunName,Arity]),
    [{transformation_text, Info} | Args].

%% @private
perform(Fun, NewOrder) ->
    Clauses1 = ?Query:exec(Fun, ?Query:seq(?Fun:definition(), ?Form:clauses())),
    Clauses  = [{Cl, ?Query:exec(Cl, ?Clause:patterns())} || Cl <- Clauses1],

    %% Note: function applications have to be determined
    %% here, as expand_funexpr will contribute to FunApps
    ArgLists = ?Query:exec(Fun, ?Query:seq(?Fun:applications(),
                                           path_application_args())),
    AppArgs  = [{ArgList, ?Query:exec(ArgList, ?Expr:children())} ||
                   ArgList <- ArgLists],
    TaggedArgs = [{{clex, Node}, NArgs} || {Node, NArgs} <- Clauses] ++
                            [{{elex, Node}, NArgs} || {Node, NArgs} <- AppArgs],
    TodoList = lists:sort(lists:concat([check_and_create_todolist(Args, NewOrder) || Args <- TaggedArgs])),
    NewTodoList = combine_lists(TodoList, [], []),

    [check_dirty_arg(Expr) || {_, ArgList} <- AppArgs, Expr <- ArgList],

    prepare_groups(NewTodoList),

    [?Transform:touch(Node) || {Node, _} <- Clauses ++ AppArgs],
    [?Syn:replace(Node, {range, hd(NArgs), lists:last(NArgs)},
                  reorder(NArgs, NewOrder)) ||
        {Node, NArgs} <- Clauses ++ AppArgs].

check_and_create_todolist({ArgListNode, ArgList}, Order)->
    L = lists:map(fun macros/1, ArgList),
    groups_to_todos(create_groups(L), Order, [], {ArgListNode, ArgList}).

%% @private
%% For every argument creates a list of the macros used in it.
%% If there are parts of the argument which are not a result of a macro
%% the 'not_virtual' atom is added to the list.
macros(Arg) ->
    L = case reflib_expression:tokens(Arg) =:= reflib_expression:virtuals(Arg) of
            true ->
                [];
            _    ->
                [not_virtual]
        end,
    L ++ lists:usort(?Query:exec(Arg, ?Expr:macros())).

%% @private
%% Creates groups of the arguments. A group can be 'not_virtual' if it doesn't
%% contain any macros, mixed if it contains virtual and not virtual tokens or
%% virtual tokens from more than one macro. The arguments in third type of
%% groups are all the result of the same macro substitution.
create_groups(List) ->
    create_groups(List, [], {first, 0, 0}, 1).

create_groups([], Res, L, _) ->
    [_|T] = lists:reverse([L |Res]),
    T;
create_groups([H| Rest], Res, L = {Last, Begin, Length}, Pos) ->
    V = case H of
            [not_virtual] ->
                if
                    Last == not_virtual -> continue;
                    true                -> new
                end;
            [X] ->
                case Last of
                    X                 -> continue;
                    {mixed, LastList} ->
                        case lists:member(X, LastList) of
                            true -> mixed;
                            _    -> new
                        end;
                    _            -> new
                end;
            List ->
                case Last of
                    not_virtual -> new_mixed;
                    {mixed, LastList}  ->
                        case LastList -- H of
                            LastList -> new_mixed;
                            _        -> mixed
                        end;
                    _                  ->
                        case lists:member(Last, List) of
                            true -> mixed;
                            _    -> new_mixed
                        end
                end
        end,
    FilterNotVirtuals = fun(X) -> X /= not_virtual end,
    case V of
        continue ->
            create_groups(Rest, Res, {Last, Begin, Length + 1}, Pos + 1);
        new ->
            create_groups(Rest, [L | Res], {hd(H), Pos, 1}, Pos + 1);
	mixed ->
            create_groups(Rest, Res, {{mixed, lists:filter(FilterNotVirtuals, H)}, Begin, Length + 1}, Pos + 1);
        new_mixed ->
            create_groups(Rest, [L |Res], {{mixed, lists:filter(FilterNotVirtuals, H)}, Pos, 1}, Pos + 1)
    end.

%% @private decides what should be done with the different groups before reordering.
%% For example every macro in a mixed group has to be inlined if the position of any
%% argument in the group changes.
groups_to_todos([], _, Res, _) -> lists:reverse(Res);
groups_to_todos([HG={Type, Begin, Length}|Groups], Order, Res, {{Tag, ArgListNode}, ArgList}) ->
    O = lists:sublist(Order, Begin, Length),
    Todo = case Length of
        1 -> ok;
        _ ->
            L = lists:seq(Begin, Begin + Length - 1),
            case Type of
                {mixed, _} ->
	            case O of
                        L -> ok;
			_ -> inline
                    end;
		{_, _, _} ->
                    SortedO = lists:sort(O),
                    case L of
                        O       -> ok;
                        SortedO -> macro;
			_       -> inline
                    end;
                _ -> ok
            end
    end,
    Do = case Todo of
              inline -> ?Check(false, ?RefError(mac_error, [])); %% {inline, HG};
              macro  ->
                  GetOrigTokens = fun(VirtTokens) -> ?Query:exec(VirtTokens, [orig]) end,
                  TokenLists = [begin
                                    VirtTokens = lists:sort(?Expr:tokens(Arg)),
                                    Tlist = lists:zip(GetOrigTokens(VirtTokens),VirtTokens),
                                    [{T, [V]} || {T, V} <- Tlist]
                                end || Arg <- lists:sublist(ArgList, 1, Length)],
                  Commas = lists:concat([?Query:exec(ArgListNode, [{Tag, N}]) ||
                                                       N <- lists:seq(Begin + 1, Begin + Length - 1)]),
                  CommaTokens = [{T, [V]} || {T, V} <- lists:zip(?Query:exec(Commas, [orig]), Commas)],

                  VirtualsInArgs = lists:concat([[V || {_, [V]} <- ArgTokens] || ArgTokens <- TokenLists]),
                  Virtuals = VirtualsInArgs ++ Commas,
                  ?Macro:check_single_usage(Virtuals, []),

                  {macro, Type, [N - Begin + 1 || N <- O], TokenLists, CommaTokens};
              _ -> {ok, HG}
          end,
    groups_to_todos(Groups, Order, [Do | Res], {{Tag, ArgListNode}, lists:nthtail(Length, ArgList)}).

%% @private
%%
combine_lists([], Last, Res) -> Res ++ Last;
combine_lists([Todo | TodoList], Last, Res) ->
    C = case Todo of
        {macro, MacNode, Order, TokenLists, Commas} ->
            case Last of
                [{macro, MacNode, _, LastTokenLists, LastCommas}] ->
                    {macro, MacNode, Order, combine_tokenlists(LastTokenLists, TokenLists, []),
                                                lists:concat(combine_tokenlists([LastCommas], [Commas], []))};
                _ -> ok
             end;
        _ -> ok
    end,
    case C of
        ok -> combine_lists(TodoList, [Todo], Res ++ Last);
        _  -> combine_lists(TodoList, [C], Res)
    end.

combine_tokenlists([], [], Res) -> Res;
combine_tokenlists([H1|T1], [H2|T2], Res) ->
    R = [{Token, H1Virt ++ H2Virt} ||
                   {{Token, H1Virt}, {Token, H2Virt}} <- lists:zip(H1, H2)],
    combine_tokenlists(T1, T2, Res ++ [R]).

%% private
%% prepares the arguments according to the todolist
prepare_groups(TodoList) ->
    F = fun(T) -> case T of
                      {macro, _, Order, TokenLists, Commas} ->
                          reorder_macro(Order, Commas, TokenLists);
                      {inline, _} -> inline; %% @TODO inline these arguments
                      _ -> ok
                  end
        end,
    lists:map(F, TodoList).

reorder_macro(Order, CommaTokens, TokenLists) ->
    AllTokens = lists:sort(lists:concat(TokenLists) ++ CommaTokens),

    TmpDataLists = [lists:map(fun({T, V}) -> {?ESG:data(T), V} end,
                                             lists:nth(N, TokenLists)) || N <- Order],

    CommaDataList = lists:map(fun({C, V}) -> {?ESG:data(C), V} end, CommaTokens),

    DataLists = insert_commas(TmpDataLists, CommaDataList, []),
    update_macro(AllTokens, DataLists).

insert_commas([H], [], Res) ->
    Res ++ [H];
insert_commas([H|T], [C|Commas], Res) ->
    insert_commas(T, Commas, Res ++ [H, [C]]).

update_macro([], _) -> ok;
update_macro(TokenList, [NewDataList|NewDataLists]) ->
    RemainingTokens = update(TokenList, NewDataList),
    case RemainingTokens of
        [] -> done;
        _  ->
            update_macro(RemainingTokens, NewDataLists)
    end.

update(TokenList, []) ->
    TokenList;
update([{Token, OldVirts}|TokenList], [{NewData, NewVirts}|NewDataList]) ->
    [?Graph:rmlink(OldVirt, orig, Token) || OldVirt <- OldVirts],
    [?Graph:mklink(NewVirt, orig, Token) || NewVirt <- NewVirts],
    ?ESG:update(Token, NewData),
    update(TokenList, NewDataList).

%% @private
reorder(Args, Order) ->
      [lists:nth(N, Args) || N <- Order].

%% @private
check_dirty_arg(Node) ->
    ?Check(not ?Expr:has_side_effect(Node), error_dirty_arg(Node)).

%% @private
error_dirty_arg(Node) ->
    [Func] = ?Query:exec(Node, path_expr_fun()),
    {_ModuleNode, MFA} = ?Fun:mod_fun_arity(Func),
    ?LocalError(dirty_arg, MFA).

%% @private
%% Checks the legality of order and begins an interaction if it isn't legal.
cc_neworder(Order, Arity) ->
    ?Check(legal_order(Order, Arity), ?RefError(bad_order, [Arity])),
    Order.

%% @private
cc_error(?RefError(bad_order, [Arity]), _Order, Arity) ->
    ?MISC:format("The given order should have all values from 1 to " ++ 
												 integer_to_list(Arity) ++ "!").
