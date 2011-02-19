%%% -*- coding: latin-1 -*-

%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://plc.inf.elte.hu/erlang/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%% License for the specific language governing rights and limitations under
%%% the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the Original Code is Eötvös Loránd University.
%%% Portions created by Eötvös Loránd University are Copyright 2008, Eötvös
%%% Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc This module implements the ``Reorder Function Parameters'' refactoring.
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(referl_tr_reorder_funpar).
-vsn("$Rev: 1969 $ ").

-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/3]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

-export([fun_data_by_pos/2]).
%%% ============================================================================
%%% State information

%%% @ type refst() = #refst{modnode = node(),
%%%                        filenode = node(),
%%%                        position = integer(),
%%%                        funname = atom(),
%%%                        arity = integer(),
%%%                        funnode = node(),
%%%                        order = string(),
%%%                        neworder = list(),
%%%                        sideeffect = bool(),
%%%                        dynamic = bool(),
%%%                        funclauses = [node()],
%%%                        funcalls = [node()],
%%%                        implicit_calls = [node()],
%%%                        funnames = [atom()],
%%%                        morefiles = [atom()]}.
%%%
%%% Fields of the State record
%%%
%%% <ul>
%%%     <li>`modnode': Module node of the used file.
%%%         `filenode': Node of the used file.
%%%         `position': Position of the marked function in the file.
%%%         `funname': Name of the marked (pointed) function.</li>
%%%     <li>`order': The new order of the parameter list in string()
%%%                  format.</li>
%%%     <li>`neworder': The new order of the parameter list.</li>
%%%     <li>`sideeffect': True if the marked function can use side effect.</li>
%%%     <li>`dynamic': True if the marked function is called dinamically.</li>
%%%     <li>`funclauses': All clauses of the marked function.</li>
%%%     <li>`funcalls': All calls of the marked function.</li>
%%%     <li>`implicit_calls': All implicit function calls of the marked
%%%                           function</li>
%%%     <li>`funnames': Names of the functions where the marked function is
%%%                     called.</li>
%%%     <li>`morefiles': Files, where the marked function is called.</li>
%%% </ul>

-record(refst, {modname,
                modnode,
                filenode,
                position,
                funname,
                arity,
                funnode,
                order,
                neworder,
                sideeffect = false,
                dynamic = false,
                funclauses,
                funcalls,
                implicit_calls,
                funnames,
                morefiles}).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface

%%% ============================================================================
%%% Callbacks



%% @spec do(ModName::string(), Position::integer(), NewOrder::string()) -> ok
%%
%% @doc Reorders the parameters of the function that is on the position
%% `Pos' in the module `ModName' based on the new order `NewOrder'.
do(ModName, Position, NewOrder) ->
     ?TRANSFORM:do(referl_tr_reorder_funpar,
                  {ModName, Position, NewOrder}).

%%% @private
init({ModName, Position, NewOrder}) ->
    #refst{modname = ModName,
           position = Position,
           order = NewOrder}.
%%% @private
steps() ->
    [
    fun get_module_node/1,
    fun order_list/1,
    fun pos_token_data/1,
    fun get_clauses/1,
    fun get_implicit_calls/1,
    fun get_calls/1,
    fun extract_implicit_funcalls/1,
    fun get_calls/1,
    fun get_files/1,
    fun check_arity_in_order/1,
    fun check_side_effect/1
    ].

%%% @private
transform(#refst{morefiles = MoreFiles, funcalls = FunC,
                 funclauses = FunD, neworder = Order}) ->
     try
       make_new_order(FunD, Order, pattern),
       ?ESG:close(),
       make_new_order(FunC, Order, sub),
       ?ESG:close()
    catch
       _:_ -> ?UI:message(error,"Bad options...",[])
    end,
    {MoreFiles, ok}.

%%% ============================================================================
%%% Implementation

%% @spec order_list(State::refst()) -> refst()
%% @throws string()
%%
%% @doc Converts the order list to erlang list format. The type of the
%% incoming list is string and the possible forms are may:
%% "2,1" or "(2,1)" or "[2,1]".
%% The following field of the `State' is set: `neworder'.
order_list(State = #refst{order = Order})->
    case convert_order_list(Order) of
         {ok, OrderList} -> State#refst{neworder = OrderList};
         _ -> throw("The order list is wrong, try again!")
    end.


%% @spec pos_token_data(State::refst()) -> refst()
%% @throws string()
%%
%% @doc Sets the following fields of the `State': `funnode', `funname',
%% `arity', `filenode'.
pos_token_data(State = #refst{modname = ModName,
                              position = Pos,
                              modnode = ModNode})->
    {file, File} = ?SYNTAX:file(ModName),
    case
      is_fundef(File, Pos) of
      {func, FunName, Arity} ->
             [FunNode] = ?ESG:path(ModNode,
               [{func, {{name, '==', FunName}, 'and',
                                {arity, '==', Arity}}}]),
                      %anal_function:function(FunName, Arity)),
          State#refst{funnode  = FunNode,
                      funname  = FunName,
                      arity = Arity,
                      filenode = File};
      _ -> throw("No any function definition at the marked position")
    end.

%% @spec get_module_node(State::refst()) -> refst()
%% @throws string()
%%
%% @doc Finds the node of the used modul and sets the `modnode' filed
%% of the `State' by using `modname' which field contains the name of
%% the module.
get_module_node(State = #refst{modname = ModName})->
   try
    RootNode       = ?ESG:root(),
    [FileNode] = ?ESG:path(RootNode, [{file, {path, '==', ModName}}]),
    [ModNode]  = ?ESG:path(FileNode,[moddef]),
    State#refst{modnode = ModNode}
   catch
    _:_-> throw("No module found")
   end;
get_module_node({State = #refst{filenode = FileNode},direct})->
    case ?GRAPH:path(FileNode, [moddef]) of
         [ModNode] -> State#refst{modnode = ModNode};
         _ -> throw("No module found")
    end.

%% @spec get_clauses(State::refst()) -> refst()
%%
%% @doc Finds the all clauses of the marked function from the graph to
%% make refactoring and sets the filed of the `State' named `funclauses'.
%% The function uses the fundef edges of the graph to find all clauses
%% of the marked function.
get_clauses(State = #refst{funnode = FunNode})->
   try
    FunClauses = ?ESG:path(FunNode,[{fundef, back}, funcl]),
    State#refst{funclauses = FunClauses}
   catch
     _:_-> throw("Error occured (in reorder funpar): 
              during collecting function clauses")
   end.

%% @spec get_calls(State::refst()) -> refst()
%%
%% @doc Finds the all occurrances where the function is used in the module
%% and sets the `funcalls' field of the `State'. These graph nodes are
%% accessible by using funref edges. The function not collects the references
%% of the functions from the export lists.
%% This function can collect the expanded implicit calls to make a new order.
get_calls(State = #refst{funnode = FunNode})->
   try
    FunCalls = ?ESG:path(FunNode,[{funref, back}]),
    FunCls = [FuncData || FuncData <- FunCalls,
              valid_call(?ESG:data(FuncData)) == valid],
    State#refst{funcalls = FunCls}
   catch
     _:_-> throw("Error (in reorder funpar): function calls")
   end.

%% @spec get_implicit_calls(State::refst()) -> refst()
%%
%% @doc Finds all implicit calls of the marked function (fun function/arity)
%% and sets the `implicit_calls' field of the `State'.
get_implicit_calls(State = #refst{funnode = FunNode})->
   try
    FunCalls = ?ESG:path(FunNode,[{funref, back}]),
    ImpCls = [FuncData || FuncData <- FunCalls,
              valid_call(?ESG:data(FuncData)) == implicit_fun],
    State#refst{implicit_calls = ImpCls}
   catch
     _:_-> throw("Error (in reorder funpar): implicit calls")
   end.

%% @spec get_files(State::refst()) -> refst()
%%
%% @doc Collets the files where the refactoring make changing something
%% to save and reload this files and set the `morefiles' filed of the
%% `State'. This field contains the original file too (which file contains
%% the marked function).
get_files(State = #refst{funcalls = FunCalls,
                         funclauses = FunClauses})->
   try
    MoreF = [files(FunCl) || FunCl <- FunCalls++FunClauses],
    MoreFiles = lists:usort(MoreF),
    State#refst{morefiles = MoreFiles}
   catch
     _:_-> throw("Error (in reorder funpar): get files")
   end.


extract_implicit_funcalls(State = #refst{implicit_calls = Imp})->
   try
    [extract(ImplFunExpr) || ImplFunExpr <- Imp],
    State
   catch
     _:_-> throw("Error (in reorder funpar):extract implicit calls")
   end.

%%% @private
extract(ImplFunExpr)->
    ?MANIP:expand_funexpr(ImplFunExpr),
    ?ESG:close(),
    ok.

%% @spec make_new_order(Clauses::list(),Order::list(),
%%                      Option::atom()) -> ok
%%
%% @doc This function is a top level transformation. Makes a new order in
%% the parameter list of the marked function and changes the order of the
%% parameter lists in the function usings.
make_new_order(Clauses, Order, Option)->
    Args = [{C, gimmi_chld({C,Option})} || C <- Clauses],
    %Args = [{C, ?ESG:children(C)} || C <- Clauses],
    [repeat_reorder(Cl, Arg , Order) || {Cl, Arg} <- Args],
    ok.

%%% ----------------------------------------------------------------------------
%%% Checks

%% @spec check_arity_in_order(State::refst()) -> refst()
%%
%% @doc Compares the arity of the marked function and the number of the
%% order list element. The `State' is not changed.
check_arity_in_order(State = #refst{neworder = NewOrder, arity = Arity})->
    ValidOrder = lists:usort(NewOrder),
    case lists:max(ValidOrder) > length(ValidOrder) of 
           true ->  throw("Bad number(position) in the list.");
	_ -> ok
    end,
    case lists:min(ValidOrder)< 1 of
        true -> throw("Bad number(position) in the list.");
	_ -> ok
    end,
    ENumber = length(ValidOrder),    
    case Arity == ENumber of
         true -> State;
            _ -> throw("The arity of the order list is bad.")
    end.


%% @spec check_side_effect(State::refst()) -> refst()
%%
%% @doc Try to collect side effects,
check_side_effect(State = #refst{})->
    %B = anal_sideeffect:is_true_dirty(FunSemanticalNode),
    State.

%%% ----------------------------------------------------------------------------
%%% Private

%% Reorders the nodes of a repeat construct.
repeat_reorder(Parent, RepNodes, Order) ->
    {{repeat, _TType, SLink, _Reps}, _TokenIdx} =
        containing_repeat(Parent, RepNodes),
    OrdNodes = [lists:nth(N, RepNodes) || N <- Order ],
    [ ?ESG:remove(Parent, SLink, Node) || Node <- RepNodes ],
    [ ?ESG:insert(Parent, SLink, Node) || Node <- OrdNodes ].

% Returns the repeat structure
% that contains all of the given nodes.
% Also returns the number of child tokens before the structure.
containing_repeat(Parent, Nodes) ->
    Str = ?SYNTAX:structure_with_nodes(Parent),
    containing_repeat(Str, Nodes, 1).

containing_repeat(Str, Nodes, PrevTokenCount) ->
    [Container] =
        [ StrElem
            ||  StrElem <- Str,
                lists:all(  fun(Node) ->
                                lists:member(Node, str_nodes(StrElem))
                            end, Nodes) ],
    Prev = lists:takewhile(fun(Node) -> Node /= Container end, Str),
    PrevTokens = [ token || {'$gn', lex, _} <- str_nodes(Prev) ],
    NewPrevTokenCount = PrevTokenCount + length(PrevTokens),
    case Container of
        {repeat, _, _, _}  -> {Container, NewPrevTokenCount};
        {optional, OptStr} -> containing_repeat(OptStr, Nodes, NewPrevTokenCount)
    end.

% Returns all of the nodes that are contained in the structure.
str_nodes(Nodes) when is_list(Nodes) -> lists:flatmap(fun str_nodes/1, Nodes);
str_nodes({token, _, Node})          -> [Node];
str_nodes({symbol, _, Node})         -> [Node];
str_nodes({optional, Structure})     -> 
                         lists:flatmap(fun str_nodes/1, Structure);
str_nodes({repeat, _, _, Nodes})     -> Nodes.

%funname([ActC])->
%    #func{name=Name, arity=Arity} = ?ESG:data(ActC),
%    {Name, Arity}.

%%% @private
files(FunCall)->
    case {?ESG:data(FunCall), ?ESG:data(element(2,hd(?ESG:parent(FunCall))))} of
        {#expr{}, #expr{}} ->
                [FCl] = ?ESG:path(FunCall,
                                       [sup, {visib,back},scope,functx]),
            [FNode] = ?ESG:path(FCl, [{funcl,back},{form,back}]);
        {#expr{}, #clause{}} ->
            [FNode] = ?GRAPH:path(FunCall, %%howto
                     [{visib,back},scope,functx,{funcl,back},{form,back}]);
        {#clause{}, _}->[FNode] = ?ESG:path(FunCall,
                            [{funcl,back},{form,back}])
    end,
    FNode.

%%% @private
fun_data_by_pos(Fname, Pos) ->
    case is_fundef(Fname, Pos) of
       {func, Name, Arity}->
            {Name, Arity};
       _ -> {no_fun_at, Pos}
    end.

%%% @private
valid_call(#expr{kind=Kind}) ->
    case Kind of
        application ->
            valid;
       implicit_fun ->
            implicit_fun;
                  _ -> bad
    end.
%%% @private
convert_order_list(Order)->
  try
    Stripped1    = string:strip(Order,both,$(),
    Stripped2    = string:strip(Stripped1,both,$)),
    Stripped3    = string:strip(Stripped2,both,$[),
    Stripped4    = string:strip(Stripped3,both,$]),
    {_, Cleaned} = regexp:split(Stripped4," "),
    Joined    = string:join(Cleaned,""),
    Result    = string:tokens(Joined,","),
    Final     = [typer(Token) || Token <- Result],
    {ok, Final}
   catch
     _:_-> bad_value
   end.

%%% @private
typer({Type, Data})->
    case Type of
         integer -> {Res,_} = string:to_integer(Data);
         float   -> {Res,_} = string:to_float(Data);
         _       -> Res = Data
    end,
    case Res of
       error -> can_not_convert;
           _ -> Res
    end;
typer(Data) ->
    {Res,_} = string:to_integer(Data),
    case Res of
       error -> can_not_convert;
           _ -> Res
    end.

%%% @private
file_node(File) when is_list(File)->
   {file, Fnode} = ?SYNTAX:file(File),
   Fnode;
file_node(File) ->
   File.

%%% @private
is_fundef(Fname, Pos) ->
  try
    File = file_node(Fname),
    {ok, Token} = ?LEX:token_by_pos(File,Pos),
      case  ?ESG:data(Token) of
          #lex{type=token,data=#token{type=atom}}->
          %{_,_,{_,atom,_,_,_,_}}
            [Par]    = ?LEX:token_parent(Token),
            [{_,GPar}] = ?ESG:parent(Par),
            case {?ESG:data(Par), ?ESG:data(GPar)} of
              {#expr{},#clause{}}-> %%howto
              %{{expr,_,_,_},{clause,funcl,_}}
                   [FunDef] = ?ESG:path(GPar,[{funcl,back},fundef]),
                  case ?ESG:data(FunDef) of
                       #func{name=Name, arity=Arity} ->
                            {func, Name, Arity};
                      _-> no
                   end;
               _-> no
            end;
      _-> no
    end
   catch
     _:_ -> throw("No any token at the marked position.")
   end.

%%% @private
%% REM DIAL no call uses the third case `_'
gimmi_chld({Clause, Option})->
   case Option of
     pattern -> ?ESG:path(Clause,[Option]);
         sub -> tl(?ESG:path(Clause,[Option]));
         _   -> []
   end.

