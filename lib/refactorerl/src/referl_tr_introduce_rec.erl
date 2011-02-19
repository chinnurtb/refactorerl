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
%%% <li>A tuple (see {@link referl_args:expr_range/1}).</li>
%%% <li>A record name (see {@link referl_args:name/1}).</li>
%%% <li>Record filed names separated by whitespace
%%%   (see {@link referl_args:string/1}).</li>
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
%%% <li> If the tuple was the head of a case clause, find the head of the
%%%   case and insert a function expression around it to convert it from a
%%%   tuple to a record.</li>
%%% <li> If the tuple is a RHS of a match expression and the LHS is a
%%%   variable, then at each occurence of the variable insert a function
%%%   expression wich convert back the record to a tuple.</li>
%%% </ol>

%%% == Implementation status ==
%%% This refactoring is fully implemented.

%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>

-module(referl_tr_introduce_rec).
-vsn("$Rev: 3185 $").
-include("refactorerl.hrl").

%% Callbacks
-export([prepare/1, error_text/2]).

-import(lists, [zipwith/3, flatten/1, any/2]).

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
         
         #clause{kind=fundef} ->
             ?Check(?Query:exec(Parent, 
                                ?Query:seq([?Clause:form(), ?Form:func(),
                                            ?Fun:implicits()])) == [],
                    ?LocalError(implicit, [])),
             Sites = funcl_sites(Parent, Expr),
             transform_pattern(Recname, Recfields, Expr, Sites);
         
         #clause{kind=pattern} ->
             Sites = casecl_sites(Parent),
             transform_pattern(Recname, Recfields, Expr, Sites);

         #expr{type=pattern, kind=match_expr} -> 
             [{_, Cl}] = ?Syn:parent(Parent),
             ?Check(?Query:exec(Parent, 
                                ?Query:seq([?Expr:clause(), ?Clause:form(),
                                            ?Form:func(), ?Fun:implicits()])) == [],
                    ?LocalError(implicit, [])),
             Sites = funcl_sites(Cl, Parent),
             transform_pattern(Recname, Recfields, Expr, Sites);
         
         #expr{type=expr, kind=match_expr} -> 
             Sites = match_sites(Parent),
             transform_expr(Recname, Recfields, Expr, Sites);
         
         _ -> throw(?LocalError(illegal_selection, []))
     end].


transform_pattern(Recname, Recfields, Expr, Sites) ->
    Exprs = ?Query:exec(Expr, ?Expr:children()),
    fun(_) ->
            Record = make_record(Recname, Recfields, Exprs),
            Fun = make_funexpr(Expr, Record),
            ?Transform:touch(Expr),
            replace(Expr, Record),
            [begin
                 ?Transform:touch(Site),
                 replace(Site, make_app(copy(Fun), Site))
             end || Site <- Sites]
    end.


transform_expr(Recname, Recfields, Expr, Sites) ->
    Exprs = ?Query:exec(Expr, ?Expr:children()),
    fun(_) ->
            Record = make_record(Recname, Recfields, Exprs),
            Fun = make_funexpr(Record, Expr),
            ?Transform:touch(Expr),
            replace(Expr, Record),
            [replace(Site, make_app(copy(Fun), Site)) || Site <- Sites]
    end.


%% @spec casecl_sites(node()) -> [node()] 

%% @doc Finds the sites to be modified when the tuple is a pattern in
%%      a case clause. It returns a singleton list containing the head
%%      clause.
casecl_sites(CaseCl) ->
    ?Query:exec(CaseCl, [{exprcl, back}, headcl, body]).


%% @spec funcl_sites(node(), node()) -> [node()]

%% @doc Finds the calling sites when the tuple is a parameter of a
%%      function. It returns the corresponding argument node.
funcl_sites(FunCl, Param) ->
    Apps = ?Query:exec(FunCl, ?Query:seq([?Clause:form(), ?Form:func(),
                                          ?Fun:applications()])),
    Index = ?Syn:index(FunCl, pattern, Param),
    flatten([?Query:exec(E, [{sub, Index+1}]) || E <- Apps]).


%% @spec match_sites(node()) -> [node()]

%% @doc Finds all occurences when the tuple is bound to a variable.
match_sites(Match) ->
    [Var, _Expr] = ?Query:exec(Match, ?Expr:children()),
    ?Query:exec(Var, ?Query:seq(?Expr:varbinds(), ?Var:references())).


%% @spec make_app(Fun::node(), Arg::node()) -> node()

%% @doc Creates an application node. Does not copy!
make_app(Fun, Arg) ->
    ?Syn:create(#expr{kind=application}, [{sub, [enclose(Fun), Arg]}]).


get_skeleton(Exprs) ->
    ?Check(length(Exprs) == 1, ?LocalError(not_tuple, [])),    
    [Expr] = Exprs,
    ?Check(?Expr:kind(Expr) == tuple, ?LocalError(not_tuple, [])),
    Expr.


split_fields(S) ->
    [list_to_atom(A) || A <- string:tokens(S, " ")].


%% @spec make_record(atom(), [atom()], [node()]) -> node()

%% @doc Creates a record expression. Copies the expressions.
make_record(RName, RFields, Exprs) ->
    RFNames = 
        [?Syn:create(#expr{kind=atom}, [io_lib:write(V)])
         || V <- RFields],
    RFNodes = 
        zipwith(fun(A,B) -> 
                        ?Syn:create(#expr{kind=record_field},
                                    [{sub, [A, copy(B)]}])
                end, RFNames, Exprs),
    RNNode = ?Syn:create(#expr{kind=atom}, [io_lib:write(RName)]),
    ?Syn:create(#expr{kind=record_expr}, [{sub, [RNNode | RFNodes]}]).


%% @spec make_funexpr(P::node(), B::node()) -> node()

%% @doc Creates a fun expr with pattern `P' and body `B'. Copies the
%%      arguments.
make_funexpr(Pattern, Body) ->
    Clause1 =
        ?Syn:create(#clause{kind=funexpr},
                    [{pattern, copy(Pattern)}, {body, copy(Body)}]),
    Clause2 =
        ?Syn:create(#clause{kind=funexpr},
                    [{pattern, ?Syn:create(#expr{kind=variable}, ["X"])},
                     {body,    ?Syn:create(#expr{kind=variable}, ["X"])}]),
    ?Syn:create(#expr{kind=fun_expr}, [{exprcl, [Clause1, Clause2]}]).


%% @spec define_record(atom(), [atom()]) -> form()

%% @doc Creates a record definition.
define_record(Name, Fields) ->
    RName   = ?Syn:create(#expr{kind=atom}, [io_lib:write(Name)]),
    RFields = ?Syn:create(#expr{kind=tuple},
                          [{sub, [?Syn:create(#expr{kind=atom},
                                              [io_lib:write(F)])
                                  || F <- Fields]}]),
    ?Syn:create(#form{type=attrib, tag=record},
                ["-", "record", {attr, [RName,RFields]}]).

copy(Node) ->
    proplists:get_value(Node, ?Syn:copy(Node)).

enclose(Node) ->
    ?Syn:create(#expr{kind=parenthesis}, [{sub, Node}]).

replace(From, To) ->
    [{_, Parent}] = ?Syn:parent(From),
    ?Syn:replace(Parent, {node, From}, [To]).

