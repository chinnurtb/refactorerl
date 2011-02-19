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

%%% @author Lilla Hajós <lya@elte.hu>

-module(referl_sq_lib).
-vsn("$Rev: 3682 $").

-include("refactorerl.hrl").

-record(entity, {name, selectors, properties}).
-record(selector, {name, type, func}).
-record(property, {name, type, func}).

-export([entity/1, sel_fun/2, prop_fun/2, sel_type/2, prop_type/2]).
-export([init_sel/2]).
-export([error_text/2]).


error_text(illegal_initial_selector, Param) -> 
    io_lib:format("illegal selector: ~p  ", Param);
error_text(illegal_property, Params) ->
    io_lib:format("illegal ~p property: ~p", Params);
error_text(illegal_selector, Params) ->
    io_lib:format("illegal ~p selector: ~p  ", Params).


%% TODO: Expressions should be given by range too.
init_sel(Params,  '@fun')      -> {function,   [?Args:function(Params)]};
init_sel(Params,  '@var')      -> {variable,   [?Args:variable(Params)]};
init_sel(Params,  '@macro')    -> {macro,      [?Args:macro(Params)]};
init_sel(Params,  '@rec')      -> {record,     [?Args:record(Params)]};
init_sel(Params,  '@file')     -> {file,       [?Args:file(Params)]};
init_sel(Params,  '@expr')     -> {expression, [?Args:expression(Params)]};
init_sel(Params,  '@recfield') -> {field,      [?Args:record_field(Params)]};
init_sel(Params,  '@mod')      -> {file,       [?Args:module(Params)]};
init_sel(_Params, files)       -> {file,       ?Query:exec([file])};
init_sel(_Params, mods)        -> {file,       ?Query:exec([module])};
init_sel(Params,  '@def') ->
    FilePath = proplists:get_value(file, Params),
    Pos = proplists:get_value(position, Params),

    [Token] = ?Query:exec(?Query:seq(?File:find(FilePath),?File:token(Pos))),
    Mac = ?Query:exec(Token, ?Query:any([{llex,back},mref],
                                        [{{flex,back}, {type, '==', macro}}])),
    Ent = case Mac of
              []  ->
                  [Expr] = ?Query:exec(Token, ?Token:expr()),
                  ?Query:exec(
                     Expr,
                     ?Query:any(
                        [ ?Expr:variables(),
                          [fielddef],
                          [fieldref],
                          [{attr,back}, recdef],
                          ?Query:seq(?Expr:parent(), [recref]),
                          [modref],
                          ?Query:seq([?Expr:nameof(),
                                      ?Clause:form(), ?Form:func()]),
                          ?Query:seq(?Expr:parent(), ?Expr:function()),
                          ?Query:seq([?Expr:parent(),
                                      ?Expr:parent(), ?Expr:function()]) ]));
              Mac -> Mac
          end,
    Type = case Ent of
               []     -> none;
               [Node] -> case ?Syn:node_type(Node) of
                             form     -> macro;
                             variable -> variable;
                             field    -> field;
                             record   -> record;
                             module   -> file;
                             func     -> function
                         end
           end,
    {Type, Ent};
init_sel(_Params, Selector) ->
    throw(?LocalError(illegal_initial_selector, [Selector])).


entites() ->

%%% ============================================================================
%%% File entity

    [#entity{
        name = file,
        selectors =
        [#selector{
            name = funs,
            type = function,
            func = fun(File) -> ?Query:exec(mod(File), ?Mod:locals()) end},

         #selector{
            name = records,
            type = record,
            func = fun(File) -> ?Query:exec(file(File), ?File:records()) end},

         #selector{
            name = macros,
            type = macro,
            func = fun(File) -> ?Query:exec(file(File), ?File:macros()) end},

         #selector{
            name = includes,
            type = file,
            func = fun(File) ->
                           FileNode = file(File),
                           ?Query:exec(FileNode, ?File:includes()) -- [FileNode]
                   end},

         #selector{
            name = included_by,
            type = file,
            func = fun(File) ->
                           FileNode = file(File),
                           ?Query:exec(FileNode, ?File:included()) -- [FileNode]
                   end},

         #selector{
            name = imports,
            type = function,
            func = fun(File) -> ?Query:exec(mod(File), [funimp]) end},

         #selector{
            name = exports,
            type = function ,
            func = fun(File) -> ?Query:exec(mod(File), [funexp]) end}
        ],

%%% ----------------------------------------------------------------------------
%%% File properties

        properties =
        [#property{
            name = module,
            type = bool,
            func = fun file_is_module/1},

         #property{
            name = header,
            type = bool,
            func = fun(File) -> not file_is_module(File) end},

         #property{
            name = name,
            type = string,
            func = fun(File) -> file_prop(name, File) end},

         #property{
            name = dir,
            type = string,
            func = fun(File) -> file_prop(dir, File) end},

         #property{
            name = path,
            type = string,
            func = fun(File) -> file_prop(path, File)  end}
        ]},

%%% ============================================================================
%%% Function entity

     #entity{
        name = function,
        selectors =
        [#selector{
            name = refs,
            type = expression,
            func = fun(Fun) ->
                        ?Query:exec(Fun, ?Query:all([?Fun:applications(),
                                                     ?Fun:implicits(),
                                                     ?Fun:impexps()]))
                   end},

         #selector{
            name = calls,
            type = function,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:body(),
                                                        ?Expr:funapps()]))
                   end},

         #selector{
            name = called_by,
            type = function,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Query:all(
                                                           ?Fun:applications(),
                                                           ?Fun:implicits()),
                                                        ?Expr:clause(),
                                                        ?Clause:form(),
                                                        ?Form:func()]))
                   end},

         #selector{
            name = args,
            type = expression,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:patterns()]))
                   end},

         #selector{
            name = body,
            type = expression,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:body()]))
                   end},

         #selector{
            name = exprs,
            type = expression,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:exprs()]))
                   end},

         #selector{
            name = vars,
            type = variable,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:variables()]))
                   end},

         #selector{
            name = file,
            type = file,
            func = fun(Fun) -> ?Query:exec(Fun, ?Fun:module()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Function properties

        properties =
        [#property{
            name = exported,
            type = bool,
            func = fun(Fun) ->
                           case ?Query:exec(Fun, ?Fun:definition()) of
                               [] -> true;
                               _  ->  ?Fun:exported(Fun)
                           end
                   end},

         #property{
            name = name,
            type = atom,
            func = fun ?Fun:name/1},

         #property{
            name = arity,
            type = int,
            func = fun ?Fun:arity/1},

         #property{
            name = bif,
            type = bool,
            func = fun(Fun) ->
                           ?Fun:autoimported(?Fun:name(Fun), ?Fun:arity(Fun))
                   end},

         #property{
            name = pure,
            type = bool,
            func = fun(Fun) -> not(?Fun:dirty(Fun)) end},

         #property{
            name = defined,
            type = bool,
            func = fun(Fun) -> ?Query:exec(Fun, ?Fun:definition()) /= [] end},

         #property{
            name = module,
            type = atom,
            func = fun(Fun) ->
                           ?Mod:name(hd(?Query:exec(Fun, ?Fun:module())))
                   end}
        ]},

%%% ============================================================================
%%% Variable entity

     #entity{
        name = variable,
        selectors =
        [#selector{
            name = refs,
            type = expression,
            func = fun(Var) -> ?Query:exec(Var, ?Var:references()) end},

         #selector{
            name = bindings,
            type = expression,
            func = fun(Var) -> ?Query:exec(Var, ?Var:bindings()) end},

         #selector{
            name = fundef,
            type = function,
            func = fun(Var) -> ?Query:exec(Var, ?Query:seq([[{vardef, back}],
                                                            ?Clause:form(),
                                                            ?Form:func()]))
                   end},

         #selector{
            name = visib,
            type = expression,
            func = undef}
%         #selector{name = file, type = file, func = undef}
        ],

%%% ----------------------------------------------------------------------------
%%% Variable properties

        properties =
        [#property{
            name = name,
            type = string,
            func = fun ?Var:name/1}
        ]},

%%% ============================================================================
%%% Record entity

     #entity{
        name = record,
        selectors =
        [#selector{
            name = refs,
            type = expression,
            func = fun(Rec) -> ?Query:exec(Rec, [{recref,back}]) end},

         #selector{
            name = fields,
            type = field,
            func = fun(Rec) -> ?Query:exec(Rec, [field]) end},

         #selector{
            name = file,
            type = file,
            func = fun(Rec) -> ?Query:exec(Rec, ?Rec:file()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Record properties

        properties =
        [#property{
            name = name,
            type = string,
            func = fun ?Rec:name/1}
        ]},


%%% ============================================================================
%%% Record field entity
     #entity{
        name = field,
        selectors =
        [#selector{
            name = refs,
            type = expression,
            func = fun(Field) -> ?Query:exec(Field, [{fieldref, back}]) end},

         #selector{
            name = record,
            type = record,
            func = fun(Field) -> ?Query:exec(Field, [{field, back}]) end},

         #selector{
            name = file,
            type = file,
            func = fun(Field) -> ?Query:exec(Field, [{field, back},
                                                     {record, back}]) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Record field properties

        properties =
        [#property{
            name = name,
            type = atom,
            func = fun(Field) -> (?Graph:data(Field))#field.name end}
        ]},

%%% ============================================================================
%%% Macro entity

     #entity{
        name = macro,
        selectors =
        [#selector{
            name = refs,
            type = expression,
            func = fun(Macro) ->
                           Subts = ?Query:exec(Macro, [{mref, back}]),
                           Fun = fun(S) -> 
                                         Exprs = ?Query:exec(S, [{llex, back},
                                                                 {elex, back}]),
                                         last_common_node(
                                           ?Syn:root_path(hd(Exprs)),
                                           ?Syn:root_path(lists:last(Exprs))
                                          )
                                 end,
                           lists:filter(fun ?Expr:is_expr/1,
                                        lists:map(Fun, Subts))
                   end},
         #selector{
            name = file,
            type = file,
            func = fun(Macro) -> ?Query:exec(Macro, ?Macro:file()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Macro properties

        properties =
        [#property{
            name = name,
            type = string,
            func = fun ?Macro:name/1},

         #property{
            name = arity,
            type = int,
            func = fun(Macro) ->
                           HasVar =
                               fun(X) ->
                                       Token = ((?Graph:data(X))#lex.data),
                                       Token#token.type == variable
                               end,
                           case ?Query:exec(Macro,
                                            [{flex, {type, '==', arg}}]) of
                               [] ->
                                   0;
                               Args ->
                                   length(
                                     lists:filter(
                                       HasVar,
                                       ?Query:exec(Args, [llex])))
                           end
                   end},

         #property{
            name = const,
            type = bool,
            func = fun(Macro) ->
                        ?Query:exec(Macro, [{flex, {type, '==', arg}}]) == []
                   end}
        ]},

%%% ============================================================================
%%% Expression entity

     #entity{
        name = expression,
        selectors =
        [#selector{
            name = fundef,
            type = function,
            func = fun(Expr) -> ?Query:exec(Expr, ?Query:seq([?Expr:clause(),
                                                              ?Clause:form(),
                                                              ?Form:func()]))
                   end},

         #selector{
            name = funs,
            type = function,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:functions()) end},

         #selector{
            name = vars,
            type = variable,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:variables()) end},

         #selector{
            name = records,
            type = record,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:records()) end},

         #selector{
            name = macros,
            type = macro,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:macros()) end},

         #selector{
            name = sub,
            type = expression,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:deep_sub()) end},

         #selector{
            name = param,
            type = expression,
            func = fun(Expr) -> case ?Expr:kind(Expr) of
                                    application ->
                                        [_|Xs] = ?Query:exec(Expr, [sub]),
                                        Xs;
                                    match_expr ->
                                        ?Query:exec(Expr, [{sub, 2}]);
                                    cons ->
                                        ?Query:exec(Expr, 
                                                    ?Query:all([{sub, 1}, sub], 
                                                    [{sub, 2}]));
                                    infix_expr ->
                                        ?Query:exec(Expr, ?Query:any([sub], 
                                                                     [exprcl, 
                                                                      body]));
                                    _ ->
                                        ?Query:exec(Expr, ?Expr:children()) 
                                end
                   end},

         #selector{
            name = top,
            type = expression,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:sup()) end},

         #selector{
            name = reach,
            type = expression,
            func = dataflow_reach([])},

         #selector{
            name = origin,
            type = expression,
            func = dataflow_reach([back])},

         #selector{
            name = file,
            type = file,
            func = fun(Expr) -> ?Query:exec(Expr, ?Query:seq([?Expr:clause(),
                                                              ?Clause:form(),
                                                              ?Form:file()]))
                   end}
        ],

%%% ----------------------------------------------------------------------------
%%% Expression properties

        properties =
        [#property{
            name = type,
            type = atom,
            func = fun(Expr) ->
                           case ?Expr:kind(Expr) of
                               prefix_expr -> ?Expr:value(Expr);
                               infix_expr  -> ?Expr:value(Expr);
                               Kind        -> Kind
                           end
                   end},

         #property{
            name = value,
            type = any,
            func = fun ?Expr:value/1},

         #property{
            name = class,
            type = atom,
            func = fun ?Expr:type/1},

         #property{
            name = last,
            type = bool,
            func = fun is_last_expr/1},

         #property{
            name = index,
            type = int,
            func = fun(Expr) ->
                           lists:foldl(
                             fun(Tag, none) ->
                                     case ?ESG:path(Expr, [{Tag, back}]) of
                                         [Parent] ->
                                             ?ESG:index(Parent, Tag, Expr);
                                         [] ->
                                             none
                                     end;
                                (_, Index) -> Index
                             end,
                             none,
                             [pattern, body, sub])
                   end},

         #property{
            name = tailcall,
            type = bool,
            func = fun(Expr) ->
                           is_last_expr(Expr) andalso
                               ?Expr:kind(Expr) == application
                   end}
        ]}
    ].


%%% ============================================================================
%%% Helper functions

%% @todo this is an extremely inefficient variant, recalculates the static
%% data flow graph for every single input node
dataflow_reach(Opt) ->
    fun (Expr) ->
            DG = referl_dataflow:new(),
            referl_dataflow:add(DG, ?Query:exec(?Mod:all())),
            referl_dataflow:reach(DG, [Expr], Opt)
    end.


%% @type entity().

%% @spec entity(Type::atom()) -> entity()
%% @doc Returns the entity determined by `Type'.
entity(Type) ->
    case lists:keysearch(Type, 2, entites()) of
        false ->
            throw(io_lib:format("illegal entity: ~p~n", [Type]));
        {value, Rec} ->
            Rec
    end.

%% @type node() = referl_graph:node().

%% @spec sel_fun(entity(), atom()) -> Fun
%%       Fun = (node()) -> [node()]
%% @doc Returns a function that computes the nodes resulting by the use of the
%%      given selector.
sel_fun(Entity, Selector) ->
    func(Entity#entity.name, Selector, Entity#entity.selectors, selector).

%% @spec prop_fun(entity(), atom()) -> Fun
%%       Fun = (node()) -> atom()
%% @doc Returns a function that computes the value of the given property.
prop_fun(Entity, Property) ->
    func(Entity#entity.name, Property, Entity#entity.properties, property).

%% todo: record syntax?
%% (won't need undef after the module is fully implemented)
func(Type, SelOrFilt, RecList, PropOrSel) ->
    case lists:keysearch(SelOrFilt, 2, RecList) of
        false -> false;
        {value, Rec} ->
            case element(4, Rec) of
                undef ->
                    Msg = io_lib:format(
                            "the ~p ~p ~p has not been implemented yet~n",
                            [Type, PropOrSel, SelOrFilt]),
                    throw(Msg);
                Fun -> Fun
            end
    end.

%% @spec sel_type(entity(), atom()) -> atom()
%% @doc The result is the type of the entities you'll get after the use of the
%% given selector.
sel_type(Entity, Selector) ->  
    Sels = Entity#entity.selectors,
    case lists:keysearch(Selector, 2, Sels) of
        false -> throw(?LocalError(illegal_selector,
                                   [Entity#entity.name, Selector]));
        {value, Sel} ->  Sel#selector.type
    end.

%% @spec prop_type(entity(), atom()) -> atom()
%% @doc The result is the type of the property.
prop_type(Entity, Property) ->  
    Props = Entity#entity.properties,
    case lists:keysearch(Property, 2, Props) of
        false -> throw(?LocalError(illegal_property,
                                   [Entity#entity.name, Property]));
        {value, Prop} -> Prop#property.type
    end.

%%% ============================================================================

file_is_module(File) ->
    case ?Syn:node_type(File) of
        module -> true;
        file -> ?File:type(File) == module
    end.


file_prop(Prop, File) ->
    case ?Syn:node_type(File) of
        module ->
            io_lib:write_atom(?Mod:name(File));
        file ->
            Path = ?File:path(File),
            case Prop of
                path -> Path;
                name -> string:substr(Path, string:rstr(Path, "/")+1);
                dir -> string:substr(Path, 1, string:rstr(Path, "/"))
            end
    end.


mod(Node) ->
    case ?Syn:node_type(Node) of
        file -> ?Query:exec(Node, ?File:module());
        module -> Node
    end.

file(Node) ->
    case ?Syn:node_type(Node) of
        file -> Node;
        module -> ?Query:exec(Node, ?Mod:file())
    end.

is_last_expr(Expr) ->
    Last = ?Query:exec(Expr, ?Query:seq([?Expr:clause(), [{visib, last}]])),
    Top = ?Query:exec(Expr, ?Expr:sup()),
%    ?Expr:is_same_expr({Last, Top}).
    Last == Top.

last_common_node([{_L, N} = H|T1], [H|T2]) -> last_common_node(T1, T2, N);
last_common_node(_,_) -> ?Graph:root().

last_common_node([{_L, N} = H|T1], [H|T2], _) -> last_common_node(T1, T2, N);
last_common_node(_, _, Node) -> Node.