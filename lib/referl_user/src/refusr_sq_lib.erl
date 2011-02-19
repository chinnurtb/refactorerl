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

-module(refusr_sq_lib).
-vsn("$Rev: 5088 $ ").

-include("user.hrl").

-define(Metrics,refusr_metrics).

-record(entity, {name, selectors, properties}).
-record(selector, {name, type, func}).
-record(property, {name, type, func}).

-export([sel_fun/2, prop_fun/2, sel_type/2, prop_type/2]).
-export([init_sel/2]).
-export([error_text/2]).


error_text(illegal_entity, Params) ->
    io_lib:format("illegal entity: ~p", Params);
error_text(illegal_initial_selector, Param) ->
    io_lib:format("illegal selector: ~p  ", Param);
error_text(illegal_property, Params) ->
    io_lib:format("illegal ~p property: ~p", Params);
error_text(illegal_selector, Params) ->
    io_lib:format("illegal ~p selector: ~p  ", Params).

%% @spec init_sel(Params::proplist(), SelectorName::atom()) ->
%%               {Type::atom(), [entity()]}
%% @doc Returns a list of entities and their type.
init_sel(Params,  '@fun')      -> {function,   [?Args:function(Params)]};
init_sel(Params,  '@var')      -> {variable,   [?Args:variable(Params)]};
init_sel(Params,  '@macro')    -> {macro,      [?Args:macro(Params)]};
init_sel(Params,  '@rec')      -> {record,     [?Args:record(Params)]};
init_sel(Params,  '@file')     -> {file,       [?Args:file(Params)]};
init_sel(Params,  '@expr')     -> {expression, [?Args:expression(Params)]};
init_sel(Params,  '@recfield') -> {field,      [?Args:record_field(Params)]};
init_sel(Params,  '@mod')      -> {file,       [?Args:module(Params)]};
init_sel(_Params, files)       -> {file,       ?Query:exec([file])};
init_sel(_Params, mods)        -> {file,       ?Query:exec(?Mod:all())};
init_sel(Params,  '@def') ->
    FilePath = proplists:get_value(file, Params),
    Pos = proplists:get_value(position, Params),

    [Token] = ?Query:exec(?Query:seq(?File:find(FilePath),?File:token(Pos))),
    Entity =
        ?Query:exec(
           Token,
           ?Query:any(
              [ [{llex,back}, mref], %macro ref
                ?Query:seq(?Token:expr(), ?Expr:variables()), %var
                ?Query:seq(?Token:expr(), ?Expr:field()), %field ref
                ?Query:seq(?Token:expr(), ?Expr:record()), %recref
                ?Query:seq(?Token:typexp(), ?Expr:fielddef()), %field def
                ?Query:seq(?Token:expr(), ?Expr:module()), %modq
                ?Query:seq([?Token:expr(), ?Expr:parent(), ?Expr:function()]),
                ?Query:seq([?Token:expr(), ?Expr:parent(),
                            ?Expr:parent(), ?Expr:function()]), %funref infix
                ?Query:seq([?Token:expr(), ?Expr:nameof(),
                            ?Clause:form(), ?Form:func()]), %fundef
                [{{flex,back}, {type, '==', macro}}], %macro form
                ?Query:seq(?Token:form(), ?Form:record()) ])), %rec form

    Type = case Entity of
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
    {Type, Entity};

init_sel(Params,  '@function')   -> init_sel(Params,  '@fun');
init_sel(Params,  '@variable')   -> init_sel(Params,  '@var');
init_sel(Params,  '@record')     -> init_sel(Params,  '@rec');
init_sel(Params,  '@field')      -> init_sel(Params,  '@recfield');
init_sel(Params,  '@expression') -> init_sel(Params,  '@expr');
init_sel(Params,  '@module')     -> init_sel(Params,  '@mod');
init_sel(Params,  '@definition') -> init_sel(Params,  '@def');
init_sel(_Params, modules)       -> init_sel(_Params, mods);

init_sel(_Params, Selector) ->
    throw(?LocalError(illegal_initial_selector, [Selector])).


entites() ->

%%% ============================================================================
%%% File entity

    [#entity{
        name = file,
        selectors =
        [#selector{
            name = [function, functions, 'fun', funs],
            type = function,
            func = fun(File) -> ?Query:exec(mod(File), ?Mod:locals()) end},

         #selector{
            name = [record, records, rec, recs],
            type = record,
            func = fun(File) -> ?Query:exec(file(File), ?File:records()) end},

         #selector{
            name = [macro, macros],
            type = macro,
            func = fun(File) -> ?Query:exec(file(File), ?File:macros()) end},

         #selector{
            name = [includes],
            type = file,
            func = fun(File) ->
                           FileNode = file(File),
                           ?Query:exec(FileNode, ?File:includes()) -- [FileNode]
                   end},

         #selector{
            name = [included_by],
            type = file,
            func = fun(File) ->
                           FileNode = file(File),
                           ?Query:exec(FileNode, ?File:included()) -- [FileNode]
                   end},

         #selector{
            name = [imports],
            type = function,
            func = fun(File) -> ?Query:exec(mod(File), ?Mod:imports()) end},

         #selector{
            name = [exports],
            type = function ,
            func = fun(File) -> ?Query:exec(mod(File), ?Mod:exports()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% File properties

        properties =
        [#property{
            name = [module, is_module, mod, is_mod],
            type = bool,
            func = fun file_is_module/1},

         #property{
            name = [header, is_header],
            type = bool,
            func = fun(File) -> not file_is_module(File) end},

         #property{
            name = [name],
            type = string,
            func = fun(File) -> file_prop(name, File) end},

         #property{
            name = [dir, directory],
            type = string,
            func = fun(File) -> file_prop(dir, File) end},

         #property{
            name = [path],
            type = string,
            func = fun(File) -> file_prop(path, File)  end},

%%%.............................................................................
%%% Metrics as properties
         #property{
            name = [module_sum, mod_sum],
            type = int,
            func = fun(File) -> file_metrics(module_sum, File, true) end},

         #property{
            name = [line_of_code, loc],
            type = int,
            func = fun(File) -> file_metrics(line_of_code, File, true) end},

         #property{
            name = [char_of_code, choc],
            type = int,
            func = fun(File) -> file_metrics(char_of_code, File, true) end},

         #property{
            name = [number_of_fun, num_of_fun,
                    num_of_functions, number_of_functions],
            type = int,
            func = fun(File) -> file_metrics(number_of_fun, File, false) end}, 

         #property{
            name = [number_of_macros, num_of_macros, num_of_macr] ,
            type = int,
            func = fun(File) -> file_metrics(number_of_macros, File, true) end},

         #property{
            name = [number_of_records, num_of_records, num_of_rec],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_records, File, true)
                   end},

         #property{
            name = [included_files, inc_files],
            type = int,
            func = fun(File) -> file_metrics(included_files, File, true) end},

         #property{
            name = [imported_modules, imp_modules, imported_mod, imp_mod],
            type = int,
            func = fun(File) -> file_metrics(imported_modules, File, true) end},

         #property{
            name = [number_of_funpath, number_of_funpaths,
                    num_of_funpath, num_of_funpaths],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_funpath, File, true)
                   end},

         #property{
            name = [function_calls_in, fun_calls_in],
            type = int,
            func = fun(File) ->
                           file_metrics(function_calls_in, File, true)
                   end},

         #property{
            name = [function_calls_out, fun_calls_out],
            type = int,
            func = fun(File) ->
                           file_metrics(function_calls_out, File, true)
                   end},

         #property{
            name = [cohesion, coh],
            type = int,
            func = fun(File) -> file_metrics(cohesion, File, true) end},

         #property{
            name = [otp_used, otp],
            type = int,
            func = fun(File) -> file_metrics(otp_used, File, true) end},

         #property{
            name = [max_application_depth, max_app_depth],
            type = int,
            func = fun(File) ->
                           file_metrics(max_application_depth, File, true)
                   end},

          #property{
            name = [max_depth_of_calling, max_depth_calling,
                    max_depth_of_call, max_depth_call],
            type = int,
            func = fun(File) ->
                           file_metrics(max_depth_of_calling, File, true)
                   end},

          #property{
            name = [min_depth_of_calling, min_depth_calling,
                    min_depth_of_call, min_depth_call],
            type = int,
            func = fun(File) ->
                           file_metrics(min_depth_of_calling, File, true)
                   end},

         #property{
            name = [max_depth_of_cases, max_depth_cases],
            type = int,
            func = fun(File) ->
                           file_metrics(max_depth_of_cases, File, true)
                   end},

         #property{
            name = [number_of_funclauses, num_of_funclauses,
                    number_of_funclaus, num_of_funclaus],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_funclauses, File, true)
                   end},

         #property{
            name = [branches_of_recursion, branches_of_rec,
                    branch_of_recursion, branch_of_rec],
            type = int,
            func = fun(File) ->
                           file_metrics(branches_of_recursion, File, true)
                   end},

         #property{
            name = [mcCabe, mccabe],
            type = int,
            func = fun(File) -> file_metrics(mcCabe, File, true) end},

         #property{
            name = [number_of_funexpr, num_of_funexpr],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_funexpr, File, true)
                   end},

         #property{
            name = [number_of_messpass],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_messpass, File, true)
                   end},

         #property{
            name = [fun_return_points, fun_return_point,
                    function_return_points, function_return_point],
            type = int,
            func = fun(File) ->
                           file_metrics(fun_return_points, File, true)
                   end},
       
         #property{
            name = [max_length_of_line],
            type = int,
            func = fun(File) ->
                           file_metrics(max_length_of_line, File, true)
                   end},

         #property{
            name = [average_length_of_line, avg_length_of_line],
            type = int,
            func = fun(File) ->
                           file_metrics(average_length_of_line, File, true)
                   end},

         #property{
            name = [no_space_after_comma],
            type = int,
            func = fun(File) ->
                           file_metrics(no_space_after_comma, File, true)
                   end}

        ]},

%%% ============================================================================
%%% Function entity

     #entity{
        name = function,
        selectors =
        [#selector{
            name = [refs, references, reference, ref],
            type = expression,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:all([?Fun:applications(),
                                                        ?Fun:implicits(),
                                                        ?Fun:impexps()]))
                   end},

         #selector{
            name = [calls],
            type = function,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:body(),
                                                        ?Expr:funapps()]))
                   end},

         #selector{
            name = [called_by],
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
            name = [args, arguments],
            type = expression,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:patterns()]))
                   end},

         #selector{
            name = [body],
            type = expression,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:body()]))
                   end},

         #selector{
            name = [exprs, expressions, expr, expression],
            type = expression,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:exprs()]))
                   end},

         #selector{
            name = [vars, var, variables, variable],
            type = variable,
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:variables()]))
                   end},

         #selector{
            name = [file],
            type = file,
            func = fun(Fun) -> ?Query:exec(Fun, ?Fun:module()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Function properties

        properties =
        [#property{
            name = [exported],
            type = bool,
            func = fun(Fun) ->
                           case ?Query:exec(Fun, ?Fun:definition()) of
                               [] -> true;
                               _  ->  ?Fun:is_exported(Fun)
                           end
                   end},

         #property{
            name = [name],
            type = atom,
            func = fun ?Fun:name/1},

         #property{
            name = [arity],
            type = int,
            func = fun ?Fun:arity/1},

         #property{
            name = [bif],
            type = bool,
            func = fun(Fun) ->
                           ?Fun:is_autoimported(?Fun:name(Fun), ?Fun:arity(Fun))
                   end},

         #property{
            name = [pure],
            type = bool,
            func = fun(Fun) -> not ?Fun:is_dirty(Fun) end},

         #property{
            name = [defined],
            type = bool,
            func = fun(Fun) -> ?Query:exec(Fun, ?Fun:definition()) /= [] end},

         #property{
            name = [dirty, is_dirty],
            type = bool,
            func = fun ?Fun:is_dirty/1},

         #property{
            name = [module, mod],
            type = atom,
            func = fun(Fun) ->
                           ?Mod:name(hd(?Query:exec(Fun, ?Fun:module())))
                   end},

%%%.............................................................................
%%% Metrics as properties

         #property{
            name = [line_of_code, loc],
            type = int,
            func = fun(Fun) -> fun_metrics(line_of_code, Fun) end},

         #property{
            name = [char_of_code, choc],
            type = int,
            func = fun(Fun) -> fun_metrics(char_of_code, Fun) end},

         #property{
            name = [function_sum, fun_sum],
            type = int,
            func = fun(Fun) -> fun_metrics(function_sum, Fun) end},

         #property{
            name = [max_application_depth, max_app_depth],
            type = int,
            func = fun(Fun) -> fun_metrics(max_application_depth, Fun) end},

          #property{
            name = [max_depth_of_calling, max_depth_calling,
                    max_depth_of_call, max_depth_call],
            type = int,
            func = fun(Fun) -> fun_metrics(max_depth_of_calling, Fun) end},

         #property{
            name = [max_depth_of_cases, max_depth_cases],
            type = int,
            func = fun(Fun) -> fun_metrics(max_depth_of_cases, Fun) end},

         #property{
            name = [number_of_funclauses, num_of_funclauses,
                    number_of_funclaus, num_of_funclaus],
            type = int,
            func = fun(Fun) -> fun_metrics(number_of_funclauses, Fun) end},

         #property{
            name = [branches_of_recursion, branches_of_rec,
                    branch_of_recursion, branch_of_rec],
            type = int,
            func = fun(Fun) -> fun_metrics(branches_of_recursion, Fun) end},

         #property{
            name = [mcCabe, mccabe],
            type = int,
            func = fun(Fun) -> fun_metrics(mcCabe, Fun) end},

         #property{
            name = [calls_for_function, calls_for_fun,
                    call_for_function, call_for_fun],
            type = int,
            func = fun(Fun) -> fun_metrics(calls_for_function, Fun) end},

         #property{
            name = [calls_from_function, calls_from_fun,
                   call_from_function, call_from_fun],
            type = int,
            func = fun(Fun) -> fun_metrics(calls_from_function, Fun) end},

         #property{
            name = [number_of_funexpr, num_of_funexpr],
            type = int,
            func = fun(Fun) -> fun_metrics(number_of_funexpr, Fun) end},

         #property{
            name = [number_of_messpass],
            type = int,
            func = fun(Fun) -> fun_metrics(number_of_messpass, Fun) end},

         #property{
            name = [fun_return_points, fun_return_point,
                    function_return_points, function_return_point],
            type = int,
            func = fun(Fun) -> fun_metrics(fun_return_points, Fun) end},
 
         #property{
            name = [max_length_of_line],
            type = int,
            func = fun(Fun) -> fun_metrics(max_length_of_line, Fun) end},

         #property{
            name = [average_length_of_line, avg_length_of_line],
            type = int,
            func = fun(Fun) -> fun_metrics(average_length_of_line, Fun) end},

         #property{
            name = [no_space_after_comma],
            type = int,
            func = fun(Fun) -> fun_metrics(no_space_after_comma, Fun) end},
        
         #property{
            name = [is_tail_recursive],
            type = int,
            func = fun(Fun) -> fun_metrics(is_tail_recursive, Fun) end}
          
        ]},

%%% ============================================================================
%%% Variable entity

     #entity{
        name = variable,
        selectors =
        [#selector{
            name = [refs, references, ref, reference],
            type = expression,
            func = fun(Var) -> ?Query:exec(Var, ?Var:references()) end},

         #selector{
            name = [bindings],
            type = expression,
            func = fun(Var) -> ?Query:exec(Var, ?Var:bindings()) end},

         #selector{
            name = [fundef],
            type = function,
            func = fun(Var) -> ?Query:exec(Var, ?Query:seq([?Var:clause(),
                                                            ?Clause:form(),
                                                            ?Form:func()]))
                   end}%,
% TODO
%         #selector{
%            name = visib,
%            type = expression,
%            func = undef}
%         #selector{name = file, type = file, func = undef}
        ],

%%% ----------------------------------------------------------------------------
%%% Variable properties

        properties =
        [#property{
            name = [name],
            type = string,
            func = fun ?Var:name/1}
        ]},

%%% ============================================================================
%%% Record entity

     #entity{
        name = record,
        selectors =
        [#selector{
            name = [refs, references, ref, reference],
            type = expression,
            func = fun(Rec) -> ?Query:exec(Rec, ?Rec:references()) end},

         #selector{
            name = [fields],
            type = field,
            func = fun(Rec) -> ?Query:exec(Rec, ?Rec:fields()) end},

         #selector{
            name = [file],
            type = file,
            func = fun(Rec) -> ?Query:exec(Rec, ?Rec:file()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Record properties

        properties =
        [#property{
            name = [name],
            type = string,
            func = fun ?Rec:name/1}
        ]},

%%% ============================================================================
%%% Record field entity
     #entity{
        name = field,
        selectors =
        [#selector{
            name = [refs, references, ref, reference],
            type = expression,
            func = fun(Field) ->
                           ?Query:exec(Field, ?RecField:references())
                   end},

         #selector{
            name = [record, rec],
            type = record,
            func = fun(Field) -> ?Query:exec(Field, ?RecField:recorddef()) end},

         #selector{
            name = [file],
            type = file,
            func = fun(Field) -> ?Query:exec(Field, ?RecField:file()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Record field properties

        properties =
        [#property{
            name = [name],
            type = atom,
            func = fun ?RecField:name/1}
        ]},

%%% ============================================================================
%%% Macro entity

     #entity{
        name = macro,
        selectors =
        [#selector{
            name = [refs, references, ref, reference],
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
            name = [file],
            type = file,
            func = fun(Macro) -> ?Query:exec(Macro, ?Macro:file()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Macro properties

        properties =
        [#property{
            name = [name],
            type = string,
            func = fun ?Macro:name/1},

         #property{
            name = [arity],
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
            name = [const],
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
            name = [fundef],
            type = function,
            func = fun(Expr) ->
                           ?Query:exec(Expr, ?Query:seq([?Expr:clause(),
                                                         ?Clause:form(),
                                                         ?Form:func()]))
                   end},

         #selector{
            name = [function, functions, 'fun', funs],
            type = function,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:functions()) end},

         #selector{
            name = [vars, variables, var, variable],
            type = variable,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:variables()) end},

         #selector{
            name = [record, records, rec, recs],
            type = record,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:records()) end},

         #selector{
            name = [macro, macros],
            type = macro,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:macros()) end},

         #selector{
            name = [sub, esub, subexpr, subexpression],
            type = expression,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:deep_sub()) end},

         #selector{
            name = [param, parameter],
            type = expression,
            func = fun(Expr) ->
                           case ?Expr:type(Expr) of
                               application ->
                                   [_|Xs] = ?Query:exec(Expr, ?Expr:children()),
                                   Xs;
                               match_expr ->
                                   ?Query:exec(Expr, ?Expr:child(2));
                               cons ->
                                   ?Query:exec(Expr,
                                               ?Query:all(
                                                  ?Query:seq(?Expr:child(1),
                                                             ?Expr:children()),
                                                  ?Expr:child(2)));
                               infix_expr ->
                                   ?Query:exec(Expr, ?Query:any(
                                                        ?Expr:children(),
                                                        [exprcl, body]));
                               _ ->
                                   ?Query:exec(Expr, ?Expr:children())
                           end
                   end},

         #selector{
            name = [top_expression, top, top_expr],
            type = expression,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:top()) end},

         #selector{
            name = [reach],
            type = expression,
            func = dataflow_reach([])},

         #selector{
            name = [origin],
            type = expression,
            func = dataflow_reach([back])},

         #selector{
            name = [file],
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
            name = [type],
            type = atom,
            func = fun(Expr) ->
                           case ?Expr:type(Expr) of
                               prefix_expr -> ?Expr:value(Expr);
                               infix_expr  -> ?Expr:value(Expr);
                               Kind        -> Kind
                           end
                   end},

         #property{
            name = [value, val],
            type = any,
            func = fun ?Expr:value/1},

         #property{
            name = [class],
            type = atom,
            func = fun ?Expr:role/1},

         #property{
            name = [last, is_last],
            type = bool,
            func = fun is_last_expr/1},

         #property{
            name = [index],
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
                             [pattern, body, esub])
                   end},

         #property{
            name = [has_side_effect],
            type = bool,
            func = fun ?Expr:has_side_effect/1},

         #property{
            name = [tailcall, is_tailcall],
            type = bool,
            func = fun(Expr) ->
                           is_last_expr(Expr) andalso
                               ?Expr:type(Expr) == application
                   end},

          #property{
            name = [tuple_repr_of_record, is_tuple_repr_of_record,
                   record_tuple, is_record_tuple],
            type = bool,
            func = fun(Expr) ->
                           ?Expr:type(Expr) == tuple andalso
                               has_record_definition(Expr)
                   end}
        ]}
    ].


%%% ============================================================================
%%% Helper functions

dataflow_reach(Opt) ->
    fun (Expr) ->
            ?Dataflow:reach([Expr], Opt)
    end.


%% @type entity().

%% @private
%% @spec entity(Type::atom()) -> entity()
%% @doc Returns the entity record determined by `Type'.
entity(Type) ->
    case lists:keyfind(Type, 2, entites()) of
        false -> throw(?LocalError(illegal_entity, [Type]));
        Rec   -> Rec
    end.

%% @type node() = refcore_graph:node().

%% @spec sel_fun(EntityType::atom(), SelectorName::atom()) -> [Fun]
%%       Fun = (node()) -> [node()]
%% @doc Returns a function that computes the nodes resulting by the use of the
%%      given selector.
sel_fun(EntityType, SelectorName) ->
    Entity = entity(EntityType),
    [Selector#selector.func ||
        Selector <- Entity#entity.selectors,
        lists:member(SelectorName, Selector#selector.name)].

%% @spec prop_fun(EntityType::atom(), PropertyName::atom()) -> [Fun]
%%       Fun = (node()) -> atom()
%% @doc Returns a function that computes the value of the given property.
prop_fun(EntityType, PropertyName) ->
    Entity = entity(EntityType),
    [Property#property.func ||
        Property <- Entity#entity.properties,
        lists:member(PropertyName, Property#property.name)].

%% @spec sel_type(EntityType::atom(), SelectorName::atom()) -> [atom()]
%% @doc The result is the type of the entities you'll get after the use of the
%%      given selector.
sel_type(EntityType, SelectorName) ->
    Entity = entity(EntityType),
    [Selector#selector.type ||
        Selector <- Entity#entity.selectors,
        lists:member(SelectorName, Selector#selector.name)].

%% @spec prop_type(EntityType::atom(), PropertyName::atom()) -> [atom()]
%% @doc The result is the type of the property.
prop_type(EntityType, PropertyName) ->
    Entity = entity(EntityType),
    [Property#property.type ||
        Property <- Entity#entity.properties,
        lists:member(PropertyName, Property#property.name)].

%%% ============================================================================

file_is_module(File) ->
    case ?Syn:node_type(File) of
        module -> true;
        file -> ?File:type(File) == module
    end.

file_prop(name, File) ->
    case file_is_module(File) of
        true ->
            [ModuleNode] = mod(File),
            io_lib:write_atom(?Mod:name(ModuleNode));
        false ->
            Path = ?File:path(File),
            string:substr(Path, string:rstr(Path, "/")+1)
    end;
file_prop(Prop, File) ->
    case file(File) of
        [] -> io_lib:write_atom(?Mod:name(File));
        [FileNode] ->
            Path = ?File:path(FileNode),
            case Prop of
                path -> Path;
                dir -> string:substr(Path, 1, string:rstr(Path, "/"))
            end
    end.

%% file_prop(Prop, File) ->
%%     case ?Syn:node_type(File) of
%%         module ->
%%             io_lib:write_atom(?Mod:name(File));
%%         file ->
%%             Path = ?File:path(File),
%%             case Prop of
%%                 path -> Path;
%%                 name -> string:substr(Path, string:rstr(Path, "/")+1);
%%                 dir -> string:substr(Path, 1, string:rstr(Path, "/"))
%%             end
%%     end.


mod(Node) ->
    case ?Syn:node_type(Node) of
        file -> ?Query:exec(Node, ?File:module());
        module -> [Node]
    end.

file(Node) ->
    case ?Syn:node_type(Node) of
        file -> [Node];
        module -> ?Query:exec(Node, ?Mod:file())
    end.

is_last_expr(Expr) ->
    Last = ?Query:exec(Expr, ?Query:seq([?Expr:clause(), [{visib, last}]])),
    Top = ?Query:exec(Expr, ?Expr:top()),
%    ?Expr:is_same_expr({Last, Top}).
    Last == Top.

last_common_node([{_L, N} = H|T1], [H|T2]) -> last_common_node(T1, T2, N);
last_common_node(_,_) -> ?Graph:root().

last_common_node([{_L, N} = H|T1], [H|T2], _) -> last_common_node(T1, T2, N);
last_common_node(_, _, Node) -> Node.
 
has_record_definition(Tuple) ->
    case lists:filter (fun(Node) -> ?Expr:type(Node) == atom end,
                       ?Dataflow:reach(
                          ?Query:exec(Tuple, ?Expr:child(1)), [back])) 
    of
        [AtomExpr] ->  
            case find_rec_with_name(Tuple, ?Expr:value(AtomExpr)) of
                [Rec] ->
                    length(?Query:exec(Rec, ?Rec:fields())) + 1 ==
                        length(?Query:exec(Tuple, ?Expr:children()));
                [] -> 
                    false
            end;
        _ ->
             false
    end.
           
find_rec_with_name(Tuple, Atom) ->
    ?Query:exec(Tuple, ?Query:seq([?Expr:clause(),
                                   ?Clause:form(),
                                   ?Form:file(),
                                   ?Rec:find(Atom)])).
                         
%% @private
%% @spec file_metrics(Name::atom(), File::entity(), Check::atom()) -> int()
%% @doc Metrics needs a module node, and a file entity can be either a module
%%      node or a file node, so it needs to be converted.
%%      Module nodes exist without files in the database, and most of the
%%      metrics needs the files too. The `Check' parameter indicates whether
%%      the file is needed.
%%      The default value is 0 for all the metrics. 
file_metrics(Name, File, false) ->
    case mod(File) of
        []       -> 0;
        [Module] -> ?Metrics:metric({Name, module, Module})
    end;
file_metrics(Name, File, true) ->
    case file(File) of
        [] -> 0;
        _  -> file_metrics(Name, File, false)
    end.

fun_metrics(Name, Fun) ->
    case ?Query:exec(Fun, ?Fun:definition()) of
        []       -> 0;
        [_FunDef] -> ?Metrics:metric({Name, function, Fun})
    end.
