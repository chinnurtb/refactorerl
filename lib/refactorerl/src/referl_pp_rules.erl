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

%%% @doc
%%% This module is a library of the Pretty Printer to handle the different 
%%% formatting rule types. 
%%% Supply the default formatting rules and configurations of the Erlang 
%%% language. There are several function to get the indentation values, the 
%%% required space size between tokens, the recommended line breakers, the 
%%% long line breakers and the line bbreakers of the multiple line terms.
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(referl_pp_rules).
-vsn("$Rev: $").
-include("refactorerl.hrl").
-include("referl_pp_types.hrl").


%%% ============================================================================
%%% Exports

% Rules for Erlang syntax
-export([erlang/0]).

% Handle rules
-export([get_indent_diff/4, get_token_rules/3,
         merge_token_defrules/1, merge_token_defrules/2, merge_token_rules/2,
         class_token_rulesR/1, class_token_rules/1,
         get_token_rule/2, get_token_rule_between/3]).
% For pp
-export([get_child_idx/4]).






%%% ============================================================================
%%% Rules for Erlang syntax



%% @spec erlang() -> ppConfig()
%% @doc  Give back the default formatting rules for the Erlang language.
erlang() ->
    #ppConfig{
        % General
        general = #ppConfig_general{
            useTab        = false,          % bool()
            tabSize       = 4,              % natural()
            optLineLength = 80,             % natural()
            minTextChars  = 30,             % natural()
            maxTextChars  = 65,             % natural()
            nlStr         = "\n",           % string()
            commentLines  = ["%"]           % [string()]
        },

        % Reserved words
        resWords = [
            'after','catch','cond','let','of','query','when',
            'begin','case','fun','if','receive','try','end',
            'and','andalso','not','or','orelse','xor',
            'band','bnot','bor','bsl','bsr','bxor',
            'div','rem'],

        % --- Indentations ------------------------------------------
        %#region % indentations
        indent = [
            % form
            #indentRule{
                parent = [{form, []}],
                indent = {parts,
                          [{'after', {flex,lex, [{#token.type,'==',op_paren}]},
                            to,      last,
                            1}]}},
            % fundef, funexpr, guard, pattern clauses
            #indentRule{
                parent = [{clause, [{#clause.kind,'==',fundef}]},
                          {clause, [{#clause.kind,'==',funexpr}]},
                          {clause, [{#clause.kind,'==',guard}]},
                          {clause, [{#clause.kind,'==',pattern}]}],
                indent = {parts,
                          [{'after', {clex,lex, [{#token.type,'==',op_paren}]},
                            to,      {clex,lex, [{#token.type,'==',arrow   }]},
                            2},
                           {'after', {clex,lex, [{#token.type,'==',arrow}]},
                            to,      last,
                            1}]}},
            % timeout clause
            #indentRule{
                parent = [{clause, [{#clause.kind,'==',timeout}]}],
                indent = {parts,
                          [{'after', {clex,lex, [{#token.type,'==',arrow}]},
                            to,      last,
                            1}]}},
            % block, if, case, try, receive expression
            #indentRule{
                parent = [{expr, [{#expr.kind,'==',block_expr  }]},
                          {expr, [{#expr.kind,'==',if_expr     }]},
                          {expr, [{#expr.kind,'==',case_expr   }]},
                          {expr, [{#expr.kind,'==',try_expr    }]},
                          {expr, [{#expr.kind,'==',receive_expr}]} ],
                indent = {except,
                          [{elex, lex, [{#token.type,'==','begin'  }]},
                           {elex, lex, [{#token.type,'==','if'     }]},
                           {elex, lex, [{#token.type,'==','case'   }]},
                           {elex, lex, [{#token.type,'==','of'     }]},
                           {elex, lex, [{#token.type,'==','try'    }]},
                           {elex, lex, [{#token.type,'==','catch'  }]},
                           {elex, lex, [{#token.type,'==','after'  }]},
                           {elex, lex, [{#token.type,'==','receive'}]},
                           {elex, lex, [{#token.type,'==','end'    }]} ],
                          1}},
            % function expr
            #indentRule{
                parent = [{expr, [{#expr.kind,'==',fun_expr}]}],
                indent = {except,
                          [{elex,   lex,    [{#token.type,'==','fun'}]},
                           {exprcl, clause, []                        },
                           {elex,   lex,    [{#token.type,'==','end'}]} ],
                          1}},
            % match expr
            #indentRule{
                parent = [{expr, [{#expr.kind,'==',match_expr}]}],
                indent = {parts,
                          [{'after', {elex,lex,[{#token.type,'==',equal}]},
                            to,      last,
                            1}]}},
            % application expr
            #indentRule{
                parent = [{expr, [{#expr.kind,'==',application}]}],
                indent = {parts,
                          [{'after', {elex,lex,[{#token.type,'==',op_paren}]},
                            to,      last,
                            1}]}},
            % macro definition parameters and body
            #indentRule{
                parent = [{lex,  [{#lex.type, '==',arg}]}],
                indent = {parts,
                          [{'after', first,
                            to,      last,
                            1}]}},
            % macro subsituation
            #indentRule{
                parent = [{lex,  [{#lex.type, '==',subst}]}],
                indent = {parts,
                          [{'after',{llex,lex,[{#token.type,'==',op_paren}]},
                            to,     last,
                            1}]}}
            ],
        %#endregion % indentations

        % --- White space and required line breakers ----------------
        %#region % white spaces and required line breakers
        ws_nl = [
            % --- Macro subsituations ---
            #tokenRule{
                parents = [{lex, [{#lex.type,'==',subst}]}],
                rules   = [{[comma,{last}],       [{ws,0,1}]},
                           {[{first}],            [{ws,1,0}]} ]},
            % macro definition parameters and body
            #tokenRule{
                parents = [{lex, [{#lex.type,'==',arg}]}],
                rules   = [{[comma],              [{ws,0,1}]}]},
            #tokenRule{
                parents = [{lex, [{#lex.type,'==',body}]}],
                rules   = [{[{any}],              [{ws,1,1}]}]},
            % --- Unary operators ---
            % +/-, not/bnot
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',prefix_bit_expr}]},
                           {expr, [{#expr.kind,'==',prefix_expr}]}    ],
                rules   = [
                    {[plus,minus],                [{ws,1,0}]},
                    {['not','bnot'],              [{ws,1,1}]} ]},
            % catch
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',catch_expr}]}],
                rules   = [{['catch'],            [{ws,0,1}]} ]},
            % --- Binary operators ---
            % binary operators
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',infix_expr}]}],
                rules = [
                    {[plus,minus,star,slash,dbl_plus,dbl_minus], [{ws,0,0}]},
                    {['and','andalso','or','orelse','xor'],      [{ws,1,1}]},
                    {['band','bor','bsl','bsr','bxor'],          [{ws,1,1}]},
                    {['div','rem'],                              [{ws,1,1}]},
                    {[comma,semicol],                            [{ws,0,1}]},
                    {[colon],                                    [{ws,0,0}]},
                    {[less,greater,eqless,greatereq,dbl_eq,
                      eqcoleq,slasheq,eqslasheq],                [{ws,0,0}]} ]},
            % --- Expressions ---
            % block if try receive
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',block_expr}]},
                           {expr, [{#expr.kind,'==',if_expr}]},
                           {expr, [{#expr.kind,'==',try_expr}]},
                           {expr, [{#expr.kind,'==',receive_expr}]}],
                rules   = [
                    {['begin','if','try','receive'],    [{ws,0,1},{nl,0,1}]},
                    {[semicol],                         [{ws,0,1},{nl,0,1}]},
                    {['of','catch','after'],            [{ws,1,1},{nl,1,1}]},
                    {['end'],                           [{ws,1,0},{nl,1,0}]} ]},
            % case
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',case_expr}]}],
                rules   = [{['case'],            [{ws,0,1}]},
                           {['of'],              [{ws,1,1},{nl,0,1}]},
                           {[semicol],           [{ws,0,1},{nl,0,1}]},
                           {['end'],             [{ws,1,0},{nl,1,0}]} ]},
            % fun expression
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',fun_expr}]}],
                rules   = [{['fun'],             [{ws,0,0}]},
                           {['end'],             [{ws,1,0}]}]},
            % implicit function
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',implicit_fun}]}],
                rules   = [{['fun'],             [{ws,0,1}]}]},
            % function name
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',atom}]}],
                rules   = [
                    {[{check_fun,fun funcform_first_name/1,atom}],[{nl,2,0}]}]},
            % general expr
            #tokenRule{
                parents = [{expr, []}],
                rules   = [
                    {['cond','let','query'],         [{ws,0,1}] },
                    {[comma,semicol],                [{ws,0,1}]},
                    {[op_paren,cl_paren,
                      op_brace,cl_brace,
                      op_bracket,cl_bracket],        [{ws,0,0}]},
                    {[dbl_less,dbl_greater],         [{ws,0,0}]},
                    {[dot,backarrow,backbinarrow],   [{ws,0,0}]},
                    {[hash],                         [{ws,0,0}]},
                    {[vline,dbl_vline,exclam,equal], [{ws,1,1}]} ]},
            % --- Clauses ---
            %
            #tokenRule{
                parents = [{clause, [{#clause.kind,'==',fundef }]},
                           {clause, [{#clause.kind,'==',guard  }]},
                           {clause, [{#clause.kind,'==',pattern}]} ],
                rules   = [
                    {[{after_link,'body',comma}],    [{ws,0,1},{nl,0,1}]} ]},
            % general clause
            #tokenRule{
                parents = [{clause, []}],
                rules   = [
                    {[op_paren,cl_paren],            [{ws,0,0}]},
                    {['when'],                       [{ws,1,1}]},
                    {[arrow],                        [{ws,1,1}]},
                    {[{after_link,'body',comma}],    [{ws,0,1}]},
                    {[comma],                        [{ws,0,1}]} ]},
            % --- Forms ---
            % function
            #tokenRule{
                parents = [{form, [{#form.type,'==',func}]}],
                rules   = [
                    {[semicol],     [{ws,0,1},{nl,0,1}]},
                    {[stop],        [{ws,0,1},{nl,0,2}]} ]},
            % record
            #tokenRule{
                parents = [
                    {form, [{#form.type,'==',attrib},{#form.tag,'==',record}]}],
                rules = [
                    {[minus],       [{ws,1,0},{nl,2,0}]},
                    {[stop],        [{ws,0,1},{nl,0,2}]} ]},
            % module
            #tokenRule{
                parents = [
                    {form, [{#form.type,'==',attrib},{#form.tag,'==',module}]}],
                rules = [
                    {[minus],       [{ws,0,0}]},
                    {[stop],        [{ws,0,1},{nl,0,2}]} ]},
            % general form
            #tokenRule{
                parents = [
                    {form, []}],
                rules = [
                    {[comma],       [{ws,0,1}         ]},
                    {[minus],       [{ws,1,0},{nl,1,0}]},
                    {[stop],        [{ws,0,1},{nl,0,1}]} ]}
            ],
        %#endregion % white spaces and rquired line breakers

        % --- Line breakers for long lines --------------------------
        %#region % long line breakers
        longNl = [
            % macro subsituation
            #tokenRule{
                parents = [{lex,  [{#lex.type, '==',subst}]}],
                rules   = [{[comma],              [{nl,0,1}]}]},
            % macro definition body
            #tokenRule{
                parents = [{lex, [{#lex.type,'==',arg}]}],
                rules   = [{[comma],              [{nl,0,1}]}]},
            #tokenRule{
                parents = [{lex,  [{#lex.type,'==',body}]}],
                rules   = [{
                    [plus,minus,star,dbl_plus,dbl_minus,
                     'and','andalso','or','orelse','xor',
                     'band','bor','bsl','bsr','bxor',
                     'div','rem',
                     comma,semicol,
                     less,greater,eqless,greatereq,dbl_eq,
                     eqcoleq,slasheq,eqslasheq,
                     vline,dbl_vline,exclam,equal,
                     arrow],
                    [{nl,0,1}]}]},
            % binary operators
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',infix_expr}]}],
                rules   = [{
                    [plus,minus,star,dbl_plus,dbl_minus,
                     {check_fun,fun no_impexp_slash/1,slash},
                     'and','andalso','or','orelse','xor',
                     'band','bor','bsl','bsr','bxor',
                     'div','rem',
                     comma,semicol,
                     less,greater,eqless,greatereq,dbl_eq,
                     eqcoleq,slasheq,eqslasheq],
                    [{nl,0,1}]}]},
            % application expr
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',application}]}],
                rules   = [{[op_paren, comma],       [{nl,0,1}]}]},
            % function expression
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',fun_expr}]}],
                rules   = [{[semicol],                       [{nl,1,0}]}]},
            % general expr
            #tokenRule{
                parents = [{expr, []}],
                rules   = [{[comma,vline,dbl_vline,exclam,equal], [{nl,0,1}]}]},
            % clause
            #tokenRule{
                parents = [{clause, [{#clause.kind,'==',fundef }]},
                           {clause, [{#clause.kind,'==',guard  }]},
                           {clause, [{#clause.kind,'==',pattern}]}],
                rules   = [{[arrow],                        [{nl,0,1}]}]},
            #tokenRule{
                parents = [{clause, [{#clause.kind,'==',funexpr}]}],
                rules   = [{[arrow,{after_link,'body',comma}], [{nl,0,1}]}]},
            #tokenRule{
                parents = [{clause, []}],
                rules   = [{[comma,cl_paren,'when'],        [{nl,0,1}]}]},
            % form
            #tokenRule{
                parents = [{form,  []}],
                rules   = [{[comma],                        [{nl,0,1}]}]}
            ],
        %#endregion % long line breakers

        % --- Extra line breakers for long terms --------------------
        %#region % terms in multiple lines
        multiNl = [
            % function expression
            #tokenRule{
                parents = [{expr, [{#expr.kind,'==',fun_expr}]}],
                rules   = [{[semicol],                     [{nl,0,1}]},
                           {['end'],                       [{nl,1,0}]}]},
            % clause
            #tokenRule{
                parents = [{clause, [{#clause.kind,'==',fundef }]},
                           {clause, [{#clause.kind,'==',guard  }]},
                           {clause, [{#clause.kind,'==',pattern}]}],
                rules   = [{[arrow],                        [{nl,0,1}]}]},
            #tokenRule{
                parents = [{clause, [{#clause.kind,'==',funexpr}]}],
                rules   = [{[arrow,{after_link,'body',comma}], [{nl,0,1}]}]}
            ]
        %#endregion % terms in multiple lines

    }.


% Check `Token' is the name of the first clause of a function form.
funcform_first_name(Token) ->
    case ?Graph:path(Token, [{elex,back},{name,back},{funcl,back}]) of
        [Form] -> [Token] == ?Graph:path(Form, [{funcl,1},{name,1},{elex,1}]);
        _ -> false
    end.

% Check `Token' is not slash in an import or export list
no_impexp_slash(Token) ->
    [] == ?Graph:path(Token, [{elex,back},{sub,back},{sub,back},
            {{attr,back}, {{type,'==',attrib},'and',
                          {{tag,'==',import},'or',{tag,'==',export}}}}]).






%%% ============================================================================
%%% Handler functions for `indentRule' typed rules



%% @spec get_indent_diff(IndentRules::[indentRule()], Node::node(),
%%               Child::node(), LinkTag::atom()) -> natural()
%% @doc  Get the diffrent between the indetation levels of the parent `Node'
%%       and `Child'. That is an natural number typically 0 or 1.
get_indent_diff([], _Node, _Child, _Tag) -> 0;
get_indent_diff([#indentRule{parent=PChecks, indent=ChRule}|Rules],
        Node, Child, Tag) when
        is_list(PChecks), is_tuple(ChRule), is_atom(Tag) ->
    case ?GR_UTILS:check_node(Node, PChecks) of
        {0,_} -> get_indent_diff(Rules, Node, Child, Tag);
        {_,_} -> get_indent_diff_(ChRule, Node, Child, Tag)
    end.


% Implementation helper functions for get_indent_diff/4
% Child is accepted is it is not in the exception list
get_indent_diff_({except, ExcChilds, Ind}, _Node, Child, Tag) ->
    FindResult = ?MISC:list_find(
        fun({ExcTag, ExcType, ExcFilter}) ->
            Tag==ExcTag andalso
            ?GR_UTILS:check_node_token(Child, ExcType, ExcFilter)
        end,
        ExcChilds),
    if
        0==element(1,FindResult) -> Ind;
        true -> 0
    end;
% Child is accepted if it is in a part
get_indent_diff_({parts, []}, _Node, _Child, _Tag) -> 0;
get_indent_diff_({parts, [{Mode1,Child1,Mode2,Child2,Ind}|Parts]},
        Node, Child, Tag) ->
    % Find part in the children list
    ChildTNIs = ?GR_UTILS:edges_idx(Node, forward, ?Syn:children(Node)),
    Idx1 = get_child_idx(Child1, Mode1, ChildTNIs, 1),
    Idx2 = get_child_idx(Child2, Mode2, ChildTNIs, Idx1),
    PartChildTNIs = if
        Idx1=<Idx2 andalso 0<Idx1 andalso 0<Idx2 ->
            lists:sublist(ChildTNIs, Idx1, Idx2-Idx1+1);
        true -> []
    end,
    % Find `Child' in the part
    case ?MISC:list_find(fun({_,PartChild,_}) -> PartChild==Child end,
                         PartChildTNIs) of
        {0,_} -> get_indent_diff_({parts, Parts}, Node, Child, Tag);
        {_,_} -> Ind
    end.


%% @private
%% @spec get_child_idx(Desc, Mode, ChildTNIs::[NodeTNI], Start::integer()) ->
%%           Index::integer()
%%       Desc = first | {first,any()} | last | {last,any()} |
%%              {path, [{LinkTag::atom(),Index::natural()}]} |
%%              {LinkTag::atom(), Index::natural()} |
%%              {LinkTag::atom(), NodeType::atom(), NodeFilters} |
%%              ((NodeTNI) -> boolean())
%%       NodeFilters  = [{NodeType::atom(), NoteDataFilters::[recordFilter()]}]
%%       RecordFilter = {FieldIndex::natural(), Op, Value::term()}
%%       Op           = '==' | '/='
%%       NodeTNI = {LinkTag::atom(), Node::node(), Index::natural()}
%%       Mode = from | to | before | 'after'
%% @doc  Get the index of first element in the `ChildTNIs'
%%       list which is accepted by the `Desc' descriptor.
%%       The `first'-`before', `last'-`after', `{first,_}'-`before' and
%%       `{last,_}'-`after' pairs are invalid.
%%       The searching starts from the `Start' index.
%%       If there is no founded element return 0.
get_child_idx(first,     from,   _ChildTNIs, _S) -> 1;
get_child_idx(first,     'after',_ChildTNIs, _S) -> 2;
get_child_idx(last,      to,      ChildTNIs, _S) when is_list(ChildTNIs) ->
    length(ChildTNIs);
get_child_idx(last,      before,  ChildTNIs, _S) when is_list(ChildTNIs) ->
    length(ChildTNIs)-1;
get_child_idx({first,_}, Mode,    ChildTNIs, Start) ->
    get_child_idx(first, Mode,    ChildTNIs, Start);
get_child_idx({last,_},  Mode,    ChildTNIs, Start) ->
    get_child_idx(last,  Mode,    ChildTNIs, Start);
get_child_idx({path,[{Tag,Idx}|_]}, Mode, ChildTNIs, Start) when
        is_atom(Tag), is_integer(Idx), 0<Idx ->
    get_child_idx({Tag,Idx}, Mode, ChildTNIs, Start);
get_child_idx({Tag,Idx}, Mode,    ChildTNIs, Start) when
        is_atom(Tag), is_integer(Idx), 0<Idx ->
    get_child_idx(fun({T,_,I}) -> T==Tag andalso I==Idx end,
        Mode, ChildTNIs, Start);
get_child_idx({Tag,Type,Filter}, Mode, ChildTNIs, Start) when
        is_atom(Tag), is_atom(Type), is_list(Filter) ->
    get_child_idx(fun({T,N,_}) ->
            T==Tag andalso ?GR_UTILS:check_node_token(N, Type, Filter)
        end,
        Mode, ChildTNIs, Start);
get_child_idx(FindFun,   Mode,    ChildTNIs, Start) when is_function(FindFun),
        is_atom(Mode), is_list(ChildTNIs), is_integer(Start) ->
    Length = length(ChildTNIs),
    Idx0 = if
        0<Start andalso Start=<Length ->
            element(1,?MISC:list_find(FindFun,
                                      lists:nthtail(Start-1, ChildTNIs)));
        true -> 0
    end,
    if
        0<Idx0 ->
            Idx = Start-1 + case Mode of
                from    -> Idx0;
                to      -> Idx0;
                before  -> Idx0-1;
                'after' -> Idx0+1
            end,
            if
                Idx<1      -> 1;
                Length<Idx -> Length;
                true       -> Idx
            end;
        true -> Idx0
    end.




%%% ----------------------------------------------------------------------------
%%% Handler functions for `tokenRule' typed rules


%% @spec get_token_rules(ChildRules::[tokenRules()], Token::token(),
%%               ParentNode::node()) ->
%%           {Rules, DefaultRules}
%% @doc  Find rules and default rules for `Token'.
get_token_rules([], _Token, _Parent) -> {[],[]};
get_token_rules([#tokenRule{parents=PChecks,default=Defs,rules=ChRules}|Rules],
        Token, Parent) when is_list(PChecks), is_list(Defs), is_list(ChRules) ->
    % Check parent
    case ?GR_UTILS:check_node(Parent, PChecks) of
        {0,_} -> get_token_rules(Rules, Token, Parent);
        {_,_} ->
            % Check tokens
            FindChildRuleFun = fun({TokenDescs,_TokenRules}) ->
                    ?MISC:list_member(
                        fun(Desc) -> check_token(Desc,Token,Parent) end,
                        TokenDescs)
                end,
            case ?MISC:list_find(FindChildRuleFun, ChRules) of
                {0,_} -> get_token_rules(Rules, Token, Parent);
                {_,{_TokenDescs,TokRules}} ->
                    {TokRules,Defs} % Return rules and default rulse
            end
    end.


% Check token by token descriptor
check_token({first}, Token, Parent) ->
    Token =:= element(2,hd(?Syn:children(Parent)));
check_token({last}, Token, Parent) ->
    Token =:= element(2,lists:last(?Syn:children(Parent)));
check_token({before_link,LinkTag,Type}, Token, Parent) ->
    check_token_link_(?Syn:children(Parent),
                      LinkTag, Type, Token, Parent);
check_token({after_link, LinkTag,Type}, Token, Parent) ->
    check_token_link_(lists:reverse(?Syn:children(Parent)),
                      LinkTag, Type, Token, Parent);
check_token({check_fun,CheckFun,Type}, Token, Parent) ->
    check_token(Type, Token, Parent) andalso CheckFun(Token);
check_token({any}, _Token, _Parent) ->
    true;
check_token(Type, Token, _Parent) ->
    Type==((?Graph:data(Token))#lex.data)#token.type.

% Implementation helper function for check_token/3
check_token_link_([],    _LinkTag,_Type,_Token,_Parent) -> false;
check_token_link_(Childs, LinkTag, Type, Token, Parent) ->
    case check_token(Type, Token, Parent) of
        false -> false;
        _ ->
            CheckFun = fun({_,N}) -> N/=Token end,
            case lists:dropwhile(CheckFun, Childs) of
                [{_,Token},{LinkTag,_}|_] -> true;
                _ -> false
            end
    end.




%%% ----------------------------------------------------------------------------
%%% Handler functions for rules of tokens


%% @spec merge_token_defrules(
%%               {Rules::[tokenRule()], DefaultRules::[tokenRule()]}) ->
%%           Rules::[tokenRule()]
%% @doc  Add missing rules to `Rules' from default rules. Wrapper function
%%       for {@link merge_token_defrules/2}.
%% @see merge_defrule/2
merge_token_defrules({Rules,DefRules}) when is_list(Rules), is_list(DefRules) ->
    merge_token_defrules(Rules,DefRules).

%% @spec merge_token_defrules(Rules::[tokenRule()], DefaultRules::[tokenRule()])
%%           -> Rules::[tokenRule()]
%% @doc  Add missing rules to `Rules' from default rules.
merge_token_defrules(Rules,DefRules) when is_list(Rules), is_list(DefRules) ->
    Rules ++ ?MISC:list_substract(DefRules, Rules,
        fun({Type,_,_},{Type,_,_}) -> true; ({_,_,_},{_,_,_}) -> false end).


%% @spec merge_token_rules(Rules1::[tokenRule()], Rules2::[tokenRule()]) ->
%%           Rules::[tokenRule()]
%% @doc  Merge the rules. If there are same rules in both list the smaller rule
%%       in absolute value will be dropped.
merge_token_rules(Rules1, Rules2) when is_list(Rules1), is_list(Rules2) ->
    ClassDict = class_token_rulesR(lists:merge(lists:usort(Rules1),
                                               lists:usort(Rules2))),
    lists:reverse(dict:fold(
        fun(_Type, Rules, List) ->
            case Rules of
                [] -> List;
                _  -> [maxabs_token_rule(Rules)|List]
            end
        end,
        [],
        ClassDict)).


%% @spec class_token_rulesR(Rules::[tokenRule()]) ->
%%           ClassDict::dict(RuleType::atom(), Rules::[tokenRule()])
%% @doc  Split `Rules' into separated lists by the first element of the 3-tuple
%%       rules. The keys of `ClassDict' are the first elements and the values
%%       are the separated lists. The list are contain rules in reverse order
%%       as the `Rules' list.
class_token_rulesR(Rules) when is_list(Rules) ->
    lists:foldl(fun(Rule={Type,_,_}, Dict) ->
            case dict:find(Type,Dict) of
                error    -> dict:store(Type, [Rule], Dict);
                {ok, Rs} -> dict:store(Type, [Rule|Rs], Dict)
            end
        end,
        dict:new(),
        Rules).

%% @spec class_token_rules(Rules::[tokenRule()]) ->
%%           ClassDict::dict(RuleType::atom(), Rules::[tokenRule()])
%% @doc  Split `Rules' into separated lists by the first element of the 3-tuple
%%       rules. The keys of `ClassDict' are the first elements and the values
%%       are the separated lists. The list are contain rules in same order
%%       as the `Rules' list.
class_token_rules(Rules) ->
    dict:map(fun(_Type,Rs) -> lists:reverse(Rs) end,
        class_token_rulesR(Rules)).


% The list must contain same typed rules. Return that rule values that are
% greatest in absolute value.
maxabs_token_rule([]) -> no;
maxabs_token_rule([{Type,Val1,Val2}|Tail]) ->
    MaxAbsFun = fun({_T,V1,V2},{MA1,MV1,MA2,MV2}) ->
        A1 = abs(V1), A2 = abs(V2),
        case {MA1<A1, MA2<A2} of
            {true,  true } -> { A1,  V1,  A2,  V2};
            {true,  false} -> { A1,  V1, MA2, MV2};
            {false, true } -> {MA1, MV1,  A2,  V2};
            _              -> {MA1, MV1, MA2, MV2}
        end
    end,
    {_,MV1,_,MV2} = lists:foldl(MaxAbsFun,{abs(Val1),Val1,abs(Val2),Val2},Tail),
    {Type,MV1,MV2}.


%% @spec get_token_rule(Type::atom(), Rules::[tokenRule()]) -> tokenRule()
%% @doc  Return the `Type' typed rule from `Rules'.
get_token_rule(Type, Rules) when is_atom(Type), is_list(Rules) ->
    element(2,
        ?MISC:list_find(fun({T,_,_}) when T==Type ->
                    true;
            ({_,_,_}) ->
                false
            end,
            Rules)).


%% @spec get_token_rule_between(Type::atom(), Rules1::[tokenRule()],
%%               Rules2::[tokenRule()]) ->
%%           {Max::integer(), Sum::integer(), After1::integer(),
%%            Before2::integer()}
%% @doc  `Rules1' and `Rules2' are the rules of two following token and it
%%       return the maximum, summary and original values of `Type' typed rules
%%       for the formatting between tokens.
get_token_rule_between(Type, Rules1, Rules2) when is_atom(Type),
        is_list(Rules1), is_list(Rules2) ->
    case {get_token_rule(Type, Rules1), get_token_rule(Type, Rules2)} of
        {{_T,_B1,A1},{_T,B2,_A2}} -> {lists:max([A1,B2]),A1+B2,A1,B2};
        {{_T,_B1,A1},no         } -> {A1, A1, A1, 0 };
        {no,         {_T,B2,_A2}} -> {B2, B2, 0,  B2};
        {no,         no         } -> {0,  0,  0,  0 }
    end.


