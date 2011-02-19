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

%%% @doc Generic refactoring argument handler. The goal of this module is to
%%% let refactoring code be independent of parameter representation. Parameter
%%% lists are represented by property lists, but refactorings need not know
%%% the property keys used for a certain parameter. This allows multiple ways
%%% of specifying a parameter (like a function can be specified by a file and
%%% a position, or a module and a function name).
%%%
%%% Parameters are classified as follows:
%%% <dl>
%%%  <dt>Source parameters</dt>
%%%  <dd>These are usually existing entities which are to be modified by the
%%%      refactoring. Interface functions are named after the kind of entity
%%%      that is required by the refactoring, and they return graph nodes.</dd>
%%%
%%%  <dt>Target parameters</dt>
%%%  <dd>These parameter values are usually either properties (like a new
%%%      name) or entities that need not exist (like a target file which can
%%%      be created). Target interface functions thus always return a property
%%%      value, which can be used to identify target entities (e.g. a module
%%%      by its name).</dd>
%%% </dl>
%%%
%%% @type arglist() = [{Key::atom(), Value}]. Refactoring argument list, which
%%% may contain the following key-value pairs:
%%% <table border="1">
%%%  <tr><th>key</th><th>value type</th><th>constraint</th></tr>
%%%  <tr><td>`arity'</td>  <td>{@type integer()}</td><td>Non-negative</td></tr>
%%%  <tr><td>`create_new_file'</td> <td>{@type bool()}</td></tr>
%%%  <tr><td>`file'</td>     <td>{@type string()}</td>
%%%                                                <td>Valid filename</td></tr>
%%%  <tr><td>`filename'</td> <td>{@type string()}</td>
%%%                                                <td>Valid filename</td></tr>
%%%  <tr><td>`function'</td> <td>{@type atom()}</td></tr>
%%%  <tr><td>`funlist'</td>
%%%                    <td>{@type [{Fun::atom(), Arity::integer()@}]}</td></tr>
%%%  <tr><td>`funclusters'</td>
%%       <td>{@type [[{Mod::atom(), Fun::atom(), Arity::integer()@}]]}</td></tr>
%%%  <tr><td>`has_side_effect'</td> <td>{@type bool()}</td></tr>
%%%  <tr><td>`macname'</td>    <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`macro'</td>    <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`maclist'</td><td>{@type [atom()|string()]}</td></tr>
%%%  <tr><td>`module'</td>   <td>{@type atom()}</td></tr>
%%%  <tr><td>`name'</td>     <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`number'</td>   <td>{@type number()}</td></tr>
%%%  <tr><td>`order'</td>    <td>{@type [integer()]}</td>
%%%                                 <td>The numbers 1..n in any order</td></tr>
%%%  <tr><td>`position'</td> <td>{@type integer()}</td><td>Positive</td></tr>
%%%  <tr><td>`posrange'</td><td>{@type {Begin::integer(),End::integer()@}}</td>
%%%                                       <td>1 &lt;= Begin &lt;= End</td></tr>
%%%  <tr><td>`record'</td><td>{@type atom()}</td></tr>
%%%  <tr><td>`recfield'</td><td>{@type atom()}</td></tr>
%%%  <tr><td>`reclist'</td><td>{@type [atom()]}</td></tr>
%%%  <tr><td>`text'</td>     <td>{@type string()|atom()}</td></tr>
%%%  <tr><td>`varname'</td>  <td>{@type string()}</td>
%%%                                          <td>Valid variable name</td></tr>
%%% </table>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reflib_args).
-vsn("$Rev: 5504 $ ").

-export([string/1, string/2, string/3, recfield_names/1,
         integer/1, integer/3,
         name/1, atom/3, bool/3]).
-export([function/1, variable/1, variable/2, module/1, moduse/1, file/1, form/1,
         expression/1, functions/1, record/1, record_field/1, records/1,
         import_form/1, macro/1, macros/1, funclusters/1]).
-export([varname/1, filename/1, macname/1, macuse/1]).
-export([expr_range/1, order/1]).
-export([error_text/2]).
-export([ask_missing/1, algorithm/1, entity/1]).
-export([ask/6, ask/5, ask/4]).

-include("lib.hrl").
-define(CallArg(A,S,L),?MISC:call_arg(A,S,L)).

%%% @type node() = refcore_graph:node()

%%% ============================================================================
%%% Error text

error_text(bad_atom, [Text]) ->
    ["\"", Text, "\" is not a valid atom (use single quotes)"];
error_text(bad_funclusters, [Clusters]) ->
    ["\"", io_lib:print(Clusters), "\" is not a valid function clustering"].

error_action(bad_mod_name, ModName) ->
    Info = ?MISC:format("The given module (" ++
                                atom_to_list(ModName) ++ ") does not exist."),
    module([{ask_missing, true}, {info_text, Info}]);

error_action(bad_moduse_name, ModName) ->
    Info = ?MISC:format("The given module (" ++
                                atom_to_list(ModName) ++ ") does not exist."),
    moduse([{ask_missing, true}, {info_text, Info}]);

error_action(bad_file, File) ->
    Info = ?MISC:format("The given file (" ++
                                File ++ ") cannot be found in the database."),
    file([{ask_missing, true}, {info_text, Info}]);

error_action(bad_fun_name, [ModName, [FunName, Arity]]) ->
    Info = ?MISC:format("The function " ++
                                         atom_to_list(FunName) ++ "/" ++
                                         integer_to_list(Arity) ++ " does not exist."),
    function([{module, ModName},{ask_missing, true}, {info_text, Info}]);

error_action(bad_macro_name, [File, Macro]) ->
    Info = ?MISC:format("The macro ?~p does not exist.", [Macro]),
    macro([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_rec_name, [File, Record]) ->
    Info = ?MISC:format("The record field #~p does not exist.", [Record]),
    record([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_var_name, Var) ->
    Info = ?MISC:format("The name ~p is not a valid variable name", [Var]),
    varname([{ask_missing, true}, {info_text, Info}]).


error_action(bad_recfield_name, File, [Record, RecField]) ->
    Info = ?MISC:format("The record field #~p.~p does not exist.", [Record, RecField]),
    record_field([{file, File},{record, Record},{ask_missing, true}, {info_text, Info}]);

error_action(bad_fun_pos, ModName, Pos) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate function."),
    function([{module, ModName},{ask_missing, true}, {info_text, Info}]);

error_action(bad_mod_pos, _File, Pos) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate a module."),
    module([{ask_missing, true}, {info_text, Info}]);

error_action(bad_moduse_pos, File, Pos) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate a module."),
    moduse([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_macro_pos, File, Pos) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate a macro."),
    macro([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_macuse_pos, File, Pos) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate a macro usage."),
    macuse([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_rec_pos, File, Pos) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate a record."),
    record([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_recfield_pos, File, Pos) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate a record field."),
    record_field([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_var_pos, _File, Pos) ->
    ?Transform:question([[{format,info},{text,"The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate a variable or has to be inside a function clause."}]]);

error_action(bad_importform_pos, _File, Pos) ->
    ?Transform:question([[{format,info},{text,"The given position (" ++ integer_to_list(Pos) ++
     ") has to indicate an import form."}]]).

%%% ============================================================================
%%% Database independent functions

%% @spec string(arglist()) -> string()
%% @doc Returns a generic target string value. This value is specified with a
%% `text' key in the argument list.
string(Args) -> string(text, "text: ", Args).

%% @spec recfield_names(arglist()) -> string()
%% @doc Returns target string value, which specifies the field names of a record.
recfield_names(Args) -> string(text, "record field names: ", Args).

%% @spec string(arglist(), string()) -> string()
%% @doc Returns a generic target string value. This value is specified with a
%% `text' key in the argument list.
string(Args, Text) -> string(text, Text, Args).

%% @spec integer(arglist()) -> integer()
%% @doc Returns a generic target integer value. This value is specified with a
%% `number' key in the argument list.
integer(Args) -> integer(number, "A number", Args).

%% @spec name(arglist()) -> atom()
%% @doc Returns a generic target name as an atom (which can be used as a
%% function or module name). This value is specified with a `name' key in the
%% argument list.
name(Args) ->
    DefText = "Please enter a target name (an atom):",
    NewArgs = ask_missing(name, Args, atom, DefText, fun list_to_atom/1),
    atom(name, "Target name", NewArgs).


%% @spec string(atom(), string(), arglist()) -> string()
%% @doc Returns an arbitrary string parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link string/1} is insufficient.
string(Key, Desc, Args) ->
    DefText = "Please enter the " ++ Desc,
    NewArgs = ask_missing(Key, Args, text, DefText),
    ?CallArg(NewArgs, Desc, [{stringval(Key), [Key]}]).

stringval(Key) ->
    fun
        (Atom) when is_atom(Atom) -> atom_to_list(Atom);
        (Str) when is_list(Str) -> Str;
        (_) -> throw(?RefError(arg_type, [Key, "string"]))
    end.

%% @spec integer(atom(), string(), arglist()) -> integer()
%% @doc Returns an arbitrary integer parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link integer/1} is insufficient.
integer(Key, Desc, Args) ->
    ?CallArg(Args, Desc, [{intval(Key), [Key]}]).

intval(Key) ->
    fun
        (Num) when is_integer(Num) -> Num;
        (Num) when is_float(Num)   -> trunc(Num);
        (_) -> throw(?RefError(arg_type, [Key, "integer"]))
    end.

%% @spec bool(atom(), string(), arglist()) -> bool()
%% @doc Returns an arbitrary boolean parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message.
bool(Key, Desc, Args) ->
    ?CallArg(Args, Desc, [{boolval(Key), [Key]}]).

boolval(Key) ->
    fun
        (B) when is_boolean(B) -> B;
        (_) -> throw(?RefError(arg_type, [Key, "bool"]))
    end.

%% @spec atom(atom(), string(), arglist()) -> atom()
%% @doc Returns an arbitrary atom parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link name/1} is insufficient.
atom(Key, Desc, Args) ->
    ?CallArg(Args, Desc, [{atomval(Key), [Key]}]).

%% Note: since `erl_scan:string/1` identifies `spec` as a category in itself
%%       and not as an atom, it is dealt with as a unique case.
atomval(Key) ->
    fun
        (Atom) when is_atom(Atom) -> Atom;
        ("spec") -> spec;
        (Str) when is_list(Str) ->
            case erl_scan:string(Str) of
                {ok, [{atom, _, Atom}], _} -> Atom;
                _ -> throw(?LocalError(bad_atom, [Str]))
            end;
        (_) -> throw(?RefError(arg_type, [Key, "atom"]))
    end.

%% @spec filename(arglist()) -> string()
%% @doc Returns the target file name. This value is specified with
%% a `filename' key in the argument list.
filename(Args) ->
    DefText = "Please enter the target file name:",
    NewArgs = ask_missing(filename, Args, header, DefText),
    string(filename, "Target file name", NewArgs).

%% @spec order(arglist()) -> [integer()]
%% @doc Returns the target order (a list of consecutive integers starting from
%% 1, in an arbitrary order). This value is specified with an `order' key in
%% the argument list.
order(Args) ->
    DefText = "Please specify the order (e.g 3 1 2):",
    NewArgs = ask_missing(order, Args, none, DefText, fun convert_order/1),
    ?CallArg(NewArgs, "Order", [{fun orderbylst/1, [order]}]).

convert_order(Order) ->
    [list_to_integer(T) || T <- string:tokens(Order, " ")].

orderbylst(List) ->
    if
        is_list(List) ->
            ListLength = length(List),
            case lists:sort(List) =:= ?MISC:seq2(1, ListLength) of
                true ->
                    List;
                false ->
                    throw(?RefError(bad_order, [ListLength]))
            end;
        true ->
            throw(?RefError(order_not_list, []))
    end.

ask_missing(Args) ->
    case proplists:is_defined(ask_missing, Args) of
        true ->
            Interaction = proplists:get_all_values(ask_missing, Args),
            lists:all(fun(I) -> I == true end, Interaction);
        false ->
            false
    end.

% -spec ask(args(), atom(), (data(), infos()) -> collected(),
%           (referror_action(), data(), infos()) -> string(), infos()) -> collected().
%% @doc This function inputs an argument using `ArgFun', a function of arity one from `Args'.
%% `ArgFun' has to take both `Args' and `[{ask_missing, true}]' as arguments.
%% It then preconverts the data using `PreConvert',
%% collects infos (`CollectAndCheckFun', getting the asked arg
%% and additionally supplied arguments `AddInfos') and does checks;
%% if the checks fail and interactions are on,
%% it displays a format error and tries to get another input from the user.
%% The error is displayed using `AskErrorFmt/3',
%% which takes the ?RefError, the collected data and the additional infos,
%% and returns `{ErrorFmt, ErrorFmtData}'.
%% @todo Should return the new `Args' as well.
ask(Args, ArgFun, CollectAndCheckFun, AskErrorFmt, AddInfos, PreConvert) ->
    InitData = ?Args:ArgFun(Args),
    case ask_missing(Args) of
        false ->
            ConvInitData = PreConvert(InitData),
            CollectAndCheckFun(ConvInitData, AddInfos);
        true ->
            ask2(Args, CollectAndCheckFun, AddInfos, ArgFun, AskErrorFmt, PreConvert, InitData)
    end.

ask(Args, ArgFun, CollectAndCheckFun, AskErrorFmt, AddInfos) ->
    PreConvert = fun(X) -> X end,
    ask(Args, ArgFun, CollectAndCheckFun, AskErrorFmt, AddInfos, PreConvert).

%% @doc The only difference between function `ask/5' and this one is that
%%      `AddInfos' is not required here, and the functions
%%      `CollectAndCheckFun' and `AskErrorFmt' do not receive it.
ask(Args, ArgFun, CollectAndCheckFun, AskErrorFmt) ->
    CnC2 = fun(Data, unused)        -> CollectAndCheckFun(Data) end,
    AEF2 = fun(Error, Data, unused) -> AskErrorFmt(Error, Data) end,
    ask(Args, ArgFun, CnC2, AEF2, unused).

ask2(Args, CollectAndCheckFun, AddInfos, ArgFun, AskErrorFmt, PreConvert, Data) ->
    ConvData = PreConvert(Data),
    try
        CollectAndCheckFun(ConvData, AddInfos)
    catch
        % todo Should only catch thrown ?RefError(_, _) and ?LocalError(_, _)
        throw:Error ->
            ErrorTxt = AskErrorFmt(Error, ConvData, AddInfos),
            NewData = ?Args:ArgFun([{ask_missing, true}, {info_text, ErrorTxt}|Args]),
            ask2(Args, CollectAndCheckFun, AddInfos, ArgFun, AskErrorFmt, PreConvert, NewData)
    end.

% @todo document
algorithm(Args) ->
    NewArgs =
        case proplists:is_defined(algorithm, Args) of
            false ->
                Qu = [{format,info},{text, "Please select an algorithm for clustering"}],
                {_Args2, Qu2} = get_info_text(Args, Qu),
                Question = [Qu2] ++
                           [[{format,radio},{text, "Agglomerative"},{default,false}],
                            [{format,radio},{text, "Genetic"},{default,false}]],
                Ans = ?Transform:question(Question),
                Alg = case get_data(Ans, Question) of
                          "Agglomerative" -> agglom_attr;
                          "Genetic"       -> genetic
                      end,
                Args ++ [{algorithm, Alg}];
            true  ->
                Args
        end,

    ?CallArg(NewArgs, "Clustering algorithm",
             [{fun algVal/1, [algorithm]}]).

algVal(Algorithm) ->
    case Algorithm of
        agglom_attr -> agglom_attr;
        genetic     -> genetic;
        _           -> algorithm([])
    end.

% @todo document
entity(Args) ->
    NewArgs =
        case proplists:is_defined(entity, Args) of
            false ->
                Qu = [{format,info},{text, "Please select an entity type for clustering"}],
                {_Args2, Qu2} = get_info_text(Args, Qu),
                Question = [Qu2] ++
                           [[{format,radio},{text, "function"},{default,false}],
                            [{format,radio},{text, "module"},{default,false}]],
                Ans = ?Transform:question(Question),
                Ent = list_to_atom(get_data(Ans, Question)),
                Args ++ [{entity, Ent}];
            true  ->
                Args
        end,

    ?CallArg(NewArgs, "Clustering algorithm",
             [{fun entVal/1, [entity]}]).

entVal(Entity) ->
    case Entity of
        function -> function;
        module   -> module;
        _        -> entity([])
    end.

%%% ============================================================================
%%% Database dependent functions

% @todo document
funclusters(Args) ->
    ?CallArg(Args, "Function clusters",
        [{fun funclusters_/1, [funclusters]}]).

funclusters_(Clusters) ->
    try
        [[ begin
               {M,F,A}=Fun,
               true = is_atom(M) andalso is_atom(F) andalso is_integer(A)
           end
           || Fun <- Cluster ] || Cluster <- Clusters],
        Clusters
    catch
        _:_ -> throw(?RefError(bad_funclusters,[Clusters]))
        %@todo badmatch
    end.


%% @spec function(arglist()) -> node()
%% @doc Returns the source function node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`module', `function', and `arity' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
function(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  ->
                NArgs = getModule(Args),
                case proplists:is_defined(function,NArgs) or proplists:is_defined(position,NArgs) of
                    false ->
                        Funs = ?Query:exec(module(NArgs), ?Mod:locals()),
                        ?Check(Funs /= [], ?RefErr0r(no_fun)),

                        Qu = [{format,info},{text,"Please specify a function:"}],
%                        {_Args2, Qu2} = get_info_text(Args, Qu),
                        Question = [Qu] ++
                            [addtoproplist(?Fun:name(F),?Fun:arity(F))|| F <- Funs],
                        Ans = ?Transform:question(Question),
                        [Name,Arity] = getfundata(Ans,Question),
                        %%io:format("Ans:~p~n", [Ans]),
                        %%io:format("Name, arity:~p,~p~n", [Name, Arity]),
                        NArgs ++ [{function,list_to_atom(Name)},
                                  {arity,list_to_integer(Arity)}];
                    _     ->
                        NArgs
                end;
            false ->
                Args
         end,
     ?CallArg(NewArgs, "Function",
             [{fun funbyname/4, [module, function, arity, ask_missing]},
              {fun funbypos/3, [file, position, ask_missing]},
              {fun funnode/1, [nodes]}]).

funnode([Node])-> %@todo verify
    Node.

getfundata(Ans,Question) ->
    FunInList = proplists:get_value(yes,lists:zip(Ans,Question),no_answer),
    case FunInList == no_answer of
        false ->
            Fun = proplists:get_value(text,FunInList),
            string:tokens(Fun,"/");
        true  ->
            throw(?RefErr0r(cancelled))
    end.

addtoproplist(Name,Arity) ->
    [{format,radio},
     {text,lists:flatten(io_lib:format("~p~s~p",[Name,"/",Arity]))},
     {default,false}].

getfundatas(Ans,Question) ->
    ValidAns = proplists:get_all_values(yes,lists:zip(Ans,Question)),
    case ValidAns of
        []          ->
            throw(?RefErr0r(cancelled));
        FunsInList  ->
            Funs = [ proplists:get_value(text, FunInList) || FunInList <- FunsInList],
            [list_to_tuple(string:tokens(Fun,"/")) || Fun <- Funs]
    end.

addtoproplist2(Name,Arity) ->
    [{format,checkbox},
     {text,lists:flatten(io_lib:format("~p~s~p",[Name,"/",Arity]))},
     {default,true},{validator,function}].

funbyname(Mod, Fun, Arity, AskMissing) ->
    Function = ?Query:exec(
                  ?Query:seq([?Mod:find(Mod),
                              ?Mod:local(Fun, Arity)])),

    case Function of
        [Result] -> Result;
        _       ->
            case AskMissing of
                true  -> error_action(bad_fun_name, [Mod,[Fun,Arity]]);
                false -> throw(?RefError(fun_not_found, [Mod, Fun, Arity]))
            end
    end.

%% @todo Accept the `fun' token of implicit functions
funbypos(File, Pos, AskMissing) ->
    Expr = ?Query:exec(
              ?Query:seq([?File:find(File),
                          ?File:token(Pos),
                          ?Token:expr()])),

    case Expr of
        [_Result] ->
            case ?Query:exec(Expr, ?Query:seq([?Expr:nameof(),
                                               ?Clause:form(),
                                               ?Form:func()])) of
                  [Fun] -> Fun;
                  []    -> funbyexportlist(Expr, Pos)
            end;
        _        ->
            case AskMissing of
                true  -> error_action(bad_fun_pos, ?Mod:name(modbyfile(File,true)), Pos);
                false -> throw(?RefError(pos_bad_type, ['fun', Pos]))
            end
    end.

funbyexportlist(Expr, Pos) ->
    case ?Query:exec(Expr, ?Expr:function()) of
       [Fun] -> Fun;
       [] ->
           case ?Query:exec(Expr, ?Query:seq(?Expr:parent(),
                                             ?Expr:function())) of
               [Fun] -> Fun;
               [] -> funbyref(Expr, Pos)
           end
    end.

funbyref(Expr, Pos) ->
    case ?Query:exec(Expr, ?Expr:parent()) of
        [] -> throw(?RefError(pos_bad_type, ['fun', Pos]));
        [Par] ->
            case ?Expr:type(Par) of
                T when T =:= application;
                       T =:= implicit_fun ->
                    ?Query:exec1(Par, ?Expr:function(),
                                 ?RefError(pos_bad_type, ['fun', Pos]));
                infix_expr ->
                    ?Query:exec1(Par,
                                 ?Query:seq(?Expr:parent(),
                                            ?Expr:function()),
                                 ?RefError(pos_bad_type, ['fun', Pos]));
                _ ->
                    throw(?RefError(pos_bad_type, ['fun', Pos]))
            end
    end.

%% @spec functions(arglist()) -> [node()]
%% @doc Returns the source function node list. This value is specified with
%% `file' and `funlist' keys in the argument list.
functions(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  ->
                NArgs = getFile(Args),
                case proplists:is_defined(funlist, NArgs) of
                    false ->
                        Mod = modbyfile(proplists:get_value(file, NArgs), true),
                        Funs = ?Query:exec(Mod, ?Mod:locals()),
                        Qu = [{format,info},{text, "Please select the functions from the list:"}],
                        {_Args2, Qu2} = get_info_text(Args, Qu),
                        Question = [Qu2] ++
                            [addtoproplist2(?Fun:name(F),?Fun:arity(F))|| F <- Funs],
                        Ans = ?Transform:question(Question),
                        NArgs ++ [{funlist, [{list_to_atom(N),list_to_integer(A)}
                                             || {N,A} <- getfundatas(Ans,Question)]}];
                    true  ->
                        NArgs
                end;
            false ->
                Args
        end,

    ?CallArg(NewArgs, "Function list",
             [{fun funsbyname/2, [file, funlist]},
              {fun funbypos_/2, [file, position]},
              {fun funnodes/1, [nodes]}]).

funnodes(Nodes)-> %@todo verify
    Nodes.

funsbyname(File, Funs) ->
    Mod = ?Query:exec1(
             ?Query:seq([?File:find(File),
                         ?File:module()]),
             ?RefError(file_not_module, [File])),
    [?Query:exec1(Mod, ?Mod:local(Fun, Arity),
                  ?RefError(fun_not_found, [Fun, Arity])) ||
        {Fun, Arity} <- Funs].

funbypos_(File,Pos) ->
    [funbypos(File,Pos,false)].

%% @spec macro(arglist()) -> node()
%% @doc Returns the source macro node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`file' and `macro' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
macro(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  ->
                NArgs = getFile(Args),
                case proplists:is_defined(macro, NArgs) or proplists:is_defined(position, NArgs) of
                    false ->
                        File = filebyname(proplists:get_value(file, NArgs), true),
                        Macros = ?Query:exec(File, ?File:macros()),
                        Qu = [{format,info},{text,"Please specify a macro:"}],
                        {_Args2, Qu2} = get_info_text(Args, Qu),
                        Question = [Qu2] ++
                            [add_to_proplist(?Macro:name(M)) || M <- Macros],
                        Ans = ?Transform:question(Question),
                        NArgs ++ [{macro, get_data(Ans, Question)}];
                    _     ->
                        NArgs
                end;
            false ->
                Args
        end,

    ?CallArg(NewArgs, "Macro",
             [{fun macbyname/3, [file, macro, ask_missing]},
              {fun macbypos/3, [file, position, ask_missing]},
              {fun macnode/1, [nodes]}]).

macnode([Node])-> %@todo verify
    Node.

add_to_proplist(Name) ->
    [{format,radio},
     {text,lists:flatten(Name)},
     {default,false}].

add_to_proplist2(Name) ->
    [{format,checkbox},
     {text, lists:flatten(Name)},
     {default,false}].

get_data(Ans, Question) ->
    EntInList = proplists:get_value(yes,lists:zip(Ans,Question),no_answer),
    case EntInList == no_answer of
        false ->
            Entity = proplists:get_value(text,EntInList),
            Entity;
        true  ->
            throw(?RefErr0r(cancelled))
    end.

get_datas(Ans, Question) ->
    ValidAns = proplists:get_all_values(yes, lists:zip(Ans,Question)),
    case ValidAns of
        []           -> throw(?RefErr0r(cancelled));
        EntsInList ->
            [proplists:get_value(text, EntInList) || EntInList <- EntsInList]
    end.

macbyname(File, Macro, AskMissing) ->
    Mac = ?Query:exec(
              ?Query:seq([?File:find(File),
                          ?Macro:find(Macro)])),

    case Mac of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  -> error_action(bad_macro_name, [File, Macro]);
                false -> throw(?RefError(mac_not_found, [Macro]))
            end
    end.

macbypos(File, Pos, AskMissing) ->
    T = ?Query:exec(
         ?Query:seq([?File:find(File),
                     ?File:token(Pos)])),

    case T of
        [_Result] ->
            case ?Query:exec([T], [{llex,back},mref]) of
                [Macro] -> Macro;
                []      -> Mac = ?Query:exec([T], ?Query:seq([
                                          [{{flex,back}, {type, '==', macro}}] ])),
                           case Mac of
                               [Res] -> Res;
                               _     ->
                                   case AskMissing of
                                       true  -> error_action(bad_macro_pos, File, Pos);
                                       false -> throw(?RefError(pos_bad_type, [mac, Pos]))
                                   end
                           end
            end;
        _        ->
            case AskMissing of
                true  -> error_action(bad_macro_pos, File, Pos);
                false -> throw(?RefError(pos_bad_type, [mac, Pos]))
            end
    end.

%% @spec macuse(arglist()) -> node()
%% @doc Returns the source macro use node. This value is specified in the
%% argument list with `file' and `position' keys.
macuse(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  ->
                NArgs = getFile(Args),
                case proplists:is_defined(position, NArgs) of
                    false ->
                        File = filebyname(proplists:get_value(file, NArgs), true),
                        Macros = ?Query:exec(File, ?Query:seq(?File:macros(), [{mref,back},{llex,2}])),
%                       MacUses = {mref,back} -> {llex,2}
                        ?Check(Macros /= [], ?RefError(no_macuse,[])),
                        Qu = [{format,info},{text,"Please specify a macro:"}],
                        {_Args2, Qu2} = get_info_text(Args, Qu),
                        Question = [Qu2] ++
                            [add_to_proplist(io_lib:fwrite("~s:~p",
                                                 [?Token:text(M),element(1,?Token:pos(File, M))]))
                                || M <- Macros],
                        Ans = ?Transform:question(Question),
                        Macro = get_data(Ans,Question),
                        [_, Pos] = string:tokens(Macro, ":"),
                        NArgs ++ [{position, list_to_integer(Pos)}];
                    true  ->
                        NArgs
                end;
            false ->
                Args
        end,

    ?CallArg(NewArgs, "Macro",
             [{fun macusebypos/3, [file, position, ask_missing]}]).

macusebypos(File, Pos, AskMissing) ->
    P= ?Query:exec(
         ?Query:seq([?File:find(File),
                     ?File:token(Pos),
                     [{llex,back}]])),

    case P of
        [_Result] ->
            case ?Query:exec([P], [mref]) of
                [_] -> P;
                []  ->
                    case AskMissing of
                        true  -> error_action(bad_macuse_pos, File, Pos);
                        false -> throw(?RefError(pos_bad_type, [mac, Pos]))
                    end
            end;
        _         ->
            case AskMissing of
                true  -> error_action(bad_macuse_pos, File, Pos);
                false -> throw(?RefError(pos_bad_type, [mac, Pos]))
            end
    end.

%% @spec macros(arglist()) -> [node()]
%% @doc Returns the source macro node list. This value is specified with
%% `file' and `maclist' keys in the argument list.
macros(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  ->
                NArgs = getFile(Args),
                case proplists:is_defined(maclist, NArgs) of
                    false ->
                        File = proplists:get_value(file, NArgs),
                        Macros =?Query:exec(?Query:seq(?File:find(File), ?File:macros())),
                        Qu = [{format,info},{text,"Please select the macros from the list:"}],
                        {_Args2, Qu2} = get_info_text(Args, Qu),
                        Question = [Qu2] ++ [add_to_proplist2(?Macro:name(Mac)) ||  Mac <- Macros],
                        Ans = ?Transform:question(Question),
                        NArgs ++ [{maclist, [list_to_atom(MacName) || MacName <- get_datas(Ans, Question)]}];
                    true  ->
                        NArgs
                end;
            false ->
                Args
        end,

    ?CallArg(NewArgs, "Macro list",
             [{fun macsbyname/2, [file, maclist]},
              {fun macnodes/1, [nodes]}]).

macnodes(Nodes)-> %@todo verify
    Nodes.

macsbyname(File, Macs) ->
    F = ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])),
    [?Query:exec1(F, ?File:macro(Mac),
                  ?RefError(mac_not_found, [Mac])) || Mac <- Macs].

%% @spec record(arglist()) -> node()
%% @doc Returns the source record node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`file' and `record' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
record(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  -> getRecord(Args);
            false -> Args
        end,

    ?CallArg(NewArgs, "Record",
             [{fun recbyname/3, [file, record, ask_missing]},
              {fun recbypos/3,  [file, position, ask_missing]},
              {fun recnode/1, [nodes]}]).

recnode([Node])-> %@todo verify
    Node.

recbyname(File, Record, AskMissing) ->
    Rec = ?Query:exec(
                 ?Query:seq([?File:find(File),
                             ?Rec:find(Record)])),

    case Rec of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  -> error_action(bad_rec_name, [File, Record]);
                false -> throw(?RefError(rec_not_found, [Record]))
            end
    end.

recbypos(File, Pos, AskMissing) ->
    [Token] = ?Query:exec(
                  ?Query:seq(?File:find(File),
                             ?File:token(Pos))),
    Rec = [Form || Form <- ?Query:exec(Token, ?Token:form()),
               ?Form:type(Form) == record], %rec-ben hasznalt rec?
    case ?Query:exec(Rec, ?Form:record()) of
        [Result] -> Result;
        []       -> Record = ?Query:exec(Token,
                              ?Query:seq([?Token:expr(),
                                         ?Expr:record()])),
                    case Record of
                        [Result] -> Result;
                        _        ->
                            case AskMissing of
                                true  -> error_action(bad_rec_pos, File, Pos);
                                false -> throw(?RefError(pos_bad_type, [rec, Pos]))
                            end
                    end
    end.

%% @doc This function asks the user to specify a record if it isn't specified yet.
getRecord(Args) ->
    NArgs = getFile(Args),

    NewArgs =
        case proplists:is_defined(record, NArgs) or proplists:is_defined(position, NArgs) of
            false ->
                File = filebyname(proplists:get_value(file, NArgs), true),
                Records = ?Query:exec(File, ?File:records()),
                Qu = [{format,info},{text,"Please specify a record:"}],
                {_Args2, Qu2} = get_info_text(Args, Qu),
                Question = [Qu2] ++
                    [add_to_proplist(io_lib:format("~p",[?Rec:name(R)])) || R <- Records],
                Ans = ?Transform:question(Question),
                NArgs ++ [{record, list_to_atom(get_data(Ans, Question))}];
            _     ->
                NArgs
        end,

    NewArgs.

%% @spec record_field(arglist()) -> node()
%% @doc Returns the source record field node. This value is
%% specified in the argument list with
%% <ul>
%%  <li>`file', `record' and `recfield' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>

record_field(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true ->
                NArgs = getRecord(Args),
                case proplists:is_defined(recfield, NArgs) or proplists:is_defined(position, NArgs) of
                    false ->
                        File = proplists:get_value(file, NArgs),
                        Record = recbyname(File, proplists:get_value(record, NArgs),
                                           ask_missing(NArgs)),
                        RecFields = ?Query:exec(Record, ?Rec:fields()),
                        Qu = [{format,info},{text,"Please specify a record field:"}],
                        {_Args2, Qu2} = get_info_text(Args, Qu),
                        Question = [Qu2] ++
                            [add_to_proplist(atom_to_list(?RecField:name(RF))) || RF <- RecFields],
                        Ans = ?Transform:question(Question),
                        NArgs ++ [{recfield, list_to_atom(get_data(Ans, Question))}];
                    _     ->
                        NArgs
                end;
            false ->
                Args
        end,

    ?CallArg(NewArgs, "Record field",
             [{fun recfieldbyname/4, [file, record, recfield, ask_missing]},
              {fun recfieldbypos/3,  [file, position, ask_missing]},
              {fun fldnode/1, [nodes]}]).

fldnode([Node])-> %@todo verify
    Node.

recfieldbyname(File, Record, Field, AskMissing) ->
    Rec = ?Query:exec1(
             ?Query:seq([?File:find(File),
                         ?Rec:find(Record)]),
             ?RefError(rec_not_found, Record)),

    RecField = ?Query:exec(Rec, ?Rec:field(Field)),
    case RecField of
        [Res] -> Res;
        _     ->
            case AskMissing of
                true  -> error_action(bad_recfield_name, File, Record);
                false -> throw(?RefError(recfld_not_found, [Record,Field]))
            end
    end.


recfieldbypos(File, Pos, AskMissing) ->
    [Token] = ?Query:exec(?Query:seq(?File:find(File), ?File:token(Pos))),
    case ?Query:exec(Token, ?Query:seq(?Token:typexp(), ?Expr:fielddef())) of
        [Field]  -> Field;
        []       -> RecField = ?Query:exec(Token,
                                    ?Query:seq(?Token:expr(), ?Expr:field())),
                    case RecField of
                        [Result] -> Result;
                        _         ->
                            case AskMissing of
                                true  -> error_action(bad_recfield_pos, File, Pos);
                                false -> throw(?RefError(pos_bad_type, [recfield, Pos]))
                            end
                    end
    end.

%% @spec records(arglist()) -> [node()]
%% @doc Returns the source record node list. This value is specified with
%% `file' and `reclist' keys in the argument list.
records(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  ->
                NArgs = getFile(Args),
                case proplists:is_defined(reclist, NArgs) of
                    false ->
                        File = proplists:get_value(file, NArgs),
                        Records =?Query:exec(?Query:seq(?File:find(File), ?File:records())),
                        Qu = [{format,info},{text,"Please select the records from the list:"}],
                        {_Args2, Qu2} = get_info_text(Args, Qu),
                        Question = [Qu2] ++ [add_to_proplist2(atom_to_list(?Rec:name(Rec))) ||  Rec <- Records],
                        Ans = ?Transform:question(Question),
                        NArgs ++ [{reclist, [list_to_atom(RecName) || RecName <- get_datas(Ans, Question)]}];
                    true  ->
                        NArgs
                end;
            false ->
                Args
        end,

    ?CallArg(NewArgs, "Record list",
             [{fun recsbyname/2, [file, reclist]},
              {fun recnodes/1, [nodes]}]).

recnodes(Nodes)-> %@todo verify
    Nodes.

recsbyname(File, Recs) ->
    F = ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])),
    [?Query:exec1(F, ?File:record(Rec),
                  ?RefError(rec_not_found, [Rec])) || Rec <- Recs].

%% @spec variable(arglist()) -> node()
%% @doc Returns the source variable node. This value is specified with
%% `file' and `position' keys in the argument list.
variable(Args) ->
    ?CallArg(Args, "Variable",
             [{fun varbypos/2, [file, position]},
              {fun varnode/1, [nodes]}]).

varnode([Node])-> %@todo verify
    Node.

varbypos(File, Pos) ->
    ?Query:exec1(
        ?Query:seq([?File:find(File),
                    ?File:token(Pos),
                    ?Token:expr(),
                    ?Expr:variables()]),
        ?RefError(pos_bad_type, ['variable',Pos])).

variable(Args, Transformation) ->
    NArgs = getFile(Args),
    NewArgs = NArgs ++ [{transform, Transformation}],

    ?CallArg(NewArgs, "Variable",
             [{fun varbypos/4, [file, position, ask_missing, transform]},
              {fun varnode/1, [nodes]}]).

var_question(V, F) ->
    [File] = ?Query:exec(?File:find(F)),
    [Token] = ?Query:exec(V, ?Query:seq(?Var:bindings(), [elex])),
    {_, Pos} = ?Token:pos(File, Token),
    add_to_proplist(io_lib:format("~s~s~p", [?Var:name(V), ":", Pos])).

varbypos(File, Pos, AskMissing, Transformation) ->
    Variable = ?Query:exec(
                   ?Query:seq([?File:find(File),
                               ?File:token(Pos),
                               ?Token:expr(),
                               ?Expr:variables()])),
    case Variable of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  ->
                    error_action(bad_var_pos, File, Pos),
                    FunCl = ?Query:exec1(
                                ?Query:seq([?File:find(File),
                                            ?File:token(Pos),
                                            ?Token:expr(), ?Expr:clause()]),
                                ?RefError(pos_bad_type, ['variable',Pos])),
                    FunForm = ?Query:exec(FunCl, ?Clause:form()),
                    AllVars = ?Query:exec(FunForm,
                                   ?Query:seq([?Form:clauses(),
                                               ?Clause:variables()])),
                    PatternVars = ?Query:exec(FunForm,
                                       ?Query:seq([?Form:clauses(),
                                                   ?Clause:patterns(),
                                                   ?Expr:varbinds()])),
                    Vars =
                        case Transformation of
                            eliminate -> AllVars -- PatternVars;
                            rename    -> AllVars;
                            _         -> throw(?RefErr0r(unknown_exception))
                        end,
                    ?Check(Vars /= [], ?RefError(no_elim_var, [])),
                    Qu = [{format,info},{text,"Please specify a variable:"}],
                    Question = [Qu] ++
                               [var_question(V, File) || V <- Vars],
                    Ans = ?Transform:question(Question),
                    Var = get_data(Ans, Question),
                    [_, Position] = string:tokens(Var, ":"),
                    variable([{file,File},{position,list_to_integer(Position)},
                              {ask_missing,true}], eliminate);
                false -> throw(?RefError(pos_bad_type, ['variable',Pos]))
            end
    end.

%% @spec varname(arglist()) -> string()
%% @doc Returns the target variable name. This value is specified with
%% a `varname' key in the argument list.
varname(Args) ->
    DefText = "Please type in a new variable name:",
    NewArgs = ask_missing(varname, Args, variable, DefText),
    ?CallArg(NewArgs, "Target variable name",
             [{fun varstr/2, [varname, ask_missing]}]).

%% Displays an error message and requests new input in a textbox.
%% If the `info_text' property is set in `Args', it is used as
%% an additonal message; otherwise, only `DefText' is shown.
%% If `Validator' is `none', it is unused.
%% The resulting value is converted by `ConverterFun'.
ask_missing(Name, Args, Validator, DefText) ->
    ask_missing(Name, Args, Validator, DefText, fun(X) -> X end).

ask_missing(Name, Args, Validator, DefText, ConverterFun) ->
    case ask_missing(Args) andalso (not proplists:is_defined(Name, Args)) of
        false ->
            Args;
        true  ->
            Val =
                case Validator of
                    none -> [];
                    _    -> [{validator,Validator}]
                end,
            Textbox =
                [{format,textbox},
                 {text, DefText}] ++
                Val ++
                [{default,-1}],
            {Args2, Textbox2} = get_info_text(Args, Textbox),
            [NewValue] = ?Transform:question([Textbox2]),
            [{Name, ConverterFun(NewValue)} | proplists:delete(missing_text, Args2)]
    end.

%% If `Args' has an `info_text' property, removes it and puts it before
%% the `text' of the `Textbox'.
%% It also prefixes the text with `transformation_text' from `Args'
%% (and does not remove this element).
get_info_text(Args, Textbox) ->
    Text    = proplists:get_value(text, Textbox),
    TrTexts = proplists:get_all_values(transformation_text, Args),
    TrText  = ?MISC:join(lists:reverse(TrTexts), "\n"),
    NewText =
        case proplists:get_value(info_text, Args) of
            undefined -> ?MISC:flatjoin([TrText, Text], "\n");
            InfoText  -> ?MISC:flatjoin([TrText, InfoText, Text], "\n")
        end,
    NewTextbox = [case Elem of
                    {text, _} -> {text, NewText};
                    _         -> Elem
                  end || Elem <- Textbox],
    NewArgs    = proplists:delete(info_text, Args),
    {NewArgs, NewTextbox}.

varstr(Var, AskMissing) ->
    case ?Var:valid_name(Var) of
        true  -> Var;
        false ->
            case AskMissing of
                true  -> error_action(bad_var_name, Var);
                false -> throw(?RefErr0r(bad_var_name))
            end
    end.

%% @spec expression(arglist()) -> node()
%% @doc Returns the source expression. This value is specified with
%% `file' and `position' keys in the argument list.
expression(Args) ->
    ?CallArg(Args, "Source expression",
             [{fun exprbypos/2, [file, position]},
              {fun exprnode/1, [nodes]}]).

exprnode([Node])-> %@todo verify
    Node.

exprbypos(File, Pos) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?File:token(Pos),
                   ?Token:expr()]),
       ?RefError(token_parent, [expr])).

%% @spec file(arglist()) -> node()
%% @doc Returns the source file node. This value is specified with
%% a `file' key in the argument list.
file(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  -> getFile(Args);
            false -> Args
        end,

    ?CallArg(NewArgs, "Source file",
             [{fun filebyname/2, [file, ask_missing]},
              {fun filenode/1, [nodes]}]).

filenode([Node])-> %@todo verify
    Node.

filebyname(File, AskMissing) ->
    F = ?Query:exec(?File:find(File)),

    case F of
        [Result] -> Result;
        _         ->
            case AskMissing of
                true  -> error_action(bad_file, File);
                false -> throw(?RefError(file_not_present, [File]))
            end
    end.


%% @spec form(arglist()) -> node()
%% @doc Returns the form node. This value is specified with
%% a `file' and `position' keys in the argument list.
form(Args) ->
    ?CallArg(Args, "Form", [{fun formbypos/2, [file, position]}]).

formbypos(File, Pos) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?File:token(Pos),
                   ?Token:form()]),
       ?RefError(illegal_pos, [File, Pos])).

import_form(Args) ->
    NewArgs = getFile(Args),

    ?CallArg(NewArgs, "Import form",
            [{fun import_form_bypos/3, [file, position, ask_missing]},
             {fun impnode/1, [nodes]}]).

impnode([Node])-> %@todo verify
    Node.

import_form_bypos(File, Position, Interaction) ->
    Form = ?Query:exec1(
               ?Query:seq([?File:find(File),
                           ?File:token(Position),
                           ?Token:form()]),
               ?RefError(illegal_pos, [File, Position])),

    case ?Form:type(Form) == import of
        true        -> Form;
        false       ->
            case Interaction of
                true  -> error_action(bad_importform_pos, File, Position),
                         Forms = ?Query:exec(
                                     ?Query:seq([?File:find(File),?File:forms()])),
                         Imports = lists:filter(fun(F) -> ?Form:type(F) == import end, Forms),
                         Qu = [{format,info},{text,"Please specify an import:"}],
                         Question = [Qu] ++
                                    [add_import_form_question(F) || F <- Imports],
                         Ans = ?Transform:question(Question),
                         [[Pos], _, _] = string:tokens(get_data(Ans, Question),":"),
                         import_form([{file, File},{position,list_to_integer(Pos)},{ask_missing,true}]);
                false -> throw(?RefError(illegal_pos, [File, Position]))
            end
    end.

add_import_form_question(Form) ->
    [Module] = ?Query:exec(Form, ?Form:expr(1)),
    [FormToken] = ?Query:exec(Form, [{flex,2}]),
    Funlist = ?Query:exec(Form, ?Query:seq([?Form:expr(2),?Expr:children(),?Expr:function()])),
    FunListText = [list_to_atom(atom_to_list(?Fun:name(F)) ++ "/" ++ integer_to_list(?Fun:arity(F)))
                  || F <- Funlist],
    [{format,radio},{text,io_lib:fwrite("~p:~s:~p",[element(1,?Token:pos(FormToken)),
                                                    ?Expr:value(Module),FunListText])}].


%% This function asks the user to specify a source file if it isn't specified yet.
getFile(Args) ->
   case (not ask_missing(Args)) orelse proplists:is_defined(file, Args) of
        false ->
            [NewFile] = ?Transform:question([[{format,textbox},
                                             {text, "Please type in the path of source file:"},
                                             {validator,file},
                                             {default,-1}]]),
            NewArgs = Args ++ [{file, NewFile}],
            NewArgs;
        _     ->
            Args
    end.

%% @spec macname(arglist()) -> string()
%% @doc Returns the target macro name. This value is specified with
%% a `macname' key in the argument list.
macname(Args) ->
    DefText = "Please type in a new macro name:",
    NewArgs = ask_missing(macname, Args, macro, DefText, fun convert_mac_name/1),
    ?CallArg(NewArgs, "Target macro name", [{fun macrostr/1, [macname]}]).


convert_mac_name(NewName) ->
    case string:to_upper(hd(NewName)) =:= hd(NewName) of
        false -> list_to_atom(NewName);
        _     -> NewName
    end.

macrostr(Name) when is_atom(Name) ->
    io_lib:write(Name);
macrostr(Name) when is_list(Name) ->
    case ?Var:valid_name(Name) of
        true  -> Name;
        false -> throw(?RefErr0r(bad_mac_name))
    end.

%% @spec module(arglist()) -> node()
%% @doc Returns the source module node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>a `module' key, or</li>
%%  <li>a `file' key.</li>
%% </ul>
module(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  -> getModule(Args);
            false -> Args
        end,

    ?CallArg(NewArgs, "Source module",
             [{fun modbyname/2, [module, ask_missing]},
              {fun modbypos/3,  [file, position, ask_missing]},
              {fun modbyfile/2, [file, ask_missing]},
              {fun modnode/1, [nodes]}]).

modnode([Node])-> %@todo verify
    Node.

getModule(Args) ->
    case proplists:is_defined(module, Args) or proplists:is_defined(file, Args) of
        false ->
            Question =
                [{format,textbox},
                 {text, "Please type in the module name:"},
                 {validator,module},
                 {default,-1}],
            {_, Question2} = get_info_text(Args, Question),
            [NewMod] = ?Transform:question([Question2]),
            [{module, list_to_atom(NewMod)} | Args];
        true  ->
            Args
    end.

modbyname(Mod, AskMissing) ->
    Module = ?Query:exec(?Mod:find(Mod)),

    case Module of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  -> error_action(bad_mod_name, Mod);
                false -> throw(?RefError(mod_not_found, [Mod]))
            end
    end.

modbypos(File, Pos, AskMissing) ->
    Mod = ?Query:exec(
              ?Query:seq( [?File:find(File),
                           ?File:token(Pos),
                           ?Token:expr(),
                           ?Expr:module() ])),

    case Mod of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  -> error_action(bad_mod_pos, File, Pos);
                false -> throw(?RefError(pos_bad_type, [module, Pos]))
            end
    end.

modbyfile(File, AskMissing) ->
    Mod = ?Query:exec(
             ?Query:seq([?File:find(File),
                         ?File:module()])),

    case Mod of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  -> error_action(bad_file, File);
                false -> throw(?RefError(file_not_module, [File]))
            end
    end.

% @todo document
moduse(Args) ->
    NewArgs =
        case ask_missing(Args) of
            true  ->
                NArgs = getFile(Args),
                case proplists:is_defined(position, Args) or proplists:is_defined(module, Args) of
                    false ->
                        File = filebyname(proplists:get_value(file, NArgs), false),
                        [SourceMod] = ?Query:exec(File, ?File:module()),
                        Modules = ?Query:exec(File,
                                      ?Query:seq([?File:forms(),?Form:func(),
                                                  ?Fun:funcalls(),?Fun:module()])),
                        ?Check(Modules /= [], ?RefError(no_moduse, [])),
                        ModUses = lists:filter(fun(M) -> ?Mod:name(M) /= ?Mod:name(SourceMod) andalso
                                                         ?Mod:name(M) /= erlang end, Modules),
                        ?Check(ModUses /= [], ?RefError(no_moduse, [])),
                        Qu = [[{format,info},{text,"Please specify a module to import:"}]],
                        Question = Qu ++ [add_to_proplist(atom_to_list(?Mod:name(ModUse)))
                                          || ModUse <- ModUses],
                        Ans = ?Transform:question(Question),
                        NArgs ++ [{module,list_to_atom(get_data(Ans,Question))}];
                    true -> NArgs
                end;
            false -> Args
        end,
    ?CallArg(NewArgs, "Module usage",
             [{fun modusebyname/2, [module, ask_missing]},
              {fun modusebypos/3,  [file, position, ask_missing]},
              {fun modusenode/1, [nodes]}]).

modusenode([Node])-> %@todo verify
    Node.

modusebyname(Mod, AskMissing) ->
    Module = ?Query:exec(?Mod:find(Mod)),

    case Module of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  -> error_action(bad_moduse_name, Mod);
                false -> throw(?RefError(mod_not_found, [Mod]))
            end
    end.

modusebypos(File, Pos, AskMissing) ->
    Mod = ?Query:exec(
              ?Query:seq( [?File:find(File),
                           ?File:token(Pos),
                           ?Token:expr(),
                           ?Expr:module() ])),

    case Mod of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  -> error_action(bad_moduse_pos, File, Pos);
                false -> throw(?RefError(pos_bad_type, [module, Pos]))
            end
    end.

%% @spec expr_range(arglist()) -> [node()]
%% @doc Returns the source expression range. A valid expression range is
%% either a non-empty continuous list of top level expressions linked with the
%% same tag to a clause, or a single expression. This value is specified with
%% `file' and `posrange' keys in the argument list.
expr_range(Args) ->
    ?CallArg(Args, "Expression range",
             [{fun expr_range/2,   [file, posrange]},
              {fun expr_posnumr/3, [file, position, number]},
              {fun exprnodes/1, [nodes]}]).

exprnodes(Nodes)-> %@todo verify
    Nodes.

expr_posnumr(File, Pos1, Num) ->
    ?Check(Num>=1, ?RefErr0r(bad_range)),
    IllP  = fun(Loc,Path)-> ?Query:exec1(Loc,Path,
                            ?RefError(illegal_pos, [File, Pos1])) end,
    F      = get_filenode(File),
    Start  = IllP(F, ?File:token(Pos1)),
    StPat  = IllP(Start, [{elex,back}]),
    FunCl  = IllP(StPat, [{pattern,back}]),
    FunDef = IllP(FunCl, [{funcl,back}, fundef]),
    Arity  = ?Fun:arity(FunDef),
    ?Check(Num=<Arity, ?RefErr0r(bad_range)),
    IsIdx  = fun(I) -> StPat =:= IllP(FunCl, [{pattern,I}]) end,
    {Idx,_Pos} = ?MISC:list_find(IsIdx, lists:seq(1,Arity-Num+1)),
    EndIdx = Idx+Num-1,
    ?Check((Idx>0) andalso (EndIdx=<Arity), ?RefErr0r(bad_range)),
    EndE   = IllP(FunCl, [{pattern,EndIdx}]),
    End    = hd(lists:reverse(?Query:exec(EndE,[elex]))),
    expr_range_common(Start, End).

expr_range(File, {Pos1, Pos2}) ->
    F     = get_filenode(File),
    Start = ?Query:exec1(F, ?File:token(Pos1),
                         ?RefError(illegal_pos, [File, Pos1])),
    End   = ?Query:exec1(F, ?File:token(Pos2),
                         ?RefError(illegal_pos, [File, Pos2])),
    expr_range_common(Start, End).

get_filenode(File) ->
    ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])).

expr_range_common(Start, End) ->
    Exprs = range(joint(?Syn:root_path(Start, left),
                        ?Syn:root_path(End, right))),
    [First] = ?Query:exec(hd(Exprs), ?Syn:first_leaf()),
    [Last]  = ?Query:exec(lists:last(Exprs), ?Syn:last_leaf()),
    ?Check(First =:= Start andalso Last =:= End,
           ?RefErr0r(bad_range)),
    Exprs.

%% Turns the result of `joint/2' into a list of expressions.
range({Expr, esub, F, L}) ->
    Chld = ?Syn:children(Expr),
    case {hd(Chld), lists:last(Chld)} of
        {{esub,F}, {esub,L}} -> [Expr];
        _ ->
            FI = ?Syn:index(Expr, esub, F),
            LI = ?Syn:index(Expr, esub, L),
            ?ESG:path(Expr, [{esub, {FI, LI+1}}])
    end;
range({Expr, elex, _, _}) ->
    [Expr];
range({Cls,  CT, F, L}) when CT == body; CT == pattern; CT == guard ->
    FI = ?Syn:index(Cls, CT, F),
    LI = ?Syn:index(Cls, CT, L),
    ?ESG:path(Cls, [{CT, {FI, LI+1}}]);
range({Lex, llex, _, _}) ->
    %% No multiple parents here
    [{Tag, Parent}] = ?Syn:parent(Lex),
    range({Parent, Tag, Lex, Lex});
range(_) ->
    throw(?RefErr0r(bad_range)).

%% @spec joint(P1::[{atom(), node()}], P2::[{atom(), node()}]) ->
%%         {node(), atom(), node(), node()}
%% @doc Returns the fork point of the argument paths. `P1' and `P2' must have
%% a common prefix which is continued with the same link tag (the prefix may
%% be empty). The returned tuple contains the last common node after the
%% longest such prefix, the common link tag, and the continuing nodes (they
%% may be the same when the two paths are continued with different link tags).
joint([{L,N}=H|T1], [H|T2])     -> joint(T1, T2, {?Graph:root(), L, N});
joint([{L,N1}|_],   [{L,N2}|_]) -> {?Graph:root(), L, N1, N2};
joint(_, _)                     -> throw(?RefErr0r(bad_range)).

%% Drop the common prefix. The third argument contains the last two common
%% nodes and the link tag between them.
joint([{L,N}=H|T1], [H|T2],      {_,_,P}) -> joint(T1, T2, {P,L,N});
joint([{L, N1}|_],  [{L, N2}|_], {_,_,P}) -> {P, L, N1, N2};
joint(_,           _,            {P,L,C}) -> {P, L, C, C}.
