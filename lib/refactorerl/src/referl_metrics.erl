%%% -*- coding: latin-1 -*-
%%% PLACEHOLDER FOR LICENSE TEXT

%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>


%%% @doc <b>Metric Query Language</b>
%%%
%%% The query language is relatively simple and easy to expand 
%%% both regarding syntax and the list of metrics that can be 
%%% implemented in this module.
%%%
%%% Usage in command line:
%%% 
%%% Metric Query::show metric_function for nodetype nodelist 
%%%                                                    aggregation
%%%
%%% metric functions::
%%%        line_of_code | char_of_code | number_of_fun
%%%        | number_of_macros | number_of_records
%%%        |included_files | imported_modules
%%%        |number_of_funpath | function_calls_in
%%%        |function_calls_outok | cohesion,
%%%        |otp_used | function_sum | max_application_depth
%%%        |max_depth_of_calling | min_depth_of_calling
%%%        |max_depth_of_cases | number_of_funclauses
%%%        |branches_of_recursion | mcCabe
%%%        |calls_for_function | calls_from_function
%%%        |number_of_funexpr | number_of_messpass
%%%        |fun_return_points | average_size
%%%
%%% nodetype::module | function
%%% 
%%% nodelist::('modname',...)
%%%           |({'modname','funname',arity},...)
%%% 
%%% aggregation::
%%%          sum|min|max|fmaxname|avg|tolist|maxlist
%%% 
%%% 
%%% Implemented metrics:
%%% 
%%% <b>Effective Line of code</b> The number of the lines of part
%%%  of the text, function, or module. The number of empty lines is not
%%%  included in the sum. As the number of lines can be measured on more
%%%  functions, or modules and the system is capable of returning the
%%%  sum of these, the number of lines of the whole loaded program text
%%%  can be enquired.
%%% 
%%% <b>Characters of the code</b> The number of characters in a program
%%%  text. This metric is capable of measuring both the codes of functions
%%%  and modules and with the help of aggregating functions we can enquire
%%%  the total and average number of characters in a cluster, or in the
%%%  whole source text.
%%%
%%% <b>Number of functions</b> This metric gives the number of functions
%%%  implemented in the concrete module, but it does not contain the number
%%%  of non-defined functions in the module.
%%%
%%% <b>Number of macros</b> This metric gives the number of defined macros
%%%  in the concrete module, or modules. It is also possible to enquire the
%%%  number of implemented macros in a module.
%%% 
%%% <b>Number of records</b> This metric gives the number of defined
%%%  records in a module. It is also possible to enquire the number of
%%%  implemented records in a module.
%%% 
%%% <b>Number of included files</b> This metric gives the number of visible
%%%  header files in a module.
%%% 
%%% <b>Imported modules</b> This metric gives the number of imported modules
%%%  used in a concrete module. The metric does not contain the number of
%%%  qualified calls (calls that have the following form: 
%%%  `module:function'.
%%% 
%%% <b>Number of funpath</b> The total number of function paths in a module.
%%%  The metric, besides the number of internal function links, also
%%%  contains the number of external paths, or the number of paths that
%%%  lead outward from the module. It is very similar to the metric called
%%%  cohesion.
%%% 
%%% <b>Function calls into the module</b> Gives the number of function calls
%%%  into a module from other modules. It can not be implemented to measure
%%%  a concrete function. For that we use the `calls_for' function.
%%% 
%%% <b>Function calls from the module</b> Gives the number of every function
%%%  call from a module towards other modules. It can not be implemented to
%%%  measure a concrete function. For that we use the `calls_from' function.
%%% 
%%% <b>Cohesion of the module</b> The number of call-paths of functions
%%%  that call each other. 
%%% 
%%% <b>Function sum</b> The sum calculated from the functions complexity
%%%  metrics that characterizes the complexity of the function. It can
%%%  be calculated using various metrics together. 
%%%  We can define metrics that are necessary to calculate the metrics
%%%  constituting the sum.
%%% 
%%% <b>OTP used</b> Gives the number of OTP callback modules used in modules.
%%%  We are planning to expand the function, besides measuring the number
%%%  of modules, to also measure the number of implemented callback functions.
%%%
%%% <b>Max depth of calling</b> The length of function call-paths, namely
%%%  the path with the maximum depth. It gives the depth of non-recursive
%%%  calls.
%%%
%%%  <b>Depth of applications</b> Maximum depth of embedded function 
%%%  applications in the module `Mod' or the function `Fun'.
%%% 
%%% <b>Min depth of calling</b> The minimum depth of functions embedding
%%%  within a module. It gives the depth of non-recursive calls.
%%% 
%%% <b>Min depth of cases</b> Gives the minimum of `case' control
%%%  structures embedded in case of a given function. In case of a
%%%  module it measures the same regarding all of the functions in
%%%  the module.
%%% 
%%% <b>Max dept of cases</b> Gives the maximum of case control structures
%%%  embedded in `case' of a concrete function (how deeply are the case
%%%  control structures embedded). In case of a module it measures the
%%%  same regarding all the functions in the module. Measuring does not
%%%  break in case of `case-expressions', namely when the `case' is not
%%%  embedded into a `case' structure.
%%%  
%%% <b>Number of funclauses</b> Gives the number of a functions clauses.
%%%  Counts all distinct branches, but does not add the functions
%%%  having the same name, but different arity, to the sum. 
%%% 
%%% <b>Branches of recursion</b> Gives the number of a certain function's
%%%  branches, how many times a function calls itself, and not the number
%%%  of clauses it has besides definition.
%%% 
%%% <b>McCabe</b> McCabe cyclomatic complexity metric. We define the
%%%  complexity metric in a control flow graph with the number of defined
%%%  basic edges, namely the number of outputs a function can have
%%%  disregarding the number of function outputs functions within the
%%%  function can have. Functions called each count as one possible output. 
%%%
%%% <b>Calls for the function</b> This metric gives the number of calls
%%%  for a concrete function. It is not equivalent with the number of
%%%  other functions calling the function, because all of these other
%%%  functions can refer to the measured one more than once. 
%%% 
%%% <b>Calls from the function</b> This metric gives the number of calls
%%%  from a certain function, namely how many times does a function refer
%%%  to another one (the result includes recursive calls as well).
%%% 
%%% <b>Number of funexpr</b> Gives the number of function expressions
%%%  in a given module. It does not measure the call of function expressions,
%%%  only their initiation. 
%%% 
%%% <b>Number of message passing</b> In case of functions it measures the
%%%  number of code snippets implementing messages from a function, while
%%%  in case of modules it measures the total number of messages in all of
%%%  the modules functions.
%%%
%%% <b>Average size</b> The average value of the given complexity metrics 
%%% (e.g. Average `branches_of_recursion' calculated from the functions of
%%% the given module).
%%%
%%% <b>Function return points</b> The metric gives the number of the functions
%%%  possible return points or the functions of the given module.
%%% 
%%% <b>Aggregation filters:</b>
%%% 
%%% max      : maximum on the result list
%%% 
%%% tolist   : default return value of the query
%%% 
%%% fmaxname : maximum with the name of the node
%%%
%%% maxlist  : all the elements where the value equals to maximum
%%% 
%%% avg      : average on the result list
%%% 
%%% min      : minimum of the result list
%%% 
%%% sum      : sum of the result list
%%%
%%% <b>Usage in the Emacs interface:</b>
%%% 
%%% M-x refactorerl-metrics-query
%%% 
%%% show max_depth_of_cases for function ({'a','f',1},{'a','f',2})
%%% 
%%% show number_of_funpath for module ('a','b')
%%%
%%% show number_of_fun for module ('a','b') sum

-module(referl_metrics).
-vsn("$Rev: 3687 $ ").

-export([cmd/1, prepare/1, run/1, batch/1, maxkeys2/1]).
%-compile(export_all).

-include("refactorerl.hrl").

-define(LEXER, referl_m_lexer).
-define(PARSER, referl_m_parser).

%%% @private
prepare([{_, Query}])->
    case cmd(Query) of
        {list, List} ->
            ?UI:message(
               metric,
               "Query: ~s~n"
               "Result:~n"
               "~s",
               [Query,
                [["   ", createText(E), "\n"] || E <- List]]);
        {Agr, Data} ->
            ?UI:message(
               metric,
               "Query: ~s~n"
               "Result:~n"
               "~s",
               [Query,
                ["   ", createText({Agr, Data})]]);
        SE ->
            ?UI:message(metric, "~s", [SE])
    end,
    fun () -> nomsg end.


%%% @private
%{[{1,"a"},{1,"testap2"}]}
numformat({Data}) when is_list(Data) ->
   concat("", Data);
numformat({Num, {Module, Function, Arity}}) ->
    "("++Module++":"++Function++"/"++integer_to_list(Arity)
    ++" with "++integer_to_list(Num)++")";
numformat({Num, Name}) ->
    "("++Name++" with "++numformat(Num)++")";
numformat(Num) when is_integer(Num) ->
    integer_to_list(Num);
numformat(Num) when is_float(Num) ->
    float_to_list(Num).

%%% @private
concat(Acc, [{N, {Mod, Fun, Arity}}|Ls])->
    concat(Acc++" {"++Mod++":"++Fun
              ++"/"++integer_to_list(Arity)++","
              ++integer_to_list(N)++"}", Ls);
concat(Acc, [{N, Name}|Ls])->
    concat(Acc++" ("++Name++" "
    ++integer_to_list(N)++")", Ls);
concat(Acc, []) ->
    lists:flatten(Acc).

%%% @private
createText({ResultList}) 
          when is_list(ResultList)->
    "Maximum elements of the result: "++numformat(ResultList);
createText({Agr, Data})->
    atom_to_list(Agr)++" is "++numformat(Data);
createText({{Module, Function, Arity}, _, nonode})->
    Module++":"++Function++"/"++integer_to_list(Arity)
    ++" is not found";
createText({{Module, Function, Arity}, Me, NumON})->
    atom_to_list(Me)++" of the '"++Module++":"++Function++"/"
    ++integer_to_list(Arity)++"' is "++numformat(NumON);
createText({Name, _, nonode})->
    "'"++Name++"' is not found";
createText({Name, Me, NumON})->
    atom_to_list(Me)++" of the '"++Name
    ++"' is "++numformat(NumON).

%%% @spec run({module, Metirc::atom(), Filter::atom()})->
%%%                                            Result::term()
run({module, Metric, Filter})->
   Modules = referl_esg:path(referl_esg:root(),[module]),
   Mods = [atom_to_list(?Mod:name(Mod))|| Mod <- Modules],
   use([Metric], module, Mods, Filter);

%%% @spec run({NodeType::atom(), ModuleName::atom(), 
%%%                      Metirc::[atom()], Filter::atom()})->
%%%                                            Result::term()
run({NodeType, Nodes, Metrics, Filter}) ->
    use(Metrics, NodeType, Nodes, Filter).

%%% @private
use([Metric|Metrics], NodeType, Node, Filter)->
    io:format("~s : ~p~n",[Metric,
           referl_metrics:preQuery(
                    {{show, atom_to_list(Metric), 
                          NodeType, Node}, atom_to_list(Filter)})]),
    use(Metrics, NodeType, Node, Filter);
use([], _, _, _) ->
    ok.

%%% @spec batch(BatchList::list()) -> Result::term()
%%% 
%%% @doc This function can run all metric function defined
%%%  in `BatchList' on all module in the database.
batch(BatchList)->
    [begin
      run({module, Metric, tolist}),
      run({module, Metric, sum}),
      run({module, Metric, max}),
      run({module, Metric, min}),
      run({module, Metric, avg}),
      run({module, Metric, fmaxname})
     end || Metric <- BatchList].

%%% @spec cmd(Query::string()) -> string()
%%% @doc Interface function of the command line interface.
cmd(Query) ->
     case ?LEXER:string(Query) of
          {ok, Tokens, _EndL} -> 
          case ?PARSER:parse(Tokens) of
               {ok, PrT} -> preQuery(PrT);
	       {error, Err} -> format_error({parser_error, Err})
          end;
	  {error, Err, _LNum} -> format_error({scanner_error, Err})
     end.

format_error({parser_error, {_,_, Mesg}})->
    throw(?RefError(m_parser_error, [Mesg]));
format_error({scanner_error, {_,_, Err}}) ->
    throw(?RefError(m_scanner_error, [Err]));
format_error({bad_command, Command})->
    throw(?RefError(bad_command, [Command]));
format_error({bad_filter, Filter}) ->
    throw(?RefError(bad_filter, [Filter]));
format_error({metric_fun, Metric}) ->
    throw(?RefError(metric_fun, [Metric]));
format_error(Other) ->
    case io_lib:deep_char_list(Other) of
	true -> throw(?RefError(m_unknown_error, [Other]));
	   _ -> throw(?RefError(m_unknown_error, [io_lib:write(Other)]))
    end.

%%% @spec preQuery({{atom(), Metric::string(), NodeType::atom(),
%%%                                 Ident::list()},Filter::atom()}) 
%%%                                               -> ResultList::list()
%%%                                                  | {error, Reason}
%%% @private
preQuery({{show, Metric, NodeType, Ident}, Filter}) ->
    execQuery({Metric, NodeType, Ident, Filter});
preQuery({show, Metric, NodeType, Ident}) ->
    execQuery({Metric, NodeType, Ident, tolist});
preQuery({save, Metrics, TopTree}) ->
    save({save, Metrics, TopTree});
%TODO: Handling other types of the messages
preQuery(Command) -> 
    format_error({bad_command, Command}).
      
%%% @private
execQuery({Metric, NodeType, IdentList, Filter}) ->
    Nodes = find_node(NodeType, IdentList),
    ResultList = [show({Metric, NodeType, Node})|| Node <- Nodes, 
                                                       Node /= error],
    agreg(Filter, ResultList).

%%% @private
agreg(tolist, Result)->
    {list, Result};

agreg(max, Result)->
    RList = [Num || {_, _, Num} <- Result, is_integer(Num)],
    {max, lists:max(RList)};

agreg(fmaxname, Result)->
    RList = [{Num, Name}|| {Name, _, Num} <- Result, is_integer(Num)],
    {Name, Max} = maxkey(RList),
    {fmaxname, {Max, Name}};

agreg(min, Result)->
    RList = [Num || {_, _, Num} <- Result, is_integer(Num)],
    {min, lists:min(RList)};

agreg(sum, Result) -> 
    RList = [Num || {_, _, Num} <- Result, is_integer(Num)],
    {sum, lists:foldl(fun(X, Sum) -> X + Sum end, 0, RList)};

agreg(avg, Result) ->
    Rd = [R || {_, R, Num} <- Result, Num =/= 0],
    %TODO DB where element of the tuple is not 0.
    RList = [Num || {_, _, Num} <- Result, is_integer(Num), Num =/= 0],
    NData = lists:foldl(fun(X, Sum) -> X + Sum end, 0, RList),         
    {avg, NData/length(Rd)};

agreg(maxlist, Result)->
    RList = [{Num, Name}|| {Name, _, Num} <- Result, is_integer(Num)],
    MaxList = maxkeys2(RList),
    {maxlist, MaxList};

agreg(Filter, _Result) ->
    format_error({bad_filter, Filter}).

%%% @private
maxkey(MTuples)->
    {L2, _L1} = lists:unzip(MTuples),
    Key = lists:max(L2),
    {_,{_, Name}} = lists:keysearch(Key, 1, MTuples),
    {Name, Key}.

%%% @private
maxkeys2(MTuples)->
    {L2, _L1} = lists:unzip(MTuples),
    Key = lists:max(L2),
    TList = lists:filter(
                fun({K, _})-> 
                     K == Key
                 end, MTuples),
    {lists:flatten(TList)}.

%%% @private
find_node(function, FunList) ->
    WList = [begin 
         [ModNode] = ?Query:exec(?Mod:find(list_to_atom(Mod))),
         {?Query:exec(ModNode, ?Fun:find(list_to_atom(Name),Arity)), {Mod, Name, Arity}} end 
                                          || {Mod, Name, Arity} <- FunList],
    extractList([],WList);
find_node(module, ModList) ->
       WList = [{?Query:exec(?Mod:find(list_to_atom(Mod))), Mod} 
                                                           || Mod <- ModList],
       extractList([],WList).

%%% @private
extractList(Acc,[Head|List])->
    case Head of
        {[], Name}     -> Acc1 = Acc++[{result, {nonode, Name}}];
	{[Node], Name} -> Acc1 = Acc++[{result, {Node, Name}}];
	Er     -> Acc1 = Acc++[{error, Er}]
    end,
    extractList(Acc1, List);
extractList(Acc,[]) ->
    Acc.

%%% @private
fun_sum()->
    [fun loc/1, fun choc/1, fun funclauses/1,
     fun b_recursion/1,
     fun mCb/1, fun calls_for/1, fun calls_from/1,
     fun fun_return_points/1, fun messpass/1].

%%% @private
show({Metric, NodeType, Node})->
   Fun = case Metric of
      module_sum            -> fun modsum/1;
      line_of_code          -> fun loc/1;
      char_of_code          -> fun choc/1;
      number_of_fun         -> fun nof/1;
      number_of_macros      -> fun num_of_macros/1;
      number_of_records     -> fun num_of_rec/1;
      included_files        -> fun inc_file/1;
      imported_modules      -> fun imp_mod/1;
      number_of_funpath     -> fun number_of_funpath/1;
      function_calls_in     -> fun fcalls_in/1;
      function_calls_out    -> fun fcalls_out/1;
      cohesion              -> fun cohesion/1;
      otp_used              -> fun otp_used/1;
      function_sum          -> fun fun_inside/1;
      max_application_depth -> fun max_application_depth/1;
      max_depth_of_calling  -> fun max_depth_of_calling/1;
      min_depth_of_calling  -> fun min_depth_of_calling/1;
      max_depth_of_cases    -> fun max_depth_of_cases/1;
      number_of_funclauses  -> fun funclauses/1;
      branches_of_recursion -> fun b_recursion/1;
      mcCabe                -> fun mCb/1;
      calls_for_function    -> fun calls_for/1;
      calls_from_function   -> fun calls_from/1;
      number_of_funexpr     -> fun funexp/1;
      number_of_messpass    -> fun messpass/1;
      fun_return_points     -> fun fun_return_points/1;
      average_size          -> fun average_size/1;
                          _ -> format_error({metric_fun, [Metric]})
   end,
   try 
     case Node of
             {result, {nonode, Name}} -> {Name, Metric, nonode};
             {result, {RNode, Name}}      -> {Name, Metric,
                                         result_handler(
                                            apply(Fun, [{NodeType, RNode}]))};
	     {error, Er}      -> format_error({error, Er})
     end
   catch  
     error:Err -> Err
   end.

%%% @private
result_handler(Data) when is_list(Data) ->
    length(Data);
result_handler(Data) when is_number(Data) ->
    Data.

%%% @private    
save(_List) ->
    throw("Not implemented").

%%% @private
loc({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    string:tokens(lists:flatten(referl_syntax:tree_text(File)),"\n");
loc({function, Fun})->
    [Def] = ?Query:exec(Fun, ?Fun:definition()),
    Text = ?Syn:tree_text(Def),
    string:tokens(lists:flatten(Text),"\n").

%%% @private
choc({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    erlang:length(lists:flatten(?Syn:tree_text(File)));
choc({function, Fun}) -> 
    [Def| _] = ?Query:exec(Fun,?Fun:definition()),
    Text = ?Syn:tree_text(Def),
    string:join(string:tokens(lists:flatten(Text),"\n"),"").

%%% @private
nof({module, Mod})->
    ?Query:exec(Mod, ?Mod:locals());
nof({function, _})->
    throw(?RefError(incompat, [number_of_function])).
    
%%% @private
num_of_macros({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    referl_esg:path(File, [{form, {type, '==', macro}}]);
num_of_macros({function,_})->
    throw(?RefError(incompat, [number_of_macros])).

%%% @private
num_of_rec({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    ?Query:exec(File, ?File:records());
num_of_rec({function,_})->
    throw(?RefError(incompat, [number_of_records])).
%%% @private

inc_file({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    ?Query:exec(File, ?File:included())
    -- [File];
inc_file({function,_})->
    throw(?RefError(incompat, [included_files])).

%%% @private
imp_mod({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    Forms = ?Query:exec(File, ?File:forms()),
    IncForms = [IncForm||IncForm <- Forms, 
                             ?Form:type(IncForm) == import],
    unic([],[],IncForms);
imp_mod({function, _})->
    throw(?RefError(incompat, [implemented_modules])).

%%% @private
unic(Acc, AttrList, [H|List])->
     [Attr] = referl_esg:path(H,[{attr,1}]),
     Value = ?Expr:value(Attr),
     case lists:member(Value, AttrList) of
          true  -> unic(Acc, AttrList, List);
	  false -> unic(Acc++[H], AttrList++[Value], List)
     end;
unic(Acc, _, []) ->
    Acc.

%%% @private
%% The result is the number of function path (calls between functions)
%%      in the module `Mod'. 
number_of_funpath({module, Mod})->
     referl_esg:path(Mod,
            [func,{fundef,back},funcl,
                    {scope,back},visib,{sup,back},funref]);
number_of_funpath({function,_})->
    throw(?RefError(incompat, [number_of_funpath])).

%% This functions result is the number of the incoming function calls
%%      into the module `Mod'.

%%% @private
fcalls_in({module, Mod})->
    Functs = ?Query:exec(Mod, ?Mod:locals()),
    FunApps = [referl_esg:path(Fun, [{{funref, back}, 
                           {kind, '==', application}}]) || Fun <- Functs],
    Clauses = [referl_esg:path(FunApp,[sup, {visib,back}]) 
                                    || FunApp <- lists:flatten(FunApps)],
    Forms = [?Query:exec(Clause, ?Clause:form())||Clause <- Clauses],
    [?Query:exec(Form, ?Form:module())||Form <- Forms]--[Mod];
fcalls_in({function,_})->
    throw(?RefError(incompat, [function_calls_in])).

%% This functions result is the number of the outgoing function calls
%%      from the module `Mod'.
%%% @private
fcalls_out({module, Mod})->
    Functs = ?Query:exec(Mod, ?Mod:locals()),
    RefFuns = [referl_esg:path(Fun, [{fundef, back}, funcl,
                   {scope, back}, visib, {sup,back}, funref])||Fun <- Functs],
    [?Query:exec(RFun, ?Fun:module())||RFun <- lists:flatten(RefFuns)]-- [Mod];
fcalls_out({function,_})->
    throw(?RefError(incompat, [function_calls_in])).

%%% @private
cohesion({module, Mod})->
    Functs = ?Query:exec(Mod, ?Mod:locals()),
    %Functs = referl_esg:path(Mod,[func]),
    RefFuns = [referl_esg:path(Fun, [{fundef, back}, funcl,
                   {scope, back}, visib, {sup,back}, funref])||Fun <- Functs],
    Mods = [?Query:exec(RFun, ?Fun:module())|| RFun <- lists:flatten(RefFuns)],
    [ModX || ModX <- Mods, [Mod] == ModX];
cohesion({function,_})->
    throw(?RefError(incompat, [cohesion])).

%%% @private
modsum({module, Mod})->
    FunList = ?Query:exec(Mod, ?Mod:locals()),
    NodeList =   [use_on_all(FunNode) ||FunNode <- FunList],
    lists:flatten(NodeList);
modsum({function,_})->
    throw(?RefError(incompat, [modsum])).

%%% @private
use_on_all(Fun)->
    [apply(Metric , [{function, Fun}]) || Metric 
                                          <- fun_sum()].
%%% @private
otp_used({module, _Mod})->
    [];
otp_used({function,_})->
    throw(?RefError(incompat, [otp_used])).

%%% @private
fun_inside({function, Fun})->
    [apply(Metric , [{function, Fun}]) || Metric 
                                          <- fun_sum()];
fun_inside({module, _})->
    throw(?RefError(incompat, [fun_inside])).

%% This function returns the maximum depth of embedded function
%%      applications in the module `Mod' or the function `Fun'.
%%% @private
max_application_depth({module, Mod}) -> 
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = [max_application_depth({function, Fun}) || Fun <- Functions],
    lists:max([0] ++ Depths);
max_application_depth({function, Fun}) ->
    TopExprs = ?Query:exec(Fun, ?Query:seq(
                                  [?Fun:definition(),
                                  ?Form:clauses(),
                                  ?Clause:exprs()])),
    max_application_depth({exprs, TopExprs});
max_application_depth({exprs, Exprs}) ->
    Depths     = [max_application_depth({expr, Expr, ?Expr:kind(Expr)}) ||
                     Expr <- Exprs],
    lists:max([0] ++ Depths);
max_application_depth({expr, Expr, application}) ->
    1 + max_application_depth({expr, Expr, other});
max_application_depth({expr, Expr, _}) ->
    SubExprs   = ?Query:exec(Expr, ?Expr:children()),
    max_application_depth({exprs, SubExprs}).

%% This function returns the maximum depth of the call tree
%%      starting from the module `Mod' or the function `Fun'.
%%% @private
max_depth_of_calling({module, Mod}) ->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = [max_depth_of_calling({function, Fun}) || Fun <- Functions],
    lists:max([0] ++ Depths);
max_depth_of_calling({function, Fun}) ->
    max_depth_of_calling({function, Fun}, {historyset, sets:from_list([Fun])}).

%%% @private
max_depth_of_calling({function, Fun}, {historyset, History}) ->
    CalledList  = ?Query:exec(Fun, ?Query:seq(
                                  [?Fun:definition(),
                                   ?Form:clauses(),
                                   ?Clause:exprs(),
                                   ?Expr:funapps()])),
    CalledSet   = sets:from_list(CalledList),
    NewCalled   = sets:subtract(CalledSet, History),
    NewCallList = sets:to_list(NewCalled),
    Depths      = [1 + max_depth_of_calling(
                         {function, Called},
                         {historyset, sets:add_element(Called, History)}) ||
                      Called <- NewCallList],
    lists:max([0] ++ Depths).

%%% @private
min_depth_of_calling({module, _Mod})->
    [];
min_depth_of_calling({function, _})->
    throw(?RefError(incompat, [min_depth_of_calling])).

%%% @private
max_depth_of_cases({module, Mod})->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = [max_case_depth(Fun)|| Fun <- Functions],
    lists:max(Depths);
max_depth_of_cases({function, Fun})->
    max_case_depth(Fun).
    
%% Returns the maximal depth of embedded case structures.
%%% @private
max_case_depth(Fun) ->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
                                  ?Fun:definition(), ?Form:clauses())),
    TopExprs   = [Expr ||   Cl   <- FunClauses,
                            Expr <- ?Query:exec(Cl, ?Clause:exprs())],
    lists:max(lists:flatten([0|[max_case_depth(Expr, 0) || Expr <- TopExprs]])).

%% Returns a deep list of maximal depths of embedding.
%%% @private
max_case_depth({Expr, fun_expr}, Depth) ->
    %[Depth];
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_case_depth(E, Depth ) || E <- Exprs, E =/= Expr]];
max_case_depth({Expr, case_expr}, Depth) ->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_case_depth(E, Depth + 1) || E <- Exprs, E =/= Expr]];
max_case_depth({Expr, try_expr}, Depth)->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_case_depth(E, Depth ) || E <- Exprs, E =/= Expr]];
max_case_depth({Expr, if_expr}, Depth)->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_case_depth(E, Depth ) || E <- Exprs, E =/= Expr]];
max_case_depth({Expr, _NotNestedExpr}, Depth) ->
    TopExprs = [E || E <- ?Query:exec(Expr, ?Expr:sub())],
    [Depth|[max_case_depth(E, Depth) || E <- TopExprs, E =/= Expr]];
max_case_depth(Expr, Depth) ->
    max_case_depth({Expr, ?Expr:kind(Expr)}, Depth).

%%% @private
funclauses({function, Fun})->
    ?Query:exec(Fun, ?Query:seq(
                             ?Fun:definition(), ?Form:clauses()));

%%% @private
funclauses({module, Mod}) -> 
    FunCs = ?Query:exec(Mod, ?Mod:locals()),
    Clauses = [?Query:exec(FunNode, ?Query:seq(
                             ?Fun:definition(), ?Form:clauses())) || 
                                                      FunNode <- FunCs],
    lists:flatten(Clauses).

%%% @private
b_recursion({module, Mod}) ->
    FunCs = ?Query:exec(Mod, ?Mod:locals()),
    lists:flatten([b_recursion({function, Func}) || Func <- FunCs]);
b_recursion({function, Fun}) ->
    Fundef = ?Query:exec(Fun, ?Fun:definition()),
    lists:flatten([?Query:exec(Fun, ?Fun:applications(Expr))
     	|| Expr <- ?Query:exec([Fundef], ?Query:seq(?Form:clauses(),
                                                     ?Clause:exprs()))]).

%%% @private    
mCb({function, _Fun})->
    [];
mCb({module, _Mod})->
    [].

%%% @private
calls_for({function, Fun})->
    referl_esg:path(Fun, [{{funref, back}, 
                           {kind, '==', application}}]);
calls_for({module, _})->
    throw(?RefError(incompat, [calls_for_function])).

%%% @private
calls_from({function, Fun})->
    referl_esg:path(Fun, [{fundef, back}, funcl,
                    {scope, back}, visib, {sup,back}, funref]);
calls_from({module, _})->
    throw(?RefError(incompat, [calls_from_function])).
%%% @private
funexp({module, Mod})->
   Functions = ?Query:exec(Mod, ?Mod:locals()),
   lists:flatten([funexp({function, Fun}) || Fun <- Functions]);
funexp({function, Fun})->
   type_expression(Fun, fun_expr).

%%% @private
messpass({module, Mod})->
   Functions = ?Query:exec(Mod, ?Mod:locals()),
   lists:flatten([messpass({function, Fun}) || Fun <- Functions]);
messpass({function, Fun})->
   type_expression(Fun, send_expr).

%%% @private
%%% TODO:need to optimize - 
%%% Too many collected expressions are in the result list before
%%% selecting the given type so the function is wery slow.
type_expression(Fun, ExprType) ->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
                                  ?Fun:definition(), ?Form:clauses())),
    TopExprs   = [Expr || Cl <- FunClauses, Expr 
                             <- ?Query:exec(Cl, ?Clause:exprs())],
    EmbExpr = 
    lists:flatten([nested(Expr, [Expr]) || Expr <- TopExprs]),
    lists:usort(
    [?Expr:kind(Expr) || Expr <- EmbExpr, ?Expr:kind(Expr)  == ExprType]).

%%% @private
nested({Expr, Type}, List) when Type == case_expr; 
                                Type == try_expr;
                                Type == if_expr;
                                Type == fun_expr ->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Expr|[nested(E, List) || E <- Exprs, E =/= Expr]];
nested({Expr, _NotNExpr}, List) ->
    TopExprs = [E || E <- ?Query:exec(Expr, ?Expr:sub())],
    [Expr|[nested(E, List) || E <- TopExprs, E =/= Expr]];
nested(Expr, List) ->
    nested({Expr, ?Expr:kind(Expr)}, List).

%%% @private
average_size({module, _Mod})->
    [];
average_size({function, _})->
    throw(?RefError(incompat, [average_size])).

%%% @private
fun_return_points({module, Mod})->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    lists:flatten([fun_return_points({function, Fun}) || Fun <- Functions]);
%%% @private
fun_return_points({function, Fun})->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
                                  ?Fun:definition(), ?Form:clauses())),
    LastTopExprs = [lists:last(
            ?Query:exec(Cl, ?Clause:exprs())) || Cl <- FunClauses],
    lists:flatten([return_points(LTP, ?Expr:kind(LTP)) || LTP <- LastTopExprs]).

%%% @private    
return_points(Expr, Kind) when  Kind == case_expr; 
                                Kind == try_expr; 
                                Kind == if_expr->
    Clauses = ?Query:exec(Expr, ?Expr:clauses()),
    HeadClauses = ?Query:exec(Expr, [headcl]),
    Exprs = [lists:last(?Query:exec(Cl, ?Clause:exprs())) 
                                            || Cl <- Clauses -- HeadClauses],
    [return_points(ExprL,?Expr:kind(ExprL)) || ExprL <- Exprs];
return_points(Expr, _Other)->
    Expr.
