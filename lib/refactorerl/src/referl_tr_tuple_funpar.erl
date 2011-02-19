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

%%% @doc This refactoring contact one or more parameters of a function into
%%%      a tuple. In effect the function arity may be change. All definition
%%%      clause, function calls in application expressions, arity qualifiers in
%%%      import/export lists and function references in implicit function
%%%      expressions will be modified to match with the new function signature.
%%%
%%% @author Kornél Horváth <kornel@inf.elte.hu>

-module(referl_tr_tuple_funpar).
-vsn("$Rev: 1980 $").
-include("refactorerl.hrl").

%%% ============================================================================
%%% Exports

%% Interface
-export([do/1]).

%% Callbacks
-export([init/1, steps/0, transform/1]).

%% For testing
-export([edges_idx/3, get_node_type/1, check_node/4, check_record/3]).


%%% ============================================================================
%%% Refactoring state

%% @ type refState().
%%  Contain the informations of refactoring.
-record(refst, {initType,           % Which init function called first
                fileNameStr,        % Actual file name (string)
                posFirst, posLast,  % First and last position of the marked part

                refCase,            % Refactoring case
                upNode,             % Node of refactoring case
                upPrev1, upPrev2,   % Previous nodes and links on path to upNode

                funNode, modNode,   % Semantical function and module node
                modName,            % Module name (atom)
                funName, funArity,  % Function name (atom) and arity
                newArity,           % New arity of function
                idx1, idx2,         % Indexes of parameters
                collideAutoImp,     % Is new function conflict with an auto
                                    % imported function
                funDefs,            % Clauses of function definition
                funApps,            % Applications of function
                funImpExps,         % Import/export list elements of function
                funImpls,           % Implicit function nodes of function

                modifFileNodes=[]   % Modified modules
               }).

% Print state of refactoring
%print_state(St=#refst{}) ->
%    io:format("State:\n~p\n",
%        [record_to_propertylist(St, record_info(fields, refst))]).

%%% ============================================================================
%%% Errors


%%% ============================================================================
%%% Interface


%% @spec do(Initialize) -> ok
%%   Initialize = {file_pos, FileNameStr::string(), PosFirst::integer(), PosLast::integer()} |
%%                {node_idx, FunNode::node(), IdxFirst::integer(), IdxLast::integer()}
%% @throws term()
%% @doc Contract function arguments into tuple.
%%
%% More ways are avaible to initialize the transformation. It is decided by the
%% first element of the tuple:
%% <ul>
%%   <li>`file_pos': initialize with text selection in a file
%%     <ul>
%%       <li>`FileNameStr': name of file which contain the selected text</li>
%%       <li>`PosFirst', `PosLast': the first and last position of marked part
%%           in the file. These are charcater indices.</li>
%%     </ul>
%%   </li>
%%   <li>`node_idx': initialize with a function node and parameter indices
%%     <ul>
%%       <li>`FunNode': semantical function node from ESG graph</li>
%%       <li>`IdxFirst', `IdxLast': indices of the first and last arguments that
%%            shuold contract into tuple.</li>
%%     </ul>
%%   </li>
%% </ul>
do({file_pos, FileNameStr, PosFirst, PosLast}) ->
    ?TRANSFORM:do(?MODULE, {file_pos, FileNameStr, PosFirst, PosLast});
do({node_idx, FunNode, IdxFirst, IdxLast}) ->
    ?TRANSFORM:do(?MODULE, {node_idx, FunNode, IdxFirst, IdxLast}).



%%% ============================================================================
%%% Callbacks

%% @private
%% @spec init(Initialize) -> refState()
%%       Initialize = {file_pos, FileNameStr::string(), PosFirst::integer(), PosLast::integer()} |
%%                    {node_idx, FunNode::node(), IdxFirst::integer(), IdxLast::integer()}
%% @throws term()
%% @doc Initialize the tuple function parameters refactoring.
%% Initialize with text selection in a file:
%% <ul>
%%   <li>`FileNameStr': name of file which contain the selected text</li>
%%   <li>`PosFirst', `PosLast': the first and last position of marked part in the
%%       file. These are charcater indices.</li>
%% </ul>
%% Initialize with a function node and parameter indices:
%% <ul>
%%   <li>`FunNode': semantical function node from ESG graph</li>
%%   <li>`IdxFirst', `IdxLast': indices of the first and last arguments that
%%        shuold contract into tuple.</li>
%% </ul>
init({file_pos, FileNameStr, PosFirst, PosLast}) ->
    {P1, P2} = if
        PosFirst=<PosLast -> {PosFirst, PosLast};
        true -> {PosLast, PosFirst}
    end,
    #refst{initType=file_pos, fileNameStr=FileNameStr, posFirst=P1, posLast=P2};
init({node_idx, FunNode, IdxFirst, IdxLast}) ->
    {Idx1, Idx2} = if
        IdxFirst=<IdxLast -> {IdxFirst, IdxLast};
        true -> {IdxLast, IdxFirst}
    end,
    case get_node_type(FunNode) of
        func ->
            #func{name=FunName, arity=FunArity} = ?GRAPH:data(FunNode),
            if
                (Idx1<1) or (Idx2<1) or (FunArity<Idx2) ->
                    throw_error("Incorrect paramter indices!", user, []);
                true ->
                    #refst{initType=node_idx, funNode=FunNode, funName=FunName,
                        funArity=FunArity, idx1=Idx1, idx2=Idx2}
            end;
        _ -> throw_error("Node is not function node!",user,[])
    end.


%% @private
%% @spec steps() -> [Step::((refState()) -> refState())]
%% @doc  Give back the step functions of the tuple function parameters refactoring.
steps() ->
    [fun find_sinNodes/1,      % Find sintactical nodes in the syntax tree
     fun find_funNode/1,       % Find semantical function node
     fun find_modNode/1,       % Find semantical module node
     fun check_case/1,         % Check cases of refactoring
     fun check_function/1,     % Check function arity and definition
     fun calc_indices/1,       % Calc indexes of parameters
     fun check_collision/1,    % Check collusions with other functions
     fun find_references/1,    % Collect defs, apps, imps/exps, impl funcs
     fun find_files/1          % Collect nodes of modified files
    ].


%% @private
%% @spec transform(State::refState()) -> {ModifiedFiles::[node()], ok}
%% @throws term()
%% @doc  Do the tuple function parameters refactoring transformation.
transform(St=#refst{funNode=FunNode, modName=ModName, funName=FunName,
        funArity=FunArity, newArity=NewArity, idx1=Idx1, idx2=Idx2,
        collideAutoImp=CollideAutoImp, funDefs=FunDefs, funApps=FunApps,
        funImpExps=FunImpExps, funImpls=FunImpls}) ->
    % Get function data
    FunData = ?GRAPH:data(FunNode),
    % --- ESG batch starts here ---
    % Transform function definitions
    lists:map(fun(D) -> transform_def(Idx1, Idx2, D) end, FunDefs),
    % Transform function applications
    lists:map(fun(A) ->
            transform_app(ModName, FunName, Idx1, Idx2, CollideAutoImp, A) end,
        FunApps),
    % If function arity is change: transform import/expot lists
    if
        FunArity==NewArity -> ok;
        true ->
            ?ESG:update(FunNode, FunData#func{arity=NewArity}),
            lists:map(fun(IE) -> transform_impexp(NewArity, IE) end,
                FunImpExps)
    end,
    % Transform implicit function expressions
    lists:map(fun(I) ->
            transform_impl(ModName, FunName, FunArity, Idx1, Idx2, I) end,
        FunImpls),
    % Close ESG batch
    ?ESG:close(),
    % --- ESG batch ends here ---
    % Return with modified files
    {St#refst.modifFileNodes, ok}.



%%% ============================================================================
%%% Description

%% TODO translate to English

% Függvényparaméterek összevonása tuple-ba
% ========================================
%
% Az elsõ és az utolsó pozíció alapján meg kell határozni a tokeneket. Meg kell
% keresni a tokenek lagalsó közös szintaktikus csúcsát (TopNode).
%
% Ha az nem funcl, application, arity_qualifier, akkor addig kell felmenni,
% amíg egy ilyet nem találunk (UpNode). Ott meg kell határozni, hogy mi volt az
% utolsó él, amin oda jutottunk. (Lehet azonos és különbõzõ élek is)
%
% Ha funcl-be jutottunk, akkor az utolsó él:
%  clex: token csak ( vagy ) lehet
%  name: függvény neve
%  pattern: formális paraméter
% Meg kell határozni a kijeleölt paramétereket. Ha a kijelölés nem egyértelmû,
% akkor kérdés kell a felhasználó felé.
% A func élen vissza és funref élen elõre eljutunk a függvény szemantikus
% csúcsához.
%
% Ha application-ba jutottunk, akkor az utolsó él:
%  elex: token csak ( vagy ) lehet
%  sub/1: a függvény neve
%  sub/?: aktuális paraméter
% Meg kell határozni a kijeleölt paramétereket. Ha a kijelölés nem egyértelmû,
% akkor kérdés kell a felhasználó felé.
% A funref élen elõre eljutunk a függvény szemantikus csúcsához.
%
% Ha arity_qualifier-be jutottunk, akkor nincs szükség az utoló élekre.
% Mindenféle képpen kérdés kell a felhasználó felé.
% A funref élen elõre eljutunk a függvény szemantikus csúcsához.
%
% Ha a függvény 0 aritású, akkor nem lehet a paramtereket összevonni.
%
% Megvan a függvény szemantikus csúcsa, az összevonadó paraméterek száma.
% A fundef élen vissza és a funcl éleken elõre össze kell gyüjteni a definíció
% klózait. Ha nincs definíció, akkor nem lehet az összevonást végrehajtani.
%
% Össze kell gyûjteni a modulokat, amelyekben importálva van, és ellenõrizni
% kell az új aritással, hogy nincs-e ütközés:
%  - lokálisan nem ütközhet, az importált függvényekkel sem
%  - ha exportolva van, akkor azokban a modulokban sem ütközhet a lokális és
%    importált fv-ekkel, ahol importálva van
%  - ha valahol importálva van, akkor nem ütközhet automatikusan importált
%    függvénnyel sem
%
% A fuunref éleneken vissza össze kell gyülyteni a hivatkozásokat. 3 csoportja
% lesz:
%  - application: függvény hívás
%  - arity_qualifier: import/export listákban
%  - implicit fun: implicit függvény
%
% El lehet végezni a transzformációt.
%  - definíció klózaiban össze kell vonni a formális paramétereket tuple-ba
%  - függvényhívásnál össze kell vonni a formális paramétereket tuple-ba, és ha
%    az új függvény egy automatikusan importált függvénnyel ütközik, és nem
%    modul minõsítéssel hívódik, akkor minõsíteni kell
%  - implicit függvényeknél függvénykifejezést kell köré építeni, aminek az
%    aritása azonos az eredeti aritással, és egyetlen függvényhívás tartalmaz
%    az új függvényre tuple-be összevont aktuális paraméterekkel
%  - import/export listák arity qualifier-jeiben az aritás értékét frissíteni
%    kell (feltéva, hogy megváltozott)
%



%%% ============================================================================
%%% Implementation


% Find simple sintactical nodes of refactoring
find_simple_sinNodes(#refst{fileNameStr=FileNameStr,
                        posFirst=PosFirst, posLast=PosLast}) ->
    MarkError = error_info("Marked part must be in an expression!", user,
        [{"File",FileNameStr},{"First position",PosFirst},
         {"Last position",PosLast}]),
    % Find file node
    FileNode = case ?SYNTAX:file(FileNameStr) of
        {file, FN} -> FN;
        _ -> throw_error("File isn't added to the database!", user,
                [{"File",FileNameStr}])
    end,
    % Find tokens
    TokenFirst = case ?LEX:token_by_pos(FileNode, PosFirst) of
        {ok, Token1} -> Token1;
        _ -> throw_error(MarkError)
    end,
    TokenLast = case ?LEX:token_by_pos(FileNode, PosLast) of
        {ok, Token2} -> Token2;
        _ -> throw_error(MarkError)
    end,
    % Find top node
    {TopNode, Path1, Path2} = case top_node(TokenFirst, TokenLast) of
        no -> throw_error(MarkError);
        {TN,P1,P2} -> {TN,P1,P2}
    end,
    % Return with simple sintactical nodes
    {TopNode, Path1, Path2}.


% Find sintactical nodes of refactoring
find_sinNodes(St=#refst{initType=file_pos}) ->
    % Find simple sintactical nodes
    {TopNode, Path1, Path2} = find_simple_sinNodes(St),
    % Find way trom TopNode up to rafactoring case node
    UpTop = upnode(TopNode, [],
            [{clause,          clause, record_info(fields, clause),
                [{kind,'==',fundef}]},
             {funexpr,         clause, record_info(fields, clause),
                [{kind,'==',funexpr}]}, % Bad case: Not supported !!!
             {application,     expr,   record_info(fields, expr),
                [{type,'==',expr},{kind,'==',application}]},
             {implicit_fun,    expr,   record_info(fields, expr),
                [{type,'==',expr},{kind,'==',implicit_fun}]} ],
            [{arity_qualifier, fun is_arity_qualifier/1}] ),
    % Determine refactoring case
    {UpNode, PathTop, RefCase} = case UpTop of
        {_,_, filter, funexpr, _} ->
            throw_error("Function expression is not supported!",user,[]);
        {N,_, filter,   FilterId,   P} -> {N,P,FilterId};
        {N,_, function, FunctionId, P} -> {N,P,FunctionId};
        _ -> throw_error("Marked part isn't a legal selection!",user,[])
    end,
    % Previouos nodes on pathes from tokens to UpNode
    {PrevL1,PrevI1} = hd(PathTop++Path1),
    [PrevN1] = ?GRAPH:path(UpNode, [{PrevL1,PrevI1}]),
    {PrevL2,PrevI2} = hd(PathTop++Path2),
    [PrevN2] = ?GRAPH:path(UpNode, [{PrevL2,PrevI2}]),
    % Update state
    St#refst{refCase=RefCase, upNode=UpNode,
        upPrev1={PrevL1,PrevN1,PrevI1}, upPrev2={PrevL2,PrevN2,PrevI2}};
% No text based refactoring case
find_sinNodes(St=#refst{}) -> St.


% Find semantical function node
find_funNode(St=#refst{refCase=RefCase, upNode=UpNode,
        funNode=undefined}) ->
    % Set path to semantical function node
    PathToFunNode = case RefCase of
        clause -> [{funcl,back},fundef];
        application -> [funref];
        implicit_fun -> [funref];
        arity_qualifier -> [funref];
        X -> throw_error("Unknown case of refactoring! Possibbly not " ++
            "implemented case.", internal, [{"Case",X}])
    end,
    % Find semantical function node
    FunNode = case ?GRAPH:path(UpNode, PathToFunNode) of
        [] -> throw_error("Semantical function node is not found!",internal,
                [{"Upnode",UpNode},{"Case",RefCase}]);
        [FN] -> FN
    end,
    % Get function name and arity
    #func{name=FunName, arity=FunArity} = ?GRAPH:data(FunNode),
    % Update status
    St#refst{funNode=FunNode, funName=FunName, funArity=FunArity};
% Function node is already decided
find_funNode(St=#refst{}) -> St.


% Find semantical module node
find_modNode(St=#refst{funNode=FunNode, funName=FunName, funArity=FunArity}) ->
    % Find semantical module node
    ModuleNode = case ?GRAPH:path(FunNode, [{func,back}]) of
        [] -> throw_error("Semantical module node is not found!",internal,
                [{"Function node",FunNode},
                 {"Function",lists:concat([FunName,'/',FunArity])}]);
        [MN] -> MN
    end,
    % Get module name
    #module{name=ModName} = ?GRAPH:data(ModuleNode),
    % Update status
    St#refst{modNode=ModuleNode, modName=ModName}.


% Calculate idexes of arguments of function that ought put into tuple
calc_indices(St=#refst{refCase=RefCase,
        idx1=undefined, idx2=undefined}) ->
    UnknownCaseError = error_info("Unknown case of refactoring! " ++
        "Possibbly not implemented case.", internal, []),
    % Check upnode childs
    IdxAnswer = case RefCase of
        clause          -> calc_indices_cl_app(St);
        application     -> calc_indices_cl_app(St);
        implicit_fun    -> question;
        arity_qualifier -> question;
        X -> erlang:error(error_info(UnknownCaseError,[{"Case",X}]))
    end,
    % Updating state
    case IdxAnswer of
        question    ->
            %ask_user(St);
            % The tool currently not support question for user ...
            throw_error("Marked text is not a legal selection of function " ++
                        "arguments!", user, []);
        {Idx1,Idx2} -> St#refst{idx1=Idx1, idx2=Idx2}
    end;
% Process user answer
calc_indices({answer, Answer, St=#refst{funArity=FunArity}}) ->
    Ans = string:strip(Answer),
    case lists:member(Ans,
            ["quit","exit","abort","cancel","bye","no","q","n",""]) of
        true ->
            ?TRANSFORM:cancel();
        _ ->
            try
                L = string:tokens(Answer, " ,;()[]{}"),
                case lists:map(fun erlang:list_to_integer/1, L) of
                    [Idx1,Cnt] ->
                        if
                            (0<Idx1) and (0<Cnt) and (Idx1+Cnt-1=<FunArity) ->
                                St#refst{idx1=Idx1, idx2=Idx1+Cnt-1};
                            true ->
                                ask_user(St)
                        end;
                    _ -> ask_user(St)
                end
            catch
                _ -> ask_user(St)
            end
    end;
% Indices are already decided
calc_indices(St=#refst{}) -> St.


% Calculate idices of arguments in the function clause or application case
calc_indices_cl_app(#refst{upNode=UpNode, upPrev1={_,PrevN1,_},
                           upPrev2={_,PrevN2,_}, funArity=FunArity}) ->
    % Get childs of UpNode and calc indexes of childs
    Childs = ?SYNTAX:children(UpNode),
    Idx1 = list_find(fun({_,N}) -> N==PrevN1 end, Childs),
    Idx2 = list_find(fun({_,N}) -> N==PrevN2 end, Childs),
    IdxClP = if
        0==FunArity -> 3;
        true        -> (2+2*FunArity)
    end,
    % Calculate indexes of arguments that ought put into tuple
    if
        % A bound of marked part is at comma
        ((2<Idx1) and (Idx1<IdxClP) and ((Idx1 rem 2)==0)) or
        ((2<Idx2) and (Idx2<IdxClP) and ((Idx2 rem 2)==0)) ->
            throw_error("Marked part bounds mustn't be at comma!",user,[]);
        % Just function name or only close parenthesis is marked
        (Idx2=<2) or
        ((Idx1==Idx2) and (Idx1==IdxClP)) -> question;
        % Function names and arguments are marked
        Idx1=<2 -> {1,(Idx2-1) div 2};
        % Just arguments are marked
        true    -> {(Idx1-1) div 2, (Idx2-1) div 2}
    end.


% Collect definitions, applications, imports, exports of the function
find_references(St=#refst{funNode=FunNode}) ->
    % Collect function definition clauses and patterns
    FunDefs    = lists:map(
        fun(DN) -> {DN} end,
        ?GRAPH:path(FunNode, [{fundef,back},funcl])),
    % Collect function application expressions and check application name is a
    % module qualifier
    FunApps = lists:map(
        fun(AN) -> {AN, is_module_qualifier(hd(?GRAPH:path(AN, [{sub,1}])))} end,
        ?GRAPH:path(FunNode,
            [{{funref,back},{{type,'==',expr},'and',{kind,'==',application}}}])),
    % Collect arity qualifiers and arity with his token
    FunImpExps = lists:map(
        fun(IEN) -> {IEN, hd(?GRAPH:path(IEN, [{sub,2}])),
            hd(?GRAPH:path(IEN, [{sub,2},{elex,1}]))} end,
        ?GRAPH:path(FunNode,
            [{{funref,back},{{type,'==',expr},'and',
                {{kind,'==',infix_expr},'and',{value,'==','/'}}} }])),
    % Collect implicit function expressons and parent
    FunImpls = lists:map(
        fun(IN) -> {IN, hd(?ESG:parent(IN))} end,
        ?GRAPH:path(FunNode,
            [{{funref,back},{{type,'==',expr},'and',{kind,'==',implicit_fun}}}])),
    % Updating state
    St#refst{funDefs=FunDefs, funApps=FunApps, funImpExps=FunImpExps,
             funImpls=FunImpls}.


% Collect files where the transformation will modify
find_files(St=#refst{funArity=FunArity, newArity=NewArity, funDefs=FunDefs,
        funApps=FunApps, funImpExps=FunImpExps, funImpls=FunImpls}) ->
    % Collect all nodes which will be modified
    IENodes = if
        FunArity==NewArity -> [];
        true -> lists:map(fun({IEN,_,_}) -> IEN end, FunImpExps)
    end,
    Nodes = lists:map(fun({DN}) -> DN end, FunDefs) ++
            lists:map(fun({AN,_}) -> AN end, FunApps) ++
            lists:map(fun({IN,_}) -> IN end, FunImpls) ++ IENodes,
    % Find file nodes from collected nodes
    FileNodes = lists:usort(lists:flatten(lists:map(fun(N) ->
            {FN,_,_,_,_} = upnode(N,[],[{file,file,record_info(fields,file),[]}],[]),
            FN end, Nodes))),
    % Update state
    St#refst{modifFileNodes=FileNodes}.



%%% ----------------------------------------------------------------------------
%%% Checks

% Additional checks for different cases
check_case(St=#refst{refCase=RefCase, initType=file_pos}) ->
    % Additional checks
    case RefCase of
        clause          -> check_clause(St);
        application     -> ok;
        implicit_fun    -> ok;
        arity_qualifier -> ok;
        X -> throw_error("Unknown case of refactoring! Possibbly not " ++
            "implemented case.", internal, [{"Case",X}])
    end,
    % Return state
    St;
% No text based refactoring case
check_case(St=#refst{}) -> St.


% Check marked part is in the signature of function clause
check_clause(#refst{upNode=UpNode, upPrev2={_,PrevN2,_}, funArity=FunArity}) ->
    % Get childs of UpNode and calc indexes of childs
    Childs = ?SYNTAX:children(UpNode),
    Idx2 = list_find(fun({_,N}) -> N==PrevN2 end, Childs),
    IdxClP = if
        0==FunArity -> 3;
        true        -> (2+2*FunArity)
    end,
    % Marked part isn't in the "FunctionName(Arg1, ... ArgN)" region
    if
        IdxClP<Idx2 ->
            throw_error("Marked part isn't a legal selection!",user,[]);
        true -> ok
    end.


% Check function arity and definition
check_function(St=#refst{modName=ModName, funNode=FunNode, funName=FunName,
        funArity=FunArity}) ->
    % Check function arity
    if
        FunArity==0 -> throw_error("Function arity is zero!",user,[]);
        true -> ok
    end,
    % Check function has definition (it exclude BIFs also)
    case has_function_definition(FunNode) of
        false ->
            throw_error("Definition of function isn't added to the database!",
                user, [{"Function",
                    lists:concat([ModName,':',FunName,'/',FunArity])}]);
        _ -> ok
    end,
    % Return state
    St.


% Check collusion with another functions
check_collision(St=#refst{funName=FunName, funArity=FunArity,
        idx1=Idx1, idx2=Idx2}) ->
    % Calc new arity
    NewArity = FunArity - Idx2 + Idx1,
    CollideAutoImp = is_auto_imported(FunName, NewArity),
    State = St#refst{newArity=NewArity, collideAutoImp=CollideAutoImp},
    % Collusion detection
    if
        FunArity == NewArity -> ok;
        true -> check_collision_(State)
    end,
    % Updatig state
    State.

check_collision_(#refst{modNode=ModNode, funNode=FunNode,
        funName=FunName, newArity=NewArity,
        collideAutoImp=CollideAutoImp}) ->
    % Collect modules where function is visible without module qualifier
    % and collect functions which are maybe conflict with it
    ImpModNodes = ?GRAPH:path(FunNode, [{funimp,back}]),
    FunNodes = lists:usort(lists:flatten([
                    [?GRAPH:path(MN, [func]), ?GRAPH:path(MN, [funimp])] ||
                    MN<-[ModNode|ImpModNodes] ])),
    % Collision detection
    case lists:dropwhile(
            fun(FN) ->
                not check_node(FN, func, record_info(fields,func),
                    [{name,'==',FunName},{arity,'==',NewArity}])
                end,
            FunNodes) of
        [] -> ok;
        [OtherFunNode|_]  ->
            [OtherModNode] = ?GRAPH:path(OtherFunNode, [{func,back}]),
            #module{name=OtherModName} = ?GRAPH:data(OtherModNode),
            throw_error("Function with new arity is collide with an "++
                "another function!",user,[
                    {"Another function",
                        lists:concat([OtherModName,':',FunName,'/',NewArity])}])
    end,
    % Collusion detection with auto imported functions
    if
        CollideAutoImp andalso ([]=/=ImpModNodes) ->
            throw_error("Function with new arity is collide with an "++
                "auto imported function!",user,[
                    {"Auto imported function",
                        lists:concat(['erlang:',FunName,'/',NewArity])}]);
        true -> ok
    end.



%%% ----------------------------------------------------------------------------
%%% Interaction

% Ask user for missing information
ask_user(St=#refst{modName=ModName, funName=FunName, funArity=FunArity}) ->
    Str=?MISC:format("Function ~p:~p/~b is selected. "++
        "Give two positive integers as the index of first argument " ++
        "(must be in [1..~b]) and the number of parameters " ++
        "(must be in [1..~b-first]) that want to put into tuple. " ++
        "Or type exit to abort. (Example: 1 2)",
        [ModName, FunName, FunArity, FunArity, (FunArity+1)]),
    {question, Str, St}.



%%% ----------------------------------------------------------------------------
%%% Transformation

% Make transformation on function definition clauses
transform_def(Idx1, Idx2, {FunDefNode}) ->
    % Replace patterns with tuple
    % Lst contains the all child of FunDefNode function clause node beetwen
    % pattern/Idx1 and pattern/Idx2 childs:
    % [pattern/Idx1, clex/n, pattern/Idx1+1, clex/n+1, ... parretn/Idx2]
    ?SYNTAX:replace(FunDefNode, {pattern, Idx1, Idx2+1},
                    fun(Lst) ->
                        [{pattern, ?SYNTAX:create(#expr{type=pattern,kind=tuple},
                                    [{case T of
                                        pattern -> sub;
                                        clex -> elex
                                      end, N} || {T,N} <- Lst])}]
                    end).


% Make transformation on function application expressions
transform_app(ModName, FunName, Idx1, Idx2, CollideAutoImp,
        {FunAppNode, ModuleQualifier}) ->
    % If the new function is collide with an auto imported function and the
    % first sub node of apllication (name node, sub/1) is not an module
    % qualifier than replace that with an module qualifier
    if
        CollideAutoImp andalso (not ModuleQualifier) ->
            MqExNode = create_module_qualifier(ModName, FunName),
            ?SYNTAX:replace(FunAppNode, {sub,1,2}, [{sub,MqExNode}]);
        true -> ok
    end,
    % Replace patterns with tuple
    % Lst contains the all child of FunAppNode function application node beetwen
    % sub/Idx1 and sub/Idx2 childs:
    % [sub/Idx1, elex/n, sub/Idx1+1, elex/n+1, ... sub/Idx2]
    ?SYNTAX:replace(FunAppNode, {sub, Idx1+1, Idx2+2},
                    fun(Lst) ->
                        [{sub, ?SYNTAX:create(#expr{type=expr, kind=tuple},
                                [{T,N} || {T,N} <- Lst])}]
                    end).


% Make transformation on arity qualifiers in import/export lists
transform_impexp(NewArity, {_,ArityNode,ArityToken}) ->
    % Update arity node and his token
    ?ESG:update(ArityNode, #expr{type=expr,kind=integer,value=NewArity}),
    ?ESG:update(ArityToken, #lex{type=token,
        data=#token{type=integer,value=NewArity,text=?MISC:to_list(NewArity)}}).


% Make transformation on implicit function expressions
transform_impl(ModName, FunName, FunArity, Idx1, Idx2,
        {FunImplNode,{ParentLink,ParentNode}}) ->
    % Create function expression which contains application expression
    FunExprNode = create_fun_expr(ModName, FunName, FunArity, Idx1, Idx2),
    % Replace implicit function expression with function expression
    ?SYNTAX:replace(ParentNode, {node,FunImplNode}, [{ParentLink,FunExprNode}]).



%%% ----------------------------------------------------------------------------
%%% Subtree generation functions

% Create module qualifier
create_module_qualifier(ModName, FunName) ->
    MExNode = ?SYNTAX:create(#expr{type=expr, kind=atom, value=ModName},
                            [?MISC:to_list(ModName)]),
    FExNode = ?SYNTAX:create(#expr{type=expr, kind=atom, value=FunName},
                            [?MISC:to_list(FunName)]),
    ?SYNTAX:create(#expr{type=expr, kind=infix_expr, value=':'},
                            [{sub, MExNode}, {sub, FExNode}] ).


% Create function expression
create_fun_expr(ModName, FunName, FunArity, Idx1, Idx2) ->
    % --- Application ---
    MqNode = create_module_qualifier(ModName, FunName),
    % Create parameters and variables
    ParNameStrs = [ lists:concat(["P",I]) || I<-lists:seq(1,FunArity) ],
    VarNodes = [
            ?SYNTAX:create(#expr{type=expr, kind=variable, value=P}, [P]) ||
            P<-ParNameStrs ],
    % Split variable list
    {BeforeVarNodes, TAVarNodes   } = lists:split(Idx1-1, VarNodes),
    {TupleVarNodes,  AfterVarNodes} = lists:split(Idx2-Idx1+1, TAVarNodes),
    % Create variable tuple // {expr,pattern,tuple,undefined}
    TupleNode = ?SYNTAX:create(#expr{type=pattern, kind=tuple},
                    [{sub, TupleVarNodes}] ),
    % Create application
    AppNode = ?SYNTAX:create(#expr{type=expr, kind=application},
                [{sub, [MqNode|BeforeVarNodes] ++ [TupleNode|AfterVarNodes]}] ),
    % --- Function expression ---
    % Create pattern nodes
    PatternNodes = [
        ?SYNTAX:create(#expr{type=pattern,kind=variable,value=P}, [P]) ||
        P<-ParNameStrs ],
    % Create function expression clause
    ClauseNode = ?SYNTAX:create(#clause{type=scope, kind=funexpr},
                [{pattern,PatternNodes},{body,AppNode}]),
    % Create function expression
    ?SYNTAX:create(#expr{type=expr, kind=fun_expr},
                [{exprcl,ClauseNode}]).



%%% ============================================================================
%%% Private


%%% ----------------------------------------------------------------------------
%%% Error types and functions


%% @ type errorInfo() = {errorInfo, Msg::string(), Type::errorType(), [errorItem()]}.
%%  Desribe the error. Contain a general message, the type of error and some
%%  additional propeties.
%% @ type errorType() = user | internal.
%%  <ul>
%%   <li>user: the fault is origins from user commands</li>
%%   <li>internal:  probably implementation error</li>
%%  </ul>
%% @ type errorItem() = {errorItem, KeyStr::string(), Value::term()}.
-record(errorInfo, {msg, type=user, items=[]}).
-record(errorItem, {key,value}).


%% @private
%% @spec error_info(Msg::string(), Type::errorType(),
%%                  [{KeyStr::string(), Value::term()}]) ->
%%           errorInfo()
%% @doc  Create an {@link errorInfo()} record.
%% @see  errorInfo()
error_info(Msg, Type, Items) ->
    error_info_(lists:reverse(Items), #errorInfo{msg=Msg, type=Type}).

error_info_([], ErrorInfo) -> ErrorInfo;
error_info_([{Key,Value}|Is], ErrorInfo=#errorInfo{items=Items}) ->
    error_info_(Is,
        ErrorInfo#errorInfo{items=[#errorItem{key=Key,value=Value}|Items]}).


%% @private
%% @spec error_info(ErrorInfo::errorInfo(), [{KeyStr::string(), Value::term()}]) ->
%%           errorInfo()
%% @doc  Extend ErrorInfo with new items.
%% @see  errorInfo()
error_info(ErrorInfo, Items) ->
    reverse_error_info(error_info_(Items, reverse_error_info(ErrorInfo))).


%% @private
%% @spec reverse_error_info(ErrorInfo::errorInfo()) -> errorInfo()
%% @doc  Revers the items of ErrorInfo record.
%% @see  errorInfo()
reverse_error_info(ErrorInfo=#errorInfo{items=Items}) ->
    ErrorInfo#errorInfo{items=lists:reverse(Items)}.


%% @private
%% @spec throw_error(ErrorInfo::errorInfo()) -> any()
%% @throws errorInfo()
%% @doc  Throw `ErrorInfo'. If type of `ErrorInfo' is user than
%%       use erlang:throw/1 function else erlang:error/1.
throw_error(ErrorInfo) ->
    case ErrorInfo#errorInfo.type of
        user -> throw(error_info_to_string(ErrorInfo));
        _    -> erlang:error(ErrorInfo)
    end.

%% @private
%% @spec throw_error(Msg::string(), Type::errorType(),
%%           [{KeyStr::string(), Value::term()}]) -> any()
%% @throws errorInfo()
%% @doc  Create an {@link errorInfo()} record and throw that.
%% @see  throw_error/1
%% @see  error_info/3
throw_error(Msg, Type, Items) ->
    throw_error(error_info(Msg, Type, Items)).


%% @ private
%% @ spec throw_error(ErrorInfo::errorInfo(), [{KeyStr::string(), Value::term()}]) -> any
%% @ throws errorInfo()
%% @ doc  Extend `ErrorInfo' with new items and throw that.
%% @ see  throw_error/1
%% @ see  error_info/2
%% @ see  errorInfo()
%throw_error(ErrorInfo, Items) ->
%    throw_error(error_info(ErrorInfo, Items)).


%% @private
%% @spec error_info_to_string(ErrorInfo::errorInfo()) -> string()
%% @doc  Convert an {@link errorInfo()} record into string.
error_info_to_string(#errorInfo{msg=Msg, items=Items}) ->
    ParameterStr = lists:flatten([
        ?MISC:format("~s: ~s; ",[?MISC:to_list(Key),?MISC:to_list(Value)]) ||
        #errorItem{key=Key, value=Value}<-Items ]),
    case Items of
        [] -> Msg;
        _  -> Msg ++ " " ++ ParameterStr
    end.



%%% ----------------------------------------------------------------------------
%%% General functions

%% @ type record() = tuple()
%% @ type recordField() = atom()


%% @private
%% @spec list_find(Pred::function(), List::[term()]) -> integer() | not_found
%% @doc  Find the first element int the `List' that is accepted by `Pred' function
%%       (`Pred(ListElement) == true'). Return one based index of finded element.
list_find(Fun, List) -> list_find_(Fun, List, 1).

list_find_(_, [], _) -> not_found;
list_find_(Fun, [X|Xs], Idx) ->
    case Fun(X) of
        true -> Idx;
        _    -> list_find_(Fun, Xs, Idx+1)
    end.


%% @private
%% @spec list_compare(List1::[term()], List2::[trem()]) ->
%%            {Common::[term()], Tail1::[term()], Tail2::[term()]}
%% @doc  Compare lists and return the same prefix and the different tails.
list_compare(List1, List2) ->
    list_compare_(List1, List2, []).

list_compare_([], Tail2, Common) -> {lists:reverse(Common), [], Tail2};
list_compare_(Tail1, [], Common) -> {lists:reverse(Common), Tail1, []};
list_compare_(Tail1=[X|Xs], Tail2=[Y|Ys], Common) ->
    if
        X==Y -> list_compare_(Xs, Ys, [X|Common]);
        true -> {lists:reverse(Common), Tail1, Tail2}
    end.


%% @private
%% @spec get_rec_value(Record::record(), Field::recordField(), [recordField()]) ->
%%           record()
%% @throws {badmatch, recordField()}
%% @doc  Return the value of the `Field' field of the `Record'. You must give
%%       the fields of the record type in a list.
%%
%% Example:
%% ```
%% get_rec_value(#people{name="Brian"}, name, record_info(fields, people))
%% '''
get_rec_value(Record, Key, RecordInfo) ->
    case list_find(fun(Field)->Field==Key end, RecordInfo) of
        not_found -> erlang:error({badmatch,Key});
        Num -> element(Num+1, Record)
    end.


%% @ private
%% @ spec set_rec_value(Record::record(), Field::recordField(), Value::term(),
%%           [recordField()]) -> record()
%% @ throws {badmatch, recordField()}
%% @ doc  Set the `Field' field of the `Record' to `Value'. You must give the
%%       fields of the record type in a list.
%%
%% Example:
%% ```
%% set_rec_value(#people{name="Brian", age=32}, age, 33, record_info(fields, people))
%% '''
%set_rec_value(Record, Key, Value, RecordInfo) ->
%    case list_find(fun(Field)->Field==Key end, RecordInfo) of
%        not_found -> erlang:error({badmatch,Key});
%        Num -> setelement(Num+1, Record, Value)
%    end.


%% @ type recordFilter() = {Key::recordField(), Op, Value::term()}
%%       Op = '==' | '/='

%% @private
%% @spec check_record(Record::record(), Fields::[recordField()],
%%           Filters::[recordFilter()]) -> bool()
%% @throws {badmatch, recordField()}
%% @doc  Check fields of `Record' by the given filter list.
%%       Get the field of the `Record' which specified with `Key' and compare
%%       that with `Value' using the `Op' as operator. If `Filters' is empty
%%       `Record' will be accepted.
%%
%% Example:
%% ```
%% check_record(#people{name="Brian", age=33}, record_info(fields, people), [{age,'==',33}])
%% '''
%% @see  get_rec_value/3
check_record(_Record, _RecordInfo, []) -> true;
check_record(Record, RecordInfo, [{Key, Op, Value}|Filters]) ->
    RecordValue = get_rec_value(Record, Key, RecordInfo),
    Match = case Op of
        '=='  -> RecordValue ==  Value;
        '/=' -> RecordValue /= Value;
        _     -> erlang:error({badmatch,Op})
    end,
    case Match of
        true -> check_record(Record, RecordInfo, Filters);
        _    -> false
    end.


%% @ private
%% @ spec record_to_propertylist(Record::record(), RecordFields::[recordField()]) ->
%%           [{recordField(), term()}]
%% @ doc  Put the record field name and value pairs into a tuple list.
%record_to_propertylist(Record, RecFields) ->
%    lists:zip(RecFields,tl(tuple_to_list(Record))).

%% @ private
%% @ spec record_to_propertylist(Record::record(), Fields::[recordField()],
%%               RecordFields::[recordField()]) ->
%%           [{recordField(), term()}]
%% @ doc  Put the record field name and value pairs into a tuple list.
%%       `Fields' may be partial and permuted list.
%record_to_propertylist(Record, Fields, RecFields) ->
%    lists:foldl(fun(F, List) ->
%           [{F,get_rec_value(Record, F, RecFields)}|List] end,
%       [], Fields,).



%%% ----------------------------------------------------------------------------
%%% General graph query functions

%% @private
%% @spec get_node_type(node()) -> atom()
%% @doc  Return the type of the given node.
get_node_type(Node) -> element(2,Node).


%% @private
%% @spec get_node_data(node()) -> term()
%% @doc  Return the data of the original node of given node.
get_node_data(Node) -> 
	Data = ?GRAPH:data(Node),
	case {get_node_type(Node), Data} of
		{lex, #lex{data=virtual}} ->
			[OrignalToken] = ?GRAPH:path(Node, [{orig,1}]),
			?GRAPH:data(OrignalToken);
		_ -> Data
	end.


%% @private
%% @spec check_node(Node::node(), Type::atom, NodeDataFields::[recordField()],
%%           NoteDataFilters::[recordFilter()]) -> bool()
%% @throws {badmatch, recordField()}
%% @doc  Check `Node' type and data. If `Type' is `lex' than it will
%%       check the `data' field of node's `lex' record typed data. If the
%%       lexical node is virtual it will check the original lexical token.
%%       If `NodeDataFilters' is empty `Node' will be accepted.
%%
%% Example:
%% ```
%% check_node(#expr{kind=tuple}, expr, record_info(fields, expr), [{kind,'==',tuple}])
%% '''
%% @see  check_record/3
%% @see  get_rec_value/3
check_node(Node, Type, Fields, Filters) ->
    get_node_type(Node)==Type andalso
    check_record(get_node_data(Node), Fields, Filters).


%% REM DIAL parameter Dir is not needed if only called with `back'
%% @private
%% @spec edges_idx(FromNode::node(), Direction, ToNodes::[{Tag,node()}]) ->
%%           [{Tag,node(),integer()}]
%%       Direction = forward | back
%%       Tag = atom()
%% @doc  Expand links with indexes. Get indices of links from `FromNode' to
%%       `ToNodes' and and give back the extended path element.
edges_idx(FromNode, Dir, ToNodes) ->
    edges_idx_(FromNode, Dir, lists:reverse(ToNodes), []).

edges_idx_(_,_, [], Edges) -> Edges;
edges_idx_(FromNode, Dir, [{ToLink,ToNode}|ToNodes], Edges) ->
    Edge = case Dir of
        forward -> {ToLink,ToNode,?GRAPH:index(FromNode,ToLink,ToNode)};
        _       -> {ToLink,ToNode,?GRAPH:index(ToNode,ToLink,FromNode)}
    end,
    edges_idx_(FromNode, Dir, ToNodes, [Edge|Edges]).



%%% ============================================================================
%%% Cleanup

%%% ----------------------------------------------------------------------------
%%% Syntactic query functions

%% TODO move these functions to ?SYNTAX or use functions from there

%% @private
%% @spec upnode(Node::node(), Nodes::[node()], Filters::[NodeFilter],
%%                Functions::[NodeFunction]) ->
%%            {UpNode::node(), Prev, MatchType, MatchObject, PathFromUpNodeToNode} | no
%%       NodeFilter = {Id, NodeType::recordType(), NodeDataFields::[recordField()],
%%                     NodeDataFilters::[recordFilter()]}
%%       Id = atom()
%%       NodeFunction = {Id, Pred::((Node::node()) -> bool()) }
%%       Prev = {TagLinkTag::atom(), PrevNode::node(), PrevLinkIdx::integer()} | no
%%       MatchType   = node | filter | function
%%       MatchObject = node() | Id
%%       PathFromUpNodeToNode = [{LinkTag::atom(), LinkIdx::integer()}]
%% @throws {badmatch, RecordField} | term()
%% @doc  Find first syntactical parent node of `Node' which is correspond to conditions.
%%       A node is accepted if same as one from Nodes. Or {@link check_node/4}
%%       accept that with one filter from Filters. Or a `Pred' function  from
%%       Functions accept that (`Pred(CurrentNode) == true'). The checks are
%%       executed in the previous order. If all three lists are empty `Node' will
%%       be accepted.
%%
%%       The `Prev' contains the tag and index of the last link and the previous
%%       node of path to up the finded node. If the finded node is the given
%%       node than `Prev' is no.
%%       Matching type (node, filter, function) with the matced object
%%       (node() or id()) determine the matching case.
%%       The `Path' is unambiguous and indexed sintactical path from finded node
%%       to `Node'. It can use with {@link referl_graph:path/2} function.
%%
%% Example:
%% ```
%% upnode(Node, [Root],
%%              [expr_id, expr, record_info(fields, expr), [{kind,'==',tuple}]],
%%              [fun is_arity_qualifier/1])
%% '''
%% @see  check_node/4
%% @see  check_record/3
%% @see  get_rec_value/3
upnode(Node, [], [], []) -> Node;
upnode(Node, NodeList, FilterList, FunList)
        when is_list(NodeList), is_list(FilterList), is_list(FunList) ->
    upnode_(Node, no, NodeList, FilterList, FunList, []).

upnode_(Node, PrevNodeLink, NodeList, FilterList, FunList, Path) ->
    % Compare with Nodes
    case lists:dropwhile(fun(N)->(N=/=Node)end, NodeList) of
        [] ->
            % Check node with filters
            case lists:dropwhile(
                    fun({_Id,Type,Fields, Filters}) ->
                        not check_node(Node, Type, Fields, Filters)
                        end,
                    FilterList) of
                [] ->
                    % Check node with functions
                    case lists:dropwhile(
                            fun({_Id,Fun}) -> not Fun(Node) end, FunList) of
                        [] ->
                            % Go up to parent
                            case edges_idx(Node, back, ?ESG:parent(Node)) of
                                [] -> no;
                                [{Link,Parent,Idx}] ->
                                    upnode_(Parent,{Link,Node},NodeList,
                                        FilterList,FunList, [{Link,Idx}|Path])
                            end;
                        [{MatchId,_}|_] -> {Node,PrevNodeLink,function,MatchId,Path}
                    end;
                [{MatchId,_,_,_}|_] -> {Node, PrevNodeLink, filter, MatchId,Path}
            end;
        [MatchNode|_] -> {Node, PrevNodeLink, node, MatchNode,Path}
    end.

%% @private
%% @spec top_node(Node1::node(), Node2::node()) ->
%%           {node(), PathFromTopNodeToNode1::Path, PathFromTopNodeToNode1::Path} | no
%%       Path = [{LinkTag::atom(),LinkIdx::integer()}]
%% @throws term()
%% @doc  Find lowest common sintactical or lexical parent node of given
%%       sintactical or lexical nodes. If the top node is the root then it returns
%%       with no. Both node must be in the syntax tree else an erlang error
%%       will be raised. Paths from top node to nodes are also returned.
%% TODO use ?SYNTAX:top_node/2 if applicable
top_node(Node1, Node2) ->
    Root = ?GRAPH:root(),
    {_,_,node,_,PathToNode1} = upnode(Node1, [Root], [], []),
    {_,_,node,_,PathToNode2} = upnode(Node2, [Root], [], []),
    {PathCommon, Tail1, Tail2} = list_compare(PathToNode1, PathToNode2),
    case PathCommon of
        [] -> no;
        _  -> {hd(?GRAPH:path(Root, PathCommon)), Tail1, Tail2}
    end.


%% @ private
%% @ spec sin_path(Node1::node(), Node2::node()) -> [Link] | no
%%       Link = {LinkTag::atom(), back} | {LinkTag::atom(), LinkIdx::integer()}
%% @ throws term()
%% @ doc  Find the sintactical path beetween nodes in the sintax tree.
%%       The path is unambiguous. It can use with referl_graph:path/2 function.
%%       Both node must be in the syntax tree else an erlang error will be
%%       raised.
%sin_path(Node1, Node2) ->
%    Root = ?GRAPH:root(),
%    {_,_,node,_,PathToNode1} = upnode(Node1, [Root], [], []),
%    {_,_,node,_,PathToNode2} = upnode(Node2, [Root], [], []),
%    {_, Path1,Path2} = list_compare(PathToNode1, PathToNode2),
%    lists:foldl(fun({Tag,_Idx},Path)->[{Tag,back}|Path] end, Path2, Path1).



%%% ----------------------------------------------------------------------------
%%% Semantic query functions

%% TODO move these functions to ?SEMINF or use functions from there

%% @private
%% @spec is_auto_imported(FunName::atom(), FunArity::integer()) -> bool()
%% @doc  Check an auto imported function is exist with given name and arity.
is_auto_imported(FunName,FunArity) ->
    erl_internal:bif(FunName, FunArity).


%% @private
%% @spec has_function_definition(FunNode::node()) -> bool()
%% @doc  Check `FunNode' has definition.
has_function_definition(FunNode) ->
    [] =/= ?GRAPH:path(FunNode, [{fundef,back},funcl]).


%% @private
%% @spec is_arity_qualifier(ExprNode::node()) -> bool()
%% @doc  Check `ExprNode' is an arity qualifier.
is_arity_qualifier(Node) ->
    check_node(Node, expr, record_info(fields, expr),
        [{type,'==',expr},{kind,'==',infix_expr},{value,'==','/'}]) andalso
    [] =/= ?GRAPH:path(Node, [funref]).


%% @private
%% @spec is_module_qualifier(ExprNode::node()) -> bool()
%% @doc  Check `ExprNode' is a module qualifier.
is_module_qualifier(Node) ->
    check_node(Node, expr, record_info(fields, expr),
        [{type,'==',expr},{kind,'==',infix_expr},{value,'==',':'}]) andalso
    [] =/= ?GRAPH:path(Node, [{sub,1},modref]).


