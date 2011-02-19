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

%%% ============================================================================
%%% Module information

%%% @doc This module implements a console user interface for the tool.
%%%
%%% The idea behind `ri' is to enable the user to be able to do refactorings
%%% as easily as you can do debugging with Erlang's dbg module.
%%% You give a simple, short command, and get response on the console.
%%% Functions suffixed by `_h' give brief help about the respective function.
%%%
%%% == Server management command list ==
%%% <ul>
%%%   <li>add(FDML)
%%%       - add a module, file/dir or a list of these to the database</li>
%%%   <li>drop(FDML)
%%%       - drop (remove) the said way from the database</li>
%%%   <li>save(FDML)
%%%       - save the said way from the database to physical files</li>
%%%   <li>ls()
%%%       - lists files that are in the database</li>
%%%   <li>undo()
%%%       - undo the transformation (rollback, only once!)</li>
%%%   <li>reset()
%%%       - reset the database to an empty state, but valid schema</li>
%%%   <li>graph(Target)
%%%       - assume no options and call one of the next two</li>
%%%   <li>graph(Atom,Options)
%%%       - assume ".dot" extension and call the one below</li>
%%%   <li>graph(File,Options)
%%%       - draw the database graph with the given options</li>
%%% </ul>

%%% == Transformation command list ==
%%% <ul>
%%%   <li>elimvar(In, Pos)
%%%       - eliminate variable</li>
%%%   <li>extfun (In, Range)
%%%       - extract the selected expressions into a function</li>
%%%   <li>expfun (In, Pos)
%%%       - expand implicit funexpression to function call</li>
%%%   <li>genfun (In, Range, NewVarName)
%%%       - generalize function with a new argument</li>
%%%   <li>inlfun (In, Pos)
%%%       - inline function at application</li>
%%%   <li>inlmac (In, Pos)
%%%       - inline macro at use</li>
%%%   <li>intrec (In, Range, NewRec, [RecFldName1, RecFldName2, ...]))
%%%       - introduce record instead of tuple</li>
%%%   <li>merge  (In, Range, NewVarName)
%%%       - merge common subexpressions into a new variable</li>
%%%   <li>movfun (From, ToMod, [@{FunName,Arity@}|_])
%%%       - move function</li>
%%%   <li>movrec (From, To, [RecName|_])
%%%       - move record</li>
%%%   <li>movmac (From, To, [MacName|_])
%%%       - move macro</li>
%%%   <li>reorder(In, @{FunName,Arity@}, [ArgIdx|_])
%%%       - reorder function arguments</li>
%%%   <li>renfld (In, RecName, RecFldName, NewRecFldName)
%%%       - rename record field</li>
%%%   <li>renfun (In, @{FunName,Arity@}, NewFunName)
%%%       - rename function</li>
%%%   <li>renhrl (FromFile, ToFile)
%%%       - rename header file</li>
%%%   <li>renrec (In, RecName, NewRecName)
%%%       - rename record</li>
%%%   <li>renmac (In, MacName, NewMacName)
%%%       - rename macro</li>
%%%   <li>renmod (From, ToMod)
%%%       - rename module</li>
%%%   <li>renvar (In, Range, NewVarName)
%%%       - rename variable</li>
%%%   <li>tupfun (In, Range)
%%%       - change function arguments into tuple</li>
%%%   <li>upregex()
%%%       - upgrade regexp from "regexp" module to "re" module usage</li>
%%% </ul>
%%%
%%% == An explanation of argument types ==
%%% <ul>
%%%   <li>filename as string or module name as atom: In, From and To.</li>
%%%   <li>strings: FromFile, ToFile and MacName.</li>
%%%   <li>atoms: ToMod, RecName, RecFldName and FunName.</li>
%%%   <li>integers: Arity and ArgIdx.</li>
%%% </ul>
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

% @todo :wishlist factor down to smaller modules
% @todo expand the comments at the top to reflect added features
% @todo generated function and menu help from spec and doc tags
% @todo support for filename suffixes for add/drop
% @todo :wishlist better parameter checking (Name, Pos, ...)!
% @todo :wishlist verbose commenting with edoc tags
% @todo :wishlist spec tags
% @todo :wishlist research alternatives like
%       Beg,End -> line.col:line.col; line.col:col; line:regex ;
% @todo :wishlist function scope regexps
% @todo: better regexp mark handling
%

-module(ri).
-vsn("$Rev: 5623 $ ").

-export([help/0, h/0, h/1]).

-export([add/1, add/2,  drop/1, save/1, stop/0,
         ls/0,    ls/1,   reset/0, cat/1,   cat/2, cat/3,
         undo/0,  graph/0,graph/1,graph/2,
         svg/0,   svg/1,  svg/2, gn/2,
         getcfg/0,setcfg/3,
         build/0, transform/2 ]).
-export([envs/0, env/1, env/2, addenv/3, setenv/2,
         delenv/1, delenv/2, delenv/3]).

-export([add_h/0,    drop_h/0,  save_h/0, stop_h/0,
         ls_h/0,     reset_h/0, cat_h/0,
         undo_h/0,   graph_h/0, svg_h/0, gn_h/0,
         getcfg_h/0, setcfg_h/0,
         build_h/0,  transform_h/0 ]).
-export([envs_h/0, env_h/0, addenv_h/0, setenv_h/0,
         delenv_h/0]).

-export([elimvar/2,extfun/3, expfun/2, genfun/3,
         inlfun/2, inlmac/2, intrec/4,
         merge/3,  movfun/3, movrec/3, movmac/3,
         reorder/3,renfld/4, renfun/3,
         renhrl/2, renrec/3, renmac/3,
         renmod/2, renvar/3, tupfun/2, upregex/0,
         appfuncl/1]).
-export([q/1, q/2, q/3, q/4, metric/1, cluster/0, cluster/2]).
-export([lsl/0]).

-export([elimvar_h/0,extfun_h/0, expfun_h/0, genfun_h/0,
         inlfun_h/0, inlmac_h/0, intrec_h/0,
         merge_h/0,  movfun_h/0, movrec_h/0, movmac_h/0,
         reorder_h/0,renfld_h/0, renfun_h/0,
         renhrl_h/0, renrec_h/0, renmac_h/0,
         renmod_h/0, renvar_h/0, tupfun_h/0, upregex_h/0,
         appfuncl_h/0]).
-export([q_h/0, metric_h/0, cluster_h/0]).
-export([lsl_h/0]).

-export([start_yaws/0, start_yaws/1, stop_yaws/0]).

-export([errors/0, errors/1]).

-export([error_text/2]).
-export([test/0, test/1, test/2, testall/0]).
%-export([rgxpos_text/2]). %%DEBUG
%-export([getparen/1]). %%DEBUG

-export([cat_errors/0, cat_errors/1]).

-export([modsave/1, modload/1]).

-export([draw_dep/2, print_dep/2]).

% @private only for internal use!
-export([global_printer_process/0, get_constrained/3, ui_loop/1]).

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

-define(DEF_GRAPH_NAME,graph).
-define(PrintProc,referl_ri_global_printer). %@todo supervise

ui(NameArgs)->
%    io:format(" DEBUG ri request ~p~n",[NameArgs]),
    _PIDPORT = spawn_printer_process(),
    ReqID = ?UI:getid(),
    ok = ?UI:request(ReqID,NameArgs),
    ui_loop(ReqID).

ui_loop(ReqID) ->
    receive
        {ReqID, reply, R} ->
            case R of
                {error, {_Code, Msg}} ->
                    message("Error: " ++ Msg),
                    error;
                _ ->
                    R
            end;

        {ReqID, progress, {add, File, 1, Max}} ->
            message2(io_lib:format("loading: ~s (~p forms)~n~4w|",
                   [File, Max, 1])),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {drop, File, 1, Max}} ->
            message2(io_lib:format("dropping: ~s (~p forms)~n~4w|",
                   [File, Max, 1])),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {add, File, Percent, FormCount, FormMax, KBps}} ->
            print_progress(File, Percent, FormCount, FormMax, KBps),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {drop, File, _Percent, FormCount, FormMax, KBps}} ->
            print_progress(File, FormCount/FormMax, FormCount, FormMax, KBps),
            ?MODULE:ui_loop(ReqID);

        {ReqID, question, {QID,Questions}} ->
            Text1 = " (blank to abort).",
            case form_type(Questions) of
                checkbox->
                    Text0 = "Please select some items from the list",
                    io:format("~s~s~n",[Text0,Text1]),
                    ok = answer_box(ReqID, QID, Questions, checkbox);
                radio->
                    Text0 = "Please choose an item from the list",
                    io:format("~s~s~n",[Text0,Text1]),
                    ok = answer_box(ReqID, QID, Questions, radio);
                other->
                    case interactive(Questions) of
                        true ->
                            Text0 = "Please answer the following question",
                            io:format("~s~s~n",[?MISC:plural(Text0,Questions),Text1]);
                        false ->
                            Text0 = "See the direct information feed below:",
                            io:format("~s~n",[Text0])
                        end,
                    ok = answer0(ReqID, QID, Questions)
            end,
            ?MODULE:ui_loop(ReqID);
        M ->
            Ui_loop = io_lib:format(" WARNING ri:ui_loop(~p) got:~n ~p ~n",[ReqID,M]),
            ?d(Ui_loop),
            ?MODULE:ui_loop(ReqID)
    end.

print_progress(File, Percent, FormCnt, FormMax, KBps) ->
    PrgMax       = 30,
    Mark         = $\>,
    KBpsTxt      = ?MISC:format("~5.2f kB/s", [KBps]),
    KBpsLen      = length(KBpsTxt),
    MarkCnt      = round(Percent*PrgMax),
    PrgWidth     = PrgMax - 1 - KBpsLen,
    PrgWidthTxt  = integer_to_list(PrgWidth),
    FormFNameTxt = ?MISC:format("[~4w/~4w] ~s",
                                [FormCnt, FormMax, filename:basename(File)]),
    case PrgWidth < MarkCnt of
        false ->
            MarkCntTxt = integer_to_list(MarkCnt),
            io:format("\r|~-" ++ PrgWidthTxt ++ "." ++ MarkCntTxt ++ "c ~s| ~s",
                      [Mark, KBpsTxt, FormFNameTxt]);
        true ->
            MarkCntTxt = integer_to_list(MarkCnt - 1 - KBpsLen),
            io:format("\r|~s ~-" ++ PrgWidthTxt ++ "." ++ MarkCntTxt ++ "c| ~s",
                      [KBpsTxt, Mark, FormFNameTxt])
    end,
    case Percent == 1 of
        true  -> io:put_chars("\n");
        false -> ok
    end.


interactive(Questions)->
    Formats = lists:usort([ hd(?MISC:pgetu([format],Q)) || Q <- Questions ]),
    not is_subset(Formats,[info]).

form_type(Questions)->
    Formats = lists:usort([ hd(?MISC:pgetu([format],Q)) || Q <- Questions ]),
    case ?MISC:intersect(Formats, [radio, checkbox]) == [] of
        true -> other;
        false ->
            case is_subset(Formats, [info, radio]) of
                true -> radio;
                false ->
                    case is_subset(Formats, [info, checkbox]) of
                        true -> checkbox;
                        false -> other
                    end
            end
    end.

is_subset(Sub,Sup)->
    []==Sub--Sup.

answer0(ReqID, QID, Questions) ->
    answer1(ReqID, QID, [], Questions). %@todo check validators

answer1(ReqID, QID, Answers, _Questions=[Question|Qs]) ->
    [Format,Text] = ?MISC:pgetu([format,text], Question),
    Result =
        case Format of
            textbox ->
                get_constrained(
                  " "++Text++" ",
                  "",
                  fun(X)->{ok, X}end);
            T when T==yesno; T==checkbox -> % TODO: spank interaction developer
                get_constrained(
                  " "++Text++" (y/n) -> ",
                  "Answer either 'y' or 'n'",
                  fun
                      ("y") -> {ok, yes};
                      ("n") -> {ok, no};
                      (_) ->   {error, incorrect}
                  end);
            info ->
                io:format(" ~s~n", [Text]),
                {just, info};
%            RC when RC==radiotodo orelse RC==checkbox -> %@todo
%                _Max = show_boxes(Questions, RC, 1),
%                answer2(ReqID, QID, Answers, Questions, RC, 1);
            _ ->
                Prompt = " Type in the answer for this question" ++
                    " as a raw Erlang term:",
                get_term(io_lib:format("~s~n ~p ~n -> ", [Prompt,Question]))
        end,
    case Result of
        nothing ->
            ?UI:request(ReqID, {cancel, QID});
        {just, Answer} ->
            answer1(ReqID, QID, [Answer|Answers], Qs)
    end;
answer1(ReqID, QID, Answers, []) ->
    ?UI:request(ReqID, {reply, QID, lists:reverse(Answers)}).

answer_box(ReqID, QID, Questions, Type) ->
    Min = 1,
    Idx = show_boxes(Questions, Type, Min),
    {Init, Err} =
        case Type of
            radio ->
                {" type the index of your choice: ",
                 "Answer with an index in range"};
            checkbox ->
                {" type the indices (space delimited): ",
                 "Answer with indices in range"}
        end,
    Result =
    get_constrained(
      Init,
      Err,
      fun
          (S) ->
              {Split,_} = ?MISC:string_split(S, [" "], -1, false, false),
              Indices0 = [case string:to_integer(M) of
                            {error,_} -> Min-1;
                            {N,_Rest}-> N
                          end || M <- Split ],
              Indices = lists:usort(Indices0),
              Nothing = ((Type==checkbox) andalso ([0] == Indices)),
              {Beg, End} =
                case Nothing of
                    true ->  {fun(Q)-> [0|Q] end,
                              fun erlang:tl/1};
                    false -> {fun(Q)-> Q end,
                              fun(A)-> A end}
                end,
              Inrange =
                Nothing orelse
                case Type of
                  radio -> length(Indices)==1;
                  checkbox -> true
                end andalso
                lists:all(fun(I)-> I>=Min andalso I<Min+length(Idx) end, Indices),
              case Inrange of
                  true  ->
                    Questions1 = Beg(Questions),
                    NumQ = lists:zip(lists:seq(Min,length(Questions1)), Questions1),
                    A = [begin
                            [Format] = ?MISC:pgetu([format], Q),
                            case Format of
                                info -> info;
                                Type ->
                                    case find(I-Min+1, Idx) of
                                        false ->
                                            no;
                                        N -> case lists:member(N, Indices) of
                                            true -> yes;
                                            false -> no
                                        end
                                    end
                            end
                        end || {I, Q} <- NumQ],
                    A2 = End(A),
                    {ok, A2};
                  false ->
                    {error, incorrect}
              end
      end),
    case Result of
        nothing ->
            ?UI:request(ReqID, {cancel, QID});
        {just, Answers} ->
            ?UI:request(ReqID, {reply, QID, Answers})
    end.

find(E, L)->
    find(E, L, 1).
find(_E, [], _N) ->
    false;
find(E, [H|_T], N) when H==E->
    N;
find(E, [_H|T], N)->
    find(E, T, N+1).

show_boxes([Question|Qs], Type=checkbox, Min) ->
    io:format(" 0. (none)~n", []),
    show_boxes([Question|Qs], Type, Min, Min, [0]);
show_boxes([Question|Qs], Type=radio, Min) ->
    show_boxes([Question|Qs], Type, Min, Min, []).
show_boxes([Question|Qs], Type, Num, Idx, Idxs) ->
    [Format,Text] = ?MISC:pgetu([format,text], Question),
    case Format of
        info ->
            io:format(" ~s~n", [Text]),
            show_boxes(Qs, Type, Num, Idx+1, Idxs);
        Type ->
            io:format(" ~p. ~s~n", [Num,Text]),
            show_boxes(Qs, Type, Num+1, Idx+1, [Idx | Idxs])
    end;
show_boxes([], _Type, _Num, _Idx, Idxs)->
    lists:reverse(Idxs).


last_chars(String, N) ->
    K=string:len(String),
    case (0=<N andalso N=<K) of
        true ->
            string:substr(String, K-N+1);
        false ->
            String
    end.

get_term(Prompt) ->
    get_constrained(Prompt,
                    "You have given an invalid term.",
                    fun(Pruned)->
                            String = case last_chars(Pruned,1) of
                                         [$.] -> Pruned;
                                         _    -> Pruned ++ "."
                                     end,
                            ?MISC:string_to_term(String)
                    end).

get_constrained(Prompt, Error, Process) ->
    Raw = io:get_line(Prompt),
    Pruned = ?MISC:strip(Raw),
    case Pruned of
        "" ->
            nothing;
        _ ->
            case Process(Pruned) of
                {ok, Result} ->
                    {just, Result};
                _ ->
                    io:format("~s~n",[Error]),
                    ?MODULE:get_constrained(Prompt, Error, Process)
            end
    end.

spawn_printer_process()->
    case whereis(?PrintProc) of
        undefined ->
            PID = spawn_link(fun global_printer_init/0),
            register(?PrintProc,PID),
            referl_ui_evsend:start(PID);
        PID ->
            PID
    end.

global_printer_init()->
%   ?d(spawn_global_printer), TODO
   receive
      installed ->
%         ?d(connected_global_printer), TODO
         global_printer_process()
   end.

global_printer_process()->
    receive
        M={global, statusinfo, [{change, Changes}]} ->
            [case ?MISC:pget([rename,content,present,error],Prop) of
                [[New],_,_,_]->
                    message("renamed "++File++" to "++New);
                [_,_,_,[_]]->
                    message("error in "++File);
                [_,[true],_,_]->
                    message("modified "++File);
                [_,_,[true],_]->
                    ok;
                [_,_,[false],_]->
                    message("dropped "++File);
                _->
                    ?d({debug, M})
             end || {File,Prop} <- Changes],
            ?MODULE:global_printer_process();
        _M={global, _, _} ->
%            GlobPrnt = M,
%            ?d(GlobPrnt),
            ?MODULE:global_printer_process();
        M ->
            Msg = "terminating due to unexpected message:",
            io:format(" ERROR: ~p ~s~n ~p~n",[?PrintProc,Msg,M]),
            error
    end.

%% @type mod_file() = atom() | string().
%% @type mod_file_dir_list() = mod_file() | [mod_file_dir_list()].
%% @type eol() = 'cr' | 'lf' | 'crlf' | {eol(), 'eol' | 'noeol'}.
%% @type ri_result() = any(). %% TODO


%% @private
message(Data) ->
    io:format("~s~n", [Data]).

%% @private
message2(Data) ->
    io:format("~s", [Data]).

%% @private
error_text(not_ready,[File]) ->
    ["File ", File, " is not ready for refactoring"];
error_text(invalid_regexp,[Rgx]) ->
    ["The regular expression ", io_lib:print(Rgx), " is invalid"];
error_text(invalid_pos,[Pos]) ->
    ["The position ", io_lib:print(Pos), " is invalid"];
error_text(unmatched_regexp,[Rgx]) ->
    ["The regular expression ", Rgx, " yielded no result"];
error_text(unbound_idx_regexp,[Rgx,Idx]) ->
    ["The index ",integer_to_list(Idx),
     " is invalid for the regular expression ", Rgx,
     " in this file"];
error_text(no_file,[]) ->
    ["No file specified"];
error_text(internal_unexpected,[X]) ->
    ["Unexpected return value ",
     io_lib:print(X)];
error_text(load_beam, [String]) ->
    ["BEAM loading failure: ", String];

error_text(ErrType, ErrParams) ->
    ["Unknown error: {",
     io_lib:print(ErrType), ", ", io_lib:print(ErrParams), "}"].


%% @doc Shows brief help text
help() ->
    h().

%% @doc Shows brief help text
h() ->
  message2([
    "The following help topics are available:\n"
    " server   - server management\n"
    " renmov   - rename and move refactorings\n"
    " refac    - other refactorings\n"
    " allrefac - all refactorings\n"
    " regexp   - regexp syntax information\n"
    " usage    - a few words about basic usage\n"
    "\n"
    "call " ++ io_lib:write(?MODULE) ++ ":h(Item) for"
    " a brief description of one of the items above.\n"
    "You can also give one of the exported functions to get help on that.\n"
  ]).

%% @doc Shows brief help text on a topic or function
h(all) ->
    h(),
    lists:foreach(fun h/1, [usage,refac,server,regexp]);
h(Topic) when is_atom(Topic) ->
    try
        message2(ht(Topic))
    catch
        error:function_clause ->
            try
                Name = list_to_atom(atom_to_list(Topic)++"_h"),
                apply(?MODULE,Name,[])
            catch
                error:undef ->
                    message("The given help topic cannot be found!\n"),
                    h()
            end
    end;
h(_) ->
    message("Invalid argument!\n"),
    h().

ht(usage) ->
  [ "You first need to add some files to the database with"
    " the add/1 command. Then you can commit transformations on them,"
    " by referring to them by either module name or filename.\n" ];

ht(renmov) ->
  [ "movfun(from_mod,to_mod,[{f,2},{g,0}])"
    " - move functions to another module\n"
    "movrec(from_mod,to_mod,[rec1,rec2])"
    " - move records to another module\n"
    "movmac(from_mod,to_mod,[\"Mac1\",\"Mac2\"])"
    " - move macro to another module\n"
    "renfld(mod_or_file,rec,oldfield1,newfield1)"
    " - rename record field\n"
    "renfun(mod_or_file,{func,2},newfun)"
    " - rename function\n"
    "renrec(mod_or_file,oldrec,newrec)"
    " - rename record\n"
    "renmac(mod_or_file,\"OldMac\",\"NewMac\")"
    " - rename macro\n"
    "renmod(mod_or_file, newmod)"
    " - rename module\n"
    "renhrl(\"old.hrl\", \"new.hrl\")"
    " - rename header file\n"
    "renvar(mod_or_file, \"X=\", \"NewVar\")"
    " - rename variable\n"
  ];

ht(refac) ->
  [
    "elimvar(mod_or_file,\"X=\")"
    " - eliminate variable\n"
    "extfun(mod_or_file,\"A+B\",newfunc)"
    " - extract function\n"
    "expfun(mod_or_file, \"fun g\")"
    " - expand implicit funexpression to function call\n"
    "genfun(mod_or_file, \"[+]<2\", \"NewArg\")"
    " - generalize function with new argument\n"
    "inlfun(mod_or_file,\"f\\(1\")"
    " - inline function at application\n"
    "inlmac(mod_or_file,\"?Mac\")"
    " - inline macro at use\n"
    "intrec(mod_or_file, \"{X,Y}\", newrec, [f1, f2])"
    " - introduce record in place of tuple\n"
    "merge(mod_or_file,\"1+2\",\"NewVar\")"
    " - merge common subexpressions into a new variable\n"
    "reorder(mod_or_file,{func,2},[2,1])"
    " - reorder function arguments\n"
    "tupfun(mod_or_file,\"A,B\")"
    " - change function arguments into tuple\n"
    "upregex()"
    " - upgrade regexp from \"regexp\" module to \"re\" module usage\n"
  ];

ht(allrefac) ->
    ht(refac) ++ ht(renmov);

ht(server) ->
  [ "add(X)   - add a module, file, directory or a list of these"
    " to the database\n"
    "drop(X)  - drop (remove) like in add/1\n"
    "save(X)  - save from the database to physical file(s)\n"
    "ls()     - list database contents\n"
    "ls(FM)   - list forms in the file or module\n"
    "cat(FM)  - prints the contents of a file or module from the database\n"
    "cat(FM,RM)  - prints the definition of a macro or record\n"
    "cat(FM,F,A) - prints the definition of a function\n"
    "undo()   - undo the transformation (rollback, only once!)\n"
    "reset()  - reset the database to an empty state, but valid schema\n"
    "graph(T) - assume no options and call graph/2\n"
    "graph(Atom,Opt) - assume \".dot\" extension and call graph/2\n"
    "graph(File,Opt) - draw the database graph with the given options\n"
    "svg/0,1,2 - calls graph and generates SVG output\n"
    "gn(Type,Idx) - returns the data of a graph node\n"
    "getcfg() - display the current settings that can be set via setcfg/3\n"
    "setcfg(AppDirs,IncDirs,OutDir) - save settings\n"
  ];

ht(regexp) ->
  [ "You can substitute regexp in place "
    "of positions and posranges like so:\n"
    "* a plain regexp matches precisely a posrange or "
    "marks the start of a position;\n"
    "* use plain angle brackets '<' and '>' to highlight"
    " part of a regexp;\n"
    "* to get angle bracket characters instead,"
    " escape them like \\< and \\>;\n"
    "* the nth match can be selected instead of the first one "
    "by substituting the tuple {\"regexp\",index1} for \"regexp\".\n"
  ].

%%% ============================================================================
%%% Refactorings

extfun_h() ->
    message("extfun(ModFile,Range_of_body,NewFunc)").

%% @doc Extract function refactoring
extfun(File, Range, Name)
  when is_atom(Name), (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(extract_fun, File, Range, [{name, Name}]);
extfun(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, atom())"),
    usage.
% ri:extfun(mod_or_file,"A+B",f).


merge_h() ->
    message("merge(ModFile,Range_of_expression,NewVar)").

%% @doc Merge common subexpressions refactoring
merge(File, Range, Varname=[C|_]) when is_integer(C),
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(merge, File, Range, [{varname, Varname}]);
merge(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, string())"),
    usage.

% ri:merge(mod_or_file,"1+2","V").

inlfun_h() ->
    message("inlfun(ModFile,Pos_of_fun_application)").

%% @doc Inline function refactoring
inlfun(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(inline_fun, File, Pos, []);
inlfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.

% ri:inlfun(mod_or_file,"f[(]1").

inlmac_h() ->
    message("inlmac(ModFile,Pos_of_macro_use)").

%% @doc Inline macro refactoring
inlmac(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(inline_mac, File, Pos, []);
inlmac(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.
% ri:inlmac(mod_or_file,"?Mac").

tupfun_h() ->
    message("tupfun(ModFile,Range_of_arguments)").

%% @doc Tuple function arguments refactoring
tupfun(File, Range) when (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(tuple_funpar, File, Range, []);
tupfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()})"),
    usage.
%%@todo :wishlist Begin,End -> name/arity first last
% ri:tupfun(mod_or_file,"A,B").

%%Not recommended
%reorder(File, Fun, Arity, Order) ->
%    reorder(File, {Fun,Arity}, Order).

reorder_h() ->
    message("reorder(ModFile,{Fun,Arity},PermutationList)").
%@todo :wishlist check Order

%% @doc Reorder function arguments refactoring
reorder(File, {Fun,Arity}, Order=[I|_])
  when (is_atom(File) or is_list(File)), is_atom(Fun), is_integer(Arity), is_integer(I) ->
    transform_catch(reorder_funpar, File,
              [{function, Fun}, {arity, Arity}, {order, Order}]);
reorder(_,_,_)->
    message("::(modfile(), {atom(), natural()}, [positive()])"),
    usage.
% ri:reorder(mod_or_file,{f,2},[2,1]).

expfun_h() ->
    message("expfun(ModFile,Pos_of_funexpr)").

%% @doc Expand implicit fun expression refactoring
expfun(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(expand_funexpr, File, Pos, []);
expfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.
% ri:expfun(mod_or_file, "fun g").

movfun_h() ->
    message("movfun(Source,Target,FunctionList=[{FunctionName,Arity}|_])").

%% @doc Move function refactoring
movfun(Source, Target, Fun={A,I})
  when is_atom(A), is_integer(I) ->
    movfun(Source, Target, [Fun]);
% ri:movfun(mod_or_file,b,{f,2}).

%%@todo :wishlist check Funlist
movfun(Source, Target, Funlist=[{A,I}|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_atom(A), is_integer(I) ->
    transform2(move_fun, Source, Target, [{funlist, Funlist}]);
movfun(_,_,_)->
    message("::(modfile(), modfile(), [{atom(), natural()}])"),
    usage.
% ri:movfun(mod_or_file,b,[{f,2},{g,0}]).

movrec_h() ->
    message("movrec(Source,Target,RecordList=[Record|_])").

%% @doc Move record refactoring
movrec(Source, Target, Rec) when
        is_atom(Rec) ->
    movrec(Source, Target, [Rec]);
% ri:movrec(mod_or_file,b,rec).

%%@todo :wishlist check Reclist
movrec(Source, Target, Reclist=[A|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_atom(A) ->
    transform2(move_rec, Source, Target, [{reclist, Reclist}]);
movrec(_,_,_)->
    message("::(modfile(), modfile(), [atom()])"),
    usage.
% ri:movrec(mod_or_file,b,[r1,r2]).

movmac_h() ->
    message("movmac(Source,Target,MacroList=[Macro|_])").

%% @doc Move macro refactoring
movmac(Source, Target, Mac=[C|_]) when
        is_integer(C) ->
    movmac(Source, Target, [Mac]);
% ri:movmac(mod_or_file,b,"Mac").

%%@todo :wishlist check Maclist
movmac(Source, Target, Maclist=[[C|_]|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_integer(C) ->
    transform2(move_mac, Source, Target, [{maclist, Maclist}]);
movmac(_,_,_)->
    message("::(modfile(), modfile(), [string()])"),
    usage.
% ri:movmac(mod_or_file,b,["Mac1","Mac2"]).

genfun_h() ->
    message("genfun(ModFile,Range_of_body,NewVar)").

%% @doc Generalize function by new argument refactoring
genfun(File, Range, Newname=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)),
        is_integer(C) ->
    transformr(gen, File, Range, [{varname, Newname}]);
genfun(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, string())"),
    usage.

% ri:genfun(mod_or_file, "[+]\\(2", "Two").

%%Not recommended
%renfun(File, Fun, Arity, Newname) ->
%    renfun(File, {Fun,Arity}, Newname).

renfun_h() ->
    message("renfun(ModFile,{FunctionName,Arity},NewFun)").

%% @doc Rename function refactoring
renfun(File, {Fun,Arity}, Newname) when
        (is_atom(File) or is_list(File)),
        is_atom(Fun), is_integer(Arity), is_atom(Newname) ->
    transform_catch(rename_fun, File,
              [{function, Fun}, {arity, Arity}, {name, Newname}]);
renfun(_,_,_)->
    message("::(modfile(), {atom(), natural()}, atom())"),
    usage.

% ri:renfun(mod_or_file,{f,2},new).

renvar_h() ->
    message("revar(ModFile,Pos_of_variable,NewVar)").

%% @doc Rename variable refactoring
renvar(File, Pos, Newname=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)),
        is_integer(C) ->
    transformp(rename_var, File, Pos, [{varname, Newname}]);
renvar(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()}, string())"),
    usage.

% ri:renvar(mod_or_file, "X=", "V").

renmod_h() ->
    message("renmod(OldModFile,NewMod)").
%% @doc Rename module refactoring
renmod(File, Newname) when
        (is_atom(File) or is_list(File)),
        is_atom(Newname) ->
    transform_catch(rename_mod, File, [{name, Newname}]);
renmod(_,_)->
    message("::(modfile(), atom())"),
    usage.

% ri:renmod(mod_or_file, newmod).

renhrl_h() ->
    message("renhrl(OldHrl,NewHrl)").

%% @doc Rename header refactoring
renhrl(File, Newname=[C|_]) when
        (is_atom(File) or is_list(File)), % note: can't be an atom...
        is_integer(C) ->
    transform_catch(rename_header, File, [{name, Newname}]);
renhrl(_,_)->
    message("::(modfile(), string())"),
    usage.

% ri:renhrl("a.hrl", "b.hrl").

renrec_h() ->
    message("renrec(ModFile,OldRecord,NewRecord)").

%% @doc Rename record refactoring
renrec(File,Record,NewName) when
        (is_atom(File) or is_list(File)),
        is_atom(Record), is_atom(NewName) ->
    transform_catch(rename_rec, File,
              [{record, Record}, {name, NewName}]);
renrec(_,_,_)->
    message("::(modfile(), atom(), atom())"),
    usage.


% ri:renrec(mod_or_file,recname,newrecname).

renfld_h() ->
    message("renfld(ModFile,Record,OldField,NewField)").

%% @doc Rename record field refactoring
renfld(File,Record,Field,NewName) when
        (is_atom(File) or is_list(File)),
        is_atom(Record), is_atom(Field), is_atom(NewName) ->
    transform_catch(rename_recfield, File,
              [{record,Record}, {recfield,Field}, {name,NewName}]);
renfld(_,_,_,_)->
    message("::(modfile(), atom(), atom(), atom())"),
    usage.


% ri:renfld(mod_or_file,recname,field1,newfield1).

elimvar_h() ->
    message("elimvar(ModFile,Pos_of_variable)").

%% @doc Eliminate variable by inlining refactoring
elimvar(File, Pos) when
        (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(elim_var, File, Pos, []);
elimvar(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.


% ri:elimvar(mod_or_file,"X=").

%%Not recommended
%intrec(File, A, B, Newname, Fields) ->
%    intrec(File, {A,B}, Newname, Fields).

intrec_h() ->
    message("intrec(ModFile,Range_of_tuple,NewRecord,Fields=[field|_])").

%% @doc Introduce record in place of a tuple refactoring
intrec(File, URange, Newname, AFields=[A|_])
  when is_atom(A) ->
    SList = lists:map(fun atom_to_list/1, AFields),
    Fields = string:join(SList," "),
    intrec(File, URange, Newname, Fields);
% ri:intrec(mod_or_file, "{X,Y}", newrec, [f1, f2]).

intrec(File, Range, Newname, Fields=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)),
        is_atom(Newname), is_integer(C) ->
    transformr(introduce_rec, File, Range,
               [{name,Newname}, {text,Fields}]);
intrec(_,_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, atom(), [atom()])"),
    usage.


% ri:intrec(mod_or_file, "{X,Y}", rec, "f1 f2"]).

renmac_h() ->
    message("renmac(ModFile,OldMacro,NewMacro)").

%% @doc Rename macro refactoring
renmac(File, Macro, Newname) when
        (is_atom(File) or is_list(File)),
        (is_atom(Macro) or is_list(Macro)),
        (is_atom(Newname) or is_list(Newname)) ->
    transform_catch(rename_mac, File, [{macro,Macro}, {macname,Newname}]);
renmac(_,_,_)->
    message("::(modfile(), atom() | string(), atom() | string())"),
    usage.
% ri:renmac(mod_or_file,"Macname","NewMecname").

%intimp(File) ->
%    transform_catch(introduce_import, File, []).

upregex_h() ->
    message("upregex()").

%% @doc Upgrade regular expression syntax refactoring
upregex() ->
    catch_referr(fun()->
        transform(upgrade_regexp,[])
    end).

appfuncl_h()->
    message("appfuncl(Clusters)").

%% @doc Apply function clustering results refactoring
appfuncl(Clusters)->
    catch_referr(fun()->
                         transform(apply_funcluster,
                                   [{funclusters,Clusters}])
                 end).

metric_h()->
    message("metric(Query)").

%% @doc Run a metric query
metric(Query) when is_atom(Query)->
    metric(atom_to_list(Query));
metric(Query) when is_list(Query)->
    transform_(metric_query,
               [{querys,Query}],
               fun(Results)->
                   [Result] = ?MISC:pgetu([result],Results),
                   message(Result),
                   ok
               end);
metric(_)->
    message("::(atom() | string())"),
    usage.

cluster_h()->
    message(["cluster()\n"
             "cluster(Algorithm,EntityType)"]).

cluster()->
    cluster_([]).

cluster(Algorithm,EntityType)->
    cluster_([{algorithm,Algorithm},{entity,EntityType}]).

cluster_(Args)->
    transform_(clustering,
               Args,
               fun(_)->result end).

q_h()->
    message2(["q(SemanticQuery)\n"
              "q(ModFile,SemanticQuery)\n"
              "q(ModFile,PositionRegexp,SemanticQuery)\n"
              "q(SemanticQuery,Options)\n"
              "q(ModFile,SemanticQuery,Options)\n"
              "q(ModFile,PositionRegexp,SemanticQuery,Options)\n"
              "Example usage:\n"
              " ri:q(\"mods.funs.name\").\n"
              " ri:q(mod1, \"f\\\\(X, Y\\\\)\", \"@fun.var\").\n"
              " ri:q(\"mods.funs\",[linenum,{out,\"result.txt\"}]).\n"]).

%% @doc Run a semantic query
q(Q) when is_atom(Q)->
    q(atom_to_list(Q));
q(Q=[C|_]) when is_integer(C)->
    q_(Q, [], addqnone());
q(_)->
    message("::(atom() | string())"),
    usage.

%% @doc Run a semantic query starting from the given file
q(ModFile,Q) when (is_atom(ModFile) or is_list(ModFile)), is_atom(Q)->
    q(ModFile,atom_to_list(Q));
q(ModFile,Q=[C|_]) when (is_atom(ModFile) or is_list(ModFile)), is_integer(C)->
    q_(Q,[],addqmod(ModFile));

q(Q, Options=[O|_]) when is_atom(Q), (is_atom(O) orelse is_tuple(O))->
    q(atom_to_list(Q),Options);
q(Q=[C|_], Options=[O|_]) when is_integer(C), (is_atom(O) orelse is_tuple(O))->
    q_(Q,Options,addqnone());
q(_, _)->
    message("::(modfile() | atom() | string(), atom() | string() | proplist())"),
    usage.

%% @doc Run a semantic query starting from the given position
q(ModFile,Pos=[A|_],Q)
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_atom(Q)->
    q(ModFile,Pos,atom_to_list(Q));
q(ModFile,Pos=[A|_],Q=[B|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_integer(B)->
    q_(Q,[],addqmodpos(ModFile,Pos));

q(ModFile,Q,Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_atom(Q), (is_atom(O) orelse is_tuple(O)) ->
    q(ModFile,atom_to_list(Q),Options);
q(ModFile,Q=[C|_], Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(C), (is_atom(O) orelse is_tuple(O)) ->
    q_(Q,Options,addqmod(ModFile));
q(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()} | atom() | string(), atom() | string() | proplist())"),
    usage.

%% @doc Run a semantic query starting from the given position with options
q(ModFile,Pos=[A|_],Q,Options=[O|_]) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_integer(A), is_atom(Q),
        (is_atom(O) orelse is_tuple(O)) ->
    q(ModFile,Pos,atom_to_list(Q),Options);
q(ModFile,Pos=[A|_],Q=[B|_], Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_integer(B),
       (is_atom(O) orelse is_tuple(O)) ->
    q_(Q,Options,addqmodpos(ModFile,Pos));
q(_,_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, atom() | string(), proplist())"),
    usage.

addqnone()->
    fun()->
        []
    end.

addqmod(ModFile)->
    fun()->
        [{file,mod2filec(ModFile)}]
    end.

addqmodpos(ModFile,Pos)->
    fun()->
        File = mod2filec(ModFile),
        [{file,File},
        {position,to_pos(File,Pos)}]
    end.

q_(Query=[Ch|_],Options,StartFun)
  when is_integer(Ch), is_list(Options), is_function(StartFun,0) ->
    catch_referr(
      fun()->
              Start = StartFun(),
              DispOpt =
                  [case E of
                       {out,_} ->
                           [];
                       {linenum,true}->
                           [{positions,linecol}];
                       _ ->
                           message("warning: unknown option '"++io_lib:print(E)++"'"),
                           []
                   end || E <- proplists:unfold(Options)],
              Args = [{display_opt,lists:append(DispOpt)},
                      {start_opt,Start},
                      {querystr,Query}],
              transform_(semantic_query,Args,
                  fun(Results)->
                    [Result] = ?MISC:pgetu([result],Results),
                    case proplists:lookup_all(out,Options) of
                        []->
                            io:put_chars(Result);
                        [{out,FileName}]->
                            {ok,IODev} = file:open(FileName, [write]),
                            io:put_chars(IODev, Result),
                            ok = file:close(IODev)
                    end,
                    ok
                  end)
      end).

%%% ============================================================================
%%% Others

%cluster_agglom() -> %%@todo :wishlist

%cluster_genetic() -> %%@todo :wishlist

%%% ============================================================================
%%% Server management

%% @private
get_filenode(ModFile) ->
    File = mod2file(ModFile),
    ?Query:exec1(?File:find(File),?RefError(file_not_present,[File])).

%% @private
get_modnode(ModFile) ->
    Mod = file2mod(ModFile),
    ?Query:exec1(?Mod:find(Mod),?RefError(mod_not_found,[Mod])).

cat_h() ->
    message(["cat(ModFile)\n",
             "cat(ModFile,RecMac)\n"
             "cat(ModFile,FunName,Arity)"]).

%% @doc Display the contents of the given file or module
cat(ModFile) when
        (is_atom(ModFile) or is_list(ModFile)) ->
    catch_referr(fun() ->
        message(?Syn:tree_text(get_filenode(ModFile))) end);
cat(_)->
    message("::(modfile())"),
    usage.

%% @doc Display the definition of the given record or macro
cat(ModFile, {FunName,Arity}) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_atom(FunName), is_integer(Arity)->
    cat(ModFile, FunName, Arity);
cat(ModFile, RecMac) when
        (is_atom(ModFile) or is_list(ModFile)),
        (is_atom(RecMac) or is_list(RecMac)) ->
    catch_referr(fun() ->
        message2(recmactext(ModFile, RecMac))
    end);
cat(_,_)->
    message("::(modfile(), {atom(), natural()} | atom() | string())"),
    usage.

%% @doc Display a function definition
cat(ModFile, FunName, Arity) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_atom(FunName), is_integer(Arity)->
    catch_referr(fun() ->
        message2(funtext(ModFile, FunName, Arity))
    end);
cat(_,_,_)->
    message("::(modfile(), atom(), natural())"),
    usage.

%% @private
recmactext(ModFile,RecMac)->
    File = get_filenode(ModFile),
    QE   = fun(L)-> ?Query:exec([File], ?Query:seq(L)) end,
    Obj  =
        case QE([?Rec:find(RecMac),?Rec:form()]) of
             [] ->
                 M = QE([?Macro:find(RecMac)]),
                 case (M==[]) and (is_atom(RecMac)) of
                     true  -> QE([?Macro:find(io_lib:write(RecMac))]);
                     false -> M
                 end;
             R  -> R
        end,
    case Obj of
        []  -> throw(?RefError(mac_not_found,[RecMac]));
        [O] -> ?Syn:tree_text(O)
    end.

%% @private
funtext(ModFile, FunName, Arity) ->
    Mod = get_modnode(ModFile),
    Fun = ?Query:exec1([Mod],
                       ?Query:seq([?Fun:find(FunName,Arity),
                                   ?Fun:definition()]),
                       ?RefError(fun_not_found,[FunName,Arity])),
    ?Syn:tree_text(Fun).

%% -----------------------------------------------------------------------------

getcfg_h() ->
    message("getcfg()").

%% @doc Display environment variables (deprecated)
%% @todo delete
getcfg() ->
    Config = ui({showconfig}),
    message(?MISC:any_to_string(Config)). % @todo

setcfg_h() ->
    message("setcfg(AppDirs,IncDirs,OutDir)").

%% @doc Configure directories (deprecated)
%% @todo delete
setcfg(AppDirs,IncDirs,OutDir) ->
    Success = ui({saveconfig,AppDirs,IncDirs,OutDir}),
    message(?MISC:any_to_string(Success)). % @todo

envs_h()->
    message("envs()").

%% @doc Lists all environment nodes
envs()->
    case ?Syn:get_envs() of
        [] ->
            message("no environment variable set");
        L ->
            message2(lists:map(fun show_env/1, L))
    end.

env_h()->
    message(["env(Variable)\n"
             "env(Variable,SubKey)"]).
%% @doc Lists a specific environment node
env(Name)->
    message2(show_env({Name,?Syn:get_env(Name)})).

%#env{name=env_var, value=[{EnvName, EnvVal}]}

addenv_h()->
    message("addenv(Variable,SubKey,NewValue)").

%% @doc Adds a new subkey to a proplist environment node
addenv(Name, EnvName, EnvVal) when is_atom(EnvName)->
    addenv(Name, atom_to_list(EnvName), EnvVal);
addenv(Name, EnvName, EnvVal) when is_atom(EnvVal)->
    addenv(Name, EnvName, atom_to_list(EnvVal));
addenv(Name, EnvName, EnvVal)->
    case ?Syn:env_type(Name) of
        proplist ->
            [_|_] = ?Syn:add_env(Name, {EnvName, EnvVal}),
            ok;
        atomic ->
            message("error: type of '"++
                    ?MISC:any_to_string(Name)++"' is not a proplist")
    end.

delenv_h()->
    message(["delenv(Variable)\n"
             "delenv(Variable,SubKey)\n"
             "delenv(Variable,SubKey,OldValue)"]).

%% @doc Deletes a subkey from a proplist environment node
delenv(Name, EnvName) when is_atom(EnvName)->
    delenv(Name,atom_to_list(EnvName));
delenv(Name, EnvName)->
    case ?Syn:env_type(Name) of
        proplist ->
            case ?Syn:del_env_sub(Name,EnvName) of
                [] -> message("key not found"), fail;
                [_|_] -> ok
            end;
        atomic ->
            message("error: type of '"++
                    ?MISC:any_to_string(Name)++"' is not a proplist"),
            fail
    end.

%% @doc Deletes a specific key-value pair from a proplist environmnent node
delenv(Name, EnvName, EnvVal) when is_atom(EnvName)->
    delenv(Name, atom_to_list(EnvName), EnvVal);
delenv(Name, EnvName, EnvVal) when is_atom(EnvVal)->
    delenv(Name, EnvName, atom_to_list(EnvVal));
delenv(Name, EnvName, EnvVal)->
    case ?Syn:env_type(Name) of
        proplist ->
            case ?Syn:del_env_val(Name,{EnvName,EnvVal}) of
                [] -> message("error: key-value pair not found"), fail;
                [_|_] -> ok
            end;
        atomic ->
            message("error: type of '"++
                    ?MISC:any_to_string(Name)++"' is not a proplist"),
            fail
    end.

%% @doc Lists the value for a subkey of a proplist environment node
env(Name, EnvName) when is_atom(EnvName)->
    env(Name, atom_to_list(EnvName));
env(Name, EnvName)->
    case ?Syn:env_type(Name) of
        proplist ->
            case ?Syn:get_env(Name,EnvName) of
                L=[_|_] ->
                    message2(show_env({Name,[{EnvName,EV} || EV <- L]}));
                [] ->
                    message("error: Environment variable sub-key not found"),
                    fail
            end;
        atomic ->
            message("error: type of '"++
                    ?MISC:any_to_string(Name)++"' is not a proplist"),
            fail
    end.

%#env{name=output}
%#env{name=backup}

setenv_h()->
    message("setenv(Variable,Value)").

%% @doc Sets the value of an environment node
%% @todo setenv/3
setenv(Name, Value)->
    case ?Syn:env_type(Name) of
        atomic ->
            ?Syn:set_env(Name,Value),
            ok;
        proplist ->
            message("Please use addenv/3, delenv/2 and delenv/3 instead."),
            fail
    end.

%% @doc Deletes an environment node
delenv(Name)->
    case ?Syn:del_env(Name) of
        [] -> message("not found"), fail;
        [_|_] -> ok
    end.

show_env({_Name,[]})->
    "error: Environment variable not found\n";
show_env({Name,Values})->
    case ?Syn:env_type(Name) of
        proplist ->
            [io_lib:format("~p:~n", [Name]),
             [io_lib:format(" ~p = ~p~n",[K,V]) || {K,V} <- Values]];
        atomic ->
            [Value] = Values,
            io_lib:format("~p = ~p~n", [Name,Value])
    end.

%% @doc Returns the names of the files
%% that are described in the named environment.
%% @equiv refcore_syntax:get_env(Name)
%% @todo delete
dirs_by_env(Name) ->
    [(?Graph:data(Dir))#env.value ||
        Dir <- ?Graph:path(?Graph:root(), [{env, {name, '==', Name}}])].

%% -----------------------------------------------------------------------------

add_h() ->
    message2(["add(ModFileDirList)\n"
              "add(AppBase, App)"]).

%% @doc Adds an application to the database.
add(AppBase, App) ->
    AppBaseS = ?MISC:to_list(AppBase),
    AppS     = ?MISC:to_list(App),
    case [AB || AB <- dirs_by_env(appbase),
                {match, _} <- [re:run(AB, AppBaseS)]] of
        [Base] ->
            AppNotFound = "Application " ++ AppS ++
                " not found under " ++ AppBaseS,
            case filelib:wildcard(filename:join(Base, AppS ++ "*")) of
                [] ->
                    message(AppNotFound),
                    not_found;
                Dirs ->
                    case filelib:is_dir(filename:join(Base, AppS)) of
                        true ->
                            add_src_dir(filename:join(Base, AppS));
                        false ->
                            case Dirs == [Dir || Dir <- Dirs, {match, _}
                             <- [re:run(Dir, AppS ++ "[a-zA-Z]*-[0-9.]*")]] of
                                true -> %@todo format
                                    add_src_dir(lists:last(Dirs));
                                false ->
                                    message(AppNotFound),
                                    io:format("Applications under :~n" ++ AppBaseS),
                                    [io:format("    ~s~n", [Dir]) || Dir <- Dirs],
                                    not_found
                            end
                    end
            end;
        [] -> message("Application base " ++ AppBaseS ++ " not found"),
            missing_appbase
    end.

add_src_dir(Dir) ->
    SrcDir = filename:join(Dir, "src"),
    message("Adding: " ++ SrcDir),
    add(SrcDir).


%% @doc Add either a module, a file, a directory or a list of these
add(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun add_/1,L);
add(X) ->
    catch_referr(
      fun()->
              case add_(X) of
                  error -> error;
                  _     -> ok
              end
      end).

add_(Mod) when is_atom(Mod) ->
    try
        add_(mod2file(Mod,true))
    catch
        ?RefError(mod_not_found,[Mod]) ->
            N = atom_to_list(Mod),
            add_if_exists(N++".erl") orelse
            add_if_exists(N++".beam") orelse
            begin
                message("Error: no matching .erl or .beam"),
                error
            end
    end;
add_(File=[C|_]) when is_integer(C) ->
    case io_lib:deep_char_list(File) of
        true ->
            add_file(File);
        false ->
            message("Error: bad file argument given"),
            error
    end;
add_(_)->
    message("::(modfile() | [modfile()])"),
    usage.

add_if_exists(F)->
    filelib:is_regular(F) andalso
        error/=add_(F).

print_files(error) ->
%print_files({error,{_,E}}) ->
%    message(E),
    error;
print_files({ok,Files}) ->
    message(?MISC:any_to_string(Files)). % @todo

%% @private
add_file(File) ->
%    Added = ui({add_dir,File}),
%    print_files(Added).
    ui({add_dir,File}).

%ri:add([a,b]).
%ri:add("a.erl").

drop_h() ->
    message("drop(ModFileDirList)").

%% @doc Drop either a module, a file, a directory or a list of these
drop(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun drop/1,L);
drop(Mod) when is_atom(Mod) ->
    catch_referr(fun()->
        drop(mod2file(Mod,true))
    end);
drop(File) when is_list(File) ->
    drop_file(File);
drop(_)->
    message("::(modfile() | [modfile()])"),
    usage.

%% @private
drop_file(File) ->
    Dropped = ui({drop,File}),
    print_files(Dropped).
%ri:drop(a).

save_h() ->
    message2(["save(ModFileDirList)\n"]).

%% @doc Saves a file (debug)
%% @todo Is this still needed?
save(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun save/1,L);
save(Mod) when is_atom(Mod) ->
    catch_referr(fun()->
        save(mod2file(Mod,true))
    end);
save(File) when is_list(File) ->
%    recurse_erl(File, fun save_file/1).
%save_file(File) ->
    catch_referrsave(fun()->
        ?FileMan:save_file(get_filenode(File))
    end).


lsl_h() ->
    message2(["lsl()\n"]). %@todo

lsl() ->
    print_files(ui({status_info,[]})).

ls_h() ->
    message2(["ls()\n"
              "ls(ModFile)\n" ]).

%% @doc Lists modules contained in the database
ls() ->
    Files = ui({filelist}),
    print_files(Files).

%% @doc Lists includes, records, macros and functions in a file or moule
ls(ModFile) when
        (is_atom(ModFile) or is_list(ModFile))->
    catch_referr(fun() -> ls_(ModFile) end);
ls(_)->
    message("::(modfile())"),
    usage.

ls_(ModFile) ->
    FN  = get_filenode(ModFile),
    GetFunName = fun(F)-> ?MISC:fun_text([?Fun:name(F),?Fun:arity(F)]) end,
    GetRecName = fun(R)-> atom_to_list(?Rec:name(R)) end,
    Gets = [{"includes",?File:includes(),fun ?File:path/1},
            {"records", ?File:records(), GetRecName},
            {"macros",  ?File:macros(),  fun ?Macro:name/1},
            {"funcs",   ?Query:seq([[form],[fundef]]), GetFunName}],
    Text = [ begin Res = ?Query:exec(FN,Q) -- [FN],
                   case Res of
                       "" -> "";
                       _  -> S ++ ":" ++ [ " " ++ D(E) || E <- Res ] ++ "\n"
                   end
             end
           || {S,Q,D} <- Gets ],
    message2(Text).

undo_h() ->
    message("undo()").

%% @doc Undoes the previous refactoring
undo() ->
    Success = ui({undo,[]}),
    message(?MISC:any_to_string(Success)). % @todo.

graph_h() ->
    message2([
        "graph()\n"
        "graph(TargetNameFile)\n"
        "graph(TargetFile,FilterList)\n" ]).

%% @doc Draws a graph with default name from database contents (debug)
graph() ->
    graph(?DEF_GRAPH_NAME).

%% @doc Draws a graph from database contents, saves result in `Target' (debug)
graph(Target) ->
    graph(Target,[]).

%% @doc Draws a graph from filtered database contents (debug)
graph(Target, Filter) when is_atom(Target) ->
    graph(atom_to_list(Target)++".dot",Filter);

graph(Target, Filter) ->
    Success = ui({draw, Target, Filter}),
    message(?MISC:any_to_string(Success)). % @todo

svg_h() ->
    message2([
        "svg()\n"
        "svg(TargetNameFile)\n"
        "svg(Target,Options)\n" ]).

%% @doc Draws a graph converted to SVG with default name (debug)
svg() ->
    svg(?DEF_GRAPH_NAME).

%% @doc Draws a graph converted to SVG from database contents (debug)
svg(Target) ->
    svg(Target, []).

%% @doc Draws a graph converted to SVG from filtered database contents (debug)
svg(Target, Opts) when is_atom(Target) ->
    svg(atom_to_list(Target)++".svg",Opts);
svg(Target, Opts) when is_list(Target) ->
    DotName = filename:rootname(Target) ++ ".dot",
    io:format("Making ~s...~n", [DotName]),
    _Success1 = ui({draw,DotName, Opts}), %%TODO: tooltip
    io:format("Calling Graphviz...~n"),
    Res = os:cmd("dot -Tsvg "++DotName++" -o"++Target),
    case Res of
        [] -> ok;
        _  -> {error,Res}
    end;
svg(_, _)->
    message("::(atom() | string(), proplist())"),
    usage.

gn_h() ->
    message("gn(TypeAtom,Index)").

%% @doc Prints out data of a graph node (debug)
gn(TypeAtom,Index) when is_atom(TypeAtom), is_integer(Index) ->
    message(io_lib:print(?Graph:data({'$gn',TypeAtom,Index})));
gn(_,_)->
    message("::(atom(), integer())"),
    usage.

stop_h()->
    message("stop()").

%% @doc Stops the RefactorErl server.
stop()->
    Success = ui({stop}),
    message(?MISC:any_to_string(Success)). % @todo

reset_h() ->
    message("reset()").

%% @doc Clears database contents and resets its schema
reset()  ->
    Success = ui({reset}),
    message(?MISC:any_to_string(Success)). % @todo

%%% ============================================================================
%%% Regexp helpers

%% @doc gets a posrange in a file from a regexp
to_posrange(File,Range) ->
    rgxpos(File,Range).

%% @doc gets a position in a file from a regexp
to_pos(_,Pos) when is_integer(Pos) ->
    Pos;
to_pos(File,Pos) ->
    {P,_}=rgxpos(File,Pos),
    P.

%% @doc gets a position or a posrange in a file from a regexp
%% @private
%% @todo :wishlist improve rgxpos/3 to be context sensitive and indexless
%% @todo :wishlist check Rgx
rgxpos(File,Pos) ->
    case ?Query:exec(?File:find(File)) of
        [FileNode] -> Text = lists:flatten(?Syn:tree_text(FileNode)),
                      rgxpos_text(Text,Pos);
        _          -> throw(?RefError(file_not_present,[File]))
    end.

%% @doc gets a position or a posrange in text from a regexp
rgxpos_text(_,{Beg,End}) when is_integer(Beg), is_integer(End) ->
    {Beg,End};
rgxpos_text(Text,Rgx=[C|_]) when is_integer(C) ->
    rgxpos_text(Text,{Rgx,1});
rgxpos_text(Text,{Rgx=[C|_],Idx}) when is_integer(Idx), is_integer(C) ->
    case getparen(Rgx) of
        invalid ->
            throw(?LocalError(invalid_regexp,[Rgx]));
        {{P,Q},RRgx} ->
            Matches  =
                case tryrgx(matches,[Text,RRgx]) of
                    [] -> throw(?LocalError(unmatched_regexp,[Rgx]));
                    M  -> M
                end,
            {Beg,Len} =
              try
                lists:nth(Idx,Matches)
              catch
                error:function_clause ->
                    throw(?LocalError(unbound_idx_regexp,[Rgx,Idx]))
              end,
            Match = lists:sublist(Text,Beg,Len),
            Pre = [$^] ++ lists:sublist(RRgx,1,Q),
            Suf = lists:nthtail(P-1,RRgx) ++ [$$],
            Tails = length(tryrgx(sub,[Match,Pre,""])),
            Inits = length(tryrgx(sub,[Match,Suf,""])),
            {Beg+Inits, Beg+Len-1-Tails}
%%@todo !!! better separation!!!
    end;
rgxpos_text(_,Pos) -> throw(?LocalError(invalid_pos,[Pos])).

tryrgx(RegFun,Params=[_,RRgx|_]) ->
    case apply(regexp,RegFun,Params) of
        {match,M} -> M;
        {ok,R,1}  -> R;
        _         -> throw(?LocalError(invalid_regexp,[RRgx]))
    end.

%% @private
%% @doc Gets the location of escaped parenthesis in text
%% Starts up the state machine for getparen/4.
getparen(L=[_|_]) ->
    getparen([],1,[],L).

%% @doc Gets the location of escaped parenthesis in text
%%
%% Example invocations: <ol>
%%  <li>getparen("abc\\tdef")   = {{1,8},"abc\\tdef"}</li>
%%  <li>getparen("abc&lt;de&gt;f")    = {{4,5},"abcdef"}</li>
%%  <li>getparen("a&lt;b\\&lt;cd&lt;ef") = {{2,5},"ab&lt;cdef"}</li>
%%  <li>getparen("a&lt;b\ncd&lt;ef")  = {{2,5},"ab\ncdef"}</li>
%%  <li>getparen("a&lt;bcdef&lt;")    = {{2,6},"abcdef"}</li>
%%  <li>getparen("ab&lt;cdef")     = {{3,6},"abcdef"}</li>
%%  <li>getparen("abcde&lt;f")     = {{1,5},"abcdef"}</li>
%%  <li>getparen("ab&lt;c&lt;d&lt;")     = invalid</li>
%%  </ol>
%%
getparen([], I,R,[$<|T]) ->
    getparen([I],I,R,T);
getparen([],I,R,[$>|T]) ->
    getparen([1,I-1],I,R,T);
getparen([P],I,R,[$>|T]) ->
    getparen([P,I-1],I,R,T);
getparen(S,  I,R,[$\\,C |T]) when (C==$<) or (C==$>) ->
    getparen(S,I+1,R++[C],T);
getparen(S,  I,R,[$\\,C |T]) when is_integer(C) ->
    getparen(S,I+2,R++[$\\,C],T);
getparen(S,  I,R,[C|T])      when is_integer(C), C/=$\\, C/=$<, C/=$> ->
    getparen(S,I+1,R++[C],T);
getparen([], I,R,[])-> {{1,I-1},R};
getparen([P],I,R,[])-> {{P,I-1},R};
getparen([P,Q],_,R,[])-> {{P,Q},R};
getparen(_,_,_,_)-> invalid.

%%% ============================================================================
%%% Other helpers

%% @doc transformation helper for source+dest refactorings
transform2(Ref, Source, Dest, Args) ->
    catch_referr(fun()->
        DestTup = case Ref of
            move_fun ->
                {name,    file2modc(Dest)};
            A when (A==move_rec) or (A==move_mac) ->
                {filename,mod2filec(Dest)}
        end,
        transform_inject(Ref, Source, fun(A)->A end, [DestTup|Args])
     end).

%% @doc transformation helper for refactorings needing a position
transformp(Ref,Source,Pos,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{position,to_pos(File,Pos)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

%% @doc transformation helper for refactorings needing a posrange
transformr(Ref,Source,Range,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{posrange,to_posrange(File,Range)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

%% @private
%% @doc transformation helper that prints exceptions
transform_catch(Ref, Source, Args) ->
    catch_referr(fun()->
        transform_inject(Ref, Source, fun(X)->X end, Args)
    end).

%% @private
%% @doc transformation helper that adds both file and module information
transform_inject(Ref, Source, Fun, Arg) when is_function(Fun,1) ->
    Args = [{file,  mod2filec(Source)},
            {module,file2modc(Source)}|Arg],
    transform(Ref, Fun(Args)).

%% @private
catch_referrsave(Fun) when is_function(Fun,0) ->
     catch_referr(fun() ->
         catch_save(Fun)
     end).

%% @private
%% @doc prints out refactorerl exceptions as textual errors
catch_referr(Fun) when is_function(Fun,0) ->
    try
        Fun() %{ok,}
    catch
        E={M,_,_} when is_atom(M) -> %(M==?MODULE) or (M==?Error) ->
            message(?Error:error_text(E)),
            error; %{error,E}
        Error -> ?d(Error)
    end.

%% @private
%% @doc prints out save_file exceptions as textual errors
catch_save(Fun) when is_function(Fun,0) ->
    R = try
            Fun()
        catch
            unsafe_save -> {error,"unsafe save denied"}
        end,
    case R of
        {error,Error} ->
            message(Error);
        ok -> ok
    end.

transform_h()->
    message("transform(Refac,Args)").

%% @doc transformation helper that catches messages synchronously
%% exported for debugging purposes
transform(Refac, Args) when is_atom(Refac), is_list(Args) ->
    transform_(Refac, Args, fun(_)->success end).

%% @doc transformation helper that catches messages synchronously
transform_(Refac, Args0, Fun) when is_function(Fun,1),
  is_atom(Refac), is_list(Args0) ->
%    io:format("DEBUG: ~p:~p~n",[Refac,Args]),
    Args = [{ask_missing,true} | Args0],
    case ui({transform, Refac, Args}) of
        {ok,Result}->
            case Result of
                {result,R} -> Fun(R);
                {abort, {_,A}} ->
                    message(A),
                    deny;
                Error -> ?d(Error)
            end;
        {error, {_,E}} ->
            message("Fatal: " ++ E),
            error;
        Error -> ?d(Error)
    end.

%% @doc handle synchronous UI/transform calls
trap(Pre, Action, Done)
  when is_function(Pre,0), is_function(Action,1),
       is_function(Done,1) ->

    State  = Pre(),
    Answer =
        try
            Action(State)
        catch
            E ->
                Done(E),
                throw(E);
              error:E ->
                Done({error,E}),
                erlang:error(E)
        end,
   Done(Answer).

%%% ----------------------------------------------------------------------------
%%% File and module related helpers

%% @private
%% @doc looks up the module name of a loaded and error-free file
file2modc(Source) ->
    _ = guard_ready(Source),
    file2mod(Source).

%% @private
%% @doc looks up the module name of a loaded file
file2mod([]) ->
    throw(?LocalError(no_file,[]));
file2mod(Mod) when is_atom(Mod) ->
    Mod;
file2mod(Fil) when is_list(Fil) ->
    File = filename:absname(Fil),
    FileNode = ?Query:exec1(?File:find(File),
                            ?RefError(file_not_present, [File])),
    ModNode  = ?Query:exec1([FileNode], ?File:module(),
                            ?RefError(file_not_module,  [File])),
    ?Mod:name(ModNode).

%% @private
%% @doc looks up the filename for a loaded and error-free module
mod2filec(Source) ->
    guard_ready(mod2file(Source)).

%% @private
%% @doc looks up the filename for a loaded module
mod2file([],_Amb) ->
    throw(?LocalError(no_file,[]));
mod2file(File,_Amb) when is_list(File) ->
    filename:absname(File);
mod2file(Mod,Amb) when is_atom(Mod), is_boolean(Amb) ->
    FileNodes =
        ?Query:exec(?Query:seq([
            ?Mod:find(Mod),
            ?Mod:file() ])),
    case FileNodes of
        [File] ->
            ?File:path(File);
        [] ->
            throw(?RefError(mod_not_found, [Mod]));
        [_|_] ->
            case Amb of
                false ->
                    throw(?RefError(ambiguous_mod, [Mod]));
                true ->
                    [?File:path(File) || File <- FileNodes]
            end;
        X      -> throw(?LocalError(internal_unexpected, [X]))
    end.

%% @private
%% @doc returns only a single filename, otherwise throws ambiguous_mod
mod2file(X) -> mod2file(X,false).

%% @private
%% @doc ensures that the given file is error-free
guard_ready([])   ->
    throw(?LocalError(no_file,[]));
guard_ready(File) ->
    case state(File) of
        ok      -> File;
        invalid -> throw(?LocalError(not_ready,[File]));
        _       -> throw(?RefError(file_not_present, [File]))
    end.

%% @private
%% @doc gets the status of a file or module
state(Source) ->
    trap(
      fun() ->
              mod2file(Source)
      end,
      fun(F) ->
              {ok,[{F,Prop}]} = ui({status,F}),
              case ?MISC:pget([present,error],Prop) of
                  [[true],[]]    ->
                      ok;
                  [[false],[]] ->
                      missing;
                  [[true],[true]] ->
                      invalid
              end
      end,
      fun(X) -> X end).

%%% ----------------------------------------------------------------------------
%%% Elementary helpers

build_h()->
    message("build()").

%% @doc Builds RefactorErl.
build() ->
    referl_gen_build:run(tool_dir(), tool, [debug_info]).

%% @private
%% @doc Returns the `tool' directory of RefactorErl.
tool_dir() ->
    filename:join([code:lib_dir(referl_core), "..", ".."]).

%% @spec errors() -> ok | 'errors!'
%% @doc Prints a list of functions that have cross reference problems:
%%      unused, deprecated, or most importantly, undefined functions.
%%      Ideally, this function should print nothing, and return `ok'.
errors() ->
    errors(all).

%% @spec errors(all | loaded) -> ok | 'errors!'
%% @doc  A helper for `errors/0',
%%       this function can print the errors of all known modules (`all'),
%%       or only those that are loaded (`loaded').
errors(Opt) ->
    {ok, [{script, _, Actions}]} =
        file:consult(filename:join(tool_dir(), "refactorerl.script")),
    Mods = [Mod ||  {apply, {application, load,
                             [{application, _App, Opts}]}} <- Actions,
                    {modules, Mods} <- Opts,
                    Mod <- Mods],
%    io:format("Checking modules:~n    ~p~n", [Mods]),
    Problems = [{St, Funs} ||   Mod <- Mods,
                                lists:prefix("ref", atom_to_list(Mod)),
                                {St, Funs} <- xref:m(Mod),
                                Funs =/= []],
    P1 = print_problems(unused, "Unused functions:", Problems, Mods, Opt),
    P2 = print_problems(deprecated, "Deprecated functions:", Problems, Mods, Opt),
    P3 = print_problems(undefined, "Missing functions:", Problems, Mods, Opt),
    case {P1, P2, P3} of
        {ok, ok, ok} -> ok;
        _            -> 'errors!'
    end.

print_problems(ProblemAtom, Descr, Problems, Mods, Opt) ->
    case lists:usort(lists:concat([Funs ||  {A, Funs} <- Problems,
                                            A =:= ProblemAtom])) of
        [] ->
            ok;
        ProblemFuns ->
            io:format("~s~n", [Descr]),
            [io:format("  ~p:~p/~p~n", [M,F,A]) || {M,F,A} <- ProblemFuns],
            PrCalled = [{M,F,A} || {_, {M,F,A}} <- ProblemFuns],
            [begin
                io:format("  ~p:~p/~p~n", [PM,PF,PA]),
                [io:format("      <- ~p:~p/~p~n", [M,F,A]) ||
                    {{M,F,A},{M2,F2,A2}} <- ProblemFuns,
                    {M2,F2,A2} =:= {PM,PF,PA},
                    Opt =:= all orelse lists:member(M, Mods)]
             end || {PM,PF,PA} <- lists:usort(PrCalled),
                    Opt =:= all orelse lists:member(PM, Mods)],
            errors
    end.


%% @doc Runs the unit tests.
test() ->
    reftest_refact:run().

%% @doc Runs all test cases.
%% @todo Run unit tests and other regression indicators
testall() ->
    {reftest_lib:run(),
     reftest_refact:run()}.

%% @doc Runs the test cases of a transformation.
%% The name of the transformation can be abbreviated up to ambiguity.
%% @todo Run on a given entity
test(Params) when is_list(Params) ->
    reftest_refact:run(Params);
test(Mod) when is_atom(Mod) ->
    {ok, TestDirFiles} = file:list_dir(filename:join(["..", "test"])),
    ModList = atom_to_list(Mod),
    case [F || F <- lists:usort(TestDirFiles), lists:prefix(ModList, F)] of
        [FileName|_] -> reftest_refact:run([{test, [list_to_atom(FileName)]}]);
        []           -> list_files(TestDirFiles);
        FileNames    -> list_files(FileNames)
    end;
test(_)->
    message("::(atom() | proplist())"),
    usage.

%% @doc Runs a specific test case of a transformation.
%% If the name of the test case is numeric, it can be given as an integer.
%% The name of the transformation can be abbreviated up to ambiguity.
test(Mod, Case) when is_atom(Mod), is_integer(Case) ->
    CaseList = integer_to_list(Case),
    CaseAtom =
        if
            Case < 10 -> list_to_atom([$0|CaseList]);
            true      -> list_to_atom(CaseList)
        end,
    test(Mod, CaseAtom);
test(Mod, Case) when is_atom(Mod), is_atom(Case) ->
    {ok, TestDirFiles} = file:list_dir(filename:join(["..", "test"])),
    ModList = atom_to_list(Mod),
    CaseList = atom_to_list(Case),
    case [F || F <- lists:usort(TestDirFiles), lists:prefix(ModList, F)] of
        [ModName|_] ->
            {ok, TestDir2} = file:list_dir(filename:join(["..", "test", ModName])),
            case lists:member(CaseList, TestDir2) of
                false ->
                    list_files(TestDir2);
                true ->
                    reftest_refact:run([{test, [{list_to_atom(ModName), Case}]}])
            end;
        []         -> list_files(TestDirFiles);
        FileNames  -> list_files(FileNames)
    end;
test(_,_)->
    message("::(atom(), atom() | integer())"),
    usage.


%% @doc Lists the files of a directory.
list_files(AllFiles) ->
    io:format("Possible parameters:~n"),
    Files = lists:usort(AllFiles) -- [".svn", "DESC"],
    [io:format("    ~s~n", [File]) || File <- Files],
    missing_dir.


%% @doc Returns the list of differences after files have been loaded.
%%      This function does NOT consider forms with load errors.
%% @todo Parallelising this operation does not seem to bring any speedup.
cat_errors() ->
    % todo [file] should have an interface function
    Files = [File || File <- ?Query:exec([file]),
                     not has_errors(File)],
    io:format("~B files ", [length(Files)]),
    Time1 = now(),
    Results = pmap(fun cat_errors_with_display/1, Files),
%    Results = lists:map(fun cat_errors_with_display/1, Files),
    Time2 = now(),
    {H,M,S} = calendar:seconds_to_time(timer:now_diff(Time2, Time1) div 1000000),
    io:format("~nchecked in ~B hours, ~B minutes, ~B seconds~n", [H,M,S]),
    case [Err || Err <- Results, Err =/= ok] of
        []     -> no_cat_errors;
        Errors -> Errors
    end.

has_errors(File) ->
    Types = [(?ESG:data(Form))#form.type || Form <- ?Query:exec(File, ?File:real_forms())],
    lists:member(error, Types).


%% @doc Returns the list of differences between the original and the database
%% version of a file.
cat_errors(File) ->
    ?FileMan:create_scanner(),
    #file{path=Path, eol=Eol} = ?ESG:data(File),
    {Text, _EOL} = ?FileMan:file_text(Path),
    FileHashWForms = [{Hash, tokens_to_text(Tokens, Eol)}
                        || {Hash, Tokens} <- ?FileMan:tokenize(Text)],
    GForms = ?Query:exec(File, ?File:real_forms()),
    case length(GForms) == length(FileHashWForms) of
        false -> not_matching_form_count;
        true ->
            MatchingForms =
                [{{GForm, (?ESG:data(GForm))#form.hash}, {{FHash, FText}, Idx}}
                 || {GForm, {{FHash, FText}, Idx}} <-
                  lists:zip(GForms, ?MISC:index_list(FileHashWForms))],
            case [{Idx, GForm, FText}
                    ||  {{GForm, Hash}, {{FHash, FText}, Idx}} <- MatchingForms,
                        Hash =/= FHash] of
                BadForms = [_|_] ->
                    {changed_forms, BadForms};
                [] ->
                    case [{Idx, GForm, flat_text(GForm), FText}
%                    case [{Idx, GForm, remove_ws(flat_text(GForm)), remove_ws(FText)}
                             || {{GForm, _Hash}, {{_FHash, FText}, Idx}} <- MatchingForms,
                                not is_similar(flat_text(GForm), FText)] of
                        [] ->
                            ok;
                        BadForms ->
                            {not_matching_forms, File, BadForms}
                    end
            end
    end.


is_similar(invalid_children, _) ->
    false;
is_similar(_, invalid_children) ->
    false;
is_similar(Txt1, Txt2) ->
    Txt1 == Txt2.
%    remove_ws(Txt1) == remove_ws(Txt2).

% remove_ws(invalid_children) ->
%     invalid_children;
% remove_ws(Txt) ->
%     lists:reverse(remove_prefix_ws(lists:reverse(remove_prefix_ws(Txt)))).
%
% remove_prefix_ws(invalid_children) ->
%     invalid_children;
% remove_prefix_ws(Txt) ->
%     {_WS, Rest} = lists:splitwith(fun(Ch) -> lists:member(Ch, " \t\n") end, Txt),
%     Rest.


tokens_to_text(Tokens, Eol) ->
    ?FileMan:orig_text(Eol, lists:flatten([Pre ++ Text ++ Post ||
     #token{prews=Pre, text=Text, postws=Post} <- Tokens])).

%% @todo Move to ?Syn.
flat_text(Form) ->
    try
        lists:flatten(?Syn:tree_text(Form))
    catch
        throw:{invalid_children, _, _} -> invalid_children
    end.




cat_errors_with_display(File) ->
    Ret = cat_errors(File),
    case Ret of
        ok ->
            io:put_chars("o");
        _ ->
            io:put_chars("X")
    end,
    Ret.


pmap(Fun, Xs) ->
    S = self(),
    [spawn(fun() -> pmap_f(S, Fun, X) end) || X <- Xs],
    pmap_gather(length(Xs)).

pmap_gather(0) ->
    [];
pmap_gather(N) ->
    receive
        {pmap, _, Ret} ->
            [Ret|pmap_gather(N-1)]
    end.

pmap_f(Parent, Fun, X) ->
    Parent ! {pmap, self(), (catch Fun(X))}.

%% -----------------------------------------------------------------------------

modsave(Mod) ->
    refcore_store_graph:save(Mod).

modload(Mod) ->
    refcore_store_graph:load(Mod).

%% -----------------------------------------------------------------------------

%% @doc Start yaws with default configuration.
start_yaws() ->
    catch_referr(fun() -> web_helper:start_yaws() end).

%% @doc Configure and start yaws web server.
start_yaws(Proplist) ->
    catch_referr(fun() -> web_helper:start_yaws(Proplist) end).

%% @doc Stop yaws web server.
stop_yaws() ->
    web_helper:stop_yaws().


%% -----------------------------------------------------------------------------

%% @spec draw_dep(Level, Par) -> any()
%%           Level = mod | func
%%           Par =  node() | Name | string()
%%           Name = Module | Function
%%           Module = atom()
%%           Function = string()
%% @doc Draws out the dependency graph (in either module or function
%% level) into a dot file. A function can be specified (as
%% "Module:Function/Arity" or as a graph node) to only look at cycles
%% touching that fun. `Par' also can be `all' for drawing the whole
%% graph or `cycles' for only drawing the cycles.
draw_dep(mod,  Par) -> draw_dep_mod(Par);
draw_dep(func, Par) -> draw_dep_fun(Par);
draw_dep(_,      _) -> message("Error: unknown dependency level given.").

%% @spec print_dep(Level, Par) -> any()
%%           Level = mod | func
%%           Par =  node() | Name | string()
%%           Name = Module | Function
%%           Module = atom()
%%           Function = string()
%% @doc Prints out the dependency graph in either module or function
%% level. A function can be specified (as "Module:Function/Arity" or
%% as a graph node) to only look at cycles touching that fun. `Par'
%% also can be `all' for printing the whole graph or `cycles' for only
%% printing the cycles.
print_dep(mod,  Par) -> print_dep_mod(Par);
print_dep(func, Par) -> print_dep_fun(Par);
print_dep(_,      _) -> message("Error: unknown dependency level given.").

draw_dep_fun("all")    -> refusr_cyclic_fun:draw();
draw_dep_fun("cycles") -> refusr_cyclic_fun:draw("cycle");
draw_dep_fun(Node)     -> refusr_cyclic_fun:draw(Node).

draw_dep_mod("all")    -> refusr_cyclic_mod:draw();
draw_dep_mod("cycles") -> refusr_cyclic_mod:draw("cycle");
draw_dep_mod(Node)     -> refusr_cyclic_mod:draw(Node).

print_dep_fun("all")    -> refusr_cyclic_fun:check_cycle();
print_dep_fun("cycles") -> refusr_cyclic_fun:print_cycle();
print_dep_fun(Node)     -> refusr_cyclic_fun:check_function(Node).

print_dep_mod("all")    -> refusr_cyclic_mod:check_cycle();
print_dep_mod("cycles") -> refusr_cyclic_mod:print_cycle();
print_dep_mod(Node)     -> refusr_cyclic_mod:check_module(Node).
