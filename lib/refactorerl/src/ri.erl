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

%%% @doc This module implements a console user interface for the tool

%%% TODO: help
%%% TODO: asynchronous handling option
%%% TODO:wishlist supplement filelist output with status
%%% TODO:wishlist better parameter checking (Name, Pos, ...)!
%%% TODO:wishlist remove guards from exported functions
%%%       and signal errors verbosely
%%% TODO: fill this place
%%% TODO: basic commenting
%%% TODO:wishlist verbose commenting
%%% TODO:wishlist add edocs to every function
%%% TODO:wishlist research alternatives like
%%%       Beg,End -> line.col:line.col; line.col:col; line:regex ;
%%% TODO:wishlist function scope regexps
%%% TODO: better regexp mark handling
%%%
%%% == New heading ==
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(ri).
-vsn("$Rev: 0000 $").
-include("refactorerl.hrl").

-export([h/0]).
-export([add/1,   drop/1, ls/0,    reset/0,
         backup/0,undo/0, clean/0, graph/1, graph/2 ]).

-export([elimvar/2,extfun/3, expfun/2, genfun/3,
         inlfun/2, inlmac/2, intrec/4, intrec/5,
         intimp/1,
         merge/3,  movfun/3, movrec/3, movmac/3,
         reorder/3,reorder/4,renfld/4, renfun/3,
         renfun/4, renhrl/2, renrec/3, renmac/3,
         renmod/2, renvar/3, tupfun/2, upregex/0
         
 ]).
-export([error_text/2]).
-export([rgxpos_text/2]). %%DEBUG

-define(TO,9000). %timout in ms for Ui messages
-define(TRAN_WT,100). %delay in ms between transformation and wait

message(Data) ->
    io:format("~s~n", [Data]).

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
error_text(ambiguous_mod,[Mod]) ->
    ["Multiple files loaded with the module name ",
     io_lib:print_atom(Mod)];
error_text(internal_unexpected,[X]) ->
    ["Unexpected return value ",
     io_lib:print(X)];

error_text(_,_) -> unknown.


h() -> message( [
    "Consult the following example use cases:\n",
    "ri:elimvar(a,\"X=\").\n",
    "ri:extfun(a,\"A+B\",f).\n",
    "ri:expfun(a, \"fun g\").\n",
    "ri:genfun(a, \"[+]\\(2\", \"Two\").\n",
    "ri:inlfun(a,\"f[(]1\").\n",
    "ri:inlmac(a,\"?Mac\").\n",
    "ri:intrec(a, \"{X,Y}\", newrec, [f1, f2]).\n",
    "ri:merge(a,\"1+2\",\"V\").\n",
    "ri:movfun(a,b,[{f,2},{g,0}]).\n",
    "ri:movrec(a,b,[rec1,rec2]).\n",
    "ri:movmac(a,b,[\"Mac1\",\"Mac2\"]).\n",
    "ri:reorder(a,{f,2},[2,1]).\n",
    "ri:renfld(a,rec,oldfield1,newfield1).\n",
    "ri:renfun(a,{f,2},new).\n",
    "ri:renrec(a,oldrec,newrec).\n",
    "ri:renmac(a,\"OldMac\",\"NewMac\").\n",
    "ri:renmod(a, newmod).\n",
    "ri:renhrl(\"a.hrl\", \"b.hrl\").\n",
    "ri:renvar(a, \"X=\", \"V\").\n",
    "ri:tupfun(a,\"A,B\").\n",
    "ri:upregex().\n",
    "\n",
    "Note that you can substitute regexp in place ",
    "of positions and posranges like so:\n",
    "* a plain regexp matches precisely a posrange or ",
    "marks the start of a position;\n",
    "* use escaped parenthesis \\( and \\) to highlight",
    " part of a regexp;\n",
    "* the nth match can be selected instead of the first one ",
    "by substituting the tuple {\"regexp\",index1} for \"regexp\";\n",
    "* escape plain parenthesis by a choice, ie. [(] or [)]."
    ]).

%% ===============
%% Refactorings:
%%

extfun(File, Range, Name) ->
    transformr(extract_fun, File, Range, [{name, Name}]).
% ri:extfun(a,"A+B",f).

merge(File, Range, Varname=[C|_]) when is_integer(C) ->
    transformr(merge, File, Range, [{varname, Varname}]).
% ri:merge(a,"1+2","V").

inlfun(File, Pos) ->
    transformp(inline_fun, File, Pos, []).
% ri:inlfun(a,"f[(]1").

inlmac(File, Pos) ->
    transformp(inline_mac, File, Pos, []).
% ri:inlmac(a,"?Mac").

tupfun(File, Range) ->
    transformr(tuple_funpar, File, Range, []).
%%TODO:wishlist Begin,End -> name/arity first last
% ri:tupfun(a,"A,B").

%%Not recommended
reorder(File, Fun, Arity, Order) ->
    reorder(File, {Fun,Arity}, Order).

%TODO:wishlist check Order
reorder(File, {Fun,Arity}, Order=[I,_])
      when is_atom(Fun), is_integer(Arity), is_integer(I) ->
    transform(reorder_funpar, File,
              [{function, Fun}, {arity, Arity}, {order, Order}]).
% ri:reorder(a,{f,2},[2,1]).

expfun(File, Pos) ->
    transformp(expand_funexpr, File, Pos, []).
% ri:expfun(a, "fun g").

movfun(Source, Target, Fun={A,I})
      when is_atom(A), is_integer(I) ->
    movfun(Source, Target, [Fun]);
% ri:movfun(a,b,{f,2}).

%%TODO:wishlist check Funlist
movfun(Source, Target, Funlist=[{A,I}|_])
      when is_atom(A), is_integer(I) ->
    transform2(move_fun, Source, Target, [{funlist, Funlist}]).
% ri:movfun(a,b,[{f,2},{g,0}]).

movrec(Source, Target, Rec) when is_atom(Rec) ->
    movrec(Source, Target, [Rec]);
% ri:movrec(a,b,rec).

%%TODO:wishlist check Reclist
movrec(Source, Target, Reclist=[A|_]) when is_atom(A) ->
    transform2(move_rec, Source, Target, [{reclist, Reclist}]).
% ri:movrec(a,b,[r1,r2]).

movmac(Source, Target, Mac=[C|_]) when is_integer(C) ->
    movmac(Source, Target, [Mac]);
% ri:movmac(a,b,"Mac").

%%TODO:wishlist check Maclist
movmac(Source, Target, Maclist=[[C|_]|_]) when is_integer(C) ->
    transform2(move_mac, Source, Target, [{maclist, Maclist}]).
% ri:movmac(a,b,["Mac1","Mac2"]).

genfun(File, Range, Newname) ->
    transformr(gen, File, Range, [{varname, Newname}]).
% ri:genfun(a, "[+]\\(2", "Two").

%%Not recommended
renfun(File, Fun, Arity, Newname) ->
    renfun(File, {Fun,Arity}, Newname).

renfun(File, {Fun,Arity}, Newname)
      when is_atom(Fun), is_integer(Arity) ->
    transform(rename_fun, File,
              [{function, Fun}, {arity, Arity}, {name, Newname}]).
% ri:renfun(a,{f,2},new).

renvar(File, Pos, Newname=[_|_]) ->
    transformp(rename_var, File, Pos, [{varname, Newname}]).
% ri:renvar(a, "X=", "V").

renmod(File, Newname) ->
    transform(rename_mod, File, [{name, Newname}]).
% ri:renmod(a, newmod).

renhrl(File, Newname) ->
    transform(rename_header, File, [{name, Newname}]).
% ri:renhrl("a.hrl", "b.hrl").

renrec(File,Record,NewName)
      when is_atom(Record) ->
    transform(rename_rec, File,
              [{record, Record}, {name, NewName}]).
% ri:renrec(a,recname,newrecname).

renfld(File,Record,Field,NewName)
      when is_atom(Record), is_atom(Field) ->
    transform(rename_recfield, File,
              [{record,Record}, {recfield,Field}, {name,NewName}]).
% ri:renfld(a,recname,field1,newfield1).

elimvar(File, Pos) ->
    transformp(elim_var, File, Pos, []).
% ri:elimvar(a,"X=").

%%Not recommended
intrec(File, A, B, Newname, Fields) ->
    intrec(File, {A,B}, Newname, Fields).

intrec(File, URange, Newname, AFields=[A|_]) when is_atom(A) ->
    SList = lists:map(fun atom_to_list/1, AFields),
    Fields = string:join(SList," "),
    intrec(File, URange, Newname, Fields);
% ri:intrec(a, "{X,Y}", newrec, [f1, f2]).

intrec(File, Range, Newname, Fields=[C|_]) when is_integer(C) ->
    transformr(introduce_rec, File, Range,
               [{name,Newname}, {text,Fields}]).
% ri:intrec(a, "{X,Y}", rec, "f1 f2"]).

renmac(File, Macro, Newname) ->
    transform(rename_mac, File, [{macro,Macro}, {name,Newname}]).
% ri:renmac(a,"Macname","NewMecname").

intimp(File) ->
    transform(introduce_import, File, []).

upregex() ->
    catch_referr(fun()->
        transform_trap(upgrade_regexp,[])
    end).

%% ===============
%% Others:
%%

%cluster_agglom() -> %%TODO:wishlist

%cluster_genetic() -> %%TODO:wishlist

%% ===============
%% server management:
%%

%% Add either a file, or a complete directory by
%% recursively addig all "*.erl" it contains.
add(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun add/1,L);
add(Mod) when is_atom(Mod) ->
    add(atom_to_list(Mod)++".erl");
add(File) when is_list(File) ->
    recurse_erl(File, fun add_file/1).
add_file(File) ->
    trapui(fun()-> ?UI:add(File) end).
%ri:add([a,b]).
%ri:add("a.erl").

drop(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun drop/1,L);
drop(Mod) when is_atom(Mod) ->
    trapui(fun ()-> mod2file(Mod) end,
           fun(F)-> ?UI:drop(F) end);
drop(File) when is_list(File) ->
    recurse_erl(File, fun drop_file/1).
drop_file(File) ->
    trapui(fun()-> ?UI:drop(File) end).
%ri:drop(a).

ls()   -> trapui(fun ?UI:filelist/0).

undo() -> trapui(fun()-> ?UI:undo([]) end).

graph(Target) ->
    graph(Target,[]).

graph(Target, Filter) when is_atom(Target) ->
    graph(atom_to_list(Target)++".dot",Filter);

graph(Target, Filter) ->
    trapui(fun()-> ?UI:draw(Target, Filter) end).

backup() -> trapui(fun ?UI:backup/0).

clean()  -> trapui(fun ?UI:clean/0).

reset()  -> trapui(fun ?UI:reset/0).

%% ===============
%% Regexp helpers:
%%
%%@private

to_posrange(File,Range) ->
    rgxpos(File,Range).

to_pos(_,Pos) when is_integer(Pos) ->
    Pos;

to_pos(File,Pos) ->
    {P,_}=rgxpos(File,Pos),
    P.

%%TODO:wishlist improve rgxpos/3 to be context sensitive and indexless
%%TODO:wishlist check Rgx
rgxpos(File,Pos) ->
    case ?Query:exec(?File:find(File)) of
        [FileNode] -> Text = lists:flatten(?Syn:tree_text(FileNode)),
                      rgxpos_text(Text,Pos);
        _          -> throw(?RefError(file_not_present,[File]))
    end.

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
%%TODO:!!! better separation!!!
    end;
rgxpos_text(_,Pos) -> throw(?LocalError(invalid_pos,[Pos])).

tryrgx(RegFun,Params=[_,RRgx|_]) ->
    case apply(regexp,RegFun,Params) of
        {match,M} -> M;
        {ok,R,1}  -> R;
        _         -> throw(?LocalError(invalid_regexp,[RRgx]))
    end.

%% getparen("abc\\tdef")     = {{1,8},"abc\\tdef"}
%% getparen("abc\\(de\\)f")  = {{4,5},"abcdef"}
%% getparen("a\\(b\nd\\)ef") = {{2,4},"ab\ndef"}
%% getparen("a\\(bcdef\\)")  = {{2,6},"abcdef"}
%% getparen("ab\\(cdef")     = {{3,6},"abcdef"}
%% getparen("abcde\\)f")     = {{1,5},"abcdef"}
%% getparen("ab\\)c\\(d\\)") = invalid

getparen(L=[_|_]) ->
    getparen([],1,[],L).

getparen([], I,R,[$\\,$(|T]) ->
    getparen([I],I,R,T);
getparen([],I,R,[$\\,$)|T]) ->
    getparen([1,I-1],I,R,T);
getparen([P],I,R,[$\\,$)|T]) ->
    getparen([P,I-1],I,R,T);
getparen(S,  I,R,[$\\,C |T]) when is_integer(C), C/=$(, C/=$) ->
    getparen(S,I+2,R++[$\\,C],T);
getparen(S,  I,R,[C|T])      when is_integer(C), C/=$\\ ->
    getparen(S,I+1,R++[C],T);
getparen([], I,R,[])-> {{1,I-1},R};
getparen([P],I,R,[])-> {{P,I-1},R};
getparen([P,Q],_,R,[])-> {{P,Q},R};
getparen(_,_,_,_)-> invalid.

%% ===============
%% Other helpers:
%%

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

transformp(Ref,Source,Pos,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{position,to_pos(File,Pos)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

transformr(Ref,Source,Range,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{posrange,to_posrange(File,Range)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

transform(Ref, Source, Args) ->
    catch_referr(fun()->
        transform_inject(Ref, Source, fun id/1, Args)
    end).

transform_inject(Ref, Source, Fun, Arg) when is_function(Fun,1) ->
    Args = [{file,  mod2filec(Source)},
            {module,file2modc(Source)}|Arg],
    transform_trap(Ref, Fun(Args)).

catch_referr(Fun) when is_function(Fun,0) ->
    try
        Fun() %{ok,}
    catch
        E -> message(?Error:error_text(E)),
             error %{error,E}
    end.

transform_trap(Refac, Args) when is_atom(Refac), is_list(Args) ->
%    io:format("DEBUG: ~p:~p~n",[Refac,Args]),
    Ref = list_to_atom("referl_tr_"++atom_to_list(Refac)),
    {Res,Status} =
      trap(
        fun() ->
            _Previous = busywait(dont_care),
            ?UI:transform(Ref, Args),
            timer:sleep(?TRAN_WT),
            busywait(missing)
        end,
        fun(X) -> {X, get_all_msg(trfinished)} end),
    io:format("status: ~p~n",[Status]),
    case Res of
        missing    -> message("Received no result!"),
                      unknown;
        {result,_} -> result;
        {abort, _} -> %message(?Error:error_text(M)),
                      deny;
        {error, E} -> message("Fatal: " ++ io_lib:print(E)),
                      error
    end.

busywait(P) ->
    case Res=?Transform:wait() of
        none -> P;
        _    -> busywait(Res)
    end.

trapui(Action) when is_function(Action,0) ->
    trapui(fun nil/0, fun(_)-> Action() end).

trapui(Pre,Action) ->
    catch_referr(fun()->
        trap(Pre,
             Action,
             fun(X)->{X,get_all_msg(uifinished)} end)
    end).

trap(Action,Done) when is_function(Action,0) ->
    trap(fun nil/0, fun(_) -> Action() end, Done).

trap(Pre, Action, Done)
        when is_function(Pre,0), is_function(Action,1),
             is_function(Done,1) ->
    process_flag(trap_exit, true),
    PID = spawn_link(fun() ->
      try
        referl_ui_evsend:start(self()),
        receive installed -> installed end,
        State  = Pre(),
        Answer =
          try
            Action(State)
          catch
            E       -> Done(E),
                       throw(E);
            error:E -> Done({error,E}),
                       erlang:error(E)
          end,
        exit({ok,Done(Answer)})
      catch
        error:E2 -> exit({error,E2});
        E2       -> exit({throw,E2})
      end
    end),
    receive
        {'EXIT',PID,{throw,Exc}} -> throw(Exc);
        {'EXIT',PID,{error,Exc}} -> erlang:error(Exc);
        {'EXIT',PID,{ok,Reason}} -> Reason
    end.

get_all_msg(F) ->
    get_all_msg_loop(F,nostatus).

get_all_msg_loop(F,S) ->
    receive
        {F,[]}      -> finished;
        {F,_}       -> S;
        {uifinished,_} -> get_all_msg_loop(F,S);
        {status,S2} -> get_all_msg_loop(F,S2);
        M           -> io:format("message: ~p~n",[M]),
                       get_all_msg_loop(F,S)
    after
        ?TO -> message("Error: timeout when "++
                       "waiting for `" ++ io_lib:print(F) ++
                       "' message.~n"),
               timeout
    end.

recurse_erl(Fil, Action) when is_function(Action,1) ->
  File = filename:absname(Fil),
  case filelib:is_dir(File) of
    true ->
        All = filelib:wildcard(filename:join(File,"*")),
        {Dir,NotDir} = lists:partition(fun filelib:is_dir/1,All),
        IsErl = fun(S)-> nomatch /= regexp:match(S,
                         "\.[eE][rR][lL]$") end,
        Erl = lists:filter(IsErl, NotDir),
        lists:foreach(fun(F) -> recurse_erl(F,Action) end,
                      Erl ++ Dir);
    false ->
        case filelib:is_regular(File) of
            true  -> Action(File);
            false -> message("Warning: Not a file: " ++ File)
        end
  end.

file2modc(Source) ->
    _ = guard_ready(Source),
    file2mod(Source).

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

mod2filec(Source) ->
    guard_ready(mod2file(Source)).

mod2file([]) ->
    throw(?LocalError(no_file,[]));
mod2file(File) when is_list(File) ->
    filename:absname(File);
mod2file(Mod ) when is_atom(Mod ) ->
    FileNodes =
        ?Query:exec(?Query:seq([
            ?Mod:find(Mod),
            ?Mod:file() ])),
    case FileNodes of
        [File] -> ?File:path(File);
        []     -> throw(?RefError(mod_not_found, [Mod]));
        [_|_]  -> throw(?LocalError(ambiguous_mod, [Mod]));
        X      -> throw(?LocalError(internal_unexpected, [X]))
    end.

guard_ready([])   ->
    throw(?LocalError(no_file,[]));
guard_ready(File) ->
    case state(File) of
        ok      -> File;
        invalid -> throw(?LocalError(not_ready,[File]));
        _       -> throw(?RefError(file_not_present, [File]))
    end.

state(Source) -> trap(
  fun() ->
    mod2file(Source)
  end,
  fun(F) ->
    ?UI:status(F),
    receive
        {add,F}        -> get_uifinished(ok);
        {S,F}          -> get_uifinished(S);
        {uifinished,_} -> not_present
    after
        ?TO  -> timeout
    end
  end,
  fun id/1).

get_uifinished(R) ->
    receive
        {uifinished,_} -> R;
        E -> message("unexpected msg: " ++ io_lib:print(E)),
             get_uifinished(R)
    after
        ?TO -> timeout
    end.

%% ===============
%% Elementary:

nil()-> [].
id(X)-> X.

%% ===============
