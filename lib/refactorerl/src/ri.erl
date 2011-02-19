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

%%% @doc This module implements a console user interface for the tool.
%%%
%%% The idea behind "ri" is to enable the user to be able to do refactorings
%%% as easily as you can do debugging with Erlang's dbg module.
%%% You give a simple, short command, and get response on the console.
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
% @todo generated help from spec and doc tags
% @todo support for filename suffixes for add/drop
% @todo :wishlist better help
% @todo asynchronous handling option
% @todo :wishlist supplement filelist output with status
% @todo :wishlist better parameter checking (Name, Pos, ...)!
% @todo :wishlist remove guards from exported functions
%       and signal errors verbosely
% @todo :wishlist verbose commenting with edoc tags
% @todo :wishlist spec tags
% @todo :wishlist research alternatives like
%       Beg,End -> line.col:line.col; line.col:col; line:regex ;
% @todo :wishlist function scope regexps
% @todo: better regexp mark handling
%

-module(ri).
-vsn("$Rev: 3676 $").
-include("refactorerl.hrl").

-export([help/0, h/0, h/1]).

-export([add/1,   drop/1, save/1,
         ls/0,    ls/1,   reset/0, cat/1,   cat/2, cat/3,
         undo/0,  graph/0,graph/1,graph/2,
         svg/0,   svg/1,  svg/2, gn/2,
         getcfg/0,setcfg/3 ]).

-export([add_h/0,    drop_h/0,  save_h/0,
         ls_h/0,     reset_h/0, cat_h/0,
         undo_h/0,   graph_h/0, svg_h/0, gn_h/0,
         getcfg_h/0, setcfg_h/0 ]).

-export([elimvar/2,extfun/3, expfun/2, genfun/3,
         inlfun/2, inlmac/2, intrec/4,
         merge/3,  movfun/3, movrec/3, movmac/3,
         reorder/3,renfld/4, renfun/3,
         renhrl/2, renrec/3, renmac/3,
         renmod/2, renvar/3, tupfun/2, upregex/0 ]).

-export([elimvar_h/0,extfun_h/0, expfun_h/0, genfun_h/0,
         inlfun_h/0, inlmac_h/0, intrec_h/0,
         merge_h/0,  movfun_h/0, movrec_h/0, movmac_h/0,
         reorder_h/0,renfld_h/0, renfun_h/0,
         renhrl_h/0, renrec_h/0, renmac_h/0,
         renmod_h/0, renvar_h/0, tupfun_h/0, upregex_h/0 ]).

-export([error_text/2]).
%-export([rgxpos_text/2]). %%DEBUG
%-export([getparen/1]). %%DEBUG

-define(TO,9000). %timout in ms for Ui messages
-define(TRAN_WT,100). %delay in ms between transformation and wait

-define(DEF_GRAPH_NAME,graph).

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

error_text(_,_) -> unknown.


help() ->
    h().

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
extfun(File, Range, Name) ->
    transformr(extract_fun, File, Range, [{name, Name}]).
% ri:extfun(mod_or_file,"A+B",f).

merge_h() ->
    message("merge(ModFile,Range_of_expression,NewVar)").
merge(File, Range, Varname=[C|_]) when is_integer(C) ->
    transformr(merge, File, Range, [{varname, Varname}]).
% ri:merge(mod_or_file,"1+2","V").

inlfun_h() ->
    message("inlfun(ModFile,Pos_of_fun_application)").
inlfun(File, Pos) ->
    transformp(inline_fun, File, Pos, []).
% ri:inlfun(mod_or_file,"f[(]1").

inlmac_h() ->
    message("inlmac(ModFile,Pos_of_macro_use)").
inlmac(File, Pos) ->
    transformp(inline_mac, File, Pos, []).
% ri:inlmac(mod_or_file,"?Mac").

tupfun_h() ->
    message("tupfun(ModFile,Range_of_arguments)").
tupfun(File, Range) ->
    transformr(tuple_funpar, File, Range, []).
%%@todo :wishlist Begin,End -> name/arity first last
% ri:tupfun(mod_or_file,"A,B").

%%Not recommended
%reorder(File, Fun, Arity, Order) ->
%    reorder(File, {Fun,Arity}, Order).

reorder_h() ->
    message("reorder(ModFile,{Fun,Arity},PermutationList)").
%@todo :wishlist check Order
reorder(File, {Fun,Arity}, Order=[I|_])
      when is_atom(Fun), is_integer(Arity), is_integer(I) ->
    transform(reorder_funpar, File,
              [{function, Fun}, {arity, Arity}, {order, Order}]).
% ri:reorder(mod_or_file,{f,2},[2,1]).

expfun_h() ->
    message("expfun(ModFile,Pos_of_funexpr)").
expfun(File, Pos) ->
    transformp(expand_funexpr, File, Pos, []).
% ri:expfun(mod_or_file, "fun g").

movfun_h() ->
    message("movfun(Source,Target,FunctionList=[{FunctionName,Arity}|_])").
movfun(Source, Target, Fun={A,I})
      when is_atom(A), is_integer(I) ->
    movfun(Source, Target, [Fun]);
% ri:movfun(mod_or_file,b,{f,2}).

%%@todo :wishlist check Funlist
movfun(Source, Target, Funlist=[{A,I}|_])
      when is_atom(A), is_integer(I) ->
    transform2(move_fun, Source, Target, [{funlist, Funlist}]).
% ri:movfun(mod_or_file,b,[{f,2},{g,0}]).

movrec_h() ->
    message("movrec(Source,Target,RecordList=[Record|_])").
movrec(Source, Target, Rec) when is_atom(Rec) ->
    movrec(Source, Target, [Rec]);
% ri:movrec(mod_or_file,b,rec).

%%@todo :wishlist check Reclist
movrec(Source, Target, Reclist=[A|_]) when is_atom(A) ->
    transform2(move_rec, Source, Target, [{reclist, Reclist}]).
% ri:movrec(mod_or_file,b,[r1,r2]).

movmac_h() ->
    message("movmac(Source,Target,MacroList=[Macro|_])").
movmac(Source, Target, Mac=[C|_]) when is_integer(C) ->
    movmac(Source, Target, [Mac]);
% ri:movmac(mod_or_file,b,"Mac").

%%@todo :wishlist check Maclist
movmac(Source, Target, Maclist=[[C|_]|_]) when is_integer(C) ->
    transform2(move_mac, Source, Target, [{maclist, Maclist}]).
% ri:movmac(mod_or_file,b,["Mac1","Mac2"]).

genfun_h() ->
    message("genfun(ModFile,Range_of_body,NewVar)").
genfun(File, Range, Newname) ->
    transformr(gen, File, Range, [{varname, Newname}]).
% ri:genfun(mod_or_file, "[+]\\(2", "Two").

%%Not recommended
%renfun(File, Fun, Arity, Newname) ->
%    renfun(File, {Fun,Arity}, Newname).

renfun_h() ->
    message("renfun(ModFile,{FunctionName,Arity},NewFun)").
renfun(File, {Fun,Arity}, Newname)
      when is_atom(Fun), is_integer(Arity) ->
    transform(rename_fun, File,
              [{function, Fun}, {arity, Arity}, {name, Newname}]).
% ri:renfun(mod_or_file,{f,2},new).

renvar_h() ->
    message("revar(ModFile,Pos_of_variable,NewVar)").
renvar(File, Pos, Newname=[_|_]) ->
    transformp(rename_var, File, Pos, [{varname, Newname}]).
% ri:renvar(mod_or_file, "X=", "V").

renmod_h() ->
    message("renmod(OldModFile,NewMod)").
renmod(File, Newname) ->
    transform(rename_mod, File, [{name, Newname}]).
% ri:renmod(mod_or_file, newmod).

renhrl_h() ->
    message("renhrl(OldHrl,NewHrl)").
renhrl(File, Newname) ->
    transform(rename_header, File, [{name, Newname}]).
% ri:renhrl("a.hrl", "b.hrl").

renrec_h() ->
    message("renrec(ModFile,OldRecord,NewRecord)").
renrec(File,Record,NewName)
      when is_atom(Record) ->
    transform(rename_rec, File,
              [{record, Record}, {name, NewName}]).
% ri:renrec(mod_or_file,recname,newrecname).

renfld_h() ->
    message("renfld(ModFile,Record,OldField,NewField)").
renfld(File,Record,Field,NewName)
      when is_atom(Record), is_atom(Field) ->
    transform(rename_recfield, File,
              [{record,Record}, {recfield,Field}, {name,NewName}]).
% ri:renfld(mod_or_file,recname,field1,newfield1).

elimvar_h() ->
    message("elimvar(ModFile,Pos_of_variable)").
elimvar(File, Pos) ->
    transformp(elim_var, File, Pos, []).
% ri:elimvar(mod_or_file,"X=").

%%Not recommended
%intrec(File, A, B, Newname, Fields) ->
%    intrec(File, {A,B}, Newname, Fields).

intrec_h() ->
    message("intrec(ModFile,Range_of_tuple,NewRecord,Fields=[field|_])").
intrec(File, URange, Newname, AFields=[A|_]) when is_atom(A) ->
    SList = lists:map(fun atom_to_list/1, AFields),
    Fields = string:join(SList," "),
    intrec(File, URange, Newname, Fields);
% ri:intrec(mod_or_file, "{X,Y}", newrec, [f1, f2]).

intrec(File, Range, Newname, Fields=[C|_]) when is_integer(C) ->
    transformr(introduce_rec, File, Range,
               [{name,Newname}, {text,Fields}]).
% ri:intrec(mod_or_file, "{X,Y}", rec, "f1 f2"]).

renmac_h() ->
    message("renmac(ModFile,OldMacro,NewMacro)").
renmac(File, Macro, Newname) ->
    transform(rename_mac, File, [{macro,Macro}, {name,Newname}]).
% ri:renmac(mod_or_file,"Macname","NewMecname").

%intimp(File) ->
%    transform(introduce_import, File, []).

upregex_h() ->
    message("upregex()").
upregex() ->
    catch_referr(fun()->
        transform_trap(upgrade_regexp,[])
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
cat(ModFile) ->
    catch_referr(fun() ->
        message(?Syn:tree_text(get_filenode(ModFile))) end).

cat(ModFile, RecMac) ->
    catch_referr(fun() ->
        message2(recmactext(ModFile, RecMac))
    end).

cat(ModFile, FunName, Arity) ->
    catch_referr(fun() ->
        message2(funtext(ModFile, FunName, Arity))
    end).

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

getcfg_h() ->
    message("getcfg()").
getcfg() ->
    trapui(fun ?UI:showconfig/0).

setcfg_h() ->
    message("setcfg(AppDirs,IncDirs,OutDir)").
setcfg(AppDirs,IncDirs,OutDir) ->
    trapui(fun()->
               ?UI:saveconfig(AppDirs,IncDirs,OutDir)
           end).

add_h() ->
    message("add(ModFileDirList)").

%% @doc Add either a module, a file, a directory or a list of these
add(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun add/1,L);
add(Mod) when is_atom(Mod) ->
    catch_referr(fun()->
        try
            add(mod2file(Mod,true))
        catch
            ?RefError(mod_not_found,[Mod]) ->
                add(atom_to_list(Mod)++".erl")
        end
    end);
add(File=[C|_]) when is_integer(C) ->
    recurse_erl(File, fun add_file/1).

%% @private
add_file(File) ->
    trapui(fun()-> ?UI:add(File) end).
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
    recurse_erl(File, fun drop_file/1).

%% @private
drop_file(File) ->
    trapui(fun()-> ?UI:drop(File) end).
%ri:drop(a).

save_h() ->
    message2(["save(ModFileDirList)\n"]).

save(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun save/1,L);
save(Mod) when is_atom(Mod) ->
    catch_referr(fun()->
        save(mod2file(Mod,true))
    end);
save(File) when is_list(File) ->
    recurse_erl(File, fun save_file/1).

%% @private
save_file(File) ->
    catch_referrsave(fun()->
        ?FileMan:save_file(get_filenode(File))
    end).


ls_h() ->
    message2(["ls()\n"
              "ls(ModFile)\n" ]).

ls() ->
    trapui(fun ?UI:filelist/0).

ls(ModFile) ->
    catch_referr(fun() -> ls_(ModFile) end).

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
undo() -> trapui(fun()-> ?UI:undo([]) end).

graph_h() ->
    message2([
        "graph()\n"
        "graph(TargetNameFile)\n"
        "graph(TargetFile,FilterList)\n" ]).

graph() ->
    graph(?DEF_GRAPH_NAME).

graph(Target) ->
    graph(Target,[]).

graph(Target, Filter) when is_atom(Target) ->
    graph(atom_to_list(Target)++".dot",Filter);

graph(Target, Filter) ->
    trapui(fun()-> ?UI:draw(Target, Filter) end).

svg_h() ->
    message2([
        "svg()\n"
        "svg(TargetNameFile)\n"
        "svg(Target,Options)\n" ]).
svg() ->
    svg(?DEF_GRAPH_NAME).

svg(Target) ->
    svg(Target, []).

svg(Target, Opts) when is_atom(Target) ->
    svg(atom_to_list(Target)++".svg",Opts);
svg(Target, Opts) when is_list(Target) ->
    DotName = filename:rootname(Target) ++ ".dot",
    trapui(fun()-> ?UI:draw(DotName, Opts) end), %%TODO: tooltip
    Res = os:cmd("dot -Tsvg "++DotName++" -o"++Target),
    case Res of
        [] -> ok;
	_  -> {error,Res}
    end.

gn_h() ->
    message("gn(TypeAtom,Index)").
gn(TypeAtom,Index) when is_atom(TypeAtom), is_integer(Index) ->
    message(io_lib:print(?Graph:data({'$gn',TypeAtom,Index}))).

reset_h() ->
    message("reset()").
reset()  -> trapui(fun ?UI:reset/0).

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

%% @doc transform helper for source+dest refactorings
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

%% @doc transform helper for refactorings needing a position
transformp(Ref,Source,Pos,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{position,to_pos(File,Pos)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

%% @doc transform helper for refactorings needing a posrange
transformr(Ref,Source,Range,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{posrange,to_posrange(File,Range)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

%% @private
%% @doc transform helper that prints exceptions
transform(Ref, Source, Args) ->
    catch_referr(fun()->
        transform_inject(Ref, Source, fun id/1, Args)
    end).

%% @private
%% @doc transform helper that adds both file and module information
transform_inject(Ref, Source, Fun, Arg) when is_function(Fun,1) ->
    Args = [{file,  mod2filec(Source)},
            {module,file2modc(Source)}|Arg],
    transform_trap(Ref, Fun(Args)).

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
            error %{error,E}
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

%% @private
to_string(S) ->
    SF = lists:flatten([S]),
    io:format("~p~n",[SF]),
    case io_lib:char_list(SF) of
        true  -> SF;
        false -> io_lib:write(S)
    end.

%% @doc transform helper that catches messages synchronously
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
    message("status: "++to_string(Status)),
    case Res of
        missing    -> message("Received no result!"),
                      unknown;
        {result,_} -> result;
        {abort, _} -> %message(?Error:error_text(M)),
                      deny;
        {error, E} -> message("Fatal: " ++ io_lib:format("~p",[E])),
                      error
    end.

%% @private
%% @doc transform helper that waits for a transformation to finish
busywait(P) ->
    case Res=?Transform:wait() of
        none -> P;
        _    -> busywait(Res)
    end.

%% @private
%% @doc handle synchronous UI calls without a prelude
trapui(Action) when is_function(Action,0) ->
    trapui(fun nil/0, fun(_)-> Action() end).

%% @private
%% @doc handle synchronous UI calls
trapui(Pre,Action) ->
    catch_referr(fun()->
        trap(Pre,
             Action,
             fun(X)->{X,get_all_msg(uifinished)} end)
    end).

%% @private
%% @doc handle synchronous UI/transform calls without a prelude
trap(Action,Done) when is_function(Action,0) ->
    trap(fun nil/0, fun(_) -> Action() end, Done).

%% @doc handle synchronous UI/transform calls
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

%% @private
get_all_msg(F) ->
    get_all_msg_loop(F,nostatus).

%% @private
%% @doc print out all messages of results
get_all_msg_loop(F,S) ->
    receive
        {F,[]}      -> finished;
        {F,_}       -> S;
        {uifinished,_} -> get_all_msg_loop(F,S);
        {status,S2} -> get_all_msg_loop(F,S2);
        M           -> message("message: "++io_lib:format("~p",[M])),
                       get_all_msg_loop(F,S)
    after
        ?TO -> message("Error: timeout when "++
                       "waiting for `" ++ to_string(F) ++
                       "' message.~n"),
               timeout
    end.

%%% ----------------------------------------------------------------------------
%%% File and module related helpers

%% @doc Traverse a complete directory recursively while doing the action
%% specified on all "*.erl" files each folder contains.
recurse_erl(Fil=[_|_], Action) when is_function(Action,1) ->
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
state(Source) -> trap(
  fun() ->
    mod2file(Source)
  end,
  fun(F) ->
    ?UI:status(F),
    receive
        {add,[F]}      -> get_uifinished(ok);
        {S,F}          -> get_uifinished(S);
        {uifinished,_} -> not_present
    after
        ?TO  -> timeout
    end
  end,
  fun id/1).

%% @private
%% @doc waits until a UI call is finished and returns the result
get_uifinished(R) ->
    receive
        {uifinished,_} -> R;
        E -> message("unexpected msg: " ++ to_string(E)),
             get_uifinished(R)
    after
        ?TO -> timeout
    end.

%%% ----------------------------------------------------------------------------
%%% Elementary helpers

%% @private
nil()-> [].

%% @private
id(X)-> X.
