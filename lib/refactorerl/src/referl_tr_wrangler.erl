-module(referl_tr_wrangler).

-vsn("$Rev: 3000 $").
-export([rename_function/1, rename_variable/1, rename_module/1,
         move_function/1, extract_function/1, tuple_funargs/1,
         introduce_macro/1]).
-include("refactorerl.hrl").

-define(Get(T, A), proplists:get_value(T, A)).

get_dirs()->
    Dirs = [filename:dirname(?File:path(FileNode))
              || FileNode <- ?Graph:path(?Graph:root(), [file])],
    lists:usort(Dirs).

get_dirs(FilePath) ->
    lists:usort(get_dirs() ++ [filename:dirname(FilePath)]).
    

%% @spec rename_function(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args should contain: 
%%     {file, string()}, {line, integer()}, {col, integer()}, {name, string()}
rename_function(Args)->
   File = ?Get(file, Args), 
   Line = ?Get(line, Args),
   Col  = ?Get(col,  Args), 
   NewName = ?Get(name, Args),
   wrangler:rename_fun(File, Line, Col, NewName, get_dirs(File), 8).

%% @spec rename_variable(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args should contain: 
%%   {file, string()}, {line, integer()}, {col, integer()}, {varname, string()}
rename_variable(Args)->
   File = ?Get(file, Args), 
   Line = ?Get(line, Args),
   Col  = ?Get(col,  Args), 
   NewName = ?Get(varname, Args),
   case wrangler:rename_var(File, Line, Col, NewName, get_dirs(), 8) of
       {ok, _} -> {ok, [File]}; 
       Result -> Result
   end.
    

%% @spec rename_module(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args should contain: 
%%     {file, string()}, {name, string()}
rename_module(Args)->
   File = ?Get(file, Args), 
   NewName = ?Get(name, Args),
   NewPath = filename:join(filename:dirname(File), NewName ++ ".erl"), 
   {NewPath, wrangler:rename_mod(File, NewName, get_dirs(), 8)}.

%% @spec move_function(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args should contain: 
%%     {file, string()}, {line, integer()}, {col, integer()}, {name, string()}
move_function(Args)->
   File = ?Get(file, Args), 
   Line = ?Get(line, Args),
   Col  = ?Get(col,  Args), 
   Name = ?Get(name, Args),
   wrangler:move_fun(File, Line, Col, Name, true, get_dirs(), 8).
%% TODO: Ask the user about creating the new file


%% @spec extract_function(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args should contain: 
%%     {file, string()}, {startline, integer()}, {startcol, integer()}, 
%%     {endline, integer()}, {endcol, integer()},{name, string()}
extract_function(Args)->
   File  = ?Get(file, Args), 
   Start = {?Get(startline, Args), ?Get(startcol, Args)},
   End   = {?Get(endline,  Args), ?Get(endcol, Args)}, 
   Name  = ?Get(name, Args),
   case wrangler:fun_extraction(File, Start, End, Name, 8) of
       {ok, _} -> {ok, [File]};
       Res -> Res
   end.
 

%% @spec tuple_funargs(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args should contain: 
%%   {file, string()}, {line, integer()}, {col, integer()}, {number, integer()}
tuple_funargs(Args)->
   File = ?Get(file, Args), 
   Line = ?Get(line, Args),
   Col  = ?Get(col,  Args), 
   Num = ?Get(number, Args),
   wrangler:tuple_funpar(File, Line, Col, Num, get_dirs(), 8).

 
%% @spec introduce_macro(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args should contain: 
%%     {file, string()}, {startline, integer()}, {startcol, integer()}, 
%%     {endline, integer()}, {endcol, integer()},{name, string()}
introduce_macro(Args)->
   File  = ?Get(file, Args), 
   Start = {?Get(startline, Args), ?Get(startcol, Args)},
   End   = {?Get(endline,  Args), ?Get(endcol, Args)}, 
   Name  = ?Get(name, Args),
   case wrangler:new_macro(File, Start, End, Name, get_dirs(), 8) of
       {ok, _} -> {ok, [File]};
       Res -> Res
   end.
