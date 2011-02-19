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

%%% @doc This module is an output library.
%%%
%%% It can be used by functions that are parametrized by their output and may
%%% call each other.
%%% 
%%% A template:
%%% ```
%%% f(Output) ->
%%%     {W, C} = cl_out:open(Output),        %% Opening the output.
%%%     cl_out:fwrite(W, "g started~n", []), %% Writing to the output.
%%%     g(W),                                %% Calling a function that may
%%%                                          %% write to the output.
%%%     cl_out:fwrite(W, "g stopped~n", []), %% Writing to the output.
%%%     cl_out:close(C).                     %% Closing the output.
%%%
%%% g(Output) ->
%%%     {W, C} = cl_out:open(Output),        %% Opening the output.
%%%     cl_out:fwrite(W, "g running~n", []), %% Writing to the output.
%%%     cl_out:close(C).                     %% Closing the output.
%%% '''
%%%
%%% Both `f' and `g' will work correctly with every output (that has the type
%%% {@link output_ref()}).
%%% E.g. if `f({file, FileName, [write]})' is called, the opened file will be
%%% closed only at the end of `f'. But if `g({file, FileName, [write]})' is
%%% called, `g' will close the file.

%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_out).
-vsn("$Rev: 3675 $").

-export([open/1, close/1, fwrite/2, fwrite/3, new_section/1,
         start_section_process/1, file_name_gen/2,
         start_output_collector/0, get_string_and_stop/1]).

%%% @type file_name() = string().
%%%
%%% A string which represents a file by its name.

%%% @type io_device().
%%%
%%% As returned by {@link file:open/2}, a process handling IO protocols.

%%% @type output_function() = ((string() | new_section | close) -> ok).
%%%
%%% A callback function that is invoked with a string when that string would be
%%% written to the output.
%%% When the {@link new_section/1} function is called, the output function will
%%% be called with a `new_section' atom (if the `send_new_section' option is
%%% `true').
%%% When the {@link close/1} function is called, the output function will be
%%% called with a `close' atom (if the `send_close' option is `true').

%%% @type output_ref() = null |
%%%                      stdout |
%%%                      {stdout, Options::proplist()} |
%%%                      {file, file_name()} |
%%%                      {file, file_name(), Options::proplist()} |
%%%                      {device, io_device()} |
%%%                      {device, io_device(), Options::proplist()} |
%%%                      {function, output_function()} |
%%%                      {function, output_function(), Options::proplist()} |
%%%                      {process, pid()} |
%%%                      {process, pid(), Options::proplist()} |
%%%                      {list, [output_ref()]} |
%%%                      {list, [output_ref()], Options::proplist()}.
%%%
%%% It is a reference to an output device, which can be opened by
%%% {@link open/1}.
%%% If the `Options' is omitted, every options will have its default value.
%%%
%%% The different types of `output_ref()':
%%% <ul>
%%%     <li>If it is `null', the output will not be written anywhere.</li>
%%%
%%%     <li>If it is `{stdout, Options}', the output will be written to the
%%%         standard output.
%%%
%%%         Options:
%%%         <ul>
%%%             <li>`new_section_string::string()': specifies what string to
%%%                 print when the {@link new_section/1} function is called.
%%%                 It is 
%%% ```
%%% "\n"++
%%% "==============================\n"++
%%% "==============================\n"++
%%% "\n"
%%% '''
%%%                 by default.</li>
%%%         </ul></li>
%%%     <li>If it is `{file, file_name(), Options}', the output will be written
%%%         to the specified file.
%%%         
%%%         Options:
%%%         <ul>
%%%             <li>`mode::(write | append)'.
%%%                 The default is `write'.
%%%                 This option specifies what to do when the file exists: if
%%%                 the `mode' is `write', it will be overwritten, if the `mode'
%%%                 is `append', the output will be appended to the file.</li>
%%%             <li>`fast::bool()'.
%%%                 The default is `false'.
%%%                 If `false', the file will be opened and closed at every
%%%                 writing request, so if {@link close/1} was not called
%%%                 (e.g. the function that uses the output crashes), the file
%%%                 would still be closed.
%%%                 These files (where `fast' is `false') can be used as more
%%%                 than one channel, e.g. if `f(Output)' is called as follows:
%%% ```
%%% f({list, [{file, "out.txt", [{mode, write}]},
%%%          {file, "out.txt", [{mode, write}]}]})
%%% '''
%%%                 then every line will be written twice into `out.txt'.
%%%                 This cannot be done when `fast' is `true'.
%%%                 However, the printing can be quite slow without the `fast'
%%%                 option.
%%%                 An example: printing into 300 files line-by-line (each file
%%%                 had 300 to 1500 lines) took 60 minutes on an avarage machine
%%%                 without the `fast' option.
%%%                 If both the fast execution and using the files in more than
%%%                 one channel are important, a process can be used that will
%%%                 handle the file.</li>
%%%             <li>`new_section_string': specifies what string to print when
%%%                 the {@link new_section/1} function is called.
%%%                 See the default value above.</li>
%%%         </ul></li>
%%%
%%%     <li>If it is `{device, io_device(), Options}', the output will be
%%%         written to the specified device. (The device will not be closed.)
%%%
%%%         Options:
%%%         <ul>
%%%             <li>`new_section_string': specifies what string to insert when
%%%                 the {@link new_section/1} function is called.
%%%                 See the default value above.</li>
%%%         </ul></li>
%%%
%%%     <li>If it is `{function, output_function(), Options}', then every time
%%%         when something is written to the output, the function will be called
%%%         with `{output, Text}' as an argument.
%%%
%%%         Options:
%%%         <ul>
%%%             <li>`send_new_section': if `true', the function will be
%%%                  called with a `new_section' atom when the
%%%                  {@link new_section/1} function is called.
%%%                  It is `true' by default.</li>
%%%             <li>`send_close': if `true', the function will be called with a
%%%                 `close' atom when the {@link close/1} function is called.
%%%                 It is `true' by default.</li>
%%%         </ul></li>
%%%
%%%     <li>If it is `{process, pid()}', the output will be sent to the process.
%%%         The output is sent in the form of `{output, Text}' messages.
%%%
%%%         Options:
%%%         <ul>
%%%             <li>`send_new_section': if `true', the process will be
%%%                 sent a `new_section' atom when the {@link new_section/1}
%%%                 function is called.
%%%                 It is `true' by default.</li>
%%%             <li>`send_close': if `true', the process will be called with a
%%%                 `close' atom when the {@link close/1} function is called.
%%%                 It is `true' by default.</li>
%%%         </ul></li>
%%%
%%%     <li>If it is `{list, [output_ref()], Options}', all output references
%%%         will be handled, and the output will be written to all.</li>
%%% </ul>

%%% @type writable_device() = null |
%%%                           {stdout, Options} |
%%%                           {file, file_name(), Options} |
%%%                           {device, io_device(), Options} |
%%%                           {function, output_function(), Options} |
%%%                           {process, pid(), Options} |
%%%                           {list, [writable_device()], Options}.
%%%
%%% It is a reference to an (opened) output device, to which the
%%% {@link cl_out:fwrite/2} and {@link cl_out:fwrite/3} functions can write.
%%%
%%% It is a special kind of {@link output_ref()}. If a function uses a
%%% `writable_device()', but it needs to be rewritten to use an
%%% {@link output_ref()}, only two lines need to be added (if the original
%%% argument's name will be the new writable_device's name).
%%%
%%% E.g. a function that uses a {@link writable_device()}:
%%% ```
%%% %% @spec f(writable_device()) -> ok
%%% f(W) ->
%%%     cl_out:fwrite(W, "hello"),
%%%     g(W).
%%% '''
%%%
%%% The function modified to use {@link output_ref()}:
%%% ```
%%% %% @spec f(output_ref()) -> ok
%%% f(Output) ->
%%%     {W, C} = cl_out:open(Output),
%%%     cl_out:fwrite(W, "hello"),
%%%     g(W),
%%%     cl_out:close(W).
%%% '''

%%% @type closable_device() = null |
%%%                           {device, io_device(), Options} |
%%%                           {function, output_function(), Options} |
%%%                           {process, pid(), Options} |
%%%                           {list, [closable_device()], Options}.
%%%
%%% It is a reference to an (opened) output device, which can be closed by
%%% {@link close/1}.

%%%%% core functions

%% @spec open(output_ref()) -> {writable_device(), closable_device()} |
%%                             {error, Reason}
%%
%% @doc Opens a device for writing.
%% Returns the device which should be used for writing and passing as an
%% argument (for further opening, writing and closing), and the device which
%% should be closed after using the output device.
open(null) ->
    {null, null};
open(stdout) ->
    open({stdout, []});
open({stdout, Options}) ->
    {{stdout, Options}, null};
open({Type, Device}) ->
    open({Type, Device, []});
open({file, FileName, Options}) ->
    case get_option(file, fast, Options) of
        true ->
            case file:open(FileName, [get_option(file, mode, Options)]) of
                {ok, Device} ->
                    {{device, Device, Options}, {device, Device, Options}};
                {error, Reason} ->
                    {error, {FileName, file:format_error(Reason)}}
            end;
        false ->
            open_slow_file(FileName, Options)
    end;
open({device, Device, Options}) ->
    {{device, Device, Options}, null};
open({function, Fun, Options}) ->
    C = case get_option(function, send_close, Options) of
            true -> {function, Fun, Options};
            false -> null
        end,
    WOptions = [{send_close,false}|proplists:delete(send_close, Options)],
    {{function, Fun, WOptions}, C};
open({process, Pid, Options}) ->
    C = case get_option(process, send_close, Options) of
            true -> {process, Pid, Options};
            false -> null
        end,
    WOptions = [{send_close,false}|proplists:delete(send_close, Options)],
    {{process, Pid, WOptions}, C};
open({list, OutputRefs, Options}) ->
    {WL, CL} = lists:foldl(
                 fun(OutputRef, {WListAcc, CListAcc}) -> 
                         case open(OutputRef) of
                             {error, Reason} ->
                                 close({list, CListAcc, Options}),
                                 {error, Reason};
                             {W, C} ->
                                 {[W|WListAcc], [C|CListAcc]}
                         end

                 end,
                 {[], []},
                 OutputRefs),
    {{list, lists:reverse(WL), Options},
     {list, CL, Options}}.

%% @spec open_slow_file(file_name(), proplist()) -> {W, C} | {error, Reason}
%%
%% @doc It opens a file, which does not have the `fast' option true.
%% If the option `mode' is `write', it creates an empty file.
open_slow_file(FileName, Options) ->
    case get_option(file, mode, Options) of
        write ->
            case file:open(FileName, [write]) of
                {ok, Device} ->
                    file:close(Device),
                    Opts2 = [{mode, append}|proplists:delete(mode,Options)],
                    {{file, FileName, Opts2}, null};
                {error, Reason} ->
                    {error, {FileName, file:format_error(Reason)}}
            end;
        append ->
            {{file, FileName, Options}, null}
    end.

%% @spec fwrite(writable_device(), string()) -> ok
%%
%% @doc Writes `Format' to the output device.
fwrite(WritableDevice, Format)->
    fwrite(WritableDevice, Format, []).

%% @spec fwrite(writable_device(), string(), [term()]) -> ok
%%
%% @doc Writes `Data' with the specified formatting to the output device.
%% For the format of `Format', see the documentation of {@link io:fwrite/3}
%% function.
fwrite(null, _, _) ->
    ok;
fwrite({stdout, _Options}, Format, Data) ->
    io:fwrite(Format, Data);
fwrite({file, FileName, _Options}, Format, Data) ->
    append_to_file(FileName, Format, Data);
fwrite({device, Device, _Options}, Format, Data) ->
    io:fwrite(Device, Format, Data);
fwrite({function, Fun, _Options}, Format, Data) ->
    Fun(lists:flatten(io_lib:fwrite(Format, Data)));
fwrite({process, Pid, _Options}, Format, Data) ->
    Pid ! {output, lists:flatten(io_lib:fwrite(Format, Data))},
    ok;
fwrite({list, Devices, _Options}, Format, Data) ->
    lists:foreach(
      fun(Device) -> 
              ok = fwrite(Device, Format, Data)
      end,
      Devices),
    ok.

%% @spec new_section(writable_device()) -> ok
%%
%% @doc Starts a new section in the device.
new_section(null) ->
    ok;
new_section({stdout, Options}) ->
    io:fwrite("~s", [get_option(stdout, new_section_string, Options)]);
new_section({file, FileName, Options}) ->
    append_to_file(FileName, "~s",
                   [get_option(file, new_section_string, Options)]);
new_section({device, Device, Options}) ->
    io:fwrite(Device, "~s", [get_option(device, new_section_string, Options)]);
new_section({function, Fun, Options}) ->
    case get_option(function, send_new_section, Options) of
        true -> 
            Fun(new_section);
        false ->
            ok
    end;
new_section({process, Pid, Options}) ->
    case get_option(process, send_new_section, Options) of
        true -> 
            Pid ! new_section,
            ok;
        false ->
            ok
    end;
new_section({list, Devices, _Options}) ->
    lists:foreach(
      fun(Device) -> 
              new_section(Device)
      end,
      Devices),
    ok.

%% @spec close(closable_device()) -> ok | {error, Reason}
%%
%% @doc Closes the device.
close(null) ->
    ok;
close({device, Device, _Options}) ->
    file:close(Device);
close({function, Fun, Options}) ->
    case get_option(function, send_close, Options) of
        true -> 
            Fun(close);
        false ->
            ok
    end;
close({process, Pid, Options}) ->
    case get_option(function, send_close, Options) of
        true -> 
            Pid ! close,
            ok;
        false ->
            ok
    end;
close({list, Devices, _Options}) ->
    lists:foreach(
      fun(Device) ->
              ok = close(Device)
      end,
      Devices),
    ok.

%% @spec get_option(atom(), atom(), proplist()) -> term()
%%
%% @doc Returns the option stored in the `Options'. If the option is not
%% present, the default value of the option will be returned (which can be
%% different for different device types).
get_option(DeviceType, Key, Options) ->
    case proplists:lookup(Key, Options) of
        none -> default_option(DeviceType, Key);
        {Key, Value} -> Value
    end.

default_option(_, new_section_string) ->
    "\n==============================\n"++
        "==============================\n\n";
default_option(file, fast) ->
    false;
default_option(file, mode) ->
    write;
default_option(_, send_new_section) ->
    true;
default_option(_, send_close) ->
    true.

append_to_file(FileName, Format, Data) ->
    case file:open(FileName, [append]) of
        {ok, Device} ->
            io:fwrite(Device, Format, Data),
            file:close(Device);
        {error, Reason} ->
            {error, {FileName, file:format_error(Reason)}}
    end.

%%%%% Library functions

%%% @type output_ref_generator() = 
%%%           (undefined | state() | {state(),close}) -> 
%%%               {output_ref(), state()} | ok
%%%
%%%           state() = term().
%%% 
%%% Represents a function that generates output references.
%%% It does not handle the references, they will be handled by the caller of the
%%% output generator.
%%%
%%% The information can be saved between the calls of the output generator,
%%% using the `state' argument.
%%% 
%%% The output generator should be used in the following way:
%%% <ul>
%%%     <li>First, it is called with an `undefined' argument.
%%%         It returns an output reference and the new state in a tuple.</li>
%%%     <li>Then, it can be called many times with the state that it returned
%%%         before.
%%%         Again, it returns the new output reference and the new state.</li>
%%%     <li>Finally, it is called with a `{state(),close}' tuple.
%%%         It should return `ok'.</li>
%%% </ul>

%% @spec start_section_process(output_ref_generator()) -> pid()
%%
%% @doc Starts an output handler process and returns its pid.
%% It forwards the output to the `OutGenFun'.
%% It used `OutGenFun' to create a new output for each section.
%%
%% Example:
%% ```
%% FileNameGenerator = cl_out:file_name_gen("output", ".txt"),
%% OutputHandler = cl_out:start_section_process(FileNameGenerator),
%% f({process, OutputHandler})
%% '''
%%
%% `f' uses the output handler function of the `cl_out' module.
%% The output of `f' will be placed in `output0001.txt', `output0002.txt' etc.
%% Every section of the output will be placed into a different file.
start_section_process(OutGenFun) ->
    spawn_link(fun() -> section_process(OutGenFun) end).

%% @spec section_process(output_ref_generator()) -> ok
%%
%% @doc Starts to handle the output.
section_process(OutGenFun) ->
    section_process_loop(init, OutGenFun, undefined).

%% @spec section_process_loop(ProcessState, OutGenFun, NextFunState) -> ok
%%
%% @doc The loop of the output handler process.
section_process_loop(init, OutGenFun, undefined) ->
    receive
        {output, T} -> 
            {OutputRef, State} = OutGenFun(undefined),
            {W, C} = open(OutputRef),
            fwrite(W, "~s", [T]),
            section_process_loop({W, C}, OutGenFun, State);
        new_section ->
            {OutputRef, State} = OutGenFun(undefined),
            {W, C} = open(OutputRef),
            section_process_loop({W, C}, OutGenFun, State);
        close ->
            OutGenFun({undefined,close})
    end;
section_process_loop({W, C}, OutGenFun, State) ->
    receive
        {output, T} -> 
            fwrite(W, "~s", [T]),
            section_process_loop({W, C}, OutGenFun, State);
        new_section ->
            close(C),
            {OutputRef, State2} = OutGenFun(State),
            {W2, C2} = open(OutputRef),
            section_process_loop({W2, C2}, OutGenFun, State2);
        close ->
            close(C),
            OutGenFun({State,close})
    end.

%% @spec file_name_gen(string(), string()) -> output_ref_generator()
%%
%% @doc This function is an `output_ref_generator'.
%% It generates file outputs like `file0001.txt', `file0002.txt' etc.
%% It is the same as `file_name_gen(Prefix, Postfix, 4)'
file_name_gen(Prefix, Postfix) ->
    file_name_gen(Prefix, 4, Postfix).

%% @spec file_name_gen(string(), integer(), string()) -> output_ref_generator()
%%
%% @doc This function is an `output_ref_generator'.
%% It generates file outputs like `file0001.txt', `file0002.txt' etc.
%% The format of the generated file names will be `Prefix++Number++Postfix',
%% where `Number' consists of `Numerals' numerals.
file_name_gen(Prefix, Numerals, Postfix) ->
    fun(undefined) ->
            {{file,
              Prefix ++ referl_misc:integer_to_list(1, Numerals) ++ Postfix,
              [fast]}, 2};
       ({_,close}) ->
            ok;
       (N) ->
            {{file, 
              Prefix ++ referl_misc:integer_to_list(N, Numerals) ++ Postfix,
              [fast]}, N+1}
    end.

%%% @type output_collector_pid() = pid().
%%%
%%% It represents a process that collects the output and can return it as a
%%% string.

%% @spec start_output_collector() -> output_collector_pid()
%%
%% @doc Starts an output collector process.
start_output_collector() ->
    spawn_link(fun() -> output_collector() end).

%% @spec output_collector() -> output_collector_pid()
%%
%% @doc Starts to collect output.
%%
%% Example:
%% ```
%% Collector = cl_out:start_output_collector(),
%% f({process, Collector}),
%% S = cl_out:get_string_and_stop() 
%% '''
%%
%% `f' uses the output handler functions of the `cl_out' module.
%% The output of `f' will be collected and placed in the string `S'.
output_collector() ->
    output_collector_loop([]).

%% @spec output_collector_loop([string()]) -> ok
%%
%% @doc The loop of the output collector process.
output_collector_loop(Strings) ->
    receive
        {output, T} ->
            output_collector_loop([T|Strings]);
        new_section ->
            %% @todo: it is not the best way to indicate new_section
            output_collector_loop(
              [get_option(process, new_section_string, [])|Strings]);
        close ->
            output_collector_loop([Strings]);
        {get_string, Pid} ->
            Pid ! lists:flatten(lists:reverse(Strings)),
            output_collector_loop([Strings]);
        stop ->
            ok
    end.

%% @spec get_string_and_stop(output_collector_pid()) -> string()
%%
%% @doc Returns the string collected by the collector process and stops the
%% process.
get_string_and_stop(Pid) ->
    Pid ! {get_string, self()},
    Pid ! stop,
    receive
        String -> String
    end.
