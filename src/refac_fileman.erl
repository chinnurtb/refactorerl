%%% The contents of this file are subject to the Mozilla Public License
%%% Version 1.1 (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.mozilla.org/MPL/
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
%%%
%%% Contributor(s): ______________________________________.
%%%
%%% @doc File handler interface module. This module converts between the
%%% textual and graph representation of files (parsing and saving).
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(refac_fileman).
-vsn("$Rev: 1206 $").

-export([add_file/1, drop_file/1, save_file/1, print_form/2]).

-include("refactorerl.hrl").


%% @spec add_file(string()) -> {file, node()} | {error, Reason}
%%
%% @doc Reads and parses the file `Name' and inserts the syntax tree into
%% the graph. Returns the file node on success (which may contain forms with
%% errors). Note that during preprocessing, every included file will also be
%% added to the graph.
add_file(Name) ->
    case refac_syntax:file(Name) of
        {file, F} ->
            add_file_result(F);
        _ ->
            error_logger:info_msg("Reading ~s starts~n", [Name]),
            case store_file_tokens(Name) of
                {error, Reason} ->
                    {error, Reason};
                {File, Tokens} ->
                    Input = refac_preproc:preprocess(Tokens, File),
                    case filename:extension(Name) of
                        ".erl" -> parse_forms(File, Input);
                        _      -> store_forms(File, Input)
                    end,
                    error_logger:info_msg("Processing ~s finished~n", [Name]),
                    add_file_result(File)
            end
    end.

add_file_result(File) ->
    case ?GRAPH:path(File, [{form, {type, '==', error}}]) of
        [] ->
            {file, File};
        [E|_] ->
            {error, (?GRAPH:data(E))#form.tag}
    end.

%% @spec store_file_tokens(string()) -> {node(),[FormTokens]} | {error,Reason}
%%       FormTokens = [{#token{},node()}]
%%
%% @doc Reads the file named `Name', splits it into tokens, and stores the
%% tokens in the graph. When reading or tokenization fails, nothing is stored.
store_file_tokens(Name) ->
    case file:read_file(Name) of
        {ok, BinaryText} ->
            Text = file_text(BinaryText),
            case refac_syntax_scanner:string(Text) of
                {ok, Tokens, _Line} ->
                    File = ?ESG:create(#file{path=Name}),
                    ?ESG:insert(?ESG:root(), file, File),
                    {File, store_tokens(Tokens, File)};
                {error, {Ln, Mod, Error}, _Line} ->
                    {error, {Ln, Mod:format_error(Error)}}
            end;
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

%% Converts the binary `Bin' to string, terminating it with a newline if it
%% is missing.
file_text(Bin) ->
    Size = size(Bin)-1,
    case Bin of
        <<_:Size/binary, $\n>> ->
            binary_to_list(Bin);
        _ ->
            binary_to_list(<<Bin/binary, "\n">>)
    end.

%% @spec store_tokens([Token], node()) -> [FormTokens]
%%       Token = {Type::atom(), Line::integer(), #token{}}
%%       FormTokens = [{#token{},node()}]
%%
%% @doc Splits a token list into forms and puts the tokens into the graph.
%% CHECK: `File' is not needed, the tokens are linked to the forms later

store_tokens([], _) ->
    [];
store_tokens(Tokens, File) ->
    {Form, Rest} = store_form_tokens(Tokens, File),
    [Form | store_tokens(Rest, File)].

store_form_tokens([{_Type,_Line,Data} | Tail], _File) ->
    Node = ?GRAPH:create(#lex{type=token, data=Data}),
    store_form_tokens(Tail, [{Data, Node}], Node).

store_form_tokens([{Type, _Line, Data} | Tail], Form, Prev) ->
    Node = ?GRAPH:create(#lex{type=token, data=Data}),
    ?GRAPH:mklink(Prev, next, Node),
    case Type of
        stop -> {lists:reverse([{Data,Node}|Form]), Tail};
        _ -> store_form_tokens(Tail, [{Data, Node} | Form], Node)
    end.


%% @spec store_forms(node(), [{tokens, FormTokens} | {form, node()}]) -> ok
%%       FormTokens = [{#token{},node()}]
store_forms(File, [{form, Form} | Tail]) ->
    ?ESG:insert(File, form, Form),
    ?ESG:close(),
    store_forms(File, Tail);
store_forms(File, [{tokens, Tokens} | Tail]) ->
    Form = ?ESG:create(#form{type=store}),
    ?ESG:insert(File, form, Form),
    ?ESG:close(),
    {_, First} = hd(Tokens),
    {_, Last} = lists:last(Tokens),
    ?GRAPH:mklink(Form, ffirst, token_leaf(First, lfirst)),
    ?GRAPH:mklink(Form, flast, token_leaf(Last, llast)),
    store_forms(File, Tail);
store_forms(_File, []) ->
    ok.

%% @spec parse_forms(node(), [{tokens, FormTokens} | {form, node()}]) -> ok
%%       FormTokens = [{#token{},node()}]
%%
%% @doc Parses `Tokens' form-by-form and stores the result in the graph under
%% the file node `File'.
parse_forms(File, [{form, Form}|Tail]) ->
    ?ESG:insert(File, form, Form),
    ?ESG:close(),
    parse_forms(File, Tail);
parse_forms(File, [{tokens, Tokens}|Tail]) ->
    parse(File, [{Type, 1, Data} ||
                    Data={#token{type=Type}, _} <- Tokens]),
    parse_forms(File, Tail);
parse_forms(_File, []) ->
    ok.

%% @spec parse(node(), [{atom(),integer(),{#token{}, node()}}]) -> ok
%%
%% @doc Parses `Tokens' and inserts the resulting syntax tree under `File'.
parse(File, Tokens) ->
    case refac_syntax_parser:parse(Tokens) of
        {error, {Ln, Mod, Msg}} ->
            error_logger:info_msg("Parser message: ~s~n",
                                  [Mod:format_error(Msg)]),
            Form = ?ESG:create(#form{type=error, tag={Ln, "Parse error"}}),
            {_,_,{_,First}} = hd(Tokens),
            {_,_,{_,Last}} = lists:last(Tokens);
        {ok, {Form, First, Last}} ->
            ok
    end,
    ?ESG:insert(File, form, Form),
    ?ESG:close(),
    ?GRAPH:mklink(Form, ffirst, token_leaf(First, lfirst)),
    ?GRAPH:mklink(Form, flast, token_leaf(Last, llast)),
    ok.

token_leaf(Lex, Tag) ->
    case ?GRAPH:path(Lex, [Tag]) of
        [] -> Lex; 
        [Child] -> token_leaf(Child, Tag)
    end.

%% @spec drop_file(node()) -> ok
%%
%% @doc Removes `File' from the graph (together with all other files which
%% depend on it).
drop_file(File) ->
    case ?GRAPH:path(File, [{incl, back}]) -- [File] of
        [Dep | _] ->
            drop_file(Dep),
            drop_file(File);
        [] ->
            refac_preproc:clear(File),
            ?ESG:remove(?ESG:root(), file, File),
            ?ESG:close()
    end.


%% @spec save_file(node()) -> ok | {error, Reason :: string()}
%%
%% @doc Writes the textual contents of `File' back to its source.
save_file(File) ->
    case ?GRAPH:path(?GRAPH:root(), [{env, {name, '==', output}}]) of
        [] ->
            throw(unsafe_save);
        [Output] ->
            case ?GRAPH:data(Output) of
                #env{value=original} ->
                    #file{path=Path} = ?ESG:data(File),
                    save_file(File, Path);
                #env{value=Dir} ->
                    #file{path=Path} = ?ESG:data(File),
                    save_file(File,
                              filename:join(Dir, filename:basename(Path)))
            end
    end.

save_file(File, Name) ->
    case file:open(Name, [write]) of
        {ok, Dev} ->
            [ print_form(Dev, Form) ||
                Form <- ?GRAPH:path(File, [form]) ],
            file:close(Dev),
            ok;
         {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

%% @spec print_form(io_device(), node()) -> ok
%%
%% @doc Prints the textual representation of one node. Its main use is to
%% interactively ask for the contents of a form using `standard_io' for `Dev'.
print_form(Dev, Form) ->
    [First] = ?GRAPH:path(Form, [ffirst]),
    print_tokens(Dev, First).

print_tokens(Dev, Token) ->
    case ?GRAPH:data(Token) of
        #lex{data=Data} -> print_token(Dev, Data)
    end,
    case ?GRAPH:path(Token, [next]) of
        [Next] -> print_tokens(Dev, Next);
        []     -> ok
    end.

print_token(Dev, #token{text=V, ws=#ws{bef=B, aft=A}}) ->
    print_text(Dev, B, V, A).

%% @spec print_text(Dev :: io_device(),
%%                  BWS :: string, V :: Type, AWS :: string) -> ok
%%                       Type = atom() | integer() | string()
%%
%% @doc Prints the textual form of a node.
print_text(Dev, BWS, V, AWS) when is_atom(V) ->
    io:format(Dev, "~s~s~s", [BWS, atom_to_list(V), AWS]);
print_text(Dev, BWS, V, AWS) when is_integer(V) ->
    io:format(Dev, "~s~s~s", [BWS, integer_to_list(V), AWS]);
print_text(Dev, BWS, V, AWS) ->
    io:format(Dev, "~s~s~s", [BWS, V, AWS]).
