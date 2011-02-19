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

%%% @doc File handler interface module. This module converts between the
%%% textual and graph representation of files (parsing and saving).
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(referl_fileman).
-vsn("$Rev: 1998 $").

-export([add_file/1, add_form/3, drop_file/1, save_file/1]).

-include("refactorerl.hrl").


%%% ============================================================================
%%% Interface functions


%% @spec add_file(string()) -> {file, node()} | {error, Reason}
%%
%% @doc Reads and parses the file `Name' and inserts the syntax tree into
%% the graph. Returns the file node on success (which may contain forms with
%% errors). Note that during preprocessing, every included file will also be
%% added to the graph.
add_file(Name) ->
    case ?SYNTAX:file(Name) of
        {file, F} ->
            add_file_result(F);
        not_found ->
            case store_file_tokens(Name) of
                {error, Reason} ->
                    {error, Reason};
                {File, Tokens} ->
                    ?GRAPH:mklink(File, incl, File),
                    process_forms(File, Tokens, last),
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

%% @spec add_form(node(), integer() | last, [node()]) -> ok
%% 
%% @doc Preprocesses `Tokens', creates a set of forms from the result, and
%% inserts these forms into `File' starting from the position `Index'.
%% `Index' specifies the position between links from `File' with tag `form'.
add_form(File, Index, Tokens) ->
    TokenData = [{?LEX:token_data(T), T} || T <- Tokens],
    process_forms(File, [TokenData], Index),
    ok.


%% process_forms/3: entry point for preprocessing forms
process_forms(File, Forms, Index) ->
    Type = (?GRAPH:data(File))#file.type,
    process_forms(File, Type, Forms, start, Index).

%% process_forms/5: preprocesses tokens and stores the result
process_forms(_, _, [], _, _) -> ok;
process_forms(File, Type, [Tokens | Rest], State, Index) ->
    {Forms, St1} = ?PREPROC:preprocess(Tokens, File, State),
    Ind1 = process_forms(File, Type, Forms, Index),
    ?ESG:close(),
    process_forms(File, Type, Rest, St1, Ind1).

%% process_forms/4: parses and/or stores the result of preprocessing
process_forms(_, _, [], Index) -> Index;
process_forms(File, Type, [Form | Rest], Index) ->
    ?ESG:insert(File, {form, Index}, process_form(Type, Form)),
    process_forms(File, Type, Rest, next_index(Index)).

next_index(last) -> last;
next_index(Ind) when is_integer(Ind) -> Ind+1.



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
            ?PREPROC:clear(File),
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
                    #file{path=Path, eol=Eol} = ?ESG:data(File),
                    save_file(File, Eol, Path);
                #env{value=Dir} ->
                    #file{path=Path, eol=Eol} = ?ESG:data(File),
                    save_file(File, Eol,
                              filename:join(Dir, filename:basename(Path)))
            end
    end.

%% @private
%% Can be used to save a file to a different location than its origin.
save_file(File, Eol, Name) ->
    case file:open(Name, [write]) of
        {ok, Dev} ->
            Text = orig_text(Eol, ?SYNTAX:tree_text(File)),
            io:put_chars(Dev, Text),
            file:close(Dev),
            ok;
         {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

orig_text(Eol, Text) when is_list(Text) ->
    [orig_text(Eol, El) || El <- Text];
orig_text({cr, _}, $\n)   -> $\r;
orig_text({crlf, _}, $\n) -> "\r\n";
orig_text({lf, _}, $\n)   -> $\n;
orig_text(_, Ch) when Ch >= 0, Ch =< 255 -> Ch;
orig_text(_, _) -> "".
    

%%% ============================================================================
%%% Token level manipulation


%% @spec store_file_tokens(string()) -> {node(),[FormTokens]} | {error,Reason}
%%       FormTokens = [{#token{},node()}]
%%
%% @doc Reads the file named `Name', splits it into tokens, and stores the
%% tokens in the graph. When reading or tokenization fails, nothing is stored.
store_file_tokens(Name) ->
    case file:read_file(Name) of
        {ok, BinaryText} ->
            {Text, Eol} = file_text(BinaryText),
            case ?SCANNER:string(Text) of
                {ok, Tokens, _Line} ->
                    Type = case filename:extension(Name) of
                               ".erl" -> module;
                               _      -> header
                           end,
                    File = ?ESG:create(#file{type=Type, path=Name, eol=Eol}),
                    ?ESG:insert(?ESG:root(), file, File),
                    {File, store_tokens([Tok || {_Ty, _Ln, Tok} <- Tokens])};
                {error, {Ln, Mod, Error}, _Line} ->
                    {error, {Ln, Mod:format_error(Error)}}
            end;
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

%% @spec store_tokens([#token{}]) -> [FormTokens]
%%       FormTokens = [{#token{},node()}]
%%
%% @doc Splits a token list into forms and puts the tokens into the graph.
%% Removes whitespace tokens, puts their contents into the proper token.
%% Returns the processed tokens (grouped into forms).

store_tokens(Tokens) ->
    RealTokens = merge_ws(split_stop_tokens(Tokens)),
    FormTokens = split_forms(RealTokens),
    lists:map(fun store_form_tokens/1, FormTokens).

%% @spec store_form_tokens([#token{}]) -> [{#token{}, #node{}}]
%% @doc Store token data in the graph
store_form_tokens(Tokens) ->
    [{Token, ?GRAPH:create(#lex{type=token, data=Token})} || Token <- Tokens].


%% Converts the binary `Bin' to string, terminating it with a newline if it
%% is missing.
file_text(Bin) ->
    file_text(Bin, "", any).

file_text(<<"\r\n", _/binary>>=Bin, Text, any) ->
    file_text(Bin, Text, crlf);
file_text(<<"\r", _/binary>>=Bin, Text, any) ->
    file_text(Bin, Text, cr);
file_text(<<"\n", _/binary>>=Bin, Text, any) ->
    file_text(Bin, Text, lf);

file_text(<<"\r\n">>, Text, crlf) ->
    {lists:reverse([$\n|Text]), {crlf, eol}};
file_text(<<"\r">>, Text, cr) ->
    {lists:reverse([$\n|Text]), {cr, eol}};
file_text(<<"\n">>, Text, lf) ->
    {lists:reverse([$\n|Text]), {lf, eol}};
file_text(<<>>, Text, Eol) ->
    {lists:reverse(Text), {Eol, noeol}};

file_text(<<"\r\n", Tail/binary>>, Text, crlf) ->
    file_text(Tail, [$\n|Text], crlf);
file_text(<<"\r", Tail/binary>>, Text, cr) ->
    file_text(Tail, [$\n|Text], cr);
file_text(<<"\n", Tail/binary>>, Text, lf) ->
    file_text(Tail, [$\n|Text], lf);
file_text(<<C, Tail/binary>>, Text, Eol) ->
    file_text(Tail, [C|Text], Eol).


%% Split `stop' tokens into a `stop' and a `ws' or `eol' token.
split_stop_tokens([#token{type=stop, text=Text} = Token | Tail]) ->
    [$.|WS] = Text,
    EOL = lists:last(WS),
    Type = if EOL == $\n; EOL == $\r -> eol; true -> ws end,
    [Token#token{text="."}, #token{type=Type, text=WS} |
     split_stop_tokens(Tail)];
split_stop_tokens([Token|Tail]) ->
    [Token|split_stop_tokens(Tail)];
split_stop_tokens([]) ->
    [].

%% Put the contents of `ws' and `eol' tokens into the `prews' and `postws'
%% fields of the proper tokens.
merge_ws([#token{type=WST, text=WS, prews=Pre},
          #token{} = Token | Tail])
  when WST == ws; WST == eol ->
    merge_ws([Token#token{prews=Pre++WS} | Tail]);

merge_ws([#token{postws=Post} = Token | Tail]) ->
    case lists:splitwith(fun (#token{type=eol}) -> true;
                             (_) -> false
                         end, Tail) of
        {Eols, []} ->
            [Token#token{postws=Post ++ join_ws(Eols)}];
        {[], Rest} ->
            [Token | merge_ws(Rest)];
        {[Eol|Eols], Rest} ->
            {Head, Next} =
                lists:splitwith(
                  fun (#token{text=Txt}) ->
                          lists:any(fun(C)-> not lists:member(C," \t\n\r") end,
                                    Txt)
                  end, Eols),
            [Token#token{postws = Post ++ join_ws([Eol|Head])} |
             merge_ws(Next ++ Rest)]
    end.

%% Join the textual contents of tokens.
join_ws(WS)                             -> join_ws(WS, []).
join_ws([#token{text=Text}|Tail], Join) -> join_ws(Tail, [Join, Text]);
join_ws([], Join)                       -> lists:flatten(Join).

%% @spec split_forms([#token{}]) -> [[#token{}]]
%% @doc Split a token list into forms.
split_forms([]) -> [];
split_forms(Tokens) ->
    {First, Rest} = split_first_form(Tokens),
    [First|split_forms(Rest)].

split_first_form(Tokens) ->
    split_first_form(Tokens, []).
split_first_form([Head=#token{type=stop}|Tail], Form) ->
    {lists:reverse([Head|Form]), Tail};
split_first_form([Head|Tail], Form) ->
    split_first_form(Tail, [Head|Form]).



%%% ============================================================================
%%% Syntax level manipulation


%% @spec process_form(module | header, Form) -> ok
%%       Form = [{tokens, FormTokens} | {form, node()}]
%%       FormTokens = [{#token{},node()}]
process_form(module, {form, Form}) -> Form;
process_form(header, {form, Form}) -> Form;
process_form(module, {tokens, Tokens}) -> parse(Tokens);
process_form(header,
             {tokens, [{#token{type=minus}, _},
                       {#token{type=atom, value=record}, _} | _] = Tokens}) ->
    parse(Tokens);
process_form(header, {tokens, Tokens}) ->
    Form = ?ESG:create(#form{type=lex, tag=store}),
    [?ESG:insert(Form, flex, Token) || {_, Token} <- Tokens],
    Form.


%% @spec parse([{#token{}, node()}]) -> node()
%%
%% @doc Parses `Tokens' and returns the result, the root form of the tree.
parse(Tokens) ->
    TokenData =[{Type, 1, Data} || Data={#token{type=Type}, _} <- Tokens],
    case ?PARSER:parse(TokenData) of
        {error, {Ln, Mod, Msg}} ->
            error_logger:info_msg("Parser message: ~s~n",
                                  [Mod:format_error(Msg)]),
            Error = lists:flatten(
                      io_lib:format("Parse error: ~s",
                                    [Mod:format_error(Msg)])),
            ?ESG:create(#form{type=error, tag={Ln, Error}});
        {ok, Result} -> Result
    end.
