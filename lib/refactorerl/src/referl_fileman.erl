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

%%% @doc File handler interface module. This module converts between the
%%% textual and graph representation of files (parsing and saving).
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(referl_fileman).
-vsn("$Rev: 3324 $").

-export([add_file/1, add_file/2, drop_file/1, save_file/1]).
-export([add_form/3, add_text/3, drop_form/2]).

-include("refactorerl.hrl").


%%% ============================================================================
%%% Interface functions


%% @spec add_file(string()) -> {file, node()} | {error, Reason}
%%
%% @doc Reads and parses the file `Name' and inserts the syntax tree into
%% the graph. Returns the file node on success (which may contain forms with
%% errors). Note that during preprocessing, every included file will also be
%% added to the graph.

add_file(Path) ->
    add_file(Path, []).

%% @spec add_file(string(), Opt) -> {file, node()} | {error, Reason}
%%
%% @doc The same as {@link add_file/1}, except that options can be passed to
%% modify the default behaviour. Currently supported options are:
%%
%% <dl>
%%
%%  <dt>{@type update | {update, true@}}</dt> <dd>Update the contents of
%%    `File' from disk, re-analysing only the forms that have been
%%    changed since the last update.</dd>
%%
%% </dl>
add_file(Path, Opt) when not is_list(Opt) ->
    add_file(Path, [Opt]);
add_file(Path, Opt) ->
    case ?Graph:path(?Graph:root(), ?File:find(Path)) of
        [File] ->
            case proplists:get_value(update, Opt, false) of
                true  -> disk_update(File);
                false -> {file, File};
                graph -> graph_update(File) % Internal use only
            end;
        [] ->
            add_new_file(Path)
    end.

%% @spec add_new_file(string()) -> {file, node()} | {error, string()}
%% @doc Adds file from `Path' into the graph, assuming that it is not in the
%% graph yet.
add_new_file(Path) ->
    try
        {Text, EOL} = file_text(Path),
        File = create_file_node(Path, EOL),
        update(File, [],
               [{ins, store_tokens(F)} || F <- tokenize(Text)], add),
        file_status(File)
    catch
        {error, Error} ->
            {error, Error}
    end.

%% @spec disk_update(node()) -> {file, node()}
%% @doc Updates the contents of `File' from its original disk file.
disk_update(File) ->
    FData = ?Graph:data(File),
    {Text, EOL} = file_text(FData#file.path),
    ?Graph:update(File, FData#file{eol=EOL}),
    Forms = real_forms(File),
    update(File, Forms, merge(Forms, tokenize(Text)), add),
    %% see remarks for graph_update
    [add_file((?Graph:data(IF))#file.path, [{update, graph}]) ||
        IF <- includers(File)],
    file_status(File).

%% @spec graph_update(node()) -> {file, node()}
%% @doc Reparses the contents of `File' without rereading it from the disk.
%% @todo This should be replaced with a more fine-grained updating strategy
%% like invalidating only the referring forms when a preprocessor definition
%% is changed.
graph_update(File) ->
    Forms = real_forms(File),
    Ins =
        [begin
             {Hash, OrigTokens} = input_tokens(Form),
             {ins, store_tokens({Hash, [T || {T, _} <- OrigTokens]})}
         end || Form <- Forms],
    update(File, Forms, [{del, F} || F <- Forms] ++ Ins, add),
    file_status(File).

%% @spec file_status(node()) -> {file, node()} | {error, Reason}
file_status(File) ->
    case ?Graph:path(File, [{form, {type, '==', error}}]) of
        []    -> {file, File};
        [E|_] -> {error, (?Graph:data(E))#form.tag}
    end.


%% @spec add_form(node(), integer() | last, [node()]) -> ok
%%
%% @doc Preprocesses `Tokens', creates a set of forms from the result, and
%% inserts these forms into `File' starting from the position `Index'.
%% `Index' specifies the position between links from `File' with tag `form'.
%% Should not be used to add a special form.

add_form(File, Index, Tokens) ->
    {Forms, Actions} = hold_prefix(File, Index, [{ins, input_tokens(Tokens)}]),
    update(File, Forms, Actions, none).


%% @spec drop_form(node(), node()) -> ok
%% @doc Removes `Form' from `File', cleaning up preprocessor-related graph
%% parts.
drop_form(File, Form) ->
    Ind = ?Graph:index(File, form, Form),
    {RealForms, Actions} = hold_prefix(File, Ind, [{del, Form}]),
    update(File, RealForms, Actions, none).


%% @spec add_text(node(), integer() | last, string()) -> ok
%%
%% @doc Turns `Text' into tokens, preprocesses them, creates a set of forms
%% from the result, and inserts these forms into `File' starting from the
%% position `Index'. `Index' specifies the position between links from `File'
%% with tag `form'.

add_text(File, Index, Text) ->
    NewForms = [{ins, store_tokens(T)} || T <- tokenize(Text)],
    {RealForms, Actions} = hold_prefix(File, Index, NewForms),
    update(File, RealForms, Actions, none).


%% @spec drop_file(node()) -> ok
%%
%% @doc Removes `File' from the graph (together with all other files which
%% depend on it).

drop_file(File) ->
    lists:foreach(
      fun (DepFile) ->
              Forms = real_forms(DepFile),
              update(DepFile, Forms, [{del, F} || F <- Forms], drop),
              ?ESG:remove(?ESG:root(), file, DepFile),
              ?ESG:close()
      end, [File | includers(File)]).


%% @spec save_file(node()) -> ok | {error, Reason :: string()}
%%
%% @doc Writes the textual contents of `File' back to its source.
save_file(File) ->
    FileData = #file{path=Path, eol=Eol} = ?Graph:data(File),
    SavePath =
        case output_env() of
            original -> Path;
            Dir      -> filename:join(Dir, filename:basename(Path))
        end,
    case file:open(SavePath, [write]) of
        {ok, Dev} ->
            Text =
                [begin
                     FormText = ?Syn:tree_text(Form),
                     set_form_hash(Form, form_hash(FormText)),
                     FormText
                 end || Form <- real_forms(File)],
            io:put_chars(Dev, orig_text(Eol, Text)),
            file:close(Dev),
            %% This is used for file saving after undo
            ?Graph:update(File, FileData#file{lastmod=now()}),
            ok;
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.


%%% ============================================================================
%%% Implementation

%%% private type
%%% formData() = {Hash::integer(), [#token{}]}. Holds the tokens of a
%%% complete form before storing them in the graph. The hash value is used to
%%% compare with existing forms in the graph.

%%% private type
%%% inputForm() = {Hash::integer(), [{#token{}, node()}]}. Holds the
%%% tokens of a complete form after storing them in the graph. The hash value
%%% will be saved in the form node. The token list can be passed directly to
%%% the preprocessor, see {@link referl_preproc:formTokens()}.


%%% ============================================================================
%%% Form level manipulations

%%% private type
%%% formAction() = {ins, inputForm()} |
%%%                {hold, Form::node()} |
%%%                {del, Form::node()}

%% @spec update(node(), [node()], [formAction()], Progress) -> ok
%%       Progress = none | add | drop
%% @doc Updates the contents of `File' by applying the elements of `Actions'.
%% `Forms' is the result of `real_forms(File)' (it is always computed before
%% calling `update'). `Progress' specifies what kind of progress reporting
%% should be done.

update(File, Forms, Actions, Progress) ->
    #file{type=FileType, path=Path} = ?Graph:data(File),
    update(Actions, Forms, 1, start, File, FileType,
           progress_start(Progress, Path, act_count(Actions))).

act_count(Actions)              -> act_count(Actions, 0).
act_count([{ins, _} | Tail], N) -> act_count(Tail, N+1);
act_count([{del, _} | Tail], N) -> act_count(Tail, N+1);
act_count([_        | Tail], N) -> act_count(Tail, N);
act_count([],                N) -> N.


update([{hold, Form} | ATail], [Form | FTail], Index, PSt, File, FT, P) ->
    Count = 1 + length(?Graph:path(Form, [{fdep, back}])),
    update(ATail, FTail, Index+Count, PSt, File, FT, P);

update([{del, Form} | ATail], [Form | FTail], Index, PSt, File, FT, P) ->
    ?PreProc:detach(File, Form),
    lists:foreach(
      fun (F) ->
              ?ESG:remove(File, form, F),
              ?ESG:close()
      end, [Form | ?Graph:path(Form, [{fdep, back}])]),
    update(ATail, FTail, Index, PSt, File, FT, progress_step(P));

update([{ins, {Hash, Input}} | ATail], Forms, Index, PSt, File, FT, P) ->
    {NewForms, PSt1} = ?PreProc:preprocess(Input, File, PSt),
    Index1 =
        lists:foldl(
          fun
              (ProcessedTokens, Index0) ->
                  Form = parse_form(FT, ProcessedTokens),
                  set_form_hash(Form, Hash),
                  ?ESG:insert(File, {form, Index0}, Form),
                  Index0+1
          end,
          Index,
          NewForms),
    ?ESG:close(),
    update(ATail, Forms, Index1, PSt1, File, FT, progress_step(P));

update([], _Forms, _Index, _PSt, _File, _FT, _P) ->
    %% here we could shut down the progress reporter if needed
    ok.


%% @spec merge(Old::[node()], New::[formData()]) -> [formAction()]
%% @doc Calculates a set of actions that update an `Old' form list to be the
%% same as the forms created from `New'. Tokens to be inserted are stored in
%% the graph during this process.
%% @todo Check if LCS algorithm is better suited. Sometimes it gives much
%% better results (e.g. when a form is moved from the end to the beginning),
%% but it is more costly (however, cutting the common prefix and postfix helps
%% in the usual case). It also needs to be considered wether preprocessor
%% changes can be taken into account with LCS (although a form-based
%% invalidation for preprocessor changes may make it irrelevant).
%%
%% Another note is that when a function is updated, its usually inserted
%% before the old one is deleted. In case of `graph_update', this approach
%% does not really work, analyser modules can't cope with that.

merge(Old, New) ->
    merge(Old, New, []).

merge(Old, [], R) ->
    lists:reverse(R, [{del, Form} || Form <- Old]);
merge([], New, R) ->
    lists:reverse(R, [{ins, store_tokens(Form)} || Form <- New]);
merge([OldForm | Old]=OL, [{NewHash, _} | New]=NL, R) ->
    #form{type=Type, tag=Tag, hash=OldHash} = ?Graph:data(OldForm),
    if
        OldHash =:= NewHash ->
            merge(Old, New, [{hold, OldForm} | R]);
        Type =:= macro;
        Type =:= lex, (Tag =/= store) and (Tag =/= skip) ->
            lists:reverse(R, [{del, F} || F <- OL] ++
                             [{ins, store_tokens(F)} || F <- NL]);
        true ->
            merge_diff(OL, NL, R)
    end.

merge_diff(Old, [{Hash, _}=NewForm | New], R) ->
    case lists:splitwith(fun(F) -> form_hash_neq(F, Hash) end, Old) of
        {_AllDifferent, []} ->
            merge(Old, New, [{ins, store_tokens(NewForm)} | R]);
        {DiffPrefix, [OldForm | Rest]} ->
            merge(Rest, New,
                  [{hold, OldForm} |
                   lists:reverse([{del, F} || F <- DiffPrefix], R)])
    end.

%% @doc Returns whether the given form does NOT have the given hash.
form_hash_neq(Form, Hash) ->
    case ?Graph:data(Form) of
        #form{hash=Hash} -> false;
        _ -> true
    end.


%% @spec hold_prefix(node(), integer()|last, [formAction()]) ->
%%                                            {[node()], [formAction()]}
%% @doc Constructs an action list that holds the first `Index' forms of
%% `File', and continues with `Actions'. Returns the list of real nodes as
%% well, because that will bu used by `update/4'.

hold_prefix(File, Index, Actions) ->
    Prefix = ?Graph:path(File, [{form, {1, Index}}]),
    RealForms = real_forms(File),
    RealSet = sets:from_list(RealForms),
    {RealForms, hold_real_prefix(Prefix, RealSet, Actions)}.

hold_real_prefix([], _, Tail) -> Tail;
hold_real_prefix([Form|Rest], Real, Tail) ->
    case sets:is_element(Form, Real) of
        true  -> [{hold, Form} | hold_real_prefix(Rest, Real, Tail)];
        false -> hold_real_prefix(Rest, Real, Tail)
    end.


%% @spec input_tokens(node() | [node()]) -> inputForm()
%% @doc Returns token data either from an existing form or a list of existing
%% token nodes.

input_tokens(Tokens) when is_list(Tokens) ->
    TokenData = token_data(Tokens),
    {form_hash(Tokens), TokenData};

input_tokens(Form) ->
    #form{hash=Hash} = ?Graph:data(Form),
    {Hash, token_data(?Syn:leaves(Form))}.

token_data(Tokens) ->
    [case ?Graph:data(Token) of
         #lex{type=token, data=D} -> {D, Token}
     end || Token <- Tokens].


%% @spec form_hash([#token{}] | Text) -> term()
%%       Text = [char() | Text]
%% @doc Returns the hash value corresponding to the given text that should be
%% stored for the form.
form_hash(Tokens = [#token{}|_]) ->
    form_hash(
      [[PreWS, Text, PostWS] ||
          #token{text=Text, prews=PreWS, postws=PostWS} <- Tokens]);
form_hash(Text) ->
    erlang:phash2(lists:flatten(Text)).


set_form_hash(Form, Hash) ->
    case ?ESG:data(Form) of
        #form{hash=virtual} -> ok;
        Data ->
            ?ESG:update(Form, Data#form{hash=Hash})
    end.


%%% ============================================================================
%%% Token level manipulation


%% @spec tokenize(string()) -> [formData()]
%% @doc Turns file text into a form list of token lists.
%% @todo lexical error handling
tokenize(Text) ->
    case ?Scanner:string(Text) of
        {ok, Tokens, _Line} ->
            TokenData = [Data || {_Type, _Ln, Data} <- Tokens],
            [{form_hash(Form), Form} ||
                Form <- split_forms(merge_ws(split_stop_tokens(TokenData)))];
        {error, {Ln, Mod, Error}, _Line} ->
            throw({Ln, Mod:format_error(Error)})
    end.

%% @spec store_tokens(formData()) -> inputForm()
%% @doc Store token data in the graph.
%% @todo probably ESG should be used to ensure garbage collection of nodes
store_tokens({Hash, Tokens}) ->
    {Hash,
     [{Token, ?Graph:create(#lex{type=token, data=Token})} || Token <- Tokens]}.


%% @doc Split `stop' tokens into a `stop' and a `ws' or `eol' token.
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

%% @doc Put the contents of `ws' and `eol' tokens into the `prews' and `postws'
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
            if
                Next =:= [] ->
                    [Token#token{postws = Post ++ Eol#token.text} |
                        merge_ws(Head ++ Next ++ Rest)];
                true ->
                    [Token#token{postws = Post ++ join_ws([Eol|Head])} |
                     merge_ws(Next ++ Rest)]
            end
    end.

%% @doc Join the textual contents of tokens.
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
split_first_form([], Form) ->
    {lists:reverse(Form), []};
split_first_form([Head|Tail], Form) ->
    split_first_form(Tail, [Head|Form]).




%%% ============================================================================
%%% File level manipulations

%% @doc Initializes a new file node for the loaded file.
create_file_node(Path, Eol) ->
    Type = case filename:extension(Path) of
               ".erl" -> module;
               _      -> header
           end,
    File = ?ESG:create(#file{type=Type, path=Path, eol=Eol, lastmod=now()}),
    ?ESG:insert(?ESG:root(), file, File),
    ?Graph:mklink(File, incl, File),
    File.

%% private type
%% eol() = {Style, Eof}
%%         Style = {'cr' | 'lf' | 'crlf'}
%%         Eof   = {'eol' | 'noeol'}

%% @spec file_text(string()) -> {string(), eol()}
%% @throws {error, string()}
%% @doc Reads the contents of a file, and returns it in a canonical format.
file_text(Path) ->
    case file:read_file(?File:abs_path(Path)) of
        {ok, BinaryText} -> file_text(BinaryText, "", any);
        {error, Reason}  -> throw({error, file:format_error(Reason)})
    end.

%% @spec (binary(), string(), 'cr'|'lf'|'crlf'|'any') -> {string(), eol()}
%% @doc Canonicalizes the binary file text into a string.
%% <ol>
%%  <li>detects end-of-line marker style</li>
%%  <li>detects whether a final EOL was present</li>
%%  <li>appends a final `~n' EOL if not present</li>
%%  <li>replaces all EOLs found to `~n'</li>
%% </ol>
file_text(<<"\r\n", _/binary>>=Bin, Text, any) -> file_text(Bin, Text, crlf);
file_text(<<"\r",   _/binary>>=Bin, Text, any) -> file_text(Bin, Text, cr);
file_text(<<"\n",   _/binary>>=Bin, Text, any) -> file_text(Bin, Text, lf);

file_text(<<"\r\n">>, Text, crlf) -> {lists:reverse([$\n|Text]), {crlf, eol}};
file_text(<<"\r">>,   Text, cr)   -> {lists:reverse([$\n|Text]), {cr,   eol}};
file_text(<<"\n">>,   Text, lf)   -> {lists:reverse([$\n|Text]), {lf,   eol}};
file_text(<<>>,       Text, Eol)  -> {lists:reverse([$\n|Text]), {Eol,  noeol}};

file_text(<<"\r\n",Tail/binary>>, Text, crlf)-> file_text(Tail,[$\n|Text],crlf);
file_text(<<"\r",  Tail/binary>>, Text, cr)  -> file_text(Tail,[$\n|Text],cr);
file_text(<<"\n",  Tail/binary>>, Text, lf)  -> file_text(Tail,[$\n|Text],lf);
file_text(<<C,     Tail/binary>>, Text, Eol) -> file_text(Tail,[C  |Text],Eol).


%% @spec orig_text(eol(), Text) -> Text
%%       Text = [char() | Text]
%% @doc Restores the original whitespaces to the text stored inside graph
%% tokens.
orig_text(Eol, Text) when is_list(Text) ->
    [orig_text(Eol, El) || El <- Text];
orig_text({cr, _}, $\n)   -> $\r;
orig_text({crlf, _}, $\n) -> "\r\n";
orig_text({lf, _}, $\n)   -> $\n;
orig_text(_, Ch) when Ch >= 0, Ch =< 255 -> Ch;
orig_text(_, _) -> "".


%% @spec real_forms(node()) -> [node()]
%% @doc Returns the forms that are physically present in a file (i.e. skipping
%% the results of file inclusion).
real_forms(File) ->
    ?Graph:path(File, [{form, {hash, '/=', virtual}}]).

%% @spec includers(node()) -> [node()]
%% @doc Returns files that depend on `File'.
includers(File) ->
    %% Probably these files should be sorted in a way that earlier files do
    %% not depend on later files. Currently this naive approach does not cause
    %% problems.
    ?Graph:path(File, [{incl,back}]) -- [File].


%% @doc Returns the output environment data stored in the graph representation.
%% There should be exactly one environment node; if it is not found,
%% `unsafe_save' is thrown.
output_env() ->
    case ?Graph:path(?Graph:root(), [{env, {name, '==', output}}]) of
        [Output] ->
            #env{value = Value} = ?Graph:data(Output),
            Value;
        _  ->
            throw(unsafe_save)
    end.


%%% ============================================================================
%%% Syntax level manipulation


%% @spec parse_form(module | header, referl_preproc:processedForm()) -> node()
parse_form(module, {form, Form}) -> Form;
parse_form(module, {tokens, Tokens}) -> parse(Tokens);
parse_form(module, {vtokens, Dep, Orig, Tokens}) ->
    Form = parse(Tokens),
    ?Graph:mklink(Form, forig, Orig),
    ?Graph:mklink(Form, fdep, Dep),
    Data = ?ESG:data(Form),
    ?ESG:update(Form, Data#form{hash=virtual}),
    Form;
parse_form(header, {form, Form}) -> Form;
parse_form(header, {tokens, [{#token{type=minus}, _},
                             {#token{type=atom,value=record},_}|_]=Tokens}) ->
    parse(Tokens);
parse_form(header, {tokens, Tokens}) ->
    Form = ?ESG:create(#form{type=lex, tag=store}),
    [?ESG:insert(Form, flex, Token) || {_, Token} <- Tokens],
    Form.


%% @spec parse([{#token{}, node()}]) -> node()
%%
%% @doc Parses `Tokens' and returns the result, the root form of the tree.
parse(Tokens) ->
    TokenData =
        [{Type, 1, Data}
            || Data={#token{type = Type}, _} <- Tokens],
    case ?Parser:parse(TokenData) of
        {error, {_Ln, Mod, Msg}} ->
            Error = lists:flatten(
                     io_lib:format("Parse error:~s",
                                     [Mod:format_error(Msg)])),
            error_logger:info_msg("Parser message: ~s~n",
                                     [Mod:format_error(Msg)]),

            Form = ?ESG:create(#form{type=error, tag={1, Error}}),
            [?ESG:insert(Form, flex, N)|| {_, N} <- Tokens],
            Form;
        {ok, Result} -> Result
    end.


%%% ============================================================================
%%% Progress reporter

-record(progress, {op, file, count, max}).

progress_start(Op, Path, Max) ->
    #progress{op=Op, file=Path, count=0, max=Max}.

progress_step(Progress) -> progress_step(Progress, 1).

progress_step(Progress=#progress{op=none}, _) -> Progress;
progress_step(Progress=#progress{op=Op, file=File, count=Count, max=Max}, N) ->
    C = Count+N,
    ?UI:message(progress, {Op, File, C, Max}),
    Progress#progress{count=C}.
