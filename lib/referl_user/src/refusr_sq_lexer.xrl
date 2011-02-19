Definitions.

Whitespace = [\t\s]
Lower      = [a-z]
Upper      = [A-Z]
Digit      = [0-9]

Rules.

{Digit}+ :
    {token, {int, TokenLine, list_to_integer(TokenChars)}}.

andalso|\, :
    {token, {'and', TokenLine}}.

orelse|\; :
    {token, {'or', TokenLine}}.

({Lower}|@)({Lower}|{Upper}|{Digit}|_|@)* :
    Atom = list_to_atom(TokenChars),
    {token, case reserved_word(Atom) of
                true  -> {Atom, TokenLine};
                false -> {atom, TokenLine, Atom}
            end}.
'[^']*' :
    Atom = list_to_atom(lists:sublist(TokenChars, 2, TokenLen-2)),
    {token, case reserved_word(Atom) of
                true  -> {Atom, TokenLine};
                false -> {atom, TokenLine, Atom}
            end}.

\"[^\"]*\" :
    String = lists:sublist(TokenChars, 2, TokenLen-2),
    {token, {string, TokenLine, String}}.

[\[\]\.\+\{\}\(\)\:] :
    {token, {list_to_atom(TokenChars), TokenLine}}.

(/=|==|\>=|=\<|\<|\>) :
    {token, {comparator, TokenLine, list_to_atom(TokenChars)}}.

{Whitespace}+ :
    skip_token.

Erlang code.

reserved_word('and') -> true;
reserved_word('or') -> true;
reserved_word('not') -> true;
reserved_word('in') -> true;
reserved_word('like') -> true;
reserved_word(_) -> false.
