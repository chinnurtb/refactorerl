Definitions.

WS = [\t\s]
L  = [a-z]
D  = [0-9]

Rules.

{D}+                  : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
({L}|@)({L}|{D}|_)*   : Atom = list_to_atom(TokenChars),
                        {token, case reserved_word(Atom) of
                                    true  -> {Atom, TokenLine};
                                    false -> {atom, TokenLine, Atom}
                                end}.
\"[^\"]*\"            : {token, {re,
                                 TokenLine,
                                 lists:sublist(TokenChars, 2, TokenLen - 2)}}.
[\[\]\.\+\{\}\(\)]    : {token, {list_to_atom(TokenChars), TokenLine}}.
(/=|==|\>=|=\<|\<|\>) : {token, {comp, TokenLine, list_to_atom(TokenChars)}}.
{WS}+                 : skip_token.

Erlang code.

reserved_word('and') -> true;
reserved_word('or') -> true;
reserved_word('not') -> true;
reserved_word('in') -> true;
reserved_word(_) -> false.
