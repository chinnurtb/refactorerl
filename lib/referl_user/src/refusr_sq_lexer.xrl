Definitions.

WS = [\t\s]
L  = [a-z]
U  = [A-Z]
D  = [0-9]

Rules.

{D}+                    : {token, {int,
                                   TokenLine,
                                   list_to_integer(TokenChars)}}.
({L}|@)({L}|{U}|{D}|_)* : Atom = list_to_atom(TokenChars),
                          {token, case reserved_word(Atom) of
                                      true  -> {Atom, TokenLine};
                                      false -> {atom, TokenLine, Atom}
                                  end}.
'[a-zA-Z<>=]*'          : {token, {atom, TokenLine, list_to_atom(erlang:tl(lists:reverse(erlang:tl(TokenChars))))}}.
\"[^\"]*\"              : String = lists:sublist(TokenChars, 2, TokenLen-2),
                          {token, {string, TokenLine, String}}.
[\[\]\.\+\{\}\(\)\:]    : {token, {list_to_atom(TokenChars),
                                   TokenLine}}.
(/=|==|\>=|=\<|\<|\>)   : {token, {comparator,
                                   TokenLine,
                                   list_to_atom(TokenChars)}}.
{WS}+                   : skip_token.

Erlang code.

reserved_word('and') -> true;
reserved_word('or') -> true;
reserved_word('not') -> true;
reserved_word('in') -> true;
reserved_word('like') -> true;
reserved_word(_) -> false.
