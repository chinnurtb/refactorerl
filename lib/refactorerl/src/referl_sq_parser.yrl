Nonterminals s query_sequence query selector filter_seq filter rv.
Terminals atom int re comp '[' ']' 'and' 'or' 'not' '.' '{' '}' '+' '(' ')'.
Rootsymbol s.

Left  100 'and'.
Left  200 'or'.
Unary 300 'not'.

s               -> query_sequence                      : '$1'.

query_sequence  -> query                               : ['$1'].
query_sequence  -> query '.' query_sequence            : ['$1'| '$3'].

query           -> '{' query '}' int                   : {element(3, '$4'),
                                                          '$2'}.
query           -> '{' query '}' '+'                   : {'+', '$2'}.
query           -> selector                            : [{selector, '$1'}].
query           -> selector '[' filter_seq ']'         : [{selector, '$1'},
                                                          {filter, '$3'}].

selector        -> atom                                : element(3, '$1').

filter_seq      -> '(' filter_seq ')'                  : '$2'.
filter_seq      -> 'not' filter_seq                    : {'not', '$2'}.

filter_seq      -> '(' filter_seq ')' 'and' filter_seq : {'and', '$2', '$5'}.
filter_seq      -> '(' filter_seq ')' 'or' filter_seq  : {'or', '$2', '$5'}.

filter_seq      -> filter 'and' filter_seq             : {'and', '$1', '$3'}.
filter_seq      -> filter 'or' filter_seq              : {'or', '$1', '$3'}.
filter_seq      -> filter                              : '$1'.

filter          -> atom                                : element(3, '$1').
filter          -> atom comp rv                        : {element(3, '$2'),
                                                          element(3, '$1'),
                                                          '$3'}.

rv              -> atom                                : element(3, '$1').
rv              -> int                                 : element(3, '$1').
rv              -> re                                  : '$1'.
