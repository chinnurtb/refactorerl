Nonterminals semantic_query query_seq query statistics
             initial_selection selection iteration closure
             filter exp100 exp200 exp300 exp400 exp_max property.

Terminals    '.' '[' ']' '{' '}' '(' ')' '+' ':'
             atom int string comparator
             'not' 'and' 'or' 'in' 'like'.

Rootsymbol   semantic_query.


semantic_query -> initial_selection
                  : [{initial_selection, '$1'}].
semantic_query -> initial_selection '.' query_seq
                  : [{initial_selection, '$1'}| '$3'].

query_seq      -> query                   : ['$1'].
query_seq      -> query ':' statistics    : ['$1', '$3'].
query_seq      -> query '.' query_seq     : ['$1'| '$3'].

statistics     -> atom                    : {statistics, [element(3, '$1')]}.

query          -> selection               : {selection, '$1'}.
query          -> iteration               : {iteration, '$1'}.
query          -> closure                 : {closure, '$1'}.


initial_selection -> atom
                  : [{initial_selector, element(3, '$1')}].
initial_selection -> atom '[' filter ']'
                  : [{initial_selector, element(3, '$1')}, {filter, '$3'}].

%% Properties are atoms too, that case can't be handled separately here.
selection -> atom
             : [{selector, element(3, '$1')}].
selection -> atom '[' filter ']'
             : [{selector, element(3, '$1')}, {filter, '$3'}].

iteration -> '{' query_seq '}' int
             : [{iteration, {query_seq, '$2'}, {mult, element(3, '$4')}}].
iteration -> '{' query_seq '}' int '[' filter ']'
             : [{iteration, {query_seq, '$2'}, {mult, element(3, '$4')}},
                {filter, '$6'}].

closure   -> '(' query_seq ')' '+'
             : [{closure, {query_seq, '$2'}, {mult, infinite}}].
closure   -> '(' query_seq ')' '+' '[' filter ']'
             : [{closure, {query_seq, '$2'}, {mult, infinite}}, {filter, '$6'}].
closure   -> '(' query_seq ')' int
             : [{closure, {query_seq, '$2'}, {mult, element(3, '$4')}}].
closure   -> '(' query_seq ')' int '[' filter ']'
             : [{closure , {query_seq, '$2'}, {mult, element(3, '$4')}},
                {filter, '$6'}].


filter   -> exp100                       : '$1'.
%%Disjunction
exp100   -> exp200 'or' exp100           : {'or', '$1', '$3'}.
exp100   -> exp200                       : '$1'.
%%Conjunction
exp200   -> exp300 'and' exp200          : {'and', '$1', '$3'}.
exp200   -> exp300                       : '$1'.
%%Comparison
exp300   -> exp400 comparator exp400     : {element(3, '$2'), '$1', '$3'}.
exp300   -> exp400 'like' exp400         : {'like', '$1', '$3'}.
exp300   -> exp400                       : '$1'.
%%Negation
exp400   -> 'not' exp_max                : {'not', '$2'}.
exp400   -> exp_max                      : '$1'.
%%Parenthesis
exp_max  -> '(' exp100 ')'               : '$2'.
%%Embedded queries
exp_max  -> property 'in' '.' query_seq  : {'in', '$1', {query_seq, '$4'}}.
exp_max  -> '.' query_seq                : {query_seq, '$2'}.
%%Simple values
exp_max  -> atom                         : element(3, '$1').
exp_max  -> int                          : element(3, '$1').
exp_max  -> string                       : element(3, '$1').

property -> atom                         : element(3, '$1').
