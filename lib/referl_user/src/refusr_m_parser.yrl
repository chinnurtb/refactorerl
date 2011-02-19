Nonterminals 
predicates predicate help list element elements te1 te2 te3 tuples telements funlist tuple.

Terminals '(' ')' ',' '{' '}'
atom show for compare save module integer string function func fil metrics filters.

Rootsymbol predicates.

predicates -> help : '$1'.

predicates -> predicate : '$1'.
predicates -> predicate fil: {'$1', unpack('$2')}.

help -> show metrics:
        {show, metrics}.
help -> show filters:
        {show, filters}.

predicate -> show func for function funlist:
            {show, unpack('$2'), function, unpack('$5')}.

predicate -> show func for module list:
            {show, unpack('$2'), module, unpack('$5')}.

predicate -> save func for tuple:
            {save, unpack('$2'), unpack('$4')}.

predicate -> save func for string:
            {save, unpack('$2'), unpack('$4')}.

predicate -> compare module list:
            {compare, module, unpack('$3')}.

predicate -> compare function funlist:
            {compare, function, unpack('$3')}.

funlist -> '(' ')' : nil.
funlist -> '(' tuples ')' : {funlist, '$2'}.

list -> '(' ')' : nil.
list -> '(' elements ')' : {list, '$2'}.

tuples -> tuple : ['$1'].
tuples -> tuple ',' tuples : ['$1'] ++ '$3'.

elements -> element : ['$1'].
elements -> element ',' elements : ['$1'] ++ '$3'.
element -> atom : '$1'.
element -> string : unpack('$1').

tuple -> '{' telements '}' : '$2'.

telements -> te1 ',' te2 ',' te3 : {'$1', '$3', '$5'}.

te3 -> integer : unpack('$1').
te2 -> string : unpack('$1').
te1 -> string : unpack('$1').

Erlang code.

unpack({_,_,V}) -> V;
unpack({_,V})-> V.
