Definitions.

D   = [0-9]
L   = ([a-z]+[A-Za-z0-9_]*)
WhiteS  = ([\000-\s]|%.*)

Rules.

show       : {token,{show,TokenLine,list_to_atom(TokenChars)}}.
for        : {token,{for,TokenLine,list_to_atom(TokenChars)}}.
save       : {token,{save,TokenLine,list_to_atom(TokenChars)}}.
compare    : {token,{compare,TokenLine,list_to_atom(TokenChars)}}.
module     : {token,{module,TokenLine,list_to_atom(TokenChars)}}.
function   : {token,{function,TokenLine,list_to_atom(TokenChars)}}.

metrics    : {token,{metrics,TokenLine,list_to_atom(TokenChars)}}.

filters    : {token,{filters,TokenLine,list_to_atom(TokenChars)}}.
sum|max|min|fmaxname|maxlist|avg|tolist :
   {token,{fil,TokenLine,list_to_atom(TokenChars)}}.
'{L}+'     : S = strip(TokenChars,TokenLen),
             {token,{string,TokenLine,S}}.
{L}+       : {token,{func,TokenLine,list_to_atom(TokenChars)}}.
{D}+       : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
[(),{}]    : {token,{list_to_atom(TokenChars),TokenLine}}.
{WhiteS}+  : skip_token.

Erlang code.

strip(TokenChars, TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).
