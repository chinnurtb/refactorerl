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
%%% @doc Update context-dependent expression types in attributes:
%%%
%%% <ul>
%%%
%%% <li>`export' attributes contain `arity_qualifier's instead of
%%% `infix_expr's</li>
%%%
%%% <li>`import' attributes contain `arity_qualifier's instead of
%%% `infix_expr's</li>
%%%
%%% </ul>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

%%% Changelog:

-module(anal_attrib).
-vsn("$Rev: 1206 $").

%% Callback exports
-export([init/0, insert/5, remove/5]).

-include("refactorerl.hrl").


%% @private
init() ->
    [].

%% @private
insert(List, #expr{kind=list}, sub,
       Expr, Data=#expr{kind=infix_expr, value='/'}) ->
    case [?GRAPH:data(A) || A <- ?GRAPH:path(List, [{attr, back}])] of
        [#form{type=attrib, tag=Kind}] when Kind == export; Kind == import ->
            ?GRAPH:update(Expr, Data#expr{kind=arity_qualifier,
                                          value=undefined});
        _ ->
            ok
    end;

insert(_, _, _, _, _) ->
    ok.

%% @private
remove(_, _, _, _, _) ->
    ok.
