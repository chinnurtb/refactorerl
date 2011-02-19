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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc This transformation upgrades the calls of the old regular
%%% expression module `regexp' to the new `re' module. The
%%% transformation is based on a generic interface upgrader module,
%%% which is invoked with the right structural and semantical change
%%% descriptors.
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>Patterns belong to applications to be updated should match
%%%   at least one of the patterns written in the change
%%%   descriptors.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li> Updating the function application:
%%%     <ul>
%%%       <li> Modifying the called function's identifier
%%%            <pre>`regexp:match' -> `re:run'</pre></li>
%%%       <li> Matching the old function arguments and creating the new
%%%            argument list
%%%            <pre>`(String, RE)' -> `(String, RE, [{capture, first}])'
%%%            </pre></li>
%%%     </ul></li>
%%%   <li> Finding the patterns to be updated and performing on them the
%%%   matching and the replacing. Futhermore, applying the correction
%%%   functions on the variable bindings or/and usages.
%%%   <pre>Pattern: `{match, 1, Len}' -> `{match, [{0, Len}]}'</pre>
%%%   <pre>Expression: `use(St, Len)' -> `use(St+1, Len)'</pre></li>
%%%   <li> Turning the case expression to a try expression and moving error
%%%   pattern to the catch block (`error:badarg' is moved to a catch
%%%   block after we changed case expression to try expression).</li>
%%% </ol>
%%%
%%% == Implementation details ==
%%%
%%% <ul>
%%%   <li> The change descriptors are given as strings and have to be
%%%   scanned and parsed. We use the standard erlang parser</li>
%%%   <li> After the parsing the terms are converted to an abstract
%%%   representation witch becomes annoted with the transformation
%%%   descriptors and the helper descriptors are injected into the referer
%%%   descriptors too. We have now a list of abstract change descriptors
%%%   that represents the whole interface change</li>
%%%   <li> When the function to be upgraded has no instance in the
%%%   database, the transformation has nothing to do, otherwise the
%%%   applications of the function are found</li>
%%%   <li> The applications are updated to use the new argument structure</li>
%%%   <li> Patterns to be updated are found based on the applications</li>
%%%   <li> These patterns around the applications are tried to match with
%%%   the abstract structure and the stored graph nodes that belong to
%%%   variables in the abstract pattern descriptors are stored</li>
%%%   <li> Based on change descriptors annoted with graph nodes the new
%%%   patterns can be created and replaced with the old ones</li>
%%%   <li> At the end of transformation the local transformation functions
%%%   (e.g. +1) are executed on the graph nodes and patterns may be putted
%%%   from case clause to try-catch clause</li>
%%% </ul>
%%%
%%% == Implementation status ==
%%%
%%% The transformation has been implemented with simplifications. This
%%% means that the main structure of the transformation is complete, but
%%% the result is not fully correct in all the cases. Its defect is
%%% similar to the problem seen by ``Introduce record'': we have to use
%%% data flow analysis to produce correct result in every case, because
%%% the application of the structure changes and user-defined
%%% transformations requires the following of the variable values.
%%%
%%% At this time the following patterns are detected:
%%% <ul>
%%%   <li> When the function application/list variable is in the head of a
%%%   case block, we analyse the case clause patterns.</li>
%%%   <li> When the function application/list variable is on the left or
%%%   the right hand side of a match expression, we analyse the other
%%%   side.</li>
%%%   <li> When a list variable is used in a list generator, we analyse the
%%%   left hand side of the genarator.</li>
%%% </ul>
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(reftr_upgrade_regexp).
-vsn("$Rev: 5733 $").
-compile([export_all]).

-export([prepare/1]).

-include("user.hrl").

-define(IFace, reftr_upgrade_iface).
%@todo bkil: I propose you use the macro instead of this import
-import(?IFace, [simple_infix_expr/1]).

-define(ErrorCD, {"{error, Reason}", "catch error:badarg"}).
%% regexp.erl:
%% format_error({illegal,What}) ->
%%        ["illegal character `",What,"'"];
%% format_error({unterminated,What}) ->
%%        ["unterminated `",What,"'"];
%% format_error({char_class,What}) ->
%%        ["illegal character class ",io_lib:write_string(What)].
-define(ErrorCDDefVal, {"{error, {illegal, \" or unterminated part "
                        "ruined the regular expression\"}}",
                        "catch error:badarg"}).

%% -----------------------------------------------------------------------------
%% Callback

%% @private
prepare(_) ->
    [fun() -> init end] ++
    [fun(_) -> ?IFace:do(?MODULE:F()) end || F <- cds()] ++
    [fun(U) -> U end].


%% -----------------------------------------------------------------------------
%% List of change descriptor funs

%% @private
cds() ->
    [Fun || {Fun, Arity} <- ?MODULE:module_info(exports),
            Arity =:= 0,
            lists:prefix("cd_", atom_to_list(Fun))].

%% -----------------------------------------------------------------------------
%% format_error/1

%% @private
cd_format_error() ->
    {{fun regexp:format_error/1, "\"Bad Regular Expression\""}, [], [], []}.

%% -----------------------------------------------------------------------------
%% match/2

%% @private
cd_match() ->
    {{fun regexp:match/2, fun re:run/3},

     %% Change desctiptors

     [{"Str, RE",         "Str, RE, [{capture, first}]"},
      {"{match, S, L}",   "{match, [{decr(S), L}]}"},
      {"nomatch",         "nomatch"},
      ?ErrorCD,

      %% For default value

      ?ErrorCDDefVal],

     %% Helpers

     [],

     %% Transforms

     [{decr, simple_infix_expr("P - 1"), simple_infix_expr("P + 1")}]
    }.

%% -----------------------------------------------------------------------------
%% first_match/2

%% @private
cd_first_match() ->
    {{fun regexp:first_match/2, fun re:run/3},

     %% Change desctiptors

     [{"Str, RE",         "Str, RE, [{capture, first}]"},
      {"{match, S, L}",   "{match, [{decr(S), L}]}"},
      {"nomatch",         "nomatch"},
      ?ErrorCD,

      %% For default value

      ?ErrorCDDefVal],

     %% Helpers

     [],

     %% Transforms

     [{decr, simple_infix_expr("P - 1"), simple_infix_expr("P + 1")}]
    }.

%% -----------------------------------------------------------------------------
%% matches/2

%% @private
cd_matches() ->
    {{fun regexp:matches/2, fun re:run/3},

     %% Change desctiptors

     [{"Str, RE",           "Str, RE, [global, {capture, first}]"},

      %%{"{match, []}",       "nomatch"},
      %%{"{match, Matches}",  "{match, map(melem, Matches)}"},

      [{"{match, Matches = []}",       "nomatch"},
       {"{match, Matches = _}",            "{match, map(melem, Matches)}"}
      ],

      ?ErrorCD,

      %% For default value

      ?ErrorCDDefVal],

     %% Helpers

     [{melem, [{"{S, L}", "[{decr(S), L}]"}]},
      {mlist, [{"[]",      "nomatch"},
               {"Matches2", "map(melem, Matches2)"}]}],
     %%],
     %% Transforms

     [{decr, simple_infix_expr("P - 1"), simple_infix_expr("P + 1")}]
    }.

%% -----------------------------------------------------------------------------
%% gsub/3

%% @private
cd_gsub() ->
    {{fun regexp:gsub/3,  fun re:replace/4},

     %% Change desctiptors

     [{"Str, RE, New",    "Str, RE, New, [{return, list}, global]"},
      {"{ok, NS, RC}",    "NS"},
      ?ErrorCD,

      %% For default value

      ?ErrorCDDefVal],

     %% Helpers

     [],

     %% Transforms

     []
    }.

%% -----------------------------------------------------------------------------
%% sub/3

%% @private
cd_sub() ->
    {{fun regexp:sub/3,   fun re:replace/4},

     %% Change desctiptors

     [{"Str, RE, New",    "Str, RE, New, [{return, list}]"},
      {"{ok, NS, RC}",    "NS"},
      ?ErrorCD,

      %% For default value

      ?ErrorCDDefVal],

     %% Helpers

     [],

     %% Transforms

     []
    }.

%% -----------------------------------------------------------------------------
%% split/2 special case

%% @private
cd_split_space() ->
    {{fun regexp:split/2, fun re:split/3},

     %% Change desctiptors

     [{"Str, \" \"",      "Str, \"[ \\t\\n]+\", [{return, list}, trim]"},
      {"{ok, Res}",       "Res"},
      ?ErrorCD,

      %% For default value

      ?ErrorCDDefVal],

     %% Helpers

     [],

     %% Transforms

     []
    }.

%% -----------------------------------------------------------------------------
%% split/2

%% @private
cd_split() ->
    {{fun regexp:split/2, fun re:split/3},

     %% Change desctiptors

     [{"Str, RE",         "Str, RE, [{return, list}]"},
      {"{ok, Res}",       "Res"},
      ?ErrorCD,

      %% For default value

      ?ErrorCDDefVal],

     %% Helpers

     [],

     %% Transforms

     []
    }.

%% -----------------------------------------------------------------------------
%% parse/1

%% @private
cd_parse() ->
    {{fun regexp:parse/1, fun re:compile/1},

     %% Change desctiptors

     [],
     %% [{"RE",              "RE"},
     %%  {"{ok, Res}",       "{ok, Res}"},
     %%  {"{error, Reason}", "{error, Reason}"}],

     %% Helpers

     [],

     %% Transforms

     []
    }.
