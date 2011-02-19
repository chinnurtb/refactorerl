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

%%% ============================================================================
%%% Module information

%%% @doc Generates a list of variable names and atoms that are close to each
%%% other in a string metric (Levenshtein or modified soundex).
%%% Usage:
%%%  searchFunction("FuncName")
%%%  searchByPos("path",Position)
%%%  searchByPos

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>

-module(refusr_strm).
-vsn("$Rev: 5375 $"). %for emacs"



-include("user.hrl").

% removed because these functions are unnecessary
% -export([ searchByPos/2, searchByPos/3, searchFunction/1, searchFunction/2 ]).
-export([ getDistance/2 ]).
-export([ levenshtein/2 , soundex/1 ]).

%% ============================================================================

%% @spec getDistance(string(), string()) -> {'lev', integer()}
%% @doc Calculates the distance between two strings
getDistance([],X) when is_list(X) ->
    {'lev', levenshtein([],X)};
getDistance(X,[]) when is_list(X) ->
    {'lev', levenshtein(X,[])};
getDistance(X,Y) when is_list(X) and is_list(Y) ->
    {'lev', levenshtein(X,Y)}.

%% renamed for debug purposes
%% getDistance2(X,Y) when (is_list(X) and is_list(Y)) ->
%%    A=levenshtein(X,Y),
%%    B=levenshtein(soundex(X),soundex(Y)),
%%    case A<B of
%%        true ->
%%            {lev,A};
%%        _ ->
%%            {sou,B}
%%    end.

%% ============================================================================


%% @spec levenshtein(string(), string()) -> integer()
%% @doc Calculates the Levenshtein distance between two strings
levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% @private
%% Recurses over every character in the source string
%% and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
    levenshtein_rec(SrcTail,
                    Target,
                    levenshtein_distlist(Target,
                                         DistList,
                                         SrcHead,
                                         [Step],
                                         Step),
                    Step + 1);
levenshtein_rec([], _, DistList, _) ->
    lists:last(DistList).

%% @private
%% Generates a distance list with distance values for every character
%% in the target string
levenshtein_distlist([TargetHead|TargetTail],
                     [DLH|DLT],
                     SourceChar,
                     NewDistList,
                     LastDist) when length(DLT) > 0 ->
    Min = lists:min([LastDist + 1,
                     hd(DLT) + 1,
                     DLH + dif(TargetHead, SourceChar)]),
    levenshtein_distlist(TargetTail,
                         DLT,
                         SourceChar,
                         NewDistList ++ [Min],
                         Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
    NewDistList.

%% @private
%% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_C1, _C2) -> 1.


%% @spec soundex(string()) -> list()
%% @doc Returns the soundex numeric code of a string. The generation algorithm
%% is the following.
%% <ol>
%%    <li>Turn string to uppercase and separate the first letter</li>
%%    <li> Make the character changes to the uppercase string made in
%%           fun changeLetters/1. </li>
%%    <li>Make the remain string uniq</li>
%%    <li>Leave out zeros</li>
%% </ol>

soundex(Input) ->
    [Head|Tail] = string:to_upper( Input),
    Post = uniq(lists:map(fun changeLetters/1,Tail)),
    [Head|lists:filter(fun(X) -> X /= 0 end,
                       lists:flatten(Post))].


%% @private
changeLetters(C) when (C == $A);(C==$E);(C==$I);(C==$O);
                      (C==$U);(C==$H);(C==$H);(C==$Y)->
    0;
changeLetters(C) when (C==$B);(C==$F);(C==$P);(C==$V) ->
    1;
changeLetters(C) when (C==$C);(C==$G);(C==$J);(C==$K);
                      (C==$Q);(C==$S);(C==$X);(C==$Z) ->
    2;
changeLetters(C) when (C==$D);(C==$T) ->
    3;
changeLetters(C) when (C==$L) ->
    4;
changeLetters(C) when (C==$M);(C==$N) ->
    5;
changeLetters(C) when (C==$R) ->
    6;
changeLetters(_) ->
    {error, not_meant_to_be_implemented}.

%% @private
uniq([]) ->
    [];
uniq([X]) ->
    [X];
uniq([Head|Tail]) ->
    uniqacc(Head,Tail).

uniqacc(Head,[Head|Tail]) ->
    uniqacc(Head,Tail);
uniqacc(Head,[]) ->
    [Head];
uniqacc(Head,[NotHead|Tail]) ->
    [Head,uniq([NotHead|Tail])].


%% Deprecated functions

% searchByPos(File, Pos) ->
%     searchByPos(File, Pos, 1).
%
% searchByPos(File, Pos, Limit) ->
%     try ?Args:variable([{file, File}, {position, Pos}]) of
%         Var -> getVar(Var,Limit)
%     catch
%         {_, pos_bad_type, [variable, _ ]} ->
%             try ?Args:function([{file, File}, {position, Pos}]) of
%                 Fun -> getFun( atom_to_list(?Fun:name(Fun)) , Limit)
%             catch
%                 _ -> {error, not_fun}
%             end;
%         _ -> {error, not_var_fun}
%     end.
%
% getVar(Var,Limit) ->
%     VarName = ?Var:name(Var),
%     Scopes = ?Query:exec([Var], ?Query:seq(?Var:scopes(),
%                                            ?Clause:variables())),
%     DistList = [{VarName,getDistance(VarName, ?Var:name(X))}|| X<-Scopes],
%     lists:filter(fun({_, {_, N}}) ->
%                              N =< Limit
%                  end,
%                      DistList).
%
% searchFunction(Name) when is_list(Name) ->
%     getFun(Name, 1).
%
% searchFunction(Name, Limit) when is_list(Name) ->
%     getFun(Name, Limit).
%
% getFun(FunName, Limit) when is_list(FunName) ->
%     Functions = ?Query:exec(?Query:seq(?Mod:all(),?Mod:locals())),
%     DistList = lists:map(fun(Func) ->
%                                  FuncName = ?Fun:name(Func),
%                                  {FuncName,
%                                   getDistance(FunName,
%                                               atom_to_list(FuncName))}
%                          end, Functions),
%     lists:filter(fun({_, {_, N}}) ->
%                              N =< Limit
%                  end,
%                      DistList).





