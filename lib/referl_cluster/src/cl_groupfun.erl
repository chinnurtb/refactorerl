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

%%% @doc This module contains functions that compute the distance of a newly
%%% created entity from other entities. The new entity is created by merging
%%% two originial entities. The functions have the following 6 arguments:
%%%
%%% <ol>
%%%     <li>Size of the first original entity</li>
%%%     <li>Size of the second original entity</li>
%%%     <li>Size of the other entity</li>
%%%     <li>Distance of the two original entities</li>
%%%     <li>Distance of the first original and the other entity</li>
%%%     <li>Distance of the second original and the other entity</li>
%%% </ol>
%%%
%%% The functions return the distance of the new, merged entity and the other
%%% entity.
%%%
%%% @author Aniko Nagyne Vig <viganiko@inf.elte.hu>
%%% @author Krisztian Toth <ccc@inf.elte.hu>

-module(cl_groupfun).
-vsn("$Rev: 3185 $").

-export([single/6, complete/6, uwaverage/6, waverage/6, centroid/6, median/6,
         ward/6]).

%% Argument usage note: functions calculate the value of d(R, P+Q) from |P|,
%% |Q|, |R|, d(P,Q), d(R,P), and d(R,Q), this is the origin of variable names.

%% @doc Single linkage distance.
single(_, _, _, _, DRP, DRQ) ->
    (DRP+DRQ-abs(DRP-DRQ))/2.

%% @doc Complete linkage distance.
complete(_, _, _, _, DRP, DRQ) ->
    (DRP+DRQ+abs(DRP-DRQ))/2.

%% @doc Unweighted average linkage distance.
uwaverage(_, _, _, _, DRP, DRQ) ->
    (DRP+DRQ)/2.

%% @doc Weighted average linkage distance.
waverage(NP, NQ, _, _, DRP, DRQ) ->
    NP/(NP+NQ)*DRP+ NQ/(NP+NQ)*DRQ.

%% @doc Centroid distance.
centroid(NP, NQ, _, DPQ, DRP, DRQ) ->
    NP/(NP+NQ)*DRP + NQ/(NP+NQ)*DRQ - NP*NQ/math:pow((NP+NQ),2)*DPQ.

%% @doc Median distance.
median(_, _, _, DPQ, DRP, DRQ) ->
    DRP/2 + DRQ/2 - DPQ/4.

%% @doc Ward distance.
ward(NP, NQ, NR, DPQ, DRP, DRQ) ->
    (NP+NR)/(NP+NQ+NR)*DRP + (NQ+NR)/(NP+NQ+NR)*DRQ - NR/(NP+NQ+NR)*DPQ.
