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
%%% Module Informations

%%% @doc Library of connection/similarity/distance functions between
%%% entities/attributes.
%%%
%%% == TODO ==
%%% Write/copy other connection functions.

%%% @todo Write/copy other connection functions.

%%% @author Kornel Horvath <kornel@inf.elte.hu>


-module(ucl_connection).
-vsn("$Rev:  $").

-include("ucluster.hrl").


%%% ============================================================================
%%% Imports/exports

% Similarty measures
-export([common_attr_cnt/3, common_ent_cnt/3]).



%%% ============================================================================
%%% Building connection matrix

%% @spec common_attr_cnt(Ent1::clItem(), Ent2::clItem(), AttrMatrix::matrix())
%%           -> ConnResult
%%       ConnResult = {def, any()} | {val, ConnVal::term()}
%% @doc Number of the common attributes of `Ent1' and `Ent2' entities.
%%      If there are no common attributes return a `{def, 0}' tuple otherwise
%%      a `{val, NumberOfCommonAtribuetes}' tuple.
common_attr_cnt(#clItem{id=Id1}, #clItem{id=Id2}, AttrMatrix) ->
    case length(?MISC:intersect(?Matrix:get_row_non_def_ids(Id1, AttrMatrix),
            ?Matrix:get_row_non_def_ids(Id2, AttrMatrix))) of
        0 -> {def,   0};
        N -> {value, N}
    end.


%% @spec common_ent_cnt(Attr1::clItem(), Attr2::clItem(), AttrMatrix::matrix())
%%           -> ConnResult
%%       ConnResult = {def, any()} | {val, ConnVal::term()}
%% @doc Number of the common entities of `Attr1' and `Attr2' attributes.
%%      If there are no common entities return a `{def, 0}' tuple otherwise
%%      a `{val, NumberOfCommonEntities}' tuple.
common_ent_cnt(#clItem{id=Id1}, #clItem{id=Id2}, AttrMatrix) ->
    case length(?MISC:intersect(?Matrix:get_col_non_def_ids(Id1, AttrMatrix),
            ?Matrix:get_col_non_def_ids(Id2, AttrMatrix))) of
        0 -> {def,   0};
        N -> {value, N}
    end.



