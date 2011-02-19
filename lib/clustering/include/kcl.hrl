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

-include("cluster.hrl").

%%% ============================================================================
%%% Modules

-define(ClState,    kcl_state).
-define(ClAttr,     kcl_attr).
-define(ClGroup,    kcl_group).
-define(ClDm,       kcl_dm).
-define(MHT,        multi_hashtree).



%%% ============================================================================
%%% Types

%% @type clItem().
%%
-record(clItem, {
    id,             % term()
    type,           % atom()
    name,           % term()
    props = []      % [{atom(), term()}]
    }).

%% @type clCluster().
%% 
-record(clCluster, {
    id,             % term()
    type,           % atom()
    mode,           % open | close
    name,           % term()
    props = []      % list()
    }).

%% @type clState() = {clState, 
%%          items = tid(),
%%          attr_matrix = matrix(),
%%          conn_matrix = matrix(),
%%          conn_sym = bool()
%%          clusters  = tid(),
%%          cl_members = tid(),
%%          cl_deps = matrix(),
%%          cl_dist = matrix()}.
%% A record that hold the state of a clustering process.
%%
%% The record fields:
%% <ul>
%%   <li>`items': an ETS set table identifier (tid()) that contains the items of
%%     the clustering process in the `{ItemId::term(), Item::{@type clItem()}}' 
%%     format where `ItemId' is the identifier of the `Item'.</li>
%%   <li>`attr_matrix': the attribute matrix. It can be understanded as 
%%     dependency matrix too. The row labels are the entitie identifiers and 
%%     the column labels are the attribute identifiers.</li>
%%   <li>`conn_matrix': a matrix which contains the calculated
%%     distance between entities. The row and column labels are the 
%%     entitie identifiers.</li>
%%   <li>`conn_sym': indicate that if the connection matrix is symmetric. It it
%%     is true the connection matrix is an upper triangle matrix.</li>
%%   <li>`clusters': an ETS set table idetifier (tid()) that contains the 
%%     actually computed clusters. It contains {@type clCluster()} elements.
%%     </li>
%%   <li>`cl_members': an ETS bag table idetifier (tid()) that contains the
%%     membership of entities and clusters in {ClusterId, EntId} format.</li>
%%   <li>`cl_deps': The cluster distance matrix contains the calculated 
%%     distance between actual clusters. The row and column labels are the 
%%     cluster identifiers.</li>
%%   <li>`cl_dist': a matrix which contains the calculated distance between 
%%     clusters. The row and column labels are the identifiers of clusters.</li>
%% </ul>
-record(clState, {
    items,              % ETS set
    attr_matrix,        % matrix()
    conn_matrix,        % matrix()
    conn_sym,           % bool()
    clusters,           % ETS set
    cl_members,         % ETS bag
    cl_deps,            % matrix()
    cl_dist,            % matrix()
    etc                 % ETS set, private
    }).

