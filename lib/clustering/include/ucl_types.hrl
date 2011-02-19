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
%%% Types

%% @type clItem() = {clItem,
%%           id::term(),
%%           type::atom(),
%%           name::string(), 
%%           props::[{Key::atom(), Value::term()}]}.
%%
%% Unified item type of the clustering. It is a record type.
%% All object are used in the clustering process may be stored in a `clItem' 
%% record. This unified format allow to the clustering algorithms to handle
%% the entities and attributes of a concrete task in same way independently 
%% to the the real source of the datas.
%%
%% An `clItem' record store information about the object itself not about the
%% role of it in the clustering process. For example if we cluster modules the
%% items conatain informations about the modules. Name, source file and so on.
%% But these don't contain information about the modules are entitites or 
%% attributes. A module can be entity and attribute simultaneously.
%% Informations about when the module is entity or attribute is stored in a
%% {@type clState()} record.
%%
%% The fields:
%% <ul>
%%   <li>`id': The unique identifier of the object. It must be unique
%%     between all objects are stored in `clItem' records because the records
%%     are identified only by their `id' fields.</li>
%%   <li>`type': The type of the data stored in `clItem'. It can be an
%%     arbitrary atom. For example: if we cluster modules the entities can
%%     be module type nodes of the ESG. Than the `type' field let be `module'.
%%     </li>
%%   <li>`name': the name of the item. It will be used when the item 
%%     is writed out and the name is more suitable instead the `clItem'.
%%     For examle the nem of the module.</li>
%%   <li>`props': Any other informations about the item can be stored in this
%%     property list. The base modules of the clustering infrastucture never
%%     will modify this. This is the right place store additional informations
%%     about the source of data for specific clustering algorithms.</li>
%% </ul>
-record(clItem, {
    id,             % term()
    type,           % atom()
    name,           % string()
    props = []      % [{atom(), term()}]
    }).


%% @type clCluster() = {clCluster,
%%           id::term(),
%%           type::atom(),
%%           mode::Mode
%%           name::string(), 
%%           props::[{Key::atom(), Value::term()}]}
%%       Mode = open | close.
%%
%% Cluster record type. It contains information about a cluster itself
%% not about the elements of the cluster. Informations about the membership
%% are stored in a {@link clClustering} record.
%% 
%% The fields:
%% <ul>
%%   <li>`id': The unique identifier of the cluster. It must be unique
%%     between all cluster because the records are identified only by their 
%%     `id' fields.</li>
%%   <li>`type': The type of the cluster stored in `clCluster'. It can be an
%%     arbitrary atom. For example: if we clustering functions and we have a
%%     clusters which contain some often used functions the `type' of this
%%     cluster let be `library'.</li>
%%   <li>`mode': There are `open' and `close' clusters. For example we don't
%%     want to cluster the factory functions. Than put all functions from the
%%     module `lists' in one cluster and close it.</li>
%%   <li>`name': the name of the cluster. It will be used when the cluster
%%     is writed out and the name is more suitable instead the `clItem'.</li>
%%   <li>`props': Any other informations about the cluster can be stored in this
%%     property list. The base modules of the clustering infrastucture never
%%     will modify this. This is the right place store additional informations
%%     about the clusters for specific clustering algorithms.</li>
%% </ul>
-record(clCluster, {
    id,             % term()
    type,           % atom()
    mode,           % open | close
    name,           % term()
    props = []      % list()
    }).


%% @type clClustering() = {clClustering,
%%           clusters::tid(),
%%           cl_members::tid(),
%%           cl_conn::matrix()}.
%%
%% Grouping of the entities. This record type store the description of the
%% actual clusters (see {@link clCluster}), the membership of the the
%% entities (see {@link clItem}) in the clusters and the strength of connection
%% between the clusters.
%%
%% The fields:
%% <ul>
%%   <li>`clusters': It's an ETS set table identifier. This set store the
%%     descriptions of actual clusters in `clCluster' records. Clusters are
%%     identified by the `id' field.</li>
%%   <li>`cl_members': It's an ETS bag table identifier. This bag contains
%%     `{cluster id, entity id}' pairs. These pairs determine the members of
%%     the clusters.</li>
%%   <li>`cl_conn': It is a square matrix. The rows and columns are labeled with
%%     the cluster identifiers. The cell value in the cross a row and a column
%%     represent the connection of the row cluster with the column cluster.</li>
%% </ul>
%%
%% See the {@link ucl_clustering} library for usefull functions to handle 
%% `clClustering' records.
-record(clClustering, {
    clusters,           % ETS set
    cl_members,         % ETS bag
    cl_conn             % matrix()
    }).



%% @type clState() = {clState, 
%%          items::tid(),
%%          attr_matrix::matrix(),
%%          ent_conn_matrix::matrix(),
%%          ent_conn_sym::boolean(),
%%          attr_conn_matrix::matrix(),
%%          attr_conn_sym::boolean(),
%%          clustering::clClustering(),
%%          cl_hist::tid(),
%%          custom::term(),
%%          etc::tid()}.
%%
%% The actual state of the clustering process. It contains the entities, 
%% attributes, clusters, clusterings and the connections of all of these.
%% Clustering algorithms work with a`clState' record.
%%
%% The fields:
%% <ul>
%%   <li>`items': an ETS set table identifier. This set store the
%%     descriptions of the items of clustering process in `clItem' records.
%%     Items are identified by the `id' field.</li>
%%   <li>`attr_matrix': the attribute matrix. It can be understanded as 
%%     dependency matrix too. The row labels are the entity identifiers and 
%%     the column labels are the attribute identifiers. A cell value describe
%%     the connection between the corresponfding entity and attribute.</li>
%%   <li>`ent_conn_matrix': a matrix which contains the calculated
%%     distance/similarity between entities. The row and column labels are the 
%%     entitie identifiers.</li>
%%   <li>`ent_conn_sym': indicate that if the entity connection matrix is
%%     symmetric. If it's true the entity connection matrix is an upper triangle
%%     matrix.</li>
%%   <li>`attr_conn_matrix': a matrix which contains the calculated
%%     distance/similarity between attributes. The row and column labels are
%%     attribute identifiers.</li>
%%   <li>`attr_conn_sym': indicate that if the attribute connection matrix is
%%     symmetric. If it's true the attribute connection matrix is an upper
%%     triangle matrix.</li>
%%   <li>`clustering': an {@type clClustering} record which store the actual 
%%     clusters and the grouping of entities.</li>
%%   <li>`cl_hist': the history of the clusterings. It is an ETS set table
%%     identifier. You can store the actual clustering in this history and
%%     create a new one. It works like a stack. You can push (backup) and pop
%%     (restore) the `clustering' field into the stack (history).</li>
%%   <li>`custom': any custom data of a specific clustering algorithm.</li>
%%   <li>`etc': an ETS set table identifier. You can store any other
%%     information here.</li>
%% </ul>
%%
%% See the {@link ucl_state} library for usefull functions to handle 
%% `clState' records.
%%
%% @todo Create a new `output' field which contain a {@link cl_out} device 
%%       object. The algorithms should use this device to write out
%%       informations to the user under the executing.
-record(clState, {
    % Output device
    % output,             % 
    % Clustering items and connections of those
    items,              % ETS set
    attr_matrix,        % matrix()
    ent_conn_matrix,    % matrix()
    ent_conn_sym,       % bool()
    attr_conn_matrix,   % matrix()
    attr_conn_sym,      % bool()
    % Clustering
    clustering,         % clClustering()
    cl_hist,            % ETS set()
    % Algorithm custom data
    custom,             % term()
    % Private members
    etc                 % ETS set, private
    }).


%% @type clConfig() = {clConfig, {
%%           algs::[AlgName]
%%           steps::[StepDesc]
%%           donesteps::[StepDesc]
%%           state::clState()}
%%       AlgName = atom()
%%       StepDesc = {FunctionName::atom(), 
%%                   Options::[{Key::atom(), Value::term()}]}.
%%
%% Configuration of the clustering process. It is a record type. The `clConfig'
%% record contain the clustering algorithms, the state of the clustering process
%% and book the already executed parts of the algorithm and the future parts.
%%
%% The fields:
%% <ul>
%%   <li>`algs': the inheritance chain of the clustering algorithm. This list
%%     contains module names which are implement a clustering algorithm or a
%%     part of it. The last one is the most general algorithm and the first one
%%     is the most specified algorithm. The first is inheriteh from the second.
%%     </li>
%%   <li>`steps': the future part of the clustering algorithm. A part named
%%     clustering step or algorithm step. A step is described by a function 
%%     name and a property list. The function is an exported function of a 
%%     algorith module from the `algs' field. The first step will be executed
%%     first.</li>
%%   <li>`done_steps': the already executed steps of the clustering algorithm.
%%     Same as `steps' just in reverse order. The first step was executed last.
%%     </li>
%%   <li>`state': the actual clustering state. This state was returned by
%%     the last step and it will be passed to the next step.</li>
%% </ul>
%%
%% For more informations about clustering algorithms, the inheritance of
%% algorithms and the algothm steps see the {@link ucl_alg} module.
%%
%% The `clConfig' is just a snapshot from the clustering process. At the 
%% begining all steps are in the `steps' and `state' is the initial state.
%% At the end all steps are in the `done_steps' and the `state' is the final
%% state which contain the result of the clustring process. The {@link ucl_exec}
%% module is able to continue a clustering process from a `clConfig' snapshot.
%% 
%% See the {@link ucl_config} library for usefull functions to handle 
%% `clConfig' records. See the {@link ucl_exec} module for continue the process
%% from a snapshot.
-record(clConfig, {
    algs = [],          % [ModName::atom()]
    steps = [],         % [{FunName::atom(), Options::proplist()}]
    donesteps = [],     % [{FunName::atom(), Options::proplist()}]
    state}).            % term()


