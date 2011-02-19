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

%%% @doc Clustering with data mining techniques

%%% @todo More description

%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(ucl_alg_dm).
-vsn("$Rev:  $").

-include("ucluster.hrl").


%%% ============================================================================
%%% Imports/exports

% Steps sequence
-export([steps_default/0, steps_validator/0, steps/1]).

% Callback functions - Run the complet algorithm
-export([run_default/0, run_validator/0, run/2]).

% Callback functions - Steps
-export([init_default/0, init_validator/0, init/2,
         calc_use_query_tables/2,
         calc_alpha_set/2,
         calc_transactions/2,
         calc_associations/2,
         do_isa_alg/2,
         reorder_attr_matrix/2]).



%%% ============================================================================
%%% Types

%%
%%
-record(dmState, {
    % Options
    beta,               % int()
    gamma,              % int()
    minsup,             % int()
    ht_hashparam,       % {HashSize::int(), ((term()) -> [1..HashSize])}
    ht_leafsize,        % int()
    % Stores
    uses,               % ETS_tid()
    queries,            % ETS_tid()
    alpha_set,          % {[term()], [term()]}
    transactions,       % ETS_tid()
    itemsets,           % ETS_tid()
    large_itemsets,     % [ItemsetId]
    group_ents,         % [[ItemsetId]]
    group_attrs         % [[ItemsetId]]
    }).



%%% ============================================================================
%%% Steps sequence

%% @spec steps_default() -> DefaultOptions::proplist()
%% @doc  Return the default options of the {@link steps/1} function.
%%       It is a wrapper function to the {@link init_default/0} function.
%%       For more information see the {@link steps/1} function.
%% @see steps/1
%% @see init_default/0
steps_default() -> init_default().

%% @spec steps_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the 
%%       {@link steps/1} function.
%%       It is a wrapper function to the {@link init_validator/0} function.
%%       For more information see the {@link steps/1} function.
%% @see steps/1
%% @see init_validator/0
steps_validator() -> init_validator().

%% @spec steps(Options::proplist()) -> [StepDesc]
%%       StepDesc = {FunctionName::atom(), 
%%                   Options::[{Key::atom(), Value::term()}]}
%% @doc Return a list which contains the steps of the complet algorithm.
%%      The first step is the {@link init/2} function. All options in `Options'
%%      will be given to the {@link init/2} step.
%%
%%      For the avaible options see {@link init_default/0} and
%%      {@link init_validator/0} functions.
%% @see init_default/0
%% @see init_validator/0
steps(Options) when is_list(Options) ->
    [{init,                  Options},
     {build_conn_matrix,
        [{matrix,entity},
         {conn_fun,{ucl_connection,common_attr_cnt}},
         {conn_def_value,0},
         {symmetric,true}]},
     {build_conn_matrix,
        [{matrix,attribute},
         {conn_fun,{ucl_connection,common_ent_cnt}},
         {conn_def_value,0},
         {symmetric,true}]},
     {calc_use_query_tables, []},
     {calc_alpha_set,        []},
     {calc_transactions,     []},
     {calc_associations,     []},
     {do_isa_alg,            []},
     {reorder_attr_matrix,   []}].



%%% ============================================================================
%%% Callback functions - Run the complet algorithm

%% @spec run_default() -> DefaultOptions::proplist()
%% @doc  Return the default options of the {@link run/2} function.
%%       It is a wrapper function to the {@link steps_default/0} function.
%%       For more information see the {@link run/1} function.
%% @see run/2
%% @see steps_default/0
run_default() ->
    steps_default().

%% @spec run_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the 
%%       {@link run/2} function.
%%       It is a wrapper function to the {@link steps_validator/0} function.
%%       For more information see the {@link run/2} function.
%% @see run/2
%% @see steps_validator/0
run_validator() ->
    steps_validator().

%% @spec run(State::clState(), Options::proplist()) -> 
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc  Run the complet clustering algorithm.
%%       It is a wrapper function to the {@link steps/1} function.
%%       For more information see the {@link steps/1} function.
%% @see steps_default/0
%% @see steps_validator/0
run(St=#clState{}, Options) ->
    {St, steps(Options)}.



%%% ============================================================================
%%% Callback functions - Steps

%% @spec init_default() -> DefaultOptions::proplist()
%% @doc Return the default options of the {@link init/2} function.
%%      For the avaible options see the {@link init_validator/0} function.
%% @see init_validator/0
init_default() ->
    [{beta,1}, {gamma,1}, {minsup,3}, 
     {number_ids,false}, {hashtablesize,13}, {hashleafsize, 16}].

%% @spec init_validator() -> [{Key::atom(), ValidatorFun}]
%%       ValidatorFun = ((Value::term()) -> boolean())
%% @doc  Return the validator functions for the available options of the 
%%       {@link init/2} function.
%%
%% The availabe options and values are:
%% <ul>
%%   <li>`beta' = positive(): A positive integer.
%%     Minimal query value in the Alpha Set.</li>
%%   <li>`gamma' = positive(): A positive integer.
%%     Minimal usage value in the Alpha Set.</li>
%%   <li>`minsup' = positive(): An integer greater then one.
%%     Minimal support value.</li>
%%   <li>`number_ids' = boolean(): The identifiers of the clustering items
%%     are integers, or aren't.</li>
%%   <li>`hashtablesize' = positive(): An integer greater then one.
%%     The size of hash tables in the inner nodes of multiple level hashing 
%%     tree.</li>
%%   <li>`hashleafsize' = positive(): An integer greater then one.
%%     The maximal number of itemsets in the leaf nodes of multiple level 
%%     hashing tree not in the bottom level.</li>
%% </ul>
init_validator() ->
    [{beta,             fun(V) -> is_integer(V) andalso 0<V end},
     {gamma,            fun(V) -> is_integer(V) andalso 0<V end},
     {minsup,           fun(V) -> is_integer(V) andalso 1<V end},
     {number_ids,       fun is_boolean/1},
     {hashtablesize,    fun(V) -> is_integer(V) andalso 1<V end},
     {hashleafsize,     fun(V) -> is_integer(V) andalso 1<V end}].

%% @spec init(State::clState(), Options::proplist()) -> 
%%           {NewState::clState(), PlusSteps::proplist()}
%% @doc Create a new algorithm state based on the options given in the `Options'
%%      property list and store it in the `custom' field of the `State'
%%      clustering state.
%%
%%      For the avaible options see {@link init_default/0} and
%%      {@link init_validator/0} functions.
%% @see init_default/0
%% @see init_validator/0
init(St=#clState{}, Options) ->
    DmSt = new(get_value(hashtablesize,Options),get_value(hashleafsize,Options),
               get_value(number_ids,Options), get_value(beta,Options), 
               get_value(gamma,Options), get_value(minsup,Options)),
    % Return new state
    {St#clState{custom=DmSt}, []}.


%% @spec calc_use_query_tables(State::clState(), Options::proplist()) -> 
%%           {NewState::clState(), []}
%% @doc Create the Use and Query tables based on the usage between the entities
%%      and resources.
%%      `Options' is not used.
calc_use_query_tables(St=#clState{attr_matrix=AttrMatrix, 
        custom=#dmState{uses=UsesETS, queries=QueriesETS}}, _Options) ->
    create_use_table(AttrMatrix, UsesETS),
    create_query_table(AttrMatrix, QueriesETS),
    % Return new state
    {St, []}.


%% @spec calc_alpha_set(State::clState(), Options::proplist()) -> 
%%           {NewState::clState(), []}
%% @doc Create Alpha Set.
%%      `Options' is not used.
calc_alpha_set(St=#clState{custom=DmSt=#dmState{}}, _Options) ->
    {Ps, Fs} = calc_alpha_set(St),
    io:format("Alpha set size: entities = ~p, resources = ~p\n", 
        [length(Ps), length(Fs)]),
    if
        []==Ps orelse []==Fs -> throw(?LocalError(empty_alpha_set, []));
        true -> ok
    end,
    % Return new state
    {St#clState{custom=DmSt#dmState{alpha_set={Ps,Fs}}}, []}.


%% @spec calc_transactions(State::clState(), Options::proplist()) -> 
%%           {NewState::clState(), []}
%% @doc Create the database view of the system. Create transaction database.
%%      `Options' is not used.
calc_transactions(St=#clState{custom=DmSt=#dmState{alpha_set=AS}}, _Options) ->
    transactions(AS, DmSt),
    % Return new state
    {St, []}.


%% @spec calc_associations(State::clState(), Options::proplist()) -> 
%%           {NewState::clState(), []}
%% @doc Calculate the association rules from the transaction database.
%%      `Options' is not used.
calc_associations(St=#clState{custom=DmSt=#dmState{alpha_set=AS,itemsets=LETS}},
        _Options) ->
    % Generate large itemsets
    LsIds0 = apriori(AS, DmSt),
    % Remove 1-itemsets
    {LsIds1, [L1Ids]} = lists:split(length(LsIds0)-1, LsIds0),
    lists:foreach(fun(LId) -> ets:delete(LETS,LId) end, L1Ids),
    % Sort itemsets by support value
    LsIds = lists:reverse(element(3,lists:unzip3(lists:usort(lists:map(
        fun(LId) ->
            [{_LId, LItems, LSup}] = ets:lookup(LETS, LId),
            {LSup, length(LItems), LId}
        end,
        lists:flatten(LsIds1)))))),
    io:format("Number of large itemsets: ~p\n", [ets:info(LETS,size)]),
    % Return new state
    {St#clState{custom=DmSt#dmState{large_itemsets=LsIds}}, []}.


%% @spec do_isa_alg(State::clState(), Options::proplist()) -> 
%%           {NewState::clState(), []}
%% @doc The ISA algorithm. Reorder the entities into adjacent rows and the 
%%      attributes into adjacent colums in the attribute matrix by the 
%%      founded association rules in the previous step.
%%      `Options' is not used.
do_isa_alg(St=#clState{custom=#dmState{alpha_set={Ps,_Fs}}}, _Options) ->
    St2 = isa(length(Ps), St),
    % Return new state
    {St2, []}.


%% @spec reorder_attr_matrix(St::clState(), Options::proplist()) -> 
%%           {NewState::clState(), []}
%% @doc  Store the result of the algoritm back to the clustering state.
%%       Reorder the rows and columns of the attribute matrix by the result
%%       of the ISA algorithm.
%%       `Options' is not used.
reorder_attr_matrix(St=#clState{attr_matrix=AttrMatrix,
        custom=DmSt=#dmState{group_ents=GroupEntIds, 
                            group_attrs=GroupAttrIds}}, _Options) ->
    % Reorder rows and columns
    AllAttrIds = ?ClState:attrIds(St),
    AttrIds1   = GroupAttrIds++(AllAttrIds--GroupAttrIds),
    AM1 = ?Matrix:reorder_rows(GroupEntIds, AttrMatrix),
    AM2 = ?Matrix:reorder_cols(AttrIds1, AM1),
    % Delete algotihm state
    delete(DmSt),
    % Return new state
    {St#clState{attr_matrix=AM2}, []}.



%%% ============================================================================
%%% Data mining state

%%
%%
new(HashTableSize,HashLeafSize, NumberIds, Beta,Gamma,MinSup) ->
    % Select hash function
    HP = ?MHT:hash_gen(HashTableSize, NumberIds),
    #dmState{
        % Options
        beta            = Beta,
        gamma           = Gamma,
        minsup          = MinSup,
        ht_hashparam    = HP,
        ht_leafsize     = HashLeafSize,
        % Stores
        uses            = ets:new(dm_uses, []),
        queries         = ets:new(dm_queries, []),
        transactions    = ets:new(dm_transactions, []),
        itemsets        = ets:new(dm_itemsets, []),
        large_itemsets  = []}.


%% @spec delete(DmSt::dmState()) -> ok
%% @doc  Delete the state of the algorithm.
delete(St=#dmState{}) ->
    ets:delete(St#dmState.uses),
    ets:delete(St#dmState.queries),
    ets:delete(St#dmState.transactions),
    ets:delete(St#dmState.itemsets).



%%% ============================================================================
%%% Alpha set calculation

%%
%%
calc_alpha_set(St=#clState{custom=#dmState{beta=Beta, gamma=Gamma, uses=UseETS,
        queries=QueryETS}}) ->
    % Create initial values
    Ps0 = lists:usort(?ClState:entIds(St)),
    Fs0 = lists:usort(?ClState:attrIds(St)),
    % Calculate alpha set
    calc_alpha_set(Ps0, Fs0, u_gen(UseETS), q_gen(QueryETS), Beta, Gamma).


%%
%%
calc_alpha_set(Ps, Fs, FunU, FunQ, Beta, Gamma) ->
    Fs1 = lists:filter(fun(F) -> FunQ(F,Ps )>Beta  end, Fs),
    Ps1 = lists:filter(fun(P) -> FunU(P,Fs1)>Gamma end, Ps),
    if
        Fs1==Fs andalso Ps1==Ps -> {Ps1, Fs1};
        true -> calc_alpha_set(Ps1, Fs1, FunU, FunQ, Beta, Gamma)
    end.


%%
%%
create_use_table(AttrMatrix, UseETS) ->
    lists:foreach(
        fun(EntId) -> 
            ets:insert(UseETS, {EntId, lists:usort(
                        ?Matrix:get_row_non_def_ids(EntId, AttrMatrix))})
        end,
        lists:usort(?Matrix:rows(AttrMatrix))).

%%
%%
create_query_table(AttrMatrix, QueryETS) ->
    lists:foreach(
        fun(AttrId) -> 
            ets:insert(QueryETS, {AttrId, lists:usort(
                        ?Matrix:get_col_non_def_ids(AttrId, AttrMatrix))})
        end,
        lists:usort(?Matrix:cols(AttrMatrix))).


%%
%%
u_gen(UseETS) ->
    fun(P, Fs) ->
        [{P, Fs1}] = ets:lookup(UseETS, P),
        length(list_sort_intersect(Fs1, Fs))
    end.

%%
%%
q_gen(QueryETS) ->
    fun(F, Ps) ->
        [{F, Ps1}] = ets:lookup(QueryETS, F),
        length(list_sort_intersect(Ps1, Ps))
    end.



%%% ============================================================================
%%% Database view - transactions

%%
%%
transactions({Ps, Fs}, #dmState{queries=QueryETS, ht_hashparam={_HS,HF}, 
        transactions=TransETS}) ->
    lists:foreach(
        fun(F) -> 
            [{F, Ps1}] = ets:lookup(QueryETS, F),
            Ps2 = list_sort_intersect(Ps1, Ps),
            ets:insert(TransETS, {F, Ps2, lists:map(HF, Ps2)})
        end,
        Fs).



%%% ============================================================================
%%% Associations generation


%% @ spec apriori(D::[{TID::term(), TItems::[term()]}], MinSup::integer()) -> 
%%           {LargeKitemsetsIdxs::[[integer()]], LargeKitemsetsETS::tid()}
%% @ doc Apriori candidate generation from `D' transactions with least `MinSup'
%%      support.
%%
%%      The `LargeKitemsetsETS' ETS table contains the large itemsets in the
%%      `{Idx::integer(), Items::[term()], Support::integer()}' form. It is an
%%      ordered set by `Idx'. The itemsets belonging to same k-value are in
%%      adjancet rows and are lexicographically ordered by `Items'.
%%
%%      The `LargeKitemsetsIdxs' list contains the indices of the large 
%%      k-itemsets in lists in descending order by the k value. It means the 
%%      first element of `LargeKitemsetsIdxs' is a list which contains the 
%%      idices of the large n-itemsets. The second contains the indices of the 
%%      large n-1-itemsets, and so on. The last one consists the indices of 
%%      the large 1-itemset.
apriori({Ps,Fs}, DmSt=#dmState{}) ->
    % Calculate initial large itemset: L1
    {NextId, L1Ids, L1HT} = calc_l1({Ps,Fs}, DmSt),
    % Loop
    apriori_({L1Ids, L1HT}, 2, NextId, DmSt, []).

% Apriori candidate generation
apriori_({[],      Lk_1HT},_K,_NextId, #dmState{}, LsIds) -> 
    ?MHT:delete(Lk_1HT),
    LsIds;
apriori_({[LkId],  Lk_1HT},_K,_NextId, #dmState{}, LsIds) -> 
    ?MHT:delete(Lk_1HT),
    [[LkId]|LsIds];
apriori_({Lk_1Ids, Lk_1HT}, K, NextId, St=#dmState{minsup=MinSup, 
        ht_hashparam=HP, ht_leafsize=LS, itemsets=CETS, transactions=TransETS}, 
        LsIds) ->
    % New candidates
    CkHT = ?MHT:new(K, HP,LS),
    {NextId1, CkIds} = apriori_gen(Lk_1Ids, Lk_1HT, {NextId,CETS}, CkHT),
    % For all transaction t in TransETS:
    {CkSupDict4, BadTrans4} = ets:foldl(
        fun({Tid, Titems, THashes}, {CkSupDict1, BadTrans1}) ->
            % For all candidate c in subset(Ck, t) (candidates contained in t):
            {THasRealSubset,CkSupDict3} = lists:foldl(
                fun(Cid, {THasRealSubset2, CkSupDict2}) ->
                    [{_Cid,CItems,_CCnt}] = ets:lookup(CETS, Cid),
                    case (list_sort_substract(CItems,Titems)) of
                        [] -> % candidate c in subset(Ck, t): c.count++
                            %ets:update_counter(CETS, Cid, {3,1}),
                            {true, dict:update_counter(Cid, 1, CkSupDict2)};
                        _ -> {THasRealSubset2, CkSupDict2}
                    end
                end,
                {false, CkSupDict1},
                ?MHT:subset(THashes, CkHT)),
            % If a transaction don't contains any itemset from Ck it won't 
            % contain itemset from Cl where k<l
            if
                THasRealSubset -> {CkSupDict3, BadTrans1      };
                true           -> {CkSupDict3, [Tid|BadTrans1]}
            end
        end,
        {dict:new(), []},
        TransETS),
    % Delete transactions which don't contain any itemset from Ck
    lists:foreach(fun(Tid) -> ets:delete(TransETS, Tid) end, BadTrans4),
    % Delete candidates with too low support value
    LkIdSups = lists:usort(lists:filter(fun({_Cid, CCnt}) -> MinSup=<CCnt end,
                                        dict:to_list(CkSupDict4))),
    {LkIds, _} = lists:unzip(LkIdSups),
    BadCkIds = list_sort_substract(CkIds,LkIds),
    lists:foreach(fun(Cid) -> ets:delete(CETS, Cid) end, BadCkIds),
    lists:foreach(fun({Cid,CSup}) -> ets:update_counter(CETS,Cid,{3,CSup}) end,
                  LkIdSups),
    % Build the k-level hash tree of Lk
    ?MHT:simple_erase(BadCkIds, CkHT), % CkHT -> LkHT
    ?MHT:delete(Lk_1HT),
    % Go to k+1 step
    apriori_({LkIds, CkHT}, K+1, NextId1, St, [Lk_1Ids|LsIds]).



%%
%%
calc_l1({Ps,Fs}, #dmState{minsup=MinSup, ht_hashparam=HashParam,
        ht_leafsize=LeafSize, uses=UseETS, itemsets=CETS}) ->
    % Calculate support value for all 1-itemset and drop the smalls
    FunU = u_gen(UseETS),
    L1PCnts = lists:filter(fun({_P,Cnt}) -> MinSup=<Cnt end,
                           lists:map(fun(P) -> {P, FunU(P,Fs)} end, Ps)),
    % Use 1-itemsets which have greater or equal support value than MinSup
    L1HT = ?MHT:new(1, HashParam, LeafSize),
    NextId1 = lists:foldl(
        fun({P,Cnt}, NextId) ->
            % Insert Candidate table and 1-level hash tree
            ets:insert(CETS, {NextId, [P], Cnt}),
            ?MHT:insertL(NextId, [P], L1HT),
            NextId+1
        end,
        ets:info(CETS, size),
        L1PCnts),
    % Return ids of L1 itemsets and the hash tree of L1
    {NextId1, lists:seq(0,NextId1-1), L1HT}.


%%
%%
apriori_gen(Lk_1Ids, Lk_1HT, {NextId, CETS}, CkHT) ->
    [{_,XItems,_}] = ets:lookup(CETS, hd(Lk_1Ids)),
    Xs = tl(Lk_1Ids),
    apriori_gen(Xs, Xs, XItems, {NextId,CETS}, Lk_1HT, CkHT, []).

%
apriori_gen([],     [_X],  _XItems, {NextId,_CETS},_Lk_1HT,_CkHT, CkIds) ->
    {NextId, lists:reverse(CkIds)};
apriori_gen([],     [X|Xs],_XItems, {NextId, CETS}, Lk_1HT, CkHT, CkIds) ->
    [{_,NewXItems,_}] = ets:lookup(CETS, X),
    apriori_gen(Xs, Xs, NewXItems, {NextId,CETS}, Lk_1HT, CkHT, CkIds);
apriori_gen([Y|Ys], Xs, XItems, {NextId,CETS}, Lk_1HT, CkHT, CkIds) ->
    [{_,YItems,_}] = ets:lookup(CETS, Y),
    case ?MISC:list_compare(XItems, YItems) of
        {Common, [XLast], [YLast]} -> 
            NewItems = Common++[XLast,YLast],
            {NextId1,CkIds1} = case apriori_gen_prune(NewItems, Lk_1HT, CETS) of
                true -> 
                    ets:insert(CETS, {NextId, NewItems, 0}),
                    ?MHT:insertL(NextId, NewItems, CkHT),
                    {NextId+1, [NextId|CkIds]};
                _ -> {NextId, CkIds}
            end,
            apriori_gen(Ys, Xs, XItems, {NextId1,CETS}, Lk_1HT, CkHT, CkIds1);
        _ -> % Only in k-itemset case when 2<k (1<k-1)
            % Actual k-1 itemset hasn't common k-2 prefix any following itemset
            apriori_gen([], Xs, XItems, {NextId,CETS}, Lk_1HT, CkHT, CkIds)
    end.

% Return true if all subsetk-1(Items) are in the Lk-1
apriori_gen_prune(Items, Lk_1HT, CETS) ->
    Hashes = lists:map(?MHT:get_hash_fun(Lk_1HT), Items),
    IHk_1Subsets = lists:zip(list_n_1_subsets(Items), list_n_1_subsets(Hashes)),
    % All subsetk_1(Items) in Lk-1
    [] == lists:dropwhile(
            fun({S, SH}) -> % S in Lk-1
                [] /= lists:dropwhile(
                        fun(Cid) -> 
                            [{_,CItems,_}] = ets:lookup(CETS, Cid),
                            S /= CItems
                        end,
                        ?MHT:lookup(SH, Lk_1HT))
            end,
            IHk_1Subsets).



%%% ============================================================================
%%% ISA algorithm

%%
%%
isa(AlphaEntsCnt, St=#clState{custom=DmSt=#dmState{uses=UseETS, itemsets=LETS, 
        large_itemsets=LsIds}}) ->
    % Put all entity in large itemset into adjacent rows
    {Ents,Attrs} = isa_loop(LsIds, UseETS,LETS, {AlphaEntsCnt,0}, {[[]],[[]]}),
    % If there are entities not in the rows, put them into the rows
    RestEntIds = lists:usort(?ClState:entIds(St))--lists:flatten(Ents),
    {Ents2,Attrs2} = if
        []/=RestEntIds -> 
            isa_rest_ents(RestEntIds, UseETS, {Ents,Attrs}, St);
        true -> {Ents,Attrs}
    end,
    % Flat deep lists
    Ents3  = lists:flatten(lists:reverse(Ents2)),
    Attrs3 = lists:flatten(lists:reverse(Attrs2)),
    % Strore grouping lists
    St#clState{custom=DmSt#dmState{group_ents=Ents3, group_attrs=Attrs3}}.


% Put all entity in large itemset into the grouping table
isa_loop([],_UseETS,_LETS, {_AlphaEntsCnt,_EntsCnt}, {Ents,Attrs}) ->
    {Ents,Attrs};
isa_loop(_LsIds,_UseETS,_LETS, {AlphaEntsCnt,EntsCnt}, {Ents,Attrs}) when
        AlphaEntsCnt=<EntsCnt ->
    {Ents,Attrs};
isa_loop([LId|LsIds], UseETS, LETS, {AlphaEntsCnt,EntsCnt}, {Ents,Attrs}) ->
    % Add entities into adjacent rows and attributes into adjacent columns
    [{_LId, LItems, _LSup}] = ets:lookup(LETS, LId),
    {PlusEntsCnt, Ents1,Attrs1} = isa_add_entities(LItems,UseETS,{Ents,Attrs}),
    % Recursion
    isa_loop(LsIds, UseETS, LETS, {AlphaEntsCnt,EntsCnt+PlusEntsCnt}, 
             {Ents1, Attrs1}).


% Put entities, which are not in any large itemset, into the grouping table
isa_rest_ents([], _UseETS, {Ents,Attrs},_ClSt=#clState{}) -> {Ents,Attrs};
isa_rest_ents([RestEntId|RestEntIds], UseETS, {Ents,Attrs}, ClSt=#clState{}) ->
    % Find most similar entity
    EntIds = lists:flatten(Ents),
    EntId1 = hd(EntIds),
    {MaxEntId, _MaxConn} = lists:foldl(
        fun(EntId, {AccEntId, AccConn}) ->
            Conn = ?ClState:get_conn(entity, RestEntId, EntId, ClSt),
            if
                AccConn<Conn -> {EntId, Conn};
                true -> {AccEntId, AccConn}
            end
        end,
        {EntId1, ?ClState:get_conn(entity, RestEntId, EntId1, ClSt)},
        tl(EntIds)),
    % Add entities into adjacent rows and attributes into adjacent columns
    {_, Ents1, Attrs1} = isa_add_entities([RestEntId,MaxEntId], UseETS, 
                                          {Ents,Attrs}),
    % Recursion
    isa_rest_ents(RestEntIds, UseETS, {Ents1, Attrs1}, ClSt).


% Add entities into adjacent rows and attributes into adjacent columns
isa_add_entities(EntIds, UseETS, {Ents,Attrs}) ->
    % Put entities into adjacent rows
    {PlusEntsCnt, Ents1} = list_adjacent(EntIds, [], Ents),
    % Calculate common and free attribute identifiers
    UsedAttrIdLists = lists:map(
        fun(EntId) -> 
            [{_EntId, AttrIds1}] = ets:lookup(UseETS, EntId),
            AttrIds1
        end, 
        EntIds),
    CommonAttrIds = lists:foldl(
        fun(AttrIds, AccInt) -> list_sort_intersect(AttrIds, AccInt) end,
        hd(UsedAttrIdLists),
        tl(UsedAttrIdLists)),
    FreeAttrIds = lists:usort(lists:foldl(
        fun(AttrIds, AccFree) -> 
            list_sort_substract(AttrIds,CommonAttrIds)++AccFree
        end,
        [],
        UsedAttrIdLists)),
    % Put common attributes into adjacent columns
    {_, Attrs1} = list_adjacent(CommonAttrIds, FreeAttrIds, Attrs),
    % Return
    {PlusEntsCnt, Ents1, Attrs1}.



%%% ============================================================================
%%% List operations

%%
%%
list_n_1_subsets(List) when is_list(List) ->
    list_n_1_subsets_(List, [], []).

list_n_1_subsets_([],            _RevPrefix, SubSets) -> lists:reverse(SubSets);
list_n_1_subsets_([Elem|PostFix], RevPrefix, SubSets) ->
    list_n_1_subsets_(PostFix, [Elem|RevPrefix],
                      [ lists:reverse(RevPrefix)++PostFix |SubSets]).


%% @spec list_sort_substract(List1::[term()], List2::[term()]) -> 
%%           PartList1::[term()]
%% @doc Substact `List2' from `List1' in sorted way.
%%
%%      The complexity of `list_sort_substract(A, B)' is proportional to
%%      `length(A)+length(B)' instead of the complexity of `A--B' which 
%%      is proportional to `length(A)*length(B)'.
list_sort_substract(List1, List2) when is_list(List1), is_list(List1) ->
    list_sort_substract_(List1, List2).

list_sort_substract_([],    _Ys    ) -> [];
list_sort_substract_(Xs,     []    ) -> Xs;
list_sort_substract_([X|Xs], [X|Ys]) -> 
    list_sort_substract_(Xs, Ys);
list_sort_substract_([X|Xs], [Y|Ys]) when X<Y ->
    [X|list_sort_substract_(Xs, [Y|Ys])];
list_sort_substract_(Xs,    [_Y|Ys]) ->
    list_sort_substract_(Xs, Ys).


%% @spec list_sort_intersect(List1::[term()], List2::[term()]) -> 
%%           PartList1::[term()]
%% @doc Intersect `List1' and `List2' in sorted way.
%%
%%      The complexity of `list_sort_intersect(A, B)' is proportional to
%%      `length(A)+length(B)' instead of the complexity of `A--(A--B)' 
%%      which is proportional to `length(A)*length(B)'.
list_sort_intersect(List1, List2) when is_list(List1), is_list(List1) ->
    list_sort_intersect_(List1, List2).

list_sort_intersect_([],    _Ys    ) -> [];
list_sort_intersect_(_Xs,    []    ) -> [];
list_sort_intersect_([X|Xs], [X|Ys]) ->
    [X|list_sort_intersect_(Xs, Ys)];
list_sort_intersect_([X|Xs], [Y|Ys]) when X<Y ->
    list_sort_intersect_(Xs, [Y|Ys]);
list_sort_intersect_(Xs,    [_Y|Ys]) ->
    list_sort_intersect_(Xs, Ys).


%%
%%
list_adjacent(Commons, Frees, [FreeList0|CommonLists]) ->
    % Add missing elems
    {MissCommons, MissFrees} = lists:foldl(
        fun(Part, {AccC, AccF}) ->
            {AccC--Part, AccF--Part}
        end,
        {Commons, Frees},
        [FreeList0|CommonLists]),
    MissElems = MissFrees++MissCommons,
    FreeList  = MissElems++FreeList0,
    % Split parts by commons
    CParts = list_adjacent_split([FreeList|CommonLists], true, Commons),
    % Join neighbour common parts
    {length(MissElems), list_adjacent_join(CParts)}.

%
list_adjacent_split([],          _First,_Commons) -> [];
list_adjacent_split([Part|Parts], First, Commons) ->
    Part1 = case ?MISC:intersect(Part, Commons) of
        [] -> Part;
        CPart ->
            NCPart = Part--CPart,
            if
                []/=NCPart orelse First -> {CPart, NCPart};
                true -> CPart
            end
    end,
    [Part1 | list_adjacent_split(Parts, false, Commons)].

%
list_adjacent_join([{Cs1,Ps1}]) -> [Ps1,Cs1];
list_adjacent_join([Ps1]) -> [Ps1];
list_adjacent_join([{Cs1,Ps1},{Cs2,Ps2}|Parts]) -> 
    [Ps1,Cs1,Cs2| list_adjacent_join([Ps2|Parts])];
list_adjacent_join([{Cs1,Ps1},Ps2|Parts]) -> 
    [Ps1,Cs1 | list_adjacent_join([Ps2|Parts])];
list_adjacent_join([Ps1,{Cs2,Ps2}|Parts]) -> 
    [Ps1 | list_adjacent_join([{Cs2,Ps2}|Parts])];
list_adjacent_join([Ps1,Ps2|Parts]) -> 
    [Ps1 | list_adjacent_join([Ps2|Parts])].



