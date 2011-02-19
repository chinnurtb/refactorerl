% This is an -*- erlang -*- file.
{application, refactorer,
 [{description, "Erlang Refactorer Prototype"},
  {vsn, "0.2"},
  {mod, {refac_superv, []}},
  {env, [{db_backend, mysql},
         {db_host, "localhost"},
         {db_name, "parse"},
         {db_user, "root"},
         {db_password, "okkersarga"},
         {db_id, 1}
        ]},
  {modules, [create_nodes, db_init, d_client, delete_nodes, epp_dodger1,
             erl_recomment1, erl_scan1, erl_syntax_db, into_db,
             out_from_db, refac_checks, refac_common, refac_merge_subexpr,
             refac_ren_fun, refac_ren_var, refac_reorder_funpar,
             refac_superv, refactor_db, refactor, refac_tuple_funpar,
             refac_tuple_to_record, refac_var_elim, refac_extract_fun]}
 ]}.
