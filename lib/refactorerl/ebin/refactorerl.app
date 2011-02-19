% This is an -*- erlang -*- file.
{application, refactorerl,
 [{description, "Erlang Refactorer Prototype"},
  {vsn, "0.6"},
  {mod, {referl_superv, []}},
  {modules,
   [ %% Infrastructure
     referl_superv, referl_graph, referl_esg, referl_transform,
     %% Syntactical analysis
     referl_fileman, referl_preproc, referl_pp, referl_pp_rules,
     referl_misc, referl_syntax,
     referl_syntax_scanner, referl_syntax_parser, referl_syntax_nodes,
     %% Analyser modules
     referl_anal_context, referl_anal_fun, referl_anal_pp,
     referl_anal_mod, referl_anal_nodetype, referl_anal_rec,
     referl_anal_var,
     %% Query modules
     referl_clause, referl_expression, referl_file, referl_form,
     referl_function, referl_macro, referl_module, referl_query, referl_record,
     referl_token, referl_variable,
     %% Transformations
     referl_tr_elim_var, referl_tr_expand_funexpr, referl_tr_extract_fun,
     referl_tr_gen, referl_tr_inline_fun, referl_tr_merge,
     referl_tr_move_fun, referl_tr_move_rec, referl_tr_rename_fun,
     referl_tr_rename_mod, referl_tr_rename_rec, referl_tr_rename_recfield,
     referl_tr_rename_var, referl_tr_reorder_funpar, referl_tr_tuple_funpar,
     %% User interface
     referl_args, referl_error, referl_draw_graph,
     referl_ui, referl_ui_evsend, referl_emacs, referl_event_printer
    ]},
  {registered, [graph_server, esg_server, ui_message_server, transform_server]},
  {applications, [kernel, stdlib, sasl, mnesia]}
 ]}.
