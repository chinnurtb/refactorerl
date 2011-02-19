% This is an -*- erlang -*- file.
{application, refactorerl,
 [{description, "Erlang Refactorer Prototype"},
  {vsn, "0.5"},
  {mod, {referl_superv, []}},
  {modules,
   [ %% Infrastructure
     referl_superv, referl_graph, referl_esg, referl_transform,
     %% Syntactical analysis
     referl_fileman, referl_preproc,
     referl_synlex, referl_manip, referl_misc,
     referl_syntax, referl_lex, referl_seminf,
     referl_syntax_scanner, referl_syntax_parser, referl_syntax_nodes,
     %% Analyser modules
     referl_anal_context, referl_anal_fun, referl_anal_indent,
     referl_anal_mod, referl_anal_nodetype, referl_anal_rec,
     referl_anal_var,
     %% Transformations
     referl_tr_elim_var, referl_tr_expand_funexpr, referl_tr_extract_fun,
     referl_tr_gen, referl_tr_inline_fun, referl_tr_merge,
     referl_tr_move_fun, referl_tr_move_rec, referl_tr_rename_fun,
     referl_tr_rename_mod, referl_tr_rename_rec, referl_tr_rename_recfield,
     referl_tr_rename_var, referl_tr_reorder_funpar, referl_tr_tuple_funpar,
     %% User interface
     referl_draw_graph, referl_ui, referl_emacs, referl_event_printer
    ]},
  {registered, [graph_server, esg_server, ui_message_server, transform_server]},
  {applications, [kernel, stdlib, sasl, mnesia]}
 ]}.
