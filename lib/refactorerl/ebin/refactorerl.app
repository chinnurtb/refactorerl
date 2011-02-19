% This is an -*- erlang -*- file.
{application, refactorerl,
 [{description, "Erlang Refactorer Prototype"},
  {vsn, "0.4"},
  {mod, {refac_superv, []}},
  {modules,
   [ %% Infrastructure
     refac_superv, refac_graph, refac_anal,
     %% Semantical analysis
     anal_attrib, anal_context, anal_exprtype, anal_function, anal_module,
     anal_record,
     anal_variable,
     %% Syntactical analysis
     refac_fileman, refac_preproc,
     refac_synlex, refac_syntax_scanner, refac_syntax_parser,
     refac_query, refac_manip,
     %% Refactorings
     refac_extract_fun,
     %% User interface
     refac_draw_graph, refac_emacs, refac_event_printer
    ]},
  {registered, [graph_server, esg_server]},
  {applications, [kernel, stdlib, sasl, mnesia]},
  {env, [{anal_mods, [anal_exprtype,
                      anal_attrib,
                      anal_module,
                      anal_context,
                      anal_function,
                      anal_record,
                      anal_variable
                     ]}
        ]}
 ]}.
