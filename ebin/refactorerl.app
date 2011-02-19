% This is an -*- erlang -*- file.
{application, refactorerl,
 [{description, "Erlang Refactorer Prototype"},
  {vsn, "0.3"},
  {mod, {refac_superv, []}},
  {modules,
   [ %% Infrastructure
     refac_superv, refac_graph, refac_anal,
     %% Syntactical analysis
     refac_fileman, refac_preproc,
     refac_syntax, refac_syntax_scanner, refac_syntax_parser,
     anal_attrib, anal_exprtype,
     %% User interface
     refac_draw_graph, refac_ui, refac_emacs
    ]},
  {registered, [graph_server, esg_server, ui_message_server]},
  {applications, [kernel, stdlib, sasl, mnesia]},
  {env, [{anal_mods, [anal_exprtype, anal_attrib]}
        ]}
 ]}.
