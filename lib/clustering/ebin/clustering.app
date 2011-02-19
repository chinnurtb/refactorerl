% This is an -*- Erlang -*- file.
{application, clustering,
 [{description, "Clustering for Erlang programs"},
  {vsn, "1.0"},
  {modules,
   [cl_core % TODO: fill in module names
    ]},
  {applications, [kernel, stdlib, sasl, mnesia, refactorerl]},
  {registered, []}
 ]}.
