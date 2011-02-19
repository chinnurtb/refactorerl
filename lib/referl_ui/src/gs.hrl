%% Modules
-define(Sup, referl_gs_sup).
-define(Config, referl_gs_config).

%% Records
-record(tag, {class, type, kind, id, name, node, selected=false}).
-record(tr, {label, map=false, desc, prop=[], func, back=false, kind=transform}).

%% Debugging
-define(Debug, io:format("Line: ~p~n", [?LINE])).
-define(DebugThis(This), io:format("Line: ~p, Message: ~p~n", [?LINE, This])).
