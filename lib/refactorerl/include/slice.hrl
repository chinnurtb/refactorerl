-include_lib("stdlib/include/qlc.hrl").

-define(Slice, slice).
-define(SliceFun, slice_fun).
-define(SliceRec, slice_rec).
-define(SliceMacro, slice_macro).

-record(slice, {node, second}).
-record(slice_forms, {node, second}).
-record(funref, {node, bindings}).
-record(recref, {node, bindings}).
-record(macroref, {node, bindings}).
-record(slice_files, {filenode, filename}).
-record(exports, {filenode, exportlist}).
