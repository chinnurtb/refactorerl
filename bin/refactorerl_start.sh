#!/bin/bash
# Author: Csaba Hoch <hoch@inf.elte.hu>

# Use the "--help" option to print the help.

if [ "$#" == "0" -o "$1" = "--help" -o "$1" = "-h" ]
then
    cat <<END
NAME
    refactorerl_start.sh

USAGE
    refactorerl_start.sh /path/to/tool [args...]

DESCRIPTION
    This script starts the Erlang runtime system, the applications needed by
    Refactorerl and the Refactorerl application itself.

ARGUMENTS
    The first argument has to be the path to the tool (the path that contains
    the directories bin, build and lib).
    All the other arguments will be passed to the Erlang runtime system (erl).

EXAMPLES
    Getting an Erlang shell:
    $ refactorerl_start.sh /path/to/tool

    Generating the documentation of the tool:
    $ refactorerl_start.sh /path/to/tool -run build doc -eval "halt()" -noshell
END
exit 0
fi

path="$1"
shift

cd "$path"
erl "-sname" "refactorerl@localhost" \
    "-boot" "$path/refactorerl" \
    "-config" "$path/sys.config" \
    "-pa" "$path/lib/refactorerl/ebin" \
    "-pa" "$path/lib/clustering/ebin" \
    "-pa" "$path/build" \
    "+W" "w" \
    "$@"

