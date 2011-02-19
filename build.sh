#!/bin/sh
# See INSTALL-UNIX.txt for instructions.

### Configuration section

# Base directory of RefactorErl source code
BASE=/opt/RefactorErl-0.2.1

# Base directory of Erlang/OTP
ERLANG=/usr/local/lib/erlang

### No changes are needed below this line -----------------------------------

ERL="$ERLANG/bin/erl"

(cd $BASE/distel/src;   "$ERL" -make) || exit 1
(cd $BASE/refactor/src; "$ERL" -make) || exit 1

echo "Creating startup scripts..."
echo \#!/bin/sh               > "$BASE/runerl.sh"
echo BASE=\""$BASE"\"        >> "$BASE/runerl.sh"
echo ERLANG=\""$ERLANG"\"    >> "$BASE/runerl.sh"
cat "$BASE/inst/runerl.unix" >> "$BASE/runerl.sh"
chmod +x "$BASE/runerl.sh"

echo "(setq refactorer-base-dir \"$BASE\")"  > "$BASE/load-refac.el"
echo "(setq erlang-root-dir \"$ERLANG\")"   >> "$BASE/load-refac.el"
cat "$BASE/inst/load-refac"                 >> "$BASE/load-refac.el"

echo "(load \"$BASE/load-refac.el\")" >> $HOME/.emacs


