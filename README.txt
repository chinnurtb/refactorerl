RefactorErl 0.4

This is an early prototype release from the development of RefactorErl, a
refactoring tool for Erlang programs. It contains only two refactorings ("move
function" and "extract function"), and an analyser component that is capable
of clustering modules.

To get it working, do the following:

1. Run `make' in the top level directory (you may have to adjust the path
   settings in the Makefile)

2. Edit your ~/.emacs file and add the following lines (changing "/path/to"
   to the real path):

   (add-to-list 'load-path "/path/to/refactorerl-0.4/lib/refactorerl/emacs")
   (require 'refactorerl)

3. (Re)Start Emacs, and use `M-x customize-group' to customize the group
   `refactorerl'; you must enter the full path of the top level directory
   as `Refactorerl Base Path'.

4. Now you can use `M-x refactorerl-mode' to turn on RefactorErl mode on any
   Erlang source file. The first time the server is started as a
   subprocess. `C-c C-r C-h' gives a list of mode-specific key bindings, and
   a `Refactor' menu appears in the menu bar.  Some other operations are
   available by selecting the buffer *RefactorErl*.
