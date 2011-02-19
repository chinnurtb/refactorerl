RefactorErl 0.7

This is a prototype release from the development of RefactorErl, a
refactoring tool for Erlang programs. It contains 22 refactoring steps, and
an analyser component that is capable of clustering modules.

To get it working, do the following:

1. Run the following command in the top level directory:

     bin/referl -build tool

   On Windows, you should use a backslash instead of the forward slash:

     bin\referl -build tool

   If you do not have the `erl' command in your PATH, or want to use a
   different one, you may specify it:

     bin/referl -erl /path/to/erl -build tool

2. Edit your ~/.emacs file and add the following lines (changing "/path/to"
   to the real path):

   (add-to-list 'load-path "/path/to/refactorerl-0.7/lib/referl_ui/emacs")
   (require 'refactorerl)

3. (Re)Start Emacs, and use `M-x customize-group' to customize the group
   `refactorerl'; you _must_ enter the full path of the top level directory
   as `Refactorerl Base Path'.

4. Now you can use `M-x refactorerl-mode' to turn on RefactorErl mode on any
   Erlang source file. The first time the server is started as a subprocess.
   `C-c C-r C-h' gives a list of mode-specific key bindings, and a `Refactor'
   menu appears in the menu bar. Note that files must be explicitly selected
   for refactoring by the `Add file' function in the menu.

   If you use refactorerl-mode often, consider enabling it automatically by
   placing the following line in your ~/.emacs file:

     (add-hook 'erlang-mode-hook 'refactorerl-mode)
