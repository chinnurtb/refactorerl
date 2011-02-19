;; -*- coding: utf-8 -*-
;; The contents of this file are subject to the Erlang Public License,
;; Version 1.1, (the "License"); you may not use this file except in
;; compliance with the License. You should have received a copy of the
;; Erlang Public License along with this software. If not, it can be
;; retrieved via the world wide web at http://plc.inf.elte.hu/erlang/
;;
;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;; License for the specific language governing rights and limitations under
;; the License.
;;
;; The Original Code is RefactorErl.
;;
;; The Initial Developer of the Original Code is Eötvös Loránd University.
;; Portions created by Eötvös Loránd University are Copyright 2008, Eötvös
;; Loránd University. All Rights Reserved.


(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(provide 'refactorerl)

;; Customization variables

(defgroup refactorerl nil
  "Erlang Refactorer"
  :tag "RefactorErl"
  :group 'tools)

(defcustom refactorerl-base-path ""
  "RefactorErl base directory."
  :type 'directory
  :group 'refactorerl)

(defcustom refactorerl-erlang-runtime "erl"
  "Erlang emulator command to be used with RefactorErl."
  :type 'string
  :group 'refactorerl)

(defcustom refactorerl-data-dir 'base
  "Directory that is used to store RefactorErl data."
  :type '(choice (const :tag "Base directory" base)
                 (directory :tag "Given directory"))
  :group 'refactorerl)

(defcustom refactorerl-server-type 'internal
  "Specifies how to start the RefactorErl server process.
If its value is internal, the server process is managed internally.
It its value is external, the server should be started manually.
If its value is shell, a separate process is started by Emacs, with its
  Erlang shell accessible through the buffer *RefactorErlShell*."
  :type '(choice (const :tag "Internally managed server" internal)
                 (const :tag "Externally started server" external)
                 (const :tag "Managed server with shell access" shell))
  :group 'refactorerl)

;; Minor mode definition

(defun make-refactorerl-menu-map ()
  (let ((menu (make-sparse-keymap "Refactor")))
    (define-key menu [expfun]
      '(menu-item "Expand fun expression" refactorerl-expand-funexpr
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [extract]
      '(menu-item "Extract function" refactorerl-extract-function
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [mergeexpr]
      '(menu-item "Merge expressions" refactorerl-merge-expr
                  :enable (eq refac-buffer-state 'ok)
                  :help "Merges all instances of an expression"))
    (define-key menu [inline]
      '(menu-item "Inline function" refactorerl-inline-function
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [tuplefunpar]
      '(menu-item "Tuple function parameters" refactorerl-tuple-funpar
                  :enable (eq refac-buffer-state 'ok)
                  :help "Put tuple the marked parameters of the function"))
    (define-key menu [reorder]
      '(menu-item "Reorder function parameters" refactorerl-reorder-funpar
                  :enable (eq refac-buffer-state 'ok)
                  :help "Reorders a parameters of the marked function"))
    (define-key menu [generalize]
      '(menu-item "Generalize function definition"
                  refactorerl-generalize-function
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [renamefun]
      '(menu-item "Rename function" refactorerl-rename-function
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [renamevar]
      '(menu-item "Rename variable" refactorerl-rename-variable
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [renamemod]
      '(menu-item "Rename module" refactorerl-rename-mod
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [renamerec]
      '(menu-item "Rename record" refactorerl-rename-record
                  :enable (eq refac-buffer-state 'ok)))
   (define-key menu [renamefield]
      '(menu-item "Rename record field" refactorerl-rename-field
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [elimvar]
      '(menu-item "Eliminate variable" refactorerl-eliminate-variable
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [move]
      '(menu-item "Move function" refactorerl-move-function
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [moverec]
      '(menu-item "Move record" refactorerl-move-record
                  :enable (eq refac-buffer-state 'ok)))
    (define-key menu [sepref]  '(menu-item "--"))

    (define-key menu [update]
      '(menu-item "Update status" refactorerl-update-status
                  :enable buffer-file-name))
    (define-key menu [drop]
      '(menu-item "Drop file" refactorerl-drop-file
                  :enable buffer-file-name))
    (define-key menu [add]
      '(menu-item "Add file" refactorerl-add-file
                  :enable buffer-file-name))
    (define-key menu [sepfile]  '(menu-item "--"))

    (define-key menu [Undo]
      '(menu-item "Undo (one step only)" refactorerl-undo
                  :enable (file-exists-p
                              (concat
          (file-name-as-directory refactorerl-base-path) "data/backup.1"))
                  :help "Steps back on the database"))
    (define-key menu [sepfile]  '(menu-item "--"))

    (define-key menu [cluster]
      (list 'menu-item "Module clustering"
            (make-sparse-keymap "Module clustering")
            :enable '(refac-server-is-running)))
    (define-key menu [graph]
      '(menu-item "Draw graph" refactorerl-draw-graph
                  :enable (refac-server-is-running)))
    (define-key menu [load]
      '(menu-item "Load directory" refactorerl-load-dir
                  :enable (refac-server-is-running)))
    (define-key menu [contents]
      '(menu-item "Database contents" refactorerl-list-files
                  :enable (refac-server-is-running)))
    (define-key menu [sepglob]  '(menu-item "--"))

    (define-key menu [cluster genetic]
      '("Genetic" . refactorerl-cluster-genetic))
    (define-key menu [cluster agglom]
      '("Agglomerative" . refactorerl-cluster-agglom))

    (define-key menu [control]
      '(menu-item "Select control buffer" refactorerl-control-buffer
                  :enable (refac-server-is-running)))
    (define-key menu [quit]
      '(menu-item "Stop server" refactorerl-quit
                  :enable (refac-server-is-running)))
    (define-key menu [start]
      '(menu-item "Start server" refactorerl-restart
                  :enable (not (refac-server-is-running))))
    menu))

(defun make-refactorerl-mode-map ()
  "Creates the local keymap for Erlang Refactoring minor mode."
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-rQ"    'refactorerl-quit)
    (define-key map "\C-c\C-rR"    'refactorerl-restart)
    (define-key map "\C-c\C-ra"    'refactorerl-add-file)
    (define-key map "\C-c\C-rd"    'refactorerl-drop-file)
    (define-key map "\C-c\C-rD"    'refactorerl-debug-shell)
    (define-key map "\C-c\C-rfe"   'refactorerl-extract-function)
    (define-key map "\C-c\C-rme"   'refactorerl-merge-expr)
    (define-key map "\C-c\C-rfi"   'refactorerl-inline-function)
    (define-key map "\C-c\C-rft"   'refactorerl-tuple-funpar)
    (define-key map "\C-c\C-rfo"   'refactorerl-reorder-funpar)
    (define-key map "\C-c\C-ref"   'refactorerl-expand-funexpr)
    (define-key map "\C-c\C-rmf"   'refactorerl-move-function)
    (define-key map "\C-c\C-rmr"   'refactorerl-move-record)
    (define-key map "\C-c\C-rfg"   'refactorerl-generalize-function)
    (define-key map "\C-c\C-rrf"   'refactorerl-rename-function)
    (define-key map "\C-c\C-rrv"   'refactorerl-rename-variable)
    (define-key map "\C-c\C-rrm"   'refactorerl-rename-mod)
    (define-key map "\C-c\C-rrrd"  'refactorerl-rename-record)
    (define-key map "\C-c\C-rrrf"  'refactorerl-rename-field)
    (define-key map "\C-c\C-rev"   'refactorerl-eliminate-variable)
    (define-key map "\C-c\C-rG"    'refactorerl-draw-graph)
    (define-key map "\C-c\C-rL"    'refactorerl-load-dir)
    (define-key map "\C-c\C-rC"    'refactorerl-list-files)
    (define-key map "\C-c\C-r\C-u" 'refactorerl-update-status)
    (define-key map "\C-c\C-rt"    'refactorerl-cluster-agglom)
    (define-key map "\C-c\C-rg"    'refactorerl-cluster-genetic)
    (define-key map "\C-c\C-rS"    'refactorerl-add-checkpoint)
    (define-key map "\C-c\C-rc"    'refactorerl-clean)
    (define-key map "\C-c\C-rU"    'refactorerl-undo)
    (define-key map "\C-c\C-rTT"   'refactorerl-toggle-test)
    (define-key map [menu-bar refactorerl]
      (cons "Refactor" (make-refactorerl-menu-map)))
    map))

(defvar refac-buffer-state nil
  "Status of the file:
 - `off': not part of the active refactoring set
 - `err': there is an error in the file
 - `ok': ready for refactoring
 - nil: unknown state (e.g. during parsing)")

(define-minor-mode refactorerl-mode
  "Minor mode providing access to Erlang Refactorer operations.
\\<refactorerl-mode-map>
The first time this mode is activated the refactorer server is started as a
subprocess. It should be stopped before leaving Emacs (`\\[refactorerl-quit]').

Before using this minor mode, you must customize `refactorerl-base-path'.

Key bindings:
\\{refactorerl-mode-map}
"
  nil
  (:eval (cond ((bound-and-true-p refac-test-mode) " Refact:TEST")
               ((not refac-buffer-state) " Refact:???")
               ((eq refac-buffer-state 'off) " Refact:off")
               ((eq refac-buffer-state 'err) " Refact:error")
               ((eq refac-buffer-state 'ok) " Refact")))
  (make-refactorerl-mode-map)
  (when refactorerl-mode
    (make-local-variable 'refac-buffer-state)
    (refactorerl-start)
    (refactorerl-update-status)
    (add-hook 'after-save-hook 'refac-file-saved t t))
  (when (not refactorerl-mode)
    (remove-hook 'after-save-hook 'refac-file-saved t)))


;; Functions implementing interactive functionality (usually bound to a key)

(defun refactorerl-quit ()
  "Stops the RefactorErl server process."
  (interactive)
  (when (refac-server-is-running)
    (message "Initiating shutdown...")
    (refac-send-command 'stop)
    (while (refac-server-is-running)
      (sleep-for 0.1))
    (message "RefactorErl server has stopped.")))

(defun refactorerl-restart ()
  "Restarts the RefactorErl server process."
  (interactive)
  (when (refac-server-is-running)
    (refactorerl-quit)
    (sleep-for 0.5))
  (refactorerl-start))

(defun refactorerl-add-file ()
  "Add the visited file to the active refactoring set."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state 'off))
         (error "Already added"))
        (t
         (setq refac-buffer-state nil)
         (refac-send-command 'add buffer-file-name))))

(defun refactorerl-drop-file ()
  "Remove the visited file from the active refactoring set."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((eq refac-buffer-state 'off)
         (error "Not in the active set"))
        (t
         (setq refac-buffer-state nil)
         (refac-send-command 'drop buffer-file-name))))

(defun refactorerl-control-buffer ()
  "Selects the buffer *RefactorErl*"
  (interactive)
  (switch-to-buffer refac-server-buffer))

(defun refactorerl-draw-graph (file type)
  "Draw the contents of the graph into a file. The generated file is suitable
as an input to the graphviz package.

A numerical prefix argument can be used to filter the contents of the graph:
 1: full graph
 2: semantical, structural, and syntactical links
 3: structural and syntactical links
 4: syntactical links (bare syntax tree)
 5: syntactical and lexical links
 6: lexical links
 7: tokens
 8: all except lexical edges (for testing purpose)"
  (interactive "FGraph file: \np")
  (refac-send-command 'draw (expand-file-name file) type))

(defun refactorerl-debug-shell ()
  "Start an inferior Erlang shell (using the standard Erlang mode package)
that connects to the RefactorErl server."
  (interactive)
  (let ((inferior-erlang-machine-options
         '("-sname" "debug"
           "-remsh" "refactorerl@localhost")))
    (erlang-shell)))

(defun refactorerl-toggle-test ()
  (interactive)
  (if (boundp 'refac-test-mode)
      (setq refac-test-mode (not refac-test-mode))
    (set (make-local-variable 'refac-test-mode) t)))

(defun refac-transform (name &rest args)
  (let ((arglist (mapcar (lambda (a) (vector (car a) (cdr a))) args)))
    (cond ((not buffer-file-name)
           (error "No visited file"))
          ((bound-and-true-p refac-test-mode)
           (kill-new (refac-erl-format arglist)))
          ((not (eq refac-buffer-state 'ok))
           (error "File is not ready for refactoring"))
          (t
           (refac-send-command 'transform name arglist)))))

(defun refactorerl-extract-function (beg end name)
  "Performs the Extract Function refactoring. Operates on the expression or
expressions between the point and the mark.

1. Mark the exact part of the source you want to extract.
2. Call the refactoring from the menu or with \\[refactorerl-extract-function].
3. Type the new function name."
  (interactive "r\nsFunction name: ")
  (refac-transform 'referl_tr_extract_fun
                   (cons 'file buffer-file-name)
                   (cons 'posrange (vector beg (1- end)))
                   (cons 'name name))
  (deactivate-mark))

(defun refactorerl-merge-expr (beg end varname)
  "Performs the Merge Expression Duplicates refactoring. Operates
on the expression between the point and the mark.

1. Mark the expression whose duplicates are to be merged.
2. Call the refactoring from the menu or with \\[refactorerl-merge-expr].
3. Type the new variable name."
  (interactive "r\nsNew variable name: ")
  (refac-transform 'referl_tr_merge
                   (cons 'file buffer-file-name)
                   (cons 'posrange (vector beg (1- end)))
                   (cons 'varname varname))
  (deactivate-mark))

(defun refactorerl-inline-function (pos)
  "Performs the Inline Function refactoring.

1. Position the cursor over a function application.
2. Call the refactoring from the menu or with \\[refactorerl-inline-function]."
  (interactive "d")
  (refac-transform 'referl_tr_inline_fun
                   (cons 'file buffer-file-name)
                   (cons 'position pos)))


(defun refactorerl-rename-mod (name)
  "Performs the Rename Module refactoring.

1. Position the cursor over the name in the module attribute.
2. Call the refactoring from the menu or with \\[refactorerl-rename-mod].
3. Type the new module name."
  (interactive "sNew module name:")
  (refac-transform 'referl_tr_rename_mod
                   (cons 'file buffer-file-name)
                   (cons 'name name)))

(defun refactorerl-tuple-funpar (beg end)
  "Performs the Tuple Function Parameters refactoring. Operates on the
parameters of function which are marked.

1. Mark the arguments to be tupled in the function definition.
2. Call the refactoring from the menu or with \\[refactorerl-tuple-funpar]."
  (interactive "r")
  (refac-transform 'referl_tr_tuple_funpar
                   (cons 'file buffer-file-name)
                   (cons 'posrange (vector beg (1- end))))
  (deactivate-mark))

(defun refactorerl-reorder-funpar (pos order)
  "Performs the Reorder Function Parameters refactoring.

1. Position the cursor over the name of the function in any
      clause of the function definition.
2. Call the refactoring from the menu or with \\[refactorerl-reorder-funpar].
3. Type the name for the new order of the function arguments."
  (interactive "d\nsNew order (e.g. 3 1 2): ")
  (refac-transform 'referl_tr_reorder_funpar
                   (cons 'file buffer-file-name)
                   (cons 'position pos)
                   (cons 'order (car (read-from-string
                                      (concat "(" order ")"))))))

(defun refactorerl-generalize-function (beg end name)
  "Performs the Generalize Function refactoring. Operates on the expression or
expressions between the point and the mark.

1. Select the expression along which the generalization should be done.
2. Call the refactoring from the menu or with \\[refactorerl-generalize-function].
3. Type the name for the new function argument."
  (interactive "r\nsNew parameter name: ")
  (refac-transform 'referl_tr_gen
                   (cons 'file buffer-file-name)
                   (cons 'posrange (vector beg (1- end)))
                   (cons 'varname name))
  (deactivate-mark))

(defun refactorerl-rename-function (pos name)
  "Performs the Rename Function refactoring.

1. Position the cursor over the name of the function in any
      clause of the function definition, any application of the
      function or the function in an export list.
2. Call the refactoring from the menu or with \\[refactorerl-rename-function].
3. Type the new function name."

  (interactive "d\nsNew function name: ")
  (refac-transform 'referl_tr_rename_fun
                   (cons 'file buffer-file-name)
                   (cons 'position pos)
                   (cons 'name name)))

(defun refactorerl-rename-variable (pos name)
  "Performs the Rename Variable refactoring.

1. Position the cursor over any instance of the variable.
2. Call the refactoring from the menu or with \\[refactorerl-rename-variable].
3. Type the new variable name."
  (interactive "d\nsNew variable name: ")
  (refac-transform 'referl_tr_rename_var
                   (cons 'file buffer-file-name)
                   (cons 'position pos)
                   (cons 'varname name)))

(defun refactorerl-rename-record (pos name)
  "Performs the Rename Record refactoring.

1. Position the cursor over the name in the record definition.
2. Call the refactoring from the menu or with \\[refactorerl-rename-record].
3. Type the new record name."
  (interactive "d\nsNew record name: ")
  (refac-transform 'referl_tr_rename_rec
                   (cons 'file buffer-file-name)
                   (cons 'position pos)
                   (cons 'name name)))

(defun refactorerl-rename-field (pos name)
  "Performs the Rename Record Field refactoring.

1. Position the cursor over the field name in the record definition.
2. Call the refactoring from the menu or with \\[refactorerl-rename-field].
3. Type the new field name."
  (interactive "d\nsNew field name: ")
  (refac-transform 'referl_tr_rename_recfield
                   (cons 'file buffer-file-name)
                   (cons 'position pos)
                   (cons 'name name)))

(defun refactorerl-eliminate-variable (pos)
  "Performs the Eliminate Variable refactoring.

1. Position the cursor over any instance of the variable.
2. Call the refactoring from the menu or with \\[refactorerl-eliminate-variable]."
  (interactive "d")
  (refac-transform 'referl_tr_elim_var
                   (cons 'file buffer-file-name)
                   (cons 'position pos)))

(defun refactorerl-expand-funexpr (pos)
  "Performs the Expand Fun Expression refactoring.

1. Position the cursor over the name of the function to be expanded.
2. Call the refactoring from the menu or with \\[refactorerl-expand-funexpr]."
  (interactive "d")
  (refac-transform 'referl_tr_expand_funexpr
                   (cons 'file buffer-file-name)
                   (cons 'position pos)))

(defun refactorerl-move-function ()
  "Performs the Move Function refactoring. The functions to be
moved can be selected from a list.

1. Call the refactoring from the menu or with \\[refactorerl-move-function].
2. Fill out the form that pops up:
    - type the name of the target module,
    - select the records to be moved.
3. Start the transformation with the `Move' button."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state 'ok))
         (error "File is not ready for refactoring"))
        (t
         (refac-move-fun-params))))

(defun refactorerl-move-record ()
  "Performs the move record refactoring. The records to be
moved can be selected from a list.
1. Call the refactoring from the menu or with \\[refactorerl-move-record].
2. Fill out the form that pops up:
    - type the name of the target module,
    - select the records to be moved.
3. Start the transformation with the `Move' button."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state 'ok))
         (error "File is not ready for refactoring"))
        (t
         (refac-move-rec-params))))

(defun refactorerl-cluster-agglom ()
  (interactive)
  (cluster-ui-options-agglom))

(defun refactorerl-cluster-genetic ()
  (interactive)
  (cluster-ui-options-genetic))

(defun refactorerl-add-checkpoint ()
  "Creates a new checkpoint file from the database."
  (interactive)
  (refac-send-command 'backup))

(defun refactorerl-clean ()
  "Deletes all checkpoint files from the database."
  (interactive)
  (refac-send-command 'clean))

(defun refactorerl-undo ()
  "Steps backward on the refactoring database."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state 'ok))
         (error "File is not ready for refactoring"))
        ((yes-or-no-p "All changes since last refactoring will be lost. Continue? ")
         (refac-send-command 'undo buffer-file-name))))

(defun refactorerl-list-files (&optional same-win)
  "Shows the contents of the active refactoring set."
  (interactive)
  (with-current-buffer (refac-list-buffer)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (refac-send-command 'filelist)
    (set (make-local-variable 'last-file-dir) ""))
  (if same-win
      (switch-to-buffer (refac-list-buffer))
    (switch-to-buffer-other-window (refac-list-buffer))))

(defun refactorerl-load-dir (dirname)
  "Adds the contents of a directory to the active refactoring set."
  (interactive "D")
  (let ((files (directory-files dirname t ".*\\.erl$")))
    (if (equal 0 (length files))
        (error (concat "No Erlang files in " dirname))
      (setq refac-progress [nil 0 ""])
      (aset refac-progress 0 files)
      (aset refac-progress 2 dirname)
      (dolist (file files)
        (refac-send-command 'add file)))))

;; Cluster Ui

(defvar cluster-ui-buffer nil)
(defun cluster-ui-options-genetic()
  (refac-send-command 'cl_options 'genetic)
)
(defun cluster-ui-options-agglom()
  (refac-send-command 'cl_options 'agglom_attr)
)

;Buffer of the clustering
(defvar cluster-ui-result-buffer nil)
(defun res-apply-aggl (&rest args)
  (let ((value)
        (algo alg)
        (createdb (widget-value create))
       )
    (dolist (elt cl-options-list value)
      (setq value (cons (widget-value elt) value)))
    (setq cluster-ui-result-buffer
          (generate-new-buffer "*Clustering*"))
    (switch-to-buffer cluster-ui-result-buffer)
    (refac-send-command 'run_cl (reverse value) algo createdb)
  ))

(defun cluster-ui-refresh (&rest args)
   (refac-send-command 'cl_refresh)
  )

(defun cluster-ui-cleanup (&rest args)
  (delete-window (get-buffer-window cluster-ui-buffer))
  (kill-buffer cluster-ui-buffer))

;; Move function

(defvar refac-move-fun-buffer nil)
(defun refac-move-fun-params ()
  (let ((source (buffer-file-name)))
    (with-current-buffer
        (setq refac-move-fun-buffer (generate-new-buffer "*Move Function*"))
      (widget-insert
       (propertize "Move multiple functions to another module\n"
                   'face 'bold))
      (widget-insert (concat "Source file: " source "\n"))
      (set (make-local-variable 'target-entry)
           (widget-create 'editable-field
                          :size 30
                          :format "Target module: %v"
                          ""))
      (widget-insert "\nSelect functions to be moved:\n")
      (set (make-local-variable 'source-file) source))
    (refac-send-command 'funlist source)))

(defun refac-handle-funlist (function-list)
  (switch-to-buffer-other-window refac-move-fun-buffer)
  (set (make-local-variable 'function-checklist)
       (apply 'widget-create 'checklist
              (mapcar (lambda (fn)
                        (list 'const
                              :tag (format "%s/%d" (elt fn 0) (elt fn 1))
                              :format "%t\n"
                              fn))
                      function-list)))
  (widget-create 'push-button
                 :notify 'refac-move-fun-apply
                 "Move")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify 'refac-move-fun-cleanup
                 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(defun refac-move-fun-apply (&rest args)
  (let ((funlist (widget-value function-checklist))
        (target  (widget-value target-entry)))
    (when (equal target "")
      (error "No target module specified"))
    (when (equal funlist nil)
      (error "No functions selected"))
    (refac-send-command 'transform
                        'referl_tr_move_fun
                        (list (vector 'file source-file)
                              (vector 'funlist funlist)
                              (vector 'name target)))
    (refac-move-fun-cleanup)))

(defun refac-move-fun-cleanup (&rest args)
  (delete-window (get-buffer-window refac-move-fun-buffer))
  (kill-buffer refac-move-fun-buffer))

;; Move record

(defvar refac-move-rec-buffer nil)
(defun refac-move-rec-params ()
  (let ((source (buffer-file-name)))
    (with-current-buffer
        (setq refac-move-rec-buffer (generate-new-buffer "*Move Record*"))
      (widget-insert
       (propertize "Move records to another file\n"
                   'face 'bold))
      (widget-insert (concat "Source file: " source "\n"))
      (set (make-local-variable 'target-entry)
           (widget-create 'editable-field
                          :size 30
                          :format "Target file path: %v"
                          ""))
      (widget-insert "\nSelect records to be moved:\n")
      (set (make-local-variable 'source-file) source))
    (refac-send-command 'recordlist source)))

(defun refac-handle-recordlist (record-list)
  (switch-to-buffer-other-window refac-move-rec-buffer)
  (set (make-local-variable 'record-checklist)
       (apply 'widget-create 'checklist
              (mapcar (lambda (rec)
                        (list 'const
                              :tag (format "%s" rec)
                              :format "%t\n"
                              rec))
                      record-list)))
  (widget-create 'push-button
                 :notify 'refac-move-rec-apply
                 "Move")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify 'refac-move-rec-cleanup
                 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(defun refac-move-rec-apply (&rest args)
  (let ((recordlist (widget-value record-checklist))
        (target  (widget-value target-entry)))
    (when (equal target "")
      (error "No target module specified"))
    (when (equal recordlist nil)
      (error "No records selected"))
    (refac-send-command 'transform
                        'referl_tr_move_rec
                        (list (vector 'file source-file)
                              (vector 'reclist recordlist)
                              (vector 'filename target)))
  (refac-move-rec-cleanup)))

(defun refac-move-rec-cleanup (&rest args)
  (delete-window (get-buffer-window refac-move-rec-buffer))
  (kill-buffer refac-move-rec-buffer))

;; Implementation of non-interactive functionality

(defvar refac-server-process nil
  "The RefactorErl server process.")

(defun refactorerl-start ()
  "Checks if the RefactorErl server is running, and starts it if neccessary."
  (when (equal refactorerl-base-path "")
    (error "Configuration error: RefactorErl base path is not set"))
  (when (not (refac-server-is-running))
    (let* ((base-path (file-name-as-directory refactorerl-base-path))
           (server-cmd (concat base-path "bin/referl"))
           (server-data-dir (if (eq refactorerl-data-dir 'base)
                                   refactorerl-base-path
                                 refactorerl-data-dir))
           (server-args (list "-base" refactorerl-base-path
                              "-erl"  refactorerl-erlang-runtime)))
      (when (eq refactorerl-server-type 'shell)
        (let ((default-directory (file-name-as-directory server-data-dir)))
          (with-current-buffer
              (apply 'make-comint (append (list "RefactorErlShell"
                                                server-cmd
                                                nil)
                                          server-args))
            (when (require 'erlang nil t)
              (erlang-shell-mode)))))
      (setq refac-server-buffer
            (save-excursion (refac-make-server-buffer)))
      (setq refac-server-process
            (apply 'start-process
                   (append (list "RefactorErl" refac-server-buffer
                                 server-cmd)
                           server-args
                           (if (eq refactorerl-server-type 'internal)
                               (list "-emacs")
                             (list "-emacs" "-client" "-name" "referlemacs")))))
      (set-process-filter refac-server-process 'refac-output-filter)
      (set-process-sentinel refac-server-process 'refac-process-sentinel)
      )))

(defvar refac-output-buffer "")
(defun refac-output-filter (proc string)
  "Analyses server output, puts unrecognised messages into the process buffer."
  (when (equal proc refac-server-process)
    (setq refac-output-buffer (concat refac-output-buffer string))
    (save-match-data
      (let (eol line)
        (while (setq eol (string-match "\n" refac-output-buffer))
          (setq line (substring refac-output-buffer 0 eol))
          (setq refac-output-buffer (substring refac-output-buffer (1+ eol)))
          (if (or (equal line "")
                  (not (equal 2 (elt line 0))))
              (with-current-buffer (process-buffer proc)
                (save-excursion
                  (goto-char (point-max))
                  (widget-insert (concat line "\n"))))
            (let* ((data (read (substring line 1)))
                   (type (elt data 0))
                   (func (intern (concat "refac-handle-" (symbol-name type)))))
              (if (functionp func)
                  (apply func (list (elt data 1)))
                (error "unhandled message type %s" type)))))))))

(defun refac-process-sentinel (proc event)
  (when (equal proc refac-server-process)
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (concat "RefactorErl input process " event))))))

(defun refac-handle-options (data)
         (setq cluster-ui-buffer (generate-new-buffer "*Cluster Ui*"))
      (switch-to-buffer cluster-ui-buffer)
      (widget-insert
       (propertize "Clustering Buffer\n\n"
                   'face 'bold)
       (propertize "Options of the clustering\n\n"
                   'face 'italic)
       )
       (set (make-local-variable 'lista) (cdr data))
       (set (make-local-variable 'alg) (car data))
       (set (make-local-variable 'cl-options-list)
              (mapcar (lambda (fx)
                        (widget-create 'editable-field
                              :size 20
                              :format (concat (format "%s" (car fx))
                               (make-string
                                 (- 30 (length (format "%s"(car fx)))) ? )
                                ": %v\n")
                              (format "%s" (car (cdr fx)))
                              ))
                       lista
                     )
             )
  (widget-insert "\nSave result into a database : ")
  (set (make-local-variable 'create)
  (widget-create 'checkbox t)
  )
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify 'res-apply-aggl
                 "Run algorithm.")
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify 'cluster-ui-refresh
                 "Cleaning storage")
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify 'cluster-ui-cleanup
                 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
)

(defun refac-handle-flist (data)
  (widget-insert
         (concat "\nList of the Fitness numbers:\n----------------\n"
                   data "\n"))
)

(defun refac-handle-result (string)
       (set (make-local-variable 'str) (cdr string))
       (set (make-local-variable 'algo) (car string))
   (widget-insert
       (propertize "Result of the clustering: \n"
                 'face 'bold))
    (widget-insert "-------------------------\n")
    (widget-insert (format "%s" algo))
    (widget-insert
     (replace-regexp-in-string
        "fn" "\n(fitness number: "
      (replace-regexp-in-string
        "\((" "\n\n("
        (replace-regexp-in-string
         "\n)" ")"
         (replace-regexp-in-string
            "\((" "\("
          (replace-regexp-in-string
             "\(\n\(" "("
           (replace-regexp-in-string
              "\))" ")"
            (concat (prin1-to-string str) "\n")
           )
          )
         )
        )
       )
      )
   )
)


(defun refac-handle-error (string)
  (message "RefactorErl internal error: %s" string))

(defun refac-handle-status (string)
  (message "%s" string))

(defun refac-handle-reload (file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (revert-buffer t t t)))))

(defun refac-handle-backup (string)
  (message "%s" string))

(defun refac-handle-add (file)
  (refac-progress-report 'add file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (setq refac-buffer-state 'ok)))))

(defun refac-handle-drop (file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (setq refac-buffer-state 'off)))))

(defun refac-handle-reload (file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (revert-buffer t t t)))))

(defun refac-handle-rename (files)
  (let* ((frompath  (elt files 0))
         (topath    (elt files 1)))
    (let ((buf (get-file-buffer frompath)))
      (when buf
        (with-current-buffer buf
          (set-visited-file-name topath))))))

(defun refac-handle-invalid (file)
  (refac-progress-report 'err file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (setq refac-buffer-state 'err)))))

(defun refac-handle-filelist (file)
  (with-current-buffer (refac-list-buffer)
    (let ((inhibit-read-only t))
      (refac-file-list-add file))))

(defun refac-handle-question (desc)
  (let* ((id    (elt desc 0))
         (props (elt desc 1))
         (text  (refac-get-prop props 'text "?"))
         (type  (refac-get-prop props 'type 'string))
         (reply (refac-ask-question type text)))
    (cond ((not reply)
           (refac-send-command 'cancel id))
          (t
           (refac-send-command 'reply id reply)))))

(defun refac-ask-question (type text)
  (condition-case nil
      (cond ((eq type 'string)
             (read-string (concat text " ")))
            ((eq type 'yesno)
             (if (yes-or-no-p (concat text " "))
                 'yes 'no)))
    (quit nil)))

(defun refac-get-prop (props key default)
  (catch 'found
    (dolist (prop props default)
      (cond ((and (vectorp prop)
                  (eq (elt prop 0) key))
             (throw 'found (elt prop 1)))
            ((eq prop key)
             (throw 'found 'true))))))

(defvar refac-progress [])
(defun refac-progress-report (type file)
  (when (< 0 (length refac-progress))
    (let ((files (aref refac-progress 0))
          (err (aref refac-progress 1))
          (dir (aref refac-progress 2)))
      (when (member file files)
        (setq files (delete file files))
        (aset refac-progress 0 files)
        (when (equal type 'err)
          (setq err (1+ err))
          (aset refac-progress 1 err))
        (if (not (eq files nil))
            (message "Loading %s: %d remaining (%d errors)"
                     dir (length files) err)
          (message "Loading %s done (%d errors)" dir err)
          (setq refac-progress []))))))

(defun refac-list-buffer ()
  (get-buffer-create "*RefactorErl File List*"))

(defun refac-file-list-add (stname)
  (let* ((state (if (equal (elt stname 0) ?!) 'err 'ok))
         (name (if (equal state 'ok) stname (substring stname 1)))
         (dirname (file-name-directory name))
         (basename (file-name-nondirectory name)))
    (unless (equal dirname last-file-dir)
      (insert (concat (propertize dirname 'face 'dired-header) ":\n"))
      (setq last-file-dir dirname))
    (if (equal state 'ok)
        (insert "   ")
      (insert (propertize " ! " 'face 'compilation-error)))
    (insert-text-button basename
                        'filename name
                        'help-echo "mouse-2, RET: open file"
                        'action (lambda (btn)
                                  (find-file (button-get btn 'filename))))
    (insert " (")
    (insert-text-button "drop"
                        'filename name
                        'help-echo "mouse-2, RET: drop file"
                        'action (lambda (btn)
                                  (refac-send-command
                                   'drop (button-get btn 'filename))
                                  (refactorerl-list-files t)))
    (insert ")\n")))


(defun refac-file-saved ()
  "Informs the server that a file is changed. Runs from `after-save-hook'."
  (when (or (eq refac-buffer-state 'ok)
            (eq refac-buffer-state 'err))
    (setq refac-buffer-state nil)
    (refac-send-command 'add buffer-file-name)))

(defun refactorerl-update-status ()
  "Requests a file status update from the server."
  (interactive)
  (when buffer-file-name
    (refac-send-command 'status buffer-file-name)))

;; Server buffer manipulation

(defun refac-make-server-buffer (&rest args)
  (set-buffer (get-buffer-create "*RefactorErl*"))
  (kill-all-local-variables)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert "**RefactorErl Server Control Buffer**\n")
  (widget-create 'push-button
                 :notify 'refac-show-config
                 "Configuration")
  (widget-insert "\n")
  (set (make-local-variable 'config-start-marker) (point-marker))
  (widget-insert "------------------------------------------------\n")
  (widget-create 'push-button
                 :notify (lambda (&rest args)
                           (when (yes-or-no-p "Clear database contents? ")
                             (refac-send-command 'reset)))
                 "Reset database")
  (widget-insert " (Hint: open configuration and save it after reset)\n")
  (widget-create 'push-button
                 :notify 'refac-make-server-buffer
                 "Clear buffer")
  (widget-insert "\n================================================\n")
  (use-local-map
   (let ((map widget-keymap))
     (set-keymap-parent map refactorerl-mode-map)
     map))
  (widget-setup)
  (current-buffer))

(defun refac-show-config (&rest args)
  (refac-send-command 'showconfig))

(defun refac-handle-showconfig (config)
  (with-current-buffer refac-server-buffer
    (set (make-local-variable 'config-inc-dir) nil)
    (set (make-local-variable 'config-app-dir) nil)
    (set (make-local-variable 'config-out-dir) "/tmp")
    (if (equal config "") (setq config nil))
    (dolist (cfg config)
      (cond ((eq (elt cfg 0) 'appbase)
             (add-to-list 'config-app-dir (elt cfg 1)))
            ((eq (elt cfg 0) 'include)
             (add-to-list 'config-inc-dir (elt cfg 1)))
            ((eq (elt cfg 0) 'output)
             (setq config-out-dir (elt cfg 1)))))
    (goto-char config-start-marker)
    (widget-insert "Application directories:\n")
    (set (make-local-variable 'appdir-list)
         (widget-create 'editable-list
                        :value config-app-dir
                        '(editable-field)))
    (widget-insert "\nInclude directories:\n")
    (set (make-local-variable 'incdir-list)
         (widget-create 'editable-list
                        :value config-inc-dir
                        '(editable-field)))
    (widget-insert "\n")
    (set (make-local-variable 'outdir-menu)
         (widget-create 'menu-choice
                        :value config-out-dir
                        :tag "Output directory"
                        '(const :tag "Original" original)
                        '(editable-field :menu-tag "Specify" "")))
    (widget-insert "\n")
    (set (make-local-variable 'save-button)
         (widget-create 'push-button
                        :notify 'refac-save-config
                        "Save"))
    (set (make-local-variable 'cancel-button)
         (widget-create 'push-button
                        :notify 'refac-hide-config
                        "Cancel"))
    (widget-insert "\n")
    (set (make-local-variable 'config-end-marker) (point-marker))))

(defun refac-hide-config (&rest args)
  (widget-delete appdir-list)
  (widget-delete incdir-list)
  (widget-delete outdir-menu)
  (widget-delete save-button)
  (widget-delete cancel-button)
  (let ((inhibit-read-only t))
    (delete-region config-start-marker config-end-marker))
  (set-marker config-end-marker nil))

(defun refac-save-config (&rest args)
  (refac-send-command 'saveconfig
                      (widget-value appdir-list)
                      (widget-value incdir-list)
                      (widget-value outdir-menu))
  (refac-hide-config))

;; Generic supporting functions

(defun refac-send-command (cmd &rest args)
  "Send a command to the server. A tuple is sent to the server process, the
first element is `cmd', the rest comes from `args'. See `refac-erl-format'
for available types."
  (let ((msg (concat "{"
                     (mapconcat 'refac-erl-format
                                (append (list cmd) args) ",")
                     "}.\n")))
    ;(insert msg)
    (process-send-string refac-server-process msg)))


(defun refac-erl-format (data)
  "Turns a piece of data into Erlang string representation. Available types:
 * symbols become atoms
 * strings become strings
 * numbers become numbers
 * lists become lists with their elements converted recursively
 * vectors become tuples with theit elements converted recursively"
  (cond ((equal data nil)
         "[]")
        ((symbolp data)
         (symbol-name data))
        ((stringp data)
         (concat "\"" (apply 'concat (mapcar 'refac-escape-char data)) "\""))
        ((numberp data)
         (number-to-string data))
        ((listp data)
         (concat "[" (mapconcat 'refac-erl-format data ",") "]"))
        ((vectorp data)
         (concat "{" (mapconcat 'refac-erl-format data ",") "}"))))

(defun refac-escape-char (char)
  "Perform character escaping according to Erlang rules."
  (cond ((eq char ?\")     "\\\"")
        ((eq char ?\\)     "\\\\")
        ((or (< char 32)
             (> char 126)) (format "\\%03o" char))
        (t                 (string char))))



(defun refac-server-is-running ()
  "Checks if the RefactorErl server is running."
  (and refac-server-process
       (equal 'run (process-status refac-server-process))))

