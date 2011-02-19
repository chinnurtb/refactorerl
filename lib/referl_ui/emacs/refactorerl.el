;; -*- coding: utf-8 -*-

;; The  contents of this  file are  subject to  the Erlang  Public License,
;; Version  1.1, (the  "License");  you may  not  use this  file except  in
;; compliance  with the License.  You should  have received  a copy  of the
;; Erlang  Public License  along  with this  software.  If not,  it can  be
;; retrieved at http://plc.inf.elte.hu/erlang/
;;
;; Software  distributed under  the License  is distributed  on an  "AS IS"
;; basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
;; the License  for the specific language governing  rights and limitations
;; under the License.
;;
;; The Original Code is RefactorErl.
;;
;; The Initial Developer of the  Original Code is Eötvös Loránd University.
;; Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
;; Eötvös Loránd University. All Rights Reserved.

(require 'widget)
(require 'cl)
(eval-when-compile
  (require 'wid-edit)
  (require 'cl))

(require 'refactorerl-operations)
(require 'refactorerl-customization)
(require 'refactorerl-server)
(require 'refactorerl-mode)

(provide 'refactorerl)

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
        ((not (eq refac-buffer-state :off))
         (error "Already added"))
        (t
         (refac-set-buffer-state nil)
         (refac-send-command 'add buffer-file-name))))

(defun refactorerl-drop-file ()
  "Remove the visited file from the active refactoring set."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((eq refac-buffer-state :off)
         (error "Not in the active set"))
        (t
         (refac-set-buffer-state nil)
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

(defun refactorerl-cluster-agglom ()
  (interactive)
  (cluster-ui-options-agglom))

(defun refactorerl-cluster-genetic ()
  (interactive)
  (cluster-ui-options-genetic))

(defun refactorerl-undo ()
  "Steps backward on the refactoring database."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state :ok))
         (error "File is not ready for refactoring"))
        ((yes-or-no-p "All changes since last refactoring will be lost. Continue? ")
         (refac-send-command 'undo buffer-file-name))))

(defun refactorerl-list-files (&optional same-win)
  "Shows the contents of the active refactoring set."
  (interactive)
  (let ((buf (refac-buffer-list-ensure)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set (make-local-variable 'last-file-dir) "")))
    (refac-send-command 'filelist)    
    (pop-to-buffer buf (not same-win))))

;; TODO: unify this with list-files and other popup commands
(defun refactorerl-show-errors ()
  "Shows the parser's error messages"
  (interactive)
  (let ((buf (refac-buffer-list-ensure)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (refac-send-command 'error_attr)
    (pop-to-buffer buf t)))


(defun refactorerl-load-dir (dirname)
  "Adds the contents of a directory to the active refactoring set."
  (interactive "D")
  (let ((files (directory-files dirname t ".*\\.erl$")))
    (when (equal 0 (length files))
      (error (concat "No Erlang files in " dirname)))
    (setf refac-progress (list files 0 dirname))
    (dolist (file files)
      (refac-send-command 'add file))))

;; Cluster Ui

(defvar cluster-ui-buffer nil)
(defun cluster-ui-options-genetic()
  (refac-send-command 'cl_options 'genetic))

(defun cluster-ui-options-agglom()
  (refac-send-command 'cl_options 'agglom_attr))


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
    (refac-send-command 'run_cl (reverse value) algo createdb)))

(defun cluster-ui-refresh (&rest args)
   (refac-send-command 'cl_refresh))


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

(defun refac-move-fun-apply (&rest args)
  (let ((funlist (widget-value function-checklist))
        (target  (widget-value target-entry)))
    (when (equal target "")
      (error "No target module specified"))
    (when (equal funlist nil)
      (error "No functions selected"))
    (refac-send-command 'transform
                        'reftr_move_fun
                        (list (vector :file source-file)
                              (vector :funlist funlist)
                              (vector :name target)))
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

(defun refac-move-rec-apply (&rest args)
  (let ((recordlist (widget-value record-checklist))
        (target  (widget-value target-entry)))
    (when (equal target "")
      (error "No target module specified"))
    (when (equal recordlist nil)
      (error "No records selected"))
    (refac-send-command 'transform
                        'reftr_move_rec
                        (list (vector :file source-file)
                              (vector :reclist recordlist)
                              (vector :filename target)))
  (refac-move-rec-cleanup)))

(defun refac-move-rec-cleanup (&rest args)
  (delete-window (get-buffer-window refac-move-rec-buffer))
  (kill-buffer refac-move-rec-buffer))

;; Move macro

(defvar refac-move-mac-buffer nil)
(defun refac-move-mac-params ()
  (let ((source (buffer-file-name)))
    (with-current-buffer
        (setq refac-move-mac-buffer (generate-new-buffer "*Move Macro*"))
      (widget-insert
       (propertize "Move macros to another file\n"
                   'face 'bold))
      (widget-insert (concat "Source file: " source "\n"))
      (set (make-local-variable 'target-entry)
           (widget-create 'editable-field
                          :size 30
                          :format "Target file path: %v"
                          ""))
      (widget-insert "\nSelect macros to be moved:\n")
      (set (make-local-variable 'source-file) source))
    (refac-send-command 'macrolist source)))

(defun refac-move-mac-apply (&rest args)
  (let ((macrolist (widget-value macro-checklist))
        (target  (widget-value target-entry)))
    (when (equal target "")
      (error "No target module specified"))
    (when (equal macrolist nil)
      (error "No macros selected"))
    (refac-send-command 'transform
                        'reftr_move_mac
                        (list (vector :file source-file)
                              (vector :maclist macrolist)
                              (vector :filename target)))
  (refac-move-mac-cleanup)))

(defun refac-move-mac-cleanup (&rest args)
  (delete-window (get-buffer-window refac-move-mac-buffer))
  (kill-buffer refac-move-mac-buffer))

;; Implementation of non-interactive functionality

(defun refac-ask-question (type text)
  (condition-case nil
      (cond ((eq type 'string)
             (read-string (concat text " ")))
            ((eq type 'yesno)
             (if (yes-or-no-p (concat text " "))
                 'yes 'no))
	    ((destructuring-bind (select values)(list-from-vector type)
	       (cond ((eq select 'select)
		      (widget-choose text (pairlis values values)))))))
    (quit nil)))