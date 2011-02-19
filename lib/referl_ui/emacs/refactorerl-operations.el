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

(provide 'refactorerl-operations)
(require 'refactorerl-def)
(eval-when-compile
  (require 'cl))

(defun refac-test-mode-p ()
  (and (boundp 'refac-test-mode) refac-test-mode))

(defun refac-transform* (wranglerp name &rest args)
  (let ((arglist
         (loop for (arg-name arg-value) on args by #'cddr
               unless (keywordp arg-name)
               do (warn "Argument name ~A is not a keyword" arg-name)
               collect (vector arg-name arg-value))))
    (cond ((not buffer-file-name)
           (error "No visited file"))
          ((refac-test-mode-p)
           (kill-new (refac-erl-format arglist)))
          ((and (not wranglerp) (not (eq refac-buffer-state :ok)))
           (error "File is not ready for refactoring"))
          (t
           (refac-send-command 'transform name arglist)))))
;           (refac-send-command (if wranglerp 'wr_transform 'transform) name arglist)))))

(defun refac-transform (name &rest args)
  (apply #' refac-transform* nil name (cons :ask_missing (cons 'true args))))

(defun refac-transform/wrangler (name &rest args)
  (when (eq refactorerl-wrangler-path 'disabled)
    (error "Wrangler mode turned off"))
  (apply #' refac-transform* t name args))

;; -----------------------------------------------------------------------------
;; Upgrade

(define-refac-operation (refactorerl-upgrade-regexp-iface :menu "Upgrade regexp interface"
                                                          :menu-group upgrade
                                                          :key "uir"
                                                          :precondition :buffer-state)
  ((pos :point))
  "Performs deprecated regexp module upgrade to new re module.

2. Call the refactoring from the menu or with \\[refactorerl-upgrade-regexp-iface]."
  (refac-transform 'upgrade_regexp
                   :file buffer-file-name))

;; -----------------------------------------------------------------------------
;; Introduce/eliminate constructs

(define-refac-operation (refactorerl-inline-function :menu "Eliminate function call" :key "ef"
                                                     :menu-group introelim
                                                     :precondition :buffer-state)
  ((pos :point))
  "Performs the Inline Function refactoring.

1. Position the cursor over a function application.
2. Call the refactoring from the menu or with \\[refactorerl-inline-function]."
  (refac-transform 'inline_fun
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-inline-macro :menu "Eliminate macro substitution" :key "em"
                                                     :menu-group introelim
                                                     :precondition :buffer-state)
  ((pos :point))
  "Performs the Inline Macro refactoring.

1. Position the cursor over a macro usage.
2. Call the refactoring from the menu or with \\[refactorerl-inline-macro]."
  (refac-transform 'inline_mac
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-eliminate-variable :menu "Eliminate variable" :key "ev"
                                                        :menu-group introelim
                                                        :precondition :buffer-state)
  ((pos :point))
  "Performs the Eliminate Variable refactoring.

1. Position the cursor over any instance of the variable.
2. Call the refactoring from the menu or with \\[refactorerl-eliminate-variable]."
  (refac-transform 'elim_var
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-eliminate-import :menu "Eliminate import"
                                                      :menu-group introelim
                                                      :key "ei"
                                                      :precondition :buffer-state)
  ((pos :point))
  "Performs the Eliminate Import refactoring.

1. Position the cursor over any import form.
2. Call the refactoring from the menu or with \\[refactorerl-eliminate-import]."
  (refac-transform 'eliminate_import
                   :file buffer-file-name
                   :position pos))

; todo This operation should be available under C-c C-r e f,
;      the same as eliminate function; the system should tell them apart.
(define-refac-operation (refactorerl-eliminate-funexpr :menu "Eliminate fun expression" :key "eu"
                                                       :menu-group introelim
                                                       :precondition :buffer-state)
  ((pos :point))
  "Performs the Eliminate Fun Expression refactoring
   that expands the fun expression.

1. Position the cursor over the name of the function to be expanded.
2. Call the refactoring from the menu or with \\[refactorerl-eliminate-funexpr]."
  (refac-transform 'expand_funexpr
                   :file buffer-file-name
                   :position pos))


(define-refac-operation (refactorerl-extract-function :menu "Introduce function" :key "if"
                                                      :menu-group introelim
                                                      :precondition :buffer-state)
  (((beg end) :region))
  "Performs the Extract Function refactoring. Operates on the expression or
expressions between the point and the mark.

1. Mark the exact part of the source you want to extract.
2. Call the refactoring from the menu or with \\[refactorerl-extract-function].
3. Type the new function name."
  (refac-transform 'extract_fun
                   :file buffer-file-name
                   :posrange (vector beg (1- end)))
  (deactivate-mark))

(define-refac-operation (refactorerl-introduce-import :menu "Introduce import"
                                                      :menu-group introelim
                                                      :key "ii"
                                                      :precondition :buffer-state)
  ((pos :point))
  "Performs the Introduce Import refactoring.

1. Position the cursor over any instance of the module qualifier.
2. Call the refactoring from the menu or with \\[refactorerl-introduce-import]."
  (refac-transform 'introduce_import
                   :file buffer-file-name
                   :position pos))


;; todo This transformation does not seem stable enough.
;; (define-refac-operation (refactorerl-introduce-macro :menu "Introduce macro" :key "mi"
;;                                                       :menu-group introelim
;;                                                       :precondition :buffer-state)
;;   (((beg end) :region))
;;   "Performs the Introduce Function refactoring. Operates on the expression or
;; expressions between the point and the mark.
;;
;; 1. Mark the exact part of the source you want to extract.
;; 2. Call the refactoring from the menu or with \\[refactorerl-introduce-macro].
;; 3. Type the new macro name."
;;   (refac-transform 'introduce_macro
;;                    :file buffer-file-name
;;                    :posrange (vector beg (1- end)))
;;   (deactivate-mark))

(define-refac-operation (refactorerl-introduce-rec :menu "Introduce record" :key "ir"
                                                   :menu-group introelim
                                                   :precondition :buffer-state)
  (((beg end) :region))
  "Performs the introduce record refactoring."
  (refac-transform 'introduce_rec
                    :file buffer-file-name
                    :posrange (vector beg (1- end)))
  (deactivate-mark))

(define-refac-operation (refactorerl-merge-expr :menu "Introduce variable" :key "iv"
                                                :menu-group introelim
                                                :help "Merges all instances of an expression"
                                                :precondition :buffer-state)
    (((beg end) :region))
  "Performs the Merge Expression Duplicates refactoring. Operates
on the expression between the point and the mark.

1. Mark the expression whose duplicates are to be merged.
2. Call the refactoring from the menu or with \\[refactorerl-merge-expr].
3. Type the new variable name."
  (refac-transform 'merge
                   :file buffer-file-name
                   :posrange (vector beg (1- end)))
  (deactivate-mark))


(define-refac-operation (refactorerl-introduce-process :menu "Introduce process"
                                                       :key "ip"
                                                       :menu-group introelim
                                                       :precondition :buffer-state)
  ((pos :point))
  "Performs the Introduce process refactoring.

1. Position the cursor over the name of the function application to be delegated to a separate process.
2. Call the refactoring from the menu or with \\[refactorerl-introduce-process]."
  (refac-transform 'funapp_to_proc
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-introduce-function-argument :menu "Introduce function argument"
                                                                 :key "ia"
                                                                 :menu-group introelim
                                                                 :precondition :buffer-state)
  (((beg end) :region))
  "Performs the Generalize Function refactoring. Operates on the expression or
expressions between the point and the mark.

1. Select the expression along which the generalization should be done.
2. Call the refactoring from the menu or with \\[refactorerl-introduce-function-argument].
3. Type the name for the new function argument."
  (refac-transform 'gen
                   :file buffer-file-name
                   :posrange (vector beg (1- end)))
  (deactivate-mark))


(define-refac-operation (refactorerl-introduce-tuple :menu "Introduce tuple"
                                                     :key "it"
                                                     :menu-group introelim
                                                     :help "Group parameters of the function into a tuple"
                                                     :precondition :buffer-state)
  (((beg end) :region))
  "Performs the Tuple Function Parameters refactoring. Operates on the
parameters of function which are marked.

1. Mark the arguments to be tupled in the function definition.
2. Call the refactoring from the menu or with \\[refactorerl-introduce-tuple]."
  (refac-transform 'tuple_funpar
                   :file buffer-file-name
                   :posrange (vector beg (1- end)))
  (deactivate-mark))


(define-refac-operation (refactorerl-transform-listcomp :menu "Transform list comprehension" :key "tl"
                                                        :menu-group introelim
                                                        :help "Turns lists:map or lists:filter into a list comprehension"
                                                        :precondition :buffer-state)
    (((beg end) :region))
  "If an application of lists:map/2 or lists:filter/2 is selected,
   the transformation turns it into the corresponding list comprehension.
   Conversely, if a list comprehension is selected,
   the transformation turns it into calls to the functions.

1. Mark the expression to be transformed.
2. Call the refactoring from the menu or with \\[refactorerl-transform-listcomp]."
  (refac-transform 'list_comp
                   :file buffer-file-name
                   :posrange (vector beg (1- end)))
  (deactivate-mark))


;; -----------------------------------------------------------------------------
;; Moves

(define-refac-operation (refactorerl-move-function :menu "Move function" :key "mf"
                                                   :menu-group move
                                                   :precondition :buffer-state)
  ()
  "Performs the Move Function refactoring. The functions to be
moved can be selected from a list.

1. Call the refactoring from the menu or with \\[refactorerl-move-function].
2. Fill out the form that pops up:
    - type the name of the target module,
    - select the records to be moved.
3. Start the transformation with the `Move' button."
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state :ok))
         (error "File is not ready for refactoring")))
  (refac-transform 'move_fun
                   :file buffer-file-name))

(define-refac-operation (refactorerl-move-macro :menu "Move macro" :key "mm"
                                                :menu-group move
                                                :precondition :buffer-state)
    ()
  "Performs the move macro refactoring. The macros to be
moved can be selected from a list.
1. Call the refactoring from the menu or with \\[refactorerl-move-macro].
2. Fill out the form that pops up:
    - type the name of the target module,
    - select the macros to be moved.
3. Start the transformation with the `Move' button."
  (unless buffer-file-name
    (error "No visited file"))
  (unless  (eq refac-buffer-state :ok)
    (error "File is not ready for refactoring"))
  (refac-transform 'move_mac
                   :file buffer-file-name))

(define-refac-operation (refactorerl-move-record :menu "Move record" :key "mr"
                                                 :menu-group move
                                                 :precondition :buffer-state)
    ()
  "Performs the move record refactoring. The records to be
moved can be selected from a list.
1. Call the refactoring from the menu or with \\[refactorerl-move-record].
2. Fill out the form that pops up:
    - type the name of the target module,
    - select the records to be moved.
3. Start the transformation with the `Move' button."
  (unless buffer-file-name
    (error "No visited file"))
  (unless  (eq refac-buffer-state :ok)
    (error "File is not ready for refactoring"))
  (refac-transform 'move_rec
                   :file buffer-file-name))

;; -----------------------------------------------------------------------------
;; Functions


(define-refac-operation (refactorerl-reorder-funpar :menu "Reorder function parameters" :key "of"
                                                    :menu-group func
                                                    :precondition :buffer-state)
  ((pos :point))
  "Performs the Reorder Function Parameters refactoring.

1. Position the cursor over the name of the function in any
      clause of the function definition.
2. Call the refactoring from the menu or with \\[refactorerl-reorder-funpar].
3. Type the name for the new order of the function arguments."
  (refac-transform 'reorder_funpar
                   :file buffer-file-name
                   :position pos))




;; -----------------------------------------------------------------------------
;; Rename

(define-refac-operation (refactorerl-rename-universal :menu "Universal renamer" :menu-group rename
                                                     :key "r"
                                                     :precondition :buffer-state)
  ((pos :point))
  "Tries to rename the entity under the cursor,
   using the appropriate rename operation."
  (refac-transform 'rename
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-rename-function :menu "Rename function" :menu-group rename
                                                     :key "xrf"
                                                     :precondition :buffer-state)
  ((pos :point))
  "Performs the Rename Function refactoring.

1. Position the cursor over the name of the function in any
      clause of the function definition, any application of the
      function or the function in an export list.
2. Call the refactoring from the menu or with \\[refactorerl-rename-function].
3. Type the new function name."
  (refac-transform 'rename_fun
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-rename-header :menu "Rename header"
                                                   :menu-group rename
                                                   :key "xrh"
                                                   :precondition :buffer-state)
  ()
  "Performs the Rename Header File refactoring.

1. Position the cursor in the header file.
2. Call the refactoring from the menu or with \\[refactorerl-rename-header].
3. Type the new header file name."
  (refac-transform 'rename_header
                   :file buffer-file-name))

(define-refac-operation (refactorerl-rename-macro :menu "Rename macro"
                                                  :menu-group rename
                                                  :key "xrc"
                                                  :precondition :buffer-state)
  ((pos :point))
  "Performs the Rename Macro refactoring.

1. Position the cursor over the name in the macro definition.
2. Call the refactoring from the menu or with \\[refactorerl-rename-macro].
3. Type the new macro name."
  (refac-transform 'rename_mac
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-rename-mod :menu "Rename module"
                                                :menu-group rename
                                                :key "xrm"
                                                :precondition :buffer-state)
  ()
  "Performs the Rename Module refactoring.

1. Position the cursor over the name in the module attribute.
2. Call the refactoring from the menu or with \\[refactorerl-rename-mod].
3. Type the new module name."
  (refac-transform 'rename_mod
                   :file buffer-file-name))

(define-refac-operation (refactorerl-rename-record :menu "Rename record"
                                                   :menu-group rename
                                                   :key "xrrd"
                                                   :precondition :buffer-state)
  ((pos :point))
  "Performs the Rename Record refactoring.

1. Position the cursor over the name in the record definition.
2. Call the refactoring from the menu or with \\[refactorerl-rename-record].
3. Type the new record name."
  (refac-transform 'rename_rec
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-rename-field :menu "Rename record field"
                                                  :menu-group rename
                                                  :key "xrrf"
                                                  :precondition :buffer-state)
  ((pos :point))
  "Performs the Rename Record Field refactoring.

1. Position the cursor over the field name in the record definition.
2. Call the refactoring from the menu or with \\[refactorerl-rename-field].
3. Type the new field name."
  (refac-transform 'rename_recfield
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-rename-variable :menu "Rename variable" :menu-group rename
                                                     :key "xrv"
                                                     :precondition :buffer-state)
  ((pos :point))
  "Performs the Rename Variable refactoring.

1. Position the cursor over any instance of the variable.
2. Call the refactoring from the menu or with \\[refactorerl-rename-variable].
3. Type the new variable name."
  (refac-transform 'rename_var
                   :file buffer-file-name
                   :position pos))
