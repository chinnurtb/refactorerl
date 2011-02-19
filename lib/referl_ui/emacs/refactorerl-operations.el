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
           (refac-send-command (if wranglerp 'wr_transform 'transform) name arglist)))))

(defun refac-transform (name &rest args)
  (apply #' refac-transform* nil name args))

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
  (refac-transform 'reftr_upgrade_regexp
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
  (refac-transform 'reftr_inline_fun
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-inline-macro :menu "Eliminate macro substitution" :key "em"
                                                     :menu-group introelim
                                                     :precondition :buffer-state)
  ((pos :point))
  "Performs the Inline Macro refactoring.

1. Position the cursor over a macro usage.
2. Call the refactoring from the menu or with \\[refactorerl-inline-macro]."
  (refac-transform 'reftr_inline_mac
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-eliminate-variable :menu "Eliminate variable" :key "ev"
                                                        :menu-group introelim
                                                        :precondition :buffer-state)
  ((pos :point))
  "Performs the Eliminate Variable refactoring.

1. Position the cursor over any instance of the variable.
2. Call the refactoring from the menu or with \\[refactorerl-eliminate-variable]."
  (refac-transform 'reftr_elim_var
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-extract-function :menu "Introduce function" :key "if"
                                                      :menu-group introelim
                                                      :precondition :buffer-state)
  (((beg end) :region) (name "Function name"))
  "Performs the Extract Function refactoring. Operates on the expression or
expressions between the point and the mark.

1. Mark the exact part of the source you want to extract.
2. Call the refactoring from the menu or with \\[refactorerl-extract-function].
3. Type the new function name."
  (refac-transform 'reftr_extract_fun
                   :file buffer-file-name
                   :posrange (vector beg (1- end))
                   :name name)
  (deactivate-mark))

(define-refac-operation (refactorerl-introduce-import :menu "Introduce import"
                                                      :menu-group introelim
                                                      :key "ii"
                                                      :precondition :buffer-state)
  ((pos :point))
  "Performs the Introduce Import refactoring.

1. Position the cursor over any instance of the module qualifier.
2. Call the refactoring from the menu or with \\[refactorerl-introduce-import]."
  (refac-transform 'reftr_introduce_import
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
  (refac-transform 'reftr_eliminate_import
                   :file buffer-file-name
                   :position pos))


;; todo This transformation does not seem stable enough.
;; (define-refac-operation (refactorerl-introduce-macro :menu "Introduce macro" :key "mi"
;;                                                       :menu-group introelim
;;                                                       :precondition :buffer-state)
;;   (((beg end) :region) (name "Macro name"))
;;   "Performs the Introduce Function refactoring. Operates on the expression or
;; expressions between the point and the mark.
;;
;; 1. Mark the exact part of the source you want to extract.
;; 2. Call the refactoring from the menu or with \\[refactorerl-introduce-macro].
;; 3. Type the new macro name."
;;   (refac-transform 'reftr_introduce_macro
;;                    :file buffer-file-name
;;                    :posrange (vector beg (1- end))
;;                    :name name)
;;   (deactivate-mark))

(define-refac-operation (refactorerl-introduce-rec :menu "Introduce record" :key "ir"
                                                   :menu-group introelim
                                                   :precondition :buffer-state)
  (((beg end) :region) (name "The record name") (fields "The recored fields separated by whitespace"))
  "Performs the introduce record refactoring."
  (refac-transform 'reftr_introduce_rec
                    :file buffer-file-name
                    :posrange (vector beg (1- end))
                    :name name
                    :text fields)
  (deactivate-mark))

(define-refac-operation (refactorerl-merge-expr :menu "Introduce variable" :key "iv"
                                                :menu-group introelim
                                                :help "Merges all instances of an expression"
                                                :precondition :buffer-state)
    (((beg end) :region) (varname "New variable name"))
  "Performs the Merge Expression Duplicates refactoring. Operates
on the expression between the point and the mark.

1. Mark the expression whose duplicates are to be merged.
2. Call the refactoring from the menu or with \\[refactorerl-merge-expr].
3. Type the new variable name."
  (refac-transform 'reftr_merge
                   :file buffer-file-name
                   :posrange (vector beg (1- end))
                   :varname varname)
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
  (refac-transform 'reftr_list_comp
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
         (error "File is not ready for refactoring"))
        (t
         (refac-move-fun-params))))

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
  (refac-move-mac-params))

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
  (refac-move-rec-params))

;; -----------------------------------------------------------------------------
;; Functions

(define-refac-operation (refactorerl-funapp-to-proc :menu "Delegate function application to new process" :key "fp"
                                                    :menu-group func
                                                    :precondition :buffer-state)
  ((pos :point))
  "Performs the Delegate function application to new process refactoring.

1. Position the cursor over the name of the function application to be delegated to a separate process.
2. Call the refactoring from the menu or with \\[refactorerl-funapp-to-proc]."
  (refac-transform 'reftr_funapp_to_proc
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-expand-funexpr :menu "Expand fun expression" :key "xf"
                                                    :menu-group func
                                                    :precondition :buffer-state)
  ((pos :point))
  "Performs the Expand Fun Expression refactoring.

1. Position the cursor over the name of the function to be expanded.
2. Call the refactoring from the menu or with \\[refactorerl-expand-funexpr]."
  (refac-transform 'reftr_expand_funexpr
                   :file buffer-file-name
                   :position pos))

(define-refac-operation (refactorerl-generalize-function :menu "Generalize function definition" :key "gf"
                                                         :menu-group func
                                                         :precondition :buffer-state)
  (((beg end) :region) (name "New parameter name"))
  "Performs the Generalize Function refactoring. Operates on the expression or
expressions between the point and the mark.

1. Select the expression along which the generalization should be done.
2. Call the refactoring from the menu or with \\[refactorerl-generalize-function].
3. Type the name for the new function argument."
  (refac-transform 'reftr_gen
                   :file buffer-file-name
                   :posrange (vector beg (1- end))
                   :varname name)
  (deactivate-mark))

(define-refac-operation (refactorerl-reorder-funpar :menu "Reorder function parameters" :key "of"
                                                    :menu-group func
                                                    :precondition :buffer-state)
  ((pos :point) (order "New order (e.g. 3 1 2)"))
  "Performs the Reorder Function Parameters refactoring.

1. Position the cursor over the name of the function in any
      clause of the function definition.
2. Call the refactoring from the menu or with \\[refactorerl-reorder-funpar].
3. Type the name for the new order of the function arguments."
  (refac-transform 'reftr_reorder_funpar
                   :file buffer-file-name
                   :position pos
                   :order (car (read-from-string
                                (concat "(" order ")")))))

(define-refac-operation (refactorerl-tuple-funpar :menu "Tuple function parameters" :key "tf"
                                                  :menu-group func
                                                  :help "Group parameters of the function into a tuple"
                                                  :precondition :buffer-state)
  (((beg end) :region))
  "Performs the Tuple Function Parameters refactoring. Operates on the
parameters of function which are marked.

1. Mark the arguments to be tupled in the function definition.
2. Call the refactoring from the menu or with \\[refactorerl-tuple-funpar]."
  (refac-transform 'reftr_tuple_funpar
                   :file buffer-file-name
                   :posrange (vector beg (1- end)))
  (deactivate-mark))



;; -----------------------------------------------------------------------------
;; Rename

(define-refac-operation (refactorerl-rename-function :menu "Rename function" :menu-group rename
                                                     :key "rf"
                                                     :precondition :buffer-state)
  ((pos :point) (name "New function name"))
  "Performs the Rename Function refactoring.

1. Position the cursor over the name of the function in any
      clause of the function definition, any application of the
      function or the function in an export list.
2. Call the refactoring from the menu or with \\[refactorerl-rename-function].
3. Type the new function name."
  (refac-transform 'reftr_rename_fun
                   :file buffer-file-name
                   :position pos
                   :name name))

(define-refac-operation (refactorerl-rename-header :menu "Rename header"
                                                   :menu-group rename
                                                   :key "rh"
                                                   :precondition :buffer-state)
  ((name "New headerfile name"))
  "Performs the Rename Header File refactoring.

1. Position the cursor in the header file.
2. Call the refactoring from the menu or with \\[refactorerl-rename-header].
3. Type the new header file name."
  (refac-transform 'reftr_rename_header
                   :file buffer-file-name
                   :filename name))

(define-refac-operation (refactorerl-rename-macro :menu "Rename macro" :menu-group rename :key "rc"
                                                   :precondition :buffer-state)
  ((pos :point) (name "New macro name"))
  "Performs the Rename Macro refactoring.

1. Position the cursor over the name in the macro definition.
2. Call the refactoring from the menu or with \\[refactorerl-rename-macro].
3. Type the new macro name."
  (refac-transform 'reftr_rename_mac
                   :file buffer-file-name
                   :position pos
                   :macname name))

(define-refac-operation (refactorerl-rename-mod :menu "Rename module"
                                                :menu-group rename
                                                :key "rm"
                                                :precondition :buffer-state)
  ((name "New module name"))
  "Performs the Rename Module refactoring.

1. Position the cursor over the name in the module attribute.
2. Call the refactoring from the menu or with \\[refactorerl-rename-mod].
3. Type the new module name."
  (refac-transform 'reftr_rename_mod
                   :file buffer-file-name
                   :name name))

(define-refac-operation (refactorerl-rename-record :menu "Rename record" :menu-group rename :key "rrd"
                                                   :precondition :buffer-state)
  ((pos :point) (name "New record name"))
  "Performs the Rename Record refactoring.

1. Position the cursor over the name in the record definition.
2. Call the refactoring from the menu or with \\[refactorerl-rename-record].
3. Type the new record name."
  (refac-transform 'reftr_rename_rec
                   :file buffer-file-name
                   :position pos
                   :name name))

(define-refac-operation (refactorerl-rename-field :menu "Rename record field" :menu-group rename :key "rrf"
                                                  :precondition :buffer-state)
  ((pos :point) (name "New field name"))
  "Performs the Rename Record Field refactoring.

1. Position the cursor over the field name in the record definition.
2. Call the refactoring from the menu or with \\[refactorerl-rename-field].
3. Type the new field name."
  (refac-transform 'reftr_rename_recfield
                   :file buffer-file-name
                   :position pos
                   :name name))

(define-refac-operation (refactorerl-rename-variable :menu "Rename variable" :menu-group rename
                                                     :key "rv"
                                                     :precondition :buffer-state)
  ((pos :point) (name "New variable name"))
  "Performs the Rename Variable refactoring.

1. Position the cursor over any instance of the variable.
2. Call the refactoring from the menu or with \\[refactorerl-rename-variable].
3. Type the new variable name."
  (refac-transform 'reftr_rename_var
                   :file buffer-file-name
                   :position pos
                   :varname name))
