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

(provide 'refactorerl-def)

(eval-when-compile
 (require 'cl))

(defvar refactorerl-menu-groups (list))
(defstruct (menu-group)
  (label)
  (children))


(defmacro* define-menu-group (name label)
  `(progn
     (when (assq (quote ,name) refactorerl-menu-groups)
       (warn "Menu group %s already defined" ,name))
     (setf refactorerl-menu-groups (cons (cons (quote ,name) (make-menu-group :label ,label :children (list))) refactorerl-menu-groups))))

(define-menu-group refactor "Refactoring")
(define-menu-group rename "Renaming")
(define-menu-group query "Semantic query")
(define-menu-group wrangler "Wrangler")
(define-menu-group dups "Detect duplicates with Wrangler")

(defvar refactorerl-mode-map (make-sparse-keymap))
(defvar refactorerl-mode-keybindings nil)
(defvar refactorerl-mode-keybindings/wrangler nil)
(defvar refactorerl-mode-menu nil)
(defvar refactorerl-mode-menu/ops nil)
(defvar refactorerl-mode-menu/wrangler nil)

(defun* add-to-menu-group (group-name name label &key enable)
  (let ((menu-group (cdr (assq group-name refactorerl-menu-groups))))
    (unless menu-group
      (error "Menu group %s not defined" group-name))
    (setf (menu-group-children menu-group)
          (append (menu-group-children menu-group) (list (list label name :enable enable))))))

(defmacro* defun/interactive (name (&rest args) doc &body body)
  ;; TODO: Support for special initializers not actually supported by (interactive), such as line and col number
  (flet ((interactive-code (init-spec)
           (if (stringp init-spec)
               (interactive-code `(:string ,init-spec))
               (destructuring-bind (init-op &rest init-args) (if (consp init-spec) init-spec (list init-spec))
                 (flet ((prompt (directive)
                          (concat directive (apply #'concat init-args) ": ")))
                   (ecase init-op
                     ((:string)
                      (prompt "s"))                     
                     ((:number)
                      (prompt "n"))                     
                     ((:dir)
                      (prompt "D"))                     
                     ((:file)
                      (prompt "f"))                     
                     ((:newfile)
                      (prompt "F"))
                     ((:point)
                      "d")
                     ((:region)
                      "r"))))))         
         (collect-argument (arg-spec)
           (cond ((atom arg-spec) (list arg-spec))
                 ((consp (car arg-spec)) (car arg-spec))
                 (t (list (car arg-spec))))))

    (let ((args/names (loop for arg in args
                            append (collect-argument arg)))
          (args/interactive (mapconcat (lambda (arg)
                                         (interactive-code (if (consp arg) (cadr arg) (symbol-name arg))))
                                       args "\n")))
      `(defun ,name ,args/names
         ,@(when (stringp doc) (list doc))
         ,(if args/interactive
              `(interactive ,args/interactive)
              '(interactive))
         ,(unless (stringp doc) doc)
         ,@body))))

(defmacro* define-refac-operation ((name &key (menu-group 'refactor) menu key precondition help) args
                                   doc &body body)
  `(progn
     ,(when key
        `(push '(,key ,name) refactorerl-mode-keybindings))
     ,(when menu
        `(add-to-menu-group ',menu-group ',name ,menu :enable ,precondition))
     (defun/interactive ,name (,@args) ,doc ,@body)))

 ;; TODO: Merge this with define-refac-operation
(defmacro* define-refac-operation/wrangler ((name &key menu key help) args
                                            doc &body body)
  `(progn
     ,(when key
        `(push '(,key ,name) refactorerl-mode-keybindings/wrangler))
     ,(when menu        
        `(add-to-menu-group 'wrangler ',name ,menu :enable :wrangler))     
     (defun/interactive ,name (,@args) ,doc ,@body)))


