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


(provide 'refactorerl-def)

(defvar refactorerl-mode-map (make-sparse-keymap))
(defvar refactorerl-mode-keybindings nil)
(defvar refactorerl-mode-keybindings/wrangler nil)
(defvar refactorerl-mode-menu nil)
(defvar refactorerl-mode-menu/ops nil)
(defvar refactorerl-mode-menu/wrangler nil)

(eval-when-compile
 (require 'cl))
(defmacro* defun/interactive (name (&rest args) doc &body body)
  ;; TODO: Support for special initializers not actually supported by (interactive), such as line and col number
  (flet ((interactive-code (init-spec)
           (if (stringp init-spec)
               (interactive-code `(:string ,init-spec))
               (destructuring-bind (init-op &rest init-args) (if (consp init-spec) init-spec (list init-spec))
                 (ecase init-op
                   ((:string)
                    (concat "s" (apply #'concat init-args) ": "))
                   ((:point)
                    "d")
                   ((:region)
                    "r")))))
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
 
(defmacro* define-refac-operation ((name &key (menu-group "Refactoring") menu key precondition help) args
                                   doc &body body)
  `(progn
     ,(when key
            `(push '(,key ,name) refactorerl-mode-keybindings))
     ,(when menu
            `(progn
               (unless (assoc ,menu-group refactorerl-mode-menu/ops)
                 (setf refactorerl-mode-menu/ops (push (cons ,menu-group (list)) refactorerl-mode-menu/ops)))
               (setf (cdr (assoc ,menu-group refactorerl-mode-menu/ops))
                     (push '(,menu ,name :enable ,precondition) (cdr (assoc ,menu-group refactorerl-mode-menu/ops))))))      

     (defun/interactive ,name (,@args) ,doc ,@body)))

 ;; TODO: Merge this with define-refac-operation
 
(defmacro* define-refac-operation/wrangler ((name &key menu key help) args
                                            doc &body body)
  `(progn
     ,(when key
            `(push '(,key ,name) refactorerl-mode-keybindings/wrangler))
     ,(when menu
            `(push '(,menu ,name :enable (not (eq refactorerl-wrangler-path 'disabled)))
                   refactorerl-mode-menu/wrangler))
      
     (defun/interactive ,name (,@args) ,doc ,@body)))

