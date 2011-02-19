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
(eval-when-compile
  (require 'wid-edit)
  (require 'cl))

(provide 'refactorerl-form)

(defmacro* destructuring-case (expr &body cases)
  (let ((e (gensym))
        (headvar (gensym)))
    `(let ((,e ,expr))
       (case (car ,e)
         ,@(loop for ((head . paramspec) . body) in cases
                 collect `(',head
                           (destructuring-bind (,headvar ,@paramspec) ,e
                             ,@body)))))))

(defmacro* with-form-popup (buffer-name &body body)
  (let ((buffer (gensym)))
    `(let ((,buffer (generate-new-buffer ,buffer-name)))
       (with-current-buffer ,buffer
         ,@body
         (use-local-map widget-keymap)
         (widget-setup)
         (widget-forward 1))
       (display-buffer ,buffer)
       (pop-to-buffer ,buffer t)
       )))

(defun add-label (text)
  (when text
    (widget-insert text)
    (widget-insert " ")))

(defun create-form (fields cb-submit cb-cancel)
  "Pops up a form showing `fields', then calls `cb-submit' with a list containing the value of each field"
  (with-form-popup "query"
    (lexical-let ((buffer (current-buffer))
                  (widgets (list))
                  (cb-submit cb-submit)
                  (cb-cancel cb-cancel))

      (lexical-let ((radios (list)))
        (dolist (fieldspec fields)
          (push (destructuring-case fieldspec
                                    ((info &key text &allow-other-keys)
                                     (widget-insert text)
                                     (widget-insert "\n\n")
                                     nil)
                                    ((checkbox &key text default &allow-other-keys)
                                     (prog1 (widget-create 'checkbox )
                                       (add-label text)
                                       (widget-insert "\n")))
                                    ((textbox &key text default &allow-other-keys)
                                     (when (= default -1) (setf default nil))
                                     (add-label text)
                                     (prog1 (widget-create 'editable-field (or default ""))))

                                    ((radio &key text default &allow-other-keys)
                                     (let ((widget (widget-create 'radio-button
                                                                  :notify (lambda (self &rest ignore)
                                                                            (dolist (radio radios)
                                                                              (unless (eql radio self)
                                                                                (widget-value-set radio nil)))))))
                                       (add-label text)
                                       (widget-insert "\n")
                                       (push widget radios)
                                       widget)))
                widgets)))

      (widget-insert "\n")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (let ((results (list)))
                                 (dolist (widget widgets)
                                   (push (if widget (widget-value widget) 'info) results))
                                 (kill-buffer buffer)
                                 (delete-window (get-buffer-window buffer))
                                 (apply cb-submit results)))
                     "Submit")
      (widget-insert " ")

      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (funcall cb-cancel)
                               (delete-window (get-buffer-window buffer))
                               (kill-buffer buffer))
                     "Cancel")
      (widget-insert "\n"))))

(defun refactorerl-test-form ()
  (interactive)
  (let ((fields
         ;; TODO: radio
         '((info :text "Select macros to move")
           (checkbox :text "Macro1" :default nil)
           (checkbox :text "Macro2" :default nil)
           (textbox :text "Destination module" :validator file))))
    (create-form fields (lambda (&rest results)
                          (message "Form results: %s" results)))))

(defun parse-formspec (formspec)
  (loop for fieldspec in formspec
        collect  (let* (format
                        (plist (loop for (key value) in fieldspec
                                     when (equal key 'format) do (setf format value)
                                     else append (list (intern (concat ":" (symbol-name key))) value))))
                   (cons format plist))))

(defun refactorerl-test-rename-fun ()
  (interactive)
  (let ((fields '((textbox :text "The given function name is already used. Please type in a new function name:" :default nil))))
        (create-form fields (lambda (new-name)
                          (message "Form results: %s" new-name)))))

