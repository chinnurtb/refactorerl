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

(provide 'refactorerl-query)
(require 'refactorerl-def)
(eval-when-compile
  (require 'cl))

(defmacro* define-refac-query/point ((name &key menu key) query)
  `(define-refac-operation (,name :menu ,menu :key ,key :menu-group query :precondition :buffer-state)
     ((pos :point))
     (refac-send/callbacks ('transform 'semantic_query (list
      (vector 'querystr ,query)
      (vector 'start_opt (list
       (vector 'ask_missing 'false)
       (vector 'file buffer-file-name)
       (vector 'position pos)))
      (vector 'display_opt '([positions scalar] [output msg]))))
                          (:reply (ok queryres)
                                  (refac-handle-query-res queryres)))))

(define-refac-operation (refactorerl-semantic-query :menu "Run query"
                                                    :key "sq"
                                                    :menu-group query
                                                    :precondition :server)
  ((pos :point) (query "Query"))
  "Runs a semantic query."
     (refac-send/callbacks ('transform 'semantic_query (list
      (vector 'querystr query)
      (vector 'start_opt (list
       (vector 'ask_missing 'false)
       (vector 'file buffer-file-name)
       (vector 'position pos)))
      (vector 'display_opt '([positions scalar] [output msg]))))
                          (:reply (ok queryres)
                                  (refac-handle-query-res queryres))))


(define-refac-query/point (refactorerl-goto-def :menu "Go to definition" :key ("\M-."))
  "@def")

(define-refac-query/point (refactorerl-find-funrefs :menu "Find function references" :key "qr")
  "@fun.refs")

(define-refac-operation (refactorerl-metric-query   :menu "Run metric query"
                                                    :key "mq"
                                                    :menu-group query
                                                    :precondition :buffer-state)
  ((mquery "Metric Query"))
  "Runs a metric query."
     (refac-send/callbacks ('transform 'metric_query (list
      (vector 'querys mquery)))
                          (:reply (ok queryres)
                                  (refac-handle-metric-res queryres))))

(defun refac-handle-metric-res (args)
 (refac-handle-res 'show-metric-res args)
)

(defun refac-handle-query-res (args)
 (refac-handle-res 'show-query-res args)
)

(defun show-metric-res (string)
  (display-buffer (refac-buffer-list-ensure))
;  (refac-debug
;  (message "string: %s" string))
  (with-refac-buffer-list
   (erase-buffer)
   (widget-insert string "\n")))

(defun refac-handle-res (handler args)
  (setq type (elt args 0))
  (setq data (elt args 1))
  (case type
    ('result
      (progn
       (setq results nil)
       (loop for kv in data when (equal (elt kv 0) 'result)
             do (setq results (elt kv 1)))
       (if (or (equal results "") (not results))
        (message "RefactorErl: No results.")
        (apply handler (cons results ())))))
    ('abort (let ((text (elt data 1)))
             (message "RefactorErl: denied: %s" text)))
    ('error (let ((text (elt data 1)))
             (message "RefactorErl: error: %s" text)))
   (otherwise         (message "RefactorErl: unknown error: %s" args))))

(defvar refac-find-refs-popup t)
(defvar refac-find-refs-highlight nil)

(defun show-query-res (results)
    (flet ((result-no-pos (result)
                          (equal (elt result 0) 'nopos)))
      (let ((highlight (or refac-find-refs-highlight
                           ;; (= (length (remove-if #'result-no-pos results)) 1)
                           )))
        (when refac-find-refs-popup
          (with-refac-buffer-list
           (erase-buffer)))
        (when highlight
          (refac-start-overlay-group))
        (dolist (result results)
          (if (result-no-pos result)
              (when refac-find-refs-popup
                (with-refac-buffer-list
                 (widget-insert (elt result 1))))
            (destructuring-bind ((file start-pos end-pos) text)
                (list-from-vector result)
              (when refac-find-refs-popup
                (with-refac-buffer-list
                 (refac-widget-link file :start-pos start-pos :end-pos (1+ end-pos) :label text)))
              (when highlight
                (refac-highlight file :start-pos start-pos :end-pos (1+ end-pos)))
              )))
        (when refac-find-refs-popup
          (pop-to-buffer (refac-buffer-list-ensure))))))
