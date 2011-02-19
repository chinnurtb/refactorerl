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
     (refac-transform 'refusr_sq :querystr ,query :file buffer-file-name :position pos)))

(define-refac-operation (refactorerl-semantic-query :menu "Run query"
                                                    :key "sq"
                                                    :menu-group query
                                                    :precondition :server)
  ((pos :point) (query "Query"))
  "Runs a semantic query."
  (refac-transform 'refusr_sq
                   :querystr query
                   :file buffer-file-name
                   :position pos))


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
  (refac-transform 'refusr_metrics
                   :querys mquery))
