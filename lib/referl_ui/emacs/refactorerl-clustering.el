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

(provide 'refactorerl-clustering)

(defun refactorerl-cluster-agglom ()
  (interactive)
  (refac-send/callbacks ('transform 'clustering (list
                   (vector 'algorithm 'agglom_attr)
                   (vector 'entity 'module)))))

(defun refactorerl-cluster-genetic ()
  (interactive)
  (refac-send/callbacks ('transform 'clustering (list
                   (vector 'algorithm 'genetic)
                   (vector 'entity 'module)))))

(defun refactorerl-cluster-function ()
  (interactive)
  (refac-send/callbacks ('transform 'clustering (list
                   (vector 'algorithm 'agglom_attr)
                   (vector 'entity 'function)))))  

(defvar cluster-ui-buffer nil)
  
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
