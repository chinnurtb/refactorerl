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

(provide 'refactorerl-flasher)
(require 'eldoc)
(eval-when-compile
  (require 'cl))

(defun refactorerl-eldoc ()
  ;; TODO: Call async context-sensitive info getter
  nil)

(defun refactorerl-flasher (enablep)
  (if (not enablep)
      (eldoc-mode 0)
    (make-local-variable 'eldoc-documentation-function)
    (setf eldoc-documentation-function 'refactorerl-eldoc)
    (eldoc-mode 1)))
