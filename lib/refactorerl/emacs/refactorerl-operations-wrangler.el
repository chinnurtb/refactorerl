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


(provide 'refactorerl-operations-wrangler)
(require 'refactorerl-def)
(require 'refactorerl-operations)
(eval-when-compile
  (require 'cl))

(defmacro* with-wrangler-vars ((col line &optional col-end line-end) &body body)
  `(let ((,col (1+ (current-column)))
         (,line (line-number-at-pos)))
     ,@body))

(define-refac-operation/wrangler (refactorerl-wrangler-rename-variable :menu "Rename variable" :key "rv")  
  ((pos :point) (name "New variable name"))
  "Performs the Rename Variable refactoring via Wrangler.

1. Position the cursor over any instance of the variable.
2. Call the refactoring from the menu or with \\[refactorerl-wrangler-rename-variable].
3. Type the new variable name."
  (with-wrangler-vars (col line)
    (refac-transform/wrangler 'rename_variable :file buffer-file-name :line line :col col :varname name)))

(define-refac-operation/wrangler (refactorerl-wrangler-rename-function :menu "Rename function" :key "rf")  
  ((pos :point) (name "New function name"))
  "Performs the Rename Function refactoring via Wrangler.

1. Position the cursor over the name of the function in any
      clause of the function definition, any application of the
      function or the function in an export list.
2. Call the refactoring from the menu or with \\[refactorerl-rename-function].
3. Type the new function name."
  (with-wrangler-vars (col line)
    (refac-transform/wrangler 'rename_function :file buffer-file-name :line line :col col :name name)))

(define-refac-operation/wrangler (refactorerl-wrangler-rename-module :menu "Rename module" :key "rm")
  ((name "New module name"))
  (refac-transform/wrangler 'rename_module :file buffer-file-name :name name))

(define-refac-operation (refactorerl-wrangler-search-dups :menu "Search for code duplication" :menu-group nil
                                                          :precondition :buffer-state)
  ()  
  ;; ((num-tokens "Minimal number of tokens to match") (appearance-count "Minimal number of duplicates to detect"))  
  (let ((num-tokens 10)
        (appearance-count 2))
    (refac-send-command 'search_dup
                        (list (vector :dirs (list "."))
                              (vector :token num-tokens)
                              (vector :clone appearance-count)))))

