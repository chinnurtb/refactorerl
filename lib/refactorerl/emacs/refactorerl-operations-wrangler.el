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

(provide 'refactorerl-operations-wrangler)
(require 'refactorerl-def)
(require 'refactorerl-operations)
(require 'refactorerl-ui)
(eval-when-compile
  (require 'cl))

(define-refac-operation/wrangler (refactorerl-wrangler-rename-variable :menu "Rename variable" :key "rv")
  ((pos :point) (name "New variable name"))
  "Performs the Rename Variable refactoring via Wrangler.

1. Position the cursor over any instance of the variable.
2. Call the refactoring from the menu or with \\[refactorerl-wrangler-rename-variable].
3. Type the new variable name."
  (refac-transform/wrangler 'rename_variable :file buffer-file-name :position pos :varname name))

(define-refac-operation/wrangler (refactorerl-wrangler-rename-function :menu "Rename function" :key "rf")
  ((pos :point) (name "New function name"))
  "Performs the Rename Function refactoring via Wrangler.

1. Position the cursor over the name of the function in any
      clause of the function definition, any application of the
      function or the function in an export list.
2. Call the refactoring from the menu or with \\[refactorerl-wrangler-rename-function].
3. Type the new function name."
  (refac-transform/wrangler 'rename_function :file buffer-file-name :position pos :name name))

(define-refac-operation/wrangler (refactorerl-wrangler-rename-module :menu "Rename module" :key "rm")
    ((name "New module name"))
  (refac-transform/wrangler 'rename_module :file buffer-file-name :name name))

(define-refac-operation (refactorerl-wrangler-search-dups :menu "Search in directory" :menu-group dups :precondition :wrangler)
    ((dir (:dir "Directory"))
     (num-tokens (:number "Minimal number of tokens to match"))
     (appearance-count (:number "Minimal number of duplicates to detect")))
  (async-with-refac-buffer-list
   (refac-send-command 'search_dup
                       (list (vector :dirs (when dir (list (expand-file-name dir))))
                             (vector :token num-tokens)
                             (vector :clone appearance-count)))))

(define-refac-operation (refactorerl-wrangler-search-dups/all :menu "Search all files" :menu-group dups :precondition :wrangler)  
    ((num-tokens (:number "Minimal number of tokens to match"))
     (appearance-count (:number "Minimal number of duplicates to detect")))  
  (refactorerl-wrangler-search-dups nil num-tokens appearance-count))

(define-refac-operation (refactorerl-wrangler-search-dups/current :menu "Search current buffer" :menu-group dups :precondition :wrangler)
    ((num-tokens (:number "Minimal number of tokens to match"))
     (appearance-count (:number "Minimal number of duplicates to detect")))  
  (refactorerl-wrangler-search-dups (buffer-file-name) num-tokens appearance-count))


(define-refac-operation/wrangler (refactorerl-wrangler-extract-fun :menu "Extract function")  
  ((name "Name of new function") ((beg end) :region))    
  (refac-transform/wrangler 'extract_function :file buffer-file-name :name name
                            :posrange (vector beg (1- end))))

(define-refac-operation/wrangler (refactorerl-wrangler-tuple-funargs :menu "Function arguments to tuples")
  ((argument-count (:number "Number of arguments to move to tuple"))
   (pos :point))
  (refac-transform/wrangler 'tuple_funargs :file buffer-file-name :position pos :number argument-count))

(define-refac-operation/wrangler (refactorerl-wrangler-introduce-macro :menu "Introduce macro")
  ((name "Name of new macro") ((beg end) :region))
  (refac-transform/wrangler 'introduce_macro :file buffer-file-name :name name :posrange (vector beg (1- end))))

(define-refac-operation/wrangler (refactorerl-wrangler-move-function :menu "Move function")
  ((target (:newfile "Move to")) (pos :point))
  (refac-transform/wrangler 'move_function :file buffer-file-name :position pos :name target :create_new_file :true))

