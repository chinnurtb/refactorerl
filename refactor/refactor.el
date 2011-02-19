;; Copyright Notice © 2007 Eötvös Loránd University and Ericsson Hungary
;; Software development supported by Ericsson Hungary and
;; GVOP-3.2.2-2004-07-0005/3.0 ELTE IKKK.

;; Materials  were  created, authored,  and/or  prepared  by  the Authors  at
;; Department   of  Programming  Languages   and  Compilers,   Eötvös  Loránd
;; University,  Budapest,  Hungary  (ELTE)  for Ericsson  Hungary  under  the
;; agreement  between  Ericsson  Hungary  and  ELTE  IKKK.  Unless  otherwise
;; specifically stated, no claim to copyright is being asserted and it may be
;; freely  used as  in the  public domain  in accordance  with  Erlang Public
;; License.  All rights,  including copyright,  are owned  or  controlled for
;; these purposes by  the Ericsson Hungary and ELTE.  Copyright exists in all
;; other original  material published on the  internet and may  belong to the
;; authors depending on the circumstances of publication.

;; --------------------------------------------------------------------------
;; ``The  contents of this  file are  subject to  the Erlang  Public License,
;; Version  1.1,  (the  "License"); you  may  not  use  this file  except  in
;; compliance with the License. You should have received a copy of the Erlang
;; Public License along  with this software. If not, it  can be retrieved via
;; the world wide web at http://www.erlang.org/.

;; Software distributed under the License is distributed on an "AS IS" basis,
;; WITHOUT WARRANTY OF  ANY KIND, either express or  implied. See the License
;; for  the specific  language  governing rights  and  limitations under  the
;; License.

;; The Initial  Developer of  the Original Code  is Ericsson  Utvecklings AB.
;; Portions created by Ericsson  are Copyright 1999, Ericsson Utvecklings AB.
;; All Rights Reserved.''
;; --------------------------------------------------------------------------

;; The Contributors are the Authors listed below. All Rights Reserved.

;; You may not alter or remove any trademark, copyright or other notice from
;; copies of the content.

;; Authors: Zoltán Csörnyei
;;          Zoltán Horváth
;;          Roland Király
;;          Róbert Kitlei
;;          Tamás Kozsik
;;          László Lövei
;;          Tamás Nagy
;;          Melinda Tóth
;;          Anikó Víg

;; Author contact: erlang@plc.inf.elte.hu
;; --------------------------------------------------------------------------

;;(buffer-modified-p)

;;(save-buffer)

;;(cadddr (cddr (file-attributes (buffer-file-name))))

;;;(make-variable-buffer-local 'timestamp)

;;(if true-or-false-test
;;    action-to-carry-out-if-the-test-returns-true
;;  action-to-carry-out-if-the-test-returns-false)

(defun set-timestamp ()
  (setq timestamp (convert-timestamp (get-file-timestamp))))

(defun get-timestamp ()
  timestamp)


(defun get-file-timestamp ()
  (cadddr (cddr (file-attributes (buffer-file-name)))))

(defun convert-timestamp (time)
  (+ (* (car time) (expt 2 16)) (cadr time)))

(defun check-modification-p ()
  (if (buffer-modified-p)
      (message "Save the buffer and reload the file before making any refactoring!")
    (if (/= (get-timestamp)  (convert-timestamp (get-file-timestamp)))
        (message "Reload the file into the refactoring system before applying any refactoring!")
      nil)))

(require 'distel)

(provide 'refactor)

(defconst refactor-version "0.1")

;; Starter

(defun refactor-init ()
  (make-variable-buffer-local 'timestamp)
  (setq erlang-menu-items
	(erlang-menu-add-below 'distel-refactor-items
			       'distel-menu-items
			       erlang-menu-items))
  (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)
  (refactor-bind-keys))

(defvar distel-refactor-items
  '(nil
    ("Refactor"
    ( 
      ("About" erl-refactor-about)
      nil
      ("Rename" erl-refactor-rename)
      ("Rename function" erl-refactor-function)
      ("Rename variable" erl-refactor-variable)
      ("Reorder function parameters" erl-refactor-reorder)
      ("Tuple function parameters" erl-refactor-tuple)
      ("Eliminate variable" erl-refactor-varelim)
      ("Merge subexpression duplicates" erl-refactor-merge)
      ("Extract function" erl-refactor-exfun)
      ("Tuple to record(BETA)" erl-refactor-record)
      nil
      ("Into_db and reload from there" erl-refactor-intodb)
      ("Initialise database" erl-refactor-dbinit)
      ("Check out latest version in the database" erl-refactor-check-out)
      ))))


(defconst refactor-keys
  '(
      ("\C-c\C-er" erl-refactor-rename)
      ("\C-c\C-eu" erl-refactor-function)
      ("\C-c\C-ev" erl-refactor-variable)
      ("\C-c\C-ei" erl-refactor-dbinit)
      ("\C-c\C-en" erl-refactor-intodb)
      ("\C-c\C-eo" erl-refactor-reorder)
      ("\C-c\C-et" erl-refactor-tuple)
      ("\C-c\C-ee" erl-refactor-varelim)
      ("\C-c\C-em" erl-refactor-merge)
      ("\C-c\C-ef" erl-refactor-exfun)
      ("\C-c\C-ed" erl-refactor-record)
   )
  "Keys to bind for refactoring.")

(defun erl-refactor-about ()
"Version, Authors, Supporters"
    (interactive)
   (switch-to-buffer-other-window (generate-new-buffer "About_refactorer"))
   (insert "Version 1.1\n\n")
   (insert "Authors: Zoltan Csornyei, Zoltan Horvath, Roland Kiraly, Robert Kitlei,\n Tamas Kozsik, Laszlo Lovei, Tamas Nagy, Melinda Toth, Aniko Vig\n\n")
   (insert "Institute: ELTE Faculty of Informatics, Budapest\n\n")
   (insert "This program is free software; you can redistribute it and/or modify it under the terms of the Erlang Public License; either version 1.1 of the License, or any later version.\nhttp://www.erlang.org/EPLICENSE\n\n")
   (insert "Software distributed under the License is distributed on an AS IS basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the specific language governing rights and limitations under the License.\n")
   (setq buffer-read-only t))

(defun erl-refactor-dbinit (node)
"Initialise the database"
  (interactive (list (erl-target-node)))
  (message "*Initialising*")
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'db_init
		  (list ))
    (erl-receive ()
	((['rex ['ok 'ok]]
	  (message "*Initialised*"))
	 (['rex ['error reason]]
	  (message "Error: %S" reason))
	 (other
	  (message "Unexpected: %S" other))))))

(defun erl-refactor-intodb (node name bf)
"Put the erlang code to the database, and reload it from there"
  (message "*Reloading*")
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (current-buffer)))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'db_tofrom
		  (list name))
    (erl-receive ()
	((['rex ['ok file]]
	  (save-excursion
	   (set-buffer (get-file-buffer file))
	   (revert-buffer t t) (set-timestamp))
	  (message "*Reloaded*"))
	 (['rex ['error reason]]
	  (message "Error: %S" reason))
	 (other
	  (message "Unexpected: %S" other))))))

(defun erl-refactor-check-out (node name bf)
"Reload file from database"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (current-buffer)))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'check_out
		  (list name))
    (erl-receive ()
	((['rex ['ok file]]
	  (save-excursion
	   (set-buffer (get-file-buffer file))
	   (revert-buffer t t) (set-timestamp))
	  (message "*Reloaded*"))
	 (['rex ['not_exists file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "This file( %S ) does not exist in the database. You have to put it in, to be able to check it out" file))	 
	 (['rex ['error reason]]
	  (message "Error: %S" reason))
	 (other
	  (message "Unexpected: %S" other))))))

(defun erl-refactor-function (node name bf line col newname)
"Rename function"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (current-buffer)
		     (current-line)
		     (current-column)
		     (read-string "New function name:")))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'rename_function
		  (list name line col newname))
    (erl-receive ()
	((['rex ['ok newname file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Rename successful to %S in file %S" newname file))
	 (['rex ['not_exists file file2]]
	  (message "Error: %S file doesn't exists in database" file))
	 (['rex ['pos_error [line col] file]]
	  (message "Error: The nearest element(line %S, col %S) is not a function. File:%S" line col file))
	 (['rex ['not_function_name_error newname file]]
	  (message "Error: The new name( %S ) is not a valid function name. File:%S" newname file))
	 (['rex ['autoimported_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with autoimported names. File:%S" newname file))
	 (['rex ['reserved_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with reserved names. File:%S" newname file))
	 (['rex ['user_denied_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with user denied names. File:%S" newname file))
	 (['rex ['exists_error [newname arity] file]]
	  (message "Error: The new function name ( %S ) clashes with an exsisting function(%S\%S). File:%S" newname newname arity file))
	 (['rex ['import_error [newname arity] file]]
	  (message "Error: The new function name ( %S ) clashes with an imported function(%S\%S). File:%S" newname newname arity file))
         (['rex ['out_of_scope 0 file]]
	  (message "Error: The renamed function's definition is not in the database. File:%S" file))
	 (['rex ['warning 'dynamic file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This function rename may cause runtime problems, because the module uses dynamic function calls. File:%S" file))
	 (['rex ['warning 'apply file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This function rename may cause runtime problems, because the module uses the apply BIF. File:%S" file))
	 (['rex ['warning 'spawn file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This function rename may cause runtime problems, because the module uses the spawn BIF. File:%S" file))
	 (['rex ['warning 'hibernate file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This function rename may cause runtime problems, because the module uses the erlang:hibernate function. File:%S" file))
	 (other
	  (message "Unexpected: %S" other)))))))
;;1 {pos_error,{Line,CCol}
;;2 {not_function_name_error, Newname}
;;3 {autoimported_error, Newname
;;4 {reserved_error, Newname
;;5 {user_denied_error, Newname
;;6 {exists_error,{Newname,Arity}}
;;7 {import_error,{Newname,Arity}}
;;8 {warning, dynamic}
;;9 {warning, apply}
;;10 {warning, spawn}
;;11 {warning, hibernate}

(defun erl-refactor-variable (node name bf line col newname)
"Rename variable"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (current-buffer)
		     (current-line)
		     (current-column)
		     (read-string "New variable name:")))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'rename_variable
		  (list name line col newname))
    (erl-receive ()
	((['rex ['ok newname file]]
	  (save-excursion
	   (set-buffer (get-file-buffer file))
	   (revert-buffer t t) (set-timestamp))
	  (message "Rename successful to %S in file %S" newname file))
	 (['rex ['not_exists file file2]]
	  (message "Error: File %S doesn't exist in the database" file))
	 (['rex ['pos_error [line col] file]]
	  (message "Error: The nearest element(line %S, col %S) is not a variable in file %S" line col file))
	 (['rex ['not_variable_name_error newname file]]
	  (message "Error: Invalid new variable name %S." newname))
	 (['rex ['name_capture [newname [line col]] file]]
	  (message "Error: New name (%S) would capture variable at line:%S, col:%S, file:%S" newname line col file))
	 (['rex ['name_clash [newname [line col]] file]]
	  (message "Error: New name (%S) would clash variable at line:%S, col:%S, file:%S" newname line col file))
	 (other
	  (message "Unexpected: %S" other)))))))

;; {not_exists,File} ;
;; {pos_error,{Line,CCol}} ;
;; {not_variable_name, NewName} ;
;; {name_capture, {NewName, get_pos(Ref,MId,Id)}}
;; {name_clash, {NewName, get_pos(Ref,MId,Id)}}
;; {ok,NewName} ;

(defun erl-refactor-reorder (node name bf line col order)
"Reorder function parameters"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (current-buffer)
		     (current-line)
		     (current-column)
		     (read-string "New order(example: 2,3,1):")))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'reorder_funpar
		  (list name line col order))
    (erl-receive ()
	((['rex ['ok 'done file]]
	  (save-excursion
	   (set-buffer (get-file-buffer file))
	   (revert-buffer t t) (set-timestamp))
	  (message "Reorder successful in %S file" file))
	 (['rex ['not_exists file file2]]
	  (message "Error: File %S doesn't exist in the database" file))
	 (['rex ['pos_error [line col] file]]
	  (message "Error: The nearest element(line %S, col %S) is not a function, application or implicit function. File:%S" line col file))
	 (['rex ['no_parameters [0] file]]
	  (message "Error: The pointed function hasn't got any parameters."))
	 (['rex ['no_change [0] file]]
	  (message "Error: The given order does not change anything."))
	 (['rex ['bad_list [arity] file]]
	  (message "Error: The given new order list is invalid. It should contain all the elements from 1 to %S." arity))
	 (['rex ['length_error [length arity] file]]
	  (message "Error: The given new order list is not enough long.")
	  (message "It should contain all the elements from 1 to %S, and it only contains %S elements." arity length))
	 (['rex ['element_error [length arity] file]]
	  (message "Error: The given new order list is long enough, but it doesn't contain all the elements.")
	  (message "It should contain all the elements from 1 to %S." arity))
         (['rex ['out_of_scope 0 file]]
	  (message "Error: The reordered function's definition is not in the database. File:%S" file))
	 (['rex ['warning 'dynamic file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This parameter reordering may cause runtime problems, because the module uses dynamic function calls. File:%S" file))
	 (['rex ['warning 'apply file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This parameter reordering may cause runtime problems, because the module uses the apply BIF. File:%S" file))
	 (['rex ['warning 'spawn file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This parameter reordering may cause runtime problems, because the module uses the spawn BIF. File:%S" file))
	 (['rex ['warning 'hibernate file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This parameter reordering may cause runtime problems, because the module uses the erlang:hibernate function. File:%S" file))
	 (other
	  (message "Unexpected: %S" other)))))))

;;{not_exists,File}
;;{pos_error,{Line,CCol}}
;;{no_parameters, {0}}
;;{no_change,{0}}
;;{bad_list, {Arity}}
;;{length_error, {Length, Arity}}
;;{element_error, {Length, Arity}}
;;{ok,{done}}
(defun erl-refactor-tuple (node name bf line col number)
"Tuple function parameters"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (current-buffer)
		     (current-line)
		     (current-column)
		     (read-string "How many elements would you like to tuple:")))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'tuple_funpar
		  (list name line col number))
    (erl-receive ()
	((['rex ['ok 'done file]]
	  (save-excursion
	   (set-buffer (get-file-buffer file))
	   (revert-buffer t t) (set-timestamp))
	  (message "Tuple successful in %S file" file))
	 (['rex ['not_exists file file2]]
	  (message "Error: File %S doesn't exist in the database" file))
	 (['rex ['pos_error [line col] file]]
	  (message "Error: There's no clause or application near the element(line %S, col %S). File:%S" line col file))
	 (['rex ['not_parameter_error [line col] file]]
	  (message "Error: The selected element(line %S, col %S) is not a parameter. File:%S" line col file))
	 (['rex ['number_error num file]]
	  (message "Error: The entered number is too small, it has to be bigger than 0."))
	 (['rex ['too_big_number_error num file]]
	  (message "Error: The entered number is too big. The function doesn't have enough parameters from the pointed parameter to create the tuple."))
	 (['rex ['not_function_clause_error 0 file]]
	  (message "Error: The selected parameter is in a function expression clause.")
	  (message "This is currently not supported."))
	 (['rex ['inner_name_clash [name arity] file]]
	  (message "Error: After the creating of the tuple the modified function would clash with %S/%S." name arity))
	 (['rex ['clash_existing_function_error [module name arity] file]]
	  (message "Error: After the creating of the tuple the modified function would clash with %S:%S/%S." module name arity))
	 (['rex ['clash_imported_function_error [module [module2 name arity]] file]]
	  (message "Error: After the creating of the tuple the modified function would clash with imported function %S:%S/%S in %S module." module2 name arity module))
	 (['rex ['warning 'dynamic file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This tupleing may cause runtime problems, because the module uses dynamic function calls. File:%S" file))
	 (['rex ['warning 'apply file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This tupleing may cause runtime problems, because the module uses the apply BIF. File:%S" file))
	 (['rex ['warning 'spawn file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This tupleing may cause runtime problems, because the module uses the spawn BIF. File:%S" file))
	 (['rex ['warning 'hibernate file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This tupleing may cause runtime problems, because the module uses the erlang:hibernate function. File:%S" file))
	 (other
	  (message "Unexpected: %S" other)))))))

(defun erl-refactor-varelim (node name bf line col)
"Eliminate variable"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (current-buffer)
		     (current-line)
		     (current-column)))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'var_elim
		  (list name line col))
    (erl-receive ()
	((['rex ['ok newname file]]
	  (save-excursion
	   (set-buffer (get-file-buffer file))
	   (revert-buffer t t) (set-timestamp))
	  (message "Variable %S successfully eliminated in file %S" newname file))
	 (['rex ['not_exists file file2]]
	  (message "Error: File %S doesn't exist in the database" file))
	 (['rex ['pos_error [line col] file]]
	  (message "Error: There's no variable near the pointed location(line %S, col %S). File:%S" line col file))
	 (['rex ['bad_binding [line col] file]]
	  (message "Error: The selection can't be eliminated, because of not eliminable binding(line %S, col %S). File:%S" line col file))
	 (['rex ['ambiguous_defining_error num file]]
	  (message "Error: The binding of the variable is not unambiguous. File:%S" file))
	 (['rex ['too_big_number_error num file]]
	  (message "Error: The entered number is too big. The function doesn't have enough parameters from the pointed parameter to create the tuple."))
	 (['rex ['sideffect_error 0 file]]
	  (message "Error: The resulting expression bound to the variable has sideeffect."))
	 (['rex ['body_variable_shadowed_error [name line col] file]]
	  (message "Error: The resulting expression bound to the variable contains a variable(%S)" name)
	  (message "that is shadowed(line %S, col %S) near an appearance of the eliminated variable." line col))
	 (['rex ['non_binding_pattern_occurrence nil file]]
	  (message "Error: The variable occurs on the left hand side of a match expression. File: %S" file))
	 (['rex ['warning 'dynamic file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This eliminating may cause runtime problems, because the module uses dynamic function calls. File:%S" file))
	 (['rex ['warning 'apply file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This eliminating may cause runtime problems, because the module uses the apply BIF. File:%S" file))
	 (['rex ['warning 'spawn file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This eliminating may cause runtime problems, because the module uses the spawn BIF. File:%S" file))
	 (['rex ['warning 'hibernate file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This eliminating may cause runtime problems, because the module uses the erlang:hibernate function. File:%S" file))
	 (other
	  (message "Unexpected: %S" other)))))))

(defun current-line ()
  "Return the current line in buffer."
  (count-lines (point-min) (+ (point) 1)))

(defun refactor-bind-keys ()
  "Bind `distel-keys' in `erlang-extended-mode-map'."
  (interactive)
  (dolist (spec refactor-keys)
    (define-key erlang-extended-mode-map (car spec) (cadr spec))))


(defun erl-refactor-rename (node name line col newname)
"Rename function or variable"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (current-line)
		     (current-column)
		     (read-string "New name:")))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'rename
		  (list name line col newname))
    (erl-receive ()
	((['rex ['ok newname file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Rename successful to %S in file %S" newname file))
	 (['rex ['not_exists file file2]]
	  (message "Error: %S file doesn't exists in database" file))
	 (['rex ['pos_error [line col] file]]
	  (message "Error: The nearest element(line %S, col %S) is not a variable/function. File:%S" line col file))
	 (['rex ['not_function_name_error newname file]]
	  (message "Error: The new name( %S ) is not a valid function name. File:%S" newname file))
	 (['rex ['autoimported_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with autoimported names. File:%S" newname file))
	 (['rex ['reserved_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with reserved names. File:%S" newname file))
	 (['rex ['user_denied_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with user denied names. File:%S" newname file))
	 (['rex ['exists_error [newname arity] file]]
	  (message "Error: The new function name ( %S ) clashes with an exsisting function(%S\%S). File:%S" newname newname arity file))
	 (['rex ['import_error [newname arity] file]]
	  (message "Error: The new function name ( %S ) clashes with an imported function(%S\%S). File:%S" newname newname arity file))
         (['rex ['out_of_scope 0 file]]
	  (message "Error: The renamed function's definition is not in the database. File:%S" file))
	  (['rex ['not_variable_name_error newname file]]
	  (message "Error: Invalid new variable name %S." newname))
	 (['rex ['name_capture [newname [line col]] file]]
	  (message "Error: New name (%S) would capture variable at line:%S, col:%S, file:%S" newname line col file))
	 (['rex ['name_clash [newname [line col]] file]]
	  (message "Error: New name (%S) would clash variable at line:%S, col:%S, file:%S" newname line col file)) 
	 (['rex ['warning 'dynamic file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This function rename may cause runtime problems, because the module uses dynamic function calls. File:%S" file))
	 (['rex ['warning 'apply file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This function rename may cause runtime problems, because the module uses the apply BIF. File:%S" file))
	 (['rex ['warning 'spawn file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This function rename may cause runtime problems, because the module uses the spawn BIF. File:%S" file))
	 (['rex ['warning 'hibernate file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Warning: This function rename may cause runtime problems, because the module uses the erlang:hibernate function. File:%S" file))
	 (other
	  (message "Unexpected: %S" other)))))))




(defun find-line (mark)
  ""
  (save-excursion (goto-char mark)
                  (current-line)))

(defun find-col (mark)
  ""
  (save-excursion (goto-char mark)
                  (current-column)))

(defun erl-refactor-merge (node filename begin end newname)
"Merge subexpressions"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (region-beginning)
		     (region-end)
                     (read-string "New binding variable name: ")))
  (set 'line1 (find-line begin))
  (set  'col1 (find-col  begin))
  (set 'line2 (find-line end  ))
  (set  'col2 (find-col  end  ))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'merge_subexpression
		  (list filename line1 col1 line2 col2 newname))
    (erl-receive ()
	((['rex ['ok newname file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Merge to %S successful in file %S" newname file))
	 (['rex ['not_exists file file2]]
	  (message "Error: file %S doesn't exist in database" file))
	 (['rex ['not_found nil file]]
	  (message "Error: The selection is not a valid expression."))
	 (['rex ['found_first nil file]]
	  (message "Error: The selection is not a valid expression."))
	 (['rex ['found_last nil file]]
	  (message "Error: The selection is not a valid expression."))
	 (['rex ['found_body nil file]]
	  (message "Error: Body of multiple expressions is selected."))
	 (['rex ['ambiguous_binding newname file]]
	  (message "Error: Name already bound: %S." newname))
	 (['rex ['in_list_head parent file]]
	  (message "Error: The selection is part of a list comprehension. File:%S" file))
	 (['rex ['in_list_body parent file]]
	  (message "Error: The selection is part of a list body. File:%S" file))
	 (['rex ['in_generator_pattern parent file]]
	  (message "Error: The selection is part of a generator pattern. File:%S" file))
	 (['rex ['in_clause_guard parent file]]
	  (message "Error: The selection is part of a guard sequence. File:%S" file))
	 (['rex ['in_clause_pattern parent file]]
	  (message "Error: The selection is part of a clause pattern. File:%S" file))
	 (['rex ['in_macro parent file]]
	  (message "Error: The selection is part of a macro. File:%S" file))
	 (['rex ['in_send parent file]]
	  (message "Error: The selection contains message passing. File:%S" file))
	 (['rex ['sideffect_error 0 file]]
	  (message "Error: The selected expression has sideeffect.File:%S" file)) 
	 (other
	  (message "Unexpected: %S" other)))))))

(defun erl-refactor-exfun (node filename begin end newname)
"Extract function"
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (region-beginning)
		     (region-end)
                     (read-string "New function name: ")))
  (set 'line1 (find-line begin))
  (set  'col1 (find-col  begin))
  (set 'line2 (find-line end  ))
  (set  'col2 (find-col  end  ))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'extract_function
		  (list filename line1 col1 line2 col2 newname))
    (erl-receive ()
	((['rex ['ok newname file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Extract to %S successful in file %S" newname file))
	 (['rex ['not_exists file file2]]
	  (message "Error: %S file doesn't exist in database" file))
	 (['rex ['not_function_name_error newname file]]
	  (message "Error: The new name( %S ) is not a valid function name. File:%S" newname file))
	 (['rex ['autoimported_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with autoimported names. File:%S" newname file))
	 (['rex ['reserved_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with reserved names. File:%S" newname file))
	 (['rex ['user_denied_error newname file]]
	  (message "Error: The new function name ( %S ) clashes with user denied names. File:%S" newname file))
	 (['rex ['exists_error [newname arity] file]]
	  (message "Error: The new function name ( %S ) clashes with an existing function(%S/%S). File:%S" newname newname arity file))
	 (['rex ['import_error [newname arity] file]]
	  (message "Error: The new function name ( %S ) clashes with an imported function(%S/%S). File:%S" newname newname arity file))
	 (['rex ['invalid_body [[from to] [from2 to2]] file]]
	  (message "Error: The starting and ending positions don't delimit a sequence of expressions. File:%S" file))
	 (['rex ['not_all_var_bound_ok outsideused file]]
	  (message "Error: Variables with possible binding occurrences in the selected sequence of expressions appear outside of the selected part. File:%S" file))
	 (['rex ['in_list_head parent file]]
	  (message "Error: The selection is part of a list comprehension. File:%S" file))
	 (['rex ['in_list_body parent file]]
	  (message "Error: The selection is part of a list body. File:%S" file))
	 (['rex ['in_generator_pattern parent file]]
	  (message "Error: The selection is part of a generator pattern. File:%S" file))
	 (['rex ['in_clause_guard parent file]]
	  (message "Error: The selection is part of a guard sequence. File:%S" file))
	 (['rex ['in_clause_pattern parent file]]
	  (message "Error: The selection is part of a clause pattern. File:%S" file))
	 (['rex ['in_macro parent file]]
	  (message "Error: The selection is part of a macro. File:%S" file))
	 (other
	  (message "Unexpected: %S" other)))))))

(defun erl-refactor-record (node filename begin end newname params)
  (interactive (list (erl-target-node)
		     (buffer-file-name (current-buffer))
		     (region-beginning)
		     (region-end)
                     (read-string "New record name:")
                     (read-string "Record fields:")))
  (set 'line1 (find-line begin))
  (set  'col1 (find-col  begin))
  (set 'line2 (find-line end  ))
  (set  'col2 (find-col  end  ))
  (if (not (check-modification-p))
  (erl-spawn
    (erl-send-rpc node
		  'd_client
		  'tuple_to_record
		  (list filename line1 col1 line2 col2 newname params))
    (erl-receive ()
	((['rex ['ok 'done file]]
	  (save-excursion
	    (set-buffer (get-file-buffer file))
	    (revert-buffer t t) (set-timestamp))
	  (message "Tuple to record refactoring successful. File:%S" file))
	 (['rex ['not_exists file file2]]
	  (message "Error: %S file doesn't exist in database" file))
	 (['rex ['invalid_paramname error file]]
	  (message "Error: Given record fields are bad. File:%S" file))
	 (['rex ['invalid_expr [[fromline fromcol] [toline tocol]] file]]
	  (message "Error: Selected area is not a tuple. from(%S, %S) to(%S, %S)File:%S" fromline fromcol toline tocol file))
	 (['rex ['length_mismatch [given tuples] file]]
	  (message "Error: The given record fields number(%S) and the number of tuple's elements(%S) are not equal. File:%S" given tuples file))
	 (['rex ['not_supported type file]]
	  (message "Error: Embedded tuples are not yet supported. File:%S" file))
	 (other
	  (message "Unexpected: %S" other)))))))
