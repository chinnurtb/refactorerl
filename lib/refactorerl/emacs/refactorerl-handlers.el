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
(require 'cl)
(eval-when-compile
  (require 'wid-edit))
(require 'refactorerl-ui)

(provide 'refactorerl-handlers)

(defvar refac-handlers (make-hash-table)
  "Dictionary of RefactorErl callback handlers")

(defvar refac-progress-reporters (make-hash-table :test 'equal)
  "Dictionary of progress reporters for file analysing. Keys are
complete file names, values are progress reporters as returned by
`refac-progress-start'.")

(defmacro* define-handler (message (&rest args) &body body)
  `(setf (gethash ',message refac-handlers)
         (lambda (,@args)
           ,@body)))

(define-handler error (string)
  (message "RefactorErl internal error: %s" string))

(define-handler status (string)
  (message "%s" string))

(define-handler uifinished (string)
  ())

(define-handler trfinished (string)
  ())

(defun plist-from-eproplist (tree)
  (typecase tree
    (cons (mapcan #'plist-from-eproplist tree))
    (vector (let ((keyword (intern (format ":%s" (elt tree 0))))
                  (value (elt tree 1)))
              (list keyword value)))))

(defun refac-cb-visit (widget &rest args)
  (refac-visit (widget-get widget :filepath)
               (widget-get widget :start-pos) (widget-get widget :start-line) (widget-get widget :start-col)
               (widget-get widget :end-pos) (widget-get widget :end-line) (widget-get widget :end-col)))

(defun point-from-pos (line col)
  (save-excursion
    (goto-line line)
    (forward-char (or col 0))
    (point)))

(defun* refac-visit (filepath &optional start-pos start-line start-col end-pos end-line end-col)
  (let ((buf (find-file-noselect filepath)))
    (with-current-buffer buf
      (setf refactorerl-mode t))
    (pop-to-buffer buf))
  (when start-line
    (setf start-pos (point-from-pos start-line (1- start-col))))
  (when start-pos
    (goto-char start-pos)
    (when end-line
      (setf end-pos (point-from-pos end-line end-col)))
    (when end-pos
      (refac-add-overlay start-pos end-pos))))

(defun* refac-widget-link (filepath &key start-pos start-line start-col end-pos end-line end-col label (style 'refactorerl-link))
  (assert (if start-pos (not (or start-line start-col)) t))
  (assert (if end-pos (not (or end-line end-col)) t))
  (assert (if start-line start-col t))
  (assert (if start-col start-line t))
  (assert (if end-line end-col t))
  (assert (if end-col end-line t))
  
  (widget-create 'link
                 :button-prefix ""
                 :button-suffix ""
                 :button-face style
                 :filepath filepath
                 :start-pos start-pos :start-line start-line :start-col start-col                 
                 :end-pos end-pos :end-line end-line :end-col end-col
                 :help-echo "mouse-2, RET: open file"
                 :notify #'refac-cb-visit
                 (or label (file-name-nondirectory filepath))))

(define-handler errorlist (parse-errors)
  (if (not (consp parse-errors))
      (message "No errors found")  
    (save-excursion
      (with-refac-buffer-list 
       (dolist (parse-error (car parse-errors))
         (destructuring-bind (&key filepath nexttokentext position) (plist-from-eproplist parse-error)
           (let ((start-pos (elt position 0))
                 (end-pos (1+ (elt position 1))))             
             (widget-insert "Parse error at ")
             (refac-widget-link filepath
                                :start-pos start-pos :end-pos end-pos
                                :style 'refactorerl-error
                                :label (format "%s:%d-%d" (file-name-nondirectory filepath) start-pos end-pos))
             (widget-insert " near '" nexttokentext "'")
             (widget-insert "\n"))))))))

(define-handler filestatus (status-lines)
  (when (and (consp status-lines) (consp (car status-lines)))
    (save-excursion
      (with-refac-buffer-list
       (dolist (status-line (car status-lines))
         (destructuring-bind (&key file error type lastmod status) (plist-from-eproplist status-line)
           (refac-widget-link file)
           (widget-insert ":")
           (widget-insert (format "%s" type) "\t"
                          (if (eql error 'yes) (propertize "Err" 'face 'refactorerl-error) "OK ") "\t"
                          (if (eql lastmod 'undefined) "" (format "%s\t" lastmod))
                          (format "%s" status))
           (widget-insert "\n")))))))

;; (define-handler reload (file)
;;   (let ((buf (get-file-buffer file)))
;;     (when buf
;;       (with-current-buffer buf
;;         (revert-buffer t t t)))))

(define-handler reload (file-list)
  (save-excursion
    (mapc (lambda (file)
            (let ((buf (get-file-buffer file)))
              (when buf
                (set-buffer buf)
                (revert-buffer t t t))))
          file-list)))

(define-handler add (file-list)
  (save-excursion
    (dolist (file file-list)
      (refac-progress-report 'add file)
      (let ((buf (get-file-buffer file)))
        (when buf
          (set-buffer buf)
          (refac-set-buffer-state :ok))))))

(define-handler drop (file-list)
  (save-excursion
    (mapc (lambda (file)
	    (let ((buf (get-file-buffer file)))
	      (when buf
		(set-buffer buf)
                (refac-set-buffer-state :off))))          
	  file-list)))

(define-handler rename (files)
  (let* ((frompath  (elt files 0))
         (topath    (elt files 1))
         (buf       (get-file-buffer frompath)))
    (when buf
      (with-current-buffer buf
        (set-visited-file-name topath)))))

(define-handler invalid (file)
  (refac-progress-report :err file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (refac-set-buffer-state :err)))))

(defun refac-file-list-entry (filepath)
  (refac-widget-link filepath)
  (widget-insert " ")
  (widget-create 'push-button
                 :filepath filepath
                 :help-echo "drop file from database"
                 :notify (lambda (btn &rest args)
                           (refac-send-command 'drop (widget-get btn :filepath))
                           (refactorerl-list-files t))
                 "drop"))

(defun refac-file-list-add (file)
  (let ((state (if (string-equal (elt file 1) "error") :err :ok))
	(filepath (elt file 0)))
    (let ((dirname (file-name-directory filepath)))
      (unless (equal dirname last-file-dir)
        (insert (concat (propertize dirname 'face 'refactorerl-header) ":\n"))
        (setq last-file-dir dirname)))
    (insert (if (eq state :ok) "   " (propertize " ! " 'face 'refactorerl-error)))
    (refac-file-list-entry filepath)
    (insert "\n")))

(define-handler filelist (file-list)
  (with-refac-buffer-list
   (mapcar 'refac-file-list-add file-list)))

(defun list-from-vector (vector)
  (if (vectorp vector)
      (map 'list #'list-from-vector vector)
    vector))

;;; TODO: unify this one with filelist and errorlist handlers
;;; TODO: Maybe use a tree widget?
(define-handler filepos (dups)
  (save-excursion
    (with-refac-buffer-list
     (dolist (dup-group dups)
       (widget-insert "Group\n")
       (dolist (dup dup-group)
         (destructuring-bind (filepath (start-line start-col) (end-line end-col)) (list-from-vector dup)             
           (widget-insert "   ")
           (refac-widget-link filepath :start-line start-line :start-col start-col :end-line end-line :end-col end-col
                              :label (format "%s: (%d,%d)-(%d,%d)" (file-name-nondirectory filepath) start-line start-col end-line end-col)))
         (widget-insert "\n"))))))

(defvar refac-progress nil)
(defun refac-progress-report (type file)
  (when refac-progress
    (destructuring-bind (files err dir) refac-progress
      (when (member file files)
        (setf files (delete file files))
        (when (equal type :err)
          (incf err))
        (if files
            (message "Loading %s: %d remaining (%d errors)"
                     dir (length files) err)
          (message "Loading %s done (%d errors)" dir err)))
      (setf refac-progress (when files (list files err dir))))))

(define-handler funlist (function-list)
  (switch-to-buffer-other-window refac-move-fun-buffer)
  (set (make-local-variable 'function-checklist)
       (apply #'widget-create 'checklist
              (mapcar (lambda (fn)
                        (list 'const
                              :tag (format "%s/%d" (elt fn 0) (elt fn 1))
                              :format "%t\n"
                              fn))
                      function-list)))
  (widget-create 'push-button
                 :notify 'refac-move-fun-apply
                 "Move")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify 'refac-move-fun-cleanup
                 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(define-handler recordlist (record-list)
  (switch-to-buffer-other-window refac-move-rec-buffer)
  (set (make-local-variable 'record-checklist)
       (apply #'widget-create 'checklist
              (mapcar (lambda (rec)
                        (list 'const
                              :tag (format "%s" rec)
                              :format "%t\n"
                              rec))
                      record-list)))
  (widget-create 'push-button
                 :notify 'refac-move-rec-apply
                 "Move")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify 'refac-move-rec-cleanup
                 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(define-handler macrolist (macro-list)
  (switch-to-buffer-other-window refac-move-mac-buffer)
  (set (make-local-variable 'macro-checklist)
       (apply #'widget-create 'checklist
              (mapcar (lambda (mac)
                        (list 'const
                              :tag (format "%s" mac)
                              :format "%t\n"
                              mac))
                      macro-list)))
  (widget-create 'push-button
                 :notify 'refac-move-mac-apply
                 "Move")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify 'refac-move-mac-cleanup
                 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(define-handler options (data)
  (setq cluster-ui-buffer (generate-new-buffer "*Cluster Ui*"))
  (switch-to-buffer cluster-ui-buffer)
  (widget-insert
   (propertize "Clustering Buffer\n\n"
               'face 'bold)
   (propertize "Options of the clustering\n\n"
               'face 'italic))
  (set (make-local-variable 'lista) (cdr data))
  (set (make-local-variable 'alg) (car data))
  (set (make-local-variable 'cl-options-list)
       (mapcar (lambda (fx)
                 (widget-create 'editable-field
                                :size 20
                                :format (concat (format "%s" (car fx))
                                                (make-string
                                                 (- 30 (length
                                                        (format
                                                         "%s"(car fx)))) ? )
                                                ": %v\n")
                                (format "%s" (car (cdr fx)))))
               lista))

  (widget-insert "\nSave result into a database : ")
  (set (make-local-variable 'create)
       (widget-create 'checkbox t))
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify 'res-apply-aggl
                 "Run algorithm.")
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify 'cluster-ui-refresh
                 "Cleaning storage")
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify 'cluster-ui-cleanup
                 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(define-handler flist (data)
  (widget-insert
   (concat "\nList of the Fitness numbers:\n----------------\n"
           data "\n")))


(define-handler metric (string)
  (display-buffer (refac-buffer-list-ensure))
  (with-refac-buffer-list
   (erase-buffer)
   (widget-insert string "\n")))

(define-handler result (string)
  (set (make-local-variable 'str) (cdr string))
  (set (make-local-variable 'algo) (car string))
  (widget-insert
   (propertize "Result of the clustering: \n"
               'face 'bold))
  (widget-insert "-------------------------\n")
  (widget-insert (format "%s" algo))
  (widget-insert
   (replace-regexp-in-string
    "fn" "\n(fitness number: "
    (replace-regexp-in-string
     "\((" "\n\n("
     (replace-regexp-in-string
      "\n)" ")"
      (replace-regexp-in-string
       "\((" "\("
       (replace-regexp-in-string
        "\(\n\(" "("
        (replace-regexp-in-string
         "\))" ")"
         (concat (prin1-to-string str) "\n")))))))))


(define-handler question (desc)
  (let* ((id    (elt desc 0))
         (props (elt desc 1))
         (text  (refac-get-prop props 'text "?"))
         (type  (refac-get-prop props 'type 'string))
         (reply (refac-ask-question type text)))
    (cond ((not reply)
           (refac-send-command 'cancel id))
          (t
           (refac-send-command 'reply id reply)))))

(define-handler showconfig (config)
  (with-current-buffer refac-server-buffer
    (set (make-local-variable 'config-inc-dir) nil)
    (set (make-local-variable 'config-app-dir) nil)
    (set (make-local-variable 'config-out-dir) "/tmp")
    (if (equal config "") (setq config nil))
    (dolist (cfg config)
      (cond ((eq (elt cfg 0) 'appbase)
             (add-to-list 'config-app-dir (elt cfg 1)))
            ((eq (elt cfg 0) 'include)
             (add-to-list 'config-inc-dir (elt cfg 1)))
            ((eq (elt cfg 0) 'output)
             (setq config-out-dir (elt cfg 1)))))
    (goto-char config-start-marker)
    (widget-insert "Application directories:\n")
    (set (make-local-variable 'appdir-list)
         (widget-create 'editable-list
                        :value config-app-dir
                        '(editable-field)))
    (widget-insert "\nInclude directories:\n")
    (set (make-local-variable 'incdir-list)
         (widget-create 'editable-list
                        :value config-inc-dir
                        '(editable-field)))
    (widget-insert "\n")
    (set (make-local-variable 'outdir-menu)
         (widget-create 'menu-choice
                        :value config-out-dir
                        :tag "Output directory"
                        '(const :tag "Original" original)
                        '(editable-field :menu-tag "Specify" "")))
    (widget-insert "\n")
    (set (make-local-variable 'save-button)
         (widget-create 'push-button
                        :notify 'refac-save-config
                        "Save"))
    (set (make-local-variable 'cancel-button)
         (widget-create 'push-button
                        :notify 'refac-hide-config
                        "Cancel"))
    (widget-insert "\n")
    (set (make-local-variable 'config-end-marker) (point-marker))))

(defun refac-progress-start (text max)
  "Returns a new progress reporter that displays progress using
TEXT. MAX is the maximal progress value. See also `refac-progress-update'."
  (list 0 '(0 0 0) max text))

(defun refac-progress-update (progress count)
  "Updates progress reporter PROGRESS with a new progress value
COUNT. Displays a progress message when more than a second has
elapsed since the last progress message."
  (when (> count (car progress))
    (let ((max  (caddr progress))
          (text (cadddr progress)))
      (when (cond ((>= count max)
                   (message "%s finished" text)
                   t)
                  ((>= (cadr (time-since (cadr progress))) 3)
                   (message "%s: %d%%" text (/ (* 100 count) max))
                   t))
        (setcar progress count)
        (setcar (cdr progress) (current-time))
        t))))

(define-handler progress (progress-data)
  (let* ((op    (elt progress-data 0))
         (file  (elt progress-data 1))
         (count (elt progress-data 2))
         (max   (elt progress-data 3))
         (progr (gethash file refac-progress-reporters)))
    (if progr
        (progn
          (refac-progress-update progr count)
          (when (eq count max)
            (remhash file refac-progress-reporters)))
      (let* ((optext (case op
                       ('add "Analysing")
                       ('drop "Dropping")))
             (text (format "%s %s" optext (file-name-nondirectory file)))
             (progr (refac-progress-start text max)))
        (refac-progress-update progr count)
        (when (< count max)
          (puthash file progr refac-progress-reporters))))))

(define-handler queryres (results)
  (when (equal "" results)
    (setq results nil))
  (with-refac-buffer-list
   (erase-buffer)
   (if (not results)
       (widget-insert "No results.")
     (dolist (result results)
       (if (eq (elt result 0) 'nopos)
           (widget-insert (elt result 1))
         (destructuring-bind ((file start-pos end-pos) text)
             (list-from-vector result)
           (refac-widget-link file
                              :start-pos start-pos
                              :end-pos (1+ end-pos)
                              :label text))))))

  (let ((links (remove-if (lambda (x) (eq (elt x 0) 'nopos)) results)))
    (if (> (length links) 1)
        (pop-to-buffer (refac-buffer-list-ensure))
      (let ((win (get-buffer-window +refac-buffer-list+)))
        (when win
          (delete-window win)))
      (if (not results)
          (message "No results.")
        (destructuring-bind ((file start-pos end-pos) text)
            (list-from-vector (elt links 0))
          (let ((buf (find-file-noselect file)))
            (with-current-buffer buf
              (setf refactorerl-mode t))
            (pop-to-buffer buf))
          (message "%s"
                   (apply 'concat (mapcar (lambda (r) (elt r 1)) results)))
          (push-mark (point) t)
          (goto-char start-pos)
          (refac-add-overlay start-pos (1+ end-pos)))))))
