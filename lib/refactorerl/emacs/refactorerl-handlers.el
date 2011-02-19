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

(require 'widget)
(require 'cl)
(eval-when-compile
  (require 'wid-edit))

(provide 'refactorerl-handlers)

(defvar refac-handlers (make-hash-table))

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
               (widget-get widget :start-pos)))

(defun refac-visit (filepath pos)
  (find-file-existing filepath)
  (when pos (goto-char pos)))

(define-handler errorlist (parse-errors)
  (when (consp parse-errors)
    (save-excursion
      (with-current-buffer refac-server-buffer
        (goto-char (point-max))
        (dolist (parse-error (car parse-errors))
         (destructuring-bind (&key filepath nexttokentext position) (plist-from-eproplist parse-error)
           (let ((start-pos (elt position 0))
                 (end-pos (elt position 1)))
             (widget-insert "Parse error at ")
             (widget-create 'push-button :notify #'refac-cb-visit
                            :filepath filepath
                            :start-pos start-pos
                            :end-pos end-pos
                            (format "%s:%d-%d" filepath start-pos end-pos))
             (widget-insert "\n"))))))))

(define-handler filestatus (status-lines)
  (when (and (consp status-lines) (consp (car status-lines)))
    (save-excursion    
      (with-current-buffer refac-server-buffer
        (goto-char (point-max))
        (dolist (status-line (car status-lines))
          (destructuring-bind (&key file error type lastmod status) (plist-from-eproplist status-line)
            (widget-create 'push-button :notify #'refac-cb-visit
                           :filepath file
                           (file-name-nondirectory file))
            (widget-insert ":")
            (widget-insert (format "%s" type) "\t"
                           (if (eql error 'yes) "Err" "OK ") "\t"
                           (if (eql lastmod 'undefined) "" (format "%s\t" lastmod))
                           (format "%s" status))
            (widget-insert "\n")))))))

(define-handler reload (file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (revert-buffer t t t)))))

(define-handler backup (string)
  (message "%s" string))

(define-handler add (file)
  (refac-progress-report 'add file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (setq refac-buffer-state :ok)))))

(define-handler drop (file)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
        (setq refac-buffer-state :off)))))

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
        (setq refac-buffer-state :err)))))

(define-handler filelist (file)
  (with-current-buffer (refac-list-buffer)
    (let ((inhibit-read-only t))
      (refac-file-list-add file))))

(define-handler filepos (positions)
  (message "Filepos: %S" positions))


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
