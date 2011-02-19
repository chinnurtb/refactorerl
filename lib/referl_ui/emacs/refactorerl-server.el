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
(require 'erlang)
(eval-when-compile
  (require 'wid-edit)
  (require 'cl))

(require 'refactorerl-handlers)
(require 'refactorerl-ui)
(require 'refactorerl-customization)
(require 'refactorerl-form)

(provide 'refactorerl-server)

(defvar refac-debug-mode nil)
(defmacro* refac-debug (&body body)
  `(when refac-debug-mode ,@body))

(defvar refac-server-process nil
  "The RefactorErl server process.")

(defun refactorerl-start ()
  "Checks if the RefactorErl server is running, and starts it if neccessary."
  (when (equal refactorerl-base-path "")
    (error "Configuration error: RefactorErl base path is not set"))
  (when (not (refac-server-is-running))
    (let* ((base-path (file-name-as-directory refactorerl-base-path))
           (server-cmd (concat base-path "bin/referl"))
           (server-args (append (list "-base" refactorerl-base-path)
                                (list "-erl" refactorerl-erlang-runtime)
                                (when (stringp refactorerl-wrangler-path)
                                  (list "-wrangler" refactorerl-wrangler-path)))))
      (when (eq refactorerl-server-type 'shell)
        (make-directory (refactorerl-data-dir) t)
        (let ((default-directory (refactorerl-data-dir)))
          (with-current-buffer (apply #'make-comint "RefactorErlShell" server-cmd nil server-args)
            (require 'erlang)
            (when (featurep 'erlang)
              (erlang-shell-mode)))))
      (setf refac-server-buffer
            (save-excursion (refac-make-server-buffer)))
      (setf refac-server-process
            (apply #'start-process "RefactorErl" refac-server-buffer server-cmd
                   (append server-args (if (eq refactorerl-server-type 'internal)
                                           (list "-emacs")
                                           (list "-emacs" "-client" "-name" "referlemacs@localhost")))))
      ;; (with-current-buffer refac-server-buffer
      ;;   (make-local-variable '*reqid*)
      ;;   (make-local-variable '*reqid-queue*))
      (set-process-filter refac-server-process 'refac-output-filter)
      (set-process-sentinel refac-server-process 'refac-process-sentinel))))

(defvar *refac-callbacks* (make-hash-table :test 'equal))

(defun refac-output-filter (proc string)
  "Analyses server output, puts unrecognised messages into the process buffer."
  (when (equal proc refac-server-process)
    (setf refac-output-buffer (concat refac-output-buffer string))
    (save-match-data
      (let (eol line)
        (while (setq eol (string-match "\n" refac-output-buffer))
          (setf line (substring refac-output-buffer 0 eol))
          (setf refac-output-buffer (substring refac-output-buffer (1+ eol)))
          (if (or (equal line "")
                  (not (equal (elt line 0) ?\02)))
              (with-current-buffer (process-buffer proc)
                (save-excursion
                  (goto-char (point-max))
                  (widget-insert (concat line "\n"))))
            (let ((data (read (substring line 1))))
              (refac-debug
               (message "RECV: %s" (substring line 1)))
              ;; This is either a new ID or part of an existing conversation
              (if (eql (elt data 0) 'reqid)
                  (handle-reqid (elt data 1))

                (lexical-let ((reqid (elt data 0)))
                  (case (elt data 1)
                    (statusinfo
                     (mapc #'refac-handle-statusinfo (list-from-vector (elt data 2))))

                    (reply
                     (let ((callback (gethash reqid *refac-callbacks*)))
                       (when callback
                         (unwind-protect
                             (apply callback (list-from-vector (elt data 2)))
                           (remhash reqid *refac-callbacks*)))))

                    (question
                     (refac-debug (message "%s" (elt data 2)))
                     (destructuring-bind (qid formspec)
                         (list-from-vector (elt data 2))

                       (lexical-let ((qid qid))
                         ;; (refac-send-command 'cancel reqid)
                         (create-form (parse-formspec formspec)
                                      (lambda (&rest results)
                                        (refac-send reqid 'reply qid (to-yesno results)))
                                      (lambda ()
                                        (refac-send reqid 'cancel qid))))))

                    (progress
                     (refac-debug
                      (message "PROGRESS %s" (list-from-vector (elt data 2)))))))))))))))

(defun to-yesno (list)
  (loop for x in list
        collect (if x
                    (case x
                      ('t 'yes)
                      (otherwise x))
                  'no)))

(defun refac-handle-statusinfo (statusinfo)
  (let ((key (elt statusinfo 0))
        (payload (elt statusinfo 1)))
    (case key
      (change
       (refac-handle-change payload))
      (shutdown
       (message "%s" payload)))))

(defun refac-handle-change (changes)
  (dolist (change changes)
    (refac-debug
     (message "Change: %s" change))
    (destructuring-bind (filename events) change
      (let ((buffer (get-file-buffer filename)))
        (when buffer
          (with-current-buffer buffer
            (loop for (event-key . event-args) in events
                  do (case event-key
                       (added
                        (message "Added: %s" filename)
                        (refac-set-buffer-state :ok))
                       (dropped
                        (message "Dropped: %s" filename)
                        (refac-set-buffer-state :off))
                       (error
                        (message "Error in: %s" filename)
                        (refac-set-buffer-state :err))
                       (present
                        (refac-set-buffer-state (case (car event-args)
                                                  (true (progn (message "Added: %s" filename) :ok))
                                                  (false (progn (message "Dropped: %s" filename) :off)))))
                       (content
                        (revert-buffer t t t))
                       (rename
                        (set-visited-file-name (car event-args)))))))))))

(defun refac-process-sentinel (proc event)
  (when (equal proc refac-server-process)
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (concat "RefactorErl input process " event))))))

(defun refac-get-prop (props key default-value)
  (loop for prop in props
        when (and (vectorp prop) (eq (elt prop 0) key)) return (elt prop 1)
        else when (eq prop key) return 'true
        finally return default-value))

(defun refac-file-saved ()
  "Informs the server that a file is changed. Runs from `after-save-hook'."
  (when (member refac-buffer-state '(:ok :err))
    (refac-set-buffer-state nil)
    (refac-send-command 'add buffer-file-name)))

(defun refactorerl-update-status ()
  "Requests a file status update from the server."
  (interactive)
  (when buffer-file-name
    (refac-send/callbacks ('status buffer-file-name)
                          (:reply (ok changes)
                                  (refac-handle-change changes)))))

;; Generic supporting functions

;; Conversation IDs
(defvar *reqid*)
(defvar *reqid-queue* nil)

(defun handle-reqid (new-id)
  (setf *reqid* new-id)
  (when *reqid-queue*
    (funcall *reqid-queue*)
    (setf *reqid-queue* nil)))

(defun refac-send (&rest args)
  "Send `args' directly to the server. Use `refac-send-command'
to automatically get a new conversation ID"
  (let ((msg (refac-erl-format (vconcat args))))
    (refac-debug (message "SEND: %s" msg))
    (process-send-string refac-server-process (concat msg ".\n"))))

(defun refac-get-id ()
  (refac-send 'getid))

(defmacro* with-refac-id ((&rest vars) &body body)
  "Send `getid' to the server, and call `body' once the response
with the new ID has arrived. By then, global variable `*reqid*'
is bound to the latest request ID."
  `(progn
     (when *reqid-queue*
       (error "GetID reentrancy"))
     (lexical-let (,@(loop for var in vars
                           collect `(,var ,var)))
       (setf *reqid-queue* (lambda ()
                             (unwind-protect
                                 (progn ,@body)
                               (setf *reqid-queue* nil)))))
     (refac-get-id)))

(defmacro* refac-send-command (&rest cmdargs)
  "Send a command to the server. After getting a new conversation
ID, a tuple is sent to the server process, the first element is
`cmd', the rest comes from `args'. See `refac-erl-format' for
available types."
  `(refac-send/callbacks ,cmdargs (:reply (ok cmdres)
                                  (refac-handle-cmdres cmdres))))

(defun refac-handle-cmdres (cmdres)
  (if (or (equal cmdres "") (not cmdres) (not (equal (list-length cmdres) 2)))
    (message "ok")
   (progn (setq type (elt cmdres 0))
    (setq data (elt cmdres 1))
    (case type
     ('result
       (message "RefactorErl: transformation done."))
     ('abort (let ((text (elt data 1)))
              (message "RefactorErl: denied: %s" text)))
;     ('error (let ((text (elt data 1)))
;              (message "RefactorErl: error: %s" text)))
     (otherwise
;              (message "RefactorErl: unknown error: %s" cmdres)
              (message "ok") ; TODO: separate funs for refactoring and other commands
              )))))

(defmacro* refac-send/callbacks ((cmd &rest args) &body body)
  "Send a command to server, and then handle progress and reply messages. See the following example:
  (refac-send/callbacks ('status buffer-file-name)
    (:reply (ok changes)
      (refac-handle-change changes)))"
  `(progn
     (lexical-let ((cmd ,cmd)
                  (args (list ,@args)))
      (refac-debug (message "Cmd: %s" cmd))
      (refac-debug (message "args: %s" args))
      (with-refac-id ()
                     (setf (gethash *reqid* *refac-callbacks*)
                           (lambda (status &rest status-args)
                             (case status
                               ,@(loop for (kind . clause) in body when (eql kind :reply)
                                       collect (destructuring-bind ((status &rest lambda-form) &body action) clause
                                                 `(,status
                                                   (destructuring-bind ,lambda-form status-args
                                                     ,@action))))
                               ((error)
                                (message "RefactorErl error: %s" (elt (elt status-args 0) 1))))))
                     (apply #'refac-send *reqid* cmd args)))))

;; Conversion between Erlang and ELisp representation
(defun refac-erl-format (data)
  "Turns a piece of data into Erlang string representation. Available types:
 * symbols become atoms (the leading \":\" is stripped from keywords)
 * strings become strings
 * numbers become numbers
 * lists become lists with their elements converted recursively
 * vectors become tuples with their elements converted recursively"
  (cond ((equal data nil)
         "[]")
        ((keywordp data)
         (substring (symbol-name data) 1))
        ((symbolp data)
         (symbol-name data))
        ((stringp data)
         (concat "\"" (apply #'concat (mapcar 'refac-escape-char data)) "\""))
        ((numberp data)
         (number-to-string data))
        ((listp data)
         (concat "[" (mapconcat 'refac-erl-format data ",") "]"))
        ((vectorp data)
         (concat "{" (mapconcat 'refac-erl-format data ",") "}"))))

(defun refac-escape-char (char)
  "Perform character escaping according to Erlang rules."
  (cond ((eq char ?\")     "\\\"")
        ((eq char ?\\)     "\\\\")
        ((or (< char 32)
             (> char 126)) (format "\\%03o" char))
        (t                 (string char))))


;; Server maintenance
(defun refac-server-is-running ()
  "Checks if the RefactorErl server is running."
  (and refac-server-process
       (equal 'run (process-status refac-server-process))))

(defun refac-save-config (&rest args)
  (refac-send-command 'saveconfig
                      (widget-value appdir-list)
                      (widget-value incdir-list)
                      (widget-value outdir-menu))
  (refac-hide-config))

(defun refactorerl-server-show-files ()
  (interactive)
  (async-with-refac-buffer-list (refac-send-command 'status_info (list))))

(defun refactorerl-server-reset-db ()
  (interactive)
  (when (yes-or-no-p "Clear database contents? ")
    (refac-send-command 'reset)))

