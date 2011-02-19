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

(provide 'refactorerl-server)

(defvar refac-server-process nil
  "The RefactorErl server process.")

(defun refactorerl-start ()
  "Checks if the RefactorErl server is running, and starts it if neccessary."
  (when (equal refactorerl-base-path "")
    (error "Configuration error: RefactorErl base path is not set"))
  (when (not (refac-server-is-running))
    (let* ((base-path (file-name-as-directory refactorerl-base-path))
           (server-cmd (concat base-path "bin/referl"))
           (server-args (list "-base" refactorerl-base-path
                              "-erl" refactorerl-erlang-runtime)))
      (when (stringp refactorerl-wrangler-path)
        (setq server-args (append (list "-wrangler" refactorerl-wrangler-path)
                                  server-args)))
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
      (set-process-filter refac-server-process 'refac-output-filter)
      (set-process-sentinel refac-server-process 'refac-process-sentinel))))

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
            (let* ((data (read (substring line 1)))
                   (message-key (elt data 0))
                   (func (gethash message-key refac-handlers)))
              (if (functionp func)
                  (funcall func (elt data 1))
                  (error "unhandled message type %s" message-key)))))))))

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
    (refac-send-command 'status buffer-file-name)))

;; Generic supporting functions

(defun refac-send-command (cmd &rest args)
  "Send a command to the server. A tuple is sent to the server process, the
first element is `cmd', the rest comes from `args'. See `refac-erl-format'
for available types."
  (let ((msg (concat "{"
                     (mapconcat 'refac-erl-format
                                (append (list cmd) args) ",")
                     "}.\n")))
    ;(insert msg)
    (process-send-string refac-server-process msg)))


(defun refac-erl-format (data)
  "Turns a piece of data into Erlang string representation. Available types:
 * symbols become atoms (the leading \":\" is stripped from keywords)
 * strings become strings
 * numbers become numbers
 * lists become lists with their elements converted recursively
 * vectors become tuples with theit elements converted recursively"
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

(defun refac-server-show-files ()
  (interactive)
  (async-with-refac-buffer-list (refac-send-command 'status_info (list))))

(defun refac-server-show-parseerrors ()
  (interactive)
  (async-with-refac-buffer-list (refac-send-command 'error_attr)))

(defun refac-server-reset-db ()
  (interactive)
  (when (yes-or-no-p "Clear database contents? ")
    (refac-send-command 'reset)))

