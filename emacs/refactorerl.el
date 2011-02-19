;; The contents of this file are subject to the Mozilla Public License
;; Version 1.1 (the "License"); you may not use this file except in
;; compliance with the License. You may obtain a copy of the License at
;; http://www.mozilla.org/MPL/

;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;; License for the specific language governing rights and limitations under
;; the License.

;; The Original Code is RefactorErl.

;; The Initial Developer of the Original Code is Eötvös Loránd University.
;; Portions created by Eötvös Loránd University are Copyright 2008, Eötvös
;; Loránd University. All Rights Reserved.

;; Contributor(s): ______________________________________.

(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(provide 'refactorerl)

;; Customization variables

(defgroup refactorerl nil
  "Erlang Refactorer"
  :tag "RefactorErl"
  :group 'tools)

(defcustom refactorerl-base-path ""
  "RefactorErl base directory."
  :type 'directory
  :group 'refactorerl)

(defcustom refactorerl-erlang-runtime "erl"
  "Erlang emulator command to be used with RefactorErl."
  :type 'string
  :group 'refactorerl)

(defcustom refactorerl-data-dir 'base
  "Directory that is used to store RefactorErl data."
  :type '(choice (const :tag "Base directory" base)
                 (directory :tag "Given directory"))
  :group 'refactorerl)

(defcustom refactorerl-server-shell 'noshell
  "Launch the server using `erlang-shell' to get a direct shell access,
or launch it as a direct subprocess, in which case a remote shell can be
used to access the server."
  :type '(choice (const :tag "Without local shell" noshell)
                 (const :tag "With local shell" shell))
  :group 'refactorerl)

;; Minor mode definition

(defun make-refactorerl-mode-map ()
  "Creates the local keymap for Erlang Refactoring minor mode."
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-rQ" 'refactorerl-quit)
    (define-key map "\C-c\C-rR" 'refactorerl-restart)
    (define-key map "\C-c\C-ra" 'refactorerl-add-file)
    (define-key map "\C-c\C-rd" 'refactorerl-drop-file)
    (define-key map "\C-c\C-rD" 'refactorerl-debug-shell)
    (define-key map "\C-c\C-re" 'refactorerl-extract-function)
    (define-key map "\C-c\C-rG" 'refactorerl-draw-graph)
    (define-key map "\C-c\C-r\C-u" 'refac-update-status)
    map))

(defvar refactorerl-mode-map (make-refactorerl-mode-map)
  "Keymap for RefactorErl minor mode.")

(defvar refac-buffer-state nil
  "Status of the file:
 - `off': not part of the active refactoring set
 - `err': there is an error in the file
 - `ok': ready for refactoring
 - nil: unknown state (e.g. during parsing)")

(define-minor-mode refactorerl-mode
  "Minor mode providing access to Erlang Refactorer operations.
\\<refactorerl-mode-map>
The first time this mode is activated the refactorer server is started as a
subprocess. It should be stopped before leaving Emacs (`\\[refactorerl-quit]').

Before using this minor mode, you must customize `refactorerl-base-path'.

Key bindings:
\\{refactorerl-mode-map}
"
  nil
  (:eval (cond ((not refac-buffer-state) " Refact:???")
               ((eq refac-buffer-state 'off) " Refact:off")
               ((eq refac-buffer-state 'err) " Refact:error")
               ((eq refac-buffer-state 'ok) " Refact")))
  refactorerl-mode-map
  (when refactorerl-mode
    (make-local-variable 'refac-buffer-state)
    (refactorerl-start)
    (refac-update-status)
    (add-hook 'after-save-hook 'refac-file-saved t t))
  (when (not refactorerl-mode)
    (remove-hook 'after-save-hook 'refac-file-saved t)))


;; Functions implementing interactive functionality (usually bound to a key)

(defun refactorerl-quit ()
  "Stops the RefactorErl server process."
  (interactive)
  (when (refac-server-is-running)
    (message "Initiating shutdown...")
    (refac-send-command 'quit)
    (while (refac-server-is-running)
      (sleep-for 0.1))
    (message "RefactorErl server has stopped.")))

(defun refactorerl-restart ()
  "Restarts the RefactorErl server process."
  (interactive)
  (when (refac-server-is-running)
    (refactorerl-quit)
    (sleep-for 0.5))
  (refactorerl-start))

(defun refactorerl-add-file ()
  "Add the visited file to the active refactoring set."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state 'off))
         (error "Already added"))
        (t
         (setq refac-buffer-state nil)
         (refac-send-command 'add buffer-file-name))))

(defun refactorerl-drop-file ()
  "Remove the visited file from the active refactoring set."
  (interactive)
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((eq refac-buffer-state 'off)
         (error "Not in the active set"))
        (t
         (setq refac-buffer-state nil)
         (refac-send-command 'drop buffer-file-name))))

(defun refactorerl-draw-graph (file type)
  "Draw the contents of the graph into a file. The generated file is suitable
as an input to the graphviz package.

A numerical prefix argument can be used to filter the contents of the graph:
 1: full graph
 2: semantical, structural, and syntactical links
 3: structural and syntactical links
 4: syntactical links (bare syntax tree)
 5: syntactical and lexical links
 6: lexical links
 7: tokens
 8: all except lexical edges (for testing purpose)"
  (interactive "FGraph file: \np")
  (refac-send-command 'draw (expand-file-name file) type))

(defun refactorerl-debug-shell ()
  "Start an inferior Erlang shell (using the standard Erlang mode package)
that connects to the RefactorErl server."
  (interactive)
  (let ((inferior-erlang-machine-options
         '("-sname" "debug"
           "-remsh" "refactorerl@localhost")))
    (erlang-shell)))

(defun refactorerl-extract-function (beg end name)
  "Performs the Extract Function refactoring. Operates on the expression or
expressions between the point and the mark."
  (interactive "r\nsFunction name: ")
  (cond ((not buffer-file-name)
         (error "No visited file"))
        ((not (eq refac-buffer-state 'ok))
         (error "File is not ready for refactoring"))
        (t
         (refac-send-command 'extract buffer-file-name beg end name)
         (deactivate-mark))))

(defun refactorerl-list-files (&optional same-win)
  "Shows the contents of the active refactoring set."
  (interactive)
  (with-current-buffer (refac-list-buffer)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (refac-send-command 'filelist)
    (set (make-local-variable 'last-file-dir) ""))
  (if same-win
      (switch-to-buffer (refac-list-buffer))
    (switch-to-buffer-other-window (refac-list-buffer))))

(defun refactorerl-load-dir (dirname)
  "Adds the contents of a directory to the active refactoring set."
  (interactive "D")
  (let ((files (directory-files dirname t ".*\\.erl$")))
    (if (equal 0 (length files))
        (error (concat "No Erlang files in " dirname))
      (setq refac-progress [nil 0 ""])
      (aset refac-progress 0 files)
      (aset refac-progress 2 dirname)
      (dolist (file files)
        (refac-send-command 'add file)))))

;; Implementation of non-interactive functionality

(defvar refac-server-process nil
  "The RefactorErl server process.")

(defun refactorerl-start ()
  "Checks if the RefactorErl server is running, and starts it if neccessary."
  (when (equal refactorerl-base-path "")
    (error "Configuration error: RefactorErl base path is not set"))
  (when (not (refac-server-is-running))
    (let* ((base-path (file-name-as-directory refactorerl-base-path))
           (srv-args (list
                      "-sname"  "refactorerl@localhost"
                      "-boot"   (concat base-path "refactorerl")
                      "-config" (concat base-path "sys.config")
                      "-pa"     (concat base-path "ebin")
                      "-pa"     (concat base-path "build")
                      "+W"      "w"))
           (inp-args (list
                      "-run"    "refac_emacs"
                      "-noshell"))
           (default-directory (if (eq refactorerl-data-dir 'base)
                                  refactorerl-base-path
                                refactorerl-data-dir)))
      (when (eq refactorerl-server-shell 'shell)
        (let ((inferior-erlang-machine refactorerl-erlang-runtime)
              (inferior-erlang-machine-options srv-args)
              (inferior-erlang-process-name "RefactorErlShell")
              (inferior-erlang-buffer-name "*RefactorErlShell*"))
          (erlang-shell))
        (setq srv-args (list "-sname" "refacinput@localhost"
                             "-pa" (concat base-path "ebin"))))
      (setq refac-server-buffer (refac-make-server-buffer))
      (setq refac-server-process
            (apply 'start-process
                   (append (list "RefactorErl" refac-server-buffer
                                 refactorerl-erlang-runtime)
                           srv-args
                           inp-args)))
      (set-process-filter refac-server-process 'refac-output-filter)
      (set-process-sentinel refac-server-process 'refac-process-sentinel)
      )))

(defvar refac-output-buffer "")
(defun refac-output-filter (proc string)
  "Analyses server output, puts unrecognised messages into the process buffer."
  (setq refac-output-buffer (concat refac-output-buffer string))
  (save-match-data
    (let (eol line)
      (while (setq eol (string-match "\n" refac-output-buffer))
        (setq line (substring refac-output-buffer 0 eol))
        (setq refac-output-buffer (substring refac-output-buffer (1+ eol)))
        (if (and (< 2 (length line))
                 (equal "##" (substring line 0 2)))
            (refac-handle-output (substring line 2 5) (substring line 5))
          (with-current-buffer (process-buffer proc)
            (save-excursion
              (goto-char (point-max))
              (widget-insert (concat line "\n")))))))))

(defun refac-process-sentinel (proc event)
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (concat "RefactorErl input process " event)))))


(defun refac-handle-output (type string)
  "Handles server messages."
  (cond ((equal type "ERR") ; Error message
         (message (concat "RefactorErl internal error: " string)))
        ((equal type "MSG") ; Simple string message
         (message string))
        ((equal type "ADD") ; Added file
         (refac-progress-report 'add string)
         (let ((buf (get-file-buffer string)))
           (when buf
             (with-current-buffer buf
               (setq refac-buffer-state 'ok)))))
        ((equal type "DRP") ; Dropped file
         (let ((buf (get-file-buffer string)))
           (when buf
             (with-current-buffer buf
               (setq refac-buffer-state 'off)))))
        ((equal type "INV") ; Problem with a file
         (refac-progress-report 'err string)
         (let ((buf (get-file-buffer string)))
           (when buf
             (with-current-buffer buf
               (setq refac-buffer-state 'err)))))
        ((equal type "RLD") ; File should be reloaded
         (let ((buf (get-file-buffer string)))
           (when buf
             (with-current-buffer buf
               (revert-buffer t t t)))))
        ((equal type "INC") ; Include directory
         (with-current-buffer refac-server-buffer
           (add-to-list 'config-inc-dir string)))
        ((equal type "APP") ; Application directory
         (with-current-buffer refac-server-buffer
           (add-to-list 'config-app-dir string)))
        ((equal type "OUT") ; Output directory
         (with-current-buffer refac-server-buffer
           (if (equal string "original")
               (setq config-out-dir 'original)
             (setq config-out-dir string))))
        ((equal type "CFG") ; End of configuration
         (with-current-buffer refac-server-buffer
           (refac-display-config)))
        ((equal type "LFN") ; List of file names
         (with-current-buffer (refac-list-buffer)
           (let ((inhibit-read-only t))
             (refac-file-list-add string))))
        ))

(defvar refac-progress [])
(defun refac-progress-report (type file)
  (when (< 0 (length refac-progress))
    (let ((files (aref refac-progress 0))
          (err (aref refac-progress 1))
          (dir (aref refac-progress 2)))
      (when (member file files)
        (setq files (delete file files))
        (aset refac-progress 0 files)
        (when (equal type 'err)
          (setq err (1+ err))
          (aset refac-progress 1 err))
        (if (not (eq files nil))
            (message (concat "Loading " dir ": "
                           (format "%d remaining (%d errors)"
                                   (length files) err)))
          (message (concat "Loading " dir " done"
                         (format " (%d errors)" err)))
          (setq refac-progress []))))))

(defun refac-list-buffer ()
  (get-buffer-create "*RefactorErl File List*"))

(defun refac-file-list-add (stname)
  (let* ((state (if (equal (elt stname 0) ?!) 'err 'ok))
         (name (if (equal state 'ok) stname (substring stname 1)))
         (dirname (file-name-directory name))
         (basename (file-name-nondirectory name)))
    (unless (equal dirname last-file-dir)
      (insert (concat (propertize dirname 'face 'dired-header) ":\n"))
      (setq last-file-dir dirname))
    (if (equal state 'ok)
        (insert "   ")
      (insert (propertize " ! " 'face 'compilation-error)))
    (insert-text-button basename
                        'filename name
                        'help-echo "mouse-2, RET: open file"
                        'action (lambda (btn)
                                  (find-file (button-get btn 'filename))))
    (insert " (")
    (insert-text-button "drop"
                        'filename name
                        'help-echo "mouse-2, RET: drop file"
                        'action (lambda (btn)
                                  (refac-send-command
                                   'drop (button-get btn 'filename))
                                  (refactorerl-list-files t)))
    (insert ")\n")))


(defun refac-file-saved ()
  "Informs the server that a file is changed. Runs from `after-save-hook'."
  (when (or (eq refac-buffer-state 'ok)
            (eq refac-buffer-state 'err))
    (setq refac-buffer-state nil)
    (refac-send-command 'add buffer-file-name)))

(defun refac-update-status ()
  "Requests a file status update from the server."
  (interactive)
  (when buffer-file-name
    (refac-send-command 'status buffer-file-name)))

;; Server buffer manipulation

(defun refac-make-server-buffer (&rest args)
  (set-buffer (get-buffer-create "*RefactorErl*"))
  (kill-all-local-variables)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert "**RefactorErl Server Control Buffer**\n")
  (widget-create 'push-button
                 :notify 'refac-show-config
                 "Configuration")
  (widget-insert "\n")
  (set (make-local-variable 'config-start-marker) (point-marker))
  (widget-insert "------------------------------------------------\n")
  (widget-create 'push-button
                 :notify (lambda (&rest args) (refactorerl-list-files))
                 "Show database contents")
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest args)
                           (call-interactively 'refactorerl-load-dir))
                 "Load directory contents")
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest args)
                           (when (yes-or-no-p "Clear database contents? ")
                             (refac-send-command 'reset)))
                 "Reset database")
  (widget-insert " (Hint: open configuration and save it after reset)\n")
  (widget-create 'push-button
                 :notify 'refac-make-server-buffer
                 "Clear buffer")
  (widget-insert "\n================================================\n")
  (use-local-map widget-keymap)
  (widget-setup)
  (current-buffer))

(defun refac-show-config (&rest args)
  (set (make-local-variable 'config-inc-dir) nil)
  (set (make-local-variable 'config-app-dir) nil)
  (set (make-local-variable 'config-out-dir) "/tmp")
  (refac-send-command 'showconfig))

(defun refac-display-config ()
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
  (set (make-local-variable 'config-end-marker) (point-marker)))

(defun refac-hide-config (&rest args)
  (widget-delete appdir-list)
  (widget-delete incdir-list)
  (widget-delete outdir-menu)
  (widget-delete save-button)
  (widget-delete cancel-button)
  (let ((inhibit-read-only t))
    (delete-region config-start-marker config-end-marker))
  (set-marker config-end-marker nil))

(defun refac-save-config (&rest args)
  (refac-send-command 'saveconfig
                      (widget-value appdir-list)
                      (widget-value incdir-list)
                      (widget-value outdir-menu))
  (refac-hide-config))

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
 * symbols become atoms
 * strings become strings
 * numbers become numbers
 * lists become lists with their elements converted recursively"
  (cond ((equal data nil)
         "[]")
        ((symbolp data)
         (symbol-name data))
        ((stringp data)
         (concat "\"" (apply 'concat (mapcar 'refac-escape-char data)) "\""))
        ((numberp data)
         (number-to-string data))
        ((listp data)
         (concat "[" (mapconcat 'refac-erl-format data ",") "]"))))

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
