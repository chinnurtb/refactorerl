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

(provide 'refactorerl-mode)
(require 'refactorerl-operations)
(require 'refactorerl-operations-wrangler)
(require 'refactorerl-flasher)
(eval-when-compile
  (require 'cl))

;; Minor mode definition

(setf refactorerl-mode-keybindings
      (append refactorerl-mode-keybindings
              '(("Q"    refactorerl-quit)
                ("R"    refactorerl-restart)
                ("a"    refactorerl-add-file)
                ("d"    refactorerl-drop-file)
                ("D"    refactorerl-debug-shell)

                ("G"    refactorerl-draw-graph)
                ("L"    refactorerl-load-dir)
                ("C"    refactorerl-list-files)
                ("\C-u" refactorerl-update-status)
                ("t"    refactorerl-cluster-agglom)
                ("g"    refactorerl-cluster-genetic)
                ("U"    refactorerl-undo)
                ("TT"   refactorerl-toggle-test))))

(let ((map refactorerl-mode-map)
      (prefix "\C-c\C-r"))
  (loop for (suffix fun) in refactorerl-mode-keybindings
        do (define-key map (concat prefix suffix) fun)))

(let ((map refactorerl-mode-map)
      (prefix "\C-c\C-rw"))
  (loop for (suffix fun) in refactorerl-mode-keybindings/wrangler
        do (define-key map (concat prefix suffix) fun)))

(defun* menu-from-group (group-name &key enable)
  (let ((menu-group (cdr (assq group-name refactorerl-menu-groups))))
    (unless menu-group
      (error "Menu group %s not defined" group-name))
    
    (cons `(,(menu-group-label menu-group) :enable ,enable)
          (menu-group-children menu-group))))

(setf refactorerl-mode-menu
      `(("Start server" refactorerl-restart :enable (lambda () (not (refac-server-is-running))))
        ("Stop server" refactorerl-quit :enable :server)
        ("Select control buffer" refactorerl-control-buffer :enable :server)
        "--"
        ("Add file" refactorerl-add-file :enable :buffer-file)
        ("Drop file" refactorerl-drop-file :enable :buffer-file)
        ("Update status" refactorerl-update-status :enable :buffer-file)
        ("Undo (one step only)" refactorerl-undo
         :enable (lambda () (file-exists-p (concat (refactorerl-data-dir) "data/backup.1")))
         :help "Steps back on the database")
        "--"
        ("Draw graph" refactorerl-draw-graph :enable :server)
        ("Load directory" refactorerl-load-dir :enable :server)
        ("Database contents" refactorerl-list-files :enable :server)
        ("Show parse errors" refactorerl-show-errors :enable :server)
        "--"
        (("Module clustering" :enable :server)
         ("Agglomerative" refactorerl-cluster-agglom :enable :server)
         ("Genetic" refactorerl-cluster-genetic :enable :server))
        "--"
        ,(menu-from-group 'query)
        ,(menu-from-group 'refactor)
        ,(menu-from-group 'rename)
        "--"
        ,(menu-from-group 'dups :enable :wrangler)
        ,(menu-from-group 'wrangler :enable :wrangler)))

(defun refactorerl-expand-enabler (enabler)
  (case enabler
    ((:buffer-state)
     `(and (refac-server-is-running) (eq refac-buffer-state :ok)))
    ((:buffer-file)
     `buffer-file-name)
    ((:server)
     `(refac-server-is-running))
    ((:wrangler)
     `(and (not (eq refactorerl-wrangler-path 'disabled))))
    (otherwise
     (list enabler))))

(defun* refactorerl-easy-menu (name items &key enable)
  `(,name
    ,@(when (and (not (featurep 'xemacs)) enable) `(:active ,(refactorerl-expand-enabler enable)))
    ,@(loop for menu-item in items
            collect (if (consp menu-item)
                        (if (stringp (car menu-item))
                            (destructuring-bind (label callback &key enable help) menu-item
                              `[,label ,callback :active ,(if enable
                                                              (refactorerl-expand-enabler enable)
                                                            't)])
                          (destructuring-bind ((label &key enable help) &body items) menu-item
                            (refactorerl-easy-menu label items :enable enable)))
                      menu-item))))

(easy-menu-define refactorerl-mode-easymenu refactorerl-mode-map "Refactor menu"
  (refactorerl-easy-menu "Refactor" refactorerl-mode-menu))

(defvar refac-buffer-state nil
  "Status of the file:
 - `:off': not part of the active refactoring set
 - `:err': there is an error in the file
 - `:ok': ready for refactoring
 - nil: unknown state (e.g. during parsing)")

(defvar refac-buffer-state-is-off nil)
(defvar refac-buffer-state-is-err nil)
(defvar refac-buffer-state-is-ok nil)
(defvar refac-buffer-state-is-nil nil)

(defun refac-set-buffer-state (new-state)
  "refac-buffer-state needs to be set with this function so that
refac-buffer-state-is-* can be kept in sync. This is needed
because in XEmacs, there is no dynamic modeline lighter, and
thus, we have to make do with the very simple conditional
modeline facility it offers (see also the :lighter of
refactorerl-mode)"
  (setf refac-buffer-state new-state)
  (setf refac-buffer-state-is-off (eq new-state :off))
  (setf refac-buffer-state-is-err (eq new-state :err))
  (setf refac-buffer-state-is-ok (eq new-state :ok))
  (setf refac-buffer-state-is-nil (not new-state)))

(defmacro* define-minor-mode* (name doc &key lighter map menu on off)
  (let ((manual-menu-add (featurep 'xemacs)))
    `(define-minor-mode ,name ,doc
       :lighter ,lighter
       :map map
       (if ,name
           (progn
             ,(when manual-menu-add
                    `(easy-menu-add ,menu))
             ,on)               
           (progn
             ,(when manual-menu-add
                    `(easy-menu-remove ,menu))
             ,off)))))

(define-minor-mode* refactorerl-mode
  "Minor mode providing access to Erlang Refactorer operations.
\\<refactorerl-mode-map>
The first time this mode is activated the refactorer server is started as a
subprocess. It should be stopped before leaving Emacs (`\\[refactorerl-quit]').

Before using this minor mode, you must customize `refactorerl-base-path'.

Key bindings:
\\{refactorerl-mode-map}
"
  :lighter (" Refact"
            (refac-buffer-state-is-err ":error" "")
            (refac-buffer-state-is-off ":off" "")
            (refac-buffer-state-is-nil ":???" ""))  
  :map refactorerl-mode-map
  :menu refactorerl-mode-easymenu
  :on (progn
        (make-local-variable 'refac-buffer-state)
        (loop for var in '(refac-buffer-state-is-off refac-buffer-state-is-ok refac-buffer-state-is-err refac-buffer-state-is-nil)
              do (make-local-variable var))
        (refactorerl-start)
        (refactorerl-update-status)

        (refactorerl-flasher t)

        (add-hook 'after-save-hook 'refac-file-saved t t)
        (add-hook 'post-command-hook 'refac-remove-overlays nil t))
  :off (progn
         (refactorerl-flasher nil)         
         (remove-hook 'after-save-hook 'refac-file-saved t)))
