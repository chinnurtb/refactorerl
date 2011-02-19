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


(provide 'refactorerl-mode)
(require 'refactorerl-operations)
(require 'refactorerl-operations-wrangler)
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
                ("S"    refactorerl-add-checkpoint)
                ("c"    refactorerl-clean)
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

(setf refactorerl-mode-menu
      `(("Start server" refactorerl-restart :enable (not (refac-server-is-running)))
        ("Stop server" refactorerl-quit :enable :server)
        ("Select control buffer" refactorerl-control-buffer :enable :server)
        "--"
        ("Add file" refactorerl-add-file :enable :buffer-file)
        ("Drop file" refactorerl-drop-file :enable :buffer-file)
        ("Update status" refactorerl-update-status :enable :buffer-file)
        ("Undo (one step only)" refactorerl-undo
         :enable (file-exists-p
                  (concat
                   (file-name-as-directory refactorerl-base-path) "data/backup.1"))
         :help "Steps back on the database")
        "--"
        ("Draw graph" refactorerl-draw-graph :enable :server)
        ("Load directory" refactorerl-load-dir :enable :server)
        ("Database contents" refactorerl-list-files :enable :server)
        "--"
        (("Module clustering" :enable :server)
         ("Agglomerative" refactorerl-cluster-agglom :enable :server)
         ("Genetic" refactorerl-cluster-genetic :enable :server))
        "--"
        ,@(cdr (assoc nil refactorerl-mode-menu/ops))
        ,@(loop for (group . items) in refactorerl-mode-menu/ops
                when group collect (cons (list group) items))
        "--"
        (("Wrangler")
         ,@refactorerl-mode-menu/wrangler))
      )

(defun refactorerl-expand-enabler (enabler)
  (case enabler
    ((:buffer-state)                            
     `(eq refac-buffer-state :ok))                           
    ((:buffer-file)
     `buffer-file-name)
    ((:server)
     `(refac-server-is-running))
    (otherwise
     enabler)))

(defun refactorerl-easy-menu (name items)
  (cons name
        (loop for menu-item in items
              collect (if (consp menu-item)
                          (if (stringp (car menu-item))
                              (destructuring-bind (label callback &key enable help) menu-item
                                `[,label ,callback :active ,(if enable
                                                                (refactorerl-expand-enabler enable)
                                                              't)])
                            (destructuring-bind ((label &key enable help) &body items) menu-item
                              (refactorerl-easy-menu label items)))
                        menu-item))))

(easy-menu-define refactorerl-mode-easymenu refactorerl-mode-map "Refactor menu"
  (refactorerl-easy-menu "Refactor" refactorerl-mode-menu))

(defvar refac-buffer-state nil
  "Status of the file:
 - `:off': not part of the active refactoring set
 - `:err': there is an error in the file
 - `:ok': ready for refactoring
 - nil: unknown state (e.g. during parsing)")

(defmacro* define-minor-mode* (name doc &key lighter/static lighter/dynamic map menu on off)
  `(if (featurep 'xemacs)
       (define-minor-mode ,name ,doc
         :lighter ,lighter/static
         :map ,map
         (if ,name
             (progn (easy-menu-add ,menu)
                    ,on)
           (progn (easy-menu-remove ,menu)
                  ,off)))
     (define-minor-mode ,name ,doc
       :lighter ,(if lighter/dynamic
                     `(:eval ,lighter/dynamic)
                     lighter/static)
       :map map
       (if ,name ,on ,off))))

(define-minor-mode* refactorerl-mode
  "Minor mode providing access to Erlang Refactorer operations.
\\<refactorerl-mode-map>
The first time this mode is activated the refactorer server is started as a
subprocess. It should be stopped before leaving Emacs (`\\[refactorerl-quit]').

Before using this minor mode, you must customize `refactorerl-base-path'.

Key bindings:
\\{refactorerl-mode-map}
"
  :lighter/static " Refact"
  :lighter/dynamic (concat " Refact"
                           (let ((refac-state-str
                                  (if (refac-test-mode-p)
                                      "TEST"
                                    (case refac-buffer-state
                                      ((:ok) nil)
                                      ((:off) "off")
                                      ((:err) "error")
                                      (otherwise "???")))))
                             (if refac-state-str (concat ":" refac-state-str))))
  :map refactorerl-mode-map
  :menu refactorerl-mode-easymenu
  :on (progn
        (make-local-variable 'refac-buffer-state)
        (refactorerl-start)
        (refactorerl-update-status)
        (add-hook 'after-save-hook 'refac-file-saved t t))
  :off (remove-hook 'after-save-hook 'refac-file-saved t))
