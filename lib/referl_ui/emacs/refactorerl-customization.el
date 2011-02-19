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

(provide 'refactorerl-customization)

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

(defcustom refactorerl-data-dir "~/.refactorerl"
  "Directory that is used to store RefactorErl data."
  :type '(choice (const :tag "Base directory" base)
                 (directory :tag "Given directory"))
  :group 'refactorerl)

(defun refactorerl-data-dir ()
  (file-name-as-directory
   (case refactorerl-data-dir
     ((base)
      refactorerl-base-path)
     (otherwise
      refactorerl-data-dir))))

(defcustom refactorerl-server-type 'shell
  "Specifies how to start the RefactorErl server process.
If its value is internal, the server process is managed internally.
It its value is external, the server should be started manually.
If its value is shell, a separate process is started by Emacs, with its
  Erlang shell accessible through the buffer *RefactorErlShell*."
  :type '(choice (const :tag "Internally managed server" internal)
                 (const :tag "Externally started server" external)
                 (const :tag "Managed server with shell access" shell))
  :group 'refactorerl)

(defcustom refactorerl-wrangler-path 'disabled
  "Wrangler installation directory."
  :type '(choice (const :tag "No wrangler installation available" disabled)
                 (directory :tag "Directory"))
  :group 'refactorerl)


(defgroup refactorerl-faces nil
  "Faces used by RefactorErl"
  :group 'refactorerl)

(defface refactorerl-header '((t (:inherit bold)))
  "Face used by RefactorErl for section headers"
  :group 'refactorerl-faces)

(defface refactorerl-error '((t (:inherit bold :foreground "red")))
  "Face used by RefactorErl for errors"
  :group 'refactorerl-faces)

(defface refactorerl-link '((t (:foreground "blue" :underline t)))
  "Face used by RefactorErl for links"
  :group 'refactorerl-faces)

(defface refactorerl-highlight '((t (:background "darkseagreen2")))
  "Face used by RefactorErl for highlights"
  :group 'refactorerl-faces)

