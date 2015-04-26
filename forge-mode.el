;; -*- lexical-binding: t; -*-

;;
;; A minor mode to help development of minecraft mods using MinecraftForge and MCP
;;
;; Author: Alfredo Mazzinghi (qwattash)
;; 
;; License: GPLv3
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 

;; =============================================================================
;; Commands description
;; 
;; M-g m Run minecraft client with the mod(s)
;; M-g d Debug minecraft client with the mod(s)
;; M-g n Run minecraft server with the mod(s)
;; M-g e Debug minecraft server with the mod(s)

(defcustom forge-gradle-buffer-name
  "*forge-gradle*"
  "The gradle output buffer"
  :group 'forge)

(defcustom forge-gradle-bin
  "gradlew" ;"echo ./gradlew Listening for transport dt_socket at address: 5005"
  "The gradle wrapper script in the main project directory"
  :group 'forge)

(defcustom forge-jdb-trigger-re
  "Listening for transport dt_socket at address: *\\([0-9]+\\)"
  "The regex used to extract the address to which jdb should attach
this should have only one group enclosing the address number."
  :group 'forge)

(defcustom forge-search-gradle
  t
  "forge-mode will look for the gradle executable in all the parent tree up until the home of the user. 
The search for the gradle script can be disabled in configuration by setting forge-search-gradle to nil.
In the latter case the value in forge-gradle bin will be used directly to run the process."
  :group 'forge)

;; Internal variables

(defvar forge-jdb-buffer-name
  nil
  "The current java debugger buffer, this is generated in the same way GUD
names its buffer so we can control where the buffer appears")

(defvar current-gradle-w
  nil
  "The gradle window")

(defvar current-jdb-w
  nil
  "The jdb window")

;; window & gui handling

(defun forge-create-popup ()
  "Create a window that can accomodate a forge buffer, the window is created from an existing forge window
if valid, otherwise the top level window is split"
  (if (window-valid-p current-gradle-w)
      (split-window current-gradle-w nil 'left)
    (if (window-valid-p current-jdb-w)
	(split-window current-jdb-w nil 'right)
      (let* ((top (frame-root-window))
	     (two-thirds (* 2 (/ (window-total-height top) 3))))
	(split-window top two-thirds 'below)))))


(defun forge-enable-gradle (buffer-name)
  "Make window for the gradle output buffer if needed and pop the buffer there"
  (if (window-valid-p current-gradle-w) ; if a previously created window exists
      (set-window-buffer current-gradle-w buffer-name) ; just set the buffer
    (let ((gradle-w (get-buffer-window buffer-name))) ; else 
      (if (null gradle-w) ; check the window that contains the buffer we want to display
	  (let ((win (forge-create-popup))) ; if it does not exist, create a new window and return it
	    (setq current-gradle-w win)
	    (set-window-buffer win buffer-name))
	  (setq current-gradle-w gradle-w))))) ; if it exists, store it and return
  

(defun forge-enable-jdb (buffer-name)
  "Make window for the jdb in-out buffer if needed and pop the buffer there"
  (if (window-valid-p current-jdb-w)
      (set-window-buffer current-jdb-w buffer-name)
    (let ((win (forge-create-popup)))
      (set-window-buffer win buffer-name)
      (setq current-jdb-w win))))

;; buffer content logic


(defun forge-build-command (gradle-task)
  "Build a directory independant command that runs gradle with the given task"
  (let ((base (format "cd %s && %s" (file-name-directory forge-gradle-bin) forge-gradle-bin)))
    (concat base " " gradle-task)))

(defun forge-gradle-filter (proc string)
  "A custom filter for the gradle process output.
The filter looks for the string signalling that debugger is listening and jdb can be attached
and trigger the jdb task to be started correctly."
  (when (string-match forge-jdb-trigger-re string)
    (let ((address (match-string 1 string)))
      (forge-exec-jdb-task address))))


(defun forge-setup-jdb-attach-filter (gradle-process)
  "Setup a proxy buffer filter for the gradle process that intercepts the output before sending
it to the default filter, the custom filter looks for the debugger server ready string and attach
jdb as soon as it is required."
  (add-function :before (process-filter gradle-process) #'forge-gradle-filter))


(defun forge-exec-gradle-task (command)
  "Execute a gradle task in the forge gradle temporary buffer"
  (let ((process-handle "forge-gradle")
	(gradle-command (forge-build-command command)))
    (start-process-shell-command process-handle forge-gradle-buffer-name gradle-command)))


(defun forge-exec-jdb-task (address)
  "Execute jdb and attach to the address initialised by gradle, then send the run input to
the process so that the debugging is started automatically."
  (setq forge-jdb-buffer-name (format "*gud-%s*" address))
  (let ((jdb-buffer (get-buffer-create forge-jdb-buffer-name)))
    (forge-enable-jdb forge-jdb-buffer-name)
    (with-selected-window (get-buffer-window forge-jdb-buffer-name)
      (with-current-buffer jdb-buffer
	(jdb (concat "jdb -attach " address))
	(process-send-string (get-buffer-process jdb-buffer) "run\r\n")))))

;; gradle running context

(defun forge-find-gradle (cwd)
  "Locate the first parent directory containing the gradle script
the directory will be used as cwd before executing gradle"
  (let* ((dir-list (directory-files cwd))
	 (gradle-bin (file-name-nondirectory forge-gradle-bin))
  	 (full-cwd (expand-file-name cwd))
	 (parent (file-name-directory (directory-file-name full-cwd)))
  	 (home (expand-file-name "~")))
    (if (not (string= (directory-file-name home) (directory-file-name full-cwd)))
	(if (member gradle-bin dir-list)
	    (concat (file-name-as-directory full-cwd) gradle-bin)
	  (forge-get-gradle-base-dir parent))
      (error "Gradle executable not found"))))

;; user interface functions

(defun forge-run-client ()
  "Run minecraft client and load the mod"
  (interactive)
  (get-buffer-create forge-gradle-buffer-name)
  (forge-enable-gradle forge-gradle-buffer-name)
  (forge-exec-gradle-task "runClient"))


(defun forge-debug-client ()
  "Run the minecraft client in debug mode and attach the debugger in a separate comint buffer"
  (interactive)
  (get-buffer-create forge-gradle-buffer-name)
  (forge-enable-gradle forge-gradle-buffer-name)
  (forge-setup-jdb-attach-filter (forge-exec-gradle-task "runClient --debug-jvm")))
  

(defun forge-run-srv ()
  (interactive)
  (get-buffer-create forge-gradle-buffer-name)
  (forge-enable-gradle forge-gradle-buffer-name)
  (forge-exec-gradle-task "runServer"))

(defun forge-debug-srv ()
  (interactive)
  (get-buffer-create forge-gradle-buffer-name)
  (forge-enable-gradle forge-gradle-buffer-name)
  (forge-setup-jdb-attach-filter (forge-exec-gradle-task "runServer --debug-jvm")))

;; keybindings


(define-minor-mode forge-mode
  "A minor mode that helps developing minecraft mods using MinecraftForge and MCP"
  :lighter " forge-mode"
  :group 'forge
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-g m") 'forge-run-client)
	    (define-key map (kbd "M-g d") 'forge-debug-client)
	    (define-key map (kbd "M-g n") 'forge-run-srv)
	    (define-key map (kbd "M-g e") 'forge-debug-srv)
	    map)
  (if forge-search-gradle
      (setq forge-gradle-bin (forge-find-gradle "."))))

(provide 'forge-mode)
