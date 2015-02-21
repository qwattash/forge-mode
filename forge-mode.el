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


(defcustom forge-gradle-buffer-name
  "*forge-gradle*"
  "The gradle output buffer")

(defcustom forge-jdb-buffer-name
  "*forge-dbg*"
  "The java debugger buffer")

(defcustom forge-key-run-client
  ""
  "Key binding for the forge-run-client function")

(defcustom forge-key-run-server
  ""
  "Key binding for the forge-run-server function")

(defcustom forge-key-debug-client
  ""
  "Key binding for the forge-debug-client function")

(defcustom forge-key-run-server
  ""
  "Key binding for the forge-debug-server function")

(defcustom forge-gradle-bin
  "echo ./gradlew"
  "The gradle wrapper script in the main project directory")

(defcustom forge-jdb-bin
  "echo jdb"
  "The java debugger binary")

(defcustom forge-jdb-address
  "5005"
  "The address to which jdb should attach, this is set by gradle when running in debug mode")

(defvar current-gradle-w
  nil
  "The gradle window")

(defvar current-jdb-w
  nil
  "The jdb window")

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

(defun exec-gradle-task (command)
  "Execute a gradle task in the forge gradle temporary buffer"
  (let ((process-handle "forge-gradle")
	(gradle-command (concat forge-gradle-bin " " command)))
    (start-process-shell-command process-handle forge-gradle-buffer-name gradle-command)))

(defun exec-debugger-task ()
  "Execute jdb and attach to the address initialised by gradle"
  (let ((process-handle "forge-jdb")
	(jdb-command (concat forge-jdb-bin " attach " forge-jdb-address)))
    (start-process-shell-command process-handle forge-debugger-buffer-name jdb-command)))

(defun forge-run-client ()
  "Run minecraft client and load the mod"
  (interactive)
  (progn
    (get-buffer-create forge-gradle-buffer-name)
    (forge-enable-gradle forge-gradle-buffer-name)
    (exec-gradle-task "runClient")))

(defun forge-debug-client ()
  "Run the minecraft client in debug mode and attach the debugger in a separate comint buffer"
  (interactive)
  (progn
    (forge-init-buffers)
    (exec-gradle-task "runClient --debug-jvm")
    (exec-debugger-task)))
  

(defun forge-run-srv ()
  nil)

(defun forge-debug-srv ()
  nil)


;; define a comint mode to control the debugger input



(define-minor-mode forge-mode
  "A minor mode that helps developing minecraft mods using MinecraftForge and MCP"
  :lighter "forge-mode")

(provide 'forge-mode)
