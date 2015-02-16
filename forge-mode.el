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

(defcustom forge-debugger-buffer-name
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
  "gradlew"
  "The gradle wrapper script in the main project directory")

(defcustom forge-gradle-path
  "./"
  "The path where the gradle wrapper build script resides")

(defvar current-gradle-process
  nil
  "The gradle command currently runnning, this avoids changing things while a task is running")

(defvar current-debugger-instance
  nil
  "The current debugger process instance")

(defun forge-create-window ()
  "Create a new window by splitting the root window leaving 2/3 of the frame to the root"
  (let* ((top-window (frame-root-window))
	 (split-one-third (* 2 (/ (window-body-height top-window t) 3))))
    (split-window top-window split-one-third 'below)))

(defun forge-display-buffer (buffer-name)
  "Display a buffer in a window if not window shows it"
  (let ((buffer-in-window (get-buffer-window buffer-name)))
    (if (null buffer-in-window)
	(set-window-buffer (forge-create-window) buffer-name)
      nil)))

(defun exec-gradle-task (command)
  "Execute a command in the forge gradle temporary buffer"
  (let ((process-handle "forge-gradle"),
	(gradle-command (concat forge-gradle-bin " " command)))
    (start-process-shell-command process-handle forge-gradle-buffer-name gradle-command)
    (forge-display-buffer forge-gradle-buffer-name)))

(defun forge-run-client ()
  "Run minecraft client and load the mod"
  (interactive)
  (exec-gradle-task "ls"))


(defun forge-debug ()
  nil)

(defun forge-run-srv ()
  nil)

(defun forge-debug-srv ()
  nil)


(define-minor-mode forge-mode
  "A minor mode that helps developing minecraft mods using MinecraftForge and MCP"
  :lighter "forge-mode")

(provide 'forge-mode)
