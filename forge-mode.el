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
  "echo";"./gradlew"
  "The gradle wrapper script in the main project directory")

(defcustom forge-jdb-bin
  "echo";"jdb"
  "The java debugger binary")

(defcustom forge-jdb-address
  "5005"
  "The address to which jdb should attach, this is set by gradle when running in debug mode")

(defvar current-gradle-process
  nil
  "The gradle command currently runnning, this avoids changing things while a task is running")

(defvar current-debugger-instance
  nil
  "The current debugger process instance")

(defun debug ()
  (interactive)
  (let* ((top-window (frame-root-window))
	 (split-one-third (* 2 (/ (window-body-height top-window t) 3)))
	 (split-half (/ (window-body-width top-window) 2))
	 (w1 (split-window top-window split-one-third 'below))
	 (w2 (split-window w1 split-half 'right))
	 (b1 (get-buffer-create "buffer a"))
	 (b2 (get-buffer-create "buffer b")))
    (progn
      (set-window-buffer w1 b1)
      (set-window-buffer w2 b2))))

(defun forge-create-window ()
  "Create a new window by splitting the root window leaving 2/3 of the frame to the root,
if a forge buffer is already visible split it and use half for the new window"
  (let* ((top-window (frame-root-window))
	 (split-one-third (* 2 (/ (window-body-height top-window t) 3)))
	 (debugger-win (get-buffer-window forge-debugger-buffer-name))
	 (gradle-win (get-buffer-window forge-gradle-buffer-name)))
    (if (null debugger-win)
	(split-window top-window split-one-third 'below)
      (let ((split-half (/ (window-body-width debugger-win) 2)))
	(split-window debugger-win split-half 'right)))
    (let ((split-half (/ (window-body-width gradle-win) 2)))
      (split-window gradle-win split-half 'right))))

(defun forge-display-buffer (buffer-name)
  "Display a buffer in a window if not window shows it"
  (let ((buffer-in-window (get-buffer-window buffer-name)))
    (if (null buffer-in-window)
	(set-window-buffer (forge-create-window) buffer-name)
      nil)))

(defun exec-gradle-task (command)
  "Execute a gradle task in the forge gradle temporary buffer"
  (let ((process-handle "forge-gradle")
	(gradle-command (concat forge-gradle-bin " " command)))
    (start-process-shell-command process-handle forge-gradle-buffer-name gradle-command)
    (forge-display-buffer forge-gradle-buffer-name)))

(defun exec-debugger-task ()
  "Execute jdb and attach to the address initialised by gradle"
  (let ((process-handle "forge-jdb")
	(jdb-command (concat forge-jdb-bin " attach " forge-jdb-address)))
    (start-process-shell-command process-handle forge-debugger-buffer-name jdb-command)
    (forge-display-buffer forge-debugger-buffer-name)))

(defun forge-run-client ()
  "Run minecraft client and load the mod"
  (interactive)
  (progn (exec-gradle-task "runClient")
	 (exec-debugger-task)))

(defun forge-debug ()
  "Run the minecraft client in debug mode and attach the debugger in a separate comint buffer"
  (interactive)
  (exec-gradle-task "debugClient"))

(defun forge-run-srv ()
  nil)

(defun forge-debug-srv ()
  nil)


(define-minor-mode forge-mode
  "A minor mode that helps developing minecraft mods using MinecraftForge and MCP"
  :lighter "forge-mode")

(provide 'forge-mode)
