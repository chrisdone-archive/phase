;;; phase.el --- Phase. Web-browser front-end to Emacs

;; Copyright (c) 2018 Chris Done. All rights reserved

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'websocket)
(require 'json)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defcustom phase-port
  4006
  "Port to listen on."
  :group 'intero
  :type 'number)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals

(defvar phase-listener
  nil
  "A TCP listener process.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun phase-listen ()
  "Start listening for connections on port `phase-port'."
  (interactive)
  (if (and (processp phase-listener)
           (process-live-p phase-listener))
      (message "Phase is already listening.")
    (progn (setq phase-listener (phase-make-listener))
           (message "Phase is listening ..."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listener setup

(defun phase-make-listener ()
  "Make the listener process on port `phase-port'."
  (websocket-server
   phase-port
   :host 'local
   :on-message 'phase-on-message
   :on-open 'phase-on-open
   :on-close 'phase-on-close))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection setup

(defun phase-on-open (websocket)
  (message "Client connection opened!")
  (phase-send-window-configuration websocket))

(defun phase-send-window-configuration (websocket)
  (websocket-send-text
   websocket
   (phase-json-object
    (list
     (phase-json-pair "tag" (phase-json-string "setWindowConfiguration"))
     (phase-json-pair "tree" (phase-json-window-tree (car (window-tree))))))))

(defun phase-json-window-tree (tree)
  "Window TREE to json."
  (if (windowp tree)
      (phase-json-window tree)
    (phase-json-split tree)))

(defun phase-json-split (split)
  "SPLIT to json."
  (phase-json-object
   (list
    (phase-json-pair "vertical" (phase-json-bool (car split)))
    (phase-json-pair "edges" (phase-json-array (mapcar 'phase-json-number (cadr split))))
    (phase-json-pair "windows"
                     (phase-json-array
                      (mapcar 'phase-json-window-tree (cddr split)))))))

(defun phase-json-window (window)
  "WINDOW to json."
  (phase-json-object
   (list (phase-json-pair "key" (phase-json-string (phase-window-key window)))
         (phase-json-pair "width" (phase-json-number (window-body-width window t)))
         (phase-json-pair "height" (phase-json-number (window-body-height window t))))))

(defun phase-window-key (window)
  "Return a unique string name for WINDOW."
  (if (windowp window)
      (replace-regexp-in-string
       "#<window \\([0-9]+\\).*"
       "\\1"
       (prin1-to-string window))
    (error "Not a window: %S" window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection teardown

(defun phase-on-close (websocket)
  (message "Client connection closed."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incoming

(defun phase-on-message (ws frame)
  (message "Server received frame: %S" frame)
  (websocket-send-text
   ws
   (websocket-frame-text frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outgoing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON encoding

(defun phase-json-array (pairs)
  (concat "["
          (with-temp-buffer
            (cl-loop with len = (length pairs)
                     for pair in pairs
                     and i from 1
                     do (insert pair (if (= i len) "" ",")))
            (buffer-string))
          "]"))

(defun phase-json-object (pairs)
  (concat "{"
          (with-temp-buffer
            (cl-loop with len = (length pairs)
                     for pair in pairs
                     and i from 1
                     do (insert pair (if (= i len) "" ",")))
            (buffer-string))
          "}"))

(defun phase-json-pair (key value)
  (concat (json-encode-string key) ":" value))

(defun phase-json-string (string)
  (json-encode-string string))

(defun phase-json-number (number)
  (json-encode-number number))

(defun phase-json-bool (bool)
  (if bool "true" "false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'phase)

;;; phase.el ends here
