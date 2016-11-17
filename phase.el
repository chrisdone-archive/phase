;;; phase.el --- Phase

;; Copyright (c) 2016 Chris Done. All rights reserved.

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

;; Acts as a server for web clients.

;;; Code:

(require 'websocket)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Variables

(defvar phase-server nil
  "Current listening server.")

(defvar phase-port 2017
  "Websocket port to listen on.")

(defvar phase-clients (list)
  "List of connected clients.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables

(defvar-local phase-point
  -1
  "Current point in the buffer.")

(defvar-local phase-change-lines
  nil
  "Record the lines about to change.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun phase-start ()
  "Start listening for connections."
  (interactive)
  (if phase-server
      (error "Already listening on port %d" phase-port)
    (setq phase-server
          (websocket-server
           phase-port
           :host 'local
           :on-message 'phase-on-message
           :on-open 'phase-on-open
           :on-close 'phase-on-close))))

(defun phase-enable-buffer (buffer)
  "Enable phasing in BUFFER."
  (interactive "bBuffer to phase: ")
  (with-current-buffer buffer
    (add-hook 'post-command-hook 'phase-post-command-function t t)
    (add-hook 'before-change-functions 'phase-before-change-function nil t)
    (add-hook 'after-change-functions 'phase-after-change-function nil t)
    (message "Phasing buffer %s." buffer)))

(defun phase-disable-buffer (buffer)
  "Disable phasing in BUFFER."
  (interactive "bBuffer to disable phasing: ")
  (with-current-buffer buffer
    (remove-hook 'post-command-hook 'phase-post-command-function)
    (remove-hook 'before-change-functions 'phase-before-change-function)
    (remove-hook 'after-change-functions 'phase-after-change-function)
    (message "No longer phasing buffer %s." buffer)))

(defun phase-refresh-buffer ()
  "Refresh the current buffer."
  (interactive)
  (dolist (connection phase-clients)
    (phase-send-buffer-contents connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection handlers

(defun phase-on-open (connection)
  "Callback on a new client CONNECTION."
  (message "Phase: New client connected.")
  (add-to-list 'phase-clients connection)
  (phase-send-faces connection)
  (when (member 'phase-post-command-function post-command-hook)
    (phase-send-buffer-contents connection)))

(defun phase-on-close (connection)
  "Callback when a CONNECTION is closed."
  (delete connection phase-clients))

(defun phase-on-message (connection message)
  "Callback when a CONNECTION receives a MESSAGE.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer update hooks

(defun phase-post-command-function ()
  "Handle point movement."
  (when (not (= phase-point (point)))
    (setq phase-point (point))
    (dolist (connection phase-clients)
      (phase-send-point connection phase-point))))

(defun phase-before-change-function (beg end)
  "Handle change about to occur between BEG and END."
  (let ((start-line (save-excursion (goto-char beg) (line-number-at-pos)))
        (end-line (save-excursion (goto-char end) (line-number-at-pos))))
    (setq phase-change-lines (cons start-line end-line))))

(defun phase-after-change-function (beg end changed)
  "Handle change occurred between BEG and END, ignoring CHANGED."
  (let ((before-start-line (car phase-change-lines))
        (before-end-line (cdr phase-change-lines))
        (after-start-line (save-excursion (goto-char beg) (line-number-at-pos)))
        (after-end-line (save-excursion (goto-char end) (line-number-at-pos))))
    (dolist (connection phase-clients)
      (phase-send-region
       connection
       (current-buffer)
       before-start-line
       before-end-line
       after-start-line
       after-end-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transmission functions

(defun phase-send-buffer-contents (connection)
  "Send the current buffer contents to CONNECTION."
  (let ((end-line
         (save-excursion (goto-char (point-max))
                         (line-number-at-pos))))
    (phase-send-region
     connection
     (current-buffer)
     0
     end-line 0 end-line)))

(defun phase-send-region (connection buffer before-start-line before-end-line after-start-line after-end-line)
  "Send on CONNECTION what changed in BUFFER.
Result is to remove the lines between BEFORE-START-LINE and
BEFORE-END-LINE, and insert the contents between AFTER-START-LINE
and AFTER-END-LINE."
  (websocket-send-text
   connection
   (concat
    "{"
    "\"type\": \"region\","
    "\"beg\":" (number-to-string before-start-line) ","
    "\"end\":" (number-to-string before-end-line) ","
    "\"lines\": " (phase-region-json buffer after-start-line after-end-line)
    "}")))

(defun phase-send-point (connection point)
  "On CONNECTION send the current POINT."
  (websocket-send-text
   connection
   (concat
    "{"
    "\"type\": \"point\","
    "\"line\": " (number-to-string (line-number-at-pos)) ","
    "\"column\": " (number-to-string (current-column))
    "}")))

(defun phase-send-faces (connection)
  "On CONNECTION send a complete list of faces."
  (let ((output
         (concat
          "{"
          "\"type\": \"faces\", \"faces\": "
          (json-encode-array
           (apply #'append ;; anywqay
                  (mapcar (lambda (face)
                            (list face
                                  (face-foreground face nil 'default)))
                          (face-list))))
          "}")))
    (websocket-send-text connection output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer data functions

(defun phase-region-json (buffer start-line end-line)
  "Generate JSON in BUFFER for the lines between START-LINE and END-LINE."
  (with-current-buffer buffer
    (json-encode-array
     (mapcar (lambda (line)
               (list :text line
                     :faces (phase-string-faces line)
                     ))
             (phase-buffer-lines start-line end-line)))))

(defun phase-buffer-lines (start-line end-line)
  "Get the lines between START-LINE and END-LINE in the current buffer."
  (split-string
   (buffer-substring
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start-line))
      (line-beginning-position))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- end-line))
      (line-end-position)))
   "\n"))

(defun phase-string-faces (object)
  "Collect a list of faces that apply to OBJECT.

TOOD: optimize."
  (let ((point 0)
        (point-max (length object))
        (faces (list)))
    (while (not (>= point point-max))
      (let ((plist (text-properties-at point object))
            (next-change
             (or (next-property-change point object)
                 point-max)))
        (push point faces)
        (push next-change faces) ;; hello world!
        (push (get-text-property point 'face object) faces)
        (setq point next-change)))
    (reverse faces)))

(provide 'phase)

;;; phase.el ends here
