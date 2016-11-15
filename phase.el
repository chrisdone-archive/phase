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

;;; Code:

(require 'websocket)

(setq websocket-debug t)

(defvar phase-socket
  (websocket-server
   2016
   :host 'local
   :on-message 'phase-on-message
   :on-open 'phase-on-open
   :on-close 'phase-on-close))

(defvar phase-client nil)

(defvar phase-buffer (get-buffer "phase.el"))

(defun phase-on-open (socket)
  (setq phase-client socket)
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
    (websocket-send-text socket output)))

(defun phase-on-close (socket)
  )

(defun phase-on-message (socket frame)
  ;; (message "Received: %S" (websocket-frame-text frame))
  (with-current-buffer phase-buffer
    (let ((end-line
           (save-excursion (goto-char (point-max))
                           (line-number-at-pos))))
      (phase-send-region
       socket
       (current-buffer)
       0
       end-line 0 end-line))))

(defvar-local phase-change-lines nil)
(add-hook 'before-change-functions 'phase-before-change-function) ;; TODO: just local atm
(defun phase-before-change-function (beg end)
  (when (eq (current-buffer) phase-buffer)
    (let ((start-line (save-excursion (goto-char beg) (line-number-at-pos)))
          (end-line (save-excursion (goto-char end) (line-number-at-pos))))
      (setq phase-change-lines (cons start-line end-line)))))

(add-hook 'after-change-functions 'phase-after-change-function) ;; TODO: just local atm
(defun phase-after-change-function (beg end _)
  (when (eq (current-buffer) phase-buffer)
    (let ((before-start-line (car phase-change-lines))
          (before-end-line (cdr phase-change-lines))
          (after-start-line (save-excursion (goto-char beg) (line-number-at-pos)))
          (after-end-line (save-excursion (goto-char end) (line-number-at-pos))))
      ;; (message "Before lines: %d-%d" before-start-line before-end-line)
      ;; (message "After lines: %d-%d" after-start-line after-end-line)
      (phase-send-region
       phase-client
       (current-buffer)
       before-start-line
       before-end-line
       after-start-line
       after-end-line))))

(defun phase-send-region (socket
                          buffer
                          before-start-line
                          before-end-line
                          after-start-line
                          after-end-line)
  (websocket-send-text
   socket
   (concat
    "{"
    "\"type\": \"region\","
    "\"beg\":" (number-to-string before-start-line) ","
    "\"end\":" (number-to-string before-end-line) ","
    "\"lines\": " (phase-region-json buffer after-start-line after-end-line)
    "}")))

(defun phase-region-json (buffer start-line end-line)
  (with-current-buffer buffer
    (json-encode-array
     (mapcar (lambda (line)
               (list :text line
                     :faces (phase-string-faces line)
                     ))
             (phase-buffer-lines start-line end-line)))))

(defun phase-buffer-lines (start-line end-line)
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
