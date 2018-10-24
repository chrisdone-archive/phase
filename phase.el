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
(require 'subr-x)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defcustom phase-port
  4006
  "Port to listen on."
  :group 'intero
  :type 'number)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(progn
  (add-hook 'window-configuration-change-hook 'phase-window-configuration-change)
  (add-hook 'post-command-hook 'phase-post-command)
  (add-hook 'after-change-functions 'phase-after-change)
  (add-hook 'kill-buffer-hook 'phase-kill-buffer))

;; TODO:
;;
;; Use the below redisplay hook to queue events up before an
;; (inevitable) redisplay. For example, every time a change occurs on
;; a visible buffer, you could queue that up because there will
;; inevitably be a redisplay at the end, but you can double buffer and
;; avoid seeing the drawing in action and just send the finished
;; result in one go.
;;
;; — Variable: pre-redisplay-functions
;;
;; This hook is run just before redisplay. It is called once in each
;; window that is about to be redisplayed, with current-buffer set to
;; the buffer displayed in that window.

(defun phase-window-configuration-change ()
  (when (phase-listening-p)
    (phase-broadcast 'phase-send-window-configuration)))

(defun phase-post-command ()
  (when (and (buffer-live-p (current-buffer))
             (phase-buffer-visible-p (current-buffer)))
    (add-hook 'jit-lock-functions 'phase-jit-lock-function))
  (when (phase-listening-p)
    (phase-broadcast 'phase-send-post-command-updates)))

(defun phase-after-change (beg end old-length)
  (when (and (buffer-live-p (current-buffer))
             (phase-buffer-visible-p (current-buffer)))
    (when (phase-listening-p)
      (phase-broadcast 'phase-send-change
                       (buffer-substring beg end)
                       beg
                       (+ beg old-length)))))

(defun phase-jit-lock-function (beg end)
  (when (and (buffer-live-p (current-buffer))
             (phase-buffer-visible-p (current-buffer)))
    (when (phase-listening-p)
      (phase-broadcast 'phase-send-jit-lock beg (buffer-substring beg end)))))

(defun phase-kill-buffer ()
  (when (phase-buffer-visible-p (current-buffer))
    (let (kill-buffer-hook)
      (when (phase-listening-p)
        (phase-broadcast 'phase-send-buffer-killed)))))

(defun phase-buffer-visible-p (buffer)
  (let ((name (buffer-name buffer)))
    (and (> (length name) 0)
         (not (string= " " (substring name 0 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals

(defvar phase-listener
  nil
  "A TCP listener process.")

(defvar phase-clients
  (make-hash-table :test 'equal)
  "List of connected clients.")

(defvar phase-cursor-color ""
  "Current cursor color.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun phase-listen ()
  "Start listening for connections on port `phase-port'."
  (interactive)
  (if (phase-listening-p)
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

(defun phase-listening-p ()
  (and (processp phase-listener)
       (process-live-p phase-listener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection setup

(defun phase-on-open (websocket)
  (puthash (websocket-accept-string websocket) websocket phase-clients)
  (phase-send-window-configuration websocket)
  (phase-send-cursor-color websocket))

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
  (remhash (websocket-accept-string websocket) phase-clients))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incoming

(defun phase-on-message (ws frame)
  (let* ((event (json-read-from-string (websocket-frame-text frame)))
         (tag (cdr (assoc 'tag event))))
    (cond
     ((string= tag "get-buffers")
      (let ((names (cdr (assoc 'names event))))
        (websocket-send-text
         ws
         (phase-json-object
          (list
           (phase-json-pair "tag" (phase-json-string "setBuffers"))
           (phase-json-pair "buffers"
                            (phase-json-array
                             (mapcar (lambda (name)
                                       (phase-json-object
                                        (let ((string
                                               (with-current-buffer (get-buffer name)
                                                 (buffer-string))))
                                          (list (phase-json-pair "name" (phase-json-string name))
                                                (phase-json-pair "string" (phase-json-string string))
                                                (phase-json-pair "properties"
                                                                 (json-encode (phase-string-properties 0 string)))))))
                                     names))))))))
     ((string= tag "get-faces")
      (let ((names (cdr (assoc 'names event))))
        (websocket-send-text
         ws
         (phase-json-object
          (list
           (phase-json-pair "tag" (phase-json-string "setFaces"))
           (phase-json-pair
            "faces"
            (phase-json-object
             (mapcar
              (lambda (name)
                (phase-json-pair
                 name
                 (phase-json-object
                  (list (phase-json-pair
                         "font-family"
                         (phase-json-string
                          (plist-get (font-face-attributes (face-attribute (intern name) :font nil 'default))
                                     :family)))
                        (phase-json-pair
                         "font-size"
                         (phase-json-string
                          (concat (number-to-string
                                   (/ (face-attribute (intern name) :height nil 'default) 10))
                                  "pt")))
                        (phase-json-pair
                         "font-weight"
                         (phase-json-string
                          (cl-case (face-attribute (intern name) :weight nil 'default)
                            (ultra-bold "bold")
                            (extra-bold "bold")
                            (bold "bold")
                            (semi-bold "bold")
                            (normal "normal")
                            (semi-light "normal")
                            (light "normal")
                            (extra-light "normal")
                            (ultra-light "normal"))))
                        (phase-json-pair
                         "color"
                         (phase-json-string (face-attribute (intern name) :foreground nil 'default)))
                        (phase-json-pair
                         "background-color"
                         (phase-json-string (face-attribute (intern name) :background nil 'default)))))))
              names)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outgoing

(defun phase-broadcast (func &rest args)
  (mapc (lambda (client)
          (apply func (cons client args)))
        (hash-table-values phase-clients)))

(defun phase-send-buffer-killed (websocket)
  (websocket-send-text
   websocket
   (phase-json-object
    (list
     (phase-json-pair "tag" (phase-json-string "killBuffer"))
     (phase-json-pair "buffer" (phase-json-string (buffer-name)))))))

(defun phase-send-window-configuration (websocket)
  (websocket-send-text
   websocket
   (phase-json-object
    (list
     (phase-json-pair "tag" (phase-json-string "setWindowConfiguration"))
     (phase-json-pair "tree" (phase-json-window-tree (car (window-tree))))
     (phase-json-pair "selected" (phase-json-string (phase-window-key (selected-window))))
     (phase-json-pair "minibuffer" (phase-json-window (cadr (window-tree))))))))

(defun phase-send-post-command-updates (websocket)
  (when (not (string= phase-cursor-color
                      (frame-parameter (selected-frame) 'cursor-color)))
    (phase-send-cursor-color websocket))
  (websocket-send-text
   websocket
   (phase-json-object
    (list
     (phase-json-pair "tag" (phase-json-string "setWindowPoints"))
     (phase-json-pair
      "windows"
      (phase-json-object
       (mapcar (lambda (window)
                 (phase-json-pair (phase-window-key window)
                                  (phase-json-window-points window)))
               (window-list))))))))

(defun phase-send-cursor-color (websocket)
  (progn
    (setq phase-cursor-color (frame-parameter (selected-frame) 'cursor-color))
    (websocket-send-text
     websocket
     (phase-json-object
      (list
       (phase-json-pair "tag" (phase-json-string "setCursorColor"))
       (phase-json-pair "color" (phase-json-string phase-cursor-color)))))))

(defun phase-send-change (websocket replacement beg end)
  (websocket-send-text
   websocket
   (phase-json-object
    (list
     (phase-json-pair "tag" (phase-json-string "replaceRange"))
     (phase-json-pair "buffer" (phase-json-string (buffer-name)))
     (phase-json-pair "replacement" (phase-json-string replacement))
     (phase-json-pair "properties"
                      (json-encode (phase-string-properties (1- beg) replacement)))
     (phase-json-pair "from" (phase-json-number beg))
     (phase-json-pair "to" (phase-json-number end))))))

(defun phase-send-jit-lock (websocket beg replacement)
  (websocket-send-text
   websocket
   (phase-json-object
    (list
     (phase-json-pair "tag" (phase-json-string "jitLock"))
     (phase-json-pair "buffer" (phase-json-string (buffer-name)))
     (phase-json-pair "properties"
                      (json-encode (phase-string-properties (1- beg) replacement)))))))

(defun phase-json-window-points (window)
  (phase-json-object
   (with-current-buffer (window-buffer window)
     (list (phase-json-pair
            "start"
            (phase-json-number
             (window-start window)))
           (phase-json-pair
            "line"
            (phase-json-number
             (- (line-number-at-pos (window-point window))
                1)))
           (phase-json-pair
            "ch"
            (phase-json-number
             (with-current-buffer (window-buffer window)
               (save-excursion
                 (goto-char (window-point window))
                 (current-column)))))))))

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

(defun phase-json-window-tree (tree)
  "Window TREE to json."
  (if (windowp tree)
      (phase-json-window tree)
    (phase-json-split tree)))

(defun phase-json-split (split)
  "SPLIT to json."
  (phase-json-object
   (list
    (phase-json-pair "tag" (phase-json-string "split"))
    (phase-json-pair "vertical" (phase-json-bool (car split)))
    (phase-json-pair "edges" (phase-json-array (mapcar 'phase-json-number (cadr split))))
    (phase-json-pair "windows"
                     (phase-json-array
                      (mapcar 'phase-json-window-tree (cddr split)))))))

(defun phase-json-window (window)
  "WINDOW to json."
  (phase-json-object
   (list
    (phase-json-pair "tag" (phase-json-string "window"))
    (phase-json-pair "key" (phase-json-string (phase-window-key window)))
    (phase-json-pair "point" (phase-json-window-points window))
    (phase-json-pair "start" (phase-json-number (window-start window)))
    (phase-json-pair "width" (phase-json-number (window-body-width window t)))
    (phase-json-pair "buffer" (phase-json-string (buffer-name (window-buffer window))))
    (phase-json-pair "height" (phase-json-number (window-body-height window t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text properties & faces

(defun phase-string-properties (base string)
  "Return properties of STRING as a list."
  (cl-loop
   with point = 0
   with point-max = (length string)
   while (not (>= point point-max))
   for next-change = (or (next-property-change point string)
                         point-max)
   for face = (get-text-property point 'face string)
   collect (+ base point)
   collect (+ base next-change)
   collect (or face 'default)
   do (setq point next-change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'phase)

;;; phase.el ends here
