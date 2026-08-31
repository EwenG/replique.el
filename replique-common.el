;;; replique-common.el --- What the rest of replique shares  -*- lexical-binding: t; -*-

;; Copyright © 2016 Ewen Grosjean

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The customization group and the faces.  They live in a file of their own
;; because a defcustom needs its group to exist by the time it is read, and
;; the two buffers that render what a process says - the repl and the process
;; output - are at different levels of the require chain.  Putting the group
;; in whichever of them happens to load first would make its home look like
;; an accident.
;;
;; Everything a face is used for here is text a process produced.  What
;; replique says about a process rather than what the process said is the one
;; exception, and it is deliberately quiet.
;;
;; The other thing they share is what to do about output that arrived in a
;; buffer no window shows.  The echo area is the wrong place to say so: what
;; arrives on its own arrives while nobody is looking, and a message is gone
;; by the time anybody does.  It is said in the global mode line instead,
;; where it waits to be read.

;;; Code:

(require 'comint)
(require 'seq)

(defgroup replique nil
  "A development environment for Clojure."
  :group 'tools)

(defface replique-stderr
  '((t (:inherit error)))
  "Face for what a process prints on its error stream."
  :group 'replique)

(defface replique-note
  '((t (:inherit shadow :slant italic)))
  "Face for what replique says about a process rather than what it printed."
  :group 'replique)

(defface replique-prompt
  '((t (:inherit comint-highlight-prompt)))
  "Face for the prompt of a repl."
  :group 'replique)

(defface replique-exception
  '((t (:inherit error)))
  "Face for what a form threw."
  :group 'replique)

;;; Output nothing has seen

(defcustom replique-track-unread t
  "Whether the mode line names the buffers holding output nobody has seen.

What a repl answers to a form sent from a buffer is not that: its result
is put in the echo area where the form was sent from.  This is for what
arrives on its own - what a future printed, what a thread threw."
  :type 'boolean
  :group 'replique)

(defface replique-unread
  '((t (:inherit mode-line-emphasis)))
  "Face naming a buffer with unseen output, in the mode line.

Not `replique-unread\=' inheriting `shadow\=' like the rest of what replique
says about itself: `shadow\=' is a foreground picked to recede against the
background of a buffer, and a mode line has neither that background nor
that purpose.  `mode-line-emphasis\=' is what a theme defines for
something a mode line should be read for, so it is legible wherever the
mode line is."
  :group 'replique)

(defvar replique--unread '()
  "The buffers that received output while no window showed them.

In the order they did it in, so that the mode line reads as things
happened.")

(defun replique--unread-name (buffer)
  "Return the short name of BUFFER for the mode line.

The decoration a buffer name carries is what tells a buffer apart from a
file in a buffer list; in a mode line naming nothing else it is noise."
  (let* ((name (buffer-name buffer))
         (name (replace-regexp-in-string "\\`\\*\\|\\*\\(<[0-9]+>\\)?\\'" "" name)))
    (replace-regexp-in-string "\\`replique\\(: \\|-\\)" "" name)))

(defun replique--unread-entry (buffer)
  "Return the mode line entry for BUFFER."
  (propertize (replique--unread-name buffer)
              'face 'replique-unread
              'mouse-face 'mode-line-highlight
              'help-echo (format "%s: output nothing has seen\nmouse-1: show it"
                                 (buffer-name buffer))
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line mouse-1]
                             (lambda ()
                               (interactive)
                               (when (buffer-live-p buffer)
                                 (pop-to-buffer buffer))))
                           map)))

(defun replique-unread-mode-line ()
  "Return the global mode line description of output nothing has seen."
  (let ((buffers (seq-filter #'buffer-live-p replique--unread)))
    (if (null buffers)
        ""
      (concat " " (mapconcat #'replique--unread-entry buffers ",")))))

(defvar replique--unread-mode-line '(:eval (replique-unread-mode-line))
  "The `global-mode-string' element naming the buffers nobody has read.")

(defun replique--unread-install ()
  "Put the unread element in the global mode line, once.

Put there when there is something to say rather than when replique is
loaded: an editor that has not started a process has no reason to have
been changed."
  (or global-mode-string (setq global-mode-string '("")))
  (unless (member replique--unread-mode-line global-mode-string)
    (setq global-mode-string
          (append global-mode-string (list replique--unread-mode-line)))))

(defun replique-track-unread (buffer)
  "Note that BUFFER received output while no window showed it.

Noted once: what is being said is that there is something to read, and
saying it again per line of it says nothing more."
  (when (and replique-track-unread
             (buffer-live-p buffer)
             (not (get-buffer-window buffer 'visible))
             (not (memq buffer replique--unread)))
    (replique--unread-install)
    (setq replique--unread (append replique--unread (list buffer)))
    (force-mode-line-update t)))

(defun replique--unread-seen (&rest _)
  "Forget the buffers now on screen, and the ones that are gone.

Shown is read enough: what the mode line offers is a way to the buffer,
and it has been taken."
  (when replique--unread
    (let ((left (seq-filter (lambda (buffer)
                              (and (buffer-live-p buffer)
                                   (not (get-buffer-window buffer 'visible))))
                            replique--unread)))
      (unless (equal left replique--unread)
        (setq replique--unread left)
        (force-mode-line-update t)))))

;; Rather than a timer: a buffer becomes visible by being put in a window,
;; and this is run when one changes the buffer it shows
(add-hook 'window-buffer-change-functions #'replique--unread-seen)

;;; Buffers

(defun replique-insert-output (buffer string &optional face)
  "Insert STRING at the end of BUFFER, with FACE.

A window already at the end follows the output, and one further up is
left where the reader put it."
  (when (and (buffer-live-p buffer) string (not (string-empty-p string)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (windows (seq-filter (lambda (w) (= (window-point w) (point-max)))
                                 (get-buffer-window-list buffer nil t)))
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          ;; `face' rather than `font-lock-face': these buffers have no font
          ;; lock to honour the latter, so it would simply not be coloured
          (insert (if face (propertize string 'face face) string)))
        (when at-end (goto-char (point-max)))
        (dolist (w windows) (set-window-point w (point-max)))))
    (replique-track-unread buffer)))

(provide 'replique-common)

;;; replique-common.el ends here
