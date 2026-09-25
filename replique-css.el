;;; replique-css.el --- Stylesheets, in the page  -*- lexical-binding: t; -*-

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

;; Seeing a stylesheet you have just edited, without reloading the page.
;;
;; WHAT IS HERE IS THE KEYSTROKE AND NOTHING ELSE.  The process holds the
;; connection to the pages, and the page itself decides which of its
;; stylesheets the file being edited is - see the `:reload-css' op in
;; doc/protocol.md.  That is where the work is, and it is there rather than
;; here for a reason worth writing down: an editor knows a path on this
;; machine, a page knows URLs, and NOTHING ON EITHER SIDE KNOWS BOTH.  Where
;; a project serves its assets from is the project's own arrangement.  So the
;; two are brought together by the longest path suffix they share, in the
;; page, which needs neither half to have been told about the other.
;;
;; WHAT REPLIQUE 1 ASKED AND THIS DOES NOT.  It listed the page's stylesheets
;; in one round trip, filtered them by basename here, and asked you which one
;; when more than one matched - then reloaded the one you chose in a second
;; round trip.  The list now comes back inside every answer, matched or not,
;; which is both the second round trip and the prompt gone: a reload that
;; found nothing says what the page HAS, where replique 1 said "Could not
;; find a css file to reload" and left you to guess.
;;
;; TURN THE MODE ON WHERE YOU WANT THE KEY, which is what replique 1 wanted
;; too:
;;
;;   (add-hook 'css-mode-hook #'replique-css-mode)
;;
;; `scss-mode' and `less-css-mode' are derived from `css-mode', so that hook
;; is all of them.  Nothing is added to it from here: a .css file is a file
;; like any other and most of them are opened in projects that have never
;; heard of replique, where a mode that installed itself would be a keymap
;; nobody asked for.

;;; Code:

(require 'comint)
(require 'subr-x)
(require 'replique-name)
(require 'replique-process)

(defvar replique-css-mode-map
  (let ((map (make-sparse-keymap)))
    ;; The same key a Clojure buffer loads with, and the same sentence: put
    ;; what is in this buffer into the process.  `replique-load-file' is not
    ;; it - what that sends is a form for a repl to read, and a stylesheet is
    ;; not code this process runs
    (define-key map (kbd "C-c C-l") #'replique-reload-css)
    map)
  "Keymap of `replique-css-mode'.")

;;;###autoload
(define-minor-mode replique-css-mode
  "Reload this buffer\\='s stylesheet in the pages a replique process has.

Turned on where you want the key - see the commentary.  Nothing here
needs a process to be running: the command says so when it is used.

\\{replique-css-mode-map}"
  :lighter " replique-css"
  :keymap replique-css-mode-map)

(defun replique-css--report (file frame)
  "Say what the process answered about reloading FILE.

FRAME is the reply.  Four answers and not two, because \"nothing
happened\" has three different reasons and they are not the same thing to
whoever pressed the key: the process could not be asked, the page could
not be asked, the page was asked and holds nothing like this file, or the
page holds no stylesheets at all.

A SENTENCE THE PROCESS WROTE IS SHOWN AS IT WAS WRITTEN.  Where there is
no page open, that sentence names the URL to open - which is the whole of
what somebody needs and is not something this could word better."
  (cond
   ((equal "error" (plist-get frame :tag))
    (message "replique: %s" (plist-get frame :message)))
   ((plist-get frame :note)
    (message "replique: %s" (plist-get frame :note)))
   ((plist-get frame :reloaded)
    (message "replique: reloaded %s"
             (string-join (plist-get frame :reloaded) ", ")))
   ((plist-get frame :stylesheets)
    ;; What the page has, which is the answer to the question somebody is
    ;; about to ask.  Replique 1 had this list in its hand at this exact
    ;; moment and threw it away
    (message "replique: nothing on the page matches %s - it has %s"
             (file-name-nondirectory file)
             (string-join (plist-get frame :stylesheets) ", ")))
   (t (message "replique: the page has no stylesheets"))))

;;;###autoload
(defun replique-reload-css (&optional file process)
  "Reload the stylesheet FILE in every page connected to PROCESS.

FILE is this buffer\\='s file when it is not given, and PROCESS is the one
the commands act on by default.

EVERY PAGE, and not the page: you have the application open and the tab
you were comparing it against, and a stylesheet that reloaded in one of
them is a stylesheet that did not reload.

WHAT IS RELOADED IS THE FILE ON THE DISK - the page fetches it from
whatever serves the application\\='s assets - so a buffer with unsaved
changes is offered to be saved first, which is `replique-load-file\\='s
answer to the same question and is asked in the same words.

NOTHING WAITS FOR THE ANSWER.  Asking starts the process\\='s browser
runtime where it is not up, which is seconds the first time, and what
happened is said when it is known.

The page is not reloaded and nothing in it is lost: a fresh <link> is put
in beside the old one and the old one goes when the new one has loaded.
A stylesheet that 404s or no longer parses therefore costs nothing - the
one that was working is still there - which is the case worth having,
because what you were doing when it happened was editing that file."
  (interactive
   (let ((file (or (buffer-file-name)
                   (user-error "This buffer holds no stylesheet to reload"))))
     ;; Before anything is sent, because saving is what puts the change where
     ;; the page can fetch it
     (comint-check-source file)
     (list file)))
  (let ((file (expand-file-name
               (or file (buffer-file-name)
                   (user-error "This buffer holds no stylesheet to reload"))))
        (process (or process (replique-name-process) (replique-process-ensure))))
    (replique-process-request
     process (list :op :reload-css :file file)
     (lambda (frame) (replique-css--report file frame)))))

(provide 'replique-css)

;;; replique-css.el ends here
