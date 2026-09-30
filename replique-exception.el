;;; replique-exception.el --- Browsing an exception  -*- lexical-binding: t; -*-

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

;; Every exception the protocol carries has the same shape - a class, a
;; message, printed ex-data, a trace, and a cause that has all of that again -
;; whether it came from a form at a repl, a thread that died, or a process
;; that would not start.  One buffer shows all of them.
;;
;; A chain is what makes an exception hard to read, so the chain is what this
;; navigates: one line per cause, and the trace of the one that is selected.
;; The root is marked, because the message a repl reports is the root's - not
;; the outermost one's - which is the single most confusing thing about
;; reading a Clojure exception as a tree.
;;
;; Nothing is hidden without saying so.  A frame carries the top of a trace
;; and the outermost causes, and what it left out is written where it was left
;; out.  The runtime frames can be folded away, and then the buffer says how
;; many are folded: a filtered trace must never be mistaken for a whole one.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'mule-util)
(require 'replique-common)

(defcustom replique-exception-runtime-frames
  '("\\`clojure\\.lang\\."
    "\\`clojure\\.main"
    "\\`clojure\\.core\\$"
    ;; the forked clojure's compiler recording what it resolved, around every
    ;; evaluation
    "\\`clojure\\.analysis\\$"
    "\\`java\\.base/"
    "\\`jdk\\.internal\\."
    "\\`replique\\.")
  "What counts as plumbing, as regexps matched against a trace line.

The machinery between a form and its evaluation: the compiler, the repl
that read it, the thread it ran on.  `java.base/' is the jdk, which is
how java prints it since it printed modules - so a frame of the jdk
matches this like any other does.

Which does not fold the one worth reading, because no rule here has to
spare it.  The throw path is kept whatever it is made of - see
`replique-exception--fold' - so `Integer.parseInt' survives where it
is what threw, and folds away further down where it is only something the
code went through.  Where a frame is says which of the two it is; what it
is called does not."
  :type '(repeat regexp)
  :group 'replique)

(defvar-local replique-exception--exception nil
  "The exception the buffer is showing.")

(defvar-local replique-exception--summary nil
  "What a terminal repl would have printed for it.")

(defvar-local replique-exception--phase nil
  "Which step of the repl it came out of.")

(defvar-local replique-exception--origin nil
  "Where the exception was caught, in a few words.")

(defvar-local replique-exception--index 0
  "Which cause of the chain is selected.")

(defvar-local replique-exception--folded nil
  "Whether the runtime frames are folded away.")

(defvar-local replique-exception--selection nil
  "Where the selected cause was written, to put point back on it.")

;;; Reading what a frame carries

(defun replique-exception-chain (exception)
  "Return EXCEPTION and its causes, outermost first."
  (let ((chain nil))
    (while exception
      (push exception chain)
      (setq exception (plist-get exception :cause)))
    (nreverse chain)))

(defun replique-exception--runtime-p (frame)
  "Return non-nil when FRAME is a frame of the runtime rather than of code."
  (seq-find (lambda (re) (string-match-p re frame))
            replique-exception-runtime-frames))

(defun replique-exception--fold (trace)
  "Return TRACE without its plumbing.

Nothing above the first frame of code is folded, whatever it is made of.
That prefix is the throw path - where the exception came from and what
called it - and for an exception the jdk threw it is the only part that
says anything: `Integer.parseInt' is the answer, `Thread.run' is not, and
no rule about package names tells them apart."
  (let ((path nil)
        (rest trace))
    (while (and rest (replique-exception--runtime-p (car rest)))
      (push (pop rest) path))
    (append (nreverse path)
            (when rest (list (car rest)))
            (seq-remove #'replique-exception--runtime-p (cdr rest)))))

(defun replique-exception--split (frame)
  "Return (METHOD . LOCATION) for FRAME, or (FRAME) when it is not that shape.

A trace line is what java prints for a stack frame.  A line that does not
look like one is shown as it came rather than forced into columns."
  (if (string-match "\\`\\(.*\\)(\\(.*\\))\\'" frame)
      (cons (match-string 1 frame) (match-string 2 frame))
    (list frame)))

;;; Rendering

(defun replique-exception--header (chain selected hidden)
  "Return the line describing CHAIN, its SELECTED cause and HIDDEN frames."
  (let* ((trace (plist-get selected :trace))
         (dropped (plist-get selected :trace-dropped))
         (parts (list (when replique-exception--phase
                        (format "phase %s" replique-exception--phase))
                      (if (cdr chain)
                          (format "%s causes%s" (length chain)
                                  (if (plist-get (car (last chain)) :cause-dropped)
                                      ", and the chain goes on" ""))
                        "one cause")
                      (if dropped
                          (format "%s of %s frames carried"
                                  (length trace) (+ (length trace) dropped))
                        (format "%s frames" (length trace)))
                      (when (and hidden (> hidden 0))
                        (format "%s runtime frames folded" hidden))
                      replique-exception--origin)))
    (string-join (delq nil parts) " · ")))

(defun replique-exception--insert-causes (chain)
  "Write CHAIN, one line per cause."
  (insert (propertize "Causes" 'face 'bold))
  (when (cdr chain)
    (insert (propertize " — the message above names the root" 'face 'replique-note)))
  (insert "\n")
  (let ((width (min 48 (apply #'max 0
                              (mapcar (lambda (c) (length (or (plist-get c :class) "?")))
                                      chain)))))
    (seq-do-indexed
     (lambda (cause index)
       (let* ((selected (= index replique-exception--index))
              (class (or (plist-get cause :class) "?")))
         (when selected (setq replique-exception--selection (point)))
         (insert (if selected (propertize "  ▸ " 'face 'bold) "    "))
         (insert (propertize (format "%d  " (1+ index)) 'face 'replique-note))
         (insert (propertize class
                             'face (if selected 'replique-exception 'default)))
         (insert (make-string (max 2 (- (+ width 2) (length class))) ?\s))
         (insert (truncate-string-to-width
                  (car (split-string (or (plist-get cause :message) "") "\n"))
                  60 nil nil t))
         ;; The root is the one the reported message names, and a chain that was
         ;; cut has its root below what the frame carried
         (when (and (= index (1- (length chain)))
                    (not (plist-get cause :cause-dropped)))
           (insert (propertize "  root" 'face 'replique-note)))
         (insert "\n")))
     chain))
  (when (plist-get (car (last chain)) :cause-dropped)
    (insert (propertize "       … the chain goes on below what the frame carried\n"
                        'face 'replique-note))))

(defun replique-exception--insert-trace (selected)
  "Write the trace of SELECTED."
  (let* ((trace (plist-get selected :trace))
         (shown (if replique-exception--folded
                    (replique-exception--fold trace)
                  trace))
         (folded (- (length trace) (length shown)))
         (split (mapcar #'replique-exception--split shown))
         (width (min 64 (apply #'max 0 (mapcar (lambda (p) (length (car p))) split)))))
    (insert "\n" (propertize (format "%s: %s\n"
                                     (or (plist-get selected :class) "?")
                                     (or (plist-get selected :message) ""))
                             'face 'bold))
    (when-let* ((data (plist-get selected :data)))
      (insert (propertize "data  " 'face 'replique-note) data "\n"))
    (insert "\n")
    (dolist (frame split)
      (insert "  " (car frame))
      (when (cdr frame)
        (insert (make-string (max 1 (- width (length (car frame)))) ?\s))
        (insert (propertize (cdr frame) 'face 'replique-note)))
      (insert "\n"))
    (when (> folded 0)
      (insert (propertize
               (format "  … %s runtime frames folded away, t shows them\n" folded)
               'face 'replique-note)))
    (when-let* ((dropped (plist-get selected :trace-dropped)))
      (insert (propertize (format "  … %s frames the frame did not carry\n" dropped)
                          'face 'replique-note)))))

(defun replique-exception--render ()
  "Write the exception of the current buffer."
  (let* ((inhibit-read-only t)
         (chain (replique-exception-chain replique-exception--exception))
         (selected (nth replique-exception--index chain))
         (trace (plist-get selected :trace))
         (folded (if replique-exception--folded
                     (- (length trace) (length (replique-exception--fold trace)))
                   0)))
    (erase-buffer)
    (setq replique-exception--selection nil)
    (when replique-exception--summary
      (insert (propertize replique-exception--summary 'face 'replique-exception) "\n"))
    (insert (propertize (replique-exception--header chain selected folded)
                        'face 'replique-note)
            "\n\n")
    (replique-exception--insert-causes chain)
    (replique-exception--insert-trace selected)
    (goto-char (or replique-exception--selection (point-min)))))

;;; The mode

(defvar replique-exception-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'replique-exception-next-cause)
    (define-key map (kbd "p") #'replique-exception-previous-cause)
    (define-key map (kbd "t") #'replique-exception-toggle-runtime-frames)
    (define-key map (kbd "w") #'replique-exception-copy)
    (define-key map (kbd "g") #'replique-exception-refresh)
    map)
  "Keymap of an exception buffer.")

(define-derived-mode replique-exception-mode special-mode "Replique-Exception"
  "Major mode for browsing one exception.

\\{replique-exception-mode-map}"
  (setq-local truncate-lines nil))

(defun replique-exception-refresh ()
  "Write the exception again."
  (interactive)
  (replique-exception--render))

(defun replique-exception--select (index)
  "Select the cause at INDEX."
  (let ((count (length (replique-exception-chain replique-exception--exception))))
    (setq replique-exception--index (max 0 (min index (1- count))))
    (replique-exception--render)))

(defun replique-exception-next-cause ()
  "Select the next cause, which is one step closer to the root."
  (interactive)
  (replique-exception--select (1+ replique-exception--index)))

(defun replique-exception-previous-cause ()
  "Select the previous cause."
  (interactive)
  (replique-exception--select (1- replique-exception--index)))

(defun replique-exception-toggle-runtime-frames ()
  "Fold the frames of the runtime away, or show them again."
  (interactive)
  (setq replique-exception--folded (not replique-exception--folded))
  (replique-exception--render))

(defun replique-exception-copy ()
  "Copy the text of the buffer."
  (interactive)
  (kill-new (buffer-substring-no-properties (point-min) (point-max)))
  (message "replique: the exception is in the kill ring"))

;;; Showing one

(defun replique-exception-show (exception &optional summary phase origin)
  "Show EXCEPTION in a buffer that can be browsed.

SUMMARY is what a terminal repl would have printed - which says more than
the exception does, because it tells a read failure from a macroexpansion
failure.  PHASE is which step it came out of, ORIGIN says where it was
caught."
  (unless exception (user-error "No exception"))
  (let ((buffer (get-buffer-create "*replique-exception*")))
    (with-current-buffer buffer
      ;; Before anything buffer local is set: the mode kills them
      (replique-exception-mode)
      (setq replique-exception--exception exception
            replique-exception--summary summary
            replique-exception--phase phase
            replique-exception--origin origin
            replique-exception--index 0
            replique-exception--folded nil)
      (replique-exception--render))
    (pop-to-buffer buffer)
    buffer))

(defun replique-exception-button (text exception &optional summary phase origin)
  "Return TEXT, as something that opens EXCEPTION when it is clicked.

SUMMARY, PHASE and ORIGIN are what `replique-exception-show\' is given.
The text of an exception in a repl buffer is what a developer looks at
first, so it is also the way to the whole of it."
  (if (null exception)
      text
    (propertize (buttonize text
                           (lambda (_)
                             (replique-exception-show exception summary phase origin)))
                'help-echo "RET or mouse-1: browse this exception")))

(provide 'replique-exception)

;;; replique-exception.el ends here
