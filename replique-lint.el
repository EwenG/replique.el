;;; replique-lint.el --- What is wrong with a file, as the compiler saw it  -*- lexical-binding: t; -*-

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

;; clj-kondo's lints - an unused binding, a require nothing uses, a call with
;; the wrong number of arguments, a var that is not there - asked of the
;; compiler rather than of the text, and shown through flymake.
;;
;; THE COMPILER IS RIGHT ABOUT ONE VERSION OF THE FILE: the one on disk that it
;; last compiled.  The process answers about that version and nothing else, and
;; says which it is - the modification time of the file it compiled - so what
;; this file has to get right is where that version's positions are in the text
;; somebody is now editing.  Which is three rules:
;;
;;   SHOWN AFTER A LOAD.  Positions are only taken as they come while the buffer
;;   holds the version they are about - unmodified, and visiting a file of that
;;   modification time.  That is when each top level form's place is written
;;   down, as a marker: the table `replique-lint--table\=' holds.  From then on a
;;   lint is placed relative to the start of its form, wherever the form has
;;   moved to, so typing above it moves it and loses nothing.
;;
;;   HIDDEN IN A FORM BEING EDITED.  What a lint says about a form is about the
;;   form as it was compiled; edit it and the lint is about text that is not
;;   there any more.  So an edit marks the forms it touches, and their lints go
;;   until the file is loaded again.  The rest of the file keeps its lints.
;;
;;   AND A VERDICT ABOUT THE WHOLE FILE GOES AT THE FIRST EDIT.  A require
;;   nothing uses, a private var nothing calls: any edit anywhere may have made
;;   it false, so a lint the process says is about the file (its `:scope\=') is
;;   shown only while the buffer is exactly what was compiled.
;;
;; ASKED AGAIN WHEN THE ANSWER MAY HAVE CHANGED.  The process says so, with an
;; `analysis' event, after anything that rewrote what it knows - a load, a
;; reload, a var removed - and the lints of a file change when another file
;; does: a call is wrong the moment its callee's arity is.  A buffer in a window
;; asks at once; one nobody is looking at asks when it is next shown.

;;; Code:

(require 'flymake)
(require 'replique-common)
(require 'replique-parse)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-name)

(defgroup replique-lint nil
  "What is wrong with a file, as the compiler saw it."
  :group 'replique)

(defcustom replique-lint-flymake t
  "Whether a Clojure buffer turns `flymake-mode\=' on to show its lints.

The lints are a flymake backend either way - `replique-lint-flymake\=' -
so nil is for somebody who turns flymake on themselves, or shows the lints
some other way through `replique-lint-diagnostics\='."
  :type 'boolean)

;;; What the process said

(defvar-local replique-lint--answers nil
  "What the process answered about this buffer\\='s file, by dialect.

An alist of (DIALECT . FRAME), one entry per compiler asked - two for a
.cljc file, which both compile.")

(defvar-local replique-lint--generation nil
  "The `analysis\=' generation this buffer\\='s answers were asked at, or nil.")

(defvar replique-lint--latest 0
  "The latest `analysis\=' generation a process has announced.

A buffer asked at an earlier one is behind, and asks again when it is
next shown.")

;;; Where the compiled version is

(defvar-local replique-lint--table nil
  "Where the top level forms of the compiled version are now, or nil.

A list of vectors [LINE COLUMN END-LINE END-COLUMN START END EDITED]: where
the form was in the compiled version, markers where it is now, and whether
it has been edited since.")

(defvar-local replique-lint--table-mtime nil
  "The modification time, in milliseconds, of the version the table is of.")

(defvar-local replique-lint--edited nil
  "Whether anything in the buffer was edited since the table was written.")

(defun replique-lint--visited-mtime ()
  "The modification time of the file this buffer visits, in milliseconds.

Nil where it visits none.  Milliseconds because that is what the process
reads a file\\='s time in."
  (let ((modtime (visited-file-modtime)))
    (unless (or (null modtime) (equal modtime 0))
      (car (time-convert modtime 1000)))))

(defun replique-lint--line-column (pos)
  "Where POS is, as the line and the column the Clojure reader counts.

Both from 1, and the column in characters: a tab is one."
  (save-excursion
    (goto-char pos)
    (list (line-number-at-pos pos t) (1+ (- pos (line-beginning-position))))))

(defun replique-lint--forget-table ()
  "Let go of the table and its markers."
  (dolist (entry replique-lint--table)
    (set-marker (aref entry 4) nil)
    (set-marker (aref entry 5) nil))
  (setq replique-lint--table nil
        replique-lint--table-mtime nil
        replique-lint--edited nil))

(defun replique-lint--write-table (mtime)
  "Write down where every top level form is, as the version of MTIME.

Only called while the buffer holds that version."
  (replique-lint--forget-table)
  (save-restriction
    (widen)
    (let ((table nil))
      (dolist (form (replique-parse-forms-in (point-min) (1+ (point-max))))
        (unless (replique-parse-gap-p form)
          (let* ((start (replique-parse-start form))
                 (end (replique-parse-end form))
                 (from (replique-lint--line-column start))
                 (to (replique-lint--line-column end)))
            (push (vector (car from) (cadr from) (car to) (cadr to)
                          ;; text typed at the start of a form is typed
                          ;; before it: the marker moves on past it
                          (copy-marker start t) (copy-marker end) nil)
                  table))))
      (setq replique-lint--table (nreverse table)
            replique-lint--table-mtime mtime
            replique-lint--edited nil))))

(defun replique-lint--table-for (mtime)
  "Whether the table is of the version of MTIME, writing it if it can be.

It can be where the buffer is that version: unmodified, and visiting a file
of that time.  Written again there if anything was edited since it was last
written, which is an edit undone - or the same version loaded again, which
is what brings the lints of an edited form back."
  (cond
   ((null mtime) nil)
   ((and (not (buffer-modified-p))
         (eql mtime (replique-lint--visited-mtime)))
    (when (or (not (eql mtime replique-lint--table-mtime)) replique-lint--edited)
      (replique-lint--write-table mtime))
    t)
   ((eql mtime replique-lint--table-mtime) t)))

(defun replique-lint--note-change (beginning end)
  "Mark the forms the change of the text between BEGINNING and END touches.

Before the change, while the markers still say where each form is.  Text
replaced or deleted touches a form it overlaps; text inserted touches one it
lands strictly inside - typed just before a form or just after it, it is
text beside the form and the form still reads as it was compiled."
  (when replique-lint--table
    (setq replique-lint--edited t)
    (dolist (entry replique-lint--table)
      (let ((start (marker-position (aref entry 4)))
            (stop (marker-position (aref entry 5))))
        (when (if (= beginning end)
                  (and (< start beginning) (< beginning stop))
                (and (< beginning stop) (> end start)))
          (aset entry 6 t))))))

(defun replique-lint--entry-at (line column)
  "The form of the compiled version written over LINE and COLUMN, or nil."
  (let ((at (list line column)))
    (seq-find (lambda (entry)
                (and (not (replique-lint--before at (list (aref entry 0) (aref entry 1))))
                     (replique-lint--before at (list (aref entry 2) (aref entry 3)))))
              replique-lint--table)))

(defun replique-lint--before (a b)
  "Whether the line and column A come before B."
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cadr a) (cadr b)))))

(defun replique-lint--in-form (entry line column)
  "Where LINE and COLUMN of the compiled version are now, inside ENTRY\\='s form.

The form has not been edited, so its text is the compiled text: the place
is the same distance from its start."
  (save-excursion
    (goto-char (aref entry 4))
    (if (= line (aref entry 0))
        (forward-char (max 0 (- column (aref entry 1))))
      (forward-line (- line (aref entry 0)))
      (forward-char (min (max 0 (1- column)) (- (line-end-position) (point)))))
    (point)))

(defun replique-lint--absolute (line column)
  "Where LINE and COLUMN are in the buffer, counted from its start."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char (min (max 0 (1- column)) (- (line-end-position) (point))))
      (point))))

(defun replique-lint--end-of-form-at (pos)
  "Where the form written at POS ends, for a lint that says only where it starts."
  (save-excursion
    (goto-char pos)
    (condition-case nil
        (progn (forward-sexp) (point))
      (error (line-end-position)))))

(defun replique-lint--place (lint)
  "Where LINT is now, as (BEGINNING . END), or nil where it is not to be shown."
  (let ((line (plist-get lint :line))
        (column (or (plist-get lint :column) 1))
        (end-line (plist-get lint :end-line))
        (end-column (plist-get lint :end-column)))
    (when line
      (if (equal "file" (plist-get lint :scope))
          ;; Nothing edited, so the buffer is the compiled text as it stands
          (unless replique-lint--edited
            (let ((beginning (replique-lint--absolute line column)))
              (cons beginning
                    (if end-line
                        (replique-lint--absolute end-line end-column)
                      (replique-lint--end-of-form-at beginning)))))
        (let ((entry (replique-lint--entry-at line column)))
          (when (and entry (not (aref entry 6)))
            (let ((beginning (replique-lint--in-form entry line column)))
              (cons beginning
                    (if (and end-line end-column)
                        (replique-lint--in-form entry end-line end-column)
                      (replique-lint--end-of-form-at beginning))))))))))

(defun replique-lint--level (lint)
  "LINT\\='s level as a symbol: `error\=', `warning\=' or `info\='."
  (pcase (plist-get lint :level)
    ("error" 'error)
    ("warning" 'warning)
    (_ 'info)))

(defun replique-lint-diagnostics ()
  "What the current buffer\\='s lints are, where they are now.

A list of plists (:beginning :end :level :type :message), in the order they
are in the buffer.  The data a display is made of, flymake\\='s or anybody
else\\='s - see `replique-lint-flymake\='.

Where a .cljc file was answered by both compilers, a lint both say is said
once and one only one of them says names which."
  (let* ((answers (seq-filter (lambda (answer) (plist-get (cdr answer) :mtime))
                              replique-lint--answers))
         (both (cdr answers))
         (said (make-hash-table :test #'equal))
         (found nil))
    (dolist (answer answers)
      (when (replique-lint--table-for (plist-get (cdr answer) :mtime))
        (dolist (lint (plist-get (cdr answer) :lints))
          (let ((key (list (plist-get lint :line) (plist-get lint :column)
                           (plist-get lint :message))))
            (puthash key (cons (car answer) (gethash key said)) said)
            (unless (cdr (gethash key said))
              (when-let* ((place (replique-lint--place lint)))
                (push (list :key key
                            :beginning (car place) :end (cdr place)
                            :level (replique-lint--level lint)
                            :type (plist-get lint :type)
                            :message (plist-get lint :message))
                      found)))))))
    (sort (mapcar (lambda (diagnostic)
                    (let ((dialects (gethash (plist-get diagnostic :key) said)))
                      (when (and both (null (cdr dialects)))
                        (setq diagnostic
                              (plist-put diagnostic :message
                                         (format "[%s] %s"
                                                 (substring (symbol-name (car dialects)) 1)
                                                 (plist-get diagnostic :message)))))
                      diagnostic))
                  found)
          (lambda (a b) (< (plist-get a :beginning) (plist-get b :beginning))))))

;;; Flymake

(defun replique-lint-flymake (report-fn &rest _)
  "Report this buffer\\='s lints to flymake through REPORT-FN.

A flymake backend.  Answers at once from what the process last said, placed
where the text is now - flymake asks again after every edit, which is what
takes the lints of an edited form away."
  (funcall report-fn
           (mapcar (lambda (d)
                     (flymake-make-diagnostic
                      (current-buffer) (plist-get d :beginning) (plist-get d :end)
                      (pcase (plist-get d :level)
                        ('error :error) ('warning :warning) (_ :note))
                      (format "%s [%s]" (plist-get d :message) (plist-get d :type))))
                   (replique-lint-diagnostics))))

(defun replique-lint--show ()
  "Show what the current buffer\\='s answers say."
  (when (bound-and-true-p flymake-mode)
    (flymake-start)))

;;; Asking

(defun replique-lint--dialects ()
  "The compilers to ask about the current buffer, with the keys of each.

A list of (DIALECT . KEYS).  A .cljc file is compiled by both, and both are
asked where a ClojureScript repl is open."
  (let ((cljs (lambda ()
                (append (list :dialect :cljs)
                        (when-let* ((repl (replique-repl-for-dialect :cljs))
                                    (target (replique-repl-target repl)))
                          (list :target target))))))
    (cond
     ((derived-mode-p 'replique-clojure-clojurescript-mode)
      (list (cons :cljs (funcall cljs))))
     ((derived-mode-p 'replique-clojure-clojurec-mode)
      (cons (cons :clj nil)
            (when (replique-repl-for-dialect :cljs)
              (list (cons :cljs (funcall cljs))))))
     (t (list (cons :clj nil))))))

(defun replique-lint--lintable-p ()
  "Whether the current buffer is a Clojure file the compiler could have compiled."
  (and (buffer-file-name)
       (not (string-match-p "\\.edn\\'" (buffer-file-name)))
       (derived-mode-p 'replique-clojure-mode)))

(defun replique-lint-refresh (&optional buffer)
  "Ask the process again about BUFFER\\='s file, or the current buffer\\='s."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((process (replique-name-process))
          (buffer (current-buffer))
          (file (buffer-file-name)))
      (when (and process (replique-lint--lintable-p)
                 (replique-conn-live-p (replique-process--control process)))
        (setq replique-lint--generation replique-lint--latest)
        (dolist (dialect (replique-lint--dialects))
          (replique-process-request
           process (append (list :op :lints :file file) (cdr dialect))
           (lambda (frame)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq replique-lint--answers
                       (cons (cons (car dialect) frame)
                             (assq-delete-all (car dialect)
                                              (copy-sequence replique-lint--answers))))
                 (replique-lint--show))))))))))

(defun replique-lint--behind-p (buffer)
  "Whether BUFFER shows lints and has not asked since the last announcement."
  (and (buffer-local-value 'replique-lint-mode buffer)
       (not (eql (buffer-local-value 'replique-lint--generation buffer)
                 replique-lint--latest))))

(defun replique-lint--on-analysis (_process _frame)
  "The process says what it knows changed: ask again where it shows.

Counted here rather than read off the event\='s `:generation\=', which is the
process\='s count - and there can be more than one process."
  (setq replique-lint--latest (1+ replique-lint--latest))
  (dolist (buffer (buffer-list))
    (when (and (replique-lint--behind-p buffer) (get-buffer-window buffer t))
      (replique-lint-refresh buffer))))

(defun replique-lint--on-shown (frame)
  "Ask again for a buffer shown in FRAME that is behind."
  (dolist (window (window-list frame 'never))
    (let ((buffer (window-buffer window)))
      (when (replique-lint--behind-p buffer)
        (replique-lint-refresh buffer)))))

(add-hook 'replique-process-analysis-functions #'replique-lint--on-analysis)
(add-hook 'window-buffer-change-functions #'replique-lint--on-shown)

;;; Turning it on

;;;###autoload
(define-minor-mode replique-lint-mode
  "Show what the compiler found wrong with this file.

Asked of the process after a load and whenever it says what it knows has
changed, and shown through flymake - see `replique-lint-flymake\='.  A lint
is shown only while the text it is about is the text that was compiled: an
edited form loses its lints, and a lint about the whole file - a require
nothing uses - goes at the first edit.  \\[replique-load-file] brings them
back."
  :lighter nil
  (if replique-lint-mode
      (progn
        (add-hook 'before-change-functions #'replique-lint--note-change nil t)
        (add-hook 'flymake-diagnostic-functions #'replique-lint-flymake nil t)
        (when (and replique-lint-flymake (not (bound-and-true-p flymake-mode)))
          (flymake-mode 1))
        (replique-lint-refresh))
    (remove-hook 'before-change-functions #'replique-lint--note-change t)
    (remove-hook 'flymake-diagnostic-functions #'replique-lint-flymake t)
    (replique-lint--forget-table)
    (setq replique-lint--answers nil
          replique-lint--generation nil)))

;;;###autoload
(defun replique-lint-install ()
  "Show lints in the current buffer, where it is a Clojure file."
  (when (replique-lint--lintable-p)
    (replique-lint-mode 1)))

(defun replique-lint-uninstall ()
  "Stop showing lints in the current buffer."
  (when replique-lint-mode
    (replique-lint-mode -1)))

(provide 'replique-lint)

;;; replique-lint.el ends here
