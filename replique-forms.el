;;; replique-forms.el --- How a namespace writes the forms that bind  -*- lexical-binding: t; -*-

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

;; Which symbol means which form, inside one namespace.
;;
;; `replique-locals' reads a parse to say what is a local, and to do that it
;; has to know which of the symbols written in it name the forms that bind.
;; Nearly always they are written the way clojure.core writes them - let is
;; let - and `replique-locals-default-forms' says so without asking anybody.
;;
;; Not always.  A namespace that aliases clojure.core writes c/let, one that
;; referred let under another name writes that name, and one that excluded it
;; and defined a let of its own writes let for a var that binds nothing at
;; all.  Which of those a namespace did is a question only the process can
;; answer, because only the process has the namespace, and the :spellings op
;; is the asking.
;;
;; What comes back is kept for a moment.  A tool that reads locals reads them
;; per keystroke, and asking per keystroke would be a request for an answer
;; that changes when somebody evaluates a namespace again - which is a thing
;; done once and typed after.  So it is kept for less time than that takes to
;; notice, and nothing has to work out when it went stale.
;;
;; The default is also the answer whenever there is no better one: no process,
;; a process that would not answer, a wait that ran out.  A table that is
;; right nearly everywhere beats none at all, and being wrong here costs a
;; local that goes unnoticed - which is asked about and not found, where a var
;; wrongly called a local is a symbol nothing will describe.

;;; Code:

(require 'replique-locals)
(require 'replique-process)
(require 'replique-repl)

(defconst replique-forms-timeout 1.0
  "How long to wait for the process to say, in seconds.

Shorter than a completion waits.  This is asked on the way to answering
something else, and what it answers has a default that is nearly always
right - so a process that is busy is one to stop waiting for early.")

(defconst replique-forms-kept 2.0
  "How long an answer stands before it is asked for again, in seconds.")

(defvar replique-forms--kept (make-hash-table :test #'equal)
  "What was answered, as (WHEN . FORMS), by process and namespace.")

(defun replique-forms-forget ()
  "Forget what every namespace was said to write its forms as."
  (clrhash replique-forms--kept))

(defun replique-forms--spellings (frame)
  "Return the spellings FRAME carries, by the qualified name of each var.

The keys of a JSON object arrive as keywords, and what they name here is
a var: the colon that made each of them a keyword is taken back off."
  (let ((plist (plist-get frame :spellings))
        (found nil))
    (while plist
      (push (cons (substring (symbol-name (car plist)) 1) (cadr plist)) found)
      (setq plist (cddr plist)))
    found))

(defun replique-forms--ask (process ns)
  "Ask PROCESS what NS writes the forms that bind as, or nil for no answer.

Every var of `replique-locals-vars' at once rather than one question per
form: they are asked about together, and one namespace is one lookup
whichever of them is being asked about."
  (let ((frame (replique-process-request-sync
                process
                (append (list :op :spellings
                              :vars (apply #'append (mapcar #'cdr replique-locals-vars)))
                        (when ns (list :ns ns))
                        ;; How a form binds is a question about a var, and
                        ;; the two worlds hold two sets of them
                        (replique-dialect-keys))
                replique-forms-timeout)))
    (cond
     ;; C-g, which is somebody who is no longer waiting for what this was on
     ;; the way to answering
     ((null frame) nil)
     ((equal "error" (plist-get frame :tag))
      (message "replique: %s" (plist-get frame :message))
      nil)
     (t (replique-locals-forms (replique-forms--spellings frame))))))

(defun replique-forms-for (process ns)
  "Return what NS in PROCESS writes the forms that bind as.

For `replique-locals-at', which is what the table is for.
`replique-locals-default-forms' wherever there is no better answer - see
the commentary for why that is the right thing to fall back to."
  (if (null process)
      replique-locals-default-forms
    (let* ((key (cons (replique-process--id process) ns))
           (kept (gethash key replique-forms--kept)))
      (if (and kept (< (- (float-time) (car kept)) replique-forms-kept))
          (cdr kept)
        (let ((found (replique-forms--ask process ns)))
          (cond
           (found (puthash key (cons (float-time) found) replique-forms--kept)
                  found)
           ;; What was kept, however old, beats the default: it was the answer
           ;; for this namespace, and the default is the answer for a
           ;; namespace nobody has asked about
           (kept (cdr kept))
           (t replique-locals-default-forms)))))))

(provide 'replique-forms)

;;; replique-forms.el ends here
