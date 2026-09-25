;;; replique-completion.el --- What could be written where point is  -*- lexical-binding: t; -*-

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

;; Completion, which is two questions asked in one request.
;;
;; Which names could be written where point is depends on the slot it is in,
;; and reading that is `replique-deps'.  Which names those are depends on the
;; classpath, and only the process knows it.  So what goes out is the slot,
;; what has been typed in it, and whatever else that slot needs - the prefix
;; a namespace is written under, the namespace a var is referred from - and
;; what comes back is the candidates for it.
;;
;; The process matches a piece at a time: cljs.st finds cljs.spec.test and
;; jud finds java.util.Date.  So a candidate is not a completion of what was
;; typed in the sense the styles Emacs comes with mean, and every one of them
;; would filter almost all of them out again.  The `replique' style is what
;; keeps them - the matching was done where the names are, and there is
;; nothing left to do here.  It is put on this completion alone, through the
;; category the table names, so that `completion-styles' stays whatever it
;; was set to and every front end sees the same candidates: what reads a
;; table reads it through the styles of its category.
;;
;; The order is the process's too, shortest first, so nothing here sorts.
;; Neither is anything kept: the answer is capped, and what was cut off a
;; short text is what a longer one would have found - so a longer text is
;; asked again rather than filtered out of the answer to a shorter one.
;;
;; In ordinary code every kind of name goes at once - a local, a var the
;; namespace maps, a class it imported, an alias, a namespace, a special
;; form - and the locals are the half of that only this side knows.  What is
;; read out of the buffer to ask any of that is `replique-name', which is
;; the half `replique-symbol' asks with too.
;;
;; The wait is what a capf leaves no way around.  It is called for what it
;; returns, so the answer has to be there by then; what makes that safe to do
;; behind a keystroke is that C-g is heard while it waits, which is
;; `replique-conn-request-sync'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'replique-name)
(require 'replique-process)

(defconst replique-completion--annotations
  '(("namespace" . "n")
    ("namespace-prefix" . "np")
    ("class" . "c")
    ("package" . "p")
    ("macro" . "m")
    ("function" . "f")
    ("var" . "v")
    ("keyword" . "k")
    ("path" . "r")
    ("local" . "l")
    ("special-form" . "s")
    ("method" . "mt")
    ("field" . "fd")
    ("constructor" . "ct"))
  "What each kind of candidate is shown as, by the name the process gives it.

Two letters at most, because this is written beside every candidate of a
list somebody is reading down: what it is worth saying there is that
these three are functions and that one is a macro, and a reader who wants
the word rather than the letter is a reader who has stopped scanning.")

;;; Asking

(defun replique-completion--candidate (candidate)
  "Return the string CANDIDATE, a candidate as it arrived, is.

What it is and where it came from ride on it as properties: the string
itself is what gets written into the buffer, and everything else about it
is for the list it is shown in."
  (propertize (plist-get candidate :candidate)
              'replique-type (plist-get candidate :type)
              'replique-ns (plist-get candidate :ns)
              'replique-package (plist-get candidate :package)
              'replique-match-index (plist-get candidate :match-index)))

(defun replique-completion--ask (context text)
  "Return the candidates for TEXT in CONTEXT, by asking the process."
  (let* ((process (replique-name-process))
         (frame (and process
                     (replique-process-request-sync
                      process
                      (replique-name-message :completions context text)
                      replique-name-timeout))))
    (cond
     ;; C-g, which is somebody saying they are no longer waiting.  Nothing
     ;; to say about it: they know
     ((null frame) nil)
     ((equal "error" (plist-get frame :tag))
      (message "replique: %s" (plist-get frame :message))
      nil)
     (t (mapcar #'replique-completion--candidate (plist-get frame :completions))))))

(defvar replique-completion--last nil
  "The answer to the last question, as (CONTEXT TEXT . CANDIDATES).

One question rather than a cache of them.  A completion is looked at more
than once for the same text - what could be written there, whether what
is written there is one of them, whether there is only one - and each of
those would otherwise be a request of its own.  What it must not do is
answer for a text that was not asked about, which is why it holds the
text it was the answer to.")

(defun replique-completion--candidates (context text)
  "Return the candidates for TEXT in CONTEXT."
  (if (and replique-completion--last
           (equal context (nth 0 replique-completion--last))
           (equal text (nth 1 replique-completion--last)))
      (cddr replique-completion--last)
    (let ((found (replique-completion--ask context text)))
      (setq replique-completion--last (cons context (cons text found)))
      found)))

(defun replique-completion--table (context)
  "Return the completion table for CONTEXT.

A function and not a list, so that every text is a question the process
answers.  A list would be filtered here instead, which is the one thing
that must not happen: the process matched, and it capped what it found."
  (lambda (string pred action)
    (cond
     ((eq action 'metadata)
      '(metadata (category . replique-completion)
                 ;; shortest first, which is the order they arrived in
                 (display-sort-function . identity)
                 (cycle-sort-function . identity)))
     ((eq (car-safe action) 'boundaries) nil)
     (t
      (let* ((all (replique-completion--candidates context string))
             (all (if pred (seq-filter pred all) all)))
        (cond
         ((eq action t) all)
         ((eq action 'lambda) (and (member string all) t))
         ((null action)
          (cond
           ((null all) nil)
           ((and (null (cdr all)) (equal string (car all))) t)
           (t string)))))))))

;;; What a candidate is shown as

(defun replique-completion-annotation (candidate)
  "Return what CANDIDATE is and where it came from, or nil.

Where it came from is the one thing a name does not say about itself: a
var is written without the namespace it is public in, a class without
the package it is in, and an alias says nothing at all about what it
reaches."
  (let* ((type (get-text-property 0 'replique-type candidate))
         (from (or (get-text-property 0 'replique-ns candidate)
                   (get-text-property 0 'replique-package candidate)))
         (short (cdr (assoc type replique-completion--annotations))))
    (cond
     ((and from short) (format " %s <%s>" from short))
     (short (format " <%s>" short))
     (from (format " %s" from)))))

(defun replique-completion--matched (candidate)
  "Return CANDIDATE with the part that matched faced.

How far the match reached is what the process said, and it is the only
thing that can say it: the tokens were matched there, and a front end
looking at the text and the candidate could not work out from the two of
them which pieces of one it was that found the other.  `completions-common-part'
is where every front end reads it from."
  (let ((candidate (copy-sequence candidate))
        (index (get-text-property 0 'replique-match-index candidate)))
    (when (and (natnump index) (> index 0))
      (add-face-text-property 0 (min index (length candidate))
                              'completions-common-part nil candidate))
    candidate))

;;; The style

(defun replique-completion--all (string table pred _point)
  "Return every candidate for STRING in TABLE, for the `replique' style.

Which is all of them, faced.  PRED is passed on to TABLE.  The last cdr
is where the candidates start in STRING, and they start at the beginning
of it: what was typed is what they replace."
  (let ((all (all-completions string table pred)))
    (when all
      (nconc (mapcar #'replique-completion--matched all) 0))))

(defun replique-completion--try (string table pred point)
  "Return what STRING completes to in TABLE, for the `replique' style.

The one candidate when there is one, and STRING back unchanged with
POINT where it was when there are several.  There is nothing to grow it
by - candidates matched a piece at a time have no beginning in common,
and two that do have one, clojure.set and clojure.string, have it by
being namespaces rather than by having been matched.  PRED is passed on
to TABLE."
  (let ((all (all-completions string table pred)))
    (cond
     ((null all) nil)
     ((and (null (cdr all)) (equal string (car all))) t)
     ((null (cdr all)) (cons (car all) (length (car all))))
     (t (cons string point)))))

(add-to-list 'completion-styles-alist
             '(replique
               replique-completion--try
               replique-completion--all
               "Completion as the replique process matched it.

Which is a piece at a time, and so not by any prefix of what was typed.
The candidates arrive matched and in order, and this is what keeps them
that way."))

;; On this completion and no other.  `completion-styles' is somebody's
;; setting, and a package that changes it changes every completion in the
;; editor to fix the one it is responsible for.  A category is the seam
;; emacs offers instead, and it stays overridable: whoever wants these
;; filtered again has `completion-category-overrides' to say so in
(add-to-list 'completion-category-defaults
             '(replique-completion (styles replique)))

;;; The entry point

(defun replique-completion-at-point ()
  "Return what could be written at point, for `completion-at-point-functions'.

What point is writing is `replique-name-context\\=' to read - the slot of
a dependency form, the string point is in, or the name being written in
ordinary code - and the region a candidate replaces is
`replique-name-bounds\\='.

Nil where nothing here has an answer: with no process to ask, in a
buffer that is not read as Clojure, and where point is somewhere no name
goes at all - in a comment, or at a name being given rather than used.

In a string this stands aside when it has nothing.  Most strings are text
and a few of them are paths, and which of the two this one is, is the
process's to say - so a string it answers nothing for is a string this
had no business speaking for, and whoever else completes in this buffer
gets their turn at it.  Everywhere else an empty answer is an answer: the
process was asked what could be written and said nothing could."
  (when (replique-name-process)
    (when-let* ((context (replique-name-context)))
      (let ((bounds (replique-name-bounds)))
        (append (list (car bounds)
                      (cdr bounds)
                      (replique-completion--table context)
                      :annotation-function #'replique-completion-annotation)
                (when (eq (plist-get context :position) :string)
                  (list :exclusive 'no)))))))

;;;###autoload
(defun replique-completion-install ()
  "Answer completion in the current buffer with what the process knows."
  (add-hook 'completion-at-point-functions #'replique-completion-at-point nil t))

(defun replique-completion-uninstall ()
  "Stop answering completion in the current buffer."
  (remove-hook 'completion-at-point-functions #'replique-completion-at-point t))

;;; Reading the classpath again

;;;###autoload
(defun replique-update-classpath ()
  "Have the process read its classpath again.

It reads it once and keeps it, because walking every jar and every
directory on it is not work to do behind a keystroke.  What that costs is
a file written afterwards, which is not found until this is asked for -
and a namespace somebody has just created is exactly the namespace they
are about to require.

The answer nobody is waiting for either, so this does not wait for it: a
classpath of any size takes long enough that holding the editor for it
would be felt."
  (interactive)
  (let ((process (or (replique-name-process) (replique-process-ensure))))
    (setq replique-completion--last nil)
    (replique-process-request
     process (list :op :update-classpath)
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (message "replique: %s" (plist-get frame :message))
         (message "replique: %s namespaces and %s classes on the classpath"
                  (plist-get frame :namespaces)
                  (plist-get frame :classes)))))))

(provide 'replique-completion)

;;; replique-completion.el ends here
