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
;; form - and the locals are the half of that only this side knows: a name
;; bound by the form being written is a name the process has never seen.  So
;; they travel with the request, and the process answers all of them in one
;; list.  That is also what makes a local shadow: a `let' that binds `map'
;; is answered as that local rather than as the var, and the two lists meet
;; nowhere else.
;;
;; The wait is what a capf leaves no way around.  It is called for what it
;; returns, so the answer has to be there by then; what makes that safe to do
;; behind a keystroke is that C-g is heard while it waits, which is
;; `replique-conn-request-sync'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'treesit)
(require 'replique-clojure-mode)
(require 'replique-conn)
(require 'replique-deps)
(require 'replique-eval)
(require 'replique-forms)
(require 'replique-locals)
(require 'replique-process)
(require 'replique-repl)

(defcustom replique-completion-timeout 2.0
  "How long to wait for the candidates, in seconds.

A control connection answers its requests in order, so a completion asked
while the process is resolving a library waits for the library.  What
this is for is the process that will not answer at all: \\[keyboard-quit]
is what stops a wait somebody is tired of, and this is what stops one
nobody is watching."
  :type 'number
  :group 'replique)

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
    ("special-form" . "s"))
  "What each kind of candidate is shown as, by the name the process gives it.

Two letters at most, because this is written beside every candidate of a
list somebody is reading down: what it is worth saying there is that
these three are functions and that one is a macro, and a reader who wants
the word rather than the letter is a reader who has stopped scanning.")

;;; What is being asked, and of whom

(defun replique-completion--process ()
  "Return the process to ask, or nil when there is none.

The process of the repl the commands act on, which is the process the
code being written would be evaluated in - a name that is offered where
another process would have to be asked to load it is a name offered in
the wrong buffer.  A repl buffer answers for itself.  Without a repl
anywhere it is the current process, since a classpath is a thing a
process has whether or not anybody has opened a repl on it."
  (let ((repl (replique-repl-current)))
    (if repl
        (replique-repl-process repl)
      (replique-process-current))))

(defun replique-completion--namespace ()
  "Return the namespace point is writing in, or nil when nothing names one.

What `clojure.core/load' resolves a relative path against, and the one
thing a load needs that is not written in the load itself.

A repl buffer is written in the namespace its prompt gave rather than in
whatever the buffer names: a transcript holds every namespace the repl
has been in, and the one it is in now was never written there at all."
  (if replique--buffer-repl
      (replique-repl--ns replique--buffer-repl)
    (replique-eval--ns-at (point))))

(defun replique-completion--name-start (start end)
  "Return where the name written between START and END begins.

Emacs reads a symbol as starting at the reader macro in front of it: the
whole of a quoted name is one symbol to `bounds-of-thing-at-point\=', and
so are a var quote and a discarded form.  What a candidate replaces is
the name, and the macro in front of it stays where it is - a require
written as (require \='clojure.st) is completing a namespace, and writing
the candidate over the quote as well would unquote it.

An underscore is skipped only behind a hash, since a name may begin with
one: _x is a symbol somebody wrote and #_x is a symbol somebody wrote
behind a discard."
  (let ((start start))
    (while (and (< start end) (memq (char-after start) '(?# ?\')))
      (setq start (1+ start))
      (when (and (< start end)
                 (eq ?_ (char-after start))
                 (eq ?# (char-after (1- start))))
        (setq start (1+ start))))
    start))

(defun replique-completion--bounds ()
  "Return the region point is completing in, as a cons of two positions.

Inside a string it starts after the quote, because what is written there
is a path: a slash is not part of a symbol, and the whole of what was
typed is what a candidate replaces.  Outside one it is the symbol point
is in, less whatever reader macro is written in front of it - see
`replique-completion--name-start\='.  A keyword is a symbol here, and its
colon is part of it: a candidate for one carries its colon for that
reason.

It ends at point rather than at the end of what point is in.  What
follows point is what somebody has already written and did not ask about,
and completing over it would answer a question nobody asked.

Never nil.  Point sitting where no symbol is - after a bracket, which is
where a spec is about to be written - is nothing typed rather than
nothing to offer, and nothing typed is every name."
  (let* ((state (syntax-ppss))
         (string (and (nth 3 state) (nth 8 state)))
         (symbol (bounds-of-thing-at-point 'symbol)))
    (cond
     (string (cons (1+ string) (point)))
     (symbol (cons (replique-completion--name-start (car symbol) (point)) (point)))
     (t (cons (point) (point))))))

(defun replique-completion--locals (forms)
  "Return the locals in scope at point, as the request carries them.

FORMS says what each written form binds - see `replique-locals-forms'.

Each of them is a map holding its name, which is a shape with room in
it: what else this side knows about a local - the type a ^String on it
declares, which is what says what can be called on it - has somewhere to
go the day the process reads it.

A name bound twice is answered twice by `replique-locals-at', nearest
first, since nothing else could tell that a binding was shadowed.  What
travels is each name once: what is being asked is which names could be
written, and a name bound twice is one name to write."
  (let ((seen nil)
        (found nil))
    (dolist (local (replique-locals-at (point) forms))
      (unless (member (car local) seen)
        (push (car local) seen)
        (push (list :name (car local)) found)))
    (nreverse found)))

(defun replique-completion--code-context ()
  "Return what point is asking for in ordinary code, or nil.

Which is every kind of name at once, where a dependency form asks for
one kind and no other.  The process is the half that has most of them -
the vars of the namespace, the classes it imported, what is on the
classpath - and the locals are the half only this side has, so they
travel with the request.  It is the process that then puts them in one
order and drops the var a local shadows, which is work that can only
happen where the two lists meet.

Nil inside a string or a comment, where what is written is not a name
being written.  Nil too where the name at point is one being given
rather than used, which is a name nothing knows yet - see
`replique-locals-at-binding-position-p'.

Nil as well in a buffer that is not read as Clojure.  The parse is what
says a name written here is a Clojure name, which is what makes this
safe to turn on wherever somebody wants it - the default value of the
hook included."
  (let ((state (syntax-ppss)))
    (unless (or (nth 3 state) (nth 4 state) (null (treesit-parser-list nil 'treejure)))
      (let* ((ns (replique-completion--namespace))
             (forms (replique-forms-for (replique-completion--process) ns)))
        (unless (replique-locals-at-binding-position-p (point) forms)
          (append (list :position :code)
                  (when ns (list :ns ns))
                  (when-let* ((locals (replique-completion--locals forms)))
                    (list :locals locals))))))))

(defun replique-completion--message (context text)
  "Return the request that asks for the candidates of TEXT in CONTEXT.

CONTEXT is what was read at point - the slot of a dependency form, or the
name being written in ordinary code - and what it holds is already what
the op asks for: a position, and the prefix or the namespace or the
locals that position needs.  The namespace a load is written in is the
one thing it cannot hold, since it is not written in the load."
  (let ((msg (append (list :op :completions :text text) context)))
    (if (eq (plist-get context :position) :load-path)
        (append msg (list :ns (replique-completion--namespace)))
      msg)))

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
  (let* ((process (replique-completion--process))
         (frame (and process
                     (replique-process-request-sync
                      process
                      (replique-completion--message context text)
                      replique-completion-timeout))))
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

The slot of a dependency form point is in, and where point is in none of
those, the name it is writing in ordinary code.  Which of the two it is
is settled before either of them is asked, because the two mean
different things by having no answer: inside a require, what follows an
:as is a name being given and nothing is offered for it, where the same
nil outside one would be a point in ordinary code.

Nil where nothing here has an answer: with no process to ask, in a
buffer that is not read as Clojure, and where point is somewhere no name
goes at all - in a comment, in a string that is not a path, or at a name
being given rather than used."
  (when (replique-completion--process)
    (when-let* ((context (if (replique-deps-form-at-p (point))
                             (replique-deps-context-at (point))
                           (replique-completion--code-context))))
      (let ((bounds (replique-completion--bounds)))
        (list (car bounds)
              (cdr bounds)
              (replique-completion--table context)
              :annotation-function #'replique-completion-annotation)))))

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
  (let ((process (or (replique-completion--process) (replique-process-ensure))))
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
