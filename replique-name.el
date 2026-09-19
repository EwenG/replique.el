;;; replique-name.el --- What point is writing, and what to ask about it  -*- lexical-binding: t; -*-

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

;; Two ops ask about the name point is writing.  `replique-completion' asks
;; what could be written there, and `replique-symbol' what the one written
;; there is - and both of them send the same request: the slot of the form
;; point is in, the text written in it, and the namespace, the locals and
;; the type this side read around it.
;;
;; So reading that out of the buffer is one job rather than two, and this is
;; where it is done.  Which process to ask is here for the same reason: it
;; is the process the code being written would be evaluated in, and that is
;; the same process whichever of the two is asking.
;;
;; The locals are the half only this side has.  A name bound by the form
;; being written is a name the process has never seen - it is in the text
;; and nowhere else - so they travel with the request and are answered
;; there, beside the vars.  Everything else here is of the same kind: what
;; the buffer says and the process cannot.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'replique-parse)
(require 'replique-deps)
(require 'replique-eval)
(require 'replique-forms)
(require 'replique-locals)
(require 'replique-process)
(require 'replique-repl)

(defcustom replique-name-timeout 2.0
  "How long to wait for what the process says about a name, in seconds.

A control connection answers its requests in order, so a question asked
while the process is resolving a library waits for the library.  What
this is for is the process that will not answer at all: \\[keyboard-quit]
is what stops a wait somebody is tired of, and this is what stops one
nobody is watching.

What waits is what has to: a completion is asked for what it returns, and
so is a definition somebody just pressed a key to be taken to.  What is
asked about a call while somebody reads it does not wait at all - see
`replique-symbol-eldoc\='."
  :type 'number
  :group 'replique)

;;; What is being asked, and of whom

(defun replique-name-process ()
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

(defun replique-name-namespace ()
  "Return the namespace point is writing in, or nil when nothing names one.

What `clojure.core/load' resolves a relative path against, and the one
thing a load needs that is not written in the load itself.

A repl buffer is written in the namespace its prompt gave rather than in
whatever the buffer names: a transcript holds every namespace the repl
has been in, and the one it is in now was never written there at all."
  (if replique--buffer-repl
      (replique-repl--ns replique--buffer-repl)
    (replique-eval--ns-at (point))))

(defun replique-name--start (start end)
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

(defun replique-name-bounds ()
  "Return the region point is completing in, as a cons of two positions.

Inside a string it starts after the quote, because what is written there
is a path: a slash is not part of a symbol, and the whole of what was
typed is what a candidate replaces.  Outside one it is the symbol point
is in, less whatever reader macro is written in front of it - see
`replique-name--start\='.  A keyword is a symbol here, and its
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
     (symbol (cons (replique-name--start (car symbol) (point)) (point)))
     (t (cons (point) (point))))))

(defun replique-name--locals (forms)
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

(defun replique-name--clojure-p ()
  "Return non-nil when this buffer is one replique reads as Clojure.

What set the reading up says so, where it used to be the parse that did:
the text is read where a name is asked about, so every buffer can be read
and being readable no longer tells a Clojure buffer from a prose one.
What this keeps is why the question was asked - these are safe to turn on
wherever somebody wants them, the default value of the hook included.

`replique-clojure-read-p\=' rather than the mode, because the repl is
not in that mode and is Clojure: `replique-clojure-setup\=' is what
makes a buffer one, and the repl calls it too."
  (and replique-clojure-read-p t))

(defconst replique-name--literals
  '(string keyword number character boolean regex)
  "The nodes that are their own class.

What is written as one of these needs nothing resolved to be known: a
string is a String wherever it is written, and nothing has to be
evaluated to find that out.")

(defconst replique-name--threading
  '("->" "->>" "some->" "some->>" "doto")
  "The forms that write what they thread into each step of themselves.

A member written as a step of one is written on what is threaded:
\(-> s .length) calls .length on s.  Only the first step, since every step
after it is written on what the one before it returned, and what an
expression returns is not knowable without running it.

Read plainly or qualified with clojure.core, which is how
`replique-deps\=' reads the forms it knows.")

(defun replique-name--threading-p (node)
  "Return non-nil if NODE is the head of a threading form."
  (when (and node (eq 'symbol (replique-parse-type node)))
    (let ((parts (replique-parse-name-parts (replique-parse-text node))))
      (and (or (null (car parts))
               (equal "clojure.core" (car parts)))
           (member (cdr parts) replique-name--threading)
           t))))

(defun replique-name--member-target ()
  "Return the node a member written at point would be written on, or nil.

Which is what the list holds next where point is writing its head - the s
of (.length s) - and what a threading form threads where point is writing
its first step.  A .name written anywhere else is not a call on anything,
so nothing is written on."
  (when-let* ((start (car (replique-name-bounds)))
              (form (replique-parse-form-at start)))
    ;; the innermost list with something of it written at point, which is
    ;; the deepest one the way down has a step after it
    (let ((path (replique-parse-path form start))
          (list nil)
          (node nil))
      (while (cdr path)
        (when (eq 'list (replique-parse-type (car path)))
          (setq list (car path) node (cadr path)))
        (setq path (cdr path)))
      (when list
        (let* ((forms (replique-parse-forms list))
               (index (seq-position forms node #'eq)))
          (when (and index
                     (or (= index 0)
                         (and (= index 2)
                              (replique-name--threading-p (car forms)))))
            (nth 1 forms)))))))

(defun replique-name--written-on (written forms)
  "Return what the node WRITTEN is, as far as this side can tell.

WRITTEN is what a member being written would be called on - what the list
holds after the name, or what a threading form threads.  What travels is
what this side can say about it without running anything: the type a
^String declares, on the local or at the call site, and the target itself
where it is not a local, which the process reads as a var that declares
its type or as a literal that is its own.

FORMS says what each written form binds - see `replique-locals-forms\='.

Nil where nothing is written on, and nil where the node says nothing
about what it is.  The process answers a member with nothing then, which
is the right answer: what an expression would return is not knowable
without running it."
  (when-let* ((written written)
              (target (replique-parse-unwrap-meta written))
              (text (replique-parse-text target))
              (tag (or (replique-locals-tag-at (replique-parse-start target)) :none)))
    (let ((type (replique-parse-type target))
          (tag (and (stringp tag) tag)))
      (cond
       ((eq 'symbol type)
        (if-let* ((local (assoc text (replique-locals-at (point) forms))))
            (when-let* ((tag (or tag (replique-locals-tag-at (cdr local)))))
              (list :tag tag))
          (append (list :target text) (when tag (list :tag tag)))))
       ((memq type replique-name--literals) (list :target text))
       (tag (list :tag tag))))))

(defun replique-name--call-node ()
  "Return the innermost list or function literal point is inside, or nil."
  (when-let* ((form (replique-parse-form-at (point))))
    (let ((found nil))
      (dolist (node (replique-parse-path form (point)))
        (when (memq (replique-parse-type node) '(list fn))
          (setq found node)))
      found)))

(defun replique-name--enclosing ()
  "Return the form point is writing in, or nil when point is in none.

A plist holding :argument, which argument of that form point is at - 0 at
the head of it, 1 at the first argument, and so on; :call, the name that
head is, where the head is a name; and :on, whatever is written after the
head, which is what a member written at the head would be called on.

Which argument point is at, is how many of them end before it.  A name
point is still inside has not ended, so point is at that one; a name
point sits at the end of is the one being written, and not the next; and
at the head none of them have ended, which is what makes the head nought.

The head is not always a name.  What ((f x) y) calls is not something to
ask about, and neither is the nothing at the head of a form somebody has
just opened - and point is writing an argument of both of those all the
same, so :argument is there where :call is not."
  (when-let* ((form (replique-name--call-node)))
    (let* ((children (replique-parse-forms form))
           (head (replique-parse-unwrap-meta (car children))))
      (append (list :argument (seq-count (lambda (child)
                                           (< (replique-parse-end child) (point)))
                                         children))
              (when (and head (eq 'symbol (replique-parse-type head)))
                (list :call (replique-parse-text head)))
              (when-let* ((on (nth 1 children)))
                (list :on on))))))

(defun replique-name--asking (ns written forms)
  "Return what to ask about a name written in ordinary code.

NS is the namespace it is written in, WRITTEN the node a member there
would be called on, and FORMS what each written form binds.

All three are what the process cannot know: the namespace is what a file
declares and not what has been loaded, the locals are bound by the form
being written, and what a member is called on is written beside it.  What
the process has is everything else."
  (append (list :position :code)
          (when ns (list :ns ns))
          (when-let* ((locals (replique-name--locals forms)))
            (list :locals locals))
          (replique-name--written-on written forms)))

(defun replique-name--code-context ()
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

Nil as well in a buffer that is not read as Clojure - see
`replique-name--clojure-p'."
  (let ((state (syntax-ppss)))
    (unless (or (nth 3 state) (nth 4 state) (not (replique-name--clojure-p)))
      (let* ((ns (replique-name-namespace))
             (forms (replique-forms-for (replique-name-process) ns)))
        (unless (replique-locals-at-binding-position-p (point) forms)
          (append (replique-name--asking ns (replique-name--member-target) forms)
                  ;; and which argument of the form point is at, which is
                  ;; what says a special form could be written there: one is
                  ;; written at the head of a form and nowhere else
                  (when-let* ((enclosing (replique-name--enclosing)))
                    (list :argument (plist-get enclosing :argument)))))))))

(defun replique-name--string-context ()
  "Return what point is asking for inside a string, or nil.

Most strings are text and a few of them are paths, and what tells the two
apart is the call the string is written in: what is written in an
\(io/resource ...) is a name on the classpath, and what is written in a
\(str ...) is a message somebody is writing.  So what travels is the call
point is writing an argument of and which argument of it this is, and the
process works out the rest.

It is the process that can: the call is written under whatever alias that
namespace was given, and reading an alias means holding the namespace it
is a mapping of.  Which is what the namespace travels for.

Both are absent where point is in no form at all, which a string at the
top of a file is.  What is asked there is what the string names, since
that is a question worth asking of any of them.

Nil outside a string, and nil in a buffer that is not read as Clojure."
  (let ((state (syntax-ppss)))
    (when (and (nth 3 state) (replique-name--clojure-p))
      (let ((enclosing (replique-name--enclosing)))
        (append (list :position :string)
                (when-let* ((ns (replique-name-namespace))) (list :ns ns))
                (when-let* ((call (plist-get enclosing :call))) (list :call call))
                (when enclosing (list :argument (plist-get enclosing :argument))))))))

(defun replique-name-context ()
  "Return what point is writing, or nil when point is writing no name.

The slot of a dependency form point is in; the string point is in, where
it is in one; and otherwise the name it is writing in ordinary code.

The dependency form is asked first, and asked whether point is in one
before it is asked what point is writing there, because it is the one of
the three that means something else by having no answer: inside a
require, what follows an :as is a name being given and nothing is offered
for it, where the same nil outside one would be a point in ordinary code.
A string and ordinary code settle between themselves - each of them is
read where point is where the other is not."
  ;; The dependency form is asked whether this is a Clojure buffer too, and
  ;; not only the other two.  `replique-parse\=' reads any buffer at all -
  ;; it is text that it reads - so a require form written in a prose buffer
  ;; parses as one, and answering about it would be replique speaking for a
  ;; buffer it has no business speaking for.
  (if (and (replique-name--clojure-p) (replique-deps-form-at-p (point)))
      (replique-deps-context-at (point))
    (or (replique-name--string-context)
        (replique-name--code-context))))

(defun replique-name-message (op context text)
  "Return the request that asks OP about TEXT in CONTEXT.

CONTEXT is what was read at point - the slot of a dependency form, the
string point is in, or the name being written in ordinary code - and what
it holds is already what the op asks for: a position, and the prefix or
the namespace or the locals or the call that position needs.  The
namespace a load is written in is the one thing it cannot hold, since it
is not written in the load.

OP is which of the two questions is being asked.  They take the same
request, so the only difference between them is the word."
  (let ((msg (append (list :op op :text text) context)))
    (if (eq (plist-get context :position) :load-path)
        (append msg (list :ns (replique-name-namespace)))
      msg)))

;;; The whole of a name, and the call around it

(defun replique-name-at-point ()
  "Return the whole of the name point is in, as a cons of two positions.

Which is `replique-name-bounds\=' with its end let out.  A completion
replaces what has been typed so far, so it stops where point is; a name
being asked about is the whole of the one point is in, because somebody
who wants to know what a name is has finished writing it and left point
wherever they left it - in the middle of it as often as at the end.

Nil where point is in no name at all.  A completion answers there with
the empty region it would write a candidate into, and an empty region is
not a name to look anything up by.

Nil too where point is on the reader macro in front of a name rather than
on the name.  What a quote is written in front of is not part of the name
- see `replique-name--start\=' - so point there is point in front of a
name and not in one, which is what a completion reads it as as well."
  (let* ((state (syntax-ppss))
         (string (and (nth 3 state) (nth 8 state)))
         (bounds (bounds-of-thing-at-point 'symbol)))
    (cond
     (string (when-let* ((after (ignore-errors (scan-sexps string 1)))
                         ((> (1- after) (1+ string))))
               (cons (1+ string) (1- after))))
     (bounds (let ((start (replique-name--start (car bounds) (cdr bounds))))
               (when (and (< start (cdr bounds)) (<= start (point)))
                 (cons start (cdr bounds)))))
     (t nil))))

(defun replique-name-bound-at (text)
  "Return where TEXT is bound where point is, as a marker, or nil.

Which only this side can say, and can say without asking anything: a
local is bound by a form written in this buffer, and where that form is
is what a jump to the definition of that name is a jump to.

The nearest binding, since that is the one that shadows the rest - what
the name means where it is written is what it was last bound to."
  (when-let* ((bound (cdr (assoc text
                                 (replique-locals-at
                                  (point)
                                  (replique-forms-for (replique-name-process)
                                                      (replique-name-namespace)))))))
    (copy-marker bound)))

(defun replique-name-call-at-point ()
  "Return the call point is inside, or nil when point is inside none.

A plist holding :text, the name the enclosing list calls; :context, what
to ask about that name; and :argument, which argument point is at - 1 at
the first, 2 at the second, and so on.

Which is a different question from the name at point, and it is the one
an arglist answers.  Point is nowhere in particular while somebody writes
the arguments of a call - after a space, between two forms, at the end of
a line - and what is worth being told there is what the call takes and
how far along it they are.

Which argument that is, and what the call is, is what
`replique-name--enclosing\=' reads - the same reading a completion asks
with, since what point is writing an argument of is one question however
many things want the answer.

Nil while point is still on the head, where nothing has been written to
be an argument of yet.  Nil in a comment, and nil where the head is not a
name - what ((f x) y) calls is not something to ask about.

Answered inside a string, where a completion has an answer of its own:
what a call takes is worth saying while any of its arguments is being
written, and a path is an argument like the rest of them."
  (let ((state (syntax-ppss)))
    (unless (or (nth 4 state) (not (replique-name--clojure-p)))
      (when-let* ((enclosing (replique-name--enclosing))
                  (text (plist-get enclosing :call))
                  (argument (plist-get enclosing :argument))
                  ((> argument 0)))
        ;; the namespace outside the when-let, since a buffer that names
        ;; none is a buffer this still answers in - what it is read against
        ;; then is what the process reads an absent one as
        (let ((ns (replique-name-namespace)))
          (list :text text
                :argument argument
              ;; What the head is called on, where the head is a member -
              ;; the s of (.length s).  Read here rather than left to
              ;; `replique-name--member-target\=', which answers for the
              ;; name being written and not for the one around it
                :context (replique-name--asking
                          ns
                          (when (string-prefix-p "." text)
                            (plist-get enclosing :on))
                          (replique-forms-for (replique-name-process) ns))))))))

(provide 'replique-name)

;;; replique-name.el ends here
