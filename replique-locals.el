;;; replique-locals.el --- What a symbol is bound by  -*- lexical-binding: t; -*-

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

;; Which names are locals where, and where each of them is bound.
;;
;; The grammar replique reads Clojure with is the reader's grammar: it tells
;; a string from a comment and a vector from a list, and it knows nothing of
;; `let'.  Nothing in a parse says that the x in (let [x 1] x) is not a var.
;; Every tool that asks the process about a symbol needs that difference,
;; because a process asked about x answers about whatever var is named x -
;; a wrong answer rather than a missing one.
;;
;; So this is the part that is Clojure rather than syntax: which forms bind,
;; and what each of them binds where.
;;
;; Scope is read from where things are rather than tracked while walking:
;; a form binds a name at POS when POS is past the point where that binding
;; takes effect and inside the form.  Which is what makes the two scoping
;; rules a comparison each.  Bindings are sequential in `let' and what is
;; shaped like it - in (let [x 1 y x] ...) the x of y is this let\='s, and in
;; (let [x x] ...) it is not, it is whatever x was outside.  They are
;; parallel in a parameter vector, where all of them take effect at once.
;;
;; What comes back is nearest first: the innermost form before the one
;; around it, and within a form the last binding before the ones before it.
;; So `assoc' finds the binding that shadows, and nothing has to be removed
;; from the list to get shadowing right.
;;
;; The forms outside the table below bind nothing here yet, which is the safe
;; direction to be wrong in: a local that goes unnoticed is asked about and
;; not found, where a var wrongly called a local is a symbol nothing will
;; describe.
;;
;; In three places it says a little more than Clojure would, and each of them
;; is left as it is on purpose.  The name an if-let or a when-let gives is in
;; scope in the branch that is taken and not in the other, and here it is in
;; scope in both - which branch a point is in is a question about what the
;; form means rather than about where the text is.  A form written behind a
;; #_ binds what it would have bound, which is what the rest of replique does
;; with one: what was commented out is still code, and evaluating it is the
;; way back from having commented it out.  And a let written inside quoted
;; data binds, where nothing would be evaluated at all.
;;
;; All three cost the same thing, which is a name called a local that nothing
;; binds - a tool that says nothing about it rather than one that says
;; something untrue.
;;
;; What is in scope inside a string or behind a semicolon is what is in scope
;; around it, and that is not a mistake: the locals of a body are the locals
;; of the whole of it.  Whether there is a name at that point to be asked
;; about at all is a different question, and not this one.

;;; Code:

(require 'treesit)
(require 'replique-clojure-mode)

(defconst replique-locals--let-like
  '("let" "loop" "when-let" "if-let" "when-some" "if-some"
    "with-open" "with-local-vars" "dotimes")
  "The forms that bind pairwise, in a vector, sequentially.

`binding\=' is not one of them.  It rebinds vars rather than binding
locals, and a var of its own name is exactly what the tooling should go
on describing inside it.")

(defconst replique-locals--for-like
  '("for" "doseq")
  "The forms that bind pairwise, with modifiers written among the pairs.")

(defconst replique-locals--fn-like
  '("fn" "defn" "defn-" "defmacro")
  "The forms that bind a parameter vector, one per arity.")

(defconst replique-locals--letfn-like
  '("letfn")
  "The forms that bind names to the functions written beside them.")

(defconst replique-locals--defmethod-like
  '("defmethod")
  "The forms that bind a parameter vector written after a dispatch value.")

(defconst replique-locals--deftype-like
  '("deftype" "defrecord")
  "The forms that bind a vector of fields and then methods.

The only forms here whose methods are not closures, which is why the
walk stops at one - see `replique-locals--closes-over-p\='.")

(defconst replique-locals--method-like
  '("reify" "proxy" "extend-type" "extend-protocol")
  "The forms that bind nothing of their own and hold methods.")

(defconst replique-locals--named-third
  '("catch" "as->")
  "The forms that name what the rest of them is written about, third.")

(defconst replique-locals--self-naming
  '("fn")
  "The forms of `replique-locals--fn-like\=' that also bind their own name.

The name of an (fn name [x] ...) is a local, which is how it calls
itself.  The name of a defn is a var, and a var is what the process
should be asked about.")

(defun replique-locals--head-name (node)
  "Return the name of the form NODE is, or nil when it is not one.

A form is a list whose head is a symbol qualified by nothing or by
`clojure.core\=', which is how it is written where `clojure.core\=' is not
referred - the same rule `replique-eval--ns-form-name\=' reads `in-ns\=' by.
A head reached through an alias is not recognised."
  (when (replique-clojure--list-node-p node)
    (let ((head (replique-clojure--first-value-child node)))
      (when (and head (replique-clojure--symbol-node-p head))
        (let ((qualifier (treesit-node-child-by-field-name head "namespace")))
          (when (or (null qualifier)
                    (equal "clojure.core" (treesit-node-text qualifier t)))
            (replique-clojure--named-node-text head)))))))

(defun replique-locals--vector-node-p (node)
  "Return non-nil when NODE is a vector."
  (and node (equal "vector_literal" (treesit-node-type node))))

(defun replique-locals--name-text (node)
  "Return the name of the symbol or keyword NODE, without its namespace."
  (treesit-node-text (treesit-node-child-by-field-name node "name") t))

(defun replique-locals--pattern-bound (node)
  "Return what the binding pattern NODE binds, last first.

Last first because a pattern can name the same thing twice, and the one
that counts is the last of them - which is the order the whole answer is
in, so that `assoc\=' finds what shadows.

A pattern is a name, or a vector or map to be taken apart, and either of
those can hold another pattern - so this and the two below call each
other for as deep as the pattern goes.  Anything else binds nothing: a
number or a string can be written where a pattern goes, and what it
would bind has no name."
  (let ((node (replique-clojure--unwrap-meta node)))
    (cond
     ((null node) nil)
     ((replique-clojure--symbol-node-p node)
      (list (cons (treesit-node-text node t) (treesit-node-start node))))
     ((replique-locals--vector-node-p node)
      (replique-locals--vector-pattern-bound node))
     ((equal "map_literal" (treesit-node-type node))
      (replique-locals--map-pattern-bound node))
     ;; #:person{:keys [a]} - the namespace says what the keys are called
     ;; and not what anything is bound to, so what is read is the map
     ((equal "namespaced_map_literal" (treesit-node-type node))
      (replique-locals--pattern-bound
       (treesit-node-child-by-field-name node "body")))
     (t nil))))

(defun replique-locals--vector-pattern-bound (node)
  "Return what the vector pattern NODE binds, last first.

The & of a rest argument is written as a symbol and names none of them:
what is bound is the pattern after it, which is read where it is written
like any other.  Nothing is said here about the :as of a vector either -
it is a keyword, which binds nothing, and the name it is written before
is bound by being that name."
  (let ((found nil))
    (dolist (child (treesit-node-children node t))
      (let ((child (replique-clojure--unwrap-meta child)))
        (unless (and (replique-clojure--symbol-node-p child)
                     (equal "&" (treesit-node-text child t)))
          (setq found (append (replique-locals--pattern-bound child) found)))))
    found))

(defun replique-locals--map-pattern-bound (node)
  "Return what the map pattern NODE binds, last first.

A map pattern is written the other way round from the map it takes
apart: what is bound is the key and where to find it is the value.
Except for the four keywords that are written as keys - :keys, :strs and
:syms, where the names are in the vector they are given, and :as, where
the name of the whole is."
  (let ((found nil))
    (dolist (pair (treesit-node-children node t))
      (when (equal "pair" (treesit-node-type pair))
        (let ((key (replique-clojure--unwrap-meta
                    (treesit-node-child-by-field-name pair "key")))
              (value (treesit-node-child-by-field-name pair "value")))
          (if (replique-clojure--keyword-node-p key)
              (let ((name (replique-locals--name-text key)))
                (cond
                 ((member name '("keys" "strs" "syms"))
                  (setq found (append (replique-locals--keys-bound value) found)))
                 ((equal name "as")
                  (setq found (append (replique-locals--pattern-bound value) found)))
                 ;; :or binds nothing.  The names it holds are bound by the
                 ;; :keys beside it, and what they are written against are
                 ;; the expressions to use where the map holds nothing
                 (t nil)))
            (setq found (append (replique-locals--pattern-bound key) found))))))
    found))

(defun replique-locals--keys-bound (node)
  "Return what the :keys, :strs or :syms vector NODE binds, last first.

Written as symbols or as keywords, and qualified or not.  What is bound
is the name either way: {:keys [foo/bar]} binds bar, and where it is
looked for is the rest of it."
  (let ((node (replique-clojure--unwrap-meta node))
        (found nil))
    (when (replique-locals--vector-node-p node)
      (dolist (child (treesit-node-children node t))
        (let ((child (replique-clojure--unwrap-meta child)))
          (when (or (replique-clojure--symbol-node-p child)
                    (replique-clojure--keyword-node-p child))
            (push (cons (replique-locals--name-text child)
                        (treesit-node-start child))
                  found)))))
    found))

(defun replique-locals--pairs-bound (vec pos)
  "Return what the pairs of the binding vector VEC bind that is in scope at POS.

A binding is in scope once the expression it is bound to has been read,
which is what makes these sequential: it covers the expressions after
its own and the body, and not its own."
  (let ((vec (replique-clojure--unwrap-meta vec))
        (found nil))
    (when (replique-locals--vector-node-p vec)
      (let ((children (treesit-node-children vec t)))
        (while (cdr children)
          (let ((target (car children))
                (init (nth 1 children)))
            (when (<= (treesit-node-end init) pos)
              (setq found (append (replique-locals--pattern-bound target) found))))
          (setq children (cddr children)))))
    found))

(defun replique-locals--binding-vector (node)
  "Return the vector NODE is written with, which is its second element."
  (nth 1 (treesit-node-children node t)))

(defun replique-locals--for-bound (node pos)
  "Return what the `for\='-like form NODE binds that is in scope at POS.

Written as pairs like a `let\=', with modifiers among them.  What follows
a :let is a binding vector of its own, which is the whole of what is
read differently here.  A :when or a :while needs nothing said about it:
the keyword is written where a name goes, and a keyword names nothing,
so the expression after it falls where an expression falls and the pairs
after it stay where they are."
  (let ((vec (replique-clojure--unwrap-meta
              (replique-locals--binding-vector node)))
        (found nil))
    (when (replique-locals--vector-node-p vec)
      (let ((children (treesit-node-children vec t)))
        (while (cdr children)
          (let* ((target (replique-clojure--unwrap-meta (car children)))
                 (init (nth 1 children)))
            (if (and (replique-clojure--keyword-node-p target)
                     (equal "let" (replique-locals--name-text target)))
                (setq found (append (replique-locals--pairs-bound init pos) found))
              (when (<= (treesit-node-end init) pos)
                (setq found (append (replique-locals--pattern-bound target) found)))))
          (setq children (cddr children)))))
    found))

(defun replique-locals--arity-vector (node pos)
  "Return the parameter vector of the `fn\='-like NODE that covers POS.

The vector of a form written with one arity, and of the arity POS is in
where there are several.  Which one that is has to be looked for rather
than counted to: a defn can be written with a docstring and an attribute
map before its parameters, so there is no index the vector is at."
  (let ((children (cdr (treesit-node-children node t))))
    ;; the name, where there is one, and then what can be written between
    ;; the name and the parameters
    (when (and children (replique-clojure--symbol-node-p
                         (replique-clojure--unwrap-meta (car children))))
      (setq children (cdr children)))
    (while (and children
                (member (treesit-node-type
                         (replique-clojure--unwrap-meta (car children)))
                        '("string" "map_literal")))
      (setq children (cdr children)))
    (let ((first (and children (replique-clojure--unwrap-meta (car children)))))
      (if (replique-locals--vector-node-p first)
          ;; One arity: the parameters cover everything written after them,
          ;; which leaves out a docstring, and they cover themselves
          (and (<= (treesit-node-start first) pos) first)
        ;; Several: the parameters of one arity are not in scope in another
        (let ((found nil))
          (dolist (child children)
            (let ((child (replique-clojure--unwrap-meta child)))
              (when (and (replique-clojure--list-node-p child)
                         (<= (treesit-node-start child) pos)
                         (< pos (treesit-node-end child)))
                (let ((vec (replique-clojure--first-value-child child)))
                  (when (replique-locals--vector-node-p vec)
                    (setq found vec))))))
          found)))))

(defun replique-locals--fn-bound (node pos self-naming)
  "Return what the `fn\='-like form NODE binds that is in scope at POS.

SELF-NAMING says whether the name it may be written with is a local -
see `replique-locals--self-naming\='."
  (let ((found nil))
    (when self-naming
      (let ((name (replique-clojure--unwrap-meta
                   (nth 1 (treesit-node-children node t)))))
        (when (replique-clojure--symbol-node-p name)
          (setq found (replique-locals--pattern-bound name)))))
    ;; in front of the name it may have: the parameters shadow it
    (when-let* ((vec (replique-locals--arity-vector node pos)))
      (setq found (append (replique-locals--vector-pattern-bound vec) found)))
    found))

(defun replique-locals--method-bound (node pos)
  "Return the parameters of the method of NODE that POS is written in.

A method is written as a name and a parameter vector, which is the shape
of one arity of a fn - so what it binds is read the same way.  The this
of a `reify\=' or a `deftype\=' is a local like the others by being written
where a parameter is, and needs nothing said about it here."
  (let ((found nil))
    (dolist (child (treesit-node-children node t))
      (let ((child (replique-clojure--unwrap-meta child)))
        (when (and (replique-clojure--list-node-p child)
                   (<= (treesit-node-start child) pos)
                   (< pos (treesit-node-end child)))
          (when-let* ((vec (replique-locals--arity-vector child pos)))
            (setq found (append (replique-locals--vector-pattern-bound vec) found))))))
    found))

(defun replique-locals--letfn-bound (node pos)
  "Return what the `letfn\=' form NODE binds that is in scope at POS.

The names it gives are in scope in the whole of it, one another
included, which is what it is written for.  The parameters of one of
them are in scope in that one only."
  (let ((vec (replique-clojure--unwrap-meta
              (replique-locals--binding-vector node)))
        (names nil)
        (params nil))
    (when (replique-locals--vector-node-p vec)
      (dolist (child (treesit-node-children vec t))
        (let ((child (replique-clojure--unwrap-meta child)))
          (when (replique-clojure--list-node-p child)
            (setq names (append (replique-locals--pattern-bound
                                 (replique-clojure--first-value-child child))
                                names))
            (when (and (<= (treesit-node-start child) pos)
                       (< pos (treesit-node-end child)))
              (when-let* ((arity (replique-locals--arity-vector child pos)))
                (setq params (append (replique-locals--vector-pattern-bound arity)
                                     params))))))))
    ;; the parameters of one of them shadow the names of all of them
    (append params names)))

(defun replique-locals--deftype-bound (node pos)
  "Return what the `deftype\='-like form NODE binds that is in scope at POS.

The fields are in scope in every method it is written with, and each
method binds what it is written with of its own."
  (let ((children (cdr (treesit-node-children node t)))
        (found nil))
    (when (and children (replique-clojure--symbol-node-p
                         (replique-clojure--unwrap-meta (car children))))
      (setq children (cdr children)))
    (let ((fields (and children (replique-clojure--unwrap-meta (car children)))))
      (when (and (replique-locals--vector-node-p fields)
                 (<= (treesit-node-start fields) pos))
        (setq found (replique-locals--vector-pattern-bound fields))))
    (append (replique-locals--method-bound node pos) found)))

(defun replique-locals--defmethod-bound (node pos)
  "Return what the `defmethod\=' form NODE binds that is in scope at POS.

Counted to rather than looked for, unlike a fn: what is written between
the name of the multimethod and the parameters is the value dispatched
on, and that can be a vector - so the first vector is not the one."
  (let* ((children (nthcdr 3 (treesit-node-children node t)))
         (params (and children (replique-clojure--unwrap-meta (car children)))))
    (when (and (replique-locals--vector-node-p params)
               (<= (treesit-node-start params) pos))
      (replique-locals--vector-pattern-bound params))))

(defun replique-locals--third-bound (node pos)
  "Return the name NODE gives third, when POS is past where it is given.

\(catch Exception e ...) and (as-> expr name ...) are written the same
way: two things, and then a name for what the rest of them is about."
  (let ((name (nth 2 (treesit-node-children node t))))
    (when (and name (<= (treesit-node-end name) pos))
      (replique-locals--pattern-bound name))))

(defconst replique-locals--implicit-parameter-regexp
  (rx bos "%" (opt (or "&" (one-or-more digit))) eos)
  "A regexp matching the parameters a #() gives no name to.")

(defun replique-locals--implicit-parameters (node)
  "Return the % parameters written anywhere in NODE, in order, once each.

Everything below NODE is looked at.  A #() cannot be written inside
another, so nothing found down there is somebody else\='s."
  (let ((found nil))
    (dolist (child (treesit-node-children node t))
      (dolist (name (if (replique-clojure--symbol-node-p child)
                        (let ((text (treesit-node-text child t)))
                          (and (string-match-p
                                replique-locals--implicit-parameter-regexp text)
                               (list text)))
                      (replique-locals--implicit-parameters child)))
        (unless (member name found)
          (setq found (append found (list name))))))
    found))

(defun replique-locals--fn-literal-bound (node)
  "Return what the #() NODE binds.

They are bound by being written rather than named, so what is in scope
is what is there: a % that has not been written is not offered, and
where each of them is bound is the #( they are written in."
  (let ((start (treesit-node-start node))
        (found nil))
    (dolist (name (replique-locals--implicit-parameters node))
      (push (cons name start) found))
    found))

(defun replique-locals--bound-by (node pos)
  "Return what NODE binds that is in scope at POS, nearest first."
  (if (replique-clojure--anon-fn-node-p node)
      (replique-locals--fn-literal-bound node)
    (let ((name (replique-locals--head-name node)))
      (cond
       ((member name replique-locals--let-like)
        (replique-locals--pairs-bound (replique-locals--binding-vector node) pos))
       ((member name replique-locals--for-like)
        (replique-locals--for-bound node pos))
       ((member name replique-locals--fn-like)
        (replique-locals--fn-bound
         node pos (and (member name replique-locals--self-naming) t)))
       ((member name replique-locals--letfn-like)
        (replique-locals--letfn-bound node pos))
       ((member name replique-locals--defmethod-like)
        (replique-locals--defmethod-bound node pos))
       ((member name replique-locals--deftype-like)
        (replique-locals--deftype-bound node pos))
       ((member name replique-locals--method-like)
        (replique-locals--method-bound node pos))
       ((member name replique-locals--named-third)
        (replique-locals--third-bound node pos))
       (t nil)))))

(defun replique-locals--enclosing (pos)
  "Return the forms POS is written inside, innermost first.

Read upwards from the node at POS rather than downwards from the root,
which comes to the same forms and does not need the parse to be asked
for.  The ones that do not hold POS are dropped: `treesit-node-at\='
answers with the node after POS where nothing covers it, and what is
after POS is not what POS is inside of."
  (let ((node (treesit-node-at pos))
        (found nil))
    (while node
      (when (and (<= (treesit-node-start node) pos)
                 (< pos (treesit-node-end node)))
        (push node found))
      (setq node (treesit-node-parent node)))
    (nreverse found)))

(defun replique-locals--closes-over-p (node)
  "Say whether what is written inside NODE can see the locals around it.

Almost everything can, and the exception is `deftype\=' and `defrecord\='.
Their methods are compiled to methods of a class, and a class has
nowhere to keep what was around it - so a name bound outside one is not
a local inside it, it is a name Clojure refuses to compile a use of.  A
`reify\=' or a `proxy\=' is a closure and is not one of these, which is
why they are read as ordinary forms on the way out.

Asked of every form POS is inside rather than of the binding ones only:
what a `deftype\=' does to the ones around it, it does whether or not it
binds anything at POS itself."
  (not (member (replique-locals--head-name node)
               replique-locals--deftype-like)))

(defun replique-locals-at (pos)
  "Return the locals in scope at POS as (NAME . POSITION), nearest first.

POSITION is where the name is bound, which is where a client that jumps
to a definition jumps to.  Nearest first means `assoc\=' answers with the
binding that shadows the rest, and `cdr\=' with where that one of them is.

A name bound twice is in the answer twice.  What the parse says is left
in rather than tidied away, since nothing else can tell that a binding
was shadowed - so showing the names to somebody wants `delete-dups\='
over them.

Nothing written around a `deftype\=' or a `defrecord\=' is in scope inside
one, so the walk out stops there - see `replique-locals--closes-over-p\='.

Read from the whole of the buffer rather than from what a narrowing left
reachable: a form is inside what it is written inside whether or not that
is on screen, and a narrowing below a `let\=' would otherwise make the
names it binds stop being locals.  Which is the wrong way round to be
wrong - a name not known to be a local is asked about, and answered with
whatever var happens to be called that.

A buffer with no Clojure parse has nothing written around anything, and
the answer is that nothing is in scope.  Whether it was a buffer worth
asking is the caller\='s to know."
  (save-restriction
    (widen)
    (let ((nodes (replique-locals--enclosing pos))
          (found nil)
          (closes-over t))
      (while (and nodes closes-over)
        (let ((node (pop nodes)))
          (setq found (append found (replique-locals--bound-by node pos)))
          (setq closes-over (replique-locals--closes-over-p node))))
      found)))

(defun replique-locals--pairs-naming (vec)
  "Return the names the binding vector VEC gives, wherever they are.

Every other element of it, and every name in the pattern each of those
is - which is `replique-locals--pairs-bound\=' with nothing said about
scope.  That is the whole of the difference, and it is the point: a name
is being given exactly where scope has not reached it yet.

An element with nothing after it counts like the rest.  A binding vector
is written a target at a time, and a target with no init yet is what a
name half typed looks like."
  (let ((vec (replique-clojure--unwrap-meta vec))
        (found nil))
    (when (replique-locals--vector-node-p vec)
      (let ((children (treesit-node-children vec t)))
        (while children
          (setq found (append (replique-locals--pattern-bound (car children)) found))
          (setq children (cddr children)))))
    found))

(defun replique-locals--for-naming (node)
  "Return the names the `for\='-like form NODE gives, wherever they are.

What follows a :let is a binding vector of its own, read here the way
`replique-locals--for-bound\=' reads it."
  (let ((vec (replique-clojure--unwrap-meta
              (replique-locals--binding-vector node)))
        (found nil))
    (when (replique-locals--vector-node-p vec)
      (let ((children (treesit-node-children vec t)))
        (while children
          (let ((target (replique-clojure--unwrap-meta (car children))))
            (if (and (replique-clojure--keyword-node-p target)
                     (equal "let" (replique-locals--name-text target)))
                (setq found (append (replique-locals--pairs-naming (nth 1 children))
                                    found))
              (setq found (append (replique-locals--pattern-bound target) found))))
          (setq children (cddr children)))))
    found))

(defun replique-locals--named (node)
  "Return the name NODE gives itself, as a list of one, or nil.

Its second element, when that is a symbol."
  (let ((name (replique-clojure--unwrap-meta
               (nth 1 (treesit-node-children node t)))))
    (when (replique-clojure--symbol-node-p name)
      (replique-locals--pattern-bound name))))

(defun replique-locals--naming-by (node)
  "Return the names NODE gives that are not in scope where they are written.

Two kinds of name are missing from what `replique-locals-at\=' answers,
and both of them on purpose.  A sequential binding is not in scope at its
own target - in (let [x x] ...) the second x is the one from outside - so
a point at the first x is a point at a name being given and at nothing
that is in scope.  And the name of a `defn\=' or a `deftype\=' is a var or
a class rather than a local, so nothing binds it anywhere.

Everything else is already answered where it is written.  A parameter, a
field, a `letfn\=' name, what a catch caught: all of them take effect at
the start of what they are written in, which is in front of themselves.
The name of a `fn\=' is read here as well as there - it is a local and it
is answered twice, which two of the same name is the right answer to."
  (let ((name (replique-locals--head-name node)))
    (cond
     ((member name replique-locals--let-like)
      (replique-locals--pairs-naming (replique-locals--binding-vector node)))
     ((member name replique-locals--for-like)
      (replique-locals--for-naming node))
     ((or (member name replique-locals--fn-like)
          (member name replique-locals--deftype-like))
      (replique-locals--named node))
     (t nil))))

(defun replique-locals--name-node-at (probe pos)
  "Return the symbol or keyword written at PROBE, when POS is in it.

PROBE is where to look and POS is what the answer has to cover, and they
are two because `treesit-node-at\=' answers for a position inside a token
rather than for one at either of its edges.  POS is in a name from where
the name starts to just after it ends, that last being where point is
once a name has been typed and nothing else has."
  (let ((node (treesit-node-at probe))
        (found nil))
    (while (and node (null found))
      (when (and (or (replique-clojure--symbol-node-p node)
                     (replique-clojure--keyword-node-p node))
                 (<= (treesit-node-start node) pos)
                 (<= pos (treesit-node-end node)))
        (setq found node))
      (setq node (treesit-node-parent node)))
    found))

(defun replique-locals--name-node (pos)
  "Return the symbol or keyword POS is at, or nil.

Looked for at POS and then at the character before it, since
`treesit-node-at\=' answers with what follows POS where nothing covers it
- and what POS is just after is a name POS is at, where what follows POS
is not."
  (or (replique-locals--name-node-at pos pos)
      (and (> pos (point-min))
           (replique-locals--name-node-at (1- pos) pos))))

(defun replique-locals--at-name-p (start names)
  "Say whether one of NAMES is the name written at START.

Each of NAMES is a (NAME . POSITION), and the two are compared by where
they are rather than by what they say.  A name and the text it is written
as are not the same length: what {:keys [foo/bar]} binds is bar, and
where it binds it is the start of foo/bar - so a name plus its length
covers the namespace and stops before the name."
  (and (rassoc start names) t))

(defun replique-locals-at-binding-position-p (pos)
  "Say whether POS is where a name is being given rather than used.

The point in (let [x| 1] x) and in (defn f|oo [] 1), and not the point in
\(let [x 1] x|).  What is being written there is a new name, so nothing
knows it yet and nothing should be offered for it: a completion at a
binding position can only offer something the name is not.

Read as the names bound around POS rather than as the shapes a form is
written in, so that a name in a destructuring is one of these wherever it
is nested, and the default after an :or is not one - it is an expression
written where an expression goes.

A name is at POS or it is not, all of it: the whole of the foo/bar in
{:keys [foo/bar]} is a name being given, though only the bar of it is
the name that it gives.

Reads the whole of the buffer, for the reason `replique-locals-at\=' does."
  (save-restriction
    (widen)
    (when-let* ((node (replique-locals--name-node pos))
                (start (treesit-node-start node)))
      (or (replique-locals--at-name-p start (replique-locals-at pos))
          (let ((nodes (replique-locals--enclosing pos))
                (found nil))
            (while (and nodes (not found))
              (setq found (replique-locals--at-name-p
                           start (replique-locals--naming-by (pop nodes)))))
            found)))))

(provide 'replique-locals)

;;; replique-locals.el ends here
