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
;; shaped like it - in (let [x 1 y x] ...) the x of y is this let's, and in
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

(require 'replique-parse)

(defconst replique-locals-vars
  '((let-like "clojure.core/let" "clojure.core/loop"
              "clojure.core/when-let" "clojure.core/if-let"
              "clojure.core/when-some" "clojure.core/if-some"
              "clojure.core/with-open" "clojure.core/with-local-vars"
              "clojure.core/dotimes")
    (for-like "clojure.core/for" "clojure.core/doseq")
    (defn-like "clojure.core/defn" "clojure.core/defn-" "clojure.core/defmacro")
    (fn-like "clojure.core/fn")
    (letfn-like "clojure.core/letfn")
    (defmethod-like "clojure.core/defmethod")
    (deftype-like "clojure.core/deftype" "clojure.core/defrecord")
    (method-like "clojure.core/reify" "clojure.core/proxy"
                 "clojure.core/extend-type" "clojure.core/extend-protocol")
    (named-third "clojure.core/as->"))
  "The vars that bind, by what each of them binds.

let-like binds pairwise, in a vector, sequentially.  for-like binds
pairwise with modifiers written among the pairs.  defn-like binds a
parameter vector, one per arity, and fn-like binds its own name as well:
the name of an (fn name [x] ...) is a local and is how it calls itself,
where the name of a defn is a var and a var is what the process should
be asked about.  letfn-like binds names to the functions written beside
them.  defmethod-like binds a parameter vector written
after a dispatch value.  deftype-like binds a vector of fields and then
methods, and is the only kind whose methods are not closures - which is
why the walk out stops at one, see `replique-locals--closes-over-p'.
method-like binds nothing of its own and holds methods.  named-third
names what the rest of the form is written about, third.

`binding' is not one of them.  It rebinds vars rather than binding
locals, and a var of its own name is exactly what the tooling should go
on describing inside it.

Named qualified because that is what is asked about them: which symbol a
namespace writes clojure.core/let as is a question only a process can
answer - see `replique-locals-forms'.

What each of them binds is named after the form it is shaped like, which
is what somebody with a macro of their own would reach for: a macro that
takes a binding vector is let-like whatever it is called.  Those names
are the vocabulary a process would declare a var in - see the protocol,
under what a namespace calls a var - so they are chosen to be written by
somebody who knows the shape of their macro and nothing about this.")

(defconst replique-locals-special-forms
  '((named-third "catch"))
  "The forms that bind and are not vars, by what each of them binds.

A special form is read by the compiler rather than resolved, so no
namespace writes it differently and there is nothing to ask about one:
catch is catch everywhere.  It cannot be aliased, referred under another
name, or shadowed by a var of its own name.")

(defun replique-locals--simple-name (var)
  "Return the qualified name VAR without its namespace."
  (if (string-match "/\\(.+\\)\\'" var) (match-string 1 var) var))

(defun replique-locals-forms (&optional spellings)
  "Return what each form that binds is called, as an alist of (WRITTEN . KIND).

SPELLINGS says how one namespace writes the vars of
`replique-locals-vars': an alist of the qualified name of each to the
names that namespace can write it as.  It is what the :spellings op
answers, and it is the half of this that only a process has - a namespace
that aliases clojure.core writes let as c/let, one that referred it under
another name writes it as that name, and one that excluded it and defined
its own writes let for a var that binds nothing at all.

Without it, the names of clojure.core as a namespace that refers them
plainly writes them.  Which is what they are called nearly everywhere,
and what has to be assumed of a namespace nothing has been asked
about."
  (append
   (mapcan (lambda (entry)
             (let ((kind (car entry)))
               (mapcan (lambda (var)
                         (mapcar (lambda (written) (cons written kind))
                                 (or (cdr (assoc var spellings))
                                     (list (replique-locals--simple-name var) var))))
                       (cdr entry))))
           replique-locals-vars)
   (mapcan (lambda (entry)
             (let ((kind (car entry)))
               (mapcar (lambda (written) (cons written kind)) (cdr entry))))
           replique-locals-special-forms)))

(defconst replique-locals-default-forms (replique-locals-forms)
  "What each form that binds is called, nothing having been asked.

Which is the answer for every buffer with no process behind it.  Worked
out once, because rebuilding it per name looked at would be the same
list every time.")

(defsubst replique-locals--list-p (node)
  "Return non-nil when NODE is a list."
  (and node (eq 'list (replique-parse-type node))))

(defsubst replique-locals--fn-literal-p (node)
  "Return non-nil when NODE is a `#()'."
  (and node (eq 'fn (replique-parse-type node))))

(defsubst replique-locals--symbol-p (node)
  "Return non-nil when NODE is a symbol."
  (and node (eq 'symbol (replique-parse-type node))))

(defsubst replique-locals--keyword-p (node)
  "Return non-nil when NODE is a keyword."
  (and node (eq 'keyword (replique-parse-type node))))

(defun replique-locals--first-value (node)
  "Return the first form NODE is made of, with its metadata taken off."
  (replique-parse-unwrap-meta (car (replique-parse-forms node))))

(defun replique-locals--head-text (node)
  "Return the symbol at the head of the form NODE is, as it is written.

Nil where NODE is not a form.  As it is written, with whatever namespace
is on it, because that is what says which var it names: let and c/let and
clojure.core/let are three ways of writing one form and three different
symbols, and which of them a namespace writes is what
`replique-locals-forms' is given."
  (when (replique-locals--list-p node)
    (let ((head (replique-locals--first-value node)))
      (when (and head (replique-locals--symbol-p head))
        (replique-parse-text head)))))

(defun replique-locals--kind (node forms)
  "Return what the form NODE is binds, or nil for neither.

FORMS is what each written form binds - see `replique-locals-forms'."
  (when-let* ((written (replique-locals--head-text node)))
    (cdr (assoc written forms))))

(defun replique-locals--vector-node-p (node)
  "Return non-nil when NODE is a vector."
  (and node (eq 'vector (replique-parse-type node))))

(defun replique-locals--name-text (node)
  "Return the name of the symbol or keyword NODE, without its namespace."
  (cdr (replique-parse-name-parts (replique-parse-text node))))

(defun replique-locals--pattern-bound (node)
  "Return what the binding pattern NODE binds, last first.

Last first because a pattern can name the same thing twice, and the one
that counts is the last of them - which is the order the whole answer is
in, so that `assoc' finds what shadows.

A pattern is a name, or a vector or map to be taken apart, and either of
those can hold another pattern - so this and the two below call each
other for as deep as the pattern goes.  Anything else binds nothing: a
number or a string can be written where a pattern goes, and what it
would bind has no name."
  (let ((node (replique-parse-unwrap-meta node)))
    (cond
     ((null node) nil)
     ((replique-locals--symbol-p node)
      (list (cons (replique-parse-text node) (replique-parse-start node))))
     ((replique-locals--vector-node-p node)
      (replique-locals--vector-pattern-bound node))
     ((eq 'map (replique-parse-type node))
      (replique-locals--map-pattern-bound node))
     ;; #:person{:keys [a]} - the namespace says what the keys are called
     ;; and not what anything is bound to, so what is read is the map
     ((eq 'namespaced-map (replique-parse-type node))
      (replique-locals--pattern-bound (replique-parse-target node)))
     (t nil))))

(defun replique-locals--vector-pattern-bound (node)
  "Return what the vector pattern NODE binds, last first.

The & of a rest argument is written as a symbol and names none of them:
what is bound is the pattern after it, which is read where it is written
like any other.  Nothing is said here about the :as of a vector either -
it is a keyword, which binds nothing, and the name it is written before
is bound by being that name."
  (let ((found nil))
    (dolist (child (replique-parse-forms node))
      (let ((child (replique-parse-unwrap-meta child)))
        (unless (and (replique-locals--symbol-p child)
                     (equal "&" (replique-parse-text child)))
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
    (dolist (pair (replique-parse-forms node))
      (when (eq 'pair (replique-parse-type pair))
        (let* ((forms (replique-parse-forms pair))
               (key (replique-parse-unwrap-meta (car forms)))
               (value (cadr forms)))
          (if (replique-locals--keyword-p key)
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
  (let ((node (replique-parse-unwrap-meta node))
        (found nil))
    (when (replique-locals--vector-node-p node)
      (dolist (child (replique-parse-forms node))
        (let ((child (replique-parse-unwrap-meta child)))
          (when (or (replique-locals--symbol-p child)
                    (replique-locals--keyword-p child))
            (push (cons (replique-locals--name-text child)
                        (replique-parse-start child))
                  found)))))
    found))

(defun replique-locals--pairs-bound (vec pos)
  "Return what the pairs of the binding vector VEC bind that is in scope at POS.

A binding is in scope once the expression it is bound to has been read,
which is what makes these sequential: it covers the expressions after
its own and the body, and not its own."
  (let ((vec (replique-parse-unwrap-meta vec))
        (found nil))
    (when (replique-locals--vector-node-p vec)
      (let ((children (replique-parse-forms vec)))
        (while (cdr children)
          (let ((target (car children))
                (init (nth 1 children)))
            (when (<= (replique-parse-end init) pos)
              (setq found (append (replique-locals--pattern-bound target) found))))
          (setq children (cddr children)))))
    found))

(defun replique-locals--binding-vector (node)
  "Return the vector NODE is written with, which is its second element."
  (nth 1 (replique-parse-forms node)))

(defun replique-locals--for-bound (node pos)
  "Return what the `for'-like form NODE binds that is in scope at POS.

Written as pairs like a `let', with modifiers among them.  What follows
a :let is a binding vector of its own, which is the whole of what is
read differently here.  A :when or a :while needs nothing said about it:
the keyword is written where a name goes, and a keyword names nothing,
so the expression after it falls where an expression falls and the pairs
after it stay where they are."
  (let ((vec (replique-parse-unwrap-meta
              (replique-locals--binding-vector node)))
        (found nil))
    (when (replique-locals--vector-node-p vec)
      (let ((children (replique-parse-forms vec)))
        (while (cdr children)
          (let* ((target (replique-parse-unwrap-meta (car children)))
                 (init (nth 1 children)))
            (if (and (replique-locals--keyword-p target)
                     (equal "let" (replique-locals--name-text target)))
                (setq found (append (replique-locals--pairs-bound init pos) found))
              (when (<= (replique-parse-end init) pos)
                (setq found (append (replique-locals--pattern-bound target) found)))))
          (setq children (cddr children)))))
    found))

(defun replique-locals--arity-vector (node pos)
  "Return the parameter vector of the `fn'-like NODE that covers POS.

The vector of a form written with one arity, and of the arity POS is in
where there are several.  Which one that is has to be looked for rather
than counted to: a defn can be written with a docstring and an attribute
map before its parameters, so there is no index the vector is at."
  (let ((children (cdr (replique-parse-forms node))))
    ;; the name, where there is one, and then what can be written between
    ;; the name and the parameters
    (when (and children (replique-locals--symbol-p
                         (replique-parse-unwrap-meta (car children))))
      (setq children (cdr children)))
    (while (and children
                (memq (replique-parse-type
                       (replique-parse-unwrap-meta (car children)))
                      '(string map)))
      (setq children (cdr children)))
    (let ((first (and children (replique-parse-unwrap-meta (car children)))))
      (if (replique-locals--vector-node-p first)
          ;; One arity: the parameters cover everything written after them,
          ;; which leaves out a docstring, and they cover themselves
          (and (<= (replique-parse-start first) pos) first)
        ;; Several: the parameters of one arity are not in scope in another
        (let ((found nil))
          (dolist (child children)
            (let ((child (replique-parse-unwrap-meta child)))
              (when (and (replique-locals--list-p child)
                         (<= (replique-parse-start child) pos)
                         (< pos (replique-parse-end child)))
                (let ((vec (replique-locals--first-value child)))
                  (when (replique-locals--vector-node-p vec)
                    (setq found vec))))))
          found)))))

(defun replique-locals--fn-bound (node pos self-naming)
  "Return what the `fn'-like form NODE binds that is in scope at POS.

SELF-NAMING says whether the name it may be written with is a local -
which is what fn-like says in `replique-locals-vars'."
  (let ((found nil))
    (when self-naming
      (let ((name (replique-parse-unwrap-meta
                   (nth 1 (replique-parse-forms node)))))
        (when (replique-locals--symbol-p name)
          (setq found (replique-locals--pattern-bound name)))))
    ;; in front of the name it may have: the parameters shadow it
    (when-let* ((vec (replique-locals--arity-vector node pos)))
      (setq found (append (replique-locals--vector-pattern-bound vec) found)))
    found))

(defun replique-locals--method-bound (node pos)
  "Return the parameters of the method of NODE that POS is written in.

A method is written as a name and a parameter vector, which is the shape
of one arity of a fn - so what it binds is read the same way.  The this
of a `reify' or a `deftype' is a local like the others by being written
where a parameter is, and needs nothing said about it here."
  (let ((found nil))
    (dolist (child (replique-parse-forms node))
      (let ((child (replique-parse-unwrap-meta child)))
        (when (and (replique-locals--list-p child)
                   (<= (replique-parse-start child) pos)
                   (< pos (replique-parse-end child)))
          (when-let* ((vec (replique-locals--arity-vector child pos)))
            (setq found (append (replique-locals--vector-pattern-bound vec) found))))))
    found))

(defun replique-locals--letfn-bound (node pos)
  "Return what the `letfn' form NODE binds that is in scope at POS.

The names it gives are in scope in the whole of it, one another
included, which is what it is written for.  The parameters of one of
them are in scope in that one only."
  (let ((vec (replique-parse-unwrap-meta
              (replique-locals--binding-vector node)))
        (names nil)
        (params nil))
    (when (replique-locals--vector-node-p vec)
      (dolist (child (replique-parse-forms vec))
        (let ((child (replique-parse-unwrap-meta child)))
          (when (replique-locals--list-p child)
            (setq names (append (replique-locals--pattern-bound
                                 (replique-locals--first-value child))
                                names))
            (when (and (<= (replique-parse-start child) pos)
                       (< pos (replique-parse-end child)))
              (when-let* ((arity (replique-locals--arity-vector child pos)))
                (setq params (append (replique-locals--vector-pattern-bound arity)
                                     params))))))))
    ;; the parameters of one of them shadow the names of all of them
    (append params names)))

(defun replique-locals--deftype-bound (node pos)
  "Return what the `deftype'-like form NODE binds that is in scope at POS.

The fields are in scope in every method it is written with, and each
method binds what it is written with of its own."
  (let ((children (cdr (replique-parse-forms node)))
        (found nil))
    (when (and children (replique-locals--symbol-p
                         (replique-parse-unwrap-meta (car children))))
      (setq children (cdr children)))
    (let ((fields (and children (replique-parse-unwrap-meta (car children)))))
      (when (and (replique-locals--vector-node-p fields)
                 (<= (replique-parse-start fields) pos))
        (setq found (replique-locals--vector-pattern-bound fields))))
    (append (replique-locals--method-bound node pos) found)))

(defun replique-locals--defmethod-bound (node pos)
  "Return what the `defmethod' form NODE binds that is in scope at POS.

Counted to rather than looked for, unlike a fn: what is written between
the name of the multimethod and the parameters is the value dispatched
on, and that can be a vector - so the first vector is not the one."
  (let* ((children (nthcdr 3 (replique-parse-forms node)))
         (params (and children (replique-parse-unwrap-meta (car children)))))
    (when (and (replique-locals--vector-node-p params)
               (<= (replique-parse-start params) pos))
      (replique-locals--vector-pattern-bound params))))

(defun replique-locals--third-bound (node pos)
  "Return the name NODE gives third, when POS is past where it is given.

\(catch Exception e ...) and (as-> expr name ...) are written the same
way: two things, and then a name for what the rest of them is about."
  (let ((name (nth 2 (replique-parse-forms node))))
    (when (and name (<= (replique-parse-end name) pos))
      (replique-locals--pattern-bound name))))

(defconst replique-locals--implicit-parameter-regexp
  (rx bos "%" (opt (or "&" (one-or-more digit))) eos)
  "A regexp matching the parameters a #() gives no name to.")

(defun replique-locals--implicit-parameters (node)
  "Return the % parameters written anywhere in NODE, in order, once each.

Everything below NODE is looked at.  A #() cannot be written inside
another, so nothing found down there is somebody else's."
  (let ((found nil))
    (dolist (child (replique-parse-children node))
      (dolist (name (if (replique-locals--symbol-p child)
                        (let ((text (replique-parse-text child)))
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
  (let ((start (replique-parse-start node))
        (found nil))
    (dolist (name (replique-locals--implicit-parameters node))
      (push (cons name start) found))
    found))

(defun replique-locals--bound-by (node pos forms)
  "Return what NODE binds that is in scope at POS, nearest first.

FORMS says what each written form binds - see `replique-locals-forms'."
  (if (replique-locals--fn-literal-p node)
      (replique-locals--fn-literal-bound node)
    (let ((kind (replique-locals--kind node forms)))
      (cond
       ((eq kind 'let-like)
        (replique-locals--pairs-bound (replique-locals--binding-vector node) pos))
       ((eq kind 'for-like)
        (replique-locals--for-bound node pos))
       ((memq kind '(defn-like fn-like))
        (replique-locals--fn-bound node pos (eq kind 'fn-like)))
       ((eq kind 'letfn-like)
        (replique-locals--letfn-bound node pos))
       ((eq kind 'defmethod-like)
        (replique-locals--defmethod-bound node pos))
       ((eq kind 'deftype-like)
        (replique-locals--deftype-bound node pos))
       ((eq kind 'method-like)
        (replique-locals--method-bound node pos))
       ((eq kind 'named-third)
        (replique-locals--third-bound node pos))
       (t nil)))))

(defun replique-locals--enclosing (pos)
  "Return the forms POS is written inside, innermost first.

Read down from the top level form POS is in rather than up from the name
at POS.  A node does not hold what it is written inside - there is no
parent to walk to - and that is on purpose: a parent in every node is a
field written once per node and read only by the walks that go upwards,
and every one of those starts from a form it was handed.  This one is
handed it by `replique-parse-form-at', which is what the buffer has
already been read into.

Nothing has to be dropped from what comes back either.  What is written
over POS is what a walk down goes through, where a walk up from the node
after POS goes through forms POS is not inside of."
  (when-let* ((form (replique-parse-form-at pos)))
    (nreverse (replique-parse-path form pos))))

(defun replique-locals--closes-over-p (node forms)
  "Say whether what is written inside NODE can see the locals around it.

Almost everything can, and the exception is `deftype' and `defrecord'.
Their methods are compiled to methods of a class, and a class has
nowhere to keep what was around it - so a name bound outside one is not
a local inside it, it is a name Clojure refuses to compile a use of.  A
`reify' or a `proxy' is a closure and is not one of these, which is
why they are read as ordinary forms on the way out.

Asked of every form POS is inside rather than of the binding ones only:
what a `deftype' does to the ones around it, it does whether or not it
binds anything at POS itself.

FORMS says what each written form binds - see `replique-locals-forms'."
  (not (eq 'deftype-like (replique-locals--kind node forms))))

(defun replique-locals-tag-at (pos)
  "Return the type declared on the name written at POS, or nil.

Which is a ^Type written in front of it - (let [^String s ...] ...) says
every s below it holds a string, and it is the one thing a Clojure file
says about what a local holds.  POS is where the name is, which is what
`replique-locals-at\=' answers with, and it is where a name written
anywhere else is too: a ^String at the call site is written the same way.

A ^Symbol and nothing else.  ^{:tag String} means the same to the
compiler and is not read here - it is written where somebody wants to
say more than the type, and reading the type out of it is reading a map
whose other keys this knows nothing about.

Nil where nothing is declared, which is the usual answer: a name says
nothing about what it holds unless somebody wrote it down."
  (when-let* ((path (replique-locals--enclosing pos))
              (name (car path))
              (around (cadr path))
              ((eq 'meta (replique-parse-type around)))
              ;; written in front of the name itself, rather than in front
              ;; of something the name is written inside of
              ((eq name (replique-parse-target around)))
              (tag (car (replique-parse-forms around)))
              ((replique-locals--symbol-p tag)))
    (replique-parse-text tag)))

(defun replique-locals-at (pos &optional forms)
  "Return the locals in scope at POS as (NAME . POSITION), nearest first.

POSITION is where the name is bound, which is where a client that jumps
to a definition jumps to.  Nearest first means `assoc' answers with the
binding that shadows the rest, and `cdr' with where that one of them is.

A name bound twice is in the answer twice.  What the parse says is left
in rather than tidied away, since nothing else can tell that a binding
was shadowed - so showing the names to somebody wants `delete-dups'
over them.

Nothing written around a `deftype' or a `defrecord' is in scope inside
one, so the walk out stops there - see `replique-locals--closes-over-p'.

Read from the whole of the buffer rather than from what a narrowing left
reachable: a form is inside what it is written inside whether or not that
is on screen, and a narrowing below a `let' would otherwise make the
names it binds stop being locals.  Which is the wrong way round to be
wrong - a name not known to be a local is asked about, and answered with
whatever var happens to be called that.

A buffer with no Clojure parse has nothing written around anything, and
the answer is that nothing is in scope.  Whether it was a buffer worth
asking is the caller's to know.

FORMS says what each written form binds, `replique-locals-default-forms'
by default - see `replique-locals-forms' for what a process adds to it."
  (save-restriction
    (widen)
    (let ((forms (or forms replique-locals-default-forms))
          (nodes (replique-locals--enclosing pos))
          (found nil)
          (closes-over t))
      (while (and nodes closes-over)
        (let ((node (pop nodes)))
          (setq found (append found (replique-locals--bound-by node pos forms)))
          (setq closes-over (replique-locals--closes-over-p node forms))))
      found)))

(defun replique-locals--pairs-naming (vec)
  "Return the names the binding vector VEC gives, wherever they are.

Every other element of it, and every name in the pattern each of those
is - which is `replique-locals--pairs-bound' with nothing said about
scope.  That is the whole of the difference, and it is the point: a name
is being given exactly where scope has not reached it yet.

An element with nothing after it counts like the rest.  A binding vector
is written a target at a time, and a target with no init yet is what a
name half typed looks like."
  (let ((vec (replique-parse-unwrap-meta vec))
        (found nil))
    (when (replique-locals--vector-node-p vec)
      (let ((children (replique-parse-forms vec)))
        (while children
          (setq found (append (replique-locals--pattern-bound (car children)) found))
          (setq children (cddr children)))))
    found))

(defun replique-locals--for-naming (node)
  "Return the names the `for'-like form NODE gives, wherever they are.

What follows a :let is a binding vector of its own, read here the way
`replique-locals--for-bound' reads it."
  (let ((vec (replique-parse-unwrap-meta
              (replique-locals--binding-vector node)))
        (found nil))
    (when (replique-locals--vector-node-p vec)
      (let ((children (replique-parse-forms vec)))
        (while children
          (let ((target (replique-parse-unwrap-meta (car children))))
            (if (and (replique-locals--keyword-p target)
                     (equal "let" (replique-locals--name-text target)))
                (setq found (append (replique-locals--pairs-naming (nth 1 children))
                                    found))
              (setq found (append (replique-locals--pattern-bound target) found))))
          (setq children (cddr children)))))
    found))

(defun replique-locals--named (node)
  "Return the name NODE gives itself, as a list of one, or nil.

Its second element, when that is a symbol."
  (let ((name (replique-parse-unwrap-meta
               (nth 1 (replique-parse-forms node)))))
    (when (replique-locals--symbol-p name)
      (replique-locals--pattern-bound name))))

(defun replique-locals--naming-by (node forms)
  "Return the names NODE gives that are not in scope where they are written.

Two kinds of name are missing from what `replique-locals-at' answers,
and both of them on purpose.  A sequential binding is not in scope at its
own target - in (let [x x] ...) the second x is the one from outside - so
a point at the first x is a point at a name being given and at nothing
that is in scope.  And the name of a `defn' or a `deftype' is a var or
a class rather than a local, so nothing binds it anywhere.

Everything else is already answered where it is written.  A parameter, a
field, a `letfn' name, what a catch caught: all of them take effect at
the start of what they are written in, which is in front of themselves.
The name of a `fn' is read here as well as there - it is a local and it
is answered twice, which two of the same name is the right answer to.

FORMS says what each written form binds - see `replique-locals-forms'."
  (let ((kind (replique-locals--kind node forms)))
    (cond
     ((eq kind 'let-like)
      (replique-locals--pairs-naming (replique-locals--binding-vector node)))
     ((eq kind 'for-like)
      (replique-locals--for-naming node))
     ((memq kind '(defn-like fn-like deftype-like))
      (replique-locals--named node))
     (t nil))))

(defun replique-locals--name-node-at (probe pos)
  "Return the symbol or keyword written at PROBE, when POS is in it.

PROBE is where to look and POS is what the answer has to cover, and they
are two because a position is at a name from where it starts to just
after it ends, where what is read at a position is what covers it - and
just after a name is not covered by it.  That last is where point is
once a name has been typed and nothing else has."
  (let ((path (replique-locals--enclosing probe))
        (found nil))
    (while (and path (null found))
      (let ((node (pop path)))
        (when (and (or (replique-locals--symbol-p node)
                       (replique-locals--keyword-p node))
                   (<= (replique-parse-start node) pos)
                   (<= pos (replique-parse-end node)))
          (setq found node))))
    found))

(defun replique-locals--name-node (pos)
  "Return the symbol or keyword POS is at, or nil.

Looked for at POS and then at the character before it: nothing covers
the position just after a name, and a name POS is just after is a name
POS is at."
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

(defun replique-locals-at-binding-position-p (pos &optional forms)
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

Reads the whole of the buffer, for the reason `replique-locals-at' does.
FORMS is what that one takes, and means the same thing here."
  (save-restriction
    (widen)
    (let ((forms (or forms replique-locals-default-forms)))
      (when-let* ((node (replique-locals--name-node pos))
                  (start (replique-parse-start node)))
        (or (replique-locals--at-name-p start (replique-locals-at pos forms))
            (let ((nodes (replique-locals--enclosing pos))
                  (found nil))
              (while (and nodes (not found))
                (setq found (replique-locals--at-name-p
                             start (replique-locals--naming-by (pop nodes) forms))))
              found))))))

(provide 'replique-locals)

;;; replique-locals.el ends here
