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

(defconst replique-locals--deftype-like
  '("deftype" "defrecord")
  "The forms that bind a vector of fields and then methods.")

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
       ((equal name "letfn")
        (replique-locals--letfn-bound node pos))
       ((equal name "defmethod")
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

(defun replique-locals-at (pos)
  "Return the locals in scope at POS as (NAME . POSITION), nearest first.

POSITION is where the name is bound, which is where a client that jumps
to a definition jumps to.  Nearest first means `assoc\=' answers with the
binding that shadows the others, and that the answer is usable as it
comes: what to describe a name by, whether a name is a local at all, and
what names are in scope, are the same list read three ways."
  (let ((found nil))
    (dolist (node (replique-locals--enclosing pos))
      (setq found (append found (replique-locals--bound-by node pos))))
    found))

(provide 'replique-locals)

;;; replique-locals.el ends here
