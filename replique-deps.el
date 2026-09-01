;;; replique-deps.el --- Where point is in a require or an import  -*- lexical-binding: t; -*-

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

;; What a namespace says it depends on, and where in saying it point is.
;; The requires and the imports of an ns form, that is, and not deps.edn.
;;
;; Every slot of one of these forms is written in a different alphabet.
;; The namespace in (:require [clojure.st|]) is a name on the classpath,
;; which no var is; what follows a :refer is a var of that namespace and of
;; no other; what is written in an (:import (java.util Da|)) is a class of
;; that package.  A tool that offered the same names in all of them would be
;; wrong in all of them but one, and the answer here is which of them point
;; is in - so that the process is asked the question that slot is asking.
;;
;; The same clauses are written twice over: as (:require ...) inside an ns
;; form, and as (require '[...]) on its own, which is how a namespace is
;; loaded from a repl.  They mean the same thing and they are read the same
;; way, from a table of each spelling.
;;
;; What comes back is what the slot is, and what the process needs to answer
;; it: a namespace with the prefix it is written under, a var with the
;; namespace to look in, a class with its package.
;;
;; Read downwards from the form rather than forwards from its start, which
;; is what makes each slot a question about an index.  A spec is a name and
;; then options, so the first element of one is the namespace and the rest
;; are read against what precedes them: what follows a :refer is a vector of
;; vars, what follows an :as is a name being given and nothing to complete.
;; A spec whose elements are themselves specs is a prefix list, and the only
;; thing that carries down through the descent is the prefix it makes.
;;
;; One thing is left alone.  What follows a :rename is a map of one name to
;; another, and only the keys of it are vars - it is answered as nothing
;; rather than as vars, which is the safe way round.
;;
;; So is what follows a :refer-macros, for a reason worth writing down.  The
;; vars it names are macros of the Clojure namespace of that name, and there
;; is nothing in the answer that says which of the two worlds a var is from.
;; A namespace has :namespace-macros to say it with and a var has nothing,
;; so a var is answered as nothing until it has.

;;; Code:

(require 'treesit)
(require 'replique-clojure-mode)

(defconst replique-deps--clauses
  '(("require" . require) ("require-macros" . require-macros) ("use" . use)
    ("import" . import) ("refer-clojure" . refer-clojure) ("load" . load))
  "The clauses of an ns form, and what each of them is read as.")

(defconst replique-deps--calls
  '(("require" . require) ("require-macros" . require-macros) ("use" . use)
    ("import" . import) ("refer" . refer) ("refer-clojure" . refer-clojure)
    ("load" . load))
  "The calls that say what a clause says, and what each of them is read as.

`refer\=' is here and not among the clauses: an ns form has no :refer of
its own, and the one written in a libspec is an option of it.")

(defconst replique-deps--var-options
  '("refer" "only" "exclude")
  "The options of a spec that are written against a vector of vars.")

(defconst replique-deps--collections
  '("list_literal" "vector_literal" "map_literal" "set_literal")
  "The nodes that hold other nodes, which point is in rather than at.")

(defun replique-deps--unwrap (node)
  "Return what NODE is written around: its metadata and its quoting.

A spec is quoted where it is written as a call - (require \\='[a :as b]) -
and quoting says nothing about which slot is which."
  (let ((type (and node (treesit-node-type node))))
    (if (member type '("with_metadata" "quote" "syntax_quote"))
        (replique-deps--unwrap (treesit-node-child-by-field-name node "target"))
      node)))

(defun replique-deps--branch-at (node pos)
  "Return what the reader conditional NODE stands for at POS, or nil.

One is written as platforms and what each of them stands for, and the
one that counts is the one POS is written in - which platform is read
for is a question about what the form means rather than about where the
text is, and the branch POS is in is the branch being written.

The platforms are written where a key goes and what they stand for
where a value goes, so a branch is at an odd index and a platform at an
even one - and a point at a platform is at a platform, which is not a
dependency of anything.

A splicing one stands for several things at once, written in a vector,
and the one that counts is again the one POS is in."
  (let* ((body (treesit-node-child-by-field-name node "body"))
         (children (and body (treesit-node-children body t)))
         (index (and body (replique-deps--child-index-at body pos)))
         (branch (and index (= 1 (mod index 2)) (nth index children))))
    (when branch
      (if (equal "marker_splicing"
                 (treesit-node-type (treesit-node-child-by-field-name node "marker")))
          (let ((inner (replique-deps--child-index-at branch pos)))
            (and inner (nth inner (treesit-node-children branch t))))
        branch))))

(defun replique-deps--effective (node pos)
  "Return what NODE stands for at POS.

Its quoting and its metadata read through, and a reader conditional read
down into - what is written in one of those is written where the
conditional is."
  (let ((node (replique-deps--unwrap node)))
    (if (equal "reader_conditional" (treesit-node-type node))
        (when-let* ((branch (replique-deps--branch-at node pos)))
          (replique-deps--effective branch pos))
      node)))

(defun replique-deps--sequential-node-p (node)
  "Return non-nil if NODE is a list or a vector.

A spec is written as either, and a prefix list conventionally as a list -
the two are not told apart by their brackets."
  (or (replique-clojure--list-node-p node)
      (equal "vector_literal" (treesit-node-type node))))

(defun replique-deps--child-index-at (node pos)
  "Return the index of the child of NODE that POS is written in, or nil.

The one that covers POS, or - where none does - the one that ends at POS,
which is where point is once a name has been typed and nothing else has.
A collection ends at its closing bracket and point after that one is
outside it, so only what point is at counts for that."
  (let ((children (treesit-node-children node t))
        (index 0)
        (covering nil)
        (ending nil))
    (dolist (child children)
      (when (and (<= (treesit-node-start child) pos)
                 (< pos (treesit-node-end child))
                 (null covering))
        (setq covering index))
      (when (and (= pos (treesit-node-end child))
                 (not (member (treesit-node-type child) replique-deps--collections))
                 (null ending))
        (setq ending index))
      (setq index (1+ index)))
    (or covering ending)))

(defun replique-deps--option-name (node)
  "Return the name of NODE when it is an unqualified keyword, or nil."
  (when (and node
             (replique-clojure--keyword-node-p node)
             (null (treesit-node-child-by-field-name node "namespace")))
    (replique-clojure--named-node-text node)))

(defun replique-deps--under (prefix node)
  "Return the namespace NODE names, written under PREFIX, or nil.

A prefix list gives the start of a name once and the rest of it as many
times as it has specs: (clojure [string :as s]) names clojure.string.

Nil where NODE is not a name.  A spec whose first element is a string or
a vector names nothing, and one written as a reader conditional names a
different thing on each platform - which of them being a question about
what the form means, and there is no point inside it to read for.  What
is asked with a namespace is asked of the process, and a namespace that
is not one is a question about nothing: nothing is the answer to give."
  (when (replique-clojure--symbol-node-p node)
    (let ((name (treesit-node-text node t)))
      (if (equal "" prefix) name (concat prefix "." name)))))

(defun replique-deps--namespace-position (kind)
  "Return the position a namespace is at in a form of kind KIND.

What a :require-macros names is a namespace of the other of the two
worlds ClojureScript compiles with - a Clojure one, holding the macros
that its own namespaces use.  The names to offer there are not the names
to offer in a :require, so the two are not one position."
  (if (eq kind 'require-macros) :namespace-macros :namespace))

(defun replique-deps--option-position (kind)
  "Return the position an option is at in a form of kind KIND.

What a :use takes is what a refer takes, :only and :exclude, where a
:require takes :as and :refer."
  (if (eq kind 'use) :libspec-option-refer :libspec-option))

(defun replique-deps--spec-context (spec pos prefix kind)
  "Return the slot of SPEC that POS is written in, under PREFIX.

KIND is what the form the spec is written in is, which decides what a
namespace and an option are read as.

The first element of a spec is the namespace it names.  The rest are read
against what precedes them, so that a vector after a :refer is vars and a
name after an :as is a name being given - which nothing can be offered
for, and which is answered as nothing.  An element that follows no option
at all is a spec of its own, written under the prefix this one makes."
  (let* ((children (mapcar #'replique-deps--unwrap
                           (treesit-node-children spec t)))
         (index (replique-deps--child-index-at spec pos))
         (child (and index (replique-deps--effective (nth index children) pos))))
    (cond
     ((null index) nil)
     ((= 0 index)
      (list :position (replique-deps--namespace-position kind) :prefix prefix))
     ((replique-clojure--keyword-node-p child)
      (list :position (replique-deps--option-position kind)))
     (t
      (let ((option (replique-deps--option-name (nth (1- index) children)))
            (namespace (replique-deps--under prefix (car children))))
        (cond
         ;; everything left is read against the namespace the spec names,
         ;; as the vars of it or as the prefix of another one
         ((null namespace) nil)
         ((member option replique-deps--var-options)
          (when (replique-deps--sequential-node-p child)
            (list :position :var :namespace namespace)))
         ;; what follows any other option is that option's own business:
         ;; the name given by an :as, the map of a :rename
         (option nil)
         ((replique-clojure--symbol-node-p child)
          (list :position (replique-deps--namespace-position kind)
                :prefix namespace))
         ((replique-deps--sequential-node-p child)
          (replique-deps--spec-context child pos namespace kind))))))))

(defun replique-deps--require-context (node pos kind)
  "Return the slot of the require-like form NODE that POS is written in.

KIND is what the form is.  What is written in one is specs, and keywords
among them are the flags of the whole form rather than the options of
any spec."
  (let* ((children (mapcar #'replique-deps--unwrap
                           (treesit-node-children node t)))
         (index (replique-deps--child-index-at node pos))
         (child (and index (replique-deps--effective (nth index children) pos))))
    (cond
     ((or (null index) (= 0 index)) nil)
     ((replique-clojure--keyword-node-p child) (list :position :flag))
     ((replique-clojure--symbol-node-p child)
      (list :position (replique-deps--namespace-position kind) :prefix ""))
     ((replique-deps--sequential-node-p child)
      (replique-deps--spec-context child pos "" kind)))))

(defun replique-deps--import-context (node pos)
  "Return the slot of the import-like form NODE that POS is written in.

An import is written as a class, or as a package and the classes of it.
A package that is not a name names no classes, as in
`replique-deps--under\='."
  (let* ((children (mapcar #'replique-deps--unwrap
                           (treesit-node-children node t)))
         (index (replique-deps--child-index-at node pos))
         (child (and index (replique-deps--effective (nth index children) pos))))
    (cond
     ((or (null index) (= 0 index)) nil)
     ((replique-clojure--symbol-node-p child) (list :position :package-or-class))
     ((replique-deps--sequential-node-p child)
      (let* ((package (replique-deps--unwrap
                       (car (treesit-node-children child t))))
             (inner (replique-deps--child-index-at child pos)))
        (cond
         ((null inner) nil)
         ((= 0 inner) (list :position :package-or-class))
         ((replique-clojure--symbol-node-p package)
          (list :position :class
                :package (treesit-node-text package t)))))))))

(defun replique-deps--refer-context (node pos namespace from)
  "Return the slot of the refer-like form NODE that POS is in, for NAMESPACE.

FROM is the index of the first element that is an option, which is one
for a clause and two for a (refer \\='the-ns ...) - what a call names
first is the namespace to refer from.

NAMESPACE is nil where the form names one that is not a name, and the
vars of it are answered as nothing for the reason `replique-deps--under\='
answers as nothing."
  (let* ((children (mapcar #'replique-deps--unwrap
                           (treesit-node-children node t)))
         (index (replique-deps--child-index-at node pos))
         (child (and index (replique-deps--effective (nth index children) pos))))
    (cond
     ((null index) nil)
     ((< index from)
      (when (and (= index 1) (replique-clojure--symbol-node-p child))
        (list :position :namespace :prefix "")))
     ((replique-clojure--keyword-node-p child)
      (list :position :libspec-option-refer))
     ((and namespace
           (member (replique-deps--option-name (nth (1- index) children))
                   replique-deps--var-options)
           (replique-deps--sequential-node-p child))
      (list :position :var :namespace namespace)))))

(defun replique-deps--refer-call-context (node pos)
  "Return the slot of the (refer \\='the-ns ...) form NODE that POS is in."
  (let* ((children (mapcar #'replique-deps--unwrap
                           (treesit-node-children node t)))
         (named (nth 1 children)))
    (replique-deps--refer-context
     node pos
     (and named (replique-clojure--symbol-node-p named)
          (treesit-node-text named t))
     2)))

(defun replique-deps--load-context (node pos)
  "Return the slot of the load-like form NODE that POS is written in.

What is written in one is paths, and a path is written as a string."
  (let* ((children (treesit-node-children node t))
         (index (replique-deps--child-index-at node pos))
         (child (and index (nth index children))))
    (when (and index (> index 0) (replique-clojure--string-node-p child))
      (list :position :load-path))))

(defun replique-deps--written-in-ns-form-p (node)
  "Return non-nil if NODE is written in an ns form.

The nearest form around it rather than whatever holds it, so that a
clause written inside a reader conditional is written in what the
conditional is written in.  What a conditional is made of - the list of
its platforms, the vector of a splicing one - is not a form anything is
written in, and neither is a vector or a map: a form is a list whose head
is a symbol, and the rest is read through."
  (let ((parent (treesit-node-parent node))
        (found nil))
    (while (and parent (null found))
      (when (replique-clojure--list-node-sym-text parent)
        (setq found parent))
      (setq parent (treesit-node-parent parent)))
    (equal "ns" (replique-clojure--list-node-sym-text found))))

(defun replique-deps--form-kind (node)
  "Return what dependency form NODE is, or nil.

A clause counts where it is written in an ns form and not anywhere else:
a list whose head is a keyword is a lookup in that keyword wherever else
it is written, and the one thing it is not is a require."
  (when (replique-clojure--list-node-p node)
    (let ((head (replique-clojure--first-value-child node)))
      (cond
       ((and (replique-deps--option-name head)
             (replique-deps--written-in-ns-form-p node))
        (cdr (assoc (replique-clojure--named-node-text head)
                    replique-deps--clauses)))
       ((and head (replique-clojure--symbol-node-p head))
        (let ((qualifier (treesit-node-child-by-field-name head "namespace")))
          (when (or (null qualifier)
                    (equal "clojure.core" (treesit-node-text qualifier t)))
            (cdr (assoc (replique-clojure--named-node-text head)
                        replique-deps--calls)))))))))

(defun replique-deps--form-at (pos)
  "Return the dependency form POS is written in and what it is, or nil.

A cons of the node and its kind.  Read upwards, so the answer is the
innermost of them - a (require ...) written inside another form is read
as itself."
  (let ((node (treesit-node-at pos))
        (found nil))
    (while (and node (null found))
      (when (and (<= (treesit-node-start node) pos)
                 (< pos (treesit-node-end node)))
        (let ((kind (replique-deps--form-kind node)))
          (when kind (setq found (cons node kind)))))
      (setq node (treesit-node-parent node)))
    found))

(defun replique-deps-context-at (pos)
  "Return what the dependency form at POS is asking for there, or nil.

A plist holding :position, and what the process needs to answer it:

  :dependency-type    the :require or :import of an ns form
  :namespace          a namespace, :prefix being what it is written under
  :namespace-macros   a namespace of macros, written under a :prefix too
  :var                a var, :namespace being the one to look in
  :package-or-class   an import written as one name
  :class              a class, :package being the one it is in
  :libspec-option     an option of a require, :as and the like
  :libspec-option-refer   an option of a refer, :only and the like
  :flag               a flag of the whole form, :reload and the like
  :load-path          the path of a load, written as a string

The :namespace of a var is the keyword :refer-clojure where the form is a
refer-clojure, whose namespace is not written anywhere in it.

Nil where POS is in no dependency form, and where it is in one but at
nothing that can be answered: a name being given after an :as, or a space
between two specs.

Reads the whole of the buffer rather than what a narrowing left reachable,
since a clause is in an ns form whether or not the ns form is on screen."
  (save-restriction
    (widen)
    (when-let* ((form (replique-deps--form-at pos))
                (node (car form))
                (kind (cdr form)))
      (if (and (equal 0 (replique-deps--child-index-at node pos))
               (replique-deps--option-name
                (replique-clojure--first-value-child node)))
          (list :position :dependency-type)
        (cond
         ((eq kind 'import) (replique-deps--import-context node pos))
         ((eq kind 'load) (replique-deps--load-context node pos))
         ((eq kind 'refer-clojure)
          (replique-deps--refer-context node pos :refer-clojure 1))
         ((eq kind 'refer) (replique-deps--refer-call-context node pos))
         (t (replique-deps--require-context node pos kind)))))))

(provide 'replique-deps)

;;; replique-deps.el ends here
