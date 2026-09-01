;;; replique-deps-test.el --- Tests for the dependency context  -*- lexical-binding: t; -*-

;;; Commentary:

;; Which slot of a require or an import point is in.  Like the locals tests
;; these need no process: what they check is a reading of the parse.
;;
;; Each of them is written as one form with a | in it where the question is
;; asked, and the same clause is often written twice - once inside an ns
;; form and once as the call that says the same thing.

;;; Code:

(require 'ert)
(require 'replique-test)
(require 'replique-deps)

(defun replique-deps-test--context (text)
  "Return what the dependency form in TEXT is asking for where | is."
  (replique-test-grammar)
  (with-temp-buffer
    (replique-clojure-mode)
    (insert text)
    (goto-char (point-min))
    (unless (search-forward "|" nil t)
      (error "The text says nowhere to look: %s" text))
    (let ((pos (match-beginning 0)))
      (delete-region (match-beginning 0) (match-end 0))
      (replique-deps-context-at pos))))

(defun replique-deps-test--context-narrowed (text)
  "Return what TEXT asks for where | is, with the buffer narrowed to its line."
  (replique-test-grammar)
  (with-temp-buffer
    (replique-clojure-mode)
    (insert text)
    (goto-char (point-min))
    (search-forward "|")
    (let ((pos (match-beginning 0)))
      (delete-region (match-beginning 0) (match-end 0))
      (goto-char pos)
      (narrow-to-region (line-beginning-position) (line-end-position))
      (replique-deps-context-at pos))))

;;; The clause and the call

(ert-deftest replique-deps-test-a-clause-and-the-call-that-says-it-are-one-thing ()
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a (:require b|))")))
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(require 'b|)")))
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(clojure.core/require '[b|])"))))

(ert-deftest replique-deps-test-what-a-clause-is-named-is-a-dependency-type ()
  (should (equal '(:position :dependency-type)
                 (replique-deps-test--context "(ns a (:req|uire [b]))")))
  ;; the head of a call is a var like any other and is completed like one
  (should-not (replique-deps-test--context "(requ|ire '[b])")))

(ert-deftest replique-deps-test-a-clause-counts-only-in-an-ns-form ()
  ;; a list whose head is a keyword is a lookup in that keyword anywhere else
  (should-not (replique-deps-test--context "(:require b|)"))
  (should-not (replique-deps-test--context "(foo :require b|)"))
  (should-not (replique-deps-test--context "(foo (:require b|))"))
  (should-not (replique-deps-test--context "(let [x 1] (:require b|))"))
  (should-not (replique-deps-test--context "(ns a (:foo/require b|))")))

(ert-deftest replique-deps-test-a-clause-is-in-the-nearest-form-around-it ()
  ;; one written inside a reader conditional is written in the ns form,
  ;; whatever the conditional is made of holds it
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a #?(:clj (:require [b|])))")))
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a #?@(:clj [(:require [b|])]))"))))

;;; Namespaces

(ert-deftest replique-deps-test-a-namespace-is-written-alone-or-first-in-a-spec ()
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a (:require [b| :as c]))")))
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a (:require [b|]))")))
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a (:require [b :as c] [d| :as e]))"))))

(ert-deftest replique-deps-test-a-prefix-list-says-what-its-specs-are-under ()
  (should (equal '(:position :namespace :prefix "c")
                 (replique-deps-test--context "(ns a (:require (c b|)))")))
  (should (equal '(:position :namespace :prefix "c")
                 (replique-deps-test--context "(ns a (:require (c [b| :as x])))")))
  ;; and one written inside another says both
  (should (equal '(:position :namespace :prefix "c.d")
                 (replique-deps-test--context "(ns a (:require (c (d [b|]))))"))))

(ert-deftest replique-deps-test-metadata-does-not-hide-a-spec ()
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a (:require ^:x [b|]))"))))

;;; Vars

(ert-deftest replique-deps-test-what-follows-a-refer-is-vars-of-that-namespace ()
  (should (equal '(:position :var :namespace "b")
                 (replique-deps-test--context "(ns a (:require [b :refer [d|]]))")))
  (should (equal '(:position :var :namespace "b")
                 (replique-deps-test--context "(ns a (:require [b :refer [c d|]]))")))
  (should (equal '(:position :var :namespace "b")
                 (replique-deps-test--context "(require '[b :refer [d|]])"))))

(ert-deftest replique-deps-test-a-prefix-list-carries-down-to-the-vars ()
  (should (equal '(:position :var :namespace "c.b")
                 (replique-deps-test--context "(ns a (:require (c [b :refer [d|]])))"))))

(ert-deftest replique-deps-test-an-only-and-an-exclude-are-refers-too ()
  (should (equal '(:position :var :namespace "b")
                 (replique-deps-test--context "(ns a (:use [b :only [d|]]))")))
  (should (equal '(:position :var :namespace "b")
                 (replique-deps-test--context "(use '[b :only [d|]])")))
  (should (equal '(:position :var :namespace "clojure.set")
                 (replique-deps-test--context "(refer 'clojure.set :only [uni|on])"))))

(ert-deftest replique-deps-test-a-refer-clojure-has-its-namespace-nowhere-in-it ()
  (should (equal '(:position :var :namespace :refer-clojure)
                 (replique-deps-test--context "(ns a (:refer-clojure :exclude [ma|p]))")))
  (should (equal '(:position :var :namespace :refer-clojure)
                 (replique-deps-test--context "(refer-clojure :exclude [ma|p])"))))

(ert-deftest replique-deps-test-what-a-refer-call-names-first-is-a-namespace ()
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(refer 'clojure.s|et :only [union])"))))

;;; Options and flags

(ert-deftest replique-deps-test-an-option-of-a-require-is-not-one-of-a-refer ()
  (should (equal '(:position :libspec-option)
                 (replique-deps-test--context "(ns a (:require [b :a|s c]))")))
  (should (equal '(:position :libspec-option-refer)
                 (replique-deps-test--context "(ns a (:use [b :o|nly [d]]))")))
  (should (equal '(:position :libspec-option-refer)
                 (replique-deps-test--context "(ns a (:refer-clojure :ex|clude [map]))"))))

(ert-deftest replique-deps-test-a-refer-all-is-an-option-and-not-a-var ()
  (should (equal '(:position :libspec-option)
                 (replique-deps-test--context "(ns a (:require [b :refer :a|ll]))"))))

(ert-deftest replique-deps-test-a-keyword-among-the-specs-is-a-flag ()
  (should (equal '(:position :flag)
                 (replique-deps-test--context "(ns a (:require [b] :relo|ad))")))
  (should (equal '(:position :flag)
                 (replique-deps-test--context "(require '[b] :relo|ad)"))))

(ert-deftest replique-deps-test-what-follows-an-as-is-a-name-being-given ()
  (should-not (replique-deps-test--context "(ns a (:require [b :as c|]))"))
  (should-not (replique-deps-test--context "(ns a (:require [b :as-alias c|]))"))
  (should-not (replique-deps-test--context "(require '[b :as c|])")))

(ert-deftest replique-deps-test-a-namespace-that-is-not-a-name-names-nothing ()
  ;; what is asked with a namespace is asked of the process, so a namespace
  ;; that is not one would be a question about nothing
  (should-not (replique-deps-test--context "(ns a (:require [\"b\" :refer [d|]]))"))
  (should-not (replique-deps-test--context "(ns a (:require [[b] :refer [d|]]))"))
  ;; and one written as a reader conditional names a different thing on each
  ;; platform, with no point inside it to read for
  (should-not (replique-deps-test--context
               "(ns a (:require [#?(:clj b :cljs c) :refer [d|]]))"))
  (should-not (replique-deps-test--context "(ns a (:require (#?(:clj c) b|)))"))
  ;; an option of the spec needs no namespace and is answered anyway
  (should (equal '(:position :libspec-option)
                 (replique-deps-test--context "(ns a (:require [#?(:clj b) :a|s c]))"))))

(ert-deftest replique-deps-test-a-refer-of-no-namespace-names-no-vars ()
  (should-not (replique-deps-test--context "(refer (get-ns) :only [y|])"))
  (should-not (replique-deps-test--context "(refer :kw :only [y|])")))

(ert-deftest replique-deps-test-a-package-that-is-not-a-name-holds-nothing ()
  (should-not (replique-deps-test--context "(ns a (:import ([x] Da|te)))"))
  (should-not (replique-deps-test--context
               "(ns a (:import (#?(:clj java.util) Da|te)))")))

(ert-deftest replique-deps-test-what-follows-a-rename-is-answered-as-nothing ()
  ;; only the keys of it are vars, and vars for both would be wrong for one
  (should-not (replique-deps-test--context "(ns a (:require [b :rename {c| d}]))")))

;;; Imports

(ert-deftest replique-deps-test-an-import-is-a-class-or-a-package-and-classes ()
  (should (equal '(:position :package-or-class)
                 (replique-deps-test--context "(ns a (:import java.util.D|ate))")))
  (should (equal '(:position :package-or-class)
                 (replique-deps-test--context "(ns a (:import (java.u|til Date)))")))
  (should (equal '(:position :class :package "java.util")
                 (replique-deps-test--context "(ns a (:import (java.util Da|te)))")))
  (should (equal '(:position :class :package "java.util")
                 (replique-deps-test--context "(ns a (:import (java.util Date UU|ID)))")))
  (should (equal '(:position :class :package "java.util")
                 (replique-deps-test--context "(import '(java.util Da|te))"))))

;;; Load

(ert-deftest replique-deps-test-a-load-is-a-path-written-as-a-string ()
  (should (equal '(:position :load-path)
                 (replique-deps-test--context "(ns a (:load \"/fo|o\"))")))
  (should (equal '(:position :load-path)
                 (replique-deps-test--context "(load \"a\" \"b|\")"))))

;;; Where nothing is asked

(ert-deftest replique-deps-test-nothing-is-asked-between-two-specs ()
  (should-not (replique-deps-test--context "(ns a (:require [b :as c]) |)"))
  (should-not (replique-deps-test--context "(ns a (:require |))"))
  (should-not (replique-deps-test--context "(ns a (:require [|]))")))

(ert-deftest replique-deps-test-the-name-of-an-ns-form-is-not-a-dependency ()
  (should-not (replique-deps-test--context "(ns a| (:require [b]))"))
  (should-not (replique-deps-test--context "|(ns a (:require [b]))")))

(ert-deftest replique-deps-test-a-reader-conditional-is-read-branch-by-branch ()
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a (:require #?(:clj [b|] :cljs [c])))")))
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a (:require #?(:clj [b] :cljs [c|])))")))
  (should (equal '(:position :var :namespace "b")
                 (replique-deps-test--context "(ns a (:require #?(:clj [b :refer [d|]])))")))
  (should (equal '(:position :class :package "java.util")
                 (replique-deps-test--context "(ns a (:import #?(:clj (java.util Da|te))))")))
  ;; one written among the specs of a prefix list is under it like the rest
  (should (equal '(:position :namespace :prefix "c")
                 (replique-deps-test--context "(ns a (:require (c #?(:clj [b|]))))")))
  ;; and the quoting of a call reads through it
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(require #?(:clj '[b|]))"))))

(ert-deftest replique-deps-test-a-splicing-conditional-holds-several-specs ()
  (should (equal '(:position :namespace :prefix "")
                 (replique-deps-test--context "(ns a (:require #?@(:clj [[b|] [c]])))")))
  (should (equal '(:position :var :namespace "c")
                 (replique-deps-test--context
                  "(ns a (:require #?@(:clj [[b] [c :refer [d|]]])))"))))

(ert-deftest replique-deps-test-a-platform-is-not-a-dependency ()
  (should-not (replique-deps-test--context "(ns a (:require #?(:c|lj [b])))"))
  (should-not (replique-deps-test--context "(ns a (:require #?@(:clj [[b] |[c]])))")))

;;; Macros

(ert-deftest replique-deps-test-a-require-macros-names-a-namespace-of-macros ()
  ;; a Clojure namespace, where the :require beside it names a ClojureScript
  ;; one - the names to offer are not the same names
  (should (equal '(:position :namespace-macros :prefix "")
                 (replique-deps-test--context "(ns a (:require-macros b|))")))
  (should (equal '(:position :namespace-macros :prefix "")
                 (replique-deps-test--context "(ns a (:require-macros [b| :as c]))")))
  (should (equal '(:position :namespace-macros :prefix "c")
                 (replique-deps-test--context "(ns a (:require-macros (c b|)))")))
  (should (equal '(:position :namespace-macros :prefix "")
                 (replique-deps-test--context "(require-macros '[b|])"))))

(ert-deftest replique-deps-test-a-require-macros-is-a-require-in-every-other-way ()
  (should (equal '(:position :libspec-option)
                 (replique-deps-test--context "(ns a (:require-macros [b :a|s c]))")))
  (should (equal '(:position :var :namespace "b")
                 (replique-deps-test--context "(ns a (:require-macros [b :refer [d|]]))"))))

(ert-deftest replique-deps-test-what-follows-a-refer-macros-is-answered-as-nothing ()
  ;; the vars it names are macros of the Clojure namespace of that name, and
  ;; nothing in the answer says which of the two worlds a var is from
  (should-not (replique-deps-test--context "(ns a (:require [b :refer-macros [d|]]))"))
  (should-not (replique-deps-test--context "(ns a (:require [b :include-macros tru|e]))")))

(ert-deftest replique-deps-test-a-narrowing-does-not-hide-the-ns-form ()
  ;; a clause is in an ns form whether or not the ns form is on screen
  (should (equal '(:position :var :namespace "b")
                 (replique-deps-test--context-narrowed
                  "(ns a\n  (:require [b :refer [d|]]))"))))

(provide 'replique-deps-test)

;;; replique-deps-test.el ends here
