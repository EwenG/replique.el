;;; replique-parse-test.el --- Tests for the Clojure reader  -*- lexical-binding: t; -*-

;;; Commentary:

;; What the reader makes of text, checked against what Clojure's own reader
;; makes of it.  The cases that matter are the ones where reading left to
;; right would give a different answer - `a/b/c', `clojure.core//', `08',
;; `\\(' - and the ones where the text does not read at all, because a buffer
;; being typed in is text that does not read and the answer for it has to be
;; a tree rather than a signal.

;;; Code:

(require 'ert)
(require 'replique-parse)

(defun replique-parse-test--shape (node)
  "The type of NODE and of everything under it, as a list."
  (let ((type (replique-parse-type node))
        (children (replique-parse-children node)))
    (if children
        (cons type (mapcar #'replique-parse-test--shape children))
      type)))

(defun replique-parse-test--read (text)
  "Read TEXT and return its root."
  (with-temp-buffer
    (insert text)
    (replique-parse-buffer)))

(defun replique-parse-test--shape-of (text)
  "The shape of what TEXT reads as, with the root left off."
  (cdr (replique-parse-test--shape (replique-parse-test--read text))))

(defun replique-parse-test--one (text)
  "The single form TEXT reads as."
  (car (replique-parse-children (replique-parse-test--read text))))

(defun replique-parse-test--kind (text)
  "The type of the single form TEXT reads as, or its complaint."
  (let ((node (replique-parse-test--one text)))
    (if (replique-parse-error node)
        (list (replique-parse-type node) (replique-parse-error node))
      (replique-parse-type node))))


;;;; Tokens

(ert-deftest replique-parse-test-token-kinds ()
  (should (eq 'symbol (replique-parse-test--kind "foo")))
  (should (eq 'keyword (replique-parse-test--kind ":foo")))
  (should (eq 'keyword (replique-parse-test--kind "::foo")))
  (should (eq 'null (replique-parse-test--kind "nil")))
  (should (eq 'boolean (replique-parse-test--kind "true")))
  (should (eq 'boolean (replique-parse-test--kind "false")))
  (should (eq 'number (replique-parse-test--kind "1")))
  (should (eq 'string (replique-parse-test--kind "\"a\"")))
  (should (eq 'regex (replique-parse-test--kind "#\"a\"")))
  (should (eq 'character (replique-parse-test--kind "\\a")))
  ;; a name that only looks like one of those
  (should (eq 'symbol (replique-parse-test--kind "niln")))
  (should (eq 'symbol (replique-parse-test--kind "truee"))))

(ert-deftest replique-parse-test-token-boundaries ()
  ;; `#' and `'' and `%' may be written inside a name
  (should (equal '(symbol) (replique-parse-test--shape-of "foo'bar")))
  (should (equal '(symbol) (replique-parse-test--shape-of "foo#bar")))
  (should (equal '(symbol) (replique-parse-test--shape-of "foo:bar")))
  ;; the terminating ones may not
  (should (equal '(symbol (deref symbol)) (replique-parse-test--shape-of "foo@bar")))
  (should (equal '(symbol character) (replique-parse-test--shape-of "foo\\b")))
  (should (equal '(symbol (list symbol)) (replique-parse-test--shape-of "foo(bar)")))
  ;; a comma is whitespace
  (should (equal '((vector number number)) (replique-parse-test--shape-of "[1, 2]")))
  ;; and the space Unicode calls one and Java does not is not whitespace
  (should (equal '(symbol) (replique-parse-test--shape-of "a\u00a0b"))))

(ert-deftest replique-parse-test-names ()
  (dolist (text '("foo" "foo-bar" "!?" "." ".." ".5" "/" ":/" "+" "-"
                  "clojure.string/join" "a/b/c" "clojure.core//" ":a:b"
                  "::foo" "::a/b" "%" "%1" "%&"))
    (should (null (replique-parse-error (replique-parse-test--one text)))))
  (dolist (text '("a::b" "foo:" ":" "::" "a:/b" "/foo" "a/b/" "//"))
    (should (eq 'invalid (replique-parse-error (replique-parse-test--one text))))))

(ert-deftest replique-parse-test-numbers ()
  (dolist (text '("0" "-0" "+5" "42" "0x1F" "-0xFF" "017" "2r1011" "36rZZ"
                  "22/7" "-22/7" "1.5" "1." "1.5e3" "1.5e-3" "1M" "1.5e3M"
                  "42N"))
    (should (null (replique-parse-error (replique-parse-test--one text)))))
  ;; A token that opens with a digit is a number or it is nothing
  (dolist (text '("08" "123abc" "1r0" "37rZ" "0x" "2r2" "1/0.5"))
    (should (equal (list 'number 'invalid) (replique-parse-test--kind text))))
  ;; and one that does not open with a digit is a name
  (should (eq 'symbol (replique-parse-test--kind ".5")))
  (should (eq 'symbol (replique-parse-test--kind "-foo"))))

(ert-deftest replique-parse-test-characters ()
  (dolist (text '("\\a" "\\newline" "\\space" "\\tab" "\\formfeed" "\\backspace"
                  "\\return" "\\u0041" "\\o377" "\\(" "\\;" "\\\\" "\\ "))
    (should (null (replique-parse-error (replique-parse-test--one text)))))
  (dolist (text '("\\foo" "\\uD800" "\\o400" "\\u00" "\\o7777"))
    (should (equal (list 'character 'invalid) (replique-parse-test--kind text))))
  ;; the character after the backslash is taken whatever it is, so a
  ;; semicolon written as one is not a comment
  (should (equal '(character symbol) (replique-parse-test--shape-of "\\; x"))))


;;;; Collections

(ert-deftest replique-parse-test-collections ()
  (should (equal '((list number number)) (replique-parse-test--shape-of "(1 2)")))
  (should (equal '((vector number)) (replique-parse-test--shape-of "[1]")))
  (should (equal '((set number)) (replique-parse-test--shape-of "#{1}")))
  (should (equal '((fn symbol symbol number)) (replique-parse-test--shape-of "#(+ % 1)")))
  (should (equal '((map (pair keyword number))) (replique-parse-test--shape-of "{:a 1}"))))

(ert-deftest replique-parse-test-map-entries ()
  ;; A comment between a key and its value stays inside the entry
  (should (equal '((map (pair keyword comment number)))
                 (replique-parse-test--shape-of "{:a ;; c\n 1}")))
  ;; and one between entries stays between them
  (should (equal '((map (pair keyword number) comment (pair keyword number)))
                 (replique-parse-test--shape-of "{:a 1 ;; c\n :b 2}")))
  ;; A discarded form is not half an entry
  (should (equal '((map (pair keyword (discard number) number)))
                 (replique-parse-test--shape-of "{:a #_1 2}")))
  ;; A key with no value is a map that is not data
  (should (eq 'odd (replique-parse-error (replique-parse-test--one "{:a 1 :b}")))))


;;;; Reader macros

(ert-deftest replique-parse-test-reader-macros ()
  (should (equal '((quote symbol)) (replique-parse-test--shape-of "'foo")))
  (should (equal '((syntax-quote symbol)) (replique-parse-test--shape-of "`foo")))
  (should (equal '((unquote symbol)) (replique-parse-test--shape-of "~foo")))
  (should (equal '((unquote-splicing symbol)) (replique-parse-test--shape-of "~@foo")))
  (should (equal '((deref symbol)) (replique-parse-test--shape-of "@foo")))
  (should (equal '((var-quote symbol)) (replique-parse-test--shape-of "#'foo")))
  (should (equal '((discard symbol)) (replique-parse-test--shape-of "#_foo")))
  (should (equal '((eval (list number))) (replique-parse-test--shape-of "#=(1)")))
  (should (equal '((tagged symbol string)) (replique-parse-test--shape-of "#inst \"2020\"")))
  (should (equal '((meta keyword symbol)) (replique-parse-test--shape-of "^:private foo")))
  (should (equal '((meta keyword symbol)) (replique-parse-test--shape-of "#^:private foo")))
  (should (equal '(symbolic) (replique-parse-test--shape-of "##Inf")))
  (should (equal (list 'symbolic 'invalid) (replique-parse-test--kind "##Nope"))))

(ert-deftest replique-parse-test-metadata-chains ()
  (should (equal '((meta keyword (meta keyword (list symbol symbol number))))
                 (replique-parse-test--shape-of "^:private ^:dynamic (def x 1)")))
  ;; whatever is written between the marker and its form is kept
  (should (equal '((meta string comment (vector number)))
                 (replique-parse-test--shape-of "^ \"S\"\n;; c\n[1]"))))

(ert-deftest replique-parse-test-namespaced-maps ()
  (should (equal '((namespaced-map keyword (map (pair keyword number))))
                 (replique-parse-test--shape-of "#:foo{:a 1}")))
  ;; `#::{…}' writes a bare `::', which is not a keyword anybody could write
  ;; on its own and is not refused here
  (should (null (replique-parse-error (replique-parse-test--one "#::{:a 1}"))))
  (should (null (replique-parse-error (replique-parse-test--one "#::al{:a 1}"))))
  ;; what it holds has to be a map
  (should (eq 'invalid (replique-parse-error (replique-parse-test--one "#:foo[1]")))))

(ert-deftest replique-parse-test-reader-conditionals ()
  (should (equal '((reader-conditional (list keyword number keyword number)))
                 (replique-parse-test--shape-of "#?(:clj 1 :cljs 2)")))
  (should (equal '((reader-conditional-splicing (list keyword (vector number))))
                 (replique-parse-test--shape-of "#?@(:clj [1])")))
  ;; neither branch is chosen: what is read is what is written
  (should (eq 'invalid (replique-parse-error (replique-parse-test--one "#?[1]")))))

(ert-deftest replique-parse-test-target ()
  ;; The text a node was read from is read out of the buffer it was read
  ;; from, so asking for it means still being there
  (dolist (case '(("'foo" . "foo")
                  ("#:foo{:a 1}" . "{:a 1}")
                  ("^:private x" . "x")
                  ("#inst \"2020\"" . "\"2020\"")))
    (with-temp-buffer
      (insert (car case))
      (should (equal (cdr case)
                     (replique-parse-text
                      (replique-parse-target
                       (car (replique-parse-children (replique-parse-buffer)))))))))
  ;; a macro written in front of nothing is in front of nothing
  (should (null (replique-parse-target (replique-parse-test--one "#_")))))


;;;; Text that does not read

(ert-deftest replique-parse-test-unclosed ()
  (let ((node (replique-parse-test--one "(defn foo [a")))
    (should (eq 'unclosed (replique-parse-error node)))
    ;; and what was written inside it is still there to be looked at
    (should (equal '(list symbol symbol (vector symbol))
                   (replique-parse-test--shape node)))))

(ert-deftest replique-parse-test-mismatched ()
  ;; The bracket that does not close the form being read is left for the form
  ;; outside, so one wrong bracket costs one form rather than the rest
  (should (equal '((vector list)) (replique-parse-test--shape-of "[(]")))
  (should (eq 'mismatched
              (replique-parse-error
               (car (replique-parse-children (replique-parse-test--one "[(]")))))))

(ert-deftest replique-parse-test-unmatched ()
  (should (equal '(unmatched list) (replique-parse-test--shape-of ") ()")))
  (should (eq 'unmatched (replique-parse-error (replique-parse-test--one ")")))))

(ert-deftest replique-parse-test-cut-off ()
  (should (eq 'unclosed (replique-parse-error (replique-parse-test--one "\"abc"))))
  (should (eq 'unclosed (replique-parse-error (replique-parse-test--one "#\"abc"))))
  (dolist (text '("'" "`" "~" "~@" "@" "^" "#'" "#_" "#"))
    (should (eq 'eof (replique-parse-error (replique-parse-test--one text))))))

(ert-deftest replique-parse-test-error-reaches-the-root ()
  (let ((root (replique-parse-test--read "(a) (b")))
    (should (eq t (replique-parse-error root)))
    (should (null (replique-parse-error
                   (car (replique-parse-children root))))))
  (should (null (replique-parse-error (replique-parse-test--read "(a) (b)")))))


;;;; The shape of the tree

(ert-deftest replique-parse-test-nesting ()
  (with-temp-buffer
    (insert "(defn f [x] {:a [1 #{2}] :b \"s\"}) ;; c\n^:m #inst \"2020\"\n")
    (let ((root (replique-parse-buffer)))
      (replique-parse-test--check-nesting root))))

(defun replique-parse-test--check-nesting (node)
  "Signal unless what NODE is made of is inside it and in order."
  (let ((previous (replique-parse-start node)))
    (dolist (child (replique-parse-children node))
      (should (>= (replique-parse-start child) previous))
      (should (<= (replique-parse-end child) (replique-parse-end node)))
      (setq previous (replique-parse-end child))
      (replique-parse-test--check-nesting child))))

(ert-deftest replique-parse-test-region ()
  (with-temp-buffer
    (insert "(a) (b) (c)")
    ;; Only what was asked for is read, and the positions are the buffer's
    (let ((root (replique-parse-region 5 8)))
      (should (= 1 (length (replique-parse-children root))))
      (should (= 5 (replique-parse-start (car (replique-parse-children root)))))
      (should (equal "(b)" (replique-parse-text
                            (car (replique-parse-children root))))))))

(ert-deftest replique-parse-test-shebang ()
  (should (equal '(shebang (list symbol number number))
                 (replique-parse-test--shape-of "#!/usr/bin/env bb\n(+ 1 1)")))
  ;; only where one can be written
  (should (equal '(symbol (tagged symbol symbol))
                 (replique-parse-test--shape-of "x #!foo bar"))))


;;;; Finding what is written where

(ert-deftest replique-parse-test-queries ()
  (with-temp-buffer
    (insert "(defn f [x] x)")
    (let ((root (replique-parse-buffer)))
      (should (equal '(root list vector symbol)
                     (mapcar #'replique-parse-type (replique-parse-path root 10))))
      (should (eq 'symbol (replique-parse-type (replique-parse-node-at root 10))))
      (should (eq 'list (replique-parse-type
                         (replique-parse-top-level-at root 10))))
      ;; the position a form opens at is in it and the one it ends at is not
      (should (replique-parse-top-level-at root 1))
      (should (null (replique-parse-top-level-at root 15))))))

(ert-deftest replique-parse-test-name-parts ()
  (should (equal '(nil . "foo") (replique-parse-name-parts "foo")))
  (should (equal '("a" . "b") (replique-parse-name-parts "a/b")))
  (should (equal '("a/b" . "c") (replique-parse-name-parts "a/b/c")))
  (should (equal '("clojure.core" . "/") (replique-parse-name-parts "clojure.core//")))
  (should (equal '(nil . "/") (replique-parse-name-parts "/")))
  (should (equal '("a" . "b") (replique-parse-name-parts ":a/b")))
  (should (equal '("a" . "b") (replique-parse-name-parts "::a/b")))
  (should (replique-parse-auto-resolve-p "::foo"))
  (should (not (replique-parse-auto-resolve-p ":foo"))))

(provide 'replique-parse-test)

;;; replique-parse-test.el ends here
