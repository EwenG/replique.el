;;; replique-clojure-mode-test.el --- Tests for the syntax layer  -*- lexical-binding: t; -*-

;;; Commentary:

;; What the mode paints and how it indents.
;;
;; Two things nothing else in the suite checks, and the two that are looked at
;; most: a face is on the screen every time the file is open, and indentation
;; is applied every time a line is typed.  They were written after the fact,
;; against what the mode already does, so what they pin is the behaviour as it
;; stands rather than an opinion about what it ought to be.
;;
;; Indentation is checked twice over and the two are not the same check.  Text
;; already indented the way the rules want is left alone, which is what a file
;; being edited needs; and text with its indentation taken away comes back
;; indented, which is what says the rules put it there rather than that
;; nothing happened.  The second is the stronger of the two and the first is
;; the one that is safe around a string written over several lines, where
;; taking the indentation away would take it out of the value.
;;
;; The faces are written as one form with a | in it at the token being asked
;; about, which is how the locals tests are written and reads the same way.

;;; Code:

(require 'ert)
(require 'replique-test)
(require 'replique-clojure-mode)

(defun replique-clojure-test--face (text &optional level)
  "The face painted at | in TEXT, at LEVEL or at the default one.

No grammar is asked for.  Painting reads the buffer with `replique-parse'
and nothing else, and a test that installs a grammar to check a face would
be saying it needs one."
  (with-temp-buffer
    (let ((replique-clojure-ensure-grammars nil))
      (replique-clojure-mode))
    (when level (setq-local replique-clojure-font-lock-level level))
    (insert text)
    (goto-char (point-min))
    (unless (search-forward "|" nil t)
      (error "The text says nowhere to look: %s" text))
    (let ((pos (match-beginning 0)))
      (delete-region (match-beginning 0) (match-end 0))
      (font-lock-ensure)
      (let ((face (get-text-property pos 'face)))
        (if (and (consp face) (null (cdr face))) (car face) face)))))

(defun replique-clojure-test--reindent (text)
  "TEXT with the indentation taken off every line and put back by the rules."
  (with-temp-buffer
    (let ((replique-clojure-ensure-grammars nil))
      (replique-clojure-mode))
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]+" nil t) (replace-match ""))
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun replique-clojure-test--reindents-to-itself (text)
  "Say whether TEXT comes back as it is once the rules have had it."
  (equal text (replique-clojure-test--reindent text)))

(defun replique-clojure-test--left-alone (text)
  "Say whether TEXT is left as it is by indenting the whole of it."
  (equal text
         (with-temp-buffer
           (let ((replique-clojure-ensure-grammars nil))
             (replique-clojure-mode))
           (insert text)
           (indent-region (point-min) (point-max))
           (buffer-string))))


;;; What is painted

(ert-deftest replique-clojure-mode-test-literals-are-painted ()
  (should (eq 'font-lock-string-face (replique-clojure-test--face "\"a|b\"")))
  (should (eq 'font-lock-number-face (replique-clojure-test--face "4|2")))
  (should (eq 'font-lock-constant-face (replique-clojure-test--face "ni|l")))
  (should (eq 'font-lock-constant-face (replique-clojure-test--face "tru|e")))
  (should (eq 'font-lock-constant-face (replique-clojure-test--face "##In|f")))
  (should (eq 'replique-clojure-character-face
              (replique-clojure-test--face "\\new|line")))
  (should (eq 'replique-clojure-keyword-face
              (replique-clojure-test--face ":fo|o"))))

(ert-deftest replique-clojure-mode-test-a-keyword-shows-its-namespace ()
  ;; the namespace of a qualified keyword is a type, the rest of it a keyword
  (should (eq 'font-lock-type-face (replique-clojure-test--face ":fo|o/bar")))
  (should (eq 'replique-clojure-keyword-face
              (replique-clojure-test--face ":foo/b|ar"))))

(ert-deftest replique-clojure-mode-test-a-regex-is-not-quite-a-string ()
  (should (eq 'font-lock-preprocessor-face (replique-clojure-test--face "|#\"a\"")))
  (should (eq 'font-lock-string-face (replique-clojure-test--face "#\"a|\""))))

(ert-deftest replique-clojure-mode-test-a-comment-is-painted ()
  (should (eq 'font-lock-comment-face (replique-clojure-test--face ";; he|re"))))

(ert-deftest replique-clojure-mode-test-what-is-built-in-is-painted-as-such ()
  ;; at the head of a form, and under clojure.core as well as bare
  (should (eq 'font-lock-keyword-face (replique-clojure-test--face "(le|t [x 1] x)")))
  (should (eq 'font-lock-keyword-face
              (replique-clojure-test--face "(clojure.core/le|t [x 1] x)")))
  ;; and not where it is written as an argument
  (should-not (eq 'font-lock-keyword-face
                  (replique-clojure-test--face "(f le|t)"))))

(ert-deftest replique-clojure-mode-test-earmuffs-are-warned-about ()
  (should (eq 'font-lock-warning-face
              (replique-clojure-test--face "*ou|t*"))))

(ert-deftest replique-clojure-mode-test-a-definition-names-itself ()
  (should (eq 'font-lock-function-name-face
              (replique-clojure-test--face "(defn fo|o [] 1)")))
  (should (eq 'font-lock-variable-name-face
              (replique-clojure-test--face "(def fo|o 1)"))))

(ert-deftest replique-clojure-mode-test-a-docstring-is-a-docstring ()
  (should (eq 'font-lock-doc-face
              (replique-clojure-test--face "(defn f \"wh|at it does\" [] 1)")))
  ;; and a string written where no docstring goes is an ordinary one
  (should (eq 'font-lock-string-face
              (replique-clojure-test--face "(f \"wh|at\" 1)"))))


(ert-deftest replique-clojure-mode-test-a-namespace-on-a-name-is-a-type ()
  ;; What a name is written under is where it is from, which is what a type
  ;; face says here.  The name itself is left to the semantic layer
  (should (eq 'font-lock-type-face (replique-clojure-test--face "(s|tr/join x)")))
  (should-not (replique-clojure-test--face "(str/jo|in x)"))
  (should (eq 'font-lock-type-face
              (replique-clojure-test--face "(Str|ing/valueOf x)"))))

(ert-deftest replique-clojure-mode-test-a-tag-on-a-name-is-a-type ()
  (should (eq 'font-lock-type-face (replique-clojure-test--face "^Str|ing x"))))

(ert-deftest replique-clojure-mode-test-what-is-written-wrongly-is-warned-about ()
  ;; A token that opens with a digit is a number or it is nothing, and a
  ;; character has to name one
  (should (eq 'font-lock-warning-face (replique-clojure-test--face "0|8")))
  (should (eq 'font-lock-warning-face (replique-clojure-test--face "12|3abc")))
  (should (eq 'font-lock-warning-face (replique-clojure-test--face "\\fo|o"))))

(ert-deftest replique-clojure-mode-test-a-type-definition-names-a-type ()
  (dolist (form '("(deftype Fo|o [])"
                  "(defrecord Fo|o [])"
                  "(defprotocol Fo|o)"
                  "(definterface Fo|o)"
                  ;; and what is named among the methods is a protocol
                  ;; being implemented, which is a type as well
                  "(deftype T [] |P (m [this]))"
                  "(reify |P (m [this]))"))
    (should (eq 'font-lock-type-face (replique-clojure-test--face form))))
  ;; and a method written inside one names a function
  (should (eq 'font-lock-function-name-face
              (replique-clojure-test--face "(defprotocol P (fo|o [this]))"))))

(ert-deftest replique-clojure-mode-test-a-keyword-is-a-keyword-wherever-it-is ()
  (should (eq 'replique-clojure-keyword-face
              (replique-clojure-test--face "(:ke|y m)"))))


;;; What the form around a token says it is
;;
;; The tests above ask what a token looks like.  These ask what the form it
;; is written in makes of it, which is the part a set of tree queries could
;; not keep straight and the reason the fontifier was rewritten.

(ert-deftest replique-clojure-mode-test-quoted-data-is-not-code ()
  ;; A quote makes a list of symbols out of what would otherwise be a
  ;; definition, and nothing in it defines or calls anything
  (should-not (replique-clojure-test--face "'(def|n foo [x] x)"))
  (should-not (replique-clojure-test--face "'(defn fo|o [x] x)"))
  (should-not (replique-clojure-test--face "`(wh|en x y)"))
  ;; what a token is, it still is
  (should (eq 'replique-clojure-keyword-face
              (replique-clojure-test--face "'(:|a 1)")))
  (should (eq 'font-lock-number-face (replique-clojure-test--face "'(:a |1)")))
  ;; and an unquote is code again, which is what makes a macro body read
  ;; the way it runs
  (should (eq 'font-lock-keyword-face
              (replique-clojure-test--face "`(a ~(wh|en x y))"))))

(ert-deftest replique-clojure-mode-test-metadata-nests-as-far-as-it-is-written ()
  ;; Four wrappers, where the queries this replaced spelled out nought to
  ;; three and stopped seeing the form at the fourth
  (should (eq 'font-lock-variable-name-face
              (replique-clojure-test--face "^:a ^:b ^:c ^:d (def |x 1)")))
  (should (eq 'font-lock-function-name-face
              (replique-clojure-test--face "(defn ^:a ^:b ^:c ^:d |f [])"))))

(ert-deftest replique-clojure-mode-test-a-method-is-its-head-and-no-more ()
  (should (eq 'font-lock-function-name-face
              (replique-clojure-test--face "(deftype T [] P (|m [this] this))")))
  ;; the symbol the query matched as well, because it was written directly
  ;; inside the method and the query asked for no more than that
  (should-not (replique-clojure-test--face "(deftype T [] P (m [this] thi|s))"))
  (should-not (replique-clojure-test--face "(deftype T [] P (m [thi|s] this))")))

(ert-deftest replique-clojure-mode-test-a-multimethod-names-a-function ()
  ;; What clojure-mode does, and what the form does: a multimethod is
  ;; called the way a function is called
  (should (eq 'font-lock-function-name-face
              (replique-clojure-test--face "(defmulti ar|ea :kind)")))
  (should (eq 'font-lock-function-name-face
              (replique-clojure-test--face "(defmethod ar|ea :square [s] 1)"))))

(ert-deftest replique-clojure-mode-test-letfn-names-its-local-functions ()
  (should (eq 'font-lock-function-name-face
              (replique-clojure-test--face "(letfn [(|f [x] x)] (f 1))"))))

(ert-deftest replique-clojure-mode-test-a-docstring-can-be-written-as-metadata ()
  (should (eq 'font-lock-doc-face
              (replique-clojure-test--face "(defn ^{:doc \"wh|at\"} f [])")))
  (should (eq 'font-lock-doc-face
              (replique-clojure-test--face "(def ^{:doc \"wh|at\"} x 1)"))))

(ert-deftest replique-clojure-mode-test-a-method-declares-its-own-docstring ()
  ;; After the argument vector rather than after the name, which is where a
  ;; protocol method declares one
  (should (eq 'font-lock-doc-face
              (replique-clojure-test--face
               "(defprotocol P (m [this] \"wh|at\"))")))
  ;; and a string written there in something that is not a protocol is a
  ;; value being returned
  (should (eq 'font-lock-string-face
              (replique-clojure-test--face "(let [x 1] \"wh|at\")"))))

(ert-deftest replique-clojure-mode-test-def-calls-a-string-a-docstring-only-with-a-value ()
  ;; What `(def x "a")' defines x as is that string
  (should (eq 'font-lock-string-face
              (replique-clojure-test--face "(def x \"wh|at\")")))
  (should (eq 'font-lock-doc-face
              (replique-clojure-test--face "(def x \"wh|at\" 1)"))))

(ert-deftest replique-clojure-mode-test-the-arguments-of-a-function-literal-are-named ()
  (should (eq 'font-lock-variable-name-face
              (replique-clojure-test--face "#(inc |%)")))
  (should (eq 'font-lock-variable-name-face
              (replique-clojure-test--face "#(+ |%1 %2)")))
  (should (eq 'font-lock-variable-name-face
              (replique-clojure-test--face "#(apply + |%&)")))
  ;; only inside one: `%' is an ordinary name anywhere else
  (should-not (replique-clojure-test--face "(inc |%)")))

(ert-deftest replique-clojure-mode-test-what-the-reader-reads-specially-is-marked ()
  (dolist (form '("|#?(:clj 1 :cljs 2)"
                  "|#?@(:clj [1])"
                  "|#:foo{:a 1}"
                  "|#'foo"
                  "|#inst \"2024\""
                  "|#\"re\""))
    (should (eq 'font-lock-preprocessor-face
                (replique-clojure-test--face form))))
  ;; a discarded form is not read, so nothing in it means anything
  (should (eq 'font-lock-comment-face
              (replique-clojure-test--face "#_(defn f| [] 1)"))))

(ert-deftest replique-clojure-mode-test-a-reader-conditional-holds-no-call ()
  ;; What is written in one is platforms and what each of them stands for,
  ;; so nothing there heads anything
  (should-not (replique-clojure-test--face "#?(wh|en 1)" 4))
  ;; and a list written inside one is an ordinary form again
  (should (eq 'font-lock-function-call-face
              (replique-clojure-test--face "#?(:clj (i|nc 1))" 4))))

(ert-deftest replique-clojure-mode-test-a-bracket-that-closes-nothing-is-warned-about ()
  (should (eq 'font-lock-warning-face (replique-clojure-test--face "(foo)|)")))
  ;; and one nobody has closed yet is not warned about: that is every
  ;; bracket for as long as it takes to type the rest of the line
  (should (eq 'font-lock-bracket-face (replique-clojure-test--face "|(foo" 4))))

(ert-deftest replique-clojure-mode-test-how-much-is-painted-can-be-said ()
  ;; One paints what is not code, and stops there
  (should (eq 'font-lock-string-face (replique-clojure-test--face "\"a|b\"" 1)))
  (should-not (replique-clojure-test--face "4|2" 1))
  (should (eq 'font-lock-number-face (replique-clojure-test--face "4|2" 2)))
  (should-not (replique-clojure-test--face "(defn f|oo [])" 2))
  (should (eq 'font-lock-function-name-face
              (replique-clojure-test--face "(defn f|oo [])" 3)))
  ;; and four paints what could be a call, and the brackets
  (should-not (replique-clojure-test--face "(ma|p inc xs)" 3))
  (should (eq 'font-lock-function-call-face
              (replique-clojure-test--face "(ma|p inc xs)" 4)))
  (should (eq 'font-lock-bracket-face (replique-clojure-test--face "|(f)" 4))))

(ert-deftest replique-clojure-mode-test-a-namespace-is-marked-under-whatever-carries-it ()
  (should (eq 'font-lock-type-face (replique-clojure-test--face ":fo|o/bar")))
  (should (eq 'replique-clojure-keyword-face
              (replique-clojure-test--face ":foo/b|ar")))
  (should (eq 'font-lock-type-face (replique-clojure-test--face "::fo|o/bar")))
  ;; and what heads a form under clojure.core is the one everybody knows
  (should (eq 'font-lock-keyword-face
              (replique-clojure-test--face "(clojure.core/wh|en x y)")))
  (should (eq 'font-lock-type-face
              (replique-clojure-test--face "(clojure.c|ore/when x y)"))))


(ert-deftest replique-clojure-mode-test-painting-asks-for-no-grammar ()
  ;; Nothing here is a major mode, a parser or a grammar - a buffer, a
  ;; region and the faces that go on it.  Which is what says the fontifier
  ;; is the reader and not a layer over somebody else's tree
  (with-temp-buffer
    (insert "(defn foo \"what it does\" [x] (inc x))")
    (replique-clojure-font-lock-region (point-min) (point-max))
    (should (eq 'font-lock-keyword-face (get-text-property 2 'face)))
    (should (eq 'font-lock-function-name-face (get-text-property 7 'face)))
    (should (eq 'font-lock-doc-face (get-text-property 12 'face)))))


;;; How it indents

(ert-deftest replique-clojure-mode-test-a-body-is-indented-two ()
  (should (replique-clojure-test--reindents-to-itself
           "(defn foo [x]\n  (inc x))\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(when x\n  (println 1)\n  (println 2))\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(let [x 1\n      y 2]\n  (+ x y))\n")))

(ert-deftest replique-clojure-mode-test-what-takes-no-head-lines-up ()
  ;; :block 0 - every element of it is a body element
  (should (replique-clojure-test--reindents-to-itself
           "(cond\n  a 1\n  b 2)\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(do\n  (a)\n  (b))\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(try\n  (a)\n  (catch Exception e\n    (b)))\n")))

(ert-deftest replique-clojure-mode-test-the-arguments-of-a-call-line-up ()
  ;; not a form with a rule: the arguments align under the first of them
  (should (replique-clojure-test--reindents-to-itself
           "(println 1\n         2)\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(map inc\n     [1 2])\n")))

(ert-deftest replique-clojure-mode-test-a-collection-lines-up-inside-itself ()
  (should (replique-clojure-test--reindents-to-itself
           "[1\n 2]\n"))
  (should (replique-clojure-test--reindents-to-itself
           "{:a 1\n :b 2}\n"))
  (should (replique-clojure-test--reindents-to-itself
           "#{1\n  2}\n")))

(ert-deftest replique-clojure-mode-test-a-threading-form-lines-up-its-steps ()
  (should (replique-clojure-test--reindents-to-itself
           "(-> x\n    inc\n    dec)\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(->> xs\n     (map inc)\n     (filter odd?))\n"))
  ;; Under the step before it rather than under the first of them, which
  ;; is the same column until two steps are written on one line
  (should (equal "(-> x\n    (foo) (baz)\n          (bar))\n"
                 (replique-clojure-test--reindent "(-> x\n(foo) (baz)\n(bar))\n")))
  ;; and a threading macro is whatever ends like one
  (should (replique-clojure-test--reindents-to-itself
           "(some->> xs\n         (map inc))\n")))

(ert-deftest replique-clojure-mode-test-an-ns-form-is-indented ()
  (should (replique-clojure-test--reindents-to-itself
           "(ns foo.bar\n  (:require [clojure.string :as s]\n            [clojure.set :as set]))\n")))

(ert-deftest replique-clojure-mode-test-a-type-indents-its-methods ()
  (should (replique-clojure-test--reindents-to-itself
           "(defrecord Foo [a b]\n  Bar\n  (baz [this]\n    1))\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(reify\n  Bar\n  (baz [this]\n    1))\n")))

(ert-deftest replique-clojure-mode-test-a-function-literal-is-a-form ()
  ;; Indented from where the form starts, which for a `#(' is the `#' and
  ;; not the bracket after it - which is where cljfmt indents it from
  (should (replique-clojure-test--reindents-to-itself
           "(map #(do\n       (inc %))\n     xs)\n")))

(ert-deftest replique-clojure-mode-test-a-reader-conditional-is-indented ()
  ;; Its elements line up with one another.  They are platforms and what
  ;; each of them stands for, so the first of them is not the head of a
  ;; call and what follows it does not line up under the second
  (should (replique-clojure-test--reindents-to-itself
           "#?(:clj 1\n   :cljs 2)\n"))
  (should (replique-clojure-test--reindents-to-itself
           "#?@(:clj [1]\n    :cljs [2])\n"))
  (should (replique-clojure-test--reindents-to-itself
           "#?(:clj\n   (a)\n   :cljs\n   (b))\n")))

(ert-deftest replique-clojure-mode-test-metadata-goes-with-what-it-is-on ()
  (should (replique-clojure-test--reindents-to-itself
           "(defn ^:private foo [x]\n  x)\n")))

(ert-deftest replique-clojure-mode-test-a-string-over-several-lines-is-left-alone ()
  ;; whoever wrote those lines meant them: spaces pushed into a string are
  ;; spaces pushed into the value
  (should (replique-clojure-test--left-alone
           "(def s \"one\ntwo\n  three\")\n"))
  (should (replique-clojure-test--left-alone
           "(defn f\n  \"A docstring\n  over two lines.\"\n  []\n  1)\n")))

(ert-deftest replique-clojure-mode-test-a-body-is-two-in-from-its-form ()
  ;; Not from the line the form is written on.  A form nested inside
  ;; another is indented from where it begins, wherever that is
  (should (equal "(when a\n  (when b\n    (c)))\n"
                 (replique-clojure-test--reindent
                  "(when a\n(when b\n(c)))\n"))))

(ert-deftest replique-clojure-mode-test-a-rule-reaches-what-is-written-inside ()
  ;; :inner - a method of a protocol is a body without anybody having
  ;; written a rule for the name of the method
  (should (replique-clojure-test--reindents-to-itself
           "(defprotocol P\n  (m [this]\n    (a)))\n"))
  ;; two out, at the first argument: the functions letfn binds
  (should (replique-clojure-test--reindents-to-itself
           "(letfn [(f [x]\n          (inc x))]\n  (f 1))\n"))
  ;; and a form with no rule anywhere above it lines its arguments up
  (should (replique-clojure-test--reindents-to-itself
           "(foo bar\n     baz)\n")))

(ert-deftest replique-clojure-mode-test-a-rule-counts-the-arguments-it-takes ()
  ;; :block 1 - the first argument is not body, everything after it is
  (should (replique-clojure-test--reindents-to-itself
           "(when-let [x 1]\n  (a)\n  (b))\n"))
  ;; :block 2
  (should (replique-clojure-test--reindents-to-itself
           "(condp = x\n  1 :one\n  2 :two)\n"))
  ;; and the argument the count reaches is not body: the condition of an
  ;; `if' written on a line of its own goes one in, and the two branches
  ;; after it go two
  (should (equal "(if\n a\n  b\n  c)\n"
                 (replique-clojure-test--reindent "(if\na\nb\nc)\n"))))

(ert-deftest replique-clojure-mode-test-what-is-discarded-is-not-an-argument ()
  ;; `#_' is read and thrown away, so what follows it is the argument it
  ;; would have been without it
  ;; `y' is the condition and goes where a condition goes, rather than
  ;; being the first branch because something unread came before it
  (should (equal "(if\n #_ x\n y\n  z)\n"
                 (replique-clojure-test--reindent "(if\n#_ x\ny\nz)\n"))))

(ert-deftest replique-clojure-mode-test-a-comment-is-not-lined-up-under ()
  ;; A comment at the end of a line is not a step of the threading form it
  ;; is written in, so what comes after it lines up with the step before
  (should (replique-clojure-test--reindents-to-itself
           "(->> xs\n     (map inc) ; why\n     (filter odd?))\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(foo bar ; why\n     baz)\n")))

(ert-deftest replique-clojure-mode-test-metadata-does-not-make-two-forms ()
  ;; `^:private x' is one form written over two lines, and the second half
  ;; is not something written inside the first
  (should (replique-clojure-test--reindents-to-itself
           "(def ^{:doc \"a\"}\n  x 1)\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(let [x ^long\n      (foo)]\n  x)\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(foo bar\n     ^:a\n     baz)\n")))

(ert-deftest replique-clojure-mode-test-a-collection-opens-as-wide-as-it-is ()
  (should (replique-clojure-test--reindents-to-itself "[a\n b]\n"))
  (should (replique-clojure-test--reindents-to-itself "{:a 1\n :b 2}\n"))
  (should (replique-clojure-test--reindents-to-itself "#{a\n  b}\n"))
  (should (replique-clojure-test--reindents-to-itself "#(inc\n  %)\n"))
  ;; a value written on a line of its own belongs to the map, not to its key
  (should (replique-clojure-test--reindents-to-itself "{:a\n 1}\n")))

(ert-deftest replique-clojure-mode-test-a-quoted-form-is-still-in-its-form ()
  ;; The quote is stepped over, so what it quotes is placed by whatever the
  ;; whole of it is written in
  (should (replique-clojure-test--reindents-to-itself
           "(eval\n '(do\n    (a)\n    (b)))\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(run-tests\n 'a\n 'b)\n")))

(ert-deftest replique-clojure-mode-test-a-rule-of-somebody-own-is-read ()
  (let ((replique-clojure-semantic-indent-rules '(("my-when" . ((:block 1))))))
    (should (equal "(my-when a\n  (b))\n"
                   (replique-clojure-test--reindent "(my-when a\n(b))\n")))))

(ert-deftest replique-clojure-mode-test-a-macro-under-an-alias-is-the-same-macro ()
  ;; A rule names a macro, and a macro reached through an alias is it
  (should (replique-clojure-test--reindents-to-itself
           "(c/when a\n  (b))\n"))
  (should (replique-clojure-test--reindents-to-itself
           "(clojure.core/when a\n  (b))\n")))

(ert-deftest replique-clojure-mode-test-indenting-a-region-and-a-line-agree ()
  ;; The region is indented by following one reading of each form rather
  ;; than reading it again a line at a time, so the two have to agree
  (dolist (text '("(defn f [x]\n(let [y 1]\n(+ x y\n(foo bar\nbaz))))\n"
                  "(ns a.b\n(:require [c :as d]\n[e :as f]))\n"
                  "(-> x\n(foo)\n(bar 1\n2))\n"
                  "(deftype T [a]\nP\n(m [this]\n(a)))\n"
                  "#?(:clj 1\n:cljs 2)\n"))
    (should (equal (replique-clojure-test--reindent text)
                   (with-temp-buffer
                     (let ((replique-clojure-ensure-grammars nil))
                       (replique-clojure-mode))
                     (insert text)
                     (goto-char (point-min))
                     (while (re-search-forward "^[ \t]+" nil t) (replace-match ""))
                     (goto-char (point-min))
                     (while (< (point) (point-max))
                       (replique-clojure-indent-line)
                       (forward-line 1))
                     (buffer-string))))))

(ert-deftest replique-clojure-mode-test-indenting-asks-for-no-grammar ()
  (with-temp-buffer
    (insert "(when a\n(b))\n")
    (should (= 2 (progn (goto-char (point-min))
                        (forward-line 1)
                        (replique-clojure-indent-column (point)))))))

(ert-deftest replique-clojure-mode-test-indenting-settles ()
  ;; whatever it does, doing it twice does it once
  (dolist (text '("(defn foo [x]\n(inc x))\n"
                  "(let [x 1]\n(+ x\n1))\n"
                  "(cond\na 1\nb 2)\n"
                  "(-> x\ninc)\n"
                  "{:a 1\n:b 2}\n"))
    (let ((once (replique-clojure-test--reindent text)))
      (should (equal once (replique-clojure-test--reindent once))))))

(provide 'replique-clojure-mode-test)

;;; replique-clojure-mode-test.el ends here
