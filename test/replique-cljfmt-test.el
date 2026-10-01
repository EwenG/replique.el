;;; replique-cljfmt-test.el --- Tests for formatting the way cljfmt does  -*- lexical-binding: t; -*-

;;; Commentary:

;; What `replique-cljfmt' reads and what `replique-format-buffer' writes.
;;
;; The tables at the end are not opinions.  Each pair is a file and what
;; `clojure-lsp format' made of it - clojure-lsp 2026.05.05, cljfmt
;; underneath - under the configuration the table says, which is how they
;; were written down: the point of formatting here is to agree with it, so
;; the only thing a test can say is whether it does.

;;; Code:

(require 'ert)
(require 'replique-cljfmt)
(require 'replique-format)

(defmacro replique-cljfmt-test--in-project (files &rest body)
  "Run BODY in a project directory holding FILES, a list of (NAME . TEXT).
`default-directory' is the project, and nothing outside it is read: not
the global clojure-lsp settings, nor a configuration read before."
  (declare (indent 1))
  `(let* ((directory (file-name-as-directory (make-temp-file "replique-cljfmt" t)))
          (process-environment (cons (concat "XDG_CONFIG_HOME=" directory "xdg")
                                     process-environment))
          (replique-cljfmt-read-project-config t))
     (unwind-protect
         (progn
           (replique-cljfmt-forget)
           (dolist (file ,files)
             (let ((path (expand-file-name (car file) directory)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr file)))))
           (let ((default-directory directory))
             ,@body))
       (replique-cljfmt-forget)
       (delete-directory directory t))))

(defun replique-cljfmt-test--format (text &optional config)
  "TEXT formatted in a project whose `.cljfmt.edn' is CONFIG."
  (replique-cljfmt-test--in-project
      (and config (list (cons ".cljfmt.edn" config)))
    (with-temp-buffer
      (replique-clojure-mode)
      (insert text)
      (replique-format-buffer)
      (buffer-string))))

(defun replique-cljfmt-test--table (table &optional config)
  "The pairs of TABLE that do not format as they are said to, under CONFIG."
  (seq-remove (lambda (pair)
                (equal (cadr pair) (replique-cljfmt-test--format (car pair) config)))
              table))


;;; Regexes

(ert-deftest replique-cljfmt-test-a-lookahead-is-followed ()
  ;; cljfmt's own rule for definitions
  (let ((regex (replique-cljfmt-regex "^def(?!ault)(?!late)(?!er)")))
    (dolist (name '("def" "defn" "defnc" "define" "defonce"))
      (should (replique-cljfmt-regex-match regex name)))
    (dolist (name '("default" "deflate" "defer" "undef" "xdef"))
      (should-not (replique-cljfmt-regex-match regex name))))
  (let ((regex (replique-cljfmt-regex "a(?=b)")))
    (should (replique-cljfmt-regex-match regex "xacab"))
    (should-not (replique-cljfmt-regex-match regex "xacad"))))

(ert-deftest replique-cljfmt-test-java-is-translated ()
  (should (replique-cljfmt-regex-match (replique-cljfmt-regex "^with-") "with-open"))
  (should-not (replique-cljfmt-regex-match (replique-cljfmt-regex "^with-") "x-with-"))
  ;; found anywhere, as `re-find' finds
  (should (replique-cljfmt-regex-match (replique-cljfmt-regex "foo|bar") "xbarx"))
  (should (replique-cljfmt-regex-match (replique-cljfmt-regex "^\\w+\\d{2}$") "ab12"))
  (should-not (replique-cljfmt-regex-match (replique-cljfmt-regex "^\\w+\\d{2}$") "ab1"))
  (should (replique-cljfmt-regex-match (replique-cljfmt-regex "^[^\\]x-]+$") "abc"))
  (should-not (replique-cljfmt-regex-match (replique-cljfmt-regex "^[^\\]x-]+$") "a-c"))
  (should (replique-cljfmt-regex-match (replique-cljfmt-regex "\\Q.*\\E") "a.*b"))
  (should-not (replique-cljfmt-regex-match (replique-cljfmt-regex "\\Q.*\\E") "ab"))
  ;; and case matters
  (should-not (replique-cljfmt-regex-match (replique-cljfmt-regex "^def") "DEFN")))

(ert-deftest replique-cljfmt-test-what-cannot-be-translated-says-so ()
  (should-error (replique-cljfmt-regex "(?<=a)b"))
  (should-error (replique-cljfmt-regex "(?i)a"))
  (should-error (replique-cljfmt-regex "a(?!b)|c")))


;;; Reading the configuration

(ert-deftest replique-cljfmt-test-edn-is-read ()
  (should (equal (replique-cljfmt-read-edn
                  "{:a [1 \"s\" true false nil] b #re \"x\\\\.y\" :c #\"z\" :d {:e :f}}")
                 '(:map (:a . [1 "s" t :false nil])
                        ((:symbol . "b") . (:regex . "x\\.y"))
                        (:c . (:regex . "z"))
                        (:d . (:map (:e . :f)))))))

(ert-deftest replique-cljfmt-test-the-project-configuration-is-read ()
  (replique-cljfmt-test--in-project
      '((".lsp/config.edn" . "{:cljfmt {:indent-line-comments? true :extra-indents {a [[:block 1]]}}}")
        (".cljfmt.edn" . "{:indent-line-comments? false :extra-indents {b [[:inner 0]]}}"))
    (let* ((config (replique-cljfmt-config default-directory '(("c" . ((:block 2))))))
           (rules (plist-get config :rules)))
      ;; The file is merged over clojure-lsp's settings, deeply
      (should (equal nil (replique-cljfmt-option config :indent-line-comments? t)))
      (should (assoc '(name "a") rules))
      (should (assoc '(name "b") rules))
      ;; and a buffer's own rules over both
      (should (equal '((:block 2)) (nth 1 (assoc '(name "c") rules))))
      ;; with cljfmt's defaults still there
      (should (assoc '(name "let") rules))
      (should (seq-find (lambda (rule) (eq 'regex (car (car rule)))) rules)))))

(ert-deftest replique-cljfmt-test-a-config-path-is-followed ()
  (replique-cljfmt-test--in-project
      '((".lsp/config.edn" . "{:cljfmt-config-path \"fmt/cljfmt.edn\"}")
        (".cljfmt.edn" . "{:extra-indents {a [[:block 1]]}}")
        ("fmt/cljfmt.edn" . "{:extra-indents {b [[:block 1]]}}"))
    (let ((rules (plist-get (replique-cljfmt-config default-directory) :rules)))
      (should (assoc '(name "b") rules))
      (should-not (assoc '(name "a") rules)))))

(ert-deftest replique-cljfmt-test-a-changed-configuration-is-read-again ()
  (replique-cljfmt-test--in-project '((".cljfmt.edn" . "{}"))
    (should-not (replique-cljfmt-option (replique-cljfmt-config default-directory)
                                        :indent-line-comments?))
    (with-temp-file ".cljfmt.edn" (insert "{:indent-line-comments? true}"))
    (set-file-times ".cljfmt.edn" (time-add nil 10))
    (let ((replique-cljfmt--recheck-seconds -1))
      (should (replique-cljfmt-option (replique-cljfmt-config default-directory)
                                      :indent-line-comments?)))))

(ert-deftest replique-cljfmt-test-rules-are-tried-in-cljfmt-order ()
  (let* ((config (replique-cljfmt-config nil '(("my.ns/foo" . ((:block 1)))
                                               ("foo" . ((:block 1))))))
         (rules (plist-get config :rules))
         (position (lambda (rule) (seq-position rules rule))))
    ;; the deepest :inner first, then qualified symbols, plain ones, regexes
    (should (equal '(name "letfn") (car (car rules))))
    (should (< (funcall position (assoc '(qualified "my.ns" "foo") rules))
               (funcall position (assoc '(name "foo") rules))
               (funcall position (seq-find (lambda (rule) (eq 'regex (car (car rule))))
                                           rules))))))


;;; Indenting

(ert-deftest replique-cljfmt-test-a-block-body-starts-its-own-line ()
  ;; Or it lines up with the arguments, the way cljfmt does
  (should (equal "(let [x 1] (foo)\n     (bar))\n"
                 (replique-cljfmt-test--format "(let [x 1] (foo)\n(bar))\n")))
  (should (equal "(if x y\n    z)\n"
                 (replique-cljfmt-test--format "(if x y\nz)\n"))))

(ert-deftest replique-cljfmt-test-definitions-and-with-forms-have-bodies ()
  ;; cljfmt's fuzzy rules, which name families of macros
  (should (equal "(defstate foo\n  :start 1)\n"
                 (replique-cljfmt-test--format "(defstate foo\n:start 1)\n")))
  (should (equal "(with-foo [x y]\n  (bar))\n"
                 (replique-cljfmt-test--format "(with-foo [x y]\n(bar))\n")))
  (should (equal "(default foo\n         (bar))\n"
                 (replique-cljfmt-test--format "(default foo\n(bar))\n"))))

(ert-deftest replique-cljfmt-test-a-buffer-rule-comes-first ()
  (let ((replique-clojure-semantic-indent-rules '(("with-foo" . ((:block 1))))))
    (should (equal "(with-foo [x y] (a)\n          (bar))\n"
                   (replique-cljfmt-test--format "(with-foo [x y] (a)\n(bar))\n")))))

(ert-deftest replique-cljfmt-test-a-comment-line-is-left-where-it-is ()
  ;; unless the configuration says to indent them, and then only `;;'
  (should (equal "(foo\n;; c\n a)\n"
                 (replique-cljfmt-test--format "(foo\n;; c\na)\n")))
  (should (equal "(foo\n ;; c\n   ; d\n a)\n"
                 (replique-cljfmt-test--format "(foo\n;; c\n   ; d\na)\n"
                                               "{:indent-line-comments? true}"))))

(ert-deftest replique-cljfmt-test-indenting-a-region-reads-the-project ()
  (replique-cljfmt-test--in-project '((".cljfmt.edn" . "{:extra-indents {frob [[:block 1]]}}"))
    (with-temp-buffer
      (replique-clojure-mode)
      (insert "(frob a\n(b))\n")
      (indent-region (point-min) (point-max))
      (should (equal "(frob a\n  (b))\n" (buffer-string))))))


;;; Formatting

(ert-deftest replique-cljfmt-test-a-buffer-that-does-not-read-is-left-alone ()
  (should-error (replique-cljfmt-test--format "(foo\n  (bar)\n") :type 'user-error)
  ;; but a token cljfmt reads and this reader does not is no reason
  (should (equal "(.f x ^String/1 (a)\n    b)\n"
                 (replique-cljfmt-test--format "(.f x ^String/1 (a)\nb)\n"))))

(defconst replique-cljfmt-test--default-table
  '(
    ("(a\n\n  )\n\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a)\n\n  ;; c\n(b)\n"
     "(a)\n\n  ;; c\n(b)\n")
    ("(a\n  )\n  ;; c\n(b)\n"
     "(a)\n  ;; c\n(b)\n")
    ("(a  )\n  ;; c\n(b)\n"
     "(a)\n  ;; c\n(b)\n")
    ("[(a\n )\n     ;; c\n (b)]\n"
     "[(a)\n     ;; c\n (b)]\n")
    ("(a\n )  ;; c\n(b)\n"
     "(a)  ;; c\n(b)\n")
    ("(a ;; x\n  )\n   ;; c\n(b)\n"
     "(a ;; x\n )\n   ;; c\n(b)\n")
    ("(a\n  )\n\n\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a\n  )\n  ;c\n(b)\n"
     "(a)\n  ;c\n(b)\n")
    ("(foo (a\n  ))\n  ;; c\n(b)\n"
     "(foo (a))\n  ;; c\n(b)\n")
    ("(a\n  )\n\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a\n\n  )\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a\n\n)\n\n  ;; c\n(b)\n"
     "(a)\n\n  ;; c\n(b)\n")
    ("(a\n\n  )\n\n  (b)\n"
     "(a)\n\n(b)\n")
    ("[(a\n\n )\n\n     ;; c\n (b)]\n"
     "[(a)\n\n;; c\n (b)]\n")
    ("(a\n\n  )\n\n  ;; c\n\n\n(b)\n"
     "(a)\n\n;; c\n\n(b)\n")
    ("(foo ;; c\n)\n\n\n(b)\n"
     "(foo ;; c\n )\n(b)\n")
    ("(foo ;; c\n\n\n)\n(b)\n"
     "(foo ;; c\n )\n(b)\n")
    ("(foo\n\n\n;; c\n a)\n"
     "(foo\n\n;; c\n a)\n")
    ("\n\n\n(ns a)\n"
     "\n\n\n(ns a)\n")
    ("\n\n(ns a)\n"
     "\n\n(ns a)\n")
    ("(a ;; x\n  )\n\n  ;; c\n(b)\n"
     "(a ;; x\n )\n\n  ;; c\n(b)\n")
    ("(a\n\n\n)"
     "(a)")
    ("(a\n\n\n)\n"
     "(a)\n")
    ("(a\n\n\n   )(b)\n"
     "(a)\n\n(b)\n")
    ("(foo (bar\n\n  ) \n\n )\n(b)\n"
     "(foo (bar))\n\n(b)\n")
    ("(a ,\n\n,\n\n b)\n"
     "(a\n\n b)\n")
    ("{:a 1\n\n\n :b 2}\n"
     "{:a 1\n\n :b 2}\n")
    ("(a)\n\n\n\n;; end\n"
     "(a)\n\n;; end\n")
    ("(x (a\n  ) )\n  ;; c\n  (b)\n"
     "(x (a))\n  ;; c\n(b)\n")
    ("(let [a 1\n\n\n      b 2]\n  a)\n"
     "(let [a 1\n\n      b 2]\n  a)\n")
    ("(f #_ x\n\n\n y)\n"
     "(f #_x\n\n y)\n")
    ("#_ (foo)\n"
     "#_(foo)\n")
    ("' (a)\n"
     "'(a)\n")
    ("@ x\n"
     "@x\n")
    ("`(~ @x ~ (y))\n"
     "`(~ @x ~(y))\n")
    ("(foo\n  #_x\n  a\n  b)\n"
     "(foo\n #_x\n a\n b)\n")
    ("(foo #_x\n a\n b)\n"
     "(foo #_x\n a\n     b)\n")
    ("^:a  (foo)\n"
     "^:a  (foo)\n")
    ("^:a(foo)\n"
     "^:a (foo)\n")
    ("#inst \"2020\"\n"
     "#inst \"2020\"\n")
    ("(a)(b)[c]\"d\"e\n"
     "(a) (b) [c] \"d\" e\n")
    ("(a ;; c\n)\n"
     "(a ;; c\n )\n")
    ("(a\n ;; c\n )\n"
     "(a\n ;; c\n )\n")
    ("( ;; c\n a)\n"
     "( ;; c\n a)\n")
    ("(\n\n\n ;; c\n a)\n"
     "(\n\n;; c\n a)\n")
    ("(foo   bar    baz)\n"
     "(foo   bar    baz)\n")
    ("(foo\t\tbar)\n"
     "(foo\t\tbar)\n")
    ("(defn f [x]   \n  x)   \n   \n"
     "(defn f [x]\n  x)\n\n")
    ("(a)\n\n\n"
     "(a)\n\n\n")
    ("(a)  "
     "(a)")
    (";; only\n"
     ";; only\n")
    ("(comment\n  (foo)\n\n  )\n(defn g [])\n"
     "(comment\n  (foo))\n\n(defn g [])\n")
    ("(ns x.c\n  (:require [clojure.string :as str]))\n(str/join \",\"\nxs)\n(clojure.core/let [a 1]\n(foo))\n"
     "(ns x.c\n  (:require [clojure.string :as str]))\n(str/join \",\"\n          xs)\n(clojure.core/let [a 1]\n  (foo))\n")
    ("(ns x.d (:require [a.b :refer [my-let]]))\n(my-let [x 1]\nx)\n"
     "(ns x.d (:require [a.b :refer [my-let]]))\n(my-let [x 1]\n        x)\n")
    ("(when-let [x 1] (foo)\n(bar))\n"
     "(when-let [x 1] (foo)\n          (bar))\n")
    ("(cond-> x a\n(b))\n"
     "(cond-> x a\n        (b))\n")
    ("(defrecord R [a] P (m [x]\n1)\n(n [y] 2))\n"
     "(defrecord R [a] P (m [x]\n                     1)\n           (n [y] 2))\n")
    ("(proxy [Object] []\n(toString []\n\"x\"))\n"
     "(proxy [Object] []\n  (toString []\n    \"x\"))\n")
    ("(extend-protocol P\nString\n(m [x]\nx))\n"
     "(extend-protocol P\n  String\n  (m [x]\n    x))\n")
    ("(try\n(foo)\n(catch Exception e\n(bar))\n(finally\n(baz)))\n"
     "(try\n  (foo)\n  (catch Exception e\n    (bar))\n  (finally\n    (baz)))\n")
    ("(fn [x]\n x)\n"
     "(fn [x]\n  x)\n")
    ("(fn foo [x]\n x)\n"
     "(fn foo [x]\n  x)\n")
    ("(#(foo %\n1))\n"
     "(#(foo %\n       1))\n")
    ("(:require [a]\n [b])\n"
     "(:require [a]\n          [b])\n")
    ("(\"str\" a\n b)\n"
     "(\"str\" a\n       b)\n")
    ("(1 2\n 3)\n"
     "(1 2\n   3)\n")
    ("([a] b\n c)\n"
     "([a] b\n     c)\n")
    ("({:a 1} b\n c)\n"
     "({:a 1} b\n        c)\n")
    ("(foo\n bar\n baz)\n"
     "(foo\n bar\n baz)\n")
    ("(foo bar\n baz\n   qux)\n"
     "(foo bar\n     baz\n     qux)\n")
    ("(->> xs (map inc)\n(filter odd?))\n"
     "(->> xs (map inc)\n     (filter odd?))\n")
    ("(some-> x\n  (foo) (bar)\n  (baz))\n"
     "(some-> x\n        (foo) (bar)\n        (baz))\n")
    ("(let [x ^long\n(foo)]\nx)\n"
     "(let [x ^long\n      (foo)]\n  x)\n")
    ("(def ^{:doc \"a\"}\nx 1)\n"
     "(def ^{:doc \"a\"}\n  x 1)\n")
    ("(foo bar\n^:a\nbaz)\n"
     "(foo bar\n     ^:a\n     baz)\n")
    ("^:foo\n(bar)\n"
     "^:foo\n(bar)\n")
    ("#?(:clj\n(a)\n:cljs\n(b))\n"
     "#?(:clj\n   (a)\n   :cljs\n   (b))\n")
    ("#?@(:clj [1]\n:cljs [2])\n"
     "#?@(:clj [1]\n    :cljs [2])\n")
    ("(#?(:clj when :cljs if) x\ny)\n"
     "(#?(:clj when :cljs if) x\n  y)\n")
    ("#:foo{:a 1\n:b 2}\n"
     "#:foo{:a 1\n      :b 2}\n")
    ("#js {:a 1\n:b 2}\n"
     "#js {:a 1\n     :b 2}\n")
    ("(reify P\n(m [this]\n1))\n"
     "(reify P\n  (m [this]\n    1))\n")
    ("(letfn [(f [x]\n(inc x))\n(g [y]\ny)]\n(f 1))\n"
     "(letfn [(f [x]\n          (inc x))\n        (g [y]\n          y)]\n  (f 1))\n")
    ("(defprotocol P\n\"doc\"\n(m [this]\n\"x\"))\n"
     "(defprotocol P\n  \"doc\"\n  (m [this]\n    \"x\"))\n")
    ("(deftype T [a]\nP\n(m [_]\na))\n"
     "(deftype T [a]\n  P\n  (m [_]\n    a))\n")
    ("(case x\n1\n:a\n:b)\n"
     "(case x\n  1\n  :a\n  :b)\n")
    ("(do\n;; c\na)\n"
     "(do\n;; c\n  a)\n")
    ("(foo\n  ;; c\n  a)\n"
     "(foo\n  ;; c\n a)\n")
    ("(foo\n  ;c\n  a)\n"
     "(foo\n  ;c\n a)\n")
    ("(foo\n  ;;; c\n  a)\n"
     "(foo\n  ;;; c\n a)\n")
    ("(foo\n\t a)\n"
     "(foo\n a)\n")
    ("(a b\n\n c)\n"
     "(a b\n\n   c)\n")
    ("(if x\ny\nz)\n"
     "(if x\n  y\n  z)\n")
    ("(if\nx\ny\nz)\n"
     "(if\n x\n  y\n  z)\n")
    ("(if x y\n\nz)\n"
     "(if x y\n\n    z)\n")
    ("(do a\n\nb)\n"
     "(do a\n\n    b)\n")
    ("(do\na\nb)\n"
     "(do\n  a\n  b)\n")
    ("(defn f\n\"doc\nmore\n  doc\"\n[x])\n"
     "(defn f\n  \"doc\nmore\n  doc\"\n  [x])\n")
    ("(def s \"a\n   b\")\n"
     "(def s \"a\n   b\")\n")
    ("(foo #\"re\n  x\"\n bar)\n"
     "(foo #\"re\n  x\"\n     bar)\n")
    ("(with-redefs [a b]\n(c))\n"
     "(with-redefs [a b]\n  (c))\n")
    ("(with-x [a b] (c)\n(d))\n"
     "(with-x [a b] (c)\n  (d))\n")
    ("(defx foo\n(bar))\n"
     "(defx foo\n  (bar))\n")
    ("(default foo\n(bar))\n"
     "(default foo\n         (bar))\n")
    ("(deflate foo\n(bar))\n"
     "(deflate foo\n         (bar))\n")
    ("(GET \"/\" []\n(foo))\n"
     "(GET \"/\" []\n  (foo))\n")
    ("(alt! a\n(b))\n"
     "(alt! a\n      (b))\n")
    ("(go-loop [x 1]\n(recur x))\n"
     "(go-loop [x 1]\n  (recur x))\n"))
  "Files, and what `clojure-lsp format' makes of them without configuration.")

(defconst replique-cljfmt-test--qualified-config
  "{:indents {my.lib/frob [[:block 1]] x.q/local [[:block 1]] other.lib/deff [[:inner 0]] [my.lib #re \"^fr\"] [[:inner 0]] #re \"^zap-.*t$\" [[:inner 0]] zip [[:block 2]] [a.b thing] [[:block 1]] #re \"foo-ba(?=z)\" [[:inner 0]] with-transaction [[:block 1]]} :alias-map {cd a.b}}"
  "A configuration of every kind of key.")

(defconst replique-cljfmt-test--qualified-table
  '(
    ("(ns x.q (:require [my.lib :as ml] [other.lib :refer [deff]]))\n(ml/frob a\n(b))\n(frob a\n(b))\n(deff a\n(b))\n(my.lib/frob a\n(b))\n(x.q/local a\n(b))\n(local a\n(b))\n"
     "(ns x.q (:require [my.lib :as ml] [other.lib :refer [deff]]))\n(ml/frob a\n  (b))\n(frob a\n      (b))\n(deff a\n  (b))\n(my.lib/frob a\n  (b))\n(x.q/local a\n  (b))\n(local a\n  (b))\n")
    ("(ns x.r (:require [my [lib :as ml]]))\n(ml/frob a\n(b))\n"
     "(ns x.r (:require [my [lib :as ml]]))\n(ml/frob a\n  (b))\n")
    ("(ns x.s)\n(frob a\n(b))\n(zap-it a\n(b))\n(zip a\nb\nc)\n"
     "(ns x.s)\n(frob a\n      (b))\n(zap-it a\n  (b))\n(zip a\n     b\n  c)\n")
    ("(ns x.t (:require [a.b :as ab]))\n(ab/thing x\n(y))\n(cd/thing x\n(y))\n"
     "(ns x.t (:require [a.b :as ab]))\n(ab/thing x\n  (y))\n(cd/thing x\n  (y))\n")
    ("(foo-bar 1\n(x))\n(foo-baz 1\n(x))\n(defnc f []\n(x))\n"
     "(foo-bar 1\n         (x))\n(foo-baz 1\n  (x))\n(defnc f []\n  (x))\n")
    ("(ns x.u (:require [my.lib :as ml]))\n(with-transaction [tx db] (a)\n(b))\n(when x\ny)\n"
     "(ns x.u (:require [my.lib :as ml]))\n(with-transaction [tx db] (a)\n                  (b))\n(when x\n  y)\n"))
  "Files, and what `clojure-lsp format' makes of them under
`replique-cljfmt-test--qualified-config'.")

(defconst replique-cljfmt-test--options-config
  "{:indent-line-comments? true :function-arguments-indentation :zprint :remove-multiple-non-indenting-spaces? true :normalize-newlines-at-file-end? true}"
  "A configuration that turns on what is off by default.")

(defconst replique-cljfmt-test--options-table
  '(
    ("(a\n\n  )\n\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a)\n\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a\n  )\n  ;; c\n(b)\n"
     "(a)\n;; c\n(b)\n")
    ("(a  )\n  ;; c\n(b)\n"
     "(a)\n;; c\n(b)\n")
    ("[(a\n )\n     ;; c\n (b)]\n"
     "[(a)\n ;; c\n (b)]\n")
    ("(a\n )  ;; c\n(b)\n"
     "(a)  ;; c\n(b)\n")
    ("(a ;; x\n  )\n   ;; c\n(b)\n"
     "(a ;; x\n  )\n;; c\n(b)\n")
    ("(a\n  )\n\n\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a\n  )\n  ;c\n(b)\n"
     "(a)\n  ;c\n(b)\n")
    ("(foo (a\n  ))\n  ;; c\n(b)\n"
     "(foo (a))\n;; c\n(b)\n")
    ("(a\n  )\n\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a\n\n  )\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a\n\n)\n\n  ;; c\n(b)\n"
     "(a)\n\n;; c\n(b)\n")
    ("(a\n\n  )\n\n  (b)\n"
     "(a)\n\n(b)\n")
    ("[(a\n\n )\n\n     ;; c\n (b)]\n"
     "[(a)\n\n ;; c\n (b)]\n")
    ("(a\n\n  )\n\n  ;; c\n\n\n(b)\n"
     "(a)\n\n;; c\n\n(b)\n")
    ("(foo ;; c\n)\n\n\n(b)\n"
     "(foo ;; c\n  )\n(b)\n")
    ("(foo ;; c\n\n\n)\n(b)\n"
     "(foo ;; c\n  )\n(b)\n")
    ("(foo\n\n\n;; c\n a)\n"
     "(foo\n\n  ;; c\n  a)\n")
    ("\n\n\n(ns a)\n"
     "\n\n\n(ns a)\n")
    ("\n\n(ns a)\n"
     "\n\n(ns a)\n")
    ("(a ;; x\n  )\n\n  ;; c\n(b)\n"
     "(a ;; x\n  )\n\n;; c\n(b)\n")
    ("(a\n\n\n)"
     "(a)\n")
    ("(a\n\n\n)\n"
     "(a)\n")
    ("(a\n\n\n   )(b)\n"
     "(a)\n\n(b)\n")
    ("(foo (bar\n\n  ) \n\n )\n(b)\n"
     "(foo (bar))\n\n(b)\n")
    ("(a ,\n\n,\n\n b)\n"
     "(a\n\n  b)\n")
    ("{:a 1\n\n\n :b 2}\n"
     "{:a 1\n\n :b 2}\n")
    ("(a)\n\n\n\n;; end\n"
     "(a)\n\n;; end\n")
    ("(x (a\n  ) )\n  ;; c\n  (b)\n"
     "(x (a))\n;; c\n(b)\n")
    ("(let [a 1\n\n\n      b 2]\n  a)\n"
     "(let [a 1\n\n      b 2]\n  a)\n")
    ("(f #_ x\n\n\n y)\n"
     "(f #_x\n\n  y)\n")
    ("#_ (foo)\n"
     "#_(foo)\n")
    ("' (a)\n"
     "'(a)\n")
    ("@ x\n"
     "@x\n")
    ("`(~ @x ~ (y))\n"
     "`(~ @x ~(y))\n")
    ("(foo\n  #_x\n  a\n  b)\n"
     "(foo\n  #_x\n  a\n  b)\n")
    ("(foo #_x\n a\n b)\n"
     "(foo #_x\n  a\n     b)\n")
    ("^:a  (foo)\n"
     "^:a (foo)\n")
    ("^:a(foo)\n"
     "^:a (foo)\n")
    ("#inst \"2020\"\n"
     "#inst \"2020\"\n")
    ("(a)(b)[c]\"d\"e\n"
     "(a) (b) [c] \"d\" e\n")
    ("(a ;; c\n)\n"
     "(a ;; c\n  )\n")
    ("(a\n ;; c\n )\n"
     "(a\n  ;; c\n  )\n")
    ("( ;; c\n a)\n"
     "( ;; c\n a)\n")
    ("(\n\n\n ;; c\n a)\n"
     "(\n\n ;; c\n a)\n")
    ("(foo   bar    baz)\n"
     "(foo bar baz)\n")
    ("(foo\t\tbar)\n"
     "(foo bar)\n")
    ("(defn f [x]   \n  x)   \n   \n"
     "(defn f [x]\n  x)\n")
    ("(a)\n\n\n"
     "(a)\n")
    ("(a)  "
     "(a)\n")
    (";; only\n"
     ";; only\n")
    ("(comment\n  (foo)\n\n  )\n(defn g [])\n"
     "(comment\n  (foo))\n\n(defn g [])\n")
    ("(ns x.c\n  (:require [clojure.string :as str]))\n(str/join \",\"\nxs)\n(clojure.core/let [a 1]\n(foo))\n"
     "(ns x.c\n  (:require [clojure.string :as str]))\n(str/join \",\"\n          xs)\n(clojure.core/let [a 1]\n  (foo))\n")
    ("(ns x.d (:require [a.b :refer [my-let]]))\n(my-let [x 1]\nx)\n"
     "(ns x.d (:require [a.b :refer [my-let]]))\n(my-let [x 1]\n        x)\n")
    ("(when-let [x 1] (foo)\n(bar))\n"
     "(when-let [x 1] (foo)\n          (bar))\n")
    ("(cond-> x a\n(b))\n"
     "(cond-> x a\n        (b))\n")
    ("(defrecord R [a] P (m [x]\n1)\n(n [y] 2))\n"
     "(defrecord R [a] P (m [x]\n                     1)\n           (n [y] 2))\n")
    ("(proxy [Object] []\n(toString []\n\"x\"))\n"
     "(proxy [Object] []\n  (toString []\n    \"x\"))\n")
    ("(extend-protocol P\nString\n(m [x]\nx))\n"
     "(extend-protocol P\n  String\n  (m [x]\n    x))\n")
    ("(try\n(foo)\n(catch Exception e\n(bar))\n(finally\n(baz)))\n"
     "(try\n  (foo)\n  (catch Exception e\n    (bar))\n  (finally\n    (baz)))\n")
    ("(fn [x]\n x)\n"
     "(fn [x]\n  x)\n")
    ("(fn foo [x]\n x)\n"
     "(fn foo [x]\n  x)\n")
    ("(#(foo %\n1))\n"
     "(#(foo %\n       1))\n")
    ("(:require [a]\n [b])\n"
     "(:require [a]\n          [b])\n")
    ("(\"str\" a\n b)\n"
     "(\"str\" a\n       b)\n")
    ("(1 2\n 3)\n"
     "(1 2\n   3)\n")
    ("([a] b\n c)\n"
     "([a] b\n     c)\n")
    ("({:a 1} b\n c)\n"
     "({:a 1} b\n        c)\n")
    ("(foo\n bar\n baz)\n"
     "(foo\n  bar\n  baz)\n")
    ("(foo bar\n baz\n   qux)\n"
     "(foo bar\n     baz\n     qux)\n")
    ("(->> xs (map inc)\n(filter odd?))\n"
     "(->> xs (map inc)\n     (filter odd?))\n")
    ("(some-> x\n  (foo) (bar)\n  (baz))\n"
     "(some-> x\n        (foo) (bar)\n        (baz))\n")
    ("(let [x ^long\n(foo)]\nx)\n"
     "(let [x ^long\n      (foo)]\n  x)\n")
    ("(def ^{:doc \"a\"}\nx 1)\n"
     "(def ^{:doc \"a\"}\n  x 1)\n")
    ("(foo bar\n^:a\nbaz)\n"
     "(foo bar\n     ^:a\n     baz)\n")
    ("^:foo\n(bar)\n"
     "^:foo\n(bar)\n")
    ("#?(:clj\n(a)\n:cljs\n(b))\n"
     "#?(:clj\n   (a)\n   :cljs\n   (b))\n")
    ("#?@(:clj [1]\n:cljs [2])\n"
     "#?@(:clj [1]\n    :cljs [2])\n")
    ("(#?(:clj when :cljs if) x\ny)\n"
     "(#?(:clj when :cljs if) x\n  y)\n")
    ("#:foo{:a 1\n:b 2}\n"
     "#:foo{:a 1\n      :b 2}\n")
    ("#js {:a 1\n:b 2}\n"
     "#js {:a 1\n     :b 2}\n")
    ("(reify P\n(m [this]\n1))\n"
     "(reify P\n  (m [this]\n    1))\n")
    ("(letfn [(f [x]\n(inc x))\n(g [y]\ny)]\n(f 1))\n"
     "(letfn [(f [x]\n          (inc x))\n        (g [y]\n          y)]\n  (f 1))\n")
    ("(defprotocol P\n\"doc\"\n(m [this]\n\"x\"))\n"
     "(defprotocol P\n  \"doc\"\n  (m [this]\n    \"x\"))\n")
    ("(deftype T [a]\nP\n(m [_]\na))\n"
     "(deftype T [a]\n  P\n  (m [_]\n    a))\n")
    ("(case x\n1\n:a\n:b)\n"
     "(case x\n  1\n  :a\n  :b)\n")
    ("(do\n;; c\na)\n"
     "(do\n  ;; c\n  a)\n")
    ("(foo\n  ;; c\n  a)\n"
     "(foo\n  ;; c\n  a)\n")
    ("(foo\n  ;c\n  a)\n"
     "(foo\n  ;c\n  a)\n")
    ("(foo\n  ;;; c\n  a)\n"
     "(foo\n  ;;; c\n  a)\n")
    ("(foo\n\t a)\n"
     "(foo\n  a)\n")
    ("(a b\n\n c)\n"
     "(a b\n\n   c)\n")
    ("(if x\ny\nz)\n"
     "(if x\n  y\n  z)\n")
    ("(if\nx\ny\nz)\n"
     "(if\n  x\n  y\n  z)\n")
    ("(if x y\n\nz)\n"
     "(if x y\n\n    z)\n")
    ("(do a\n\nb)\n"
     "(do a\n\n    b)\n")
    ("(do\na\nb)\n"
     "(do\n  a\n  b)\n")
    ("(defn f\n\"doc\nmore\n  doc\"\n[x])\n"
     "(defn f\n  \"doc\nmore\n  doc\"\n  [x])\n")
    ("(def s \"a\n   b\")\n"
     "(def s \"a\n   b\")\n")
    ("(foo #\"re\n  x\"\n bar)\n"
     "(foo #\"re\n  x\"\n     bar)\n")
    ("(with-redefs [a b]\n(c))\n"
     "(with-redefs [a b]\n  (c))\n")
    ("(with-x [a b] (c)\n(d))\n"
     "(with-x [a b] (c)\n  (d))\n")
    ("(defx foo\n(bar))\n"
     "(defx foo\n  (bar))\n")
    ("(default foo\n(bar))\n"
     "(default foo\n         (bar))\n")
    ("(deflate foo\n(bar))\n"
     "(deflate foo\n         (bar))\n")
    ("(GET \"/\" []\n(foo))\n"
     "(GET \"/\" []\n  (foo))\n")
    ("(alt! a\n(b))\n"
     "(alt! a\n      (b))\n")
    ("(go-loop [x 1]\n(recur x))\n"
     "(go-loop [x 1]\n  (recur x))\n"))
  "Files, and what `clojure-lsp format' makes of them under
`replique-cljfmt-test--options-config'.")

(ert-deftest replique-cljfmt-test-formatting-agrees-with-clojure-lsp ()
  (should-not (replique-cljfmt-test--table replique-cljfmt-test--default-table)))

(ert-deftest replique-cljfmt-test-every-kind-of-key-agrees-with-clojure-lsp ()
  (should-not (replique-cljfmt-test--table replique-cljfmt-test--qualified-table
                                           replique-cljfmt-test--qualified-config)))

(ert-deftest replique-cljfmt-test-the-options-agree-with-clojure-lsp ()
  (should-not (replique-cljfmt-test--table replique-cljfmt-test--options-table
                                           replique-cljfmt-test--options-config)))

(provide 'replique-cljfmt-test)

;;; replique-cljfmt-test.el ends here
