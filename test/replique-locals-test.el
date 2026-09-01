;;; replique-locals-test.el --- Tests for the locals analyzer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Which names are locals where.  Unlike the rest of the suite these need no
;; process: what they check is a reading of the parse, and a parse is
;; something a temporary buffer has.
;;
;; `replique-test' is loaded for what says whether the grammar is installed,
;; so that a run without one skips here for the same reason and in the same
;; words as it skips there.
;;
;; Each of them is written as one form with a | in it where the question is
;; asked - which reads as the buffer would look with point in it, and keeps
;; what is being asked next to what is being asked about.

;;; Code:

(require 'ert)
(require 'replique-test)
(require 'replique-locals)

(defun replique-locals-test--locals (text)
  "Return the locals in scope where | is in TEXT, nearest first."
  (replique-test-grammar)
  (with-temp-buffer
    (replique-clojure-mode)
    (insert text)
    (goto-char (point-min))
    (unless (search-forward "|" nil t)
      (error "The text says nowhere to look: %s" text))
    (let ((pos (match-beginning 0)))
      (delete-region (match-beginning 0) (match-end 0))
      (replique-locals-at pos))))

(defun replique-locals-test--names (text)
  "Return the names of the locals in scope where | is in TEXT."
  (mapcar #'car (replique-locals-test--locals text)))

;;; let and what is shaped like it

(ert-deftest replique-locals-test-a-let-binds-in-its-body ()
  (should (equal '("x") (replique-locals-test--names "(let [x 1] |)"))))

(ert-deftest replique-locals-test-a-let-does-not-bind-in-its-own-value ()
  ;; the x being bound is not the x being read.  Point is inside an
  ;; expression rather than where one would go: an empty one leaves a vector
  ;; of one, which binds nothing whatever the scope rule says
  (should (equal nil (replique-locals-test--names "(let [x (inc |)] 1)"))))

(ert-deftest replique-locals-test-a-let-binds-in-the-values-after-it ()
  (should (equal '("x") (replique-locals-test--names "(let [x 1 y (inc |)] y)"))))

(ert-deftest replique-locals-test-a-let-binds-in-order ()
  (should (equal '("y" "x") (replique-locals-test--names "(let [x 1 y 2] |)"))))

(ert-deftest replique-locals-test-the-last-of-two-of-a-name-is-the-one-that-counts ()
  (let ((locals (replique-locals-test--locals "(let [x 1 x 2] |)")))
    (should (equal '("x" "x") (mapcar #'car locals)))
    (should (equal 11 (cdr (assoc "x" locals))))))

(ert-deftest replique-locals-test-a-let-inside-a-let-shadows-it ()
  (let ((locals (replique-locals-test--locals "(let [x 1] (let [x 2] |))")))
    (should (equal '("x" "x") (mapcar #'car locals)))
    (should (equal 18 (cdr (assoc "x" locals))))))

(ert-deftest replique-locals-test-a-let-outside-a-let-is-still-in-scope ()
  (should (equal '("y" "x") (replique-locals-test--names "(let [x 1] (let [y 2] |))"))))

(ert-deftest replique-locals-test-a-let-binds-nothing-outside-itself ()
  (should (equal nil (replique-locals-test--names "(let [x 1] x) |"))))

(ert-deftest replique-locals-test-the-forms-shaped-like-let-bind-the-same-way ()
  (dolist (form '("loop" "when-let" "if-let" "when-some" "if-some"
                  "with-open" "with-local-vars" "dotimes"))
    (should (equal '("x")
                   (replique-locals-test--names (format "(%s [x 1] |)" form))))))

(ert-deftest replique-locals-test-binding-binds-no-local ()
  ;; it rebinds a var, and a var is what the process should be asked about
  (should (equal nil (replique-locals-test--names "(binding [*x* 1] |)"))))

(ert-deftest replique-locals-test-a-let-written-out-in-full-binds ()
  (should (equal '("x") (replique-locals-test--names "(clojure.core/let [x 1] |)"))))

(ert-deftest replique-locals-test-a-let-reached-through-an-alias-does-not ()
  (should (equal nil (replique-locals-test--names "(c/let [x 1] |)"))))

(ert-deftest replique-locals-test-a-let-with-no-vector-binds-nothing ()
  (should (equal nil (replique-locals-test--names "(let |)")))
  (should (equal nil (replique-locals-test--names "(let x |)"))))

;;; fn and what is shaped like it

(ert-deftest replique-locals-test-a-fn-binds-its-parameters ()
  (should (equal '("y" "x") (replique-locals-test--names "(fn [x y] |)"))))

(ert-deftest replique-locals-test-a-defn-binds-its-parameters ()
  (should (equal '("x") (replique-locals-test--names "(defn f [x] |)"))))

(ert-deftest replique-locals-test-the-name-of-a-fn-is-a-local ()
  ;; it is how it calls itself
  (should (equal '("x" "f") (replique-locals-test--names "(fn f [x] |)"))))

(ert-deftest replique-locals-test-the-name-of-a-defn-is-not ()
  ;; it is a var, and the process knows about it
  (should (equal '("x") (replique-locals-test--names "(defn f [x] |)"))))

(ert-deftest replique-locals-test-a-rest-argument-is-a-local-and-the-ampersand-is-not ()
  (should (equal '("ys" "x") (replique-locals-test--names "(fn [x & ys] |)"))))

(ert-deftest replique-locals-test-parameters-are-found-past-a-docstring ()
  (should (equal '("x")
                 (replique-locals-test--names "(defn f \"doc\" {:m 1} [x] |)"))))

(ert-deftest replique-locals-test-parameters-are-not-in-scope-in-the-docstring ()
  (should (equal nil (replique-locals-test--names "(defn f \"do|c\" [x] x)"))))

(ert-deftest replique-locals-test-only-the-arity-point-is-in-binds ()
  (should (equal '("x") (replique-locals-test--names "(fn ([x] |) ([x y] y))")))
  (should (equal '("y" "x") (replique-locals-test--names "(fn ([x] x) ([x y] |))"))))

(ert-deftest replique-locals-test-between-two-arities-nothing-is-bound ()
  (should (equal nil (replique-locals-test--names "(fn ([x] x) | ([x y] y))"))))

(ert-deftest replique-locals-test-a-fn-inside-a-let-binds-both ()
  (should (equal '("y" "x") (replique-locals-test--names "(let [x 1] (fn [y] |))"))))

;;; Patterns taken apart

(ert-deftest replique-locals-test-a-vector-pattern-binds-what-it-names ()
  (should (equal '("b" "a") (replique-locals-test--names "(let [[a b] v] |)"))))

(ert-deftest replique-locals-test-a-rest-pattern-binds-what-follows-the-ampersand ()
  (should (equal '("more" "a") (replique-locals-test--names "(let [[a & more] v] |)"))))

(ert-deftest replique-locals-test-as-binds-the-whole-of-it ()
  (should (equal '("all" "a") (replique-locals-test--names "(let [[a :as all] v] |)"))))

(ert-deftest replique-locals-test-a-pattern-inside-a-pattern-binds-too ()
  (should (equal '("b" "a") (replique-locals-test--names "(let [[a [b]] v] |)")))
  (should (equal '("c") (replique-locals-test--names "(let [{{:keys [c]} :m} m] |)"))))

(ert-deftest replique-locals-test-keys-binds-the-vector-it-is-given ()
  (should (equal '("b" "a") (replique-locals-test--names "(let [{:keys [a b]} m] |)")))
  (should (equal '("a") (replique-locals-test--names "(let [{:strs [a]} m] |)")))
  (should (equal '("a") (replique-locals-test--names "(let [{:syms [a]} m] |)"))))

(ert-deftest replique-locals-test-a-map-pattern-binds-its-keys ()
  ;; written the other way round: the name is the key and where to find it
  ;; is the value
  (should (equal '("a") (replique-locals-test--names "(let [{a :x} m] |)"))))

(ert-deftest replique-locals-test-a-qualified-key-binds-its-name ()
  (should (equal '("a") (replique-locals-test--names "(let [{:person/keys [a]} m] |)")))
  (should (equal '("bar") (replique-locals-test--names "(let [{:keys [foo/bar]} m] |)"))))

(ert-deftest replique-locals-test-keys-can-be-written-as-keywords ()
  (should (equal '("a") (replique-locals-test--names "(let [{:keys [:a]} m] |)"))))

(ert-deftest replique-locals-test-a-namespaced-map-pattern-binds-its-keys ()
  (should (equal '("a") (replique-locals-test--names "(let [#:person{:keys [a]} m] |)"))))

(ert-deftest replique-locals-test-or-binds-nothing-of-its-own ()
  ;; the names in it are bound by the :keys beside it, and what they are
  ;; written against are expressions
  (should (equal '("a") (replique-locals-test--names
                         "(let [{:keys [a] :or {a 1}} m] |)"))))

(ert-deftest replique-locals-test-as-of-a-map-binds-the-whole-of-it ()
  (should (equal '("whole" "a")
                 (replique-locals-test--names "(let [{:keys [a] :as whole} m] |)"))))

(ert-deftest replique-locals-test-parameters-are-taken-apart-the-same-way ()
  (should (equal '("b" "a") (replique-locals-test--names "(fn [{:keys [a]} [b]] |)"))))

(ert-deftest replique-locals-test-a-pattern-is-bound-where-a-name-would-be ()
  ;; sequential all the same: the map is not taken apart until what it is
  ;; bound to has been read
  (should (equal nil (replique-locals-test--names "(let [{:keys [a]} (inc |)] a)"))))

(ert-deftest replique-locals-test-a-destructured-local-says-where-it-is-bound ()
  (should (equal '(("a" . 15)) (replique-locals-test--locals "(let [{:keys [a]} m] |)"))))

(ert-deftest replique-locals-test-what-has-no-name-binds-nothing ()
  ;; what a number would be bound to has no name to be asked about
  (should (equal '("a") (replique-locals-test--names "(let [[a 1 \"s\"] v] |)"))))

;;; for and what is shaped like it

(ert-deftest replique-locals-test-a-for-binds-what-it-walks-over ()
  (should (equal '("y" "x") (replique-locals-test--names "(for [x xs y ys] |)")))
  (should (equal '("x") (replique-locals-test--names "(doseq [x xs] |)"))))

(ert-deftest replique-locals-test-the-let-of-a-for-binds ()
  (should (equal '("y" "x")
                 (replique-locals-test--names "(for [x xs :let [y (f x)]] |)"))))

(ert-deftest replique-locals-test-what-a-for-is-guarded-by-binds-nothing ()
  ;; :when and :while are written where a name would be and are expressions
  (should (equal '("x") (replique-locals-test--names "(for [x xs :when (f x)] |)")))
  (should (equal '("x") (replique-locals-test--names "(for [x xs :while (f x)] |)"))))

(ert-deftest replique-locals-test-a-for-is-sequential-too ()
  (should (equal '("x") (replique-locals-test--names "(for [x xs y (f |)] y)"))))

(ert-deftest replique-locals-test-a-for-takes-patterns-apart ()
  (should (equal '("b" "a") (replique-locals-test--names "(for [[a b] xs] |)"))))

;;; letfn

(ert-deftest replique-locals-test-letfn-binds-its-names-everywhere-in-itself ()
  (should (equal '("g" "f") (replique-locals-test--names "(letfn [(f [x] x) (g [y] y)] |)")))
  ;; in one another, which is what it is written for
  (should (equal '("y" "g" "f")
                 (replique-locals-test--names "(letfn [(f [x] x) (g [y] |)] (f 1))"))))

(ert-deftest replique-locals-test-the-parameters-of-one-letfn-name-are-its-own ()
  (should (equal '("x" "g" "f")
                 (replique-locals-test--names "(letfn [(f [x] |) (g [y] y)] (f 1))"))))

(ert-deftest replique-locals-test-a-letfn-name-can-be-written-with-several-arities ()
  (should (equal '("y" "x" "f")
                 (replique-locals-test--names "(letfn [(f ([x] x) ([x y] |))] (f 1))"))))

;;; deftype and what holds methods

(ert-deftest replique-locals-test-the-fields-of-a-deftype-are-in-scope-in-its-methods ()
  (should (equal '("x" "this" "b" "a")
                 (replique-locals-test--names "(deftype T [a b] P (m [this x] |))")))
  (should (equal '("b" "a")
                 (replique-locals-test--names "(defrecord T [a b] P (m [] |))"))))

(ert-deftest replique-locals-test-a-method-of-another-type-binds-nothing-here ()
  (should (equal '("b" "a")
                 (replique-locals-test--names "(deftype T [a b] P (m [this x] x) (n [] |))"))))

(ert-deftest replique-locals-test-what-holds-methods-binds-what-they-are-written-with ()
  (dolist (form '("reify" "proxy" "extend-type" "extend-protocol"))
    (should (equal '("x" "this")
                   (replique-locals-test--names
                    (format "(%s P (m [this x] |))" form))))))

;;; defmethod

(ert-deftest replique-locals-test-a-defmethod-binds-its-parameters ()
  (should (equal '("x") (replique-locals-test--names "(defmethod area :circle [x] |)"))))

(ert-deftest replique-locals-test-what-a-defmethod-dispatches-on-is-not-its-parameters ()
  ;; it can be written as a vector, which is why it is counted to
  (should (equal '("x")
                 (replique-locals-test--names "(defmethod area [::a ::b] [x] |)"))))

;;; What is named third

(ert-deftest replique-locals-test-a-catch-binds-what-it-caught ()
  (should (equal '("e") (replique-locals-test--names "(try x (catch Exception e |))"))))

(ert-deftest replique-locals-test-a-catch-binds-nothing-outside-itself ()
  (should (equal nil (replique-locals-test--names "(try | (catch Exception e e))"))))

(ert-deftest replique-locals-test-a-catch-binds-nothing-before-it-catches ()
  ;; inside the catch, where the name has not been given yet
  (should (equal nil (replique-locals-test--names "(try x (catch Exception | e e))"))))

(ert-deftest replique-locals-test-as-names-what-is-threaded ()
  (should (equal '("$") (replique-locals-test--names "(as-> x $ (f |))")))
  (should (equal nil (replique-locals-test--names "(as-> x | $ (f $))"))))

;;; The parameters nothing names

(ert-deftest replique-locals-test-a-function-literal-binds-what-is-written-in-it ()
  (should (equal '("%2" "%") (replique-locals-test--names "#(+ % %2 |)")))
  (should (equal '("%&" "%") (replique-locals-test--names "#(apply + % %& |)"))))

(ert-deftest replique-locals-test-a-function-literal-names-each-of-them-once ()
  (should (equal '("%") (replique-locals-test--names "#(+ % % |)"))))

(ert-deftest replique-locals-test-what-is-not-a-parameter-is-not-one-of-them ()
  (should (equal nil (replique-locals-test--names "#(inc |)")))
  (should (equal nil (replique-locals-test--names "#(f %x |)"))))

(ert-deftest replique-locals-test-a-function-literal-says-it-binds-where-it-is-written ()
  (should (equal '(("%" . 1)) (replique-locals-test--locals "#(inc % |)"))))

;;; Where a name is bound

(ert-deftest replique-locals-test-a-local-says-where-it-is-bound ()
  (let ((locals (replique-locals-test--locals "(let [x 1] |)")))
    (should (equal '(("x" . 7)) locals))))

(ert-deftest replique-locals-test-metadata-does-not-move-a-binding ()
  (let ((locals (replique-locals-test--locals "(fn [^long x] |)")))
    (should (equal '(("x" . 12)) locals))))

;;; What it says a little more of

(ert-deftest replique-locals-test-an-if-let-is-in-scope-in-both-branches ()
  ;; in the one that is taken, really.  Which branch a point is in is about
  ;; what the form means rather than about where the text is
  (should (equal '("x") (replique-locals-test--names "(if-let [x 1] x |)"))))

(ert-deftest replique-locals-test-what-was-commented-out-binds-as-it-would-have ()
  ;; the way the rest of replique reads a discard: what is behind it is
  ;; still code, and evaluating it is the way back from commenting it out
  (should (equal '("x") (replique-locals-test--names "#_(let [x 1] |)"))))

(ert-deftest replique-locals-test-quoted-data-binds-as-if-it-were-code ()
  (should (equal '("x") (replique-locals-test--names "'(let [x 1] |)"))))

;;; What is in scope where nothing is written

(ert-deftest replique-locals-test-a-string-is-inside-what-encloses-it ()
  ;; the locals of a body are the locals of the whole of it.  Whether
  ;; there is a name here to ask about is a different question
  (should (equal '("x") (replique-locals-test--names "(let [x 1] \"a str|ing\")"))))

(ert-deftest replique-locals-test-a-comment-is-inside-what-encloses-it ()
  (should (equal '("x") (replique-locals-test--names "(let [x 1] ;; a com|ment\n  x)"))))

(ert-deftest replique-locals-test-a-parameter-vector-covers-itself ()
  (should (equal '("y" "x") (replique-locals-test--names "(fn [x |y] x)"))))

;;; Nowhere in particular

(ert-deftest replique-locals-test-nothing-is-bound-at-the-top-level ()
  (should (equal nil (replique-locals-test--names "|")))
  (should (equal nil (replique-locals-test--names "(inc |)"))))

(ert-deftest replique-locals-test-nothing-is-bound-at-the-name-of-the-form ()
  (should (equal nil (replique-locals-test--names "(|let [x 1] x)"))))

(ert-deftest replique-locals-test-a-form-that-only-looks-like-one-binds-nothing ()
  (should (equal nil (replique-locals-test--names "(let-something [x 1] |)")))
  (should (equal nil (replique-locals-test--names "(fnord [x] |)"))))

(ert-deftest replique-locals-test-what-a-form-beside-this-one-binds-stays-there ()
  (should (equal '("b") (replique-locals-test--names "(let [a 1] a) (let [b 2] |)"))))

(ert-deftest replique-locals-test-the-nearest-of-them-comes-first ()
  ;; whatever kinds of form they were written by
  (should (equal '("c" "b" "a")
                 (replique-locals-test--names "(let [a 1] (fn [b] (let [c 2] |)))"))))

(ert-deftest replique-locals-test-the-nearest-of-two-of-a-name-is-the-one-assoc-finds ()
  (let ((locals (replique-locals-test--locals "(let [x 1] (fn [x] |))")))
    (should (equal '("x" "x") (mapcar #'car locals)))
    (should (equal 17 (cdr (assoc "x" locals))))))

(provide 'replique-locals-test)

;;; replique-locals-test.el ends here
