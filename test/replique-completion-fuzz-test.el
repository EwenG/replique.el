;;; replique-completion-fuzz-test.el --- Random buffers, read at every point  -*- lexical-binding: t; -*-

;;; Commentary:

;; A buffer is read wherever point is, and point is wherever somebody left
;; it: halfway through a name, inside a bracket nothing closes, behind a
;; quote, in the middle of a form that is not written yet.  What the reading
;; has to do there is answer or answer nothing, and what it must never do is
;; signal - a completion runs on `completion-at-point-functions', which is
;; called while somebody types, and an error there is an error in front of
;; them.
;;
;; So the text is generated rather than written out: forms cut off at a
;; random character, which is what a buffer looks like mid-typing, and
;; brackets and macros thrown together, which is what a parse has to survive.
;; Point is then put at every one of a handful of positions in each of them.
;;
;; What is asserted is not what is read - there is no right answer to a
;; random buffer - but the rules that hold whatever is read: it came back, it
;; is the shape a request is, and the region it says a candidate replaces is
;; a region that ends where point is.
;;
;; Seeded and stepped by hand rather than by `random', so that a run that
;; fails fails again on the next machine: what a fuzz test reports is the
;; text and the position, and those two are a test that can be written.
;;
;; A reading that never comes back is not caught here - it hangs the run
;; rather than failing it, since a timer does not fire inside a lisp loop
;; that does not end.  A hung test run says the same thing as a failed one
;; and says it less clearly, which is worth knowing before reading a run
;; that stopped saying anything.

;;; Code:

(require 'ert)
(require 'replique-test)
(require 'replique-completion)
(require 'replique-deps)
(require 'replique-edn)

;;; Random

(defvar replique-completion-fuzz--state 1
  "Where the generator has got to.")

(defun replique-completion-fuzz--seed (seed)
  "Start the generator again at SEED."
  (setq replique-completion-fuzz--state (+ 1 (* 7 seed))))

(defun replique-completion-fuzz--next (limit)
  "Return a number below LIMIT, the next of the sequence."
  (setq replique-completion-fuzz--state
        (mod (+ (* 1103515245 replique-completion-fuzz--state) 12345) 2147483648))
  (mod (/ replique-completion-fuzz--state 65536) limit))

(defun replique-completion-fuzz--pick (list)
  "Return one of LIST."
  (nth (replique-completion-fuzz--next (length list)) list))

;;; What a buffer holds

(defconst replique-completion-fuzz--pieces
  '("(" ")" "[" "]" "{" "}" "\"" "\\" ";" "#_" "#" "'" "^" "@" "~" "`" "," "~@"
    " " "\n" "  " "#{" "#(" "@(" "#?@"
    "defn" "def" "let" "fn" "loop" "if-let" "when-let" "doseq" "for" "try"
    "catch" "ns" "->" "->>" "some->" "doto" "recur" "quote"
    ":require" ":as" ":refer" ":import" ":only" ":rename" ":reload" ":load"
    "clojure.string" "str/join" "^String" "^{:tag String}" "s" "x" "xs"
    ".length" ".-field" "::alias/k" ":keyword" "::" "map" "Date." "String/"
    "\"a string\"" "\"clojure/core\"" "1" "nil" "true" "e")
  "The pieces a fuzzed buffer is thrown together out of.

The brackets and the reader macros are what a parse has to survive, and
the names are what the reading has to recognise between them.")

(defconst replique-completion-fuzz--forms
  '("(ns my.app (:require [clojure.string :as str] [clojure.set :refer [union]]))"
    "(ns my.app (:import [java.util Date UUID]) (:load \"clojure/core\"))"
    "(defn f [^String s] (.length s))"
    "(defn f [^String s] (-> s .length))"
    "(defn f [^java.util.Date d] (doto d .getTime))"
    "(let [x 1 {:keys [a b]} m [c] v] (+ x a c))"
    "(doseq [x xs :let [y (inc x)]] (println y))"
    "(try (f) (catch Exception e (.getMessage e)) (finally (g)))"
    "(defmethod foo :bar [x] (clojure.string/join \",\" x))"
    "(fn [a & rest] (str a ::keyword :other/keyword))"
    "(loop [i 0 acc []] (if (< i 10) (recur (inc i) (conj acc i)) acc))"
    "(defrecord Point [x y] Object (toString [this] (str x)))"
    "(ns my.app (:require [clojure (string) (set :as s)] :reload))"
    "(ns my.app (:refer-clojure :exclude [map]) (:require [clojure.string :refer [join] :rename {join j}]))"
    "(require '[clojure.string :as str] :verbose)"
    "(import '[java.util Date] 'java.util.UUID)")
  "Forms that are written the way somebody writes them.

Cut off at a random character, which is what a buffer holds while it is
being typed: half a name at the end of it and nothing closed after that.
Random pieces alone reach the parse and almost never reach the reading,
which only has something to say where a form is nearly a form.")

(defun replique-completion-fuzz--junk ()
  "Return a string of random pieces."
  (let ((count (replique-completion-fuzz--next 12))
        (text ""))
    (dotimes (_ count)
      (setq text (concat text (replique-completion-fuzz--pick
                               replique-completion-fuzz--pieces))))
    text))

(defun replique-completion-fuzz--cut (text)
  "Return TEXT cut off at a random character."
  (substring text 0 (replique-completion-fuzz--next (1+ (length text)))))

(defun replique-completion-fuzz--text ()
  "Return the text of a fuzzed buffer."
  (pcase (replique-completion-fuzz--next 6)
    (0 (replique-completion-fuzz--junk))
    (1 (replique-completion-fuzz--pick replique-completion-fuzz--forms))
    (2 (concat (replique-completion-fuzz--cut
                (replique-completion-fuzz--pick replique-completion-fuzz--forms))
               (replique-completion-fuzz--junk)))
    (3 (concat (replique-completion-fuzz--pick replique-completion-fuzz--forms)
               "\n"
               (replique-completion-fuzz--cut
                (replique-completion-fuzz--pick replique-completion-fuzz--forms))))
    (_ (replique-completion-fuzz--cut
        (replique-completion-fuzz--pick replique-completion-fuzz--forms)))))

;;; What must be true of what is read

(defun replique-completion-fuzz--names (locals)
  "Return what is wrong with LOCALS, the locals of a request, or nil."
  (cond
   ((not (listp locals)) "the locals are not a list")
   (t (catch 'wrong
        (dolist (local locals)
          (unless (and (plistp local) (stringp (plist-get local :name)))
            (throw 'wrong (format "a local is %S" local))))
        nil))))

(defun replique-completion-fuzz--ill-formed (context)
  "Return what CONTEXT holds that a request does not hold, or nil."
  (or (unless (plistp context) "the context is not a plist")
      (unless (keywordp (plist-get context :position))
        (format "the position is %S" (plist-get context :position)))
      (replique-completion-fuzz--names (plist-get context :locals))
      (catch 'wrong
        (dolist (key '(:ns :prefix :package :tag :target))
          (let ((value (plist-get context key)))
            (unless (or (null value) (stringp value))
              (throw 'wrong (format "the %s is %S" key value)))))
        ;; the namespace of a refer-clojure is the keyword, since a
        ;; refer-clojure names no namespace anywhere in itself
        (let ((namespace (plist-get context :namespace)))
          (unless (or (null namespace) (stringp namespace)
                      (eq :refer-clojure namespace))
            (throw 'wrong (format "the namespace is %S" namespace))))
        nil)))

(defun replique-completion-fuzz--problem ()
  "Return what is wrong with what is read at point, or nil when nothing is."
  (condition-case error
      (let* ((bounds (replique-completion--bounds))
             (context (if (replique-deps-form-at-p (point))
                          (replique-deps-context-at (point))
                        (replique-completion--code-context))))
        (or (unless (consp bounds) (format "the bounds are %S" bounds))
            (unless (and (integerp (car bounds)) (integerp (cdr bounds)))
              (format "the bounds are %S" bounds))
            (unless (<= (point-min) (car bounds) (point))
              (format "the bounds start at %s, point being %s" (car bounds) (point)))
            (unless (= (cdr bounds) (point))
              (format "the bounds end at %s, point being %s" (cdr bounds) (point)))
            (when context
              (or (replique-completion-fuzz--ill-formed context)
                  (condition-case printing
                      (progn (replique-edn-print
                              (replique-completion--message
                               context
                               (buffer-substring-no-properties (car bounds) (cdr bounds))))
                             nil)
                    (error (format "the request does not print: %S" printing)))))))
    (error (format "signalled %S" error))))

(defun replique-completion-fuzz--failing (seed count)
  "Return the first of COUNT fuzzed buffers of SEED read wrongly, or nil.

Read at eight positions each, one of them the end - which is where point
is while somebody types, and the position every other test asks at."
  (replique-completion-fuzz--seed seed)
  (catch 'found
    (dotimes (_ count)
      (let ((text (replique-completion-fuzz--text)))
        (with-temp-buffer
          (replique-clojure-mode)
          (insert text)
          (dotimes (which 8)
            (goto-char (if (= which 0)
                           (point-max)
                         (+ (point-min)
                            (replique-completion-fuzz--next
                             (max 1 (- (point-max) (point-min) -1))))))
            (let ((problem (replique-completion-fuzz--problem)))
              (when problem
                (throw 'found (list :seed seed :text text :point (point)
                                    :problem problem))))))))
    nil))

;;; The runs

(ert-deftest replique-completion-fuzz-nothing-in-a-buffer-breaks-the-reading ()
  "Read a few thousand positions of a few hundred buffers."
  (replique-test-grammar)
  (dolist (seed '(1 2 3 4 5 6 7 8))
    (should (null (replique-completion-fuzz--failing seed 100)))))

(provide 'replique-completion-fuzz-test)

;;; replique-completion-fuzz-test.el ends here
