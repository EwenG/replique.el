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
;; Both readings, since both of them read the same buffer.  A completion
;; stops at point and a name being asked about does not, and the call point
;; is inside is read out of the form around point rather than out of the text
;; at it - which is a third thing that a buffer nobody could parse has to be
;; held up against.
;;
;; And what is made of the answer, which is the other half of eldoc and of
;; going to a definition.  An arglist arrives as a string and the argument
;; point is at has to be found inside it, which is indexing into text the
;; process wrote; a definition arrives as a file, an entry, a line and a
;; column, and what is done with those four is open something and go to a
;; place in it.  Neither is reached by reading a buffer alone, so the answers
;; are asked for as well - of a real process, since the answers worth reading
;; are the ones a process really gives - and the string surgery is fuzzed on
;; its own besides, where a bracket nobody closed can be handed to it
;; directly.
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
(require 'replique-name)
(require 'replique-deps)
(require 'replique-edn)
(require 'replique-symbol)

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
    ;; a name of the process itself, whose source is a file of a directory
    ;; on the classpath rather than an entry of a jar: the two are opened
    ;; differently, and a corpus naming only clojure names reaches one
    "replique.completion" "replique.names/scope-of"
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

(defun replique-completion-fuzz--printable (op context text)
  "Return what is wrong with the request OP, CONTEXT and TEXT make, or nil."
  (or (replique-completion-fuzz--ill-formed context)
      (condition-case printing
          (progn (replique-edn-print (replique-name-message op context text)) nil)
        (error (format "the request does not print: %S" printing)))))

(defun replique-completion-fuzz--asked ()
  "Return what is wrong with what a completion reads at point, or nil.

The region a candidate replaces, which ends where point is, and the slot
it would be written in."
  (let ((bounds (replique-name-bounds))
        (context (replique-name-context)))
    (or (unless (consp bounds) (format "the bounds are %S" bounds))
        (unless (and (integerp (car bounds)) (integerp (cdr bounds)))
          (format "the bounds are %S" bounds))
        (unless (<= (point-min) (car bounds) (point))
          (format "the bounds start at %s, point being %s" (car bounds) (point)))
        (unless (= (cdr bounds) (point))
          (format "the bounds end at %s, point being %s" (cdr bounds) (point)))
        (when context
          (replique-completion-fuzz--printable
           :completions context
           (buffer-substring-no-properties (car bounds) (cdr bounds)))))))

(defun replique-completion-fuzz--named ()
  "Return what is wrong with what a symbol op reads at point, or nil.

The whole of the name point is in, which a completion stops at point
instead, and the call point is inside, which is neither of those: it is
read out of the form around point rather than out of the text at it, and
a buffer nobody could parse is what it has to hold up against."
  (let ((name (replique-name-at-point))
        (call (replique-name-call-at-point)))
    (or (when name
          (or (unless (and (integerp (car name)) (integerp (cdr name)))
                (format "the name is %S" name))
              (unless (<= (point-min) (car name) (cdr name) (point-max))
                (format "the name is at %S" name))
              (unless (<= (car name) (point) (cdr name))
                (format "the name is at %S, point being %s" name (point)))
              (when-let* ((context (replique-name-context)))
                (replique-completion-fuzz--printable
                 :symbol context
                 (buffer-substring-no-properties (car name) (cdr name))))))
        (when call
          (or (unless (stringp (plist-get call :text))
                (format "the call is %S" (plist-get call :text)))
              (unless (and (integerp (plist-get call :argument))
                           (> (plist-get call :argument) 0))
                (format "the argument is %S" (plist-get call :argument)))
              (replique-completion-fuzz--printable
               :symbol (plist-get call :context) (plist-get call :text)))))))

(defun replique-completion-fuzz--problem ()
  "Return what is wrong with what is read at point, or nil when nothing is."
  (condition-case error
      (or (replique-completion-fuzz--asked) (replique-completion-fuzz--named))
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

;;; What is made of the answer

(defconst replique-completion-fuzz--kinds
  '("namespace" "namespace-prefix" "macro" "function" "var" "class" "package"
    "path" "keyword" "local" "special-form" "method" "field" "constructor")
  "Every kind of name the process says it found.

What holds a name is different for each of them - a var carries the
namespace it is public in, a class its package, a member its class - so
what is written where a client shows one is a different join for each.")

(defconst replique-completion-fuzz--arglists
  '("[]" "[f]" "[f coll]" "[x & more]" "[& more]" "[&]"
    "[{:keys [a b]} c]" "[[a b] & rest]" "[a {:as m} [b [c]]]"
    "^int []" "^String [int int]" "^Map$Entry [Object]" "^void [String[]]"
    "[test then else?]" "[classname name expr*]" "[x y & {:keys [k]}]")
  "Parameter vectors the way the process writes them.

A method arrives with the return tagged in front of the vector, a rest
argument with an ampersand in front of it, and a destructured name as a
map or a vector of its own - which is what makes finding the argument
point is at more than splitting on a space.")

(defun replique-completion-fuzz--maybe (key value)
  "Return the plist holding KEY and VALUE, or nothing, at random.

Which is what an answer is: every key but the kind and the name is
absent as often as it is there, because an absent value is an absent
key and half of what the process finds carries none of them."
  (when (zerop (replique-completion-fuzz--next 2)) (list key value)))

(defun replique-completion-fuzz--answer ()
  "Return an answer the way the process gives one.

Every kind, with the keys that kind carries there as often as not.  Not
an answer to anything in particular - what is being asked is what is
made of one, and a run that only ever saw the answers a buffer led to
would be a run that never saw a field, a rest argument or an arity
nobody called."
  (let ((kind (replique-completion-fuzz--pick replique-completion-fuzz--kinds)))
    (append
     (list :type kind :name (replique-completion-fuzz--pick
                             '("map" "join" "x" "/" "Date" "SIZE" "new" "if"
                               "substring" "Map$Entry" "a-name")))
     (replique-completion-fuzz--maybe :ns "clojure.core")
     (replique-completion-fuzz--maybe :class "java.lang.String")
     (replique-completion-fuzz--maybe :package "java.util")
     (replique-completion-fuzz--maybe :tag "int")
     (replique-completion-fuzz--maybe :doc "What it is for.\nOn two lines.")
     (replique-completion-fuzz--maybe
      :arglists (let ((count (replique-completion-fuzz--next 4)))
                  (mapcar (lambda (_)
                            (replique-completion-fuzz--pick
                             replique-completion-fuzz--arglists))
                          (number-sequence 1 count)))))))

(defun replique-completion-fuzz--faced (text)
  "Return where TEXT is faced as the argument point is at, or nil."
  (when (stringp text)
    (text-property-any 0 (length text) 'face
                       'eldoc-highlight-function-argument text)))

(defun replique-completion-fuzz--miscounted (arglist)
  "Return what is wrong with the arguments read out of ARGLIST, or nil.

Where each of them is written, which is what facing one means: a region
of the arglist, inside it, holding something, after the one before it
and not overlapping it.  A string that is no arglist at all is held to
the same rule - it is text the process wrote, and what is done with it
is indexing into it."
  (let ((found (replique-symbol--arguments arglist))
        (length (length arglist))
        (previous 0)
        (wrong nil))
    (dolist (where found)
      (cond
       ((not (consp where)) (setq wrong (format "an argument is %S" where)))
       ((not (and (integerp (car where)) (integerp (cdr where))))
        (setq wrong (format "an argument is %S" where)))
       ((not (<= 0 (car where) (cdr where) length))
        (setq wrong (format "an argument is at %S of %S" where arglist)))
       ((= (car where) (cdr where))
        (setq wrong (format "an argument is empty at %S of %S" where arglist)))
       ((< (car where) previous)
        (setq wrong (format "an argument is at %S, behind %s" where previous)))
       ((string-blank-p (substring arglist (car where) (cdr where)))
        (setq wrong (format "an argument is blank at %S of %S" where arglist)))
       (t (setq previous (cdr where)))))
    (or wrong
        ;; and the one it says an argument is at is one of them: facing a
        ;; region of an arglist that is no argument of it is facing
        ;; something nobody is writing
        (catch 'wrong
          (dolist (argument (number-sequence 1 8))
            (let ((where (replique-symbol--argument arglist argument)))
              (when (and where (not (member where found)))
                (throw 'wrong (format "argument %s is at %S, which is no argument of %S"
                                      argument where arglist)))))
          nil))))

(defun replique-completion-fuzz--arguments-of (arglists)
  "Return every argument written in ARGLISTS, as the text of each."
  (apply #'append
         (mapcar (lambda (arglist)
                   (mapcar (lambda (where)
                             (substring arglist (car where) (cdr where)))
                           (replique-symbol--arguments arglist)))
                 arglists)))

(defun replique-completion-fuzz--unsaid (found argument)
  "Return what is wrong with what is said about FOUND at ARGUMENT, or nil.

The name is what everything else hangs off, so it is what is said first;
the faced argument is one of the arguments of one of the arglists, which
is what facing one means; and nought is point at what a method is called
on, where nothing of the arglists is one of its arguments.

Which argument it is is not checked here - there is no right answer to an
argument nobody wrote - but that what was faced is an argument at all.
Where each of them is written is `replique-symbol--arguments\=' to say
and `replique-completion-fuzz--miscounted\=' to check; what is asked here
is of the two that read it."
  (let* ((name (replique-symbol-full-name found))
         (said (replique-symbol--said found argument))
         (faced (replique-completion-fuzz--faced said)))
    (or (unless (stringp name) (format "the name is %S" name))
        (unless (stringp said) (format "what is said is %S" said))
        (unless (string-prefix-p name said)
          (format "what is said does not start with the name: %S" said))
        (when (and faced (<= argument 0))
          (format "an argument is faced at %s: %S" argument said))
        (when faced
          (let* ((end (or (next-single-property-change faced 'face said)
                          (length said)))
                 (text (substring-no-properties said faced end)))
            (or (unless (< faced end) (format "the faced argument is empty in %S" said))
                (when (string-blank-p text)
                  (format "the faced argument is blank in %S" said))
                (when (string-search "\n" text)
                  (format "the faced argument runs over a line in %S" said))
                (unless (member text (replique-completion-fuzz--arguments-of
                                      (plist-get found :arglists)))
                  (format "%S is faced, which is no argument of %S"
                          text (plist-get found :arglists)))))))))

(defun replique-completion-fuzz--unmade (seed count)
  "Return the first of COUNT answers of SEED made something wrong of, or nil.

Each of them read at every argument a call could be written with, and at
nought, which is where point is while a method is being written on
something."
  (replique-completion-fuzz--seed seed)
  (catch 'found
    (dotimes (_ count)
      (let ((arglist (if (zerop (replique-completion-fuzz--next 2))
                         (replique-completion-fuzz--pick
                          replique-completion-fuzz--arglists)
                       (replique-completion-fuzz--junk))))
        (when-let* ((problem (condition-case error
                                 (replique-completion-fuzz--miscounted arglist)
                               (error (format "signalled %S" error)))))
          (throw 'found (list :seed seed :arglist arglist :problem problem))))
      (let ((answer (replique-completion-fuzz--answer)))
        (dolist (argument (number-sequence 0 6))
          (when-let* ((problem (condition-case error
                                   (replique-completion-fuzz--unsaid answer argument)
                                 (error (format "signalled %S" error)))))
            (throw 'found (list :seed seed :answer answer
                                :argument argument :problem problem))))))
    nil))

;;; What the process answers what was read

(defun replique-completion-fuzz--unanswered ()
  "Return what is wrong with what the process answers at point, or nil.

Asked for real, because the answers worth making something of are the
ones a process really gives: a name half written resolves to nothing far
more often than it resolves to anything, and the few that resolve are
the only ones that reach the rest of this.

Asked and waited for, rather than through eldoc, which hands its answer
to a callback: what is being fuzzed is what is made of the answer, and a
run that waited out every name that means nothing would spend itself
waiting."
  (or (when-let* ((call (replique-name-call-at-point))
                  (found (replique-symbol--ask (plist-get call :context)
                                               (plist-get call :text))))
        (replique-completion-fuzz--unsaid
         found (replique-symbol--argument-of (plist-get call :text)
                                             (plist-get call :argument))))
      (when-let* ((bounds (replique-name-at-point))
                  (context (replique-name-context))
                  (found (replique-symbol--ask
                          context
                          (buffer-substring-no-properties (car bounds) (cdr bounds))))
                  ((plist-get found :file)))
        (let* ((location (replique-symbol--make-location found))
               (group (xref-location-group location))
               (marker (xref-location-marker location)))
          (or (unless (stringp group) (format "the group is %S" group))
              (unless (markerp marker) (format "the marker is %S" marker))
              (unless (buffer-live-p (marker-buffer marker))
                (format "the marker is in no buffer: %S" found))
              (unless (with-current-buffer (marker-buffer marker)
                        (<= (point-min) (marker-position marker) (point-max)))
                (format "the marker is outside what it is in: %S" found)))))))

(defun replique-completion-fuzz--unanswerable (seed count)
  "Return the first of COUNT fuzzed buffers of SEED answered wrongly, or nil.

Read at three positions each rather than eight: every one of them is a
question put to a process, and what is being looked for here is what is
made of an answer rather than what is read out of a buffer.

One buffer, written again for each text, rather than one buffer each.
What is being fuzzed is the text and where point is in it, and neither of
those needs a buffer nobody has used yet - where turning the major mode
on in a new one is four fifths of what this run costs, and every question
it puts to the process together is a quarter of what is left."
  (replique-completion-fuzz--seed seed)
  (catch 'found
    (with-temp-buffer
      (replique-clojure-mode)
      (dotimes (_ count)
        (let ((text (replique-completion-fuzz--text)))
          (erase-buffer)
          (insert text)
          (dotimes (which 3)
            (goto-char (if (= which 0)
                           (point-max)
                         (+ (point-min)
                            (replique-completion-fuzz--next
                             (max 1 (- (point-max) (point-min) -1))))))
            (let ((problem (condition-case error
                               (replique-completion-fuzz--unanswered)
                             (error (format "signalled %S" error)))))
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

(ert-deftest replique-completion-fuzz-nothing-answered-breaks-what-is-said ()
  "Every kind of answer, at every argument a call could be written with.

No process: what is being asked is what is made of an answer, and an
answer made up here reaches the kinds a buffer hardly ever leads to."
  (dolist (seed '(1 2 3 4 5 6 7 8))
    (should (null (replique-completion-fuzz--unmade seed 500)))))

(ert-deftest replique-completion-fuzz-nothing-in-a-buffer-breaks-the-answer ()
  "A few hundred positions, each of them asked of a real process, and what
comes back said and opened."
  (replique-test-grammar)
  (replique-test-process)
  (let ((before (buffer-list)))
    (unwind-protect
        (dolist (seed '(1 2 3 4 5 6 7 8))
          (should (null (replique-completion-fuzz--unanswerable seed 40))))
      ;; the buffers a definition was opened in, which is a jar entry read
      ;; out into one of its own as often as a file
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer))))))

(provide 'replique-completion-fuzz-test)

;;; replique-completion-fuzz-test.el ends here
