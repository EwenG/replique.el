;;; replique-completion-test.el --- Tests for completion  -*- lexical-binding: t; -*-

;;; Commentary:

;; What is offered where point is.  The names come from a process, so the
;; tests that check which names those are need one.
;;
;; What does not need one is the rest of it: which region a candidate
;; replaces, and the style.  The style is the piece worth testing without a
;; process anyway, since what it has to do is keep candidates that no other
;; style would - a table of its own is how that is asked plainly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-completion)

(defmacro replique-completion-test--at (text &rest body)
  "Run BODY in a Clojure buffer holding TEXT, with point where its | was."
  (declare (indent 1))
  `(progn
     (replique-test-grammar)
     (with-temp-buffer
       (replique-clojure-mode)
       (insert ,text)
       (goto-char (point-min))
       (unless (search-forward "|" nil t)
         (error "The text says nowhere to look: %s" ,text))
       (let ((pos (match-beginning 0)))
         (delete-region pos (1+ pos))
         (goto-char pos))
       ,@body)))

(defun replique-completion-test--text ()
  "Return what the completion at point would replace."
  (let ((bounds (replique-completion--bounds)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun replique-completion-test--all ()
  "Return the candidates offered at point, as the front end receives them.

Through `completion-all-completions', which is what every front end asks:
what it answers is the style's doing, and the style is half of what is
being tested."
  (let ((capf (replique-completion-at-point)))
    (when capf
      (let ((string (buffer-substring-no-properties (nth 0 capf) (nth 1 capf))))
        (completion-all-completions string (nth 2 capf) nil (length string))))))

(defun replique-completion-test--list (all)
  "Return the candidates in ALL, which is a list the base size is the tail of."
  (let ((found nil))
    (while (consp all)
      (push (car all) found)
      (setq all (cdr all)))
    (nreverse found)))

(defun replique-completion-test--names (all)
  "Return the names in ALL, without what rides on them."
  (mapcar #'substring-no-properties (replique-completion-test--list all)))

(defun replique-completion-test--offered ()
  "Return the names offered at point."
  (replique-completion-test--names (replique-completion-test--all)))

(defun replique-completion-test--table (candidates)
  "Return a table answering CANDIDATES, the way the one for a process does."
  (lambda (string pred action)
    (cond
     ((eq action 'metadata)
      '(metadata (category . replique-completion)
                 (display-sort-function . identity)
                 (cycle-sort-function . identity)))
     ((eq (car-safe action) 'boundaries) nil)
     ((eq action t) (if pred (seq-filter pred candidates) candidates))
     ((eq action 'lambda) (and (member string candidates) t))
     ((null action) (if (member string candidates) t string)))))

;;; What a candidate replaces

(ert-deftest replique-completion-test-what-is-replaced-is-what-was-typed ()
  (should (equal "clojure.st"
                 (replique-completion-test--at "(ns a (:require [clojure.st|]))"
                   (replique-completion-test--text))))
  (should (equal "jo"
                 (replique-completion-test--at "(ns a (:require [b :refer [jo|]]))"
                   (replique-completion-test--text)))))

(ert-deftest replique-completion-test-a-keyword-is-replaced-with-its-colon ()
  "A candidate for one carries its colon, because a keyword without one is
not something that can be written where that keyword is."
  (should (equal ":r"
                 (replique-completion-test--at "(ns a (:require [b :r|]))"
                   (replique-completion-test--text)))))

(ert-deftest replique-completion-test-a-path-is-replaced-from-the-quote ()
  "What is written in a load is a path and not a symbol, and a path can hold
what ends one: the whole of what was typed is what a candidate replaces,
so where it starts is the quote and not wherever a symbol would begin."
  (should (equal "clojure/co"
                 (replique-completion-test--at "(load \"clojure/co|\")"
                   (replique-completion-test--text))))
  (should (equal "my dir/co"
                 (replique-completion-test--at "(load \"my dir/co|\")"
                   (replique-completion-test--text)))))

(ert-deftest replique-completion-test-nothing-typed-is-a-region-of-no-width ()
  "Point after a bracket is nothing typed rather than nothing to offer, and
nothing typed is every name."
  (should (equal ""
                 (replique-completion-test--at "(ns a (:require [|]))"
                   (replique-completion-test--text)))))

(ert-deftest replique-completion-test-what-follows-point-is-left-alone ()
  "It is what somebody has already written and did not ask about."
  (should (equal "clojure.st"
                 (replique-completion-test--at "(ns a (:require [clojure.st|ring]))"
                   (replique-completion-test--text)))))

;;; What is asked

(ert-deftest replique-completion-test-a-load-says-which-namespace-it-is-in ()
  "The one thing a load needs that is not written in the load: what
`clojure.core/load' resolves a relative path against."
  (should (equal '(:op :completions :text "co" :position :load-path :ns "a.b")
                 (replique-completion-test--at "(ns a.b)\n(load \"co|\")"
                   (replique-completion--message
                    (replique-deps-context-at (point)) "co")))))

(ert-deftest replique-completion-test-what-the-slot-needs-rides-along ()
  "A namespace is written under a prefix and a var is referred from a
namespace, and the slot is what says which."
  (should (equal '(:op :completions :text "st" :position :namespace :prefix "clojure")
                 (replique-completion-test--at "(ns a (:require [clojure [st|]]))"
                   (replique-completion--message
                    (replique-deps-context-at (point)) "st"))))
  (should (equal '(:op :completions :text "jo"
                       :position :var :namespace "clojure.string")
                 (replique-completion-test--at "(ns a (:require [clojure.string :refer [jo|]]))"
                   (replique-completion--message
                    (replique-deps-context-at (point)) "jo")))))

;;; The style

(ert-deftest replique-completion-test-a-candidate-is-kept-though-it-is-no-prefix ()
  "The process matched a piece at a time, and every style emacs comes with
would filter what it found out again."
  (let* ((table (replique-completion-test--table
                 (list (propertize "java.util.Date" 'replique-match-index 14))))
         (completion-styles '(basic))
         (all (completion-all-completions "j.u.Da" table nil 6)))
    (should (equal '("java.util.Date") (replique-completion-test--names all)))
    (should (equal 0 (cdr (last all))))))

(ert-deftest replique-completion-test-the-style-is-put-on-this-completion-alone ()
  "`completion-styles' is somebody's setting.  What says which style reads
these candidates is the category the table names."
  (should (equal '((styles replique))
                 (cdr (assq 'replique-completion completion-category-defaults))))
  (let ((completion-styles '(basic)))
    (should-not (completion-all-completions
                 "j.u.Da" (list "java.util.Date") nil 6))))

(ert-deftest replique-completion-test-how-far-the-match-reached-is-shown ()
  "Which is the only thing that can say it: the tokens were matched at the
process, and the text and the candidate together do not say which pieces
of one found the other."
  (let* ((table (replique-completion-test--table
                 (list (propertize "java.util.Date" 'replique-match-index 11))))
         (candidate (car (replique-completion-test--list
                          (completion-all-completions "j.u.D" table nil 5)))))
    (should (eq 'completions-common-part (get-text-property 0 'face candidate)))
    (should (eq 'completions-common-part (get-text-property 10 'face candidate)))
    (should-not (get-text-property 11 'face candidate))))

(ert-deftest replique-completion-test-nothing-here-reorders-them ()
  "They arrive shortest first, which is the process's doing."
  (let* ((names '("clojure.set" "clojure.string" "clojure.stacktrace"))
         (table (replique-completion-test--table names)))
    (should (equal names (replique-completion-test--names
                          (completion-all-completions "cl" table nil 2)))))
  ;; and the table the process is asked through says so, which is what a
  ;; front end reads before it sorts them itself
  (let ((table (replique-completion--table '(:position :namespace :prefix ""))))
    (should (eq 'identity
                (cdr (assq 'display-sort-function
                           (completion-metadata "" table nil)))))))

(ert-deftest replique-completion-test-one-candidate-is-what-is-written ()
  (let ((table (replique-completion-test--table '("clojure.string"))))
    (should (equal '("clojure.string" . 14) (completion-try-completion "cl.st" table nil 5)))
    (should (eq t (completion-try-completion "clojure.string" table nil 14))))
  (let ((table (replique-completion-test--table '("clojure.set" "clojure.string"))))
    ;; nothing to grow it by: candidates matched a piece at a time have no
    ;; common beginning, and two that do have one have it by accident
    (should (equal '("cl.st" . 5) (completion-try-completion "cl.st" table nil 5)))))

;;; What a candidate is shown as

(ert-deftest replique-completion-test-a-candidate-says-what-it-is ()
  (should (equal " <n>" (replique-completion-annotation
                         (propertize "clojure.string" 'replique-type "namespace"))))
  (should (equal " clojure.string <f>"
                 (replique-completion-annotation
                  (propertize "join" 'replique-type "function" 'replique-ns "clojure.string"))))
  (should-not (replique-completion-annotation "unsaid")))

;;; Waiting for an answer

(ert-deftest replique-completion-test-a-wait-is-interrupted-by-a-quit ()
  "Emacs is held inside `accept-process-output' for as long as the process
takes, so C-g has to be heard there - and the connection has to be whole
afterwards, which is why the request is left pending rather than dropped."
  (let* ((process (replique-test-process))
         (conn (replique-process--control process))
         (started (float-time)))
    (should-not (cl-letf (((symbol-function 'accept-process-output)
                           (lambda (&rest _) (setq quit-flag t) nil)))
                  (replique-conn-request-sync conn (list :op :echo :value 1))))
    ;; heard where it happened rather than at the end of the wait, which
    ;; would leave the editor held for the rest of the timeout by a
    ;; keystroke that said to stop holding it
    (should (< (- (float-time) started) 1.0))
    (should-not quit-flag)
    (should (equal 2 (plist-get (replique-conn-request-sync
                                 conn (list :op :echo :value 2))
                                :value)))))

(ert-deftest replique-completion-test-a-wait-that-runs-out-is-answered-for ()
  "A control connection answers in order, so a request behind a slow one
waits for that one too.  What comes back says the process is busy rather
than that it refused."
  (let* ((process (replique-test-process))
         (conn (replique-process--control process))
         ;; asked for and never sent, which is a process that will not answer
         (frame (cl-letf (((symbol-function 'replique-conn-request)
                           (lambda (&rest _) 0)))
                  (replique-conn-request-sync conn (list :op :echo :value 1) 0.2))))
    (should (equal "error" (plist-get frame :tag)))
    (should (equal replique-conn-timeout-error (plist-get frame :error)))))

(ert-deftest replique-completion-test-a-connection-that-is-gone-is-answered-for ()
  (let* ((process (replique-test-process))
         (conn (replique-conn-open (replique-process--host process)
                                   (replique-process--port process)
                                   'control)))
    (replique-test-wait-for (lambda () (replique-conn--id conn)))
    (replique-conn-close conn)
    (should (equal replique-conn-closed-error
                   (plist-get (replique-conn-request-sync conn (list :op :echo :value 1))
                              :error)))))

;;; What the process answers

(ert-deftest replique-completion-test-a-namespace-of-the-classpath-is-offered ()
  (replique-test-process)
  (should (member "clojure.string"
                  (replique-completion-test--at "(ns a (:require [clojure.st|]))"
                    (replique-completion-test--offered)))))

(ert-deftest replique-completion-test-a-namespace-under-a-prefix-is-what-goes-there ()
  "A prefix list gives the start of a name once, so what is offered under
one is the rest of it and not the whole."
  (replique-test-process)
  (let ((names (replique-completion-test--at "(ns a (:require [clojure [st|]]))"
                 (replique-completion-test--offered))))
    (should (member "string" names))
    (should-not (member "clojure.string" names))))

(ert-deftest replique-completion-test-a-var-says-what-it-is-and-where-it-is-from ()
  "Where it is from is the one thing about a var that its own name does not
say: what is written after a :refer is written without its namespace."
  (replique-test-process)
  (let* ((candidates (replique-completion-test--at
                         "(ns a (:require [clojure.string :refer [jo|]]))"
                       (replique-completion-test--list (replique-completion-test--all))))
         (join (car (seq-filter (lambda (c) (equal "join" (substring-no-properties c)))
                                candidates))))
    (should join)
    (should (equal " clojure.string <f>" (replique-completion-annotation join)))))

(ert-deftest replique-completion-test-a-class-is-found-a-piece-at-a-time ()
  "Which is the thing the style is there for: java.util.Date is no
completion of j.u.Da in the sense any other style means."
  (replique-test-process)
  (should (member "java.util.Date"
                  ;; set to a style that would throw it away, so that what
                  ;; keeps it is the category and not the settings of
                  ;; whoever is running this
                  (let ((completion-styles '(substring)))
                    (replique-completion-test--at "(ns a (:import j.u.Da|))"
                      (replique-completion-test--offered))))))

(ert-deftest replique-completion-test-an-option-is-offered-with-its-colon ()
  (replique-test-process)
  (let ((names (replique-completion-test--at "(ns a (:require [clojure.string :r|]))"
                 (replique-completion-test--offered))))
    (should (member ":refer" names))
    (should (member ":rename" names))))

(ert-deftest replique-completion-test-a-path-of-the-classpath-is-offered ()
  (replique-test-process)
  (should (member "/replique/core"
                  (replique-completion-test--at "(load \"/replique/co|\")"
                    (replique-completion-test--offered)))))

(ert-deftest replique-completion-test-they-arrive-shortest-first ()
  (replique-test-process)
  (let ((names (replique-completion-test--at "(ns a (:require [clojure.st|]))"
                 (replique-completion-test--offered))))
    (should names)
    (should (equal (length (car names)) (apply #'min (mapcar #'length names))))))

(ert-deftest replique-completion-test-one-keystroke-is-one-question ()
  "A completion is looked at more than once for the same text - what could
be written there, whether there is only one of them - and each of those
would otherwise be a request of its own."
  (replique-test-process)
  (replique-completion-test--at "(ns a (:require [clojure.st|]))"
    (setq replique-completion--last nil)
    (let ((asked 0)
          (original (symbol-function 'replique-process-request-sync)))
      (cl-letf (((symbol-function 'replique-process-request-sync)
                 (lambda (&rest args) (setq asked (1+ asked)) (apply original args))))
        (should (replique-completion-test--all))
        (should (replique-completion-test--all))
        (should (equal 1 asked))))))

(ert-deftest replique-completion-test-a-longer-text-is-asked-again ()
  "An answer is the answer to the text it was asked about and to no other.
What was cut off a short text is what a longer one is after, so the
answer to the short one is not something to filter."
  (replique-test-process)
  (setq replique-completion--last nil)
  (let ((broad (replique-completion-test--at "(ns a (:require [clojure.s|]))"
                 (replique-completion-test--offered)))
        (narrow (replique-completion-test--at "(ns a (:require [clojure.str|]))"
                  (replique-completion-test--offered))))
    (should (member "clojure.set" broad))
    (should (member "clojure.string" narrow))
    (should-not (member "clojure.set" narrow))))

(ert-deftest replique-completion-test-only-where-clojure-is-read ()
  "Nothing is offered in a buffer that is not read as Clojure.

Which is what makes this safe to put wherever somebody wants it, the
default value of the hook included: a buffer with no Clojure in it is a
buffer in no dependency form, and that is answered as nothing without
anything here having to ask which buffer it is."
  (replique-test-process)
  (with-temp-buffer
    (insert "(ns a (:require [clojure.st]))")
    (goto-char (- (point-max) 3))
    (should-not (replique-completion-at-point))))

(ert-deftest replique-completion-test-an-abandoned-request-is-still-read ()
  "Its reply is read and handed to a callback nobody is listening to.
Dropping it instead would leave a reply with nothing to match, and it
would be handled as something the process said unprompted - an error
nobody asked for, put in the echo area of somebody who had moved on."
  (let* ((process (replique-test-process))
         (conn (replique-process--control process)))
    (should-not
     (replique-test-message
       (cl-letf (((symbol-function 'accept-process-output)
                  (lambda (&rest _) (setq quit-flag t) nil)))
         (replique-conn-request-sync conn (list :op :no-such-op)))
       (replique-test-settle)))))

(ert-deftest replique-completion-test-nothing-is-offered-where-nothing-is-asked ()
  "The usual case, which is a point in no dependency form at all: every
other completion in the buffer is left to answer for itself."
  (replique-test-process)
  (should-not (replique-completion-test--at "(defn f [x] (inc x|))"
                (replique-completion-at-point)))
  (should-not (replique-completion-test--at "(ns a (:require [b :as c|]))"
                (replique-completion-at-point))))

(ert-deftest replique-completion-test-the-name-is-written-in-place-of-what-was-typed ()
  "All of it, which is the whole point of a name that was matched a piece at
a time: what was typed is not the beginning of what is written."
  (replique-test-process)
  (replique-completion-test--at "(ns a (:import j.u.Date|))"
    (completion-at-point)
    (should (equal "(ns a (:import java.util.Date))"
                   (buffer-substring-no-properties (point-min) (point-max))))
    (should (equal (point) (- (point-max) 2)))))

(ert-deftest replique-completion-test-it-is-asked-where-clojure-is-written ()
  "A Clojure buffer, which `replique-mode' is on in, and a prompt, which
reads Clojure without being one.  Turning the mode off takes it back out,
which is how somebody says they would rather complete another way."
  (replique-test-grammar)
  (with-temp-buffer
    (replique-clojure-mode)
    (should (memq #'replique-completion-at-point completion-at-point-functions))
    (replique-mode -1)
    (should-not (memq #'replique-completion-at-point completion-at-point-functions)))
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (replique-test-hide (current-buffer))
      (should (memq #'replique-completion-at-point completion-at-point-functions)))))

(ert-deftest replique-completion-test-a-repl-completes-at-its-prompt ()
  "A repl reads Clojure at its prompt, requires included - which is how a
namespace is loaded from one."
  (replique-test-grammar)
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (replique-test-hide (current-buffer))
      (goto-char (point-max))
      (insert "(require '[clojure.st])")
      (goto-char (- (point-max) 2))
      (should (member "clojure.string" (replique-completion-test--offered))))))

(provide 'replique-completion-test)

;;; replique-completion-test.el ends here
