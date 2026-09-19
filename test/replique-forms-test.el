;;; replique-forms-test.el --- Tests for the table of binding forms  -*- lexical-binding: t; -*-

;;; Commentary:

;; What a namespace writes the forms that bind as, asked of a process.
;;
;; The tests that build a table need no process; the ones that ask a
;; namespace do, and they make the namespaces they ask about - a namespace
;; that aliases clojure.core is not something a project has lying around.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-forms)

(defun replique-forms-test--kind (forms written)
  "Return what FORMS says WRITTEN binds."
  (cdr (assoc written forms)))

(defun replique-forms-test--make-ns (repl &rest forms)
  "Evaluate FORMS in REPL, which are expected to make a namespace.

One at a time, and back to user afterwards: what waits for a form to be
answered waits for one prompt, and a repl left in the namespace a test
made is a repl the next test starts in."
  (dolist (form forms) (replique-test-eval repl form))
  (replique-test-eval repl "(in-ns 'user)"))

(defun replique-forms-test--locals (text forms)
  "Return the names of the locals where | is in TEXT, reading it with FORMS."
  (with-temp-buffer
    (replique-clojure-mode)
    (insert text)
    (goto-char (point-min))
    (unless (search-forward "|" nil t)
      (error "The text says nowhere to look: %s" text))
    (let ((pos (match-beginning 0)))
      (delete-region (match-beginning 0) (match-end 0))
      (mapcar #'car (replique-locals-at pos forms)))))

;;; The table itself

(ert-deftest replique-forms-test-the-default-is-what-core-calls-them ()
  "Which is what they are called nearly everywhere, and what has to be
assumed of a namespace nothing has been asked about."
  (should (eq 'let-like (replique-forms-test--kind
                         replique-locals-default-forms "let")))
  (should (eq 'let-like (replique-forms-test--kind
                         replique-locals-default-forms "clojure.core/let")))
  (should (eq 'fn-like (replique-forms-test--kind
                                    replique-locals-default-forms "fn")))
  (should (eq 'defn-like (replique-forms-test--kind
                        replique-locals-default-forms "defn")))
  (should-not (replique-forms-test--kind replique-locals-default-forms "c/let")))

(ert-deftest replique-forms-test-a-special-form-is-in-it-without-being-asked ()
  "It is read by the compiler rather than resolved, so there is no
namespace that writes it differently."
  (should (eq 'named-third (replique-forms-test--kind
                            replique-locals-default-forms "catch")))
  (should (eq 'named-third (replique-forms-test--kind
                            (replique-locals-forms '(("clojure.core/let" "lettuce")))
                            "catch"))))

(ert-deftest replique-forms-test-what-a-namespace-writes-them-as-replaces-it ()
  "All of it: a namespace that writes let as lettuce writes let for
something else, or for nothing, and offering both would be offering a
form that namespace does not have."
  (let ((forms (replique-locals-forms '(("clojure.core/let" "lettuce" "clojure.core/let")))))
    (should (eq 'let-like (replique-forms-test--kind forms "lettuce")))
    (should (eq 'let-like (replique-forms-test--kind forms "clojure.core/let")))
    (should-not (replique-forms-test--kind forms "let"))
    ;; and what was not asked about keeps the name it usually has
    (should (eq 'for-like (replique-forms-test--kind forms "for")))))

;;; Asked of a process

(ert-deftest replique-forms-test-a-namespace-that-aliases-core ()
  "The one master answers and this could not: c/let is a let, and no
reading of the text alone says so."
  (let ((process (replique-test-process)))
    (replique-test-with-repl repl
      (replique-forms-test--make-ns
       repl "(ns probe.aliased (:require [clojure.core :as c]))")
      (replique-forms-forget)
      (let ((forms (replique-forms-for process "probe.aliased")))
        (should (eq 'let-like (replique-forms-test--kind forms "c/let")))
        (should (eq 'let-like (replique-forms-test--kind forms "let")))
        (should (equal '("x") (replique-forms-test--locals "(c/let [x 1] |)" forms)))))))

(ert-deftest replique-forms-test-a-namespace-that-shadows-one ()
  "A namespace that excluded let and defined its own writes let for a var
that binds nothing, so what is written there binds nothing."
  (let ((process (replique-test-process)))
    (replique-test-with-repl repl
      (replique-forms-test--make-ns
       repl "(ns probe.shadowed (:refer-clojure :exclude [let]))"
       "(def let :not-a-binding-form)")
      (replique-forms-forget)
      (let ((forms (replique-forms-for process "probe.shadowed")))
        (should-not (replique-forms-test--kind forms "let"))
        (should (eq 'let-like (replique-forms-test--kind forms "clojure.core/let")))
        (should-not (replique-forms-test--locals "(let [x 1] |)" forms))
        (should (equal '("x")
                       (replique-forms-test--locals "(clojure.core/let [x 1] |)" forms)))))))

(ert-deftest replique-forms-test-a-namespace-that-renamed-one ()
  (let ((process (replique-test-process)))
    (replique-test-with-repl repl
      (replique-forms-test--make-ns
       repl "(ns probe.renamed (:refer-clojure :rename {let lettuce}))")
      (replique-forms-forget)
      (let ((forms (replique-forms-for process "probe.renamed")))
        (should (eq 'let-like (replique-forms-test--kind forms "lettuce")))
        (should-not (replique-forms-test--kind forms "let"))
        (should (equal '("x") (replique-forms-test--locals "(lettuce [x 1] |)" forms)))))))

(ert-deftest replique-forms-test-a-namespace-the-process-does-not-have ()
  "Which is every file until it is loaded.  What a namespace refers before
it refers anything is clojure.core, so the names of core mean there what
they mean anywhere."
  (let ((process (replique-test-process)))
    (replique-forms-forget)
    (let ((forms (replique-forms-for process "no.such.namespace")))
      (should (eq 'let-like (replique-forms-test--kind forms "let")))
      (should-not (replique-forms-test--kind forms "c/let")))))

;;; What is kept

(ert-deftest replique-forms-test-an-answer-stands-for-a-moment ()
  "A tool that reads locals reads them per keystroke, and asking per
keystroke would be a request for an answer that hardly ever changes."
  (let ((process (replique-test-process))
        (asked 0))
    (replique-forms-forget)
    (let ((original (symbol-function 'replique-process-request-sync)))
      (cl-letf (((symbol-function 'replique-process-request-sync)
                 (lambda (&rest args) (setq asked (1+ asked)) (apply original args))))
        (should (replique-forms-for process "user"))
        (should (replique-forms-for process "user"))
        (should (equal 1 asked))
        ;; a different namespace is a different question
        (should (replique-forms-for process "clojure.string"))
        (should (equal 2 asked))))))

(ert-deftest replique-forms-test-an-answer-does-not-stand-forever ()
  "What changes it is a namespace being evaluated again, which is a thing
somebody does and then goes on typing after - so what is kept is kept
briefly enough that the next thing typed sees the change."
  (let ((process (replique-test-process))
        (asked 0))
    (replique-forms-forget)
    (let ((original (symbol-function 'replique-process-request-sync))
          (replique-forms-kept 0))
      (cl-letf (((symbol-function 'replique-process-request-sync)
                 (lambda (&rest args) (setq asked (1+ asked)) (apply original args))))
        (should (replique-forms-for process "user"))
        (should (replique-forms-for process "user"))
        (should (equal 2 asked))))))

(ert-deftest replique-forms-test-without-an-answer-there-is-still-a-table ()
  "A table that is right nearly everywhere beats none at all."
  (should (eq replique-locals-default-forms (replique-forms-for nil "user")))
  (let ((process (replique-test-process)))
    (replique-forms-forget)
    (cl-letf (((symbol-function 'replique-process-request-sync)
               (lambda (&rest _) nil)))
      (should (eq replique-locals-default-forms
                  (replique-forms-for process "no.answer"))))))

(ert-deftest replique-forms-test-what-was-answered-once-outlasts-a-silence ()
  "It was the answer for this namespace, where the default is the answer
for a namespace nobody has asked about."
  (let ((process (replique-test-process)))
    (replique-forms-forget)
    (replique-test-with-repl repl
      (replique-forms-test--make-ns
       repl "(ns probe.outlasts (:require [clojure.core :as k]))")
      (let ((forms (replique-forms-for process "probe.outlasts")))
        (should (eq 'let-like (replique-forms-test--kind forms "k/let")))
        (let ((replique-forms-kept 0))
          (cl-letf (((symbol-function 'replique-process-request-sync)
                     (lambda (&rest _) nil)))
            (should (eq 'let-like (replique-forms-test--kind
                                   (replique-forms-for process "probe.outlasts")
                                   "k/let")))))))))

(ert-deftest replique-forms-test-what-the-client-got-wrong ()
  (let* ((process (replique-test-process))
         (kind (lambda (msg) (plist-get (replique-process-request-sync process msg) :error))))
    (should (equal "invalid-message" (funcall kind '(:op :spellings))))
    (should (equal "invalid-message" (funcall kind '(:op :spellings :vars []))))
    (should (equal "invalid-message" (funcall kind '(:op :spellings :vars "let"))))
    (should (equal "invalid-message" (funcall kind '(:op :spellings :vars ["let"]))))
    (should (equal "invalid-message" (funcall kind '(:op :spellings :vars [42]))))
    (should (equal "invalid-message"
                   (funcall kind '(:op :spellings :vars ["clojure.core/let"] :ns 42))))
    ;; a name that resolves to nothing is a fact about the process rather
    ;; than a message written wrongly
    (let ((frame (replique-process-request-sync
                  process '(:op :spellings :vars ["clojure.core/no-such-var"]))))
      (should (equal "reply" (plist-get frame :tag)))
      (should-not (plist-get frame :spellings)))))

(provide 'replique-forms-test)

;;; replique-forms-test.el ends here
