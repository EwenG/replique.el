;;; replique-dialect-test.el --- Tests for which world a question is about  -*- lexical-binding: t; -*-

;;; Commentary:

;; A name is read against one of two worlds, and which one is the client's to
;; say.  What says it is the buffer: .clj is Clojure, .cljs is ClojureScript,
;; and .cljc is whichever repl the commands are pointed at, because a .cljc
;; namespace really is a namespace of both and nothing in the file chooses.
;;
;; Most of that is a decision about the current buffer and needs no process,
;; so most of these tests make a repl by hand.  What such a repl has to have
;; is a live connection - that is what `replique-repl-current' filters on -
;; and the handshake reply it carries, which is where the dialect and the
;; target are read from.  A `cat' standing in for the network process is
;; enough for both: nothing here writes to it.
;;
;; The two at the end use a process, because what is being checked there is
;; that the keys this builds are keys a process reads.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-name)
(require 'replique-repl)

(defvar replique-dialect-test--processes nil
  "The stand-in processes the fake repls hold, newest first.")

(defun replique-dialect-test--repl (&optional dialect target)
  "Return a live repl whose handshake reply said DIALECT and TARGET.

Both are strings, the way they arrive over the wire - a reply is JSON,
and the process writes the dialect and the target as the names of the
keywords it read.  Nil for either is a reply that did not carry it,
which is what a Clojure repl gets."
  (let* ((proc (start-process "replique-dialect-test" nil "cat"))
         (conn (replique-conn--make
                :proc proc :kind 'repl
                :info (append (when dialect (list :dialect dialect))
                              (when target (list :target target))))))
    (push proc replique-dialect-test--processes)
    (replique-repl--make :conn conn :to-echo 0)))

(defmacro replique-dialect-test--with-repl (repl &rest body)
  "Run BODY with REPL as the repl the commands act on.

REPL is what `replique-dialect-test--repl' returns, or nil for a session
with no repl at all."
  (declare (indent 1))
  `(let ((replique-current-repl ,repl))
     (unwind-protect (progn ,@body)
       (dolist (proc replique-dialect-test--processes)
         (when (process-live-p proc) (delete-process proc)))
       (setq replique-dialect-test--processes nil))))

(defmacro replique-dialect-test--in-mode (mode &rest body)
  "Run BODY in a temporary buffer in MODE."
  (declare (indent 1))
  `(with-temp-buffer
     (funcall ,mode)
     ,@body))

;;; What the buffer says

(ert-deftest replique-dialect-test-a-clojure-buffer-asks-about-nothing ()
  "Absent means Clojure, which is the process's rule.  A message about
Clojure carries no dialect at all rather than one that says the default -
so a Clojure session sends what it sent before there was a second world."
  (replique-dialect-test--with-repl nil
    (replique-dialect-test--in-mode #'replique-clojure-mode
      (should (eq :clj (replique-dialect)))
      (should (null (replique-dialect-keys))))))

(ert-deftest replique-dialect-test-a-clojurescript-buffer-asks-about-clojurescript ()
  (replique-dialect-test--with-repl nil
    (replique-dialect-test--in-mode #'replique-clojure-clojurescript-mode
      (should (eq :cljs (replique-dialect)))
      (should (equal '(:dialect :cljs) (replique-dialect-keys))))))

(ert-deftest replique-dialect-test-a-cljc-buffer-follows-the-repl ()
  "Nothing in a .cljc file chooses: it really is a namespace of both worlds.
So what it is asked about is the repl the commands are pointed at, which
is the rule replique 1 settled on."
  (replique-dialect-test--with-repl (replique-dialect-test--repl "cljs" "browser")
    (replique-dialect-test--in-mode #'replique-clojure-clojurec-mode
      (should (eq :cljs (replique-dialect)))))
  (replique-dialect-test--with-repl (replique-dialect-test--repl)
    (replique-dialect-test--in-mode #'replique-clojure-clojurec-mode
      (should (eq :clj (replique-dialect)))))
  (replique-dialect-test--with-repl nil
    (replique-dialect-test--in-mode #'replique-clojure-clojurec-mode
      (should (eq :clj (replique-dialect))))))

(ert-deftest replique-dialect-test-a-cljs-buffer-is-clojurescript-whatever-the-repl-is ()
  "The extension says it, and a repl does not unsay it: somebody reading a
.cljs file with a Clojure repl open is reading ClojureScript."
  (replique-dialect-test--with-repl (replique-dialect-test--repl)
    (replique-dialect-test--in-mode #'replique-clojure-clojurescript-mode
      (should (eq :cljs (replique-dialect))))))

(ert-deftest replique-dialect-test-a-repl-buffer-is-about-its-own-repl ()
  "A repl buffer is not a file and has no extension to read.  What it is
about is the repl it is the buffer of, which is what `replique-repl-current'
answers first."
  (let ((repl (replique-dialect-test--repl "cljs" "node")))
    (replique-dialect-test--with-repl nil
      (with-temp-buffer
        (setq-local replique--buffer-repl repl)
        (should (eq :cljs (replique-dialect)))
        (should (equal '(:dialect :cljs :target :node)
                       (replique-dialect-keys)))))))

;;; What the target is

(ert-deftest replique-dialect-test-the-target-comes-from-the-repl ()
  "A browser build and a node build are two compilations of the same
sources, holding different code - so a question about one is not an answer
about the other."
  (replique-dialect-test--with-repl (replique-dialect-test--repl "cljs" "node")
    (replique-dialect-test--in-mode #'replique-clojure-clojurescript-mode
      (should (equal '(:dialect :cljs :target :node)
                     (replique-dialect-keys))))))

(ert-deftest replique-dialect-test-no-clojurescript-repl-lends-no-target ()
  "Left to the process, which answers a reading op about its own default.
A Clojure repl has no target to lend, and neither has no repl at all."
  (replique-dialect-test--with-repl (replique-dialect-test--repl)
    (replique-dialect-test--in-mode #'replique-clojure-clojurescript-mode
      (should (equal '(:dialect :cljs) (replique-dialect-keys)))))
  (replique-dialect-test--with-repl nil
    (replique-dialect-test--in-mode #'replique-clojure-clojurescript-mode
      (should (equal '(:dialect :cljs) (replique-dialect-keys))))))

;;; What a repl itself is

(ert-deftest replique-dialect-test-a-repl-is-what-the-handshake-said ()
  "Read off the reply rather than remembered from what was asked for: a repl
that asked for nothing is a Clojure repl without having said so."
  (replique-dialect-test--with-repl nil
    (let ((cljs (replique-dialect-test--repl "cljs" "browser"))
          (clj (replique-dialect-test--repl)))
      (should (eq :cljs (replique-repl-dialect cljs)))
      (should (eq :browser (replique-repl-target cljs)))
      (should (equal '(:dialect :cljs :target :browser)
                     (replique-repl-dialect-keys cljs)))
      (should (eq :clj (replique-repl-dialect clj)))
      (should (null (replique-repl-target clj)))
      (should (null (replique-repl-dialect-keys clj)))
      (should (null (replique-repl-dialect-keys nil))))))

;;; What a request carries

(ert-deftest replique-dialect-test-a-request-carries-what-the-buffer-is ()
  "Every op that can be asked either way takes the keys the same way, so the
one place that builds a request about a name is the one place that says
which world it is about."
  (replique-dialect-test--with-repl (replique-dialect-test--repl "cljs" "node")
    (replique-dialect-test--in-mode #'replique-clojure-clojurescript-mode
      (let ((msg (replique-name-message :completions nil "ma")))
        (should (eq :cljs (plist-get msg :dialect)))
        (should (eq :node (plist-get msg :target)))))
    (replique-dialect-test--in-mode #'replique-clojure-mode
      (let ((msg (replique-name-message :symbol nil "map")))
        (should (null (plist-get msg :dialect)))
        (should (null (plist-get msg :target)))))))

;;; Against a process

(ert-deftest replique-dialect-test-a-clojurescript-question-is-one-a-process-reads ()
  "The keys this builds are keys the process acts on.

Either answer proves it read them: a process with the compiler answers
about the ClojureScript world, and one without refuses by name rather
than answering an empty list - which is the whole point of the refusal,
since a namespace that does not exist and a process that cannot say would
otherwise come back the same."
  (let ((process (replique-test-process))
        (answer nil))
    (replique-process-request
     process (list :op :namespaces :dialect :cljs)
     (lambda (frame) (setq answer frame)))
    (should (replique-test-wait-for (lambda () answer)))
    (if (equal "error" (plist-get answer :tag))
        (progn
          (should (equal "no-cljs" (plist-get answer :error)))
          (should (string-match-p "ClojureScript" (plist-get answer :message))))
      (should (plist-member answer :namespaces)))))

(ert-deftest replique-dialect-test-a-dialect-the-process-cannot-speak-is-refused ()
  "Not silently answered as Clojure.  A client that believes a third world
exists finds out here, and that is the only place it could."
  (let ((process (replique-test-process))
        (answer nil))
    (replique-process-request
     process (list :op :namespaces :dialect :elisp)
     (lambda (frame) (setq answer frame)))
    (should (replique-test-wait-for (lambda () answer)))
    (should (equal "error" (plist-get answer :tag)))
    (should (string-match-p "dialect" (plist-get answer :message)))))

(ert-deftest replique-dialect-test-a-handshake-carries-the-dialect ()
  "What a repl asks to be is what the process reads.

The shared process has no ClojureScript compiler, which is what makes
this say something: a handshake that carried the dialect is refused by
name, and one that lost it on the way would have been accepted as the
Clojure repl it then was."
  (let* ((process (replique-test-process))
         (frame nil)
         (conn (replique-conn-open
                (replique-process--host process)
                (replique-process--port process)
                'repl
                :process-id (replique-process--id process)
                :hello (list :dialect :cljs :target :node)
                :on-error (lambda (f) (setq frame f)))))
    (unwind-protect
        (progn
          (should (replique-test-wait-for (lambda () frame)))
          (should (equal "error" (plist-get frame :tag)))
          (should (equal "no-cljs" (plist-get frame :error))))
      (replique-conn-close conn))))

(ert-deftest replique-dialect-test-a-clojurescript-repl-is-not-quietly-a-clojure-one ()
  "Through the command, and for the same reason.

A repl asked for in a dialect this process cannot run never reaches a
prompt.  What would be worse than the refusal is the other thing: a repl
that was asked for in ClojureScript, came up in Clojure, and said so
nowhere."
  (let* ((process (replique-test-process))
         (repl (replique-repl process :cljs :node)))
    (unwind-protect
        (progn
          (replique-test-settle)
          (should (null (replique-repl--ns repl)))
          (should-not (replique-repl-live-p repl)))
      (when (replique-repl--conn repl)
        (replique-conn-close (replique-repl--conn repl)))
      (when (buffer-live-p (replique-repl--buffer repl))
        (kill-buffer (replique-repl--buffer repl))))))

(provide 'replique-dialect-test)

;;; replique-dialect-test.el ends here
