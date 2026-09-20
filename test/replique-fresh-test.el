;;; replique-fresh-test.el --- Tests for loading what changed first  -*- lexical-binding: t; -*-

;;; Commentary:

;; The offer, and the waiting that makes it worth making.
;;
;; What is decided here is decided out of what the process answered, so
;; most of it is tested on an answer written out by hand: whether the
;; question is asked at all, what it says, what declining leaves behind,
;; and what a load that stopped does to the command it was for.
;;
;; The waiting is not: what it is is a repl running something and this not
;; returning until it has, which is only true of a real one.  Those tests
;; need a process and are skipped without one.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-fresh)

(defvar replique-fresh-test--loaded nil
  "Whether the reload was asked for while a test ran.")

(defvar replique-fresh-test--asked nil
  "Whether the process was asked what has changed while a test ran.")

(defmacro replique-fresh-test--answering (found &rest body)
  "Run BODY with the process answering FOUND about what it has not read.

A repl there to load in, since a load happens in one, and a reload that
records that it was asked for rather than one that loads anything: what
is being decided is whether to load, and the loading is
`replique-reload-all\\=', which is tested where it is written."
  (declare (indent 1))
  `(let ((replique-fresh-test--loaded nil)
         (replique-fresh-test--asked nil))
     (cl-letf (((symbol-function 'replique-repl-current)
                (lambda () (replique-repl--make :process (replique-process--make :id "test"))))
               ((symbol-function 'replique-fresh--asked)
                (lambda (_process)
                  (setq replique-fresh-test--asked t)
                  ,found))
               ((symbol-function 'replique-reload-all)
                (lambda (&optional _waiting)
                  (setq replique-fresh-test--loaded t)
                  '(:tag "ret" :value "[]"))))
       ,@body)))

;;; Whether anything is asked at all

(ert-deftest replique-fresh-test-nothing-changed-is-nothing-said ()
  "The usual case, and the one that has to cost nothing: a question nobody
answers is a question that should not have been asked."
  (replique-fresh-test--answering '(:changed nil :stale nil)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) (error "Nothing to ask about"))))
      (should-not (replique-fresh-ensure "finding every use of a name"))
      (should-not replique-fresh-test--loaded))))

(ert-deftest replique-fresh-test-what-changed-is-offered-and-loaded ()
  "And the answer that follows is out of the files as they are now, which
is the whole point of the offer."
  (replique-fresh-test--answering '(:changed ((:file "/p/src/app/util.clj")) :stale nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (should-not (replique-fresh-ensure "finding every use of a name"))
      (should replique-fresh-test--loaded))))

(ert-deftest replique-fresh-test-declining-asks-anyway-and-says-what-it-leaves-out ()
  "Declining is an answer.  What was asked for is asked for, out of what
the process read, and what that leaves out is said once - a use written
since is not in the list and nothing in a list can point at what is not in
it."
  (replique-fresh-test--answering '(:changed ((:file "/p/src/app/util.clj"))
                                    :stale ((:file "/p/src/app/core.clj")))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (let ((said (replique-test-message
                    (should (equal 2 (length (replique-fresh-ensure "finding a use")))))))
        (should-not replique-fresh-test--loaded)
        (should (string-match-p "2 files have changed" said))))))

(ert-deftest replique-fresh-test-loading-without-asking-is-a-setting ()
  "For somebody who always wants it: the question has one answer, and being
asked it every time is being asked to confirm what they already decided."
  (let ((replique-reload-before-asking 'always))
    (replique-fresh-test--answering '(:changed ((:file "/p/a.clj")) :stale nil)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (error "Asked after being told not to"))))
        (should-not (replique-fresh-ensure "finding every use of a name"))
        (should replique-fresh-test--loaded)))))

(ert-deftest replique-fresh-test-never-asks-the-process-nothing ()
  "Not asked and not offered: the setting is there for somebody who does
not want the question, and asking the process in order to say nothing
about the answer is paying for it anyway."
  (let ((replique-reload-before-asking 'never))
    (replique-fresh-test--answering '(:changed ((:file "/p/a.clj")) :stale nil)
      (should-not (replique-fresh-ensure "finding every use of a name"))
      (should-not replique-fresh-test--asked)
      (should-not replique-fresh-test--loaded))))

(ert-deftest replique-fresh-test-a-process-that-records-nothing-is-left-alone ()
  "Stock clojure, whose compiler writes none of this down.  It cannot say
what changed and there is nothing to load it for - and what was about to
be asked of it refuses itself, with a message about the right thing."
  (replique-fresh-test--answering nil
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) (error "Offered a load to a process that records nothing"))))
      (should-not (replique-fresh-ensure "finding every use of a name"))
      (should-not replique-fresh-test--loaded))))

(ert-deftest replique-fresh-test-no-repl-is-nothing-to-load-in ()
  "A load compiles, prints, and is interrupted in a repl.  Without one there
is nowhere for it to happen, so the process is not even asked."
  (let ((replique-fresh-test--asked nil))
    (cl-letf (((symbol-function 'replique-repl-current) (lambda () nil))
              ((symbol-function 'replique-fresh--asked)
               (lambda (_process) (setq replique-fresh-test--asked t) nil)))
      (should-not (replique-fresh-ensure "finding every use of a name"))
      (should-not replique-fresh-test--asked))))

;;; What the question says

(ert-deftest replique-fresh-test-the-question-counts-both-halves ()
  "Two counts, because they are two different facts: these were edited, and
those were not and are out of date all the same.  The second is the one
nothing in a buffer says, which is the reason to show it in a question
somebody answers with one key."
  (should (equal "util.clj changed.  Load it before finding a use? "
                 (replique-fresh--question '((:file "/p/src/app/util.clj")) nil
                                           "finding a use")))
  (should (equal "2 files changed.  Load them before finding a use? "
                 (replique-fresh--question '((:file "/p/a.clj") (:file "/p/b.clj")) nil
                                           "finding a use")))
  (should (equal "1 files changed, 2 more need compiling.  Load them before finding a use? "
                 (replique-fresh--question '((:file "/p/a.clj"))
                                           '((:file "/p/b.clj") (:file "/p/c.clj"))
                                           "finding a use")))
  (should (equal "clojure/string.clj changed.  Load it before finding a use? "
                 (replique-fresh--question '((:file "/m2/clojure.jar"
                                              :entry "clojure/string.clj"))
                                           nil "finding a use"))))

;;; A load that stopped

(ert-deftest replique-fresh-test-a-load-that-stopped-stops-what-it-was-for ()
  "A file that will not compile leaves the process holding some of the new
files and some of the old.  Answering out of that is answering out of a
model half way through being brought up to date, which is worse than the
answer that was just refused - so the command it was for stops too, and
what threw is in the repl where it can be read."
  (replique-fresh-test--answering '(:changed ((:file "/p/a.clj")) :stale nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'replique-reload-all)
               (lambda (&optional _waiting)
                 '(:tag "exception" :message "Unable to resolve symbol: nope"))))
      (let ((refusal (should-error (replique-fresh-ensure "finding a use")
                                   :type 'user-error)))
        (should (string-match-p "Unable to resolve symbol" (error-message-string refusal)))))))

;;; Waiting for the repl

(ert-deftest replique-fresh-test-a-busy-repl-is-not-waited-on ()
  "The frames of a repl carry no request id - it is a stream of what it read
and printed - so the evaluation that ends next is whichever one was
running.  Waiting on a repl that is already evaluating would be waiting for
somebody else's form and calling its result ours."
  (let ((repl (replique-repl--make :at-prompt nil)))
    (cl-letf (((symbol-function 'replique-conn-live-p) (lambda (_conn) t)))
      (let ((refusal (should-error (replique-repl-send-code-sync repl "(+ 1 2)")
                                   :type 'user-error)))
        (should (string-match-p "busy" (error-message-string refusal)))))))

(ert-deftest replique-fresh-test-waiting-holds-until-the-repl-has-answered ()
  "Which is what the whole thing turns on: the question after the load has
to be asked of the process the load left behind, and a send that returned
as soon as it was written would ask it of the one before."
  (replique-test-process)
  (replique-test-with-repl repl
    (replique-test-hide (replique-repl--buffer repl))
    (let ((frame (replique-repl-send-code-sync repl "(+ 1 2)")))
      (should (equal "ret" (plist-get frame :tag)))
      (should (equal "3" (plist-get frame :value)))
      ;; and it really waited: the repl is done and the buffer says so
      (should (replique-repl--at-prompt repl))
      (should (string-match-p "3" (replique-test-text repl))))))

(ert-deftest replique-fresh-test-waiting-gets-the-exception-a-form-threw ()
  "An evaluation ends in a value, an exception, or the error a repl answers
what it could not read with, and which of them it was is the caller's to
read: a load that stopped is what stops the command it was for."
  (replique-test-process)
  (replique-test-with-repl repl
    (replique-test-hide (replique-repl--buffer repl))
    (let ((frame (replique-repl-send-code-sync repl "(throw (ex-info \"no\" {}))")))
      (should (equal "exception" (plist-get frame :tag)))
      (should (string-match-p "no" (plist-get frame :message))))))

(ert-deftest replique-fresh-test-a-real-process-that-records-nothing-offers-nothing ()
  "The same as the answer written out by hand, against a process really
answering: stock clojure refuses the question, and a refusal is not a list
of files to load."
  (replique-test-process)
  (replique-test-with-repl repl
    (replique-test-hide (replique-repl--buffer repl))
    (let ((frame (replique-process-request-sync (replique-repl-process repl)
                                                (list :op :stale)
                                                replique-name-timeout)))
      (if (equal "error" (plist-get frame :tag))
          (progn
            (should (string-match-p "keep track" (plist-get frame :message)))
            (should-not (replique-fresh--asked (replique-repl-process repl))))
        ;; the other half: a process whose compiler does record answers with
        ;; the two lists, and a fresh one has read every file as it is
        (should (plist-member frame :changed))
        (should-not (append (plist-get frame :changed) (plist-get frame :stale)))))))


;;; The whole of it, against a process that records what it compiled

(defun replique-fresh-test--fork ()
  "Return the jar of the clojure that records what it compiled, or skip.

The one the `:analysis\=' alias of the replique project names, built out
of the checkout beside it.  Everything here turns on a process that can
say what it has not read as it now is, and stock clojure cannot: the
tests that need one are skipped rather than passing against a process
that refuses every question they ask."
  (let ((jar (expand-file-name "../replique-clj/target/clojure-1.12.5-r1.jar"
                               (replique-test-project))))
    (unless (file-exists-p jar)
      (ert-skip "The clojure that records what it compiled is not built"))
    jar))

(defun replique-fresh-test--written (file text)
  "Write TEXT as FILE, dated ahead so that a reader sees it changed.

A file written and asked about in the same millisecond is a file whose
modification time has not moved, and what has changed is a comparison of
that time against the one the process recorded when it read it."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text))
  (set-file-times file (time-add (current-time) 10)))

(defun replique-fresh-test--started (directory)
  "Start a process in DIRECTORY and return it once it has connected."
  (let ((known replique-processes))
    (replique-start directory)
    (unless (replique-test-wait-for
             (lambda () (seq-difference replique-processes known)) 120)
      (error "The process did not start"))
    (car (seq-difference replique-processes known))))

(defun replique-fresh-test--uses ()
  "Return the uses of the var point is in, as xref would show them."
  (save-excursion
    (goto-char (point-min))
    (search-forward "(defn thing")
    (forward-char -2)
    (xref-backend-references 'replique (xref-backend-identifier-at-point 'replique))))

(ert-deftest replique-fresh-test-what-changed-is-loaded-before-the-uses-are-asked-for ()
  "The whole of it, against a process really answering: a file is loaded,
edited to use a var once more, and asked who uses that var.  The use
written since the process read the file is in the answer, which it is not
without the load - and that is the difference between a rename that
renames everything and one that leaves a caller behind."
  (let ((jar (replique-fresh-test--fork))
        (replique-reload-before-asking 'always))
    (replique-test-with-project dir
      ;; Named to the process the way the editor has it, which on this platform
      ;; is through a symlink: a temporary directory is reached through one, and
      ;; the process resolves the directory it was started in while the editor
      ;; does not.  So this also stands over the translation of a path into the
      ;; name the classpath gives it - see `replique.analysis/source-path'.
      (let* ((source (expand-file-name "src/probe/core.clj" dir)))
        (replique-fresh-test--written
         (expand-file-name "deps.edn" dir)
         (format "{:paths [\"src\"] :deps {org.clojure/clojure {:local/root %S}}}" jar))
        (replique-fresh-test--written
         source
         "(ns probe.core)\n(defn thing [] 1)\n(defn one [] (thing))\n")
        (let* ((replique-coordinates (format "{:local/root %S}" (replique-test-project)))
               (process (replique-fresh-test--started dir)))
          (unwind-protect
              (let ((repl (replique-repl process)))
                (replique-test-hide (replique-repl--buffer repl))
                (let ((buffer (find-file-noselect source)))
                  (unwind-protect
                      (with-current-buffer buffer
                        (setq replique-current-repl repl)
                        (replique-load-file)
                        (should (replique-test-wait-for
                                 (lambda () (replique-repl--at-prompt repl))))
                        ;; read once, and nothing has moved since
                        (let ((found (replique-fresh--asked process)))
                          (should found)
                          (should-not (append (plist-get found :changed)
                                              (plist-get found :stale))))
                        (should (equal 1 (length (replique-fresh-test--uses))))
                        ;; a second caller, written since
                        (replique-fresh-test--written
                         source
                         (concat "(ns probe.core)\n(defn thing [] 1)\n"
                                 "(defn one [] (thing))\n(defn two [] (thing))\n"))
                        (should (equal 2 (length (replique-fresh-test--uses))))
                        ;; and it was the load that did it, not the asking
                        (should (string-match-p "probe/core.clj"
                                                (replique-test-text repl))))
                    (kill-buffer buffer))))
            (replique-kill-process process)))))))


(provide 'replique-fresh-test)

;;; replique-fresh-test.el ends here
