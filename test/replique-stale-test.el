;;; replique-stale-test.el --- Tests for what has to be loaded again  -*- lexical-binding: t; -*-

;;; Commentary:

;; What a reload would load, shown before it does it.  Which files those
;; are is the process's answer and is tested where it is computed; what is
;; here is the buffer that shows it - that the two lists stay two, that a
;; file is named the way somebody working in the project names it, and that
;; each one opens.
;;
;; The rendering is tested without a process: what it is given is what the
;; process said, and a plist is a plist however it arrived.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-stale)

(defun replique-stale-test--shown (found &optional directory)
  "Return the text a buffer showing FOUND holds, named from DIRECTORY."
  (with-temp-buffer
    (setq replique-stale--process (replique-process--make :directory directory)
          replique-stale--found found)
    (replique-stale--render)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest replique-stale-test-the-two-lists-stay-two ()
  "What was edited and what that made stale are two different facts about a
file, and the second is the one nothing in a buffer says: these were not
touched, and hold the expansion an edited macro used to make."
  (let ((text (replique-stale-test--shown
               '(:changed ((:file "/p/src/app/util.clj"))
                 :stale ((:file "/p/src/app/core.clj")))
               "/p/")))
    (should (string-match-p "Changed since the process read them" text))
    (should (string-match-p "src/app/util.clj" text))
    (should (string-match-p "out of date all the same" text))
    (should (string-match-p "src/app/core.clj" text))
    ;; and in that order, which is what makes the second heading mean the
    ;; files under it rather than the ones above
    (should (< (string-match "util.clj" text) (string-match "core.clj" text)))))

(ert-deftest replique-stale-test-nothing-stale-says-so ()
  "Rather than showing two headings with nothing under them, which reads as
an answer that did not arrive."
  (let ((text (replique-stale-test--shown '(:changed nil :stale nil))))
    (should (string-match-p "Nothing has changed" text))
    (should-not (string-match-p "out of date" text))))

(ert-deftest replique-stale-test-only-what-changed-is-shown-where-nothing-is-stale ()
  "A macro nothing expands is an ordinary thing to edit, and the second
heading would then stand over nothing."
  (let ((text (replique-stale-test--shown '(:changed ((:file "/p/a.clj")) :stale nil))))
    (should (string-match-p "Changed since" text))
    (should-not (string-match-p "out of date all the same" text))))

(ert-deftest replique-stale-test-a-file-is-named-the-way-its-project-does ()
  "The path it has under the directory the process was started in, which is
how a developer names their own files.  The whole path where the file is
somewhere else: a relative name climbing out of the project says less than
the path it is a longer way of writing.  And an entry of an archive has no
path at all, so it is named as the two halves this protocol writes one in."
  (should (equal "src/app/util.clj"
                 (replique-stale--label '(:file "/p/src/app/util.clj") "/p/")))
  (should (equal "/elsewhere/other.clj"
                 (replique-stale--label '(:file "/elsewhere/other.clj") "/p/")))
  (should (equal "/p/src/app/util.clj"
                 (replique-stale--label '(:file "/p/src/app/util.clj") nil)))
  (should (equal "clojure/string.clj in clojure-1.12.5.jar"
                 (replique-stale--label '(:file "/m2/clojure-1.12.5.jar"
                                          :entry "clojure/string.clj")
                                        "/p/"))))

(ert-deftest replique-stale-test-each-file-opens ()
  "The list is a list of places to go to.  What opens one is what opens a
definition - the file, and the entry beside it where the file is an
archive - so a file inside a jar opens here too."
  (let ((opened nil))
    (cl-letf (((symbol-function 'replique-symbol-visit)
               (lambda (found) (setq opened found) (current-buffer)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (with-temp-buffer
        (setq replique-stale--process (replique-process--make :directory "/p/")
              replique-stale--found '(:changed ((:file "/p/src/app/util.clj"))))
        (replique-stale--render)
        (goto-char (point-min))
        (should (search-forward "util.clj" nil t))
        (push-button (1- (point)))))
    (should (equal '(:file "/p/src/app/util.clj") opened))))

(ert-deftest replique-stale-test-what-cannot-be-answered-is-said-and-nothing-is-shown ()
  "A buffer is what an answer looks like, so a refusal must not open one -
an empty staleness buffer would read as a process with nothing to do."
  (when (get-buffer "*replique-stale*") (kill-buffer "*replique-stale*"))
  (let ((said nil))
    (cl-letf (((symbol-function 'replique-process-request)
               (lambda (_process _msg callback)
                 (funcall callback '(:tag "error" :message "no analysis here"))))
              ((symbol-function 'message)
               (lambda (format &rest args) (setq said (apply #'format format args)))))
      (replique-stale--ask 'a-process))
    (should (string-match-p "no analysis here" said))
    (should-not (get-buffer "*replique-stale*"))))

(ert-deftest replique-stale-test-the-process-is-asked-and-the-answer-shown ()
  "Whichever process it is.  One whose compiler wrote down what it compiled
answers with two lists; one that did not says so, and says what to start it
on instead of answering that there is nothing to do."
  (let* ((process (replique-test-process))
         (said nil)
         (shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args) (setq said (apply #'format format args))))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _) (setq shown buffer))))
      (replique-stale--ask process)
      (should (replique-test-wait-for (lambda () (or said shown)))))
    (if said
        (should (string-match-p "keep track of what it compiled" said))
      (should (buffer-live-p shown))
      (should (string-match-p "changed\\|Changed"
                              (with-current-buffer shown (buffer-string)))))
    (when (get-buffer "*replique-stale*") (kill-buffer "*replique-stale*"))))

(provide 'replique-stale-test)

;;; replique-stale-test.el ends here
