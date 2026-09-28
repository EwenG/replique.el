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

(defun replique-stale-test--shown (found &optional directory dialect-keys)
  "Return the text a buffer showing FOUND holds, named from DIRECTORY.

DIALECT-KEYS is the question it answered, nil being Clojure."
  (replique-stale-test--shown-all
   (list (list :label (if dialect-keys "ClojureScript" "Clojure")
               :dialect-keys dialect-keys
               :found found))
   directory))

(defun replique-stale-test--shown-all (sections &optional directory stylesheets)
  "Return the text a buffer showing SECTIONS holds, named from DIRECTORY.

STYLESHEETS is what `replique-css-stale-in' answered, or nil."
  (with-temp-buffer
    (setq replique-stale--process (replique-process--make :directory directory)
          replique-stale--sections sections
          replique-stale--stylesheets stylesheets)
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
              replique-stale--sections
              '((:label "Clojure" :dialect-keys nil
                 :found (:changed ((:file "/p/src/app/util.clj"))))))
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
      (replique-stale--ask 'a-process nil))
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
      (replique-stale--ask process nil)
      (should (replique-test-wait-for (lambda () (or said shown)))))
    (if said
        (should (string-match-p "keep track of what it compiled" said))
      (should (buffer-live-p shown))
      (should (string-match-p "changed\\|Changed"
                              (with-current-buffer shown (buffer-string)))))
    (when (get-buffer "*replique-stale*") (kill-buffer "*replique-stale*"))))

(ert-deftest replique-stale-test-a-clojurescript-answer-says-which-it-is ()
  "A process can hold a Clojure application and a ClojureScript one at once,
and one buffer shows either - so the answer that would otherwise be taken for
the Clojure one says which language it is about.  The Clojure answer says
nothing, for the reason an absent `:dialect' means Clojure: it reads as it
always did."
  (let ((cljs (replique-stale-test--shown
               '(:changed ((:file "/p/src/app/core.cljs")) :stale nil)
               "/p/" '(:dialect :cljs :target :browser)))
        (clj (replique-stale-test--shown
              '(:changed ((:file "/p/src/app/core.clj")) :stale nil)
              "/p/")))
    (should (string-match-p "ClojureScript" cljs))
    (should (string-match-p "compiled them" cljs))
    (should-not (string-match-p "ClojureScript" clj))
    (should (string-match-p "read them" clj)))
  (should (string-match-p
           "compiled it"
           (replique-stale-test--shown '(:changed nil :stale nil) nil
                                       '(:dialect :cljs)))))

(ert-deftest replique-stale-test-a-stale-file-does-not-claim-the-macro-is-above ()
  "True of Clojure and not of ClojureScript, which is why it is not said: a
.cljs file expands macros written in .clj files, and those are not in the
list above - that list is the ClojureScript files."
  (let ((text (replique-stale-test--shown
               '(:changed nil :stale ((:file "/p/src/app/core.cljs")))
               "/p/" '(:dialect :cljs))))
    (should (string-match-p "of a file that has changed" text))
    (should-not (string-match-p "of a file above" text))))

(ert-deftest replique-stale-test-the-question-carries-the-buffers-dialect ()
  "Which language is stale is two questions in a process holding both, and
what says which is asked is the buffer - the same rule every question about a
name follows."
  (let ((asked nil))
    (cl-letf (((symbol-function 'replique-process-request)
               (lambda (_process msg _callback) (setq asked msg)))
              ((symbol-function 'replique-dialect-keys)
               (lambda () '(:dialect :cljs :target :node)))
              ((symbol-function 'replique-name-process)
               (lambda () 'a-process)))
      (replique-stale))
    (should (equal '(:op :stale :dialect :cljs :target :node) asked))))

(ert-deftest replique-stale-test-asking-again-asks-the-same-question ()
  "The buffer is not a buffer of either language, so reading the dialect off
it would read the dialect of whatever repl the commands are pointed at now -
and a buffer that answered about one language under the same heading as
another would be two answers nothing tells apart."
  (let ((asked nil))
    (with-temp-buffer
      (setq replique-stale--process 'a-process
            replique-stale--scope 'here
            replique-stale--sections
            '((:label "ClojureScript" :dialect-keys (:dialect :cljs :target :browser)
               :found nil)))
      (cl-letf (((symbol-function 'replique-process-request)
                 (lambda (_process msg _callback) (setq asked msg))))
        (replique-stale-refresh)))
    (should (equal '(:op :stale :dialect :cljs :target :browser) asked))))

(ert-deftest replique-stale-test-loading-loads-what-is-shown ()
  "`l' reloads the language the buffer is showing rather than the one the
commands are pointed at, for the reason `g' asks the same question again."
  (let ((reloaded 'unasked))
    (with-temp-buffer
      (setq replique-stale--scope 'here
            replique-stale--sections
            '((:dialect-keys (:dialect :cljs :target :node))))
      (cl-letf (((symbol-function 'replique-reload-all)
                 (lambda (&optional _waiting dialect) (setq reloaded dialect))))
        (replique-stale-reload)))
    (should (eq :cljs reloaded))
    (with-temp-buffer
      (setq replique-stale--scope 'here
            replique-stale--sections '((:dialect-keys nil)))
      (cl-letf (((symbol-function 'replique-reload-all)
                 (lambda (&optional _waiting dialect) (setq reloaded dialect))))
        (replique-stale-reload)))
    (should (eq :clj reloaded))
    (with-temp-buffer
      (setq replique-stale--scope 'app)
      (cl-letf (((symbol-function 'replique-reload-app)
                 (lambda () (setq reloaded 'app))))
        (replique-stale-reload)))
    (should (eq 'app reloaded))))

(ert-deftest replique-stale-test-every-language-is-a-section-of-its-own ()
  "A process can hold a Clojure application and a ClojureScript one on two
runtimes at once, and what is stale in one says nothing about the others.
Named, in the order they would be reloaded, because a list of files under
no heading is a list nothing says which compiler it is about."
  (let ((text (replique-stale-test--shown-all
               '((:label "Clojure" :dialect-keys nil
                  :found (:changed ((:file "/p/src/app/util.clj"))))
                 (:label "ClojureScript (browser)"
                  :dialect-keys (:dialect :cljs :target :browser)
                  :found (:changed ((:file "/p/src/app/core.cljs")) :connected t))
                 (:label "ClojureScript (node)"
                  :dialect-keys (:dialect :cljs :target :node)
                  :found (:changed nil :stale nil :connected t)))
               "/p/")))
    (should (string-match-p "^Clojure$" text))
    (should (string-match-p "ClojureScript (browser)" text))
    (should (string-match-p "ClojureScript (node)" text))
    (should (string-match-p "src/app/util.clj" text))
    (should (string-match-p "src/app/core.cljs" text))
    (should (string-match-p "Nothing has changed since this process compiled it" text))
    ;; in the order they are reloaded in, which is the order they are shown in
    (should (< (string-match "util.clj" text) (string-match "core.cljs" text)))))

(ert-deftest replique-stale-test-a-runtime-with-nowhere-to-run-it-says-so ()
  "A ClojureScript reload has a second act - the bodies have to be run in the
runtime - so a runtime with nothing connected to it is a reload that would
compile all of it and land nowhere.  Under the files, because it is the
answer to \"and then what\"; and only where there are files, because a
language with nothing to load has no then."
  (let ((stuck (replique-stale-test--shown-all
                '((:label "ClojureScript (browser)"
                   :dialect-keys (:dialect :cljs :target :browser)
                   :found (:changed ((:file "/p/a.cljs")) :connected nil)))
                "/p/"))
        (empty (replique-stale-test--shown-all
                '((:label "ClojureScript (browser)"
                   :dialect-keys (:dialect :cljs :target :browser)
                   :found (:changed nil :stale nil :connected nil)))
                "/p/"))
        (clj (replique-stale-test--shown-all
              '((:label "Clojure" :dialect-keys nil
                 :found (:changed ((:file "/p/a.clj")))))
              "/p/")))
    (should (string-match-p "Nothing is connected to this runtime" stuck))
    (should (< (string-match "a.cljs" stuck)
               (string-match "Nothing is connected" stuck)))
    (should-not (string-match-p "Nothing is connected" empty))
    ;; and never of Clojure, where the question does not exist
    (should-not (string-match-p "Nothing is connected" clj))))

(ert-deftest replique-stale-test-a-refused-question-is-shown-as-refused ()
  "One question refused is nothing to show and is said in the echo area.  One
of several refused, beside the ones that were answered, is a fact about
this process worth reading next to the rest - and two empty lists under a
heading would report a process that cannot answer as one with nothing to
do."
  (let ((text (replique-stale-test--shown-all
               '((:label "Clojure" :dialect-keys nil
                  :found (:tag "error" :message "no analysis here"))
                 (:label "ClojureScript (node)"
                  :dialect-keys (:dialect :cljs :target :node)
                  :found (:changed ((:file "/p/a.cljs")) :connected t)))
               "/p/")))
    (should (string-match-p "no analysis here" text))
    (should-not (string-match-p "Nothing has changed" text))))

(ert-deftest replique-stale-test-the-stylesheets-are-a-weaker-fact-and-say-so ()
  "The other sections are what the process compiled and when.  This one is
two modification times compared in Emacs, because the process has never
heard of a .scss - so the heading says what it is rather than claiming the
build would read these."
  (let ((text (replique-stale-test--shown-all
               '((:label "Clojure" :dialect-keys nil
                  :found (:changed nil :stale nil)))
               "/p/"
               '(("/p/public/css/main.css" . "/p/scss/_colours.scss")))))
    (should (string-match-p "Stylesheets" text))
    (should (string-match-p "public/css/main.css" text))
    (should (string-match-p "scss/_colours.scss is newer" text))
    (should (string-match-p "sass's to know" text))))

(ert-deftest replique-stale-test-a-stylesheet-opens-and-the-source-does-not ()
  "The output is the file that is behind - what a page fetches, and what a
build would write over.  The source is named beside it to say what the
output is behind, and it is one of many."
  (let ((opened nil))
    (cl-letf (((symbol-function 'find-file-noselect)
               (lambda (path &rest _) (setq opened path) (current-buffer)))
              ((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (with-temp-buffer
        (setq replique-stale--process (replique-process--make :directory "/p/")
              replique-stale--sections nil
              replique-stale--stylesheets
              '(("/p/public/css/main.css" . "/p/scss/main.scss")))
        (replique-stale--render)
        (goto-char (point-min))
        (should (search-forward "public/css/main.css" nil t))
        (push-button (1- (point)))))
    (should (equal "/p/public/css/main.css" opened))))

(ert-deftest replique-stale-test-the-application-is-asked-once-per-repl ()
  "One question per language and runtime rather than one per repl - two repls
on one runtime are one program - in the order they would be reloaded, and
the buffer is written when the last of them has arrived."
  (let ((asked nil)
        (shown nil))
    (cl-letf (((symbol-function 'replique-reload--repls)
               (lambda (_process)
                 (list 'clj-repl 'browser-repl)))
              ((symbol-function 'replique-reload--dialect-keys)
               (lambda (repl)
                 (when (eq repl 'browser-repl) '(:dialect :cljs :target :browser))))
              ((symbol-function 'replique-reload--label)
               (lambda (repl)
                 (if (eq repl 'browser-repl) "ClojureScript (browser)" "Clojure")))
              ((symbol-function 'replique-css-stale-in) (lambda (_root) nil))
              ((symbol-function 'replique-process-request)
               (lambda (_process msg callback)
                 (push msg asked)
                 (funcall callback '(:changed nil :stale nil :connected t))))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _) (setq shown buffer))))
      (replique-stale--ask-app (replique-process--make :directory "/p/")))
    (should (equal '((:op :stale)
                     (:op :stale :dialect :cljs :target :browser))
                   (nreverse asked)))
    (should (buffer-live-p shown))
    (with-current-buffer shown
      (should (eq 'app replique-stale--scope))
      (should (string-match-p "ClojureScript (browser)" (buffer-string))))
    (when (get-buffer "*replique-stale*") (kill-buffer "*replique-stale*"))))

(provide 'replique-stale-test)

;;; replique-stale-test.el ends here
