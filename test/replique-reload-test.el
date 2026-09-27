;;; replique-reload-test.el --- Tests for reloading the whole application  -*- lexical-binding: t; -*-

;;; Commentary:

;; One key that loads the Clojure, the ClojureScript of every runtime the
;; process has open, and the stylesheets.  What is tested here is the part
;; that is this command's own: the order, which repls are asked, what happens
;; to the ones after a load that stopped, and the one sentence the three of
;; them come back as.
;;
;; NOTHING HERE COMPILES OR BUILDS ANYTHING.  What `#replique/reload' loads
;; is the process's question and is tested in replique-2; what a stylesheet
;; build runs is `replique-css-test's, and the page's half of the reload is
;; tested against a document in replique-2's css-test.  The repls are the
;; stand-ins replique-repl-choice-test uses - a `cat' for the connection and
;; the handshake reply the dialect and the target are read off - and what is
;; sent to them is stubbed at `replique-repl-send-code-sync', which is the
;; one thing between this command and a compiler.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'replique-test)
(require 'replique-clojure-mode)
(require 'replique-reload)

(defvar replique-reload-test--procs nil
  "The operating system processes standing in for connections.")

(defvar replique-reload-test--buffers nil
  "The buffers standing in for repl buffers.")

(defun replique-reload-test--conn (&optional dialect target)
  "Return a live connection whose handshake reply said DIALECT and TARGET.

Both are strings, the way they arrive over the wire - see
`replique-repl-choice-test--conn', which this is."
  (let ((proc (start-process "replique-reload-test" nil "cat")))
    (push proc replique-reload-test--procs)
    (replique-conn--make
     :proc proc :kind 'repl :id "c1"
     :info (append (list :tag "reply" :op "hello" :role "repl" :connection "c1")
                   (when dialect (list :dialect dialect))
                   (when target (list :target target))))))

(defun replique-reload-test--process ()
  "Return a stand-in process, started in \"/p/\", with no repls yet."
  (replique-process--make :id "reload-test" :host "127.0.0.1" :port 1
                          :directory "/p/"
                          :control (replique-reload-test--conn)))

(defun replique-reload-test--repl (process &optional dialect target)
  "Return a repl of PROCESS of DIALECT and TARGET, and put it on PROCESS.

At a prompt, which is what a repl waiting to be sent something is: the
command asks all of them before it sends any of them anything."
  (let* ((buffer (generate-new-buffer "*replique-reload-test*"))
         (repl (replique-repl--make :process process
                                    :buffer buffer
                                    :given-name (buffer-name buffer)
                                    :conn (replique-reload-test--conn dialect target)
                                    :at-prompt t
                                    :to-echo 0)))
    (push buffer replique-reload-test--buffers)
    (push repl (replique-process--repls process))
    repl))

(defmacro replique-reload-test--with-repls (spec &rest body)
  "Run BODY with one process holding the repls SPEC names.

SPEC is a list of (VAR DIALECT TARGET), oldest first."
  (declare (indent 1))
  `(let* ((replique-reload-test--procs nil)
          (replique-reload-test--buffers nil)
          (process (replique-reload-test--process))
          (replique-processes (list process))
          (replique-current-process process)
          (replique-current-repl nil)
          ,@(mapcar (lambda (s)
                      `(,(nth 0 s) (replique-reload-test--repl
                                    process ,(nth 1 s) ,(nth 2 s))))
                    spec))
     (ignore process)
     (unwind-protect (progn ,@body)
       (dolist (proc replique-reload-test--procs)
         (when (process-live-p proc) (delete-process proc)))
       (dolist (buffer replique-reload-test--buffers)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun replique-reload-test--ret (value)
  "The frame a repl ends an evaluation that returned VALUE with."
  (list :tag "ret" :value value))

(defvar replique-reload-test--asked nil
  "The labels of the repls the reload was sent to, in order.")

(defvar replique-reload-test--said nil
  "What was put in the echo area, newest first.")

(defvar replique-reload-test--built nil
  "The commands the stylesheet build was asked to run.")

(defvar replique-reload-test--sent nil
  "The ops sent to the process, oldest first.")

(cl-defmacro replique-reload-test--run ((&key answers build reply saved) &rest body)
  "Run BODY with everything the process would do stubbed out.

ANSWERS is a function of the repl returning the frame its reload ends
with, and answers a vector of one file when it is not given.  BUILD is
what the stylesheet build returns - nil, meaning it worked, unless a test
says otherwise.  REPLY is the frame a `:reload-css' op is answered with,
and no answer at all when it is not given: an op nothing replies to is an
op whose callback never runs, which is what the tests that are not about
the sentence want.  SAVED, when given, is a variable the predicate
`save-some-buffers' was called with is put in - nothing is ever saved
here.

What is stubbed is `replique-repl-send-code-sync', which is the whole of
what reaches a compiler, and `replique-css--build', which is the whole of
what reaches a shell - `replique-css-test' says why the build is stubbed
there and not `call-process'."
  (declare (indent 1))
  ;; Set rather than bound, because what the assertions read is read after
  ;; this form has ended: a `let' of them would put back what they were, which
  ;; is nothing, exactly as the test starts looking
  `(progn
     (setq replique-reload-test--asked nil
           replique-reload-test--said nil
           replique-reload-test--built nil
           replique-reload-test--sent nil)
     (cl-letf (((symbol-function 'replique-repl-send-code-sync)
                (lambda (repl _code &optional _display)
                  (push (replique-reload--label repl) replique-reload-test--asked)
                  (funcall (or ,answers
                               (lambda (_r) (replique-reload-test--ret "[\"a.clj\"]")))
                           repl)))
               ((symbol-function 'replique-css--build)
                (lambda (commands _root)
                  (setq replique-reload-test--built commands)
                  ,build))
               ((symbol-function 'replique-process-request)
                (lambda (_process msg &optional callback)
                  (setq replique-reload-test--sent
                        (append replique-reload-test--sent (list msg)))
                  (when (and callback ,reply) (funcall callback ,reply))))
               ((symbol-function 'save-some-buffers)
                (lambda (&optional _arg predicate)
                  ,(if saved `(setq ,saved predicate) '(ignore predicate))))
               ((symbol-function 'message)
                (lambda (format &rest args)
                  (push (apply #'format format args) replique-reload-test--said))))
       ,@body)
     (setq replique-reload-test--asked (nreverse replique-reload-test--asked))))

(defun replique-reload-test--sentence ()
  "The last thing said, which is the one sentence the command ends with."
  (car replique-reload-test--said))

(defun replique-reload-test--files ()
  "The files the `:reload-css' ops named, in the order they went out."
  (mapcar (lambda (msg) (plist-get msg :file)) replique-reload-test--sent))

;;; Which repls, and in which order

(ert-deftest replique-reload-test-clojure-is-loaded-before-clojurescript ()
  "AND THAT IS THE ONE THING THE ORDER HAS TO GET RIGHT.  A ClojureScript
compile expands Clojure macros on this JVM, so a ClojureScript reload run
first would recompile the application against the macros of the branch
that was just left - and it would look like it worked.

The ClojureScript repl is the most recent one, which is the order a
process lists them in: a command that sent the reload to them as it found
them would send it to that one first."
  (replique-reload-test--with-repls ((clj nil nil) (cljs "cljs" "browser"))
    (ignore cljs clj)
    (replique-reload-test--run ()
      (replique-reload-app))
    (should (equal '("Clojure" "ClojureScript (browser)")
                   replique-reload-test--asked))))

(ert-deftest replique-reload-test-every-runtime-is-reloaded-and-each-one-once ()
  "A browser repl and a node repl are two programs, compiled separately out
of two compile environments, and reloading one says nothing about the
other.  Two repls on ONE runtime are one program: the second would find
that nothing had changed since the first loaded it, which is true and is
not what was asked."
  (replique-reload-test--with-repls ((clj1 nil nil)
                                     (browser1 "cljs" "browser")
                                     (node "cljs" "node")
                                     (browser2 "cljs" "browser")
                                     (clj2 nil nil))
    (ignore clj1 browser1 node browser2 clj2)
    (replique-reload-test--run ()
      (replique-reload-app))
    (should (equal '("Clojure" "ClojureScript (browser)" "ClojureScript (node)")
                   replique-reload-test--asked))))

(ert-deftest replique-reload-test-a-language-with-no-repl-is-skipped ()
  "WHICH IS WHERE THIS PARTS COMPANY WITH `replique-reload-all'.  That one is
asked for a language, by a buffer, and says there is no repl for it
because there is nothing else it could have been asked.  This one is asked
for whatever is running, and a process running only Clojure is a process
with nothing wrong with it."
  (replique-reload-test--with-repls ((clj nil nil))
    (ignore clj)
    (replique-reload-test--run ()
      (replique-reload-app))
    (should (equal '("Clojure") replique-reload-test--asked))
    (should-not (string-match-p "ClojureScript" (replique-reload-test--sentence)))))

(ert-deftest replique-reload-test-a-process-with-no-repl-at-all-is-told-what-to-open ()
  "There is nothing to reload in a process nobody has opened a repl on, and
nothing this could load it in either: a reload compiles, prints and is
interrupted in a repl."
  (replique-reload-test--with-repls ()
    (let ((message (cadr (should-error (replique-reload-app) :type 'user-error))))
      (should (string-match-p "replique-repl" message)))))

;;; A load that stopped

(ert-deftest replique-reload-test-a-load-that-stopped-stops-the-ones-after-it ()
  "A checkout that does not compile is ONE thing wrong, and the second wall
of errors is the same thing wrong said twice.  The ClojureScript is not
asked, and what stopped is named rather than repeated - the exception is
in the repl that threw it, whole and triaged, which is where it is read."
  (replique-reload-test--with-repls ((cljs "cljs" "browser") (clj nil nil))
    (ignore cljs clj)
    (replique-reload-test--run
        (:answers (lambda (_repl)
                    (list :tag "exception" :message "Syntax error")))
      (replique-reload-app))
    (should (equal '("Clojure") replique-reload-test--asked))
    (should (string-match-p "the Clojure load stopped"
                            (replique-reload-test--sentence)))))

(ert-deftest replique-reload-test-the-stylesheets-are-built-although-a-load-stopped ()
  "Sass has no opinion about a macro that will not compile.  The two halves
are built by two programs that neither read each other nor are read by
each other, and leaving the stylesheets on the old branch because the
Clojure did not compile would be a second thing to notice later."
  (replique-reload-test--with-repls ((clj nil nil))
    (ignore clj)
    (let ((replique-css-entry "scss/main.scss")
          (replique-css-outputs '("public/css/main.css")))
      (replique-reload-test--run
          (:answers (lambda (_repl) (list :tag "exception" :message "Syntax error")))
        (replique-reload-app)))
    (should (equal '(("sass" "--embed-source-map" "/p/scss/main.scss"
                      "/p/public/css/main.css"))
                   replique-reload-test--built))
    (should (equal '("/p/public/css/main.css") (replique-reload-test--files)))))

;;; The stylesheets

(ert-deftest replique-reload-test-the-build-is-the-projects-and-so-are-the-paths ()
  "Relative to the directory the process was started in, and not to whatever
buffer the key was pressed in - which for this command is a buffer that
may have nothing to do with stylesheets at all.  What is reloaded is what
the build WROTE."
  (replique-reload-test--with-repls ((clj nil nil))
    (ignore clj)
    (let ((default-directory "/somewhere/else/")
          (replique-css-entry "scss/main.scss")
          (replique-css-outputs '("public/css/main.css" "public/css/trial.css")))
      (replique-reload-test--run ()
        (replique-reload-app)))
    (should (equal '(("sass" "--embed-source-map" "/p/scss/main.scss"
                      "/p/public/css/main.css")
                     ("sass" "--embed-source-map" "/p/scss/main.scss"
                      "/p/public/css/trial.css"))
                   replique-reload-test--built))
    (should (equal '("/p/public/css/main.css" "/p/public/css/trial.css")
                   (replique-reload-test--files)))))

(ert-deftest replique-reload-test-a-project-with-no-stylesheets-is-not-an-error ()
  "`replique-reload-css' is pressed in a stylesheet, so a project that says
nothing about how to build one is a project that has been asked something
it cannot answer, and it is told what to write in .dir-locals.el.  Here
nobody asked about stylesheets: the languages are reloaded and nothing is
said about a project that has none."
  (replique-reload-test--with-repls ((clj nil nil))
    (ignore clj)
    (let ((replique-css-entry nil)
          (replique-css-outputs nil)
          (replique-css-build-command nil))
      (replique-reload-test--run ()
        (replique-reload-app)))
    (should (equal '("Clojure") replique-reload-test--asked))
    (should-not replique-reload-test--built)
    (should (equal "replique: loaded Clojure" (replique-reload-test--sentence)))))

(ert-deftest replique-reload-test-a-build-that-failed-is-shown-as-it-printed ()
  "With what the languages did in front of it rather than instead of it.
What is wrong with a stylesheet is something sass has already said better
than this could, and the languages having loaded is still the answer to
half of what was asked - it is not something to throw away because the
other half failed."
  (replique-reload-test--with-repls ((clj nil nil))
    (ignore clj)
    (let ((replique-css-entry "scss/main.scss")
          (replique-css-outputs '("public/css/main.css")))
      (replique-reload-test--run (:build "Error: Undefined variable.")
        (replique-reload-app)))
    (should-not (replique-reload-test--files))
    (should (string-match-p "loaded Clojure" (replique-reload-test--sentence)))
    (should (string-match-p "Undefined variable" (replique-reload-test--sentence)))))

;;; One sentence

(ert-deftest replique-reload-test-the-three-of-them-are-one-sentence ()
  "THE THREE HALVES FINISH AT THREE DIFFERENT MOMENTS, and three messages in
the echo area are the first two gone.  The one anybody would have wanted
to read is whichever of them did not do what was expected, and it is never
the last one."
  (replique-reload-test--with-repls ((cljs "cljs" "browser") (clj nil nil))
    (ignore cljs clj)
    (let ((replique-css-entry "scss/main.scss")
          (replique-css-outputs '("public/css/main.css")))
      (replique-reload-test--run
          (:reply (list :tag "reply"
                        :reloaded '("http://localhost:8082/css/main.css")
                        :stylesheets '("http://localhost:8082/css/main.css")))
        (replique-reload-app)))
    (should (equal (concat "replique: loaded Clojure, ClojureScript (browser)"
                           " - reloaded http://localhost:8082/css/main.css")
                   (replique-reload-test--sentence)))))

(ert-deftest replique-reload-test-a-language-with-nothing-to-load-says-so ()
  "And is not reported as having loaded something.  A reload answers with the
files it loaded, and the empty vector is what it answers when there were
none - which is told apart from a list of files by being exactly \"[]\",
the printed empty vector, rather than by anything read out of the rest."
  (replique-reload-test--with-repls ((cljs "cljs" "browser") (clj nil nil))
    (ignore cljs clj)
    (replique-reload-test--run
        (:answers (lambda (repl)
                    (replique-reload-test--ret
                     (if (eq :clj (replique-repl-dialect repl))
                         "[]"
                       "[\"a.cljs\"]"))))
      (replique-reload-app))
    (should (equal "replique: loaded ClojureScript (browser)"
                   (replique-reload-test--sentence)))))

(ert-deftest replique-reload-test-nothing-to-load-anywhere-names-what-was-asked ()
  "Rather than saying nothing, which is how a key that did nothing because
it went to the wrong process looks exactly like a key that did nothing
because there was nothing to do."
  (replique-reload-test--with-repls ((cljs "cljs" "browser") (clj nil nil))
    (ignore cljs clj)
    (replique-reload-test--run
        (:answers (lambda (_repl) (replique-reload-test--ret "[]")))
      (replique-reload-app))
    (should (equal "replique: nothing to load in Clojure, ClojureScript (browser)"
                   (replique-reload-test--sentence)))))

;;; Before anything is sent

(ert-deftest replique-reload-test-a-busy-repl-is-said-so-before-anything-is-sent ()
  "ALL OF THEM ARE ASKED BEFORE ANY OF THEM IS SENT ANYTHING.  Refusing the
second one after the first has already recompiled the application would
leave the process half way between two branches, with nothing said about
which half - which is the state this command exists to get out of."
  (replique-reload-test--with-repls ((cljs "cljs" "browser") (clj nil nil))
    (ignore clj)
    (setf (replique-repl--at-prompt cljs) nil)
    (replique-reload-test--run ()
      (let ((message (cadr (should-error (replique-reload-app) :type 'user-error))))
        (should (string-match-p "ClojureScript (browser)" message))
        (should (string-match-p "busy" message))))
    (should-not replique-reload-test--asked)))

;;; What is offered to be saved

(ert-deftest replique-reload-test-both-languages-and-the-stylesheets-are-saved ()
  "What is about to be read is the disk: the process compiles files and the
build reads them.  `replique-reload-all' offers the Clojure alone because
Clojure is all it loads, and a stylesheet left unsaved here would be built
as it was before the edit and reloaded as though it were after it."
  (let ((pred nil))
    (replique-reload-test--with-repls ((clj nil nil))
      (ignore clj)
      (replique-reload-test--run (:saved pred)
        (replique-reload-app)))
    (should pred)
    (should (with-temp-buffer
              (setq buffer-file-name "/p/src/a.clj")
              (replique-clojure-mode)
              (funcall pred)))
    (should (with-temp-buffer
              (setq buffer-file-name "/p/scss/_buttons.scss")
              (funcall pred)))))

(ert-deftest replique-reload-test-a-file-of-another-project-is-not-saved ()
  "This command is handed no file and would otherwise offer you every
modified stylesheet open in Emacs, including the ones of a project this
process has never heard of.  The other two reloads never face it: they are
each about one file and they are handed it."
  (let ((pred nil))
    (replique-reload-test--with-repls ((clj nil nil))
      (ignore clj)
      (replique-reload-test--run (:saved pred)
        (replique-reload-app)))
    (should-not (with-temp-buffer
                  (setq buffer-file-name "/elsewhere/scss/main.scss")
                  (funcall pred)))
    (should-not (with-temp-buffer
                  (setq buffer-file-name "/p/notes.org")
                  (funcall pred)))))

(provide 'replique-reload-test)

;;; replique-reload-test.el ends here
