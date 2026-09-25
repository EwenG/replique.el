;;; replique-css-test.el --- Tests for reloading a stylesheet  -*- lexical-binding: t; -*-

;;; Commentary:

;; The editor's half of a stylesheet reload, which is the keystroke and the
;; sentence: which file goes out, and what is said about what came back.
;;
;; Everything that decides WHICH stylesheet is reloaded happens in the page -
;; the op sends a path and the page matches it against its own URLs - so that
;; is tested where it is decided, against a document, in replique-2's
;; css-test.  What is left here is small on purpose.
;;
;; Most of it needs no process: what goes out is a message and what comes
;; back is a frame, and a plist is a plist however it arrived.  The one at
;; the end uses a process, because whether the op this sends is an op a
;; process answers is not something a stub can say.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'replique-test)
(require 'replique-css)

(defmacro replique-css-test--said (&rest body)
  "Run BODY with `message' collected, and return what it said, newest first."
  (declare (indent 0))
  `(let ((said nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args) (push (apply #'format format args) said))))
       ,@body)
     said))

(defmacro replique-css-test--sent (&rest body)
  "Run BODY with the process stubbed, and return the message it sent."
  (declare (indent 0))
  `(let ((sent nil))
     (cl-letf (((symbol-function 'replique-name-process)
                (lambda () (replique-process--make :directory "/p/")))
               ((symbol-function 'replique-process-request)
                (lambda (_process msg &optional _callback) (setq sent msg))))
       ,@body)
     sent))

;;; What is said about what came back

(ert-deftest replique-css-test-a-refusal-is-shown-as-the-process-wrote-it ()
  "An op a process cannot answer is answered all the same, and the sentence
names what to change.  Nothing here can word it better than the process
that knows why."
  (let ((said (replique-css-test--said
                (replique-css--report
                 '("/a/main.css")
                 '((:tag "error" :error "no-cljs"
                        :message "This process cannot reload a stylesheet in a browser: there is no ClojureScript compiler on its classpath."))))))
    (should (equal 1 (length said)))
    (should (string-match-p "no ClojureScript compiler" (car said)))))

(ert-deftest replique-css-test-a-page-that-could-not-be-asked-keeps-its-url ()
  "The answer when nobody has the page open, and the useful half of it is
the URL: a repl started before the browser was opened is the normal way
round.  Shown as it was written, because rewording it is how the URL
would be lost."
  (let ((said (replique-css-test--said
                (replique-css--report
                 '("/a/main.css")
                 '((:tag "reply" :reloaded nil :stylesheets nil
                        :note "No browser is connected. Open http://127.0.0.1:59280/ - or import runtime_browser.js from it in a page of your own - and evaluate this again."))))))
    (should (string-match-p "http://127.0.0.1:59280/" (car said)))))

(ert-deftest replique-css-test-what-was-reloaded-is-named ()
  "All of it.  Every link that ties for the longest match is reloaded, so
one keystroke can be several stylesheets, and saying \"reloaded\" without
saying what would leave the page that did not match looking like the page
that did."
  (let ((said (replique-css-test--said
                (replique-css--report
                 '("/a/css/main.css")
                 '((:tag "reply"
                        :reloaded ("http://localhost:8082/css/main.css"
                                   "http://localhost:8082/theme/main.css")
                        :stylesheets ("http://localhost:8082/css/main.css"
                                      "http://localhost:8082/theme/main.css"
                                      "http://localhost:8082/vendor.css")))))))
    (should (string-match-p "http://localhost:8082/css/main\\.css" (car said)))
    ;; both of them, or the one that did not reload looks like the one that did
    (should (string-match-p "http://localhost:8082/theme/main\\.css" (car said)))
    ;; and not the ones that were only listed
    (should-not (string-match-p "vendor" (car said)))))

(ert-deftest replique-css-test-nothing-matching-says-what-the-page-has ()
  "THE WHOLE OF WHY THE LIST COMES BACK.  Replique 1 had this list in its
hand at this exact moment - it had just asked for it - and threw it away,
leaving \"Could not find a css file to reload\" and nothing to go on.  The
usual reason is that the file being edited is not the one the page
includes, and the list is what says so."
  (let ((said (replique-css-test--said
                (replique-css--report
                 '("/a/scss/partials/_buttons.css")
                 '((:tag "reply" :reloaded nil
                        :stylesheets ("http://localhost:8082/css/main.css")))))))
    (should (string-match-p "_buttons\\.css" (car said)))
    (should (string-match-p "http://localhost:8082/css/main\\.css" (car said)))))

(ert-deftest replique-css-test-a-page-with-no-stylesheets-says-that-instead ()
  "Which is a different thing from a page that has some and matches none,
and it is nearly always the same mistake: the page connected to the repl
is not the page the application is in."
  (let ((said (replique-css-test--said
                (replique-css--report '("/a/main.css")
                                      '((:tag "reply" :reloaded nil :stylesheets nil))))))
    (should (string-match-p "no stylesheets" (car said)))))

;;; What goes out

(ert-deftest replique-css-test-the-op-carries-the-file-and-nothing-else ()
  "A path on this machine, absolute.  What the page does with it is the
page's - it is matched against the page's URLs by the longest suffix the
two share, and never opened - so there is nothing else for this half to
send and nothing here to decide."
  (let ((sent (replique-css-test--sent
                (replique-reload-css "/a/css/main.css"))))
    (should (equal :reload-css (plist-get sent :op)))
    (should (equal "/a/css/main.css" (plist-get sent :file)))
    (should (equal 4 (length sent)))))

(ert-deftest replique-css-test-a-relative-file-goes-out-absolute ()
  "Expanded here rather than left to the process, which would root it in
the directory the jvm was started in - a directory neither of them was
talking about."
  (let* ((default-directory "/tmp/somewhere/")
         (sent (replique-css-test--sent (replique-reload-css "css/main.css"))))
    (should (equal "/tmp/somewhere/css/main.css" (plist-get sent :file)))))

(ert-deftest replique-css-test-the-buffer-is-offered-to-be-saved-first ()
  "What the page fetches is the file on the disk, so a buffer with unsaved
changes would reload the version you have just replaced - the one case
where the command looks broken while working exactly as told.  Offered in
`comint-check-source's words, which is what `replique-load-file' answers
the same question with."
  (let ((file (make-temp-file "replique-css-test" nil ".css" "a{}"))
        (offered nil))
    (unwind-protect
        (with-temp-buffer
          (set-visited-file-name file t)
          (set-buffer-modified-p nil)
          (cl-letf (((symbol-function 'comint-check-source)
                     (lambda (f) (setq offered f))))
            (let ((sent (replique-css-test--sent
                          (call-interactively #'replique-reload-css))))
              (should (equal file offered))
              (should (equal file (plist-get sent :file))))))
      (delete-file file))))

(ert-deftest replique-css-test-a-buffer-with-no-file-is-refused ()
  "There is nothing to reload and nothing to guess at: what the page holds
is files, and a buffer nobody has saved is not one of them."
  (with-temp-buffer
    (should-error (call-interactively #'replique-reload-css) :type 'user-error)))

;;; One sentence for the whole build

(ert-deftest replique-css-test-several-outputs-are-one-sentence ()
  "THE REASON THE REPORT TAKES ALL OF THEM AT ONCE.  A build that writes
main.css, trial.css and design-system.css is three ops, and the page you
have open includes one of the three.  Reported one at a time, the two that
did not match would both say so and the last of them would be the sentence
left on the screen - the reload that worked would be the one you could not
see."
  (let ((said (replique-css-test--said
                (replique-css--report
                 '("/p/css/main.css" "/p/css/trial.css")
                 '((:tag "reply" :reloaded ("http://localhost:8082/css/main.css")
                         :stylesheets ("http://localhost:8082/css/main.css"))
                   (:tag "reply" :reloaded nil
                         :stylesheets ("http://localhost:8082/css/main.css")))))))
    (should (equal 1 (length said)))
    (should (string-match-p "reloaded http://localhost:8082/css/main\\.css" (car said)))
    (should-not (string-match-p "nothing on the page" (car said)))))

(ert-deftest replique-css-test-what-reloaded-and-what-could-not-be-asked-are-both-said ()
  "Because they can happen together: a page busy with the require you sent
a moment ago answers one of these and not the other, and saying only the
half that worked is how the half that did not goes unnoticed."
  (let ((said (replique-css-test--said
                (replique-css--report
                 '("/p/css/main.css" "/p/css/trial.css")
                 '((:tag "reply" :reloaded ("http://localhost:8082/css/main.css"))
                   (:tag "reply" :reloaded nil :stylesheets nil
                         :note "The runtime was busy for the whole 2000ms: nothing was evaluated."))))))
    (should (string-match-p "reloaded http" (car said)))
    (should (string-match-p "busy" (car said)))))

;;; Building what a page can fetch

(defmacro replique-css-test--built (config &rest body)
  "Run BODY with CONFIG let-bound and the build stubbed.
Return (COMMANDS . SENT): what would have run, and the ops that went out.

`replique-css--build\=' and not `call-process\=': Emacs compiles elisp
natively in the background and reaches for `call-process\=' to do it, so a
stub of that one answers questions this test was never asked - which is
not a guess, it is what happened here first."
  (declare (indent 1))
  `(let ((commands nil)
         (sent nil))
     (cl-letf (((symbol-function 'replique-name-process)
                (lambda () (replique-process--make :directory "/p/")))
               ((symbol-function 'replique-process-request)
                (lambda (_process msg &optional _callback) (push msg sent)))
               ((symbol-function 'replique-css--build)
                (lambda (cs _root) (setq commands cs) nil)))
       (let ,config ,@body))
     (cons commands (nreverse sent))))

(ert-deftest replique-css-test-a-css-file-is-not-built ()
  "It is already what a page fetches.  Building one would mean running sass
over a stylesheet that is not sass."
  (let ((done (replique-css-test--built ((replique-css-entry "scss/main.scss")
                                         (replique-css-outputs '("public/css/main.css")))
                (replique-reload-css "/p/public/css/main.css"))))
    (should-not (car done))
    (should (equal '("/p/public/css/main.css")
                   (mapcar (lambda (m) (plist-get m :file)) (cdr done))))))

(ert-deftest replique-css-test-a-partial-builds-the-entry-point-and-not-itself ()
  "THE ONE THING REPLIQUE 1 GOT RIGHT HERE, kept.  A file whose name begins
with an underscore is included by another one and compiles to nothing on
its own, so building the buffer would build nothing at all - and what is
reloaded is what the build wrote, which is not this buffer either."
  (let ((done (replique-css-test--built ((replique-css-entry "scss/main.scss")
                                         (replique-css-outputs '("public/css/main.css")))
                (replique-reload-css "/p/scss/_buttons.scss"))))
    (should (equal '(("sass" "--embed-source-map" "/p/scss/main.scss"
                      "/p/public/css/main.css"))
                   (car done)))
    (should (equal '("/p/public/css/main.css")
                   (mapcar (lambda (m) (plist-get m :file)) (cdr done))))))

(ert-deftest replique-css-test-a-build-command-is-run-as-written ()
  "Once, with nothing substituted into it.  A project with a real build has
a pipeline - sass, then autoprefixer, then three destinations - and
replique running a second one of its own would write almost the same CSS
and disagree with the first in exactly the ways that are hard to see."
  (let ((done (replique-css-test--built
                  ((replique-css-build-command '("npx" "gulp" "devCss"))
                   (replique-css-outputs '("public/css/main.css"
                                           "public/css/trial.css")))
                (replique-reload-css "/p/scss/main.scss"))))
    (should (equal '(("npx" "gulp" "devCss")) (car done)))
    (should (equal '("/p/public/css/main.css" "/p/public/css/trial.css")
                   (mapcar (lambda (m) (plist-get m :file)) (cdr done))))))

(ert-deftest replique-css-test-a-build-that-failed-reloads-nothing ()
  "And says what it printed, as it printed it: what is wrong with a
stylesheet is something sass has already said better than this could, and
the line it names is in the file you are looking at.  Reloading anyway
would put the last stylesheet that DID build back into the page and call
it success."
  (let ((sent nil)
        (said nil))
    (cl-letf (((symbol-function 'replique-name-process)
               (lambda () (replique-process--make :directory "/p/")))
              ((symbol-function 'replique-process-request)
               (lambda (_process msg &optional _callback) (push msg sent)))
              ((symbol-function 'message)
               (lambda (format &rest args) (push (apply #'format format args) said)))
              ((symbol-function 'replique-css--build)
               (lambda (_commands _root) "Error: Undefined variable.")))
      (let ((replique-css-entry "scss/main.scss")
            (replique-css-outputs '("public/css/main.css")))
        (replique-reload-css "/p/scss/main.scss")))
    (should-not sent)
    (should (string-match-p "Undefined variable" (car said)))))

;;; And the build itself, with a real subprocess

(ert-deftest replique-css-test-a-build-that-worked-says-nothing ()
  "Nil is the answer that means the outputs are worth reloading."
  (should-not (replique-css--build '(("sh" "-c" "exit 0")) temporary-file-directory)))

(ert-deftest replique-css-test-a-build-answers-with-what-it-printed ()
  "Both streams, as the program wrote them: sass says what is wrong with a
stylesheet better than anything here could, and it says it on stdout."
  (let ((failure (replique-css--build '(("sh" "-c" "echo boom; exit 3"))
                                      temporary-file-directory)))
    (should (string-match-p "boom" failure))))

(ert-deftest replique-css-test-a-build-stops-at-the-first-failure ()
  "A build is steps, and a step after a failed one is a step run on what the
failed one did not write."
  (let ((failure (replique-css--build '(("sh" "-c" "echo first; exit 1")
                                        ("sh" "-c" "echo second; exit 1"))
                                      temporary-file-directory)))
    (should (string-match-p "first" failure))
    (should-not (string-match-p "second" failure))))

(ert-deftest replique-css-test-the-build-runs-where-the-project-is ()
  "And not in the directory of whatever partial you happened to be editing."
  (let ((where (replique-css--build '(("sh" "-c" "pwd; exit 1"))
                                    temporary-file-directory)))
    (should (equal (file-truename (file-name-as-directory where))
                   (file-truename temporary-file-directory)))))

(ert-deftest replique-css-test-a-project-that-says-nothing-is-told-what-to-say ()
  "Rather than asked.  Replique 1 asked which file to compile to on every
single reload - with the remembered answer preselected, so it was a return
key you pressed a hundred times a day - and forgot all of it when Emacs
stopped.  Two lines in .dir-locals.el are in the repository, are the same
for everybody working on it, and are never asked for again."
  (cl-letf (((symbol-function 'replique-name-process)
             (lambda () (replique-process--make :directory "/p/"))))
    (let ((replique-css-entry nil)
          (replique-css-outputs nil)
          (replique-css-build-command nil))
      (let ((message (cadr (should-error (replique-reload-css "/p/scss/main.scss")
                                         :type 'user-error))))
        (should (string-match-p "replique-css-entry" message))
        (should (string-match-p "replique-css-outputs" message))
        (should (string-match-p "dir-locals" message))))))

(ert-deftest replique-css-test-the-paths-are-the-processs-and-not-the-buffers ()
  "A build runs where the project is, and the two paths it is given are
written relative to that - which is the directory the process was started
in, the same one a relative :main-js file is relative to.  `default-directory'
would be the directory of whatever partial you happened to be editing."
  (let* ((default-directory "/somewhere/else/")
         (done (replique-css-test--built ((replique-css-entry "scss/main.scss")
                                          (replique-css-outputs '("public/css/main.css")))
                 (replique-reload-css "/p/scss/_buttons.scss"))))
    (should (equal '(("sass" "--embed-source-map" "/p/scss/main.scss"
                      "/p/public/css/main.css"))
                   (car done)))))

;;; And a process

(ert-deftest replique-css-test-the-process-is-asked-and-answers ()
  "Whichever process it is.  One with the ClojureScript compiler on its
classpath starts its browser runtime and answers that no page is
connected - naming the URL to open, which is the honest answer to a
reload asked for before the browser was opened - and one without it
refuses by name.  Either way the op is an op a process answers, and the
reply is a reply this can read, which is what nothing short of a process
can say."
  (replique-test-with-project dir
    (let ((said (replique-css-test--said
                  (replique-reload-css (expand-file-name "css/main.css" dir)
                                       (replique-test-process))
                  (replique-test-wait-for
                   (lambda ()
                     (seq-find (lambda (line)
                                 (string-match-p "No browser is connected\\|ClojureScript"
                                                 line))
                               said))
                   120))))
      (should (seq-find (lambda (l)
                          (string-match-p "No browser is connected\\|ClojureScript" l))
                        said)))))

(provide 'replique-css-test)

;;; replique-css-test.el ends here
