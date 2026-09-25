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
                 "/a/main.css"
                 '(:tag "error" :error "no-cljs"
                        :message "This process cannot reload a stylesheet in a browser: there is no ClojureScript compiler on its classpath.")))))
    (should (equal 1 (length said)))
    (should (string-match-p "no ClojureScript compiler" (car said)))))

(ert-deftest replique-css-test-a-page-that-could-not-be-asked-keeps-its-url ()
  "The answer when nobody has the page open, and the useful half of it is
the URL: a repl started before the browser was opened is the normal way
round.  Shown as it was written, because rewording it is how the URL
would be lost."
  (let ((said (replique-css-test--said
                (replique-css--report
                 "/a/main.css"
                 '(:tag "reply" :reloaded nil :stylesheets nil
                        :note "No browser is connected. Open http://127.0.0.1:59280/ - or import runtime_browser.js from it in a page of your own - and evaluate this again.")))))
    (should (string-match-p "http://127.0.0.1:59280/" (car said)))))

(ert-deftest replique-css-test-what-was-reloaded-is-named ()
  "All of it.  Every link that ties for the longest match is reloaded, so
one keystroke can be several stylesheets, and saying \"reloaded\" without
saying what would leave the page that did not match looking like the page
that did."
  (let ((said (replique-css-test--said
                (replique-css--report
                 "/a/css/main.css"
                 '(:tag "reply"
                        :reloaded ("http://localhost:8082/css/main.css"
                                   "http://localhost:8082/theme/main.css")
                        :stylesheets ("http://localhost:8082/css/main.css"
                                      "http://localhost:8082/theme/main.css"
                                      "http://localhost:8082/vendor.css"))))))
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
                 "/a/scss/partials/_buttons.css"
                 '(:tag "reply" :reloaded nil
                        :stylesheets ("http://localhost:8082/css/main.css"))))))
    (should (string-match-p "_buttons\\.css" (car said)))
    (should (string-match-p "http://localhost:8082/css/main\\.css" (car said)))))

(ert-deftest replique-css-test-a-page-with-no-stylesheets-says-that-instead ()
  "Which is a different thing from a page that has some and matches none,
and it is nearly always the same mistake: the page connected to the repl
is not the page the application is in."
  (let ((said (replique-css-test--said
                (replique-css--report "/a/main.css"
                                      '(:tag "reply" :reloaded nil :stylesheets nil)))))
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
