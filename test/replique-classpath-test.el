;;; replique-classpath-test.el --- Tests for keeping the classpath up to date  -*- lexical-binding: t; -*-

;;; Commentary:

;; What is tested here is the editor's half: what it compares to decide
;; whether to ask, what it tells the process about how it would be started
;; now, the order the questions go in, and what each answer comes to - what
;; is asked about, what is added, what is offered a restart.
;;
;; NOTHING HERE RESOLVES ANYTHING.  What the deps tool makes of a deps.edn,
;; and what a running process can and cannot take, are the process's
;; questions and are tested in replique-2's deps-plan-test.  The process is
;; stubbed at `replique-process-request', and the npm packages are a project
;; written into a temporary directory.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'replique-test)
(require 'replique-classpath)
(require 'replique-reload-test)

(defun replique-classpath-test--write (file text)
  "Write TEXT into FILE, making the directories it is in."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun replique-classpath-test--process (directory)
  "A stand-in process running in DIRECTORY, its inputs read now."
  (replique-process--make :id "classpath-test" :directory directory
                          :inputs (replique-process-inputs directory)))

(defvar replique-classpath-test--sent nil
  "The ops sent to the process, oldest first.")

(cl-defmacro replique-classpath-test--run ((&key status plan sync yes restarted) &rest body)
  "Run BODY with the process answering STATUS, PLAN and SYNC.

Each is the frame its op is answered with: `:classpath-status',
`:classpath-plan' and `:sync-classpath'.  YES is what every question is
answered with.  RESTARTED, when given, is a variable `replique-restart'
puts the process it was asked to restart in."
  (declare (indent 1))
  `(progn
     (setq replique-classpath-test--sent nil)
     (cl-letf (((symbol-function 'replique-classpath--soon)
                (lambda (function) (funcall function)))
               ((symbol-function 'y-or-n-p) (lambda (_prompt) ,yes))
               ((symbol-function 'display-warning) #'ignore)
               ((symbol-function 'replique-restart)
                (lambda (process)
                  ,(if restarted `(setq ,restarted process) '(ignore process))))
               ((symbol-function 'replique-process-request)
                (lambda (_process msg &optional callback)
                  (setq replique-classpath-test--sent
                        (append replique-classpath-test--sent (list msg)))
                  (when callback
                    (funcall callback
                             (pcase (plist-get msg :op)
                               (:classpath-status (or ,status '(:tag "reply")))
                               (:update-classpath '(:tag "reply" :namespaces 1 :classes 1))
                               (:classpath-plan (or ,plan '(:tag "reply" :verdict "current")))
                               (:sync-classpath (or ,sync '(:tag "reply" :added ("my/lib"))))))))))
       ,@body)))

(defun replique-classpath-test--ops ()
  "The ops sent, in order."
  (mapcar (lambda (msg) (plist-get msg :op)) replique-classpath-test--sent))

(defun replique-classpath-test--act (process report)
  "What `replique-classpath-act' says about REPORT, as (SENTENCE GO-ON)."
  (let (result)
    (replique-classpath-act process report
                            (lambda (sentence go-on) (setq result (list sentence go-on))))
    result))

;;; What changed on this side

(ert-deftest replique-classpath-test-nothing-changed-is-nothing-to-ask ()
  (replique-test-with-project dir
    (replique-classpath-test--write (expand-file-name "deps.edn" dir) "{:paths [\"src\"]}")
    (let ((process (replique-classpath-test--process dir)))
      (should-not (replique-classpath--changed process))
      ;; Written again and saying the same thing, which is what a worktree
      ;; pointed at another one is: every file a new date, and a classpath
      ;; that does not change with it
      (replique-classpath-test--write (expand-file-name "deps.edn" dir) "{:paths [\"src\"]}")
      (should-not (replique-classpath--changed process)))))

(ert-deftest replique-classpath-test-a-file-that-says-something-else-is-a-change ()
  (replique-test-with-project dir
    (let ((deps (expand-file-name "deps.edn" dir)))
      (replique-classpath-test--write deps "{:paths [\"src\"]}")
      (let ((process (replique-classpath-test--process dir)))
        (replique-classpath-test--write deps "{:paths [\"src\" \"dev\"]}")
        (should (equal (list deps) (plist-get (replique-classpath--changed process) :files))))
      ;; and so is one that appeared, which is the aliases file the day it is written
      (progn
        (let ((process (replique-classpath-test--process dir))
              (aliases (expand-file-name replique-aliases-file dir)))
          (replique-test-write-aliases dir "{:mine {}}")
          (should (equal (list aliases)
                         (plist-get (replique-classpath--changed process) :files))))))))

(ert-deftest replique-classpath-test-a-process-nothing-is-known-about-says-nothing ()
  (should-not (replique-classpath--changed (replique-process--make :directory "/p/")))
  (should-not (replique-classpath--configuration (replique-process--make :directory "/p/"))))

(ert-deftest replique-classpath-test-other-aliases-are-told-to-the-process ()
  "WHAT IT WAS STARTED WITH IS NOT SAID, because for a process this Emacs
only connected to that is not known - what is known is that it has not
changed since.  What has is said, whole, so that the process can compute
the classpath it would be started with now."
  (replique-test-with-project dir
    (let* ((replique-aliases '("dev"))
           (process (replique-classpath-test--process dir)))
      (should-not (replique-classpath--configuration process))
      (let ((replique-aliases '("dev" "test")))
        (should (equal '(:aliases ["dev" "test"] :extra "")
                       (replique-classpath--configuration process)))
        (should (plist-get (replique-classpath--changed process) :launch))
        ;; and once seen it is no longer a change, and is still what is told
        (progn
          (replique-classpath--seen process)
          (should-not (replique-classpath--changed process))
          (should (equal '(:aliases ["dev" "test"] :extra "")
                         (replique-classpath--configuration process))))))))

(ert-deftest replique-classpath-test-the-aliases-file-is-told-as-the-deps-of-sdeps ()
  (replique-test-with-project dir
    (let ((replique-coordinates nil)
          (process (replique-classpath-test--process dir)))
      (replique-test-write-aliases dir "{:mine {:extra-paths [\"x\"]}}")
      (should (equal "{:aliases {:mine {:extra-paths [\"x\"]}}\n}"
                     (plist-get (replique-classpath--configuration process) :extra))))))

;;; Asking

(ert-deftest replique-classpath-test-a-reading-due-is-done-before-the-plan ()
  "The reading is what the plan compares against."
  (replique-test-with-project dir
    (let ((process (replique-classpath-test--process dir))
          (report nil))
      (replique-classpath-test--run (:status '(:tag "reply" :reading-due t))
        (replique-classpath-check process (lambda (r) (setq report r)) t t))
      (should (equal '(:classpath-status :update-classpath :classpath-plan)
                     (replique-classpath-test--ops)))
      (should (plist-get report :rescanned)))))

(ert-deftest replique-classpath-test-asked-without-acting-changes-nothing ()
  (replique-test-with-project dir
    (let ((process (replique-classpath-test--process dir))
          (report nil))
      (replique-classpath-test--run (:status '(:tag "reply" :reading-due t))
        (replique-classpath-check process (lambda (r) (setq report r))))
      (should (equal '(:classpath-status) (replique-classpath-test--ops)))
      (should (plist-get report :reading-due))
      (should-not (plist-get report :rescanned)))))

(ert-deftest replique-classpath-test-the-deps-tool-is-asked-only-where-a-file-changed ()
  (replique-test-with-project dir
    (let ((deps (expand-file-name "deps.edn" dir)))
      (replique-classpath-test--write deps "{}")
      (let ((process (replique-classpath-test--process dir)))
        (replique-classpath-test--run ()
          (replique-classpath-check process #'ignore nil t))
        (should (equal '(:classpath-status) (replique-classpath-test--ops)))
        (replique-classpath-test--write deps "{:deps {my/lib {:mvn/version \"1\"}}}")
        (replique-classpath-test--run ()
          (replique-classpath-check process #'ignore nil t))
        (should (equal '(:classpath-status :classpath-plan) (replique-classpath-test--ops)))))))

;;; What it comes to

(ert-deftest replique-classpath-test-what-can-be-added-is-asked-about-and-added ()
  (replique-test-with-project dir
    (let* ((process (replique-classpath-test--process dir))
           (report '(:plan (:tag "reply" :verdict "additive"
                                 :added-libs ((:lib "my/lib" :now "1.0.0"))))))
      (replique-classpath-test--run (:yes t)
        (let ((said (replique-classpath-test--act process report)))
          (should (equal '(:sync-classpath) (replique-classpath-test--ops)))
          (should (equal "added my/lib to the classpath" (car said)))
          (should (cadr said))))
      ;; and not added where the answer is no, which is said
      (progn
        (replique-classpath-test--run (:yes nil)
          (let ((said (replique-classpath-test--act process report)))
            (should (null (replique-classpath-test--ops)))
            (should (string-match-p "my/lib 1.0.0 is not on the classpath yet"
                                    (car said)))))))))

(ert-deftest replique-classpath-test-what-cannot-be-added-offers-a-restart ()
  (let* ((process (replique-process--make :id "p" :directory "/p/"))
         (report '(:plan (:tag "reply" :verdict "restart"
                               :moved ((:lib "my/lib" :was "1" :now "2")))))
         (restarted nil))
    (replique-classpath-test--run (:yes t :restarted restarted)
      (let ((said (replique-classpath-test--act process report)))
        (should (eq process restarted))
        (should (equal "restarting p" (car said)))
        (should-not (cadr said))))
    ;; declined, it is said and changes nothing
    (progn
      (setq restarted nil)
      (replique-classpath-test--run (:yes nil :restarted restarted)
        (let ((said (replique-classpath-test--act process report)))
          (should-not restarted)
          (should (equal "p cannot follow its classpath (my/lib 2 instead of 1) - M-x replique-restart"
                           (car said)))
          (should (cadr said)))))))

(ert-deftest replique-classpath-test-a-directory-that-moved-is-a-restart-without-a-plan ()
  "It costs no resolving to find, so it is found every time."
  (let ((process (replique-process--make :id "p" :directory "/p/"))
        (restarted nil))
    (replique-classpath-test--run (:yes t :restarted restarted)
      (replique-classpath-test--act
       process '(:frozen ((:entry "/stage/src" :was "/a/src" :now "/b/src")))))
    (should (eq process restarted))))

(ert-deftest replique-classpath-test-what-was-taken-out-is-said-once ()
  (replique-test-with-project dir
    (let ((process (replique-classpath-test--process dir))
          (deps (expand-file-name "deps.edn" dir)))
      (replique-classpath-test--write deps "{}")
      (replique-classpath-test--run ()
        (let ((said (replique-classpath-test--act
                     process '(:plan (:tag "reply" :verdict "current"
                                           :removed-libs ("my/lib"))))))
          (should (equal "my/lib left deps.edn and stays loaded until a restart"
                         (car said)))
          (should (cadr said))))
      (should-not (replique-classpath--changed process)))))

(ert-deftest replique-classpath-test-deps-files-the-tool-refused-are-said ()
  (let ((process (replique-process--make :id "p" :directory "/p/")))
    (replique-classpath-test--run ()
      (should (string-match-p
               "could not be resolved: Error building classpath"
               (car (replique-classpath-test--act
                     process '(:plan (:tag "error" :error "exception"
                                           :message "Error building classpath")))))))
    ;; and a process that cannot be asked is not news
    (progn
      (replique-classpath-test--run ()
        (should-not (car (replique-classpath-test--act
                          process '(:plan (:tag "error" :error "unknown-op"))))))
      (replique-classpath-test--run ()
        (should-not (car (replique-classpath-test--act
                          process '(:plan (:tag "error" :error "no-basis")))))))))

;;; The npm packages

(defun replique-classpath-test--package (root name version)
  "Install NAME at VERSION under ROOT's node_modules."
  (replique-classpath-test--write
   (expand-file-name (concat "node_modules/" name "/package.json") root)
   (format "{\"name\": \"%s\", \"version\": \"%s\"}" name version)))

(ert-deftest replique-classpath-test-node-modules-is-held-up-against-the-lock ()
  (replique-test-with-project root
    (replique-classpath-test--write
     (expand-file-name "package.json" root)
     (concat "{\"dependencies\": {\"same\": \"^1.0.0\", \"older\": \"^2.0.0\","
             " \"missing\": \"^1.0.0\", \"unlocked\": \"^1.0.0\","
             " \"local\": \"file:js/local\"},"
             " \"devDependencies\": {\"@scope/dev\": \"~3.0.0\"}}"))
    (replique-classpath-test--write
     (expand-file-name "package-lock.json" root)
     (concat "{\"lockfileVersion\": 3, \"packages\": {"
             "\"node_modules/same\": {\"version\": \"1.2.0\"},"
             "\"node_modules/older\": {\"version\": \"2.1.0\"},"
             "\"node_modules/missing\": {\"version\": \"1.0.0\"},"
             "\"node_modules/local\": {\"version\": \"0.0.1\"},"
             "\"node_modules/@scope/dev\": {\"version\": \"3.0.1\"}}}"))
    (replique-classpath-test--package root "same" "1.2.0")
    (replique-classpath-test--package root "older" "2.0.0")
    (replique-classpath-test--package root "unlocked" "1.0.0")
    (replique-classpath-test--package root "local" "0.0.9")
    (replique-classpath-test--package root "@scope/dev" "3.0.1")
    (should (equal '("missing is not installed"
                     "older 2.0.0 is installed, package-lock.json has 2.1.0"
                     "unlocked is in package.json and not in package-lock.json")
                   (replique-classpath-npm-problems
                    (file-name-as-directory (expand-file-name "src/app" root)))))))

(ert-deftest replique-classpath-test-a-project-without-node-modules-says-nothing ()
  (replique-test-with-project root
    (replique-classpath-test--write (expand-file-name "package.json" root)
                                    "{\"dependencies\": {\"a\": \"1\"}}")
    (should-not (replique-classpath-npm-problems root))))

;;; On save

(ert-deftest replique-classpath-test-saving-a-deps-file-asks-about-its-process ()
  (replique-test-with-project dir
    (let* ((deps (expand-file-name "deps.edn" dir))
           (_ (replique-classpath-test--write deps "{}"))
           (process (replique-classpath-test--process dir))
           (asked nil))
      (cl-letf (((symbol-function 'replique-processes-live) (lambda () (list process)))
                ((symbol-function 'replique-classpath-check)
                 (lambda (p &rest _) (push p asked))))
        (with-temp-buffer
          (setq buffer-file-name deps)
          (replique-classpath--after-save))
        (should (equal (list process) asked))
        (setq asked nil)
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "src/a.clj" dir))
          (replique-classpath--after-save))
        (should-not asked)))))

;;; In the reload

(ert-deftest replique-classpath-test-the-reload-adds-first-and-says-so-first ()
  "A library added is the first thing the sentence says, because it happened
first and what was loaded after it was loaded against it."
  (replique-reload-test--with-repls ((clj nil nil))
    (ignore clj)
    (cl-letf (((symbol-function 'replique-classpath-check)
               (lambda (_process callback &rest _)
                 (funcall callback '(:plan (:tag "reply" :verdict "additive"
                                                 :added-libs ((:lib "my/lib" :now "1")))))))
              ((symbol-function 'replique-classpath--sync)
               (lambda (_process done) (funcall done "added my/lib to the classpath")))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (replique-reload-test--run ()
        (replique-reload-app)))
    (should (equal '("Clojure") replique-reload-test--asked))
    (should (string-prefix-p "replique: added my/lib to the classpath - loaded Clojure"
                             (replique-reload-test--sentence)))))

(ert-deftest replique-classpath-test-a-restart-instead-reloads-nothing ()
  (replique-reload-test--with-repls ((clj nil nil))
    (ignore clj)
    (cl-letf (((symbol-function 'replique-classpath-check)
               (lambda (_process callback &rest _)
                 (funcall callback '(:frozen ((:entry "/stage/src"))))))
              ((symbol-function 'replique-restart) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (replique-reload-test--run ()
        (replique-reload-app)))
    (should (null replique-reload-test--asked))
    (should (equal "replique: restarting reload-test" (replique-reload-test--sentence)))))

;;; Against a process really answering

(ert-deftest replique-classpath-test-a-library-added-to-deps-edn-reaches-the-process ()
  "The whole of it: a process is started, a library is added to its
deps.edn, and the process can require what that library provides - asked
about first, and without a restart."
  (replique-test-with-project dir
    (let ((lib (file-name-as-directory (make-temp-file "replique-test-lib" t))))
      (unwind-protect
          (progn
            (replique-classpath-test--write (expand-file-name "deps.edn" lib)
                                            "{:paths [\"src\"]}")
            (replique-classpath-test--write (expand-file-name "src/added/thing.clj" lib)
                                            "(ns added.thing)\n(def x 42)\n")
            (replique-classpath-test--write (expand-file-name "deps.edn" dir)
                                            "{:paths [\"src\"]}")
            (let* ((replique-coordinates (format "{:local/root %S}" (replique-test-project)))
                   (process (replique-test-started-in dir))
                   (said nil)
                   (asked nil))
              (unwind-protect
                  (progn
                    (replique-classpath-test--write
                     (expand-file-name "deps.edn" dir)
                     (format "{:paths [\"src\"] :deps {my/added {:local/root %S}}}" lib))
                    (cl-letf (((symbol-function 'y-or-n-p)
                               (lambda (prompt) (setq asked prompt) t))
                              ((symbol-function 'message)
                               (lambda (format &rest args)
                                 (setq said (apply #'format format args)))))
                      (replique-sync-classpath process)
                      (should (replique-test-wait-for
                               (lambda () (and said (string-match-p "added\\|could not" said)))
                               120)))
                    (should (string-match-p "Add my/added" asked))
                    (should (equal "replique: added my/added to the classpath" said))
                    (let ((answer (replique-process-request-sync
                                   process '(:op :completions :position :namespace
                                                 :text "added.thing"))))
                      (should (seq-some (lambda (candidate)
                                          (equal "added.thing" (plist-get candidate :candidate)))
                                        (plist-get answer :completions))))
                    ;; Seen, so the next reload has nothing to ask
                    (should-not (replique-classpath--changed process)))
                (replique-kill-process process))))
        (delete-directory lib t)))))

(provide 'replique-classpath-test)

;;; replique-classpath-test.el ends here
