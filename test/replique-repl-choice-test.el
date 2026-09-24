;;; replique-repl-choice-test.el --- Tests for which repl a buffer's code goes to  -*- lexical-binding: t; -*-

;;; Commentary:

;; A .clj file is evaluated in a Clojure repl and a .cljs file in a
;; ClojureScript one, whichever repl the commands were last pointed at.  A
;; .cljc file is evaluated in whichever that is, because it is a file of both
;; worlds and nothing in it chooses.
;;
;; What that needs is repls, and what a repl has to have for these is a live
;; connection - `replique-repl-live-p' filters on it - the handshake reply it
;; carries, which is where the dialect and the target are read from, and a
;; buffer, because choosing a repl is choosing a buffer name.  A `cat'
;; standing in for the network process is enough for the first: nothing here
;; writes to one.
;;
;; The process needs one too.  `replique-process-current' answers the process
;; the commands act on and will not answer one whose control connection has
;; gone, so the stand-in process carries a stand-in control connection.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-repl)
(require 'replique-eval)

(defvar replique-repl-choice-test--procs nil
  "The operating system processes standing in for connections.")

(defvar replique-repl-choice-test--buffers nil
  "The buffers standing in for repl buffers.")

(defun replique-repl-choice-test--conn (&optional dialect target)
  "Return a live connection whose handshake reply said DIALECT and TARGET.

Both are strings, the way they arrive over the wire: a reply is JSON and
the process writes the dialect and the target as the names of the
keywords it read.  Nil for either is a reply that did not carry it, which
is what a Clojure repl gets - absent means Clojure, and a Clojure repl has
no target at all."
  (let ((proc (start-process "replique-repl-choice-test" nil "cat")))
    (push proc replique-repl-choice-test--procs)
    (replique-conn--make
     :proc proc :kind 'repl :id "c1"
     :info (append (list :tag "reply" :op "hello" :role "repl" :connection "c1")
                   (when dialect (list :dialect dialect))
                   (when target (list :target target))))))

(defun replique-repl-choice-test--process (id)
  "Return a stand-in process named ID, live enough to be the current one."
  (replique-process--make :id id :host "127.0.0.1" :port 1
                          :directory "/tmp/"
                          :control (replique-repl-choice-test--conn)))

(defun replique-repl-choice-test--repl (process &optional dialect target)
  "Return a repl of PROCESS of DIALECT and TARGET, and put it on PROCESS.

Pushed, which is what opening one does, so the last one made is the most
recent."
  (let* ((buffer (generate-new-buffer
                  (replique-repl--name process
                                       (and dialect :cljs)
                                       (and target (intern (concat ":" target))))))
         (repl (replique-repl--make :process process
                                    :buffer buffer
                                    :given-name (buffer-name buffer)
                                    :conn (replique-repl-choice-test--conn dialect target)
                                    :to-echo 0)))
    (push buffer replique-repl-choice-test--buffers)
    (push repl (replique-process--repls process))
    repl))

(defmacro replique-repl-choice-test--with-repls (spec &rest body)
  "Run BODY with one process holding the repls SPEC names.

SPEC is a list of (VAR DIALECT TARGET), oldest first: they are opened in
the order written, so the LAST one named is the most recent of its
dialect.  Nothing is chosen - `replique-current-repl' starts nil, and a
test that is about the choice makes it itself."
  (declare (indent 1))
  `(let* ((replique-repl-choice-test--procs nil)
          (replique-repl-choice-test--buffers nil)
          (process (replique-repl-choice-test--process "choice-test"))
          (replique-processes (list process))
          (replique-current-process process)
          (replique-current-repl nil)
          ,@(mapcar (lambda (s)
                      `(,(nth 0 s) (replique-repl-choice-test--repl
                                    process ,(nth 1 s) ,(nth 2 s))))
                    spec))
     (ignore process)
     (unwind-protect (progn ,@body)
       (dolist (proc replique-repl-choice-test--procs)
         (when (process-live-p proc) (delete-process proc)))
       (dolist (buffer replique-repl-choice-test--buffers)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun replique-repl-choice-test--choose (repl)
  "Choose REPL the way `replique-select-repl' does, by its buffer name."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) (buffer-name (replique-repl--buffer repl)))))
    (replique-select-repl)))

(defmacro replique-repl-choice-test--in-mode (mode &rest body)
  "Run BODY in a temporary buffer in MODE."
  (declare (indent 1))
  `(with-temp-buffer (funcall ,mode) ,@body))

;;; Where a buffer's code goes

(ert-deftest replique-repl-choice-test-a-clojure-file-goes-to-a-clojure-repl ()
  "Though a ClojureScript repl is the one chosen.  A .clj file handed to
the ClojureScript compiler is a file that compiler was never going to be
able to read, and which repl was last selected is no reason to try: what
the file is, is written in its name."
  (replique-repl-choice-test--with-repls ((clj nil nil) (cljs "cljs" "browser"))
    (setq replique-current-repl cljs)
    (replique-repl-choice-test--in-mode #'replique-clojure-mode
      (should (eq clj (replique-repl-for-dialect :clj)))
      (should (eq clj (replique-repl-ensure-here))))))

(ert-deftest replique-repl-choice-test-a-clojurescript-file-goes-to-a-clojurescript-repl ()
  "And the same read the other way, which is the case that bites more
often: a Clojure repl is what a session starts with, so it is what a .cljs
file would go to if the selection decided."
  (replique-repl-choice-test--with-repls ((clj nil nil) (cljs "cljs" "browser"))
    (setq replique-current-repl clj)
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurescript-mode
      (should (eq cljs (replique-repl-ensure-here))))))

(ert-deftest replique-repl-choice-test-a-cljc-file-goes-to-whichever-is-chosen ()
  "A .cljc namespace really is a namespace of both worlds and nothing in
the file chooses, so the selection is what is left to choose with - and it
chooses, rather than being overruled by a default."
  (replique-repl-choice-test--with-repls ((clj nil nil) (cljs "cljs" "browser"))
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurec-mode
      (setq replique-current-repl cljs)
      (should (eq cljs (replique-repl-ensure-here)))
      (setq replique-current-repl clj)
      (should (eq clj (replique-repl-ensure-here))))))

(ert-deftest replique-repl-choice-test-a-repl-buffer-acts-on-itself ()
  "Whatever it is a repl of, and whatever is chosen elsewhere.  A repl
buffer is not a file: there is no extension to read it against, and the
repl it is the buffer of is exactly what it is about."
  (replique-repl-choice-test--with-repls ((clj nil nil)
                                          (browser "cljs" "browser")
                                          (node "cljs" "node"))
    (ignore node)
    (setq replique-current-repl clj)
    (with-temp-buffer
      ;; The older of the two ClojureScript repls, so that answering out of
      ;; the process's list - the newer one - would be a different repl and
      ;; not the same answer reached another way
      (setq-local replique--buffer-repl browser)
      (should (eq browser (replique-repl-ensure-here))))))

(ert-deftest replique-repl-choice-test-the-most-recent-of-each-dialect-is-the-one ()
  "Two ClojureScript repls, and the newer is what a .cljs file goes to -
while the Clojure repl a .clj file goes to is untouched by either of them.
The two questions have different answers and only one of them was asked."
  (replique-repl-choice-test--with-repls ((clj nil nil)
                                          (browser "cljs" "browser")
                                          (node "cljs" "node"))
    (ignore browser)
    (setq replique-current-repl clj)
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurescript-mode
      (should (eq node (replique-repl-ensure-here))))
    (replique-repl-choice-test--in-mode #'replique-clojure-mode
      (should (eq clj (replique-repl-ensure-here))))))

(ert-deftest replique-repl-choice-test-choosing-a-repl-outlives-the-next-choice ()
  "`replique-current-repl' holds ONE repl, so choosing a Clojure repl is
where a ClojureScript one chosen before it would be forgotten - and a
.cljs file would go back to whichever was opened last rather than to the
one that was picked.  Choosing reorders the process's list for exactly
this, which is what replique 1 does in `replique/switch-active-repl'."
  (replique-repl-choice-test--with-repls ((clj nil nil)
                                          (browser "cljs" "browser")
                                          (node "cljs" "node"))
    ;; Opened last, so this is where a .cljs file goes until somebody says
    ;; otherwise
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurescript-mode
      (should (eq node (replique-repl-ensure-here))))
    (replique-repl-choice-test--choose browser)
    (should (eq browser replique-current-repl))
    ;; And now the Clojure repl, which is what takes the ClojureScript one
    ;; out of `replique-current-repl'
    (replique-repl-choice-test--choose clj)
    (should (eq clj replique-current-repl))
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurescript-mode
      (should (eq browser (replique-repl-ensure-here))))
    (replique-repl-choice-test--in-mode #'replique-clojure-mode
      (should (eq clj (replique-repl-ensure-here))))))

(ert-deftest replique-repl-choice-test-nothing-of-that-dialect-is-said-as-the-buffer-would ()
  "The message names what the buffer needed and not what the lookup asked
for.  A .cljc buffer with nothing open would have been looked up as
Clojure - `replique-dialect' resolves it to the chosen repl, and there is
none - and saying that no Clojure repl is open would be naming one of the
two answers that would have done."
  (replique-repl-choice-test--with-repls ((cljs "cljs" "browser"))
    (setq replique-current-repl cljs)
    (replique-repl-choice-test--in-mode #'replique-clojure-mode
      (should-not (replique-repl-for-dialect :clj))
      (let ((err (should-error (replique-repl-ensure-here) :type 'user-error)))
        (should (string-match-p "No Clojure repl" (cadr err)))))
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurec-mode
      (should (eq cljs (replique-repl-ensure-here)))))
  (replique-repl-choice-test--with-repls ()
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurec-mode
      (let ((err (should-error (replique-repl-ensure-here) :type 'user-error)))
        (should (string-match-p "\\`No repl" (cadr err)))))
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurescript-mode
      (let ((err (should-error (replique-repl-ensure-here) :type 'user-error)))
        (should (string-match-p "No ClojureScript repl" (cadr err)))))))

(ert-deftest replique-repl-choice-test-the-target-asked-about-is-the-one-running ()
  "A .cljs buffer read from beside a Clojure repl is still a question about
the ClojureScript that is running.  A Clojure repl carries no target, so
reading the target off whatever repl is in hand sent the question out
about the process's default - the browser build - while the program open
in the next window was a node one."
  (replique-repl-choice-test--with-repls ((clj nil nil) (node "cljs" "node"))
    (ignore node)
    (setq replique-current-repl clj)
    (replique-repl-choice-test--in-mode #'replique-clojure-clojurescript-mode
      (should (equal '(:dialect :cljs :target :node) (replique-dialect-keys))))))

(ert-deftest replique-repl-choice-test-evaluating-a-form-goes-by-the-buffer ()
  "The commands and not only the lookup.  What decides where a form goes is
of no use unless what sends the form asks it - and a test that only calls
the lookup would pass while every key still sent its form to whatever was
chosen."
  (replique-repl-choice-test--with-repls ((clj nil nil) (cljs "cljs" "browser"))
    (setq replique-current-repl clj)
    (let ((sent-to nil))
      (cl-letf (((symbol-function 'replique-repl-send-code)
                 (lambda (repl &rest _) (setq sent-to repl))))
        (replique-repl-choice-test--in-mode #'replique-clojure-clojurescript-mode
          (insert "(+ 1 2)")
          (replique-eval-last-sexp))
        (should (eq cljs sent-to))
        (setq sent-to nil)
        (replique-repl-choice-test--in-mode #'replique-clojure-mode
          (insert "(+ 1 2)")
          (replique-eval-last-sexp))
        (should (eq clj sent-to))))))

;;; What a repl buffer is called

(ert-deftest replique-repl-choice-test-a-repl-is-named-after-what-it-is ()
  "Two repls of one process differ in the only way repls of one process
can, and a list of buffers distinguished by nothing but <2> is a list that
makes somebody open both to find out which is which."
  (let ((process (replique-repl-choice-test--process "myproj")))
    (unwind-protect
        (progn
          (should (equal "*replique: myproj clj*"
                         (replique-repl--name process nil nil)))
          (should (equal "*replique: myproj clj*"
                         (replique-repl--name process :clj nil)))
          (should (equal "*replique: myproj cljs:browser*"
                         (replique-repl--name process :cljs :browser)))
          (should (equal "*replique: myproj cljs:node*"
                         (replique-repl--name process :cljs :node)))
          ;; Asked for before the reply says which target it got
          (should (equal "*replique: myproj cljs*"
                         (replique-repl--name process :cljs nil))))
      (dolist (proc replique-repl-choice-test--procs)
        (when (process-live-p proc) (delete-process proc)))
      (setq replique-repl-choice-test--procs nil))))

(ert-deftest replique-repl-choice-test-a-repl-is-renamed-after-what-it-turned-out-to-be ()
  "A repl opened with no target gets the process's default, and which one
that is belongs to the process: the buffer is named from what was asked
for, because it exists before there is a reply to read, and named again
from the reply."
  (let* ((process (replique-repl-choice-test--process "myproj"))
         (name (replique-repl--buffer-name process :cljs nil))
         (buffer (generate-new-buffer name))
         (repl (replique-repl--make
                :process process :buffer buffer :given-name name
                :conn (replique-repl-choice-test--conn "cljs" "browser"))))
    (unwind-protect
        (progn
          (should (equal "*replique: myproj cljs*" (buffer-name buffer)))
          (replique-repl--rename repl)
          (should (equal "*replique: myproj cljs:browser*" (buffer-name buffer)))
          (should (equal "*replique: myproj cljs:browser*"
                         (replique-repl--given-name repl))))
      (kill-buffer buffer)
      (dolist (proc replique-repl-choice-test--procs)
        (when (process-live-p proc) (delete-process proc)))
      (setq replique-repl-choice-test--procs nil))))

(ert-deftest replique-repl-choice-test-a-repl-that-is-what-it-asked-to-be-keeps-its-name ()
  "The name is compared before it is made unique.  A Clojure repl asks for
Clojure and gets it, so by the time the reply is in, the buffer already
holds the name the rename wants - and asking whether that name is taken is
being told yes by this very buffer, which renamed the first repl of a
process to <2> for having been right all along."
  (let* ((process (replique-repl-choice-test--process "myproj"))
         (name (replique-repl--buffer-name process nil nil))
         (buffer (generate-new-buffer name))
         (repl (replique-repl--make
                :process process :buffer buffer :given-name name
                :conn (replique-repl-choice-test--conn))))
    (unwind-protect
        (progn
          (should (equal "*replique: myproj clj*" (buffer-name buffer)))
          (replique-repl--rename repl)
          (should (equal "*replique: myproj clj*" (buffer-name buffer)))
          ;; And again, because nothing here happens only once
          (replique-repl--rename repl)
          (should (equal "*replique: myproj clj*" (buffer-name buffer))))
      (kill-buffer buffer)
      (dolist (proc replique-repl-choice-test--procs)
        (when (process-live-p proc) (delete-process proc)))
      (setq replique-repl-choice-test--procs nil))))

(ert-deftest replique-repl-choice-test-a-second-repl-of-a-kind-is-the-one-that-moves ()
  "Two Clojure repls of one process do need telling apart, and the one that
carries the suffix is the one that arrived second - not the one that was
there first."
  (let* ((process (replique-repl-choice-test--process "myproj"))
         (first-name (replique-repl--buffer-name process nil nil))
         (first-buffer (generate-new-buffer first-name))
         (second-name (replique-repl--buffer-name process :cljs nil))
         (second-buffer (generate-new-buffer second-name))
         (second (replique-repl--make
                  :process process :buffer second-buffer :given-name second-name
                  ;; Asked for ClojureScript, and the process had no
                  ;; compiler to give it one - so the reply says Clojure
                  :conn (replique-repl-choice-test--conn))))
    (unwind-protect
        (progn
          (replique-repl--rename second)
          (should (equal "*replique: myproj clj*" (buffer-name first-buffer)))
          (should (equal "*replique: myproj clj*<2>" (buffer-name second-buffer)))
          (should (equal "*replique: myproj clj*<2>"
                         (replique-repl--given-name second))))
      (kill-buffer first-buffer)
      (kill-buffer second-buffer)
      (dolist (proc replique-repl-choice-test--procs)
        (when (process-live-p proc) (delete-process proc)))
      (setq replique-repl-choice-test--procs nil))))

(ert-deftest replique-repl-choice-test-a-buffer-somebody-renamed-is-left-alone ()
  "This renames the name it gave, and a name it did not give is somebody's
doing and not ours to undo."
  (let* ((process (replique-repl-choice-test--process "myproj"))
         (name (replique-repl--buffer-name process :cljs nil))
         (buffer (generate-new-buffer name))
         (repl (replique-repl--make
                :process process :buffer buffer :given-name name
                :conn (replique-repl-choice-test--conn "cljs" "node"))))
    (unwind-protect
        (progn
          (with-current-buffer buffer (rename-buffer "the one I am using"))
          (replique-repl--rename repl)
          (should (equal "the one I am using" (buffer-name buffer))))
      (kill-buffer buffer)
      (dolist (proc replique-repl-choice-test--procs)
        (when (process-live-p proc) (delete-process proc)))
      (setq replique-repl-choice-test--procs nil))))

(provide 'replique-repl-choice-test)

;;; replique-repl-choice-test.el ends here
