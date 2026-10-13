;;; replique-trace.el --- The calls of chosen functions, as a tree  -*- lexical-binding: t; -*-

;; Copyright © 2016 Ewen Grosjean

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A function traced says each time it is called: what it was called
;; with, what it returned or threw, how long it took, and the calls it
;; made - a request handled, a test run, a button clicked, as a tree of
;; calls.  Which functions is what `replique-trace' asks:
;;
;;   f  the function at point
;;   a  it, with the functions that call it and those it calls - as the
;;      compiler recorded them, so the files have to have been loaded
;;   n  every function of the namespace
;;   d  the functions a branch changed: those whose definitions a
;;      `git diff' against a revision touches, the work tree included
;;
;; Nothing is evaluated again: a function is traced by its var, and let go
;; of as quickly.  One defined again while it is traced is traced still.
;; A ClojureScript function is traced in the runtime of the repl, and a
;; .cljc one in both.
;;
;; The calls are kept from the first one on, up to a limit: clear them
;; (c in the buffer of the calls) and do what is to be looked at, and the
;; tree is that and nothing else.
;;
;;   C-c C-d T  `replique-trace'           C-c C-d c  `replique-trace-calls'
;;
;; In the buffer of the calls:
;;
;;   TAB  the calls the call at point made     RET  its function's code
;;   v    what it returned, or threw           a    what it was called with
;;   r    record its function                  R    call it again, with its args
;;   c    forget the calls kept                u    trace nothing any more
;;   t    trace more                           g    ask again
;;
;; C-u u traces nothing any more, forgets the calls and closes the buffer.
;;
;; A call to look into is recorded and made again: r evaluates its
;; function again to record what each of its forms evaluates to, written
;; in its code - see `replique-debug-record' - and R calls it again in the
;; repl, with the values it was called with.  The code edited, R again is
;; the same call through the new code.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'text-property-search)
(require 'replique-common)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-parse)
(require 'replique-eval)
(require 'replique-inspect)
(require 'replique-debug)
(require 'replique-fresh)

(defcustom replique-trace-depth 1
  "How many calls around a function `replique-trace' traces with it, by default."
  :type 'natnum
  :group 'replique)

(defface replique-trace-thrown
  '((t :inherit error))
  "Face for what a traced call threw."
  :group 'replique)

(defface replique-trace-about
  '((t :inherit shadow))
  "Face for what is said of a traced call after it: its time, its thread."
  :group 'replique)

;;; Which functions

(defconst replique-trace--not-functions
  '("defmacro" "defmulti" "defprotocol" "defrecord" "deftype" "definterface"
    "defstruct" "defonce")
  "The definitions that are not of a function a var holds, to trace.")

(defun replique-trace--defined (node)
  "Return the name NODE, a top level form, defines a function as - or nil."
  (when (eq 'list (replique-parse-type node))
    (let* ((forms (replique-parse-forms node))
           (op (car forms))
           (head (and op (eq 'symbol (replique-parse-type op))
                      (replace-regexp-in-string "\\`.*/" "" (replique-parse-text op))))
           (named (and (cadr forms) (replique-parse-unwrap-meta (cadr forms)))))
      (and head
           (string-prefix-p "def" head)
           (not (member head replique-trace--not-functions))
           named
           (eq 'symbol (replique-parse-type named))
           (replique-parse-text named)))))

(defun replique-trace--ns-form (forms)
  "Return the name the first `ns' form of FORMS declares, or nil."
  (seq-some (lambda (node)
              (when (eq 'list (replique-parse-type node))
                (let ((parts (replique-parse-forms node)))
                  (when (and (cdr parts)
                             (equal "ns" (replique-parse-text (car parts))))
                    (replique-parse-text (replique-parse-unwrap-meta (cadr parts)))))))
            forms))

(defun replique-trace--at-point ()
  "Return the qualified name of the function defined around point."
  (let* ((node (or (replique-parse-form-at (point))
                   (user-error "No definition here")))
         (named (or (replique-trace--defined node)
                    (user-error "No function is defined here")))
         (ns (or (replique-eval--ns-at (replique-parse-start node))
                 (user-error "This buffer names no namespace"))))
    (format "%s/%s" ns named)))

(defun replique-trace--dialects (file)
  "Return the dialects whose functions FILE defines, by its extension."
  (pcase (file-name-extension file)
    ("clj" '(:clj))
    ("cljs" '(:cljs))
    ("cljc" '(:clj :cljs))))

(defun replique-trace--hunks (diff)
  "Return the lines DIFF, a `git diff --unified=0', touches in the new files.

As (FILE (START . END)...), FILE as the diff names it.  A hunk that only
takes lines away touches the line it took them after."
  (let ((files nil) (file nil))
    (dolist (line (split-string diff "\n"))
      (cond
       ((string-match "\\`\\+\\+\\+ b/\\(.*\\)" line)
        (setq file (match-string 1 line))
        (push (list file) files))
       ((string-match "\\`\\+\\+\\+ /dev/null" line)
        (setq file nil))
       ((and file (string-match "\\`@@ -[0-9,]+ \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line))
        (let* ((start (string-to-number (match-string 1 line)))
               (count (if (match-string 2 line) (string-to-number (match-string 2 line)) 1))
               (end (if (zerop count) start (+ start count -1))))
          (push (cons (max 1 start) (max 1 end)) (cdr (car files)))))))
    (mapcar (lambda (f) (cons (car f) (nreverse (cdr f)))) (nreverse files))))

(defun replique-trace--touched (file ranges)
  "Return the functions of FILE the line RANGES touch, qualified - or with
RANGES t, every one."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((forms (replique-parse-forms (replique-parse-buffer)))
           (ns (replique-trace--ns-form forms)))
      (when ns
        (delq nil
              (mapcar (lambda (node)
                        (when-let* ((named (replique-trace--defined node)))
                          (let ((from (line-number-at-pos (replique-parse-start node)))
                                (to (line-number-at-pos (replique-parse-end node))))
                            (when (or (eq t ranges)
                                      (seq-some (lambda (r) (and (<= (car r) to) (>= (cdr r) from)))
                                                ranges))
                              (format "%s/%s" ns named)))))
                      forms))))))

(defun replique-trace--git (root &rest args)
  "Return what git says to ARGS in ROOT, or signal what it said instead."
  (with-temp-buffer
    (let* ((default-directory root)
           (status (apply #'process-file "git" nil t nil args)))
      (unless (eql 0 status)
        (user-error "git %s: %s" (car args) (string-trim (buffer-string))))
      (buffer-string))))

(defun replique-trace--base (root)
  "Return the revision a branch in ROOT is likely to be compared with."
  (or (seq-some (lambda (upstream)
                  (ignore-errors
                    (string-trim (replique-trace--git root "merge-base" "HEAD" upstream))))
                '("origin/HEAD" "origin/master" "origin/main" "master" "main"))
      "HEAD"))

(defun replique-trace--changed (root base)
  "Return the functions changed in ROOT since BASE, by dialect: ((:clj . NAMES)
\(:cljs . NAMES)).  The work tree as it is, the files nobody added yet with it."
  (let* ((globs '("*.clj" "*.cljc" "*.cljs"))
         (hunks (replique-trace--hunks
                 (apply #'replique-trace--git root "diff" "--unified=0" "--no-color"
                        "--no-ext-diff" base "--" globs)))
         (added (split-string (apply #'replique-trace--git root "ls-files" "--others"
                                     "--exclude-standard" "--" globs)
                              "\n" t))
         (by-dialect (list (list :clj) (list :cljs))))
    (dolist (touched (append hunks (mapcar (lambda (f) (cons f t)) added)))
      (let ((file (expand-file-name (car touched) root)))
        (when (file-readable-p file)
          (let ((names (replique-trace--touched file (cdr touched))))
            (dolist (dialect (replique-trace--dialects file))
              (setcdr (assq dialect by-dialect)
                      (append (cdr (assq dialect by-dialect)) names)))))))
    by-dialect))

;;; Tracing

(defun replique-trace--cljs-keys ()
  "Return the dialect keys of the ClojureScript runtime the repl evaluates in."
  (append (list :dialect :cljs)
          (when-let* ((repl (replique-repl-for-dialect :cljs))
                      (target (replique-repl-target repl)))
            (list :target target))))

(defun replique-trace--keys (dialect)
  "Return the dialect keys of a question about DIALECT's functions."
  (when (eq :cljs dialect) (replique-trace--cljs-keys)))

(defun replique-trace--why-not (skipped)
  "Say why the vars SKIPPED were not traced, those of each reason together."
  (let ((by-why nil))
    (dolist (s skipped)
      (let* ((why (plist-get s :why))
             (cell (or (assoc why by-why) (car (push (list why) by-why)))))
        (setcdr cell (append (cdr cell) (list (plist-get s :var))))))
    (mapconcat (lambda (cell)
                 (let ((vars (cdr cell)))
                   (format "%s%s (%s): %s%s"
                           (car cell)
                           (if (equal "no such var" (car cell))
                               ", its file not loaded?" "")
                           (length vars)
                           (string-join (seq-take vars 3) ", ")
                           (if (nthcdr 3 vars) ", …" ""))))
               (nreverse by-why) "; ")))

(defun replique-trace--said (frame what)
  "Say what came of tracing WHAT, as FRAME answers it, and return FRAME."
  (if (equal "error" (plist-get frame :tag))
      (message "replique: %s" (plist-get frame :message))
    (let* ((skipped (plist-get frame :skipped))
           (asked (plist-get frame :asked))
           (callers (plist-get frame :callers))
           (callees (plist-get frame :callees)))
      (message "replique: %s%s: %s%s - %s"
               what
               (if (or callers callees)
                   (format " with %s caller%s and %s callee%s"
                           (length callers) (if (= 1 (length callers)) "" "s")
                           (length callees) (if (= 1 (length callees)) "" "s"))
                 "")
               (if asked (format "%s traced" (- asked (length skipped))) "traced")
               (if skipped
                   (format ", %s not - %s" (length skipped)
                           (replique-trace--why-not skipped))
                 "")
               (substitute-command-keys "\\[replique-trace-calls] shows the calls"))))
  frame)

(defun replique-trace--send (process keys msg what)
  "Ask PROCESS to trace, as MSG says, in the dialect of KEYS; show the calls."
  (replique-process-request
   process (append msg keys)
   (lambda (frame)
     (replique-trace--said frame what)
     (unless (equal "error" (plist-get frame :tag))
       (replique-trace--show process keys)))))

;;;###autoload
(defun replique-trace (scope &optional depth)
  "Trace functions: what each is called with, returns, and the calls it makes.

SCOPE, asked: the function at point, `f'; it with the functions around
it, `a' - DEPTH calls away, the prefix argument, `replique-trace-depth'
without; every function of the namespace, `n'; or those a branch
changed, `d', against a revision asked for.  See `replique-trace-calls'
for the calls, as a tree.

What changed on disk since the process read it is offered to be loaded
first - see `replique-fresh-ensure'."
  (interactive
   (list (car (read-multiple-choice
               "Trace"
               '((?f "function" "the function at point")
                 (?a "around" "the function at point, what calls it and what it calls")
                 (?n "namespace" "every function of this namespace")
                 (?d "diff" "the functions changed since a revision"))))
         (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let ((process (replique-process-ensure))
        (keys (replique-dialect-keys)))
    (pcase scope
      ;; what changed on disk loaded first: a var traced is the function
      ;; the process holds, and a file edited since is not that
      (?f (let ((name (replique-trace--at-point)))
            (replique-fresh-ensure "tracing")
            (replique-trace--send process keys (list :op :trace :vars (vector name)) name)))
      (?a (let ((name (replique-trace--at-point)))
            (replique-fresh-ensure "tracing")
            (replique-trace--send process keys
                                  (list :op :trace :around name
                                        :depth (or depth replique-trace-depth))
                                  name)))
      ;; above the ns form, the buffer's
      (?n (let ((ns (or (replique-eval--ns-at (point)) (replique-eval--ns-at (point-max))
                        (user-error "This buffer names no namespace"))))
            (replique-fresh-ensure "tracing")
            (replique-trace--send process keys (list :op :trace :ns ns) ns)))
      (?d (let* ((root (or (locate-dominating-file default-directory ".git")
                           (user-error "Not in a git repository")))
                 (base (read-string "Changed since: " (replique-trace--base root)))
                 (changed (replique-trace--changed root base))
                 (any nil))
            (when (or (cdr (assq :clj changed)) (cdr (assq :cljs changed)))
              (replique-fresh-ensure "tracing"))
            (dolist (dialect '(:clj :cljs))
              (when-let* ((names (delete-dups (cdr (assq dialect changed)))))
                (setq any t)
                (replique-trace--send process (replique-trace--keys dialect)
                                      (list :op :trace :vars (vconcat names))
                                      (format "%s function%s changed since %s"
                                              (length names) (if (cdr names) "s" "")
                                              (if (string-match-p "\\`[0-9a-f]\\{40\\}\\'" base)
                                                  (substring base 0 7)
                                                base)))))
            (unless any (message "replique: no function changed since %s" base)))))))

;;;###autoload
(defun replique-untrace ()
  "Trace no function any more, of the dialect of this buffer."
  (interactive)
  (let ((process (replique-process-ensure)))
    (replique-process-request
     process (append (list :op :untrace) (replique-dialect-keys))
     (lambda (frame)
       (message "replique: %s"
                (if (equal "error" (plist-get frame :tag))
                    (plist-get frame :message)
                  (format "%s function%s traced no more"
                          (length (plist-get frame :untraced))
                          (if (= 1 (length (plist-get frame :untraced))) "" "s"))))))))

;;; The calls, as a tree

(defvar-local replique-trace--process nil
  "The process whose calls the buffer shows.")

(defvar-local replique-trace--keys nil
  "The dialect keys of the calls the buffer shows, nil for Clojure.")

(defvar-local replique-trace--said nil
  "What the process said of the calls made in none: the reply.")

(defvar-local replique-trace--made nil
  "The calls each call opened made, by its number.")

(defvar-local replique-trace--open nil
  "The calls whose calls are shown, by number.")

(defvar-local replique-trace--asking 0
  "Which refresh the answers coming are for: an older one's are dropped.")

(defvar replique-trace--timers (make-hash-table :test #'equal)
  "The refresh to come of each buffer of calls, by buffer.")

(defun replique-trace--buffer-of (process keys)
  "Return the buffer of the calls PROCESS traced in the dialect of KEYS, or nil."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'replique-trace-mode)
                     (eq process replique-trace--process)
                     (equal keys replique-trace--keys))))
            (buffer-list)))

(defun replique-trace--value (text)
  "Return TEXT, a value printed, as a line shows it: its beginning, coloured."
  (replique-clojure-fontify-string
   (let ((replique-debug-value-width replique-debug-recorded-width))
     (replique-debug--value-text text))))

(defun replique-trace--insert (call depth)
  "Insert the line of CALL, DEPTH calls deep, and the calls it made if open."
  (let* ((n (plist-get call :call))
         (open (gethash n replique-trace--open))
         (made (or (plist-get call :made) 0))
         (kept (or (plist-get call :kept) 0))
         (start (point)))
    (insert (make-string (* 2 depth) ?\s)
            (cond ((zerop made) "  ") (open "▾ ") (t "▸ "))
            (if (plist-get call :made-in-gone)
                (propertize "↖ " 'face 'shadow 'help-echo "Made in a call not kept")
              "")
            "(" (propertize (plist-get call :var) 'face 'font-lock-function-name-face))
    (dolist (arg (plist-get call :args))
      (insert " " (replique-trace--value arg)))
    (insert ")")
    (pcase (plist-get call :state)
      ("returned" (insert " ⇒ " (replique-trace--value (plist-get call :value))))
      ("threw" (insert (propertize (concat " ⇒ " (plist-get call :thrown))
                                   'face 'replique-trace-thrown)))
      (_ (insert (propertize " …" 'face 'shadow))))
    (insert (propertize
             (concat "  · "
                     (string-join
                      (delq nil
                            (list (when-let* ((ms (plist-get call :ms))) (format "%.1f ms" ms))
                                  (plist-get call :thread)
                                  (when-let* ((at (plist-get call :at)))
                                    (format-time-string "%T" (/ at 1000.0)))
                                  (unless (zerop made)
                                    (format "%s call%s" made (if (= 1 made) "" "s")))))
                      " · "))
             'face 'replique-trace-about)
            "\n")
    (put-text-property start (point) 'replique-trace-call call)
    (when open
      (dolist (c (gethash n replique-trace--made))
        (replique-trace--insert c (1+ depth)))
      (when (> made kept)
        (insert (make-string (* 2 (1+ depth)) ?\s)
                (propertize (format "  … %s call%s not kept" (- made kept)
                                    (if (= 1 (- made kept)) "" "s"))
                            'face 'shadow)
                "\n")))))

(defun replique-trace--where (pos)
  "Return where POS is: its call, line and column.
To find it again once the tree is written again."
  (save-excursion
    (goto-char pos)
    (list (plist-get (get-text-property (point) 'replique-trace-call) :call)
          (line-number-at-pos)
          (current-column))))

(defun replique-trace--back-to (where)
  "Go back to WHERE, from `replique-trace--where', and return point.
On its call if still shown, else on its line."
  (pcase-let ((`(,here ,line ,column) where))
    (goto-char (point-min))
    (let ((found (and here (text-property-search-forward
                            'replique-trace-call here
                            (lambda (n call) (eql n (plist-get call :call)))))))
      (if found
          (goto-char (prop-match-beginning found))
        (forward-line (1- line))))
    (move-to-column column)
    (point)))

(defun replique-trace--render ()
  "Write the tree of the calls, keeping point on the call it was on.
In the windows showing it, that call stays on the line it was on."
  (let* ((inhibit-read-only t)
         (here (replique-trace--where (point)))
         (windows (mapcar (lambda (window)
                            (let ((at (window-point window)))
                              (list window
                                    (replique-trace--where at)
                                    (count-lines (window-start window)
                                                 (save-excursion
                                                   (goto-char at)
                                                   (line-beginning-position))))))
                          (get-buffer-window-list nil nil t)))
         (said replique-trace--said)
         (traced (plist-get said :traced))
         (dropped (or (plist-get said :dropped) 0)))
    (erase-buffer)
    (insert (propertize
             (substitute-command-keys
              (concat (format "%s function%s traced · %s call%s kept"
                              (length traced) (if (= 1 (length traced)) "" "s")
                              (or (plist-get said :kept) 0)
                              (if (eql 1 (plist-get said :kept)) "" "s"))
                      (if (zerop dropped) ""
                        (format ", %s not - \\<replique-trace-mode-map>\\[replique-trace-clear] to keep the next"
                                dropped))
                      "\n"))
             'face 'shadow))
    (cond
     ((plist-get said :calls)
      (dolist (call (plist-get said :calls))
        (replique-trace--insert call 0)))
     ((null traced)
      (insert (substitute-command-keys
               "Nothing is traced - \\<replique-mode-map>\\[replique-trace] in a Clojure buffer traces functions\n")))
     (t (insert "No traced function was called yet\n")))
    ;; Written again, a window would start at the top and show point
    ;; wherever it fits: the tree would jump.
    (pcase-dolist (`(,window ,where ,above) windows)
      (let ((at (replique-trace--back-to where)))
        (set-window-point window at)
        (set-window-start window (progn (forward-line (- above)) (point)) t)))
    (replique-trace--back-to here)))

(defun replique-trace-refresh ()
  "Ask the process for the calls again, and the calls made in the ones open."
  (interactive)
  (let* ((buffer (current-buffer))
         (process replique-trace--process)
         (keys replique-trace--keys)
         (asking (setq replique-trace--asking (1+ replique-trace--asking)))
         (open (hash-table-keys replique-trace--open))
         (pending (1+ (length open)))
         (made (make-hash-table))
         (said nil)
         (answered
          (lambda ()
            (setq pending (1- pending))
            (when (and (zerop pending) (buffer-live-p buffer))
              (with-current-buffer buffer
                (when (= asking replique-trace--asking)
                  (when said (setq replique-trace--said said))
                  (setq replique-trace--made made)
                  (replique-trace--render)))))))
    (replique-process-request
     process (append (list :op :trace-calls) keys)
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (message "replique: %s" (plist-get frame :message))
         (setq said frame))
       (funcall answered)))
    (dolist (n open)
      (replique-process-request
       process (append (list :op :trace-calls :parent n) keys)
       (lambda (frame)
         (unless (equal "error" (plist-get frame :tag))
           (puthash n (plist-get frame :calls) made))
         (funcall answered))))))

(defun replique-trace--called (process frame)
  "Show the calls again soon, where they are shown: FRAME, an event of
PROCESS, says traced functions were called.  Once for many: a function
called in a loop says so at every call."
  (when (equal "trace-called" (plist-get frame :event))
    (let ((keys (when (equal "cljs" (plist-get frame :dialect))
                  (list :dialect :cljs :target (intern (concat ":" (plist-get frame :target)))))))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'replique-trace-mode)
                     (eq process replique-trace--process)
                     (equal (plist-get keys :dialect) (plist-get replique-trace--keys :dialect))
                     (or (null keys) (null (plist-get replique-trace--keys :target))
                         (equal (plist-get keys :target) (plist-get replique-trace--keys :target)))
                     (get-buffer-window buffer t)
                     (not (gethash buffer replique-trace--timers)))
            (puthash buffer
                     (run-with-timer 0.3 nil
                                     (lambda ()
                                       (remhash buffer replique-trace--timers)
                                       (when (buffer-live-p buffer)
                                         (with-current-buffer buffer
                                           (replique-trace-refresh)))))
                     replique-trace--timers)))))))

(add-hook 'replique-process-event-functions #'replique-trace--called)

(defun replique-trace--at ()
  "Return the call at point, or say there is none."
  (or (get-text-property (point) 'replique-trace-call)
      (user-error "No call here")))

(defun replique-trace-toggle ()
  "Show the calls the call at point made, or hide them."
  (interactive)
  (let* ((call (replique-trace--at))
         (n (plist-get call :call)))
    (cond
     ((gethash n replique-trace--open)
      (remhash n replique-trace--open)
      (replique-trace--render))
     ((zerop (or (plist-get call :made) 0)) (message "replique: it called no traced function"))
     (t (puthash n t replique-trace--open)
        (replique-trace-refresh)))))

(defun replique-trace-visit ()
  "Show the code of the function of the call at point."
  (interactive)
  (unless (replique-debug--show-place (replique-trace--at) nil)
    (user-error "Where this function is written is not known")))

(defun replique-trace--inspect (what title)
  "Browse WHAT of the call at point - `:value' or `:args' - titled TITLE."
  (let ((call (replique-trace--at)))
    (replique-inspect-show
     replique-trace--process replique-trace--keys
     (list :trace (list :call (plist-get call :call) what t))
     (format "%s %s" (plist-get call :var) title))))

(defun replique-trace-inspect-value ()
  "Browse what the call at point returned, or threw."
  (interactive)
  (replique-trace--inspect :value "returned"))

(defun replique-trace-inspect-args ()
  "Browse what the call at point was called with."
  (interactive)
  (replique-trace--inspect :args "args"))

(defun replique-trace-clear ()
  "Forget the calls kept: what is called next is kept from the first call."
  (interactive)
  (let ((buffer (current-buffer)))
    (replique-process-request
     replique-trace--process (append (list :op :trace-clear) replique-trace--keys)
     (lambda (_frame)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (clrhash replique-trace--open)
           (replique-trace-refresh)))))))

(defun replique-trace-untrace-all (&optional done)
  "Trace no function any more - the calls kept stay.
With DONE, the prefix argument, forget the calls too and close the buffer."
  (interactive "P")
  (when (y-or-n-p (if done
                      "Trace no function any more, forget the calls and close? "
                    "Trace no function any more? "))
    (let ((buffer (current-buffer))
          (process replique-trace--process)
          (keys replique-trace--keys))
      (replique-process-request
       process (append (list :op :untrace) keys)
       (lambda (_frame)
         (if done
             (replique-process-request process (append (list :op :trace-clear) keys))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (replique-trace-refresh))))))
      (when done (quit-window t)))))

(defun replique-trace--repl ()
  "Return the repl the calls of the buffer are made in, or say there is none."
  (let ((dialect (if replique-trace--keys :cljs :clj))
        (target (plist-get replique-trace--keys :target)))
    (or (seq-find (lambda (repl)
                    (and (replique-repl-live-p repl)
                         (eq dialect (replique-repl-dialect repl))
                         (or (null target) (eq target (replique-repl-target repl)))))
                  (replique-process--repls replique-trace--process))
        (user-error (if (eq :cljs dialect)
                        "No ClojureScript repl in this runtime"
                      "No Clojure repl")))))

(defun replique-trace-replay ()
  "Call the function of the call at point again, with what it was called with.

In the repl of the calls, which shows what it returns or throws - and the
call is a new one of the tree.  The function is the one its var holds
now: the code edited and evaluated since is what is called, and,
recorded - \\<replique-trace-mode-map>\\[replique-trace-record] - what each of its forms
evaluates to is written in it.  The args are the values it was called
with and not copies of them: what was mutated since is as it is now."
  (interactive)
  (let* ((call (replique-trace--at))
         (repl (replique-trace--repl)))
    (replique-repl-send-code
     repl
     (format "(apply %s%s (replique.trace/args-of %s))"
             ;; a var, which can be private
             (if (eq :cljs (replique-repl-dialect repl)) "" "#'")
             (plist-get call :var) (plist-get call :call))
     nil t)))

(defun replique-trace-record ()
  "Record the function of the call at point, as `replique-debug-record' does.

Its definition is evaluated again, from its buffer as it is now, to keep
what each of its forms evaluates to, written in its code call after call -
and is traced still.  \\<replique-trace-mode-map>\\[replique-trace-replay] calls it again with
what the call at point was called with."
  (interactive)
  (let* ((call (replique-trace--at))
         (repl (replique-trace--repl))
         (marker (or (replique-debug--show-place call nil)
                     (user-error "Where this function is written is not known"))))
    (with-current-buffer (marker-buffer marker)
      (unless (buffer-file-name)
        (user-error "%s is not written in a file of the project" (plist-get call :var)))
      (save-excursion
        (goto-char marker)
        ;; a .cljc file is recorded in the runtime of the calls
        (let ((replique-current-repl repl))
          (replique-debug-record))))))

(defvar replique-trace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'replique-trace-toggle)
    (define-key map (kbd "RET") #'replique-trace-visit)
    (define-key map (kbd "v") #'replique-trace-inspect-value)
    (define-key map (kbd "a") #'replique-trace-inspect-args)
    (define-key map (kbd "r") #'replique-trace-record)
    (define-key map (kbd "R") #'replique-trace-replay)
    (define-key map (kbd "c") #'replique-trace-clear)
    (define-key map (kbd "u") #'replique-trace-untrace-all)
    (define-key map (kbd "t") #'replique-trace)
    (define-key map (kbd "g") #'replique-trace-refresh)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap of the buffer of the traced calls.")

(define-derived-mode replique-trace-mode special-mode "Replique-Trace"
  "Major mode for the calls of the functions traced, as a tree.

\\{replique-trace-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (replique-trace-refresh))))

(defun replique-trace--show (process keys)
  "Show the calls PROCESS traced in the dialect of KEYS, in their buffer."
  (let ((buffer (or (replique-trace--buffer-of process keys)
                    (generate-new-buffer
                     (format "*replique-trace %s%s*" (replique-process--id process)
                             (if keys (format " %s" (substring (symbol-name (or (plist-get keys :target)
                                                                               :cljs))
                                                               1))
                               ""))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'replique-trace-mode)
        (replique-trace-mode)
        (setq replique-trace--process process
              replique-trace--keys keys
              replique-trace--open (make-hash-table)
              replique-trace--made (make-hash-table)))
      (replique-trace-refresh))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun replique-trace-calls ()
  "Show the calls of the functions traced, as a tree.

Of the dialect of this buffer.  Each with what it was called with and
returned, or threw, how long it took, and the calls it made.  Shown
again as calls come, while the buffer is shown."
  (interactive)
  (pop-to-buffer (replique-trace--show (replique-process-ensure) (replique-dialect-keys))))

(provide 'replique-trace)

;;; replique-trace.el ends here
