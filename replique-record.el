;;; replique-record.el --- Which functions are recorded  -*- lexical-binding: t; -*-

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

;; A function recorded keeps what each of its forms evaluated to, call by
;; call - see `replique-debug-record' - and its calls, with those of the
;; other functions recorded they made, are a tree, gone through a value at
;; a time - see `replique-recording'.  Which functions is what
;; `replique-record' asks:
;;
;;   f  the function at point
;;   a  it, with the functions that call it and those it calls - as the
;;      compiler recorded them, so the files have to have been loaded
;;   n  every function of the namespace
;;   d  the functions a branch changed: those whose definitions a
;;      `git diff' against a revision touches, the work tree included
;;
;; Each is evaluated again from its file, to be recorded - the files
;; opened, where they are not, since a definition recorded shows which call
;; it is at in its code.  A function is a `defn', a `defmethod' or a `def'
;; of a `fn': evaluating anything else again would make it anew - an atom,
;; a connection.
;;
;; The functions of libraries are not recorded, nor clojure.core's: a
;; library's code is in a jar, not to be read through.  With
;; `replique-record-libraries', those the functions recorded call are
;; recorded by their calls only - what each was called with, and what it
;; returned or threw - among the calls of the tree: going into one is
;; going to what it was called with, and then to what it returned.
;;
;;   C-c C-d r  `replique-record'           C-c C-d c  `replique-recording-calls'

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'replique-common)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-parse)
(require 'replique-eval)
(require 'replique-debug)
(require 'replique-fresh)

(defcustom replique-record-depth 1
  "How many calls around a function `replique-record' records with it, by default."
  :type 'natnum
  :group 'replique)

(defcustom replique-record-libraries nil
  "Whether the library functions recorded functions call are recorded too.
By their calls only: what each was called with and what it returned or
threw, and only the calls made in a call recorded.  Never clojure.core's
nor cljs.core's."
  :type 'boolean
  :group 'replique)

;;; Which functions

(defun replique-record--head (node)
  "Return the name NODE, a list, starts with, without its namespace - or nil."
  (when-let* (((eq 'list (replique-parse-type node)))
              (op (car (replique-parse-forms node)))
              ((eq 'symbol (replique-parse-type op))))
    (replace-regexp-in-string "\\`.*/" "" (replique-parse-text op))))

(defun replique-record--defined (node)
  "Return the name NODE, a top level form, defines a function as - or nil.

A `defn' of any namespace - a schema one, a spec one - a `defmethod', or a
`def' of a `fn': what evaluating again defines again, and makes nothing
anew.  A `def' of anything else would make an atom anew, or a connection."
  (let* ((forms (and (eq 'list (replique-parse-type node)) (replique-parse-forms node)))
         (head (replique-record--head node))
         (named (and (cadr forms) (replique-parse-unwrap-meta (cadr forms)))))
    (and head
         named
         (eq 'symbol (replique-parse-type named))
         (or (member head '("defn" "defn-" "defmethod"))
             (and (equal head "def")
                  (nth 2 forms)
                  (member (replique-record--head (nth 2 forms)) '("fn" "fn*"))))
         (replique-parse-text named))))

(defun replique-record--ns-form (forms)
  "Return the name the first `ns' form of FORMS declares, or nil."
  (seq-some (lambda (node)
              (when (eq 'list (replique-parse-type node))
                (let ((parts (replique-parse-forms node)))
                  (when (and (cdr parts)
                             (equal "ns" (replique-parse-text (car parts))))
                    (replique-parse-text (replique-parse-unwrap-meta (cadr parts)))))))
            forms))

(defun replique-record--at-point ()
  "Return the qualified name of the function defined around point."
  (let* ((node (or (replique-parse-form-at (point))
                   (user-error "No definition here")))
         (named (or (replique-record--defined node)
                    (user-error "No function is defined here")))
         (ns (or (replique-eval--ns-at (replique-parse-start node))
                 (user-error "This buffer names no namespace"))))
    (format "%s/%s" ns named)))

(defun replique-record--dialects (file)
  "Return the dialects whose functions FILE defines, by its extension."
  (pcase (file-name-extension file)
    ("clj" '(:clj))
    ("cljs" '(:cljs))
    ("cljc" '(:clj :cljs))))

(defun replique-record--hunks (diff)
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

(defun replique-record--functions (forms ranges)
  "Return the functions FORMS, of the current buffer, define on lines RANGES.
Or, RANGES t, every one: ((NAME . NODE)...), NAME qualified."
  (when-let* ((ns (replique-record--ns-form forms)))
    (delq nil
          (mapcar (lambda (node)
                    (when-let* ((named (replique-record--defined node)))
                      (let ((from (line-number-at-pos (replique-parse-start node)))
                            (to (line-number-at-pos (replique-parse-end node))))
                        (when (or (eq t ranges)
                                  (seq-some (lambda (r) (and (<= (car r) to) (>= (cdr r) from)))
                                            ranges))
                          (cons (format "%s/%s" ns named) node)))))
                  forms))))

(defun replique-record--touched (file ranges)
  "Return the functions of FILE the line RANGES touch, qualified - or with
RANGES t, every one."
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar #'car (replique-record--functions (replique-parse-forms (replique-parse-buffer))
                                              ranges))))

(defun replique-record--git (root &rest args)
  "Return what git says to ARGS in ROOT, or signal what it said instead."
  (with-temp-buffer
    (let* ((default-directory root)
           (status (apply #'process-file "git" nil t nil args)))
      (unless (eql 0 status)
        (user-error "git %s: %s" (car args) (string-trim (buffer-string))))
      (buffer-string))))

(defun replique-record--base (root)
  "Return the revision a branch in ROOT is likely to be compared with."
  (or (seq-some (lambda (upstream)
                  (ignore-errors
                    (string-trim (replique-record--git root "merge-base" "HEAD" upstream))))
                '("origin/HEAD" "origin/master" "origin/main" "master" "main"))
      "HEAD"))

(defun replique-record--changed (root base)
  "Return the functions changed in ROOT since BASE, by dialect and file.

As ((:clj (FILE . NAMES)...) (:cljs (FILE . NAMES)...)), FILE absolute.
The work tree as it is, the files nobody added yet with it."
  (let* ((globs '("*.clj" "*.cljc" "*.cljs"))
         (hunks (replique-record--hunks
                 (apply #'replique-record--git root "diff" "--unified=0" "--no-color"
                        "--no-ext-diff" base "--" globs)))
         (added (split-string (apply #'replique-record--git root "ls-files" "--others"
                                     "--exclude-standard" "--" globs)
                              "\n" t))
         (by-dialect (list (list :clj) (list :cljs))))
    (dolist (touched (append hunks (mapcar (lambda (f) (cons f t)) added)))
      (let ((file (expand-file-name (car touched) root)))
        (when (file-readable-p file)
          (when-let* ((names (replique-record--touched file (cdr touched))))
            (dolist (dialect (replique-record--dialects file))
              (setcdr (assq dialect by-dialect)
                      (append (cdr (assq dialect by-dialect)) (list (cons file names)))))))))
    by-dialect))

;;; Recording

(defun replique-record--why-not (skipped)
  "Say why the vars SKIPPED were not recorded, those of each reason together."
  (let ((by-why nil))
    (dolist (s skipped)
      (let* ((why (plist-get s :why))
             (cell (or (assoc why by-why) (car (push (list why) by-why)))))
        (setcdr cell (append (cdr cell) (list (plist-get s :var))))))
    (mapconcat (lambda (cell)
                 (let ((vars (cdr cell)))
                   (format "%s (%s): %s%s"
                           (car cell)
                           (length vars)
                           (string-join (seq-take vars 3) ", ")
                           (if (nthcdr 3 vars) ", …" ""))))
               (nreverse by-why) "; ")))

(defun replique-record--plural (n word)
  "Return N WORDs, as it is said."
  (format "%s %s%s" n word (if (= 1 n) "" "s")))

(defun replique-record--libraries (process keys names)
  "Ask PROCESS to record by their calls the library functions NAMES call.
In the dialect of KEYS - and say how many it does."
  (replique-process-request
   process (append (list :op :record-calls :callees-of (vconcat names)) keys)
   (lambda (frame)
     (if (equal "error" (plist-get frame :tag))
         (message "replique: the calls of library functions are not recorded - %s"
                  (plist-get frame :message))
       (let ((recorded (plist-get frame :recorded))
             (skipped (plist-get frame :skipped)))
         (message "replique: the calls of %s recorded%s"
                  (replique-record--plural (length recorded) "library function")
                  (if skipped
                      (format ", of %s not - %s" (length skipped)
                              (replique-record--why-not skipped))
                    "")))))))

(defun replique-record--files (repl files)
  "Record the functions FILES name, in REPL: ((FILE . NAMES)...).
Each file opened, where it is not - a definition recorded shows what it
recorded in its code - and its functions evaluated again from there.
Return how many are."
  (let ((count 0))
    (pcase-dolist (`(,file . ,names) files)
      (with-current-buffer (find-file-noselect file)
        (let* ((forms (replique-parse-forms (replique-parse-buffer)))
               (nodes (mapcar #'cdr (seq-filter (lambda (f) (member (car f) names))
                                                (replique-record--functions forms t)))))
          (when nodes
            (setq count (+ count (length nodes)))
            ;; a .cljc file is recorded in the runtime of the repl in hand
            (let ((replique-current-repl repl))
              (replique-eval--send nodes 'record))))))
    count))

(defun replique-record--said (count files what)
  "Say COUNT functions of FILES files are recorded, as WHAT."
  (message "replique: %s: %s%s recorded - %s"
           what
           (replique-record--plural count "function")
           (if (> files 1) (format " of %s files" files) "")
           (substitute-command-keys "\\[replique-recording-calls] shows the calls")))

;;;###autoload
(defun replique-record (scope &optional depth)
  "Record functions: what each of their forms evaluates to, call by call.

SCOPE, asked: the function at point, `f' - or the form at point, which
`replique-debug-record' records whatever it is; it with the functions
around it, `a' - DEPTH calls away, the prefix argument,
`replique-record-depth' without; every function of the namespace, `n';
or those a branch changed, `d', against a revision asked for.  And with
`replique-record-libraries', the library functions these call, by their
calls only.  See `replique-recording' to go through what they did, and
`replique-recording-calls' for their calls, as a tree.

Of the dialect of this buffer: a .clj buffer records Clojure, a .cljs one
ClojureScript, and a .cljc one - or a buffer of neither, as a diff -
the dialect of the repl in hand.

Each is evaluated again, from its file, to be recorded.  What changed on
disk since the process read it is offered to be loaded first, for what
is around a function to be what the files say - see
`replique-fresh-ensure'."
  (interactive
   (list (car (read-multiple-choice
               "Record"
               '((?f "function" "the function at point")
                 (?a "around" "the function at point, what calls it and what it calls")
                 (?n "namespace" "every function of this namespace")
                 (?d "diff" "the functions changed since a revision"))))
         (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let ((process (replique-process-ensure))
        (keys (replique-dialect-keys)))
    (pcase scope
      (?f (let ((name (ignore-errors (replique-record--at-point))))
            (replique-debug-record)
            (when (and name replique-record-libraries)
              (replique-record--libraries process keys (list name)))))
      (?a (let ((name (replique-record--at-point))
                (repl (replique-repl-ensure-here)))
            (replique-fresh-ensure "recording")
            (replique-process-request
             process (append (list :op :recording-scope :around name
                                   :depth (or depth replique-record-depth))
                             keys)
             (lambda (frame)
               (if (equal "error" (plist-get frame :tag))
                   (message "replique: %s" (plist-get frame :message))
                 (let* ((vars (plist-get frame :vars))
                        (files nil))
                   (dolist (v vars)
                     (let ((cell (or (assoc (plist-get v :file) files)
                                     (car (push (list (plist-get v :file)) files)))))
                       (setcdr cell (append (cdr cell) (list (plist-get v :var))))))
                   (replique-record--said
                    (replique-record--files repl (nreverse files)) (length files)
                    (format "%s with %s and %s" name
                            (replique-record--plural (length (plist-get frame :callers)) "caller")
                            (replique-record--plural (length (plist-get frame :callees)) "callee")))
                   (when replique-record-libraries
                     (replique-record--libraries process keys
                                                 (mapcar (lambda (v) (plist-get v :var)) vars)))))))))
      ;; above the ns form, the buffer's
      (?n (let* ((ns (or (replique-eval--ns-at (point)) (replique-eval--ns-at (point-max))
                         (user-error "This buffer names no namespace")))
                 (functions (save-restriction
                              (widen)
                              (replique-record--functions
                               (replique-parse-forms (replique-parse-buffer)) t))))
            (unless functions (user-error "No function is defined in this buffer"))
            (replique-fresh-ensure "recording")
            (replique-eval--send (mapcar #'cdr functions) 'record)
            (replique-record--said (length functions) 1 ns)
            (when replique-record-libraries
              (replique-record--libraries process keys (mapcar #'car functions)))))
      ;; of the dialect of this buffer only, as the other scopes: a .cljc
      ;; one's, that of the repl in hand
      (?d (let* ((root (or (locate-dominating-file default-directory ".git")
                           (user-error "Not in a git repository")))
                 (base (read-string "Changed since: " (replique-record--base root)))
                 (dialect (replique-dialect))
                 (files (cdr (assq dialect (replique-record--changed root base))))
                 (since (if (string-match-p "\\`[0-9a-f]\\{40\\}\\'" base)
                            (substring base 0 7)
                          base)))
            (if (not files)
                (message "replique: no %s function changed since %s"
                         (if (eq :cljs dialect) "ClojureScript" "Clojure") since)
              (let ((repl (replique-repl-ensure-here)))
                (replique-fresh-ensure "recording")
                (replique-record--said (replique-record--files repl files) (length files)
                                       (format "changed since %s" since))
                (when replique-record-libraries
                  (replique-record--libraries process keys
                                              (delete-dups (apply #'append (mapcar #'cdr files))))))))))))

(provide 'replique-record)

;;; replique-record.el ends here
