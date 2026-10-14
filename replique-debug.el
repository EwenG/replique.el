;;; replique-debug.el --- A thread of the process, stopped where the code says  -*- lexical-binding: t; -*-

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

;; A thread that reaches (replique.debug/break!) stops there, and this is
;; what is shown of it: where it stopped, with an arrow in the fringe of the
;; file; its frames, innermost first, in a buffer of the thread's own; and
;; the locals of a frame, as a value to browse - see `replique-inspect'.
;;
;;   c   continue                  RET  the frame at point: its source and
;;   a   abort - throw from break!      its locals
;;   r   start the call of the frame at point over, running the code as
;;       it is now: redefine, then restart
;;   e   a repl evaluating in the frame at point, with its locals bound,
;;       on the thread that stopped - and the code of the Clojure
;;       buffers with it, see `replique-debug-eval-in-frame'
;;   d   in the view of the locals: keep the local at point as a var
;;   n   step to the next form       i  step into a call   o  step out
;;   v   the value the form stepped to evaluated to, as a value to browse
;;
;; Stepping is through code evaluated with `replique-debug-defun': it stops
;; each time it is called, and from there goes form by form, each form shown
;; in the source as it is evaluated, with what it evaluated to after it.  The
;; values stay, dimmed, as the thread goes on - the way the data went
;; through the function, written over its code.  A value is shown without
;; realizing what of it is lazy and not realized yet, which printing it
;; would do - on another thread, before the code got to it.
;;
;; A definition stepped through is marked in the fringe, and stops each
;; time it is called until `replique-debug-uninstrument' - or until it is
;; evaluated again, the ordinary way.  It needs the compiler of
;; replique-clj.
;;
;; A breakpoint - `replique-debug-break', on a form - stops the thread once
;; that form has its value, where a condition holds if it was given one.
;; Its definition is evaluated to stop at its breakpoints only, rather than
;; on entry, where it is not stepped through already; and it keeps its
;; breakpoints when it is evaluated to be stepped through again.  Evaluated
;; the ordinary way, it has none.
;;
;; A definition evaluated with `replique-debug-record' stops nothing, but
;; keeps what each of its forms evaluates to, call by call: the values of
;; the last call are written after its forms, as they come, and the calls
;; before it are gone back to - each value one to browse.  An example of
;; everything the function was handed and made, written over its code.  It
;; does not need the debugger.  Once its code is edited, what is written is
;; of code that is not there any more: the values of the forms edited go,
;; and the others are faded - what they were before the edit, browsed and
;; said by eldoc still - until the definition is recorded again.  Edited
;; back to what was recorded, undone, they are shown as they were.
;;
;; What is written is a value per line by default - what the outermost
;; forms of the line evaluated to, at its end, where it is out of the way of
;; the code - and C-c C-d t goes to a value after every form, and to none.
;; The value of the form at point is said by eldoc, before what a call
;; takes, and the form is highlighted.
;;
;; What a function was called with is shown on its params, as what it
;; was handed.
;;
;; A call recorded is one to try things against, the way a frame of a
;; stopped thread is, without anything stopped: C-c C-d e in a recorded
;; definition opens a repl whose code is evaluated with the locals in scope
;; at point bound to what they were in the call shown - see
;; `replique-debug-eval-in-call'.  It follows the call shown.
;;
;; A loop of a call is gone round an iteration at a time: C-c C-d ] with
;; point in it shows what the code it goes round evaluated in its next
;; iteration, and nothing for what it did not evaluate then, C-c C-d [ in
;; the one before, C-c C-d s in the first where something holds, and
;; C-c C-d w the whole call again.  C-c C-d e and C-c C-d v follow it.
;; C-c C-d i browses what the form at point evaluated to in each iteration,
;; or with C-u, or elsewhere in the loop, every iteration, a row each, with
;; a column for the form at point.
;;
;; ClojureScript is recorded too, and only recorded: a page or a node
;; program cannot be stopped from here, so stepping and breakpoints are
;; Clojure's.  A .cljs definition is recorded in the runtime of the repl it
;; is sent to, which keeps its values; it is let go of when that page is
;; reloaded, or its namespace is compiled again.
;;
;;   C-c C-d d  `replique-debug-defun'      C-c C-d b  `replique-debug-break'
;;   C-c C-d r  `replique-debug-record'     C-c C-d v  the value recorded
;;   C-c C-d p  the call before             C-c C-d n  the call after
;;   C-c C-d u  `replique-debug-uninstrument'
;;   C-c C-d t  `replique-debug-cycle-recorded'
;;   C-c C-d ]  the next iteration of a loop   C-c C-d [  the one before
;;   C-c C-d s  `replique-debug-find-iteration'  C-c C-d w  the whole call
;;   C-c C-d i  `replique-debug-inspect-iterations'
;;   C-c C-d e  `replique-debug-eval-in-frame', or in a recorded
;;              definition `replique-debug-eval-in-call'
;;
;; What stopped is the thread and nothing else: the process goes on
;; answering, and the other repls go on evaluating.  A repl whose form
;; stopped waits for it, the way it waits for anything slow.
;;
;; A thread has two buffers: its frames, and the locals of the frame looked
;; at - one view, which follows the frame visited.  Once the thread goes on
;; they go, with the files opened only to show where it was, unless it stops
;; again within `replique-debug-linger' seconds - as a loop does, or a call
;; started over: then they stay, and what changed in the locals since the
;; last stop is highlighted.
;;
;; The process has to be started for it: `replique-connect' offers a start
;; with the debugger, and `replique-debugger' starts every process with it.
;; Starting a call over needs its arguments, which Clojure clears after their
;; last use unless asked not to - see `replique-debug-keep-locals'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'replique-common)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-exception)
(require 'replique-pprint)
(require 'replique-symbol)
(require 'replique-inspect)
(require 'replique-eval)

(defcustom replique-debug-linger 1.5
  "How long what is shown of a thread stays once it goes on, in seconds.

A thread that stops again by then is shown in the same buffers, the locals
highlighted where they changed; otherwise the buffers go."
  :type 'number
  :group 'replique)

(defface replique-debug-stopped-line
  '((t :inherit secondary-selection :extend t))
  "Face for the line a thread stopped on."
  :group 'replique)

(defcustom replique-debug-value-width 60
  "How much of a value a step shows after the form, in characters."
  :type 'natnum
  :group 'replique)

(defface replique-debug-value
  '((t :inherit success :weight bold))
  "Face for what the form a thread stopped after evaluated to."
  :group 'replique)

(defface replique-debug-old-value
  '((t :inherit shadow))
  "Face for what a form evaluated to, before the thread went on."
  :group 'replique)

(defface replique-debug-instrumented
  '((t :inherit warning))
  "Face for the fringe mark of a definition that is stepped through."
  :group 'replique)

(defface replique-debug-breakpoint
  '((((class color)) :underline (:color "red3"))
    (t :underline t))
  "Face for a form a breakpoint is on, and its mark in the fringe."
  :group 'replique)

(defface replique-debug-breakpoint-inactive
  '((((class color)) :underline (:color "gray50" :style wave))
    (t :underline t))
  "Face for a breakpoint whose definition is not stepped through yet."
  :group 'replique)

(defcustom replique-debug-recorded-width 40
  "How much of a value recorded is shown after its form, in characters."
  :type 'natnum
  :group 'replique)

(defcustom replique-debug-recorded-shown 'line
  "Which values recorded a definition that records shows over its code.

`line', at the end of each line, what the outermost forms written on it
evaluated to; `every', after every form; `none', no value.  Whichever it
is, eldoc says what the form at point evaluated to, and
\\[replique-debug-cycle-recorded] goes from one to the next.  Literals,
which evaluate to themselves, are not shown."
  :type '(choice (const :tag "One per line" line)
                 (const :tag "After every form" every)
                 (const :tag "None" none))
  :group 'replique)

(defface replique-debug-recorded
  '((t :inherit shadow :slant italic))
  "Face for what a form of a definition that records evaluated to."
  :group 'replique)

(defface replique-debug-recorded-edited
  '((((class color) (background dark)) :foreground "gray35" :slant italic)
    (((class color) (background light)) :foreground "gray70" :slant italic)
    (t :inherit replique-debug-recorded))
  "Face for what a form evaluated to, recorded before its definition was edited."
  :group 'replique)

(defface replique-debug-recorded-at-point
  '((t :inherit highlight))
  "Face for the recorded form point is at, which eldoc says the value of.
Only its background is used, so that the code keeps its colors - a
theme's `highlight' often has a foreground too."
  :group 'replique)

(defface replique-debug-recorded-call
  '((t :inherit replique-note))
  "Face for which call of a definition that records is shown."
  :group 'replique)

(defface replique-debug-host-frame
  '((t :inherit shadow))
  "Face for a frame that is not a Clojure function."
  :group 'replique)

;;; Where a thread stopped, in the file

(defvar replique-debug--arrow nil
  "Where the last thread that stopped stopped, as a marker.")

(add-to-list 'overlay-arrow-variable-list 'replique-debug--arrow)

(defvar replique-debug--line nil
  "The overlay on the line the last thread that stopped stopped on.")

(defvar replique-debug--arrow-thread nil
  "Which (PROCESS . THREAD) the arrow is for.")

(defun replique-debug--forget-arrow ()
  "Take the arrow and the highlight away."
  (setq replique-debug--arrow nil
        replique-debug--arrow-thread nil)
  (when (overlayp replique-debug--line)
    (delete-overlay replique-debug--line))
  (setq replique-debug--line nil))

(defvar replique-debug--opened (make-hash-table :test #'equal)
  "The buffers opened to show where a thread is, by (PROCESS . THREAD).

Each as (BUFFER . TICK), TICK its `buffer-chars-modified-tick' once opened:
one written in since is not the debugger's any more.")

(defun replique-debug--locate (found &optional for)
  "Return a marker at where FOUND says, or nil where its file cannot be had.

FOUND carries `:file', and `:entry' for a file inside a jar, `:line' and
`:column' - what the process answers a place in a source with.  A buffer
opened for it is one of the thread FOR, (PROCESS . THREAD), and goes with
what is shown of the thread."
  (when-let* ((before (buffer-list))
              (buffer (replique-symbol-visit found)))
    (when (and for (not (memq buffer before)))
      (push (cons buffer (buffer-chars-modified-tick buffer))
            (gethash for replique-debug--opened)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (when-let* ((line (plist-get found :line)))
            (forward-line (1- line))
            (when-let* ((column (plist-get found :column)))
              (forward-char (min (1- column) (- (line-end-position) (point))))))
          (point-marker))))))

(defun replique-debug--show-place (found for &optional arrow)
  "Show where FOUND says in a window, and return the marker there.

FOR is the thread it is shown for, (PROCESS . THREAD).  With ARROW, the
fringe arrow and the highlight are put there, as where it stopped."
  (when-let* ((marker (replique-debug--locate found for)))
    (when arrow
      (replique-debug--forget-arrow)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (setq replique-debug--arrow (copy-marker (line-beginning-position)))
          ;; The form, where it has a span - which is where a step stopped -
          ;; and otherwise the line
          (let ((end (and (plist-get found :end-line)
                          (replique-debug--locate (list :file (plist-get found :file)
                                                        :entry (plist-get found :entry)
                                                        :line (plist-get found :end-line)
                                                        :column (plist-get found :end-column))))))
            (unless (and end (eq (marker-buffer end) (current-buffer)) (> end marker))
              (setq end nil))
            (setq replique-debug--line
                  (if end
                      (make-overlay marker end)
                    (make-overlay (line-beginning-position) (1+ (line-end-position)))))
            (replique-debug--show-step-value for end (plist-get found :value)))
          (overlay-put replique-debug--line 'face 'replique-debug-stopped-line)
          (overlay-put replique-debug--line 'priority 10))))
    (let ((window (display-buffer (marker-buffer marker)
                                  '((display-buffer-reuse-window
                                     display-buffer-use-some-window)
                                    (inhibit-same-window . t)))))
      (when window
        (set-window-point window marker)))
    marker))

;;; What the forms evaluated to

(defvar replique-debug--values (make-hash-table :test #'equal)
  "The values shown after the forms a thread stepped through.

By (PROCESS . THREAD), as overlays: the last one is the value of the form
the thread is stopped after.")

(defun replique-debug--value-text (value)
  "Return VALUE, a value printed, as it is shown after its form."
  (let ((text (replace-regexp-in-string "[ \t\n]+" " " value)))
    (if (> (length text) replique-debug-value-width)
        (concat (string-trim-right (substring text 0 replique-debug-value-width)) "…")
      text)))

(defun replique-debug--painted (arrow value face)
  "Return ARROW and VALUE, a value as shown, painted for after a form.

VALUE is painted as Clojure, and FACE is put under its colours, and over
the whole of ARROW."
  (let ((value (replique-clojure-fontify-string value)))
    (add-face-text-property 0 (length value) face t value)
    ;; the cursor stays against the form, not after the value
    (concat (propertize arrow 'face face 'cursor t) value)))

(defun replique-debug--show-step-value (for end value)
  "Show VALUE after the form ending at END, for the thread FOR.

What was shown for the thread before is dimmed, and a value shown at END
before - the form evaluated again, in a loop - goes."
  (let ((overlays (seq-filter #'overlay-buffer (gethash for replique-debug--values))))
    (dolist (o overlays)
      (if (and end (= (overlay-start o) end))
          (progn (delete-overlay o) (setq overlays (delq o overlays)))
        (overlay-put o 'after-string
                     (propertize (overlay-get o 'replique-debug-text)
                                 'face 'replique-debug-old-value))))
    (when (and end value)
      (let ((o (make-overlay end end nil t))
            (text (replique-debug--value-text value)))
        (overlay-put o 'replique-debug-text (concat " ⇒ " text))
        (overlay-put o 'after-string
                     (replique-debug--painted " ⇒ " text 'replique-debug-value))
        (push o overlays)))
    (puthash for overlays replique-debug--values)))

(defun replique-debug--forget-values (for)
  "Take away the values shown for the thread FOR."
  (mapc #'delete-overlay (gethash for replique-debug--values))
  (remhash for replique-debug--values))

;;; What is stepped through

(defvar replique-debug--units (make-hash-table :test #'equal)
  "The marks of the definitions stepped through, by (PROCESS . UNIT).

UNIT is the number the process gave what it evaluated to be stepped
through.  Each mark is an overlay over the definition.")

(defun replique-debug--position (line column)
  "Return where LINE and COLUMN, counted from 1, are in the current buffer."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char (min (1- (or column 1)) (- (line-end-position) (point))))
      (point))))

(defun replique-debug--unit-keys (unit)
  "Return the dialect keys of what UNIT is asked about, nil for Clojure.

Those of the runtime its code is in, which the event says - see
`replique-dialect-keys'."
  (when (equal "cljs" (plist-get unit :dialect))
    (append (list :dialect :cljs)
            (when-let* ((target (plist-get unit :target)))
              (list :target (intern (concat ":" target)))))))

(defun replique-debug--instrumented (process unit)
  "Mark UNIT, what PROCESS steps through, as the process said it.

In the buffer visiting its file, where there is one: a file is not opened
to be marked.  A mark of the same process starting where it starts is of
the unit it replaces, let go of: the process says so too, but of
ClojureScript only after it says this."
  (when-let* ((n (plist-get unit :unit))
              (file (plist-get unit :file))
              ((not (plist-get unit :entry)))
              (line (plist-get unit :line))
              (buffer (find-buffer-visiting file)))
    (replique-debug--uninstrumented process n)
    (with-current-buffer buffer
      (let* ((start (replique-debug--position line (plist-get unit :column)))
             (end (if (plist-get unit :end-line)
                      (replique-debug--position (plist-get unit :end-line)
                                                (plist-get unit :end-column))
                    (save-excursion (goto-char start) (line-end-position))))
             (o (make-overlay start (max start end))))
        (dolist (old (replique-debug--marks-in start (1+ start)))
          (when (and (= start (overlay-start old))
                     (eq process (overlay-get old 'replique-debug-process)))
            ;; the repl evaluating in its calls goes on, in this one's
            (when-let* ((repl (overlay-get old 'replique-debug-repl)))
              (overlay-put old 'replique-debug-repl nil)
              (overlay-put o 'replique-debug-repl repl))
            (replique-debug--uninstrumented process (overlay-get old 'replique-debug-unit))))
        (overlay-put o 'replique-debug-unit n)
        (overlay-put o 'replique-debug-process process)
        ;; where the unit was read from, which its forms are found by
        (overlay-put o 'replique-debug-origin (cons line (or (plist-get unit :column) 1)))
        (overlay-put o 'evaporate t)
        (overlay-put o 'before-string
                     (propertize "*" 'display
                                 '(left-fringe filled-square replique-debug-instrumented)))
        (overlay-put o 'help-echo
                     (cond
                      ((plist-get unit :record) "Recorded - M-x replique-debug-uninstrument")
                      ((plist-get unit :breaks-only)
                       "Stops at its breakpoints - M-x replique-debug-uninstrument")
                      (t "Stepped through - M-x replique-debug-uninstrument")))
        (overlay-put o 'replique-debug-record (plist-get unit :record))
        (when (plist-get unit :record)
          (overlay-put o 'modification-hooks (list #'replique-debug--edited))
          (replique-debug--recording-here))
        (overlay-put o 'replique-debug-keys (replique-debug--unit-keys unit))
        (puthash (cons process n) o replique-debug--units)
        (dolist (b (replique-debug--breaks-in (overlay-start o) (overlay-end o)))
          (replique-debug--set-break b o))
        (when (plist-get unit :record)
          (replique-debug--ask-recorded o)
          (replique-debug--repl-follows o))))))

(defun replique-debug--unmark (o)
  "Take away the mark O, of a unit not stepped through any more.

Its breakpoints stay, not set: a unit stepped through there again sets
them."
  (when-let* ((buffer (overlay-buffer o)))
    (with-current-buffer buffer
      (dolist (b (replique-debug--breaks-in (overlay-start o) (overlay-end o)))
        (replique-debug--break-set b nil nil))))
  (replique-debug--forget-recorded o)
  (when-let* ((view (overlay-get o 'replique-debug-view))
              ((buffer-live-p view)))
    (kill-buffer view))
  (when-let* ((repl (overlay-get o 'replique-debug-repl)))
    (replique-debug--kill (replique-repl--buffer repl)))
  (delete-overlay o))

(defun replique-debug--uninstrumented (process n)
  "Take away the mark of the unit N of PROCESS, which is not stepped through."
  (when-let* ((o (gethash (cons process n) replique-debug--units)))
    (replique-debug--unmark o)
    (remhash (cons process n) replique-debug--units)))

(defun replique-debug--marks-in (start end)
  "Return the marks of what is stepped through between START and END."
  (seq-filter (lambda (o) (overlay-get o 'replique-debug-unit)) (overlays-in start end)))

(defun replique-debug--uninstrument (process n &optional quiet)
  "Ask PROCESS to stop stepping through its unit N - or every one, N nil.

Saying how many it stopped stepping through, unless QUIET."
  (replique-process-request
   process (append (list :op :debug-uninstrument) (when n (list :unit n)))
   (lambda (frame)
     (if (equal "error" (plist-get frame :tag))
         (message "replique: %s" (plist-get frame :message))
       (let ((units (plist-get frame :uninstrumented)))
         ;; the events said so too, but an answer is never lost
         (dolist (unit units) (replique-debug--uninstrumented process unit))
         (unless quiet
           (message "replique: %s"
                    (pcase (length units)
                      (0 "nothing was stepped through")
                      (1 "no longer stepped through")
                      (count (format "%s definitions no longer stepped through" count))))))))))

(defun replique-debug--evaluated-again (repl start end instrument)
  "Stop stepping through what was between START and END, evaluated again.

In the process of REPL, which it was sent to: what it is now is what was
sent, which was compiled to be stepped through or not - a new unit, if it
was, which its breakpoints are set in once the process says it is there.
Evaluated the ordinary way, as INSTRUMENT says, it has none."
  (dolist (o (replique-debug--marks-in start end))
    (let ((process (overlay-get o 'replique-debug-process)))
      (when (eq process (replique-repl--process repl))
        (replique-debug--uninstrument process (overlay-get o 'replique-debug-unit) t))))
  (unless instrument
    (mapc #'delete-overlay (replique-debug--breaks-in start end))))

;;; Breakpoints

(defun replique-debug--stepped-here ()
  "Refuse to stop the code of the current buffer where it is ClojureScript.

Which a page or a node program cannot be, from here: it is recorded."
  (when (eq :cljs (replique-dialect))
    (user-error "%s" (substitute-command-keys
                      "ClojureScript is recorded, and not stepped through - \[replique-debug-record]"))))

(defvar replique-debug--condition-history nil
  "The conditions breakpoints were given.")

(defun replique-debug--breaks-in (start end)
  "Return the breakpoints on the forms between START and END."
  (seq-filter (lambda (o) (overlay-get o 'replique-debug-break)) (overlays-in start end)))

(defun replique-debug--break-set (o process site)
  "Say that the breakpoint O is the one numbered SITE in PROCESS - or none."
  (overlay-put o 'replique-debug-process process)
  (overlay-put o 'replique-debug-site site)
  (let ((face (if site 'replique-debug-breakpoint 'replique-debug-breakpoint-inactive)))
    (overlay-put o 'face face)
    (overlay-put o 'before-string
                 (propertize "*" 'display `(left-fringe filled-rectangle ,face)))))

(defun replique-debug--line-column (pos)
  "Return where POS is, as (LINE . COLUMN), both counted from 1."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (cons (line-number-at-pos pos t) (1+ (- pos (line-beginning-position)))))))

(defun replique-debug--origin-of (mark pos)
  "Return where POS, in the unit MARK marks, was when the unit was read.
As (LINE . COLUMN): where it is now with the lines and columns the unit
moved since."
  (let ((origin (overlay-get mark 'replique-debug-origin))
        (now (replique-debug--line-column (overlay-start mark)))
        (here (replique-debug--line-column pos)))
    (cons (+ (car origin) (- (car here) (car now)))
          (if (= (car here) (car now))
              (+ (cdr origin) (- (cdr here) (cdr now)))
            (cdr here)))))

(defun replique-debug--set-break (o mark)
  "Set the breakpoint O in the unit MARK marks, as the process has it.

The form is named by where it was when the unit was read, which is where
it is now with the lines and columns the unit moved since."
  (let* ((process (overlay-get mark 'replique-debug-process))
         (place (replique-debug--origin-of mark (overlay-start o)))
         (line (car place))
         (column (cdr place)))
    (replique-process-request
     process
     (append (list :op :debug-break :unit (overlay-get mark 'replique-debug-unit)
                   :line line :column column)
             (when-let* ((condition (overlay-get o 'replique-debug-condition)))
               (list :condition condition)))
     (lambda (frame)
       (cond
        ((not (overlay-buffer o)))
        ((equal "error" (plist-get frame :tag))
         (delete-overlay o)
         (message "replique: no breakpoint - %s" (plist-get frame :message)))
        (t (replique-debug--break-set o process (plist-get frame :site))))))))

(defun replique-debug--form-at-point ()
  "Return the bounds of the form point is in or on, as (START . END).

A list: what a breakpoint can be on is a call, a macro or a special form."
  (save-excursion
    (let ((start (if (eq (char-after) ?\()
                     (point)
                   (condition-case nil
                       (progn (backward-up-list 1 t t)
                              (while (not (eq (char-after) ?\())
                                (backward-up-list 1 t t))
                              (point))
                     (scan-error nil)))))
      (unless start (user-error "No form at point"))
      (cons start (scan-sexps start 1)))))

(defun replique-debug--remove-break (o)
  "Take the breakpoint O away, in the process too where it is set there."
  (when-let* ((site (overlay-get o 'replique-debug-site)))
    (replique-process-request (overlay-get o 'replique-debug-process)
                              (list :op :debug-unbreak :site site)
                              #'ignore))
  (delete-overlay o))

;;;###autoload
(defun replique-debug-break (&optional condition)
  "Put a breakpoint on the form around point, or take it away.

A thread stops there once the form has its value - showing it, as a step
does - and is then stepped through from there.  With CONDITION, code
asked for interactively with a prefix argument, only where it holds: it is
run in the frame, with the locals of the form bound, each time the thread
gets there.

The definition the form is in is evaluated to stop at its breakpoints,
where it is not stepped through already: not on entry, and not until it
is called.  It needs the compiler of replique-clj, and the process started
with the debugger - see `replique-connect'."
  (interactive
   (progn
     (replique-debug--stepped-here)
     (list (when current-prefix-arg
             (read-from-minibuffer "Stop where: " nil read-expression-map nil
                                   'replique-debug--condition-history)))))
  (replique-debug--stepped-here)
  (let* ((form (replique-debug--form-at-point))
         (there (seq-find (lambda (o) (= (car form) (overlay-start o)))
                          (replique-debug--breaks-in (car form) (cdr form)))))
    (when there (replique-debug--remove-break there))
    (if (and there (not condition))
        (message "replique: breakpoint taken away")
      (let ((o (make-overlay (car form) (cdr form)))
            (mark (seq-find (lambda (m) (<= (overlay-start m) (car form) (overlay-end m)))
                            (replique-debug--marks-in (car form) (cdr form)))))
        (overlay-put o 'replique-debug-break t)
        (overlay-put o 'evaporate t)
        (overlay-put o 'help-echo (if condition
                                      (format "Breakpoint, where %s" condition)
                                    "Breakpoint"))
        (when (and condition (not (string-blank-p condition)))
          (overlay-put o 'replique-debug-condition condition))
        (replique-debug--break-set o nil nil)
        (if mark
            (replique-debug--set-break o mark)
          ;; set once the process says it steps through it - see
          ;; `replique-debug--instrumented'
          (replique-eval--send (list (replique-eval--defun-at-point)) 'break))))))

(add-hook 'replique-eval-evaluated-functions #'replique-debug--evaluated-again)

;;; Recording

(defun replique-debug--here (mark line column)
  "Return where LINE and COLUMN of the unit MARK marks are now.

As the unit was read, from where it was written: where that is now is
where it was with the lines and columns the unit moved since."
  (let* ((origin (overlay-get mark 'replique-debug-origin))
         (now (replique-debug--line-column (overlay-start mark))))
    (replique-debug--position (+ line (- (car now) (car origin)))
                              (if (= line (car origin))
                                  (+ column (- (cdr now) (cdr origin)))
                                column))))

(defun replique-debug--forget-recorded (mark)
  "Take away what is shown of what the unit MARK marks recorded."
  (mapc #'delete-overlay (overlay-get mark 'replique-debug-shown))
  (overlay-put mark 'replique-debug-shown nil)
  (when-let* ((buffer (overlay-buffer mark)))
    (with-current-buffer buffer (replique-debug--follow-point))))

(defun replique-debug--ask-recorded (mark &optional call)
  "Show what the unit MARK marks recorded in the call CALL - the last one.

Without CALL the last call is followed: shown again as the process says
there are more."
  (let ((process (overlay-get mark 'replique-debug-process))
        (iteration (overlay-get mark 'replique-debug-iteration)))
    (overlay-put mark 'replique-debug-call call)
    (replique-process-request
     process
     (append (list :op :debug-recorded :unit (overlay-get mark 'replique-debug-unit))
             (when call (list :call call))
             (when iteration (list :iteration iteration)))
     (lambda (frame)
       (cond
        ((not (overlay-buffer mark)))
        ((overlay-get mark 'replique-debug-edited))
        ((equal "error" (plist-get frame :tag))
         (message "replique: %s" (plist-get frame :message)))
        ;; another call, or iteration, was asked for since
        ((not (and (eql call (overlay-get mark 'replique-debug-call))
                   (equal iteration (overlay-get mark 'replique-debug-iteration)))))
        (t (replique-debug--show-recorded mark frame)))))))

(defun replique-debug--recorded-text (value times &optional param edited)
  "Return VALUE, printed, as it is shown after its form, evaluated TIMES.

Or, with PARAM, what the param of that name was called with, said where
the param itself is not.  EDITED, its definition was edited since: faded,
and not painted as Clojure."
  (let ((arrow (if param (format " %s ⇒ " param) " ⇒ "))
        (text (let ((replique-debug-value-width replique-debug-recorded-width))
                (replique-debug--value-text value)))
        (face (if edited 'replique-debug-recorded-edited 'replique-debug-recorded)))
    (concat (if edited
                (propertize (concat arrow text) 'face face 'cursor t)
              (replique-debug--painted arrow text face))
            (if (and times (> times 1))
                (propertize (format " ×%s" times) 'face face)
              ""))))

(defun replique-debug--literal-p (text)
  "Return non-nil when TEXT, a form, is a literal: what it evaluates to."
  (string-match-p (rx bos (or (seq (? (any "+-")) digit) "\"" "#\"" ":" "\\"
                              (seq (or "nil" "true" "false") eos)))
                  text))

(defun replique-debug--outermost (forms)
  "Return the FORMS no other of FORMS is in, by where they start."
  (let ((kept nil))
    (dolist (o (sort (copy-sequence forms)
                     (lambda (a b) (or (< (overlay-start a) (overlay-start b))
                                       (and (= (overlay-start a) (overlay-start b))
                                            (> (overlay-end a) (overlay-end b)))))))
      (when (or (null kept) (>= (overlay-start o) (overlay-end (car kept))))
        (push o kept)))
    (nreverse kept)))

(defun replique-debug--params-at (start)
  "Return the params of the fn whose form starts at START, as nodes.

The first vector written in it, which is where `fn', `defn' and an arity
have them - each param the form a value is for, the `&' left out."
  (when-let* ((top (replique-parse-form-at start))
              (form (seq-find (lambda (node)
                                (and (= start (replique-parse-start node))
                                     (eq 'list (replique-parse-type node))))
                              (replique-parse-path top start)))
              (params (seq-find (lambda (node) (eq 'vector (replique-parse-type node)))
                                (mapcar #'replique-parse-unwrap-meta
                                        (replique-parse-forms form)))))
    (seq-remove (lambda (node)
                  (and (eq 'symbol (replique-parse-type node))
                       (equal "&" (replique-parse-text node))))
                (replique-parse-forms params))))

(defun replique-debug--entered (mark value)
  "Return overlays over the params VALUE, the entry of a fn, says the args of.

In the unit MARK marks: each with what its param was called with, as the
value of a form is."
  (let ((start (replique-debug--here mark (plist-get value :line) (plist-get value :column)))
        (args (plist-get value :args))
        (i 0)
        (overlays nil))
    (when (<= (overlay-start mark) start (overlay-end mark))
      (dolist (node (replique-debug--params-at start))
        (when (< i (length args))
          (let ((o (make-overlay (replique-parse-start node) (replique-parse-end node)))
                (param (replique-parse-text node)))
            (overlay-put o 'replique-debug-recorded
                         (list :site (plist-get value :site) :arg i :param param
                               :value (nth i args) :times (plist-get value :times)))
            (push o overlays)))
        (setq i (1+ i))))
    overlays))

(defun replique-debug--call-text (mark)
  "Return which call the unit MARK marks shows, as it is said after its first line.

Edited since, it says so."
  (let* ((calls (overlay-get mark 'replique-debug-calls))
         (call (overlay-get mark 'replique-debug-shown-call))
         (index (seq-position calls call (lambda (c n) (eql n (plist-get c :call)))))
         (about (nth (or index 0) calls)))
    (if (overlay-get mark 'replique-debug-edited)
        (propertize
         (format "  ◂ call %s of %s, before the edit - %s records it again"
                 (1+ (or index 0)) (length calls)
                 (substitute-command-keys "\\[replique-debug-record]"))
         'face 'replique-debug-recorded-edited)
      (propertize
       (format "  ◂ call %s of %s%s · %s%s%s"
               (1+ (or index 0)) (length calls)
               (if (overlay-get mark 'replique-debug-call) "" ", the last")
               (or (plist-get about :thread) "?")
               (if-let* ((at (plist-get about :at)))
                   (format-time-string " · %T" (/ at 1000.0))
                 "")
               (replique-debug--iteration-text mark))
       'face 'replique-debug-recorded-call))))

(defun replique-debug--show-recorded (mark recorded)
  "Show RECORDED, what the process said the unit MARK marks recorded."
  (overlay-put mark 'replique-debug-calls (plist-get recorded :calls))
  (overlay-put mark 'replique-debug-shown-call (plist-get recorded :call))
  (overlay-put mark 'replique-debug-shown-iteration (plist-get recorded :iteration))
  (overlay-put mark 'replique-debug-recorded-said recorded)
  (replique-debug--draw-recorded mark)
  (replique-debug--view-follows mark)
  (replique-debug--repl-follows mark))

(defun replique-debug--view-source (mark)
  "Return what the view of MARK shows of the call shown, to browse."
  (when-let* ((of (overlay-get mark 'replique-debug-view-of)))
    (list :debug (append (plist-get of :form)
                         (list :call (overlay-get mark 'replique-debug-shown-call))
                         (when-let* ((it (overlay-get mark 'replique-debug-shown-iteration)))
                           (list :iteration it))))))

(defun replique-debug--view-title (mark)
  "Return the title of the view of MARK, of the call shown."
  (let ((call (overlay-get mark 'replique-debug-shown-call))
        (calls (overlay-get mark 'replique-debug-calls)))
    (format "%s, call %s of %s%s"
            (plist-get (overlay-get mark 'replique-debug-view-of) :text)
            (1+ (or (seq-position calls call (lambda (c n) (eql n (plist-get c :call))))
                    (1- (length calls))))
            (length calls)
            (if-let* ((it (overlay-get mark 'replique-debug-shown-iteration)))
                (format ", iteration %s" (plist-get it :index))
              ""))))

(defun replique-debug--view-follows (mark)
  "Make the view of a value MARK recorded show it in the call shown now.
A view of every call stays as it is.  Not shown if it was not."
  (when-let* ((view (overlay-get mark 'replique-debug-view))
              ((buffer-live-p view))
              (source (replique-debug--view-source mark))
              ((not (equal source (buffer-local-value 'replique-inspect--source view)))))
    (replique-inspect-show
     (overlay-get mark 'replique-debug-process) (overlay-get mark 'replique-debug-keys)
     source (replique-debug--view-title mark) view t)))

(defun replique-debug--draw-recorded (mark)
  "Show what the unit MARK marks recorded, as `replique-debug-recorded-shown' says.

Each form recorded is an overlay, which what is at point is found by,
whatever is written - and which an edit of its text takes away, see
`replique-debug--edited'."
  (replique-debug--forget-recorded mark)
  (with-current-buffer (overlay-buffer mark)
    (let* ((recorded (overlay-get mark 'replique-debug-recorded-said))
           (forms nil))
      (dolist (value (plist-get recorded :values))
        (when (equal "enter" (plist-get value :kind))
          (setq forms (append (replique-debug--entered mark value) forms)))
        (when (and (plist-get value :end-line)
                   (not (equal "enter" (plist-get value :kind))))
          (let ((start (replique-debug--here mark (plist-get value :line)
                                             (plist-get value :column)))
                (end (replique-debug--here mark (plist-get value :end-line)
                                           (plist-get value :end-column))))
            (when (and (<= (overlay-start mark) start end (overlay-end mark))
                       (not (replique-debug--literal-p
                             (buffer-substring-no-properties start end))))
              (let ((o (make-overlay start end)))
                ;; a local is said by its name, as a param is
                (overlay-put o 'replique-debug-recorded
                             (append
                              (when (equal "local" (plist-get value :kind))
                                (list :param (buffer-substring-no-properties start end)))
                              ;; evaluated in the iteration shown, maybe, but
                              ;; not kept
                              (when (plist-get value :not-kept)
                                (list :value "…"))
                              value))
                (push o forms))))))
      (dolist (o forms)
        (overlay-put o 'evaporate t)
        (overlay-put o 'modification-hooks (list #'replique-debug--form-edited)))
      ;; what the values are of, to tell an edit undone by
      (overlay-put mark 'replique-debug-text
                   (buffer-substring-no-properties (overlay-start mark) (overlay-end mark)))
      (replique-debug--write-recorded mark forms))))

(defun replique-debug--write-recorded (mark forms)
  "Write the values of FORMS, recorded by the unit MARK marks, over its code.

As `replique-debug-recorded-shown' says, and faded where the unit was
edited since."
  (with-current-buffer (overlay-buffer mark)
    (let ((edited (overlay-get mark 'replique-debug-edited))
          (lines (make-hash-table))
          (shown nil)
          (first-line (line-number-at-pos (overlay-start mark))))
      (mapc #'delete-overlay (seq-difference (overlay-get mark 'replique-debug-shown) forms))
      (dolist (o forms)
        (let ((value (overlay-get o 'replique-debug-recorded)))
          (overlay-put o 'replique-debug-old edited)
          (overlay-put o 'after-string
                       (when (eq 'every replique-debug-recorded-shown)
                         (replique-debug--recorded-text (plist-get value :value)
                                                        (plist-get value :times)
                                                        nil edited)))))
      ;; what is written at the end of a line, by line
      (when (eq 'line replique-debug-recorded-shown)
        (dolist (o forms)
          (let ((line (line-number-at-pos (overlay-start o))))
            (when (= line (line-number-at-pos (overlay-end o)))
              (push o (gethash line lines))))))
      (maphash (lambda (line on-it)
                 (puthash line
                          (mapconcat (lambda (o)
                                       (let ((value (overlay-get o 'replique-debug-recorded)))
                                         (replique-debug--recorded-text
                                          (plist-get value :value) (plist-get value :times)
                                          (plist-get value :param) edited)))
                                     (replique-debug--outermost on-it)
                                     "")
                          lines))
               lines)
      (when (plist-get (overlay-get mark 'replique-debug-recorded-said) :call)
        (puthash first-line
                 (concat (gethash first-line lines "") (replique-debug--call-text mark))
                 lines))
      (maphash (lambda (line text)
                 (let* ((eol (save-excursion
                               (goto-char (point-min))
                               (forward-line (1- line))
                               (line-end-position)))
                        (o (make-overlay eol eol)))
                   ;; the cursor stays at the end of the line, before it
                   (put-text-property 0 1 'cursor t text)
                   (overlay-put o 'after-string text)
                   (push o shown)))
               lines)
      (overlay-put mark 'replique-debug-shown (append forms shown))
      (replique-debug--follow-point))))

(defun replique-debug--recorded (process unit)
  "Show again what the unit UNIT of PROCESS recorded, which it says changed.

Where the last call is shown: a call gone back to stays."
  (when-let* ((mark (gethash (cons process unit) replique-debug--units)))
    (unless (or (overlay-get mark 'replique-debug-call)
                (overlay-get mark 'replique-debug-edited))
      (replique-debug--ask-recorded mark))))

(defun replique-debug-cycle-recorded ()
  "Show recorded values the next way: one per line, after every form, none.

See `replique-debug-recorded-shown'."
  (interactive)
  (setq replique-debug-recorded-shown
        (pcase replique-debug-recorded-shown ('line 'every) ('every 'none) (_ 'line)))
  (maphash (lambda (_ mark)
             (when (and (overlay-buffer mark)
                        (overlay-get mark 'replique-debug-recorded-said))
               (if (overlay-get mark 'replique-debug-edited)
                   (replique-debug--write-recorded mark (replique-debug--forms-of mark))
                 (replique-debug--draw-recorded mark))))
           replique-debug--units)
  (message "replique: %s"
           (pcase replique-debug-recorded-shown
             ('line "a recorded value per line")
             ('every "a recorded value after every form")
             (_ "no recorded value written - eldoc says the one at point"))))

;;; The recorded form at point

(defvar-local replique-debug--at-point nil
  "The overlay highlighting the recorded form point is at.")

(defun replique-debug--recorded-form-at (pos)
  "Return the overlay of the recorded form at POS, or nil.

The one ending at POS - just written, the form `eval-last-sexp' would
take - or else the innermost one POS is in."
  (let* ((forms (seq-filter (lambda (o) (overlay-get o 'replique-debug-recorded))
                            (overlays-in (max (point-min) (1- pos))
                                         (min (point-max) (1+ pos)))))
         (innermost (lambda (os)
                      (car (sort os (lambda (a b) (> (overlay-start a) (overlay-start b))))))))
    (or (funcall innermost (seq-filter (lambda (o) (= pos (overlay-end o))) forms))
        (funcall innermost (seq-filter (lambda (o) (and (<= (overlay-start o) pos)
                                                        (< pos (overlay-end o))))
                                       forms)))))

(defun replique-debug--follow-point ()
  "Highlight the recorded form point is at, the one eldoc says the value of."
  (let ((form (replique-debug--recorded-form-at (point))))
    (cond
     (form
      (if (and replique-debug--at-point (overlay-buffer replique-debug--at-point))
          (move-overlay replique-debug--at-point (overlay-start form) (overlay-end form))
        (setq replique-debug--at-point (make-overlay (overlay-start form) (overlay-end form)))
        (overlay-put replique-debug--at-point 'priority -50))
      ;; the background alone, the theme's as it is now
      (overlay-put replique-debug--at-point 'face
                   (list :background (face-background 'replique-debug-recorded-at-point nil t))))
     (replique-debug--at-point
      (delete-overlay replique-debug--at-point)))))

(defun replique-debug-eldoc (&rest _ignored)
  "Return what the recorded form at point evaluated to, in the call shown.

For `eldoc-documentation-functions', before what says what a call takes:
in a definition that records, which is read rather than written - written
in, it records nothing to show - this is what there is to know.  Not cut,
as what is written after the form is."
  (when-let* ((form (replique-debug--recorded-form-at (point)))
              (value (overlay-get form 'replique-debug-recorded)))
    (concat (propertize "⇒ " 'face 'replique-debug-recorded)
            (replique-clojure-fontify-string (plist-get value :value))
            (let ((times (plist-get value :times)))
              (if (and times (> times 1))
                  (propertize (format " ×%s" times) 'face 'replique-debug-recorded)
                ""))
            (if (overlay-get form 'replique-debug-old)
                (propertize "  - before the edit" 'face 'replique-debug-recorded-edited)
              ""))))

(defun replique-debug--recording-here ()
  "Say what is recorded in the current buffer, by eldoc, and highlight it."
  (add-hook 'eldoc-documentation-functions #'replique-debug-eldoc -50 t)
  (add-hook 'post-command-hook #'replique-debug--follow-point nil t))

(defun replique-debug--forms-of (mark)
  "Return the overlays of the forms the unit MARK shows the values of."
  (seq-filter (lambda (o) (and (overlay-buffer o) (overlay-get o 'replique-debug-recorded)))
              (overlay-get mark 'replique-debug-shown)))

(defun replique-debug--form-edited (form after &rest _)
  "Say that the text of FORM, a form recorded, is being edited.

Before the change - AFTER nil - so that every form it touches is known by
the time the definition's own hook writes them again - see
`replique-debug--edited'."
  (unless after
    (overlay-put form 'replique-debug-changed t)))

(defun replique-debug--mark-edited (mark edited)
  "Mark the unit MARK as EDITED since it was recorded, or not."
  (overlay-put mark 'replique-debug-edited edited)
  (overlay-put mark 'before-string
               (propertize "*" 'display
                           (list 'left-fringe 'filled-square
                                 (if edited 'replique-debug-recorded-edited
                                   'replique-debug-instrumented))))
  (overlay-put mark 'help-echo
               (if edited
                   (substitute-command-keys
                    "Edited since it was recorded - \\[replique-debug-record] records it again")
                 "Recorded - M-x replique-debug-uninstrument")))

(defun replique-debug--edited (mark after &rest _)
  "Fade what the unit MARK marks recorded: its code was edited.

What was recorded is of the code as it was sent.  The values of the forms
edited go: they are not of anything written any more.  The others stay,
faded, as what they were before the edit.  Edited back to what was
recorded, the values are shown as they were.  The process records it
still, until it is evaluated again or `replique-debug-uninstrument' lets
it go - but what it records is not shown, being of the code before.

After the change only - AFTER non-nil - when the forms it touched are
known, see `replique-debug--form-edited'."
  (when (and after (overlay-buffer mark) (overlay-get mark 'replique-debug-recorded-said))
    (if (equal (buffer-substring-no-properties (overlay-start mark) (overlay-end mark))
               (overlay-get mark 'replique-debug-text))
        (when (overlay-get mark 'replique-debug-edited)
          (replique-debug--mark-edited mark nil)
          (replique-debug--draw-recorded mark))
      (unless (overlay-get mark 'replique-debug-edited)
        (replique-debug--mark-edited mark t))
      (let ((forms nil))
        (dolist (o (replique-debug--forms-of mark))
          (if (overlay-get o 'replique-debug-changed)
              (delete-overlay o)
            (push o forms)))
        (replique-debug--write-recorded mark (nreverse forms))))))

;;;###autoload
(defun replique-debug-record ()
  "Evaluate the top level form around point, to record what it does.

What each of its forms evaluates to is kept, call by call - the last ones
- and written after the form: the values of the last call, as calls come,
or of the one gone back to with \[replique-debug-recorded-before].  Each
is a value to browse - \[replique-debug-inspect-recorded].  Nothing stops,
and the process does not need the debugger, only the compiler of
replique-clj.

In a ClojureScript buffer it is recorded in the runtime of the repl it is
sent to - the node program, or the page - and let go of when that page
is reloaded, or its namespace compiled again.

Recorded until `replique-debug-uninstrument', or until it is evaluated
again the ordinary way.  Once its code is edited, the values of the forms
edited go, and the others are faded: they are of code as it was before -
see `replique-debug--edited'."
  (interactive)
  (replique-eval--send (list (replique-eval--defun-at-point)) 'record))

(defun replique-debug--recording-at-point (&optional edited-too)
  "Return the mark of the unit around point that records.

Or say there is none, or - unless EDITED-TOO - that its code was edited
since."
  (let ((mark (or (seq-find (lambda (o) (overlay-get o 'replique-debug-record))
                            (replique-debug--marks-in (point) (1+ (point))))
                  (user-error "Nothing here is recorded"))))
    (when (and (overlay-get mark 'replique-debug-edited) (not edited-too))
      (user-error "%s" (substitute-command-keys
                        "Edited since it was recorded - \\[replique-debug-record] records it again")))
    mark))

(defun replique-debug--go-to-call (offset)
  "Show the call OFFSET calls after the one shown, of the unit at point."
  (let* ((mark (replique-debug--recording-at-point))
         (calls (overlay-get mark 'replique-debug-calls))
         (shown (overlay-get mark 'replique-debug-shown-call))
         (index (+ offset (or (seq-position calls shown
                                            (lambda (c n) (eql n (plist-get c :call))))
                              (1- (length calls))))))
    (when (and calls (<= 0 index) (< index (length calls)))
      ;; an iteration is of a call
      (overlay-put mark 'replique-debug-iteration nil)
      (overlay-put mark 'replique-debug-iteration-pinned nil))
    (cond
     ((null calls) (user-error "Not called yet"))
     ((< index 0) (user-error "No call before this one is kept"))
     ((>= index (length calls)) (user-error "This is the last call"))
     ;; the last call is followed
     ((= index (1- (length calls))) (replique-debug--ask-recorded mark))
     (t (replique-debug--ask-recorded mark (plist-get (nth index calls) :call))))))

;;;###autoload
(defun replique-debug-recorded-before (&optional n)
  "Show what the definition at point recorded in the call before - N before."
  (interactive "p")
  (replique-debug--go-to-call (- (or n 1))))

;;;###autoload
(defun replique-debug-recorded-after (&optional n)
  "Show what the definition at point recorded in the call after - N after.

The last call is followed: shown as the next calls come."
  (interactive "p")
  (replique-debug--go-to-call (or n 1)))

;;; An iteration of a loop

(defun replique-debug--in-span-p (span line column)
  "Return non-nil when LINE and COLUMN are inside SPAN.
SPAN is a plist of :line :column :end-line :end-column, the end just past."
  (let ((here (list line column)))
    (and (not (version-list-< here (list (plist-get span :line) (plist-get span :column))))
         (version-list-< here (list (plist-get span :end-line) (plist-get span :end-column))))))

(defun replique-debug--loop-of (mark site)
  "Return the loop of the call shown by MARK whose name is bound at SITE, or nil."
  (seq-find (lambda (l) (eql site (plist-get l :site)))
            (plist-get (overlay-get mark 'replique-debug-recorded-said) :loops)))

(defun replique-debug--loop-at (mark pos)
  "Return the loop POS is in, of the call the unit MARK marks shows.

The loop of the iteration shown, where POS is in it, or else the
innermost one - as the process said it, see `:debug-recorded' - or nil."
  (let* ((place (replique-debug--origin-of mark pos))
         (in (seq-filter (lambda (l) (replique-debug--in-span-p l (car place) (cdr place)))
                         (plist-get (overlay-get mark 'replique-debug-recorded-said) :loops)))
         (shown (plist-get (overlay-get mark 'replique-debug-iteration) :site)))
    (or (seq-find (lambda (l) (eql shown (plist-get l :site))) in)
        (car (last in)))))

(defun replique-debug--kept-indices (loop)
  "Return the indices of the iterations of LOOP kept, from 1, in order."
  (seq-mapcat (lambda (range) (number-sequence (car range) (cadr range)))
              (plist-get loop :kept)))

(defun replique-debug--loop-name (mark loop)
  "Return what LOOP, of the code the unit MARK marks, is called.
Its head and the name it binds each time round, as written."
  (with-current-buffer (overlay-buffer mark)
    (let* ((start (replique-debug--here mark (plist-get loop :line) (plist-get loop :column)))
           (head (save-excursion
                   (goto-char (1+ start))
                   (thing-at-point 'symbol t)))
           (bound (seq-find (lambda (v) (eql (plist-get loop :site) (plist-get v :site)))
                            (plist-get (overlay-get mark 'replique-debug-recorded-said) :values)))
           (name (when bound
                   (buffer-substring-no-properties
                    (replique-debug--here mark (plist-get bound :line) (plist-get bound :column))
                    (replique-debug--here mark (plist-get bound :end-line)
                                          (plist-get bound :end-column))))))
      (string-join (delq nil (list head name)) " "))))

(defun replique-debug--iteration-text (mark)
  "Return which iteration the unit MARK marks shows, as the call says it, or \"\"."
  (if-let* ((it (overlay-get mark 'replique-debug-shown-iteration))
            (loop (replique-debug--loop-of mark (plist-get it :site))))
      (format " · %s, iteration %s of %s"
              (replique-debug--loop-name mark loop)
              (plist-get it :index) (plist-get loop :iterations))
    ""))

(defun replique-debug--show-iteration (mark site index)
  "Show what the unit MARK marks recorded in an iteration of a loop.
The iteration INDEX of the loop whose name is bound at SITE - in the call
shown, which stays shown as calls come, until the whole call is shown
again."
  (unless (overlay-get mark 'replique-debug-call)
    (overlay-put mark 'replique-debug-call (overlay-get mark 'replique-debug-shown-call))
    (overlay-put mark 'replique-debug-iteration-pinned t))
  (overlay-put mark 'replique-debug-iteration (list :site site :index index))
  (replique-debug--ask-recorded mark (overlay-get mark 'replique-debug-call)))

(defun replique-debug--recorded-loop-at-point ()
  "Return the mark of the recorded definition at point and the loop at point.
As (MARK . LOOP), or say there is none."
  (let* ((mark (replique-debug--recording-at-point))
         (loop (or (replique-debug--loop-at mark (point))
                   (user-error "No loop here went round in the call shown"))))
    (cons mark loop)))

(defun replique-debug--go-to-iteration (offset)
  "Show the iteration OFFSET iterations after the one shown, of the loop at point.
From the first one, or the last, where none of it is shown."
  (pcase-let* ((`(,mark . ,loop) (replique-debug--recorded-loop-at-point))
               (indices (or (replique-debug--kept-indices loop)
                            (user-error "No iteration of this loop is kept")))
               (it (overlay-get mark 'replique-debug-iteration))
               (current (and (eql (plist-get it :site) (plist-get loop :site))
                             (seq-position indices (plist-get it :index))))
               (position (cond
                          (current (+ current offset))
                          ((> offset 0) (1- offset))
                          (t (+ (length indices) offset)))))
    (cond
     ((< position 0) (user-error "This is the first iteration kept"))
     ((>= position (length indices)) (user-error "This is the last iteration kept"))
     (t
      (let ((index (nth position indices)))
        (when (and current (> (abs (- index (nth current indices))) (abs offset)))
          (message "replique: iterations %s to %s are not kept - the first ones and the last ones are"
                   (1+ (min index (nth current indices)))
                   (1- (max index (nth current indices)))))
        (replique-debug--show-iteration mark (plist-get loop :site) index))))))

;;;###autoload
(defun replique-debug-next-iteration (&optional n)
  "Show what the loop at point did in its next iteration - N iterations on.

Of a recorded definition, in the call shown: every form the loop goes
round shows what it evaluated to in that iteration, and a form it did not
evaluate then shows nothing.  The rest shows the whole call.  The first
iteration, where none of the loop is shown.  \[replique-debug-whole-call]
shows the whole call again.  The first iterations of a loop are kept, and
the last ones - see `:debug-recorded'."
  (interactive "p")
  (replique-debug--go-to-iteration (or n 1)))

;;;###autoload
(defun replique-debug-previous-iteration (&optional n)
  "Show what the loop at point did in its iteration before - N before.
The last iteration, where none of the loop is shown.  See
`replique-debug-next-iteration'."
  (interactive "p")
  (replique-debug--go-to-iteration (- (or n 1))))

;;;###autoload
(defun replique-debug-whole-call ()
  "Show what the definition at point recorded in the whole call, not an iteration."
  (interactive)
  (let ((mark (replique-debug--recording-at-point)))
    (unless (overlay-get mark 'replique-debug-iteration)
      (user-error "The whole call is shown"))
    (overlay-put mark 'replique-debug-iteration nil)
    (replique-debug--ask-recorded mark (unless (overlay-get mark 'replique-debug-iteration-pinned)
                                         (overlay-get mark 'replique-debug-call)))
    (overlay-put mark 'replique-debug-iteration-pinned nil)))

(defvar replique-debug--find-history nil
  "What `replique-debug-find-iteration' was asked to find.")

;;;###autoload
(defun replique-debug-find-iteration (code &optional backward)
  "Show the first iteration of the loop at point where CODE is true.

CODE is evaluated with the locals in scope at point bound to what they
were in each iteration, as `replique-debug-eval-in-call' binds them - after
the iteration shown, or from the first one.  With BACKWARD, interactively
the prefix argument, the iteration before it, or the last.  The iterations
kept only."
  (interactive (list (read-string (if current-prefix-arg "Last iteration where: "
                                    "First iteration where: ")
                                  nil 'replique-debug--find-history)
                     current-prefix-arg))
  (pcase-let* ((`(,mark . ,loop) (replique-debug--recorded-loop-at-point))
               (place (replique-debug--origin-of mark (point)))
               (it (overlay-get mark 'replique-debug-iteration))
               (from (and (eql (plist-get it :site) (plist-get loop :site))
                          (plist-get it :index))))
    (replique-process-request
     (overlay-get mark 'replique-debug-process)
     (append (list :op :debug-find-iteration
                   :unit (overlay-get mark 'replique-debug-unit)
                   :site (plist-get loop :site)
                   :line (car place) :column (cdr place)
                   :code code)
             (when-let* ((c (overlay-get mark 'replique-debug-shown-call))) (list :call c))
             (when from (list :from from))
             (when backward (list :backward t)))
     (lambda (frame)
       (cond
        ((equal "error" (plist-get frame :tag))
         (message "replique: %s" (plist-get frame :message)))
        ((plist-get frame :index)
         (when (overlay-buffer mark)
           (replique-debug--show-iteration mark (plist-get loop :site) (plist-get frame :index))))
        (t (message "replique: no iteration kept %sholds that%s"
                    (cond ((null from) "")
                          (backward (format "before iteration %s " from))
                          (t (format "after iteration %s " from)))
                    (if-let* ((error (plist-get frame :error)))
                        (format " - it threw: %s" error)
                      ""))))))))

;;;###autoload
(defun replique-debug-inspect-iterations (&optional table)
  "Browse what each iteration of the loop at point did, in the call shown.

On a form evaluated again each time round, what it evaluated to last in
each iteration it was evaluated in, by the index of the iteration.
Elsewhere in the loop, or with TABLE, interactively the prefix argument,
a row per iteration kept, of the names it binds each time round and the
ones bound in it - what each was bound to then - and a column of the
form at point, named its code."
  (interactive "P")
  (pcase-let* ((`(,mark . ,loop) (replique-debug--recorded-loop-at-point))
               (name (replique-debug--loop-name mark loop))
               (calls (overlay-get mark 'replique-debug-calls))
               (call (overlay-get mark 'replique-debug-shown-call))
               (o (replique-debug--recorded-form-at (point)))
               (form (and o (overlay-get o 'replique-debug-recorded)))
               ;; a form in the loop, not the loop itself, nor a name it
               ;; binds, which has a column, nor a param of a fn entered
               (place (and o (replique-debug--origin-of mark (overlay-start o))))
               (in-loop (and form
                             (not (plist-get form :arg))
                             (not (equal "local" (plist-get form :kind)))
                             (replique-debug--in-span-p loop (car place) (cdr place))
                             (not (equal place (cons (plist-get loop :line)
                                                     (plist-get loop :column))))))
               (only (and in-loop (not table))))
    (replique-inspect-show
     (overlay-get mark 'replique-debug-process) (overlay-get mark 'replique-debug-keys)
     (list :debug (append (list :loop (plist-get loop :site))
                          (when call (list :call call))
                          (when in-loop (list :site (plist-get form :site)))
                          (when only (list :only t))))
     (format "%s, every iteration%s, call %s of %s"
             (if only (replique-debug--form-text o) name)
             (if only (format " of %s" name) "")
             (1+ (or (seq-position calls call (lambda (c n) (eql n (plist-get c :call))))
                     (1- (length calls))))
             (length calls)))))

;;;###autoload
(defun replique-debug--unit-name (mark)
  "Return the name the definition MARK marks defines, or where it is."
  (with-current-buffer (overlay-buffer mark)
    (save-excursion
      (goto-char (overlay-start mark))
      (if (looking-at (concat "(\\s-*\\(?:\\sw\\|\\s_\\)+[ \t\n]+"
                              "\\(?:\\^\\(?:\\sw\\|\\s_\\)+[ \t\n]+\\)*"
                              "\\(\\(?:\\sw\\|\\s_\\)+\\)"))
          (match-string-no-properties 1)
        (format "line %s" (line-number-at-pos))))))

(defun replique-debug--form-text (o)
  "Return the code of the recorded form O, on one line and shortened."
  (truncate-string-to-width
   (replace-regexp-in-string
    "[ \t\n]+" " " (buffer-substring-no-properties (overlay-start o) (overlay-end o)))
   30 nil nil "…"))

(defun replique-debug-inspect-recorded (&optional every-call)
  "Browse what the form at point evaluated to, in the call shown.

The value itself, rather than its printing.  With EVERY-CALL,
interactively the prefix argument, what it evaluated to in each call kept,
one value per call.

In the one view the definition has, made a view of this value whatever
it showed before, and killed when the definition records no more.  It
follows the call shown: the call before or after, or the last as calls
come, shows the value in that call there too - `:not-recorded' where the
form was not evaluated."
  (interactive "P")
  (let* ((mark (replique-debug--recording-at-point t))
         (o (replique-debug--recorded-form-at (point)))
         (value (or (and o (overlay-get o 'replique-debug-recorded))
                    (user-error "No form recorded at point")))
         (text (replique-debug--form-text o))
         (form (append (list :site (plist-get value :site))
                       (when-let* ((arg (plist-get value :arg))) (list :arg arg))))
         (view (overlay-get mark 'replique-debug-view)))
    (overlay-put mark 'replique-debug-view-of
                 (unless every-call (list :form form :text text)))
    (overlay-put
     mark 'replique-debug-view
     (replique-inspect-show
      (overlay-get mark 'replique-debug-process) (overlay-get mark 'replique-debug-keys)
      (if every-call (list :debug form) (replique-debug--view-source mark))
      (if every-call (format "%s, every call" text) (replique-debug--view-title mark))
      (if (buffer-live-p view)
          view
        (generate-new-buffer (format "*replique-recorded %s*"
                                     (replique-debug--unit-name mark))))))))

;;; A repl in a recorded call

(defun replique-debug--call-label (process call)
  "Return what the prompt of a repl of PROCESS evaluating in CALL says.
CALL is (:unit N :call C ...), as the prompt said it: the definition, and
the call - the last one, without C."
  (let* ((mark (gethash (cons process (plist-get call :unit)) replique-debug--units))
         (at (and mark (plist-get call :call)
                  (plist-get (seq-find (lambda (c) (eql (plist-get c :call) (plist-get call :call)))
                                       (overlay-get mark 'replique-debug-calls))
                             :at))))
    (format "%s, %s%s"
            (if mark (replique-debug--unit-name mark) "gone")
            (cond ((null (plist-get call :call)) "last call")
                  (at (format-time-string "call %T" (/ at 1000.0)))
                  (t "call"))
            (if-let* ((it (plist-get call :iteration)))
                (format ", iteration %s" (plist-get it :index))
              ""))))

(defun replique-debug--call-directive (call)
  "Return the directive that points a repl at CALL, (:unit N :call C ...)."
  (format "#replique/call {:unit %s%s :line %s :column %s%s}"
          (plist-get call :unit)
          (if (plist-get call :call) (format " :call %s" (plist-get call :call)) "")
          (plist-get call :line) (plist-get call :column)
          (if-let* ((it (plist-get call :iteration)))
              (format " :iteration {:site %s :index %s}"
                      (plist-get it :site) (plist-get it :index))
            "")))

(defun replique-debug--repl-follows (mark)
  "Point the repl evaluating in a call of the unit MARK marks at the call shown.
At the place of the code it was at.  Not a repl pointed elsewhere since."
  (when-let* ((repl (overlay-get mark 'replique-debug-repl))
              ((replique-repl-live-p repl))
              (in (replique-repl--in-call repl))
              (call (append (list :unit (overlay-get mark 'replique-debug-unit))
                            (when-let* ((c (overlay-get mark 'replique-debug-call)))
                              (list :call c))
                            (list :line (plist-get in :line) :column (plist-get in :column))
                            (when-let* ((it (overlay-get mark 'replique-debug-iteration)))
                              (list :iteration it)))))
    (unless (equal call in)
      (replique-repl-send-directive repl (replique-debug--call-directive call)))))

;;;###autoload
(defun replique-debug-eval-in-call ()
  "Evaluate in the call shown of the recorded definition at point.

From a repl of its own, with the locals in scope at point bound to what
they were in that call: its params, to what it was called with, and the
locals of its lets, to what they were bound to - recorded, not evaluated
again.  The locals of a loop, a doseq or a for, to what they were bound
to last - or in the iteration shown, see `replique-debug-next-iteration',
which it follows too.  A local whose value is not known is not bound: of
a form that was not evaluated in that call, or that iteration.  In the
namespace of the code, on the repl's own thread, and the ordinary way
otherwise: a recorded function
called there records that call too.

It is the repl the commands act on while it is there, so what a Clojure
buffer sends goes to it too: `\[replique-eval-last-sexp]' on a form of the
definition evaluates it with the values it had.

It follows the call shown - see `replique-debug-recorded-before' - the last
one while the last one is followed.  Asked from another place of the code,
it moves there.  It goes when the definition records no more - and stays
when it is recorded again, in the calls of the definition as it is now.

A ClojureScript definition is evaluated in from a ClojureScript repl on the
runtime it was recorded in, which keeps the values - and binds a local
whose value is not known to :replique/not-recorded."
  (interactive)
  (let* ((mark (replique-debug--recording-at-point))
         (process (overlay-get mark 'replique-debug-process))
         (place (replique-debug--origin-of mark (point)))
         (call (append (list :unit (overlay-get mark 'replique-debug-unit))
                       (when-let* ((c (overlay-get mark 'replique-debug-call)))
                         (list :call c))
                       (list :line (car place) :column (cdr place))
                       (when-let* ((it (overlay-get mark 'replique-debug-iteration)))
                         (list :iteration it))))
         (keys (overlay-get mark 'replique-debug-keys))
         (repl (overlay-get mark 'replique-debug-repl)))
    (unless (overlay-get mark 'replique-debug-calls)
      (user-error "Not called yet"))
    (if (replique-repl-live-p repl)
        (progn
          (unless (equal call (replique-repl--in-call repl))
            (replique-repl-send-directive repl (replique-debug--call-directive call)))
          (setq replique-current-repl repl)
          (pop-to-buffer (replique-repl--buffer repl)))
      (let ((name (format "*replique-recorded %s repl*" (replique-debug--unit-name mark))))
        (setq repl (replique-repl--open process (plist-get keys :dialect) (plist-get keys :target)
                                        nil nil nil call))
        ;; Renamed after it is opened, and so kept - see
        ;; `replique-debug-eval-in-frame'
        (with-current-buffer (replique-repl--buffer repl)
          (rename-buffer name t))
        (overlay-put mark 'replique-debug-repl repl)))))

;;; A buffer per stopped thread

(defvar-local replique-debug--process nil
  "The process the thread of the buffer is a thread of.")

(defvar-local replique-debug--thread nil
  "The id of the thread the buffer is about.")

(defvar-local replique-debug--pause nil
  "What the process said when the thread stopped, nil while it runs.")

(defvar-local replique-debug--frames nil
  "The frames of the stopped thread, innermost first, as the process said.")

(defvar-local replique-debug--host-frames nil
  "Whether the frames that are not Clojure functions are shown.")

(defun replique-debug--buffer-of (process thread)
  "Return the buffer of the thread THREAD of PROCESS, or nil."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'replique-debug-mode)
                     (eq process replique-debug--process)
                     (equal thread replique-debug--thread))))
            (buffer-list)))

(defun replique-debug--repl-of (process connection)
  "Return the repl of PROCESS whose connection is CONNECTION, or nil."
  (when connection
    (seq-find (lambda (repl)
                (equal connection (replique-conn--id (replique-repl--conn repl))))
              (replique-process--repls process))))

(defun replique-debug--place (found)
  "Return where FOUND is, as a short text."
  (let ((file (or (plist-get found :entry) (plist-get found :file)
                  (plist-get found :source))))
    (concat (if file (file-name-nondirectory file) "?")
            (when-let* ((line (plist-get found :line))) (format ":%s" line)))))

(defun replique-debug--frame-line (frame)
  "Insert the line of FRAME."
  (let ((start (point))
        (fn (plist-get frame :fn)))
    (insert (format "%3d  " (plist-get frame :index)))
    (insert (if fn
                (propertize fn 'face 'font-lock-function-name-face)
              (propertize (format "%s.%s" (plist-get frame :class) (plist-get frame :method))
                          'face 'replique-debug-host-frame)))
    (insert "  " (propertize (replique-debug--place frame) 'face 'replique-note) "\n")
    (put-text-property start (point) 'replique-debug-frame frame)))

(defun replique-debug--render ()
  "Write what the buffer shows, leaving point on the frame it was on."
  (let ((inhibit-read-only t)
        (at (get-text-property (point) 'replique-debug-frame))
        (hidden 0))
    (erase-buffer)
    (cond
     ((null replique-debug--pause)
      (insert (propertize "Running.\n" 'face 'replique-note)))
     ((null replique-debug--frames)
      (insert (propertize "…\n" 'face 'replique-note)))
     (t
      (dolist (frame replique-debug--frames)
        (if (or replique-debug--host-frames (plist-get frame :fn)
                ;; the frame that stopped is shown whatever it is
                (eql 0 (plist-get frame :index)))
            (replique-debug--frame-line frame)
          (cl-incf hidden)))
      (when (> hidden 0)
        (insert (propertize (format "     %s frames of the host - j shows them\n" hidden)
                            'face 'replique-note)))))
    (goto-char (point-min))
    (when at
      (when-let* ((found (text-property-search-forward
                          'replique-debug-frame (plist-get at :index)
                          (lambda (index frame) (eql index (plist-get frame :index))))))
        (goto-char (prop-match-beginning found))))
    (force-mode-line-update)))

(defun replique-debug--header ()
  "Return the header line of the buffer."
  (let* ((pause replique-debug--pause)
         (repl (and pause (replique-debug--repl-of replique-debug--process
                                                   (plist-get pause :connection)))))
    (string-join
     (delq nil
           (list (propertize (format "thread %s"
                                     (or (plist-get pause :name) replique-debug--thread))
                             'face 'bold)
                 (if pause
                     (propertize (format "stopped at %s%s" (replique-debug--place pause)
                                         (if (plist-get pause :break) ", a breakpoint" ""))
                                 'face 'replique-exception)
                   "running")
                 (when-let* ((value (plist-get pause :value)))
                   (replique-debug--painted "⇒ " (replique-debug--value-text value)
                                            'replique-debug-value))
                 (when repl
                   (format "evaluating for %s" (buffer-name (replique-repl--buffer repl))))))
     "  ·  ")))

(defun replique-debug--ask-frames ()
  "Ask the process for the frames of the thread of the buffer."
  (let ((buffer (current-buffer))
        (pause replique-debug--pause))
    (when pause
      (replique-process-request
       replique-debug--process
       (list :op :debug-frames :thread replique-debug--thread)
       (lambda (frame)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             ;; an answer about a stop that is over is not about this one
             (when (eq pause replique-debug--pause)
               (if (equal "error" (plist-get frame :tag))
                   (replique-debug--answered-error frame)
                 (setq replique-debug--frames (plist-get frame :frames))
                 (replique-debug--render))))))))))

(defun replique-debug--answered-error (frame)
  "Say what FRAME, an error, says - and that the thread runs, where it does."
  (when (equal "not-paused" (plist-get frame :error))
    (setq replique-debug--pause nil
          replique-debug--frames nil)
    (replique-debug--render)
    (replique-debug--leave replique-debug--process replique-debug--thread))
  (message "replique: %s" (plist-get frame :message)))

;;; Being told

(defun replique-debug--stopped (process pause)
  "Show that a thread of PROCESS stopped, as PAUSE says."
  (let* ((thread (plist-get pause :thread))
         (for (cons process thread))
         (buffer (or (progn (replique-debug--stay process thread)
                            (replique-debug--buffer-of process thread))
                     (generate-new-buffer (format "*replique-debug %s*"
                                                  (plist-get pause :name))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'replique-debug-mode)
        (replique-debug-mode)
        (setq replique-debug--process process
              replique-debug--thread thread))
      (setq replique-debug--pause pause
            replique-debug--frames nil)
      (replique-debug--render)
      (replique-debug--ask-frames))
    (replique-debug--locals process thread 0 "stopped")
    ;; the value looked at is the value of the stop, where it has one
    (when-let* ((view (replique-debug--view-of process thread t)))
      (if (plist-member pause :value)
          (save-selected-window (replique-debug--value process thread))
        (replique-debug--kill view)))
    (replique-debug--show-place pause for t)
    (setq replique-debug--arrow-thread for)
    (pop-to-buffer buffer)
    (message "%s%s"
             (if-let* ((error (plist-get pause :condition-error)))
                 (format "replique: the condition of the breakpoint failed - %s\n" error)
               "")
             (pcase (plist-get pause :kind)
               ("value" (format "replique: ⇒ %s  - n next, i in, o out, c continue"
                                (replique-debug--value-text (plist-get pause :value))))
               ("enter" (format "replique: thread %s called %s - n steps, c continues"
                                (plist-get pause :name) (replique-debug--place pause)))
               (_ (format "replique: thread %s stopped at %s - c continues"
                          (plist-get pause :name) (replique-debug--place pause)))))))

(defun replique-debug--resumed (process thread)
  "Show that the thread THREAD of PROCESS runs again."
  (when (equal replique-debug--arrow-thread (cons process thread))
    (replique-debug--forget-arrow))
  (when-let* ((buffer (replique-debug--buffer-of process thread)))
    (with-current-buffer buffer
      (setq replique-debug--pause nil
            replique-debug--frames nil)
      (replique-debug--render)))
  (replique-debug--leave process thread))

;;; What goes once a thread goes on

(defvar replique-debug--leaving (make-hash-table :test #'equal)
  "The timers taking away what is shown of a thread, by (PROCESS . THREAD).")

(defun replique-debug--stay (process thread)
  "Keep what is shown of the thread THREAD of PROCESS, which stopped again."
  (when-let* ((timer (gethash (cons process thread) replique-debug--leaving)))
    (cancel-timer timer)
    (remhash (cons process thread) replique-debug--leaving)))

(defun replique-debug--leave (process thread)
  "Take away what is shown of the thread THREAD of PROCESS, which went on.

In `replique-debug-linger' seconds, unless it stops again by then."
  (replique-debug--stay process thread)
  (puthash (cons process thread)
           (run-at-time replique-debug-linger nil #'replique-debug--clear process thread)
           replique-debug--leaving))

(defun replique-debug--kill (buffer)
  "Kill BUFFER, giving the windows it was in back to what they showed."
  (when (buffer-live-p buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (when (and (window-live-p window) (eq buffer (window-buffer window)))
        (quit-restore-window window 'bury)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun replique-debug--stopped-buffers ()
  "Return the buffers of the threads that are stopped."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'replique-debug-mode) replique-debug--pause)))
              (buffer-list)))

(defun replique-debug--clear (process thread)
  "Take away what is shown of the thread THREAD of PROCESS, now.

Its buffer, the view of its locals, the repl evaluating in it, and the
buffers opened only to show where it was - where they were not written in
since."
  (let ((for (cons process thread)))
    (replique-debug--stay process thread)
    (when (equal replique-debug--arrow-thread for)
      (replique-debug--forget-arrow))
    (replique-debug--forget-values for)
    (dolist (opened (gethash for replique-debug--opened))
      (let ((buffer (car opened)))
        (when (and (buffer-live-p buffer)
                   (not (buffer-modified-p buffer))
                   (= (cdr opened) (buffer-chars-modified-tick buffer)))
          (replique-debug--kill buffer))))
    (remhash for replique-debug--opened)
    (mapc #'replique-debug--kill (replique-debug--views-of process thread))
    (replique-debug--kill (replique-debug--frame-repl-buffer process thread))
    (replique-debug--kill (replique-debug--buffer-of process thread))
    (unless (replique-debug--stopped-buffers)
      (replique-debug--kill (get-buffer "*replique-value*")))))

(defun replique-debug--forgotten (process)
  "Take away what is shown of the threads of PROCESS, which is gone."
  (let ((threads nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'replique-debug-mode) (eq process replique-debug--process))
          (push replique-debug--thread threads))))
    (maphash (lambda (for _) (when (eq process (car for)) (push (cdr for) threads)))
             replique-debug--opened)
    (dolist (thread (delete-dups threads))
      (replique-debug--clear process thread))
    (maphash (lambda (for o)
               (when (eq process (car for))
                 (replique-debug--unmark o)
                 (remhash for replique-debug--units)))
             replique-debug--units)))

(add-hook 'replique-process-forgotten-functions #'replique-debug--forgotten)

(defun replique-debug--event (process frame)
  "Handle FRAME, an event of PROCESS, where it is about a thread stopping."
  (pcase (plist-get frame :event)
    ("debug-paused"
     (replique-debug--stopped process (replique-process--renamed-frame process frame)))
    ("debug-resumed"
     (replique-debug--resumed process (plist-get frame :thread)))
    ("debug-instrumented"
     (replique-debug--instrumented process (replique-process--renamed-frame process frame)))
    ("debug-uninstrumented"
     (replique-debug--uninstrumented process (plist-get frame :unit)))
    ("debug-recorded"
     (replique-debug--recorded process (plist-get frame :unit)))))

(add-hook 'replique-process-event-functions #'replique-debug--event)

;;; The locals of a frame

(defun replique-debug--views-of (process thread)
  "Return the buffers showing something of the thread THREAD of PROCESS.

The locals of one of its frames, and the value it stopped after."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'replique-inspect-mode)
                       (eq process replique-inspect--process)
                       (equal thread (plist-get (plist-get replique-inspect--source :debug)
                                                :thread)))))
              (buffer-list)))

(defun replique-debug--locals (process thread index what)
  "Show the locals of the frame INDEX of THREAD of PROCESS, called WHAT.

In the one view of the locals the thread has: a view of another frame is
made a view of this one."
  (let ((view (or (replique-debug--view-of process thread nil)
                  (replique-debug--new-view process thread "locals"))))
    (save-selected-window
      (replique-inspect-show process nil
                             (list :debug (list :thread thread :frame index))
                             (format "locals of %s" what)
                             view))))

(defun replique-debug--view-of (process thread value)
  "Return the view of the locals of THREAD of PROCESS - of its VALUE, with it."
  (seq-find (lambda (buffer)
              (eq (not value)
                  (not (plist-get (plist-get (buffer-local-value 'replique-inspect--source buffer)
                                             :debug)
                                  :value))))
            (replique-debug--views-of process thread)))

(defun replique-debug--new-view (process thread what)
  "Return a new buffer for a view of WHAT of the thread THREAD of PROCESS."
  (generate-new-buffer
   (if-let* ((buffer (replique-debug--buffer-of process thread)))
       (format "%s %s*" (string-remove-suffix "*" (buffer-name buffer)) what)
     (format "*replique-debug %s*" what))))

(defun replique-debug--value (process thread)
  "Show the value the thread THREAD of PROCESS stopped after, to browse.

In the one view of it the thread has, which follows the stops."
  (replique-inspect-show process nil
                         (list :debug (list :thread thread :value t))
                         "value of the form"
                         (or (replique-debug--view-of process thread t)
                             (replique-debug--new-view process thread "value"))))

;;; Commands

(defun replique-debug--this ()
  "Return the buffer of the stopped thread the commands act on.

The one this is, in a buffer of a thread; otherwise the only thread that
is stopped, or the one chosen."
  (if (derived-mode-p 'replique-debug-mode)
      (current-buffer)
    (let ((stopped (seq-filter (lambda (buffer)
                                 (with-current-buffer buffer
                                   (and (derived-mode-p 'replique-debug-mode)
                                        replique-debug--pause)))
                               (buffer-list))))
      (cond
       ((null stopped) (user-error "No thread is stopped"))
       ((null (cdr stopped)) (car stopped))
       (t (get-buffer (completing-read "Thread: " (mapcar #'buffer-name stopped) nil t)))))))

(defmacro replique-debug--in-this (&rest body)
  "Run BODY in the buffer of the stopped thread the commands act on."
  (declare (indent 0))
  `(with-current-buffer (replique-debug--this)
     (unless replique-debug--pause (user-error "The thread is running"))
     ,@body))

(defun replique-debug--frame-at-point ()
  "Return the index of the frame point is on, 0 where it is on none."
  (or (and (derived-mode-p 'replique-debug-mode)
           (plist-get (get-text-property (point) 'replique-debug-frame) :index))
      0))

(defun replique-debug--frame-name (index)
  "Return what the frame INDEX of the buffer is called."
  (let ((frame (seq-find (lambda (f) (eql index (plist-get f :index)))
                         replique-debug--frames)))
    (or (plist-get frame :fn) (plist-get frame :class) (format "frame %s" index))))

(defun replique-debug--request (msg then)
  "Send MSG about the thread of the buffer, and call THEN with the answer.

THEN is not called where the answer is an error, which is said instead."
  (let ((buffer (current-buffer)))
    (replique-process-request
     replique-debug--process msg
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (if (buffer-live-p buffer)
               (with-current-buffer buffer (replique-debug--answered-error frame))
             (message "replique: %s" (plist-get frame :message)))
         (funcall then frame))))))

(defun replique-debug-continue (&optional abort)
  "Let the stopped thread go on from where it stopped.

With ABORT, interactively the prefix argument, `break!' throws instead,
which ends what the thread was doing the way an exception would."
  (interactive "P")
  (replique-debug--in-this
    (replique-debug--request
     (append (list :op :debug-continue :thread replique-debug--thread)
             (when abort (list :abort t)))
     (lambda (_) (message (if abort "replique: aborted" "replique: continued"))))))

(defun replique-debug--step (mode)
  "Let the stopped thread go on to where MODE, a step, says."
  (replique-debug--in-this
    (replique-debug--request
     (list :op :debug-step :thread replique-debug--thread :mode mode)
     #'ignore)))

(defun replique-debug-next ()
  "Let the stopped thread go on to the next form of the call it is in.

Over the calls that form makes, and to the call it returns to once this
one is done.  Stops where code compiled to be stepped through is - see
`replique-debug-defun'."
  (interactive)
  (replique-debug--step "next"))

(defun replique-debug-in ()
  "Let the stopped thread go on to the next form, inside a call or not.

Which is into the call the form makes, where what it calls was compiled to
be stepped through - see `replique-debug-defun'."
  (interactive)
  (replique-debug--step "in"))

(defun replique-debug-out ()
  "Let the stopped thread go on to the form that called the one it is in."
  (interactive)
  (replique-debug--step "out"))

;;;###autoload
(defun replique-debug-defun ()
  "Evaluate the top level form around point, to be stepped through.

A definition stops the thread that calls it, each time it is called; any
other form stops the repl as it starts evaluating it.  From there, \\`n'
goes to the next form, \\`i' into a call and \\`o' out of it, each form
shown as it is evaluated, with its value after it.  Its locals are kept,
for stepping to show them and a restart to have them.

A definition is marked in the fringe while it is stepped through, which
is until `replique-debug-uninstrument', or until it is evaluated again
the ordinary way - `replique-eval-defun'.

The process has to run the compiler of replique-clj, and to have been
started with the debugger - see `replique-connect'.  ClojureScript is
not stepped through, but recorded - see `replique-debug-record'."
  (interactive)
  (replique-debug--stepped-here)
  (replique-eval--send (list (replique-eval--defun-at-point)) t))

(defun replique-debug-inspect-value ()
  "Browse what the form the thread stopped after evaluated to.

The value itself, rather than its printing: what of it is lazy is realized
as it is looked at, a page at a time."
  (interactive)
  (replique-debug--in-this
    (unless (plist-member replique-debug--pause :value)
      (user-error "The thread did not stop after a form"))
    (replique-debug--value replique-debug--process replique-debug--thread)))

;;;###autoload
(defun replique-debug-uninstrument (&optional all)
  "Stop stepping through the definition around point.

It stops the threads that call it no more, and its mark in the fringe
goes, with its breakpoints.  Its code stays what it was compiled to -
calling the debugger for every form, which costs - until it is evaluated
again.

With ALL, interactively the prefix argument, every definition the process
steps through."
  (interactive "P")
  (if all
      (let ((process (replique-process-ensure)))
        (maphash (lambda (for o)
                   (when (and (eq process (car for)) (overlay-buffer o))
                     (with-current-buffer (overlay-buffer o)
                       (mapc #'delete-overlay
                             (replique-debug--breaks-in (overlay-start o) (overlay-end o))))))
                 replique-debug--units)
        (replique-debug--uninstrument process nil))
    (let* ((node (replique-eval--defun-at-point))
           (marks (replique-debug--marks-in (replique-parse-start node)
                                            (replique-parse-end node))))
      (unless marks (user-error "Nothing here is stepped through"))
      (dolist (o marks)
        (mapc #'delete-overlay (replique-debug--breaks-in (overlay-start o) (overlay-end o)))
        (replique-debug--uninstrument (overlay-get o 'replique-debug-process)
                                      (overlay-get o 'replique-debug-unit))))))

(defun replique-debug-abort ()
  "Let the stopped thread go on by throwing from where it stopped."
  (interactive)
  (replique-debug-continue t))

(defun replique-debug-restart ()
  "Start the call of the frame at point over.

Every frame inside it goes, and the call is made again with the arguments
it was made with - to the code as it is now.  Which is what makes a fix
the next thing that runs: change the function, evaluate it, and restart
the frame that called it.

What the call did before it stopped stays done: an atom it swapped is
swapped.

The arguments are the ones the frame still holds, which is all of them
only in code compiled to keep its locals - see `replique-debug-keep-locals'.
Where the code that stopped clears them, the restart is asked for again."
  (interactive)
  (let ((index (replique-debug--frame-at-point)))
    (replique-debug--in-this
      (when (and (plist-get replique-debug--pause :locals-cleared)
                 (not (y-or-n-p (concat "The code that stopped clears its locals, so the"
                                        " call may start over with nil arguments"
                                        " - restart anyway? "))))
        (user-error "Not restarted - M-x replique-debug-keep-locals, and load it again"))
      (replique-debug--request
       (list :op :debug-restart :thread replique-debug--thread :frame index)
       (lambda (_) (message "replique: restarted"))))))

;;; Evaluating in a frame

(defvar-local replique-debug--evaluating-in nil
  "The (PROCESS . THREAD) a repl buffer evaluates in a frame of.")

(defun replique-debug--frame-repl-buffer (process thread)
  "Return the buffer of the repl evaluating in THREAD of PROCESS, or nil."
  (seq-find (lambda (buffer)
              (equal (cons process thread)
                     (buffer-local-value 'replique-debug--evaluating-in buffer)))
            (buffer-list)))

;;;###autoload
(defun replique-debug-eval-in-frame ()
  "Evaluate in the frame at point, from a repl of its own.

What the repl reads runs on the thread that stopped, with the locals of
the frame bound, in the namespace of its code - printing to the repl, and
with its *1 and *e.  The repl the thread was started from is usually
waiting for it, and this one is not.

It is the repl the commands act on while it is there, so what a Clojure
buffer sends goes to it too: `\[replique-eval-last-sexp]' on a form of
the code that stopped evaluates it with the values it has there.  Loading
a file, or evaluating a definition to step through or to record, is done
as anywhere else.

Its prompt says which frame, counted from where the thread stopped: a
thread stepped on is evaluated in where it got to.  Asked from another
frame, the repl moves there.  It goes with the buffer of the thread once
the thread goes on - or when it is killed.

In a recorded definition, evaluate in the call shown instead - see
`replique-debug-eval-in-call'."
  (interactive)
  (if (and (not (derived-mode-p 'replique-debug-mode))
           (seq-find (lambda (o) (overlay-get o 'replique-debug-record))
                     (replique-debug--marks-in (point) (1+ (point)))))
      (replique-debug-eval-in-call)
    (let ((index (replique-debug--frame-at-point)))
      (replique-debug--in-this
       (let* ((process replique-debug--process)
              (thread replique-debug--thread)
              (frame (list :thread thread :frame index))
              (buffer (replique-debug--frame-repl-buffer process thread))
              (repl (and buffer (buffer-local-value 'replique--buffer-repl buffer))))
         (if (replique-repl-live-p repl)
             (progn
               (unless (equal frame (replique-repl--in-frame repl))
                 (replique-repl-send-directive
                  repl (format "#replique/frame {:thread %s :frame %s}" thread index)))
               (setq replique-current-repl repl)
               (pop-to-buffer buffer))
           (when buffer (replique-debug--kill buffer))
           (let ((name (format "%s repl*" (string-remove-suffix "*" (buffer-name)))))
             ;; Renamed after it is opened, and so kept: what the handshake
             ;; says names a buffer only where it is still named as opened -
             ;; see `replique-repl--rename'
             (with-current-buffer (replique-repl--buffer
                                   (replique-repl--open process nil nil nil nil nil frame))
               (rename-buffer name t)
               (setq replique-debug--evaluating-in (cons process thread))))))))))

(defun replique-debug-visit-frame ()
  "Show the source of the frame at point, and its locals."
  (interactive)
  (let ((frame (get-text-property (point) 'replique-debug-frame)))
    (unless frame (user-error "Not on a frame"))
    (unless replique-debug--pause (user-error "The thread is running"))
    (let ((index (plist-get frame :index)))
      (replique-debug--locals replique-debug--process replique-debug--thread index
                              (replique-debug--frame-name index))
      (unless (replique-debug--show-place
               frame (cons replique-debug--process replique-debug--thread))
        (message "replique: %s is not a file to open"
                 (or (plist-get frame :source) (plist-get frame :class)))))))

(defun replique-debug-refresh ()
  "Ask for the frames of the thread again."
  (interactive)
  (replique-debug--in-this
    (replique-debug--ask-frames)))

(defun replique-debug-toggle-host-frames ()
  "Show the frames that are not Clojure functions, or stop showing them."
  (interactive)
  (setq replique-debug--host-frames (not replique-debug--host-frames))
  (replique-debug--render))

(defvar replique-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'replique-debug-continue)
    (define-key map (kbd "a") #'replique-debug-abort)
    (define-key map (kbd "r") #'replique-debug-restart)
    (define-key map (kbd "e") #'replique-debug-eval-in-frame)
    (define-key map (kbd "n") #'replique-debug-next)
    (define-key map (kbd "i") #'replique-debug-in)
    (define-key map (kbd "o") #'replique-debug-out)
    (define-key map (kbd "v") #'replique-debug-inspect-value)
    (define-key map (kbd "RET") #'replique-debug-visit-frame)
    (define-key map (kbd "g") #'replique-debug-refresh)
    (define-key map (kbd "j") #'replique-debug-toggle-host-frames)
    map)
  "Keymap of a buffer of a stopped thread.")

(define-derived-mode replique-debug-mode special-mode "Replique-Debug"
  "Major mode for a thread of a replique process that stopped.

\\{replique-debug-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (replique-debug-refresh)))
  (setq-local header-line-format '(:eval (replique-debug--header))))

;;;###autoload
(defun replique-debug-keep-locals (&optional clear)
  "Have the code the process compiles from now on keep its locals.

Clojure clears a local after its last use, so that a lazy sequence it
holds can be collected - which leaves a stopped frame holding nils where
its locals were, and a call started over called with nils.  Code compiled
to keep them keeps every one for as long as its frame lives, which costs
the memory such a sequence would have given back.

From now on, and only for what is compiled from now on: code already
loaded keeps clearing until it is loaded again, which is offered for the
file of this buffer - or, in the buffer of a stopped thread, for the file
it stopped in.  A repl whose thread is stopped loads it once the thread
goes on.

With CLEAR, interactively the prefix argument, the code compiled from now
on clears its locals again, which is how Clojure compiles by default."
  (interactive "P")
  (let* ((process (or (and (derived-mode-p 'replique-debug-mode) replique-debug--process)
                      (replique-name-process)
                      (user-error "No process - M-x replique-connect")))
         (frame (replique-process-request-sync
                 process (list :op :locals-clearing :clear (if clear t 'false)))))
    (cond
     ((or (null frame) (equal "error" (plist-get frame :tag)))
      (message "replique: %s" (or (plist-get frame :message) "the process did not answer")))
     (clear (message "replique: the code compiled from now on clears its locals"))
     (t
      (let ((file (replique-debug--file-to-reload)))
        (if (and file (called-interactively-p 'any)
                 (y-or-n-p (format "Locals are kept by the code compiled from now on - load %s again? "
                                   (file-name-nondirectory file))))
            (with-current-buffer (find-file-noselect file)
              (replique-load-file))
          (message "replique: locals are kept by the code compiled from now on%s"
                   " - load what you debug again")))))))

(defun replique-debug--file-to-reload ()
  "Return the file `replique-debug-keep-locals' offers to load again, or nil."
  (cond
   ((and (derived-mode-p 'replique-debug-mode) replique-debug--pause
         (not (plist-get replique-debug--pause :entry)))
    (plist-get replique-debug--pause :file))
   ((and buffer-file-name (derived-mode-p 'replique-clojure-mode)
         (string-match-p "\\.clj[c]?\\'" buffer-file-name))
    buffer-file-name)))

;;;###autoload
(defun replique-debug ()
  "Show a thread of the current process that is stopped.

Asked of the process, so that a thread that stopped while Emacs was not
connected - or whose buffer was killed - is shown all the same."
  (interactive)
  (let ((process (replique-process-ensure)))
    (replique-process-request
     process (list :op :debug-paused)
     (lambda (frame)
       (let ((paused (plist-get frame :paused)))
         (cond
          ((equal "error" (plist-get frame :tag))
           (message "replique: %s" (plist-get frame :message)))
          ((and (null paused) (not (plist-get frame :available)))
           (message "replique: no thread is stopped, and none can be - %s"
                    "the process was not started with `replique-debugger'"))
          ((null paused) (message "replique: no thread is stopped"))
          ((null (cdr paused)) (replique-debug--stopped process (car paused)))
          (t (let* ((names (mapcar (lambda (p) (cons (plist-get p :name) p)) paused))
                    (name (completing-read "Thread: " names nil t)))
               (replique-debug--stopped process (cdr (assoc name names)))))))))))

(provide 'replique-debug)

;;; replique-debug.el ends here
