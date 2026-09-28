;;; replique-repl.el --- REPL buffers  -*- lexical-binding: t; -*-

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

;; One repl connection per repl buffer.  A repl connection is not a message
;; channel: after the handshake the client writes plain Clojure and reads the
;; frames it produces, which is what makes a real stdin - and so nested repls,
;; (read-line) and debuggers - work.
;;
;; comint owns the input and nothing else.  What is displayed is assembled
;; from frames rather than echoed by a terminal, so the filter parses and
;; hands comint the text to insert.  Going through `comint-output-filter'
;; rather than inserting directly is what keeps the process mark, the fields
;; and the input ring consistent.
;;
;; RET sends what is at the prompt when it is a form and point is at the end
;; of it, and makes a new line anywhere earlier in it, so that a form
;; spanning several lines can be typed rather than pasted.  Whether it is
;; balanced would not be enough to go on by itself: a closing delimiter
;; inserted with its opening one leaves a form finished before it has been
;; written, and where point is is what says which of the two was meant.  It
;; only asks any of that where forms are what is being read: the code a repl
;; evaluates is handed a real stdin, and a line typed to a form that is
;; running is finished when the developer says it is.
;;
;; Two things a client learns the hard way.  A prompt does not mean a form was
;; answered: a read error, or a line holding only a comment, produces one of
;; its own, so consecutive prompts are collapsed rather than counted.  And the
;; output of a nested repl arrives as out frames - its prompt included - so it
;; is rendered as it comes rather than reconciled with anything.
;;
;; Code sent while the repl is busy is held back and written at the next
;; prompt, so that the transcript reads in the order the repl answered rather
;; than the order the editor asked.  That holds one prompt per thing sent,
;; which is right for one form and only approximate when a single send holds
;; several of them, or when a programmatic send lands in the middle of a form
;; being typed: the frames of a repl connection carry no id, so nothing here
;; can tell which form a result belongs to.  Per evaluation ids in the
;; protocol would settle it.
;;
;; The same window - between a form being sent from a buffer and the answer
;; coming back - is what the echo area reports: what the form printed, then
;; what it returned.  It is a boundary the protocol draws.  What arrives
;; outside of it arrived while nobody was looking, and the echo area is no
;; place to say so: the mode line names the buffer instead, and goes on
;; naming it until it is read.
;;
;; The buffer is read as Clojure - see `replique-repl--clojure' - since what
;; is typed at the prompt is Clojure and there is no reason for it to look
;; and move like anything else.  What comint reads it as by default is a
;; terminal session, and that is not only a poorer answer but a wrong one:
;; comint watches output for a shell asking for a password and answers it
;; with the process of the buffer, which here is the repl connection.  The
;; watcher is taken off.  What asks for a password on a terminal is the jvm,
;; on a standard input no repl reads - see `replique-process-input-password'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'comint)
(require 'replique-clojure-mode)
(require 'replique-common)
(require 'replique-edn)
(require 'replique-conn)
(require 'replique-exception)
(require 'replique-process)
(require 'replique-pprint)

(defcustom replique-prompt-read-only t
  "Whether the prompt of a repl buffer is read only."
  :type 'boolean
  :group 'replique)

(defcustom replique-echo-results t
  "Whether what a form evaluated from a buffer produced is shown in the echo area.

What it printed as well as what it returned: a result read without the
printing that went with it is half of what happened.  Both are in the
repl buffer either way - this is about not having to look at it."
  :type 'boolean
  :group 'replique)

(cl-defstruct (replique-repl
               (:constructor replique-repl--make)
               (:conc-name replique-repl--))
  "One repl of a process.

CONN is its connection, whose id is what `:interrupt' targets.  NS is the
namespace the next form will be read in, as the last prompt gave it.
AT-PROMPT says the buffer already ends with a prompt nothing has been
written after.  TO-ECHO counts the forms sent from a buffer whose result
has not come back yet, and ECHOED holds what they have printed so far -
the two of them are the evaluation the echo area is about to report.
QUEUED holds the code that was sent while the repl was
still busy with what came before it, waiting for a prompt to be written
after.  LAST-EXCEPTION is the whole exception frame rather than the
exception it carries: showing one takes the message and the phase too.
ON-END is what to call when the evaluation now running ends, for the one
caller that cannot carry on until it has - see
`replique-repl-send-code-sync\\='.  GIVEN-NAME is the buffer name this gave
the buffer, which is how `replique-repl--rename\\=' tells a name of its own
from one somebody else chose."
  process conn buffer given-name ns params at-prompt to-echo echoed queued
  last-exception on-end)

(defvar-local replique--buffer-repl nil
  "The repl a buffer is the buffer of.")

(defvar replique-current-repl nil
  "The repl the commands of a Clojure buffer act on.")

;;; Rendering

(defun replique-repl--insert (repl string &optional face)
  "Insert STRING in the buffer of REPL, with FACE."
  (let ((buffer (replique-repl--buffer repl))
        (proc (replique-conn--proc (replique-repl--conn repl))))
    (when (and string
               (not (string-empty-p string))
               (buffer-live-p buffer)
               (marker-buffer (process-mark proc)))
      (setf (replique-repl--at-prompt repl) nil)
      (comint-output-filter proc (if face (propertize string 'face face) string)))))

(defun replique-repl--aside (buffer string &optional face)
  "Write STRING at the end of BUFFER, in FACE, as replique\\='s own word.

NOT `replique-repl--insert\\=', which writes what the repl said and needs a
repl to read a connection and a process mark off.  What goes through here
is what replique has to say about a connection that has none - one being
opened, and one that was refused or has closed - so it writes into the
buffer and nothing else.

At the end and leaving point where it was: the buffer may be the one
somebody is reading, and a note is not a reason to move them."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (propertize string 'face (or face 'replique-note))))))))

(defun replique-repl--unattended (repl &optional string face)
  "Note STRING, shown in FACE, arriving in REPL with nothing waiting for it.

What a buffer asked for is reported where it was asked from.  The rest
is only ever in the repl buffer, and a repl buffer no window shows is
where output goes to not be read - so it is said about there instead, in
the mode line and in the echo area.  See `replique-note-unread\\='."
  (when (= 0 (or (replique-repl--to-echo repl) 0))
    (replique-note-unread (replique-repl--buffer repl) string face)))

(defun replique-repl--echo-keep (repl string)
  "Keep STRING, printed by the form REPL is answering, for the echo area.

Only what a buffer is waiting for: what arrives on its own is nothing
anybody asked to be told, and the mode line is what says it arrived.
Kept up to one character past what the echo area shows, which is what
tells a whole one from a cut one."
  (when (and replique-echo-results
             (> (or (replique-repl--to-echo repl) 0) 0))
    (let* ((kept (or (replique-repl--echoed repl) ""))
           (room (- (1+ replique-echo-max-chars) (length kept))))
      (when (> room 0)
        (setf (replique-repl--echoed repl)
              (concat kept (if (> (length string) room)
                               (substring string 0 room)
                             string)))))))

(defun replique-repl--echo (repl string)
  "Show STRING, and what the form printed, in the echo area of REPL.

Only when a buffer is waiting for it: what a repl buffer was typed into
is on screen already.  What was printed comes first and the result last,
in the order the buffer shows them.

The evaluation is the boundary, rather than what arrived in the last
second or so: the frames of a repl connection are not a stream of bytes,
and between the code being sent and the answer coming back is a window
the protocol draws rather than one a client guesses at."
  (when (> (or (replique-repl--to-echo repl) 0) 0)
    (setf (replique-repl--to-echo repl) (1- (replique-repl--to-echo repl)))
    (let ((printed (replique-repl--echoed repl)))
      (setf (replique-repl--echoed repl) nil)
      (when replique-echo-results
        (message "%s"
                 (replique-echo-shorten
                  (if (and printed (not (string-empty-p (string-trim printed))))
                      (concat (string-trim-right printed "\n+") "\n" string)
                    string)
                  (replique-repl--buffer repl)))))))

(defun replique-repl--awaited-p ()
  "Return non-nil when an evaluation is about to say what it produced.

What a buffer asked for owns the echo area, and it is about to be put
there: a line a background thread printed in the meantime does not get to
be the last thing said.  Any repl of any process, because the echo area
is one."
  (seq-some (lambda (process)
              (seq-some (lambda (repl)
                          (> (or (replique-repl--to-echo repl) 0) 0))
                        (replique-process--repls process)))
            (replique-processes-live)))

;; See `replique-echo-awaited-function': the file holding the echo area holds
;; no repls, and a repl is what an evaluation belongs to
(setq replique-echo-awaited-function #'replique-repl--awaited-p)

(defun replique-repl--ended (repl frame)
  "Tell whoever is waiting for the evaluation of REPL that FRAME ended it.

Three frames end one: a value, an exception, and the error a repl answers
what it could not even read with.  Nothing tells them apart at the point
of waiting - what is waited for is the repl being done - and which of them
it was is the caller\\='s to read.  The third is why they are three: a wait
that only ever ended on the first two would wait for ever on something the
repl never got as far as evaluating.

After the buffer has been written rather than before, so that whatever
runs next finds the transcript as somebody reading it would."
  (when-let* ((on-end (replique-repl--on-end repl)))
    (setf (replique-repl--on-end repl) nil)
    (funcall on-end frame)))

(defun replique-repl--truncation (exception)
  "Return what EXCEPTION left out, or nil.

A frame carries the top of the trace and the outermost causes.  The root
cause is the one the reported message names, so a chain that was cut must
be shown as cut rather than as a whole one."
  (let ((dropped (plist-get exception :trace-dropped))
        (cut (plist-get exception :cause-dropped)))
    (cond
     ((and dropped cut) (format " (%s more frames, and the chain goes on)" dropped))
     (dropped (format " (%s more frames)" dropped))
     (cut " (the chain goes on below the causes carried)")
     (t nil))))

(defun replique-repl--runtime (repl frame)
  "Say what FRAME, the `runtime\\=' event of REPL, came to.

WHAT A CLOJURESCRIPT HANDSHAKE NO LONGER SAYS.  The reply comes back
before the compiler is loaded and before the runtime is started, because
those are seconds and a handshake that waits for them is a handshake an
editor gives up on - see `replique-repl\\='.  So where the runtime is
arrives afterwards, as this, and it is the frame before the first prompt.

TWO THINGS AND THE SAME FRAME CARRIES EITHER.  A `url\\=' is the page to
open, which only a browser repl has.  A `message\\=' is why there is no
runtime and so no prompt either - a compiler that would not load, a node
that is not on PATH, a port already taken - and the connection closes
after it.

A node runtime that started says neither, and there is nothing to write:
what it would say is that the waiting is over, and the prompt says that
better."
  (let ((message (plist-get frame :message))
        (url (plist-get frame :url)))
    (cond
     (message
      (replique-repl--unattended repl (concat message "\n") 'replique-exception)
      (replique-repl--insert
       repl
       (replique-exception-button (concat message "\n")
                                  (plist-get frame :exception)
                                  message nil "starting the runtime")
       'replique-exception))
     (url
      (replique-repl--insert repl (format "Open %s\n" url) 'replique-note)))))

(defun replique-repl--frame (repl frame)
  "Render FRAME in the buffer of REPL."
  (pcase (plist-get frame :tag)
    ("out"
     (replique-repl--unattended repl (plist-get frame :string))
     (replique-repl--echo-keep repl (plist-get frame :string))
     (replique-repl--insert repl (plist-get frame :string)))
    ("err"
     (replique-repl--unattended repl (plist-get frame :string) 'replique-stderr)
     (replique-repl--echo-keep repl (plist-get frame :string))
     (replique-repl--insert repl (plist-get frame :string) 'replique-stderr))
    ("ret"
     (let ((value (plist-get frame :value)))
       (replique-repl--unattended repl (concat value "\n"))
       (replique-repl--insert repl (concat value "\n"))
       (replique-repl--echo repl value))
     (replique-repl--ended repl frame))
    ("exception"
     (let* ((message (plist-get frame :message))
            (phase (plist-get frame :phase))
            (exception (plist-get frame :exception))
            (truncated (and exception (replique-repl--truncation exception))))
       (replique-repl--unattended repl (concat message "\n") 'replique-exception)
       (setf (replique-repl--last-exception repl) frame)
       ;; The line is the way to the whole of it: what a developer looks at
       ;; first is what they would click
       (replique-repl--insert
        repl
        (replique-exception-button (concat message (or truncated "") "\n")
                                   exception message phase "at the repl")
        'replique-exception)
       (replique-repl--echo repl message))
     (replique-repl--ended repl frame))
    ("prompt"
     (let ((moved (not (equal (replique-repl--ns repl) (plist-get frame :ns)))))
       (setf (replique-repl--ns repl) (plist-get frame :ns))
       (setf (replique-repl--params repl) (plist-get frame :params))
       ;; A form is not what produces a prompt - a read error and a comment
       ;; produce one too - so the buffer is what says whether one is needed.
       ;; Unless the namespace moved under a prompt that is already written,
       ;; which is what a directive does: nothing was evaluated, so nothing
       ;; consumed that prompt, and leaving it standing would leave the
       ;; buffer saying the repl is somewhere it is not
       (unless (and (replique-repl--at-prompt repl) (not moved))
         ;; On a line of its own.  What normally comes before a prompt is
         ;; the newline of whatever was printed above it, and a prompt
         ;; written under a prompt has nothing above it but the one it is
         ;; replacing
         (when (replique-repl--at-prompt repl)
           (replique-repl--insert repl "\n"))
         ;; Written as a prompt rather than merely painted like one: a repl
         ;; buffer is read as Clojure, and `user=>' reads as a symbol - so
         ;; without this the prompt is a form, and the form before point
         ;; everywhere point usually is.  See `replique-prompt-text'
         (replique-repl--insert
          repl (replique-prompt-text (format "%s=> " (plist-get frame :ns))))
         (setf (replique-repl--at-prompt repl) t)))
     ;; What was sent while the repl was busy is written now: the transcript
     ;; reads in the order the repl answered, not the order the editor asked
     (when-let* ((queued (replique-repl--queued repl)))
       (setf (replique-repl--queued repl) (cdr queued))
       (replique-repl--insert repl (concat (car queued) "\n"))))
    ("error"
     (replique-repl--unattended repl)
     (replique-repl--insert repl
                            (format "%s: %s\n"
                                    (plist-get frame :error)
                                    (plist-get frame :message))
                            'replique-exception)
     (replique-repl--ended repl frame))
    ;; A repl connection carries one, and it is the runtime a ClojureScript
    ;; repl is waiting for - see `replique-repl--runtime'.  Anything else is
    ;; the control connection's business and is not read here
    ("event"
     (when (equal "runtime" (plist-get frame :event))
       (replique-repl--runtime repl frame)))
    (_ nil)))

;;; Whether what is typed is a form yet

(defconst replique-repl--syntax
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Enough of the syntax of Clojure to tell where a form ends.

Its own rather than the table the buffer is in.  A repl buffer is given
`replique-clojure-mode-syntax-table', which is a table somebody can
change; whether RET sends what is at the prompt or starts a new line is
not an answer a customization should be able to move.  All this is asked
is where the delimiters, the strings and the comments are, which is
little enough that
the two things that look like they need a rule of their own do not: a
character literal is a backslash, which is an escape, and that is what
keeps the paren of \\=\\( from counting; and a regex is a dispatch
character followed by an ordinary string.")

(defun replique-repl--reading-a-form-p (proc)
  "Return non-nil when what PROC is waiting for is a form.

What is typed in a repl buffer is not always Clojure.  The repl hands the
code it evaluates a real stdin, so a form that calls read-line is
answered by typing in the same place, and a line of that is finished when
the developer says it is and not when a delimiter closes.  A prompt is
what says forms are being read again - the one this repl wrote, or the
one a nested repl wrote, which arrives as output and is a prompt all the
same.

The line the prompt is on is asked for with `inhibit-field-text-motion'
bound.  comint gives what it printed a field of its own, and the process
mark is the boundary of it, so `line-beginning-position' answers there
with the process mark itself - a limit with the prompt outside it, which
is a prompt nothing can match."
  (save-excursion
    (goto-char (process-mark proc))
    (let ((inhibit-field-text-motion t))
      (looking-back comint-prompt-regexp (line-beginning-position)))))

(defun replique-repl--unfinished-p (start end)
  "Return non-nil when what is between START and END is unfinished.

Unfinished means the reader would want more, and only what more text
would finish counts: an unclosed delimiter, an unclosed
string, a trailing backslash.  Delimiters that close what was never
opened are not unfinished but wrong, and waiting for them would be
waiting forever - that goes to the reader, which says what is wrong with
it better than anything here could."
  (with-syntax-table replique-repl--syntax
    (let ((state (parse-partial-sexp start end)))
      (or (> (nth 0 state) 0)
          (nth 3 state)
          (nth 5 state)))))

;;; The mode

;; Defined in replique-eval, which requires this file rather than the other
;; way round: what the command offers to move to is read out of a buffer, and
;; reading a buffer is that file's job.  Bound here because this is where it
;; is used from
(declare-function replique-in-ns "replique-eval")
(declare-function replique-remove-var "replique-symbol")
(declare-function replique-reload-app "replique-reload")

(defvar replique-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'replique-interrupt)
    (define-key map (kbd "C-c C-q") #'replique-quit-repl)
    (define-key map (kbd "C-c C-e") #'replique-show-last-exception)
    (define-key map (kbd "C-c C-p") #'replique-pprint)
    ;; Autoloaded from replique-eval: what it offers is read from a buffer,
    ;; and a repl buffer is one it has nothing to read out of - but the
    ;; command is the same one, and it is bound where it is used
    (define-key map (kbd "C-c M-n") #'replique-in-ns)
    ;; Autoloaded from replique-reload, and bound here because a repl buffer
    ;; is where somebody watching an application notices it is out of date -
    ;; the command is about the process rather than about a buffer, so a
    ;; buffer with no code in it is as good a place to press it as any
    (define-key map (kbd "C-c M-r") #'replique-reload-app)
    ;; A name written at the prompt is as removable as one written in a file,
    ;; and eldoc and xref already answer about it here - see `replique.el'.
    ;; There is no load-file, because a repl buffer holds no file
    (define-key map (kbd "C-c C-u") #'replique-remove-var)
    ;; The same key as in a Clojure buffer, and the same command.  What comint
    ;; has here is `comint-delete-output', which takes the last answer out of
    ;; the transcript and writes "*** output flushed ***" where it was - a
    ;; terminal habit, and not what this key means anywhere else in replique
    (define-key map (kbd "C-c C-o") #'replique-show-process-output)
    (define-key map (kbd "RET") #'replique-repl-return)
    ;; What comint keeps on these two is meant for a subprocess of a terminal,
    ;; and what this buffer holds is a socket.  `comint-stop-subjob' sends no
    ;; signal down one: it stops Emacs reading what arrives, so the repl falls
    ;; silent and looks hung with nothing anywhere to say why - the worst of
    ;; the two, because it is the one that looks like a crash.
    ;; `comint-quit-subjob' asks for a signal that a connection cannot carry.
    ;; Masked rather than left to fall through, so the key says it is
    ;; undefined instead of quietly doing something else: interrupting is
    ;; `C-c C-c' and quitting is `C-c C-q', which are what these are reached
    ;; for
    (define-key map (kbd "C-c C-z") #'undefined)
    (define-key map (kbd "C-c C-\\") #'undefined)
    map)
  "Keymap of a repl buffer.")

(define-derived-mode replique-repl-mode comint-mode "Replique"
  "Major mode for a replique REPL.

What is typed at the prompt is Clojure, so the buffer is given the syntax
and the parse `replique-clojure-mode' reads Clojure with - see
`replique-repl--clojure'.

\\{replique-repl-mode-map}"
  :syntax-table replique-clojure-mode-syntax-table
  (setq-local comint-prompt-regexp "^[^ \n]*=> *")
  (setq-local comint-prompt-read-only replique-prompt-read-only)
  (setq-local comint-input-sender #'replique-repl--input-sender)
  (setq-local comint-process-echoes nil)
  ;; What comint watches for is a password prompt of a shell, and what it
  ;; does about one is send the answer to the process of this buffer - which
  ;; here is the repl connection, where it would be read as code.  A repl
  ;; printing "Password: " is a repl printing something.  What does ask on a
  ;; terminal is the jvm itself, through java.io.Console, which reads the
  ;; standard input the repl is not - see `replique-process-input-password'
  (setq-local comint-output-filter-functions
              (remq 'comint-watch-for-password-prompt
                    comint-output-filter-functions))
  (setq-local mode-line-process '(:eval (replique-repl--mode-line)))
  (replique-repl--clojure))

(defun replique-repl--clojure ()
  "Read the current buffer as Clojure, the way `replique-clojure-mode' does.

Which is what makes what is typed at the prompt highlighted, indented and
navigable as the code it is, rather than as the text a comint buffer
holds by default.

The parse covers the transcript as well as the prompt, and a transcript
is not Clojure: what a form printed parses as whatever it happens to look
like.  That is the cost of one parse of one buffer, and it is paid in the
part nobody edits.  It also means a repl that has printed a great deal is
a large buffer being reparsed, which is worth knowing when one is slow."
  (replique-clojure-setup))

(defun replique-repl--mode-line ()
  "Return the mode line description of the repl of the current buffer."
  (let ((repl replique--buffer-repl))
    (cond
     ((null repl) "")
     ((not (replique-conn-live-p (replique-repl--conn repl))) ":closed")
     (t (format ":%s" (or (replique-repl--ns repl) "?"))))))

(defun replique-repl--input-sender (proc string)
  "Send STRING, typed at the prompt, to PROC."
  (let ((repl (process-get proc 'replique-repl)))
    (when repl (setf (replique-repl--at-prompt repl) nil)))
  (comint-simple-send proc string))

(defun replique-repl-return (&optional anyway)
  "Send what is at the prompt, or start a new line in what is not finished.

What is at the prompt is sent when it is a form and point is at the end
of it.  Anywhere earlier in it, this makes a new line, which is what
makes a form spanning several lines something that can be typed rather
than pasted.

Whether it is balanced is not enough to go on by itself.  A closing
delimiter inserted with its opening one - `electric-pair-mode' and the
like - leaves a form finished before it has been written, and sending
every one of those the moment its first line was done would leave nothing
that can be typed over two lines at all.  Where point is says which of
the two was meant: a form still being written is written from inside it.

Both questions are only asked of what is typed where the repl is reading
forms - see `replique-repl--reading-a-form-p' - and never of what is
typed to a form that is running.

With a prefix argument, ANYWAY, send what is there whatever state it is
in: an unclosed delimiter, or point left in the middle of it.  Where the
text ends is a guess made about text nothing has read yet, and a guess is
worth a way to overrule it.

Point above the input sends what is under it, the way `comint-send-input'
does: everything above the prompt is a transcript, and a transcript is
read rather than continued."
  (interactive "P")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (not anyway)
             proc
             (>= (point) (process-mark proc))
             (replique-repl--reading-a-form-p proc)
             (or (< (point) (point-max))
                 (replique-repl--unfinished-p (process-mark proc) (point-max))))
        ;; `comint-accumulate' rather than an insert of a newline: it marks
        ;; where the line being typed begins, and `comint-delete-input' -
        ;; which is how the input ring replaces what is at the prompt - takes
        ;; back to that mark rather than to the process mark.  Without it,
        ;; recalling a previous input in the middle of a form throws away the
        ;; lines of that form already written.  Not `newline' either: there
        ;; is no indenting a comint buffer - the line above the input can be
        ;; anything the process printed - and `electric-indent-mode' would
        ;; try
        (comint-accumulate)
      (comint-send-input))))

;;; Opening

(defun replique-repl--name (process dialect target)
  "Return the name a repl of PROCESS, DIALECT and TARGET goes under.

WHAT A REPL IS, IN ITS NAME, because the name is what there is to choose
between.  Two repls of one process differ in the only way repls of one
process can - the language they read, and the runtime they read it into -
and a list of buffers distinguished by nothing but `<2>\\=' is a list that
makes somebody open both to find out which is which.  `replique-select-repl\\='
offers exactly these names and nothing else, so what is in the name is the
whole of what there is to choose by.

THE TARGET IS PART OF IT AND THE DIALECT IS NOT ENOUGH: a browser build
and a node build are two compilations of the same sources holding
different code, so `cljs:browser\\=' and `cljs:node\\=' are as far apart as
either is from `clj\\='.  A Clojure repl has no target - it runs in the
process rather than in a runtime of its own - and carries none.

Replique 1 wrote `*replique*<dir>*<type>*<session>*\\=' and renamed the
buffer whenever the repl changed type, because there a repl flipped
between Clojure and ClojureScript in place.  Here a repl is one
connection and its dialect is settled by the handshake, so the name is
settled with it."
  (format "*replique: %s %s*"
          (replique-process--id process)
          (if (eq :cljs dialect)
              (if target
                  (format "cljs:%s" (substring (symbol-name target) 1))
                "cljs")
            "clj")))

(defun replique-repl--buffer-name (process dialect target)
  "Return an unused name for a repl buffer of PROCESS, DIALECT and TARGET."
  (generate-new-buffer-name (replique-repl--name process dialect target)))

(defun replique-repl--rename (repl)
  "Rename REPL\\='s buffer after what the handshake said it turned out to be.

WHAT WAS ASKED FOR IS NOT WHAT IT IS.  A repl opened with no target gets
the process\\='s default, and which one that is belongs to the process:
reading it off the reply is one spelling of the rule where writing
`browser\\=' here would be a second.  So the buffer is named from what was
asked for - it exists before there is a reply to read - and named again
from the reply, which is the first moment the answer is here.

A BUFFER SOMEBODY ELSE RENAMED IS LEFT ALONE, which is how replique 1 did
it too: this renames the name it gave, and a name it did not give is
somebody\\='s doing and not ours to undo.  Unnamed and already right are
both nothing to do.

THE NAME IT WANTS IS COMPARED BEFORE IT IS MADE UNIQUE, which is the
whole of why `replique-repl--name\\=' exists beside
`replique-repl--buffer-name\\='.  A repl that turned out to be what it asked
to be already holds the name this wants - so uniquifying first asks
whether that name is taken, is told yes BY THIS VERY BUFFER, and renames
the first repl of a process to <2> for having been right all along.
Renaming with UNIQUE does the same comparison the other way round: it
leaves this buffer\\='s own name out of what counts as taken, so a second
Clojure repl of one process still lands on <2> and the first one stays
where it is."
  (let ((buffer (replique-repl--buffer repl)))
    (when (and (buffer-live-p buffer)
               (equal (buffer-name buffer) (replique-repl--given-name repl)))
      (let ((name (replique-repl--name (replique-repl--process repl)
                                       (replique-repl-dialect repl)
                                       (replique-repl-target repl))))
        (unless (equal name (buffer-name buffer))
          (with-current-buffer buffer (rename-buffer name t))
          ;; What it was actually called, which is not `name' when another
          ;; repl of this process already holds it
          (setf (replique-repl--given-name repl) (buffer-name buffer)))))))

(defconst replique-repl-dialects '("clj" "cljs")
  "The languages a repl can be a repl of.

The same two the process takes, spelled the same way - see the
`:dialect' of its handshake.  Written out here rather than asked of the
process because they are what the protocol says, not what a particular
process happens to have: a process without the ClojureScript compiler
refuses a cljs repl with a sentence about its classpath, which is a
better answer than never offering the choice.")

(defconst replique-repl-targets '("browser" "node")
  "The runtimes a ClojureScript repl can run in.

Two, and they are not interchangeable: a browser and node resolve npm
packages differently, so they are two different compilations of the same
sources and the process keeps one of each.")

(defconst replique-repl-main-modules-timeout 10
  "How long to wait for the process to say what its main modules are, in seconds.

The answer is a walk of the project directory, which is milliseconds in
most projects and a fraction of a second in one with a large node_modules
beside it - the walk does not go in, but it does step over everything at
the top of it.  Bounded so that a process which stopped answering is a
prompt with nothing to offer rather than an Emacs that hangs.")

(defun replique-repl--main-namespaces (process)
  "Return the namespaces the main modules under PROCESS name.

Each of them is a program a page of this project loads, which is what
makes it a program this project can be started on - see
`replique-repl--read-main' for why the two ends are one namespace.  The
walk that finds those files is the process\\='s, so this is asked for
rather than done here; replique 1 walked the project from the editor and
built the same list as it went.

Sorted and without repeats: two pages loading one program is an ordinary
thing, and the order a walk came back in says nothing.

Nil where the process cannot say, and NOT AN ERROR: this is the
convenience half of opening a repl, and a command that refused to open one
because a menu could not be built would be refusing over the part nobody
asked for.  A refusal carries no modules and neither does a process that
stopped answering, so both of them are read as the empty menu they are."
  (let ((frame (replique-process-request-sync
                process (list :op :main-modules)
                replique-repl-main-modules-timeout)))
    (sort (delete-dups
           (delq nil (mapcar (lambda (module) (plist-get module :main))
                             (plist-get frame :modules))))
          #'string<)))

(defun replique-repl--read-main (process)
  "Read the namespace a ClojureScript repl of PROCESS is to be started on.

WHAT THE PAGES OF THIS PROJECT LOAD is what is offered, which is the
`mainNs' of each main module under it - see `replique-main-js'.  The two
are one namespace seen from its two ends: a page imports what its own
module names, and that import finds something on disk only where
something compiled it.  Starting the repl on that namespace is what
compiles it, so a page opened afterwards loads a program rather than a
404.  Replique 1 offered the same list, harvested the same way, for the
same reason.

Offered as text rather than as a default so that it can be cleared: a
repl standing in no particular program is an ordinary thing to want, and
with a default, return would be the only answer the prompt has.

What is typed is what is sent.  A project may have no main module at all,
and a namespace that is the `mainNs' of none is as good a place to start
as any - what this compiles is the namespace, not the file that named it."
  (let* ((namespaces (replique-repl--main-namespaces process))
         (main (string-trim
                (completing-read "Main namespace (empty for none): "
                                 namespaces nil nil (car namespaces)))))
    (unless (string-empty-p main) main)))

(defun replique-repl--read ()
  "Read the arguments of a repl about to be opened.

Nothing is asked without a prefix argument, and nothing is then sent:
a repl is a Clojure repl unless somebody says otherwise, which is the
rule the process follows too - an absent `:dialect' is Clojure there.
So the common case stays one command with no questions, and the keys are
absent from the handshake of every Clojure repl rather than written on
every one of them.

The target and the namespace are asked only for a ClojureScript repl,
being the two things that have no meaning for the other.

The process is settled before anything is asked, and is answered rather
than left to the command to find again: which namespaces are offered is a
fact about one process, and asking one about its main modules and then
opening the repl on another would be a menu of somewhere else."
  (if (not current-prefix-arg)
      (list nil nil nil nil)
    (let* ((process (replique-process-ensure))
           (dialect (intern (concat ":" (completing-read
                                         (format-prompt "Dialect" "clj")
                                         replique-repl-dialects nil t
                                         nil nil "clj")))))
      (if (not (eq dialect :cljs))
          (list process dialect nil nil)
        (list process dialect
              (intern (concat ":" (completing-read
                                   (format-prompt "Target" "browser")
                                   replique-repl-targets nil t
                                   nil nil "browser")))
              (replique-repl--read-main process))))))

;;;###autoload
(defun replique-repl (&optional process dialect target main)
  "Open a REPL on PROCESS, the current process by default.

DIALECT is `:clj' or `:cljs', Clojure when it is nil.  TARGET is
`:browser' or `:node' and is only about a ClojureScript repl, the
process\\='s own default when it is nil.  MAIN is a namespace such a repl
is to be started on, and is nil for one standing in no particular
program.  With a prefix argument they are asked for - see
`replique-repl--read'.

MAIN IS COMPILED BEFORE THE FIRST PROMPT, it and everything it depends
on, so the first form sent is not the one that pays for the dependency
graph.  What that promises is the OUTPUT DIRECTORY rather than the
runtime: on node the namespace is required into the runtime as well, and
on the browser the page is what loads it, whenever somebody opens the
page.  A namespace that does not compile is written into the buffer,
before the prompt.

A CLOJURESCRIPT HANDSHAKE IS ANSWERED AT ONCE AND ITS RUNTIME STARTED
AFTERWARDS, which is what the seconds between opening this and the first
prompt are.  Loading the compiler takes clojure\\='s require lock with it,
so a Clojure repl on the same process that is loading anything at all
holds those seconds open for as long as it takes - and a handshake that
waited would be one `replique-conn-handshake-timeout\\=' gives up on, which
reads in the buffer as a process that died.

So the page to open arrives afterwards, as the `runtime\\=' event
`replique-repl--runtime\\=' writes into the buffer - and it is the whole of
what a browser repl needs of whoever started it: nothing runs, and so
nothing is evaluated, until a browser is on that page.  Why there will be
no runtime arrives the same way, and so does why there is no repl at all:
a refused handshake says so in the buffer rather than only in the echo
area."
  (interactive (replique-repl--read))
  (let* ((process (or process (replique-process-ensure)))
         ;; Named from what is being asked for, and named again from the
         ;; reply - see `replique-repl--rename'.  The buffer has to exist
         ;; before the connection does: it is where the connection writes
         (name (replique-repl--buffer-name process dialect target))
         (buffer (get-buffer-create name))
         (repl (replique-repl--make :process process :buffer buffer
                                    :given-name name :to-echo 0)))
    (with-current-buffer buffer
      ;; Before anything buffer local is set: comint-mode kills them
      (replique-repl-mode)
      (setq-local replique--buffer-repl repl)
      (when-let* ((directory (replique-process--directory process)))
        (setq-local default-directory (file-name-as-directory directory))))
    (setf
     (replique-repl--conn repl)
     (condition-case err
         (replique-conn-open
          (replique-process--host process)
          (replique-process--port process)
          'repl
          :buffer buffer
          :process-id (replique-process--id process)
          :hello (append (when dialect (list :dialect dialect))
                         (when target (list :target target))
                         ;; Left out rather than sent as nil, as the two
                         ;; above are: an absent key is how the protocol
                         ;; writes a repl standing in no program
                         (when main (list :main main)))
          :on-ready (lambda (conn)
                      (let ((proc (replique-conn--proc conn))
                            (cljs (equal "cljs" (plist-get (replique-conn--info conn)
                                                           :dialect))))
                        (process-put proc 'replique-repl repl)
                        ;; Before anything is written into the buffer, so
                        ;; that what a repl says arrives in a buffer already
                        ;; named what it is
                        (replique-repl--rename repl)
                        (with-current-buffer buffer
                          (goto-char (point-max))
                          ;; Before the process mark is set, so that the
                          ;; mark - and the prompt the process writes at
                          ;; it - comes after the note rather than before
                          ;; it.  `replique-repl--aside' is what would
                          ;; write this anywhere else, and it cannot be
                          ;; used here: it writes at the end and this has
                          ;; to be written before a mark that is not set
                          ;; yet
                          ;;
                          ;; SAID BECAUSE NOTHING ELSE IS, and for as long
                          ;; as it takes: a ClojureScript handshake is
                          ;; answered at once and its compiler and its
                          ;; runtime are started afterwards - see
                          ;; `replique-repl--runtime' - so between this and
                          ;; the first prompt there are seconds in which
                          ;; the buffer would otherwise be empty, which is
                          ;; what a repl that failed to open looks like
                          (when cljs
                            (let ((inhibit-read-only t))
                              (insert (propertize
                                       "Starting the ClojureScript compiler and its runtime...\n"
                                       'face 'replique-note))))
                          (goto-char (point-max))
                          (set-marker (process-mark proc) (point))
                          (run-hooks 'comint-exec-hook))))
          :on-frame (lambda (frame) (replique-repl--frame repl frame))
          ;; IN THE BUFFER AND NOT ONLY IN THE ECHO AREA, which is what
          ;; giving this at all is for.  A refused handshake closes the
          ;; connection, so without it the buffer says "The connection is
          ;; closed" and nothing else, the reason having been a message that
          ;; is gone by the time anybody looks - and a repl that would not
          ;; open reads as a process that died.  The buffer is what is still
          ;; there afterwards, so the reason goes there and the echo area
          ;; keeps its copy for whoever is looking now
          :on-error (lambda (frame)
                      (let* ((host (replique-process--host process))
                             (port (replique-process--port process))
                             (kind (plist-get frame :error))
                             (said (cond
                                    ((equal kind replique-conn-closed-error)
                                     (format "%s:%s closed the connection before answering"
                                             host port))
                                    ((equal kind replique-conn-unanswered-error)
                                     (format "%s:%s took the connection and did not answer in %ss"
                                             host port replique-conn-handshake-timeout))
                                    (t (format "%s (%s)"
                                               (plist-get frame :message) kind)))))
                        (replique-repl--aside
                         buffer (format "This repl could not be opened: %s\n" said)
                         'replique-exception)
                        (message "replique: %s" said)))
          :on-close (lambda (_conn)
                      (replique-repl--aside buffer "\nThe connection is closed\n")
                      (setf (replique-process--repls process)
                            (delq repl (replique-process--repls process)))
                      (when (eq replique-current-repl repl)
                        (setq replique-current-repl nil))))
       ;; The process is gone.  `replique-process--connect' says this for the
       ;; control connection; a repl is opened later, and the process can
       ;; have left in between
       (file-error
        (kill-buffer buffer)
        (user-error "Nothing is listening on %s:%s - %s"
                    (replique-process--host process)
                    (replique-process--port process)
                    (or (nth 2 err) "the process is gone")))))
    (push repl (replique-process--repls process))
    (setq replique-current-repl repl)
    (pop-to-buffer buffer)
    repl))

;;; What the commands act on

(defun replique-repl-live-p (repl)
  "Return non-nil when REPL is still connected."
  (and repl (replique-conn-live-p (replique-repl--conn repl))))

(defun replique-repl-current ()
  "Return the repl the commands act on, or nil.

The repl of the current buffer when it is one - a repl buffer acts on
itself - then the one `replique-select-repl' chose, then the most recent
live repl of the current process."
  (or (and (replique-repl-live-p replique--buffer-repl) replique--buffer-repl)
      (and (replique-repl-live-p replique-current-repl) replique-current-repl)
      (let ((process (replique-process-current)))
        (when process
          (setq replique-current-repl
                (seq-find #'replique-repl-live-p (replique-process--repls process)))))))

(defun replique-select-repl ()
  "Choose the repl the commands of a Clojure buffer act on."
  (interactive)
  (let* ((repls (seq-mapcat (lambda (process)
                              (seq-filter #'replique-repl-live-p
                                          (replique-process--repls process)))
                            (replique-processes-live)))
         (choices (mapcar (lambda (repl)
                            (cons (buffer-name (replique-repl--buffer repl)) repl))
                          repls)))
    (unless choices (user-error "No repl"))
    (let ((choice (completing-read "Repl: " choices nil t)))
      (setq replique-current-repl (cdr (assoc choice choices)))
      ;; MOVED TO THE FRONT OF ITS PROCESS as well, because the list is
      ;; where `replique-repl-for-dialect' looks when the repl in hand is of
      ;; the other dialect - and what it wants there is the most recent of
      ;; each, which is what choosing one makes it.  Without this, choosing
      ;; between two ClojureScript repls would move .cljs files to it and
      ;; leave .clj files pointed at whichever Clojure repl was opened last.
      ;; Replique 1 reorders one list for the same reason, in
      ;; `replique/switch-active-repl'
      (let* ((repl replique-current-repl)
             (process (replique-repl-process repl)))
        (setf (replique-process--repls process)
              (cons repl (delq repl (replique-process--repls process)))))
      (message "replique: %s" choice))))

(defun replique-repl-process (repl)
  "Return the process REPL is a repl of.

Which is what its control connection belongs to, and so what answers the
ops asked about a repl - rather than whatever process the commands are
currently pointed at."
  (replique-repl--process repl))

(defun replique-repl-ensure ()
  "Return the repl the commands act on, or signal that there is none."
  (or (replique-repl-current)
      (user-error "No repl - M-x replique-repl")))

;;; Which world a question is about, and which repl it is asked of
;;
;; A name is read against one of two worlds, and which one is the client's to
;; say: the file has an extension and the process has only what the message
;; says.  So every op that can be asked either way carries a `:dialect', and a
;; ClojureScript one carries the `:target' as well - a browser build and a node
;; build are two different compilations of the same sources, holding different
;; code.
;;
;; ABSENT MEANS CLOJURE, which is the process's rule and not an accident of
;; this side: a message about Clojure carries no dialect at all rather than
;; carrying one that says the default.  So these return nil rather than
;; `(:dialect :clj)', and a Clojure session sends exactly what it sent before
;; there was a second answer.
;;
;; Which world a BUFFER asks about is the extension's, and replique 1 settled
;; the three cases: .clj is Clojure, .cljs is ClojureScript, and .cljc is
;; whichever repl the commands are pointed at, because a .cljc namespace really
;; is a namespace of both worlds and nothing in the file chooses.  A repl
;; buffer falls in with .cljc for the same reason read the other way: it is not
;; a file, and the repl it is the buffer of is exactly what it is about.
;;
;; THE SAME THREE CASES DECIDE WHERE CODE GOES, and not only what a name is
;; read against.  A .clj file evaluated in a ClojureScript repl is a file
;; compiled by a compiler that was never going to be able to read it, and the
;; repl the commands happen to be pointed at is no reason to try: what the
;; file is, is written in its name.  So `replique-repl-for-dialect' answers
;; where a buffer's code goes, and the commands that send a buffer's code -
;; evaluating a form, loading the file, loading what changed - ask it rather
;; than `replique-repl-current'.  A command about a REPL still asks
;; `replique-repl-current': interrupting one, quitting one, or moving one into
;; a namespace is about that repl whatever buffer the key was pressed in.

(defun replique-repl-dialect (repl)
  "Return the dialect REPL is a repl of: `:cljs' or `:clj'.

Read off the handshake reply rather than remembered from what was asked
for: what a repl turned out to be is the process's answer, and a repl
that asked for nothing is a Clojure repl without having said so."
  (if (equal "cljs" (plist-get (replique-conn--info (replique-repl--conn repl))
                               :dialect))
      :cljs
    :clj))

(defun replique-repl-target (repl)
  "Return the runtime REPL runs in - `:browser' or `:node' - or nil.

Nil for a Clojure repl, which runs in the process and not in a runtime of
its own.  The process names it in the handshake reply whether or not the
repl asked for one, so this is the target in force rather than the target
requested."
  (when-let* ((target (plist-get (replique-conn--info (replique-repl--conn repl))
                                 :target)))
    (intern (concat ":" target))))

(defun replique-repl-dialect-keys (repl)
  "Return the dialect keys of a question about REPL itself.

What the repl is, rather than what the buffer asking is: moving a repl
into a namespace is about the namespaces that repl has, and a Clojure
buffer is as good a place to ask it from as any other."
  (when (and repl (eq :cljs (replique-repl-dialect repl)))
    (append (list :dialect :cljs)
            (when-let* ((target (replique-repl-target repl)))
              (list :target target)))))

(defun replique-dialect ()
  "Return the dialect the current buffer\\='s questions are about.

`:cljs' or `:clj' - see the commentary above for the three cases and for
why a repl buffer is one of them."
  (cond
   ((derived-mode-p 'replique-clojure-clojurescript-mode) :cljs)
   ((derived-mode-p 'replique-clojure-clojurec-mode)
    (replique-dialect--of-current-repl))
   ((derived-mode-p 'replique-clojure-mode) :clj)
   (t (replique-dialect--of-current-repl))))

(defun replique-dialect--of-current-repl ()
  "Return the dialect of the repl the commands act on, `:clj' for none.

Clojure where there is no repl, so that a buffer nothing is pointed at
asks what it asked before there was a second world to ask about."
  (if-let* ((repl (replique-repl-current)))
      (replique-repl-dialect repl)
    :clj))

(defun replique-dialect-keys ()
  "Return the dialect keys of a question about the current buffer.

Appended to a request by everything that asks the process about a name.
Nil for Clojure - see the commentary above.

The target is that of the repl this buffer's code would be evaluated in:
a .cljs buffer read while a node repl is open is a question about the
program that repl is running.  Asked of `replique-repl-for-dialect' and
not of whatever repl is in hand, so that a .cljs buffer read from beside
a Clojure repl is still a question about the ClojureScript that is
running - a Clojure repl carries no target at all, and the question would
have gone out about the process's default rather than about the node
program open in the next window.  With no ClojureScript repl anywhere the
key is absent and the process answers about its own default, which is
what it does for anything that does not say."
  (when (eq :cljs (replique-dialect))
    (append (list :dialect :cljs)
            (when-let* ((repl (replique-repl-for-dialect :cljs))
                        (target (replique-repl-target repl)))
              (list :target target)))))

(defun replique-repl-for-dialect (dialect)
  "Return the repl DIALECT\\='s code is evaluated in, or nil.

THE REPL OF THAT DIALECT AND NOT THE ONE IN HAND, which is the whole of
what this is for: a repl is chosen once and then stands, and the file
under the cursor changes every time somebody opens another one.

In order: the repl of the current buffer when it is a repl buffer, which
acts on itself whatever it is a repl of; then the repl the commands are
pointed at, when that is one of DIALECT; then the most recent live repl
of DIALECT of the current process.  Nil where the process has none.

NOTHING IS SELECTED BY ASKING.  Evaluating a .clj file does not move what
a .cljc file would be evaluated in, because the two questions have
different answers and only one of them was asked.  Which is replique 1\\='s
rule as well - there `replique/active-repl' reads a list that only
`replique/switch-active-repl' reorders.

A repl still shaking hands has not said what it is and reads as Clojure,
which is what the protocol says an absent dialect means.  The window is
between the socket opening and the reply arriving; what falls in it is
one command answered as though the repl being opened were not there yet,
which it very nearly is not."
  (or (and (replique-repl-live-p replique--buffer-repl) replique--buffer-repl)
      (and (replique-repl-live-p replique-current-repl)
           (eq dialect (replique-repl-dialect replique-current-repl))
           replique-current-repl)
      (when-let* ((process (replique-process-current)))
        (seq-find (lambda (repl)
                    (and (replique-repl-live-p repl)
                         (eq dialect (replique-repl-dialect repl))))
                  (replique-process--repls process)))))

(defun replique-repl--none-here ()
  "Return what to say when the current buffer has no repl to send code to.

NAMED AFTER THE BUFFER AND NOT AFTER THE DIALECT THAT WAS LOOKED FOR.  A
.cljc file is a file of both worlds and a repl of either would do, so
saying that no Clojure repl is open would be naming one of the two
answers that would have worked.  `replique-dialect' resolves a .cljc
buffer to whatever repl the commands are pointed at, and with no repl at
all that is Clojure - true of the lookup, and not the thing to say."
  (cond
   ((derived-mode-p 'replique-clojure-clojurescript-mode)
    "No ClojureScript repl - M-x replique-repl with a prefix argument")
   ((derived-mode-p 'replique-clojure-clojurec-mode)
    "No repl - M-x replique-repl")
   ((derived-mode-p 'replique-clojure-mode)
    "No Clojure repl - M-x replique-repl")
   (t "No repl - M-x replique-repl")))

(defun replique-repl-ensure-here ()
  "Return the repl this buffer\\='s code goes to, or signal that there is none.

What `replique-repl-ensure' is for a command about a repl, this is for a
command about a buffer - see the commentary above for which commands are
which."
  (replique-repl-ensure-for-dialect (replique-dialect)))

(defun replique-repl-ensure-for-dialect (dialect)
  "Return the repl DIALECT\\='s code goes to, or signal that there is none.

`replique-repl-ensure-here' asked about a dialect the caller names rather
than about the current buffer.  Which is what a buffer that is not the one
being talked about needs: a staleness buffer is showing what one language
has to compile, and the command that compiles it has to reach that
language\\='s repl whatever the commands are pointed at now."
  (or (replique-repl-for-dialect dialect)
      (user-error "%s" (replique-repl--none-here))))

;;; Sending code

(defun replique-repl-send-code (repl code &optional display echo)
  "Evaluate CODE in REPL.

CODE goes out as it is, over as many lines as it takes.  DISPLAY is what
the repl buffer is shown instead, CODE itself when it is nil: a source
directive is protocol rather than something the developer wrote, and a
transcript showing it is a transcript of the wire.  When ECHO, what the
one result the code is expected to produce printed and returned is shown
in the echo area too."
  (let ((conn (replique-repl--conn repl))
        (display (string-trim (or display code)))
        (code (string-trim code)))
    (unless (replique-conn-live-p conn)
      (user-error "The repl is closed"))
    ;; Written when the repl is ready for it rather than when it was sent:
    ;; the answer to what came before has not arrived yet, and a transcript
    ;; that shows the next form above the last result is a lie about what
    ;; happened
    (if (replique-repl--at-prompt repl)
        (replique-repl--insert repl (concat display "\n"))
      (setf (replique-repl--queued repl)
            (append (replique-repl--queued repl) (list display))))
    (when echo
      (setf (replique-repl--to-echo repl) (1+ (or (replique-repl--to-echo repl) 0))))
    (replique-conn-send-code conn code)))

(defun replique-repl-send-code-then (repl code display callback)
  "Evaluate CODE in REPL, and call CALLBACK with the frame that ended it.

`replique-repl-send-code-sync\=' without the waiting, and the same three
frames end an evaluation: the \"ret\" carrying what it printed and
returned, the \"exception\" carrying what it threw, or the \"error\"
saying the repl could not read it.  DISPLAY is what the repl buffer is
shown instead of CODE, as in `replique-repl-send-code\='.

FOR A COMMAND THAT IS SEVERAL EVALUATIONS IN A ROW, which is the one shape
that made the synchronous version necessary and does not need it: a
ClojureScript compile expands Clojure macros, so a whole-application
reload has to know the Clojure load ended before it starts the
ClojureScript one.  That is an ORDER and not a WAIT.  CALLBACK sends the
next one, and Emacs is free in between - which is the difference between
a reload that takes twenty seconds and an editor that is gone for twenty
seconds.

Only when the repl is at a prompt, for `replique-repl-send-code-sync\='s
reason and with one more consequence: nothing matches a frame to what
asked for it, so the evaluation that ends next is whichever was running.
A caller stepping through several repls has to ask again before each
step, because between two steps the repl is free and somebody may have
typed in it.

CALLBACK IS CALLED ON THE PROCESS FILTER, which is what it means for this
not to wait: whatever it does happens while Emacs is in the middle of
reading from a socket.  A repl that closes under it is a frame that never
comes, and a caller with something to finish has nothing to hang its
finishing on - which is the price of not holding the editor, and is why
this is for commands rather than for questions."
  (let ((conn (replique-repl--conn repl)))
    (unless (replique-conn-live-p conn)
      (user-error "The repl is closed"))
    (unless (replique-repl--at-prompt repl)
      (user-error "The repl is busy with something else"))
    (setf (replique-repl--on-end repl) callback)
    (replique-repl-send-code repl code display t)))

(defun replique-repl-send-code-sync (repl code &optional display)
  "Evaluate CODE in REPL, wait for it to end, and return the frame that ended it.

The \"ret\" frame carrying what it printed and returned, the \"exception\"
frame carrying what it threw, or the \"error\" frame saying the repl could
not read it - the caller reads which.
DISPLAY is what the repl buffer is shown instead of CODE, as in
`replique-repl-send-code\\='.

For a command that has to ask the process something once the code has run
and cannot say what it wants until then - loading what changed before
asking what uses a name, where the answer would otherwise be about the
files as the process last read them.  Everything else sends and carries
on: a repl is where somebody watches things happen, and holding the
editor still while they do is the opposite of that.

Only when the repl is at a prompt, so that the evaluation this waits for
is this one.  Frames carry no request id - a repl is a stream of what it
read and printed, not a set of answers to match up - so the evaluation
that ends next is whichever one was running, and the only way to know it
is ours is that nothing else was in flight.  Nothing else can be sent
while this waits, either: Emacs is inside it.

\\[keyboard-quit] is heard, and abandons the wait rather than the
evaluation - the code was sent and the process is running it, and the
repl buffer is where it goes on being watched.  `inhibit-quit\\=' around
the loop and `with-local-quit\\=' inside it are what make that a quit
between two reads rather than one out of the middle of one; the same
shape as `replique-conn-request-sync\\='."
  (let ((conn (replique-repl--conn repl)))
    (unless (replique-conn-live-p conn)
      (user-error "The repl is closed"))
    (unless (replique-repl--at-prompt repl)
      (user-error "The repl is busy with something else"))
    (let ((ended nil)
          (proc (replique-conn--proc conn)))
      (unwind-protect
          (progn
            (setf (replique-repl--on-end repl) (lambda (frame) (setq ended frame)))
            (replique-repl-send-code repl code display t)
            (let ((inhibit-quit t))
              (while (and (null ended)
                          (null quit-flag)
                          (replique-conn-live-p conn))
                (with-local-quit
                  ;; Only this process, for the reason
                  ;; `replique-conn-request-sync' gives: what another one
                  ;; wrote is not what this wait is about
                  (accept-process-output proc 0.1 nil t)))
              (cond
               ;; Before the frame, so that a C-g pressed as the evaluation
               ;; ended is a C-g: what it was for is no longer wanted either
               (quit-flag (setq quit-flag nil) (signal 'quit nil))
               (ended ended)
               (t (user-error "The repl closed while it was evaluating")))))
        (setf (replique-repl--on-end repl) nil)))))

(defun replique-repl-send-directive (repl directive)
  "Write DIRECTIVE on REPL, showing nothing in its buffer.

A directive is not a form.  It has no result, so there is nothing for the
transcript to show under it, and nobody wrote it, so there is nothing to
show as having been typed either.  What comes back is the prompt of the
next read, which is where the answer is: a directive that moved the repl
moved the namespace the prompt says.

Not `replique-repl-send-code', which is about forms and which would put a
blank line in the buffer for something nobody wrote."
  (let ((conn (replique-repl--conn repl)))
    (unless (replique-conn-live-p conn)
      (user-error "The repl is closed"))
    ;; The blank line is what makes the prompt come back.  A directive is
    ;; consumed and then the reader goes on looking for the form it is about;
    ;; it is the end of a line with nothing pending that tells the repl there
    ;; is nothing more coming, and a repl with nothing to read prints a prompt
    (replique-conn-send-code conn (concat directive "\n"))))

;;; Commands

(defun replique-interrupt ()
  "Interrupt what the current repl is evaluating.

This interrupts the thread: it stops code that blocks or that checks the
interrupt flag, and nothing else.  A repl waiting for the next form is
left alone."
  (interactive)
  (let* ((repl (replique-repl-ensure))
         (process (replique-repl--process repl))
         (id (replique-conn--id (replique-repl--conn repl))))
    (replique-process-request
     process (list :op :interrupt :connection id)
     (lambda (frame)
       (cond
        ((equal "error" (plist-get frame :tag))
         (message "replique: could not interrupt: %s" (plist-get frame :message)))
        ((eq t (plist-get frame :interrupted)) (message "replique: interrupted"))
        (t (message "replique: the repl was not evaluating")))))))

(defun replique-quit-repl ()
  "End the current repl, as :repl/quit does at any Clojure socket repl."
  (interactive)
  (let ((repl (replique-repl-ensure)))
    (replique-conn-send-code (replique-repl--conn repl) ":repl/quit")))

(defun replique-show-last-exception ()
  "Browse the last exception of the current repl."
  (interactive)
  (let* ((repl (replique-repl-ensure))
         (frame (replique-repl--last-exception repl)))
    (unless frame (user-error "No exception yet"))
    (replique-exception-show (plist-get frame :exception)
                             (plist-get frame :message)
                             (plist-get frame :phase)
                             "at the repl")))

(defconst replique-kill-timeout 2
  "How long to wait for a process to stop, in seconds, before killing it.")

(defun replique-process--stop (proc)
  "Stop PROC, the operating system process Emacs started.

An interrupt rather than a kill: the jvm answers it by running its
shutdown hooks, and one of them deletes the port file the process wrote.
A process killed outright leaves that file behind, and `replique-connect'
goes on offering a process that is not there.

Waited for rather than left to happen, so that what the command says it
did is done when it returns.  A jvm that will not go is killed anyway -
whatever it is doing, it is holding a port and a file that say it is
listening."
  (interrupt-process proc)
  (let ((limit (+ (float-time) replique-kill-timeout)))
    (while (and (process-live-p proc) (< (float-time) limit))
      (accept-process-output nil 0.05)))
  (when (process-live-p proc)
    (delete-process proc)))

(defun replique-process--ask-to-stop (process)
  "Ask PROCESS to stop, and wait for it to go.  Return non-nil when it went.

The only way that reaches a process Emacs did not start: it is no child of
this Emacs, there is nothing to signal, and after Emacs restarts none of
the processes it is connected to are.  What says it went is the control
connection closing, which is what the process leaving does to it."
  (let ((conn (replique-process--control process)))
    (when (replique-conn-live-p conn)
      (replique-conn-request conn (list :op :shutdown) nil)
      (let ((limit (+ (float-time) replique-kill-timeout)))
        (while (and (replique-conn-live-p conn) (< (float-time) limit))
          (accept-process-output nil 0.05)))
      (not (replique-conn-live-p conn)))))

(defun replique-process--close (process)
  "Close the connections to PROCESS and forget it.

The repl buffers are left as they are: what a repl printed is what was
printed, and a connection that closed says so in the buffer it belonged
to."
  (dolist (repl (replique-process--repls process))
    (replique-conn-close (replique-repl--conn repl)))
  (replique-conn-close (replique-process--control process))
  (replique-process--forget process))

(defun replique-disconnect (process)
  "Let go of PROCESS: close the connections to it and leave it running.

What to do with a process that is not yours to stop - one that belongs to
a terminal, or to whoever is working on the machine it runs on.  It goes
on running and its port file goes on saying where it is, so
`replique-connect' finds it again."
  (interactive (list (replique-process-ensure)))
  (replique-process--close process)
  (message "replique: let go of %s" (replique-process--id process)))

(defun replique-kill-process (process)
  "Stop PROCESS and close the connections to it.

Asked before it is signalled: asking is what works for a process Emacs did
not start, and a signal is what is left for one that will not answer -
which Emacs can only send to a process of its own.  Either way the process
runs its shutdown hooks, and one of them deletes the port file: what stops
here stops saying it is running.

A process that answers neither - one Emacs did not start, which did not go
when it was asked - is left running, and saying so is all this can do about
it.  Silence there would be this command behaving the way
`replique-disconnect' does, under the name that promises the opposite.

To let go of a process without stopping it, see `replique-disconnect'."
  (interactive (list (replique-process-ensure)))
  (let* ((id (replique-process--id process))
         (proc (replique-process--proc process))
         (stopped (replique-process--ask-to-stop process)))
    (replique-process--close process)
    (when (process-live-p proc)
      (replique-process--stop proc)
      (setq stopped t))
    (if stopped
        (message "replique: stopped %s" id)
      (message (concat "replique: %s would not stop - Emacs did not start it,"
                       " so there is nothing to signal.  Its port file goes on"
                       " naming it")
               id))))

(defun replique-switch-to-repl ()
  "Show the buffer of the current repl."
  (interactive)
  (pop-to-buffer (replique-repl--buffer (replique-repl-ensure))))

(provide 'replique-repl)

;;; replique-repl.el ends here
