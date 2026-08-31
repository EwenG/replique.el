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
;; RET sends what is at the prompt, or inserts a newline while what is there
;; is not a form yet, so that a form spanning several lines can be typed
;; rather than pasted.  It only asks that where forms are what is being read:
;; the code a repl evaluates is handed a real stdin, and a line typed to a
;; form that is running is finished when the developer says it is.
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

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'comint)
(require 'replique-common)
(require 'replique-edn)
(require 'replique-conn)
(require 'replique-exception)
(require 'replique-process)

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

(defconst replique-repl--echo-max-lines 10
  "How many lines of what a form produced the echo area shows.")

(defconst replique-repl--echo-max-chars 1000
  "How much of what a form produced the echo area shows, in characters.")

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
exception it carries: showing one takes the message and the phase too."
  process conn buffer ns params at-prompt to-echo echoed queued last-exception)

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

(defun replique-repl--unattended (repl)
  "Note output in the buffer of REPL that nothing is waiting for.

What a buffer asked for is reported where it was asked from.  The rest
is only ever in the repl buffer, and a repl buffer no window shows is
where output goes to not be read."
  (when (= 0 (or (replique-repl--to-echo repl) 0))
    (replique-note-unread (replique-repl--buffer repl))))

(defun replique-repl--echo-keep (repl string)
  "Keep STRING, printed by the form REPL is answering, for the echo area.

Only what a buffer is waiting for: what arrives on its own is nothing
anybody asked to be told, and the mode line is what says it arrived.
Kept up to one character past what the echo area shows, which is what
tells a whole one from a cut one."
  (when (and replique-echo-results
             (> (or (replique-repl--to-echo repl) 0) 0))
    (let* ((kept (or (replique-repl--echoed repl) ""))
           (room (- (1+ replique-repl--echo-max-chars) (length kept))))
      (when (> room 0)
        (setf (replique-repl--echoed repl)
              (concat kept (if (> (length string) room)
                               (substring string 0 room)
                             string)))))))

(defun replique-repl--echo-shorten (repl text)
  "Return TEXT cut down to what the echo area of REPL should hold.

A form that printed a thousand lines is not a message.  Where it was cut
the buffer holding the whole of it is named, so that a cut reads as one."
  (let* ((cut (> (length text) replique-repl--echo-max-chars))
         (text (if cut (substring text 0 replique-repl--echo-max-chars) text))
         (lines (split-string text "\n"))
         (cut (or cut (> (length lines) replique-repl--echo-max-lines)))
         (text (string-join (seq-take lines replique-repl--echo-max-lines) "\n")))
    (if cut
        (concat text (propertize
                      (format " ... see %s" (buffer-name (replique-repl--buffer repl)))
                      'face 'replique-note))
      text)))

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
                 (replique-repl--echo-shorten
                  repl
                  (if (and printed (not (string-empty-p (string-trim printed))))
                      (concat (string-trim-right printed "\n+") "\n" string)
                    string)))))))

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

(defun replique-repl--frame (repl frame)
  "Render FRAME in the buffer of REPL."
  (pcase (plist-get frame :tag)
    ("out"
     (replique-repl--unattended repl)
     (replique-repl--echo-keep repl (plist-get frame :string))
     (replique-repl--insert repl (plist-get frame :string)))
    ("err"
     (replique-repl--unattended repl)
     (replique-repl--echo-keep repl (plist-get frame :string))
     (replique-repl--insert repl (plist-get frame :string) 'replique-stderr))
    ("ret"
     (replique-repl--unattended repl)
     (let ((value (plist-get frame :value)))
       (replique-repl--insert repl (concat value "\n"))
       (replique-repl--echo repl value)))
    ("exception"
     (replique-repl--unattended repl)
     (let* ((message (plist-get frame :message))
            (phase (plist-get frame :phase))
            (exception (plist-get frame :exception))
            (truncated (and exception (replique-repl--truncation exception))))
       (setf (replique-repl--last-exception repl) frame)
       ;; The line is the way to the whole of it: what a developer looks at
       ;; first is what they would click
       (replique-repl--insert
        repl
        (replique-exception-button (concat message (or truncated "") "\n")
                                   exception message phase "at the repl")
        'replique-exception)
       (replique-repl--echo repl message)))
    ("prompt"
     (setf (replique-repl--ns repl) (plist-get frame :ns))
     (setf (replique-repl--params repl) (plist-get frame :params))
     ;; A form is not what produces a prompt - a read error and a comment
     ;; produce one too - so the buffer is what says whether one is needed
     (unless (replique-repl--at-prompt repl)
       (replique-repl--insert repl (format "%s=> " (plist-get frame :ns))
                              'replique-prompt)
       (setf (replique-repl--at-prompt repl) t))
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
                            'replique-exception))
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

A repl buffer is a comint buffer, so the syntax of what is typed in it is
not the syntax of any mode it is in.  All this is asked is where the
delimiters, the strings and the comments are, which is little enough that
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
same."
  (save-excursion
    (goto-char (process-mark proc))
    (looking-back comint-prompt-regexp (line-beginning-position))))

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

(defvar replique-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'replique-interrupt)
    (define-key map (kbd "C-c C-q") #'replique-quit-repl)
    (define-key map (kbd "C-c C-e") #'replique-show-last-exception)
    (define-key map (kbd "RET") #'replique-repl-return)
    map)
  "Keymap of a repl buffer.")

(define-derived-mode replique-repl-mode comint-mode "Replique"
  "Major mode for a replique REPL.

\\{replique-repl-mode-map}"
  (setq-local comint-prompt-regexp "^[^ \n]*=> *")
  (setq-local comint-prompt-read-only replique-prompt-read-only)
  (setq-local comint-input-sender #'replique-repl--input-sender)
  (setq-local comint-process-echoes nil)
  (setq-local mode-line-process '(:eval (replique-repl--mode-line))))

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
  "Send what is at the prompt, or start a new line while it is not a form yet.

Which is what makes a form that spans several lines something that can be
typed rather than pasted.  Whether it is finished is only asked of what
is typed where the repl is reading forms - see
`replique-repl--reading-a-form-p\=' - and never of what is typed to a form
that is running.

With a prefix argument, ANYWAY, send what is there whatever state it is
in.  Where the text ends is a guess made about text nothing has read yet,
and a guess is worth a way to overrule it.  A newline in input that is
already finished is what quoted insert, and \\[open-line], are for.

Point above the input sends what is under it, the way `comint-send-input\='
does: everything above the prompt is a transcript, and a transcript is
read rather than continued."
  (interactive "P")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (not anyway)
             proc
             (>= (point) (process-mark proc))
             (replique-repl--reading-a-form-p proc)
             (replique-repl--unfinished-p (process-mark proc) (point-max)))
        ;; Not `newline\=': there is no indenting a comint buffer - the line
        ;; above the input can be anything the process printed - and
        ;; `electric-indent-mode\=' would try
        (insert "\n")
      (comint-send-input))))

;;; Opening

(defun replique-repl--buffer-name (process)
  "Return a name for a repl buffer of PROCESS."
  (generate-new-buffer-name (format "*replique: %s*" (replique-process--id process))))

;;;###autoload
(defun replique-repl (&optional process)
  "Open a REPL on PROCESS, the current process by default."
  (interactive)
  (let* ((process (or process (replique-process-ensure)))
         (buffer (get-buffer-create (replique-repl--buffer-name process)))
         (repl (replique-repl--make :process process :buffer buffer :to-echo 0)))
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
          :on-ready (lambda (conn)
                      (let ((proc (replique-conn--proc conn)))
                        (process-put proc 'replique-repl repl)
                        (with-current-buffer buffer
                          (goto-char (point-max))
                          (set-marker (process-mark proc) (point))
                          (run-hooks 'comint-exec-hook))))
          :on-frame (lambda (frame) (replique-repl--frame repl frame))
          :on-close (lambda (_conn)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (let ((inhibit-read-only t))
                            (save-excursion
                              (goto-char (point-max))
                              (insert (propertize "\nThe connection is closed\n"
                                                  'face 'replique-note))))))
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
itself - then the one `replique-select-repl\=' chose, then the most recent
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
      (message "replique: %s" choice))))

(defun replique-repl-ensure ()
  "Return the repl the commands act on, or signal that there is none."
  (or (replique-repl-current)
      (user-error "No repl - M-x replique-repl")))

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
A process killed outright leaves that file behind, and `replique-connect\='
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
`replique-connect\=' finds it again."
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
`replique-disconnect\=' does, under the name that promises the opposite.

To let go of a process without stopping it, see `replique-disconnect\='."
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
