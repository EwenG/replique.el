;;; replique-common.el --- What the rest of replique shares  -*- lexical-binding: t; -*-

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

;; The customization group and the faces.  They live in a file of their own
;; because a defcustom needs its group to exist by the time it is read, and
;; the two buffers that render what a process says - the repl and the process
;; output - are at different levels of the require chain.  Putting the group
;; in whichever of them happens to load first would make its home look like
;; an accident.
;;
;; Everything a face is used for here is text a process produced.  What
;; replique says about a process rather than what the process said is the one
;; exception, and it is deliberately quiet.
;;
;; The other thing they share is what to do about output that arrived in a
;; buffer no window shows.  It is said twice, because either one alone is a
;; way of not being told: in the global mode line, which names the buffer and
;; how much of it there is and waits to be read, and in the echo area, which
;; says what arrived while it is arriving.  A message on its own is gone by
;; the time anybody looks up; a mark on its own appeared once and then stood
;; still through the thousand lines that came after the first.

;;; Code:

(require 'comint)
(require 'seq)
(require 'subr-x)

(defgroup replique nil
  "A development environment for Clojure."
  :group 'tools)

(defface replique-stderr
  '((t (:inherit error)))
  "Face for what a process prints on its error stream."
  :group 'replique)

(defface replique-note
  '((t (:inherit shadow :slant italic)))
  "Face for what replique says about a process rather than what it printed."
  :group 'replique)

(defface replique-prompt
  '((t (:inherit comint-highlight-prompt)))
  "Face for the prompt of a repl."
  :group 'replique)

(defface replique-exception
  '((t (:inherit error)))
  "Face for what a form threw."
  :group 'replique)

;;; Which text in a repl buffer is the repl talking
;;
;; A repl buffer is read as Clojure - what is typed at the prompt is Clojure,
;; and the parse that reads it covers the transcript with it.  Which is mostly
;; harmless, because what a form printed parses as whatever it happens to look
;; like and nothing asks it anything.  The prompt is where it stops being
;; harmless: `user=>' reads as a symbol, so it is a form, so it is the form
;; before point everywhere point usually is - the end of the buffer, just after
;; the last one.  A command looking for the value that was printed found the
;; prompt instead.
;;
;; So the prompt says it is a prompt, in a property of its own.  Not the face,
;; which somebody may set to anything; not the read only property, which is
;; `replique-prompt-read-only' and may be off; and not comint's own
;; `comint-last-prompt', which knows about one of them.  The one place that
;; knows a prompt is a prompt is the code that writes one.

(defconst replique-prompt-property 'replique-prompt
  "The text property that marks a repl prompt as being one.")

(defun replique-prompt-text (string)
  "Return STRING written the way a repl prompt is written.

Its face and the property that says what it is, together, because the two
are one decision: text that looks like a prompt and does not say so is
what this exists to stop."
  (propertize string 'face 'replique-prompt replique-prompt-property t))

(defun replique-prompt-at-p (pos)
  "Return non-nil when POS holds prompt text."
  (get-text-property pos replique-prompt-property))

;;; Output nothing has seen

(defcustom replique-track-unread t
  "Whether the mode line names the buffers holding output nobody has seen.

What a repl answers to a form sent from a buffer is not that: its result
is put in the echo area where the form was sent from.  This is for what
arrives on its own - what a future printed, what a thread threw.

Named with how much of it there is, and that number moves as more
arrives.  A mark that appeared once and then stood still says the same
thing whether one line came or a thousand, and says nothing at all about
the second thousand."
  :type 'boolean
  :group 'replique)

(defcustom replique-echo-unread t
  "Whether output that arrives on its own is also shown in the echo area.

The mode line says there is something to read and waits to be read; this
says what it was, while it is happening.  Both, because they answer
different questions: a name in the mode line is no use to somebody not
looking down there, and a message is gone by the time anybody comes back
to the frame.

Only where nothing was asked for.  A form evaluated from a buffer reports
where it was sent from - see `replique-echo-results\\=' - and what a
background thread printed in the meantime does not get to overwrite it."
  :type 'boolean
  :group 'replique)

(defface replique-unread
  '((t (:inherit mode-line-emphasis)))
  "Face naming a buffer with unseen output, in the mode line.

Not `replique-unread' inheriting `shadow' like the rest of what replique
says about itself: `shadow' is a foreground picked to recede against the
background of a buffer, and a mode line has neither that background nor
that purpose.  `mode-line-emphasis' is what a theme defines for
something a mode line should be read for, so it is legible wherever the
mode line is."
  :group 'replique)

(defvar replique--unread '()
  "What arrived where nobody saw it, as a list of (BUFFER . LINES).

In the order they did it in, so that the mode line reads as things
happened.")

(defun replique--unread-lines (string)
  "Return how many lines STRING ended."
  (if string (seq-count (lambda (character) (= character ?\n)) string) 0))

(defun replique--unread-name (buffer)
  "Return the short name of BUFFER for the mode line.

The decoration a buffer name carries is what tells a buffer apart from a
file in a buffer list; in a mode line naming nothing else it is noise."
  (let* ((name (buffer-name buffer))
         (name (replace-regexp-in-string "\\`\\*\\|\\*\\(<[0-9]+>\\)?\\'" "" name)))
    (replace-regexp-in-string "\\`replique\\(: \\|-\\)" "" name)))

(defun replique--unread-entry (entry)
  "Return the mode line entry for ENTRY, a buffer and the lines it holds.

At least one line: what is counted is newlines, and the first thing
written is a line that has not ended yet.  Nought would read as nothing
having arrived, which is the one thing this is here to deny."
  (let* ((buffer (car entry))
         (lines (max 1 (cdr entry))))
    (propertize (format "%s (%d)" (replique--unread-name buffer) lines)
                'face 'replique-unread
                'mouse-face 'mode-line-highlight
                'help-echo (format "%s: %d lines nothing has seen\nmouse-1: show it"
                                   (buffer-name buffer) lines)
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1]
                                         (lambda ()
                                           (interactive)
                                           (when (buffer-live-p buffer)
                                             (pop-to-buffer buffer))))
                             map))))

(defun replique-unread-mode-line ()
  "Return the global mode line description of output nothing has seen."
  (let ((entries (seq-filter (lambda (entry) (buffer-live-p (car entry)))
                             replique--unread)))
    (if (null entries)
        ""
      (concat " " (mapconcat #'replique--unread-entry entries ",")))))

(defvar replique--unread-mode-line '(:eval (replique-unread-mode-line))
  "The `global-mode-string' element naming the buffers nobody has read.")

(defun replique--unread-install ()
  "Put the unread element in the global mode line, once.

Put there when there is something to say rather than when replique is
loaded: an editor that has not started a process has no reason to have
been changed."
  (or global-mode-string (setq global-mode-string '("")))
  (unless (member replique--unread-mode-line global-mode-string)
    (setq global-mode-string
          (append global-mode-string (list replique--unread-mode-line)))))

(defun replique--unread-mark (buffer string)
  "Note in the mode line that BUFFER received STRING and nobody saw it.

The count is what changes about a buffer already named there.  Noting it
once was enough while the mark was the whole of what was being said, and
a mark that is already up is not a way of saying that more has come."
  (replique--unread-install)
  (let ((entry (assq buffer replique--unread))
        (lines (replique--unread-lines string)))
    (if entry
        (setcdr entry (+ (cdr entry) lines))
      (setq replique--unread
            (append replique--unread (list (cons buffer lines))))))
  (force-mode-line-update t))

;;; What arrived while nobody was looking, in the echo area

;; Gathered rather than said as it comes.  Output arrives in whatever chunks
;; the operating system handed over, so a message per chunk is the last
;; fragment of a line flashing past where a line was wanted - and a thread
;; printing in a loop is an echo area nothing else can use.  An idle timer is
;; the clock: a burst becomes one message, and idle is the moment nothing
;; else is saying anything.

(defconst replique-echo-max-lines 10
  "How many lines of what a process produced the echo area shows.")

(defconst replique-echo-max-chars 1000
  "How much of what a process produced the echo area shows, in characters.")

(defconst replique--echo-delay 0.2
  "How long Emacs is left idle before what arrived unseen is said.")

(defvar replique-echo-awaited-function nil
  "A function of no arguments saying an evaluation is about to report.

Nil here and set by `replique-repl\\=' where that is loaded: what owns the
echo area is an evaluation, an evaluation belongs to a repl, and a repl
is not something this file knows about.  See `replique-echo-unread\\='.")

(defvar replique--echo-pending nil
  "What is about to be said, as (BUFFER . TEXT).")

(defvar replique--echo-timer nil
  "The timer that will say what `replique--echo-pending\\=' holds.")

(defun replique-echo-shorten (text buffer)
  "Return TEXT cut down to what the echo area should hold, naming BUFFER.

A form that printed a thousand lines is not a message.  Where it was cut
the buffer holding the whole of it is named, so that a cut reads as one."
  (let* ((cut (> (length text) replique-echo-max-chars))
         (text (if cut (substring text 0 replique-echo-max-chars) text))
         (lines (split-string text "\n"))
         (cut (or cut (> (length lines) replique-echo-max-lines)))
         (text (string-join (seq-take lines replique-echo-max-lines) "\n")))
    (if cut
        (concat text (propertize (format " ... see %s" (buffer-name buffer))
                                 'face 'replique-note))
      text)))

(defun replique--echo-flush ()
  "Say what arrived in a buffer nobody was looking at.

Dropped rather than kept for later where the echo area is somebody
else's.  What is being reported is that something arrived just now, and
the mode line is what goes on saying so afterwards."
  ;; Cancelled rather than only forgotten: this is called by the timer, and
  ;; also by a second buffer writing before it fired
  (when replique--echo-timer (cancel-timer replique--echo-timer))
  (setq replique--echo-timer nil)
  (let ((buffer (car replique--echo-pending))
        (text (cdr replique--echo-pending)))
    (setq replique--echo-pending nil)
    (when (and (buffer-live-p buffer)
               text
               (not (string-empty-p (string-trim text)))
               ;; Somebody is typing an answer to something
               (null (active-minibuffer-window))
               ;; Somebody is about to be told what they asked for
               (not (and replique-echo-awaited-function
                         (funcall replique-echo-awaited-function)))
               ;; Somebody went and looked
               (not (get-buffer-window buffer 'visible)))
      (message "%s" (replique-echo-shorten (string-trim-right text "\n+") buffer)))))

(defun replique--echo-note (buffer string face)
  "Gather STRING, which arrived unseen in BUFFER, for the echo area.

FACE is what it is shown in, which is the face it went into the buffer
in: a line that came on standard error then reads as one without having
to be read.

Bounded, like what is kept for an evaluation: a thread printing in a loop
must not be accumulated in full for the sake of the ten lines of it that
will be shown."
  (when (and string (not (string-empty-p string)))
    (unless (eq buffer (car replique--echo-pending))
      ;; Another buffer's turn, and a message of its own rather than one
      ;; made of the two of them
      (replique--echo-flush))
    (let* ((kept (or (cdr replique--echo-pending) ""))
           (room (- (1+ replique-echo-max-chars) (length kept))))
      (when (> room 0)
        (let ((string (if (> (length string) room) (substring string 0 room) string)))
          (setq replique--echo-pending
                (cons buffer (concat kept (if face
                                              (propertize string 'face face)
                                            string)))))))
    (unless (and replique--echo-timer (memq replique--echo-timer timer-idle-list))
      (setq replique--echo-timer
            (run-with-idle-timer replique--echo-delay nil #'replique--echo-flush)))))

(defun replique-note-unread (buffer &optional string face)
  "Note that BUFFER received STRING, shown in FACE, while no window showed it.

Named apart from `replique-track-unread\\=', the setting it reads: one
symbol that is both a variable and a function is a symbol whose two
descriptions are about different things.

Two things are said about the one fact and they are not the same thing:
the mode line names the buffer and waits to be read, the echo area says
what arrived while it is arriving.  See `replique-echo-unread\\='."
  (when (and (buffer-live-p buffer)
             (not (get-buffer-window buffer 'visible)))
    (when replique-track-unread
      (replique--unread-mark buffer string))
    (when replique-echo-unread
      (replique--echo-note buffer string face))))

(defun replique--unread-seen (&rest _)
  "Forget the buffers now on screen, and the ones that are gone.

Shown is read enough: what the mode line offers is a way to the buffer,
and it has been taken."
  (when replique--unread
    (let ((left (seq-filter (lambda (entry)
                              (and (buffer-live-p (car entry))
                                   (not (get-buffer-window (car entry) 'visible))))
                            replique--unread)))
      (unless (equal left replique--unread)
        (setq replique--unread left)
        (force-mode-line-update t)))))

;; Rather than a timer: a buffer becomes visible by being put in a window,
;; and this is run when one changes the buffer it shows
(add-hook 'window-buffer-change-functions #'replique--unread-seen)

;;; Buffers

(defun replique-insert-output (buffer string &optional face)
  "Insert STRING at the end of BUFFER, with FACE.

A window already at the end follows the output, and one further up is
left where the reader put it."
  (when (and (buffer-live-p buffer) string (not (string-empty-p string)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (windows (seq-filter (lambda (w) (= (window-point w) (point-max)))
                                 (get-buffer-window-list buffer nil t)))
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          ;; `face' rather than `font-lock-face': these buffers have no font
          ;; lock to honour the latter, so it would simply not be coloured
          (insert (if face (propertize string 'face face) string)))
        (when at-end (goto-char (point-max)))
        (dolist (w windows) (set-window-point w (point-max)))))
    (replique-note-unread buffer string face)))

;;; Code read out of an archive

;; A definition inside a jar has no file for Emacs to visit - there is no path
;; to a file inside an archive - so the entry is read out into a buffer of its
;; own, and that buffer is given a name made of the two of them: see
;; `replique-symbol--visit-entry\='.
;;
;; That name is not a path, and nothing but the buffer that made it knows how
;; to take it apart again.  So the two halves are kept on the buffer instead,
;; because a command asked to do something with what is in one has to be able
;; to say to the process what it is.

(defvar-local replique-archive-file nil
  "The archive this buffer was read out of, or nil where it holds a file.")

(defvar-local replique-archive-entry nil
  "Which entry of `replique-archive-file\=' this buffer holds.")

;; Kept through a change of major mode, which is otherwise where they would
;; go: turning a mode on kills the local variables of the buffer, and the
;; buffer is given its mode after it has been filled - `set-auto-mode\=' reads
;; the name it was given and the text that was put in it.  What these say is
;; not about the mode anyway.  It is what the buffer holds, which is the same
;; whichever mode is reading it.
(put 'replique-archive-file 'permanent-local t)
(put 'replique-archive-entry 'permanent-local t)

(defun replique-buffer-file ()
  "Return what to tell the process this buffer is, or nil for nothing to tell.

A property list holding the :file, and the :entry beside it where that
file is an archive - which is how a file inside a jar is written
throughout the protocol, and how the process answers where a definition
was written.  So what comes back from asking about a name is the same
shape as what goes out to act on it.

Nil where the buffer is neither, which is a buffer holding nothing the
process could be pointed at: a scratch buffer, or a repl."
  (cond
   ((and replique-archive-file replique-archive-entry)
    (list :file replique-archive-file :entry replique-archive-entry))
   ((buffer-file-name) (list :file (buffer-file-name)))))

(provide 'replique-common)

;;; replique-common.el ends here
