;;; replique-fresh.el --- Loading what changed before asking about it  -*- lexical-binding: t; -*-

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

;; Offering to load what changed, before asking the process something that
;; is answered out of what it loaded.
;;
;; Where a name is used is answered from what the compiler wrote down while
;; it compiled the files - which is what the files said when it read them.
;; A file edited since is a file the answer is wrong about, and wrong
;; silently: a use that was deleted is still listed, one that was written
;; is not listed at all, and nothing in the list says which files it is
;; out of date about.  The same goes for a file nobody edited that expands
;; a macro of one that was - it holds the expansion the old macro made, and
;; the uses recorded in it are that expansion's.
;;
;; Emacs offers to save a buffer before doing something that reads the
;; file.  This offers to load what changed before asking something that
;; reads what was loaded, for the same reason: the thing about to be done
;; is about to be done to the wrong text, and the one who knows whether
;; that matters is the one who edited it.
;;
;; The offer costs one question to the process, which reads the
;; modification time of every file it compiled - a few milliseconds on a
;; project of a thousand files, against an answer that is wrong without it.
;; Nothing is asked and nothing is said when nothing changed, which is
;; almost always.
;;
;; Declining is a real answer and is taken as one: the question is asked
;; anyway, of the files as they were read, and what that leaves out is said
;; once rather than left to be discovered.

;;; Code:

(require 'subr-x)
(require 'replique-common)
(require 'replique-eval)
(require 'replique-name)
(require 'replique-process)
(require 'replique-repl)

(defcustom replique-reload-before-asking 'ask
  "What to do when the process has not read the files as they are now.

Before a question the process answers out of what it compiled - where a
name is used - and only when something has in fact changed.

`ask\\=' offers to load what changed, which is what \\[replique-reload-all]
does, and waits for it before asking.  `always\\=' loads it without asking.
`never\\=' asks the process nothing about it, and so never offers, never
waits, and never says that an answer is out of date - the answer is
whatever the process last read, which is what this was before there was
anything to set."
  :type '(choice (const :tag "Offer to load what changed" ask)
                 (const :tag "Load what changed" always)
                 (const :tag "Ask about the files as they were read" never))
  :group 'replique)

(defun replique-fresh--asked (process)
  "Return the frame PROCESS answers the `:stale\\=' op with, or nil.

The whole frame, with its two lists as they came: what is decided here is
whether to load, which both halves are loaded by, and they are told apart
only long enough to be counted - see `replique-fresh--question\\='.  A
process with nothing to load answers the two lists empty, which is a frame
like any other.

Nil where the process did not answer, which is a process whose compiler
records nothing, and a process that did not answer in time.  There is
nothing to offer either of them, and the question this was going to guard
says so itself if there is anything to say - said once, where it is about
to be said anyway, rather than twice."
  (let ((frame (replique-process-request-sync
                process (list :op :stale) replique-name-timeout)))
    (and frame (not (equal "error" (plist-get frame :tag))) frame)))

(defun replique-fresh--name (found)
  "Return what to call the one file FOUND, in a question about it.

Its name without the directories, which is how somebody names the file
they were just editing.  An entry of an archive is named as the entry it
is - nobody edits one, so this is only ever reached by a file that is one
of a list."
  (let ((file (plist-get found :file)))
    (or (plist-get found :entry)
        (and file (file-name-nondirectory file))
        "a file")))

(defun replique-fresh--question (changed stale what)
  "Return the question to ask before WHAT, about CHANGED and STALE.

The two counts rather than one, because they are two different facts and
the second is the one nobody can see: these files were not edited and are
out of date all the same, because they expand a macro of one that was.
CHANGED is never empty here - nothing is stale without something having
changed - so it is what the singular is decided by."
  (let ((one (and (null (cdr changed)) (null stale))))
    (concat (if one
                (format "%s changed" (replique-fresh--name (car changed)))
              (format "%d files changed" (length changed)))
            (if stale
                (format ", %d more need compiling" (length stale))
              "")
            ".  Load " (if one "it" "them") " before " what "? ")))

(defun replique-fresh--warn (files)
  "Say that FILES are not what the answer about to be given was read from.

Once, in the echo area, where whoever declined the offer is looking.  The
answer itself cannot say it: what it leaves out is the uses written since
the process read these files, and something that is not in a list cannot
be marked in it."
  (message "replique: %s"
           (if (cdr files)
               (format (concat "%d files have changed since the process read them"
                               " - what they say now is not in this")
                       (length files))
             (format (concat "%s has changed since the process read it"
                             " - what it says now is not in this")
                     (replique-fresh--name (car files))))))

(defun replique-fresh--reload ()
  "Load what changed, waiting for it, and return nil when it all loaded.

Stops on anything but a value, which is the loading having ended by
loading everything.  A file that will not compile leaves the
process holding some of the new files and some of the old, and the one
that threw is still what it was - so the answer that was going to be
asked for would be out of a model half way through being brought up to
date, which is worse than the one that was refused.  What threw is in the
repl buffer, whole, which is where somebody fixes it."
  (let ((frame (replique-reload-all t)))
    (unless (equal "ret" (plist-get frame :tag))
      (user-error "Replique: the load stopped: %s" (plist-get frame :message)))
    nil))

(defun replique-fresh-ensure (what)
  "Offer to load what changed before the process is asked WHAT.

WHAT is what is about to be asked for, written to follow \"before\" - so
\"finding every use of a name\" and not \"find every use of a name\".

Returns what the process still has not read as it now is: nothing when
nothing had changed, nothing when what changed was just loaded, and the
files themselves when the offer was declined.  Which has been said once
already, in the echo area - the return value is for a caller with
somewhere better to say it than that.

Nothing is asked of the process when there is nothing to do with the
answer: `replique-reload-before-asking\\=' set to `never\\=', or no repl
to load anything in.  A load happens in a repl - it compiles, it prints,
and it is interrupted there - so a process nobody has opened one on is
told nothing and asked nothing."
  (unless (eq replique-reload-before-asking 'never)
    (when-let* ((repl (replique-repl-current))
                (process (replique-repl-process repl))
                (found (replique-fresh--asked process)))
      (let* ((changed (plist-get found :changed))
             (stale (plist-get found :stale))
             (files (append changed stale)))
        (when files
          (if (or (eq replique-reload-before-asking 'always)
                  (y-or-n-p (replique-fresh--question changed stale what)))
              (replique-fresh--reload)
            (replique-fresh--warn files)
            files))))))

(provide 'replique-fresh)

;;; replique-fresh.el ends here
