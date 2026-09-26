;;; replique-stale.el --- What has to be loaded again  -*- lexical-binding: t; -*-

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

;; What a reload would load, looked at before it does it.
;;
;; Two lists, because they are two different facts about a file.  The copy
;; on the disk is newer than what the process read, which is a file that
;; was edited; or the file was not edited and what the process holds of it
;; is out of date all the same, because it expands a macro of a file that
;; was.  The second is the half nobody can work out by looking at their
;; buffers, and it is the reason this is worth a command of its own: what
;; needs compiling is not the same list as what was typed in.
;;
;; Asked of the process rather than worked out here.  What changed is a
;; comparison against the time the process read each file, which nothing
;; but the process knows, and what that made stale is a graph its compiler
;; recorded while it compiled - see `replique-reload-all', which is the
;; same question answered by doing it.
;;
;; AND ASKED ABOUT ONE LANGUAGE.  A process can hold a Clojure application
;; and a ClojureScript one at once, each with its own files behind the disk,
;; and what is stale in one says nothing about the other - so the question
;; carries the dialect of the buffer it was asked from, the way every other
;; question about a name does.  The buffer then REMEMBERS it: what `g'
;; asks again has to be the question this buffer answered, and the buffer is
;; not a Clojure buffer of either kind, so asking it afresh from here would
;; ask about whatever repl the commands are pointed at now.  `l' reloads
;; that same language for the same reason.
;;
;; Nothing waits for the answer.  The process reads the modification time
;; of every file it has compiled, which is a question about a disk rather
;; than about memory, so the buffer is written when the answer arrives.

;;; Code:

(require 'subr-x)
(require 'replique-eval)
(require 'replique-name)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-symbol)

(defvar-local replique-stale--found nil
  "What the process answered, as the buffer showing it was written.")

(defvar-local replique-stale--process nil
  "The process the buffer was written from, and is written again from.")

(defvar-local replique-stale--dialect-keys nil
  "The dialect keys the question carried, and is asked again with.

Nil for Clojure, which is what an absent `:dialect' means to the process
as well.  Kept because this buffer is not the buffer the question was
asked from: reading the dialect off it would read the dialect of whatever
repl the commands are pointed at now.")

;;; Rendering

(defun replique-stale--label (found directory)
  "Return what to call the file FOUND, for somebody working in DIRECTORY.

The path it has under the directory the process was started in, which is
how a developer names their own files - and the whole path where the file
is somewhere else, since a relative name climbing out of the project says
less than the path it is a longer way of writing.

A file inside an archive has no path: it is named as the entry it is,
beside the archive holding it, which is how this protocol writes one
everywhere."
  (let ((file (plist-get found :file))
        (entry (plist-get found :entry)))
    (cond
     (entry (format "%s in %s" entry (file-name-nondirectory file)))
     ((and directory (string-prefix-p (expand-file-name directory) file))
      (file-relative-name file directory))
     (t (abbreviate-file-name file)))))

(defun replique-stale--open (found)
  "Open the file FOUND names."
  (pop-to-buffer (or (replique-symbol-visit found)
                     (user-error "Replique: %s is not there to be opened"
                                 (plist-get found :file)))))

(defun replique-stale--insert (files directory)
  "Insert FILES, each as something that opens it, named from DIRECTORY."
  (dolist (found files)
    (insert "  "
            (propertize (buttonize (replique-stale--label found directory)
                                   #'replique-stale--open found)
                        'help-echo "RET or mouse-1: open this file")
            "\n")))

(defun replique-stale--render ()
  "Write what the process answered into the current buffer."
  (let* ((inhibit-read-only t)
         (process replique-stale--process)
         (directory (and process (replique-process--directory process)))
         (cljs (and replique-stale--dialect-keys t))
         (changed (plist-get replique-stale--found :changed))
         (stale (plist-get replique-stale--found :stale)))
    (erase-buffer)
    (if (and (null changed) (null stale))
        (insert (if cljs
                    "Nothing has changed since this process compiled it.\n"
                  "Nothing has changed since this process read it.\n"))
      ;; Named, and only for ClojureScript, for the reason an absent
      ;; `:dialect' means Clojure: a Clojure answer reads as it always did,
      ;; and the one that would otherwise be taken for it says which it is.
      (when cljs (insert "ClojureScript\n\n"))
      (insert (if cljs
                  "Changed since the process compiled them\n\n"
                "Changed since the process read them\n\n"))
      (replique-stale--insert changed directory)
      (when stale
        ;; "a file that has changed" rather than "a file above", which is
        ;; true of Clojure and not of ClojureScript: a .cljs file expands
        ;; macros written in .clj files, and those are not in the list above
        ;; - they are Clojure files, and this answer is about ClojureScript
        ;; ones.
        (insert "\nNot changed, and out of date all the same: these expand a macro\n"
                "of a file that has changed, and hold the expansion the old one made\n\n")
        (replique-stale--insert stale directory)))
    (goto-char (point-min))))

;;; The mode

(defvar replique-stale-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'replique-stale-refresh)
    (define-key map (kbd "l") #'replique-stale-reload)
    map)
  "Keymap of a staleness buffer.")

(define-derived-mode replique-stale-mode special-mode "Replique-Stale"
  "Major mode for looking at what has to be loaded again.

\\{replique-stale-mode-map}"
  (setq-local truncate-lines nil))

;;; Asking

(defun replique-stale--show (process dialect-keys found)
  "Show what PROCESS answered in FOUND, and return the buffer.

DIALECT-KEYS is the question it answered, kept for `replique-stale-refresh'
and `replique-stale-reload'."
  (let ((buffer (get-buffer-create "*replique-stale*")))
    (with-current-buffer buffer
      ;; Before anything buffer local is set: the mode kills them
      (replique-stale-mode)
      (setq replique-stale--process process
            replique-stale--dialect-keys dialect-keys
            replique-stale--found found)
      (replique-stale--render))
    (pop-to-buffer buffer)
    buffer))

(defun replique-stale--ask (process &optional dialect-keys)
  "Ask PROCESS what has to be loaded again, and show the answer.

DIALECT-KEYS says which language to ask about, nil being Clojure."
  (replique-process-request
   process (append (list :op :stale) dialect-keys)
   (lambda (frame)
     (if (equal "error" (plist-get frame :tag))
         (message "replique: %s" (plist-get frame :message))
       (replique-stale--show process dialect-keys frame)))))

(defun replique-stale-refresh ()
  "Ask again, of the process this buffer was written from.

That process rather than whichever one the commands act on now: a buffer
that answered about one process and then answered about another, under
the same heading, would be two answers nothing tells apart."
  (interactive)
  (replique-stale--ask (or replique-stale--process
                           (user-error "This buffer was written from no process"))
                       replique-stale--dialect-keys))

(defun replique-stale-reload ()
  "Load what this buffer is showing.

`replique-reload-all' told which language to reload rather than left to
work it out: this buffer is not a buffer of either language, so it would
otherwise reload whatever repl the commands are pointed at now - which
could be the one this answer is not about."
  (interactive)
  (replique-reload-all nil (if replique-stale--dialect-keys :cljs :clj)))

;;;###autoload
(defun replique-stale ()
  "Show what loading everything that changed would load.

The same question `replique-reload-all' answers by doing it, asked
without doing it: nothing is compiled, and nothing in the process
changes.

Two lists.  The files whose copy on the disk is newer than what the
process read - what was edited - and, apart from those, the files that
were not edited and are out of date all the same, because they expand a
macro of a file that was and hold the expansion the old one made.  The
second list is the one worth looking at: what needs compiling is not the
same as what was typed in, and nothing in a buffer says which files
those are.

About the language of this buffer, which a process holding a Clojure
application and a ClojureScript one at once has two answers for: a .cljs
buffer asks about the ClojureScript, a .clj buffer about the Clojure, and a
.cljc buffer about whichever repl the commands are pointed at - the three
cases every question about a name follows.

Each one opens.  \\<replique-stale-mode-map>\\[replique-stale-refresh] \
asks again, \\[replique-stale-reload] loads them.

Only a process whose compiler wrote down what it compiled can answer
this.  One that cannot says so, and says what to start it on instead."
  (interactive)
  (replique-stale--ask (or (replique-name-process)
                           (user-error "No replique process"))
                       (replique-dialect-keys)))

(provide 'replique-stale)

;;; replique-stale.el ends here
