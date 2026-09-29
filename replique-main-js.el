;;; replique-main-js.el --- The module a page loads  -*- lexical-binding: t; -*-

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

;; The file an application's own page includes in order to reach a process's
;; ClojureScript.
;;
;; A ClojureScript repl on the browser compiles a program into a directory
;; and stops there, because the runtime is a page somebody opens and THE
;; PAGE IS WHAT LOADS THE PROGRAM.  This is the half that lives in the
;; project rather than in a temporary directory: one ES module, included
;; from the application's own <script type="module">, that connects the page
;; to the process and imports what the process compiled.
;;
;; The process writes it and this says where, because neither of them knows
;; both things.  Where it goes is a fact about the application's assets,
;; which replique has never seen; what goes in it is the port two servers
;; are listening on this minute, which an editor cannot know.
;;
;; ASKED FOR ONCE, and not once a session.  The port in the file is gone the
;; moment the process stops, and the next process moves every main module
;; under the directory it was started in to its own port when its browser
;; runtime starts.  Replique 1 did that walk from the editor, which is why
;; it had a command for it and a `.repliqueignore' to keep it out of build
;; directories; here it is the process's, and what is left for an editor is
;; the first write and any later move - see `replique-main-js'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'replique-eval)
(require 'replique-name)
(require 'replique-process)

(defconst replique-main-js--marker "//replique-2 main module"
  "The first line of a main module, and the whole of how one is recognised.

Deliberately not a prefix of replique 1\\='s marker, so that the two do not
rewrite each other\\='s files in a project where both are used - which is
the process\\='s reason for choosing it and this one\\='s for matching it.")

(defconst replique-main-js--main-ns-regexp "^const mainNs = \"\\([^\"]*\\)\";$"
  "How a main module names the program its page loads.

Each of the four constants is on a line of its own so that one can be
read - and rewritten - without reading JavaScript.  A module that names
no program writes null there and does not match, which is the answer
wanted: nil.")

(defconst replique-main-js--head 2048
  "How much of a file to read before deciding what it is, in characters.

The marker is the first line and the constants are in the few hundred
characters after it, so this is the whole of what is being looked for
with room to spare.  Bounded because a project is full of .js files that
are megabytes of somebody else\\='s bundle, and one of them is as likely to
be chosen by mistake as any other file is.")

(defun replique-main-js--names (file)
  "Return the namespace the main module FILE names, or nil.

WHAT THE FILE ALREADY SAYS, which is what it goes on saying unless
somebody says otherwise: a module written again is usually a module being
moved rather than one whose program has changed.  `mainNs\\=' is in there
for a reader rather than for the browser - the protocol says as much -
and this is the reader it is in there for.

Nil for a file that is not one of ours, which is every other .js in a
project, and nil for one of ours whose page loads nothing."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 replique-main-js--head)
      (goto-char (point-min))
      (when (and (looking-at-p (regexp-quote replique-main-js--marker))
                 (re-search-forward replique-main-js--main-ns-regexp nil t))
        (match-string 1)))))

(defun replique-main-js--label (file directory)
  "Return what to call FILE, for somebody working in DIRECTORY.

The path it has under the directory the process was started in, which is
how a developer names their own files, and the whole path where the file
is somewhere else - the same answer `replique-stale--label\\=' gives, for
the reason written there."
  (let ((directory (and directory (file-name-as-directory
                                   (expand-file-name directory)))))
    (if (and directory (string-prefix-p directory file))
        (file-relative-name file directory)
      (abbreviate-file-name file))))

(defun replique-main-js--read ()
  "Read the arguments of a main module about to be written.

BOTH ARE ASKED FOR, because neither has a default worth pressing return
on: where the file goes is a fact about assets this has never seen, and
which program the page runs is a fact about the application rather than
about the buffer somebody happened to be in when they asked.

What the file already names is offered where there is a file to name it -
see `replique-main-js--names\\=' - and is offered as text rather than as a
default so that it can be cleared: a page that loads nothing is a repl in
a page of yours with no program in it, and is a thing to want.

The namespaces the browser side of the process has are what can be
chosen, and what is typed is what is sent: a module may name a program
that has not been compiled yet, and the process is free to have no
ClojureScript at all and to say so when it is asked to write the file."
  (let* ((process (or (replique-name-process) (replique-process-ensure)))
         (file (read-file-name
                "Main module: "
                (or (replique-process--directory process) default-directory)))
         (main (completing-read
                "Main namespace (empty for none): "
                (replique-namespaces process '(:dialect :cljs :target :browser))
                nil nil (replique-main-js--names file))))
    (list file
          (let ((main (string-trim main)))
            (unless (string-empty-p main) main))
          process)))

;;;###autoload
(defun replique-main-js (file &optional main process)
  "Write FILE as the module an application\\='s own page includes.

MAIN is the namespace that page loads, or nil for a page that connects
and loads nothing.  PROCESS is the one to ask, the one the commands act
on by default.  A relative FILE is relative to the directory the process
was started in, for the reason its port file is.

THE BROWSER AND ONLY THE BROWSER.  There is no page on node and nothing
for one to include; a node repl needs none of this, and the file this
writes names the port of the process\\='s browser runtime whichever repl is
open at the time.

ASKING STARTS THAT RUNTIME if it is not up, which is seconds the first
time: the port is the whole point of the file, and there is no port until
the two servers are listening.  So nothing waits for the answer - what
was written is said when it arrives.

ASKING ONCE IS ENOUGH, and this is not a command to put in a hook.  The
port in the file is gone the moment the process stops, and the next
process refreshes it where it lies - every main module under the
directory it was started in is moved to its own port when its browser
runtime starts.  Ask for this when the application first needs the file,
when it is to move, and when it is to load a different program.  Replique
1 refreshed them from the editor and had a command for that too; here
that half is the process\\='s.

Include what it writes with a <script type=\"module\">.  A 404 from the
process afterwards means a namespace nothing has compiled, which is the
answer to give rather than compiling it behind the page\\='s back: require
it at a ClojureScript repl and the page will find it the next time it
asks."
  (interactive (replique-main-js--read))
  (let ((process (or process (replique-name-process) (replique-process-ensure))))
    (replique-process-request
     process (append (list :op :main-js :file file)
                     ;; Left out rather than sent as nil: absent is how the
                     ;; protocol writes a module that names no program, and
                     ;; a key whose value is null is a client saying the
                     ;; same thing in a second way
                     (when main (list :main main)))
     ;; An error frame carries no file, so what to call one is asked for
     ;; only where there is one - the refusal is a sentence of the
     ;; process's and is shown as it was written
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (message "replique: %s" (plist-get frame :message))
         (let ((written (replique-main-js--label
                         (plist-get frame :file)
                         (replique-process--directory process))))
           (if (plist-get frame :main)
               (message "replique: %s loads %s from %s"
                        written (plist-get frame :main) (plist-get frame :url))
             (message "replique: %s connects to %s and loads nothing"
                      written (plist-get frame :url)))))))))


;;;###autoload
(defun replique-refresh-main-js (&optional process)
  "Move every main module under PROCESS\='s directory to its port.

PROCESS is the one to ask, the one the commands act on by default.

THIS IS THE ONE FOR A HOOK, and `replique-main-js\=' is the one to keep out
of one - the difference is what each does when no browser runtime is up.
Writing a module is writing a port, so asking for it starts the two
servers; moving one to a port that does not exist yet is nothing anybody
wants, so this leaves them alone and says nothing moved.

WHAT IT IS FOR IS THE FILES MOVING RATHER THAN THE PORT.  A process
refreshes every main module under its directory when its browser runtime
starts, which is the moment the port changes and the only such moment the
process can find by itself.  It is not the only moment the answer
changes: a project directory that is a tree of links into a checkout
elsewhere - one directory pointed at whichever worktree is being worked
on - has another checkout\='s modules under it the moment those links move,
naming whichever port was current the day they were last written.  The
process cannot see that happen.  Whoever moved the links can, and this is
what they say it with.

Nothing waits for the answer, and what moved is said when it arrives.
Silent where nothing did, which is most of the time and every project
with no main module in it."
  (interactive)
  (let ((process (or process (replique-name-process) (replique-process-ensure))))
    (replique-process-request
     process (list :op :refresh-main-js)
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (message "replique: %s" (plist-get frame :message))
         (let* ((modules (plist-get frame :modules))
                (failed (seq-filter (lambda (m) (plist-get m :error)) modules))
                (moved (seq-count (lambda (m) (plist-get m :refreshed)) modules)))
           (cond
            ;; A module the process could not read or write is the one
            ;; thing here worth a sentence of its own: the file is still
            ;; naming a port nobody serves, and the page that includes it
            ;; will reach nothing at all.
            (failed
             (message "replique: %s"
                      (mapconcat
                       (lambda (m)
                         (format "%s: %s"
                                 (replique-main-js--label
                                  (plist-get m :file)
                                  (replique-process--directory process))
                                 (plist-get m :error)))
                       failed "; ")))
            ((> moved 0)
             (message "replique: %d main module%s refreshed to %s"
                      moved (if (> moved 1) "s" "")
                      (plist-get frame :url))))))))))

(provide 'replique-main-js)

;;; replique-main-js.el ends here
