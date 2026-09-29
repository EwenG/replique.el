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
;; AND ASKED ABOUT EVERY LANGUAGE THE PROCESS HAS OPEN.  A process can hold a
;; Clojure application and a ClojureScript one at once, each with its own files
;; behind the disk, and what is stale in one says nothing about the other - so
;; the question is one per repl and the answer is a section each.  Which is the
;; question `replique-reload-app' is about to answer by doing it, and the
;; moment worth looking before leaping is exactly that one, after a branch has
;; been switched.
;;
;; THE BUFFER REMEMBERS WHICH PROCESS IT WAS: what `g' asks again has to be the
;; process this buffer answered about, and the buffer is not a Clojure buffer of
;; either kind, so asking it afresh from here would ask about whatever repl the
;; commands are pointed at now.  `l' reloads the same thing for the same
;; reason.
;;
;; AND A PROCESS THAT HAS READ NOTHING IS NOT A PROCESS WITH NOTHING TO LOAD.
;; A Clojure model holds what the compiler read under the sink, which is what a
;; load and a reload push - so an application that arrived by `require', from an
;; init script or at a prompt, is running and is in no model.  Two empty lists,
;; every time, whatever is edited.  The process says how many files it has read
;; so that the two can be told apart, because reporting the wrong one of them
;; is telling somebody their application is up to date on the strength of a
;; process that has never heard of it.
;;
;; AND THE STYLESHEETS, WHICH ARE A WEAKER FACT AND SAY SO.  The other two
;; lists are what the process compiled and when; this one is two modification
;; times compared in Emacs, because the process has never heard of a .scss and
;; what reads which partial is sass's to know.  It is still the question
;; somebody has - is the .css on disk behind the .scss - and it is the third of
;; `replique-reload-app' that nothing said anything about until now.
;;
;; Nothing waits for the answer.  The process reads the modification time
;; of every file it has compiled, which is a question about a disk rather
;; than about memory, so the buffer is written when the answer arrives - and
;; where there is a question per repl, when the last of them arrives.

;;; Code:

(require 'replique-css)
(require 'replique-process)
(require 'replique-reload)
(require 'replique-symbol)

(defvar-local replique-stale--sections nil
  "What the process answered, as the buffer showing it was written.

A list of plists, one per question asked, in the order they are reloaded
in:

  :label         what to call the language, `replique-reload--label\='
  :dialect-keys  the keys that question carried, nil for Clojure
  :found         the frame that came back

The stylesheets are not in here.  They are not something the process was
asked - see `replique-stale--stylesheets\=' - and putting them in a list
whose every other entry is an answer would be the one place in this
buffer where the shape lies about where a fact came from.")

(defvar-local replique-stale--process nil
  "The process the buffer was written from, and is written again from.")

(defvar-local replique-stale--stylesheets nil
  "The built stylesheets a source has got ahead of, or nil.

As `replique-css-stale-in\=' answers them.

Worked out here rather than asked, because the process has never heard of
a .scss.  Which is also why it is kept apart from the answers: see
`replique-stale--sections\='.")

;;; Rendering

(defun replique-stale--load-key ()
  "How to name the command that loads a file, as this buffer has to name it.

Through `replique-mode-map\=' because this buffer is not a buffer that map
is active in, and guarded because that map lives in the file that requires
this one: loading this file on its own would otherwise be asked for the
bindings of a keymap that is not there yet."
  (if (boundp 'replique-mode-map)
      (substitute-command-keys "\\<replique-mode-map>\\[replique-load-file]")
    "M-x replique-load-file"))

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

(defun replique-stale--open-path (path)
  "Open the file at PATH."
  (if (file-exists-p path)
      (pop-to-buffer (find-file-noselect path))
    (user-error "Replique: %s is not there to be opened" path)))

(defun replique-stale--path-label (path directory)
  "Return what to call PATH, for somebody working in DIRECTORY.

`replique-stale--label\=' for a file nobody was asked about: a stylesheet
is a path this worked out rather than a place the process answered with,
so there is no entry to name and no archive it could be inside."
  (if (and directory (string-prefix-p (expand-file-name directory) path))
      (file-relative-name path directory)
    (abbreviate-file-name path)))

(defun replique-stale--insert-paths (pairs directory)
  "Insert PAIRS, each (OUTPUT . SOURCE), named from DIRECTORY.

The OUTPUT is what opens, because the output is the file that is behind:
what a page fetches, and what a build would write over.  The source is
named beside it and does not open - it is there to say what the output is
behind, and it is one of many."
  (dolist (pair pairs)
    (insert "  "
            (propertize (buttonize (replique-stale--path-label (car pair) directory)
                                   #'replique-stale--open-path (car pair))
                        'help-echo "RET or mouse-1: open this file")
            (format "  (%s is newer)"
                    (replique-stale--path-label (cdr pair) directory))
            "\n")))

(defun replique-stale--insert-unread (unread)
  "Say that UNREAD of this project\='s files are running here and unread.

THE ANSWER ABOVE IS PARTIAL, AND NOTHING ELSE IN IT SAYS SO.  A model
holds what the compiler read while it was loading a file; a namespace
that arrived by `require\=' - at a prompt, from an init script, or pulled
in by the first one that was loaded - is loaded, is running, and is in no
model.  So the lists above are the whole truth about the files the
process has read, and say nothing whatever about the rest: read off one
of those, `nothing has changed\=' is an application reported as up to date
by a process that has never heard of most of it.

THE CASE WORTH THE SENTENCE IS NOT THE EMPTY MODEL, which is answered
above this and is rare.  It is the model with one or two files in it,
which is what a process whose application came up by `require\=' and was
then loaded from once actually holds - not zero, so it reads as a model,
and it answers nothing whatever is edited.

Written under the answer rather than instead of it, and under both of the
shapes the answer takes: a partial list of changed files is worth having
and is still partial."
  (insert "\n  " (number-to-string unread)
          (if (eql 1 unread)
              " more file of this project is running here and has\n  not been read"
            " more files of this project are running here and have\n  not been read")
          ", so nothing above speaks for "
          (if (eql 1 unread) "it" "them") ".\n"
          "\n  What this process holds is what its compiler read while it was\n"
          "  loading a file for you - "
          (replique-stale--load-key)
          " - and a namespace that arrived\n"
          "  by `require', at a prompt or from an init script, is loaded, is\n"
          "  running, and is not in it.\n"))

(defun replique-stale--render-section (section directory named)
  "Write SECTION into the current buffer, naming files from DIRECTORY.

NAMED says whether to write the language above it, which is asked rather
than worked out: one Clojure answer reads as it always did, and every
other arrangement - ClojureScript, or more than one of them - has to say
which is which."
  (let* ((found (plist-get section :found))
         (cljs (and (plist-get section :dialect-keys) t))
         (changed (plist-get found :changed))
         (stale (plist-get found :stale))
         (message (plist-get found :message)))
    (when named (insert (plist-get section :label) "\n\n"))
    (cond
     ;; A process that keeps no track of what it compiled refuses the
     ;; question, and the refusal is a sentence of its own saying what to
     ;; start the process on instead.  Shown as it was written: a heading with
     ;; two empty lists under it would report a process that cannot answer as
     ;; a process with nothing to load.
     ((equal "error" (plist-get found :tag))
      (insert "  " (or message "the process would not answer") "\n"))
     ;; Before the two empty lists, because it is the other way of answering
     ;; nothing and is a different fact.  A model with no files in it will
     ;; answer this whatever is edited, and "nothing has changed" read off one
     ;; of those is an application reported as up to date by a process that has
     ;; never heard of it.  An answer with no count in it at all is an older
     ;; process than this and is read the way it always was.
     ((eql 0 (plist-get found :analysed))
      (insert (if cljs
                  "  Nothing has been compiled by this process yet, so there is\n"
                "  Nothing has been loaded through this process yet, so there is\n")
              "  nothing it can say has changed.\n")
      (unless cljs
        (insert "\n  What it holds is what its compiler read while it was loading a\n"
                "  file for you - "
                (replique-stale--load-key)
                " - and a namespace that arrived by `require', at a\n"
                "  prompt or from an init script, is loaded, is running, and is not\n"
                "  in it.\n")))
     ((and (null changed) (null stale))
      (insert (if cljs
                  "  Nothing has changed since this process compiled it.\n"
                "  Nothing has changed since this process read it.\n")))
     (t
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
        (replique-stale--insert stale directory))))
    ;; Under the files rather than above them, and only where there are files:
    ;; it is the answer to "and then what", and a language with nothing to load
    ;; has no then.
    (when (and cljs (or changed stale)
               (not (plist-get found :connected)))
      (insert "\n  Nothing is connected to this runtime, so a reload would compile\n"
              "  all of it and land nowhere.  Open the application, or check that\n"
              "  its main module names this process - M-x replique-main-js.\n"))
    ;; And under everything, because it is true of everything above it: the
    ;; lists and the sentence that stands in for them are equally about the
    ;; files this process has read, and equally silent about the rest.  Not
    ;; where the process refused the question, which has no answer to be
    ;; partial, and not where it has read nothing at all, which is said in
    ;; full above and would otherwise be said twice.  An answer with no such
    ;; count in it is a process older than this one and is read as it always
    ;; was.
    (let ((unread (plist-get found :unread)))
      (when (and (not (equal "error" (plist-get found :tag)))
                 (not (eql 0 (plist-get found :analysed)))
                 (integerp unread) (> unread 0))
        (replique-stale--insert-unread unread)))))

(defun replique-stale--render ()
  "Write what the process answered into the current buffer."
  (let* ((inhibit-read-only t)
         (process replique-stale--process)
         (directory (and process (replique-process--directory process)))
         (sections replique-stale--sections)
         ;; Named where naming them tells them apart, which is every
         ;; arrangement except the one this buffer has always had
         (named (or (cdr sections)
                    (plist-get (car sections) :dialect-keys)
                    replique-stale--stylesheets)))
    (erase-buffer)
    (dolist (section sections)
      (unless (eq section (car sections)) (insert "\n"))
      (replique-stale--render-section section directory named))
    (when replique-stale--stylesheets
      (insert "\nStylesheets\n\n"
              "Built, and a file the build would read is newer.  Which is two\n"
              "modification times compared here rather than anything the process\n"
              "compiled: what reads which partial is sass's to know, and a build\n"
              "rebuilds the entry point whatever this says.\n\n")
      (replique-stale--insert-paths replique-stale--stylesheets directory))
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

(defun replique-stale--show (process sections stylesheets)
  "Show what PROCESS said in SECTIONS, with STYLESHEETS under them.

Returns the buffer.  PROCESS is kept for `replique-stale-refresh' and
`replique-stale-reload' - see `replique-stale--process'."
  (let ((buffer (get-buffer-create "*replique-stale*")))
    (with-current-buffer buffer
      ;; Before anything buffer local is set: the mode kills them
      (replique-stale-mode)
      (setq replique-stale--process process
            replique-stale--sections sections
            replique-stale--stylesheets stylesheets)
      (replique-stale--render))
    (pop-to-buffer buffer)
    buffer))

(defun replique-stale--ask-app (process)
  "Ask PROCESS what every repl it has open would load, and show the answers.

ONE QUESTION PER REPL, which is one per language and runtime rather than
one per repl - `replique-reload--repls' decides that, and the order it
gives is the order they are reloaded in.  They go out together and come
back in whatever order the process answers them, so the buffer is written
when the last one has arrived.

A REFUSAL IS SHOWN RATHER THAN THROWN AWAY.  Only a process whose
compiler wrote down what it compiled can answer this, and one section of
four saying so, beside three that answered, is a fact about this process
worth reading next to the rest.

And the stylesheets under them, which are worked out here and asked of
nobody - see `replique-css-stale-in'."
  (let ((repls (replique-reload--repls process)))
    (unless repls
      (user-error "This process has no repl open - M-x replique-repl"))
    (let ((answers (make-hash-table :test #'eq))
          (left (length repls))
          (root (or (replique-process--directory process) default-directory)))
      (dolist (repl repls)
        (replique-process-request
         process (append (list :op :stale) (replique-reload--dialect-keys repl))
         (lambda (frame)
           (puthash repl frame answers)
           (setq left (1- left))
           (when (zerop left)
             (replique-stale--show
              process
              (mapcar (lambda (repl)
                        (list :label (replique-reload--label repl)
                              :dialect-keys (replique-reload--dialect-keys repl)
                              :found (gethash repl answers)))
                      repls)
              (replique-css-stale-in root)))))))))

(defun replique-stale-refresh ()
  "Ask again, of the process this buffer was written from.

That process rather than whichever one the commands act on now: a buffer
that answered about one process and then answered about another, under
the same heading, would be two answers nothing tells apart.

And of the repls it has NOW, which is what the question is about: a repl
opened since this buffer was written is a language whose staleness is as
much a part of the answer as the rest."
  (interactive)
  (replique-stale--ask-app
   (or replique-stale--process
       (user-error "This buffer was written from no process"))))

(defun replique-stale-reload ()
  "Load what this buffer is showing.

The whole process, stylesheets and all, which is what this buffer is
showing.  Told what to reload rather than left to work it out: this
buffer is not a buffer of either language, so a reload that asked it
would be asking the wrong thing."
  (interactive)
  (replique-reload-app))

;;;###autoload
(defun replique-stale-app ()
  "Show what bringing the whole application up to date would load.

What \\[replique-reload-app] is about to do, asked without doing it:
every language the process has a repl open on, and this project's
stylesheets, with nothing compiled and nothing in the process changed.

FOR THE MOMENT BEFORE A BRANCH THAT HAS JUST BEEN SWITCHED IS RELOADED,
which is the moment worth looking before leaping: what changed is not
what you were editing, no buffer on the screen says which language it was
in, and the list of files nobody can predict is exactly what this is.

A SECTION PER LANGUAGE AND RUNTIME, in the order they would be reloaded -
the Clojure, then each ClojureScript runtime - each with two lists.  The
files whose copy on the disk is newer than what the process read, which is
what was edited; and, apart from those, the files that were not edited and
are out of date all the same, because they expand a macro of a file that
was and hold the expansion the old one made.  The second list is the one
worth looking at: what needs compiling is not what was typed in, and
nothing in a buffer says which files those are.

A ClojureScript runtime with something to load and nothing connected to it
says so under its files: that reload would compile the whole program
correctly and land nowhere.

AND A LANGUAGE THIS PROCESS HAS READ NO FILES OF SAYS THAT, rather than
saying nothing has changed.  A Clojure model holds what the compiler read
while it was loading a file - \\[replique-load-file], and a reload - so an
application that arrived by `require\\=', from an init script or at a prompt,
is running and is in no model.  It would answer two empty lists whatever
was edited, and reading that as an application up to date is reading it
off a process that has never heard of it.

AND SO DOES ONE THAT HAS READ ALMOST NONE OF THEM, which is the same
state and the one a process is actually in: an application required in and
then loaded from once has read a file, so it is not nothing, and its lists
are the whole truth about that one file and silent about the other three
hundred.  Where there are files running here that no model holds, every
section of Clojure says how many under whatever it answered - the lists as
much as the sentence that stands in for them, since a partial list is
worth having and is still partial.

AND THE STYLESHEETS, WHICH ARE A WEAKER FACT AND SAY SO.  The other
sections are what the process compiled and when.  This one is the built
.css compared against the files under `replique-css-entry', in Emacs,
because the process has never heard of a .scss - so it does not know
which partial the entry point reads, and a build rebuilds the entry
whatever it says.  It still answers the question somebody has: is the
.css a page fetches behind the .scss.  Absent where this project says
nothing about stylesheets, and absent where it says only how to run its
build - a command is not a list of files.

Each one opens.  \\<replique-stale-mode-map>\\[replique-stale-refresh] \
asks again, \\[replique-stale-reload] reloads the application.

A LANGUAGE WHOSE QUESTION IS REFUSED IS SHOWN AS REFUSED rather than left
out: only a process whose compiler wrote down what it compiled can answer
this, and one of four sections saying so beside three that answered is
worth reading."
  (interactive)
  (replique-stale--ask-app (replique-process-ensure)))

(provide 'replique-stale)

;;; replique-stale.el ends here
