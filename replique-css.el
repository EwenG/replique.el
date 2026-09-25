;;; replique-css.el --- Stylesheets, in the page  -*- lexical-binding: t; -*-

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

;; Seeing a stylesheet you have just edited, without reloading the page.
;;
;; WHAT IS HERE IS THE KEYSTROKE AND NOTHING ELSE.  The process holds the
;; connection to the pages, and the page itself decides which of its
;; stylesheets the file being edited is - see the `:reload-css' op in
;; doc/protocol.md.  That is where the work is, and it is there rather than
;; here for a reason worth writing down: an editor knows a path on this
;; machine, a page knows URLs, and NOTHING ON EITHER SIDE KNOWS BOTH.  Where
;; a project serves its assets from is the project's own arrangement.  So the
;; two are brought together by the longest path suffix they share, in the
;; page, which needs neither half to have been told about the other.
;;
;; WHAT REPLIQUE 1 ASKED AND THIS DOES NOT.  It listed the page's stylesheets
;; in one round trip, filtered them by basename here, and asked you which one
;; when more than one matched - then reloaded the one you chose in a second
;; round trip.  The list now comes back inside every answer, matched or not,
;; which is both the second round trip and the prompt gone: a reload that
;; found nothing says what the page HAS, where replique 1 said "Could not
;; find a css file to reload" and left you to guess.
;;
;; AND THE FILE YOU EDIT IS NOT ALWAYS THE FILE THE PAGE HOLDS.  A .scss is
;; built into a .css and it is the .css a page fetches, so the key builds
;; first and reloads what the build wrote - see `replique-css-outputs'.
;;
;; WHAT REPLIQUE 1 DID ABOUT THE BUILD, and what is kept.  Kept: the file it
;; compiles is not the file you are looking at.  Editing a partial has to
;; rebuild the ENTRY POINT that includes it, and a naive "compile this
;; buffer" gets that wrong on every file whose name starts with an
;; underscore.  Replique 1 remembered an entry point per output file, which
;; is the right shape.
;;
;; Not kept: WHERE it remembered them.  A `defvar' filled in by answering a
;; `completing-read' means the prompt comes back on every single reload -
;; even with one output remembered, you press return to it - and the whole
;; memory dies with the Emacs session.  The same two facts written in
;; `.dir-locals.el' are in the repository, are the same for everybody
;; working on it, and are never asked for again.
;;
;; TURN THE MODE ON WHERE YOU WANT THE KEY, which is what replique 1 wanted
;; too:
;;
;;   (add-hook 'css-mode-hook #'replique-css-mode)
;;
;; `scss-mode' and `less-css-mode' are derived from `css-mode', so that hook
;; is all of them.  Nothing is added to it from here: a .css file is a file
;; like any other and most of them are opened in projects that have never
;; heard of replique, where a mode that installed itself would be a keymap
;; nobody asked for.

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'seq)
(require 'subr-x)
(require 'replique-name)
(require 'replique-process)

;;; What this project builds, and how


;; NONE OF THE THREE IS MARKED SAFE, and that is the decision rather than an
;; omission.  What `replique-css-build-command' names is a program this runs,
;; and the other two are the paths a build reads and WRITES - so a repository
;; that set them could have a keystroke of yours overwrite a file of yours.
;; Emacs asks once per project, remembers the answer, and never asks again,
;; which is a different thing from replique 1's prompt on every reload.

(defcustom replique-css-entry nil
  "The stylesheet a build of this project reads, or nil.

A path, relative to the directory the process was started in.  This and
not the buffer: editing a partial rebuilds the entry point that includes
it, which is the one thing replique 1 got right here and the thing a
\='build the file I am looking at\=' gets wrong for every file whose name
begins with an underscore.

Usually set in `.dir-locals.el\=', where it is written once and is the
same for everybody working on the project:

  ((scss-mode
    . ((replique-css-entry . \"scss/main.scss\")
       (replique-css-outputs . (\"public/css/main.css\")))))

Not read at all where `replique-css-build-command\=' is set, since a
command of your own takes whatever arguments it takes."
  :type '(choice (const :tag "None" nil) string)
  :group 'replique)

(defcustom replique-css-outputs nil
  "The .css files a build of this project writes, as a list of paths.

Relative to the directory the process was started in.  These are what is
reloaded, and they are what a page fetches: the .scss you are editing is
not a file any browser has ever asked for.

Usually set in `.dir-locals.el\=' - see `replique-css-entry\='."
  :type '(repeat string)
  :group 'replique)

(defcustom replique-css-build-command nil
  "The command that builds this project\='s stylesheets, or nil for sass.

A list of strings, the program and its arguments, run in the directory
the process was started in.  Nil runs

  sass --embed-source-map ENTRY OUTPUT

once for each of `replique-css-outputs\=', which is what replique 1 ran and
is right for a project whose build IS sass.

SET IT AND NOTHING IS SUBSTITUTED: the command is run as written, once,
and `replique-css-outputs\=' is then only the list of what to reload.  That
is what a project with a real build wants - `(\"npx\" \"gulp\" \"devCss\")\='
runs the pipeline the project already has, autoprefixer and all, rather
than a second one replique invented that writes almost the same CSS."
  :type '(choice (const :tag "sass" nil) (repeat string))
  :group 'replique)

(defvar replique-css-mode-map
  (let ((map (make-sparse-keymap)))
    ;; The same key a Clojure buffer loads with, and the same sentence: put
    ;; what is in this buffer into the process.  `replique-load-file' is not
    ;; it - what that sends is a form for a repl to read, and a stylesheet is
    ;; not code this process runs
    (define-key map (kbd "C-c C-l") #'replique-reload-css)
    map)
  "Keymap of `replique-css-mode'.")

;;;###autoload
(define-minor-mode replique-css-mode
  "Reload this buffer\\='s stylesheet in the pages a replique process has.

Turned on where you want the key - see the commentary.  Nothing here
needs a process to be running: the command says so when it is used.

\\{replique-css-mode-map}"
  :lighter " replique-css"
  :keymap replique-css-mode-map)

(defun replique-css--report (files frames)
  "Say what the process answered about reloading FILES.

FRAMES are the replies, one for each.  Five answers and not two, because
\"nothing happened\" has four different reasons and they are not the same
thing to whoever pressed the key: the process could not be asked, the
page could not be asked, the page was asked and holds nothing like these
files, or the page holds no stylesheets at all.

ONE SENTENCE FOR THE WHOLE BUILD, and that is why this takes all of them
at once.  A build that writes main.css, trial.css and design-system.css
is three ops, and the page you have open includes ONE of the three - so
reported one at a time, two of them would say \"nothing on the page
matches\" and the last of those would be the sentence left on the screen.
The reload that worked would be the one you could not see.

A SENTENCE THE PROCESS WROTE IS SHOWN AS IT WAS WRITTEN.  Where there is
no page open, that sentence names the URL to open - which is the whole of
what somebody needs and is not something this could word better."
  (let ((failed (seq-find (lambda (f) (equal "error" (plist-get f :tag))) frames))
        (reloaded (seq-mapcat (lambda (f) (plist-get f :reloaded)) frames))
        (note (seq-some (lambda (f) (plist-get f :note)) frames))
        (sheets (delete-dups
                 (seq-mapcat (lambda (f) (plist-get f :stylesheets)) frames))))
    (cond
     (failed (message "replique: %s" (plist-get failed :message)))
     ;; With the note beside it where there is one: some of these may have
     ;; reloaded while others could not be asked, and saying only the half
     ;; that worked is how the half that did not goes unnoticed
     (reloaded (message "replique: reloaded %s%s"
                        (string-join reloaded ", ")
                        (if note (format " - %s" note) "")))
     (note (message "replique: %s" note))
     (sheets
      ;; What the page has, which is the answer to the question somebody is
      ;; about to ask.  Replique 1 had this list in its hand at this exact
      ;; moment and threw it away
      (message "replique: nothing on the page matches %s - it has %s"
               (string-join (mapcar #'file-name-nondirectory files) ", ")
               (string-join sheets ", ")))
     (t (message "replique: the page has no stylesheets")))))

(defun replique-css--reload (files process)
  "Ask PROCESS to reload FILES, and say what came of all of them.

One op each, because the op is about one file, and one sentence at the
end - see `replique-css--report\='."
  (let ((frames nil)
        (left (length files)))
    (dolist (file files)
      (replique-process-request
       process (list :op :reload-css :file file)
       (lambda (frame)
         (push frame frames)
         (setq left (1- left))
         (when (zerop left)
           (replique-css--report files (nreverse frames))))))))

(defun replique-css--commands (entry outputs)
  "The commands that build OUTPUTS from ENTRY, as a list of lists.

One command, run as written, where `replique-css-build-command\=' says so.
Otherwise sass, once per output, which is replique 1\='s command and is
right for a project whose build is sass and nothing else."
  (if replique-css-build-command
      (list replique-css-build-command)
    (mapcar (lambda (output)
              (list "sass" "--embed-source-map" entry output))
            outputs)))

(defun replique-css--build (commands root)
  "Run COMMANDS in ROOT, and return what the first failure printed.

Nil where they all succeeded, which is what says the outputs are worth
reloading.  Stopped at the first failure, because a build is steps and a
step after a failed one is a step run on what the failed one did not
write.

SYNCHRONOUS, and deliberately.  On a real project sass over two hundred
partials is a third of a second, the reload has to happen after it
anyway, a failure has to be read where the key was pressed, and two saves
in a row must not become two builds racing to write one file.  What waits
here is what `replique-conn-request-sync\=' already waits for elsewhere: a
keystroke, with \\[keyboard-quit] to abandon it."
  (let ((failure nil))
    (dolist (command commands)
      (unless failure
        (with-temp-buffer
          (let* ((default-directory (file-name-as-directory root))
                 (code (apply #'call-process (car command) nil t nil (cdr command))))
            (unless (equal 0 code)
              (ansi-color-apply-on-region (point-min) (point-max))
              (setq failure (string-trim (buffer-string))))))))
    failure))

;;;###autoload
(defun replique-reload-css (&optional file process)
  "Make the stylesheet FILE shows appear in every page connected to PROCESS.

FILE is this buffer\='s file when it is not given, and PROCESS is the one
the commands act on by default.

A .css IS WHAT A PAGE FETCHES, so one is reloaded as it stands.  ANYTHING
ELSE IS BUILT FIRST - a .scss is a file no browser has ever asked for -
and what is reloaded is then what the build wrote, which is
`replique-css-outputs\=' and not this buffer.  Master dispatched the same
key the same way, on the major mode; this asks the file, which is the
fact, and holds whichever mode you happen to read .scss in.

WHAT IS BUILT IS `replique-css-entry\=', NOT THIS BUFFER.  Editing a
partial has to rebuild the entry point that includes it, and building the
buffer would be wrong for every file whose name begins with an
underscore.  Those two, and `replique-css-build-command\=' where sass is
not your build, are named once in `.dir-locals.el\=' - see
`replique-css-entry\='.  Nothing is remembered anywhere else and nothing is
asked for: replique 1 asked which output to write on every single reload
and forgot the answer when Emacs stopped.

EVERY PAGE, and not the page: you have the application open and the tab
you were comparing it against, and a stylesheet that reloaded in one of
them is a stylesheet that did not reload.

WHAT IS RELOADED IS THE FILE ON THE DISK - the page fetches it from
whatever serves the application\='s assets - so a buffer with unsaved
changes is offered to be saved first, which is `replique-load-file\='s
answer to the same question and is asked in the same words.

The build waits and the reload does not.  A build is a third of a second
and the reload cannot start until it has finished; asking the process
starts its browser runtime where it is not up, which is seconds the first
time, and what happened is said when it is known.

The page is not reloaded and nothing in it is lost: a fresh <link> is put
in beside the old one and the old one goes when the new one has loaded.
A stylesheet that 404s or no longer parses therefore costs nothing - the
one that was working is still there - which is the case worth having,
because what you were doing when it happened was editing that file."
  (interactive
   (let ((file (or (buffer-file-name)
                   (user-error "This buffer holds no stylesheet to reload"))))
     ;; Before anything is built or sent, because saving is what puts the
     ;; change where the build - and the page - can read it
     (comint-check-source file)
     (list file)))
  (let* ((file (expand-file-name
                (or file (buffer-file-name)
                    (user-error "This buffer holds no stylesheet to reload"))))
         (process (or process (replique-name-process) (replique-process-ensure)))
         ;; Where the build runs and what its paths are relative to, which is
         ;; the same directory a relative :main-js file is relative to: a
         ;; client that named the process's directory once should not have to
         ;; know where the jvm was started
         (root (or (replique-process--directory process) default-directory)))
    (if (string-suffix-p ".css" file t)
        (replique-css--reload (list file) process)
      (let ((entry replique-css-entry)
            (outputs replique-css-outputs))
        (unless (and outputs (or entry replique-css-build-command))
          (user-error (concat "Nothing says how to build %s: set"
                              " `replique-css-entry' and `replique-css-outputs'"
                              " - or `replique-css-build-command' - in"
                              " .dir-locals.el")
                      (file-name-nondirectory file)))
        (let* ((entry (and entry (expand-file-name entry root)))
               (outputs (mapcar (lambda (o) (expand-file-name o root)) outputs))
               (failed (replique-css--build
                        (replique-css--commands entry outputs) root)))
          (if failed
              ;; As the build printed it.  What is wrong with a stylesheet is
              ;; something sass has already said better than this could, and
              ;; the line and column it names are in the file you are in
              (message "%s" failed)
            (replique-css--reload outputs process)))))))

(provide 'replique-css)

;;; replique-css.el ends here
