;;; replique.el --- A development environment for Clojure  -*- lexical-binding: t; -*-

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

;; Author: Ewen Grosjean
;; URL: https://github.com/EwenG/replique.el
;; Version: 2.0.0-SNAPSHOT
;; Package-Requires: ((emacs "30"))

;;; Commentary:

;; The entry point: what to turn on in a Clojure buffer.
;;
;;   M-x replique-start      start a process in a directory and connect to it
;;   M-x replique-connect    connect to one that is already running
;;   M-x replique-repl       open a repl on it
;;
;; This is the editor client of the replique protocol, plus the mode it reads
;; Clojure with.  Completion is answered wherever a name is written, in the
;; dependency forms and in ordinary code - see `replique-completion'.  What
;; one name is - eldoc while a call is being written, and a definition to go
;; to when somebody asks where it came from - is `replique-symbol'.  Both ask
;; about what `replique-name' reads out of the buffer.
;;
;; `replique-clojure-mode' is what .clj, .cljs, .cljc and .edn open in, and
;; what the eval commands read a buffer with - see `replique-eval'.  Where a
;; form begins is a question sexp motion answers wrongly for #_ and for
;; metadata, and a wrong answer there evaluates what somebody commented out.
;; The mode answers it from `replique-parse', and asking the mode rather than
;; parsing again is what keeps the answer the editor indents by and the answer
;; the repl is sent the same one.
;;
;; That mode turns `replique-mode' on, so the commands below are bound in a
;; Clojure file without anything having to be turned on by hand.

;;; Code:

(require 'replique-common)
(require 'replique-clojure-mode)
(require 'replique-edn)
(require 'replique-conn)
(require 'replique-exception)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-eval)
(require 'replique-pprint)
(require 'replique-name)
(require 'replique-completion)
(require 'replique-symbol)

(defconst replique--version (package-get-version)
  "Which replique this is, read from the Version header of this file.

Read rather than written out again, so that there is one place to change
it and no way for two of them to disagree - the header being the one
package.el, and everything built on it, goes by.

Read once, here, because `package-get-version' answers about the file it
is being read in, and that is this one only while this file is loading.
Asked from inside a command it would answer about whatever buffer
happened to be current, which is a buffer of somebody else's.")

(defvar replique-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'replique-eval-last-sexp)
    (define-key map (kbd "C-M-x") #'replique-eval-defun)
    (define-key map (kbd "C-c C-r") #'replique-eval-region)
    (define-key map (kbd "C-c C-l") #'replique-load-file)
    (define-key map (kbd "C-c C-u") #'replique-remove-var)
    (define-key map (kbd "C-c C-c") #'replique-interrupt)
    (define-key map (kbd "C-c M-n") #'replique-in-ns)
    (define-key map (kbd "C-c C-z") #'replique-switch-to-repl)
    (define-key map (kbd "C-c C-o") #'replique-show-process-output)
    (define-key map (kbd "C-c C-e") #'replique-show-last-exception)
    (define-key map (kbd "C-c C-p") #'replique-pprint)
    map)
  "Keymap of `replique-mode'.")

;;;###autoload
(define-minor-mode replique-mode
  "Evaluate what is in this buffer in a replique repl.

Turned on by `replique-clojure-mode', so a Clojure file is a file these
commands work in.  Nothing here needs a repl to be running: a command
that needs one says so when it is used.

\\{replique-mode-map}"
  :lighter " replique"
  :keymap replique-mode-map
  ;; Added to the buffer rather than to the default value of the hook: what
  ;; answers here is a process, and a buffer that is not Clojure has none to
  ;; ask.  Turning the mode off takes it back out, which is what makes it
  ;; the way to stop replique answering completion in a buffer
  (if replique-mode
      (progn (replique-completion-install)
             (replique-symbol-install))
    (replique-completion-uninstall)
    (replique-symbol-uninstall)))

;; A repl reads Clojure at its prompt, requires included, and it is parsed
;; the way a Clojure buffer is - see `replique-repl--clojure'.  Added from
;; here rather than by the repl mode itself, which would make the file that
;; defines a repl depend on the one that completes in it
(add-hook 'replique-repl-mode-hook #'replique-completion-install)
(add-hook 'replique-repl-mode-hook #'replique-symbol-install)

;; From the autoloads rather than from this file: opening a Clojure file
;; loads the major mode and nothing else, and a keymap that arrived only
;; once something else had loaded replique would be a keymap that is there
;; the second time you look.  Remove it to keep the mode without the keys
;;;###autoload
(add-hook 'replique-clojure-mode-hook #'replique-mode)

;;;###autoload
(defun replique-version ()
  "Say which replique this is."
  (interactive)
  (message "replique %s, protocol version 1" (or replique--version "unknown")))

(provide 'replique)

;;; replique.el ends here
