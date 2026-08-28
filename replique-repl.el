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

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'comint)
(require 'replique-common)
(require 'replique-edn)
(require 'replique-conn)
(require 'replique-process)

(defcustom replique-prompt-read-only t
  "Whether the prompt of a repl buffer is read only."
  :type 'boolean
  :group 'replique)

(defcustom replique-echo-results t
  "Whether the result of a form evaluated from a buffer is shown in the echo area.

The result is in the repl buffer either way - this is about not having to
look at it."
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
has not come back yet.  QUEUED holds the code that was sent while the repl was
still busy with what came before it, waiting for a prompt to be written
after."
  process conn buffer ns params at-prompt to-echo queued last-exception)

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

(defun replique-repl--echo (repl string)
  "Show STRING in the echo area if a buffer is waiting for it in REPL."
  (when (> (or (replique-repl--to-echo repl) 0) 0)
    (setf (replique-repl--to-echo repl) (1- (replique-repl--to-echo repl)))
    (when replique-echo-results
      (message "%s" string))))

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
    ("out" (replique-repl--insert repl (plist-get frame :string)))
    ("err" (replique-repl--insert repl (plist-get frame :string) 'replique-stderr))
    ("ret"
     (let ((value (plist-get frame :value)))
       (replique-repl--insert repl (concat value "\n"))
       (replique-repl--echo repl value)))
    ("exception"
     (let* ((message (plist-get frame :message))
            (exception (plist-get frame :exception))
            (truncated (and exception (replique-repl--truncation exception))))
       (setf (replique-repl--last-exception repl) exception)
       (replique-repl--insert repl
                              (concat message (or truncated "") "\n")
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
     (replique-repl--insert repl
                            (format "%s: %s\n"
                                    (plist-get frame :error)
                                    (plist-get frame :message))
                            'replique-exception))
    (_ nil)))

;;; The mode

(defvar replique-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'replique-interrupt)
    (define-key map (kbd "C-c C-q") #'replique-quit-repl)
    (define-key map (kbd "C-c C-e") #'replique-show-last-exception)
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
    (setf (replique-repl--conn repl)
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
                         (setq replique-current-repl nil)))))
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
transcript showing it is a transcript of the wire.  When ECHO, the one
result the code is expected to produce is shown in the echo area too."
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
  "Show what the last exception of the current repl carried."
  (interactive)
  (let* ((repl (replique-repl-ensure))
         (exception (replique-repl--last-exception repl)))
    (unless exception (user-error "No exception yet"))
    (let ((buffer (get-buffer-create "*replique-exception*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (replique-repl--insert-exception exception 0))
        (goto-char (point-min))
        (special-mode))
      (pop-to-buffer buffer))))

(defun replique-repl--insert-exception (exception depth)
  "Write EXCEPTION at DEPTH into the current buffer."
  (let ((indent (make-string (* 2 depth) ?\s)))
    (insert indent (or (plist-get exception :class) "?") ": "
            (or (plist-get exception :message) "") "\n")
    (when-let* ((data (plist-get exception :data)))
      (insert indent "  data: " data "\n"))
    (dolist (frame (plist-get exception :trace))
      (insert indent "  at " frame "\n"))
    (when-let* ((dropped (plist-get exception :trace-dropped)))
      (insert indent (format "  ... %s more frames\n" dropped)))
    (when-let* ((cause (plist-get exception :cause)))
      (insert indent "caused by:\n")
      (replique-repl--insert-exception cause (1+ depth)))
    (when (plist-get exception :cause-dropped)
      (insert indent "caused by: ... the chain goes on, and its root is what"
              " the reported message names\n"))))

(defun replique-kill-process (process)
  "Close the connections to PROCESS and stop it if Emacs started it."
  (interactive (list (replique-process-ensure)))
  (dolist (repl (replique-process--repls process))
    (replique-conn-close (replique-repl--conn repl)))
  (replique-conn-close (replique-process--control process))
  (when (and (replique-process--proc process)
             (process-live-p (replique-process--proc process)))
    (delete-process (replique-process--proc process)))
  (replique-process--forget process))

(defun replique-switch-to-repl ()
  "Show the buffer of the current repl."
  (interactive)
  (pop-to-buffer (replique-repl--buffer (replique-repl-ensure))))

(provide 'replique-repl)

;;; replique-repl.el ends here
