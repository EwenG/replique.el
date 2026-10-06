;;; replique-debug.el --- A thread of the process, stopped where the code says  -*- lexical-binding: t; -*-

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

;; A thread that reaches (replique.debug/break!) stops there, and this is
;; what is shown of it: where it stopped, with an arrow in the fringe of the
;; file; its frames, innermost first, in a buffer of the thread's own; and
;; the locals of a frame, as a value to browse - see `replique-inspect'.
;;
;;   c   continue                  RET  the frame at point: its source and
;;   a   abort - throw from break!      its locals
;;   r   start the call of the frame at point over, running the code as
;;       it is now: redefine, then restart
;;   e   evaluate in the frame at point, with its locals bound, on the
;;       thread that stopped
;;
;; What stopped is the thread and nothing else: the process goes on
;; answering, and the other repls go on evaluating.  A repl whose form
;; stopped waits for it, the way it waits for anything slow.
;;
;; A thread has two buffers: its frames, and the locals of the frame looked
;; at - one view, which follows the frame visited.  Once the thread goes on
;; they go, with the files opened only to show where it was, unless it stops
;; again within `replique-debug-linger' seconds - as a loop does, or a call
;; started over: then they stay, and what changed in the locals since the
;; last stop is highlighted.
;;
;; The process has to be started for it: `replique-connect' offers a start
;; with the debugger, and `replique-debugger' starts every process with it.
;; Starting a call over needs its arguments, which Clojure clears after their
;; last use unless asked not to - see `replique-debug-keep-locals'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'replique-common)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-exception)
(require 'replique-pprint)
(require 'replique-symbol)
(require 'replique-inspect)

(defcustom replique-debug-linger 1.5
  "How long what is shown of a thread stays once it goes on, in seconds.

A thread that stops again by then is shown in the same buffers, the locals
highlighted where they changed; otherwise the buffers go."
  :type 'number
  :group 'replique)

(defface replique-debug-stopped-line
  '((t :inherit secondary-selection :extend t))
  "Face for the line a thread stopped on."
  :group 'replique)

(defface replique-debug-host-frame
  '((t :inherit shadow))
  "Face for a frame that is not a Clojure function."
  :group 'replique)

;;; Where a thread stopped, in the file

(defvar replique-debug--arrow nil
  "Where the last thread that stopped stopped, as a marker.")

(add-to-list 'overlay-arrow-variable-list 'replique-debug--arrow)

(defvar replique-debug--line nil
  "The overlay on the line the last thread that stopped stopped on.")

(defvar replique-debug--arrow-thread nil
  "Which (PROCESS . THREAD) the arrow is for.")

(defun replique-debug--forget-arrow ()
  "Take the arrow and the highlight away."
  (setq replique-debug--arrow nil
        replique-debug--arrow-thread nil)
  (when (overlayp replique-debug--line)
    (delete-overlay replique-debug--line))
  (setq replique-debug--line nil))

(defvar replique-debug--opened (make-hash-table :test #'equal)
  "The buffers opened to show where a thread is, by (PROCESS . THREAD).

Each as (BUFFER . TICK), TICK its `buffer-chars-modified-tick' once opened:
one written in since is not the debugger's any more.")

(defun replique-debug--locate (found &optional for)
  "Return a marker at where FOUND says, or nil where its file cannot be had.

FOUND carries `:file', and `:entry' for a file inside a jar, `:line' and
`:column' - what the process answers a place in a source with.  A buffer
opened for it is one of the thread FOR, (PROCESS . THREAD), and goes with
what is shown of the thread."
  (when-let* ((before (buffer-list))
              (buffer (replique-symbol-visit found)))
    (when (and for (not (memq buffer before)))
      (push (cons buffer (buffer-chars-modified-tick buffer))
            (gethash for replique-debug--opened)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (when-let* ((line (plist-get found :line)))
            (forward-line (1- line))
            (when-let* ((column (plist-get found :column)))
              (forward-char (min (1- column) (- (line-end-position) (point))))))
          (point-marker))))))

(defun replique-debug--show-place (found for &optional arrow)
  "Show where FOUND says in a window, and return the marker there.

FOR is the thread it is shown for, (PROCESS . THREAD).  With ARROW, the
fringe arrow and the highlight are put there, as where it stopped."
  (when-let* ((marker (replique-debug--locate found for)))
    (when arrow
      (replique-debug--forget-arrow)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (setq replique-debug--arrow (copy-marker (line-beginning-position)))
          (setq replique-debug--line
                (make-overlay (line-beginning-position) (1+ (line-end-position))))
          (overlay-put replique-debug--line 'face 'replique-debug-stopped-line)
          (overlay-put replique-debug--line 'priority 10))))
    (let ((window (display-buffer (marker-buffer marker)
                                  '((display-buffer-reuse-window
                                     display-buffer-use-some-window)
                                    (inhibit-same-window . t)))))
      (when window
        (set-window-point window marker)))
    marker))

;;; A buffer per stopped thread

(defvar-local replique-debug--process nil
  "The process the thread of the buffer is a thread of.")

(defvar-local replique-debug--thread nil
  "The id of the thread the buffer is about.")

(defvar-local replique-debug--pause nil
  "What the process said when the thread stopped, nil while it runs.")

(defvar-local replique-debug--frames nil
  "The frames of the stopped thread, innermost first, as the process said.")

(defvar-local replique-debug--host-frames nil
  "Whether the frames that are not Clojure functions are shown.")

(defun replique-debug--buffer-of (process thread)
  "Return the buffer of the thread THREAD of PROCESS, or nil."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'replique-debug-mode)
                     (eq process replique-debug--process)
                     (equal thread replique-debug--thread))))
            (buffer-list)))

(defun replique-debug--repl-of (process connection)
  "Return the repl of PROCESS whose connection is CONNECTION, or nil."
  (when connection
    (seq-find (lambda (repl)
                (equal connection (replique-conn--id (replique-repl--conn repl))))
              (replique-process--repls process))))

(defun replique-debug--place (found)
  "Return where FOUND is, as a short text."
  (let ((file (or (plist-get found :entry) (plist-get found :file)
                  (plist-get found :source))))
    (concat (if file (file-name-nondirectory file) "?")
            (when-let* ((line (plist-get found :line))) (format ":%s" line)))))

(defun replique-debug--frame-line (frame)
  "Insert the line of FRAME."
  (let ((start (point))
        (fn (plist-get frame :fn)))
    (insert (format "%3d  " (plist-get frame :index)))
    (insert (if fn
                (propertize fn 'face 'font-lock-function-name-face)
              (propertize (format "%s.%s" (plist-get frame :class) (plist-get frame :method))
                          'face 'replique-debug-host-frame)))
    (insert "  " (propertize (replique-debug--place frame) 'face 'replique-note) "\n")
    (put-text-property start (point) 'replique-debug-frame frame)))

(defun replique-debug--render ()
  "Write what the buffer shows, leaving point on the frame it was on."
  (let ((inhibit-read-only t)
        (at (get-text-property (point) 'replique-debug-frame))
        (hidden 0))
    (erase-buffer)
    (cond
     ((null replique-debug--pause)
      (insert (propertize "Running.\n" 'face 'replique-note)))
     ((null replique-debug--frames)
      (insert (propertize "…\n" 'face 'replique-note)))
     (t
      (dolist (frame replique-debug--frames)
        (if (or replique-debug--host-frames (plist-get frame :fn)
                ;; the frame that stopped is shown whatever it is
                (eql 0 (plist-get frame :index)))
            (replique-debug--frame-line frame)
          (cl-incf hidden)))
      (when (> hidden 0)
        (insert (propertize (format "     %s frames of the host - j shows them\n" hidden)
                            'face 'replique-note)))))
    (goto-char (point-min))
    (when at
      (when-let* ((found (text-property-search-forward
                          'replique-debug-frame (plist-get at :index)
                          (lambda (index frame) (eql index (plist-get frame :index))))))
        (goto-char (prop-match-beginning found))))
    (force-mode-line-update)))

(defun replique-debug--header ()
  "Return the header line of the buffer."
  (let* ((pause replique-debug--pause)
         (repl (and pause (replique-debug--repl-of replique-debug--process
                                                   (plist-get pause :connection)))))
    (string-join
     (delq nil
           (list (propertize (format "thread %s"
                                     (or (plist-get pause :name) replique-debug--thread))
                             'face 'bold)
                 (if pause
                     (propertize (format "stopped at %s" (replique-debug--place pause))
                                 'face 'replique-exception)
                   "running")
                 (when repl
                   (format "evaluating for %s" (buffer-name (replique-repl--buffer repl))))))
     "  ·  ")))

(defun replique-debug--ask-frames ()
  "Ask the process for the frames of the thread of the buffer."
  (let ((buffer (current-buffer))
        (pause replique-debug--pause))
    (when pause
      (replique-process-request
       replique-debug--process
       (list :op :debug-frames :thread replique-debug--thread)
       (lambda (frame)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             ;; an answer about a stop that is over is not about this one
             (when (eq pause replique-debug--pause)
               (if (equal "error" (plist-get frame :tag))
                   (replique-debug--answered-error frame)
                 (setq replique-debug--frames (plist-get frame :frames))
                 (replique-debug--render))))))))))

(defun replique-debug--answered-error (frame)
  "Say what FRAME, an error, says - and that the thread runs, where it does."
  (when (equal "not-paused" (plist-get frame :error))
    (setq replique-debug--pause nil
          replique-debug--frames nil)
    (replique-debug--render)
    (replique-debug--leave replique-debug--process replique-debug--thread))
  (message "replique: %s" (plist-get frame :message)))

;;; Being told

(defun replique-debug--stopped (process pause)
  "Show that a thread of PROCESS stopped, as PAUSE says."
  (let* ((thread (plist-get pause :thread))
         (for (cons process thread))
         (buffer (or (progn (replique-debug--stay process thread)
                            (replique-debug--buffer-of process thread))
                     (generate-new-buffer (format "*replique-debug %s*"
                                                  (plist-get pause :name))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'replique-debug-mode)
        (replique-debug-mode)
        (setq replique-debug--process process
              replique-debug--thread thread))
      (setq replique-debug--pause pause
            replique-debug--frames nil)
      (replique-debug--render)
      (replique-debug--ask-frames))
    (replique-debug--locals process thread 0 "stopped")
    (replique-debug--show-place pause for t)
    (setq replique-debug--arrow-thread for)
    (pop-to-buffer buffer)
    (message "replique: thread %s stopped at %s - c continues"
             (plist-get pause :name) (replique-debug--place pause))))

(defun replique-debug--resumed (process thread)
  "Show that the thread THREAD of PROCESS runs again."
  (when (equal replique-debug--arrow-thread (cons process thread))
    (replique-debug--forget-arrow))
  (when-let* ((buffer (replique-debug--buffer-of process thread)))
    (with-current-buffer buffer
      (setq replique-debug--pause nil
            replique-debug--frames nil)
      (replique-debug--render)))
  (replique-debug--leave process thread))

;;; What goes once a thread goes on

(defvar replique-debug--leaving (make-hash-table :test #'equal)
  "The timers taking away what is shown of a thread, by (PROCESS . THREAD).")

(defun replique-debug--stay (process thread)
  "Keep what is shown of the thread THREAD of PROCESS, which stopped again."
  (when-let* ((timer (gethash (cons process thread) replique-debug--leaving)))
    (cancel-timer timer)
    (remhash (cons process thread) replique-debug--leaving)))

(defun replique-debug--leave (process thread)
  "Take away what is shown of the thread THREAD of PROCESS, which went on.

In `replique-debug-linger' seconds, unless it stops again by then."
  (replique-debug--stay process thread)
  (puthash (cons process thread)
           (run-at-time replique-debug-linger nil #'replique-debug--clear process thread)
           replique-debug--leaving))

(defun replique-debug--kill (buffer)
  "Kill BUFFER, giving the windows it was in back to what they showed."
  (when (buffer-live-p buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (when (and (window-live-p window) (eq buffer (window-buffer window)))
        (quit-restore-window window 'bury)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun replique-debug--stopped-buffers ()
  "Return the buffers of the threads that are stopped."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'replique-debug-mode) replique-debug--pause)))
              (buffer-list)))

(defun replique-debug--clear (process thread)
  "Take away what is shown of the thread THREAD of PROCESS, now.

Its buffer, the view of its locals, and the buffers opened only to show
where it was - where they were not written in since."
  (let ((for (cons process thread)))
    (replique-debug--stay process thread)
    (when (equal replique-debug--arrow-thread for)
      (replique-debug--forget-arrow))
    (dolist (opened (gethash for replique-debug--opened))
      (let ((buffer (car opened)))
        (when (and (buffer-live-p buffer)
                   (not (buffer-modified-p buffer))
                   (= (cdr opened) (buffer-chars-modified-tick buffer)))
          (replique-debug--kill buffer))))
    (remhash for replique-debug--opened)
    (mapc #'replique-debug--kill (replique-debug--views-of process thread))
    (replique-debug--kill (replique-debug--buffer-of process thread))
    (unless (replique-debug--stopped-buffers)
      (replique-debug--kill (get-buffer "*replique-value*")))))

(defun replique-debug--forgotten (process)
  "Take away what is shown of the threads of PROCESS, which is gone."
  (let ((threads nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'replique-debug-mode) (eq process replique-debug--process))
          (push replique-debug--thread threads))))
    (maphash (lambda (for _) (when (eq process (car for)) (push (cdr for) threads)))
             replique-debug--opened)
    (dolist (thread (delete-dups threads))
      (replique-debug--clear process thread))))

(add-hook 'replique-process-forgotten-functions #'replique-debug--forgotten)

(defvar replique-debug--evaluations (make-hash-table :test #'equal)
  "What to do with what comes of code run in a frame, by (PROCESS . NUMBER).

The number is the one the reply to `:debug-eval' gave the evaluation, and
the event saying what came of it carries.")

(defun replique-debug--event (process frame)
  "Handle FRAME, an event of PROCESS, where it is about a thread stopping."
  (pcase (plist-get frame :event)
    ("debug-paused"
     (replique-debug--stopped process (replique-process--renamed-frame process frame)))
    ("debug-resumed"
     (replique-debug--resumed process (plist-get frame :thread)))
    ("debug-evaluated"
     (let ((key (cons process (plist-get frame :evaluation))))
       (when-let* ((then (gethash key replique-debug--evaluations)))
         (remhash key replique-debug--evaluations)
         (funcall then frame))))))

(add-hook 'replique-process-event-functions #'replique-debug--event)

;;; The locals of a frame

(defun replique-debug--views-of (process thread)
  "Return the buffers showing the locals of a frame of THREAD of PROCESS."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'replique-inspect-mode)
                       (eq process replique-inspect--process)
                       (equal thread (plist-get (plist-get replique-inspect--source :debug)
                                                :thread)))))
              (buffer-list)))

(defun replique-debug--locals (process thread index what)
  "Show the locals of the frame INDEX of THREAD of PROCESS, called WHAT.

In the one view of the locals the thread has: a view of another frame is
made a view of this one."
  (let ((view (or (car (replique-debug--views-of process thread))
                  (generate-new-buffer
                   (if-let* ((buffer (replique-debug--buffer-of process thread)))
                       (format "%s locals*" (string-remove-suffix "*" (buffer-name buffer)))
                     "*replique-debug locals*")))))
    (save-selected-window
      (replique-inspect-show process nil
                             (list :debug (list :thread thread :frame index))
                             (format "locals of %s" what)
                             view))))

;;; Commands

(defun replique-debug--this ()
  "Return the buffer of the stopped thread the commands act on.

The one this is, in a buffer of a thread; otherwise the only thread that
is stopped, or the one chosen."
  (if (derived-mode-p 'replique-debug-mode)
      (current-buffer)
    (let ((stopped (seq-filter (lambda (buffer)
                                 (with-current-buffer buffer
                                   (and (derived-mode-p 'replique-debug-mode)
                                        replique-debug--pause)))
                               (buffer-list))))
      (cond
       ((null stopped) (user-error "No thread is stopped"))
       ((null (cdr stopped)) (car stopped))
       (t (get-buffer (completing-read "Thread: " (mapcar #'buffer-name stopped) nil t)))))))

(defmacro replique-debug--in-this (&rest body)
  "Run BODY in the buffer of the stopped thread the commands act on."
  (declare (indent 0))
  `(with-current-buffer (replique-debug--this)
     (unless replique-debug--pause (user-error "The thread is running"))
     ,@body))

(defun replique-debug--frame-at-point ()
  "Return the index of the frame point is on, 0 where it is on none."
  (or (and (derived-mode-p 'replique-debug-mode)
           (plist-get (get-text-property (point) 'replique-debug-frame) :index))
      0))

(defun replique-debug--frame-name (index)
  "Return what the frame INDEX of the buffer is called."
  (let ((frame (seq-find (lambda (f) (eql index (plist-get f :index)))
                         replique-debug--frames)))
    (or (plist-get frame :fn) (plist-get frame :class) (format "frame %s" index))))

(defun replique-debug--request (msg then)
  "Send MSG about the thread of the buffer, and call THEN with the answer.

THEN is not called where the answer is an error, which is said instead."
  (let ((buffer (current-buffer)))
    (replique-process-request
     replique-debug--process msg
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (if (buffer-live-p buffer)
               (with-current-buffer buffer (replique-debug--answered-error frame))
             (message "replique: %s" (plist-get frame :message)))
         (funcall then frame))))))

(defun replique-debug-continue (&optional abort)
  "Let the stopped thread go on from where it stopped.

With ABORT, interactively the prefix argument, `break!' throws instead,
which ends what the thread was doing the way an exception would."
  (interactive "P")
  (replique-debug--in-this
    (replique-debug--request
     (append (list :op :debug-continue :thread replique-debug--thread)
             (when abort (list :abort t)))
     (lambda (_) (message (if abort "replique: aborted" "replique: continued"))))))

(defun replique-debug-abort ()
  "Let the stopped thread go on by throwing from where it stopped."
  (interactive)
  (replique-debug-continue t))

(defun replique-debug-restart ()
  "Start the call of the frame at point over.

Every frame inside it goes, and the call is made again with the arguments
it was made with - to the code as it is now.  Which is what makes a fix
the next thing that runs: change the function, evaluate it, and restart
the frame that called it.

What the call did before it stopped stays done: an atom it swapped is
swapped.

The arguments are the ones the frame still holds, which is all of them
only in code compiled to keep its locals - see `replique-debug-keep-locals'.
Where the code that stopped clears them, the restart is asked for again."
  (interactive)
  (let ((index (replique-debug--frame-at-point)))
    (replique-debug--in-this
      (when (and (plist-get replique-debug--pause :locals-cleared)
                 (not (y-or-n-p (concat "The code that stopped clears its locals, so the"
                                        " call may start over with nil arguments"
                                        " - restart anyway? "))))
        (user-error "Not restarted - M-x replique-debug-keep-locals, and load it again"))
      (replique-debug--request
       (list :op :debug-restart :thread replique-debug--thread :frame index)
       (lambda (_) (message "replique: restarted"))))))

(defvar replique-debug--eval-history nil
  "What was evaluated in a frame.")

(defun replique-debug--show-value (text)
  "Say TEXT, a value printed, laid out where it does not fit a line."
  (if (and (< (length text) (- (frame-width) 20))
           (not (string-search "\n" text)))
      (message "%s" text)
    (let ((buffer (get-buffer-create "*replique-value*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (condition-case nil (replique-pprint-string text) (error text)))
          (goto-char (point-min)))
        (unless (derived-mode-p 'special-mode) (special-mode))
        (setq header-line-format "evaluated in a frame"))
      (display-buffer buffer))))

(defun replique-debug-eval (code)
  "Evaluate CODE in the frame at point, on the thread that stopped.

With the locals of the frame bound, and in its namespace: what the code
sees is what the frame sees, the bindings of the thread included.  A
`def' made here is a `def' of the process like any other."
  (interactive
   (list (read-from-minibuffer "Evaluate in frame: " nil read-expression-map nil
                               'replique-debug--eval-history)))
  (let ((index (replique-debug--frame-at-point)))
    (replique-debug--in-this
      (let ((process replique-debug--process)
            (thread replique-debug--thread))
        (replique-debug--request
         (list :op :debug-eval :thread thread :frame index :code code)
         (lambda (frame)
           ;; What came of it is said later, by an event carrying the number
           ;; this reply gave it - which arrives after this, on the same
           ;; connection
           (puthash (cons process (plist-get frame :evaluation))
                    (lambda (said) (replique-debug--evaluated process thread said))
                    replique-debug--evaluations)))))))

(defun replique-debug--evaluated (process thread said)
  "Show SAID, what came of code run in a frame of THREAD of PROCESS."
  (cond
   ((plist-get said :exception)
    (replique-exception-show (plist-get said :exception) nil nil "evaluated in a frame"))
   ((plist-get said :error) (message "replique: %s" (plist-get said :message)))
   (t (replique-debug--show-value (plist-get said :value))))
  ;; what it ran may have changed what the locals hold
  (dolist (buffer (replique-debug--views-of process thread))
    (with-current-buffer buffer (replique-inspect-refresh))))

(defun replique-debug-visit-frame ()
  "Show the source of the frame at point, and its locals."
  (interactive)
  (let ((frame (get-text-property (point) 'replique-debug-frame)))
    (unless frame (user-error "Not on a frame"))
    (unless replique-debug--pause (user-error "The thread is running"))
    (let ((index (plist-get frame :index)))
      (replique-debug--locals replique-debug--process replique-debug--thread index
                              (replique-debug--frame-name index))
      (unless (replique-debug--show-place
               frame (cons replique-debug--process replique-debug--thread))
        (message "replique: %s is not a file to open"
                 (or (plist-get frame :source) (plist-get frame :class)))))))

(defun replique-debug-refresh ()
  "Ask for the frames of the thread again."
  (interactive)
  (replique-debug--in-this
    (replique-debug--ask-frames)))

(defun replique-debug-toggle-host-frames ()
  "Show the frames that are not Clojure functions, or stop showing them."
  (interactive)
  (setq replique-debug--host-frames (not replique-debug--host-frames))
  (replique-debug--render))

(defvar replique-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'replique-debug-continue)
    (define-key map (kbd "a") #'replique-debug-abort)
    (define-key map (kbd "r") #'replique-debug-restart)
    (define-key map (kbd "e") #'replique-debug-eval)
    (define-key map (kbd "RET") #'replique-debug-visit-frame)
    (define-key map (kbd "g") #'replique-debug-refresh)
    (define-key map (kbd "j") #'replique-debug-toggle-host-frames)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap of a buffer of a stopped thread.")

(define-derived-mode replique-debug-mode special-mode "Replique-Debug"
  "Major mode for a thread of a replique process that stopped.

\\{replique-debug-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (replique-debug-refresh)))
  (setq-local header-line-format '(:eval (replique-debug--header))))

;;;###autoload
(defun replique-debug-keep-locals (&optional clear)
  "Have the code the process compiles from now on keep its locals.

Clojure clears a local after its last use, so that a lazy sequence it
holds can be collected - which leaves a stopped frame holding nils where
its locals were, and a call started over called with nils.  Code compiled
to keep them keeps every one for as long as its frame lives, which costs
the memory such a sequence would have given back.

From now on, and only for what is compiled from now on: code already
loaded keeps clearing until it is loaded again, which is offered for the
file of this buffer - or, in the buffer of a stopped thread, for the file
it stopped in.  A repl whose thread is stopped loads it once the thread
goes on.

With CLEAR, interactively the prefix argument, the code compiled from now
on clears its locals again, which is how Clojure compiles by default."
  (interactive "P")
  (let* ((process (or (and (derived-mode-p 'replique-debug-mode) replique-debug--process)
                      (replique-name-process)
                      (user-error "No process - M-x replique-connect")))
         (frame (replique-process-request-sync
                 process (list :op :locals-clearing :clear (if clear t 'false)))))
    (cond
     ((or (null frame) (equal "error" (plist-get frame :tag)))
      (message "replique: %s" (or (plist-get frame :message) "the process did not answer")))
     (clear (message "replique: the code compiled from now on clears its locals"))
     (t
      (let ((file (replique-debug--file-to-reload)))
        (if (and file (called-interactively-p 'any)
                 (y-or-n-p (format "Locals are kept by the code compiled from now on - load %s again? "
                                   (file-name-nondirectory file))))
            (with-current-buffer (find-file-noselect file)
              (replique-load-file))
          (message "replique: locals are kept by the code compiled from now on%s"
                   " - load what you debug again")))))))

(defun replique-debug--file-to-reload ()
  "Return the file `replique-debug-keep-locals' offers to load again, or nil."
  (cond
   ((and (derived-mode-p 'replique-debug-mode) replique-debug--pause
         (not (plist-get replique-debug--pause :entry)))
    (plist-get replique-debug--pause :file))
   ((and buffer-file-name (derived-mode-p 'replique-clojure-mode)
         (string-match-p "\\.clj[c]?\\'" buffer-file-name))
    buffer-file-name)))

;;;###autoload
(defun replique-debug ()
  "Show a thread of the current process that is stopped.

Asked of the process, so that a thread that stopped while Emacs was not
connected - or whose buffer was killed - is shown all the same."
  (interactive)
  (let ((process (replique-process-ensure)))
    (replique-process-request
     process (list :op :debug-paused)
     (lambda (frame)
       (let ((paused (plist-get frame :paused)))
         (cond
          ((equal "error" (plist-get frame :tag))
           (message "replique: %s" (plist-get frame :message)))
          ((and (null paused) (not (plist-get frame :available)))
           (message "replique: no thread is stopped, and none can be - %s"
                    "the process was not started with `replique-debugger'"))
          ((null paused) (message "replique: no thread is stopped"))
          ((null (cdr paused)) (replique-debug--stopped process (car paused)))
          (t (let* ((names (mapcar (lambda (p) (cons (plist-get p :name) p)) paused))
                    (name (completing-read "Thread: " names nil t)))
               (replique-debug--stopped process (cdr (assoc name names)))))))))))

(provide 'replique-debug)

;;; replique-debug.el ends here
