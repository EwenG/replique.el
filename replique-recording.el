;;; replique-recording.el --- Going through what a recorded definition did  -*- lexical-binding: t; -*-

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

;; What a definition recorded with `replique-debug-record' did is gone
;; through a value at a time, the way a stopped thread is stepped through -
;; back as well as forward, and with nothing stopped.  Where it is at is a
;; cursor: a call, and one of the values it had.  The rest follows from it -
;; the form that had that value, the iteration of each loop around it, the
;; locals in scope there and what they were, and the calls it is in, a call
;; of a recorded definition made while another one ran being that one's.
;;
;; C-c C-d v puts the cursor at the form at point, in the call the
;; definition shows, and shows it in the buffer of the process's
;; recordings: one buffer, which a move moves - with C-u, a new one, to
;; keep the one there is where it is.  It shows the frames of the cursor,
;; innermost first - in each call, the loops around where it is, then the
;; call - and below them the value, under the code that had it, and the
;; locals, to browse as a value is, see `replique-inspect'.  The form of the
;; frame looked at is shown in its code, with an arrow in the fringe, and
;; each definition the cursor is in shows the call it is in, and the
;; iteration.
;;
;;   n  the value after               p  the value before
;;   i  into the call made there      o  out of the call
;;   ]  the same form in the next iteration of the loop of the frame
;;      looked at - or the innermost one of its call -  [  the one before
;;   }  the same form in the next call of the function of the frame
;;      looked at                      {  the one before
;;   s  the first iteration after this one where some code holds
;;   <  look at the frame further out  >  the one further in
;;   v  show what the frame is at, its form in each iteration of its
;;      loop - in each call of its function - or that and the names
;;      bound in each, in turn
;;   c  show the tree of the calls recorded, or the cursor again - see
;;      `replique-recording-calls', which shows it from the code
;;   RET  on a frame, look at it - on an iteration, or a call, go there
;;   g  ask again   q  quit
;;
;; The tree of the calls is of every definition recorded in the process:
;; the calls made in none kept, and under each - TAB - the calls it made,
;; each with what it was called with and what it returned - those of a
;; library function recorded by its calls only too, see `replique-record'.  The calls the
;; cursor is in are open, and RET on a call puts the cursor there, at its
;; first value.
;;
;; A move is made from the frame looked at: from a frame further out, the
;; cursor goes on into the call of the same function made there, at the
;; same form - the next iteration of a loop that calls a function, in
;; that function, where it was.  What only the first values and the last
;; ones of were kept is gone through those, and what was passed by is
;; said.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'text-property-search)
(require 'replique-common)
(require 'replique-process)
(require 'replique-inspect)
(require 'replique-debug)

(defface replique-recording-cursor
  '((t :inherit replique-debug-stopped-line))
  "Face for the form a cursor through what was recorded is at."
  :group 'replique)

(defface replique-recording-frame
  '((t :inherit bold))
  "Face for the frame of the cursor that is looked at."
  :group 'replique)

(defface replique-recording-about
  '((t :inherit shadow))
  "Face for what is said of a call in the tree of the calls: when, where."
  :group 'replique)

(defcustom replique-recording-value-width 40
  "How much of a value a call shows on its line in the tree, in characters."
  :type 'natnum
  :group 'replique)

;;; Where the cursor is, in the code

(defvar replique-recording--arrow nil
  "Where the last cursor shown is, in its code, as a marker.")

(add-to-list 'overlay-arrow-variable-list 'replique-recording--arrow)

(defvar replique-recording--form nil
  "The overlay on the form the last cursor shown is at.")

(defun replique-recording--forget-arrow ()
  "Take the arrow and the highlight away."
  (setq replique-recording--arrow nil)
  (when (overlayp replique-recording--form)
    (delete-overlay replique-recording--form))
  (setq replique-recording--form nil))

(defun replique-recording--mark (process frame)
  "Return the mark of the definition of PROCESS FRAME is in, or nil."
  (when-let* ((unit (plist-get frame :unit))
              (mark (gethash (cons process unit) replique-debug--units))
              ((overlay-buffer mark)))
    mark))

(defun replique-recording--bounds (mark frame)
  "Return where the form FRAME is at is, in the unit MARK marks: (START . END).
The params of a fn entered, rather than the fn."
  (with-current-buffer (overlay-buffer mark)
    (let* ((start (replique-debug--here mark (plist-get frame :line) (plist-get frame :column)))
           (end (if (plist-get frame :end-line)
                    (replique-debug--here mark (plist-get frame :end-line)
                                          (plist-get frame :end-column))
                  (save-excursion (goto-char start) (line-end-position))))
           (params (and (equal "enter" (plist-get frame :site-kind))
                        (replique-debug--params-at start))))
      (if params
          (cons (1- (replique-parse-start (car params)))
                (1+ (replique-parse-end (car (last params)))))
        (cons start (max start end))))))

(defun replique-recording--show-code (process frame)
  "Show the form FRAME, of PROCESS, is at in its code, with the arrow."
  (replique-recording--forget-arrow)
  (when-let* ((mark (replique-recording--mark process frame))
              ((plist-get frame :line))
              (bounds (replique-recording--bounds mark frame))
              (buffer (overlay-buffer mark)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (car bounds))
        (setq replique-recording--arrow (copy-marker (line-beginning-position))))
      (setq replique-recording--form (make-overlay (car bounds) (cdr bounds)))
      (overlay-put replique-recording--form 'face 'replique-recording-cursor)
      (overlay-put replique-recording--form 'priority 10))
    (when-let* ((window (display-buffer buffer '((display-buffer-reuse-window
                                                  display-buffer-use-some-window)
                                                 (inhibit-same-window . t)))))
      (set-window-point window (car bounds)))))

;;; The buffer

(defvar-local replique-recording--process nil
  "The process whose recordings the buffer goes through.")
(put 'replique-recording--process 'permanent-local t)

(defvar-local replique-recording--keys nil
  "The dialect keys of the cursor: nil for Clojure.")
(put 'replique-recording--keys 'permanent-local t)

(defvar-local replique-recording--cursor nil
  "Where the cursor is, as (:unit UNIT :call CALL :at AT), or nil.")
(put 'replique-recording--cursor 'permanent-local t)

(defvar-local replique-recording--frames nil
  "The frames of the cursor, innermost first, as the process said them.")
(put 'replique-recording--frames 'permanent-local t)

(defvar-local replique-recording--frame 0
  "The index of the frame looked at, among `replique-recording--frames'.")
(put 'replique-recording--frame 'permanent-local t)

(defvar-local replique-recording--note nil
  "What the last move passed by or did not find, or nil.")
(put 'replique-recording--note 'permanent-local t)

(defvar-local replique-recording--pinned nil
  "The marks of the definitions made to show the call the cursor is in.")
(put 'replique-recording--pinned 'permanent-local t)

(defvar-local replique-recording--view nil
  "What is shown of the frame looked at.
Nil for what it is at, `iterations' for what its form was in each
iteration of its loop - each call of its function - and `locals' for
that and the names bound in it.")
(put 'replique-recording--view 'permanent-local t)

(defvar-local replique-recording--calls-shown nil
  "Whether the buffer shows the tree of the calls, rather than the cursor.")
(put 'replique-recording--calls-shown 'permanent-local t)

(defvar-local replique-recording--roots nil
  "The calls made in none kept, as the process said them.")
(put 'replique-recording--roots 'permanent-local t)

(defvar-local replique-recording--made nil
  "The calls each call open made, by the call.")
(put 'replique-recording--made 'permanent-local t)

(defvar-local replique-recording--open nil
  "The calls whose calls are shown, by the call.")
(put 'replique-recording--open 'permanent-local t)

(defvar-local replique-recording--soon nil
  "The timer of the tree of the calls being asked for again.")

(defvar-local replique-recording--asking 0
  "Which asking for the calls the answers coming are for.")
(put 'replique-recording--asking 'permanent-local t)

(defvar-local replique-recording--to-row nil
  "Whether point goes to the iteration of the cursor once it is shown.")
(put 'replique-recording--to-row 'permanent-local t)

(defun replique-recording--main-name (process)
  "Return the name of the buffer of the recordings of PROCESS."
  (format "*replique-recording %s*" (replique-process--id process)))

(defun replique-recording--looked-at ()
  "Return the frame looked at, or nil."
  (nth replique-recording--frame replique-recording--frames))

(defun replique-recording--count (frame)
  "Return which time round FRAME is, as it is said: call 2 of 3, iteration 1 of 9."
  (format "%s %s of %s"
          (cond ((equal "call" (plist-get frame :kind)) "call")
                ((plist-get frame :fn) "call")
                (t "iteration"))
          (or (plist-get frame :index) "?")
          (plist-get frame :of)))

(defun replique-recording--where (frame)
  "Return where the form FRAME is at is written, as a short text, or nil."
  (when-let* ((mark (replique-recording--mark replique-recording--process frame))
              ((plist-get frame :line)))
    (with-current-buffer (overlay-buffer mark)
      (format "%s:%s" (buffer-name)
              (line-number-at-pos (replique-debug--here mark (plist-get frame :line)
                                                        (plist-get frame :column))
                                  t)))))

(defun replique-recording--title ()
  "Return what the buffer says it is at: the innermost call of the cursor."
  (if-let* ((call (seq-find (lambda (f) (equal "call" (plist-get f :kind)))
                            replique-recording--frames)))
      (string-join
       (delq nil (list (plist-get call :label)
                       (replique-recording--count call)
                       (plist-get call :thread)
                       (when-let* ((at (plist-get call :started)))
                         (format-time-string "%T" (/ at 1000.0)))))
       " · ")
    "recorded"))

(defun replique-recording--preamble ()
  "Write what is shown above the view - or, the calls shown, all there is."
  (if replique-recording--calls-shown
      (progn (replique-recording--insert-calls) 'alone)
    (replique-recording--insert-frames)))

(defun replique-recording--insert-frames ()
  "Write the frames of the cursor, and what the last move said."
  (let* ((frames replique-recording--frames)
         (width (apply #'max 0 (mapcar (lambda (f) (string-width (or (plist-get f :label) "")))
                                       frames)))
         (counts (mapcar (lambda (f) (if (equal "gone" (plist-get f :kind))
                                         ""
                                       (replique-recording--count f)))
                         frames))
         (count-width (apply #'max 0 (mapcar #'string-width counts)))
         (i 0))
    (insert (propertize "Frames" 'face 'replique-inspect-label) "\n")
    (dolist (frame frames)
      (let ((start (point))
            (looked-at (= i replique-recording--frame)))
        (insert (if looked-at "▸ " "  "))
        (if (equal "gone" (plist-get frame :kind))
            (insert (propertize "made by a call not kept any more" 'face 'replique-note))
          (insert (string-pad (plist-get frame :label) width) "  "
                  (string-pad (nth i counts) count-width)
                  (if-let* ((where (replique-recording--where frame)))
                      (concat "  " (propertize where 'face 'replique-note))
                    "")))
        (when looked-at
          (add-face-text-property start (point) 'replique-recording-frame t))
        (insert "\n")
        (put-text-property start (point) 'replique-recording-frame i))
      (setq i (1+ i)))
    (when replique-recording--note
      (insert (propertize replique-recording--note 'face 'replique-note) "\n"))
    (insert "\n")
    (when-let* ((heading (replique-recording--heading)))
      (insert (propertize heading 'face 'replique-inspect-label) "\n"))))

(defun replique-recording--viewed ()
  "Return what is shown of the frame looked at: nil, `iterations' or `locals'.
Nil, what it is at, of a call not kept any more."
  (unless (equal "gone" (plist-get (replique-recording--looked-at) :kind))
    replique-recording--view))

(defun replique-recording--loop-p (frame)
  "Return non-nil when FRAME is the frame of a loop."
  (equal "loop" (plist-get frame :kind)))

(defun replique-recording--heading ()
  "Return what the view says it shows, above it, or nil."
  (when-let* ((view (replique-recording--viewed))
              (frame (replique-recording--looked-at)))
    (format "%s, in each %s %s%s"
            (plist-get frame :code)
            (if (and (replique-recording--loop-p frame) (not (plist-get frame :fn)))
                "iteration of"
              "call of")
            (plist-get frame :label)
            (cond ((not (eq view 'locals)) "")
                  ((replique-recording--loop-p frame) ", and the names bound in it")
                  (t ", and its params")))))

(defun replique-recording--source ()
  "Return the source of the view of what the frame looked at is at.
Or of what its form was each time round - see `replique-recording--view'."
  (let ((frame (replique-recording--looked-at))
        (view (replique-recording--viewed)))
    (list :debug
          (cond
           ((null view)
            (list :cursor (append replique-recording--cursor
                                  (list :frame replique-recording--frame))))
           ((replique-recording--loop-p frame)
            (append (list :loop (plist-get frame :loop) :call (plist-get frame :call)
                          :site (plist-get frame :form))
                    (when (eq view 'iterations) (list :only t))))
           ((eq view 'iterations) (list :site (plist-get frame :form)))
           (t (list :calls (plist-get frame :unit) :site (plist-get frame :form)))))))

(defun replique-recording--row-label (frame)
  "Return the label of the row of the iteration FRAME is in, or nil."
  (when-let* ((index (plist-get frame :index)))
    (format "%s %s" (if (replique-recording--loop-p frame) "iteration" "call") index)))

(defun replique-recording--row (label)
  "Return the node of the row LABEL of the view, or nil."
  (when (and replique-inspect--nodes (null replique-inspect--focus))
    (seq-find (lambda (id)
                (let ((line (plist-get (replique-inspect--node id) :line)))
                  (and (equal "label" (plist-get line :via))
                       (equal label (plist-get line :key)))))
              (plist-get (replique-inspect--node 0) :children))))

(defvar-local replique-recording--row-overlay nil
  "The overlay on the row of the iteration of the cursor.")

(defun replique-recording--rendered ()
  "Show which row the cursor is in - and go there, once, after a move."
  (when (overlayp replique-recording--row-overlay)
    (delete-overlay replique-recording--row-overlay))
  (when-let* (((replique-recording--viewed))
              (row (replique-recording--row
                    (replique-recording--row-label (replique-recording--looked-at))))
              (at (plist-get (replique-inspect--node row) :at)))
    (let ((start (save-excursion (goto-char at) (line-beginning-position))))
      (setq replique-recording--row-overlay
            (make-overlay start (save-excursion (goto-char at) (line-end-position))))
      (overlay-put replique-recording--row-overlay 'face 'replique-recording-cursor)
      (when replique-recording--to-row
        (setq replique-recording--to-row nil)
        (goto-char start))))
  ;; what is written before the view is asked for is no row
  (when replique-inspect--nodes
    (setq replique-recording--to-row nil)))

(defun replique-recording--follow (frames)
  "Make each definition FRAMES are in show the call they are in, and the iteration.
The innermost call of a definition called inside itself."
  (dolist (frame (reverse frames))
    (when-let* (((equal "call" (plist-get frame :kind)))
                (mark (replique-recording--mark replique-recording--process frame)))
      (let ((call (plist-get frame :call))
            (iteration (plist-get frame :iteration)))
        (unless (and (eql call (overlay-get mark 'replique-debug-call))
                     (equal iteration (overlay-get mark 'replique-debug-iteration)))
          (overlay-put mark 'replique-debug-iteration iteration)
          (replique-debug--ask-recorded mark call))
        (cl-pushnew mark replique-recording--pinned)))))

(defun replique-recording--unpin ()
  "Make the definitions the buffer made show a call show the last one again."
  (dolist (mark replique-recording--pinned)
    (when (overlay-buffer mark)
      (overlay-put mark 'replique-debug-iteration nil)
      (unless (overlay-get mark 'replique-debug-edited)
        (replique-debug--ask-recorded mark))))
  (setq replique-recording--pinned nil))

(defun replique-recording--killed ()
  "Let go of what the buffer, being killed, showed in the code."
  (replique-recording--unpin)
  (replique-recording--forget-arrow))
(put 'replique-recording--killed 'permanent-local-hook t)

(defun replique-recording--show (&optional quiet)
  "Show what the cursor is at, in the buffer and in the code.
With QUIET, the buffer is not shown, only made what it shows."
  (let ((buffer (current-buffer)))
    ;; on before the view is written, which writes the frames above it
    (replique-recording-mode 1)
    (add-hook 'kill-buffer-hook #'replique-recording--killed nil t)
    (replique-inspect-show replique-recording--process replique-recording--keys
                           (replique-recording--source) (replique-recording--title)
                           buffer quiet)
    (with-current-buffer buffer
      (when-let* ((frame (replique-recording--looked-at)))
        (replique-recording--show-code replique-recording--process frame)))))

(defun replique-recording--moved (frame)
  "Take in FRAME, where the process said the cursor is, and show it."
  (setq replique-recording--cursor (list :unit (plist-get frame :unit)
                                         :call (plist-get frame :call)
                                         :at (plist-get frame :at))
        replique-recording--frames (plist-get frame :frames)
        replique-recording--frame (or (plist-get frame :frame) 0)
        replique-recording--note (plist-get frame :note))
  (when replique-recording--note
    (message "replique: %s" replique-recording--note))
  (replique-recording--follow replique-recording--frames)
  (replique-recording--show)
  (when replique-recording--calls-shown
    (replique-recording--open-cursor-calls)
    (replique-recording-refresh-calls)))

(defun replique-recording--ask (msg &optional then)
  "Ask where the cursor goes, MSG saying how, and show it there.
MSG is added to the cursor, and to the frame looked at.  THEN is called
in the buffer once it is shown."
  (let ((buffer (current-buffer)))
    (replique-process-request
     replique-recording--process
     (append (list :op :debug-cursor) replique-recording--cursor
             (list :frame replique-recording--frame)
             msg)
     (lambda (frame)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (if (equal "error" (plist-get frame :tag))
               (message "replique: %s" (plist-get frame :message))
             (replique-recording--moved frame)
             (when then (funcall then)))))))))

;;; Moving

(defun replique-recording--this (&optional no-cursor)
  "Signal that the current buffer is not one going through a recording.
Or, unless NO-CURSOR, that it has no cursor yet."
  (unless replique-recording--process
    (user-error "This is not a buffer going through what was recorded"))
  (unless (or no-cursor replique-recording--cursor)
    (user-error "%s" (substitute-command-keys
                      "No cursor yet - \\<replique-recording-mode-map>\\[replique-recording-select] on a call puts it there"))))

(defun replique-recording--move (move &rest more)
  "Move the cursor by MOVE, from the frame looked at, with MORE said."
  (replique-recording--this)
  (setq replique-recording--to-row t)
  (replique-recording--ask (append (list :move move) more)))

(defun replique-recording-next ()
  "Go to the value after - past the last one of a call, the one it was made for."
  (interactive)
  (replique-recording--move "next"))

(defun replique-recording-previous ()
  "Go to the value before - before the first one of a call, the one before it."
  (interactive)
  (replique-recording--move "previous"))

(defun replique-recording-in ()
  "Go into the call made there, at its first value: what it was called with.
The call made for the value, or else the next call made."
  (interactive)
  (replique-recording--move "in"))

(defun replique-recording-out ()
  "Go out of the call, to the value it was made for."
  (interactive)
  (replique-recording--move "out"))

(defun replique-recording-next-iteration ()
  "Go to the same form in the next iteration kept of a loop.
The loop looked at, or the innermost one of the call looked at."
  (interactive)
  (replique-recording--move "next-iteration"))

(defun replique-recording-previous-iteration ()
  "Go to the same form in the iteration before, kept, of a loop.
See `replique-recording-next-iteration'."
  (interactive)
  (replique-recording--move "previous-iteration"))

(defun replique-recording-next-call ()
  "Go to the same form in the next call kept of the function looked at."
  (interactive)
  (replique-recording--move "next-call"))

(defun replique-recording-previous-call ()
  "Go to the same form in the call before, kept, of the function looked at."
  (interactive)
  (replique-recording--move "previous-call"))

(defun replique-recording--look-at (index)
  "Look at the frame INDEX of the cursor: its value, its locals, its code."
  (replique-recording--this)
  (unless (and (<= 0 index) (< index (length replique-recording--frames)))
    (user-error (if (< index 0) "This is the innermost frame" "This is the outermost frame")))
  (when (equal "gone" (plist-get (nth index replique-recording--frames) :kind))
    (user-error "That call is not kept any more"))
  (setq replique-recording--frame index
        replique-recording--note nil
        replique-recording--to-row t)
  (replique-recording--show))

(defun replique-recording-up ()
  "Look at the frame further out."
  (interactive)
  (replique-recording--look-at (1+ replique-recording--frame)))

(defun replique-recording-down ()
  "Look at the frame further in."
  (interactive)
  (replique-recording--look-at (1- replique-recording--frame)))

(defun replique-recording--row-at-point ()
  "Return the iteration, or the call, of the row point is on: (MOVE INDEX).
Or nil."
  (when-let* (((replique-recording--viewed))
              (id (get-text-property (point) 'replique-inspect-node))
              ((memq id (plist-get (replique-inspect--node 0) :children)))
              ((null replique-inspect--focus))
              (key (plist-get (plist-get (replique-inspect--node id) :line) :key))
              ((string-match "\\`\\(iteration\\|call\\) \\([0-9]+\\)\\'" key)))
    (list (match-string 1 key) (string-to-number (match-string 2 key)))))

(defun replique-recording-select ()
  "Look at the frame at point - or go to the iteration of the row at point.
Or, on a value, browse the part at point - or, in the tree of the calls, go
to the call at point."
  (interactive)
  (if-let* ((index (and (not replique-recording--calls-shown)
                        (get-text-property (point) 'replique-recording-frame))))
      (replique-recording--look-at index)
    (cond
     ((replique-recording--call-at-point)
      (replique-recording--go-to-call (replique-recording--call-at-point)))
     (replique-recording--calls-shown (user-error "No call here"))
     ((replique-recording--row-at-point)
      (let ((row (replique-recording--row-at-point)))
        (setq replique-recording--to-row t)
        (replique-recording--move (car row) :index (cadr row))))
     (t (replique-inspect-focus)))))

(defun replique-recording-cycle-view ()
  "Show something else of the frame looked at, in turn.
What it is at, the value of its form in each iteration of its loop - each
call of its function, of a call - and that and the names bound in each."
  (interactive)
  (replique-recording--this)
  (when (equal "gone" (plist-get (replique-recording--looked-at) :kind))
    (user-error "That call is not kept any more"))
  (setq replique-recording--view (pcase replique-recording--view
                                   ('nil 'iterations)
                                   ('iterations 'locals)
                                   (_ nil))
        replique-recording--to-row t)
  (message "replique: %s"
           (pcase replique-recording--view
             ('nil "what the frame is at")
             ('iterations "the form, each time round")
             (_ "the form and the names, each time round")))
  (replique-recording--show))

(defun replique-recording-refresh ()
  "Ask again where the cursor is, and what is there - or the calls, shown."
  (interactive)
  (replique-recording--this replique-recording--calls-shown)
  (if replique-recording--calls-shown
      (replique-recording-refresh-calls)
    (replique-recording--ask nil)))

(defvar replique-recording--find-history nil
  "What `replique-recording-find' was asked to find.")

(defun replique-recording-find (code &optional backward)
  "Go to the first iteration after this one where CODE is true.

Of the loop looked at, or the innermost one of the call looked at - CODE
evaluated with the locals in scope at the form of the cursor bound to what
they were in each iteration kept.  With BACKWARD, interactively the
prefix argument, the last one before."
  (interactive (list (read-string (if current-prefix-arg "Last iteration where: "
                                    "First iteration where: ")
                                  nil 'replique-recording--find-history)
                     current-prefix-arg))
  (replique-recording--this)
  (let* ((looked-at (replique-recording--looked-at))
         (frames replique-recording--frames)
         (level (plist-get looked-at :level))
         (loop (if (equal "loop" (plist-get looked-at :kind))
                   looked-at
                 (or (seq-find (lambda (f) (and (eql level (plist-get f :level))
                                                (equal "loop" (plist-get f :kind))))
                               frames)
                     (user-error "No loop went round around here"))))
         (index (seq-position frames loop))
         (call (seq-find (lambda (f) (and (eql level (plist-get f :level))
                                          (equal "call" (plist-get f :kind))))
                         frames))
         (from (plist-get loop :index))
         (buffer (current-buffer)))
    (replique-process-request
     replique-recording--process
     (append (list :op :debug-find-iteration
                   :unit (plist-get loop :unit) :call (plist-get loop :call)
                   :site (plist-get loop :loop)
                   :line (plist-get call :line) :column (plist-get call :column)
                   :code code)
             (when from (list :from from))
             (when backward (list :backward t)))
     (lambda (frame)
       (cond
        ((equal "error" (plist-get frame :tag))
         (message "replique: %s" (plist-get frame :message)))
        ((plist-get frame :index)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq replique-recording--frame index)
             (replique-recording--ask (list :move "iteration" :index (plist-get frame :index))))))
        (t (message "replique: no iteration kept %sholds that%s"
                    (cond ((null from) "")
                          (backward (format "before iteration %s " from))
                          (t (format "after iteration %s " from)))
                    (if-let* ((error (plist-get frame :error)))
                        (format " - it threw: %s" error)
                      ""))))))))

(defun replique-recording-quit ()
  "Kill the buffer, and let go of what it showed in the code."
  (interactive)
  (quit-window t))

(defvar replique-recording-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'replique-recording-next)
    (define-key map (kbd "p") #'replique-recording-previous)
    (define-key map (kbd "i") #'replique-recording-in)
    (define-key map (kbd "o") #'replique-recording-out)
    (define-key map (kbd "]") #'replique-recording-next-iteration)
    (define-key map (kbd "[") #'replique-recording-previous-iteration)
    (define-key map (kbd "}") #'replique-recording-next-call)
    (define-key map (kbd "{") #'replique-recording-previous-call)
    (define-key map (kbd "<") #'replique-recording-up)
    (define-key map (kbd ">") #'replique-recording-down)
    (define-key map (kbd "s") #'replique-recording-find)
    (define-key map (kbd "v") #'replique-recording-cycle-view)
    (define-key map (kbd "c") #'replique-recording-toggle-calls)
    (define-key map (kbd "TAB") #'replique-recording-toggle)
    (define-key map (kbd "<tab>") #'replique-recording-toggle)
    (define-key map (kbd "RET") #'replique-recording-select)
    (define-key map (kbd "g") #'replique-recording-refresh)
    (define-key map (kbd "q") #'replique-recording-quit)
    map)
  "Keymap of a buffer going through what was recorded.")

(define-minor-mode replique-recording-mode
  "Go through what a recorded definition did, a value at a time.

The buffer is a view of what the cursor is at, which a move moves - see
`replique-recording'.

\\{replique-recording-mode-map}"
  :lighter " Recorded"
  :keymap replique-recording-mode-map
  (setq-local replique-inspect-preamble-function
              (when replique-recording-mode #'replique-recording--preamble))
  (setq-local replique-inspect-rendered-function
              (when replique-recording-mode #'replique-recording--rendered)))
(put 'replique-recording-mode 'permanent-local t)

;;; The tree of the calls

(defun replique-recording--cursor-calls ()
  "Return the calls the cursor is in, innermost first, each (UNIT . CALL)."
  (delq nil (mapcar (lambda (f) (and (equal "call" (plist-get f :kind))
                                     (replique-recording--key f)))
                    replique-recording--frames)))

(defun replique-recording--key (call)
  "Return what CALL, a plist, is known by in the tree: (UNIT . CALL).
The calls of a ClojureScript runtime are numbered apart from the JVM's."
  (cons (plist-get call :unit) (plist-get call :call)))

(defun replique-recording--value (text)
  "Return TEXT, a value printed, as a line of the tree shows it."
  (replique-clojure-fontify-string
   (let ((replique-debug-value-width replique-recording-value-width))
     (replique-debug--value-text text))))

(defun replique-recording--insert-call (call depth in)
  "Write the line of CALL, DEPTH calls deep, and the calls it made, open.
IN are the calls the cursor is in, innermost first."
  (let* ((n (replique-recording--key call))
         (open (gethash n replique-recording--open))
         (made (or (plist-get call :made) 0))
         (start (point)))
    (insert (make-string (* 2 depth) ?\s)
            (cond ((zerop made) "  ") (open "▾ ") (t "▸ "))
            (if (plist-get call :made-in-gone)
                (propertize "↖ " 'face 'replique-recording-about
                            'help-echo "Made in a call not kept any more")
              "")
            "(" (propertize (plist-get call :label) 'face 'font-lock-function-name-face))
    (dolist (arg (plist-get call :args))
      (insert " " (replique-recording--value arg)))
    (insert ")"
            (let ((value (plist-get call :value)))
              (cond ((null value) (propertize " …" 'face 'replique-recording-about))
                    ;; of a library function, recorded by its calls only
                    ((plist-get call :threw)
                     (propertize (concat " ⇒ threw " value) 'face 'replique-exception))
                    (t (concat " ⇒ " (replique-recording--value value)))))
            (propertize
             (concat "  · "
                     (string-join
                      (delq nil
                            (list (when (plist-get call :index)
                                    (format "call %s of %s" (plist-get call :index)
                                            (plist-get call :of)))
                                  (plist-get call :thread)
                                  (when-let* ((at (plist-get call :started)))
                                    (format-time-string "%T" (/ at 1000.0)))
                                  (unless (zerop made)
                                    (format "%s call%s" made (if (= 1 made) "" "s")))))
                      " · "))
             'face 'replique-recording-about))
    (cond ((equal n (car in))
           (add-face-text-property start (point) 'replique-recording-cursor t)
           (insert (propertize "  ◂ the cursor" 'face 'replique-recording-frame)))
          ((member n in)
           (add-face-text-property start (point) 'replique-recording-frame t)))
    (insert "\n")
    (put-text-property start (point) 'replique-recording-call call)
    (when open
      (dolist (c (gethash n replique-recording--made))
        (replique-recording--insert-call c (1+ depth) in)))))

(defun replique-recording--insert-calls ()
  "Write the tree of the calls."
  (insert (propertize (substitute-command-keys
                       "Calls recorded - \\<replique-recording-mode-map>\\[replique-recording-toggle-calls] for the cursor\n\n")
                      'face 'replique-inspect-label))
  (if replique-recording--roots
      (let ((in (replique-recording--cursor-calls)))
        (dolist (call replique-recording--roots)
          (replique-recording--insert-call call 0 in)))
    (insert (propertize "No call recorded is kept\n" 'face 'replique-note))))

(defun replique-recording--render-calls ()
  "Write the buffer again, the tree shown."
  (let ((inhibit-read-only t))
    (replique-inspect--render)))

(defun replique-recording--call-line (n)
  "Return where the line of the call N starts, or nil."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((found (text-property-search-forward
                        'replique-recording-call n
                        (lambda (n call) (equal n (replique-recording--key call))))))
      (prop-match-beginning found))))

(defun replique-recording--tree-kept ()
  "Make what keeps which calls of the tree are open, where there is none."
  (unless (hash-table-p replique-recording--open)
    (setq replique-recording--open (make-hash-table :test #'equal)
          replique-recording--made (make-hash-table :test #'equal))))

(defun replique-recording-refresh-calls (&optional then)
  "Ask the process for the calls again, and the calls made in the ones open.
THEN is called in the buffer once they are written."
  (interactive)
  (replique-recording--this t)
  (replique-recording--tree-kept)
  (let* ((buffer (current-buffer))
         (process replique-recording--process)
         (asking (setq replique-recording--asking (1+ replique-recording--asking)))
         (open (hash-table-keys replique-recording--open))
         (pending (1+ (length open)))
         (made (make-hash-table :test #'equal))
         (roots :none)
         (answered
          (lambda ()
            (setq pending (1- pending))
            (when (and (zerop pending) (buffer-live-p buffer))
              (with-current-buffer buffer
                (when (= asking replique-recording--asking)
                  (unless (eq roots :none) (setq replique-recording--roots roots))
                  (setq replique-recording--made made)
                  (when replique-recording--calls-shown
                    (replique-recording--render-calls))
                  (when then (funcall then))))))))
    (replique-process-request
     process (list :op :debug-calls)
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (message "replique: %s" (plist-get frame :message))
         (setq roots (plist-get frame :calls)))
       (funcall answered)))
    (dolist (n open)
      (replique-process-request
       process (list :op :debug-calls :unit (car n) :call (cdr n))
       (lambda (frame)
         (unless (equal "error" (plist-get frame :tag))
           (puthash n (plist-get frame :calls) made))
         (funcall answered))))))

(defun replique-recording--open-cursor-calls ()
  "Open the calls the cursor is in, so that its call is shown in the tree."
  (replique-recording--tree-kept)
  (dolist (f (cdr (seq-filter (lambda (f) (equal "call" (plist-get f :kind)))
                              replique-recording--frames)))
    (puthash (replique-recording--key f) t replique-recording--open)))

(defun replique-recording-toggle-calls ()
  "Show the tree of the calls recorded, or the cursor again.
In the tree, the calls the cursor is in are open, and point is on its call."
  (interactive)
  ;; the tree is shown with no cursor yet, and the cursor only with one
  (replique-recording--this (not replique-recording--calls-shown))
  (replique-recording--tree-kept)
  (setq replique-recording--calls-shown (not replique-recording--calls-shown))
  (if (not replique-recording--calls-shown)
      (progn (replique-recording--to-top)
             (replique-recording--show))
    (replique-recording--open-cursor-calls)
    (replique-recording--render-calls)
    (replique-recording-refresh-calls
     (lambda ()
       (when-let* ((at (replique-recording--call-line (car (replique-recording--cursor-calls)))))
         (goto-char at)
         (when-let* ((window (get-buffer-window (current-buffer))))
           (set-window-point window at)))))))

(defun replique-recording--call-at-point ()
  "Return the call of the line at point in the tree, or nil."
  (and replique-recording--calls-shown
       (get-text-property (point) 'replique-recording-call)))

(defun replique-recording-toggle ()
  "Show the calls the call at point made, or hide them - or, on a value,
open the part at point, or close it."
  (interactive)
  (if-let* ((call (replique-recording--call-at-point)))
      (let ((n (replique-recording--key call)))
        (cond
         ((gethash n replique-recording--open)
          (remhash n replique-recording--open)
          (replique-recording--render-calls))
         ((zerop (or (plist-get call :made) 0))
          (message "replique: it made no call kept"))
         (t (puthash n t replique-recording--open)
            (replique-recording-refresh-calls))))
    (unless replique-recording--calls-shown
      (replique-inspect-toggle))))

(defun replique-recording--to-top ()
  "Go to the top of the buffer, to see the cursor from there once shown."
  (goto-char (point-min))
  (when-let* ((window (get-buffer-window (current-buffer))))
    (set-window-point window (point-min))))

(defun replique-recording--go-to-call (call)
  "Move the cursor to the first value of CALL, and show the cursor."
  (replique-recording--to-top)
  (setq replique-recording--calls-shown nil
        ;; of the runtime it is in, which the tree mixes
        replique-recording--keys (replique-debug--unit-keys call)
        replique-recording--cursor (list :unit (plist-get call :unit) :call (plist-get call :call))
        replique-recording--frame 0
        replique-recording--note nil
        replique-recording--to-row t)
  (replique-recording--ask nil))

(defun replique-recording--event (process frame)
  "Show the calls again soon, where they are shown: FRAME, an event of
PROCESS, says a definition recorded.  Once for many."
  (when (equal "debug-recorded" (plist-get frame :event))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and replique-recording-mode
                   replique-recording--calls-shown
                   (eq process replique-recording--process)
                   (get-buffer-window buffer t)
                   (not (timerp replique-recording--soon)))
          (setq replique-recording--soon
                (run-with-timer 0.3 nil
                                (lambda ()
                                  (when (buffer-live-p buffer)
                                    (with-current-buffer buffer
                                      (setq replique-recording--soon nil)
                                      (when replique-recording--calls-shown
                                        (replique-recording-refresh-calls))))))))))))

(add-hook 'replique-process-event-functions #'replique-recording--event)

;;; From the code

;;;###autoload
(defun replique-recording-calls ()
  "Show the calls recorded in this buffer\\='s process, as a tree.

Of every function recorded, Clojure and ClojureScript: the calls made in
none kept, oldest first, and under each the calls it made - each with what
it was called with and what it returned.  RET on one goes through what it
did, from its first value.  In the buffer of the recordings of the
process, which shows the cursor again with
\\<replique-recording-mode-map>\\[replique-recording-toggle-calls] - see `replique-recording'."
  (interactive)
  (let* ((process (replique-process-ensure))
         (buffer (get-buffer-create (replique-recording--main-name process))))
    (with-current-buffer buffer
      (unless replique-recording-mode
        ;; a buffer of the recordings with nothing to look at yet, but the
        ;; calls
        (replique-inspect-mode)
        (setq replique-inspect--process process)
        (replique-recording-mode 1)
        (add-hook 'kill-buffer-hook #'replique-recording--killed nil t))
      (setq replique-recording--process process)
      (if replique-recording--calls-shown
          (replique-recording-refresh-calls)
        (replique-recording-toggle-calls)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun replique-recording (&optional new)
  "Go through what the definition at point recorded, from the form at point.

In the call it shows, at the value of the form at point - the one nearest
where the cursor of the call is, or its last - in the buffer of the
recordings of the process, the one cursor it has.  With NEW,
interactively the prefix argument, in a buffer of its own, which keeps
the one there is where it is.  See `replique-recording-mode'."
  (interactive "P")
  (let* ((mark (replique-debug--recording-at-point t))
         (process (overlay-get mark 'replique-debug-process))
         (o (replique-debug--recorded-form-at (point)))
         (site (and o (overlay-get o 'replique-debug-site)))
         (call (overlay-get mark 'replique-debug-shown-call))
         (name (replique-recording--main-name process))
         (buffer (if new (generate-new-buffer name) (get-buffer-create name))))
    (unless call (user-error "Not called yet"))
    (with-current-buffer buffer
      (let ((at (and (equal call (plist-get replique-recording--cursor :call))
                     (plist-get replique-recording--cursor :at))))
        (setq replique-recording--process process
              replique-recording--calls-shown nil
              replique-recording--keys (overlay-get mark 'replique-debug-keys)
              replique-recording--cursor (list :unit (overlay-get mark 'replique-debug-unit)
                                               :call call)
              replique-recording--frame 0)
        (replique-recording--ask (append (when at (list :at at)) (when site (list :site site))))))
    buffer))

(provide 'replique-recording)

;;; replique-recording.el ends here
