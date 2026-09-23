;;; replique-conn.el --- Connections to a replique process  -*- lexical-binding: t; -*-

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

;; One TCP connection to a replique process, of either role.
;;
;; The process writes one JSON object per line, so a raw newline is always a
;; frame boundary: the filter can look for one without invoking the parser.
;; What a line holds is parsed in C, with null and false read as nil - a
;; frame's own keys are never null, so absence is all there is to test.
;;
;; A control connection answers its requests in order and every reply carries
;; the id of its request, which is also what tells a reply from an unsolicited
;; event.  Everything else is handed to the connection's frame handler.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'replique-edn)

(cl-defstruct (replique-conn
               (:constructor replique-conn--make)
               (:conc-name replique-conn--))
  "A connection to a replique process.

PROC is the network process, KIND is `control' or `repl', ID is the name
the process gave the connection - what `:interrupt' targets - and INFO is
the reply to the handshake.  PENDING holds the callbacks of the requests
that have not been answered yet, ACC the piece of a line that has arrived
without its newline."
  proc kind id info pending next-id acc on-frame on-close)

(defconst replique-conn-closed-error "connection-closed"
  "The `:error' of the frame a request gets when the connection dies.

Not something a process sends - it is the answer replique gives on its
behalf when there will be no answer.  A caller that only ever hears back
when a request succeeds is a command that silently does nothing when the
process is gone.")

(defconst replique-conn-unanswered-error "unanswered"
  "The `:error' of the frame a handshake gets when nothing answers it.

Like `replique-conn-closed-error\=', not something a process sends: it is
what replique answers on its own behalf when a port took the connection
and then said nothing.  What it says about the port file it was read from
is what a refusal says - the process that file names is not there - and
it is the only thing that can say it, because a port that accepts a
connection is a port that does not look dead.")

(defcustom replique-conn-handshake-timeout 10
  "How long to wait, in seconds, for the reply to a handshake.

A port that accepts a connection is not a process that speaks replique.
The port file of a process that was killed goes on naming a port, and by
the time it is read that port can be held by something else - or by the
same jvm, no longer answering, which the kernel takes a connection for
anyway, out of the backlog of a socket nothing is reading.  Either way
the reply never comes, and with no deadline the command that went looking
waits for it for the rest of the session, saying nothing and leaving the
file that sent it there in place.

Generous rather than tight: the process is already running and the reply
is the first thing it writes, so what is being waited out is a jvm busy
with something else - and a wait that gave up early would throw away the
port file of a process that is there."
  :type 'number
  :group 'replique)

(defun replique-conn-open (host port kind &rest keys)
  "Open a connection to the process at HOST and PORT and shake hands.

KIND is `control' or `repl'.  KEYS may hold:

  :process-id  refuse the connection unless the process is that one, which
               is what guards against a stale port file
  :on-ready    called with the connection once the handshake reply is in.
               No frame precedes it, so nothing can be missed by waiting
  :on-frame    called with every frame that is not a reply to a request
               made here
  :on-close    called with the connection when the process closes it
  :on-error    called with the error frame when the handshake does not
               succeed, instead of saying it in the echo area.  Which
               frame says which is in the handshake below
  :buffer      the buffer of the network process, for the repl role
  :hello       extra keys for the handshake message.  What the role itself
               takes rather than what every connection takes - the
               `:dialect' and `:target' of a repl - so that this knows the
               shape of a handshake without knowing what each role makes of
               one"
  (let* ((process-id (plist-get keys :process-id))
         (on-ready (plist-get keys :on-ready))
         (on-error (plist-get keys :on-error))
         (proc (make-network-process
                :name (format "replique-%s" kind)
                :host host
                :service port
                :buffer (plist-get keys :buffer)
                :coding 'utf-8-unix
                :noquery t
                :filter #'replique-conn--filter
                :sentinel #'replique-conn--sentinel))
         (conn (replique-conn--make
                :proc proc :kind kind :next-id 1 :acc ""
                :on-frame (plist-get keys :on-frame)
                :on-close (plist-get keys :on-close))))
    (process-put proc 'replique-conn conn)
    (let* ((timer nil)
           (refused
            (lambda (frame said)
              ;; The connection is closed after an unsuccessful handshake,
              ;; so there is nothing to recover - say what happened and let
              ;; go.  What it means is the caller's to know: a port file
              ;; naming a process that is not there is something it can act
              ;; on, and the frame is what tells the three apart
              (if on-error
                  (funcall on-error frame)
                (message "replique: %s" said))))
           (id (replique-conn-request
                conn
                (append (list :op :hello :role (intern (format ":%s" kind)))
                        (when process-id (list :process-id process-id))
                        (plist-get keys :hello))
                (lambda (frame)
                  (when timer (cancel-timer timer) (setq timer nil))
                  (cond
                   ;; A connection that closed before it answered is a
                   ;; refusal of its own, and reported as one.  Passing over
                   ;; it leaves the caller nothing: it read this port from a
                   ;; file, and a port that takes a connection and drops it
                   ;; is the file saying something that is no longer true.
                   ;; `:on-close' handles a process going away, which is what
                   ;; this is not - nothing was ever connected to
                   ((equal replique-conn-closed-error (plist-get frame :error))
                    (funcall refused frame
                             (format "%s:%s closed the connection before answering"
                                     host port)))
                   ((equal "error" (plist-get frame :tag))
                    (funcall refused frame
                             (format "the handshake was refused: %s (%s)"
                                     (plist-get frame :message)
                                     (plist-get frame :error))))
                   (t
                    (setf (replique-conn--id conn) (plist-get frame :connection))
                    (setf (replique-conn--info conn) frame)
                    (when on-ready (funcall on-ready conn))))))))
      ;; After the request and only while it is still waiting: a reply read
      ;; on the way out of `replique-conn-request' has been handled already,
      ;; and a timer armed behind it would be one nothing cancels
      (when (assoc id (replique-conn--pending conn))
        (setq timer
              (run-at-time
               replique-conn-handshake-timeout nil
               (lambda ()
                 (setq timer nil)
                 ;; Taken off the list before anything is said, so that a
                 ;; reply arriving late is a reply to nobody rather than a
                 ;; second answer to a question already answered
                 (when-let* ((cell (assoc id (replique-conn--pending conn))))
                   (setf (replique-conn--pending conn)
                         (delq cell (replique-conn--pending conn)))
                   (funcall refused
                            (list :tag "error"
                                  :error replique-conn-unanswered-error
                                  :message (format "No reply in %ss"
                                                   replique-conn-handshake-timeout)
                                  :id id)
                            (format "%s:%s took the connection and did not answer in %ss"
                                    host port replique-conn-handshake-timeout))
                   ;; Nothing is going to come of it, and a socket left open
                   ;; on a port that answers nothing is a connection the
                   ;; commands would go on finding
                   (replique-conn-close conn)))))))
    conn))

(defun replique-conn-live-p (conn)
  "Return non-nil when CONN is still connected."
  (and conn (process-live-p (replique-conn--proc conn))))

(defun replique-conn-close (conn)
  "Close CONN."
  (when (replique-conn-live-p conn)
    (delete-process (replique-conn--proc conn))))

(defun replique-conn-request (conn msg &optional callback)
  "Send MSG, a property list, as a request on CONN.

CALLBACK is called with the reply - or with the error frame, which carries
the same id.  Returns the id."
  (let ((id (replique-conn--next-id conn)))
    (setf (replique-conn--next-id conn) (1+ id))
    (when callback
      ;; Appended rather than pushed: replies come back in request order, and
      ;; the list reads the way the requests were made
      (setf (replique-conn--pending conn)
            (append (replique-conn--pending conn) (list (cons id callback)))))
    (replique-conn-send-line conn (replique-edn-map (append msg (list :id id))))
    id))

(defconst replique-conn-timeout-error "timeout"
  "The `:error' of the frame a synchronous request gets when none came back.

Like `replique-conn-closed-error', not something a process sends: it is
what replique answers on its behalf when the wait ran out.  A control
connection answers its requests in order, so a request made behind a slow
one waits for that one too - what this says is that the process is busy,
and not that it refused.")

(defconst replique-conn--unanswered (make-symbol "unanswered")
  "What a synchronous request holds until its reply arrives.

A symbol of its own because every other value is one a reply could be:
nil is what a frame that has not arrived and a frame that arrived empty
would both look like.")

(defun replique-conn-request-sync (conn msg &optional timeout)
  "Send MSG on CONN and wait for the reply, which is returned.

Nil where nobody waited to the end, which is what \\[keyboard-quit]
says: a keystroke that was abandoned has nothing to report.  Everything
else comes back as a frame, an error one included - a request that cannot
be answered is answered all the same, so that there is one thing to look
at and not two.

For what a keystroke asks.  `replique-conn-request' is what everything
else uses: an answer that arrives in a callback is an answer nothing had
to wait for, and waiting is right only where the caller cannot carry on
without it - `completion-at-point-functions' is called for what it
returns and has nowhere to put an answer that comes later.

Quitting is what makes this safe to call while somebody is typing.  Emacs
is held inside `accept-process-output' for as long as the process takes,
so \\[keyboard-quit] has to be heard: `inhibit-quit' keeps it from
unwinding out of the middle of the wait, and `with-local-quit' is where
it is heard instead,
which leaves the connection whole and the request pending.  A pending
request is the right thing to leave: its reply is read and handed to a
callback nobody is listening to, where dropping it would leave a reply
with nothing to match and it would be handled as if the process had said
it unprompted.

TIMEOUT is how long to wait, in seconds, two of them by default.  The
wait is made in short pieces rather than in one, because a timer that
fires while this one waits can ask a question of its own, and a frame
that arrives is read by whichever call to `accept-process-output' is
running - so a wait that asked to be woken by output alone could be woken
by none of it.  Each piece looks at what arrived while it was not
running."
  (if (not (replique-conn-live-p conn))
      (list :tag "error"
            :error replique-conn-closed-error
            :message "The connection to the process closed")
    (let* ((answer replique-conn--unanswered)
           (proc (replique-conn--proc conn))
           (timeout (or timeout 2.0))
           (deadline (+ (float-time) timeout))
           (id (replique-conn-request conn msg (lambda (frame) (setq answer frame)))))
      (let ((inhibit-quit t))
        (while (and (eq answer replique-conn--unanswered)
                    (null quit-flag)
                    (> deadline (float-time)))
          (with-local-quit
            (accept-process-output
             proc
             (min 0.1 (max 0.001 (- deadline (float-time))))
             nil
             ;; Only this process: what another one wrote is not what this
             ;; wait is about, and reading it here would run its filter
             ;; underneath whatever asked for this
             t)))
        (cond
         ;; Before the answer, so that a C-g pressed as one arrived is a
         ;; C-g: what was asked for is no longer wanted either way
         (quit-flag (setq quit-flag nil) nil)
         ((not (eq answer replique-conn--unanswered)) answer)
         (t (list :tag "error"
                  :error replique-conn-timeout-error
                  :message (format "The process did not answer in %ss" timeout)
                  :id id)))))))

(defun replique-conn-send-line (conn line)
  "Write LINE, a message, on CONN."
  (process-send-string (replique-conn--proc conn) (concat line "\n")))

(defun replique-conn-send-code (conn code)
  "Write CODE on the repl connection CONN.

After the handshake a repl reads code, not messages, so this goes out as
it is - over as many lines as it takes."
  (process-send-string (replique-conn--proc conn) (concat code "\n")))

(defun replique-conn--filter (proc string)
  "Split what PROC wrote in STRING into frames."
  (let ((conn (process-get proc 'replique-conn)))
    (when conn
      (let ((acc (concat (replique-conn--acc conn) string))
            (start 0)
            (lines nil)
            (idx nil))
        (while (setq idx (string-search "\n" acc start))
          (push (substring acc start idx) lines)
          (setq start (1+ idx)))
        ;; What is left is a line that has not arrived whole.  Kept before
        ;; anything is dispatched: a handler that fails must not make the
        ;; frames of this very write arrive twice
        (setf (replique-conn--acc conn) (substring acc start))
        (dolist (line (nreverse lines))
          (replique-conn--line conn line))))))

(defun replique-conn--line (conn line)
  "Parse LINE and dispatch it on CONN."
  (let ((frame (condition-case err
                   (json-parse-string line
                                      :object-type 'plist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object nil)
                 (error
                  (message "replique: could not parse a frame: %s (%s)"
                           line (error-message-string err))
                  nil))))
    (when frame
      (with-demoted-errors "replique: error handling a frame: %S"
        (replique-conn--dispatch conn frame)))))

(defun replique-conn--dispatch (conn frame)
  "Hand FRAME to whatever is waiting for it on CONN."
  (let* ((id (plist-get frame :id))
         (cell (and id (assoc id (replique-conn--pending conn)))))
    (if cell
        (progn
          (setf (replique-conn--pending conn)
                (delq cell (replique-conn--pending conn)))
          (funcall (cdr cell) frame))
      (when (replique-conn--on-frame conn)
        (funcall (replique-conn--on-frame conn) frame)))))

(defun replique-conn--abandon (conn)
  "Tell whoever is waiting on CONN that no answer is coming.

The same shape a process would have refused with, so that a caller has
one way of hearing about both - see `replique-conn-closed-error' for
telling this one apart."
  (let ((pending (replique-conn--pending conn)))
    (setf (replique-conn--pending conn) nil)
    (dolist (cell pending)
      (with-demoted-errors "replique: error abandoning a request: %S"
        (funcall (cdr cell)
                 (list :tag "error"
                       :error replique-conn-closed-error
                       :message "The connection to the process closed"
                       :id (car cell)))))))

(defun replique-conn--sentinel (proc _event)
  "Notice that PROC is gone."
  (unless (process-live-p proc)
    (let ((conn (process-get proc 'replique-conn)))
      (when conn
        (replique-conn--abandon conn)
        (when (replique-conn--on-close conn)
          (with-demoted-errors "replique: error closing a connection: %S"
            (funcall (replique-conn--on-close conn) conn)))))))

(provide 'replique-conn)

;;; replique-conn.el ends here
