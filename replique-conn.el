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
  :on-error    called with the error frame when the handshake is refused,
               instead of saying it in the echo area
  :buffer      the buffer of the network process, for the repl role"
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
    (replique-conn-request
     conn
     (append (list :op :hello :role (intern (format ":%s" kind)))
             (when process-id (list :process-id process-id)))
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           ;; The connection is closed after an unsuccessful handshake, so
           ;; there is nothing to recover - say what happened and let go.
           ;; What a refusal means is the caller\='s to know: a port file
           ;; naming a process that is not there is a refusal it can act on
           (if on-error
               (funcall on-error frame)
             (message "replique: the handshake was refused: %s (%s)"
                      (plist-get frame :message) (plist-get frame :error)))
         (setf (replique-conn--id conn) (plist-get frame :connection))
         (setf (replique-conn--info conn) frame)
         (when on-ready (funcall on-ready conn)))))
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

(defun replique-conn--sentinel (proc _event)
  "Notice that PROC is gone."
  (unless (process-live-p proc)
    (let ((conn (process-get proc 'replique-conn)))
      (when conn
        ;; The requests that will never be answered go with it
        (setf (replique-conn--pending conn) nil)
        (when (replique-conn--on-close conn)
          (with-demoted-errors "replique: error closing a connection: %S"
            (funcall (replique-conn--on-close conn) conn)))))))

(provide 'replique-conn)

;;; replique-conn.el ends here
