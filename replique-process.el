;;; replique-process.el --- Starting and finding replique processes  -*- lexical-binding: t; -*-

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

;; A replique process is either one Emacs started, or one that was already
;; running and left its description in .replique-processes.
;;
;; Where the output of the process shows up depends on which of the two it
;; is.  A process Emacs started writes to a pipe Emacs reads, so it is asked
;; not to tee - it would otherwise be reported twice, once through the pipe
;; and once as an event.  A process Emacs merely connected to reports through
;; its control connection.  Both end up in the same buffer.
;;
;; That buffer matters more than it looks: what reaches it is everything the
;; process printed that belongs to no repl - a background thread, a logging
;; framework, a library that writes to System.out from the very form you just
;; evaluated.  A developer who cannot find it will think that output vanished.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'replique-common)
(require 'replique-edn)
(require 'replique-conn)

(defcustom replique-clojure-program "clojure"
  "The clojure command used to start a process."
  :type 'string
  :group 'replique)

(defcustom replique-deps nil
  "EDN passed to clojure as -Sdeps when starting a process.

Nil when the project puts replique on the classpath itself, which is the
usual case for the project replique is being developed in."
  :type '(choice (const :tag "The project provides replique" nil) string)
  :group 'replique)

(cl-defstruct (replique-process
               (:constructor replique-process--make)
               (:conc-name replique-process--))
  "A replique process.

INFO is the description the process gives of itself - the same map it
writes to its port file.  PROC is the operating system process when Emacs
started it, nil when Emacs only connected to it."
  id host port directory info proc control repls output-buffer)

(defvar replique-process-started-hook nil
  "Functions called with a process once it is connected.")

(defvar replique-processes nil
  "The replique processes this Emacs is connected to.")

(defvar replique-current-process nil
  "The process the commands act on.")

;;; The process registry

(defun replique-process-live-p (process)
  "Return non-nil when PROCESS still has a control connection."
  (and process (replique-conn-live-p (replique-process--control process))))

(defun replique-processes-live ()
  "Return the processes that are still connected."
  (seq-filter #'replique-process-live-p replique-processes))

(defun replique-process--register (process)
  "Remember PROCESS and make it the current one."
  (setq replique-processes (cons process replique-processes))
  (setq replique-current-process process)
  process)

(defun replique-process--forget (process)
  "Drop PROCESS from the registry."
  (setq replique-processes (delq process replique-processes))
  (when (eq replique-current-process process)
    (setq replique-current-process (car (replique-processes-live)))))

(defun replique-process-current ()
  "Return the process the commands act on, or nil.

Falls back to the only live one, which is what there usually is."
  (cond
   ((replique-process-live-p replique-current-process) replique-current-process)
   (t (setq replique-current-process (car (replique-processes-live))))))

(defun replique-process-ensure ()
  "Return the process the commands act on, or signal that there is none."
  (or (replique-process-current)
      (user-error "No replique process - M-x replique-start or M-x replique-connect")))

;;; The output of a process

(defun replique-process-buffer (process)
  "Return the buffer holding what PROCESS printed outside of any repl."
  (let ((buffer (replique-process--output-buffer process)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer
                    (format "*replique-process: %s*" (replique-process--id process))))
      (with-current-buffer buffer
        (setq-local buffer-read-only t))
      (setf (replique-process--output-buffer process) buffer))
    buffer))

(defun replique-process--insert (process string &optional face)
  "Show STRING in the output buffer of PROCESS, with FACE."
  (when (and string (not (string-empty-p string)))
    (let ((buffer (replique-process-buffer process)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              ;; Follow the output only for a window that was already at the
              ;; end - a developer reading further up is not dragged along
              (windows (seq-filter
                        (lambda (w) (= (window-point w) (point-max)))
                        (get-buffer-window-list buffer nil t)))
              (at-end (= (point) (point-max))))
          (save-excursion
            (goto-char (point-max))
            (insert (if face (propertize string 'font-lock-face face) string)))
          (when at-end (goto-char (point-max)))
          (dolist (w windows) (set-window-point w (point-max))))))))

(defun replique-process--note (process format &rest args)
  "Say something about PROCESS in its output buffer, from FORMAT and ARGS."
  (replique-process--insert process
                          (concat (apply #'format format args) "\n")
                          'replique-note))

;;; Events

(defun replique-process--event (process frame)
  "Handle FRAME, an unsolicited frame from the control connection of PROCESS."
  (pcase (plist-get frame :event)
    ("out" (replique-process--insert process (plist-get frame :string)))
    ("err" (replique-process--insert process (plist-get frame :string) 'replique-stderr))
    ("uncaught-exception"
     (let ((thread (plist-get frame :thread))
           (message (plist-get frame :message)))
       (replique-process--insert
        process
        (format "Exception in thread \"%s\" %s\n" thread message)
        'replique-stderr)
       (message "replique: exception in thread \"%s\": %s" thread message)))
    ("dropped"
     ;; Written where the gap is: everything that survived came before it
     (replique-process--note process "... %s events were dropped ..."
                            (plist-get frame :count)))
    (_ nil)))

(defun replique-process--frame (process frame)
  "Handle FRAME on the control connection of PROCESS."
  (pcase (plist-get frame :tag)
    ("event" (replique-process--event process frame))
    ("error" (message "replique: %s: %s"
                      (plist-get frame :error) (plist-get frame :message)))
    (_ nil)))

;;; Connecting

(defun replique-processes-directory (directory)
  "Return the directory holding the description of the processes in DIRECTORY."
  (expand-file-name ".replique/processes/" directory))

(defun replique-process-descriptions (directory)
  "Return the processes that say they are running in DIRECTORY.

A description is what the process wrote to its port file.  A stale file -
one whose process is gone - is not told apart here: the :process-id of the
handshake is what catches that."
  (let ((dir (replique-processes-directory directory)))
    (when (file-directory-p dir)
      (seq-keep
       (lambda (file)
         (condition-case nil
             (with-temp-buffer
               (insert-file-contents file)
               (json-parse-string (buffer-string)
                                  :object-type 'plist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil))
           (error nil)))
       (directory-files dir t "\\.json\\'")))))

(defun replique-process--connect (info os-proc on-ready)
  "Open the control connection of the process described by INFO.

OS-PROC is the operating system process when Emacs started it.  ON-READY
is called with the replique process once the handshake is in."
  (let ((process (replique-process--make
                  :id (plist-get info :process-id)
                  :host (plist-get info :host)
                  :port (plist-get info :port)
                  :directory (plist-get info :directory)
                  :info info
                  :proc os-proc
                  :repls nil)))
    (setf (replique-process--control process)
          (replique-conn-open
           (plist-get info :host)
           (plist-get info :port)
           'control
           ;; Against a stale port file: the process answering on that port
           ;; may not be the one the file describes
           :process-id (plist-get info :process-id)
           :on-frame (lambda (frame) (replique-process--frame process frame))
           :on-close (lambda (_conn)
                       (replique-process--note process "The process is gone")
                       (replique-process--forget process))
           :on-ready (lambda (_conn)
                       (replique-process--register process)
                       (when on-ready (funcall on-ready process)))))
    process))

;;; Starting

(defun replique-process--id-for (directory)
  "Return a process id naming DIRECTORY, or nil when its name cannot be one.

A process id is used as a file name, so the process refuses anything that
would not make one."
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (when (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9._+-]\\{0,127\\}\\'" name)
      name)))

(defun replique-process--command (directory)
  "Return the command starting a replique process in DIRECTORY."
  (let ((id (replique-process--id-for directory)))
    (append (list replique-clojure-program)
            (when replique-deps (list "-Sdeps" replique-deps))
            (list "-M" "-m" "replique.main")
            (list (replique-edn-map
                   (append
                    ;; Emacs reads the pipe of a process it started, and would
                    ;; otherwise be told everything twice
                    (list :tee-output 'false)
                    (when id (list :process-id id))))))))

(defun replique-process--spawn-filter (proc string)
  "Read the startup line PROC wrote in STRING, then let its output through."
  (let ((process (process-get proc 'replique-process)))
    (if process
        (replique-process--insert process string)
      (let* ((acc (concat (or (process-get proc 'replique-acc) "") string))
             (idx (string-search "\n" acc)))
        (if (not idx)
            (process-put proc 'replique-acc acc)
          (let ((line (substring acc 0 idx))
                (rest (substring acc (1+ idx))))
            (process-put proc 'replique-acc nil)
            (replique-process--started proc line)
            (unless (string-empty-p rest)
              (replique-process--spawn-filter proc rest))))))))

(defun replique-process--started (proc line)
  "Act on LINE, the startup line PROC wrote."
  (let ((info (condition-case nil
                  (json-parse-string line
                                     :object-type 'plist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)
                (error nil))))
    (cond
     ((null info)
      (message "replique: the process did not say it started: %s" line))
     ((equal "error" (plist-get info :tag))
      (message "replique: the process could not start: %s"
               (plist-get info :message)))
     (t
      (let ((process (replique-process--connect
                      info proc
                      (lambda (process)
                        (message "replique: %s listening on %s:%s"
                                 (replique-process--id process)
                                 (replique-process--host process)
                                 (replique-process--port process))
                        (run-hook-with-args 'replique-process-started-hook process)))))
        (process-put proc 'replique-process process)
        ;; The buffer exists from the start: what the process prints while a
        ;; repl is being opened belongs in it
        (replique-process-buffer process))))))

(defun replique-process--spawn-sentinel (proc _event)
  "Notice that the operating system process PROC is gone."
  (unless (process-live-p proc)
    (let ((process (process-get proc 'replique-process)))
      (when process
        (replique-process--note process "The process exited")))))

;;;###autoload
(defun replique-start (directory)
  "Start a replique process in DIRECTORY and connect to it."
  (interactive (list (read-directory-name "Project directory: " nil nil t)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (default-directory directory)
         (command (replique-process--command directory)))
    (message "replique: %s" (string-join command " "))
    (let ((proc (make-process
                 :name "replique"
                 :buffer nil
                 :command command
                 :coding 'utf-8-unix
                 :connection-type 'pipe
                 :noquery t
                 :filter #'replique-process--spawn-filter
                 :sentinel #'replique-process--spawn-sentinel)))
      proc)))

;;;###autoload
(defun replique-connect (directory)
  "Connect to a replique process already running in DIRECTORY."
  (interactive (list (read-directory-name "Project directory: " nil nil t)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (descriptions (replique-process-descriptions directory)))
    (cond
     ((null descriptions)
      (user-error "No process is running in %s" directory))
     (t
      (let* ((choices (mapcar (lambda (info)
                                (cons (format "%s (%s:%s)"
                                              (plist-get info :process-id)
                                              (plist-get info :host)
                                              (plist-get info :port))
                                      info))
                              descriptions))
             (info (if (cdr choices)
                       (cdr (assoc (completing-read "Process: " choices nil t)
                                   choices))
                     (cdar choices))))
        (replique-process--connect
         info nil
         (lambda (process)
           (replique-process-buffer process)
           (message "replique: connected to %s" (replique-process--id process))
           (run-hook-with-args 'replique-process-started-hook process))))))))

;;; Ops

(defun replique-process-request (process msg &optional callback)
  "Send MSG on the control connection of PROCESS."
  (let ((conn (replique-process--control process)))
    (unless (replique-conn-live-p conn)
      (user-error "The process is not connected"))
    (replique-conn-request conn msg callback)))

(defun replique-describe-process ()
  "Say what the current process is."
  (interactive)
  (let ((process (replique-process-ensure)))
    (replique-process-request
     process (list :op :process-info)
     (lambda (frame)
       (message "replique: %s in %s - clojure %s, java %s, up %ss"
                (plist-get frame :process-id)
                (plist-get frame :directory)
                (plist-get frame :clojure-version)
                (plist-get frame :java-version)
                (/ (or (plist-get frame :uptime) 0) 1000))))))

;;;###autoload
(defun replique-select-process ()
  "Choose the process the commands act on."
  (interactive)
  (let ((processes (replique-processes-live)))
    (unless processes (user-error "No replique process"))
    (let* ((choices (mapcar (lambda (p) (cons (replique-process--id p) p)) processes))
           (choice (completing-read "Process: " choices nil t)))
      (setq replique-current-process (cdr (assoc choice choices)))
      (message "replique: %s" choice))))

(defun replique-show-process-output ()
  "Show what the current process printed outside of any repl."
  (interactive)
  (pop-to-buffer (replique-process-buffer (replique-process-ensure))))

(provide 'replique-process)

;;; replique-process.el ends here
