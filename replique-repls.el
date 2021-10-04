;; replique-repls.el ---   -*- lexical-binding: t; -*-

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

;; Commentary:

;; Code:

(require 'subr-x)
(require 'comint)
(require 'clj-print)
(require 'clj-pprint)
(require 'replique-transit)

(defvar replique/repls nil)

(defun replique/plist->alist (plist)
  (let ((alist '()))
    (while plist
      (setq alist (push `(,(car plist) . ,(cadr plist)) alist))
      (setq plist (cddr plist)))
    alist))

(defun replique/update-repl (old-repl updated-repl)
  (setq replique/repls (mapcar (lambda (repl)
                                 (if (eq repl old-repl)
                                     updated-repl
                                   repl))
                               replique/repls))
  updated-repl)

(defun replique/repls-or-repl-by (filtering-fn matching-fn source &rest args)
  (let* ((pred (lambda (repl)
                 (seq-every-p (lambda (arg)
                                (let ((k (car arg))
                                      (v (cdr arg)))
                                  (or (equal :error-on-nil k)
                                      (and (clj-data/contains? repl k)
                                           (funcall matching-fn (clj-data/get repl k) v)))))
                              (replique/plist->alist args))))
         (found (funcall filtering-fn pred source)))
    (if (and (null found) (plist-get args :error-on-nil))
        (user-error "No started REPL")
      found)))

(defun replique/repl-by (&rest args)
  (apply 'replique/repls-or-repl-by 'seq-find 'equal replique/repls
         `(:started? t ,@args)))

(defun replique/repl-by-maybe-not-started (&rest args)
  (apply 'replique/repls-or-repl-by 'seq-find 'equal replique/repls args))

(defun replique/repls-by (&rest args)
  (apply 'replique/repls-or-repl-by 'seq-filter 'equal replique/repls
         `(:started? t ,@args)))

(defun replique/repls-by-maybe-not-started (&rest args)
  (apply 'replique/repls-or-repl-by 'seq-filter 'equal replique/repls args))

(defun replique/active-repl (repl-type-or-types &optional error-on-nil)
  ;; filter the repls to only keep the repls of the active process
  (let* ((active-process-dir (thread-first
                                 (replique/repl-by :repl-type :tooling :error-on-nil error-on-nil)
                               (clj-data/get :directory)))
         (replique/repls (if active-process-dir
                             (seq-filter (lambda (repl)
                                           (string= active-process-dir
                                                    (clj-data/get repl :directory)))
                                         replique/repls))))
    (if (listp repl-type-or-types)
        (apply 'replique/repls-or-repl-by 'seq-find
               (lambda (repl-v repl-type-or-types)
                 (member repl-v repl-type-or-types))
               replique/repls (list :repl-type repl-type-or-types))
      (replique/repl-by :repl-type repl-type-or-types :error-on-nil error-on-nil))))

(defmacro replique/with-modes-dispatch (&rest modes-alist)
  (let* ((tooling-repl-sym (make-symbol "tooling-repl"))
         (clj-repl-sym (make-symbol "clj-repl-sym"))
         (cljs-repl-sym (make-symbol "cljs-repl-sym"))
         (active-repl-sym (make-symbol "active-repl-sym"))
         (dispatch-code
          (mapcar
           (lambda (item)
             (let ((m (car item))
                   (f (cdr item)))
               ;; The order of priority is the order of the modes as defined during
               ;; the use of the macro
               (cond ((equal 'clojure-mode m)
                      `((equal 'clojure-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,clj-repl-sym)))
                     ((equal 'clojurescript-mode m)
                      `((equal 'clojurescript-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'clojurec-mode m)
                      `((equal 'clojurec-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,active-repl-sym)))
                     ((equal 'css-mode m)
                      `((equal 'css-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'js2-mode m)
                      `((equal 'js2-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'sass-mode m)
                      `((equal 'sass-mode major-mode)
                        (funcall ,f ,tooling-repl-sym (or ,cljs-repl-sym ,active-repl-sym))))
                     ((equal 'scss-mode m)
                      `((equal 'scss-mode major-mode)
                        (funcall ,f ,tooling-repl-sym (or ,cljs-repl-sym ,active-repl-sym))))
                     ((equal 'less-css-mode m)
                      `((equal 'less-css-mode major-mode)
                        (funcall ,f ,tooling-repl-sym (or ,cljs-repl-sym ,active-repl-sym))))
                     ((equal 'stylus-mode m)
                      `((equal 'stylus-mode major-mode)
                        (funcall ,f ,tooling-repl-sym (or ,cljs-repl-sym ,active-repl-sym))))
                     ((equal 'replique/mode m)
                      ;; Don't pass the tooling-repl since the buffer currently in use may not
                      ;; be the active one
                      `((equal 'replique/mode major-mode)
                        (funcall ,f (replique/repl-by :buffer (current-buffer)))))
                     ((equal 't m)
                      `(t ,f)))))
           modes-alist)))
    `(let* ((,tooling-repl-sym (replique/active-repl :tooling t))
            (,clj-repl-sym (replique/active-repl :clj))
            (,cljs-repl-sym (replique/active-repl :cljs))
            (,active-repl-sym (replique/active-repl '(:clj :cljs))))
       (cond ,@dispatch-code))))

(defvar replique/correlation-id 0)
(defvar replique/synchronous-messages (clj-data/hash-map))

(defun replique/send-tooling-msg (tooling-repl msg)
  (let* ((tooling-network-proc (clj-data/get tooling-repl :network-proc))
         (process-id (clj-data/get tooling-repl :directory))
         (correlation-id replique/correlation-id)
         (msg (clj-data/assoc msg
                              :process-id process-id
                              :correlation-id correlation-id))
         (serialized-msg (clj-print/print-str msg)))
    ;; replique-context may generate large message (fn-param, fn-param-meta ...)
    ;; We limit the size of a message because of the jvm method size limit
    (when (< (length serialized-msg) 60000)
      (puthash correlation-id clj-data/nothing replique/synchronous-messages)
      (setq replique/correlation-id  (if (= most-positive-fixnum correlation-id)
                                         0
                                       (+ 1 correlation-id)))
      (process-send-string
       tooling-network-proc
       (format "(replique.tooling-msg/tooling-msg-handle %s)\n" serialized-msg))
      ;; Synchronously wait for a response from the tooling REPL.
      ;; accept-process-output maybe be wake up by a quit signal or some output from the network
      ;; process. The network process output may be split into multiple incomplete
      ;; messages chunks.
      ;; Also, multiple call to replique/send-tooling-msg may happen simultaneously because
      ;; of timers and the output received from the network process may not wake up all
      ;; blocking calls to accept-process-output
      (let ((inhibit-quit t)
            (wait-output? t)
            (result nil))
        (while wait-output?
          (with-local-quit (accept-process-output tooling-network-proc nil nil t))
          (let ((sync-msg (clj-data/get replique/synchronous-messages correlation-id)))
            (cond ((not (eq clj-data/nothing sync-msg))
                   (remhash correlation-id replique/synchronous-messages)
                   ;; There may be other blocking calls to accept-process-output.
                   ;; Try to wake up them by sending a block message
                   (process-send-string tooling-network-proc "nil\n")
                   (setq quit-flag nil)
                   (setq wait-output? nil)
                   (setq result sync-msg))
                  (quit-flag (remhash correlation-id replique/synchronous-messages)))))
        (replique-transit/decode result)))))

(defun replique/comint-is-closed-sexpr (start limit)
  (let* ((parser-state (parse-partial-sexp start limit -1))
         (depth (car parser-state))
         (in-string? (nth 3 parser-state)))
    (and (equal depth 0) (null in-string?))))

(defvar replique/eval-from-source-meta (clj-data/hash-map :url nil
                                                          :line nil
                                                          :column nil))

(defun replique/comint-input-sender (proc string)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (resp (replique/send-tooling-msg
                tooling-repl
                (clj-data/assoc replique/eval-from-source-meta
                                :type :source-meta
                                :repl-env :replique/clj))))
    (let ((err (clj-data/get resp :error)))
      (when err
        (message "%s" (clj-pprint/pprint-error-str err))
        (message "source-meta failed"))
      (comint-simple-send proc string))))

(defun replique/comint-send-input ()
  "Send the pending comint buffer input to the comint process if the pending input is well formed and point is after the prompt"
  (interactive)
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (comint-input-sender 'replique/comint-input-sender))
        (cond (;; Point is at the end of the line and the sexpr is
               ;; terminated
               (and (equal (point) (point-max))
                    (replique/comint-is-closed-sexpr pmark (point)))
               (comint-send-input))
              ;; Point is after the prompt but (before the end of line or
              ;; the sexpr is not terminated)
              ((comint-after-pmark-p) (comint-accumulate))
              ;; Point is before the prompt. Do nothing.
              (t nil))))))

(defun replique/comint-kill-input ()
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (if (> (point) (marker-position pmark))
        (let ((killed (buffer-substring-no-properties pmark (point))))
          (kill-region pmark (point))
          killed)
      "")))

(defun replique/comint-send-input-from-source (input)
  (let ((process (get-buffer-process (current-buffer))))
    (when (not process)
      (user-error "Current buffer has no process"))
    (goto-char (point-max))
    (let ((old-input (replique/comint-kill-input)))
      (goto-char (process-mark process))
      (insert input)
      (replique/comint-send-input)
      (goto-char (process-mark process))
      (insert old-input))))

(defun replique/send-input-from-source-session (input repl)
  (let ((buff (clj-data/get repl :buffer)))
    (with-current-buffer buff
      (replique/comint-send-input-from-source input))))

(defun replique/send-input-from-source-clj (input tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (let ((buff (clj-data/get clj-repl :buffer)))
      (with-current-buffer buff
        (replique/comint-send-input-from-source input)))))

(defun replique/send-input-from-source-cljs (input tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (let ((buff (clj-data/get cljs-repl :buffer)))
      (with-current-buffer buff
        (replique/comint-send-input-from-source input)))))

(defun replique/send-input-from-source-cljc
    (input tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((buff (clj-data/get repl :buffer)))
      (with-current-buffer buff
        (replique/comint-send-input-from-source input)))))

(defun replique/send-input-from-source-dispatch (input)
  (replique/with-modes-dispatch
   (replique/mode . (apply-partially 'replique/send-input-from-source-session input))
   (clojure-mode . (apply-partially 'replique/send-input-from-source-clj input))
   (clojurescript-mode . (apply-partially'replique/send-input-from-source-cljs input))
   (clojurec-mode . (apply-partially'replique/send-input-from-source-cljc input))
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(provide 'replique-repls)
