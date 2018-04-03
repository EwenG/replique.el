;; replique-watch.el ---   -*- lexical-binding: t; -*-

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

(require 'replique-list-vars)
(require 'replique-pprint)

(defvar-local replique-watch/directory nil)
(defvar-local replique-watch/repl-env nil)
(defvar-local replique-watch/repl-type nil)
(defvar-local replique-watch/var-name nil)
(defvar-local replique-watch/buffer-id nil)
(defvar-local replique-watch/print-length nil)
(defvar-local replique-watch/print-level nil)
(defvar-local replique-watch/browse-path nil)
(defvar buffer-id-generator 0)

(defun replique-watch/new-buffer-id ()
  (let ((new-buffer-id buffer-id-generator))
    (setq buffer-id-generator (+ 1 buffer-id-generator))
    new-buffer-id))

(defun replique-watch/pprint (val)
  (let ((inhibit-read-only t)
        (replique-pprint/is-multi-line? nil)
        (p (point)))
    (font-lock-mode -1)
    (erase-buffer)
    (insert val)
    (goto-char (point-min))
    (replique-pprint/pprint)
    (font-lock-mode 1)
    (goto-char p)
    (set-buffer-modified-p nil)))

(defun replique-watch/do-watch (tooling-repl repl var-ns var-name)
  (let* ((var-name (format "%s/%s" var-ns var-name))
         (buffer-id (replique-watch/new-buffer-id))
         (repl-env (replique/get repl :repl-env))
         (repl-type (replique/get repl :repl-type))
         (watch-buffer (generate-new-buffer (format "*watch*%s*" var-name)))
         (resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :add-watch
                                   :repl-env repl-env
                                   :var-sym (make-symbol (format "'%s" var-name))
                                   :buffer-id buffer-id))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (kill-buffer watch-buffer)
            (message "%s" (replique-pprint/pprint-error-str err))
            (message "watch failed with var %s" var-name)
            nil)
        (let* ((var-value (replique/get resp :var-value))
               (ref-watchers (replique/get tooling-repl :ref-watchers))
               (ns-prefix (if (equal repl-type :cljs) "cljs.core" "clojure.core"))
               (print-length (thread-first repl
                               (replique/get :params)
                               (replique/get (concat ns-prefix "/*print-length*"))))
               (print-level (thread-first repl
                              (replique/get :params)
                              (replique/get (concat ns-prefix "/*print-level*")))))
          (with-current-buffer watch-buffer
            (buffer-disable-undo watch-buffer)
            (clojure-mode)
            (replique-watch/minor-mode)
            (setq buffer-read-only t)
            (setq replique-watch/directory (replique/get resp :process-id))
            (setq replique-watch/repl-env repl-env)
            (setq replique-watch/repl-type repl-type)
            (setq replique-watch/var-name var-name)
            (setq replique-watch/buffer-id buffer-id)
            (setq replique-watch/print-length print-length)
            (setq replique-watch/print-level print-level)
            (setq replique-watch/browse-path '()))
          (puthash buffer-id watch-buffer ref-watchers)
          (let ((resp (replique/send-tooling-msg
                       tooling-repl
                       (replique/hash-map :type :refresh-watch
                                          :update? t
                                          :repl-env repl-env
                                          :var-sym (make-symbol (format "'%s" var-name))
                                          :buffer-id buffer-id
                                          :print-length print-length
                                          :print-level print-level
                                          :browse-path `(quote ,replique-watch/browse-path)))))
            (let ((err (replique/get resp :error)))
              (if err
                  (if (replique/get resp :undefined)
                      (message "%s is undefined" var-name)
                    (message "%s" (replique-pprint/pprint-error-str err))
                    (message "refresh watch failed with var %s" var-name))
                (with-current-buffer watch-buffer
                  (replique-watch/pprint (replique/get resp :var-value)))
                (display-buffer watch-buffer)))))))))

(defun replique-watch/on-kill-buffer ()
  (when replique-watch/directory
    (when-let ((tooling-repl (replique/repl-by :repl-type :tooling
                                               :directory replique-watch/directory)))
      (let ((resp (replique/send-tooling-msg
                   tooling-repl
                   (replique/hash-map :type :remove-watch
                                      :repl-env replique-watch/repl-env
                                      :buffer-id replique-watch/buffer-id))))
        (let ((err (replique/get resp :error)))
          (when err
            (message "%s" (replique-pprint/pprint-error-str err))
            (error "Error while removing watcher for var: %s" replique-watch/var-name)))
        (let ((ref-watchers (replique/get tooling-repl :ref-watchers)))
          (remhash replique-watch/buffer-id ref-watchers))))))

(add-hook 'kill-buffer-hook 'replique-watch/on-kill-buffer)

(defun replique-watch/watch* (var-ns tooling-repl repl)
  (replique-list-vars/list-vars
   var-ns tooling-repl repl
   "Watch var: " 'replique-watch/do-watch))

(defun replique-watch/watch-clj (tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (when var-ns
        (replique-watch/watch* var-ns tooling-repl clj-repl)))))

(defun replique-watch/watch-cljs (tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (when var-ns
        (replique-watch/watch* var-ns tooling-repl cljs-repl)))))

(defun replique-watch/watch-cljc (tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (when var-ns
        (replique-watch/watch* var-ns tooling-repl repl)))))

(defun replique/watch ()
  (interactive)
  (if (not (featurep 'ivy))
      (user-error "replique-watch requires ivy-mode")
    (replique/with-modes-dispatch
     (clojure-mode . 'replique-watch/watch-clj)
     (clojurescript-mode . 'replique-watch/watch-cljs)
     (clojurec-mode . 'replique-watch/watch-cljc)
     (t . (user-error "Unsupported major mode: %s" major-mode)))))

(defun replique-watch/notify-update (msg)
  (let* ((directory (replique/get msg :process-id))
         (tooling-repl (replique/repl-by :repl-type :tooling
                                         :directory directory)))
    (when tooling-repl
      (let* ((buffer-id (replique/get msg :buffer-id))
             (var-value (replique/get msg :var-value))
             (ref-watchers (replique/get tooling-repl :ref-watchers))
             (watch-buffer (replique/get ref-watchers buffer-id)))
        (with-current-buffer watch-buffer
          (set-buffer-modified-p t))))))

(defun replique-watch/check-orphan-buffer (tooling-repl buffer-id buffer)
  (let ((ref-watchers (replique/get tooling-repl :ref-watchers)))
    (when (not (replique/contains? ref-watchers buffer-id))
      (puthash buffer-id buffer ref-watchers)))) 

(defun replique-watch/refresh (&optional no-update?)
  (interactive)
  (if (not (seq-contains minor-mode-list 'replique-watch/minor-mode))
      (user-error "replique-watch/refresh can only be used in a watch buffer")
    (when replique-watch/directory
      (let ((tooling-repl (replique/repl-by :repl-type :tooling
                                            :directory replique-watch/directory)))
        (when tooling-repl
          (replique-watch/check-orphan-buffer
           tooling-repl replique-watch/buffer-id (current-buffer))
          (let ((resp (replique/send-tooling-msg
                       tooling-repl
                       (replique/hash-map :type :refresh-watch
                                          :update? (null no-update?)
                                          :repl-env replique-watch/repl-env
                                          :var-sym (make-symbol
                                                    (format "'%s" replique-watch/var-name))
                                          :buffer-id replique-watch/buffer-id
                                          :print-length replique-watch/print-length
                                          :print-level replique-watch/print-level
                                          :browse-path `(quote ,replique-watch/browse-path)))))
            (let ((err (replique/get resp :error)))
              (if err
                  (if (replique/get resp :undefined)
                      (message "%s is undefined" replique-watch/var-name)
                    (message "%s" (replique-pprint/pprint-error-str err))
                    (message "refresh watch failed with var %s" var-name))
                (message "Refreshing ...")
                (replique-watch/pprint (replique/get resp :var-value))
                (message "Refreshing ... done")))))))))

(defun replique-watch/do-browse (candidate)
  (setq replique-watch/browse-path replique-watch/temporary-browse-path)
  (replique-watch/refresh t))

(defun replique-watch/browse-backward-delete-char ()
  (interactive)
  (if (equal "" ivy-text)
      (let ((new-browse-path (cdr replique-watch/temporary-browse-path)))
        (ivy-quit-and-run
         (let ((replique-watch/temporary-browse-path new-browse-path))
           (replique-watch/browse*))))
    (ivy-backward-delete-char)))

(defun replique-watch/browse-alt-done ()
  (interactive)
  (when-let (candidate (nth ivy--index ivy--all-candidates))
    (let ((new-browse-path (cons candidate replique-watch/temporary-browse-path)))
      (ivy-quit-and-run
       (let ((replique-watch/temporary-browse-path new-browse-path))
         (replique-watch/browse*))))))

(defvar replique-watch/browse-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") 'replique-watch/browse-backward-delete-char)
    (define-key map (kbd "C-j") 'replique-watch/browse-alt-done)
    map))

(defun replique-watch/browse-path->string (browse-path)
  (if (eq '() browse-path)
      ""
    (concat (string-join (reverse browse-path) " ") " ")))

(defun replique-watch/browse-candidates (user-input)
  (with-ivy-window
    (let* ((tooling-repl (replique/repl-by :repl-type :tooling
                                           :directory replique-watch/directory))
           (resp (replique/send-tooling-msg
                  tooling-repl
                  (replique/hash-map :type :browse-candidates
                                     :repl-env replique-watch/repl-env
                                     :var-sym (make-symbol (format "'%s" replique-watch/var-name))
                                     :buffer-id replique-watch/buffer-id
                                     :browse-path `(quote ,replique-watch/temporary-browse-path)
                                     :prefix user-input))))
      (let ((err (replique/get resp :error)))
        (if err
            (if (replique/get resp :undefined)
                (error "%s is undefined" replique-watch/var-name)
              (message "%s" (replique-pprint/pprint-error-str err))
              (error "Browse failed while requesting browse candidates"))
          (replique/get resp :candidates))))))

(defvar replique-watch/temporary-browse-path nil)

(defun replique-watch/browse* ()
  (ivy-read (concat "Browse path: "
                    (replique-watch/browse-path->string
                     replique-watch/temporary-browse-path))
            'replique-watch/browse-candidates
            :dynamic-collection t
            :action 'replique-watch/do-browse
            :require-match nil
            :keymap replique-watch/browse-map
            :caller 'replique-watch/browse))

(defun replique-watch/browse ()
  (interactive)
  (cond ((not (featurep 'ivy))
         (user-error "replique-watch/browse requires ivy-mode"))
        ((not (bound-and-true-p replique-watch/minor-mode))
         (user-error "replique-watch/browse can only be used from a watch buffer"))
        (t (let ((replique-watch/temporary-browse-path replique-watch/browse-path))
             (replique-watch/browse*)))))

(defvar replique-watch/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'replique-watch/refresh)
    (define-key map "b" 'replique-watch/browse)
    (easy-menu-define replique-watch/minor-mode-menu map
      "Replique-watch Minor Mode Menu"
      '("Replique-watch"
        ["Refresh watch" replique-watch/refresh t]
        ["Browse" replique-watch/browse t]))
    map))

(define-minor-mode replique-watch/minor-mode
  "Minor mode for interacting with a replique watch buffer."
  :lighter "Replique-watch" :keymap replique-watch/minor-mode-map)

(provide 'replique-watch)

;; hide non public interactive commands
