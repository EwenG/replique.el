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

(require 'replique-repls)
(require 'replique-list-vars)
(require 'clj-data)
(require 'clj-context)
(require 'clj-highlight)
(require 'replique-context)
(require 'clj-pprint)
(require 'clj-print)
(require 'clj-browse)
(require 'ivy)

(defvar-local replique-watch/directory nil)
(defvar-local replique-watch/repl-env nil)
(defvar-local replique-watch/repl-type nil)
(defvar-local replique-watch/var-name nil)
(defvar-local replique-watch/buffer-id nil)
(defvar-local replique-watch/print-length nil)
(defvar-local replique-watch/print-level nil)
(defvar-local replique-watch/print-meta nil)
(defvar-local replique-watch/record-size nil)
(defvar buffer-id-generator 0)

(defun replique-watch/new-buffer-id (watch-type)
  (let ((new-buffer-id buffer-id-generator))
    (setq buffer-id-generator (+ 1 buffer-id-generator))
    (concat watch-type "-" (number-to-string new-buffer-id))))

(defun replique-watch/init-watch-buffer (tooling-repl repl buffer-id watch-buffer)
  (let* ((ref-watchers (clj-data/get tooling-repl :ref-watchers))
         (repl-type (clj-data/get repl :repl-type))
         (repl-env (clj-data/get repl :repl-env))
         (ns-prefix (if (equal repl-type :cljs) "cljs.core" "clojure.core"))
         (print-length (thread-first repl
                         (clj-data/get :params)
                         (clj-data/get (concat ns-prefix "/*print-length*"))))
         (print-level (thread-first repl
                        (clj-data/get :params)
                        (clj-data/get (concat ns-prefix "/*print-level*"))))
         (print-meta (thread-first repl
                       (clj-data/get :params)
                       (clj-data/get (concat ns-prefix "/*print-meta*")))))
    (with-current-buffer watch-buffer
      (replique-watch/minor-mode)
      (setq replique-watch/repl-type repl-type)
      (setq replique-watch/repl-env repl-env)
      (setq replique-watch/directory (clj-data/get tooling-repl :directory))
      (setq replique-watch/print-length print-length)
      (setq replique-watch/print-level print-level)
      (setq replique-watch/print-meta print-meta)
      (setq replique-watch/buffer-id buffer-id)
      (setq replique-watch/directory (clj-data/get tooling-repl :directory)))
    (puthash buffer-id watch-buffer ref-watchers)))

(defun replique-watch/do-watch (tooling-repl repl buffer-id var-name)
  (let* ((repl-env (clj-data/get repl :repl-env))
         (watch-buffer (generate-new-buffer (format "*watch*%s*" var-name)))
         (resp (replique/send-tooling-msg
                tooling-repl
                (clj-data/hash-map :type :add-watch
                                   :repl-env repl-env
                                   :var-sym (make-symbol (format "'%s" var-name))
                                   :buffer-id buffer-id))))
    (let ((err (clj-data/get resp :error)))
      (if err
          (progn
            (kill-buffer watch-buffer)
            (message "%s" (clj-pprint/pprint-error-str err))
            (message "watch failed with var %s" var-name)
            nil)
        (let* ((repl-type (clj-data/get repl :repl-type))
               (repl-env (clj-data/get repl :repl-env))
               (ns-prefix (if (equal repl-type :cljs) "cljs.core" "clojure.core"))
               (print-length (thread-first repl
                               (clj-data/get :params)
                               (clj-data/get (concat ns-prefix "/*print-length*"))))
               (print-level (thread-first repl
                              (clj-data/get :params)
                              (clj-data/get (concat ns-prefix "/*print-level*"))))
               (print-meta (thread-first repl
                             (clj-data/get :params)
                             (clj-data/get (concat ns-prefix "/*print-meta*")))))
          (clj-browse/init-browse-buffer watch-buffer)
          (replique-watch/init-watch-buffer tooling-repl repl buffer-id watch-buffer)
          (with-current-buffer watch-buffer
            (setq replique-watch/var-name var-name)
            (replique-watch/refresh t nil nil))
          (pop-to-buffer-same-window watch-buffer))))))

(defun replique-watch/on-kill-buffer ()
  (when replique-watch/directory
    (when-let ((tooling-repl (replique/repl-by :repl-type :tooling
                                               :directory replique-watch/directory)))
      (when replique-watch/var-name
        (let ((resp (replique/send-tooling-msg
                     tooling-repl
                     (clj-data/hash-map :type :remove-watch
                                        :repl-env replique-watch/repl-env
                                        :buffer-id replique-watch/buffer-id))))
          (let ((err (clj-data/get resp :error)))
            (when err
              (message "%s" (clj-pprint/pprint-error-str err))
              (error "Error while removing watcher for var: %s" replique-watch/var-name)))))
      (let ((ref-watchers (clj-data/get tooling-repl :ref-watchers)))
        (remhash replique-watch/buffer-id ref-watchers)))))

(add-hook 'kill-buffer-hook 'replique-watch/on-kill-buffer)

(defun replique-watch/do-watch-var (tooling-repl repl var-ns var-name)
  (let ((var-name (format "%s/%s" var-ns var-name))
        (buffer-id (replique-watch/new-buffer-id "var")))
    (replique-watch/do-watch tooling-repl repl buffer-id var-name)))

(defun replique-watch/watch-printed (tooling-repl repl)
  (let* ((repl-type (clj-data/get repl :repl-type))
         (directory (clj-data/get repl :directory))
         (session (clj-data/get repl :session))
         (printed-buffer-id (if (equal :cljs repl-type)
                                "var-replique.cljs-env.watch/printed"
                              (concat "printed-" session)))
         (ref-watchers (clj-data/get tooling-repl :ref-watchers))
         (existing-printed-watch-buffer (clj-data/get ref-watchers printed-buffer-id)))
    (if existing-printed-watch-buffer
        (pop-to-buffer-same-window existing-printed-watch-buffer)
      (let* ((buffer-name (if (equal :cljs repl-type)
                              "*watch*replique.cljs-env.watch/printed*"
                            (format "*watch*printed*%s*%s*%s*"
                                    (file-name-nondirectory
                                     (directory-file-name directory))
                                    (replique/keyword-to-string repl-type)
                                    session)))
             (watch-buffer (generate-new-buffer buffer-name)))
        (clj-browse/init-browse-buffer watch-buffer)
        (replique-watch/init-watch-buffer tooling-repl repl printed-buffer-id watch-buffer)
        (with-current-buffer watch-buffer
          (replique-watch/refresh t nil nil))
        (pop-to-buffer-same-window watch-buffer)))))

(defun replique-watch/watch-results (tooling-repl repl)
  (let* ((repl-type (clj-data/get repl :repl-type))
         (directory (clj-data/get repl :directory))
         (session (clj-data/get repl :session))
         (results-buffer-id (if (equal :cljs repl-type)
                                "var-replique.cljs-env.watch/results"
                              (concat "results-" session)))
         (ref-watchers (clj-data/get tooling-repl :ref-watchers))
         (existing-results-watch-buffer (clj-data/get ref-watchers results-buffer-id)))
    (if existing-results-watch-buffer
        (pop-to-buffer-same-window existing-results-watch-buffer)
      (let* ((buffer-name (if (equal :cljs repl-type)
                              "*watch*replique.cljs-env.watch/results*"
                            (format "*watch*results*%s*%s*%s*"
                                    (file-name-nondirectory
                                     (directory-file-name directory))
                                    (replique/keyword-to-string repl-type)
                                    session)))
             (watch-buffer (generate-new-buffer buffer-name)))
        (clj-browse/init-browse-buffer watch-buffer)
        (replique-watch/init-watch-buffer tooling-repl repl results-buffer-id watch-buffer)
        (with-current-buffer watch-buffer
          (replique-watch/refresh t nil nil))
        (pop-to-buffer-same-window watch-buffer)))))

(defun replique-watch/watch* (var-ns tooling-repl repl)
  (let* ((watched-data (completing-read "Watch REPL data: "
                                        '("printed" "results" "var")
                                        nil t nil nil "printed")))
    (cond ((equal watched-data "var")
           (let ((var-ns (replique-list-vars/list-namespaces tooling-repl repl var-ns)))
             (replique-list-vars/list-vars
              var-ns tooling-repl repl
              "Watch var: " 'replique-watch/do-watch-var)))
          ((equal watched-data "printed")
           (replique-watch/watch-printed tooling-repl repl))
          ((equal watched-data "results")
           (replique-watch/watch-results tooling-repl repl)))))

(defun replique-watch/watch-clj (tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (replique-watch/watch* var-ns tooling-repl clj-repl))))

(defun replique-watch/watch-cljs (tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (replique-watch/watch* var-ns tooling-repl cljs-repl))))

(defun replique-watch/watch-cljc (tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (replique-watch/watch* var-ns tooling-repl repl))))

(defun replique-watch/watch-session (repl)
  (let* ((repl-ns (clj-data/get repl :ns))
         (directory (clj-data/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling)))
    (replique-watch/watch* (symbol-name repl-ns) tooling-repl repl)))

(defun replique-watch/kill-repl-watch-buffers (repl)
  (let ((directory (clj-data/get repl :directory))
        (session (clj-data/get repl :session)))
    (when-let (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
      (let ((ref-watchers (clj-data/get tooling-repl :ref-watchers)))
        (when-let (watch-buffer (clj-data/get ref-watchers (concat "printed-" session)))
          (kill-buffer watch-buffer))
        (when-let (watch-buffer (clj-data/get ref-watchers (concat "results-" session)))
          (kill-buffer watch-buffer))
        (when-let (watch-buffer (clj-data/get
                                 ref-watchers "var-replique.cljs-env.watch/printed"))
          (kill-buffer watch-buffer))
        (when-let (watch-buffer (clj-data/get
                                 ref-watchers "var-replique.cljs-env.watch/results"))
          (kill-buffer watch-buffer))))))

(defun replique/watch ()
  (interactive)
  (replique/with-modes-dispatch
   (replique/mode . 'replique-watch/watch-session)
   (clojure-mode . 'replique-watch/watch-clj)
   (clojurescript-mode . 'replique-watch/watch-cljs)
   (clojurec-mode . 'replique-watch/watch-cljc)
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(defun replique-watch/notify-update (msg)
  (let* ((directory (clj-data/get msg :process-id))
         (tooling-repl (replique/repl-by :repl-type :tooling
                                         :directory directory)))
    (when tooling-repl
      (let* ((buffer-id (clj-data/get msg :buffer-id))
             (ref-watchers (clj-data/get tooling-repl :ref-watchers))
             (watch-buffer (clj-data/get ref-watchers buffer-id)))
        (when watch-buffer
          (with-current-buffer watch-buffer
            (set-buffer-modified-p t)))))))

(defun replique-watch/is-orphan-buffer? (tooling-repl buffer-id)
  (let ((ref-watchers (clj-data/get tooling-repl :ref-watchers)))
    (not (clj-data/contains? ref-watchers buffer-id))))

(defun replique-watch/refresh (&optional first-render? no-update? minibuffer?)
  (interactive)
  (if (not (seq-contains minor-mode-list 'replique-watch/minor-mode))
      (user-error "replique-watch/refresh can only be used in a watch buffer")
    (let* ((tooling-repl (replique/repl-by :repl-type :tooling
                                           :directory replique-watch/directory))
           (msg (clj-data/hash-map :type :refresh-watch
                                   :update? (null no-update?)
                                   :repl-env replique-watch/repl-env
                                   :buffer-id replique-watch/buffer-id
                                   :print-length replique-watch/print-length
                                   :print-level replique-watch/print-level
                                   :print-meta replique-watch/print-meta
                                   :browse-path `(quote ,clj-browse/browse-path)))
           (msg (if replique-watch/var-name
                    (clj-data/assoc msg :var-sym
                                    (make-symbol (format "'%s" replique-watch/var-name)))
                  msg)))
      (when tooling-repl
        (let ((resp (replique/send-tooling-msg tooling-repl msg)))
          (let ((err (clj-data/get resp :error)))
            (if err
                (progn
                  (message "%s" (clj-pprint/pprint-error-str err))
                  (cond ((null replique-watch/var-name)
                         (message "refresh watch failed"))
                        ((clj-data/get resp :undefined)
                         (message "%s is undefined" replique-watch/var-name))
                        (t (message "refresh watch failed with var %s"
                                    replique-watch/var-name))))
              ;; Ensure the buffer is not an orphan one
              (let ((ref-watchers (clj-data/get tooling-repl :ref-watchers)))
                (puthash replique-watch/buffer-id (current-buffer) ref-watchers))
              (when (not first-render?)
                (message "Refreshing ..."))
              (let ((resp (clj-data/get resp :refresh-watch)))
                (setq replique-watch/record-size (clj-data/get resp :record-size))
                (clj-browse/pprint (clj-data/get resp :var-value))
                (when (not first-render?)
                  (if minibuffer?
                      (minibuffer-message "Refreshing ... done")
                    (message "Refreshing ... done")))))))))))

(defun replique-watch/browse-done (candidate)
  (if (equal clj-browse/no-candidate candidate)
      (progn
        (setq clj-browse/browse-path clj-browse/temporary-browse-path)
        (replique-watch/refresh nil t nil)
        (goto-char (point-min)))
    (let* ((tooling-repl (replique/repl-by :repl-type :tooling
                                           :directory replique-watch/directory))
           (repl-env replique-watch/repl-env)
           (resp (replique/send-tooling-msg
                  tooling-repl
                  (clj-data/hash-map :type :can-browse?
                                     :repl-env repl-env
                                     :candidate candidate))))
      (let ((err (clj-data/get resp :error)))
        (if err
            (progn
              (message "%s" (clj-pprint/pprint-str err))
              (message "can-browse? failed with candidate: %s"
                       (clj-print/print-str candidate)))
          (if (clj-data/get resp :can-browse?)
              (let* ((browse-index (clj-data/get clj-browse/candidate->index candidate))
                     (candidate (propertize candidate 'replique-watch/browse-index browse-index)))
                (setq clj-browse/browse-path (cons candidate clj-browse/temporary-browse-path))
                (replique-watch/refresh nil t nil)
                (goto-char (point-min)))
            (message "Cannot browse the selected path")))))))

(defun replique-watch/browse-backward-delete-char ()
  (interactive)
  (if (equal "" ivy-text)
      (let ((new-browse-path (cdr clj-browse/temporary-browse-path)))
        (ivy-quit-and-run
          (let ((clj-browse/temporary-browse-path new-browse-path))
            (clj-browse/browse* 'replique-watch/browse-candidates*
                                'replique-watch/browse-done
                                replique-watch/browse-map))))
    (ivy-backward-delete-char)))

(defun replique-watch/browse-alt-done ()
  (interactive)
  (when-let (candidate (nth ivy--index ivy--all-candidates))
    (when (not (equal clj-browse/no-candidate candidate))
      (let* ((tooling-repl (with-ivy-window
                             (replique/repl-by :repl-type :tooling
                                               :directory replique-watch/directory)))
             (repl-env (with-ivy-window replique-watch/repl-env))
             (resp (replique/send-tooling-msg
                    tooling-repl
                    (clj-data/hash-map :type :can-browse?
                                       :repl-env repl-env
                                       :candidate candidate))))
        (let ((err (clj-data/get resp :error)))
          (if err
              (progn
                (message "%s" (clj-pprint/pprint-str err))
                (message "can-browse? failed with candidate: %s"
                         (clj-print/print-str candidate))
                (sit-for 0.5))
            (if (clj-data/get resp :can-browse?)
                (progn
                  (let* ((browse-index (clj-data/get clj-browse/candidate->index candidate))
                         (candidate (propertize candidate 'replique-watch/browse-index browse-index))
                         (new-browse-path (cons candidate clj-browse/temporary-browse-path)))
                    (ivy-quit-and-run
                      (let ((clj-browse/temporary-browse-path new-browse-path))
                        (clj-browse/browse* 'replique-watch/browse-candidates*
                                            'replique-watch/browse-done
                                            replique-watch/browse-map)))))
              (message "Cannot browse the selected path")
              (sit-for 0.5))))))))

(defvar replique-watch/browse-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") 'replique-watch/browse-backward-delete-char)
    (define-key map (kbd "C-j") 'replique-watch/browse-alt-done)
    map))

(defun replique-watch/browse-candidates* (user-input)
  (let* ((tooling-repl (replique/repl-by :repl-type :tooling
                                         :directory replique-watch/directory))
         (resp (replique/send-tooling-msg
                tooling-repl
                (clj-data/hash-map :type :browse-candidates
                                   :repl-env replique-watch/repl-env
                                   :var-sym (make-symbol (format "'%s" replique-watch/var-name))
                                   :buffer-id replique-watch/buffer-id
                                   :browse-path `(quote ,clj-browse/temporary-browse-path)
                                   :prefix user-input
                                   :print-meta replique-watch/print-meta))))
    (let ((err (clj-data/get resp :error)))
      (if err
          (if (clj-data/get resp :undefined)
              (error "%s is undefined" replique-watch/var-name)
            (message "%s" (clj-pprint/pprint-error-str err))
            (error "Browse failed while requesting browse candidates"))
        (clj-data/get resp :candidates)))))

(defun replique-watch/read-one ()
  (let ((clj-context/splice-ends '())
        (clj-context/symbol-separators clj-pprint/symbol-separators)
        (clj-context/symbol-separator-re clj-pprint/symbol-separator-re))
    (clj-context/read-one)))

(defun replique-watch/compute-init-candidate-sequential (target-point seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while continue
      (clj-context/forward-comment)
      (let* ((object-start (point))
             (object (replique-watch/read-one)))
        (cond ((null object)
               (setq index (max 0 (- index 1)))
               (setq continue nil))
              ((>= (point) target-point)
               (setq continue nil))
              (t (setq index (+ 1 index))))))
    (number-to-string index)))

(defun replique-watch/compute-init-candidate-map (target-point map)
  (let ((continue t)
        (candidate nil)
        (index 0))
    (goto-char (+ 1 (oref map :start)))
    (while continue
      (clj-context/forward-comment)
      (let* ((object-start (point))
             (object (replique-watch/read-one)))
        (cond ((null object)
               (setq continue nil))
              ((>= (point) target-point)
               (when (equal 0 (logand index 1))
                 (setq candidate (buffer-substring-no-properties object-start (point))))
               (setq continue nil))
              (t (when (equal 0 (logand index 1))
                   (setq candidate (buffer-substring-no-properties object-start (point))))))
        (setq index (+ 1 index))))
    candidate))

(defun replique-watch/compute-init-candidate-dispatch-macro (target-point dm)
  (let ((dm-type (oref dm :dispatch-macro))
        (dm-value (clj-context/meta-value (oref dm :value))))
    (cond ((and (eq :set dm-type)
                (cl-typep dm-value 'clj-context/object-delimited))
           (replique-watch/compute-init-candidate-map target-point dm-value))
          ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                (cl-typep dm-value 'clj-context/object-delimited))
           (if (equal :map (oref dm-value :delimited))
               (replique-watch/compute-init-candidate-map target-point dm-value)
             (replique-watch/compute-init-candidate-sequential target-point dm-value))))))

(defun replique-watch/compute-init-candidate-dispatch (target-point)
  (let* ((object (replique-watch/read-one))
         (object-meta-value (clj-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'clj-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (replique-watch/compute-init-candidate-map target-point object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-delimited)
             (replique-watch/compute-init-candidate-sequential target-point
                                                               object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-dispatch-macro)
             (replique-watch/compute-init-candidate-dispatch-macro
              target-point object-meta-value))))))

(defun replique-watch/compute-init-candidate ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((target-point (point)))
        (goto-char (point-min))
        (replique-watch/compute-init-candidate-dispatch target-point)))))

(defun replique-watch/browse ()
  (interactive)
  (if (not (bound-and-true-p replique-watch/minor-mode))
      (user-error "replique-watch/browse can only be used from a watch buffer")
    (when-let (tooling-repl (replique/repl-by :repl-type :tooling
                                              :directory replique-watch/directory))
      (if (replique-watch/is-orphan-buffer? tooling-repl replique-watch/buffer-id)
          (message "The buffer must be refreshed")
        (let ((clj-browse/temporary-browse-path clj-browse/browse-path)
              (init-candidate (replique-watch/compute-init-candidate)))
          (when tooling-repl
            (clj-browse/browse* 'replique-watch/browse-candidates*
                                'replique-watch/browse-done
                                replique-watch/browse-map
                                init-candidate)))))))

(defun replique-watch/set-param-watch (tooling-repl param param-value)
  (cond ((equal "*print-length*" param)
         (setq replique-watch/print-length (when (not (equal "nil" param-value))
                                             (string-to-number param-value))))
        ((equal "*print-level*" param)
         (setq replique-watch/print-level (when (not (equal "nil" param-value))
                                            (string-to-number param-value))))
        ((equal "*print-meta*" param)
         (setq replique-watch/print-meta (if (equal "true" param-value)
                                             t
                                           nil))))
  (replique-watch/refresh t))

(defun replique-watch/params-watch ()
  (let ((tooling-repl (replique/repl-by :repl-type :tooling
                                        :directory replique-watch/directory))
        (ns-prefix (if (equal replique-watch/repl-type :replique/cljs)
                       "cljs.core"
                     "clojure.core")))
    (when tooling-repl
      (replique-params/params* (clj-data/hash-map
                                (concat ns-prefix "/*print-length*") replique-watch/print-length
                                (concat ns-prefix "/*print-level*") replique-watch/print-level
                                (concat ns-prefix "/*print-meta*") replique-watch/print-meta)
                               (apply-partially 'replique-watch/set-param-watch tooling-repl)))))

(defvar replique-watch/record-history nil)

(defun replique-watch/record ()
  (interactive)
  (if (not (bound-and-true-p replique-watch/minor-mode))
      (user-error "replique-watch/record can only be used from a watch buffer")
    (when-let (tooling-repl (replique/repl-by :repl-type :tooling
                                              :directory replique-watch/directory))
      (if (replique-watch/is-orphan-buffer? tooling-repl replique-watch/buffer-id)
          (message "The buffer must be refreshed")
        (replique-params/edit-numerical
         (propertize "Recording size"
                     'replique-params/history 'replique-watch/record-history
                     'replique-params/default-val replique-watch/record-size)
         (lambda (param value)
           (let* ((record-size (when (not (equal "nil" value))
                                 (string-to-number value)))
                  (resp (replique/send-tooling-msg
                         tooling-repl
                         (clj-data/hash-map :type (if (and record-size (> record-size 1))
                                                      :start-recording :stop-recording)
                                            :repl-env replique-watch/repl-env
                                            :var-sym (make-symbol
                                                      (format "'%s" replique-watch/var-name))
                                            :buffer-id replique-watch/buffer-id
                                            :record-size record-size))))
             (let ((err (clj-data/get resp :error)))
               (if err
                   (if (clj-data/get resp :undefined)
                       (message "%s is undefined" replique-watch/var-name)
                     (message "%s" (clj-pprint/pprint-error-str err))
                     (message "%s failed with var sym: %s"
                              (if (and record-size (> record-size 1))
                                  "start-recording" "stop-recording")
                              (make-symbol (format "'%s" replique-watch/var-name))))
                 (setq replique-watch/record-size record-size)
                 (message (if (and record-size (> record-size 1))
                              (format "Recording started. Recording size: %s" record-size)
                            "Recording stopped")))))))))))


(defvar replique-watch/minibuffer-map-record-menu
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "p" 'replique-watch/record-previous)
    (define-key map "n" 'replique-watch/record-next)
    map))

(defvar-local replique-watch/record-index nil)
(defvar-local replique-watch/record-count nil)
(defvar-local replique-watch/record-refresh-timer nil)

(defun replique-watch/record-position-refresh (watch-buffer)
  (when (buffer-live-p watch-buffer)
    (let ((minibuffer-buffer (current-buffer)))
      (with-current-buffer watch-buffer
        (replique-watch/refresh nil t t)))))

(defun replique-watch/record-update-index (minibuffer-buffer new-index)
  (with-selected-window (minibuffer-selected-window)
    (when-let (tooling-repl (replique/repl-by :repl-type :tooling
                                              :directory replique-watch/directory))
      (let ((resp (replique/send-tooling-msg
                   tooling-repl
                   (clj-data/hash-map :type :set-record-position
                                      :repl-env replique-watch/repl-env
                                      :var-sym (make-symbol
                                                (format "'%s" replique-watch/var-name))
                                      :buffer-id replique-watch/buffer-id
                                      :index (- new-index 1)))))
        (let ((err (clj-data/get resp :error)))
          (if err
              (if (clj-data/get resp :undefined)
                  (error "%s is undefined" replique-watch/var-name)
                (message "%s" (clj-pprint/pprint-error-str err))
                (error "record-next failed with var sym: %s"
                       (make-symbol (format "'%s" replique-watch/var-name))))
            (let ((record-position (clj-data/get resp :record-position)))
              (when replique-watch/record-refresh-timer
                (cancel-timer replique-watch/record-refresh-timer))
              (setq replique-watch/record-refresh-timer
                    (run-at-time 0.5 nil
                                 'replique-watch/record-position-refresh
                                 (current-buffer)))
              (with-current-buffer minibuffer-buffer
                (setq replique-watch/record-index (clj-data/get record-position :index))
                (setq replique-watch/record-count (clj-data/get record-position :count))
                (let ((inhibit-read-only t))
                  (delete-minibuffer-contents)
                  (insert (format "%s/%s "
                                  replique-watch/record-index
                                  replique-watch/record-count)))))))))))

(defun replique-watch/record-previous ()
  (interactive)
  (when (> replique-watch/record-index 1)
    (let ((new-index (- replique-watch/record-index 1))
          (minibuffer-buffer (current-buffer)))
      (replique-watch/record-update-index minibuffer-buffer new-index))))

(defun replique-watch/record-next ()
  (interactive)
  (when (< replique-watch/record-index replique-watch/record-count)
    (let ((new-index (+ 1 replique-watch/record-index))
          (minibuffer-buffer (current-buffer)))
      (replique-watch/record-update-index minibuffer-buffer new-index))))

(defun replique-watch/record-menu ()
  (interactive)
  (if (not (bound-and-true-p replique-watch/minor-mode))
      (user-error "replique-watch/record-menu can only be used from a watch buffer")
    (when-let (tooling-repl (replique/repl-by :repl-type :tooling
                                              :directory replique-watch/directory))
      (if (replique-watch/is-orphan-buffer? tooling-repl replique-watch/buffer-id)
          (message "The buffer must be refreshed")
        (let ((resp (replique/send-tooling-msg
                     tooling-repl
                     (clj-data/hash-map :type :record-position
                                        :repl-env replique-watch/repl-env
                                        :var-sym (make-symbol
                                                  (format "'%s" replique-watch/var-name))
                                        :buffer-id replique-watch/buffer-id))))
          (let ((err (clj-data/get resp :error)))
            (if err
                (if (clj-data/get resp :undefined)
                    (message "%s is undefined" replique-watch/var-name)
                  (message "%s" (clj-pprint/pprint-error-str err))
                  (message "record-menu failed with var sym: %s"
                           (make-symbol (format "'%s" replique-watch/var-name))))
              (let* ((record-position (clj-data/get resp :record-position))
                     (index (clj-data/get record-position :index))
                     (count (clj-data/get record-position :count))
                     (minibuffer-setup-hook (lambda ()
                                              (setq buffer-read-only t)
                                              (setq replique-watch/record-index index)
                                              (setq replique-watch/record-count count))))
                (read-from-minibuffer "Record position: "
                                      (format "%s/%s " index count)
                                      replique-watch/minibuffer-map-record-menu)))))))))

(defun replique-watch/copy-browse-path ()
  (interactive)
  (let* ((browse-path (mapcar 'substring-no-properties clj-browse/browse-path))
         (browse-path (concat "[" (string-join browse-path " ") "]")))
    (kill-new browse-path)
    (message "Browse path has been copied to clipboard: %s" browse-path)))

(defvar replique-watch/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'replique-watch/refresh)
    (define-key map "b" 'replique-watch/browse)
    (define-key map "r" 'replique-watch/record)
    (define-key map "p" 'replique-watch/record-menu)
    (define-key map "n" 'replique-watch/record-menu)
    (easy-menu-define replique-watch/minor-mode-menu map
      "Replique-watch Minor Mode Menu"
      '("Replique-watch"
        ["Refresh watch" replique-watch/refresh t]
        ["Browse" replique-watch/browse t]
        ["Record" replique-watch/record t]
        ["Record menu" replique-watch/record-menu t]
        ["Record menu" replique-watch/record-menu t]))
    map))

(define-minor-mode replique-watch/minor-mode
  "Minor mode for interacting with a replique watch buffer."
  :lighter "Replique-watch" :keymap replique-watch/minor-mode-map)

(provide 'replique-watch)
