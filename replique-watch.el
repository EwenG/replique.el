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
(require 'replique-context)
(require 'replique-pprint)

(defvar-local replique-watch/directory nil)
(defvar-local replique-watch/repl-env nil)
(defvar-local replique-watch/repl-type nil)
(defvar-local replique-watch/var-name nil)
(defvar-local replique-watch/buffer-id nil)
(defvar-local replique-watch/print-length nil)
(defvar-local replique-watch/print-level nil)
(defvar-local replique-watch/print-meta nil)
(defvar-local replique-watch/browse-path nil)
(defvar buffer-id-generator 0)

(defvar replique-watch/no-candidate "")

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
    (let* ((p-min (point-min))
           (p-max (point-max))
           (printed-raw (buffer-substring-no-properties p-min p-max)))
      (replique-pprint/pprint)
      (put-text-property p-min (max p-min (+ 1 p-min))
                         'replique-watch/raw-printed
                         printed-raw))
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
                              (replique/get (concat ns-prefix "/*print-level*"))))
               (print-meta (thread-first repl
                             (replique/get :params)
                             (replique/get (concat ns-prefix "/*print-meta*")))))
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
            (setq replique-watch/print-meta print-meta)
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
                                          :print-meta print-meta
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
                                          :print-meta replique-watch/print-meta
                                          :browse-path `(quote ,replique-watch/browse-path)))))
            (let ((err (replique/get resp :error)))
              (if err
                  (if (replique/get resp :undefined)
                      (message "%s is undefined" replique-watch/var-name)
                    (message "%s" (replique-pprint/pprint-error-str err))
                    (message "refresh watch failed with var %s" replique-watch/var-name))
                (message "Refreshing ...")
                (replique-watch/pprint (replique/get resp :var-value))
                (message "Refreshing ... done")))))))))

(defun replique-watch/do-browse (candidate)
  (if (equal replique-watch/no-candidate candidate)
      (progn
        (setq replique-watch/browse-path replique-watch/temporary-browse-path)
        (replique-watch/refresh t)
        (goto-char (point-min)))
    (let* ((tooling-repl (replique/repl-by :repl-type :tooling
                                           :directory replique-watch/directory))
           (repl-env replique-watch/repl-env)
           (resp (replique/send-tooling-msg
                  tooling-repl
                  (replique/hash-map :type :can-browse?
                                     :repl-env repl-env
                                     :candidate candidate))))
      (let ((err (replique/get resp :error)))
        (if err
            (progn
              (message "%s" (replique-pprint/pprint-str err))
              (message "can-browse? failed with candidate: %s"
                       (replique-print/print-str candidate)))
          (if (replique/get resp :can-browse?)
              (let* ((browse-index (replique/get replique-watch/candidate->index candidate))
                     (candidate (propertize candidate 'replique-watch/browse-index browse-index)))
                (setq replique-watch/browse-path
                      (cons candidate replique-watch/temporary-browse-path))
                (replique-watch/refresh t)
                (goto-char (point-min)))
            (message "Cannot browse the selected path")))))))

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
    (when (not (equal replique-watch/no-candidate candidate))
      (let* ((tooling-repl (with-ivy-window
                             (replique/repl-by :repl-type :tooling
                                               :directory replique-watch/directory)))
             (repl-env (with-ivy-window replique-watch/repl-env))
             (resp (replique/send-tooling-msg
                    tooling-repl
                    (replique/hash-map :type :can-browse?
                                       :repl-env repl-env
                                       :candidate candidate))))
        (let ((err (replique/get resp :error)))
          (if err
              (progn
                (message "%s" (replique-pprint/pprint-str err))
                (message "can-browse? failed with candidate: %s"
                         (replique-print/print-str candidate))
                (sit-for 0.5))
            (if (replique/get resp :can-browse?)
                (let* ((browse-index (replique/get replique-watch/candidate->index candidate))
                       (candidate (propertize candidate 'replique-watch/browse-index browse-index))
                       (new-browse-path (cons candidate replique-watch/temporary-browse-path)))
                  (ivy-quit-and-run
                    (let ((replique-watch/temporary-browse-path new-browse-path))
                      (replique-watch/browse*))))
              (message "Cannot browse the selected path")
              (sit-for 0.5))))))))

(defvar replique-watch/browse-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") 'replique-watch/browse-backward-delete-char)
    (define-key map (kbd "C-j") 'replique-watch/browse-alt-done)
    map))

(defun replique-watch/browse-path->string (browse-path)
  (if (eq '() browse-path)
      ""
    (concat (string-join (reverse browse-path) " ") " ")))

(defun replique-watch/browse-candidates* (user-input)
  (let* ((tooling-repl (replique/repl-by :repl-type :tooling
                                         :directory replique-watch/directory))
         (resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :browse-candidates
                                   :repl-env replique-watch/repl-env
                                   :var-sym (make-symbol (format "'%s" replique-watch/var-name))
                                   :buffer-id replique-watch/buffer-id
                                   :browse-path `(quote ,replique-watch/temporary-browse-path)
                                   :prefix user-input
                                   :print-meta replique-watch/print-meta))))
    (let ((err (replique/get resp :error)))
      (if err
          (if (replique/get resp :undefined)
              (error "%s is undefined" replique-watch/var-name)
            (message "%s" (replique-pprint/pprint-error-str err))
            (error "Browse failed while requesting browse candidates"))
        (replique/get resp :candidates)))))

(defun replique-watch/browse-candidates (user-input)
  (with-ivy-window
    (replique-watch/browse-candidates* user-input)))

(defun replique-watch/read-one ()
  (let ((replique-context/splice-ends '())
        (replique-context/symbol-separators replique-pprint/symbol-separators)
        (replique-context/symbol-separator-re replique-pprint/symbol-separator-re))
    (replique-context/read-one)))

(defun replique-watch/compute-browse-positions-sequential (index->pos seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while continue
      (replique-context/forward-comment)
      (let ((p-start (point))
            (object (replique-watch/read-one)))
        (if (null object)
            (setq continue nil)
          (puthash index p-start index->pos)
          (setq index (+ 1 index)))))
    index->pos))

(defun replique-watch/compute-browse-positions-map (index->pos map)
   (let ((continue t)
         (index 0))
     (goto-char (+ 1 (oref map :start)))
     (while continue
       (replique-context/forward-comment)
       (let ((p-start (point))
             (object (replique-watch/read-one)))
         (if (null object)
             (setq continue nil)
           (when (equal 0 (logand index 1))
             (puthash index p-start index->pos))
           (setq index (+ 1 index)))))
     index->pos))

(defun replique-watch/compute-browse-positions-dispatch-macro (index->pos dm)
   (let ((dm-type (oref dm :dispatch-macro))
         (dm-value (replique-context/meta-value (oref dm :value))))
     (cond ((and (eq :set dm-type)
                 (cl-typep dm-value 'replique-context/object-delimited))
            (replique-watch/compute-browse-positions-sequential index->pos dm-value))
           ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                 (cl-typep dm-value 'replique-context/object-delimited))
            (if (equal :map (oref dm-value :delimited))
                (replique-watch/compute-browse-positions-map index->pos dm-value)
              (replique-watch/compute-browse-positions-sequential index->pos dm-value))))
     index->pos))

(defun replique-watch/compute-browse-positions-dispatch (index->pos)
  (let* ((object (replique-watch/read-one))
         (object-meta-value (replique-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'replique-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (replique-watch/compute-browse-positions-map index->pos object-meta-value))
            ((cl-typep object-meta-value 'replique-context/object-delimited)
             (replique-watch/compute-browse-positions-sequential index->pos object-meta-value))
            ((cl-typep object-meta-value 'replique-context/object-dispatch-macro)
             (replique-watch/compute-browse-positions-dispatch-macro
              index->pos object-meta-value)))))
  index->pos)

(defun replique-watch/goto-indexes-path-position-sequential (target-index seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while (and (< index target-index) continue)
      (replique-context/forward-comment)
      (let ((object (replique-watch/read-one)))
        (if (null object)
            (setq continue nil)
          (setq index (+ 1 index)))))
    (>= index target-index)))

(defun replique-watch/goto-indexes-path-position-map (target-index seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while (and (<= index target-index) continue)
      (replique-context/forward-comment)
      (let ((object (replique-watch/read-one)))
        (if (null object)
            (setq continue nil)
          (setq index (+ 1 index)))))
    (> index target-index)))

(defun replique-watch/goto-indexes-path-position-dispatch-macro (target-index dm)
  (let ((dm-type (oref dm :dispatch-macro))
        (dm-value (replique-context/meta-value (oref dm :value))))
    (cond ((and (eq :set dm-type)
                (cl-typep dm-value 'replique-context/object-delimited))
           (replique-watch/goto-indexes-path-position-sequential target-index dm-value))
          ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                (cl-typep dm-value 'replique-context/object-delimited))
           (if (equal :map (oref dm-value :delimited))
               (replique-watch/goto-indexes-path-position-map target-index dm-value)
             (replique-watch/goto-indexes-path-position-sequential target-index dm-value))))))

(defun replique-watch/goto-indexes-path-position-dispatch (index)
  (let* ((object (replique-watch/read-one))
         (object-meta-value (replique-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'replique-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (replique-watch/goto-indexes-path-position-map index object-meta-value))
            ((cl-typep object-meta-value 'replique-context/object-delimited)
             (replique-watch/goto-indexes-path-position-sequential index object-meta-value))
            ((cl-typep object-meta-value 'replique-context/object-dispatch-macro)
             (replique-watch/goto-indexes-path-position-dispatch-macro
              index object-meta-value))))))

(defun replique-watch/compute-browse-positions (indexes-path)
  (let ((index->pos (replique/hash-map))
        (at-position? t))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and at-position? (consp indexes-path))
          (if (null (car indexes-path))
              (setq at-position? nil)
            (replique-context/forward-comment)
            (setq at-position?
                  (replique-watch/goto-indexes-path-position-dispatch (car indexes-path)))
            (setq indexes-path (cdr indexes-path))))
        (when at-position?
          (replique-context/forward-comment)
          (puthash -1 (point) index->pos)
          (replique-watch/compute-browse-positions-dispatch index->pos))))
    index->pos))

(defun replique-watch/compute-browse-indexes-sequential (candidate->index seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while continue
      (replique-context/forward-comment)
      (let* ((object (replique-watch/read-one)))
        (if (null object)
            (setq continue nil)
          (puthash index index candidate->index)
          (setq index (+ 1 index)))))
    candidate->index))

(defun replique-watch/compute-browse-indexes-map (candidate->index map)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref map :start)))
    (while continue
      (replique-context/forward-comment)
      (let* ((object-start (point))
             (object (replique-watch/read-one)))
        (if (null object)
            (setq continue nil)
          (when (equal 0 (logand index 1))
            (let ((k-str (buffer-substring-no-properties object-start (point))))
              ;; If there are multiple identical keys in a map, then we cannot
              ;; distinguish between them
              (if (replique/contains? candidate->index k-str)
                  (remhash k-str candidate->index)
                (puthash k-str index candidate->index))))
          (setq index (+ 1 index)))))
    candidate->index))

(defun replique-watch/compute-browse-indexes-dispatch-macro (candidate->index dm)
  (let ((dm-type (oref dm :dispatch-macro))
        (dm-value (replique-context/meta-value (oref dm :value))))
    (cond ((and (eq :set dm-type)
                (cl-typep dm-value 'replique-context/object-delimited))
           (replique-watch/compute-browse-indexes-sequential candidate->index dm-value))
          ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                (cl-typep dm-value 'replique-context/object-delimited))
           (if (equal :map (oref dm-value :delimited))
               (replique-watch/compute-browse-indexes-map candidate->index dm-value)
             (replique-watch/compute-browse-indexes-sequential candidate->index dm-value))))
    candidate->index))

(defun replique-watch/compute-browse-indexes-dispatch (candidate->index)
  (let* ((object (replique-watch/read-one))
         (object-meta-value (replique-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'replique-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (replique-watch/compute-browse-indexes-map candidate->index object-meta-value))
            ((cl-typep object-meta-value 'replique-context/object-delimited)
             (replique-watch/compute-browse-indexes-sequential candidate->index
                                                               object-meta-value))
            ((cl-typep object-meta-value 'replique-context/object-dispatch-macro)
             (replique-watch/compute-browse-indexes-dispatch-macro
              candidate->index object-meta-value)))))
  candidate->index)

(defun replique-watch/compute-browse-indexes (indexes-path)
  (let ((candidate->index (replique/hash-map))
        (at-position? t))
    (goto-char (point-min))
    (while (and at-position? (consp indexes-path))
      (if (null (car indexes-path))
          (setq at-position? nil)
        (replique-context/forward-comment)
        (setq at-position?
              (replique-watch/goto-indexes-path-position-dispatch (car indexes-path)))
        (setq indexes-path (cdr indexes-path))))
    (when at-position?
      (replique-context/forward-comment)
      (replique-watch/compute-browse-indexes-dispatch candidate->index))
    candidate->index))

(defun replique-watch/compute-init-candidate-sequential (target-point seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while continue
      (replique-context/forward-comment)
      (let* ((object-start (point))
             (object (replique-watch/read-one)))
        (cond ((null object)
               (setq index (max 0 (- index 1)))
               (setq continue nil))
              ((>= (point) target-point)
               (setq continue nil))
              (t (setq index (+ 1 index))))))
    index))

(defun replique-watch/compute-init-candidate-map (target-point map)
  (let ((continue t)
        (candidate nil)
        (index 0))
    (goto-char (+ 1 (oref map :start)))
    (while continue
      (replique-context/forward-comment)
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
        (dm-value (replique-context/meta-value (oref dm :value))))
    (cond ((and (eq :set dm-type)
                (cl-typep dm-value 'replique-context/object-delimited))
           (replique-watch/compute-init-candidate-map target-point dm-value))
          ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                (cl-typep dm-value 'replique-context/object-delimited))
           (if (equal :map (oref dm-value :delimited))
               (replique-watch/compute-init-candidate-map target-point dm-value)
             (replique-watch/compute-init-candidate-sequential target-point dm-value))))))

(defun replique-watch/compute-init-candidate-dispatch (target-point)
  (let* ((object (replique-watch/read-one))
         (object-meta-value (replique-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'replique-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (replique-watch/compute-init-candidate-map target-point object-meta-value))
            ((cl-typep object-meta-value 'replique-context/object-delimited)
             (replique-watch/compute-init-candidate-sequential target-point
                                                               object-meta-value))
            ((cl-typep object-meta-value 'replique-context/object-dispatch-macro)
             (replique-watch/compute-browse-indexes-dispatch-macro
              target-point object-meta-value))))))

(defun replique-watch/compute-init-candidate ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((target-point (point)))
        (goto-char (point-min))
        (replique-watch/compute-init-candidate-dispatch target-point)))))

(defun replique-watch/compute-path-indexes ()
  (let ((browse-path (reverse replique-watch/browse-path))
        (temporary-browse-path (reverse replique-watch/temporary-browse-path)))
    (while (and
            (car browse-path)
            (car temporary-browse-path)
            (equal (car browse-path) (car temporary-browse-path)))
      (setq browse-path (cdr browse-path))
      (setq temporary-browse-path (cdr temporary-browse-path)))
    (if (null (car browse-path))
        (mapcar (apply-partially 'get-text-property 0 'replique-watch/browse-index)
                temporary-browse-path)
      :negative-path)))

(defvar replique-watch/temporary-browse-path nil)
(defvar replique-watch/candidate->index nil)
(defvar replique-watch/index->pos nil)

(defun replique-watch/browse* (&optional init-candidate)
  (let* ((printed-raw (get-text-property (point-min) 'replique-watch/raw-printed))
         (path-indexes (replique-watch/compute-path-indexes))
         (replique-watch/candidate->index (when (not (equal :negative-path path-indexes))
                                            (with-temp-buffer
                                              (insert printed-raw)
                                              (replique-watch/compute-browse-indexes
                                               path-indexes))))
         (replique-watch/index->pos (when (not (equal :negative-path path-indexes))
                                      (replique-watch/compute-browse-positions path-indexes)))
         ;; for whatever reason :preset with :dynamic-collection does not work when :preselect
         ;; is a string, but its works if it is a number
         ;; Thus we start be prefectching the candidates to find the index of the init-candidate
         (initial-candidates (replique-watch/browse-candidates* ""))
         (preselect (when init-candidate (seq-position initial-candidates init-candidate))))
    (ivy-read
     (concat "Browse path: "
             (replique-watch/browse-path->string
              replique-watch/temporary-browse-path))
     'replique-watch/browse-candidates
     :dynamic-collection t
     :action 'replique-watch/do-browse
     :preselect preselect
     :update-fn (lambda ()
                  (with-ivy-window
                    (replique-highlight/unhighlight)
                    (when-let ((candidate (nth ivy--index ivy--all-candidates)))
                      (if (equal replique-watch/no-candidate candidate)
                          (when-let (pos (replique/get replique-watch/index->pos -1))
                            (goto-char pos)
                            (replique-highlight/highlight-no-line
                             pos (min (point-max) (+ 1 pos))))
                        (when-let (index (replique/get replique-watch/candidate->index
                                                       candidate))
                          (when-let (pos (replique/get replique-watch/index->pos index))
                            (goto-char pos)
                            (replique-highlight/highlight-no-line
                             pos (min (point-max) (+ 1 pos)))))))))
     :require-match t
     :keymap replique-watch/browse-map
     :caller 'replique-watch/browse
     :unwind (lambda () (replique-highlight/unhighlight)))))

(defun replique-watch/browse ()
  (interactive)
  (cond ((not (featurep 'ivy))
         (user-error "replique-watch/browse requires ivy-mode"))
        ((not (bound-and-true-p replique-watch/minor-mode))
         (user-error "replique-watch/browse can only be used from a watch buffer"))
        (t (let ((replique-watch/temporary-browse-path replique-watch/browse-path)
                 (init-candidate (replique-watch/compute-init-candidate)))
             (replique-watch/browse* init-candidate)))))

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
