;; replique-remove-var.el ---   -*- lexical-binding: t; -*-

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

(require 'replique-hashmap)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defvar-local replique-remove-var-line-overlay nil)

(defun replique/remove-var-make-overlay ()
  (let ((ol (make-overlay (point) (point))))
    (overlay-put ol 'priority -50)           ;(bug#16192)
    (overlay-put ol 'face '((t :inherit highlight)))
    ol))

(defun replique/remove-var-move-overlay (overlay)
  (let (tmp b e)
    (setq tmp t
          b (line-beginning-position)
          e (line-beginning-position 2))
    (if tmp
        (move-overlay overlay b e)
      (move-overlay overlay 1 1))))

(defun replique/remove-var-highlight ()
  (progn
    (unless replique-remove-var-line-overlay
      (setq replique-remove-var-line-overlay (replique/remove-var-make-overlay)))
    (overlay-put replique-remove-var-line-overlay
                 'window (selected-window))
    (replique/remove-var-move-overlay replique-remove-var-line-overlay)))

(defun replique/remove-var-unhighlight ()
  (when replique-remove-var-line-overlay
    (delete-overlay replique-remove-var-line-overlay)))

(defun replique/do-remove-var (repl var-ns var-name)
  (replique/send-input-from-source-dispatch
   (format "(replique.interactive/remove-var %s/%s)" var-ns var-name)))

(defun replique/remove-var-candidate-metas (candidates var-name)
  (thread-first (seq-find (lambda (cand)
                            (string= (symbol-name (aref cand 0)) var-name))
                          candidates)
    (aref 1)))

(defun replique/remove-var* (var-ns tooling-repl repl)
  (let ((tooling-chan (replique/get tooling-repl :chan)))
    (replique/send-tooling-msg
     tooling-repl (replique/hash-map :type :list-vars
                                     :repl-env (replique/get repl :repl-env)
                                     :ns var-ns))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (replique/get resp :error)))
           (if err
               (progn
                 (message "%s" (replique-edn/pr-str err))
                 (message "list-vars failed with ns: %s" var-ns))
             (let* ((vars (replique/get resp :vars))
                    (var-names (mapcar (lambda (var-arr) (aref var-arr 0)) vars)))
               (when (> (length var-names) 0)
                 (replique/return-nil-on-quit
                  (ivy-read
                   "Remove var: "
                   var-names
                   :require-match t
                   :action (apply-partially 'replique/do-remove-var repl var-ns)
                   :update-fn (lambda ()
                                (with-ivy-window
                                  (replique/remove-var-unhighlight)
                                  (let ((candidate (nth ivy--index ivy--old-cands)))
                                    (when candidate
                                      (let* ((metas (replique/remove-var-candidate-metas
                                                     vars candidate))
                                             (file (replique/get metas :file))
                                             (line (replique/get metas :line))
                                             (column (replique/get metas :column)))
                                        (when (and file line)
                                          (when-let (buff (replique-resources/find-file file))
                                            (pop-to-buffer-same-window buff)
                                            (goto-char (point-min))
                                            (forward-line (1- line))
                                            (when column
                                              (move-to-column column))
                                            (replique/remove-var-highlight))))))))))
                 (replique/remove-var-unhighlight))))))))))

(defun replique/remove-var-clj (tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (when var-ns
        (replique/remove-var* var-ns tooling-repl clj-repl)))))

(defun replique/remove-var-cljs (tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (when var-ns
        (replique/remove-var* var-ns tooling-repl cljs-repl)))))

(defun replique/remove-var-cljc (tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((var-ns (replique-context/clojure-find-ns)))
      (when var-ns
        (replique/remove-var* var-ns tooling-repl repl)))))

(defun replique/remove-var-session (repl)
  (replique/remove-var*
   (symbol-name (replique/get repl :ns)) (replique/active-repl :tooling) repl))

(defun replique/remove-var-get-ns ()
  (if (equal major-mode 'replique/mode)
      (replique-context/clojure-find-ns)))

(defun replique/remove-var ()
  "Unmaps all symbols that map to the var to be removed"
  (interactive)
  (if (not (featurep 'ivy))
      (user-error "replique-remove-var requires ivy-mode")
    (replique/with-modes-dispatch
     (replique/mode . 'replique/remove-var-session)
     (clojure-mode . 'replique/remove-var-clj)
     (clojurescript-mode . 'replique/remove-var-cljs)
     (clojurec-mode . 'replique/remove-var-cljc)
     (t . (user-error "Unsupported major mode: %s" major-mode)))))

(provide 'replique-remove-var)

;;; replique-remove-var.el ends here
