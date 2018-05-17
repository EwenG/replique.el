;; replique-list-vars.el ---   -*- lexical-binding: t; -*-

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

(require 'replique-highlight)

(defun replique-list-vars/candidate-metas (candidates var-name)
  (thread-first (seq-find (lambda (cand)
                            (string= (symbol-name (aref cand 0)) var-name))
                          candidates)
    (aref 1)))

(defun replique-list-vars/vars-sorter (line-number-ref v1 v2)
  (let ((l1 (replique/get (aref v1 1) :line))
        (l2 (replique/get (aref v2 1) :line)))
    (cond ((null l2) t)
          ((null l1) nil)
          (t (if (or (and (< l1 line-number-ref) (< l2 line-number-ref))
                     (and (>= l1 line-number-ref) (>= l2 line-number-ref)))
                 (<= l1 l2)
               (> l1 l2))))))

(defun replique-list-vars/sort-vars (vars)
  (let ((line-number (line-number-at-pos)))
    (sort vars (apply-partially 'replique-list-vars/vars-sorter line-number))))

(defun replique-list-vars/list-vars (var-ns tooling-repl repl prompt action-fn)
  (let ((resp (replique/send-tooling-msg
               tooling-repl (replique/hash-map :type :list-vars
                                               :repl-env (replique/get repl :repl-env)
                                               :ns var-ns))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-pprint/pprint-str err))
            (message "list-vars failed with ns: %s" var-ns))
        (let* ((vars (replique/get resp :vars))
               (vars (replique-list-vars/sort-vars vars))
               (var-names (mapcar (lambda (var-arr) (aref var-arr 0)) vars)))
          (if (> (length var-names) 0)
              (let (;; Used to restore the moved positions in the visited buffers
                    (previous-point nil)
                    (previous-buffer nil))
                (ivy-read
                 prompt
                 var-names
                 :require-match t
                 :action (apply-partially action-fn tooling-repl repl var-ns)
                 :update-fn (lambda ()
                              (with-ivy-window
                                (when previous-point
                                  (goto-char previous-point))
                                (replique-highlight/unhighlight)
                                (let ((candidate (nth ivy--index ivy--old-cands)))
                                  (when candidate
                                    (let* ((metas (replique-list-vars/candidate-metas
                                                   vars candidate))
                                           (file (replique/get metas :file))
                                           (line (replique/get metas :line))
                                           (column (replique/get metas :column)))
                                      (when (and file line)
                                        (when-let (buff (replique-resources/find-file file))
                                          (pop-to-buffer-same-window buff)
                                          (setq previous-buffer buff)
                                          (setq previous-point (point))
                                          (goto-char (point-min))
                                          (forward-line (1- line))
                                          (when column
                                            (move-to-column column))
                                          (replique-highlight/highlight))))))))
                 :unwind (lambda ()
                           (when previous-buffer
                             (with-current-buffer previous-buffer
                               (goto-char previous-point)))
                           (replique-highlight/unhighlight))))
            (message "No var in the namespace: %s" var-ns)))))))

(defun replique-list-vars/list-namespaces (tooling-repl repl &optional default-ns)
  (let ((resp (replique/send-tooling-msg
               tooling-repl (replique/hash-map :type :list-namespaces
                                               :repl-env (replique/get repl :repl-env)))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-pprint/pprint-str err))
            (message "list-namespaces failed"))
        (let ((namespaces (replique/get resp :namespaces)))
          (completing-read "Namespace: " namespaces nil t default-ns))))))

(provide 'replique-list-vars)
