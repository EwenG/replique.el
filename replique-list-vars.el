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

(defvar-local replique-list-vars/line-overlay nil)

(defun replique-list-vars/make-overlay ()
  (let ((ol (make-overlay (point) (point))))
    (overlay-put ol 'priority -50)           ;(bug#16192)
    (overlay-put ol 'face '((t :inherit highlight)))
    ol))

(defun replique-list-vars/move-overlay (overlay)
  (let (tmp b e)
    (setq tmp t
          b (line-beginning-position)
          e (line-beginning-position 2))
    (if tmp
        (move-overlay overlay b e)
      (move-overlay overlay 1 1))))

(defun replique-list-vars/unhighlight ()
  (when replique-list-vars/line-overlay
    (delete-overlay replique-list-vars/line-overlay)))

(defun replique-list-vars/highlight ()
  (progn
    (unless replique-list-vars/line-overlay
      (setq replique-list-vars/line-overlay (replique-list-vars/make-overlay)))
    (overlay-put replique-list-vars/line-overlay
                 'window (selected-window))
    (replique-list-vars/move-overlay replique-list-vars/line-overlay)))

(defun replique-list-vars/candidate-metas (candidates var-name)
  (thread-first (seq-find (lambda (cand)
                            (string= (symbol-name (aref cand 0)) var-name))
                          candidates)
    (aref 1)))

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
               (var-names (mapcar (lambda (var-arr) (aref var-arr 0)) vars)))
          (when (> (length var-names) 0)
            (replique/return-nil-on-quit
             (ivy-read
              prompt
              var-names
              :require-match t
              :action (apply-partially action-fn tooling-repl repl var-ns)
              :update-fn (lambda ()
                           (with-ivy-window
                             (replique-list-vars/unhighlight)
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
                                       (goto-char (point-min))
                                       (forward-line (1- line))
                                       (when column
                                         (move-to-column column))
                                       (replique-list-vars/highlight))))))))))
            (replique-list-vars/unhighlight)))))))

(provide 'replique-list-vars)
