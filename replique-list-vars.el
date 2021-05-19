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

(require 'clj-data)
(require 'replique-repls)
(require 'clj-highlight)
(require 'replique-resources)
(require 'ivy)

(defun replique-list-vars/candidate-meta (candidate)
  (clj-data/hash-map
   :file (get-text-property 0 'replique-list-vars/file candidate)
   :line (get-text-property 0 'replique-list-vars/line candidate)
   :column (get-text-property 0 'replique-list-vars/column candidate)
   :beginning (get-text-property 0 'replique-list-vars/match-beginning candidate)
   :end (get-text-property 0 'replique-list-vars/match-end candidate)))

(defun replique-list-vars/ivy-read-with-highlight
    (prompt collection &key action)
  (let (;; Used to restore the moved positions in the visited buffers
        (previous-point nil)
        (previous-buffer nil))
    (ivy-read
     prompt
     collection
     :require-match t
     :action action
     :update-fn (lambda ()
                  (with-ivy-window
                    (when previous-buffer
                      (with-current-buffer previous-buffer
                        (goto-char previous-point)))
                    (clj-highlight/unhighlight)
                    (let ((candidate (nth ivy--index ivy--old-cands)))
                      (when candidate
                        (let* ((metas (replique-list-vars/candidate-meta candidate))
                               (file (clj-data/get metas :file))
                               (line (clj-data/get metas :line))
                               (column (clj-data/get metas :column))
                               (beginning (clj-data/get metas :beginning))
                               (end (clj-data/get metas :end)))
                          (when (or (and file line) (and file beginning end))
                            (when-let (buff (replique-resources/find-file file))
                              (pop-to-buffer-same-window buff)
                              (setq previous-buffer buff)
                              (setq previous-point (point))
                              (cond ((and beginning end)
                                     (goto-char beginning)
                                     (clj-highlight/highlight beginning end))
                                    (line
                                     (goto-char (point-min))
                                     (forward-line (1- line))
                                     (when column
                                       (move-to-column column))
                                     (clj-highlight/highlight))))))))))
     :unwind (lambda ()
               (when previous-buffer
                 (with-current-buffer previous-buffer
                   (goto-char previous-point)))
               (clj-highlight/unhighlight)))))

(defun replique-list-vars/vars-sorter (line-number-ref v1 v2)
  (let ((l1 (clj-data/get (aref v1 1) :line))
        (l2 (clj-data/get (aref v2 1) :line)))
    (cond ((null l2) t)
          ((null l1) nil)
          (t (if (or (and (< l1 line-number-ref) (< l2 line-number-ref))
                     (and (>= l1 line-number-ref) (>= l2 line-number-ref)))
                 (<= l1 l2)
               (> l1 l2))))))

(defun replique-list-vars/sort-vars (vars)
  (let ((line-number (line-number-at-pos)))
    (sort vars (apply-partially 'replique-list-vars/vars-sorter line-number))))

(defun replique-list-vars/candidate-with-meta (var-arr)
  (let ((meta (aref var-arr 1)))
    (propertize (symbol-name (aref var-arr 0))
                'replique-list-vars/file (clj-data/get meta :file)
                'replique-list-vars/line (clj-data/get meta :line)
                'replique-list-vars/column (clj-data/get meta :column))))

(defun replique-list-vars/list-vars (var-ns tooling-repl repl prompt action-fn)
  (let ((resp (replique/send-tooling-msg
               tooling-repl (clj-data/hash-map :type :list-vars
                                               :repl-env (clj-data/get repl :repl-env)
                                               :ns var-ns))))
    (let ((err (clj-data/get resp :error)))
      (if err
          (progn
            (message "%s" (clj-pprint/pprint-str err))
            (message "list-vars failed with ns: %s" var-ns))
        (let* ((vars (clj-data/get resp :vars))
               (vars (replique-list-vars/sort-vars vars))
               (var-names (mapcar 'replique-list-vars/candidate-with-meta vars)))
          (if (> (length var-names) 0)
              (replique-list-vars/ivy-read-with-highlight
               prompt var-names
               :action (apply-partially action-fn tooling-repl repl var-ns))
            (message "No var in the namespace: %s" var-ns)))))))

(defun replique-list-vars/list-namespaces (tooling-repl repl &optional default-ns)
  (let ((resp (replique/send-tooling-msg
               tooling-repl (clj-data/hash-map :type :list-namespaces
                                               :repl-env (clj-data/get repl :repl-env)))))
    (let ((err (clj-data/get resp :error)))
      (if err
          (progn
            (message "%s" (clj-pprint/pprint-str err))
            (message "list-namespaces failed"))
        (let ((namespaces (clj-data/get resp :namespaces)))
          (completing-read "Namespace: " namespaces nil t default-ns nil (car namespaces)))))))

(provide 'replique-list-vars)
