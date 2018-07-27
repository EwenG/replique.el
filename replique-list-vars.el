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

(require 'replique-hashmap)
(require 'replique-repls)
(require 'replique-highlight)
(require 'replique-resources)
(require 'ivy)

(defun replique-list-vars/candidate-meta (candidate)
  (replique/hash-map
   :file (get-text-property 0 'replique-list-vars/file candidate)
   :line (get-text-property 0 'replique-list-vars/line candidate)
   :column (get-text-property 0 'replique-list-vars/column candidate)
   :beginning (get-text-property 0 'replique-list-vars/match-beginning candidate)
   :end (get-text-property 0 'replique-list-vars/match-end candidate)))

(defun replique-list-vars/ivy-read-with-hightlight
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
                    (replique-highlight/unhighlight)
                    (let ((candidate (nth ivy--index ivy--old-cands)))
                      (when candidate
                        (let* ((metas (replique-list-vars/candidate-meta candidate))
                               (file (replique/get metas :file))
                               (line (replique/get metas :line))
                               (column (replique/get metas :column))
                               (beginning (replique/get metas :beginning))
                               (end (replique/get metas :end)))
                          (when (or (and file line) (and file beginning end))
                            (when-let (buff (replique-resources/find-file file))
                              (pop-to-buffer-same-window buff)
                              (setq previous-buffer buff)
                              (setq previous-point (point))
                              (cond ((and beginning end)
                                     (goto-char beginning)
                                     (replique-highlight/highlight beginning end))
                                    (line
                                     (goto-char (point-min))
                                     (forward-line (1- line))
                                     (when column
                                       (move-to-column column))
                                     (replique-highlight/highlight))))))))))
     :unwind (lambda ()
               (when previous-buffer
                 (with-current-buffer previous-buffer
                   (goto-char previous-point)))
               (replique-highlight/unhighlight)))))

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

(defun replique-list-vars/candidate-with-meta (var-arr)
  (let ((meta (aref var-arr 1)))
    (propertize (symbol-name (aref var-arr 0))
                'replique-list-vars/file (replique/get meta :file)
                'replique-list-vars/line (replique/get meta :line)
                'replique-list-vars/column (replique/get meta :column))))

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
               (var-names (mapcar 'replique-list-vars/candidate-with-meta vars)))
          (if (> (length var-names) 0)
              (replique-list-vars/ivy-read-with-hightlight
               prompt var-names
               :action (apply-partially action-fn tooling-repl repl var-ns))
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
