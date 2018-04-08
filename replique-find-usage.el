;; replique-find-usage.el ---   -*- lexical-binding: t; -*-

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

(require 'replique-context)


(comment
 (progn (re-search-forward (replique-find-usage/symbols-regexp '(eee)) nil t)
        (match-string-no-properties 1))
 )

(defvar replique-find-usage/excluded-folders '("target" "out"))
(defvar replique-find-usage/read-discard-re "#_")

;; ?: -> shy group
;; beginnin of line or symbol separator or end of line
(defvar replique-find-usage/symbol-separator-re
  (concat "\\(?:^\\|" "[" replique-context/symbol-separators "]" "\\|$\\)"))

(defun replique-find-usage/symbols-regexp (symbols)
  (concat replique-find-usage/symbol-separator-re
          ;; Detect dispatch macro
          "\\(#+[\s\n\t]+\\)?"
          ;; Handle read discards
          "\\(?:#_\\)*"
          "\\(" (mapconcat 'symbol-name symbols "\\|") "\\)"
          replique-find-usage/symbol-separator-re))

(defun replique-find-usage/jump-to-result (result)
  (let ((file (get-text-property 0 'replique-find-usage/file result))
        (position (get-text-property 0 'replique-find-usage/position result)))
    (when-let (buff (replique-resources/find-file file))
      (xref-push-marker-stack)
      (pop-to-buffer-same-window buff)
      (replique-list-vars/unhighlight)
      (goto-char position))))

(defun replique-find-usage/show-results (tooling-repl prompt results)
  (replique/return-nil-on-quit
   (ivy-read
    prompt
    results
    :require-match t
    :action 'replique-find-usage/jump-to-result
    :update-fn (lambda ()
                 (with-ivy-window
                   (replique-list-vars/unhighlight)
                   (let ((candidate (nth ivy--index ivy--old-cands)))
                     (when candidate
                       (let ((file (get-text-property
                                    0 'replique-find-usage/file candidate))
                             (position (get-text-property
                                        0 'replique-find-usage/position candidate)))
                         (when (and file position)
                           (when-let (buff (replique-resources/find-file file))
                             (pop-to-buffer-same-window buff)
                             (goto-char position)
                             (replique-list-vars/highlight))))))))
    :unwind 'replique-list-vars/unhighlight))
  (replique-list-vars/unhighlight))

(defun replique-find-usage/select-directories (tooling-repl)
  (let ((dir (completing-read "Search in directory: "
                              `(,(replique/get tooling-repl :directory)
                                "Classpath"
                                "Other directory")
                              nil t)))
    (cond ((equal dir (replique/get tooling-repl :directory))
           (list dir))
          ((equal "Classpath" dir)
           '())
          ((equal "Other directory" dir)
           (when-let (dir (read-directory-name "Search in directory: " nil nil t))
             (list dir))))))

(defun replique-find-usage/make-result (f)
  (let* ((match-end (match-end 2))
         (f (propertize f
                        'face 'compilation-info
                        'replique-find-usage/file f
                        'replique-find-usage/position match-end))
         (line-number (propertize (number-to-string (line-number-at-pos match-end))
                                  'face 'compilation-line-number)))
    (concat f ":"
            line-number ": "
            (buffer-substring (line-beginning-position)
                              (line-end-position)))))

(defun replique-find-usage/do-find-usage (directories default-ns symbols-in-namespaces)
  (with-temp-buffer
    (let ((results nil))
      (dolist (d directories)
        (dolist (f (directory-files-recursively d "^[^.].*\\.clj\\(:?[sc]\\)?$" nil))
          (insert-file-contents-literally f nil nil nil t)
          (goto-char (point-min))
          (let ((replique-context/ns-starts nil))
            (replique-context/clojure-find-ns-starts* nil)
            (when (null replique-context/ns-starts)
              (setq replique-context/ns-starts `((,default-ns . 0))))
            (while replique-context/ns-starts
              (let* ((ns-start (car replique-context/ns-starts))
                     (ns (car ns-start))
                     (start-pos (cdr ns-start))
                     (stop-pos (cdr (cadr replique-context/ns-starts)))
                     (symbols (replique/get symbols-in-namespaces ns)))
                (when symbols
                  (goto-char start-pos)
                  (while (re-search-forward (replique-find-usage/symbols-regexp symbols)
                                            stop-pos t)
                    ;; when this is not a tag reader
                    (when (null (match-string-no-properties 1))
                      (let ((result (replique-find-usage/make-result f)))
                        (setq results (cons result results))))))
                (setq replique-context/ns-starts (cdr replique-context/ns-starts)))))))
      results)))

(defun replique-find-usage/find-usage* (symbol tooling-repl repl-env ns)
  (when-let (directories (replique-find-usage/select-directories tooling-repl))
    (let ((resp (replique/send-tooling-msg
                 tooling-repl
                 (replique/hash-map :type :symbols-in-namespaces
                                    :repl-env repl-env
                                    :context (replique-context/get-context ns repl-env)
                                    :ns ns
                                    :symbol symbol))))
      (let ((err (replique/get resp :error)))
        (if err
            (progn
              (message "%s" (replique-pprint/pprint-error-str err))
              (message "find-usage failed with symbol: %s" symbol))
          (let ((results (replique-find-usage/do-find-usage
                          directories
                          (replique/get resp :default-ns)
                          (replique/get resp :symbols-in-namespaces))))
            (if results
                (replique-find-usage/show-results
                 tooling-repl
                 "Warning: Some files have been excluded because they were too large"
                 (nreverse results))
              (message "No match found"))))))))

(defun replique-find-usage/find-usage-clj (symbol tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                               (replique/get clj-repl :repl-env)
                               (replique-context/clojure-find-ns))))

(defun replique-find-usage/find-usage-cljs (symbol tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                               (replique/get cljs-repl :repl-env)
                               (replique-context/clojure-find-ns))))

(defun replique-find-usage/find-usage-cljc (symbol tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                               (replique/get repl :repl-env)
                               (replique-context/clojure-find-ns))))

(defun replique/find-usage (symbol)
  (interactive (list (let ((sym (symbol-at-point)))
                       (when sym (symbol-name sym)))))
  (if (not (featurep 'ivy))
      (user-error "replique/find-usage requires ivy-mode")
    (replique/with-modes-dispatch
     (clojure-mode . (apply-partially 'replique-find-usage/find-usage-clj symbol))
     (clojurescript-mode . (apply-partially 'replique-find-usage/find-usage-cljs symbol))
     (clojurec-mode . (apply-partially 'replique-find-usage/find-usage-cljc symbol))
     (t . (user-error "Unsupported major mode: %s" major-mode)))))

(provide 'replique-find-usage)


;; clj env -> exclude .cljs, cljs env -> exclude .clj
;; Message when there is no result candidates - add the var qualified name to the result
;; Warnings on error
;; exclude large files
;; Display progress
;; Exclude target / out directories
;; show a more precise overlay. Remove all overlays on C-g
;; Analyze locals for results with no namespace
;; Display file name relative to the directory
