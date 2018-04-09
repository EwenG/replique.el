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
(require 'replique-highlight)

(comment
 (progn (re-search-forward (replique-find-usage/symbols-regexp '(eee)) nil t)
        (match-string-no-properties 1))
 )

(defvar replique-find-usage/directory-max-depth 20
  "The maximum directory depth used when searching for Clojure/Clojurescript files during the `replique/find-usage' command")
(defvar replique-find-usage/read-discard-re "#_")
(defvar replique-find-usage/clj-file-re "^[^.].*\\.cljc?$")
(defvar replique-find-usage/cljs-file-re "^[^.].*\\.clj[sc]$")

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
        (position (get-text-property 0 'replique-find-usage/match-beginning result)))
    (when-let (buff (replique-resources/find-file file))
      (xref-push-marker-stack)
      (pop-to-buffer-same-window buff)
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
                   (replique-highlight/unhighlight)
                   (let ((candidate (nth ivy--index ivy--old-cands)))
                     (when candidate
                       (let ((file (get-text-property
                                    0 'replique-find-usage/file candidate))
                             (beginning (get-text-property
                                         0 'replique-find-usage/match-beginning candidate))
                             (end (get-text-property
                                         0 'replique-find-usage/match-end candidate)))
                         (when (and file beginning end)
                           (when-let (buff (replique-resources/find-file file))
                             (pop-to-buffer-same-window buff)
                             (goto-char beginning)
                             (replique-highlight/highlight beginning end))))))))))
  (replique-highlight/unhighlight))

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
  (let* ((f (propertize f
                        'face 'compilation-info
                        'replique-find-usage/file f
                        'replique-find-usage/match-beginning (match-beginning 2)
                        'replique-find-usage/match-end (point)))
         (line-number (propertize (number-to-string (line-number-at-pos (point)))
                                  'face 'compilation-line-number)))
    (concat f ":"
            line-number ": "
            (buffer-substring (line-beginning-position) (line-end-position)))))

(defun replique-find-usage/do-find-usage
    (directories repl-type default-ns symbols-in-namespaces
                 type global-symbol)
  (with-temp-buffer
    (let ((results nil)
          (file-re (if (equal :cljs repl-type)
                       replique-find-usage/cljs-file-re
                     replique-find-usage/clj-file-re)))
      (dolist (d directories)
        (let ((files (replique/directory-files-recursively
                      d file-re 'replique/default-dir-predicate
                      replique-find-usage/directory-max-depth)))
          (when replique/directory-max-depth-reached
            (display-warning
             "replique/find-usage"
             "The maximum directory depth has been reached. One or multiple files may have been skipped. See replique-find-usage/directory-max-depth"))
          (dolist (f files)
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
                       (stop-pos (cdr (cadr replique-context/ns-starts))))
                  (when (replique/contains? symbols-in-namespaces ns)
                    (let ((symbols (cons global-symbol (replique/get symbols-in-namespaces ns))))
                      (goto-char start-pos)
                      (while (re-search-forward (replique-find-usage/symbols-regexp symbols)
                                                stop-pos t)
                        ;; move back, otherwise we may miss the next match
                        (goto-char (match-end 2))
                        ;; when this is not a tag reader
                        (when (null (match-string-no-properties 1))
                          (let ((result (replique-find-usage/make-result f)))
                            (setq results (cons result results)))))))
                  (setq replique-context/ns-starts (cdr replique-context/ns-starts))))))))
      results)))

(defun replique-find-usage/find-usage* (symbol tooling-repl repl-type repl-env ns)
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
          (let* ((type (replique/get resp :find-usage-type))
                 (global-symbol (replique/get resp type)))
            (cond ((equal :var type)
                   (let ((results (replique-find-usage/do-find-usage
                                   directories
                                   repl-type
                                   (replique/get resp :default-ns)
                                   (replique/get resp :symbols-in-namespaces)
                                   type global-symbol)))
                     (if results
                         (replique-find-usage/show-results
                          tooling-repl
                          (format "Match results for the var %s: " global-symbol)
                          (nreverse results))
                       (message "No match found for: %s" global-symbol))))
                  ((equal :keyword type)
                   (let ((results (replique-find-usage/do-find-usage
                                   directories
                                   repl-type
                                   (replique/get resp :default-ns)
                                   (replique/get resp :symbols-in-namespaces)
                                   type global-symbol)))
                     (if results
                         (replique-find-usage/show-results
                          tooling-repl
                          (format "Match results for the keyword %s: " global-symbol)
                          (nreverse results))
                       (message "No match found for: %s" global-symbol))))
                  (t (message "Cannot resolve symbol: %s" symbol)))))))))

(defun replique-find-usage/find-usage-clj (symbol tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                                     (replique/get clj-repl :repl-type)
                                     (replique/get clj-repl :repl-env)
                                     (replique-context/clojure-find-ns))))

(defun replique-find-usage/find-usage-cljs (symbol tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                                     (replique/get cljs-repl :repl-type)
                                     (replique/get cljs-repl :repl-env)
                                     (replique-context/clojure-find-ns))))

(defun replique-find-usage/find-usage-cljc (symbol tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                                     (replique/get repl :repl-type)
                                     (replique/get repl :repl-env)
                                     (replique-context/clojure-find-ns))))

(defun replique/find-usage (symbol)
  (interactive (list (let ((sym (symbol-at-point)))
                       (when sym (symbol-name sym)))))
  (if (not (featurep 'ivy))
      (user-error "replique/find-usage requires ivy-mode")
    (comint-check-source (buffer-file-name))
    (replique/with-modes-dispatch
     (clojure-mode . (apply-partially 'replique-find-usage/find-usage-clj symbol))
     (clojurescript-mode . (apply-partially 'replique-find-usage/find-usage-cljs symbol))
     (clojurec-mode . (apply-partially 'replique-find-usage/find-usage-cljc symbol))
     (t . (user-error "Unsupported major mode: %s" major-mode)))))

(provide 'replique-find-usage)


;; Message when there is no result candidates - add the var qualified name to the result
;; Warning + exclusion for too large file
;; Display progress
;; Analyze locals for results symbols with no namespace
;; Display file name relative to the directory
;; Option to search in string and comments
;; invert select-directory / find-usage
;; C-g during ivy-read: return to the starting point
;; what about quoted symbols?
