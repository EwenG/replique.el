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

(require 'arc-mode)
(require 'clojure-mode)
(require 'replique-utils)
(require 'replique-repls)
(require 'replique-files)
(require 'clj-data)
(require 'clj-context)
(require 'clj-pprint)
(require 'replique-context)
(require 'ivy)
(require 'xref)

(comment
 (progn (re-search-forward (replique-find-usage/symbols-regexp '(eee)) nil t)
        (match-string-no-properties 1))
 )

(defvar replique-find-usage/search-in-strings-and-comments nil
  "Whether the replique/find-usage should display matches found in strings or comments")
(defvar replique-find-usage/read-discard-re "#_")
(defvar replique-find-usage/clj-file-re "^[^.].*\\.cljc?$")
(defvar replique-find-usage/cljs-file-re "^[^.].*\\.clj[sc]$")
(defvar replique-find-usage/tag-reader-group-index 1)
(defvar replique-find-usage/symbol-group-index 2)

;; ?: -> shy group
;; beginning of line or symbol separator or end of line
(defvar replique-find-usage/symbol-separator-re
  (concat "\\(?:^\\|" "[" clj-context/symbol-separators "]" "\\|$\\)"))

(defvar replique-find-usage/symbol-ns-class-separator-re
  (concat "\\(?:^\\|" "[" clj-context/symbol-separators "/" "]" "\\|$\\)"))

(defun replique-find-usage/escaped-symbol-name (sym)
  (regexp-quote (symbol-name sym)))

(defun replique-find-usage/symbols-regexp (symbols &optional ns-or-class?)
  (concat replique-find-usage/symbol-separator-re
          ;; Detect tag reader
          "\\(#+[\s\n\t]+\\)?"
          ;; Handle read discards
          "\\(?:#_\\)*"
          ;; Handle quoted symbols
          "\\(?:'\\)?"
          ;; Handle vars
          "\\(?:#'\\)?"
          "\\(" (mapconcat 'replique-find-usage/escaped-symbol-name symbols "\\|") "\\)"
          (if ns-or-class?
              replique-find-usage/symbol-ns-class-separator-re
            replique-find-usage/symbol-separator-re)))

(defun replique-find-usage/variables ()
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1)
  (setq-local comment-use-syntax t)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local parse-sexp-ignore-comments t))

(defun replique-find-usage/jump-to-result (result)
  (let ((file (get-text-property 0 'replique-list-vars/file result))
        (position (get-text-property 0 'replique-list-vars/match-beginning result)))
    (when-let (buff (replique-resources/find-file file))
      (xref-push-marker-stack)
      (pop-to-buffer-same-window buff)
      (goto-char position))))

(defun replique-find-usage/show-results (tooling-repl prompt results)
  (replique-list-vars/ivy-read-with-highlight
   prompt results
   :action 'replique-find-usage/jump-to-result))

(defun replique-find-usage/maybe-visited-jar ()
  (let ((file-name (buffer-file-name)))
    (when (string-match "^\\(.*\\.jar\\)" file-name)
      (match-string-no-properties 0 file-name))))

(defun replique-find-usage/directory-or-jar? (f)
  (or (file-directory-p f)
      (string-suffix-p ".jar" f)))

(defun replique-find-usage/select-directories (tooling-repl)
  (let* ((default (or (replique-find-usage/maybe-visited-jar)
                      (replique/guess-project-root-dir)
                      (clj-data/get tooling-repl :directory)))
         (dir (let ((ivy-sort-functions-alist nil)
                    (candidates (replique/presorted-completion-table
                                 (list default
                                       "Classpath"
                                       "Other directory"))))
                (completing-read "Search in directory: "
                                 candidates nil t nil nil default))))
    (cond ((equal dir default)
           (list dir))
          ((equal "Classpath" dir)
           (let ((resp (replique/send-tooling-msg
                        tooling-repl
                        (clj-data/hash-map :type :classpath-for-sources
                                           :repl-env :replique/clj))))
             (let ((err (clj-data/get resp :error)))
               (if err
                   (progn
                     (message "%s" (clj-pprint/pprint-error-str err))
                     (message "classpath "))
                 (clj-data/get resp :paths)))))
          ((equal "Other directory" dir)
           (when-let (dir (read-file-name "Search in directory: " nil nil t nil
                                          'replique-find-usage/directory-or-jar?))
             (list dir))))))

(defun replique-find-usage/subdirectory? (dir base)
  (when (and dir base (file-directory-p dir) (file-directory-p base))
    (string-prefix-p (file-name-as-directory base) (file-name-as-directory dir))))

(defun replique-find-usage/subdirectory-dirs? (dir bases)
  (seq-some (apply-partially 'replique-find-usage/subdirectory? dir) bases))

(defun replique-find-usage/unset-subdirectory-dirs (bases dir)
  (while bases
    (when (and (car bases) (replique-find-usage/subdirectory? (car bases) dir))
      (setcar bases nil))
    (setq bases (cdr bases))))

;; Ensure no directory is the subdirectory of another in order to avoid duplicate files
;; We expect both paths to be absolute and canonicalized
(defun replique-find-usage/filter-subdirectories (dirs)
  (let ((filtered-dirs nil))
    (while dirs
      (cond ((null (car dirs)) nil)
            ((not (file-directory-p (car dirs)))
             (setq filtered-dirs (cons (car dirs) filtered-dirs)))
            ((not (replique-find-usage/subdirectory-dirs? (car dirs) (cdr dirs)))
             (setq filtered-dirs (cons (car dirs) filtered-dirs))
             (replique-find-usage/unset-subdirectory-dirs (cdr dirs) (car dirs))))
      (setq dirs (cdr dirs)))
    filtered-dirs))

(defun replique-find-usage/make-result (f-display f)
  (let* ((f-display (propertize f-display
                                'face 'compilation-info
                                'replique-list-vars/file f
                                'replique-list-vars/match-beginning
                                (match-beginning replique-find-usage/symbol-group-index)
                                'replique-list-vars/match-end (point)))
         (line-number (propertize (number-to-string (line-number-at-pos (point)))
                                  'face 'compilation-line-number)))
    (concat f-display ":"
            line-number ": "
            (buffer-substring (line-beginning-position) (line-end-position)))))

(defun replique-find-usage/jar-reducer (jar-file file-re files jar-entry)
  (if (and jar-entry (string-match-p file-re (aref jar-entry 0)))
      (cons (aref jar-entry 0) files)
    files))

(defun replique-find-usage/list-directory-files (file-re directory)
  (let ((replique/directory-files-recursively-files nil))
    (when (equal
           (replique/return-value-on-quit
            :interrupted
            (replique/directory-files-recursively*
             directory file-re
             'replique/default-dir-predicate "replique/find-usage"))
           :interrupted)
      (display-warning
       "replique/find-usage"
       "The listing of source files was interruted. replique/find-usage may return a partial result"))
    replique/directory-files-recursively-files))

(defvar replique-find-usage/jar-entries-cache (clj-data/hash-map))

(defun replique-find-usage/list-jar-entries (file-re jar-file)
  (with-temp-buffer
    (insert-file-contents jar-file)
    (nreverse
     (seq-reduce (apply-partially 'replique-find-usage/jar-reducer jar-file file-re)
                 (archive-zip-summarize)
                 '()))))

(defun replique-find-usage/do-find-usage
    (directories repl-type default-ns symbols-in-namespaces
                 include-string-and-comments? global-symbol
                 ns-or-class?)
  (with-temp-buffer
    (let ((results nil)
          (file-re (if (equal :cljs repl-type)
                       replique-find-usage/cljs-file-re
                     replique-find-usage/clj-file-re)))
      (dolist (d directories)
        (let* ((dir-or-jar (cond ((file-directory-p d) :dir)
                                 ((and (file-exists-p d) (string-suffix-p ".jar" d)) :jar)))
               (files (cond ((equal dir-or-jar :dir)
                             (replique-find-usage/list-directory-files file-re d))
                            ((equal dir-or-jar :jar)
                             (replique-find-usage/list-jar-entries file-re d)))))
          (dolist (f files)
            (when (or (equal dir-or-jar :jar) (file-readable-p f))
              (cond ((equal dir-or-jar :dir)
                     (insert-file-contents f nil nil nil t))
                    ((equal dir-or-jar :jar)
                     (erase-buffer)
                     (archive-zip-extract d f)))
              (replique-find-usage/variables)
              (setq case-fold-search nil)
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
                    (when (clj-data/contains? symbols-in-namespaces ns)
                      (let ((symbols (cons global-symbol
                                           (clj-data/get symbols-in-namespaces ns))))
                        (goto-char start-pos)
                        (while (re-search-forward (replique-find-usage/symbols-regexp
                                                   symbols ns-or-class?)
                                                  stop-pos t)
                          ;; move back, otherwise we may miss the next match
                          (goto-char (match-end replique-find-usage/symbol-group-index))
                          ;; when this is not a tag reader
                          (when (null (match-string-no-properties
                                       replique-find-usage/tag-reader-group-index))
                            (let ((ppss (when (not include-string-and-comments?)
                                          (with-syntax-table clojure-mode-syntax-table
                                            (syntax-ppss)))))
                              ;; Not in a string or a comment
                              (when (null (nth 8 ppss))
                                (let ((result (cond ((equal dir-or-jar :dir)
                                                     (replique-find-usage/make-result
                                                      (file-relative-name f d) f))
                                                    ((equal dir-or-jar :jar)
                                                     (let ((jar-file
                                                            (format "jar:file:%s!/%s" d f)))
                                                       (replique-find-usage/make-result
                                                        jar-file jar-file))))))
                                  (setq results (cons result results)))))))))
                    (setq replique-context/ns-starts (cdr replique-context/ns-starts)))))))))
      results)))

(defun replique-find-usage/find-usage* (symbol tooling-repl repl-type repl-env ns)
  (let ((resp (replique/send-tooling-msg
               tooling-repl
               (clj-data/hash-map :type :symbols-in-namespaces
                                  :repl-env repl-env
                                  :context (replique-context/get-context
                                            tooling-repl ns repl-env)
                                  :ns ns
                                  :symbol symbol))))
    (let ((err (clj-data/get resp :error)))
      (if err
          (progn
            (message "%s" (clj-pprint/pprint-error-str err))
            (message "find-usage failed with symbol: %s" symbol))
        (let* ((type (clj-data/get resp :find-usage-type))
               (global-symbol (clj-data/get resp type))
               (directories (when type (replique-find-usage/select-directories tooling-repl)))
               (directories (replique-find-usage/filter-subdirectories directories)))
          (cond ((equal :var type)
                 (let ((results (replique-find-usage/do-find-usage
                                 directories
                                 repl-type
                                 (clj-data/get resp :default-ns)
                                 (clj-data/get resp :symbols-in-namespaces)
                                 replique-find-usage/search-in-strings-and-comments
                                 global-symbol nil)))
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
                                 (clj-data/get resp :default-ns)
                                 (clj-data/get resp :symbols-in-namespaces)
                                 replique-find-usage/search-in-strings-and-comments
                                 global-symbol nil)))
                   (if results
                       (replique-find-usage/show-results
                        tooling-repl
                        (format "Match results for the keyword %s: " global-symbol)
                        (nreverse results))
                     (message "No match found for: %s" global-symbol))))
                ((equal :class type)
                 (let ((results (replique-find-usage/do-find-usage
                                 directories
                                 repl-type
                                 (clj-data/get resp :default-ns)
                                 (clj-data/get resp :symbols-in-namespaces)
                                 replique-find-usage/search-in-strings-and-comments
                                 global-symbol t)))
                   (if results
                       (replique-find-usage/show-results
                        tooling-repl
                        (if (equal :clj repl-type)
                            (format "Match results for the class %s: " global-symbol)
                          (format "Match results for %s: " global-symbol))
                        (nreverse results))
                     (message "No match found for: %s" global-symbol))))
                ((equal :namespace type)
                 (let ((results (replique-find-usage/do-find-usage
                                 directories
                                 repl-type
                                 (clj-data/get resp :default-ns)
                                 (clj-data/get resp :symbols-in-namespaces)
                                 replique-find-usage/search-in-strings-and-comments
                                 global-symbol t)))
                   (if results
                       (replique-find-usage/show-results
                        tooling-repl
                        (format "Match results for the namespace %s: " global-symbol)
                        (nreverse results))
                     (message "No match found for: %s" global-symbol))))
                ((equal :special-form type)
                 (message "Find usage does not supports special forms"))
                (t (message "Cannot resolve symbol: %s" symbol))))))))

(defun replique-find-usage/find-usage-clj (symbol tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                                     (clj-data/get clj-repl :repl-type)
                                     (clj-data/get clj-repl :repl-env)
                                     (replique-context/clojure-find-ns))))

(defun replique-find-usage/find-usage-cljs (symbol tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                                     (clj-data/get cljs-repl :repl-type)
                                     (clj-data/get cljs-repl :repl-env)
                                     (replique-context/clojure-find-ns))))

(defun replique-find-usage/find-usage-cljc (symbol tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique-find-usage/find-usage* symbol tooling-repl
                                     (clj-data/get repl :repl-type)
                                     (clj-data/get repl :repl-env)
                                     (replique-context/clojure-find-ns))))

(defun replique/find-usage (symbol)
  (interactive (list (let ((sym (symbol-at-point)))
                       (when sym (symbol-name sym)))))
  (when (buffer-file-name)
    (comint-check-source (buffer-file-name)))
  (replique/with-modes-dispatch
   (clojure-mode . (apply-partially 'replique-find-usage/find-usage-clj symbol))
   (clojurescript-mode . (apply-partially 'replique-find-usage/find-usage-cljs symbol))
   (clojurec-mode . (apply-partially 'replique-find-usage/find-usage-cljc symbol))
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(defvar replique-find-usage/search-in-strings-and-comments-history nil)

(defun replique-find-usage/param->history (param)
  (cond ((equal "search-in-strings-and-comments" param)
         'replique-find-usage/search-in-strings-and-comments-history)))

(defun replique-find-usage/param->param-candidate (k v)
  (propertize k
              'replique-params/param v
              'replique-params/default-val (symbol-value v)
              'replique-params/history (replique-find-usage/param->history k)))

(defun replique-find-usage/params->params-candidate (params)
  (let ((candidates nil))
    (maphash (lambda (k v)
               (push (replique-find-usage/param->param-candidate k v) candidates))
             params)
    candidates))

(defun replique-find-usage/edit-param (action-fn param)
  (cond ((equal "search-in-strings-and-comments" param)
         (replique-params/edit-boolean param action-fn))))

(defun replique-find-usage/set-param (param param-value)
  (set (get-text-property 0 'replique-params/param param)
       (cond ((equal "true" param-value) t)
             ((equal "false" param-value) nil)
             (t (string-to-number param-value)))))

(defun replique-find-usage/parameters ()
  (interactive)
  (let ((params (clj-data/hash-map
                 "search-in-strings-and-comments"
                 'replique-find-usage/search-in-strings-and-comments)))
    (ivy-read "Parameters: " (replique-find-usage/params->params-candidate params)
              :require-match t
              :action (apply-partially 'replique-find-usage/edit-param
                                       'replique-find-usage/set-param))))

(provide 'replique-find-usage)

;; Find usage does not find definitions generated by macros such as deftype/defrecord
;; locals analysis -> not implemented because quite complex for little value
;; Warning + exclusion for too large files -> Not implemented. We should find a way to check the size of a jar entry
