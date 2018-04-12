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

(defvar replique-find-usage/search-in-strings-and-comments nil
  "Whether the replique/find-usage should display matches found in strings or comments")
(defvar replique-find-usage/read-discard-re "#_")
(defvar replique-find-usage/clj-file-re "^[^.].*\\.cljc?$")
(defvar replique-find-usage/cljs-file-re "^[^.].*\\.clj[sc]$")
(defvar replique-find-usage/tag-reader-group-index 1)
(defvar replique-find-usage/symbol-group-index 2)

;; ?: -> shy group
;; beginnin of line or symbol separator or end of line
(defvar replique-find-usage/symbol-separator-re
  (concat "\\(?:^\\|" "[" replique-context/symbol-separators "]" "\\|$\\)"))

(defun replique-find-usage/escaped-symbol-name (sym)
  (regexp-quote (symbol-name sym)))

(defun replique-find-usage/symbols-regexp (symbols)
  (concat replique-find-usage/symbol-separator-re
          ;; Detect tag reader
          "\\(#+[\s\n\t]+\\)?"
          ;; Handle read discards
          "\\(?:#_\\)*"
          ;; Handle quoted symbols
          "\\(?:'\\)?"
          "\\(" (mapconcat 'replique-find-usage/escaped-symbol-name symbols "\\|") "\\)"
          replique-find-usage/symbol-separator-re))

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
  (let ((file (get-text-property 0 'replique-find-usage/file result))
        (position (get-text-property 0 'replique-find-usage/match-beginning result)))
    (when-let (buff (replique-resources/find-file file))
      (xref-push-marker-stack)
      (pop-to-buffer-same-window buff)
      (goto-char position))))

(defun replique-find-usage/show-results (tooling-repl prompt results)
  (let (;; Used to restore the moved positions in the visited buffers
        (previous-point nil)
        (previous-buffer nil))
    (ivy-read
     prompt
     results
     :require-match t
     :action 'replique-find-usage/jump-to-result
     :update-fn (lambda ()
                  (with-ivy-window
                    (when previous-point
                      (goto-char previous-point))
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
                              (setq previous-buffer buff)
                              (setq previous-point (point))
                              (goto-char beginning)
                              (replique-highlight/highlight beginning end))))))))
     :unwind (lambda ()
               (when previous-buffer
                 (with-current-buffer previous-buffer
                   (goto-char previous-point)))
               (replique-highlight/unhighlight)))))

(defun replique-find-usage/select-directories (tooling-repl)
  (let* ((default (or (replique/guess-project-root-dir) (replique/get tooling-repl :directory)))
         (dir (completing-read "Search in directory: "
                               `(,default
                                  "Classpath"
                                  "Other directory")
                               nil t)))
    (cond ((equal dir default)
           (list dir))
          ((equal "Classpath" dir)
           '())
          ((equal "Other directory" dir)
           (when-let (dir (read-directory-name "Search in directory: " nil nil t))
             (list dir))))))

(defun replique-find-usage/subdirectory? (dir base)
  (when (and dir base (file-directory-p dir) (file-directory-p base))
    (string-prefix-p base dir)))

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

(defun replique-find-usage/make-result (d f)
  (let* ((f (propertize f
                        'face 'compilation-info
                        'replique-find-usage/file f
                        'replique-find-usage/match-beginning
                        (match-beginning replique-find-usage/symbol-group-index)
                        'replique-find-usage/match-end (point)))
         (line-number (propertize (number-to-string (line-number-at-pos (point)))
                                  'face 'compilation-line-number)))
    (concat (file-relative-name f d) ":"
            line-number ": "
            (buffer-substring (line-beginning-position) (line-end-position)))))

(defun replique-find-usage/do-find-usage
    (directories repl-type default-ns symbols-in-namespaces
                 include-string-and-comments? global-symbol)
  (with-temp-buffer
    (let ((results nil)
          (file-re (if (equal :cljs repl-type)
                       replique-find-usage/cljs-file-re
                     replique-find-usage/clj-file-re)))
      (dolist (d directories)
        (let ((replique/directory-files-recursively-files nil))
          (replique/directory-files-recursively*
           d file-re 'replique/default-dir-predicate "replique/find-usage")
          (let ((files replique/directory-files-recursively-files))
            (dolist (f files)
              (when (file-readable-p f)
                (insert-file-contents-literally f nil nil nil t)
                (replique-find-usage/variables)
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
                        (let ((symbols (cons global-symbol
                                             (replique/get symbols-in-namespaces ns))))
                          (goto-char start-pos)
                          (while (re-search-forward (replique-find-usage/symbols-regexp symbols)
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
                                  (let ((result (replique-find-usage/make-result d f)))
                                    (setq results (cons result results)))))))))
                      (setq replique-context/ns-starts (cdr replique-context/ns-starts))))))))))
      results)))

(defun replique-find-usage/find-usage* (symbol tooling-repl repl-type repl-env ns)
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
               (global-symbol (replique/get resp type))
               (directories (when type (replique-find-usage/select-directories tooling-repl)))
               (directories (replique-find-usage/filter-subdirectories directories)))
          (cond ((equal :var type)
                 (let ((results (replique-find-usage/do-find-usage
                                 directories
                                 repl-type
                                 (replique/get resp :default-ns)
                                 (replique/get resp :symbols-in-namespaces)
                                 replique-find-usage/search-in-strings-and-comments
                                 global-symbol)))
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
                                 replique-find-usage/search-in-strings-and-comments
                                 global-symbol)))
                   (if results
                       (replique-find-usage/show-results
                        tooling-repl
                        (format "Match results for the keyword %s: " global-symbol)
                        (nreverse results))
                     (message "No match found for: %s" global-symbol))))
                (t (message "Cannot resolve symbol: %s" symbol))))))))

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
    (when (buffer-file-name)
      (comint-check-source (buffer-file-name)))
    (replique/with-modes-dispatch
     (clojure-mode . (apply-partially 'replique-find-usage/find-usage-clj symbol))
     (clojurescript-mode . (apply-partially 'replique-find-usage/find-usage-cljs symbol))
     (clojurec-mode . (apply-partially 'replique-find-usage/find-usage-cljc symbol))
     (t . (user-error "Unsupported major mode: %s" major-mode)))))

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
  (if (not (featurep 'ivy))
      (user-error "replique-find-usage/parameters requires ivy-mode")
    (let ((params (replique/hash-map
                   "search-in-strings-and-comments" 'replique-find-usage/search-in-strings-and-comments)))
      (ivy-read "Parameters: " (replique-find-usage/params->params-candidate params)
                :require-match t
                :action (apply-partially 'replique-find-usage/edit-param
                                         'replique-find-usage/set-param)))))

(provide 'replique-find-usage)

;; Warning + exclusion for too large file
;; handle user interruption + warning
;; Analyze locals for results symbols with no namespace. Ignore locals on quoted forms
;; Search in jars
;; search in a directory -> exclude directories that are a subdirectory of one of the others

;; Find usage does not find definitions generated by macros such as deftype/defrecord
