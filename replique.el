;; replique.el ---   -*- lexical-binding: t; -*-

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

;; Version 0.0.15-SNAPSHOT
;; Package-Requires: ((emacs "25") (clojure-mode "5.6.0") (ivy "0.10.0"))

;; Commentary:

;; Code:

(require 'subr-x)
(require 'comint)
(require 'clojure-mode)
(require 'map)
(require 'ivy)
(require 'replique-utils)
(require 'replique-repls)
(require 'replique-hashmap)
(require 'replique-cli)
(require 'replique-resources)
(require 'replique-files)
(require 'replique-remove-var)
(require 'replique-transit)
(require 'replique-context)
(require 'replique-print)
(require 'replique-pprint)
(require 'replique-params)
(require 'replique-watch)
(require 'replique-find-usage)
(require 'replique-logback)
(require 'replique-input)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique/pp-repls ()
  (string-join
   (thread-last replique/repls
     (mapcar (lambda (repl)
               (let* ((repl (if (replique/contains? repl :buffer)
                                (replique/assoc repl :buffer '**)
                              repl))
                      (repl (if (replique/contains? repl :proc)
                                (replique/assoc repl :proc '**)
                              repl))
                      (repl (if (replique/contains? repl :network-proc)
                                (replique/assoc repl :network-proc '**)
                              repl))
                      (repl (if (replique/contains? repl :recent-output)
                                (replique/assoc repl :recent-output '**)
                              repl)))
                 repl)))
     (mapcar 'replique-pprint/pprint-str))
   "\n"))

;; Auto completion

(defconst replique/annotations-map
  (replique/hash-map
   :method "me"
   :field "fi"
   :static-method "sm"
   :static-field "sf"
   :keyword "k"
   :local "l"
   :namespace "n"
   :class "c"
   :macro "m"
   :function "f"
   :var "v"
   :resource "r"
   :special-form "s"
   :directory "d"))

(defun replique/make-candidate (c)
  (map-let
      ((:candidate candidate) (:type type) (:package package) (:ns ns) (:match-index match-index))
      c
    (propertize candidate
                'meta (replique/hash-map :type type :package package :ns ns)
                'match-index match-index)))

(defun replique/auto-complete* (prefix tooling-repl repl-env ns)
  (let* ((resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :completion
                                   :repl-env repl-env
                                   :context (replique-context/get-context
                                             tooling-repl ns repl-env)
                                   :ns ns
                                   :prefix prefix))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-pprint/pprint-error-str err))
            (message "completion failed with prefix %s" prefix)
            nil)
        (let* ((candidates (replique/get resp :candidates)))
          (mapcar 'replique/make-candidate candidates))))))

(defun replique/auto-complete-session (prefix repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (repl-env (replique/get repl :repl-env))
         (ns (symbol-name (replique/get repl :ns))))
    (replique/auto-complete* prefix tooling-repl repl-env ns)))

(defun replique/auto-complete-clj (prefix tooling-repl clj-repl)
  (when clj-repl
    (replique/auto-complete* prefix tooling-repl
                             (replique/get clj-repl :repl-env)
                             (replique-context/clojure-find-ns))))

(defun replique/auto-complete-cljs (prefix tooling-repl cljs-repl)
  (when cljs-repl
    (replique/auto-complete* prefix tooling-repl
                             (replique/get cljs-repl :repl-env)
                             (replique-context/clojure-find-ns))))

(defun replique/auto-complete-cljc (prefix tooling-repl repl)
  (when repl
    (replique/auto-complete* prefix tooling-repl
                             (replique/get repl :repl-env)
                             (replique-context/clojure-find-ns))))

(defun replique/auto-complete (prefix)
  (when (not (null (replique/active-repl :tooling)))
    (replique/with-modes-dispatch
     (replique/mode . (apply-partially 'replique/auto-complete-session prefix))
     (clojure-mode . (apply-partially 'replique/auto-complete-clj prefix))
     (clojurescript-mode . (apply-partially 'replique/auto-complete-cljs prefix))
     (clojurec-mode . (apply-partially 'replique/auto-complete-cljc prefix))
     (t . nil))))

(defun replique/auto-complete-annotation (candidate)
  (when-let ((meta (get-text-property 0 'meta candidate)))
    (map-let ((:type type) (:package package) (:ns ns)) meta
      (let ((type (gethash type replique/annotations-map)))
        (cond ((and ns type)
               (format "%s <%s>" ns type))
              ((and package type)
               (format "%s <%s>" package type))
              (type
               (format "<%s>" type))
              (ns
               (format "%s" ns))
              (package
               (format "%s" package))
              (t nil))))))

(defun replique/auto-complete-match-index (candidate)
  (get-text-property 0 'match-index candidate))

(defun replique/repliquedoc-format-arglist (index arglist)
  (let ((args-index 1)
        (force-highlight nil))
    (thread-first
        (lambda (arg)
          (cond ((equal "&" arg)
                 (when (<= args-index index)
                   (setq force-highlight t))
                 arg)
                ((or force-highlight (equal index args-index))
                 (setq args-index (1+ args-index))
                 (replique-print/with-face :object arg :face 'eldoc-highlight-function-argument))
                (t
                 (setq args-index (1+ args-index))
                 arg)))
      (mapcar arglist)
      (seq-into 'vector))))

(defun replique/repliquedoc-format-arglists (index arglists)
  (thread-first (lambda (x) (replique/repliquedoc-format-arglist index x))
    (mapcar arglists)
    replique-print/print-str))

(defun replique/repliquedoc-format (msg)
  (map-let ((:name name) (:arglists arglists) (:return return) (:index index)) msg
    (cond ((and arglists return)
           (format "%s: %s -> %s"
                   name (replique/repliquedoc-format-arglists index arglists) return))
          (arglists
           (format "%s: %s" name (replique/repliquedoc-format-arglists index arglists)))
          (name (format "%s" name))
          (t nil))))

(defun replique/repliquedoc* (tooling-repl repl-env ns)
  (let ((resp (replique/send-tooling-msg
               tooling-repl
               (replique/hash-map :type :repliquedoc
                                  :repl-env repl-env
                                  :context (replique-context/get-context
                                            tooling-repl ns repl-env)
                                  :ns ns))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-pprint/pprint-error-str err))
            (message "eldoc failed")
            nil)
        (with-demoted-errors "eldoc error: %s"
          (replique/repliquedoc-format (replique/get resp :doc)))))))

(defun replique/repliquedoc-session (repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (repl-env (replique/get repl :repl-env))
         (ns (symbol-name (replique/get repl :ns))))
    (replique/repliquedoc* tooling-repl repl-env ns)))

(defun replique/repliquedoc-clj (tooling-repl clj-repl)
  (when clj-repl
    (replique/repliquedoc*
     tooling-repl (replique/get clj-repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/repliquedoc-cljs (tooling-repl cljs-repl)
  (when cljs-repl
    (replique/repliquedoc*
     tooling-repl (replique/get cljs-repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/repliquedoc-cljc (tooling-repl repl)
  (when repl
    (replique/repliquedoc*
     tooling-repl (replique/get repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/eldoc-documentation-function ()
  ;; Ensures a REPL is started, since eldoc-mode is enabled globally by default
  (when (not (null (replique/active-repl :tooling)))
    (replique/with-modes-dispatch
     (replique/mode . 'replique/repliquedoc-session)
     (clojure-mode . 'replique/repliquedoc-clj)
     (clojurescript-mode . 'replique/repliquedoc-cljs)
     (clojurec-mode . 'replique/repliquedoc-cljc)
     (t . nil))))

(defun replique/in-ns-session (repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (all-ns (replique/get (replique/list-namespaces
                                tooling-repl (replique/get repl :repl-env))
                               :namespaces))
         (ns-name (when all-ns
                    (completing-read "Set ns to: " all-ns))))
    (when (> (length ns-name) 0)
      (if (equal :clj (replique/get repl :repl-type))
          (replique/send-input-from-source-clj
           (format "(clojure.core/in-ns '%s)" ns-name) tooling-repl repl)
        (replique/send-input-from-source-cljs
         (format "(replique.interactive/cljs-in-ns '%s)" ns-name) tooling-repl repl)))))

(defun replique/in-ns-clj (tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (let* ((all-ns (replique/get (replique/list-namespaces
                                  tooling-repl (replique/get clj-repl :repl-env))
                                 :namespaces))
           (ns-name (when all-ns
                      (completing-read
                       "Set ns to: " all-ns nil nil (replique-context/clojure-find-ns)))))
      (when (> (length ns-name) 0)
        (replique/send-input-from-source-clj
         (format "(clojure.core/in-ns '%s)" ns-name) tooling-repl clj-repl)))))

(defun replique/in-ns-cljs (tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (let* ((all-ns (replique/get (replique/list-namespaces
                                  tooling-repl (replique/get cljs-repl :repl-env))
                                 :namespaces))
           (ns-name (when all-ns
                      (completing-read
                       "Set ns to: " all-ns nil nil (replique-context/clojure-find-ns)))))
      (when (> (length ns-name) 0)
        (replique/send-input-from-source-cljs
         (format "(replique.interactive/cljs-in-ns '%s)" ns-name) tooling-repl cljs-repl)))))

(defun replique/in-ns-cljc (tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")

    (let* ((all-ns (replique/get (replique/list-namespaces
                                  tooling-repl (replique/get repl :repl-env))
                                 :namespaces))
           (ns-name (when all-ns
                      (completing-read
                       "Set ns to: " all-ns nil nil (replique-context/clojure-find-ns)))))
      (when (> (length ns-name) 0)
        (replique/send-input-from-source-cljc
         (if (equal :cljs (replique/get repl :repl-type))
             (format "(replique.interactive/cljs-in-ns '%s)" ns-name)
           (format "(clojure.core/in-ns '%s)" ns-name))
         tooling-repl repl)))))

(defun replique/in-ns ()
  "Change the active REPL namespace"
  (interactive)
  (replique/with-modes-dispatch
   (replique/mode . 'replique/in-ns-session)
   (clojure-mode . 'replique/in-ns-clj)
   (clojurescript-mode . 'replique/in-ns-cljs)
   (clojurec-mode . 'replique/in-ns-cljc)
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(defun replique/symbol-backward ()
  (let* ((ppss (syntax-ppss (point)))
         (in-string? (not (null (nth 3 ppss)))))
    (if in-string?
        (let ((string-start (nth 8 ppss)))
          (when (and string-start
                     (< (+ 1 string-start) (point))
                     (>= string-start (line-beginning-position)))
            (buffer-substring-no-properties (+ 1 string-start) (point))))
      (let ((sym-bounds (bounds-of-thing-at-point 'symbol)))
        (when sym-bounds
          (buffer-substring-no-properties (car sym-bounds) (point)))))))

(defun replique/company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cond
   ((equal command 'interactive)
    (company-begin-backend 'replique/company-backend))
   ((equal command 'prefix) (when (or (derived-mode-p 'clojure-mode)
                                      (derived-mode-p 'replique/mode))
                              (replique/symbol-backward)))
   ((equal command 'sorted) t)
   ((equal command 'candidates) (replique/auto-complete arg))
   ((equal command 'annotation) (replique/auto-complete-annotation arg))
   ((equal command 'no-cache) t)
   ((equal command 'match) (replique/auto-complete-match-index arg))))

(defun replique/with-interrupt-warning (fn &rest params)
  (let ((maybe-interrupted (replique/return-value-on-quit
                            :interrupted
                            (apply fn params))))
    (if (not (equal maybe-interrupted :interrupted))
        maybe-interrupted
      (display-warning "Replique" (format "%s was interrupted. If it took too long to complete, consider adding a .repliqueignore file to the directories you want to be ignored" fn))
      nil)))

(defun replique/maybe-change-ns-clj (tooling-repl clj-repl)
  (let ((ns-name (replique-context/clojure-find-ns))
        (repl-ns (replique/get clj-repl :ns)))
    (when (and ns-name repl-ns (not (string= ns-name (symbol-name repl-ns))))
      (replique/send-input-from-source-clj
       (format "(clojure.core/in-ns '%s)" ns-name) tooling-repl clj-repl))))

(defun replique/maybe-change-ns-cljs (tooling-repl cljs-repl)
  (let ((ns-name (replique-context/clojure-find-ns))
        (repl-ns (replique/get cljs-repl :ns)))
    (when (and ns-name repl-ns (not (string= ns-name (symbol-name repl-ns))))
      (replique/send-input-from-source-cljs
       (format "(replique.interactive/cljs-in-ns '%s)" ns-name) tooling-repl cljs-repl))))

(defun replique/maybe-change-ns-cljc (tooling-repl repl)
  (let ((ns-name (replique-context/clojure-find-ns))
        (repl-ns (replique/get repl :ns)))
    (when (and ns-name repl-ns (not (string= ns-name (symbol-name repl-ns))))
      (if (equal :cljs (replique/get repl :repl-type))
          (replique/send-input-from-source-cljs
           (format "(replique.interactive/cljs-in-ns '%s)" ns-name) tooling-repl repl)
        (replique/send-input-from-source-clj
         (format "(clojure.core/in-ns '%s)" ns-name) tooling-repl repl)))))

(defun replique/maybe-change-ns ()
  (let ((active-repl (replique/active-repl '(:clj :cljs)))
        (ns-name (replique-context/clojure-find-ns)))
    (replique/with-modes-dispatch
     (clojure-mode . 'replique/maybe-change-ns-clj)
     (clojurescript-mode . 'replique/maybe-change-ns-cljs)
     (clojurec-mode . 'replique/maybe-change-ns-cljc)
     (t . nil))))

(defun replique/column-number-at-pos (&optional pos)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (or pos (point)))
      (current-column))))

(defun replique/line-number-at-pos (&optional pos)
  (save-restriction
    (widen)
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))

(defun replique/eval-region (start end)
  "Eval the currently highlighted region"
  (interactive "r\nP")
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (input (filter-buffer-substring start end)))
    (when (> (length input) 0)
      (replique/maybe-change-ns)
      (let* ((line (replique/line-number-at-pos start))
             (column (1+ (replique/column-number-at-pos start)))
             (replique/eval-from-source-meta (replique/hash-map
                                              :line line
                                              :column column
                                              :url (replique/buffer-url
                                                    (buffer-file-name)))))
        (replique/send-input-from-source-dispatch input)))))

(defun replique/eval-last-sexp ()
  "Eval the previous sexp"
  (interactive)
  (replique/eval-region
   (save-excursion
     (backward-sexp 1)
     (replique-context/maybe-skip-dispatch-macro-or-quoted-backward)
     (replique-context/maybe-skip-read-discard-or-splice-forward)
     (point))
   (point)))

(defun replique/eval-defn ()
  "Eval the top level sexpr at point"
  (interactive)
  (let* ((expr-bounds (save-excursion
                        (let ((target-point (point)))
                          (skip-chars-backward replique-context/symbol-separator-re)
                          (let ((top-level (or (syntax-ppss-toplevel-pos
                                                (replique-context/syntax-ppss (point)))
                                               (point))))
                            (goto-char top-level)
                            (replique-context/maybe-skip-dispatch-macro-or-quoted-backward)
                            (replique-context/maybe-skip-read-discard-or-splice-forward)
                            (replique-context/unwrap-comment target-point)))))
         (expr (when expr-bounds
                 (buffer-substring-no-properties (car expr-bounds) (cdr expr-bounds)))))
    (when expr
      (replique/maybe-change-ns)
      (let* ((line (replique/line-number-at-pos (car expr-bounds)))
             (column (1+ (replique/column-number-at-pos (car expr-bounds))))
             (replique/eval-from-source-meta (replique/hash-map
                                              :line line
                                              :column column
                                              :url (replique/buffer-url
                                                    (buffer-file-name)))))
        (replique/send-input-from-source-dispatch
         expr)))))

(defun replique/load-url-clj (url props clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/send-input-from-source-clj
     (format "(replique.interactive/load-url \"%s\")" url)
     props clj-repl)))

(defun replique/load-url-cljs (url props cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/send-input-from-source-cljs
     (format "(replique.interactive/load-url \"%s\")" url)
     props cljs-repl)))

(defun replique/load-url-cljc (url props repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique/send-input-from-source-cljc
     (format "(replique.interactive/load-url \"%s\")" url)
     props repl)))

(defun replique/buffer-url (file-name)
  (if (string-match "^\\(.*\\.jar\\):\\(.+\\)" file-name)
      (format "jar:file:%s!/%s" (match-string 1 file-name) (match-string 2 file-name))
    (format "file://%s" file-name)))

(defun replique/load-file (file-path)
  (interactive (list (buffer-file-name)))
  (comint-check-source file-path)
  (replique/with-modes-dispatch
   (clojure-mode . (apply-partially 'replique/load-url-clj
                                    (replique/buffer-url file-path)))
   (clojurescript-mode . (apply-partially 'replique/load-url-cljs
                                          (replique/buffer-url file-path)))
   (clojurec-mode . (apply-partially 'replique/load-url-cljc
                                     (replique/buffer-url file-path)))
   (css-mode . (apply-partially 'replique/load-css file-path))
   (js2-mode . (apply-partially 'replique/load-js file-path))
   (stylus-mode . (apply-partially 'replique/load-stylus file-path))
   (scss-mode . (apply-partially 'replique/load-scss file-path))
   (sass-mode . (apply-partially 'replique/load-sass file-path))
   (less-css-mode . (apply-partially 'replique/load-less file-path))))

(defun replique/reload-all-clj (props clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/send-input-from-source-clj
     (format "(require '%s :reload-all)" (replique-context/clojure-find-ns))
     props clj-repl)))

(defun replique/reload-all-cljs (props cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/send-input-from-source-cljs
     (format "(replique.interactive/load-url \"%s\" :reload-all)"
             (replique/buffer-url (buffer-file-name)))
     props cljs-repl)))

(defun replique/reload-all-cljc (props repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((form (if (equal :cljs (replique/get repl :repl-type))
                    (format "(replique.interactive/load-url \"%s\" :reload-all)"
                            (replique/buffer-url (buffer-file-name)))
                  (format "(require '%s :reload-all)" (replique-context/clojure-find-ns)))))
      (replique/send-input-from-source-cljc form props repl))))

(defun replique/reload-all ()
  (interactive)
  (comint-check-source (buffer-file-name))
  (replique/with-modes-dispatch
   (clojure-mode . 'replique/reload-all-clj)
   (clojurescript-mode . 'replique/reload-all-cljs)
   (clojurec-mode . 'replique/reload-all-cljc)))

(defun replique/browser ()
  "Open a browser tab on the port the REPL is listening to"
  (interactive)
  (let ((cljs-repl (replique/active-repl :cljs)))
    (when (not cljs-repl)
      (user-error "No active Clojurescript REPL"))
    (browse-url (format "http://%s:%s"
                        (replique/get cljs-repl :http-host)
                        (replique/get cljs-repl :http-port)))))

(defun replique/switch-active-repl (repl-buff-name)
  "Switch the currently active REPL session"
  (interactive
   (let* ((active-repl (replique/active-repl :tooling t))
          (directory (replique/get active-repl :directory))
          (repls (seq-filter (lambda (repl)
                               (let ((repl-type (replique/get repl :repl-type))
                                     (d (replique/get repl :directory)))
                                 (and
                                  (equal d directory)
                                  (not (equal :tooling repl-type)))))
                             replique/repls))
          (repl-names (mapcar (lambda (repl)
                                (buffer-name (replique/get repl :buffer)))
                              repls)))
     (when (null repls)
       (user-error "No started REPL"))
     (let ((ivy-sort-functions-alist nil))
       (list (completing-read "Switch to REPL: "
                              (replique/presorted-completion-table repl-names) nil t)))))
  ;; All windows displaying the previously active repl are set to display the newly active
  ;; repl, unless the newly active repl is already a visible buffer
  (let* ((buffer (get-buffer repl-buff-name))
         (repl (replique/repl-by :buffer buffer))
         (prev-active-repl (replique/active-repl '(:clj :cljs)))
         (prev-active-buffer (replique/get prev-active-repl :buffer))
         (prev-active-windows (get-buffer-window-list prev-active-buffer)))
    (setq replique/repls (delete repl replique/repls))
    (setq replique/repls (push repl replique/repls))
    (when (not (seq-contains (replique/visible-buffers) buffer))
      (mapcar (lambda (window)
                (set-window-buffer window buffer))
              prev-active-windows))))

(defun replique/switch-active-process (proc-name)
  "Switch the currently active JVM process"
  (interactive
   (let* ((tooling-repls (replique/repls-by :repl-type :tooling))
          (directories (mapcar (lambda (repl) (replique/get repl :directory))
                               tooling-repls)))
     (when (not (car directories))
       (user-error "No started REPL"))
     (let ((ivy-sort-functions-alist nil))
       (list (completing-read "Switch to process: "
                              (replique/presorted-completion-table directories)
                              nil t)))))
  (let* ((new-active-repls (replique/repls-by :directory proc-name))
         (prev-active-tooling-repl (replique/active-repl :tooling))
         (prev-active-directory (replique/get prev-active-tooling-repl :directory))
         (prev-repls (replique/repls-by :directory prev-active-directory))
         (prev-repl-types (mapcar (lambda (repl) (replique/get repl :repl-type)) prev-repls)))
    ;; Buffer replacement is disabled for now, until we find a better user experience
    (comment
     ;; All windows displaying the previously active repls are set to display the newly active
     ;; repls (repl with the same repl-type and same position in the replique/repls list)
     (dolist (repl-type prev-repl-types)
       (let* ((prev-active-repl (replique/repl-by :directory prev-active-directory
                                                  :repl-type repl-type))
              (prev-buffer (replique/get prev-active-repl :buffer))
              (new-active-repl (replique/repl-by
                                :directory proc-name
                                :repl-type repl-type))
              (new-buffer (replique/get new-active-repl :buffer))
              (windows (get-buffer-window-list prev-buffer)))
         (when new-buffer
           (dolist (window windows)
             (set-window-buffer window new-buffer))))))
    (dolist (repl (seq-reverse new-active-repls))
      (setq replique/repls (delete repl replique/repls))
      (setq replique/repls (push repl replique/repls)))))

(defun replique/list-namespaces (tooling-repl repl-env)
  (let ((resp (replique/send-tooling-msg
               tooling-repl
               (replique/hash-map :type :list-namespaces
                                  :repl-env repl-env))))
    (let ((err (replique/get resp :error)))
      (when err
        (message "%s" (replique-pprint/pprint-error-str err))
        (message "list-namespaces failed")))
    resp))

(defun replique/cljs-repl ()
  "Start a Clojurescript REPL in the currently active Clojure REPL"
  (interactive)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (clj-repl (replique/active-repl :clj)))
    (when (not clj-repl)
      (user-error "No active Clojure REPL"))
    (let* ((main-cljs-namespaces (replique/get tooling-repl :main-cljs-namespaces))
           (main-cljs-namespace (when main-cljs-namespaces
                                  (replique/return-value-on-quit
                                   nil
                                   (completing-read "Main Clojurescript namespace: "
                                                    main-cljs-namespaces
                                                    nil t)))))
      (if main-cljs-namespace
          (replique/send-input-from-source-clj
           (format "(replique.interactive/cljs-repl '%s)" main-cljs-namespace)
           tooling-repl clj-repl)
        (replique/send-input-from-source-clj
         "(replique.interactive/cljs-repl)" tooling-repl clj-repl)))))

(defun replique/cljs-repl-nashorn ()
  "Start a Clojurescript Nashorn REPL in the currently active Clojure REPL"
  (interactive)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (clj-repl (replique/active-repl :clj)))
    (when (not clj-repl)
      (user-error "No active Clojure REPL"))
    (replique/send-input-from-source-clj
     "(replique.interactive/cljs-repl-nashorn)" tooling-repl clj-repl)))

(defun replique/output-main-js-file (output-to &optional main-ns)
  "Write a main javascript file to disk. The main javascript file acts as an entry point to your application and contains code to connect to the Clojurescript REPL"
  (interactive (list nil))
  (let ((tooling-repl (replique/active-repl :tooling t)))
    (when-let ((output-to (or
                           output-to
                           (read-file-name "Output main js file to: "
                                           (replique/get tooling-repl :directory)))))
      (let ((output-to (file-truename output-to)))
        (when (file-directory-p output-to)
          (user-error "%s is a directory" output-to))
        (when (or (not (file-exists-p output-to))
                  (yes-or-no-p (format "Override %s?" output-to)))
          (let ((resp (if main-ns
                          (replique/hash-map :namespaces nil)
                        (replique/list-namespaces tooling-repl :replique/cljs))))
            (when (not (replique/get resp :error))
              (let* ((namespaces (replique/get resp :namespaces))
                     (main-ns (cond (main-ns main-ns)
                                    ((seq-empty-p namespaces) nil)
                                    (t (replique/return-value-on-quit
                                        nil
                                        (completing-read "Main Clojurescript namespace: "
                                                         namespaces nil t nil nil '(nil))))))
                     (resp (replique/send-tooling-msg
                            tooling-repl
                            (replique/hash-map :type :output-main-js-files
                                               :repl-env :replique/cljs
                                               :output-to output-to
                                               :main-ns main-ns))))
                (let ((err (replique/get resp :error)))
                  (if err
                      (progn
                        (message "%s" (replique-pprint/pprint-error-str err))
                        (message "output-main-js-file failed"))
                    (message "Main javascript file written to: %s" output-to)
                    (when main-ns
                      (if-let ((main-cljs-namescapes
                                (replique/get tooling-repl :main-cljs-namespaces)))
                          (puthash :main-cljs-namespaces
                                   (cons main-ns main-cljs-namescapes)
                                   tooling-repl)
                        (puthash :main-cljs-namespaces (list main-ns) tooling-repl)))))))))))))

;; I don't know why goog/deps.js is needed here but is not needed in the init script returned
;; by the cljs repl
;; Wait a second before connecting to the cljs repl because there is no way to plug into
;; the goog.require loading mechanism
(defun replique/cljs-repl-connection-script ()
  (interactive)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (http-host (replique/get tooling-repl :http-host))
         (http-port (replique/get tooling-repl :http-port))
         (script (format "(function() {
  var scripts = [];
  var makeScript = function() {
    var script = document.createElement(\"script\");
    scripts.push(script);
    return script;
  }
  var execScripts = function() {
    if(scripts.length > 0) {
      scripts[0].onload = execScripts;
      document.body.appendChild(scripts.shift());
    }
  }
  makeScript().src = \"http://%s:%s/goog/base.js\";
  makeScript().src = \"http://%s:%s/goog/deps.js\";
  makeScript().src = \"http://%s:%s/cljs_deps.js\";
  makeScript().src = \"http://%s:%s/replique/cljs_env/bootstrap.js\";
  makeScript().textContent = \"goog.require(\\\"replique.cljs_env.repl\\\"); goog.require(\\\"replique.cljs_env.browser\\\");\";
  execScripts();
  setTimeout(function(){replique.cljs_env.repl.connect(\"http://%s:%s\");}, 1000);
})();"
                         http-host http-port
                         http-host http-port
                         http-host http-port
                         http-host http-port
                         http-host http-port)))
    (kill-new script)
    (message script)))

(defun replique/list-css (tooling-repl cljs-repl)
  (let* ((repl-env (replique/get cljs-repl :repl-env))
         (resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :list-css
                                   :repl-env repl-env))))
    (let ((err (replique/get resp :error)))
      (when err
        (message "%s" (replique-pprint/pprint-error-str err))
        (message "list-css failed")))
    resp))

(defun replique/asset-url-matches? (file-name url)
  (let ((url-filename (file-name-nondirectory (url-filename (url-generic-parse-url url)))))
    (equal file-name url-filename)))

(defvar replique/css-prefered-files (replique/hash-map))

;; Choose a css url either by looking into replique/css-prefered-files (if the user already
;; has chosen one before) or by prompting the user
(defun replique/select-css-url (file-path css-urls)
  (or (replique/get replique/css-prefered-files file-path)
      (let ((selected-url (completing-read "Select the css file to reload: " css-urls nil t)))
        (setq replique/css-prefered-files (replique/assoc replique/css-prefered-files
                                                          file-path selected-url))
        selected-url)))

(defun replique/load-css (file-path tooling-repl cljs-repl)
  (when (not cljs-repl)
    (user-error "No active Clojurescript REPL"))
  (let* ((repl-env (replique/get cljs-repl :repl-env))
         (resp (replique/list-css tooling-repl cljs-repl)))
    (when (not (replique/get resp :error))
      (let* ((css-urls (replique/get resp :css-urls))
             (css-urls (seq-filter (apply-partially 'replique/asset-url-matches?
                                                    (file-name-nondirectory file-path))
                                   css-urls))
             (selected-url (cond ((equal 0 (length css-urls))
                                  ;; No stylesheet matches, do nothing
                                  nil)
                                 ((equal 1 (length css-urls))
                                  ;; Reload the stylesheet that matches
                                  (car css-urls))
                                 (t (replique/select-css-url file-path css-urls)))))
        (if (null selected-url)
            (message "Could not find a css file to reload")
          (let ((resp (replique/send-tooling-msg
                       tooling-repl
                       (replique/hash-map :type :load-css
                                          :repl-env repl-env
                                          :url selected-url))))
            (let ((err (replique/get resp :error)))
              (if err
                  (progn
                    (message "%s" (replique-pprint/pprint-error-str err))
                    (message "load-css %s: failed" file-path))
                (message "load-css %s: done" file-path)))))))))

(defun replique/load-js (file-path tooling-repl cljs-repl)
  (when (not cljs-repl)
    (user-error "No active Clojurescript REPL"))
  (let* ((repl-env (replique/get cljs-repl :repl-env))
         (resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :load-js
                                   :repl-env repl-env
                                   :file-path file-path))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-pprint/pprint-error-str err))
            (message "load-js %s: failed" file-path))
        (message "load-js %s: done" file-path)))))

;; {directory {css-preprocessor-type {:output-files {output-file main-source-file}
;;                                   {:last-selected-output-file output-file}}}}
;; When the user triggers a compilation, he is prompted for an output css file. The
;; compilation is performed using the main source file.
(defvar replique/css-preprocessors-output-files (replique/hash-map))

;; Move x to the first position in coll
(defun replique/move-to-front (coll x)
  (when (and coll (seq-contains coll x))
    (thread-last coll
      (seq-remove (lambda (y) (equal x y)))
      (cons x))))

(defcustom replique/stylus-executable "stylus"
  "Stylus executable path"
  :type 'string
  :group 'replique)

(defun replique/stylus-args-builder-default (input output)
  (list "-m" "--sourcemap-inline" "--out" output input))

(defvar replique/stylus-args-builder 'replique/stylus-args-builder-default
  "Function that returns the command to use when compiling stylus files")

(defcustom replique/sass-executable "sass"
  "Sass executable path"
  :type 'string
  :group 'replique)

(defun replique/sass-args-builder-default (input output)
  (list "--sourcemap=inline" "--default-encoding" "utf-8" input output))

(defvar replique/sass-args-builder 'replique/sass-args-builder-default
  "Function that returns the command to use when compiling sass files")

(defcustom replique/scss-executable "scss"
  "Scss executable path"
  :type 'string
  :group 'replique)

(defun replique/scss-args-builder-default (input output)
  (list "--sourcemap=inline" "--default-encoding" "utf-8" input output))

(defvar replique/scss-args-builder 'replique/scss-args-builder-default
  "Function that returns the command to use when compiling scss files")

(defcustom replique/less-executable "lessc"
  "Scss executable path"
  :type 'string
  :group 'replique)

(defun replique/less-args-builder-default (input output)
  (list "--source-map" "--source-map-less-inline" input output))

(defvar replique/less-args-builder 'replique/less-args-builder-default
  "Function that returns the command to use when compiling less files")

(defun replique/load-css-preprocessor
    (type executable args-builder file-path tooling-repl cljs-repl)
  (when (not cljs-repl)
    (user-error "No active Clojurescript REPL"))
  (let* ((directory (replique/get tooling-repl :directory))
         (output-files (replique/get-in replique/css-preprocessors-output-files
                                        `[,directory ,type :output-files]))
         (output-files (replique/keys output-files))
         (last-selected-output-file (replique/get-in
                                     replique/css-preprocessors-output-files
                                     `[,directory ,type :last-selected-output-file]))
         (output-files (replique/move-to-front output-files last-selected-output-file))
         ;; We don't want ivy to sort candidates by alphabetical order
         (ivy-sort-functions-alist nil)
         (output (if output-files
                     (let ((ivy-sort-functions-alist nil))
                       (completing-read "Compile to file: "
                                        ;; All candidates are lists in order for completing-read
                                        ;; to keep the order of the candidates
                                        (replique/presorted-completion-table
                                         (append output-files '("*new-file*")))
                                        nil t))
                   "*new-file*"))
         (is-new-file (equal output "*new-file*"))
         (output (if is-new-file
                     (read-file-name "Compile to file: "
                                     (replique/get tooling-repl :directory)
                                     nil t)
                   output))
         (main-source-file (if is-new-file
                               file-path
                             (replique/get-in replique/css-preprocessors-output-files
                                              `[,directory ,type :output-files ,output])))
         (output (file-truename output))
         (command-result 0))
    (when (file-directory-p output)
      (user-error "%s is a directory" output))
    (when (or (not is-new-file)
              (not (file-exists-p output))
              (yes-or-no-p (format "Override %s?" output)))
      (with-temp-buffer
        (setq command-result (apply 'call-process executable nil t nil
                                    (funcall args-builder main-source-file output)))
        (when (not (equal 0 command-result))
          (ansi-color-apply-on-region (point-min) (point-max))
          (message (buffer-substring (point-min) (point-max)))))
      (when (equal 0 command-result)
        (setq replique/css-preprocessors-output-files
              (replique/update-in
               replique/css-preprocessors-output-files
               `[,directory ,type]
               (lambda (x)
                 (thread-first x
                   (replique/assoc :last-selected-output-file output)
                   (replique/assoc-in `[:output-files ,output] main-source-file)))))
        (replique/load-css output tooling-repl cljs-repl)))))

(defun replique/load-stylus (file-path tooling-repl cljs-repl)
  (replique/load-css-preprocessor :stylus replique/stylus-executable
                                  replique/stylus-args-builder
                                  file-path tooling-repl cljs-repl))


(defun replique/load-scss (file-path tooling-repl cljs-repl)
  (replique/load-css-preprocessor :scss replique/scss-executable
                                  replique/scss-args-builder
                                  file-path tooling-repl cljs-repl))

(defun replique/load-sass (file-path tooling-repl cljs-repl)
  (replique/load-css-preprocessor :sass replique/sass-executable
                                  replique/sass-args-builder
                                  file-path tooling-repl cljs-repl))

(defun replique/load-less (file-path tooling-repl cljs-repl)
  (replique/load-css-preprocessor :less replique/less-executable
                                  replique/less-args-builder
                                  file-path tooling-repl cljs-repl))

(defun replique/jump-to-definition* (symbol tooling-repl repl-env ns)
  (let* ((resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :meta
                                   :repl-env repl-env
                                   :ns ns
                                   :symbol symbol
                                   :context (replique-context/get-context
                                             tooling-repl ns repl-env)))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-pprint/pprint-error-str err))
            (message "jump-to-definition failed with symbol: %s" symbol))
        (let* ((meta (replique/get resp :meta))
               (file (replique/get-in resp [:meta :file]))
               (files (replique/get-in resp [:meta :files]))
               (local? (replique/get-in resp [:meta :local?]))
               (point-start (replique/get-in resp [:meta :point-start]))
               (line (replique/get-in resp [:meta :line]))
               (column (replique/get-in resp [:meta :column])))
          (cond ((not (replique-transit/is-small-number? line))
                 (message "Cannot handle line number %s" line)
                 (message "jump-to-definition failed with symbol: %s" symbol))
                ((not (replique-transit/is-small-number? column))
                 (message "Cannot handle column number %s" column)
                 (message "jump-to-definition failed with symbol: %s" symbol))
                ((and local? point-start)
                 (xref-push-marker-stack)
                 (goto-char point-start))
                (t
                 (let ((file (if (and files (null file))
                                 (completing-read "Jump to file: " files nil t)
                               file)))
                   (when-let (buff (replique-resources/find-file file))
                     (xref-push-marker-stack)
                     (pop-to-buffer-same-window buff)
                     (goto-char (point-min))
                     (when line
                       (forward-line (1- line))
                       (when column
                         (move-to-column column))))))))))))

(defun replique/jump-to-definition-session (symbol repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (repl-env (replique/get repl :repl-env))
         (ns (symbol-name (replique/get repl :ns))))
    (replique/jump-to-definition* symbol tooling-repl repl-env ns)))

(defun replique/jump-to-definition-clj (symbol tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/jump-to-definition*
     symbol tooling-repl (replique/get clj-repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/jump-to-definition-cljs (symbol tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/jump-to-definition*
     symbol tooling-repl (replique/get cljs-repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/jump-to-definition-cljc (symbol tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique/jump-to-definition*
     symbol tooling-repl (replique/get repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/jump-to-definition (symbol)
  "Jump to symbol at point definition, if the metadata for the symbol at point contains enough information"
  (interactive (list (let ((sym (symbol-at-point)))
                       (when sym (symbol-name sym)))))
  (replique/with-modes-dispatch
   (replique/mode . (apply-partially 'replique/jump-to-definition-session symbol))
   (clojure-mode . (apply-partially 'replique/jump-to-definition-clj symbol))
   (clojurescript-mode . (apply-partially 'replique/jump-to-definition-cljs symbol))
   (clojurec-mode . (apply-partially 'replique/jump-to-definition-cljc symbol))
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(comment
 (defun replique/install-node-deps ()
   "Install Clojurescript :npm-deps"
   (interactive)
   (let* ((tooling-repl (replique/active-repl :tooling t))
          (active-repl (replique/active-repl '(:clj :cljs))))
     (when (not active-repl)
       (user-error "No active Clojure REPL"))
     (replique/send-input-from-source-session "(replique.interactive/install-node-deps!)"
                                              active-repl)))
 )

(defconst replique/version "0.0.15-SNAPSHOT")

(defcustom replique/prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)

(defcustom replique/prompt-read-only t
  "Whether to make the prompt read-only or not"
  :type 'boolean
  :group 'replique)

(defcustom replique/clojure-bin "clojure"
  "Name of the clojure binary to be used. The binary will be searched in exec-path."
  :type 'string
  :group 'replique)

(defcustom replique/replique-coords "{:git/url \"git@github.com:EwenG/replique.git\" :sha \"267c48f57334f1028b9cab28b24c801de92d5c1c\"}"
  "The tools.deps coordinates of the Replique dependency."
  :type 'string
  :group 'replique)

(defcustom replique/start-script-extensions '("sh" "bat")
  "The allowed tools.deps start script extensions"
  :type 'list
  :group 'replique)

(defcustom replique/company-tooltip-align-annotations t
  "See company-tooltip-align-annotations"
  :type 'boolean
  :group 'replique)

(defcustom replique/display-output-in-minibuffer t
  "Whether the output of the active REPL is displayed in the minibuffer when the REPL buffer 
is not visible"
  :type 'boolean
  :group 'replique)

(defface replique/minibuffer-output-repl-name-face
  '((t :slant italic :height 110 :overline t))
  "Face for displaying the name of the REPL the output being displayed came from"
  :group 'replique )

(defcustom replique/clj-comment '("comment" "clojure.core/comment")
  "The symbols considered to be starting a clojure comment block. Used by replique/eval-defn when
unwrapping a top level comment block "
  :type 'list
  :group 'replique)

(defvar replique/mode-hook '()
  "Hook for customizing replique mode.")

(defvar replique/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-m" 'replique/comint-send-input)
    (define-key map "\C-c\M-n" 'replique/in-ns)
    (define-key map "\C-c\C-r" 'replique/switch-active-repl)
    (define-key map "\M-." 'replique/jump-to-definition)
    (define-key map "\C-c\C-c" 'replique/pprint)
    (define-key map "\C-c\C-w" 'replique/watch)
    map))

(defun replique/commons ())

(define-derived-mode replique/mode comint-mode "Replique"
  "Commands:\\<replique/mode-map>"
  (setq-local comint-prompt-regexp replique/prompt)
  (setq-local comint-prompt-read-only replique/prompt-read-only)
  (setq-local comint-output-filter-functions (remove 'comint-watch-for-password-prompt
                                                     comint-output-filter-functions))
  (setq-local comint-output-filter-functions (add-to-list
                                              'comint-output-filter-functions
                                              'replique-input/comint-watch-for-password-prompt
                                              t))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (set-syntax-table clojure-mode-syntax-table)
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'replique/company-backend)
    (setq-local company-tooltip-align-annotations
                replique/company-tooltip-align-annotations))
  (add-function :before-until (local 'eldoc-documentation-function)
                'replique/eldoc-documentation-function))

(defvar replique/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-r" 'replique/eval-region)
    (define-key map "\C-x\C-e" 'replique/eval-last-sexp)
    (define-key map "\C-\M-x" 'replique/eval-defn)
    (define-key map "\C-c\C-l" 'replique/load-file)
    (define-key map "\C-c\M-n" 'replique/in-ns)
    (define-key map "\C-c\C-r" 'replique/switch-active-repl)
    (define-key map "\M-." 'replique/jump-to-definition)
    (define-key map "\M-," 'xref-pop-marker-stack)
    (define-key map "\C-c\C-c" 'replique/pprint)
    (define-key map "\C-c\C-u" 'replique/find-usage)
    (define-key map "\C-c\C-w" 'replique/watch)
    (easy-menu-define replique/minor-mode-menu map
      "Replique Minor Mode Menu"
      '("Replique"
        ["Eval region" replique/eval-region t]
        ["Eval last sexp" replique/eval-last-sexp t]
        ["Eval defn" replique/eval-defn t]
        "--"
        ["Load file" replique/load-file t]
        "--"
        ["Set REPL ns" replique/in-ns t]
        "--"
        ["Switch active REPL" replique/switch-active-repl t]
        "--"
        ["Jump to definition" replique/jump-to-definition t]
        "--"
        ["Pretty print expression at point" replique/pprint t]
        "--"
        ["Find usage of thing at point" replique/find-usage t]
        "--"
        ["Watch a var or a REPL printed-data/results using a dedicated buffer" replique/watch t]
        ))
    map))

;;;###autoload
(define-minor-mode replique/minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique/minor-mode-map}"
  :lighter "Replique" :keymap replique/minor-mode-map
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'replique/company-backend)
    (setq-local company-tooltip-align-annotations
                replique/company-tooltip-align-annotations))
  (add-function :before-until (local 'eldoc-documentation-function)
                'replique/eldoc-documentation-function))

(defun replique/classpath ()
  (interactive)
  (let* ((tooling-repl (replique/active-repl :tooling))
         (default-directory (replique/get tooling-repl :directory))
         (spath-script (completing-read
                        "-Spath script: "
                        (thread-last (directory-files default-directory)
                          (seq-filter 'replique/script-file-predicate)
                          (cons "*default*"))
                        nil
                        t))
         (spath-script (when (not (equal spath-script "*default*")) spath-script))
         (spath-command (replique-cli/spath-command replique/clojure-bin
                                                    replique/replique-coords
                                                    spath-script))
         (_ (message "Updating the classpath with command:\n%s" (string-join spath-command " ")))
         (classpath (with-temp-buffer
                      (let ((exit-code (apply 'call-process (car spath-command)
                                              nil t nil (cdr spath-command))))
                        (if (not (equal exit-code 0))
                            (error (buffer-substring-no-properties (point-min) (point-max)))
                          ;; The output may includes "downloading ..." prints. Thus we
                          ;; only keep the last line 
                          (goto-char (point-max))
                          (re-search-backward "^.+" nil t)
                          (buffer-substring-no-properties (point) (point-max))))))
         (resp (replique/send-tooling-msg
                tooling-repl (replique/hash-map :type :update-classpath
                                                :repl-env :replique/clj
                                                :classpath classpath))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-pprint/pprint-error-str err))
            (message "replique/classpath failed"))
        (message "Classpath updated")))))

;; Not used anymore but kept just in case ...
;; lein update-in :plugins conj "[replique/replique \"0.0.1-SNAPSHOT\"]" -- trampoline replique localhost 9000
(comment
 (defun replique/lein-command (host port directory)
   `(,(executable-find replique/lein-script)
     ,@(when replique/offline-mode-flag '("-o"))
     "update-in" ":plugins" "conj"
     ,(format "[replique/replique \"%s\"]" replique/version)
     "--"
     "trampoline" "replique"
     ,(format "%s" host) ,(format "%s" port)
     ,(format "%s" directory))))

(defun replique/close-tooling-repl (tooling-repl)
  (let ((tooling-proc (replique/get tooling-repl :proc)))
    (when (process-live-p tooling-proc)
      ;; SIGINT, the jvm should stop, even if there is pending non daemon threads, shutdown
      ;; hooks should run
      (interrupt-process tooling-proc))
    (setq replique/repls (delete tooling-repl replique/repls))))

;; Close all the tooling repls that have no "session" REPL
(defun replique/maybe-close-tooling-repl (directory)
  (let ((other-repls (replique/repls-by-maybe-not-started :directory directory)))
    (when (seq-every-p (lambda (repl)
                         (equal :tooling (replique/get repl :repl-type)))
                       other-repls)
      (mapcar (lambda (tooling-repl)
                (replique/close-tooling-repl tooling-repl))
              other-repls))))

(defun replique/close-repl (repl kill-buffers)
  (let* ((buffer (replique/get repl :buffer))
         (directory (replique/get repl :directory))
         (proc (get-buffer-process buffer)))
    (when proc
      (set-process-sentinel proc nil)
      (delete-process proc))
    (replique-watch/kill-repl-watch-buffers repl)
    (if kill-buffers
        (kill-buffer buffer)
      (with-current-buffer buffer
        ;; Disable replique/mode
        (fundamental-mode)
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\nConnection broken by remote peer\n")))))
    (setq replique/repls (delete repl replique/repls))
    ;; If the only repl left is a tooling repl, let's close it
    (replique/maybe-close-tooling-repl directory)))

(defun replique/close-repls (directory kill-buffers)
  (let* ((repls (replique/repls-by-maybe-not-started :directory directory))
         (not-tooling-repls (seq-filter (lambda (repl)
                                          (not (equal :tooling (replique/get repl :repl-type))))
                                        repls))
         (tooling-repls (seq-filter (lambda (repl)
                                      (equal :tooling (replique/get repl :repl-type)))
                                    repls)))
    (dolist (repl not-tooling-repls)
      (replique/close-repl repl kill-buffers))
    (dolist (tooling-repl tooling-repls)
      (replique/close-tooling-repl tooling-repl))))

(defun replique/on-repl-close (buffer process event)
  (let ((closing-repl (replique/repl-by-maybe-not-started :buffer buffer)))
    (when closing-repl
      (cond ((string= "deleted\n" event)
             (replique/close-repl closing-repl t))
            ((string= "connection broken by remote peer\n" event)
             (replique/close-repl closing-repl nil))
            (t nil)))))

(defun replique/on-tooling-repl-close (network-proc-buffer directory process event)
  (cond ((string= "deleted\n" event)
         (replique/close-repls directory t))
        ((string= "connection broken by remote peer\n" event)
         (replique/close-repls directory nil))
        (t nil))
  (replique/kill-process-buffer network-proc-buffer))

(defun replique/check-clojure-bin (command)
  (when (not (executable-find command))
    "Error while starting the REPL. No clojure executable found in exec-path"))

(defun replique/process-filter-skip-repl-starting-output (proc callback)
  (let ((started-regexp "Replique listening on host \"\\(.*\\)\" and port \\([0-9]+\\)\nHTTP server listening on host \"\\(.*\\)\" and port \\([0-9]+\\)\n"))
    (set-process-filter
     proc (lambda (proc string)
            ;; REPL is started
            (if (string-match started-regexp string)
                (funcall callback
                         (match-string 1 string)
                         (string-to-number (match-string 2 string))
                         (match-string 3 string)
                         (string-to-number (match-string 4 string)))
              ;; REPL is still starting. Print all the init messages
              (message "%s" string))))))

(defun replique/is-main-js-file (f)
  (let* ((first-line "//main-js-file autogenerated by replique")
         (check-length (length first-line)))
    (and (file-exists-p f)
         (condition-case err
             (with-temp-buffer
               (insert-file-contents f nil 0 check-length)
               (equal (buffer-substring-no-properties 1 (+ 1 check-length)) first-line))
           (error nil)))))

(defun replique/default-dir-predicate (d warning-type)
  (when (not (string-prefix-p "." (file-name-base d)))
    (if (file-readable-p d)
        (not (file-exists-p (expand-file-name ".repliqueignore" d)))
      (when warning-type
        (display-warning warning-type (format "%s is not a readable directory" d)))
      nil)))

(defun replique/find-main-js-files (directory)
  (message "Looking for main js files ...")
  (let ((js-files (replique/directory-files-recursively
                   directory "\\.js$"
                   'replique/default-dir-predicate
                   "replique/refresh-main-js-file"))
        (main-js-files nil))
    (dolist (f js-files)
      (when (and (file-readable-p f) (replique/is-main-js-file f))
        (setq main-js-files (cons f main-js-files))))
    (message "Looking for main js files ... %s file(s) found" (length main-js-files))
    main-js-files))

(defun replique/refresh-main-js-files (main-js-files host port)
  (message "Refreshing main js files ...")
  (let ((namespaces nil)
        (nb-refreshed 0))
    (dolist (f main-js-files)
      (when (and (file-readable-p f) (file-writable-p f))
        (setq nb-refreshed (+ nb-refreshed 1))
        (with-temp-file f
          (insert-file-contents f)
          (save-match-data
            (when (re-search-forward "var port = \'[0-9]+\';" nil t)
              (replace-match (format "var port = \'%s\';" port)))
            (when (re-search-forward "var host = \'.+\';" nil t)
              (replace-match (format "var host = \'%s\';" host)))
            (when (re-search-forward "var mainNs = \'\\(.*\\)\';$" nil t)
              (when (match-string-no-properties 1)
                (setq namespaces (cons (match-string-no-properties 1) namespaces)))))
          (buffer-substring-no-properties 1 (point-max)))))
    (message "Refreshing main js files ... %s file(s) refreshed" nb-refreshed)
    namespaces))

;; We do not use (message ...) because messages may be received splitted and the message fn
;; always prints a new line. Instead we set a text property on the last char printed in the
;; messages buffer in order to detect two messages coming from the same process. When necessary,
;; messages are concatenated by direclty inserting them in the messages buffer
(defun replique/proc-message-fn (proc string)
  (when (not (eq "" string))
    (with-current-buffer (messages-buffer)
      (let ((inhibit-read-only t)
            (pname (process-name proc)))
        (save-excursion
          (goto-char (point-max))
          (if (and (> (point) 1)
                   (equal (get-text-property (1- (point)) 'replique/proc-out) pname))
              (insert string)
            (insert (format "\nProcess %s:\n%s" pname string)))
          (add-text-properties (1- (point)) (point) `(replique/proc-out
                                                      ,pname rear-nonsticky ,t)))))
    (replique/message-nolog
     (format
      "%s\n%s" string
      (propertize (process-name proc) 'face 'replique/minibuffer-output-repl-name-face)))))

(defun replique/make-tooling-repl (directory repl-script host port)
  (let* ((default-directory directory)
         (repl-cmd (replique-cli/clojure-command
                    replique/clojure-bin replique/replique-coords
                    host port directory repl-script))
         (clj-script-error (replique/check-clojure-bin (car repl-cmd))))
    (when clj-script-error
      (user-error clj-script-error))
    (message "Starting REPL with Clojure command:\n%s" (string-join repl-cmd " "))
    (let* ((proc (apply 'start-process directory nil (car repl-cmd) (cdr repl-cmd)))
           (tooling-repl (replique/hash-map
                          :directory directory
                          :repl-type :tooling
                          :proc proc
                          :host host
                          :port port
                          :http-host nil
                          :http-port nil
                          :repl-cmd repl-cmd
                          :ref-watchers (replique/hash-map)
                          :main-cljs-namespaces nil
                          :started? nil)))
      ;; Perform the starting REPLs cleaning in case of an error when starting the process
      (set-process-sentinel
       proc
       (lambda (process event)
         (replique/close-repls directory t)))
      (push tooling-repl replique/repls)
      (replique/make-repl-starting repl-cmd directory host port)
      (let (;; Search for the main js files while the process is starting
            (main-js-files (replique/with-interrupt-warning 'replique/find-main-js-files directory)))
        (replique/process-filter-skip-repl-starting-output
         proc
         (lambda (host port http-host http-port)
           ;; Print process messages
           (set-process-filter proc 'replique/proc-message-fn)
           (let* ((network-proc-buff (generate-new-buffer (format " *%s*" directory)))
                  (network-proc (open-network-stream directory nil host port))
                  (timeout (run-at-time
                            2 nil (lambda (proc)
                                    (message "Error while starting the REPL. The port number may already be used by another process")
                                    (when (process-live-p proc)
                                      (interrupt-process proc)))
                            proc)))
             (puthash :port port tooling-repl)
             (puthash :host host tooling-repl)
             (puthash :http-port http-port tooling-repl)
             (puthash :http-host http-host tooling-repl)
             (puthash :network-proc network-proc tooling-repl)
             (set-process-sentinel
              network-proc
              (apply-partially 'replique/on-tooling-repl-close network-proc-buff directory))
             ;; The sentinel on the network proc already handles the cleaning of REPLs
             (set-process-sentinel proc nil)
             (replique/process-filter-read
              network-proc network-proc-buff
              ;; The cljs-compile-path is not used anymore, but is kept as an example of reading
              ;; informations about the clojure process at init time
              (lambda (cljs-compile-path)
                (cancel-timer timeout)
                (let* ((cljs-compile-path (replique-transit/decode cljs-compile-path))
                       (starting-repls (replique/repls-by-maybe-not-started :directory directory)))
                  (replique/process-filter-read
                   network-proc network-proc-buff 'replique/dispatch-tooling-msg)
                  (puthash :started? t tooling-repl)
                  (dolist (starting-repl starting-repls)
                    (condition-case err
                        (when (not (eq starting-repl tooling-repl))
                          (if (buffer-live-p (replique/get starting-repl :buffer))
                              (replique/start-repl tooling-repl starting-repl)
                            (replique/close-repl starting-repl t)))
                      (error
                       (replique/close-repls directory t)
                       ;; rethrow the error
                       (signal (car err) (cdr err))))))))
             ;; No need to wait for the return value of shared-tooling-repl
             ;; since it does not print anything
             (process-send-string
              network-proc "(replique.repl/shared-tooling-repl :elisp)\n")
             (process-send-string network-proc "replique.utils/cljs-compile-path\n")
             ;; Refresh main js files as soon as we know the http-port number
             (when main-js-files
               (when-let ((main-cljs-namespaces (replique/refresh-main-js-files
                                                 main-js-files http-host http-port)))
                 (puthash :main-cljs-namespaces main-cljs-namespaces tooling-repl))))))))))

(defun replique/close-process (directory)
  "Close all the REPL sessions associated with a JVM process"
  (interactive
   (let* ((tooling-repls (replique/repls-by-maybe-not-started
                          :repl-type :tooling :error-on-nil t))
          (directories (mapcar (lambda (repl) (replique/get repl :directory)) tooling-repls)))
     (list (completing-read "Close process: " directories nil t))))
  (replique/close-repls directory t))

(defun replique/time-diff (t1 t2)
  (float-time (time-subtract t1 t2)))

(defun replique/is-recent-output (now output)
  (let ((diff (replique/time-diff now (car output))))
    (< (float-time diff) 1)))

;;seq-take-while
(defun replique/concat-recent-output (recent-output)
  (let* ((recent-elements (thread-last (ring-elements recent-output)
                            (seq-take-while (apply-partially 'replique/is-recent-output
                                                             (current-time)))
                            (mapcar 'cadr)
                            reverse))
         (recent-elements (mapcar 'ansi-color-filter-apply recent-elements)))
    (apply 'concat recent-elements)))

(defun replique/comint-output-filter (process string)
  "Wrap comint-output-filter in order to display the output of hidden active REPLs in the 
minibuffer"
  (let* ((buffer (process-buffer process))
         (repl (replique/repl-by-maybe-not-started :buffer buffer))
         (recent-output (replique/get repl :recent-output))
         (active-repl-clj (replique/active-repl :clj))
         (active-repl-cljs (replique/active-repl :cljs)))
    (ring-insert recent-output (list (current-time) string))
    (when (and (or (eq active-repl-clj repl)
                   (eq active-repl-cljs repl))
               ;; do not print when a minibuffer is active
               (null (active-minibuffer-window))
               (not (seq-contains (replique/visible-buffers) buffer))
               replique/display-output-in-minibuffer)
      (replique/message-nolog
       (format "%s\n%s"
               (replique/concat-recent-output recent-output)
               (propertize (buffer-name buffer)
                           'face 'replique/minibuffer-output-repl-name-face))))
    (comint-output-filter process string)))

(defun replique/make-comint (proc buffer)
  (with-current-buffer buffer
    (unless (derived-mode-p 'comint-mode)
      (comint-mode))
    (set-process-filter proc 'replique/comint-output-filter)
    (setq-local comint-ptyp process-connection-type)
    ;; Jump to the end, and set the process mark.
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (run-hooks 'comint-exec-hook)
    buffer))

(defun replique/start-repl (tooling-repl starting-repl)
  (let* ((directory (replique/get tooling-repl :directory))
         (host (replique/get tooling-repl :host))
         (port (replique/get tooling-repl :port))
         (http-host (replique/get tooling-repl :http-host))
         (http-port (replique/get tooling-repl :http-port))
         (repl-buffer (replique/get starting-repl :buffer))
         (repl-type (replique/get starting-repl :repl-type))
         (proc (open-network-stream (buffer-name repl-buffer) repl-buffer host port))
         (repl-cmd (format "(replique.watch/repl)\n")))
    (set-process-sentinel proc (apply-partially 'replique/on-repl-close repl-buffer))
    (replique/process-filter-read
     proc repl-buffer
     (lambda (resp)
       (if (buffer-live-p repl-buffer)
           (let* ((resp (replique-transit/decode resp))
                  (session (replique/get resp :client)))
             (with-current-buffer repl-buffer
               (erase-buffer)
               (rename-buffer (generate-new-buffer-name
                               (replique/buffer-name directory repl-type session))))
             (set-process-filter proc nil)
             (replique/make-comint proc repl-buffer)
             (comment (set-buffer repl-buffer))
             (replique/mode)
             (process-send-string proc repl-cmd)
             (puthash :host host starting-repl)
             (puthash :port port starting-repl)
             (puthash :http-host http-host starting-repl)
             (puthash :http-port http-port starting-repl)
             (puthash :session session starting-repl)
             (puthash :ns 'user starting-repl)
             (puthash :started? t starting-repl)
             (comment (display-buffer repl-buffer)))
         (when (process-live-p proc)
           (interrupt-process proc)))))
    (process-send-string proc "clojure.core.server/*session*\n")))

(defun replique/make-repl-starting (repl-cmd directory host port)
  (let* ((repl-buffer (generate-new-buffer (replique/buffer-name directory :clj)))
         (repl (replique/hash-map :directory directory
                                  :host host
                                  :port port
                                  :http-host nil
                                  :http-port nil
                                  :repl-type :clj
                                  :repl-env :replique/clj
                                  :buffer repl-buffer
                                  :recent-output (make-ring 10)
                                  :params nil
                                  :started? nil)))
    (with-current-buffer repl-buffer
      (insert (format "Clojure REPL starting...\n\nDirectory:\n%s\n\nCommand:\n%s"
                      directory (string-join repl-cmd " "))))
    (push repl replique/repls)
    (display-buffer repl-buffer)
    (redisplay)
    repl))

(defun replique/buffer-name (directory repl-type &optional session)
  (if session
      (let* (;; client id number 1 is used by the tooling REPL
             (session-dec (ignore-errors (- (string-to-number session) 1)))
             (session (or session-dec session)))
        (format "*replique*%s*%s*%s*"
                (file-name-nondirectory (directory-file-name directory))
                (replique/keyword-to-string repl-type)
                ;; session number 1 is used by the tooling REPL
                session))
    (format "*replique*%s*%s*"
            (file-name-nondirectory (directory-file-name directory))
            (replique/keyword-to-string repl-type))))

(defun replique/normalize-directory-name (directory)
  (file-name-as-directory (expand-file-name directory)))

(defun replique/script-file-predicate (f)
  (seq-contains replique/start-script-extensions (file-name-extension f)))

;;;###autoload
(defun replique/repl (directory repl-script &optional host port)
  "Start a REPL session"
  (interactive
   (let* ((directory (read-directory-name
                      "Project directory: " (replique/guess-project-root-dir) nil t))
          (directory (replique/normalize-directory-name directory))
          (existing-repl (replique/repl-by-maybe-not-started
                          :directory directory :repl-type :tooling))
          (repl-script (when (not existing-repl)
                         (completing-read
                          "REPL start script: "
                          (thread-last (directory-files directory)
                            (seq-filter 'replique/script-file-predicate)
                            (cons "*default*"))
                          nil
                          t))))
     (list directory (when (not (equal repl-script "*default*")) repl-script))))
  (let* (;; Normalizing the directory name is necessary in order to be able to find repls
         ;; by directory name
         (directory (replique/normalize-directory-name directory))
         (existing-repl (replique/repl-by-maybe-not-started
                         :directory directory :repl-type :tooling))
         (host (cond (existing-repl
                      (replique/get existing-repl :host))
                     (host host)
                     ((equal 4 (prefix-numeric-value current-prefix-arg))
                      (read-string "Hostname: " "localhost"))
                     (t nil)))
         (port (cond
                (existing-repl
                 (replique/get existing-repl :port))
                (port port)
                ((equal 4 (prefix-numeric-value current-prefix-arg))
                 (read-number "Port number: " 0))
                (t nil))))
    (cond
     ((and existing-repl (null (replique/get existing-repl :started?)))
      (replique/make-repl-starting (replique/get existing-repl :repl-cmd)
                                   directory host port))
     (existing-repl
      (let ((starting-repl (replique/make-repl-starting (replique/get existing-repl :repl-cmd)
                                                        directory host port)))
        (condition-case err
            (replique/start-repl existing-repl starting-repl)
          (error
           (replique/close-repl starting-repl t)
           ;; rethrow the error
           (signal (car err) (cdr err))))))
     (t
      (replique/make-tooling-repl directory repl-script host port)))))

(defun replique/on-repl-type-change (repl new-repl-type new-repl-env)
  (let* ((directory (replique/get repl :directory))
         (repl-type (replique/get repl :repl-type))
         (repl-env (replique/get repl :repl-env))
         (repl-buffer (replique/get repl :buffer))
         (session (replique/get repl :session)))
    (when (not (equal repl-type new-repl-type))
      (when (equal (replique/buffer-name directory repl-type session)
                   (buffer-name repl-buffer))
        (with-current-buffer repl-buffer
          (rename-buffer (generate-new-buffer-name
                          (replique/buffer-name directory new-repl-type session))))))
    (if (and (equal repl-type new-repl-type) (equal repl-env new-repl-env))
        repl
      (replique/update-repl repl (replique/assoc repl
                                                 :repl-type new-repl-type
                                                 :repl-env new-repl-env)))))
(defun replique/dispatch-async-msg (msg)
  (let ((msg-type (replique/get msg :type)))
    (cond
     (;; Global error (uncaught exception)
      (equal :error msg-type)
      (let ((msg (replique-transit/decode-for-printing msg)))
        (message
         (with-temp-buffer
           (insert (propertize "Uncaught exception" 'face '(:foreground "red")))
           (insert " - Thread: ")
           (insert (replique/get msg :thread))
           (insert "\n")
           (replique-print/print (replique/get msg :value))
           (let ((replique-context/platform-tag :clj))
             (let ((replique-pprint/threshold 80))
               (replique-pprint/pprint*)))
           (buffer-substring (point-min) (point-max))))))
     ((equal :repl-meta msg-type)
      (let* ((msg (replique-transit/decode msg))
             (repl (replique/repl-by
                    :session (replique/get (replique/get msg :session) :client)
                    :directory (replique/get msg :process-id))))
        (when repl
          (let ((repl (replique/on-repl-type-change
                       repl (replique/get msg :repl-type) (replique/get msg :repl-env))))
            (replique/update-repl repl (replique/assoc repl
                                                       :ns (replique/get msg :ns)
                                                       :params (replique/get msg :params)))))))
     ((equal :watch-update msg-type)
      (let* ((directory (replique/get msg :process-id))
             (msg (replique-transit/decode msg)))
        (replique-watch/notify-update msg)))
     (t (message "Received unsupported message while dispatching tooling messages: %s" msg)))))

(defun replique/with-new-dyn-context (fn &rest args)
  (apply 'run-at-time nil nil fn args))

(defun replique/dispatch-tooling-msg (msg)
  (cond ((null msg)
         ;; Ignore nil messages. This is only used to wake up (accept-process-output)
         ;; blocking calls
         nil)
        ((not (hash-table-p msg))
         (message "Received invalid message while dispatching tooling messages: %s" msg))
        ((replique/get msg :correlation-id)
         (let* ((tooling-repl (replique/repl-by
                               :repl-type :tooling
                               :directory (replique/get msg :process-id)))
                (correlation-id (replique/get msg :correlation-id))
                (sync-msg (replique/get replique/synchronous-messages correlation-id)))
           (when sync-msg
             (puthash correlation-id msg replique/synchronous-messages))))
        (t (replique/with-new-dyn-context 'replique/dispatch-async-msg msg))))

(defvar replique/parser-state)

(defun replique/read-buffer* (dispatch-fn)
  (setq replique/parser-state (parse-partial-sexp
                               (point) (point-max) 0 nil replique/parser-state))
  (while (and
          (buffer-live-p (current-buffer))
          ;; unbalanced expression or nothing to read
          (= 0 (car replique/parser-state))
          (not (null (nth 2 replique/parser-state)))
          ;; "#s" is the prefix of a hashtable
          (not (and (equal ?\# (char-before (- (point) 1))) (equal ?s (char-before)))))
    (goto-char (point-min))
    (when-let ((o (read (current-buffer))))
      (funcall dispatch-fn o))
    (delete-region (point-min) (point))
    (setq replique/parser-state (parse-partial-sexp
                                 (point) (point-max) 0 nil replique/parser-state))))

(defun replique/read-buffer (string dispatch-fn proc buffer)
  (when (and string (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((p (point)))
        (insert string)
        (goto-char p))
      (replique/read-buffer* dispatch-fn))))

(defun replique/process-filter-read (proc buffer dispatch-fn)
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))
    (setq replique/parser-state nil))
  (buffer-disable-undo buffer)
  (set-process-filter
   proc (lambda (proc string)
          (replique/read-buffer string dispatch-fn proc buffer))))

(defun replique/kill-process-buffer (buffer)
  (if (not (equal (buffer-size buffer) 0))
      ;; There is still stuff in the buffer, let a chance to the reader to finish reading the
      ;; process output
      (run-at-time 1 nil (lambda () (kill-buffer buffer)))
    (kill-buffer buffer)))

(provide 'replique)

;; compliment keywords cljs -> missing :require ... ?
;; CSS / HTML autocompletion, with core.spec ?
;; The cljs-env makes no use of :repl-require

;; autocomplete using the spec first, compliment next if no candidates

;; spec autocomplete for files -> emacs first line local variables
;; spec autocomplete for macros -> specized macros

;; Document the use of cljsjs to use js libs
;; direct linking not supported because of alter-var-root! calls
;; Binding to the loopback address prevents connecting from the outside (mobile device ...)
;; cljs repl server hangs on serving assets on a broken connection ?
;; restore print-namespaced-maps somewhere
;; copy html / css on load-url (problem: override or not (web app context))
;; new target directory for assets resources
;; emacs 26 has built-in, faster "line-number-at-pos"
;; check the infer-externs cljs option
;; eval interruption
;; rename /etc/alternatives/java.save to /etc/alternatives/java
;; try under jdk7
;; autocompletion for nested classes (with a "$")
;; cljs tagged literal should not work when defined in a cljc file (it works because it is defined in the clojure process)
;; deps.edn / tools.alpha support
;; replique/classpath -> lein classpath outputs downloading messages
;; Completing read - sort order of replique/presorted-completion-table not preserved because of a ivy bug? - https://github.com/abo-abo/swiper/issues/1611 -- setting ivy-sort-functions-alist to  nil as a temporary workaround
;; HTTP read-post / get -> blocking read while bytes are still coming in

;; min versions -> clojure 1.8.0, clojurescript 1.9.473
;; byte-recompile to check warnings ----  M-x C-u 0 byte-recompile-directory

;; printing something that cannot be printed by the elisp printer results something that cannot be read by the reader because the elisp printer will print a partial object before throwing an exception

(comment
 (local-set-key (kbd "C-c C-c") 'outline-hide-other)
 (local-set-key (kbd "C-s C-s") 'outline-show-all)
 )

;; check windows
;; build script
