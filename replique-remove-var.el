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
(require 'replique-list-vars)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique/do-remove-var (tooling-repl repl var-ns var-name)
  (replique/send-input-from-source-dispatch
   (format "(replique.interactive/remove-var %s/%s)" var-ns var-name)))

(defun replique/remove-var* (var-ns tooling-repl repl)
  (replique-list-vars/list-vars var-ns tooling-repl repl
                                "Remove var: " 'replique/do-remove-var))

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
