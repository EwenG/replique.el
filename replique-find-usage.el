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

(defun replique-find-usage/find-usage* (symbol tooling-repl repl-env ns)
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
        ))))

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

