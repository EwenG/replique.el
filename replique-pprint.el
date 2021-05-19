;; replique-pprint.el ---   -*- lexical-binding: t; -*-

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

(require 'replique-repls)
(require 'clj-data)
(require 'clj-context)
(require 'clj-pprint)

(defun replique-pprint/pprint-clj (tooling-repl clj-repl)
  ;; platform-tag /splice end is only needed for clj-context/read-one. It is only used
  ;; during walk-init
  (let ((clj-context/platform-tag ":clj")
        (clj-context/splice-ends '()))
    (clj-pprint/pprint*)))

(defun replique-pprint/pprint-cljs (tooling-repl cljs-repl)
  (let ((clj-context/platform-tag ":cljs")
        (clj-context/splice-ends '()))
    (clj-pprint/pprint*)))

(defun replique-pprint/pprint-cljc (tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((clj-context/platform-tag (symbol-name (clj-data/get repl :repl-type)))
          (clj-context/splice-ends '()))
      (clj-pprint/pprint*))))

(defun replique-pprint/pprint-session (repl)
  (let ((clj-context/platform-tag (symbol-name (clj-data/get repl :repl-type)))
        (clj-context/splice-ends '()))
    (clj-pprint/pprint*)))

(defun replique/pprint ()
  (interactive)
  (replique/with-modes-dispatch
   (replique/mode . 'replique-pprint/pprint-session)
   (clojure-mode . 'replique-pprint/pprint-clj)
   (clojurescript-mode . 'replique-pprint/pprint-cljs)
   (clojurec-mode . 'replique-pprint/pprint-cljc)))

(provide 'replique-pprint)
