;; replique-logback.el ---   -*- lexical-binding: t; -*-

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
(require 'replique-pprint)

(defun replique/logback-reload ()
  (interactive)
  (let* ((clj-repl (replique/active-repl :clj t))
         (directory (replique/get clj-repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling)))
    (replique/send-input-from-source-clj "(replique.interactive/logback-reload)"
                                         tooling-repl clj-repl)))

(provide 'replique-logback)
