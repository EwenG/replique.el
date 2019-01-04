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

(require 'replique-utils)
(require 'replique-repls)
(require 'replique-pprint)

(defun replique/logback-reload (&optional file-url)
  (interactive)
  (when (buffer-file-name)
    (comint-check-source (buffer-file-name))
    (let* ((tooling-repl (replique/active-repl :tooling t))
           (clj-repl (replique/active-repl :clj t))
           (file-url (or file-url
                         (read-file-name "Logback configuration file: "
                                         (replique/get tooling-repl :directory)
                                         (buffer-file-name) t (buffer-file-name))))
           (directory (replique/get clj-repl :directory))
           (tooling-repl (replique/repl-by :directory directory :repl-type :tooling)))
      (replique/send-input-from-source-clj
       (format "(replique.interactive/logback-reload \"%s\")"
               (replique/buffer-url file-url))
       tooling-repl clj-repl))))

(provide 'replique-logback)
