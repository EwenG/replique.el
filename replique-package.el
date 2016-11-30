;; replique-package.el ---   -*- lexical-binding: t; -*-

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

(defun replique/package (dir)
  (interactive (list (read-directory-name "Replique dir: " nil nil t)))
  (byte-recompile-directory dir 0 t)
  (let ((default-directory dir))
    (shell-command (concat "zip replique.zip *.el *.elc")))
  (dolist (f (directory-files dir))
    (when (string-match-p "\.elc$" f)
      (delete-file f))))

(provide 'replique-package)
