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

(require 'package-x)

(defvar local-archive
  (expand-file-name "packages/" "~/replique.el")
  "Location of the package archive.")
(setq package-archive-upload-base local-archive)

(defun make-package (version)
  (let ((default-directory "~/"))
    (shell-command (format "cp -R replique.el replique-%s" version))
    (shell-command (format "COPYFILE_DISABLE=1 tar -cvf replique-%s.tar --exclude=\"replique-%s/.*\" --exclude=\"replique-%s/packages\" replique-%s/" version version version version))
    (shell-command (format "rm -r ~/replique-%s" version))
    (package-upload-file (format "~/replique-%s.tar" version))
    (shell-command (format "rm ~/replique-%s.tar" version))))

(comment
 (make-package "0.0.6")
 )

;; package-upload-file

(provide 'replique-package)
