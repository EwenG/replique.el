;; replique-files.el ---   -*- lexical-binding: t; -*-

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

(defvar replique/directory-files-recursively-files nil)

(defun replique/directory-files-recursively* (dir regexp dir-predicate warning-type)
  (dolist (file (file-name-all-completions "" dir))
    (unless (member file '("./" "../"))
      (if (directory-name-p file)
          (let* ((leaf (substring file 0 (1- (length file))))
                 (full-file (expand-file-name leaf dir)))
            ;; Don't follow symlinks to other directories.
            (unless (or (file-symlink-p full-file)
                        (null (funcall dir-predicate full-file warning-type)))
              (replique/directory-files-recursively*
               full-file regexp dir-predicate warning-type)))
        (when (string-match-p regexp file)
          (push (expand-file-name file dir) replique/directory-files-recursively-files)))))
  replique/directory-files-recursively-files)

(defun replique/directory-files-recursively (dir regexp dir-predicate warning-type)
  (let ((replique/directory-files-recursively-files nil))
    (replique/directory-files-recursively* dir regexp dir-predicate warning-type)
    replique/directory-files-recursively-files))

(defun replique/guess-project-root-dir (&optional dir)
  (or (locate-dominating-file (or dir default-directory) "deps.edn")
      (or dir default-directory)))

(provide 'replique-files)
