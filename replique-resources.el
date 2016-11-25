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

;; Version 0.0.1-SNAPSHOT
;; Package-Requires: ((emacs "25") (clojure-mode "5.6.0"))

;; Commentary:

;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theses functions are taken from cider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun replique-resources/url-to-file (url)
  "Return the filename from the resource URL.
Uses `url-generic-parse-url' to parse the url.  The filename is extracted and
then url decoded.  If the decoded filename has a Windows device letter followed
by a colon immediately after the leading '/' then the leading '/' is dropped to
create a valid path."
  (let ((filename (url-unhex-string (url-filename (url-generic-parse-url url)))))
    (if (string-match "^/\\([a-zA-Z]:/.*\\)" filename)
        (match-string 1 filename)
      filename)))

(let* ((url "jar:file:/Users/egr/.m2/repository/org/clojure/clojure/1.8.0/clojure-1.8.0.jar!/clojure/core.clj")
       (entry (match-string 3 url))
       (file (replique-resources/url-to-file url)))
  (format "%s:%s" file entry))

(defun replique-resources/find-file (url)
  "Return a buffer visiting the file URL if it exists, or nil otherwise.
If URL has a scheme prefix, it must represent a fully-qualified file path
or an entry within a zip/jar archive.  If URL doesn't contain a scheme
prefix and is an absolute path, it is treated as such."
  (require 'arc-mode)
  (when url
    (cond ((string-match "^file:\\(.+\\)" url)
           (when-let ((file (replique-resources/url-to-file (match-string 1 url))))
             (when (file-exists-p file)
               (find-file-noselect file))))
          ((string-match "^\\(jar\\|zip\\):\\(file:.+\\)!/\\(.+\\)" url)
           (when-let ((entry (match-string 3 url))
                      (file  (replique-resources/url-to-file (match-string 2 url)))
                      (name  (format "%s:%s" file entry)))
             (or (find-buffer-visiting name)
                 ;; Use external zip program to just extract the single file
                 (with-current-buffer (generate-new-buffer (file-name-nondirectory entry))
                   (archive-zip-extract file entry)
                   (set-visited-file-name name)
                   (setq-local default-directory (file-name-directory file))
                   (setq-local buffer-read-only t)
                   (set-buffer-modified-p nil)
                   (set-auto-mode)
                   (current-buffer)))))
          ((file-exists-p url) (find-file-noselect url))
          (t nil))))

(provide 'replique-resources)
