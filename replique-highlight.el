;; replique-highlight.el ---   -*- lexical-binding: t; -*-

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

(defun replique-highlight/make-overlay ()
  (let ((ol (make-overlay (point) (point))))
    (overlay-put ol 'priority -50)           ;(bug#16192)
    (overlay-put ol 'face 'isearch)
    ol))

(defun replique-highlight/make-line-overlay ()
  (let ((ol (make-overlay (point) (point))))
    (overlay-put ol 'priority -50)           ;(bug#16192)
    (overlay-put ol 'face 'highlight)
    ol))

(defvar replique-highlight/overlay (replique-highlight/make-overlay))
(defvar replique-highlight/line-overlay (replique-highlight/make-line-overlay))

(defun replique-highlight/highlight (&optional b e)
  (move-overlay replique-highlight/line-overlay
                (line-beginning-position)
                (line-beginning-position 2)
                (current-buffer))
  (if (and b e)
      (move-overlay replique-highlight/overlay b e (current-buffer))
    (delete-overlay replique-highlight/overlay)))

(defun replique-highlight/unhighlight ()
  (delete-overlay replique-highlight/overlay)
  (delete-overlay replique-highlight/line-overlay))

(comment
 (defun replique-list-vars/highlight ()
   (progn
     (unless replique-list-vars/line-overlay
       (setq replique-list-vars/line-overlay (replique-list-vars/make-overlay)))
     (overlay-put replique-list-vars/line-overlay
                  'window (selected-window))
     (replique-list-vars/move-overlay replique-list-vars/line-overlay))))

(provide 'replique-highlight)
