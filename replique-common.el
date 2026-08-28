;;; replique-common.el --- What the rest of replique shares  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; The customization group and the faces.  They live in a file of their own
;; because a defcustom needs its group to exist by the time it is read, and
;; the two buffers that render what a process says - the repl and the process
;; output - are at different levels of the require chain.  Putting the group
;; in whichever of them happens to load first would make its home look like
;; an accident.
;;
;; Everything a face is used for here is text a process produced.  What
;; replique says about a process rather than what the process said is the one
;; exception, and it is deliberately quiet.

;;; Code:

(require 'comint)

(defgroup replique nil
  "A development environment for Clojure."
  :group 'tools)

(defface replique-stderr
  '((t (:inherit error)))
  "Face for what a process prints on its error stream."
  :group 'replique)

(defface replique-note
  '((t (:inherit shadow :slant italic)))
  "Face for what replique says about a process rather than what it printed."
  :group 'replique)

(defface replique-prompt
  '((t (:inherit comint-highlight-prompt)))
  "Face for the prompt of a repl."
  :group 'replique)

(defface replique-exception
  '((t (:inherit error)))
  "Face for what a form threw."
  :group 'replique)

(provide 'replique-common)

;;; replique-common.el ends here
