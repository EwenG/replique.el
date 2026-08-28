;;; replique-edn.el --- Printing EDN  -*- lexical-binding: t; -*-

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

;; Clients write EDN, the process writes JSON.  Only the writing side needs
;; code here - JSON is parsed in C by `json-parse-string'.
;;
;; A control connection reads one message per line, so a message must fit on
;; one line: the newlines inside strings are escaped rather than written.

;;; Code:

(require 'subr-x)

(defun replique-edn-string (s)
  "Print the string S as an EDN string."
  (concat "\""
          (mapconcat
           (lambda (c)
             (cond
              ((eq c ?\") "\\\"")
              ((eq c ?\\) "\\\\")
              ((eq c ?\n) "\\n")
              ((eq c ?\r) "\\r")
              ((eq c ?\t) "\\t")
              ;; A raw control character would be read back as itself, and a
              ;; raw newline would end the message halfway through
              ((< c 32) (format "\\u%04x" c))
              (t (char-to-string c))))
           s "")
          "\""))

(defun replique-edn-print (x)
  "Print X as EDN.

Emacs has no false and no keyword type of its own: t is true, the symbol
`false' is false, and a symbol whose name starts with a colon - which is
what a keyword literal reads as - is a keyword."
  (cond
   ((null x) "nil")
   ((eq x t) "true")
   ((stringp x) (replique-edn-string x))
   ((symbolp x) (symbol-name x))
   ((integerp x) (number-to-string x))
   ((floatp x) (number-to-string x))
   ((vectorp x) (concat "[" (mapconcat #'replique-edn-print x " ") "]"))
   ((consp x) (concat "(" (mapconcat #'replique-edn-print x " ") ")"))
   (t (error "Cannot print as EDN: %S" x))))

(defun replique-edn-map (plist)
  "Print the property list PLIST as an EDN map."
  (let ((parts nil))
    (while plist
      (push (concat (replique-edn-print (car plist))
                    " "
                    (replique-edn-print (cadr plist)))
            parts)
      (setq plist (cddr plist)))
    (concat "{" (string-join (nreverse parts) " ") "}")))

(provide 'replique-edn)

;;; replique-edn.el ends here
