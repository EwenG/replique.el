;;; replique-format.el --- Formatting a buffer the way cljfmt does  -*- lexical-binding: t; -*-

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

;; `replique-format-buffer' does to a buffer what `clojure-lsp format' -
;; cljfmt underneath - does to a file, configured the way the project
;; configures cljfmt (see `replique-cljfmt').  Without a process: what
;; cljfmt does besides indenting is all about the whitespace between forms,
;; and the whitespace between forms is what `replique-parse' leaves between
;; the nodes it reads.
;;
;; So the buffer is read once, every stretch of whitespace between two
;; nodes is looked at once and rewritten if cljfmt would rewrite it, and
;; then the whole of it is indented with `replique-clojure-indent-region',
;; which already indents the way cljfmt does.  The stretches are, in the
;; order cljfmt applies them:
;;
;;   :remove-consecutive-blank-lines?        two blank lines or more become one
;;   :remove-surrounding-whitespace?         none just inside a bracket
;;   :insert-missing-whitespace?             a space between two forms
;;                                           written against each other
;;   :remove-multiple-non-indenting-spaces?  one space between two forms
;;   :indentation?
;;   :remove-trailing-whitespace?
;;   :normalize-newlines-at-file-end?
;;
;; each followed where the configuration turns it on, cljfmt's defaults
;; otherwise.  What reorders or realigns forms - sorting the ns references,
;; splitting map entries, aligning columns - is not done, and is said to
;; be not done when a configuration asks for it.
;;
;; A comment owns the newline that ends it, to cljfmt, and owns whatever
;; spaces are written before that newline.  So the space at the end of a
;; comment is not trailing whitespace, and the line break after one is not
;; whitespace before a closing bracket - which is the one place the two
;; readings differ, since a comment node here ends where its line does.

;;; Code:

(require 'seq)
(require 'replique-parse)
(require 'replique-cljfmt)
(require 'replique-clojure-mode)

(defconst replique-format--unsupported
  '(:split-keypairs-over-multiple-lines? :sort-ns-references?
    :align-map-columns? :align-form-columns? :align-binding-columns?
    :remove-blank-lines-in-forms?)
  "What cljfmt can be configured to do that formatting here does not.")

(defconst replique-format--prefix-widths
  '((list . 1) (vector . 1) (map . 1) (set . 2) (fn . 2)
    (quote . 1) (syntax-quote . 1) (deref . 1) (unquote . 1)
    (unquote-splicing . 2) (var-quote . 2) (discard . 2) (eval . 2)
    (tagged . 1) (namespaced-map . 1)
    (reader-conditional . 2) (reader-conditional-splicing . 3))
  "How long the text a form opens with is, before what it holds.")

(defconst replique-format--closed '(list vector map set fn)
  "The forms that end with a bracket of their own.")

(defconst replique-format--reader-macros
  '(tagged namespaced-map reader-conditional reader-conditional-splicing)
  "What cljfmt reads as a reader macro of several parts.

The space between its parts is not whitespace inside a bracket, and two of
them written against each other are not two forms missing a space:
`#inst \"…\"' and `#?(…)' are written the way they are written.")

(defun replique-format--prefix-width (node)
  "How long the text NODE opens with is."
  (if (eq 'meta (replique-parse-type node))
      ;; `#^' is how metadata was written before `^' was
      (if (eq ?# (char-after (replique-parse-start node))) 2 1)
    (or (alist-get (replique-parse-type node) replique-format--prefix-widths) 0)))

(defun replique-format--children (node)
  "What NODE holds, in order, with the entries of a map opened up."
  (let ((children nil))
    (dolist (child (replique-parse-children node))
      (if (eq 'pair (replique-parse-type child))
          (dolist (part (replique-parse-children child))
            (push part children))
        (push child children)))
    (nreverse children)))

(defconst replique-format--broken '(unclosed mismatched unmatched eof)
  "What is wrong with a form that makes where it ends a guess.

A token written in a way the reader refuses, or a map holding an odd
number of forms, is still where it is - and is often not wrong at all:
`#?@' splices into a map, and `String/1' is how Clojure 1.12 names an
array class.  So formatting goes ahead around those.")

(defun replique-format--first-error (node)
  "The first node at or below NODE whose extent cannot be known, or nil."
  (let ((err (replique-parse-error node)))
    (cond
     ((null err) nil)
     ((memq err replique-format--broken) node)
     (t (seq-some #'replique-format--first-error (replique-parse-children node))))))

(defun replique-format--comment-p (node)
  "Return non-nil when NODE is a comment."
  (and node (eq 'comment (replique-parse-type node))))

(defun replique-format--single-spaces (text after-comment before-comment)
  "TEXT with every run of spaces that does not indent a line made one space.

AFTER-COMMENT says TEXT comes after a comment, whose newline it starts
with, and BEFORE-COMMENT that a comment comes after it: the spaces before
a comment are where somebody put it."
  (let ((start 0)
        (out nil))
    (while (string-match "[ \t]+" text start)
      (let ((beginning (match-beginning 0))
            (end (match-end 0)))
        (push (substring text start beginning) out)
        (push (if (or (and (> beginning 0) (eq ?\n (aref text (1- beginning))))
                      (and (= beginning 0) after-comment)
                      (and (= end (length text)) before-comment))
                  (match-string 0 text)
                " ")
              out)
        (setq start end)))
    (push (substring text start) out)
    (apply #'concat (nreverse out))))

(defun replique-format--last-leaf (node)
  "The last thing written in NODE, however deep, or NODE where it holds nothing."
  (let ((children (and node (replique-format--children node))))
    (while children
      (setq node (car (last children))
            children (replique-format--children node)))
    node))

(defun replique-format--blank-lines (from to left)
  "How many line breaks cljfmt counts from the whitespace FROM TO, after LEFT.

Zero where none starts there.  The count runs from the first line break
on - the second, after a comment, whose own line break is its own - and
carries on past a closing bracket where only spaces are written before
it, because cljfmt steps over those spaces with a move that leaves the
form.  A line break with the bracket right after it is the last thing in
the form, and the count stops there."
  (let* ((text (buffer-substring-no-properties from to))
         (comment (replique-format--comment-p left))
         (first (string-search "\n" text))
         (start (and first (if comment (string-search "\n" text (1+ first)) first))))
    (if (null start)
        0
      (save-excursion
        (goto-char (+ from start))
        (let ((count (if comment 1 0))
              (done nil))
          (while (not done)
            (cond
             ((eq (char-after) ?\n)
              (setq count (1+ count))
              (forward-char 1)
              (when (memq (char-after) '(?\) ?\] ?\}))
                (setq done t)))
             ((memq (char-after) '(?\s ?\t ?,))
              (skip-chars-forward " \t,)]}")
              (unless (eq (char-after) ?\n) (setq done t)))
             (t (setq done t))))
          count)))))

(defun replique-format--anything-after-p (position)
  "Return non-nil when anything but whitespace is written after POSITION."
  (save-excursion
    (goto-char position)
    (skip-chars-forward " \t\n,)]}")
    (not (eobp))))

(defun replique-format--gap (text left right parent root-p options)
  "What the whitespace TEXT between LEFT and RIGHT in PARENT is rewritten as.

LEFT is nil where TEXT follows what PARENT opens with and RIGHT where it
comes before PARENT's end.  ROOT-P is whether PARENT is the buffer itself.
OPTIONS is a function of an option and its default, answering what the
configuration says.  Blank lines are not this function's: see
`replique-format--edits'."
  (let ((type (replique-parse-type parent))
        (new text))
    (cond
     ((and root-p (null left) right))
     ;; Just inside the brackets
     ((and (not root-p) (null left) right)
      (when (and (funcall options :remove-surrounding-whitespace? t)
                 (not (memq type replique-format--reader-macros))
                 (not (replique-format--comment-p right))
                 ;; `~ @x' is not `~@x'
                 (not (and (eq 'unquote type)
                           (eq 'deref (replique-parse-type right)))))
        (setq new "")))
     ((and (not root-p) (null right))
      (when (funcall options :remove-surrounding-whitespace? t)
        (setq new (if (replique-format--comment-p left) "\n" ""))))
     ;; Between two things
     ((and left right)
      (cond
       ((and (equal text "")
             (funcall options :insert-missing-whitespace? t)
             (not (replique-format--comment-p right))
             (not (memq type replique-format--reader-macros)))
        (setq new " "))
       ((funcall options :remove-multiple-non-indenting-spaces? nil)
        (setq new (replique-format--single-spaces
                   text (replique-format--comment-p left)
                   (replique-format--comment-p right))))))
     ;; The end of the buffer
     ((and root-p (null right) left
           (funcall options :normalize-newlines-at-file-end? nil))
      (setq new "\n")))
    (replique-format--trim new (and root-p (null right)) options)))

(defun replique-format--trim (text end-p options)
  "TEXT without the spaces that end a line, nor those that end the buffer.
END-P is whether TEXT is what the buffer ends with.  OPTIONS is as for
`replique-format--gap'."
  (when (funcall options :remove-trailing-whitespace? t)
    (setq text (replace-regexp-in-string "[ \t]+\n" "\n" text t t))
    (when end-p
      (setq text (replace-regexp-in-string "[ \t]+\\'" "" text t t))))
  text)

(defun replique-format--edits (root options)
  "The rewrites of the whitespace in the tree ROOT, last first.
Each a list of where the whitespace starts, where it ends, and what it is
rewritten as.  OPTIONS is as for `replique-format--gap'.

Blank lines are taken away the way cljfmt takes them away, which is not
the way its option reads.  Where it counts more than two line breaks in a
row it searches forward for the next thing written and backward from that
for the last thing written, takes away all the whitespace between the two
- the indentation of the next thing included - and puts the next thing
two lines under the last, or one where the last is a comment.  The count
crosses a closing bracket where spaces stand before it, and so does the
search: blank lines before a bracket take with them the line breaks after
it, and put back one blank line after it, wherever the next thing is.
Every file `clojure-lsp format' has been through has been through that, so
it is done here too."
  (let ((edits nil)
        (pending nil)
        (blank-lines (funcall options :remove-consecutive-blank-lines? t)))
    (named-let walk ((node root) (root-p t))
      (let* ((children (replique-format--children node))
             (open (if root-p
                       (replique-parse-start node)
                     (+ (replique-parse-start node)
                        (replique-format--prefix-width node))))
             (close (if (and (not root-p)
                             (memq (replique-parse-type node) replique-format--closed))
                        (1- (replique-parse-end node))
                      (replique-parse-end node)))
             (from open)
             (left nil))
        (dolist (child (append children (list nil)))
          (let* ((to (if child (replique-parse-start child) close))
                 (text (buffer-substring-no-properties from to))
                 (count (if (and blank-lines (not pending)
                                 ;; The top and the bottom of the buffer
                                 ;; are nobody's business
                                 (not (and root-p (or (null child) (null left)))))
                            (replique-format--blank-lines from to left)
                          0))
                 (collapse (and (> count 2) (replique-format--anything-after-p from)))
                 (new
                  (cond
                   ;; The thing that blank lines before a bracket were followed by
                   ((and pending child left)
                    (prog1 pending (setq pending nil)))
                   ((and pending (null child))
                    (if (replique-format--comment-p left) "\n" ""))
                   ((and collapse (null child))
                    (setq pending (if (replique-format--comment-p
                                       (replique-format--last-leaf left))
                                      "\n" "\n\n"))
                    (if (replique-format--comment-p left) "\n" ""))
                   ((and collapse left)
                    (let ((last (replique-format--last-leaf left)))
                      (if (and (replique-format--comment-p last) (not (eq last left)))
                          "\n"
                        "\n\n")))
                   (collapse
                    ;; At the top of a form, or of the buffer.  What is just
                    ;; inside a bracket goes after this, unless a comment is
                    ;; written there
                    (replique-format--gap "\n\n" left child node root-p options))
                   (t (replique-format--gap text left child node root-p options)))))
            (unless (equal new text)
              (push (list from to new) edits))
            (when child
              (when (or (replique-parse-children child)
                        (memq (replique-parse-type child) replique-format--closed))
                (walk child nil))
              (setq left child
                    from (replique-parse-end child)))))))
    (sort edits (lambda (a b) (> (car a) (car b))))))

;;;###autoload
(defun replique-format-buffer ()
  "Format the buffer the way cljfmt would, as `clojure-lsp format' does.

Configured the way the project configures cljfmt - see
`replique-cljfmt-config' - and indented with `replique-clojure-indent-region'.
A buffer that does not read is left as it is."
  (interactive)
  (let* ((config (replique-clojure-cljfmt-config))
         (options (lambda (key default)
                    (replique-cljfmt-option config key default)))
         (root (save-restriction (widen) (replique-parse-buffer)))
         (broken (replique-format--first-error root)))
    (when broken
      (user-error "Not formatted: what is written at line %d does not read (%s)"
                  (line-number-at-pos (replique-parse-start broken))
                  (replique-parse-error broken)))
    (let ((ignored (seq-filter (lambda (key) (replique-cljfmt-option config key))
                               replique-format--unsupported)))
      (when ignored
        (message "replique: cljfmt options not supported, ignored: %s"
                 (mapconcat #'symbol-name ignored " "))))
    (save-excursion
      (save-restriction
        (widen)
        (dolist (edit (replique-format--edits root options))
          (goto-char (nth 0 edit))
          (delete-region (nth 0 edit) (nth 1 edit))
          (insert (nth 2 edit)))
        (when (funcall options :indentation? t)
          (let ((replique-clojure--cljfmt config))
            (replique-clojure-indent-region (point-min) (point-max))))))))

(provide 'replique-format)

;;; replique-format.el ends here
