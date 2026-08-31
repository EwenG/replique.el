;;; replique-eval.el --- Evaluating what is in a buffer  -*- lexical-binding: t; -*-

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

;; Sending what is in a buffer to a repl, saying where it came from.
;;
;; The source directive applies to the next form only, so a region holding
;; several forms is sent as several forms, each with its own: they would
;; otherwise all be recorded at the line of the first.
;;
;; Which means replique has to agree with the reader about where a form
;; begins, and sexp motion does not.  `forward-sexp' stops after #_ and after
;; ^meta, which are not forms but reader macros that consume the form after
;; them - so a directive spliced in there is the form the discard eats.  The
;; form that was commented out gets evaluated, and metadata lands on the
;; directive instead of on the definition it was written for.
;;
;; So the forms are read from a tree-sitter parse, where a discard and what it
;; discards are one node and metadata is part of the form it is on.  There is
;; no list of reader macros here to keep in step with the reader: the grammar
;; is the list.
;;
;; That parse is the one `replique-clojure-mode' already made, which is why
;; these commands ask for that mode rather than for a grammar of their own.
;; Where a form begins is a question the mode answers too - it is what it
;; indents and highlights by - and a buffer that answered it one way for the
;; editor and another for the repl would be a buffer where what you see is not
;; what gets evaluated.
;;
;; A point command descends into a discard on purpose: C-M-x on a form that
;; was commented out with #_ evaluates it, which is the way back from having
;; commented it out.  A region does not, since a discard inside a region was
;; commented out by whoever selected it.
;;
;; A comment is not a form and is never sent, whichever command asks.  What
;; it would produce is a directive nothing consumes: the reader answers a
;; comment with a prompt rather than with a form, so the directive stays
;; pending and lands on whatever is read next - a form typed at the prompt,
;; recorded in a file it was never in.  Nothing empty is sent either, for
;; the same reason - see `replique-eval--send'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'treesit)
(require 'replique-clojure-mode)
(require 'replique-edn)
(require 'replique-repl)

(defconst replique-eval-grammar 'treejure
  "The grammar `replique-clojure-mode' parses with, and replique reads with.")

;;; The parse

(defun replique-eval--parser ()
  "Return the parse of the current buffer that forms are read from.

`replique-clojure-mode' is what makes it, and installs the grammar it
needs - see `replique-clojure-ensure-grammars'.  Nothing is made here:
the mode is asked for rather than worked around, so that a buffer answers
where a form begins once rather than once for the editor and once for the
repl."
  (unless (derived-mode-p 'replique-clojure-mode)
    (user-error "Not a Clojure buffer of replique's - M-x replique-clojure-mode"))
  (or (car (treesit-parser-list nil replique-eval-grammar))
      ;; The mode parses only once the grammar is ready, and says so itself
      ;; when it is not
      (user-error "This buffer has no %s parse - see replique-clojure-mode"
                  replique-eval-grammar)))

(defun replique-eval--root ()
  "Return the root of the parse of the current buffer."
  (treesit-parser-root-node (replique-eval--parser)))

;;; Finding forms

(defun replique-eval--top-level (node)
  "Return the top level form NODE is part of, or nil when it is in none."
  (let ((node node))
    (while (and node
                (treesit-node-parent node)
                (treesit-node-parent (treesit-node-parent node)))
      (setq node (treesit-node-parent node)))
    ;; nil for the root: it is the buffer, not a form in it
    (and node (treesit-node-parent node) node)))

(defun replique-eval--comment-p (node)
  "Return non-nil when NODE is a comment rather than a form.

Asked wherever a node becomes something to send.  A comment consumes no
directive - see the commentary - so a command that finds one has found
nothing to evaluate, and says so rather than sending it."
  (and node (equal "comment" (treesit-node-type node))))

(defun replique-eval--covering (pos)
  "Return the top level form covering POS, or nil when none does.

`treesit-node-at' answers with the first node after POS when nothing
covers it, which is a form somewhere below rather than the one asked
about.  A comment covering POS is nothing covering it: point on a comment
is point on no form.  Only a top level one is ever answered with - inside
a list the walk lands on the list."
  (let ((node (replique-eval--top-level
               (treesit-node-at pos (replique-eval--parser)))))
    (when (and node
               (not (replique-eval--comment-p node))
               (<= (treesit-node-start node) pos)
               (< pos (treesit-node-end node)))
      node)))

(defun replique-eval--ending-at (pos)
  "Return the largest form ending exactly at POS, or nil.

The largest rather than the smallest: point after the last paren of
\"(a (b))\" is at the end of both, and the one that was just finished is
the outer one."
  (let ((node (treesit-node-at (max (point-min) (1- pos)) (replique-eval--parser)))
        (found nil))
    (while node
      (when (and (treesit-node-parent node)
                 (treesit-node-check node 'named)
                 (= (treesit-node-end node) pos))
        (setq found node))
      (setq node (treesit-node-parent node)))
    found))

(defun replique-eval--back-over-space (pos)
  "Return POS with the whitespace before it skipped."
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t\n\r\f")
    (point)))

(defun replique-eval--before (pos)
  "Return the form ending at or before POS, or nil when there is none.

The whitespace behind POS is skipped, and so are the comments behind
that: a comment is not a form, and what was asked for is the form before
it.  Which is what `eval-last-sexp\=' does in Emacs Lisp, and it is the
answer that makes \\[replique-eval-last-sexp] work at the end of a file
whose last line is a note."
  (let ((pos (replique-eval--back-over-space pos))
        (node nil))
    (while (progn
             (setq node (replique-eval--ending-at pos))
             (and node (replique-eval--comment-p node)))
      (setq pos (replique-eval--back-over-space (treesit-node-start node))))
    node))

(defun replique-eval--discarded (node)
  "Return what NODE discards, or NODE when it discards nothing.

A form commented out with #_ is evaluated when it is the form point is
on: that is the way back from having commented it out, and it is what
putting point on a form and asking for it means.  A stacked discard -
#_#_(x)(y), which discards both - answers with the last of them, there
being no better answer to a question that has two."
  (while (and node (equal "discard" (treesit-node-type node)))
    (setq node (let ((target nil))
                 (dotimes (i (treesit-node-child-count node t))
                   (let ((child (treesit-node-child node i t)))
                     (unless (replique-eval--comment-p child)
                       (setq target child))))
                 target)))
  node)

(defun replique-eval--nodes (start end)
  "Return the forms between START and END.

A form that starts inside the region is taken whole, even where it runs
past END: half a form is a read error, not an evaluation.  A region
inside one form is the forms inside it, so three expressions selected in
the body of a function are three forms.

Comments are left out - a comment is not a form, and the reader answers
one with a prompt of its own.  A discard is not: whoever selected a
region selected what was commented out in it too."
  (let* ((root (replique-eval--root))
         (cover (or (treesit-node-descendant-for-range root start (max start end))
                    root)))
    (if (and (treesit-node-parent cover)
             (>= (treesit-node-start cover) start))
        ;; The region is one form, whether or not all of it was selected.
        ;; Its children are the parts of that form, which is not what was
        ;; asked for, and half a form is still the form it is half of -
        ;; unless it is a comment, which is not a form at all
        (unless (replique-eval--comment-p cover) (list cover))
      (let ((nodes nil))
        (dotimes (i (treesit-node-child-count cover t))
          (let ((child (treesit-node-child cover i t)))
            (when (and (>= (treesit-node-start child) start)
                       (< (treesit-node-start child) end)
                       (not (replique-eval--comment-p child)))
              (push child nodes))))
        (nreverse nodes)))))

;;; Sending them

(defun replique-src-directive (file line)
  "Return the directive saying that what follows was taken from FILE at LINE.

A repl reads from a socket, so the file and the line numbers the compiler
records mean nothing to an editor unless the client says where the code
came from.  It applies to the next form only, and it is in band rather
than a process wide setting: it cannot race with another repl, and it
stays in order with the code it describes."
  (format "#replique/src %s"
          (replique-edn-map (append (when file (list :file file))
                                    (when line (list :line line))))))

(defun replique-eval--send (nodes)
  "Evaluate NODES, forms of the current buffer, in the current repl.

A node with no text in it is dropped.  That is what a grammar answers an
unfinished construct with - a zero width node standing where the form
would have been, which is what a trailing #_ produces - and writing a
directive with nothing after it is writing a directive nothing consumes:
it stays pending, and the next form read, typed at the prompt, is
recorded where the buffer said this one was."
  ;; Widened around the whole of it: the parse is of the buffer, so a form
  ;; of it can be outside what a narrowing left reachable - and reading the
  ;; text of one is how that is noticed
  (save-restriction
    (widen)
    (let ((nodes (seq-remove (lambda (node)
                               (string-blank-p (treesit-node-text node t)))
                             nodes)))
      (unless nodes (user-error "Nothing to evaluate"))
      (let ((repl (replique-repl-ensure))
            (file (buffer-file-name)))
        (replique-repl-send-code
         repl
         (mapconcat (lambda (node)
                      (concat (replique-src-directive
                               file (line-number-at-pos (treesit-node-start node) t))
                              "\n"
                              (treesit-node-text node t)))
                    nodes
                    "\n")
         ;; What the buffer is shown leaves the directives out: they are
         ;; protocol, not something anybody wrote
         (mapconcat (lambda (node) (treesit-node-text node t)) nodes "\n")
         ;; Only one form has one result to show
         (null (cdr nodes)))))))

;;; Commands

;;;###autoload
(defun replique-eval-last-sexp ()
  "Evaluate the form before point.

A comment behind point is skipped the way whitespace is, so the last line
of a file being a note does not stop this - see `replique-eval--before'.
A form commented out with #_ is evaluated rather than discarded - see
`replique-eval--discarded'."
  (interactive)
  (let ((node (replique-eval--discarded (replique-eval--before (point)))))
    (unless node (user-error "No form before point"))
    (replique-eval--send (list node))))

;;;###autoload
(defun replique-eval-defun ()
  "Evaluate the top level form around point.

The metadata a definition carries goes with it, and a form commented out
with #_ is evaluated rather than discarded.

Point just after a form counts as being on it, which is where point is
left by having typed it.  Point on a comment is on no form: what is
behind the comment was not what was asked for."
  (interactive)
  (let* ((back (replique-eval--back-over-space (point)))
         (covering (or (replique-eval--covering (point))
                       (and (> back (point-min))
                            (replique-eval--covering (1- back)))))
         (node (replique-eval--discarded covering)))
    (unless node (user-error "No form at point"))
    (replique-eval--send (list node))))

;;;###autoload
(defun replique-eval-region (start end)
  "Evaluate the forms between START and END.

A form that starts inside the region is evaluated whole, even where it
runs past END: half a form is a read error, not an evaluation."
  (interactive "r")
  (replique-eval--send (replique-eval--nodes start end)))

(provide 'replique-eval)

;;; replique-eval.el ends here
