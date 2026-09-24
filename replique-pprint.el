;;; replique-pprint.el --- Laying out data so it can be read  -*- lexical-binding: t; -*-

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

;; Breaking a printed value over lines so that it can be read.  What a repl
;; prints is one line however long the value is, and a map of two hundred
;; keys arrives as two hundred keys of one line.
;;
;; Data, not code.  Nothing here knows that the second element of a `let' is
;; a binding vector or that a `defn' takes its arglist on the first line -
;; the input is a value something printed, and a value has no such shape.
;; Code laid out by this comes out formatted like data, which is not wrong so
;; much as beside the point.
;;
;; It is written in two passes, measure and then emit, and that is the whole
;; design.  The obvious way is one pass that reformats the text where it
;; already is, and the obvious way is a trap: every space inserted moves
;; everything after it, so every position taken before the insertion has to be
;; patched by the length of what was inserted, and the arithmetic to keep them
;; in step becomes most of the program.  Worse, indenting a run of lines that
;; way means walking a range of characters and inserting at the start of each,
;; and a range of characters is not a thing that can be indented: a string with
;; a newline in it is several lines that are one token, and spaces pushed into
;; it are spaces pushed into the value.  Master's printer does exactly this,
;; and a string that goes over two lines comes back out of it longer than it
;; went in.
;;
;; Measuring first removes both.  How wide a form is written flat is a
;; question about the parse, answerable before a character is emitted, so
;; there is nothing to patch; and emitting builds new text, so indentation
;; only ever goes at the start of a line this made itself.  A token is written
;; out exactly as it was written in, always - which is why the string above
;; comes back unchanged, and why nothing here has to know what a token means.
;;
;; A token with a newline in it never fits.  It is not measured against the
;; width and it is not broken up; it is put down whole, and where the next
;; thing goes is read off the buffer, which is already counting columns.
;;
;; What is compared against the width is the column in the output, not the
;; width of a form on its own.  A form nested ten deep starts ten columns in
;; and has ten fewer to spend, which is the point of a width - master compares
;; each collection against a budget it restarts at that collection's own
;; opening bracket, so a two key map at the top level breaks and a fifty
;; character one nested six deep does not.
;;
;; A map breaks one entry per line, everything else fills - as many elements
;; to a line as fit.  Filling is right for the vector of numbers that is most
;; of what a repl prints; one entry per line is right for a map, where the
;; pairs are what is being read.  A value hangs after its key rather than
;; going under it, because under it is where the next key goes and a value
;; written there reads as one.
;;
;; Hanging is also what runs a line past the width, each key hanging the next
;; one further in than the last, and it is worth knowing how much: over four
;; hundred generated values, at the width this is set to by default one in
;; thirty has a line past it and never by more than three columns; at half
;; that width one in five does and the worst is twenty four columns over.  So
;; the exchange is a narrow width for a map whose entries can be told apart,
;; and it is a narrow width that pays for it.
;;
;; Whitespace between a reader macro and what it applies to is written as one
;; space rather than removed.  `#inst "…"' keeps the space it was written
;; with, `#foo{…}' gains none it did not have, and - this is the reason - no
;; two tokens are ever pushed together into a third, which is what removing
;; the space in `^:m x' would do.
;;
;; Comments are refused, not deleted.  This lays out data, and it has no idea
;; which of the things around a comment the comment was about, so there is
;; nowhere to put one.  Master deletes them, which is the one answer that
;; loses what you wrote without saying so.

;;; Code:

(require 'subr-x)
(require 'replique-common)
(require 'replique-parse)

(defcustom replique-pprint-width 80
  "How many columns pretty printed data is laid out to fit in.

A number of columns rather than the width of whatever window happens to
be showing it: the same value laid out twice is then the same text, and
a window resized in between does not make it something else.  Set it
buffer locally where one buffer wants a different width."
  :type 'integer
  :group 'replique)


;;;; The shape of a node
;;
;; Three kinds, and everything else is a token written out as it was written.
;; The point of the default being "as it was written" is that a construct
;; nobody thought of here is put down whole rather than taken apart wrongly.

(defvar replique-pprint--source nil
  "The buffer the tree being written out was read from.

The text is written into a buffer of its own, so the one the tree came
from has to be named rather than assumed: a node holds where it starts
and ends, and not what it starts and ends in.")

(defconst replique-pprint--collections
  '((list "(" ")")
    (vector "[" "]")
    (map "{" "}")
    (set "#{" "}")
    (fn "#(" ")"))
  "The collections, each with the text it opens and closes with.

Taken from here rather than from the buffer so that a collection is
written with the delimiters its kind has, whatever was in the text.")

(defconst replique-pprint--macros
  '(quote syntax-quote unquote unquote-splicing deref var-quote discard
          meta tagged namespaced-map reader-conditional
          reader-conditional-splicing eval)
  "The reader macros, which are each written in front of a form.

Which form that is, is the last one the macro is made of, whichever macro
it is - see `replique-parse-target'.  What is written in front of it is
the macro's own text and is written out unchanged - see
`replique-pprint--prefix' - so nothing here needs to know that a tagged
literal is a # and a symbol while a namespaced map is a # and a keyword.")

(defun replique-pprint--delimiters (node)
  "Return how NODE opens and closes, or nil when it is not a collection."
  (cdr (assq (replique-parse-type node) replique-pprint--collections)))

(defun replique-pprint--pair-p (node)
  "Return non-nil when NODE is one entry of a map."
  (eq 'pair (replique-parse-type node)))

(defun replique-pprint--wrapped (node)
  "Return the form the reader macro NODE is in front of, or nil for neither.

Nil as well for a macro in front of nothing, which is a trailing #_:
there is no form to write after the macro, so what is left is text, and
text is written out as it stands."
  (when (memq (replique-parse-type node) replique-pprint--macros)
    (replique-parse-target node)))

(defun replique-pprint--elements (node)
  "Return what NODE is made of.

Every part of it, with nothing dropped and nothing checked, because the
parse was checked before any of this ran - see `replique-pprint--check'."
  (replique-parse-children node))


(defun replique-pprint--prefix (node wrapped)
  "Return the text NODE writes in front of WRAPPED.

Every run of whitespace in it is written as one space rather than
removed.  Removing it is what would push two tokens together into a
third - `^:m x' into `^:mx' - and keeping it is what lets `#inst \"…\"'
stay as it is while `#foo{…}' gains nothing."
  (replace-regexp-in-string
   "[ \t\n\r\f,]+" " "
   (with-current-buffer replique-pprint--source
     (buffer-substring-no-properties (replique-parse-start node)
                                     (replique-parse-start wrapped)))))


;;;; Measuring
;;
;; How wide a node is written flat, or nil for one that cannot be written
;; flat at all.  Nil is not "too wide": it is a token with a newline in it,
;; which no width would make fit and which nothing here is going to reflow.
;;
;; Every level asks this of its children and is asked it by its parent, so
;; the answers are kept - without that, a form is measured once per level
;; above it.
;;
;; Keeping them is also what stops the two recursions adding up, which is
;; worth knowing before anyone measures the whole tree up front to avoid it.
;; The first thing asked about a form is its width, and answering that walks
;; all of it - so the measuring is done and off the stack before a character
;; of it has been written out, and every width the writing out asks for
;; after that is one already worked out.  The two are deep at different
;; times, not at once; measuring up front is a fifth slower and buys nothing.

(defvar replique-pprint--widths nil
  "Widths already measured this run, keyed by the bounds of the node.")

(defun replique-pprint--width (node)
  "Return the width NODE has written on one line, or nil for one it has not."
  (let* ((key (cons (replique-parse-start node) (replique-parse-end node)))
         (known (gethash key replique-pprint--widths 'unmeasured)))
    (if (eq known 'unmeasured)
        (puthash key (replique-pprint--measure node) replique-pprint--widths)
      known)))

(defun replique-pprint--measure-elements (node)
  "Return the width what NODE is made of has on one line, or nil.

One space between each of them, which is what writing them flat puts
there, and nil as soon as one of them cannot be written flat at all."
  (let ((total 0)
        (first t)
        (flat t))
    (dolist (child (replique-pprint--elements node))
      (when flat
        (let ((width (replique-pprint--width child)))
          (if (null width)
              (setq flat nil)
            (setq total (+ total width (if first 0 1)))
            (setq first nil)))))
    (and flat total)))

(defun replique-pprint--measure (node)
  "Return the width NODE has written on one line, or nil for one it has not."
  (let ((delimiters (replique-pprint--delimiters node))
        (wrapped (replique-pprint--wrapped node)))
    (cond
     (delimiters
      (when-let* ((inside (replique-pprint--measure-elements node)))
        (+ (length (car delimiters)) inside (length (cadr delimiters)))))
     ((replique-pprint--pair-p node)
      (replique-pprint--measure-elements node))
     (wrapped
      (when-let* ((width (replique-pprint--width wrapped)))
        (+ (string-width (replique-pprint--prefix node wrapped)) width)))
     ;; A token, measured as the text it is.  The one that answers nil is the
     ;; one written over several lines - a string, and nothing else - and it
     ;; is the reason this answers nil at all
     (t
      (let ((text (replique-parse-text node replique-pprint--source)))
        (unless (string-search "\n" text)
          (string-width text)))))))


;;;; Emitting
;;
;; Into the current buffer, at point, which is where the column being
;; measured against comes from: the buffer is counting them already, and a
;; token put down whole leaves point wherever its last line ended.

(defun replique-pprint--emit-flat-elements (node)
  "Write what NODE is made of into the current buffer, on one line."
  (let ((first t))
    (dolist (child (replique-pprint--elements node))
      (unless first (insert " "))
      (setq first nil)
      (replique-pprint--emit-flat child))))

(defun replique-pprint--emit-flat (node)
  "Write NODE into the current buffer on one line."
  (let ((delimiters (replique-pprint--delimiters node))
        (wrapped (replique-pprint--wrapped node)))
    (cond
     (delimiters
      (insert (car delimiters))
      (replique-pprint--emit-flat-elements node)
      (insert (cadr delimiters)))
     ((replique-pprint--pair-p node)
      (replique-pprint--emit-flat-elements node))
     (wrapped
      (insert (replique-pprint--prefix node wrapped))
      (replique-pprint--emit-flat wrapped))
     (t (insert (replique-parse-text node replique-pprint--source))))))

(defun replique-pprint--emit-collection (node open close width fill)
  "Write NODE, between OPEN and CLOSE, broken over lines to fit WIDTH.

The first element goes against the opening delimiter.  Where FILL says
so, the ones after it go on the line so far when there is room for them
written flat, and on a new line at the same column as the first
otherwise; where it does not, each of them goes on a line of its own.
Filling is right for the vector of numbers that is most of what a repl
prints, and one to a line is right for a map, where what is being read is
the pairs.

A new line as well after an element that took more than one, which is the
difference between a long element and its neighbours reading as separate
things and reading as one run."
  (insert open)
  (let ((indent (current-column))
        (first t)
        (broke nil))
    (dolist (child (replique-pprint--elements node))
      (unless first
        (let ((flat (replique-pprint--width child)))
          (if (and fill flat (not broke) (<= (+ (current-column) 1 flat) width))
              (insert " ")
            (insert "\n" (make-string indent ?\s)))))
      ;; Which line point is on rather than its number: a number is counted
      ;; from the start of the buffer, and counting it once an element makes
      ;; laying out a long one cost the square of its length
      (let ((line (line-beginning-position)))
        (replique-pprint--emit child width)
        (setq broke (/= line (line-beginning-position))))
      (setq first nil)))
  (insert close))

(defun replique-pprint--emit-pair (node width)
  "Write NODE, one entry of a map, to fit WIDTH.

The value hangs after the key however little room is left for it.  Under
the key is where the next key goes, so a value written there reads as
one, and a map whose entries cannot be told apart is worse than a map
that runs past the width - see the commentary for how far past."
  (let ((first t))
    (dolist (child (replique-pprint--elements node))
      (unless first (insert " "))
      (setq first nil)
      (replique-pprint--emit child width))))

(defun replique-pprint--emit-broken (node width)
  "Write NODE into the current buffer over several lines, to fit WIDTH."
  (let ((delimiters (replique-pprint--delimiters node))
        (wrapped (replique-pprint--wrapped node)))
    (cond
     (delimiters
      (replique-pprint--emit-collection
       node (car delimiters) (cadr delimiters) width
       ;; every collection fills but a map, whose entries are what is read
       (not (eq 'map (replique-parse-type node)))))
     ((replique-pprint--pair-p node)
      (replique-pprint--emit-pair node width))
     (wrapped
      (insert (replique-pprint--prefix node wrapped))
      (replique-pprint--emit wrapped width))
     ;; A token is one thing and there is nowhere in it to break.  The one
     ;; that gets here is the string written over several lines, and it is
     ;; put down as it was written: whoever wrote those lines meant them
     (t (replique-pprint--emit-flat node)))))

(defun replique-pprint--emit (node width)
  "Write NODE into the current buffer at point, laid out to fit WIDTH.

On one line where it fits on the line so far, and broken up where it does
not.  What it is measured against is the column point is at, so a form
that is written far in has that much less room - see the commentary."
  (let ((flat (replique-pprint--width node)))
    (if (and flat (<= (+ (current-column) flat) width))
        (replique-pprint--emit-flat node)
      (replique-pprint--emit-broken node width))))


;;;; What can be laid out

(defun replique-pprint--commented-p (node)
  "Return non-nil when a comment is written anywhere in NODE."
  (or (eq 'comment (replique-parse-type node))
      (let ((children (replique-parse-children node))
            (found nil))
        (while (and children (not found))
          (setq found (replique-pprint--commented-p (car children)))
          (setq children (cdr children)))
        found)))

(defun replique-pprint--check (node)
  "Signal unless NODE is data that can be written back out.

Two things stop it.  Text that did not read, which is what an unbalanced
form is: what would be written back is not what was read.  And a comment,
which has nowhere to go - see the commentary.

Refusing the first is also what lets the rest of this take the tree as it
finds it: whatever did not read said so in the node it did not read into,
and a node says as much for everything below it - see
`replique-parse-error'."
  (when (replique-parse-error node)
    (user-error "This does not read as Clojure data"))
  (when (replique-pprint--commented-p node)
    (user-error "A comment cannot be laid out - this lays out data")))

(defun replique-pprint--lay-out (node width column)
  "Return NODE written out to fit WIDTH columns, starting at COLUMN.

COLUMN because a value is not always written at the left margin - the one
a repl prints starts after the prompt - and what is laid out to fit a
width has to know where it begins to know how much of it is left.

Called from the buffer NODE was read out of, which is where the text of
every token still is: what is written out is written somewhere else."
  (let ((replique-pprint--source (current-buffer)))
    (with-temp-buffer
      (insert (make-string column ?\s))
      (let ((replique-pprint--widths (make-hash-table :test #'equal)))
        (replique-pprint--emit node width))
      (buffer-substring-no-properties (+ (point-min) column) (point-max)))))


;;;; Finding what to lay out

(defun replique-pprint--prompt-p (node)
  "Return non-nil when NODE is a repl prompt rather than something written.

A repl buffer is read as Clojure, transcript and all, so `user=>\\=' is a
symbol there and a symbol is a form.  It is also the form immediately
before point everywhere point usually is in a repl buffer, which is
exactly where the value that was just printed is what somebody means -
see `replique-prompt-text\\=' for why the prompt says so in a property of
its own rather than being recognised by how it looks."
  (and node (replique-prompt-at-p (replique-parse-start node))))

(defun replique-pprint--before (pos)
  "Return the top level form ending before POS, or nil.

The comments behind POS are skipped, which is what `replique-eval-last-sexp\\='
does with them: a comment is not a form, and what was asked for is the form
before it.  Behind point only - a comment POS is in is one point was put on,
and that one is refused rather than read past.

The prompts are skipped for a reason one step further along: a comment is
something somebody wrote and a prompt is not written by anybody.  Skipped
one after another, because a directive moves the repl without evaluating
anything and leaves two prompts standing together."
  (let ((node (replique-parse-form-before pos))
        (done nil))
    (while (and node (not done))
      (if (or (eq 'comment (replique-parse-type node))
              (replique-pprint--prompt-p node))
          ;; strictly back each time, so this ends at the top of the buffer
          ;; on a buffer that is nothing but comments
          (setq node (replique-parse-form-before (replique-parse-start node)))
        (setq done t)))
    node))

(defun replique-pprint--form-at (pos)
  "Return the form to lay out for point at POS, or nil.

The one POS is in, or - where POS is in none - the one before it.  The
second is what makes the command work at the end of a repl buffer, where
point is after the value that was printed rather than in it.

POS IN THE PROMPT IS POS IN NOTHING.  Point on the prompt is point where
the repl is waiting, and what somebody means there is the value above it -
the same thing they mean from the end of the line, which is the same
place."
  (let ((node (replique-parse-form-at pos)))
    (if (and node (not (replique-pprint--prompt-p node)))
        node
      (replique-pprint--before pos))))


;;;; Commands

(defun replique-pprint-string (text &optional width)
  "Return TEXT, which must be Clojure data, laid out to fit WIDTH columns.

WIDTH defaults to `replique-pprint-width'.  Several forms in TEXT come
back one to a line, each laid out on its own.  Nothing is evaluated and
nothing is read: what comes back is the same tokens in another
arrangement - see `replique-pprint--check' for the two arrangements
this refuses to make."
  (with-temp-buffer
    (insert text)
    (let ((root (replique-parse-buffer)))
      (replique-pprint--check root)
      (mapconcat (lambda (node)
                   (replique-pprint--lay-out
                    node (or width replique-pprint-width) 0))
                 (replique-pprint--elements root)
                 "\n"))))

;;;###autoload
(defun replique-pprint ()
  "Lay out the data at point so that it fits `replique-pprint-width'.

The form point is in, or the one before it - which is the one a repl just
printed, when point is at the prompt after it.  It is replaced by the
same tokens in another arrangement, as one change, so one \\[undo] puts
it back.

Data, not code: see the commentary.  A form that did not parse and one
with a comment in it are both refused rather than guessed at, and so is a
token: there is nowhere in one to break a line, so laying it out is
writing it back exactly as it stands.  Saying so is the point.  A command
that answers by doing nothing is a command that cannot be told from one
that did not run - which is how the prompt stood in front of every value
a repl printed without anybody noticing, since laying out `user=>\\=' put
`user=>\\=' back."
  (interactive)
  (save-restriction
    ;; The forms are the whole buffer's, so the one found can reach past what
    ;; a narrowing left - and a form that cannot be written back is worse
    ;; than one written back outside the narrowing
    (widen)
    (let* ((node (or (replique-pprint--form-at (point))
                     (user-error "Nothing to lay out here"))))
      (replique-pprint--check node)
      (unless (replique-parse-children node)
        (user-error "A %s is one token - there is nothing to lay out"
                    (replique-parse-type node)))
      (let* ((start (replique-parse-start node))
             (end (replique-parse-end node))
             (column (save-excursion (goto-char start) (current-column)))
             (text (replique-pprint--lay-out node replique-pprint-width column)))
        (unless (equal text (buffer-substring-no-properties start end))
          ;; No boundary between the two, so one undo is what puts it back
          (save-excursion
            (delete-region start end)
            (goto-char start)
            (insert text)))))))

(provide 'replique-pprint)

;;; replique-pprint.el ends here
