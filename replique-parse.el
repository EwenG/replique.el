;;; replique-parse.el --- Reading Clojure into a tree  -*- lexical-binding: t; -*-

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

;; Clojure text, read into a tree, in Emacs Lisp.
;;
;; What this is for is everything that has to know the shape of what is
;; written: laying data out over lines, painting it, indenting it, saying
;; which locals are in scope at a position, saying what form point is in.
;; Each of those is a walk over the same shape, and this is the shape.
;;
;; A node is a vector of five slots - a type, where it starts, where it ends,
;; what it is made of, and what is wrong with it.  Five `aref's.  That is the
;; whole representation, and the reason for it is that every consumer here is
;; Emacs Lisp walking the tree: a walk pays for each field it reads, and a
;; field read out of a vector is a machine instruction where a field read out
;; of a parse held on the other side of a module boundary is a call.  Over the
;; kind of buffer this gets pointed at the difference is the larger half of
;; the cost - walking a tree is dearer than building one.
;;
;; The tree is built eagerly over whatever region it is asked for, and the
;; laziness is the caller's: ask for the top level form point is in, not for
;; the buffer.  Clojure is a sequence of top level forms and every question
;; above is a question about one of them, so the region is always small and
;; already delimited.  That is also what stands in for incremental reparsing -
;; a form is read again when it changes, and reading one is cheap enough that
;; keeping the old one around and patching it would cost more than it saves.
;; It is worth saying that the alternative was tried: the reader in master is
;; lazy the other way, jumping a collection with `scan-sexps' and descending
;; only where the caller is heading, and what it buys is not worth what it
;; costs - a collection jumped over has no children at all, so a second
;; question about the same form reads it a second time, and `scan-sexps' has
;; to be lied to about `#_' and about `\\(' before it will jump correctly.
;;
;; Nothing here interprets.  A reader conditional is a node with a list in
;; it, not the branch some platform would take; metadata is a node with two
;; things in it, not an annotation moved onto the second.  Interpreting is
;; what master's reader does, and it is why master's reader cannot be used to
;; paint or to lay out text - a tree that has already chosen the `:clj' branch
;; cannot write back what was written.  Choosing is a question the consumers
;; ask, and `replique-parse-target' and friends below are what they ask it
;; with.
;;
;; Nothing here signals, either.  Text being typed is text that does not read:
;; `(defn foo [' is the normal state of a buffer, not an unusual one, and a
;; reader that threw on it would be unusable for the only job it has.  Every
;; construct that can be left unfinished has an end - the end of the region -
;; and a node that ends there says so in its fifth slot.  The slot carries the
;; node's own complaint, or `t' where the complaint is somewhere below it, so
;; asking whether a whole form reads is one `aref' rather than a walk.

;;; Code:

(require 'subr-x)


;;;; What splits one token from the next
;;
;; Clojure's reader stops a token at whitespace and at a terminating macro
;; character.  Which characters those are is written here as a syntax table
;; rather than as a `skip-chars-forward' set, and the reason is worth putting
;; down because the two look interchangeable and are not: `skip-chars-forward'
;; parses its set into a lookup table on every call, and a reader calls it
;; once per token, so the parsing of the set is most of the cost of the scan.
;; `skip-syntax-forward' looks characters up in a table that is already built.
;; On four hundred kilobytes of Clojure the same scan is twice as quick, and
;; there is no accuracy given up for it - the table below says exactly what
;; the set said, including the Unicode whitespace.
;;
;; Whitespace is what Java's `Character.isWhitespace' says it is, plus the
;; comma - and Java's answer is not Unicode's: the three non breaking spaces
;; U+00A0, U+2007 and U+202F are whitespace to Unicode and are not whitespace
;; to Clojure, so a symbol may be written with one in it.
;;
;; Terminating are `"' `;' `@' `^' `` ` '' `~' and the six brackets and the
;; backslash.  Not `#', not `'' and not `%': those three are reader macros
;; that may also be written inside a name, which is why `foo'bar' is one
;; symbol and `foo@bar' is two forms.
;;
;; The colon and the slash are given the word class and everything else that
;; may be written in a token is given the symbol class.  Nothing here wants
;; words - it is so that one `skip-syntax-forward' answers whether a token
;; holds either of the two characters that make reading it more than looking
;; at it, which is what keeps the name every symbol is written with off the
;; slow path.

(defconst replique-parse--syntax-table
  (let ((table (make-char-table 'syntax-table (string-to-syntax "_")))
        (whitespace (string-to-syntax " "))
        (terminating (string-to-syntax ".")))
    (dolist (c '(?\s ?\t ?\n ?\v ?\f ?\r ?,))
      (aset table c whitespace))
    (dolist (c '(?\x1680 ?  ?  ?\x205f ?　))
      (aset table c whitespace))
    (set-char-table-range table '(?\x1c . ?\x1f) whitespace)
    (set-char-table-range table '(?\x2000 . ?\x2006) whitespace)
    (set-char-table-range table '(?\x2008 . ?\x200a) whitespace)
    (dolist (c '(?\" ?\; ?@ ?^ ?\` ?~ ?\( ?\) ?\[ ?\] ?{ ?} ?\\))
      (aset table c terminating))
    (dolist (c '(?: ?/))
      (aset table c (string-to-syntax "w")))
    table)
  "What a character is to Clojure's reader.

Whitespace, a character that terminates a token, the colon and the slash,
or an ordinary character a token may be written with.")

(defconst replique-parse--gaps '(comment discard shebang)
  "The node types that stand between forms rather than being one.

They are kept in the tree - a comment is written somewhere and whoever
wrote it meant it there - and they are kept out of the counting, so that
the target of `#_' is the form after it whatever is written in between.")


;;;; The shape of a node

(defsubst replique-parse-type (node)
  "The kind of form NODE is, as a symbol."
  (aref node 0))

(defsubst replique-parse-start (node)
  "Where NODE starts, as a position in the buffer it was read from."
  (aref node 1))

(defsubst replique-parse-end (node)
  "Where NODE ends, as a position in the buffer it was read from."
  (aref node 2))

(defsubst replique-parse-children (node)
  "What NODE is made of, in the order it is written, or nil for a token."
  (aref node 3))

(defsubst replique-parse-error (node)
  "What is wrong with NODE, or nil when nothing is.

A symbol saying what - `unclosed' for a form the region ends inside of,
`mismatched' for one closed by the wrong bracket, `unmatched' for a
bracket that closes nothing, `odd' for a map of an odd number of forms,
`eof' for a reader macro with nothing after it, `invalid' for a token
that is not written the way its kind is written.

Or t, which says the node itself is fine and something below it is not.
Which is the point of keeping it in a slot rather than working it out:
asking whether a form reads at all is this, and not a walk."
  (aref node 4))

(defsubst replique-parse-text (node &optional buffer)
  "The text NODE was read from, out of BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (buffer-substring-no-properties (aref node 1) (aref node 2))))

(defsubst replique-parse-gap-p (node)
  "Return non-nil when NODE stands between forms rather than being one."
  (memq (aref node 0) replique-parse--gaps))

(defun replique-parse-forms (node)
  "What NODE is made of, with the comments and discarded forms left out."
  (let ((forms nil))
    (dolist (child (aref node 3))
      (unless (memq (aref child 0) replique-parse--gaps)
        (push child forms)))
    (nreverse forms)))

(defun replique-parse-target (node)
  "The form the reader macro NODE is written in front of, or nil for none.

Nil where the macro is written in front of nothing, which is a trailing
`#_' or a `^' at the end of the buffer.

The last of what it is made of, whatever the macro is - which is one rule
where a grammar needs a table, because `#inst \"…\"' and `#:foo{…}' and
`^:private x' and `'x' all put what they apply to last.  What is written
before it is the macro's own text and is read straight out of the buffer,
so nothing here has to know that a tagged literal is a `#' and a symbol
while a namespaced map is a `#' and a keyword."
  (let ((children (aref node 3))
        (target nil))
    (dolist (child children)
      (unless (memq (aref child 0) replique-parse--gaps)
        (setq target child)))
    target))


;;;; Reading
;;
;; Recursive descent over the buffer, with point as the position.  The region
;; being read is narrowed to rather than passed down, so that end of region
;; and end of buffer are the same thing: every `skip-chars-forward' stops at
;; it on its own, and no function here carries a limit it would have to check.
;;
;; `replique-parse--read' answers nil in two cases that are the same case to
;; its caller and different cases to the caller above it - the region has run
;; out, and there is a closing bracket here.  Which of the two it was is read
;; off the buffer by whoever wanted to know, and that is what makes recovery
;; work: a bracket that does not close the form being read is left where it
;; is, so the form outside can close on it.  `[(]' is then a vector holding a
;; list that was closed by the wrong bracket, rather than one blob of text
;; that did not parse.

(defsubst replique-parse--skip-whitespace ()
  "Move point past whitespace, of which the comma is some."
  (skip-syntax-forward " "))

(defsubst replique-parse--child-error (children)
  "Return t when any of CHILDREN has something wrong with it or below it."
  (let ((err nil))
    (while children
      (if (aref (car children) 4)
          (setq err t children nil)
        (setq children (cdr children))))
    err))

(defun replique-parse--read-body ()
  "Read forms until a closing bracket or the end of the region.
Point is left on the bracket, or at the end."
  (let ((children nil)
        (node nil))
    (replique-parse--skip-whitespace)
    (while (setq node (replique-parse--read))
      (push node children)
      (replique-parse--skip-whitespace))
    (nreverse children)))

(defun replique-parse--read-delimited (type start close)
  "Read a collection of TYPE opened at START and closed by CLOSE.
Point is just past the opening text."
  (let* ((children (replique-parse--read-body))
         (err (replique-parse--child-error children)))
    (cond
     ((eq (char-after) close)
      (forward-char 1))
     ;; A bracket that is not this form's is left for the form outside, which
     ;; is what lets one missing bracket cost one form rather than all of them
     ((memq (char-after) '(?\) ?\] ?\}))
      (setq err 'mismatched))
     (t (setq err 'unclosed)))
    (vector type start (point) children err)))

(defun replique-parse--pairs (children)
  "Group CHILDREN into the entries of a map.
Return a cons of the grouping and whether a key was left without a value.

A comment or a discarded form between a key and its value goes inside the
entry, and one between entries stays between them, so that what is
written where stays where it is written.  A discarded form is why this
counts entries rather than halving a length: `{:a #_1 2}' is one entry."
  (let ((out nil)
        (key nil)
        (pending nil)
        (odd nil))
    (dolist (child children)
      (if (memq (aref child 0) replique-parse--gaps)
          (if key (push child pending) (push child out))
        (if (null key)
            (setq key child)
          (let* ((inside (cons key (nreverse (cons child pending))))
                 (err (replique-parse--child-error inside)))
            (push (vector 'pair (aref key 1) (aref child 2) inside err) out)
            (setq key nil pending nil)))))
    (when key
      (setq odd t)
      (push key out)
      (dolist (gap (nreverse pending)) (push gap out)))
    (cons (nreverse out) odd)))

(defun replique-parse--read-map (type start close)
  "Read a map of TYPE opened at START and closed by CLOSE, in entries."
  (let* ((node (replique-parse--read-delimited type start close))
         (grouped (replique-parse--pairs (aref node 3))))
    (aset node 3 (car grouped))
    (when (and (cdr grouped) (null (aref node 4)))
      (aset node 4 'odd))
    node))

(defun replique-parse--read-operands (n)
  "Read N forms, keeping whatever is written between them.
Return a cons of everything read and whether fewer than N forms were there."
  (let ((children nil)
        (got 0)
        (node t))
    (while (and (< got n) node)
      (replique-parse--skip-whitespace)
      (setq node (replique-parse--read))
      (when node
        (push node children)
        (unless (memq (aref node 0) replique-parse--gaps)
          (setq got (1+ got)))))
    (cons (nreverse children) (< got n))))

(defun replique-parse--read-prefixed (type start n)
  "Read a reader macro of TYPE opened at START taking N forms.
Point is just past the macro's own text."
  (let* ((marker-end (point))
         (read (replique-parse--read-operands n))
         (children (car read))
         (err (if (cdr read) 'eof (replique-parse--child-error children)))
         ;; Ending where the last form ends rather than where reading stopped,
         ;; so that the whitespace after it belongs to whoever comes next
         (end (if children (aref (car (last children)) 2) marker-end)))
    (goto-char end)
    (vector type start end children err)))

(defun replique-parse--read-string (type start)
  "Read a string of TYPE opened at START, point on its opening quote.

A backslash takes the character after it whatever it is, which is the one
rule a string and a regexp share - `#\"\\\\\"\"' holds a quote in both."
  (forward-char 1)
  (let ((err nil)
        (done nil))
    (while (not done)
      (skip-chars-forward "^\"\\\\")
      (cond
       ((eobp) (setq done t err 'unclosed))
       ((eq (char-after) ?\") (forward-char 1) (setq done t))
       (t (forward-char 1)
          (if (eobp) (setq done t err 'unclosed) (forward-char 1)))))
    (vector type start (point) nil err)))

(defun replique-parse--read-line (type start)
  "Read a comment or a shebang of TYPE opened at START, up to the newline."
  (end-of-line)
  (vector type start (point) nil nil))


;;;; Tokens
;;
;; A token is read in one call - `skip-chars-forward' to the first character
;; that splits it - and then classified from the text it turned out to be.
;; Which is the order Clojure's own reader works in, and it is worth keeping:
;; what is and is not a number is three regexps applied to a finished token,
;; where scanning for one character at a time turns the same rules into a
;; state machine with a flag per rule.  The three below are the ones out of
;; `LispReader', and `0x1F', `2r1011', `22/7', `1.5e3M' and `08' come out of
;; them saying what they say in Clojure - `08' is a number written wrongly
;; rather than a symbol, because a token that starts with a digit is a number
;; or it is nothing.

(defconst replique-parse--integer-re
  "\\`[-+]?\\(?:0\\|\\([1-9][0-9]*\\)\\|0[xX]\\([0-9A-Fa-f]+\\)\\|0\\([0-7]+\\)\\|\\([1-9][0-9]?\\)[rR]\\([0-9A-Za-z]+\\)\\|0[0-9]+\\)N?\\'"
  "How an integer is written, in decimal, hex, octal or any radix.")

(defconst replique-parse--ratio-re
  "\\`[-+]?[0-9]+/[0-9]+\\'"
  "How a ratio is written.  The denominator carries no sign.")

(defconst replique-parse--float-re
  "\\`[-+]?[0-9]+\\(?:\\.[0-9]*\\)?\\(?:[eE][-+]?[0-9]+\\)?M?\\'"
  "How a float is written.  `1M' is one of them: a big decimal.")

(defconst replique-parse--symbol-re
  "\\`:?\\(?:[^0-9/].*/\\)?\\(?:/\\|[^0-9/][^/]*\\)\\'"
  "How a symbol or a keyword is written.

The namespace runs to the last slash rather than the first, which is what
the pattern in `LispReader' says and is not what reading left to right
would give: `a/b/c' is the name `c' in the namespace `a/b'.")

(defun replique-parse--radix-digits-p (digits base)
  "Return non-nil when every one of DIGITS is a digit in BASE."
  (let ((ok (and (>= base 2) (<= base 36)))
        (i 0)
        (n (length digits)))
    (while (< i n)
      (let* ((c (aref digits i))
             (v (cond ((and (>= c ?0) (<= c ?9)) (- c ?0))
                      ((and (>= c ?a) (<= c ?z)) (+ 10 (- c ?a)))
                      ((and (>= c ?A) (<= c ?Z)) (+ 10 (- c ?A)))
                      (t 99))))
        (if (< v base) (setq i (1+ i)) (setq ok nil i n))))
    ok))

(defun replique-parse--number-p (text)
  "Return non-nil when TEXT is a number written the way Clojure writes one."
  (cond
   ((string-match replique-parse--integer-re text)
    (let ((radix (match-string 4 text)))
      (cond
       (radix (replique-parse--radix-digits-p
               (match-string 5 text) (string-to-number radix)))
       ((or (match-string 1 text) (match-string 2 text) (match-string 3 text)) t)
       ;; What is left is a zero, or a leading zero followed by a digit that
       ;; is not an octal one - `0' is a number and `08' is not
       (t (and (string-match-p "\\`[-+]?0N?\\'" text) t)))))
   ((string-match-p replique-parse--ratio-re text) t)
   ((string-match-p replique-parse--float-re text) t)
   (t nil)))

(defun replique-parse--name-slow (text body)
  "Return non-nil when TEXT, whose name opens at BODY, is written as one.

BODY is where the name proper starts, which is after the colon a keyword
opens with or the two an auto resolved one does.

The namespace runs to the last slash rather than the first, which is what
the pattern in `LispReader' says and is not what reading left to right
would give: `a/b/c' is the name `c' in the namespace `a/b'.  Unless that
last slash is itself the name - `clojure.core//' is the division of that
namespace - in which case the namespace stops at the slash before it.

A double colon may only open a name, and neither the name nor the
namespace may end in a colon, so `a::b', `foo:' and `a:/b' are written
wrongly.  A colon anywhere else is an ordinary character, which is why
`:a:b' is a keyword."
  (let ((n (length text))
        (i 0)
        (slash -1)
        (previous-slash -1)
        (double nil))
    (while (< i n)
      (let ((c (aref text i)))
        (cond
         ((and (eq c ?/) (>= i body))
          (setq previous-slash slash slash i))
         ((and (eq c ?:) (>= i 2) (eq (aref text (1- i)) ?:))
          (setq double t))))
      (setq i (1+ i)))
    (cond
     (double nil)
     ;; The name is the slash itself: the symbol `/', or the keyword `:/'
     ((and (= slash body) (= n (1+ body))) t)
     ((< slash 0)
      (let ((first (aref text body)))
        (and (not (and (>= first ?0) (<= first ?9)))
             (not (eq (aref text (1- n)) ?:)))))
     (t
      (let ((namespace-end slash)
            (name (1+ slash)))
        (when (= slash (1- n))
          (setq name (1- n))
          (setq namespace-end (if (= previous-slash (- n 2)) previous-slash -1)))
        (and (> namespace-end body)
             (let ((first (aref text body)))
               (and (not (and (>= first ?0) (<= first ?9)))
                    (not (eq first ?/))))
             (not (eq (aref text (1- namespace-end)) ?:))
             (< name n)
             (let ((first (aref text name)))
               (if (eq first ?/)
                   (= name (1- n))
                 (and (not (and (>= first ?0) (<= first ?9)))
                      (not (eq (aref text (1- n)) ?:)))))))))))

(defun replique-parse--name-ok (start end)
  "Return non-nil when the token between START and END is written as a name.

Almost every name is a run of ordinary characters and is answered without
being read out of the buffer at all: one `skip-syntax-forward' says
whether a colon or a slash is written in it, and all that is left to ask
of one where neither is, is whether it opens with a digit."
  (let ((body start))
    (when (eq (char-after body) ?:)
      (setq body (1+ body))
      (when (and (< body end) (eq (char-after body) ?:))
        (setq body (1+ body))))
    (and (< body end)
         (save-excursion
           (goto-char body)
           (skip-syntax-forward "_" end)
           (if (= (point) end)
               (let ((first (char-after body)))
                 (not (and (>= first ?0) (<= first ?9))))
             (replique-parse--name-slow
              (buffer-substring-no-properties start end)
              (- body start)))))))

(defconst replique-parse--named-characters
  '("newline" "space" "tab" "backspace" "formfeed" "return")
  "The characters that are written with a name rather than with themselves.")

(defun replique-parse--character-p (text)
  "Return non-nil when TEXT, what follows a backslash, names a character."
  (let ((n (length text)))
    (cond
     ((= n 1) t)
     ((member text replique-parse--named-characters) t)
     ((and (= n 5) (eq (aref text 0) ?u))
      (and (string-match-p "\\`u[0-9A-Fa-f]\\{4\\}\\'" text)
           ;; half of a character is not one
           (let ((code (string-to-number (substring text 1) 16)))
             (not (and (>= code #xD800) (<= code #xDFFF))))))
     ((and (> n 1) (<= n 4) (eq (aref text 0) ?o))
      (and (string-match-p "\\`o[0-7]+\\'" text)
           (<= (string-to-number (substring text 1) 8) 255)))
     (t nil))))

(defun replique-parse--read-character (start)
  "Read a character literal opened at START, point on the backslash.

The character right after the backslash is taken whatever it is, so that
`\\(' and `\;' and `\\ ' are the three characters they are written as
rather than the brackets and the comment and the space they look like."
  (forward-char 1)
  (if (eobp)
      (vector 'character start (point) nil 'eof)
    (forward-char 1)
    (skip-syntax-forward "w_")
    (let ((text (buffer-substring-no-properties (1+ start) (point))))
      (vector 'character start (point) nil
              (unless (replique-parse--character-p text) 'invalid)))))

(defun replique-parse--read-token (start)
  "Read a symbol, keyword, number, boolean or nil opened at START.

Which of them it is is told from the character it opens with, and that is
enough for all of them: a token opening with a digit - or with a sign and
a digit - is a number or it is nothing, and what Clojure does with
`123abc' is refuse it rather than read a symbol out of it."
  (let ((c (char-after start)))
    (skip-syntax-forward "w_")
    (let* ((end (point))
           (length (- end start)))
      (cond
       ((eq c ?:)
        (vector 'keyword start end nil
                (unless (replique-parse--name-ok start end) 'invalid)))
       ((or (and (>= c ?0) (<= c ?9))
            (and (or (eq c ?-) (eq c ?+))
                 (> length 1)
                 (let ((d (char-after (1+ start)))) (and (>= d ?0) (<= d ?9)))))
        (vector 'number start end nil
                (unless (replique-parse--number-p
                         (buffer-substring-no-properties start end))
                  'invalid)))
       ((and (eq c ?n) (= length 3)
             (equal "nil" (buffer-substring-no-properties start end)))
        (vector 'null start end nil nil))
       ((and (or (eq c ?t) (eq c ?f))
             (or (= length 4) (= length 5))
             (member (buffer-substring-no-properties start end)
                     '("true" "false")))
        (vector 'boolean start end nil nil))
       (t (vector 'symbol start end nil
                  (unless (replique-parse--name-ok start end) 'invalid)))))))

;;;; Dispatch
;;
;; `#' is not one macro but a dozen, told apart by the character after it,
;; and the one thing they have in common is that `#' is not a character a
;; token may open with.  Whatever is not one of the dozen is a tagged
;; literal - a tag and the form it tags - which is the rule that keeps a
;; reader tag nobody here has heard of from being taken apart wrongly.

(defun replique-parse--read-dispatch (start)
  "Read whatever the `#' at START opens."
  (let ((c2 (char-after (1+ start))))
    (cond
     ((null c2) (forward-char 1) (vector 'tagged start (point) nil 'eof))
     ((eq c2 ?\{) (forward-char 2) (replique-parse--read-delimited 'set start ?\}))
     ((eq c2 ?\() (forward-char 2) (replique-parse--read-delimited 'fn start ?\)))
     ((eq c2 ?\") (forward-char 1) (replique-parse--read-string 'regex start))
     ((eq c2 ?\') (forward-char 2) (replique-parse--read-prefixed 'var-quote start 1))
     ((eq c2 ?_) (forward-char 2) (replique-parse--read-prefixed 'discard start 1))
     ((eq c2 ?=) (forward-char 2) (replique-parse--read-prefixed 'eval start 1))
     ;; `#^' is how metadata was written before `^' was
     ((eq c2 ?^) (forward-char 2) (replique-parse--read-prefixed 'meta start 2))
     ((eq c2 ?<) (forward-char 2) (vector 'unreadable start (point) nil 'invalid))
     ((and (eq c2 ?!) (= start (point-min)))
      (replique-parse--read-line 'shebang start))
     ((eq c2 ?#)
      (goto-char (+ start 2))
      (skip-syntax-forward "w_")
      (let ((text (buffer-substring-no-properties (+ start 2) (point))))
        (vector 'symbolic start (point) nil
                (unless (member text '("Inf" "-Inf" "NaN")) 'invalid))))
     ((eq c2 ??)
      (let ((splicing (eq (char-after (+ start 2)) ?@)))
        (goto-char (+ start (if splicing 3 2)))
        (let* ((node (replique-parse--read-prefixed
                      (if splicing 'reader-conditional-splicing 'reader-conditional)
                      start 1))
               (target (replique-parse-target node)))
          ;; What a reader conditional holds is a list, always: the platforms
          ;; and the forms they choose between are written in one
          (when (and target (null (aref node 4)) (not (eq (aref target 0) 'list)))
            (aset node 4 'invalid))
          node)))
     ((eq c2 ?:)
      ;; `#:foo{…}' and `#::alias{…}' and `#::{…}'.  The last of those writes
      ;; a bare `::', which is the namespace the file is in and is not a
      ;; keyword anybody could write on its own - so it is read here rather
      ;; than left to the token reader, which would refuse it
      (goto-char (1+ start))
      (let* ((marker-start (point))
             (_ (skip-syntax-forward "w_"))
             (marker (vector 'keyword marker-start (point) nil
                             (unless (or (= (- (point) marker-start) 2)
                                         (replique-parse--name-ok
                                          marker-start (point)))
                               'invalid)))
             (read (replique-parse--read-operands 1))
             (children (cons marker (car read)))
             (target (car (last children)))
             (err (cond ((cdr read) 'eof)
                        ((replique-parse--child-error children) t)
                        ((not (eq (aref target 0) 'map)) 'invalid))))
        (goto-char (aref target 2))
        (vector 'namespaced-map start (point) children err)))
     (t (forward-char 1) (replique-parse--read-prefixed 'tagged start 2)))))

(defun replique-parse--read ()
  "Read the form at point, or nil at a closing bracket or the end.

Which of those two the nil was is left to whoever asked, by leaving point
where it is: a bracket is still there to be looked at."
  (unless (eobp)
    (let ((c (char-after))
          (start (point)))
      (cond
       ((eq c ?\() (forward-char 1) (replique-parse--read-delimited 'list start ?\)))
       ((eq c ?\[) (forward-char 1) (replique-parse--read-delimited 'vector start ?\]))
       ((eq c ?\{) (forward-char 1) (replique-parse--read-map 'map start ?\}))
       ((eq c ?\)) nil)
       ((eq c ?\]) nil)
       ((eq c ?\}) nil)
       ((eq c ?\") (replique-parse--read-string 'string start))
       ((eq c ?\;) (replique-parse--read-line 'comment start))
       ((eq c ?\\) (replique-parse--read-character start))
       ((eq c ?\') (forward-char 1) (replique-parse--read-prefixed 'quote start 1))
       ((eq c ?\`) (forward-char 1) (replique-parse--read-prefixed 'syntax-quote start 1))
       ((eq c ?@) (forward-char 1) (replique-parse--read-prefixed 'deref start 1))
       ((eq c ?^) (forward-char 1) (replique-parse--read-prefixed 'meta start 2))
       ((eq c ?~)
        (if (eq (char-after (1+ start)) ?@)
            (progn (forward-char 2)
                   (replique-parse--read-prefixed 'unquote-splicing start 1))
          (forward-char 1)
          (replique-parse--read-prefixed 'unquote start 1)))
       ((eq c ?#) (replique-parse--read-dispatch start))
       (t (replique-parse--read-token start))))))


;;;; What to ask for

(defun replique-parse-region (from to)
  "Read the text between FROM and TO, and return the tree of it.

The root is a node of its own holding the forms written there, so that a
region holding none and a region holding several are the same shape.

This is what to call with a top level form, or with a window, or with a
string something printed - not with a buffer, unless the buffer is the
question.  Reading is quick and reading again is quicker than keeping the
last answer in step with an edit, which is the whole of the story about
incremental reparsing here: the unit that gets read again is the top level
form that changed, and `replique-parse-top-level-at' is how to find it."
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region from to)
      (goto-char from)
      (with-syntax-table replique-parse--syntax-table
        ;; A `syntax-table' text property is somebody else's answer to the
        ;; question this table answers, and looking for one costs a check
        ;; per character scanned
        (let ((parse-sexp-lookup-properties nil)
              (children nil)
              (node nil))
          (replique-parse--skip-whitespace)
          (while (not (eobp))
            (setq node (replique-parse--read))
            (if node
                (push node children)
              ;; A bracket that closes nothing.  It is one node wide and reading
              ;; carries on past it, which is what keeps one stray bracket from
              ;; costing the rest of the buffer
              (push (vector 'unmatched (point) (1+ (point)) nil 'unmatched) children)
              (forward-char 1))
            (replique-parse--skip-whitespace))
          (setq children (nreverse children))
          (vector 'root from to children
                  (replique-parse--child-error children)))))))

(defun replique-parse-buffer ()
  "Read the whole of what is accessible in the current buffer."
  (replique-parse-region (point-min) (point-max)))

(defun replique-parse-covers-p (node pos)
  "Return non-nil when NODE is written over POS.

Its start counts and its end does not, so that the position between two
forms belongs to neither and the position a form opens at belongs to it."
  (and (<= (aref node 1) pos) (< pos (aref node 2))))

(defun replique-parse-path (root pos)
  "The nodes of ROOT's tree written over POS, outermost first."
  (let ((path (list root))
        (node root)
        (descended t))
    (while descended
      (setq descended nil)
      (let ((children (aref node 3)))
        (while children
          (let ((child (car children)))
            (if (and (<= (aref child 1) pos) (< pos (aref child 2)))
                (progn (push child path)
                       (setq node child descended t children nil))
              (setq children (cdr children)))))))
    (nreverse path)))

(defun replique-parse-node-at (root pos)
  "The innermost node of ROOT's tree written over POS."
  (car (last (replique-parse-path root pos))))

(defun replique-parse-top-level-at (root pos)
  "The form of ROOT written over POS, or nil when none is."
  (let ((children (aref root 3))
        (found nil))
    (while children
      (let ((child (car children)))
        (if (and (<= (aref child 1) pos) (< pos (aref child 2)))
            (setq found child children nil)
          (setq children (cdr children)))))
    found))


;;;; Reading a name
;;
;; What a symbol or a keyword is written under, worked out from its text
;; rather than held in the tree.  A grammar that splits every name into a
;; namespace node and a name node spends two nodes on each of them and the
;; consumers put them back together again; this is one `string-match' asked
;; for by whoever wants the answer.

(defun replique-parse-auto-resolve-p (text)
  "Return non-nil when TEXT is a keyword written to resolve where it stands."
  (string-prefix-p "::" text))

(defun replique-parse-name-parts (text)
  "The namespace TEXT is written under and its name, as a cons.

The namespace runs to the last slash rather than the first, so `a/b/c' is
the name `c' in the namespace `a/b'.  Unless the last slash is itself the
name, which is how `clojure.core//' is written; and a name that is
nothing but a slash has no namespace, which is how `/' is."
  (let* ((offset (cond ((string-prefix-p "::" text) 2)
                       ((string-prefix-p ":" text) 1)
                       (t 0)))
         (body (substring text offset))
         (n (length body))
         (slash (and (> n 1) (string-match "/[^/]*\\'" body))))
    (when (and slash (= slash (1- n)))
      (setq slash (string-match "/[^/]*/\\'" body)))
    (if (and slash (> slash 0))
        (cons (substring body 0 slash) (substring body (1+ slash)))
      (cons nil body))))

(provide 'replique-parse)

;;; replique-parse.el ends here
