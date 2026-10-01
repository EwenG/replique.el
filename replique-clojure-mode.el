;; replique-clojure-mode.el ---   -*- lexical-binding: t; -*-

;; Copyright © 2026 Ewen Grosjean

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

;; `replique-clojure-mode' is the syntax layer for Clojure and ClojureScript.
;; It owns painting, indentation and moving about, and reads all three out
;; of `replique-parse' - the reader, rather than a grammar:
;;
;;   * painting - what each token is, and then what the form it is written
;;     in says it is.  One walk down each form, and no precedence between
;;     rules, which is the whole of the design; the section below says why.
;;   * cljfmt indentation - cljfmt's rules, configured the way the project
;;     configures cljfmt (see `replique-cljfmt') and followed the way cljfmt
;;     follows them, so that `clojure-lsp format' agrees with the editor.
;;     Read once per form and then followed, so that indenting a file reads
;;     it once.
;;   * a defun is a top level form, any of them, so that moving to the top
;;     of the form point is in moves to the top of the form point is in.
;;     Moving over an expression is left to the syntax table, which already
;;     knows the reader macros.
;;
;; There is no grammar, no parser and no `treesit' here: a buffer needs
;; nothing installed in it to be read as Clojure.
;;
;; Semantic faces (`:local', `:macro-invocation', `:special-form',
;; `:unresolved', unused greyout) and diagnostics are not here and are not
;; part of replique: they are computed by a C module that is layered over
;; these faces as an overlay.
;;
;; What replique needs of it is neither of those.  A form is sent to a repl
;; with the line it was written on, so replique has to agree with the reader
;; about where a form begins - a question sexp motion answers wrongly for #_
;; and for metadata - and this is the parse it asks.  See `replique-eval'.
;;
;; Customization (M-x customize-group RET replique-clojure RET):
;;   `replique-clojure-font-lock-level'           how much is painted, 1 to 4
;;   `replique-clojure-extra-def-forms'           macros highlighted like defn
;;   `replique-clojure-semantic-indent-rules'     per-symbol indent overrides
;;   `replique-clojure-docstring-fill-column'     fill-column for docstrings
;;   `replique-clojure-docstring-fill-prefix-width'  docstring fill prefix
;; The faces are themeable via the deffaces below, and the indent rules /
;; extra def-forms are `.dir-locals.el'-friendly.

;;; Code:

(require 'seq)
(require 'replique-parse)
(require 'replique-cljfmt)
(eval-when-compile (require 'subr-x))   ; thread-first / thread-last / when-let*


;;;; Customization

(defgroup replique-clojure nil
  "Reading Clojure in a buffer: what it is painted, how it is indented."
  :prefix "replique-clojure-"
  :group 'languages)

(defcustom replique-clojure-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe #'integerp)

(defcustom replique-clojure-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value follows the de-facto Clojure convention, aligning
continuation lines with the opening double quote on the third column."
  :type 'integer
  :safe #'integerp)


;;;; Faces
;;
;; Grammar faces are assigned directly in the font-lock rules.  Two categories
;; that have no good standard face get a dedicated, themeable face here;
;; everything else reuses the standard `font-lock-*' faces, so a theme controls
;; them with no rebuild.

(defface replique-clojure-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face for Clojure keywords (`:something').")

(defface replique-clojure-character-face
  '((t (:inherit font-lock-string-face)))
  "Face for Clojure character literals (`\\a').")


;;;; Font-lock — what a form says its parts are
;;
;; Painting Clojure is two questions asked in that order.  What is this
;; token, which its own text answers - a string is a string wherever it is
;; written.  And then what does the form around it say it is: the second
;; thing in a `defn' is a name being defined and the third may be a
;; docstring, and neither of them looks any different from any other symbol
;; or any other string.
;;
;; The second question is the one worth building around, because there is
;; only ever one form that holds the answer - the one the token is written
;; directly inside.  So it is asked there and nowhere else.  Every list
;; looks up whatever heads it in one table, and the table says what the rest
;; of it means.
;;
;; That is the whole of the difference from the set of tree queries this
;; replaced, where each rule walked the tree on its own and what a character
;; ended up painted was settled by which rule ran last with `:override' on.
;; Here a form is painted for what it is, and then the form around it says
;; what it is for - two shallow passes over one form's own children, and no
;; precedence anywhere.  A rule cannot reach across the buffer to repaint
;; something, because a rule is a branch of a function that was handed one
;; node.
;;
;; Three things follow from that which queries could not do:
;;
;;   - Quoted data is not code.  `'(defn foo [x])' is a list of symbols and
;;     is painted as one.  A query would have to write "and not under a
;;     quote" into every pattern it has; a walk carries it down as an
;;     argument, and the branch that paints definitions is simply not taken.
;;     An unquote inside a syntax quote is code again, which is what makes a
;;     macro body read the way it runs.
;;
;;   - Metadata nests as far as somebody writes it.  `^:a ^:b ^:c ^:d (def x
;;     1)' is four wrappers and a fifth would be five; the queries spelled
;;     out nought to three and stopped seeing the form at the fourth.
;;
;;   - A method name is what heads its list and nothing else is.  The query
;;     for `(deftype T [] P (m [this] this))' matched every symbol written
;;     directly in the method, so `this' came out a function name.
;;
;; Errors are painted out of the reader's own account of them rather than as
;; a blob: a token not written the way its kind is written is marked where
;; it is written, and a bracket that closes nothing is marked by itself.
;; What is deliberately left unmarked is the form somebody is in the middle
;; of typing.  A bracket with nothing closing it yet is every bracket for as
;; long as it takes to type the rest of the line, and painting that red says
;; only that the file is not finished being written.
;;
;; Four levels, in the order somebody reading the screen wants them:
;;
;;   1  what is not code - comments, discarded forms, strings, docstrings
;;   2  what a token is  - numbers, characters, constants, keywords,
;;                         namespaces, reader macros, what is written wrongly
;;   3  what the code means - built-ins, definitions and the names they
;;                         define, types, earmuffs, the arguments of a #()
;;   4  everything else that could be a call, and the brackets themselves

(defcustom replique-clojure-font-lock-level 3
  "How much of the buffer is painted, from 1 to 4.

Each level is the ones before it and more.  One paints what is not code,
two what each token is, three what the code means, and four every head of
a form as a call and the brackets around it.  Three is the default because
four paints a symbol as a call on the strength of it being written first,
which a macro or a quoted list makes a guess rather than a reading."
  :type '(choice (const :tag "What is not code" 1)
                 (const :tag "What each token is" 2)
                 (const :tag "What the code means" 3)
                 (const :tag "Everything" 4))
  :safe #'integerp)


;;;; Font-lock — what heads a form

(defconst replique-clojure-builtin-symbols
  '("do" "if" "let*" "var" "fn" "fn*" "loop*" "recur"
    "throw" "try" "catch" "finally" "set!" "new"
    "monitor-enter" "monitor-exit" "quote" "->" "->>" ".." "."
    "amap" "and" "areduce" "as->" "assert" "binding" "bound-fn"
    "case" "comment" "cond" "cond->" "cond->>" "condp"
    "declare" "def" "definline" "definterface" "defmacro" "defmethod"
    "defmulti" "defn" "defn-" "defonce" "defprotocol" "defrecord"
    "defstruct" "deftype" "delay" "doall" "dorun" "doseq" "dosync"
    "dotimes" "doto" "extend-protocol" "extend-type" "extend"
    "for" "future" "gen-class" "gen-interface" "if-let" "if-not"
    "if-some" "import" "in-ns" "io!" "lazy-cat" "lazy-seq" "let"
    "letfn" "locking" "loop" "memfn" "ns" "or" "proxy" "proxy-super"
    "pvalues" "refer-clojure" "reify" "some->" "some->>" "sync"
    "time" "vswap!" "when" "when-first" "when-let" "when-not"
    "when-some" "while" "with-bindings" "with-in-str"
    "with-loading-context" "with-local-vars" "with-open"
    "with-out-str" "with-precision" "with-redefs" "with-redefs-fn"
    "deftest" "deftest-" "is" "are" "testing")
  "The special forms and core macros painted as built-in when they head a form.

What is here is what the compiler and the core macros read specially, not
every name clojure.core holds.  A function is called the way any other
function is called and reads no better for being coloured differently from
the one beside it; a macro reads its arguments in a way the shape of the
form does not show, which is the thing worth marking.")

(defconst replique-clojure--builtins
  (let ((table (make-hash-table :test #'equal :size 256)))
    (dolist (name replique-clojure-builtin-symbols table)
      (puthash name t table)))
  "`replique-clojure-builtin-symbols' as a set.

A set rather than the `regexp-opt' this replaced.  A query predicate can
only ask a regexp, so membership had to be written as a hundred-branch
alternation matched against every symbol in the buffer; asked from elisp it
is the question that was meant, answered once.")

(defconst replique-clojure--definer-forms
  '(("def"             :name variable :doc value)
    ("defonce"         :name variable)
    ("defn"            :name function :doc name)
    ("defn-"           :name function :doc name)
    ("defmacro"        :name function :doc name)
    ("definline"       :name function :doc name)
    ("defmulti"        :name function :doc name)
    ("defmethod"       :name function)
    ("deftest"         :name function :doc name)
    ("deftest-"        :name function :doc name)
    ("fn"              :name function)
    ("ns"              :name type     :doc name)
    ("deftype"         :name type     :methods 2)
    ("defrecord"       :name type     :methods 2)
    ("defstruct"       :name type)
    ("defprotocol"     :name type     :doc name :methods 2 :method-doc t)
    ("definterface"    :name type     :methods 2 :method-doc t)
    ("extend-type"     :name type     :methods 2)
    ("extend-protocol" :name type     :methods 2)
    ("reify"                          :methods 1)
    ("letfn"           :bindings t))
  "What each form that defines something says about the rest of itself.

One row a head symbol, holding any of:

  :name        what the form after the head is - `variable', `function' or
               `type'.  Only ever painted when what is written there is a
               symbol, so the vector of `(fn [x] x)' is left alone.
  :doc         where a docstring is.  `name' for a string written straight
               after the name, which is the `defn' family; `value' for one
               that only counts when a value follows it, which is `def'
               alone - `(def x \"a\")' defines x as that string.
  :methods     the place of the first form that could be a method, which is
               one for `reify' and two for everything that names itself
               first.  Every list from there on has its head painted as a
               method name, and nothing else in it is touched.
  :method-doc  whether a string written after a method's argument vector is
               that method's docstring, which `defprotocol' and
               `definterface' say and the others do not.
  :bindings    `letfn', whose second form is a vector of lists each headed
               by the name of a local function.

The faces follow clojure-mode, which is what somebody coming to this has
been reading Clojure in: `defmulti' and `defmethod' name functions, and
only the four forms that really do make a type name a type.")

(defconst replique-clojure--definers
  (let ((table (make-hash-table :test #'equal :size 64)))
    (dolist (row replique-clojure--definer-forms table)
      (puthash (car row) (cdr row) table)))
  "`replique-clojure--definer-forms' keyed by the head symbol.")

(defvar-local replique-clojure--extra-definers nil
  "The definers of this buffer, or nil to use `replique-clojure--definers'.

Set where `replique-clojure-extra-def-forms' names macros of somebody's
own, in which case it holds those as well as the built-in ones.")

(defsubst replique-clojure--definer (name)
  "What the form headed by NAME says about the rest of itself, or nil."
  (gethash name (or replique-clojure--extra-definers
                    replique-clojure--definers)))

(defun replique-clojure--head-name (node)
  "The name NODE heads a form under, or nil when it heads none.

Nil for anything that is not a symbol, and for a symbol written under a
namespace that is not clojure.core: `str/join' heads a call and nothing
more, while `clojure.core/when' heads a `when'."
  (when (eq 'symbol (replique-parse-type node))
    (let ((parts (replique-parse-name-parts (replique-parse-text node))))
      (when (or (null (car parts)) (equal "clojure.core" (car parts)))
        (cdr parts)))))


;;;; Font-lock — docstrings
;;
;; Where a docstring is, is a property of the form it is written in and not
;; of the string, which is why it is asked of the head symbol like
;; everything else here.  Two readers want the answer and they must not
;; disagree: the fontifier, which paints it, and `fill-paragraph', which
;; refuses to reflow anything else.  So they ask the same two functions.

(defun replique-clojure--meta-doc (node)
  "The docstring written in the metadata on NODE, or nil where there is none.

Which is the `:doc' of a map written as metadata - `(defn ^{:doc \"...\"} f
[])' - and it is looked for through as many wrappers as somebody wrote,
because metadata written twice is metadata on metadata."
  (let ((found nil))
    (while (eq 'meta (replique-parse-type node))
      (let ((value (car (replique-parse-forms node))))
        (when (eq 'map (replique-parse-type value))
          (dolist (entry (replique-parse-forms value))
            (let* ((inside (replique-parse-forms entry))
                   (key (car inside))
                   (val (cadr inside)))
              (when (and (eq 'keyword (replique-parse-type key))
                         (equal ":doc" (replique-parse-text key))
                         (eq 'string (replique-parse-type val)))
                (setq found val))))))
      (setq node (replique-parse-target node)))
    found))

(defun replique-clojure--doc-nodes (forms rule)
  "The strings among FORMS that are docstrings, the head having said RULE.

FORMS is what the form is made of with the comments left out, RULE is what
a row of `replique-clojure--definer-forms' holds under :doc, and either of
them may be nil - a form that is not a definition can still carry a
docstring in the metadata on its name."
  (let ((found nil)
        (meta (replique-clojure--meta-doc (nth 1 forms))))
    (when meta (push meta found))
    (when rule
      (let ((named (replique-parse-unwrap-meta (nth 1 forms)))
            (third (replique-parse-unwrap-meta (nth 2 forms))))
        ;; A string is the docstring only where a name comes before it.  And
        ;; for `def', only where something comes after it as well: what
        ;; `(def x "a")' defines x as is that string, and painting it as
        ;; prose would be saying the form does something it does not
        (when (and (eq 'symbol (replique-parse-type named))
                   (eq 'string (replique-parse-type third))
                   (or (eq rule 'name) (nth 3 forms)))
          (push third found))))
    found))

(defun replique-clojure--method-doc-node (forms)
  "The docstring among FORMS, read as a method of a protocol or interface.

Which is a string written after the argument vector rather than after the
name, because that is where a method declares one."
  (let ((args (replique-parse-unwrap-meta (nth 1 forms)))
        (third (replique-parse-unwrap-meta (nth 2 forms))))
    (when (and (eq 'vector (replique-parse-type args))
               (eq 'string (replique-parse-type third)))
      third)))

(defun replique-clojure--method-doc-p (node)
  "Return non-nil when the methods NODE holds may declare docstrings."
  (let ((name (replique-clojure--head-name
               (replique-parse-unwrap-meta
                (car (replique-parse-forms node))))))
    (and name (plist-get (replique-clojure--definer name) :method-doc))))

(defun replique-clojure-docstring-bounds (pos)
  "Where the docstring written over POS starts and ends, as a cons.

Nil where POS is not written in one.  What counts as one is asked of the
same functions the fontifier asks, so that what is painted as prose and
what is filled as prose cannot come apart."
  (when-let* ((root (replique-parse-form-at pos))
              (path (nreverse (replique-parse-path root pos))))
    (while (and path (not (eq 'string (replique-parse-type (car path)))))
      (setq path (cdr path)))
    (let ((string (car path)))
      (setq path (cdr path))
      ;; Metadata wraps what it is written on, so a docstring carrying any
      ;; is that many nodes further in than the form holding it
      (while (eq 'meta (replique-parse-type (car path)))
        (setq path (cdr path)))
      (when (and string
                 (eq 'list (replique-parse-type (car path)))
                 (memq string (replique-clojure--form-docstrings
                               (car path) (cadr path))))
        (cons (replique-parse-start string) (replique-parse-end string))))))

(defun replique-clojure--form-docstrings (node around)
  "The docstrings the form NODE holds, it being written inside AROUND.

AROUND is wanted because a method declares its docstring in a place of its
own, and what says a list is a method is the form it is written in."
  (let* ((forms (replique-parse-forms node))
         (name (replique-clojure--head-name
                (replique-parse-unwrap-meta (car forms))))
         (definer (and name (replique-clojure--definer name))))
    (append (replique-clojure--doc-nodes forms (plist-get definer :doc))
            (when (and around (replique-clojure--method-doc-p around))
              (let ((string (replique-clojure--method-doc-node forms)))
                (and string (list string)))))))


;;;; Font-lock — painting
;;
;; One walk down from each top level form the region touches.  A child that
;; is written nowhere near the region is not descended into, so what a call
;; costs is the part of the tree the region covers rather than the size of
;; the form - which is what keeps a file with one bracket missing, where
;; every line is inside the same top level form, from being repainted in
;; full on every keystroke.
;;
;; What is painted is not clipped to the region, only what is walked is.  A
;; string running off the top of the window is painted whole, from wherever
;; it starts, and painting the same character twice with the same face costs
;; nothing and says the same thing.

(defconst replique-clojure--earmuff-regexp "\\`\\*.+\\*\\'"
  "What an earmuffed name looks like.
Stars around something, rather than around nothing: `**' is a name
somebody wrote and not a dynamic var.")

(defvar replique-clojure--fl-from nil
  "The first position the walk in progress is interested in.")

(defvar replique-clojure--fl-to nil
  "The last position the walk in progress is interested in.")

(defvar replique-clojure--fl-in-fn nil
  "Whether the walk in progress is inside a `#()'.

Which is what says that `%' is an argument rather than a name, and it is
carried this way rather than as an argument because that is what it is: a
scope, opened by one node and closed with it.")

(defsubst replique-clojure--put (start end face level)
  "Paint FACE from START to END, where LEVEL is one this buffer paints."
  (when (<= level replique-clojure-font-lock-level)
    (put-text-property start end 'face face)))

(defsubst replique-clojure--put-node (node face level)
  "Paint the whole of NODE with FACE, where LEVEL is one this buffer paints."
  (replique-clojure--put (replique-parse-start node) (replique-parse-end node)
                         face level))

(defun replique-clojure--put-name (node face level)
  "Paint the name part of the symbol NODE with FACE at LEVEL.

What it is written under is left as it was painted, which is what says how
it was reached: the `clojure.core' of `clojure.core/when' is a namespace
being named and the `when' after it is the one everybody knows."
  (let* ((start (replique-parse-start node))
         (namespace (car (replique-parse-name-parts (replique-parse-text node)))))
    (replique-clojure--put (if namespace (+ start (length namespace) 1) start)
                           (replique-parse-end node)
                           face level)))

(defun replique-clojure--marker-end (node)
  "Where the text NODE opens with stops, or nil where it opens with none.

The reader macros do not hold their own marker as a node - what a `#inst'
is made of is the tag and the form it tags - so how wide it is is read off
what kind of macro it is.  Never past the end of the node, which is how a
`#' typed at the end of the buffer is still one character wide."
  (let ((start (replique-parse-start node))
        (width (pcase (replique-parse-type node)
                 ((or 'var-quote 'discard 'eval 'reader-conditional
                      'set 'fn 'unreadable)
                  2)
                 ('reader-conditional-splicing 3)
                 ((or 'namespaced-map 'tagged 'regex) 1)
                 (_ nil))))
    (when width (min (+ start width) (replique-parse-end node)))))

(defun replique-clojure--fl-children (node context)
  "Paint what NODE is made of, as CONTEXT reads it."
  (dolist (child (replique-parse-children node))
    (replique-clojure--fl-node child context)))

(defun replique-clojure--fl-marked (node context &optional face)
  "Paint NODE's own marker with FACE and what it is written on as CONTEXT."
  (let ((marker (replique-clojure--marker-end node)))
    (when (and marker face)
      (replique-clojure--put (replique-parse-start node) marker face 2)))
  (replique-clojure--fl-children node context))

(defun replique-clojure--fl-collection (node context)
  "Paint the collection NODE and what is written inside it, as CONTEXT reads it.

The brackets are the last level: they are already where they are, and a
colour of their own is a preference rather than a reading.  A bracket that
closes nothing is never painted as one, because there is none there - the
node stops where the text ran out."
  (let* ((start (replique-parse-start node))
         (end (replique-parse-end node))
         (open (min (or (replique-clojure--marker-end node) (1+ start)) end)))
    (replique-clojure--put start open 'font-lock-bracket-face 4)
    (when (and (> end open)
               (not (memq (replique-parse-error node) '(unclosed mismatched))))
      (replique-clojure--put (1- end) end 'font-lock-bracket-face 4)))
  (replique-clojure--fl-children node context))

(defun replique-clojure--fl-token (node face level &optional warn)
  "Paint NODE with FACE at LEVEL, or as WARN where it is written wrongly."
  (if (eq 'invalid (replique-parse-error node))
      (replique-clojure--put-node node (or warn 'font-lock-warning-face) 2)
    (replique-clojure--put-node node face level)))

(defun replique-clojure--fl-symbol (node context)
  "Paint the symbol NODE, as CONTEXT reads it."
  (if (eq 'invalid (replique-parse-error node))
      (replique-clojure--put-node node 'font-lock-warning-face 2)
    (let* ((start (replique-parse-start node))
           (parts (replique-parse-name-parts (replique-parse-text node)))
           (namespace (car parts))
           (name (cdr parts)))
      (when namespace
        (replique-clojure--put start (+ start (length namespace))
                               'font-lock-type-face 2))
      (when (eq context 'code)
        (cond
         ;; The arguments of a `#()', which are named nowhere and so read as
         ;; nothing at all unless they are marked.  Only inside one: `%' is
         ;; an ordinary name everywhere else, and a tree is what makes that
         ;; difference tellable where a regexp over the text is not
         ((and replique-clojure--fl-in-fn
               (or (equal name "%") (equal name "%&")
                   (string-match-p "\\`%[1-9][0-9]*\\'" name)))
          (replique-clojure--put-name node 'font-lock-variable-name-face 3))
         ((string-match-p replique-clojure--earmuff-regexp name)
          (replique-clojure--put-name node 'font-lock-warning-face 3)))))))

(defun replique-clojure--fl-keyword (node)
  "Paint the keyword NODE, with whatever it is written under marked."
  (if (eq 'invalid (replique-parse-error node))
      (replique-clojure--put-node node 'font-lock-warning-face 2)
    (let* ((start (replique-parse-start node))
           (text (replique-parse-text node))
           (namespace (car (replique-parse-name-parts text)))
           (colons (if (replique-parse-auto-resolve-p text) 2 1)))
      (replique-clojure--put-node node 'replique-clojure-keyword-face 2)
      (when namespace
        (replique-clojure--put (+ start colons)
                               (+ start colons (length namespace))
                               'font-lock-type-face 2)))))

(defun replique-clojure--fl-node (node context)
  "Paint NODE and what is written inside it, as CONTEXT reads it.

CONTEXT is `code' for what will be run, `quoted' for what a quote has made
data of, and nothing else - what is discarded is painted where it is met
and never walked into."
  (when (and (>= (replique-parse-end node) replique-clojure--fl-from)
             (<= (replique-parse-start node) replique-clojure--fl-to))
    (pcase (replique-parse-type node)
      ((or 'list 'fn)
       (let ((replique-clojure--fl-in-fn
              (if (eq 'fn (replique-parse-type node))
                  t
                replique-clojure--fl-in-fn)))
         (replique-clojure--fl-collection node context)
         ;; What the form is made of is painted for what it is, and then the
         ;; form says what it is for.  That order is the whole of the
         ;; precedence there is here, and it reaches exactly this far
         (when (eq context 'code)
           (replique-clojure--fl-meaning node))))
      ((or 'vector 'map 'set)
       (replique-clojure--fl-collection node context))
      ((or 'pair 'root)
       (replique-clojure--fl-children node context))
      ('symbol (replique-clojure--fl-symbol node context))
      ('keyword (replique-clojure--fl-keyword node))
      ('number (replique-clojure--fl-token node 'font-lock-number-face 2))
      ('character
       (replique-clojure--fl-token node 'replique-clojure-character-face 2))
      ((or 'boolean 'null 'symbolic)
       (replique-clojure--fl-token node 'font-lock-constant-face 2))
      ('string (replique-clojure--put-node node 'font-lock-string-face 1))
      ('regex
       (replique-clojure--put-node node 'font-lock-string-face 1)
       (replique-clojure--put (replique-parse-start node)
                              (replique-clojure--marker-end node)
                              'font-lock-preprocessor-face 2))
      ((or 'comment 'shebang)
       (replique-clojure--put-node node 'font-lock-comment-face 1))
      ('discard
       ;; Not walked into.  What is discarded is not read, so nothing in it
       ;; means anything, and painting it as one thing is what says so
       (replique-clojure--put-node node 'font-lock-comment-face 1)
       (replique-clojure--put (replique-parse-start node)
                              (replique-clojure--marker-end node)
                              'font-lock-comment-delimiter-face 1))
      ((or 'quote 'syntax-quote)
       (replique-clojure--fl-children node 'quoted))
      ((or 'unquote 'unquote-splicing)
       ;; Back to code, which is the point of writing one: what a syntax
       ;; quote holds is data until an unquote says this part of it is not
       (replique-clojure--fl-children node 'code))
      ((or 'reader-conditional 'reader-conditional-splicing)
       (replique-clojure--put (replique-parse-start node)
                              (replique-clojure--marker-end node)
                              'font-lock-preprocessor-face 2)
       ;; What it holds is a list and the list is not a call.  What is
       ;; written in one is platforms and what each of them stands for, so
       ;; nothing there heads anything
       (dolist (child (replique-parse-children node))
         (if (eq 'list (replique-parse-type child))
             (replique-clojure--fl-children child context)
           (replique-clojure--fl-node child context))))
      ('tagged
       (replique-clojure--fl-children node context)
       (let ((tag (car (replique-parse-forms node))))
         (replique-clojure--put (replique-parse-start node)
                                (if tag
                                    (replique-parse-end tag)
                                  (replique-parse-end node))
                                'font-lock-preprocessor-face 2)))
      ('meta
       (replique-clojure--fl-children node context)
       ;; A bare symbol written as metadata is a type hint and nothing else
       (let ((value (car (replique-parse-forms node))))
         (when (and (eq context 'code)
                    (eq 'symbol (replique-parse-type value))
                    (not (eq 'invalid (replique-parse-error value))))
           (replique-clojure--put-node value 'font-lock-type-face 3))))
      ((or 'var-quote 'eval 'namespaced-map)
       (replique-clojure--fl-marked node context 'font-lock-preprocessor-face))
      ((or 'unreadable 'unmatched)
       (replique-clojure--put-node node 'font-lock-warning-face 2))
      (_ (replique-clojure--fl-children node context)))))


;;;; Font-lock — what a form makes of the rest of itself

(defun replique-clojure--fl-meaning (node)
  "Paint the parts of the form NODE for what its head says they are."
  (let* ((forms (replique-parse-forms node))
         (head (replique-parse-unwrap-meta (car forms))))
    (when (eq 'symbol (replique-parse-type head))
      (let* ((name (replique-clojure--head-name head))
             (definer (and name (replique-clojure--definer name))))
        (cond
         ;; `(comment ...)' is read and thrown away, so the head is marked
         ;; the way a `;' is.  What is written inside is left readable: a
         ;; comment form is usually code somebody means to come back to
         ((equal name "comment")
          (replique-clojure--put-name
           head 'font-lock-comment-delimiter-face 3))
         (definer
          (replique-clojure--put-name head 'font-lock-keyword-face 3)
          (replique-clojure--fl-definition forms definer))
         ((and name (gethash name replique-clojure--builtins))
          (replique-clojure--put-name head 'font-lock-keyword-face 3))
         ;; Anything else written first could be a call, and could as well
         ;; be a macro reading its arguments some way of its own or a list
         ;; nobody is going to call at all.  A guess, so it is the last
         ;; level and off by default
         (t (replique-clojure--put-name
             head 'font-lock-function-call-face 4)))))))

(defconst replique-clojure--definition-faces
  '((function . font-lock-function-name-face)
    (variable . font-lock-variable-name-face)
    (type . font-lock-type-face))
  "The face each kind of name a definition gives is painted with.")

(defun replique-clojure--fl-definition (forms definer)
  "Paint what DEFINER says the rest of FORMS is.

FORMS is what a definition form is made of with the comments left out, and
DEFINER the row of `replique-clojure--definer-forms' its head was found
under."
  (let ((kind (plist-get definer :name))
        (methods (plist-get definer :methods))
        (named (replique-parse-unwrap-meta (nth 1 forms))))
    ;; Only where a name is written there.  `(fn [x] x)' names nothing, and
    ;; neither does a `defn' somebody has typed the head of and no more
    (when (and kind (eq 'symbol (replique-parse-type named)))
      (replique-clojure--put-name
       named (cdr (assq kind replique-clojure--definition-faces)) 3))
    (dolist (string (replique-clojure--doc-nodes forms (plist-get definer :doc)))
      (replique-clojure--put-node string 'font-lock-doc-face 1))
    (when methods
      (let ((place 0)
            (method-doc (plist-get definer :method-doc)))
        (dolist (child forms)
          (when (>= place methods)
            (pcase (replique-parse-type child)
              ;; What is named among the methods is a protocol or an
              ;; interface being implemented, which is a type
              ('symbol (replique-clojure--put-name
                        child 'font-lock-type-face 3))
              ('list
               (let* ((inside (replique-parse-forms child))
                      (name (replique-parse-unwrap-meta (car inside))))
                 ;; The head and nothing else.  The query this replaced
                 ;; matched every symbol written directly in the method, so
                 ;; the `this' of `(m [this] this)' came out a function name
                 (when (eq 'symbol (replique-parse-type name))
                   (replique-clojure--put-name
                    name 'font-lock-function-name-face 3))
                 (when method-doc
                   (let ((string (replique-clojure--method-doc-node inside)))
                     (when string
                       (replique-clojure--put-node
                        string 'font-lock-doc-face 1))))))))
          (setq place (1+ place)))))
    (when (plist-get definer :bindings)
      (let ((vector (replique-parse-unwrap-meta (nth 1 forms))))
        (when (eq 'vector (replique-parse-type vector))
          (dolist (binding (replique-parse-forms vector))
            (when (eq 'list (replique-parse-type binding))
              (let ((name (replique-parse-unwrap-meta
                           (car (replique-parse-forms binding)))))
                (when (eq 'symbol (replique-parse-type name))
                  (replique-clojure--put-name
                   name 'font-lock-function-name-face 3))))))))))


;;;; Font-lock — the region

(defun replique-clojure-font-lock-region (beg end &optional _loudly)
  "Paint the Clojure written between BEG and END.

This is the whole of what `font-lock' calls here.  The region is widened to
whole lines and then back to wherever the top level form holding its start
begins, because what a token is painted depends on the form it is written
in and the head of that form may be off the top of the window.

Answers with the region it painted, which is what tells `jit-lock' not to
ask again for the part of it that was not asked for."
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char beg)
        (setq beg (line-beginning-position))
        (goto-char end)
        (setq end (line-end-position))
        (let* ((bounds (replique-parse-top-level-bounds beg))
               (from (if bounds (car bounds) beg))
               (replique-clojure--fl-from beg)
               (replique-clojure--fl-to end)
               (replique-clojure--fl-in-fn nil))
          (with-silent-modifications
            (font-lock-unfontify-region beg end)
            (dolist (form (replique-parse-forms-in from end))
              (replique-clojure--fl-node form 'code)))))))
  `(jit-lock-bounds ,beg . ,end))


;;;; Font-lock — def forms of somebody's own

(defun replique-clojure--compute-extra-definers (names)
  "The definers of a buffer where NAMES define the way `defn' does.
Nil where NAMES is empty, which says to read the built-in ones as they are."
  (when names
    (let ((table (copy-hash-table replique-clojure--definers)))
      (dolist (name names table)
        (puthash name '(:name function :doc name) table)))))

(defun replique-clojure--set-extra-def-forms (symbol value)
  "Setter for `replique-clojure-extra-def-forms'.
Sets SYMBOL to VALUE and repaints every `replique-clojure-mode' buffer."
  (set-default-toplevel-value symbol value)
  (let ((new (replique-clojure--compute-extra-definers value)))
    (dolist (buffer (buffer-list))
      (when (buffer-local-boundp 'replique-clojure--extra-definers buffer)
        (with-current-buffer buffer
          (setq replique-clojure--extra-definers new)
          (font-lock-flush))))))

(defcustom replique-clojure-extra-def-forms nil
  "List of macro names painted the same way as `defn'.
Each listed symbol, when it heads a list, colors its head as a builtin, the
following symbol as a definition name, and a trailing string as a docstring."
  :safe #'listp
  :type '(repeat string)
  :set #'replique-clojure--set-extra-def-forms)


;;;; Indentation — the rules
;;
;; The rules are cljfmt's and so is the reading of them.  Which rules there
;; are is what the project configures cljfmt with - `.cljfmt.edn', the
;; `:cljfmt' of `.lsp/config.edn' - read by `replique-cljfmt' the way
;; `clojure-lsp format' reads it, so that a file indented here is a file
;; `clojure-lsp format' leaves alone.  What a buffer adds is below.

(defcustom replique-clojure-semantic-indent-rules nil
  "Indentation rules of this buffer's own, over the cljfmt configuration.
Each entry is (\"symbol-name\" . (SPEC ...)) where SPEC is one of
\(:block N), (:inner D) or (:inner D I), matching cljfmt semantics.  A symbol
listed here replaces whatever rule the configuration has for it.

The project's cljfmt configuration is read as well, and is the better
place for a rule, because `clojure-lsp format' reads it too: see
`replique-cljfmt-config'."
  :safe #'listp
  :type '(alist :key-type string
                :value-type (repeat (choice (list (choice (const :tag "Block indentation rule" :block)
                                                          (const :tag "Inner indentation rule" :inner))
                                                  integer)
                                            (list (const :tag "Inner indentation rule" :inner)
                                                  integer
                                                  integer)))))

(defvar replique-clojure--cljfmt nil
  "The cljfmt configuration followed while a region is being indented.
Nil outside of one, where it is asked for each time it is wanted - which
is a lookup, `replique-cljfmt-config' keeping what it has read.")

(defun replique-clojure-cljfmt-config ()
  "The cljfmt configuration the current buffer is indented by."
  (or replique-clojure--cljfmt
      (replique-cljfmt-config default-directory
                              replique-clojure-semantic-indent-rules)))

(defvar replique-clojure--ns-context 'unread
  "What the symbols of the buffer resolve to, while a region is indented.
`unread' outside of one, where it is read when a rule needs it.")

(defun replique-clojure--read-libspec (node prefix found)
  "Note what the libspec NODE aliases and refers, into FOUND.
PREFIX is the namespace a prefix list puts in front of it.  FOUND is a
cons of the aliases and the refers, each an alist of strings."
  (when (memq (replique-parse-type node) '(vector list))
    (let* ((forms (mapcar #'replique-parse-unwrap-meta (replique-parse-forms node)))
           (lib (and (eq 'symbol (replique-parse-type (car forms)))
                     (replique-parse-text (car forms))))
           (full (and lib (if prefix (concat prefix "." lib) lib)))
           (rest (cdr forms)))
      (when full
        (while rest
          (let ((form (car rest))
                (next (cadr rest)))
            (cond
             ((and next (eq 'keyword (replique-parse-type form))
                   (equal ":as" (replique-parse-text form))
                   (eq 'symbol (replique-parse-type next)))
              (push (cons (replique-parse-text next) full) (car found))
              (setq rest (cdr rest)))
             ((and next (eq 'keyword (replique-parse-type form))
                   (equal ":refer" (replique-parse-text form)))
              (when (memq (replique-parse-type next) '(vector list))
                (dolist (name (replique-parse-forms next))
                  (when (eq 'symbol (replique-parse-type name))
                    (push (cons (replique-parse-text name) full) (cdr found)))))
              (setq rest (cdr rest)))
             (t (replique-clojure--read-libspec form full found))))
          (setq rest (cdr rest)))))))

(defun replique-clojure--read-ns-context ()
  "The namespace the buffer is in, and what its ns form aliases and refers.
A plist of `:ns', `:aliases' and `:refers', the last two alists of strings
with the cljfmt configuration's `:alias-map' and `:refer-map' over them."
  (let ((config (replique-clojure-cljfmt-config))
        (found (cons nil nil))
        (ns nil))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when-let* ((_ (re-search-forward "^(ns\\_>" nil t))
                    (form (replique-parse-form-at (match-beginning 0))))
          (let ((forms (replique-parse-forms form)))
            (when-let* ((name (replique-parse-unwrap-meta (nth 1 forms))))
              (when (eq 'symbol (replique-parse-type name))
                (setq ns (replique-parse-text name))))
            (dolist (clause (nthcdr 2 forms))
              (when (and (eq 'list (replique-parse-type clause))
                         (equal ":require"
                                (replique-parse-text
                                 (car (replique-parse-forms clause)))))
                (dolist (spec (cdr (replique-parse-forms clause)))
                  (replique-clojure--read-libspec spec nil found))))))))
    (list :ns ns
          :aliases (append (replique-cljfmt-string-map config :alias-map)
                           (car found))
          :refers (append (replique-cljfmt-string-map config :refer-map)
                          (cdr found)))))

(defun replique-clojure--ns-context ()
  "What the symbols of the buffer resolve to."
  (if (eq replique-clojure--ns-context 'unread)
      (replique-clojure--read-ns-context)
    replique-clojure--ns-context))

(defun replique-clojure--resolve (text)
  "The namespace the symbol written TEXT resolves to, or nil.

Through the aliases of the ns form where it is written under one, through
what the ns form refers where it is not, and otherwise to the namespace
the buffer is in - which is what cljfmt guesses, having no more to go on
than the text either."
  (let* ((parts (replique-parse-name-parts text))
         (context (replique-clojure--ns-context)))
    (if (car parts)
        (or (cdr (assoc (car parts) (plist-get context :aliases))) (car parts))
      (or (cdr (assoc (cdr parts) (plist-get context :refers)))
          (plist-get context :ns)))))

(defun replique-clojure--part-matches-p (part string)
  "Return non-nil when the key PART, a string or a regex, matches STRING."
  (if (stringp part)
      (equal part string)
    (replique-cljfmt-regex-match part string)))

(defun replique-clojure--key-matches-p (key head)
  "Return non-nil when the rule KEY names HEAD.
HEAD is a cons of the text a symbol is written as and its name, or nil."
  (when head
    (let ((name (cdr head)))
      (pcase key
        (`(name ,n) (equal n name))
        (`(regex ,regex) (replique-cljfmt-regex-match regex name))
        (`(qualified ,ns ,n)
         (and (equal n name) (equal ns (replique-clojure--resolve (car head)))))
        (`(parts ,ns-part ,name-part)
         (let ((ns (or (replique-clojure--resolve (car head))
                       (car (replique-parse-name-parts (car head))))))
           (and ns
                (replique-clojure--part-matches-p ns-part ns)
                (replique-clojure--part-matches-p name-part name))))))))


;;;; Indentation — where a line goes
;;
;; Indenting a line is one question: what form is still open at the start of
;; it, and how far in does that form put what it holds.  The path from the
;; top level form down to the start of the line is walked from the inside
;; out, and the first node on it that begins before the line does is the one
;; that governs: a node that begins at the start of the line is the thing
;; being indented and has no say in where it goes.  A blank line and a
;; written line are then the same case, the one with nothing written on it
;; yet.
;;
;; What the governing node does with it is what cljfmt does, case for case,
;; because the point is to agree with it:
;;
;;   in a list   the first rule that says anything says where it goes, and
;;               without one it lines up under the first argument - or goes
;;               one in, where it is the first argument or there is none
;;   in metadata it goes where the whole of the metadata would
;;   in what a reader conditional holds, or in any other collection, one
;;               in from where the collection opens, past its bracket
;;
;; A map entry is stepped over: it is a grouping the reader makes and not
;; something anybody wrote, so a value on a line of its own belongs to the
;; map.

(defconst replique-clojure--indent-widths
  '((list . 1) (vector . 1) (map . 1) (set . 2) (fn . 2)
    (tagged . 1) (namespaced-map . 1)
    (reader-conditional . 1) (reader-conditional-splicing . 1)
    (quote . 1) (syntax-quote . 1) (unquote . 1) (deref . 1)
    (unquote-splicing . 2) (var-quote . 2) (discard . 2) (eval . 2))
  "How far in from a form's own column what it holds is written.

Which is how wide the text before the first thing in it is: the bracket
of a collection, the `#' of a tagged literal or of a reader conditional -
whose `?' is the first thing in it, to cljfmt.")

(defvar replique-clojure--indent-shifts nil
  "How far the region being indented has moved so far, or nil for none.

A list of conses of a position and how far everything from there on has
moved since, newest first - so descending, because a region is indented
downwards and each line is moved after the ones above it.

Indenting a line moves it sideways.  It does not add a line, take one
away, or change what the form is made of, so a form read before any of it
was indented stays true throughout: a node is where it was, plus how far
the lines above it have moved.  That is what lets a whole form be read
once rather than once a line.")

(defun replique-clojure--translate (position)
  "Where POSITION, read before the region was indented, is written now."
  (let ((shifts replique-clojure--indent-shifts))
    (while (and shifts (< position (caar shifts)))
      (setq shifts (cdr shifts)))
    (if shifts (+ position (cdar shifts)) position)))

(defsubst replique-clojure--column (position)
  "The column POSITION is written at."
  (save-excursion (goto-char (replique-clojure--translate position))
                  (current-column)))

(defun replique-clojure--node-text (node)
  "The text NODE is written as now.

Read from where NODE is written now rather than from where it was read:
a form that is being indented has had the lines above this one moved
under it, and the text at the positions it was read at is not its own any
more."
  (buffer-substring-no-properties
   (replique-clojure--translate (replique-parse-start node))
   (replique-clojure--translate (replique-parse-end node))))

(defun replique-clojure--elements (node)
  "What NODE holds, comments left out and the entries of a map opened up.

A discarded form is kept, because cljfmt keeps it where it moves from one
thing in a form to the next - and leaves it out where it counts them,
which is `replique-clojure--forms-before'."
  (let ((elements nil))
    (dolist (child (replique-parse-children node))
      (pcase (replique-parse-type child)
        ('comment nil)
        ('pair (dolist (part (replique-parse-children child))
                 (unless (eq 'comment (replique-parse-type part))
                   (push part elements))))
        (_ (push child elements))))
    (nreverse elements)))

(defun replique-clojure--forms-before (node position)
  "How many of the forms NODE holds are written before POSITION.

Which is the place a form beginning at POSITION has among them, and the
place the one somebody is about to write there would have.  The key and
the value of a map entry are two of them."
  (let ((count 0))
    (dolist (child (replique-parse-children node) count)
      (if (eq 'pair (replique-parse-type child))
          (dolist (part (replique-parse-children child))
            (when (and (< (replique-parse-start part) position)
                       (not (replique-parse-gap-p part)))
              (setq count (1+ count))))
        (when (and (< (replique-parse-start child) position)
                   (not (replique-parse-gap-p child)))
          (setq count (1+ count)))))))

(defun replique-clojure--element-before-p (node position)
  "Return non-nil when anything but a comment is written in NODE before POSITION."
  (seq-some (lambda (element) (< (replique-parse-start element) position))
            (replique-clojure--elements node)))

(defun replique-clojure--first-in-line-p (node)
  "Return non-nil when nothing but spaces is written before NODE on its line."
  (save-excursion
    (goto-char (replique-clojure--translate (replique-parse-start node)))
    (skip-chars-backward " \t")
    (bolp)))

(defun replique-clojure--symbol-head (node)
  "The symbol NODE is written as, as a cons of its text and its name, or nil."
  (let ((node (replique-parse-unwrap-meta node)))
    (when (eq 'symbol (replique-parse-type node))
      (let ((text (replique-clojure--node-text node)))
        (cons text (cdr (replique-parse-name-parts text)))))))

(defun replique-clojure--rule-head (node)
  "The symbol NODE is headed by, as a cons of its text and its name, or nil.

The first thing in it with its metadata taken off.  Or, where that is a
reader conditional, the symbol the first platform in it stands for: a
macro that is spelled one way on one platform and another way on the
other is still the one macro."
  (let ((first (replique-parse-unwrap-meta (car (replique-clojure--elements node)))))
    (if (memq (replique-parse-type first)
              '(reader-conditional reader-conditional-splicing))
        (let ((elements (and (replique-parse-target first)
                             (replique-clojure--elements
                              (replique-parse-target first)))))
          (while (and elements (not (eq 'keyword (replique-parse-type (car elements)))))
            (setq elements (cdr elements)))
          (and (cadr elements) (replique-clojure--symbol-head (cadr elements))))
      (replique-clojure--symbol-head first))))

(defun replique-clojure-indent-column (position)
  "The column a line beginning at POSITION is to be indented to.

Nil where it is not to be indented at all, which is a line written inside
a string: what is between the quotes is the value, and moving it would be
changing what the program says."
  (let ((root (replique-parse-form-at position)))
    (if root
        (replique-clojure--indent-column root position)
      ;; Written between forms rather than in one
      0)))

(defun replique-clojure--indent-column (root position)
  "The column a line beginning at POSITION goes to, ROOT being the form it is in."
  (let ((path (nreverse (replique-parse-path root position))))
    (while (and path
                (or (>= (replique-parse-start (car path)) position)
                    (eq 'pair (replique-parse-type (car path)))))
      (setq path (cdr path)))
    (if (null path)
        ;; Written inside nothing, which is where a top level form goes
        0
      (replique-clojure--indent-amount
       (car path)
       (seq-remove (lambda (node) (eq 'pair (replique-parse-type node)))
                   (cdr path))
       position))))

(defun replique-clojure--coll-indent (node)
  "One in from where NODE opens, past the text it opens with."
  (+ (replique-clojure--column (replique-parse-start node))
     (or (alist-get (replique-parse-type node) replique-clojure--indent-widths)
         0)))

(defun replique-clojure--indent-amount (node around position)
  "The column a line beginning at POSITION goes to, inside NODE.
AROUND is what NODE is written inside, innermost first."
  (let ((type (replique-parse-type node)))
    (cond
     ((memq type '(string regex)) nil)
     ((memq (replique-parse-type (car around))
            '(reader-conditional reader-conditional-splicing))
      (replique-clojure--coll-indent node))
     ((memq type '(list fn))
      (replique-clojure--custom-indent node around position))
     ;; `^:private x' written over two lines is still one form, the second
     ;; thing in its `def', and goes where that would
     ((eq type 'meta)
      (if around
          (replique-clojure--indent-amount (car around) (cdr around)
                                           (replique-parse-start node))
        0))
     (t (replique-clojure--coll-indent node)))))


;;;; Indentation — what the form says

(defun replique-clojure--custom-indent (node around position)
  "The column a line beginning at POSITION goes to, inside the list NODE.

AROUND is what NODE is written inside, innermost first, which is wanted
because an :inner rule is written on a form some way out from this one.
The rules are tried in the order cljfmt tries them, and the first that
has anything to say about the line says where it goes."
  (let* ((config (replique-clojure-cljfmt-config))
         (place (replique-clojure--forms-before node position))
         (left (replique-clojure--element-before-p node position))
         (heads nil)
         (head (lambda (depth)
                 (let ((known (assq depth heads)))
                   (if known
                       (cdr known)
                     (let* ((level (if (= depth 0) node (nth (1- depth) around)))
                            (value (and level (replique-clojure--rule-head level))))
                       (push (cons depth value) heads)
                       value))))))
    (or (catch 'found
          (dolist (rule (plist-get config :rules))
            (let ((key (car rule)))
              (dolist (spec (nth 1 rule))
                (let ((column (replique-clojure--spec-column
                               key spec node around place left head config)))
                  (when column (throw 'found column)))))))
        (replique-clojure--list-indent node place config))))

(defun replique-clojure--indent-width (node)
  "How far in from NODE its body is written: two for a list, three for a `#('."
  (if (eq 'fn (replique-parse-type node)) 3 2))

(defun replique-clojure--index-at-depth (node around depth)
  "Where the form DEPTH out from NODE is, among the forms of the one around it.
AROUND is what NODE is written inside, innermost first."
  (let ((inner (if (= depth 0) node (nth (1- depth) around)))
        (outer (nth depth around)))
    (and inner outer
         (replique-clojure--forms-before outer (replique-parse-start inner)))))

(defun replique-clojure--spec-column (key spec node around place left head config)
  "The column the rule SPEC of KEY puts a line in NODE at, or nil.

PLACE is how many forms of NODE are written before the line and LEFT is
whether anything is.  HEAD answers what the form some depth out from NODE
is headed by, AROUND being the forms around it."
  (pcase spec
    ;; The first N arguments are arguments and what follows them is body -
    ;; provided the body starts a line of its own.  `(let [x 1] (foo)' has
    ;; said where the rest of it goes by putting something there
    (`(:block ,n)
     (when (replique-clojure--key-matches-p key (funcall head 0))
       (let ((after (nth (1+ n) (replique-clojure--elements node))))
         (if (and (or (null after) (replique-clojure--first-in-line-p after))
                  (> place n))
             (+ (replique-clojure--column (replique-parse-start node))
                (replique-clojure--indent-width node))
           (replique-clojure--list-indent node place config)))))
    (`(:inner ,depth . ,index)
     (when (and left
                (replique-clojure--key-matches-p key (funcall head depth))
                (or (null index)
                    (and (> depth 0)
                         (eql (1+ (car index))
                              (replique-clojure--index-at-depth
                               node around (1- depth))))))
       (+ (replique-clojure--column (replique-parse-start node))
          (replique-clojure--indent-width node))))
    (_ (when (replique-clojure--key-matches-p key (funcall head 0))
         (replique-clojure--list-indent node place config)))))

(defun replique-clojure--list-indent (node place config)
  "Where a line goes in the list NODE when no rule says, PLACE forms in.

Under the first argument where it comes after it, which is what makes the
arguments of a call read as a column - whatever the head is.  Otherwise
one in, or two where CONFIG's `:function-arguments-indentation' says so."
  (if (> place 1)
      (replique-clojure--column
       (replique-parse-start (nth 1 (replique-clojure--elements node))))
    (+ (replique-clojure--coll-indent node)
       (if (replique-clojure--two-space-p node config) 1 0))))

(defun replique-clojure--two-space-p (node config)
  "Return non-nil when what NODE holds is indented two rather than one.

Which CONFIG's `:function-arguments-indentation' says from the first thing
written in NODE - whitespace or a comment included, which is what cljfmt
looks at."
  (let* ((first (car (replique-parse-children node)))
         (first (if (eq 'pair (replique-parse-type first))
                    (car (replique-parse-children first))
                  first))
         (type (if (and first
                        (= (replique-clojure--translate (replique-parse-start first))
                           (+ (replique-clojure--translate (replique-parse-start node))
                              (alist-get (replique-parse-type node)
                                         replique-clojure--indent-widths))))
                   (replique-parse-type first)
                 'whitespace)))
    (pcase (replique-cljfmt-option config :function-arguments-indentation)
      (:cursive (not (memq (if (eq type 'meta)
                               (replique-parse-type (replique-parse-unwrap-meta first))
                             type)
                           '(vector map list set))))
      (:zprint (memq type '(symbol keyword number string boolean null character
                                   symbolic list)))
      (_ nil))))


;;;; Indentation — the line and the region

(defun replique-clojure-indent-line ()
  "Indent the current line."
  (let ((column (replique-clojure-indent-column (line-beginning-position))))
    (when column
      (indent-line-to column))))

(defun replique-clojure--left-alone-p (comments)
  "Return non-nil when the line point is at the start of is not indented.

Which is a blank line, and a line that holds nothing but a comment - unless
COMMENTS, cljfmt's `:indent-line-comments?', says to indent those, and then
still a comment written with one semicolon or with three, which cljfmt
reads as placed on purpose."
  (or (looking-at-p "[ \t]*$")
      ;; Closing brackets before the comment are stepped over, since cljfmt
      ;; steps out of a form looking for one
      (and (looking-at "[ \t]*\\(?:[])}][ \t]*\\)*\\(;+\\)")
           (or (not comments)
               (not (equal ";;" (match-string 1)))))))

(defun replique-clojure--top-level-spans (from to)
  "Each top level form between FROM and TO, as read before anything moved.

A list of [BEGINNING END TREE WHERE], in the order they are written, where
BEGINNING and END are markers and WHERE is the position TREE was read at.
Markers, because indenting one form moves every form after it, and asking
the buffer where the next one is after each line is what makes indenting a
file cost the length of the file once per line of it."
  (let ((start (or (car (replique-parse-top-level-bounds from)) from)))
    (mapcar (lambda (form)
              (vector (copy-marker (replique-parse-start form))
                      (copy-marker (replique-parse-end form))
                      form
                      (replique-parse-start form)))
            (replique-parse-forms-in start to))))

(defun replique-clojure--indent-span (span limit comments)
  "Indent the lines of the top level form SPAN, as far as LIMIT.

Point is left where the form ends, or at LIMIT.  The form was read before
any of the region was indented and is followed rather than read again,
which is what `replique-clojure--indent-shifts' is for - including at the
start of it, where the forms before this one have already moved it.
COMMENTS is whether a line holding only a comment is indented."
  (let* ((tree (aref span 2))
         (moved (- (marker-position (aref span 0)) (aref span 3)))
         (replique-clojure--indent-shifts
          (unless (zerop moved) (list (cons (aref span 3) moved)))))
    (while (and (< (point) (aref span 1)) (< (point) limit))
      (unless (replique-clojure--left-alone-p comments)
        (let* ((was (point))
               (column (replique-clojure--indent-column tree (- was moved))))
          (when column
            (let ((delta (- column (current-indentation))))
              (unless (zerop delta)
                (indent-line-to column)
                (setq moved (+ moved delta))
                (push (cons (- was (- moved delta)) moved)
                      replique-clojure--indent-shifts))))))
      (forward-line 1))))

(defun replique-clojure-indent-region (from to)
  "Indent every line between FROM and TO, the way cljfmt would.

Line by line and downwards, because a line is placed by where the lines
above it ended up: the arguments of a call line up under the first of
them, and where that one is is not known until it has been put there.

One top level form at a time, each read once.  Where they are is taken
before anything has moved and held as markers, so that indenting one does
not lose the next."
  (save-excursion
    (let* ((replique-clojure--cljfmt (replique-clojure-cljfmt-config))
           (replique-clojure--ns-context
            (if (plist-get replique-clojure--cljfmt :qualified)
                (replique-clojure--read-ns-context)
              'unread))
           (comments (replique-cljfmt-option replique-clojure--cljfmt
                                             :indent-line-comments?))
           (limit (copy-marker to))
           (spans (replique-clojure--top-level-spans from to)))
      (goto-char from)
      (beginning-of-line)
      (dolist (span spans)
        ;; What is written between two forms is written in nothing
        (while (and (< (point) limit) (< (point) (aref span 0)))
          (unless (replique-clojure--left-alone-p comments) (indent-line-to 0))
          (forward-line 1))
        (when (< (point) limit)
          (replique-clojure--indent-span span limit comments)))
      (while (< (point) limit)
        (unless (replique-clojure--left-alone-p comments) (indent-line-to 0))
        (forward-line 1))
      (set-marker limit nil)
      (dolist (span spans)
        (set-marker (aref span 0) nil)
        (set-marker (aref span 1) nil)))))


;;;; Navigation
;;
;; Moving over forms, which is the last thing here that asked for a grammar
;; and the one it was doing worst.
;;
;; A defun is a top level form.  Any of them - not only the ones that begin
;; with `def', which is what `treesit-thing-settings' was saying and what
;; made `C-M-a' inside a `(comment ...)' walk past it into the definition
;; before it, and `C-M-a' inside a `(println ...)' walk back into a `#_'
;; that the reader had thrown away.  What somebody means by the top of the
;; form they are in is the top of the form they are in.
;;
;; Moving over an expression is left to the syntax table, which already
;; knows the reader macros: `#' and `?' and `\'' carry the prefix flag and
;; the rest are of the prefix class, so `#{1 2}' and `#?(:clj 1)' and
;; `~@foo' each move as one.  Walking twenty four shapes with `forward-sexp'
;; said the table and the grammar answer the same thing in twenty three of
;; them, so what a grammar was buying here was `#:ns{...}', and both of them
;; read `#_ x' and `^:private x' as two expressions where the reader reads
;; one.  That is worth fixing on its own account and is not worth a grammar.
;;
;; `show-paren-mode' and `transpose-sexps' are left alone for the same
;; reason: they are built on the syntax table and it answers them.

(defun replique-clojure--defun-before (position)
  "Where the top level form to move back to from POSITION is, or nil.

The form POSITION is written in, unless POSITION is where it begins - in
which case there is nowhere to move back to inside it, and what is wanted
is the form before it."
  (let ((here (replique-parse-top-level-bounds position)))
    (if (and here (> position (car here)))
        here
      (replique-parse-bounds-before position))))

(defun replique-clojure-beginning-of-defun (&optional arg)
  "Move to the beginning of a top level form, ARG of them.

Backwards for a positive ARG and forwards for a negative one, which is
what `beginning-of-defun-function' is asked for.  Answers nil where there
were fewer than ARG of them to move over, having moved as far as it could."
  (setq arg (or arg 1))
  (let ((moved t))
    (while (and moved (> arg 0))
      (let ((bounds (replique-clojure--defun-before (point))))
        (if bounds
            (goto-char (car bounds))
          (goto-char (point-min))
          (setq moved nil)))
      (setq arg (1- arg)))
    (while (and moved (< arg 0))
      (let ((bounds (replique-parse-bounds-after (1+ (point)))))
        (if bounds
            (goto-char (car bounds))
          (goto-char (point-max))
          (setq moved nil)))
      (setq arg (1+ arg)))
    moved))

(defun replique-clojure-end-of-defun (&optional _arg)
  "Move to the end of the top level form point is written in or before.

`end-of-defun' puts point at the beginning of one before asking, so what
is wanted is nearly always the form point is at the beginning of; the one
after it is for a point written between forms, where there is no form to
be at the end of but there is one to move to."
  (let ((bounds (or (replique-parse-top-level-bounds (point))
                    (replique-parse-bounds-after (point)))))
    (if bounds
        (progn (goto-char (cdr bounds)) t)
      (goto-char (point-max))
      nil)))

(defun replique-clojure-current-defun ()
  "The name of the top level form point is written in, or nil for none.

What `add-log-current-defun' and `which-function-mode' show.  Read from
the same table the head of a form is painted from, so that what the
screen calls a definition and what a commit message calls one are the
same thing - and so that a form the reader throws away, which is a `#_'
or a `(comment ...)', is called nothing at all."
  (when-let* ((form (replique-parse-unwrap-meta
                     (replique-parse-form-at (point)))))
    (let* ((forms (replique-parse-forms form))
           (name (replique-clojure--head-name (replique-parse-unwrap-meta (car forms))))
           (definer (and name (replique-clojure--definer name)))
           (named (replique-parse-unwrap-meta (nth 1 forms))))
      (when (and definer
                 (plist-get definer :name)
                 (eq 'symbol (replique-parse-type named)))
        (replique-parse-text named)))))


;;;; Docstring filling

(defun replique-clojure--docstring-fill-prefix ()
  "Return the docstring fill prefix (a run of spaces)."
  (make-string replique-clojure-docstring-fill-prefix-width ?\s))

(defun replique-clojure--fill-paragraph (&optional justify)
  "Like `fill-paragraph', but aware of Clojure docstrings.
If JUSTIFY is non-nil, justify as well as fill."
  (let ((bounds (replique-clojure-docstring-bounds (point))))
    (if bounds
        (let ((fill-column (or replique-clojure-docstring-fill-column fill-column))
              (fill-prefix (replique-clojure--docstring-fill-prefix)))
          (save-restriction
            (narrow-to-region (car bounds) (cdr bounds))
            (fill-paragraph justify)))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify)))
    t))


;;;; Syntax table

(defvar replique-clojure-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ASCII as symbol constituents by default.
    (modify-syntax-entry '(0 . 127) "_" table)
    ;; Word syntax.
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)
    ;; Whitespace.
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table)   ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)
    ;; Comma is punctuation, not whitespace.
    (modify-syntax-entry ?, "." table)
    ;; Delimiters.
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Reader prefix chars.
    (modify-syntax-entry ?` "'" table)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    (modify-syntax-entry ?? "_ p" table)    ; ? is a prefix outside symbols
    (modify-syntax-entry ?# "_ p" table)    ; # is allowed inside keywords
    (modify-syntax-entry ?' "_ p" table)    ; ' allowed anywhere but symbol start
    ;; Comments, strings, escape.
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    table)
  "Syntax table for `replique-clojure-mode'.
Drives electric pairs, and what the rest of Emacs takes for a string or a
comment.  Neither painting nor indentation reads it: both read the buffer
with `replique-parse', which has a table of its own because what splits
one Clojure token from the next is not what a syntax table is for.")


;;;; Mode setup

(defvar-local replique-clojure-read-p nil
  "Whether replique reads this buffer as Clojure.

Set by `replique-clojure-setup\\=', which is what makes a buffer one -
the mode calls it, and so does the repl, whose buffer is not in that mode
and holds Clojure all the same.  So this is true of exactly the buffers
where the reading was set up, which `derived-mode-p\\=' is not: it is
false in a repl, and a repl is the buffer somebody is likeliest to be
writing Clojure in.

Not a permanent local.  Turning another mode on takes the reading away,
and this goes with it.")

(defun replique-clojure-setup ()
  "Read the current buffer as Clojure.

Painting, indentation, filling and moving over forms - all of them out of
`replique-parse' or out of the syntax table, and none of them out of a
grammar.

Called by `replique-clojure-mode', and by the repl, whose buffer is not in
that mode and holds Clojure all the same."
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+ *")
  (setq-local font-lock-defaults
              '(nil nil nil nil
                    (font-lock-fontify-region-function
                     . replique-clojure-font-lock-region)))
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'replique-clojure-indent-line)
  (setq-local indent-region-function #'replique-clojure-indent-region)
  (setq-local fill-paragraph-function #'replique-clojure--fill-paragraph)
  (setq-local replique-clojure--extra-definers
              (replique-clojure--compute-extra-definers
               replique-clojure-extra-def-forms))
  (setq-local beginning-of-defun-function #'replique-clojure-beginning-of-defun)
  (setq-local end-of-defun-function #'replique-clojure-end-of-defun)
  (setq-local add-log-current-defun-function #'replique-clojure-current-defun)
  ;; Last, so that it is true of a buffer where all of the above is done
  (setq-local replique-clojure-read-p t)
  (add-hook 'change-major-mode-hook #'replique-parse-forget nil t))

(defun replique-clojure--hack-local-variables ()
  "Recompute buffer-local caches after `.dir-locals.el' has been applied."
  (setq-local replique-clojure--extra-definers
              (replique-clojure--compute-extra-definers
               replique-clojure-extra-def-forms))
  (font-lock-flush))

;;;###autoload
(define-derived-mode replique-clojure-mode prog-mode "Replique[clj]"
  "Major mode for editing Clojure code.

Highlighting, indentation and moving over forms are read out of
`replique-parse', which reads Clojure the way the Clojure reader does.
Semantic faces and diagnostics come from a C module that is not part of
replique."
  :syntax-table replique-clojure-mode-syntax-table
  (replique-clojure-setup)
  (add-hook 'hack-local-variables-hook
            #'replique-clojure--hack-local-variables 0 t))

;;;###autoload
(define-derived-mode replique-clojure-clojurescript-mode replique-clojure-mode
  "Replique[cljs]"
  "Major mode for editing ClojureScript code.

\\{replique-clojure-clojurescript-mode-map}")

;;;###autoload
(define-derived-mode replique-clojure-clojurec-mode replique-clojure-mode
  "Replique[cljc]"
  "Major mode for editing ClojureC code.

\\{replique-clojure-clojurec-mode-map}")

;;;###autoload
(progn
  ;; Clojure + EDN
  (add-to-list 'auto-mode-alist
               '("\\.\\(clj\\|edn\\)\\'" . replique-clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . replique-clojure-clojurescript-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . replique-clojure-clojurec-mode))
  ;; babashka scripts are Clojure source files.
  (add-to-list 'interpreter-mode-alist '("bb" . replique-clojure-mode)))

(provide 'replique-clojure-mode)

;;; replique-clojure-mode.el ends here
