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
;; It owns font-lock and indentation, and it owns them out of two different
;; readings of the buffer:
;;
;;   * font-lock, out of `replique-parse' - what each token is, and then what
;;     the form it is written in says it is.  One walk, no grammar, and no
;;     precedence between rules; the section below says why that is the whole
;;     of the design.
;;   * cljfmt indentation (`treesit-simple-indent') over the `treejure'
;;     grammar - the `:block'/`:inner' rule model, with a `no-indent' seam so
;;     multi-line string interiors are left untouched.  This is the last
;;     thing here that asks for a grammar.
;;
;; Semantic faces (`:local', `:macro-invocation', `:special-form',
;; `:unresolved', unused greyout), diagnostics and navigation are not here and
;; are not part of replique: they are computed by a C module that is layered
;; over these faces as an overlay, and this file is pure, in-core treesit.
;;
;; What replique needs of it is neither of those.  A form is sent to a repl
;; with the line it was written on, so replique has to agree with the reader
;; about where a form begins - a question sexp motion answers wrongly for #_
;; and for metadata - and this is the parse it asks.  See `replique-eval'.
;;
;; Customization (M-x customize-group RET replique-clojure RET):
;;   `replique-clojure-font-lock-level'           how much is painted, 1 to 4
;;   `replique-clojure-ensure-grammars'           install/update the grammar
;;   `replique-clojure-extra-def-forms'           macros highlighted like defn
;;   `replique-clojure-semantic-indent-rules'     per-symbol indent overrides
;;   `replique-clojure-docstring-fill-column'     fill-column for docstrings
;;   `replique-clojure-docstring-fill-prefix-width'  docstring fill prefix
;; The faces are themeable via the deffaces below, and the indent rules /
;; extra def-forms are `.dir-locals.el'-friendly.

;;; Code:

(require 'treesit)
(require 'seq)
(require 'replique-parse)
(eval-when-compile (require 'subr-x))   ; thread-first / thread-last / when-let*

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-eq "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")


;;;; Customization

(defgroup replique-clojure nil
  "Tree-sitter syntax layer for Clojure (treejure grammar)."
  :prefix "replique-clojure-"
  :group 'languages)

(defcustom replique-clojure-ensure-grammars t
  "When non-nil, ensure the required Tree-sitter grammars are installed."
  :safe #'booleanp
  :type 'boolean)

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

(defconst replique-clojure-grammar-recipes
  '((treejure "https://github.com/EwenG/tree-sitter-treejure.git" "main"))
  "Tree-sitter grammar recipes used by `treesit-install-language-grammar'.")


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


;;;; Indentation — cljfmt rule data

(defvar replique-clojure--semantic-indent-rules-defaults
  '(("alt!"            . ((:block 0)))
    ("alt!!"           . ((:block 0)))
    ("comment"         . ((:block 0)))
    ("cond"            . ((:block 0)))
    ("delay"           . ((:block 0)))
    ("do"              . ((:block 0)))
    ("finally"         . ((:block 0)))
    ("future"          . ((:block 0)))
    ("go"              . ((:block 0)))
    ("thread"          . ((:block 0)))
    ("try"             . ((:block 0)))
    ("with-out-str"    . ((:block 0)))
    ("defprotocol"     . ((:block 1) (:inner 1)))
    ("definterface"    . ((:block 1) (:inner 1)))
    ("binding"         . ((:block 1)))
    ("case"            . ((:block 1)))
    ("cond->"          . ((:block 1)))
    ("cond->>"         . ((:block 1)))
    ("doseq"           . ((:block 1)))
    ("dotimes"         . ((:block 1)))
    ("doto"            . ((:block 1)))
    ("extend"          . ((:block 1)))
    ("extend-protocol" . ((:block 1) (:inner 1)))
    ("extend-type"     . ((:block 1) (:inner 1)))
    ("for"             . ((:block 1)))
    ("go-loop"         . ((:block 1)))
    ("if"              . ((:block 1)))
    ("if-let"          . ((:block 1)))
    ("if-not"          . ((:block 1)))
    ("if-some"         . ((:block 1)))
    ("let"             . ((:block 1)))
    ("letfn"           . ((:block 1) (:inner 2 0)))
    ("locking"         . ((:block 1)))
    ("loop"            . ((:block 1)))
    ("match"           . ((:block 1)))
    ("ns"              . ((:block 1)))
    ("struct-map"      . ((:block 1)))
    ("testing"         . ((:block 1)))
    ("when"            . ((:block 1)))
    ("when-first"      . ((:block 1)))
    ("when-let"        . ((:block 1)))
    ("when-not"        . ((:block 1)))
    ("when-some"       . ((:block 1)))
    ("while"           . ((:block 1)))
    ("with-local-vars" . ((:block 1)))
    ("with-open"       . ((:block 1)))
    ("with-precision"  . ((:block 1)))
    ("with-redefs"     . ((:block 1)))
    ("defrecord"       . ((:block 2) (:inner 1)))
    ("deftype"         . ((:block 2) (:inner 1)))
    ("are"             . ((:block 2)))
    ("as->"            . ((:block 2)))
    ("catch"           . ((:block 2)))
    ("condp"           . ((:block 2)))
    ("bound-fn"        . ((:inner 0)))
    ("def"             . ((:inner 0)))
    ("defmacro"        . ((:inner 0)))
    ("defmethod"       . ((:inner 0)))
    ("defmulti"        . ((:inner 0)))
    ("defn"            . ((:inner 0)))
    ("defn-"           . ((:inner 0)))
    ("defonce"         . ((:inner 0)))
    ("deftest"         . ((:inner 0)))
    ("fdef"            . ((:inner 0)))
    ("fn"              . ((:inner 0)))
    ("reify"           . ((:inner 0) (:inner 1)))
    ("proxy"           . ((:block 2) (:inner 1)))
    ("use-fixtures"    . ((:inner 0))))
  "Default cljfmt-style semantic indentation rules.
Aligned with
https://github.com/weavejester/cljfmt/blob/0.13.0/cljfmt/resources/cljfmt/indents/clojure.clj")

(defvar-local replique-clojure--semantic-indent-rules-cache nil
  "Merged user + default indentation rules for the current buffer.")

(defun replique-clojure--compute-semantic-indent-cache (rules)
  "Return RULES unioned over the defaults, user rules taking precedence."
  (seq-union rules
             replique-clojure--semantic-indent-rules-defaults
             (lambda (e1 e2) (equal (car e1) (car e2)))))

(defun replique-clojure--set-semantic-indent-rules (symbol value)
  "Setter for `replique-clojure-semantic-indent-rules'.
Sets SYMBOL to VALUE and refreshes the per-buffer cache everywhere."
  (set-default-toplevel-value symbol value)
  (let ((new (replique-clojure--compute-semantic-indent-cache value)))
    (dolist (buf (buffer-list))
      (when (buffer-local-boundp 'replique-clojure--semantic-indent-rules-cache buf)
        (with-current-buffer buf
          (setq replique-clojure--semantic-indent-rules-cache new))))))

(defcustom replique-clojure-semantic-indent-rules nil
  "Custom rules extending the default cljfmt indentation rules.
Each entry is (\"symbol-name\" . (SPEC ...)) where SPEC is one of
\(:block N), (:inner D) or (:inner D I), matching cljfmt semantics.  A symbol
listed here fully replaces the built-in rules for that symbol.
The defaults live in `replique-clojure--semantic-indent-rules-defaults'."
  :safe #'listp
  :type '(alist :key-type string
                :value-type (repeat (choice (list (choice (const :tag "Block indentation rule" :block)
                                                          (const :tag "Inner indentation rule" :inner))
                                                  integer)
                                            (list (const :tag "Inner indentation rule" :inner)
                                                  integer
                                                  integer))))
  :set #'replique-clojure--set-semantic-indent-rules)


;;;; Indentation — node predicates

(defun replique-clojure--list-node-p (node)
  "Return non-nil if NODE is a Clojure list."
  (string-equal "list_literal" (treesit-node-type node)))

(defun replique-clojure--anon-fn-node-p (node)
  "Return non-nil if NODE is a function literal."
  (string-equal "fn_literal" (treesit-node-type node)))

(defun replique-clojure--opening-paren-node-p (node)
  "Return non-nil if NODE is an opening paren."
  (string-equal "(" (treesit-node-text node)))

(defun replique-clojure--symbol-node-p (node)
  "Return non-nil if NODE is a symbol."
  (string-equal "symbol" (treesit-node-type node)))

(defun replique-clojure--string-node-p (node)
  "Return non-nil if NODE is a string literal."
  (string-equal "string" (treesit-node-type node)))

(defun replique-clojure--keyword-node-p (node)
  "Return non-nil if NODE is a keyword."
  (string-equal "keyword" (treesit-node-type node)))

(defun replique-clojure--var-node-p (node)
  "Return non-nil if NODE is a var quote (e.g. #\\'foo)."
  (string-equal "var_quote" (treesit-node-type node)))

(defun replique-clojure--unwrap-meta (node)
  "Recursively unwrap NODE from its `with_metadata' wrappers."
  (if (string-equal "with_metadata" (treesit-node-type node))
      (replique-clojure--unwrap-meta
       (treesit-node-child-by-field-name node "target"))
    node))

(defun replique-clojure--first-value-child (node)
  "Return NODE's first named, metadata-unwrapped child."
  (replique-clojure--unwrap-meta (car (treesit-node-children node t))))

(defun replique-clojure--named-node-text (node)
  "Return the name of symbol/keyword NODE (without its namespace)."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun replique-clojure--symbol-matches-p (symbol-regexp node)
  "Return non-nil if NODE is a symbol whose name matches SYMBOL-REGEXP."
  (and (replique-clojure--symbol-node-p node)
       (string-match-p symbol-regexp (replique-clojure--named-node-text node))))

(defun replique-clojure--list-node-sym-text (node &optional include-anon-fn-lit)
  "Return the head-symbol name of list NODE, or nil.
With INCLUDE-ANON-FN-LIT, also handle function literals."
  (let ((node (replique-clojure--unwrap-meta node)))
    (when (or (replique-clojure--list-node-p node)
              (and include-anon-fn-lit (replique-clojure--anon-fn-node-p node)))
      (when-let* ((first-child (replique-clojure--first-value-child node))
                  ((replique-clojure--symbol-node-p first-child)))
        (replique-clojure--named-node-text first-child)))))

(defun replique-clojure--list-node-sym-match-p (node regex &optional include-anon-fn-lit)
  "Return non-nil if NODE is a list whose head symbol matches REGEX.
With INCLUDE-ANON-FN-LIT, also handle function literals."
  (when-let* ((sym-text (replique-clojure--list-node-sym-text node include-anon-fn-lit)))
    (string-match-p regex sym-text)))


;;;; Indentation — thing settings, defun support

(defconst replique-clojure--sexp-nodes
  '("with_metadata"
    "nil" "boolean" "symbolic_value" "erroneous_symbolic_value"
    "number" "string" "regex" "character"
    "symbol" "keyword"
    "list_literal" "vector_literal" "map_literal" "set_literal" "namespaced_map_literal"
    "fn_literal" "reader_conditional"
    "var_quote" "eval_literal"
    "tagged_literal"
    "deref" "quote" "syntax_quote"
    "unquote_splicing" "unquote"
    "discard"
    "invalid_character" "invalid_number")
  "Node types treated as s-expressions.")

(defconst replique-clojure--list-nodes
  '("list_literal" "fn_literal" "reader_conditional"
    "map_literal" "namespaced_map_literal" "vector_literal" "set_literal")
  "Node types treated as lists.")

(defconst replique-clojure--defun-symbols-regex
  (rx bol
      (or "def" "defn" "defn-" "definline" "defrecord" "defmacro" "defmulti"
          "defonce" "defprotocol" "deftest" "deftest-" "ns" "definterface"
          "deftype" "defstruct")
      eol)
  "A regexp matching top-level defining forms.")

(defun replique-clojure--defun-node-p (node)
  "Return non-nil if NODE is a function or var definition."
  (replique-clojure--list-node-sym-match-p node replique-clojure--defun-symbols-regex))

(defun replique-clojure--defun-name-function (node)
  "Return the name of the defun NODE."
  (let ((node (replique-clojure--unwrap-meta node)))
    (when (replique-clojure--defun-node-p node)
      (when-let* ((name-node (treesit-node-child node 1 t))
                  (unwrapped (replique-clojure--unwrap-meta name-node)))
        (treesit-node-text unwrapped t)))))

(defconst replique-clojure--thing-settings
  `((treejure
     (sexp ,(regexp-opt replique-clojure--sexp-nodes))
     (list ,(regexp-opt replique-clojure--list-nodes))
     (text ,(regexp-opt '("comment")))
     (defun ,#'replique-clojure--defun-node-p)))
  "Value for `treesit-thing-settings'.")


;;;; Indentation — semantic rule lookup

(defun replique-clojure--find-semantic-rules-for-node (node)
  "Return the list of semantic rules for NODE's head symbol."
  (when-let* ((first-child (treesit-node-child node 0 t))
              (symbol-name (replique-clojure--named-node-text first-child)))
    (alist-get symbol-name
               replique-clojure--semantic-indent-rules-cache
               nil nil #'equal)))

(defun replique-clojure--find-semantic-rule (node parent current-depth)
  "Return a suitable indentation rule for NODE within PARENT at CURRENT-DEPTH."
  (let ((idx (if node (- (treesit-node-index node) 2) 999))) ; 999 ⇒ treat nil as body
    (if-let* ((rule-set (replique-clojure--find-semantic-rules-for-node parent)))
        (if (zerop current-depth)
            (let ((rule (car rule-set)))
              (if (equal (car rule) :block)
                  rule
                (pcase-let ((`(,_ ,rule-depth ,rule-idx) rule))
                  (when (and (equal rule-depth current-depth)
                             (or (null rule-idx) (equal rule-idx idx)))
                    rule))))
          (thread-last rule-set
                       (seq-filter (lambda (rule)
                                     (pcase-let ((`(,rule-type ,rule-depth ,rule-idx) rule))
                                       (and (equal rule-type :inner)
                                            (equal rule-depth current-depth)
                                            (or (null rule-idx) (equal rule-idx idx))))))
                       (seq-first)))
      (when-let* (((< current-depth 3))
                  (new-parent (treesit-node-parent parent)))
        (replique-clojure--find-semantic-rule parent new-parent (1+ current-depth))))))


;;;; Indentation — logical-context resolution

(defconst replique-clojure--collection-node-types
  '("list_literal" "vector_literal" "map_literal" "set_literal"
    "namespaced_map_literal" "fn_literal")
  "Node types representing collection literals.")

(defun replique-clojure--resolve-indentation-context (node parent)
  "Resolve the logical collection and direct child for NODE / PARENT.
Returns (COLLECTION . DIRECT-CHILD) or nil.  When NODE is nil (indenting a
blank line) PARENT is the collection.  Wrappers like quote/pair/with_metadata
are traversed so the collection that actually governs indentation is found."
  (cond
   ;; NODE is itself a collection: anchor to its logical parent collection.
   ((and node (member (treesit-node-type node) replique-clojure--collection-node-types))
    (if (string-equal "pair" (treesit-node-type parent))
        (when-let* ((coll (treesit-parent-until
                           parent
                           (lambda (n) (member (treesit-node-type n)
                                               replique-clojure--collection-node-types)))))
          (cons coll parent))
      (when-let* ((p (treesit-node-parent node)))
        (cons p node))))
   ;; Blank line whose parent is the collection: do NOT escalate.
   ((and (null node) (member (treesit-node-type parent) replique-clojure--collection-node-types))
    (cons parent nil))
   ;; Normal element / whitespace / wrapped node: walk up to the collection.
   (t
    (let ((start-node (or node parent)))
      (when-let* ((coll (treesit-parent-until
                         start-node
                         (lambda (n) (member (treesit-node-type n)
                                             replique-clojure--collection-node-types)))))
        (let ((direct-child start-node))
          (while (and direct-child
                      (not (treesit-node-eq (treesit-node-parent direct-child) coll)))
            (setq direct-child (treesit-node-parent direct-child)))
          (when direct-child
            (cons coll direct-child))))))))

(defun replique-clojure--anchor-parent-opening-paren (_node parent _bol)
  "Return the position of PARENT's first opening paren (skipping metadata)."
  (thread-first parent
                (treesit-search-subtree #'replique-clojure--opening-paren-node-p nil t 1)
                (treesit-node-start)))

(defun replique-clojure--anchor-logical-parent-opening-paren (node parent bol)
  "Anchor to the start of the logical parent collection."
  (if-let* ((res (replique-clojure--resolve-indentation-context node parent)))
      (treesit-node-start (car res))
    (replique-clojure--anchor-parent-opening-paren node parent bol)))

(defun replique-clojure--anchor-logical-prev-sibling (node parent _bol)
  "Anchor to the previous sibling of the direct child in the logical parent."
  (if-let* ((res (replique-clojure--resolve-indentation-context node parent))
            (direct-child (cdr res)))
      (treesit-node-start (treesit-node-prev-sibling direct-child))
    (treesit-node-start (treesit-node-prev-sibling (or node parent)))))

(defun replique-clojure--anchor-logical-nth-sibling (n)
  "Return an anchor function for the Nth child of the logical parent."
  (lambda (node parent &rest _)
    (if-let* ((res (replique-clojure--resolve-indentation-context node parent)))
        (treesit-node-start (treesit-node-child (car res) n t))
      (treesit-node-start (treesit-node-child (or node parent) n t)))))

(defun replique-clojure--match-wrapped-in-non-list-collection (node parent _bol)
  "Match if NODE sits inside a vector/map/set (not a list/fn)."
  (when-let* ((res (replique-clojure--resolve-indentation-context node parent)))
    (member (treesit-node-type (car res))
            '("vector_literal" "map_literal" "set_literal" "namespaced_map_literal"))))

(defun replique-clojure--anchor-wrapped-in-non-list-collection (node parent _bol)
  "Anchor to the start of the non-list collection plus its delimiter width."
  (when-let* ((res (replique-clojure--resolve-indentation-context node parent)))
    (let ((coll (car res)))
      (+ (treesit-node-start coll)
         (if (string-equal "set_literal" (treesit-node-type coll)) 2 1)))))

(defun replique-clojure--match-marker-splicing (_node parent _bol)
  "Match a splicing reader conditional (#?@)."
  (string-equal "marker_splicing"
                (treesit-node-type
                 (treesit-node-child-by-field-name parent "marker"))))

(defun replique-clojure--match-with-metadata (node &optional _parent _bol)
  "Match NODE when it is wrapped in metadata."
  (string-equal "with_metadata" (treesit-node-type (treesit-node-parent node))))


;;;; Indentation — block / threading / call-arg matchers

(defun replique-clojure--match-block-0-body (bol first-child)
  "Match if the body is not on the same line as FIRST-CHILD.
With no body, check that BOL is not on FIRST-CHILD's line."
  (let ((body-pos (if-let* ((body (treesit-node-next-sibling first-child)))
                      (treesit-node-start body)
                    bol)))
    (< (line-number-at-pos (treesit-node-start first-child))
       (line-number-at-pos body-pos))))

(defun replique-clojure--node-pos-match-block (node parent bol block)
  "Return non-nil if NODE's index in PARENT is past BLOCK arguments.
When NODE is nil, use the first child after BOL."
  (if node
      (> (treesit-node-index node) (1+ block))
    (when-let* ((node-after-bol (treesit-node-first-child-for-pos parent bol)))
      (> (treesit-node-index node-after-bol) (1+ block)))))

(defun replique-clojure--match-form-body (node parent bol)
  "Match the body of a form governed by a semantic rule.
See https://guide.clojure.style/#body-indentation"
  (when-let* ((res (replique-clojure--resolve-indentation-context node parent)))
    (let ((logical-parent (car res))
          (direct-child (cdr res)))
      (and (or (replique-clojure--list-node-p logical-parent)
               (replique-clojure--anon-fn-node-p logical-parent))
           (let ((first-child (replique-clojure--first-value-child logical-parent)))
             (when-let* ((rule (replique-clojure--find-semantic-rule
                                (or direct-child first-child) logical-parent 0)))
               (let ((rule-type (car rule))
                     (rule-value (cadr rule)))
                 (if (equal rule-type :block)
                     (if (zerop rule-value)
                         (replique-clojure--match-block-0-body bol first-child)
                       (replique-clojure--node-pos-match-block
                        direct-child logical-parent bol rule-value))
                   t))))))))

(defvar replique-clojure--threading-macro
  (rx (and "->" (? ">") line-end))
  "A regexp matching a threading macro.")

(defun replique-clojure--match-threading-macro-arg (node parent _bol)
  "Match an argument of a threading macro.
See https://guide.clojure.style/#threading-macros-alignment"
  (when-let* ((res (replique-clojure--resolve-indentation-context node parent)))
    (let ((logical-parent (car res)))
      (and (or (replique-clojure--list-node-p logical-parent)
               (replique-clojure--anon-fn-node-p logical-parent))
           (replique-clojure--symbol-matches-p
            replique-clojure--threading-macro
            (replique-clojure--first-value-child logical-parent))))))

(defun replique-clojure--match-function-call-arg (node parent _bol)
  "Match an argument of a plain function call (to align under the first arg).
See https://guide.clojure.style/#vertically-align-fn-args"
  (when-let* ((res (replique-clojure--resolve-indentation-context node parent)))
    (let ((logical-parent (car res))
          (direct-child (cdr res)))
      (and (or (replique-clojure--list-node-p logical-parent)
               (replique-clojure--anon-fn-node-p logical-parent))
           ;; The list a reader conditional holds is not a call.  What is
           ;; written in one is platforms and what each of them stands for,
           ;; and a platform is not the head of anything - so its elements
           ;; line up with one another rather than under the first of them
           (not (equal "reader_conditional"
                       (treesit-node-type (treesit-node-parent logical-parent))))
           (let ((first-child (replique-clojure--first-value-child logical-parent))
                 (second-child (treesit-node-child logical-parent 1 t)))
             (and first-child
                  second-child
                  (or (null direct-child)
                      (not (treesit-node-eq second-child direct-child)))
                  (or (replique-clojure--symbol-node-p first-child)
                      (replique-clojure--keyword-node-p first-child)
                      (replique-clojure--var-node-p first-child))))))))

(defun replique-clojure--match-docstring (_node parent _bol)
  "Match PARENT when it is a docstring (so its interior is left untouched)."
  (when (replique-clojure--string-node-p parent)
    (equal (replique-clojure-docstring-bounds (treesit-node-start parent))
           (cons (treesit-node-start parent) (treesit-node-end parent)))))

(defun replique-clojure--match-string-interior (_node _parent bol)
  "Match when BOL falls inside (not at the start of) a string.
A continuation line of a multi-line string/docstring has no node starting at
BOL, so treesit would otherwise indent it as if it were a body form.  Leaving
it untouched preserves the author's whitespace inside string literals."
  (nth 3 (syntax-ppss bol)))

(defun replique-clojure--indent-rules ()
  "Return the `treesit-simple-indent-rules' for treejure."
  `((treejure
     ((parent-is "^source$") parent-bol 0)
     ;; Never reindent the interior of a multi-line string / docstring.
     (replique-clojure--match-string-interior no-indent 0)
     ;; Literal collections.
     ((parent-is "^vector_literal$") parent 1)
     ((parent-is "^map_literal$") parent 1)
     ((parent-is "^set_literal$") parent 2)
     ((and (parent-is "^reader_conditional$")
           replique-clojure--match-marker-splicing)
      parent 3)
     ((parent-is "^reader_conditional$") parent 2)
     ((parent-is "^tagged_literal$") parent 1)
     ((parent-is "^namespaced_map_literal$") parent 1)
     ;; Semantic body indentation (cljfmt :block / :inner).
     (replique-clojure--match-form-body
      replique-clojure--anchor-logical-parent-opening-paren 2)
     ;; Threading macro arguments.
     (replique-clojure--match-threading-macro-arg
      replique-clojure--anchor-logical-prev-sibling 0)
     ;; Function-call argument alignment.
     (replique-clojure--match-function-call-arg
      ,(replique-clojure--anchor-logical-nth-sibling 1) 0)
     ;; One-space indent for the rest of a list / fn literal.
     ((parent-is "^list_literal$") parent 1)
     ((parent-is "^fn_literal$") parent 2)
     (replique-clojure--match-with-metadata parent 0)
     ;; Catch-all for wrapped elements inside vectors/maps/sets.
     (replique-clojure--match-wrapped-in-non-list-collection
      replique-clojure--anchor-wrapped-in-non-list-collection 0))))


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
Drives sexp navigation, electric pairs and string/comment detection;
highlighting itself comes from treesit, not this table.")


;;;; Grammar installation

(defun replique-clojure--query-valid-p (query)
  "Return non-nil if QUERY compiles against the treejure grammar."
  (ignore-errors
    (treesit-query-compile 'treejure query t)
    t))

(defun replique-clojure--grammar-outdated-p ()
  "Return non-nil if the installed treejure grammar is too old."
  (not (replique-clojure--query-valid-p '((_visible_form)))))

(defvar replique-clojure--grammar-checked nil
  "Internal flag to check/install the grammar at most once per session.")

(defun replique-clojure--ensure-grammars ()
  "Install or update the treejure grammar when needed."
  (when (and replique-clojure-ensure-grammars
             (not replique-clojure--grammar-checked))
    (dolist (recipe replique-clojure-grammar-recipes)
      (let ((grammar (car recipe)))
        (when (or (not (treesit-language-available-p grammar nil))
                  (and (eq grammar 'treejure)
                       (replique-clojure--grammar-outdated-p)))
          (message "Replique: Installing/Updating %s grammar..." grammar)
          (let ((treesit-language-source-alist replique-clojure-grammar-recipes))
            (treesit-install-language-grammar grammar)))))
    (setq replique-clojure--grammar-checked t)))


;;;; Mode setup

(defun replique-clojure--mode-variables ()
  "Set up the buffer-local treesit variables for `replique-clojure-mode'."
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+ *")

  (setq-local replique-clojure--extra-definers
              (replique-clojure--compute-extra-definers
               replique-clojure-extra-def-forms))

  (setq-local treesit-defun-prefer-top-level t)
  (setq-local treesit-defun-tactic 'top-level)
  (setq-local treesit-defun-name-function #'replique-clojure--defun-name-function)

  (setq-local replique-clojure--semantic-indent-rules-cache
              (replique-clojure--compute-semantic-indent-cache
               replique-clojure-semantic-indent-rules))
  (setq-local treesit-simple-indent-rules (replique-clojure--indent-rules))
  (setq-local fill-paragraph-function #'replique-clojure--fill-paragraph)

  (when (boundp 'treesit-thing-settings)   ; Emacs 30+
    (setq-local treesit-thing-settings replique-clojure--thing-settings)))

(defun replique-clojure--hack-local-variables ()
  "Recompute buffer-local caches after `.dir-locals.el' has been applied."
  (setq-local replique-clojure--semantic-indent-rules-cache
              (replique-clojure--compute-semantic-indent-cache
               replique-clojure-semantic-indent-rules))
  (setq-local replique-clojure--extra-definers
              (replique-clojure--compute-extra-definers
               replique-clojure-extra-def-forms))
  (font-lock-flush))

;;;###autoload
(define-derived-mode replique-clojure-mode prog-mode "Replique[clj]"
  "Major mode for editing Clojure code.

Highlighting is read out of `replique-parse', which reads Clojure the way
the Clojure reader does.  Indentation is Emacs' built-in `treesit'.
Semantic faces, diagnostics and navigation come from a C module that is
not part of replique."
  :syntax-table replique-clojure-mode-syntax-table
  ;; Painting asks nothing of a grammar, so it is set up whether or not one
  ;; is there to be had.  What a missing grammar costs is indentation
  (setq-local font-lock-defaults
              '(nil nil nil nil
                    (font-lock-fontify-region-function
                     . replique-clojure-font-lock-region)))
  (add-hook 'change-major-mode-hook #'replique-parse-forget nil t)
  (replique-clojure--ensure-grammars)
  (when (treesit-ready-p 'treejure)
    (treesit-parser-create 'treejure)
    (replique-clojure--mode-variables)
    (treesit-major-mode-setup)
    (add-hook 'hack-local-variables-hook
              #'replique-clojure--hack-local-variables 0 t)))

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
(if (treesit-available-p)
    (progn
      ;; Clojure + EDN
      (add-to-list 'auto-mode-alist
                   '("\\.\\(clj\\|edn\\)\\'" . replique-clojure-mode))
      (add-to-list 'auto-mode-alist '("\\.cljs\\'" . replique-clojure-clojurescript-mode))
      (add-to-list 'auto-mode-alist '("\\.cljc\\'" . replique-clojure-clojurec-mode))
      ;; babashka scripts are Clojure source files.
      (add-to-list 'interpreter-mode-alist '("bb" . replique-clojure-mode)))
  (message "Replique: Clojure mode not activated — Tree-sitter support is missing."))

(provide 'replique-clojure-mode)

;;; replique-clojure-mode.el ends here
