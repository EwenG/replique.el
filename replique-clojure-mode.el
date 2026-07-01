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

;; Version 1.1.0-SNAPSHOT
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; `replique-clojure-mode' is the Tree-sitter *syntax layer* for Clojure and
;; ClojureScript, built on Emacs' built-in `treesit' over the `treejure'
;; grammar. It owns grammar-level font-lock and indentation:
;;
;;   * grammar-level font-lock (`treesit-font-lock-rules') — strings, regexes,
;;     keywords, comments, numbers, characters, brackets, errors, plus the
;;     "built-in knowledge" faces (def names, builtins, types, docstrings) that
;;     are still grammar-shaped, matched on a list's head symbol;
;;   * cljfmt indentation (`treesit-simple-indent') — the `:block'/`:inner' rule
;;     model, with a `no-indent' seam so multi-line string interiors are left
;;     untouched.
;;
;; Semantic faces (`:local', `:macro-invocation', `:special-form',
;; `:unresolved', unused greyout), diagnostics and navigation are deliberately
;; NOT here: They are computed by the C semantic module and layered *on top* of
;; these faces as an overlay. This file is pure, in-core treesit.
;;
;; Customization (M-x customize-group RET replique-clojure RET):
;;   `replique-clojure-ensure-grammars'           install/update the grammar
;;   `replique-clojure-extra-def-forms'           macros highlighted like defn
;;   `replique-clojure-semantic-indent-rules'     per-symbol indent overrides
;;   `replique-clojure-docstring-fill-column'     fill-column for docstrings
;;   `replique-clojure-docstring-fill-prefix-width'  docstring fill prefix
;; The faces are themeable via the deffaces below, and the indent rules /
;; extra def-forms are `.dir-locals.el'-friendly.  The single semantic seam in
;; indentation — resolving a symbol to a `clojure-mode'-style indent spec — is
;; exposed through `replique-clojure-get-indent-function'.

;;; Code:

(require 'treesit)
(require 'seq)
(eval-when-compile (require 'subr-x))   ; thread-first / thread-last / when-let*

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-eq "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function replique-clojure-semantic-mode "replique-clojure-semantic")


;;;; Customization

(defgroup replique-clojure nil
  "Tree-sitter syntax layer for Clojure (treejure grammar)."
  :prefix "replique-clojure-"
  :group 'languages)

(defcustom replique-clojure-ensure-grammars t
  "When non-nil, ensure the required Tree-sitter grammars are installed."
  :safe #'booleanp
  :type 'boolean)

(defcustom replique-clojure-enable-semantic t
  "When non-nil, layer the treejure semantic analysis over the syntax mode.
On entering `replique-clojure-mode' this loads `replique-clojure-semantic' and
enables `replique-clojure-semantic-mode' (the C-module client: semantic faces +
Flymake diagnostics).  When the library or the C module is unavailable the
major mode still works as the pure treesit syntax layer."
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
;; Grammar faces are assigned directly in the font-lock rules (the
;; clojure-ts-mode model).  Two categories that have no good standard face get a
;; dedicated, themeable face here; everything else reuses the standard
;; `font-lock-*' faces, so a theme controls them with no rebuild.

(defface replique-clojure-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face for Clojure keywords (`:something').")

(defface replique-clojure-character-face
  '((t (:inherit font-lock-string-face)))
  "Face for Clojure character literals (`\\a').")


;;;; Built-in knowledge — symbol sets
;;
;; These drive the grammar-shaped "built-in knowledge" faces: which head symbols
;; read as builtins, which forms define vars/types/interfaces, and which take a
;; docstring.  Kept as plain regexps so the font-lock queries can `:match' them.

(defconst replique-clojure-builtin-symbols-regexp
  (eval-when-compile
    (concat "^"
            (regexp-opt
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
               "deftest" "deftest-" "is" "are" "testing"))
            "$"))
  "A regexp matching Clojure special forms and core macros.")

(defconst replique-clojure-function-docstring-symbols
  (rx line-start
      (or "definline" "defmulti" "defmacro" "defn" "defn-" "defprotocol" "ns")
      line-end)
  "Symbols whose second argument may be a docstring.")

(defconst replique-clojure-definition-docstring-symbols
  (rx line-start "def" line-end)
  "Symbols whose second argument is a docstring only when a value follows.
\"def\" is the only builtin Clojure symbol that behaves like this.")

(defconst replique-clojure-variable-definition-symbol-regexp
  (rx line-start (or "def" "defonce") line-end)
  "A regexp matching a symbol used to define a variable.")

(defconst replique-clojure-typedef-symbol-regexp
  (rx line-start
      (or "defprotocol" "defmulti" "deftype" "defrecord"
          "definterface" "defmethod" "defstruct")
      line-end)
  "A regexp matching a symbol used to define a type.")

(defconst replique-clojure-interface-def-symbol-regexp
  (rx line-start (or "defprotocol" "definterface") line-end)
  "A regexp matching a symbol used to define an interface.")

(defun replique-clojure-symbol-regexp (symbols)
  "Return a regexp matching exactly one of SYMBOLS."
  (concat "^" (regexp-opt symbols) "$"))


;;;; Font-lock — query helpers

(defmacro replique-clojure--meta-choice (&rest pattern)
  "Wrap PATTERN in a choice supporting 0 to 3 levels of metadata nesting.
Metadata (`^:foo', `^Type') wraps the form it precedes in `with_metadata'
nodes; matching the head/name symbol of a form therefore has to see through
them.  Expands to a Tree-sitter alternation vector."
  `[,@pattern
    (with_metadata target: ,@pattern)
    (with_metadata target: (with_metadata target: ,@pattern))
    (with_metadata target: (with_metadata target: (with_metadata target: ,@pattern)))])

(defun replique-clojure--docstring-query ()
  "Return the font-lock query that captures docstrings."
  `(;; (def name "docstring" value)
    ((list_literal :anchor [(comment) (discard)] :*
                   :anchor (symbol name: (symbol_name) @_def_symbol)
                   :anchor [(comment) (discard)] :*
                   :anchor ,(replique-clojure--meta-choice (symbol))
                   :anchor [(comment) (discard)] :*
                   :anchor ,(replique-clojure--meta-choice (string) @font-lock-doc-face)
                   :anchor (_visible_form))
     (:match ,replique-clojure-definition-docstring-symbols @_def_symbol))
    ;; Docstrings supplied through the name's metadata map (:doc "...").
    ((list_literal :anchor [(comment) (discard)] :*
                   :anchor (symbol name: (symbol_name) @_def_symbol)
                   :anchor [(comment) (discard)] :*
                   :anchor (with_metadata
                            meta: (metadata
                                   value: (map_literal
                                           (pair
                                            key: (keyword name: (symbol_name) @_doc-keyword)
                                            value: (string) @font-lock-doc-face)))))
     (:match ,(replique-clojure-symbol-regexp
               '("def" "defonce" "defn" "defn-" "defmacro" "ns"
                 "defmulti" "definterface" "defprotocol"
                 "deftest" "deftest-"
                 "deftype" "defrecord" "defstruct"))
             @_def_symbol)
     (:equal @_doc-keyword "doc"))
    ;; (defn name "docstring" [args] ...) and friends.
    ((list_literal :anchor [(comment) (discard)] :*
                   :anchor (symbol name: (symbol_name) @_def_symbol)
                   :anchor [(comment) (discard)] :*
                   :anchor ,(replique-clojure--meta-choice (symbol))
                   :anchor [(comment) (discard)] :*
                   :anchor ,(replique-clojure--meta-choice (string) @font-lock-doc-face))
     (:match ,replique-clojure-function-docstring-symbols @_def_symbol))
    ;; (defprotocol P (method [args] "docstring")).
    ((list_literal :anchor [(comment) (discard)] :*
                   :anchor (symbol name: (symbol_name) @_def_symbol)
                   (list_literal :anchor ,(replique-clojure--meta-choice (symbol))
                                 :anchor ,(replique-clojure--meta-choice (vector_literal))
                                 (string) @font-lock-doc-face))
     (:match ,replique-clojure-interface-def-symbol-regexp @_def_symbol))))

(defconst replique-clojure--match-docstring-query
  (treesit-query-compile 'treejure (replique-clojure--docstring-query))
  "Precompiled query matching Clojure docstrings (used by filling too).")


;;;; Font-lock — rules

(defconst replique-clojure--font-lock-queries
  (treesit-font-lock-rules
   :language 'treejure
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'treejure
   :feature 'regex
   :override t
   '((regex
      marker: _ @font-lock-preprocessor-face
      value: (string) @font-lock-string-face))

   :language 'treejure
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'treejure
   :feature 'constant
   '([(nil) (boolean) (symbolic_value)] @font-lock-constant-face)

   :language 'treejure
   :feature 'char
   '((character) @replique-clojure-character-face)

   :language 'treejure
   :feature 'keyword
   '((keyword
      marker: _ @replique-clojure-keyword-face
      namespace: (symbol_name) @font-lock-type-face
      name: (symbol_name) @replique-clojure-keyword-face)
     (keyword
      marker: _ @replique-clojure-keyword-face
      name: (symbol_name) @replique-clojure-keyword-face))

   :language 'treejure
   :feature 'builtin
   `(((list_literal
       :anchor [(comment) (discard)] :*
       :anchor ,(replique-clojure--meta-choice
                 (symbol !namespace name: (symbol_name) @font-lock-keyword-face)))
      (:match ,replique-clojure-builtin-symbols-regexp @font-lock-keyword-face))
     ((list_literal
       :anchor [(comment) (discard)] :*
       :anchor ,(replique-clojure--meta-choice
                 (symbol namespace: ((symbol_name) @ns (:equal "clojure.core" @ns))
                         name: (symbol_name) @font-lock-keyword-face)))
      (:match ,replique-clojure-builtin-symbols-regexp @font-lock-keyword-face))
     ((fn_literal
       :anchor [(comment) (discard)] :*
       :anchor ,(replique-clojure--meta-choice
                 (symbol !namespace name: (symbol_name) @font-lock-keyword-face)))
      (:match ,replique-clojure-builtin-symbols-regexp @font-lock-keyword-face))
     ((fn_literal
       :anchor [(comment) (discard)] :*
       :anchor ,(replique-clojure--meta-choice
                 (symbol namespace: ((symbol_name) @ns (:equal "clojure.core" @ns))
                         name: (symbol_name) @font-lock-keyword-face)))
      (:match ,replique-clojure-builtin-symbols-regexp @font-lock-keyword-face)))

   :language 'treejure
   :feature 'earmuff
   '(((symbol name: (symbol_name) @font-lock-warning-face)
      (:match "^\\*.*\\*$" @font-lock-warning-face)))

   :language 'treejure
   :feature 'symbol
   '((symbol namespace: (symbol_name) @font-lock-type-face))

   :language 'treejure
   :feature 'definition
   :override t
   `(;; (defn name ...), (defmacro name ...), ...
     ((list_literal
       :anchor ,(replique-clojure--meta-choice (symbol name: (symbol_name) @font-lock-keyword-face))
       :anchor [(comment) (discard)] :*
       :anchor ,(replique-clojure--meta-choice (symbol name: (symbol_name) @font-lock-function-name-face)))
      (:match ,(rx bol (or "fn" "defn" "defn-" "defmulti" "defmethod"
                           "deftest" "deftest-" "defmacro" "definline" "defonce")
                   eol)
              @font-lock-keyword-face))
     ;; Method implementations in defrecord/deftype/defprotocol/definterface.
     ((list_literal
       :anchor (,(replique-clojure--meta-choice (symbol name: (symbol_name) @def))
                ((:match ,(rx bol (or "defrecord" "definterface" "deftype" "defprotocol") eol)
                         @def)))
       :anchor [(comment) (discard)] :*
       :anchor [(symbol name: (symbol_name) @font-lock-type-face)]
       (list_literal (symbol name: (symbol_name) @font-lock-function-name-face))))
     ;; reify / extend-protocol / extend-type method implementations.
     ((list_literal
       :anchor ([(symbol name: (symbol_name) @def)]
                ((:match ,(rx bol (or "reify" "extend-protocol" "extend-type") eol) @def)))
       :anchor [(comment) (discard)] :*
       :anchor [(symbol name: (symbol_name) @font-lock-type-face)]
       (list_literal (symbol name: (symbol_name) @font-lock-function-name-face))))
     ;; letfn local function names.
     ((list_literal
       :anchor ([(symbol name: (symbol_name) @symbol)] ((:equal "letfn" @symbol)))
       (vector_literal (list_literal (symbol name: (symbol_name) @font-lock-function-name-face))))))

   :language 'treejure
   :feature 'variable
   `(((list_literal
       :anchor (symbol name: (symbol_name) @font-lock-keyword-face)
       :anchor [(comment) (discard)] :*
       :anchor ,(replique-clojure--meta-choice (symbol name: (symbol_name) @font-lock-variable-name-face)))
      (:match ,replique-clojure-variable-definition-symbol-regexp @font-lock-keyword-face)))

   :language 'treejure
   :feature 'type
   :override t
   `(;; (deftype Name ...), (defrecord Name ...), ...
     ((list_literal
       :anchor (symbol name: (symbol_name) @def)
       :anchor [(comment) (discard)] :*
       :anchor ,(replique-clojure--meta-choice (symbol name: (symbol_name) @font-lock-type-face)))
      (:match ,replique-clojure-typedef-symbol-regexp @def))
     ;; Type hints carried in metadata.
     (with_metadata meta: (metadata value: (symbol name: (symbol_name) @font-lock-type-face)))
     ;; The namespace name in an (ns ...) form.
     ((list_literal
       :anchor (symbol name: (symbol_name) @def)
       :anchor (symbol name: (symbol_name) @font-lock-type-face))
      (:equal "ns" @def)))

   :language 'treejure
   :feature 'tagged-literals
   :override t
   '((tagged_literal
      marker: _ @font-lock-preprocessor-face
      tag: (symbol) @font-lock-preprocessor-face))

   :language 'treejure
   :feature 'doc
   :override t
   (replique-clojure--docstring-query)

   :language 'treejure
   :feature 'quote
   '((var_quote marker: _ @font-lock-delimiter-face))

   :language 'treejure
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face
     (set_literal :anchor "#{" @font-lock-bracket-face))

   :language 'treejure
   :feature 'comment
   :override t
   '((comment) @font-lock-comment-face
     (shebang) @font-lock-comment-face
     (discard "#_" @font-lock-comment-delimiter-face
              target: (_) @font-lock-comment-face)
     ((list_literal :anchor (symbol) @font-lock-comment-delimiter-face)
      (:match "^\\(\\(clojure.core/\\)?comment\\)$" @font-lock-comment-delimiter-face)))

   ;; Level-4, off by default: any non-builtin head symbol read as a call.
   ;; Inherently approximate (macros, quoted lists, imports), hence opt-in.
   :language 'treejure
   :feature 'function
   '((list_literal :anchor (symbol name: (symbol_name) @font-lock-function-call-face))
     (fn_literal :anchor (symbol name: (symbol_name) @font-lock-function-call-face)))

   ;; `invalid_string' is a hidden grammar rule and not directly queryable;
   ;; malformed strings still surface through the surrounding `ERROR' node.
   :language 'treejure
   :feature 'error
   '((invalid_character) @font-lock-warning-face
     (invalid_number) @font-lock-warning-face
     (erroneous_symbolic_value) @font-lock-warning-face
     (ERROR) @font-lock-warning-face
     (MISSING) @font-lock-warning-face))
  "Grammar-level + built-in-knowledge font-lock rules for treejure.")


;;;; Font-lock — user-extensible def forms

(defvar-local replique-clojure--extra-queries nil
  "Compiled font-lock rules derived from `replique-clojure-extra-def-forms'.")

(defun replique-clojure--compute-extra-def-queries (syms)
  "Return font-lock rules highlighting SYMS like `defn', or nil when empty."
  (when syms
    (treesit-font-lock-rules
     :language 'treejure
     :feature 'definition
     :override t
     `(((list_literal
         :anchor ,(replique-clojure--meta-choice (symbol name: (symbol_name) @font-lock-keyword-face))
         :anchor [(comment) (discard)] :*
         :anchor ,(replique-clojure--meta-choice (symbol name: (symbol_name) @font-lock-function-name-face)))
        (:match ,(replique-clojure-symbol-regexp syms) @font-lock-keyword-face)))

     :language 'treejure
     :feature 'doc
     :override t
     ;; Mirror the built-in `defn'-family docstring query (see
     ;; `replique-clojure--docstring-query'): a string in the second argument
     ;; position is the docstring, with no trailing form required -- extra
     ;; def-forms are highlighted exactly like `defn', not like value-carrying
     ;; `def'.
     `(((list_literal
         :anchor [(comment) (discard)] :*
         :anchor (symbol name: (symbol_name) @_def_symbol)
         :anchor [(comment) (discard)] :*
         :anchor ,(replique-clojure--meta-choice (symbol))
         :anchor [(comment) (discard)] :*
         :anchor ,(replique-clojure--meta-choice (string) @font-lock-doc-face))
        (:match ,(replique-clojure-symbol-regexp syms) @_def_symbol))))))

(defun replique-clojure--font-lock-settings ()
  "Return the full font-lock settings for the current buffer."
  (append replique-clojure--font-lock-queries
          replique-clojure--extra-queries))

(declare-function replique-clojure--refresh-def-forms "replique-clojure-semantic")

(defun replique-clojure--set-extra-def-forms (symbol value)
  "Setter for `replique-clojure-extra-def-forms'.
Sets SYMBOL to VALUE and refreshes every `replique-clojure-mode' buffer — both
the treesit font-lock and, where active, the semantic layer."
  (set-default-toplevel-value symbol value)
  (let ((new (replique-clojure--compute-extra-def-queries value)))
    (dolist (buf (buffer-list))
      (when (buffer-local-boundp 'replique-clojure--extra-queries buf)
        (with-current-buffer buf
          (setq replique-clojure--extra-queries new)
          (setq-local treesit-font-lock-settings (replique-clojure--font-lock-settings))
          (font-lock-flush)
          (when (and (bound-and-true-p replique-clojure-semantic-mode)
                     (fboundp 'replique-clojure--refresh-def-forms))
            (replique-clojure--refresh-def-forms)))))))

(defcustom replique-clojure-extra-def-forms nil
  "List of macro names highlighted the same way as `defn'.
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

(defvar replique-clojure-get-indent-function nil
  "Function returning the dynamic indent spec of a symbol, or nil.
Called with the symbol name exactly as it appears in the buffer (it may carry
a namespace alias).  This is the single semantic seam in indentation: a client
\(e.g. the REPL) resolves a project macro's `:style/indent' / arglist to a
`clojure-mode'-compatible spec, which is converted to a treejure rule here.")


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

(defun replique-clojure--node-namespace-text (node)
  "Return the namespace of symbol/keyword NODE, or nil."
  (treesit-node-text (treesit-node-child-by-field-name node "namespace")))

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


;;;; Indentation — dynamic (semantic) seam

(defun replique-clojure--unwrap-dynamic-spec (spec current-depth)
  "Convert a nested `clojure-mode' SPEC at CURRENT-DEPTH to a treejure rule.
For example ((:defn)) becomes (:inner 2) and (:defn) becomes (:inner 1)."
  (if (consp spec)
      (replique-clojure--unwrap-dynamic-spec (car spec) (1+ current-depth))
    (cond
     ((equal spec :defn) (list :inner current-depth))
     (t nil))))

(defun replique-clojure--dynamic-indent-for-symbol (sym &optional ns)
  "Return the dynamic indentation rule list for SYM (optionally in NS), or nil.
Consults `replique-clojure-get-indent-function' and converts its
`clojure-mode'-compatible return value into treejure rules.  For example
\(1 ((:defn)) nil) becomes ((:block 1) (:inner 2))."
  (when (and sym (functionp replique-clojure-get-indent-function))
    (let* ((full-symbol (if ns (concat ns "/" sym) sym))
           (spec (funcall replique-clojure-get-indent-function full-symbol)))
      (if (integerp spec)
          (list (list :block spec))
        (when (sequencep spec)
          (thread-last spec
                       (seq-map (lambda (el)
                                  (cond
                                   ((integerp el) (list :block el))
                                   ((equal el :defn) (list :inner 0))
                                   ((consp el) (replique-clojure--unwrap-dynamic-spec el 0))
                                   (t nil))))
                       (seq-remove #'null)
                       (seq-sort (lambda (spec1 _spec2)
                                   (equal (car spec1) :block)))))))))

(defun replique-clojure--find-semantic-rules-for-node (node)
  "Return the list of semantic rules for NODE's head symbol."
  (when-let* ((first-child (treesit-node-child node 0 t)))
    (let ((symbol-name (replique-clojure--named-node-text first-child))
          (symbol-namespace (replique-clojure--node-namespace-text first-child)))
      (or (replique-clojure--dynamic-indent-for-symbol symbol-name symbol-namespace)
          (alist-get symbol-name
                     replique-clojure--semantic-indent-rules-cache
                     nil nil #'equal)))))

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
    (when-let* ((top-level-node (treesit-parent-until parent 'defun t))
                (result (treesit-query-capture
                         top-level-node replique-clojure--match-docstring-query)))
      (seq-find (lambda (elt)
                  (and (eq (car elt) 'font-lock-doc-face)
                       (treesit-node-eq (cdr elt) parent)))
                result))))

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
  (let ((current-node (treesit-node-at (point) 'treejure t)))
    (if (replique-clojure--match-docstring nil current-node nil)
        (let ((fill-column (or replique-clojure-docstring-fill-column fill-column))
              (fill-prefix (replique-clojure--docstring-fill-prefix))
              (beg-doc (treesit-node-start current-node))
              (end-doc (treesit-node-end current-node)))
          (save-restriction
            (narrow-to-region beg-doc end-doc)
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

  (setq-local replique-clojure--extra-queries
              (replique-clojure--compute-extra-def-queries
               replique-clojure-extra-def-forms))
  (setq-local treesit-font-lock-settings
              (replique-clojure--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '((comment definition variable)
                (keyword string char symbol builtin type earmuff error)
                (constant number quote doc regex)
                (bracket function tagged-literals)))

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
  (setq-local replique-clojure--extra-queries
              (replique-clojure--compute-extra-def-queries
               replique-clojure-extra-def-forms))
  (setq-local treesit-font-lock-settings (replique-clojure--font-lock-settings))
  (font-lock-flush))

(defconst replique-clojure--data-file-extensions '("edn")
  "Extensions of EDN data files (not Clojure source).
`.edn' hold pure data — no namespace form, no code — so the semantic
layer has nothing to analyse and is skipped for them.")

(defun replique-clojure--data-buffer-p ()
  "Return non-nil if this buffer holds EDN data rather than Clojure source."
  (and buffer-file-name
       (member (file-name-extension buffer-file-name)
               replique-clojure--data-file-extensions)))

;;;###autoload
(define-derived-mode replique-clojure-mode prog-mode "Replique[clj]"
  "Major mode for editing Clojure code, built on Tree-sitter (treejure grammar).

Highlighting and indentation are provided by Emacs' built-in `treesit'.
Semantic faces, diagnostics and navigation are layered separately by the
treejure semantic module and are not part of this mode."
  :syntax-table replique-clojure-mode-syntax-table
  (replique-clojure--ensure-grammars)
  (when (treesit-ready-p 'treejure)
    (treesit-parser-create 'treejure)
    (replique-clojure--mode-variables)
    (treesit-major-mode-setup)
    (add-hook 'hack-local-variables-hook
              #'replique-clojure--hack-local-variables 0 t)
    ;; Layer the treejure semantic faces/diagnostics on top, when enabled and
    ;; the C module is available.  The library is loaded lazily so the syntax
    ;; mode stays self-contained; failures degrade to the pure syntax layer.
    ;; Skipped for EDN data buffers (`.edn'): they carry no namespace or
    ;; code for the semantic pass to analyse.
    (when (and replique-clojure-enable-semantic module-file-suffix
               (not (replique-clojure--data-buffer-p))
               (require 'replique-clojure-semantic nil t))
      (replique-clojure-semantic-mode 1))))

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
