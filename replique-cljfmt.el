;;; replique-cljfmt.el --- What cljfmt is configured to do  -*- lexical-binding: t; -*-

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

;; The configuration cljfmt reads, read the way clojure-lsp reads it - which
;; is what `clojure-lsp format', and so `bb fmt' in a project that wraps it,
;; formats a file by.  Indenting in the editor and formatting on the command
;; line have to agree, or every file that is saved from one is a diff in the
;; other.
;;
;; What is read, in the order a later one wins:
;;
;;   * cljfmt's own defaults - `clojure.clj', `compojure.clj' and
;;     `fuzzy.clj', carried here as the EDN they are written in upstream
;;   * the `:cljfmt' map of clojure-lsp's settings, global then project
;;   * the file `:cljfmt-config-path' names, `.cljfmt.edn' by default,
;;     deep-merged over that
;;   * `replique-clojure-semantic-indent-rules', which is per buffer and is
;;     what a `.dir-locals.el' says
;;
;; `:indents' is merged into the defaults rather than replacing them, which
;; is what clojure-lsp does and is not what cljfmt on its own does.  And
;; where nothing names a config path, the four names cljfmt looks for are all
;; tried, `.cljfmt.edn' first - clojure-lsp only tries the one.
;;
;; What clojure-lsp adds from its analysis - the `:style/indent' metadata of
;; the macros a project defines - is not here: it would take a repl.
;;
;; The keys of an indent rule are symbols, qualified symbols, regexes, or a
;; vector of two of those.  The regexes are Java's, and they are run here by
;; translating them, lookaheads included, which Emacs has none of: cljfmt's
;; own `#"^def(?!ault)(?!late)(?!er)"' is written with three.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'replique-parse)


;;;; Customization

(defgroup replique-cljfmt nil
  "Formatting Clojure the way cljfmt is configured to."
  :prefix "replique-cljfmt-"
  :group 'languages)

(defcustom replique-cljfmt-read-project-config t
  "Whether the cljfmt configuration of a project is read.

When nil only cljfmt's defaults and `replique-clojure-semantic-indent-rules'
are followed."
  :type 'boolean
  :safe #'booleanp)


;;;; Reading EDN
;;
;; Out of the tree `replique-parse' reads, rather than with a reader of its
;; own.  A configuration is small and what it holds is a handful of shapes,
;; so they are given a shape each here:
;;
;;   map      (:map (KEY . VALUE) ...)
;;   vector   a vector, and so is a list and a set
;;   symbol   (:symbol . TEXT)
;;   regex    (:regex . SOURCE), from `#"..."' and from `#re "..."'
;;   keyword  the Emacs keyword of the same name
;;   boolean  t, or :false - nil is nil

(defun replique-cljfmt--edn-string (text)
  "The string the Clojure string literal TEXT says, or nil."
  (condition-case nil
      (let ((value (car (read-from-string text))))
        (and (stringp value) value))
    (error nil)))

(defun replique-cljfmt--edn-value (node)
  "The EDN value NODE says, in the shapes this file gives them."
  (let ((text (and node (replique-parse-text node))))
    (pcase (replique-parse-type node)
      ('map
       (cons :map
             (let ((entries nil))
               (dolist (child (replique-parse-children node) (nreverse entries))
                 (when (eq 'pair (replique-parse-type child))
                   (let ((forms (replique-parse-forms child)))
                     (push (cons (replique-cljfmt--edn-value (car forms))
                                 (replique-cljfmt--edn-value (cadr forms)))
                           entries)))))))
      ((or 'vector 'list 'set)
       (vconcat (mapcar #'replique-cljfmt--edn-value (replique-parse-forms node))))
      ('symbol (cons :symbol text))
      ('keyword (intern text))
      ('string (replique-cljfmt--edn-string text))
      ;; What is between the quotes of a regex literal is handed to the
      ;; pattern as it is written: nothing is unescaped
      ('regex (cons :regex (substring text 2 -1)))
      ('number (string-to-number text))
      ('boolean (if (equal text "true") t :false))
      ('meta (replique-cljfmt--edn-value (replique-parse-target node)))
      ('tagged
       (let ((forms (replique-parse-forms node)))
         (when (equal "re" (replique-parse-text (car forms)))
           (let ((source (replique-cljfmt--edn-value (cadr forms))))
             (when (stringp source) (cons :regex source))))))
      (_ nil))))

(defun replique-cljfmt-read-edn (string)
  "The first form STRING holds, read as EDN, or nil where it does not read."
  (with-temp-buffer
    (insert string)
    (let* ((root (replique-parse-buffer))
           (form (seq-find (lambda (node) (not (replique-parse-gap-p node)))
                           (replique-parse-children root))))
      (when (and form (null (replique-parse-error form)))
        (replique-cljfmt--edn-value form)))))

(defun replique-cljfmt--read-file (file)
  "The EDN FILE holds, or nil where there is no such file or it does not read."
  (when (file-readable-p file)
    (let ((value (replique-cljfmt-read-edn
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))))
      (unless (eq :map (car-safe value))
        (message "replique: %s does not read as a map, ignored" file)
        (setq value nil))
      value)))

(defun replique-cljfmt--get (map key)
  "The value KEY has in the EDN MAP, or nil."
  (cdr (assoc key (cdr-safe map))))

(defun replique-cljfmt--true-p (value)
  "Return non-nil when the EDN VALUE is truthy."
  (and value (not (eq value :false))))

(defun replique-cljfmt--merge (a b &optional deep)
  "The EDN map A with the entries of B over it, merged DEEP where asked."
  (let ((entries (mapcar (lambda (entry) (cons (car entry) (cdr entry)))
                         (cdr-safe a))))
    (dolist (entry (cdr-safe b))
      (let* ((old (assoc (car entry) entries))
             (value (if (and deep old
                             (eq :map (car-safe (cdr old)))
                             (eq :map (car-safe (cdr entry))))
                        (replique-cljfmt--merge (cdr old) (cdr entry) t)
                      (cdr entry))))
        (if old
            (setcdr (assoc (car entry) entries) value)
          (setq entries (append entries (list (cons (car entry) value)))))))
    (cons :map entries)))


;;;; Java regexes
;;
;; A regex is turned into a list of steps: an Emacs regexp to search for,
;; and after it, at the position it matched up to, regexps that must match
;; there, must not match there, or must match and be stepped over.  Which is
;; what a lookahead written at the top level of a pattern is, and that is the
;; only place one is understood: `X(?!Y)Z' is X searched for, Y refused where
;; it ended and Z matched from there.  It is exact wherever X can match only
;; one way from where it starts, which is what anybody writes to name a
;; family of macros - and anything it cannot say is refused rather than
;; guessed at, so that a rule is followed or is reported, never followed
;; wrongly.

(defun replique-cljfmt--class (source i)
  "Translate the character class opening at I in SOURCE.
Return a cons of the Emacs class and where the Java one ends."
  (let ((n (length source))
        (j (1+ i))
        (negate nil)
        (items nil)
        (bracket nil)
        (caret nil)
        (dash nil)
        (first t)
        (done nil))
    (when (and (< j n) (eq (aref source j) ?^))
      (setq negate t j (1+ j)))
    (while (not done)
      (when (>= j n) (error "Unclosed character class"))
      (let ((c (aref source j)))
        (cond
         ((and (eq c ?\]) (not first)) (setq done t))
         ((eq c ?\]) (setq bracket t))
         ((eq c ?\[) (error "Nested character classes are not supported"))
         ((and (eq c ?&) (< (1+ j) n) (eq (aref source (1+ j)) ?&))
          (error "Class intersections are not supported"))
         ((eq c ?\\)
          (when (>= (1+ j) n) (error "Trailing backslash"))
          (let ((e (aref source (1+ j))))
            (setq j (1+ j))
            (pcase e
              (?d (push "0-9" items))
              (?w (push "[:alnum:]_" items))
              (?s (push "[:space:]" items))
              (?t (push "\t" items))
              (?n (push "\n" items))
              (?r (push "\r" items))
              (?f (push "\f" items))
              (?\] (setq bracket t))
              (?- (setq dash t))
              (?^ (setq caret t))
              (_ (if (or (and (>= e ?a) (<= e ?z)) (and (>= e ?A) (<= e ?Z))
                         (and (>= e ?0) (<= e ?9)))
                     (error "Unsupported escape \\%c in a class" e)
                   (push (char-to-string e) items))))))
         ((and (eq c ?-)
               (or first (and (< (1+ j) n) (eq (aref source (1+ j)) ?\]))))
          (setq dash t))
         ((eq c ?^) (setq caret t))
         (t (push (char-to-string c) items))))
      (setq first nil j (1+ j)))
    (cons (concat "[" (if negate "^" "") (if bracket "]" "")
                  (apply #'concat (nreverse items))
                  (if caret "^" "") (if dash "-" "") "]")
          j)))

(defun replique-cljfmt--translate (source)
  "The Emacs regexp the Java regex SOURCE is, lookaheads aside."
  (let ((n (length source))
        (i 0)
        (out nil))
    (while (< i n)
      (let ((c (aref source i)))
        (cond
         ((eq c ?\\)
          (when (>= (1+ i) n) (error "Trailing backslash"))
          (let ((e (aref source (1+ i))))
            (setq i (+ i 2))
            (push (pcase e
                    (?d "[0-9]") (?D "[^0-9]")
                    (?w "[[:alnum:]_]") (?W "[^[:alnum:]_]")
                    (?s "[[:space:]]") (?S "[^[:space:]]")
                    (?b "\\b") (?B "\\B") (?A "\\`") (?z "\\'") (?Z "\\'")
                    (?t "\t") (?n "\n") (?r "\r") (?f "\f")
                    (?Q (let ((end (or (string-search "\\E" source i) n)))
                          (prog1 (regexp-quote (substring source i end))
                            (setq i (min n (+ end 2))))))
                    (_ (if (or (and (>= e ?a) (<= e ?z)) (and (>= e ?A) (<= e ?Z))
                               (and (>= e ?0) (<= e ?9)))
                           (error "Unsupported escape \\%c" e)
                         (regexp-quote (char-to-string e)))))
                  out)))
         ((eq c ?\[)
          (let ((class (replique-cljfmt--class source i)))
            (push (car class) out)
            (setq i (cdr class))))
         ((eq c ?\()
          (cond
           ((string-prefix-p "(?:" (substring source i (min n (+ i 3))))
            (push "\\(?:" out)
            (setq i (+ i 3)))
           ((and (< (1+ i) n) (eq (aref source (1+ i)) ??))
            (error "Unsupported group construct at %d" i))
           (t (push "\\(" out) (setq i (1+ i)))))
         ((eq c ?\)) (push "\\)" out) (setq i (1+ i)))
         ((eq c ?|) (push "\\|" out) (setq i (1+ i)))
         ((and (eq c ?{)
               (eq i (string-match "{\\([0-9]+\\)\\(,[0-9]*\\)?}" source i)))
          (push (concat "\\{" (match-string 1 source)
                        (or (match-string 2 source) "") "\\}")
                out)
          (setq i (match-end 0)))
         (t (push (char-to-string c) out) (setq i (1+ i))))))
    (apply #'concat (nreverse out))))

(defun replique-cljfmt--split (source)
  "SOURCE cut at the lookaheads written at its top level.
A list of (KIND . JAVA-SOURCE), KIND being `re', `is' or `not'."
  (let ((n (length source))
        (i 0)
        (depth 0)
        (start 0)
        (alternation nil)
        (parts nil))
    (while (< i n)
      (let ((c (aref source i)))
        (cond
         ((eq c ?\\)
          (if (and (< (1+ i) n) (eq (aref source (1+ i)) ?Q))
              (setq i (let ((end (string-search "\\E" source i)))
                        (if end (+ end 2) n)))
            (setq i (+ i 2))))
         ((eq c ?\[) (setq i (cdr (replique-cljfmt--class source i))))
         ((and (eq c ?\() (= depth 0)
               (member (substring source i (min n (+ i 3))) '("(?=" "(?!")))
          (let ((kind (if (eq (aref source (+ i 2)) ?=) 'is 'not))
                (j (+ i 3))
                (inner 1))
            (while (and (< j n) (> inner 0))
              (let ((d (aref source j)))
                (cond
                 ((eq d ?\\) (setq j (1+ j)))
                 ((eq d ?\[) (setq j (1- (cdr (replique-cljfmt--class source j)))))
                 ((eq d ?\() (setq inner (1+ inner)))
                 ((eq d ?\)) (setq inner (1- inner)))))
              (setq j (1+ j)))
            (when (> inner 0) (error "Unclosed group"))
            (push (cons 're (substring source start i)) parts)
            (push (cons kind (substring source (+ i 3) (1- j))) parts)
            (setq i j start j)))
         ((eq c ?\() (setq depth (1+ depth) i (1+ i)))
         ((eq c ?\)) (setq depth (1- depth) i (1+ i)))
         ((and (eq c ?|) (= depth 0)) (setq alternation t i (1+ i)))
         (t (setq i (1+ i))))))
    (push (cons 're (substring source start)) parts)
    (setq parts (nreverse parts))
    (when (and alternation (cdr parts))
      (error "A lookahead beside a top level alternation is not supported"))
    parts))

(defun replique-cljfmt-regex (source)
  "The Java regex SOURCE, as something `replique-cljfmt-regex-match' runs.
Signals an error saying why where SOURCE says something that cannot be
translated."
  (list :regex source
        (mapcar (lambda (part)
                  (cons (car part) (replique-cljfmt--translate (cdr part))))
                (seq-remove (lambda (part)
                              (and (eq 're (car part)) (equal "" (cdr part))))
                            (replique-cljfmt--split source)))))

(defun replique-cljfmt-regex-match (regex string)
  "Return non-nil when REGEX is found in STRING, the way `re-find' finds it."
  (let* ((steps (nth 2 regex))
         (steps (if (eq 're (caar steps)) steps (cons '(re . "") steps)))
         (case-fold-search nil)
         (n (length string))
         (start 0)
         (found nil))
    (while (and (not found) start (<= start n))
      (let ((beginning (string-match (cdar steps) string start)))
        (if (null beginning)
            (setq start nil)
          (let ((position (match-end 0))
                (ok t)
                (rest (cdr steps)))
            (while (and ok rest)
              (let ((at (eql position (string-match (cdar rest) string position))))
                (pcase (caar rest)
                  ('re (if at (setq position (match-end 0)) (setq ok nil)))
                  ('is (unless at (setq ok nil)))
                  ('not (when at (setq ok nil)))))
              (setq rest (cdr rest)))
            (if ok
                (setq found t)
              (setq start (1+ beginning)))))))
    found))


;;;; cljfmt's defaults

(defconst replique-cljfmt--default-indents-edn
  "{alt!            [[:block 0]]
 alt!!           [[:block 0]]
 are             [[:block 2]]
 as->            [[:block 2]]
 binding         [[:block 1]]
 bound-fn        [[:inner 0]]
 case            [[:block 1]]
 catch           [[:block 2]]
 comment         [[:block 0]]
 cond            [[:block 0]]
 condp           [[:block 2]]
 cond->          [[:block 1]]
 cond->>         [[:block 1]]
 def             [[:inner 0]]
 defmacro        [[:inner 0]]
 defmethod       [[:inner 0]]
 defmulti        [[:inner 0]]
 defn            [[:inner 0]]
 defn-           [[:inner 0]]
 defonce         [[:inner 0]]
 defprotocol     [[:block 1] [:inner 1]]
 defrecord       [[:block 2] [:inner 1]]
 defstruct       [[:block 1]]
 deftest         [[:inner 0]]
 deftype         [[:block 2] [:inner 1]]
 delay           [[:block 0]]
 do              [[:block 0]]
 doseq           [[:block 1]]
 dotimes         [[:block 1]]
 doto            [[:block 1]]
 extend          [[:block 1]]
 extend-protocol [[:block 1] [:inner 1]]
 extend-type     [[:block 1] [:inner 1]]
 fdef            [[:inner 0]]
 finally         [[:block 0]]
 fn              [[:inner 0]]
 for             [[:block 1]]
 future          [[:block 0]]
 go              [[:block 0]]
 go-loop         [[:block 1]]
 if              [[:block 1]]
 if-let          [[:block 1]]
 if-not          [[:block 1]]
 if-some         [[:block 1]]
 let             [[:block 1]]
 let*            [[:block 1]]
 letfn           [[:block 1] [:inner 2 0]]
 locking         [[:block 1]]
 loop            [[:block 1]]
 match           [[:block 1]]
 ns              [[:block 1]]
 proxy           [[:block 2] [:inner 1]]
 reify           [[:inner 0] [:inner 1]]
 struct-map      [[:block 1]]
 testing         [[:block 1]]
 thread          [[:block 0]]
 try             [[:block 0]]
 use-fixtures    [[:inner 0]]
 when            [[:block 1]]
 when-first      [[:block 1]]
 when-let        [[:block 1]]
 when-not        [[:block 1]]
 when-some       [[:block 1]]
 while           [[:block 1]]
 with-local-vars [[:block 1]]
 with-open       [[:block 1]]
 with-out-str    [[:block 0]]
 with-precision  [[:block 1]]
 with-redefs     [[:block 1]]

 ANY        [[:inner 0]]
 DELETE     [[:inner 0]]
 GET        [[:inner 0]]
 HEAD       [[:inner 0]]
 OPTIONS    [[:inner 0]]
 PATCH      [[:inner 0]]
 POST       [[:inner 0]]
 PUT        [[:inner 0]]
 context    [[:inner 0]]
 defroutes  [[:inner 0]]
 let-routes [[:block 1]]
 rfn        [[:inner 0]]

 #\"^def(?!ault)(?!late)(?!er)\" [[:inner 0]]
 #\"^with-\"                     [[:inner 0]]}"
  "The default indents of cljfmt: its clojure.clj, compojure.clj, fuzzy.clj.
As written in https://github.com/weavejester/cljfmt/tree/master/cljfmt/resources/cljfmt/indents")

(defvar replique-cljfmt--default-indents nil
  "`replique-cljfmt--default-indents-edn', read.")

(defun replique-cljfmt--default-indents ()
  "The default indents of cljfmt, as an EDN map."
  (or replique-cljfmt--default-indents
      (setq replique-cljfmt--default-indents
            (replique-cljfmt-read-edn replique-cljfmt--default-indents-edn))))


;;;; Rules
;;
;; A rule is (KEY SPECS SORT), KEY being one of
;;
;;   (name NAME)            a symbol with no namespace: any `NAME'
;;   (qualified NS NAME)    a qualified symbol, matched against the name a
;;                          symbol resolves to
;;   (regex REGEX)          found in the name, whatever the namespace
;;   (parts NS NAME)        a vector key: each of NS and NAME a string or
;;                          a regex, NS matched against the namespace
;;
;; and SPECS the list of (:block N), (:inner DEPTH) or (:inner DEPTH INDEX)
;; it says.  The rules are kept in the order cljfmt tries them in - the
;; deepest :inner first, then qualified symbols, then plain ones, then
;; regexes, then by how the key is written - and the first rule that has
;; anything to say about a line says where it goes.

(defun replique-cljfmt--key (key)
  "The rule key the EDN KEY is, or nil for one that is not a key."
  (pcase key
    (`(:symbol . ,text)
     (let ((parts (replique-parse-name-parts text)))
       (if (car parts)
           (list 'qualified (car parts) (cdr parts))
         (list 'name text))))
    (`(:regex . ,source) (list 'regex (replique-cljfmt-regex source)))
    ((pred vectorp)
     (when (= 2 (length key))
       (let ((part (lambda (k)
                     (pcase k
                       (`(:symbol . ,text) text)
                       (`(:regex . ,source) (replique-cljfmt-regex source))
                       (_ (error "Not a key part: %S" k))))))
         (list 'parts (funcall part (aref key 0)) (funcall part (aref key 1))))))
    ;; What `replique-clojure-semantic-indent-rules' writes
    ((pred stringp) (replique-cljfmt--key (cons :symbol key)))))

(defun replique-cljfmt--key-text (key)
  "How the EDN KEY is written, which is what ties between rules are broken by."
  (pcase key
    (`(:symbol . ,text) text)
    (`(:regex . ,source) source)
    ((pred vectorp)
     (concat "[" (mapconcat #'replique-cljfmt--key-text key " ") "]"))
    ((pred stringp) key)
    (_ "")))

(defun replique-cljfmt--specs (specs)
  "The rule specs the EDN SPECS say, as lists."
  (mapcar (lambda (spec)
            (let ((spec (append spec nil)))
              (pcase spec
                ((and `(:block ,(pred integerp)) spec) spec)
                ((and `(:inner ,(pred integerp)) spec) spec)
                ((and `(:inner ,(pred integerp) ,(pred integerp)) spec) spec)
                (_ (list :default)))))
          (append specs nil)))

(defun replique-cljfmt--sort-key (key specs text)
  "What a rule of KEY and SPECS, written as TEXT, is sorted by."
  (list (- (apply #'max 0 (mapcar (lambda (spec)
                                    (if (eq :inner (car spec)) (nth 1 spec) 0))
                                  specs)))
        (pcase (car key) ('parts -1) ('qualified 0) ('name 1) ('regex 2))
        text))

(defun replique-cljfmt--sort-less-p (a b)
  "Return non-nil when the sort key A comes before the sort key B."
  (cond ((/= (nth 0 a) (nth 0 b)) (< (nth 0 a) (nth 0 b)))
        ((/= (nth 1 a) (nth 1 b)) (< (nth 1 a) (nth 1 b)))
        (t (string< (nth 2 a) (nth 2 b)))))

(defun replique-cljfmt--rules (indents errors)
  "The rules the EDN map INDENTS says, sorted.
What cannot be followed is said in ERRORS, a cons whose car is pushed to."
  (let ((rules nil))
    (dolist (entry (cdr indents))
      (condition-case err
          (let ((key (replique-cljfmt--key (car entry)))
                (specs (replique-cljfmt--specs (cdr entry))))
            (when (and key specs)
              (push (list key specs
                          (replique-cljfmt--sort-key
                           key specs (replique-cljfmt--key-text (car entry))))
                    rules)))
        (error (push (format "%s: %s" (replique-cljfmt--key-text (car entry))
                             (error-message-string err))
                     (car errors)))))
    (sort rules (lambda (a b) (replique-cljfmt--sort-less-p (nth 2 a) (nth 2 b))))))

(defun replique-cljfmt--extra-indents (rules)
  "The EDN map the rules of `replique-clojure-semantic-indent-rules' RULES say."
  (cons :map (mapcar (lambda (rule)
                       (cons (cons :symbol (car rule))
                             (vconcat (mapcar #'vconcat (cdr rule)))))
                     rules)))


;;;; Finding the configuration

(defconst replique-cljfmt--config-files
  '(".cljfmt.edn" ".cljfmt.clj" "cljfmt.edn" "cljfmt.clj")
  "The names cljfmt looks for its configuration under, in that order.")

(defun replique-cljfmt--project-root (directory)
  "The nearest directory at or above DIRECTORY that configures formatting."
  (when directory
    (locate-dominating-file
     directory
     (lambda (dir)
       (seq-some (lambda (name) (file-exists-p (expand-file-name name dir)))
                 (cons ".lsp/config.edn" replique-cljfmt--config-files))))))

(defun replique-cljfmt--global-lsp-config ()
  "Where clojure-lsp's global settings are written."
  (expand-file-name "clojure-lsp/config.edn"
                    (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(defun replique-cljfmt--files (root)
  "The files the configuration of the project at ROOT is read from.
A cons of the clojure-lsp settings files and the cljfmt file, or nil for none."
  (let* ((lsp-files (list (replique-cljfmt--global-lsp-config)
                          (and root (expand-file-name ".lsp/config.edn" root))))
         (settings (seq-reduce (lambda (acc file)
                                 (replique-cljfmt--merge
                                  acc (and file (replique-cljfmt--read-file file)) t))
                               lsp-files nil))
         (path (replique-cljfmt--get settings :cljfmt-config-path)))
    (list (delq nil lsp-files)
          settings
          (when root
            (if (stringp path)
                (expand-file-name path root)
              (seq-find #'file-exists-p
                        (mapcar (lambda (name) (expand-file-name name root))
                                replique-cljfmt--config-files)))))))

(defun replique-cljfmt--mtime (file)
  "When FILE was last written, or nil where it does not exist."
  (and file (file-attribute-modification-time (file-attributes file))))


;;;; The configuration

(defvar replique-cljfmt--cache (make-hash-table :test #'equal)
  "The configurations read, by project root and per buffer rules.
Each a vector of the configuration, the files it was read from with when
each was written, and when they were last looked at.")

(defconst replique-cljfmt--recheck-seconds 2
  "How long a configuration is followed before its files are looked at again.")

(defun replique-cljfmt--build (root extra)
  "Read the configuration of the project at ROOT, with the rules EXTRA over it.
Return a cons of the configuration and the files read."
  (let* ((files (and root replique-cljfmt-read-project-config
                     (replique-cljfmt--files root)))
         (settings (nth 1 files))
         (file (nth 2 files))
         (user (replique-cljfmt--merge
                (replique-cljfmt--get settings :cljfmt)
                (and file (replique-cljfmt--read-file file))
                t))
         (indents (replique-cljfmt--merge
                   (replique-cljfmt--merge
                    (replique-cljfmt--merge (replique-cljfmt--default-indents)
                                            (replique-cljfmt--get user :indents))
                    (replique-cljfmt--get user :extra-indents))
                   (replique-cljfmt--extra-indents extra)))
         (errors (list nil))
         (rules (replique-cljfmt--rules indents errors)))
    (when (car errors)
      (message "replique: cljfmt rules ignored - %s"
               (string-join (nreverse (car errors)) "; ")))
    (cons (list :rules rules
                :options (cdr user)
                :file file
                :qualified (seq-some (lambda (rule)
                                       (memq (car (car rule)) '(qualified parts)))
                                     rules))
          (append (car files) (and file (list file))))))

(defun replique-cljfmt-config (directory &optional extra)
  "The cljfmt configuration for a file in DIRECTORY, EXTRA rules over it.

A plist: `:rules' the indent rules in the order they are tried, `:options'
the rest of what the configuration says as an alist of keywords, `:file'
the cljfmt file read, and `:qualified' whether any rule needs to know what
a symbol resolves to.

EXTRA is `replique-clojure-semantic-indent-rules' as a buffer has it.  The
configuration is read once and read again when one of its files changes."
  (let* ((root (and replique-cljfmt-read-project-config
                    (replique-cljfmt--project-root directory)))
         (key (list root extra replique-cljfmt-read-project-config))
         (entry (gethash key replique-cljfmt--cache))
         (now (float-time)))
    (if (and entry
             (or (< (- now (aref entry 2)) replique-cljfmt--recheck-seconds)
                 (and (equal (aref entry 1)
                             (mapcar (lambda (f) (cons f (replique-cljfmt--mtime f)))
                                     (mapcar #'car (aref entry 1))))
                      (aset entry 2 now))))
        (aref entry 0)
      (let ((built (replique-cljfmt--build root extra)))
        (puthash key
                 (vector (car built)
                         (mapcar (lambda (f) (cons f (replique-cljfmt--mtime f)))
                                 (cdr built))
                         now)
                 replique-cljfmt--cache)
        (car built)))))

(defun replique-cljfmt-forget ()
  "Forget every cljfmt configuration read, so that each is read again."
  (interactive)
  (clrhash replique-cljfmt--cache)
  (setq replique-cljfmt--default-indents nil))

(defun replique-cljfmt-option (config key &optional default)
  "The value the option KEY has in CONFIG, or DEFAULT where it is not set.
KEY is a keyword, `:indent-line-comments?' say, and a boolean option comes
back as t or nil."
  (let ((entry (assq key (plist-get config :options))))
    (if (null entry)
        default
      (let ((value (cdr entry)))
        (cond ((eq value :false) nil)
              ((eq value t) t)
              (t value))))))

(defun replique-cljfmt-string-map (config key)
  "The EDN map option KEY of CONFIG, as an alist of strings."
  (let ((map (cdr (assq key (plist-get config :options)))))
    (delq nil
          (mapcar (lambda (entry)
                    (let ((k (pcase (car entry)
                               (`(:symbol . ,text) text)
                               ((pred stringp) (car entry))
                               ((pred keywordp) (substring (symbol-name (car entry)) 1))))
                          (v (pcase (cdr entry)
                               (`(:symbol . ,text) text)
                               ((pred stringp) (cdr entry)))))
                      (and k v (cons k v))))
                  (cdr-safe map)))))

(provide 'replique-cljfmt)

;;; replique-cljfmt.el ends here
