;;; replique-pprint-test.el --- Tests for laying out data  -*- lexical-binding: t; -*-

;;; Commentary:

;; Laying a printed value out over lines.  These need no process: what they
;; check is text in and text out.
;;
;; Several of them are written against what master's printer does with the
;; same input, since that is what this replaces - the tests named for a
;; column budget, for a token being written as it was written, and for a map
;; that fits are the three places master gets it wrong.

;;; Code:

(require 'ert)
(require 'replique-test)
(require 'replique-pprint)

(defun replique-pprint-test--pp (text &optional width)
  "Return TEXT laid out to fit WIDTH columns, or skip without the grammar."
  (replique-pprint-string text width))

(defun replique-pprint-test--command (text width)
  "Return TEXT after `replique-pprint' ran at | in it, to fit WIDTH."
  (with-temp-buffer
    (replique-clojure-mode)
    (insert text)
    (goto-char (point-min))
    (unless (search-forward "|" nil t)
      (error "The text says nowhere to run: %s" text))
    (delete-region (match-beginning 0) (match-end 0))
    (goto-char (match-beginning 0))
    (let ((replique-pprint-width width))
      (replique-pprint))
    (buffer-substring-no-properties (point-min) (point-max))))

;;; The budget

(ert-deftest replique-pprint-test-what-fits-is-left-on-one-line ()
  ;; master breaks a map at every entry whatever its width, so this one comes
  ;; out over two lines there
  (should (equal "{:a 1 :b 2}" (replique-pprint-test--pp "{:a 1 :b 2}" 20)))
  (should (equal "[1 2 3]" (replique-pprint-test--pp "[1 2 3]" 20)))
  (should (equal "{:a 1 :b 2}" (replique-pprint-test--pp "{:a 1\n :b 2}" 20))))

(ert-deftest replique-pprint-test-what-does-not-fit-is-broken ()
  (should (equal "{:aaaa 1\n :bbbb 2}"
                 (replique-pprint-test--pp "{:aaaa 1 :bbbb 2}" 10))))

(ert-deftest replique-pprint-test-the-budget-is-the-column-not-the-form ()
  ;; master measures each collection from its own opening bracket, so the
  ;; innermost of these is eleven columns wide against a budget of twelve
  ;; and the whole thing stays on one line there, nineteen columns wide
  (should (equal "[[[[[1 2 3 4\n     5]]]]]"
                 (replique-pprint-test--pp "[[[[[1 2 3 4 5]]]]]" 12)))
  (should (equal "[[[[[1 2 3 4 5]]]]]"
                 (replique-pprint-test--pp "[[[[[1 2 3 4 5]]]]]" 19))))

(ert-deftest replique-pprint-test-a-form-is-laid-out-from-where-it-starts ()
  ;; the same map, written three columns in, has three fewer to spend
  (should (equal "   {:aaa 1\n    :bbb 2}"
                 (replique-pprint-test--command "   {:aaa 1 :bbb |2}" 16)))
  (should (equal "   {:aaa 1 :bbb 2}"
                 (replique-pprint-test--command "   {:aaa 1 :bbb |2}" 18))))

;;; Tokens

(ert-deftest replique-pprint-test-a-token-is-written-as-it-was-written ()
  ;; master indents the lines of a broken up form one at a time, walking
  ;; over whatever characters are in the range - so it writes spaces into
  ;; the middle of this string and the value stops being the value
  (should (equal "{:k [\"aa\nbb\"\n     11111\n     22222]}"
                 (replique-pprint-test--pp "{:k [\"aa\nbb\" 11111 22222]}" 12)))
  (should (equal "\"aa\nbb\"" (replique-pprint-test--pp "\"aa\nbb\"" 3)))
  ;; and it is never measured as though it were one line: what a width for
  ;; it would say is a number that is not a column, and every decision made
  ;; after it on that line would be made against it
  (should (equal "[\"aa\nbb\"\n 1]" (replique-pprint-test--pp "[\"aa\nbb\" 1]" 20))))

(ert-deftest replique-pprint-test-a-token-that-does-not-fit-is-written-anyway ()
  (should (equal "[aaaaaaaaaa\n bbbbbbbbbb]"
                 (replique-pprint-test--pp "[aaaaaaaaaa bbbbbbbbbb]" 4))))

;;; How a break is chosen

(ert-deftest replique-pprint-test-a-map-breaks-one-entry-to-a-line ()
  (should (equal "{:a 1\n :b 2\n :c 3}"
                 (replique-pprint-test--pp "{:a 1 :b 2 :c 3}" 8)))
  ;; and one to a line however many would have fit on it: a map filled the
  ;; way a vector is is a map whose entries have to be counted out to be read
  (should (equal "{:a 1\n :b 2\n :c 3}"
                 (replique-pprint-test--pp "{:a 1 :b 2 :c 3}" 12))))

(ert-deftest replique-pprint-test-everything-else-fills ()
  (should (equal "[1 2 3\n 4 5 6\n 7]" (replique-pprint-test--pp "[1 2 3 4 5 6 7]" 7)))
  (should (equal "#{1 2 3\n  4 5 6}" (replique-pprint-test--pp "#{1 2 3 4 5 6}" 8)))
  (should (equal "(1 2 3\n 4 5 6)" (replique-pprint-test--pp "(1 2 3 4 5 6)" 7))))

(ert-deftest replique-pprint-test-an-element-of-several-lines-gets-its-own ()
  ;; without this the 1 and the 2 carry on after the closing bracket of the
  ;; vector above them, and read as part of it
  (should (equal "[[1 2 3 4 5\n  6]\n 1 2]"
                 (replique-pprint-test--pp "[[1 2 3 4 5 6] 1 2]" 12))))

(ert-deftest replique-pprint-test-a-value-hangs-after-its-key ()
  ;; under the key is where the next key goes
  (should (equal "{:a [1 2 3\n     4 5]\n :b 2}"
                 (replique-pprint-test--pp "{:a [1 2 3 4 5] :b 2}" 11)))
  ;; and what it costs is a map of maps, where each key hangs the next one
  ;; further in than the last - past the width once there are enough of
  ;; them.  The exchange is deliberate, and the commentary says what it
  ;; comes to at a width somebody would actually set
  (should (equal "{:a {:b {:c [1\n             2]}}}"
                 (replique-pprint-test--pp "{:a {:b {:c [1 2]}}}" 12))))

;;; Reader macros

(ert-deftest replique-pprint-test-a-reader-macro-keeps-what-it-is-applied-to ()
  (should (equal "#foo.Bar{:a 1\n         :b 2}"
                 (replique-pprint-test--pp "#foo.Bar{:a 1 :b 2}" 14)))
  (should (equal "#:x{:a 1\n    :b 2}" (replique-pprint-test--pp "#:x{:a 1 :b 2}" 8)))
  (should (equal "#?(:clj 1\n   :cljs 2)"
                 (replique-pprint-test--pp "#?(:clj 1 :cljs 2)" 10)))
  (should (equal "'(1 2)" (replique-pprint-test--pp "'(1 2)" 20)))
  (should (equal "@a" (replique-pprint-test--pp "@a" 20)))
  (should (equal "#'a" (replique-pprint-test--pp "#'a" 20)))
  (should (equal "[1 #_2 3]" (replique-pprint-test--pp "[1 #_2 3]" 20))))

(ert-deftest replique-pprint-test-a-space-is-kept-where-there-was-one ()
  ;; removing it would push the two together into a third token
  (should (equal "^:m x" (replique-pprint-test--pp "^:m x" 20)))
  (should (equal "^:m x" (replique-pprint-test--pp "^:m\n   x" 20)))
  (should (equal "#inst \"2020\"" (replique-pprint-test--pp "#inst \"2020\"" 20)))
  ;; and none is added where there was none
  (should (equal "#foo{:a 1}" (replique-pprint-test--pp "#foo{:a 1}" 20))))

;;; What is refused

(ert-deftest replique-pprint-test-a-comment-is-refused-not-deleted ()
  ;; master deletes it, which loses what was written without saying so
  (should-error (replique-pprint-test--pp "{:a 1 ;; why\n :b 2}") :type 'user-error)
  (should-error (replique-pprint-test--pp ";; why\n{:a 1}") :type 'user-error))

(ert-deftest replique-pprint-test-what-did-not-parse-is-refused ()
  (should-error (replique-pprint-test--pp "[1 2") :type 'user-error)
  (should-error (replique-pprint-test--pp "{:a 1 :b}") :type 'user-error)
  (should-error (replique-pprint-test--pp "'") :type 'user-error))

;;; Nothing, and more than one thing

(ert-deftest replique-pprint-test-nothing-lays-out-as-nothing ()
  (should (equal "" (replique-pprint-test--pp "")))
  (should (equal "" (replique-pprint-test--pp "   \n  ")))
  (should (equal "()" (replique-pprint-test--pp "(  )" 2)))
  (should (equal "[]" (replique-pprint-test--pp "[]" 1)))
  (should (equal "{}" (replique-pprint-test--pp "{}" 1)))
  (should (equal "#{}" (replique-pprint-test--pp "#{}" 1))))

(ert-deftest replique-pprint-test-several-forms-come-back-one-to-a-line ()
  (should (equal "{:a 1}\n{:b 2}" (replique-pprint-test--pp "{:a 1} {:b 2}"))))

;;; Laying out what is already laid out

(ert-deftest replique-pprint-test-laying-out-twice-is-laying-out-once ()
  ;; master grows the string in the last of these by four spaces a time
  (dolist (text '("{:a 1 :b 2 :c 3}"
                  "[1 2 3 4 5 6 7 8 9 10 11 12]"
                  "{:a {:b {:c [1 2 3 4 5]}}}"
                  "[{:name \"aa\" :id 1} {:name \"bb\" :id 2}]"
                  "#foo.Bar{:a 1 :b 2}"
                  "#:x{:a [1 2 3 4 5 6] :b 2}"
                  "#?(:clj 1 :cljs 2)"
                  "^:m [1 2 3 4 5 6 7 8]"
                  "{:k [\"aa\nbb\" 11111 22222]}"))
    (dolist (width '(4 12 40))
      (let* ((once (replique-pprint-test--pp text width))
             (twice (replique-pprint-string once width)))
        (should (equal once twice))))))

;;; The command

(ert-deftest replique-pprint-test-the-command-lays-out-the-form-point-is-in ()
  (should (equal "(a)\n{:aa 1\n :bb 2}"
                 (replique-pprint-test--command "(a)\n{:aa 1 :b|b 2}" 8)))
  ;; and leaves the form it is not in alone
  (should (equal "{:aa 1 :bb 2}\n{:cc 1\n :dd 2}"
                 (replique-pprint-test--command
                  "{:aa 1 :bb 2}\n{:cc 1 :d|d 2}" 8))))

(ert-deftest replique-pprint-test-the-command-lays-out-the-form-before-point ()
  ;; which is where point is at the prompt of a repl, after what was printed
  (should (equal "{:aa 1\n :bb 2}\n"
                 (replique-pprint-test--command "{:aa 1 :bb 2}\n|" 8))))

(ert-deftest replique-pprint-test-a-comment-behind-point-is-read-past ()
  ;; the way `replique-eval-last-sexp' reads past one, so that the last
  ;; line of a file being a note does not stop this
  (should (equal "{:aa 1\n :bb 2}\n;; a note\n"
                 (replique-pprint-test--command "{:aa 1 :bb 2}\n;; a note\n|" 8)))
  (should (equal "{:aa 1\n :bb 2}\n;; one\n;; two\n"
                 (replique-pprint-test--command
                  "{:aa 1 :bb 2}\n;; one\n;; two\n|" 8)))
  ;; but a comment point is in is one point was put on
  (should-error (replique-pprint-test--command "{:aa 1 :bb 2}\n;; a no|te\n" 8)
                :type 'user-error)
  ;; and nothing behind a comment is still nothing
  (should-error (replique-pprint-test--command ";; a note\n|" 8) :type 'user-error))

(ert-deftest replique-pprint-test-the-command-puts-it-back-in-one-undo ()
  (with-temp-buffer
    (replique-clojure-mode)
    (insert "{:aa 1 :bb 2}")
    (goto-char 3)
    (setq buffer-undo-list nil)
    (let ((replique-pprint-width 8))
      (replique-pprint))
    (should (equal "{:aa 1\n :bb 2}" (buffer-string)))
    (primitive-undo 1 buffer-undo-list)
    (should (equal "{:aa 1 :bb 2}" (buffer-string)))))

(ert-deftest replique-pprint-test-the-command-changes-nothing-it-need-not ()
  (with-temp-buffer
    (replique-clojure-mode)
    (insert "{:aa 1 :bb 2}")
    (set-buffer-modified-p nil)
    (goto-char 3)
    (let ((replique-pprint-width 80))
      (replique-pprint))
    (should-not (buffer-modified-p))))

(ert-deftest replique-pprint-test-the-command-needs-a-form ()
  ;; A form and nothing else.  The text is read where it is asked about, so
  ;; there is no parse to be missing and no mode a buffer has to be in -
  ;; which is what makes this work on a buffer that is just holding what
  ;; something printed
  (with-temp-buffer
    (insert "{:aa 1 :bb 2}")
    (goto-char 3)
    (let ((replique-pprint-width 10))
      (replique-pprint))
    (should (equal "{:aa 1\n :bb 2}" (buffer-string))))
  (with-temp-buffer
    (insert "   ")
    (goto-char (point-min))
    (should-error (replique-pprint) :type 'user-error)))

(ert-deftest replique-pprint-test-a-deeply-nested-value-is-laid-out ()
  ;; Writing a form out recurses a frame or so a level, so how deeply a value
  ;; is nested is bounded by the stack rather than by anything here.  Where
  ;; the bound is is worth pinning: master manages a value nested three
  ;; hundred deep, and a change that spent one more frame a level would take
  ;; this under that without anything else noticing.
  ;;
  ;; Only when compiled.  Interpreted, every level costs several times the
  ;; frames it costs compiled, and what is being pinned here is how many
  ;; levels fit in a stack, not how elisp was loaded
  (skip-unless (compiled-function-p (symbol-function 'replique-pprint--emit)))
  (let* ((depth 300)
         (text (concat (make-string depth ?\[) "1 2 3" (make-string depth ?\])))
         ;; a width every one of those levels is past, so that the deep way
         ;; through is the one taken
         (once (replique-pprint-test--pp text 10)))
    (should (stringp once))
    (should (equal once (replique-pprint-string once 10)))))

(ert-deftest replique-pprint-test-a-value-the-printer-cut-short-is-laid-out ()
  "Which is the value that most needs it.  The repl prints under
`*print-length*' and `*print-level*' - it says so in the `print-length'
and `print-level' of every prompt - so a value big enough to be worth
laying out is a value with `...' or `#' written into it, and neither of
those reads as Clojure."
  (should (equal (concat "{:e \"e\"\n"
                         " :f \"ffffff\"\n"
                         " :ggggg {:e 33}\n"
                         " ...}")
                 (replique-pprint-test--pp "{:e \"e\" :f \"ffffff\" :ggggg {:e 33} ...}" 20)))
  (should (equal (concat "#com.stuartsierra.component.SystemMap{:e \"e\"\n"
                         "                                      :f \"ffffff\"\n"
                         "                                      ...}")
                 (replique-pprint-test--pp
                  "#com.stuartsierra.component.SystemMap{:e \"e\" :f \"ffffff\" ...}" 50)))
  (should (equal "{:a #\n :b 2}" (replique-pprint-test--pp "{:a #, :b 2}" 8)))
  (should (equal "[# #]" (replique-pprint-test--pp "[# #]" 20)))
  (should (equal "{:a {:b #}\n ...}" (replique-pprint-test--pp "{:a {:b #} ...}" 10))))

(ert-deftest replique-pprint-test-a-map-that-is-not-data-is-still-refused ()
  "The elisions are read because the printer writes them, not because a map
with a key and no value has become data.  `...' in the middle of one is a
map somebody wrote wrong, and so is a key with nothing after it."
  (should-error (replique-pprint-test--pp "{:a 1 :b}" 20) :type 'user-error)
  (should-error (replique-pprint-test--pp "{:a 1 ... :b 2}" 20) :type 'user-error)
  (should-error (replique-pprint-test--pp "{:a 1" 20) :type 'user-error))

(ert-deftest replique-pprint-test-what-a-real-repl-printed-is-laid-out ()
  "End to end: the printer writes the elisions, the reader reads them and
the layout writes them back.  What an elision looks like is the printer's
to decide, and a test that writes one by hand goes on passing after the
printer stops writing that one - so this asks a repl for a value it has
to cut short, and lays out what actually came back."
  (replique-test-with-repl repl
    (replique-test-eval repl "(set! *print-length* 3)")
    (replique-test-eval repl "(set! *print-level* 2)")
    (let ((printed (replique-test-eval
                    repl "(zipmap [:aaaa :bbbb :cccc :dddd] (repeat {:x {:y 1}}))")))
      ;; The value really was cut short both ways, which is what the rest of
      ;; this is about
      (should (string-match-p "\\.\\.\\." printed))
      (should (string-match-p "#" printed)))
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      ;; The nearest `{' going back is the one the value opens with: the
      ;; prompt under it holds none, and the form that was typed is further
      ;; back than the value it printed
      (should (search-backward "{" nil t))
      (let ((start (point))
            (replique-pprint-width 20))
        (replique-pprint)
        (let ((laid-out (buffer-substring-no-properties start (point-max))))
          (should (string-match-p "\n" laid-out))
          (should (string-match-p "\\.\\.\\." laid-out)))))))


;;; In a repl buffer, where the prompt is not a form

(defun replique-pprint-test--transcript (text width)
  "Return TEXT after `replique-pprint' ran at | in it, to fit WIDTH.

TEXT is a repl transcript, and every `ns=> ' in it is written the way a
repl writes a prompt - see `replique-prompt-text'.  Written here rather
than printed by a repl so that these need no process; that a real prompt
is written that way is `replique-pprint-test-a-real-prompt-says-it-is-one'."
  (with-temp-buffer
    (replique-repl-mode)
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^[^ \n]*=> " nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         replique-prompt-property t))
    (goto-char (point-min))
    (unless (search-forward "|" nil t)
      (error "The text says nowhere to run: %s" text))
    (delete-region (match-beginning 0) (match-end 0))
    (goto-char (match-beginning 0))
    (let ((replique-pprint-width width))
      (replique-pprint))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest replique-pprint-test-at-the-prompt-the-value-above-is-laid-out ()
  "Which is where point is the moment a value is printed, so it is the only
place the command is ever reached for in a repl buffer.  `user=>' reads as
a symbol, so it was the form before point - and laying out a symbol writes
the symbol back, so the command did nothing and said nothing."
  (should (equal (concat "user=> (f)\n"
                         "{:aaa 1\n"
                         " :bbb 2}\n"
                         "user=> ")
                 (replique-pprint-test--transcript
                  "user=> (f)\n{:aaa 1 :bbb 2}\nuser=> |" 10))))

(ert-deftest replique-pprint-test-on-the-prompt-is-the-same-place ()
  "Point on the prompt is point where the repl is waiting, and what somebody
means there is the value above it."
  (should (equal (concat "user=> (f)\n"
                         "{:aaa 1\n"
                         " :bbb 2}\n"
                         "user=> ")
                 (replique-pprint-test--transcript
                  "user=> (f)\n{:aaa 1 :bbb 2}\nus|er=> " 10))))

(ert-deftest replique-pprint-test-prompts-standing-together-are-walked-past ()
  "A directive moves the repl without evaluating anything, so nothing
consumes the prompt that was standing and a second one is written under
it.  One skip would stop on the first of them."
  (should (equal (concat "user=> (f)\n"
                         "{:aaa 1\n"
                         " :bbb 2}\n"
                         "user=> \n"
                         "other=> ")
                 (replique-pprint-test--transcript
                  "user=> (f)\n{:aaa 1 :bbb 2}\nuser=> \nother=> |" 10))))

(ert-deftest replique-pprint-test-a-prompt-with-nothing-above-it-is-refused ()
  "A repl that has printed nothing has nothing to lay out, and the prompt is
not it."
  (should-error (replique-pprint-test--transcript "user=> |" 10) :type 'user-error)
  ;; And what a value of one token is, is said rather than done silently:
  ;; doing nothing is what could not be told from not running
  (let ((err (should-error (replique-pprint-test--transcript
                            "user=> (+ 1 2)\n3\nuser=> |" 10)
                           :type 'user-error)))
    (should (string-match-p "one token" (cadr err)))))

(ert-deftest replique-pprint-test-what-was-typed-at-the-prompt-is-still-laid-out ()
  "The prompt is skipped, not the line it is on.  A form written at the
prompt is written by somebody, and laying it out is what the command is
for everywhere else."
  (should (equal "user=> {:aaa 1\n        :bbb 2}"
                 (replique-pprint-test--transcript "user=> {:aaa 1 :bbb 2}|" 10))))

(ert-deftest replique-pprint-test-a-real-prompt-says-it-is-one ()
  "The transcripts above are written by hand, and a property applied by hand
in every test is a property that can quietly stop being applied for real."
  (replique-test-with-repl repl
    (replique-test-eval repl "(zipmap [:aaaa :bbbb :cccc] (repeat :x))")
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (should (replique-prompt-at-p (1- (point-max))))
      (let ((replique-pprint-width 20))
        (replique-pprint))
      ;; Laid out the value above, and left point where it was
      (should (= (point) (point-max)))
      (should (string-match-p "\n :bbbb" (buffer-string))))))

(provide 'replique-pprint-test)

;;; replique-pprint-test.el ends here
