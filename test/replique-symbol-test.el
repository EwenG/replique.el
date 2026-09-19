;;; replique-symbol-test.el --- Tests for what a name is  -*- lexical-binding: t; -*-

;;; Commentary:

;; What one name is, asked two ways.  Which call point is inside and which
;; argument of it point is at are read here and can be tested without a
;; process; what that call turns out to be comes from one, so the tests that
;; check an arglist need one.
;;
;; The arglists arrive as strings, so facing the argument point is at means
;; finding it in one - and a parameter vector holds maps and vectors of its
;; own, which is what makes that more than splitting on a space.  That part
;; is tested on its own, since it is the part with a rule in it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-name)
(require 'replique-symbol)

(defmacro replique-symbol-test--at (text &rest body)
  "Run BODY in a Clojure buffer holding TEXT, with point where its | was."
  (declare (indent 1))
  `(progn
     (with-temp-buffer
       (replique-clojure-mode)
       (insert ,text)
       (goto-char (point-min))
       (unless (search-forward "|" nil t)
         (error "The text says nowhere to look: %s" ,text))
       (let ((pos (match-beginning 0)))
         (delete-region pos (1+ pos))
         (goto-char pos))
       ,@body)))

(defun replique-symbol-test--name ()
  "Return the whole of the name point is in."
  (when-let* ((bounds (replique-name-at-point)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun replique-symbol-test--call ()
  "Return the call point is inside, as (TEXT . ARGUMENT)."
  (when-let* ((call (replique-name-call-at-point)))
    (cons (plist-get call :text) (plist-get call :argument))))

;;; The whole of a name

(ert-deftest replique-symbol-test-a-name-is-read-whole ()
  "A completion replaces what has been typed so far and stops where point
is; a name being asked about is the whole of the one point is in."
  (should (equal "clojure.string" (replique-symbol-test--at "(f clojure.st|ring)"
                                    (replique-symbol-test--name))))
  (should (equal "map" (replique-symbol-test--at "(ma|p inc x)"
                         (replique-symbol-test--name))))
  (should (equal "map" (replique-symbol-test--at "(map| inc x)"
                         (replique-symbol-test--name))))
  (should (equal "::key" (replique-symbol-test--at "(f ::k|ey)"
                           (replique-symbol-test--name))))
  (should (equal ".length" (replique-symbol-test--at "(.len|gth s)"
                             (replique-symbol-test--name)))))

(ert-deftest replique-symbol-test-a-reader-macro-is-not-part-of-the-name ()
  "Emacs reads a symbol as starting at the macro in front of it, and what
is being asked about is the name."
  (should (equal "clojure.string" (replique-symbol-test--at "(require '|clojure.string)"
                                    (replique-symbol-test--name))))
  (should (equal "map" (replique-symbol-test--at "#'ma|p"
                         (replique-symbol-test--name))))
  ;; and point on the macro is point in front of a name rather than in one,
  ;; which is what a completion reads it as too
  (should-not (replique-symbol-test--at "(require |'clojure.string)"
                (replique-symbol-test--name)))
  (should-not (replique-symbol-test--at "(require |'[clojure.string])"
                (replique-symbol-test--name))))

(ert-deftest replique-symbol-test-a-path-in-a-string-is-read-whole ()
  "Which is what a load names, and the whole of what is written between the
quotes is the path."
  (should (equal "clojure/core" (replique-symbol-test--at "(load \"clo|jure/core\")"
                                  (replique-symbol-test--name)))))

(ert-deftest replique-symbol-test-point-in-no-name-is-no-name ()
  "A completion answers there with the empty region it would write a
candidate into, and an empty region is not a name to look anything up by."
  (should-not (replique-symbol-test--at "(map inc |)" (replique-symbol-test--name)))
  (should-not (replique-symbol-test--at "(f [|] x)" (replique-symbol-test--name)))
  ;; and point at the end of a name is in that name: it is where somebody
  ;; who has just finished writing one leaves point
  (should (equal "inc" (replique-symbol-test--at "(map inc| )"
                         (replique-symbol-test--name)))))

;;; The call point is inside

(ert-deftest replique-symbol-test-the-call-is-the-enclosing-list ()
  "Point is nowhere in particular while the arguments of a call are being
written, and what is worth saying there is what the call takes."
  (should (equal '("map" . 2) (replique-symbol-test--at "(map inc |)"
                                (replique-symbol-test--call))))
  (should (equal '("map" . 1) (replique-symbol-test--at "(map |inc)"
                                (replique-symbol-test--call))))
  (should (equal '("map" . 1) (replique-symbol-test--at "(map in|c xs)"
                                (replique-symbol-test--call))))
  ;; and nothing has to be closed for it: a call being written is a call
  ;; whose closing paren is not there yet
  (should (equal '("map" . 2) (replique-symbol-test--at "(map inc |"
                                (replique-symbol-test--call)))))

(ert-deftest replique-symbol-test-which-argument-is-how-many-end-before-point ()
  "A name point is still inside has not ended, so point is at that one; a
name point sits at the end of is the one being written, and not the next."
  (should (equal '("f" . 1) (replique-symbol-test--at "(f x|)"
                              (replique-symbol-test--call))))
  (should (equal '("f" . 2) (replique-symbol-test--at "(f x |)"
                              (replique-symbol-test--call))))
  (should (equal '("f" . 2) (replique-symbol-test--at "(f x y|)"
                              (replique-symbol-test--call)))))

(ert-deftest replique-symbol-test-nothing-is-said-at-the-head-of-a-call ()
  "Nothing has been written there to be an argument of yet."
  (should-not (replique-symbol-test--at "(ma|p inc)" (replique-symbol-test--call)))
  (should-not (replique-symbol-test--at "(map| inc)" (replique-symbol-test--call)))
  (should-not (replique-symbol-test--at "(| inc)" (replique-symbol-test--call))))

(ert-deftest replique-symbol-test-the-call-is-the-innermost-one ()
  "And a vector inside one is an argument of it rather than a call of its
own."
  (should (equal '("inc" . 1) (replique-symbol-test--at "(map (inc |) xs)"
                                (replique-symbol-test--call))))
  (should (equal '("f" . 1) (replique-symbol-test--at "(f [1 |2])"
                              (replique-symbol-test--call)))))

(ert-deftest replique-symbol-test-a-call-in-a-comment-is-not-one ()
  "What is written there is not a call being written."
  (should-not (replique-symbol-test--at "(f ; a |b\n)" (replique-symbol-test--call))))

(ert-deftest replique-symbol-test-a-call-is-answered-inside-a-string ()
  "What a call takes is worth saying while any of its arguments is being
written, and a string is an argument like the rest of them."
  (should (equal '("f" . 1) (replique-symbol-test--at "(f \"a |b\")"
                              (replique-symbol-test--call)))))

(ert-deftest replique-symbol-test-a-head-that-is-not-a-name-is-nothing-to-ask ()
  "What ((f x) y) calls is not something to ask the process about."
  (should-not (replique-symbol-test--at "((f x) |y)" (replique-symbol-test--call))))

(ert-deftest replique-symbol-test-a-member-call-says-what-it-is-written-on ()
  "The s of (.length s), which is what says which class the method is of."
  (should (equal '(:tag "String")
                 (replique-symbol-test--at "(defn f [^String s] (.length s |))"
                   (let ((call (replique-name-call-at-point)))
                     (list :tag (plist-get (plist-get call :context) :tag))))))
  ;; and a call that is not a member says nothing about what is written
  ;; after it, since nothing is written on
  (should-not (replique-symbol-test--at "(defn f [^String s] (str s |))"
                (plist-get (plist-get (replique-name-call-at-point) :context) :tag))))

(ert-deftest replique-symbol-test-a-local-head-travels-as-a-local ()
  "So that a let that binds f is answered as that local rather than as a var
of the namespace spelled the same."
  (should (equal '((:name "f"))
                 (replique-symbol-test--at "(let [f inc] (f |))"
                   (plist-get (plist-get (replique-name-call-at-point) :context)
                              :locals)))))

;;; Facing the argument point is at

(defun replique-symbol-test--faced-part (text)
  "Return the first part of TEXT faced as the argument point is at, or nil."
  (when-let* ((at (text-property-any 0 (length text) 'face
                                     'eldoc-highlight-function-argument text)))
    (substring-no-properties
     text at (or (next-single-property-change at 'face text) (length text)))))

(defun replique-symbol-test--faced (arglist argument)
  "Return which of ARGLIST is faced when ARGUMENT is, or nil."
  (replique-symbol-test--faced-part (replique-symbol--written arglist argument)))

(ert-deftest replique-symbol-test-the-argument-is-faced-where-there-is-one ()
  "Which is what says how far along a call somebody is."
  (should (equal "f" (replique-symbol-test--faced "[f coll]" 1)))
  (should (equal "coll" (replique-symbol-test--faced "[f coll]" 2)))
  (should-not (replique-symbol-test--faced "[f coll]" 3))
  (should-not (replique-symbol-test--faced "[]" 1)))

(ert-deftest replique-symbol-test-a-rest-argument-takes-everything-after-it ()
  "Which is what the ampersand in front of it says."
  (should (equal "x" (replique-symbol-test--faced "[x & more]" 1)))
  (should (equal "more" (replique-symbol-test--faced "[x & more]" 2)))
  (should (equal "more" (replique-symbol-test--faced "[x & more]" 9))))

(ert-deftest replique-symbol-test-a-destructured-name-is-one-argument ()
  "However many names are written inside it."
  (should (equal "{:keys [a b]}" (replique-symbol-test--faced "[{:keys [a b]} c]" 1)))
  (should (equal "c" (replique-symbol-test--faced "[{:keys [a b]} c]" 2)))
  (should (equal "[a b]" (replique-symbol-test--faced "[[a b] c]" 1))))

(ert-deftest replique-symbol-test-what-a-method-is-called-on-is-not-an-argument ()
  "One is written on the thing it is called on, and the parameters it takes
start after that thing - so the arguments are one behind what is written."
  (should (equal 0 (replique-symbol--argument-of ".substring" 1)))
  (should (equal 1 (replique-symbol--argument-of ".substring" 2)))
  (should (equal 1 (replique-symbol--argument-of "String/.substring" 2)))
  ;; a static one is called on nothing, and a constructor on nothing either
  (should (equal 2 (replique-symbol--argument-of "Integer/parseInt" 2)))
  (should (equal 2 (replique-symbol--argument-of "java.util.Date." 2)))
  (should (equal 2 (replique-symbol--argument-of "map" 2)))
  ;; and nought faces nothing, which is what point at the receiver is
  (should-not (replique-symbol-test--faced "^String [int int]" 0)))

(ert-deftest replique-symbol-test-what-stands-between-two-arguments ()
  "Whatever clojure reads as whitespace, which is what wrote the arglist -
a comma and a newline included, and neither of them is an argument."
  (should (equal "b" (replique-symbol-test--faced "[a, b]" 2)))
  (should (equal "b" (replique-symbol-test--faced "[a\nb]" 2)))
  (should-not (replique-symbol-test--faced "[a\nb]" 3)))

(ert-deftest replique-symbol-test-a-return-type-is-not-an-argument ()
  "A method arrives with its return tagged on the vector, which is the
notation the language has for it and is not something the call takes."
  (should (equal "int" (replique-symbol-test--faced "^String [int int]" 1)))
  (should-not (replique-symbol-test--faced "^String []" 1)))

;;; What is said

(ert-deftest replique-symbol-test-a-name-is-written-with-what-holds-it ()
  "They arrive apart because a completion carries them apart: a name does
not say where it came from, and that is what is worth saying beside it."
  (should (equal "clojure.core/map"
                 (replique-symbol-full-name '(:type "function" :name "map"
                                                    :ns "clojure.core"))))
  (should (equal "java.util.Date"
                 (replique-symbol-full-name '(:type "class" :name "Date"
                                                    :package "java.util"))))
  (should (equal "java.lang.String/length"
                 (replique-symbol-full-name '(:type "method" :name "length"
                                                    :class "java.lang.String"))))
  (should (equal ":my.app/thing"
                 (replique-symbol-full-name '(:type "keyword" :name "thing"
                                                    :ns "my.app"))))
  (should (equal ":thing" (replique-symbol-full-name '(:type "keyword" :name "thing"))))
  (should (equal "x" (replique-symbol-full-name '(:type "local" :name "x")))))

(ert-deftest replique-symbol-test-what-is-said-is-the-name-then-the-doc ()
  "How much of it reaches the echo area is eldoc's to decide, which is
somebody's setting rather than this package's business."
  (should (equal "clojure.core/map: ([f] [f coll])\nReturns a lazy sequence."
                 (substring-no-properties
                  (replique-symbol--said '(:type "function" :name "map" :ns "clojure.core"
                                                 :arglists ["[f]" "[f coll]"]
                                                 :doc "Returns a lazy sequence.")
                                         nil))))
  ;; a field has a type where it has no arglists
  (should (equal "java.lang.Integer/SIZE: ^int"
                 (substring-no-properties
                  (replique-symbol--said '(:type "field" :name "SIZE"
                                                 :class "java.lang.Integer" :tag "int")
                                         nil))))
  ;; and a name with neither is still a name
  (should (equal "x" (substring-no-properties
                      (replique-symbol--said '(:type "local" :name "x") nil)))))

;;; What the process says

(ert-deftest replique-symbol-test-a-var-is-answered-with-its-arglists ()
  "Which is what a call being written is worth being told about."
  (replique-test-process)
  (let ((found (replique-symbol-test--at "(map |)"
                 (replique-symbol--ask (replique-name-context) "map"))))
    (should (equal "function" (plist-get found :type)))
    (should (equal "clojure.core" (plist-get found :ns)))
    (should (equal '("[f]" "[f coll]" "[f c1 c2]" "[f c1 c2 c3]" "[f c1 c2 c3 & colls]")
                   (plist-get found :arglists)))
    (should (string-prefix-p "Returns a lazy sequence" (plist-get found :doc)))))

(ert-deftest replique-symbol-test-a-name-that-means-nothing-is-answered-with-nothing ()
  "Which is half of what somebody writes: a name they have not finished."
  (replique-test-process)
  (should-not (replique-symbol-test--at "(no-such-name-at-all |)"
                (replique-symbol--ask (replique-name-context) "no-such-name-at-all"))))

(ert-deftest replique-symbol-test-a-definition-in-a-jar-is-a-file-and-an-entry ()
  "There is no path to a file inside an archive, so both halves travel and
the entry is read out of the archive into a buffer of its own."
  (replique-test-process)
  (let ((found (replique-symbol-test--at "(map| inc)"
                 (replique-symbol--ask (replique-name-context) "map"))))
    (should (string-suffix-p ".jar" (plist-get found :file)))
    (should (equal "clojure/core.clj" (plist-get found :entry)))
    (let ((buffer (replique-symbol--visit found)))
      (unwind-protect
          (with-current-buffer buffer
            (should buffer-read-only)
            (should (string-match-p "clojure/core.clj" (buffer-file-name)))
            (goto-char (point-min))
            (forward-line (1- (plist-get found :line)))
            (should (string-match-p "defn map" (thing-at-point 'line t))))
        (kill-buffer buffer)))))

(ert-deftest replique-symbol-test-xref-finds-a-definition ()
  "Through xref, which is what makes \\[xref-find-definitions] the way to
one and \\[xref-go-back] the way back."
  (replique-test-process)
  (replique-symbol-test--at "(clojure.string/joi|n)"
    (should (equal 'replique (replique-symbol-xref-backend)))
    (let* ((identifier (xref-backend-identifier-at-point 'replique))
           (found (xref-backend-definitions 'replique identifier)))
      (should (equal "clojure.string/join" (substring-no-properties identifier)))
      (should (equal 1 (length found)))
      (let* ((marker (xref-location-marker (xref-item-location (car found))))
             (buffer (marker-buffer marker)))
        (unwind-protect
            (with-current-buffer buffer
              (should (string-match-p "clojure/string.clj" (buffer-file-name)))
              ;; where the form starts, which is the paren of the defn: the
              ;; column a var records counts from one and emacs counts from
              ;; nought
              (should (equal ?\( (char-after marker)))
              (should (string-match-p
                       "defn .*join"
                       (save-excursion
                         (goto-char marker)
                         (buffer-substring-no-properties (point)
                                                         (line-end-position))))))
          (kill-buffer buffer))))))

(ert-deftest replique-symbol-test-a-string-is-where-what-it-names-is ()
  "A string is a path as often as it is text, and where the thing it names
is is worth asking of any of them - so no call is read here, where a
completion reads one to know what could be written."
  (replique-test-process)
  (let ((found (replique-symbol-test--at "(str \"clojure/version.prope|rties\")"
                 (replique-symbol--ask (replique-name-context)
                                       (replique-symbol-test--name)))))
    (should (equal "path" (plist-get found :type)))
    (should (equal "clojure/version.properties" (plist-get found :name)))
    (should (string-suffix-p ".jar" (plist-get found :file)))
    (should (equal "clojure/version.properties" (plist-get found :entry))))
  ;; and a string that names nothing is a string that is text, which most of
  ;; them are
  (should-not (replique-symbol-test--at "(str \"a message somebody is writ|ing\")"
                (replique-symbol--ask (replique-name-context)
                                      (replique-symbol-test--name)))))

(ert-deftest replique-symbol-test-xref-opens-what-a-string-names ()
  "Which is \\[xref-find-definitions] on a path, and the entry is read out of
the jar into a buffer of its own the way any definition inside one is."
  (replique-test-process)
  (replique-symbol-test--at "(str \"clojure/version.prope|rties\")"
    (let ((found (xref-backend-definitions
                  'replique (xref-backend-identifier-at-point 'replique))))
      (should (equal 1 (length found)))
      (let ((buffer (marker-buffer
                     (xref-location-marker (xref-item-location (car found))))))
        (unwind-protect
            (with-current-buffer buffer
              (should (string-match-p "clojure/version.properties"
                                      (buffer-file-name))))
          (kill-buffer buffer))))))

(ert-deftest replique-symbol-test-a-local-is-where-it-is-bound ()
  "Which this side answers without asking anything: a local is bound by a
form in the buffer, and the process has never seen it."
  (replique-test-process)
  (replique-symbol-test--at "(defn f [x] (let [y 1] (+ x y|)))"
    (let ((found (xref-backend-definitions
                  'replique (xref-backend-identifier-at-point 'replique))))
      (should (equal 1 (length found)))
      (let ((marker (xref-location-marker (xref-item-location (car found)))))
        (should (eq (current-buffer) (marker-buffer marker)))
        (should (equal ?y (char-after marker))))))
  ;; and the nearest binding, which is the one that shadows the rest
  (replique-symbol-test--at "(let [x 1] (let [x 2] x|))"
    (let* ((found (xref-backend-definitions
                   'replique (xref-backend-identifier-at-point 'replique)))
           (marker (xref-location-marker (xref-item-location (car found)))))
      (should (equal 18 (marker-position marker))))))

(ert-deftest replique-symbol-test-a-definition-in-a-directory-is-a-file ()
  "Which is opened as one, where a definition inside a jar is read out of
the archive - an absent entry is what says which of the two it is."
  (replique-test-process)
  (replique-symbol-test--at "(replique.completion/completio|ns)"
    (let* ((found (xref-backend-definitions
                   'replique (xref-backend-identifier-at-point 'replique)))
           (marker (xref-location-marker (xref-item-location (car found))))
           (buffer (marker-buffer marker)))
      (unwind-protect
          (with-current-buffer buffer
            (should (string-suffix-p "src/replique/completion.clj" (buffer-file-name)))
            (should-not buffer-read-only)
            (should (equal ?\( (char-after marker)))
            (should (string-match-p
                     "defn completions"
                     (save-excursion
                       (goto-char marker)
                       (buffer-substring-no-properties (point) (line-end-position))))))
        (kill-buffer buffer)))))

(ert-deftest replique-symbol-test-a-name-with-no-file-is-no-definition ()
  "A class of the runtime is compiled, and the Java it was written in is not
on the classpath to open."
  (replique-test-process)
  (replique-symbol-test--at "(String|/valueOf)"
    (should-not (xref-backend-definitions
                 'replique (xref-backend-identifier-at-point 'replique)))))

(ert-deftest replique-symbol-test-eldoc-says-what-the-call-takes ()
  "Without waiting for it: eldoc hands a callback over, and a non-nil answer
is what says one is coming."
  (replique-test-process)
  (replique-symbol-test--at "(map inc |)"
    (let ((said nil))
      (should (replique-symbol-eldoc (lambda (text &rest _) (setq said text))))
      (should (replique-test-wait-for (lambda () said) 10))
      (should (string-prefix-p "clojure.core/map: " (substring-no-properties said)))
      ;; and the argument point is at is faced in the arglists it is in
      (should (equal "coll" (replique-symbol-test--faced-part said))))))

(ert-deftest replique-symbol-test-eldoc-faces-the-argument-of-a-method ()
  "Which is one behind what is written, since the first thing written after
the name is the thing the method is called on."
  (replique-test-process)
  (cl-flet ((faced (text)
              (replique-symbol-test--at text
                (let ((said nil))
                  (replique-symbol-eldoc (lambda (s &rest _) (setq said s)))
                  (replique-test-wait-for (lambda () said) 10)
                  (replique-symbol-test--faced-part said)))))
    (should-not (faced "(defn f [^String s] (.substring |s))"))
    (should (equal "int" (faced "(defn f [^String s] (.substring s |1))")))
    (should (equal "int" (faced "(defn f [^String s] (String/.substring s 1 |))")))
    ;; a static method is called on nothing, so what is written first is
    ;; what it takes
    (should (equal "String" (faced "(Integer/parseInt |)")))))

(ert-deftest replique-symbol-test-eldoc-says-nothing-where-there-is-no-call ()
  (replique-test-process)
  (replique-symbol-test--at "(ma|p inc)"
    (should-not (replique-symbol-eldoc #'ignore))))

;;; Taking a definition away

(ert-deftest replique-symbol-test-a-var-is-named-where-it-lives ()
  "The name at point means whatever the namespace around it maps, so what
goes out is the name the var has where it lives, not the name it was written
as.  Which is the whole reason the process is asked first."
  (let ((asked nil))
    (cl-letf (((symbol-function 'replique-symbol--ask)
               (lambda (&rest _) '(:type "function" :name "map" :ns "clojure.core")))
              ((symbol-function 'replique-name-process) (lambda () 'a-process))
              ((symbol-function 'replique-name-context) (lambda () '(:position :code :ns "my.app")))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'replique-process-request-sync)
               (lambda (_process msg &rest _)
                 (setq asked msg)
                 '(:tag "reply" :removed "clojure.core/map"
                        :unmapped (:clojure.core ["map"])))))
      (replique-symbol-test--at "(ns my.app)\n(ma|p inc [1])"
        (replique-remove-var))
      (should (equal '(:op :remove-var :var "clojure.core/map") asked)))))

(ert-deftest replique-symbol-test-a-var-of-another-namespace-is-asked-about ()
  "It is the one somebody may not have meant: removing what point is on must
not be a quiet way to unmap clojure.core from the process.  Declining leaves
it alone."
  (let ((asked nil)
        (sent nil))
    (cl-letf (((symbol-function 'replique-symbol--ask)
               (lambda (&rest _) '(:type "function" :name "map" :ns "clojure.core")))
              ((symbol-function 'replique-name-process) (lambda () 'a-process))
              ((symbol-function 'replique-name-context) (lambda () '(:position :code :ns "my.app")))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt) (setq asked prompt) nil))
              ((symbol-function 'replique-process-request-sync)
               (lambda (&rest _) (setq sent t) nil)))
      (replique-symbol-test--at "(ns my.app)\n(ma|p inc [1])"
        (replique-remove-var))
      (should (string-match-p "clojure.core/map" asked))
      (should-not sent))))

(ert-deftest replique-symbol-test-a-var-of-this-namespace-goes-without-asking ()
  "A definition of the file being edited, taken out of the process the file
was loaded into, is what was asked for and holds no surprise."
  (let ((sent nil))
    (cl-letf (((symbol-function 'replique-symbol--ask)
               (lambda (&rest _) '(:type "function" :name "parse" :ns "my.app")))
              ((symbol-function 'replique-name-process) (lambda () 'a-process))
              ((symbol-function 'replique-name-context) (lambda () '(:position :code :ns "my.app")))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (error "Nothing should be asked")))
              ((symbol-function 'replique-process-request-sync)
               (lambda (_process msg &rest _)
                 (setq sent msg)
                 '(:tag "reply" :removed "my.app/parse" :unmapped (:my.app ["parse"])))))
      (replique-symbol-test--at "(ns my.app)\n(pars|e \"x\")"
        (replique-remove-var))
      (should (equal '(:op :remove-var :var "my.app/parse") sent)))))

(ert-deftest replique-symbol-test-only-a-var-can-be-unmapped ()
  "A class, a local, a keyword: there is no var behind any of them, and
refusing here says so better than the process could."
  (dolist (type '("class" "local" "keyword" "namespace"))
    (cl-letf (((symbol-function 'replique-symbol--ask)
               (lambda (&rest _) (list :type type :name "thing" :ns "my.app")))
              ((symbol-function 'replique-name-context)
               (lambda () '(:position :code :ns "my.app"))))
      (replique-symbol-test--at "(ns my.app)\n(thin|g)"
        ;; The message and not only the failure: without a process there is
        ;; always a user-error to be had further down, and a test that took
        ;; any of them would pass with this check gone
        (let ((err (should-error (replique-remove-var) :type 'user-error)))
          (should (string-match-p "only a var can be unmapped" (cadr err)))
          (should (string-match-p type (cadr err))))))))

(ert-deftest replique-symbol-test-a-name-that-means-nothing-is-not-removed ()
  (cl-letf (((symbol-function 'replique-symbol--ask) (lambda (&rest _) nil))
            ((symbol-function 'replique-name-context)
             (lambda () '(:position :code :ns "my.app"))))
    (replique-symbol-test--at "(ns my.app)\n(no-such-nam|e)"
      (should-error (replique-remove-var) :type 'user-error))))

(ert-deftest replique-symbol-test-what-was-unmapped-elsewhere-is-said ()
  "The namespace it lived in is already in what is being said.  The rest are
the namespaces that referred it, which are the ones whose code will not
compile until somebody edits them - so those are what is worth naming, by the
names they wrote it as being beside the point."
  (should (equal '("my.app.cli" "my.app.web")
                 (replique-symbol--elsewhere
                  '(:my.app.web ["read-it"] :my.app ["parse"] :my.app.cli ["parse"])
                  "my.app")))
  (should-not (replique-symbol--elsewhere '(:my.app ["parse"]) "my.app")))

(ert-deftest replique-symbol-test-a-var-is-taken-away-from-everywhere ()
  "Against a real process: a var referred elsewhere is gone from there too,
which is the whole point of the op - unmapping it where it was defined would
leave every caller still calling it."
  (replique-test-with-repl repl
    (replique-test-eval repl "(clojure.core/ns replique.test-removable)")
    (replique-test-eval repl "(defn gone [] :here)")
    (replique-test-eval
     repl (concat "(clojure.core/ns replique.test-refers "
                  "(:require [replique.test-removable :refer [gone]]))"))
    (replique-test-eval repl "(clojure.core/in-ns 'user)")
    ;; it is there, and there under both names
    (should (replique-symbol-test--at "(ns replique.test-removable)\n(gon|e)"
              (replique-symbol--ask (replique-name-context) "gone")))
    (should (replique-symbol-test--at "(ns replique.test-refers)\n(gon|e)"
              (replique-symbol--ask (replique-name-context) "gone")))
    (replique-symbol-test--at "(ns replique.test-removable)\n(gon|e)"
      (replique-remove-var))
    (should-not (replique-symbol-test--at "(ns replique.test-removable)\n(gon|e)"
                  (replique-symbol--ask (replique-name-context) "gone")))
    (should-not (replique-symbol-test--at "(ns replique.test-refers)\n(gon|e)"
                  (replique-symbol--ask (replique-name-context) "gone")))))

(ert-deftest replique-symbol-test-a-buffer-read-out-of-a-jar-names-its-entry ()
  "Which is what makes jumping into a dependency and loading what is there
work: the two halves the process answered are the two halves that go back."
  (replique-test-process)
  (let ((found (replique-symbol-test--at "(map| inc)"
                 (replique-symbol--ask (replique-name-context) "map"))))
    (let ((buffer (replique-symbol--visit found)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((what (replique-buffer-file)))
              (should (equal (plist-get found :file) (plist-get what :file)))
              (should (equal "clojure/core.clj" (plist-get what :entry)))
              (should (string-match-p (regexp-quote ":entry \"clojure/core.clj\"")
                                      (replique-load-directive what)))))
        (kill-buffer buffer)))))

;;; Turning it on

(ert-deftest replique-symbol-test-it-is-on-in-a-clojure-buffer ()
  "Turning `replique-mode' off takes it back out, which is how somebody says
they would rather be told by something else."
  (with-temp-buffer
    (replique-clojure-mode)
    (should (memq #'replique-symbol-eldoc eldoc-documentation-functions))
    (should (memq #'replique-symbol-xref-backend xref-backend-functions))
    (replique-mode -1)
    (should-not (memq #'replique-symbol-eldoc eldoc-documentation-functions))
    (should-not (memq #'replique-symbol-xref-backend xref-backend-functions))))

(provide 'replique-symbol-test)

;;; replique-symbol-test.el ends here
