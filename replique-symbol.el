;;; replique-symbol.el --- What the name written here is  -*- lexical-binding: t; -*-

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

;; What one name is, which the editor wants to know twice over.
;;
;; While somebody writes a call it wants the arglists, and when they ask
;; where a name came from it wants a file and a line - and both of those are
;; what that name resolves to in that namespace, so both come back from the
;; same request.  It is the request a completion sends, with point at the end
;; of a name rather than partway through one; reading it out of the buffer is
;; `replique-name', which is the half the two of them share.
;;
;; Eldoc asks about the call point is inside, and not about the name at
;; point.  Point is nowhere in particular while the arguments of a call are
;; being written, and what is worth saying there is what the call takes and
;; which argument they are on - so what goes out is the head of the enclosing
;; list.  Which argument that is stays here, because the parse is here: the
;; process is asked what a name is, and answers arglists that are true
;; wherever the name is written.
;;
;; It asks without waiting, which is what eldoc is for and what a completion
;; cannot do.  A capf is called for what it returns; a documentation function
;; is handed a callback and says the answer is coming.
;;
;; Finding a definition is xref's, which is what finds one and what goes
;; back from it, so neither key is bound here.  A definition inside a jar
;; has no file for Emacs to visit - there is no path to a file inside an
;; archive - so the entry is read out into a buffer of its own, which is
;; what cider does and where this took it from.

;;; Code:

(require 'arc-mode)
(require 'cl-lib)
(require 'eldoc)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'replique-common)
(require 'replique-fresh)
(require 'replique-locals)
(require 'replique-name)
(require 'replique-process)

;;; Asking

(defun replique-symbol--asked (op context text)
  "Return the reply to asking OP about TEXT in CONTEXT, or nil.

For the commands somebody presses a key for.  What is asked while they
read rather than while they wait does not wait - see
`replique-symbol-eldoc'.

The whole frame, an error frame included, because what an error means
depends on what was asked.  A name the process could not look up is an
ordinary answer where somebody is reading, and something worth stopping
for where they asked for every use of it - see `xref-backend-references'.

Nil where the wait was quit, which is somebody saying they are no longer
waiting.  There is nothing to say about that: they know."
  (let ((process (replique-name-process)))
    (and process
         (replique-process-request-sync
          process
          (replique-name-message op context text)
          replique-name-timeout))))

(defun replique-symbol--ask (context text)
  "Return what TEXT is in CONTEXT, or nil, by asking the process and waiting.

What could not be answered is said once and answered with nothing, which
is what the commands that read a name want: they are asked about whatever
point happens to be on."
  (when-let* ((frame (replique-symbol--asked :symbol context text)))
    (if (equal "error" (plist-get frame :tag))
        (progn (message "replique: %s" (plist-get frame :message)) nil)
      (plist-get frame :symbol))))

;;; What a name is called

(defun replique-symbol-full-name (found)
  "Return the name in FOUND, written out with what it came from.

Which is the name itself and whatever it came from: a var written under
the namespace it is public in, a class under its package, a member under
the class it is of.  They arrive apart because that is how a completion
carries them - a name does not say where it came from, and that is the
one thing worth saying beside it - and this is where they are put back
together."
  (let ((name (plist-get found :name))
        (type (plist-get found :type))
        (ns (plist-get found :ns))
        (class (plist-get found :class))
        (package (plist-get found :package)))
    (cond
     ((null name) nil)
     ((equal "keyword" type) (if ns (format ":%s/%s" ns name) (format ":%s" name)))
     (ns (format "%s/%s" ns name))
     (class (format "%s/%s" class name))
     (package (format "%s.%s" package name))
     (t name))))

;;; The arguments of an arglist

(defconst replique-symbol--openers '(?\[ ?\( ?{)
  "What opens something written inside an arglist.")

(defconst replique-symbol--closers '(?\] ?\) ?})
  "What closes something written inside an arglist.")

(defconst replique-symbol--blanks '(?\s ?\t ?\n ?\r ?\f ?,)
  "What stands between two arguments of an arglist.

Whatever Clojure reads as whitespace, which is what wrote the arglist -
the comma included, since a comma is whitespace there and a map printed
with more than one key in it is written with one.  A newline is one too:
nothing forces an arglist onto a line, and what stands between two
arguments is not something to face as though it were an argument.")

(defun replique-symbol--arguments (arglist)
  "Return where each argument of ARGLIST is written in it, as (START . END).

ARGLIST is a parameter vector as the process wrote it - \"[f coll]\", and
\"^int [String int]\" for a method, where what stands in front of the
vector is the return type rather than an argument.

What is counted is what stands at the depth the vector opens at, so a
name destructured into a map is one argument however many names are
written inside it, and the ampersand of a rest argument is one of its
own - which is what says the argument after it takes everything from
there on."
  (let ((depth 0)
        (index 0)
        (length (length arglist))
        (from nil)
        (found nil))
    (while (< index length)
      (let ((character (aref arglist index)))
        (cond
         ((memq character replique-symbol--openers)
          (when (and (> depth 0) (null from)) (setq from index))
          (setq depth (1+ depth)))
         ((memq character replique-symbol--closers)
          (if (= depth 1)
              (progn (when from (push (cons from index) found))
                     (setq from nil depth 0))
            (setq depth (max 0 (1- depth)))))
         ((memq character replique-symbol--blanks)
          (when (and (= depth 1) from)
            (push (cons from index) found)
            (setq from nil)))
         (t (when (and (> depth 0) (null from)) (setq from index)))))
      (setq index (1+ index)))
    (nreverse found)))

(defun replique-symbol--argument (arglist argument)
  "Return where ARGUMENT is written in ARGLIST, or nil for nowhere.

ARGUMENT counts from one, and nought is point at what a method is called
on rather than at anything it takes - which is written nowhere in an
arglist, since the arglist is what it takes.

An arglist with a rest argument is written with an ampersand in front of
it and answers for every argument from there on, which is what a rest
argument is; one without answers for the arguments it holds and for no
others, which is what says the call being written is not of this arity."
  (when (> argument 0)
    (replique-symbol--argument-in arglist argument)))

(defun replique-symbol--argument-in (arglist argument)
  "Return where ARGUMENT, which is at least one, is written in ARGLIST."
  (let* ((wheres (replique-symbol--arguments arglist))
         (written (mapcar (lambda (where) (substring arglist (car where) (cdr where)))
                          wheres))
         (rest (seq-position written "&")))
    (if (and rest (> argument rest))
        (nth (1+ rest) wheres)
      (nth (1- argument) wheres))))

(defun replique-symbol--written (arglist argument)
  "Return ARGLIST with ARGUMENT faced, where there is one to face."
  (let ((arglist (copy-sequence arglist))
        (where (replique-symbol--argument arglist argument)))
    (when where
      (add-face-text-property (car where) (cdr where)
                              'eldoc-highlight-function-argument nil arglist))
    arglist))

;;; What is said about a call

(defun replique-symbol--argument-of (text argument)
  "Return which argument of the call TEXT names the ARGUMENT written is.

The same one, except for an instance method.  One is written on the thing
it is called on - (.length s) and (String/.length s) both write s where
the first argument would go - and that thing is not something the method
takes: its parameters start after it.  So what is written first there is
the receiver, and nothing of the arglists is faced while point is at it."
  (if (or (string-prefix-p "." text) (string-match-p "/\\." text))
      (1- argument)
    argument))

(defun replique-symbol--signature (found argument)
  "Return what FOUND takes, with ARGUMENT faced, or nil when it takes nothing.

ARGUMENT is which argument point is at, nil where that is not being
asked, and nought where point is at what a method is called on rather
than at anything it takes.  A field has no arglists and a type instead,
which is written as the tag it would be declared with."
  (let ((arglists (plist-get found :arglists))
        (tag (plist-get found :tag)))
    (cond
     (arglists
      (format "(%s)"
              (mapconcat (lambda (arglist)
                           (if argument
                               (replique-symbol--written arglist argument)
                             arglist))
                         arglists " ")))
     (tag (format "^%s" tag)))))

(defun replique-symbol--said (found argument)
  "Return what there is to say about FOUND, with ARGUMENT faced, or nil.

The name and what it takes on one line, and the docstring under it.  How
much of that reaches the echo area is `eldoc-echo-area-use-multiline-p',
which is somebody's setting rather than this package's business: what
does not fit there is in the documentation buffer, and both of those are
eldoc's to decide."
  (when-let* ((name (replique-symbol-full-name found)))
    (let ((signature (replique-symbol--signature found argument))
          (doc (plist-get found :doc)))
      (concat name
              (when signature (concat ": " signature))
              (when doc (concat "\n" doc))))))

;;;###autoload
(defun replique-symbol-eldoc (callback &rest _ignored)
  "Say what the call point is inside takes, through CALLBACK.

For `eldoc-documentation-functions'.  The head of the enclosing list is
what is asked about and which argument point is at is faced in the
arglists, which is what somebody halfway through writing a call wants to
be told - see `replique-name-call-at-point'.

Asked without waiting.  Eldoc hands a callback over and takes a non-nil
answer to mean one is coming, so nothing here holds the editor while the
process thinks.  What comes back late is dropped: the answer is to a call
point was inside, and point has moved on."
  (when-let* ((process (replique-name-process))
              (call (replique-name-call-at-point))
              (buffer (current-buffer))
              (position (point)))
    (replique-process-request
     process
     (replique-name-message :symbol (plist-get call :context) (plist-get call :text))
     (lambda (frame)
       (when (and (buffer-live-p buffer)
                  (eq position (with-current-buffer buffer (point))))
         (when-let* ((found (plist-get frame :symbol))
                     (said (replique-symbol--said
                            found (replique-symbol--argument-of
                                   (plist-get call :text) (plist-get call :argument)))))
           (funcall callback said :thing (replique-symbol-full-name found))))))
    t))

;;; Where a name was written

(defun replique-symbol--visit-entry (file entry)
  "Return a buffer holding ENTRY of the archive FILE.

There is no path to a file inside an archive, so there is no file for
Emacs to visit: the entry is read out of the archive into a buffer of its
own, named after the two of them together so that the next question finds
that buffer rather than reading it again.  Read only, because writing it
back would mean writing into a jar somebody's build put there.

Which is `archive-zip-extract', from the mode Emacs reads archives with,
used the way cider uses it."
  (let ((name (format "%s:%s" file entry)))
    (or (find-buffer-visiting name)
        (with-current-buffer (generate-new-buffer (file-name-nondirectory entry))
          (archive-zip-extract file entry)
          (set-visited-file-name name t)
          ;; What the buffer holds, said in the two halves the protocol writes
          ;; a file inside a jar in: the name above is not a path, and a
          ;; command asked to load what is in here has to be able to say what
          ;; it is - see `replique-buffer-file'
          (setq-local replique-archive-file file)
          (setq-local replique-archive-entry entry)
          (setq-local default-directory (file-name-directory file))
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (set-auto-mode)
          (current-buffer)))))

(defun replique-symbol-visit (found)
  "Return a buffer holding the file FOUND was written in, or nil.

An entry is what says the file is an archive and the definition is inside
it.  Without one the file is a file and is opened as one.

Public because a definition is not the only thing the process answers with
a file: what has to be loaded again is a list of them, and opening one is
the same two halves resolved the same way - see `replique-stale-app\\='."
  (when-let* ((file (plist-get found :file))
              ((file-exists-p file)))
    (if-let* ((entry (plist-get found :entry)))
        (replique-symbol--visit-entry file entry)
      (find-file-noselect file t))))

;;; Finding a definition

(cl-defstruct (replique-symbol--location
               (:constructor replique-symbol--make-location (found)))
  "Where a name was written, as the process said it.

A location of its own because a definition inside a jar is not a file
Emacs can visit, and there would be nothing to make an
`xref-file-location\\=' out of.  FOUND is what the process answered."
  found)

(cl-defmethod xref-location-marker ((location replique-symbol--location))
  "Return where LOCATION is, opening the file it was written in."
  (let* ((found (replique-symbol--location-found location))
         (buffer (or (replique-symbol-visit found)
                     (user-error "Replique: %s is not there to be opened"
                                 (plist-get found :file)))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        ;; where the form starts, which is what the metadata of a var
        ;; records - the line and column of the paren rather than of the
        ;; name inside it
        (when-let* ((line (plist-get found :line)))
          (forward-line (1- line))
          (when-let* ((column (plist-get found :column)))
            ;; Characters rather than `move-to-column', which counts what a
            ;; tab takes up on screen.  A column the process sends is a count
            ;; of characters - it is what the reader counted while it read the
            ;; file - so a line with a tab in it would land somewhere else
            ;; entirely.  Held to the end of the line, since a column past the
            ;; end of one is a file edited since the process read it
            (forward-char (min (1- column) (- (line-end-position) (point))))))
        (point-marker)))))

(cl-defmethod xref-location-group ((location replique-symbol--location))
  "Return what LOCATION is shown under, which is the file it is in."
  (let ((found (replique-symbol--location-found location)))
    (or (plist-get found :entry) (plist-get found :file) "")))

(cl-defmethod xref-location-line ((location replique-symbol--location))
  "Return the line LOCATION is on, which is what is shown beside it.

It also tells xref that the summary of an item at this location is the
text of that line, so that two names used on one line are shown on one
line - see `replique-symbol--references'."
  (plist-get (replique-symbol--location-found location) :line))

;;;###autoload
(defun replique-symbol-xref-backend ()
  "Return the xref backend for this buffer, or nil when nothing can answer.

For `xref-backend-functions'.  Which is what makes
\\[xref-find-definitions] the way to a definition and \\[xref-go-back]
the way back from it, without either of them being bound here: finding a
definition is a thing Emacs already does, and a package that bound its
own keys for it would be a package whose history is not the history the
rest of the editor keeps."
  (and (replique-name-process) 'replique))

(defun replique-symbol--asked-about ()
  "Return what point is asking about, or nil where it asks about nothing.

`replique-name-context' and, where that answers nothing, the name of a
definition.  It answers nothing there on purpose: a name being given is a
name a completion has nothing to offer for, since nothing knows it yet.

But the name of a `defn' or a `deftype' is a var or a class all the same -
see `replique-locals-at-definition-name-p' - and it is exactly the name to
ask about with point on it.  Who calls this is asked where the definition
is written, not at one of the call sites; and what this is, is asked there
by anything that reads a buffer looking for a definition to describe.

A parameter, a `let' binding and the name of an (fn name [x] ...) are not
these.  Each of them is a local, and a local is a name the process has
never seen."
  (or (replique-name-context)
      (when (replique-locals-at-definition-name-p (point))
        (append (list :position :code)
                (when-let* ((ns (replique-name-namespace)))
                  (list :ns ns))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql replique)))
  "Return the name point is in, carrying what it was read in.

The context rides on the string because that is the only thing xref
hands back: what a name means depends on the namespace it is written in
and the slot of the form it is written at, and by the time the backend is
asked for the definition, point may be somewhere else entirely."
  (when-let* ((bounds (replique-name-at-point))
              (context (replique-symbol--asked-about)))
    (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (apply #'propertize text 'replique-context context
             ;; and where it is bound, when it is bound here.  A local is
             ;; not something the process has ever seen, so where it was
             ;; written is not something it could answer
             (when-let* ((bound (replique-name-bound-at text)))
               (list 'replique-bound bound))))))

(cl-defmethod xref-backend-definitions ((_backend (eql replique)) identifier)
  "Return where IDENTIFIER was written, as a list of one or as nil.

One, because one name is one thing.  None where the name means nothing
here, and none where it means something with no file to open - a class of
the runtime is compiled and the Java it was written in is not on the
classpath.

A local is answered without asking anything, since a local is bound by a
form in the buffer and the process has never seen it.

IDENTIFIER carries the context it was read in where it was read out of a
buffer, and where it is bound where it is a local.  One somebody typed at
the prompt carries neither, and is read against the namespace they typed
it in."
  (if-let* ((bound (get-text-property 0 'replique-bound identifier)))
      (list (xref-make (substring-no-properties identifier)
                       (xref-make-buffer-location (marker-buffer bound)
                                                  (marker-position bound))))
    (replique-symbol--definitions identifier)))

(defun replique-symbol--context (identifier)
  "Return what IDENTIFIER was read in, as the request carries it.

Whatever rode along on the string where it came out of a buffer, and
otherwise what point is in now - which is what a name somebody typed at a
prompt is read against."
  (or (get-text-property 0 'replique-context identifier)
      (replique-name-context)
      (append (list :position :code)
              (when-let* ((ns (replique-name-namespace)))
                (list :ns ns)))))

(defun replique-symbol--definitions (identifier)
  "Return where IDENTIFIER was written, by asking the process."
  (when-let* ((context (replique-symbol--context identifier))
              (found (replique-symbol--ask context (substring-no-properties identifier)))
              ((plist-get found :file)))
    (list (xref-make (or (replique-symbol--said found nil) identifier)
                     (replique-symbol--make-location found)))))

;;; Finding every use

;; Which is a different question from finding a definition, and a harder one.
;; A definition is one place and the process knows it because the var
;; remembers it; a use is every place a name was written, and the name is not
;; the same name in each of them.  `clojure.core/let' is written let where
;; core is referred, c/let where it is aliased, something else again where a
;; :rename gave it another name - and a macro writes it in code nobody typed.
;; Searching the text finds the ones spelled the way point is spelled, misses
;; the rest, and hits every let in every string and comment on the way.
;;
;; So it is the process that answers, out of what its compiler resolved while
;; it read the files - see the :usages op.  Not every process can: it takes a
;; compiler that writes that down, which is a fork of clojure rather than
;; clojure, and one that does not says so when asked rather than answering
;; that the name is used nowhere.

(defun replique-symbol--source (found opened)
  "Return a buffer holding what FOUND was written in, or nil.

OPENED remembers what has already been read, so that a file holding a
hundred uses is read once - and says which of them are this command's to
kill afterwards.  A file is read into a buffer of its own, with none of
what visiting one does; an entry of an archive is read into the buffer
`replique-symbol--visit-entry' keeps, which is the one somebody lands in
when they jump there, so it is left alone."
  (let* ((file (plist-get found :file))
         (entry (plist-get found :entry))
         (key (cons file entry))
         (known (gethash key opened 'missing)))
    (car (if (eq 'missing known)
             (puthash key
                      (if entry
                          (cons (replique-symbol-visit found) nil)
                        (cons (when (and file (file-readable-p file))
                                (let ((buffer (generate-new-buffer " *replique-source*" t)))
                                  (with-current-buffer buffer
                                    (insert-file-contents file))
                                  buffer))
                              t))
                      opened)
           known))))

(defun replique-symbol--line-of (buffer line)
  "Return the text of LINE of BUFFER, or nil when there is no such line."
  (when (and buffer line)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (zerop (forward-line (1- line)))
          (buffer-substring-no-properties (point) (line-end-position)))))))

(defun replique-symbol--same-line-p (a b)
  "Whether the usages A and B are on the same line of the same file."
  (and (equal (plist-get a :file) (plist-get b :file))
       (equal (plist-get a :entry) (plist-get b :entry))
       (equal (plist-get a :line) (plist-get b :line))))

(defun replique-symbol--width (usage)
  "Return how many characters USAGE covers, or nil when it does not say.

Which is the name as it is written there rather than the name of the var:
seven where it is written c/thing and five where it is written thing.
Nothing for one that does not end on the line it starts on, which a name
cannot do - but the process is answering out of what it recorded, and a
recording with no end in it is not one to make a replacement out of."
  (let ((line (plist-get usage :line))
        (column (plist-get usage :column))
        (end-line (plist-get usage :end-line))
        (end-column (plist-get usage :end-column)))
    (when (and column end-column (equal line end-line) (> end-column column))
      (- end-column column))))

(defun replique-symbol--reference (usage previous next opened)
  "Return the xref item for USAGE, or nil when its file cannot be read.

PREVIOUS and NEXT are the usages either side of it, where those are on
the same line, and between them they say what piece of that line the
summary is.  OPENED is where the files already read are remembered - see
`replique-symbol--source'.

Xref shows the items of one line of one file as one line, by writing
their summaries one after another - so the first of them holds
the line up to the second, the second the line up to the third, and the
last one the rest of it.  Written whole, each of them, the line would be
shown once for every name in it.

That is also what makes \\[xref-query-replace-in-results] work on the
answer: it asks whether the text at each item is still the text its
summary says, reading from the start of the line for the first of them
and from the item itself for the rest - which is what these pieces are -
and replaces the many characters `replique-symbol--width' says.  So
renaming a var across a project is `xref-find-references' and then one
command, and neither of them is replique's."
  (when-let* ((column (plist-get usage :column))
              (buffer (replique-symbol--source usage opened))
              (line (replique-symbol--line-of buffer (plist-get usage :line)))
              (from (if previous (1- column) 0))
              (to (if next (1- (plist-get next :column)) (length line)))
              ((<= 0 from to (length line))))
    (let ((summary (substring line from to))
          (location (replique-symbol--make-location usage))
          (width (replique-symbol--width usage)))
      (if width (xref-make-match summary location width) (xref-make summary location)))))

(defun replique-symbol--references (usages)
  "Return the xref items for USAGES, as the process answered with them.

In the order they came, which is by file and down each file: what an
answer is used for is walking through it, and one that came back in a
different order each time is one nothing can be walked through.  It is
also the order the summaries are cut up in - see
`replique-symbol--reference'."
  (let ((opened (make-hash-table :test #'equal))
        (previous nil)
        (found nil))
    (unwind-protect
        (progn
          (while usages
            (let* ((usage (car usages))
                   (rest (cdr usages))
                   (before (and previous
                                (replique-symbol--same-line-p previous usage)
                                previous))
                   (next (and rest
                              (replique-symbol--same-line-p usage (car rest))
                              (car rest))))
              (when-let* ((item (replique-symbol--reference usage before next opened)))
                (push item found))
              (setq previous usage)
              (setq usages rest)))
          (nreverse found))
      ;; The buffers this read a file into, and not the ones it was handed:
      ;; an entry of an archive is read into the buffer somebody lands in
      ;; when they jump there, which was there before this and stays after it
      (maphash (lambda (_key value)
                 (when (and (cdr value) (buffer-live-p (car value)))
                   (kill-buffer (car value))))
               opened))))

(defun replique-symbol--members (usages)
  "Return what USAGES use of a package, as (MEMBER . COUNT), most used first.

A use of the package itself rather than of anything in it - the module
object, js/console as a value - is counted under the package's own name,
which is nil here and is written as such by `replique-symbol--say-members'."
  (let ((counts nil))
    (dolist (usage usages)
      (unless (plist-get usage :declaration)
        (let* ((member (plist-get usage :member))
               (entry (assoc member counts)))
          (if entry (setcdr entry (1+ (cdr entry))) (push (cons member 1) counts)))))
    ;; In the order they were first used among equals, which is the order the
    ;; list shows them in: `sort' keeps it
    (sort (nreverse counts) (lambda (a b) (> (cdr a) (cdr b))))))

(defun replique-symbol--say-members (found usages)
  "Say which parts of the package FOUND names USAGES use, where they say.

Where the name at point is a package of the host's - a JavaScript module,
a Closure namespace, an object under js/ - every use of anything in it is
an answer, and each says which part of it it was.  The list shows the
places; this is the other half of the question, which the list only says
one line at a time: what the project uses of the package at all."
  (when (cl-some (lambda (usage) (plist-get usage :member)) usages)
    (let ((name (replique-symbol-full-name found)))
      (message "%s: %s" name
               (mapconcat (lambda (entry)
                            (format "%s ×%d" (or (car entry) name) (cdr entry)))
                          (replique-symbol--members usages) ", ")))))

(cl-defmethod xref-backend-references ((_backend (eql replique)) identifier)
  "Return every place IDENTIFIER is used, as a list of xref items.

For \\[xref-find-references], and through it for
\\[xref-query-replace-in-results] - which is what renaming a var across a
project is, and it is xref's command rather than one of replique's.

A var, a keyword and a class are all answered, since all three are things
somebody renames and none of the three can be found by reading the text.
So is a name of the host's in ClojureScript - js/console, gstr/trim, an
export of a JavaScript module - and a package of them, whose answer is
every use of anything in it: see `replique-symbol--say-members'.

A local is not: it is bound by a form in this buffer, the process has
never seen it, and a process that answered about one would be answering
about somebody else's.  Said rather than answered with nothing, which
xref would show as the name being used nowhere.

What changed on disk is offered to be loaded first - see
`replique-fresh-ensure\\='.  This is answered out of what the compiler
recorded while it compiled the files, so a file edited since is one the
answer is quietly wrong about: a use that was deleted is still in it and
one that was written is not.  Which matters most here of anywhere, since
what this is for is renaming a var everywhere it is written, and
everywhere has to be all of them."
  (when (get-text-property 0 'replique-bound identifier)
    (user-error "Replique: %s is bound here, so the process has never seen it"
                (substring-no-properties identifier)))
  ;; What point is on is read before anything is offered: point is on
  ;; nothing to ask about as often as not, and being offered a load first
  ;; would be being offered one for a question that was never going to be
  ;; asked
  (let ((context (or (replique-symbol--context identifier)
                     (user-error "Replique: nothing here to ask about"))))
    (replique-fresh-ensure "finding every use of a name")
    (let ((frame (replique-symbol--asked :usages context
                                         (substring-no-properties identifier))))
      (when frame
        (when (equal "error" (plist-get frame :tag))
          (user-error "Replique: %s" (plist-get frame :message)))
        (replique-symbol--say-members (plist-get frame :symbol) (plist-get frame :usages))
        (replique-symbol--references (plist-get frame :usages))))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql replique)))
  "Return nothing to complete an identifier with.

What could be written where point is, is a completion, and it is answered
where somebody is writing rather than at a prompt asking for a name in
the abstract - see `replique-completion-at-point'."
  nil)

;;; Taking a definition away

;; A definition put into a process stays there.  Rename one and evaluate the
;; file again and the process holds both, the old name resolving to the old
;; code - which is the long repl's oldest disease: the repl agrees with itself
;; all afternoon and the build is the first thing to disagree.
;;
;; So the var to remove is nearly never the one at point.  It is the old name,
;; and by the time somebody wants it gone it is written nowhere in the buffer:
;; they renamed it.  What is offered is therefore the namespace's own vars,
;; which only the process has - it is the process that holds the definition
;; that is no longer written anywhere.
;;
;; The name at point is the default rather than the answer.  Pressing return
;; removes what point is on, which is the other half of what this is for, and
;; anything else is chosen from the list.

(defconst replique-symbol-vars-timeout 5
  "How long to wait for the process to say what a namespace holds, in seconds.")

(defun replique-symbol--vars (process ns)
  "Return the vars NS holds, as PROCESS wrote them, or nil.

Waited for rather than answered later: these are the choices of a prompt
about to be shown, and there is no showing a prompt before there is
anything to put in it.  Bounded, so that a process which stopped
answering is a command that fails rather than an Emacs that hangs.

The order is the process's, which is the order they were written in the
file - so the list reads like the file, and the definition somebody just
renamed is where they would look for it."
  (let ((frame (replique-process-request-sync
                process (append (list :op :vars :ns ns)
                                (replique-dialect-keys))
                replique-symbol-vars-timeout)))
    (cond
     ;; C-g, which is somebody saying they are no longer waiting
     ((null frame) nil)
     ((equal "error" (plist-get frame :tag))
      (user-error "%s" (plist-get frame :message)))
     (t (plist-get frame :vars)))))

(defun replique-symbol--var-names (vars)
  "Return the names in VARS, a private one annotated as private.

The annotation and not a separate list: what is being chosen from is the
whole of what the namespace holds, and which of them are private is worth
seeing rather than worth filtering by."
  (mapcar (lambda (var)
            (let ((name (plist-get var :name)))
              (if (plist-get var :private)
                  (propertize name 'replique-annotation " private")
                name)))
          vars))

(defun replique-symbol--annotate (name)
  "Return what to show beside NAME in the list, or nil."
  (get-text-property 0 'replique-annotation name))

(defun replique-symbol--at-point (names)
  "Return the name at point, when it is one of NAMES written plainly.

Plainly, because a qualified name at point is a var of somewhere else: it
is str/join that is written str/join, and join is not what this buffer
holds.  The list is of what this namespace holds, so the default has to
be one of them."
  (when-let* ((bounds (replique-name-at-point))
              (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
              ((member text names)))
    text))

(defun replique-symbol--elsewhere (unmapped home)
  "Return the namespaces UNMAPPED names other than HOME, sorted.

UNMAPPED is what the process said, which is a namespace to the names it
wrote the var as.  HOME is where the var lived, and is left out because
it is already in what is being said: the interesting half is the rest,
the namespaces that referred it and whose code will not compile until
somebody edits them."
  (let ((names nil))
    (while unmapped
      (let ((ns (substring (symbol-name (car unmapped)) 1)))
        (unless (equal ns home) (push ns names)))
      (setq unmapped (cddr unmapped)))
    (sort names #'string<)))

(defun replique-symbol--removed (frame)
  "Say what FRAME says was taken away."
  (let* ((removed (plist-get frame :removed))
         (home (when (string-match "\\`\\(.*\\)/" removed)
                 (match-string 1 removed)))
         (elsewhere (replique-symbol--elsewhere (plist-get frame :unmapped) home)))
    (if elsewhere
        (message "replique: removed %s, and unmapped it from %s"
                 removed (string-join elsewhere ", "))
      (message "replique: removed %s" removed))))

;;;###autoload
(defun replique-remove-var (var)
  "Unmap VAR from everywhere the process maps it.

Not `ns-unmap\\=', which would leave it where it was referred.  A var that
was referred is in every namespace that referred it, under whatever name
that namespace referred it as, so taking it away from where it was
defined leaves every caller still calling it.

What is offered is what the namespace this buffer is in holds, which is
where a renamed definition is to be found: by the time the old name is
worth removing it has been renamed in the buffer and is written nowhere
in it.  The name at point is the default, so removing the definition
point is on is a return.

A name typed rather than chosen is sent as it was typed, the way
`replique-in-ns\\=' sends a namespace: a bare one is a var of this
namespace, and a qualified one is a var of somewhere else and is asked
about first - `map\\=' means clojure.core\\='s var in most namespaces, and
unmapping clojure.core from the process is not something to do by
pressing return.

There is no undoing it short of evaluating the definition again."
  (interactive
   (let* ((process (or (replique-name-process)
                       (user-error "No process - M-x replique-connect")))
          (ns (or (replique-name-namespace)
                  (user-error "Nothing here says which namespace to look in")))
          (names (replique-symbol--var-names (replique-symbol--vars process ns))))
     (unless names
       (user-error "The process holds nothing in %s" ns))
     (let ((default (replique-symbol--at-point names))
           (completion-extra-properties
            (list :annotation-function #'replique-symbol--annotate)))
       (list (completing-read (format-prompt "Remove var" default)
                              names nil nil nil nil default)))))
  (let* ((written (string-trim (substring-no-properties var)))
         (name (if (string-match-p "/" written)
                   written
                 (format "%s/%s" (replique-name-namespace) written))))
    (when (string-empty-p written)
      (user-error "No var"))
    (when (or (not (string-match-p "/" written))
              (yes-or-no-p (format "Remove %s from the process? " name)))
      (let ((frame (replique-process-request-sync
                    (replique-name-process)
                    (append (list :op :remove-var :var name)
                            (replique-dialect-keys))
                    replique-name-timeout)))
        (cond
         ;; C-g, which is somebody saying they are no longer waiting
         ((null frame) nil)
         ((equal "error" (plist-get frame :tag))
          (user-error "%s" (plist-get frame :message)))
         (t (replique-symbol--removed frame)))))))

;;; Turning it on

;;;###autoload
(defun replique-symbol-install ()
  "Answer documentation and find definitions in the current buffer."
  (add-hook 'eldoc-documentation-functions #'replique-symbol-eldoc nil t)
  (add-hook 'xref-backend-functions #'replique-symbol-xref-backend nil t))

(defun replique-symbol-uninstall ()
  "Stop answering documentation and finding definitions in this buffer."
  (remove-hook 'eldoc-documentation-functions #'replique-symbol-eldoc t)
  (remove-hook 'xref-backend-functions #'replique-symbol-xref-backend t))

(provide 'replique-symbol)

;;; replique-symbol.el ends here
