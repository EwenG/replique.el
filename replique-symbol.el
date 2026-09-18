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
(require 'replique-name)
(require 'replique-process)

;;; Asking

(defun replique-symbol--ask (context text)
  "Return what TEXT is in CONTEXT, or nil, by asking the process and waiting.

For the commands somebody presses a key for.  What is asked while they
read rather than while they wait does not wait - see
`replique-symbol-eldoc'."
  (let* ((process (replique-name-process))
         (frame (and process
                     (replique-process-request-sync
                      process
                      (replique-name-message :symbol context text)
                      replique-name-timeout))))
    (cond
     ;; C-g, which is somebody saying they are no longer waiting.  Nothing
     ;; to say about it: they know
     ((null frame) nil)
     ((equal "error" (plist-get frame :tag))
      (message "replique: %s" (plist-get frame :message))
      nil)
     (t (plist-get frame :symbol)))))

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
          (setq-local default-directory (file-name-directory file))
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (set-auto-mode)
          (current-buffer)))))

(defun replique-symbol--visit (found)
  "Return a buffer holding the file FOUND was written in, or nil.

An entry is what says the file is an archive and the definition is inside
it.  Without one the file is a file and is opened as one."
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
`xref-file-location\=' out of.  FOUND is what the process answered."
  found)

(cl-defmethod xref-location-marker ((location replique-symbol--location))
  "Return where LOCATION is, opening the file it was written in."
  (let* ((found (replique-symbol--location-found location))
         (buffer (or (replique-symbol--visit found)
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
            (move-to-column (1- column))))
        (point-marker)))))

(cl-defmethod xref-location-group ((location replique-symbol--location))
  "Return what LOCATION is shown under, which is the file it is in."
  (let ((found (replique-symbol--location-found location)))
    (or (plist-get found :entry) (plist-get found :file) "")))

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

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql replique)))
  "Return the name point is in, carrying what it was read in.

The context rides on the string because that is the only thing xref
hands back: what a name means depends on the namespace it is written in
and the slot of the form it is written at, and by the time the backend is
asked for the definition, point may be somewhere else entirely."
  (when-let* ((bounds (replique-name-at-point))
              (context (replique-name-context)))
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

(defun replique-symbol--definitions (identifier)
  "Return where IDENTIFIER was written, by asking the process."
  (when-let* ((context (or (get-text-property 0 'replique-context identifier)
                           (replique-name-context)
                           (append (list :position :code)
                                   (when-let* ((ns (replique-name-namespace)))
                                     (list :ns ns)))))
              (found (replique-symbol--ask context (substring-no-properties identifier)))
              ((plist-get found :file)))
    (list (xref-make (or (replique-symbol--said found nil) identifier)
                     (replique-symbol--make-location found)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql replique)))
  "Return nothing to complete an identifier with.

What could be written where point is, is a completion, and it is answered
where somebody is writing rather than at a prompt asking for a name in
the abstract - see `replique-completion-at-point'."
  nil)

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
