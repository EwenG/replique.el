;;; replique-inspect.el --- Browsing a value of the process, a piece at a time  -*- lexical-binding: t; -*-

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

;; A value of the process, shown as a tree that is opened a node at a time.
;; What a repl prints of a value is cut by *print-length* and *print-level*
;; all at once, and what was cut is gone; here nothing is cut, it is only not
;; asked for yet.  The value stays in the process - in the runtime, for
;; ClojureScript - and what comes over is a line per child, a page at a time:
;; a short printing of it, what it is, how many things it holds.
;;
;; Three things can be looked at this way:
;;
;;   `replique-watch'            a var, and the atom it holds - watched, so the
;;                               buffer knows when the atom is swapped
;;   `replique-inspect-results'  what a repl returned last, as *1, *2 and *3
;;   `replique-taps'             what was given to `tap>', newest last
;;
;; A watched value is never refreshed behind your back: the buffer says that
;; it changed, and \\`g' shows what it changed to.  A value that moves under
;; the cursor while it is being read is a value that cannot be read.  What
;; changed since the last refresh is highlighted.  The last values of a watched var are kept, and [ and ] go
;; through them.
;;
;; A node is a path, to the process: what was open stays open across a
;; refresh, and a node that is no longer there says so.  A view whose process
;; restarted, or whose page reloaded, is opened again by the next refresh, and
;; what was open is opened again by the keys that led to it.
;;
;; See doc/protocol.md in replique for the ops this is built on.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'replique-common)
(require 'replique-conn)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-name)
(require 'replique-pprint)
(require 'replique-symbol)

(defcustom replique-inspect-page-size 100
  "How many children of a node are asked for at a time."
  :type 'natnum
  :group 'replique)

(defcustom replique-inspect-history 10
  "How many of the values a watched var held are kept, to go back through.

Each change is one more, and the oldest goes.  Zero keeps none."
  :type 'natnum
  :group 'replique)

(defface replique-inspect-changed
  '((((class color) (background dark)) :background "#3f3a1c")
    (((class color) (background light)) :background "#fbf2c4")
    (t :inverse-video t))
  "Face for what changed since the value shown before it."
  :group 'replique)

(defface replique-inspect-key
  '((t :inherit font-lock-constant-face))
  "Face for the key a line of an inspected value is under."
  :group 'replique)

(defface replique-inspect-gone
  '((t :inherit shadow :strike-through t))
  "Face for a node of an inspected value that is no longer there."
  :group 'replique)

;;; What a buffer is showing

(defvar-local replique-inspect--process nil
  "The process the value is in.")

(defvar-local replique-inspect--keys nil
  "The dialect keys of the view: nil for Clojure.")

(defvar-local replique-inspect--source nil
  "Where the value comes from, as the `:inspect' op takes it.")

(defvar-local replique-inspect--title nil
  "What the buffer is a view of, in a few words.")

(defvar-local replique-inspect--view nil
  "The id the process gave the view, nil until it is opened.")

(defvar-local replique-inspect--nodes nil
  "What is known of each node, by id.

A plist of the `:line' the process sent, whether it is `:open', its
`:children' ids, whether there are `:more' and how many in `:total', its
`:depth', and whether it is
`:gone'.")

(defvar-local replique-inspect--history nil
  "Where the history of a watched var is: (:count N) live, (:count N :at I)
showing an older value.  Nil where none is kept.")

(defvar-local replique-inspect--meta nil
  "Whether metadata is shown, as the first child of what has some.")

(defvar-local replique-inspect--focus nil
  "The nodes the buffer was narrowed to, innermost first.")

(defvar-local replique-inspect--stale nil
  "Whether the value changed since it was last asked for.")

(defvar-local replique-inspect--generation 0
  "Bumped by every refresh and every opening.

So that what answers an older one is not mistaken for what answers this
one.")

(defvar-local replique-inspect--busy nil
  "Whether a refresh is waiting for its answers.")

(defvar-local replique-inspect--again nil
  "Whether another refresh was asked for while one was waiting.")

(defvar-local replique-inspect--error nil
  "Why the view could not be opened, shown in place of the value.")

;;; Asking the process

(defun replique-inspect--live-process ()
  "Return the process the value is in, or the one now in its directory.

A process that was restarted is a new process, and a view of the old one
is opened again on it."
  (let ((process replique-inspect--process))
    (unless (replique-process-live-p process)
      (let ((again (and process (replique-process--directory process)
                        (replique-process-in (replique-process--directory process)))))
        (unless again
          (user-error "The process is gone"))
        (setq replique-inspect--process again
              replique-inspect--view nil)))
    replique-inspect--process))

(defun replique-inspect--width (depth)
  "Return how wide the printing of a line at DEPTH can be."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 30 (- (if window (window-body-width window) 100)
               (* 2 depth) 24))))

(defun replique-inspect--ask (msg callback)
  "Send MSG about the view of the current buffer, and call CALLBACK.

CALLBACK is called in this buffer with the reply, and not at all when the
buffer was killed or the view opened again in the meantime."
  (let ((buffer (current-buffer))
        (generation replique-inspect--generation))
    (replique-process-request
     (replique-inspect--live-process)
     msg
     (lambda (frame)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (= generation replique-inspect--generation)
             (funcall callback frame))))))))

(defun replique-inspect--gone-p (frame)
  "Return non-nil when FRAME says the view itself is no longer there."
  (and (equal "error" (plist-get frame :tag))
       (member (plist-get frame :error) '("unknown-view" "view-gone"))))

(defun replique-inspect--failed (frame)
  "Say why FRAME, an error frame, could not be answered."
  (message "replique: %s" (plist-get frame :message)))

;;; The nodes

(defun replique-inspect--node (id)
  "Return what is known of the node ID."
  (gethash id replique-inspect--nodes))

(defun replique-inspect--set (id &rest props)
  "Set PROPS on the node ID."
  (let ((node (gethash id replique-inspect--nodes)))
    (while props
      (setq node (plist-put node (pop props) (pop props))))
    (puthash id node replique-inspect--nodes)))

(defun replique-inspect--store (parent frame offset)
  "Keep the children FRAME holds, the page of PARENT's from OFFSET."
  (if (plist-get frame :gone)
      (replique-inspect--set parent :gone t :children nil :open nil)
    (let* ((depth (1+ (or (plist-get (replique-inspect--node parent) :depth) 0)))
           (ids (mapcar (lambda (line)
                          (let ((id (plist-get line :node)))
                            (replique-inspect--set id :line line :depth depth :gone nil)
                            id))
                        (plist-get frame :children))))
      (replique-inspect--set
       parent
       :children (if (> offset 0)
                     (append (plist-get (replique-inspect--node parent) :children) ids)
                   ids)
       :more (eq t (plist-get frame :more))
       :total (plist-get frame :total)
       :gone nil))))

(defun replique-inspect--shown (frame)
  "Keep what FRAME, the answer to an opening or a refresh, shows."
  (let ((root (plist-get frame :root)))
    (replique-inspect--set 0 :line root :depth 0 :open t :gone nil)
    (if (plist-get root :expandable)
        (replique-inspect--store 0 frame 0)
      (replique-inspect--set 0 :children nil :more nil :total nil))
    (setq replique-inspect--history (plist-get frame :history))))

(defun replique-inspect--open-nodes ()
  "Return the open nodes below the root that are shown, parents first."
  (let ((open nil))
    (cl-labels ((walk (id)
                  (dolist (child (plist-get (replique-inspect--node id) :children))
                    (let ((node (replique-inspect--node child)))
                      (if (plist-get node :open)
                          (progn (push child open) (walk child))
                        ;; What is cached below a closed node is of the value
                        ;; before this one, and is asked for again when it is
                        ;; opened
                        (replique-inspect--set child :children nil))))))
      (walk 0)
      (dolist (id replique-inspect--focus)
        (unless (memq id open) (push id open)))
      (nreverse open))))

(defun replique-inspect--key-path (id)
  "Return the keys that lead from the root to the node ID.

What a node is called by its parent: what opens it again in a view opened
anew, where the ids are the new view's."
  (let ((path nil)
        (found t))
    (while (and found (not (eql id 0)))
      (setq found nil)
      (maphash (lambda (parent node)
                 (when (and (not found) (memql id (plist-get node :children)))
                   (setq found parent)))
               replique-inspect--nodes)
      (when found
        (let ((line (plist-get (replique-inspect--node id) :line)))
          (push (list (plist-get line :via) (or (plist-get line :key) (plist-get line :value)))
                path))
        (setq id found)))
    (when found path)))

(defun replique-inspect--child-by-key (parent key)
  "Return the child of PARENT called KEY - see `replique-inspect--key-path'."
  (seq-find (lambda (id)
              (let ((line (plist-get (replique-inspect--node id) :line)))
                (equal key (list (plist-get line :via)
                                 (or (plist-get line :key) (plist-get line :value))))))
            (plist-get (replique-inspect--node parent) :children)))

;;; Opening and refreshing

(defun replique-inspect--open ()
  "Open the view of the current buffer, and open again what was open in it."
  (let ((paths (when replique-inspect--nodes
                 (delq nil (mapcar #'replique-inspect--key-path
                                   (replique-inspect--open-nodes)))))
        (focus (when replique-inspect--focus
                 (replique-inspect--key-path (car replique-inspect--focus)))))
    (setq replique-inspect--generation (1+ replique-inspect--generation)
          replique-inspect--busy t
          replique-inspect--error nil)
    (replique-inspect--ask
     (append (list :op :inspect
                   :source replique-inspect--source
                   :width (replique-inspect--width 1)
                   :limit replique-inspect-page-size)
             (when replique-inspect--meta (list :meta t))
             (when (and (plist-get replique-inspect--source :var)
                        (> replique-inspect-history 0))
               (list :history replique-inspect-history))
             replique-inspect--keys)
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (progn
             (setq replique-inspect--busy nil
                   replique-inspect--view nil
                   replique-inspect--error (plist-get frame :message))
             (replique-inspect--render))
         (setq replique-inspect--view (plist-get frame :view)
               replique-inspect--nodes (make-hash-table)
               replique-inspect--focus nil
               replique-inspect--stale nil)
         (replique-inspect--shown frame)
         (replique-inspect--reopen paths focus))))))

(defun replique-inspect--reopen (paths focus)
  "Open again the nodes PATHS lead to, one after the other.

Then narrow to FOCUS, where it still leads somewhere."
  (if (null paths)
      (progn
        (when focus
          (let ((id 0))
            (dolist (key focus)
              (setq id (and id (replique-inspect--child-by-key id key))))
            (when id (setq replique-inspect--focus (list id)))))
        (setq replique-inspect--busy nil)
        (replique-inspect--render)
        (replique-inspect--again-if-asked))
    (let ((id 0))
      (dolist (key (car paths))
        (setq id (and id (replique-inspect--child-by-key id key))))
      (if (null id)
          (replique-inspect--reopen (cdr paths) focus)
        (replique-inspect--ask
         (list :op :inspect-children :view replique-inspect--view :node id
               :limit replique-inspect-page-size
               :width (replique-inspect--width
                       (1+ (or (plist-get (replique-inspect--node id) :depth) 0)))
               :meta replique-inspect--meta)
         (lambda (frame)
           (unless (equal "error" (plist-get frame :tag))
             (replique-inspect--store id frame 0)
             (replique-inspect--set id :open t))
           (replique-inspect--reopen (cdr paths) focus)))))))

(defun replique-inspect--again-if-asked ()
  "Refresh again, where a refresh was asked for while one was waiting."
  (when replique-inspect--again
    (setq replique-inspect--again nil)
    (replique-inspect-refresh)))

(defun replique-inspect-refresh ()
  "Ask the process for the value again, and for every node that is open."
  (interactive)
  (cond
   (replique-inspect--busy (setq replique-inspect--again t))
   ((or (null replique-inspect--view)
        (not (replique-process-live-p replique-inspect--process)))
    (replique-inspect--live-process)
    (replique-inspect--open))
   (t
    (setq replique-inspect--generation (1+ replique-inspect--generation)
          replique-inspect--busy t
          replique-inspect--stale nil)
    (let ((open (replique-inspect--open-nodes))
          (loaded (length (plist-get (replique-inspect--node 0) :children))))
      (replique-inspect--ask
       (append (list :op :inspect-refresh :view replique-inspect--view
                     :width (replique-inspect--width 1)
                     :limit (max replique-inspect-page-size loaded))
               (when-let* ((at (plist-get replique-inspect--history :at)))
                 (list :at at))
               (when replique-inspect--meta (list :meta t)))
       (lambda (frame)
         (cond
          ((replique-inspect--gone-p frame)
           (setq replique-inspect--busy nil)
           (replique-inspect--open))
          ((equal "error" (plist-get frame :tag))
           (setq replique-inspect--busy nil)
           (replique-inspect--failed frame))
          (t
           (replique-inspect--shown frame)
           (replique-inspect--refresh-nodes open)))))))))

(defun replique-inspect--refresh-nodes (open)
  "Ask again for the children of the nodes OPEN, then show them all."
  (if (null open)
      (progn (setq replique-inspect--busy nil)
             (replique-inspect--render)
             (replique-inspect--again-if-asked))
    (let* ((id (car open))
           (node (replique-inspect--node id)))
      (replique-inspect--ask
       (list :op :inspect-children :view replique-inspect--view :node id
             :limit (max replique-inspect-page-size (length (plist-get node :children)))
             :width (replique-inspect--width (1+ (or (plist-get node :depth) 0)))
             :meta replique-inspect--meta)
       (lambda (frame)
         (cond
          ((replique-inspect--gone-p frame)
           (setq replique-inspect--busy nil)
           (replique-inspect--open))
          (t
           (unless (equal "error" (plist-get frame :tag))
             (replique-inspect--store id frame 0))
           (replique-inspect--refresh-nodes (cdr open)))))))))

;;; Showing it

(defun replique-inspect--annotation (line)
  "Return what LINE is said to be, beside its printing."
  (let ((kind (plist-get line :kind))
        (count (plist-get line :count)))
    (when (member kind '("map" "set" "vector" "list" "seq" "array" "collection"
                         "object" "ref" "record" "var"))
      (string-join (delq nil (list (if (member kind '("object" "ref" "record"))
                                       (plist-get line :type)
                                     kind)
                                   (when count (number-to-string count))))
                   " · "))))

(defun replique-inspect--insert-line (id depth)
  "Write the line of the node ID, at DEPTH."
  (let* ((node (replique-inspect--node id))
         (line (plist-get node :line))
         (key (plist-get line :key))
         (gone (plist-get node :gone))
         (start (point)))
    (insert (make-string (* 2 depth) ?\s))
    (insert (cond ((not (plist-get line :expandable)) "  ")
                  ((plist-get node :open) "▾ ")
                  (t "▸ ")))
    (when key
      (insert (propertize key 'face (if (equal "index" (plist-get line :via))
                                        'replique-note
                                      'replique-inspect-key))
              "  "))
    (insert (propertize (or (plist-get line :value) "")
                        'face (cond (gone 'replique-inspect-gone)
                                    ((eq t (plist-get line :changed))
                                     'replique-inspect-changed))))
    (when (eq t (plist-get line :truncated))
      (insert (propertize "…" 'face 'replique-note)))
    (when-let* ((said (replique-inspect--annotation line)))
      (insert "  " (propertize said 'face 'replique-note)))
    (when gone
      (insert "  " (propertize "no longer there" 'face 'replique-note)))
    (insert "\n")
    (put-text-property start (point) 'replique-inspect-node id)))

(defun replique-inspect--insert-node (id depth)
  "Write the node ID at DEPTH, and what is open below it."
  (let ((node (replique-inspect--node id)))
    (replique-inspect--insert-line id depth)
    (when (plist-get node :open)
      (dolist (child (plist-get node :children))
        (replique-inspect--insert-node child (1+ depth)))
      (when (plist-get node :more)
        (let ((start (point))
              (total (plist-get node :total))
              (shown (length (plist-get node :children))))
          (insert (make-string (* 2 (1+ depth)) ?\s) "  "
                  (propertize (if total
                                  (format "… %s more" (- total shown))
                                "… more")
                              'face 'replique-note)
                  "\n")
          (put-text-property start (point) 'replique-inspect-more id))))))

(defun replique-inspect--at-point ()
  "Return what point is on: (node ID) or (more ID), or nil."
  (cond
   ((get-text-property (point) 'replique-inspect-more)
    (list 'more (get-text-property (point) 'replique-inspect-more)))
   ((get-text-property (point) 'replique-inspect-node)
    (list 'node (get-text-property (point) 'replique-inspect-node)))))

(defun replique-inspect--render ()
  "Write what the buffer shows, leaving point on the line it was on."
  (let* ((inhibit-read-only t)
         (at (replique-inspect--at-point))
         (column (current-column))
         (window (get-buffer-window (current-buffer)))
         (start (when window (window-start window))))
    (erase-buffer)
    (cond
     (replique-inspect--error
      (insert (propertize replique-inspect--error 'face 'replique-exception) "\n"))
     ((null replique-inspect--nodes)
      (insert (propertize "…\n" 'face 'replique-note)))
     (t
      (replique-inspect--insert-node (or (car replique-inspect--focus) 0) 0)))
    (goto-char (point-min))
    (when at
      (let ((found (text-property-search-forward
                    (if (eq 'more (car at)) 'replique-inspect-more 'replique-inspect-node)
                    (cadr at) #'eql)))
        (if found
            (progn (goto-char (prop-match-beginning found))
                   (move-to-column column))
          (goto-char (point-min)))))
    (when (and window start (window-live-p window))
      (set-window-start window (min start (point-max)) t))
    (force-mode-line-update)))

(defun replique-inspect--header ()
  "Return the header line of the buffer."
  (let* ((history replique-inspect--history)
         (at (plist-get history :at))
         (count (plist-get history :count)))
    (string-join
     (delq nil
           (list (propertize (or replique-inspect--title "") 'face 'bold)
                 (cond ((null history) nil)
                       (at (propertize (format "value %s of %s" (1+ at) count)
                                       'face 'replique-exception))
                       ((> count 1) (format "live, %s values back" (1- count)))
                       (t "live"))
                 (when replique-inspect--focus
                   (format "in %s"
                           (mapconcat (lambda (key) (format "%s" (cadr key)))
                                      (replique-inspect--key-path
                                       (car replique-inspect--focus))
                                      " ")))
                 (when replique-inspect--meta "metadata shown")
                 (when replique-inspect--stale
                   (propertize "changed - g to see it" 'face 'replique-exception))
                 (when (and replique-inspect--busy (not replique-inspect--stale))
                   (propertize "asking…" 'face 'replique-note))))
     "  ·  ")))

;;; Being told it changed

(defun replique-inspect--buffer-of (process view)
  "Return the buffer showing VIEW of PROCESS, or nil."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'replique-inspect-mode)
                     (eq process replique-inspect--process)
                     (equal view replique-inspect--view))))
            (buffer-list)))

(defun replique-inspect--changed (process frame)
  "Handle FRAME, which says a view of PROCESS changed.

Marked as out of date and nothing more: what it changed to is shown when
it is asked for, with \\`g'."
  (when (equal "inspect-changed" (plist-get frame :event))
    (when-let* ((buffer (replique-inspect--buffer-of process (plist-get frame :view))))
      (with-current-buffer buffer
        (setq replique-inspect--stale t)
        (force-mode-line-update)))))

(add-hook 'replique-process-event-functions #'replique-inspect--changed)

;;; The commands of the buffer

(defun replique-inspect--node-at-point ()
  "Return the node point is on, or signal that it is on none."
  (or (get-text-property (point) 'replique-inspect-node)
      (user-error "Not on a line of the value")))

(defun replique-inspect--load (id offset then)
  "Ask for the children of ID from OFFSET, and call THEN once they are kept."
  (replique-inspect--ask
   (list :op :inspect-children :view replique-inspect--view :node id
         :offset offset :limit replique-inspect-page-size
         :width (replique-inspect--width
                 (1+ (or (plist-get (replique-inspect--node id) :depth) 0)))
         :meta replique-inspect--meta)
   (lambda (frame)
     (cond
      ((replique-inspect--gone-p frame) (replique-inspect--open))
      ((equal "error" (plist-get frame :tag)) (replique-inspect--failed frame))
      (t (replique-inspect--store id frame offset)
         (funcall then))))))

(defun replique-inspect-toggle ()
  "Open the node point is on, or close it.

On the \"more\" line of a node, load more of it."
  (interactive)
  (pcase (replique-inspect--at-point)
    (`(more ,id)
     (replique-inspect--load id (length (plist-get (replique-inspect--node id) :children))
                             #'replique-inspect--render))
    (`(node ,id)
     (let ((node (replique-inspect--node id)))
       (cond
        ((not (plist-get (plist-get node :line) :expandable))
         (message "replique: there is nothing more in it than this line"))
        ((plist-get node :open)
         (replique-inspect--set id :open nil)
         (replique-inspect--render))
        ((plist-get node :children)
         (replique-inspect--set id :open t)
         (replique-inspect--render))
        (t (replique-inspect--load id 0
                                   (lambda ()
                                     (replique-inspect--set id :open t)
                                     (replique-inspect--render)))))))
    (_ (user-error "Not on a line of the value"))))

(defun replique-inspect-focus ()
  "Show only the node point is on, and what is in it.

On a line with nothing more in it, show the value it holds whole instead -
see `replique-inspect-print'."
  (interactive)
  (pcase (replique-inspect--at-point)
    (`(more ,_) (replique-inspect-toggle))
    (`(node ,id)
     (let ((node (replique-inspect--node id)))
       (if (not (plist-get (plist-get node :line) :expandable))
           (replique-inspect-print)
         (let ((show (lambda ()
                       (replique-inspect--set id :open t)
                       (push id replique-inspect--focus)
                       (replique-inspect--render)
                       (goto-char (point-min)))))
           (if (plist-get node :children)
               (funcall show)
             (replique-inspect--load id 0 show))))))
    (_ (user-error "Not on a line of the value"))))

(defun replique-inspect-up ()
  "Show what the node shown was narrowed from."
  (interactive)
  (unless replique-inspect--focus
    (user-error "The whole value is shown"))
  (let ((was (pop replique-inspect--focus)))
    (replique-inspect--render)
    (when-let* ((found (save-excursion
                         (goto-char (point-min))
                         (text-property-search-forward 'replique-inspect-node was #'eql))))
      (goto-char (prop-match-beginning found)))))

(defun replique-inspect-print ()
  "Show the value of the node point is on, printed whole and laid out."
  (interactive)
  (let ((id (replique-inspect--node-at-point))
        (title replique-inspect--title))
    (replique-inspect--ask
     (append (list :op :inspect-print :view replique-inspect--view :node id)
             (when replique-inspect--meta (list :meta t)))
     (lambda (frame)
       (cond
        ((replique-inspect--gone-p frame) (replique-inspect--open))
        ((equal "error" (plist-get frame :tag)) (replique-inspect--failed frame))
        ((plist-get frame :gone) (message "replique: it is no longer there"))
        (t
         (let ((text (plist-get frame :value))
               (buffer (get-buffer-create "*replique-value*")))
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (condition-case nil
                           (replique-pprint-string text)
                         (error text)))
               (unless (eq t (plist-get frame :whole))
                 (insert (propertize "\n… cut here, it goes on\n" 'face 'replique-note)))
               (goto-char (point-min)))
             (unless (derived-mode-p 'special-mode) (special-mode))
             (setq header-line-format (format "%s, printed whole" title)))
           (pop-to-buffer buffer))))))))

(defun replique-inspect-copy (&optional value)
  "Copy the code that reaches the node point is on.

Which is the path through the value where every key on the way reads
back, and the call that fetches it out of the view where one does not -
or where VALUE, interactively the prefix argument, says so.  The call
works for as long as the view is open."
  (interactive "P")
  (let ((id (replique-inspect--node-at-point)))
    (replique-inspect--ask
     (list :op :inspect-path :view replique-inspect--view :node id)
     (lambda (frame)
       (cond
        ((replique-inspect--gone-p frame) (replique-inspect--open))
        ((equal "error" (plist-get frame :tag)) (replique-inspect--failed frame))
        (t
         (let ((code (or (and (not value) (plist-get frame :code))
                         (plist-get frame :value))))
           (kill-new code)
           (message "replique: copied %s" code))))))))

(defun replique-inspect--go (at)
  "Show the value the history holds at AT, the live one where it is nil."
  (unless replique-inspect--history
    (user-error "No history is kept of this value"))
  (setq replique-inspect--history
        (plist-put (copy-sequence replique-inspect--history) :at at))
  (replique-inspect-refresh))

(defun replique-inspect-older ()
  "Show the value it held before the one shown."
  (interactive)
  (let* ((count (or (plist-get replique-inspect--history :count) 0))
         (at (or (plist-get replique-inspect--history :at) (1- count))))
    (if (<= at 0)
        (user-error "This is the oldest value kept")
      (replique-inspect--go (1- at)))))

(defun replique-inspect-newer ()
  "Show the value it held after the one shown, and the live one after that."
  (interactive)
  (let ((count (or (plist-get replique-inspect--history :count) 0))
        (at (plist-get replique-inspect--history :at)))
    (cond
     ((null at) (user-error "This is the live value"))
     ((>= (1+ at) (1- count)) (replique-inspect--go nil))
     (t (replique-inspect--go (1+ at))))))

(defun replique-inspect-live ()
  "Show the value it holds now."
  (interactive)
  (replique-inspect--go nil))

(defun replique-inspect-toggle-meta ()
  "Show metadata as the first child of what has some, or stop showing it."
  (interactive)
  (setq replique-inspect--meta (not replique-inspect--meta))
  (dolist (id (hash-table-keys (or replique-inspect--nodes (make-hash-table))))
    (unless (plist-get (replique-inspect--node id) :open)
      (replique-inspect--set id :children nil)))
  (replique-inspect-refresh))

(defun replique-inspect--close ()
  "Let the process go of the view of the buffer being killed."
  (when (and replique-inspect--view
             (replique-process-live-p replique-inspect--process))
    (ignore-errors
      (replique-process-request replique-inspect--process
                                (list :op :inspect-close :view replique-inspect--view)))))

(defvar replique-inspect-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'replique-inspect-toggle)
    (define-key map (kbd "<tab>") #'replique-inspect-toggle)
    (define-key map (kbd "RET") #'replique-inspect-focus)
    (define-key map (kbd "^") #'replique-inspect-up)
    (define-key map (kbd "l") #'replique-inspect-up)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "g") #'replique-inspect-refresh)
    (define-key map (kbd "[") #'replique-inspect-older)
    (define-key map (kbd "]") #'replique-inspect-newer)
    (define-key map (kbd ".") #'replique-inspect-live)
    (define-key map (kbd "P") #'replique-inspect-print)
    (define-key map (kbd "w") #'replique-inspect-copy)
    (define-key map (kbd "m") #'replique-inspect-toggle-meta)
    map)
  "Keymap of a buffer showing an inspected value.")

(define-derived-mode replique-inspect-mode special-mode "Replique-Inspect"
  "Major mode for browsing a value of a replique process.

\\{replique-inspect-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (replique-inspect-refresh)))
  (setq-local header-line-format '(:eval (replique-inspect--header)))
  (add-hook 'kill-buffer-hook #'replique-inspect--close nil t))

;;; Opening a view

(defun replique-inspect-show (process keys source title)
  "Show the value SOURCE names, in PROCESS, in a buffer of its own.

KEYS are the dialect keys - see `replique-dialect-keys'.  TITLE is what the
buffer is a view of, in a few words.  A view of the same thing already
shown is shown again, and refreshed."
  (let* ((name (format "*replique-inspect %s*" title))
         (existing (seq-find (lambda (buffer)
                               (with-current-buffer buffer
                                 (and (derived-mode-p 'replique-inspect-mode)
                                      (equal source replique-inspect--source)
                                      (equal keys replique-inspect--keys)
                                      (eq process replique-inspect--process))))
                             (buffer-list)))
         (buffer (or existing (generate-new-buffer name))))
    (with-current-buffer buffer
      (if existing
          (replique-inspect-refresh)
        (replique-inspect-mode)
        (setq replique-inspect--process process
              replique-inspect--keys keys
              replique-inspect--source source
              replique-inspect--title title)
        (replique-inspect--render)
        (replique-inspect--open)))
    (pop-to-buffer buffer)
    buffer))

(defun replique-inspect--var-at-point ()
  "Return the var the name at point is, written in full, or nil."
  (when-let* ((bounds (replique-name-at-point))
              (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
              (context (replique-name-context))
              (found (replique-symbol--ask context text))
              (ns (plist-get found :ns))
              ((not (member (plist-get found :type)
                            '("keyword" "namespace" "class" "special-form")))))
    (format "%s/%s" ns (plist-get found :name))))

(defun replique-inspect--read-var (process)
  "Read the name of a var of PROCESS, offering the vars of this namespace."
  (let* ((default (ignore-errors (replique-inspect--var-at-point)))
         (ns (replique-name-namespace))
         (names (when ns
                  (mapcar (lambda (var) (format "%s/%s" ns (plist-get var :name)))
                          (ignore-errors (replique-symbol--vars process ns))))))
    (completing-read (format-prompt "Watch var" default) names nil nil nil nil default)))

;;;###autoload
(defun replique-watch (var)
  "Show the value of VAR, and say when it changes.

What VAR holds is shown, and where that is an atom - or any reference -
what the atom holds: a var holding an atom is a view of the atom, and is
watched through both, since defining the var again is a change as much
as swapping the atom is.  The buffer says when it changed, and \\`g'
shows what it changed to - see `replique-inspect-mode'.

Asked about in the language of this buffer: in a ClojureScript buffer it
is a var of the program the ClojureScript repl is running.  A name that
is not written in full is a var of the namespace this buffer is in."
  (interactive
   (let ((process (or (replique-name-process)
                      (user-error "No process - M-x replique-connect"))))
     (list (replique-inspect--read-var process))))
  (let* ((process (or (replique-name-process)
                      (user-error "No process - M-x replique-connect")))
         (written (string-trim (substring-no-properties var)))
         (name (cond ((string-empty-p written) (user-error "No var"))
                     ((string-match-p "/" written) written)
                     (t (format "%s/%s"
                                (or (replique-name-namespace)
                                    (user-error "Nothing here says which namespace %s is in"
                                                written))
                                written))))
         (keys (replique-dialect-keys)))
    (replique-inspect-show process keys (list :var name) name)))

;;;###autoload
(defun replique-inspect-results ()
  "Show what the current repl returned last: *1, *2 and *3.

Each value a form returns there is the next *1, and \\`g' shows it."
  (interactive)
  (let* ((repl (replique-repl-ensure))
         (process (replique-repl-process repl))
         (keys (replique-repl-dialect-keys repl))
         (source (if keys
                     ;; A ClojureScript repl's *1 is the runtime's, which is
                     ;; one program for every repl on it
                     (list :results t)
                   (list :results (plist-get (replique-conn--info (replique-repl--conn repl))
                                             :connection)))))
    (replique-inspect-show process keys source
                           (format "results of %s" (buffer-name (replique-repl--buffer repl))))))

;;;###autoload
(defun replique-taps ()
  "Show what was given to `tap>' last, newest last.

In the language of this buffer: what a ClojureScript program tapped is in
that program, and is shown from a ClojureScript buffer or repl."
  (interactive)
  (let ((process (or (replique-name-process)
                     (user-error "No process - M-x replique-connect")))
        (keys (replique-dialect-keys)))
    (replique-inspect-show process keys (list :taps t)
                           (if keys
                               (format "taps of %s"
                                       (string-remove-prefix
                                        ":" (format "%s" (or (plist-get keys :target)
                                                             "ClojureScript"))))
                             "taps"))))

(provide 'replique-inspect)

;;; replique-inspect.el ends here
