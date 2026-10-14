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

;; A value of the process, shown the way it prints - and opened and closed
;; a part at a time.  What a repl prints of a value is cut by *print-length*
;; and *print-level* all at once, and what was cut is gone; here nothing is
;; cut, it is only not asked for yet.  The value stays in the process - in the
;; runtime, for ClojureScript - and what comes over is a short printing of
;; each child, a page at a time.
;;
;; Laid out as a pretty printer lays it out: an open map an entry a line,
;; keys first, and what holds only what fits filled into as few lines as it
;; takes.  A part that is closed is its printing, cut to fit on its line, and
;; \`TAB' opens it in place, between the brackets it prints between.  What
;; *print-length* would leave out is \`…N more', to load, and what
;; *print-level* would is a part still closed.  A view is opened as far as a
;; window shows, first levels first.  What a part is - its type, how many
;; things it holds, where it is in the value - is said by \`i'.
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
;; A seq of maps - rows from a database, the iterations of a loop recorded -
;; is shown as a table: a row per map, a column per key, the keys first.  A
;; cell is the thing its map holds, closed, and \`TAB' or \`RET' browses it
;; on its own.  \`T' shows it the way it prints instead.
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

(defcustom replique-inspect-cell-width 30
  "How many chars a cell of a table is shown in, at most.
A seq of maps is shown as a table, a row per map and a column per key - see
`replique-inspect-toggle-tables'."
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
  "Face for what a line of an inspected value is under, other than a key.
A property of an object, @ for what a reference holds and ^ for metadata:
the key of a map is painted as the value it is, and an index as a note."
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
`:children' ids, whether there are `:more' and how many in `:total', the
`:delimiters' it is written between once open, as (OPEN . CLOSE), the
`:column' and the position, `:at', its value was last written at, its
`:parent', and whether it is `:gone'.")

(defvar-local replique-inspect--history nil
  "Where the history of a watched var is: (:count N) live, (:count N :at I)
showing an older value.  Nil where none is kept.")

(defvar-local replique-inspect--plain nil
  "Non-nil when a seq of maps is shown the way it prints, not as a table.")

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

(defun replique-inspect--width ()
  "Return how wide a line of the buffer can be."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 40 (1- (if window (window-body-width window) 100)))))

(defun replique-inspect--budget ()
  "Return how many lines a view is opened as far as: a window of them."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 10 (- (if window (window-body-height window) 40) 2))))

(defun replique-inspect--where (id)
  "Return where the node ID is written once open, as the ops take it.

Which is at the start of its entry's line, after its key: an open child
breaks the line its parent was written on, where it was on one."
  (let* ((node (replique-inspect--node id))
         (parent (plist-get node :parent))
         (column (plist-get (replique-inspect--node parent) :column)))
    (list :width (replique-inspect--width)
          :column (if (and parent column (not (memql id replique-inspect--focus)))
                      (+ column
                         (length (car (plist-get (replique-inspect--node parent) :delimiters)))
                         (length (replique-inspect--label id)))
                    (or (plist-get node :column) 0)))))

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
  (and replique-inspect--nodes (gethash id replique-inspect--nodes)))

(defun replique-inspect--set (id &rest props)
  "Set PROPS on the node ID."
  (let ((node (gethash id replique-inspect--nodes)))
    (while props
      (setq node (plist-put node (pop props) (pop props))))
    (puthash id node replique-inspect--nodes)))

(defun replique-inspect--store (parent frame offset)
  "Keep the children FRAME holds, the page of PARENT's from OFFSET.

A child the process opened has its page as `:page', kept as well, and is
open."
  (if (plist-get frame :gone)
      (replique-inspect--set parent :gone t :children nil :open nil)
    (let ((ids (mapcar (lambda (line)
                         (let ((id (plist-get line :node))
                               (page (plist-get line :page)))
                           (replique-inspect--set id :line line :parent parent :gone nil)
                           ;; a row of a table: what it holds, by key
                           (when-let* ((cells (plist-get line :cells)))
                             (replique-inspect--set
                              id :cells
                              (mapcar (lambda (cell)
                                        (let ((cell-id (plist-get cell :node)))
                                          (replique-inspect--set cell-id :line cell :parent id
                                                                 :gone nil :cell t)
                                          (cons (plist-get cell :key) cell-id)))
                                      cells)))
                           (when page
                             (replique-inspect--store id page 0)
                             (replique-inspect--set id :open t))
                           id))
                       (plist-get frame :children))))
      (replique-inspect--set
       parent
       :children (if (> offset 0)
                     (append (plist-get (replique-inspect--node parent) :children) ids)
                   ids)
       :more (eq t (plist-get frame :more))
       :total (plist-get frame :total)
       ;; the keys of the rows of a table, of every page loaded
       :columns (when-let* ((columns (append (plist-get frame :columns) nil)))
                  (seq-uniq (append (when (> offset 0)
                                      (plist-get (replique-inspect--node parent) :columns))
                                    columns)))
       :delimiters (cons (or (plist-get frame :open) "") (or (plist-get frame :close) ""))
       :gone nil))))

(defun replique-inspect--shown (frame)
  "Keep what FRAME, the answer to an opening or a refresh, shows."
  (let ((root (plist-get frame :root)))
    (replique-inspect--set 0 :line root :open t :gone nil)
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
      (setq found (plist-get (replique-inspect--node id) :parent))
      (when found
        (let ((line (plist-get (replique-inspect--node id) :line)))
          (push (list (plist-get line :via) (or (plist-get line :key) (plist-get line :value)))
                path))
        (setq id found)))
    (when found path)))

(defun replique-inspect--child-by-key (parent key)
  "Return the child of PARENT called KEY - see `replique-inspect--key-path'."
  (let ((node (replique-inspect--node parent)))
    (seq-find (lambda (id)
                (let ((line (plist-get (replique-inspect--node id) :line)))
                  (equal key (list (plist-get line :via)
                                   (or (plist-get line :key) (plist-get line :value))))))
              ;; a cell of a row of a table too
              (append (plist-get node :children) (mapcar #'cdr (plist-get node :cells))))))

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
                   :width (replique-inspect--width)
                   :column 0
                   :limit replique-inspect-page-size)
             ;; opened as far as a window goes the first time, and as it was
             ;; left after that
             (unless paths (list :expand (replique-inspect--budget)))
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
         (append (list :op :inspect-children :view replique-inspect--view :node id
                       :limit replique-inspect-page-size
                       :meta replique-inspect--meta)
                 (replique-inspect--where id))
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
                     :width (replique-inspect--width)
                     :column 0
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
       (append (list :op :inspect-children :view replique-inspect--view :node id
                     :limit (max replique-inspect-page-size (length (plist-get node :children)))
                     :meta replique-inspect--meta)
               (replique-inspect--where id))
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

(defvar replique-inspect--painted (make-hash-table :test #'equal)
  "The printings of values already painted, each to the copy painted.

Shared by every view: a view is written again whole each time something
in it changes, and most of what it shows was painted the time before.")

(defun replique-inspect--painted (printing)
  "Return a copy of PRINTING painted as Clojure, to be written in a view."
  (or (when-let* ((painted (gethash printing replique-inspect--painted)))
        (copy-sequence painted))
      (progn
        (when (> (hash-table-count replique-inspect--painted) 5000)
          (clrhash replique-inspect--painted))
        (let ((painted (replique-clojure-fontify-string printing)))
          (puthash printing painted replique-inspect--painted)
          (copy-sequence painted)))))

(defun replique-inspect--annotation (line)
  "Return what LINE is: its kind, its type and how many things it holds."
  (string-join (delq nil (list (plist-get line :kind)
                               (plist-get line :type)
                               (when-let* ((count (plist-get line :count)))
                                 (format "%s held" count))))
               " · "))

(defun replique-inspect--part (text id &optional face)
  "Return TEXT as a part of the node ID, with FACE added where non-nil."
  (let ((text (copy-sequence text)))
    (when face (add-face-text-property 0 (length text) face t text))
    (put-text-property 0 (length text) 'replique-inspect-node id text)
    text))

(defun replique-inspect--open-p (id)
  "Return non-nil when the node ID is written open, between its delimiters."
  (let ((node (replique-inspect--node id)))
    (and (plist-get node :open)
         (plist-get node :delimiters)
         (not (plist-get node :gone))
         (plist-get (plist-get node :line) :expandable))))

(defun replique-inspect--closed (id)
  "Return the node ID written closed: its printing, on one line."
  (let* ((node (replique-inspect--node id))
         (line (plist-get node :line))
         (value (replique-inspect--painted (or (plist-get line :value) ""))))
    (cond ((plist-get node :gone)
           (add-face-text-property 0 (length value) 'replique-inspect-gone nil value))
          ((eq t (plist-get line :changed))
           (add-face-text-property 0 (length value) 'replique-inspect-changed t value)))
    ;; cut at the end of the line, rather than shortened the way a printing
    ;; under *print-length* and *print-level* is
    (when (eq t (plist-get line :cut))
      (setq value (concat value (propertize "…" 'face 'replique-note))))
    (replique-inspect--part value id)))

(defun replique-inspect--label (id)
  "Return what the node ID is written after: its key in a map, or nil."
  (let* ((line (plist-get (replique-inspect--node id) :line))
         (key (plist-get line :key)))
    (pcase (plist-get line :via)
      ;; a key of a map is a value, painted as one
      ("key" (replique-inspect--part (concat (replique-inspect--painted key) " ") id))
      ;; a property of a JavaScript object, the way #js {} prints it
      ("prop" (replique-inspect--part
               (concat (replique-inspect--painted
                        (if (string-match-p "\\`[[:alpha:]_$*+!?<>=-][[:alnum:]_$*+!?<>=.-]*\\'" key)
                            (concat ":" key)
                          (prin1-to-string key)))
                       " ")
               id))
      ("meta" (replique-inspect--part "^" id 'replique-inspect-key)))))

(defun replique-inspect--more (id)
  "Return the entry saying there are more children of the node ID, to load."
  (let* ((node (replique-inspect--node id))
         (total (plist-get node :total)))
    (propertize (if total
                    (format "…%s more" (- total (length (plist-get node :children))))
                  "…more")
                'face 'replique-note
                'replique-inspect-more id)))

(defun replique-inspect--insert-value (id)
  "Write the node ID at point, open or closed.

What is open in it is laid out the way it prints."
  (replique-inspect--set id :column (current-column) :at (point))
  (cond
   ((not (replique-inspect--open-p id)) (insert (replique-inspect--closed id)))
   ((replique-inspect--table-p id) (replique-inspect--insert-table id))
   (t (replique-inspect--insert-open id))))

(defun replique-inspect--table-p (id)
  "Return non-nil when the node ID, open, is written as a table."
  (and (not replique-inspect--plain)
       (plist-get (replique-inspect--node id) :columns)))

(defun replique-inspect--in-table-p (id)
  "Return non-nil when the node ID is shown as a cell of a table.
A cell is the child of its row by its key, the same node - which, the rows
laid out the way they print, is a part like any other."
  (when-let* ((row (and (plist-get (replique-inspect--node id) :cell)
                        (plist-get (replique-inspect--node id) :parent))))
    (replique-inspect--table-p (plist-get (replique-inspect--node row) :parent))))

(defun replique-inspect--cell (text width id &optional last)
  "Return TEXT, part of a cell of the row ID, in WIDTH chars: cut, or padded.
Not padded where it is the LAST cell of its row."
  (let ((text (if (> (string-width text) width)
                  (concat (truncate-string-to-width text (1- width))
                          (replique-inspect--part (propertize "…" 'face 'replique-note) id))
                text)))
    (if last
        text
      (concat text (replique-inspect--part (make-string (- width (string-width text)) ?\s) id)))))

(defun replique-inspect--insert-table (id)
  "Write the node ID, a seq of maps, as a table between its delimiters.

A row per map, and a column per key - the keys first, on the line of the
opening delimiter.  Each cell is the thing the map holds under that key,
closed, and a map that holds nothing under it has nothing there."
  (let* ((node (replique-inspect--node id))
         (delimiters (plist-get node :delimiters))
         (face (when (eq t (plist-get (plist-get node :line) :changed))
                 'replique-inspect-changed))
         (columns (plist-get node :columns))
         (rows (plist-get node :children))
         (texts (mapcar (lambda (row)
                          (let ((cells (plist-get (replique-inspect--node row) :cells)))
                            (cons row (mapcar (lambda (column)
                                                (when-let* ((cell (cdr (assoc column cells))))
                                                  (cons cell (replique-inspect--closed cell))))
                                              columns))))
                        rows))
         (widths (seq-map-indexed
                  (lambda (column i)
                    (min replique-inspect-cell-width
                         (apply #'max (string-width column)
                                (mapcar (lambda (row)
                                          (if-let* ((cell (nth i (cdr row))))
                                              (string-width (cdr cell))
                                            0))
                                        texts))))
                  columns)))
    (insert (replique-inspect--part (car delimiters) id face))
    (let ((base (current-column))
          (first t))
      (cl-mapc (lambda (column width i)
                 (unless first (insert (replique-inspect--part "  " id)))
                 (setq first nil)
                 (insert (replique-inspect--cell
                          (replique-inspect--part (replique-inspect--painted column) id
                                                  'bold)
                          width id (= i (1- (length columns))))))
               columns widths (number-sequence 0 (1- (length columns))))
      (dolist (row texts)
        (insert (replique-inspect--part "\n" (car row))
                (replique-inspect--part (make-string base ?\s) (car row)))
        (replique-inspect--set (car row) :column (current-column) :at (point))
        (let ((first t))
          ;; up to the last cell the row has, padded
          (let ((cells (cdr row)))
            (while (and cells (null (car (last cells))))
              (setq cells (butlast cells)))
            (cl-mapc (lambda (cell width i)
                       (unless first (insert (replique-inspect--part "  " (car row))))
                       (setq first nil)
                       (when cell
                         (replique-inspect--set (car cell) :column (current-column) :at (point)))
                       (insert (replique-inspect--cell (if cell (cdr cell) "") width (car row)
                                                       (= i (1- (length cells))))))
                     cells widths (number-sequence 0 (1- (length cells)))))))
      (when (plist-get node :more)
        (insert (replique-inspect--part "\n" id)
                (replique-inspect--part (make-string base ?\s) id)
                (replique-inspect--more id))))
    (insert (replique-inspect--part (cdr delimiters) id face))))

(defun replique-inspect--insert-open (id)
  "Write the node ID open: its delimiters, and its children between them.

On one line where they all fit on it.  Otherwise an entry a line, for a map
and for what holds things - and filled into as few lines as they take, for
what holds only things that hold nothing, numbers, strings."
  (let* ((node (replique-inspect--node id))
         (delimiters (plist-get node :delimiters))
         (face (when (eq t (plist-get (plist-get node :line) :changed))
                 'replique-inspect-changed))
         (width (replique-inspect--width))
         (keyed (seq-some (lambda (child)
                            (member (plist-get (plist-get (replique-inspect--node child) :line) :via)
                                    '("key" "prop")))
                          (plist-get node :children)))
         (flat (seq-every-p (lambda (child)
                              (let ((line (plist-get (replique-inspect--node child) :line)))
                                (not (or (plist-get line :count)
                                         (plist-get line :expandable)))))
                            (plist-get node :children)))
         ;; (ID . TEXT), TEXT being the entry written closed, or nil where it
         ;; is open; and (nil . TEXT) for saying there are more
         (entries (append (mapcar (lambda (child)
                                    (cons child (unless (replique-inspect--open-p child)
                                                  (concat (replique-inspect--label child)
                                                          (replique-inspect--closed child)))))
                                  (plist-get node :children))
                          (when (plist-get node :more)
                            (list (cons nil (replique-inspect--more id))))))
         (separator (if keyed ", " " ")))
    (insert (replique-inspect--part (car delimiters) id face))
    (let* ((base (current-column))
           (break (lambda (before next)
                    ;; a new line, indented as the first entry: the end of
                    ;; the line a part of the entry it ends, and the
                    ;; indentation of the one it starts
                    (insert (replique-inspect--part "\n" (or before id))
                            (replique-inspect--part (make-string base ?\s) (or next id)))))
           (one-line (and (seq-every-p #'cdr entries)
                          (<= (+ base
                                 (apply #'+ (mapcar (lambda (entry) (length (cdr entry))) entries))
                                 (* (length separator) (max 0 (1- (length entries))))
                                 (length (cdr delimiters)))
                              width))))
      (cond
       (one-line
        (let ((first t))
          (dolist (entry entries)
            (unless first
              (insert (replique-inspect--part separator (or (car entry) id))))
            (setq first nil)
            (replique-inspect--insert-entry entry))))
       ((or keyed (not flat))
        (let ((before nil))
          (dolist (entry entries)
            (when before
              (when keyed (insert (replique-inspect--part "," (car before))))
              (funcall break (car before) (car entry)))
            (setq before entry)
            (replique-inspect--insert-entry entry))))
       (t
        (let ((before nil))
          (dolist (entry entries)
            (when before
              (if (> (+ (current-column) 1 (length (cdr entry))) (1- width))
                  (funcall break (car before) (car entry))
                (insert (replique-inspect--part " " (or (car entry) id)))))
            (setq before entry)
            (replique-inspect--insert-entry entry))))))
    (insert (replique-inspect--part (cdr delimiters) id face))))

(defun replique-inspect--insert-entry (entry)
  "Write ENTRY of `replique-inspect--insert-open' at point."
  (let ((id (car entry)))
    (cond
     ((null id) (insert (cdr entry)))
     ((cdr entry)
      (let ((label (length (replique-inspect--label id))))
        (replique-inspect--set id :column (+ (current-column) label) :at (+ (point) label)))
      (insert (cdr entry)))
     (t
      (when-let* ((label (replique-inspect--label id)))
        (insert label))
      (replique-inspect--insert-value id)))))

(defun replique-inspect--at-point ()
  "Return what point is on: (node ID) or (more ID), or nil."
  (cond
   ((get-text-property (point) 'replique-inspect-more)
    (list 'more (get-text-property (point) 'replique-inspect-more)))
   ((get-text-property (point) 'replique-inspect-node)
    (list 'node (get-text-property (point) 'replique-inspect-node)))))

(defun replique-inspect--anchor (at)
  "Return where AT, what `replique-inspect--at-point' returns, is written.

The start of the value of a node, after its key, or of the entry saying
there are more."
  (pcase at
    (`(node ,id) (plist-get (replique-inspect--node id) :at))
    (`(more ,id) (save-excursion
                   (goto-char (point-min))
                   (when-let* ((found (text-property-search-forward
                                       'replique-inspect-more id #'eql)))
                     (prop-match-beginning found))))))

(defun replique-inspect--on-p (pos at)
  "Return non-nil when POS is on AT, what `replique-inspect--at-point' returns."
  (and (<= (point-min) pos) (< pos (point-max))
       (eql (cadr at) (get-text-property pos (if (eq 'more (car at))
                                                  'replique-inspect-more
                                                'replique-inspect-node)))))

(defun replique-inspect--render (&optional on)
  "Write what the buffer shows, keeping point where it was.

On the part it was on: as far into it as it was, where that is still in
it, and at its start otherwise.  And on the same row of the window, so that
what is around it moves rather than it.  With ON, a node, at the start of
that node instead."
  (let* ((inhibit-read-only t)
         (window (get-buffer-window (current-buffer)))
         (_ (when window (goto-char (window-point window))))
         (at (replique-inspect--at-point))
         (anchor (replique-inspect--anchor at))
         (offset (when anchor (- (point) anchor)))
         (row (when (and window (>= (point) (window-start window)))
                (let ((row (count-lines (window-start window) (line-beginning-position))))
                  (when (< row (window-body-height window)) row)))))
    (erase-buffer)
    (cond
     (replique-inspect--error
      (insert (propertize replique-inspect--error 'face 'replique-exception) "\n"))
     ((null replique-inspect--nodes)
      (insert (propertize "…\n" 'face 'replique-note)))
     (t
      (replique-inspect--insert-value (or (car replique-inspect--focus) 0))
      (insert "\n")))
    (goto-char (or (if on
                       (replique-inspect--anchor (list 'node on))
                     (when-let* ((anchor (and at (replique-inspect--anchor at))))
                       (if (replique-inspect--on-p (+ anchor offset) at)
                           (+ anchor offset)
                         anchor)))
                   (point-min)))
    (when (and window (window-live-p window))
      (set-window-point window (point))
      (when row
        (set-window-start window (save-excursion (forward-line (- row)) (point)) t)))
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
      (user-error "Not on a part of the value")))

(defun replique-inspect--load (id offset then &optional expand at-edge)
  "Ask for the children of ID from OFFSET, and call THEN once they are kept.

With EXPAND, the children are opened as far as that many lines.  With
AT-EDGE, ID is to be written at the left edge, rather than where it is."
  (replique-inspect--ask
   (append (list :op :inspect-children :view replique-inspect--view :node id
                 :offset offset :limit replique-inspect-page-size
                 :meta replique-inspect--meta)
           (if at-edge
               (list :width (replique-inspect--width) :column 0)
             (replique-inspect--where id))
           (when expand (list :expand expand)))
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
     (let ((loaded (length (plist-get (replique-inspect--node id) :children))))
       (replique-inspect--load id loaded
                               (lambda ()
                                 ;; on the first of what was loaded
                                 (replique-inspect--render
                                  (nth loaded (plist-get (replique-inspect--node id) :children)))))))
    (`(node ,id)
     (let ((node (replique-inspect--node id)))
       (cond
        ((not (plist-get (plist-get node :line) :expandable))
         (message "replique: there is nothing more in it than is shown"))
        ;; a cell of a table is opened on its own, the table staying a table
        ((replique-inspect--in-table-p id) (replique-inspect-focus))
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
    (_ (user-error "Not on a part of the value"))))

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
             ;; written at the left edge, and opened as far as a window goes
             (replique-inspect--load id 0 show (replique-inspect--budget) t))))))
    (_ (user-error "Not on a part of the value"))))

(defun replique-inspect-up ()
  "Show what the node shown was narrowed from."
  (interactive)
  (unless replique-inspect--focus
    (user-error "The whole value is shown"))
  (replique-inspect--render (pop replique-inspect--focus)))

(defun replique-inspect-describe ()
  "Say what the part point is on is, and where it is in the value."
  (interactive)
  (let* ((id (replique-inspect--node-at-point))
         (node (replique-inspect--node id))
         (path (replique-inspect--key-path id)))
    (message "%s"
             (string-join
              (delq nil (list (replique-inspect--annotation (plist-get node :line))
                              (when path
                                (format "at %s" (mapconcat (lambda (key) (format "%s" (cadr key)))
                                                           path " ")))
                              (when (plist-get node :gone) "no longer there")))
              "  ·  "))))

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

(defun replique-inspect--default-name (id)
  "Return the name a var holding the node ID would be called, or nil.

What its parent calls it, where that reads as a name: a local of a
frame, an entry of a map whose keys are symbols."
  (let ((key (plist-get (plist-get (replique-inspect--node id) :line) :key)))
    (when (and key (string-match-p "\\`[^][(){}\"#:;@^`~\\', 0-9][^][(){}\"@^`~\\', ]*\\'" key))
      key)))

(defun replique-inspect-def (name)
  "Define the var NAME, holding what the node point is on holds.

A value looked at, kept to work with at the repl - a local of a stopped
thread in particular, which is gone once the thread goes on.  Defined in
the namespace of the code the value comes from - the frame\='s, for the
locals of a frame, the code recorded\='s, for a value recorded, and the
function\='s, for a traced call - and in the namespace of the Clojure repl
otherwise.  Interactively, the name is asked for, what the node is called
being the default."
  (interactive
   (let ((default (replique-inspect--default-name (replique-inspect--node-at-point))))
     (list (read-string (format-prompt "Define var" default) nil nil default))))
  (when (plist-get replique-inspect--keys :dialect)
    (user-error "A value of a ClojureScript runtime is not defined in the process"))
  (when (string-blank-p name) (user-error "A var needs a name"))
  (let ((id (replique-inspect--node-at-point))
        (ns (unless (or (plist-get replique-inspect--source :debug)
                        (plist-get replique-inspect--source :trace))
              (or (when-let* ((repl (replique-repl-for-dialect :clj)))
                    (replique-repl--ns repl))
                  (user-error "No Clojure repl to say which namespace")))))
    (replique-inspect--ask
     (append (list :op :inspect-def :view replique-inspect--view :node id :name name)
             (when ns (list :ns ns)))
     (lambda (frame)
       (cond
        ((replique-inspect--gone-p frame) (replique-inspect--open))
        ((equal "error" (plist-get frame :tag)) (replique-inspect--failed frame))
        (t (message "replique: defined %s" (plist-get frame :var))))))))

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

(defun replique-inspect-toggle-tables ()
  "Show a seq of maps as a table, or the way it prints.
As a table, a row per map and a column per key, the keys on the first line."
  (interactive)
  (setq replique-inspect--plain (not replique-inspect--plain))
  (replique-inspect--render)
  (message "replique: a seq of maps is shown %s"
           (if replique-inspect--plain "the way it prints" "as a table")))

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

(defun replique-inspect-quit (&optional bury)
  "Kill the buffer, and let the process go of the value it shows.

With BURY, interactively the prefix argument, the buffer is only put out
of the way, and the process keeps the value."
  (interactive "P")
  (quit-window (not bury)))

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
    (define-key map (kbd "i") #'replique-inspect-describe)
    (define-key map (kbd "P") #'replique-inspect-print)
    (define-key map (kbd "w") #'replique-inspect-copy)
    (define-key map (kbd "d") #'replique-inspect-def)
    (define-key map (kbd "m") #'replique-inspect-toggle-meta)
    (define-key map (kbd "T") #'replique-inspect-toggle-tables)
    (define-key map (kbd "q") #'replique-inspect-quit)
    map)
  "Keymap of a buffer showing an inspected value.")

(define-derived-mode replique-inspect-mode special-mode "Replique-Inspect"
  "Major mode for browsing a value of a replique process.

\\{replique-inspect-mode-map}"
  (setq-local truncate-lines t)
  ;; what is shown is Clojure, which sexp motion goes through
  (set-syntax-table replique-clojure-mode-syntax-table)
  (setq-local revert-buffer-function (lambda (&rest _) (replique-inspect-refresh)))
  (setq-local header-line-format '(:eval (replique-inspect--header)))
  (add-hook 'kill-buffer-hook #'replique-inspect--close nil t))

;;; Opening a view

(defun replique-inspect-show (process keys source title &optional buffer quiet)
  "Show the value SOURCE names, in PROCESS, in a buffer of its own.

KEYS are the dialect keys - see `replique-dialect-keys'.  TITLE is what the
buffer is a view of, in a few words.  A view of the same thing already
shown is shown again, and refreshed.

With BUFFER, the view is that buffer, made a view of SOURCE whatever it
was a view of before: one buffer that follows something, the way the
view of the locals of a stopped thread follows the frame looked at.
With QUIET, the buffer is not shown: what is shown stays as it is."
  (let* ((name (format "*replique-inspect %s*" title))
         (same (lambda (buffer)
                 (with-current-buffer buffer
                   (and (derived-mode-p 'replique-inspect-mode)
                        (equal source replique-inspect--source)
                        (equal keys replique-inspect--keys)
                        (eq process replique-inspect--process)))))
         (existing (if buffer
                       (and (funcall same buffer) buffer)
                     (seq-find same (buffer-list))))
         (buffer (or existing buffer (generate-new-buffer name))))
    ;; shown first, so that what it asks for fits the window it is in
    (unless quiet (pop-to-buffer buffer))
    (with-current-buffer buffer
      (if existing
          (replique-inspect-refresh)
        ;; what the buffer was a view of is let go of, and what answers it
        ;; is not taken for what answers this
        (let ((generation replique-inspect--generation))
          (replique-inspect--close)
          (replique-inspect-mode)
          (setq replique-inspect--generation (1+ generation)))
        (setq replique-inspect--process process
              replique-inspect--keys keys
              replique-inspect--source source
              replique-inspect--title title)
        (replique-inspect--render)
        (replique-inspect--open)))
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
  "Show what the repl of this buffer returned last: *1, *2 and *3.

The repl the buffer's code goes to - a .clj buffer's Clojure one, a .cljs
buffer's ClojureScript one, a .cljc one's the repl in hand - or, in a
repl buffer, that repl.  Each value a form returns there is the next *1,
and \\`g' shows it."
  (interactive)
  (let* ((repl (replique-repl-ensure-here))
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
