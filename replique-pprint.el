;; replique-pprint.el ---   -*- lexical-binding: t; -*-

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

;; Commentary:

;; Code:

(defvar replique-pprint/max-width 72)
(defvar replique-pprint/miser-width 40)

(defvar replique-pprint/state nil)

(defun replique-pprint/initial-state (lb)
  (replique/hash-map
   :sections nil
   :mode :writing
   :trailing-white-space nil
   :logical-block lb
   :buffer '()
   :buffer-block lb
   :buffer-level 1
   :miser-width replique-pprint/miser-width
   :pos 0))

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique-pprint/buffer-count (buffer)
  (let ((buffer-first (car buffer)))
    (if buffer-first
        (let ((buffer-last (car (last buffer))))
          (- (replique/get buffer-first :end-pos) (replique/get buffer-last :start-pos)))
      0)))

(defun replique-pprint/reversed-buffer-count (buffer)
  (let ((buffer-first (car buffer)))
    (if buffer-first
        (let ((buffer-last (car (last buffer))))
          (- (replique/get buffer-last :end-pos) (replique/get buffer-first :start-pos)))
      0)))

(defun replique-pprint/buffer-overflow? (items)
  (>= (+ (current-column) (replique-pprint/buffer-count items))
      replique-pprint/max-width))

(defun replique-pprint/reversed-buffer-overflow? (items)
  (>= (+ (current-column) (replique-pprint/reversed-buffer-count items))
      replique-pprint/max-width))

(defun replique-pprint/write-nl (nl)
  (insert ?\n)
  (puthash :trailing-white-space nil replique-pprint/state)
  (let* ((lb (replique/get nl :logical-block))
         (indent-string (make-string (replique/get lb :indent) ?\s)))
    (insert indent-string)
    (puthash :intra-block nil lb)
    (puthash :done-nl t lb)
    (let ((lb (replique/get lb :parent)))
      (while lb
        (puthash :done-nl t lb)
        (puthash :intra-block t lb)
        (setq lb (replique/get lb :parent))))))

(defun replique-pprint/write-item (item)
  (let ((item-type (replique/get item :item-type)))
    (cond ((equal :start-block-t item-type)
           (when-let (cb (replique/get replique-pprint/state :logical-block-callback))
             (funcall cb :start))
           (let* ((lb (replique/get item :logical-block))
                  (prefix (replique/get lb :prefix)))
             (when prefix (insert prefix))
             (let ((col (current-column)))
               (puthash :start-col col lb)
               (puthash :indent col lb))))
          ((equal :end-block-t item-type)
           (when-let (cb (replique/get replique-pprint/state :logical-block-callback))
             (funcall cb :end))
           (when-let (suffix (replique/get (replique/get item :logical-block) :suffix))
             (insert suffix)))
          ((equal :indent-t item-type)
           (let ((lb (replique/get item :logical-block))
                 (relative-to (replique/get item :relative-to)))
             (puthash
              :indent (+ (replique/get item :offset)
                         (cond ((equal relative-to :block) (replique/get lb :start-col))
                               ((equal relative-to :current) (current-column))))
              lb)))
          ((equal :buffer-blob item-type)
           (insert (replique/get item :data)))
          ((equal :nl-t item-type)
           (let ((lb (replique/get item :logical-block))
                 (nl-type (replique/get item :nl-type)))
             (if (or (equal nl-type :mandatory)
                     (and (not (equal nl-type :fill))
                          (replique/get lb :done-nl)))
                 (replique-pprint/write-nl item)
               (when-let (tws (replique/get replique-pprint/state :trailing-white-space))
                 (insert tws)))
             (puthash :trailing-white-space nil replique-pprint/state))))))

(defun replique-pprint/write-items (items force-trailing-whitespace)
  (let ((continue t))
    (while continue
      (let ((item (car items)))
        (if (null item)
            (setq continue nil)
          (when (not (equal :nl-t (replique/get item :item-type)))
            (when-let (tws (replique/get replique-pprint/state :trailing-white-space))
              (insert tws)))
          (replique-pprint/write-item item)
          (puthash :trailing-white-space (replique/get item :trailing-white-space)
                   replique-pprint/state)
          (setq items (cdr items))))))
  (when-let (tws (replique/get replique-pprint/state :trailing-white-space))
    (when force-trailing-whitespace
      (insert tws)
      (puthash :trailing-white-space nil replique-pprint/state))))

(defun replique-pprint/write-items-until-newline (items)
  (let ((continue t))
    (while continue
      (let ((item (car items)))
        (if (or (null item) (equal :nl-t (replique/get item :item-type)))
            (setq continue nil)
          (when-let (tws (replique/get replique-pprint/state :trailing-white-space))
            (insert tws))
          (replique-pprint/write-item item)
          (puthash :trailing-white-space (replique/get item :trailing-white-space)
                   replique-pprint/state)
          (setq items (cdr items))))))
  items)

(defun replique-pprint/linear-nl? (lb section)
  (or (replique-pprint/reversed-buffer-overflow? section)
      (replique/get lb :done-nl)))

(defun replique-pprint/miser-nl? (lb section)
  (let ((miser-width (replique/get replique-pprint/state :miser-width)))
    (and miser-width
         (>= (replique/get lb :start-col) (- replique-pprint/max-width) miser-width)
         (replique-pprint/linear-nl? lb section))))

(defun replique-pprint/write-nl? (newl section subsection)
  (let ((newl-type (replique/get newl :nl-type))
        (lb (replique/get newl :logical-block)))
    (cond ((equal newl-type :linear)
           (replique-pprint/linear-nl? lb section))
          ((equal newl-type :miser)
           (replique-pprint/miser-nl? lb section))
          ((equal newl-type :fill)
           (or (replique/get lb :intra-block-nl)
               (replique-pprint/reversed-buffer-overflow? subsection)
               (replique-pprint/miser-nl? lb section)))
          ((equal newl-type :mandatory)
           t))))

(defun replique-pprint/ancestor? (parent child)
  (let ((child (replique/get child :parent)))
    (while (and child (not (eq parent child)))
      (setq child (replique/get child :parent)))
    (if (null child) nil t)))

(defun replique-pprint/get-section (nl buffer)
  (let ((lb (replique/get nl :logical-block)))
    (seq-take-while
     (lambda (t) (not (and (equal :nl-t (replique/get t :item-type))
                           (replique-pprint/ancestor? (replique/get t :logical-block) lb))))
     buffer)))

(defun replique-pprint/get-sub-section (nl buffer)
  (let ((lb (replique/get nl :logical-block)))
    (seq-take-while
     (lambda (t)
       (let ((nl-lb (replique/get t :logical-block)))
         (not (and (equal :nl-t (replique/get t :item-type))
                   (or
                    (equal nl-lb lb)
                    (replique-pprint/ancestor? nl-lb lb))))))
     buffer)))

(defun replique-pprint/write-item-string (items)
  (when-let (items (replique-pprint/write-items-until-newline items))
    (let* ((newnl (car items))
           (remainder-newline items)
           (items (cdr items))
           (section (replique-pprint/get-section newnl items))
           (sub-section (replique-pprint/get-sub-section newnl items))
           (remainder (seq-drop items (max (length section) (length sub-section))))
           (do-nl (replique-pprint/write-nl? newnl section sub-section))
           (result (if do-nl
                       (progn (replique-pprint/write-nl newnl) (cdr remainder-newline))
                     remainder-newline))
           (long-section (replique-pprint/reversed-buffer-overflow? result)))
      (if long-section
          (let ((length-before (length section))
                (rem2 (replique-pprint/write-item-string section)))
            (if (equal length-before (length rem2))
                (progn
                  (replique-pprint/write-items section nil)
                  remainder)
              (seq-concatenate 'list rem2 remainder)))
        result))))

(defun replique-pprint/write-line (buffer)
  (let* ((continue t)
         (buffer-overflow? t))
    (while continue
      (if buffer-overflow?
          (let* ((buffer-count-before (length buffer))
                 (buffer-after (replique-pprint/write-item-string buffer)))
            (if (equal buffer-count-before (length buffer-after))
                (setq continue nil)
              (setq buffer buffer-after))
            (setq buffer-overflow? (replique-pprint/reversed-buffer-overflow? buffer)))
        (setq continue nil)))
    buffer))

(defun replique-pprint/add-to-buffer (item)
  (puthash :buffer
           (cons item (replique/get replique-pprint/state :buffer))
           replique-pprint/state)
  (when (replique-pprint/buffer-overflow? (replique/get replique-pprint/state :buffer))
    (let* ((buffer (replique/get replique-pprint/state :buffer))
           (buffer (nreverse buffer))
           (buffer (replique-pprint/write-line buffer)))
      (puthash :buffer (nreverse buffer) replique-pprint/state))))

(defun replique-pprint/write-buffered-output ()
  (when-let (buf (replique/get replique-pprint/state :buffer))
    (let ((buffer-overflow? (replique-pprint/buffer-overflow? buf))
          (buf (nreverse buf)))
      (when buffer-overflow?
        (setq buf (replique-pprint/write-line buf)))
      (replique-pprint/write-items buf t)
      (puthash :buffer '() replique-pprint/state))))

(defun replique-pprint/write-white-space ()
  (when-let (tws (replique/get replique-pprint/state :trailing-white-space))
    (insert tws)
    (puthash :trailing-white-space nil replique-pprint/state)))

(defun replique-pprint/write-initial-lines (s)
  (let ((lines (split-string s "\n")))
    (if (null (cadr lines))
        s
      (let ((line (car lines)))
        (if (equal :buffering (replique/get replique-pprint/state :mode))
            (let* ((oldpos (replique/get replique-pprint/state :pos))
                   (newpos (+ oldpos (length line))))
              (puthash :pos newpos replique-pprint/state)
              (replique-pprint/add-to-buffer (replique/hash-map :item-type :buffer-blob
                                                                :data line
                                                                :trailing-white-space nil
                                                                :start-pos oldpos
                                                                :end-pos newpos))
              (replique-pprint/write-buffered-output))
          (replique-pprint/write-white-space)
          (insert line))
        (insert ?\n)
        (setq lines (cdr lines))
        (while (cadr lines)
          (insert (car lines))
          (insert ?\n)
          (setq lines (cdr lines)))
        ;;(puthash :mode :writing replique-pprint/state)
        (car lines)))))

(defmacro replique-pprint/with-pretty-writer (&rest body)
  `(let* ((lb (replique/hash-map :parent nil
                                 :section nil
                                 :start-col 0
                                 :indent 0
                                 :done-nl nil
                                 :intra-block-nl nil
                                 :prefix nil
                                 :suffix nil
                                 :logical-block-callback nil))
          (replique-pprint/state (replique-pprint/initial-state lb)))
     ,@body))

(defun replique-pprint/write (x)
  (let* ((x (if (stringp x) x (string x)))
         (s0 (replique-pprint/write-initial-lines x))
         (s (replace-regexp-in-string "\s+$" "" s0))
         (white-space (substring-no-properties s0 (length s)))
         (mode (replique/get replique-pprint/state :mode)))
    (if (equal mode :writing)
        (progn
          (replique-pprint/write-white-space)
          (insert s)
          (puthash :trailing-white-space white-space replique-pprint/state))
      (let* ((oldpos (replique/get replique-pprint/state :pos))
             (newpos (+ oldpos (length s0))))
        (puthash :pos newpos replique-pprint/state)
        (replique-pprint/add-to-buffer
         (replique/hash-map :item-type :buffer-blob
                            :data s
                            :trailing-white-space white-space
                            :start-pos oldpos
                            :end-pos newpos))))))

(defun replique-pprint/start-block (prefix suffix)
  (let ((lb (replique/hash-map :parent (replique/get replique-pprint/state :logical-blocks)
                               :section nil
                               :start-col 0
                               :indent 0
                               :done-nl nil
                               :intra-block-nl nil
                               :prefix prefix
                               :suffix suffix)))
    (puthash :logical-blocks lb replique-pprint/state)
    (if (equal (replique/get replique-pprint/state :mode) :writing)
        (progn
          (replique-pprint/write-white-space)
          (when-let (cb (replique/get replique-pprint/state :logical-block-callback))
            (funcall cb :start))
          (when prefix (insert prefix))
          (let ((col (current-column)))
            (puthash :start-col col lb)
            (puthash :indent col lb)))
      (let* ((oldpos (replique/get replique-pprint/state :pos))
             (newpos (+ oldpos (if prefix (length prefix) 0))))
        (puthash :pos newpos replique-pprint/state)
        (replique-pprint/add-to-buffer (replique/hash-map
                                        :item-type :start-block-t
                                        :logical-block lb
                                        :start-pos oldpos
                                        :end-pos newpos))))))

(defun replique-pprint/end-block ()
  (let* ((lb (replique/get replique-pprint/state :logical-blocks))
         (suffix (replique/get lb :suffix)))
    (if (equal (replique/get replique-pprint/state :mode) :writing)
        (progn
          (replique-pprint/write-white-space)
          (when suffix (insert suffix))
          (when-let (cb (replique/get replique-pprint/state :logical-block-callback))
            (funcall cb :end)))
      (let* ((oldpos (replique/get replique-pprint/state :pos))
             (newpos (+ oldpos (if suffix (length suffix) 0))))
        (puthash :pos newpos replique-pprint/state)
        (replique-pprint/add-to-buffer (replique/hash-map
                                        :item-type :end-block-t
                                        :logical-block lb
                                        :start-pos oldpos
                                        :end-pos newpos))))
    (puthash :logical-blocks (replique/get lb :parent) replique-pprint/state)))

(defun replique-pprint/nl (nl-type)
  (puthash :mode :buffering replique-pprint/state)
  (let ((pos (replique/get replique-pprint/state :pos)))
    (replique-pprint/add-to-buffer (replique/hash-map
                                    :item-type :nl-t
                                    :nl-type nl-type
                                    :logical-block (replique/get replique-pprint/state
                                                                 :logical-blocks)
                                    :start-pos pos
                                    :end-pos pos))))

(defun replique-pprint/indent (relative-to offset)
  (let ((lb (replique/get replique-pprint/state :logical-blocks)))
    (if (equal (replique/get replique-pprint/state :mode) :writing)
        (let ((col (cond ((equal relative-to :block) (replique/get lb :start-col))
                         ((equal relative-to :current) (current-column)))))
          (replique-pprint/write-white-space)
          (puthash :indent (+ offset col) lb))
      (let ((pos (replique/get replique-pprint/state :pos)))
        (replique-pprint/add-to-buffer (replique/hash-map
                                        :item-type :indent-t
                                        :logical-block lb
                                        :relative-to relative-to
                                        :offset offset
                                        :start-pos pos
                                        :end-pos pos))))))

(defmacro replique-pprint/print-logical-block (opts &rest body)
  `(progn
     (replique-pprint/start-block ,(cadr (assoc :prefix opts)) ,(cadr (assoc :suffix opts)))
     ,@body
     (replique-pprint/end-block)))

(defun replique-pprint/print-list (l)
  (replique-pprint/print-logical-block
   ((:prefix "(") (:suffix ")"))
   (while (car l)
     (replique-pprint/print-dispatch (car l))
     (setq l (cdr l))
     (when (car l)
       (replique-pprint/write " ")
       (replique-pprint/nl :linear)))))

(defun replique-pprint/print-vector (v)
  (replique-pprint/print-logical-block
   ((:prefix "[") (:suffix "]"))
   (let ((i 0)
         (l (length v)))
     (seq-do (lambda (e)
               (replique-pprint/print-dispatch e)
               (setq i (+ 1 i))
               (when (< i l)
                 (replique-pprint/write " ")
                 (replique-pprint/nl :linear)))
             v))))

(defun replique-pprint/print-set (s)
  (replique-pprint/print-logical-block
   ((:prefix "#{") (:suffix "}"))
   (let ((i 0)
         (l (length s)))
     (seq-do (lambda (e)
               (replique-pprint/print-dispatch e)
               (setq i (+ 1 i))
               (when (< i l)
                 (replique-pprint/write " ")
                 (replique-pprint/nl :linear)))
             s))))

(defun replique-pprint/print-map (m)
  (replique-pprint/print-logical-block
   ((:prefix "{") (:suffix "}"))
   (let ((i 0)
         (l (hash-table-count m)))
     (maphash (lambda (k v)
                (replique-pprint/print-dispatch k)
                (replique-pprint/write " ")
                (replique-pprint/nl :linear)
                (replique-pprint/print-dispatch v)
                (setq i (+ 1 i))
                (when (< i l)
                  (replique-pprint/write " ")
                  (replique-pprint/nl :linear)))
              m))))

(defun replique-pprint/print-with-meta (m)
  (let ((meta (oref m :meta))
        (value (oref m :value)))
    (if (not (hash-table-empty-p meta))
        (replique-pprint/print-logical-block
         ()
         (replique-pprint/write "^")
         (replique-pprint/print-map meta)
         (replique-pprint/indent :block 1)
         (replique-pprint/write " ")
         (replique-pprint/nl :linear)
         (replique-pprint/print-dispatch value))
      (replique-pprint/print-dispatch value))))

(defun replique-pprint/print-tagged-value (o)
  (let ((tag (oref o :tag))
        (value (oref o :value)))
    (cond ((equal tag ?n)
           (replique-pprint/write value))
          ((equal tag ?f)
           (replique-pprint/write value))
          ((equal tag ?v)
           (replique-pprint/write "#'")
           (replique-pprint/write value))
          ((equal tag ?c)
           (replique-pprint/write "\\")
           (replique-pprint/write value))
          ((equal tag ?p)
           (replique-pprint/write "#\"")
           (replique-pprint/write value)
           (replique-pprint/write "\""))
          ((equal tag ?u)
           (replique-pprint/write "#uuid \"")
           (replique-pprint/write value)
           (replique-pprint/write "\""))
          ((equal tag ?i)
           (replique-pprint/write "#inst \"")
           (replique-pprint/write value)
           (replique-pprint/write "\""))
          ((cl-typep tag 'replique-transit/tag)
           (let ((tag (oref tag :tag)))
             (cond ((equal tag "set")
                    (replique-pprint/print-set value))
                   ((equal tag "taggedliteral")
                    (let ((tag (symbol-name (aref value 0)))
                          (value (aref value 1)))
                      (replique-pprint/print-logical-block
                       ()
                       (replique-pprint/write "#")
                       (replique-pprint/write tag)
                       (replique-pprint/write " ")
                       (replique-pprint/indent :block (+ 2 (length tag)))
                       (replique-pprint/print-dispatch value))))
                   (t
                    (replique-pprint/print-logical-block
                     ()
                     (replique-pprint/write "#")
                     (replique-pprint/write tag)
                     (replique-pprint/write " ")
                     (replique-pprint/indent :block (+ 2 (length tag)))
                     (replique-pprint/print-dispatch value)))))))))

(defun replique-pprint/print-nil (o)
  (replique-pprint/write "nil"))

(defun replique-pprint/print-true (o)
  (replique-pprint/write "true"))

(defun replique-pprint/print-inf (o)
  (replique-pprint/write "##Inf"))

(defun replique-pprint/print-neg-inf (o)
  (replique-pprint/write "##-Inf"))

(defun replique-pprint/print-nan (o)
  (replique-pprint/write "##NaN"))

(defun replique-pprint/print-symbol (o)
  (replique-pprint/write (symbol-name o)))

(defun replique-pprint/char-escape-string (c)
  (cond ((equal c ?\n) "\\n")
        ((equal c ?\t) "\\t")
        ((equal c ?\r) "\\r")
        ((equal c ?\") "\\\"")
        ((equal c ?\\) "\\\\")
        ((equal c ?\f) "\\f")
        ((equal c ?\b) "\\b")))

(defun replique-pprint/print-string (o)
  (replique-pprint/write "\"")
  (seq-doseq (c o)
    (let ((escaped (replique-pprint/char-escape-string c)))
      (if escaped
          (replique-pprint/write escaped)
        (replique-pprint/write c))))
  (replique-pprint/write "\""))

(defun replique-pprint/print-default (o)
  (replique-pprint/write (with-output-to-string (prin1 o))))

(defun replique-pprint/print-dispatch (o)
  (cond ((null o) (replique-pprint/print-nil o))
        ((eq t o) (replique-pprint/print-true o))
        ((cl-typep o 'replique/with-meta) (replique-pprint/print-with-meta o))
        ((cl-typep o 'replique-transit/tagged-value) (replique-pprint/print-tagged-value o))
        ((symbolp o) (replique-pprint/print-symbol o))
        ((stringp o) (replique-pprint/print-string o))
        ((listp o) (replique-pprint/print-list o))
        ((vectorp o) (replique-pprint/print-vector o))
        ((hash-table-p o) (replique-pprint/print-map o))
        ((equal 1.0e+INF o) (replique-pprint/print-inf o))
        ((equal -1.0e+INF o) (replique-pprint/print-neg-inf o))
        ((equal 1.0e+NaN o) (replique-pprint/print-nan o))
        (t (replique-pprint/print-default o))))

(defun replique-pprint/flush ()
  (if (equal (replique/get replique-pprint/state :mode) :buffering)
      (progn
        (replique-pprint/write-items (nreverse (replique/get replique-pprint/state :buffer)) t)
        (puthash :buffer '() replique-pprint/state))
    (replique-pprint/write-white-space)))

(defun replique-pprint/pprint (o)
  (replique-pprint/with-pretty-writer
   (replique-pprint/print-dispatch o)
   (replique-pprint/flush)))

(defun replique-pprint/pprint-str (o)
  (with-temp-buffer
    (replique-pprint/pprint o)
    (buffer-substring (point-min) (point-max))))

(comment
 (replique-pprint/pprint (list
                          3

                          `[1 "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" ,(replique-transit/tagged-value
                                                                                                                                :tag ?v
                                                                                                                                :value "clojure.core/prn") 3]
                          (replique-transit/tagged-value
                          :tag ?n
                          :value "222222222222222222222222222222222222")
                          (replique/hash-map :eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
                                             :f (replique-transit/tagged-value
                                                 :tag (replique-transit/tag :tag "object")
                                                 :value ["[Iggggggggggggggggggggggggggggggggggggggggggggggggggggggg" 0x3850d28c "[I@3850d28c"]))
                          4))

 (replique-pprint/pprint (replique/with-meta :value '(1 2) :meta (replique/hash-map :e "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")))

 (replique-pprint/pprint nil)

 (replique-pprint/pprint
  (replique-transit/tagged-value
   :tag (replique-transit/tag :tag "object")
   :value ["[I" 0x3850d28c "[I@3850d28c"]))

 (replique-pprint/pprint (replique-transit/tagged-value
                          :tag ?n
                          :value "222222222222222222222222222222222222"))

 (replique-pprint/pprint (replique-transit/tagged-value
                          :tag ?f
                          :value "22.4"))

 (replique-pprint/pprint '\?rr)

 (replique-pprint/pprint (replique-transit/tagged-value
                          :tag ?v
                          :value "clojure.core/prn"))

 (replique-pprint/pprint "e\"ee")

 (replique-pprint/pprint (replique-transit/tagged-value
                          :tag (replique-transit/tag :tag "set")
                          :value [1 2]))

 (replique-pprint/pprint (replique-transit/tagged-value
                          :tag ?c
                          :value "d"))

 (replique-pprint/pprint (replique-transit/tagged-value
                          :tag ?p
                          :value "EQ\\n"))

 (replique-pprint/pprint (replique-transit/decode (read "[\"~#error\" #s(hash-table test equal data (:cause nil :via [#s(hash-table test equal data (:type java.lang.NullPointerException :message nil :at [replique.elisp_printer$eval4060 invokeStatic \"form-init5304808313595444657.clj\" 406]))] :trace [[replique.elisp_printer$eval4060 invokeStatic \"form-init5304808313595444657.clj\" 406] [replique.elisp_printer$eval4060 invoke \"form-init5304808313595444657.clj\" 405] [clojure.lang.Compiler eval \"Compiler.java\" 7062] [clojure.lang.Compiler eval \"Compiler.java\" 7025] [clojure.core$eval invokeStatic \"core.clj\" 3211] [clojure.core$eval invoke \"core.clj\" 3207] [clojure.main$repl$read_eval_print__8574$fn__8577 invoke \"main.clj\" 243] [clojure.main$repl$read_eval_print__8574 invoke \"main.clj\" 243] [clojure.main$repl$fn__8583 invoke \"main.clj\" 261] [clojure.main$repl invokeStatic \"main.clj\" 261] [clojure.main$repl doInvoke \"main.clj\" 177] [clojure.lang.RestFn applyTo \"RestFn.java\" 137] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$apply invoke \"core.clj\" 652] [replique.repl$repl invokeStatic \"repl.clj\" 150] [replique.repl$repl invoke \"repl.clj\" 148] [replique.repl$eval3362 invokeStatic \"form-init5304808313595444657.clj\" 2] [replique.repl$eval3362 invoke \"form-init5304808313595444657.clj\" 2] [clojure.lang.Compiler eval \"Compiler.java\" 7062] [clojure.lang.Compiler eval \"Compiler.java\" 7025] [clojure.core$eval invokeStatic \"core.clj\" 3211] [clojure.core$eval invoke \"core.clj\" 3207] [clojure.main$repl$read_eval_print__8574$fn__8577 invoke \"main.clj\" 243] [clojure.main$repl$read_eval_print__8574 invoke \"main.clj\" 243] [clojure.main$repl$fn__8583 invoke \"main.clj\" 261] [clojure.main$repl invokeStatic \"main.clj\" 261] [clojure.main$repl doInvoke \"main.clj\" 177] [clojure.lang.RestFn invoke \"RestFn.java\" 512] [replique.repl$tooling_repl invokeStatic \"repl.clj\" 21] [replique.repl$tooling_repl invoke \"repl.clj\" 20] [clojure.lang.AFn applyToHelper \"AFn.java\" 152] [clojure.lang.AFn applyTo \"AFn.java\" 144] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$with_bindings_STAR_ invokeStatic \"core.clj\" 1970] [clojure.core$with_bindings_STAR_ doInvoke \"core.clj\" 1970] [clojure.lang.RestFn invoke \"RestFn.java\" 425] [clojure.lang.AFn applyToHelper \"AFn.java\" 156] [clojure.lang.RestFn applyTo \"RestFn.java\" 132] [clojure.core$apply invokeStatic \"core.clj\" 661] [clojure.core$bound_fn_STAR_$fn__5473 doInvoke \"core.clj\" 2000] [clojure.lang.RestFn invoke \"RestFn.java\" 397] [clojure.lang.AFn applyToHelper \"AFn.java\" 152] [clojure.lang.RestFn applyTo \"RestFn.java\" 132] [clojure.lang.Var applyTo \"Var.java\" 702] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$apply invoke \"core.clj\" 652] [replique.server$accept_connection invokeStatic \"server.clj\" 57] [replique.server$accept_connection invoke \"server.clj\" 37] [replique.server$start_server$fn__419$fn__420$fn__423 invoke \"server.clj\" 154] [clojure.lang.AFn run \"AFn.java\" 22] [java.lang.Thread run \"Thread.java\" 748]]))]")))

 (replique-pprint/pprint (replique-transit/decode (read "[\"~#taggedliteral\" [ee \"e\\\"\"]]")))

 (replique-pprint/pprint (replique-transit/decode (read "\"~ucb8acc46-b7a5-4806-9020-c737f402bc2f\"")))

 (replique-pprint/pprint t)

 (replique-pprint/pprint (replique-transit/decode (read "\"~i2018-02-01T22:48:40.916-00:00\"")))

 (replique-pprint/pprint "e
")

 (replique-pprint/pprint (replique-transit/decode (read "[\"~#js\" #s(hash-table test equal size 1 data (:e \"f\"))]")))

 (replique-pprint/pprint (replique-transit/decode (read "[\"~#js\" [\"e\" \"f\"]]")))

 (replique-pprint/pprint (replique-transit/decode (read "[\"~#object\" [TypeError TypeError: Cannot read property 'call' of null]]")))

 (replique-pprint/pprint (replique-transit/decode (read "[\"~#queue\" []]")))

 (replique-pprint/pprint (replique-transit/decode (read "[\"~#+\" \\#]")))
 (replique-pprint/pprint (replique-transit/decode (read "#s(hash-table test equal size 3 data (:e [\"~#+\" [\\#]] [\"~#+\" [\\.\\.\\.]] [\"~#+\" [\\.\\.\\.]]))")))

 
 )

(provide 'replique-pprint)
