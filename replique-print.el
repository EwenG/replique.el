;; replique-print.el ---   -*- lexical-binding: t; -*-

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

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique-print/print-list (l)
  (let ((first-element? t))
    (insert "(")
    (dolist (x l)
      (if first-element?
          (setq first-element? nil)
        (insert-char ?\s))
      (replique-print/print-dispatch x))
    (insert ")")))

(defun replique-print/print-vector (v)
  (insert "[")
  (let ((i 0)
        (l (length v)))
    (seq-do (lambda (e)
              (replique-print/print-dispatch e)
              (setq i (+ 1 i))
              (when (< i l)
                (insert-char ?\s)))
            v))
  (insert "]"))

(defun replique-print/print-set (s)
  (insert "#{")
  (let ((i 0)
        (l (length s)))
    (seq-do (lambda (e)
              (replique-print/print-dispatch e)
              (setq i (+ 1 i))
              (when (< i l)
                (insert ?\s)))
            s))
  (insert "}"))

(defun replique-print/print-map (m)
  (insert "{")
  (let ((i 0)
        (l (hash-table-count m)))
    (maphash (lambda (k v)
               (replique-print/print-dispatch k)
               (insert-char ?\s)
               (replique-print/print-dispatch v)
               (setq i (+ 1 i))
               (when (< i l)
                 (insert-char ?\s)))
             m))
  (insert "}"))

(defun replique-print/print-with-meta (m)
  (let ((meta (oref m :meta))
        (value (oref m :value)))
    (if (not (hash-table-empty-p meta))
        (progn
         (insert "^")
         (replique-print/print-map meta)
         (insert-char ?\s)
         (replique-print/print-dispatch value))
      (replique-print/print-dispatch value))))

(defun replique-print/print-tagged-value (o)
  (let ((tag (oref o :tag))
        (value (oref o :value)))
    (cond ((equal tag ?n)
           (insert value))
          ((equal tag ?f)
           (insert value))
          ((equal tag ?v)
           (insert "#'")
           (insert value))
          ((equal tag ?c)
           (insert "\\")
           (insert value))
          ((equal tag ?p)
           (insert "#\"")
           (insert value)
           (insert "\""))
          ((equal tag ?u)
           (insert "#uuid \"")
           (insert value)
           (insert "\""))
          ((equal tag ?i)
           (insert "#inst \"")
           (insert value)
           (insert "\""))
          ((cl-typep tag 'replique-transit/tag)
           (let ((tag (oref tag :tag)))
             (cond ((equal tag "set")
                    (replique-print/print-set value))
                   (t (insert ?#)
                      (insert tag)
                      (insert ?\s)
                      (replique-print/print-dispatch value))))))))

(defun replique-print/print-nil (o)
  (insert "nil"))

(defun replique-print/print-true (o)
  (insert "true"))

(defun replique-print/print-inf (o)
  (insert "##Inf"))

(defun replique-print/print-neg-inf (o)
  (insert "##-Inf"))

(defun replique-print/print-nan (o)
  (insert "##NaN"))

(defun replique-print/print-symbol (o)
  (insert (symbol-name o)))

(defun replique-print/char-escape-string (c)
  (cond ((equal c ?\n) "\\n")
        ((equal c ?\t) "\\t")
        ((equal c ?\r) "\\r")
        ((equal c ?\") "\\\"")
        ((equal c ?\\) "\\\\")
        ((equal c ?\f) "\\f")
        ((equal c ?\b) "\\b")))

(defun replique-print/print-string (o)
  (insert "\"")
  (seq-doseq (c o)
    (let ((escaped (replique-print/char-escape-string c)))
      (if escaped
          (insert escaped)
        (insert c))))
  (insert "\""))

(defun replique-print/print-more (o)
  (let ((type (oref o :type)))
    (cond ((equal type "level")
           (insert-char ?#))
          ((equal type "length")
           (insert "...")))))

(defclass replique-print/printable ()
  ()
  :abstract t)

(defun replique-print/object-slots (o)
  (mapcar 'eieio-slot-descriptor-name (eieio-class-slots (eieio-object-class o))))

(defmethod replique-print/print-method ((o replique-print/printable))
  (let ((slots (mapcar (lambda (s)
                         (intern (concat ":" (symbol-name s))))
                       (replique-print/object-slots o)))
        (l nil)
        (m (make-hash-table :test 'equal)))
    (dolist (s slots)
      (push (slot-value o s) l)
      (push s l))
    (let ((data-rest l))
      (while data-rest
        (puthash (car data-rest) (cadr data-rest) m)
        (setq data-rest (cddr data-rest))))
    (insert-char ?#)
    (insert (eieio-object-class-name o))
    (insert ?\s)
    (replique-print/print-dispatch m)))

(defclass replique-print/with-face (replique-print/printable)
  ((object :initarg :object)
   (face :initarg :face :type symbol)))

(defmethod replique-print/print-method ((o replique-print/with-face))
  (let ((start (point)))
    (replique-print/print (oref o object))
    (put-text-property start (point) 'face (oref o face))))

(defun replique-print/print-default (o)
  (insert (with-output-to-string (prin1 o))))

(defun replique-print/print-dispatch (o)
  ;; false is represented by a "false" symbol
  (cond ((null o) (replique-print/print-nil o))
        ((eq t o) (replique-print/print-true o))
        ((cl-typep o 'replique-print/printable) (replique-print/print-method o))
        ((cl-typep o 'replique/more) (replique-print/print-more o))
        ((cl-typep o 'replique-transit/tagged-value) (replique-print/print-tagged-value o))
        ((cl-typep o 'replique-transit/empty-list) (replique-print/print-list '()))
        ((symbolp o) (replique-print/print-symbol o))
        ((stringp o) (replique-print/print-string o))
        ((listp o) (replique-print/print-list o))
        ((vectorp o) (replique-print/print-vector o))
        ((hash-table-p o) (replique-print/print-map o))
        ((equal 1.0e+INF o) (replique-print/print-inf o))
        ((equal -1.0e+INF o) (replique-print/print-neg-inf o))
        ((equal 1.0e+NaN o) (replique-print/print-nan o))
        (t (replique-print/print-default o))))

(defun replique-print/print (o)
  (if font-lock-mode
      (progn
        (font-lock-mode -1)
        (replique-print/print-dispatch o)
        (font-lock-mode 1))
    (replique-print/print-dispatch o)))

(defun replique-print/print-str (o)
  (with-temp-buffer
    (replique-print/print o)
    (buffer-substring (point-min) (point-max))))

(comment
 (replique-print/print (list
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

 (replique-print/print (replique/with-meta :value '(1 2) :meta (replique/hash-map :e "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")))

 (replique-print/print nil)

 (replique-print/print
  (replique-transit/tagged-value
   :tag (replique-transit/tag :tag "object")
   :value ["[I" 0x3850d28c "[I@3850d28c"]))

 (replique-print/print (replique-transit/tagged-value
                          :tag ?n
                          :value "222222222222222222222222222222222222"))

 (replique-print/print (replique-transit/tagged-value
                          :tag ?f
                          :value "22.4"))

 (replique-print/print '\?rr)

 (replique-print/print (replique-transit/tagged-value
                          :tag ?v
                          :value "clojure.core/prn"))

 (replique-print/print "e\"ee")

 (replique-print/print (replique-transit/tagged-value
                          :tag (replique-transit/tag :tag "set")
                          :value [1 2]))

 (replique-print/print (replique-transit/tagged-value
                          :tag ?c
                          :value "d"))

 (replique-print/print (replique-transit/tagged-value
                          :tag ?p
                          :value "EQ\\n"))

 (replique-print/print (replique-transit/decode (read "[\"~#error\" #s(hash-table test equal data (:cause nil :via [#s(hash-table test equal data (:type java.lang.NullPointerException :message nil :at [replique.elisp_printer$eval4060 invokeStatic \"form-init5304808313595444657.clj\" 406]))] :trace [[replique.elisp_printer$eval4060 invokeStatic \"form-init5304808313595444657.clj\" 406] [replique.elisp_printer$eval4060 invoke \"form-init5304808313595444657.clj\" 405] [clojure.lang.Compiler eval \"Compiler.java\" 7062] [clojure.lang.Compiler eval \"Compiler.java\" 7025] [clojure.core$eval invokeStatic \"core.clj\" 3211] [clojure.core$eval invoke \"core.clj\" 3207] [clojure.main$repl$read_eval_print__8574$fn__8577 invoke \"main.clj\" 243] [clojure.main$repl$read_eval_print__8574 invoke \"main.clj\" 243] [clojure.main$repl$fn__8583 invoke \"main.clj\" 261] [clojure.main$repl invokeStatic \"main.clj\" 261] [clojure.main$repl doInvoke \"main.clj\" 177] [clojure.lang.RestFn applyTo \"RestFn.java\" 137] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$apply invoke \"core.clj\" 652] [replique.repl$repl invokeStatic \"repl.clj\" 150] [replique.repl$repl invoke \"repl.clj\" 148] [replique.repl$eval3362 invokeStatic \"form-init5304808313595444657.clj\" 2] [replique.repl$eval3362 invoke \"form-init5304808313595444657.clj\" 2] [clojure.lang.Compiler eval \"Compiler.java\" 7062] [clojure.lang.Compiler eval \"Compiler.java\" 7025] [clojure.core$eval invokeStatic \"core.clj\" 3211] [clojure.core$eval invoke \"core.clj\" 3207] [clojure.main$repl$read_eval_print__8574$fn__8577 invoke \"main.clj\" 243] [clojure.main$repl$read_eval_print__8574 invoke \"main.clj\" 243] [clojure.main$repl$fn__8583 invoke \"main.clj\" 261] [clojure.main$repl invokeStatic \"main.clj\" 261] [clojure.main$repl doInvoke \"main.clj\" 177] [clojure.lang.RestFn invoke \"RestFn.java\" 512] [replique.repl$tooling_repl invokeStatic \"repl.clj\" 21] [replique.repl$tooling_repl invoke \"repl.clj\" 20] [clojure.lang.AFn applyToHelper \"AFn.java\" 152] [clojure.lang.AFn applyTo \"AFn.java\" 144] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$with_bindings_STAR_ invokeStatic \"core.clj\" 1970] [clojure.core$with_bindings_STAR_ doInvoke \"core.clj\" 1970] [clojure.lang.RestFn invoke \"RestFn.java\" 425] [clojure.lang.AFn applyToHelper \"AFn.java\" 156] [clojure.lang.RestFn applyTo \"RestFn.java\" 132] [clojure.core$apply invokeStatic \"core.clj\" 661] [clojure.core$bound_fn_STAR_$fn__5473 doInvoke \"core.clj\" 2000] [clojure.lang.RestFn invoke \"RestFn.java\" 397] [clojure.lang.AFn applyToHelper \"AFn.java\" 152] [clojure.lang.RestFn applyTo \"RestFn.java\" 132] [clojure.lang.Var applyTo \"Var.java\" 702] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$apply invoke \"core.clj\" 652] [replique.server$accept_connection invokeStatic \"server.clj\" 57] [replique.server$accept_connection invoke \"server.clj\" 37] [replique.server$start_server$fn__419$fn__420$fn__423 invoke \"server.clj\" 154] [clojure.lang.AFn run \"AFn.java\" 22] [java.lang.Thread run \"Thread.java\" 748]]))]")))

 (replique-print/print (replique-transit/decode (read "[\"~#taggedliteral\" [ee \"e\\\"\"]]")))

 (replique-print/print (replique-transit/decode (read "\"~ucb8acc46-b7a5-4806-9020-c737f402bc2f\"")))

 (replique-print/print t)

 (replique-print/print (replique-transit/decode (read "\"~i2018-02-01T22:48:40.916-00:00\"")))

 (replique-print/print "e
")

 (replique-print/print (replique-transit/decode (read "[\"~#js\" #s(hash-table test equal size 1 data (:e \"f\"))]")))

 (replique-print/print (replique-transit/decode (read "[\"~#js\" [\"e\" \"f\"]]")))

 (replique-print/print (replique-transit/decode (read "[\"~#object\" [TypeError TypeError: Cannot read property 'call' of null]]")))

 (replique-print/print (replique-transit/decode (read "[\"~#queue\" []]")))

 (replique-print/print (replique-transit/decode (read "\"~+level\"")))
 (replique-print/print (replique-transit/decode (read "#s(hash-table test equal size 3 data (:e \"~+level\" \"~+length\" \"~+length\"))")))

 (replique-print/print (replique/hash-map :directory "clojure/replique/" :host "localhost" :port 38889 :repl-type :clj :repl-env :replique/clj))
 )

(provide 'replique-print)


