;; replique-transit.el ---   -*- lexical-binding: t; -*-

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

(require 'replique-hashmap)

(defclass replique-transit/tagged-value ()
  ((tag :initarg :tag)
   (value :initarg :value)))

(defclass replique-transit/tag ()
  ((tag :initarg :tag)))

;; Decoder fns

(defun replique-transit/decode-boolean (tag rep)
  (if (equal rep "t")
      t
    nil))

(defun replique-transit/decode-special-number (tag rep)
  (cond ((equal rep "Inf")
         1.0e+INF)
        ((equal rep "-Inf")
         -1.0e+INF)
        (t 1.0e+NaN)))

(defun replique-transit/decode-bigint (tag rep)
  (let ((n (string-to-number rep)))
    (if (integerp n)
        n
      (replique-transit/tagged-value :tag tag :value rep))))

(defun replique-transit/decode-with-meta (tag rep)
  (replique/with-meta :meta (aref rep 0) :value (aref rep 1)))

(defvar replique-transit/default-read-handlers
  (replique/hash-map ?\? 'replique-transit/decode-boolean
                     ?z 'replique-transit/decode-special-number
                     ;; do not decode big decimal since we may loose precision
                     ?n 'replique-transit/decode-bigint
                     (replique-transit/tag :tag "with-meta") 'replique-transit/decode-with-meta))

;; Tagged values

;; Does not include special numbers
(defun replique-transit/is-big-number? (n)
  (and (cl-typep n 'replique-transit/tagged-value)
       (or (equal ?n (oref n :tag)) (equal ?f (oref n :tag)))))

(defun replique-transit/is-small-number? (n)
  (if (or (replique-transit/is-big-number? n)
          (equal n 1.0e+INF) (equal n -1.0e+INF)
          (equal n 1.0e+NaN) (equal n -1.0e+NaN))
      nil
    t))

;; End of tagged values

(defun replique-transit/decode-list (l)
  (let ((r l)
        (e (car l)))
    (while e
      (let ((decoded (replique-transit/decode e)))
        (when (not (eq e decoded))
          (setcar r decoded))
        (setq r (cdr r))
        (setq e (car r))))
    l))

(defun replique-transit/decode-map (m)
  (maphash (lambda (k v)
             (let ((k-decoded (replique-transit/decode k))
                   (v-decoded (replique-transit/decode v)))
               (cond ((not (eq k k-decoded))
                      (remhash k m)
                      (puthash k-decoded v-decoded m))
                     ((not (eq v v-decoded))
                      (puthash k-decoded v-decoded m))
                     (t nil))))
           m)
  m)

(defun replique-transit/is-tag (tag)
  (cl-typep tag 'replique-transit/tag))

(defun replique-transit/decode-array* (a a-length)
  (let ((i 0))
    (while (< i a-length)
      (let* ((element (aref a i))
             (decoded (replique-transit/decode element)))
        (when (not (eq decoded element))
          (aset a i decoded)))
      (setq i (1+ i))))
  a)

(defun replique-transit/decode-array (a)
  (let ((a-length (length a)))
    (if (and (equal a-length 2)
             (stringp (aref a 0)))
        (let ((maybe-tagged-value (replique-transit/decode (aref a 0))))
          (if (replique-transit/is-tag maybe-tagged-value)
              (let ((tag-handler (replique/get replique-transit/default-read-handlers
                                               maybe-tagged-value)))
                (if tag-handler
                    (funcall tag-handler maybe-tagged-value (replique-transit/decode (aref a 1)))
                  (replique-transit/tagged-value
                   :tag maybe-tagged-value
                   :value (replique-transit/decode (aref a 1)))))
            (replique-transit/decode-array* a a-length)))
      (replique-transit/decode-array* a a-length))))

(defun replique-transit/decode-string (s)
  (if (and (> (length s) 1) (eq ?~ (aref s 0)))
      (cond ((eq ?# (aref s 1)) (replique-transit/tag :tag (substring-no-properties s 2)))
            ((eq ?~ (aref s 1)) (substring-no-properties s 1))
            (t (let ((tag-handler (replique/get replique-transit/default-read-handlers
                                                (aref s 1))))
                 (if tag-handler
                     (funcall tag-handler (aref s 1) (substring-no-properties s 2))
                   (replique-transit/tagged-value
                    :tag (aref s 1) :value (substring-no-properties s 2))))))
    s))

(defun replique-transit/decode (node)
  (cond
   ((listp node) (progn (replique-transit/decode-list node) node))
   ((hash-table-p node) (replique-transit/decode-map node))
   ((stringp node) (replique-transit/decode-string node))
   ((arrayp node) (replique-transit/decode-array node))
   (t node)))

(provide 'replique-transit)

(comment
 (replique-transit/decode (read "\"e\\n\""))
 (replique-transit/decode '(1 ["~#circle" 2] 3))
 (replique-transit/decode ["~#circle" 2])
 (replique-transit/decode (replique/hash-map ["~#circle" [3 4]] 1 :f "~hee"))
 (replique-transit/decode (read "[\"~#with-meta\" [(1 2) #s(hash-table test equal size 1 data (:e \"e\"))]]"))
 (replique-transit/decode (read "[\"~#set\" [1 2]]"))
 (replique-transit/decode (read "[\"~#object\" [\"[I\" 0x3850d28c \"[I@3850d28c\"]]"))
 (replique-transit/decode (read "[\"~#replique.elisp_printer.Rr\" #s(hash-table test equal size 0 data ())]"))
 (replique-transit/decode (read "\"~f22.4\""))
 (replique-transit/decode (read "\"~n22\""))
 (replique-transit/decode (read "\"~n2222222222222222222222222222222222222222222222222222222\""))
 (replique-transit/decode (read "\"~ce\""))
 (replique-transit/decode (read "\"~cspace\""))
 (replique-transit/decode (read "\"~pe\""))
 (replique-transit/decode (read "\"~pe\\\\n\""))
 (replique-transit/decode (read "\"~zInf\""))
 (replique-transit/decode (read "\"~z-Inf\""))
 (replique-transit/decode (read "\"~z-NaN\""))
 (replique-transit/decode (read "\"~vclojure.core/prn\""))
 (replique-transit/decode (read "\"eee\\\"ffff\""))
 (replique-transit/decode (read "\"~bt\""))
 (replique-transit/decode (read "\"~bf\""))
 (replique-transit/decode (read "[\"~#object\" [clojure.lang.Atom 0x6506067d #s(hash-table test equal size 2 data (:status :ready :val #s(hash-table test equal size 0 data ())))]]"))
 (replique-transit/decode (read "[\"~#taggedliteral\" [e 3]]"))
 (replique-transit/decode (read "\"~ucb8acc46-b7a5-4806-9020-c737f402bc2f\""))
 (replique-transit/decode (read "\"~i2018-02-01T22:48:40.916-00:00\""))
 (replique-transit/decode (read "\"~i2018-02-02T17:06:25.859000000-00:00\""))
 

 (replique-transit/decode (read "[\"~#error\" #s(hash-table test equal data (:cause nil :via [#s(hash-table test equal data (:type java.lang.NullPointerException :message nil :at [replique.elisp_printer$eval4060 invokeStatic \"form-init5304808313595444657.clj\" 406]))] :trace [[replique.elisp_printer$eval4060 invokeStatic \"form-init5304808313595444657.clj\" 406] [replique.elisp_printer$eval4060 invoke \"form-init5304808313595444657.clj\" 405] [clojure.lang.Compiler eval \"Compiler.java\" 7062] [clojure.lang.Compiler eval \"Compiler.java\" 7025] [clojure.core$eval invokeStatic \"core.clj\" 3211] [clojure.core$eval invoke \"core.clj\" 3207] [clojure.main$repl$read_eval_print__8574$fn__8577 invoke \"main.clj\" 243] [clojure.main$repl$read_eval_print__8574 invoke \"main.clj\" 243] [clojure.main$repl$fn__8583 invoke \"main.clj\" 261] [clojure.main$repl invokeStatic \"main.clj\" 261] [clojure.main$repl doInvoke \"main.clj\" 177] [clojure.lang.RestFn applyTo \"RestFn.java\" 137] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$apply invoke \"core.clj\" 652] [replique.repl$repl invokeStatic \"repl.clj\" 150] [replique.repl$repl invoke \"repl.clj\" 148] [replique.repl$eval3362 invokeStatic \"form-init5304808313595444657.clj\" 2] [replique.repl$eval3362 invoke \"form-init5304808313595444657.clj\" 2] [clojure.lang.Compiler eval \"Compiler.java\" 7062] [clojure.lang.Compiler eval \"Compiler.java\" 7025] [clojure.core$eval invokeStatic \"core.clj\" 3211] [clojure.core$eval invoke \"core.clj\" 3207] [clojure.main$repl$read_eval_print__8574$fn__8577 invoke \"main.clj\" 243] [clojure.main$repl$read_eval_print__8574 invoke \"main.clj\" 243] [clojure.main$repl$fn__8583 invoke \"main.clj\" 261] [clojure.main$repl invokeStatic \"main.clj\" 261] [clojure.main$repl doInvoke \"main.clj\" 177] [clojure.lang.RestFn invoke \"RestFn.java\" 512] [replique.repl$tooling_repl invokeStatic \"repl.clj\" 21] [replique.repl$tooling_repl invoke \"repl.clj\" 20] [clojure.lang.AFn applyToHelper \"AFn.java\" 152] [clojure.lang.AFn applyTo \"AFn.java\" 144] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$with_bindings_STAR_ invokeStatic \"core.clj\" 1970] [clojure.core$with_bindings_STAR_ doInvoke \"core.clj\" 1970] [clojure.lang.RestFn invoke \"RestFn.java\" 425] [clojure.lang.AFn applyToHelper \"AFn.java\" 156] [clojure.lang.RestFn applyTo \"RestFn.java\" 132] [clojure.core$apply invokeStatic \"core.clj\" 661] [clojure.core$bound_fn_STAR_$fn__5473 doInvoke \"core.clj\" 2000] [clojure.lang.RestFn invoke \"RestFn.java\" 397] [clojure.lang.AFn applyToHelper \"AFn.java\" 152] [clojure.lang.RestFn applyTo \"RestFn.java\" 132] [clojure.lang.Var applyTo \"Var.java\" 702] [clojure.core$apply invokeStatic \"core.clj\" 657] [clojure.core$apply invoke \"core.clj\" 652] [replique.server$accept_connection invokeStatic \"server.clj\" 57] [replique.server$accept_connection invoke \"server.clj\" 37] [replique.server$start_server$fn__419$fn__420$fn__423 invoke \"server.clj\" 154] [clojure.lang.AFn run \"AFn.java\" 22] [java.lang.Thread run \"Thread.java\" 748]]))]"))

 (replique-transit/decode (read "[\"~#js\" #s(hash-table test equal size 1 data (:e \"f\"))]"))

 )

;; check tooling-msg for omniscient

;; In case we want to make calculations on bigdecimals:
;; (require 'calc)
;; (require 'calc-ext)
;; (math-add (math-normalize '(frac (bigpos 213693951 305843009 2) 1)) 2)

;; All messages must be decoded because of string "~" escaping
