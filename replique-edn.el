;;; replique-edn2.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique-edn/safe-elt (vector i)
  (let ((l (length vector)))
    (if (and (> l 0) (< i l))
        (elt vector i)
      0)))

(defclass replique-edn/reader ()
  ((str :initarg :str
        :initform ""
        :type string)
   (unread-char :initarg :unread-char
                :initform nil
                :type (or null character))
   (unread-char2 :initarg :unread-char2
                 :initform nil
                 :type (or null character))
   (index :initarg :index
          :initform 0
          :type number)))

(defmethod replique-edn/reader-read
  ((r replique-edn/reader) &optional u-char)
  (cond ((and u-char (oref r unread-char2))
         (error "Cannot unread more than 2 characters."))
        ((and u-char (oref r unread-char))
         (oset r unread-char2 u-char))
        (u-char
         (oset r unread-char u-char))
        ((oref r unread-char2)
         (let ((ret (oref r unread-char2)))
           (oset r unread-char2 nil)
           ret))
        ((oref r unread-char)
         (let ((ret (oref r unread-char)))
           (oset r unread-char nil)
           ret))
        (t (let ((res (replique-edn/safe-elt
                       (oref r str)
                       (oref r index))))
             (oset r index (1+ (oref r index)))
             res))))

(defmethod replique-edn/reader-peek-char
    ((r replique-edn/reader))
  (let ((ch (replique-edn/reader-read r)))
    (when (not (equal 0 ch))
      (replique-edn/reader-read r ch))
    ch))

(defmethod replique-edn/reader-rest-string
  ((r replique-edn/reader))
  (cond
   ((> (oref r index) (length (oref r str)))
    "")
   (t
    (concat
     (if (oref r unread-char)
         (char-to-string (oref r unread-char))
       "")
     (if (oref r unread-char2)
         (char-to-string (oref r unread-char2))
       "")
     (substring (oref r str) (oref r index))))))

(defun replique-edn/state-print (state)
  (-let (((&alist :reader reader
                  :actions actions
                  :result-state result-state
                  :result result) (symbol-value state)))
    `((:reader . ,reader)
      (:actions . ,(symbol-value actions))
      (:result-state . ,(symbol-value result-state))
      (:result . ,(symbol-value result)))))

(defun replique-edn/result (state)
  (-let (((&alist :result result
                  :result-state result-state)
          (symbol-value state)))
    (when (not (equal :done (symbol-value result-state)))
      (error (format "result-state is %s"
                     (symbol-value result-state))))
    (car (symbol-value result))))








(defun replique-edn/is-separator (ch)
  (or (equal ?\s ch)
      (equal ?\, ch)
      (equal ?\n ch)))

(defun replique-edn/digit (ch base)
  (if (equal ?0 ch) 0
    (let ((res (string-to-number
                (char-to-string ch)
                base)))
      (if (equal 0 res) -1 res))))





(defun replique-edn/dispatch-macros (ch)
  (cond
   ((equal ch ?\{) '(replique-edn/read-set))
   ((equal ch ?<) (error "Unreadable form"))
   ((equal ch ?\;) '(replique-edn/read-comment))
   ((equal ch ?_) '(replique-edn/read-discard))
   ((equal ch ?\') '(replique-edn/read-var))
   ((equal ch ?\") '(replique-edn/read-regexp))
   (t nil)))

(defun replique-edn/macros (ch)
  (cond ((equal ?\" ch) '(replique-edn/read-string*))
        ((equal ?: ch) '(replique-edn/read-keyword))
        ((equal ?\; ch) '(replique-edn/read-comment))
        ;; Discard metas
        ((equal ?^ ch) '(replique-edn/read-discard))
        ((equal ?\( ch) '(replique-edn/read-list))
        ((equal ?\) ch) `(replique-edn/read-unmatched-delimiter ,ch))
        ((equal ?\[ ch) '(replique-edn/read-vector))
        ((equal ?\] ch) `(replique-edn/read-unmatched-delimiter ,ch))
        ((equal ?\{ ch) '(replique-edn/read-map))
        ((equal ?\} ch) `(replique-edn/read-unmatched-delimiter ,ch))
        ((equal ?\\ ch) '(replique-edn/read-char))
        ((equal ?# ch) '(replique-edn/read-dispatch))
        (t nil)))

(defun replique-edn/number-literal? (reader initch)
  (or (replique-edn/is-numeric initch)
      (and (or (equal ?+ initch)
               (equal ?- initch))
           (replique-edn/is-numeric
            (replique-edn/reader-peek-char reader)))))

(defvar replique-edn/int-pattern "^\\([-+]?\\)\\(?:\\(0\\)\\|\\([1-9][0-9]*\\)\\|0[xX]\\([0-9A-Fa-f]+\\)\\|0\\([0-7]+\\)\\|\\([1-9][0-9]?\\)[rR]\\([0-9A-Za-z]+\\)\\|0[0-9]+\\)\\(N\\)?$")
(defvar replique-edn/float-pattern "^\\([-+]?[0-9]+\\(\\.[0-9]*\\)?\\([eE][-+]?[0-9]+\\)?\\)\\(M\\)?$")
(defvar replique-edn/ratio-pattern "^\\([-+]?[0-9]+\\)/\\([0-9]+\\)$")

(defun replique-edn/match-int (s)
  (if (match-string 2 s)
      (if (match-string-no-properties 8 s)
          (error "Bignum is not supported")
        0)
    (let* ((negate (string= "-" (match-string-no-properties 1 s)))
           (a (cond ((match-string-no-properties 3 s)
                     `(,(match-string-no-properties 3 s) 10))
                    ((match-string-no-properties 4 s)
                     `(,(match-string-no-properties 4 s) 16))
                    ((match-string-no-properties 5 s)
                     `(,(match-string-no-properties 5 s) 8))
                    ((match-string-no-properties 7 s)
                     `(,(match-string-no-properties 7 s)
                       ,(string-to-number
                         (match-string-no-properties 6 s))))
                    (t '(nil nil))))
           (n (car a))
           (radix (cadr a)))
      (when n
        (let* ((bn (string-to-number n radix))
               (bn (if negate (- bn) bn)))
          (if (match-string-no-properties 8 s)
              (error "Bignum is not supported")
            bn))))))

(defun replique-edn/match-float (s)
  (if (match-string-no-properties 4 s)
      (error "Bignum is not supported")
    (string-to-number s)))

(defun replique-edn/match-ratio (s)
  (let* ((numerator (match-string-no-properties 1 s))
         (denominator (match-string-no-properties 2 s))
         (numerator (if (string-suffix-p "+" numerator)
                        (substring numerator 1)
                      numerator)))
    (/ (float (string-to-number numerator))
       (float (string-to-number denominator)))))

(defun replique-edn/match-number (s)
  (save-match-data
    (if (string-match replique-edn/int-pattern s)
        (replique-edn/match-int s)
      (if (string-match replique-edn/float-pattern s)
          (replique-edn/match-float s)
        (when (string-match replique-edn/ratio-pattern s)
          (replique-edn/match-ratio s))))))

(defun replique-edn/read-number (state sb)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/reader-read reader)))
    (while (and (not (replique-edn/is-separator ch))
                (not (replique-edn/macros ch))
                (not (equal 0 ch)))
      (setq sb (concat sb (char-to-string ch)))
      (setq ch (replique-edn/reader-read reader)))
    (if (equal 0 ch)
        (progn (push `(replique-edn/read-number ,sb)
                     (symbol-value actions))
               (set result-state :waiting))
      (progn (replique-edn/reader-read reader ch)
             (let ((number (replique-edn/match-number sb)))
               (if number
                   (push number (symbol-value result))
                 (error "Invalid number format [ %s ]" sb)))))))

(defun replique-edn/is-macro-terminating (ch)
  (and (not (equal ?# ch))
       (not (equal ?` ch))
       (not (equal ?: ch))
       (replique-edn/macros ch)))

(defun replique-edn/is-letter (ch)
  (memq (get-char-code-property ch 'general-category)
        '(Ll Lu Lo Lt Lm Mn Mc Me Nl)))

(defun replique-edn/is-numeric (ch)
  (and (>= ch 48) (<= ch 57)))

(defun replique-edn/symbol-constituent (ch)
  (or (replique-edn/is-letter ch)
      (replique-edn/is-numeric ch)
      (equal ?. ch)
      (equal ?* ch)
      (equal ?+ ch)
      (equal ?! ch)
      (equal ?- ch)
      (equal ?_ ch)
      (equal ?? ch)
      (equal ?$ ch)
      (equal ?% ch)
      (equal ?& ch)
      (equal ?= ch)
      (equal ?< ch)
      (equal ?> ch)
      (equal ?+ ch)
      (equal ?- ch)
      (equal ?: ch)
      (equal ?# ch)
      (equal ?/ ch)))

(defun replique-edn/symbol-constituent-first (ch)
  (and (not (replique-edn/is-numeric ch))
       (not (or (equal ?: ch)
                (equal ?# ch)
                (equal ?/ ch)))
       (replique-edn/symbol-constituent ch)))

(defun replique-edn/read-unicode (s)
  (read (format "?%s" (concat "\\u" s))))

(defun replique-edn/read-symbol* (state sb)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/reader-read reader)))
    (while (and (not (replique-edn/is-separator ch))
                (not (replique-edn/is-macro-terminating ch))
                (not (equal 0 ch)))
      (when (not (replique-edn/symbol-constituent ch))
        (error "Invalid character: %c" ch))
      (setq sb (concat sb (char-to-string ch)))
      (setq ch (replique-edn/reader-read reader)))
    (if (equal 0 ch)
        (progn
          (push `(replique-edn/read-symbol* ,sb)
                (symbol-value actions))
          (set result-state :waiting))
      (progn
        (replique-edn/reader-read reader ch)
        (push sb (symbol-value result))))))

(defun replique-edn/read-symbol (state validate-first)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (initch (replique-edn/reader-peek-char reader)))
    (cond ((equal 0 initch)
           (push `(replique-edn/read-symbol ,initch ,validate-first)
                 (symbol-value actions)))
          ((and validate-first
                (not (replique-edn/symbol-constituent-first initch)))
           (error "Invalid first character: %c" initch))
          (t (replique-edn/read-symbol* state "")))))

(defun replique-edn/parse-var (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (symbol (pop (symbol-value result))))
    (push (replique-edn/var :nil :full-name symbol)
     (symbol-value result))))

(defun replique-edn/read-var (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push `(replique-edn/parse-var)
          (symbol-value actions))
    (push `(replique-edn/parse-symbol-extended)
          (symbol-value actions))
    (push `(replique-edn/read-symbol t)
          (symbol-value actions))))

(defun replique-edn/token-to-unicode (state)
  (-let* (((&alist :actions actions
                   :result result)
           state)
          (token (pop (symbol-value result)))
          (ch (replique-edn/read-unicode token)))
    (push ch (symbol-value result))))

(defun replique-edn/token-to-char (state)
  (-let* (((&alist :actions actions
                   :result result)
           state)
          (token (pop (symbol-value result)))
          (ch (cond
               ((equal 1 (length token)) (string-to-char token))
               ((string= token "newline") ?\n)
               ((string= token "return") ?\r)
               ((string= token "space") ?\s)
               ((string= token "tab") ?\t)
               ((string= token "backspace") ?\b)
               ((string= token "formfeed") ?\f)
               ((string-prefix-p "u" token)
                (replique-edn/read-unicode (substring token 1)))
               (t (error "Invalid character: \\%s" token)))))
    (push ch (symbol-value result))))

(defun replique-edn/read-char (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/reader-read reader)))
    (if (equal 0 ch)
        (progn (push '(replique-edn/read-char)
                     (symbol-value actions))
               (set result-state :waiting))
      (cond ((equal ?\s ch)
             (error "Backslash cannot be followed by whitespace"))
            (t (replique-edn/reader-read reader ch)
               (push '(replique-edn/token-to-char)
                     (symbol-value actions))
               (push `(replique-edn/read-symbol nil)
                     (symbol-value actions)))))))

(defun replique-edn/escaped-char (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/reader-read reader))
          (ec (cond ((equal 0 ch) 0)
                    ((equal ?t ch) ?\t)
                    ((equal ?r ch) ?\r)
                    ((equal ?n ch) ?\n)
                    ((equal ?\\ ch) ?\\)
                    ((equal ?\" ch) ?\")
                    ((equal ?b ch) ?\b)
                    ((equal ?f ch) ?\f)
                    ((equal ?u ch) ?u)
                    (t (error "Unsupported escape character: \\%c" ch)))))
    (cond ((equal 0 ec)
           (push '(replique-edn/escaped-char) (symbol-value actions))
           (set result-state :waiting))
          ((equal ?u ec)
           (push '(replique-edn/token-to-unicode)
                 (symbol-value actions))
           (push `(replique-edn/read-symbol nil)
                 (symbol-value actions)))
          (t (push ec (symbol-value result))))))

(defun replique-edn/append-char (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (pop (symbol-value result)))
          (sb (pop (symbol-value result))))
    (setq sb (concat sb (char-to-string ch)))
    (push sb (symbol-value result))))

;; read-string* with a star because a read-string method might be added
;; later to the public API.
(defun replique-edn/read-string* (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (sb (pop (symbol-value result)))
          (ch (replique-edn/reader-read reader)))
    (while (and (not (equal ?\" ch))
                (not (equal 0 ch))
                (not (equal ?\\ ch)))
      (setq sb (concat sb (char-to-string ch)))
      (setq ch (replique-edn/reader-read reader)))
    (push sb (symbol-value result))
    (cond ((equal 0 ch)
           (push '(replique-edn/read-string*) (symbol-value actions))
           (set result-state :waiting))
          ((equal ?\\ ch)
           (push `(replique-edn/read-string*) (symbol-value actions))
           (push `(replique-edn/append-char) (symbol-value actions))
           (push `(replique-edn/escaped-char) (symbol-value actions)))
          (t nil))))

(defun replique-edn/write-string* (str)
  (let ((idx 0)
        (chars nil))
    (while (< idx (length str))
      (let ((ch (elt str idx)))
        (cond ((or (equal ?\\ ch)
                   (equal ?\" ch))
               (push ?\\ chars)
               (push ch chars))
              (t (push ch chars))))
      (setq idx (1+ idx)))
    (format "\"%s\""
            (apply 'string (reverse chars)))))

(defun replique-edn/parse-regexp (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (s (pop (symbol-value result))))
    (push (replique-edn/regexp :nil :pattern s)
          (symbol-value result))))

(defun replique-edn/read-regexp (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push `(replique-edn/parse-regexp)
          (symbol-value actions))
    (push `(replique-edn/read-string*)
          (symbol-value actions))
    (push "" (symbol-value result))))

(defun replique-edn/parse-symbol (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (token (pop (symbol-value result)))
          (token (when (and (not (string= "" token))
                            (not (string-suffix-p ":" token))
                            (not (string-prefix-p "::" token)))
                   (let* ((ns-idx (position ?/ token))
                          (ns (when ns-idx
                                (substring-no-properties
                                 token 0 ns-idx))))
                     (if ns
                         (let ((ns-idx (1+ ns-idx)))
                           (when (not (equal ns-idx (length token)))
                             (let ((sym (substring-no-properties
                                         token ns-idx)))
                               (when (and (not (replique-edn/is-numeric
                                                (elt sym 0)))
                                          (not (string= "" sym))
                                          (not (string-suffix-p ":" ns))
                                          (or (string= sym "/")
                                              (not (position ?/ sym))))
                                 (list ns sym)))))
                       (when (or (string= token "/")
                                 (not (position ?/ token)))
                         (list nil token)))))))
    (push token (symbol-value result))))

(defun replique-edn/parse-keyword (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (s (pop (symbol-value result)))
          (token (pop (symbol-value result)))
          (keyword (if (and s (not (string-match-p "::" token)))
                       (let ((ns (car s))
                             (name (cadr s)))
                         (if (equal ?: (elt token 0))
                             (error "Invalid token: :%s" token)
                           (if ns
                               (intern (concat ":" ns "/" name))
                             (intern (concat ":" name)))))
                     (error "Invalid token: :%s" token))))
    (push keyword (symbol-value result))))

(defun replique-edn/duplicate (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (val (car (symbol-value result))))
    (push val (symbol-value result))))

(defun replique-edn/read-keyword (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/reader-read reader)))
    (cond ((equal 0 ch)
           (push '(replique-edn/read-keyword)
                 (symbol-value actions))
           (set result-state :waiting))
          ((replique-edn/is-separator ch)
           (error "Invalid token: :"))
          (t
           (replique-edn/reader-read reader ch)
           (push '(replique-edn/parse-keyword)
                 (symbol-value actions))
           (push '(replique-edn/parse-symbol)
                 (symbol-value actions))
           (push '(replique-edn/duplicate)
                 (symbol-value actions))
           (push `(replique-edn/read-symbol nil)
                 (symbol-value actions))))))

(defun replique-edn/parse-ns-symbol (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (p (pop (symbol-value result)))
          (s (let ((ns (car p))
                   (n (cadr p)))
               (if ns
                   (intern (concat ns "/" n))
                 (intern n)))))
    (push s (symbol-value result))))

(defun replique-edn/parse-symbol-extended (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (symbol (pop (symbol-value result))))
    (cond ((string= symbol "nil")
           (push nil (symbol-value result)))
          ((string= symbol "true")
           (push t (symbol-value result)))
          ((string= symbol "false")
           (push nil (symbol-value result)))
          ((string= symbol "/")
           (push '/ (symbol-value result)))
          ((string= symbol "NaN")
           (push 0.0e+NaN (symbol-value result)))
          ((string= symbol "-Infinity")
           (push -1.0e+INF (symbol-value result)))
          ((string= symbol "+Infinity")
           (push +1.0e+INF (symbol-value result)))
          ((string= symbol "Infinity")
           (push +1.0e+INF (symbol-value result)))
          (t
           (push symbol (symbol-value result))
           (push '(replique-edn/parse-ns-symbol)
                 (symbol-value actions))
           (push '(replique-edn/parse-symbol)
                 (symbol-value actions))))))

(defun replique-edn/skip-line (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/reader-read reader)))
    (cond ((equal 0 ch)
           (push '(replique-edn/skip-line)
                 (symbol-value actions))
           (set result-state :waiting))
          ((not (equal ?\n ch))
           (replique-edn/skip-line state))
          (t (push '(replique-edn/read*)
                   (symbol-value actions))))))

(defun replique-edn/read-comment (state)
  (replique-edn/skip-line state))

(defun replique-edn/discard (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (pop (symbol-value result))))

(defun replique-edn/read-discard (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push '(replique-edn/read*)
          (symbol-value actions))
    (push '(replique-edn/discard)
          (symbol-value actions))
    (push '(replique-edn/read*)
          (symbol-value actions))))

(defun replique-edn/read-past (reader)
  (let ((ch (replique-edn/reader-read reader)))
    (if (replique-edn/is-separator ch)
        (replique-edn/read-past reader)
      ch)))

(defun replique-edn/append-object (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (o (pop (symbol-value result)))
          (l (pop (symbol-value result))))
    (push o l)
    (push l (symbol-value result))))

(defun replique-edn/read-unmatched-delimiter (state ch)
  (error "Unmatched delimiter %s" (char-to-string ch)))

(defun replique-edn/read-delimited (state delim)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/read-past reader)))
    (cond ((equal 0 ch)
           (push `(replique-edn/read-delimited ,delim)
                 (symbol-value actions))
           (set result-state :waiting))
          ((equal delim ch) nil)
          (t (replique-edn/reader-read reader ch)
             (push `(replique-edn/read-delimited ,delim)
                   (symbol-value actions))
             (push '(replique-edn/append-object)
                   (symbol-value actions))
             (push '(replique-edn/read*)
                   (symbol-value actions))))))

(defvar replique-edn/default-data-readers
  (make-hash-table :test #'equal))

(defclass replique-edn/printable ()
  ()
  :abstract t)

(defmethod replique-edn/print-method ((o replique-edn/printable))
  (let ((slots (mapcar (lambda (s)
                         (intern (concat ":" (symbol-name s))))
                       (object-slots o)))
        (l nil))
    (mapcar (lambda (s)
              (push (slot-value o s) l)
              (push s l))
            slots)
    (format "#%s %s"
            (object-class o)
            (replique-edn/pr-str
             (replique-edn/list-to-map l)))))

(defclass replique-edn/inst (replique-edn/printable)
  ((high :initarg :high
         :type number)
   (low :initarg :low
        :type number)))

(defmethod replique-edn/print-method ((o replique-edn/inst))
  (format
   "#inst %s"
   (format-time-string "\"%Y-%m-%dT%H:%M:%S.52Z\""
                       (list (oref o :high) (oref o :low))
                       :utc)))

(defclass replique-edn/uuid (replique-edn/printable)
  ((uuid :initarg :uuid
         :type string)))

(defmethod replique-edn/print-method ((o replique-edn/uuid))
  (format
   "#uuid %s"
   (oref o :uuid)))

(puthash 'inst
         (lambda (inst)
           (let ((time (date-to-time inst)))
             (replique-edn/inst nil
                                 :high (car time)
                                 :low (cadr time))))
         replique-edn/default-data-readers)

(puthash 'uuid
         (lambda (uuid)
           (replique-edn/uuid nil :uuid uuid))
         replique-edn/default-data-readers)

(defun replique-edn/parse-tagged (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result
                   :tagged-readers tagged-readers)
           state)
          (object (pop (symbol-value result)))
          (tag (pop (symbol-value result))))
    (when (not (symbolp tag))
      (error "Reader tag must be a symbol"))
    (let ((f (cdr (assoc tag tagged-readers))))
      (if f
          (push (funcall f object) (symbol-value result))
        (let ((d (gethash tag replique-edn/default-data-readers)))
          (if d (push (funcall d object) (symbol-value result))
            (error "No reader function for tag %s" tag)))))))

(defun replique-edn/read-tagged (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push '(replique-edn/parse-tagged) (symbol-value actions))
    (push '(replique-edn/read*) (symbol-value actions))
    (push '(replique-edn/read*) (symbol-value actions))))

(defun replique-edn/read-dispatch (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/reader-read reader)))
    (if (equal 0 ch)
        (progn (push '(replique-edn/read-dispatch)
                     (symbol-value actions))
               (set result-state :waiting))
      (let ((dm (replique-edn/dispatch-macros ch)))
        (if dm
            (push dm (symbol-value actions))
          (progn
            (replique-edn/reader-read reader ch)
            (push '(replique-edn/read-tagged)
                  (symbol-value actions))))))))

(defun replique-edn/parse-list (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (l (pop (symbol-value result))))
    (push (reverse l) (symbol-value result))))

(defun replique-edn/parse-vector (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (l (pop (symbol-value result))))
    (push (apply 'vector (reverse l))
          (symbol-value result))))

(defun replique-edn/read-list (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push nil (symbol-value result))
    (push '(replique-edn/parse-list) (symbol-value actions))
    (push '(replique-edn/read-delimited ?\)) (symbol-value actions))))

(defun replique-edn/read-vector (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push nil (symbol-value result))
    (push '(replique-edn/parse-vector) (symbol-value actions))
    (push '(replique-edn/read-delimited ?\]) (symbol-value actions))))

(defun replique-edn/list-to-map* (idx l m)
  (if (> (length l) idx)
      (let ((k (elt l idx))
            (val (elt l (1+ idx))))
        (puthash k val m)
        (replique-edn/list-to-map* (+ 2 idx) l m))
    m))

(defun replique-edn/list-to-map (l)
  (let ((m (make-hash-table :test 'equal)))
    (replique-edn/list-to-map* 0 l m)))

(defun replique-edn/parse-hashmap (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (l (pop (symbol-value result))))
    (if (equal 1 (logand 1 (length l)))
        (error "Map literal must contain an even number of forms")
      (push (replique-edn/list-to-map (reverse l))
            (symbol-value result)))))

(defun replique-edn/read-map (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push nil (symbol-value result))
    (push '(replique-edn/parse-hashmap) (symbol-value actions))
    (push '(replique-edn/read-delimited ?\}) (symbol-value actions))))

(defclass replique-edn/set (replique-edn/printable)
  ((vals :initarg :vals
         :type (or null cons))))

(defmethod replique-edn/print-method ((o replique-edn/set))
  (format
   "#{%s}"
   (s-join " " (mapcar
                'replique-edn/pr-str
                (oref o :vals)))))

(defun replique-edn/parse-set (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (l (pop (symbol-value result))))
    (push (replique-edn/set :nil :vals l)
          (symbol-value result))))

(defun replique-edn/read-set (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push nil (symbol-value result))
    (push '(replique-edn/parse-set) (symbol-value actions))
    (push '(replique-edn/read-delimited ?\}) (symbol-value actions))))

(defclass replique-edn/var (replique-edn/printable)
  ((full-name :initarg :full-name
              :type symbol)))

(defmethod replique-edn/print-method ((o replique-edn/var))
  (format "#'%s" (oref o :full-name)))

(defclass replique-edn/regexp (replique-edn/printable)
  ((pattern :initarg :pattern
            :type string)))

(defmethod replique-edn/print-method ((o replique-edn/regexp))
  (format "#%s" (replique-edn/write-string* (oref o :pattern))))

(defun replique-edn/read* (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn/reader-read reader)))
    (cond ((equal 0 ch)
           (push '(replique-edn/read*) (symbol-value actions))
           (set result-state :waiting))
          ((replique-edn/is-separator ch)
           (push '(replique-edn/read*) (symbol-value actions)))
          ((replique-edn/number-literal? reader ch)
           (push `(replique-edn/read-number ,(char-to-string ch))
                 (symbol-value actions)))
          (t (let ((f (replique-edn/macros ch)))
               (cond ((equal 'replique-edn/read-string* (car f))
                      (push "" (symbol-value result))
                      (push f (symbol-value actions)))
                     (f (push f (symbol-value actions)))
                     (t
                      (replique-edn/reader-read reader ch)
                      (push '(replique-edn/parse-symbol-extended)
                            (symbol-value actions))
                      (push `(replique-edn/read-symbol t)
                            (symbol-value actions)))))))))

(defun replique-edn/init-state (reader &optional state)
  (let ((state (or state (make-symbol "state")))
        (actions (make-symbol "actions"))
        (result-state (make-symbol "result-state"))
        (result (make-symbol "result"))
        (tagged-readers '()))
    (set actions '((replique-edn/read*)))
    (set result-state :reading)
    (set result '())
    (set state `((:reader . ,reader)
                 (:actions . ,actions)
                 (:result-state . ,result-state)
                 (:result . ,result)
                 (:tagged-readers . ,tagged-readers)))
    state))

(defun replique-edn/set-reader (state reader)
  (setcdr (assoc :reader (symbol-value state)) reader)
  state)

(defun replique-edn/set-tagged-readers (state readers)
  (setcdr (assoc :tagged-readers (symbol-value state)) readers)
  state)

(defun replique-edn/read (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result) (symbol-value state)))
    (set result-state :reading)
    (while (and (symbol-value actions)
                (equal :reading (symbol-value result-state)))
      (let* ((action (pop (symbol-value actions))))
        (apply (car action) (symbol-value state) (cdr action))))
    (when (equal :reading (symbol-value result-state))
      (set result-state :done))
    state))

(defun replique-edn/read-string (s)
  (->> (replique-edn/reader nil :str s)
       replique-edn/init-state
       replique-edn/read
       replique-edn/result))

(defun replique-edn/pr-str (data)
  (cond ((null data) "nil")
        ((equal t data) "true")
        ((replique-edn/printable-child-p data)
         (replique-edn/print-method data))
        ((numberp data) (format "%s" data))
        ((stringp data) (replique-edn/write-string* data))
        ((symbolp data) (format "%s" data))
        ((vectorp data)
         (format
          "[%s]"
          (s-join " " (mapcar 'replique-edn/pr-str data))))
        ((hash-table-p data)
         (let ((l nil))
           (maphash
            (lambda (x y)
              (push x l)
              (push y l))
            data)
           (format
            "{%s}"
            (s-join
             " "
             (mapcar 'replique-edn/pr-str (reverse l))))))
        ((listp data)
         (format
          "(%s)"
          (s-join " " (mapcar 'replique-edn/pr-str data))))
        (t (error "%s cannot be printed to EDN." data))))

(comment

 (let* ((state1 (-> (replique-edn/reader nil :str "1e3") ;
                     replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "1 "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "\"\\377\" ")
                   replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader nil :str "\"\\n\" ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader nil :str "\"\\uE000\" ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader nil :str ":e/r ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader nil :str ":e")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "/r "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str ";aze\n:e ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader nil :str "(1 2 (3)) ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader
                   nil :str "(1 22 ();d
 \"3\") ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader nil :str "(1 ;az")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "e\n:ef ) "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let* ((state1 (-> (replique-edn/reader nil :str "(1 (\"e")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "\" 3) 2) "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "[1 22 [3];dfsdf
 \"3\"] ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader nil :str "[1 (\"e")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "\" 3) 2] "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let* ((state1 (-> (replique-edn/reader nil :str "[1")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str " 2] "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "{:e [3] 3 4}")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader nil :str "{:e [")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "3] 3 \"tt\"}"))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "\\e ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader nil :str "\\uE000 ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader nil :str "\\")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "uE000 "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                                 replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "#{} ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader nil :str "#{1 (1 ")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "2) 3} "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "rr/sss ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader nil :str "rr/")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "ss "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader
                   nil :str "#inst \"1985-04-12T23:20:50.52Z\"")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader
                   nil :str
                   "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader
                     nil :str "#inst \"1985-04-12T23:20")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str ":50.52Z\""))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let* ((state1 (-> (replique-edn/reader nil :str "#in")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader
                  nil :str "st \"1985-04-12T23:20:50.52Z\""))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "#_ee r ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader nil :str "#_e")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "e tt "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                    replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "#rr/sss{:e 2} ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader nil :str "#rr/sss{:e 2} ")
                  replique-edn/init-state
                  (replique-edn/set-tagged-readers
                   `((rr/sss . ,(lambda (x) x)))))))
   (replique-edn/state-print (replique-edn/read state)))

 (let* ((state1 (-> (replique-edn/reader
                     nil :str "#inst \"1985-04-")
                    replique-edn/init-state))
        (reader2 (replique-edn/reader nil :str "12T23:20:50.52Z\") "))
        (state (replique-edn/read state1)))
   (replique-edn/state-print (-> (replique-edn/set-reader state reader2)
                                 replique-edn/read)))

 (let ((state (-> (replique-edn/reader nil :str "\"\\\"#ob\\\"\" ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader nil :str "#'ff ")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))

 (let ((state (-> (replique-edn/reader nil :str "{:result #\"fff\"}
")
                  replique-edn/init-state)))
   (replique-edn/state-print (replique-edn/read state)))





  )

(provide 'replique-edn)

;;; replique-edn.el ends here
