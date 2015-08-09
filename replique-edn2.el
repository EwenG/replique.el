;;; replique-edn.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique-edn2/safe-elt (vector i)
  (let ((l (length vector)))
    (if (and (> l 0) (< i l))
        (elt vector i)
      0)))

(defclass replique-edn2/reader ()
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

(defmethod replique-edn2/reader-read
  ((r replique-edn2/reader) &optional u-char)
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
        (t (let ((res (replique-edn2/safe-elt
                       (oref r str)
                       (oref r index))))
             (oset r index (1+ (oref r index)))
             res))))

(defmethod replique-edn2/reader-peek-char
    ((r replique-edn2/reader))
  (let ((ch (replique-edn2/reader-read r)))
    (when (not (equal 0 ch))
      (replique-edn2/reader-read r ch))
    ch))

(defmethod replique-edn2/reader-rest-string
  ((r replique-edn2/reader))
  (cond
   ((> (oref r index) (length (oref r str)))
    "")
   (t
    (concat
     (char-to-string (or (oref r unread-char) ""))
     (char-to-string (or (oref r unread-char2) ""))
     (substring (oref r str) (oref r index))))))

(defun state-print (state)
  (-let (((&alist :reader reader
                  :actions actions
                  :result-state result-state
                  :result result) (symbol-value state)))
    `((:reader . ,reader)
      (:actions . ,(symbol-value actions))
      (:result-state . ,(symbol-value result-state))
      (:result . ,(symbol-value result)))))









(defun replique-edn2/is-separator (ch)
  (or (equal ?\s ch)
      (equal ?\, ch)
      (equal ?\n ch)))

(defun replique-edn2/digit (ch base)
  (if (equal ?0 ch) 0
    (let ((res (string-to-number
                (char-to-string ch)
                base)))
      (if (equal 0 res) -1 res))))





(defun replique-edn2/dispatch-macros (ch)
  (cond
   ;; ((equal ch ?^) (replique-edn2/read-meta)))) ;Not supported
   ((equal ch ?\{) '(replique-edn2/read-set))
   ((equal ch ?<) (error "Unreadable form"))
   ((equal ch ?\;) '(replique-edn2/read-comment))
   ((equal ch ?_) '(replique-edn2/read-discard))
   (t nil)))

(defun replique-edn2/macros (ch)
  (cond ((equal ?\" ch) '(replique-edn2/read-string*))
        ((equal ?: ch) '(replique-edn2/read-keyword))
        ((equal ?\; ch) '(replique-edn2/read-comment))
        ;;((equal ?^ ch) 'replique-edn2/read-meta) Metadata not supported yet
        ((equal ?\( ch) '(replique-edn2/read-list))
        ((equal ?\) ch) `(replique-edn2/read-unmatched-delimiter ,ch))
        ((equal ?\[ ch) '(replique-edn2/read-vector))
        ((equal ?\] ch) `(replique-edn2/read-unmatched-delimiter ,ch))
        ((equal ?\{ ch) '(replique-edn2/read-map))
        ((equal ?\} ch) `(replique-edn2/read-unmatched-delimiter ,ch))
        ((equal ?\\ ch) '(replique-edn2/read-char*))
        ((equal ?# ch) '(replique-edn2/read-dispatch))
        (t nil)))

(defun replique-edn2/is-numeric (ch)
  (and (>= ch 48) (<= ch 57)))

(defun replique-edn2/number-literal? (reader initch)
  (or (replique-edn2/is-numeric initch)
      (and (or (equal ?+ initch)
               (equal ?- initch))
           (replique-edn2/is-numeric
            (replique-edn2/reader-peek-char reader)))))

(defvar replique-edn2/int-pattern "^\\([-+]?\\)\\(?:\\(0\\)\\|\\([1-9][0-9]*\\)\\|0[xX]\\([0-9A-Fa-f]+\\)\\|0\\([0-7]+\\)\\|\\([1-9][0-9]?\\)[rR]\\([0-9A-Za-z]+\\)\\|0[0-9]+\\)\\(N\\)?$")
(defvar replique-edn2/float-pattern "^\\([-+]?[0-9]+\\(\\.[0-9]*\\)?\\([eE][-+]?[0-9]+\\)?\\)\\(M\\)?$")
(defvar replique-edn2/ratio-pattern "^\\([-+]?[0-9]+\\)/\\([0-9]+\\)$")

(defun replique-edn2/match-int (s)
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

(defun replique-edn2/match-float (s)
  (if (match-string-no-properties 4 s)
      (error "Bignum is not supported")
    (string-to-number s)))

(defun replique-edn2/match-ratio (s)
  (let* ((numerator (match-string-no-properties 1 s))
         (denominator (match-string-no-properties 2 s))
         (numerator (if (string-suffix-p "+" numerator)
                        (substring numerator 1)
                      numerator)))
    (/ (float (string-to-number numerator))
       (float (string-to-number denominator)))))

(defun replique-edn2/match-number (s)
  (save-match-data
    (if (string-match replique-edn2/int-pattern s)
        (replique-edn2/match-int s)
      (if (string-match replique-edn2/float-pattern s)
          (replique-edn2/match-float s)
        (when (string-match replique-edn2/ratio-pattern s)
          (replique-edn2/match-ratio s))))))

(defun replique-edn2/read-number (state sb)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader)))
    (while (and (not (replique-edn2/is-separator ch))
                (not (replique-edn2/macros ch))
                (not (equal 0 ch)))
      (setq sb (concat sb (char-to-string ch)))
      (setq ch (replique-edn2/reader-read reader)))
    (if (equal 0 ch)
        (progn (push `(replique-edn2/read-number ,sb)
                     (symbol-value actions))
               (set result-state :waiting))
      (progn (replique-edn2/reader-read reader ch)
             (let ((number (replique-edn2/match-number sb)))
               (if number
                   (push number (symbol-value result))
                 (error "Invalid number format [ %s ]" sb)))))))

(defun replique-edn2/is-macro-terminating (ch)
  (and (not (equal ?# ch))
       (not (equal ?` ch))
       (not (equal ?: ch))
       (replique-edn2/macros ch)))

(defun replique-edn2/not-constituent (ch)
  (or (equal ?@ ch)
      (equal ?` ch)
      (equal ?~ ch)))

(defun replique-edn2/read-unicode-char* (token offset length base i uc)
  (let ((l (+ offset length)))
    (when (not (equal (length token) l))
      (error "Invalid unicode character: \\%s" token))
    (if (equal i l)
        uc
      (let ((d (replique-edn2/digit (elt token i) base)))
        (if (equal -1 d)
            (error "Invalid digit: %c" (elt token i))
          (replique-edn2/read-unicode-char*
           token offset length base (1+ i) (+ d (* uc base))))))))

(defun replique-edn2/read-token* (state sb)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader)))
    (while (and (not (replique-edn2/is-separator ch))
                (not (replique-edn2/is-macro-terminating ch))
                (not (equal 0 ch)))
      (when (replique-edn2/not-constituent ch)
        (error "Invalid constituent character: %c" ch))
      (setq sb (concat sb (char-to-string ch)))
      (setq ch (replique-edn2/reader-read reader)))
    (if (equal 0 ch)
        (progn
          (push `(replique-edn2/read-token* ,sb)
                (symbol-value actions))
          (set result-state :waiting))
      (progn
        (replique-edn2/reader-read reader ch)
        (push sb (symbol-value result))))))

(defun replique-edn2/read-token (state validate-leading)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (initch (replique-edn2/reader-peek-char reader)))
    (cond ((equal 0 initch)
           (push `(replique-edn2/read-token ,initch ,validate-leading)
                 (symbol-value actions)))
          ((and validate-leading
                (replique-edn2/not-constituent initch))
           (error "Invalid leading character: %c" initch))
          (t (replique-edn2/read-token* state "")))))

(defun replique-edn2/token-to-char (state)
  (-let* (((&alist :actions actions
                   :result result)
           state)
          (token (pop (symbol-value result)))
          (ch (cond
               ((equal 1 (length token)) (string-to-char token))
               ((string= token "newline") ?\n)
               ((string= token "space") ?\s)
               ((string= token "tab") ?\t)
               ((string= token "backspace") ?\b)
               ((string= token "formfeed") ?\f)
               ((string= token "return") ?\r)
               ((string-prefix-p "u" token)
                (replique-edn2/read-unicode-char*
                 token 1 4 16 1 0))
               ((string-prefix-p "o" token)
                (let ((len (1- (length token))))
                  (if (> len 3)
                      (error "Invalid octal escape sequence length: %d"
                             length)
                    (let ((uc (replique-edn2/read-unicode-char*
                               token 1 len 8 1 0)))
                      (if (> uc 255)
                          (error
                           "Octal escape sequence must be in range [0,377]")
                        uc)))))
               (t (error "Unsupported character: \\%s" token)))))
    (push ch (symbol-value result))))

(defun replique-edn2/read-char* (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader)))
    (if (equal 0 ch)
        (progn (push '(replique-edn2/read-char*)
                     (symbol-value actions))
               (set result-state :waiting))
      (if (or (replique-edn2/is-macro-terminating ch)
              (replique-edn2/not-constituent ch)
              (replique-edn2/is-separator ch))
          (push (char-to-string ch) (symbol-value result))
        (progn (replique-edn2/reader-read reader ch)
               (push '(replique-edn2/token-to-char)
                     (symbol-value actions))
               (push `(replique-edn2/read-token nil)
                     (symbol-value actions)))))))

(defun replique-edn2/check-first-unicode-char (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader)))
    (cond ((equal 0 ch)
           (push
            '(replique-edn2/check-first-unicode-char)
            (symbol-value actions))
           (set result-state :waiting))
          ((equal (replique-edn2/digit ch 16) -1)
           (error "Invalid digit: %c" ch))
          (t (push
              `(replique-edn2/read-unicode-char
                16 4 t ,(replique-edn2/digit ch 16) 1)
              (symbol-value actions))))))

(defun replique-edn2/read-unicode-char (state base length exact uc i)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (if (not (equal i length))
        (let ((ch (replique-edn2/reader-peek-char reader)))
          (cond
           ((equal 0 ch)
            (push
             `(replique-edn2/read-unicode-char
               ,base ,length ,exact ,uc ,i)
             (symbol-value actions))
            (set result-state :waiting))
           ((or (replique-edn2/is-separator ch)
                (replique-edn2/macros ch))
            (if exact
                (error "Invalid character length: %d, should be: %d"
                       i length)
              (push uc (symbol-value result))))
           (t (let ((d (replique-edn2/digit ch base)))
                (replique-edn2/reader-read reader)
                (if (equal -1 d)
                    (error "Invalid digit: %c" ch)
                  (replique-edn2/read-unicode-char
                   state base length exact
                   (+ d (* uc base)) (1+ i))
                  (symbol-value actions))))))
      (push uc (symbol-value result)))))

(defun replique-edn2/check-unicode-char (state)
  (-let* (((&alist :result result) state)
          (ch (car (symbol-value result))))
    (if (> ch 255)
        (error "Octal escape sequence must be in range [0, 377]")
      ch)))

(defun replique-edn2/escape-char (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader))
          (ec (cond ((equal 0 ch) 0)
                    ((equal ?t ch) ?\t)
                    ((equal ?r ch) ?\r)
                    ((equal ?n ch) ?\n)
                    ((equal ?\\ ch) ?\\)
                    ((equal ?\" ch) ?\")
                    ((equal ?b ch) ?\b)
                    ((equal ?f ch) ?\f)
                    ((equal ?u ch) ?u)
                    ((replique-edn2/is-numeric ch) ch)
                    (t (error "Unsupported escape character: \\%c" ch)))))
    (cond ((equal 0 ec)
           (push '(replique-edn2/escape-char) (symbol-value actions))
           (set result-state :waiting))
          ((equal ?u ec)
           (push '(replique-edn2/check-first-unicode-char)
                 (symbol-value actions)))
          ((replique-edn2/is-numeric ec)
           (when (equal -1 (replique-edn2/digit ec 8))
               (error "Invalid digit: %c" initch))
           (push '(replique-edn2/check-unicode-char)
                 (symbol-value actions))
           (push `(replique-edn2/read-unicode-char
                   8 3 nil ,(replique-edn2/digit ec 8) 1)
                 (symbol-value actions)))
          (t (push ec (symbol-value result))))))

(defun replique-edn2/append-char (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (pop (symbol-value result)))
          (sb (pop (symbol-value result))))
    (setq sb (concat sb (char-to-string ch)))
    (push sb (symbol-value result))))

(defun replique-edn2/read-string* (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (sb (pop (symbol-value result)))
          (ch (replique-edn2/reader-read reader)))
    (while (and (not (equal ?\" ch))
                (not (equal 0 ch))
                (not (equal ?\\ ch)))
      (setq sb (concat sb (char-to-string ch)))
      (setq ch (replique-edn2/reader-read reader)))
    (push sb (symbol-value result))
    (cond ((equal 0 ch)
           (push '(replique-edn2/read-string*) (symbol-value actions))
           (set result-state :waiting))
          ((equal ?\\ ch)
           (push `(replique-edn2/read-string*) (symbol-value actions))
           (push `(replique-edn2/append-char) (symbol-value actions))
           (push `(replique-edn2/escape-char) (symbol-value actions)))
          (t nil))))

(defun replique-edn2/write-string* (str)
  (let ((idx 0)
        (chars nil))
    (while (< idx (length str))
      (let ((ch (elt str idx)))
        (cond ((or (equal ?\\ ch)
                   (equal ?\t ch)
                   (equal ?\r ch)
                   (equal ?\n ch)
                   (equal ?\" ch)
                   (equal ?\b ch)
                   (equal ?\f ch))
               (push ?\\ chars)
               (push ch chars))
              (t (push ch chars))))
      (setq idx (1+ idx)))
    (format "\"%s\""
            (apply 'string (reverse chars)))))

(defun replique-edn2/parse-symbol (state)
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
                               (when (and (not (replique-edn2/is-numeric
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

(defun replique-edn2/parse-keyword (state)
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

(defun replique-edn2/duplicate (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (val (car (symbol-value result))))
    (push val (symbol-value result))))

(defun replique-edn2/read-keyword (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader)))
    (cond ((equal 0 ch)
           (push '(replique-edn2/read-keyword)
                 (symbol-value actions))
           (set result-state :waiting))
          ((replique-edn2/is-separator ch)
           (error "Invalid token: :"))
          (t
           (replique-edn2/reader-read reader ch)
           (push '(replique-edn2/parse-keyword)
                 (symbol-value actions))
           (push '(replique-edn2/parse-symbol)
                 (symbol-value actions))
           (push '(replique-edn2/duplicate)
                 (symbol-value actions))
           (push `(replique-edn2/read-token t)
                 (symbol-value actions))))))

(defun replique-edn2/parse-ns-symbol (state)
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

(defun replique-edn2/parse-symbol-extended (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (token (pop (symbol-value result))))
    (cond ((string= token "nil")
           (push nil (symbol-value result)))
          ((string= token "true")
           (push t (symbol-value result)))
          ((string= token "false")
           (push nil (symbol-value result)))
          ((string= token "/")
           (push '/ (symbol-value result)))
          ((string= token "NaN")
           (push 0.0e+NaN (symbol-value result)))
          ((string= token "-Infinity")
           (push -1.0e+INF (symbol-value result)))
          ((string= token "+Infinity")
           (push +1.0e+INF (symbol-value result)))
          ((string= token "Infinity")
           (push +1.0e+INF (symbol-value result)))
          (t
           (push token (symbol-value result))
           (push '(replique-edn2/parse-ns-symbol)
                 (symbol-value actions))
           (push '(replique-edn2/parse-symbol)
                 (symbol-value actions))))))

(defun replique-edn2/read-symbol (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push '(replique-edn2/parse-symbol-extended)
          (symbol-value actions))
    (push `(replique-edn2/read-token t)
          (symbol-value actions))))

(defun replique-edn2/skip-line (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader)))
    (cond ((equal 0 ch)
           (push '(replique-edn2/skip-line)
                 (symbol-value actions))
           (set result-state :waiting))
          ((not (equal ?\n ch))
           (replique-edn2/skip-line state))
          (t (push '(replique-edn2/read*)
                   (symbol-value actions))))))

(defun replique-edn2/read-comment (state)
  (replique-edn2/skip-line state))

(defun replique-edn2/discard (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (pop (symbol-value result))))

(defun replique-edn2/read-discard (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push '(replique-edn2/read*)
          (symbol-value actions))
    (push '(replique-edn2/discard)
          (symbol-value actions))
    (push '(replique-edn2/read*)
          (symbol-value actions))))

(defun replique-edn2/read-past (reader)
  (let ((ch (replique-edn2/reader-read reader)))
    (if (replique-edn2/is-separator ch)
        (replique-edn2/read-past reader)
      ch)))

(defun replique-edn2/append-object (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (o (pop (symbol-value result)))
          (l (pop (symbol-value result))))
    (push o l)
    (push l (symbol-value result))))

(defun replique-edn2/read-unmatched-delimiter (state ch)
  (error "Unmatched delimiter %s" (char-to-string ch)))

(defun replique-edn2/read-delimited (state delim)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/read-past reader)))
    (cond ((equal 0 ch)
           (push `(replique-edn2/read-delimited ,delim)
                 (symbol-value actions))
           (set result-state :waiting))
          ((equal delim ch) nil)
          (t (replique-edn2/reader-read reader ch)
             (push `(replique-edn2/read-delimited ,delim)
                   (symbol-value actions))
             (push '(replique-edn2/append-object)
                   (symbol-value actions))
             (push '(replique-edn2/read*)
                   (symbol-value actions))))))

(defvar replique-edn2/default-data-readers
  (make-hash-table :test #'equal))

(defclass replique-edn2/printable ()
  ()
  :abstract t)

(defmethod replique-edn2/print-method ((o replique-edn2/printable))
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
            (replique-edn2/pr-str
             (replique-edn2/list-to-map l)))))

(defclass replique-edn2/inst (replique-edn2/printable)
  ((high :initarg :high
         :type number)
   (low :initarg :low
        :type number)))

(defmethod replique-edn2/print-method ((o replique-edn2/inst))
  (format
   "#inst %s"
   (format-time-string "\"%Y-%m-%dT%H:%M:%S.52Z\""
                       (list (oref o :high) (oref o :low))
                       :utc)))

(defclass replique-edn2/uuid (replique-edn2/printable)
  ((uuid :initarg :uuid
         :type string)))

(defmethod replique-edn2/print-method ((o replique-edn2/uuid))
  (format
   "#uuid %s"
   (oref o :uuid)))

(puthash 'inst
         (lambda (inst)
           (let ((time (date-to-time inst)))
             (replique-edn2/inst nil
                                 :high (car time)
                                 :low (cadr time))))
         replique-edn2/default-data-readers)

(puthash 'uuid
         (lambda (uuid)
           (replique-edn2/uuid nil :uuid uuid))
         replique-edn2/default-data-readers)

(defun replique-edn2/parse-tagged (state)
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
        (let ((d (gethash tag replique-edn2/default-data-readers)))
          (if d (push (funcall d object) (symbol-value result))
            (error "No reader function for tag %s" tag)))))))

(defun replique-edn2/read-tagged (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push '(replique-edn2/parse-tagged) (symbol-value actions))
    (push '(replique-edn2/read*) (symbol-value actions))
    (push '(replique-edn2/read*) (symbol-value actions))))

(defun replique-edn2/read-dispatch (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader)))
    (if (equal 0 ch)
        (progn (push '(replique-edn2/read-dispatch)
                     (symbol-value actions))
               (set result-state :waiting))
      (let ((dm (replique-edn2/dispatch-macros ch)))
        (if dm
            (push dm (symbol-value actions))
          (progn
            (replique-edn2/reader-read reader ch)
            (push '(replique-edn2/read-tagged)
                  (symbol-value actions))))))))

(defun replique-edn2/parse-list (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (l (pop (symbol-value result))))
    (push (reverse l) (symbol-value result))))

(defun replique-edn2/parse-vector (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (l (pop (symbol-value result))))
    (push (apply 'vector (reverse l))
          (symbol-value result))))

(defun replique-edn2/read-list (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push nil (symbol-value result))
    (push '(replique-edn2/parse-list) (symbol-value actions))
    (push '(replique-edn2/read-delimited ?\)) (symbol-value actions))))

(defun replique-edn2/read-vector (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push nil (symbol-value result))
    (push '(replique-edn2/parse-vector) (symbol-value actions))
    (push '(replique-edn2/read-delimited ?\]) (symbol-value actions))))

(defun replique-edn2/list-to-map* (idx l m)
  (if (> (length l) idx)
      (let ((k (elt l idx))
            (val (elt l (1+ idx))))
        (puthash k val m)
        (replique-edn2/list-to-map* (+ 2 idx) l m))
    m))

(defun replique-edn2/list-to-map (l)
  (let ((m (make-hash-table :test 'equal)))
    (replique-edn2/list-to-map* 0 l m)))

(defun replique-edn2/parse-hashmap (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (l (pop (symbol-value result))))
    (if (equal 1 (logand 1 (length l)))
        (error "Map literal must contain an even number of forms")
      (push (replique-edn2/list-to-map (reverse l))
            (symbol-value result)))))

(defun replique-edn2/read-map (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push nil (symbol-value result))
    (push '(replique-edn2/parse-hashmap) (symbol-value actions))
    (push '(replique-edn2/read-delimited ?\}) (symbol-value actions))))

(defclass replique-edn2/set (replique-edn2/printable)
  ((vals :initarg :vals
         :type (or null cons))))

(defmethod replique-edn2/print-method ((o replique-edn2/set))
  (format
   "#{%s}"
   (s-join " " (mapcar
                'replique-edn2/pr-str
                (oref o :vals)))))

(defun replique-edn2/parse-set (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (l (pop (symbol-value result))))
    (push (replique-edn2/set :nil :vals l)
          (symbol-value result))))

(defun replique-edn2/read-set (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state))
    (push nil (symbol-value result))
    (push '(replique-edn2/parse-set) (symbol-value actions))
    (push '(replique-edn2/read-delimited ?\}) (symbol-value actions))))

(defun replique-edn2/read* (state)
  (-let* (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result)
           state)
          (ch (replique-edn2/reader-read reader)))
    (cond ((equal 0 ch)
           (push '(replique-edn2/read*) (symbol-value actions))
           (set result-state :waiting))
          ((replique-edn2/is-separator ch)
           (push '(replique-edn2/read*) (symbol-value actions)))
          ((replique-edn2/number-literal? reader ch)
           (push `(replique-edn2/read-number ,(char-to-string ch))
                 (symbol-value actions)))
          (t (let ((f (replique-edn2/macros ch)))
               (cond ((equal 'replique-edn2/read-string* (car f))
                      (push "" (symbol-value result))
                      (push f (symbol-value actions)))
                     (f (push f (symbol-value actions)))
                     (t
                      (replique-edn2/reader-read reader ch)
                      (push '(replique-edn2/read-symbol)
                              (symbol-value actions)))))))))

(defun replique-edn2/init-state (reader)
  (let ((state (make-symbol "state"))
        (actions (make-symbol "actions"))
        (result-state (make-symbol "result-state"))
        (result (make-symbol "result"))
        (tagged-readers (make-symbol "tagged-readers")))
    (set actions '((replique-edn2/read*)))
    (set result-state :reading)
    (set result '())
    (set tagged-readers '())
    (set state `((:reader . ,reader)
                 (:actions . ,actions)
                 (:result-state . ,result-state)
                 (:result . ,result)
                 (:tagged-readers . ,tagged-readers)))
    state))

(defun replique-edn2/set-reader (state reader)
  (setcdr (assoc :reader (symbol-value state)) reader)
  state)

(defun replique-edn2/set-tagged-readers (state readers)
  (setcdr (assoc :tagged-readers (symbol-value state)) readers)
  state)

(defun replique-edn2/read (state)
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


(defun replique-edn2/pr-str (data)
  (cond ((null data) "nil")
        ((equal t data) "true")
        ((replique-edn2/printable-child-p data)
         (replique-edn2/print-method data))
        ((numberp data) (format "%s" data))
        ((stringp data) (replique-edn2/write-string* data))
        ((symbolp data) (format "%s" data))
        ((vectorp data)
         (format
          "[%s]"
          (s-join " " (mapcar 'replique-edn2/pr-str data))))
        ((hash-table-p data)
         (let ((l nil))
           (maphash
            (lambda (x y)
              (push y l)
              (push x l))
            data)
           (format
            "{%s}"
            (s-join
             " "
             (mapcar 'replique-edn2/pr-str l)))))
        ((listp data)
         (format
          "(%s)"
          (s-join " " (mapcar 'replique-edn2/pr-str data))))
        (t (error "%s cannot be printed to EDN." data))))

(comment
 (defun state-print (state)
   (-let (((&alist :reader reader
                   :actions actions
                   :result-state result-state
                   :result result) (symbol-value state)))
     `((:reader . ,reader)
       (:actions . ,(symbol-value actions))
       (:result-state . ,(symbol-value result-state))
       (:result . ,(symbol-value result)))))

 (let* ((state1 (-> (replique-edn2/reader nil :str "1e3")
                     replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "1 "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "\"\\377\" ")
                   replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "\"\\37")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "7\" "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "\"\\n\" ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let ((state (-> (replique-edn2/reader nil :str "\"\\uE000\" ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let ((state (-> (replique-edn2/reader nil :str "\"\\387\" ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let ((state (-> (replique-edn2/reader nil :str ":e/r ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str ":e")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "/r "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str ";aze\n:e ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let ((state (-> (replique-edn2/reader nil :str "(1 2 (3)) ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let ((state (-> (replique-edn2/reader
                   nil :str "(1 22 ();d
 \"3\") ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "(1 ;az")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "e\n:ef ) "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "(1 (\"e")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "\" 3) 2) "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "[1 22 [3];dfsdf
 \"3\"] ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "[1 (\"e")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "\" 3) 2] "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "[1")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str " 2] "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "{:e [3] 3 4}")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "{:e [")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "3] 3 \"tt\"}"))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "\\e ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let ((state (-> (replique-edn2/reader nil :str "\\uE000 ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "\\")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "uE000 "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "#{} ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "#{1 (1 ")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "2) 3} "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "rr/sss ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "rr/")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "ss "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader
                   nil :str "#inst \"1985-04-12T23:20:50.52Z\"")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let ((state (-> (replique-edn2/reader
                   nil :str
                   "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader
                     nil :str "#inst \"1985-04-12T23:20")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str ":50.52Z\""))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "#in")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader
                  nil :str "st \"1985-04-12T23:20:50.52Z\""))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "#_ee r ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader nil :str "#_e")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "e tt "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader state reader2)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "#rr/sss{:e 2} ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))

 (let ((state (-> (replique-edn2/reader nil :str "#rr/sss{:e 2} ")
                  replique-edn2/init-state
                  (replique-edn2/set-tagged-readers
                   `((rr/sss . ,(lambda (x) x)))))))
   (state-print (replique-edn2/read state)))

 (let* ((state1 (-> (replique-edn2/reader
                     nil :str "#uuid (e #inst \"1985-04-")
                    replique-edn2/init-state))
        (reader2 (replique-edn2/reader nil :str "12T23:20:50.52Z\") "))
        (state (replique-edn2/read state1)))
   (state-print (-> (replique-edn2/set-reader reader2 state)
                    replique-edn2/read)))

 (let ((state (-> (replique-edn2/reader nil :str "\"\\\"#ob\\\"\" ")
                  replique-edn2/init-state)))
   (state-print (replique-edn2/read state)))



 )

(provide 'replique-edn2)

;;; replique-edn2.el ends here
