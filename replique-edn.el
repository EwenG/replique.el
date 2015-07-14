;;; replique-edn.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

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

(defclass replique-edn/continuation ()
  ((f :initarg :f)))

(defmethod replique-edn/contcall ((cont replique-edn/continuation) rdr)
  (funcall (oref cont f) rdr))

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
   (index :initarg :index
          :initform 0
          :type number)))

(defmethod replique-edn/reader-read
  ((r replique-edn/reader) &optional u-char)
  (cond ((and u-char (oref r unread-char))
         (error "Cannot unread multiple characters."))
        (u-char (oset r unread-char u-char))
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
  (cond ((and
          (null (oref r unread-char))
          (equal
           (oref r index)
           (length (oref r str))))
         nil)
        ((not (null (oref r unread-char)))
         (concat (char-to-string (oref r unread-char))
                 (substring (oref r str) (oref r index))))
        (t (substring (oref r str) (oref r index)))))

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

(defun replique-edn/is-numeric (ch)
  (and (>= ch 48) (<= ch 57)))

(defun replique-edn/number-literal? (reader initch)
  (or (replique-edn/is-numeric initch)
      (and (or (equal ?+ initch)
               (equal ?- initch))
           (replique-edn/is-numeric
            (replique-edn/peek-char reader)))))

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

(defun replique-edn/read-number (reader sb opts)
  (let ((ch (replique-edn/reader-read reader)))
    (while (and (not (replique-edn/is-separator ch))
                (not (replique-edn/macros ch)))
      (when (equal 0 ch)
        (throw 'continuation
               (replique-edn/continuation
                nil :f
                (lambda (reader)
                  (replique-edn/read-number reader sb opts)))))
      (setq sb (concat sb (char-to-string ch)))
      (setq ch (replique-edn/reader-read reader)))
    (replique-edn/reader-read reader ch)
    (or (replique-edn/match-number sb)
        (error "Invalid number format [ %s ]" sb))))

(defun replique-edn/read-unicode-char* (token offset length base i uc)
  (let ((l (+ offset length)))
    (when (not (equal (length token) l))
      (error "Invalid unicode character: \\%s" token))
    (if (equal i l)
        uc
      (let ((d (replique-edn/digit (elt token i) base)))
        (if (equal -1 d)
            (error "Invalid digit: %c" (elt token i))
          (replique-edn/read-unicode-char*
           token offset length base (1+ i) (+ d (* uc base))))))))

(defun replique-edn/read-unicode-char (rdr initch base length exact uc i)
  (if (equal uc -1)
      (error "Invalid digit: %c" initch)
    (if (not (equal i length))
        (let ((ch (replique-edn/reader-peek-char rdr)))
          (cond
           ((equal 0 ch) (throw 'continuation
                                (replique-edn/continuation
                                 nil :f
                                 (lambda (rdr)
                                   (replique-edn/read-unicode-char
                                    rdr initch base length exact uc i)))))
           ((or (replique-edn/is-separator ch)
                (replique-edn/macros ch))
            (if exact
                (error "Invalid character length: %d, should be: %d"
                       i length)
              uc))
           (t (let ((d (replique-edn/digit ch base)))
                (replique-edn/reader-read rdr)
                (if (equal -1 d)
                    (error "Invalid digit: %c" ch)
                  (replique-edn/read-unicode-char
                   rdr initch base length
                   exact (+ d (* uc base)) (1+ i)))))))
      uc)))

(defun replique-edn/escape-char (rdr)
  (let ((ch (replique-edn/reader-read rdr)))
    (cond ((equal 0 ch)
           (throw 'continuation
                  (replique-edn/continuation
                   nil :f
                   (lambda (rdr)
                     (replique-edn/escape-char rdr)))))
          ((equal ?t ch) ?\t)
          ((equal ?r ch) ?\r)
          ((equal ?n ch) ?\n)
          ((equal ?\\ ch) ?\\)
          ((equal ?\" ch) ?\")
          ((equal ?b ch) ?\b)
          ((equal ?f ch) ?\f)
          ((equal ?u ch)
           (let ((ch (replique-edn/reader-read rdr)))
             (cond ((equal 0 ch)
                    (throw 'continuation
                           (replique-edn/continuation
                            nil :f
                            (lambda (rdr)
                              (replique-edn/escape-char rdr)))))
                   ((equal -1 (replique-edn/digit ch 16))
                    (error "Invalid unicode escape: \\u%c" ch))
                   (t (replique-edn/read-unicode-char
                       rdr ch 16 4 t (replique-edn/digit ch 16) 1)))))
          ((replique-edn/is-numeric ch)
           (let ((ch (replique-edn/read-unicode-char
                      rdr ch 8 3 nil (replique-edn/digit ch 8) 1)))
             (if (> ch 255)
                 (error "Octal escape sequence must be in range [0, 377]")
               ch)))
          (t (error "Unsupported escape character: \\%c" ch)))))

(defun replique-edn/read-string* (reader sb opts &optional cont)
  (let ((ch (replique-edn/reader-read reader)))
    (while (not (equal ?\" ch))
      (when (equal 0 ch)
        (throw 'continuation
               (replique-edn/continuation
                nil :f
                (lambda (reader)
                  (replique-edn/read-string* reader sb opts cont)))))
      (if (equal ?\\ ch)
          (let ((esc-c
                 (catch 'continuation
                   (if cont
                       (replique-edn/contcall cont reader)
                     (replique-edn/escape-char reader)))))
            (when (replique-edn/continuation-p esc-c)
              (throw 'continuation
                     (replique-edn/continuation
                      nil :f
                      (lambda (rdr)
                        (replique-edn/reader-read rdr ch)
                        (replique-edn/read-string*
                         rdr sb opts esc-c)))))
            (setq sb (concat sb (char-to-string esc-c))))
        (setq sb (concat sb (char-to-string ch))))
      (setq cont nil)
      (setq ch (replique-edn/reader-read reader))))
  sb)

(defun replique-edn/write-string* (str)
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

(defun replique-edn/not-constituent (ch)
  (or (equal ?@ ch)
      (equal ?` ch)
      (equal ?~ ch)))

(defun replique-edn/is-macro-terminating (ch)
  (and (not (equal ?# ch))
       (not (equal ?` ch))
       (not (equal ?: ch))
       (replique-edn/macros ch)))

(defun replique-edn/read-token* (rdr sb ch)
  (while (and (not (replique-edn/is-separator ch))
              (not (replique-edn/is-macro-terminating ch)))
    (when (equal 0 ch)
      (throw 'continuation
             (replique-edn/continuation
              nil :f
              (lambda (rdr)
                (replique-edn/read-token*
                 rdr sb (replique-edn/reader-peek-char rdr))))))
    (when (replique-edn/not-constituent ch)
      (error "Invalid constituent character: %c" ch))
    (setq sb (concat sb (char-to-string (replique-edn/reader-read rdr))))
    (setq ch (replique-edn/reader-peek-char rdr)))
  sb)

(defun replique-edn/read-token (rdr initch validate-leading)
  (cond ((equal 0 initch)
         (throw 'continuation
                (replique-edn/continuation
                 nil :f
                 (lambda (rdr)
                   (replique-edn/read-token
                    rdr (replique-edn/reader-read rdr) validate-leading)))))
        ((and validate-leading
              (replique-edn/not-constituent initch))
         (error "Invalid leading character: %c" initch))
        (t (replique-edn/reader-read rdr initch)
           (replique-edn/read-token* rdr "" initch))))

(defun replique-edn/parse-symbol (token)
  (when (and (not (string= "" token))
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
          (list nil token))))))

(defun replique-edn/read-keyword (reader sb opts &optional token-cont)
  (let ((ch (replique-edn/reader-read reader)))
    (if (not (replique-edn/is-separator ch))
        (let ((token (catch 'continuation
                       (if token-cont
                           (replique-edn/contcall token-cont reader)
                         (replique-edn/read-token reader ch t)))))
          (when (replique-edn/continuation-p token)
            (throw
             'continuation
             (replique-edn/continuation
              nil :f
              (lambda (rdr)
                (replique-edn/reader-read rdr ch)
                (replique-edn/read-keyword
                 rdr sb opts token)))))
          (let ((s (replique-edn/parse-symbol token)))
            (if (and s (not (string-match-p "::" token)))
                (let ((ns (car s))
                      (name (cadr s)))
                  (if (equal ?: (elt token 0))
                      (error "Invalid token: :%s" token)
                    (if ns
                        (intern (concat ":" ns "/" name))
                      (intern (concat ":" name)))))
              (error "Invalid token: :%s" token))))
      (error "Invalid token: :"))))

(defun replique-edn/skip-line (rdr)
  (let ((ch (replique-edn/reader-read rdr)))
    (cond ((equal 0 ch)
           (throw 'continuation
                  (replique-edn/continuation
                   nil :f
                   (lambda (rdr)
                     (replique-edn/skip-line rdr)))))
          ((not (equal ?\n ch))
           (replique-edn/skip-line rdr))
          (t rdr))))

(defun replique-edn/read-comment (rdr sb opts)
  (replique-edn/skip-line rdr))

(defun replique-edn/read-past (pred rdr)
  (let ((ch (replique-edn/reader-read rdr)))
    (if (funcall pred ch)
        (replique-edn/read-past pred rdr)
      ch)))

(defun replique-edn/read-delimited (delim rdr a opts &optional cont)
  (let* ((ch (replique-edn/read-past
              'replique-edn/is-separator rdr)))
    (while (not (equal delim ch))
      (when (equal 0 ch)
        (throw 'continuation
               (replique-edn/continuation
                nil :f
                (lambda (rdr)
                  (replique-edn/read-delimited
                   delim rdr a opts)))))
      (let ((macrofn (funcall 'replique-edn/macros ch)))
        (if macrofn
            (let ((mret (catch 'continuation
                          (if cont
                              (replique-edn/contcall cont rdr)
                            (funcall macrofn rdr "" opts)))))
              (cond ((replique-edn/continuation-p mret)
                     (throw 'continuation
                            (replique-edn/continuation
                             nil :f
                             (lambda (rdr)
                               (replique-edn/reader-read rdr ch)
                               (replique-edn/read-delimited
                                delim rdr a opts mret)))))
                    ((replique-edn/reader-p mret)
                     nil)
                    (t (setq a (cons mret a)))))
          (progn
            (when (not cont)
              (replique-edn/reader-read rdr ch))
            (let* ((o (catch 'continuation
                        (if cont
                            (replique-edn/contcall cont rdr)
                          (replique-edn/read rdr opts)))))
              (if (replique-edn/continuation-p o)
                  (throw 'continuation
                         (replique-edn/continuation
                          nil :f
                          (lambda (rdr)
                            (replique-edn/reader-read rdr ch)
                            (replique-edn/read-delimited
                             delim rdr a opts o))))
                (setq a (cons o a)))))))
      (setq cont nil)
      (setq ch (replique-edn/read-past
                'replique-edn/is-separator rdr)))
    (reverse a)))

(defun replique-edn/read-list (rdr sb opts &optional cont)
  (let ((the-list (catch 'continuation
                    (if cont
                        (replique-edn/contcall cont rdr)
                      (replique-edn/read-delimited ?\) rdr nil opts)))))
    (cond ((replique-edn/continuation-p the-list)
           (throw 'continuation
                  (replique-edn/continuation
                   nil :f
                   (lambda (rdr)
                     (replique-edn/read-list rdr sb opts the-list)))))
          (t the-list))))

(defun replique-edn/read-vector (rdr sb opts &optional cont)
  (let ((l (catch 'continuation
             (if cont
                 (replique-edn/contcall cont rdr)
               (replique-edn/read-delimited ?\] rdr nil opts)))))
    (when (replique-edn/continuation-p l)
      (throw 'continuation
             (replique-edn/continuation
              nil :f
              (lambda (rdr)
                (replique-edn/read-vector rdr sb opts l)))))
    (apply 'vector l)))

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

(defun replique-edn/read-map (rdr sb opts &optional cont)
  (let ((l (catch 'continuation
             (if cont
                 (replique-edn/contcall cont rdr)
               (replique-edn/read-delimited ?\} rdr nil opts)))))
    (cond ((replique-edn/continuation-p l)
           (throw 'continuation
                  (replique-edn/continuation
                   nil :f
                   (lambda (rdr)
                     (replique-edn/read-map rdr sb opts l)))))
          ((equal 1 (logand 1 (length l)))
           (error "Map literal must contain an even number of forms"))
          (t (replique-edn/list-to-map l)))))

(defun replique-edn/read-unmatched-delimiter (rdr sb opts)
  (error "Unmatched delimiter %s" sb))

(defun replique-edn/read-char* (rdr backslash opts &optional cont)
  (let ((ch (replique-edn/reader-read rdr)))
    (if (equal 0 ch)
        (throw 'continuation
               (replique-edn/continuation
                nil :f
                (lambda (rdr)
                  (replique-edn/read-char* rdr backslash opts cont))))
      (let ((token
              (if (or (replique-edn/is-macro-terminating ch)
                      (replique-edn/not-constituent ch)
                      (replique-edn/is-separator ch))
                  (char-to-string ch)
                (catch 'continuation
                  (if cont
                      (replique-edn/contcall cont rdr)
                    (replique-edn/read-token rdr ch nil))))))
        (when (replique-edn/continuation-p token)
          (throw 'continuation
                 (replique-edn/continuation
                  nil :f
                  (lambda (rdr)
                    (replique-edn/reader-read rdr ch)
                    (replique-edn/read-char*
                     rdr backslash opts token)))))
        (cond
         ((equal 1 (length token)) (string-to-char token))
         ((string= token "newline") ?\n)
         ((string= token "space") ?\s)
         ((string= token "tab") ?\t)
         ((string= token "backspace") ?\b)
         ((string= token "formfeed") ?\f)
         ((string= token "return") ?\r)
         ((string-prefix-p "u" token) (replique-edn/read-unicode-char*
                                       token 1 4 16 1 0))
         ((string-prefix-p "o" token)
          (let ((len (1- (length token))))
            (if (> len 3)
                (error "Invalid octal escape sequence length: %d"
                       length)
              (let ((uc (replique-edn/read-unicode-char*
                         token 1 len 8 1 0)))
                (if (> uc 255)
                    (error
                     "Octal escape sequence must be in range [0,377]")
                  uc)))))
         (t (error "Unsupported character: \\%s" token)))))))

(defclass replique-edn/set (replique-edn/printable)
  ((vals :initarg :vals
         :type (or null cons))))

(defmethod replique-edn/print-method ((o replique-edn/set))
  (format
   "#{%s}"
   (s-join " " (mapcar
                'replique-edn/pr-str
                (oref o :vals)))))

(defun replique-edn/read-set (rdr sb opts &optional cont)
  (let ((l (catch 'continuation
             (if cont
                 (replique-edn/contcall cont rdr)
               (replique-edn/read-delimited ?\} rdr nil opts)))))
    (cond ((replique-edn/continuation-p l)
           (throw 'continuation
                  (replique-edn/continuation
                   nil :f
                   (lambda (rdr)
                     (replique-edn/read-set rdr sb opts l)))))
          (t (replique-edn/set :nil :vals l)))))

(defun replique-edn/read-discard (rdr sb opts &optional cont)
  (let ((o (catch 'continuation
             (if cont
                 (replique-edn/contcall cont rdr)
               (replique-edn/read rdr opts)))))
    (when (replique-edn/continuation-p o)
      (throw 'continuation
             (replique-edn/continuation
              nil :f
              (lambda (rdr)
                (replique-edn/read-discard rdr sb opts o)
                rdr)))))
  rdr)

(defun replique-edn/dispatch-macros (ch)
  (cond
   ;; ((equal ch ?^) (replique-edn/read-meta)))) ;Not supported
   ((equal ch ?\{) 'replique-edn/read-set)
   ((equal ch ?<) (error "Unreadable form"))
   ((equal ch ?\;) 'replique-edn/read-comment)
   ((equal ch ?_) 'replique-edn/read-discard)
   (t nil)))

(defun replique-edn/read-tagged (rdr initch opts &optional cont-t cont-o)
  (let ((tag (catch 'continuation
               (if cont-t
                   (replique-edn/contcall cont-t rdr)
                 (replique-edn/read rdr opts)))))
    (when (replique-edn/continuation-p tag)
      (throw 'continuation
             (replique-edn/continuation
              nil :f
              (lambda (rdr)
                (replique-edn/read-tagged
                 rdr initch opts tag nil)))))
    (let ((object (catch 'continuation
                    (if cont-o
                        (replique-edn/contcall cont-o rdr)
                      (replique-edn/read rdr opts)))))
      (when (replique-edn/continuation-p object)
        (throw 'continuation
               (replique-edn/continuation
                nil :f
                (lambda (rdr)
                  (replique-edn/read-tagged
                   rdr initch opts
                   (replique-edn/continuation
                    nil :f (lambda (rdr) tag))
                   object)))))
      (when (not (symbolp tag))
        (error "Reader tag must be a symbol"))
      (let ((f (cdr (assoc tag (cdr (assoc ':readers opts))))))
        (if f
            (funcall f object)
          (let ((d (gethash tag replique-edn/default-data-readers)))
            (if d (funcall d object)
              (error "No reader function for tag %s" tag))))))))

(defun replique-edn/read-dispatch (rdr sb opts &optional cont)
  (let ((ch (replique-edn/reader-read rdr)))
    (if (equal 0 ch)
        (throw 'continuation
               (replique-edn/continuation
                nil :f
                (lambda (rdr)
                  (replique-edn/read-dispatch rdr sb opts cont))))
      (let ((dm (replique-edn/dispatch-macros ch)))
        (if dm
            (funcall dm rdr (char-to-string ch) opts)
          (progn
            (when (not cont)
              (replique-edn/reader-read rdr ch))
            (let ((obj (catch 'continuation
                         (if cont
                             (replique-edn/contcall cont rdr)
                           (replique-edn/read-tagged rdr ch opts)))))
              (when (replique-edn/continuation-p obj)
                (throw 'continuation
                       (replique-edn/continuation
                        nil :f
                        (lambda (rdr)
                          (replique-edn/reader-read rdr ch)
                          (replique-edn/read-dispatch
                           rdr sb opts obj)))))
              (when (not obj)
                (error "No dispatch macro for %c" ch))
              obj)))))))

(defun replique-edn/macros (ch)
  (cond ((equal ?\" ch) 'replique-edn/read-string*)
        ((equal ?: ch) 'replique-edn/read-keyword)
        ((equal ?\; ch) 'replique-edn/read-comment)
        ;;((equal ?^ ch) 'replique-edn/read-meta) Metadata not supported yet
        ((equal ?\( ch) 'replique-edn/read-list)
        ((equal ?\) ch) 'replique-edn/read-unmatched-delimiter)
        ((equal ?\[ ch) 'replique-edn/read-vector)
        ((equal ?\] ch) 'replique-edn/read-unmatched-delimiter)
        ((equal ?\{ ch) 'replique-edn/read-map)
        ((equal ?\} ch) 'replique-edn/read-unmatched-delimiter)
        ((equal ?\\ ch) 'replique-edn/read-char*)
        ((equal ?# ch) 'replique-edn/read-dispatch)
        (t nil)))

(defun replique-edn/read-symbol (rdr initch &optional cont)
  (let ((token (catch 'continuation
                 (if cont
                     (replique-edn/contcall cont rdr)
                   (replique-edn/read-token rdr initch t)))))
    (when (replique-edn/continuation-p token)
      (throw 'continuation
             (replique-edn/continuation
              nil :f
              (lambda (rdr)
                (replique-edn/read-symbol
                 rdr initch token)))))
    (cond ((string= token "nil") nil)
          ((string= token "true") t)
          ((string= token "false") nil)
          ((string= token "/") '/)
          ((string= token "NaN") 0.0e+NaN)
          ((string= token "-Infinity") -1.0e+INF)
          ((string= token "+Infinity") +1.0e+INF)
          ((string= token "Infinity") +1.0e+INF)
          (t (let ((p (replique-edn/parse-symbol token)))
               (let ((ns (car p))
                     (n (cadr p)))
                 (if ns
                     (intern (concat ns "/" n))
                   (intern n))))))))

(defun replique-edn/read (reader &optional opts cont)
  (let ((ch (replique-edn/reader-read reader)))
    (cond ((replique-edn/is-separator ch)
           (replique-edn/read reader opts))
          ((equal 0 ch)
           (throw 'continuation
                  (replique-edn/continuation
                   nil :f
                   (lambda (rdr)
                     (replique-edn/read rdr opts cont)))))
          ((replique-edn/number-literal? reader ch)
           (replique-edn/read-number reader (char-to-string ch) opts))
          (t (let ((f (replique-edn/macros ch)))
               (if f (let ((res
                            (catch 'continuation
                              (if cont
                                  (replique-edn/contcall cont reader)
                                (funcall f reader "" opts)))))
                       (cond ((replique-edn/reader-p res)
                              (replique-edn/read reader opts))
                             ((replique-edn/continuation-p res)
                              (throw
                               'continuation
                               (replique-edn/continuation
                                nil :f
                                (lambda (rdr)
                                  (replique-edn/reader-read rdr ch)
                                  (replique-edn/read
                                   rdr opts res)))))
                             (t res)))
                 (replique-edn/read-symbol reader ch))))
          (t nil))))


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
              (push y l)
              (push x l))
            data)
           (format
            "{%s}"
            (s-join
             " "
             (mapcar 'replique-edn/pr-str l)))))
        ((listp data)
         (format
          "(%s)"
          (s-join " " (mapcar 'replique-edn/pr-str data))))
        (t (error "%s cannot be printed to EDN." data))))


(comment

 (defmacro with-continuation (body catch-fn)
   (let ((cont (make-symbol "cont")))
     `(let ((,cont (catch (quote continuation) ,body)))
        (if (replique-edn/continuation-p ,cont)
            (funcall ,catch-fn ,cont)
          ,cont))))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "1e3")))
        (replique-edn/read reader))
  (lambda (cont)
    (replique-edn/contcall cont
             (replique-edn/reader nil :str "1 "))))

 (let ((reader (replique-edn/reader nil :str "\"\\377\" ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "\"\\37")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "7\" "))))

 (let ((reader (replique-edn/reader nil :str "\"\\387\" ")))
   (replique-edn/read reader))

 (let ((reader (replique-edn/reader nil :str ":e/r ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str ":e")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "/r "))))

 (let ((reader (replique-edn/reader nil :str ";aze\n:e ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "(1 ;az")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "e\n:e) "))))

 (let ((reader (replique-edn/reader nil :str "(1 22 ();d
 \"3\") ")))
   (replique-edn/read reader))

 (let ((reader (replique-edn/reader nil :str "(2 (1)) ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "(1 (\"e")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "\" 3) 2) "))))

 (let ((reader (replique-edn/reader nil :str "[1 22 [3];dfsdf
 \"3\"] ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "[1 (\"e")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall
     conti
     (replique-edn/reader nil :str "\" 3) 2] "))))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "[1")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall
     conti
     (replique-edn/reader nil :str " 2] "))))

 (let ((reader (replique-edn/reader nil :str "{:e [3] 3 4}")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "{:e [")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "3] 3 \"tt\"}"))))

 (let ((reader (replique-edn/reader nil :str "\\e ")))
   (replique-edn/read reader))

 (let ((reader (replique-edn/reader nil :str "\\uE000 ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "\\")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "uE000 "))))

 (let ((reader (replique-edn/reader nil :str "#{} ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "#{1 (1 ")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "2) 3} "))))

 (let ((reader (replique-edn/reader nil :str "rr/sss ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "rr/")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "ss "))))

 (let ((reader (replique-edn/reader nil :str "#inst \"1985-04-12T23:20:50.52Z\"")))
   (replique-edn/read reader))

 (let ((reader (replique-edn/reader nil :str "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "#inst \"1985-04-12T23:20")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str ":50.52Z\""))))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "#in")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "st \"1985-04-12T23:20:50.52Z\""))))

 (let ((reader (replique-edn/reader nil :str "#_ee r ")))
   (replique-edn/read reader))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "#_e")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall conti
             (replique-edn/reader nil :str "e tt "))))

 (let ((reader (replique-edn/reader nil :str "#rr/sss{:e 2} ")))
   (replique-edn/read reader))

 (let ((reader (replique-edn/reader nil :str "#rr/sss{:e 2} ")))
   (replique-edn/read
    reader
    `((:readers . ((rr/sss . ,(lambda (x) x)))))))

 (with-continuation
  (let ((reader (replique-edn/reader nil :str "#uuid (e #inst \"1985-04-")))
    (replique-edn/read reader))
  (lambda (conti)
    (replique-edn/contcall
     conti
     (replique-edn/reader nil :str "12T23:20:50.52Z\") "))))

 (replique-edn/read (replique-edn/reader
                     nil :str "\"\\\"#ob\\\"\" "))

 )

(provide 'replique-edn)

;;; replique-edn.el ends here
