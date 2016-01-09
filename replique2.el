;;; replique2.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24") (clojure-mode "4.0.1") (dash "2.11.0") (company "0.8.12") (dash-functional "1.2.0") (s "1.9.0"))
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'comint)
(require 'clojure-mode)
(require 'replique-edn2)
(require 'replique-async2)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique2/assoc (alist key val)
  (assq-delete-all key alist)
  (cons `(,key . ,val) alist))

(defun replique2/message-nolog (format-string &rest args)
  (let ((message-log-max nil))
    (apply 'message format-string args)))

(defun replique2/visible-buffers ()
  (let ((buffers '()))
    (walk-windows
     (lambda (window) (push (window-buffer window) buffers))
     nil 'visible)
    buffers))

(defgroup replique2 nil
  ""
  :group 'clojure)

(defcustom replique2/prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique2 mode."
  :type 'regexp
  :group 'replique2)

(defvar replique2/processes nil)

(defun replique2/alist-to-map (alist)
  (let ((m (make-hash-table :test 'equal)))
    (mapcar (-lambda ((k . v))
              (puthash k v m))
            alist)
    m))

(defun replique2/process-filter-chan (proc)
  (let ((chan (replique-async2/chan)))
    (set-process-filter
     proc (lambda (proc string)
            (replique-async2/put! chan string)))
    chan))

(defun replique2/process-props-by (pred &optional error-on-nil)
  (let ((props (-first pred replique2/processes)))
    (if (and (null props) error-on-nil)
        (user-error "No started REPL")
      props)))

(defun replique2/active-process-props (&optional error-on-nil)
  (replique2/process-props-by
   (-lambda ((&alist :active active))
     active)
   error-on-nil))

(defun replique2/process-props-by-host-port
    (host port &optional error-on-nil)
  (replique2/process-props-by
   (-lambda ((&alist :host h :port p))
     (and (string= h host) (equal p port)))
   error-on-nil))

(defun replique2/get-active-clj-repl (&optional error-on-nil)
  (->> (replique2/active-process-props error-on-nil)
       (assoc :active-clj-repl) cdr))

(defun replique2/get-active-cljs-repl (&optional error-on-nil)
  (->> (replique2/active-process-props error-on-nil)
       (assoc :active-cljs-repl) cdr))

(defun replique2/get-repl-by-session
    (repl-type session &optional error-on-nil)
  (let* ((props (replique2/active-process-props error-on-nil))
         (repls (cond ((equal :clj repl-type)
                       (cdr (assoc :clj-repls props)))
                      ((equal :cljs repl-type)
                       (cdr (assoc :cljs-repls props)))
                      (t '()))))
    (-first (lambda (repl)
              (equal (cdr (assoc :session repl)) session))
            repls)))

(defun replique2/get-repl-by-buffer
    (buffer &optional error-on-nil)
  (let* ((props (replique2/active-process-props error-on-nil))
         (clj-repls (cdr (assoc :clj-repls props)))
         (cljs-repls (cdr (assoc :cljs-repls props))))
    (or (-first (lambda (repl)
                  (eq (cdr (assoc :buffer repl)) buffer))
                clj-repls)
        (-first (lambda (repl)
                  (eq (cdr (assoc :buffer repl)) buffer))
                cljs-repls))))

(defun replique2/set-active-process
    (host port &optional display-msg)
  (interactive
   (let* ((buffer-descs (mapcar
                         (-lambda ((&alist :directory dir
                                     :host host
                                     :port port))
                           `(,(format "%s|%s:%s" dir host port)
                             . (,host ,port)))
                         replique2/processes))
          (buffer-names (mapcar 'car buffer-descs))
          (choosen (ido-completing-read
                    "Set active buffer to: "
                    buffer-names nil t)))
     (append '(choosen)
             (cdr (-first
                   (lambda (x) (string= (car x) choosen))
                   buffer-descs))
             '(t))))
  (setq replique2/processes
        (mapcar
         (-lambda (props)
           (-let (((&alist :host proc-host :port proc-port) props))
             (if (and (string= proc-host host)
                      (equal proc-port port))
                 (replique2/assoc props :active t)
               (replique2/assoc props :active nil))))
         replique2/processes))
  (when display-msg
    (message "Active process switched")))

(defun replique2/comint-is-closed-sexpr (start limit)
  (let ((depth (car (parse-partial-sexp start limit))))
    (if (<= depth 0) t nil)))

(defun replique2/comint-send-input (&optional no-newline artificial)
  (interactive)
  (let* ((buff (current-buffer))
         (repl (replique2/get-repl-by-buffer buff))
         (eval-chan (cdr (assoc :eval-chan repl)))
         (proc (get-buffer-process buff)))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc)))
        (cond (;; Point is at the end of the line and the sexpr is
               ;; terminated
               (and (equal (point) (point-max))
                    (replique2/comint-is-closed-sexpr pmark (point)))
               (comint-send-input no-newline artificial)
               (replique-async2/<!
                eval-chan
                (lambda (msg)
                  (when msg
                    (replique2/display-eval-result msg buff)))))
              ;; Point is after the prompt but (before the end of line or
              ;; the sexpr is not terminated)
              ((comint-after-pmark-p) (comint-accumulate))
              ;; Point is before the prompt. Do nothing.
              (t nil))))))

(defun replique2/comint-send-input-from-source
    (input &optional no-newline artificial)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (comint-add-to-input-history input)
      (run-hook-with-args 'comint-input-filter-functions
                          (if no-newline input
                            (concat input "\n")))

      (comint-snapshot-last-prompt)

      (setq comint-save-input-ring-index comint-input-ring-index)
      (setq comint-input-ring-index nil)

      (let ((comint-input-sender-no-newline no-newline))
        (funcall comint-input-sender proc input))

      ;; This used to call comint-output-filter-functions,
      ;; but that scrolled the buffer in undesirable ways.
      (run-hook-with-args 'comint-output-filter-functions ""))))

(defun replique2/keyword-to-string (k)
  (substring (symbol-name k) 1))

(defun replique2/format-eval-message (msg)
  (if (gethash :error msg)
      (let ((value (gethash :value msg))
            (repl-type (gethash :repl-type msg))
            (ns (gethash :ns msg)))
        (if (s-starts-with-p "Error:" value)
            (format "(%s) %s=> %s"
                    (replique2/keyword-to-string repl-type) ns value)
          (format "(%s) %s=> Error: %s"
                  (replique2/keyword-to-string repl-type) ns value)))
    (let ((result (gethash :result msg))
          (repl-type (gethash :repl-type msg))
          (ns (gethash :ns msg)))
      (format "(%s) %s=> %s"
              (replique2/keyword-to-string repl-type) ns result))))

(defun replique2/display-eval-result (msg buff)
  (when (not (-contains? (replique2/visible-buffers) buff))
    (replique2/message-nolog
     (replique2/format-eval-message msg))))

(defun replique2/format-eval-messages (clj-msg cljs-msg)
  (cond ((and clj-msg cljs-msg)
         (replique2/message-nolog
          "%s\n%s"
          (replique2/format-eval-message clj-msg)
          (replique2/format-eval-message cljs-msg)))
        (clj-msg (replique2/message-nolog
                  (replique2/format-eval-message clj-msg)))
        (cljs-msg (replique2/message-nolog
                   (replique2/format-eval-message cljs-msg)))))

(defun replique2/display-eval-results (msg1 msg2 clj-buff cljs-buff)
  (let ((visible-buffers (replique2/visible-buffers))
        (clj-msg (-first (lambda (msg)
                           (equal :clj (gethash :repl-type msg)))
                         (list msg1 msg2)))
        (cljs-msg (-first (lambda (msg)
                            (equal :cljs (gethash :repl-type msg)))
                          (list msg1 msg2))))
    (cond ((and (not (-contains? visible-buffers clj-buff))
                (not (-contains? visible-buffers cljs-buff)))
           (replique2/format-eval-messages clj-msg cljs-msg))
          ((not (-contains? visible-buffers clj-buff))
           (replique2/format-eval-messages clj-msg nil))
          ((not (-contains? visible-buffers cljs-buff))
           (replique2/format-eval-messages nil cljs-msg)))))

(defun replique2/send-input-from-source-dispatch
    (input &optional no-newline artificial)
  (-let ((extension (file-name-extension (buffer-name)))
         ((&alist :buffer clj-buff
                  :eval-chan clj-eval-chan)
          (replique2/get-active-clj-repl))
         ((&alist :buffer cljs-buff
                  :eval-chan cljs-eval-chan)
          (replique2/get-active-cljs-repl)))
    (cond ((string= "clj" extension)
           (if clj-buff
               (progn (with-current-buffer
                          clj-buff
                        (replique2/comint-send-input-from-source input))
                      (replique-async2/<!
                       clj-eval-chan
                       (lambda (msg)
                         (when msg
                           (replique2/display-eval-result msg clj-buff)))))
             (user-error "No active Clojure buffer")))
          ((string= "cljs" extension)
           (if cljs-buff
               (progn
                 (with-current-buffer
                     cljs-buff
                   (replique2/comint-send-input-from-source input))
                 (replique-async2/<!
                  cljs-eval-chan
                  (lambda (msg)
                    (when msg
                      (replique2/display-eval-result msg cljs-buff)))))
             (user-error "No active Clojurescript buffer")))
          ((string= "cljc" extension)
           (if (or clj-buff cljs-buff)
               (progn
                 (when clj-buff
                   (with-current-buffer
                       clj-buff
                     (replique2/comint-send-input-from-source input)))
                 (when cljs-buff
                   (with-current-buffer
                       cljs-buff
                     (replique2/comint-send-input-from-source input)))
                 (cond ((and clj-buff cljs-buff)
                        (let ((chan (replique-async2/chan)))
                          (replique-async2/<!
                           clj-eval-chan
                           (lambda (msg)
                             (replique-async2/put! chan msg)))
                          (replique-async2/<!
                           cljs-eval-chan
                           (lambda (msg)
                             (replique-async2/put! chan msg)))
                          (replique-async2/<!
                           chan
                           (lambda (msg1)
                             (replique-async2/<!
                              chan
                              (lambda (msg2)
                                (replique2/display-eval-results
                                 msg1 msg2 clj-buff cljs-buff)
                                (replique-async2/close! chan)))))))
                       (clj-buff
                        (replique-async2/<!
                         clj-eval-chan
                         (lambda (msg)
                           (when msg
                             (replique2/display-eval-result
                              msg clj-buff)))))
                       (cljs-buff
                        (replique-async2/<!
                         cljs-eval-chan
                         (lambda (msg)
                           (when msg
                             (replique2/display-eval-result
                              msg cljs-buff)))))))
             (user-error "No active Clojure or Clojurescript buffer")))
          (t (user-error
              (format "Cannot eval from buffer %s" (current-buffer)))))))

(defun replique2/eval-region (start end)
  "Eval the currently highlighted region."
  (interactive "r")
  (let ((input (filter-buffer-substring start end)))
    (replique2/send-input-from-source-dispatch input)))

(defun replique2/eval-last-sexp ()
  "Eval the previous sexp."
  (interactive)
  (replique2/eval-region
   (save-excursion
     (clojure-backward-logical-sexp 1) (point))
   (point)))

(defun replique2/eval-defn ()
  "Eval the current defn expression."
  (interactive)
  (let ((input (->> (thing-at-point 'defun)
                    (replace-regexp-in-string "\n" ""))))
    (replique2/send-input-from-source-dispatch input)))

(defvar replique2/prev-dir/file nil
  "PREV-DIR/FILE is the (DIRECTORY . FILE) pair
from the last source processing command.
Used by comint-get-source")

(defun replique2/init-load-file (file-name)
  (comint-check-source file-name)
  (setq replique2/prev-dir/file
        (cons (file-name-directory file-name)
              (file-name-nondirectory file-name)))
  (message "Loading file: %s ..." file-name))

(defun replique2/load-file (file-name)
  (interactive (comint-get-source
                "Load file: "
                replique2/prev-dir/file
                (list major-mode) t))
  "Load a Clojure/Clojurescript file."
  (replique2/init-load-file file-name))

(defvar replique2/mode-hook '()
  "Hook for customizing replique2 mode.")

(defvar replique2/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-m" 'replique2/comint-send-input)
    map))

(define-derived-mode replique2/mode comint-mode "Replique2"
  "Commands:\\<replique2/mode-map>"
  (setq comint-prompt-regexp replique2/prompt)
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  ;;(add-to-list 'company-backends 'replique2/company-backend)
  )

(defvar replique2/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (comment (define-key map "\C-c\C-r" 'replique2/eval-region)
             (define-key map "\C-x\C-e" 'replique2/eval-last-sexp)
             (define-key map "\C-\M-x" 'replique2/eval-defn)
             (define-key map "\C-c\C-l" 'replique2/load-file)
             (easy-menu-define replique2/minor-mode-menu map
               "Replique Minor Mode Menu"
               '("Replique"
                 ["Eval region" replique2/eval-region t]
                 ["Eval last sexp" replique2/eval-last-sexp t]
                 ["Eval defn" replique2/eval-defn t]
                 "--"
                 ["Load file" replique2/load-file t])))
    map))

(defvar replique2/generic-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;;;###autoload
(define-minor-mode replique2/minor-mode
  "Minor mode for interacting with the replique2 process buffer.

The following commands are available:

\\{replique2/minor-mode-map}"
  :lighter "Replique2" :keymap replique2/minor-mode-map
  (comment (make-local-variable 'company-backends)))

;;;###autoload
(define-minor-mode replique2/generic-minor-mode
  "Minor mode for interacting with the replique2 process buffer.

The following commands are available:

\\{replique2/generic-minor-mode-map}"
  :lighter "Replique2" :keymap replique2/generic-minor-mode-map)

(defun replique2/is-valid-port-nb? (port-nb)
  (< -1 port-nb 65535))

(defun replique2/guess-port-nb ()
  (let ((port-file (locate-dominating-file
                    default-directory ".replique-port")))
    (when port-file
      (with-temp-buffer
        (insert-file-contents (concat port-file ".replique-port"))
        (->> (buffer-string)
             replique-edn2/read-string
             (gethash :repl))))))

(defvar replique2/edn-tag-readers
  `((error . ,'identity)
    (object . ,'identity)))

(defun replique2/dispatch-eval-msg* (in-chan out-chan)
  (replique-async2/<!
   in-chan
   (lambda (msg)
     (cond ((null msg)
            (replique-async2/close! out-chan))
           ((equal :eval (gethash :type msg))
            (let ((repl (replique2/get-repl-by-session
                         (gethash :repl-type msg)
                         (gethash :client (gethash :session msg)))))
              (when repl
                (replique-async2/put! (cdr (assoc :eval-chan repl)) msg)))
            (replique2/dispatch-eval-msg* in-chan out-chan))
           (t (replique-async2/put! out-chan msg)
              (replique2/dispatch-eval-msg* in-chan out-chan))))))

(defun replique2/dispatch-eval-msg (in-chan)
  (let ((out-chan (replique-async2/chan)))
    (replique2/dispatch-eval-msg* in-chan out-chan)
    out-chan))

(defun replique2/edn-read-stream* (chan-in chan-out edn-state)
  (replique-async2/<!
   chan-in
   (lambda (s)
     (if (not s)
         (replique-async2/close! chan-out)
       (let ((continue t))
         (while continue
           (let* ((reader (replique-edn2/reader nil :str s))
                  (result-state (assoc :result-state
                                       (symbol-value edn-state)))
                  (result-state (if result-state (cdr result-state) nil)))
             (if (equal :waiting (symbol-value result-state))
                 (replique-edn2/set-reader edn-state reader)
               (-> (replique-edn2/init-state reader edn-state)
                   (replique-edn2/set-tagged-readers
                    replique2/edn-tag-readers)))
             (replique-edn2/read edn-state)
             (-let (((&alist :result result
                             :result-state result-state)
                     (symbol-value edn-state))
                    (rest-string
                     (replique-edn2/reader-rest-string reader)))
               (when (car (symbol-value result))
                 (replique-async2/put!
                  chan-out (car (symbol-value result))))
               (if (not (string= "" rest-string))
                   (setq s rest-string)
                 (setq continue nil)))))
         (replique2/edn-read-stream* chan-in chan-out edn-state))))))

(defun replique2/edn-read-stream (chan-in)
  (let ((edn-state (-> (replique-edn2/reader nil :str "")
                       replique-edn2/init-state
                       (replique-edn2/set-tagged-readers
                        replique2/edn-tag-readers)))
        (chan-out (replique-async2/chan)))
    (replique2/edn-read-stream* chan-in chan-out edn-state)
    chan-out))

(comment
 (defvar in-chan (replique-async2/chan))
 (defvar out-chan (replique2/edn-read-stream in-chan))

 (replique-async2/<!
  out-chan
  (lambda (x)
    (print x)))

 (oref in-chan closed)
 (oref out-chan closed)
 (replique-async2/close! in-chan)
 (replique-async2/put! in-chan "3 ")
 )

(defun replique2/send-tooling-msg (props msg)
  (-let (((&alist :tooling-proc proc) props)
         (msg (replique2/alist-to-map msg)))
    (process-send-string
     proc (format "(ewen.replique.server/tooling-msg-handle %s)\n"
                  (replique-edn/pr-str msg)))))

(defun replique2/clj-buff-name (directory host port)
  (generate-new-buffer-name
   (format "%s|%s:%s|clj" (file-name-nondirectory directory) host port)))

(defun replique2/cljs-buff-name (directory host port)
  (generate-new-buffer-name
   (format "%s|%s:%s|cljs" (file-name-nondirectory directory) host port)))

(comment
 (replique2/cljs-buff-name "/home/egr" "127.0.0.1" 9000)
 )

(defun replique2/cleanup-processes ()
  (let ((all-props (-filter
                    (-lambda ((&alist :clj-repls clj-repls
                                :cljs-repls cljs-repls))
                      (and (equal '() clj-repls)
                           (equal '() cljs-repls)))
                    replique2/processes))
        (active-removed? nil))
    (mapcar (-lambda (props)
              (-let (((&alist :active active
                              :tooling-proc tooling-proc)
                      props))
                (when active (setq active-removed? t))
                (when (process-live-p tooling-proc)
                  (set-process-filter
                   tooling-proc (lambda (proc string) nil))
                  (process-send-eof tooling-proc))
                (setq replique2/processes
                      (delete props replique2/processes))))
            all-props)
    (when active-removed?
      (let ((props (car replique2/processes)))
        (when props
          (setcdr (assoc :active props) t))))))

(defun replique2/cleanup-repl (b)
  (mapcar (lambda (props)
            (-let (((&alist :clj-repls clj-repls
                            :cljs-repls cljs-repls
                            :active-clj-repl active-clj-repl
                            :active-cljs-repl active-cljs-repl)
                    props))
              (let ((repl (-first (lambda (repl)
                                    (eq b (cdr (assoc :buffer repl))))
                                  clj-repls)))
                (when repl
                  (let* ((buff (cdr (assoc :buffer repl)))
                         (proc (get-buffer-process buff))
                         (eval-chan (cdr (assoc :eval-chan repl))))
                    (setcdr (assoc :clj-repls props)
                            (delete repl clj-repls))
                    (when (and proc (process-live-p proc))
                      (process-send-eof proc))
                    (when eval-chan
                      (replique-async2/close! eval-chan))))
                (when (eq active-clj-repl repl)
                  (setcdr (assoc :active-clj-repl props)
                          (cadr (assoc :clj-repls props)))))
              (let ((repl (-first (lambda (repl)
                                    (eq b (cdr (assoc :buffer repl))))
                                  cljs-repls)))
                (when repl
                  (let* ((buff (cdr (assoc :buffer repl)))
                         (proc (get-buffer-process buff))
                         (eval-chan (cdr (assoc :eval-chan repl))))
                    (setcdr (assoc :cljs-repls props)
                            (delete repl cljs-repls))
                    (when (and proc (process-live-p proc))
                      (process-send-eof proc))
                    (when eval-chan
                      (replique-async2/close! eval-chan))))
                (when (eq active-cljs-repl repl)
                  (setcdr (assoc :active-cljs-repl props)
                          (cadr (assoc :cljs-repls props)))))))
          replique2/processes)
  (replique2/cleanup-processes))

(defun replique2/cleanup-repls (host port)
  (mapcar (lambda (props)
            (-let (((&alist :clj-repls clj-repls
                            :cljs-repls cljs-repls
                            :host h :port p)
                    props))
              (when (and (string= host h) (equal port p))
                (mapcar (lambda (repl)
                          (replique2/cleanup-repl
                           (cdr (assoc :buffer repl))))
                        clj-repls)
                (mapcar (lambda (repl)
                          (replique2/cleanup-repl
                           (cdr (assoc :buffer repl))))
                        cljs-repls))))
          replique2/processes)
  (replique2/cleanup-processes))

(defun replique2/on-repl-close (buffer process event)
  (cond ((string= "deleted\n" event)
         (replique2/cleanup-repl buffer))
        ((string= "connection broken by remote peer\n" event)
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-max))
             (insert (concat "\n" event "\n"))))
         (replique2/cleanup-repl buffer))
        (t nil)))

(defun replique2/make-repl
    (buffer-name host port repl-type &optional pop-buff)
  (-let* ((buff (make-comint (concat " " buffer-name) `(,host . ,port)))
          (proc (get-buffer-process buff))
          (chan-src (replique2/process-filter-chan proc))
          (props (replique2/process-props-by-host-port host port))
          ((&alist :clj-repls clj-repls :cljs-repls cljs-repls
                   :active-clj-repl active-clj-repl
                   :active-cljs-repl active-cljs-repl) props)
          (repl-cmd (format "(ewen.replique.server/repl %s)\n" repl-type)))
    (set-process-sentinel proc (-partial 'replique2/on-repl-close buff))
    ;; Discard the prompt
    (replique-async2/<!
     chan-src
     (lambda (x)
       (let ((chan (replique2/edn-read-stream chan-src)))
         ;; Get the session number
         (process-send-string proc "(ewen.replique.server/tooling-repl)\n")
         (process-send-string proc "clojure.core.server/*session*\n")
         (replique-async2/<!
          chan
          (lambda (resp)
            (let ((session (gethash :client resp)))
              ;; Reset process filter to the default one
              (set-process-filter proc 'comint-output-filter)
              (replique-async2/close! chan-src)
              (set-buffer buff)
              (replique2/mode)
              (process-send-string proc repl-cmd)
              (let ((repl `((:session . ,session)
                            (:buffer . ,buff)
                            (:eval-chan . ,(replique-async2/chan)))))
                (when (equal :clj repl-type)
                  (setcdr (assoc :clj-repls props)
                          (push repl clj-repls))
                  (setcdr (assoc :active-clj-repl props) repl))
                (when (equal :cljs repl-type)
                  (setcdr (assoc :cljs-repls props)
                          (push repl clj-repls))
                  (setcdr (assoc :active-cljs-repl props) repl)))
              (when pop-buff
                (pop-to-buffer buff))))))))))

(defun replique2/repl-existing (props)
  (-let (((&alist :active active
                  :host host :port port
                  :clj-repls clj-repls :cljs-repls cljs-repls
                  :active-clj-repl active-clj-repl
                  :active-cljs-repl active-cljs-repl
                  :tooling-proc tooling-proc
                  :tooling-chan tooling-chan)
          props))
    (when (not active)
      (replique2/set-active-process host port t))
    (replique2/send-tooling-msg props `((:type . :repl-infos)))
    (replique-async2/<!
     tooling-chan
     (lambda (resp)
       (let* ((directory (gethash :directory resp))
              (clj-buff-name (replique2/clj-buff-name
                              directory host port))
              (cljs-buff-name (replique2/cljs-buff-name
                               directory host port)))
         (when (equal '() clj-repls)
           (replique2/make-repl clj-buff-name host port :clj t))
         (when (equal '() cljs-repls)
           (replique2/cljs-buff-name directory host port)
           (replique2/make-repl cljs-buff-name host port :cljs)))))))

(defun replique2/on-tooling-repl-close
    (tooling-chan-src host port process event)
  (cond ((string= "deleted\n" event)
         (replique-async2/close! tooling-chan-src)
         (replique2/cleanup-repls host port))
        ((string= "connection broken by remote peer\n" event)
         (replique-async2/close! tooling-chan-src)
         (replique2/cleanup-repls host port))
        (t nil)))

(defun replique2/make-tooling-proc (host port callback)
  (let* ((proc (open-network-stream "replique" nil host port))
         (chan (replique2/process-filter-chan proc)))
    ;; Discard the prompt
    (replique-async2/<!
     chan (lambda (x)
            (set-process-filter proc nil)
            (replique-async2/close! chan)
            (process-send-string
             proc
             "(ewen.replique.server/shared-tooling-repl)\n")
            (funcall callback proc)))))

;;;###autoload
(defun replique2/repl (&optional host port)
  "Start a Clojure/Clojurescript REPL"
  (interactive
   (let ((host (read-string "Host: " "127.0.0.1"))
         (port (read-number "Port number: " (replique2/guess-port-nb))))
     (list host port)))
  (if (not (replique2/is-valid-port-nb? port))
      (message "Invalid port number: %d" port)
    (let ((existing-proc (replique2/process-props-by-host-port host port)))
      (if existing-proc
          (replique2/repl-existing existing-proc)
        (replique2/make-tooling-proc
         host port
         (lambda (tooling-proc)
           (let* ((tooling-chan-src (replique2/process-filter-chan
                                     tooling-proc))
                  (tooling-chan (-> tooling-chan-src
                                    replique2/edn-read-stream
                                    replique2/dispatch-eval-msg)))
             (set-process-sentinel
              tooling-proc
              (-partial 'replique2/on-tooling-repl-close
                        tooling-chan-src host port))
             (replique2/send-tooling-msg
              `((:tooling-proc . ,tooling-proc))
              `((:type . :repl-infos)))
             (replique-async2/<!
              tooling-chan
              (lambda (resp)
                (let* ((directory (gethash :directory resp))
                       (repl-type (gethash :repl-type resp))
                       (clj-buff-name
                        (replique2/clj-buff-name directory host port))
                       (cljs-buff-name
                        (replique2/cljs-buff-name directory host port))
                       (buff-props `((:directory . ,directory)
                                     (:active . t)
                                     (:tooling-proc . ,tooling-proc)
                                     (:host . ,host)
                                     (:port . ,port)
                                     (:tooling-chan . ,tooling-chan)
                                     (:clj-repls . ,'())
                                     (:cljs-repls . ,'())
                                     (:active-clj-repl . ,nil)
                                     (:active-cljs-repl . ,nil))))
                  (push buff-props replique2/processes)
                  (replique2/set-active-process host port nil)
                  (cond ((equal :clj repl-type)
                         (replique2/make-repl
                          clj-buff-name host port :clj t))
                        ((equal :cljs repl-type)
                         (replique2/make-repl
                          clj-buff-name host port :clj)
                         (replique2/make-repl
                          cljs-buff-name host port :cljs t))
                        (t (replique2/cleanup-repls host port)
                           (error "Error while starting the REPL. Invalid REPL type: %s" repl-type)))))))))))))


(provide 'replique2)

;;; replique2.el ends here
