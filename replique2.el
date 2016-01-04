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
    (message format-string args)))

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

(defcustom replique2/prompt-filter "^[^=> \n]+=> "
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

(defun replique2/process-filter-chan (chan proc string)
  (replique-async2/put! chan string))

(defun replique2/process-filter-chan2 (chan proc string)
  (print "filter2 ")
  (print string)
  (replique-async2/put! chan string))

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

(defun replique2/get-active-clj-buffer (&optional error-on-nil)
  (->> (replique2/active-process-props error-on-nil)
       (assoc :active-clj-repl)
       cdr
       (assoc :buffer)
       cdr))

(defun replique2/get-active-cljs-buffer (&optional error-on-nil)
  (->> (replique2/active-process-props error-on-nil)
       (assoc :active-cljs-buff)
       cdr))

(defun replique2/set-active-process (host port &optional display-msg)
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
     (list (cdr (-first
                 (lambda (x) (string= (car x) choosen))
                 buffer-descs))
           t)))
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
    (message "Active process switched to: %s" buffer-name)))

(defun replique2/comint-is-closed-sexpr (start limit)
  (let ((depth (car (parse-partial-sexp start limit))))
    (if (<= depth 0) t nil)))

(defun replique2/comint-send-input (&optional no-newline artificial)
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc)))
        (cond (;; Point is at the end of the line and the sexpr is
               ;; terminated
               (and (equal (point) (point-max))
                    (replique2/comint-is-closed-sexpr pmark (point)))
               (comint-send-input no-newline artificial))
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

(defun replique2/send-input-from-source-dispatch
    (input &optional no-newline artificial)
  (let ((extension (file-name-extension (buffer-name)))
        (clj-buff (replique2/get-active-clj-buffer))
        (cljs-buff (replique2/get-active-cljs-buffer)))
    (cond ((string= "clj" extension)
           (if clj-buff
               (with-current-buffer
                   clj-buff
                 (replique2/comint-send-input-from-source input))
             (user-error "No active Clojure buffer")))
          ((string= "cljs" extension)
           (if cljs-buff
               (with-current-buffer
                   cljs-buff
                 (replique2/comint-send-input-from-source input))
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
                     (replique2/comint-send-input-from-source input))))
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
     (backward-sexp) (point))
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
             (gethash :tooling-repl))))))

(defvar replique2/tooling-reader-state nil)

(defvar replique2/edn-tag-readers
  `((error . identity)
    (object . identity)
    ;;Reader conditionals
    (\? 'identity)))

(defun replique2/dispatch-eval-msg (tooling-chan-src)
  (let ((tooling-chan (replique-async2/chan)))
    (replique-async2/<!
     tooling-chan-src
     (lambda (msg)
       (cond ((null msg)
              (replique-async2/close! tooling-chan))
             ((equal :eval (gethash :type msg))
              nil)
             (t (replique-async2/put! tooling-chan msg)
                (replique2/handle-tooling-msg tooling-chan-src)))))
    tooling-chan))

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
               (replique-edn2/init-state reader edn-state))
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

(defun replique2/init-repl-process-filter (chan proc string)
  (replique-async2/put! chan string))

(defun replique2/repl-process-filter (buff proc string)
  (comment (when (not (-contains? (replique2/visible-buffers) buff))
             (let* (;; Remove prompt
                    (cleaned-string (replace-regexp-in-string
                                     replique2/prompt-filter "" string))
                    ;; Remove the last end of line
                    (cleaned-string (replace-regexp-in-string
                                     "\n\\'" "" cleaned-string))
                    (cleaned-string (replace-regexp-in-string
                                     "\`\n" "" cleaned-string)))
               ;; Don't display if there is nothing to be displayed
               (when (not (string= cleaned-string ""))
                 (replique2/message-nolog cleaned-string)))))
  (comint-output-filter proc string))

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
    (replique2/send-tooling-msg
     `((:tooling-proc . ,tooling-proc))
     `((:type . :repl-infos)))
    (let* ((resp (replique-async2/<!! tooling-chan))
           (directory (gethash :directory resp))
           (host-clj (->> (gethash :replique-clj-repl resp)
                          (gethash :host)))
           (port-clj (->> (gethash :replique-clj-repl resp)
                          (gethash :port)))
           (host-cljs (->> (gethash :replique-cljs-repl resp)
                           (gethash :host)))
           (port-cljs (->> (gethash :replique-cljs-repl resp)
                           (gethash :port))))
      (when (equal '() clj-repls)
        (let* ((clj-buff-name
                (replique2/clj-buff-name directory host-clj port-clj))
               (clj-repl
                (replique2/make-repl-clj
                 clj-buff-name host-clj port-clj)))
          (set-buffer (cdr (assoc :buffer clj-repl)))
          (replique2/mode)
          (setcdr (assoc :clj-repls props) (list clj-repl))
          (when (not active-clj-repl)
            (setcdr (assoc :active-clj-repl props) clj-repl))))
      (when (equal '() cljs-repls)
        (let* ((cljs-buff-name
                (replique2/cljs-buff-name directory host-cljs port-cljs))
               (cljs-repl
                (replique2/make-repl-cljs
                 cljs-buff-name host-cljs port-cljs)))
          (set-buffer (cdr (assoc :buffer cljs-repls)))
          (replique2/mode)
          (setcdr (assoc :cljs-repls props) (list cljs-repl))
          (when (not active-cljs-repl)
            (setcdr (assoc :active-cljs-repl props) cljs-repl))))
      (pop-to-buffer (cdr (assoc :buffer (car clj-repls)))))))

(defun replique2/cleanup-repl (b)
  (mapcar (lambda (props)
            (-let (((&alist :clj-repls clj-repls
                            :cljs-repls cljs-repls
                            :active-clj-repl active-clj-repl
                            :active-cljs-repl active-cljs-repl)
                    props))
              (let ((repl (-first (lambda (repl)
                                    (equal b (cdr (assoc :buffer repl))))
                                  clj-repls)))
                (when repl
                  (setcdr (assoc :clj-repls props)
                          (delete repl clj-repls)))
                (when (equal active-clj-repl repl)
                  (setcdr (assoc :active-clj-repl props)
                          (cadr (assoc :clj-repls props)))))
              (let ((repl (-first (lambda (repl)
                                    (equal b (cdr (assoc :buffer repl))))
                                  cljs-repls)))
                (when repl
                  (setcdr (assoc :cljs-repls props)
                          (delete repl cljs-repls)))
                (when (equal active-cljs-repl repl)
                  (setcdr (assoc :active-cljs-repl props)
                          (cadr (assoc :cljs-repls props)))))))
          replique2/processes)
  (let ((all-props (-filter
                    (-lambda ((&alist :clj-repls clj-repls
                                :cljs-repls cljs-repls))
                      (and (equal '() clj-repls)
                           (equal '() cljs-repls)))
                    replique2/processes))
        (active-removed? nil))
    (mapcar (-lambda (props)
              (-let (((&alist :active active
                              :tooling-proc tooling-proc
                              :tooling-chan-src tooling-chan-src)
                      props))
                (when active (setq active-removed? t))
                (delete-process tooling-proc)
                (replique-async2/close! tooling-chan-src)
                (setq replique2/processes
                      (delete props replique2/processes))))
            all-props)
    (when active-removed?
      (let ((props (car replique2/processes)))
        (when props
          (setcdr (assoc :active props) t))))))

(defun replique2/cleanup-process (host port)
  (mapcar (lambda (props)
            (-let (((&alist :clj-repls clj-repls
                            :cljs-repls cljs-repls
                            :host h :port p)
                    props))
              (when (and (string= host h) (equal port p))
                (mapcar (lambda (repl)
                          (let ((proc (get-buffer-process
                                       (cdr (assoc :buffer repl)))))
                            (when proc (delete-process proc))))
                        clj-repls)
                (mapcar (lambda (repl)
                          (let ((proc (get-buffer-process
                                       (cdr (assoc :buffer repl)))))
                            (when proc (delete-process proc))))
                        cljs-repls))))
          replique2/processes))

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

(defun replique2/make-repl-clj (buffer-name host port)
  (let ((buff (make-comint buffer-name `(,host . ,port)))
        (chan (replique-async2/chan)))
    (set-process-sentinel
     (get-buffer-process buff)
     (-partial 'replique2/on-repl-close buff))
    (set-process-filter
     (get-buffer-process buff)
     (-partial 'replique2/init-repl-process-filter chan))
    (replique-async2/close! chan)
    (set-process-filter
     (get-buffer-process buff)
     (-partial 'replique2/repl-process-filter buff))
    `((:session . "1") (:buffer . ,buff))))

(defun replique2/make-repl-cljs (buffer-name host port)
  (let* ((buff (make-comint buffer-name `(,host . ,port)))
         (proc (get-buffer-process buff))
         (chan-src (replique-async2/chan))
         (chan (replique2/edn-read-stream chan-src)))
    (set-process-sentinel proc (-partial 'replique2/on-repl-close buff))
    (set-process-filter
     proc
     (-partial 'replique2/process-filter-chan2 chan-src))
    (process-send-string proc "(ewen.replique.server-cljs/*session*)\n")
    (let ((session (->> (replique-async2/<!! chan)
                        (gethash :client))))
      (replique-async2/close! chan-src)
      (set-process-filter
       proc
       (-partial 'replique2/repl-process-filter buff))
      `((:session . ,session) (:buffer . ,buff)))))

(defun replique2/on-tooling-repl-close (host port process event)
  (cond ((string= "connection broken by remote peer\n" event)
         (replique2/cleanup-process host port))
        (t nil)))

(defun replique2/make-tooling-proc (host port)
  (let ((proc (open-network-stream "replique" nil host port)))
    (set-process-sentinel
     proc (-partial 'replique2/on-tooling-repl-close host port))
    proc))

;;;###autoload
(defun replique2/repl (&optional host port-nb)
  "Run a Clojure REPL, input and output via buffer '*replique2*'"
  (interactive
   (let ((host (read-string "Host: " "127.0.0.1"))
         (port (read-number "Port number: " (replique2/guess-port-nb))))
     (list host port)))
  (if (not (replique2/is-valid-port-nb? port-nb))
      (message "Invalid port number: %d" port-nb)
    (let ((existing-proc (replique2/process-props-by
                          (-lambda ((&alist :port port
                                      :host buff-host))
                            (and (equal port-nb port)
                                 (equal host buff-host))))))
      (if existing-proc
          (replique2/repl-existing existing-proc)
        (let* ((tooling-proc (replique2/make-tooling-proc
                              host port-nb))
               (tooling-chan-src (replique-async2/chan))
               (tooling-chan (-> tooling-chan-src
                                 replique2/edn-read-stream
                                 replique2/dispatch-eval-msg)))
          (set-process-filter
           tooling-proc
           (-partial 'replique2/process-filter-chan tooling-chan-src))
          (replique2/send-tooling-msg
           `((:tooling-proc . ,tooling-proc))
           `((:type . :repl-infos)))
          (let* ((resp (replique-async2/<!! tooling-chan))
                 (directory (gethash :directory resp))
                 (host-clj (->> (gethash :replique-clj-repl resp)
                                (gethash :host)))
                 (port-clj (->> (gethash :replique-clj-repl resp)
                                (gethash :port)))
                 (host-cljs (->> (gethash :replique-cljs-repl resp)
                                 (gethash :host)))
                 (port-cljs (->> (gethash :replique-cljs-repl resp)
                                 (gethash :port)))
                 (clj-buff-name
                  (replique2/clj-buff-name directory host-clj port-clj))
                 (cljs-buff-name
                  (replique2/cljs-buff-name directory host-cljs port-cljs))
                 (clj-repl
                  (replique2/make-repl-clj
                   clj-buff-name host-clj port-clj))
                 (cljs-repl
                  (replique2/make-repl-cljs
                   cljs-buff-name host-cljs port-cljs))
                 (buff-props
                  `((:directory . ,directory)
                    (:active . t)
                    (:tooling-proc . ,tooling-proc)
                    (:host . ,host)
                    (:port . ,port-nb)
                    (:tooling-chan-src . ,tooling-chan-src)
                    (:tooling-chan . ,tooling-chan)
                    (:clj-repls . (,clj-repl))
                    (:cljs-repls . (,cljs-repl))
                    (:active-clj-repl . ,clj-repl)
                    (:active-cljs-repl . ,cljs-repl))))
            (push buff-props replique2/processes)
            (set-buffer (cdr (assoc :buffer cljs-repl)))
            (replique2/mode)
            (set-buffer (cdr (assoc :buffer clj-repl)))
            (replique2/mode)
            (pop-to-buffer (cdr (assoc :buffer clj-repl)))))))))


(provide 'replique2)

;;; replique2.el ends here
