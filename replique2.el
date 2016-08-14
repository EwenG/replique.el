;;; replique2.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24") (clojure-mode "4.0.1") (dash "2.12.0") (company "0.9.0") (dash-functional "1.2.0") (s "1.9.0"))
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'comint)
(require 'clojure-mode)
(require 'replique-edn)
(require 'replique-async)
(require 'company)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

;; New project REPL / new REPL buffer

;; Active tooling buffer
;; Active buffers for each tooling buffer
;; A list of all buffers

(defvar replique/repls nil)

(defun replique/plist->alist (plist)
  (let ((alist '()))
    (while plist
      (setq alist (push `(,(car plist) . ,(cadr plist)) alist))
      (setq plist (cddr plist)))
    alist))

(defun replique/repls-or-repl-by (filtering-fn source &rest args)
  (-let* ((pred (lambda (repl-props)
                  (and
                   (-all? (-lambda ((k . v))
                            (or (not (plist-member args k))
                                (equal (plist-get args k) v)))
                          repl-props)
                   (-all? (-lambda ((k . v))
                            (-any? (-lambda ((repl-k . repl-v))
                                     (equal k repl-k))
                                   repl-props))
                          (replique/plist->alist args)))))
          (found (funcall filtering-fn pred source)))
    (if (and (null found) (plist-get args :error-on-nil))
        (user-error "No started REPL")
      found)))

(defun replique/repl-by (&rest args)
  (apply 'replique/repls-or-repl-by '-first replique/repls args))

(defun replique/repls-by (&rest args)
  (apply 'replique/repls-or-repl-by '-filter replique/repls args))

(defun replique/guess-project-root-dir ()
  (or (locate-dominating-file default-directory "project.clj")
      default-directory))

(defun replique/replique-root-dir ()
  (-> (locate-library "replique")
      file-name-directory))




;; Auto completion

(defun replique/auto-complete-session (prefix company-callback props repl)
  (let* ((tooling-chan (cdr (assoc :tooling-chan props)))
         (repl-type (cdr (assoc :repl-type repl)))
         (msg-type (cond ((equal :clj repl-type)
                          :clj-completion)
                         ((equal :cljs repl-type)
                          :cljs-completion)
                         (t (error "Invalid REPL type: %s" repl-type)))))
    (replique/send-tooling-msg
     props
     `((:repl-type . ,msg-type)
       (:context . ,(replique/form-with-prefix))
       (:ns . (quote ,(cdr (assoc :ns repl))))
       (:prefix . ,prefix)))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let ((err (gethash :error resp)))
         (if err
             (progn
               (message (replique-edn/pr-str err))
               (message "completion failed with prefix %s" prefix))
           (funcall company-callback (gethash :candidates resp)))))
     t)))

(defun replique/auto-complete* (prefix company-callback props msg-type)
  (let ((tooling-chan (cdr (assoc :tooling-chan props))))
    (replique/send-tooling-msg
     props
     `((:repl-type . ,msg-type)
       (:context . ,(replique/form-with-prefix))
       (:ns . (quote ,(make-symbol (clojure-find-ns))))
       (:prefix . ,prefix)))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let ((err (gethash :error resp)))
         (if err
             (progn
               (message (replique-edn/pr-str err))
               (message "completion failed with prefix %s" prefix))
           (funcall company-callback (gethash :candidates resp)))))
     t)))

(defun replique/auto-complete-clj (prefix company-callback props clj-repl)
  (replique/auto-complete* prefix company-callback props :clj-completion))

(defun replique/auto-complete-cljs (prefix company-callback props cljs-repl)
  (replique/auto-complete* prefix company-callback props :cljs-completion))

(defun replique/auto-complete (prefix company-callback)
  (replique/with-modes-dispatch
   (replique/mode . (-partial 'replique/auto-complete-session
                              prefix company-callback))
   (clojure-mode . (-partial 'replique/auto-complete-clj
                             prefix company-callback))
   (clojurescript-mode . (-partial 'replique/auto-complete-cljs
                                   prefix company-callback))))

(defun replique/company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cond
   ((equal command 'interactive)
    (company-begin-backend 'replique/company-backend))
   ((equal command 'prefix) (when (or (derived-mode-p 'clojure-mode)
                                      (derived-mode-p 'replique/mode))
                              (replique/symbol-backward)))
   ((equal command 'candidates)
    `(:async . ,(-partial 'replique/auto-complete arg)))))

(defun replique/display-eval-result (msg buff)
  (when (not (-contains? (replique/visible-buffers) buff))
    (replique/message-nolog
     (replique/format-eval-message msg))))

(defun replique/comint-is-closed-sexpr (start limit)
  (let ((depth (car (parse-partial-sexp start limit))))
    (if (<= depth 0) t nil)))

(defun replique/comint-send-input (&optional no-read-eval-chan)
  (interactive)
  (let* ((buff (current-buffer))
         (repl (replique/repl-by :buffer buff))
         (eval-chan (cdr (assoc :eval-chan repl)))
         (proc (get-buffer-process buff)))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc)))
        (cond (;; Point is at the end of the line and the sexpr is
               ;; terminated
               (and (equal (point) (point-max))
                    (replique/comint-is-closed-sexpr pmark (point)))
               (comint-send-input)
               (when (null no-read-eval-chan)
                 (replique-async/<!
                  eval-chan
                  (lambda (msg)
                    (when msg
                      (replique/display-eval-result msg buff)))
                  t)))
              ;; Point is after the prompt but (before the end of line or
              ;; the sexpr is not terminated)
              ((comint-after-pmark-p) (comint-accumulate))
              ;; Point is before the prompt. Do nothing.
              (t nil))))))

(defcustom replique/prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)

(defvar replique/mode-hook '()
  "Hook for customizing replique mode.")

(defvar replique/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-m" 'replique/comint-send-input)
    map))

(define-derived-mode replique/mode comint-mode "Replique"
  "Commands:\\<replique/mode-map>"
  (setq comint-prompt-regexp replique/prompt)
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (add-to-list 'company-backends 'replique/company-backend)
  )

(defun replique/lein-command (port)
  `("/Users/egr/bin/lein" "update-in" ":source-paths" "conj"
    ,(format "\"%ssrc\"" (replique/replique-root-dir))
    "--" "run" "-m" "ewen.replique.main/-main"
    ,(format "{:type :clj :port %s}" (number-to-string port))))

(defun replique/is-lein-project (directory)
  (file-exists-p (expand-file-name "project.clj" directory)))

(defun replique/process-filter-chan (proc)
  (let ((chan (replique-async/chan)))
    (set-process-filter
     proc (lambda (proc string)
            (replique-async/put! chan string)))
    chan))

(defun replique/is-valid-port-nb? (port-nb)
  (< -1 port-nb 65535))

(defun replique/close-tooling-repl (repl)
  (-let (((&alist :network-proc tooling-network-proc :proc tooling-proc) repl))
    (when (process-live-p tooling-network-proc)
      (set-process-filter tooling-network-proc (lambda (proc string) nil))
      (process-send-eof tooling-network-proc))
    (when (process-live-p tooling-proc)
      (delete-process tooling-proc))
    (setq replique/repls (delete repl replique/repls))))

(defun replique/close-repl (repl-props)
  (-let* (((&alist :buffer buffer :host host :port port) repl-props)
          (proc (get-buffer-process buffer)))
    (setq replique/repls (delete repl-props replique/repls))
    (when (and proc (process-live-p proc))
      (process-send-eof proc))
    ;; If the only repl left is a tooling repl, then close it
    (let ((other-repls (replique/repls-by :host host :port port)))
      (when (-all? (-lambda ((&alist :repl-type repl-type))
                     (equal :tooling repl-type))
                   other-repls)
        (mapcar 'replique/close-tooling-repl other-repls)))))

(defun replique/close-repls (host port)
  (let* ((repls (replique/repls-by :host host :port port))
         (not-tooling-repls (-filter (-lambda ((&alist :repl-type repl-type))
                                       (not (equal repl-type :tooling)))
                                     repls))
         (tooling-repls (-filter (-lambda ((&alist :repl-type repl-type))
                                   (equal repl-type :tooling))
                                 repls)))
    (mapcar (lambda (repl)
              (replique/close-repl))
            not-tooling-repls)
    (mapcar (lambda (repl)
              (replique/remove-from-active-repls host port)
              (replique/close-tooling-repl repl))
            tooling-repls)))

(defun replique/on-tooling-repl-close
    (tooling-chan-src host port process event)
  (cond ((string= "deleted\n" event)
         (replique/close-repls host port))
        ((string= "connection broken by remote peer\n" event)
         (replique/close-repls host port))
        (t nil)))

(defun replique/make-tooling-repl (host port directory out-chan)
  (let* ((default-directory directory)
         (repl-cmd (replique/lein-command port))
         (proc (apply 'start-file-process directory nil (car repl-cmd) (cdr repl-cmd)))
         (chan (replique/process-filter-chan proc)))
    (replique-async/<!
     chan (lambda (x)
            (set-process-filter proc (lambda (proc string) nil))
            ;;TODO handle errors while reading (if proc throws an exception)
            (let* ((repl-infos (replique-edn/read-string x))
                   (host (gethash :host repl-infos))
                   (port (gethash :port repl-infos))
                   (directory (replique/normalize-directory-name
                               (gethash :directory repl-infos)))
                   (network-proc (open-network-stream directory nil host port))
                   (tooling-chan (replique/process-filter-chan network-proc))
                   (repl-props `((:directory . ,directory)
                                 (:repl-type . :tooling)
                                 (:proc . ,proc)
                                 (:network-proc . ,network-proc)
                                 (:host . ,host)
                                 (:port . ,port)
                                 (:chan . ,tooling-chan))))
              (set-process-sentinel
               network-proc (-partial 'replique/on-tooling-repl-close chan host port))
              ;; Discard the prompt
              (replique-async/<!
               tooling-chan (lambda (x)
                              (process-send-string
                               network-proc
                               "(ewen.replique.server/shared-tooling-repl)\n")
                              (push repl-props replique/repls)
                              (replique-async/put! out-chan repl-props))))))))

(defun replique/on-repl-close (host port buffer process event)
  (let ((closing-repl (replique/repl-by :host host :port port :buffer buffer)))
    (when closing-repl
      (cond ((string= "deleted\n" event)
             (replique/close-repl closing-repl))
            ((string= "connection broken by remote peer\n" event)
             (with-current-buffer buffer
               (save-excursion
                 (goto-char (point-max))
                 (insert (concat "\n" event "\n"))))
             (replique/close-repl closing-repl))
            (t nil)))))

(defun replique/make-repl (buffer-name host port make-active)
  (-let* ((buff (get-buffer-create buffer-name))
          (buff (make-comint-in-buffer buffer-name buff `(,host . ,port)))
          (proc (get-buffer-process buff))
          (chan-src (replique/process-filter-chan proc))
          (repl-cmd (format "(ewen.replique.server/repl :clj)\n")))
    (set-process-sentinel
     proc (-partial 'replique/on-repl-close host port buff))
    ;; Discard the prompt
    (replique-async/<!
     chan-src
     (lambda (x)
       (let ((chan (replique/edn-read-stream chan-src)))
         (process-send-string proc "(ewen.replique.server/tooling-repl)\n")
         ;; Get the session number
         (process-send-string proc "clojure.core.server/*session*\n")
         (replique-async/<!
          chan
          (lambda (resp)
            (let ((session (gethash :client resp)))
              ;; Reset process filter to the default one
              (set-process-filter proc 'comint-output-filter)
              (set-buffer buff)
              (replique/mode)
              (process-send-string proc repl-cmd)
              (let ((repl `((:host . ,host)
                            (:port . ,port)
                            (:repl-type . :clj)
                            (:session . ,session)
                            (:ns . 'user)
                            (:buffer . ,buff)
                            (:eval-chan . ,(replique-async/chan)))))
                (push repl replique/repls))
              (when t
                (pop-to-buffer buff))))))))))

(defun replique/clj-buff-name (directory repl-type)
  (let ((repl-type-string (substring (symbol-name repl-type) 1)))
    (generate-new-buffer-name
     (format "*replique*%s*%s*"
             (file-name-nondirectory (directory-file-name directory))
             repl-type-string))))

(defun replique/normalize-directory-name (directory)
  (file-name-as-directory (file-truename directory)))

;;;###autoload
(defun replique/repl (&optional directory host port)
  (interactive            
   (let ((directory (read-directory-name
                     "Project directory: " (replique/guess-project-root-dir) nil t))
         (host "127.0.0.1")
         (port (read-number "Port number: " 0)))
     ;; Normalizing the directory name is necessary in order to be able to search repls
     ;; by directory name
     (list (replique/normalize-directory-name directory) host port)))
  (if (not (replique/is-valid-port-nb? port))
      (message "Invalid port number: %d" port)
    (progn
      (when (not (replique/is-lein-project directory))
        (error "Not a lein project"))
      (let* ((existing-repl (replique/repl-by
                             :directory directory
                             :repl-type :tooling))
             (tooling-repl-chan (replique-async/chan)))
        (if existing-repl
            (replique-async/put! tooling-repl-chan existing-repl)
          (replique/make-tooling-repl host port directory tooling-repl-chan))
        (replique-async/<!
         tooling-repl-chan
         (-lambda ((&alist :directory directory
                           :host host :port port
                           :chan tooling-chan))
           (let* ((buff-name (replique/clj-buff-name directory :clj)))
             (replique/make-repl buff-name host port t))
           (comment (replique/close-repls host (cdr (assoc :port tooling-repl))))))))))

(defvar replique/edn-tag-readers
  `((error . identity)
    (object . identity)))

(defun replique/edn-read-stream* (chan-in chan-out edn-state)
  (replique-async/<!
   chan-in
   (lambda (s)
     (let ((continue t))
       (while continue
         (let* ((reader (replique-edn/reader nil :str s))
                (result-state (assoc :result-state
                                     (symbol-value edn-state)))
                (result-state (if result-state (cdr result-state) nil)))
           (if (equal :waiting (symbol-value result-state))
               (replique-edn/set-reader edn-state reader)
             (-> (replique-edn/init-state reader edn-state)
                 (replique-edn/set-tagged-readers
                  replique/edn-tag-readers)))
           (replique-edn/read edn-state)
           (-let (((&alist :result result
                           :result-state result-state)
                   (symbol-value edn-state))
                  (rest-string
                   (replique-edn/reader-rest-string reader)))
             (when (and
                    (not (equal :waiting (symbol-value result-state)))
                    (car (symbol-value result)))
               (replique-async/put!
                chan-out (car (symbol-value result))))
             (if (not (string= "" rest-string))
                 (setq s rest-string)
               (setq continue nil)))))
       (replique/edn-read-stream* chan-in chan-out edn-state)))))

(defun replique/edn-read-stream (chan-in)
  (let ((edn-state (-> (replique-edn/reader nil :str "")
                       replique-edn/init-state
                       (replique-edn/set-tagged-readers
                        replique/edn-tag-readers)))
        (chan-out (replique-async/chan)))
    (replique/edn-read-stream* chan-in chan-out edn-state)
    chan-out))

(comment
 (defvar in-chan (replique-async/chan))
 (defvar out-chan (replique/edn-read-stream in-chan))

 (replique-async/<!
  out-chan
  (lambda (x)
    (print x)))

 (replique-async/put! in-chan "3 ")
 )

(provide 'replique2)

;;; replique.el ends here
