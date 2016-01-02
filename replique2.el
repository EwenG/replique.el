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
       (assoc :active-clj-buff)
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

(defun replique2/eval-region (start end)
  "Eval the currently highlighted region."
  (interactive "r")
  (let ((extension (file-name-extension (buffer-name)))
        (clj-buff (replique2/get-active-clj-buffer))
        (cljs-buff (replique2/get-active-cljs-buffer))
        (input (filter-buffer-substring start end)))
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

(defun replique2/eval-last-sexp ()
  "Eval the previous sexp."
  (interactive)
  (replique2/eval-region
   (save-excursion
     (backward-sexp) (point))
   (point)))

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
    (define-key map "\C-c\C-r" 'replique2/eval-region)
    (define-key map "\C-x\C-e" 'replique2/eval-last-sexp)
    (easy-menu-define replique/minor-mode-menu map
      "Replique Minor Mode Menu"
      '("Replique"
        ["Eval region" replique2/eval-region t]
        ["Eval last sexp" replique2/eval-last-sexp t]))
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

(defun replique2/tooling-process-filter (chan proc string)
  (replique-async2/>! chan string (lambda () nil)))

(defun replique2/repl-process-filter (buff proc string)
  (when (not (-contains? (replique2/visible-buffers) buff))
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
        (replique2/message-nolog cleaned-string))))
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
                  :clj-buffs clj-buffs :cljs-buffs cljs-buffs
                  :active-clj-buff active-clj-buff
                  :active-cljs-buff active-cljs-buff
                  :tooling-proc tooling-proc :chan chan)
          props))
    (when (not active)
      (replique2/set-active-process host port t))
    (replique2/send-tooling-msg
     `((:tooling-proc . ,tooling-proc))
     `((:type . :repl-infos)))
    (let* ((resp (replique-async2/<!! chan))
           (resp (replique-edn2/read-string resp))
           (directory (gethash :directory resp))
           (host-clj (->> (gethash :replique-clj-repl resp)
                          (gethash :host)))
           (port-clj (->> (gethash :replique-clj-repl resp)
                          (gethash :port)))
           (host-cljs (->> (gethash :replique-cljs-repl resp)
                           (gethash :host)))
           (port-cljs (->> (gethash :replique-cljs-repl resp)
                           (gethash :port))))
      (when (equal '() clj-buffs)
        (let* ((clj-buff-name
                (replique2/clj-buff-name directory host-clj port-clj))
               (clj-buff
                (replique2/make-repl-buffer
                 clj-buff-name host-clj port-clj)))
          (set-buffer clj-buff)
          (replique2/mode)
          (setcdr (assoc :clj-buffs props) (list clj-buff))
          (when (not active-clj-buff)
            (setcdr (assoc :active-clj-buff props) clj-buff))))
      (when (equal '() cljs-buffs)
        (let* ((cljs-buff-name
                (replique2/cljs-buff-name directory host-cljs port-cljs))
               (cljs-buff
                (replique2/make-repl-buffer
                 cljs-buff-name host-cljs port-cljs)))
          (set-buffer cljs-buff)
          (replique2/mode)
          (setcdr (assoc :cljs-buffs props) (list cljs-buff))
          (when (not active-cljs-buff)
            (setcdr (assoc :active-cljs-buff props) cljs-buff))))
      (pop-to-buffer (car clj-buffs)))))

(defun replique2/cleanup-repl (b)
  (mapcar (lambda (props)
            (-let (((&alist :clj-buffs clj-buffs
                            :cljs-buffs cljs-buffs
                            :active-clj-buff active-clj-buff
                            :active-cljs-buff active-cljs-buff)
                    props))
              (when (-contains? clj-buffs b)
                (setcdr (assoc :clj-buffs props)
                        (delete b clj-buffs)))
              (when (equal active-clj-buff b)
                (setcdr (assoc :active-clj-buff props)
                        (cadr (assoc :clj-buffs props))))
              (when (-contains? cljs-buffs b)
                (setcdr (assoc :cljs-buffs props)
                        (delete b cljs-buffs)))
              (when (equal active-cljs-buff b)
                (setcdr (assoc :active-cljs-buff props)
                        (cadr (assoc :cljs-buffs props))))))
          replique2/processes)
  (let ((all-props (-filter
                    (-lambda ((&alist :clj-buffs clj-buffs
                                :cljs-buffs cljs-buffs))
                      (and (equal '() clj-buffs)
                           (equal '() cljs-buffs)))
                    replique2/processes))
        (active-removed? nil))
    (mapcar (-lambda (props)
              (-let (((&alist :active active
                              :tooling-proc tooling-proc
                              :chan chan)
                      props))
                (when active (setq active-removed? t))
                (delete-process tooling-proc)
                (replique-async2/close! chan)
                (setq replique2/processes
                      (delete props replique2/processes))))
            all-props)
    (when active-removed?
      (let ((props (car replique2/processes)))
        (when props
          (setcdr (assoc :active props) t))))))

(defun replique2/cleanup-process (host port)
  (mapcar (lambda (props)
            (-let (((&alist :clj-buffs clj-buffs
                            :cljs-buffs cljs-buffs
                            :host h :port p)
                    props))
              (when (and (string= host h) (equal port p))
                (mapcar (lambda (buff)
                          (when (get-buffer-process buff)
                            (delete-process (get-buffer-process buff))))
                        clj-buffs)
                (mapcar (lambda (buff)
                          (when (get-buffer-process buff)
                            (delete-process (get-buffer-process buff))))
                        cljs-buffs))))
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

(defun replique2/make-repl-buffer (buffer-name host port)
  (let ((buff (make-comint buffer-name `(,host . ,port))))
    (set-process-sentinel
     (get-buffer-process buff)
     (-partial 'replique2/on-repl-close buff))
    (set-process-filter
     (get-buffer-process buff)
     (-partial 'replique2/repl-process-filter buff))
    buff))

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
    (let ((existing-buffer (replique2/process-props-by
                            (-lambda ((&alist :port port
                                        :host buff-host))
                              (and (equal port-nb port)
                                   (equal host buff-host))))))
      (if existing-buffer
          (replique2/repl-existing existing-buffer)
        (let* ((tooling-proc (replique2/make-tooling-proc
                              host port-nb))
               (chan (replique-async2/chan)))
          (set-process-filter
           tooling-proc (-partial 'replique2/tooling-process-filter chan))
          (replique2/send-tooling-msg
           `((:tooling-proc . ,tooling-proc))
           `((:type . :repl-infos)))
          (let* ((resp (replique-async2/<!! chan))
                 (resp (replique-edn2/read-string resp))
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
                 (clj-buff
                  (replique2/make-repl-buffer
                   clj-buff-name host-clj port-clj))
                 (cljs-buff
                  (replique2/make-repl-buffer
                   cljs-buff-name host-cljs port-cljs))
                 (buff-props
                  `((:directory . ,directory)
                    (:active . t)
                    (:tooling-proc . ,tooling-proc)
                    (:host . ,host)
                    (:port . ,port-nb)
                    (:chan . ,chan)
                    (:clj-buffs . (,clj-buff))
                    (:cljs-buffs . (,cljs-buff))
                    (:active-clj-buff . ,clj-buff)
                    (:active-cljs-buff . ,cljs-buff))))
            (push buff-props replique2/processes)
            (set-buffer cljs-buff)
            (replique2/mode)
            (set-buffer clj-buff)
            (replique2/mode)
            (pop-to-buffer clj-buff)))))))


(provide 'replique2)

;;; replique2.el ends here
