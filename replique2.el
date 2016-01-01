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
  (setq-default message-log-max nil)
  (message format-string args)
  (setq-default message-log-max 1000))

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

(defun replique2/process-props-by (pred &optional error-on-nil)
  (let ((props (-first pred replique2/processes)))
    (if (and (null props) error-on-nil)
        (error "No started REPL")
      props)))

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

(defvar replique2/mode-hook '()
  "Hook for customizing replique2 mode.")

(defvar replique2/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-x\C-e" #'replique2/eval-last-sexp)
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
  (make-local-variable 'company-backends))

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

(defun replique2/process-filter (chan proc string)
  (replique-async2/>! chan string (lambda ())))

(defun replique2/send-tooling-msg (props msg)
  (-let (((&alist :tooling-proc proc) props)
         (msg (replique2/alist-to-map msg)))
    (process-send-string
     proc (format "(ewen.replique.server/tooling-msg-handle %s)\n"
                  (replique-edn/pr-str msg)))))

(defun replique2/clj-buff-name (directory host port)
  (generate-new-buffer-name (format "%s|%s:%s|clj" directory host port)))

(defun replique2/cljs-buff-name (directory host port)
  (generate-new-buffer-name (format "%s|%s:%s|cljs" directory host port)))

(comment
 (replique2/cljs-buff-name "/home/egr" "127.0.0.1" 9000)
 )

(defun replique2/repl-existing (props)
  )

(defun replique2/clean-processes ()
  (let ((b (current-buffer)))
    (mapcar (lambda (props)
              (-let (((&alist :clj-buffs clj-buffs
                              :cljs-buffs cljs-buffs) props))
                (when (-contains? clj-buffs b)
                  (setcdr (assoc :clj-buffs props)
                          (delete b clj-buffs)))
                ;;TODO Send cljs/quit to the comint buffer in order to
                ;; cleanup the cljs socket repl, because it is not cleaned
                ;; up on stream end
                (when (-contains? cljs-buffs b)
                  (setcdr (assoc :cljs-buffs props)
                          (delete b cljs-buffs)))))
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
            (setcdr (assoc :active props) t)))))))

;;;###autoload
(defun replique2/repl (&optional host port-nb)
  "Run a Clojure REPL, input and output via buffer '*replique2*'"
  (interactive
   (let ((host (read-string "Host: " "127.0.0.1"))
         (port (read-number "Port number: " (replique2/guess-port-nb))))
     (list host port)))
  (add-hook 'kill-buffer-hook 'replique2/clean-processes)
  (if (not (replique2/is-valid-port-nb? port-nb))
      (message "Invalid port number: %d" port-nb)
    (let ((existing-buffer (replique2/process-props-by
                            (-lambda ((&alist :port port
                                        :host buff-host))
                              (and (equal port-nb port)
                                   (equal host buff-host))))))
      (if nil ;existing-buffer
          (replique2/repl-existing existing-buffer)
        (let* ((tooling-proc (open-network-stream
                      "replique" nil host port-nb))
               (chan (replique-async2/chan)))
          (set-process-filter
           tooling-proc (-partial 'replique2/process-filter chan))
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
                  (make-comint clj-buff-name `(,host-clj . ,port-clj)))
                 (cljs-buff
                  (make-comint cljs-buff-name `(,host-cljs . ,port-cljs)))
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
