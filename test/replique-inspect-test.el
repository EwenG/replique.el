;;; replique-inspect-test.el --- Tests for browsing a value of the process  -*- lexical-binding: t; -*-

;;; Commentary:

;; What a buffer showing a value asks the process, and what it makes of the
;; answers, without a process: the requests are kept and answered by the
;; test, in order, the way a control connection answers them.  What a real
;; process answers is in the tests at the bottom, which need one.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-inspect)

(defvar replique-inspect-test--asked nil
  "The requests not answered yet, oldest first: (MSG . CALLBACK).")

(defmacro replique-inspect-test--with (&rest body)
  "Run BODY with a process that keeps what it is asked, to be answered."
  (declare (indent 0))
  `(let ((replique-inspect-test--asked nil)
         (process (replique-process--make :id "inspect-test" :directory "/tmp/")))
     (ignore process)
     (cl-letf (((symbol-function 'replique-process-live-p) (lambda (p) (and p t)))
               ((symbol-function 'replique-process-request)
                (lambda (_process msg &optional callback)
                  (setq replique-inspect-test--asked
                        (append replique-inspect-test--asked (list (cons msg callback))))
                  nil)))
       (unwind-protect (progn ,@body)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'replique-inspect-mode)
               (let ((kill-buffer-hook nil)) (kill-buffer buffer)))))))))

(defun replique-inspect-test--asked ()
  "Return the oldest request not answered yet."
  (car (car replique-inspect-test--asked)))

(defun replique-inspect-test--answer (frame)
  "Answer the oldest request with FRAME, and return what it asked."
  (let ((cell (pop replique-inspect-test--asked)))
    (unless cell (error "Nothing was asked"))
    (when (cdr cell) (funcall (cdr cell) frame))
    (car cell)))

(defun replique-inspect-test--line (node value &rest more)
  "A line the process would send for NODE, printed VALUE, with MORE."
  (append (list :node node :value value :kind "number") more))

(defun replique-inspect-test--opened (&rest more)
  "The answer to opening a view of {:a 1 :b [...]}, with MORE - first, so
that what it says is what is read."
  (append
   more
   (list :tag "reply" :view 7
         :root (list :node 0 :value "{:a 1, :b [0 1 2 ...]}" :kind "map"
                     :count 2 :expandable t :truncated t)
         :children (list (replique-inspect-test--line 1 "1" :via "key" :key ":a")
                         (list :node 2 :value "[0 1 2 ...]" :kind "vector" :count 300
                               :expandable t :truncated t :via "key" :key ":b"))
         :total 2 :more nil)))

(defun replique-inspect-test--text ()
  "Return what the current buffer shows."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun replique-inspect-test--goto (text)
  "Put point at the start of the line showing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(defmacro replique-inspect-test--in-view (&rest body)
  "Run BODY in a buffer showing the view `replique-inspect-test--opened' answers."
  (declare (indent 0))
  `(replique-inspect-test--with
     (let ((buffer (replique-inspect-show process nil '(:var "user/state") "user/state")))
       (with-current-buffer buffer
         (replique-inspect-test--answer (replique-inspect-test--opened :history '(:count 1)))
         ,@body))))

(ert-deftest replique-inspect-test-a-view-is-opened-on-what-it-is-a-view-of ()
  (replique-inspect-test--with
    (let ((replique-inspect-history 4))
      (replique-inspect-show process '(:dialect :cljs :target :node) '(:var "app/state") "app/state")
      (let ((msg (replique-inspect-test--asked)))
        (should (eq :inspect (plist-get msg :op)))
        (should (equal '(:var "app/state") (plist-get msg :source)))
        (should (equal 4 (plist-get msg :history)))
        (should (eq :cljs (plist-get msg :dialect)))
        (should (eq :node (plist-get msg :target)))
        (should (equal replique-inspect-page-size (plist-get msg :limit)))))))

(ert-deftest replique-inspect-test-what-is-not-a-var-keeps-no-history ()
  (replique-inspect-test--with
    (replique-inspect-show process nil '(:taps t) "taps")
    (should-not (plist-member (replique-inspect-test--asked) :history))))

(ert-deftest replique-inspect-test-the-root-is-shown-open ()
  (replique-inspect-test--in-view
    (let ((text (replique-inspect-test--text)))
      (should (string-match-p "▾ {:a 1, :b \\[0 1 2 \\.\\.\\.\\]}…  map · 2" text))
      (should (string-match-p "^    :a  1$" text))
      (should (string-match-p "^  ▸ :b  \\[0 1 2 \\.\\.\\.\\]…  vector · 300$" text)))))

(ert-deftest replique-inspect-test-a-node-is-opened-a-page-at-a-time ()
  (replique-inspect-test--in-view
    (replique-inspect-test--goto ":b  ")
    (replique-inspect-toggle)
    (let ((msg (replique-inspect-test--answer
                (list :tag "reply"
                      :children (list (replique-inspect-test--line 3 "0" :via "index" :key "0")
                                      (replique-inspect-test--line 4 "1" :via "index" :key "1"))
                      :total 300 :more t))))
      (should (eq :inspect-children (plist-get msg :op)))
      (should (equal 2 (plist-get msg :node)))
      (should (equal 7 (plist-get msg :view))))
    (should (string-match-p "▾ :b" (replique-inspect-test--text)))
    (should (string-match-p "^      0  0$" (replique-inspect-test--text)))
    (should (string-match-p "… 298 more" (replique-inspect-test--text)))
    (progn ;; more
     (replique-inspect-test--goto "… 298 more")
     (replique-inspect-toggle)
     (let ((msg (replique-inspect-test--answer
                 (list :tag "reply"
                       :children (list (replique-inspect-test--line 5 "2" :via "index" :key "2"))
                       :total 300 :more t))))
       (should (equal 2 (plist-get msg :offset))))
     (should (string-match-p "^      2  2$" (replique-inspect-test--text)))
     (should (string-match-p "… 297 more" (replique-inspect-test--text))))
    (progn ;; closing
     (replique-inspect-test--goto ":b  ")
     (replique-inspect-toggle)
     (should (null replique-inspect-test--asked))
     (should-not (string-match-p "^      0  0$" (replique-inspect-test--text)))
     (should (string-match-p "▸ :b" (replique-inspect-test--text))))))

(ert-deftest replique-inspect-test-a-line-with-nothing-more-in-it-does-not-open ()
  (replique-inspect-test--in-view
    (replique-inspect-test--goto ":a  ")
    (replique-inspect-toggle)
    (should (null replique-inspect-test--asked))))

(ert-deftest replique-inspect-test-a-refresh-asks-again-for-what-is-open ()
  "And for nothing that is not, and shows what changed."
  (replique-inspect-test--in-view
    (replique-inspect-test--goto ":b  ")
    (replique-inspect-toggle)
    (replique-inspect-test--answer
     (list :tag "reply"
           :children (list (replique-inspect-test--line 3 "0" :via "index" :key "0"))
           :total 1 :more nil))
    (replique-inspect-refresh)
    (let ((msg (replique-inspect-test--answer
                (replique-inspect-test--opened
                 :children (list (replique-inspect-test--line 1 "2" :via "key" :key ":a" :changed t)
                                 (list :node 2 :value "[0 1 2 ...]" :kind "vector" :count 300
                                       :expandable t :truncated t :via "key" :key ":b"))))))
      (should (eq :inspect-refresh (plist-get msg :op))))
    (let ((msg (replique-inspect-test--answer
                (list :tag "reply"
                      :children (list (replique-inspect-test--line 3 "9" :via "index" :key "0"
                                                                   :changed t))
                      :total 1 :more nil))))
      (should (eq :inspect-children (plist-get msg :op)))
      (should (equal 2 (plist-get msg :node))))
    (should (null replique-inspect-test--asked))
    (should (string-match-p "^      0  9$" (replique-inspect-test--text)))
    (replique-inspect-test--goto ":a  ")
    (search-forward "2")
    (should (eq 'replique-inspect-changed (get-text-property (1- (point)) 'face)))))

(ert-deftest replique-inspect-test-a-view-the-process-lost-is-opened-again ()
  "And what was open in it is opened again, by the keys that led to it."
  (replique-inspect-test--in-view
    (replique-inspect-test--goto ":b  ")
    (replique-inspect-toggle)
    (replique-inspect-test--answer
     (list :tag "reply"
           :children (list (replique-inspect-test--line 3 "0" :via "index" :key "0"))
           :total 1 :more nil))
    (replique-inspect-refresh)
    (replique-inspect-test--answer (list :tag "error" :error "view-gone"
                                         :message "The page view 7 was in is gone"))
    (should (eq :inspect (plist-get (replique-inspect-test--asked) :op)))
    ;; the new view numbers its nodes as it likes
    (replique-inspect-test--answer
     (replique-inspect-test--opened
      :view 8
      :children (list (replique-inspect-test--line 11 "1" :via "key" :key ":a")
                      (list :node 12 :value "[0 1 2 ...]" :kind "vector" :count 300
                            :expandable t :truncated t :via "key" :key ":b"))))
    (let ((msg (replique-inspect-test--answer
                (list :tag "reply"
                      :children (list (replique-inspect-test--line 13 "0" :via "index" :key "0"))
                      :total 1 :more nil))))
      (should (equal 8 (plist-get msg :view)))
      (should (equal 12 (plist-get msg :node))))
    (should (equal 8 replique-inspect--view))
    (should (string-match-p "▾ :b" (replique-inspect-test--text)))
    (should (string-match-p "^      0  0$" (replique-inspect-test--text)))))

(ert-deftest replique-inspect-test-a-change-is-said-and-waits-for-g ()
  "Shown or not, nothing is asked until a refresh is."
  (replique-inspect-test--in-view
    (display-buffer (current-buffer))
    (replique-inspect--changed process (list :tag "event" :event "inspect-changed" :view 7))
    (should replique-inspect--stale)
    (should (string-match-p "changed" (replique-inspect--header)))
    (should (null replique-inspect-test--asked))
    (replique-inspect-refresh)
    (should (eq :inspect-refresh (plist-get (replique-inspect-test--asked) :op)))
    (should-not replique-inspect--stale)))

(ert-deftest replique-inspect-test-a-change-of-another-view-is-not-this-ones ()
  (replique-inspect-test--in-view
    (replique-inspect--changed process (list :tag "event" :event "inspect-changed" :view 99))
    (should-not replique-inspect--stale)))

(ert-deftest replique-inspect-test-the-history-is-gone-through-and-back-to-live ()
  (replique-inspect-test--in-view
    (setq replique-inspect--history (list :count 3))
    (should (string-match-p "live, 2 values back" (replique-inspect--header)))
    (replique-inspect-older)
    (should (equal 1 (plist-get (replique-inspect-test--answer
                                 (replique-inspect-test--opened :history '(:count 3 :at 1)))
                                :at)))
    (should (string-match-p "value 2 of 3" (replique-inspect--header)))
    (replique-inspect-older)
    (replique-inspect-test--answer (replique-inspect-test--opened :history '(:count 3 :at 0)))
    (should-error (replique-inspect-older) :type 'user-error)
    (replique-inspect-newer)
    (replique-inspect-test--answer (replique-inspect-test--opened :history '(:count 3 :at 1)))
    (replique-inspect-newer)
    (should-not (plist-member (replique-inspect-test--answer
                               (replique-inspect-test--opened :history '(:count 3)))
                              :at))
    (should-error (replique-inspect-newer) :type 'user-error)))

(ert-deftest replique-inspect-test-a-node-can-be-shown-on-its-own ()
  (replique-inspect-test--in-view
    (replique-inspect-test--goto ":b  ")
    (replique-inspect-focus)
    (replique-inspect-test--answer
     (list :tag "reply"
           :children (list (replique-inspect-test--line 3 "0" :via "index" :key "0"))
           :total 1 :more nil))
    (should (string-prefix-p "▾ :b" (replique-inspect-test--text)))
    (should-not (string-match-p ":a" (replique-inspect-test--text)))
    (should (string-match-p "in :b" (replique-inspect--header)))
    (replique-inspect-up)
    (should (string-match-p ":a" (replique-inspect-test--text)))))

(ert-deftest replique-inspect-test-a-view-that-cannot-be-opened-says-why ()
  (replique-inspect-test--with
    (with-current-buffer (replique-inspect-show process nil '(:var "user/nope") "user/nope")
      (replique-inspect-test--answer (list :tag "error" :error "unknown-var"
                                           :message "No var user/nope"))
      (should (string-match-p "No var user/nope" (replique-inspect-test--text))))))

(ert-deftest replique-inspect-test-the-path-is-copied ()
  (replique-inspect-test--in-view
    (replique-inspect-test--goto ":b  ")
    (replique-inspect-copy)
    (should (eq :inspect-path (plist-get (replique-inspect-test--asked) :op)))
    (replique-inspect-test--answer (list :tag "reply" :code "(-> (deref user/state) :b)"
                                         :value "(replique.inspect/value 7 2)"))
    (should (equal "(-> (deref user/state) :b)" (current-kill 0)))
    (replique-inspect-copy t)
    (replique-inspect-test--answer (list :tag "reply" :code "(-> (deref user/state) :b)"
                                         :value "(replique.inspect/value 7 2)"))
    (should (equal "(replique.inspect/value 7 2)" (current-kill 0)))))

(ert-deftest replique-inspect-test-a-view-is-let-go-of-with-its-buffer ()
  (replique-inspect-test--in-view
    (kill-buffer (current-buffer))
    (let ((msg (replique-inspect-test--asked)))
      (should (eq :inspect-close (plist-get msg :op)))
      (should (equal 7 (plist-get msg :view))))))

(ert-deftest replique-inspect-test-the-same-thing-is-shown-in-the-same-buffer ()
  (replique-inspect-test--in-view
    (let ((buffer (current-buffer)))
      (should (eq buffer (replique-inspect-show process nil '(:var "user/state") "user/state")))
      (should (eq :inspect-refresh (plist-get (replique-inspect-test--asked) :op))))))

;;; Against a process

(ert-deftest replique-inspect-test-an-atom-is-watched ()
  "The buffer says the atom changed, and shows what to when asked."
  (replique-test-project)
  (replique-test-with-repl repl
    (replique-test-eval repl "(def watched (atom {:n 0 :big (vec (range 500))}))")
    (let ((buffer (with-current-buffer (replique-repl--buffer repl)
                    (replique-watch "user/watched"))))
      (unwind-protect
          (with-current-buffer buffer
            (should (replique-test-wait-for
                     (lambda () (string-match-p ":n  0" (replique-inspect-test--text)))
                     10))
            (should (string-match-p "vector · 500" (replique-inspect-test--text)))
            (replique-test-eval repl "(swap! watched assoc :n 1)")
            (should (replique-test-wait-for (lambda () replique-inspect--stale) 10))
            (should (string-match-p ":n  0" (replique-inspect-test--text)))
            (replique-inspect-refresh)
            (should (replique-test-wait-for
                     (lambda () (string-match-p ":n  1" (replique-inspect-test--text)))
                     10))
            (progn ;; older-value
             (replique-test-wait-for (lambda () (not replique-inspect--busy)) 5)
             (replique-inspect-older)
             (should (replique-test-wait-for
                      (lambda () (string-match-p ":n  0" (replique-inspect-test--text)))
                      10))))
        (kill-buffer buffer)))))

(ert-deftest replique-inspect-test-what-a-repl-returned-is-shown ()
  (replique-test-project)
  (replique-test-with-repl repl
    (replique-test-eval repl ":first")
    (let ((buffer (with-current-buffer (replique-repl--buffer repl)
                    (replique-inspect-results))))
      (unwind-protect
          (with-current-buffer buffer
            (should (replique-test-wait-for
                     (lambda () (string-match-p "\\*1  :first" (replique-inspect-test--text)))
                     10))
            (replique-test-eval repl ":second")
            (should (replique-test-wait-for (lambda () replique-inspect--stale) 10))
            (replique-inspect-refresh)
            (should (replique-test-wait-for
                     (lambda () (string-match-p "\\*1  :second" (replique-inspect-test--text)))
                     10)))
        (kill-buffer buffer)))))

(provide 'replique-inspect-test)

;;; replique-inspect-test.el ends here
