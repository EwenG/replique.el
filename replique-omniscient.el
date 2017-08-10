;; replique-omniscient.el ---   -*- lexical-binding: t; -*-

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

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defvar replique/omniscient-expected-msg-id 0)
(defvar replique/omniscient-candidates nil)
(defvar replique/omniscient-indexes nil)

(defvar replique/omniscient-prev-index nil)

(defvar replique/omniscient-symbol nil)
(defvar replique/omniscient-repl nil)

(defun replique/omniscient-setup-hook ()
  (when (boundp 'smartparens-mode)
    (smartparens-mode 1)))

(defun replique/select-environment (repl sym r &optional index)
  (when (> (replique/count replique/omniscient-indexes) 0)
    (let ((selected-index (aref replique/omniscient-indexes (or index ivy--index)))
          (buff (replique/get repl :buffer)))
      (with-current-buffer buff
        (replique/comint-send-input-from-source
         (format "(replique.omniscient/repl %s %s)" sym selected-index))))))

(defun replique/expand-environment ()
  (interactive)
  (when (> (replique/count replique/omniscient-indexes) 0)
    (let ((selected-index (aref replique/omniscient-indexes ivy--index))
          (buff (replique/get replique/omniscient-repl :buffer))
          (repl-type (replique/get replique/omniscient-repl :repl-type)))
      (with-current-buffer buff
        (replique/comint-send-input-from-source
         (format (if (equal repl-type :cljs)
                     "(cljs.pprint/pprint (replique.omniscient/get-env %s %s))"
                   "(clojure.pprint/pprint (replique.omniscient/get-env %s %s))")
                 replique/omniscient-symbol selected-index))))))

(defun replique/omniscient-filter-query (session-ns symbol filter)
  (setq replique/omniscient-expected-msg-id (+ 1 replique/omniscient-expected-msg-id))
  (replique/hash-map :type (thread-last (replique/get replique/omniscient-repl :repl-type)
                             replique/keyword-to-string
                             (concat ":omniscient-filter-")
                             intern)
                     :msg-id replique/omniscient-expected-msg-id
                     :session-ns session-ns
                     :symbol symbol
                     ;; whether the cursor is over a string
                     :filter-term filter
                     :prev-index replique/omniscient-prev-index))

(defun replique/omniscient-filter (session-ns symbol tooling-repl repl)
  (let ((tooling-chan (replique/get tooling-repl :chan))
        (init-candidates nil))
    (replique/send-tooling-msg
     tooling-repl (replique/omniscient-filter-query session-ns symbol ""))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (replique/get resp :error)))
           (if err
               (progn
                 (message "%s" (replique-edn/pr-str err))
                 (message "omniscient failed with symbol: %s, ns: %s, filter term: %s"
                          symbol session-ns ""))
             (let ((locals (replique/get resp :locals))
                   (indexes (replique/get resp :indexes))
                   (index (replique/get resp :index)))
               (setq replique/omniscient-candidates locals)
               (setq replique/omniscient-indexes indexes)
               (when index (ivy-set-index index))
               (cond
                ((equal 0 (replique/count locals))
                 (message "No captured environment for %s" symbol session-ns))
                ((equal 1 (replique/count locals))
                 (replique/select-environment repl symbol (car locals) 0))
                (t (let ((minibuffer-setup-hook (cons 'replique/omniscient-setup-hook
                                                      minibuffer-setup-hook)))
                     (ivy-read
                      "Environment: "
                      (lambda (filter)
                        (when (> (replique/count replique/omniscient-indexes) 0)
                          (setq replique/omniscient-prev-index
                                (aref replique/omniscient-indexes ivy--index)))
                        (replique/send-tooling-msg
                         tooling-repl (replique/omniscient-filter-query session-ns symbol filter))
                        (replique-async/<!
                         tooling-chan
                         (lambda (resp)
                           (when resp
                             (let ((err (replique/get resp :error)))
                               (if err
                                   (progn
                                     (message "%s" (replique-edn/pr-str err))
                                     (message
                                      "omniscient failed with symbol: %s, ns: %s, filter term: %s"
                                      symbol session-ns filter)
                                     (setq replique/omniscient-candidates '())
                                     (setq replique/omniscient-indexes [])
                                     (ivy--set-candidates '())
                                     (ivy--exhibit))
                                 (let ((msg-id (replique/get resp :msg-id))
                                       (locals (replique/get resp :locals))
                                       (indexes (replique/get resp :indexes))
                                       (index (replique/get resp :index)))
                                   (when (equal msg-id replique/omniscient-expected-msg-id)
                                     (setq replique/omniscient-candidates locals)
                                     (setq replique/omniscient-indexes indexes)
                                     (ivy--set-candidates (or locals '("")))
                                     (when index (ivy-set-index index))
                                     (ivy--exhibit))))))))
                        replique/omniscient-candidates)
                      :dynamic-collection t
                      :action (apply-partially 'replique/select-environment repl symbol)
                      :keymap replique/omniscient-minibuffer-map))))))))))))

(comment
 (let ((ivy-partial-or-done (lambda (x) (print "ttt")))
       (minibuffer-setup-hook (cons 'replique/omniscient-setup-hook minibuffer-setup-hook)))
   (ivy-read "prompt " (lambda (x)
                         (run-at-time 1 nil
                                      (lambda ()
                                        (ivy--set-candidates '("ee" "ff" "ee2" "ff2" "ee3" "ff3"))
                                        (ivy--exhibit)))
                         '("fetching results..."))
             :dynamic-collection t
             :action (lambda (r) (print r))
             :keymap replique/omniscient-minibuffer-map))
 )

(defun replique/omniscient* (symbol tooling-repl repl)
  (setq replique/omniscient-candidates nil)
  (setq replique/omniscient-indexes nil)
  (setq replique/omniscient-prev-index nil)
  (setq ivy--index 0)
  (let ((session-ns (symbol-name (replique/get repl :ns))))
    (setq replique/omniscient-symbol symbol)
    (setq replique/omniscient-repl repl)
    (replique/omniscient-filter session-ns symbol tooling-repl repl)))

(defun replique/omniscient-clj (symbol tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/omniscient* symbol tooling-repl clj-repl)))

(defun replique/omniscient-cljs (symbol tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/omniscient* symbol tooling-repl cljs-repl)))

(defun replique/omniscient-cljc (symbol tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique/omniscient* symbol tooling-repl repl)))

(defun replique/omniscient-session (symbol repl)
  (replique/omniscient* symbol (replique/active-repl :tooling) repl))

(defun replique/omniscient (symbol)
  "Start a REPL in which the locals of the environment of the function/mutlimethod/protocol at point are bound. Clears all captured environments when called with a prefix argument"
  (interactive (let ((sym (symbol-at-point)))
                 (list (when sym (symbol-name sym)))))
  (if (not (featurep 'ivy))
      (user-error "replique-omniscient requires ivy-mode")
    (if (equal 4 (prefix-numeric-value current-prefix-arg))
        (replique/send-input-from-source-dispatch "(replique.omniscient-runtime/clear)")
      (when symbol
        (replique/with-modes-dispatch
         (replique/mode . (apply-partially 'replique/omniscient-session symbol))
         (clojure-mode . (apply-partially 'replique/omniscient-clj symbol))
         (clojurescript-mode . (apply-partially 'replique/omniscient-cljs symbol))
         (clojurec-mode . (apply-partially 'replique/omniscient-clj symbol))
         (t . (user-error "Unsupported major mode: %s" major-mode)))))))

(defvar replique/omniscient-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'replique/expand-environment)
    map))

(provide 'replique-omniscient)

;;; replique-omniscient.el ends here
