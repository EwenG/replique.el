;;; replique-helm ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'replique-edn)

(defun replique-helm/ns-from-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (search-forward "(ns " nil t)
    (when (not (bobp))
      (backward-char)
      (let ((ns-form (->> (thing-at-point 'defun)
                          (replique-edn/reader nil :str)
                          replique-edn/read)))
        (if (equal '^ (cadr ns-form))
            (cadddr ns-form)
          (cadr ns-form))))))

(defun replique-helm/files-to-ns (cljs-files)
  (mapcar 'replique-helm/ns-from-file cljs-files))

(defvar replique-helm/cljs-select-ns-actions
  `(("select-ns" ,(lambda (candidate)
                    (helm-marked-candidates)))))

(defun replique-helm/source-cljs-files (root-dir)
  (let ((cljs-files (->> (format "find %s -name \"*.clj[sc]\"" root-dir)
                        eshell-command-result
                        (s-split "\n")
                        (-remove-item ""))))
    `((name . "Namespaces")
      (init . ,(lambda ()
                 (helm-init-candidates-in-buffer 'global cljs-files)))
      (candidates . ,cljs-files)
      (candidate-transformer . replique-helm/files-to-ns)
      (action . replique-helm/cljs-select-ns-actions))))

(defvar replique-helm/source-nil
  `((name . "No namespace")
    (init . ,(lambda ()
               (helm-init-candidates-in-buffer 'global '(nil))))
    (candidates . (nil))))

(defun replique-helm/cljs-select-ns (root-dir)
  (helm :sources (list (replique-helm/source-cljs-files root-dir)
                       replique-helm/source-nil)
        :buffer "*Select cljs NS*"))

(provide 'replique-helm)

;;; replique-helm.el ends here
