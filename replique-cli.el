;; replique-cli.el ---   -*- lexical-binding: t; -*-

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

(require 'clj-data)
(require 'clj-context)
(require 'replique-context)

;; Any number of spaces followed by a call to clojure or clj (may be with an absolute path) followed by the arguments
(defvar replique-cli/cli-call-re "^\s*\\([^\s\n\t]*[/\]?\\(?:clojure\\|clj\\)\\)\\(\s.*\\)?$")

(comment
 (progn (re-search-forward replique-cli/cli-call-re)
        (match-string-no-properties 1))
 )

(defun replique-cli/remove-escaped-newlines ()
  (while (re-search-forward "\\\\\n" nil t)
    (replace-match "")))

(defun replique-cli/parse-cli-call (cli-file)
  (with-temp-buffer
    (insert-file-contents cli-file)
    (goto-char (point-min))
    (replique-cli/remove-escaped-newlines)
    (goto-char (point-min))
    (let ((cli-args '()))
      (while (and (< (length cli-args) 2)
                  (re-search-forward replique-cli/cli-call-re nil t))
        (let* ((command (match-string-no-properties 1))
               (args (or (match-string-no-properties 2) ""))
               ;; clj is clojure + rlwrap. Use the clojure executable instead
               (command (if (string-suffix-p "clj" command)
                            (concat (substring-no-properties command nil (- (length command) 3))
                                    "clojure")
                          (when (string-suffix-p "clojure" command)
                            command))))
          (when (and command (executable-find command))
            (push (clj-data/hash-map :command command :args args) cli-args))))
      (when (> (length cli-args) 1)
        (user-error "Multiple calls to the Clojure command line interface found in file %s"
                    cli-file))
      (when (equal 0 (length cli-args))
        (user-error "No Clojure command line interface usage found in script %s" cli-file))
      (car cli-args))))

(comment
 (replique-cli/parse-cli-call "/home/ewen/clojure/replique/repl.sh")
 )

(defun replique-cli/sdeps-with-replique (replique-coords sdeps)
  (with-temp-buffer
    (insert sdeps)
    (goto-char (point-min))
    (clj-context/forward-comment)
    (let* ((sdeps-map (replique-context/extracted-value (clj-context/read-one))))
      (when (and (cl-typep sdeps-map 'clj-context/object-delimited)
                 (eq :map (oref sdeps-map :delimited)))
        (goto-char (+ 1 (oref sdeps-map :start)))
        (clj-context/forward-comment)
        (let ((deps-key (replique-context/extracted-value (clj-context/read-one))))
          (while (and
                  deps-key
                  (not (and (cl-typep deps-key 'clj-context/object-symbol)
                            (equal ":deps" (oref deps-key :symbol)))))
            (clj-context/forward-comment)
            (setq deps-key (replique-context/extracted-value (clj-context/read-one))))
          (if deps-key
              (progn
                (clj-context/forward-comment)
                (let ((deps-map (replique-context/extracted-value (clj-context/read-one))))
                  (when (and (cl-typep deps-map 'clj-context/object-delimited)
                             (eq :map (oref deps-map :delimited)))
                    (goto-char (- (oref deps-map :end) 1))
                    (insert " replique/replique ")
                    (insert replique-coords)
                    (buffer-substring-no-properties (point-min) (point-max)))))
            (insert " replique/replique ")
            (insert replique-coords)
            (buffer-substring-no-properties (point-min) (point-max))))))))

(comment
 (replique-cli/sdeps-with-replique "{:local/root \"path\"}"
                                   "{:deps {org.clojure/tools.deps.alpha
                {:git/url \"https://github.com/clojure/tools.deps.alpha.git\"
                 :sha \"d492e97259c013ba401c5238842cd3445839d020\"} :rr 44}}")

 (replique-cli/sdeps-with-replique "{:local/root \"path\"}"
                                   "{eee [eee]}")

 
 )

;; Keep only known/usefull-for-starting-a-repl arguments to be able to have some expectations about the command behavior
;; The downside is that supporting new CLI arguments requires a replique update !
;; Also, we remove init-opt because we cannot control what will be evaluated (catch exceptions ...)
(defun replique-cli/cli-args-with-replique
    (replique-coords host port args-str)
  (with-temp-buffer
    (insert args-str)
    ;; Insert a space to a void matching the end of buffer when calling looking-at-p
    (insert " ")
    (goto-char (point-min))
    (let ((continue t)
          (sdeps-found nil)
          (args '()))
      (while continue
        (skip-chars-forward "\s\t")
        (let ((char1+ (char-after))
              (char2+ (char-after (+ 1 (point)))))
          (cond ((and (equal ?- char1+) (or (equal ?J char2+)
                                            (equal ?O char2+)
                                            (equal ?R char2+)
                                            (equal ?C char2+)
                                            (equal ?M char2+)
                                            (equal ?A char2+)))
                 (let ((p (point)))
                   (skip-chars-forward "^\s\t")
                   (push (buffer-substring-no-properties p (point)) args)))
                ((looking-at-p "-Sdeps[\s\t]")
                 (setq sdeps-found t)
                 (push "-Sdeps" args)
                 (forward-char 6)
                 (skip-chars-forward "\s\t")
                 (let* ((char1+ (char-after (point))))
                   (cond ((equal ?\' char1+)
                          (forward-char 1)
                          (let* ((start (point))
                                 (end (search-forward "\'" (line-end-position) t))
                                 (sdeps (when end
                                          (replique-cli/sdeps-with-replique
                                           replique-coords
                                           (buffer-substring-no-properties
                                            (+ 1 start) (- end 1))))))
                            (if (null sdeps)
                                (error "Could not parse clojure command line arguments: %s" args-str)
                              (push sdeps args))))
                         ((equal ?\" char1+)
                          (let* ((sdeps (ignore-errors (read (current-buffer))))
                                 (sdeps (when sdeps
                                          (replique-cli/sdeps-with-replique
                                           replique-coords sdeps))))
                            (if (null sdeps)
                                (error "Could not parse clojure command line arguments: %s" args-str)
                              (push sdeps args))))
                         ((or (looking-at-p "nil[\s\t]")
                              (looking-at-p "{}[\s\t]")
                              (looking-at-p "\\[\\][\s\t]"))
                          (let ((sdeps (format "{:deps {replique/replique %s}}"
                                               replique-coords)))
                            (push sdeps args)
                            (forward-char 3)))
                         (t (error "Could not parse clojure command line arguments: %s" args-str)))))
                ((looking-at-p "-Spath[\s\t]")
                 (forward-char 6))
                ((looking-at-p "-Scp[\s\t]")
                 (push "-Scp" args)
                 (forward-char 4)
                 (skip-chars-forward "\s\t")
                 (let ((p (point)))
                   (skip-chars-forward "^\s\t")
                   (push (buffer-substring-no-properties p (point)) args)))
                ((looking-at-p "-Srepro[\s\t]")
                 (push "-Srepro" args)
                 (forward-char 7))
                ((looking-at-p "-Sforce[\s\t]")
                 (push "-Sforce" args)
                 (forward-char 7))
                ((looking-at-p "-Spom[\s\t]")
                 (forward-char 5))
                ((looking-at-p "-Stree[\s\t]")
                 (forward-char 6))
                ((looking-at-p "-Sresolve-tags[\s\t]")
                 (forward-char 14))
                ((looking-at-p "-Sverbose[\s\t]")
                 (push "-Sverbose" args)
                 (forward-char 9))
                ((looking-at-p "-Sdescribe")
                 (forward-char 10))
                (t
                 (when (not sdeps-found)
                   (setq args (seq-concatenate 'list args (list (format "{:deps {replique/replique %s}}" replique-coords) "-Sdeps"))))
                 (when host (push (format "-J-Dreplique.server.host=%s" host) args))
                 (when port (push (format "-J-Dreplique.server.port=%s" port) args))
                 (setq continue nil)))))
      args)))

(comment
 (replique-cli/cli-args-with-replique
  "{:local/root \"path\"}"
  "")
 
 (replique-cli/cli-args-with-replique
  "{:local/root \"path\"}"
  "-J:opt    -O:alias1:alias2  -O -Rr -Cc -Mm -Aa -Spath -O -Srepro -Scp ffff -Sforce -Spom -Stree -Sresolve-tags -Sdescribe -Sverbose")

 (replique-cli/cli-args-with-replique
  "{:local/root \"path\"}"
  "-J:opt    -Sdeps  \"  {rr :deps {kk \\\"rr\\\" {:deps }}}   \"  -O")

 (replique-cli/cli-args-with-replique
  "{:local/root \"path\"}" "localhost" 0 "dir"
  "-J:opt    -Sdeps  '  {rr :deps {kk \"rr\" {:deps }}} '    -O ")
 )

(defun replique-cli/clojure-command
    (default-clojure-bin replique-coords host port directory cli-file)
  (if (or (null cli-file) (equal cli-file directory))
      `(,default-clojure-bin
         "-Sdeps" ,(format "{:deps {replique/replique %s}}" replique-coords)
         ,@(when host (list (format "-J-Dreplique.server.host=%s" host)))
         ,@(when port (list (format "-J-Dreplique.server.port=%s" port)))
         "-M" "-m" "replique.main" ,(format "%s" directory))
    (let* ((parsed-command (replique-cli/parse-cli-call cli-file))
           (parsed-args (replique-cli/cli-args-with-replique
                         replique-coords host port
                         (clj-data/get parsed-command :args))))
      (push "-m" parsed-args)
      (push "replique.main" parsed-args)
      (push (format "%s" directory) parsed-args)
      (cons (clj-data/get parsed-command :command) (nreverse parsed-args)))))

(defun replique-cli/spath-command
    (default-clojure-bin replique-coords cli-file)
  (if (null cli-file)
      `(,default-clojure-bin
         "-Sdeps" ,(format "{:deps {replique/replique %s}}" replique-coords)
         "-Spath")
    (let* ((parsed-command (replique-cli/parse-cli-call cli-file))
           (parsed-args (replique-cli/cli-args-with-replique
                         replique-coords nil nil
                         (clj-data/get parsed-command :args)))
           (parsed-args (nreverse parsed-args)))
      (push "-Spath" parsed-args)
      (cons (clj-data/get parsed-command :command) parsed-args))))

(provide 'replique-cli)

;; replique-cli does not support scripts that dynamically build the cli command (let's say based on parameters)
