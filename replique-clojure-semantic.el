;; replique-clojure-semantic.el ---   -*- lexical-binding: t; -*-

;; Copyright © 2026 Ewen Grosjean

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

;; Version 1.1.0-SNAPSHOT
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; `replique-clojure-semantic-mode' is the Elisp side of the PLAN's *semantic*
;; layer for Clojure -- the client of the treejure C dynamic module
;; (treejure-module.so).  It layers on top of `replique-clojure-mode', whose
;; built-in `treesit' *syntax* layer (grammar font-lock + cljfmt indentation)
;; lives in `replique-clojure-mode.el'.
;;
;; This file owns neither the parse trees nor the analysis (those live in C); it
;; only loads the module, pushes the active buffer's live text to it on edit /
;; save / buffer-switch, and renders what the module computes -- semantic-face
;; spans as an overlay *on top of* treesit's faces, and diagnostics through
;; Flymake.  `replique-clojure-mode' enables this minor mode automatically when
;; `replique-clojure-enable-semantic' is non-nil and the module is available;
;; failures degrade gracefully to the pure syntax layer.
;;
;; The module returns buffer-only faces -- `:local' / unused-greyout from the
;; scope pass and the `:special-form' / `:macro-invocation' form-head faces --
;; plus grammar-level (`ERROR'/`MISSING'/`invalid_*'), `unused-binding' and the
;; require/var Tier-1/2 diagnostics.  There is deliberately no var face: a
;; resolved var is colored by the treesit syntax layer, not the semantic
;; overlay.  The `:unresolved' face arrives with a later module slice and needs
;; no change here beyond the `category -> face' map
;; (`replique-clojure-semantic-category-faces').
;;
;; INVARIANT (byte<->position): the module always parses the whole *widened*
;; buffer (see `replique-clojure--semantic-buffer-text'), so tree byte 0 ==
;; buffer byte 1.  `position-bytes' / `byte-to-position' are absolute
;; (narrowing-independent), so the conversions below are correct under any
;; restriction.

;;; Code:

(require 'flymake)
(require 'xref)

(defgroup replique-clojure-semantic nil
  "Treejure semantic analysis layer for Clojure (the C-module client)."
  :prefix "replique-clojure-"
  :group 'languages)

(defcustom replique-clojure-semantic-module-path
  (expand-file-name "~/clojure/tree-sitter-treejure/treejure-module.so")
  "Path to the compiled treejure semantic module (.so).
Rebuild it from the tree-sitter-treejure repo (see commands.txt) after
changing the C source."
  :type 'file)

(defcustom replique-clojure-semantic-idle-delay 0.3
  "Seconds of idle time before the buffer-local semantic check after an edit."
  :type 'number)

(defcustom replique-clojure-semantic-source-dirs '("src" "test")
  "Project-relative source directories searched for cross-namespace resolution.
Used to seed a workspace's classpath so cross-file jump-to-definition can find
the project's own namespaces.  Entries that do not exist under the project root
are ignored; if none exist, the root itself is used.  This is an interim
heuristic — the JVM oracle supplies the full classpath (incl. jars) later."
  :type '(repeat string)
  :safe #'listp)

(defcustom replique-clojure-semantic-references-scope 'ask
  "Search scope for project-wide find-usages (`xref-find-references', \\[xref-find-references]).
One of:
  `ask'      prompt for the scope on each invocation (classpath / a picked
             directory / buffer-only) — the choice is transient, re-picked
             each time (the PLAN search-scope model);
  `project'  the project source dirs (`replique-clojure-semantic-source-dirs')
             with no prompt;
  `buffer'   this buffer only (no cross-file scan).
A local binding is always buffer-scoped, regardless of this setting (a local
cannot escape its file).  Cross-file results are filtered by resolved identity,
so a same-name var in another namespace never matches."
  :type '(choice (const :tag "Ask each time" ask)
                 (const :tag "Project source dirs" project)
                 (const :tag "Buffer only" buffer))
  :safe #'symbolp)

(defface replique-clojure-local-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for resolved local bindings and their usages (the `:local' category).")

(defface replique-clojure-unused-face
  '((t (:inherit shadow)))
  "Face for unused local bindings (the `:local-unused' greyout category).")

(defface replique-clojure-unresolved-face
  '((t (:inherit font-lock-warning-face)))
  "Face for symbols resolving to nothing (the `:unresolved' category).")

(defface replique-clojure-macro-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for known non-core macro invocations (the `:macro-invocation' category).
The module paints this only on the head of a *known* non-core macro — today a
user-declared def-form (see `replique-clojure-extra-def-forms'); library macros
follow once macro knowledge / `:lint-as' lands.  Special forms and core macros
read as `:special-form' (`font-lock-keyword-face') instead, so this distinct hue
means \"a macro the grammar could not have known about\" — treesit cannot tell a
macro call from a function call without resolution.")

(defcustom replique-clojure-semantic-category-faces
  '((:local            . replique-clojure-local-face)
    (:local-unused     . replique-clojure-unused-face)
    ;; There is no var face: a resolved var (same- or cross-namespace) is left to
    ;; the syntax layer.  treesit already colors a qualified symbol's namespace
    ;; (`font-lock-type-face') and the def-name forms, so the semantic overlay
    ;; paints only what treesit cannot -- locals, form heads, and (later)
    ;; `:unresolved' -- which also lets `:local' visually stand out.
    ;; Special forms AND core macros both arrive as `:special-form' (keyword);
    ;; `:macro-invocation' is reserved for KNOWN non-core macros (user
    ;; def-forms), so a project macro reads in the distinct macro hue.
    (:special-form     . font-lock-keyword-face)
    (:macro-invocation . replique-clojure-macro-face)
    (:unresolved       . replique-clojure-unresolved-face))
  "Map a treejure semantic-face category (keyword) to an Emacs face.
The keywords match `treejure-category-names'; C decides categories, Elisp owns
the visual mapping, so new faces need no module rebuild.  A category mapped to
nil (or absent) is left unpainted — treesit's own faces then show through."
  :type '(alist :key-type symbol
                :value-type (choice (const :tag "Unpainted (treesit shows)" nil)
                                    face)))

(declare-function treejure-init "treejure-module")
(declare-function treejure-check-buffer "treejure-module")
(declare-function treejure-semantic-faces "treejure-module")
(declare-function treejure-definition "treejure-module")
(declare-function treejure-references "treejure-module")
(declare-function treejure-jar-entry "treejure-module")
(declare-function treejure-close-buffer "treejure-module")
(declare-function treejure-set-def-forms "treejure-module")
(declare-function treejure-category-names "treejure-module")
(declare-function treejure-diagnostic-ids "treejure-module")

;; Defined in replique-clojure-mode.el (the syntax layer); also drives this
;; layer, where listed macros are analysed like `defn'.
(defvar replique-clojure-extra-def-forms)
(defvar replique-clojure-enable-semantic)
(declare-function replique-clojure-mode "replique-clojure-mode")
(declare-function replique-clojure-clojurescript-mode "replique-clojure-mode")
(declare-function replique-clojure-clojurec-mode "replique-clojure-mode")

(defvar replique-clojure--module-loaded nil
  "Non-nil once the treejure dynamic module has been loaded.")

(defvar replique-clojure--workspaces (make-hash-table :test 'equal)
  "Map a project root (string) to its treejure workspace handle.")

(defvar-local replique-clojure--ws nil
  "The treejure workspace handle for this buffer's project.")

(defvar-local replique-clojure--file-id nil
  "The path this buffer is analyzed under in the module (its FileNode key).")

(defvar-local replique-clojure--category-faces nil
  "Vector mapping a category integer to a face (per `treejure-category-names').")

(defvar-local replique-clojure--last-diags nil
  "Diagnostics (list of plists) returned by the most recent check.")

(defvar-local replique-clojure--flymake-report-fn nil
  "The Flymake report function registered by `replique-clojure--flymake'.")

(defvar-local replique-clojure--dirty nil
  "Non-nil when the buffer changed since the last semantic check.")

(defvar-local replique-clojure--idle-timer nil
  "Idle timer scheduled to run the buffer-local semantic check.")

(defvar-local replique-clojure--pending-cross-file nil
  "Tier requested by the pending idle check: t = full, nil = fast.
A pending full-tier request is never downgraded to fast by a later edit.")

(defun replique-clojure--ensure-module ()
  "Load the treejure dynamic module once per session.
Signals if the module file is missing."
  (unless replique-clojure--module-loaded
    (unless (file-exists-p replique-clojure-semantic-module-path)
      (error "Replique: treejure module not found at %s (build it first)"
             replique-clojure-semantic-module-path))
    (module-load replique-clojure-semantic-module-path)
    (setq replique-clojure--module-loaded t)))

(defun replique-clojure--project-root ()
  "Return the project root that owns the current buffer.
Picks the nearest ancestor with a `deps.edn'/`project.clj'/`.git', else
`default-directory'."
  (expand-file-name
   (or (locate-dominating-file default-directory "deps.edn")
       (locate-dominating-file default-directory "project.clj")
       (locate-dominating-file default-directory ".git")
       default-directory)))

(defun replique-clojure--classpath (root)
  "Return the cross-namespace search dirs for project ROOT.
The existing `replique-clojure-semantic-source-dirs' under ROOT that exist,
else ROOT itself.  This is an interim heuristic: it resolves the project's own
namespaces (enough for cross-file jump-to-definition between your files).  The
full classpath — library/external deps and jars — is supplied by the JVM oracle
in a later slice (PLAN step 6)."
  (let ((dirs (delq nil
                    (mapcar (lambda (d)
                              (let ((p (expand-file-name d root)))
                                (and (file-directory-p p) p)))
                            replique-clojure-semantic-source-dirs))))
    (or dirs (list (directory-file-name root)))))

(defun replique-clojure--get-workspace (root)
  "Return the workspace for ROOT, creating it on first use."
  (or (gethash root replique-clojure--workspaces)
      (puthash root (treejure-init root (replique-clojure--classpath root))
               replique-clojure--workspaces)))

(defun replique-clojure--build-category-faces ()
  "Compute the category-int -> face vector from `treejure-category-names'."
  (let* ((names (treejure-category-names))
         (vec (make-vector (length names) nil)))
    (dotimes (i (length names))
      (aset vec i (cdr (assq (aref names i)
                             replique-clojure-semantic-category-faces))))
    vec))

;; Raw eight-bit bytes (undecodable bytes in a partially-decoded file) are 2
;; internal bytes yet not valid Unicode, so `copy_string_contents' rejects the
;; whole string.  Substitute a 2-byte placeholder so the parse stays valid and
;; byte offsets stay aligned with Emacs' internal representation.
(defconst replique-clojure--raw-byte-regexp "[\x3FFF80-\x3FFFFF]"
  "Matches Emacs \"eight-bit\" raw bytes in a multibyte buffer.")

(defconst replique-clojure--raw-byte-placeholder "·"
  "Byte-width-preserving (2-byte) replacement for a raw eight-bit char.")

(defun replique-clojure--semantic-buffer-text ()
  "Return the whole (widened) buffer text, sanitized for the module.
This `widen' is the single point enforcing the byte<->position invariant: the
module always parses from absolute buffer position 1, so tree byte 0 == buffer
byte 1."
  (let ((text (save-restriction
                (widen)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-match-p replique-clojure--raw-byte-regexp text)
        (replace-regexp-in-string replique-clojure--raw-byte-regexp
                                  replique-clojure--raw-byte-placeholder text t t)
      text)))

(defun replique-clojure--byte->pos (byte)
  "Return the buffer position for 0-based tree BYTE offset, or nil."
  (byte-to-position (1+ byte)))

(defun replique-clojure--refresh-overlays ()
  "Repaint the semantic-face overlays over the whole (widened) buffer.
Overlay faces take display precedence over treesit's text-property faces, so
the semantic layer wins where it has a span (PLAN precedence)."
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'replique-clojure-semantic t)
    (let* ((end-byte (1- (position-bytes (point-max))))
           (faces replique-clojure--category-faces)
           (spans (treejure-semantic-faces replique-clojure--ws
                                            replique-clojure--file-id 0 end-byte))
           (n (length spans))
           (i 0))
      (while (< i n)
        (let* ((cat  (aref spans (+ i 2)))
               (face (and (< cat (length faces)) (aref faces cat)))
               (beg  (and face (replique-clojure--byte->pos (aref spans i))))
               (fin  (and face (replique-clojure--byte->pos (aref spans (1+ i))))))
          (when (and beg fin)
            (let ((ov (make-overlay beg fin nil t nil)))
              (overlay-put ov 'replique-clojure-semantic t)
              (overlay-put ov 'face face)
              (overlay-put ov 'evaporate t))))
        (setq i (+ i 3))))))

(defun replique-clojure--diag->flymake (buf d)
  "Convert one module diagnostic plist D into a Flymake diagnostic for BUF."
  (let ((beg (replique-clojure--byte->pos (plist-get d :beg)))
        (end (replique-clojure--byte->pos (plist-get d :end)))
        (type (if (eq (plist-get d :sev) :error) :error :warning))
        (msg (plist-get d :msg)))
    (when beg
      (setq end (or end beg))
      ;; Flymake needs a non-empty region.  A zero-width node (a MISSING form)
      ;; gets a one-char region, pulled back at end-of-buffer where it cannot
      ;; extend forward.
      (cond ((> end beg))
            ((< (1+ beg) (point-max)) (setq end (1+ beg)))
            (t (setq beg (max (point-min) (1- beg)) end (point-max))))
      (flymake-make-diagnostic buf beg end type msg))))

(defun replique-clojure--report-flymake (diags)
  "Hand DIAGS to the registered Flymake report function, if any."
  (when replique-clojure--flymake-report-fn
    (let ((buf (current-buffer)))
      (funcall replique-clojure--flymake-report-fn
               (delq nil (mapcar (lambda (d) (replique-clojure--diag->flymake buf d))
                                 diags))))))

(defun replique-clojure--flymake (report-fn &rest _)
  "Flymake backend: register REPORT-FN and report the latest diagnostics."
  (setq replique-clojure--flymake-report-fn report-fn)
  (replique-clojure--report-flymake replique-clojure--last-diags))

(defun replique-clojure--push-def-forms ()
  "Push `replique-clojure-extra-def-forms' to the module for this workspace.
The module then analyses those macros like `defn' (skips the def name, binds
any param vectors), matching how the syntax layer highlights them."
  (when replique-clojure--ws
    (treejure-set-def-forms
     replique-clojure--ws
     (vconcat (delq nil (mapcar (lambda (s) (and (stringp s) s))
                                replique-clojure-extra-def-forms))))))

(defun replique-clojure--refresh-def-forms ()
  "Re-push the extra def-forms and re-run the check (config changed).
Called after `.dir-locals.el' is applied and when the defcustom changes."
  (when (bound-and-true-p replique-clojure-semantic-mode)
    (replique-clojure--push-def-forms)
    (replique-clojure--check t)))

(defun replique-clojure--check (cross-file-p)
  "Run the buffer-local semantic check, refresh overlays, report to Flymake.
CROSS-FILE-P selects the module's fast (nil) or full (t) tier."
  (when (and replique-clojure--ws replique-clojure--file-id)
    (setq replique-clojure--last-diags
          (treejure-check-buffer replique-clojure--ws
                                 replique-clojure--file-id
                                 (replique-clojure--semantic-buffer-text)
                                 cross-file-p))
    (setq replique-clojure--dirty nil)
    (replique-clojure--refresh-overlays)
    (replique-clojure--report-flymake replique-clojure--last-diags)))

(defun replique-clojure--schedule-check (cross-file-p)
  "Arm (or re-arm) the idle timer for a semantic check, coalescing bursts.
CROSS-FILE-P selects the tier; a pending full-tier request is never
downgraded to fast.  Deferring to idle keeps the full tier's dependency I/O
off the redisplay path and collapses bursts (e.g. several
`window-buffer-change-functions' calls) into a single check."
  (when cross-file-p (setq replique-clojure--pending-cross-file t))
  (when (timerp replique-clojure--idle-timer)
    (cancel-timer replique-clojure--idle-timer))
  (let ((buf (current-buffer)))
    (setq replique-clojure--idle-timer
          (run-with-idle-timer
           replique-clojure-semantic-idle-delay nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 ;; Run if the buffer changed (fast) or a full check is pending
                 ;; (a dependency may have gone stale with no local edit).
                 (when (and (bound-and-true-p replique-clojure-semantic-mode)
                            (or replique-clojure--dirty
                                replique-clojure--pending-cross-file))
                   (let ((cf replique-clojure--pending-cross-file))
                     (setq replique-clojure--pending-cross-file nil)
                     (replique-clojure--check cf))))))))))

(defun replique-clojure--after-change (_beg _end _len)
  "Mark the buffer dirty and schedule a debounced fast-tier check."
  (setq replique-clojure--dirty t)
  (replique-clojure--schedule-check nil))

(defun replique-clojure--full-check (&rest _)
  "Run the full-tier check synchronously (save trigger)."
  (when (bound-and-true-p replique-clojure-semantic-mode)
    (replique-clojure--check t)))

(defun replique-clojure--schedule-full-check (&rest _)
  "Defer a coalesced full-tier check (buffer-switch / focus triggers).
Used for `window-buffer-change-functions', a redisplay hook: arming a timer
returns instantly, so the full tier's dependency I/O never runs during
redisplay, and a burst of calls collapses into one check."
  (when (bound-and-true-p replique-clojure-semantic-mode)
    (replique-clojure--schedule-check t)))


;;;; Navigation — xref backend
;;
;; Jump-to-definition and find-references over the module's cached analysis.
;; In-file: a local usage resolves to its binding, a same-namespace var usage to
;; its definition (by resolved identity, so shadowing is handled).  Cross-file:
;; an aliased/qualified or `:refer'-ed var jumps to its definition in the
;; resolved dependency (jump-to-def only; references stay buffer-scoped).  A
;; jar-backed target comes back as a synthetic \"<jar>!<entry>\" path, which we
;; open in a read-only buffer holding the entry's source (extracted via the
;; module's `treejure-jar-entry').  The module returns 0-based byte offsets into
;; whichever file owns the target.

(defun replique-clojure--point-byte (&optional pos)
  "Return the 0-based tree byte offset of POS (or point)."
  (1- (position-bytes (or pos (point)))))

(defun replique-clojure--line-at (pos)
  "Return the text of the line containing POS in the current buffer."
  (save-excursion
    (goto-char pos)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun replique-clojure--jar-location-p (file)
  "Non-nil if FILE is a synthetic \"<jar>!<entry>\" jar-backed location."
  (and (stringp file) (string-match-p "\\.jar!" file)))

(defun replique-clojure--split-jar-location (file)
  "Split a \"<jar>!<entry>\" FILE into (JAR ENTRY), or nil.
Splits at the jar's `.jar' boundary (greedy), the convention the module builds
the synthetic path with, so the entry part may itself contain dots."
  (when (string-match "\\`\\(.*\\.jar\\)!\\(.*\\)\\'" file)
    (list (match-string 1 file) (match-string 2 file))))

(defun replique-clojure--jar-entry-major-mode (entry)
  "Return the major-mode function for a jar ENTRY, by its file extension."
  (pcase (file-name-extension entry)
    ("cljs" #'replique-clojure-clojurescript-mode)
    ("cljc" #'replique-clojure-clojurec-mode)
    (_      #'replique-clojure-mode)))

(defvar replique-clojure--jar-entry-buffers (make-hash-table :test 'equal)
  "Map a \"<jar>!<entry>\" key to its cached read-only source buffer.
Keyed by the full jar path (not its basename), so two jars sharing a basename
never collide.  Buffers are session-lived; a killed one is re-extracted.")

(defun replique-clojure--jar-entry-buffer (jar entry)
  "Return a read-only buffer holding ENTRY extracted from JAR, or nil.
The buffer is extracted once and reused on later jumps (keyed by the full
JAR!ENTRY identity, so same-basename jars do not collide).  Its text is the
exact UTF-8 source the module parsed, so the module's 0-based byte offsets map
through `byte-to-position' unchanged.  Highlighting is on, but the semantic
layer is NOT enabled: this is a read-only viewer of foreign library source, not
a project file to analyse.  Were it enabled it would inherit the *current*
project's workspace (via `default-directory') and, since the buffer has no
`buffer-file-name', intern a junk FileNode under a name-derived bogus path,
running pointless analysis and Flymake on un-editable code."
  (let* ((key (format "%s!%s" jar entry))
         (cached (gethash key replique-clojure--jar-entry-buffers)))
    (if (buffer-live-p cached)
        cached
      (when-let* ((text (treejure-jar-entry jar entry))
                  (buf (generate-new-buffer
                        (format "*treejure %s!%s*"
                                (file-name-nondirectory jar) entry))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (insert text)
            (goto-char (point-min)))
          (set-buffer-modified-p nil)
          (let ((replique-clojure-enable-semantic nil))
            (funcall (replique-clojure--jar-entry-major-mode entry)))
          (read-only-mode 1))
        (puthash key buf replique-clojure--jar-entry-buffers)
        buf))))

(defun replique-clojure--xref-from-buffer (buf byte)
  "Build an xref item at 0-based tree BYTE within BUF, or nil."
  (with-current-buffer buf
    (when-let* ((pos (byte-to-position (1+ byte))))
      (xref-make (replique-clojure--line-at pos)
                 (xref-make-buffer-location buf pos)))))

(defun replique-clojure--xref-item (loc)
  "Build an xref item from module LOC plist (:file :beg :end), or nil.
LOC's `:file' is this buffer (an in-file target), another source file (a
cross-namespace target, read from disk at its last-saved state), or a synthetic
\"<jar>!<entry>\" path (a jar-backed var, opened read-only); the summary line and
position come from whichever buffer owns the target."
  (let* ((file (plist-get loc :file))
         (byte (plist-get loc :beg))
         (buf (cond
               ((and replique-clojure--file-id (equal file replique-clojure--file-id))
                (current-buffer))
               ((replique-clojure--jar-location-p file)
                (pcase (replique-clojure--split-jar-location file)
                  (`(,jar ,entry) (replique-clojure--jar-entry-buffer jar entry))))
               ((file-readable-p file)
                (find-file-noselect file t)))))
    (and buf (replique-clojure--xref-from-buffer buf byte))))

(defun replique-clojure--xref-query-byte (identifier)
  "Return the query byte for IDENTIFIER (its captured point), else point."
  (or (and (stringp identifier)
           (get-text-property 0 'replique-clojure--byte identifier))
      (replique-clojure--point-byte)))

(defun replique-clojure--xref-backend ()
  "The xref backend symbol when the semantic layer is active."
  (and (bound-and-true-p replique-clojure-semantic-mode)
       replique-clojure--ws replique-clojure--file-id
       'replique-clojure))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql replique-clojure)))
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
    (propertize (buffer-substring-no-properties (car bounds) (cdr bounds))
                'replique-clojure--byte
                (replique-clojure--point-byte (car bounds)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql replique-clojure)))
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql replique-clojure)) identifier)
  (replique-clojure--check t)            ; ensure the cached analysis is current
  (when-let* ((loc (treejure-definition
                    replique-clojure--ws replique-clojure--file-id
                    (replique-clojure--xref-query-byte identifier)))
              (item (replique-clojure--xref-item loc)))
    (list item)))

(defun replique-clojure--references-scope ()
  "Return the cross-file find-usages scope as a list of directories, or nil for
buffer-only.  Honors `replique-clojure-semantic-references-scope': `buffer'
yields nil, `project' the project source dirs, and `ask' prompts (classpath /
a picked directory / buffer-only).  The choice is transient — re-decided on
each call, never persisted (PLAN search-scope model)."
  (pcase replique-clojure-semantic-references-scope
    ('buffer nil)
    ('project (replique-clojure--classpath (replique-clojure--project-root)))
    (_ (pcase (completing-read
               "Find references in: "
               '("Project source dirs" "A directory..." "This buffer only")
               nil t nil nil "Project source dirs")
         ("A directory..."
          (list (expand-file-name (read-directory-name "Search directory: "))))
         ("This buffer only" nil)
         (_ (replique-clojure--classpath (replique-clojure--project-root)))))))

(cl-defmethod xref-backend-references ((_backend (eql replique-clojure)) identifier)
  (replique-clojure--check t)
  ;; A local binding is always buffer-scoped (the module ignores the scope for
  ;; it); a var is searched across the chosen scope, filtered by resolved
  ;; identity.  An empty/nil scope means buffer-only (this file's occurrences).
  (let ((scope (replique-clojure--references-scope)))
    (delq nil (mapcar #'replique-clojure--xref-item
                      (treejure-references
                       replique-clojure--ws replique-clojure--file-id
                       (replique-clojure--xref-query-byte identifier)
                       (and scope (vconcat scope)))))))

;;;###autoload
(define-minor-mode replique-clojure-semantic-mode
  "Layer treejure's semantic faces and diagnostics over the syntax layer.
Loads the C module, runs buffer-local analysis on edit/save/buffer-switch, and
renders semantic-face overlays plus Flymake diagnostics."
  :lighter " cljsem"
  (if replique-clojure-semantic-mode
      (condition-case err
          (progn
            (replique-clojure--ensure-module)
            (setq replique-clojure--ws
                  (replique-clojure--get-workspace (replique-clojure--project-root)))
            (setq replique-clojure--file-id
                  (or buffer-file-name (expand-file-name (buffer-name))))
            (setq replique-clojure--category-faces
                  (replique-clojure--build-category-faces))
            (add-hook 'after-change-functions #'replique-clojure--after-change nil t)
            (add-hook 'after-save-hook #'replique-clojure--full-check nil t)
            (add-hook 'window-buffer-change-functions
                      #'replique-clojure--schedule-full-check nil t)
            (add-hook 'flymake-diagnostic-functions #'replique-clojure--flymake nil t)
            (add-hook 'xref-backend-functions #'replique-clojure--xref-backend nil t)
            ;; Re-push def-forms + re-check once `.dir-locals.el' is applied
            ;; (which happens after this mode body runs).
            (add-hook 'hack-local-variables-hook
                      #'replique-clojure--refresh-def-forms nil t)
            (flymake-mode 1)
            (replique-clojure--push-def-forms)
            (replique-clojure--check t))
        (error
         (message "Replique: semantic layer disabled (%s)" (error-message-string err))
         (setq replique-clojure-semantic-mode nil)))
    ;; Teardown.
    (when (timerp replique-clojure--idle-timer)
      (cancel-timer replique-clojure--idle-timer))
    (remove-hook 'after-change-functions #'replique-clojure--after-change t)
    (remove-hook 'after-save-hook #'replique-clojure--full-check t)
    (remove-hook 'window-buffer-change-functions
                 #'replique-clojure--schedule-full-check t)
    (remove-hook 'flymake-diagnostic-functions #'replique-clojure--flymake t)
    (remove-hook 'xref-backend-functions #'replique-clojure--xref-backend t)
    (remove-hook 'hack-local-variables-hook #'replique-clojure--refresh-def-forms t)
    (save-restriction
      (widen)
      (remove-overlays (point-min) (point-max) 'replique-clojure-semantic t))
    (when (and replique-clojure--ws replique-clojure--file-id)
      (treejure-close-buffer replique-clojure--ws replique-clojure--file-id))
    (setq replique-clojure--ws nil
          replique-clojure--file-id nil
          replique-clojure--last-diags nil
          replique-clojure--pending-cross-file nil)))

(provide 'replique-clojure-semantic)

;;; replique-clojure-semantic.el ends here
