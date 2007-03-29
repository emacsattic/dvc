;;; xmtn-dvc.el --- DVC backend for monotone

;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;; Commentary:

;; This file implements a DVC backend for the distributed version
;; control system monotone.  The backend will only work with an
;; appropriate version of the mtn binary installed.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl)
  (require 'dvc-unified)
  (require 'xmtn-basic-io)
  (require 'xmtn-base)
  (require 'xmtn-run)
  (require 'xmtn-automate)
  (require 'xmtn-ids)
  (require 'xmtn-match)
  (require 'dvc-log)
  (require 'dvc-diff))

;; For debugging.
(defun xmtn--load ()
  (require 'dvc-unified)
  (mapc (lambda (file)
          (byte-compile-file file t))
        '("xmtn-minimal.el"
          "xmtn-compat.el"
          "xmtn-match.el"
          "xmtn-base.el"
          "xmtn-run.el"
          "xmtn-automate.el"
          "xmtn-basic-io.el"
          "xmtn-ids.el"
          "xmtn-dvc.el"
          "xmtn-revlist.el")))
;;; (xmtn--load)

;;;###autoload
(dvc-register-dvc 'xmtn "monotone")

(defmacro* xmtn--with-automate-command-output-basic-io-parser
    ((parser root-form command-form &key ((:may-kill-p may-kill-p-form)))
     &body body)
  (let ((parser-tmp (gensym))
        (root (gensym))
        (command (gensym))
        (may-kill-p (gensym))
        (session (gensym))
        (handle (gensym)))
    `(let ((,root ,root-form)
           (,command ,command-form)
           (,may-kill-p ,may-kill-p-form))
       (xmtn-automate-with-session (,session ,root)
         (xmtn-automate-with-command (,handle
                                      ,session ,command
                                      :may-kill-p ,may-kill-p)
           (xmtn-automate-check-for-and-report-error ,handle)
           (xmtn-automate-command-wait-until-finished ,handle)
           (xmtn-basic-io-with-stanza-parser (,parser
                                              (xmtn-automate-command-buffer
                                               ,handle))
             ,@body))))))

;;;###autoload
(defun xmtn-dvc-log-edit-file-name-func (&optional root)
  (concat (file-name-as-directory (or root (dvc-tree-root)))
          "_MTN/log"))

(defun xmtn--tree-default-branch (root)
  (xmtn-automate-simple-command-output-line root '("get_option" "branch")))

(defun xmtn--tree-has-changes-p-future (root)
  (lexical-let ((future
                 (xmtn--command-output-lines-future
                  root
                  ;; Isn't there a better solution to this?
                  '("ls" "changed"))))
    (lambda ()
      (not (endp (funcall future))))))

(defun xmtn--toposort (root revision-hash-ids)
  (xmtn-automate-simple-command-output-lines root
                                             `("toposort"
                                               ,@revision-hash-ids)))

(defun xmtn--map-parsed-certs (xmtn--root xmtn--revision-hash-id xmtn--thunk)
  (lexical-let ((root xmtn--root)
                (revision-hash-id xmtn--revision-hash-id)
                (thunk xmtn--thunk))
    (xmtn--with-automate-command-output-basic-io-parser
      (xmtn--next-stanza root `("certs" ,revision-hash-id))
      (loop
       for xmtn--stanza = (funcall xmtn--next-stanza)
       while xmtn--stanza
       do (xmtn-match xmtn--stanza
            ((("key" (string $xmtn--key))
              ("signature" (string $xmtn--signature))
              ("name" (string $xmtn--name))
              ("value" (string $xmtn--value))
              ("trust" (string $xmtn--trust)))
             (setq xmtn--signature (xmtn-match xmtn--signature
                                     ("ok" 'ok)
                                     ("bad" 'bad)
                                     ("unknown" 'unknown)))
             (let ((xmtn--trusted (xmtn-match xmtn--trust
                                    ("trusted" t)
                                    ("untrusted" nil))))
               (macrolet ((decodef (var)
                            `(setq ,var (decode-coding-string
                                         ,var 'xmtn--monotone-normal-form))))
                 (decodef xmtn--key)
                 (decodef xmtn--name)
                 ;; I'm not sure this is correct.  The documentation
                 ;; mentions a cert_is_binary hook, but it doesn't
                 ;; exist; and even if it did, we would have no way of
                 ;; calling it from here.  But, since cert values are
                 ;; always passed on the command line, and command
                 ;; line arguments are converted to utf-8, I suspect
                 ;; certs will also always be in utf-8.
                 (decodef xmtn--value))
               (funcall thunk
                        xmtn--key xmtn--signature xmtn--name xmtn--value
                        xmtn--trusted))))))))

(defun xmtn--list-parsed-certs (root revision-hash-id)
  (lexical-let ((accu '()))
    (xmtn--map-parsed-certs root revision-hash-id
                            (lambda (key signature name value trusted)
                              (push (list key signature name value trusted)
                                    accu)))
    (setq accu (nreverse accu))
    accu))

(defvar xmtn--log--normalized-files nil)
(defvar xmtn--log--branch nil)
(defvar xmtn--log--root nil)

(defun xmtn--insert-log-edit-hints (root branch buffer prefix normalized-files)
  (with-current-buffer buffer
    (flet ((insert-line (&optional format-string-or-null &rest format-args)
             (if format-string-or-null
                 (let ((line (apply #'format
                                    format-string-or-null format-args)))
                   (assert (not (position ?\n line)))
                   (insert prefix line ?\n))
               (assert (endp format-args))
               (insert prefix ?\n))))
      (save-excursion
        ;; Launching these mtn processes in parallel is a noticeable
        ;; speedup (~14% on some informal benchmarks).  At least it
        ;; was with the version that I benchmarked, etc.
        (xmtn-automate-with-session (nil root)
          (let ((unknown-future (xmtn--unknown-files-future root))
                (missing (funcall (xmtn--missing-files-future root))))
            (when missing
              (insert-line
               "WARNING: There are missing files in this tree.")
              (insert-line "Commit will fail unless you fix this first.")
              (insert-line)
              (insert-line "%s missing file(s):" (length missing))
              (dolist (file missing) (insert-line "%s" file))
              (insert-line)
              (insert-line))
            (insert-line "Committing on branch:")
            (insert-line branch)
            (insert-line)
            (case normalized-files
              (all
               (insert-line "All files selected for commit."))
              (t
               (insert-line "File(s) selected for commit:")
               ;; Normalized file names are easier to read when coming
               ;; from dired buffer, since otherwise, they would contain
               ;; the entire path.
               (dolist (file
                        ;; Sort in an attempt to match the order of
                        ;; "patch" lines, below.
                        (sort (copy-list normalized-files) #'string<))
                 (insert-line "%s" file))))
            ;; Due to the possibility of race conditions, this check
            ;; doesn't guarantee the operation will succeed.
            (if missing
                ;; FIXME: Since automate get_revision can't deal with
                ;; missing files, we should be using automate
                ;; inventory instead.  But its output format needs yet
                ;; another parser...
                (progn (insert-line)
                       (insert-line
                        (concat "Unable to compute modified files while"
                                " files are missing from the tree.")))
              (let ((revision (xmtn--get-revision root `(local-tree ,root))))
                (let ((committed-changes (list))
                      (other-changes (list)))
                  (flet ((collect (path message)
                           (if (or (eql normalized-files 'all)
                                   (member path normalized-files))
                               (push message committed-changes)
                             (push message other-changes))))
                    (loop
                     for (path) in (xmtn--revision-delete revision)
                     do (collect path (format "delete    %s" path)))
                    (loop
                     for (from to) in (xmtn--revision-rename revision)
                     ;; FIXME: collect from or collect to?  Monotone
                     ;; doesn't specify how restrictions work for
                     ;; renamings.
                     do (collect to   (format "rename %s to %s" from to)))
                    (loop
                     for (path) in (xmtn--revision-add-dir revision)
                     do (collect path (format "add_dir   %s" path)))
                    (loop
                     for (path contents)
                     in (xmtn--revision-add-file revision)
                     do (collect path (format "add_file  %s" path)))
                    (loop
                     for (path from-contents to-contents)
                     in (xmtn--revision-patch-file revision)
                     do (collect path (format "patch     %s" path)))
                    (loop
                     for (path attr-name)
                     in (xmtn--revision-clear-attr revision)
                     do (collect path (format "clear %s %s"
                                             path attr-name)))
                    (loop
                     for (path attr-name attr-value)
                     in (xmtn--revision-set-attr revision)
                     do (collect path (format "set %s %s %s"
                                             path attr-name attr-value))))
                  (setq committed-changes (nreverse committed-changes))
                  (setq other-changes (nreverse other-changes))
                  (loop
                   for (lines heading-if heading-if-not) in
                   `((,committed-changes
                      ,(format "%s change(s) in selected files:"
                               (length committed-changes))
                      "No changes in selected files.")
                     (,other-changes
                      ,(format
                        "%s change(s) in files not selected for commit:"
                        (length other-changes))
                      "No changes in files not selected for commit."))
                   do
                   (insert-line)
                   (insert-line "%s" (if lines heading-if heading-if-not))
                   (dolist (line lines) (insert-line "%s" line))))))
            (let ((unknown (funcall unknown-future)))
              (insert-line)
              (if (endp unknown)
                  (insert-line "No unknown files.")
                (insert-line "%s unknown file(s):" (length unknown))
                (dolist (file unknown) (insert-line "%s" file))))))))
    (cond ((eql (point) (point-min))
           ;; We take this as an indicator that there is no log message
           ;; yet.  So insert a blank line.
           (insert "\n")
           (goto-char (point-min)))
          (t
           ;; Moving up onto the last line of the log message seems to
           ;; be better than having the cursor sit at the ## prefix of
           ;; the first line of our hints.
           (forward-line -1))))
  nil)

(add-to-list 'format-alist
             '(xmtn--log-file
               "This format automatically removes xmtn's log edit hints from
the file before saving."
               nil
               xmtn--log-file-format-from-fn
               xmtn--log-file-format-to-fn
               t
               nil
               nil))

(defun xmtn--log-file-format-from-fn (begin end)
  (xmtn--assert-nil))

(defun xmtn--log-file-format-to-fn (begin end buffer)
  (dvc-log-flush-commit-file-list))

;;;###autoload
(defun xmtn-dvc-log-edit ()
  (let ((root (dvc-tree-root))
        (orig-buffer (current-buffer))
        log-edit-buffer)
    (prog2
        (progn
          (dvc-save-some-buffers root)
          (setq log-edit-buffer (dvc-get-buffer-create (dvc-current-active-dvc)
                                                       'log-edit))
          (with-current-buffer log-edit-buffer
            (let ((previously-modified-p (buffer-modified-p)))
              (unwind-protect
                  (dvc-log-flush-commit-file-list)
                (set-buffer-modified-p previously-modified-p)))))
        (dvc-dvc-log-edit)
      (with-current-buffer log-edit-buffer
        (setq buffer-file-coding-system 'xmtn--monotone-normal-form)
        (add-to-list 'buffer-file-format 'xmtn--log-file)
        (let* ((files (or (with-current-buffer dvc-partner-buffer
                            (dvc-current-file-list 'nil-if-none-marked))
                          'all))
               (normalized-files
                (case files
                  (all 'all)
                  (t
                   ;; Need to normalize in original buffer, since
                   ;; switching buffers changes default-directory and
                   ;; therefore the semantics of relative file names.
                   (with-current-buffer orig-buffer
                     (xmtn--normalize-file-names root files))))))
          (let ((previously-modified-p (buffer-modified-p)))
            (unwind-protect
                (let ((branch (xmtn--tree-default-branch root)))
                  (goto-char (point-max))
                  (xmtn--insert-log-edit-hints root
                                               branch
                                               (current-buffer)
                                               dvc-log-edit-flush-prefix
                                               normalized-files)
                  (set (make-local-variable 'xmtn--log--root) root)
                  (set (make-local-variable 'xmtn--log--normalized-files)
                       normalized-files)
                  (set (make-local-variable 'xmtn--log--branch) branch))
              (set-buffer-modified-p previously-modified-p))))
        ;; This allows using `find-file-at-point' on file names in our
        ;; log edit hints.  Really convenient.
        (setq default-directory root)))))

;;;###autoload
(defun xmtn-dvc-log-edit-done ()
  (let* ((root xmtn--log--root)
         (normalized-files xmtn--log--normalized-files)
         (branch xmtn--log--branch))
    ;; Saving the buffer will automatically flush the log edit hints.
    (save-buffer)
    (dvc-save-some-buffers root)
    ;; Need to do this after `dvc-save-some-buffers'.
    ;;
    ;; Due the possibility of race conditions, this check doesn't
    ;; guarantee the operation will succeed.
    (unless (endp (funcall (xmtn--missing-files-future root)))
      (error "Missing files in tree, unable to commit"))
    ;; mtn ls changed doesn't work while the tree has missing files,
    ;; so we can't run the two futures in parallel.  (Or maybe we
    ;; could, if a future that is never forced would never report
    ;; errors in its process.)
    (unless (funcall (xmtn--tree-has-changes-p-future root))
      (error "No changes to commit"))
    (lexical-let* ((progress-message
                    (case normalized-files
                      (all (format "Committing all files in %s" root))
                      (t (case (length normalized-files)
                           (0 (assert nil))
                           (1 (format "Committing file %s in %s"
                                      (first normalized-files) root))
                           (t
                            (format "Committing %s files in %s"
                                    (length normalized-files)
                                    root))))))
                   (log-edit-buffer (current-buffer))
                   (log-edit-file (buffer-file-name))
                   (commit-message-file
                    (xmtn--make-temp-file
                     (concat (expand-file-name log-edit-file) "-xmtn")
                     nil ".tmp")))
      ;; Monotone's rule that _MTN/log must not exist when committing
      ;; non-interactively is really a pain to deal with.
      (rename-file log-edit-file commit-message-file t)
      (xmtn--run-command-async
       root
       `("commit" ,(concat "--message-file=" commit-message-file)
         ,(concat "--branch=" branch)
         ,@(case normalized-files
             (all '())
             (t (list*
                 ;; This is the right thing for directory renames
                 ;; marked in diff buffer.  I don't know yet whether
                 ;; it's the right thing in other cases.
                 "--depth=0"
                 "--" normalized-files))))
       :error (lambda (output error status arguments)
                (rename-file commit-message-file log-edit-file)
                (dvc-default-error-function output error
                                            status arguments))
       :killed (lambda (output error status arguments)
                 (rename-file commit-message-file log-edit-file)
                 (dvc-default-killed-function output error
                                              status arguments))
       :finished (lambda (output error status arguments)
                   (message "%s...done" progress-message)
                   ;; Monotone creates an empty log file when the
                   ;; commit was successful.  Let's not interfere with
                   ;; that.  (Calling `dvc-log-close' would.)
                   (delete-file commit-message-file)
                   (kill-buffer log-edit-buffer)))
      ;; Show message _after_ spawning command to override DVC's
      ;; debugging message.
      (message "%s..." progress-message))
    (set-window-configuration dvc-pre-commit-window-configuration)))

;; The term "normalization" here has nothing to do with Unicode
;; normalization.
(defun xmtn--normalize-file-name (root file-name)
  (assert root)
  (let ((normalized-name (file-relative-name file-name root)))
    normalized-name))

(defun xmtn--normalize-file-names (root file-names)
  (check-type file-names list)
  (mapcar (lambda (file-name) (xmtn--normalize-file-name root file-name))
          file-names))

(defun xmtn--display-buffer-maybe (buffer dont-switch)
  (let ((orig-buffer (current-buffer)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (when dont-switch (pop-to-buffer orig-buffer)))
  nil)

(defun xmtn--parse-diff-for-dvc (changes-buffer)
  ;; The protocol to tell DVC about the changes is kind of unknown.
  (flet ((add-entry (path status modif dir &optional orig-path)
           (with-current-buffer changes-buffer
             (ewoc-enter-last
              dvc-diff-cookie
              (list 'file path
                    ;;; These are guesswork.  Partly based on
                    ;;; `dvc-diff-chose-face'.  I have no idea what
                    ;;; the distinction between status and modif is.
                    (ecase status
                      (deleted "D")     ; conflict?
                      (renamed "R")
                      (() "?")          ; unknown
                      (() "C")          ; conflict
                      (() "P")
                      (added "A")
                      (modified "M")
                      (unchanged " "))
                    (ecase modif
                      (added "A")
                      (modified "M")
                      (renamed "R")
                      (() "C")
                      (deleted "D")
                      (unchanged " "))
                    (if dir "/" " ") orig-path))))
         (likely-dir-p (path) (string-match "/\\'" path)))
    ;; First parse the basic_io contained in dvc-header.
    (let ((revision
           (with-temp-buffer
             (insert dvc-header)
             (goto-char (point-min))
             (while (re-search-forward "^# ?" nil t)
               (replace-match ""))
             (goto-char (point-min))
             ;; Skip first line, it's empty.
             (forward-line)
             (delete-region (point-min) (point))
             (xmtn-basic-io-with-stanza-parser
              (parser (current-buffer))
              (xmtn--parse-partial-revision parser)))))
      ;; FIXME: I don't know what status and modif mean, so I don't
      ;; know the correct values for them for each case.
      (loop
       for (path) in (xmtn--revision-delete revision)
       do (add-entry path 'deleted 'deleted
                     (likely-dir-p path)))
      (loop
       for (from to) in (xmtn--revision-rename revision)
       do (assert (eql (not (likely-dir-p from))
                       (not (likely-dir-p to))))
       do (add-entry to 'renamed 'renamed
                     (likely-dir-p to)
                     from))
      (loop
       for (path) in (xmtn--revision-add-dir revision)
       do (add-entry path 'added 'added t))
      (loop
       for (path contents)
       in (xmtn--revision-add-file revision)
       do (add-entry path 'added 'added nil))
      (loop
       for (path from-contents to-contents)
       in (xmtn--revision-patch-file revision)
       do (add-entry path 'modified 'modified nil))
      ;; Do nothing about clear-attr and set-attr.
      ))
  ;; This would suppress mtn's revision basic_io junk at the top.  Not
  ;; sure if anyone needs it.  But since we also delete the file
  ;; content ids from the diff headings, I'd rather at least keep them
  ;; there.
  ;;(setq dvc-header "")
  nil)

;;;###autoload
(defun xmtn-show-base-revision ()
  "Shows the base revision of the current monotone tree in the minibuffer."
  (interactive)
  (let ((root (dvc-tree-root)))
    (message "Base revision of tree %s is %s"
             root
             (xmtn--get-base-revision-hash-id root))))


;;;###autoload
(defun xmtn-dvc-search-file-in-diff (file)
  (re-search-forward
   (let ((quoted-file (regexp-quote file)))
     (concat "^\\(\\("
             "\\+\\+\\+ " quoted-file
             "\\)\\|\\("
             ;; FIXME: What `dvc-diff-diff-or-list' does doesn't work
             ;; for this case, since `diff-hunk-next' doesn't recognize
             ;; mtn's output for this case as a diff hunk.
             "# " quoted-file " is binary"
             "\\)\\)$"))))


;; I don't know what the semantics of AGAINST and BASE-REV are.  I'll
;; assume that AGAINST would be the first argument to diff ("old") and
;; defaults to the revision the workspace is based on, and BASE-REV
;; would be the second argument to diff ("new") and defaults to the
;; workspace.
;;;###autoload
(defun xmtn-dvc-diff (&optional against path dont-switch base-rev)
  (let ((root (dvc-tree-root path)))
    (unless against (setq against
                          ;;`(xmtn (previous-revision (local-tree ,root) 1))
                          `(xmtn (last-revision ,root 1))))
    (unless base-rev (setq base-rev `(xmtn (local-tree ,root))))
    (lexical-let ((buffer (dvc-prepare-changes-buffer against base-rev
                                                      'diff root 'xmtn))
                  (dont-switch dont-switch))
      (buffer-disable-undo buffer)
      (dvc-save-some-buffers root)
      ;; Due to the possibility of race conditions, this check doesn't
      ;; guarantee the operation will succeed.
      (unless (endp (funcall (xmtn--missing-files-future root)))
        (error "Missing files in tree, unable to diff"))
      (let ((against-resolved (xmtn--resolve-revision-id root against))
            (base-rev-resolved (xmtn--resolve-revision-id root base-rev)))
        (let ((rev-specs
               `(,(xmtn-match against-resolved
                    ((local-tree $path)
                     ;; AGAINST is not a committed revision, but the
                     ;; workspace.  mtn diff can't directly handle
                     ;; this case.
                     (error "not implemented"))
                    ((revision $hash-id)
                     (concat "--revision=" hash-id)))
                 ,@(xmtn-match base-rev-resolved
                     ((local-tree $path)
                      (assert (xmtn--same-tree-p root path))
                      `())
                     ((revision $hash-id)
                      `(,(concat "--revision=" hash-id)))))))
          ;; FIXME: Use automate content_diff and get_revision.
          (xmtn--run-command-async
           root `("diff" ,@rev-specs)
           :related-buffer buffer
           :finished
           (lambda (output error status arguments)
             (with-current-buffer output
               (xmtn--remove-content-hashes-from-diff))
             (dvc-show-changes-buffer output 'xmtn--parse-diff-for-dvc
                                      buffer dont-switch "^=")))))
      (xmtn--display-buffer-maybe buffer dont-switch))))

(defun xmtn--remove-content-hashes-from-diff ()
  ;; Hack: Remove mtn's file content hashes from diff headings since
  ;; `dvc-diff-diff-or-list' and `dvc-diff-find-file-name' gets
  ;; confused by them.
  (save-excursion
    (goto-char (point-min))
    (while
        (re-search-forward
         "^\\(\\+\\+\\+\\|---\\) \\(.*\\)\\(\t[0-9a-z]\\{40\\}\\)$"
         nil t)
      (replace-match "" t nil nil 3))))


(defun xmtn--simple-finished-notification (buffer)
  (lexical-let ((buffer buffer))
    (lambda (output error status arguments)
      (message "Process %s finished" buffer))))

;;;###autoload
(defun xmtn-dvc-command-version ()
  (format "%s\nautomate interface version %s"
          (third (xmtn--command-version))
          (third (xmtn--automate-interface-version))))

(defun xmtn--unknown-files-future (root)
  (xmtn--command-output-lines-future root '("ls" "unknown")))

(defun xmtn--missing-files-future (root)
  (xmtn--command-output-lines-future root '("ls" "missing")))

;; FIXME: I don't know what AGAINST is for.
;;;###autoload
(defun xmtn-dvc-status (&optional against)
  ;; FIXME: Use mtn automate inventory?
  (let ((root (dvc-tree-root)))
    (let ((missing-future (xmtn--missing-files-future root)))
      (lexical-let ((buffer (dvc-get-buffer-create 'xmtn 'status root))
                    (root root))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (view-mode 1)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Status for %s:\n\n" root))
            (goto-char (point-min))))
        ;; Due to the possibility of race conditions, this check
        ;; doesn't guarantee the operation will succeed.
        (if (funcall missing-future)
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (save-excursion
                  (goto-char (point-max))
                  (insert "\nMissing files:\n")
                  (dolist (file (funcall missing-future))
                    (insert file ?\n))
                  (insert "\nUnable to compute remainder of status while files are missing from the tree\n"))))
          (xmtn--command-append-to-buffer-async
           buffer root
           `("status")
           :finished
           (lambda (output error status arguments)
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (save-excursion
                   (goto-char (point-max))
                   (insert "\nUnknown:\n"))))
             (xmtn--command-append-to-buffer-async
              buffer root
              `("ls" "unknown")
              :finished (xmtn--simple-finished-notification buffer)))))
        (xmtn--display-buffer-maybe buffer nil)))))

;;;###autoload
(defun xmtn-dvc-revision-direct-ancestor (revision-id)
  (let* ((root (dvc-tree-root))
         (resolved-id (xmtn--resolve-revision-id root revision-id)))
    `(xmtn ,(xmtn--resolve-backend-id root
                                      `(previous-revision ,resolved-id 1)))))

;;;###autoload
(defun xmtn-dvc-name-construct (backend-revision)
  (check-type backend-revision xmtn--hash-id)
  backend-revision)

(defun xmtn--mtnignore-file-name (root)
   (concat (file-name-as-directory root) ".mtn-ignore"))

;;;###autoload
(defun xmtn-dvc-edit-ignore-files ()
  (find-file-other-window (xmtn--mtnignore-file-name (dvc-tree-root))))

(defun xmtn--quote-string-as-partial-perl-regexp (string)
  ;; The set of file names/patterns to be ignored by monotone is
  ;; customizable by the user through a hook.  So we can't guarantee
  ;; that writing something to .mtn-ignore really has the desired
  ;; effect.  However, we implement the correct behavior for the
  ;; default hook.
  ;;
  ;; The default hook uses the function regex.search, which is defined
  ;; in lua.cc, which, as of monotone revision
  ;; 341e4a18c594cec49896fa97bd4e74de7bee5827, uses Boost.Regex with
  ;; the default settings (Perl syntax).
  ;;
  ;; http://www.boost.org/libs/regex/doc/syntax_perl.html describes
  ;; this syntax.  This implementation is based on that description.
  (let ((special-chars ".[{()\*+?|^$"))
    (with-output-to-string
      (loop for char across string
            do
            (when (position char special-chars) (write-char ?\\))
            (write-char char)))))

(defun xmtn--perl-regexp-for-extension (extension)
  (format "\\.%s$" (xmtn--quote-string-as-partial-perl-regexp extension)))

(defun xmtn--perl-regexp-for-file-name (file-name)
  (format "^%s$" (xmtn--quote-string-as-partial-perl-regexp file-name)))

(defun xmtn--perl-regexp-for-files-in-directory (directory-file-name)
  (format "^%s" (xmtn--quote-string-as-partial-perl-regexp
                 (file-name-as-directory directory-file-name))))

(defun xmtn--add-patterns-to-mtnignore (root patterns interactive-p)
  (let ((mtnignore-file-name (xmtn--mtnignore-file-name root)))
    (save-window-excursion
      (find-file-other-window mtnignore-file-name)
      (save-excursion
        (let ((modified-p nil))
          (loop for pattern in patterns
                do
                (goto-char (point-min))
                (unless (re-search-forward (concat "^" (regexp-quote pattern)
                                                   "$")
                                           nil t)
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert pattern "\n")
                  (setq modified-p t)))
          (when modified-p
            (if interactive-p
                (lexical-let ((buffer (current-buffer)))
                  (save-some-buffers nil (lambda ()
                                           (eql (current-buffer) buffer))))
              (save-buffer))
            (when (not (xmtn--file-registered-p root mtnignore-file-name))
              (when (if interactive-p
                        (y-or-n-p (format "Add file %s to workspace manifest? "
                                          mtnignore-file-name))
                      t)
                (xmtn--add-files root (list mtnignore-file-name)))))))))
  nil)

;;;###autoload
(defun xmtn-dvc-ignore-files (file-names)
  (assert (not (endp file-names)))
  (let* ((root (dvc-tree-root))
         (normalized-file-names (xmtn--normalize-file-names root file-names))
         (msg (case (length file-names)
                (1 (format "%s" (first normalized-file-names)))
                (t (format "%s files/directories"
                           (length normalized-file-names))))))
    (when (y-or-n-p (format "Ignore %s in monotone tree %s? " msg root))
      (xmtn--add-patterns-to-mtnignore
       root
       (let ((default-directory root))
         (mapcan (lambda (file-name)
                   (if (or (file-symlink-p file-name)
                           (not (file-directory-p file-name)))
                       (list (xmtn--perl-regexp-for-file-name file-name))
                     (setq file-name (directory-file-name file-name))
                     ;; For a file-name that is a directory, add not
                     ;; just "^file-name$", but also "^file-name/", so
                     ;; that files inside the directory will also be
                     ;; ignored.  This is usually What I Mean.
                     (list
                      (xmtn--perl-regexp-for-file-name file-name)
                      (xmtn--perl-regexp-for-files-in-directory file-name))))
                 normalized-file-names))
       t))))

;;;###autoload
(defun xmtn-dvc-ignore-file-extensions (file-names)
  (let* ((extensions (delete nil (mapcar #'file-name-extension file-names)))
         (root (dvc-tree-root))
         (msg (case (length extensions)
                (1 (format "extension *.%s" (first extensions)))
                (t (format "%s extensions" (length extensions))))))
    ;; This UI stuff shouldn't have to be part of the backend.
    (if extensions
        (when (y-or-n-p (format "Ignore %s in monotone tree %s? " msg root))
          (xmtn--add-patterns-to-mtnignore
           root
           (mapcar #'xmtn--perl-regexp-for-extension extensions)
           t))
      (error "No files with an extension selected"))))

(defun xmtn--add-files (root file-names)
  (dolist (file-name file-names)
    ;; On directories, mtn add will recurse, which isn't what we want.
    (assert (not (file-directory-p file-name)))
    ;; I don't know how mtn handles symlinks (and symlinks to
    ;; directories), so forbid them for now.
    (assert (not (file-symlink-p file-name))))
  (setq file-names (xmtn--normalize-file-names root file-names))
  (xmtn--run-command-sync root
                          `("add" "--" ,@file-names)))

(defun xmtn--file-registered-p (root file-name)
  (and (xmtn--revision-manifest-file-entry root `(local-tree ,root)
                                           (xmtn--normalize-file-name root
                                                                      file-name))
       t))

;;;###autoload
(defun xmtn-dvc-add-files (&rest files)
  (xmtn--add-files (dvc-tree-root) files))

;; Appears redundant, given that there is `xmtn-dvc-add-files'.  But
;; it's part of the DVC API.  FIXME.
;;;###autoload
(defun xmtn-dvc-add (file)
  (xmtn--add-files (dvc-tree-root) (list file)))

(defun xmtn--do-remove (root file-names do-not-execute)
  (xmtn--run-command-sync
   root `("drop"
          ,@(if do-not-execute `() `("--execute"))
          "--" ,@(xmtn--normalize-file-names root file-names)))
  nil)

;;;###autoload
(defun xmtn-dvc-remove-files (&rest files)
  (xmtn--do-remove (dvc-tree-root) files nil))

(defun xmtn--do-rename (root from-normalized-name to-normalized-name
                             do-not-execute)
  (xmtn--run-command-sync
   root `("rename"
          ,@(xmtn--version-case
              ((or mainline (> 0 33))
               (if do-not-execute `("--bookkeep-only") `()))
              (t
               (if do-not-execute `() `("--execute"))))
          "--" ,from-normalized-name ,to-normalized-name))
  ;; FIXME: We should do something analogous to
  ;; `dvc-revert-some-buffers' (but for renaming) here.  But DVC
  ;; doesn't provide a function for that.
  nil)

;;;###autoload
(defun xmtn-dvc-rename ()
  ;; For the moment, we only provide an interface to the --execute
  ;; variant of mtn rename.  I think the non---execute variant should
  ;; be an entirely different command.  (One indication of this is
  ;; that it needs different completion criteria.)
  (let ((root (dvc-tree-root)))
    ;; This UI stuff shouldn't have to be part of the backend.
    (let* ((from-name (dvc-confirm-read-file-name "Rename: " t))
           (to-name (dvc-confirm-read-file-name
                     (format "Rename %s to: " from-name)
                     nil "" from-name)))
      (unless (or (not (file-exists-p to-name))
                  (file-directory-p to-name))
        (error "%s exists and is not a directory" to-name))
      (when (file-directory-p to-name)
        (setq to-name (file-name-as-directory to-name)))
      (let ((to-normalized-name (xmtn--normalize-file-name root to-name))
            (from-normalized-name (xmtn--normalize-file-name root from-name)))
        (xmtn--do-rename root
                         from-normalized-name
                         to-normalized-name
                         nil)))))

(defun xmtn--heads (root branch)
  (xmtn-automate-simple-command-output-lines root `("heads" ,branch)))


(defun xmtn--insert-hint-into-process-buffer (string)
  (let ((inhibit-read-only t)
        deactivate-mark)
    (save-excursion
      (let ((start (point)))
        (insert string)
        (let ((end (1- (point))))
          (add-text-properties start end '(face (:slant italic))))))))

(defun xmtn--run-command-that-might-invoke-merger (root command)
  ;; Run async, not sync; it might recursively invoke emacsclient for
  ;; merging; and we might need to send an enter keystroke when
  ;; finished.
  (xmtn--run-command-async
   root command
   :finished
   (lambda (output error status arguments)
     (with-current-buffer output
       (save-excursion
         (goto-char (point-max))
         (xmtn--insert-hint-into-process-buffer "[process finished]\n"))))
   :error
   (lambda (output error status arguments)
     (with-current-buffer output
       (save-excursion
         (goto-char (point-max))
         (xmtn--insert-hint-into-process-buffer
          "[process terminated with an error]\n")))))
  ;; Show process buffer.  Monotone might spawn an external merger and
  ;; ask the user to hit enter when finished.
  (dvc-show-process-buffer)
  (goto-char (point-min))
  (xmtn--insert-hint-into-process-buffer
   (substitute-command-keys
    (concat
     "This buffer will show the output of the mtn subprocess, if any."
     "\nTo send an \"enter\" keystroke to mtn, use"
     " \\[xmtn-send-enter-to-subprocess]"
     "\nin this buffer.  This might be necessary"
     " if mtn launches an external merger."
     "\nWhen mtn has finished, just bury this buffer, or kill it."
     "\n")))
  (goto-char (point-max))
  ;; I don't think DVC's process filter can deal with read-only
  ;; buffers yet.
  ;;(setq buffer-read-only t)
  )

;;;###autoload
(defun xmtn-send-enter-to-subprocess ()
  "Send an \"enter\" keystroke to a monotone subprocess.

To be used in an xmtn process buffer.  Useful when monotone
spawns an external merger and asks you to hit enter when
finished."
  (interactive)
  (let ((process (loop for (process nil) in dvc-process-running
                       when (eql (current-buffer) (process-buffer process))
                       return process)))
    (unless process
      (error "No active process for buffer %s found" (current-buffer)))
    (process-send-string process "\n")
    (save-excursion
      (goto-char (point-max))
      (xmtn--insert-hint-into-process-buffer "[sent enter keystroke]\n"))))

;;; It's kind of a wart that these "xmtn--do-<operation>" functions
;;; don't have the same contract with respect to
;;; synchronousness/asynchronousness, progress messages and return
;;; value.

(defun xmtn--do-explicit-merge (root left-revision-hash-id right-revision-hash-id
                                     destination-branch-name)
  (check-type root string)
  (check-type left-revision-hash-id xmtn--hash-id)
  (check-type right-revision-hash-id xmtn--hash-id)
  (check-type destination-branch-name string)
  (xmtn--run-command-that-might-invoke-merger root
                                              `("explicit_merge"
                                                "--"
                                                ,left-revision-hash-id
                                                ,right-revision-hash-id
                                                ,destination-branch-name))
  nil)

(defun xmtn--do-disapprove-future (root revision-hash-id)
  ;; Returns a future so the calling code can block on its completion
  ;; if it wants to.
  (check-type root string)
  (check-type revision-hash-id xmtn--hash-id)
  (xmtn--command-output-lines-future root `("disapprove" ,revision-hash-id)))

(defun xmtn--do-update (root target-revision-hash-id changes-p-future)
  (check-type root string)
  (check-type target-revision-hash-id xmtn--hash-id)
  (let ((progress-message (format "Updating tree %s to revision %s"
                                  root target-revision-hash-id))
        (command `("update" ,(concat "--revision=" target-revision-hash-id))))
    (if (funcall changes-p-future)
        (progn (message "%s" progress-message)
               (xmtn--run-command-that-might-invoke-merger root command))
      (message "%s..." progress-message)
      (xmtn--run-command-sync root command)
      (message "%s...done" progress-message)))
  (dvc-revert-some-buffers root)
  nil)

(defun xmtn--update-after-confirmation (root target-revision-hash-id)
  (when (equal (xmtn--get-base-revision-hash-id root) target-revision-hash-id)
    (error "Tree %s is already based on target revision %s"
           root target-revision-hash-id))
  (dvc-save-some-buffers root)
  ;; Due to the possibility of race conditions, this check doesn't
  ;; guarantee the operation will succeed.
  (unless (endp (funcall (xmtn--missing-files-future root)))
    (error "Missing files in tree, unable to update"))
  ;; tree-has-changes-p will break if files are missing; can't spawn
  ;; them in parallel.
  (let ((changes-p-future (xmtn--tree-has-changes-p-future root)))
    (unless (y-or-n-p
             (format (concat "Update tree %s to revision %s? ")
                     root target-revision-hash-id))
      (error "Aborted update"))
    (when (funcall changes-p-future)
      (unless (yes-or-no-p
               (format (concat
                        "Tree %s contains uncommitted changes.  Update anyway? ")
                       root))
        (error "Aborted update")))
    (xmtn--do-update root target-revision-hash-id changes-p-future))
  nil)

;;;###autoload
(defun xmtn-dvc-update ()
  (let ((root (dvc-tree-root)))
    (xmtn-automate-with-session (nil root)
      (let* ((branch (xmtn--tree-default-branch root))
             (heads (xmtn--heads root branch)))
        (case (length heads)
          (0 (assert nil))
          (1
           (xmtn--update-after-confirmation root
                                            (first heads)))
          (t
           ;; Should probably prompt user to choose one head, but let's
           ;; keep it simple for now.
           (error (substitute-command-keys
                   (concat "Branch %s is unmerged (%s heads)."
                           "  Try \\[xmtn-view-heads-revlist] and"
                           " \\[xmtn-revlist-update] or"
                           " \\[xmtn-revlist-explicit-merge]"))
                  branch (length heads)))))))
  nil)

;;;###autoload
(defun xmtn-dvc-revert-files (file-names)
  ;; Accepting a string seems to be part of the API.
  (when (stringp file-names) (setq file-names (list file-names)))
  (let ((root (dvc-tree-root)))
    (assert (not (endp file-names)))
    (dvc-save-some-buffers root)
    (let ((normalized-file-names (xmtn--normalize-file-names root file-names)))
      (lexical-let
          ((root root)
           (progress-message
            (if (eql (length file-names) 1)
                (format "Reverting file %s" (first file-names))
              (format "Reverting %s files" (length file-names)))))
        (message "%s..." progress-message)
        (xmtn--run-command-sync root `("revert" "--"
                                       ,@normalized-file-names)
                                :finished
                                (lambda (output error status arguments)
                                  (message "%s...done" progress-message)))
        (dvc-revert-some-buffers root))))
  nil)

;;;###autoload
(defun xmtn-dvc-files-to-commit ()
  ;; This is called only from `dvc-log-insert-commit-file-list'.  But
  ;; we have our own function `xmtn--insert-log-edit-hints', which
  ;; subsumes it.  FIXME: DVC doesn't seem to provide a well-defined
  ;; way to override `dvc-log-insert-commit-file-list'.  We should
  ;; change that.
  (error "not implemented"))

;;;###autoload
(defun xmtn-revision-get-previous-revision (file stuff)
  (xmtn--revision-get-file-helper file `(previous-revision ,@stuff)))

;;;###autoload
(defun xmtn-revision-get-last-revision (file stuff)
  (xmtn--revision-get-file-helper file `(last-revision ,@stuff)))

;;;###autoload
(defun xmtn-revision-get-file-revision (file stuff)
  (xmtn--revision-get-file-helper file `(revision ,@stuff)))

(defun xmtn--revision-get-file-helper (file backend-id)
  (let ((root (dvc-tree-root)))
    (xmtn-automate-with-session (nil root)
      (let* ((normalized-file (xmtn--normalize-file-name root file))
             (corresponding-file
              (xmtn--get-corresponding-path root normalized-file
                                            `(local-tree ,root) backend-id)))
        (if (null corresponding-file)
            ;; File doesn't exist.  Since this function is (as far
            ;; as I know) only called from diff-like functions, a
            ;; missing file is not an error but just means the diff
            ;; should be computed against an empty file.  So just
            ;; leave the buffer empty.
            (progn)
          ;; Note: This could be simplified slightly with the new
          ;; automate get_file_of operation.  Not worth any effort
          ;; right now, though.  And it would break compatibility with
          ;; 0.30 for no good reason.
          (let ((contents-hash
                 (xmtn--revision-file-contents-hash root backend-id
                                                    corresponding-file)))
            (xmtn--insert-file-contents root contents-hash
                                        (current-buffer))))))))


(defun xmtn--get-corresponding-path (root normalized-file-name
                                          source-revision-backend-id
                                          target-revision-backend-id)
  (block get-corresponding-path
    (xmtn-automate-with-session (nil root)
      (let (source-revision-hash-id
            target-revision-hash-id
            (file-name-postprocessor #'identity))
        (let ((resolved-source-revision
               (xmtn--resolve-backend-id root source-revision-backend-id))
              (resolved-target-revision
               (xmtn--resolve-backend-id root target-revision-backend-id)))
          (xmtn-match resolved-source-revision
            ((revision $hash-id)
             (setq source-revision-hash-id hash-id))
            ((local-tree $path)
             (assert (xmtn--same-tree-p root path))
             (let ((base-revision-hash-id
                    (xmtn--get-base-revision-hash-id path)))
               (if (null base-revision-hash-id)
                   (xmtn-match resolved-target-revision
                     ((revision $hash-id)
                      (return-from get-corresponding-path nil))
                     ((local-tree $target-path)
                      (assert (xmtn--same-tree-p path target-path))
                      (return-from get-corresponding-path normalized-file-name)))
                 (let* ((revision
                         (xmtn--get-revision path `(local-tree ,path)))
                        (rename-entry (find normalized-file-name
                                            (xmtn--revision-rename revision)
                                            :key #'second
                                            :test #'equal)))
                   (when rename-entry
                     (setq normalized-file-name (first rename-entry)))
                   (setq source-revision-hash-id base-revision-hash-id))))))
          (xmtn-match resolved-target-revision
            ((revision $hash-id)
             (setq target-revision-hash-id hash-id))
            ((local-tree $path)
             (assert (xmtn--same-tree-p root path))
             (let ((base-revision-hash-id
                    (xmtn--get-base-revision-hash-id path)))
               (if (null base-revision-hash-id)
                   (return-from get-corresponding-path nil)
                 (setq target-revision-hash-id base-revision-hash-id
                       file-name-postprocessor
                       (lexical-let ((path path)
                                     (base-revision-hash-id
                                      base-revision-hash-id))
                         (lambda (file-name)
                           (let* ((revision
                                   (xmtn--get-revision path `(local-tree ,path)))
                                  (rename-entry
                                   (find file-name
                                         (xmtn--revision-rename revision)
                                         :key #'first
                                         :test #'equal)))
                             (if (null rename-entry)
                                 file-name
                               (second rename-entry)))))))))))
        (xmtn--with-automate-command-output-basic-io-parser
          (next-stanza root `("get_corresponding_path"
                              ,source-revision-hash-id
                              ,normalized-file-name
                              ,target-revision-hash-id))
          (xmtn-match (funcall next-stanza)
            (nil nil)
            ((("file" (string $result)))
             (assert (null (funcall next-stanza)))
             (funcall file-name-postprocessor result))))))))

(defun xmtn--manifest-find-file (root manifest normalized-file-name)
  (let ((matches (remove* normalized-file-name
                          (remove* 'file manifest :key #'first :test-not #'equal)
                          :key #'second :test-not #'equal)))
    (xmtn--assert-optional (member (length matches) '(0 1)))
    (first matches)))

(defun xmtn--revision-manifest-file-entry (root backend-id
                                                normalized-file-name)
  (let ((manifest (xmtn--get-manifest root backend-id)))
    (xmtn--manifest-find-file root manifest normalized-file-name)))

(defun xmtn--revision-file-contents-hash (root backend-id normalized-file-name)
  (xmtn-match (xmtn--revision-manifest-file-entry root backend-id
                                                  normalized-file-name)
    ((file $relative-path $file-contents-hash $attrs)
     (assert (equal relative-path normalized-file-name))
     file-contents-hash)))

(defun xmtn--file-contents-as-string (root content-hash-id)
  (check-type content-hash-id xmtn--hash-id)
  (xmtn-automate-simple-command-output-string
   root `("get_file" ,content-hash-id)))

(defun xmtn--insert-file-contents (root content-hash-id buffer)
  (check-type content-hash-id xmtn--hash-id)
  (xmtn-automate-simple-command-output-insert-into-buffer
   root buffer `("get_file" ,content-hash-id)))


(defun xmtn--same-tree-p (a b)
  (equal (file-truename a) (file-truename b)))

(defun xmtn--get-manifest (root backend-id)
  (xmtn-automate-with-session (nil root)
    (let ((resolved-id (xmtn--resolve-backend-id root backend-id)))
      (xmtn--with-automate-command-output-basic-io-parser
       (parser root `("get_manifest_of"
                      ,@(xmtn-match resolved-id
                          ((local-tree $path)
                           ;; FIXME: I don't really know what to do if
                           ;; PATH is not the same as ROOT.  Maybe
                           ;; revision id resolution needs to return
                           ;; the proper root, too.
                           (assert (xmtn--same-tree-p root path))
                           '())
                          ((revision $hash-id)
                           `(,hash-id)))))
       (assert (equal (funcall parser) '(("format_version" (string "1")))))
       (loop for stanza = (funcall parser)
             while stanza
             collect (xmtn-match stanza
                       ((("dir" (string $normalized-path)))
                        (let ((dir (decode-coding-string
                                    normalized-path
                                    'xmtn--monotone-normal-form)))
                          (xmtn--assert-optional
                           (or (equal dir "")
                               (not (eql (aref dir (1- (length dir))) ?/))))
                          `(dir ,dir)))
                       ((("file" (string $normalized-path))
                         ("content" (id $hash-id))
                         . $attrs)
                        `(file
                          ,(decode-coding-string
                            normalized-path 'xmtn--monotone-normal-form)
                          ,hash-id
                          ,(mapcar (lambda (attr-entry)
                                     (xmtn-match attr-entry
                                       (("attr"
                                         (string $attr-name)
                                         (string $attr-value))
                                        (list attr-name attr-value))))
                                   attrs)))))))))

(defstruct (xmtn--revision (:constructor xmtn--make-revision))
  new-manifest-hash-id
  old-revision-hash-ids
  delete
  rename
  add-dir
  add-file
  patch-file
  clear-attr
  set-attr
  )


(defun xmtn--get-revision (root backend-id)
  (xmtn-automate-with-session (nil root)
    (let ((resolved-id (xmtn--resolve-backend-id root backend-id)))
      (xmtn--with-automate-command-output-basic-io-parser
       (parser root `("get_revision"
                      ,@(xmtn-match resolved-id
                          ((local-tree $path)
                           ;; FIXME: I don't really know what to do if
                           ;; PATH is not the same as ROOT.  Maybe
                           ;; revision id resolution needs to return
                           ;; the proper root, too.
                           (assert (xmtn--same-tree-p root path))
                           '())
                          ((revision $hash-id)
                           `(,hash-id)))))
       (assert (equal (funcall parser) '(("format_version" (string "1")))))
       (let ((new-manifest-hash-id (xmtn-match (funcall parser)
                                     ((("new_manifest" (id $hash-id)))
                                      hash-id))))
         (let ((proto-revision (xmtn--parse-partial-revision parser)))
           (setf (xmtn--revision-new-manifest-hash-id proto-revision)
                 new-manifest-hash-id)
           proto-revision))))))

(defun xmtn--parse-partial-revision (parser)
  "Parses the basic_io output from get_revision, starting with the
old_revision stanzas."
  (let ((old-revision-hash-ids (list))
        (delete (list))
        (rename (list))
        (add-dir (list))
        (add-file (list))
        (patch-file (list))
        (clear-attr (list))
        (set-attr (list)))
    (flet ((decode-path (path)
             (decode-coding-string path 'xmtn--monotone-normal-form)))
      (loop for stanza = (funcall parser)
            while stanza
            do
            (xmtn-match stanza
              ;; Most common case, "patch", first.
              ((("patch" (string $filename))
                ("from" (id $from-id))
                ("to" (id $to-id)))
               (push `(,(decode-path filename) ,from-id ,to-id)
                     patch-file))
              ((("old_revision" (null-id)))
               ;; Why doesn't mtn just skip this stanza?
               )
              ((("old_revision" (id $hash-id)))
               (push hash-id old-revision-hash-ids))
              ((("delete" (string $path)))
               (push `(,(decode-path path)) delete))
              ((("rename" (string $from-path))
                ("to" (string $to-path)))
               (push `(,(decode-path from-path) ,(decode-path to-path))
                     rename))
              ((("add_dir" (string $path)))
               (push `(,(decode-path path)) add-dir))
              ((("add_file" (string $path))
                ("content" (id $file-id)))
               (push `(,(decode-path path) ,file-id)
                     add-file))
              ;; "patch": See above.
              ((("clear" (string $path))
                ("attr" (string $attr-name)))
               (push `(,(decode-path path) ,attr-name)
                     clear-attr))
              ((("set" (string $path))
                ("attr" (string $attr-name))
                ("value" (string $attr-value)))
               (push `(,(decode-path path) ,attr-name ,attr-value)
                     set-attr)))))
    (setq old-revision-hash-ids (nreverse old-revision-hash-ids)
          delete (nreverse delete)
          rename (nreverse rename)
          add-dir (nreverse add-dir)
          add-file (nreverse add-file)
          patch-file (nreverse patch-file)
          clear-attr (nreverse clear-attr)
          set-attr (nreverse set-attr))
    (xmtn--make-revision
     :old-revision-hash-ids old-revision-hash-ids
     :delete delete
     :rename rename
     :add-dir add-dir
     :add-file add-file
     :patch-file patch-file
     :clear-attr clear-attr
     :set-attr set-attr
     )))


;;;###autoload
(defun xmtn-dvc-file-diff (&rest args)
  ;; There is a reasonable default implementation to fall back on.
  (apply #'dvc-dvc-file-diff args))

;;;###autoload
(defun xmtn-dvc-revision-nth-ancestor (&rest args)
  ;; There is a reasonable default implementation to fall back on.  It
  ;; will just call `xmtn-dvc-revision-direct-ancestor' N times.  We
  ;; can't do any better than linear-time anyway, since we have to
  ;; chase the ancestry links (and check the uniqueness at each step).
  (apply #'dvc-dvc-revision-nth-ancestor args))

(provide 'xmtn-dvc)

;;; xmtn-dvc.el ends here