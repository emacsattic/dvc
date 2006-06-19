;;; bzr.el --- Support for Bazaar 2 in DVC

;; Copyright (C) 2005-2006 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;

;;; Code:

(require 'bzr-core)
(require 'dvc-diff)
(require 'dvc-core)
(require 'dvc-defs)
(require 'dvc-revlist)
(eval-and-compile (require 'dvc-lisp))

(eval-when-compile (require 'cl))

(defvar bzr-default-init-repository-directory "~/"
  "The default directory that is suggested when calling `bzr-init-repository'.
This setting is useful, if you'd like to create a bunch of repositories in
a common base directory.")

(defvar bzr-command-version nil
  "Version of bzr that we are using.")

;;example:
;;(setq bzr-mail-notification-destination
;;      '(("dvc-dev-bzr" ("[commit][dvc] " "dvc-dev@gna.org"))))
(defcustom bzr-mail-notification-destination nil
"*Preset some useful values for commit emails.

An alist of rules to map branch names to target
email addresses and the prefix string for the subject line.

This is used by the `bzr-send-commit-notification' function."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Bzr branch")
                (list :tag "Target"
                      (string :tag "Email subject prefix")
                      (string :tag "Email address"))))
  :group 'dvc)


(defun bzr-init (&optional dir)
  "Run bzr init."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for bzr init: "
                                                     (or default-directory
                                                         (getenv "HOME"))))))
  (dvc-run-dvc-sync 'bzr (list "init" dir)
                     :finished (dvc-capturing-lambda
                                   (output error status arguments)
                                 (message (format "bzr init %s finished" dir)))))

(defun bzr-init-repository (&optional dir)
  "Run bzr init-repository.
When called interactively, `bzr-default-init-repository-directory' is used as
starting point to enter the new repository directory. That directory is created
via bzr init-repository."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for bzr init-repository: "
                                                     (or
                                                      bzr-default-init-repository-directory
                                                      default-directory
                                                      (getenv "HOME"))))))
  (dvc-run-dvc-sync 'bzr (list "init-repository" dir)
                     :finished (dvc-capturing-lambda
                                   (output error status arguments)
                                 (message (format "bzr init-repository '%s' finished" dir))))
  dir)

(defun bzr-checkout (branch-location to-location &optional lightweight revision)
  "Run bzr checkout."
  (interactive
   (list (read-string "bzr checkout branch location: ")
         (expand-file-name (dvc-read-directory-name "bzr checkout to: "
                                                    (or default-directory
                                                        (getenv "HOME"))))
         (y-or-n-p "Do a lightweight checkout? ")
         nil))
  (dvc-run-dvc-sync 'bzr (list "checkout"
                               (when lightweight "--lightweight")
                               branch-location to-location)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message (format "bzr checkout%s %s -> %s finished"
                                                 (if lightweight " --lightweight" "")
                                                 branch-location to-location))
                                (dired to-location))))

(defun bzr-update ()
  "Run bzr update."
  (interactive)
  (dvc-run-dvc-async 'bzr (list "update")
                     :finished
                     (dvc-capturing-lambda
                         (output error status arguments)
                       (message (format "bzr update finished => %s"
                                        (concat (dvc-buffer-content error) (dvc-buffer-content output)))))))
  

;; bzr-start-project implements the following idea:
;;  bzr init-repo repo
;;  bzr init repo/trunk
;;  bzr checkout --lightweight repo/trunk trunk-checkout
;;  cd trunk-checkout
;;  (add files here)
(defun bzr-start-project ()
  "Initializes a repository with a trunk branch and finally checks out a working copy.
The following functions are called:
`bzr-init-repository': create a shared repository
`bzr-init':            create the trunk branch in the repository above
`bzr-checkout':        check out the trunk branch to the entered working directory"
  (interactive)
  (let ((init-repo-dir)
        (branch-repo-dir)
        (checkout-dir))
    (setq init-repo-dir (call-interactively 'bzr-init-repository))
    (setq branch-repo-dir (dvc-uniquify-file-name (concat init-repo-dir "/trunk")))
    (bzr-init branch-repo-dir)
    (setq checkout-dir (dvc-uniquify-file-name (dvc-read-directory-name "checkout the branch to: ")))
    (bzr-checkout branch-repo-dir checkout-dir t)))

(defun bzr-parse-diff (changes-buffer)
  (dvc-trace "bzr-parse-diff")
  (dvc-trace-current-line)
  (save-excursion
    (while (re-search-forward
            "^=== \\([a-z]*\\) file '\\([^']*\\)'\\( => '\\([^']*\\)'\\)?$" nil t)
      (let* ((origname (match-string-no-properties 2))
             (newname  (or (match-string-no-properties 4) origname))
             (renamed  (string= (match-string-no-properties 1) "renamed"))
             (added    (string= (match-string-no-properties 1) "added")))
        (with-current-buffer changes-buffer
          (ewoc-enter-last dvc-diff-cookie
                           (list 'file
                                 newname
                                 (cond (added   "A")
                                       (renamed "R")
                                       (t " "))
                                 (cond (added " ")
                                       (t "M"))
                                 " "    ; dir
                                 (when (and renamed
                                            (not added))
                                   origname))))))))

;;;###autoload
(defun bzr-diff (&optional against path dont-switch)
  "Run \"bzr diff\".

TODO: DONT-SWITCH and AGAINST are currently ignored."
  (interactive (list nil nil current-prefix-arg))
  (let* ((dir (or path default-directory))
         (root (bzr-tree-root dir))
         (buffer (dvc-prepare-changes-buffer
                  `(bzr (last-revision ,root 1))
                  `(bzr (local-tree ,root))
                  'diff root 'bzr)))
    (when dvc-switch-to-buffer-first
      (dvc-switch-to-buffer buffer))
    (dvc-save-some-buffers root)
    (dvc-run-dvc-async
     'bzr '("diff")
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (dvc-diff-no-changes (capture buffer)
                             "No changes in %s"
                             (capture root)))
     :error
     (dvc-capturing-lambda (output error status arguments)
       (if (/= 1 status)
           (dvc-diff-error-in-process (capture buffer)
                                       "Error in diff process"
                                       (capture root)
                                       output error)
         (dvc-show-changes-buffer output 'bzr-parse-diff
                                  (capture buffer)))))))

(defun bzr-delta (base modified dont-switch)
  "Run bzr diff -r BASE -r MODIFIED.

TODO: dont-switch is currently ignored."
  (dvc-trace "base, modified=%S, %S" base modified)
  (let ((base-str (bzr-revision-id-to-string base))
        (modified-str (bzr-revision-id-to-string modified))
        (buffer (dvc-prepare-changes-buffer
                 base modified
                 'diff nil 'bzr)))
    (when dvc-switch-to-buffer-first
      (dvc-switch-to-buffer buffer))
    (let ((default-directory (bzr-revision-id-location modified)))
      (dvc-run-dvc-async
       'bzr `("diff"
              "--revision" ,(concat base-str ".." modified-str))
       :finished
       (dvc-capturing-lambda (output error status arguments)
         (dvc-diff-no-changes (capture buffer)
                              "No changes between %s"
                              (concat (capture base-str) " and " (capture modified-str))))
       :error
       (dvc-capturing-lambda (output error status arguments)
         (if (/= 1 status)
             (dvc-diff-error-in-process (capture buffer)
                                        "Error in diff process"
                                        ""
                                        output error)
           (dvc-show-changes-buffer output 'bzr-parse-diff
                                    (capture buffer)))))
      ;; We must return the buffer (even in asynchronous mode)
      (with-current-buffer buffer (goto-char (point-min)))
      buffer)))

(defun bzr-send-commit-notification ()
  "Send a commit notification email for the changelog entry at point.

`bzr-mail-notification-destination' can be used to specify a prefix for
the subject line, the rest of the subject line contains the summary line
of the commit. Additionally the destination email address can be specified."
  (interactive)
  (let ((dest-specs (cadar bzr-mail-notification-destination));;(tla--name-match-from-list
                     ;;(tla--name-split (tla-changelog-revision-at-point))
                     ;;tla-mail-notification-destination))
        (rev "??");;(tla-changelog-revision-at-point))
        (summary "");;(tla-changelog-log-summary-at-point))
        (log-message ""));;(dvc-changelog-log-message-at-point)))
    (message "Preparing commit email for %s" rev)
    (compose-mail (if dest-specs (cadr dest-specs) "")
                  (if dest-specs (car dest-specs) ""))
    (message-goto-subject)
    (insert summary)
    (message-goto-body)
    (insert (concat "Committed " rev "\n\n"))
    (insert log-message)
    (message-goto-body)))


(defun bzr-unknowns ()
  "Run bzr unknowns."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("unknowns")))

(defun bzr-parse-status (changes-buffer)
  (dvc-trace "bzr-parse-status (while)")
  (while (> (point-max) (point))
    (dvc-trace-current-line)
    (cond ((looking-at "^\\([^ ][^\n]*:\\)")
           (let ((msg (match-string-no-properties 1)))
             (with-current-buffer changes-buffer
               (ewoc-enter-last dvc-diff-cookie
                                (list 'message msg)))))
          ((looking-at "^ +\\([^ ][^\n]*?\\)\\([/@]\\)? => \\([^\n]*?\\)\\([/@]\\)?$")
           (let ((oldname (match-string-no-properties 1))
                 (dir (match-string-no-properties 2))
                 (newname (match-string-no-properties 3)))
             (with-current-buffer changes-buffer
               (ewoc-enter-last dvc-diff-cookie
                                (list 'file newname
                                      " " " " dir
                                      oldname)))))
          ((looking-at " +\\([^\n]*?\\)\\([/@]\\)?$")
           (let ((file (match-string-no-properties 1))
                 (dir (match-string-no-properties 2)))
             (with-current-buffer changes-buffer
               (ewoc-enter-last dvc-diff-cookie
                                (list 'file file
                                      ;; TODO perhaps not only " ".
                                      " " " " dir nil)))))
          (t (error "unrecognized context in bzr-parse-status")))
    (forward-line 1)))

;;;###autoload
(defun bzr-status (&optional against path)
  "Run \"bzr status\"."
  (interactive (list default-directory))
  (let* ((dir (or path default-directory))
         (root (bzr-tree-root dir))
         (buffer (dvc-prepare-changes-buffer
                  `(bzr (last-revision ,root 1))
                  `(bzr (local-tree ,root))
                  'status root 'bzr)))
    (dvc-switch-to-buffer-maybe buffer)
    (setq dvc-buffer-refresh-function 'bzr-status)
    (dvc-save-some-buffers root)
    (dvc-run-dvc-async
     'bzr '("status")
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer (capture buffer)
         (if (> (point-max) (point-min))
             (dvc-show-changes-buffer output 'bzr-parse-status
                                      (capture buffer))
         (dvc-diff-no-changes (capture buffer)
                             "No changes in %s"
                             (capture root))))
       :error
       (dvc-capturing-lambda (output error status arguments)
         (dvc-diff-error-in-process (capture buffer)
                                     "Error in diff process"
                                     (capture root)
                                     output error))))))

;;;###autoload
(defun bzr-add (file)
  "Adds FILE to the repository."
  (interactive "fAdd file or directory: ")
  (message
   (let ((default-directory (bzr-tree-root)))
     (dvc-run-dvc-sync
      'bzr (list "add" (file-relative-name file))
      :finished 'dvc-output-and-error-buffer-handler))))

(defun bzr-add-files (&rest files)
  "Run bzr add."
  (message "bzr-add-files: %s" files)
  (let ((default-directory (bzr-tree-root)))
    (dvc-run-dvc-sync 'bzr (append '("add") (mapcar #'file-relative-name
                                                    files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "bzr add finished")))))

(defun bzr-log-edit-done ()
  "Finish a commit for Bzr."
  (interactive)
  (let ((buffer (find-file-noselect (dvc-log-edit-file-name))))
    (dvc-log-flush-commit-file-list)
    (save-buffer buffer)
    (let ((default-directory (dvc-uniquify-file-name default-directory)))
      (dvc-run-dvc-async
       'bzr
       (append
        (list "commit" "--verbose" "--file" (dvc-log-edit-file-name))
        ;;  Get marked  files to  do  a selected  file commit.  Nil
        ;; otherwise (which means commit all files).
        (with-current-buffer dvc-partner-buffer
          (mapcar #'dvc-uniquify-file-name
                  (dvc-current-file-list 'nil-if-none-marked))))
       :finished (dvc-capturing-lambda (output error status arguments)
                   (dvc-show-error-buffer output 'commit)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert (with-current-buffer error
                               (buffer-string))))
                   (dvc-log-close (capture buffer))
                   (dvc-diff-clear-buffers
                    'bzr
                    (capture default-directory)
                    "* Just committed! Please refresh buffer\n")
                   (message "Bzr commit finished !"))))
    (dvc-tips-popup-maybe)))

;; Revisions

(defun bzr-revision-id-location (rev-id)
  "Extract the location component from REVISION-ID."
  (case (dvc-revision-get-type rev-id)
    ((revision previous-revision)
     (let* ((data (car (dvc-revision-get-data rev-id)))
            (location (nth 1 data)))
       location))
    (otherwise nil)))

(defun bzr-revision-id-to-string (rev-id)
  "Turn a DVC revision ID to a bzr revision spec.

\(bzr (revision (local \"/path/to/archive\" 3)))
=> \"revno:3\".

TODO: This works only for local revision now, because bzr lacks a way
to do it in the general case."
  (case (dvc-revision-get-type rev-id)
    (revision (let* ((data (car (dvc-revision-get-data rev-id)))
                     (location (nth 0 data)))
                (unless (eq location 'local)
                  (error "TODO: Non local (%S) revisions not supported here."
                         location))
                (concat "revno:" (int-to-string (nth 2 data)))))
    (previous-revision
     (let* ((data (car (dvc-revision-get-data rev-id)))
            (location (nth 0 data)))
       (unless (eq location 'local)
         (error "TODO: Non local (%S) revisions not supported here."
                location))
         (concat "revno:" (int-to-string (- (nth 2 data) 1)))))
    (otherwise (error "TODO: not implemented: %S" rev-id))))


(defun bzr-revision-get-file-revision (file revision)
  "Insert the content of FILE in REVISION, in current buffer.

REVISION looks like
\(local \"path\" NUM)."
  (let ((bzr-rev
         (if (eq (car revision) 'local)
             (int-to-string (nth 2 revision))
           (error "TODO: revision=%S" revision)))
        (path (if (eq (car revision) 'local)
                  default-directory)))
    (let ((default-directory path))
      (insert
       (dvc-run-dvc-sync
        ;; TODO what if I'm not at the tree root ?
        'bzr (list "cat" "--revision" bzr-rev file)
        :finished 'dvc-output-buffer-handler)))))

;;;###autoload
(defun bzr-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)
"
  (let ((bzr-rev (concat "last:" (int-to-string
                                  (nth 1 last-revision))))
        (default-directory (car last-revision)))
    (insert
     (dvc-run-dvc-sync
      ;; TODO what if I'm not at the tree root ?
      'bzr (list "cat" "--revision" bzr-rev file)
      :finished 'dvc-output-buffer-handler))))

(defun bzr-command-version ()
  "Run bzr version."
  (interactive)
  (setq bzr-command-version
        (dvc-run-dvc-sync
         'bzr (list "version")
         :finished (lambda (output error status arguments)
                     (set-buffer output)
                     (goto-char (point-min))
                     (buffer-substring (point) (point-at-eol)))))
  (when (interactive-p)
    (message "Bazaar-NG Version: %s" bzr-command-version))
  bzr-command-version)

(defun bzr-whoami ()
  "Run bzr whomai."
  (interactive)
  (let ((whoami (dvc-run-dvc-sync 'bzr (list "whoami")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "bzr whoami: %s" whoami))
    whoami))

(defun bzr-info ()
  "Run bzr info."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("info")))

(defun bzr-check ()
  "Run bzr check."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("check") t))

(defun bzr-ignored ()
  "Run bzr ignored."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("ignored")))

(defun bzr-conflicts ()
  "Run bzr conflicts."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("conflicts")))

(defun bzr-deleted ()
  "Run bzr deleted."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("deleted")))

(defun bzr-renames ()
  "Run bzr renames."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("renames")))

(defun bzr-ignore (pattern)
  "Run bzr ignore PATTERN."
  (interactive "sbzr ignore: ")
  (dvc-run-dvc-sync 'bzr (list "ignore" pattern)))

(provide 'bzr)
;; arch-tag: Matthieu Moy, Sun Sep  4 23:27:53 2005 (bzr.el)
;;; bzr.el ends here
