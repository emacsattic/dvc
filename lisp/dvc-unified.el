;;; dvc-unified.el --- The unification layer for dvc

;; Copyright (C) 2005-2007 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>

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

;; This file provides the functionality that unifies the various dvc layers


;;; History:

;;

;;; Code:

(require 'dvc-register)
(require 'dvc-core)
(require 'dvc-defs)
(require 'dvc-tips)

;; --------------------------------------------------------------------------------
;; unified functions
;; --------------------------------------------------------------------------------

;;;###autoload
(defun dvc-add-files (&rest files)
  "Add FILES to the currently active dvc. FILES is a list of
strings including path from root; interactive defaults
to (dvc-current-file-list)."
  (interactive (dvc-current-file-list))
  (if dvc-confirm-add
      (let* ((dvc (dvc-current-active-dvc))
             (multiprompt (format "Add %%d files to %s? " dvc))
             (singleprompt (format "Add file to %s: " dvc)))
        (when (setq files (dvc-confirm-read-file-name-list multiprompt files
                                                           singleprompt t))
          (dvc-apply "dvc-add-files" files)))
    (dvc-apply "dvc-add-files" files)))

;;;###autoload
(defun dvc-revert-files (&rest files)
  "Revert FILES for the currently active dvc."
  (interactive (dvc-current-file-list))
  (let* ((dvc (dvc-current-active-dvc))
         (multiprompt (format "Revert %%d files to their stored version in %s? "
                              dvc))
         (singleprompt (format "Revert file to its state in %s: " dvc)))
    (when (setq files (dvc-confirm-read-file-name-list multiprompt files
                                                       singleprompt nil))
      (dvc-apply "dvc-revert-files" files))))

;;;###autoload
(defun dvc-remove-files (&rest files)
  "Remove FILES for the currently active dvc."
  (interactive (dvc-current-file-list))
  (let* ((dvc (dvc-current-active-dvc))
         (multiprompt (format "Remove %%d files from %s control? " dvc))
         (singleprompt (format "Remove file from %s: " dvc)))
    (when (setq files (dvc-confirm-read-file-name-list multiprompt files
                                                       singleprompt nil))
      (dvc-apply "dvc-remove-files" files))))

;;;###autoload
(defmacro define-dvc-unified-command (name args comment &optional interactive)
  `(defun ,name ,args
     ,comment
     ,@(when interactive (list interactive))
     (dvc-call ,(symbol-name name) ,@(remove '&optional args))))

;;;###autoload
(defun dvc-diff (&optional base-rev path dont-switch)
  "Display the changes from BASE-REV to the local tree in PATH.
BASE-REV (a revision-id) defaults to base revision of the
tree. Use `dvc-delta' for differencing two revisions.
PATH defaults to `default-directory'.
The new buffer is always displayed; if DONT-SWITCH is nil, select it."
  ;; FIXME: this should _only_ diff
  ;; working tree against its base revision; dvc-delta handles other diffs.
  (interactive (list nil default-directory current-prefix-arg))
  (setq base-rev (or base-rev
                     ;; allow back-ends to override this for e.g. git,
                     ;; which can return either the index or the last
                     ;; revision.
                     (dvc-call "dvc-last-revision" path)))
  (dvc-call "dvc-diff" base-rev path dont-switch))

(defun dvc-dvc-last-revision (path)
  (list (dvc-current-active-dvc)
        (list 'last-revision path 1)))

;;;###autoload
(define-dvc-unified-command dvc-delta (base modified &optional dont-switch)
  "Display from revision BASE to MODIFIED.

BASE and MODIFIED must be revision ID.

The new buffer is always displayed; if DONT-SWITCH is nil, select it.")

;;;###autoload
(define-dvc-unified-command dvc-file-diff (file &optional base modified
                                                dont-switch)
  "Display the changes in FILE (default current buffer file) for
the actual dvc."
  ;; FIXME: other operations default to (dvc-current-file-list); this
  ;; should default to (dvc-get-file-info-at-point)
  (interactive (list buffer-file-name)))

;;;###autoload
(defun dvc-status (&optional path)
  "Display the status in optional PATH tree."
  (interactive)
  (let* ((path (when path (expand-file-name path)))
         (default-directory (or path default-directory)))
    ;; this should be done in back-ends, so that
    ;; M-x <back-end>-status RET also prompts for save.
    ;; We keep it here as a safety belt, in case the back-end forgets
    ;; to do it.
    (dvc-save-some-buffers path)
    (dvc-call "dvc-status" path)))

(define-dvc-unified-command dvc-name-construct (back-end-revision)
  "Returns a string representation of BACK-END-REVISION.")

;;;###autoload
(defun dvc-log (&optional path last-n)
  "Display the log for PATH (default entire tree), LAST-N
entries (default `dvc-log-last-n'; all if nil)."
  (interactive)
  (dvc-call "dvc-log" path (if last-n last-n dvc-log-last-n)))

;;;###autoload
(define-dvc-unified-command dvc-changelog (&optional arg)
  "Display the changelog in this tree for the actual dvc."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-add (file)
  "Adds FILE to the repository."
  (interactive))

(define-dvc-unified-command dvc-revision-direct-ancestor (revision)
  "Computes the direct ancestor of a revision.")

(define-dvc-unified-command dvc-revision-nth-ancestor (revision n)
  "Computes the direct ancestor of a revision.")

(define-dvc-unified-command dvc-resolved (file)
  "Mark FILE as resolved"
  (interactive (list (buffer-file-name))))

(define-dvc-unified-command dvc-rename ()
  "Rename file from-file-name to to-file-name."
  (interactive))

(defvar dvc-command-version nil)
;;;###autoload
(defun dvc-command-version ()
  "Returns and/or shows the version identity string of backend command."
  (interactive)
  (setq dvc-command-version (dvc-call "dvc-command-version"))
  (when (interactive-p)
    (message "%s" dvc-command-version))
  dvc-command-version)


;;;###autoload
(defun dvc-tree-root (&optional path no-error)
  "Get the tree root for PATH or the current `default-directory'.

When called interactively, print a message including the tree root and
the current active back-end."
  (interactive)
  ;; FIXME: this ignores dvc-buffer-current-active-dvc and
  ;; dvc-temp-current-active-dvc; should use dvc-current-active-dvc
  ;; (or parts of it). This only matters when one directory is under
  ;; more than one CM tool, and they have different roots.
  (let ((dvc-list (append dvc-select-priority dvc-registered-backends))
        (root "/")
        (dvc)
        (tree-root-func)
        (path (or path default-directory)))
    (while dvc-list
      (setq tree-root-func (dvc-function (car dvc-list) "tree-root" t))
      (when (fboundp tree-root-func)
        (let ((current-root (funcall tree-root-func path t)))
          (when (and current-root (> (length current-root) (length root)))
            (setq root current-root)
            (setq dvc (car dvc-list)))))
      (setq dvc-list (cdr dvc-list)))
    (when (string= root "/")
      (unless no-error (error "Tree %s is not under version control"
                              path))
      (setq root nil))
    (when (interactive-p)
      (message "Root: %s (managed by %s)"
               root (dvc-variable dvc "backend-name")))
    root))

;;;###autoload
(defun dvc-log-edit (&optional other-frame no-init)
  "Edit the log before commiting. Optional OTHER_FRAME (default
user prefix) puts log edit buffer in a separate frame. Optional
NO-INIT if non-nil suppresses initialization of buffer if one is
reused."
  (interactive "P")
  ;; Reuse an existing log-edit buffer if possible.
  ;;
  ;; If this is invoked from a status or diff buffer,
  ;; dvc-buffer-current-active-dvc is set. If invoked from another
  ;; buffer (ie a source file, either directly or via
  ;; dvc-add-log-entry), dvc-buffer-current-active-dvc is nil, there
  ;; might be two back-ends to choose from, and dvc-current-active-dvc
  ;; might prompt. So we look for an existing log-edit buffer for the
  ;; current tree first, and assume the user wants the back-end
  ;; associated with that buffer (ie, it was the result of a previous
  ;; prompt).
  (let ((log-edit-buffers (dvc-get-matching-buffers dvc-buffer-current-active-dvc 'log-edit default-directory)))
    (case (length log-edit-buffers)
      (0 ;; Need to create a new log-edit buffer
         (dvc-call "dvc-log-edit" other-frame nil))

      (1 ;; Just reuse the buffer. Switch to it first so
         ;; dvc-buffer-current-active-dvc is set.
       (set-buffer (nth 1 (car log-edit-buffers)))
       (dvc-call "dvc-log-edit" other-frame no-init))

      (t ;; multiple matching buffers
       (if dvc-buffer-current-active-dvc
           (error "More than one log-edit buffer for %s in %s; can't tell which to use. Please close some."
              dvc-buffer-current-active-dvc default-directory)
         (error "More than one log-edit buffer for %s; can't tell which to use. Please close some."
                default-directory))))))

(defvar dvc-back-end-wrappers
  '(("log-edit" (&optional OTHER-FRAME))
    ("add-log-entry" ())
    ("add-files" (&rest files))
    ("revert-files" (&rest files))
    ("remove-files" (&rest files))
    ("ignore-file-extensions" (file-list))
    ("ignore-file-extensions-in-dir" (file-list)))
  "Alist of descriptions of back-end wrappers to define.

A back-end wrapper is a fuction called <back-end>-<something>, whose
body is a simple wrapper around dvc-<something>. This is usefull for
functions which are totally generic, but will use some back-end
specific stuff in their body.

At this point in the file, we don't have the list of back-ends, which
is why we don't do the (defun ...) here, but leave a description for
use by `dvc-register-dvc'.")

;;;###autoload
(define-dvc-unified-command dvc-log-edit-done (&optional arg)
  "Commit and close the log buffer."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-edit-ignore-files ()
  "Edit the ignored file list."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-ignore-files (file-list)
  "Ignore the marked files."
  (interactive (list (dvc-current-file-list))))

;;;###autoload
(defun dvc-ignore-file-extensions (file-list)
  "Ignore the file extensions of the marked files, in all
directories of the workspace."
  (interactive (list (dvc-current-file-list)))
  (let* ((extensions (delete nil (mapcar 'file-name-extension file-list)))
         ;; FIXME: should also filter duplicates. use delete-duplicates
         (root (dvc-tree-root))
         (msg (case (length extensions)
                (1 (format "extension *.%s" (first extensions)))
                (t (format "%d extensions" (length extensions))))))
    (if extensions
        (when (y-or-n-p (format "Ignore %s in workspace %s? " msg root))
          (dvc-call "dvc-backend-ignore-file-extensions" extensions))
      (error "No files with an extension selected"))))

;;;###autoload
(defun dvc-ignore-file-extensions-in-dir (file-list)
  "Ignore the file extensions of the marked files, only in the
directories containing the files, and recursively below them."
  (interactive (list (dvc-current-file-list)))
  ;; We have to match the extensions to the directories, so reject
  ;; command if either is nil.
  (let* ((extensions (mapcar 'file-name-extension file-list))
         (dirs (mapcar 'file-name-directory file-list))
         (msg (case (length extensions)
                (1 (format "extension *.%s in directory `%s'" (first extensions) (first dirs)))
                (t (format "%d extensions in directories" (length extensions))))))
    (dolist (extension extensions)
      (if (not extension)
          (error "A file with no extension selected")))
    (dolist (dir dirs)
      (if (not dir)
          (error "A file with no directory selected")))
    (when (y-or-n-p (format "Ignore %s? " msg))
          (dvc-call "dvc-backend-ignore-file-extensions-in-dir" file-list))))

;;;###autoload
(define-dvc-unified-command dvc-missing (&optional other)
  "Show the missing changesets for this working copy in regard to other."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-inventory ()
  "Show the inventory for this working copy."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-save-diff (file)
  "Store the diff from the working copy against the repository in a file."
  (interactive (list (read-file-name "Save the diff to: "))))

;;;###autoload
(define-dvc-unified-command dvc-update ()
  "Update this working copy."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-pull ()
  "Pull changes from the remote source to the working copy or
local database, as appropriate for the current back-end."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-merge (&optional other)
  "Merge with OTHER.
If OTHER is nil, merge heads in current database.
If OTHER is a string, it identifies a (local or remote)
database to merge into the current database or workspace."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-submit-patch ()
  "Submit a patch for the current project under DVC control."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-send-commit-notification ()
  "Send a commit notification for the changeset at point."
  (interactive))

(provide 'dvc-unified)

;;; dvc-unified.el ends here
