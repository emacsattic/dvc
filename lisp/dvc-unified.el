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
  "Add FILES to the currently active dvc."
  (interactive (dvc-current-file-list))
  (let* ((dvc (dvc-current-active-dvc))
         (multiprompt (format "Add %%d files to %s? " dvc))
         (singleprompt (format "Add file to %s: " dvc)))
    (when (setq files (dvc-confirm-read-file-name-list multiprompt files
                                                       singleprompt t))
      (apply 'dvc-apply "dvc-add-files" files))))

;;;###autoload
(defun dvc-revert-files (&rest files)
  "Revert FILES for the currently active dvc."
  (interactive (dvc-current-file-list))
  (let* ((dvc (dvc-current-active-dvc))
         (multiprompt (format "Revert %%d files to their stored version in %s? " dvc))
         (singleprompt (format "Revert file to its state in %s: " dvc)))
    (when (setq files (dvc-confirm-read-file-name-list multiprompt files
                                                       singleprompt nil))
      (apply 'dvc-apply "dvc-revert-files" files))))

;;;###autoload
(defun dvc-remove-files (&rest files)
  "Remove FILES for the currently active dvc."
  (interactive (dvc-current-file-list))
  (let* ((dvc (dvc-current-active-dvc))
         (multiprompt (format "Remove %%d files from %s control? " dvc))
         (singleprompt (format "Remove file from %s: " dvc)))
    (when (setq files (dvc-confirm-read-file-name-list multiprompt files
                                                       singleprompt nil))
      (apply 'dvc-apply "dvc-remove-files" files))))

;;;###autoload
(progn
  (defmacro dvc-create-unified-command (name args comment &optional interactive)
    `(defun ,(intern (concat "dvc-" name)) ,args
       ,comment
       ,@(when interactive (list interactive))
       (dvc-apply ,(concat "dvc-" name) ,@(remove '&optional args))
       )))

(put 'dvc-create-unified-command
     'lisp-indent-function 'defun)

;;;###autoload
(dvc-create-unified-command "diff" (&optional against path dont-switch)
  "Display the changes in this tree for the actual dvc."
  (interactive (list nil nil current-prefix-arg)))

;;;###autoload
(dvc-create-unified-command "delta" (&optional base modified dont-switch)
  "Display from revision BASE to MODIFIED.

BASE and MODIFIED must be revision ID.

If DONT-SWITCH is nil, switch to the newly created buffer.")

;;;###autoload
(dvc-create-unified-command "file-diff" (file &optional base modified dont-switch)
  "Display the changes in FILE for the actual dvc."
  (interactive (list buffer-file-name)))

;;;###autoload
(defun dvc-status (&optional against path)
  "Display the status in optional PATH tree."
  (interactive)
  (if path
      (let ((default-directory path))
        (dvc-apply "dvc-status" against path))
    (dvc-apply "dvc-status" against nil)))

(dvc-create-unified-command "name-construct" (back-end-revision)
  "Returns a string representation of BACK-END-REVISION.")

;;;###autoload
(dvc-create-unified-command "log" (&optional arg)
  "Display the log in this tree for the actual dvc."
  (interactive))

;;;###autoload
(dvc-create-unified-command "changelog" (&optional arg)
  "Display the changelog in this tree for the actual dvc."
  (interactive))

;;;###autoload
(dvc-create-unified-command "add" (file)
  "Adds FILE to the repository."
  (interactive))

(dvc-create-unified-command "revision-direct-ancestor" (revision)
  "Computes the direct ancestor of a revision.")

(dvc-create-unified-command "revision-nth-ancestor" (revision n)
  "Computes the direct ancestor of a revision.")

(dvc-create-unified-command "resolved" (file)
  "Mark FILE as resolved"
  (interactive (list (buffer-file-name))))

(dvc-create-unified-command "rename" ()
  "Rename.file from-file-name to to-file-name."
  (interactive))

(defvar dvc-command-version nil)
;;;###autoload
(defun dvc-command-version ()
  (interactive)
  "Returns and/or shows the version identity string of backend command."
  (setq dvc-command-version (dvc-apply "dvc-command-version"))
  (when (interactive-p)
    (message "%s" dvc-command-version))
  dvc-command-version)


;;;###autoload
(defun dvc-tree-root (&optional path no-error)
  "Get the tree root for PATH or the current `default-directory'.

When called interactively, print a message including the tree root and
the current active back-end."
  (interactive)
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
      (unless no-error (error "Tree %s is not version controled"
                              path))
      (setq root nil))
    (when (interactive-p)
      (message "Root: %s (managed by %s)" root (dvc-variable dvc "backend-name")))
    root))

;;;###autoload
(dvc-create-unified-command "log-edit" ()
  "Edit the log before commiting."
  (interactive))

;;;###autoload
(dvc-create-unified-command "log-edit-done" ()
  "Commit and close the log buffer."
  (interactive))

;;;###autoload
(dvc-create-unified-command "edit-ignore-files" ()
  "Edit the ignored file list."
  (interactive))

;;;###autoload
(dvc-create-unified-command "ignore-files" (file-list)
  "Ignore the marked files."
  (interactive (list (dvc-current-file-list))))

;;;###autoload
(dvc-create-unified-command "ignore-file-extensions" (file-list)
  "Ignore the file extensions of the marked files."
  (interactive (list (dvc-current-file-list))))

;;;###autoload
(dvc-create-unified-command "missing" ()
  "Show the missing changesets for this working copy."
  (interactive))

;;;###autoload
(dvc-create-unified-command "inventory" ()
  "Show the inventory for this working copy."
  (interactive))

;;###autoload
(dvc-create-unified-command "save-diff" (file)
  "Store the diff from the working copy against the repository in a file."
  (interactive (list (read-file-name "Save the diff to: "))))

;;;###autoload
(dvc-create-unified-command "update" ()
  "Update this working copy."
  (interactive))

;;;###autoload
(dvc-create-unified-command "pull" ()
  "Pull changes from the remote source to the working copy or
local database, as appropriate for the current back-end."
  (interactive))

;;;###autoload
(dvc-create-unified-command "submit-patch" ()
  "Submit a patch for the current project under DVC control."
  (interactive))

;;;###autoload
(dvc-create-unified-command "send-commit-notification" ()
  "Send a commit notification for the changeset at point."
  (interactive))

(provide 'dvc-unified)
;;; dvc-unified.el ends here
