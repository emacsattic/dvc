;;; xgit-dvc.el --- The dvc layer for git

;; Copyright (C) 2006-2007 by all contributors

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

;; This file provides the common dvc layer for git


;;; History:

;;

;;; Code:

(require 'cg)
(eval-and-compile (require 'dvc-unified))

;;;###autoload
(dvc-register-dvc 'xgit "git")

;;;###autoload
(defalias 'xgit-dvc-tree-root 'xgit-tree-root)

;;;###autoload
(defun xgit-dvc-status (&optional path)
  (xgit-status))

(defalias 'xgit-dvc-add-files 'xgit-add-files)
(defalias 'xgit-dvc-revert-files 'xgit-revert-files)

;;;###autoload
(defalias 'xgit-dvc-command-version 'xgit-command-version)

(defalias 'xgit-dvc-diff 'xgit-diff)

(defun xgit-dvc-log-edit-file-name-func ()
  xgit-log-edit-file-name)

(defun xgit-dvc-log-edit ()
  (dvc-dvc-log-edit))

(defun xgit-dvc-log-edit-done ()
  "Finish a commit for git."
  (let ((buffer (find-file-noselect (dvc-log-edit-file-name)))
        (files-to-commit (with-current-buffer dvc-partner-buffer (dvc-current-file-list 'nil-if-none-marked))))
    (dvc-log-flush-commit-file-list)
    (save-buffer buffer)
    (message "committing %S in %s" (or files-to-commit "all files") (dvc-tree-root))
    (dvc-run-dvc-sync
     ;; cg 0.17 supports the -M command line switch for commit
     'cg (append (list "commit"
                       (unless (xgit-tree-has-head) "-C") ;; specifiy -C for the initial commit
                       "-M" (dvc-log-edit-file-name))
                 files-to-commit)
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (dvc-show-error-buffer output 'commit)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert (with-current-buffer error
                             (buffer-string))))
                 (dvc-log-close (capture buffer))
                 ;; doesn't work at the moment (Stefan, 10.02.2006)
                 ;; (dvc-diff-clear-buffers 'cg (capture default-directory)
                 ;;  "* Just committed! Please refresh buffer\n")
                 (message "git commit finished")))
    (dvc-tips-popup-maybe)))

;;TODO: Use the dvc log system
(defun xgit-dvc-log (arg)
  "Shows the changelog in the current git tree.
ARG is passed as prefix argument"
  (call-interactively 'xgit-log))

(defun xgit-dvc-changelog (arg)
  "Shows the changelog in the current git tree.
ARG is passed as prefix argument"
  (call-interactively 'xgit-log))

(provide 'xgit-dvc)
;;; xgit-dvc.el ends here
