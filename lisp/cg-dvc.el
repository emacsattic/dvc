;;; cg-dvc.el --- The dvc layer for cg

;; Copyright (C) 2006 by all contributors

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

;; This file provides the common dvc layer for cogito/git


;;; History:

;;

;;; Code:

(require 'cg)
(eval-and-compile (require 'dvc-unified))

;;;###autoload
(dvc-register-dvc 'cg "Cogito")

;;;###autoload
(defalias 'cg-dvc-tree-root 'cg-tree-root)

;;;###autoload
(defun cg-dvc-status (&optional path)
  (cg-status))

(defalias 'cg-dvc-add-files 'cg-add-files)
(defalias 'cg-dvc-revert-files 'cg-revert-files)

;;;###autoload
(defalias 'cg-dvc-command-version 'cg-command-version)

(defalias 'cg-dvc-diff 'cg-diff)

(defun cg-dvc-log-edit-file-name-func ()
  cg-log-edit-file-name)

(defun cg-dvc-log-edit ()
  (dvc-dvc-log-edit))

(defun cg-dvc-log-edit-done ()
  "Finish a commit for Cogito."
  (let ((buffer (find-file-noselect (dvc-log-edit-file-name)))
        (files-to-commit (with-current-buffer dvc-partner-buffer (dvc-current-file-list 'nil-if-none-marked))))
    (dvc-log-flush-commit-file-list)
    (save-buffer buffer)
    (message "committing %S in %s" (or files-to-commit "all files") (dvc-tree-root))
    (dvc-run-dvc-sync
     ;; cg 0.17 supports the -M command line switch for commit
     'cg (append (list "commit"
                       (unless (cg-tree-has-head) "-C") ;; specifiy -C for the initial commit
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
                 (message "Cogito commit finished")))
    (dvc-tips-popup-maybe)))

;;TODO: Use the dvc log system
(defun cg-dvc-log (arg)
  "Shows the changelog in the current git/cogito tree.
ARG is passed as prefix argument"
  (call-interactively 'cg-log))

(defun cg-dvc-changelog (arg)
  "Shows the changelog in the current git/cogito tree.
ARG is passed as prefix argument"
  (call-interactively 'cg-log))

(provide 'cg-dvc)
;;; cg-dvc.el ends here
