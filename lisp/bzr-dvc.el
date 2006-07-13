;;; bzr-dvc.el --- Support for Bazaar 2 in DVC's unification layer

;; Copyright (C) 2005 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords: tools

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

(eval-and-compile (require 'dvc-unified))
(require 'bzr)

;;;###autoload
(dvc-register-dvc 'bzr "Bazaar 2")
;;;###autoload
(defalias 'bzr-dvc-tree-root 'bzr-tree-root)
;;;###autoload
(defalias 'bzr-dvc-diff 'bzr-diff)
;;;###autoload
(defalias 'bzr-dvc-status 'bzr-status)
;;;###autoload
(defalias 'bzr-dvc-submit-patch 'bzr-submit-patch)
;;;###autoload
(defalias 'bzr-dvc-add 'bzr-add)

(defalias 'bzr-dvc-add-files 'bzr-add-files)

;;;###autoload
(defalias 'bzr-dvc-log-edit-done 'bzr-log-edit-done)

;;;###autoload
(defun bzr-dvc-search-file-in-diff (file)
  (re-search-forward (concat "^=== .* '" file "'$")))

;;;###autoload
(defun bzr-dvc-name-construct (back-end-revision)
  (nth 1 back-end-revision))

;;;###autoload
(defvar bzr-log-edit-file-name ".tmp-bzr-log-edit.txt"
  "The filename, used to store the log message before commiting.
Usually that file is placed in the tree-root of the working tree.")

(add-to-list 'auto-mode-alist `(,(concat "^" (regexp-quote bzr-log-edit-file-name)
                                         "$") . bzr-log-edit-mode))


;;;###autoload
(defalias 'bzr-dvc-command-version 'bzr-command-version)

(defalias 'bzr-dvc-log 'bzr-log)

(defalias 'bzr-dvc-changelog 'bzr-changelog)

(defun bzr-dvc-update ()
  (interactive)
  (bzr-update nil))

(defun bzr-dvc-edit-ignore-files ()
  (interactive)
  (find-file-other-window (concat (bzr-tree-root) ".bzrignore")))

(defun bzr-dvc-ignore-files (file-list)
  (interactive (list (dvc-current-file-list)))
  (when (y-or-n-p (format "Ignore %S for %s? " file-list (bzr-tree-root)))
    (dolist (f-name file-list)
      (bzr-ignore (format "./%s" f-name)))))

(defun bzr-dvc-ignore-file-extensions (file-list)
  (interactive (list (dvc-current-file-list)))
  (let* ((extension-list (delete nil (mapcar 'file-name-extension file-list)))
         (msg-list (mapconcat '(lambda (ext) (concat "*." ext)) extension-list " ")))
    (if extension-list
        (when (y-or-n-p (format "Ignore %s for %s? " msg-list (bzr-tree-root)))
          (dolist (ext-name extension-list)
            (bzr-ignore (format "*.%s" ext-name))))
      (message "No files with an extension selected."))))

(autoload 'bzr-revlog-get-revision "bzr-revlog")
(defalias 'bzr-dvc-revlog-get-revision
  'bzr-revlog-get-revision)

(defalias 'bzr-dvc-delta 'bzr-delta)

(defalias 'bzr-dvc-send-commit-notification 'bzr-send-commit-notification)

(defalias 'bzr-dvc-prepare-environment 'bzr-prepare-environment)

(provide 'bzr-dvc)
;; arch-tag: 39d450b3-ffb1-4911-bb27-4d483a6abb8f
;;; bzr-dvc.el ends here
