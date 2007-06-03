;;; xgit.el --- git interface for dvc

;; Copyright (C) 2006-2007 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Takuzo O'hara <takuzo.ohara@gmail.com>

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

;; The git interface for dvc

;;; History:

;;

;;; Code:

(require 'dvc-core)
(require 'xgit-core)
(require 'xgit-log)

;; There must be something like these in standard emacs distributions
;; but I couldn't find them.
;; I don't know where to put these things so I put it here temporarily.
;; -- takuzo
(defun any-to-string (arg)
  "Tries to convert anything passed to args to a string.
Args can be list, number or string."
  (cond ((null arg)
	  "")
	((listp arg)
	 (reduce (lambda (s1 s2) (format "%s %s" s1 s2)) arg))
	((numberp arg)
	 (number-to-string arg))
	((stringp arg)
	 arg)))

(defun xgit-init (&optional dir)
  "Run git init."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for git init: "
                                                     (or default-directory
                                                         (getenv "HOME"))))))
  (let ((default-directory (or dir default-directory)))
    (dvc-run-dvc-sync 'xgit (list "init-db")
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "git init finished")))))

(defun xgit-add-files (&rest files)
  "Run git add."
  (message "xgit-add-files: %s" files)
  (let ((default-directory (xgit-tree-root)))
    (dvc-run-dvc-sync 'xgit (append '("add") (mapcar #'file-relative-name files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "git add finished")))))

(defun xgit-remove-files (&rest files)
  "Run git rm."
  (message "xgit-remove-files: %s" files)
  (dvc-run-dvc-sync 'xgit (append '("rm") (mapcar #'file-relative-name files))
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "git rm finished"))))

(defun xgit-command-version ()
  "Run git version."
  (interactive)
  (let ((version (dvc-run-dvc-sync 'xgit (list "version")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Git Version: %s" version))
    version))

;; TODO: update for git
(defun xgit-parse-status  (changes-buffer)
  (dvc-trace "xgit-parse-status (dolist)")
  (let ((status-list
         (split-string (dvc-buffer-content output) "\n")))
    (with-current-buffer changes-buffer
      (setq dvc-header (format "git status -w for %s\n" default-directory))
      (let ((buffer-read-only)
            status modif modif-char)
        (dolist (elem status-list)
          (unless (string= "" elem)
            (setq modif-char (aref elem 0))
            (cond ((eq modif-char ?M)
                   (setq status "M"
                         modif "M"))
                  ((eq modif-char ?A)
                   (setq status "A"))
                  ((eq modif-char ?R)
                   (setq status "R"))
                  ((eq modif-char ?!)
                   (setq status "D"))
                  ((eq modif-char ??)
                   (setq status "?"))
                  (t
                   (setq modif nil
                         status nil)))
            (when (or modif status)
              (ewoc-enter-last dvc-diff-cookie
                               (list 'file
                                     (substring elem 2)
                                     status
                                     modif)))))))))

;; TODO: update for git
(defun xgit-status (&optional against path)
  "Run git status."
  (interactive (list nil default-directory))
  (let* ((dir (or path default-directory))
         (root (xgit-tree-root dir))
         (buffer (dvc-prepare-changes-buffer
                  `(git (last-revision ,root 1))
                  `(git (local-tree ,root))
                  'status root 'xgit)))
    (dvc-switch-to-buffer-maybe buffer)
    (setq dvc-buffer-refresh-function 'xgit-status)
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync
     'xgit '("status")
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer (capture buffer)
         (if (> (point-max) (point-min))
             (dvc-show-changes-buffer output 'xgit-parse-status
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

;; TODO: update for git
(defun xgit-log ()
  "Run git log."
  (interactive)
  (let ((buffer (dvc-get-buffer-create 'xgit 'log)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (dvc-run-dvc-sync 'xgit '("log")
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (progn
                          (with-current-buffer (capture buffer)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert-buffer-substring output)
                              (goto-char (point-min))
                              (insert (format "git log for %s\n\n" default-directory))
                              (xgit-log-mode))))))))

;; TODO: update for git
;; copied from xhg-parse-diff: not yet fully working
(defun xgit-parse-diff (changes-buffer)
  (save-excursion
    (while (re-search-forward
            "^diff --git [^ ]+ b/\\(.*\\)$" nil t)
      (let* ((name (match-string-no-properties 1))
              ;; added, removed are not yet working
             (added (progn (forward-line 1)
                           (looking-at "^--- /dev/null")))
             (removed (progn (forward-line 1)
                             (looking-at "^\\+\\+\\+ /dev/null"))))
        (with-current-buffer changes-buffer
          (ewoc-enter-last dvc-diff-cookie
                           (list 'file
                                 name
                                 (cond (added   "A")
                                       (removed "D")
                                       (t " "))
                                 (cond ((or added removed) " ")
                                       (t "M"))
                                 " " ; dir. Nothing is a directory in hg.
                                 nil)))))))

;; TODO: update for git
(defun xgit-diff (&optional against path dont-switch)
    (interactive (list nil nil current-prefix-arg))
  (let* ((cur-dir (or path default-directory))
         (orig-buffer (current-buffer))
         (root (xgit-tree-root cur-dir))
         (buffer (dvc-prepare-changes-buffer
                  `(git (last-revision ,root 1))
                  `(git (local-tree ,root))
                  'diff root 'xgit))
         (command-list '("diff")))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (when dont-switch (pop-to-buffer orig-buffer))
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync 'xgit command-list
                       :finished
                       (dvc-capturing-lambda (output error status arguments)
                         (dvc-show-changes-buffer output 'xgit-parse-diff
                                                  (capture buffer))))))

;; TODO: update for git
(defun xgit-restore (force &rest files)
  "Run git restore

xgit-restore
 -r REVISION: Not supported yet
 -f: FORCE"
  (message "xgit-restore: %s" files)
  (let ((args (cons "restore"
                    (if force '("-f") '()))))
    (dvc-run-dvc-sync 'xgit (append args files)
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "git restore finished")))))
;; TODO: update for git
(defun xgit-revert-files (&rest files)
  "See `xgit-restore'"
  (apply 'xgit-restore t files))

;; --------------------------------------------------------------------------------
;; dvc revision support
;; --------------------------------------------------------------------------------
;; TODO: update for git
;;;###autoload
(defun xgit-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)"
  (dvc-trace "xgit-revision-get-last-revision file:%S last-revision:%S" file last-revision)
  (let (;;(xgit-rev (int-to-string (nth 1 last-revision)))
        (default-directory (car last-revision)))
    ;; TODO: support the last-revision parameter??
    (insert (dvc-run-dvc-sync
             'xgit (list "admin-cat" file)
             :finished 'dvc-output-buffer-handler))))

(provide 'xgit)
;;; xgit.el ends here
