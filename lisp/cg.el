;;; cg.el --- cogito/git interface for dvc

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

;; The cogito/git interface for dvc
;; The cogito manpage can be found online at:
;;  http://www.kernel.org/pub/software/scm/cogito/docs/

;;; History:

;;

;;; Code:

(require 'dvc-core)
(require 'cg-core)
(require 'cg-log)

(defun cg-init (&optional dir)
  "Run cg init -I."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for cg init: "
                                                     (or default-directory
                                                         (getenv "HOME"))))))
  (dvc-run-dvc-sync 'cg (list "init" "-I" dir)
                     :finished (dvc-capturing-lambda
                                   (output error status arguments)
                                 (message "cg init finished"))))

(defun cg-add-files (&rest files)
  "Run cg add."
  (message "cg-add-files: %s" files)
  (dvc-run-dvc-sync 'cg (append '("add") files)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "cg add finished"))))

(defun cg-command-version ()
  "Run cg version."
  (interactive)
  (let ((version (dvc-run-dvc-sync 'cg (list "version")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Cogito Version: %s" version))
    version))

(defun cg-parse-status  (changes-buffer)
  (dvc-trace "cg-parse-status (dolist)")
  (let ((status-list
         (split-string (dvc-buffer-content output) "\n")))
    (with-current-buffer changes-buffer
      (setq dvc-header (format "cg status -w for %s\n" default-directory))
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

(defun cg-status (&optional against path)
  "Run cg status."
  (interactive (list nil default-directory))
  (let* ((dir (or path default-directory))
         (root (cg-tree-root dir))
         (buffer (dvc-prepare-changes-buffer
                  `(cg (last-revision ,root 1))
                  `(cg (local-tree ,root))
                  'status root 'cg)))
    (dvc-switch-to-buffer-maybe buffer)
    (setq dvc-buffer-refresh-function 'cg-status)
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync
     'cg '("status" "-w")
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer (capture buffer)
         (if (> (point-max) (point-min))
             (dvc-show-changes-buffer output 'cg-parse-status
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

(defun cg-log ()
  "Run cg log."
  (interactive)
  (let ((buffer (dvc-get-buffer-create 'cg 'log)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (dvc-run-dvc-sync 'cg '("log")
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (progn
                          (with-current-buffer (capture buffer)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert-buffer-substring output)
                              (goto-char (point-min))
                              (insert (format "cg log for %s\n\n" default-directory))
                              (cg-log-mode))))))))

;; copied from xhg-parse-diff: not yet fully working
(defun cg-parse-diff (changes-buffer)
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

(defun cg-diff (&optional against path dont-switch)
    (interactive (list nil nil current-prefix-arg))
  (let* ((cur-dir (or path default-directory))
         (orig-buffer (current-buffer))
         (root (cg-tree-root cur-dir))
         (buffer (dvc-prepare-changes-buffer
                  `(cg (last-revision ,root 1))
                  `(cg (local-tree ,root))
                  'diff root 'cg))
         (command-list '("diff")))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (when dont-switch (pop-to-buffer orig-buffer))
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync 'cg command-list
                       :finished
                       (dvc-capturing-lambda (output error status arguments)
                         (dvc-show-changes-buffer output 'cg-parse-diff
                                                  (capture buffer))))))

(defun cg-restore (force &rest files)
  "Run cg restore

cg-restore
 -r REVISION: Not supported yet
 -f: FORCE"
  (message "cg-restore: %s" files)
  (let ((args (cons "restore"
                    (if force '("-f") '()))))
    (dvc-run-dvc-sync 'cg (append args files)
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "cg restore finished")))))
(defun cg-revert-files (&rest files)
  "See `cg-restore'"
  (apply 'cg-restore t files))

;; --------------------------------------------------------------------------------
;; dvc revision support
;; --------------------------------------------------------------------------------
;;;###autoload
(defun cg-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)"
  (dvc-trace "cg-revision-get-last-revision file:%S last-revision:%S" file last-revision)
  (let (;;(cg-rev (int-to-string (nth 1 last-revision)))
        (default-directory (car last-revision)))
    ;; TODO: support the last-revision parameter??
    (insert (dvc-run-dvc-sync
             'cg (list "admin-cat" file)
             :finished 'dvc-output-buffer-handler))))

(provide 'cg)
;; arch-tag: e2bf03e1-20c3-4c57-b5a0-82f24491e6f6
;;; cg.el ends here
