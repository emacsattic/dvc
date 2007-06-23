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
(eval-when-compile (require 'cl))
(require 'xgit-annotate)

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

(defvar xgit-status-line-regexp
  "^#[ \t]+\\([[:alpha:]][[:alpha:][:blank:]]+\\):\\(?:[ \t]+\\(.+\\)\\)?$"
  "Regexp that matches a line of status output.
The first match string is the status type, and the optional
second match is the file.")

(defvar xgit-status-untracked-regexp "^#\t\\(.+\\)$"
  "Regexp that matches a line of status output indicating an
untracked file.

The first match is the file.")

(defvar xgit-status-renamed-regexp "^\\(.+\\) -> \\(.+\\)$"
  "Regexp that divides a filename string.
The first match is the original file, and the second match is the
new file.")

(defun xgit-parse-status-sort (status-list)
  "Sort STATUS-LIST in the order A, M, R, D, ?."
  (let ((order '(("A" . 1) ("M" . 2) ("R" . 3) ("D" . 4) ("?" . 5))))
    (sort status-list
          #'(lambda (a b)
              (let ((ao (cdr (assoc (car (cddr a)) order)))
                    (bo (cdr (assoc (car (cddr b)) order))))
                (and (integerp ao) (integerp bo)
                     (< ao bo)))))))

(defun xgit-parse-status  (changes-buffer)
  (dvc-trace "xgit-parse-status (dolist)")
  (with-current-buffer changes-buffer
    (setq dvc-header (format "git status for %s\n" default-directory))
    (with-current-buffer output
      (save-excursion
        (goto-char (point-min))
        (let ((buffer-read-only)
              (grouping "")
              status-string
              file status modif dir orig
              status-list)
          (while (re-search-forward xgit-status-line-regexp nil t)
            (setq status-string (match-string 1)
                  file (match-string 2)
                  modif nil
                  dir nil
                  orig nil)
            (cond ((or (null file) (string= "" file))
                   (when (string= status-string "Untracked files")
                     (let ((end
                            (save-excursion
                              (re-search-forward xgit-status-line-regexp
                                                 nil 'end)
                              (point))))
                       (forward-line 2)
                       (while (re-search-forward xgit-status-untracked-regexp
                                                 end t)
                         (when (match-beginning 1)
                           (setq status-list
                                 (cons (list 'file (match-string 1) "?")
                                       status-list))))
                       (forward-line -1)))
                   (setq grouping status-string
                         status nil))
                  ((string= status-string "modified")
                   (setq status "M"))
                  ((string= status-string "new file")
                   (setq status "A"))
                  ((string= status-string "deleted")
                   (setq status "D")
                   (when (string= grouping "Changed but not updated")
                     (setq modif "?")))
                  ((string= status-string "renamed")
                   (setq status "R")
                   (when (string-match xgit-status-renamed-regexp file)
                     (setq orig (match-string 1 file)
                           file (match-string 2 file)
                           dir " ")))
                  (t
                   (setq status nil)))
            (when status
              (setq status-list (cons (list 'file file status modif dir orig)
                                      status-list))))
          (with-current-buffer changes-buffer
            (dolist (elem (xgit-parse-status-sort (nreverse status-list)))
              (ewoc-enter-last dvc-diff-cookie elem))))))))

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
                                (capture root)))))
     :error
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer (capture buffer)
         (if (> (point-max) (point-min))
             (dvc-show-changes-buffer output 'xgit-parse-status
                                      (capture buffer))
           (dvc-diff-no-changes (capture buffer)
                                "No changes in %s"
                                (capture root))))))))

(defcustom git-log-max-count -1
  "Number of logs to print.  Specify negative value for all logs.
Limiting this to low number will shorten time for log retrieval
for large projects like Linux kernel on slow machines (Linux
kernel has >50000 logs)."
  :type 'integer
  :group 'dvc-xgit)

(defcustom git-log-pretty ""
  "Specify '--pretty=' option to pass to git-log.
You can choose from 'oneline', 'short', 'medium', 'full',
'fuller', 'email' or 'raw' or other options supported in your
version of git.
If string is empty, will not pass '--pretty=' option."
  :type 'string
  :group 'dvc-xgit)

(defun* git-log (dir &key cnt log-regexp diff-match rev file)
  "Run git log for DIR.
DIR is a directory controlled by Git/Cogito.
CNT is max number of log to print.  If not specified, uses git-log-max-count.
LOG-REGEXP is regexp to filter logs by matching commit logs.
DIFF-MATCH is string to filter logs by matching commit diffs.
REV is revision to show.
FILE is filename in repostory to filter logs by matching filename.
"
  (interactive)
  (let* ((buffer (dvc-get-buffer-create 'xgit 'log))
         (repo (xgit-git-dir dir))
         (cmd "log")
         (count (format "--max-count=%s" (if cnt cnt git-log-max-count)))
         (grep (if log-regexp (format "--grep=%s" log-regexp)))
         (diff (if diff-match (format "-S%s" diff-match)))
         (pretty (if (not (string= "" git-log-pretty)) (format "--pretty=%s" git-log-pretty)))
         (fname (if file (file-relative-name file (xgit-tree-root dir))))
         (args (list repo cmd pretty count grep diff rev "--" fname)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (dvc-run-dvc-sync 'xgit args
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (progn
                          (with-current-buffer (capture buffer)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert-buffer-substring output)
                              (goto-char (point-min))
                              (insert (format "git %s\n\n" (any-to-string args)))
                              (xgit-log-mode))))))))

;; TODO: update for git
(defun xgit-log ()
  "Run git log."
  (interactive)
  (git-log default-directory))

(defun xgit-log-grep (regexp)
  "Limit the log output to ones with log message that matches the specified pattern."
  (interactive "MGrep pattern for Commit Log: ")
  (git-log default-directory :log-regexp regexp))

(defun xgit-log-file (filename)
  "Limit the log output to ones that changes the specified file."
  (interactive "FFile name: ")
  (git-log default-directory :file filename))

(defun xgit-log-diff-grep (string)
  "Limit the logs that contain the change in given string."
  (interactive "MGrep pattern for Commit Diff: ")
  (git-log default-directory :diff-match string))

(defun xgit-log-revision (rev)
  "Show log for a given hash id."
  (interactive "MID: ")
  (git-log default-directory :cnt 1 :rev rev))

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
         (command-list '("diff" "HEAD")))
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

(defcustom git-show-filter-filename-func (function (lambda (files) nil))
  "Function to filter filenames in git-show.
Function is passed a list of files as a parameter.
Function should return list of filenames that is passed to git-show or nil for all files."
  :type 'function
  :group 'dvc-xgit)

(defun git-show-filter-filename-not-quilt (files)
  "Function to filter-out quilt managed files under .pc/ and patches/."
  (loop for f in files
        when (not (string-match "\.pc/\\|patches/" f))
        collect f))

(defun git-changed-files (dir rev)
  "Returns list of files changed in given revision"
  (let* ((repo (xgit-git-dir dir))
         (cmd "diff-tree")
         (args (list repo cmd "--numstat" rev))
         (result (dvc-run-dvc-sync 'xgit args
                                   :finished 'dvc-output-buffer-split-handler)))
    (mapcar (lambda (x) (nth 2 (split-string x)))
            (cdr result ))))

(defun xgit-show (dir rev &optional files)
  "Shows diff for a given revision.
Optional argument FILES is a string of filename or list of filenames of to pass to git-show.
If FILES is nil and git-show-filter-filename-func is non-nil,
files changed in the revision is passed to git-show-filter-filename-func and result is used."
  (interactive)
  (if (and (null files) git-show-filter-filename-func)
      (setq files (funcall git-show-filter-filename-func (git-changed-files dir rev))))
  (let* ((buffer (dvc-get-buffer-create 'xgit 'diff))
         (repo (xgit-git-dir dir))
         (cmd "show")
         (args (list repo cmd rev "--")))
    (if files
        (setq args (append args (if (stringp files) (list files) files))))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (dvc-run-dvc-sync 'xgit args
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (progn
                          (with-current-buffer (capture buffer)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert-buffer-substring output)
                              (goto-char (point-min))
                              (insert (format "git %s\n\n" (any-to-string args)))
                              (diff-mode)
                              (toggle-read-only 1))))))))

(defvar git-describe-regexp "^\\(.*?\\)-\\([0-9]+\\)-g[[:xdigit:]]\\{7\\}")

(defun git-describe-tag? (abbrev)
  (not (string-match git-describe-regexp abbrev)))

(defun xgit-describe (dir rev)
  "Show the most recent tag that is reachable from a commit.
If there is no tag return nil,
if revision is a tag, return tag in a string,
else returns list of '(tag offset all-described-string)."
  (interactive)
  (let* ((repo (xgit-git-dir dir))
	 (cmd "describe")
	 (args (list repo cmd rev))
	 (info (dvc-run-dvc-sync 'xgit args
				 :finished 'dvc-output-buffer-handler
				 :error 'dvc-output-buffer-handler)))
    (if (string= "" info)
	nil				;no tag yet
      (if (git-describe-tag? info)
	   info
	(progn
	  (list (match-string 1 info)
		(match-string 2 info)
		info))))))

(defun git-annotate (dir file)
  "Run git annotate for file in DIR.
DIR is a directory controlled by Git/Cogito.
FILE is filename in repostory.
"
  (let* ((buffer (dvc-get-buffer-create 'xgit 'annotate))
	 (repo (xgit-git-dir dir))
	 (cmd "blame")
	 (fname (file-relative-name file (xgit-tree-root dir)))
	 (args (list repo cmd "--" fname)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (dvc-run-dvc-sync 'xgit args
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (progn
                          (with-current-buffer (capture buffer)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (insert-buffer-substring output)
                              (goto-char (point-min))
                              (xgit-annotate-mode))))))))

(defun xgit-annotate ()
  "Run git annotate"
  (interactive)
  (let* ((line (dvc-line-number-at-pos))
	 (filename (dvc-confirm-read-file-name "Filename to annotate: "))
	 (default-directory (xgit-tree-root filename)))
    (git-annotate default-directory filename)
    (goto-line line)))


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
