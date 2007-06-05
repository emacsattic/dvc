;;; xgit-log.el --- git interface for dvc: mode for git log style output

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

;; The git interface for dvc: a mode to handle cg log style output

;;; History:

;;

;;; Code:

(defvar xgit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map [?=] 'xgit-show-revision-at-point)
    (define-key map [?g] 'xgit-log)
    (define-key map [?s] 'xgit-status)
    (define-key map [?G] 'xgit-log-grep)
    (define-key map [?F] 'xgit-log-file)
    (define-key map [?S] 'xgit-log-diff-grep)
    (define-key map [?I] 'xgit-log-revision)
    (define-key map [?d] 'xgit-describe-revision-at-point)
    (define-key map dvc-keyvec-next 'xgit-log-next)
    (define-key map dvc-keyvec-previous 'xgit-log-previous)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in `xgit-log-mode'.")

(easy-menu-define xgit-log-mode-menu xgit-log-mode-map
  "`xgit-log-mode' menu"
  `("xgit-log"
    ["Show status" xgit-status t]
    ["Start Commiting" dvc-log-edit t]
    ))


(defvar xgit-log-font-lock-keywords
  '(("^[Cc]ommit[ter]*[Date]*[:]?" . font-lock-function-name-face)
     ("^commit" . font-lock-function-name-face)
     ("^tree" . font-lock-function-name-face)
     ("^parent" . font-lock-function-name-face)
     ("^[Aa]uthor[Date]*[:]?" . font-lock-function-name-face)
     ("^Date:" . font-lock-function-name-face)
     ("^From[:]?" . font-lock-function-name-face)
     ("^Subject" . font-lock-function-name-face)
     ("^Merge:" . font-lock-function-name-face)
     ("^No tag:" . font-lock-function-name-face)
     ("^Tag: .*$" . font-lock-warning-face)
     ("^    .*$" . font-lock-type-face)
     )
  "Keywords in `xgit-log-mode' mode.")


(define-derived-mode xgit-log-mode fundamental-mode "xgit-log"
  "Major mode to display cg log output.

Commands:
\\{xgit-log-mode-map}
"
  (set (make-local-variable 'font-lock-defaults)
       (list 'xgit-log-font-lock-keywords t nil nil))
  (toggle-read-only 1))

;; Regexp to match output of git log.
;; Matches to:
;; --pretty=oneline
;;   a679aff48d8abf688c53720cb626d2ab00ebd53d <First line of commit>
;; or
;;   commit a679aff48d8abf688c53720cb626d2ab00ebd53d
(defconst xgit-log-start-regexp
  "^commit +\\([[:xdigit:]]\\{40\\}\\)\\|^\\([[:xdigit:]]\\{40\\}\\)\\|From \\([[:xdigit:]]\\{40\\}\\)")

(defun xgit-log-next (n)
  "Move to the next changeset header of the next diff hunk"
  (interactive "p")
  (let ((retval))
    (end-of-line)
    (setq retval (re-search-forward xgit-log-start-regexp nil t n))
    (beginning-of-line)
    retval))

(defun xgit-log-previous (n)
  "Move to the previous changeset header of the previous diff hunk"
  (interactive "p")
  (let ((retval))
    (end-of-line)
    (re-search-backward xgit-log-start-regexp)
    (setq retval (re-search-backward xgit-log-start-regexp nil t n))
    retval))

(defun xgit-log-revision-at-point ()
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward xgit-log-start-regexp)
    (cond ((string= "oneline" git-log-pretty)
           (match-string-no-properties 2))
          ((string= "email" git-log-pretty)
           (match-string-no-properties 3))
          (t
           (match-string-no-properties 1)))))

(defun xgit-show-revision-at-point (n)
  "Shows diff for a given revision"
  (interactive "p")
  (let ((tmp git-show-filter-filename-func))
    (if (> n 1) (setq tmp nil))
    (let ((git-show-filter-filename-func tmp))
      (xgit-show default-directory (xgit-log-revision-at-point))))
  (xgit-describe-revision-at-point))

(defun xgit-describe-revision-at-point ()
  "Shows most recent tag for a given revision"
  (interactive)
  (end-of-line)
  (re-search-backward xgit-log-start-regexp)
  (let ((info (xgit-describe default-directory (xgit-log-revision-at-point))))
    (when info
      (toggle-read-only 0)
      (if (stringp info)
	  (insert (format "Tag: %s\n" info))
	(insert (format "No tag: %s\n" (nth 2 info))))
      (toggle-read-only 1)
      info)))

(defun xgit-describe-all-revision ()
  "Shows most recent tag for all revisions in a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (xgit-log-next 1)
      (xgit-describe-revision-at-point)
      )))

(provide 'xgit-log)
;;; xgit-log.el ends here
