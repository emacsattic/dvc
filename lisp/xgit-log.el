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
    (define-key map [?g] 'xgit-log)
    (define-key map [?s] 'xgit-status)
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
   '(("^committer" . font-lock-function-name-face)
     ("^commit" . font-lock-function-name-face)
     ("^tree" . font-lock-function-name-face)
     ("^parent" . font-lock-function-name-face)
     ("^author" . font-lock-function-name-face))
  "Keywords in `xgit-log-mode' mode.")


(define-derived-mode xgit-log-mode fundamental-mode "xgit-log"
  "Major mode to display cg log output.

Commands:
\\{xgit-log-mode-map}
"
  (set (make-local-variable 'font-lock-defaults)
       (list 'xgit-log-font-lock-keywords t nil nil))
  (toggle-read-only 1))

(defconst xgit-log-start-regexp "^commit +\\([0-9a-f]+\\)")
(defun xgit-log-next (n)
  "Move to the next changeset header of the next diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-forward xgit-log-start-regexp nil t n)
  (beginning-of-line))

(defun xgit-log-previous (n)
  "Move to the previous changeset header of the previous diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-backward xgit-log-start-regexp)
  (re-search-backward xgit-log-start-regexp nil t n))

(defun xgit-log-revision-at-point ()
  (save-excursion
    (end-of-line)
    (re-search-backward xgit-log-start-regexp)
    (match-string-no-properties 1)))

(provide 'xgit-log)
;;; xgit-log.el ends here
