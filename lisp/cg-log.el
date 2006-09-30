;;; cg-log.el --- Cogito interface for dvc: mode for git log style output

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

;; The git/cogito interface for dvc: a mode to handle cg log style output

;;; History:

;;

;;; Code:

(defvar cg-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map [?g] 'cg-log)
    (define-key map [?s] 'cg-status)
    (define-key map dvc-keyvec-next 'cg-log-next)
    (define-key map dvc-keyvec-previous 'cg-log-previous)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in `cg-log-mode'.")

(easy-menu-define cg-log-mode-menu cg-log-mode-map
  "`cg-log-mode' menu"
  `("cg-log"
    ["Show status" cg-status t]
    ["Start Commiting" dvc-log-edit t]
    ))


(defvar cg-log-font-lock-keywords
   '(("^committer" . font-lock-function-name-face)
     ("^commit" . font-lock-function-name-face)
     ("^tree" . font-lock-function-name-face)
     ("^parent" . font-lock-function-name-face)
     ("^author" . font-lock-function-name-face))
  "Keywords in `cg-log-mode' mode.")


(define-derived-mode cg-log-mode fundamental-mode "cg-log"
  "Major mode to display cg log output.

Commands:
\\{cg-log-mode-map}
"
  (set (make-local-variable 'font-lock-defaults)
       (list 'cg-log-font-lock-keywords t nil nil))
  (toggle-read-only 1))

(defconst cg-log-start-regexp "^commit +\\([0-9a-f]+\\)")
(defun cg-log-next (n)
  "Move to the next changeset header of the next diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-forward cg-log-start-regexp nil t n)
  (beginning-of-line))

(defun cg-log-previous (n)
  "Move to the previous changeset header of the previous diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-backward cg-log-start-regexp)
  (re-search-backward cg-log-start-regexp nil t n))

(defun cg-log-revision-at-point ()
  (save-excursion
    (end-of-line)
    (re-search-backward cg-log-start-regexp)
    (match-string-no-properties 1)))

(provide 'cg-log)
;;; cg-log.el ends here
