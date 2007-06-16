;;; xhg-log.el --- Mercurial interface for dvc: mode for hg log style output

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

;; The mercurial interface for dvc: a mode to handle xhg log style output

;;; History:

;;

;;; Code:

(require 'diff-mode)

(defvar xhg-log-mode-map
  (let ((map (copy-keymap diff-mode-shared-map)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map [?g] 'xhg-log)
    (define-key map [?e] 'xhg-export)
    (define-key map [?E] 'xhg-export-via-mail)
    (define-key map [?s] 'xhg-status)
    (define-key map [?=] 'xhg-log-toggle-diff-for-changeset)
    (define-key map dvc-keyvec-next 'xhg-log-next)
    (define-key map dvc-keyvec-previous 'xhg-log-previous)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)

    ;; the merge group
    (define-key map (dvc-prefix-merge ?u) 'dvc-update)
    (define-key map (dvc-prefix-merge ?f) 'dvc-pull) ;; hint: fetch, p is reserved for push
    (define-key map (dvc-prefix-merge ?m) 'dvc-missing)
    map)
  "Keymap used in `xhg-log-mode'.")

(easy-menu-define xhg-log-mode-menu xhg-log-mode-map
  "`xhg-log-mode' menu"
  `("hg-log"
    ["Show status" xhg-status t]
    ["Toggle embedded diff" xhg-log-toggle-diff-for-changeset t]
    ["Start Commiting" dvc-log-edit t]
    ["Export Changeset" xhg-export t]
    ["Export Changeset via Email" xhg-export-via-mail t]
    ))


(defvar xhg-log-font-lock-keywords
  (append
   '(("^changeset:" . font-lock-function-name-face)
     ("^tag:" . font-lock-function-name-face)
     ("^user:" . font-lock-function-name-face)
     ("^date:" . font-lock-function-name-face)
     ("^summary:" . font-lock-function-name-face)
     ("^parent:" . font-lock-function-name-face))
   diff-font-lock-keywords)
  "Keywords in `xhg-log-mode' mode.")


(define-derived-mode xhg-log-mode fundamental-mode "xhg-log"
  "Major mode to display hg log output with embedded diffs. Derives from `diff-mode'.

Commands:
\\{xhg-log-mode-map}
"
  (let ((diff-mode-shared-map (copy-keymap xhg-log-mode-map))
        major-mode mode-name)
    (diff-mode))
  (set (make-local-variable 'font-lock-defaults)
       (list 'xhg-log-font-lock-keywords t nil nil)))

(defconst xhg-log-start-regexp "^changeset: +\\([0-9]+:[0-9a-f]+\\)")
(defun xhg-log-next (n)
  "Move to the next changeset header of the next diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-forward xhg-log-start-regexp nil t n)
  (beginning-of-line))
;; TODO: add (diff-hunk-next)

(defun xhg-log-previous (n)
  "Move to the previous changeset header of the previous diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-backward xhg-log-start-regexp)
  (re-search-backward xhg-log-start-regexp nil t n))
;; TODO: add (diff-hunk-prev)

(defun xhg-log-revision-at-point ()
  (save-excursion
    (end-of-line)
    (re-search-backward xhg-log-start-regexp)
    (match-string-no-properties 1)))

(defun xhg-log-toggle-diff-for-changeset ()
  "Toggle displaying the diff for the current changeset."
  (interactive)
  (let ((rev (xhg-log-revision-at-point))
        (insert-diff))
    (dvc-trace "xhg-log-toggle-diff-for-changeset %s" rev)
    (save-excursion
      (end-of-line)
      (re-search-backward xhg-log-start-regexp)
      (re-search-forward "^$")
      (forward-line 1)
      (setq insert-diff (not (looking-at "diff")))
      (let ((buffer-read-only nil))
        (save-excursion
          (if insert-diff
              (progn (save-excursion
                       (insert
                        (dvc-run-dvc-sync 'xhg (list "log" "-r" rev "-p")
                                          :finished 'dvc-output-buffer-handler)))
                     (delete-region (point) (- (re-search-forward "^diff") 4)))
            (delete-region (point)
                           (or (and (re-search-forward xhg-log-start-regexp nil t) (line-beginning-position))
                               (goto-char (point-max))))))))))


(provide 'xhg-log)
;;; xhg-log.el ends here
