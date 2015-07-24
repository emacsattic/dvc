;;; xgit-log-edit.el --- Major mode to edit commit messages for git

;; Copyright (C) 2015  Stephen Leake
;; Copyright (C) 2009  Matthieu Moy

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords: git

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun xgit-dvc-log-edit-file-name ()
  (concat (file-name-as-directory (xgit-git-dir))
	  xgit-log-edit-file-name))

;;;###autoload
(defvar xgit-log-edit-file-name
  "DVC_EDITMSG"
  "The filename used to store the log message before commiting.
Usually that file is placed in the .git directory of the working tree.")

(defvar xgit-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?s)] 'xgit-log-edit-insert-sob)
    map)
  "Keymap used in `xgit-log-edit-mode' buffers.")

(easy-menu-define xgit-log-edit-mode-menu xgit-log-edit-mode-map
  "`xgit-log-edit-mode' menu"
  '("Log"
    ["Insert Signed-off-by:"     xgit-log-edit-insert-sob t]
    ))

(defun xgit-log-edit-insert-sob ()
  (interactive)
  (goto-char (point-max))
  (re-search-backward "^[^#\n]")
  (end-of-line)
  (newline 2)
  (insert "Signed-off-by: " user-full-name " <" user-mail-address ">"))

;;;###autoload
(define-derived-mode xgit-log-edit-mode dvc-log-edit-mode "xgit-log-edit"
  "Major Mode to edit xgit log messages."
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (setq fill-column 73))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (concat "/" xgit-log-edit-file-name "$") 'xgit-log-edit-mode))

(provide 'xgit-log-edit)
;;; xgit-log-edit.el ends here
