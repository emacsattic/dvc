;;; dvc-bookmarks.el --- The bookmark system for DVC

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

;; This file provides a hierachical bookmark system for DVC

;;; History:

;;

;;; Code:
(require 'dvc-core)
(require 'dvc-state)

;; this were the settings used for tla
;; ;; Generated file. Do not edit!!!
;; (setq
;; tla-bookmarks-alist
;; '(("dvc"
;;   (local-tree "/home/srei/work/tla/xtla")
;;   (location "stefan@xsteve.at--public-2005" "dvc" "dev" "0" nil)
;;   (timestamp . "Wed Apr 27 10:45:31 2005"))
;;  ("emacs-muse"
;;   (local-tree "/home/srei/work/tla/emacs-muse")
;;   (location "mwolson@gnu.org--2006" "muse" "main" "1.0" nil)
;;   (timestamp . "Fri Dec 10 07:05:56 2004"))))

;; what I want to have:
;; hierachical tree of bookmarks
;; support for different dvc's
;; short name for working copy/branch
;; local-tree
;; timestamp => bookmark-creation-date?
;; different colors
;; optional: dvc: xhg, bzr,...
;; bookmark editing via C-k, C-y (just like in gnus)

;; saved under ~/.dvc/bookmarks.el

;; a data structure for testing purposes
(defvar dvc-bookmark-alist
  '(("hg"
     (local-tree "~/work/hg/hg"))
    ("work-stuff"
     (children
      ("home-dir"
       (local-tree "~/"))
      ("another-dir"
       (local-tree "~/work")))))
  "The bookmarks used for dvc")
;;(pp-to-string dvc-bookmark-alist)

(defvar dvc-bookmarks-file-name "dvc-bookmarks.el" "The file that holds the dvc bookmarks")

(defvar dvc-bookmarks-loaded nil "Whether `dvc-bookmark-alist' has been loaded from `dvc-bookmarks-file-name'.")
(defvar dvc-bookmarks-cookie nil "The ewoc cookie for the *dvc-bookmarks* buffer.")

(defvar dvc-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map [return] 'dvc-bookmarks-goto)
    (define-key map "\C-m"   'dvc-bookmarks-goto)
    (define-key map "n"      'dvc-bookmarks-next)
    (define-key map "p"      'dvc-bookmarks-previous)
    (define-key map "a"      'dvc-bookmarks-add)
    (define-key map "\C-y"   'dvc-bookmarks-yank)
    (define-key map "\C-k"   'dvc-bookmarks-kill)
    (define-key map "s"      'dvc-bookmarks-status)
    (define-key map "l"      'dvc-bookmarks-changelog)
    (define-key map "L"      'dvc-bookmarks-log)
    (define-key map "m"      'dvc-bookmarks-missing)
    (define-key map "f"      'dvc-bookmarks-pull)
    (define-key map "."      'dvc-bookmarks-show-info-at-point)
    (define-key map "\C-x\C-s" 'dvc-bookmarks-save)
    map)
  "Keymap used in `dvc-bookmarks-mode'.")

(easy-menu-define dvc-bookmarks-mode-menu dvc-bookmarks-mode-map
  "`dvc-bookmarks-mode' menu"
  `("dvc-bookmarks"
    ["Go to working copy" dvc-bookmarks-goto t]
    ["DVC status" dvc-bookmarks-status t]
    ["DVC changelog" dvc-bookmarks-changelog t]
    ["DVC log" dvc-bookmarks-log t]
    ["DVC missing" dvc-bookmarks-missing t]
    ["DVC pull" dvc-bookmarks-pull t]
   "--"
    ["Add new bookmark" dvc-bookmarks-add t]
    ["Save bookmarks" dvc-bookmarks-save t]
     ))

(defun dvc-bookmarks-printer (elem)
  (let ((entry (car elem))
        (indent (cadr elem)))
  (insert (format "%s%s" (make-string indent ? ) entry))))

(defun dvc-bookmarks-add-to-cookie (elem indent &optional node)
  (let ((curr (or node (ewoc-locate dvc-bookmarks-cookie)))
        (data (list (car elem) indent elem))
        (enter-function (if (eq (dvc-line-number-at-pos) 1) 'ewoc-enter-before 'ewoc-enter-after)))
    (cond ((assoc 'children elem)
           (setq node (apply enter-function (list dvc-bookmarks-cookie curr data)))
           (dolist (child (cdr (assoc 'children elem)))
             (dvc-bookmarks-add-to-cookie child (+ indent 2) node)))
          (t
           (if curr
               (apply enter-function (list dvc-bookmarks-cookie curr data))
             (ewoc-enter-last dvc-bookmarks-cookie data))))
    (forward-line 1)))

;;;###autoload
(defun dvc-bookmarks (&optional arg)
  "Display the *dvc-bookmarks* buffer.
With prefix argument ARG, reload the bookmarks file from disk."
  (interactive "P")
  (dvc-bookmarks-load-from-file arg)
  (switch-to-buffer (get-buffer-create "*dvc-bookmarks*"))
  (toggle-read-only 0)
  (erase-buffer)
  (set (make-local-variable 'dvc-bookmarks-cookie)
       (ewoc-create (dvc-ewoc-create-api-select
		     #'dvc-bookmarks-printer)))
  (put 'dvc-bookmarks-cookie 'permanent-local t)
  (dolist (entry dvc-bookmark-alist)
    (dvc-bookmarks-add-to-cookie entry 0))
  (goto-char (point-min))
  (dvc-bookmarks-mode))

(defun dvc-bookmarks-mode ()
  "Mode to display DVC bookmarks."
  (interactive)
  (kill-all-local-variables)
  (use-local-map dvc-bookmarks-mode-map)
  (setq major-mode 'dvc-bookmarks-mode)
  (setq mode-name "dvc-bookmarks")
  (toggle-read-only 1))

(defun dvc-bookmarks-show-info-at-point ()
  (interactive)
  (message "%S" (dvc-bookmarks-current-data)))

(defun dvc-bookmarks-current-data ()
  (nth 2 (ewoc-data (ewoc-locate dvc-bookmarks-cookie))))

(defun dvc-bookmarks-current-value (key)
  (cadr (assoc key (cdr (dvc-bookmarks-current-data)))))

(defun dvc-bookmarks-add (bookmark-name bookmark-local-dir)
  "Add a DVC bookmark"
  (interactive
   (let* ((bmk-name (read-string "DVC bookmark name: "))
          (bmk-loc (dvc-read-directory-name (format "Set DVC bookmark %s in directory: " bmk-name))))
     (list bmk-name bmk-loc)))
  (let* ((elem (list bookmark-name (list 'local-tree bookmark-local-dir)))
         (data (list (car elem) 0 elem)))
    (dvc-bookmarks)
    (add-to-list 'dvc-bookmark-alist elem t)
    (ewoc-enter-last dvc-bookmarks-cookie data)))

(defun dvc-bookmarks-next ()
  (interactive)
  (forward-line 1))

(defun dvc-bookmarks-previous ()
  (interactive)
  (forward-line -1))

(defun dvc-bookmarks-goto ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (find-file local-tree)
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-status ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-status))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-changelog ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-changelog))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-log ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-log))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-missing ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-missing))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-pull ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-pull))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-yank ()
  (interactive)
  (let ((indent (save-excursion (if (eq (line-beginning-position) (line-end-position))
                                    0
                                  (forward-line 1) (nth 1 (ewoc-data (ewoc-locate dvc-bookmarks-cookie)))))))
    (dvc-bookmarks-add-to-cookie dvc-bookmarks-tmp-yank-item indent)))

(defvar dvc-bookmarks-tmp-yank-item '("hg" (local-tree "~/work/hg/hg")))
(defun dvc-bookmarks-kill ()
  (interactive)
  (setq dvc-bookmarks-tmp-yank-item (dvc-bookmarks-current-data))
  (let ((buffer-read-only nil))
    (ewoc-delete dvc-bookmarks-cookie (ewoc-locate dvc-bookmarks-cookie))))

(defun dvc-bookmarks-save ()
  "Save `dvc-bookmark-alist' to the file `dvc-bookmarks-file-name'."
  (interactive)
  (dvc-save-state '(dvc-bookmark-alist)
                  (dvc-config-file-full-path dvc-bookmarks-file-name t)
                  t))

(defun dvc-bookmarks-load-from-file (&optional force)
  "Load bookmarks from the file `dvc-bookmarks-file-name'.

If FORCE is non-nil, reload the file even if it was loaded before."
  (when (or force (not dvc-bookmarks-loaded))
    (dvc-load-state (dvc-config-file-full-path
                     dvc-bookmarks-file-name t))
    (setq dvc-bookmarks-loaded t)))

(provide 'dvc-bookmarks)
;;; dvc-bookmarks.el ends here
