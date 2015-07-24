;;; dvc-status.el --- A generic status mode for DVC

;; Copyright (C) 2007 - 2009, 2011, 2013-2015 by all contributors

;; Author: Stephen Leake, <stephen_leake@stephe-leake.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(require 'dvc-ui)
(require 'dvc-defs)
(require 'dvc-core)
(require 'dvc-fileinfo)
(require 'uniquify)

(defcustom dvc-status-display-known nil
  "If non-nil, display files with 'known' status in dvc-status buffer."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-status-display-ignored nil
  "If non-nil, display files with 'ignored' status in dvc-status buffer."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-diff-preference 'ediff
  "Prefered operation for reviewing changes to modified files.
Choices are 'ediff and 'diff; 'ediff is painfully slow over VPN
connections, so 'diff may be preferred then."
  :type '(choice (const ediff)
		 (const diff))
  :group 'dvc)

(defvar dvc-status-mode-map
  (let ((map (make-sparse-keymap)))
    ;; grouped by major function, then alphabetical by dvc-keyvec name
    ;; workspace operations
    (define-key map "E"    'dvc-fileinfo-toggle-exclude)
    (define-key map "R"    'dvc-fileinfo-rename)
    (define-key map "\M-I" 'dvc-ignore-file-extensions)
    (define-key map "\M-c" 'dvc-status-resolve-conflicts)
    (define-key map "\M-d" 'dvc-status-dtrt)
    (define-key map "\M-e" 'dvc-edit-exclude)
    (define-key map "\r"   'dvc-find-file-other-window)
    (define-key map "d"    'dvc-fileinfo-unstage-files)
    (define-key map "l"    'dvc-diff-log-single)
    (define-key map "s"    'dvc-fileinfo-stage-files)
    (define-key map "t"    'dvc-fileinfo-add-log-entry)
    (define-key map "#e"   'dvc-edit-ignore-files)
    (define-key map "I"    'dvc-ignore-file-extensions-in-dir)
    (define-key map "i"    'dvc-fileinfo-ignore-files)
    (define-key map "k"    'dvc-fileinfo-kill)
    (define-key map "a"    'dvc-fileinfo-add-files)
    (define-key map "c"    'dvc-log-edit)
    (define-key map "e"    'dvc-status-ediff-work-base)
    (define-key map "wb"   'dvc-status-ediff-work-base)
    (define-key map "ws"   'dvc-status-ediff-work-staged)
    (define-key map "bs"   'dvc-status-ediff-staged-base)
    (define-key map "="    'dvc-diff-diff)
    (define-key map "?"    'describe-mode)
    (define-key map "L"    'dvc-log)
    (define-key map "m"    'dvc-fileinfo-mark-file)
    (define-key map "**"   'dvc-fileinfo-mark-all)
    (define-key map "n"    'dvc-fileinfo-next)
    (define-key map "p"    'dvc-fileinfo-prev)
    (define-key map "q"    'dvc-buffer-quit)
    (define-key map "g"    'dvc-generic-refresh)
    (define-key map "r"    'dvc-fileinfo-remove-files)
    (define-key map "U"    'dvc-fileinfo-revert-files)
    (define-key map "u"    'dvc-fileinfo-unmark-file)
    (define-key map "*!"   'dvc-fileinfo-unmark-all)
    map)
  "Keymap used in `dvc-status-mode		   '.")

(easy-menu-define dvc-status-mode-menu dvc-status-mode-map
  "`dvc-status' menu"
  `("DVC"
    ["Refresh Buffer"              dvc-generic-refresh               t]
    ["Edit log before commit"      dvc-log-edit                      t]
    ["Quit"                        dvc-buffer-quit                   t]
    ("Merge/Update"
     ["Update"                     dvc-update                        t]
     ["Show missing"               dvc-missing                       t]
     ["Merge"                      dvc-merge                         t]
     )
    ("Mark"
     ["Mark File"                  dvc-fileinfo-mark-file            t]
     ["Mark all"                   dvc-fileinfo-mark-all             t]
     ["Unmark File"                dvc-fileinfo-unmark-file          t]
     ["Unmark all"                 dvc-fileinfo-unmark-all           t]
     )
    ("Ignore"
     ["Ignore Files"               dvc-fileinfo-ignore-files         t]
     ["Ignore Extensions in dir"   dvc-ignore-file-extensions-in-dir t]
     ["Ignore Extensions globally" dvc-ignore-file-extensions        t]
     ["Edit Ignore File"           dvc-edit-ignore-files             t]
     )
    ("Exclude"
     ["Exclude File"               dvc-fileinfo-toggle-exclude t]
     ["Edit Exclude File"          dvc-edit-exclude t]
     )
    ["Rename File"                 dvc-fileinfo-rename               t]
    ["Revert File"                 dvc-fileinfo-revert-files         t]
    ["Stage File"                  dvc-fileinfo-stage-files          (dvc-stagep)]
    ["Unstage File"                dvc-fileinfo-unstage-files        (dvc-stagep)]
    ["Resolve Conflicts in file"   dvc-status-resolve-conflicts      t]
    ["Add File"                    dvc-fileinfo-add-files            t]
    ["Do the Right Thing"          dvc-status-dtrt                   t]
    "---"
    ["Ediff work base"             dvc-status-ediff-work-base        t]
    ["Ediff work staged"           dvc-status-ediff-work-staged      (dvc-stagep)]
    ["Ediff staged base"           dvc-status-ediff                  (dvc-stagep)]
    ["diff File"                   dvc-diff-diff                     t]
    ["Delete File"                 dvc-fileinfo-remove-files         t]
    ["Kill File"                   dvc-fileinfo-kill                 t]
    ["Edit File"                   dvc-find-file-other-window        t]
    ["Add log entry"               dvc-fileinfo-add-log-entry        t]
    ["Log (single file)"           dvc-diff-log-single               t]
    ["Log (full tree)"             dvc-log                           t]
    ))

;; "<back-end>-status-mode", if defined, will be used instead of this
;; one. If so, it should be derived from dvc-status-mode (via
;; `define-derived-mode'), and rely on it for as many features as
;; possible (one can, for example, extend the menu and keymap).
;; Remember to add the new mode to uniquify-list-buffers-directory-modes
(define-derived-mode dvc-status-mode fundamental-mode "dvc-status"
  "Major mode to display workspace status."
  (setq dvc-buffer-current-active-dvc (dvc-current-active-dvc))
  (setq dvc-fileinfo-ewoc (ewoc-create 'dvc-fileinfo-printer))
  (set (make-local-variable 'dvc-get-file-info-at-point-function) 'dvc-fileinfo-current-file)
  (use-local-map dvc-status-mode-map)
  (easy-menu-add dvc-status-mode-menu)
  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(add-to-list 'uniquify-list-buffers-directory-modes 'dvc-status-mode)

(defun dvc-status-prepare-buffer (dvc root base-revision branch header-more refresh)
  "Prepare and return a status buffer. Should be called by <back-end>-dvc-status.
Calls <back-end>-status-mode.
DVC is back-end.
ROOT is absolute path to workspace.
BASE-REVISION is a string identifying the workspace's base revision.
BRANCH is a string identifying the workspace's branch.
HEADER-MORE is a function called to add other text to the ewoc header;
it should return a string.
REFRESH is a function that refreshes the status; see `dvc-buffer-refresh-function'."

  (let ((status-buffer (dvc-get-buffer-create dvc 'status root)))
    (dvc-kill-process-maybe status-buffer)
    (with-current-buffer status-buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (let ((dvc-temp-current-active-dvc dvc))
        (funcall (dvc-function dvc "status-mode")))
      (let ((header (concat
                      (format "Status for %s:\n" root)
                      (format "  base revision : %s\n" base-revision)
                      (format "  branch        : %s\n" branch)
                      (if (functionp header-more) (funcall header-more))))
            (footer ""))
        (set (make-local-variable 'dvc-buffer-refresh-function) refresh)
        (ewoc-filter dvc-fileinfo-ewoc (lambda (elem) nil))
        (ewoc-set-hf dvc-fileinfo-ewoc header footer)
        (ewoc-enter-last dvc-fileinfo-ewoc (make-dvc-fileinfo-message :text (format "Running %s..." dvc)))
        (ewoc-refresh dvc-fileinfo-ewoc)))
    (dvc-switch-to-buffer-maybe status-buffer)))

(defun dvc-status-dtrt (key-prefix)
  "Do The Right Thing in a status buffer; update, commit, resolve
conflicts, and/or ediff current files. Other choices are
available on the menu. Dispatches to back-end specific
`<dvc>-dvc-status-dtrt'. "
  (interactive "P")

  (let (status)
    ;; Note that message elements cannot be marked. Make sure all
    ;; selected files need the same action.
    (if (< 1 (length (dvc-fileinfo-marked-files)))
        (ewoc-map (lambda (fileinfo)
                    (etypecase fileinfo
                      (dvc-fileinfo-message
                       nil)

                      (dvc-fileinfo-file ; also matches dvc-fileinfo-dir
                       (when (dvc-fileinfo-file-mark fileinfo)
			 (if status
			     (if (not (equal status (dvc-fileinfo-file-status fileinfo)))
				 (error "cannot Do The Right Thing on files with different status"))
			   (setq status (dvc-fileinfo-file-status fileinfo))))
                       ;; don't redisplay the element
                       nil)))
                  dvc-fileinfo-ewoc)

      ;; else only one file
      (setq status (dvc-fileinfo-file-status (dvc-fileinfo-current-fileinfo))))

    (dvc-call "dvc-status-dtrt" key-prefix status)
    ))

(defun dvc-status-inventory-done (status-buffer)
  (with-current-buffer status-buffer
    (ewoc-enter-last dvc-fileinfo-ewoc (make-dvc-fileinfo-message :text "Parsing inventory..."))
    (ewoc-refresh dvc-fileinfo-ewoc)
    (dvc-redisplay)
    ;; delete "running", "parsing" from the ewoc now, but don't
    ;; refresh until the status is displayed
    (dvc-fileinfo-delete-messages)))

(defun dvc-status-ediff (&optional file-choice)
  "Show differences between workspace, staged, or database base files.
FILE-CHOICE is one of 'work-staged 'work-base 'staged-base (default 'work-base).
Display is determined by `dvc-diff-preference': one of 'diff 'ediff."
  (interactive)
  (when (null file-choice) (setq file-choice 'work-base))
  (when (< 1 (length (dvc-fileinfo-marked-files)))
    (error "cannot ediff more than one file at a time"))
  ;; FIXME: need user interface to specify other revision to diff
  ;; against. At least BASE and HEAD.
  (let ((dvc-temp-current-active-dvc dvc-buffer-current-active-dvc))
     (case dvc-diff-preference
       (ediff
	(dvc-file-ediff (dvc-fileinfo-current-file) file-choice))
       (diff
	(dvc-diff-diff file-choice))
       )))

(defun dvc-status-ediff-work-staged ()
  (interactive)
  (dvc-status-ediff 'work-staged))

(defun dvc-status-ediff-work-base ()
  (interactive)
  (dvc-status-ediff 'work-base))

(defun dvc-status-ediff-staged-base ()
  (interactive)
  (dvc-status-ediff 'staged-base))

(defun dvc-status-resolve-conflicts ()
  (interactive)
  ;; ediff to build resolve conflict
  (when (< 1 (length (dvc-fileinfo-marked-files)))
    (error "cannot resolve conflicts in more than one file at a time"))
  (find-file (dvc-fileinfo-current-file))
  (vc-resolve-conflicts))

(provide 'dvc-status)
;;; end of file
