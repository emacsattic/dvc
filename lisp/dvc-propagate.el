;;; dvc-propagate.el --- manage multiple propagations for DVC backends
;;
;; Copyright (C) 2009 - 2015 Stephen Leake
;;
;; Author: Stephen Leake
;; Keywords: tools
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;;; code

(require 'cl-lib)

(require 'dvc-state)

(defvar-local dvc-propagate-from-root ""
  "Buffer-local variable holding `from' root directory.
This directory is scanned for workspaces to be propagated.")
(put 'dvc-propagate-from-root 'permanent-local t)

(defvar-local dvc-propagate-to-root ""
  "Buffer-local variable holding `to' root directory.")
(put 'dvc-propagate-to-root 'permanent-local t)

(defvar-local dvc-propagate-ewoc nil
  "Buffer-local ewoc for displaying propagations.
All dvc-propagate functions operate on this ewoc.
The elements must all be of class dvc-propagate-data.")
(put 'dvc-propagate-ewoc 'permanent-local t)

(cl-defstruct (dvc-propagate-data (:copier nil))
  from-work          ; directory name relative to dvc-propagate-from-root
  to-work            ; directory name relative to dvc-propagate-to-root
		     ; from-work is often the same string as to-work (but not the same directory)
  from-name          ; display name, in buffer and menus; usually root dir name
  to-name            ; "
  from-branch        ; backend branch name (assumed never changes)
  to-branch          ; "
  need-refresh       ; nil | t; if an async process was started that invalidates state data
  from-head-revs     ; list of backend rev string; current head revision or (left right) if multiple heads
  to-head-revs       ; "
  conflicts-buffer   ; *dvc-conflicts* buffer for this propagation
  from-status-buffer ; *dvc-status* buffer for commit in from
  to-status-buffer   ; *dvc-status* buffer for commit in to
  propagate-needed   ; nil | t | 'unknown
  from-heads         ; 'at-head | 'need-update | 'need-merge
  to-heads           ; "
  from-local-changes ; see dvc-state.el dvc-state-data local-changes
  to-local-changes   ; "
  conflicts          ; 'need-scan | 'need-resolve | 'need-review-resolve-internal | 'resolved | 'none
  )

(defun dvc-propagate-from-work (data)
  (concat dvc-propagate-from-root (dvc-propagate-data-from-work data)))

(defun dvc-propagate-to-work (data)
  (concat dvc-propagate-to-root (dvc-propagate-data-to-work data)))

(defun dvc-propagate-from-name ()
  "Display name for current `from' workspace."
  (dvc-propagate-data-from-name (ewoc-data (ewoc-locate dvc-propagate-ewoc))))

(defun dvc-propagate-to-name ()
  "Display name for current `to' workspace."
  (dvc-propagate-data-to-name (ewoc-data (ewoc-locate dvc-propagate-ewoc))))

(defun dvc-propagate-need-refresh (elem data)
  "Force refresh of ELEM, which must have data DATA."
  (setf (dvc-propagate-data-need-refresh data) t)
  (ewoc-invalidate dvc-propagate-ewoc elem))

(defun dvc-propagate-printer (data)
  "Print an ewoc element."
  (if (string= (dvc-propagate-data-from-work data)
               (dvc-propagate-data-to-work data))
      ;; from, to have same dir names; just show one
      (insert (dvc-face-add (format "%s\n" (dvc-propagate-data-from-work data)) 'dvc-keyword))

    ;; show both
    (insert (dvc-face-add (format "%s -> %s\n"
                                  (dvc-propagate-data-from-work data)
                                  (dvc-propagate-data-to-work data))
                          'dvc-keyword)))

  (if (dvc-propagate-data-need-refresh data)
      ;; ewoc data is not current
      (insert (dvc-face-add "  need refresh\n" 'dvc-conflict))

    ;; ewoc data is current
    (dvc-state-print-local-changes (dvc-propagate-data-from-local-changes data) (dvc-propagate-data-from-name data))
    (dvc-state-print-local-changes (dvc-propagate-data-to-local-changes data) (dvc-propagate-data-to-name data))

    (cl-ecase (dvc-propagate-data-from-heads data)
      (at-head     nil)
      (need-update
       (insert (dvc-face-add (concat "  need update " (dvc-propagate-data-from-name data) "\n")
                             'dvc-conflict)))
      (need-merge
       (insert (dvc-face-add (concat "  need state for merge " (dvc-propagate-data-from-name data) "\n")
                             'dvc-conflict))))

    (cl-ecase (dvc-propagate-data-to-heads data)
      (at-head
       (when (null (dvc-propagate-data-propagate-needed data))
	 (insert (dvc-face-add "  need clean\n" 'dvc-conflict))))

      (need-update
       (insert (dvc-face-add (concat "  need update " (dvc-propagate-data-to-name data) "\n")
                             'dvc-conflict)))
      (need-merge
       (insert (dvc-face-add (concat "  need state for merge " (dvc-propagate-data-to-name data) "\n")
                                   'dvc-conflict))))

    (cl-case (dvc-propagate-data-propagate-needed data)
      (unknown
       (insert (dvc-face-add "  need push/pull\n" 'dvc-header)))

      (nil
       nil)

      (t
       (when (and (eq 'at-head (dvc-propagate-data-from-heads data))
		  (eq 'at-head (dvc-propagate-data-to-heads data)))
	 (cl-ecase (dvc-propagate-data-conflicts data)
	   (need-scan
	    (insert "conflicts need scan\n"))
	   (need-resolve
	    (insert (dvc-face-add "  need resolve conflicts\n" 'dvc-conflict)))
	   (need-review-resolve-internal
	    (insert (dvc-face-add "  need review resolve internal\n" 'dvc-header))
	    (if (dvc-rebasep)
		(progn
		  (insert (dvc-face-add "  need merge\n" 'dvc-conflict))
		  (insert (dvc-face-add "  need rebase\n" 'dvc-conflict)))
	      (insert (dvc-face-add "  need propagate\n" 'dvc-conflict))))
	   ((resolved none)
	    (if (dvc-rebasep)
		(progn
		  (insert (dvc-face-add "  need merge\n" 'dvc-conflict))
		  (insert (dvc-face-add "  need rebase\n" 'dvc-conflict)))
	      (insert (dvc-face-add "  need propagate\n" 'dvc-conflict)))
	    (when (dvc-create-squashed-commitp)
	      (insert (dvc-face-add "  need create squashed commit\n" 'dvc-conflict))))))
       ))
    ))

(defun dvc-propagate-create-from-status-buffer (data)
  "Create from-status buffer for DATA"
  (save-excursion
    (let ((result (dvc-status (dvc-propagate-from-work data) t)))
      (setf (dvc-propagate-data-from-status-buffer data) (car result)
	    (dvc-propagate-data-from-local-changes data) (cadr result))
      )))

(defun dvc-propagate-create-to-status-buffer (data)
  "Create to-status buffer for DATA"
  (save-excursion
    (let ((result (dvc-status (dvc-propagate-to-work data) t)))
      (setf (dvc-propagate-data-to-status-buffer data) (car result)
	    (dvc-propagate-data-to-local-changes data) (cadr result))
      )))

(defun dvc-propagate-clean-1 (data save-conflicts)
  "Clean DATA workspace, kill associated automate session.
If SAVE-CONFLICTS non-nil, don't delete conflicts files."
  (let ((default-directory (dvc-propagate-from-work data)))
    (dvc-kill-session))
  (let ((default-directory (dvc-propagate-to-work data)))
    (dvc-kill-session))
  (dvc-save-kill-buffer (dvc-propagate-data-conflicts-buffer data))
  (dvc-safe-kill-buffer (dvc-propagate-data-from-status-buffer data))
  (dvc-safe-kill-buffer (dvc-propagate-data-to-status-buffer data))

  (unless save-conflicts
    (let ((default-directory (dvc-propagate-to-work data)))
      (dvc-conflicts-clean)))
  )

(defun dvc-propagate-clean ()
  "Clean current workspace, delete from ewoc."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))

    (dvc-propagate-clean-1 data nil)
    (let ((inhibit-read-only t))
      (ewoc-delete dvc-propagate-ewoc elem))))

(defun dvc-propagate-clean-all (&optional save-conflicts)
  "Clean all remaining workspaces."
  (interactive)
  (ewoc-map 'dvc-propagate-clean-1 dvc-propagate-ewoc save-conflicts))

(defun dvc-propagate-cleanp ()
  "Non-nil if clean is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    ;; don't check need-refresh here; allow deleting after just doing
    ;; final required action in another buffer. Or we've just started,
    ;; but the user knows it's ok.
    (and (member (car (dvc-propagate-data-from-local-changes data)) '(need-scan ok))
         (member (car (dvc-propagate-data-to-local-changes data)) '(need-scan ok))
         (not (dvc-propagate-data-propagate-needed data))
         (member (dvc-propagate-data-to-heads data) '(need-scan at-head)))))

(defun dvc-propagate-do-refresh-one ()
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-refresh-one
     data
     (or current-prefix-arg
	 ;; user wants full refresh
	 (not (dvc-propagate-data-need-refresh data))))
    (ewoc-invalidate dvc-propagate-ewoc elem)))

(defun dvc-propagate-refreshp ()
  "Non-nil if refresh is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (or (dvc-propagate-data-need-refresh data)
        (eq 'need-scan (car (dvc-propagate-data-from-local-changes data)))
        (eq 'need-scan (car (dvc-propagate-data-to-local-changes data))))))

(defun dvc-propagate-state-from ()
  "Show dvc-state buffer for `from' workspace, so it can be committed, updated, merged, etc."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-need-refresh elem data)
    ;; assume user will do whatever is needed
    (setf (dvc-propagate-data-from-local-changes data) (list 'ok))
    (dvc-state-one (dvc-propagate-from-work data))
    ))

(defun dvc-propagate-state-from-prompt (prefix suffix)
  "Return menu label for `dvc-propagate-state' entry."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc)))
	(result prefix))
    (when (eq 'unknown (dvc-propagate-data-propagate-needed data))
      (setq result (concat result " push/pull")))
    (when (memq 'need-commit (dvc-propagate-data-from-local-changes data))
      (setq result (concat result " commit")))
    (when (memq 'need-stash-save (dvc-propagate-data-from-local-changes data))
      (setq result (concat result " stash-save")))
    (when (memq 'need-stash-pop (dvc-propagate-data-from-local-changes data))
      (setq result (concat result " stash-pop")))
    (concat result " " suffix)))

(defun dvc-propagate-state-fromp ()
  "Non-nil if dvc-status-one is appropriate for current `from' workspace."
  (let* ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (and (not (dvc-propagate-data-need-refresh data))
	 (or
	  (eq 'unknown (dvc-propagate-data-propagate-needed data))
	  (eq (dvc-propagate-data-from-heads data) 'need-merge)
	  (memq 'need-commit (dvc-propagate-data-from-local-changes data))
	  (memq 'need-stash-pop (dvc-propagate-data-from-local-changes data))
	  ))))

(defun dvc-propagate-state-to-prompt (prefix suffix)
  "Return menu label for `dvc-propagate-state' entry."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc)))
	(result prefix))
    (when (eq (dvc-propagate-data-to-heads data) 'need-merge)
      (setq result (concat result " merge")))
    (when (eq 'unknown (dvc-propagate-data-propagate-needed data))
      (setq result (concat result " push/pull")))
    (when (memq 'need-commit (dvc-propagate-data-to-local-changes data))
      (setq result (concat result " commit")))
    (when (memq 'need-stash-save (dvc-propagate-data-to-local-changes data))
      (setq result (concat result " stash-save")))
    (when (memq 'need-stash-pop (dvc-propagate-data-to-local-changes data))
      (setq result (concat result " stash-pop")))
    (concat result " " suffix)))

(defun dvc-propagate-state-to ()
  "Show status buffer for `to' workspace, so it can be committed, updated, or merged."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-need-refresh elem data)
    ;; assume user will do whatever is needed
    (setf (dvc-propagate-data-to-local-changes data) (list 'ok))
    (dvc-state-one (dvc-propagate-to-work data))
    ))

(defun dvc-propagate-state-top ()
  "Non-nil if dvc-status or dvc-state is appropriate for current `to' workspace."
  (let* ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (and (not (dvc-propagate-data-need-refresh data))
         (or
	  (eq 'unknown (dvc-propagate-data-propagate-needed data))
	  (eq (dvc-propagate-data-to-heads data) 'need-merge)
	  (memq 'need-commit (dvc-propagate-data-to-local-changes data))
	  (memq 'need-stash-pop (dvc-propagate-data-to-local-changes data))
	  ))))

(defun dvc-propagate-diff ()
  "Preview propagate via diff."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-need-refresh elem data)
    (ewoc-invalidate dvc-propagate-ewoc elem)

    (dvc-delta
     (car (dvc-propagate-data-from-head-revs data)) ; = left
     (car (dvc-propagate-data-to-head-revs data)) ; = right
     )))

(defun dvc-propagate-update-to ()
  "Update current `to' workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-need-refresh elem data)
    (let ((default-directory (dvc-propagate-to-work data)))
      (dvc-update (car (dvc-propagate-data-to-head-revs data))))
    (dvc-propagate-refresh-one data nil)
    (ewoc-invalidate dvc-propagate-ewoc elem)))

(defun dvc-propagate-update-top ()
  "Non-nil if update is appropriate for current `to' workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (and (not (dvc-propagate-data-need-refresh data))
	 (eq (dvc-propagate-data-to-heads data)
	     'need-update))))

(defun dvc-propagate-update-from ()
  "Update current `from' workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-need-refresh elem data)
    (let ((default-directory (dvc-propagate-from-work data)))
      (dvc-update (car (dvc-propagate-data-from-head-revs data))))
    (dvc-propagate-refresh-one data nil)
    (ewoc-invalidate dvc-propagate-ewoc elem)))

(defun dvc-propagate-update-fromp ()
  "Non-nil if update is appropriate for current `from' workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (and (not (dvc-propagate-data-need-refresh data))
	 (eq (dvc-propagate-data-from-heads data)
	     'need-update))))

(defun dvc-propagate-lca (data)
  "Return least common ancestor rev of `from', `to'."
  (dvc-lca (car (dvc-propagate-data-from-head-revs data))
	   (car (dvc-propagate-data-to-head-revs data))))

(defun dvc-propagate-propagate ()
  "Propagate current from, to."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-need-refresh elem data)

    (when (not (buffer-live-p (dvc-propagate-data-conflicts-buffer data)))
      ;; no conflicts, or user deleted conflicts buffer after
      ;; resolving conflicts; get it back
      (setf (dvc-propagate-data-conflicts data)
	    (dvc-propagate-conflicts data)))

    (when (dvc-propagate-data-conflicts-buffer data)
      (with-current-buffer (dvc-propagate-data-conflicts-buffer data)
	;; save-some-buffers does not save the conflicts buffer, which is the current buffer
	(save-buffer)))
    (dvc-propagate
     (dvc-propagate-data-from-branch data)
     (dvc-propagate-data-from-head-revs data)
     (dvc-propagate-data-to-branch data))
    (dvc-propagate-refresh-one data nil)
    (ewoc-invalidate dvc-propagate-ewoc elem)))

(defun dvc-propagate-propagatep ()
  "Non-nil if propagate is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (and (not (dvc-propagate-data-need-refresh data))
         (dvc-propagate-data-propagate-needed data)
         (eq 'at-head (dvc-propagate-data-from-heads data))
         (eq 'at-head (dvc-propagate-data-to-heads data))
         (member (dvc-propagate-data-conflicts data)
                 '(need-review-resolve-internal resolved none)))))

(defun dvc-propagate-rebase ()
  "Rebase current from, to."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-need-refresh elem data)

    ;; rebase assumes no pre-computed conflicts

    (dvc-rebase (dvc-propagate-data-from-branch data))
    (dvc-propagate-refresh-one data nil)
    (ewoc-invalidate dvc-propagate-ewoc elem)))

(defun dvc-propagate-resolve-conflicts ()
  "Resolve conflicts for current workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (dvc-propagate-need-refresh elem data)
    (setf (dvc-propagate-data-conflicts data) 'ok)
    (pop-to-buffer (dvc-propagate-data-conflicts-buffer data))))

(defun dvc-propagate-resolve-conflictsp ()
  "Non-nil if resolve conflicts is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (and (not (dvc-propagate-data-need-refresh data))
         (dvc-propagate-data-propagate-needed data)
         (eq 'at-head (dvc-propagate-data-from-heads data))
         (eq 'at-head (dvc-propagate-data-to-heads data))
         (member (dvc-propagate-data-conflicts data)
                 '(need-resolve need-review-resolve-internal)))))

(defun dvc-propagate-local-changes-to-ok ()
  "Ignore local changes in current `to' workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (dvc-propagate-data-to-local-changes data) (list 'ok))
    (ewoc-invalidate dvc-propagate-ewoc elem)))

(defun dvc-propagate-local-changes-top ()
  "Non-nil if local-changes-to-ok is appropriate for current `to' workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (and (not (dvc-propagate-data-need-refresh data))
         (member (car (dvc-propagate-data-to-local-changes data))
                 '(need-scan need-commit)))))

(defun dvc-propagate-local-changes-from-ok ()
  "Ignore local changes in current `from' workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (dvc-propagate-data-from-local-changes data) (list 'ok))
    (ewoc-invalidate dvc-propagate-ewoc elem)))

(defun dvc-propagate-local-changes-fromp ()
  "Non-nil if local-changes-from-ok is appropriate for current `from' workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-propagate-ewoc))))
    (and (not (dvc-propagate-data-need-refresh data))
         (member (car (dvc-propagate-data-from-local-changes data))
                 '(need-scan need-commit)))))

(defun dvc-propagate-quit-save ()
  "Quit, but save conflicts files for later resume."
  (interactive)
  (remove-hook 'kill-buffer-hook 'dvc-propagate-clean-all t)
  (dvc-propagate-clean-all t)
  (kill-buffer))

(defvar dvc-propagate-actions-map
  (let ((map (make-sparse-keymap "actions")))
    (define-key map [?c]  '(menu-item "c) clean/delete"
                                      dvc-propagate-clean
                                      :visible (dvc-propagate-cleanp)))
    (define-key map [?g]  '(menu-item "g) refresh"
                                      dvc-propagate-do-refresh-one
                                      :visible (dvc-propagate-refreshp)))
    (define-key map [?d]  '(menu-item "d) preview propagate diff"
                                      dvc-propagate-diff
                                      :visible (dvc-propagate-propagatep)))
    (define-key map [?9]  '(menu-item (concat "9) update " (dvc-propagate-to-name))
                                      dvc-propagate-update-to
                                      :visible (dvc-propagate-update-top)))
    (define-key map [?8]  '(menu-item (concat "8) update " (dvc-propagate-from-name))
                                      dvc-propagate-update-from
                                      :visible (dvc-propagate-update-fromp)))
    (define-key map [?7]  '(menu-item "7) squash"
                                      dvc-propagate-create-squashed
                                      :visible (and (dvc-propagate-propagatep)
						    (dvc-create-squashed-commitp))))
    (define-key map [?6]  '(menu-item "6) rebase"
                                      dvc-propagate-rebase
                                      :visible (and (dvc-rebasep)
						    (dvc-propagate-propagatep))))
    (define-key map [?5]  '(menu-item "5) propagate"
                                      dvc-propagate-propagate
                                      :visible (dvc-propagate-propagatep)))
    (define-key map [?4]  '(menu-item "4) resolve conflicts"
                                      dvc-propagate-resolve-conflicts
                                      :visible (dvc-propagate-resolve-conflictsp)))
    (define-key map [?3]  '(menu-item (concat "3) ignore local changes " (dvc-propagate-to-name))
                                      dvc-propagate-local-changes-to-ok
                                      :visible (dvc-propagate-local-changes-top)))
    (define-key map [?2]  '(menu-item (concat "2) ignore local changes " (dvc-propagate-from-name))
                                      dvc-propagate-local-changes-from-ok
                                      :visible (dvc-propagate-local-changes-fromp)))
    (define-key map [?1]  '(menu-item (dvc-propagate-state-to-prompt "1) state for" (dvc-propagate-to-name))
                                      dvc-propagate-state-to
                                      :visible (dvc-propagate-state-top)))
    (define-key map [?0]  '(menu-item (dvc-propagate-state-from-prompt "0) state for" (dvc-propagate-from-name))
                                      dvc-propagate-state-from
                                      :visible (dvc-propagate-state-fromp)))
    map)
  "Keyboard menu keymap used to manage propagates.")

(dvc-make-ewoc-next dvc-propagate-next dvc-propagate-ewoc)
(dvc-make-ewoc-prev dvc-propagate-prev dvc-propagate-ewoc)

(defvar dvc-propagate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" dvc-propagate-actions-map)
    (define-key map [?g]  'dvc-propagate-refresh)
    (define-key map [?n]  'dvc-propagate-next)
    (define-key map [?p]  'dvc-propagate-prev)
    (define-key map [?s]  'dvc-propagate-quit-save)
    (define-key map [?q]  'dvc-buffer-quit)
    map)
  "Keymap used in `dvc-propagate-mode'.")

(easy-menu-define dvc-propagate-mode-menu dvc-propagate-mode-map
  "Mtn specific status menu."
  `("DVC-Mtn"
    ["Do the right thing"    dvc-status-actions-map t]
    ["Quit, clean conflicts" dvc-buffer-quit t]
    ["Quit, save conflicts"  dvc-propagate-quit-save t]
    ))

(define-derived-mode dvc-propagate-mode nil "dvc-propagate"
  "Major mode to propagate multiple workspaces."
  (setq buffer-read-only nil)

  (dvc-install-buffer-menu)
  (add-hook 'kill-buffer-hook 'dvc-propagate-clean-all nil t)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil)
  (dvc-propagate-refresh)
  (goto-char (point-min))
  (dvc-propagate-next nil t))

(defun dvc-propagate-needed (data)
  "t if DATA needs propagate, nil if not, 'unknown otherwise."
  (let* ((from (car (dvc-propagate-data-from-head-revs data)))
	 (to (car (dvc-propagate-data-to-head-revs data)))
	 (default-directory (dvc-propagate-to-work data)) ;; merge is done here
	 (lca (dvc-lca from to)))

    (cond
     ((symbolp lca) lca)
     ((stringp lca) (not (string= lca from)))
     (t (error "programmer error")))
    ))

(defun dvc-propagate-conflicts (data)
  "Return value for dvc-propagate-data-conflicts for DATA."
  (let* ((default-directory (dvc-propagate-to-work data))
	 (result (dvc-conflicts-status
		  (dvc-propagate-data-conflicts-buffer data) ; buffer
		  (dvc-propagate-from-work data) ; left-work
		  (car (dvc-propagate-data-from-head-revs data)) ; left-rev
		  (dvc-propagate-to-work data) ; right-work
		  (car (dvc-propagate-data-to-head-revs data)) ; right-rev
		  (dvc-propagate-data-from-branch data) ; left-branch
		  (dvc-propagate-data-to-branch data) ; right-branch
		  )))
    (setf (dvc-propagate-data-conflicts-buffer data) (car result))
    (cadr result)))

(defun dvc-propagate-refresh-one (data refresh-local-changes)
  "Refresh DATA."
  (let ((from-work (dvc-propagate-from-work data))
        (to-work (dvc-propagate-to-work data)))

    (let* ((default-directory (dvc-propagate-from-work data))
	   (heads (dvc-heads))
	   (from-base-rev (dvc-base-revision)))
      (setf (dvc-propagate-data-from-head-revs data) heads)
      (case (length heads)
	(1
	 (if (string= (car heads) from-base-rev)
	     (setf (dvc-propagate-data-from-heads data) 'at-head)
	   (setf (dvc-propagate-data-from-heads data) 'need-update)))
	(t
	 (setf (dvc-propagate-data-from-heads data) 'need-merge)))
      )

    (let* ((default-directory (dvc-propagate-to-work data))
	   (heads (dvc-heads))
	   (to-base-rev (dvc-base-revision)))
      (setf (dvc-propagate-data-to-head-revs data) heads)
      (case (length heads)
	(1
	 (if (string= (car heads) to-base-rev)
	     (setf (dvc-propagate-data-to-heads data) 'at-head)
	   (setf (dvc-propagate-data-to-heads data) 'need-update)))
	(t
	 (setf (dvc-propagate-data-to-heads data) 'need-merge)))
      )

    (setf (dvc-propagate-data-propagate-needed data)
          (dvc-propagate-needed data))

    (when refresh-local-changes
      (progn
	(setf (dvc-propagate-data-from-local-changes data) (list 'need-scan))
	(setf (dvc-propagate-data-to-local-changes data) (list 'need-scan))))

    (when (eq 'need-scan (car (dvc-propagate-data-from-local-changes data)))
      (dvc-propagate-create-from-status-buffer data))

    (when (eq 'need-scan (car (dvc-propagate-data-to-local-changes data)))
      (dvc-propagate-create-to-status-buffer data))

    (if (dvc-propagate-data-propagate-needed data)
	(progn
	  (when refresh-local-changes
	    (dvc-save-kill-buffer (dvc-propagate-data-conflicts-buffer data))
	    (let ((default-directory (dvc-propagate-to-work data)))
	      (dvc-conflicts-clean)))

	  (setf (dvc-propagate-data-conflicts data)
                (dvc-propagate-conflicts data))
	  )

      ;; can't compute conflicts if propagate not needed
      (setf (dvc-propagate-data-conflicts data) 'need-scan))

    (setf (dvc-propagate-data-need-refresh data) nil)

  ;; return non-nil to refresh display as we go along
  t))

(defun dvc-propagate-refresh ()
  "Refresh status of each ewoc element. With prefix arg, reset local changes status to `unknown'."
  (interactive)
  (ewoc-map 'dvc-propagate-refresh-one dvc-propagate-ewoc current-prefix-arg)
  ;; leaves point at (point-min)
  (dvc-propagate-next nil t)
  (message "done"))

(defun dvc-propagate-make-data (from-workspace to-workspace from-name to-name)
  "FROM-WORKSPACE, TO-WORKSPACE are relative names, FROM-NAME, TO_NAME should be root dir names."
    (let* ((from-work (concat dvc-propagate-from-root from-workspace))
           (to-work (concat dvc-propagate-to-root to-workspace))
           )

      (ewoc-enter-last
       dvc-propagate-ewoc
       (make-dvc-propagate-data
        :from-work from-workspace
        :to-work to-workspace
        :from-name from-name
        :to-name to-name
        :from-branch (let ((default-directory from-work)) (dvc-branch))
        :to-branch (let ((default-directory to-work)) (dvc-branch))
        :need-refresh t
	:from-heads 'need-scan
	:to-heads 'need-scan
	:conflicts-buffer nil
	:from-status-buffer nil
	:to-status-buffer nil
	:from-local-changes (list 'need-scan)
	:to-local-changes (list 'need-scan)
	:conflicts 'need-scan
	))))

;;;###autoload
(defun dvc-propagate-multiple (from-dir to-dir &optional workspaces)
  "Show all actions needed to propagate projects under FROM-DIR
to TO-DIR. WORKSPACES (default nil) is a list of workspaces
common to from-dir and to-dir; if nil, the directories are
scanned and all common ones found are used."
  (interactive "DPropagate all from (root directory): \nDto (root directory): ")
  (pop-to-buffer (get-buffer-create "*dvc-propagate*"))
  ;; dvc-propagate-*-root are buffer-local. Note that we don't care
  ;; what 'default-directory' is for dvc-propagate buffer.
  (setq dvc-propagate-from-root (file-name-as-directory (expand-file-name (substitute-in-file-name from-dir))))
  (setq dvc-propagate-to-root (file-name-as-directory (expand-file-name (substitute-in-file-name to-dir))))
  (let ((from-workspaces (or workspaces
                             (dvc-state-filter-non-ws dvc-propagate-from-root)))
        (to-workspaces (or workspaces
                           (dvc-state-filter-non-ws dvc-propagate-to-root))))

    (setq dvc-propagate-ewoc (ewoc-create 'dvc-propagate-printer))
    (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
    (ewoc-set-hf
     dvc-propagate-ewoc
     (concat
      (format "From root : %s\n" dvc-propagate-from-root)
      (format "  To root : %s\n" dvc-propagate-to-root)
      )
     "")
    (dolist (workspace from-workspaces)
      (if (member workspace to-workspaces)
          (dvc-propagate-make-data
           (cdr workspace)
           (cdr workspace)
           (file-name-nondirectory (directory-file-name dvc-propagate-from-root))
           (file-name-nondirectory (directory-file-name dvc-propagate-to-root)))))
    (redisplay)
    (dvc-propagate-mode)))

;;;###autoload
(defun dvc-propagate-one (from-work to-work)
  "Show all actions needed to propagate FROM-WORK to TO-WORK."
  (interactive "DPropagate all from (workspace): \nDto (workspace): ")
  (setq from-work (file-name-as-directory (expand-file-name (substitute-in-file-name from-work))))
  (setq to-work (file-name-as-directory (expand-file-name (substitute-in-file-name to-work))))
  (pop-to-buffer (get-buffer-create "*dvc-propagate*"))
  (setq default-directory to-work)
  (setq dvc-propagate-from-root (expand-file-name (concat from-work "../")))
  (setq dvc-propagate-to-root (expand-file-name (concat to-work "../")))
  (setq dvc-propagate-ewoc (ewoc-create 'dvc-propagate-printer))
  (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
  (ewoc-set-hf
   dvc-propagate-ewoc
   (concat
    (format "From root : %s\n" dvc-propagate-from-root)
    (format "  To root : %s\n" dvc-propagate-to-root)
    )
   "")
  (let ((from-name (file-name-nondirectory (directory-file-name from-work)))
	(to-name (file-name-nondirectory (directory-file-name to-work))))
    (if (string-equal from-name to-name)
	(progn
	  (setq from-name (file-name-nondirectory (directory-file-name dvc-propagate-from-root)))
	  (setq to-name (file-name-nondirectory (directory-file-name dvc-propagate-to-root)))))
    (dvc-propagate-make-data
     (file-name-nondirectory (directory-file-name from-work))
     (file-name-nondirectory (directory-file-name to-work))
     from-name
     to-name))
  (dvc-propagate-mode))

(provide 'dvc-propagate)

;; end of file
