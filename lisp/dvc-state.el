;;; dvc-state.el --- manage state of one or more workspaces relative to local repository

;; Copyright (C) 2013 - 2015 Stephen Leake

;; Author: Stephen Leake

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

(eval-when-compile (require 'cl-macs))

(defvar-local dvc-state-root ""
  "Buffer-local variable holding multi-workspace root directory.")

(defvar-local dvc-state-ewoc nil
  "Buffer-local ewoc for displaying workspace state.
All dvc-state functions operate on this ewoc.
The elements must all be of class dvc-state-data.")

(cl-defstruct (dvc-state-data (:copier nil))
  work             ; workspace directory name relative to dvc-state-root
  dvc              ; backend for workspace
  branch           ; name of current branch in work
  need-refresh     ; nil | t : if an async process was started that invalidates state data
  head-revs        ; either current head revision or (left, right) if multiple heads
  conflicts-buffer ; conflicts buffer for merge
  status-buffer    ; status buffer for commit
  heads            ; 'need-scan | 'at-head | 'need-update | 'need-merge
  update-review    ; 'pending | 'need-review | 'done
  review-buffer    ; revlist buffer showing update review
  local-changes    ; list of: 'need-scan | 'need-commit | 'need-stash-save | 'need-stash-pop | 'ok
  conflicts        ; 'need-scan | 'need-resolve | 'need-review-resolve-internal | 'resolved | 'none
  )

(defun dvc-state-work (data)
  (concat dvc-state-root (dvc-state-data-work data)))

(defun dvc-state-need-refresh (elem data local-changes)
  ;; The user has selected an action that will change the state of the
  ;; workspace via backend actions; set our data to reflect that. If
  ;; local-changes is non-nil, data-local-changes is set
  ;; to that value.
  (setf (dvc-state-data-need-refresh data) t)
  (setf (dvc-state-data-heads data) 'need-scan)
  (setf (dvc-state-data-conflicts data) 'need-scan)
  (when local-changes
    (setf (dvc-state-data-local-changes data) local-changes))
  (ewoc-invalidate dvc-state-ewoc elem))

(defun dvc-state-print-local-changes (local-changes label)
  (cond
   ((eq 'need-scan (car local-changes))
    (insert "  local changes not checked " label "\n"))

   ((eq 'ok (car local-changes))
    nil)

   (t
    (when (memq 'need-commit local-changes)
      (insert (dvc-face-add (concat "  need commit " label "\n") 'dvc-conflict)))
    (when (memq 'need-stash-save local-changes)
      (insert (dvc-face-add (concat "  need stash-save " label "\n") 'dvc-conflict)))
    (when (memq 'need-stash-pop local-changes)
      (insert (concat "  need stash-pop " label "\n"))
      (insert (concat "  need stash-drop " label "\n")))
    )
   ))

(defun dvc-state-printer (data)
  "Print an ewoc element."
  (let ((dvc-temp-current-active-dvc (dvc-state-data-dvc data)))
    (insert (dvc-face-add (format "%s\n" (dvc-state-data-work data)) 'dvc-keyword))

    (if (dvc-state-data-need-refresh data)
	(insert (dvc-face-add "  need refresh\n" 'dvc-conflict))

      (dvc-state-print-local-changes (dvc-state-data-local-changes data) "")

      (ecase (dvc-state-data-conflicts data)
	(need-scan
	 (insert "  conflicts need scan\n"))
	(need-resolve
	 (insert (dvc-face-add "  need resolve conflicts\n" 'dvc-conflict)))
	(need-review-resolve-internal
	 (insert (dvc-face-add "  need review resolve internal\n" 'dvc-header)))
	(resolved
	 (insert "  conflicts resolved\n"))
	((resolved none) nil))

      (ecase (dvc-state-data-heads data)
	(at-head     nil)
	(need-update
	 (insert (dvc-face-add "  need update\n" 'dvc-conflict)))

	(need-merge
	 (insert (dvc-face-add "  need merge\n" 'dvc-conflict))

	 (when (dvc-rebasep)
	   (insert (dvc-face-add "  need rebase\n" 'dvc-conflict))))
	)

      (ecase (dvc-state-data-update-review data)
	(pending nil)
	(need-review (insert "  need update review\n"))
	(done nil))
      )))

(defun dvc-state-clean-1 (data save-conflicts)
  "Clean DATA workspace, kill associated session (if any).
If SAVE-CONFLICTS non-nil, don't delete conflicts files."
  (let ((default-directory (dvc-state-work data)))
    (dvc-kill-session)
    (dvc-save-kill-buffer (dvc-state-data-conflicts-buffer data))
    (dvc-safe-kill-buffer (dvc-state-data-status-buffer data))
    (dvc-safe-kill-buffer (dvc-state-data-review-buffer data))
    (unless save-conflicts
      (dvc-conflicts-clean))))

(defun dvc-state-clean ()
  "Clean current workspace, delete from ewoc"
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
         (inhibit-read-only t))
    (dvc-state-clean-1 data nil)
    (ewoc-delete dvc-state-ewoc elem)))

(defun dvc-state-clean-all (&optional save-conflicts)
  "Clean all remaining workspaces."
  (interactive)
  (ewoc-map 'dvc-state-clean-1 dvc-state-ewoc save-conflicts))

(defun dvc-state-cleanp ()
  "Non-nil if clean & quit is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    ;; don't check need-refresh here; allow deleting after just doing
    ;; final required action in another buffer.
    (and (member (car (dvc-state-data-local-changes data)) '(need-scan ok))
         (member (dvc-state-data-heads data) '(need-scan at-head)))))

(defun dvc-state-do-refresh-one ()
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem)))
    (dvc-state-refresh-one
     data
     (or current-prefix-arg
	 ;; user wants full refresh
	 (not (dvc-state-data-need-refresh data))))
    (ewoc-invalidate dvc-state-ewoc elem)))

(defun dvc-state-refreshp ()
  "Non-nil if refresh is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (or (dvc-state-data-need-refresh data)
        ;; everything's done, but the user just did mtn sync, and more
        ;; stuff showed up
        (eq 'ok (car (dvc-state-data-local-changes data)))
        (eq 'at-head (dvc-state-data-heads data)))))

(defun dvc-state-update ()
  "Update current workspace from local repository."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem)))
    (dvc-state-need-refresh elem data nil)
    (setf (dvc-state-data-update-review data) 'need-review)
    (let ((default-directory (dvc-state-work data)))
      (dvc-update (dvc-state-data-head-revs data)))
    (dvc-state-refresh-one data nil)
    (ewoc-invalidate dvc-state-ewoc elem)))

(defun dvc-state-updatep ()
  "Non-nil if update is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (and (not (dvc-state-data-need-refresh data))
         (eq 'need-update (dvc-state-data-heads data))
	 (or
	  (memq 'ok (dvc-state-data-local-changes data))
	  (if (dvc-stashp)
	      (memq 'need-stash-pop (dvc-state-data-local-changes data))
	    (memq 'need-commit (dvc-state-data-local-changes data)))
	  ))
    ))

(defun dvc-state-update-preview ()
  "Preview update for current workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
	 (default-directory (dvc-state-work data)))
    (dvc-missing (dvc-state-work data))))

(defun dvc-state-resolve-conflicts ()
  "Resolve conflicts for current workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem)))
    (dvc-state-need-refresh elem data nil)
    (setf (dvc-state-data-conflicts data) 'need-scan)
    (pop-to-buffer (dvc-state-data-conflicts-buffer data))))

(defun dvc-state-resolve-conflictsp ()
  "Non-nil if resolve conflicts is appropriate for current workspace."
  (let* ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (and (not (dvc-state-data-need-refresh data))
         (member (dvc-state-data-conflicts data)
                 '(need-resolve need-review-resolve-internal)))))

(defun dvc-state-ignore-local-changes ()
  "Ignore local changes in current workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem)))
    (setf (dvc-state-data-local-changes data) (list 'ok))
    (ewoc-invalidate dvc-state-ewoc elem)))

(defun dvc-state-ignore-local-changesp ()
  "Non-nil if `dvc-state-ignore-local-changes' is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (and (not (dvc-state-data-need-refresh data))
	 (and (not (eq 'ok (car (dvc-state-data-local-changes data))))
	      (let ((default-directory (dvc-state-work data)))
		(dvc-ignore-local-changesp))))
    ))

(defun dvc-state-commit ()
  "Show status buffer for current workspace, for commit."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem)))
    ;; assume user will do a commit or stash-save
    (dvc-state-need-refresh elem data (or (remove 'need-stash-save
						  (remove 'need-commit (dvc-state-data-local-changes data)))
					  (list 'ok)))
    (pop-to-buffer (dvc-state-data-status-buffer data))
    ;; FIXME: handle user closing status buffer
    ;; IMPROVEME: create a log-edit buffer now, since we have both a
    ;; status and conflict buffer, and that confuses dvc-log-edit
    ))

(defun dvc-state-commitp ()
  "Non-nil if `dvc-state-commit' is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (and (not (dvc-state-data-need-refresh data))
         (not (eq 'ok (car (dvc-state-data-local-changes data)))))
    ))

(defun dvc-state-stash-save ()
  "Save the workspace changes to the stash stack."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
	 (default-directory (dvc-state-work data)))
    (dvc-state-need-refresh elem data (list 'need-scan)) ;; show stash-pop option
    (dvc-stash-save)
    ))

(defun dvc-state-stash-savep ()
  "Non-nil if `dvc-state-stash-save' is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (and (not (dvc-state-data-need-refresh data))
         (memq 'need-stash-save (dvc-state-data-local-changes data)))
    ))

(defun dvc-state-stash-show ()
  "Show the changes in the top of the stash stack."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
	 (default-directory (dvc-state-work data)))
    (dvc-stash-show)
    ))

(defun dvc-state-stash-pop ()
  "Apply the top stash, pop the stash stack."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
	 (default-directory (dvc-state-work data)))
    (dvc-state-need-refresh elem data (list 'need-scan))
    (dvc-stash-pop)
    ))

(defun dvc-state-stash-drop ()
  "Drop the top stash."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
	 (default-directory (dvc-state-work data)))
    (dvc-state-need-refresh elem data (list 'need-scan))
    (dvc-stash-drop)
    ))

(defun dvc-state-stash-popp ()
  "Non-nil if `dvc-state-stash-pop' is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (and (not (dvc-state-data-need-refresh data))
         (memq 'need-stash-pop (dvc-state-data-local-changes data)))
    ))

(defun dvc-state-update-review ()
  "Review last update for current workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
	 (default-directory (dvc-state-work data)))
    ;; assume they are adding fixmes
    (dvc-state-need-refresh elem data (list 'need-scan))
    (setf (dvc-state-data-update-review data) 'done)
    (setf (dvc-state-data-review-buffer data) (dvc-update-review))))

(defun dvc-state-update-reviewp ()
  "Non-nil if dvc-state-update-review is appropriate for current workspace."
  (let* ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (and (not (dvc-state-data-need-refresh data))
         (eq 'need-review (dvc-state-data-update-review data)))))

(defun dvc-state-merge ()
  "Merge heads in current workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
	 (default-directory (dvc-state-work data)))
    (dvc-state-need-refresh elem data nil)
    (dvc-safe-save-buffer (dvc-state-data-conflicts-buffer data))
    (dvc-merge)
    ;; FIXME: xgit does 'update' for 'merge'; need update-review?
    ))

(defun dvc-state-heads ()
  "Show heads for current workspace."
  (interactive)
  (let* ((elem (ewoc-locate dvc-state-ewoc))
         (data (ewoc-data elem))
         (default-directory (dvc-state-work data)))
    (dvc-state-need-refresh elem data nil)
    (dvc-heads-revlist)))

(defun dvc-state-headsp ()
  "Non-nil if `dvc-state-heads' is appropriate for current workspace."
  (let* ((data (ewoc-data (ewoc-locate dvc-state-ewoc))))
    (and (not (dvc-state-data-need-refresh data))
         (eq 'need-merge (dvc-state-data-heads data)))))

(defun dvc-state-quit-save ()
  "Quit, but save conflicts files for later resume."
  (interactive)
  (remove-hook 'kill-buffer-hook 'dvc-state-clean-all t)
  (dvc-state-clean-all t)
  (kill-buffer))

(defvar dvc-state-actions-map
  (let ((map (make-sparse-keymap "actions")))
    (define-key map [?c]  '(menu-item "c) clean/delete"
                                      dvc-state-clean
                                      :visible (dvc-state-cleanp)))
    (define-key map [?g]  '(menu-item "g) refresh"
                                      dvc-state-do-refresh-one
                                      :visible (dvc-state-refreshp)))
    (define-key map [?i]  '(menu-item "i) ignore local changes"
                                      dvc-state-ignore-local-changes
                                      :visible (dvc-state-ignore-local-changesp)))
    (define-key map [?r]  '(menu-item "r) rebase"
                                      dvc-rebase
                                      :visible (dvc-state-headsp)))
    (define-key map [?d]  '(menu-item "d) stash-drop"
                                      dvc-state-stash-drop
                                      :visible (dvc-state-stash-popp)))
    (define-key map [?p]  '(menu-item "p) stash-pop"
                                      dvc-state-stash-pop
                                      :visible (dvc-state-stash-popp)))
    (define-key map [?v]  '(menu-item "v) stash-view"
                                      dvc-state-stash-show
                                      :visible (dvc-state-stash-popp)))
    (define-key map [?s]  '(menu-item "s) stash-save"
                                      dvc-state-stash-save
                                      :visible (dvc-state-stash-savep)))
    (define-key map [?6]  '(menu-item "6) preview update"
                                      dvc-state-update-preview
                                      :visible (dvc-state-updatep)))
    (define-key map [?5]  '(menu-item "5) update review"
                                      dvc-state-update-review
                                      :visible (dvc-state-update-reviewp)))
    (define-key map [?4]  '(menu-item "4) update"
                                      dvc-state-update
                                      :visible (dvc-state-updatep)))
    (define-key map [?3]  '(menu-item "3) merge"
                                      dvc-state-merge
                                      :visible (dvc-state-headsp)))
    (define-key map [?2]  '(menu-item "2) show heads"
                                      dvc-state-heads
                                      :visible (dvc-state-headsp)))
    (define-key map [?1]  '(menu-item "1) resolve conflicts"
                                      dvc-state-resolve-conflicts
                                      :visible (dvc-state-resolve-conflictsp)))
    (define-key map [?0]  '(menu-item "0) commit"
                                      dvc-state-commit
                                      :visible (dvc-state-commitp)))
    map)
  "Keyboard menu keymap used in multiple-status mode.")

(dvc-make-ewoc-next dvc-state-next dvc-state-ewoc)
(dvc-make-ewoc-prev dvc-state-prev dvc-state-ewoc)

(defvar dvc-state-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" dvc-state-actions-map)
    (define-key map [?g]  'dvc-state-refresh)
    (define-key map [?m]  'dvc-state-update-preview)
    (define-key map [?n]  'dvc-state-next)
    (define-key map [?p]  'dvc-state-prev)
    (define-key map [?r]  'dvc-state-update-review)
    (define-key map [?s]  'dvc-state-quit-save)
    (define-key map [?q]  'dvc-buffer-quit)
    map)
  "Keymap used in `dvc-multiple-status-mode'.")

(easy-menu-define dvc-state-mode-menu dvc-state-mode-map
  "DVC state menu."
  `("DVC-state"
    ["Do the right thing"    dvc-state-actions-map t]
    ["Quit, clean conflicts" dvc-buffer-quit t]
    ["Quit, save conflicts"  dvc-state-quit-save t]
    ))

(define-derived-mode dvc-state-mode nil "dvc-state"
  "Major mode to show state of one or more workspaces."
  (setq buffer-read-only nil)

  (dvc-install-buffer-menu)
  (add-hook 'kill-buffer-hook 'dvc-state-clean-all nil t)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(defun dvc-state-conflicts (data)
  "Return value for dvc-state-data-conflicts for DATA."
  ;; only called if need merge; two items in head-revs
  (let ((result (dvc-conflicts-status
		 (dvc-state-data-conflicts-buffer data) ; buffer
		 (dvc-state-work data) ; left-work
		 (car (dvc-state-data-head-revs data)) ; left-rev
		 (dvc-state-work data) ; right-work
		 (cadr (dvc-state-data-head-revs data)) ; right-rev
		 (dvc-state-data-branch data) ; left-branch
		 (dvc-state-data-branch data) ; right-branch
		 )))
    (setf (dvc-state-data-conflicts-buffer data) (car result))
    (cadr result)))

(defun dvc-state-refresh-one (data refresh-local-changes)
  "Refresh DATA."
  (let ((default-directory (dvc-state-work data)))

    (let ((heads (dvc-heads))
          (base-rev (dvc-base-revision)))
      (case (length heads)
        (1
         (setf (dvc-state-data-head-revs data) (nth 0 heads))
         (setf (dvc-state-data-conflicts data) 'none)
         (if (string= (dvc-state-data-head-revs data) base-rev)
             (setf (dvc-state-data-heads data) 'at-head)
           (setf (dvc-state-data-heads data) 'need-update)))
        (t
         (setf (dvc-state-data-head-revs data) (list (nth 0 heads) (nth 1 heads)))
         (setf (dvc-state-data-heads data) 'need-merge))))

    (when refresh-local-changes
      (setf (dvc-state-data-local-changes data) (list 'need-scan))
      (setf (dvc-state-data-update-review data) 'need-review))

    (when (eq 'need-scan (car (dvc-state-data-local-changes data)))
      (save-excursion
	(let ((result (dvc-status (dvc-state-work data) t)))
	  (setf (dvc-state-data-status-buffer data) (car result)
		(dvc-state-data-local-changes data) (cadr result))
	  )))

    (case (dvc-state-data-heads data)
      (need-merge
       (setf (dvc-state-data-conflicts data)
             (dvc-state-conflicts data)))
      (t
       (dvc-save-kill-buffer (dvc-state-data-conflicts-buffer data))
       (dvc-conflicts-clean)
       (setf (dvc-state-data-conflicts data) 'none)))

    (setf (dvc-state-data-need-refresh data) nil))

  ;; return non-nil to refresh display as we go along
  t)

(defun dvc-state-refresh ()
  "Refresh status of each ewoc element. With prefix arg, re-scan for local changes."
  (interactive)
  (ewoc-map 'dvc-state-refresh-one dvc-state-ewoc current-prefix-arg)
  (message "done"))

(defun dvc-state-filter-non-ws (dir)
  "Return a list of all workspaces in DIR (for any backend in `dvc-registered-backends').
List element is '(dvc . path), where path is relative to DIR."
  (let ((default-directory dir)
	(subdirs (directory-files dir)))
    (setq subdirs
          (mapcar (lambda (filename)
                    (and (file-directory-p filename)
			 (not (string= "." filename))
			 (not (string= ".." filename))
			 (dvc-workspace-any-p filename)))
                  subdirs))
    (delq nil subdirs)))

;;;###autoload
(defun dvc-state-multiple (dir &optional workspaces)
  "Show actions to appropriate for all workspaces under DIR relative to local repository."
  (interactive "DStatus for all (root directory): ")
  (pop-to-buffer (get-buffer-create "*dvc-state*"))
  (setq default-directory (file-name-as-directory (expand-file-name (substitute-in-file-name dir))))
  (if (not workspaces) (setq workspaces (dvc-state-filter-non-ws default-directory)))
  (if dvc-state-ewoc
      ;; ewoc previously created in this buffer; erase it. There
      ;; doesn't seem to be a good way to call ewoc-delete; just erase
      ;; the buffer and clobber the variable.
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(setq dvc-state-ewoc nil)))

  (dvc-state-mode)
  (setq dvc-state-root (file-name-as-directory default-directory))
  (setq dvc-state-ewoc (ewoc-create 'dvc-state-printer))
  (ewoc-set-hf dvc-state-ewoc (format "Root : %s\n" dvc-state-root) "")
  (dolist (workspace workspaces)
    (let ((dvc-current-active-dvc (car workspace))
	  (default-directory (concat dvc-state-root (cdr workspace))))
      (ewoc-enter-last dvc-state-ewoc
		       (make-dvc-state-data
			:work (cdr workspace)
			:dvc  (car workspace)
			:branch (dvc-branch)
			:need-refresh t
			:heads 'need-scan
			:update-review 'pending
			:local-changes (list 'need-scan)
			:conflicts 'need-scan))))
  (dvc-state-refresh)
  (goto-char (point-min))
  (dvc-state-next))

;;;###autoload
(defun dvc-state-one (work)
  "Show actions appropriate for workspace WORK relative to local repository."
  (interactive "DStatus for (workspace): ")
  ;; allow WORK to be relative, and ensure it is a workspace root
  (setq work (dvc-tree-root (expand-file-name (substitute-in-file-name work))))
  (pop-to-buffer (get-buffer-create "*dvc-state*"))
  (setq default-directory work)
  (if dvc-state-ewoc
      ;; ewoc previously created in this buffer; erase it. There
      ;; doesn't seem to be a good way to call ewoc-delete; just erase
      ;; the buffer and clobber the variable.
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(setq dvc-state-ewoc nil)))

  (dvc-state-mode);; kills all local variables

  (setq dvc-state-root (expand-file-name (concat (file-name-as-directory default-directory) "../")))
  (setq dvc-state-ewoc (ewoc-create 'dvc-state-printer))
  (ewoc-set-hf dvc-state-ewoc (format "Root : %s\n" dvc-state-root) "")
  (ewoc-enter-last dvc-state-ewoc
                   (make-dvc-state-data
                    :work (file-name-nondirectory (directory-file-name default-directory))
		    :dvc  (dvc-current-active-dvc)
		    :branch (dvc-branch)
                    :need-refresh t
                    :heads 'need-scan
		    :update-review 'pending
		    :local-changes (list 'need-scan)
		    :conflicts 'need-scan))
  (dvc-state-refresh)
  (goto-char (point-min))
  (dvc-state-next))

(provide 'dvc-state)

;; end of file
