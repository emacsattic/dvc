;;; dvc-sync.el --- remote repository sync/push/pull handling for DVC
;;
;; Copyright (C) 2014-2015 Stephen Leake
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

(eval-when-compile (require 'cl-macs))

(require 'dvc-config)

;;; User variables
(defvar dvc-sync-guess-workspace nil
  "User-supplied function to guess workspace location.
Called with a string containing the backend branch name; return a workspace root or nil.")

;;; Internal global variables
(defconst dvc-sync-save-file "sync"
  "File to save sync review state; relative to `dvc-config-directory'.
Format is a list of `dvc-sync-branch' items.")

(defconst dvc-sync-branch-file "branches"
  "File containing `dvc-sync-branch-alist'; relative to `dvc-config-directory'.
Used by `dvc-sync-guess-workspace'.")

(defvar dvc-sync-branch-alist nil
  "Alist associating repository/branch name with workspace root.")

;; Internal buffer-local variables
(defvar-local dvc-sync-ewoc nil
  "ewoc for displaying sync results.
All dvc-sync functions operate on this ewoc.
The elements must all be of type dvc-sync-branch.")

(cl-defstruct (dvc-sync-branch
            (:copier nil))
  ;; ewoc element; data for a branch that was received
  workspace  ;; absolute path to workspace used to access the repository
  repository ;; remote url or local path of repository containing branch
  name       ;; backend branch name
  revs       ;; list of '(revid date author changelog) for received revs on branch
  send-count ;; integer count of sent revs on branch
  print-mode ;; 'summary | 'brief | 'full
  state      ;; 'need-review | 'started | 'need-clean
  )

(defun dvc-sync-print-rev (rev print-mode)
  "Print a REV (element of dvc-sync-branch-revs) according to PRINT-MODE ('brief or 'full)."
  (let ((date (nth 1 rev))
	(author (nth 2 rev))
	(changelog (nth 3 rev)))
    (insert (dvc-face-add (format "\n   %s %s\n" date author) 'dvc-header))
    (ecase print-mode
      (brief
       (insert (substring changelog 0 (string-match "\n" changelog)))
       (newline))
      (full
       (insert changelog)
       (newline)))))

(defun dvc-sync-printer (branch)
  "Print an ewoc element; BRANCH must be of type dvc-sync-branch."
  ;; Backend may return a revision with no branch, when that revision
  ;; was committed on a branch that is not part of this sync.
  (when (dvc-sync-branch-repository branch)
    (insert (dvc-face-add (dvc-sync-branch-repository branch) 'dvc-keyword))
    (newline))

  (if (dvc-sync-branch-name branch)
      (insert (dvc-face-add (dvc-sync-branch-name branch) 'dvc-keyword))
    (insert (dvc-face-add "<no branch>" 'dvc-keyword)))

  (insert (format " rx %d tx %d\n"
		  (length (dvc-sync-branch-revs branch))
		  (dvc-sync-branch-send-count branch)))
  (ecase (dvc-sync-branch-print-mode branch)
    (summary nil)

    ((brief full)
     (cl-loop for rev in (dvc-sync-branch-revs branch) do
	(dvc-sync-print-rev rev (dvc-sync-branch-print-mode branch)))
     (newline))
    )

  (ecase (dvc-sync-branch-state branch)
    (need-review
     (insert (dvc-face-add " need review\n" 'dvc-header)))

    (started
     (insert (dvc-face-add " started\n" 'dvc-header)))

    (need-clean
     (insert (dvc-face-add " need clean\n" 'dvc-header)))

    )
  )

(defun dvc-sync-cycle-display ()
  "Set display mode for current item to next of {brief, full, summary}."
  (interactive)
  (let* ((elem (ewoc-locate dvc-sync-ewoc))
	 (data (ewoc-data elem)))
    (ecase (dvc-sync-branch-print-mode data)
      (brief
       (setf (dvc-sync-branch-print-mode data) 'full))
      (full
       (setf (dvc-sync-branch-print-mode data) 'summary))
      (summary
       (setf (dvc-sync-branch-print-mode data) 'brief))
      )

    (ewoc-invalidate dvc-sync-ewoc elem)))

(defun dvc-sync-get-branch-alist ()
  "Read `dvc-sync-branch-alist' from user file."
  (unless dvc-sync-branch-alist
    (let ((branch-file (expand-file-name dvc-sync-branch-file dvc-config-directory)))
      (when (file-exists-p branch-file)
	(load branch-file))))
  )

(defun dvc-sync-get-workspace (repo branch guess)
  "Return an absolute path to the workspace for REPO BRANCH."
  (let ((repo-branch (if repo
			 (concat repo "|" branch)
		       branch)))
    (dvc-sync-get-branch-alist)
    (or
     (cadr (assoc repo-branch dvc-sync-branch-alist))
     (read-directory-name
      (format "workspace root for %s %s: " repo branch)
      default-directory
      (or guess
	  (when dvc-sync-guess-workspace
	    (funcall dvc-sync-guess-workspace repo-branch))))
     )))

(defun dvc-sync-save-workspace (work repo branch)
  "Save WORK in `dvc-sync-branch-alist' for REPO BRANCH."
  (let ((repo-branch (if repo
			 (concat repo "|" branch)
		       branch)))
    (unless (assoc repo-branch dvc-sync-branch-alist)
      (push (list repo-branch work) dvc-sync-branch-alist)
      (dvc-save-state
       (list 'dvc-sync-branch-alist)
       (expand-file-name dvc-sync-branch-file dvc-config-directory)))
    ))

(defun dvc-sync-status ()
  "Start dvc-state-one for current ewoc element."
  (interactive)
  (let* ((elem (ewoc-locate dvc-sync-ewoc))
	 (data (ewoc-data elem))
	 (repo (dvc-sync-branch-repository data))
         (branch (dvc-sync-branch-name data))
         (work (dvc-sync-get-workspace repo branch (dvc-sync-branch-workspace data))))
    (setf (dvc-sync-branch-state data) 'started)
    (ewoc-invalidate dvc-sync-ewoc elem)

    (condition-case err
	(progn
	  (dvc-state-one work)
	  ;; don't save the workspace association until it is validated by dvc-state-one
	  (dvc-sync-save-workspace work repo branch))
      ('error
       ;; user or dvc-sync-guess-workspace gave an invalid workspace; prompt and try again
       (setq work (read-directory-name (format "workspace root for %s %s: " repo branch)))
       (dvc-state-one work)))

    ))

(defun dvc-sync-update ()
  "Start dvc-status-one for current ewoc element, do update if possible."
  (interactive)
  (dvc-sync-status)
  (if (dvc-state-updatep)
      (dvc-state-update)))

(defun dvc-sync-update-preview ()
  "Start dvc-status-one for current ewoc element, do update preview if possible."
  (interactive)
  (dvc-sync-status)
  (dvc-state-update-preview))

(defun dvc-sync-clean ()
  "Clean and delete current ewoc element."
  (interactive)
  (let ((elem (ewoc-locate dvc-sync-ewoc))
	(status-buffer (get-buffer "*dvc-state*"))
	(inhibit-read-only t))
    (when (buffer-live-p status-buffer)
      (kill-buffer status-buffer))
    (ewoc-delete dvc-sync-ewoc elem)))

(dvc-make-ewoc-next dvc-sync-next dvc-sync-ewoc)
(dvc-make-ewoc-prev dvc-sync-prev dvc-sync-ewoc)

(defun dvc-sync-rx-p ()
  "Non-nil if current element has non-zero rx rev count."
  (let* ((elem (ewoc-locate dvc-sync-ewoc))
	 (data (ewoc-data elem)))
    (< 0 (length (dvc-sync-branch-revs data)))))

(defvar dvc-sync-kbd-map
  (let ((map (make-sparse-keymap "action")))
    ;; last defined is first in displayed menu
    (define-key map [?c]  '(menu-item "c) clean" dvc-sync-clean))
    (define-key map [?d]  '(menu-item "d) cycle display" dvc-sync-cycle-display :visible (dvc-sync-rx-p)))
    (define-key map [?s]  '(menu-item "s) status" dvc-sync-status :visible (dvc-sync-rx-p)))
    (define-key map [?p]  '(menu-item "p) preview update" dvc-sync-update-preview :visible (dvc-sync-rx-p)))
    (define-key map [?u]  '(menu-item "u) update" dvc-sync-update :visible (dvc-sync-rx-p)))
    map)
  "Keyboard menu keymap used in `dvc-sync-mode'.")

(defvar dvc-sync-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" dvc-sync-kbd-map)
    (define-key map [?d]  'dvc-sync-cycle-display)
    (define-key map [?c]  'dvc-sync-clean)
    (define-key map [?n]  'dvc-sync-next)
    (define-key map [?p]  'dvc-sync-prev)
    (define-key map [?q]  'dvc-buffer-quit)
    (define-key map [?s]  'dvc-sync-status)
    (define-key map [?u]  'dvc-sync-update)
    (define-key map [?S]  'dvc-sync-save)
    map)
  "Keymap used in `dvc-sync-mode'.")

(easy-menu-define dvc-sync-mode-menu dvc-sync-mode-map
  "`dvc-sync' menu"
  `("DVC-sync"
    ;; first item is top in display
    ["Status"        dvc-sync-status t]
    ["Update"        dvc-sync-update t]
    ["Cycle display" dvc-sync-cycle-display t]
    ["Clean/delete"  dvc-sync-clean t]
    ["Save"          dvc-sync-save t]
    ["Save and Quit" (lambda () (kill-buffer (current-buffer))) t]
    ))

(define-derived-mode dvc-sync-mode fundamental-mode "dvc-sync"
  "Major mode to specify conflict resolutions."
  (setq dvc-sync-ewoc (ewoc-create 'dvc-sync-printer))
  (setq dvc-buffer-refresh-function nil)
  (dvc-install-buffer-menu)
  (add-hook 'kill-buffer-hook 'dvc-sync-save nil t)
  (buffer-disable-undo)
  (dvc-sync-get-branch-alist)
  (dvc-sync-load-file)
  )

(defun dvc-sync-load-file ()
  "Add contents of `dvc-sync-save-file' to current ewoc."
  (let ((save-file (expand-file-name dvc-sync-save-file dvc-config-directory))
	stuff)
    (when (file-exists-p save-file)
      (load save-file)
      (setq buffer-read-only nil)
      (dolist (data stuff) (ewoc-enter-last dvc-sync-ewoc data))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))))

(defun dvc-sync-save ()
  "Save `dvc-sync-ewoc' in `dvc-sync-save-file' for later review."
  (interactive)
  (let ((save-file (expand-file-name dvc-sync-save-file dvc-config-directory))
	stuff)
    ;; Directly saving the ewoc doesn't work; too complicated for
    ;; pp-to-string. So we turn the ewoc into a simpler list of data
    ;; items
    (ewoc-map
     (lambda (data)
       (setq stuff (add-to-list 'stuff data t))
       nil)
     dvc-sync-ewoc)

    (dvc-save-state
     (list 'stuff)
     (expand-file-name dvc-sync-save-file dvc-config-directory))))

;;;###autoload
(defun dvc-sync-review (&optional work)
  "Display results from previous pull/push/sync stored in `dvc-sync-save-file'.
If WORK is non-nil, add new results from there first. Otherwise
prompt for new results to add. This accomodates running external sync operations."
  (interactive)
  (let* ((new-results-str
	  (or work
	      (read-directory-name "new data from (backend or workspace): " default-directory "" nil "")))
	 (new-results-sym (intern-soft new-results-str))
	 (new-results-work (expand-file-name new-results-str))
	 (bufname "*dvc-sync-review*")
	 (need-refresh nil))

    (when (buffer-live-p (get-buffer bufname))
      ;; save changes before parsing new results
      (with-current-buffer bufname
	(dvc-sync-save)))

    (cond
     ((= 0 (length new-results-str))
      ;; no new results
      nil)

     (new-results-sym
      ;; a backend
      (setq need-refresh t)
      (let ((dvc-temp-current-active-dvc new-results-sym))
	(dvc-parse-sync)))

     ((dvc-workspace-any-p new-results-work)
      ;; a workspace
      (setq need-refresh t)
      (let ((default-directory new-results-work))
	(dvc-parse-sync)))
     )

    (if (buffer-live-p (get-buffer bufname))
	(progn
	  (pop-to-buffer (get-buffer bufname))
	  (when need-refresh
	    (let ((status-buffer (get-buffer "*dvc-state*"))
		  (inhibit-read-only t))
	      (when (buffer-live-p status-buffer)
		(kill-buffer status-buffer))

	    (ewoc-map (lambda (elem) (ewoc-delete dvc-sync-ewoc elem)) dvc-sync-ewoc)
	    (dvc-sync-load-file)
	    )))
      ;; else create
      (pop-to-buffer (get-buffer-create bufname))
      (setq buffer-read-only nil)
      (dvc-sync-mode))
  ))

;; parsing utilities

(defun dvc-sync-enter-rev (revs workspace repo branch revid date author changelog direction)
  "Add data for REVID to REVS. DIRECTION is one of {'receive 'send}.
Return new revs."
  (let (old-branch)
    (when revs
      (mapc
       (lambda (data)
	 (if (and (string= repo (dvc-sync-branch-repository data))
		  (string= branch (dvc-sync-branch-name data)))
	     ;; already some data for repo|branch; assume same workspace
	     (let ((revs (dvc-sync-branch-revs data)))
	       (ecase direction
		 ('receive
		  (setf (dvc-sync-branch-revs data)
			;; sync sends revs newest first, we want newest
			;; displayed last, so append to head of list
			(push (list revid date author changelog) revs)))
		 ('send
		  (setf (dvc-sync-branch-send-count data) (+ 1 (dvc-sync-branch-send-count data)))))
	       (setq old-branch t)
	       )))
       revs))

    (when (not old-branch)
      (add-to-list
       'revs
       (ecase direction
	 ('receive
	  (make-dvc-sync-branch
	   :workspace workspace
	   :repository repo
	   :name branch
	   :revs (list (list revid date author changelog))
	   :send-count 0
	   :print-mode 'summary
	   :state 'need-review))
	 ('send
	  (make-dvc-sync-branch
	   :repository repo
	   :name branch
	   :revs nil
	   :send-count 1
	   :print-mode 'summary
	   :state 'need-clean))))
      ))
  revs)


(provide 'dvc-sync)

;; end of file
