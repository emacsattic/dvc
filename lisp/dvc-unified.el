;;; dvc-unified.el --- The unification layer for dvc

;; Copyright (C) 2005-2010, 2012 - 2015 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>

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

;; functions providing a unified interface to the various backends

;;; Code:

(require 'dired-x)
(require 'ffap)
(require 'dvc-register)
(require 'dvc-core)
(require 'dvc-defs)
(require 'dvc-tips)
(require 'dvc-utils)

(defmacro define-dvc-unified-command (name args comment &optional interactive)
  "Define a DVC unified command.  &optional arguments are permitted, but
not &rest."
  (declare (indent 2)
	   (debug (&define name lambda-list stringp
			   [&optional interactive])))
  `(defun ,name ,args
     ,comment
     ,@(when interactive (list interactive))
     (dvc-apply ,(symbol-name name)
                (dvc-remove-optional-args ',args
                                          ,@(remove '&optional args)))))

(define-dvc-unified-command dvc-add (file)
  "Adds FILE to the repository."
  (interactive "fFile: "))

(defun dvc-add-files (&rest files)
  "Add FILES to the currently active dvc. FILES is a list of
strings including path from root; interactive defaults
to (dvc-current-file-list)."
  (interactive (dvc-current-file-list))
  (when (setq files (dvc-confirm-file-op "add" files dvc-confirm-add))
    (dvc-apply "dvc-add-files" files)))

(defun dvc-apply-patch (patch-name)
  "Apply patch `patch-name' on current-tree."
  (interactive (list (read-from-minibuffer "Patch: "
                                     nil nil nil nil
                                     (dired-filename-at-point))))
  (let ((current-dvc (dvc-current-active-dvc)))
    (case current-dvc
      ('xgit (xgit-apply-patch patch-name))
      ('xhg (xhg-import patch-name))
      ;; FIXME: change to use dvc-call
      (t
       (if (y-or-n-p (format "[%s] don't know how to apply patch, do you want to run a generic command instead?"
                             current-dvc))
           (shell-command (format "cat %s | patch -p1" patch-name))
           (message "I don't known yet how to patch on %s" current-dvc))))))

;;;###autoload
(define-dvc-unified-command dvc-changelog (&optional arg)
  "Display the full changelog in this tree for the actual dvc.
Use `dvc-log' for the brief log."
  (interactive))

(define-dvc-unified-command dvc-conflicts-status
    (buffer left-work left-rev right-work right-rev left-branch right-branch)
  "Return '(buffer status), where status is one of 'need-resolve
| 'need-review-resolve-internal | 'resolved | 'none for
BUFFER. Regenerate conflicts if not current. Conflicts stored in
RIGHT-WORK.")

(define-dvc-unified-command dvc-create-squashed-commitp ()
  "Return non-nil if back-end supports creating a squashed commit instead of a merge.")

(defun dvc-dvc-create-squashed-commitp ()
  ;; most backends don't support this
  nil)

(define-dvc-unified-command dvc-create-squashed-commit (from to)
  "Create a squashed commit in the current workspace equivalent to merge FROM TO.
FROM and TO are backend revision ids.")

(define-dvc-unified-command dvc-ignore-local-changesp ()
  "Return non-nil if back-end supports ignoring local changes during update.")

;;;###autoload
(defun dvc-init ()
  "Initialize a new repository.
It currently supports the initialization for bzr, xhg, xgit, tla."
  (interactive)
  (when (interactive-p)
    (let ((supported-variants (map t 'symbol-name dvc-registered-backends))
          (working-dir (dvc-uniquify-file-name default-directory))
          (dvc))
      ;; hide backends that don't provide an init function
      (mapc #'(lambda (elem)
                (setq supported-variants (delete elem supported-variants)))
              '("xdarcs" "xmtn" "baz"))
      (add-to-list 'supported-variants "bzr-repo")
      (setq dvc (intern (dvc-completing-read
                         (format "Init a repository for '%s', using dvc: " working-dir)
                         (sort supported-variants 'string-lessp))))
      (cond ((string= dvc "bzr-repo")
             (call-interactively 'bzr-init-repository))
            (t
             (funcall (dvc-function dvc "dvc-init") working-dir))))))

(define-dvc-unified-command dvc-base-revision ()
  "Return base revision of current workspace.")

(define-dvc-unified-command dvc-branch ()
  "Return the current branch name for current workspace.")

;;;###autoload
(defun dvc-clone (&optional dvc source-path dest-path rev)
  "Ask for the DVC to use and clone SOURCE-PATH."
  (interactive "P")
  (when (interactive-p)
    (let* ((ffap-url-regexp
            (concat
             "\\`\\("
             "\\(ftp\\|https?\\|git\\|www\\)://" ; needs host
             "\\)."                              ; require one more character
             ))
           (url-at-point (ffap-url-at-point))
           (all-candidates (map t 'symbol-name dvc-registered-backends))
           (git-is-candidate (re-search-backward "git clone .+" (line-beginning-position) t))
           (hg-is-candidate (re-search-backward "hg clone .+" (line-beginning-position) t))
           (bzr-is-candidate (re-search-backward "bzr get .+" (line-beginning-position) t)))
      (setq dvc (intern (dvc-completing-read
                         "Clone, using dvc: "
                         all-candidates
                         nil t
                         (cond (git-is-candidate "xgit")
                               (bzr-is-candidate "bzr")
                               (hg-is-candidate "xhg")
                               (t nil)))))
      (setq source-path (read-string (format "%S-clone from path: " dvc) url-at-point))
      (setq dest-path (expand-file-name (dvc-read-directory-name
                                         (format "Destination Directory for %S-clone: " dvc)
                                         nil nil nil "<default>")))
      (if current-prefix-arg
          (unless (not (eq dvc 'xhg))
            (setq rev (read-string "FromRevision: ")))
        nil)))
  (let ((default-directory (or (file-name-directory dest-path) default-directory)))
    (when (string= (file-name-nondirectory dest-path) "<default>")
      (setq dest-path nil))
    (if rev
        (funcall (dvc-function dvc "dvc-clone") source-path dest-path rev)
      (funcall (dvc-function dvc "dvc-clone") source-path dest-path))))

;;;###autoload
(defun dvc-command-version ()
  "Returns and if interactive shows the version identity string of backend command."
  (interactive)
  (let ((version (dvc-call "dvc-command-version")))
    (when (interactive-p)
      (message "%s" version))
    version))

;;;###autoload
(define-dvc-unified-command dvc-conflicts-clean ()
  "Clean conflicts data for current workspace.")

;;;###autoload
(define-dvc-unified-command dvc-create-branch ()
  "Create a new branch.")

;;;###autoload
(define-dvc-unified-command dvc-delta (base modified &optional dont-switch)
  "Display diff from revision BASE to MODIFIED.

BASE and MODIFIED must be full revision IDs, or strings. If
strings, the meaning is back-end specific; it should be some sort
of revision specifier.

The new buffer is always displayed; if DONT-SWITCH is nil, select it."
  (interactive "Mbase revision: \nMmodified revision: "))

;;;###autoload
(defun dvc-diff (&optional base-rev path dont-switch)
  "Display the changes from BASE-REV to the local tree in PATH.

BASE-REV (a revision-id) defaults to base revision of the
tree. Use `dvc-delta' for differencing two revisions.

PATH defaults to `default-directory', that is, the whole working tree.
See also `dvc-file-diff', which defaults to the current buffer file.

The new buffer is always displayed; if DONT-SWITCH is nil, select it."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "DVC diff (directory): "
                                       (when path (expand-file-name path)))))
    (setq base-rev (or base-rev
                       ;; Allow back-ends to override this for e.g. git,
                       ;; which can return either the index or the last
                       ;; revision.
                       (dvc-call "dvc-last-revision")))
    (dvc-save-some-buffers default-directory)
    (dvc-call "dvc-diff" base-rev default-directory dont-switch)))

;;;###autoload
(defun dvc-diff-against-url (path)
  "Show the diff from the current tree against a remote url"
  (interactive)
  (dvc-save-some-buffers default-directory)
  (dvc-call "dvc-diff-against-url" path))

(define-dvc-unified-command dvc-ediff-file-revisions ()
  "Ediff rev1 of file against rev2."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-edit-ignore-files ()
  "Edit the ignored file list."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-export-via-email ()
  "Send the changeset at point via email."
  (interactive))

(define-dvc-unified-command dvc-file-diff (file &optional base modified dont-switch)
  "Display the changes in FILE (default current buffer file)
between rev BASE (default last-revision) and rev
MODIFIED (default workspace version).  If DONT-SWITCH is non-nil,
just show the diff buffer, don't select it."
  ;; use dvc-diff-diff to default file to dvc-get-file-info-at-point
  (interactive (list buffer-file-name)))

(define-dvc-unified-command dvc-heads ()
  "Return a list of revs for the head revisions in the local repository of the branch for the current workspace.
If there are more than two, the two that should be merged together first should be listed first.")

(defun dvc-heads-revlist ()
  "Show a list of revs for the head revisions in the local repository of the branch for current workspace."
  (interactive)
  (dvc-revlist-setup
   (lambda (args)
     (list
      (list
	 (format "workspace %s" default-directory)
	 "Head revisions") ;; header-lines
      nil ;; footer-lines
      (dvc-heads))) ;; generator
   nil ;; generator-args
   nil ;; first-line-only-p
   nil ;; last-n
   ))

(define-dvc-unified-command dvc-ignore-files (file-list)
  "Ignore the marked files."
  (interactive (list (dvc-current-file-list))))

(defun dvc-ignore-file-extensions (file-list)
  "Ignore the file extensions of the marked files, in all
directories of the workspace."
  (interactive (list (dvc-current-file-list)))
  (let* ((extensions (delete nil (mapcar 'file-name-extension file-list)))
         ;; FIXME: should also filter duplicates. use delete-duplicates
         (root (dvc-tree-root))
         (msg (case (length extensions)
                (1 (format "extension *.%s" (first extensions)))
                (t (format "%d extensions" (length extensions))))))
    (if extensions
        (when (y-or-n-p (format "Ignore %s in workspace %s? " msg root))
          (dvc-call "dvc-backend-ignore-file-extensions" extensions))
      (error "No files with an extension selected"))))

(defun dvc-ignore-file-extensions-in-dir (file-list)
  "Ignore the file extensions of the marked files, only in the
directories containing the files, and recursively below them."
  (interactive (list (dvc-current-file-list)))
  (let* ((extensions (mapcar 'file-name-extension file-list))
         (dirs (mapcar 'file-name-directory file-list))
         (msg (case (length extensions)
                (1 (format "extension *.%s in directory `%s'" (first extensions) (or (first dirs) ".")))
                (t (format "%d extensions in directories" (length extensions))))))
    (dolist (extension extensions)
      (if (not extension)
          (error "A file with no extension selected")))
    (when (y-or-n-p (format "Ignore %s? " msg))
      (dvc-call "dvc-backend-ignore-file-extensions-in-dir" file-list))))

;;;###autoload
(define-dvc-unified-command dvc-inventory ()
  "Show the inventory for the current workspace."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-kill-session ()
  "Kill any background session for current workspace.")

(define-dvc-unified-command dvc-last-revision ()
  "Return rev-spec for the most recent revision in the local repository of the branch for current workspace.")

(defun dvc-dvc-last-revision ()
  ;; This is correct for most backends.
  (list (dvc-current-active-dvc)
        (list 'last-revision default-directory 1)))

(define-dvc-unified-command dvc-lca (left right)
  "Return back-end revision id for the least common ancesor of LEFT, RIGHT
(which are back-end revision ids).
If lca cannot be determined, return 'unknown.")

(define-dvc-unified-command dvc-list-branches ()
  "List available branches."
  (interactive))

;;;###autoload
(defun dvc-log (&optional path last-n)
  "Display the brief log for PATH (a file-name; default current
buffer file name; nil means entire tree; negative prefix arg
means prompt for tree depending on value of
dvc-read-project-tree-mode), LAST-N entries (default
`dvc-log-last-n'; all if nil, prefix value means that
many entries (absolute value)). Use `dvc-changelog' for the full log."
  (interactive "i\nP")
  (let* ((dir (cond
	       ((and last-n (< (prefix-numeric-value last-n) 0))
		 nil)
	       (path (file-name-directory (expand-file-name path)))
	       (t default-directory)
	       ))
	 (path (cond
		((and path
		      (string= "" (file-name-nondirectory path)))
		 nil)
		(path
		 (file-name-nondirectory path))
		(t
		 (buffer-file-name))
		))
	 (last-n (if last-n
		     (abs (prefix-numeric-value last-n))
		   dvc-log-last-n))
	 (default-directory
	   (dvc-read-project-tree-maybe "DVC tree root (directory): " dir dir)))
    ;; Since we have bound default-directory, we don't need to pass
    ;; 'root' to the back-end.
    (dvc-call "dvc-log" path last-n))
  nil)

(define-dvc-unified-command dvc-log-edit-file-name ()
  "Return a suitable file name to store the commit message.")

(defun dvc-dvc-log-edit-file-name ()
  "Default for `dvc-log-edit-file-name'; uses dvc-variable `<backend>-log-edit-file-name'."
  (concat (file-name-as-directory (dvc-tree-root))
          (dvc-variable (dvc-current-active-dvc)
                        "log-edit-file-name")))

;;;###autoload
(defun dvc-log-edit (&optional other-frame no-init)
  "Edit the log before commiting. Optional OTHER_FRAME (default
user prefix) is interpreted according to
`dvc-log-edit-other-frame'. Optional NO-INIT if non-nil
suppresses initialization of buffer if one is
reused. `default-directory' must be the tree root."
  (interactive "P")
  (setq other-frame
	(if dvc-log-edit-other-frame
	    (not other-frame)
	  other-frame))

  ;; Reuse an existing log-edit buffer if possible.
  ;;
  ;; If this is invoked from a status or diff buffer,
  ;; dvc-buffer-current-active-dvc is set. If invoked from another
  ;; buffer (ie a source file, either directly or via
  ;; dvc-add-log-entry), dvc-buffer-current-active-dvc is nil, there
  ;; might be two back-ends to choose from, and dvc-current-active-dvc
  ;; might prompt. So we look for an existing log-edit buffer for the
  ;; current tree first, and assume the user wants the back-end
  ;; associated with that buffer (ie, it was the result of a previous
  ;; prompt).
  (let ((log-edit-buffers (dvc-get-matching-buffers dvc-buffer-current-active-dvc 'log-edit default-directory)))
    (case (length log-edit-buffers)
      (0 ;; Need to create a new log-edit buffer. In the log-edit
       ;; buffer, dvc-partner-buffer must be set to a buffer with a
       ;; mode that dvc-current-file-list supports.
       ;; dvc-buffer-current-active-dvc could be nil here, so we have
       ;; to use dvc-current-active-dvc, and let it prompt.
       (let* ((dvc-temp-current-active-dvc (dvc-current-active-dvc))
              (diff-status-buffers
               (append (dvc-get-matching-buffers dvc-temp-current-active-dvc 'diff default-directory)
                       (dvc-get-matching-buffers dvc-temp-current-active-dvc 'status default-directory)
                       (dvc-get-matching-buffers dvc-temp-current-active-dvc 'conflicts default-directory)))
             (activated-from-bookmark-buffer (eq major-mode 'dvc-bookmarks-mode)))
         (case (length diff-status-buffers)
           (0 (if (not activated-from-bookmark-buffer)
                  (error "Must have a DVC diff, status, or conflict buffer before calling dvc-log-edit")
                (dvc-call "dvc-log-edit" (dvc-tree-root) other-frame nil)))
           (1
            (set-buffer (nth 1 (car diff-status-buffers)))
            (dvc-call "dvc-log-edit" (dvc-tree-root) other-frame nil))

           (t ;; multiple: choose current buffer
            (if (memq (current-buffer)
                      (mapcar #'(lambda (item) (nth 1 item))
                              diff-status-buffers))
                (dvc-call "dvc-log-edit" (dvc-tree-root) other-frame nil)

              ;; give up. IMPROVEME: could prompt
              (if dvc-buffer-current-active-dvc
                  (error "More than one diff, status, or conflict buffer for %s in %s; can't tell which to use. Please close some."
                         dvc-buffer-current-active-dvc default-directory)
                (error "More than one diff, status, or conflict buffer for %s; can't tell which to use. Please close some."
                       default-directory)))))))

      (1 ;; Just reuse the buffer. In this call, we can't use
       ;; dvc-buffer-current-active-dvc from the current buffer,
       ;; because it might be nil (if we are in a source buffer). We
       ;; want to use dvc-buffer-current-active-dvc from that buffer
       ;; for this dvc-call, but we can't switch to it first,
       ;; because dvc-log-edit needs the current buffer to set
       ;; dvc-partner-buffer.
       (let ((dvc-temp-current-active-dvc
              (with-current-buffer (nth 1 (car log-edit-buffers)) dvc-buffer-current-active-dvc)))
         (dvc-call "dvc-log-edit" (dvc-tree-root) other-frame no-init)))

      (t ;; multiple matching buffers
       (if dvc-buffer-current-active-dvc
           (error "More than one log-edit buffer for %s in %s; can't tell which to use. Please close some."
                  dvc-buffer-current-active-dvc default-directory)
         (error "More than one log-edit buffer for %s; can't tell which to use. Please close some."
                default-directory))))))

;;;###autoload
(define-dvc-unified-command dvc-log-edit-done (&optional arg)
  "Commit and close the log buffer.  Optional ARG (default user prefix) is back-end specific."
  (interactive (list current-prefix-arg)))

;;;###autoload
(define-dvc-unified-command dvc-merge (&optional other)
  "Merge with OTHER.
If OTHER is nil, merge heads in current database, or merge from
remembered database.
If OTHER is a string, it identifies a (local or remote) database or
branch to merge into the current database, branch, or workspace."
  (interactive))

;;;###autoload
(defun dvc-missing (&optional path use-current)
  "Show revisions in local repository missing from workspace PATH.
If USE-CURRENT is non-nil (default user prefix arg), PATH defaults to current tree.
Otherwise PATH defaults to prompt."
  (interactive `(nil ,current-prefix-arg))
  (let ((default-directory
          (dvc-read-project-tree-maybe "DVC missing (directory): "
                                       (when path (expand-file-name path))
                                       use-current)))
    ;; Since we have bound default-directory, we don't need to pass
    ;; `path' to the back-end.
    (dvc-save-some-buffers default-directory)
    (dvc-call "dvc-missing"))
  nil)

(define-dvc-unified-command dvc-name-construct (back-end-revision)
  "Returns a string representation of BACK-END-REVISION.")

(define-dvc-unified-command dvc-parse-sync ()
  "Parse output of a sync command in current workspace
 (saved in backend-specific file), add to `dvc-sync-save-file'.")

(define-dvc-unified-command dvc-propagate (from from-rev to)
  "Propagate branch FROM rev FROM-REV to branch TO.
FROM-REV should be the head of FROM.
Conflict resolution taken from `default-directory', which must be
a workspace for TO.")

;;;###autoload
(defun dvc-pull (&optional work)
  "Run backend pull, using defaults from workspace WORK (default prompt).
Append list of revs pushed/pulled to `dvc-sync-save-file'."
  (interactive)
  (let ((default-directory
	  (dvc-read-project-tree-maybe "DVC tree root (directory): "
				       (when work (expand-file-name work))
				       work)))
    (when (buffer-live-p (get-buffer "*dvc-sync-review*"))
      ;; save current work, so sync-run/sync-review can add to it
      (kill-buffer "*dvc-sync-review*"))
    (dvc-call "dvc-pull"))
)

;;;###autoload
(defun dvc-push (&optional work)
  "Run backend push, using defaults from workspace WORK (default prompt).
Append list of revs pushed/pulled to `dvc-sync-save-file'."
  (interactive)
  (let ((default-directory
	  (dvc-read-project-tree-maybe "DVC tree root (directory): "
				       (when work (expand-file-name work))
				       work)))
    (when (buffer-live-p (get-buffer "*dvc-sync-review*"))
      ;; save current work, so sync-run/sync-review can add to it
      (kill-buffer "*dvc-sync-review*"))
    (dvc-call "dvc-push"))
)

(define-dvc-unified-command dvc-rebasep ()
  "Return non-nil if back-end supports rebase.
Rebase is an alternative to merge; change commits on one branch
to be after commits on another branch, rather than producing a
branched commit graph.")

(defun dvc-dvc-rebasep ()
  ;; most backends do not support this
  nil)

(define-dvc-unified-command dvc-rebase (&optional upstream)
  "Rebase the current workspace from UPSTREAM (default current branch upstream)."
  (interactive))

;;;###autoload
(defun dvc-remove-files (&rest files)
  "Remove FILES for the currently active dvc.
Return t if files removed, nil if not (due to user confirm or error)."
  (interactive (dvc-current-file-list))
  (when (setq files (dvc-confirm-file-op "remove" files t))
    (dvc-apply "dvc-remove-files" files)))

(defun dvc-rename (from-name to-name)
  "Rename file FROM-NAME to TO-NAME; TO-NAME may be a directory.
When called non-interactively, if from-file-name does not exist,
but to-file-name does, just record the rename in the back-end"
  ;; back-end function <dvc>-dvc-rename (from-name to-name bookkeep-only)
  ;; If bookkeep-only nil, rename file in filesystem and back-end
  ;; If non-nil, rename file in back-end only.
  (interactive
   (let* ((from-name (dvc-confirm-read-file-name "Rename: " t))
          (to-name (dvc-confirm-read-file-name
                    (format "Rename %s to: " from-name)
                    nil "" from-name)))
     (list from-name to-name)))

  (if (file-exists-p from-name)
      (progn
        ;; rename the file in the filesystem and back-end
        (if (and (file-exists-p to-name)
                 (not (file-directory-p to-name)))
            (error "%s exists and is not a directory" to-name))
        (when (file-directory-p to-name)
          (setq to-name (file-name-as-directory to-name)))
        (dvc-call "dvc-rename" from-name to-name nil))

    ;; rename the file in the back-end only
    (progn
      ;; rename the file in the filesystem and back-end
      (if (not (file-exists-p to-name))
          (error "%s does not exist" to-name))
      (when (file-directory-p to-name)
        (setq to-name (file-name-as-directory to-name)))
      (dvc-call "dvc-rename" from-name to-name t))))

(define-dvc-unified-command dvc-resolved (file)
  "Mark FILE as resolved"
  (interactive (list (buffer-file-name))))

(defun dvc-revert-files (&rest files)
  "Revert FILES for the currently active dvc."
  (interactive (dvc-current-file-list))
  (when (setq files (dvc-confirm-file-op "revert" files t))
    (dvc-apply "dvc-revert-files" files)))

(define-dvc-unified-command dvc-revision-direct-ancestor (revision)
  "Return the direct ancestor of a revision.")

(define-dvc-unified-command dvc-revision-nth-ancestor (revision n)
  "Computes the direct ancestor of a revision.")

(define-dvc-unified-command dvc-revlist-entry (rev)
  "Return `dvc-revlist-entry' struct for REV (a back-end revision id).")

(define-dvc-unified-command dvc-save-diff (file)
  "Store the diff from the working copy of FILE (default prompt)
against the last revision in the local repository in a
file (output file name is backend specific)."
  (interactive (list (read-file-name "Save the diff to: "))))

(define-dvc-unified-command dvc-stagep ()
  "Non-nil if stage operations are supported")

(defun dvc-dvc-stagep()
  "Default for `dvc-stagep'."
  nil)

(defun dvc-stage-files (files)
  "Stage the FILES."
  ;; FILES is already a list; don't call any &rest functions that will
  ;; nest the list. So don't use 'dvc-call' or 'dvc-apply'.
  (funcall (dvc-function (dvc-current-active-dvc) "dvc-stage-files") files))

(defun dvc-sync-run (&optional work)
  "Run backend sync, using defaults from workspace WORK (default prompt).
Append list of revs pushed/pulled to `dvc-sync-save-file'."
  (interactive)
  (let ((default-directory
	  (dvc-read-project-tree-maybe "DVC tree root (directory): "
				       (when work (expand-file-name work))
				       work)))
    (when (buffer-live-p (get-buffer "*dvc-sync-review*"))
      ;; save current work, so sync-run/sync-review can add to it
      (kill-buffer "*dvc-sync-review*"))
    (dvc-call "dvc-sync-run"))
)

(defun dvc-unstage-files (files)
  "Unstage the FILES."
  ;; FILES is already a list; don't call any &rest functions that will
  ;; nest the list. So don't use 'dvc-call' or 'dvc-apply'.
  (funcall (dvc-function (dvc-current-active-dvc) "dvc-unstage-files") files))

(define-dvc-unified-command dvc-stashp ()
  "Non-nil if stash operations are supported")

(define-dvc-unified-command dvc-stash-drop ()
  "Drop the top stash.")

(define-dvc-unified-command dvc-stash-pop ()
  "Apply the top stash, pop the stash stack.")

(define-dvc-unified-command dvc-stash-save ()
  "Save non-staged workspace changes to the stash stack.")

(define-dvc-unified-command dvc-stash-show ()
  "Show the changes in the top stash.")

(defun dvc-select-branch ()
  "Select a branch."
  (interactive)
  (call-interactively (dvc-function (dvc-current-active-dvc) "dvc-select-branch")))

(define-dvc-unified-command dvc-send-commit-notification (&optional to)
  "Send a commit notification for the changeset at point.
If TO is provided, send it to that email address.  If a prefix
argument is given, modify the behavior of this command as
specified by the VCS backend."
  (interactive (list current-prefix-arg)))

;;;###autoload
(defun dvc-status (&optional path no-switch)
  "Display the status of all files in optional PATH tree,
default current workspace.
If NO-SWITCH is non-nil, don't show status buffer, just prepare it.
Return '(buffer status), where `status' is a list of:
'ok               no local changes or stashes
'need-commit      local changes can be committed
'need-stash-save  local changes can be stashed
'need-stash-apply stashed local changes can be re-applied."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "DVC status (directory): "
                                       (when path (expand-file-name path)) (not current-prefix-arg))))
    ;; Since we have bound default-directory, we don't need to pass
    ;; `path' to the back-end.
    (dvc-save-some-buffers default-directory)
    (dvc-call "dvc-status" no-switch)))

;;;###autoload
(define-dvc-unified-command dvc-submit-patch ()
  "Submit a patch for the current project under DVC control."
  (interactive))

;;;###autoload
(defun dvc-tree-root (&optional path no-error)
  "Get the tree root for PATH (default `default-directory').

When called interactively, print a message including the tree root and
the current active back-end."
  (interactive)
  (let ((dvc-list (or
                   (when dvc-temp-current-active-dvc (list dvc-temp-current-active-dvc))
                   (when dvc-buffer-current-active-dvc (list dvc-buffer-current-active-dvc))
                   (append dvc-select-priority dvc-registered-backends)))
        (root "/")
        (dvc)
        (tree-root-func)
        (path (or path default-directory)))
    (while dvc-list
      (setq tree-root-func (dvc-function (car dvc-list) "tree-root" t))
      (when (fboundp tree-root-func)
        (let ((current-root (funcall tree-root-func path t)))
          (when (and current-root (> (length current-root) (length root)))
            (setq root current-root)
            (setq dvc (car dvc-list)))))
      (setq dvc-list (cdr dvc-list)))
    (when (string= root "/")
      (unless no-error (error "Tree %s is not under version control"
                              path))
      (setq root nil))
    (when (interactive-p)
      (message "Root: %s (managed by %s)"
               root (dvc-variable dvc "backend-name")))
    root))

;;;###autoload
(define-dvc-unified-command dvc-update (&optional revision-id)
  "Update the current workspace to REVISION-ID
(a back-end id; default head of current branch)."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-update-review ()
  "Show log of revisions in last update of current workspace.
Returns buffer displaying revision list."
  (interactive))

;; unified function: dvc-workspace-p (dir)
;; return non-nil if dir is a workspace for the dvc

(defun dvc-workspace-any-p (dir)
  "Return '(dvc DIR) if DIR is a workspace for backend dvc.
Tries all backends in `dvc-registered-backends'."
  (let (dvc
	(result nil)
	(backends dvc-registered-backends))
    (while (and (setq dvc (pop backends))
		(not result))
      (when (funcall (dvc-function dvc "dvc-workspace-p") dir)
	(setq result (cons dvc dir))))
    result))

(provide 'dvc-unified)

;;; dvc-unified.el ends here
