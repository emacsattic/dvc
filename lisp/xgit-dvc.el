;;; xgit-dvc.el --- The dvc layer for git

;; Copyright (C) 2006-2009, 2013 - 2015 by all contributors

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

;; This file provides the common dvc layer for git

;;; Code:

(eval-when-compile (require 'cl-macs))
(require 'xgit)
(require 'dvc-sync)

;;;###autoload
(dvc-register-dvc 'xgit "git")

(defcustom xgit-commit-check nil
  "Function to check commit message.
Function is called with no parameters, with the log-edit buffer current.

Function should return nil, or signal an error."
  :type '(choice (function)
                 (const :tag "None" nil))
  :group 'dvc-xgit)

(defconst xgit-sync-log-file "git_fetch.log"
  "File to save fetch output for input by `dvc-parse-sync'; relative to `dvc-config-directory'.")

;;;###autoload
(defalias 'xgit-dvc-tree-root 'xgit-tree-root)

;;;###autoload
(defalias 'xgit-dvc-command-version 'xgit-command-version)

(defalias 'xgit-dvc-delta 'xgit-delta)

(defun xgit-rev-parse (ref)
  "Run 'git rev-parse REF', return result as string."
  (dvc-run-dvc-sync
   'xgit (list "rev-parse" ref)
   :finished
   (lambda (output error status arguments)
     (with-current-buffer output
       (buffer-substring-no-properties (point-min) (1- (point-max)))))
   :error
   (lambda (output error status arguments)
     ;; internal use only; callers should handle this error. Don't
     ;; show error buffer to user.
     (error "rev-parse %s failed" ref))))

(defun xgit-config-get (name)
  "Run 'git config --get NAME', return result as string or nil."
  (dvc-run-dvc-sync
   'xgit (list "config" "--get" name)
   :finished
   (lambda (output error status arguments)
     (with-current-buffer output
       (buffer-substring-no-properties (point-min) (1- (point-max)))))
   :error
   (lambda (output error status arguments)
     (with-current-buffer error
       (if (= (point-min) (point-max))
	   ;; just a newline; NAME not defined in config
	   nil
	 (error "config --get failed")))
     )
   ))

(defun xgit-dvc-base-revision ()
  "For `dvc-base-revision'."
  ;; in git, HEAD is the base revision of the current workspace
  (xgit-rev-parse "HEAD"))

(defun xgit-dvc-conflicts-clean ()
  "For `dvc-conflicts-clean'."
  ;; no conflicts data to clean
  nil)

(defun xgit-dvc-create-squashed-commitp ()
  t)

(defun xgit-dvc-create-squashed-commit (from to)
  "For `dvc-create-squashed-commit'."
  (dvc-run-dvc-sync 'xgit (list "merge" "--squashed" from to))
  )

(defun xgit-dvc-lca (rev1 rev2)
  "For `dvc-lca'"
  (dvc-run-dvc-sync
   'xgit
   (list "merge-base" rev1 rev2)
   :finished
   (lambda (output error status arguments)
     (with-current-buffer output
       (buffer-substring-no-properties (point-min) (1- (point-max)))))
   :error
   (lambda (output error status arguments)
     (with-current-buffer error
       (when (looking-at "fatal: Not a valid commit name")
	 ;; need pull or push to get revisions in same repository
	 'unknown)))
   ))

(defun xgit-dvc-upstream-head (branch)
  "Return revid for upstream head of BRANCH.
If BRANCH is local, return that head.
If BRANCH is unknown, return nil."
  (if (string= "(no branch)" branch)
      nil

    (let ((remote (xgit-config-get (concat "branch." branch ".remote"))))
      (cond
       (remote (xgit-rev-parse (concat "refs/remotes/" remote "/" branch)))
       (t
	;; branch not defined in config; must be a local only branch
	;; FIXME: better to declare "no upstream for this branch"?
	(xgit-rev-parse (concat "refs/heads/" branch)))
       ))))

(defun xgit-dvc-heads ()
  "For `dvc-heads'."
  ;; four cases:
  ;;
  ;; 1) last local commit is head: HEAD
  ;;
  ;;    child of remote head
  ;;
  ;; 2) last fetch of current branch is head: git config --get branch.<branch>.merge
  ;;
  ;;    child of HEAD
  ;;
  ;; 3) both of the above
  ;;
  ;;    siblings
  ;;
  ;; 4) no remote: HEAD
  ;;
  ;;    rev-parse of refs/remotes fails
  ;;
  ;; git merge-base HEAD refs/remotes/origin/HEAD
  ;;  = HEAD  = refs/remotes/origin/HEAD -> up-to-date, no action; HEAD or refs/remotes/origin/HEAD
  ;;  = HEAD /= refs/remotes/origin/HEAD -> need update; refs/remotes/origin/HEAD
  ;; /= HEAD  = refs/remotes/origin/HEAD -> need push; HEAD
  ;; /= HEAD /= refs/remotes/origin/HEAD -> need merge; '(HEAD refs/remotes/origin/HEAD)
  ;;
  ;; need tests!
  ;;
  ;; in git 1.8.0, use --is-ancestor

  (let* ((head (xgit-rev-parse "HEAD"))
	 (branch (xgit-dvc-branch))
	 (upstream-head (xgit-dvc-upstream-head branch))
	 (merge-base
	  (when upstream-head
	    (xgit-dvc-lca head upstream-head)))
	 )
    (if (not upstream-head)
	(list head)
      (if (string-equal merge-base head)
	  (list upstream-head)

	(if (string-equal merge-base upstream-head)
	    (list head)
	  (list head upstream-head))))
    ))

(defun xgit-dvc-ignore-local-changesp ()
  nil)

(defun xgit-dvc-kill-session ()
  "For `dvc-kill-session'."
  nil)

(defun xgit-warn-unstaged (action)
  "If any files are changed relative to the index, prompt to confirm ACTION."
  (let ((unstaged nil))
    (dvc-run-dvc-sync
     'xgit (list "status" "--porcelain")
     :finished
     (lambda (output error status arguments)
       (with-current-buffer output
	 (let* ((temp (xgit-parse-status-1))
		(status-list (cdr temp)))
	   (dolist (elem status-list)
	     (when (not (plist-get elem ':indexed))
	       (setq unstaged t)))
	 ))))

    (when
	(and unstaged
	     (not (yes-or-no-p (format "unstaged files; really %s? " action))))
      (error "unstaged files; not doing %s." action))
    ))

(defun xgit-dvc-log-edit-done ()
  "Finish a commit for git, using git commit."
  ;; can only be invoked in the log-edit buffer
  (let ((buffer (find-file-noselect (dvc-log-edit-file-name)))
        (files-to-commit (when (buffer-live-p dvc-partner-buffer)
                           (with-current-buffer dvc-partner-buffer
                             (dvc-current-file-list 'nil-if-none-marked))))
	)
    (save-buffer)
    (dvc-save-some-buffers default-directory)

    ;; check that the first line says something; it should be a summary of the rest
    (goto-char (point-min))
    (forward-line)
    (if (= (point) (1+ (point-min)))
        (error "Please put a summary comment on the first line"))

    ;; workspace-specific check
    (when xgit-commit-check (funcall xgit-commit-check))

    (unless files-to-commit
      (with-current-buffer dvc-partner-buffer (xgit-warn-unstaged "commit")))
    (message "committing %S in %s" (or files-to-commit "all files")
             (dvc-tree-root))
    (dvc-run-dvc-sync
     'xgit (append (list "commit"
                         "-F" (dvc-log-edit-file-name))
                   files-to-commit)
     :finished (lambda (output error status arguments)
                 (dvc-show-error-buffer output 'commit)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert (with-current-buffer error
                             (buffer-string))))
                 (dvc-diff-clear-buffers
		  'xgit default-directory
		  "* Just committed! Please refresh buffer\n"
		  ;; FIXME: factor out xgit-status-header, use it here
		  )
		 (kill-buffer (current-buffer))
                 (message "git commit finished")))
    ))

(defalias 'xgit-dvc-revlog-get-revision 'xgit-revlog-get-revision)

(defalias 'xgit-dvc-name-construct 'xgit-name-construct)

(defun xgit-dvc-changelog (&optional arg)
  "Shows the changelog in the current git tree.
ARG is passed as prefix argument"
  (call-interactively 'xgit-log))

(defalias 'xgit-dvc-prepare-environment 'xgit-prepare-environment)

(defun xgit-dvc-rebasep ()
  "For `dvc-rebasep'."
  t)

(defun xgit-dvc-rebase (&optional upstream)
  "For `dvc-rebase'."
  ;; if the rebase requires merges, git relies on shell
  ;; interaction, that just doesn't work here. So punt the user
  ;; to the shell.
  (error "run 'git rebase %s' in a shell in the local workspace. Use dvc-status to resolve conflicts." upstream)
  )

(defalias 'xgit-dvc-revision-get-last-revision 'xgit-revision-get-last-revision)

(defun xgit-dvc-last-revision ()
  (xgit-last-revision default-directory))

(defalias 'xgit-dvc-annotate-time 'xgit-annotate-time)

(defun xgit-dvc-missing ()
  "For `dvc-missing'"
  (interactive)
  (dvc-revlist-setup
   'xgit-log-gen (list "HEAD..refs/remotes/origin/HEAD")
   t ;; first-line-only-p
   dvc-revlist-last-n ;; last-n
   ))

(defun xgit-dvc-update  (&optional revision-id)
  "For `dvc-update'."
  (let* ((review-rev (xgit-dvc-base-revision))
	 (branch (xgit-dvc-branch))
	 (upstream-head (xgit-dvc-upstream-head branch))
	 )
    (dvc-run-dvc-sync
     'xgit (list "merge" (or revision-id upstream-head))
     :finished
     (with-temp-file ".git/refs/last_update"
       (insert review-rev))
     )))

(defun xgit-dvc-merge  (&optional revision-id)
  "For `dvc-merge'."
  (xgit-dvc-update revision-id))

(defun xgit-dvc-update-review  ()
  "For `dvc-update-review'."
  (let ((update-rev (xgit-rev-parse "last_update")))
    (dvc-revlist-setup
     'xgit-log-gen (list (format "%s..HEAD" update-rev))
     t ;; first-line-only-p
     dvc-revlist-last-n ;; last-n
     )
    ))

(defun xgit-dvc-pull ()
  "For `dvc-pull'"
  (interactive)
  (dvc-run-dvc-async
   'xgit
   (list "fetch" "--all")
   :finished
   (lambda (output error status arguments)
     (with-current-buffer error
       (goto-char (point-min))
       (insert "workspace: " default-directory "\n")
       (write-region (point-min) (point-max)
		     (concat (expand-file-name xgit-sync-log-file dvc-config-directory))
		     t)) ;; append
     (xgit-dvc-parse-sync))))

(defun xgit-dvc-push ()
  "For `dvc-push'."
  (interactive)
  (xgit-push-default))

(defalias 'xgit-dvc-clone 'xgit-clone)

(defalias 'xgit-dvc-create-branch 'xgit-branch)
(defalias 'xgit-dvc-select-branch 'xgit-checkout)
(defalias 'xgit-dvc-list-branches 'xgit-branch-list)

(defalias 'xgit-dvc-send-commit-notification 'xgit-gnus-send-commit-notification)
(defalias 'xgit-dvc-init 'xgit-init)

;;;###autoload
(defalias 'xgit-dvc-add 'xgit-add)

;;;###autoload
(defun xgit-dvc-branch ()
  "For `dvc-branch'."
  ;; in a workspace with a checkout of the master branch:
  ;; * (no branch)
  ;;   master
  ;;
  ;; or
  ;; * master
  ;;
  ;; in a workspace with a checkout of a child branch:
  ;; * electric-solenoid
  ;;   master

  (dvc-run-dvc-sync
   'xgit (list "branch")
   :finished
   (lambda (output error status arguments)
     (with-current-buffer output
       (let ((branch nil))
	 (goto-char (point-min))
	 (while (not branch)
	   (if (looking-at "^\\* \\(.*\\)$")
	       (setq branch (match-string-no-properties 1))
	     (forward-line 1)))
	 branch
	)))
   ))

(defun xgit-dvc-edit-ignore-files ()
  "Edit git's ignore file.
TODO: Support per directory ignore file.
	  This only supports exclude file now."
  (interactive)
  (find-file-other-window (xgit-get-root-exclude-file)))

(defun xgit-dvc-ignore-files (file-list)
  "Added FILE-LIST to git's ignore file.
TODO: Support per directory ignore file.
	  This only supports exclude file now."
  (interactive (list (dvc-current-file-list)))

  (when (y-or-n-p (format "Ignore %S for %s? "
			  file-list
			  (xgit-git-dir)))
	(with-current-buffer
		(find-file-noselect (xgit-get-root-exclude-file))
	  (goto-char (point-max))
	  (dolist (f-name file-list)
		(insert (format "%s\n" f-name)))
	  (save-buffer))))

(defun xgit-dvc-stage-files (files)
  "For `dvc-add-files'."
  (dvc-run-dvc-sync
   'xgit
   (append '("add")
	   (mapcar #'file-relative-name files))
   :finished #'ignore))

(defun xgit-dvc-unstage-files (files)
  "For `dvc-unstage'."
  (dvc-run-dvc-sync
   'xgit (append '("reset" "-q")
		 (mapcar #'file-relative-name files))
   :finished #'ignore
   :error
   ;; git returns an error if there are unstaged changes left after
   ;; the reset. Since that is _precisely_ why we are doing this
   ;; reset, ignore the error!
   #'ignore)
  )

(defun xgit-dvc-stashp ()
  t)
(defun xgit-dvc-stagep ()
  t)

(defun xgit-dvc-stash-save ()
  "For `dvc-stash-save'."
  (let ((label (read-from-minibuffer "stash label: ")))
    (dvc-run-dvc-sync
     'xgit (list "stash" "save" "--keep-index" "--include-untracked" label)
     :finished #'ignore)
    ))

(defun xgit-dvc-stash-drop ()
  "For `dvc-stash-drop'."
  (dvc-run-dvc-sync
   'xgit '("stash" "drop")
   :finished #'ignore
   :error
   ;; show conflicts
   (lambda (output error status arguments)
     (dvc-show-error-buffer output)))
  )

(defun xgit-dvc-stash-pop ()
  "For `dvc-stash-pop'."
  (dvc-run-dvc-sync
   'xgit '("stash" "pop")
   :finished #'ignore
   :error
   ;; show conflicts
   (lambda (output error status arguments)
     (dvc-show-error-buffer output)))
  )

(defun xgit-dvc-stash-show ()
  "For `dvc-stash-show'."
  (interactive)
  (dvc-run-dvc-display-as-info 'xgit '("stash" "show"))
  )

(defun xgit-dvc-conflicts-status (buffer left-work left-rev right-work right-rev left-branch right-branch)
  "For `dvc-conflicts-status'."
  ;; git does not support conflict resolution in this way
  (list nil 'none)
  )

(defun xgit-dvc-get-changelog (workspace repo branch rev-range stuff)
  "Get the changelog for BRANCH REV-RANGE from git log.
Add `dvc-sync-branch' entries to STUFF, return STUFF"
  (let ((default-directory workspace)
	(rev-count 0)
	(msg (format "getting changelog for %s | %s ..." repo branch))
	revid date author changelog-begin changelog-end
	changelog log-done)

    (message msg)
    (dvc-run-dvc-sync
     'xgit (list "log" "--format=medium" rev-range)
     :finished
     (lambda (output error status arguments)
       (with-current-buffer output
	 ;; commit 77719a9c033ff72b9d2d6c0d857c77642b26a8b0
	 ;; Merge: 0c8e3a1 51c140c
	 ;; Author: Dmitry Gutov <dgutov@yandex.ru>
	 ;; Date:   Sat Apr 19 14:11:04 2014 +0400
	 ;;
	 ;;     Merge commit '51c140ca9ee32d27cacc7b2b07d4539bf98ae575' from company-master
	 ;;
	 ;;     Conflicts:
	 ;;     	packages/company/company-pysmell.el
	 (goto-char (point-min))
	 (setq log-done nil)
	 (setq changelog-begin nil)
	 (while (not log-done)
	   (cond
	    ((looking-at "commit")
	     (setq rev-count (1+ rev-count))
	     (when changelog-begin
	       ;; FIXME: strip trailing blank line in changelog?
	       (setq changelog-end (1- (point)))
	       (setq changelog (buffer-substring-no-properties changelog-begin changelog-end))
	       (setq stuff (dvc-sync-enter-rev stuff workspace repo branch revid date author changelog 'receive))
	       )

	     (setq revid (buffer-substring-no-properties (+ (point) 8) (line-end-position)))
	     )

	    ((looking-at "Merge:")
	     ;; FIXME: put this info in dvc-sync buffer?
	     nil)

	    ((looking-at "Author:")
	     (setq author (buffer-substring-no-properties (+ (point) 8) (line-end-position))))

	    ((looking-at "Date:")
	     (setq date (buffer-substring-no-properties (+ (point) 8) (line-end-position)))
	     (setq changelog-begin (1+ (line-end-position))))

	    ((eobp)
	     (setq log-done t)
	     (when changelog-begin
	       (setq changelog (buffer-substring-no-properties changelog-begin (point-max)))
	       (setq stuff (dvc-sync-enter-rev stuff workspace repo branch revid date author changelog 'receive))
	       ))

	    (t ;; in changelog
	     nil)
	    )
	   (forward-line 1))
	 (message "%s: %d revs" msg rev-count)

	 ))))
  stuff)

(defun xgit-dvc-parse-sync ()
  "For `dvc-parse-sync'.
Parse `xgit-sync-log-file', append result to `dvc-sync-save-file'."
  ;; xgit-sync-log-file contains the workspace, followed by the
  ;; literal output from 'git fetch':
  ;;
  ;; workspace: /Projects/emacs/master
  ;; From git.sv.gnu.org:/srv/git/emacs
  ;;    0a3e2cf..061f310  master     -> origin/master
  ;;    fb420e7..48a9d9f  emacs-24   -> origin/emacs-24
  ;;  * [new branch]      scratch/xref -> origin/scratch/xref
  ;;  * [new tag]         emacs-24.4.90 -> emacs-24.4.90
  ;;
  ;; the first commit id is the local HEAD; the second is the new remote head

  (let ((dvc-file (expand-file-name dvc-sync-save-file dvc-config-directory))
	(git-file (expand-file-name xgit-sync-log-file dvc-config-directory))
	stuff fetch-done workspace repo rev-range branch)

    (when (file-readable-p git-file)
      (with-temp-buffer
	(insert-file-contents-literally git-file)
	(goto-char (point-min))

	(while (not fetch-done)
	  (cond
	   ((looking-at "workspace: \\(.*\\)$")
	    (setq workspace (match-string 1))
	    )

	   ((looking-at "From \\(.*\\)$")
	    (setq repo (match-string 1))
	    )

	   ((looking-at "   \\([0-9a-f]+\\.\\.[0-9a-f]+\\) *\\([^ ]+\\)")
	    (setq rev-range (match-string 1))
	    (setq branch (match-string 2))

	    (setq stuff (xgit-dvc-get-changelog workspace repo branch rev-range stuff)))

	   ((looking-at " \\* \\[new branch\\] *\\([^ ]+\\)")
	    (let* ((default-directory workspace)
		   (branch-head (xgit-rev-parse (concat "refs/remotes/origin/" branch))))
	      (setq branch (match-string 1))
	      (setq rev-range
		    (concat
		     (xgit-dvc-lca "HEAD" branch-head)
		     ".."
		     branch-head))
	      (setq stuff (xgit-dvc-get-changelog workspace repo branch rev-range stuff))
	      ))

	   ((looking-at " \\* \\[new tag\\]")
	    ;; no use for this yet
	    nil)

	   ((eobp)
	    (setq fetch-done t))

	   (t (error "unrecognized line in fetch output"))
	   )
	  (forward-line 1)) ;; one repo fetch

	(dvc-save-state (list 'stuff) dvc-file)

	;; rename git-file on the disk for debugging
	(rename-file git-file (concat git-file ".save") t)
      ))))

(defun xgit-dvc-propagate (from from-rev to)
  "For `dvc-propagate'."
  ;; 'git merge' always merges into the current branch; verify that is TO.
  ;;
  ;; if 'from' is a local branch, it was fetched into the current
  ;; workspace repository (rather than pushed), and it not defined as
  ;; a branch in the current repository. So we must use the revid in
  ;; the merge command.
  (let ((msg (format "merge from %s to %s" from to))
	(to-head (xgit-dvc-upstream-head to)))
    (unless (string= to-head (xgit-dvc-base-revision))
      (error "'to' branch %s is not current" to))

    (dvc-run-dvc-sync
     'xgit (list "merge" "-m" msg from-rev)
     :finished
     (lambda (output error status arguments)
     ;; better than default dvc message
       (message (concat msg " ... finished"))))
    ))

(defun xgit-dvc-sync-run ()
  "For `dvc-sync-run'."
  (xgit-dvc-pull))

(provide 'xgit-dvc)
;;; xgit-dvc.el ends here
