;;; xgit.el --- git interface for dvc

;; Copyright (C) 2006-2009, 2013 - 2015 by all contributors

;; Author: Stefan Reichoer <stefan@xsteve.at>
;; Contributions from:
;;    Takuzo O'hara <takuzo.ohara@gmail.com>

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

;; This is the git backend for DVC.  It requires git version 1.5.0 or
;; later.

;;; History:

;;

;;; Code:

(require 'cl-lib)
(require 'cus-edit)
(require 'dvc-core)
(require 'dvc-diff)
(require 'dvc-status)
(require 'xgit-annotate)
(require 'xgit-core)
(require 'xgit-log)

;;;###autoload
(defun xgit-init (&optional dir)
  "Run git init."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for git init: "
                                                    (or default-directory
                                                        (getenv "HOME"))))))
  (let ((default-directory (or dir default-directory)))
    (dvc-run-dvc-sync
     'xgit
     (list "init-db")
     :finished
     (lambda (output error status arguments)
       (message "git init finished")))))

;;;###autoload
(defun xgit-clone (src &optional dest)
  "Run git clone."
  (interactive (list (read-string "git clone from: ")))
  (dvc-run-dvc-async 'xgit (list "clone" src dest)))

;;;###autoload
(defun xgit-add (file)
  "Add FILE to the current git project."
  (interactive (list (dvc-confirm-read-file-name "Add file or directory: ")))
  (xgit-dvc-add-files file))

;;;###autoload
(defun xgit-add-patch (files)
  ;; this is somehow a dirty hack. DVC should have it's own
  ;; hunk-by-hunk staging feature, but waiting for that, 'git add -p'
  ;; is sooo nice, let's use it through term.el
  "Add FILES to the current git project using 'git add --patch ...'.
If FILES is nil, just run 'git add --patch'"
  (interactive (list (list (expand-file-name (dvc-confirm-read-file-name "Add file or directory: ")))))
  (require 'term)
  (let* ((root (dvc-tree-root (car files)))
         (default-directory root)
         (buffer (dvc-get-buffer-create 'xgit 'add-patch))
         (args (mapcar (lambda (f)
                         (file-relative-name (dvc-uniquify-file-name
                                              f) root))
                       files)))
    (switch-to-buffer
     (eval `(term-ansi-make-term ,(buffer-name buffer)
                                 ,xgit-executable nil "add" "-p" "--"
                                 ,@args)))))

(defun xgit-add-patch-all ()
  "Call `xgit-add-patch' without argument, to run plain 'git add -p'"
  (interactive)
  (xgit-add-patch nil))

;;;###autoload
(defun xgit-dvc-add-files (&rest files)
  "For `dvc-add-files'."
  (dvc-run-dvc-sync
   'xgit
   (append '("add")
	   (mapcar #'file-relative-name files))
   :finished #'ignore))

;;;###autoload
(defun xgit-remove (file &optional force)
  "Remove FILE from the current git project.
If FORCE is non-nil, then remove the file even if it has
uncommitted changes."
  (interactive (list (dvc-confirm-read-file-name "Remove file: ")
                     current-prefix-arg))
  (let ((default-directory (xgit-tree-root)))
    (dvc-run-dvc-sync
     'xgit (list "rm" (when force "-f") "--" (file-relative-name file))
     :finished
     (lambda (output error status arguments)
       (message "git remove finished")))))

;;;###autoload
(defun xgit-dvc-remove-files (&rest files)
  "Run git rm."
  (dvc-trace "xgit-remove-files: %s" files)
  (dvc-run-dvc-sync
   'xgit
   (nconc (list "rm" "--")
	  (mapcar #'file-relative-name files))
   :finished
   (lambda (output error status arguments)
     (message "git rm finished"))))

(defun xgit-command-version ()
  "Run git version."
  (interactive)
  (let ((version (dvc-run-dvc-sync 'xgit (list "version")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Git Version: %s" version))
    version))

;;;###autoload
(defun xgit-add-all-files (arg)
  "Run 'git add .' to add all files in the current directory tree to git.

Normally run 'git add -n .' to simulate the operation to see
which files will be added.

Only when called with a prefix argument, add the files."
  (interactive "P")
  (dvc-run-dvc-sync 'xgit (list "add" (unless arg "-n") "."))
  ;; default finish shows process buffer
  )

;;;###autoload
(defun xgit-addremove ()
  "Add all new files to the index, remove all deleted files from
the index, and add all changed files to the index.

This is done only for files in the current directory tree."
  (interactive)
  (dvc-run-dvc-sync
   'xgit (list "add" ".")
   :finished
   (lambda (output error status arguments)
     (dvc-run-dvc-sync
      'xgit (list "add" "-u" ".")
      :finished
      (lambda (output error status args)
	(message "Finished adding and removing files to index"))))))

;;;###autoload
(defun xgit-reset-hard (&rest extra-param)
  "Run 'git reset --hard'"
  (interactive)
  (when (interactive-p)
    (setq extra-param (list (ido-completing-read "git reset --hard " '("HEAD" "ORIG_HEAD")
                                                 nil nil nil nil '("HEAD" "ORIG_HEAD")))))
  (dvc-run-dvc-sync 'xgit (append '("reset" "--hard") extra-param))
  ;; default finish shows process buffer
  )

(defconst xgit-status-line-regexp
  "^#[ \t]+\\([[:alpha:]][[:alpha:][:blank:]]+\\):\\(?:[ \t]+\\(.+\\)\\)?$"
  "Regexp that matches a line of status output and extracts fields.
The first match string is either a group label ('Unmerged paths',
'Untracked files', etc) or a file status type ('modified', 'both
modified', etc). The optional second match is the file path."  )

(defvar xgit-status-renamed-regexp "^\\(.+\\) -> \\(.+\\)$"
  "Regexp that divides a filename string.
The first match is the original file, and the second match is the
new file.")

;; these confict markers appear in this order in the file
(defconst xgit-conflict-begin-re "^<<<<<<< \\(.*\\)\n")
(defconst xgit-conflict-other-re "^=======\\( .*\\)?\n")
(defconst xgit-conflict-end-re   "^>>>>>>>\\( .*\\)?\n")

(defun xgit-conflicts-p (file)
  "Return non-nil if FILE contains git conflict markers."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (search-forward-regexp xgit-conflict-begin-re nil t)
    ))

(defun xgit-file-staged-p (file)
  "Return non-nil if FILE is identical to staged version."
  ;; 'git diff <file>' diffs the workspace file with the index file.
  (dvc-run-dvc-sync
   'xgit (list "diff" file)
   :finished
   (lambda (output error status arguments)
     (with-current-buffer output
       (= (point-min) (point-max))))
   ))

(defun xgit-parse-status-1 ()
  "Parse output of 'git status --porcelain' in current buffer.
Return '(DVC-RESULT . STATUS-LIST);
DVC-RESULT is a list of one or more of 'ok, 'need-stash-save, 'need-stash-pop, 'need-commit, 'need-post-process
STATUS-LIST is a list of propertly lists, containing
:file         filename
:dir          directory name
:status       dvc-fileinfo status
:indexed      nil/t
:more-status  dvc-fileinfo more status"
  ;; example status output:
  ;;
  ;; MM packages/ada-mode/gpr-mode.el
  ;;
  ;; first letter is the index status, second is the workspace status

  (goto-char (point-min))

  (let (dvc-result
	index-status work-status
	file dir indexed status
	status-list)
    (while (not (eobp))
      (setq index-status (char-after (point)))
      (setq work-status (char-after (1+ (point))))
      (setq file (buffer-substring-no-properties (+ 3 (point)) (line-end-position)))

      (cl-case index-status
	(? ;; unmodified
	 (cl-case work-status
	   (?D
	    (setq status 'deleted
		  indexed nil)
	    (add-to-list 'dvc-result 'need-stash-save)
	    (add-to-list 'dvc-result 'need-commit);; for remove
	    )

	   (?M
	    (if (xgit-conflicts-p file)
		(setq status 'conflict
		      indexed nil)
	      (setq status 'modified
		    indexed nil))
	    (add-to-list 'dvc-result 'need-stash-save)
	    (add-to-list 'dvc-result 'need-commit);; for add
	    )

	   (t
	    (error "unknown status %s" (buffer-substring-no-properties (point) (line-end-position))))
	   ))

	(??
	 (setq status 'unknown
	       indexed nil)
	 (add-to-list 'dvc-result 'need-stash-save)
	 (add-to-list 'dvc-result 'need-commit);; for add
	 )

	(?!
	 (setq status 'ignored
	       indexed nil))

	(?A ;; added
	 ;; work-status irrelevant
	 ;; FIXME: Need more conflict info in fileinfo struct
	 (setq status 'added-staged
	       indexed t)
	 (add-to-list 'dvc-result 'need-commit))

	(?C ;; copied
	 (add-to-list 'dvc-result 'need-commit)
	 (when (not (eq work-status ? ))
	   (add-to-list 'dvc-result 'need-stash-save))

	 ;; FIXME: how can the file be copied without rename? Maybe just a directory change?
	 (string-match xgit-status-renamed-regexp file)
	 (let ((orig (match-string 1 file))
	       (new (match-string 2 file)))
	   (setq status-list
		 (cons
		  (list :file new :dir nil
			:status 'copy-target
			:indexed nil
			:more-status orig)
		  status-list))
	   (setq status-list
		 (cons
		  (list :file orig :dir nil
			:status 'copy-source
			:indexed nil
			:more-status new)
		  status-list))
	   (setq status nil)))

	(?D ;; deleted
	 (add-to-list 'dvc-result 'need-commit)
	 (cl-case work-status
	   (? ;; unmodified
	    (setq status 'deleted-staged
		  indexed t))
	   (t
	    (add-to-list 'dvc-result 'need-stash-save)
	    ;; FIXME: Need more conflict info in fileinfo struct
	    (setq status 'deleted
		  indexed t))
	   ))

	(?M ;; modified
	 (add-to-list 'dvc-result 'need-commit)

	 (cl-case work-status
	   (? ;; unmodified (during rebase)
	    (if (xgit-conflicts-p file)
		(setq status 'conflict-staged
		      indexed t)
	      (setq status 'modified-staged
		    indexed t)
	      ))

	   (?D
	    (setq status 'conflict
		  indexed t)
	    (add-to-list 'dvc-result 'need-stash-save))

	   (?M
	    (if (xgit-conflicts-p file)
		(progn
		  (setq status 'conflict
			indexed nil) ;; there are unstaged changes
		  (add-to-list 'dvc-result 'need-stash-save))

	      ;; Either there was a merge conflict, which was now
	      ;; resolved (perhaps only in the workspace), or there
	      ;; are newly staged changes from the workspace (after a
	      ;; dvc refresh). If the files are identical there is no
	      ;; way to distinguish the cases, and the file is fully
	      ;; staged. If not, it must still be staged. We'd like to
	      ;; run xgit-file-staged-p here, but that's a nested
	      ;; dvc-run-dvc-sync. So we mark this for post-processing
	      (add-to-list 'dvc-result 'need-post-process)
	      (setq status 'modified-modified-post-process)
	      ))

	   (t
	    (error "unknown status %s" (buffer-substring-no-properties (point) (line-end-position))))
	   ))

	(?R
	 (add-to-list 'dvc-result 'need-commit)
	 (when (not (eq work-status ? ))
	   (add-to-list 'dvc-result 'need-stash-save))

	 (string-match xgit-status-renamed-regexp file)
	 (let ((orig (match-string 1 file))
	       (new (match-string 2 file)))
	   (setq status-list
		 (cons
		  (list :file new :dir nil
			:status 'rename-target
			:indexed nil
			:more-status orig)
		  status-list))
	   (setq status-list
		 (cons
		  (list :file orig :dir nil
			:status 'rename-source
			:indexed nil
			:more-status new)
		  status-list))
	   (setq status nil)))


	(?U
	 ;; "updated" during rebase
	 (add-to-list 'dvc-result 'need-commit)

	 (cl-case work-status
	   (?U
	    ;; also updated during rebase
	    (if (xgit-conflicts-p file)
		(setq status 'conflict
		      indexed nil) ;; there are unstaged changes
	      (setq status 'conflict-resolved
		    indexed nil)))

	   (t
	    (error "unknown work status %s" (buffer-substring-no-properties (point) (line-end-position))))
	   ))

	(t
	 (error "unknown index status %s" (buffer-substring-no-properties (point) (line-end-position))))
	)

      (when status
	(setq status-list
	      (cons (list :file file :dir nil :indexed indexed :status status)
		    status-list)))

      (forward-line 1))

    (cons (if dvc-result
	      dvc-result
	    (list 'ok))
	  status-list)
    ))

(defun xgit-append-stash-status (result)
  (dvc-run-dvc-sync
   'xgit '("stash" "list")
   :finished
   (lambda (output error process-status arguments)
     ;; empty buffer means there is no stash
     (with-current-buffer output
       (if (= (point-min) (point-max))
	   result
	 (if (eq 'ok (car result))
	     '(need-stash-pop)
	   (append result '(need-stash-pop))))))
   ))

(defun xgit-parse-status-2  (temp status-buffer)
  "Parse output of `xgit-parse-status-1' in TEMP, display nicely in STATUS-BUFFER.
Return status for `dvc-status'."
  (let* ((dvc-result (car temp))
	 (status-list (cdr temp))
	 status-list-2
	 (unstaged-done nil))

    (with-current-buffer status-buffer
      (when (memq 'need-post-process dvc-result)
	(dolist (elem status-list)
	  (when (eq 'modified-modified-post-process (plist-get elem ':status))
	    (if (xgit-file-staged-p (plist-get elem ':file))
		;; the staged and workspace files are the same
		(progn
		  (plist-put elem ':status 'modified-staged)
		  (plist-put elem ':indexed t)
		  (add-to-list 'dvc-result 'need-commit))

	      ;; The files differ; more changes have been made since
	      ;; it was staged. The user may want to commit the
	      ;; current state (saving the new changes for a later
	      ;; commit), or update the staged file.
	      ;;
	      ;; Set :indexed nil, so this file is listed with other
	      ;; unstaged files; it needs attention.
	      (plist-put elem ':status 'modified2-staged)
	      (plist-put elem ':indexed nil)
	      (add-to-list 'dvc-result 'need-stash-save)
	      (add-to-list 'dvc-result 'need-commit))
	    )
	  (setq status-list-2 (cons elem status-list-2))
	  )

	(setq status-list status-list-2
	      status-list-2 nil)

	(setq dvc-result (delq 'need-post-process dvc-result)))

      (setq dvc-header (format "git status for %s\n" default-directory))

      ;; sort on indexed, then dir/file
      (setq
       status-list
       (sort
	status-list
	(lambda (a b)
	  (if (eq (plist-get a ':indexed)
		  (plist-get b ':indexed))
	      (string-lessp (concat (plist-get a ':dir) "/" (plist-get a ':file))
			    (concat (plist-get b ':dir) "/" (plist-get b ':file)))
	    (plist-get b ':indexed)))))


      ;; first unstaged files
      (ewoc-enter-last dvc-fileinfo-ewoc
		       (make-dvc-fileinfo-message :text "unstaged"))

      (dolist (elem status-list)
	(when (and (not unstaged-done) (plist-get elem ':indexed))
	  (ewoc-enter-last dvc-fileinfo-ewoc
			   (make-dvc-fileinfo-message :text "staged"))
	  (setq unstaged-done t))

	(ewoc-enter-last dvc-fileinfo-ewoc
			 (apply #'make-dvc-fileinfo-file elem)))
      )

    (xgit-append-stash-status dvc-result))
  )

(defun xgit-dvc-status (no-switch)
  "For `dvc-status'."
  (let* ((root default-directory)
	 (dvc-switch-to-buffer-first (not no-switch))
         (status-buffer (dvc-status-prepare-buffer
                  'xgit
		  root
		  (xgit-dvc-base-revision)
		  "" ;; FIXME: branch
		  nil ;; header-more
		  (lambda () (xgit-dvc-status t));; refresh
		  ))
	 temp
	 result-status)
    (dvc-save-some-buffers root)

    (dvc-run-dvc-sync
     'xgit `("status" "--porcelain")
     :finished (lambda (output error status arguments)
		 (with-current-buffer output
		   (setq temp (xgit-parse-status-1)))
		 )
     :error (lambda (output error status arguments)
	      (pop-to-buffer error)
	      (error "xgit status returned error"))
     )

    (dvc-status-inventory-done status-buffer)

    ;; can't run this in :finished; nested dvc-run-dvc-sync
    (setq result-status (xgit-parse-status-2 temp status-buffer))

    (with-current-buffer status-buffer
      (when (not (ewoc-locate dvc-fileinfo-ewoc))
	(ewoc-enter-last dvc-fileinfo-ewoc
			 (make-dvc-fileinfo-message
			  :text (concat " no changes in workspace")))
	(ewoc-refresh dvc-fileinfo-ewoc)))

    (list status-buffer result-status)
    ))

(defun xgit-dvc-status-dtrt (key-prefix status)
  "For `dvc-status-dtrt'."
  (ecase status
    (added-staged
     (dvc-fileinfo-add-log-entry key-prefix))

    (conflict
     ;; ediff to resolve conflict
     ;;
     ;; git uses some different conflict markers than the smerge
     ;; defaults; we let-bind them all here just for clarity.
     (let ((smerge-begin-re xgit-conflict-begin-re)
	   (smerge-end-re xgit-conflict-end-re)
	   (smerge-other-re xgit-conflict-other-re))
       (dvc-status-resolve-conflicts)))

    (conflict-resolved
     (dvc-fileinfo-stage-files))

    (deleted
     (dvc-offer-choices
      nil
      '((dvc-fileinfo-revert-files "revert")
	(dvc-fileinfo-stage-files "stage"))))

    (deleted-staged
     (dvc-offer-choices
      nil
      '((dvc-fileinfo-revert-files "revert")
	(dvc-fileinfo-unstage-files "unstage"))))

    ((rename-source rename-target)
     (dvc-status-ediff))

    (missing
     ;; File is in database, but not in workspace
     (ding)
     (dvc-offer-choices
      (concat (dvc-fileinfo-current-file) " does not exist in working directory")
      '((dvc-fileinfo-revert-files "revert")
	(dvc-fileinfo-remove-files "remove")
	(dvc-fileinfo-rename "rename"))))

    (modified
     (ding)
     (dvc-offer-choices
      nil
      '((dvc-status-ediff-work-base "ediff")
	(dvc-fileinfo-stage-files "stage")
	(dvc-fileinfo-revert-files "revert")
	)))

    (modified-staged
     (ding)
     (dvc-offer-choices
      nil
      '((dvc-status-ediff "ediff")
	(dvc-fileinfo-unstage-files "unstage")
	(dvc-fileinfo-revert-files "revert")
	)))

    (modified2-staged
     (ding)
     (dvc-offer-choices
      nil
      '((dvc-status-ediff-work-staged "ediff work staged")
	(dvc-status-ediff-work-base "ediff work base")
	(dvc-status-ediff-staged-base "ediff staged base")
	(dvc-fileinfo-unstage-files "unstage")
	(dvc-fileinfo-revert-files "revert")
	)))

    (unknown
     (dvc-offer-choices nil
			'((dvc-fileinfo-add-files "add")
			  (dvc-fileinfo-ignore-files "ignore")
			  (dvc-fileinfo-remove-files "remove")
			  (dvc-fileinfo-rename "rename"))))
    ))

(defun xgit-resolve-conflicts ()
  "Resolve conflicts in current file."
  (interactive)
  ;; git uses some different conflict markers than the smerge
  ;; defaults; we let-bind them all here just for clarity.
  (let ((smerge-begin-re xgit-conflict-begin-re)
	(smerge-end-re xgit-conflict-end-re)
	(smerge-other-re xgit-conflict-other-re))
    (smerge-ediff)))

(defun xgit-status-verbose ()
  (interactive)
  (xgit-dvc-status t))

(defun xgit-status-add-patch ()
  "Run `xgit-add-patch' on selected files."
  (interactive)
  (xgit-add-patch (dvc-current-file-list)))

(defun xgit-status-add-current ()
  "Run \"git add <files>\" on current/selected files and refresh current buffer."
  (interactive)
  (let ((args (mapcar (lambda (f)
			(file-relative-name (dvc-uniquify-file-name
					     f)
					    default-directory))
		      (dvc-current-file-list)))
	)

    (dvc-run-dvc-sync 'xgit `("add" ,@args)  :finished #'ignore)
    (dvc-generic-refresh)))

(defun xgit-status-add-u ()
  "Run \"git add -u\" and refresh current buffer."
  (interactive)
  (lexical-let ((buf (current-buffer)))
    (dvc-run-dvc-async
     'xgit '("add" "-u")
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (with-current-buffer buf
                   (dvc-generic-refresh))))))

(defun xgit-status-reset-mixed ()
  "Run \"git reset --mixed\" and refresh current buffer.

This reset the index to HEAD, but doesn't touch files."
  (interactive)
  (lexical-let ((buf (current-buffer)))
    (dvc-run-dvc-async
     'xgit '("reset" "--mixed")
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (with-current-buffer buf
                   (dvc-generic-refresh))))))

(defvar xgit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?A] 'xgit-status-add-u)
    (define-key map [?G ?r] 'xgit-status-reset-mixed)
    (define-key map [?G ?p] 'xgit-status-add-patch)
    (define-key map [?G ?P] 'xgit-add-patch-all)
    ;; 's'taged.
    (define-key map [?G ?s] 'xgit-diff-cached)
    ;; 'u'nstaged.
    (define-key map [?G ?u] 'xgit-diff-index)
    map))

(easy-menu-define xgit-diff-mode-menu xgit-diff-mode-map
  "`Git specific changes' menu."
  `("GIT-Diff"
    ["Stage current/selected files" xgit-status-add-current t]
    ["Stage all modified files (add -u)" xgit-status-add-u t]
    ["Revert current/selected files" xgit-status-revert-current t]
    ["Clear stage (reset index (reset --mixed))" xgit-status-reset-mixed t]
    "---"
    ["View staged changes" xgit-diff-cached t]
    ["View unstaged changes" xgit-diff-index t]
    "---"
    ["View all local changes" xgit-diff-head t]
    ))

(define-derived-mode xgit-diff-mode dvc-diff-mode "xgit-diff"
  "Mode redefining a few commands for diff."
  )

(dvc-add-uniquify-directory-mode 'xgit-diff-mode)

(defun xgit-parse-diff (changes-buffer)
  (while (re-search-forward
	  "^diff --git [^ ]+ b/\\(.*\\)$" nil t)
    (let* ((name (match-string-no-properties 1))
	   ;; added, removed are not yet working
	   (added (progn (forward-line 1)
			 (looking-at "new file")))
	   (removed (looking-at "deleted file"))
	   (indexed (looking-at "index")))
      (with-current-buffer changes-buffer
	(ewoc-enter-last
	 dvc-fileinfo-ewoc
	 (make-dvc-fileinfo-file
	  :dir (file-name-directory name)
	  :file (file-name-nondirectory name)
	  :status (cond (added   'added-staged)
			(removed 'deleted)
			(t 'modified))
	  :indexed indexed)))
      )))

(defun xgit-diff-1 (modified-rev path dont-switch base-rev)
  (let* ((cur-dir (or path default-directory))
         (orig-buffer (current-buffer))
         (root (xgit-tree-root cur-dir))
         (modified (if modified-rev
                      (dvc-revision-to-string modified-rev
                                              xgit-prev-format-string "HEAD")
                    "HEAD"))
         (modified-rev (or modified-rev `(xgit (last-revision ,root 1))))
         (base (if base-rev
                   (dvc-revision-to-string base-rev xgit-prev-format-string
                                           "HEAD")
                 nil))
         (local-tree `(xgit (local-tree ,root)))
         (base-rev (or base-rev local-tree))
         (buffer (dvc-prepare-changes-buffer
                  base-rev modified-rev
                  'diff root 'xgit))
         (command-list (if (equal modified-rev '(xgit (index)))
                           (if (equal base-rev local-tree)
                               '("diff" "-M")
                             (message "%S != %S" base-rev local-tree)
                             `("diff" "-M" "--staged" ,modified))
                         `("diff" "-M" ,base ,modified))))
    (dvc-switch-to-buffer-maybe buffer)
    (when dont-switch (pop-to-buffer orig-buffer))
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync
     'xgit command-list
     :finished
     (lambda (output error status arguments)
       (dvc-show-changes-buffer
	output
	'xgit-parse-diff
	buffer
	nil nil
	(mapconcat
	 (lambda (x) x)
	 (cons "git" command-list)
	 " "))))))

(defun xgit-last-revision (path)
  `(xgit (last-revision ,path 1)))

;; TODO offer completion here, e.g. xgit-tag-list
(defun xgit-read-revision-name (prompt)
  (read-string prompt))

;;;###autoload
(defun xgit-dvc-diff (&optional against-rev path dont-switch)
  (interactive (list nil nil current-prefix-arg))
  (xgit-diff-1 against-rev path dont-switch nil))

;;;###autoload
(defun xgit-diff-index (&optional against-rev path dont-switch)
  "Call \"git diff\" (diff between tree and index)."
  (interactive (list nil nil current-prefix-arg))
  (let ((path (or path (xgit-tree-root)))
        (against-rev (or against-rev '(xgit (index)))))
    (xgit-diff-1 against-rev path dont-switch
                 `(xgit (local-tree ,path)))))

;;;###autoload
(defun xgit-diff-head (&optional path dont-switch)
  "Call \"git diff HEAD\"."
  (interactive (list nil current-prefix-arg))
  (xgit-diff-1 `(xgit (local-tree ,path))
               path dont-switch
               `(xgit (last-revision ,path 1))))

;;;###autoload
(defun xgit-diff2 (base-rev against-rev &optional path dont-switch)
  "Call \"git diff BASE-REV AGAINST-REV\"."
  (interactive (list
                (xgit-read-revision-name "Base Revision: ")
                (xgit-read-revision-name "Against Revision: ")
                nil
                current-prefix-arg))
  (xgit-diff-1 `(xgit (revision ,against-rev))
               path dont-switch
               `(xgit (revision ,base-rev))))

(defvar xgit-prev-format-string "%s~%s"
  "This is a format string which is used by `dvc-revision-to-string'
when encountering a (previous ...) component of a revision indicator.
.
The first argument is a commit ID, and the second specifies how
many generations back we want to go from the given commit ID.")

(defun xgit-delta (base-rev modified &optional dont-switch)
  "For `dvc-delta'."
  (interactive (list nil nil current-prefix-arg))
  (let* ((root (xgit-tree-root))
         (buffer (dvc-prepare-changes-buffer
                  base-rev
                  modified
                  'diff root 'xgit)))
    (xgit-diff-1 modified root dont-switch base-rev)
    (with-current-buffer buffer (goto-char (point-min)))
    buffer))

;;;###autoload
(defun xgit-fetch (&optional repository)
  "Call git fetch.
When called with a prefix argument, ask for the fetch source."
  (interactive "P")
  (when (interactive-p)
    (when current-prefix-arg
      (setq repository (read-string "Git fetch from: "))))
  (dvc-run-dvc-async 'xgit (list "fetch" repository)))

(defun xgit-push-default ()
    "Run 'git push'."
  (interactive)
  (dvc-run-dvc-sync
   'xgit (list "push")
   :finished
   (lambda (output error status arguments)
     (if (eq status 0)
	 (message "xgit-push finished")
       (dvc-switch-to-buffer error)))))

(cl-defun xgit-push (url &optional (branch "master"))
    "Run 'git push url'.
with prefix arg ask for branch, default to master."
  (interactive "sGit push to: ")
  (lexical-let ((branch-name (if current-prefix-arg
                             (read-string "Which Branch?: ")
                             branch))
                (to url))
    (dvc-run-dvc-async 'xgit (list "push" url branch-name)
                       :finished
                       (dvc-capturing-lambda (output error status arguments)
                         (if (eq status 0)
                             (message "xgit-push <%s> to <%s> finished" branch-name to)
                             (dvc-switch-to-buffer error))))))

(defun xgit-split-out-added-files (files)
  "Remove any files that have been newly added to git from FILES.
This returns a two-element list.

The first element of the returned list is a list of the
newly-added files from FILES.

The second element is the remainder of FILES."
  (let* ((tree-added nil)
         (added nil)
         (not-added nil))
    ;; get list of files that have been added
    (with-temp-buffer
      ;; FIXME: delete or use porcelain
      (dvc-run-dvc-sync 'xgit (list "status")
                        :output-buffer (current-buffer)
                        :finished #'ignore :error #'ignore)
      (goto-char (point-min))
      (while (re-search-forward xgit-status-line-regexp nil t)
        (when (string= (match-string 1) "new file")
          (setq tree-added (cons (match-string 2) tree-added)))))
    ;; filter FILES
    (dolist (file files)
      (if (member file tree-added)
          (setq added (cons file added))
        (setq not-added (cons file not-added))))
    (list added not-added)))

;;;###autoload
(defun xgit-revert-file (file)
  "Revert uncommitted changes made to FILE in the current branch."
  (interactive "fRevert file: ")
  (xgit-dvc-revert-files file))

;;;###autoload
(defun xgit-dvc-revert-files (&rest files)
  "Revert uncommitted changes made to FILES in the current branch."
  (let ((default-directory (xgit-tree-root)))
    (setq files (mapcar #'file-relative-name files))
    (destructuring-bind (added not-added)
        (xgit-split-out-added-files files)

      ;; remove added files from the index
      (when added
        (let ((args (nconc (list "update-index" "--force-remove" "--")
                           added)))
          (dvc-run-dvc-sync 'xgit args
                            :finished #'ignore)))

      ;; revert other files using "git checkout HEAD ..."
      (when not-added
        (let ((args (nconc (list "checkout" "HEAD")
                           not-added)))
          (dvc-run-dvc-sync 'xgit args
                            :finished #'ignore)))

      (unless (or added not-added)
        (message "Nothing to do")))))

(defcustom xgit-show-filter-filename-func nil
  "Function to filter filenames in xgit-show.
Function is passed a list of files as a parameter.

Function should return list of filenames that is passed to
git-show or nil for all files."
  :type '(choice (const xgit-show-filter-filename-not-quilt)
                 (function)
                 (const :tag "None" nil))
  :group 'dvc-xgit)

(defun xgit-show-filter-filename-not-quilt (files)
  "Function to filter-out quilt managed files under .pc/ and patches/."
  (cl-loop for f in files
        when (not (string-match "\.pc/\\|patches/" f))
        collect f))

(defun xgit-changed-files (dir rev)
  "Returns list of files changed in given revision"
  (let* ((repo (xgit-git-dir-option dir))
         (cmd "diff-tree")
         (args (list repo cmd "--numstat" rev))
         (result (dvc-run-dvc-sync
                  'xgit args
                  :finished 'dvc-output-buffer-split-handler)))
    (mapcar (lambda (x) (nth 2 (split-string x)))
            (cdr result ))))

(defun xgit-show (dir rev &optional files)
  "Shows diff for a given revision.
Optional argument FILES is a string of filename or list of
filenames of to pass to git-show.

If FILES is nil and `xgit-show-filter-filename-func' is non-nil,
files changed in the revision is passed to
`xgit-show-filter-filename-func' and result is used."
  (interactive (list default-directory
                     (read-string "Revision (default: HEAD): "
                                  (let ((candidate (thing-at-point
                                                    'word)))
                                    (when (and candidate
                                               (string-match "[0-9a-f]"
                                                             candidate))
                                      candidate))
                                  nil "HEAD")))
  (if (and (null files) xgit-show-filter-filename-func)
      (setq files (funcall xgit-show-filter-filename-func
                           (xgit-changed-files dir rev))))
  (let* ((buffer (dvc-get-buffer-create 'xgit 'diff dir))
         (cmd "show")
         (args (list cmd rev "--")))
    (if files
        (setq args (nconc args (if (stringp files) (list files) files))))
    (dvc-switch-to-buffer-maybe buffer)
    (with-current-buffer buffer
      (dvc-run-dvc-sync
       'xgit args
       :finished
       (lambda (output error status arguments)
	 (with-current-buffer buffer
	   (let ((inhibit-read-only t))
	     (erase-buffer)
	     (insert-buffer-substring output)
	     (goto-char (point-min))
	     (insert (format "git %s\n\n"
			     (mapconcat #'identity
					args " ")))
	     (dvc-diff-mode)
	     (setq buffer-read-only 1))))))))

(defvar xgit-describe-regexp "^\\(.*?\\)-\\([0-9]+\\)-g[[:xdigit:]]\\{7\\}")

(defun xgit-describe-tag? (abbrev)
  (not (string-match xgit-describe-regexp abbrev)))

(defun xgit-describe (dir rev)
  "Show the most recent tag that is reachable from a commit.
If there is no tag return nil,
if revision is a tag, return tag in a string,
else returns list of '(tag offset all-described-string)."
  (interactive (list default-directory (read-string "Revision: ")))
  (let* ((repo (xgit-git-dir-option dir))
         (cmd "describe")
         (args (list repo cmd rev))
         (info (dvc-run-dvc-sync 'xgit args
                                 :finished 'dvc-output-buffer-handler
                                 :error 'dvc-output-buffer-handler)))
    (if (string= "" info)
        nil                             ;no tag yet
      (if (xgit-describe-tag? info)
          info
        (progn
          (list (match-string 1 info)
                (match-string 2 info)
                info))))))

(defun xgit-do-annotate (dir file)
  "Run git annotate for FILE in DIR.
DIR is a directory controlled by Git.
FILE is filename in the repository at DIR."
  (let* ((buffer (dvc-get-buffer-create 'xgit 'annotate))
         (repo (xgit-git-dir-option dir))
         (cmd "blame")
         (fname (file-relative-name (dvc-uniquify-file-name file)
                                    (xgit-tree-root dir)))
         (args (list repo cmd "--" fname)))
    (dvc-switch-to-buffer-maybe buffer)
    (dvc-run-dvc-sync
     'xgit args
     :finished
     (lambda (output error status arguments)
       (with-current-buffer buffer
	 (let ((inhibit-read-only t))
	   (buffer-disable-undo)
	   (erase-buffer)
	   (insert-buffer-substring output)
	   (goto-char (point-min))
	   (xgit-annotate-mode)))))))

(defun xgit-annotate ()
  "Run git annotate"
  (interactive)
  (save-excursion
    (let* ((filename (dvc-confirm-read-file-name "Filename to annotate: "))
	   (default-directory (xgit-tree-root filename)))
      (xgit-do-annotate default-directory filename))))

(defun xgit-stash-list (&optional only-list)
  "Run git-stash list."
  (interactive)
  (dvc-run-dvc-display-as-info 'xgit (list "stash" "list"))
  (when only-list
    (with-current-buffer "*xgit-info*"
      (let ((stash-list (split-string (buffer-string) "\n")))
        (cl-loop for i in stash-list
           with s = nil
           collect (car (split-string i ":")) into s
           finally (return s))))))

(defun xgit-stash-apply (&optional stash)
  "Run git-stash apply."
  (interactive)
  (if current-prefix-arg
      (save-window-excursion
        (let ((sl (xgit-stash-list t))
              stash-num)
          (setq stash-num (dvc-completing-read "Stash: " sl))
          (dvc-run-dvc-sync 'xgit (list "stash" "apply" stash-num))))
      (dvc-run-dvc-sync 'xgit (list "stash" "apply"))))

(defun xgit-stash-clear ()
  "Run git-stash clear."
  (interactive)
  (dvc-run-dvc-sync 'xgit (list "stash" "clear"))
  (message "All stash deleted")) ;; TODO run message in :finished

(defun xgit-stash-drop (&optional stash)
  "Run git-stash drop."
  (interactive)
  (if current-prefix-arg
      (let ((sl (xgit-stash-list t))
            stash-num)
        (save-window-excursion
          (setq stash-num (dvc-completing-read "Stash: " sl)))
        (dvc-run-dvc-sync 'xgit (list "stash" "drop" stash-num)))
      (dvc-run-dvc-sync 'xgit (list "stash" "drop"))))

(defun xgit-stash-pop (&optional stash)
  "Run git-stash pop."
  (interactive)
  (if current-prefix-arg
      (let ((sl (xgit-stash-list t))
            stash-num)
        (save-window-excursion
          (setq stash-num (dvc-completing-read "Stash: " sl)))
        (dvc-run-dvc-sync 'xgit (list "stash" "pop" stash-num)))
      (dvc-run-dvc-sync 'xgit (list "stash" "pop"))))

(defun xgit-stash-show (&optional stash)
  "Run git-stash show."
  (interactive)
  (if current-prefix-arg
      (let ((sl (xgit-stash-list t))
            stash-num)
        (save-window-excursion
          (setq stash-num (dvc-completing-read "Stash: " sl)))
        (dvc-run-dvc-display-as-info 'xgit (list "stash" "show" "-p" stash-num)))
      (dvc-run-dvc-display-as-info 'xgit (list "stash" "show" "-p")))
  (with-current-buffer "*xgit-info*"
    (diff-mode)))

(defun xgit-tag-list ()
  "Run \"git tag\" and list all defined tags"
  (interactive)
  (if (interactive-p)
      (dvc-run-dvc-display-as-info 'xgit (list "tag"))
    (dvc-run-dvc-sync 'xgit (list "tag")
                      :finished 'dvc-output-buffer-split-handler)))

(defun xgit-branch-list (&optional all)
  "Run \"git branch\" and list all known branches.
When ALL is given, show all branches, using \"git branch -a\".
When called via lisp, return the list of branches. The currently selected branch is
returned as first entry."
  (interactive "P")
  (if (interactive-p)
      (dvc-run-dvc-display-as-info 'xgit (list "branch" (when all "-a")))
    (let ((branch-list-raw
           (dvc-run-dvc-sync 'xgit (list "branch" (when all "-a"))
                             :finished 'dvc-output-buffer-split-handler))
          (branch-list))
      (dolist (branch-entry branch-list-raw)
        (cond ((string= (substring branch-entry 0 2) "* ")
               (add-to-list 'branch-list (substring branch-entry 2)))
              ((string= (substring branch-entry 0 2) "  ")
               (add-to-list 'branch-list (substring branch-entry 2) t))))
      branch-list)))

(defun xgit-branch (branch-name)
  "Run \"git branch BRANCH-NAME\" to create a new branch."
  (interactive "sCreate new git branch: ")
  (dvc-run-dvc-sync 'xgit (list "branch" branch-name)))

(defun xgit-checkout (branch-name)
  "Run \"git checout BRANCH-NAME\" to checkout an existing branch."
  (interactive (list (dvc-completing-read "Checkout git branch: " (xgit-branch-list t))))
  (dvc-run-dvc-sync 'xgit (list "checkout" branch-name))
  (message "git checkout %s done." branch-name))

;;;###autoload
(defun xgit-apply-patch (file)
  "Run \"git apply\" to apply the contents of FILE as a patch."
  (interactive (list (dvc-confirm-read-file-name
                      "Apply file containing patch: " t)))
  (dvc-run-dvc-sync 'xgit
                    (list "apply" (expand-file-name file))
                    :finished
                    (lambda (output error status arguments)
                      (message "Imported git patch from %s" file))
                    :error
                    (lambda (output error status arguments)
                      (dvc-show-error-buffer error)
                      (error "Error occurred while applying patch(es)"))))

;;;###autoload
(defun xgit-apply-mbox (mbox &optional force)
  "Run \"git am\" to apply the contents of MBOX as one or more patches.
If this command succeeds, it will result in a new commit being added to
the current git repository."
  (interactive (list (dvc-confirm-read-file-name
                      "Apply mbox containing patch(es): " t)))
  (dvc-run-dvc-sync 'xgit
                    (delq nil (list "am" (when force "-3")
                                    (expand-file-name mbox)))
                    :finished
                    (lambda (output error status arguments)
                      (message "Imported git mbox from %s" mbox))
                    :error
                    (lambda (output error status arguments)
                      (dvc-show-error-buffer error)
                      (error "Error occurred while applying patch(es)"))))

;;; DVC revision support

(defun xgit-revision-get-file-revision (file rev)
  "Insert the content of FILE in REV, in current buffer."
  (insert
   (dvc-run-dvc-sync
    'xgit
    (list "cat-file" "blob"
	  (format "%s:%s" (car rev) file))
    :finished 'dvc-output-buffer-handler-withnewline)))

(defun xgit-revision-get-staged-revision (file)
  "Insert the staged content of FILE (in the index), in current buffer."
  (insert
   (dvc-run-dvc-sync
    'xgit
    (list "cat-file" "blob"
	  (format ":%s" file))
    :finished 'dvc-output-buffer-handler-withnewline)))

(defun xgit-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)"
  (dvc-trace "xgit-revision-get-last-revision file:%S last-revision:%S"
             file last-revision)
  (let* ((xgit-rev (int-to-string (1- (nth 1 last-revision))))
         (default-directory (car last-revision))
         (fname (file-relative-name
                 (dvc-uniquify-file-name file)
                 (xgit-tree-root))))
    (insert (dvc-run-dvc-sync
             'xgit (list "cat-file" "blob"
                         (format "HEAD~%s:%s" xgit-rev fname))
             :finished 'dvc-output-buffer-handler-withnewline))))

(defun xgit-revision-get-previous-revision (file prev-rev)
  "Insert the content of FILE in REV, in current buffer."
  (insert
   (dvc-run-dvc-sync
    'xgit
    (list "cat-file" "blob"
	  (format "%s~1:%s" (cadadr prev-rev) file))
    :finished 'dvc-output-buffer-handler-withnewline)))

(defun xgit-get-root-exclude-file (&optional root)
  "returns exclude file for ROOT"
  (concat (file-name-as-directory (xgit-git-dir root))
	  "info/"
	  "exclude"))

(provide 'xgit)
;;; xgit.el ends here
