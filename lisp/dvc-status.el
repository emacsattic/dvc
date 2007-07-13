;;; dvc-status.el --- A generic status mode for DVC

;; Copyright (C) 2007 by all contributors

;; Author: Stephen Leake, <stephen_leake@stephe-leake.org>

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

;;

;;; Code:

(require 'dvc-ui)
(require 'dvc-defs)
(require 'dvc-core)
(require 'ewoc)
(eval-when-compile (require 'cl))

(defcustom dvc-status-display-known nil
  "If non-nil, display files with 'known' status in xmtn-status buffer."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-status-display-ignored nil
  "If non-nil, display files with 'ignored' status in xmtn-status buffer."
  :type 'boolean
  :group 'dvc)

(defun dvc-status-chose-face (status)
  "Return a face appropriate for STATUS."
  (case status
   ('added 'dvc-added)
   ('unknown 'dvc-unknown)
   ('modified 'dvc-modified)
   ('conflict 'dvc-conflict)
   ('move 'dvc-move)
   (t
    (dvc-trace "unknown status=%S" status)
    'default)))

(defstruct (dvc-status-fileinfo
	    (:copier nil))
  mark   	;; t/nil.
  dir		;; directory the file resides in, relative to dvc-root.
  file	     	;; The file name sans the directory.
                ;; (concat dir file) should give a valid path.
  status	;; symbol; see dvc-status-status-image for list
  more-status   ;; string; whatever else the backend has to say
  )

(defun dvc-status-status-image (status)
  "String image of STATUS."
  (ecase status
    (added          "added        ")
    (dropped        "dropped      ")
    (ignored        "ignored      ")
    (invalid        "invalid      ")
    (known          "known        ")
    (missing        "missing      ")
    (modified       "modified     ")
    (rename-source  "rename-source")
    (rename-target  "rename-target")
    (unknown        "unknown      ")))

(defvar dvc-status-ewoc nil
  "Ewoc for the status buffer.

Element is one of:
  (file dvc-status-fileinfo)
  (dir  dvc-status-fileinfo)
  (message \"<message>\")

A file element specifies the status of a file in the workspace.

A message element gives an informative message.")

(defun dvc-status-printer (elem)
  "Ewoc pretty-printer for `dvc-status-ewoc'."
  (ecase (car elem)
    ((dir file)
     (let* ((fileinfo (cadr elem))
            (line (concat
                   (dvc-status-status-image (dvc-status-fileinfo-status fileinfo))
                   " "
                   (dvc-status-fileinfo-dir fileinfo)
                   (dvc-status-fileinfo-file fileinfo)))
            (face (if (dvc-status-fileinfo-mark fileinfo)
                     'dvc-marked
                   (dvc-status-chose-face (dvc-status-fileinfo-status fileinfo)))))

       (insert " ")
       (if (dvc-status-fileinfo-mark fileinfo)
          (insert dvc-mark)
        (insert " "))

       (insert " ")
       (insert (dvc-face-add line face))))

   (message
    (insert (cadr elem)))
  ))

(defun dvc-status-current-fileinfo ()
  "Return the fileinfo (a dvc-status-fileinfo struct) for the current
file. Throws an error if point is on a message."
  (let ((elem (ewoc-data (ewoc-locate dvc-status-ewoc))))
    (ecase (car elem)
      ((dir file)
       (cadr elem))

      (message
       (error "not on a fileinfo")))))

(defun dvc-status-fileinfo-dir-file (fileinfo)
  "Return directory and file from fileinfo, as a string."
  (concat (dvc-status-fileinfo-dir fileinfo) (dvc-status-fileinfo-file fileinfo)))

(defun dvc-status-current-file ()
  "Return a string giving the filename (including path from root)
of the file element on the line at point. Throws an error if
point is not on a file element line."
  (let ((fileinfo (dvc-status-current-fileinfo)))
    (dvc-status-fileinfo-dir-file fileinfo)))

(defun dvc-status-dtrt ()
  "Do The Right Thing in a status buffer; update, commit, resolve
conflicts, and/or ediff current files."
  (interactive)

  (let (status)
    ;; Note that message elements cannot be marked. Make sure all
    ;; selected files need the same action.
    (if (< 1 (length dvc-buffer-marked-file-list))
        (ewoc-map (lambda (elem)
                    (ecase (car elem)
                      (message
                       nil)
                      ((dir file)
                       (let ((fileinfo (cadr elem)))
                         (if status
                             (if (not (equal status (dvc-status-fileinfo-status fileinfo)))
                                 (error "cannot Do The Right Thing on files with different status"))
                           (setq status (dvc-status-fileinfo-status fileinfo))))))
                    ;; don't redisplay the element
                    nil)
                  dvc-status-ewoc)
      (setq status (dvc-status-fileinfo-status (dvc-status-current-fileinfo))))

    (ecase status
      (added
       ;; Don't offer Remove here; not a common action. Just start or
       ;; continue a commit log entry.
       (dvc-logframe))

      (deleted
       (dvc-logframe))

      (missing
       ;; File is in database, but not in workspace
       (ding)
       (dvc-offer-choices (concat file " does not exist in working directory")
                          '((dvc-mode-update "update")
                            (dvc-mode-remove "remove"))))

      (modified
       ;; Don't offer undo here; not a common action
       ;; Assume user has started the commit log frame
       (if (< 1 (length dvc-buffer-marked-file-list))
           (error "cannot diff more than one file"))
       (dvc-status-ediff))

      ((or rename-source rename-target)
       (dvc-logframe))

      (unknown
       (dvc-offer-choices nil
                          '((dvc-add "add")
                            (dvc-ignore "ignore")
                            (dvc-delete "delete"))))
      )))

(defvar dvc-status-mode-map
  (let ((map (make-sparse-keymap)))
    ;; grouped by major function, then alphabetical by dvc-keyvec name, then by key
    ;; main group
    (define-key map dvc-keyvec-add      'dvc-status-add-files)
    (define-key map dvc-keyvec-commit   'dvc-log-edit)
    (define-key map dvc-keyvec-ediff    'dvc-status-ediff)
    (define-key map dvc-keyvec-help     'describe-mode)
    (define-key map dvc-keyvec-logs     'dvc-log)
    (define-key map dvc-keyvec-mark     'dvc-status-mark-file)
    (define-key map dvc-keyvec-mark-all 'dvc-status-mark-all)
    (define-key map dvc-keyvec-next     'dvc-status-next)
    (define-key map dvc-keyvec-previous 'dvc-status-prev)
    (define-key map dvc-keyvec-quit     'dvc-buffer-quit)
    (define-key map dvc-keyvec-refresh  'dvc-status-refresh)
    (define-key map dvc-keyvec-revert   'dvc-revert-files)
    (define-key map dvc-keyvec-unmark   'dvc-status-unmark-file)
    (define-key map dvc-keyvec-unmark-all   'dvc-status-unmark-all)
    (define-key map [?i]                'dvc-status-ignore-files) ; FIXME: ?i collides with dvc-key-inventory
    (define-key map [?I]                'dvc-ignore-file-extensions-in-dir)
    (define-key map "\M-I"              'dvc-ignore-file-extensions)
    (define-key map [?k]                'dvc-status-kill)
    (define-key map dvc-keyvec-remove   'dvc-status-remove-files)
    (define-key map "\r"                'dvc-find-file-other-window)
    (define-key map "\M-d"              'dvc-status-dtrt)

    ;; database operations
    (define-key map (dvc-prefix-merge ?u) 'dvc-update)
    (define-key map (dvc-prefix-merge ?s) 'dvc-sync)
    (define-key map (dvc-prefix-merge ?m) 'dvc-missing)

    ;; advanced
    (define-key map (dvc-prefix-tagging-method ?e) 'dvc-edit-ignore-files)
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)

    map)
  "Keymap used in `dvc-status-mode'.")

(easy-menu-define dvc-status-mode-menu dvc-status-mode-map
  "`dvc-status' menu"
  ;; FIXME: add all keymap operations to menu
  `("DVC"
    ["Refresh Buffer"          dvc-generic-refresh        t]
    ["Edit log before commit"  dvc-status-edit-log        t]
    ("Database"
     ["Update"                 dvc-update                 t]
     ["Sync"                   dvc-sync                   t]
     ["Show missing"           dvc-missing                t]
     )
    ("Ignore"
     ["Ignore Files"           dvc-ignore-files           t]
     ["Ignore Extensions in dir" dvc-ignore-file-extensions-in-dir t]
     ["Ignore Extensions globally" dvc-ignore-file-extensions t]
     ["Edit Ignore File"       dvc-edit-ignore-files      t]
     )
    ["Ediff File"              dvc-status-ediff           t]
    ["Delete File"             dvc-status-remove-files    t]
    ["Revert File"             dvc-revert-files           t]
    ))

(define-derived-mode dvc-status-mode nil "dvc-status"
  "Major mode to display workspace status."
  (set (make-local-variable 'dvc-status-ewoc) (ewoc-create 'dvc-status-printer))
  (set (make-local-variable 'dvc-get-file-info-at-point-function) 'dvc-status-current-file)
  (set (make-local-variable 'dvc-buffer-marked-file-list) nil)
  (use-local-map dvc-status-mode-map)
  (easy-menu-add dvc-status-mode-menu)
  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(defun dvc-status-prepare-buffer (dvc root base-revision branch header-more refresh)
  "Prepare and return a status buffer. Should be called by backend-dvc-status.
DVC is backend.
ROOT is absolute path to workspace.
BASE-REVISION is a string identifying the workspace's base revision.
BRANCH is a string identifying the workspace's branch.
HEADER-MORE is a function called to add other text to the ewoc header;
it should return a string, which is added to the header with princ.
REFRESH is a function that refreshes the status; see `dvc-buffer-refresh-function'."

  (let ((status-buffer (dvc-get-buffer-create dvc 'status root)))
    (dvc-kill-process-maybe status-buffer)
    (with-current-buffer status-buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (dvc-status-mode)
      (let ((header (with-output-to-string
                      (princ (format "Status for %s:\n" root))
                      (princ (format "  base revision : %s\n" base-revision))
                      (princ (format "  branch        : %s\n" branch))
                      (if (functionp header-more) (princ (funcall header-more)))))
            (footer ""))
        (set (make-local-variable 'dvc-buffer-refresh-function) refresh)
        (ewoc-filter dvc-status-ewoc (lambda (elem) nil))
        (ewoc-set-hf dvc-status-ewoc header footer)
        (ewoc-enter-last dvc-status-ewoc `(message ,(format "Running %s..." dvc)))
        (ewoc-refresh dvc-status-ewoc)))
    (dvc-switch-to-buffer-maybe status-buffer)))

(defun dvc-status-inventory-done (status-buffer)
  (with-current-buffer status-buffer
    (ewoc-enter-last dvc-status-ewoc '(message "Parsing inventory..."))
    (ewoc-refresh dvc-status-ewoc)
    (redisplay t)
    ;; delete "running", "parsing" from the ewoc now, but don't
    ;; refresh until the status is displayed
    (dvc-status-delete-messages)))

(defun dvc-status-refresh ()
  "Refresh the buffer."
  (interactive)
  (dvc-status default-directory))

;; ewoc operations
(defun dvc-status-delete-messages ()
  "Remove messages from the ewoc list of modifications."
  (ewoc-filter dvc-status-ewoc
               (lambda (elem)
                 (not (eq (car elem) 'message)))))

(defun dvc-status-kill ()
  "Remove the current element from the ewoc."
  (interactive)

  ;; if marked, also need to delete from dvc-buffer-marked-file-list
  (let ((fileinfo (dvc-status-current-fileinfo)))
    (if (dvc-status-fileinfo-mark fileinfo)
        (setq dvc-buffer-marked-file-list (delete (dvc-status-fileinfo-dir-file fileinfo) dvc-buffer-marked-file-list))))

  ;; binding inhibit-read-only doesn't seem to work here
  (toggle-read-only 0)
  (ewoc-delete dvc-status-ewoc (ewoc-locate dvc-status-ewoc))
  (toggle-read-only 1))

(defun dvc-status-mark-dir (ewoc dir mark)
  "In EWOC, set the mark for all files in DIR to MARK, recursively."
  (let ((dir-compare (file-name-as-directory dir)))
    (ewoc-map (lambda (elem)
                (ecase (car elem)
                  ((dir file)
                   (let* ((fileinfo (cadr elem))
                         (file (dvc-status-fileinfo-dir-file fileinfo)))
                     (if (string-equal dir-compare (dvc-status-fileinfo-dir fileinfo))
                         (progn
                           (setf (dvc-status-fileinfo-mark fileinfo) mark)
                           (if mark
                               (progn
                                 (add-to-list 'dvc-buffer-marked-file-list file))
                             (setq dvc-buffer-marked-file-list (delete file dvc-buffer-marked-file-list)))
                           (ecase (car elem)
                             (dir
                              (dvc-status-mark-dir ewoc file mark)
                              ;; return non-nil so this element is refreshed
                              t)

                             (file
                              ;; return non-nil so this element is refreshed
                              t))))))

                  (message nil)))
              dvc-status-ewoc)))

(defun dvc-status-mark-file-1 (mark)
  "Set the mark for file under point to MARK. If a directory, mark all files in that directory."
  (let* ((current (ewoc-locate dvc-status-ewoc))
         (elem (ewoc-data current)))
    (ecase (car elem)
      (dir
       (let* ((fileinfo (cadr elem))
              (file (dvc-status-fileinfo-dir-file fileinfo)))
         (setf (dvc-status-fileinfo-mark fileinfo) mark)
         (if mark
             (add-to-list 'dvc-buffer-marked-file-list file)
           (setq dvc-buffer-marked-file-list (delete file dvc-buffer-marked-file-list)))
         (ewoc-invalidate dvc-status-ewoc current)
         (dvc-status-mark-dir dvc-status-ewoc file mark)))

      (file
       (let* ((fileinfo (cadr elem))
              (file (dvc-status-fileinfo-dir-file fileinfo)))
         (setf (dvc-status-fileinfo-mark fileinfo) mark)
         (if mark
             (add-to-list 'dvc-buffer-marked-file-list file)
           (setq dvc-buffer-marked-file-list (delete file dvc-buffer-marked-file-list)))
         (ewoc-invalidate dvc-status-ewoc current)
         (dvc-status-next)))

      (message
       (error "not on a fileinfo")))))

(defun dvc-status-mark-file ()
  "Mark the file under point. If a directory, mark all files in that directory."
  (interactive)
  (dvc-status-mark-file-1 t))

(defun dvc-status-unmark-file ()
  "Unmark the file under point. If a directory, unmark all files in that directory."
  (interactive)
  (dvc-status-mark-file-1 nil))

(defun dvc-status-mark-all ()
  "Mark all files."
  (interactive)
  (ewoc-map (lambda (elem)
              (let ((fileinfo (cadr elem)))
                (setf (dvc-status-fileinfo-mark fileinfo) t)
                (add-to-list 'dvc-buffer-marked-file-list (dvc-status-fileinfo-dir-file fileinfo))
                ;; return non-nil so this element is refreshed
                t))
            dvc-status-ewoc))

(defun dvc-status-unmark-all ()
  "Unmark all files."
  (interactive)
  (setq dvc-buffer-marked-file-list nil)
  (ewoc-map (lambda (elem)
              (let ((fileinfo (cadr elem)))
                (if (dvc-status-fileinfo-mark fileinfo)
                    (progn
                      (setf (dvc-status-fileinfo-mark fileinfo) nil)
                      ;; return non-nil so this element is refreshed
                      t))))
            dvc-status-ewoc))

(defun dvc-status-next ()
  "Move to the next file."
  (interactive)
  (let* ((current (ewoc-locate dvc-status-ewoc))
         (cur-location (ewoc-location current))
         (next (ewoc-next dvc-status-ewoc current)))
    (cond
     ((> cur-location (point))
      ;; not exactly at an element; move there
      (goto-char cur-location))
     (next
      (goto-char (ewoc-location next)))
     (t
      ;; at last element
      nil))))

(defun dvc-status-prev ()
  "Move to the previous file."
  (interactive)
  (let* ((current (ewoc-locate dvc-status-ewoc))
         (cur-location (ewoc-location current))
         (prev (ewoc-prev dvc-status-ewoc current)))
    (cond
     ((> (point) cur-location)
      (goto-char cur-location))
     (prev
      (goto-char (ewoc-location prev)))
     (t
      ;; at first element
      nil))))

;; diff operations
(defun dvc-status-diff ()
  "Run diff of the current workspace file against the database version."
  (interactive)
  (let ((fileinfo (dvc-status-current-fileinfo))
        (file (concat (dvc-status-fileinfo-dir fileinfo) (dvc-status-fileinfo-file fileinfo))))
    ;; FIXME: need user interface to specify other revision to diff against
    ;; FIXME: dvc-file-diff defaults to buffer-file; change to default to (dvc-get-file-info-at-point)
    ;; default dvc-get-file-info-at-point to (buffer-file-name) for non-dvc buffers
    (dvc-file-diff file)))

(defun dvc-status-ediff ()
  "Run ediff on the current workspace file, against the database version."
  (interactive)
  ;; FIXME: need user interface to specify other revision to diff against
  ;; FIXME: dvc-file-ediff is in dvc-diff.el. promote to dvc-unified, default to (dvc-get-file-info-at-point)
  (dvc-file-ediff (dvc-status-current-file)))

;; database operations
(defun dvc-status-add-files ()
  "Add current files to the database. Directories are also added,
but not recursively."
  (interactive)
  (let* ((files (dvc-current-file-list))
         (filtered files))
    (dolist (file files)
      ;; FIXME: xmtn-dvc.el xmtn--add-files says on directories, "mtn
      ;; add" will recurse, which isn't what we want. but that's not
      ;; true for current monotone. bzr also does not recurse.
      ;;
      ;; Note that there is no "add recursive" DVC command. Selecting
      ;; all the files in a directory is the prefered approach.
      (if (file-directory-p file)
          (setq filtered (delete file filtered))))
    (apply 'dvc-add-files filtered))

  ;; Update the ewoc status of each added file to 'added'; this avoids
  ;; the need to run the backend again. Assume any directories that
  ;; were filtered out above were added because there were files in
  ;; them. FIXME: should verify that here.
  (if (= 0 (length dvc-buffer-marked-file-list))
      ;; no marked files
      (let ((fileinfo (dvc-status-current-fileinfo)))
        (setf (dvc-status-fileinfo-status fileinfo) 'added)
        (ewoc-invalidate dvc-status-ewoc (ewoc-locate dvc-status-ewoc)))
    ;; marked files
    (ewoc-map (lambda (elem)
                (let ((fileinfo (cadr elem)))
                  (if (dvc-status-fileinfo-mark fileinfo) (setf (dvc-status-fileinfo-status fileinfo) 'added))))
              dvc-status-ewoc)))

(defun dvc-status-ignore-files ()
  "Ignore current files."
  (interactive)
  (dvc-ignore-files (dvc-current-file-list))

  ;; kill the files from the ewoc, since we are ignoring them; this
  ;; avoids the need to run the backend again.
  (if (= 0 (length dvc-buffer-marked-file-list))
      ;; no marked files
      (progn
        ;; binding inhibit-read-only doesn't seem to work here
        (toggle-read-only 0)
        (ewoc-delete dvc-status-ewoc (ewoc-locate dvc-status-ewoc))
        (toggle-read-only 1))
    ;; marked files
    (setq dvc-buffer-marked-file-list nil)
    (ewoc-filter dvc-status-ewoc
                 (lambda (elem)
                   (let ((fileinfo (cadr elem)))
                     (not (dvc-status-fileinfo-mark fileinfo))))
                 )))

(defun dvc-status-remove-files ()
  "Remove current files."
  (interactive)
  (apply 'dvc-remove-files (dvc-current-file-list))

  ;; kill the files from the ewoc, since we are removing them; this
  ;; avoids the need to run the backend again.
  (if (= 0 (length dvc-buffer-marked-file-list))
      ;; no marked files
      (ewoc-delete dvc-status-ewoc (ewoc-locate dvc-status-ewoc))
    ;; marked files
    (setq dvc-buffer-marked-file-list nil)
    (ewoc-filter dvc-status-ewoc
                 (lambda (elem)
                   (let ((fileinfo (cadr elem)))
                     (not (dvc-status-fileinfo-mark fileinfo))))
                 )))

(provide 'dvc-status)
;;; end of file
