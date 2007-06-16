;;; dvc-diff.el --- A generic diff mode for DVC

;; Copyright (C) 2005-2007 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>

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

(require 'diff-mode)
(require 'dvc-ui)
(require 'dvc-unified)
(require 'dvc-defs)
(require 'dvc-core)
(eval-when-compile (require 'cl))


(defvar dvc-diff-base nil
  "BASE revision for the changes currently displayed.

Must be buffer-local.")

(defvar dvc-diff-modified nil
  "MODIFIED revision for the changes currently displayed.

Must be buffer-local.")

(defvar dvc-buffer-search-file nil
  "Function to search a file inside a diff")

(defun dvc-dvc-search-file-in-diff (file)
  (re-search-forward (concat "^\\+\\+\\+ \\(a\\|mod\\)/" file "$")))

(defun dvc-prepare-changes-buffer (base modified type path dvc)
  "Create and return a buffer to run command showing diffs.

Sets the local-variables `dvc-diff-base' and
`dvc-diff-modified' are set according to BASE and MODIFIED.

TYPE and PATH are passed to `dvc-get-buffer-create'."
  (with-current-buffer
      (dvc-get-buffer-create dvc type path)
    (let ((inhibit-read-only t)) (erase-buffer))
    (dvc-diff-mode)
    (set (make-local-variable 'dvc-diff-base)     base)
    (set (make-local-variable 'dvc-diff-modified) modified)
    (set (make-local-variable 'dvc-buffer-search-file)
         (dvc-function dvc "dvc-search-file-in-diff"))
    ;;(dvc-trace "dvc=%S function=%S" dvc dvc-buffer-search-file)
    (current-buffer)))

(defun dvc-diff-chose-face (status modif)
  "Return a face adapted to MODIF, a string, which can be A, M, C, or D."
  (cond
   ((string= "A" status) 'dvc-added)
   ((string= "?" status) 'dvc-unknown)
   ((string= "M" modif) 'dvc-modified)
   ((string= "M" status) 'dvc-modified)
   ((string= "-" modif)  'dvc-modified)
   ((string= "P" status) 'dvc-modified)
   ((string= "C" status) 'dvc-conflict)
   ((string= "D" status) 'dvc-conflict)
   ((string= "R" status) 'dvc-move)
   ((string= " " status) 'default)
   (t
    (dvc-trace "unknown status=%S or modif=%S" status modif)
    'default)))

;; ----------------------------------------------------------------------------
;; dvc-diff-mode
;; ----------------------------------------------------------------------------
;;
;; Changes mode
;;
(defvar dvc-diff-cookie nil
  "Ewoc cookie for the changes buffer.

Element should look like one of:

 (file \"filename\" \"[CRADP?]\" \"M\" \"/\" \"origname\")

To mean that FILENAME has modifications (the two first fields are
letters, inspired by baz \"status\" output, the third one is \"/\" in
the case of a directory, and the fourth one is the original name in
the case of a renamed file.


 (subtree \"name\" related-buffer changes?)

To make a reference to a subtree (in which another dvc-diff is
probably running).


 (message \"doing such or such thing\")

For informative messages.")

(defun dvc-diff-printer (elem)
  "Ewoc pretty-printer for `dvc-diff-cookie'.

Pretty-print ELEM."
  (cond
   ((eq (car elem) 'file)
    (let* ((empty-mark " ")
           (mark (when (member (cadr elem) dvc-buffer-marked-file-list)
                   dvc-mark))
           (file     (nth 1 elem))
           (status   (nth 2 elem))
           (modif    (nth 3 elem))
           (dir      (nth 4 elem))
           (origname (nth 5 elem))
           (line (concat status modif " "
                         (when origname (concat origname dir "\t => "))
                         file dir))
           (face (if mark
                     'dvc-marked
                   (dvc-diff-chose-face status modif))))
      (if mark
          (insert mark)
        (insert empty-mark))
      (insert (dvc-face-add line
                             face
                             'dvc-diff-file-map
                             dvc-diff-file-menu))))
   ((eq (car elem) 'subtree)
    (insert (dvc-face-add
             (concat " T" (cond ((not (cadddr elem)) "?")
                                ((eq  (cadddr elem) 'changes) "M")
                                ((eq  (cadddr elem) 'updated) "U")
                                ((eq  (cadddr elem) 'no-changes) "-"))
                     " " (car (cddr elem)))
             'dvc-nested-tree)))
   ((eq (car elem) 'message)
    (insert (cadr elem)))
   ((eq (car elem) 'searching-subtrees)
    (insert (dvc-face-add " T  Searching for subtrees ..."
                           'dvc-nested-tree))))
  )

(defvar dvc-diff-mode-map
  (let ((map (copy-keymap diff-mode-shared-map)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map "\C-m"   'dvc-diff-jump-to-change)
    (define-key map [return] 'dvc-diff-jump-to-change)
    (define-key map [(control x) (control j)] 'dvc-dired-jump)
    (define-key map "\M-="          'dvc-diff-scroll-up-or-diff)
    (define-key map [(meta return)] 'dvc-diff-scroll-down-or-diff)
    (define-key map "\M-\C-m"       'dvc-diff-scroll-down-or-diff)
    (define-key map [?=] 'dvc-diff-diff)
    (define-key map dvc-keyvec-add  'dvc-add-files)
    (define-key map [?h] 'dvc-buffer-pop-to-partner-buffer)
    (define-key map dvc-keyvec-ediff   'dvc-diff-ediff)
    (define-key map dvc-keyvec-refresh 'dvc-generic-refresh)
    (define-key map dvc-keyvec-commit  'dvc-log-edit)
    ;; TODO move this somewhere else.
    (define-key map [?I] 'tla-inventory)
    (define-key map dvc-keyvec-inventory 'dvc-pop-to-inventory)

    (define-key map dvc-keyvec-next      'dvc-diff-next)
    (define-key map dvc-keyvec-previous  'dvc-diff-prev)
    (define-key map dvc-keyvec-revert    'dvc-revert-files)
    (define-key map dvc-keyvec-quit      'dvc-buffer-quit)
    (define-key map [?d] 'dvc-remove-files)
    (define-key map dvc-keyvec-mark   'dvc-diff-mark-file)
    (define-key map dvc-keyvec-unmark 'dvc-diff-unmark-file)
    (define-key map [?v] 'dvc-diff-view-source)
    (define-key map dvc-keyvec-parent 'dvc-diff-master-buffer)
    (define-key map [?j] 'dvc-diff-diff-or-list)
    (define-key map (dvc-prefix-kill-ring ?d) 'dvc-diff-save-current-defun-as-kill)

    ;; Buffers group
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)

    ;; Ignore file handling
    (define-key map (dvc-prefix-tagging-method ?i) 'dvc-ignore-files)
    (define-key map (dvc-prefix-tagging-method ?I) 'dvc-ignore-file-extensions)
    ;; C-i is not a good choice since it's TAB in emacs -nw
    (define-key map (dvc-prefix-tagging-method ?e) 'dvc-edit-ignore-files)

    ;; working copy bindings
    (define-key map (vector dvc-key-working-copy) nil) ;; unbind ?W, before it can be used
    (define-key map (dvc-prefix-working-copy ?s) 'dvc-save-diff)

    ;; the merge group
    (define-key map (dvc-prefix-merge ?u) 'dvc-update)
    (define-key map (dvc-prefix-merge ?f) 'dvc-pull) ;; hint: fetch, p is reserved for push
    (define-key map (dvc-prefix-merge ?m) 'dvc-missing)
    map)
  "Keymap used in `dvc-diff-mode'.")

;;
;; Menu
;;
(defconst dvc-diff-file-menu-list
  '("File Changes"
    ["Jump to File"                   dvc-diff-jump-to-change t]
    ["Jump to Diffs"                  dvc-diff-diff-or-list   t]
    ["View Diff in Separate Buffer"   dvc-diff-diff           t]
    ["View Diff with Ediff"           dvc-diff-ediff          t]
    "--"
    ["Delete File"                    dvc-remove-files        t]
    ["Revert File"                    dvc-revert-files        t]
    )
  "Used both in the global and the context menu of `dvc-diff-mode'.")

(easy-menu-define dvc-diff-file-menu nil
  "Menu used on a `dvc-diff' file"
  dvc-diff-file-menu-list)

(easy-menu-define dvc-diff-mode-menu dvc-diff-mode-map
  "`tla-changes' menu"
  `("Changes"
    ["Refresh Buffer" dvc-generic-refresh t]
    ["Edit log before commit" dvc-diff-edit-log t]
    ["View other revisions" tla-tree-revisions t]
    ("Merge"
     ["Update" dvc-update t]
     ["Pull" dvc-pull t]
     ["Show missing" dvc-missing t]
     )
    ("Ignore"
     ["Ignore Files" dvc-ignore-files t]
     ["Ignore File Extensions" dvc-ignore-file-extensions t]
     ["Edit Ignore File" dvc-edit-ignore-files t]
     )
    ,dvc-diff-file-menu-list
    ))

(defvar dvc-diff-file-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map dvc-mouse-2 'dvc-diff-jump-to-change-by-mouse)
    map)
  "Keymap used on files in `dvc-diff-mode' buffers.")

(define-derived-mode dvc-diff-mode fundamental-mode "dvc-diff"
  "Major mode to display changesets. Derives from `diff-mode'.

Use '\\<dvc-diff-mode-map>\\[dvc-diff-mark-file]' to mark files, and '\\[dvc-diff-unmark-file]' to unmark.
If you commit from this buffer (with '\\<dvc-diff-mode-map>\\[dvc-diff-edit-log]'), then,
the list of selected files (in this buffer) will be commited (with the text you
entered as a comment) at the time you actually commit with \\<dvc-log-edit-mode-map>\\[tla-log-edit-done].

Commands:
\\{dvc-diff-mode-map}
"
  (let ((diff-mode-shared-map (copy-keymap dvc-diff-mode-map))
        major-mode mode-name)
    (diff-mode))

  (set (make-local-variable 'font-lock-defaults)
       (list 'tla-changes-font-lock-keywords t nil nil))
  (set (make-local-variable 'dvc-get-file-info-at-point-function)
       'dvc-diff-get-file-at-point)
  (set (make-local-variable 'dvc-buffer-refresh-function)
       'dvc-diff-generic-refresh)
  (set (make-local-variable 'dvc-diff-cookie)
       (ewoc-create (dvc-ewoc-create-api-select
		     #'dvc-diff-printer)))
  (make-local-variable 'dvc-buffer-marked-file-list)
  (easy-menu-add dvc-diff-mode-menu)
  (dvc-install-buffer-menu)
  (toggle-read-only 1)
  (set-buffer-modified-p nil))

(defun dvc-diff-generic-refresh ()
  "Refresh the changes buffer."
  (interactive)
  (if (eq (dvc-revision-get-type dvc-diff-modified) 'local-tree)
      (dvc-diff dvc-diff-base)
    (error "Don't know how to refresh buffer")))

(defun dvc-diff-jump-to-change (&optional other-file)
  "Jump to the corresponding file and location of the change.
The prefix argument OTHER-FILE controls whether the original or new
file is visited."
  (interactive "P")
  (let* ((elem (ewoc-locate dvc-diff-cookie))
         (data (ewoc-data elem)))
    (cond ((< (ewoc-location elem) (line-beginning-position))
           (dvc-diff-diff-goto-source other-file))
          ((eq (car data) 'file)
           (find-file (cadr data)))
          ((eq (car data) 'subtree)
           (dvc-switch-to-buffer (cadr data)))
          (t (error "Not on a recognized location")))))

(defun dvc-diff-scroll-or-diff (up-or-down)
  "If file-diff buffer is visible, scroll. Otherwise, show it."
  (interactive)
  (let ((file (dvc-get-file-info-at-point)))
    (unless file
      (error "No file info at point."))
    ;; TODO
    (let ((buffer (dvc-get-buffer tla-arch-branch 'file-diff file)))
      (unless (tla--scroll-maybe buffer up-or-down)
        (tla-file-diff file nil t)))))

(defun dvc-diff-scroll-up-or-diff ()
  (interactive)
  (dvc-diff-scroll-or-diff 'scroll-up))

(defun dvc-diff-scroll-down-or-diff ()
  (interactive)
  (dvc-diff-scroll-or-diff 'scroll-down))

(defun dvc-diff-diff-goto-source (other-file)
  "Almost the same as `diff-goto-source'.
But the target file is transformed by `tla--changes-what-changed-original-file'
to handle files in what-changed directory.
OTHER-FILE controls whether the original or new file is visited."
  (let ((dvc-original-file-exists-p (symbol-function
                                     'file-exists-p))
        (dvc-original-find-file-noselect (symbol-function
                                          'find-file-noselect)))
    (flet ((file-exists-p (file)
                          (unless (string= "/dev/null" file)
                            (funcall
                             dvc-original-file-exists-p
                             ;; TODO make this generic.
                             ;; <DVC>-diff-get-filename
                             (tla--changes-what-changed-original-file file))))
           (find-file-noselect (file &optional nowarn rawfile wildcards)
             (if (featurep 'xemacs)
                 (funcall dvc-original-find-file-noselect
                          (tla--changes-what-changed-original-file file)
                          nowarn rawfile)
               (funcall dvc-original-find-file-noselect
                        (tla--changes-what-changed-original-file file)
                        nowarn rawfile wildcards))))
      (diff-goto-source other-file))))

(defun dvc-diff-delete-messages (&optional immediate)
  "Remove messages from the ewoc list of modifications.

if IMMEDIATE is non-nil, refresh the display too."
  (when dvc-diff-cookie
    (ewoc-filter dvc-diff-cookie
                 (lambda (elem)
                   (not (eq (car elem) 'message))))))

(defun dvc-diff-diff-or-list ()
  "Move around the changes buffer.
When in the list part of the buffer, jump to the corresponding
patch. When on a patch, jump to the corresponding entry in the list of
files."
  (interactive)
  (let* ((elem (ewoc-locate dvc-diff-cookie))
         (data (ewoc-data elem)))
    (cond ((< (ewoc-location elem) (line-beginning-position))
           (let ((file (dvc-diff-get-file-at-point))
                 (elem (ewoc-nth dvc-diff-cookie 0)))
             (while (and elem
                         (or (not (eq (car (ewoc-data elem)) 'file))
                             (not (string= (expand-file-name
                                            (cadr (ewoc-data elem)))
                                           file))))
               (setq elem (ewoc-next dvc-diff-cookie elem)))
             (if elem (goto-char (ewoc-location elem))
               (error (format "Can't find file %s in list" file)))
             ))
          ((eq (car data) 'file)
           (funcall dvc-buffer-search-file (cadr data))
           (diff-hunk-next))
          ((eq (car data) 'subtree)
           (dvc-switch-to-buffer (cadr data)))
          (t (error "Not on a recognized location")))))

(defun dvc-diff-mark-file ()
  "Mark the file under point."
  (interactive)
  (if (eq (car (ewoc-data (ewoc-locate dvc-diff-cookie)))
          'message)
      (dvc-diff-mark-group)
    (let ((current (ewoc-locate dvc-diff-cookie))
          (file (dvc-get-file-info-at-point)))
      (add-to-list 'dvc-buffer-marked-file-list file)
      (ewoc-invalidate dvc-diff-cookie current)
      (goto-char (ewoc-location (or (ewoc-next dvc-diff-cookie
                                               current)
                                    current))))))

(defun dvc-diff-mark-group (&optional unmark)
  "Mark a group of file.

Must be called with the cursor on a 'message ewoc entry. Marks all
files untill the end of the ewoc, or the next ewoc entry which is not
a 'file."
  (goto-char (ewoc-location (ewoc-next dvc-diff-cookie
                                       (ewoc-locate
                                        dvc-diff-cookie))))
  (let ((ewoc-elem (ewoc-locate dvc-diff-cookie)))
    (while (and ewoc-elem
                (ewoc-data ewoc-elem)
                (eq (car (ewoc-data ewoc-elem))
                    'file))
      (let ((file (cadr (ewoc-data ewoc-elem))))
        (dvc-trace "mark/unmark %S" file)
        (if unmark
            (setq dvc-buffer-marked-file-list
                  (delete file dvc-buffer-marked-file-list))
          (add-to-list 'dvc-buffer-marked-file-list file)))
      (setq ewoc-elem (ewoc-next dvc-diff-cookie ewoc-elem)))
    (ewoc-refresh dvc-diff-cookie)
    (if ewoc-elem
        (goto-char (ewoc-location ewoc-elem))
      (goto-char (point-max)))))


(defun dvc-diff-unmark-file ()
  "Unmark the file under point."
  (interactive)
  (if (eq (car (ewoc-data (ewoc-locate dvc-diff-cookie)))
          'message)
      (dvc-diff-mark-group t)
    (let ((current (ewoc-locate dvc-diff-cookie))
          (file (dvc-get-file-info-at-point)))
      (setq dvc-buffer-marked-file-list
            (delete file dvc-buffer-marked-file-list))
      (ewoc-invalidate dvc-diff-cookie current)
      (goto-char (ewoc-location (or (ewoc-next dvc-diff-cookie
                                               current)
                                    current))))))

(defun dvc-diff-diff ()
  "Run tla file-diff on the file at point in *{tla|baz}-changes*."
  (interactive)
  (let ((on-modified-file (dvc-get-file-info-at-point)))
    (if on-modified-file
        (let ((buf (current-buffer)))
          (dvc-file-diff on-modified-file dvc-diff-base
                         dvc-diff-modified t)
          (pop-to-buffer buf))
      (error "Not on a modified file"))))

(defun dvc-diff-next ()
  "Move to the next changes."
  (interactive)
  (let ((cur-location (ewoc-location (ewoc-locate dvc-diff-cookie)))
        (next (ewoc-next dvc-diff-cookie
                         (ewoc-locate dvc-diff-cookie))))
    (cond
     ((> cur-location (point))
      (goto-char cur-location))
     (next
      (goto-char (ewoc-location next)))
     (t
      (diff-hunk-next)))))

(defun dvc-diff-prev ()
  "Move to the previous changes."
  (interactive)
  (let* ((current (ewoc-locate dvc-diff-cookie))
         (cur-location (ewoc-location current))
         (prev (ewoc-prev dvc-diff-cookie current))
         (next (ewoc-next dvc-diff-cookie current)))
    (cond (next
           (if prev (goto-char (ewoc-location prev))
             (goto-char cur-location)))
          ((condition-case nil (progn (diff-hunk-prev) t) (error nil)))
          ((> (line-beginning-position) cur-location)
           (goto-char cur-location))
          (prev
           (goto-char (ewoc-location prev)))
          (t
           (goto-char cur-location)))
    ))

;; TODO make this generic
(defun tla--changes-in-diff ()
  "Return t if cursor is in the diffs section of the changes buffer."
  (save-excursion (re-search-backward "^--- " nil t)))


(defun dvc-diff-ediff (&optional other-file)
  "Run ediff on the current changes.
The prefix argument OTHER-FILE controls whether the original or new
file is visited."
  (interactive "P")
  (unless (and (car dvc-diff-base)
               (car dvc-diff-base))
    (error "No revision information to base ediff on"))
  (let ((on-modified-file (dvc-get-file-info-at-point))
        (loc (point)))
    (if (and on-modified-file (not (tla--changes-in-diff)))
        (dvc-file-ediff-revisions on-modified-file
                                  dvc-diff-modified
                                  dvc-diff-base)
      (end-of-line)
      (dvc-trace "loc=%S" loc)
      (let ((hunk 1))
        (re-search-backward "^--- ")
        (re-search-forward "^--- ")
        (diff-hunk-next)
        (while (<= (re-search-forward "\\(^[\\+-].*\n\\)+" nil t) loc)
          (dvc-trace "hunk=%S" hunk)
          (setq hunk (1+ hunk)))
        (goto-char loc)
        (with-current-buffer
            (dvc-file-ediff-revisions on-modified-file dvc-diff-modified
                                      dvc-diff-base)
          (ediff-jump-to-difference hunk))))))

(defun dvc-diff-find-file-name ()
  "Same as `diff-find-file-name', but works in more cases."
  (cond ((re-search-backward "^\\+\\+\\+ \\(mod/\\)?\\([^\n]*?\\)\\([ \t].*\\)?$" nil t)
         (match-string-no-properties 2))
        ((not (ewoc-locate dvc-diff-cookie (point))) ;; the buffer contains no diff
         "")
        (t
         (diff-find-file-name))))

(defun dvc-diff-get-file-at-point ()
  "Find file at point in *<backend>-changes*.
Throw an error when not on a file."
  (save-excursion
    (let ((elem (ewoc-locate dvc-diff-cookie (point))))
      (or (when (and elem
                     (eq (car (ewoc-data elem)) 'file)
                     (>= (ewoc-location elem) (line-beginning-position)))
            (cadr (ewoc-data elem)))
          (expand-file-name (concat (file-name-as-directory
                                     default-directory)
                                    (dvc-diff-find-file-name)))))))

(defvar dvc-header nil
  "Free variable used to pass info from the parser to
  `dvc-show-changes-buffer'.")
;; FIXME: actually, dvc-show-changes-buffer doesn't use this

(defun dvc-show-changes-buffer (buffer parser &optional
                                       output-buffer no-switch header-end-regexp)
  ;; FIXME: pass in dvc?
  "Show the *{dvc}-changes* buffer built from the *{dvc}-process* BUFFER.

PARSER is a function to parse the diff and fill in the ewoc list.

Display changes in OUTPUT-BUFFER if non-nil; otherwise create a
new display buffer.

If NO-SWITCH is nil, don't switch to the created buffer.

If non-nil, HEADER-END-REGEXP is a regexp matching the first line
which is not part of the diff header."
  (let* ((root (with-current-buffer buffer
                 (dvc-tree-root default-directory t)))
         (changes-buffer (or output-buffer (dvc-get-buffer-create (dvc-current-active-dvc)
                                            'diff root)))
         (dvc-header ""))
    (if (or no-switch dvc-switch-to-buffer-first)
        (set-buffer changes-buffer)
      (dvc-switch-to-buffer changes-buffer))
    (let (buffer-read-only)
      (dvc-diff-delete-messages)
      (unless output-buffer
        (erase-buffer)
        (dvc-diff-mode))
      (with-current-buffer buffer
        (goto-char (point-min))
        (when header-end-regexp
          (setq dvc-header (buffer-substring-no-properties
                            (goto-char (point-min))
                            (progn (re-search-forward header-end-regexp nil t) ;; "^[^*\\.]"
                                   (beginning-of-line)
                                   (point)))))
        (beginning-of-line)
        (funcall parser changes-buffer)
        (let ((footer (concat
                       (dvc-face-add (make-string  72 ?\ ) 'dvc-separator)
                       "\n\n"
                       (buffer-substring-no-properties
                        (point) (point-max)))))
          (with-current-buffer changes-buffer
            (ewoc-set-hf dvc-diff-cookie dvc-header footer)
            (if root (cd root)))))))
  (toggle-read-only 1)
  (if (progn (goto-char (point-min))
             (re-search-forward "^---" nil t))
      (when (or global-font-lock-mode font-lock-mode)
        (let ((font-lock-verbose nil))
          (font-lock-fontify-buffer)))
    ;; Disabling font-lock mode (it's useless and it removes other
    ;; faces with Emacs 21)
    (setq font-lock-keywords nil)
    (font-lock-mode -1)
    (ewoc-refresh dvc-diff-cookie))
  (if (ewoc-nth dvc-diff-cookie 0)
      (goto-char (ewoc-location (ewoc-nth dvc-diff-cookie 0)))))


(defun dvc-diff-no-changes (diff-buffer msg dir &optional
                                         master-buffer)
  "Function to call when there are no changes in a tree.

Inserts a message in the changes buffer, and in the minibuffer.

DIFF-BUFFER is the buffer in which the diff is computed
MSG is the format string for the message to the user (%s will be
substitued with the directory.
DIR is the directory in which the diff is computed.
MASTER-BUFFER, when non-nil, is the top-level buffer in the case of a
recursive command."
  (with-current-buffer diff-buffer
    (let ((inhibit-read-only t))
      (dvc-diff-delete-messages)
      (ewoc-enter-last dvc-diff-cookie
                       (list 'message (concat "* "
                                              (format msg dir)
                                              ".\n\n")))
      (when master-buffer
        (with-current-buffer master-buffer
          (ewoc-map (lambda (x)
                      (when (and (eq (car x) 'subtree)
                                 (eq (cadr x) buffer))
                        (setcar (cdr (cddr x)) 'no-changes))
                      )
                    ;; (ewoc-refresh dvc-diff-cookie)))
                    dvc-diff-cookie)))
      (ewoc-refresh dvc-diff-cookie)
      (recenter)))
  (message msg dir))

(defun dvc-diff-error-in-process (diff-buffer msg dir output error
                                               &optional
                                               master-buffer)
  "Similar to `dvc-diff-no-changes', but to report a real error.

OUTPUT and ERROR are the buffers containing the stdout and stderr
of the process that raised an error."
  (with-current-buffer diff-buffer
    (dvc-diff-delete-messages)
    (ewoc-enter-last
     dvc-diff-cookie
     (list 'message
           (concat "* " msg ":\n"
                   (dvc-buffer-content output)
                   (dvc-buffer-content error))))
    (ewoc-refresh dvc-diff-cookie)
    (recenter))
  (message msg))

(defun dvc-diff-clear-buffers (dvc dir mesg)
  "Clears all DVC diff buffers in directory DIR, insert message MSG.

Usefull to clear diff buffers after a commit."
  (dvc-trace "dvc-diff-clear-buffers (%S %S)"
              dir mesg)
  (dolist (buffer (list (dvc-get-buffer
                         dvc 'diff
                         (dvc-tree-root dir))
                        (dvc-get-buffer
                         dvc 'status
                         (dvc-tree-root dir))))
    (dvc-trace "buffer=%S" buffer)
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (ewoc-filter dvc-diff-cookie
                       (lambda (x) (eq (car x) 'subtree)))
          (ewoc-set-hf dvc-diff-cookie "" "")
          (ewoc-enter-first
           dvc-diff-cookie
           `(message ,mesg))
          (ewoc-refresh dvc-diff-cookie))))))

;;;###autoload
(defun dvc-file-ediff (file)
  (interactive (list (buffer-file-name)))
  (let ((file-buffer (find-file-noselect file))
        (pristine-buffer
         (dvc-revision-get-file-in-buffer
          file `(,(dvc-current-active-dvc)
                 (last-revision
                  ,(dvc-tree-root file t)
                  1)))))
    (with-current-buffer pristine-buffer
      (set-buffer-modified-p nil)
      (toggle-read-only 1)
      (let ((buffer-file-name file))
        (set-auto-mode t)))
    (dvc-ediff-buffers file-buffer pristine-buffer)))

;;;###autoload
(defun dvc-file-ediff-revisions (file &optional base modified)
  "View changes in FILE between BASE and MODIFIED using ediff."
  ;; TODO could be interactive
  (dvc-ediff-buffers
   (dvc-revision-get-file-in-buffer file base)
   (dvc-revision-get-file-in-buffer file modified)))

;;;###autoload
(defun dvc-dvc-file-diff (file &optional base modified dont-switch)
  "View changes in FILE between BASE and MODIFIED."
  (let* ((dvc (or (car base) (dvc-current-active-dvc)))
         (base (or base `(,dvc (last-revision ,file 1))))
         (modified (or modified `(,dvc (local-tree ,file)))))
    (let* ((buffer (dvc-get-buffer-create
                    'dvc ;; TODO anything ;; better ?
                    'file-diff
                    (dvc-uniquify-file-name file)))
           (base-buffer
            (dvc-revision-get-file-in-buffer file base))
           (modified-buffer
            (dvc-revision-get-file-in-buffer file modified))
           (base-file (make-temp-file "DVC-file-diff-base"))
           (modified-file (make-temp-file "DVC-file-diff-mod")))
      (with-temp-file base-file
        (insert (with-current-buffer base-buffer (buffer-string)))
        (setq buffer-file-coding-system (with-current-buffer base-buffer
                                          buffer-file-coding-system)))
      (with-temp-file modified-file
        (insert (with-current-buffer modified-buffer (buffer-string)))
        (setq buffer-file-coding-system (with-current-buffer modified-buffer
                                          buffer-file-coding-system)))
      (dvc-switch-to-buffer buffer)
      (let ((inhibit-read-only t)
            (slash (unless (file-name-absolute-p file) "/")))
        (erase-buffer)
        (call-process dvc-diff-executable nil buffer nil
                      "-u"
                      ;; FIXME: If the file has been renamed between
                      ;; BASE and MODIFIED, the file names as
                      ;; displayed here may be incorrect.  The
                      ;; protocol needs to be extended to allow the
                      ;; backend to supply the correct file names.
                      (concat "-La" slash file)
                      (concat "-Lb" slash file)
                      base-file modified-file))
      (delete-file base-file)
      (delete-file modified-file)
      (message "")
      (toggle-read-only 1)
      (diff-mode))))

(defun dvc-ediff-startup-hook ()
  "Passed as a startup hook for ediff.

Programs ediff to return to the current window configuration after
quitting."
  ;; ediff-after-quit-hook-internal is local to an ediff session.
  (add-hook 'ediff-after-quit-hook-internal
            (dvc-capturing-lambda ()
               (set-window-configuration
                (capture dvc-window-config)))
            nil 'local))

(defvar dvc-window-config nil
  "Used for inter-function communication.")

(defun dvc-ediff-buffers (bufferA bufferB)
  "Wrapper around `ediff-buffers'.

Calls `ediff-buffers' on BUFFERA and BUFFERB."
  (let ((dvc-window-config (current-window-configuration)))
    (ediff-buffers bufferA bufferB
                   '(dvc-ediff-startup-hook) 'dvc-ediff)))

(provide 'dvc-diff)
;;; dvc-diff.el ends here
