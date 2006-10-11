;;; dvc-revlist.el --- Revision list in DVC

;; Copyright (C) 2005-2006 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>

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

;; Generic stuff to display revision lists.
;; Revision lists are the core of the "decentralized" aspect of DVC.

;;; Code:

(eval-when-compile (require 'cl))

(eval-and-compile
  (require 'dvc-lisp)
  (require 'dvc-utils)
  (require 'dvc-core)
  )

(require 'dvc-ui)


(defstruct (dvc-revlist-entry-patch)
  dvc ;; the back-end
  marked
  struct
  rev-id ;; DVC revision ID.
  merged-by
  log-buffer
  diff-buffer)

;; elem should be
;; ('separator "string" kind)
;; or
;; ('entry-patch struct) Where "struct" is a dvc-revlist-entry-patch
;; struct type.
;; ('entry-change "changes")
;; The second element tells if the element is marked or not.
(defun dvc-revlist-printer (elem)
  "Print an element ELEM of the revision list."
  (let ()
    (case (car elem)
      (entry-patch
       (funcall
        (dvc-function (dvc-revlist-entry-patch-dvc (nth 1 elem))
                      "revision-list-entry-patch-printer" t) (nth 1 elem)))
      (entry-change (insert  (cadr elem)))
      (message (insert (dvc-face-add (cadr elem)
                                      'dvc-messages)))
      (separator
       (case (car (cddr elem))
         (partner (insert "\n" (dvc-face-add (cadr elem)
                                              'dvc-separator)))
         (bookmark (insert "\n" (dvc-face-add
                                 (concat "*** "
                                         (cadr elem)
                                         " ***")
                                 'dvc-separator) "\n")))))))

(dvc-make-move-fn ewoc-next dvc-revision-next
                  dvc-revlist-cookie)

(dvc-make-move-fn ewoc-prev dvc-revision-prev
                  dvc-revlist-cookie)

(dvc-make-move-fn ewoc-next dvc-revision-next-unmerged
                  dvc-revlist-cookie t)

(dvc-make-move-fn ewoc-prev dvc-revision-prev-unmerged
                  dvc-revlist-cookie t)

(defun dvc-revlist-current-patch-struct ()
  "Get the dvc-revlist-entry-patch-struct at point."
  (dvc-revlist-entry-patch-struct
   (nth 1 (ewoc-data (ewoc-locate dvc-revlist-cookie)))))

(defun dvc-revision-mark-revision ()
  "Mark revision at point."
  (interactive)
  (let* ((pos (point))
         (current (ewoc-locate
                   dvc-revlist-cookie))
        (data (ewoc-data current)))
    (setf (dvc-revlist-entry-patch-marked (nth 1 data)) t)
    (ewoc-invalidate dvc-revlist-cookie current)
    (goto-char pos)
    (dvc-revision-next)))

(defun dvc-revision-marked-revisions ()
  "Return the revisions that are currently marked."
  (let ((acc '()))
    (ewoc-map (lambda (x) (when (and (eq (car x) 'entry-patch)
                                     (dvc-revlist-entry-patch-marked
                                      (cadr x)))
                            (push (dvc-revlist-entry-patch-struct
                                   (nth 1 x)) acc)))
              dvc-revlist-cookie)
    (nreverse acc)))

(defun dvc-revision-unmark-revision ()
  "Unmark the revision at point."
  (interactive)
  (let* ((pos (point))
         (current (ewoc-locate
                   dvc-revlist-cookie))
         (data (ewoc-data current)))
    (setf (dvc-revlist-entry-patch-marked (nth 1 data)) nil)
    (ewoc-invalidate dvc-revlist-cookie current)
    (goto-char pos)
    (dvc-revision-next)))

;; TODO bind this to something
(defun dvc-revision-unmark-all ()
  "Unmark all revisions."
  (interactive)
  (let ((pos (point)))
    (ewoc-map (lambda (x) (when (and (eq (car x) 'entry-patch)
                                     (nth 2 x))
                            (setcar (cddr x) nil)))
              dvc-revlist-cookie)
    (ewoc-refresh dvc-revlist-cookie)
    (goto-char pos)))


(defcustom dvc-revisions-shows-summary t
  "*Whether summary should be displayed for `dvc-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defcustom dvc-revisions-shows-creator t
  "*Whether creator should be displayed for `dvc-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defcustom dvc-revisions-shows-date t
  "*Whether date should be displayed for `dvc-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defun dvc-revision-refresh-maybe ()
  (let ((refresh-fn
         (dvc-function (dvc-current-active-dvc)
                       "revision-refresh-maybe" t)))
    (when (fboundp refresh-fn)
      (funcall refresh-fn))))

(defun dvc-revlist-toggle-date ()
  "Toggle display of the date in the revision list."
  (interactive)
  (setq dvc-revisions-shows-date (not dvc-revisions-shows-date))
  (dvc-revision-refresh-maybe)
  (ewoc-refresh dvc-revlist-cookie))

(defun dvc-revlist-toggle-summary ()
  "Toggle display of the summary information in the revision list."
  (interactive)
  (setq dvc-revisions-shows-summary (not dvc-revisions-shows-summary))
  (dvc-revision-refresh-maybe)
  (ewoc-refresh dvc-revlist-cookie))

(defun dvc-revlist-toggle-creator ()
  "Toggle display of the creator in the revision list."
  (interactive)
  (setq dvc-revisions-shows-creator (not dvc-revisions-shows-creator))
  (dvc-revision-refresh-maybe)
  (ewoc-refresh dvc-revlist-cookie))


(defvar dvc-get-revision-info-at-point-function nil
  "Variable should be local to each buffer.
Function used to get the revision info at point")

(defun dvc-get-info-at-point ()
  "Get the version information that point is on."
  (when (fboundp dvc-get-revision-info-at-point-function)
    (funcall dvc-get-revision-info-at-point-function)))

;; TODO: this is still tla-oriented.
(defun dvc-revlist-get-rev-at-point ()
  "Get archive/category--branch--version--revision--patch information.
Returns nil if not on a revision list, or not on a revision entry in a
revision list."
  (let ((elem (ewoc-data (ewoc-locate dvc-revlist-cookie))))
    (when (eq (car elem) 'entry-patch)
      (let* ((full (tla--revision-revision
                    (dvc-revlist-entry-patch-struct (nth 1 elem))))
             (buffer-revision (tla--name-construct full)))
        (list 'revision buffer-revision)))))

(autoload 'dvc-revlog-revision "dvc-revlog")

(defun dvc-revlist-show-item (&optional scroll-down)
  "Show a changeset for the current revision."
  (interactive)
  (let ((elem (ewoc-data (ewoc-locate
                          dvc-revlist-cookie)))
        (dvc-temp-current-active-dvc (dvc-current-active-dvc)))
    (case (car elem)
      (entry-patch
       ;; reuse existing buffer if possible
       (let ((buffer (dvc-revlist-entry-patch-log-buffer
                      (nth 1 elem)))
             (log-buf (current-buffer)))
         (if (and buffer (buffer-live-p buffer))
             (dvc-buffer-show-or-scroll buffer scroll-down)
           (setf (dvc-revlist-entry-patch-log-buffer
                  (nth 1 elem))
                 (dvc-revlog-revision
                  (dvc-revlist-entry-patch-rev-id (nth 1 elem)))))
         (pop-to-buffer log-buf)))
      ;; TODO: untested.
      (entry-change (let ((default-directory (car (cddr elem))))
                      (dvc-diff))))))

(defun dvc-revlist-show-item-scroll-down ()
  (interactive)
  (dvc-revlist-show-item t))

(dvc-make-bymouse-function dvc-revlist-show-item)

(defun dvc-revlist-diff (&optional scroll-down)
  "Show the diff for the current revision."
  (interactive)
  (let ((elem (ewoc-data (ewoc-locate dvc-revlist-cookie))))
    (unless (eq (car elem) 'entry-patch)
      (error "Cursor is not on a revision."))
    (let ((buffer (dvc-revlist-entry-patch-diff-buffer
                   (nth 1 elem)))
          (log-buf (current-buffer)))
      (if (and buffer (buffer-live-p buffer))
          (dvc-buffer-show-or-scroll buffer scroll-down)
        (setf (dvc-revlist-entry-patch-diff-buffer
               (nth 1 elem))
              (let* ((rev-id (dvc-revlist-entry-patch-rev-id (nth 1 elem)))
                     (rev-type (dvc-revision-get-type rev-id))
                     (rev-data (dvc-revision-get-data rev-id)))
                (unless (eq rev-type 'revision)
                  (error "Only 'revision type is supported here. Got %S" rev-type))
                (let* ((prev-rev-id `(,(car rev-id) (previous-revision
                                                     ,rev-id 1))))
                  (dvc-trace "prev-rev-id=%S" prev-rev-id)
                  (dvc-trace "rev-id=%S" rev-id)
                  (dvc-delta prev-rev-id rev-id)))))
      (pop-to-buffer log-buf))))

(defun dvc-revlist-diff-scroll-down ()
  (interactive)
  (dvc-revlist-diff t))

(defvar dvc-revlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?g] 'dvc-generic-refresh)
    (define-key map [down] 'dvc-revision-next)
    (define-key map [up] 'dvc-revision-prev)
    (define-key map [?n] 'dvc-revision-next)
    (define-key map [?p] 'dvc-revision-prev)
    (define-key map [?N] 'dvc-revision-next-unmerged)
    (define-key map [?P] 'dvc-revision-prev-unmerged)
    (define-key map [?M] 'dvc-send-commit-notification)
    (define-key map "\C-m" 'dvc-revlist-show-item)
    (define-key map [return] 'dvc-revlist-show-item)
    (define-key map [(meta return)] 'dvc-revlist-show-item-scroll-down)
    (define-key map [?=]              'dvc-revlist-diff)
    (define-key map [(meta ?=)]       'dvc-revlist-diff-scroll-down)
    (define-key map (dvc-prefix-toggle ?d) 'dvc-revlist-toggle-date)
    (define-key map (dvc-prefix-toggle ?c) 'dvc-revlist-toggle-creator)
    (define-key map (dvc-prefix-toggle ?s) 'dvc-revlist-toggle-summary)
    (define-key map dvc-keyvec-mark   'dvc-revision-mark-revision)
    (define-key map dvc-keyvec-unmark 'dvc-revision-unmark-revision)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)
    (define-key map dvc-keyvec-inventory 'dvc-pop-to-inventory)
    (define-key map [?h] 'dvc-buffer-pop-to-partner-buffer)
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)

    (define-key map dvc-keyvec-kill-ring nil)
    (define-key map (dvc-prefix-kill-ring ?l) 'dvc-revision-save-log-message-as-kill)
    map))

(define-derived-mode dvc-revlist-mode fundamental-mode
  "dvc-revlist"
  "Major mode to show revision list.

Commands are:
\\{dvc-revlist-mode-map}"
  (dvc-install-buffer-menu)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (set (make-local-variable 'dvc-revlist-cookie)
       (ewoc-create (dvc-ewoc-create-api-select
		     #'dvc-revlist-printer)))
  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'dvc-get-revision-info-at-point-function)
       'dvc-revlist-get-rev-at-point))

(defun dvc-build-revision-list (back-end type location arglist parser)
  "Runs the back-end BACK-END to build a revision list.

A buffer of type TYPE with location LOCATION is created or reused.

The back-end is launched with the arguments ARGLIST, and the
caller has to provide the function PARSER which will actually
build the revision list."
  (let ((buffer (dvc-get-buffer-create back-end type location)))
    (with-current-buffer buffer
      (let ((back-end dvc-buffer-current-active-dvc))
        (dvc-revlist-mode)
        (setq dvc-buffer-current-active-dvc back-end)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (dvc-run-dvc-async
     back-end arglist
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer output
         (funcall (capture parser) (capture buffer) (capture location))))
     ))
  )

(defun dvc-revision-log-message-at-point ()
  (dvc-apply "revision-st-message" (dvc-revlist-current-patch-struct)))

(defun dvc-revision-save-log-message-as-kill ()
  "Save the log message for the actual patch."
  (interactive)
  (kill-new (dvc-revision-log-message-at-point)))
  ;; TODO: (message "Copied log message for %s" (tla-changelog-revision-at-point)))

(provide 'dvc-revlist)
;;; dvc-revlist.el ends here
