;;; xmtn-conflicts.el --- conflict resolution for DVC backend for monotone

;; Copyright (C) 2008 Stephen Leake

;; Author: Stephen Leake
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
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

(eval-and-compile
  ;; these have macros we use
  (require 'cl)
  (require 'dvc-utils)
  (require 'xmtn-automate)
  (require 'xmtn-basic-io)
  (require 'xmtn-dvc)
  (require 'xmtn-run))

(defvar xmtn-conflicts-right-revision-spec ""
  "Buffer-local variable holding user spec of left revision.")
(make-variable-buffer-local 'xmtn-conflicts-right-revision-spec)

(defvar xmtn-conflicts-left-revision-spec ""
  "Buffer-local variable holding user spec of right revision.")
(make-variable-buffer-local 'xmtn-conflicts-left-revision-spec)

(defvar xmtn-conflicts-left-revision ""
  "Buffer-local variable holding left revision id.")
(make-variable-buffer-local 'xmtn-conflicts-left-revision-spec)

(defvar xmtn-conflicts-right-revision ""
  "Buffer-local variable holding right revision id.")
(make-variable-buffer-local 'xmtn-conflicts-right-revision-spec)

(defvar xmtn-conflicts-ancestor-revision ""
  "Buffer-local variable holding ancestor revision id.")
(make-variable-buffer-local 'xmtn-conflicts-ancestor-revision-spec)

(defvar xmtn-conflicts-output-buffer nil
  "Buffer to write basic-io to, when saving a conflicts buffer.")
(make-variable-buffer-local 'xmtn-conflicts-output-buffer)

(defvar xmtn-conflicts-current-conflict-buffer nil
  "Global variable for use in ediff quit hook.")
;; xmtn-conflicts-current-conflict-buffer cannot be buffer local,
;; because ediff leaves the merge buffer active.

(defvar xmtn-conflicts-ediff-quit-info nil
  "Stuff used by ediff quit hook.")
(make-variable-buffer-local 'xmtn-conflicts-ediff-quit-info)

(defstruct (xmtn-conflicts-root
            (:constructor nil)
            (:copier nil))
  ;; no slots; root of class for ewoc entries.
  )

(defstruct (xmtn-conflicts-content
            (:include xmtn-conflicts-root)
            (:copier nil))
  ancestor_name
  ancestor_file_id
  left_name
  left_file_id
  right_name
  right_file_id
  resolution)

(defun xmtn-conflicts-printer (conflict)
  "Print an ewoc element; CONFLICT must be of class xmtn-conflicts-root."
  (etypecase conflict
    (xmtn-conflicts-content
     (insert (dvc-face-add "content\n" 'dvc-keyword))
     (insert "ancestor:   ")
     (insert (xmtn-conflicts-content-ancestor_name conflict))
     (insert "\n")
     (insert "left:       ")
     (insert (xmtn-conflicts-content-left_name conflict))
     (insert "\n")
     (insert "right:      ")
     (insert (xmtn-conflicts-content-right_name conflict))
     (insert "\n")
     (insert "resolution: ")
     (insert (format "%s" (xmtn-conflicts-content-resolution conflict)))
     (insert "\n")
     )
    ))

(defvar xmtn-conflicts-ewoc nil
  "Buffer-local ewoc for displaying conflicts.
All xmtn-conflicts functions operate on this ewoc.
The elements must all be of class xmtn-conflicts.")
(make-variable-buffer-local 'xmtn-conflicts-ewoc)

(defun xmtn-conflicts-parse-header ()
  "Fill `xmtn-conflicts-left-revision',
`xmtn-conflicts-right-revision' and
`xmtn-conflicts-ancestor-revision' with data from conflict
header."
  ;;     left [9a019f3a364416050a8ff5c05f1e44d67a79e393]
  ;;    right [426509b2ae07b0da1472ecfd8ecc25f261fd1a88]
  ;; ancestor [dc4518d417c47985eb2cfdc2d36c7bd4c450d626]
  (xmtn-basic-io-check-line "left" (setq xmtn-conflicts-left-revision (cadar value)))
  (xmtn-basic-io-check-line "right" (setq xmtn-conflicts-right-revision (cadar value)))
  (xmtn-basic-io-check-line "ancestor" (setq xmtn-conflicts-ancestor-revision (cadar value)))
  (xmtn-basic-io-check-empty))

(defun xmtn-conflicts-parse-content-conflict ()
  "Fill an ewoc entry with data from content conflict stanza."
  ;;         conflict content
  ;;        node_type "file"
  ;;    ancestor_name "1553/gds-hardware-bus_1553-iru_honeywell-user_guide-symbols.tex"
  ;; ancestor_file_id [d1eee768379694a59b2b015dd59a61cf67505182]
  ;;        left_name "1553/gds-hardware-bus_1553-iru_honeywell-user_guide-symbols.tex"
  ;;     left_file_id [cb3fa7b591baf703d41dc2aaa220c9e3b456c4b3]
  ;;       right_name "1553/gds-hardware-bus_1553-iru_honeywell-user_guide-symbols.tex"
  ;;    right_file_id [d1eee768379694a59b2b015dd59a61cf67505182]
  ;;
  ;; optional resolution: {resolved_internal | resolved_user}
  (let ((conflict (make-xmtn-conflicts-content)))
    (xmtn-basic-io-check-line "node_type"
      (if (not (string= "file" (cadar value))) (error "expecting \"file\" found %s" (cadar value))))
    (xmtn-basic-io-check-line "ancestor_name" (setf (xmtn-conflicts-content-ancestor_name conflict) (cadar value)))
    (xmtn-basic-io-check-line "ancestor_file_id" (setf (xmtn-conflicts-content-ancestor_file_id conflict) (cadar value)))
    (xmtn-basic-io-check-line "left_name" (setf (xmtn-conflicts-content-left_name conflict) (cadar value)))
    (xmtn-basic-io-check-line "left_file_id" (setf (xmtn-conflicts-content-left_file_id conflict) (cadar value)))
    (xmtn-basic-io-check-line "right_name" (setf (xmtn-conflicts-content-right_name conflict) (cadar value)))
    (xmtn-basic-io-check-line "right_file_id" (setf (xmtn-conflicts-content-right_file_id conflict) (cadar value)))

    ;; look for a resolution
    (case (xmtn-basic-io--peek)
      ((empty eof) nil)
      (t
       (xmtn-basic-io-parse-line
        (cond
          ((string= "resolved_internal" symbol)
           (setf (xmtn-conflicts-content-resolution conflict) (list 'resolved_internal)))
          ((string= "resolved_user" symbol)
           (setf (xmtn-conflicts-content-resolution conflict) (list 'resolved_user (cadar value))))
          (t
           (error "expecting \"resolved_internal\" or \"resolved_user\", found %s" symbol))))))

    (xmtn-basic-io-check-empty)

    (ewoc-enter-last xmtn-conflicts-ewoc conflict)))

(defun xmtn-conflicts-parse-conflicts (end)
  "Parse conflict stanzas from point thru END, fill in ewoc."
  ;; first line in stanza indicates type of conflict; dispatch on that
  ;; ewoc-enter-last puts text in the buffer, after `end', preserving point.
  ;; xmtn-basic-io parsing moves point.
  (while (< (point) end)
    (xmtn-basic-io-check-line
     "conflict"
     (if (and (eq 1 (length value))
              (eq 'symbol (caar value))
              (string= "content" (cadar value)))
        (xmtn-conflicts-parse-content-conflict)
       (error "expecting \"content\" found %s" value)))))

(defun xmtn-conflicts-read (begin end)
  "Parse region BEGIN END in current buffer as basic-io, fill in ewoc, erase BEGIN END."
  ;; Matches format-alist requirements. We are not currently using
  ;; this in format-alist, but we might someday, and we need these
  ;; params anyway.
  (set-syntax-table xmtn-basic-io--*syntax-table*)
  (goto-char begin)
  (xmtn-conflicts-parse-header)
  (xmtn-conflicts-parse-conflicts (1- end)); off-by-one somewhere.
  (let ((inhibit-read-only t)) (delete-region begin (1- end)))
  (set-buffer-modified-p nil)
  (point-max))

(defun xmtn-conflicts-after-insert-file (chars-inserted)
  ;; matches after-insert-file-functions requirements

  ;; `xmtn-conflicts-read' creates ewoc entries, which are
  ;; inserted into the buffer. Since it is parsing the same
  ;; buffer, we need them to be inserted _after_ the text that is
  ;; being parsed. `xmtn-conflicts-mode' creates the ewoc at
  ;; point, and inserts empty header and footer lines.
  (goto-char (point-max))
  (let ((text-end (point)))
    (xmtn-conflicts-mode)

    ;; FIXME: save these in an associated file
    (setq xmtn-conflicts-left-revision-spec "")
    (setq xmtn-conflicts-right-revision-spec "")

    (xmtn-conflicts-read (point-min) text-end))

  (set-buffer-modified-p nil)
  (point-max))

(defun xmtn-conflicts-write-header (ewoc-buffer)
  "Write EWOC-BUFFER header info in basic-io format to current buffer."
  (xmtn-basic-io-write-id "left" (with-current-buffer ewoc-buffer xmtn-conflicts-left-revision))
  (xmtn-basic-io-write-id "right" (with-current-buffer ewoc-buffer xmtn-conflicts-right-revision))
  (xmtn-basic-io-write-id "ancestor" (with-current-buffer ewoc-buffer xmtn-conflicts-ancestor-revision)))

(defun xmtn-conflicts-write-content (conflict)
  "Write CONFLICT (a content conflict) in basic-io format to current buffer."
  (insert ?\n)
  (xmtn-basic-io-write-sym "conflict" "content")
  (xmtn-basic-io-write-str "node_type" "file")
  (xmtn-basic-io-write-str "ancestor_name" (xmtn-conflicts-content-ancestor_name conflict))
  (xmtn-basic-io-write-id "ancestor_file_id" (xmtn-conflicts-content-ancestor_file_id conflict))
  (xmtn-basic-io-write-str "left_name" (xmtn-conflicts-content-left_name conflict))
  (xmtn-basic-io-write-id "left_file_id" (xmtn-conflicts-content-left_file_id conflict))
  (xmtn-basic-io-write-str "right_name" (xmtn-conflicts-content-right_name conflict))
  (xmtn-basic-io-write-id "right_file_id" (xmtn-conflicts-content-right_file_id conflict))

  (if (xmtn-conflicts-content-resolution conflict)
      (ecase (car (xmtn-conflicts-content-resolution conflict))
        (resolved_internal
         (insert "resolved_internal \n"))

        (resolved_user
         (xmtn-basic-io-write-str "resolved_user" (cadr (xmtn-conflicts-content-resolution conflict))))
        )))

(defun xmtn-conflicts-write-conflicts (ewoc)
  "Write EWOC elements in basic-io format to xmtn-conflicts-output-buffer."
  (ewoc-map
   (lambda (conflict)
     (with-current-buffer xmtn-conflicts-output-buffer
       (etypecase conflict
         (xmtn-conflicts-content
          (xmtn-conflicts-write-content conflict)))))
   ewoc))

(defun xmtn-conflicts-save (begin end ewoc-buffer)
  "Replace region BEGIN END with EWOC-BUFFER ewoc in basic-io format."
  (delete-region begin end)
  (xmtn-conflicts-write-header ewoc-buffer)
  ;; ewoc-map sets current-buffer to ewoc-buffer, so we need a
  ;; reference to the current buffer.
  (let ((xmtn-conflicts-output-buffer (current-buffer))
        (ewoc (with-current-buffer ewoc-buffer xmtn-conflicts-ewoc)))
    (xmtn-conflicts-write-conflicts ewoc)))

;; Arrange for xmtn-conflicts-save to be called by save-buffer. We do
;; not automatically convert in insert-file-contents, because we don't
;; want to convert _all_ conflict files (consider the monotone test
;; suite!). Instead, we call xmtn-conflicts-read explicitly from
;; xmtn-conflicts-review, and set after-insert-file-functions to a
;; buffer-local value in xmtn-conflicts-mode.
(add-to-list 'format-alist
             '(xmtn-conflicts-format
               "Save conflicts in basic-io format."
               nil
               nil
               xmtn-conflicts-save
               t
               nil
               nil))

(defun xmtn-conflicts-header ()
  "Return string for ewoc header."
  (concat
   "Conflicts between\n"
   "  left : " (dvc-face-add xmtn-conflicts-left-revision-spec 'dvc-revision-name) "\n"
   "  right: " (dvc-face-add xmtn-conflicts-right-revision-spec 'dvc-revision-name) "\n"))

(dvc-make-ewoc-next xmtn-conflicts-next xmtn-conflicts-ewoc)
(dvc-make-ewoc-prev xmtn-conflicts-prev xmtn-conflicts-ewoc)

(defun xmtn-conflicts-resolvedp (elem)
  "Return non-nil if ELEM contains a conflict resolution."
  (let ((conflict (ewoc-data elem)))
    (etypecase conflict
      (xmtn-conflicts-content
       (xmtn-conflicts-content-resolution conflict))
      )))

(defun xmtn-conflicts-next-unresolved ()
  "Move to next unresolved element."
  (interactive)
  (xmtn-conflicts-next 'xmtn-conflicts-resolvedp))

(defun xmtn-conflicts-prev-unresolved ()
  "Move to prev unresolved element."
  (interactive)
  (xmtn-conflicts-prev 'xmtn-conflicts-resolvedp))

(defun xmtn-conflicts-resolve-conflict-post-ediff ()
  "Stuff to do when ediff quits."
  (remove-hook 'ediff-quit-merge-hook 'xmtn-conflicts-resolve-conflict-post-ediff)
  (ediff-dispose-of-variant-according-to-user ediff-buffer-A 'A nil nil)
  (ediff-dispose-of-variant-according-to-user ediff-buffer-B 'B nil nil)
  (ediff-dispose-of-variant-according-to-user ediff-ancestor-buffer 'Ancestor nil nil)
  ;; ediff-buffer-C is the result buffer; it's not saved yet. FIXME:
  ;; We'd like to have ediff not ask about killing it.
  ;; FIXME: restore window config
  (let ((control-buffer ediff-control-buffer))
    (pop-to-buffer xmtn-conflicts-current-conflict-buffer)
    (setq xmtn-conflicts-current-conflict-buffer nil)
    (let ((current     (nth 0 xmtn-conflicts-ediff-quit-info))
          (result-file (nth 1 xmtn-conflicts-ediff-quit-info)))
      (let ((conflict (ewoc-data current)))
        (setf (xmtn-conflicts-content-resolution conflict) (list 'resolved_user result-file)))
      (ewoc-invalidate xmtn-conflicts-ewoc current)
      (set-buffer control-buffer))))

(defun xmtn-conflicts-resolve-content-ediff (elem)
  "Resolve the content conflict in ewoc element ELEM, via ediff."
  (if xmtn-conflicts-current-conflict-buffer
      (error "another conflict resolution is already in progress."))

  (let ((conflict (ewoc-data current)))
    (let ((file-ancestor (concat "_MTN/ancestor/" (xmtn-conflicts-content-ancestor_name conflict)))
          (file-left (concat "_MTN/left/" (xmtn-conflicts-content-left_name conflict)))
          (file-right (concat "_MTN/right/" (xmtn-conflicts-content-right_name conflict)))
          (result-file (concat "_MTN/result/" (xmtn-conflicts-content-right_name conflict)))
          )

      ;; create the directories if necessary
      (mapc
       (lambda (file)
         (let ((dir (file-name-directory file)))
           (unless (file-exists-p dir)
             (make-directory dir t))))
       (list file-ancestor file-left file-right result-file))

      ;; Get the ancestor, left, right into files with nice names, so
      ;; uniquify gives the buffers nice names. Store the result in
      ;; _MTN/*, so a later 'merge --resolve-conflicts-file' can find
      ;; it.
      (xmtn--get-file-by-id default-directory (xmtn-conflicts-content-ancestor_file_id conflict) file-ancestor)
      (xmtn--get-file-by-id default-directory (xmtn-conflicts-content-left_file_id conflict) file-left)
      (xmtn--get-file-by-id default-directory (xmtn-conflicts-content-right_file_id conflict) file-right)

      (add-hook 'ediff-quit-merge-hook 'xmtn-conflicts-resolve-conflict-post-ediff)
      ;; ediff leaves the merge buffer active;
      ;; xmtn-conflicts-resolve-conflict-post-ediff needs to find the
      ;; conflict buffer.
      (setq xmtn-conflicts-current-conflict-buffer (current-buffer))
      (setq xmtn-conflicts-ediff-quit-info
            (list current result-file))
      (ediff-merge-files-with-ancestor file-left file-right file-ancestor nil result-file)
      )))

(defun xmtn-conflicts-resolve-content-file (elem)
  "Resolve the content conflict in ewoc element ELEM, by user specified file."
  (let ((conflict (ewoc-data elem))
        (result-file (read-file-name "resolution file: " "_MTN/result/" nil t (xmtn-conflicts-content-ancestor_name conflict))))
    (setf (xmtn-conflicts-content-resolution conflict) (list 'resolved_user result-file))
    (ewoc-invalidate xmtn-conflicts-ewoc elem)))

(defun xmtn-conflicts-resolve-conflict-ediff ()
  "Resolve conflict at point, via ediff."
  (interactive)
  (let ((current (ewoc-locate xmtn-conflicts-ewoc)))
    (if (and (xmtn-conflicts-resolvedp current)
             (not (y-or-n-p "Already resolved; review and override? ")))
        (error "already resolved"))
    (let ((conflict (ewoc-data current)))
      (etypecase conflict
        (xmtn-conflicts-content
         (xmtn-conflicts-resolve-content-ediff current))
        ))))

(defun xmtn-conflicts-resolve-conflict-file ()
  "Resolve conflict at point, by user specified file."
  (interactive)
  (let ((current (ewoc-locate xmtn-conflicts-ewoc)))
    (if (and (xmtn-conflicts-resolvedp current)
             (not (y-or-n-p "Already resolved; override? ")))
        (error "already resolved"))
    (let ((conflict (ewoc-data current)))
      (etypecase conflict
        (xmtn-conflicts-content
         (xmtn-conflicts-resolve-content-file current))
        ))))

(defvar xmtn-conflicts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?n]  'xmtn-conflicts-next)
    (define-key map [?N]  'xmtn-conflicts-next-unresolved)
    (define-key map [?p]  'xmtn-conflicts-prev)
    (define-key map [?P]  'xmtn-conflicts-prev-unresolved)
    (define-key map [?q]  'dvc-buffer-quit)
    (define-key map [?r]  'xmtn-conflicts-resolve-conflict-ediff)
    (define-key map [?R]  'xmtn-conflicts-resolve-conflict-file)
    (define-key map [?c]  'xmtn-conflicts-clean)
    map)
  "Keymap used in `xmtn-conflict-mode'.")

(easy-menu-define xmtn-conflicts-mode-menu xmtn-conflicts-mode-map
  "`xmtn-conflicts' menu"
  `("Mtn-conflicts"
    ["Resolve conflict"     xmtn-conflicts-resolve-conflict t]
    ))

(define-derived-mode xmtn-conflicts-mode fundamental-mode "xmtn-conflicts"
  "Major mode to specify conflict resolutions."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq buffer-read-only nil)
  (setq xmtn-conflicts-ewoc (ewoc-create 'xmtn-conflicts-printer))
  (use-local-map xmtn-conflicts-mode-map)
  (easy-menu-add xmtn-conflicts-mode-menu)
  (setq dvc-buffer-refresh-function nil)
  (add-to-list 'buffer-file-format 'xmtn-conflicts-format)

  ;; Arrange for `revert-buffer' to do the right thing
  (set (make-local-variable 'after-insert-file-functions) '(xmtn-conflicts-after-insert-file))

  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(add-to-list 'uniquify-list-buffers-directory-modes 'xmtn-conflicts-mode)

(defun xmtn-conflicts-1 (left right)
  "List conflicts between LEFT and RIGHT revisions (monotone revision specs).
Allow specifying resolutions.  LEFT and RIGHT default to current
merge heads if nil.  `default-directory must be a workspace."
    (xmtn--check-cached-command-version)
    (dvc-run-dvc-async
     'xmtn
     (list "automate" "show_conflicts" left right)
     :finished (dvc-capturing-lambda (output error status arguments)
                 (let ((conflict-file (concat default-directory "_MTN/conflicts")))
                   (with-current-buffer output (write-file conflict-file))
                   (xmtn-conflicts-review conflict-file)))

     :error (lambda (output error status arguments)
              (pop-to-buffer error))
     ))

(defun xmtn-check-workspace-for-propagate (work)
  "Check that workspace WORK is ready for propagate.
It must be merged, and should be at the head revision, and have no local changes.
Prompt if the last two conditions are not satisfied."
  (let* ((default-directory work)
         (heads (xmtn--heads default-directory nil))
         (base (xmtn--get-base-revision-hash-id-or-null default-directory)))

    (message "checking %s for multiple heads, base not head" work)

    (if (> 1 (length heads))
        (error "%s has multiple heads; can't propagate" work))

    (if (not (string= base (nth 0 heads)))
        (if (not (yes-or-no-p (format "%s base is not head; really propagate? " work)))
            (error "aborting due to not at head")))

    ;; check for local changes
    (message "checking %s for local changes" work)

    (dvc-run-dvc-sync
     'xmtn
     (list "status")
     :finished (lambda (output error status arguments)
                 ;; we don't get an error status for not up-to-date,
                 ;; so parse the output.
                 ;; FIXME: add option to automate inventory to just return status; can return on first change
                 ;; FIXME: 'patch' may be internationalized.
                 (set-buffer output)
                 (goto-char (point-min))
                 (if (search-forward "patch" (point-max) t)
                     (if (not (yes-or-no-p (format "%s has local changes; really show conflicts? " work)))
                         (error "aborting due to local changes"))))

     :error (lambda (output error status arguments)
              (pop-to-buffer error))))

  )

;;;###autoload
(defun xmtn-conflicts-propagate (left-work right-work)
  "List conflicts for a propagate from LEFT-WORK to RIGHT-WORK workspace base revisions.
Allow specifying resolutions.  LEFT-WORK and RIGHT-WORK are strings giving
workspace directories; prompted if nil. Review is done in RIGHT-WORK
workspace."
  (interactive "i\ni")
  (setq left-work (dvc-read-project-tree-maybe "Left (workspace directory): " left-work))
  (setq right-work (dvc-read-project-tree-maybe "Right (workspace directory): " right-work))

  (xmtn-check-workspace-for-propagate left-work)
  (xmtn-check-workspace-for-propagate right-work)

  (let ((default-directory right-work))
    (xmtn-conflicts-1 (xmtn--get-base-revision-hash-id left-work)
                      (xmtn--get-base-revision-hash-id right-work))))

;;;###autoload
(defun xmtn-conflicts-merge (left right)
  "List conflicts between LEFT and RIGHT revisions, allow specifying resolutions.
LEFT and RIGHT default to current merge heads if nil."
  (interactive "MLeft revision (monotone revision spec): \nMRight revision (monotone revision spec): ")
  (let ((default-directory
          (dvc-read-project-tree-maybe "Review conflicts in (workspace directory): ")))
    (xmtn-conflicts-1 left right)))

;;;###autoload
(defun xmtn-conflicts-review (&optional workspace)
  "Review conflicts for WORKSPACE (a directory; default prompt)."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "Review conflicts for (workspace directory): "
                                       (when workspace (expand-file-name workspace)))))
    (let ((conflicts-buffer (dvc-get-buffer-create 'xmtn 'conflicts default-directory)))
      (dvc-kill-process-maybe conflicts-buffer)
      (pop-to-buffer conflicts-buffer)
      ;; Arrange for `insert-file-conflicts' to finish the job
      (set (make-local-variable 'after-insert-file-functions) '(xmtn-conflicts-after-insert-file))
      (insert-file-contents "_MTN/conflicts" t))))

(defun xmtn-conflicts-clean (&optional workspace)
  "Remove conflicts resolution files from WORKSPACE (a directory; default prompt)."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "Remove conflicts resolutions for (workspace directory): "
                                       (when workspace (expand-file-name workspace)))))
    (if (file-exists-p "_MTN/conflicts")
        (delete-file "_MTN/conflicts"))

    (if (file-exists-p "_MTN/left")
        (delete-directory "_MTN/left"))

    (if (file-exists-p "_MTN/right")
        (delete-directory "_MTN/right"))

    (if (file-exists-p "_MTN/result")
        (delete-directory "_MTN/result"))
    ))

(provide 'xmtn-conflicts)

;; end of file
