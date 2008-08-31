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

(eval-when-compile
  ;; these have macros we use
  (require 'cl)
  (require 'dvc-utils)
  (require 'xmtn-basic-io)
  (require 'xmtn-automate))

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

(defmacro xmtn-basic-io-check-line (expected-symbol body)
  "Read next basic-io line at point. Error if it is `empty' or
`eof', or if its symbol is not EXPECTED-SYMBOL (a string).
Otherwise execute BODY with `value' bound to list containing
parsed rest of line. List is of form ((category value) ...)."
  (declare (indent 1) (debug (sexp body)))
  `(let ((line (xmtn-basic-io--next-parsed-line)))
     (if (or (member line '(empty eof))
             (not (string= (car line) ,expected-symbol)))
         (error "expecting \"%s\", found %s" ,expected-symbol line)
       (let ((value (cdr line)))
         ,body))))

(defun xmtn-basic-io-check-empty ()
  "Read next basic-io line at point. Error if it is not `empty' or `eof'."
  (let ((line (xmtn-basic-io--next-parsed-line)))
    (if (not (member line '(empty eof)))
        (error "expecting an empty line, found %s" line))))

(defmacro xmtn-basic-io-parse-line (body)
  "Read next basic-io line at point. Error if it is `empty' or
`eof'. Otherwise execute BODY with `symbol' bound to key (a
string), `value' bound to list containing parsed rest of line.
List is of form ((category value) ...)."
  (declare (indent 1) (debug (sexp body)))
  `(let ((line (xmtn-basic-io--next-parsed-line)))
     (if (member line '(empty eof))
         (error "expecting a line, found %s" line)
       (let ((symbol (car line))
             (value (cdr line)))
         ,body))))

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
           (setf (xmtn-conflicts-content-resolution conflict) (list 'resolved_user value)))
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
  (xmtn-basic-io-write-sym "node_type" "file")
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
  "Ignore BEGIN, END. Write EWOC-BUFFER ewoc as basic-io to current buffer."
  (xmtn-conflicts-write-header ewoc-buffer)
  ;; ewoc-map sets current-buffer to ewoc-buffer, so we need a
  ;; reference to the output-buffer.
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
  (let ((control-buffer ediff-control-buffer))
    (pop-to-buffer xmtn-conflicts-current-conflict-buffer)
    (setq xmtn-conflicts-current-conflict-buffer nil)
    (let ((conflict        (nth 0 xmtn-conflicts-ediff-quit-info))
          (buffer-ancestor (nth 1 xmtn-conflicts-ediff-quit-info))
          (buffer-left     (nth 2 xmtn-conflicts-ediff-quit-info))
          (buffer-right    (nth 3 xmtn-conflicts-ediff-quit-info))
          (result-file     (nth 4 xmtn-conflicts-ediff-quit-info))
          (current (ewoc-locate xmtn-conflicts-ewoc)))
      (setf (xmtn-conflicts-content-resolution conflict) (list 'resolved_user result-file))
      (kill-buffer buffer-ancestor)
      (kill-buffer buffer-left)
      (kill-buffer buffer-right)
      ;; ediff manages the result buffer nicely
      (ewoc-invalidate xmtn-conflicts-ewoc current)
      (set-buffer control-buffer))))

(defun xmtn-conflicts-resolve-content (conflict)
  "Resolve a content conflict, via ediff."
  ;; Get the ancestor, left, right into buffers with nice names.
  ;; Store the result in the current workspace, so a later 'merge
  ;; --resolve-conflicts-file' can find it. Current workspace is right revision.
  (let ((dvc-temp-current-active-dvc 'xmtn)
        (ancestor-revision-id (list 'xmtn (list 'revision xmtn-conflicts-ancestor-revision)))
        (left-revision-id (list 'xmtn (list 'revision xmtn-conflicts-left-revision)))
        (right-revision-id (list 'xmtn (list 'revision xmtn-conflicts-right-revision))))
    (let ((buffer-ancestor
           (dvc-revision-get-buffer (xmtn-conflicts-content-ancestor_name conflict) ancestor-revision-id))
          (buffer-left
           (dvc-revision-get-buffer (xmtn-conflicts-content-left_name conflict) left-revision-id))
          (buffer-right
           (dvc-revision-get-buffer (xmtn-conflicts-content-right_name conflict) right-revision-id))
          (result-file
           (concat default-directory (xmtn-conflicts-content-right_name conflict)))
          )

      (xmtn--insert-file-contents default-directory (xmtn-conflicts-content-ancestor_file_id conflict) buffer-ancestor)
      (xmtn--insert-file-contents default-directory (xmtn-conflicts-content-left_file_id conflict) buffer-left)
      (xmtn--insert-file-contents default-directory (xmtn-conflicts-content-right_file_id conflict) buffer-right)

      (add-hook 'ediff-quit-merge-hook 'xmtn-conflicts-resolve-conflict-post-ediff)
      ;; ediff leaves the merge buffer active;
      ;; xmtn-conflicts-resolve-conflict-post-ediff needs to find the
      ;; conflict buffer.
      (if xmtn-conflicts-current-conflict-buffer
          (error "another conflict resolution is already in progress."))
      (setq xmtn-conflicts-current-conflict-buffer (current-buffer))
      (setq xmtn-conflicts-ediff-quit-info
            (list conflict buffer-ancestor buffer-left buffer-right result-file))
      (ediff-merge-buffers-with-ancestor buffer-left buffer-right buffer-ancestor nil nil result-file)

      )))

(defun xmtn-conflicts-resolve-conflict ()
  "Resolve conflict at point, if not already resolved."
  (interactive)
  (let ((current (ewoc-locate xmtn-conflicts-ewoc)))
    (if (xmtn-conflicts-resolvedp current)
        (error "already resolved"))
    (let ((conflict (ewoc-data current)))
      (etypecase conflict
        (xmtn-conflicts-content
         (xmtn-conflicts-resolve-content conflict))
        ))))

(defvar xmtn-conflicts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?n]                  'xmtn-conflicts-next)
    (define-key map [?N]                  'xmtn-conflicts-next-unresolved)
    (define-key map [?p]                  'xmtn-conflicts-prev)
    (define-key map [?P]                  'xmtn-conflicts-prev-unresolved)
    (define-key map [?R]                  'xmtn-conflicts-resolve-conflict)
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

(defun xmtn-conflicts (left workspace)
  "List conflicts between WORKSPACE and LEFT revisions, allow specifying resolutions."
  (interactive "Mleft revision: ")
  (let ((default-directory
          (dvc-read-project-tree-maybe "Review conflicts for (workspace directory): "
                                       (when workspace (expand-file-name workspace)))))
    (xmtn--check-cached-command-version)
    (dvc-run-dvc-async
     'xmtn
     (list "automate" "show_conflicts" left (xmtn--get-base-revision-hash-id default-directory))
     :finished (dvc-capturing-lambda (output error status arguments)
                 (let ((conflict-file (concat default-directory "_MTN/conflicts")))
                   (with-current-buffer output (write-file conflict-file))
                   (xmtn-conflicts-review conflict-file)))

     :error (lambda (output error status arguments)
              (pop-to-buffer error))
     )))

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

(provide 'xmtn-conflicts)

;; end of file
