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
  (require 'cl)
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
    (xmtn-basic-io-check-line "node_type" nil)
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
  ;; matches format-alist requirements
  (set-syntax-table xmtn-basic-io--*syntax-table*)
  (goto-char begin)
  (xmtn-conflicts-parse-header)
  (xmtn-conflicts-parse-conflicts (1- end)); off-by-one somewhere.
  (let ((inhibit-read-only t)) (delete-region begin (1- end)))
  (set-buffer-modified-p nil)
  (point-max))

(defun xmtn-conflicts-after-insert-file (chars-inserted)
  ;; matches after-insert-file-functions requirements
  (xmtn-conflicts-read (point-min) (point-max))
  (point-max))

(defun xmtn-conflicts-write-header (ewoc-buffer)
  "Write EWOC-BUFFER header info in basic-io format to current buffer."
  (xmtn-basic-io-write-id "left" (with-current-buffer ewoc-buffer xmtn-conflicts-left-revision))
  (xmtn-basic-io-write-id "right" (with-current-buffer ewoc-buffer xmtn-conflicts-right-revision))
  (xmtn-basic-io-write-id "ancestor" (with-current-buffer ewoc-buffer xmtn-conflicts-ancestor-revision))
  (insert ?\n))

(defun xmtn-conflicts-write-content (conflict)
  "Write CONFLICT (a content conflict) in basic-io format to current buffer."
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
         (xmtn-basic-io-write-str "resolved_user" (cdr (xmtn-conflicts-content-resolution conflict))))
        ))
  (insert ?\n))

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

(defvar xmtn-conflicts-mode-map
  (let ((map (make-sparse-keymap)))
    ;(define-key map dvc-keyvec-add                  'dvc-status-add-files)
    map)
  "Keymap used in `xmtn-conflict-mode'.")

(easy-menu-define xmtn-conflicts-mode-menu xmtn-conflicts-mode-map
  "`xmtn-conflicts' menu"
  `("Mtn-conflicts"
    ;["Refresh Buffer"              dvc-generic-refresh               t]
    ))

(define-derived-mode xmtn-conflicts-mode fundamental-mode "xmtn-conflicts"
  "Major mode to specify conflict resolutions."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq buffer-read-only nil)
  (setq xmtn-conflicts-ewoc (ewoc-create 'xmtn-conflicts-printer))
  (use-local-map xmtn-conflicts-mode-map)
  (easy-menu-add xmtn-conflicts-mode-menu)
  (set (make-local-variable 'dvc-buffer-refresh-function) nil)
  (add-to-list 'buffer-file-format 'xmtn-conflicts-format)

  ;; Arrange for `revert-buffer' to do the right thing
  (set (make-variable-buffer-local 'after-insert-file-functions) '(xmtn-conflicts-after-insert-file))

  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(add-to-list 'uniquify-list-buffers-directory-modes 'xmtn-conflicts-mode)

(defun xmtn-conflicts (left right conflict-file)
  "List conflicts between LEFT and RIGHT revisions, allow specifying resolutions."
  (interactive "Mleft revision: \nMright revision: \nfsave as conflict file: ")
  (xmtn--check-cached-command-version)
  (dvc-run-dvc-async
   'xmtn
   (list "automate" "show_conflicts" left right)
   :finished (dvc-capturing-lambda (output error status arguments)
               (with-current-buffer output (write-file (capture conflict-file)))
               (xmtn-conflicts-review (capture conflict-file)))

   :error (lambda (output error status arguments)
                (pop-to-buffer error))
       ))))

(defun xmtn-conflicts-review (file)
  "Review conflicts file FILE."
  (interactive "fconflicts file: ")
  (let ((conflicts-buffer (dvc-get-buffer-create 'xmtn 'conflicts default-directory)))
    (dvc-kill-process-maybe conflicts-buffer)
    (pop-to-buffer conflicts-buffer)
    (with-current-buffer conflicts-buffer
      (let ((inhibit-read-only t)) (insert-file-contents file t))

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

      (set-buffer-modified-p nil))))

;; end of file
