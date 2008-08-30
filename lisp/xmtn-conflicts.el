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
  (xmtn-basic-io-check-line "left" (setq xmtn-conflicts-left-revision value))
  (xmtn-basic-io-check-line "right" (setq xmtn-conflicts-right-revision value))
  (xmtn-basic-io-check-line "ancestor" (setq xmtn-conflicts-ancestor-revision value))
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
      ((empty eof)
       (xmtn-basic-io--next-parsed-line))
      (t
       (xmtn-basic-io-parse-line
        (case symbol
          ((string= "resolved_internal")
           (setf (xmtn-conflicts-content-resolution conflict) (list 'resolved_internal)))
          ((string= "resolved_user")
           (setf (xmtn-conflicts-content-resolution conflict) (list 'resolved_user value)))
          (t
           (error "expecting \"resolved_internal\" or \"resolved_user\", found %s" line))))))

    (ewoc-enter-last xmtn-conflicts-ewoc conflict)))

(defun xmtn-conflicts-parse-conflicts ()
  "Parse conflict stanzas at point, fill in ewoc."
  ;; first line in stanza indicates type of conflict; dispatch on that
  (while (not (member (xmtn-basic-io--peek) '(empty eof)))
    (xmtn-basic-io-check-line
     "conflict"
     (if (and (eq 1 (length value))
              (eq 'symbol (caar value))
              (string= "content" (cadar value)))
        (xmtn-conflicts-parse-content-conflict)
       (error "expecting \"content\" found %s" value)))))

(defun xmtn-conflicts-read (begin end)
  "Parse region BEGIN END in current buffer as basic-io, fill in ewoc, erase BEGIN END."
  (set-syntax-table xmtn-basic-io--*syntax-table*)
  (goto-char begin)
  (xmtn-conflicts-parse-header)
  (xmtn-conflicts-parse-conflicts)
  (delete-region begin end)
  begin)

(defun xmtn-conflicts-save (begin end ewoc-buffer)
  "Ignore BEGIN, END. Write EWOC-BUFFER ewoc as basic-io to current buffer."
  nil)

(add-to-list 'format-alist
             '(xmtn-conflicts-format
               "Save conflicts in basic-io format."
               nil
               xmtn-conflicts-read
               xmtn-conflicts-save
               t
               nil
               nil))

(defun xmtn-conflicts-header ()
  "Return string for ewoc header."
  (concat
   "Conflicts between\n"
   (format "  left : %s\n" xmtn-conflicts-left-revision-spec)
   (format "  right: %s\n" xmtn-conflicts-right-revision-spec)))

(defun xmtn-conflicts-done (output-buffer)
  "Parse OUTPUT-BUFFER into current buffer ewoc."
  (let ((start (point)))
    (let ((inhibit-read-only t)) (insert-buffer output-buffer))
    (xmtn-conflicts-read start (point)))
  (let ((header (xmtn-conflicts-header))
        (footer ""))
    (ewoc-set-hf xmtn-conflicts-ewoc header footer))
  )

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
  (erase-buffer)
  (setq xmtn-conflicts-ewoc (ewoc-create 'xmtn-conflicts-printer))
  (use-local-map xmtn-conflicts-mode-map)
  (easy-menu-add xmtn-conflicts-mode-menu)
  (set (make-local-variable 'dvc-buffer-refresh-function) nil)
  (add-to-list 'buffer-file-format 'xmtn-conflicts-format)
  (setq buffer-file-name (concat default-directory "_MTN/conflicts"))
  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(add-to-list 'uniquify-list-buffers-directory-modes 'xmtn-conflicts-mode)

(defun xmtn-conflicts (left right)
  "List conflicts between LEFT and RIGHT revisions, allow specifying resolutions."
  (interactive "Mleft revision: \nMright revision: ")
  (let ((conflicts-buffer (dvc-get-buffer-create 'xmtn 'conflicts default-directory)))
    (dvc-kill-process-maybe conflicts-buffer)
    (dvc-switch-to-buffer-maybe conflicts-buffer)
    (with-current-buffer conflicts-buffer
      (xmtn-conflicts-mode)
      (setq xmtn-conflicts-left-revision-spec left)
      (setq xmtn-conflicts-right-revision-spec right)
      (xmtn--check-cached-command-version)
      (dvc-run-dvc-async
       'xmtn
       (list "automate" "show_conflicts" left right)
       :finished (lambda (output error status arguments)
                   (xmtn-conflicts-done output))

       :error (lambda (output error status arguments)
                (let ((header (xmtn-conflicts-header))
                      (footer (concat "error: " (with-current-buffer error (buffer-string)))))
                  (ewoc-set-hf xmtn-conflicts-ewoc header footer)))
       ))))

;; end of file
