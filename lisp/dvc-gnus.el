;;; dvc-gnus.el --- dvc integration to gnus

;; Copyright (C) 2003-2006 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer <stefan@xsteve.at>

;; DVC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; DVC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

(require 'tla-core)

;; gnus is optional. Load it at compile-time to avoid warnings.
(eval-when-compile
  (condition-case nil
      (progn
        (require 'gnus)
        (require 'gnus-art)
        (require 'gnus-sum))
    (error nil)))

(defvar gnus-summary-dvc-submap nil
  "DVC Key mapping added to gnus summary.")

(defun dvc-gnus-initialize-keymap ()
  "Initialize the keymap for DVC in `gnus-summary-mode-map'.

Prefix key is 'K t'."
  (unless gnus-summary-dvc-submap
    (require 'gnus)
    (require 'gnus-sum)
    (require 'gnus-art)
    (setq gnus-summary-dvc-submap (make-sparse-keymap))
    (define-key gnus-summary-mode-map [?K ?t] gnus-summary-dvc-submap)))

;;;###autoload
(defun dvc-insinuate-gnus ()
  "Insinuate Gnus for each registered DVC back-end.

Runs (<backend>-insinuate-gnus) for each registered back-end having
this function.

Additionally the following key binding is defined for the gnus summary mode map:
K t l `dvc-gnus-article-extract-log-message'
K t a `dvc-gnus-article-apply-patch'"
  (interactive)
  (dvc-gnus-initialize-keymap)
  (define-key gnus-summary-dvc-submap [?a] 'dvc-gnus-article-apply-patch)
  (define-key gnus-summary-dvc-submap [?l] 'dvc-gnus-article-extract-log-message)
  (mapcar (lambda (x)
            (let ((fn (dvc-function x "insinuate-gnus" t)))
              (when (fboundp fn)
                (dvc-trace "Insinuating Gnus for %S" x)
                (funcall fn))))
          dvc-registered-backends))

(defun dvc-gnus-article-extract-log-message ()
  "Parse the mail and extract the log information.
Save it to `dvc-memorized-log-header', `dvc-memorized-patch-sender',
`dvc-memorized-log-message' and `dvc-memorized-version'."
  (interactive)
  (gnus-summary-select-article-buffer)
  (save-excursion
    (goto-char (point-min))
    (let* ((start-pos (or (search-forward "[PATCH] " nil t) (search-forward "Subject: ")))
           (end-pos (line-end-position))
           (log-header (buffer-substring-no-properties start-pos end-pos)))
      (setq dvc-memorized-log-header log-header))
    (goto-char (point-min))
    (let* ((start-pos (search-forward "From: " nil t))
           (end-pos (line-end-position))
           (sender (when start-pos (buffer-substring-no-properties start-pos end-pos))))
      (setq dvc-memorized-patch-sender (and start-pos sender)))
    (goto-char (point-min))
    (let* ((start-pos (search-forward "[VERSION] " nil t))
           (end-pos (line-end-position))
           (version (when start-pos (buffer-substring-no-properties start-pos end-pos))))
      (setq dvc-memorized-version (and start-pos version)))
    (goto-char (point-min))
    (when (and (search-forward "<<LOG-START>>" nil t)
               (search-forward "<<LOG-END>>" nil t))
      (goto-char (point-min))
      (let* ((start-pos (+ (search-forward "<<LOG-START>>") 1))
             (end-pos (- (progn (search-forward "<LOG-END>>") (line-beginning-position)) 1))
             (log-message (buffer-substring-no-properties start-pos end-pos)))
        (setq dvc-memorized-log-message log-message)
        (message "Extracted the patch log message from '%s'" dvc-memorized-log-header))))
  (gnus-article-show-summary))

(defun dvc-gnus-article-apply-patch (n)
  "Apply MIME part N, as patchset.
When called with no prefix arg, set N := 2.
First is checked, if it is a tla changeset created with DVC.
If that is the case, `tla-gnus-apply-patch' is called.
Otherwise `dvc-gnus-apply-patch' is called."
  (interactive "p")
  (unless current-prefix-arg
    (setq n 2))
  (let ((patch-type))
    (save-window-excursion
      (gnus-summary-select-article-buffer)
      (goto-char (point-min))
      (if (re-search-forward (concat "\\[VERSION\\] " (tla-make-name-regexp 4 t t)) nil t)
          (setq patch-type 'tla)
        (setq patch-type 'dvc)))
    (cond ((eq patch-type 'tla)
           (save-window-excursion
             (tla-gnus-article-apply-patch n)))
          (t
           (gnus-article-part-wrapper n 'dvc-gnus-apply-patch)))))

(defvar dvc-apply-patch-mapping nil)
;;e.g.: (add-to-list 'dvc-apply-patch-mapping '("psvn" "~/work/myprg/psvn"))

(defun dvc-gnus-suggest-apply-patch-directory ()
  "Use `dvc-apply-patch-mapping' to suggest a directory where
the patch sould be applied."
  (gnus-summary-select-article-buffer)
  (let ((patch-directory "~/")
        (m dvc-apply-patch-mapping))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "text/x-patch; " nil t)
        (while m
          (if (looking-at (caar m))
              (progn
                (setq patch-directory (cadar m))
                (setq m nil))
            (setq m (cdr m))))))
    (gnus-article-show-summary)
    (expand-file-name patch-directory)))

(defun dvc-gnus-apply-patch (handle)
  "Apply the patch corresponding to HANDLE."
  (dvc-buffer-push-previous-window-config)
  (dvc-gnus-article-extract-log-message)
  (let ((dvc-patch-name (concat (dvc-make-temp-name "dvc-patch") ".diff"))
        (patch-buff))
    (mm-save-part-to-file handle dvc-patch-name)
    (find-file dvc-patch-name)
    (setq patch-buff (current-buffer))
    (flet ((ediff-get-default-file-name () (dvc-gnus-suggest-apply-patch-directory)))
      (ediff-patch-file 2 patch-buff))))

(provide 'dvc-gnus)
;; arch-tag: 6afaa64c-9e9f-4600-beb6-1276365400d6
;;; dvc-gnus.el ends here
