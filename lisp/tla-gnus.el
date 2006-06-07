;;; tla-gnus.el --- dvc integration to gnus

;; Copyright (C) 2003-2006 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; Xtla is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Xtla is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

(require 'tla-core)
(require 'dvc-gnus)

;; gnus is optional. Load it at compile-time to avoid warnings.
(eval-when-compile
  (condition-case nil
      (progn
        (require 'gnus)
        (require 'gnus-art)
        (require 'gnus-sum))
    (error nil)))

;; Integration into gnus

(autoload 'tla-categories-string "tla")
(autoload 'tla-branches-string "tla")
(autoload 'tla-versions-string "tla")
(autoload 'tla-revisions-string "tla")
(autoload 'tla--button-revision-fn "tla")

(defun tla-gnus-setup-buttons ()
  "Make archive@host.com/something clickable in Gnus Article buffer."
  (interactive)
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 0 t t) 1 t
                 tla-categories-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 1 t t) 1 t
                 tla-branches-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 2 t t) 1 t
                 tla-versions-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 3 t t) 1 t
                 tla-revisions-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 4 t t) 1 t
                 tla--button-revision-fn 1)))

;;;###autoload
(defun tla-insinuate-gnus ()
  "Integrate Xtla into Gnus.
The following keybindings are installed for gnus-summary:
K t v `tla-gnus-article-view-patch'
K t a `tla-gnus-article-apply-patch'
K t l `tla-gnus-article-extract-log-message'

Additionally add the `tla-submit-patch-done' function to the
`message-sent-hook'.

The archives/categories/branches/version/revision names are buttonized
in the *Article* buffers."
  (interactive)
  (dvc-gnus-initialize-keymap)
  (define-key gnus-summary-dvc-submap [?v] 'tla-gnus-article-view-patch)
  (define-key gnus-summary-dvc-submap [?a] 'tla-gnus-article-apply-patch)
  (define-key gnus-summary-dvc-submap [?l] 'tla-gnus-article-extract-log-message)
  (add-hook 'message-sent-hook 'tla-submit-patch-done)
  (tla-gnus-setup-buttons))

(defun tla-gnus-article-view-patch ()
"View a patch via gnus.
The patch can be embedded or external."
  (interactive)
  (if (save-excursion
        (gnus-summary-select-article-buffer)
        (> (gnus-article-mime-total-parts) 1))
      (tla-gnus-article-view-attached-patch 2)
    (tla-gnus-article-view-external-patch)))

(defun tla-gnus-article-view-attached-patch (n)
  "View MIME part N, as tla patchset."
  (interactive "p")
  (gnus-article-part-wrapper n 'tla-gnus-view-patch))

(defun tla-gnus-article-view-external-patch ()
  "View an external patch that is referenced in this mail.

The mail must either contain a line starting with 'Committed ' and ending
with the fully qualified revision name.

The second supported format contains an extra line for Revision and Archive."
  (interactive)
  (let ((revision)
        (archive)
        (version)
        (window-conf (current-window-configuration)))
    (gnus-summary-select-article-buffer)
    (split-window-vertically)
    (goto-char (point-min))
    (cond ((re-search-forward (concat "Committed " (tla-make-name-regexp 4 nil t)) nil t)
           (setq version (buffer-substring-no-properties
                               (+ (match-beginning 0) 10) (- (match-end 0) 1))))
          (t
           (when (search-forward "Revision: " nil t)
             (setq revision (buffer-substring-no-properties (point) (line-end-position))))
           (when (search-forward "Archive: " nil t)
             (setq archive (buffer-substring-no-properties (point) (line-end-position))))
           (when (and archive revision)
             (setq version (concat archive "/" revision)))))
    (gnus-article-show-summary)
    (if version
        (progn
          (tla-get-changeset version t)
          (save-excursion
            (set-buffer (dvc-get-buffer tla-arch-branch 'changeset version))
            (dvc-buffer-push-previous-window-config window-conf)))
      (message "No external arch patch found in this article.")
      (set-window-configuration window-conf))))


(defun tla-gnus-view-patch (handle)
  "View a patch within gnus.  HANDLE should be the handle of the part."
  (let ((archive-name (dvc-make-temp-name "gnus-patch-tgz"))
        (window-conf (current-window-configuration)))
    (mm-save-part-to-file handle archive-name)
    (gnus-summary-select-article-buffer)
    (split-window-vertically)
    (tla-show-changeset-from-tgz archive-name)
    (dvc-buffer-push-previous-window-config window-conf)
    (delete-file archive-name)))

(defun tla-gnus-article-apply-patch (n)
  "Apply MIME part N, as tla patchset.
When called with no prefix arg, set N := 2."
  (interactive "p")
  (unless current-prefix-arg
    (setq n 2))
  (gnus-article-part-wrapper n 'tla-gnus-apply-patch))

(defun tla-gnus-apply-patch (handle)
  "Apply the patch corresponding to HANDLE."
  (dvc-buffer-push-previous-window-config)
  (tla-gnus-article-extract-log-message)
  (let ((archive-name (dvc-make-temp-name "gnus-patch-tgz"))
        (tree-dir (tla--name-match-from-list
                   (when tla-memorized-version
                     (tla--name-split tla-memorized-version))
                   tla-apply-patch-mapping))
        (tree))
    (mm-save-part-to-file handle archive-name)
    (gnus-summary-select-article-buffer)
    (split-window-vertically)
    (tla-show-changeset-from-tgz archive-name)
    (setq tree (dvc-read-directory-name "Apply to tree: "
                                         tree-dir tree-dir))
    (tla-apply-changeset-from-tgz archive-name tree nil)
    (delete-file archive-name)
    (when (eq major-mode 'tla-inventory-mode)
      (delete-other-windows))))

(defun tla-gnus-article-extract-log-message ()
  "Parse the mail and extract the log information.
Save it to `tla-memorized-log-header', `tla-memorized-patch-sender',
`tla-memorized-log-message' and `tla-memorized-version'."
  (interactive)
  (gnus-summary-select-article-buffer)
  (save-excursion
    (goto-char (point-min))
    (let* ((start-pos (or (search-forward "[PATCH] " nil t) (search-forward "Subject: ")))
           (end-pos (line-end-position))
           (log-header (buffer-substring-no-properties start-pos end-pos)))
      (setq tla-memorized-log-header log-header))
    (goto-char (point-min))
    (let* ((start-pos (search-forward "From: " nil t))
           (end-pos (line-end-position))
           (sender (when start-pos (buffer-substring-no-properties start-pos end-pos))))
      (setq tla-memorized-patch-sender (and start-pos sender)))
    (goto-char (point-min))
    (let* ((start-pos (search-forward "[VERSION] " nil t))
           (end-pos (line-end-position))
           (version (when start-pos (buffer-substring-no-properties start-pos end-pos))))
      (setq tla-memorized-version (and start-pos version)))
    (goto-char (point-min))
    (let* ((start-pos (+ (search-forward "<<LOG-START>>") 1))
           (end-pos (- (progn (search-forward "<LOG-END>>") (line-beginning-position)) 1))
           (log-message (buffer-substring-no-properties start-pos end-pos)))
      (setq tla-memorized-log-message log-message)
      (message "Extracted the tla log message from '%s'" tla-memorized-log-header)))
  (gnus-article-show-summary))

;; --------------------------------------------------------------------------------
;; xhg
;; --------------------------------------------------------------------------------
;; TODO : should move to xgh-gnus.el
(defvar xhg-apply-patch-mapping nil)
;;(add-to-list 'xhg-apply-patch-mapping '("my-wiki" "~/work/wiki/"))

(defvar xhg-gnus-import-patch-force nil)
(defun xhg-gnus-article-import-patch (n)
  "Import MIME part N, as hg patch.
When N is negative, force applying the patch, even if there are
outstanding uncommitted changes."
  (interactive "p")
  (if (and (numberp n) (< n 0))
      (progn
          (setq xhg-gnus-import-patch-force t)
          (setq n (- n)))
    (setq xhg-gnus-import-patch-force nil))
  (gnus-article-part-wrapper n 'xhg-gnus-import-patch))

(defun xhg-gnus-import-patch (handle)
  "Import a hg patch via gnus.  HANDLE should be the handle of the part."
  (let ((patch-file-name (concat (dvc-make-temp-name "gnus-xhg-import-") ".patch"))
        (window-conf (current-window-configuration))
        (import-dir))
    (gnus-summary-select-article-buffer)
    (save-excursion
      (goto-char (point-min))
      ;; handle does not seem to exist for text/x-patch ...
      (search-forward "text/x-patch; ")
      (mm-save-part-to-file (get-text-property (point) 'gnus-data) patch-file-name)
      (dolist (m xhg-apply-patch-mapping)
        (when (looking-at (car m))
          (setq import-dir (dvc-uniquify-file-name (cadr m))))))
    (delete-other-windows)
    (dvc-buffer-push-previous-window-config)
    (find-file patch-file-name)
    (setq import-dir (dvc-read-directory-name "Import hg patch to: " nil nil t import-dir))
    (when import-dir
      (let ((default-directory import-dir))
        (xhg-import patch-file-name xhg-gnus-import-patch-force)))
    (delete-file patch-file-name)
    (kill-buffer (current-buffer)) ;; the patch file
    (when (and import-dir (y-or-n-p "Run hg log in patched directory? "))
      (let ((default-directory import-dir))
        (xhg-log "tip:-10")))))

(defun xhg-gnus-article-view-status-for-import-patch (n)
  "View the status for the repository, where MIME part N would be applied as hg patch.

Use the same logic as in `xhg-gnus-article-import-patch' to guess the repository path
via `xhg-apply-patch-mapping'."
  (interactive "p")
  (gnus-article-part-wrapper n 'xhg-gnus-view-status-for-import-patch))

(defun xhg-gnus-view-status-for-import-patch (handle)
  "View the status for a repository before applying a hg patch via gnus.  HANDLE should be the handle of the part."
  (let ((window-conf (current-window-configuration))
        (import-dir))
    (gnus-summary-select-article-buffer)
    (save-excursion
      (goto-char (point-min))
      ;; handle does not seem to exist for text/x-patch ...
      (search-forward "text/x-patch; ")
      (dolist (m xhg-apply-patch-mapping)
        (when (looking-at (car m))
          (setq import-dir (dvc-uniquify-file-name (cadr m))))))
    (unless import-dir ;; when we find the directory in xhg-apply-patch-mapping don't ask for confirmation
      (setq import-dir (dvc-read-directory-name "View hg repository status for: " nil nil t import-dir)))
    (let ((default-directory import-dir))
      (xhg-status)
      (delete-other-windows)
      (dvc-buffer-push-previous-window-config window-conf))))


(provide 'tla-gnus)
;; arch-tag: a858ca04-352e-42a9-88c5-6324c3743711
;;; tla-gnus.el ends here
