;;; dvc-emacs.el --- Compatibility stuff for stable version of GNU Emacs

;; Copyright (C) 2004 by all contributors

;; This file is part of DVC.
;;
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

;; GNU Emacs is a creature; it grows day by day.  Some part of DVC is
;; based on a CVS version of GNU Emacs.  Therefore DVC didn't work
;; well on the stable version of GNU Emacs(21.x). This file provides
;; functions making DVC running on the stable version of GNU Emacs.
;; The most of all code comes from the CVS version of GNU Emacs.

;;; Code:

(unless (fboundp 'minibufferp)
  (defun minibufferp ()
    "Return non-nil if within a minibuffer."
    (equal (selected-window)
           (active-minibuffer-window))))

(defalias 'dvc-make-overlay 'make-overlay)
(defalias 'dvc-delete-overlay 'delete-overlay)
(defalias 'dvc-overlay-put 'overlay-put)
(defalias 'dvc-move-overlay 'move-overlay)
(defalias 'dvc-overlay-buffer 'overlay-buffer)
(defalias 'dvc-overlay-start 'overlay-start)
(defalias 'dvc-overlay-end 'overlay-end)
(defalias 'dvc-extent-detached-p 'ignore)
(defalias 'dvc-extent-start-open 'ignore)
(defalias 'dvc-mail-strip-quoted-names 'mail-strip-quoted-names)
(defalias 'dvc-character-to-event 'identity)
(defalias 'dvc-assq-delete-all 'assq-delete-all)
(defalias 'dvc-add-text-properties 'add-text-properties)
(defalias 'dvc-put-text-property 'put-text-property)
(defconst dvc-mouse-face-prop 'mouse-face)

;; Provide compatibility code for Emacs 21
;; from CVS Emacs
(if (fboundp 'line-number-at-pos)
    (defalias 'dvc-line-number-at-pos 'line-number-at-pos)
  (defun dvc-line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))

(defun dvc-emacs-make-temp-dir (prefix)
  "Make a temporary directory using PREFIX.
Return the name of the directory."
  (let ((dir (make-temp-name
              (expand-file-name prefix temporary-file-directory))))
    (make-directory dir)
    dir))

(defalias 'dvc-make-temp-dir 'dvc-emacs-make-temp-dir)

(provide 'dvc-emacs)
;; Local Variables:
;; End:

;;; dvc-emacs.el ends here

