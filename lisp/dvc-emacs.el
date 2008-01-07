;;; dvc-emacs.el --- Compatibility stuff for old version of GNU Emacs

;; Copyright (C) 2004, 2007 - 2008 by all contributors

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

;; GNU Emacs is a creature; it grows day by day. DVC assumes GNU Emacs
;; 22, and in the future may use features from a non-released version.
;; But many people are still using Emacs 21, so this file provides
;; functions from the later versions used by DVC.

;;; Code:

(unless (fboundp 'minibufferp)
  (defun minibufferp ()
    "Return non-nil if within a minibuffer."
    (equal (selected-window)
           (active-minibuffer-window))))

;; These have different names in Gnu Emacs and XEmacs; see dvc-xemacs.el
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
;; features from Emacs 22
(if (not (boundp 'delay-mode-hooks))
    (defvar delay-mode-hooks nil))

(if (not (fboundp 'redisplay))
    (defun redisplay (foo) nil))

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

;; Emacs 21 has ewoc, but not ewoc-delete
(require 'ewoc)
(if (not (fboundp 'ewoc-delete))
  (defun ewoc-delete (ewoc &rest nodes)
    "Delete NODES from EWOC."
    (ewoc--set-buffer-bind-dll-let* ewoc
        ((L nil) (R nil) (last (ewoc--last-node ewoc)))
      (dolist (node nodes)
        ;; If we are about to delete the node pointed at by last-node,
        ;; set last-node to nil.
        (when (eq last node)
          (setf last nil (ewoc--last-node ewoc) nil))
        (delete-region (ewoc--node-start-marker node)
                       (ewoc--node-start-marker (ewoc--node-next dll node)))
        (set-marker (ewoc--node-start-marker node) nil)
        (setf L (ewoc--node-left  node)
              R (ewoc--node-right node)
              ;; Link neighbors to each other.
              (ewoc--node-right L) R
              (ewoc--node-left  R) L
              ;; Forget neighbors.
              (ewoc--node-left  node) nil
              (ewoc--node-right node) nil)))))

(provide 'dvc-emacs)
;; Local Variables:
;; End:

;;; dvc-emacs.el ends here

