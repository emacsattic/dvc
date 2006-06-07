;;; dvc-gnus.el --- dvc integration to gnus

;; Copyright (C) 2003-2006 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>

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
  "Insinuate Gnus for each registered back-end.

Runs (<backend>-insinuate-gnus) for each registered back-end having
this function."
  (interactive)
  (mapcar (lambda (x)
            (let ((fn (dvc-function x "insinuate-gnus" t)))
              (when (fboundp fn)
                (dvc-trace "Insinuating Gnus for %S" x)
                (funcall fn))))
          dvc-registered-backends))

(provide 'dvc-gnus)
;; arch-tag: 6afaa64c-9e9f-4600-beb6-1276365400d6
;;; dvc-gnus.el ends here
