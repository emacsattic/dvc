;;; xhg-gnus.el --- dvc integration to gnus

;; Copyright (C) 2003-2006 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>

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

;; gnus is optional. Load it at compile-time to avoid warnings.
(eval-when-compile
  (condition-case nil
      (progn
        (require 'gnus)
        (require 'gnus-art)
        (require 'gnus-sum))
    (error nil)))

;;;###autoload
(defun xhg-insinuate-gnus ()
  "Integrate Xhg into Gnus.
The following keybindings are installed for gnus-summary:
K t i `xhg-gnus-article-import-patch'
K t s `xhg-gnus-article-view-status-for-import-patch'"
  (interactive)
  (dvc-gnus-initialize-keymap)
  (define-key gnus-summary-dvc-submap [?i] 'xhg-gnus-article-import-patch)
  (define-key gnus-summary-dvc-submap [?s] 'xhg-gnus-article-view-status-for-import-patch)
  )

(provide 'xhg-gnus)
;;; xhg-gnus.el ends here
