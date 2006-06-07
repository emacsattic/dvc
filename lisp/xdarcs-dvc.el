;;; xdarcs-dvc.el --- The dvc layer for darcs

;; Copyright (C) 2006 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides the common dvc layer for darcs


;;; History:

;;

;;; Code:

(require 'xdarcs)
(eval-and-compile (require 'dvc-unified))

;;;###autoload
(dvc-register-dvc 'xdarcs "Darcs")

;;;###autoload
(defalias 'xdarcs-dvc-tree-root 'xdarcs-tree-root)

;;;###autoload
(defalias 'xdarcs-dvc-command-version 'xdarcs-command-version)

(defalias 'xdarcs-dvc-add-files 'xdarcs-add-files)

;;;###autoload
(defalias 'xdarcs-dvc-status    'xdarcs-whatsnew)

(defalias 'xdarcs-dvc-diff 'xdarcs-diff)

(provide 'xdarcs-dvc)
;; arch-tag: 795f1ef5-5d30-4989-abdd-edf9f0398215
;;; xdarcs-dvc.el ends here
