;;; xgit-core.el --- Common definitions for git support in DVC

;; Copyright (C) 2006-2007 by all contributors

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

;; This file provides the low-level functions used by the git interface
;; from DVC.


;;; History:

;;

;;; Code:

(require 'dvc-core)

;; Settings for git
(defvar xgit-executable
  "git"
  "The executable used for the git commandline client.")

(defvar xgit-log-edit-file-name
  "++xgit-log-edit"
  "The filename, used to store the log message before commiting.
Usually that file is placed in the tree-root of the working tree.")

;;;###autoload
(defun xgit-tree-root (&optional location no-error interactive)
  "Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an .git/
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not a
git managed tree (but return nil)."
  (dvc-tree-root-helper ".git/" (or interactive (interactive-p))
                        "%S is not in a git tree!"
                        location no-error))


(defun xgit-tree-has-head ()
  "Return t, if the git repository has a valid HEAD entry.
It will be nil before the initial commit."
  (file-readable-p (concat (xgit-tree-root) "/.git/HEAD")))

(provide 'xgit-core)
;;; xgit-core.el ends here