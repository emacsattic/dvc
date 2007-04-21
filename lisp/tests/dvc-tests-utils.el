;;; dvc-tests-utils.el --- Utilities for automated regression tests

;; Copyright (C) 2007 Stephen Leake

;; Author: Stephen Leake

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

(defun dvc-tests-wait-async ()
  "Waits for all asynchronous dvc processes to terminate."
  (while dvc-process-running
    (message "Processes: %s" dvc-process-running)
    (sit-for 0.2)))

(provide 'dvc-tests-utils)
;; end of file
