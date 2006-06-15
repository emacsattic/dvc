;;; bzr-revision.el --- Management of revision lists in bzr

;; Copyright (C) 2006  by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords:

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

;;

;;; Code:


(eval-when-compile (require 'cl))

(defstruct (bzr-revision-st)
  revno
  message
  creator
  branch-nick
  date
  merges
  )

;; bzr revision list

(defun bzr-revision-list-entry-patch-printer (elem)
  "TODO"
  (insert (if (dvc-revlist-entry-patch-marked elem)
              (concat " " dvc-mark " ") "   "))
  (let ((struct (dvc-revlist-entry-patch-struct elem)))
    (insert (dvc-face-add "revno: " 'dvc-header)
            (dvc-face-add (int-to-string (bzr-revision-st-revno struct))
                          'dvc-revision-name)
            "\n")
    (when dvc-revisions-shows-creator
      (insert "   " (dvc-face-add "committer: " 'dvc-header)
              (or (bzr-revision-st-creator struct) "?") "\n"))
    (when dvc-revisions-shows-date
      (insert "   " (dvc-face-add "timestamp: " 'dvc-header)
              (or (bzr-revision-st-date struct) "?") "\n"))
    (insert "   " (dvc-face-add "branch nick: " 'dvc-header)
            (or (bzr-revision-st-branch-nick struct) "?") "\n")
    (when dvc-revisions-shows-summary
      (insert "   " (dvc-face-add "message: " 'dvc-header)
              (or (bzr-revision-st-message struct) "?") "\n"))
    ))

;;; bzr log

(defun bzr-log-parse (log-buffer)
  "Parse the output of bzr log."
  (goto-char (point-min))
  (let ((root (bzr-tree-root)))
    (while (> (point-max) (point))
      (forward-line 1)
      (let ((start (point))
            (elem (make-bzr-revision-st)))
        (or (and (re-search-forward
                  "^------------------------------------------------------------$"
                  nil t)
                 (progn (beginning-of-line)
                        t))
            (goto-char (point-max)))
        (save-restriction
          (save-excursion
            (narrow-to-region start (- (point) 1))
            ;;(dvc-trace "parsing %S" (buffer-string))
            (goto-char (point-min))
            (while (re-search-forward "^\\([a-z][a-z ]*[a-z]\\):\\( \\|\n\\)" nil t)
              ;;(dvc-trace "match-string=%S" (match-string 1))
              (cond ((string= (match-string 1) "revno")
                     (setf (bzr-revision-st-revno elem)
                           (string-to-number
                            (buffer-substring-no-properties
                             (point) (line-end-position)))))
                    ((string= (match-string 1) "committer")
                     (setf (bzr-revision-st-creator elem)
                           (buffer-substring-no-properties
                            (point) (line-end-position))))
                    ((string= (match-string 1) "branch nick")
                     (setf (bzr-revision-st-branch-nick elem)
                           (buffer-substring-no-properties
                            (point) (line-end-position))))
                    ((string= (match-string 1) "timestamp")
                     (setf (bzr-revision-st-date elem)
                           (buffer-substring-no-properties
                            (point) (line-end-position))))
                    ((string= (match-string 1) "message")
                     ;;(dvc-trace "found message")
                     (re-search-forward "^[ \t]*")
                     (setf (bzr-revision-st-message elem)
                           (buffer-substring-no-properties
                            (point) (line-end-position)))
                     (goto-char (point-max)))
                    (t (dvc-trace "unmanaged field %S" (match-string 1))))
              (forward-line 1)
              (beginning-of-line))))
        (forward-line 1)
        (with-current-buffer log-buffer
          (ewoc-enter-last
           dvc-revlist-cookie
           `(entry-patch
             ,(make-dvc-revlist-entry-patch
               :dvc 'bzr
               :struct elem
               :rev-id `(bzr (revision (local ,root ,(bzr-revision-st-revno
                                                      elem))))))))))))

;;;###autoload
(defun bzr-log (path)
  "Run bzr log."
  (interactive (list default-directory))
  (dvc-build-revision-list 'bzr 'log path '("log") 'bzr-log-parse)
  (goto-char (point-min)))


(provide 'bzr-revision)
;; arch-tag: 7d1b4394-a149-4b1b-99e8-2174da2422d5
;;; bzr-revision.el ends here
