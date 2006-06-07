;;; xhg-mq.el --- dvc integration for hg's mq

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

;; For more information on mq see:
;;   http://www.selenic.com/mercurial/wiki/index.cgi/MqTutorial

(require 'xhg)

;;;###autoload
(defun xhg-qinit (&optional dir qinit-switch)
  "Run hg qinit.
When called without a prefix argument run hg qinit -c, otherwise hg qinit."
  (interactive
   (list (progn (setq qinit-switch (if current-prefix-arg "" "-c"))
                (expand-file-name (dvc-read-directory-name (format "Directory for hg qinit %s: " qinit-switch)
                                                           (or default-directory
                                                               (getenv "HOME")))))
         qinit-switch))
  (let ((default-directory dir))
    (dvc-run-dvc-sync 'xhg (list "qinit" qinit-switch)
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "hg qinit finished")))))

(defun xhg-qnew (patch-name &optional commit-description)
  "Run hg qnew.
When called with a prefix argument run hg qnew -m and ask for COMMIT-DESCRIPTION."
  (interactive
   (list (read-from-minibuffer "qnew patch name: ")
         (when current-prefix-arg (read-from-minibuffer "qnew commit message: "))))
  (dvc-run-dvc-sync 'xhg (list "qnew"
                               (when commit-description "-m")
                               (when commit-description (concat "\"" commit-description "\""))
                               patch-name)))

(defun xhg-qrefresh ()
  "Run hg qrefresh."
  (interactive)
  (dvc-run-dvc-sync 'xhg (list "qrefresh")))

(defun xhg-qapplied ()
  "Run hg qapplied."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("qapplied")))

(defun xhg-qunapplied ()
  "Run hg qunapplied."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("qunapplied")))

(defun xhg-qseries ()
  "Run hg qseries."
  (interactive)
  (dvc-run-dvc-display-as-info 'xhg '("qseries")))

(defun xhg-qversion ()
  "Run hg qversion."
  (interactive)
  (let ((version (dvc-run-dvc-sync 'xhg '("qversion")
                                   :finished 'dvc-output-buffer-handler))
        (version-string))
    (when version
      (setq version-string (nth 2 (split-string version " "))))
    (when (interactive-p)
      (message "Mercurial mq version: %s" version-string))
    version-string))

(defun xhg-qtop ()
  "Run hg qtop."
  (interactive)
  (let ((top (dvc-run-dvc-sync 'xhg '("qtop")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Mercurial qtop: %s" top))
    top))


(provide 'xhg-mq)
;; arch-tag: 2cb36064-5e56-48af-a836-79a3f5e80c8c
;;; xhg-mq.el ends here
