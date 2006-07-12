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

;; The following commands are available for hg's mq:
;; qapplied      print the patches already applied
;; qcommit       (No help text available)
;; qdelete       remove a patch from the series file
;; qdiff         diff of the current patch
;; qimport       import a patch
;; qinit         init a new queue repository
;; qnew          create a new patch
;; qnext         print the name of the next patch
;; qpop          pop the current patch off the stack
;; qprev         print the name of the previous patch
;; qpush         push the next patch onto the stack
;; qrefresh      update the current patch
;; qrestore      restore the queue state saved by a rev
;; qsave         save current queue state
;; qseries       print the entire series file
;; qtop          print the name of the current patch
;; qunapplied    print the patches not yet applied
;; qversion      print the version number

(defvar xhg-mq-submenu
  '("mq"
    ["mq refresh"  xhg-qrefresh t]
    ["mq diff"  xhg-qdiff t]
    ["mq push"  xhg-qpush t]
    ["mq pop"  xhg-qpop t]
    ["mq applied"  xhg-qapplied t]
    ["mq unapplied"  xhg-qunapplied t]
    ["mq series"  xhg-qseries t]
    ["mq delete"  xhg-qdelete t]
    "--"
    ["mq init" xhg-qinit t]
    ["mq new"  xhg-qnew t]
    ))

(defvar xhg-mq-sub-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?A] 'xhg-qapplied)
    (define-key map [?U] 'xhg-qunapplied)
    (define-key map [?S] 'xhg-qseries)
    (define-key map [?R] 'xhg-qrefresh)
    (define-key map [?P] 'xhg-qpush) ;; mnemonic: stack gets bigger
    (define-key map [?p] 'xhg-qpop) ;; mnemonic: stack gets smaller
    (define-key map [?t] 'xhg-qtop)
    (define-key map [?n] 'xhg-qnext)
    (define-key map [?p] 'xhg-qprev)
    (define-key map [?=] 'xhg-qdiff)
    (define-key map [?d] 'xhg-qdelete)
    (define-key map [?N] 'xhg-qnew)
    map)
  "Keymap used for xhg-mq commands.")

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

(defun xhg-qnew (patch-name &optional commit-description force)
  "Run hg qnew.
Asks for the patch name and an optional commit description.
If the commit description is not empty, run hg qnew -m \"commit description\"
When called with a prefix argument run hg qnew -f."
  (interactive
   (list (read-from-minibuffer "qnew patch name: ")
         (read-from-minibuffer "qnew commit message (empty for none): ")
         current-prefix-arg))
  (when (string= commit-description "")
    (setq commit-description nil))
  (dvc-run-dvc-sync 'xhg (list "qnew"
                               (when force "-f")
                               (when commit-description "-m")
                               (when commit-description (concat "\"" commit-description "\""))
                               patch-name)))

(defun xhg-qrefresh ()
  "Run hg qrefresh."
  (interactive)
  (dvc-run-dvc-sync 'xhg (list "qrefresh")))

(defun xhg-qpop (&optional all)
  "Run hg qpop.
When called with a prefix argument run hg qpop -a."
  (interactive
   (list current-prefix-arg))
  (let ((curbuf (current-buffer)))
    (dvc-run-dvc-sync 'xhg (list "qpop"
                                 (when all "-a")))
    (pop-to-buffer curbuf)))

(defun xhg-qpush (&optional all)
  "Run hg qpush.
When called with a prefix argument run hg qpush -a."
  (interactive
   (list current-prefix-arg))
  (let ((curbuf (current-buffer)))
    (dvc-run-dvc-sync 'xhg (list "qpush"
                                 (when all "-a")))
    (pop-to-buffer curbuf)))

(defun xhg-qapplied ()
  "Run hg qapplied."
  (interactive)
  (let ((curbuf (current-buffer)))
    (dvc-run-dvc-display-as-info 'xhg '("qapplied") nil "hg qapplied:\n")
    (pop-to-buffer curbuf)))

(defun xhg-qunapplied ()
  "Run hg qunapplied."
  (interactive)
  (let ((curbuf (current-buffer)))
    (dvc-run-dvc-display-as-info 'xhg '("qunapplied") nil "hg qunapplied:\n")
    (pop-to-buffer curbuf)))

(defun xhg-qseries ()
  "Run hg qseries."
  (interactive)
  (if (interactive-p)
      (let ((curbuf (current-buffer)))
        (dvc-run-dvc-display-as-info 'xhg '("qseries") nil "hg qseries:\n")
        (pop-to-buffer curbuf))
    (dvc-run-dvc-sync 'xhg '("qseries")
                      :finished 'dvc-output-buffer-split-handler)))

(defun xhg-qdiff (&optional file)
  "Run hg qdiff."
  (interactive)
  (let ((curbuf (current-buffer)))
    (dvc-run-dvc-display-as-info 'xhg (list "qdiff" file) nil (format "hg qdiff %s:\n" (xhg-qtop)))
    (with-current-buffer "*xhg-info*"
      (diff-mode))
    (pop-to-buffer curbuf)))

(defun xhg-qdelete (patch)
  "Run hg qdelete"
  (interactive (list (completing-read "Delete mq patch: " (xhg-qseries))))
  (dvc-run-dvc-sync 'xhg (list "qdelete" patch)))

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

(defun xhg-qnext ()
  "Run hg qnext."
  (interactive)
  (let ((next (dvc-run-dvc-sync 'xhg '("qnext")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Mercurial qnext: %s" next))
    next))

(defun xhg-qprev ()
  "Run hg qprev."
  (interactive)
  (let ((prev (dvc-run-dvc-sync 'xhg '("qprev")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Mercurial qprev: %s" prev))
    prev))


(provide 'xhg-mq)
;; arch-tag: 2cb36064-5e56-48af-a836-79a3f5e80c8c
;;; xhg-mq.el ends here
