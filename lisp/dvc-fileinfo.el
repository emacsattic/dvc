;;; dvc-fileinfo.el --- An ewoc structure for displaying file information for DVC

;; Copyright (C) 2007 by all contributors

;; Author: Stephen Leake, <stephen_leake@stephe-leake.org>

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

;;

;;; Code:

(require 'dvc-defs)
(require 'dvc-core)
(require 'ewoc)
(eval-when-compile (require 'cl))

(defstruct (dvc-fileinfo-root
            (:constructor nil)
            (:copier nil))
  ;; no slots; root of class for ewoc entries.
  )

(defvar dvc-fileinfo-ewoc nil
  "Buffer-local ewoc for displaying workspace file status.

All dvc-fileinfo functions operate on this ewoc.
The elements must all be of class dvc-fileinfo-root.")
;; We could have each mode that uses fileinfo declare their own
;; buffer-local ewoc variable (ie dvc-diff-cookie). However, then the
;; interactive functions declared here (like dvc-fileinfo-next) would
;; take an ewoc argument, making them harder to bind directly to keys.
;;
;; We assume there will only be one ewoc structure of interest in a
;; given buffer.
(make-variable-buffer-local 'dvc-fileinfo-ewoc)

(defstruct (dvc-fileinfo-file
            (:include dvc-fileinfo-root)
	    (:copier nil))
  mark   	;; t/nil.
  dir		;; Directory the file resides in, relative to dvc-root.
  file	     	;; File name sans directory.
                ;; (concat dir file) gives a valid path.
  status	;; Symbol; see dvc-fileinfo-status-image for list
  more-status   ;; String; whatever else the backend has to say
  )

(defun dvc-fileinfo-status-image (status)
  "String image of STATUS."
  (ecase status
    (added          "added        ")
    (conflict       "conflict     ")
    (deleted        "deleted      ")
    (ignored        "ignored      ")
    (invalid        "invalid      ")
    (known          "known        ")
    (missing        "missing      ")
    (modified       "modified     ")
    (rename-source  "rename-source")
    (rename-target  "rename-target")
    (unknown        "unknown      ")))

(defstruct (dvc-fileinfo-dir
            (:include dvc-fileinfo-file)
	    (:copier nil))
  ;; no extra slots
  )

(defstruct (dvc-fileinfo-message
            (:include dvc-fileinfo-root)
	    (:copier nil))
  text ;; String
  )

(defun dvc-fileinfo-choose-face (status)
  "Return a face appropriate for STATUS."
  (case status
   ('added 'dvc-added)
   ('unknown 'dvc-unknown)
   ('modified 'dvc-modified)
   ('conflict 'dvc-conflict)
   ('move 'dvc-move)
   (t
    (dvc-trace "unknown status=%S" status)
    'default)))

(defun dvc-fileinfo-printer (fileinfo)
  "Ewoc pretty-printer for dvc-fileinfo types."
  (let ((inhibit-read-only t))
    (etypecase fileinfo
      (dvc-fileinfo-file ;; also matches dvc-fileinfo-dir
       (let* ((line (concat
                     (dvc-fileinfo-status-image (dvc-fileinfo-file-status fileinfo))
                     " "
                     (dvc-fileinfo-file-dir fileinfo)
                     (dvc-fileinfo-file-file fileinfo)))
              (face (if (dvc-fileinfo-file-mark fileinfo)
                        'dvc-marked
                      (dvc-fileinfo-choose-face (dvc-fileinfo-file-status fileinfo)))))
         (insert " ")
         (if (dvc-fileinfo-file-mark fileinfo)
             (insert dvc-mark)
           (insert " "))

         (insert " ")
         (insert (dvc-face-add line face))
         (if (> (length (dvc-fileinfo-file-more-status fileinfo)) 0)
             (progn
               (newline)
               (insert "      ")
               (insert (dvc-fileinfo-file-more-status fileinfo))))))

      (dvc-fileinfo-message
       (insert (dvc-fileinfo-message-text fileinfo)))
      )))

(defun dvc-fileinfo-current-fileinfo ()
  "Return the fileinfo for the ewoc element at point. Throws an
error if point is on a message."
  (let ((fileinfo (ewoc-data (ewoc-locate dvc-fileinfo-ewoc))))
    (etypecase fileinfo
      (dvc-fileinfo-file ; also matches dvc-fileinfo-dir
       fileinfo)

      (dvc-fileinfo-message
       (error "not on a file or directory")))))

(defun dvc-fileinfo-path (fileinfo)
  "Return directory and file from fileinfo, as a string."
  (concat (dvc-fileinfo-file-dir fileinfo) (dvc-fileinfo-file-file fileinfo)))

(defun dvc-fileinfo-current-file ()
  "Return a string giving the filename (including path from root)
of the file element on the line at point. Throws an error if
point is not on a file element line."
  (let ((fileinfo (dvc-fileinfo-current-fileinfo)))
    (dvc-fileinfo-path fileinfo)))

(defun dvc-fileinfo-delete-messages ()
  "Remove all message elements from the ewoc."
  (ewoc-filter dvc-fileinfo-ewoc
               (lambda (fileinfo)
                 (not (dvc-fileinfo-message-p fileinfo)))))

(defun dvc-fileinfo-kill ()
  "Remove the current element(s) from the ewoc."
  (interactive)

  (if (= 0 (length dvc-buffer-marked-file-list))
      ;; no marked files
      (progn
        ;; binding inhibit-read-only doesn't seem to work here
        (toggle-read-only 0)
        (ewoc-delete dvc-fileinfo-ewoc (ewoc-locate dvc-fileinfo-ewoc))
        (toggle-read-only 1))
    ;; marked files
    (setq dvc-buffer-marked-file-list nil)
    (ewoc-filter dvc-fileinfo-ewoc
                 (lambda (fileinfo)
                   (not (dvc-fileinfo-file-mark fileinfo)))
                 )))

(defun dvc-fileinfo-mark-dir-1 (fileinfo mark)
  (if (string-equal dir-compare (dvc-fileinfo-file-dir fileinfo))
      (let (file (dvc-fileinfo-path fileinfo))
        (setf (dvc-fileinfo-file-mark fileinfo) mark)
        (if mark
            (progn
              (add-to-list 'dvc-buffer-marked-file-list file))
          (setq dvc-buffer-marked-file-list (delete file dvc-buffer-marked-file-list)))
        (etypecase fileinfo
          (dvc-fileinfo-dir
           (dvc-fileinfo-mark-dir file mark)
           ;; return non-nil so this element is refreshed
           t)

          (dvc-fileinfo-file
           ;; return non-nil so this element is refreshed
           t)))))

(defun dvc-fileinfo-mark-dir (dir mark)
  "Set the mark for all files in DIR to MARK, recursively."
  (let ((dir-compare (file-name-as-directory dir)))
    (ewoc-map (lambda (fileinfo)
                (etypecase fileinfo
                  (dvc-fileinfo-file ; also matches dvc-fileinfo-dir
                   (dvc-fileinfo-mark-dir-1 fileinfo mark))

                  (dvc-fileinfo-message nil)))
              dvc-fileinfo-ewoc)))

(defun dvc-fileinfo-mark-file-1 (mark)
  "Set the mark for file under point to MARK. If a directory, mark all files in that directory."
  (let* ((current (ewoc-locate dvc-fileinfo-ewoc))
         (fileinfo (ewoc-data current)))
    (etypecase fileinfo
      (dvc-fileinfo-dir
       (let ((file (dvc-fileinfo-path fileinfo)))
         (setf (dvc-fileinfo-file-mark fileinfo) mark)
         (if mark
             (add-to-list 'dvc-buffer-marked-file-list file)
           (setq dvc-buffer-marked-file-list (delete file dvc-buffer-marked-file-list)))
         (ewoc-invalidate dvc-fileinfo-ewoc current)
         (dvc-fileinfo-mark-dir file mark)))

      (dvc-fileinfo-file
       (let ((file (dvc-fileinfo-path fileinfo)))
         (setf (dvc-fileinfo-file-mark fileinfo) mark)
         (if mark
             (add-to-list 'dvc-buffer-marked-file-list file)
           (setq dvc-buffer-marked-file-list (delete file dvc-buffer-marked-file-list)))
         (ewoc-invalidate dvc-fileinfo-ewoc current)))

      (dvc-fileinfo-message
       (error "not on a file or directory")))))

(defun dvc-fileinfo-mark-file ()
  "Mark the file under point. If a directory, mark all files in
that directory. Then move to next ewoc entry."
  (interactive)
  (dvc-fileinfo-mark-file-1 t)
  (dvc-fileinfo-next))

(defun dvc-fileinfo-unmark-file (&optional prev)
  "Unmark the file under point. If a directory, unmark all files
in that directory. If PREV non-nil, move to previous ewoc entry;
otherwise move to next."
  (interactive)
  (dvc-fileinfo-mark-file-1 nil)
  (if prev
      (dvc-fileinfo-prev)
    (dvc-fileinfo-next)))

(defun dvc-fileinfo-unmark-file-up ()
  "Unmark the file under point. If a directory, unmark all files
in that directory. Then move to previous ewoc entry."
  (interactive)
  (dvc-fileinfo-unmark-file t))

(defun dvc-fileinfo-mark-all ()
  "Mark all files and directories."
  (interactive)
  (ewoc-map (lambda (fileinfo)
              (etypecase fileinfo
                (dvc-fileinfo-file ; also matches dvc-fileinfo-dir
                 (setf (dvc-fileinfo-file-mark fileinfo) t)
                 (add-to-list 'dvc-buffer-marked-file-list (dvc-fileinfo-path fileinfo))
                 ;; return non-nil so this element is refreshed
                 t)

                (dvc-fileinfo-message
                 nil)))
            dvc-fileinfo-ewoc))

(defun dvc-fileinfo-unmark-all ()
  "Unmark all files and directories."
  (interactive)
  (setq dvc-buffer-marked-file-list nil)
  (ewoc-map (lambda (fileinfo)
              (etypecase fileinfo
                (dvc-fileinfo-file ; also matches dvc-fileinfo-dir
                 (if (dvc-fileinfo-file-mark fileinfo)
                     (progn
                       (setf (dvc-fileinfo-file-mark fileinfo) nil)
                       ;; return non-nil so this element is refreshed
                       t)))

                (dvc-fileinfo-message
                 nil)))
            dvc-fileinfo-ewoc))

(defun dvc-fileinfo-next ()
  "Move to the next ewoc entry."
  (interactive)
  (let* ((current (ewoc-locate dvc-fileinfo-ewoc))
         (cur-location (ewoc-location current))
         (next (ewoc-next dvc-fileinfo-ewoc current)))
    (cond
     ((> cur-location (point))
      ;; not exactly at an element; move there
      (goto-char cur-location))

     (next
      (goto-char (ewoc-location next)))

     (t
      ;; at last element
      nil))))

(defun dvc-fileinfo-prev ()
  "Move to the previous ewoc entry."
  (interactive)
  (let* ((current (ewoc-locate dvc-fileinfo-ewoc))
         (cur-location (ewoc-location current))
         (prev (ewoc-prev dvc-fileinfo-ewoc current)))
    (cond
     ((> (point) cur-location)
      (goto-char cur-location))

     (prev
      (goto-char (ewoc-location prev)))

     (t
      ;; at first element
      nil))))

(defun dvc-fileinfo-find-file (file)
  "Return ewoc element for file (full path)."
  (let ((elem (ewoc-nth dvc-fileinfo-ewoc 0)))
    (while (and elem
                (or (not (dvc-fileinfo-file-p (ewoc-data elem)))
                    (not (string= (expand-file-name
                                   (dvc-fileinfo-path (ewoc-data elem)))
                                  file))))
      (setq elem (ewoc-next dvc-fileinfo-ewoc elem)))
    (if elem
        elem
      (error "Can't find file %s in ewoc" file))))

(provide 'dvc-fileinfo)
;;; end of file
