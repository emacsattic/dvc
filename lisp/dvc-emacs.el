;;; dvc-emacs.el --- Compatibility stuff for old versions of GNU Emacs

;; Copyright (C) 2004, 2007-2008, 2014-2015 by all contributors

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

;;; Policy:
;;
;; The DVC baseline environment is the current release of Gnu Emacs.
;; However, we also support at least one previous release of Gnu
;; Emacs.
;;
;; There is current Gnu Emacs code used in DVC that is not present in
;; previous releases of Gnu Emacs.
;;
;; This file provides versions of that code that work with previous
;; versions of Gnu Emacs.
;;
;; In most cases, the code provided here should use names prefixed
;; with `dvc-'. This is to allow for the possibility that other
;; packages also provide the same function, but the code is broken in
;; some way.  Our version will work with DVC; theirs will work with
;; their package. DVC code must use the dvc- prefixed name.
;;
;; It might be that some code is truly _not_ broken, but it's much
;; easier to just use the dvc- prefix than to prove that.

;;; Code:

(unless (fboundp 'minibufferp)
  (defun minibufferp ()
    "Return non-nil if within a minibuffer."
    (equal (selected-window)
           (active-minibuffer-window))))

;; These have different names in Gnu Emacs and XEmacs; kept for
;; historical reasons (we haven't gotten around to cleaning out the
;; XEmacs stuff).
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

;; Provide features from Emacs 22 that were not in Emacs 21 (we no
;; longer support 21, but we have not gotten around to deleting these
;; yet).
;;
;; alphabetical by symbol name

(defalias 'dvc-derived-mode-p 'derived-mode-p)
(defalias 'dvc-ewoc-delete 'ewoc-delete)
(defalias 'dvc-expand-file-name 'expand-file-name)
(defalias 'dvc-line-number-at-pos 'line-number-at-pos)
(defalias 'dvc-redisplay 'redisplay)
(defalias 'dvc-window-body-height 'window-body-height)

;; Compatibility with Emacs 25

;; parse-integer provided by parse-time.el in emacs 24; replaced by
;; incompatible cl-parse-integer in 25
(require 'cl-lib)
(unless (fboundp 'cl-parse-integer)
  (defconst cl-digit-char-table
    (let* ((digits (make-vector 256 nil))
	   (populate (lambda (start end base)
		       (mapc (lambda (i)
			       (aset digits i (+ base (- i start))))
			     (number-sequence start end)))))
      (funcall populate ?0 ?9 0)
      (funcall populate ?A ?Z 10)
      (funcall populate ?a ?z 10)
      digits))
  (defun cl-digit-char-p (char &optional radix)
    "Test if CHAR is a digit in the specified RADIX (default 10).
If true return the decimal value of digit CHAR in RADIX."
    (or (and radix
	     (<= 2 radix)
	     (>= 36 radix))
	(signal 'args-out-of-range (list 'radix radix '(2 36))))
    (let ((n (aref cl-digit-char-table char)))
      (and n (< n (or radix 10)) n)))

  (cl-defun cl-parse-integer (string &key start end radix junk-allowed)
    "Parse integer from the substring of STRING from START to END.
STRING may be surrounded by whitespace chars (chars with syntax ` ').
Other non-digit chars are considered junk.
RADIX is an integer between 2 and 36, the default is 10.  Signal
an error if the substring between START and END cannot be parsed
as an integer unless JUNK-ALLOWED is non-nil."
    (cl-check-type string string)
    (let* ((start (or start 0))
	   (len	(length string))
	   (end   (or end len))
	   (radix (or radix 10)))
      (or (and (<= start len)
	       (>= end len))
	  (error "Bad interval: [%d, %d)" start end))
      (cl-flet ((skip-whitespace ()
				 (while (and (< start end)
					     (= 32 (char-syntax (aref string start))))
				   (setq start (1+ start)))))
	(skip-whitespace)
	(let ((sign (cl-case (and (< start end) (aref string start))
		      (?+ (cl-incf start) +1)
		      (?- (cl-incf start) -1)
		      (t  +1)))
	      digit sum)
	  (while (and (< start end)
		      (setq digit (cl-digit-char-p (aref string start) radix)))
	    (setq sum (+ (* (or sum 0) radix) digit)
		  start (1+ start)))
	  (skip-whitespace)
	  (cond ((and junk-allowed (null sum)) sum)
		(junk-allowed (* sign sum))
		((or (/= start end) (null sum))
		 (error "Not an integer string: `%s'" string))
		(t (* sign sum)))))))
  ) ;; parse-integer

(defun dvc-emacs-make-temp-dir (prefix)
  "Make a temporary directory using PREFIX.
Return the name of the directory."
  (let ((dir (make-temp-name
              (expand-file-name prefix temporary-file-directory))))
    (make-directory dir)
    dir))

(defalias 'dvc-make-temp-dir 'dvc-emacs-make-temp-dir)

(provide 'dvc-emacs)
;;; dvc-emacs.el ends here
