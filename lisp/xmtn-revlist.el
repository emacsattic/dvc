;;; xmtn-revlist.el --- Interactive display of revision histories for monotone

;; Copyright (C) 2008 - 2015 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
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

;;; Commentary:

;; This file is part of xmtn and implements an interactive display of
;; revision histories.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-when-compile (require 'cl-macs))
(require 'dvc-unified)
(require 'dvc-revlist)
(require 'xmtn-ids)
(require 'xmtn-basic-io)
(require 'xmtn-automate)
(require 'xmtn-match)
(require 'xmtn-dvc)

(defvar xmtn--revlist-*info-generator-fn* nil
"Buffer-local variable pointing to a function that generates a
list of revisions to display in a revlist buffer. Called with one
arg; root. Result is of the form:
    ((header-lines)
     (footer-lines)
     (revisions))")
(make-variable-buffer-local 'xmtn--revlist-*info-generator-fn*)

(defvar xmtn--revlist-*path* nil
"Buffer-local variable containing path argument for log")
(make-variable-buffer-local 'xmtn--revlist-*path*)

(defvar xmtn--revlist-*to* nil
"Buffer-local variable containing path argument for log")
(make-variable-buffer-local 'xmtn--revlist-*to*)

(cl-defstruct (xmtn--revlist-entry (:constructor xmtn--make-revlist-entry))
  revision-hash-id
  branches
  authors
  dates
  changelogs
  tags)

;;;###autoload
(defun xmtn-revision-refresh-maybe ()
  ;; This is called to notify us whenever `dvc-revisions-shows-date',
  ;; `dvc-revisions-shows-creator' or `dvc-revisions-shows-summary'
  ;; change.
  ;;
  ;; There is nothing we need to do in response to this, though.
  nil)

;;;###autoload
(defun xmtn-revision-list-entry-patch-printer (patch)
  (let ((entry (dvc-revlist-entry-struct patch)))
    (assert (typep entry 'xmtn--revlist-entry))
    (insert (format " %s %s\n"
                    (if (dvc-revlist-entry-marked patch) "*" " ")
                    (xmtn--revlist-entry-revision-hash-id entry)))
    (dolist (tag (xmtn--revlist-entry-tags entry))
      (insert (format "   Tag: %s\n" tag)))
    (let ((authors (xmtn--revlist-entry-authors entry))
          (dates (xmtn--revlist-entry-dates entry))
          (changelogs (xmtn--revlist-entry-changelogs entry)))
      (let ((len (max (length authors) (length dates) (length changelogs))))
        (macrolet ((fillf (x)
                     `(setq ,x (append ,x (make-list (- len (length ,x)) nil)))))
          (fillf authors)
          (fillf dates)
          (fillf changelogs))
        (assert (eql (length authors) len)
                (eql (length dates) len)
                (eql (length changelogs) len)))
      (cl-loop
       ;; Matching the k-th author cert with the k-th date cert
       ;; and the k-th changelog cert, like we do here, is unlikely to
       ;; be correct in general.  That the relationship between date,
       ;; message and author of a commit is lost appears to be a
       ;; limitation of monotone's current design.
       for author in authors
       for date in dates
       for changelog in changelogs
       do
       (insert (format "   %s  %s\n"
		       (or date "date unknown")
		       (or author "author unknown")))
       (if (null changelog)
	   (insert (format "   No changelog"))
	 (let ((lines (split-string changelog "\n")))
	   (dolist (line (if dvc-revlist-brief
			     (and lines (list (first lines)))
			   lines))
	     (insert (format "     %s\n" line)))))))))

(defun xmtn--build-revlist-entry (rev)
  "Return an `xmtn--revlist-entry' for REV (back-end id)."
  (let ((branches (list))
	(authors (list))
	(dates (list))
	(changelogs (list))
	(tags (list)))
    (xmtn--map-parsed-certs
     default-directory
     rev
     (lambda (key signature name value trusted)
       (declare (ignore key)
		(ignore trusted))
       (cond
	((equal name "author")
	 (push value authors))
	((equal name "date")
	 (push value dates))
	((equal name "changelog")
	 (push value changelogs))
	((equal name "branch")
	 (push value branches))
	((equal name "tag")
	 (push value tags))
	(t
	 nil))))
    (setq authors (nreverse authors)
	  dates (nreverse dates)
	  changelogs (nreverse changelogs)
	  branches (nreverse branches)
	  tags (nreverse tags))
    (xmtn--make-revlist-entry
     :revision-hash-id rev
     :branches branches
     :authors authors
     :dates dates
     :changelogs changelogs
     :tags tags)))

(defun xmtn--revlist-setup-ewoc (ewoc header footer revs last-n)
  "Add to EWOC HEADER, FOOTER, dvc-revlist-entry for all REVS."
  (ewoc-set-hf ewoc header footer)
  (ewoc-filter ewoc (lambda (x) nil))   ; Clear it.
  ;; FIXME: setup should not modify order; this should be a waste of
  ;; time or wrong. This was here historically; see
  ;; xmtn--log-generator for comment on why I have not removed it. I
  ;; have not investigated order problems with other revlists.
  (setq revs (xmtn--toposort default-directory revs))
  (if last-n
      (let ((len (length revs)))
        (if (> len last-n)
            (setq revs (nthcdr (- len last-n) revs)))))
  (setq revs (coerce revs 'vector))
  (dotimes-with-progress-reporter (i (length revs))
      (case (length revs)
        (1 "Setting up revlist buffer (1 revision)...")
        (t (format "Setting up revlist buffer (%s revisions)..."
                   (length revs))))
    (let ((rev (aref revs i)))
      (ewoc-enter-last
       ewoc
       (make-dvc-revlist-entry
	:marked nil
	:rev-id (list 'xmtn (list 'revision rev))
	:log-buffer nil
	:diff-buffer nil
	:struct (xmtn--build-revlist-entry rev)))
      ))
  nil)

(defun xmtn-revision-st-message (entry)
  (mapconcat #'identity (xmtn--revlist-entry-changelogs entry) "\n"))

;; FIXME: replace with dvc-revlist-refresh
(defun xmtn--revlist-refresh ()
  (let ((root default-directory))
    (destructuring-bind (header-lines footer-lines revs)
        (funcall xmtn--revlist-*info-generator-fn* root)
      (let ((ewoc dvc-revlist-ewoc)
	    (count (length revs))
	    (last-n dvc-revlist-last-n))
        (xmtn--revlist-setup-ewoc ewoc
                                  (with-temp-buffer
                                    (dolist (line header-lines)
                                      (if (null line)
                                          (insert ?\n)
                                        (insert line ?\n)))
                                    (when header-lines (insert ?\n))
				    (insert
				     (cond
				      ((= 0 count) "No revisions")
				      ((= 1 count) "1 revision:")
				      ((or (null last-n)
					   (> last-n count))
				       (format "%d of %d revisions:" count count))
				      (t (format "%d of %d revisions:" last-n count))))
				    (insert ?\n)
				    (buffer-string))
                                  (with-temp-buffer
                                    (when footer-lines (insert ?\n))
                                    (dolist (line footer-lines)
                                      (if (null line)
                                          (insert ?\n)
                                        (insert line ?\n)))
                                    (buffer-string))
                                  revs
                                  dvc-revlist-last-n)
        (if (null (ewoc-nth ewoc 0))
            (goto-char (point-max))
          (ewoc-goto-node ewoc (ewoc-nth ewoc 0))))))
  nil)

;; FIXME: replace with dvc-revlist-setup
(defun xmtn--setup-revlist (root info-generator-fn path first-line-only-p last-n &optional to)
  ;; Adapted from `dvc-build-revision-list'.
  ;; See xmtn--revlist-*info-generator-fn*
  (xmtn-automate-cache-session root)
  (let ((dvc-temp-current-active-dvc 'xmtn)
        (buffer (dvc-revlist-create-buffer
                 'log root 'xmtn--revlist-refresh first-line-only-p last-n)))
    (with-current-buffer buffer
      (setq xmtn--revlist-*info-generator-fn* info-generator-fn)
      (setq xmtn--revlist-*path* (when path (file-relative-name path root)))
      (setq xmtn--revlist-*to* to)
      (xmtn--revlist-refresh))
    (xmtn--display-buffer-maybe buffer nil)
    ;; return buffer so it can be popped to and/or cleaned up
    buffer))

;;;###autoload
(defun xmtn-dvc-log (path last-n)
  ;; path may be nil or a file. The front-end ensures that
  ;; 'default-directory' is set to a tree root.
  (xmtn--setup-revlist
   default-directory
   'xmtn--log-generator
   path
   t ;; first-line-only-p
   last-n))

;;;###autoload
(defun xmtn-log (&optional path last-n)
  ;; This could be generated by dvc-back-end-wrappers, but xhg, xgit
  ;; versions of dvc-log are too different.
  (interactive)
  (let ((dvc-temp-current-active-dvc 'xmtn))
    (if (interactive-p)
        (call-interactively 'dvc-log)
      (funcall 'dvc-log path last-n))))

;;;###autoload
(defun xmtn-dvc-changelog (&optional path)
  (xmtn--setup-revlist
   (dvc-tree-root)
   'xmtn--log-generator
   path
   nil ;; first-line-only-p
   nil ;; last-n
   ))

(defun xmtn--log-generator (root)
  (let* ((default-directory root)
	 (branch (xmtn-dvc-branch))
	 (header
	  (list (format "Log for branch %s" branch)))
	 (options nil)
	 (command
	  (if xmtn--revlist-*path*
	      (list "log" xmtn--revlist-*path*)
	    (list "log")))
	 )
    (when dvc-revlist-last-n
      ;; FIXME: this gives most the recent date first, we want that
      ;; last. See mtn issue 118 for why we can't fix that with more
      ;; options. The 'toposort' in xmtn--revlist-setup-ewoc puts it
      ;; in the desired date order. In general, it would be better
      ;; if revlist-setup did not alter the order.
      (add-to-list 'options (format "%d" dvc-revlist-last-n))
      (add-to-list 'options "last"))

    (when xmtn--revlist-*to*
      (add-to-list 'options xmtn--revlist-*to*)
      (add-to-list 'options "to"))

    ;; See xmtn--revlist-*info-generator-fn* for result format
    (list header
	  '() ;; footer
	  (xmtn-automate-command-output-lines ;; revisions
	   root
	   (cons options command)))))

(defun xmtn-revlist-show-conflicts ()
  "If point is on a revision that has two parents, show conflicts
from the merge."
  ;; IMPROVEME: We just use the xmtn conflicts machinery for now. It
  ;; would be better if we had a read-only version of it.
  (interactive)
  (let ((changelog
	 (car (xmtn--revlist-entry-changelogs
	       (dvc-revlist-entry-struct
		(ewoc-data (ewoc-locate dvc-revlist-ewoc))))))
        start end left-branch left-rev right-branch right-rev)
    ;; string-match does _not_ set up match-strings properly, so we do this instead
    (cond
     ((string= (substring changelog 0 9) "propagate")
      (setq start (+ 1 (string-match "'" changelog)))
      (setq end (string-match "'" changelog start))
      (setq left-branch (substring changelog start end))

      (setq start (+ 6 (string-match "(head" changelog end)))
      (setq end (string-match ")" changelog start))
      (setq left-rev (substring changelog start end))

      (setq start (+ 1 (string-match "'" changelog end)))
      (setq end (string-match "'" changelog start))
      (setq right-branch (substring changelog start end))

      (setq start (+ 6 (string-match "(head .*)" changelog end)))
      (setq end (string-match ")" changelog start))
      (setq right-rev (substring changelog start end)))


     ((or
       (string= (substring changelog 0 5) "merge")
       (string= (substring changelog 0 14) "explicit merge"))
      (setq start (+ 4 (string-match "of" changelog)))
      (setq end (string-match "'" changelog start))
      (setq left-rev (substring changelog start end))

      (setq start (+ 5 (string-match "and" changelog start)))
      (setq end (string-match "'" changelog start))
      (setq right-rev (substring changelog start end)))

     (t
      (error "not on a two parent revision")))

    (xmtn-conflicts-review
     default-directory ; left-work
     left-rev
     default-directory ; right-work
     right-rev
     left-branch
     right-branch
     t)))

;;;###autoload
(defvar xmtn-revlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "MC" 'xmtn-revlist-show-conflicts)
    (define-key map "CC" 'xmtn-conflicts-clean)
    map))

(easy-menu-define xmtn-revlist-mode-menu xmtn-revlist-mode-map
  "Mtn specific revlist menu."
  `("DVC-Mtn"
    ["Show merge conflicts after merge" xmtn-revlist-show-conflicts t]
    ["Clean conflicts resolutions" xmtn-conflicts-clean t]
    ))

(define-derived-mode xmtn-revlist-mode dvc-revlist-mode "xmtn-revlist"
  "Add back-end-specific commands for dvc-revlist.")

(dvc-add-uniquify-directory-mode 'xmtn-revlist-mode)

;;;###autoload
(defun xmtn-dvc-missing ()
  (let* ((branch (xmtn-dvc-branch))
         (heads (xmtn--heads default-directory branch)))
    (if (/= 1 (length heads))
      (error "%d heads, need merge; use `xmtn-status-one'" (length heads)))

    (xmtn--setup-revlist
     default-directory
     (lambda (root)
       (let ((revs
	      (xmtn-automate-command-output-lines
	       root
	       (cons (list "from" "h:" "to" "w:") (list "log")))))
	 (list
	  (list ;; header
	   (format "workspace %s" root)
	   nil ;; blank line
	   "Revisions that are not in base revision")
	  '() ;; footer
	  revs)))
     nil ;; path
     nil ;; first-line-only-p
     ;; When the missing revs are due to a propagate, there can be a
     ;; lot of them, but we only really need to see the revs since the
     ;; propagate. So dvc-log-last-n is appropriate. We use
     ;; dvc-log-last-n, not dvc-revlist-last-n, because -log is user
     ;; customizable.
     dvc-log-last-n))
  nil)

;;;###autoload
(defun xmtn-dvc-update-review ()
  "For `dvc-update-review'."
  (interactive "D")
  (xmtn--setup-revlist
   default-directory
   (lambda (root)
     (let ((revs
	    (xmtn-automate-command-output-lines
	     root
	     (cons (list "from" "w:" "to" "u:") (list "log")))))
       (list
	(list ;; header
	 (format "workspace %s" root)
	 nil ;; blank line
	 "Revisions in last update")
	'() ;; footer
	revs)))
   nil ;; path
   nil ;; first-line-only-p
   dvc-log-last-n))

(defvar xmtn--*selector-history* nil)

;;;###autoload
(defun xmtn-view-revlist-for-selector (selector)
  "Display a revlist buffer showing the revisions matching SELECTOR."
  (interactive (list (read-string "View revlist for selector: "
                                  nil
                                  'xmtn--*selector-history*
                                  nil)))
  (check-type selector string)
  (let ((root (dvc-tree-root)))
    (lexical-let ((selector selector))
      (xmtn--setup-revlist
       root
       (lambda (root)
         (let* ((revision-hash-ids (xmtn--expand-selector root selector))
                (count (length revision-hash-ids)))
           (list
            (list ; header
	     (format "workspace %s" root)
	     (if (with-syntax-table (standard-syntax-table)
		   (string-match "\\`\\s *\\'" selector))
		 "Blank selector"
	       (format "Selector %s" selector))
	     "Revisions matching selector")
            '() ; footer
            revision-hash-ids)))
       nil ;; path
       nil ;; first-line-only-p
       nil ;; last-n
       )))
  nil)

;; This generates the output shown when the user hits RET on a
;; revision in the revlist buffer.
;;;###autoload
(defun xmtn-dvc-revlog-get-revision (revision-id)
  (let ((root (dvc-tree-root)))
    (let ((backend-id (xmtn--resolve-revision-id root revision-id)))
      (xmtn-match backend-id
        ((local-tree $path) (error "Not implemented"))
        ((revision $revision-hash-id)
         (with-output-to-string
           (cl-flet ((write-line (format &rest args)
                              (princ (apply #'format format args))
                              (terpri)))
             (write-line "Revision %s" revision-hash-id)
             ;; FIXME: It would be good to sort the standard certs
             ;; like author, date, branch, tag and changelog into
             ;; some canonical order and format changelog specially
             ;; since it usually spans multiple lines.
             (xmtn--map-parsed-certs
              root revision-hash-id
              (lambda (key signature name value trusted)
                (declare (ignore key))
                (if (not trusted)
                    (write-line "Untrusted cert, name=%s" name)
                  (write-line "%s: %s" name value)))))))))))

(defun xmtn-revlist-update ()
  "Update current tree to the revision at point.

To be invoked from an xmtn revlist buffer."
  (interactive)
  (let* ((root (dvc-tree-root))
         (entry (ewoc-data (ewoc-locate dvc-revlist-ewoc)))
         (target-hash-id (xmtn--revlist-entry-revision-hash-id entry)))
    (xmtn--update root target-hash-id nil nil)))

(defun xmtn-dvc-revlist-entry (rev)
  "For `dvc-revlist-entry'."
  (make-dvc-revlist-entry
   :marked nil
   :rev-id (list 'xmtn (list 'revision rev))
   :log-buffer nil
   :diff-buffer nil
   :struct (xmtn--build-revlist-entry rev)))

(provide 'xmtn-revlist)

;;; xmtn-revlist.el ends here
