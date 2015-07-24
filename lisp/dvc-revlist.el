;;; dvc-revlist.el --- Revision list in DVC

;; Copyright (C) 2005-2009, 2011, 2013 - 2015 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(eval-when-compile (require 'cl-macs))

(eval-and-compile
  (require 'dvc-lisp)
  (require 'dvc-utils)
  (require 'dvc-core)
  )

(require 'dvc-ui)

;; Display parameters
(defvar-local dvc-revlist-brief nil)

(defvar-local dvc-revlist-last-n nil
  "Buffer-local value of dvc-log-last-n.")

(defvar-local dvc-revlist-path nil)

(cl-defstruct (dvc-revlist-entry)
  marked
  rev-id ;; DVC revision ID.
  log-buffer ;; buffer displaying revlog of rev-id
  diff-buffer ;; buffer displaying diff of rev-id
  struct ;; back-end struct
)

(defvar-local dvc-revlist-ewoc nil
  "Ewoc for dvc-revlist.")

(defun dvc-revlist-printer (elem)
  "Print an element ELEM of the ewoc."
  ;; FIXME: handle non-struct elements here
  ;; FIXME: rename to revlist-entry-printer
  (dvc-call "revision-list-entry-patch-printer" elem))

(dvc-make-ewoc-next dvc-revlist-next dvc-revlist-ewoc)

(dvc-make-ewoc-prev dvc-revlist-prev dvc-revlist-ewoc)

(defun dvc-revlist-mark ()
  "Mark revision at point."
  (interactive)
  (let* ((entry (ewoc-locate dvc-revlist-ewoc))
         (data (ewoc-data entry)))
    (setf (dvc-revlist-entry-marked data) t)
    (ewoc-invalidate dvc-revlist-ewoc entry)
    (dvc-revlist-next)))

(defun dvc-revlist-marked-revisions ()
  "Return the revisions that are currently marked."
  (let ((result '()))
    (ewoc-map
     (lambda (data)
       (when (dvc-revlist-entry-marked data)
	 (push (dvc-revlist-entry-struct data) result)))
     dvc-revlist-ewoc)
    (nreverse result)))

(defun dvc-revlist-unmark-revision ()
  "Unmark the revision at point."
  (interactive)
  (let* ((entry (ewoc-locate dvc-revlist-ewoc))
         (data (ewoc-data entry)))
    (setf (dvc-revlist-entry-marked data) nil)
    (ewoc-invalidate dvc-revlist-ewoc entry)
    (dvc-revlist-next)))

(defun dvc-revlist-unmark-all ()
  "Unmark all revisions."
  (interactive)
  (save-excursion
    (ewoc-map
     (lambda (data)
       (setf (dvc-revlist-entry-marked data) nil))
     dvc-revlist-ewoc)
    (ewoc-refresh dvc-revlist-ewoc)
    ))

(defun dvc-revlist-more (&optional delta)
  "If revision list was limited by `dvc-revlist-last-n', show more revisions.
Increment DELTA may be specified interactively; default 10."
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 10)))
  (when dvc-revlist-last-n
    (setq dvc-revlist-last-n (+ dvc-revlist-last-n delta))
    (dvc-generic-refresh)))

(defun dvc-revlist-toggle-brief ()
  "Toggle between brief and full revisions."
  (interactive)
  (setq dvc-revlist-brief (not dvc-revlist-brief))
  (dvc-generic-refresh))

(defun dvc-revlist-current-rev ()
  "Retrieve the DVC revision for the current ewoc entry."
  (let* ((rev-id (dvc-revlist-entry-rev-id
                 (ewoc-data (ewoc-locate dvc-revlist-ewoc))))
         (type (dvc-revision-get-type rev-id))
         (data (dvc-revision-get-data rev-id)))
    (case type
      (revision (nth 0 data))
      (t (error "No revision at point")))))

(autoload 'dvc-revlog-revision "dvc-revlog")

(defun dvc-revlist-show-item (&optional scroll-down)
  "Show a changeset for the current revision."
  (interactive)
  (let ((elem (ewoc-data (ewoc-locate dvc-revlist-ewoc)))
	(revlist-buf (current-buffer)))
    ;; reuse existing buffer if possible
    (let ((buffer (dvc-revlist-entry-log-buffer elem)))
      (if (and buffer (buffer-live-p buffer))
	  (dvc-buffer-show-or-scroll buffer scroll-down)
	(setq buffer
	      (setf (dvc-revlist-entry-log-buffer elem)
		    (dvc-revlog-revision (dvc-revlist-entry-rev-id elem))))
	(with-current-buffer buffer (goto-char (point-min))))
      (pop-to-buffer revlist-buf))))

(dvc-make-bymouse-function dvc-revlist-show-item)

(defun dvc-revlist-show-item-scroll-down ()
  (interactive)
  (dvc-revlist-show-item t))

(defun dvc-revlist-diff (&optional scroll-down)
  "Show the diff for the current revision."
  (interactive)
  (let ((data (ewoc-data (ewoc-locate dvc-revlist-ewoc))))
    (let ((buffer (dvc-revlist-entry-diff-buffer data))
          (revlist-buf (current-buffer)))
      (if (and buffer (buffer-live-p buffer))
          (dvc-buffer-show-or-scroll buffer scroll-down)
        (setf (dvc-revlist-entry-diff-buffer data)
              (let* ((rev-id (dvc-revlist-entry-rev-id data))
                     (rev-type (dvc-revision-get-type rev-id))
                     (rev-data (dvc-revision-get-data rev-id)))
                (unless (eq rev-type 'revision)
                  (error "Only 'revision type is supported here. Got %S" rev-type))
                (let* ((prev-rev-id `(,(car rev-id) (previous-revision
                                                     ,(cadr rev-id) 1))))
                  (dvc-delta prev-rev-id rev-id))))
        (setq buffer (dvc-revlist-entry-diff-buffer data)))
      (with-current-buffer buffer
        (setq dvc-partner-buffer revlist-buf))
      (pop-to-buffer revlist-buf)
      (setq dvc-partner-buffer buffer))))

(defun dvc-revlist-diff-to-current-tree ()
  "Show the diff between the revision at point and the local tree."
  (interactive)
  (let ((data (ewoc-data (ewoc-locate dvc-revlist-ewoc))))
    (dvc-diff (dvc-revlist-entry-rev-id data) (dvc-tree-root) nil)))

(defun dvc-revlist-update ()
  "Update current workspace to revision at point"
  (interactive)
  (dvc-update (dvc-revlist-entry-rev-id (ewoc-data (ewoc-locate dvc-revlist-ewoc)))))

(defvar dvc-revlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?g] 'dvc-generic-refresh)
    (define-key map [?+] 'dvc-revlist-more)
    (define-key map [?b] 'dvc-revlist-toggle-brief)
    (define-key map [?n] 'dvc-revlist-next)
    (define-key map [?p] 'dvc-revlist-prev)
    (define-key map [?A] 'dvc-send-commit-notification) ;; Mnemonic: announce
    (define-key map [?E] 'dvc-export-via-email)
    (define-key map "\C-m" 'dvc-revlist-show-item)
    (define-key map [return] 'dvc-revlist-show-item)
    (define-key map [(meta return)] 'dvc-revlist-show-item-scroll-down)
    (define-key map [?=]              'dvc-revlist-diff)
    (define-key map [(meta ?d)]       'dvc-revlist-diff)
    (define-key map [(control ?=)]    'dvc-revlist-diff-to-current-tree)
    (define-key map [(meta ?=)]       'dvc-revlist-diff-scroll-down)
    (define-key map dvc-keyvec-mark   'dvc-revlist-mark-revision)
    (define-key map dvc-keyvec-unmark 'dvc-revlist-unmark-revision)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'dvc-bookmarks)
    (define-key map (dvc-prefix-merge ?u)                     'dvc-revlist-update)
    (define-key map [?h] 'dvc-buffer-pop-to-partner-buffer)
    map))

(easy-menu-define dvc-revlist-mode-menu dvc-revlist-mode-map
  "`dvc-revlist' menu"
  '("DVC-Revlist"
    ["Diff single rev" dvc-revlist-diff t]
    ["Diff with workspace" dvc-revlist-diff-to-current-tree t]
    ["Update to rev at point" dvc-revlist-update t]
    ))

;; dvc-revlist-create-buffer will use "<back-end>-revlist-mode", if
;; defined, instead of this one. If so, it should be derived from
;; dvc-revlist-mode (via `define-derived-mode'), and rely on it for as
;; many features as possible (one can, for example, extend the menu
;; and keymap). See `xmtn-revlist-mode' in xmtn-revlist.el for a good
;; example.
;;
;; Remember to add the new mode to
;; `uniquify-list-buffers-directory-modes' using
;; `dvc-add-uniquify-directory-mode'.
(define-derived-mode dvc-revlist-mode fundamental-mode
  "dvc-revlist"
  "Major mode to show revision list.

Commands are:
\\{dvc-revlist-mode-map}"
  (setq dvc-buffer-current-active-dvc (dvc-current-active-dvc))

  (dvc-install-buffer-menu)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq dvc-revlist-ewoc (ewoc-create #'dvc-revlist-printer))
  (setq buffer-read-only 1)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (set-buffer-modified-p nil))

(dvc-add-uniquify-directory-mode 'dvc-revlist-mode)

(defun dvc-revlist-create-buffer (type location refresh-fn brief last-n)
  "Create (or reuse) and return a buffer to display a revision list.
TYPE must be in dvc-buffer-type-alist.
LOCATION is root or a buffer name, depending on TYPE."
  (let ((dvc-temp-current-active-dvc (dvc-current-active-dvc))
        (buffer (dvc-get-buffer-create dvc-temp-current-active-dvc type location)))
    (with-current-buffer buffer
      (dvc-call "revlist-mode")
      (setq dvc-buffer-refresh-function refresh-fn)
      (setq dvc-revlist-brief brief)
      (setq dvc-revlist-last-n last-n))
    buffer))

(defvar-local dvc-revlist-generator nil
"Buffer-local variable pointing to a function that generates a
list of revisions to display in a revlist buffer. Called with no
args. Result is of the form:
    ((header-lines)
     (footer-lines)
     (revisions))")

(defvar-local dvc-revlist-generator-args nil
"Buffer-local variable containing arguments for `dvc-revlist-generator'.")

(defun dvc-revlist-setup-ewoc (ewoc header footer revs)
  (ewoc-set-hf ewoc header footer)
  (ewoc-filter ewoc (lambda (x) nil))   ; Clear it.
  (when dvc-revlist-last-n
    (let ((len (length revs)))
        (when (> len dvc-revlist-last-n)
	  (setq revs (nthcdr (- len dvc-revlist-last-n) revs)))))
  ;; FIXME: use progress-reporter
  (while revs
    (ewoc-enter-last ewoc (dvc-revlist-entry (pop revs)))
  nil))

(defun dvc-revlist-refresh ()
  "Call`dvc-revlist-generator' with `dvc-revlist-generator-args', display the resulting rev list."
  (let* ((temp (funcall dvc-revlist-generator dvc-revlist-generator-args))
	 (header-lines (nth 0 temp))
	 (footer-lines (nth 1 temp))
	 (revs         (nth 2 temp))
	 (last-n       dvc-revlist-last-n) ;; buffer-local
	 (count (length revs)))
    (dvc-revlist-setup-ewoc
     dvc-revlist-ewoc
     (with-temp-buffer ;; header
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
     (with-temp-buffer ;; footer
       (when footer-lines (insert ?\n))
       (dolist (line footer-lines)
	 (if (null line)
	     (insert ?\n)
	   (insert line ?\n)))
       (buffer-string))
     revs)

    (if (null (ewoc-nth dvc-revlist-ewoc 0))
	(goto-char (point-max))
      (ewoc-goto-node dvc-revlist-ewoc (ewoc-nth dvc-revlist-ewoc 0))))
  nil)

;;;###autoload
(defun dvc-revlist-setup (generator generator-args first-line-only-p last-n)
  "Create a revlist buffer, displaying revisions returned by (GENERATOR GENERATOR-ARGS).
GENERATOR will be called with GENERATOR-ARGS, with the revlist buffer current;
it must return a list (header-lines footer-lines revs) where:
header-lines - list of strings for header
footer-lines - list of strings for footer
revs         - list of backend revids for `dvc-revlist-entry'."
  (let ((buffer (dvc-revlist-create-buffer
                 'log default-directory 'dvc-revlist-refresh first-line-only-p last-n)))
    (with-current-buffer buffer
      (setq dvc-revlist-generator generator)
      (setq dvc-revlist-generator-args generator-args)
      (dvc-revlist-refresh))
    (dvc-switch-to-buffer-maybe buffer)
    ;; return buffer so it can be popped to and/or cleaned up
    buffer))

;; FIXME: delete; use dvc-revlist-setup
(defun dvc-build-revision-list (back-end type location arglist parser
                                         brief last-n path refresh-fn)
  "Runs the back-end BACK-END to build a revision list.

A buffer of type TYPE with location LOCATION is created or reused.

The back-end is launched with the arguments ARGLIST, and the
caller has to provide the function PARSER which will actually
build the revision list.

BRIEF, if non-nil, means show a brief entry for each revision;
nil means show full entry.

LAST-N limits the number of revisions to display; all if nil.

PATH, if non-nil, restricts the log to that file.

REFRESH-FN specifies the function to call when the user wants to
refresh the revision list buffer.  It must take no arguments."
  (let* ((dvc-temp-current-active-dvc back-end)
	 (buffer (dvc-revlist-create-buffer type location refresh-fn brief last-n)))
    (with-current-buffer buffer
      (setq dvc-revlist-path path)
      (setq dvc-revlist-brief brief)
      (setq dvc-revlist-last-n last-n))
    (dvc-switch-to-buffer-maybe buffer t)
    (dvc-run-dvc-async
     back-end arglist
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer output
         (funcall (capture parser) (capture buffer) (capture location))))
     :error
     ;; TODO handle error messages, only treat the bzr missing command
     ;; like this (errorcode=1)
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer output
         (funcall (capture parser) (capture buffer) (capture location)))))))

(provide 'dvc-revlist)
;;; dvc-revlist.el ends here
