;;; xmtn-sync.el --- database sync handling for DVC backend for monotone
;;
;; Copyright (C) 2010 - 2014 Stephen Leake
;;
;; Author: Stephen Leake
;; Keywords: tools
;;
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

(eval-when-compile (require 'cl-macs))

(eval-and-compile
  ;; these have functions and macros we use
  (require 'xmtn-automate)
  (require 'xmtn-basic-io)
  )

(require 'comint)
(require 'dvc-config)

(defconst xmtn-sync-basic-io-file "sync.basic_io"
  "File to save shell sync basic_io output for input by `xmtn-sync-review'; relative to `dvc-config-directory'.")

(defconst xmtn-sync-required-command-version '(1 0)
  ;; Sometimes the Cygwin version lags behind the MinGW version; this allows that.
  "Minimum version for `xmtn-sync-executable'; overrides xmtn--minimum-required-command-version.
Must support file:, ssh:, automate sync.")

(defun xmtn-sync-parse-revision-certs (direction)
  "Parse certs associated with a revision; return (branch changelog date author)."
  (let ((keyword (ecase direction
		   ('receive "receive_cert")
		   ('send    "send_cert")))
	cert-label branch date author changelog old-branch)
    (while (xmtn-basic-io-optional-line keyword (setq cert-label (cadar value)))
      (cond
       ((string= cert-label "branch")
	(xmtn-basic-io-check-line "value" (setq branch (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "revision"))

       ((string= cert-label "changelog")
	(xmtn-basic-io-check-line "value" (setq changelog (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "revision"))

       ((string= cert-label "date")
	(xmtn-basic-io-check-line "value" (setq date (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "revision"))

       ((string= cert-label "author")
	(xmtn-basic-io-check-line "value" (setq author (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-skip-line "revision"))

       (t
	;; ignore other certs
	(xmtn-basic-io-skip-stanza))
       )
      (xmtn-basic-io-skip-blank-lines) ;; might be at end of parsing region
      ) ;; end while cert

    (list branch changelog date author)))

(defun xmtn-sync-parse-revisions (revs direction)
  "Parse revisions with associated certs, add to REVS."
  (let ((keyword (ecase direction
		   ('receive "receive_revision")
		   ('send    "send_revision")))
	revid)
    (while (xmtn-basic-io-optional-line keyword (setq revid (cadar value)))
      (xmtn-basic-io-skip-blank-lines)
      (let* ((cert-values (xmtn-sync-parse-revision-certs direction))
	     (branch (nth 0 cert-values))
	     (changelog (nth 1 cert-values))
	     (date (nth 2 cert-values))
	     (author (nth 3 cert-values)))

	;; mtn branch names are assumed to be globally unique,
	;; (because they can exist in several repositories), so we
	;; don't need a repo arg here.
	;;
	;; mtn can run sync without a workspace.
	(setq revs (dvc-sync-enter-rev revs nil nil branch revid date author changelog direction))
	)))
  revs)

(defun xmtn-sync-parse-certs (revs direction)
  "Parse certs not associated with revisions, add to REVS.
Return new revs"
  (let ((keyword (ecase direction
		   ('receive "receive_cert")
		   ('send    "send_cert")))
	revid
	cert-label
	branch
	(date "")
	(author "")
	(changelog "create or propagate branch\n")
	old-branch)

    (while (xmtn-basic-io-optional-line keyword (setq cert-label (cadar value)))
      (cond
       ((string= cert-label "branch")
	;; This happens when a new branch is created, or a branch is
	;; propagated without any conflicts.
	(xmtn-basic-io-check-line "value" (setq branch (cadar value)))
	(xmtn-basic-io-skip-line "key")
	(xmtn-basic-io-check-line "revision" (setq revid (cadar value)))

	;; mtn branch names are assumed to be globally unique,
	;; (because they can exist in several repositories), so we
	;; don't need a repo arg here.
	;;
	;; mtn can run sync without a workspace.
	(setq revs (dvc-sync-enter-rev revs nil nil branch revid date author changelog direction)))

       (t
	;; ignore other certs
	(xmtn-basic-io-skip-stanza))
       )

      ;; move to next stanza or end of parsing region
      (xmtn-basic-io-skip-blank-lines)

      ))
  revs)

(defun xmtn-sync-parse-keys (direction)
  ;; just ignore all keys
  (let ((keyword (ecase direction
		   ('receive "receive_key")
		   ('send    "send_key"))))
    (xmtn-basic-io-skip-blank-lines)
    (while (xmtn-basic-io-optional-skip-line keyword))))

(defun xmtn-sync-parse (revs)
  "Parse output of several sync command in current buffer, add to REVS.
Return new revs."
  (set-syntax-table xmtn-basic-io--*syntax-table*)
  (goto-char (point-min))

  ;; receive_cert "branch"
  ;;        value "foo2"
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [e4352c1d28b38e87b5040f770a66be2ec9b2362d]
  ;;
  ;; ... more unattached certs
  ;;
  ;; receive_revision [e4352c1d28b38e87b5040f770a66be2ec9b2362d]
  ;;
  ;; receive_cert "branch"
  ;;        value "foo2"
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [...]
  ;;
  ;; receive_cert "changelog"
  ;;        value "more
  ;; "
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [...]
  ;;
  ;; receive_cert "date"
  ;;        value "2010-09-21T08:29:11"
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [...]
  ;;
  ;; receive_cert "author"
  ;;        value "tester@test.net"
  ;;          key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;;     revision [...]
  ;;
  ;;     ... more certs
  ;;
  ;; ... more revisions with certs
  ;;
  ;; receive_key
  ;;
  ;; key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;; key [46ec58576f9e4f34a9eede521422aa5fd299dc50]
  ;; ... more keys
  ;;
  ;; send_cert ... (unattached)
  ;;
  ;; send_revision [...]
  ;;    send_cert ...
  ;;
  ;; send_key ...

  (while (not (eobp))
    (setq revs (xmtn-sync-parse-certs revs 'receive))
    (setq revs (xmtn-sync-parse-revisions revs 'receive))
    (xmtn-sync-parse-keys 'receive)
    (setq revs (xmtn-sync-parse-certs revs 'send))
    (setq revs (xmtn-sync-parse-revisions revs 'send))
    (xmtn-sync-parse-keys 'send)
    )
  revs)

(defconst xmtn-shell-buffer-name "*xmtn*")

(defun xmtn-shell-send-string (cmd)
  "Send CMD to a shell, display shell buffer."
  (let ((work default-directory)
	(proc (get-buffer-process xmtn-shell-buffer-name)))
    (if proc
	;; display buffer
	(shell (get-buffer xmtn-shell-buffer-name))

      ;; create and display buffer
      (shell (get-buffer-create xmtn-shell-buffer-name))
      (setq proc (get-buffer-process xmtn-shell-buffer-name))
      (set-process-query-on-exit-flag proc nil))

    (comint-send-string proc (concat "cd " work "\n"))
    (comint-send-string proc (concat cmd "\n")))
  )

(defun xmtn-sync-shell-cmd (cmd)
  "Run CMD (a mtn automate sync command) in a shell."
  ;; The ticker display works much better in a shell buffer.
  (xmtn-shell-send-string
   (concat xmtn-executable " " xmtn-automate-arguments
	   " automate " cmd " --ticker=count >> "
	   (expand-file-name xmtn-sync-basic-io-file dvc-config-directory)))
  )

;;;###autoload
(defun xmtn-dvc-sync-run ()
  "For `dvc-sync-run'."
  (xmtn-sync-shell-cmd "sync"))

;;;###autoload
(defun xmtn-dvc-pull (&optional other)
  "For `dvc-pull'."
  (xmtn-sync-shell-cmd "pull"))

;;;###autoload
(defun xmtn-dvc-push (&optional other)
  "For `dvc-push'."
  (xmtn-sync-shell-cmd "push"))


;;;###autoload
(defun xmtn-dvc-parse-sync ()
  "For `dvc-parse-sync'."
  (let ((basic-io-file (expand-file-name xmtn-sync-basic-io-file dvc-config-directory))
	(dvc-file (expand-file-name dvc-sync-save-file dvc-config-directory))
	stuff)

    (when (file-exists-p basic-io-file)
      (dvc-load-state dvc-file)

      (with-temp-buffer
	(insert-file-contents-literally basic-io-file)
	(setq stuff (xmtn-sync-parse stuff)))

      (dvc-save-state (list 'stuff) dvc-file)
      (delete-file basic-io-file))))

(provide 'xmtn-sync)

;; end of file
