;;; dvc-bookmarks.el --- The bookmark system for DVC

;; Copyright (C) 2006-2008 by all contributors

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

;; This file provides a hierachical bookmark system for DVC

;;; History:

;;

;;; Code:
(require 'dvc-core)
(require 'dvc-state)
(require 'ewoc)
(eval-when-compile (require 'cl))

;; this were the settings used for tla
;; ;; Generated file. Do not edit!!!
;; (setq
;; tla-bookmarks-alist
;; '(("dvc"
;;   (local-tree "/home/srei/work/tla/xtla")
;;   (location "stefan@xsteve.at--public-2005" "dvc" "dev" "0" nil)
;;   (timestamp . "Wed Apr 27 10:45:31 2005"))
;;  ("emacs-muse"
;;   (local-tree "/home/srei/work/tla/emacs-muse")
;;   (location "mwolson@gnu.org--2006" "muse" "main" "1.0" nil)
;;   (timestamp . "Fri Dec 10 07:05:56 2004"))))

;; what I want to have:
;; hierachical tree of bookmarks
;; support for different dvc's
;; short name for working copy/branch
;; local-tree
;; timestamp => bookmark-creation-date?
;; different colors
;; optional: dvc: xhg, bzr,...
;; bookmark editing via C-k, C-y (just like in gnus)

;; saved under ~/.dvc/bookmarks.el

;; a data structure for testing purposes
(defvar dvc-bookmark-alist
  '(("hg"
     (local-tree "~/work/hg/hg"))
    ("work-stuff"
     (children
      ("home-dir"
       (local-tree "~/"))
      ("another-dir"
       (local-tree "~/work")))))
  "The bookmarks used for dvc")
;;(pp-to-string dvc-bookmark-alist)

(defvar dvc-bookmarks-file-name "dvc-bookmarks.el" "The file that holds the dvc bookmarks")

(defvar dvc-bookmarks-show-partners t
"If non-nil, display partners.
Must be non-nil for some featurs of dvc-bookmarks to work.")

(defvar dvc-bookmarks-mode-hook '()
  "*Hooks run when entering dvc-bookmarks-mode'.")

(defvar dvc-bookmarks-loaded nil "Whether `dvc-bookmark-alist' has been loaded from `dvc-bookmarks-file-name'.")
(defvar dvc-bookmarks-cookie nil "The ewoc cookie for the *dvc-bookmarks* buffer.")
(defvar dvc-bookmarks-marked-entry nil "A marked bookmark entry for some special operations.")

(defvar dvc-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map [return] 'dvc-bookmarks-goto)
    (define-key map "\C-x\C-f" 'dvc-bookmarks-find-file-in-tree)
    (define-key map "\C-m"   'dvc-bookmarks-goto)
    (define-key map "\C-o"   'dvc-bookmarks-goto-other-window)
    (define-key map "g"      'dvc-bookmarks)
    (define-key map "h"      'dvc-buffer-pop-to-partner-buffer)
    (define-key map "j"      'dvc-bookmarks-jump)
    (define-key map "n"      'dvc-bookmarks-next)
    (define-key map "p"      'dvc-bookmarks-previous)
    (define-key map "a"      'dvc-bookmarks-add)
    (define-key map "e"      'dvc-bookmarks-edit)
    (define-key map "\C-y"   'dvc-bookmarks-yank)
    (define-key map "\C-k"   'dvc-bookmarks-kill)
    (define-key map "s"      'dvc-bookmarks-status)
    (define-key map "d"      'dvc-bookmarks-diff)
    (define-key map "c"      'dvc-bookmarks-log-edit)
    (define-key map "l"      'dvc-bookmarks-changelog)
    (define-key map "L"      'dvc-bookmarks-log)
    (define-key map "Mm"     'dvc-bookmarks-missing)
    (define-key map "Mf"     'dvc-bookmarks-pull)
    (define-key map "Mp"     'dvc-bookmarks-push)
    (define-key map "Mx"     'dvc-bookmarks-merge)
    (define-key map "#"      'dvc-bookmarks-toggle-mark-entry)
    (define-key map "."      'dvc-bookmarks-show-info-at-point)
    (define-key map "\C-x\C-s" 'dvc-bookmarks-save)
    (define-key map "Ap"     'dvc-bookmarks-add-partner)
    (define-key map "Rp"     'dvc-bookmarks-remove-partner)
    (define-key map "Tp"     'dvc-bookmarks-toggle-partner-visibility)
    (define-key map "An"     'dvc-bookmarks-add-nickname)
    (define-key map "Am"     'dvc-bookmarks-add-push-location) ;; mnemonic: Add mirror
    (define-key map "Rm"     'dvc-bookmarks-remove-push-location)
    map)
  "Keymap used in `dvc-bookmarks-mode'.")

(easy-menu-define dvc-bookmarks-mode-menu dvc-bookmarks-mode-map
  "`dvc-bookmarks-mode' menu"
  `("dvc-bookmarks"
    ["Go to working copy" dvc-bookmarks-goto t]
    ["DVC diff" dvc-bookmarks-diff t]
    ["DVC status" dvc-bookmarks-status t]
    ["DVC changelog" dvc-bookmarks-changelog t]
    ["DVC log" dvc-bookmarks-log t]
    ["DVC missing" dvc-bookmarks-missing t]
    ["DVC pull" dvc-bookmarks-pull t]
    ["DVC push" dvc-bookmarks-push t]
    ["DVC merge" dvc-bookmarks-merge t]
   "--"
    ["Add new bookmark" dvc-bookmarks-add t]
    ["Edit current bookmark" dvc-bookmarks-edit t]
    ["Add partner" dvc-bookmarks-add-partner t]
    ["Remove partner" dvc-bookmarks-remove-partner t]
    ["Add/edit partner Nickname" dvc-bookmarks-add-nickname t]
    ["Add Push location" dvc-bookmarks-add-push-location t]
    ["Remove Push location" dvc-bookmarks-remove-push-location t]
    "--"
    ("Toggle visibility"
     ["Partners"    dvc-bookmarks-toggle-partner-visibility
      :style toggle :selected dvc-bookmarks-show-partners])
   "--"
    ["Save bookmarks" dvc-bookmarks-save t]
     ))

;; This data structure represents a single entry in the bookmarks
;; list.  There is one of these associated with each ewoc node.
(defstruct dvc-bookmark
  name                                  ; a string
  indent                                ; an integer
  elem)                                 ; the cdr is an alist

(defun dvc-bookmark-properties (bookmark)
  (cdr (dvc-bookmark-elem bookmark)))

(defsetf dvc-bookmark-properties (bookmark) (val)
  `(setcdr (dvc-bookmark-elem ,bookmark) ,val))

;; This data structure represents a partner of a bookmark.
(defstruct (dvc-bookmark-partner
            (:type list))
  url
  nickname)

(defun dvc-assq-all (key alist)
  "Return an alist containing all associations from ALIST matching KEY."
  (delete nil (mapcar '(lambda (e)
                         (when (and (listp e) (eq (car e) key))
                           e))
                      alist)))

(defun make-dvc-bookmark-from-assoc (assoc indent)
  "Create a `dvc-bookmark' from the association ASSOC.
The indent is taken from INDENT."
  (make-dvc-bookmark
     :name (car assoc)
     :indent indent
     :elem assoc))

(defun dvc-bookmark-key-value (bookmark key)
  "Return the association from the property of BOOKMARK matching KEY."
  (assq key (dvc-bookmark-properties bookmark)))

(defun dvc-bookmark-value (bookmark key)
  "Return the value of the property of BOOKMARK matching KEY."
  (cadr (dvc-bookmark-key-value bookmark key)))

(defun dvc-bookmark-partners (bookmark)
  "Return a list of the partners of BOOKMARK.
Each element is a `dvc-bookmark-partner' structure."
  (mapcar 'cdr
          (dvc-assq-all 'partner (dvc-bookmark-properties bookmark))))

(defun dvc-bookmark-partners-by-url (bookmark)
  "Return an alist of the partners of BOOKMARK.
The car of each association is the URL of the partner and the cdr
is the `dvc-bookmark-partner' itself."
  (mapcar (lambda (p) (cons (dvc-bookmark-partner-url p) p))
          (dvc-bookmark-partners bookmark)))

(defun dvc-bookmark-partner-urls (bookmark)
  "Return a list of the partner urls of BOOKMARK."
  (mapcar 'dvc-bookmark-partner-url (dvc-bookmark-partners bookmark)))

(defun dvc-bookmarks-printer (data)
  (let* ((entry (dvc-bookmark-name data))
         (indent (dvc-bookmark-indent data))
         (partners (and dvc-bookmarks-show-partners
                        (dvc-bookmark-partners data)))
         (nick-name)
         (entry-string (format "%s%s" (make-string indent ? ) entry)))
    ;;(dvc-trace "dvc-bookmarks-printer - data: %S, partners: %S" data partners)
    (when (and dvc-bookmarks-marked-entry (string= dvc-bookmarks-marked-entry entry))
      (setq entry-string (dvc-face-add entry-string 'dvc-marked)))
    (insert entry-string)
    (when partners
      (dolist (p partners)
        (setq nick-name (dvc-bookmark-partner-nickname p))
        (insert (format "\n%sPartner %s%s"
                        (make-string (+ 2 indent) ? )
                        (dvc-bookmark-partner-url p)
                        (if nick-name (format "  [%s]" nick-name) "")))))))

(defun dvc-bookmarks-add-to-cookie (elem indent &optional node)
  (let ((curr (or node (ewoc-locate dvc-bookmarks-cookie)))
        (data (make-dvc-bookmark-from-assoc elem indent))
        (enter-function (if (eq (dvc-line-number-at-pos) 1) 'ewoc-enter-before 'ewoc-enter-after)))
    (cond ((assoc 'children elem)
           (setq node
                 (if curr
                     (apply enter-function (list dvc-bookmarks-cookie curr data))
                   (let ((n (ewoc-enter-last dvc-bookmarks-cookie data)))
                     (forward-line 1)
                     n)))
           (dolist (child (reverse (cdr (assoc 'children elem))))
             (dvc-bookmarks-add-to-cookie child (+ indent 2) node)))
          (t
           (if curr
               (apply enter-function (list dvc-bookmarks-cookie curr data))
             (ewoc-enter-last dvc-bookmarks-cookie data))))
    (forward-line 1)))

;;;###autoload
(defun dvc-bookmarks (&optional arg)
  "Display the *dvc-bookmarks* buffer.
With prefix argument ARG, reload the bookmarks file from disk."
  (interactive "P")
  (dvc-bookmarks-load-from-file arg)
  (dvc-switch-to-buffer (get-buffer-create "*dvc-bookmarks*"))
  (let ((cur-pos (point)))
    (toggle-read-only 0)
    (erase-buffer)
    (set (make-local-variable 'dvc-bookmarks-cookie)
         (ewoc-create (dvc-ewoc-create-api-select
                       #'dvc-bookmarks-printer)))
    (put 'dvc-bookmarks-cookie 'permanent-local t)
    (dolist (entry dvc-bookmark-alist)
      (dvc-bookmarks-add-to-cookie entry 0))
    (if (eq major-mode 'dvc-bookmarks-mode)
        (goto-char cur-pos)
      (goto-char (point-min))))
  (dvc-bookmarks-mode))

(defun dvc-bookmarks-mode ()
  "Mode to display DVC bookmarks.

\\{dvc-bookmarks-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dvc-bookmarks-mode-map)
  (setq major-mode 'dvc-bookmarks-mode)
  (setq mode-name "dvc-bookmarks")
  (toggle-read-only 1)
  (run-hooks 'dvc-bookmarks-mode-hook))

(defun dvc-bookmarks-show-info-at-point ()
  (interactive)
  (message "%S" (dvc-bookmarks-current-bookmark)))

(defun dvc-bookmarks-current-bookmark ()
  (ewoc-data (ewoc-locate dvc-bookmarks-cookie)))

(defun dvc-bookmarks-invalidate-current-bookmark ()
  "Regenerate the text for the bookmark under point."
  (ewoc-invalidate dvc-bookmarks-cookie (ewoc-locate dvc-bookmarks-cookie)))

(defun dvc-bookmarks-current-value (key)
  (dvc-bookmark-value (dvc-bookmarks-current-bookmark) key))

(defun dvc-bookmarks-current-key-value (key)
  (dvc-bookmark-key-value (dvc-bookmarks-current-bookmark) key))

(defun dvc-bookmarks-marked-bookmark ()
  (when dvc-bookmarks-marked-entry
    (save-excursion
      (dvc-bookmark-goto-name dvc-bookmarks-marked-entry)
      (dvc-bookmarks-current-bookmark))))

(defun dvc-bookmarks-marked-value (key)
  (let ((marked-bookmark (dvc-bookmarks-marked-bookmark)))
    (when marked-bookmark
      (dvc-bookmark-value marked-bookmark key))))

(defun dvc-bookmarks-add (bookmark-name bookmark-local-dir)
  "Add a DVC bookmark named BOOKMARK-NAME, directory BOOKMARK-LOCAL-DIR."
  (interactive
   (let* ((bmk-name (read-string "DVC bookmark name: "))
          (bmk-loc (dvc-read-directory-name (format "DVC bookmark %s directory: " bmk-name))))
     (list bmk-name bmk-loc)))
  (let* ((elem (list bookmark-name (list 'local-tree bookmark-local-dir)))
         (data (make-dvc-bookmark-from-assoc elem 0)))
    (dvc-bookmarks)
    (add-to-list 'dvc-bookmark-alist elem t)
    (ewoc-enter-last dvc-bookmarks-cookie data)))

(defun dvc-bookmarks-edit (bookmark-name bookmark-local-dir)
  "Change the current DVC bookmark's BOOKMARK-NAME and/or LOCAL-DIR."
  (interactive
   (let* ((old-name (dvc-bookmark-name (dvc-bookmarks-current-bookmark)))
          (old-local-tree (dvc-bookmarks-current-value 'local-tree))
          (bmk-name (read-string "DVC bookmark name: " old-name))
          (bmk-loc (dvc-read-directory-name
                    (format "DVC bookmark %s directory: " bmk-name)
                    old-local-tree)))
     (list bmk-name bmk-loc)))
  (let* ((node (ewoc-locate dvc-bookmarks-cookie))
         (old-data (ewoc-data node))
         (old-indent (dvc-bookmark-indent old-data))
         (elem (dvc-bookmark-elem old-data)))
    (setcar elem bookmark-name)
    (setcdr elem (cons (list 'local-tree bookmark-local-dir)
                               (assq-delete-all 'local-tree (cdr elem))))
    (ewoc-set-data node (make-dvc-bookmark-from-assoc elem old-indent))
    (ewoc-invalidate dvc-bookmarks-cookie node)))

(defun dvc-bookmarks-next ()
  (interactive)
  (forward-line 1))

(defun dvc-bookmarks-previous ()
  (interactive)
  (forward-line -1))

(defun dvc-bookmarks-goto ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (find-file local-tree)
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-goto-other-window ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (find-file-other-window local-tree)
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-find-file-in-tree ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory (file-name-as-directory local-tree)))
          (find-file (read-file-name "Find file in bookmarked tree: ")))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-status ()
  "Run `dvc-status' for bookmark at point."
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (dvc-status local-tree)
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-diff ()
  "Run `dvc-diff' for bookmark at point."
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (dvc-diff nil local-tree)
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-log-edit ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-log-edit))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-changelog ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-changelog))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-log ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-log))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-missing ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree)
              (partner (or (dvc-bookmarks-partner-at-point) (dvc-bookmarks-marked-value 'local-tree))))
          (message "Running dvc missing for %s, against %s"
                   (dvc-bookmark-name (dvc-bookmarks-current-bookmark))
                   partner)
          (dvc-missing partner))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-pull ()
  "Pull from partner at point or default into current bookmark."
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree)
              (partner (dvc-bookmarks-partner-at-point))
              (nickname (dvc-bookmarks-nickname-at-point)))
          (message (if partner
                       (if nickname
                           (format "Pulling from %s, using URL %s" nickname partner)
                         (format "Pulling from %s" partner))
                     "Pulling from default location"))
          (dvc-pull partner))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-push ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-push))
      (message "No local-tree defined for this bookmark entry."))))

(defvar dvc-bookmarks-merge-template "Merged from %s: ")
(defun dvc-bookmarks-merge ()
  "Merge from partner at point into current bookmark."
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree)
              (partner (dvc-bookmarks-partner-at-point))
              (nickname (dvc-bookmarks-nickname-at-point)))
          (setq dvc-memorized-log-header (when nickname (format dvc-bookmarks-merge-template nickname)))
          (setq dvc-memorized-log-message nil)
          (message (if nickname
                       (format "Merging from %s, using URL %s" nickname partner)
                     (format "Merging from %s" partner)))
          (dvc-merge partner))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-yank ()
  (interactive)
  (let ((indent (save-excursion (if (eq (line-beginning-position) (line-end-position))
                                    0
                                  (forward-line 1)
                                  (dvc-bookmark-indent (dvc-bookmarks-current-bookmark))))))
    (dvc-bookmarks-add-to-cookie
     (cons (dvc-bookmark-name dvc-bookmarks-tmp-yank-item)
           (dvc-bookmark-properties dvc-bookmarks-tmp-yank-item))
     indent)))

(defvar dvc-bookmarks-tmp-yank-item '("hg" (local-tree "~/work/hg/hg")))
(defun dvc-bookmarks-kill ()
  (interactive)
  (setq dvc-bookmarks-tmp-yank-item (dvc-bookmarks-current-bookmark))
  (let ((buffer-read-only nil))
    (dvc-ewoc-delete dvc-bookmarks-cookie (ewoc-locate dvc-bookmarks-cookie))))

(defun dvc-bookmarks-toggle-mark-entry ()
  "Mark the current bookmark entry."
  (interactive)
  (let* ((cur-data (dvc-bookmarks-current-bookmark))
         (bmk-name (dvc-bookmark-name cur-data))
         (has-children (dvc-bookmarks-current-value 'children)))
    ;; (message "bmk-name: %s has-children: %s" bmk-name has-children)
    (unless has-children
      (if (string= bmk-name dvc-bookmarks-marked-entry)
          (progn
            (message "Unmarking bookmark entry %s" bmk-name)
            (setq dvc-bookmarks-marked-entry nil))
        (message "Marking bookmark entry %s" bmk-name)
        (setq dvc-bookmarks-marked-entry bmk-name))
      (dvc-bookmarks))))

(defun dvc-bookmarks-save ()
  "Save `dvc-bookmark-alist' to the file `dvc-bookmarks-file-name'."
  (interactive)
  (dvc-save-state '(dvc-bookmark-alist)
                  (dvc-config-file-full-path dvc-bookmarks-file-name t)
                  t))

(defun dvc-bookmarks-load-from-file (&optional force)
  "Load bookmarks from the file `dvc-bookmarks-file-name'.

If FORCE is non-nil, reload the file even if it was loaded before."
  (when (or force (not dvc-bookmarks-loaded))
    (dvc-load-state (dvc-config-file-full-path
                     dvc-bookmarks-file-name t))
    (setq dvc-bookmarks-loaded t)))

(defun dvc-bookmark-name-1 (entry &optional parent-name)
  (cond ((assoc 'children entry)
         (let ((names))
           (dolist (child (cdr (assoc 'children entry)))
             (add-to-list 'names (car (dvc-bookmark-name-1 child (car entry)))))
           names))
        (t
         (list (concat (if parent-name (concat  parent-name "/") "") (car entry))))))

(defun dvc-bookmark-names ()
  "Return a list with all dvc bookmark names."
  (let ((names))
    (dolist (entry dvc-bookmark-alist)
      (setq names (append names (dvc-bookmark-name-1 entry))))
    names))

(defun dvc-bookmark-local-tree-mapping-1 (entry)
  (cond ((assoc 'children entry)
         (let ((tree-mapping))
           (dolist (child (cdr (assoc 'children entry)))
             (add-to-list 'tree-mapping (car (dvc-bookmark-local-tree-mapping-1 child))))
           tree-mapping))
        (t
         (list (list (dvc-uniquify-file-name (cadr (assoc 'local-tree (cdr entry)))) (car entry))))))

;; (dvc-bookmark-local-tree-mapping)

(defun dvc-bookmark-local-tree-mapping ()
  "Return an alist that maps from working copies to bookmark names."
  (let ((tree-mapping))
    (dolist (entry dvc-bookmark-alist)
      (setq tree-mapping (append tree-mapping (dvc-bookmark-local-tree-mapping-1 entry))))
    tree-mapping))


(defun dvc-bookmark-goto-name (name)
  (let ((cur-pos (point))
        (name-list (split-string name "/"))
        (prefix ""))
    (goto-char (point-min))
    (dolist (name name-list)
      (setq name (concat prefix name))
      (setq prefix (concat "  " prefix))
      (search-forward name))
    (beginning-of-line-text)))

(defun dvc-bookmarks-jump ()
  (interactive)
  (dvc-bookmark-goto-name (dvc-completing-read "Jump to dvc bookmark: "
                                               (dvc-bookmark-names))))

(defun dvc-bookmarks-get-partner-urls ()
  (dvc-bookmark-partner-urls (dvc-bookmarks-current-bookmark)))

(defun dvc-bookmarks-add-partner ()
  (interactive)
  (let* ((cur-data (dvc-bookmarks-current-bookmark))
         (partner-url (read-string (format "Add partner to '%s': "
                                           (dvc-bookmark-name cur-data)))))
    (if (not (member partner-url (dvc-bookmarks-get-partner-urls)))
        (progn
          (setf (dvc-bookmark-properties cur-data)
                (append (dvc-bookmark-properties cur-data)
                        (list (cons 'partner
                                    (make-dvc-bookmark-partner :url partner-url)))))
          (dvc-trace "dvc-bookmarks-add-partner %s" cur-data)
          (dvc-bookmarks-invalidate-current-bookmark))
      (message "%s is already a partner for %s"
               partner-url (dvc-bookmark-name cur-data)))))

(defun dvc-bookmarks-remove-partner ()
  (interactive)
  (let* ((cur-data (dvc-bookmarks-current-bookmark))
         (partners-alist (dvc-bookmark-partners-by-url cur-data))
         (partner-to-remove (dvc-completing-read
                             (format "Remove partner from %s: "
                                     (dvc-bookmark-name cur-data))
                             (mapcar 'car partners-alist)
                             nil t nil nil
                             (dvc-bookmarks-partner-at-point))))
    (setf (dvc-bookmark-properties cur-data)
          (delete (cons 'partner (cdr (assoc partner-to-remove partners-alist)))
                  (dvc-bookmark-properties cur-data)))
    (dvc-bookmarks-invalidate-current-bookmark)))

(defun dvc-bookmarks-toggle-partner-visibility ()
  (interactive)
  (setq dvc-bookmarks-show-partners (not dvc-bookmarks-show-partners))
  (dvc-bookmarks))

(defun dvc-bookmarks-partner-at-point ()
  (save-excursion
    (let ((partner-url))
      (goto-char (line-beginning-position))
      (when (looking-at "  +Partner \\(.+?\\)\\(  \\[.+\\)?$")
        (setq partner-url (match-string 1)))
      partner-url)))

(defun dvc-bookmarks-nickname-at-point ()
  (save-excursion
    (let ((nickname))
      (goto-char (line-beginning-position))
      (when (looking-at "  +Partner \\(.+?\\)  \\[\\(.+\\)?\\]$")
        (setq nickname (match-string 2)))
      nickname)))

(defun dvc-bookmarks-add-nickname ()
  (interactive)
  ;;(message "dvc-bookmarks-add-nickname %S" (dvc-bookmarks-current-bookmark))
  (let* ((url-at-point (dvc-bookmarks-partner-at-point))
         (bookmark (dvc-bookmarks-current-bookmark))
         (partner (cdr (assoc url-at-point
                              (dvc-bookmark-partners-by-url bookmark)))))
    (if partner
      (progn
        (setf (dvc-bookmark-partner-nickname partner)
              (read-string (format "Nickname for %s: " url-at-point)
                           (dvc-bookmark-partner-nickname partner)))
        (dvc-bookmarks-invalidate-current-bookmark)
        (message "Added nickname %s to the partner %s"
                 (dvc-bookmark-partner-nickname partner) url-at-point))
      (error "No partner with URL '%s'" url-at-point))))

(defun dvc-bookmarks-add-push-location ()
  (interactive)
  (let* ((push-locations (dvc-bookmarks-current-value 'push-locations))
         (cur-data (dvc-bookmarks-current-bookmark))
         (push-location (read-string (format "Add push location to '%s': " (dvc-bookmark-name cur-data)))))
    (if (not (member push-location push-locations))
        (progn
          (if (null push-locations)
              (progn
                (setq push-locations (list 'push-locations (list push-location)))
                (setf (dvc-bookmark-properties cur-data)
                      (append (dvc-bookmark-properties cur-data)
                              (list push-locations))))
            (setcdr push-locations (append (cdr push-locations)
                                           (list push-location)))))
      (message "%s is already a push-location for %s"
               push-location (dvc-bookmark-name cur-data)))))

(defun dvc-bookmarks-remove-push-location ()
  (interactive)
  (let* ((push-locations (dvc-bookmarks-current-key-value 'push-locations))
         (cur-data (dvc-bookmarks-current-bookmark))
         (location-to-remove (dvc-completing-read "Remove push location: " (cadr push-locations)))
         (new-push-locations (delete location-to-remove (cadr push-locations))))
    (if new-push-locations
        (setcdr push-locations (list new-push-locations))
      (setf (dvc-bookmark-properties cur-data)
            (delete push-locations (dvc-bookmark-properties cur-data))))))

;;;###autoload
(defun dvc-bookmarks-current-push-locations ()
  (let* ((tree-mapping (dvc-bookmark-local-tree-mapping))
         (bookmark-name (cadr (assoc (dvc-tree-root) tree-mapping)))
         (push-locations))
    (when bookmark-name
      (save-window-excursion
        (with-current-buffer "*dvc-bookmarks*"
          (dvc-bookmark-goto-name bookmark-name)
          (setq push-locations (dvc-bookmarks-current-value 'push-locations)))))
    ;;(message "bookmark-name: %s -> push-locations: %S" bookmark-name push-locations)
    push-locations))


;; (dvc-bookmarks-load-from-file t)

(provide 'dvc-bookmarks)
;;; dvc-bookmarks.el ends here
