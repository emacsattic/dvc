;;; dvc-register.el --- Registration of DVC back-ends

;; Copyright (C) 2005-2006 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from: Matthieu Moy <Matthieu.Moy@imag.fr>

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

;; DVC Back-end registration

(require 'dvc-defs)
(require 'dvc-utils)

(defvar dvc-registered-backends nil
  "List of registered back-ends.")

(defun dvc-intern-symbol-name (dvc postfix)
  "Intern a symbol for DVC, add POSTFIX to the name.
A '-' is put between DVC and the POSTFIX.

Example: (dvc-intern-symbol-name 'xhg \"tree-root\") => xhg-tree-root"
  (intern (concat (symbol-name dvc) "-" postfix)))

(defmacro dvc-register-dvc (dvc name)
  "Register DVC, NAME is displayed for user interaction.

It's a macro, so it can be called without loading dvc-unified. The
build system inserts a (eval-when-compile (require 'dvc-unified))
at the beginning of the autoload file, so, the macro is available in
the autoloads."
  `(progn
     (defvar dvc-registered-backends nil)
     (add-to-list 'dvc-registered-backends ,dvc)
     (defvar ,(intern (concat (symbol-name (cadr dvc))
                              "-backend-name"))
       ,name
       ,(concat "Human friendly name used for the dvc backend '"
                (symbol-name (cadr dvc))
                ".\nThis variable was created by `dvc-register-dvc'"))))

(defvar dvc-backend-name "Unknown")

(defun dvc-function (dvc postfix &optional nodefault)
  "Return the function for DVC backend concatenated with POSTFIX.

To be used with `apply' or `funcall'. If NODEFAULT is nil and no
function is available for this backend, use dvc-<postfix>
instead.

POSTFIX is a string."
  (let ((res (dvc-intern-symbol-name dvc postfix)))
    (if (or nodefault (fboundp res)) res
      (let ((dvc-register-sym (intern (concat (symbol-name dvc) "-dvc"))))
        (unless (featurep dvc-register-sym)
          (dvc-trace "require %S" dvc-register-sym)
          (if (featurep 'xemacs)
              (require dvc-register-sym nil)
            (require dvc-register-sym nil t))))
      (let ((second-try (dvc-function dvc postfix t)))
        (if (fboundp second-try) second-try
          (let ((fall-back (dvc-intern-symbol-name 'dvc postfix)))
            (if (not fall-back) second-try
              ;;(dvc-trace "dvc-function: fall back to DVC for %s %s" dvc postfix)
              (dvc-intern-symbol-name 'dvc postfix))))))))

(defun dvc-variable (dvc postfix &optional nodefault)
  "Get the value of a variable in a DVC backend.

If NODEFAULT is nil and no variable is available for this
backend, use dvc-<prefix> instead."
  (let ((res (dvc-intern-symbol-name dvc postfix)))
    (if (or nodefault (boundp res)) (eval res)
      (let ((dvc-register-sym (intern (concat (symbol-name dvc) "-dvc"))))
        (unless (featurep dvc-register-sym)
          (dvc-trace "require %S" dvc-register-sym)
          (if (featurep 'xemacs)
              (require dvc-register-sym nil)
            (require dvc-register-sym nil t))))
      (let ((second-try (dvc-variable dvc postfix t)))
        second-try))))

;;;###autoload
(defun dvc-apply (postfix &rest args)
  "Apply ARGS to the `dvc-current-active-dvc' concated with POSTFIX."
  (let ((current-dvc (dvc-current-active-dvc)))
    (if current-dvc
        (apply (dvc-function current-dvc postfix) args)
      (let ((default-directory
              (dvc-read-directory-name "Local tree: ")))
        (apply 'dvc-apply postfix args)))))


(defvar dvc-current-active-dvc-cache (make-hash-table :test 'equal)
  "A cache that contains directories as keys and the DVC symbol as value.
That value is considered first in `dvc-current-active-dvc'.")

(defvar dvc-buffer-current-active-dvc nil
  "Tell DVC which back-end to use in some buffers.

Overrides the search for a control directory in `dvc-current-active-dvc'.")
(make-variable-buffer-local 'dvc-buffer-current-active-dvc)

(defvar dvc-temp-current-active-dvc nil
  "Tell DVC which back-end to use temporarily.

Overrides the search for a control directory in
`dvc-current-active-dvc'. This is meant to be set in a let statement.")

(defun dvc-current-active-dvc (&optional nocache)
  "Get the currently active dvc for the current `default-directory'.

Currently supported dvc's can be found in `dvc-registered-backends'.
`dvc-select-priority' specifies the priority, if more than one
backend is in use for the `default-directory'.
The values are cached in `dvc-current-active-dvc-cache'.

If NOCACHE is provided, ignore the cache for this call, but still
cache the result (usefull to correct an incorrect cache entry)."
  (interactive "p")
  (or dvc-buffer-current-active-dvc
      dvc-temp-current-active-dvc
      (let ((dvc (gethash (dvc-uniquify-file-name default-directory) dvc-current-active-dvc-cache)))
        (unless dvc
          (let ((dvc-list (append dvc-select-priority dvc-registered-backends))
                (root "/")
                (tree-root-func))
            (while dvc-list
              (setq tree-root-func (dvc-function (car dvc-list) "tree-root" t))
              (when (fboundp tree-root-func)
                (let ((current-root (funcall tree-root-func nil t)))
                  (when (and current-root (> (length current-root) (length root)))
                    (setq root current-root)
                    (setq dvc (car dvc-list)))))
              (setq dvc-list (cdr dvc-list)))
            (puthash (dvc-uniquify-file-name default-directory)
                     dvc dvc-current-active-dvc-cache))) ;cache the found dvc
        (when (interactive-p)
          (message "DVC: using %s for %s" dvc default-directory))
        dvc)))

(defun dvc-select-dvc (directory dvc)
  "Select the DVC to use for DIRECTORY.
The given value is stored in `dvc-current-active-dvc-cache'."
  (interactive (list (dvc-uniquify-file-name
                      (dvc-read-directory-name "Set dvc for path: " nil nil t))
                     (intern (completing-read "dvc: "
                                              (map t 'symbol-name
                                                   (append '(None) dvc-registered-backends))))))
  (when (eq dvc 'None)
    (message "Removing %s from dvc-current-active-dvc-cache" directory)
    (setq dvc nil))
  (puthash directory dvc dvc-current-active-dvc-cache))

(provide 'dvc-register)
;;; dvc-register.el ends here
