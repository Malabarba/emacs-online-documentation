;;; full-feature-lister.el --- Parse load-path and list all features available for requiring.

;; Copyright (C) 2013 Artur Malabarba <emacs@endlessparentheses.com>

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: http://github.com/Malabarba/full-feature-lister
;; Version: 0.1
;; Keywords: convenience
;; Separator: //

;;; Commentary:
;;
;; 

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 0.1 - 20130811 - Created File.
;;; Code:

(require 'finder)
(require 'cl)

(defun docgen//update-hash ()
  ""
  (setq package--builtins nil)
  (setq finder-keywords-hash (make-hash-table :test 'eq))
  (let ((el-file-regexp "^\\([^=].*\\)\\.el\\(\\.\\(gz\\|Z\\)\\)?$")
        (load-path (remove-if (lambda (p) (string-match "lisp/obsolete$" p))
                              load-path))
	package-override files base-name processed
	summary keywords package version entry desc)
    (dolist (d load-path)
      (when (file-exists-p (directory-file-name d))
	(message "Directory %s" d)
	(setq package-override
	      (intern-soft
	       (cdr-safe
		(assoc (file-name-nondirectory (directory-file-name d))
		       finder--builtins-alist))))
	(setq files (directory-files d nil el-file-regexp))
	(dolist (f files)
	  (unless (or (string-match finder-no-scan-regexp f)
		      (null (setq base-name
				  (and (string-match el-file-regexp f)
				       (intern (match-string 1 f)))))
		      (memq base-name processed))
	    (push base-name processed)
	    (with-temp-buffer
	      (insert-file-contents (expand-file-name f d))
	      (setq summary  (lm-synopsis)
		    keywords (mapcar 'intern (lm-keywords-list))
		    package  (or package-override
				 (let ((str (lm-header "package")))
				   (if str (intern str)))
				 base-name)
		    version  (lm-header "version")))
	    (when summary
	      (setq version (ignore-errors (version-to-list version)))
	      (setq entry (assq package package--builtins))
	      (cond ((null entry)
		     (push (cons package (vector version nil summary))
			   package--builtins))
		    ((eq base-name package)
		     (setq desc (cdr entry))
		     (aset desc 0 version)
		     (aset desc 2 summary)))
	      (dolist (kw keywords)
		(puthash kw
			 (cons package
			       (delq package
				     (gethash kw finder-keywords-hash)))
			 finder-keywords-hash))))))))

  (setq package--builtins
	(sort package--builtins
	      (lambda (a b) (string< (symbol-name (car a))
				     (symbol-name (car b)))))))
;;;###autoload
(defun docgen//full-feature-lister (&optional rescan include-loaded sort)
  "List all features available for requiring.

From lisp code, the list is returned (sorted if SORT is non-nil).
Interactively the list is sorted and inserted at point.

The `finder-keywords-hash' hashtable usually needs to be updated
the first time this is run for each Emacs session (which might
take a few minutes). To perform a rescan provide the RESCAN
prefix argument. If you don't, you'll just get the built-in
features (which might be what you want).

Normally returns only features from `load-path'.
With INCLUDE-LOADED non-nil, combines features available in
`load-path' with those already loaded (listed in `features')."
  (interactive "P")
  (when rescan (docgen//update-hash))
  (let ((out (if include-loaded features nil)))
    (maphash (lambda (k v) (setq out (append out v)))
             finder-keywords-hash)
    (setq out (delete-duplicates out))
    (when sort (setq out (sort out 'string<)))
    (if (called-interactively-p 'interactive)
        (insert (format "%S" (sort out 'string<)))
      out)))

