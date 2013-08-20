;;; documentation-generator.el --- Convert the documentation of all built-in functions and variables to html files.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: https://github.com/Bruce-Connor/emacs-online-documentation/
;; Version: 0.1
;; Keywords: 
;; ShortName: dg
;; Separator: /

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
;; 0.1 - 20130813 - Uploaded the file.
;; 0.1 - 20130811 - Created File.
;;; Code:

(eval-when-compile (require 'cl))
(require 'cl)
(load "full-feature-lister")

(defconst dg/version "0.1" "Version of the documentation-generator.el package.")
(defconst dg/version-int 1 "Version of the documentation-generator.el package, as an integer.")
(defun dg/bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs and dg versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/emacs-online-documentation/issues/new")
  (message "Your dg/version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           dg/version emacs-version))

(defcustom dg/symbol-list '(documentation-generator dg/cons-to-item dg/cons-list-to-item-list dg/file-list-function dg/file-list-variable dg/my-unique-var dg/variable-list dg/-list-all dg/function-list dg/file-list dg/doc-to-html dg/clean-symbol dg/format dg/symbol-to-file dg/description dg/dir dg/symbol-list dg/customize dg/bug-report dg/version-int dg/version dg/convert dg/predicate dg/verbose dg/count dg/total dg/url-string ffl/update-hash full-feature-lister)
  "This a list of all symbols we define here. It is used to filter them out from the results since these are not built-in."
  :type '(repeat symbol)
  :group 'documentation-generator
  :package-version '(documentation-generator . "0.1"))

(defcustom dg/dir (expand-file-name "~/Git-Projects/online-documentation-pages/")
  ""
  :type 'directory
  :group 'documentation-generator
  :package-version '(documentation-generator . "0.1"))

(defvar dg/description nil "The description function used.")
(defvar dg/count 0 "Used for reporting the progress of the conversion.")
(defvar dg/total 0 "Used for reporting the progress of the conversion.")
(defcustom dg/verbose t "Whether we should report the progress of the conversion."
  :type 'boolean
  :group 'documentation-generator
  :package-version '(documentation-generator . "0.1"))


(defun dg/convert ()
  "Do the whole thing. I'll comment this more when I have time."
  (interactive)
  ;; First, let's require all built-in features. So we know everything is defined.
  (mapc
   (lambda (f) (require f nil t)) ;;no-error because some features are obsolete and throw errors.
   (full-feature-lister))
  ;; These lists will be used to create the functions.html and variables.html files
  (setq dg/file-list-variable nil
        dg/file-list-function nil)
  ;; Generate the doc for functions
  (let ((fill-column 1000) ;;This is to avoid artificial line breaks in the description.
        (dg/description 'describe-function) ;;This tells `dg/doc-to-html' what describing function to use.
        (dg/format "Fun/%s.html")
        (dg/file-list 'dg/file-list-function))
    (mapc 'dg/symbol-to-file (dg/function-list))) ;;This creates a file for each fbound symbol.
  ;; Generate the doc for variables
  (let ((dg/description 'describe-variable)
        (dg/format "Var/%s.html")
        (fill-column 1000)
        (dg/file-list 'dg/file-list-variable))
    (mapc 'dg/symbol-to-file (dg/variable-list)))
  ;; Recreate the index.html
  (let ((fun    (concat dg/dir "functions.html"))
        (var    (concat dg/dir "variables.html"))
        (header (concat dg/dir "header.htmlt"))
        (footer (concat dg/dir "footer.htmlt")))
    (with-temp-file fun
      (insert-file-contents-literally header)
      (goto-char (point-max))
      (insert (dg/cons-list-to-item-list dg/file-list-function "Functions"))
      (goto-char (point-max))
      (insert-file-contents-literally footer))
    (with-temp-file var
      (insert-file-contents-literally header)
      (goto-char (point-max))
      (insert (dg/cons-list-to-item-list dg/file-list-variable "Variables"))
      (goto-char (point-max))
      (insert-file-contents-literally footer))))

(defun dg/symbol-to-file (s)
  "Takes a symbol, produces an html file with the description.

The content of the file is the description given by the function
`dg/description'. Since `describe-function' (and variable) is
pretty slow, this is where most of the time is spent.

The name of the file is a slightly sanitized version of the
symbol name, since symbol names in elisp can be almost anything.
The symbol name and file name will later be used for creating a
list of links, so a cons cell like (SYMBOLNAME . FILE) is stored
in the variable `dg/file-list'."
  (let* ((file (format dg/format (dg/clean-symbol s)))
         (path (concat dg/dir file))
         ;; For some reason some symbols which are fbound throw errors
         ;; when calling describe-function (in my case that happened
         ;; with bookmark-map). This is to skip those symbols:
         (doc (condition-case nil (funcall dg/description s)
                (error nil))))
    (when doc
      (when dg/verbose
        (message "%5d / %d - %s"
                 (setq dg/count (1+ dg/count))
                 dg/total s))
      (with-temp-file path
        (insert (dg/doc-to-html doc))
        (set-buffer-file-coding-system 'no-conversion))
      (add-to-list dg/file-list (cons (dg/url-string (symbol-name s)) file)))))

(defun dg/url-string (s)
  "Convert reserved characters from S into their % encoding for use as URL."
  (replace-regexp-in-string
   "!" "%21"
   (replace-regexp-in-string
    "#" "%23"
    (replace-regexp-in-string
     "\\$" "%24"
     (replace-regexp-in-string
      "&" "%26"
      (replace-regexp-in-string
       "'" "%27"
       (replace-regexp-in-string
        "(" "%28"
        (replace-regexp-in-string
         ")" "%29"
         (replace-regexp-in-string
          "\\*" "%2A"
          (replace-regexp-in-string
           "\\+" "%2B"
           (replace-regexp-in-string
            "," "%2C"
            (replace-regexp-in-string
             "/" "%2F"
             (replace-regexp-in-string
              ":" "%3A"
              (replace-regexp-in-string
               ";" "%3B"
               (replace-regexp-in-string
                "=" "%3D"
                (replace-regexp-in-string
                 "\\?" "%3F"
                 (replace-regexp-in-string
                  "@" "%40"
                  (replace-regexp-in-string
                   "\\[" "%5B"
                   (replace-regexp-in-string
                    "\\]" "%5D"
                    (replace-regexp-in-string
                    "%" "%25" s))))))))))))))))))))


(defun dg/clean-symbol (s)
  "This cleans the symbol a little bit, so it can be used as a file name.
Fortunately gh-pages seems to use a linux file system, so the
only forbidden character is the /."
  (replace-regexp-in-string
   "/" "%_%"
   (replace-regexp-in-string
    "%" "%%" (symbol-name s))))

(defun dg/function-list ()
  ""
  (dg/-list-all 'fboundp))

(defun dg/variable-list ()
  ""
  (dg/-list-all 'boundp))

(defun dg/-list-all (dg/predicate)
  "List all internal symbols satifying the DG/PREDICATE.

The whole process (name, difference, sorting, reversing,
internalize) is far from optimized. But the rest of the script is
1000 times slower than this, so it doesn't really matter."
  (let ((dg/temp 
         (mapcar
          'intern-soft
          (nreverse ;;Reversing the sort order guarantees the links will be created in the right order later.
           (sort* ;;We sort now because it's easier, so the final list of links will be sorted.
            (cl-set-difference
             (loop for dg/my-unique-var being the symbols
                   if (funcall dg/predicate dg/my-unique-var)
                   collect (symbol-name dg/my-unique-var))
             dg/symbol-list)
            'string< :key 'downcase)))))
    (when dg/verbose
      (setq dg/count 0)
      (setq dg/total (length dg/temp)))
    dg/temp))

(defun dg/doc-to-html (doc)
  "This function takes the doc string (using standard unix line breaks), and converts it to an html format.
Right now it only fixes the line breaks. Improvements are planned here:
https://github.com/Bruce-Connor/emacs-online-documentation/issues/2"
  (replace-regexp-in-string
   "\n" "<br/>\n"
   (with-temp-buffer
     (insert doc)
     (goto-char (point-min))
     (insert "<strong>")
     (forward-sexp 1)
     (insert "</strong>")
     (when (search-forward-regexp "in `.*'\.$" (line-end-position) t)
       (forward-char -2)
       (insert "</code>")
       (forward-line 0)
       (search-forward-regexp "in `" (line-end-position) t)       
       (insert "<code>")))))

(defun dg/cons-list-to-item-list (li type)
  "Convert the list of (SYMBOLNAME . FILE) generated in `dg/symbol-to-file' to an html list of html links."
  (concat
   "\n<h2 id=\"" (downcase type) "\">" type "</h2>\n<ul>\n"
   (mapconcat 'dg/cons-to-item li "\n")
   "\n</ul>\n"))

(defun dg/cons-to-item (cell)
  "Convert a cons cell with (NAME . FILE) to an html item with a link."
  (concat "<li><a href=\"" (cdr cell) "\">" (car cell) "</a>"))

(provide 'documentation-generator)
;;; documentation-generator.el ends here.
