;;; documentation-generator.el --- Convert the documentation of all built-in functions and variables to html files.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: https://github.com/Bruce-Connor/emacs-online-documentation/
;; Version: 0.5
;; Keywords: 
;; ShortName: docgen
;; Separator: //

;;; Commentary:
;;
;; To run this, do:
;;
;;     emacs --batch -l documentation-generator.el -f docgen//convert

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
;; 0.5 - 20130822 - Improved removal of our own symbols
;; 0.5 - 20130822 - Implemented creation of an sqlite script
;; 0.5 - 20130819 - Use built-in url-hexify instead of concoting one.
;; 0.1 - 20130813 - Uploaded the file.
;; 0.1 - 20130811 - Created File.
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(load-file "./full-feature-lister.elc")

(defconst docgen//version "0.5" "Version of the documentation-generator.el package.")
(defconst docgen//version-int 3 "Version of the documentation-generator.el package, as an integer.")
(defun docgen//bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs and dg versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/emacs-online-documentation/issues/new")
  (message "Your docgen//version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           docgen//version emacs-version))

(defcustom docgen//dir (expand-file-name "~/Git-Projects/online-documentation-pages/") "" :type 'directory)

(defcustom docgen//sql-script-file (concat docgen//dir "renew-tables.sql") "" :type 'file)
(defconst docgen//sql-insert-string "\nINSERT INTO %s(Name, External) VALUES('%s', %s);")
(defconst docgen//sql-delete-string "\nDROP TABLE IF EXISTS %s;")
(defconst docgen//sql-create-string "\nCREATE VIRTUAL TABLE %s USING fts4(Name text UNIQUE NOT NULL, External integer);")

(defvar docgen//description nil "The description function used.")
(defvar docgen//count 0 "Used for reporting the progress of the conversion.")
(defvar docgen//total 0 "Used for reporting the progress of the conversion.")
(defcustom docgen//verbose t "Whether we should report the progress of the conversion."
  :type 'boolean)

(defun docgen//convert ()
  "Do the whole thing. I'll comment this more when I have time."
  (interactive)
  (docgen//log "First, let's require all built-in features. So we know everything is defined.")
  (mapc
   (lambda (f) (condition-case nil
                   (progn (message "%s" f)
                          (require f nil t))
                 (error nil))) ;;no-error because some features are obsolete and throw errors.
   (docgen//full-feature-lister))
  ;; These lists will be used to create the functions.html and variables.html files
  (setq docgen//file-list-variable nil
        docgen//file-list-function nil)
  (docgen//log "Erase the sql-script, so we can make a new one.")
  (when (file-readable-p docgen//sql-script-file)
    (delete-file docgen//sql-script-file t))
  (append-to-file (format docgen//sql-delete-string "Functions") nil docgen//sql-script-file)
  (append-to-file (format docgen//sql-delete-string "Variables") nil docgen//sql-script-file)
  (append-to-file (format docgen//sql-create-string "Functions") nil docgen//sql-script-file)
  (append-to-file (format docgen//sql-create-string "Variables") nil docgen//sql-script-file)
  (docgen//log "Generate the doc for functions")
  (let ((fill-column 1000) ;;This is to avoid artificial line breaks in the description.
        (docgen//sql-table-name "Functions")
        (docgen//description 'describe-function) ;;This tells `docgen//doc-to-html' what describing function to use.
        (docgen//format "Fun/%s.html")
        (docgen//file-list 'docgen//file-list-function))
    (mapc 'docgen//symbol-to-file (docgen//function-list))) ;;This creates a file for each fbound symbol.
  (docgen//log "Generate the doc for variables")
  (let ((docgen//sql-table-name "Variables")
        (docgen//description 'describe-variable)
        (docgen//format "Var/%s.html")
        (fill-column 1000)
        (docgen//file-list 'docgen//file-list-variable))
    (mapc 'docgen//symbol-to-file (docgen//variable-list)))
  (docgen//log "Recreate the index.html")
  (let ((fun    (concat docgen//dir "functions.html"))
        (var    (concat docgen//dir "variables.html"))
        (header (concat docgen//dir "header.htmlt"))
        (footer (concat docgen//dir "footer.htmlt")))
    (with-temp-file fun
      (insert-file-contents-literally header)
      (goto-char (point-max))
      (insert (docgen//cons-list-to-item-list docgen//file-list-function "Functions"))
      (goto-char (point-max))
      (insert-file-contents-literally footer))
    (with-temp-file var
      (insert-file-contents-literally header)
      (goto-char (point-max))
      (insert (docgen//cons-list-to-item-list docgen//file-list-variable "Variables"))
      (goto-char (point-max))
      (insert-file-contents-literally footer))))

(defun docgen//log (&rest r)
  ""
  (when docgen//verbose
    (apply 'message r)))

(defun docgen//symbol-to-file (s)
  "Takes a symbol, produces an html file with the description.

The content of the file is the description given by the function
`docgen//description'. Since `describe-function' (and variable) is
pretty slow, this is where most of the time is spent.

The name of the file is a slightly sanitized version of the
symbol name, since symbol names in elisp can be almost anything.
The symbol name and file name will later be used for creating a
list of links, so a cons cell like (SYMBOLNAME . FILE) is stored
in the variable `docgen//file-list'."
  (let* ((file (format docgen//format (docgen//clean-symbol s)))
         (path (concat docgen//dir file))
         ;; For some reason some symbols which are fbound throw errors
         ;; when calling describe-function (in my case that happened
         ;; with bookmark-map). This is to skip those symbols:
         (doc (condition-case nil (funcall docgen//description s)
                (error nil))))
    (when doc
      (docgen//log "%5d / %d - %s" (setq docgen//count (1+ docgen//count)) docgen//total s)
      (with-temp-file path
        (insert (docgen//doc-to-html doc))
        (set-buffer-file-coding-system 'no-conversion))
      (add-to-list docgen//file-list (cons (symbol-name s) (url-hexify-string file)))
      (append-to-file
       (docgen//format-sql-command docgen//sql-insert-string
                                   docgen//sql-table-name (symbol-name s) 0)
       nil docgen//sql-script-file))))

(defun docgen//format-sql-command (fs &rest strings)
  "Double any quotes inside STRINGS, then use them in FS as a regular format string."
  (apply 'format fs
         (mapcar
          (lambda (x) (replace-regexp-in-string "'" "''" (format "%s" x)))
          strings)))

(defun docgen//clean-symbol (s)
  "This cleans the symbol a little bit, so it can be used as a file name.
Fortunately gh-pages seems to use a linux file system, so the
only forbidden character is the /."
  (replace-regexp-in-string
   "/" "%_%"
   (replace-regexp-in-string
    "%" "%%" (symbol-name s))))

(defun docgen//function-list ()
  ""
  (docgen//-list-all 'fboundp))

(defun docgen//variable-list ()
  ""
  (docgen//-list-all 'boundp))

(defun docgen//-list-all (docgen//predicate)
  "List all internal symbols satifying the DOCGEN//PREDICATE.

The whole process (name, difference, sorting, reversing,
internalize) is far from optimized. But the rest of the script is
1000 times slower than this, so it doesn't really matter."
  (let ((docgen//temp 
         (mapcar
          'intern-soft
          (nreverse ;;Reversing the sort order guarantees the links will be created in the right order later.
           (cl-sort ;;We sort now because it's easier, so the final list of links will be sorted.
            (loop for docgen//my-unique-var being the symbols
                  if (and (null (string-match "\\`docgen//" (symbol-name docgen//my-unique-var)))
                          (funcall docgen//predicate docgen//my-unique-var))
                  collect (symbol-name docgen//my-unique-var))
            'string< :key 'downcase)))))
    (when docgen//verbose
      (setq docgen//count 0)
      (setq docgen//total (length docgen//temp)))
    docgen//temp))

(defun docgen//doc-to-html (doc)
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
       (insert "<code>"))
     (buffer-string))))

(defun docgen//cons-list-to-item-list (li type)
  "Convert the list of (SYMBOLNAME . FILE) generated in `docgen//symbol-to-file' to an html list of html links."
  (concat
   "\n<h2 id=\"" (downcase type) "\">" type "</h2>\n<ul>\n"
   (mapconcat 'docgen//cons-to-item li "\n")
   "\n</ul>\n"))

(defun docgen//cons-to-item (cell)
  "Convert a cons cell with (NAME . FILE) to an html item with a link."
  (concat "<li><a href=\"" (cdr cell) "\">" (car cell) "</a>"))


