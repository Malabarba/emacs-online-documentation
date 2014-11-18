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

(eval-and-compile (require 'cl-lib))
(load-file "./full-feature-lister.elc")
(load-file "./htmlize.elc")

(defconst docgen//version "0.5" "Version of the documentation-generator.el package.")
(defconst docgen//version-int 3 "Version of the documentation-generator.el package, as an integer.")
(defun docgen//bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please include your emacs and dg versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/emacs-online-documentation/issues/new")
  (message "Your docgen//version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           docgen//version emacs-version))

(defcustom docgen//outdir (expand-file-name "~/Git-Projects/online-documentation-pages/") "" :type 'directory)
(defcustom docgen//dir (expand-file-name "./") "" :type 'directory)

(defcustom docgen//sql-script-file (expand-file-name "renew-tables.sql" docgen//outdir) "" :type 'file)
(defconst docgen//sql-insert-string "\nINSERT INTO %s(Name, External) VALUES('%s', %s);")
;; (defconst docgen//sql-delete-string "\nDROP TABLE IF EXISTS %s;")
;; (defconst docgen//sql-create-string "\nCREATE VIRTUAL TABLE %s USING fts4(Name text UNIQUE NOT NULL, External integer);")

(defvar docgen//description nil "The description function used.")
(defvar docgen//count 0 "Used for reporting the progress of the conversion.")
(defvar docgen//total 0 "Used for reporting the progress of the conversion.")
(defcustom docgen//verbose t "Whether we should report the progress of the conversion."
  :type 'boolean)

(defvar docgen//file-list-function nil)

(defvar docgen//file-list-variable nil)

(defvar docgen//file-list-face nil)

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
  ;;;;;;;;
  (docgen//log "These lists will be used to create the functions.html and variables.html files")
  (setq docgen//file-list-variable nil
        docgen//file-list-function nil
        docgen//file-list-face nil)
  (docgen//log "Erase the sql-script, so we can make a new one.")
  ;; (when (file-readable-p docgen//sql-script-file)
  ;;   (delete-file docgen//sql-script-file t))
  (unless (file-writable-p docgen//sql-script-file)
    (error "Can't access sql script file %s" docgen//sql-script-file))
  (with-temp-file docgen//sql-script-file
    (erase-buffer)
    (insert-file-contents-literally
     (expand-file-name "sql-header.sqlt" docgen//dir)))
  ;; (append-to-file (format docgen//sql-delete-string "Functions") nil docgen//sql-script-file)
  ;; (append-to-file (format docgen//sql-delete-string "Variables") nil docgen//sql-script-file)
  ;; (append-to-file (format docgen//sql-create-string "Functions") nil docgen//sql-script-file)
  ;; (append-to-file (format docgen//sql-create-string "Variables") nil docgen//sql-script-file)
  (setq docgen//documentation-template
        (with-temp-buffer
          (insert-file-contents-literally
           (file-relative-name "documentation-template.htmlt" docgen//dir))))
  (docgen//log "Generate the doc for functions")
  ;;;;;;;;
  (docgen//log "Create a doc file for each symbol.")
  (let ((docgen//sql-table-name "Functions")
        ;;This is to avoid artificial line breaks in the description.
        (fill-column 1000)
        ;;This tells `docgen//doc-to-html' what describing function to use.
        (docgen//description 'docgen//describe-function)
        (docgen//format "Fun/%s.html")
        (docgen//file-list 'docgen//file-list-function))
    ;; This creates a file for each fbound symbol.
    (mapc 'docgen//symbol-to-file (docgen//function-list))) 
  (docgen//log "Generate the doc for variables")
  (let ((docgen//sql-table-name "Variables")
        (docgen//description 'docgen//describe-variable)
        (docgen//format "Var/%s.html")
        (fill-column 1000)
        (docgen//file-list 'docgen//file-list-variable))
    (mapc 'docgen//symbol-to-file (docgen//variable-list)))
  (docgen//log "Generate the doc for faces")
  (let ((docgen//sql-table-name "Faces")
        (docgen//description 'docgen//describe-face)
        (docgen//format "Face/%s.html")
        (fill-column 1000) 
        (docgen//file-list 'docgen//file-list-face))
    (mapc 'docgen//symbol-to-file (docgen//face-list)))
  ;; Create a file listing all of the docs.
  (docgen//log "Recreate the index.html")
  (docgen//create-file-from-list docgen//file-list-function "Functions")
  (docgen//create-file-from-list docgen//file-list-variable "Variables")
  (docgen//create-file-from-list docgen//file-list-face "Faces"))

(defun docgen//create-file-from-list (list file)
  "Convert the cons LIST into a FILE listing all links of the type."
  (let ((name   (concat docgen//outdir (downcase file) ".html"))
        (header (expand-file-name "header.htmlt" docgen//outdir))
        (footer (expand-file-name "footer.htmlt" docgen//outdir)))
    (with-temp-file name
      (insert-file-contents-literally header)
      (goto-char (point-max))
      (insert (docgen//cons-list-to-item-list docgen//file-list-function file))
      (goto-char (point-max))
      (insert-file-contents-literally footer))))

;; (defun docgen//faces-file-create ()
;;   (docgen//log "First, let's require all built-in features. So we know everything is defined.")
;;   (mapc
;;    (lambda (f) (condition-case nil
;;               (progn (message "%s" f)
;;                      (require f nil t))
;;             (error nil))) ;;no-error because some features are obsolete and throw errors.
;;    (docgen//full-feature-lister))
;;   ;; These lists will be used to create the functions.html and variables.html files
;;   (setq docgen//file-list-variable nil
;;         docgen//file-list-function nil
;;         docgen//file-list-face nil)
;;   (let ((docgen//sql-table-name "Faces")
;;         (docgen//description 'docgen//describe-face)
;;         (docgen//format "Face/%s.html")
;;         (fill-column 1000) 
;;         (docgen//file-list 'docgen//file-list-face)
;;         (fun    (concat docgen//outdir "functions.html"))
;;         (var    (concat docgen//outdir "variables.html"))
;;         (face   (concat docgen//outdir "faces.html"))
;;         (header (concat docgen//outdir "header.htmlt"))
;;         (footer (concat docgen//outdir "footer.htmlt")))
;;     (mapc
;;      (lambda (s)
;;        (let* ((file (format docgen//format (docgen//clean-symbol s)))
;;               (path (concat docgen//outdir file)))
;;          (add-to-list docgen//file-list (cons (symbol-name s) (url-hexify-string file)))))
;;      (docgen//face-list))    
;;     (with-temp-file face
;;       (insert-file-contents-literally header)
;;       (goto-char (point-max))
;;       (insert (docgen//cons-list-to-item-list docgen//file-list-face "Faces"))
;;       (goto-char (point-max))
;;       (insert-file-contents-literally footer))))


(defun docgen//log (&rest r)
  ""
  (when docgen//verbose
    (apply 'message r)))

(defvar docgen//format nil)

(defvar docgen//file-list nil)

(defvar docgen//sql-table-name nil)

(defvar docgen//documentation-template nil)

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
         (path (expand-file-name file docgen//outdir))
         ;; For some reason some symbols which are fbound throw errors
         ;; when calling describe-function (in my case that happened
         ;; with bookmark-map). This is to skip those symbols:
         (doc (ignore-errors (funcall docgen//description s))))
    (when doc
      (docgen//log "%5d / %d - %s" (setq docgen//count (1+ docgen//count)) docgen//total s)
      (with-temp-file path
        (insert
         (format docgen//documentation-template doc))
        (set-buffer-file-coding-system 'no-conversion))
      (push (cons (symbol-name s)
                  (format docgen//format (url-hexify-string (docgen//clean-symbol s))))
            docgen//file-list)
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

(defun docgen//clean-string (s)
  "This cleans the symbol a little bit, so it can be used as a file name.
Fortunately gh-pages seems to use a linux file system, so the
only forbidden character is the /."
  (replace-regexp-in-string
   "/" "%_%"
   (replace-regexp-in-string
    "%" "%%" s)))

(defun docgen//function-list ()
  ""
  (docgen//-list-all 'fboundp))

(defun docgen//variable-list ()
  ""
  (docgen//-list-all 'boundp))

(defun docgen//face-list ()
  ""
  (docgen//-list-all 'facep))

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

(defun docgen//describe-variable (variable)
  "Generate the full documentation of VARIABLE (a symbol) as html.
Returns the documentation as a string."
  (let ((buffer (current-buffer))
        (file-name (find-lisp-object-file-name variable 'defvar)))
    (with-temp-buffer
      (save-excursion
        (let ((valvoid (not (boundp variable)))
              (permanent-local (get variable 'permanent-local))
              (val (symbol-value variable))
              (locus (variable-binding-locus variable))
              val-start-pos)
          (insert
           "<strong>" (symbol-name variable) "</strong>"
           (if file-name
               (concat 
                " is a variable defined in `" "<code>"
                (if (eq file-name 'C-source)
                    "C source code"
                  (file-name-nondirectory file-name))
                "</code>" "'.</br>\n"
                (if valvoid "It is void as a variable." "Its "))
             (if valvoid " is void as a variable." "'s ")))
          
          (unless valvoid
            (setq val-start-pos (point))
            (insert "value is ")
            (let ((from (point))
                  (line-beg (line-beginning-position))
                  (print-rep
                   (let ((print-quoted t))
                     (prin1-to-string val)))
                  fits)
              (setq fits (< (+ (length print-rep) (point) (- line-beg)) 68))
              (insert "<pre>")
              (if fits (insert print-rep)
                (insert "</br>\n")
                (pp val 'insert)
                ;; ;; Not sure what this did
                ;; (if (< (point) (+ 68 (line-beginning-position 0)))
                ;;     (delete-region from (1+ from))
                ;;   (delete-region (1- from) from))
                )
              (insert "</pre>")
              (let* ((sv (get variable 'standard-value))
                     (origval (and (consp sv)
                                   (condition-case nil
                                       (eval (car sv))
                                     (error :help-eval-error)))))
                (when (and (consp sv)
                           (not (equal origval val))
                           (not (equal origval :help-eval-error)))
                  (insert "</br>\n<strong>Original</strong> value was </br>\n<pre>")
                  (setq from (point))
                  (pp origval 'insert)
                  (if (< (point) (+ from 20))
                      (delete-region (1- from) from))
                  (insert "</pre>")))))
          (insert "</br>\n")
          
          (when locus
            (cond
             ((framep locus) (insert "It is a frame-local variable; "))
             ((terminal-live-p locus) (insert "It is a terminal-local variable; "))
             (t))
            (if (not (default-boundp variable))
                (insert "globally void")
              (let ((global-val (default-value variable)))
                (insert "global value is ")
                (if (eq val global-val)
                    (insert "the same.")
                  (insert "</br>\n<pre>")
                  (let ((from (point)))
                    (pp global-val 'insert)
                    (if (< (point) (+ from 20))
                        (delete-region (1- from) from)))
                  (insert "</pre>"))))
            (insert "</br>\n"))

          ;; If the value is large, move it to the end.
          (when (> (count-lines (point-min) (point-max)) 10)
            ;; Note that setting the syntax table like below
            ;; makes forward-sexp move over a `'s' at the end
            ;; of a symbol.
            (set-syntax-table emacs-lisp-mode-syntax-table)
            (goto-char val-start-pos)
            (when (looking-at "value is") (replace-match ""))
            (save-excursion
              (insert "</br>\n</br>\n<strong id=\"value\">Value:</strong>"))
            (insert "value is shown "
                    "<a href=\"#value\">below</a>"
                    ".</br>\n"))

          (let* ((alias (condition-case nil
                            (indirect-variable variable)
                          (error variable)))
                 (obsolete (get variable 'byte-obsolete-variable))
                 (use (car obsolete))
                 (safe-var (get variable 'safe-local-variable))
                 (doc (or (documentation-property
                           variable 'variable-documentation)
                          (documentation-property
                           alias 'variable-documentation)))
                 (extra-line nil))

            ;; Mention if it's a local variable.
            (insert "<ul>")
            (cond
             ((and (local-variable-if-set-p variable)
                   (or (not (local-variable-p variable))
                       (with-temp-buffer
                         (local-variable-if-set-p variable))))
              (insert "  <li>Automatically becomes ")
              (if permanent-local
                  (insert "permanently "))
              (insert "buffer-local when set.</li>"))
             ((not permanent-local))
             ((bufferp locus)
              (insert "  <li>This variable's buffer-local value is permanent.</li>"))
             (t
              (insert "  <li>This variable's value is permanent if it is given a local binding.</li>")))

            ;; Mention if it's an alias.
            (unless (eq alias variable)
              (insert (format "  <li>This variable is an alias for `<strong><a href=\"/%s\">%s</a></strong>'.</li>\n"
                              (docgen//symbol-to-link alias)
                              (replace-regexp-in-string "<" "&lt;" (symbol-name alias)))))

            (when obsolete
              (insert "<li>  <strong>This variable is obsolete"
                      (if (nth 2 obsolete) (format " since %s" (nth 2 obsolete)) "")
                      (cond ((stringp use) (concat ";</br>\n  " use))
                            (use (format ";</br>\n  use `<a href=\"/%s\">%s</a>' instead."
                                         (url-hexify-string (docgen//clean-symbol (car obsolete)))
                                         (replace-regexp-in-string "<" "&lt;" (symbol-name (car obsolete)))))
                            (t "."))
                      "</strong></li>"))

            (when (member (cons variable val)
                          (with-current-buffer buffer file-local-variables-alist))
              (if (member (cons variable val)
                          (with-current-buffer buffer dir-local-variables-alist))
                  (insert "<li>  This variable's value is directory-local.</li>")
                (insert "<li>  This variable's value is file-local.</li>")))

            (when (memq variable ignored-local-variables)
              (insert "<li>  This variable is ignored as a file-local \
variable.</li>"))

            ;; Can be both risky and safe, eg auto-fill-function.
            (when (risky-local-variable-p variable)
              (insert "<li>  This variable may be risky if used as a file-local variable.</li>"))

            (when safe-var
              (insert "<li>  This variable is safe as a file local variable "
                      "if its value</br>\n  satisfies the predicate "
                      (if (byte-code-function-p safe-var)
                          "which is a byte-compiled expression.</br>\n"
                        (format "`%s'.</li>" safe-var))))

            (if (looking-back "<ul>")
                (replace-match "")
              (insert "</ul>"))

            (insert "</br>\n<strong id=\"documentation\">Documentation:</strong></br>\n"
                    (replace-regexp-in-string 
                     "\n" "</br>\n" 
                     (or doc "Not documented as a variable."))))

          ;; Make a link to customize if this variable can be customized.
          (when (custom-variable-p variable)
            (insert "</br>\n</br>\n" 
                    "You can customize this variable.")
            ;; Note variable's version or package version
            (let ((output (describe-variable-custom-version-info variable)))
              (when output
                (insert "</br>\n</br>\n" output))))

          ;; Return the text we displayed.
          (buffer-string))))))

(defun docgen//describe-function (function)
  "Display the full documentation of FUNCTION (a symbol) as html."
  (with-temp-buffer
    (save-excursion
      (insert "<strong>"
              (replace-regexp-in-string "<" "&lt;" (symbol-name function))
              "</strong>"
              ;; Use " is " instead of a colon so that
              ;; it is easier to get out the function name using forward-sexp.
              " is ")
      (let* ((advised (and (symbolp function) (featurep 'nadvice)
                           (advice--p (advice--symbol-function function))))
             ;; If the function is advised, use the symbol that has the
             ;; real definition, if that symbol is already set up.
             (real-function
              (or (and advised
                       (advice--cd*r (advice--symbol-function function)))
                  function))
             ;; Get the real definition.
             (def (if (symbolp real-function)
                      (symbol-function real-function)
                    real-function))
             (aliased (or (symbolp def)
                          ;; Advised & aliased function.
                          (and advised (symbolp real-function))))
             (real-def (cond
                        (aliased (let ((f real-function))
                                   (while (and (fboundp f)
                                               (symbolp (symbol-function f)))
                                     (setq f (symbol-function f)))
                                   f))
                        ((subrp def) (intern (subr-name def)))
                        (t def)))
             (file-name (find-lisp-object-file-name function def))
             (pt1 (point))
             (beg (if (and (or (byte-code-function-p def)
                               (keymapp def)
                               (memq (car-safe def) '(macro lambda closure)))
                           file-name
                           (help-fns--autoloaded-p function file-name))
                      (if (commandp def)
                          "an interactive autoloaded "
                        "an autoloaded ")
                    (if (commandp def) "an interactive " "a "))))

        ;; Print what kind of function-like object FUNCTION is.
        (princ
         (if aliased
             ;; Aliases are Lisp functions, so we need to check
             ;; aliases before functions.
             (format "an alias for `<strong><a href=\"/%s\">%s</a></strong>'"
                     (docgen//symbol-to-link real-def)
                     (replace-regexp-in-string
                      "<" "&lt;" (symbol-name real-def)))
           (replace-regexp-in-string
            "<" "&lt;"
            (cond ((or (stringp def) (vectorp def))
                   "a keyboard macro")
                  ((subrp def)
                   (if (eq 'unevalled (cdr (subr-arity def)))
                       (concat beg "special form")
                     (concat beg "built-in function")))
                  ((or (eq (car-safe def) 'macro)
                       ;; For advised macros, def is a lambda
                       ;; expression or a byte-code-function-p, so we
                       ;; need to check macros before functions.
                       (macrop function))
                   (concat beg "Lisp macro"))
                  ((byte-code-function-p def)
                   (concat beg "compiled Lisp function"))
                  ((eq (car-safe def) 'lambda)
                   (concat beg "Lisp function"))
                  ((eq (car-safe def) 'closure)
                   (concat beg "Lisp closure"))
                  ((autoloadp def)
                   (format "%s autoloaded %s"
                           (if (commandp def) "an interactive" "an")
                           (if (eq (nth 4 def) 'keymap) "keymap"
                             (if (nth 4 def) "Lisp macro" "Lisp function"))))
                  ((keymapp def)
                   (let ((is-full nil)
                         (elts (cdr-safe def)))
                     (while elts
                       (if (char-table-p (car-safe elts))
                           (setq is-full t
                                 elts nil))
                       (setq elts (cdr-safe elts)))
                     (concat beg (if is-full "keymap" "sparse keymap"))))
                  (t "")))) 'insert)

        (if (and aliased (not (fboundp real-def)))
            (insert ",</br>\nwhich is not defined.  Please make a bug report.")
          
          (when file-name
            (insert " in `<code>"
                    (if (eq file-name 'C-source)
                        "C source code"
                      (file-name-nondirectory file-name))
                    "</code>'"))
          (insert ".")
          (fill-region-as-paragraph (save-excursion (goto-char pt1) (forward-line 0) (point))
                                    (point))
          (insert "</br>\n</br>\n")

          (save-excursion            
            (let* ((standard-output (current-buffer))
                   (doc-raw (documentation function t))
                   ;; If the function is autoloaded, and its docstring has
                   ;; key substitution constructs, load the library.
                   (doc (progn
                          (and (autoloadp real-def) doc-raw
                               help-enable-auto-load
                               (string-match "\\([^\\]=\\|[^=]\\|\\`\\)\\\\[[{<]"
                                             doc-raw)
                               (load (cadr real-def) t))
                          (substitute-command-keys doc-raw))))
              (help-fns--key-bindings function)
              (setq doc (help-fns--signature function doc real-def real-function))
              (run-hook-with-args 'help-fns-describe-function-functions function)
              (insert "\n" (or doc "Not documented."))))
          (save-excursion
            (while (search-forward "<" nil t)
              (replace-match "&lt;" nil nil)))
          (save-excursion
            (when (search-forward "\n\n(" nil t)
              (save-excursion
                (forward-char -1)
                (insert "<code>"))
              (forward-sexp 1)
              (while (re-search-forward "&[^ ]+" (1- (line-end-position)) t)
                (replace-match "<em>\\&</em>" t))
              (end-of-line)
              (forward-char -1)
              (insert "</code>")))
          (save-excursion
            (let (p)
              (while (setq p (text-property-any (point) (buffer-end 1) 'face 'help-argument-name))
                (goto-char p)
                (insert "<u>")
                (goto-char (or (text-property-not-all (point) (buffer-end 1) 'face 'help-argument-name)
                               (buffer-end 1)))
                (insert "</u>"))))
          (save-excursion
            (while (search-forward "\n" nil t)
              (replace-match "</br>\n" nil nil)))))

      ;; Return the text.
      (buffer-string))))

(defun docgen//describe-face (face)
  "Return an html description of the properties of face FACE."
  (with-temp-buffer
    (let* ((attrs '((:family . "Family") (:foundry . "Foundry")
                    (:width . "Width") (:height . "Height")
                    (:weight . "Weight") (:slant . "Slant")
                    (:foreground . "Foreground") (:distant-foreground . "DistantForeground")
                    (:background . "Background") (:underline . "Underline")
                    (:overline . "Overline") (:strike-through . "Strike-through")
                    (:box . "Box") (:inverse-video . "Inverse")
                    (:stipple . "Stipple") (:font . "Font")
                    (:fontset . "Fontset") (:inherit . "Inherit")))
           (max-width (apply #'max (mapcar #'(lambda (x) (length (cdr x))) attrs)))
           (standard-output (current-buffer))) 
      (unless face (setq face 'default))
      (if (not (listp face))
          (setq face (list face)))
      (with-current-buffer standard-output
        (dolist (f face)
          (if (stringp f) (setq f (intern f)))
          ;; We may get called for anonymous faces (i.e., faces
          ;; expressed using prop-value plists).  Those can't be
          ;; usefully customized, so ignore them.
          (when (symbolp f)
            (insert "Face: <strong>"
                    (replace-regexp-in-string "<" "&lt;" (symbol-name f))
                    "</strong>")
            (save-excursion
              (let (file-name)
                (insert " (") 
                (docgen//insert-propertize "sample" f)
                (insert ")</br>\n")
                (let ((alias (get f 'face-alias))
                      (face f) obsolete)
                  ;; Alias or absolete
                  (when alias
                    (setq face alias)
                    (insert
                     "<ul>"
                     (format "<li>%s is an alias for the face `<strong><a href=\"/%s\">%s</a></strong>'.</li>"
                             f (docgen//symbol-to-link alias)
                             (replace-regexp-in-string "<" "&lt;" (symbol-name alias)))
                     (if (setq obsolete (get f 'obsolete-face))
                         (format "<li>  This face is obsolete%s; use `<strong><a href=\"/%s\">%s</a></strong>' instead.</li>"
                                 (if (stringp obsolete) (format " since %s" obsolete) "")
                                 (docgen//symbol-to-link alias)
                                 (replace-regexp-in-string "<" "&lt;" (symbol-name alias)))
                       "")
                     "</ul>"))
                  ;; Documentation
                  (insert "</br>\n<strong>Documentation:</strong></br>\n"
                          (or (face-documentation face)
                              "Not documented as a face.")
                          "</br>\n</br>\n"))
                ;; Where it's defined.
                (setq file-name (find-lisp-object-file-name f 'defface))
                (when file-name
                  (insert "Defined in `<code>"
                          (replace-regexp-in-string "<" "&lt;" (file-name-nondirectory file-name))
                          "</code>'.</br>\n</br>\n"))
                (insert "<table>")
                (dolist (a attrs)
                  (let ((attr (format "%s" (face-attribute f (car a) nil))))
                    (insert "<tr><td align=\"right\">" (cdr a) "</td><td>:</td><td></td><td>"
                            (if (and (eq (car a) :inherit) (not (string= attr "unspecified")))
                                ;; Make a hyperlink to the parent face.
                                (format
                                 "<a href=\"/%s\">%s</a>"
                                 (docgen//symbol-to-link attr)
                                 (replace-regexp-in-string "<" "&lt;" attr))
                              (replace-regexp-in-string "<" "&lt;" attr))
                            "</td></tr>\n")))
                (insert "<table>"))
              (insert "</br>\n"))))))
    (buffer-string)))

(defun docgen//symbol-to-link (symb &optional format)
  "Convert `shadow' or \"shadow\" to \"Face/shadow\"."
  (format
   (or format docgen//format)
   (url-hexify-string
    (if (stringp symb)
        (docgen//clean-string symb)
      (docgen//clean-symbol symb)))))

(defun docgen//insert-propertize (string face)
  ""
  (let* ((docgen//face-map (htmlize-make-face-map (list face)))
         (fstruct-list
          (delq nil (mapcar 
                     (lambda (f)
                       (gethash f docgen//face-map))
                     (list face)))))
    (htmlize-inline-css-insert-text string fstruct-list 'insert))) 



(defun docgen//cons-list-to-item-list (li type)
  "Convert the list of (SYMBOLNAME . FILE) generated in `docgen//symbol-to-file' to an html list of html links."
  (concat
   "\n<h2 id=\"" (downcase type) "\">" type "</h2>\n<ul>\n"
   (mapconcat 'docgen//cons-to-item li "\n")
   "\n</ul>\n"))

(defun docgen//cons-to-item (cell)
  "Convert a cons cell with (NAME . FILE) to an html item with a link."
  (concat "<li><a href=\"" (cdr cell) "\">" (car cell) "</a>"))
