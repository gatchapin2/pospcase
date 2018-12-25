;;; pospcase-font-lock.el: S-expression code highlighting using pospcase -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 gatchapin

;; Author: gatchapin
;; Keywords: languages, faces
;; URL: https://github.com/gatchapin2/pospcase

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'pospcase)

(defcustom pospcase-font-lock-special-variable-name-face
  'font-lock-warning-face
  "Face name for special variables such as variables declared by
`defvar'. Disabled when set to nil."
  :type '(choice (const nil)
                 face)
  :group 'pospcase)

(defcustom pospcase-font-lock-quoted-face
  'font-lock-constant-face
  "Face name for quoted expressions. Disabled when set to nil."
  :type '(choice (const nil)
                 face)
  :group 'pospcase)

(defcustom pospcase-font-lock-backquote-face
  'font-lock-warning-face
  "Face name for backquotes and commas. Disabled when set to
nil."
  :type '(choice (const nil)
                 face)
  :group 'pospcase)

(defcustom pospcase-font-lock-quoted-function-face
  'font-lock-function-name-face
  "Face name for #' prefixed functions. Disabled when set to nil."
  :type '(choice (const nil)
                 face)
  :group 'pospcase)


;;; `pcase' powered font-lock

(defvar pospcase--matches nil
  "Currently working list of point pairs. It is
  structured like:

  (((start . end)   ; (match-string 1) of first (match-data)
    (start . end))  ; (match-string 2) of first (match-data)
   ((start . end)   ; (match-string 1) of second (match-data)
    (start . end))) ; (match-string 2) of second (match-data)")

(defvar pospcase--match-beginning nil
  "Start point of submatch 0 used by multiline font lock.")

(defvar pospcase--prematches nil
  "List of point pairs attached by matcher iterators at the head
  of each list of point pairs.")

(defun pospcase--list (&rest matches)
  "Same as `list'. But attaches non-submatcher based matches if they exist."
  (append pospcase--prematches matches))

(defvar pospcase--iterating nil
  "Internal variable for deciding if the iterator is active.")

(defvar pospcase--keyword-end nil
  "End of keyword to jump to when iteration ends.")

(defun pospcase--iterator (limit)
  "Actual iterator."
  (if pospcase--matches
      (let ((mlist (cl-loop for pair in (car pospcase--matches)
                            append (list (car pair) (cdr pair)))))
        (set-match-data (append (list pospcase--match-beginning
                                      limit)
                                mlist))
        (setq pospcase--matches (cdr pospcase--matches))
        t)
    (goto-char pospcase--keyword-end)
    (set-match-data nil)
    (setq pospcase--iterating nil
          pospcase--keyword-end nil)
    nil))

(defun pospcase-match-nil (limit)
  "Simply set `pospcase--prematches' to `pospcase--matches.'"
  (if pospcase--prematches
      (progn
        (setq pospcase--matches (list pospcase--prematches)
              pospcase--prematches nil)
        (pospcase--iterator limit))
    nil))

(defvar pospcase--ignore nil
  "When the variable is set to `t', the `pospcase--matches'
  assignment part is going to be skipped. ")

(defmacro pospcase--call-iterator (clause limit &optional allow-atom-p)
  "Catch parsing error, and call `pospcase--iterator'."
  `(condition-case nil
       (progn
         (unless (or pospcase--ignore
                     pospcase--matches
                     pospcase--iterating)
           (let ((temp (ignore-errors
                         (read-from-string
                          (pospcase--buffer-substring (point) ,limit)))))
             (setq pospcase--matches
                   (cond
                    ((null temp) nil) ; caught error while read-from-string
                    ((and (consp temp)
                          (or (null (car temp)) ; empty list at `point'
                              ,(if allow-atom-p
                                   nil  ; jump to t branch
                                 '(atom (car temp)))))
                     (if pospcase--prematches
                         (prog1
                             (list pospcase--prematches)
                           (setq pospcase--prematches nil))
                       nil))
                    (t (ignore-errors ,clause)))
                   pospcase--iterating t)))
         (pospcase--iterator ,limit))
     (error
      (goto-char ,limit)
      nil)))

(defvar pospcase--fence-start nil
  "Boundary cons cell (exp . (start . end)) for dropping
  unnecessary tree branches before here.")

(defmacro pospcase--call-list-iterator (&rest patterns)
  "Boilerplate code for arbitrary length variable list matcher iterator."
  `(pospcase--call-iterator
    (mapcar
     (lambda (srpair)
       (goto-char (cadr srpair))
       (pospcase-at (point) ,(list 'quote patterns)))
     (if pospcase--fence-start
         (member pospcase--fence-start (car (pospcase-read (point))))
       (car (pospcase-read (point)))))
    limit))

(defun pospcase-match-list/2 (limit)
  "Matcher iterator for a list of symbol or two length lists."
  (pospcase--call-list-iterator (`(,name ,type) (pospcase--list name type))
                                (`,name (pospcase--list name))))

(defun pospcase-match-parameter (limit)
  "Matcher iterator for a list of symbol, two or three length lists."
  (pospcase--call-list-iterator (`((,kw ,name) ,init ,sup) (list kw name init sup)) ; a hacky use of list
                                (`((,kw ,name) ,init) (list kw name init))
                                (`((,kw ,name)) (list kw name))
                                (`(,name ,init ,sup) (pospcase--list name init sup))
                                (`(,name ,init) (pospcase--list name init))
                                (`,name (pospcase--list name))))

(defun pospcase-match-list/1 (limit)
  "Matcher iterator for a symbol or `car's of a list of lists"
  (pospcase--call-list-iterator (`(,name . ,_) (pospcase--list name))
                                (`,name (pospcase--list name))))

(defalias #'pospcase-match-defstruct #'pospcase-match-list/1)

(defun pospcase-match-setq (limit)
  "Matcher iterator for variable names of `setq'."
  (pospcase--call-iterator
   (cl-loop for (var _) on (cdar (pospcase-read (point))) by #'cddr
            if (symbolp (car var))
            collect (pospcase--list (cdr var)))
   limit))

(defmacro pospcase--call-flet-iterator (clause)
  "Boilerplate code for arbitrary length function list matcher iterator."
  `(pospcase--call-iterator
    (cl-loop for srpair in (car (pospcase-read (point)))
             append
             (progn
               (goto-char (cadr srpair))
               (multiple-value-bind
                   (name args)
                   (pospcase-at (point)
                                '((`(,name ,args . ,_) (values name args))))
                 (progn
                   (goto-char (car args))
                   (let ((arglist (pospcase-read (point))))
                     ,clause)))))
    limit))

(defun pospcase-match-flet (limit)
  "Matcher iterator for a list of `flet' bindings."
  (pospcase--call-flet-iterator (if (car arglist)
                                    (mapcar
                                     (lambda (exp)
                                       (pospcase--list name
                                             (pospcase exp '((`(,arg . ,_) arg)
                                                             (`,arg arg)))))
                                     (car arglist))
                                  (list (pospcase--list name)))))

(defun pospcase-collect-all-symbols (node)
  "Collect all symbols from a tree generated by `pospcase-read'."
  (cl-labels
      ((pos-p (pair)
              (and (numberp (car pair))
                   (numberp (cdr pair))))
       (leaf-p (node)
               (and (atom (car node))
                    (pos-p (cdr node))))
       (ignore-p (node)
                 (and (consp (car node))
                      (member (caar node) '(\` \, quote function)))))
    (cl-loop with result
             for temp = node then (cdr temp)
             if (or (null temp)
                    (pos-p temp)
                    (ignore-p temp)) return result
             else if (leaf-p temp) return (append
                                           result
                                           (list (cdr temp)))
             else unless (ignore-p (car temp)) do (setq
                                                   result
                                                   (append
                                                    result
                                                    (pospcase-collect-all-symbols
                                                     (car temp)))))))

(defun pospcase-match-destructuring (limit)
  "Matcher iterator for symbols in an arbitrarily nested list."
  (pospcase--call-iterator
   (mapcar #'pospcase--list (pospcase-collect-all-symbols (pospcase-read (point))))
   limit
   t))

;; Unused alternate implementation without using `pospcase-read'.
;; This function collects only symbols, the above implementation
;; collects all atoms.
(defun pospcase-match-destructuring-alt (limit)
  "Matcher iterator for symbols in an arbitrarily nested list."
  (pospcase--call-iterator
   (let ((read-with-symbol-positions t))
     (mapcar (lambda (pair)
               (cons (car pair)
                     (cons (+ (point) (cdr pair)) (scan-sexps (+ (point) (cdr pair)) 1))))
             (progn
               (read (pospcase--buffer-substring (point) (scan-sexps (point) 1)))
               read-symbol-positions-list)))
   limit
   t))

(defun pospcase-match-loop (limit)
  "Matcher iterator for loop variable symbols in an arbitrarily
nested list."
  (pospcase--call-iterator
   (if (memq (save-excursion
               (backward-up-list)
               (down-list)
               (read (current-buffer)))
             '(loop cl-loop))
       (mapcar #'pospcase--list (pospcase-collect-all-symbols (pospcase-read (point)))))
   limit
   t))

(defun pospcase-match-macrolet (limit)
  "Matcher iterator for a lit of `macrolet' bindings"
  (pospcase--call-flet-iterator (if (car arglist)
                                    (mapcar (lambda (arg) (pospcase--list name arg))
                                            (pospcase-collect-all-symbols arglist))
                                  (list (pospcase--list name)))))

(defmacro pospcase--preform (&rest body)
  "Multiline font lock use submatch 0 for jit fontification."
  `(progn
     (setq pospcase--match-beginning (match-beginning 0))
     ,@body))

(defmacro pospcase--postform (&rest body)
  "Necessary for jit-lock?"
  `(progn
     (setq pospcase--matches nil
           pospcase--prematches nil
           pospcase--fence-start nil
           pospcase--ignore nil)
     ,@body))

(defvar pospcase--dummy nil
  "Used in a hack for empty `multiple-value-bind'.")

(defvar pospcase-list-group '(list/2 list/1 destructuring flet macrolet)
  "Submatchers with same behavior of `pospcase-match-list/2'.")

(defvar pospcase-defstruct-group '(defstruct setq)
  "Submatchers with same behavior of `pospcase-match-defstruct'.")

(defvar pospcase-parameter-group '(parameter)
  "Submatchers with same behavior of `pospcase-match-parameter'.")

(defvar pospcase-loop-group '(loop)
  "Submatchers with same behavior of `pospcase-match-loop'.")

(defcustom pospcase-stringify-heading-keyword-cases
  '((`(and (quote ,(and (pred symbolp) sym)) ,_) (symbol-name sym))
    (`,_ (error "Unsupported heading keyword pattern.")))
  "List of `pcase' cases for translating heading `pcase' pattern
to string for later regexp pattern creation."
  :type '(repeat (list (sexp :tag "Pattern")
                       (sexp :tag "String Generator")))
  :group 'pcase)

(defun pospcase--stringfy-heading-keyword (pattern)
  (eval `(pcase (quote ,pattern)
           ,@pospcase-stringify-heading-keyword-cases)))

(defcustom pospcase-user-submatcher-conds nil
  "User defined `cond' branches for `pospcase-font-lock-build'."
  :type '(choice (const nil)
                 (repeat (list (sexp :tag "predicate")
                               (repeat :tag "body"
                                       sexp))))
  :group 'pospcase)

(defmacro pospcase--generate-submatcher-preform ()
  "Generate submatcher specific preform code. This macro exists
to make the following `cond' branching extensible to the users."
  `(cond
    ,@pospcase-user-submatcher-conds    ; user defined branches

    ;; Static variable name comes from the bindings of the caller's
    ;; scope. In this case `submatcher' below.
    ((null submatcher)
     '(condition-case nil
          (prog1
              (scan-sexps (point) 1)
            (goto-char (match-end 0)))
        (error (1+ (goto-char (1- (match-end 0)))))))

    ((memq (cdr submatcher) pospcase-list-group)
     `(progn
        (goto-char (car ,(car submatcher))) ; all nested backquote
                                            ; comma are evaluated
                                            ; within the caller's
                                            ; scope too
        (cdr ,(car submatcher))))

    ((memq (cdr submatcher) pospcase-defstruct-group)
     `(progn
        (setq pospcase--fence-start
              (ignore-errors (pospcase-read (car ,(car submatcher)))))
        (unless pospcase--fence-start (setq pospcase--ignore t))
        (condition-case nil
            (progn
              (goto-char (car ,(car submatcher)))
              (backward-up-list)
              (scan-sexps (point) 1))
          (error (1+ (goto-char (1- (match-end 0))))))))

    ((memq (cdr submatcher) (append pospcase-parameter-group
                                    pospcase-loop-group))
     `(let ((end (match-end 0)))
        (if (memq (char-before (point))
                  '(?\\ ?\' ?\` ?\,)) ; when keyword is use as symbol
            (progn
              (setq pospcase--ignore t)
              (1+ (goto-char (1- end))))
          (goto-char end)
          (setq pospcase--fence-start
                (ignore-errors (pospcase-read end)))
          (if pospcase--fence-start
              (condition-case nil
                  ,(if (memq (cdr submatcher) pospcase-parameter-group)
                       '(prog1
                            (save-excursion
                              (up-list)
                              (point))
                          (backward-up-list))
                     '(scan-sexps (point) 1))
                (error (1+ (goto-char (1- end)))))
            (setq pospcase--ignore t)
            (1+ (goto-char (1- end)))))))

    (t (error "Not supported submatcher: %s" submatcher))))

(defun pospcase-font-lock-build (patterns specs)
  "Actual font lock keywords generator."
  (let* (noparens
         (keywords (mapcar (lambda (pattern)
                             (if (consp pattern)
                                 (cond
                                  ((eq (car pattern) 'quote)
                                   (concat "'" (prin1-to-string pattern)))
                                  ((eq (car pattern) '\`)
                                   (if (consp (cadr pattern))
                                       (cond
                                        ((and
                                          (consp (caadr pattern))
                                          (eq (caaadr pattern) '\,))
                                         (concat "("
                                                 (pospcase--stringfy-heading-keyword
                                                  (car (cdaadr pattern)))))
                                        ((and
                                          (consp (cadr pattern))
                                          (eq (caadr pattern) '\,))
                                         (setq noparens t)
                                         (pospcase--stringfy-heading-keyword
                                          (car (cdadr pattern)))))
                                     (error "Invalid heading keyword.")))
                                  (t (error "Invalid heading keyword.")))
                               (symbol-name pattern)))
                           patterns))
         (matcher (concat (when noparens "\\_<") (regexp-opt keywords) "\\_>\\s *"))
         (submatchers (cl-loop for spec in specs
                               if (consp (car spec)) collect (car spec)))
         (subvars (mapcar #'car submatchers))
         (vars (mapcar (lambda (spec) (if (consp (car spec)) (caar spec) (car spec)))
                       specs))
         (non-subvars (cl-remove-if (lambda (var) (memq var subvars)) vars)))

    ;; Let's build font lock keywords
    (mapcar
     (lambda (submatcher)
       `(,matcher
         (,(intern (concat "pospcase-match-" (symbol-name (cdr submatcher))))
          (pospcase--preform
           (goto-char (match-beginning 0))
           (setq pospcase--keyword-end (match-end 0))

           ;; If the keyword is in a string or a comment, a flag
           ;; for ignoring is set. And the cursor is moved to the
           ;; end of the string or the comment block.
           (let ((table (syntax-ppss)))
             (if (nth 8 table)          ; in string or comment
                 (progn
                   (setq pospcase--ignore t)
                   (when (nth 3 table)     ; in string
                     (goto-char (or (ignore-errors (scan-sexps (nth 8 table)) 1)
                                    (point-max))))
                   (forward-comment most-positive-fixnum)
                   (point))

               ;; If the keyword appears in actual code section,
               ;; positioal data are assigned to appropriate
               ;; variables by pattern matching using supplied
               ;; PATTERNS.
               (condition-case nil
                   (multiple-value-bind ,vars (pospcase-at
                                               (point)
                                               ',(mapcar
                                                  (lambda (pat)
                                                    (list
                                                     pat
                                                     (cons
                                                      'values
                                                      (mapcar
                                                       (lambda (var)
                                                         (list 'pospcase-pos var))
                                                       vars))))
                                                  patterns))
                     ,(unless (null non-subvars)
                        `(setq pospcase--prematches ,(cons 'list non-subvars)))

                     ;; Then appropriate preparation code (extra
                     ;; validity check, moving the cursor to
                     ;; appropriate position, calculate the end of
                     ;; highlighting region) is generated for each
                     ;; group of submatcher using a macro.
                     ,(pospcase--generate-submatcher-preform))

                 (error (goto-char (match-end 0)))))))

          (pospcase--postform)

          ;; fontspecs
          ,@(cl-loop with i = 0
                     for spec in specs
                     if (or (symbolp (car spec))
                            (and (consp (car spec))
                                 (eq (caar spec) (car submatcher))))
                     append (mapcar (lambda (font)
                                      (list (incf i)
                                            (if (symbolp font)
                                                (list 'quote font)
                                              (apply (car font) (cdr font)))
                                            nil t))
                                    (cdr spec))))))

     (or submatchers
         '(nil)))))                   ; no submatcher

(defun pospcase-font-lock (mode patterns specs &optional buffer-local-p)
  "Font lock keywords generator with `pcase' powered pattern
matching. Currently you can use submatchers:

Group one: list/2, list/1, destructuring, flet, macrolet

Group two:  defstruct, setq

Group three: parameter

Group four: loop

These submatchers are not comprehensible and only covers the
S-expression patterns the author's need. By writing your own
submatcher and registering it to `pospcase-user-submatcher-conds'
by supplying submatcher specific preform, you can fully extend
the patterns `pospcase-font-lock' supports.

Argument PATTERNS is a list of `pcase' patterns.

And SPECS is a list of slightly extended fontspec.

The rest is structured like:

  (match-name . (list of font-face-name)) or
  ((match-name . submatcher) . (list of font-face-name))

See the code of `pospcase-font-lock-lisp-setup' for working
examples."
  (let* ((id (let ((split (split-string (symbol-name mode) "-mode")))
               (if (null (cdr split))
                   (error "%s is not a mode name." mode)
                 (car split))))
         (keyvar (intern (format "pospcase-font-lock-%s-keywords" id)))
         (keyvar-local (intern (format "pospcase-font-lock-%s-local-keywords" id)))
         (keyvar-extra (intern (format "pospcase-font-lock-%s-extra-keywords" id)))
         (keyvar-installed (intern (format "pospcase-font-lock-%s-keywords--installed" id)))
         (keyvar-add (intern (format "pospcase-font-lock-%s-keywords-add" id)))
         (keyvar-remove (intern (format "pospcase-font-lock-%s-keywords-remove" id)))
         (keywords (pospcase-font-lock-build
                    ;; Simple symbol heading keyword to (and 'defun heading-keyword)
                    (mapcar
                     (lambda
                       (pattern)
                       (pcase pattern
                         (``(,(and (pred symbolp) keyword) . ,rest)
                          (list '\`
                                (cons
                                 (list '\,
                                       `(and ,(list 'quote keyword) heading-keyword))
                                 rest)))
                         (`,(and (pred symbolp) keyword)
                          (list '\`
                                (list '\,
                                      `(and ,(list 'quote keyword) heading-keyword))))
                         (`,any any)))
                     patterns)
                    specs)))
    (unless (boundp keyvar)
      (eval `(progn
               (defvar
                 ,keyvar
                 nil
                 ,(format "List of font lock keywords for %s." (capitalize id)))
               (defvar-local
                 ,keyvar-local
                 nil
                 ,(format "List of font lock buffer local keywords for %s." (capitalize id)))
               (defvar-local
                 ,keyvar-installed
                 nil
                 "List of installed font lock keywords for current buffer.")
               (defun ,keyvar-add ()
                 ,(format "Activate `pcase' based font lock for %s." (capitalize id))
                 (set (make-local-variable 'font-lock-multiline) t)
                 (when (local-variable-p
                        ',keyvar-installed)
                   (font-lock-remove-keywords nil ,keyvar-installed))
                 (set (make-local-variable ',keyvar-installed)
                      (append (if (boundp ',keyvar-extra) ,keyvar-extra)
                              ,keyvar-local
                              ,keyvar))
                 (font-lock-add-keywords nil ,keyvar-installed 'append))
               (defun ,keyvar-remove ()
                 ,(format "Deactivate `pcase' based font lock for %s." (capitalize id))
                 (font-lock-remove-keywords nil ,keyvar-installed))

               )))
    (mapc (lambda (keyword)
            (let ((var (if buffer-local-p
                           keyvar-local
                         keyvar)))
              (unless (member keyword (symbol-value var))
                (set var (cons keyword (symbol-value var))))))
          keywords)))


;;; non `pcase' powered font-lock

(defun pospcase-font-lock-variable-face-form (name)
  "Form for detecting if NAME string matches already declared
special variable name or not. And returns appropriate face name."
  `(if (ignore-errors (let ((sym (intern-soft ,name)))
                        (and sym
                             (special-variable-p sym))))
       pospcase-font-lock-special-variable-name-face
     font-lock-variable-name-face))

(defun pospcase-font-lock-match-quote-and-backquote (limit)
  "Matcher for quote and backquote. `match-data' 1 is backquote."
  (let (res)
    (while (and (setq res (re-search-forward "\\(?:\\(`\\)\\|'\\)" limit t))
                (pospcase-font-lock-is-in-comment-or-string
                 (match-beginning 0))))
    res))

(defun pospcase-font-lock-match-quoted-content (limit)
  "Submatcher for quote and backquote."
  (and (< (point) limit)
       (let ((p (point))
             res)
         (while
             (progn
               (setq res (re-search-forward "\\(,@?\\|[`']\\)" limit t))
               (and res
                    (pospcase-font-lock-is-in-comment-or-string
                     (match-beginning 0)))))
         (if res
             ;; Match up to next quoted subpart or comma operator.
             (let ((is-comma (eq (char-after (match-beginning 0)) ?,)))
               (set-match-data (list
                                ;; Match data 0: Full match.
                                p (match-end 0)
                                ;; Match data 1: Part of the quoted expression
                                p
                                (match-beginning 0)
                                ;; Match data 2; Comma operator (if present)
                                (and is-comma (match-beginning 0))
                                (and is-comma (match-end 0))))
               (condition-case nil
                   (forward-sexp)
                 (error (goto-char limit))))
           ;; Match to the end of the quoted expression.
           (set-match-data (list p limit
                                 p limit))
           (goto-char limit))
         t)))

(defvar pospcase-font-lock-loop-keywords
  (concat
   "\\_<"
   "\\("
   (regexp-opt
    '("=" "above" "across" "across-ref" "always" "and" "append" "as"
      "being" "below" "buffer" "buffers" "by"
      "collect" "collecting" "concat" "count"
      "do" "doing" "downfrom" "downto"
      "each" "element" "elements" "else" "end"
      "extent" "extents" "external-symbol" "external-symbols"
      "finally" "frames" "from"
      "hash-key" "hash-keys" "hash-value" "hash-values"
      "if" "in" "in-ref" "initially" "interval" "intervals"
      "key-binding" "key-bindings" "key-code" "key-codes" "key-seq" "key-seqs"
      "maximize" "minimize"
      "named" "nconc" "nconcing" "never"
      "of" "of-ref" "on" "overlay" "overlays"
      "present-symbol" "present-symbols" "property"
      "repeat" "return"
      "screen" "screens" "sum" "symbol" "symbols"
      "the" "then" "thereis" "to"
      "unless" "until" "upfrom" "upto" "using"
      "vconcat"
      "when" "while" "windows"))
   "\\)"
   "\\_>")
  "Regexp string which matches `loop' and `cl-loop' named
  parameters.")

(defun pospcase-font-lock-match-loop-keywords (limit)
  "Submatcher for `loop' keywords."
  (while
      (progn
        (forward-comment most-positive-fixnum)
        (and (< (point) limit)
             (not (looking-at pospcase-font-lock-loop-keywords))))
    (condition-case nil
        (forward-sexp)
      (error (goto-char limit))))
  (if (not (< (point) limit))
      nil
    (goto-char (match-end 0))
    t))

(defun pospcase-font-lock-is-in-comment-or-string (pos)
  "Return non-nil if POS is in a comment, string, constant, or
#prefixed. "
  (or (let ((table (save-excursion (syntax-ppss pos))))
        (or (nth 3 table)
            (nth 4 table)))
      (memq (char-before pos) '(?\\ ?? ?#))))


;;; `lisp-mode' and `emacs-lisp-mode' font-lock

(defvar pospcase-font-lock-lisp-extra-keywords
  ;;
  ;; non-`pcase' powered keywords
  ;;
  (let* ((symbol-start "\\_<")
         (symbol-end "\\_>")
         (symbol (concat symbol-start
                         "\\(?:\\sw\\|\\s_\\|\\\\.\\)+"
                         symbol-end)))
    (cl-flet ((regexp-or (&rest exps)
                         (concat "\\(?:"
                                 (mapconcat #'identity exps "\\|")
                                 "\\)")))
      `(;; For `cl-loop'
        (,(concat "("
                  (regexp-opt '("loop" "cl-loop"))
                  "\\_>")
         (pospcase-font-lock-match-loop-keywords
          ;; Pre-match form
          (progn
            (goto-char (match-end 0))
            (save-excursion
              (goto-char (match-beginning 0))
              (ignore-errors (scan-sexps (point) 1))))
          ;; Post-match form.
          (goto-char (match-end 0))
          ;; Faces
          (1 font-lock-builtin-face)
          (2 ,(pospcase-font-lock-variable-face-form '(match-string 2)) nil t)))
        (;; For quote and backquote
         ;;
         ;; Matcher: Set match-data 1 if backquote.
         pospcase-font-lock-match-quote-and-backquote
         (1 pospcase-font-lock-backquote-face nil t)
         (;; Submatcher, match part of quoted expression or comma.
          pospcase-font-lock-match-quoted-content
          ;; Pre-match form. Value of expression is limit for submatcher.
          (progn
            (goto-char (match-end 0))
            ;; Search limit
            (ignore-errors (scan-sexps (point) 1)))
          ;; Post-match form
          (goto-char (match-end 0))
          ;; Faces
          (1 pospcase-font-lock-quoted-face append)
          (2 pospcase-font-lock-backquote-face nil t)))
        ;; For function read syntax
        (,(concat "#'\\("
                  symbol
                  "\\)")
         1 pospcase-font-lock-quoted-function-face)
        ;; For `defclass' slots
        (,(concat
           (regexp-opt '(":accessor" ":constructor" ":copier" ":predicate"
                         ":reader" ":0writer" ":print-function" ":print-object"))
           "[ \t\n]+"
           "\\("
           symbol
           "\\)")
         (1 font-lock-function-name-face)))))
  "List of extra font lock keywords for Lisp. The highest
  fontification priority.")


(defun pospcase-font-lock-lisp-init ()
  "Setup various eye candy font lock keywords for Common Lisp and Emacs Lisp."
  (pospcase-font-lock 'lisp-mode
                      '(`(defun (setf ,name) ,args . ,_)
                        `(defun ,name ,args . ,_)
                        `(defsubst ,name ,args . ,_)
                        `(define-method-combination ,name ,args . ,_)
                        `(define-condition ,name ,args . ,_)
                        `(define-setf-expander ,name ,args . ,_)
                        `(define-compiler-macro ,name ,args . ,_)
                        `(define-inline ,name ,args . ,_)
                        `(define-modify-macro ,name ,args . ,_)
                        `(cl-defun (setf ,name) ,args . ,_)
                        `(cl-defun ,name ,args . ,_)
                        `(cl-defsubst ,name ,args . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-function-name-face))
                        ((args . list/1) .
                         ((pospcase-font-lock-variable-face-form (match-string 3))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(deftype ,name ,args . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-type-face))
                        ((args . list/1) .
                         ((pospcase-font-lock-variable-face-form (match-string 3))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defclass ,name ,supers ,slots . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-type-face))
                        ((supers . list/1) . (font-lock-type-face))
                        ((slots . list/1) . (font-lock-variable-name-face))))
  (pospcase-font-lock 'lisp-mode
                      '(`(lambda ,args . ,_)
                        `(with-slots ,args . ,_)
                        `(with-accessors ,args . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((args . list/1) .
                         ((pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(let ,binds . ,_)
                        `(let* ,binds . ,_)
                        `(multiple-value-bind ,binds . ,_)
                        `(cl-multiple-value-bind ,binds . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((binds . list/1) .
                         ((pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(symbol-macrolet ,binds . ,_)
                        `(cl-symbol-macrolet ,binds . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((binds . list/2) .
                         ((pospcase-font-lock-variable-face-form (match-string 2))
                          font-lock-constant-face))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defmethod (setf ,name) ,(pred keywordp) ,args . ,_)
                        `(defmethod (setf ,name) ,args . ,_)
                        `(defmethod ,name ,(pred keywordp) ,args . ,_)
                        `(defmethod ,name ,args . ,_)
                        `(defgeneric ,name ,args . ,_)
                        `(cl-defmethod (setf ,name) ,(pred keywordp) ,args . ,_)
                        `(cl-defmethod (setf ,name) ,args . ,_)
                        `(cl-defmethod ,name ,(pred keywordp) ,args . ,_)
                        `(cl-defmethod ,name ,args . ,_)
                        `(cl-defgeneric ,name ,args . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-function-name-face))
                        ((args . list/2) .
                         ((pospcase-font-lock-variable-face-form (match-string 3))
                          font-lock-type-face))))
  (pospcase-font-lock 'lisp-mode
                      '(`(destructuring-bind ,binds . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((binds . destructuring) .
                         ((pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defmacro ,name ,args . ,_)
                        `(cl-defmacro ,name ,args . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-function-name-face))
                        ((args . destructuring) .
                         ((pospcase-font-lock-variable-face-form (match-string 3))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(flet ,funs . ,_)
                        `(labels ,funs . ,_)
                        `(cl-flet ,funs . ,_)
                        `(cl-labels ,funs . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((funs . flet) .
                         (font-lock-function-name-face
                          (pospcase-font-lock-variable-face-form (match-string 3))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(macrolet ,macros . ,_)
                        `(cl-macrolet ,macros . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((macros . macrolet) .
                         (font-lock-function-name-face
                          (pospcase-font-lock-variable-face-form (match-string 3))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(setq . ,binds)
                        `(setf . ,binds))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((binds . setq) .
                         ((pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defstruct (,name . ,_) ,(pred stringp) . ,slots)
                        `(defstruct (,name . ,_) . ,slots)
                        `(defstruct ,name ,(pred stringp) . ,slots)
                        `(defstruct ,name . ,slots)
                        `(cl-defstruct (,name . ,_) ,(pred stringp) . ,slots)
                        `(cl-defstruct (,name . ,_) . ,slots)
                        `(cl-defstruct ,name ,(pred stringp) . ,slots)
                        `(cl-defstruct ,name . ,slots))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-type-face))
                        ((slots . defstruct) .
                         (font-lock-variable-name-face))))
  (pospcase-font-lock 'lisp-mode
                      '(&key &aux &optional)
                      '((heading-keyword .
                                         (font-lock-builtin-face)) ; a hack for &key ((:key var) init sup)
                        ((pospcase--dummy . parameter) .
                         ((pospcase-font-lock-variable-face-form (match-string 2))
                          default
                          default))))
  (pospcase-font-lock 'lisp-mode
                      '(for :for index :index into :into with :with)
                      '((heading-keyword . (font-lock-builtin-face))
                        ((pospcase--dummy . loop) .
                         ((pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(dolist (,var . ,_) . ,_)
                        `(dotimes (,var . ,_) . ,_)
                        `(with-open-file (,var . ,_) . ,_)
                        `(with-open-stream (,var . ,_) . ,_)
                        `(with-output-to-string (,var . ,_) . ,_)
                        `(with-input-from-string (,var . ,_) . ,_)
                        `(cl-dolist (,var . ,_) . ,_)
                        `(cl-dotimes (,var . ,_) . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (var . ((pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(condition-case ,var . ,_)
                        `(define-symbol-macro ,var . ,_)
                        `(define-special ,var . ,_)
                        `(define-interactive-keymap ,var . ,_)
                        `(define-stumpwm-type ,var . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (var . ((pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defvar ,var . ,_)
                        `(defconstant ,var . ,_)
                        `(defparameter ,var . ,_)
                        `(defcustom ,var . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (var . (font-lock-variable-name-face)))))


;;;###autoload
(defun pospcase-font-lock-lisp-setup ()
  "Enable `pospcase' code highlighting for `lisp-mode' and `emacs-lisp-mode'."
  (interactive)
  (pospcase-font-lock-lisp-init)
  (add-hook 'lisp-mode-hook #'pospcase-font-lock-lisp-keywords-add)
  (add-hook 'emacs-lisp-mode-hook #'pospcase-font-lock-lisp-keywords-add))


(provide 'pospcase-font-lock)
