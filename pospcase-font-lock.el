;;; pospcase-font-lock.el: S-expression font-lock -*- lexical-binding: t -*-

(require 'pospcase)

;; Non `pcase' powered code.
;; Stolen from lisp-extra-font-lock, for a historical reason

(defcustom pospcase-font-lock-loop-functions
  '("loop"
    "cl-loop")
  "List of functions using same syntax as `loop' to bind variables.."
  :type '(repeat string)
  :group 'pospcase-font-lock)

(defcustom pospcase-font-lock-special-variable-name-face
  'font-lock-warning-face
  "The face used to highlight special variables bound by `let'.

A special variable is a global variable defined by `defvar'. See
`special-variable-p' for details.

To disable this highlighting, set this to nil. To highlight
special variables like plain variables, set this to
`font-lock-variable-name-face'."
  :type '(choice (const nil)
                 face)
  :group 'pospcase-font-lock)

(defcustom pospcase-font-lock-quoted-face
  'font-lock-constant-face
  "The face used to highlight quoted expressions.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'pospcase-font-lock)

(defcustom pospcase-font-lock-backquote-face
  'font-lock-warning-face
  "The face used to highlight backquotes and the comma operator.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'pospcase-font-lock)

(defcustom pospcase-font-lock-quoted-function-face
  'font-lock-function-name-face
  "The face used to highlight #'-quoted function symbols.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'pospcase-font-lock)

(defun pospcase-font-lock-variable-face-form (name)
  "A form suitable for a font-lock face expression.

NAME is a form that should evalute to the name of the symbol, as a string."
  `(if (ignore-errors (let ((symbol (intern-soft ,name)))
                        (and symbol
                             (special-variable-p symbol))))
       pospcase-font-lock-special-variable-name-face
     font-lock-variable-name-face))

(defun pospcase-font-lock-match-quote-and-backquote (limit)
  "Search for quote and backquote in in code.
Set match data 1 if character matched is backquote."
  (let (res)
    (while
        (progn (setq res (re-search-forward "\\(?:\\(`\\)\\|'\\)" limit t))
               (and res
                    (pospcase-font-lock-is-in-comment-or-string
                     (match-beginning 0)))))
    res))

(defun pospcase-font-lock-match-quoted-content (limit)
  "Match next part of a quoted content.

Match up to next comma operator or quoted subexpression, or to
the end of the quoted expression."
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
      "when" "while" "windows"
      "for" "index" "into" "with"))
   "\\)"
   "\\_>")
  "Regexp of `cl-loop' named parameters, excluding variable binding ones.")

(defun pospcase-font-lock-match-loop-keywords (limit)
  "Match named keyword of `loop' and highlight variable arguments."
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
  "Return non-nil if POS is in a comment, string, constant, or reader macro.

This assumes that Font Lock is active and has fontified comments
and strings."
  (or (let ((props (text-properties-at (point)))
            (faces '()))
        (while props
          (let ((pr (pop props))
                (value (pop props)))
            (if (eq pr 'face)
                (setq faces value))))
        (unless (listp faces)
          (setq faces (list faces)))
        (or (memq 'font-lock-comment-face faces)
            (memq 'font-lock-string-face faces)
            (memq 'font-lock-doc-face faces)))
      ;; Plain character constant ?<char>.
      (eq (char-before pos) ??)
      ;; Escaped character constant ?\<char>.
      (and (eq (char-before pos) ?\\)
           (eq (char-before (- pos 1)) ??))
      ;; Reader macro like #'.
      (eq (char-before pos) ?#)))



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
    (goto-char limit)
    (set-match-data nil)
    (setq pospcase--iterating nil)
    nil))

(defun pospcase-match-nil (limit)
  "Simply set `pospcase--prematches' to `pospcase--matches.'"
  (if pospcase--prematches
      (progn
        (setq pospcase--matches (list pospcase--prematches)
              pospcase--prematches nil)
        (pospcase--iterator limit))
    nil))

(defmacro pospcase--call-iterator (clause limit &optional allow-atom-p)
  "Catch parsing error, and call `pospcase--iterator'."
  `(condition-case nil
       (when (< (point) ,limit)
         (unless (or pospcase--ignore
                     pospcase--matches
                     pospcase--iterating)
           (let ((temp (ignore-errors
                         (read-from-string
                          (pospcase--buffer-substring (point) ,limit)))))
             (setq pospcase--matches
                   (cond
                    ((null temp) nil)
                    ((and (consp temp)
                          (or (null (car temp)) ; empty list at `point'
                              ,(if allow-atom-p
                                   nil
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

(defun pospcase-match-list/3 (limit)
  "Matcher iterator for a list of symbol, two or three length lists."
  (pospcase--call-list-iterator (`(,name ,init ,sup) (pospcase--list name init sup))
                                (`(,name ,init) (pospcase--list name init))
                                (`,name (pospcase--list name))))

(defalias #'pospcase-match-parameter #'pospcase-match-list/3)

(defun pospcase-match-list/1 (limit)
  "Matcher iterator for a symbol or `car's of a list of lists"
  (pospcase--call-list-iterator (`(,name . ,_) (pospcase--list name))
                                (`,name (pospcase--list name))))

(defalias #'pospcase-match-defstruct #'pospcase-match-list/1)

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
                   (let ((arglist (car (pospcase-read (point)))))
                     ,clause)))))
    limit))

(defun pospcase-match-flet (limit)
  "Matcher iterator for a list of `flet' bindings."
  (pospcase--call-flet-iterator (if arglist
                                    (mapcar
                                     (lambda (exp)
                                       (list name
                                             (pospcase exp '((`(,arg . ,_) arg)
                                                             (`,arg arg)))))
                                     arglist)
                                  (list (list name)))))

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
                                           (list (pospcase--list (cdr temp))))
             else unless (ignore-p (car temp)) do (setq
                                                   result
                                                   (append
                                                    result
                                                    (pospcase-collect-all-symbols
                                                     (car temp)))))))

(defun pospcase-match-destructuring (limit)
  "Matcher iterator for symbols in an arbitrarily nested list."
  (pospcase--call-iterator
   (pospcase-collect-all-symbols (pospcase-read (point)))
   limit
   t))

;; alternate implementation without `pospcase-read'. only collects symbols
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
  (if pospcase--matches
      (pospcase--iterator limit)
    (if (and (< (point) limit)
             (memq (save-excursion
                     (backward-up-list)
                     (down-list)
                     (read (current-buffer)))
                   '(loop cl-loop)))
        (pospcase-match-destructuring limit)
      nil)))

(defun pospcase-match-macrolet (limit)
  "Matcher iterator for a lit of `macrolet' bindings"
  (pospcase--call-flet-iterator (if (car arglist)
                                    (mapcar (lambda (arg) (cons name arg))
                                            (pospcase-collect-all-symbols arglist))
                                  (list (list name)))))

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


(defvar pospcase-font-lock-lisp-keywords
  ;;
  ;; non-`pcase' powered
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
                  (regexp-opt pospcase-font-lock-loop-functions)
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
  "List of font lock keywords for Lisp.")

(defvar-local pospcase-font-lock-lisp-local-keywords nil
  "List of font lock buffer local keywords for Lisp.")

(defvar-local pospcase-font-lock-lisp-keywords--installed nil
  "BUffer local list for `pospcase-font-lock-lisp-mode'.")

(defun pospcase-font-lock-lisp-keywords-add ()
  "Activate `pcase' based font lock for Lisp modes."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'pospcase-font-lock-lisp-keywords--installed)
    (font-lock-remove-keywords nil pospcase-font-lock-lisp-keywords--installed))
  (set (make-local-variable 'pospcase-font-lock-lisp-keywords--installed)
       (append pospcase-font-lock-lisp-local-keywords
               pospcase-font-lock-lisp-keywords))
  (font-lock-add-keywords nil
                          pospcase-font-lock-lisp-keywords--installed
                          'append))

(defun pospcase-font-lock-lisp-keywords-remove ()
  "Deactivate `pcase' based font lock for Lisp modes."
  (font-lock-remove-keywords nil pospcase-font-lock-lisp-keywords--installed))

(defvar pospcase--dummy nil
  "Used in a hack for empty `multiple-value-bind'.")


(defvar pospcase-list-group '(list/2 list/1 destructuring flet macrolet)
  "Submatchers with same behavior of `pospcase-match-list/2'.")
(defvar pospcase-defstruct-group '(defstruct)
  "Submatchers with same behavior of `pospcase-match-defstruct'.")
(defvar pospcase-parameter-group '(parameter)
  "Submatchers with same behavior of `pospcase-match-parameter'.")
(defvar pospcase-loop-group '(loop)
  "Submatchers with same behavior of `pospcase-match-loop'.")

(defun pospcase-font-lock-build (patterns specs)
  "Actual font lock keywords generator. Deep magic is
involved. Don't dismay. I'm planning to simplify and to supply it
with better comments."
  (let* ((keywords (mapcar (lambda (pattern)
                             (let ((str (prin1-to-string
                                         (if (consp pattern)
                                             (if (memq (car pattern) '(\` \, quote))
                                                 (cadr pattern)
                                               (car pattern))
                                           pattern))))
                               (string-match "^\\S +" str)
                               (match-string 0 str)))
                           patterns))
         (keyword (concat "\\_<"
                          (regexp-opt
                           (mapcar (lambda (kw) (substring kw (string-match "[^(]" kw)))
                                   keywords))
                          "\\_>"))
         (matcher (regexp-opt keywords))
         (submatchers (cl-loop for spec in (cdr specs)
                               if (consp (car spec)) collect (car spec)))
         (subvars (mapcar #'car submatchers))
         (vars (mapcar (lambda (spec) (if (consp (car spec)) (caar spec) (car spec)))
                       (cdr specs)))
         (non-subvars (cl-remove-if (lambda (var) (memq var subvars)) vars)))
    (if (string-match "," matcher)
        (error "In-middle keyword is not supported.")
      (setq matcher (concat matcher "\\_>\\s *")))
    (append
     (if (car specs)
         (list `(,keyword . ,(car specs)))
       nil)
     (mapcar
      (lambda (submatcher)
        `(,matcher
          (,(intern (concat "pospcase-match-" (symbol-name (cdr submatcher))))
           (pospcase--preform
            (goto-char (match-beginning 0))

            (cl-flet ((ignore-p ()
                                (let ((table (syntax-ppss)))
                                  (or (nth 3 table)     ; in string
                                      (nth 4 table))))) ; in comment
              (if (ignore-p)
                  (progn
                    (while (and (not (eobp)) (ignore-p))
                      (forward-char))
                    (forward-comment most-positive-fixnum)
                    (point))
                (condition-case nil
                    (multiple-value-bind ,vars (pospcase-at
                                                (point)
                                                ',(mapcar
                                                   (lambda (pat)
                                                     (list pat (cons 'values vars)))
                                                   patterns))
                      ,(unless (null non-subvars)
                         `(setq pospcase--prematches ,(cons 'list non-subvars)))

                      ,(cond
                        ((memq (cdr submatcher) pospcase-list-group)
                         `(progn
                            (goto-char (car ,(car submatcher)))
                            (cdr ,(car submatcher))))

                        ((memq (cdr submatcher) pospcase-defstruct-group)
                         `(progn
                            (setq pospcase--fence-start
                                  (ignore-errors (pospcase-read (car ,(car submatcher)))))
                            (condition-case nil
                                (save-excursion
                                  (goto-char (match-end 0))
                                  (up-list)
                                  (point))
                              (error (1+ (goto-char (1- (match-end 0))))))))

                        ((memq (cdr submatcher) pospcase-parameter-group)
                         '(let ((end (match-end 0)))
                            (if (memq (char-before (point))
                                      '(?\\ ?\' ?\` ?\,)) ; when keyword is use as symbol
                                (progn
                                  (setq pospcase--ignore t)
                                  (1+ (goto-char (1- end))))
                              (setq pospcase--fence-start
                                    (ignore-errors (pospcase-read end)))
                              (condition-case nil
                                  (prog1
                                      (save-excursion
                                        (up-list)
                                        (point))
                                    (backward-up-list))
                                (error (1+ (goto-char (1- end))))))))

                        ((memq (cdr submatcher) pospcase-loop-group)
                         '(let ((end (match-end 0)))
                            (if (memq (char-before (point))
                                      '(?\\ ?\' ?\` ?\,)) ; when keyword is use as symbol
                                (progn
                                  (setq pospcase--ignore t)
                                  (1+ (goto-char (1- end))))
                              (goto-char end)
                              (setq pospcase--fence-start
                                    (ignore-errors (pospcase-read end)))
                              (condition-case nil
                                  (scan-sexps (point) 1)
                                (error (1+ (goto-char (1- end))))))))

                        ((null submatcher)
                         '(condition-case nil
                              (scan-sexps (point) 1)
                            (error (1+ (goto-char (1- (match-end 0)))))))

                        (t (error "Not supported submatcher: %s" submatcher))))

                  (error (goto-char (match-end 0)))))))

           (pospcase--postform)

           ;; fontspecs
           ,@(cl-loop with i = 0
                      for spec in (cdr specs)
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
          '(nil))))))                   ; no submatcher

(defun pospcase-font-lock (mode patterns specs &optional buffer-local-p)
  "Font lock keywords generator with `pcase' powered pattern
matching. Currently you can use sub-matchers:

Group one: list/2, list/1, destructuring, flet, macrolet

Group two:  defstruct

Group three: key

So far sufficient portions of Common Lisp and Emacs Lisp could be
highlighted using them.

Argument PATTERNS is a list of `pcase' patterns.

And SPECS is a list of slightly extended fontspec.

The first element is the face name of the heading keyword.

The rest is structured like:

  (match-name . (list of font-face-name)) or
  ((match-name . sub-matcher) . (list of font-face-name))

See the code of `pospcase-font-lock-lisp-setup' for working
examples."
  (let* ((id (let ((split (split-string (symbol-name mode) "-mode")))
               (if (null (cdr split))
                   (error "%s is not a mode name." mode)
                 (car split))))
         (container (intern (format "pospcase-font-lock-%s%s-keywords"
                                    id
                                    (if buffer-local-p "-local" ""))))
         (keywords (pospcase-font-lock-build patterns specs)))
    (unless (boundp container)
      (eval `(progn
               (,(if buffer-local-p 'defvar-local 'defvar)
                ,container
                nil
                ,(format "List of font lock %skeywords for %s."
                         (if buffer-local-p "buffer local " "")
                         (capitalize id))))))
    (mapc (lambda (keyword)
            (unless (member keyword (symbol-value container))
              (set container (cons keyword (symbol-value container)))))
          keywords)))

(defun pospcase-font-lock-lisp-init ()
  "Setup various eye candy font lock keywords for Common Lisp and Emacs Lisp."
  (pospcase-font-lock 'lisp-mode
                      '(`(defun (setf ,name) ,args . ,_)
                        `(defun ,name ,args . ,_)
                        `(defsubst ,name ,args . ,_)
                        `(define-inline ,name ,args . ,_)
                        `(define-modify-macro ,name ,args . ,_)
                        `(cl-defun (setf ,name) ,args . ,_)
                        `(cl-defun ,name ,args . ,_)
                        `(cl-defsubst ,name ,args . ,_))
                      '(font-lock-keyword-face
                        (name . (font-lock-function-name-face))
                        ((args . list/1) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defclass ,name ,supers ,slots . ,_))
                      '(font-lock-keyword-face
                        (name . (font-lock-type-face))
                        ((supers . list/1) . (font-lock-type-face))
                        ((slots . list/1) . (font-lock-variable-name-face))))
  (pospcase-font-lock 'lisp-mode
                      '(`(lambda ,args . ,_)
                        `(with-slots ,args . ,_)
                        `(with-accessors ,args . ,_))
                      '(font-lock-keyword-face
                        ((args . list/1) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(let ,binds . ,_)
                        `(let* ,binds . ,_)
                        `(multiple-value-bind ,binds . ,_)
                        `(cl-multiple-value-bind ,binds . ,_))
                      '(font-lock-keyword-face
                        ((binds . list/1) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(symbol-macrolet ,binds . ,_)
                        `(cl-symbol-macrolet ,binds . ,_))
                      '(font-lock-keyword-face
                        ((binds . list/2) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))
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
                      '(font-lock-keyword-face
                        (name . (font-lock-function-name-face))
                        ((args . list/2) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))
                          font-lock-type-face))))
  (pospcase-font-lock 'lisp-mode
                      '(`(destructuring-bind ,binds . ,_))
                      '(font-lock-keyword-face
                        ((binds . destructuring) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defmacro ,name ,args . ,_)
                        `(cl-defmacro ,name ,args . ,_))
                      '(font-lock-keyword-face
                        (name . (font-lock-function-name-face))
                        ((args . destructuring) .
                         ((pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(flet ,funs . ,_)
                        `(labels ,funs . ,_)
                        `(cl-flet ,funs . ,_)
                        `(cl-labels ,funs . ,_))
                      '(font-lock-keyword-face
                        ((funs . flet) .
                         (font-lock-function-name-face
                          (pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(macrolet ,macros . ,_)
                        `(cl-macrolet ,macros . ,_))
                      '(font-lock-keyword-face
                        ((macros . macrolet) .
                         (font-lock-function-name-face
                          (pospcase-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defstruct (,name . ,_) ,(pred stringp) ,first . ,_)
                        `(defstruct (,name . ,_) ,first . ,_)
                        `(defstruct ,name ,(pred stringp) ,first . ,_)
                        `(defstruct ,name ,first . ,_)                        
                        `(cl-defstruct (,name . ,_) ,(pred stringp) ,first . ,_)
                        `(cl-defstruct (,name . ,_) ,first . ,_)
                        `(cl-defstruct ,name ,(pred stringp) ,first . ,_)
                        `(cl-defstruct ,name ,first . ,_))
                      '(font-lock-keyword-face
                        (name . (font-lock-type-face))
                        ((first . defstruct) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(&key &aux &optional)
                      '(font-lock-type-face
                        ((pospcase--dummy . parameter) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))
                          default
                          default))))
  (pospcase-font-lock 'lisp-mode
                      '(for :for index :index into :into with :with)
                      '(nil
                        ((pospcase--dummy . loop) .
                         ((pospcase-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(dolist (,var . ,_) . ,_)
                        `(dotimes (,var . ,_) . ,_)
                        `(with-open-file (,var . ,_) . ,_)
                        `(with-open-stream (,var . ,_) . ,_)
                        `(with-output-to-string (,var . ,_) . ,_)
                        `(with-input-from-string (,var . ,_) . ,_)
                        `(cl-dolist (,var . ,_) . ,_)
                        `(cl-dotimes (,var . ,_) . ,_))
                      '(font-lock-keyword-face
                        (var . ((pospcase-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(condition-case ,var . ,_)
                        `(define-symbol-macro ,var . ,_)
                        `(defvar* ,var . ,_)
                        `(defconstant* ,var . ,_)
                        `(defparameter* ,var . ,_)
                        `(define-special ,var . ,_)
                        `(define-interactive-keymap ,var . ,_)
                        `(define-stumpwm-type ,var . ,_))
                      '(font-lock-keyword-face
                        (var . ((pospcase-font-lock-variable-face-form (match-string 1)))))))


;;;###autoload
(defun pospcase-font-lock-lisp-setup ()
  "Enable `pospcase' code highlighting for `lisp-mode' and `emacs-lisp-mode'."
  (interactive)
  (pospcase-font-lock-lisp-init)
  (add-hook 'lisp-mode-hook #'pospcase-font-lock-lisp-keywords-add)
  (add-hook 'emacs-lisp-mode-hook #'pospcase-font-lock-lisp-keywords-add))


(provide 'pospcase-font-lock)
