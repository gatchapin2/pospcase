;;; Should I get rid of this dependency?
(require 'lisp-extra-font-lock)


;;; `pcase' powered matcher codes

(defun pospcase-read (pos)
  "Read a s-expression at POS. Recursively attach positional
metadata (start . end) at cdr of each s-expression.

For example, when reading from a buffer with contents:

  (defun foo (bar)
    (if bar
        baz
      (quux)))

declared at the beginning.

Evaluating:

  (pospcase-read (point-min))

returns:

  (((defun 2 . 7)
    (foo 8 . 11)
    (((bar 13 . 16))
     12 . 17)
    (((if 21 . 23)
      (bar 24 . 27)
      (baz 34 . 37)
      (((quux 43 . 47))
       42 . 48))
     20 . 49))
   1 . 50)

You can directly use it like:

  (cl-labels
      ((collect (node)
                (if (symbolp (car node))
                    (list (list (cdr node))) ; doubly wrapped for
                                             ; `pospcase--iterator'
                                             ; consumption
                  (cl-loop for child in (car node)
                           append (collect child)))))
    (collect (pospcase-read (point-min))))

to collect every occurring symbol positions (simplified version of
`pospcase-collect-all-symbols'):

  (((2 . 7)) ((8 . 11)) ((13 . 16))
   ((21 . 23)) ((24 . 27))
   ((34 . 37))
   ((43 . 47)))

Or let `pospcase' or `pospcase-at' use it indirectly like:

  (pospcase-at
   (point-min)
   '( ; quote and extra parenthesis are necessary because
      ; `pospcase' and `pospcase-at' are functions, unlike `pcase'
     (`(defun ,name ,args . ,body)
      (list name args body))))

Which returns:

  ((8 . 11)             ; foo
   (12 . 17)            ; (bar)
   ((((if 21 . 23)      ; (if bar baz (quux))
      (bar 24 . 27)
      (baz 34 . 37)     ; Note dot notation matches the rest of
      (((quux 43 . 47)) ; s-expression for a descent technical reason.
       42 . 48))        ; Use `pospcase-pos' to get positional metadata
     20 . 49)))         ; when using dot notation for arbitrary tail match.

You can reparse trees returned by `pospcase-read' or dot notation
of `pospcase':

  (pospcase
   (car                 ; dot notation returns with extra list wrap
    (pospcase-at (point-min)
                 '((`(defun ,_ ,_ . ,temp) temp))))
   '((`,body body)))

which returns:

  (20 . 49)"
  (cl-labels
      ((walk (limit)
             (destructuring-bind (start sexp . lim)
                 (cons (point)
                       (read-from-string
                        (buffer-substring-no-properties (point) limit)))
               (incf lim (point))
               (forward-comment lim)
               (cons
                (if (or (atom sexp)
                        (memq (car sexp) '(\` \, quote function)))
                    sexp
                  (down-list)
                  (cl-loop
                   with str
                   with rpair
                   with dot
                   with temp
                   with result
                   do (setq str (progn
                                  (forward-comment lim)
                                  (buffer-substring-no-properties (point) lim))
                            rpair
                            (condition-case err
                                (read-from-string str)
                              (invalid-read-syntax
                               (when (string= (cadr err) ".")
                                 (setq dot t
                                       str (progn
                                             (forward-sexp)
                                             (forward-comment lim)
                                             (buffer-substring-no-properties (point) lim)))
                                 (read-from-string str))))
                            rlim (+ (point) (cdr rpair))
                            temp (if (or (atom (car rpair))
                                         (memq (car rpair) '(\` \, quote function)))
                                     (cons (car rpair) (cons (point) rlim))
                                   (save-excursion
                                     (walk rlim))))
                   if dot
                   do (setq result (cons result temp))
                   else
                   do (setq result (append result (list temp)))
                   until (progn
                           (ignore-errors (forward-sexp))
                           (forward-comment lim)
                           (while (> (skip-chars-forward ")") 0)
                             (forward-comment lim))
                           (>= (point) lim))
                   finally return result))
                (cons start lim)))))
    (save-excursion
      (goto-char pos)
      (walk (scan-sexps (point) 1)))))

(defmacro pospcase-translate (matcher)
  "Translate `pcase' matcher pattern (usually backquoted) to
matcher pattern consumable for `pospcase'. Beware nested
backquotes are not supported."
  (cl-labels
      ((meta-pos-symbol (sym)
                        (list '\,
                              (intern (concat
                                       (symbol-name (cadr node))
                                       "-meta-pos"))))
       (walk (node)
               (if (consp node) ; note that in elisp `,foo' is `(\, foo)'
                   (cond
                    ((eq (car node) 'quote)
                     (cons node ',_))
                    ((eq (car node) '\,)
                     ;; ,\\= is NOT supported since it
                     ;; requires fetching and unwrapping
                     ;; next (quote foo) into foo.
                     (if (or (eq (cadr node) '_)
                             (and
                              (consp (cadr node))
                              (memq (caadr node) '(or and pred guard let app))))
                         (cons node ',_)
                       (cons node (meta-pos-symbol (cadr node)))))
                    (t (cons
                        (cl-loop
                         with result
                         do (cond
                             ((and (atom (cdr node))
                                   (cdr node))
                              (cl-return
                               (cons
                                (append result (list (walk (car node))))
                                (walk (cdr node)))))
                             ((memq (car node) '(\` \, quote function))
                              (setq result (append
                                            result
                                            (if ; cdr cell ,foo matches the rest of a list
                                                (and (null (cddr node))
                                                     (symbolp (cadr node)))
                                                (meta-pos-symbol (cadr node))
                                              (walk (list (car node) (cadr node)))))
                                    node (cddr node)))
                             ((or (car node)
                                  (cdr node))
                              (setq result (append result (list (walk (car node))))
                                    node (cdr node)))
                             (t (cl-return result))))
                        ',_)))
                 (cons node ',_))))
    (if (consp matcher)
        (case (car matcher)
          ('quote (list 'quote (list '\` (cons matcher ',_))))
          ('\` (list 'quote (list '\` (walk (cadr matcher)))))
          (otherwise (list 'quote (walk matcher))))
      `,(list 'quote (list '\` (cons matcher ',_))))))

(defun pospcase (exp cases)
  "`pcase' variant for getting positional metadata."
  (eval
   `(pcase
        ,(list 'quote exp)
      ,@(mapcar
         (lambda (case)
           (list
            (eval `(pospcase-translate ,(car case)))
            (let (symbols)
              (cl-labels
                  ((collect (node)
                            (cond
                             ((and (listp node)
                                   (eq (car node) '\,)
                                   (not (eq (cadr node) '_)))
                              (setq symbols (cons (cadr node) symbols)))
                             ((consp node)
                              (collect (car node))
                              (collect (cdr node)))))
                   (translate (node)
                              (cond
                               ((memq node symbols)
                                (intern (concat (symbol-name node) "-meta-pos")))
                               ((consp node) (cons
                                              (translate (car node))
                                              (translate (cdr node))))
                               (t node))))
                (when (consp (car case))(collect (cadar case)))
                (translate (cadr case))))))
         cases))))

(defun pospcase-at (pos cases)
  "`pospcase' at position POS of buffer."
  (pospcase (pospcase-read pos) cases))

(defun pospcase-pos (match)
  "Extract positional data from a tree returned by
`pospcase-read' or dot notation pattern within `pospcase' or
`pospcase-at'."
  (when match
    (cond
     ((numberp (cdr match))                        ; (START . END)
      match)
     ((and (consp (cdr (car match)))
           (numberp (cddr (car match))))           ; ((SEXP START . END)
                                                   ;  ... (SEXP START . END))
      (cons (cadr (car match))
            (cddr (car (last match)))))
     (t
      (cons (if (numberp (car (car match)))        ; ((START . END) ...)
                (car (car match))
              (cadar (car match)))                 ; (((SEXP START . END) ...) ...)
            (if (numberp (cdr (car (last match)))) ; (... (START . END))
                (cdr (car (last match)))
              (cddr (car (last (car (last match)))))))))))


;;; font-lock specific codes

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
  (append pospcase--prematches matches))

(defun pospcase--iterator (limit)
  "Actual iterator."
  (if pospcase--matches
      (let ((mlist (cl-loop for pair in (car pospcase--matches)
                            append (-cons-to-list pair))))
        (set-match-data (append (list pospcase--match-beginning
                                      limit)
                                mlist))
        (setq pospcase--matches (cdr pospcase--matches))
        t)
    (goto-char limit)
    (set-match-data nil)
    nil))

(defmacro pospcase--call-iterator (clause limit)
  "Catch parsing error, and call `pospcase--iterator'."
  `(condition-case nil
       (when (or (< (point) limit) pospcase--matches)
         (unless pospcase--matches ; initialize
           (setq pospcase--matches
                 (if (let* ((lim (ignore-errors (scan-sexps (point) 1)))
                            (temp (when lim
                                    (ignore-errors
                                      (read-from-string
                                       (buffer-substring-no-properties (point) lim))))))
                       (and (consp temp) (null (car temp)))) ; empty list at `point'
                     (let ((mlist (pospcase--list nil)))
                       (when mlist (list mlist)))
                     ,clause)))
          (goto-char (1- ,limit)) ; whole parsing is already done, no crawling
         (pospcase--iterator ,limit))
     (error
      (goto-char ,limit))))

(defun pospcase-match-varlist (limit)
  "Matcher iterator for a list of symbol or two length lists."
  (pospcase--call-iterator
   (mapcar
    (lambda (srpair)
      (goto-char (cadr srpair))
      (pospcase-at (point)
                   '((`(,name ,type) (pospcase--list name type))
                     (`,name (pospcase--list name)))))
    (car (pospcase-read (point))))
   limit))

(defvar pospcase--fence-start nil
  "Boundary cons cell (exp . (start . end)) for dropping
  unnecessary tree branches before here.")

(defun pospcase-match-key (limit)
  "Matcher iterator for a list of symbol or two or three length lists."
  (pospcase--call-iterator
   (mapcar
    (lambda (srpair)
      (goto-char (cadr srpair))
      (pospcase-at (point)
                   '((`(,name ,init ,sup) (pospcase--list name init sup))
                     (`(,name ,init) (pospcase--list name init))
                     (`,name (pospcase--list name)))))
    (if pospcase--fence-start
        (member pospcase--fence-start (car (pospcase-read (point))))
      (car (pospcase-read (point)))))
   limit))

(defun pospcase-match-varlist-cars (limit)
  "Matcher iterator for the `car's of a list of two or longer
length lists"
  (pospcase--call-iterator
   (mapcar
    (lambda (srpair)
      (goto-char (cadr srpair))
      (pospcase-at (point)
                   '((`(,name . ,_) (pospcase--list name))
                     (`,name (pospcase--list name)))))
    (if pospcase--fence-start
        (member pospcase--fence-start (car (pospcase-read (point))))
      (car (pospcase-read (point)))))
   limit))

(defalias #'pospcase-match-defstruct #'pospcase-match-varlist-cars)

(defun pospcase-match-flet (limit)
  "Matcher iterator for a list of `flet' bindings"
  (pospcase--call-iterator
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
                    (if arglist
                        (mapcar
                         (lambda (exp)
                           (list name
                                 (pospcase exp '((`(,arg . ,_) arg)
                                                 (`,arg arg)))))
                         arglist)
                      (list (list name))))))))
   limit))

(defun pospcase-collect-all-symbols (node)
  "Collect all symbols from a tree generated by `pospcase-read'."
  (cl-labels
      ((pos-p (pair)
              (numberp (car pair))
              (numberp (cdr pair)))
       (leaf-p (node)
               (and (symbolp (car node))
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
   limit))

(defun pospcase-match-macrolet (limit)
  "Matcher iterator for a lit of `macrolet' bindings"
  (pospcase--call-iterator
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
                    (if (car arglist)
                        (mapcar (lambda (arg) (cons name arg))
                                (pospcase-collect-all-symbols arglist))
                      (list (list name))))))))
   limit))

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
           pospcase--fence-start nil)
     ,@body))


;;; easy font-lock

(defvar pospcase-font-lock-lisp-keywords
  ;;
  ;; non-`pcase' powered
  ;;
  (let* ((symbol-start "\\_<")
         (symbol-end "\\_>")
         (symbol (concat symbol-start
                         "\\(?:\\sw\\|\\s_\\)+"
                         symbol-end))
         (space* "[ \t\n]*")
         (space+ "[ \t\n]+"))
    (cl-flet ((regexp-or (&rest exps)
                         (concat "\\(?:"
                                 (mapconcat #'identity exps "\\|")
                                 "\\)")))
      `(;; For `dolist'
        (,(concat "("
                  (regexp-opt lisp-extra-font-lock-dolist-functions)
                  space+
                  "(\\("
                  symbol
                  "\\)")
         ;; Faces
         (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))))
        ;; For `condition-case'
        (,(concat "("
                  (regexp-opt lisp-extra-font-lock-bind-first-functions)
                  space+
                  "\\("
                  symbol
                  "\\)")
         ;; Faces
         (1 (and (not (string= (match-string 1) "nil"))
                 ,(lisp-extra-font-lock-variable-face-form '(match-string 1)))))
        ;; For `cl-loop'
        (,(concat "("
                  (regexp-opt lisp-extra-font-lock-loop-functions)
                  "\\_>")
         (lisp-extra-font-lock-match-loop-keywords
          ;; Pre-match form
          (progn
            (goto-char (match-end 0))
            (save-excursion
              (goto-char (match-beginning 0))
              (ignore-errors (scan-lisp-extras (point) 1))))
          ;; Post-match form.
          (goto-char (match-end 0))
          ;; Faces
          (1 font-lock-builtin-face)
          (2 ,(lisp-extra-font-lock-variable-face-form '(match-string 2)) nil t)))
        (;; For quote and backquote
         ;;
         ;; Matcher: Set match-data 1 if backquote.
         lisp-extra-font-lock-match-quote-and-backquote
         (1 lisp-extra-font-lock-backquote-face nil t)
         (;; Submatcher, match part of quoted expression or comma.
          lisp-extra-font-lock-match-quoted-content
          ;; Pre-match form. Value of expression is limit for submatcher.
          (progn
            (goto-char (match-end 0))
            ;; Search limit
            (ignore-errors (scan-lisp-extras (point) 1)))
          ;; Post-match form
          (goto-char (match-end 0))
          ;; Faces
          (1 lisp-extra-font-lock-quoted-face append)
          (2 lisp-extra-font-lock-backquote-face nil t)))
        ;; For function read syntax
        (,(concat "#'\\("
                  symbol
                  "\\)")
         1 lisp-extra-font-lock-quoted-function-face)))))

(defvar-local pospcase-font-lock-lisp-keywords--installed nil
  "BUffer local list for `pospcase-font-lock-lisp-mode'.")

(defun pospcase-font-lock-lisp-keywords-add ()
  "Activate `pcase' based font lock for Lisp modes."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'pospcase-font-lock-lisp-keywords--installed)
    (font-lock-remove-keywords nil pospcase-font-lock-lisp-keywords--installed))
  (set (make-local-variable 'pospcase-font-lock-lisp-keywords--installed)
       pospcase-font-lock-lisp-keywords)
  (font-lock-add-keywords nil
                          pospcase-font-lock-lisp-keywords--installed
                          'append))

(defun pospcase-font-lock-lisp-keywords-remove ()
  "Deactivate `pcase' based font lock for Lisp modes."
  (font-lock-remove-keywords nil pospcase-font-lock-lisp-keywords--installed))

(defvar pospcase--dummy nil
  "Used in a hack for empty `multiple-value-bind'.")

(defun pospcase-font-lock-build (patterns specs)
  "Actual font lock code generator. More simplifications are
planed."
  (let* ((matcher (let ((str (prin1-to-string
                              (if (consp (car patterns))
                                  (if (memq (caar patterns) '(\` \, quote))
                                      (cadar patterns)
                                    (caar patterns))
                                (car patterns)))))
                    (string-match "^[^ \t\n]+" str)
                    (match-string 0 str)))
         (keyword (concat "\\<"
                          (substring matcher (string-match "[^(]" matcher))
                          "\\>"))
         (submatcher (let ((temp specs) result)
                       (while (and temp (null result))
                         (setq result (and (consp (caar temp)) (cdaar temp))
                               temp (cdr temp)))
                       result))
         (subvar (let ((temp specs) result)
                   (while (and temp (null result))
                     (setq result (and (consp (caar temp)) (caaar temp))
                           temp (cdr temp)))
                   result))
         (vars (mapcar (lambda (spec) (if (consp (car spec)) (caar spec) (car spec)))
                       specs))
         (non-subvars (remove subvar vars))
         (fontspecs (cl-loop with i = 0
                             for spec in specs
                             append (mapcar (lambda (font)
                                              (list (incf i)
                                                    (if (symbolp font)
                                                        (list 'quote font)
                                                      (apply (car font) (cdr font)))
                                                    nil t))
                                            (cdr spec))))
         (cases (mapcar (lambda (pat) (list pat (cons 'values vars))) patterns))
         (varlist-group '(varlist varlist-cars destructuring flet macrolet))
         (defstruct-group '(defstruct))
         (parameter-group '(key)))
    (setq matcher (concat matcher "[ \t\n]+"))
    `((,keyword . font-lock-keyword-face)
      (,matcher
       (,(intern (concat "pospcase-match-" (symbol-name submatcher)))
        (pospcase--preform
         (goto-char (match-beginning 0))
         (let ((match-end
                ,(cond

                  ((memq submatcher varlist-group)
                   '(save-excursion
                      (or
                       (ignore-errors (scan-sexps (point) 1))
                       (progn
                         (end-of-defun)
                         (point)))))

                  ((memq submatcher defstruct-group)
                   '(save-excursion
                      (condition-case nil
                          (progn
                            (forward-char)
                            (forward-sexp 2) ; skip keyword and name
                            (forward-comment most-positive-fixnum)
                            (when (equal
                                   (syntax-after (point))
                                   '(7)) ; skip docstring
                              (forward-sexp)
                              (forward-comment most-positive-fixnum))
                            (setq pospcase--fence-start
                                  (ignore-errors (pospcase-read (point))))
                            ;; Search limit
                            (up-list)
                            (point))
                        (error (end-of-defun)
                               (point)))))

                  ((memq submatcher parameter-group)
                   '(let ((end (1- (match-end 0))))
                      (if (let ((table (syntax-ppss (point))))
                            (or (nth 3 table)   ; in string
                                (nth 4 table))) ; in comment
                          (goto-char end)
                        (setq pospcase--fence-start
                              (ignore-errors (pospcase-read (1+ end))))
                        (if (condition-case nil
                                (progn
                                  (backward-up-list)
                                  (when (> (- (match-beginning 0) (point))
                                           500) ; arbitrary limit to prevent inf-loop
                                    (goto-char end)
                                    nil))
                              (error (goto-char end) nil))
                            ;; Search limit
                            (ignore-errors (scan-sexps (point 1)))
                          end))))

                  (t (error "Not supported submatcher.")))))
           (multiple-value-bind ,vars (pospcase-at (point) ',cases)
             (if (and ,(not (memq submatcher parameter-group))
                      (memq nil ,(cons 'list vars))) ; not exact match
                 (goto-char match-end)
               ,(unless (null non-subvars)
                  `(setq pospcase--prematches ,(cons 'list non-subvars)))

               ,(cond
                 ((memq submatcher varlist-group)
                  `(goto-char (car ,subvar))))))

           match-end))
        (pospcase--postform)
        ,@fontspecs)))))

(defun pospcase-font-lock (mode patterns specs)
  "Font lock keywords generator with `pcase' powered pattern
matching. Currently you can use sub-matchers:

Group one: varlist, varlist-cars, destructuring, flet, macrolet

Group two:  defstruct

Group three: key

So far sufficiently Common Lisp and Emacs Lisp highlighting is
achieved with either of them. 

Argument PATTERNS is a list of `pcase' patterns.

And SPECS is a list of slightly extended fontspec for
`font-lock-add-keywords' formed like:

  (match-name . (list of font-face-name)) or
  ((match-name . sub-matcher) . (list of font-face-name))

See the code of `pospcase-font-lock-lisp-setup' for working
examples."
  (let* ((id (car (split-string (symbol-name mode) "-mode")))
         (container (intern (format "pospcase-font-lock-%s-keywords" id)))
         (keywords (pospcase-font-lock-build patterns specs)))
    (unless (boundp container)
      (eval `(defvar ,container nil
               ,(format "List of font lock keywords for %s." id))))
    (mapc (lambda (keyword)
            (unless (member keyword (symbol-value container))
              (set container (cons keyword (symbol-value container)))))
          keywords)))

(defun pospcase-font-lock-lisp-setup ()
  "Setup various font lock keywords for convenience."
  (pospcase-font-lock 'lisp-mode
                      '(`(defun (setf ,name) ,args . ,_)
                        `(defun ,name ,args . ,_))
                      '((name . (font-lock-function-name-face))
                        ((args . varlist-cars) .
                         ((lisp-extra-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock  'lisp-mode
                       '(`(symbol-macrolet ,binds . ,_))
                       '(((binds . varlist) .
                          ((lisp-extra-font-lock-variable-face-form (match-string 1))
                           font-lock-constant-face))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defmethod (setf ,name) ,args ,(pred keywordp) . ,_)
                        `(defmethod (setf ,name) ,args . ,_)
                        `(defmethod ,name ,args ,(pred keywordp) . ,_)
                        `(defmethod ,name ,args . ,_))
                      '((name . (font-lock-function-name-face))
                        ((args . varlist) .
                         ((lisp-extra-font-lock-variable-face-form (match-string 1))
                          font-lock-type-face))))
  (pospcase-font-lock 'lisp-mode
                      '(`(destructuring-bind ,binds . ,_))
                      '(((binds . destructuring) .
                         ((lisp-extra-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defmacro ,name ,args . ,_))
                      '((name . (font-lock-function-name-face))
                        ((args . destructuring) .
                         ((lisp-extra-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(flet ,funs . ,_))
                      '(((funs . flet) .
                         (font-lock-function-name-face
                          (lisp-extra-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(macrolet ,macros . ,_))
                      '(((macros . macrolet) .
                         (font-lock-function-name-face
                          (lisp-extra-font-lock-variable-face-form (match-string 2))))))
  (pospcase-font-lock 'lisp-mode
                      '(`(defstruct ,name ,(pred stringp) ,first . ,_)
                        `(defstruct ,name ,first . ,_))
                      '((name . (font-lock-type-face))
                        ((first . defstruct) .
                         ((lisp-extra-font-lock-variable-face-form (match-string 1))))))
  (pospcase-font-lock 'lisp-mode
                      '(&key whatever)
                      '(((pospcase--dummy . key) .
                         ((lisp-extra-font-lock-variable-face-form (match-string 1))
                          default
                          default))))
  (pospcase-font-lock 'lisp-mode
                      '(&aux whatever)
                      '(((pospcase--dummy . key) .
                         ((lisp-extra-font-lock-variable-face-form (match-string 1))
                          default
                          default))))
  (pospcase-font-lock 'lisp-mode
                      '(&optional whatever)
                      '(((pospcase--dummy . key) .
                         ((lisp-extra-font-lock-variable-face-form (match-string 1))
                          default
                          default)))))

(pospcase-font-lock-lisp-setup)
(add-hook 'lisp-mode-hook #'pospcase-font-lock-lisp-keywords-add)
(add-hook 'emacs-lisp-mode-hook #'pospcase-font-lock-lisp-keywords-add)

(provide 'pospcase)
