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

to collect every occurring symbol positions (actual code from
`pospcase-match-destructuring'):

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
      (((quux 43 . 47)) ; s-expression. You have to `cdr' to get
       42 . 48))        ; positional metadata in this case.
     20 . 49)))

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
                        (memq (car sexp) '(\` quote)))
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
                                         (memq (car rpair) '(\` quote)))
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

(defmacro pospcase-translate (exp)
  "Translate `pcase' pattern to accommodate s-expression generated
by `pospcase-read'. Nested backquote is not supported (maybe
`pcase' doesn't support it too?)."
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
                             ((memq (car node) '(\` \, quote))
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
    (if (consp exp)
        (case (car exp)
          ('quote (list 'quote (list '\` (cons exp ',_))))
          ('\` (list 'quote (list '\` (walk (cadr exp)))))
          (otherwise (list 'quote (walk exp)))))))

(defun pospcase (exp cases)
  "`pcase'-variant for getting positional metadata."
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
                                   (eq (car node) '\,))
                              (setq symbols (cons (cadr node) symbols)))
                             ((consp node)
                              (collect (car node))
                              (collect (cdr node)))))
                   (translate (node)
                              (cond
                               ((member node symbols)
                                (intern (concat (symbol-name node) "-meta-pos")))
                               ((consp node) (cons
                                              (translate (car node))
                                              (translate (cdr node))))
                               (t node))))
                (collect (cadar case))
                (translate (cadr case))))))
         cases))))

(defun pospcase-at (pos cases)
  "`pospcase' at position POS of buffer."
  (pospcase (pospcase-read pos) cases))


;;; font-lock specific codes

(defvar pospcase--matches nil
  "Place to store currently working list of positional
metadata. Structured like:

  (((start . end)   ; (match-string 1) of first (match-data)
    (start . end))  ; (match-string 2) of first (match-data)
   ((start . end)   ; (match-string 1) of second (match-data)
    (start . end))) ; (match-string 2) of second (match-data)")

(defvar pospcase--anchor nil
  "Integer used for discarding unnecessary positional data before it.")

(defvar pospcase--match-beginning nil
  "Place to store beginning of submatch 0 used by multiline font lock.")

(defun pospcase--reset ()
  "Necessary for jit-lock?"
  (setq pospcase--matches nil
        pospcase--anchor nil))

(advice-add #'font-lock-fontify-region :around
            (lambda (orig-func beg end &optional loudly)
              (pospcase--reset)
              (funcall orig-func beg end loudly)
              (pospcase--reset)))
(advice-add #'font-lock-fontify-block :around
            (lambda (orig-func &optional arg)
              (pospcase--reset)
              (funcall orig-func arg)
              (pospcase--reset)))

(defun pospcase-after-anchor (mlist)
  "Discard every positional pair (start . end) occurring before
`pospcase--anchor' in the given list. Used to overcome docstring
occurrence uncertainty in `defstruct'"
  (cl-loop for list in mlist
           with temp
           do (setq temp (cl-loop for pair in list
                                  when (<= pospcase--anchor (car pair))
                                  collect pair))
           when temp collect temp))

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
           (setq pospcase--matches ,clause)
           (when pospcase--anchor
             (setq pospcase--matches (pospcase-after-anchor
                                            pospcase--matches)
                   pospcase--anchor nil)))
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
                   '((`(,name ,type) (list name type))
                     (`,name (list name)))))
    (car (pospcase-read (point))))
   limit))

(defun pospcase-match-varlist-cars (limit)
  "Matcher iterator for the `car's of a list of two or longer
length lists"
  (pospcase--call-iterator
   (mapcar
    (lambda (srpair)
      (goto-char (cadr srpair))
      (pospcase-at (point)
                   '((`(,name . ,_) (list name))
                     (`,name (list name)))))
    (car (pospcase-read (point))))
   limit))

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

(defun pospcase-match-destructuring (limit)
  "Matcher iterator for symbols in an arbitrarily nested list."
  (pospcase--call-iterator
   (cl-labels ((collect (node) (if (symbolp (car node))
                                   (list (list (cdr node)))
                                 (cl-loop for child in (car node)
                                          append (collect child)))))
     (collect (pospcase-read (point))))
   limit))

(defmacro pospcase--preform (&rest body)
  "Multiline font lock use submatch 0 for jit fontification."
  `(progn
     (setq pospcase--match-beginning (match-beginning 0))
     ,@body))

(defun pospcase-lisp-keywords ()
  "Font-lock keywords used by `pospcase-lisp-font-lock-mode'.
The keywords highlight variable bindings and quoted expressions."
  (let ((symbol-regexp "\\(?:\\sw\\|\\s_\\)+")
        (space-regexp "[ \t\n]+"))
    `(;;
      ;; non-`pcase' powered
      ;;

      ;; For `dolist'
      (,(concat "("
                (regexp-opt lisp-extra-font-lock-dolist-functions)
                space-regexp
                "(\\("
                symbol-regexp
                "\\)\\_>")
       ;; Faces
       (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))))
      ;; For `condition-case'
      (,(concat "("
                (regexp-opt lisp-extra-font-lock-bind-first-functions)
                space-regexp
                "\\_<\\("
                symbol-regexp
                "\\)\\_>")
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
                symbol-regexp
                "\\)\\_>")
       1 lisp-extra-font-lock-quoted-function-face)

      ;;
      ;; `pcase' powered
      ;;

      ;; For `defun' and `lambda'
      (,(concat "("
                "\\(?:"
                "\\(?:"
                (regexp-opt lisp-extra-font-lock-defun-functions)
                space-regexp
                "\\(?:"
                "\\_<"
                symbol-regexp
                "\\_>"
                "\\|"
                "(setf"
                space-regexp
                "\\_<"
                symbol-regexp
                "\\_>)"
                "\\)"
                "\\)"
                "\\|"
                (regexp-opt lisp-extra-font-lock-lambda-functions)
                "\\)"
                space-regexp
                "(")
       (pospcase-match-varlist-cars
        ;; Pre-match form
        (pospcase--preform
          (goto-char (1- (match-end 0)))
          ;; Search limit
          (ignore-errors (scan-sexps (point) 1)))
        ;; Post-match form
        nil
        ;; Faces
        (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
           nil t)))

      ;; For `let'.
      (,(concat "("
                (regexp-opt lisp-extra-font-lock-let-functions)
                space-regexp
                "(")
       (pospcase-match-varlist-cars
        ;; Pre-match form
        (pospcase--preform
          (goto-char (1- (match-end 0)))
          ;; Search limit
          (ignore-errors (scan-sexps (point) 1)))
        ;; Post-match form
        nil
        ;; Faces
        (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
           nil t)))

      ;; For `flet'.
      (,(concat "("
                (regexp-opt lisp-extra-font-lock-flet-functions)
                space-regexp
                "(")
       (pospcase-match-flet
        ;; Pre-match form
        (pospcase--preform
          (goto-char (1- (match-end 0)))
          ;; Search limit
          (ignore-errors (scan-sexps (point) 1)))
        ;; Post-match form
        nil
        ;; Faces
        (1 font-lock-function-name-face
           nil t)
        (2 ,(lisp-extra-font-lock-variable-face-form '(match-string 2))
           nil t)))

      ;; For `symbol-macrolet'
      (,(concat "("
                (regexp-opt lisp-extra-font-lock-symbol-macrolet-functions)
                space-regexp
                "(")
       (pospcase-match-varlist
        ;; Pre-match form
        (pospcase--preform
          (goto-char (1- (match-end 0)))
          ;; Search limit
          (ignore-errors (scan-sexps (point) 1)))
        ;; Post-match form
        nil
        ;; Faces
        (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
           nil t)
        (2 font-lock-constant-face
           nil t)))

      ;; For `defmethod'
      (,(concat "("
                (regexp-opt lisp-extra-font-lock-defmethod-functions)
                space-regexp
                "\\(?:"
                "\\_<"
                symbol-regexp
                "\\_>"
                "\\|"
                "(setf"
                space-regexp
                "\\_<"
                symbol-regexp
                "\\_>)"
                "\\)"
                space-regexp
                (regexp-opt lisp-extra-font-lock-defmethod-keywords)
                "?"
                "[ \t\n]*"
                "(")
       (pospcase-match-varlist
        ;; Pre-match form
        (pospcase--preform
          (goto-char (1- (match-end 0)))
          ;; Search limit
          (ignore-errors (scan-sexps (point) 1)))
        ;; Post-match form
        nil
        ;; Faces
        (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
           nil t)
        (2 font-lock-type-face
           nil t)))

      ;; For `defclass'
      (,(concat "("
                (regexp-opt lisp-extra-font-lock-defclass-functions)
                space-regexp
                "\\_<"
                symbol-regexp
                "\\_>"
                space-regexp
                "(")
       (pospcase-match-varlist
        ;; Pre-match form
        (pospcase--preform
          (goto-char (1- (match-end 0)))
          ;; Search limit
          (ignore-errors (scan-sexps (point) 1)))
        ;; Post-match form
        nil
        ;; Faces
        (1 font-lock-type-face
           nil t)))

      ;; For `defclass' slots
      (,(concat "("
                (regexp-opt lisp-extra-font-lock-defclass-functions)
                space-regexp
                "\\_<"
                symbol-regexp
                "\\_>"
                space-regexp
                "("
                "[^)]*"
                ")" ; Reason some people write comment here is `defclass' has no docstring.
                "\\(?:[ \t\n]*;[^\n]*\n\\)*" ; Needs font-lock-fontify-block to work properly?
                "[ \t\n]*(")
       (pospcase-match-varlist-cars
        ;; Pre-match form
        (pospcase--preform
          (goto-char (1- (match-end 0)))
          ;; Search limit
          (ignore-errors (scan-sexps (point) 1)))
        ;; Post-match form
        nil
        ;; Faces
        (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
           nil t)))

      ;; For `defstruct'
      (,(concat "("
                "\\(?:cl-\\)?defstruct"
                space-regexp)
       (pospcase-match-varlist-cars
        ;; Pre-match form
        (pospcase--preform
          (goto-char (match-beginning 0))
          (save-excursion
            (condition-case nil
                (progn
                  (forward-char)
                  (forward-sexp 2)      ; skip keyword and name
                  (forward-comment (buffer-size))
                  (when (= (following-char) ?\") ; skip docstring
                    (forward-sexp)
                    (forward-comment (buffer-size)))
                  (setq pospcase--anchor (1- (point)))
                  ;; Search limit
                  (up-list)
                  (point))
              (error (end-of-defun)))))
        ;; Post-match form
        nil
        ;; Faces
        (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
           nil t)))

      ;; For `destructuring-bind'
      (,(concat "(\\(?:"
                "\\(?:cl-\\)?destructuring-bind"
                "\\|"
                (concat "\\(?:cl-\\|sb!xc:\\)?defmacro"
                        space-regexp
                        symbol-regexp)
                "\\)"
                space-regexp
                "(")
       (pospcase-match-destructuring
        ;; Pre-match form
        (pospcase--preform
          (goto-char (1- (match-end 0)))
          ;; Search limit
          (ignore-errors (scan-sexps (point) 1)))
        ;; Post-match form
        nil
        ;; Faces
        (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
           nil t))))))

(defvar pospcase--installed-lisp-keywords nil)

(defun pospcase-add-lisp-keywords ()
  "Add extra font-lock keywords to lisp."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'pospcase--installed-lisp-keywords)
    (font-lock-remove-keywords nil pospcase--installed-lisp-keywords))
  (let ((keywords (pospcase-lisp-keywords)))
    (set (make-local-variable 'pospcase--installed-lisp-keywords)
         keywords)
    (font-lock-add-keywords nil keywords 'append)))

(defun pospcase-remove-lisp-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil pospcase--installed-lisp-keywords))

(defgroup pospcase nil
  "`pcase' powered font lock."
  :group 'faces)

;;;###autoload
(defcustom pospcase-lisp-modes '(emacs-lisp-mode lisp-mode)
  "List of modes where Lisp Extra Font Lock Global mode should be enabled."
  :type '(repeat symbol)
  :group 'pospcase)

;;;###autoload
(define-minor-mode pospcase-lisp-font-lock-mode
  "Minor mode that highlights bound variables and quoted expressions in lisp."
  :group 'pospcase
  (if pospcase-lisp-font-lock-mode
      (pospcase-add-lisp-keywords)
    (pospcase-remove-lisp-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(define-global-minor-mode pospcase-global-lisp-font-lock-mode
  pospcase-lisp-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p pospcase-lisp-modes)
      (pospcase-lisp-font-lock-mode 1)))
  :group 'pospcase)

(provide 'pospcase)
