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
                        (memq (car sexp) '(\` \, quote)))
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
                                         (memq (car rpair) '(\` \, quote)))
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
    (if (consp matcher)
        (case (car matcher)
          ('quote (list 'quote (list '\` (cons matcher ',_))))
          ('\` (list 'quote (list '\` (walk (cadr matcher)))))
          (otherwise (list 'quote (walk matcher)))))))

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
                (collect (cadar case))
                (translate (cadr case))))))
         cases))))

(defun pospcase-at (pos cases)
  "`pospcase' at position POS of buffer."
  (pospcase (pospcase-read pos) cases))


;;; font-lock specific codes

(defvar pospcase--matches nil
  "Currently working list of point pairs. It is
  structured like:

  (((start . end)   ; (match-string 1) of first (match-data)
    (start . end))  ; (match-string 2) of first (match-data)
   ((start . end)   ; (match-string 1) of second (match-data)
    (start . end))) ; (match-string 2) of second (match-data)")

(defvar pospcase--prematches nil
  "List of point pairs attached by matcher iterators at the head
  of each list of point pairs.")
(defun pospcase--list (&rest matches)
  (append pospcase--prematches matches))

(defvar pospcase--match-beginning nil
  "Start point of submatch 0 used by multiline font lock.")

(defvar pospcase--fence-start nil
  "Boundary point for discarding unnecessary positional data before it.")
(defvar pospcase--fence-end nil
  "Boundary point for discarding unnecessary positional data after it.")

(defun pospcase-fence (mlist)
  "Discard everything before `pospcase--fence-start' and after
 `pospcase--fence-end' in MLIST. Made to overcome docstring
 occurrence uncertainty in `defstruct'"
  (cl-loop for list in mlist
           with temp
           do (setq temp
                    (cl-loop for pair in list
                             when (and
                                   (if pospcase--fence-start
                                       (<= pospcase--fence-start (car pair))
                                     t)
                                   (if pospcase--fence-end
                                       (>= pospcase--fence-end (cdr pair))
                                     t))
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
           (setq pospcase--matches (if (equal
                                        (ignore-errors
                                          (read-from-string
                                           (buffer-substring-no-properties (point) (+ (point) 2))))
                                        '(nil . 2))
                                       (let ((mlist (pospcase--list nil)))
                                         (when mlist (list mlist)))
                                     ,clause))
           (when (or pospcase--fence-start pospcase--fence-end)
             (setq pospcase--matches (pospcase-fence pospcase--matches)
                   pospcase--fence-start nil)))
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
                   '((`(,name . ,_) (pospcase--list name))
                     (`,name (pospcase--list name)))))
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
                      (member (caar node) '(\` \, quote)))))
    (cl-loop with result
             for temp = node then (cdr temp)
             if (or (null temp)
                    (pos-p temp)
                    (ignore-p temp)) return result
             else if (leaf-p temp) return (append
                                           result
                                           (list (list (cdr temp))))
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
           pospcase--fence-start nil
           pospcase--fence-end nil)
     ,@body))

(defun pospcase-lisp-keywords ()
  "Font-lock keywords used by `pospcase-lisp-font-lock-mode'.
The keywords highlight variable bindings and quoted expressions."
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
      `(;;
        ;; non-`pcase' powered
        ;;

        ;; For `dolist'
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
         1 lisp-extra-font-lock-quoted-function-face)

        ;;
        ;; `pcase' powered
        ;;

        ;; For `defun', `lambda', `let', `defclass' slots
        (,(concat
           "("
           (regexp-or
            (concat
             (regexp-opt lisp-extra-font-lock-defun-functions)
             space+
             (regexp-or
              symbol
              (concat "(setf"
                      space+
                      symbol
                      ")")))
            (regexp-opt lisp-extra-font-lock-lambda-functions)
            (regexp-opt lisp-extra-font-lock-let-functions)
            (concat
             (regexp-opt lisp-extra-font-lock-defclass-functions)
             space+
             symbol
             space+
             "("
             "[^)]*"
             ")" ; Reason some people write comment here is `defclass' has no docstring.
             "\\(?:" ; Needs font-lock-fontify-block to work properly?
             space*
             ";[^\n]*\n"
             "\\)*"
             space*))
           space+
           "(")
         (pospcase-match-varlist-cars
          (pospcase--preform
           (goto-char (1- (match-end 0)))
           ;; Search limit
           (ignore-errors (scan-sexps (point) 1)))
          (pospcase--postform)
          ;; Faces
          (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
             nil t)))

        ;; For `defclass'
        (,(concat "("
                  (regexp-opt lisp-extra-font-lock-defclass-functions)
                  space+
                  symbol
                  space+
                  "(")
         (pospcase-match-varlist-cars
          (pospcase--preform
           (goto-char (1- (match-end 0)))
           ;; Search limit
           (ignore-errors (scan-sexps (point) 1)))
          (pospcase--postform)
          ;; Faces
          (1 font-lock-type-face
             nil t)))

        ;; For `flet'.
        (,(concat "("
                  (regexp-opt lisp-extra-font-lock-flet-functions)
                  space+
                  "(")
         (pospcase-match-flet
          (pospcase--preform
           (goto-char (1- (match-end 0)))
           ;; Search limit
           (ignore-errors (scan-sexps (point) 1)))
          (pospcase--postform)
          ;; Faces
          (1 font-lock-function-name-face
             nil t)
          (2 ,(lisp-extra-font-lock-variable-face-form '(match-string 2))
             nil t)))

        ;; For `symbol-macrolet'
        (,(concat "("
                  (regexp-opt lisp-extra-font-lock-symbol-macrolet-functions)
                  space+
                  "(")
         (pospcase-match-varlist
          (pospcase--preform
           (goto-char (1- (match-end 0)))
           ;; Search limit
           (ignore-errors (scan-sexps (point) 1)))
          (pospcase--postform)
          ;; Faces
          (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
             nil t)
          (2 font-lock-constant-face
             nil t)))

        ;; For `defmethod'
        (,(concat "("
                  (regexp-opt lisp-extra-font-lock-defmethod-functions)
                  space+
                  (regexp-or
                   symbol
                   (concat "(setf"
                           space+
                           symbol
                           ")"))
                  space+
                  (regexp-opt lisp-extra-font-lock-defmethod-keywords)
                  "?"
                  space*
                  "(")
         (pospcase-match-varlist
          (pospcase--preform
           (goto-char (1- (match-end 0)))
           ;; Search limit
           (ignore-errors (scan-sexps (point) 1)))
          (pospcase--postform)
          ;; Faces
          (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
             nil t)
          (2 font-lock-type-face
             nil t)))

         ;; For `defstruct'
         (,(concat "("
                   "\\(?:cl-\\)?defstruct"
                   space+)
          (pospcase-match-varlist-cars
           (pospcase--preform
            (goto-char (match-beginning 0))
            (save-excursion
              (condition-case nil
                  (progn
                    (forward-char)
                    (forward-sexp 2)    ; skip keyword and name
                    (forward-comment (buffer-size))
                    (when (= (following-char) ?\") ; skip docstring
                      (forward-sexp)
                      (forward-comment (buffer-size)))
                    (setq pospcase--fence-start (1- (point)))
                    ;; Search limit
                    (up-list)
                    (point))
                (error (end-of-defun)))))
           (pospcase--postform)
           ;; Faces
           (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
              nil t)))

         ;; For `&key'
         (,(funcall #'regexp-opt lisp-extra-argument-list-key-keywards)
          (pospcase-match-key
           (pospcase--preform
            (if (nth 4 (syntax-ppss (point)))
                (goto-char (match-end 0))
              (setq pospcase--fence-start (match-end 0))
              (condition-case nil
                  (progn
                    (backward-up-list)
                    (when (> (- (match-beginning 0) (point)) 500)  ; arbitrary limit to prevent inf-loop
                      (goto-char (match-beginning 0))))
                (error (match-end 0)))
              ;; Search limit
              (ignore-errors (scan-sexps (point 1)))))
           (pospcase--postform)
           ;; Faces
           (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
              nil t)
           (2 'default
              nil t)
           (3 'default
              nil t)))

         ;; For `destructuring-bind'
         (,(concat (regexp-or
                    "\\(?:cl-\\)?destructuring-bind"
                    (concat "\\(?:cl-\\|sb!xc:\\)?defmacro"
                            space+
                            symbol))
                   space+
                   "(")
          (pospcase-match-destructuring
           (pospcase--preform
            (goto-char (1- (match-end 0)))
            ;; Search limit
            (ignore-errors (scan-sexps (point) 1)))
           (pospcase--postform)
           ;; Faces
           (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
              nil t)))

         ;; For `macrolet'
         (,(concat "("
                   (regexp-opt lisp-extra-font-lock-macrolet-functions)
                   space+
                   "(")
          (pospcase-match-macrolet
           (pospcase--preform
            (goto-char (1- (match-end 0)))
            ;; Search limit
            (ignore-errors (scan-sexps (point) 1)))
           (pospcase--postform)
           ;; Faces
           (1 font-lock-function-name-face
              nil t)
           (2 ,(lisp-extra-font-lock-variable-face-form '(match-string 2))
              nil t)))))))

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
