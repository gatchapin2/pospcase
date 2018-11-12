;;; Should I get rid of this dependency?
(require 'lisp-extra-font-lock)


;;; generic matchers

(defun sexp-font-lock-read-at-point ()
  "Parse the s-expression at the cursor position. Return a
s-expression with the tokens being replaced with the cons. With
which `car' is original token, and `cdr' is positional metadata
of the token as a cons in (begin . end)."
  (cl-labels
      ((walker (limit)
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
                                       (walker rlim))))
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
    (save-excursion (walker (scan-sexps (point) 1)))))

(defmacro sexp-font-lock-translate-pattern (exp)
  "Translate `pcase' pattern to fit s-expression generated by
`sexp-font-lock-read-at-point'. Nested backquote is not
supported (maybe `pcase' doesn't support it too?)."
  (cl-labels
      ((meta-pos-symbol (sym)
                        (list '\,
                              (intern (concat
                                       (symbol-name (cadr node))
                                       "-meta-pos"))))
       (walker (node)
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
                                (append result (list (walker (car node))))
                                (walker (cdr node)))))
                             ((memq (car node) '(\` \, quote))
                              (setq result (append
                                            result
                                            (if ; cdr cell ,foo matches the rest of a list
                                                (and (null (cddr node))
                                                     (symbolp (cadr node)))
                                                (meta-pos-symbol (cadr node))
                                              (walker (list (car node) (cadr node)))))
                                    node (cddr node)))
                             ((or (car node)
                                  (cdr node))
                              (setq result (append result (list (walker (car node))))
                                    node (cdr node)))
                             (t (cl-return result))))
                        ',_)))
                 (cons node ',_))))
    (if (consp exp)
        (case (car exp)
          ('quote (list 'quote (list '\` (cons exp ',_))))
          ('\` (list 'quote (list '\` (walker (cadr exp)))))
          (otherwise (list 'quote (walker exp)))))))

(defmacro sexp-font-lock-match-at-point (&rest cases)
  `(pcase
       ,(list 'quote (sexp-font-lock-read-at-point))
     ,@(mapcar
        (lambda (case)
          (list
           (eval `(sexp-font-lock-translate-pattern ,(car case)))
           (cond
            ((and (listp (cadr case))
                  (eq (caadr case) 'list))
             (cons
              'list
              (mapcar (lambda (sym)
                        (intern (concat
                                 (symbol-name sym) "-meta-pos")))
                      (cdadr case))))
            ((symbolp (cadr case))
             (intern (concat
                      (symbol-name (cadr case)) "-meta-pos")))
            (t (error "This macro is designed for extracting \
positional metadata, and not to be used as generic control \
structure. Complex operations are not supported.")))))
        cases)))


;;; font-lock specific codes

(defvar sexp-font-lock--matches nil)
(defvar sexp-font-lock--anchor nil)

(defun sexp-font-lock-after-anchor (mlist)
  (cl-loop for list in mlist
           with temp
           do (setq temp (cl-loop for pair in list
                                  when (<= sexp-font-lock--anchor (car pair))
                                  collect pair))
           when temp collect temp))

(defun sexp-font-lock--iterator ()
  (if sexp-font-lock--matches
      (let ((mlist (cl-loop for pair in (car sexp-font-lock--matches)
                            append (-cons-to-list pair))))
        (set-match-data (append '(nil nil) mlist))
        (setq sexp-font-lock--matches (cdr sexp-font-lock--matches))
        t)
    (set-match-data nil)))

(defmacro sexp-font-lock-call-iterator (clause limit) ; for catching parsing error
  `(condition-case nil
       (when (or (< (point) limit) sexp-font-lock--matches)
         (unless sexp-font-lock--matches ; initialize
           (setq sexp-font-lock--matches ,clause)
           (when sexp-font-lock--anchor
             (setq sexp-font-lock--matches (sexp-font-lock-after-anchor
                                            sexp-font-lock--matches)
                   sexp-font-lock--anchor nil))
           (goto-char ,limit)) ; whole parsing is already done, it's not crawler
         (sexp-font-lock--iterator))
     (error
      (goto-char ,limit))))

(defun sexp-font-lock-match-varlist (limit)
  (sexp-font-lock-call-iterator
   (mapcar
    (lambda (srpair)
      (or (progn
            (goto-char (cadr srpair))
            (eval '(sexp-font-lock-match-at-point (`(,name ,type) (list name type)))))
          (list (cdr srpair))))
    (car (sexp-font-lock-read-at-point)))
   limit))

(defun sexp-font-lock-match-varlist-cars (limit)
  (sexp-font-lock-call-iterator
   (mapcar
    (lambda (srpair)
      (or (progn
            (goto-char (cadr srpair))
            (eval '(sexp-font-lock-match-at-point (`(,name . ,rest) (list name)))))
          (list (cdr srpair))))
    (car (sexp-font-lock-read-at-point)))
   limit))

(defun sexp-font-lock-match-flet (limit)
  (sexp-font-lock-call-iterator
   (cl-loop for srpair in (car (sexp-font-lock-read-at-point))
            append
            (progn
              (goto-char (cadr srpair))
              (multiple-value-bind
                  (name args)
                  (eval '(sexp-font-lock-match-at-point
                          (`(,name ,args . ,_) (list name args))))
                (progn
                  (goto-char (car args))
                  (mapcar (lambda (var) (list name (cdr var)))
                          (car (sexp-font-lock-read-at-point)))))))
   limit))

(defun sexp-font-lock-keywords ()
  "Font-lock keywords used by `sexp-font-lock'.
The keywords highlight variable bindings and quoted expressions."
  `(;;;;;;;;;;;;;;;;;;;;;;;;
    ;; non-`pcase' powered
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; For `cl-dolist'
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-dolist-functions)
              "[ \t\n]+(\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))))
    ;; For `condition-case'
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-bind-first-functions)
              "[ \t\n]+\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 (and (not (string= (match-string 1) "nil"))
             ,(lisp-extra-font-lock-variable-face-form '(match-string 1)))))
    ;; For `cl-loop'
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-loop-functions)
              "\\_>")
     (lisp-extra-font-lock-match-loop-keywords
      ;; Pre-match form. Value of expression is limit for submatcher.
      (progn
        (goto-char (match-end 0))
        (save-excursion
          (goto-char (match-beginning 0))
          (ignore-errors (scan-lisp-extras (point) 1))))
      ;; Post-match form.
      (goto-char (match-end 0))
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
      ;; Highlight rules for submatcher.
      (1 lisp-extra-font-lock-quoted-face append)
      (2 lisp-extra-font-lock-backquote-face nil t)))
    ;; For function read syntax
    ("#'\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>"
     1 lisp-extra-font-lock-quoted-function-face)

    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; `pcase' powered
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; For `defun' and `lambda'
    (,(concat "("
              "\\(?:"
              "\\(?:"
              (regexp-opt lisp-extra-font-lock-defun-functions)
              "[ \t\n]+"
              "\\(?:"
              "\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
              "\\|"
              "(setf[ \t\n]+\\_<\\(?:\\sw\\|\\s_\\)+\\_>)"
              "\\)"
              "\\)"
              "\\|"
              (regexp-opt lisp-extra-font-lock-lambda-functions)
              "\\)"
              "[ \t\n]+(")
     (sexp-font-lock-match-varlist
      ;; Pre-match form
      (progn
        (goto-char (1- (match-end 0)))
        ;; Search limit
        (ignore-errors (scan-sexps (point) 1)))
      ;; Post-match form
      nil
      (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
         nil t)))

    ;; For `let'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-let-functions)
              "[ \t\n]+(")
     (sexp-font-lock-match-varlist
      ;; Pre-match form
      (progn
        (goto-char (1- (match-end 0)))
        ;; Search limit
        (ignore-errors (scan-sexps (point) 1)))
      ;; Post-match form
      nil
      (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1)))))

    ;; For `flet'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-flet-functions)
              "[ \t\n]+(")
     (sexp-font-lock-match-flet
      ;; Pre-match form
      (progn
        (goto-char (1- (match-end 0)))
        ;; Search limit
        (ignore-errors (scan-sexps (point) 1)))
      ;; Post-match form
      nil
      (1 font-lock-function-name-face)
      (2 ,(lisp-extra-font-lock-variable-face-form '(match-string 2))
         nil t)))

    ;; For `symbol-macrolet'
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-symbol-macrolet-functions)
              "[ \t\n]+(")
     (sexp-font-lock-match-varlist
      ;; Pre-match form
      (progn
        (goto-char (1- (match-end 0)))
        ;; Search limit
        (ignore-errors (scan-sexps (point) 1)))
      ;; Post-match form
      nil
      (2 font-lock-constant-face)))

    ;; For `defmethod'
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-defmethod-functions)
              "[ \t\n]+"
              "\\(?:"
              "\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
              "\\|"
              "(setf[ \t\n]+\\_<\\(?:\\sw\\|\\s_\\)+\\_>)"
              "\\)"
              "[ \t\n]+"
              (regexp-opt lisp-extra-font-lock-defmethod-keywords)
              "?"
              "[ \t\n]*"
              "(")
     (sexp-font-lock-match-varlist
      ;; Pre-match form
      (progn
        (goto-char (1- (match-end 0)))
        ;; Search limit
        (ignore-errors (scan-sexps (point) 1)))
      ;; Post-match form
      nil
      (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))
         nil t)
      (2 font-lock-type-face nil t)))

    ;; For `defclass'
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-defclass-functions)
              "[ \t\n]+\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
              "[ \t\n]+"
              "(")
     (sexp-font-lock-match-varlist
      ;; Pre-match form
      (progn
        (goto-char (1- (match-end 0)))
        ;; Search limit
        (ignore-errors (scan-sexps (point) 1)))
      ;; Post-match form
      nil
      (1 font-lock-type-face nil t)))

    ;; For `defclass' slots
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-defclass-functions)
              "[ \t\n]+\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
              "[ \t\n]+"
              "("
              "[^)]*"
              ")" ; Reason some people write comment here is `defclass' has no docstring.
              "\\(?:[ \t\n]*;[^\n]*\n\\)*" ; Needs font-lock-fontify-block to work properly?
              "[ \t\n]*(")
     (sexp-font-lock-match-varlist-cars
      ;; Pre-match form
      (progn
        (goto-char (1- (match-end 0)))
        ;; Search limit
        (ignore-errors (scan-sexps (point) 1)))
      ;; Post-match form
      nil
      (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1)))))

    ;; For `defstruct'.
    (,(concat "("
              "\\(?:cl-\\)?defstruct"
              "[ \t\n]+")
     (sexp-font-lock-match-varlist-cars
      ;; Pre-match form
      (progn
        (goto-char (match-beginning 0))
        (save-excursion
          (condition-case nil
              (progn
                (forward-char)
                (forward-sexp 2)          ; skip keyword and name
                (forward-comment (buffer-size))
                (when (= (following-char) ?\") ; skip docstring
                  (forward-sexp)
                  (forward-comment (buffer-size)))
                (setq sexp-font-lock--anchor (1- (point)))

                ;; Search limit
                (up-list)
                (point))

            (error (end-of-defun)))))
      ;; Post-match form
      nil
      (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1)))))))


(defvar sexp-font-lock--installed-keywords nil)

(defun sexp-font-lock-add-keywords ()
  "Add extra font-lock keywords to lisp."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'sexp-font-lock--installed-keywords)
    (font-lock-remove-keywords nil sexp-font-lock--installed-keywords))
  (let ((keywords (sexp-font-lock-keywords)))
    (set (make-local-variable 'sexp-font-lock--installed-keywords)
         keywords)
    (font-lock-add-keywords nil keywords 'append)))


(defun sexp-font-lock-remove-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil sexp-font-lock--installed-keywords))


(defgroup sexp-font-lock nil
  "Highlight bound variables and quoted expressions in lisp."
  :group 'faces)

;;;###autoload
(defcustom sexp-font-lock-modes '(emacs-lisp-mode lisp-mode)
  "List of modes where Lisp Extra Font Lock Global mode should be enabled."
  :type '(repeat symbol)
  :group 'sexp-font-lock)

;;;###autoload
(define-minor-mode sexp-font-lock-mode
  "Minor mode that highlights bound variables and quoted expressions in lisp."
  :group 'sexp-font-lock
  (if sexp-font-lock-mode
      (sexp-font-lock-add-keywords)
    (sexp-font-lock-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(define-global-minor-mode sexp-font-lock-global-mode
  sexp-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p sexp-font-lock-modes)
      (sexp-font-lock-mode 1)))
  :group 'sexp-font-lock)
