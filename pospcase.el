;;; pospcase.el -- `pcase' powered position extractor -*- lexical-binding: t -*-

(defcustom pospcase--elispify-alist
  (let* ((sym "\\(?:\\sw\\|\\s_\\|\\\\.\\)")
         (sym* (concat "\\(" sym "*" "\\)"))
         (sym+ (concat "\\(" sym "+" "\\)"))
         (non-printable-1 (make-string 1 8203)) ; ZERO WIDTH SPACE
         (non-printable-2 (make-string 1 8204)) ; ZERO WIDTH NON-JOINTER
         (non-printable-3 (make-string 1 8205)) ; ZERO WIDTH JOINTER
         (lambda-1 (lambda (str) (concat "\"" (substring str 2) "\"")))
         (lambda-2 (lambda (str) (concat
                                  (make-string
                                   (- (match-end 1) (match-beginning 0))
                                   8203) ; ZERO WIDTH SPACE
                                  (match-string 2 str))))
         (lambda-3 (lambda (str) (concat
                                  non-printable-2
                                  non-printable-2
                                  (replace-regexp-in-string
                                   "\\S "
                                   non-printable-1
                                   (match-string 1 str))
                                  non-printable-3
                                  non-printable-3))))
    `(("#|" . ,(concat non-printable-2
                       non-printable-2))
      ("|#" . ,(concat non-printable-3
                       non-printable-3))
      (,(concat non-printable-2
                non-printable-2
                "\\([^" non-printable-3 "]*\\)"
                non-printable-3
                non-printable-3)
       . ,lambda-3)
      ("[[{]" . "(")
      ("[]}]" . ")")
      (,(concat "#\\\\" sym+) . ,lambda-1)
      ("#\\\\." . ,lambda-1)
      (,(concat "\\(#!?[-.+]\\)" sym+) . ,lambda-2)
      (,(concat "#" sym* "\\([(\"]\\)") . ,lambda-2)))
  "Used for simple regexp based translation from Common Lisp
  S-expression to Emacs Lisp."
  :type 'alist)

(defun pospcase--buffer-substring (start end)
  "`buffer-substring' with regexp based Elisp-ification."
  (cl-reduce (lambda (str pair)
               (replace-regexp-in-string (car pair) (cdr pair) str))
             (cons (buffer-substring-no-properties start end)
                   pospcase--elispify-alist)))

(defvar pospcase--nth-chop-off nil
  "Used for chopping off trailing `. ,_', often happens in
  font-lock patterns.")

(defun pospcase-read (pos)
  "Read a s-expression at POS. Recursively wrap each s-expression
in cons cell then attach positional metadata (start . end) at
each `cdr'.

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
   (car                 ; outermost dot notation returns with extra list wrap
    (pospcase-at (point-min)
                 '((`(defun ,_ ,_ . ,temp) temp))))
   '((`,body body)))

which returns:

  (20 . 49)"
  (save-excursion
    (goto-char pos)
    (let* ((sexp-end (save-excursion
                       (if (and pospcase--nth-chop-off
                                (progn
                                  (forward-comment most-positive-fixnum)
                                  (eq (syntax-class (syntax-after (point))) 4)))
                           (progn
                             (down-list)
                             (forward-sexp pospcase--nth-chop-off))
                         (forward-sexp)
                         (forward-comment most-positive-fixnum))
                       (point)))
           (buf-off (point))
           (buf-str-1 (pospcase--buffer-substring buf-off sexp-end))
           (buf-str (with-temp-buffer
                      (insert buf-str-1)
                      (insert (make-string (car (syntax-ppss)) ?\)))
                      (buffer-substring (point-min) (point-max)))))
      (cl-labels
          ((walk (limit)
                 (condition-case nil
                     (destructuring-bind (start sexp . lim)
                         (cons (point)
                               (condition-case nil
                                   (read-from-string buf-str (- (point) buf-off))
                                 (invalid-read-syntax
                                  (cons 'pospcase-invalid-read-syntax
                                        (- limit (point))))))
                       (incf lim buf-off)
                       (forward-comment most-positive-fixnum)
                       (cons
                        (if (or (atom sexp)
                                (memq (car sexp) '(\` \, quote function)))
                            sexp
                          (down-list)
                          (cl-loop
                           with rpair
                           with rlim
                           with dot
                           with temp
                           with result
                           do (setq rpair
                                    (condition-case err
                                        (progn
                                          (forward-comment most-positive-fixnum)
                                          (read-from-string buf-str (- (point) buf-off)))
                                      (invalid-read-syntax
                                       (if (string= (cadr err) ".")
                                           (progn
                                             (setq dot t)
                                             (forward-sexp)
                                             (forward-comment most-positive-fixnum)
                                             (read-from-string buf-str (- (point) buf-off)))
                                         (cons nil (- limit (point))))))
                                    rlim (+ (cdr rpair) buf-off)
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
                                   (forward-comment most-positive-fixnum)
                                   (while (> (skip-syntax-forward ")") 0)
                                     (forward-comment most-positive-fixnum))
                                   (or (>= (point) lim)
                                       (eobp)))
                           finally return result))
                        (cons start lim)))
                   (scan-error nil))))
        (walk sexp-end)))))

(defmacro pospcase-translate (matcher)
  "Translate `pcase' matcher pattern (usually backquoted) to
matcher pattern consumable for `pospcase'.

Beware this macro support severely limited patterns of
`pcase'. Only plain symbols are bind-able.

Patterns like

  `,(or (and 'foo bar) (and 'bar quux))

or

  `,(and 'foo (let bar 123))

are beyond the scope of `pospcase'."
  (cl-labels
      ((meta-pos-symbol (sym)
                        (list '\,
                              (if (eq sym '_)
                                  '_
                                (intern (concat
                                         (symbol-name sym)
                                         "-meta-pos")))))
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
  (let ((pospcase--nth-chop-off (when (and (consp (caar cases))
                                           (eq (caaar cases) '\`)
                                           (consp (cdaar cases))
                                           (consp (car (cdaar cases)))
                                           (equal (last (car (cdaar cases)) 2) ',_))
                                  (- (length (car (cdaar cases))) 2))))
    (pospcase (pospcase-read pos) cases)))

(defun pospcase-pos (match)
  "Extract a positional metadata cons cell (start . end) from a
sexp tree. Especially useful for `pospcase-read' or a pattern
with dot cdr notation for `pospcase' or `pospcase-at' like:

  (pospcase-pos (pospcase-read (point)))

  or

  (pospcase-pos (pospcase-at (point) '((`(,foo . ,bar) bar))))"
  (when (consp match)
    (cond

     ((numberp (cdr match))                        ; (start . end)
      match)

     ((and (consp (cdr match))                     ; (sexp . (start . end))
           (numberp (cadr match))
           (numberp (cddr match)))
      (cdr match))

     ((and (consp (cdr (car match)))
           (numberp (cddr (car match))))           ; ((sexp start . end)
                                                   ;  ... (sexp start . end))
      (cons (cadr (car match))
            (cddr (car (last match)))))

     (t
      (cons (if (numberp (car (car match)))        ; ((start . end) ...)
                (car (car match))
              (cadar (car match)))                 ; (((sexp start . end) ...) ...)
            (if (numberp (cdr (car (last match)))) ; (... (start . end))
                (cdr (car (last match)))
              (cddr (car (last (car (last match)))))))))))

(provide 'pospcase)
