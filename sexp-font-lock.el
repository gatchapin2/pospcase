(defun sexp-font-lock-read-at-point ()
  "Parse the s-expression at the cursor position. Return a
s-expression with the tokens being replaced with the cons. With
which `car' is original token, and `cdr' is positional metadata
of the token as a cons in (begin . end)."
  (cl-labels
      ((walker (limit)
               (destructuring-bind (start sexp . lim)
                   (cons (point)
                         (read-from-string (buffer-substring-no-properties (point) limit)))
                 (incf lim (point))
                 (forward-comment lim)
                 (cons
                  (if (or (atom sexp)
                          (memq (car sexp) '(quote \`)))
                      sexp
                    (down-list)
                    (cl-loop with str
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
                                                       (string-match "[ \t]*\\.[ \t]+" str)
                                                       (replace-match "" nil nil str)))
                                           (read-from-string str))))
                                      rlim (+ (point) (cdr rpair))
                                      temp (if (or (atom (car rpair))
                                                   (memq (car rpair) '(quote \`)))
                                               (cons (car rpair) (cons (point) rlim))
                                             (save-excursion
                                               (walker rlim))))
                             if dot
                             do (setq result (cons result temp))
                             else
                             do (setq result (append result (list temp)))
                             until (progn
                                     (condition-case nil
                                         (forward-sexp)
                                       (error nil))
                                     (skip-chars-forward ")")
                                     (>= (point) lim))
                             finally return result))
                  (cons start lim)))))
    (save-excursion (walker (scan-sexps (point) 1)))))

(defmacro sexp-font-lock-translate-pattern (exp)
  "Translate `pcase' pattern to fit s-expression generated by
`sexp-font-lock-read-at-point'. Nested backquote is not
supported (maybe `pcase' doesn't support it too?)."
  (cl-labels
      ((walker (node &optional last)
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
                       (append
                        (if last nil (list node)) ; couldn't design better grammar
                        (list
                         '\,
                         (intern
                          (concat (symbol-name (cadr node)) "-meta-pos"))))))
                    (t (cons
                        (cl-loop with result
                                 for temp = (reverse node) ; for last cdr cell. Clever technique.
                                 then (if (memq (cadr temp) '(quote \` \,))
                                          (cddr temp)
                                        (cdr temp))
                                 if (memq (cadr temp) '(quote \` \,))
                                 do (setq result (append
                                                  (walker (list (cadr temp) (car temp)) t)
                                                  result))
                                 else if (car temp)
                                 do (setq result (append
                                                  (list (walker (car temp)))
                                                  result))
                                 while temp
                                 finally return result)
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
           (eval (macroexpand `(sexp-font-lock-translate-pattern ,(car case))))
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
structure. Complex operations are not supported."))))))
     cases)))

(defun sexp-mark-pattern-at-point (pat)
  (interactive "xPattern to match: ")
  (let ((match (eval (macroexpand `(sexp-font-lock-match-at-point ,pat)))))
    (when match
      (if (numberp (car match))
          (progn
            (goto-char (car match))
            (push-mark (cdr match) nil t))
        (goto-char (cadr (car match)))
        (push-mark (cddr (car (last match))) nil t)))))
