(defun sexp-font-lock-parse-at-point ()
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

(defmacro sexp-font-lock-make-pattern (exp)
  (cl-labels ((walker (node)
                      (if (consp node)
                          (if (and (listp (cdr node))
                                   (not (eq (cadr node) '\,)))
                              (case (car node)
                                ('quote (cons node ',_))
                                ('\, (if (eq (cadr node) '_)
                                         (cons node ',_)
                                       (list node
                                             '\,
                                             (intern
                                              (concat (symbol-name (cadr node)) "-meta-pos")))))
                                ('\` (list '\`
                                           (cl-loop for sexp in (cdr node)
                                                    collect (walker sexp))
                                           ',_))
                                (t (cons (cl-loop for sexp in node
                                                  collect (walker sexp))
                                         ',_)))
                            (cons (cons (walker (car node))
                                        (walker (cdr node)))
                                  ',_))
                        (cons node ',_))))
    (if (consp exp)
        (case (car exp)
          ('quote (list 'quote (list '\` (cons exp ',_))))
          ('\` (list 'quote (list '\` (walker (cadr exp)))))
          (list 'quote (list '\` (walker exp)))))))

(defmacro sexp-font-lock-match-at-point (&rest cases)
  (list 'quote
        (eval (pcase--expand
               (list 'quote (sexp-font-lock-parse-at-point))
               (mapcar
                (lambda (case)
                  (list
                   (eval (macroexpand `(sexp-font-lock-make-pattern ,(car case))))
                   (intern (concat
                            (symbol-name (cadr case)) "-meta-pos"))))
                cases)))))

(defun sexp-mark-pattern-at-point (pat)
  (interactive "xPattern to match: ")
  (let ((match (eval (macroexpand `(sexp-font-lock-match-at-point ,pat)))))
    (when match
      (goto-char (car match))
      (push-mark (cdr match) nil t))))
