(fset #'lisp-extra-font-lock-match-argument-list-orig (symbol-function #'lisp-extra-font-lock-match-argument-list))
(fset #'lisp-extra-font-lock-match-argument-list (symbol-function #'sexp-font-lock-match-flat-list))
(font-lock-fontify-buffer)
(fset #'lisp-extra-font-lock-match-argument-list (symbol-function #'lisp-extra-font-lock-match-argument-list-orig))

(and (sexp-font-lock-match-flat-list (scan-sexps (point) 1))
     (match-string 1))

(foo bar baz)

(setq sexp-font-lock--matches nil)

(cl-loop with p = (scan-sexps (point) 1)
         while (sexp-font-lock-match-flat-list p)
         collect (match-string-no-properties 1))

(foo bar)



(defvar sexp-debug-counter 0)
(setq sexp-debug-counter 0)
(setq sexp-font-lock--matches nil)

(and (sexp-font-lock-match-varlist (scan-sexps (point) 1))
     (match-string 1))

(cl-loop with p = (scan-sexps (point) 1)
         while (sexp-font-lock-match-varlist p)
         collect (list (cons 'name (match-string-no-properties 1))
                       (cons 'type (or (match-string-no-properties 2) 'unknown))))

((foo bar) baz (quux meow))



(setq sexp-font-lock--matches nil)
(and (sexp-font-lock-match-flet (scan-sexps (point) 1))
     (list (cons 'name (or (match-string-no-properties 1) 'none))
           (cons 'arg (or (match-string-no-properties 2) 'none))))
((foo (bar baz) quux))
