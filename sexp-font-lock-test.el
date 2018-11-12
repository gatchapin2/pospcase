(sexp-font-lock-match-at-point '((`(,name . ,_) name)))
(foo bar baz)

(and (sexp-font-lock-match-flat-list (scan-sexps (point) 1))
     (match-string 1))

(foo bar baz)

(setq sexp-font-lock--matches nil)

(cl-loop with p = (scan-sexps (point) 1)
         while (sexp-font-lock-match-flat-list p)
         collect (match-string-no-properties 1))

(foo bar)



(setq sexp-font-lock--matches nil)

(cl-loop with p = (scan-sexps (point) 1)
         while (sexp-font-lock-match-varlist p)
         collect (list (cons 'name (match-string-no-properties 1))
                       (cons 'type (or (match-string-no-properties 2) 'unknown))))

(and (sexp-font-lock-match-varlist (scan-sexps (point) 1))
     (match-string 1))

((foo bar) baz (quux meow))



(setq sexp-font-lock--matches nil)
(and (sexp-font-lock-match-flet (scan-sexps (point) 1))
     (list (cons 'name (or (match-string-no-properties 1) 'none))
           (cons 'arg (or (match-string-no-properties 2) 'none))))

((foo (bar baz) quux)
 (meow (woof) oink))

;; (defun foo (bar baz quux)
;;   body)

;; (defun foo (bar baz &rest quux)
;;   body)

(flet ((foo (bar baz) quux)
       (meow (woof) oink))
  body)

(cl-symbol-macrolet ((foo bar))
  foo)

(defun foo (bar (baz quux) meow woof)
  body)

(sexp-font-lock-match-at-point (`(,foo ,bar . ,rest) bar))
(baz quux meow)

(sexp-font-lock-after-anchor sexp-font-lock--matches)
(setq sexp-font-lock--matches '(((1 . 2)
                                 (3 . 4)
                                 (5 . 6))
                                ((11 . 12)
                                 (13 . 14)
                                 (15 . 16)))
      sexp-font-lock--anchor 4)

(setq sexp-font-lock--matches nil
      sexp-font-lock--anchor nil)

(progn
  ;; (save-excursion
  ;;   (down-list)
  ;;   (forward-sexp 2)
  ;;   (setq sexp-font-lock--anchor (point)))
  (sexp-font-lock-match-varlist-cars
         (scan-sexps (point) 1))
  (match-string-no-properties 1))

(cl-defstruct package-desc
  name
  version
  (summary package--default-summary)
  reqs
  kind
  archive
  dir
  extras
  signed)

(progn
  (sexp-font-lock-match-varlist-cars
   (scan-sexps (point) 1))
  (match-string 1))

(and (sexp-font-lock-match-flat-list (point))
     (match-data))

(setq sexp-font-lock--anchor 2050)

(setq sexp-font-lock--matches
      (sexp-font-lock-after-anchor
       sexp-font-lock--matches)
      sexp-font-lock--anchor nil)

(progn
  (setq sexp-font-lock--anchor 2050)
  (sexp-font-lock-after-anchor
   '(((2046 . 2049))
     ((2051 . 2081))
     ((2097 . 2107)))))


(cl-loop with p = (scan-sexps (point) 1)
         do (setq sexp-font-lock--anchor (+ (point) 5))
         while (sexp-font-lock-match-varlist-cars p)
         collect (match-string-no-properties 1))

((foo bar) baz (quux meow))

(defmacro foo (bar (baz quux) &body meow)
  `,body)
