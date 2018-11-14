;;; basic checks

(flet ((bar () 'bar))
  (pcase '((foo)) (`(,(bar)) bar)))

(pospcase-at (point)'(('() t)))
()

(pospcase-read (point))
(foo)

(pcase '(foo bar) (`(foo ,bar) bar))

(pospcase-at (point) '((`(,foo ,bar) (cons (list foo) (list bar)))
                       (`,all all)))
(foo bar)

(pospcase-at (point) '((`(,foo ,bar) (cons (list foo) (list bar)))
                       (`,all all)))
(foo bar baz)

(car '(`(,name . ,_) name))

(pospcase-at (point) '((`(,name . ,_) name)))
(foo bar baz)

(let ((case '(`(,name . ,_) name)))
  (cl-labels ((walk (node)
                    (cond ((member node symbols)
                           (intern (concat (symbol-name node) "-meta-pos")))
                          ((consp node) (cons
                                         (walk (car node))
                                         (walk (cdr node))))
                          (t node))))
    (walk (cadr case))))

(let ((symbols '(name)))
  (cl-labels
    ((translate (node)
                (cond
                 ((member node symbols)
                  (intern (concat (symbol-name node) "-meta-pos")))
                 ((consp node) (cons
                                (translate (car node))
                                (translate (cdr node))))
                 (t node))))
  (translate (cadr '(`,name (list name))))))


;;; pospcase-match-flat-list

(and (pospcase-match-flat-list (scan-sexps (point) 1))
     (match-string 1))

(foo bar baz)

(setq pospcase--matches nil)

(cl-loop with p = (scan-sexps (point) 1)
         while (pospcase-match-flat-list p)
         collect (match-string-no-properties 1))

(foo bar)


;;; pospcase-match-varlist

(car '(\, foo))
(car '((\, foo) (\, bar)))

(let* ((case '(`(,foo ,bar) (list foo bar)))
       collected)
  (cl-labels ((collect (node)
                       (cond
                        ((and (listp node)
                              (eq (car node) '\,))
                         (setq collected
                               (cons (cadr node) collected)))
                        ((consp node)
                         (collect (car node))
                         (collect (cdr node))))))
    (collect case))
  collected)

(pospcase-at (point)
             '((`(,name ,type) (list name type))
               (`,name (list name))))
(foo bar)
foo

(mapcar
    (lambda (srpair)
      (goto-char (cadr srpair))
      (pospcase-at (point)
                   '((`(,name ,type) (list name type))
                     (`,name (list name)))))
    (car (pospcase-read (point))))
(foo (bar baz) (qux quux))

(setq pospcase--matches nil)

(and (pospcase-match-varlist (scan-sexps (point) 1))
     (match-string 1))

((foo bar) baz (quux meow))

(and (pospcase--iterator (point))
     (match-data))

(cl-loop with p = (scan-sexps (point) 1)
         while (pospcase-match-varlist p)
         collect (list (cons 'name (match-string-no-properties 1))
                       (cons 'type (or (match-string-no-properties 2) 'unknown))))

(cl-symbol-macrolet ((foo bar))
  foo)

(defun foo (bar baz quux meow woof)
  body)


(defun foo (bar baz quux)
  body)

(defun foo (bar baz &rest quux)
  body

(pospcase-after-anchor pospcase--matches)
(setq pospcase--matches '(((1 . 2)
                                 (3 . 4)
                                 (5 . 6))
                                ((11 . 12)
                                 (13 . 14)
                                 (15 . 16)))
      pospcase--anchor 4)

(setq pospcase--matches nil
      pospcase--anchor nil)

(setq pospcase--anchor 2050)

(setq pospcase--matches
      (pospcase-after-anchor
       pospcase--matches)
      pospcase--anchor nil)

(progn
  (setq pospcase--anchor 2050)
  (pospcase-after-anchor
   '(((2046 . 2049))
     ((2051 . 2081))
     ((2097 . 2107)))))

((foo bar) baz (quux meow))

  
;;; pospcase-match-varlist-cars

(progn
  ;; (save-excursion
  ;;   (down-list)
  ;;   (forward-sexp 2)
  ;;   (setq pospcase--anchor (point)))
  (pospcase-match-varlist-cars
         (scan-sexps (point) 1))
  (match-string-no-properties 1))
(cl-defstruct foo bar (baz quux))
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
  (pospcase-match-varlist-cars
   (scan-sexps (point) 1))
  (match-string 1))

(cl-loop with p = (scan-sexps (point) 1)
         do (setq pospcase--anchor (+ (point) 5))
         while (pospcase-match-varlist-cars p)
         collect (match-string-no-properties 1))


(let ((foo (bar baz))
      (meow (woof quaack))
      (qux quux)
      ouch
      (oink ugh))
  (foo))

;;; pospcase-match-flet

(setq pospcase--matches nil)
(and (pospcase-match-flet (scan-sexps (point) 1))
     (list (cons 'name (or (match-string-no-properties 1) 'none))
           (cons 'arg (or (match-string-no-properties 2) 'none))))

((foo (bar baz) quux)
 (meow (woof) oink))

(flet ((baz (qux) quux)
       (foo () bar)
       (meow (woof oink) body))
  (foo))

(pospcase-at (point) '((`(,foo ,bar . ,rest) bar)))
(baz quux meow)


;;; pospcase-match-destructuring

(and (pospcase-match-destructuring (scan-sexps (point) 1))
     (match-data)
     (match-string-no-properties 1))

(destructuring-bind (((foo auto-mode-alist))) '((baz) quux)
  (list foo bar))

(defmacro foo (bar (baz quux) &body meow)
  `,body)
