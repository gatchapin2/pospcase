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

(ppr (pospcase-translate `(,foo)))

(pcase 1 (`,(pred integerp) t))
(ppr (pospcase-translate `,(pred integerp)))

(pospcase-at (point) '((`,(pred integerp) t)))
1

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
  body)

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

(pospcase-match-destructuring (scan-sexps (point) 1))
(foo (bar baz))

(and (pospcase-match-destructuring (scan-sexps (point) 1))
     (match-data)
     (match-string-no-properties 1))

(destructuring-bind (((foo auto-mode-alist))) '((baz) quux)
  (list foo bar))

(defmacro foo (bar (baz quux) &key (woof oink) &body meow)
  `,body)

(let ((limit (ignore-errors (scan-sexps (point) 1))))
  (goto-char
   (or
    (and
     (save-excursion
       (re-search-forward
        (funcall #'regexp-opt lisp-extra-argument-list-key-keywards)
        limit t))
     (match-beginning 0))
    limit)))

(cl-labels ((collect (node) (if (symbolp (car node))
                                (list (list (cdr node)))
                              (cl-loop for child in (car node)
                                       unless (and
                                               (consp (car child))
                                               (member (caar child) '(quote \`)))
                                       append (collect child)))))
  (collect (pospcase-read (point))))

(foo 'bar)

(destructuring-bind (foo &optional (bar 'baz))
    '(foo)
  (list foo bar))

(defmacro foo (bar (&optional (bzzt eek)) ouch) baz)

;;; pospcase-math-macrolet

(macrolet ((foo (bar (&optional (bzzt eek)) ouch) baz)
           (quux ((meow woof) oink) quaak)
           ())
  body)

(pospcase-match-macrolet nil)
((foo (bar (baz) &key (qux (quux)))))

(pospcase-collect-all-symbols
 (pospcase-read (point)))
(funfun (setf ,name) ,varlist-cars . ,_)
(funfun foo (bar baz &rest quux) body)

(destructuring-bind (head . tail) '(foo bar . baz)
  (list head tail))

(cl-labels
    ((collect (node)
              (cl-loop with result
                       for temp = node then (cdr temp)
                       if (null temp) return result
                       else if (atom temp) return (append result (list temp))
                       else do (setq result
                                     (append result
                                             (collect (car temp)))))))
  (collect '((foo (bar baz)) quux)))

(cl-labels
    ((pos-p (pair)
                (numberp (car pair))
                (numberp (cdr pair)))
     (leaf-p (node)
             (and (symbolp (car node))
                  (pos-p (cdr node))))
     (collect (node)
              (cl-loop with result
                       for temp = node then (cdr temp)
                       if (or (null temp)
                              (pos-p temp)) return result
                       else if (leaf-p temp) return (append
                                                     result
                                                     (list (list (cdr temp))))
                       else do (setq result (append result (collect (car temp)))))))
  (collect ))

;;; DSL

#'pospcase-font-lock
#'pospcase-font-unlock

;; scratch

(keywordp :before)
(keywordp '&optional)
(pcase '(defmethod foo () :before body)
  (`(defmethod ,name ,args ,(pred keywordp) . ,_)
   (list name args)))

(pospcase-at (point)
  '((`(defmethod ,name ,args ,(pred keywordp) . ,_)
     (list name args))))
(defmethod foo () :before body)

(pcase '((foo)) (`(,`(,bar)) bar))
(pcase '((foo)) (`,(bar) bar))
(let ((bar 123)) `,`(,bar))
(pospcase-translate `,`(,bar))

(pospcase-at (point)
             '((`(defun (setf ,name) ,varlist-cars . ,_)
                (values name varlist-cars))
               (`(defun ,name ,varlist-cars . ,_)
                (values name varlist-cars))))
(defun (setf ,name) ,varlist-cars  . ,_)

(font-lock-add-keywords 'text-mode
                        '("meow" (0 'font-lock-function-name-face nil t))
                        'append)

;; workables

(font-lock-add-keywords
 nil
 `((,(concat "(mydefun" "[ \t\n]+")
   (pospcase-match-varlist-cars
    (pospcase--preform
     (goto-char (match-beginning 0))
     (let ((match-end (save-excursion
                        (or
                         (ignore-errors (scan-sexps (point) 1))
                         (progn
                           (end-of-defun)
                           (point))))))
       (multiple-value-bind
           (name varlist-cars) (pospcase-at (point)
                                            '((`(mydefun (setf ,name) ,varlist-cars . ,_)
                                               (values name varlist-cars))
                                              (`(mydefun ,name ,varlist-cars . ,_)
                                               (values name varlist-cars))))
         (if (null name)
             (goto-char match-end)
           (setq pospcase--prematches (list name))
           (goto-char (car varlist-cars))))
       match-end))
    (pospcase--postform)
    (1 'font-lock-function-name-face nil t)
    (2 'font-lock-variable-name-face nil t))))
 'append)

(define-key emacs-lisp-mode-map [?\H-y]
  (lambda ()
    (interactive)
    (if (search-backward (concat "defmacro" " pospcase-font-lock") nil t)
        (progn
          (replace-match (concat "defun" " pospcase-font-lock"))
          (search-forward "font-lock-add-keywords")
          (replace-match "font-lock-remove-keywords")
          (search-forward "'append")
          (replace-match ""))
      (search-backward (concat "defun" " pospcase-font-lock"))
      (replace-match (concat "defmacro" " pospcase-font-lock"))
      (up-list)
      (backward-char 3)
      (insert "'append")
      (search-backward "font-lock-remove-keywords")
      (replace-match "font-lock-add-keywords"))))

(defmacro pospcase-font-lock (mode patterns &rest specs)
  (let* ((matcher (let ((str (prin1-to-string
                              (if (and (consp (car patterns))
                                       (memq (caar patterns) '(\` \, quote)))
                                  (cadar patterns)
                                (car patterns)))))
                    (string-match "^[^ \t\n]+" str)
                    (concat (match-string 0 str) "[ \t\n]+")))
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
         (cases (mapcar (lambda (pat) (list pat (cons 'values vars))) patterns)))
    `(font-lock-add-keywords
      ,mode
      ,(cond
        ((memq submatcher '(varlist varlist-cars destructuring flet macrolet))
         `'((,matcher
             (,(intern (concat "pospcase-match-" (symbol-name submatcher)))
              (pospcase--preform
               (goto-char (match-beginning 0))
               (let ((match-end (save-excursion
                                  (or
                                   (ignore-errors (scan-sexps (point) 1))
                                   (progn
                                     (end-of-defun)
                                     (point))))))
                 (multiple-value-bind ,vars (pospcase-at (point) ',cases)
                   (if (memq nil ,(cons 'list vars)) ; not exact match
                       (goto-char match-end)
                     ,(unless (null non-subvars)
                        `(setq pospcase--prematches ,(cons 'list non-subvars)))
                     (goto-char (car ,subvar))))
                 match-end))
              (pospcase--postform)
              ,@fontspecs))))
        (t (error "Not supported submatcher.")))
      'append)))

(ppr(pospcase-font-lock nil
                    '(`(mydefun (setf ,name ,name2) ,args . ,_)
                      `(mydefun ,name ,name2 ,args . ,_))
                    '(name . (font-lock-function-name-face))
                    '(name2 . (font-lock-function-name-face2))
                    '((args . varlist-cars) . (font-lock-variable-name-face))))

(pospcase-font-lock nil
                    (`(mydefun (setf ,name) ,args . ,_)
                     `(mydefun ,name ,args . ,_))
                    (name . (font-lock-function-name-face))
                    ((args . varlist-cars) . (font-lock-variable-name-face)))

(mydefun foo (bar) bar)

(pospcase-font-lock nil
                    (`(defclass ,name ,args . ,_))
                    (name . (font-lock-function-name-face))
                    ((args . varlist-cars) . (font-lock-type-face)))

(ppr(pospcase-font-lock  nil
                        '(`(mysymbol-macrolet ,binds . ,_))
                        '((binds . varlist) . (font-lock-variable-name-face
                                               font-lock-constant-face))))


(pospcase-font-lock  nil
                     (`(mysymbol-macrolet ,binds . ,_))
                     ((binds . varlist) . (font-lock-variable-name-face
                                           font-lock-constant-face)))

(mysymbol-macrolet ((foo bar)) baz)

(pospcase-font-lock nil
                    (`(defmethod (setf ,name) ,args ,(pred keywordp) . ,_)
                     `(defmethod (setf ,name) ,args . ,_)
                     `(defmethod ,name ,args ,(pred keywordp) . ,_)
                     `(defmethod ,name `,args . ,_))
                    (name . (font-lock-function-name-face))
                    ((args . varlist) . (font-lock-variable-name-face
                                         font-lock-type-face)))

(ppr(pospcase-font-lock nil
                    '(`(destructuring-bind ,binds . ,_))
                    '((binds . destructuring) . (font-lock-variable-name-face))))

(pospcase-font-lock nil
                    (`(mydestructuring-bind ,binds . ,_))
                    ((binds . destructuring) . (font-lock-variable-name-face)))

(mydestructuring-bind (foo ((bar) baz)) quux)


;; experimentals

(ppr(pospcase-font-lock nil
                    '(`(flet ,funs . ,_))
                    '((funs . flet) . (font-lock-function-name-face
                                      font-lock-variable-name-face))))

(pospcase-font-lock nil
                    (`(myflet ,funs . ,_))
                    ((funs . flet) . (font-lock-function-name-face
                                      font-lock-variable-name-face)))

(myflet ((foo () bar)
         (baz (quux) meow)))

(ppr(pospcase-font-lock nil
                    '(`(macrolet ,macros . ,_))
                    '((macros . macrolet) . (font-lock-function-name-face
                                            (lisp-extra-font-lock-variable-face-form '(match-string 2))))))

(pospcase-font-lock nil
                    (`(mymacrolet ,macros . ,_))
                    ((macros . macrolet) . (font-lock-function-name-face
                                            (lisp-extra-font-lock-variable-face-form (match-string 2)))))

(ppr(lisp-extra-font-lock-variable-face-form '(match-string 2)))

(mymacrolet ((foo (bar (baz)) quux)
             (meow (major-mode) woof)))

(macrolet ((foo (bar (baz)) quux)
             (meow (major-mode) woof)))

;; no idea

(let* ((exp '(foo baz 1 quux meow woof))
       (pat `(,@(reverse (memq 1 (reverse exp))) . ,(list '\, 'rest))))
  (eval `(pcase ,(list 'quote exp) ,(list (list '\` pat) 'rest))))

(pospcase-font-lock nil
                    (`(defstruct ,name ,(pred stringp) . ,slots)
                     `(defstruct ,name . ,slots))
                    (name . (font-lock-function-name-face))
                    ((slots . dot-varlist-cars) . (font-lock-variable-name-face)))


(pcase '(foo &para bar baz) (`(foo &para . ,rest) rest))

(pospcase-font-lock nil
                    (`(pareameter (,name ,init ,sup)))
                    (name . (font-lock-function-name-face))
                    (init . ('default))
                    (sup . ('default)))
